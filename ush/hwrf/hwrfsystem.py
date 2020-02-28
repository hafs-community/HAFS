"""!Combines multiple tasks together to make the HWRF post-processing system.

This is a wrapper around the hwrf.post, hwrf.gribtask, hwrf.tracker,
hwrf.nhc_products and hwrf.gsipost that creates customizable
post-processing tasks.  The main purpose of this module is to simplify
the hwrf_expt module."""


##@var __all__
# Symbols exported by "from hwrf.hwrfsystem import *"
__all__=['HWRFGSIPostProcessing','HWRFForecastPostProcessing']

import os,sys,math

import produtil.datastore, produtil.run

import hwrf.config, hwrf.wrf, hwrf.post, hwrf.numerics, hwrf.launcher
import hwrf.regrib, hwrf.gribtask, hwrf.tracker, hwrf.storminfo
import hwrf.wps, hwrf.nhc_products, hwrf.copywrf, hwrf.fcsttask, hwrf.ensda
import hwrf.relocate, hwrf.init, hwrf.prep, hwrf.gsi, hwrf.mpipomtc
import hwrf.bufrprep, hwrf.hwrftask,hwrf.multistorm

from produtil.run import exe,alias
from hwrf.numerics import to_datetime_rel
from hwrf.wrf import WRFDomain,WRFSimulation,ExternalWRFTask
from hwrf.post import PostManyWRF
from hwrf.regrib import RegribMany,igrb1,clatlon,GRIB2,SATGRIB2


def add_clatlon_grid(task,r,name,rel):
    """!Add a domain-centered lat-lon grid to a RegribMany object.

    Adds to the given hwrf.regrib.RegribMany object a
    latitude-longitude grid centered on some GRIB product.  The grid
    will be re-centered at every forecast time, so the grid is allowed
    to move.  Centering information comes from the name+"_grid"
    option in the task's section.  The value should be a comma-separated
    list of seven values:
    * res1,res2 - grid resolution for GRIB1 grid 255
    * size1,size2 - size in degrees for GRIB1 grid 255
    * scan - scanning mode flags for GRIB1 grid 255
    * n1,n2 - gridpoint count for GRIB1 grid 255
    @param task the task that provides configuration information
    @param r    the hwrf.regrib.RegribMany object to receive the grid
    @param name the name of the new grid
    @param rel  the GRIB product in r on which this grid is centered"""
    contents=task.confstr(name+"_grid")
    split=contents.rsplit(",")
    assert(len(split)==7)
    res1=float(split[0])
    res2=float(split[1])
    size1=float(split[2])
    size2=float(split[3])
    scan=int(split[4])
    n1=int(split[5])
    n2=int(split[6])
    r.add(name,clatlon(rel,res=[res1,res2],size=[size1,size2],
                       scan=scan,n=[n1,n2]))

def add_expandlatlon_grid(task,r,name,rel):
    """!Adds a grid-centered lat-lon grid to a RegribMany object

    Adds a new grid to an hwrf.regrib.RegribMany object, centered on a
    prior existing grid but with a border added around its edges.  The
    grid is recalculated at every forecast time so it works with
    moving grids as well.  Grid expansion information comes from 
    the name+"_expand" option in the given task's section.  The value
    should be a comma-separated list of nine values:
    * west extension in degrees
    * east extension in degrees
    * north extension in degrees
    * south extension in degrees
    * n1,n2 - new gridpoint count for GRIB1 grid 255
    * scan - new scanning mode flags for GRIB1 grid 255
    * res1,res2 - new resolution for GRB1 grid 255
    @param task the task that provides configuration information 
    @param r    the hwrf.regrib.RegribMany object to receive the grid
    @param name the name of the new grid
    @param rel  the grid on which this grid is centered."""

    contents=task.confstr(name+"_expand")
    split=contents.rsplit(",")
    assert(len(split)==9)
    west=float(split[0])
    east=float(split[1])
    north=float(split[2])
    south=float(split[3])
    n1=int(split[4])
    n2=int(split[5])
    scan=int(split[6])
    res1=float(split[7])
    res2=float(split[8])
    r.add(name,hwrf.tracker.expandlatlon \
              (r.GRIB(rel),west=west,east=east,north=north,south=south,
               n=[n1,n2],scan=scan,res=[res1,res2]))

def to_com_intercom(task,what,r):
    """!Requests deliver to com and/or intercom of a RegribMany product.

    Utility function for deliveries.  Given what="hwrftrk" this will
    look for options "hwrftrk%com" and "hwrftrk%intercom" in the given
    task's config section.  If each option is present and non-empty,
    the corresponding delivery is enabled and the option's value is
    used as the delivery location.
    @param task the task that provides configuration information
    @param what the RegribMany product being delivered
    @param r the hwrf.regrib.RegribMany whose products are being delivered"""
    comvar=what+'%com'
    intercomvar=what+'%intercom'
    comloc=task.confraw(comvar,'')
    intercomloc=task.confraw(intercomvar,'')
    if comloc: 
        r.to_com(comloc,what)
    if intercomloc: 
        r.to_intercom(intercomloc,what)


class HWRFGSIPostProcessing(hwrf.hwrftask.HWRFTask):
    """!Configures the GSI post-processing and regribbing.

    Constructs tasks that will run the post-processing and
    regribbing on the inputs and outputs of GSI.  Arranges for the
    standard HWRF GSI diagnostic output grids, and makes them
    configurable by the HWRF config files.

    This class uses a config section to define many aspects of the GSI
    post-processing.  

    @code{.unformatted}
        [gsi_products]
        # Settings for GRIB1 grid 255 for each grid:
        d3_grid=0.02,0.02,12.,12.,136,600,600
        d2_grid=0.06,0.06,30.,30.,136,500,500
        
        # GRIB2 compression method
        grib2_compression=32  ; complex packing with second-order differences
        # grib2_compression=40   ; "lossless" jpeg 2000
        
        # Delivery settings:
        hwrforg_n%com={out_prefix}.hwrforg_n.grb2f00
        hwrforg_i%com={out_prefix}.hwrforg_i.grb2f00
        hwrfges_n%com={out_prefix}.hwrfges_n.grb2f00
        hwrfges_i%com={out_prefix}.hwrfges_i.grb2f00
        hwrfanl_n%com={out_prefix}.hwrfanl_n.grb2f00
        hwrfanl_i%com={out_prefix}.hwrfanl_i.grb2f00
    @endcode

    The d2_grid and d3_grid options configure the intermediate and
    innermost domain output grids.  They are sent into
    add_clatlon_grid() to generate the GRIB1 grid 255 (user-defined
    regular latitude-longitude) grid definition for interpolation."""
    def __init__(self,ds,conf,section,**kwargs):
        """!HWRFGSIPostProcessing constructor
        @param ds passed to Datum: the Datastore object for this Task
        @param conf the conf object for this task (passed to HWRFTask)
        @param section the conf section for this task (passed to HWRFTask)
        @param kwargs more keyword arguments passed to superclass"""
        super(HWRFGSIPostProcessing,self).__init__(
            ds,conf,section,**kwargs)
    def make_gsi_post(self,gsi_d02,gsi_d03,storm1ghost,storm1ghost_parent,
                      ceninit,gsid03_flag,gsipost_name='gsipost',
                      gsigribber_name='gsigribber'):
        """!Creates the gsi post-processor and regribber tasks.

        Creates the gsipost member variable, an hwrf.gsipost.GSIPost;
        and the gsigribber member variable, an hwrf.gribtask.GRIBTask.
        The gsipost converts GSI input and output files (wrfinput,
        ghost or wrfanl) to native E grid GRIB1 files.  The gsigribber
        converts the E grid GRIB1 files to GRIB2 files on selected
        output grids.
        @param gsi_d02 the hwrf.gsi.FGATGSI that runs the intermediate
           domain data assimilation
        @param gsi_d03 the hwrf.gsi.FGATGSI that runs the innermost domain
           data assimilation
        @param storm1ghost the hwrf.wrf.WRFDomain that represents the 
           innermost ghost domain
        @param storm1ghost_parent the hwrf.wrf.WRFDomain that represents the
           intermediate ghost domain
        @param ceninit the hwrf.init.InitBeforeGSI that runs the
           analysis time (FGAT 0 hour) initialization off of the GDAS 6hr
           forecast.
        @param gsid03_flag True=domain 3 gsi was run, False otherwise
        @param gsipost_name the section and task name for the gsipost
        @param gsigribber_name the section and task name for the gsigribber"""
        ds=self.dstore
        conf=self.conf
        # Create the GSI post.  It will post-process the ghost
        # files from the init.wrfghost, relocation and GSI for
        # each of d02 and d03:
        gsipost = hwrf.gsipost.GSIPost(ds,conf,gsipost_name)
        if gsid03_flag:
            gsipost.add_case(
                storm1ghost,
                ceninit.runghost.get_ghout(storm1ghost),
                org=ceninit.runghost.get_ghost(storm1ghost),
                ges=ceninit.rstage3.get_ghost(storm1ghost),
                anl=gsi_d03.get_ghost(storm1ghost)  )
        gsipost.add_case(
            storm1ghost_parent,
            ceninit.runghost.get_ghout(storm1ghost_parent),
            org=ceninit.runghost.get_ghost(storm1ghost_parent),
            ges=ceninit.rstage3.get_ghost(storm1ghost_parent),
            anl=gsi_d02.get_ghost(storm1ghost_parent)  )
        
        g2p='%d'%(self.confint('grib2_compression',32),)
        g=RegribMany(
            copygb=alias(exe(conf.getexe('copygb')).env(OMP_NUM_THREADS=1,MKL_NUM_THREADS=1)),
            wgrib=alias(exe(conf.getexe('wgrib')).env(OMP_NUM_THREADS=1,MKL_NUM_THREADS=1)),
            cnvgrib_g12=alias(exe(conf.getexe('cnvgrib')).env(OMP_NUM_THREADS=1,MKL_NUM_THREADS=1)['-g12','-p'+g2p]),
            satgrib2=alias(exe(conf.getexe('satgrib2')).env(OMP_NUM_THREADS=1,MKL_NUM_THREADS=1)),
            regridmerge=alias(exe(conf.getexe('hwrf_regrid_merge')).env(OMP_NUM_THREADS=1,MKL_NUM_THREADS=1)))
        
        storm1ghost_grib=igrb1(
            gsipost,domain=storm1ghost,which_step=hwrf.gsipost.F_ORG)
        storm1ghost_parent_grib=igrb1(
            gsipost,domain=storm1ghost_parent,which_step=hwrf.gsipost.F_ORG)
        add_clatlon_grid(self,g,'d3',storm1ghost_grib)
        add_clatlon_grid(self,g,'d2',storm1ghost_parent_grib)

        g.add('hwrforg_n',igrb1(gsipost,domain=storm1ghost,
            which_step=hwrf.gsipost.F_ORG)*g.grid('d3')*GRIB2)
        g.add('hwrforg_i',igrb1(gsipost,domain=storm1ghost_parent,
            which_step=hwrf.gsipost.F_ORG)*g.grid('d2')*GRIB2)

        g.add('hwrfges_n',igrb1(gsipost,domain=storm1ghost,
            which_step=hwrf.gsipost.F_GES)*g.grid('d3')*GRIB2)
        g.add('hwrfges_i',igrb1(gsipost,domain=storm1ghost_parent,
            which_step=hwrf.gsipost.F_GES)*g.grid('d2')*GRIB2)

        g.add('hwrfanl_n',igrb1(gsipost,domain=storm1ghost,
            which_step=hwrf.gsipost.F_ANL)*g.grid('d3')*GRIB2)
        g.add('hwrfanl_i',igrb1(gsipost,domain=storm1ghost_parent,
            which_step=hwrf.gsipost.F_ANL)*g.grid('d2')*GRIB2)

        for w in ( 'org', 'ges', 'anl' ):
            for gr in 'in':
                base='hwrf%s_%s'%(w,gr)
                to_com_intercom(self,base,g)
        
        gsigribber=hwrf.gribtask.GRIBTask(
            ds,conf,gsigribber_name,g,start=conf.cycle,step=3600,end=0)
        self.gsipost=gsipost
        self.gsigribber=gsigribber
        return (gsipost,gsigribber)

    ##@var gsipost
    # The hwrf.gsipost.GSIPost object that will post-process GSI
    # inputs and outputs, creating native E grid GRIB files.
    
    ##@var gsigribber
    # The hwrf.gribtask.GRIBTask that will regrid the output of the
    # gsipost, creating lat-lon grib files as defined in the
    # configuration files.

class HWRFForecastPostProcessing(hwrf.hwrftask.HWRFTask):
    """!Creates the post-processing for the HWRF forecast job.

    Creates and configures all tasks related to the post-processing
    system.  This includes the post, regribber, trackers, and any
    NHC-specific product generation, as well as some data delivery
    settings.  Everything is made configurable via the
    hwrf.config.HWRFConfig system.

    This class is configurable through config file options:
    @code[.conf]
        [forecast_products]
        # Post-processing start, end and step for various components:
        tracker_step=1      # hours between tracker inputs
        nonsatpost_step=1   # hours between non-satellite post inputs
        satpost_step=6      # hours between satellite post inputs
        wrfcopier_start=0   # first WRF native output time to copy to COM
        wrfcopier_end=9     # last native output time to copy to COM
        wrfcopier_step=3    # hours between WRF native outputs copied to COM
        combinetrack_fhr=12 # length of cycling track in hours
        
        # Settings for GRIB1 grid 255 (native lat-lon) for each grid:
        d23_grid=0.02,0.02,12.,15.,136,751,601
        d123low_grid=0.20,0.20,90.,110.,136,551,451
        d123high_grid=0.06,0.06,90.,110.,136,1834,1500
        d2_grid=0.06,0.06,12.,14.,136,234,201
        d3_grid=0.02,0.02,7.5,9.0,136,450,375
        d2t_grid=0.06,0.06,30.,30.,128,500,500
        d1t_grid=0.10,0.10,30.,30.,128,301,301
        
        # Tracker grid expansion settings:
        trk_expand=2.5,2.5,4.0,4.0,1000,1000,128,0.02,0.02
        
        # GRIB2 compression method:
        grib2_compression=32  ; complex packing with second-order differences
        # grib2_compression=40   ; alternative: "lossless" jpeg 2000
        
        # Output filenames:
        hwrftrk%com={out_prefix}.hwrftrk.f{fahr:03d}.grb
        hwrftrk%intercom={out_prefix}.hwrftrk.grbf{fahr:02d}
        
        anl_outer={out_prefix}.wrfanl_d02
        anl_inner={out_prefix}.wrfanl_d03
        
        hwrfprs_m%intercom={out_prefix}.hwrfprs.d23.0p02.f{fahr:03d}.grb
        hwrfprs_n%intercom={out_prefix}.hwrfprs.d3.0p02.f{fahr:03d}.grb
        hwrfprs_i%intercom={out_prefix}.hwrfprs.d2.0p06.f{fahr:03d}.grb
        hwrfprs_p%intercom={out_prefix}.hwrfprs.d1.0p20.f{fahr:03d}.grb
        hwrfprs_c%intercom={out_prefix}.hwrfprs.d123.0p06.f{fahr:03d}.grb
        hwrfprs_g%intercom={out_prefix}.hwrfprs.d123.0p25.f{fahr:03d}.grb
        
        hwrfsat_m%intercom={out_prefix}.hwrfsat.d23.0p02.f{fahr:03d}.grb
        hwrfsat_n%intercom={out_prefix}.hwrfsat.d3.0p02.f{fahr:03d}.grb
        hwrfsat_i%intercom={out_prefix}.hwrfsat.d2.0p06.f{fahr:03d}.grb
        hwrfsat_p%intercom={out_prefix}.hwrfsat.d1.0p20.f{fahr:03d}.grb
        hwrfsat_c%intercom={out_prefix}.hwrfsat.d12.0p06.f{fahr:03d}.grb
        hwrfsat_g%intercom={out_prefix}.hwrfsat.d123.0p25.f{fahr:03d}.grb
        
        hwrf2prs_m%com={out_prefix}.hwrfprs.d23.0p02.f{fahr:03d}.grb2
        hwrf2prs_n%com={out_prefix}.hwrfprs.d3.0p02.f{fahr:03d}.grb2
        hwrf2prs_i%com={out_prefix}.hwrfprs.d2.0p06.f{fahr:03d}.grb2
        hwrf2prs_p%com={out_prefix}.hwrfprs.d1.0p20.f{fahr:03d}.grb2
        hwrf2prs_c%com={out_prefix}.hwrfprs.d123.0p06.f{fahr:03d}.grb2
        hwrf2prs_g%com={out_prefix}.hwrfprs.d123.0p25.f{fahr:03d}.grb2
        
        hwrf2sat_m%com={out_prefix}.hwrfsat.d23.0p02.f{fahr:03d}.grb2
        hwrf2sat_n%com={out_prefix}.hwrfsat.d3.0p02.f{fahr:03d}.grb2
        hwrf2sat_i%com={out_prefix}.hwrfsat.d2.0p06.f{fahr:03d}.grb2
        hwrf2sat_p%com={out_prefix}.hwrfsat.d1.0p20.f{fahr:03d}.grb2
        hwrf2sat_c%com={out_prefix}.hwrfsat.d12.0p06.f{fahr:03d}.grb2
        hwrf2sat_g%com={out_prefix}.hwrfsat.d123.0p25.f{fahr:03d}.grb2
    @endcode

    Config options have certain formats:
    * product%com and product%intercom --- delivery location in com
      and intercom for each regribber output product
    * *_grid --- GRIB1 grid 255 grid descriptions sent to add_clatlon_grid()
    * *_expand --- GRIB1 grid 255 grid expansions sent to add_expandlatlon_grid()
    * *_step and *_fhr --- times or timespans in hours.
    All delivery filenames are relative to {com} and are subject to 
    string expansion by hwrf.config.HWRFConfig.timestrinterp().  Hence 
    everything in the [config] and [dir] sections are available, as
    well as standard time values.

    The product names in product%com and product%intercom follow a
    certain pattern:
    @code
        hwrfprs_m
        hwrf2sat_g
    @endcode
    In the second example, here is what each component means:
    * hwrf or hwrf2 -- GRIB1 or GRIB2 file?  The GRIB1 files are created
      first, and are later converted to GRIB2
    * prs or sat -- non-satellite post (prs) or satellite post (sat)
    * m, n, i, p, c or g -- the grid
    Here are the grid meanings:
    * n -- innermost domain only at its resolution
    * i -- intermediate domain only at its resolution
    * p -- parent domain only at its resolution
    * m -- innermost and intermediate domains, at innermost domain resolution
    * c -- if prs, parent and intermediate domains; if sat, all three
      domains.  This is at the intermediate domain resolution.
    * g -- global quarter degree grid, GRIB1 grid 193.  That is the
      output grid for the GFS master GRIB files."""
    def __init__(self,ds,conf,section,runwrf,wrf,postdoms,wrfdoms,
                 moad,storm1inner,storm1outer,**kwargs):
        """!Constructor for HWRFForecastPostProcessing
        @param ds the produtil.datastore.Datastore to use
        @param conf the HWRFConfig to use for configuration options.
             This conf is passed down to the RegribMany during
             regribbing operations.
        @param section the config section to use.
        @param runwrf the object that runs the forecast, such as an
             hwrf.fcsttask.WRFAtmos or hwrf.mpipomtc.WRFCoupledPOM
        @param wrf the hwrf.wrf.WRFSimulation that represents the
             forecast model.
        @param postdoms a list of domains to post-process in order
             of GRID id.
        @param wrfdoms a list of WRF domains in order of GRID id
        @param moad the Mother Of All Domains (MOAD), an
             hwrf.wrf.WRFDomain
        @param storm1inner the first storm's intermediate domain, an
             hwrf.wrf.WRFDomain
        @param storm1outer the first storm's innermost domain, an
             hwrf.wrf.WRFDomain
        @param kwargs additional keyword arguments are passed to the
             superclass constructor"""
        super(HWRFForecastPostProcessing,self).__init__(
            ds,conf,section,**kwargs)
        self.runwrf=runwrf
        self.wrf=wrf
        self.postdoms=postdoms
        self.wrfdoms=wrfdoms
        self.moad=moad
        self.storm1inner=storm1inner
        self.storm1outer=storm1outer

    ##@var runwrf
    # The hwrf.fcsttask.WRFAtmos or subclass thereof that runs the WRF.

    ##@var wrf
    # The hwrf.wrf.WRFSimulation describing the WRF simulation that was run.

    ##@var postdoms
    # The array of WRF domains to post-process (hwrf.wrf.WRFDomain objects)

    ##@var wrfdoms
    # The array of WRF domains (hwrf.wrf.WRFDomain objects)

    ##@var moad
    # The Mother Of All Domains (MOAD) in the WRF simulation, an
    # hwrf.wrf.WRFDomain object.

    ##@var storm1outer
    # The intermediate domain, an hwrf.wrf.WRFDomain

    ##@var storm1inner
    # The innermost domain, an hwrf.wrf.WRFDomain

    def make_nonsatpost(self,default_step=1.):
        """!Creates the non-satellite post-processor.

        Creates the nonsatpost member, which runs the non-satellite
        post processing.  That is the object that runs everything
        except the synthetic satellite brightness temperatures.  This
        is a hwrf.post.PostManyWRF object.
        @return the created object
        @param default_step non-satellite post-processor input step,
        if none is provided in the config file."""
        step=self.conffloat('nonsatpost_step',default_step)*3600
        self.nonsatpost = PostManyWRF(
            self.runwrf,self.postdoms,self.conf,'nonsatpost',step,
            needcrtm=False,streams=['history','auxhist2','auxhist3'])
        return self.nonsatpost

    ##@var nonsatpost 
    # The non-satellite post-processor, an hwrf.post.PostManyWRF
    # object.

    def make_satpost(self,default_step=6):
        """!Creates the satellite post-processor

        Creates the satpost member, an hwrf.post.PostManyWRF object
        that runs the satellite post-processing.
        @return the created object
        @param default_step satellite post-processor input step,
        if none is provided in the config file."""
        # Post-processors:
        step=self.conffloat('satpost_step',default_step)*3600
        self.satpost = PostManyWRF(
            self.runwrf,self.postdoms,self.conf,'satpost',step,
            needcrtm=True,streams=['history','auxhist2','auxhist3'])
        return self.satpost
    ##@var satpost
    #  The satellite post-processor, an hwrf.post.PostManyWRF object.

    def make_wrfcopier(self,wrfcopier_name='copywrf',multistorm=False):
        """!Generates the WRF input/output copier task.

        Creates the wrfcopier member variable which copies inputs and
        outputs of the forecast job directly to COM.  This is an
        hwrf.copywrf.WRFCopyTask object.
        @return the created object
        @param wrfcopier_name the section and task name to use for the
        hwrf.copywrf.WRFCopyTask that is created."""
        copystart=self.conffloat('wrfcopier_start',0)
        copyend=self.conffloat('wrfcopier_end',9)
        copystep=self.conffloat('wrfcopier_step',3)
        cycling_interval=self.conf.getfloat('config','cycling_interval',6.0)
        assert(copystep>0)

        runwrf=self.runwrf
        wrf=self.wrf
        if multistorm:
            wrfcopier = hwrf.multistorm.WRFCopyTaskMS(
                self.dstore,self.conf,wrfcopier_name,runwrf,
                self.conf.getstr('config','out_prefix'))
        else:
            wrfcopier = hwrf.copywrf.WRFCopyTask(
                self.dstore,self.conf,wrfcopier_name,runwrf,
                self.conf.getstr('config','out_prefix'))
        wrfcopier.d_initial('namelist.input')
        wrfcopier.d_initial('wrfinput_d01')
        wrfcopier.d_initial('wrfbdy_d01')
        wrfcopier.d_initial('fort.65')
        wrfcopier.d_initial(wrf.analysis_name(self.storm1outer),
                            destname=self.confraw('anl_outer'))
        wrfcopier.d_initial(wrf.analysis_name(self.storm1inner),
                            destname=self.confraw('anl_inner'))
        copied=False
        fhr=copystart
        while fhr-0.001<copyend:
            prodstream='history' if fhr % 3 == 0 else 'auxhist2'
            for product in runwrf.products(time=3600*fhr,stream=prodstream):
                wrfcopier.d_wrfprod(product,check=False,
                  destname='{vit[stnum]:02d}{vit[basin1lc]}.'
                                    '{inname_colon_s00}')
                copied=True
            fhr2=fhr+copystep
            assert(fhr2>fhr)
            fhr=fhr2
        assert(copied)

        # Note: Regarding wrfcopier.d_final('somefile_<domain>')
        #       and run_multistorm=yes. 
        # Any domain specific file also needs to be handled by 
        # the _deliver_to_group method of WRFCopyTaskMS else the
        # same file is copied to each real storms com dir. 
        wrfcopier.d_final('track_d03.patcf')

        # Deliver POM output in wrfcopier if POM is enabled:
        ocean_model=self.conf.getstr('config','ocean_model',
                                     'no_ocean_model_specified')
        run_ocean=self.conf.getbool('config','run_ocean',True)
        if run_ocean and ocean_model=='POM':
            fcstlen=self.confint('forecast_length',126)
            pom_output_sec=self.conffloat('pom_output_step')
            pom_output_hrs=pom_output_sec/3600.
            for iout in xrange(int(math.floor(fcstlen/pom_output_hrs+0.01))):
                ocrest="%04d.nc"%iout
                fromname=self.timestr('{vit[stormname]}.{ocrest}',ocrest=ocrest)
                toname=self.timestr('{out_prefix}.pom.{ocrest}',ocrest=ocrest)
                wrfcopier.d_wrfprod(fromname,check=True,destname=toname)

        run_wave=self.confstr('run_wave','no')
        if run_wave == 'yes':
            ctime=to_datetime_rel(cycling_interval*3600,self.conf.cycle)
            for p in runwrf.products(stream='ww3rst'):
                rstime=p['restarttime']
                rstime=int(rstime)
                wrfcopier.d_wrfprod(p,check=True,
                    destname='{vit[stormid3lc]}.restart.f%03d.ww3'%rstime)

            for p in runwrf.products(stream='ww3out'):
                wrfcopier.d_final(p)

        self.wrfcopier=wrfcopier
        return wrfcopier

    ##@var wrfcopier
    # The task that copies WRF input and output files directly to COM.
    # This is an hwrf.copywrf.WRFCopyTask object.

    def make_gribber_tracker(self,extra_trackers=False,satpost_flag=True,gofile_flag=False):
        """!Generate the regribbing objects and the main tracker.

        Generates the gribber, tracker, track and nhcp member
        variables and returns them in a tuple:
        * nhcp --- the hwrf.nhc_products.NHCProducts object that
          makes NHC-specific output products and validates the 
          wrfdiag files.
        * track --- the final track file output by the tracker, returned
          by hwrf.tracker.send_atcfunix() called on the new tracker object
        * tracker --- the hwrf.tracker.TrackerTask that runs the
          GFDL vortex tracker
        * gribber --- the hwrf.gribtask.GRIBTask that runs the 
          regribbing on the post-processors created by make_satpost() 
          and make_nonsatpost()

        @param extra_trackers logical flag: are the parent and
          intermediate domain trackers also desired?  If so, extra
          regribbing is arranged, that will be required by
          make_extra_trackers()
        @param satpost_flag logical flag: is satellite post-processing
          desired
        @pre The make_nonsatpost() must have been called.  If the
          satpost_flag=True, then make_satpost() must also have been
          called.
        @returns a tuple (nhcp,track,tracker,gribber) of the objects
          that were created"""
        # Aliases to simplify below code:
        (nonsatpost,satpost)=(self.nonsatpost,self.satpost)
        (moad,storm1inner,storm1outer) = \
            (self.moad,self.storm1inner,self.storm1outer)
        conf=self.conf
        fcstlen=self.confint('forecast_length',126)
        combinetrack_fhr=self.conffloat('combinetrack_fhr',12.)

        # Domain selection.  These objects will grab Post output for the
        # specified domain for any arbitrary time when inserted into a
        # RegribMany object:
        grid3=igrb1(nonsatpost,domain=storm1inner)  # 3km non-satellite post
        grid2=igrb1(nonsatpost,domain=storm1outer)  # 9km non-satellite post
        grid1=igrb1(nonsatpost,domain=moad)         # 27km non-satellite post
        satE3=igrb1(satpost,domain=storm1inner)     # 3km satellite post
        satE2=igrb1(satpost,domain=storm1outer)     # 9km satellite post
        satE1=igrb1(satpost,domain=moad)            # 27km satellite post
        
        # The hwrfsub, when used in a RegribMany, selects the
        # hwrfprs_c and hwrfsat_c GRIB1/2 subset:
        hwrfsub=hwrf.regrib.GRIBSubsetter(
            'part',hwrf.tracker.hwrf_combine_subset,None)

        # In a RegribMany, trksub selects the subset needed by the tracker:
        trksub=hwrf.regrib.GRIBSubsetter(
            'part',hwrf.tracker.tracker_subset,None)

        # The domloc is the location of the HWRF domain center:
        domloc=hwrf.regrib.FixedLocation(lat=conf['config','domlat'],
                                         lon=conf['config','domlon'])
        basin=conf.syndat.pubbasin2
        if ((basin=='AL' or basin=='EP') and domloc.ewcenter<360.0):
            domloc.ewcenter+=360.0 # to match 2013 HWRF behavior
       
        # Create the RegribMany object.  This object describes the GRIB
        # processing to do after the post completes:
        g2p='%d'%(self.confint('grib2_compression',32),)
        r=RegribMany(
            copygb=alias(exe(conf.getexe('copygb')).env(OMP_NUM_THREADS=1,MKL_NUM_THREADS=1)),
            wgrib=alias(exe(conf.getexe('wgrib')).env(OMP_NUM_THREADS=1,MKL_NUM_THREADS=1)),
            satgrib2=alias(exe(conf.getexe('satgrib2')).env(OMP_NUM_THREADS=1,MKL_NUM_THREADS=1)),
            cnvgrib_g12=alias(exe(conf.getexe('cnvgrib')).env(OMP_NUM_THREADS=1,MKL_NUM_THREADS=1)['-g12','-p'+g2p]),
            regridmerge=alias(exe(conf.getexe('hwrf_regrid_merge')).env(OMP_NUM_THREADS=1,MKL_NUM_THREADS=1)))
       
        # Define some grids that we'll reuse later:
        add_clatlon_grid(self,r,'storm',grid3)
        stormgrid=r.grid('storm')
        add_clatlon_grid(self,r,'core',grid3)
        coregrid=r.grid('core')
        add_clatlon_grid(self,r,'trkd3',grid3)
        trkd3grid=r.grid('trkd3')
        add_clatlon_grid(self,r,'synop',domloc)
        synopgrid=r.grid('synop')
        qd=hwrf.regrib.quarterDegree # GFS quarter degree grid (grid 193)

        r.add('p123_core',grid1.regridmerge(coregrid,grid2,grid3)*GRIB2)
        r.add('p123_synop',grid1.regridmerge(synopgrid,grid2,grid3)*GRIB2)
        r.add('p123_global',grid1.regridmerge(qd,grid2,grid3)*GRIB2)
        r.add('p123_storm_grib1',grid1.regridmerge(stormgrid,grid2,grid3))
        r.add('p123_storm',r.GRIB('p123_storm_grib1')*GRIB2)
        to_com_intercom(self,'p123_core',r)
        to_com_intercom(self,'p123_storm',r)
        to_com_intercom(self,'p123_global',r)
        to_com_intercom(self,'p123_synop',r)

        # Tracker input grid:
        r.add('trkin123',(grid1/trksub).regridmerge(
                trkd3grid, grid2/trksub, grid3/trksub))
        r.add('hwrftrk',hwrf.tracker.vinttave(r.GRIB('trkin123')))
        to_com_intercom(self,'hwrftrk',r)

        # Satellite post files if enabled:
        if satpost_flag:
            r.add('s123_core',satE1.regridmerge(coregrid,satE2,satE3)*SATGRIB2)
            r.add('s123_synop',satE1.regridmerge(synopgrid,satE2,satE3)*SATGRIB2)
            r.add('s123_global',satE1.regridmerge(qd,satE2,satE3)*SATGRIB2)
            r.add('s123_storm',satE1.regridmerge(stormgrid,satE2,satE3)*SATGRIB2)
            to_com_intercom(self,'s123_core',r)
            to_com_intercom(self,'s123_storm',r)
            to_com_intercom(self,'s123_global',r)
            to_com_intercom(self,'s123_synop',r)

        # Extra domains for d1 and d1+d2 trackers:
        if extra_trackers:
            r.add('trkin12',(grid1/trksub).regridmerge(
                    trkd3grid, grid2/trksub))
            r.add('hwrftrkd02',hwrf.tracker.vinttave(r.GRIB('trkin12')))

            r.add('trkin1',(grid1/trksub).regridmerge(
                    trkd3grid))
            r.add('hwrftrkd01',hwrf.tracker.vinttave(r.GRIB('trkin1')))
            to_com_intercom(self,'hwrftrkd01',r)
            to_com_intercom(self,'hwrftrkd02',r)
                    
        # Create the GRIBTask which will actually do the re-gribbing:
        gribber=hwrf.gribtask.GRIBTask(
            self.dstore,conf,'regribber',r,start=conf.cycle,
            step=3600,end=fcstlen*3600)

        # Run the GFDL vortex tracker in multi-file moveable grid
        # mode off of the hwrftrk files from gribber:
        tracker=hwrf.tracker.TrackerTask(self.dstore,conf,'tracker',
            start=conf.cycle, step=self.conffloat('tracker_step',1)*3600,
                                         end=fcstlen*3600)
        tracker.add_moving_grid(conf.syndat,gribber,'hwrftrk')
        tracker.send_raw_atcfunix('rawatcfunix',
            conf.strinterp('dir','{com}/{out_prefix}.trak.hwrf.raw'))
        track=tracker.send_atcfunix('cleanatcfunix',
            conf.strinterp('dir','{com}/{out_prefix}.trak.hwrf.atcfunix'))
        tracker.send_atcfunix_subset('atcf3hourly',freq=3,
            location=conf.strinterp('dir','{com}/{out_prefix}.trak.hwrf.3hourly'))
        tracker.send_atcfunix_subset('atcfshort6hr',freq=6,cut=112,
            location=conf.strinterp('dir','{com}/{out_prefix}.trak.hwrf.short6hr'))
        tracker.send_atcfunix_subset('combinetrack',fhr=12,
            location=conf.strinterp(
                'dir','{com}/{vit[stnum]:02d}{basin1lc}.trak.hwrf.'
                'atcfunix.{YMDH}.combine'))

        # Added for retrospective support - rocoto dependency check
        # looks for upper case [basin1] in filename to start relocate
        # tasks for the next cycle. Py scripts still want lower case.
        tracker.send_atcfunix_subset('combinetrack_uc',fhr=12,
            location=conf.strinterp(
                'dir','{com}/{vit[stnum]:02d}{basin1}.trak.hwrf.'
                'atcfunix.{YMDH}.combine'))

        if gofile_flag:
            tracker.send_atcfunix_subset('combinetrack_00',fhr=12,
                location=conf.strinterp(
                    'dir','{realstormcom}/relocate.trak.'
                    '{YMDH}.go',realstorm='{fakestormid}'))


        # Run the hwrf_nhc_products program to generate the swath,
        # afos file and other custom outputs for NHC:
        nhcp=hwrf.nhc_products.NHCProducts(
            self.dstore,conf,'nhc_products',self.runwrf,track,
            self.wrfdoms,self.conf.syndat,fcstlen=fcstlen)

        self.nhcp=nhcp
        self.track=track
        self.tracker=tracker
        self.gribber=gribber
        return (gribber,tracker,track,nhcp)

    ##@var nhcp
    # The task that generates custom products for NHC.  This is an
    #hwrf.nhc_products.NHCProducts object.

    ##@var tracker
    #  Task that runs the GFDL vortex tracker on all three domains
    #combined.  This is an hwrf.tracker.TrackerTask.

    ##@var track
    # The main track file from self.tracker.

    ##@var gribber
    #  The hwrf.gribtask.GRIBTask that does the regribbing.

    def make_extra_trackers(self):
        """!Generates intermediate and outermost domain trackers.

        Generates trackers that use intermediate and outermost domain
        data to track the storm and analyze its intensity.  These are
        intended for use in analyzing the effects of resolution and
        upscale feedback on hurricane track, structure and
        intensity.  Creates these member variables and returns them:
        * trackerd01 --- a tracker that just uses the outermost domain data
        * trackerd02 --- a tracker that uses the outermost and
          intermediate domain data
        @return A tuple (trackerd01,trackerd02) of the new trackers
        @pre The make_gribber_tracker() must have been called with
          extra_trackers=True, so it will add the extra GRIB products
          needed by the new trackers."""
        ds=self.dstore
        conf=self.conf
        gribber=self.gribber
        fcstlen=conf.getint('config','forecast_length',126)
        tracker_step=self.conffloat('tracker_step')*3600

        # d01+d02 tracker:
        trackerd02=hwrf.tracker.TrackerTask(ds,conf,'trackerd02',
            start=conf.cycle, step=tracker_step, end=fcstlen*3600)
        trackerd02.add_moving_grid(conf.syndat,gribber,'hwrftrkd02')
        trackerd02.send_raw_atcfunix('rawatcfunix',
            conf.strinterp('dir','{com}/{out_prefix}.trak.hwrfd02.raw'))
        trackerd02.send_atcfunix('cleanatcfunix',
            conf.strinterp('dir','{com}/{out_prefix}.trak.hwrfd02.atcfunix'))
    
        # d01 tracker:
        trackerd01=hwrf.tracker.TrackerTask(ds,conf,'trackerd01',
            start=conf.cycle, step=tracker_step, end=fcstlen*3600)
        trackerd01.add_moving_grid(conf.syndat,gribber,'hwrftrkd01')
        trackerd01.send_raw_atcfunix('rawatcfunix',
            conf.strinterp('dir','{com}/{out_prefix}.trak.hwrfd01.raw'))
        trackerd01.send_atcfunix('cleanatcfunix',
            conf.strinterp('dir','{com}/{out_prefix}.trak.hwrfd01.atcfunix'))
        self.trackerd02=trackerd02
        self.trackerd01=trackerd01
        return (trackerd01,trackerd02)

    ##@var trackerd01
    # The hwrf.tracker.TrackerTask that runs the GFDL vortex tracker
    # on the outermost domain vortex only.

    ##@var trackerd02
    # The hwrf.tracker.TrackerTask that runs the GFDL vortex tracker
    # on the combined outermost and intermediate domain vortex,
    # omitting the innermost domain.

class ForecastBase(object):
    def __init__(self,conf,ds,wrf,pominit,ww3init,hycominit,ocstatus):
        super(ForecastBase,self).__init__()
        ( self.conf, self.ds, self.wrf, self.pominit, self.ww3init ) = \
             ( conf,      ds,      wrf,      pominit,      ww3init )
        ( self.hycominit, self.ocstatus ) = \
            (  hycominit,      ocstatus )
        self.runwrf=None
        self.ww3post=None
        self.hycompost=None
        self.realwrf=None
    def make_forecast(self):
        # Get config information:
        ocean_flag=self.conf.getbool('config','run_ocean')
        ocean=self.conf.getstr('config','ocean_model','POM')
        wave_flag=self.conf.getbool('config','run_wave')
        wave=self.conf.getstr('config','wave_model','WW3')
        fcst_sec=self.conf.getstr('config','forecast_section','runwrf')
        ww3_output_step=self.conf.getint('forecast_products','ww3_output_step',21600)
        ww3_pntout_step=self.conf.getint('forecast_products','ww3_pntout_step',21600)
        fcstlen=self.conf.getint('config','forecast_length',126)

        # Update return values:
        if ocean_flag and ocean=='HYCOM':
            if wave_flag and wave=='WW3': 
                self.runwrf=hwrf.ww3.WRFWW3HYCOM(
                    self.ds,self.conf,fcst_sec,self.wrf,self.ocstatus,
                    taskname='runwrf',hycominit=self.hycominit,
                    ww3init=self.ww3init)
            else:
                self.runwrf=hwrf.hycom.WRFCoupledHYCOM(
                    self.ds,self.conf,fcst_sec,self.wrf,self.ocstatus,
                    taskname='runwrf',hycominit=self.hycominit)
        elif  ocean_flag and ocean=='POM':
            if wave_flag and wave=='WW3': 
                self.runwrf=hwrf.ww3.WRFWW3POM(
                    self.ds,self.conf,fcst_sec,self.wrf,taskname='runwrf',
                    pominit=self.pominit,ww3init=self.ww3init)
            else:
                self.runwrf=hwrf.mpipomtc.WRFCoupledPOM(
                    self.ds,self.conf,fcst_sec,self.wrf,taskname='runwrf',
                    pominit=self.pominit)
        else:
            self.runwrf=hwrf.fcsttask.WRFAtmos(
                self.ds,self.conf,fcst_sec,self.wrf,taskname='runwrf')
        if wave_flag and wave=='WW3':
            self.ww3post=hwrf.ww3.WW3Post(self.ds,self.conf,'ww3post',
              outstep=ww3_output_step,pntstep=ww3_pntout_step,ww3=self.runwrf)
        if ocean_flag and ocean=='HYCOM':
            self.hycompost=hwrf.hycom.HYCOMPost(
              self.ds,self.conf,'hycompost',fcstlen=fcstlen,hycom=self.runwrf)
        self.realwrf=self.runwrf
        return self.realwrf

class FakeStormForecast(ForecastBase):
    def __init__(self,conf,ds,wrf,multistorm_sids,finalmerge,
                 stormNouter,stormNinner):
        super(FakeStormForecast,self).__init__(
            conf,ds,wrf,None,None,None,None)
        self.multistorm_sids=multistorm_sids
        self.finalmerge=finalmerge
        self.stormNouter=stormNouter
        self.stormNinner=stormNinner
    def make_forecast(self):
        # Get config information:
        ocean_flag=self.conf.getbool('config','run_ocean')
        wave_flag=self.conf.getbool('config','run_wave')
        fcst_sec=self.conf.getstr('config','forecast_section','runwrf')
        prioritystorm=self.multistorm_sids[0]
        num_realstorms=len(self.multistorm_sids)
        
        if ocean_flag or wave_flag:
            raise hwrf.exceptions.HWRFConfigUnsupported(
                'Ocean and wave must be disabled when running in '
                'multi-storm mode ([config] run_ocean and run_wave '
                'must = no).')
        # if fake multistorm runwrf:
        runwrf=hwrf.multistorm.WRFAtmosMultiStorm(
            self.ds,self.conf,fcst_sec,self.wrf,taskname='runwrf')

        fakeinit=hwrf.multistorm.FakeInit(
            self.ds,self.conf,'fakeinit',self.wrf,prioritystorm,
            self.stormNinner,self.stormNouter)
        runwrf.add_metgrid(fakeinit).add_geogrid(fakeinit) \
            .add_fort65(fakeinit).add_wrfbdy(fakeinit)

        # Using fakeinit so gfs_init/realinit wrfinput is delivered 
        # to the finalmerge  work directory.
        self.finalmerge.add_wrfinput(fakeinit,prioritystorm)
        for i in range(1,num_realstorms + 1):
            stormid=self.multistorm_sids[i-1]
            self.finalmerge.add_merge(fakeinit,stormid)
            runwrf.add_wrfanl(fakeinit,self.stormNouter['storm%souter'%i])
            runwrf.add_wrfanl(fakeinit,self.stormNinner['storm%sinner'%i])
        runwrf.add_wrfinput(self.finalmerge)
        self.runwrf=runwrf
