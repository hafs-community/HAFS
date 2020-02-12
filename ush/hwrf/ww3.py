"""This module handles WW3 related scripts for HWRF system."""

__all__ = ['WW3Init', 'WRFWW3POM', 'WW3Post', 'WRFWW3HYCOM' ]

import os, re
import produtil.datastore, produtil.fileop, produtil.cd, produtil.run, produtil.log
import hwrf.mpipomtc, hwrf.hwrftask, hwrf.numerics, hwrf.exceptions, hwrf.namelist
import hwrf.coupling, hwrf.input, hwrf.hycom

from produtil.datastore import FileProduct, RUNNING, COMPLETED, FAILED, UpstreamFile
from produtil.fileop import make_symlink, deliver_file, wait_for_files
from produtil.cd import NamedDir
from produtil.run import run, runstr, checkrun, exe, bigexe, alias
from hwrf.numerics import to_datetime, to_datetime_rel, to_fraction, to_timedelta
from hwrf.exceptions import WaveInitFailed, WW3InputError

prodnames={ 
    'mod_def':       ( './mod_def.ww3', '{com}/{out_prefix}.mod_def.ww3' ),
    'wind':          ( './wind.ww3', '{com}/{out_prefix}.wind.ww3' ),
    'current':       ( './current.ww3', '{com}/{out_prefix}.current.ww3' ),
    'restart':       ( './restart.ww3', '{com}/{out_prefix}.restart_init.ww3' ),
    'ww3_shel':      ( './ww3_shel.inp', '{com}/{out_prefix}.ww3_shel.inp' ) }

########################################################################
class WW3Init(hwrf.hwrftask.HWRFTask): 
    def __init__(self,dstore,conf,section,taskname=None,fcstlen=126,
                 outstep=21600, pntstep=21600, rststep=21600, **kwargs):
        """Creates a WW3Init
          dstore - the produtil.datastore.Datastore to use
          conf - the HWRFConfig to use
          section - the section name for this task
          taskname - the task name.  Default: section
          fcstlen - the forecast length in hours
          outstep - the output step in seconds
          pntstep - the pntout step in seconds
          rststep - the restart output step in seconds
        Other keyword arguments are passed to the superclass constructor."""
        super(WW3Init,self).__init__(dstore,conf,section,taskname=taskname,**kwargs)
        self._make_products()
        self.fcstlen=float(fcstlen)
        self.outstep=int(outstep)
        self.pntstep=int(pntstep)
        self.rststep=int(rststep)
    def _make_products(self):
        """Creates FileProduct objects for all output files.  The
        outdir is the directory to which the pom package output its
        final files."""
        self._products=dict()
        atime=hwrf.numerics.to_datetime(self.conf.cycle)
        with self.dstore.transaction():
            for prodname,filepaths in prodnames.iteritems():
                (localpath,compath)=filepaths
                prod=produtil.datastore.FileProduct(
                    self.dstore,prodname,self.taskname)
                prod.location=self.timestr(compath,atime,atime)
                prod['localpath'] = localpath
                self._products[prodname]=( prod,localpath )
    def products(self,name=None,**kwargs):
        """Iterate over all products."""
        for prodname,stuff in self._products.iteritems():
            (prod,localpath)=stuff
            if name is None or name==prod.prodname:
                yield prod

    def inputiter(self):
        atime=to_datetime(self.conf.cycle)
        etime=to_datetime_rel(3600*self.fcstlen,atime)
        interval=to_fraction(self.confint('input_step',6*3600))

        dataset=self.confstr('gfs_dataset','gfs')
        item=self.confstr('gfs_item','gfs')

        epsilon=to_timedelta(interval/10)
        ende=to_datetime_rel(epsilon,etime)
        when=atime
        while when<ende:
            yield dict(self.taskvars,dataset=dataset,item=item,ftime=when,atime=atime)
            when=to_datetime_rel(interval,when)

    def gfsgrib2iter(self):
        logger=self.log()
        atime=to_datetime(self.conf.cycle) # sim start time
        etime=to_datetime_rel(self.fcstlen*3600,atime) # sim end time
        interval=to_fraction(self.confint('input_step',6*3600))
        dataset=self.confstr('gfs_dataset','gfs')
        item=self.confstr('gfs_item','gfs')
        hd=self.confstr('catalog','hwrfdata')
        dc=hwrf.input.DataCatalog(self.conf,hd,atime)
        epsilon=to_timedelta(interval/10)
        ende=to_datetime_rel(epsilon,etime)
        when=atime
        fhour=0
        maxwait=self.confint('max_grib_wait',3600)
        sleeptime=self.confint('grib_sleep_time',20)
        min_size=self.confint('min_grib_size',1)
        min_mtime_age=self.confint('min_grib_age',30)
        while when<ende:
            thefile=dc.locate(dataset=dataset,item=item,ftime=when,atime=atime,**self.taskvars)
            if self.realtime:
                waited=wait_for_files(
                    [thefile],logger,maxwait=maxwait,sleeptime=sleeptime,
                    min_size=min_size,min_mtime_age=min_mtime_age)
                if not waited:
                    msg='%s: did not exist or was too small after %d seconds'%(
                        thefile,min_size)
                    self.log().error(msg)
                    raise hwrf.exceptions.WW3InputError(msg)
            yield thefile
            fhour=fhour+interval/3600
            when=to_datetime_rel(interval,when)

    def deliver_products(self):
        logger=self.log()
        for prodname,stuff in self._products.iteritems():
            (prod,localpath)=stuff
            prod.deliver(frominfo=localpath,keep=False,logger=logger)

    def run(self):
        """Runs the WW3 initialization"""
        logger=self.log()
        dummycurr=True
        usegfswind=self.confstr('usegfswind','yes')
        if usegfswind == 'yes':
            dummywind=False
        elif usegfswind == 'no':
            dummywind=True
        else:
            # Wrong usegfswind value
            logger.warning('Wrong usegfswind value: %s. Assume usegfswind=yes.' 
                           'Set dummywind to False.'%(usegfswind,))
            usegfswind='yes'
            dummywind=False
        try:
            self.state=RUNNING
            redirect=self.confbool('redirect',True)
            with NamedDir(self.workdir,keep=not self.scrub,logger=logger,rm_first=True) as d:
                # Run ww3_grid
                def link(s,t):
                    make_symlink(s,t,force=True,logger=logger)
                deliver_file(self.icstr('{grid_inp}'),'ww3_grid.inp',keep=True,logger=logger)
                link(self.icstr('{grid_bot}'),'.')
                link(self.icstr('{grid_msk}'),'.')
                link(self.icstr('{grid_obr}'),'.')
                link(self.getexe('ww3_grid'),'ww3_grid')
                #checkrun(exe(self.getexe('ww3_grid'))>='ww3_grid.log',logger=logger)
                cmd=exe('./ww3_grid')
                if redirect: cmd = cmd>='ww3_grid.log'
                checkrun(cmd,logger=logger)

                if usegfswind == 'yes': 
                    # Extract gfs wind from gfs grib2 data 
                    ncfile='gfs.uvgrd10m.nc' 
                    produtil.fileop.remove_file(ncfile,logger=logger)
                    cmd=alias(bigexe(self.getexe('wgrib2','wgrib2')))
                    for f in self.gfsgrib2iter(): 
                        logger.info('Extracting wind at 10 m from %s'%(f))
                        subset=''
                        for line in runstr(cmd[f],logger=logger).splitlines(True):
                            if re.search(':[UV]GRD:10 m above ground:',line):
                                subset+=line
                        runme=cmd[f,'-i', '-append', '-netcdf', ncfile] << subset
                        checkrun(runme, logger=logger)

                    if produtil.fileop.isnonempty(ncfile):
                        dummywind=False
                    else:
                        dummywind=True
                        produtil.log.jlogger.warning(
                            'ww3init: will use dummy wind because %s is missing '
                            'or empty.'%(ncfile,))
                        
                if dummywind:
                    # Run ww3_prep for dummy wind
                    deliver_file(self.icstr('{wind_inp}'),'ww3_prep.inp',keep=True,logger=logger)
                    link(self.getexe('ww3_prep'),'ww3_prep')
                    #checkrun(exe(self.getexe('ww3_prep'))>='ww3_prep_wind.log',logger=logger)
                    cmd=exe('./ww3_prep')
                    if redirect: cmd = cmd>='ww3_prep_wind.log'
                    checkrun(cmd,logger=logger)
                else:
                    # Run ww3_prnc for prep gfs wind
                    deliver_file(self.icstr('{prnc_inp_gfswind}'),'ww3_prnc.inp',keep=True,logger=logger)
                    link(self.getexe('ww3_prnc'),'ww3_prnc')
                    cmd=exe('./ww3_prnc')
                    if redirect: cmd = cmd>='ww3_prnc_wind.log'
                    checkrun(cmd,logger=logger)

                if dummycurr: 
                    # Run ww3_prep for dummy current
                    deliver_file(self.icstr('{curr_inp}'),'ww3_prep.inp',keep=True,logger=logger)
                    link(self.getexe('ww3_prep'),'ww3_prep')
                    #checkrun(exe(self.getexe('ww3_prep'))>='ww3_prep_curr.log')
                    cmd=exe('./ww3_prep')
                    if redirect: cmd = cmd>='ww3_prep_curr.log'
                    checkrun(cmd,logger=logger)
                else:
                    # Extract current from global ocean model 
                    logger.error('Not implemented yet')

                have_restart=False
                oldrst='(unknown)'
                try:
                    oldrst=self.icstr('{oldcom}/{oldvit[stormid3lc]}.restart.f006.ww3')
                    if produtil.fileop.isnonempty(oldrst):
                        produtil.fileop.deliver_file(oldrst,'restart.ww3',logger=logger)
                        have_restart=True
                        produtil.log.jlogger.info('%s: warm start for wave'%(oldrst,))
                    else:
                        produtil.log.jlogger.warning(
                            'restart.ww3: will generate dummy because %s is missing '
                            'or empty.'%(oldrst,))
                except Exception as ee:
                    produtil.log.jlogger.warning(
                        'restart.ww3: will generate dummy because %s is missing '
                        'or could not be copied; %s'%(oldrst,str(ee)),exc_info=True)

                if not have_restart:
                    logger.info('restart.ww3: generating dummy with ww3_strt')
                    # Run ww3_strt
                    deliver_file(self.icstr('{strt_inp}'),'ww3_strt.inp',keep=True,logger=logger)
                    link(self.getexe('ww3_strt'),'ww3_strt')
                    cmd=exe('./ww3_strt')
                    if redirect: cmd = cmd>='ww3_strt.log'
                    checkrun(cmd,logger=logger)

                if redirect: self._copy_log()

                # Prepare ww3_shel.inp
                ni=hwrf.namelist.NamelistInserter(self.conf,self.section)
                shel_inp=self.icstr('{shel_inp}')
                atime=to_datetime(self.conf.cycle) # sim start time
                etime=to_datetime_rel(self.fcstlen*3600,atime) # sim end time
                flddt=int(self.outstep)
                pntdt=int(self.pntstep)
                #flddt=self.conf.getint('forecast_products','ww3_output_step',10800)
                #pntdt=self.conf.getint('forecast_products','ww3_pntout_step',10800)
                if pntdt > 0:
                    # Point output requested, need to provide buoy information
                    buoy_inp=self.icstr('{buoy_inp}')
                    with open(buoy_inp,'r') as bf:
                        #Read the file content and take out the eof character in the end.
                        buoyfile=bf.read()[:-1]
                elif pntdt == 0:
                    # Point output no requested, no further info needed
                    buoyfile='$'
                else:
                    # Wrong pntdt value
                    logger.warning('Wrong ww3_pntout_step value: %d. Set ww3_pntout_step = 0'%(pntdt,))
                    pntdt=0
                    self.pntout=0
                    buoyfile='$'
                ci=self.conf.getfloat('config','cycling_interval',6)
                retime=to_datetime_rel(ci*3600*1,atime) # restart end time
                invars=dict()
                invars.update(RUN_BEG=atime.strftime('%Y%m%d %H%M%S'),
                              RUN_END=etime.strftime('%Y%m%d %H%M%S'),
                              FLD_BEG=atime.strftime('%Y%m%d %H%M%S'),
                              FLD_END=etime.strftime('%Y%m%d %H%M%S'),
                              FLD_DT=int(flddt), 
                              PNT_BEG=atime.strftime('%Y%m%d %H%M%S'),
                              PNT_END=etime.strftime('%Y%m%d %H%M%S'),
                              PNT_DT=int(pntdt), 
                              BUOY_FILE=buoyfile,
                              RST_BEG=atime.strftime('%Y%m%d %H%M%S'),
                              RST_END=retime.strftime('%Y%m%d %H%M%S'),
                              RST_DT=int(self.rststep) )
                
                with open(shel_inp,'rt') as nf:
                    with open('ww3_shel.inp','wt') as of:
                        of.write(ni.parse(nf,logger=logger,source=shel_inp,
                                          raise_all=True,atime=self.conf.cycle,**invars))
          
                self.deliver_products()
            self.state=COMPLETED
        except Exception as e:
            logger.error('Unhandled exception in wave init: %s'
                         %(str(e),),exc_info=True)
            self.state=FAILED
            self._copy_log()
            raise

    def _copy_log(self):
        logger=self.log()
        for lf in [ 'ww3_grid.log', 'ww3_prep_wind.log', 'ww3_prep_curr.log'
                    'ww3_strt.log' ]:
            comloc=self.icstr('{com}/{out_prefix}.{lf}.ww3',lf=lf)
            if os.path.exists(lf):
                deliver_file(lf,comloc,keep=True,logger=logger)

########################################################################
class WW3Initer(hwrf.coupling.ComponentIniter):
    """This is an internal implementation class that should never be
    used directly. It instructs the hwrf.coupling.CoupledWRF to call
    the WRFWW3POM.copy_ww3_inputs to check or link WW3 input data."""
    def __init__(self,wcp):
        """Creates a WW3Initer that will pass control to the given
        WRFWW3POM object, stored as self.wcp."""
        self.wcp=wcp
    def check_coupled_inputs(self,logger):
        """Calls the WRFWW3POM.copy_ww3_inputs with just_check=True."""
        return self.wcp.copy_ww3_inputs(just_check=True)
    def link_coupled_inputs(self,just_check,logger):
        """Calls the WRFWW3POM.copy_ww3_inputs passing just_check."""
        return self.wcp.copy_ww3_inputs(bool(just_check))

########################################################################

def copy_ww3_inputs(ww3init,just_check=False):
    n_copied=0
    logger=ww3init.log()
    for prod in ww3init.products():
        localname=prod['localpath']
        loc=prod.location
        avail=prod.available
        if not avail or not loc or not localname:
            prod.update()
            localname=prod['localpath']
            loc=prod.location
            avail=prod.available
            if not avail or not loc or not localname:
                msg='WW3 product %s (available=%s location=%s localname=%s)'\
                    ' is not available or has an empty location'%( 
                       prod.did,repr(prod.available), repr(prod.location),
                       repr(localname))
                if just_check:
                    logger.warning(msg)
                    return False
                else:
                    logger.error(msg)
                    raise hwrf.exceptions.WW3InputError(msg)
        if not just_check:
            deliver_file(loc,os.path.basename(localname),keep=True,
                         logger=logger)
        n_copied+=1
    logger.info('Copied %d WW3 inputs.  Returning True.'%(n_copied))
    return True

def add_ww3_product(task,rstbeg,rstdt,rstend,stream='ww3rst',fmt='restart%03d.ww3'):
    atime=task.conf.cycle
    beg=to_datetime_rel(rstbeg,atime)
    end=to_datetime_rel(rstend,atime)
    dt=to_fraction(rstdt)
    epsilon=dt/10
    eend=to_datetime_rel(epsilon,end)
    times=list()
    now=beg
    while now<eend:
        assert(now>atime)
        times.append(now)
        now=to_datetime_rel(dt,now)
    task.add_coupled_stream(stream,times)
    itime=0
    with task.dstore.transaction() as t:
        for time in times:
            # itime = 1, 2, 3, 4, ...
            # time = rstbeg, rstbeg+dt, rstbeg+2dt, ...
            itime+=1
            if '%' in fmt:
                filename=fmt % itime # restart001.ww3
            else:
                filename=fmt
            prodname=filename    # restart001.ww3
            location=os.path.join(task.location,filename) # /path/to/runwrf/restart001.ww3
            
            # The prod is the product that represents the restart output file.
            prod=UpstreamFile(task.dstore,category=task.taskname,
                              prodname=prodname,location=location)
            prod['stream']=stream
            prod['location']=location
            prod['minsize']=72
            prod['restarttime']=round(to_fraction(time-atime)/3600)
            prod['minage']=30
            
            task.add_coupled_product(stream,time,prod)
    # Say something like:
    # Created 4 output ww3rst products from 201508160600 to 201508170000
    task.log().debug('Created %d output %s products from %s to %s'%(
            itime, stream, beg.strftime('%Y%m%d%H%M'), end.strftime('%Y%m%d%H%M')))

########################################################################
class WRFCoupledWW3(hwrf.coupling.CoupledWRF):
    """Runs a WRF-WW3 coupled simulation (no ocean).  Most of the work
    of this class is done by the superclasses.  This class adds code
    to copy the inputs needed by WW3 and the coupler.  There are three
    critical new config section values:

        wm3c_ranks = number of coupler ranks.  Default: 1
        ww3_ranks = number of WW3 ranks.  Default: 24
        wrf_ranks = nubmer of WRF ranks.  No default.  This one is
          mandatory."""
    def __init__(self,dstore,conf,section,wrf,keeprun=True,
                 wrfdiag_stream='auxhist1',ww3init=None,**kwargs):
        if not isinstance(ww3init,WW3Init):
            raise TypeError(
                'The ww3init argument to WRFCoupledWW3.__init__ must be a '
                'WW3Init object.  You passed a %s %s.'%
                (type(ww3init).__name__,repr(ww3init)))
        super(WRFCoupledWW3,self).__init__(dstore,conf,section,wrf,keeprun,
                                           wrfdiag_stream,**kwargs)
        self.ww3init=ww3init
        ww3initer=WW3Initer(self)
        self.couple('coupler','hwrf_wm3c','wm3c_ranks',1)
        self.couple('ww3','hwrf_ocean_fcst','ww3_ranks',48,ww3initer)
        self.couplewrf()

        ci=self.conf.getfloat('config','cycling_interval',6)
        pntdt=ww3init.pntstep
        if pntdt>0:
            add_ww3_product(self,wrf.simend(),3600,wrf.simend(),'ww3pnt','out_pnt.ww3')

        rstdt=ww3init.rststep
        if rstdt>0:
            # Currently only ouput one restart file from wave model
            add_ww3_product(self,rstdt,rstdt,ci*1*3600,'ww3rst','restart%03d.ww3')

        add_ww3_product(self,wrf.simend(),3600,wrf.simend(),'ww3out','out_grd.ww3')
        add_ww3_product(self,wrf.simend(),3600,wrf.simend(),'ww3mdldef','mdl_def.ww3')
    def remove_wave(self):
        self.uncouple()

    def copy_ww3_inputs(self,just_check=False):
        return copy_ww3_inputs(self.ww3init,just_check)

########################################################################

def wave_setup(self,ww3init,wrf):
    """!Helper function for WRFWW3POM and WRFWW3HYCOM constructors to
    reduce code duplication.  Adds WW3 products to the forecast
    output.  Should be run after superclass constructor."""
    ci=self.conf.getfloat('config','cycling_interval',6)
    pntdt=ww3init.pntstep
    if pntdt>0:
        add_ww3_product(self,wrf.simend(),3600,wrf.simend(),'ww3pnt','out_pnt.ww3')

    rstdt=ww3init.rststep
    if rstdt>0:
        # Currently only ouput one restart file from wave model
        add_ww3_product(self,rstdt,rstdt,ci*1*3600,'ww3rst','restart%03d.ww3')

    add_ww3_product(self,wrf.simend(),3600,wrf.simend(),'ww3out','out_grd.ww3')
    add_ww3_product(self,wrf.simend(),3600,wrf.simend(),'ww3moddef','mod_def.ww3')

########################################################################

class WRFWW3POM(hwrf.mpipomtc.WRFCoupledPOM):
    def __init__(self,dstore,conf,section,wrf,keeprun=True,
                 wrfdiag_stream='auxhist1',pominit=None,ww3init=None,
                 **kwargs):
        if not isinstance(ww3init,WW3Init):
            raise TypeError(
                'The ww3init argument to WRFCoupledWW3.__init__ must be a '
                'WW3Init object.  You passed a %s %s.'%
                (type(ww3init).__name__,repr(ww3init)))
        self.ww3init=ww3init
        self.ww3initer=WW3Initer(self)
        super(WRFWW3POM,self).__init__(dstore,conf,section,wrf,keeprun,wrfdiag_stream,pominit,**kwargs)
        wave_setup(self,ww3init,wrf)
        
    def remove_ocean(self):
        self.uncouple('pom')

    def remove_wave(self):
        self.uncouple('ww3')

    def _add_wave(self):
        self.couple('ww3','ww3_shel','ww3_ranks',48,self.ww3initer)

    def copy_ww3_inputs(self,just_check=False):
        return copy_ww3_inputs(self.ww3init,just_check)

########################################################################

class WRFWW3HYCOM(hwrf.hycom.WRFCoupledHYCOM):
    def __init__(self,dstore,conf,section,wrf,ocstatus,keeprun=True,
                 wrfdiag_stream='auxhist1',hycominit=None,ww3init=None,
                 **kwargs):
        if not isinstance(ww3init,WW3Init):
            raise TypeError(
                'The ww3init argument to WRFWW3HYCOM.__init__ must be a '
                'WW3Init object.  You passed a %s %s.'%
                (type(ww3init).__name__,repr(ww3init)))
        self.ww3init=ww3init
        self.ww3initer=WW3Initer(self)
        super(WRFWW3HYCOM,self).__init__(dstore,conf,section,wrf,ocstatus,keeprun,
                                         wrfdiag_stream,hycominit,**kwargs)
        wave_setup(self,ww3init,wrf)

    def remove_ocean(self):
        self.uncouple('hycom')

    def remove_wave(self):
        self.uncouple('ww3')

    def _add_wave(self):
        self.couple('ww3','ww3_shel','ww3_ranks',48,self.ww3initer)

    def copy_ww3_inputs(self,just_check=False):
        return copy_ww3_inputs(self.ww3init,just_check)

########################################################################
class WW3Post(hwrf.hwrftask.HWRFTask):
    """Run WW3 post-process."""
    def __init__(self,ds,conf,section,outstep,pntstep,ww3,**kwargs):
        super(WW3Post,self).__init__(ds,conf,section,**kwargs)
        self.outstep=outstep
        self.pntstep=pntstep
        self.ww3=ww3 # the WW3-coupled subclass of WRFAtmos
        self.make_products()
        self._ncks_path=False

    def products(self,**kwargs):
        yield self._ww3gribprod
        redirect=self.confbool('redirect',True)
        if redirect: yield self._ww3griblog
        yield self._ww3ounfprod
        redirect=self.confbool('redirect',True)
        if redirect: yield self._ww3ounflog
        yield self._ww3ounpprod
        redirect=self.confbool('redirect',True)
        if redirect: yield self._ww3ounplog
        yield self._ww3outpbullprod
        redirect=self.confbool('redirect',True)
        if redirect: yield self._ww3outpbulllog
        yield self._ww3outpspecprod
        redirect=self.confbool('redirect',True)
        if redirect: yield self._ww3outpspeclog

    def make_products(self):
        redirect=self.confbool('redirect',True)
        self._ww3gribprod=produtil.datastore.FileProduct(
            self.dstore,"ww3.grb2",self.taskname)
        if redirect:
            self._ww3griblog =produtil.datastore.FileProduct(
                self.dstore,"ww3grb2.log",self.taskname)
        self._ww3ounfprod=produtil.datastore.FileProduct(
            self.dstore,"ww3ounf.nc",self.taskname)
        if redirect:
            self._ww3ounflog =produtil.datastore.FileProduct(
                self.dstore,"ww3ounf.log",self.taskname)
        self._ww3ounpprod=produtil.datastore.FileProduct(
            self.dstore,"ww3ounpspec.nc",self.taskname)
        if redirect:
            self._ww3ounplog =produtil.datastore.FileProduct(
                self.dstore,"ww3ounpspec.log",self.taskname)
        self._ww3outpbullprod=produtil.datastore.FileProduct(
            self.dstore,"ww3outpbull.tar",self.taskname)
        if redirect:
            self._ww3outpbulllog =produtil.datastore.FileProduct(
                self.dstore,"ww3outpbull.log",self.taskname)
        self._ww3outpspecprod=produtil.datastore.FileProduct(
            self.dstore,"ww3outpspec.tar",self.taskname)
        if redirect:
            self._ww3outpspeclog =produtil.datastore.FileProduct(
                self.dstore,"ww3outpspec.log",self.taskname)

    def __copy_ncks(self,source,target,ignore):
        ncks=self.ncks_path
        logger=self.log()
        produtil.fileop.remove_file(target,logger=logger)
        checkrun(bigexe(ncks)['-4','-L','6',source,target]<'/dev/null',
                 logger=logger)
    @property 
    def ncks_path(self):
        """Returns the path to ncks.  Returns None if ncks cannot be
        found.  This function will only search for ncks once, and will
        cache the result.  Set self._ncks_path=False to force a
        recheck."""
        if self._ncks_path is False:
            ncks=self.getexe('ncks','')
            if not self._ncks_path:
                ncks=produtil.fileop.find_exe('ncks',raise_missing=False)
            assert(ncks is None or 
                   (isinstance(ncks,basestring) and ncks!=''))
            self._ncks_path=ncks
        return self._ncks_path

    def run(self):
        """Run the WW3 post."""
        logger=self.log()
        redirect=self.confbool('redirect',True)
        self.state=RUNNING
        try:
            with NamedDir(self.workdir,keep=True,logger=logger,rm_first=True) as d:
                mdprod=[ p for p in self.ww3.products(stream='ww3moddef') ] [0]
                mdprod.check()
                if not mdprod or not mdprod.available or not mdprod.location:
                    logger.error('%s: mod_def.ww3 not yet available from forecast'%(
                            repr(mdprod),))
                ogprod=[ p for p in self.ww3.products(stream='ww3out') ] [0]
                ogprod.check()
                if not ogprod or not ogprod.available or not ogprod.location:
                    logger.error('%s: out_grd.ww3 not yet available from forecast'%(
                            repr(ogprod),))
                make_symlink(mdprod.location,'mod_def.ww3',force=True,logger=logger)
                # For field output in grib2 format
                ww3_grib_post=self.confstr('ww3_grib_post','yes',section='ww3post')
                if ww3_grib_post == 'yes' and self.outstep>0:
                    make_symlink(ogprod.location,'out_grd.ww3',force=True,logger=logger)
                    make_symlink(self.getexe('ww3_grib'),'ww3_grib',force=True,logger=logger)
                    # Prepare the namelist
                    self.make_grib_inp(logger)
                    cmd=exe('./ww3_grib')
                    if redirect: cmd = cmd>='ww3_grib.log'
                    checkrun(cmd,logger=logger)
                    ww3_grib_out=self.conf.cycle.strftime('gribfile')
                    ww3_grib_out_com=self.icstr('{com}/{out_prefix}.ww3.grb2')
                    self._ww3gribprod.deliver(frominfo=ww3_grib_out,
                                         location=ww3_grib_out_com,
                                         logger=logger,copier=None)
                # For field output in netcdf format
                ww3_ounf_post=self.confstr('ww3_ounf_post','yes',section='ww3post')
                if ww3_ounf_post == 'yes' and self.outstep>0:
                    make_symlink(ogprod.location,'out_grd.ww3',force=True,logger=logger)
                    make_symlink(self.getexe('ww3_ounf'),'ww3_ounf',force=True,logger=logger)
                    # Prepare the namelist
                    self.make_ounf_inp(logger)
                    # Run ww3_ounf
                    cmd=exe('./ww3_ounf')
                    if redirect: cmd = cmd>='ww3_ounf.log'
                    checkrun(cmd,logger=logger)
                    #ww3_ounf_out=self.conf.cycle.strftime('ww3.%Y%m%d %H.nc')
                    ww3_ounf_out=self.conf.cycle.strftime('ww3.%Y.nc')
                    ww3_ounf_out_com=self.icstr('{com}/{out_prefix}.ww3_ounf.nc')
                    self._ww3ounfprod.deliver(frominfo=ww3_ounf_out,
                                         location=ww3_ounf_out_com,
                                         logger=logger,copier=self.__copy_ncks)
                # For point spec output in netcdf format
                ww3_ounp_spec_post=self.confstr('ww3_ounp_spec_post','yes',section='ww3post')
                if ww3_ounp_spec_post == 'yes' and self.pntstep>0:
                    opprod=[ p for p in self.ww3.products(stream='ww3pnt') ] [0]
                    opprod.check()
                    if not opprod or not opprod.available or not opprod.location:
                        logger.error('%s: out_pnt.ww3 not yet available from forecast'%(
                                repr(opprod),))
                    make_symlink(opprod.location,'out_pnt.ww3',force=True,logger=logger)
                    make_symlink(self.getexe('ww3_ounp'),'ww3_ounp',force=True,logger=logger)
                    # Prepare the namelist
                    self.make_ounp_spec_inp(logger)
                    # Run ww3_ounp
                    cmd=exe('./ww3_ounp')
                    if redirect: cmd = cmd>='ww3_ounp.log'
                    checkrun(cmd,logger=logger)
                    #ww3_ounp_out=self.conf.cycle.strftime('ww3.%Y%m%d %H.nc')
                    ww3_ounp_out=self.conf.cycle.strftime('ww3.%Y_spec.nc')
                    ww3_ounp_out_com=self.icstr('{com}/{out_prefix}.ww3_ounp_spec.nc')
                    self._ww3ounpprod.deliver(frominfo=ww3_ounp_out,
                                         location=ww3_ounp_out_com,
                                         logger=logger,copier=self.__copy_ncks)
                # For point output ww3_outp
                ww3_outp_bull_post=self.confstr('ww3_outp_bull_post','yes',section='ww3post')
                ww3_outp_spec_post=self.confstr('ww3_outp_spec_post','yes',section='ww3post')
                if self.pntstep>0:
                    opprod=[ p for p in self.ww3.products(stream='ww3pnt') ] [0]
                    opprod.check()
                    if not opprod or not opprod.available or not opprod.location:
                        logger.error('%s: out_pnt.ww3 not yet available from forecast'%(
                                repr(opprod),))
                    make_symlink(opprod.location,'out_pnt.ww3',force=True,logger=logger)
                    make_symlink(self.getexe('ww3_outp'),'ww3_outp',force=True,logger=logger)
                    # Need to get information abou the total number of buoys and their IDs
                    self.make_outp_info_inp(logger)
                    cmd=exe('./ww3_outp')
                    cmd = cmd>='ww3_outp_info.log'
                    checkrun(cmd,logger=logger)
                    fname='ww3_outp_info.log'
                    with open(fname) as f:
                        ww3_outp_info = f.readlines()
                    indices = [i for i, elem in enumerate(ww3_outp_info) if '----------' in elem]
                    buoys=ww3_outp_info[indices[0]+1:indices[1]-2]
                    # For point bullitin output
                    if ww3_outp_spec_post == 'yes':
                        fileout=[] 
                        filelog=[] 
                        for i, buoy in enumerate(buoys):
                            ipnt=i+1
                            buoyid=buoy.split()[0]
                            buoylon=buoy.split()[1]
                            buoylat=buoy.split()[2]
                            logger.info('ww3_outp_bull for buoy: %i, %s, %s, %s'%(ipnt,buoyid,buoylon,buoylat))
                            self.make_outp_bull_inp(ipnt,logger)
                            cmd=exe('./ww3_outp')
                            cmd = cmd>='ww3_outp_bull_'+buoyid+'.log'
                            #checkrun(cmd,logger=logger)
                            produtil.run.run(cmd)
                            fileout.append(buoyid+'.bull')
                            fileout.append(buoyid+'.csv')
                            filelog.append('ww3_outp_bull_'+buoyid+'.log')
                        # Tar the outputs and diliver to com dir
                        cmd=exe('tar')['-cvf', 'ww3_outp_bull.tar'][fileout]
                        checkrun(cmd,logger=logger)
                        cmd=exe('cat')[filelog] >> 'ww3_outp_bull.log'
                        checkrun(cmd,logger=logger)
                        ww3_outp_bull='ww3_outp_bull.tar'
                        ww3_outp_bull_com=self.icstr('{com}/{out_prefix}.ww3_outp_bull.tar')
                        self._ww3outpbullprod.deliver(frominfo=ww3_outp_bull,
                                             location=ww3_outp_bull_com,
                                             logger=logger,copier=None)
                    # For point spec output
                    if ww3_outp_spec_post == 'yes':
                        fileout=[] 
                        filelog=[] 
                        ww3tstr=self.conf.cycle.strftime('%y%m%d%H')
                        for i, buoy in enumerate(buoys):
                            ipnt=i+1
                            buoyid=buoy.split()[0]
                            buoylon=buoy.split()[1]
                            buoylat=buoy.split()[2]
                            logger.info('ww3_outp_spec for buoy: %i, %s, %s, %s'%(ipnt,buoyid,buoylon,buoylat))
                            self.make_outp_spec_inp(ipnt,logger)
                            cmd=exe('./ww3_outp')
                            cmd = cmd>='ww3_outp_spec_'+buoyid+'.log'
                            checkrun(cmd,logger=logger)
                            deliver_file('ww3.'+ww3tstr+'.spc',buoyid+'.spc',keep=False,logger=logger)
                            fileout.append(buoyid+'.spc')
                            filelog.append('ww3_outp_spec_'+buoyid+'.log')
                        # Tar the outputs and diliver to com dir
                        cmd=exe('tar')['-cvf', 'ww3_outp_spec.tar'][fileout]
                        checkrun(cmd,logger=logger)
                        cmd=exe('cat')[filelog] >> 'ww3_outp_spec.log'
                        checkrun(cmd,logger=logger)
                        ww3_outp_spec='ww3_outp_spec.tar'
                        ww3_outp_spec_com=self.icstr('{com}/{out_prefix}.ww3_outp_spec.tar')
                        self._ww3outpspecprod.deliver(frominfo=ww3_outp_spec,
                                             location=ww3_outp_spec_com,
                                             logger=logger,copier=None)
            self.state=COMPLETED
        except Exception as e:
            self.state=FAILED
            logger.error("WW3 post failed: %s"%(str(e),),exc_info=True)
            raise

    def make_grib_inp(self,logger):
        # Prepare ww3_grib.inp
        ni=hwrf.namelist.NamelistInserter(self.conf,self.section)
        grib_inp=self.confstr('grib_inp','')
        if not grib_inp: grib_inp=self.icstr('{PARMww3}/ww3_grib.inp_tmpl')
        atime=to_datetime(self.conf.cycle) # sim start time
        invars=dict()
        invars.update(FLD_BEG=atime.strftime('%Y%m%d %H%M%S'),
                      FLD_DT=int(self.outstep),
                      RUN_BEG=atime.strftime('%Y%m%d %H%M%S'))
        with open(grib_inp,'rt') as nf:
            with open('ww3_grib.inp','wt') as of:
                of.write(ni.parse(nf,logger=logger,source=grib_inp,
                                  raise_all=True,atime=self.conf.cycle,**invars))

    def make_ounf_inp(self,logger):
        # Prepare ww3_ounf.inp
        ni=hwrf.namelist.NamelistInserter(self.conf,self.section)
        ounf_inp=self.confstr('ounf_inp','')
        if not ounf_inp: ounf_inp=self.icstr('{PARMww3}/ww3_ounf.inp_tmpl')
        atime=to_datetime(self.conf.cycle) # sim start time
        invars=dict()
        invars.update(FLD_BEG=atime.strftime('%Y%m%d %H%M%S'),
                      FLD_DT=int(self.outstep))
        with open(ounf_inp,'rt') as nf:
            with open('ww3_ounf.inp','wt') as of:
                of.write(ni.parse(nf,logger=logger,source=ounf_inp,
                                  raise_all=True,atime=self.conf.cycle,**invars))

    def make_ounp_spec_inp(self,logger):
        # Prepare ww3_ounp.inp
        ni=hwrf.namelist.NamelistInserter(self.conf,self.section)
        ounp_spec_inp=self.confstr('ounp_spec_inp','')
        if not ounp_spec_inp: ounp_spec_inp=self.icstr('{PARMww3}/ww3_ounp_spec.inp_tmpl')
        atime=to_datetime(self.conf.cycle) # sim start time
        invars=dict()
        invars.update(PNT_BEG=atime.strftime('%Y%m%d %H%M%S'),
                      PNT_DT=int(self.pntstep))
        with open(ounp_spec_inp,'rt') as nf:
            with open('ww3_ounp.inp','wt') as of:
                of.write(ni.parse(nf,logger=logger,source=ounp_spec_inp,
                                  raise_all=True,atime=self.conf.cycle,**invars))

    def make_outp_info_inp(self,logger):
        # Prepare ww3_outp.inp
        ni=hwrf.namelist.NamelistInserter(self.conf,self.section)
        outp_bull_inp=self.confstr('outp_bull_inp','')
        if not outp_bull_inp: outp_bull_inp=self.icstr('{PARMww3}/ww3_outp_bull.inp_tmpl')
        atime=to_datetime(self.conf.cycle) # sim start time
        invars=dict()
        invars.update(PNT_BEG=atime.strftime('%Y%m%d %H%M%S'),
                      PNT_DT=int(self.pntstep),
                      PNT_NUM='$',
                      RUN_BEG=atime.strftime('%Y%m%d %H%M%S'))
        with open(outp_bull_inp,'rt') as nf:
            with open('ww3_outp.inp','wt') as of:
                of.write(ni.parse(nf,logger=logger,source=outp_bull_inp,
                                  raise_all=True,atime=self.conf.cycle,**invars))

    def make_outp_bull_inp(self,ipnt,logger):
        # Prepare ww3_outp.inp
        ni=hwrf.namelist.NamelistInserter(self.conf,self.section)
        outp_bull_inp=self.confstr('outp_bull_inp','')
        if not outp_bull_inp: outp_bull_inp=self.icstr('{PARMww3}/ww3_outp_bull.inp_tmpl')
        atime=to_datetime(self.conf.cycle) # sim start time
        invars=dict()
        invars.update(PNT_BEG=atime.strftime('%Y%m%d %H%M%S'),
                      PNT_DT=int(self.pntstep),
                      PNT_NUM=int(ipnt),
                      RUN_BEG=atime.strftime('%Y%m%d %H%M%S'))
        with open(outp_bull_inp,'rt') as nf:
            with open('ww3_outp.inp','wt') as of:
                of.write(ni.parse(nf,logger=logger,source=outp_bull_inp,
                                  raise_all=True,atime=self.conf.cycle,**invars))

    def make_outp_spec_inp(self,ipnt,logger):
        # Prepare ww3_outp.inp
        ni=hwrf.namelist.NamelistInserter(self.conf,self.section)
        outp_spec_inp=self.confstr('outp_spec_inp','')
        if not outp_spec_inp: outp_spec_inp=self.icstr('{PARMww3}/ww3_outp_spec.inp_tmpl')
        atime=to_datetime(self.conf.cycle) # sim start time
        invars=dict()
        invars.update(PNT_BEG=atime.strftime('%Y%m%d %H%M%S'),
                      PNT_DT=int(self.pntstep),
                      PNT_NUM=int(ipnt),
                      RUN_BEG=atime.strftime('%Y%m%d %H%M%S'))
        with open(outp_spec_inp,'rt') as nf:
            with open('ww3_outp.inp','wt') as of:
                of.write(ni.parse(nf,logger=logger,source=outp_spec_inp,
                                  raise_all=True,atime=self.conf.cycle,**invars))

