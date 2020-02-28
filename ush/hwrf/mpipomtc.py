"""!Runs the POM initialization and POM-WRF coupled forecast.

This module handles the POM-coupled WRF simulation.  It contains two
critical pieces:

*  POMInit -- an HWRFTask that is a wrapper around the Python pom package.
*  WRFCoupledPOM - a subclass of hwrf.fcsttask.WRFAtmos that runs the
    WRF-POM two-way coupled system based on the output of the POMInit."""

##@var __all__
# The list of symbols exported by "from hwrf.mpipomtc import *"
__all__ = ['POMInit', 'WRFCoupledPOM']

import os, shutil, math, datetime
import produtil.datastore, produtil.fileop, produtil.cd, produtil.run
import produtil.rusage
import hwrf.hwrftask, hwrf.numerics, hwrf.exceptions, hwrf.fcsttask
import hwrf.coupling
import pom.master, pom.exceptions

from produtil.rusage import setrlimit, rusage, getrlimit
from produtil.datastore import UpstreamFile, wait_for_products, \
    COMPLETED, RUNNING, FAILED
from produtil.fileop import isnonempty, make_symlink, deliver_file
from produtil.cd import NamedDir
from produtil.run import mpirun, mpi
from hwrf.numerics import to_datetime, to_datetime_rel, to_fraction
from hwrf.exceptions import OceanInitFailed
from pom.exceptions import POMInputError

##@var prodnames
# Mapping from product names to a tuple.  Each tuple contains the work
# directory file location and the final output location, in that
# order.  Both directory locations are sent through hwrf.config.HWRFConfig
prodnames={ 'grid':           ( '{oceandir}/{vit[stormname]}.grid.nc',  
                                '{outdir}/{out_prefix}.pom.grid.nc'),
            'ts_initial':     ( '{oceandir}/{vit[stormname]}.ts_initial.nc', 
                                '{outdir}/{out_prefix}.pom.ts_initial.nc' ),
            'ts_clim':        ( '{oceandir}/{vit[stormname]}.ts_clim.nc',    
                                '{outdir}/{out_prefix}.pom.ts_clim.nc' ),
            'uv_initial':     ( '{oceandir}/{vit[stormname]}.uv_initial.nc', 
                                '{outdir}/{out_prefix}.pom.uv_initial.nc' ),
            'el_initial':     ( '{oceandir}/{vit[stormname]}.el_initial.nc', 
                                '{outdir}/{out_prefix}.pom.el_initial.nc' ),
            'restart.phase2': ( '{oceandir}/restart.phase2.nc',         
                                '{outdir}/{out_prefix}.pom.restart.phse2.nc'
                                ),
            'pom.nml':        ( '{nmldir}/pom.nml', 
                                '{outdir}/{out_prefix}.pom.nml' ) }
"""A mapping from product name to a two-element tuple.  The tuple
contains the path to the file in the local directory structure of the
pom package, and the destination file within the HWRF system.  Both
should be sent through string interpolation (strinterp or
timestrinterp) before use."""

class POMInit(hwrf.hwrftask.HWRFTask):
    """!A wrapper around the pom package that runs the POM initialization.

    This HWRFTask subclass is a wrapper around the pom package.  It
    runs the POM initialization, and generates the POM namelist for
    the forecast run."""
    def __init__(self,dstore,conf,section,taskname=None,vitfile=None,
                 fcstlen=None,outstep=86400,**kwargs):
        """!Creates a POMInit.
        @param dstore the produtil.datastore.Datastore to use
        @param conf the HWRFConfig to use
        @param section the section name for this task
        @param taskname the task name.  Default: section
        @param vitfile the vitals file with tcvitals for all times this
            storm has existed.  Default:
            self.icstr('{WORKhwrf}/{stormlabel}.vitals')
        @param fcstlen The forecast length in hours.
        @param outstep The output timestep in seconds.
        @param kwargs Other keyword arguments are passed to the superclass constructor."""
        super(POMInit,self).__init__(dstore,conf,section,taskname=taskname,
                                     outdir=conf.getdir('com'),**kwargs)
        
        # Make sure the output goes to com, whether the superclass
        # wants it to or not:
        self.outdir=conf.getdir('com')
        self['outdir']=conf.getdir('com')
        assert(self.outdir.find('intercom')<0)

        self._sfc_dataset = str(kwargs.get('sfc_dataset',self.confstr(
                    'sfc_dataset','hwrfdata')))
        self._loop_dataset = str(kwargs.get('loop_dataset',self.confstr(
                    'loop_dataset','hwrfdata')))
        self._sfcanl_item = str(kwargs.get('sfcanl_item',self.confstr(
                    'sfcanl_item','gfs_sfcanl')))
        self._sanl_item = str(kwargs.get('sanl_item',self.confstr(
                    'sanl_item','gfs_sanl')))
        self._loop_item = str(kwargs.get('loop_item',self.confstr(
                    'loop_item','gfdl_loop')))
        self._wc_ring_item = str(kwargs.get('wc_ring_item',self.confstr(
                    'wc_ring_item','gfdl_wc_ring')))
        self._rtofs_init_basins = str(kwargs.get('rtofs_init_basins',self.confstr(
                    'rtofs_init_basins','E')))
        self._atime=to_datetime(conf.cycle)
        self.__fcstlen=fcstlen
        self.__outstep=int(outstep)
        if self.__outstep<30: self.__outstep=86400
        if vitfile is None:
            vitfile=self.icstr('{WORKhwrf}/{stormlabel}.vitals')
        self._vitfile=vitfile
        if 'catalog' in kwargs and isinstance(kwargs['catalog'],
                                              hwrf.input.DataCatalog):
            self._catalog=kwargs['catalog']
        else:
            incat = str(kwargs.get('catalog',self.confstr(
                        'catalog','hwrfdata')))
            self._catalog=hwrf.input.DataCatalog(conf,incat,conf.cycle)

        self._products=dict()

        rundir=self.workdir
        outputdir=os.path.join(rundir,'output')
        self._make_products(outputdir)

    def run(self):
        """!Runs the POM initialization and copies the results to their
        destinations within the HWRF work area."""
        try:
            self.state=RUNNING
            logger=self.log()
            rundir=self.workdir
            assert(rundir)
            inputdir=os.path.join(rundir,'input')
            outputdir=os.path.join(rundir,'output')
            if os.path.exists(rundir):
                shutil.rmtree(rundir)
            with NamedDir(rundir,keep=True,logger=logger) as d:
                with NamedDir(inputdir,keep=True,logger=logger) as d:
                    self.get_inputs()
                with NamedDir(outputdir,keep=True,logger=logger) as d:
                    self.run_init(inputdir,outputdir)
                    self.deliver_products(os.path.join(outputdir,'OCEAN'),
                                          outputdir)
            self.state=COMPLETED
        except pom.exceptions.POMUnsupportedBasin as ue:
            logger.info('Basin is unsupported.')
            self.state=COMPLETED
            raise # caller needs to handle this
        except Exception as e:
            logger.error('Unhandled exception in ocean init: %s'
                         %(str(e),),exc_info=True)
            self.state=FAILED
            raise

    def _make_products(self,outdir):
        """!Creates FileProduct objects for all output files.  
        @param outdir The directory to which the pom package output its
        final files."""
        atime=self._atime
        oceandir=os.path.join(outdir,'OCEAN')
        with self.dstore.transaction():
            for prodname,filepaths in prodnames.iteritems():
                (localpath,compath)=filepaths
                prod=produtil.datastore.FileProduct(
                    self.dstore,prodname,self.taskname)
                assert(self.outdir.find('intercom')<0)
                prod.location=self.timestr(compath,atime,atime,
                                           outdir=self.outdir)
                assert(prod.location.find('intercom')<0)
                slocalpath=self.timestr(localpath,atime,atime,
                                        oceandir=oceandir,nmldir=outdir)
                prod['localpath']=slocalpath
                self._products[prodname]=( prod,slocalpath )

    def deliver_products(self,oceandir,nmldir,redeliver=True):
        """!Delivers files to their final destination
        Copies results to their destinations within the HWRF work areas
        @param   oceandir the OCEAN directory created by the pom package in run()
        @param nmldir the directory in which the forecast namelist was made
        @param redeliver if True, products are re-copied even if
            available=True"""
        assert(self._products)
        logger=self.log()
        good=True
        baddies=list()
        atime=self._atime
        produtil.fileop.makedirs(self.outdir,logger=logger)
        for prodname,stuff in self._products.iteritems():
            assert(isinstance(stuff,tuple))
            ( prod,localpath ) = stuff
            if prod.available and not redeliver:
                logger.info('%s: already available and redeliver=False, so '
                            'skipping this (available=%s location=%s)'%(
                        prod.did,repr(prod.available),repr(prod.location)))
                continue
            if not os.path.exists(localpath):
                logger.warning(
                    localpath+": expected output file does not exist.")
                good=False
                baddies.append(localpath+' (missing)')
                continue
            elif not isnonempty(localpath):
                logger.warning(localpath+": is empty.  Will deliver anyway."
                               +"  Beware of impending failures.")
                baddies.append(localpath+' (empty)')
            prod.deliver(frominfo=localpath,keep=False,logger=logger)
        if len(baddies)>0:
            msg='Some ocean outputs were empty or missing: '+\
                (', '.join(baddies))
            logger.warning(msg)
            if not good:
                logger.critical('Ocean init failed: '+msg)
                raise OceanInitFailed(msg)

    def run_init(self,inputdir,outputdir):
        """!Internal function that passes control to the pom package

        This internal implemenentation function passes control to
        the pom package.  This is part of the implementation of
        run().  Do not call directly except for debugging.
        @param inputdir the ocean input data directory
        @param outputdir the ocean data output directory"""
        CENTERID=self.storminfo.center.upper()
        EXEChwrf=self.getdir('EXEChwrf')
        PARMhwrf=self.getdir('PARMhwrf')
        FIXhwrf=os.path.join(self.getdir('FIXhwrf'),'hwrf-pom')
        VITDIR=inputdir
        GFSDIR=inputdir
        LCDIR=inputdir
        CSTREAM=outputdir
        COMIN=self.getdir('com')
        STARTDATE=self._atime.strftime('%Y%m%d%H')
        STORMID=self.storminfo.stormid3.upper()
        STORMNAME=self.storminfo.stormname.upper()
        kwargs=dict(logger=self.log(), conf=self.conf)
        method=self.confstr('method','')
        if method: kwargs['input_method']=method.upper()
        assert(GFSDIR.find('pom/output')<0)
        logger=self.log()
        setrlimit(logger=logger,stack=6e9,ignore=True)
        getrlimit(logger=logger)
        pom.master.run_init(STORMNAME,STORMID,STARTDATE,EXEChwrf,PARMhwrf,
                            FIXhwrf,VITDIR,GFSDIR,LCDIR,CSTREAM,COMIN,
                            fcstlen=self.__fcstlen,outstep=self.__outstep,
                            **kwargs)

    def products(self,name=None):
        """!Iterates over products

        Iterates over Product objects for all of the files that need
        to be copied to the forecast directory to run POM.  The
        products will all have a "localname" metadata value telling
        the local filename they should have in the forecast directory.
        @param name If given, only the product with this name is yielded"""
        if name is None:
            for p in self._products.itervalues(): yield p[0]
        else:
            if name in self._products: yield self._products[name][0]

    @property
    def rtofs_init_basins(self):
        """!Return a string containing the list of one-letter basins
        that use RTOFS initialization."""
        return self._rtofs_init_basins

    @property
    def parent_ocean_atime(self):
        """!Get the analysis time of the parent ocean model (RTOFS)."""
        cyc=self.conf.cycle
        return datetime.datetime(cyc.year,cyc.month,cyc.day)

    def parent_ocean_adi(self,idays=0):
        """!Get the atime, dataset and item of the parent model input,
        for hwrf.input.DataCatalog

        @return a tuple (oceanatime,dataset,item) suitable for
        hwrf.input.DataCatalog.get()

        @param idays A positive integer; the number of days to
        subtract from the RTOFS analysis time.  Negative numbers are
        ignored."""
        oceanatime=self.parent_ocean_atime
        if idays>=0.99:
            oceanatime = to_datetime_rel(round(idays)*-24*3600,oceanatime)

        dt = to_fraction(self.conf.cycle-oceanatime)
        rt00zonly = self.confint('rt00zonly',0)
        if rt00zonly>0:
            if abs(dt)<86400: # 0Z file requested if within one day of atime
                ocean_item=self.confstr('ocean_now','rtofs_now')
            else:
                ocean_item=self.confstr('ocean_fcst','rtofs_fcst')
        else:
            if abs(dt)<300: # 0Z file requested if within 5 minutes of atime
                ocean_item=self.confstr('ocean_now','rtofs_now')
            else:
                ocean_item=self.confstr('ocean_fcst','rtofs_fcst')        
        oceands=self.confstr('ocean_dataset','rtofs')

        return ( oceanatime, oceands, ocean_item )

    def inputiter(self):
        """!Iterates over all needed input data."""

        # Get the basin as it will be passed to the pom package.  This
        # pair of odd lines exactly reproduces the process that is
        # used:
        STORMID=self.storminfo.stormid3.upper()
        BASIN=STORMID[2].upper()        

        #if BASIN in self._rtofs_init_basins:
        init_data = self.confstr('ini_data','gdem').upper()
        if init_data == 'RTOF':
            hwrfatime=self.conf.cycle
            ( oceanatime, oceands, ocean_item ) = \
                self.parent_ocean_adi()

            yield dict(dataset=oceands,item=ocean_item,
                       atime=oceanatime,ftime=hwrfatime,ab='a')
            yield dict(dataset=oceands,item=ocean_item,
                       atime=oceanatime,ftime=hwrfatime,ab='b')

        # We need SANL and SFCANL if we don't use RTOFS initialization:
        yield dict(dataset=self._sfc_dataset,item=self._sanl_item,
                   atime=self._atime)
        yield dict(dataset=self._sfc_dataset,item=self._sfcanl_item,
                   atime=self._atime)
        
        # If we're in the atlantic, using GDEM climatology, we
        # also need the loop current:
        if BASIN=='L':
            yield dict(dataset=self._loop_dataset,item=self._loop_item,
                       atime=self._atime)
            yield dict(dataset=self._loop_dataset,item=self._wc_ring_item,
                       atime=self._atime)

    def get_inputs(self):
        """!Obtains input data using get_rtofs_inputs and/or
        get_gdem_inputs as appropriate, and pre-processes tcvitals"""
        logger=self.log()
        STORMID=self.storminfo.stormid3.upper()
        BASIN=STORMID[2].upper()
        self.get_gdem_inputs()
        #if BASIN in self._rtofs_init_basins:
        init_data = self.confstr('ini_data','gdem').upper()
        if init_data == 'RTOF':
            self.get_rtofs_inputs()

        # Create tcvitals file, excluding INVEST lines
        vitdest='syndat_tcvitals.%04d'%(self.storminfo.when.year,)
        logger.info('Copy vitals %s to %s'%(self._vitfile,vitdest))
        with open(vitdest,'wt') as outf:
            with open(self._vitfile,'rt') as inf:
                for line in inf:
                    if line.find('INVEST')>=0:
                        continue
                    outf.write(line)

    def get_rtofs_inputs(self):
        """!Obtains input data, links or copies to places expected by
        POM when using RTOFS initialization.

        Copies all inputs to locations expected by the pom package
        when it uses RTOFS initialization.  Copies the RTOFS a and b
        archv files for the HWRF analysis time from either the current
        or previous RTOFS cycle."""
        logger=self.log()
        hwrfatime=self.conf.cycle
        fhr=hwrfatime.hour
        rt00zonly = self.confint('rt00zonly',0)
        for itry in xrange(3):
            ( oceanatime, oceands, ocean_item ) = self.parent_ocean_adi(itry)
            if rt00zonly>0:
                afile=self._catalog.locate(oceands,ocean_item,atime=oceanatime,
                                           logger=logger,ab='a')
                bfile=self._catalog.locate(oceands,ocean_item,atime=oceanatime,
                                           logger=logger,ab='b')
            else:
                afile=self._catalog.locate(oceands,ocean_item,atime=oceanatime,
                                           ftime=hwrfatime,logger=logger,ab='a')
                bfile=self._catalog.locate(oceands,ocean_item,atime=oceanatime,
                                           ftime=hwrfatime,logger=logger,ab='b')
            if not afile or not bfile:
                logger.info('%s: cannot decide data location for this time.'%(
                        oceanatime.strftime('%Y%m%d%H'),))
            ok=True
            for (xfile,xsize) in [ [afile,10000],[bfile,1000] ]:
                (L,S) = produtil.fileop.lstat_stat(xfile)
                if S is None:
                    logger.info('%s: does not exist'%(xfile,))
                    ok=False
                    break
                if S.st_size<xsize:
                    logger.info('%s: too small (should be >=%d bytes)'%(
                            xfile,xsize))
                    ok=False
                    break
            if not ok: continue
            # We get here if the a and b files are big enough.
            if fhr>0:
                prefix='rtofs_glo.t00z.f%02d.archv.%c'
            else:
                prefix='rtofs_glo.t00z.n%02d.archv.%c'
            make_symlink(afile,prefix%(fhr,'a'),force=True,logger=logger)
            make_symlink(bfile,prefix%(fhr,'b'),force=True,logger=logger)
            return

    def get_gdem_inputs(self):
        """!Obtains input data, links or copies to places expected by
        POM when using GDEM climatology initialization.

        Copies all inputs to locations expected by the pom package
        when it uses GDEM climatology initialization.  Copies the GFS
        sanl and sfcanl, waiting for them if needed.  Makes a new
        tcvitals file by parsing the old one, and generating a new
        one, discarding lines containing "INVEST"."""
        logger=self.log()
        atime=self._atime

        # Copy GFS sanl and sfcanl files (required)
        with self.dstore.transaction() as t:
            sanlx=self._catalog.locate(self._sfc_dataset,self._sanl_item,
                                       atime=atime,logger=logger)
            sfcanlx=self._catalog.locate(self._sfc_dataset,self._sfcanl_item,
                                         atime=atime,logger=logger)
           
            sanl=UpstreamFile(self.dstore,'input_sanl',self.taskname,
                              minsize=30000)
            sanl.location=sanlx
            sanl.available=False
            sfcanl=UpstreamFile(self.dstore,'input_sfcanl',self.taskname,
                                minsize=30000)
            sfcanl.location=sfcanlx
            sfcanl.available=False

        names={ sanl:self.timestr('gfs.t{aHH}z.sanl',0,atime=self._atime),
                sfcanl:self.timestr('gfs.t{aHH}z.sfcanl',0,
                                    atime=self._atime) }
        def namer(p,logger,*a): return names[p]
        def actor(p,name,logger,*a): make_symlink(p.location,name,
                                                  force=True,logger=logger)
        wait_for_products([sanl,sfcanl],logger,namer,actor)

        # Copy loop current (optional)
        maxback=max(1,self.confint('loop_current_max_days_back',30))
        bad=True
        for idelta in xrange(maxback):
            hdelta=idelta*24.0
            looptime=to_datetime_rel(hdelta,atime)
            stime=looptime.strftime('%Y%m%d%H')
            loop=self._catalog.locate(self._loop_dataset,self._loop_item,
                                      atime=looptime,logger=logger)
            wc_ring=self._catalog.locate(
                self._loop_dataset,self._wc_ring_item,atime=looptime,
                logger=logger)
            bad=False
            if not isnonempty(loop):
                bad=True
                logger.warning('%s (loop at time %s): is empty or '
                               'non-existant'%(str(loop),stime))
            if not isnonempty(wc_ring):
                bad=True
                logger.warning('%s (loop wc_ring at time %s): is empty or '
                               'non-existant'%(str(wc_ring),stime))
            if not bad: break
        if not bad:
            looploc=self.timestr('hwrf_gfdl_loop_current_rmy5.dat.{aYMD}',
                                 0,atime=self._atime)
            make_symlink(loop,looploc,logger=logger)
            wc_ringloc=self.timestr(
                'hwrf_gfdl_loop_current_wc_ring_rmy5.dat.{aYMD}',
                0,atime=self._atime)
            make_symlink(wc_ring,wc_ringloc,logger=logger)
        else:
            logger.critical('No loop current available.  Checked %d day(s) '
                            'for loop current for %s'
                            %(maxback,atime.strftime('%Y%m%d')))

########################################################################        
class POMIniter(hwrf.coupling.ComponentIniter):
    """!This is an internal implementation class that should never be
    used directly.  It instructs the hwrf.coupling.CoupledWRF to call
    the WRFCoupledPOM.copy_pom_inputs to check or link POM input
    data."""
    def __init__(self,wcp):
        """Creates a POMIniter that will pass control to the given
        WRFCoupledPOM object, stored as self.wcp."""
        self.wcp=wcp
    def check_coupled_inputs(self,logger):
        """Calls the WRFCoupledPOM.copy_pom_inputs with just_check=True."""
        return self.wcp.copy_pom_inputs(just_check=True)
    def link_coupled_inputs(self,just_check,logger):
        """Calls the WRFCoupledPOM.copy_pom_inputs passing just_check."""
        return self.wcp.copy_pom_inputs(bool(just_check))

########################################################################        
class WRFCoupledPOM(hwrf.coupling.CoupledWRF):
    """!Runs a WRF-POM coupled simulation.  

    Most of the work of this class is done by the superclass,
    WRFAtmos.  This class adds code to copy the inputs needed by POM
    and the coupler.  There are three critical new config section
    values:

    *    wm3c_ranks = number of coupler ranks.  Default: 1
    *    pom_ranks = number of POM ranks.  Default: 9
    *    wrf_ranks = nubmer of WRF ranks.  No default.  This one is
          mandatory."""
    def __init__(self,dstore,conf,section,wrf,keeprun=True,
                 wrfdiag_stream='auxhist1',pominit=None,**kwargs):
        """!WRFCoupledPOM constructor.
        @param dstore the produtil.datastore.Datastore to use
        @param conf the hwrf.config.HWRFConfig that provides configuration ifnormation
        @param section the config section in conf
        @param wrf the hwrf.wrf.WRFSimulation object that is being run
        @param keeprun if True, the simulation temporary files are retained
        @param wrfdiag_stream the stream that generates wrfdiag files
        @param pominit The POMInit object.
        @param kwargs passed to hwrf.fcsttask.WRFAtmos.__init__"""
        if not isinstance(pominit,POMInit):
            raise TypeError(
                'The pominit argument to WRFCoupledPOM.__init__ must be a '
                'POMInit object.  You passed a %s %s.'%
                (type(pominit).__name__,repr(pominit)))
        super(WRFCoupledPOM,self).__init__(dstore,conf,section,wrf,keeprun,
                                           wrfdiag_stream,**kwargs)
        self._pominit=pominit
        pominiter=POMIniter(self)
        self.couple('coupler','hwrf_wm3c','wm3c_ranks',1)
        self._add_wave()
        # For backward compatibility, use ocean_ranks option as default:
        ocean_ranks=self.confint('ocean_ranks',9)
        self.couple('pom','hwrf_ocean_fcst','pom_ranks',ocean_ranks,pominiter)
        self.couplewrf()
        # Ocean output daily:
        self.add_coupled_stream('ocean',[
                0,86400,172800,259200,345600,432000])

    def remove_ocean(self):
        """!Removes the ocean component from coupling.
        @post Any call to run() will not include ocean coupling."""
        self.uncouple('pom')

    @property
    def pominit(self):
        """!Returns the POMInit object for this coupled forecast."""
        return self.component('pom').initer

    def _add_wave(self):
        """!Internal function for adding wave coupling.  This must be
        implemented by a subclass.
        @protected"""
        pass

    def copy_pom_inputs(self,just_check=False):
        """!Copies or checks for the inputs required by the POM model.
        This is an internal function used by the PomIniter class.  Do
        not call directly.

        @param just_check If just_check=True, the inputs are not
        copied; instead, the routine just checks for them.  Do not use
        just_check: call check_inputs instead."""
        logger=self.log()
        logger.info('Copying POM inputs from POMInit task %s'
                    %(self._pominit.taskname,))
        n_copied=0
        for prod in self._pominit.products():
            assert(isinstance(prod,produtil.datastore.Product))
            if not prod.available: prod.check(logger=logger)
            localname=prod.meta('localpath','')
            avail=prod.available 
            loc=prod.location
            if not localname:
                msg='POM product %s (available=%s location=%s) has no '\
                    'localname.'%(prod.did,repr(avail),repr(loc))
                if just_check: 
                    logger.warning(msg)
                else:
                    logger.error(msg)
                    raise POMInputError(msg)
            if not avail:
                msg='POM product %s (available=%s location=%s localname=%s)'\
                    ' is not available.'\
                    %(prod.did,repr(avail),repr(loc),repr(localname))
                if just_check:
                    logger.warning(msg)
                    return False
                else:
                    logger.error(msg)
                    raise POMInputError(msg)
            if not loc:
                msg='POM product %s (available=%s location=%s localname=%s)'\
                    ' has no location.'\
                    %(prod.did,repr(avail),repr(loc),repr(localname))
                if just_check:
                    logger.warning(msg)
                    return False
                else:
                    logger.error(msg)
                    raise POMInputError(msg)
            if not just_check:
                deliver_file(loc,os.path.basename(localname),keep=True,
                             logger=logger)
            n_copied+=1
        if n_copied<1:
            msg='No outputs reported by POM initialization.'\
                '  Did you forget to run the ocean init?'
            if just_check:
                logger.warning(msg)
                return False
            else:
                logger.error(msg)
                raise POMInputError(msg)
        logger.info('Copied %d POM inputs.  Returning True.'%(n_copied,))
        return True
