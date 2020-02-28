"""!Combines Tasks from multiple modules into a single initialization object.

This class combines Tasks from several hwrf modules to make two
different initialization systems: the InitBeforeGSI and the HWRFInit.
Also, the FGATInit is provided, which is an array of InitBeforeGSI,
one per FGAT time."""

##@var __all__
# Symbols exported by "from hwrf.init import *"
__all__=['HWRFInit','InitBeforeGSI','FGATInit']

import os, datetime, pdb
import produtil.run, produtil.rusage
import hwrf.hwrftask, hwrf.wps, hwrf.fcsttask, hwrf.post, hwrf.regrib, \
    hwrf.gribtask, hwrf.tracker, hwrf.prep, hwrf.relocate, \
    hwrf.exceptions

from produtil.ecflow import set_ecflow_label
from produtil.run import alias, exe
from hwrf.hwrftask import HWRFTask
from hwrf.numerics import to_datetime_rel, to_timedelta, to_datetime, \
    TimeMapping, to_fraction
from hwrf.regrib import clatlon
from hwrf.exceptions import NoSuchDomain, GribberError

########################################################################
class HWRFInit(HWRFTask):
    """!Runs the deterministic model initialization.

    This class runs the WPS, real, prep_hybrid (if enabled), WRF
    wrfanl and ghost jobs, and runs the tracker to find the parent
    vortex.  Passes that into the relocation (if enabled) to produce
    the final vortex.  It encapsulates that logic within one top-level
    wrapper object, and provides a way to run several parts of the
    initialization from a single function call."""

    ##@var outdir 
    # Output directory for the initialization.  Individual components
    # will be in subdirectories.

    ##@var ibdystep
    # Timestep between boundary information for the 1 minute forecast

    ##@var ghost_domains
    # List of ghost domains as hwrf.wrf.WRFDomain objects

    ##@var relocation
    # The hwrf.relocation.Relocation group of tasks

    ##@var rstage1 
    # The hwrf.relocation.Stage1 task

    ##@var rstage2
    # The hwrf.relocation.Stage2 task

    ##@var rstage3
    # The hwrf.relocation.Stage3 task

    def _subworkdir(self,childname):
        """!Helper function for constructor workdir arguments.

        Generates a workdir location for input to an HWRFTask
        constructor by appending the child name as a new directory
        path component after self.workdir.
        @param childname the child task name"""
        dir=os.path.join(self.workdir,childname)
        return dir

    def _suboutdir(self,childname):
        """!Helper function for constructor outdir arguments.

        Generates a outdir location for input to an HWRFTask
        constructor by appending the child name as a new directory
        path component after self.outdir.
        @param childname the child task name"""
        dir=os.path.join(self.outdir,childname)
        return dir

    def __init__(self,ds,conf,section, wrf,initlen,fcstlen,outdir=None,
                 realfcst=False,wrfghost=None,prep=True,track=True,
                 in_ftime=None,in_atime=None,geogrid=None,
                 prep_one_time=None,relocate=True,modin=None,
                 ibdystep=None,fgat_times=None,prepfcst=None,**kwargs):
        """!HWRFInit constructor.

        Creates the deterministic HWRF initialization.  Which pieces
        are selected, and how they are plugged in to one another, is
        dependent on the input arguments.

        @param ds passed to Datum: the Datastore object for this Task
        @param conf the conf object for this task
        @param section the conf section for this task
        @param wrf (required) the WRFSimulation representing the full WRF
          forecast
        @param initlen (required) the length of the real_nmm run for the
          initialization 
        @param fcstlen (required) the length of the real_nmm run for the
          full forecast
        @param outdir The directory in intercom to receive output.
          Default: intercom/{taskname}.  Individual initialization
          components will be in subdirectories.
        @param realfcst Flag: do we need to run real for the full forecast
          length?
        @param wrfghost hwrf.wrf.WRFSimulation object for the wrfghost
          files.  If absent, no wrfghost-related jobs will be run
        @param prep if True, use prep_hybrid input.  This changes the way
          in which real_nmm, ungrib and metgrid are run.
        @param prepfcst If True AND prep=True, use prep_hybrid for
          boundary conditions.  This changes how realfcst, ungribfcst
          and metgridfcst are run.
        @param track Run a post, regribber and tracker on the wrfanl
          history file for domain 1, to find the location of the
          parent vortex.
        @param in_ftime parent model forecast hour to treat as "time 0."
          This is used for the FGAT to use GDAS forecasts as input
        @param in_atime parent model analysis time.  Default: start time
          of wrf
        @param geogrid Should be an hwrf.wps.Geogrid, or None.  This is
          used to avoid running expensive geogrid jobs multiple times
          for multiple FGAT hours.  (This is especially important for
          hourly FGAT.)  If None or missing, a Geogrid is created.
        @param prep_one_time Flag.  If True, pretend hour 0 is also
          hour 1, 2, 3, and so on.  This is only used by prep_hybrid,
          and is intended to prevent expensive spectral to grid
          transformations of hour 3 when only the analysis time is
          needed from WRF.
        @param relocate If True, vortex relocation is performed.
        @param modin The modin argument to hwrf.relocate task constructors.
          Default: in_dataset.upper()
        @param ibdystep Optional: the boundary input frequency for the 
          ~1 minute forecasts.  Default: bdystep.
        @param fgat_times: the times at which FGAT is run.
        @param kwargs Passed to hwrf.hwrftask.HWRFTask.__init__

        @note If "in_dataset" is in kwargs, it is the option sent to
          hwrf.input.DataCatalog.locate used for obtaining input files
          for Ungrib and PrepHybrid.

          ----------------------------------------------------------

        This constructor creates the following public member variables
        or accessors.  These are intended to be used to query various
        components of the initialization.  The scripting layer can run
        the sub-objects via their run methods, or it can simply call
        the self.run_* subroutines to run them in groups.

        Reference variables:
          initlen -- timedelta for length of initialization
          fcstlen -- timedelta for length of forecast
          wrffcst -- Task for running or monitoring the WRF forecast
          wrfdoms -- WRFDomain objects from that wrffcst object
          wrfinit -- Task for running the wrfanl run of WRF
          bdytimes -- an array of boundary times needed by the forecast run
          ibdytimes -- an array of boundary times needed by the init
            run of wrf

        Initialization-Length WRF Init:
          geogrid -- the hwrf.wps.Geogrid object for running geogrid.exe
          external_geogrid -- if a geogrid argument was sent to the
            constructor, it is stored here
          ungrib -- the hwrf.wps.Ungrib object for running ungrib
          metgrid -- the hwrf.metgrid.Metgrid object for running metgrid
          realinit -- the hwrf.fcsttask.RealNMM object for running real 
            for the WRF init jobs
          runwrfanl -- the hwrf.fcsttask.WRFAnl4Trak object used to run 
            the WRF for to produce the wrfanl files and outer domain
            time 0 wrfout file
          wrfghost -- the WRFSimulation representing the ghost if
            requested.  This is the same as the wrfghost argument to
            the constructor
          runghost -- the hwrf.fcsttask.WRFGhost to run the ghost
            if requested.

        Prep Hybrid:
          prep -- the hwrf.prep.PrepHybrid object to run prep_hybrid.
            The same prep object is used for both the init-length and
            fcst-length initialization.

        Forecast-Length WRF Init:
          ungribfcst -- the hwrf.wps.Ungrib object for running ungrib for
            the full WRF forecast job, if prep=False
          metgridfcst -- the hwrf.wps.Metgrid object for running metgrid
            for the full WRF forecast job if prep=False
          realfcst -- the hwrf.fcsttask.RealNMM object

        Parent domain vortex detection:
          post -- if track=True, this is the hwrf.post.PostOneWRF
            object used to post-process the wrfanl job's time 0 outer
            domain wrfout file
          gribber -- the hwrf.gribtask.GRIBTask object used to
            regrib the output of self.post
          tracker -- the hwrf.tracker.TrackerTask object used to 
            run the tracker on the gribber
          """
        super(HWRFInit,self).__init__(ds,conf,section,**kwargs)
        self._prepfcst=bool(prepfcst)
        if prepfcst and not prep:
            raise NotImplementedError(
                'When running prep_hybrid for boundary conditions, you '
                'must also run it for initial conditions.')
        if prep_one_time is None: prep_one_time=not prepfcst
        self._prepfcst=prepfcst
        so=self._suboutdir
        sw=self._subworkdir
        logger=self.log()
        self._fgat_times=fgat_times
        self.outdir=outdir
        if outdir is None:
            self.outdir=os.path.join(self.getdir('intercom'),self.taskname)

        ds=self.dstore
        conf=self.conf

        self.initlen=to_datetime_rel(initlen,wrf.simstart())
        self.fcstlen=to_datetime_rel(fcstlen,wrf.simstart())
        assert(ibdystep is not None)
        if ibdystep is None:
            ibdystep=self.confint('ibdystep',0)
            if ibdystep<1: ibdystep=None
        self.ibdystep=ibdystep

        self.wrffcst=wrf.copy()
        self.wrffcst.set_timing(end=self.fcstlen)
        self.wrfdoms=[ d for d in self.wrffcst ]
        
        self.wrfinit=wrf.copy()
        
        self.wrfinit.set_timing(end=self.initlen)

        fwrf=self.wrffcst
        iwrf=self.wrfinit 
        if ibdystep is not None:
            self.wrfinit.set_bdystep(ibdystep)
        moad=fwrf.get_moad()
        bdytimes=[x for x in fwrf.bdytimes()]
        self.bdytimes=[ x for x in fwrf.bdytimes() ]
        self.ibdytimes=[ x for x in iwrf.bdytimes() ]

        if fwrf.bdystep() != iwrf.bdystep() and prep and realfcst:
            raise NotImplementedError(
                'You have asked me to run prep_hybrid to produce full '
                'simulation length inputs AND you specified different '
                'boundary input timesteps for the initialization-length '
                'and forecast-length jobs.  I cannot do that.  You need '
                'to either disable realfcst or disable prep_hybrid, or '
                'use the same boundary input timesteps for forecast-length'
                ' and initialization-length jobs.')
        
        starttime=iwrf.simstart()

        wpsloc=sw('wps')
        assert(wpsloc.find(self.taskname+'/'+self.taskname)<0)

        # Local utility functions for generating conf section and
        # subtask name
        def c(s): return self.confstr(s,s)
        def t(s): return '%s/%s'%(self.taskname,s)

        # Get additional options for datasets:
        ungrib_opts=dict()
        prep_opts=dict()
        if 'in_dataset' in kwargs:
            dst=kwargs['in_dataset']
            ungrib_opts['in_dataset']=dst
            prep_opts['in_dataset']=dst
            if modin is None: modin=dst.upper()
        if 'ungrib_one_time' in kwargs:
            ungrib_opts['one_time']=bool(kwargs['ungrib_one_time'])
        if modin is None: modin='GFS'

        if geogrid is not None:
            self.external_geogrid=geogrid
        else:
            self.geogrid = hwrf.wps.Geogrid(ds, conf, c('geogrid'), fwrf, 
                self.wrfdoms,taskname=t('geogrid'),location=wpsloc,
                workdir=wpsloc,outdir=so('wps'))
            geogrid=self.geogrid

        if prep:
            if fwrf.bdystep() != iwrf.bdystep():
                pbdytimes=self.ibdytimes
            else:
                pbdytimes=self.bdytimes
            self.prep    = hwrf.prep.PrepHybrid(ds, conf, c('prep_hybrid'), fwrf, 
                geogrid.geodat(moad), times=pbdytimes, taskname=t('prep_hybrid'),
                ftime=in_ftime,atime=in_atime,one_time=prep_one_time,
                workdir=sw('prep_hybrid'),outdir=so('prep_hybrid'),**prep_opts)
            # Tell ungrib and metgrid to run only the first hour:
            endtime=starttime
        else:
            if section == 'gfsinit':
                endtime=self.initlen
            else:
                endtime=starttime

        self._make_realinit(prep,ds,conf,fwrf,iwrf,endtime,wpsloc,in_atime,
                            in_ftime,ungrib_opts,geogrid,c,t,so,sw)

        if realfcst:
            self._make_realfcst(prep,prepfcst,ds,conf,fwrf,fcstlen,wpsloc,
                                in_atime,in_ftime,ungrib_opts,geogrid,
                                c,t,so,sw)

        self.runwrfanl = hwrf.fcsttask.WRFAnl4Trak(ds, conf, c('wrfanl'), iwrf, 
            taskname=t('wrfanl'), workdir=sw('wrfanl'), outdir=so('wrfanl')) \
            .add_metgrid(self.metgrid).add_geogrid(geogrid).add_real(self.realinit)
        if track:
            self._make_track(ds,conf,moad,starttime,c,t,so,sw)
        if wrfghost is not None:
            self._make_ghost(ds,conf,wrfghost,geogrid,c,t,so,sw)
        if relocate:
            self._make_relocate(ds,conf,wrfghost,track,modin,c,t,so,sw)

    def _make_realinit(self,prep,ds,conf,fwrf,iwrf,endtime,wpsloc,in_atime,
                       in_ftime,ungrib_opts,geogrid,c,t,so,sw):
        """!Makes the ungrib, metgrid and realinit member variables.
          @param prep True if prep_hybrid is in use
          @param ds the produtil.datastore.Datastore object
          @param conf the hwrf.config.HWRFConfig
          @param fwrf the hwrf.wrf.WRFSimulation for the forecast-length run
          @param iwrf the hwrf.wrf.WRFSimulation for the init-length run
          @param endtime the end time of the iwrf
          @param wpsloc directory in which to run WPS
          @param in_atime the analysis time for the parent model
          @param in_ftime the forecast time for the parent model that 
            corresponds to the analysis time of this HWRFInit
          @param ungrib_opts - dict of keyword arguments to send to the hwrf.wps.Ungrib.__init__
          @param geogrid the hwrf.wps.Geogrid
          @param c - function to generate config section names for subtasks
          @param t - function to generate task names for subtasks
          @param so - function to generate outdirs for subtasks
          @param sw - function to generate workdirs for subtasks"""
        self.ungrib  = hwrf.wps.Ungrib(ds, conf, c('ungrib'), fwrf, 
            self.wrfdoms,endtime=endtime, taskname=t('ungrib'),
            location=wpsloc,in_atime=in_atime,in_ftime=in_ftime,
            workdir=wpsloc,outdir=so('wps'),**ungrib_opts)
        self.metgrid = hwrf.wps.Metgrid(ds, conf, c('metgrid'), fwrf, 
            self.wrfdoms,increment=self.ibdystep,endtime=endtime,taskname=t('metgrid'), 
            workdir=wpsloc,outdir=so('wps'),location=wpsloc)
        self.realinit  = hwrf.fcsttask.RealNMM(ds, conf, c('realinit'), 
            iwrf, taskname=t('realinit'),workdir=sw('realinit'),
            outdir=so('realinit')) \
            .add_metgrid(self.metgrid).add_geogrid(geogrid)
        if prep: 
            self.realinit.add_prep_hybrid(self.prep)

    def _make_realfcst(self,prep,prepfcst,ds,conf,fwrf,fcstlen,wpsloc,
                       in_atime,in_ftime,ungrib_opts,geogrid,c,t,so,sw):
        """!Makes the ungribfcst, metgridfcst and realfcst member variables:
        @param prep True if prep_hybrid is in use
        @param prepfcst True if prep_hybrid is in use for boundary conditions
        @param ds the produtil.datastore.Datastore object
        @param conf the hwrf.config.HWRFConfig
        @param fwrf the WRFSimulation for the forecast-length run
        @param fcstlen the length of the forecast as a datetime.time_delta
        @param wpsloc directory in which to run WPS
        @param in_atime the analysis time for the parent model
        @param in_ftime the forecast time for the parent model that 
          corresponds to the analysis time of this HWRFInit
        @param ungrib_opts dict of keyword arguments to send to 
          hwrf.wps.Ungrib.__init__
        @param geogrid the hwrf.wps.Geogrid
        @param c function to generate config section names for subtasks
        @param t function to generate task names for subtasks
        @param so function to generate outdirs for subtasks
        @param sw function to generate workdirs for subtasks"""
        if not prepfcst:
            self.ungribfcst  = hwrf.wps.SteppingUngrib(ds, conf, c('ungrib'), fwrf, 
                    self.wrfdoms,endtime=fcstlen, taskname=t('ungribfcst'),
                    location=wpsloc,in_atime=in_atime,in_ftime=in_ftime,
                    outdir=so('wpsfcst'),workdir=wpsloc,**ungrib_opts)
            self.metgridfcst = hwrf.wps.Metgrid(ds, conf, c('metgrid'), fwrf, 
                    self.wrfdoms,endtime=fcstlen, taskname=t('metgridfcst'),
                    outdir=so('wpsfcst'),workdir=wpsloc,location=wpsloc,
                    geogrid_from=self.geogrid)
        self.realfcst = hwrf.fcsttask.RealNMM(ds, conf, c('realfcst'), fwrf, 
            outdir=so('realfcst'),workdir=sw('realfcst'),
            taskname=t('realfcst')).add_geogrid(geogrid)
        if prepfcst: 
            # Need to pull prep_hybrid data, and initial time metgrid:
            self.realfcst.add_prep_hybrid(self.prep) \
                .add_metgrid(self.metgrid)
        else:
            # Need to pull full forecast metgrid:
            self.realfcst.add_metgrid(self.metgridfcst)

    def _make_track(self,ds,conf,moad,starttime,c,t,so,sw):
        """!Makes the gribber and tracker member variables.
        @param ds the produtil.datastore.Datastore object
        @param conf the hwrf.config.HWRFConfig
        @param moad the hwrf.wrf.WRFDomain for the outermost domain
        @param starttime the start time of this HWRFInit's WRF simulations
        @param c function to generate config section names for subtasks
        @param t function to generate task names for subtasks
        @param so function to generate outdirs for subtasks
        @param sw function to generate workdirs for subtasks"""

        self.post = hwrf.post.PostOneWRF(
            self.runwrfanl,[moad],conf,c('post'),time=starttime,
            needcrtm=False,taskname=t('post'),outdir=so('post'),
            workdir=sw('post'))

        # Regridding stuff:
        grid1=hwrf.regrib.igrb1(self.post,domain=moad)
        trksub=hwrf.regrib.GRIBSubsetter(
            'part',hwrf.tracker.tracker_subset,None) 
        domloc=hwrf.regrib.FixedLocation(lat=conf['config','domlat'],
                                         lon=conf['config','domlon'])
        stormloc=hwrf.regrib.FixedLocation(lat=self.storminfo.lat,
                                           lon=self.storminfo.lon)
        basin=self.storminfo.pubbasin2
        if ((basin.upper()=='AL' or basin.upper()=='EP') \
                and domloc.ewcenter<360.0):
            domloc.ewcenter+=360.0
        r=hwrf.regrib.RegribMany(copygb=alias(exe(conf.getexe('copygb'))),
                                 wgrib=alias(exe(conf.getexe('wgrib'))))
        r.add('p25grid',clatlon(domloc,res=[0.25,0.25],size=[90.,110.],
                                scan=136,n=[441,361]))
        r.add('trkgrid',clatlon(stormloc,res=[0.25,0.25],size=[20.,20.],
                                scan=128,n=[81,81]))
        r.add('quarter_degree',grid1*r.grid('p25grid'))
        r.add('subset',r.GRIB('quarter_degree')/trksub)
        r.add('hwrftrk',hwrf.tracker.vinttave(
                r.GRIB('subset')*r.grid('trkgrid')))
        basedir=os.path.join(self.getdir('intercom'),t('regribber'))
        r.to_intercom(basedir+'/quarter_degree.grb','quarter_degree')
        r.to_intercom(basedir+'/subset.grb','subset')
        r.to_intercom(basedir+'/{out_prefix}.hwrftrk.grbf{fahr:02d}','hwrftrk')
        self.gribber=hwrf.gribtask.GRIBTask(
            ds,conf,c('regribber'),r,start=starttime,step=3600,end=starttime,
            taskname=t('regribber'),atime=starttime,workdir=sw('regribber'))
        self.tracker=hwrf.tracker.TrackerTask(
            ds,conf,c('tracker'),taskname=t('tracker'),start=starttime,
            step=3600, end=starttime,outdir=so('tracker'),
            workdir=sw('tracker'))
        self.tracker.add_moving_grid(self.storminfo,self.gribber,'hwrftrk')
        self.tracker.send_atcfunix(
            'track0',self.outdir+'/gfs.track0.atcfunix')
    def _make_ghost(self,ds,conf,wrfghost,geogrid,c,t,so,sw):
        """!Makes the wrfghost, runghost and ghost_domains member variables.
        @param ds the produtil.datastore.Datastore object
        @param conf the hwrf.config.HWRFConfig
        @param wrfghost the WRFSimulation for the ghost run of wrf
        @param geogrid the hwrf.wps.Geogrid
        @param c function to generate config section names for subtasks
        @param t function to generate task names for subtasks
        @param so function to generate outdirs for subtasks
        @param sw function to generate workdirs for subtasks"""
        self.wrfghost=wrfghost
        self.runghost = hwrf.fcsttask.WRFGhost(
            ds, conf, c('wrfghost'), self.wrfghost, taskname=t('wrfghost'),
            workdir=sw('ghost'),outdir=so('ghost'))\
            .add_metgrid(self.metgrid).add_geogrid(geogrid)\
            .add_real(self.realinit)
        self.ghost_domains=[ d for d in self.wrfghost ]

    def _make_relocate_kwargs(self,ds,conf,wrfghost,track,modin,dest_dir,so,sw):
        """!Makes a dict containing the keyword arguments to send in to
        the constructor for the hwrf.relocate task(s).
        @param ds the produtil.datastore.Datastore object
        @param conf the hwrf.config.HWRFConfig
        @param wrfghost the wrfghost argument to the constructor
        @param track the track argument to the constructor
        @param modin the modin argument to the constructor
        @param dest_dir the directory in which to run the relocate
        @param so function to generate outdirs for subtasks
        @param sw function to generate workdirs for subtasks"""
        kwargs=dict(
            wrfanl=self.runwrfanl,wrfinput=self.realinit,sim=self.wrfinit,
            domains=self.wrfdoms,ghost_domains=self.ghost_domains,
            modin=modin,dest_dir=dest_dir,workdir=sw('relocate'),
            outdir=so('relocate'))

        if wrfghost is not None: kwargs.update(wrfghost=self.runghost)
        if track: kwargs.update(parentTrack=self.tracker,trackName='track0')
        return kwargs

    def _make_relocate(self,ds,conf,wrfghost,track,modin,c,t,so,sw):
        """!Makes the relocation, rstage1, rstage2 and rstage3 member
        variables.
        @param ds the produtil.datastore.Datastore object
        @param conf the hwrf.config.HWRFConfig
        @param wrfghost the wrfghost argument to the constructor
        @param track the track argument to the constructor
        @param modin the modin argument to the constructor
        @param c function to generate config section names for subtasks
        @param t function to generate task names for subtasks
        @param so function to generate outdirs for subtasks
        @param sw function to generate workdirs for subtasks"""
        if wrfghost is None:
            raise ArgumentError('When in HWRFInit.__init__, when '
                                'relocate=True, wrfghost must not be None')
        dest_dir=os.path.join(self.workdir,'relocate')
        kwargs=self._make_relocate_kwargs(ds,conf,wrfghost,track,modin,
                                          dest_dir,so,sw)

        self.relocation=hwrf.relocate.Relocation(ds,conf,c('relocate'),
               taskname_pattern=t('relocate.stage')+'%d',**kwargs)

        self.rstage1=self.relocation.rstage1
        self.rstage2=self.relocation.rstage2
        self.rstage3=self.relocation.rstage3

    def fgat_times(self):
        """!Iterates over all fgat times, if known."""
        if self.fgat_times is not None:
            for t in self.fgat_times:
                yield t

    def run_relocate(self):
        """!Runs the relocate jobs, if present."""
        if 'rstage1' in self.__dict__: 
            self.rstage1.delete_temp()
            self.rstage1.run()
            if 'rstage2' in self.__dict__: 
                self.rstage2.run()
                if 'rstage3' in self.__dict__: 
                    self.rstage3.run()
            if self.rstage1.scrub and self.rstage2.scrub and \
                    self.rstage3.scrub:
                self.rstage3.delete_temp()
    def get_ftime(self): 
        """!Returns the parent model forecast hour treated as this
        HWRFInit object's analysis time."""
        return self._in_ftime
    def get_atime(self): 
        """!Returns the parent model analysis time.  The value of that
        may differ from this HWRFInit object's analysis time."""
        return self._in_atime

    ## Read-only property: forecast time of parent model used for
    # initializing HWRF.
    ftime=property(get_ftime,None,None,
        """!Forecast time of parent model used for initializing HWRF.""")

    ## Read-only property: analysis time of parent model.
    atime=property(get_atime,None,None,"""Analysis time of parent model.""")

    def run(self):
        """!Runs all steps of the initialization in sequence."""
        self.run_through_anl()
        self.run_init_after_anl()
        self.run_relocate()
        self.run_real_bdy()

    def run_through_anl(self):
        """!Runs the following jobs, if they are enabled: geogrid,
        ungrib, metgrid, init-length prep_hybrid, init-length real_nmm
        and wrfanl."""
        self.log().warning('run_through_anl')
        if 'geogrid' in self.__dict__:
            set_ecflow_label('status','[geogrid] process terrain')
            self.geogrid.run()
        set_ecflow_label('status','[ungrib] process parent GRIB')
        self.ungrib.run()
        set_ecflow_label('status','[metgrid] interpolate horizontally')
        self.metgrid.run()
        if 'prep' in self.__dict__:
            set_ecflow_label('status','[prep] process parent spectral')
            for i in xrange(len(self.ibdytimes)):
                t=self.ibdytimes[i]
                if t>self.initlen: break
                self.prep.run_ipiece(i)
        set_ecflow_label('status','[realinit] make wrfinput file')
        self.realinit.run()
        set_ecflow_label('status','[runwrfanl] make wrfanl files')
        self.runwrfanl.run()
        set_ecflow_label('status','Passed through anl.')
    def run_init_after_anl(self):
        """!Runs the following jobs if they are enabled: ghost, post,
        gribber, tracker."""
        logger=self.log()
        logger.warning('run_init_after_anl')
        if 'runghost' in self.__dict__: 
            produtil.rusage.getrlimit(logger=logger)
            with produtil.rusage.rusage(logger=logger):
                set_ecflow_label('status','[runghost] make wrfghost files')
                self.runghost.run()
        if 'post' in self.__dict__:
            with self.dstore.transaction() as t:
                self.post.unrun()
                self.gribber.unrun()
            set_ecflow_label('status','[post] post wrfghost for tracker')
            self.post.run(raiseall=True)
            set_ecflow_label('status','[gribber] regrid wrfghost for tracker')
            self.gribber.run(raiseall=True)
            if self.gribber.is_completed(): 
                set_ecflow_label('status','[tracker] find parent vortex')
                self.tracker.run()
            else:
                msg='Error regridding inputs to tracker.  See earlier log messages for details.'
                logger.error(msg)
                raise GribberError(msg)
        set_ecflow_label('status','Passed full init except bdy.')
    def run_real_bdy(self):
        """!Runs boundary processing jobs.

        Runs the forecast-length versions of the following jobs if
        they are enabled: ungribfcst, metgridfcst, prep_hybrid and
        real.  Note that ungribfcst and metgridfcst should be disabled
        if prep_hybrid is enabled.  That is done in the constructor
        though; the run_real_bdy will happily run all four jobs if
        requested."""
        self.log().warning('run_real_bdy')
        if 'ungribfcst' in self.__dict__:
            set_ecflow_label('status','[ungribfcst] process parent GRIB (all)')
            self.ungribfcst.run()
        if 'metgridfcst' in self.__dict__:
            set_ecflow_label('status','[metgridfcst] interpolate parent GRIB (all)')
            self.metgridfcst.run()
        if 'realfcst' in self.__dict__: 
            if 'prep' in self.__dict__ and self._prepfcst:
                for i in xrange(len(self.bdytimes)):
                    t=self.bdytimes[i]
                    if t in self.ibdytimes:
                        self.log().info(
                            '%s: skipping: is an initial bdy time (should '
                            'already be done)'%(t.strftime('%Y%m%d%H'),))
                        continue
                    self.prep.run_ipiece(i)
            set_ecflow_label('status','[realfcst] interpolate parent GRIB (all)')
            self.realfcst.run()
        set_ecflow_label('status','Entire init complete.')
    def inputiter(self):
        """!Iterates over all needed input data 

        Calls the inputiter for the ungrib, ungribfcst and prep
        members, iterating over all data they need to run the
        initialization."""
        if 'ungrib' in self.__dict__:
            for d in self.ungrib.inputiter(): yield d
        if 'ungribfcst' in self.__dict__:
            for d in self.ungribfcst.inputiter(): yield d
        if 'prep' in self.__dict__:
            for d in self.prep.inputiter(): yield d

    ##@var initlen
    # timedelta for length of initialization

    ##@var fcstlen
    # timedelta for length of forecast

    ##@var wrffcst
    # Task for running or monitoring the WRF forecast

    ##@var wrfdoms
    # WRFDomain objects from that wrffcst object

    ##@var wrfinit
    # Task for running the ~1 minute forecast to generate wrfanl files.

    ##@var bdytimes
    # An array of boundary times needed by the forecast run of WRF.

    ##@var ibdytimes
    # An array of boundary times needed by the ~1 minute wrf forecast
    # used to create wrfanl and ghost files.

    ##@var geogrid
    # the hwrf.wps.Geogrid object for running geogrid.exe

    ##@var external_geogrid
    # if a geogrid argument was sent to the constructor, it is stored here

    ##@var ungrib
    # the hwrf.wps.Ungrib object for running ungrib

    ##@var metgrid
    # the hwrf.metgrid.Metgrid object for running metgrid

    ##@var realinit
    # the hwrf.fcsttask.RealNMM object for running real for the WRF init jobs

    ##@var runwrfanl
    # the hwrf.fcsttask.WRFAnl4Trak object used to run the WRF for to
    # produce the wrfanl files and outer domain time 0 wrfout file

    ##@var wrfghost
    # the WRFSimulation representing the ghost if requested.  This is
    # the same as the wrfghost argument to the constructor

    ##@var runghost
    # the hwrf.fcsttask.WRFGhost to run the ghost if requested.

    ##@var prep
    # the hwrf.prep.PrepHybrid object to run prep_hybrid.  The same
    # prep object is used for both the init-length and fcst-length
    # initialization.

    ##@var ungribfcst
    # the hwrf.wps.Ungrib object for running ungrib for the full WRF
    # forecast job, if prep=False

    ##@var metgridfcst
    # the hwrf.wps.Metgrid object for running metgrid for the full WRF
    # forecast job if prep=False

    ##@var realfcst
    # the hwrf.fcsttask.RealNMM object

    ##@var post
    # if track=True, this is the hwrf.post.PostOneWRF object used to
    # post-process the wrfanl job's time 0 outer domain wrfout file

    ##@var gribber
    # the hwrf.gribtask.GRIBTask object used to regrib the output of
    # self.post

    ##@var tracker
    # the hwrf.tracker.TrackerTask object used to run the tracker on
    # the gribber

########################################################################
class InitBeforeGSI(HWRFInit):
    """!Runs the FGAT initialization for one FGAT member

    This class serves a similar purpose to its superclass, HWRFInit,
    but is intended for initialization that runs before the GSI.  It
    has a modified list of arguments to the hwrf.relocate classes."""
    def __init__(self,ds,conf,section,gsi_d01=None,gsi_d02=None,gsi_d03=None,
                 fgat_times=None, **kwargs):
        """!Creates a new InitBeforeGSI object.  See the
        HWRFInit.__init__ for details.  
        @param ds passed to Datum: the Datastore object for this Task
        @param conf the conf object for this task
        @param section the conf section for this task
        @param gsi_d01 passed to the hwrf.relocate constructors' 
                gsi_d01 argument
        @param gsi_d02 passed to the hwrf.relocate constructors' 
                gsi_d02 argument
        @param gsi_d03 passed to the hwrf.relocate constructors' 
                gsi_d03 argument
        @param fgat_times array of FGAT times
        @param kwargs All other keyword arguments are passed to
          HWRFInit.__init__
            """
        assert(fgat_times is not None)
        if not isinstance(fgat_times,list):
            raise TypeError('In InitBeforeGSI.__init__, fgat_times must be a list or tuple, not a %s %s.'
                            %(type(fgat_times).__name__,repr(fgat_times)))
        self._gsi_d01=gsi_d01
        self._gsi_d02=gsi_d02
        self._gsi_d03=gsi_d03
        super(InitBeforeGSI,self).__init__(
            ds,conf,section,fgat_times=fgat_times,**kwargs)
        assert(self._fgat_times is not None)

    ##@var wrfghost
    # The hwrf.wrf.WRFSimulation for the wrf ghost runs
    
    ##@var runghost
    # The hwrf.fcsttask.WRFGhost for running the WRF for the ghost case

    def _make_relocate_kwargs(self,ds,conf,wrfghost,track,modin,dest_dir,
                              so,sw):
        """!Makes the FGAT relocation.

        This serves the same purpose as the
        HWRFInit._make_relocate_kwargs(), but it adds to those
        arguments the gsi_d01, d02 and d03 sent to the constructor,
        plus the fgat_times."""
        assert(self._fgat_times is not None)
        kwargs=super(InitBeforeGSI,self)._make_relocate_kwargs(
            ds,conf,wrfghost,track,modin,dest_dir,so,sw)
        kwargs.update(gsi_d01=self._gsi_d01, gsi_d02=self._gsi_d02, 
                      gsi_d03=self._gsi_d03, fgat_times=self._fgat_times)
        return kwargs
    def _make_ghost(self,ds,conf,wrfghost,geogrid,c,t,so,sw):
        """!Makes the wrfghost, runghost and ghost_domains member
        variables.  

        Overrides the HWRFInit._make_ghost() to use the
        WRFGhostForPost class instead of WRFGhost in order to get the
        wrfout "ghout_d0X" files for each domain for post-processing.

        @param ds the produtil.datastore.Datastore object
        @param conf the hwrf.config.HWRFConfig
        @param wrfghost the WRFSimulation for the ghost run of wrf
        @param geogrid the hwrf.wps.Geogrid
        @param c function to generate config section names for subtasks
        @param t function to generate task names for subtasks
        @param so function to generate outdirs for subtasks
        @param sw function to generate workdirs for subtasks"""
        self.wrfghost=wrfghost
        self.runghost = hwrf.fcsttask.WRFGhostForPost(
            ds, conf, c('wrfghost'), self.wrfghost, taskname=t('wrfghost'),
            workdir=sw('ghost'),outdir=so('ghost'))\
            .add_metgrid(self.metgrid).add_geogrid(geogrid)\
            .add_real(self.realinit)
        self.ghost_domains=[ d for d in self.wrfghost ]
    ##@var ghost_domains
    # List of domains for the ghost run of WRF (hwrf.wrf.WRFDomains objects)

########################################################################
class FGATInit(HWRFTask):
    """!The FGATInit represents an array of InitBeforeGSI objects, each run
    for one forecast hour of some parent model (usually GDAS)."""

    ##@var outdir
    # The directory in intercom to receive output.  Individual
    # initialization components will be in subdirectories.

    ##@var inits
    # InitBeforeGSI objects for individual times.

    ##@var ftime2init
    # A mapping from forecast time to InitBeforeGSI objects.

    def __init__(self,ds,conf,section, wrf,initlen,fcstlen,outdir=None,
                 realfcst=False,wrfghost=None,prep=True,track=True,
                 in_ftimes=None,in_atime=None,cycling_interval=6*3600,
                 gsi_d01=None,gsi_d02=None,gsi_d03=None,ibdystep=None,
                 prepfcst=True,**kwargs):
        """!Creates an FGATInit, passing most arguments to the child
        InitBeforeGSI objects' constructors.  The cycling_interval is the
        time to subtract from this model's analysis time to get the
        parent model's analysis time.  The default of six hours
        (6*3600 as an int) is correct for the operational HWRF and
        GDAS.

        @param ds passed to Datum: the Datastore object for this Task
        @param conf the conf object for this task
        @param section the conf section for this task
        @param wrf (required) the WRFSimulation representing the full WRF
          forecast
        @param initlen (required) the length of the real_nmm run for the
          initialization 
        @param fcstlen (required) the length of the real_nmm run for the
          full forecast
        @param outdir The directory in intercom to receive output.
          Default: intercom/{taskname}.  Individual initialization
          components will be in subdirectories.
        @param outdir The directory in intercom to receive output.
          Default: intercom/{taskname}.  Individual initialization
          components will be in subdirectories.
        @param realfcst Flag: do we need to run real for the full forecast
          length?
        @param wrfghost hwrf.wrf.WRFSimulation object for the wrfghost
          files.  If absent, no wrfghost-related jobs will be run
        @param prep if True, use prep_hybrid input.  This changes the way
          in which real_nmm, ungrib and metgrid are run.
        @param track Run a post, regribber and tracker on the wrfanl
          history file for domain 1, to find the location of the
          parent vortex.
        @param in_ftimes array of parent model forecast hours to treat
          as "time 0" for each FGAT time. This is used for the FGAT to
          use GDAS forecasts as input
        @param in_atime parent model analysis time.  Default: start time
          of wrf
        @param cycling_interval HWRF cycling interval in seconds.
        @param gsi_d01 passed to the hwrf.relocate constructors' 
                gsi_d01 argument
        @param gsi_d02 passed to the hwrf.relocate constructors' 
                gsi_d02 argument
        @param gsi_d03 passed to the hwrf.relocate constructors' 
                gsi_d03 argument
        @param ibdystep Optional: the boundary input frequency for the 
          ~1 minute forecasts.  Default: bdystep.
        @param prepfcst If True AND prep=True, use prep_hybrid for
          boundary conditions.  This changes how realfcst, ungribfcst
          and metgridfcst are run.
        @param kwargs Other keyword arguments are passed to HWRFTask.__init__
        """
        if 'workdir' not in kwargs:
            # FGATInit is a special case: its subtasks are directly
            # under WORKhwrf.
            kwargs['workdir']=conf.getdir('WORKhwrf')
        super(FGATInit,self).__init__(ds,conf,section,**kwargs)

        log=self.log()

        self.outdir=outdir

        self._gsi_d01=gsi_d01
        self._gsi_d02=gsi_d02
        self._gsi_d03=gsi_d03

        if prepfcst and not prep:
            raise NotImplementedError(
                'When running prep_hybrid for boundary conditions, you '
                'must also run it for initial conditions.')
        self._prep=prep
        self._prepfcst=prepfcst
        self._track=track
        self._wrf=wrf
        self._wrfghost=wrfghost
        self._initlen=initlen
        self._fcstlen=fcstlen
        self._realfcst=bool(realfcst)
        if ibdystep is None:
            ibdystep=self.confint('ibdystep',0)
            if ibdystep<1: ibdystep=0
        self._ibdystep=ibdystep

        if outdir is None: self.outdir=os.path.join(self.getdir('intercom'),
                                                    self.taskname) 
        self._cycling_interval=to_timedelta(cycling_interval)
        if in_atime is None:
            in_atime=wrf.simstart() - self._cycling_interval
        self._in_atime=in_atime
        if in_ftimes is None:
            in_ftimes=[ to_datetime_rel(t,in_atime) \
                            for t in self._default_ftimes() ]
        self._in_ftimes=[ t for t in in_ftimes ]
        self.inits=list()
        self.ftime2init=TimeMapping(self._in_ftimes, lambda : 1)
        assert('ftime2init' in self.__dict__)

        # Get additional options for datasets:
        dsopt=dict()
        if 'in_dataset' in kwargs: dsopt['in_dataset']=kwargs['in_dataset']
        assert('in_dataset' in dsopt)
        assert(dsopt['in_dataset'] == 'gdas1')
        self._make_inits(**dsopt)
        assert(self.inits)

        ceninit=self.init_at_time(conf.cycle)
        centrack=ceninit.rstage3.get_track()
        log.debug('centrack %s'%(centrack))
        for fhr,init in self.fhr_and_init():
            init.rstage3.centrack=centrack
    def init_at_time(self,when):
        """!Get the subtask for one time.

        Returns the initialization InitBeforeGSI object for the
        specified time (the earliest time not before the specified
        time).  The time can be an absolute time (2015091418) or a
        forecast hour (12), as long as it is accepted by the first
        argument of to_datetime_rel.  Raises an exception if the time
        has no InitBeforeGSI objects."""
        when=to_datetime_rel(when,self._in_atime)
        return self.ftime2init[when]
    def inputiter(self):
        """!Iterates over needed input data.

        Calls all inputiter functions for all child objects, hence
        iterating over all input sources required for all FGAT hours."""
        for t,p in self.ftime2init.iteritems():
            for d in p.inputiter():
                yield d
    def get_ghosts(self,domain):
        """!Get all ghost output products.
        @param domain The domain of interest, an hwrf.wrf.WRFDomain

        Returns a TimeMapping, mapping from init time to the ghost
        product output from the runghost member of each init time."""
        ret=TimeMapping(self._in_ftimes)
        for t,p in self.ftime2init.iteritems():
            i=self.init_at_time(t)
            if 'runghost' in i.__dict__:
                rg=i.runghost
                gg=rg.get_ghost(domain)
                ret[t]=gg
        return ret
    def get_relocates(self,domain=None):
        """!Get hwrf.relocate.Stage3 objects or output products for
        each FGAT time.

        Returns a TimeMapping of objects, one per init time.  If a
        domain is specified, the objects are the wrfinput or ghost
        product output by the relocation for that domain.  If a domain
        is NOT specified, then the objects are the relocate stage 3
        tasks.
        @param domain The domain of interest."""
        ret=TimeMapping(self._in_ftimes)
        for t,p in self.ftime2init.iteritems():
            i=self.init_at_time(t)
            rr=i.rstage3
            if domain is None:
                ret[t]=rr
                continue
            assert(isinstance(rr,hwrf.relocate.Stage3))
            rdom=i.wrfghost[domain]
            if rdom.is_moad():
                rg=rr.wrfinput_at_time(t,rdom)
            else:
                rg=rr.get_ghost(rdom)
            if rg is None:
                raise NoSuchDomain(
                    'Error: %s does not exist at time %s'
                    %(str(rdom),str(t)))
            ret[t]=rg
        return ret
    def parent_fhrs(self):
        """!Iterates over all fgat times as datetime.datetime objects."""
        for ftime in self.ftime2init.iterkeys():
            assert(isinstance(ftime,datetime.datetime))
            yield ftime
    def fhr_and_init(self):
        """!Iterates over pairs of (fhr,init) where fhr is a
        fraction.Fraction FGAT hour, and init is an InitBeforeGSI
        object for that time."""
        assert(self.inits)
        for ftime,init in self.ftime2init.iteritems():
            fhr=to_fraction(ftime-self._in_atime)/3600.0
            yield fhr,init
    def _default_ftimes(self):
        """!Internal function that implements other iterators.

        This is an iterator that generates the default FGAT hours from
        the configuration file.  The iterator yields the forecast
        times for the input model as datetime.datetime objects."""
        start=self.conffloat('FGATSTR',-3.)*3600
        step=self.conffloat('FGATINV',3)*3600
        end=self.conffloat('FGATEND',3)*3600
        if step<60:
            raise ValueError('ERROR: in hwrf.init.FGATInit the FGATINV must '
                             'be at least one minute, but you provided %f '
                             'seconds'%(step,))
        now=start
        while now-30<=end: # use 30 second epsilon
            assert(self._in_atime) is not None
            assert(now is not None)
            rel=to_datetime_rel(now,self._in_atime)
            assert(rel is not None)
            yield to_datetime_rel(self._cycling_interval,rel)
            now+=step
    def _make_init(self,in_ftime,taskname,**kwargs):
        """!Internal implementation function that generates the
        InitBeforeGSI object for one FGAT time.

        @param in_ftime The forecast time of interest
        @param taskname The name of the child task
        @param kwargs Additional keyword arguments passed to
        InitBeforeGSI.__init__
        @returns the resulting InitBeforeGSI object"""
        wrf=self._wrf.copy()
        tstart=to_datetime_rel(in_ftime, self._in_atime)
        ftime=wrf.simend()-wrf.simstart()
        tend=tstart+ftime
        wrf.set_timing(start=tstart,end=tend)
        wrfghost=self._wrfghost.copy()
        wrfghost.set_timing(start=tstart,end=tend)
        in_ftime=in_ftime-self._in_atime
        fgat_times=[ t for t in self.parent_fhrs() ]
        assert(fgat_times is not None)
        child=InitBeforeGSI(ds=self.dstore,conf=self.conf,section=self.section,wrf=wrf,
                       initlen=self._initlen,fcstlen=self._fcstlen,outdir=None,
                       realfcst=self._realfcst,wrfghost=wrfghost,prep=self._prep,
                       track=self._track,in_ftime=in_ftime,in_atime=self._in_atime,
                       taskname=taskname,prep_one_time=True,ibdystep=self._ibdystep,
                       gsi_d01=self._gsi_d01, gsi_d02=self._gsi_d02, 
                       gsi_d03=self._gsi_d03,fgat_times=fgat_times,
                       prepfcst=self._prepfcst,ungrib_one_time=True,**kwargs) 
        assert(child.ungrib.one_time)
        return child
    def _make_inits(self,**kwargs):
        """!Creates all child InitBeforeGSI objects.

        Creates InitBeforeGSI objects for all input times in
        self.ftime2init and self.inits.  The self._make_init is used
        to make each individual object.  The self.inits will be a list
        of InitBeforeGSI objects, and self.ftime2init will be a
        mapping from time to InitBeforeGSI.
        @param kwargs Passed to _make_init()"""
        assert('ftime2init' in self.__dict__)
        assert(self._in_ftimes)
        for in_ftime in self._in_ftimes:
            self.log().debug('process ftime %s'%(repr(in_ftime),))
            taskname=taskname='%s.t%s'%(self.taskname,in_ftime.strftime('%Y%m%d%H%M'))
            child=self._make_init(in_ftime,taskname,**kwargs)
            self.inits.append(child)
            assert(self.inits)
            self.ftime2init[in_ftime]=child
    def run(self): 
        """!Runs all InitBeforeGSI.run() commands in sequence."""
        for init in self.inits: init.run()
    def run_through_anl(self): 
        """!Runs all InitBeforeGSI.run_through_anl() commands in sequence."""
        for init in self.inits: init.run_through_anl()
    def run_init_after_anl(self):  
        """!Runs all InitBeforeGSI.run_after_anl() commands in sequence."""
        for init in self.inits: init.run_init_after_anl()
    def run_real_bdy(self):
        """!Runs all InitBeforeGSI.run_real_bdy() commands in sequence."""
        for init in self.inits: init.run_real_bdy()
    def run_relocate(self):
        """!Runs all InitBeforeGSI.run_relocate() commands in sequence."""
        for init in self.inits: init.run_relocate()
