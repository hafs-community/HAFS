"""!Utilities for ensemble-based data assimilation.

This module contains utilities for doing various forms of
ensemble-based data assimilation.  It manages a two-dimension
ensemble-time array of Task objects that know how to run each analysis
cycle for each member.  This module also provides a way of presenting
the previous forecast cycle's ensda objects such that this cycle can
use them.  This is all built on top of the classes in the
hwrf.fcsttask module."""

##@var __all__
# The symbols exported by "from hwrf.ensda import *"
__all__=['DAEnsemble','FromPriorCycle','FromGFSENKF',
         'write_ensda_flag_file','read_ensda_flag_file',
         'CycleTDRCheck','AlwaysRunENSDA','enada_pre_object_for']

import os
import produtil.datastore, produtil.cd, produtil.fileop, produtil.log

import hwrf.hwrftask, hwrf.numerics, hwrf.prep, hwrf.fcsttask
import hwrf.input, hwrf.regrib, hwrf.tracker, hwrf.exceptions

from produtil.log import jlogger
from produtil.datastore import COMPLETED,UpstreamFile
from produtil.fileop import isnonempty, wait_for_files, make_symlink
from hwrf.numerics import to_datetime_rel, to_timedelta, TimeArray, to_datetime
from hwrf.post import PostManyWRF
from produtil.cd import NamedDir
from hwrf.wrf import WRFDomain
from produtil.run import alias, exe
from hwrf.regrib import clatlon
from produtil.ecflow import set_ecflow_event

class DAEnsemble(hwrf.hwrftask.HWRFTask):
    """!Represents a two-dimensional ensemble-vs-time array of
    hwrf.hwrftask.HWRFTask objects."""
    def __init__(self,dstore,conf,section,anlintime=None,
                 taskname=None,**kwargs):
        """!Constructor for DAEnsemble
        @param dstore     the produtil.datastore.Datastore database to use
        @param conf       the hwrf.config.HWRFConfig for configuration info
        @param section    the section to use in conf
        @param anlintime  the analysis input time
        @param taskname   the name of this task in the database
        @param kwargs     passed to the superclass constructor"""
        super(DAEnsemble,self).__init__(dstore,conf,section,taskname,
                                            **kwargs)

        if anlintime is None:
            anlintime=conf.cycle
        anlintime=to_datetime_rel(anlintime,conf.cycle)
        self.__anlintime=anlintime
        self.__memberids=set()

        cycling_interval=conf.getfloat('config','cycling_interval')*3600.0

        self.__tstep=to_timedelta(self.confstr('da_cycle',cycling_interval))
        assert(self.__tstep>to_timedelta(1800))
        endtime=to_datetime_rel(cycling_interval,conf.cycle)

        self.__members=TimeArray(conf.cycle,endtime-self.__tstep,
                                 self.__tstep,dict)
        self.__anlouttime=self.__members.lasttime

    @property
    def anlintime(self):
        """!The time at the beginning of the first ensemble step."""
        return self.__anlintime

    @property
    def anlouttime(self):
        """!The time at the end of the last ensemble step."""
        return self.__anlouttime

    @property
    def nmembers(self):
        """!The number of members of the ensemble."""
        return len(self.__memberids)

    @property
    def nsteps(self):
        """!The number of ensemble DA time steps."""
        return len(self.__members)

    @property
    def anlintimes(self):
        """!Iterates over all ensemble analysis input times."""
        for t in self.__members.times():
            yield t

    @property
    def anlouttimes(self):
        """!Iterates over all ensemble analysis output times."""
        first=True
        for t in self.__members.times():
            if first:
                first=False
            else:
                yield t
        yield self.__anlouttime

    def member_ids(self):
        """!Iterates over all member ids."""
        for memberid in self.__memberids:
            yield memberid

    def set_member(self,atime,enkfmem,task):
        """!sets the HWRFTask to use to use for one cycle of one member

        Tells member enkfmem to use the specified task to produce
        output whose input analysis time is atime.
        @param atime    the analysis time, a datetime.datetime
        @param enkfmem  the enkf member id
        @param task     the HWRFTask to use"""
        self.__memberids.add(enkfmem)
        self.__members[atime][enkfmem]=task

    def members_at_time(self,atime):
        """!iterate over members for a specified analysis input time

        Iterates over all members at the specified anlintime,
        yielding (id,member) tuples.
        @param atime the analysis time, a datetime.datetime"""
        time=to_datetime_rel(atime,self.__anlintime)
        for (enkfmem,memstep) in self.__members[time].iteritems():
            yield (enkfmem,memstep)

    def members_at_anlouttime(self):
        """!iterate over members at the final analysis output time.

        Iterates over all members at the final analysis output time,
        yielding (id,member) tuples."""
        for (e,m) in self.members_at_time(self.__members.lasttime):
            yield e,m

    def steps_for_member(self,enkfmem):
        """!iterate over analysis cycles for a specific member

        Iterates over (time,EnsembleDAMemberStep) pairs for the
        specified member."""
        for (t,a) in self.__members.iteritems():
            yield t,a[enkfmem]

    def member(self,atime,enkfmem):
        """!get the analysis cycle for the specified member and time

        Returns the da cycle for member enkfmem at analysis input
        time atime.
        @param atime    the analysis time, a datetime.datetime
        @param enkfmem  the ensemble id"""
        return self.__members[atime][enkfmem]

    def inputiter(self):
        """!iterate over all needed input data, for hwrf.input

        Calls inputiter for all steps of all ENKF members.  This is
        for use by the hwrf.input to figure out what input data is
        required for the DA ensemble."""
        for t,e in self.__members.iteritems():
            for m in e.itervalues():
                for d in m.inputiter():
                    yield d

    def dump(self):
        """!print detailed diagnostics

        Sends detailed diagnostics about all members to the print()
        statement.  This is intended for debugging only."""
        for (time,stuff) in self.__members.iteritems():
            for (enkfid,memberstep) in stuff.iteritems():
                print "self.__members[%s][%s]=%s"%(
                    repr(time),repr(enkfid),repr(memberstep))
        t=self.__members.lasttime
        print 'last time t is %s'%(repr(t),)
        for (enkfid,memberstep) in self.members_at_time(t):
            print 'self.__members[t][%s]=%s'%(
                repr(enkfid),repr(memberstep))

########################################################################

class FromPriorCycle(hwrf.hwrftask.HWRFTask):
    """!Represents an ensemble member from the previous forecast cycle.

    This is used to generate UpstreamFile objects for the previous
    cycle's ensemble of FromGFSENKF simulations."""
    def __init__(self,dstore,conf,section,domains,enkfmem,anlouttime,
                 **kwargs):
        """!FromPriorCycle constructor

        Makes a new FromPriorCycle object.
        @param dstore the produtil.datastore.Datastore database object
        @param conf   the hwrf.config.HWRFConfig with configuration info
        @param section  the section to use within conf
        @param domains  the list or tuple of hwrf.wrf.WRFDomain objects
                        to obtain from the prior cycle, in order of
                        grid ID.
        @param enkfmem  the enkf member ID
        @param anlouttime  the output time at the end of the analysis
        @param kwargs   passed to the superclass constructor"""
        super(FromPriorCycle,self).__init__(dstore,conf,section,**kwargs)
        self.__domains=[ d for d in domains ]
        self.__enkfmem=int(enkfmem)
        self.__anlouttime=hwrf.numerics.to_datetime(anlouttime)

    def products(self,domains=None,**kwargs):
        """!Iterates over all products

        Iterates over products produced by the prior forecast cycle's
        analysis cycle.
        @param domains if provided and non-None, only products from these
                       domains are yielded
        @param kwargs  ignored        """
        if False: yield None # ensures this is an iterator
        if domains is None: domains=self.__domains
        assert(domains)
        for domain in domains:
            assert(domain in self.__domains)
            if domain in self.__domains:
                if domain.is_moad():
                    yield self.get_wrfinput(domain,self.__anlouttime)
                else:
                    yield self.get_wrfanl(domain,self.__anlouttime)
    def get_wrfinput(self,domain,atime):
        """!return the product for the specified domain and analysis time

        Returns the product for the wrfinput_d01 file for the
        specified domain and time.  This is simply a wrapper around
        get_product(domain,atime)
        @param domain the domain of interest
        @param atime the analysis time as a datetime.datetime"""
        return self.get_product(domain,atime)
    def get_wrfanl(self,domain,atime):
        """!returns the wrfanl file's product"""
        return self.get_product(domain,atime)
    def get_product(self,domain,atime):
        """!Returns a product for the specified domain and time.

        Creates a new produtil.datastore.UpstreamFile for the wrfinput
        or wrfanl file for the given domain, at the given analysis
        time.  The analysis time must be the anlouttime.
        @return a newly created UpstreamFile for the file of interest
        @param domain the domain of interest
        @param atime the analysis time"""
        logger=self.log()
        atime=hwrf.numerics.to_datetime(atime)
        if not atime==self.__anlouttime:
            logger.info('Wrong atime: %s vs %s'
                        %(str(atime),str(self.__anlouttime)))
            return None
        if not domain in self.__domains:
            logger.info('Invalid domain: %s not in %s'%(
                    str(domain), ', '.join([str(x) for x in self.__domains])))
            return None
        loc=self.confstrinterp(
            '{oldcom}/{oldvit[stormnamelc]}{oldvit[stormid3lc]}.{oldvit[YMDH]}.'
            'ensda_{enkfid:03d}.wrfinput_d{domid:02d}',enkfid=self.__enkfmem,
            domid=int(domain.get_grid_id()))
        logger.info('Domain %s atime %s enkfmem %s loc %s'%(
                str(domain),str(atime),repr(self.__enkfmem),repr(loc)))
        uf=UpstreamFile(self.dstore,category=self.taskname,
                        prodname=os.path.basename(loc),
                        location=loc)
        uf.check()
        return uf

    def get_track(self,atime):
        logger=self.log()
        atime=hwrf.numerics.to_datetime(atime)
        if not atime==self.__anlouttime:
            logger.info('Wrong atime: %s vs %s'
                        %(str(atime),str(self.__anlouttime)))
            return None
        enkfid='%03d'%int(self.__enkfmem)
        loc=self.confstrinterp(
            '{oldcom}/{oldvit[stormnamelc]}{oldvit[stormid3lc]}.{oldvit[YMDH]}.'
            'trak.hwrf.atcfunix.mem'+enkfid)
        logger.info('atime %s enkfmem %s loc %s'%(
                str(atime),repr(self.__enkfmem),repr(loc)))
        uf=UpstreamFile(self.dstore,category=self.taskname,
                        prodname=os.path.basename(loc),
                        location=loc)
        uf.check()
        return uf

########################################################################

class FromGFSENKF(hwrf.hwrftask.HWRFTask):
    """! Forecast ensemble member based on the GFS ENKF.

    Runs one member of an ensemble DA forecast ensemble, using a
    member of the GFS ENKF ensemble as initial and boundary
    conditions.  Some data from the earlier deterministic forecast
    jobs is reused to simplify the process."""

    def __init__(self,dstore,conf,section,detinit,enkfmem,sim,
                 taskname=None,track=None,relocate=None,priorcycle=None,
                 **kwargs):

        """!Constructor for FromGFSENKF

        @param dstore the produtil.datastore.Datastore database to use
        @param conf the hwrf.config.HWRFConfig that provides
           configuration data
        @param section the section in conf to use
        @param detinit the deterministic initialization, an
           hwrf.init.HWRFInit or hwrf.init.InitBeforeGSI object.
        @param enkfmem the ensemble member id
        @param sim the hwrf.wrf.WRFSimulation object
        @param taskname the name of the task within the database
        @param kwargs passed to the parent class constructor"""
        """!Constructor for FromGFSENKF

        @param dstore the produtil.datastore.Datastore database to use
        @param conf the hwrf.config.HWRFConfig that provides
           configuration data
        @param section the section in conf to use
        @param detinit the deterministic initialization, an
           hwrf.init.HWRFInit or hwrf.init.InitBeforeGSI object.
        @param enkfmem the ensemble member id
        @param sim the hwrf.wrf.WRFSimulation object
        @param taskname the name of the task within the database
        @param kwargs passed to the parent class constructor"""
        super(FromGFSENKF,self).__init__(dstore,conf,section,taskname,
                                         **kwargs)
        assert(isinstance(sim,hwrf.wrf.WRFSimulation))
        if track is None or relocate is None:
            raise TypeError(
                'You must explicitly specify the track and relocate '
                'arguments to FromGFSENKF.__init__ and they must be boolean '
                'values, not None.')
        self.enkfmem=enkfmem
        self.track=bool(track)
        self.relocate=bool(relocate)
        self.make_wrf(detinit,sim)
        self.make_init(detinit)
        self.make_fcst(detinit)
        if self.track:
            self._make_track()
        if self.relocate:
            self._make_relocate(track,'ENKF',priorcycle)

    ##@var enkfmem
    # the enkf member id

    ##@var fcst
    # the forecast task, an hwrf.fcsttask.AnalysisCycle

    ##@var geogrid
    # the geogrid task from the deterministic initialization

    ##@var metgrid
    # the metgrid task from the deterministic initialization

    ##@var prep
    # the hwrf.prep.PrepHybrid that processes the GFS ENKF forecast
    # and analysis spectral data

    ##@var realinit
    # the hwrf.fcsttask.RealNMM that generates wrfinput_d01 and
    # wrfbdy_d01 files for input to the fcst and wrfanl

    ##@var wrfanl
    # the hwrf.fcsttask.WRFAnl that generates input wrfanl_d* files
    # for input to the fcst

    @property
    def anlintime(self):
        """!The analysis input time."""
        return self.__wrf.simstart()

    def make_wrf(self,detinit,sim):
        """!Create the wrf() and fcst

        This function, called from the constructor, creates the
        hwrf.wrf.WRFSimulation and hwrf.fcsttask.AnalysisCycle used
        for this forecast.
        @param detinit the deterministic model initialization, an
          hwrf.init.HWRFInit or hwrf.init.InitBeforeGSI.
        @param sim the hwrf.wrf.WRFSimulation passed to the constructor."""
        self.fcst=hwrf.fcsttask.AnalysisCycle(
            self.dstore,self.conf,self.confstr('fcsttask'),sim.copy(),
            taskname=self.taskname+'.fcst',outdir=self.outdir+'/fcst',
            workdir=self.workdir+'/fcst',keeprun=True)
        self.__wrf=sim

        # WCOSS workaround:
        self.fcst.sim.set_active_io_form_to(2)

    def make_fcst(self,detinit):
        """!Adds input sources to the forecast object.

        Adds metgrid, geogrid, wrfinput, wrfbdy, wrfanl, and coupler
        fort.65 input to the fcst member variable.
        @param delinit the deterministic model initialization, an
          hwrf.init.HWRFInit or hwrf.init.InitBeforeGSI."""
        """!Adds input sources to the forecast object.

        Adds metgrid, geogrid, wrfinput, wrfbdy, wrfanl, and coupler
        fort.65 input to the fcst member variable.
        @param delinit the deterministic model initialization, an
          hwrf.init.HWRFInit or hwrf.init.InitBeforeGSI."""
        self.fcst\
            .add_geogrid(self.geogrid) \
            .add_metgrid(self.metgrid) \
            .add_fort65(self.realinit) \
            .add_wrfinput(self.realinit) \
            .add_wrfbdy(self.realinit)
        for domain in self.sim:
            if domain.is_moad(): continue
            self.fcst.add_wrfanl(self.wrfanl,domain)
            self.fcst.sim.add_output('history',start=0,step=3600*6,end=6*3600)

    @property
    def sim(self):
        """!The wrf simulation made by make_wrf()"""
        return self.__wrf

    def inputiter(self):
        """!Passes control to the hwrf.prep.PrepHybrid.inputiter().

        Iterates over the prep member's inputiter, a
        hwrf.prep.PrepHybrid.inputiter().  Yields all input data
        information needed by the hwrf.input module to pull input
        data."""
        for d in self.prep.inputiter(): yield d

    def make_init(self,detinit):
        """!Creates initialization tasks.

        Called from the constructor.  Creates the initialization
        tasks, prep, realinit and wrfanl; and links to the
        deterministic init geogrid and metgrid tasks.  This is called
        by the constructor, to create the needed inputs to the fcst
        member.
        @param detinit the deterministic model initialization, an
          hwrf.init.HWRFInit or hwrf.init.InitBeforeGSI."""

        self.geogrid=detinit.geogrid
        #self.realbdy=detinit.realinit
        self.metgrid=detinit.metgrid
        wrf=self.sim
        moad=wrf.get_moad()
        geodat=self.geogrid.geodat(moad)

        realsection=self.confstr('realinit')
        prepsection=self.confstr('prep_hybrid')

        self.prep=hwrf.prep.PrepHybrid(
            self.dstore,self.conf,prepsection,self.sim,geodat,
            atime=self.anlintime,taskname=self.taskname+'.prep',
            workdir=self.workdir+'/prep',outdir=self.outdir+'/prep')

        self.realinit=hwrf.fcsttask.RealNMM(
            self.dstore,self.conf,realsection,self.sim,
            taskname=self.taskname+'.real',keeprun=False,
            outdir=self.outdir+'/real',workdir=self.workdir+'/real') \
            .add_geogrid(self.geogrid)\
            .add_metgrid(self.metgrid)\
            .add_prep_hybrid(self.prep)

        # WCOSS workaround:
        self.realinit.sim.set_active_io_form_to(2)

        self.prep.tvset('enkfmem',self.enkfmem)

        # We use a WRFGhost instead of a WRFAnl since WRFGhost
        # disables the history stream.
        self.wrfanl=hwrf.fcsttask.WRFGhost(
            self.dstore,self.conf,realsection,self.sim,False,
            atime=self.anlintime,taskname=self.taskname+'.wrfanl',
            workdir=self.workdir+'/wrfanl',outdir=self.outdir+'/wrfanl')\
            .add_geogrid(self.geogrid) \
            .add_metgrid(self.metgrid) \
            .add_fort65(self.realinit) \
            .add_wrfinput(self.realinit) \
            .add_wrfbdy(self.realinit)

    def _make_track(self):
        """Makes the gribber and tracker member variables."""

        ensdadoms=[ d for d in self.fcst.sim ]
        d2=ensdadoms[-1]
        postdomains=[ d2 ]
        self.post=PostManyWRF(self.fcst,postdomains,self.conf,
                 self.confstr('post'),6*3600,needcrtm=False,
                 streams=['history'],taskname=self.taskname+'.post',
                 outdir=self.outdir+'/post',workdir=self.workdir+'/post')

        # Regridding stuff:
        grid2=hwrf.regrib.igrb1(self.post,domain=d2)
        trksub=hwrf.regrib.GRIBSubsetter(
            'part',hwrf.tracker.tracker_subset,None)
        domloc=hwrf.regrib.FixedLocation(lat=self.conf['config','domlat'],
                                         lon=self.conf['config','domlon'])
        stormloc=hwrf.regrib.FixedLocation(lat=self.storminfo.lat,
                                           lon=self.storminfo.lon)
        basin=self.storminfo.pubbasin2
        if ((basin.upper()=='AL' or basin.upper()=='EP') \
                and domloc.ewcenter<360.0):
            domloc.ewcenter+=360.0
        r=hwrf.regrib.RegribMany(copygb=alias(exe(self.conf.getexe('copygb'))),
                                 wgrib=alias(exe(self.conf.getexe('wgrib'))))
        r.add('d2',clatlon(grid2,res=[0.045,0.045],size=[30.,30.],
                           scan=136,n=[667,667]))
        r.add('trkgrid',grid2*r.grid('d2'))
        r.add('hwrftrk',hwrf.tracker.vinttave(r.GRIB('trkgrid')/trksub))
        basedir=self.outdir+'/regribber'
        enkf000='%03d'%int(self.enkfmem)
        r.to_intercom('{out_prefix}.hwrftrk.grbf{fahr:02d}.mem'+enkf000,'hwrftrk')
        self.gribber=hwrf.gribtask.GRIBTask(
            self.dstore,self.conf,self.confstr('regribber'),r,
            start=self.anlintime,step=6*3600,end=6*3600,
            taskname=self.taskname+'.regribber',
            atime=self.anlintime,workdir=self.workdir+'/regribber')
        self.tracker=hwrf.tracker.TrackerTask(
            self.dstore,self.conf,self.confstr('tracker'),
            taskname=self.taskname+'tracker',start=self.anlintime,
            step=6*3600,end=6*3600,outdir=self.outdir+'/tracker',
            workdir=self.workdir+'/tracker')
        self.tracker.add_moving_grid(self.storminfo,self.gribber,'hwrftrk')
        self.tracker.send_atcfunix(
            'track0','{com}/{out_prefix}.trak.hwrf.atcfunix.mem'+enkf000)

    def products(self,**kwargs):
        """!Iterates over all forecast products.

        Passes control to hwrf.fcsttask.AnalysisCycle.products() to
        iterate over all products that match the specified arguments.
        @param kwargs passed to hwrf.fcsttask.AnalysisCycle.products()"""
        for p in self.fcst.products(**kwargs):
            yield p

    def run(self):
        """!Runs the initialization and forecast for this ensemble member.

        Runs the prep, realinit, wrfanl and fcst member tasks, using input
        from the GFS ENKF, and the deterministic initialization."""
        where=self.workdir
        logger=self.log()
        logger.info('Run ensemble member in %s'%(where,))
        with produtil.cd.NamedDir(where,keep=False,keep_on_error=True,
                                  logger=logger):
            self.prep.run()
            self.realinit.run()
            self.wrfanl.run()
            self.fcst.run()
            if self.track:
                with self.dstore.transaction() as t:
                    self.post.unrun()
                    self.gribber.unrun()
                self.post.run()
                self.gribber.run()
                if self.gribber.is_completed():
                    self.tracker.run()
                else:
                    msg='Error regridding inputs to tracker.  See earlier log messages for details.'
                    logger.error(msg)
                    raise hwrf.exceptions.GribberError(msg)
            self.state=COMPLETED

    def _make_relocate_kwargs(self,track,modin,dest_dir,priorcycle):
        """Makes a dict containing the keyword arguments to send in to
        the constructor for the hwrf.relocate task(s).
          modin - the modin argument to the constructor
          dest_dir - the directory in which to run the relocate"""

        ensdadoms=[ d for d in self.fcst.sim ]
        kwargs=dict(
            sim=self.fcst.sim,domains=ensdadoms,
            modin=modin,dest_dir=dest_dir,
            workdir=self.workdir+'/relocate',
            outdir=self.outdir+'/relocate')

        if priorcycle is not None: kwargs.update(ensda=priorcycle)
        if track: kwargs.update(parentTrack=self.tracker,trackName='track0')
        return kwargs

    def _make_relocate(self,track,modin,priorcycle):
        """Makes the relocation, rstage1, rstage2 and rstage3 member
        variables.
          track - the track argument to the constructor
          modin - the modin argument to the constructor"""

        dest_dir=os.path.join(self.workdir,'relocate')
        kwargs=self._make_relocate_kwargs(track,modin,dest_dir,priorcycle)

        self.relocation=hwrf.relocate.Relocation(self.dstore,self.conf,
               self.confstr('relocate'),
               taskname_pattern=self.taskname+'relocate.stage'+'%d',
               **kwargs)

        self.rstage1=self.relocation.rstage1
        self.rstage3=self.relocation.rstage3

    def run_relocate(self):
        """Runs the relocate jobs, if present."""
        if 'rstage1' in self.__dict__:
            self.rstage1.delete_temp()
            self.rstage1.run()
            if 'rstage3' in self.__dict__:
                self.rstage3.run()
            if self.rstage1.scrub and self.rstage3.scrub:
                self.rstage3.delete_temp()

########################################################################

def write_ensda_flag_file(flag_file,run_ensda):
    """!Writes the stormX.run_ensda flag file.

    Writs the storm*.run_ensda flag file for this cycle.  The purpose
    of the file is to tell the workflow and scripting layers whether
    the ensemble needs to be run.  The file will contain a single
    line: RUN_ENSDA=YES or RUN_ENSDA=NO.  Will also log a message to
    the jlogfile at INFO level telling which was written.
    @param flag_file the full path to the flag file
    @param run_ensda True or False: should the ENSDA be run? """
    if run_ensda is True:
        with open(flag_file,'wt') as f:
            f.write('RUN_ENSDA=YES\n')
        produtil.log.jlogger.info('Will run HWRF ENSDA for this cycle.')
    else: # None, False, anything else:
        with open(flag_file,'wt') as f:
            f.write('RUN_ENSDA=NO\n')
        produtil.log.jlogger.info('Disabled HWRF ENSDA for this cycle.')

def read_ensda_flag_file(flag_file):
    """!Reads the stormX.run_ensda flag file

    This function is used by the scripting and workflow layers to
    determine if the data assimilation ensemble should be run.  Reads
    the storm*.run_ensda flag file line by line, searching for a
    single line RUN_ENSDA=YES or RUN_ENSDA=NO.  Returns True for YES
    or False for No, based on the last such line seen.  Returns None
    otherwise."""
    run_ensda=None
    with open(flag_file,'rt') as f:
        for line in f:
            if line.find('RUN_ENSDA=YES')>=0:
                run_ensda=True
            elif line.find('RUN_ENSDA=NO')>=0:
                run_ensda=False
    return run_ensda

########################################################################

class Storm1Ensda(hwrf.hwrftask.HWRFTask):
    """!Tells HWRF to run the ENSDA for storm 1, but not any other storms."""
    def __init__(self,dstore,conf,section,**kwargs):
        """!Storm1Ensda constructor.

        Create a new Storm1Ensda which will instruct HWRF to run ensda
        only for a specific storm priority, by default storm 1.  This
        is intended only for testing purposes.
        @param dstore the produtil.datastore.Datastore database object
        @param conf   the hwrf.config.HWRFConfig with configuration info
        @param section  the section to use within conf
        @param kwargs passed to the superclass constructor
        """
        super(Storm1Ensda,self).__init__(dstore,conf,section,**kwargs)
        self.__ensda_flag_file=self.confstr('tdr_flag_file')
    def should_run_ensda(self):
        """!Should ENSDA be run? Returns true if the storm is priority 1,
        and false otherwise."""
        storm_num=self.conf.get('config','storm_num')
        storm_num=int(storm_num)
        return storm_num==1
    def write_flag_file(self,run_ensda):
        """!Write the ensda flag file.

        Calls hwrf.ensda.write_ensda_flag_file to write the flag file.
        @param run_ensda True means the ensemble should be run, False
          if it should not be run."""
        write_ensda_flag_file(self.__ensda_flag_file,run_ensda)
        #ensda_ecflow_comms(self.conf,self.log(),run_ensda)
        if run_ensda: set_ecflow_event('Ensda',self.log())
    def run(self):
        """!creates the storm*.run_ensda file

        Creates the storm1.run_ensda flag file with RUN_ENSDA=YES for
        storm 1, and RUN_ENSDA=NO otherwise."""
        self.write_flag_file(self.should_run_ensda())

########################################################################
class CycleTDRCheck(hwrf.hwrftask.HWRFTask):
    """!Determines if Tail Doppler Radar (TDR) data is available.

    This class checks to see if a specified cycle has Tail Doppler
    Radar data available.  This is the condition used to decide
    whether to run the DA ensemble in the 2015 Operational HWRF."""
    def __init__(self,dstore,conf,section,cycle_rel,**kwargs):
        """!CycleTDRCheck constructor.

        Create a new CycleTDRCheck which will look for TDR for the
        specified cycle.  The cycle_rel is anything accepted by
        to_datetime_rel's second argument.
        @param dstore the produtil.datastore.Datastore database object
        @param conf   the hwrf.config.HWRFConfig with configuration info
        @param section  the section to use within conf
        @param cycle_rel specifies the cycle.  This must be a number
          of hours relative to the current cycle (conf.cycle) analysis
          time.  For example, -6*3600 would be the prior cycle and
          48*3600 would be two days from now.
        @param kwargs passed to the superclass constructor"""
        super(CycleTDRCheck,self).__init__(dstore,conf,section,**kwargs)
        incat_name=self.confstr('catalog')
        self.__ensda_flag_file=self.confstr('tdr_flag_file')
        self.__run_ensda=None
        self.tgtcycle=to_datetime_rel(cycle_rel,conf.cycle)
        self.__in_catalog=hwrf.input.DataCatalog(
            self.conf,incat_name,self.tgtcycle)
        dataset=self.confstr('dataset','tdr')
        item=self.confstr('item','gdas1_bufr')
        obstype=self.confstr('obstype','tldplr')
        self.__tdrdict=dict(self.taskvars,dataset=dataset,item=item,
            obstype=obstype,atime=self.tgtcycle,ftime=self.tgtcycle,
            optional=True)
        self._dirname=self.workdir
        self._stormid='999'

    ##@var tgtcycle
    # the cycle for whom TDR data is checked

    def should_run_ensda(self):
        """!Should ENSDA be run?

        If self.run was called in this process, returns the cached
        result of that.  Otherwise, reads the run_ensda flag file from
        COM.  Uses hwrf.ensda.read_ensda_flag_file()"""
        if self.__run_ensda is None:
            self.__run_ensda=read_ensda_flag_file(self.__ensda_flag_file)
        return self.__run_ensda

    def inputiter(self):
        """!Iterates over needed files from upstream workflows.

        This iterator's purpose is to provide "all files external
        to this workflow" that are needed by the task.  In this case,
        of course, it is only the TDR bufr_d file.  Hence, there is a
        single "yield" statement that requests the TDR."""
        yield self.__tdrdict

    def run(self):
        """!creates the storm*.run_ensda file

        Creates the storm1.run_ensda flag file with RUN_ENSDA=YES if
        the TDR data is available, and RUN_ENSDA=NO otherwise."""

        run_ensda=False

        try:
            if self._actually_run():
                run_ensda=True
        finally:
            self.write_flag_file(run_ensda)

    def write_flag_file(self,run_ensda):
        """!Write the ensda flag file.

        Calls hwrf.ensda.write_ensda_flag_file to write the flag file.
        @param run_ensda True means the ensemble should be run, False
          if it should not be run."""
        write_ensda_flag_file(self.__ensda_flag_file,run_ensda)
        #ensda_ecflow_comms(self.conf,self.log(),run_ensda)
        if run_ensda: set_ecflow_event('Ensda',self.log())

    def tdr_this_cycle(self):
        """!Check if TDR data is available for this cycle

        Checks the on-disk input data to see if the TDR data is
        available."""
        logger=self.log()
        atime=to_datetime(self.conf.cycle)
        self._in_catalog=hwrf.input.DataCatalog(
                         self.conf,self.confstr('catalog'),atime)
        item=self.conf.get('tdr_new_obstype','item')
        ds=os.path.join(self.getdir('intercom'),'bufrprep')
        it=self._in_catalog.parse(item,atime=atime,
                            logger=logger,obstype='tldplr')
        there=os.path.join(ds,it)
        logger.info('TDR bufrprep should be at %s'%there)
        if isnonempty(there):
            return True
        else:
            return False

    def _actually_run(self):
        """!Search on disk for TDR or a trigger file.

        Do not call this routine directly; it is an internal
        implementation routine.  This routine contains the code that
        actually determines if TDR is present or not.  The "run"
        routine calls _actually_run in a try-finally block to ensure
        the run_ensda flag file is created no matter what."""
        logger=self.log()

        input_catalog=self.conf.get('config','fcst_catalog')
        if not self.conf.has_section(input_catalog):
            logger.error('Forecast catalog section [%s] does not exist. '
                         'Disabling ENSDA.'%(input_catalog,))
            return False
        dcomenv=os.path.join(os.environ.get(
            'DCOMROOT','/you/forgot/to/set/DCOMROOT'),'us007003')
        dcom=self.conf.get(input_catalog,'dcom',dcomenv)
        if self.realtime and dcom and os.path.isdir(dcom):
            if self.tdr_this_cycle():
                logger.info('TDR data is available for current cycle %s!'
                    'Enabling ENSDA.'%self.conf.cycle.strftime('%Y%m%d%H'))
                return True
            elif self.read_trigger_file():
                logger.info('There will be TDR data for cycle %s!'
                    'Enabling ENSDA.'%self.tgtcycle.strftime('%Y%m%d%H'))
                return True
            else:
                logger.warning('ensda trigger file is not found. '
                               'TDR data is not available for current cycle. '
                               'Will continue without ENSDA.')
                return False
        else:
            ic=self.__in_catalog
            there_it_is=ic.locate(**self.__tdrdict)
            if there_it_is is None:
                logger.error(
                    'Configuration error: DataCatalog %s does not know how '
                    'to find TDR data.  Will continue without ENSDA.'
                    %(repr(ic),))
                return False
            elif not isnonempty(there_it_is):
                logger.warning(
                    '%s: %s Tail Doppler Radar bufr_d file is empty or '
                    'non-existant.  Will continue without ENSDA.'
                    %(there_it_is,self.tgtcycle.strftime('%Y%m%d%H')))
                return False
            else:
                logger.info('%s: TDR data found for cycle %s!  Enabling ENSDA.'
                             %(there_it_is,self.tgtcycle.strftime('%Y%m%d%H')))
                return True

    def read_trigger_file(self):
        """!Read TDR trigger file for operational run

        Reads the TDR trigger file sent by the Aircraft Operations
        Center (AOC) before a NOAA P3 Orion flight.  This is used in
        NCEP Operations when running the operational HWRF, to
        determine if TDR is going to be available soon."""
        logger=self.log()
        atime=to_datetime(self.tgtcycle)
        ymdh=atime.strftime('%Y%m%d%H')
        basin=self.storminfo.pubbasin2
        if int(ymdh[0:4]) <= 2013:
            self._stormid=self.storminfo.stnum
        elif basin.upper()=='AL':
            self._stormid='%s%02d' % ('1',self.storminfo.stnum)
        elif basin.upper()=='EP':
            self._stormid='%s%02d' % ('2',self.storminfo.stnum)
        elif basin.upper()=='CP':
            self._stormid='%s%02d' % ('3',self.storminfo.stnum)
        else:
            self._stormid='999'

        input_catalog=self.conf.get('config','fcst_catalog')
        dcom=self.conf.get(input_catalog,'dcom')
        if os.path.isdir(dcom):
            btime=to_datetime_rel(-24*3600,atime)
            tank1=os.path.join(dcom,atime.strftime("%Y%m%d"),'b006/xx070')
            tank2=os.path.join(dcom,btime.strftime("%Y%m%d"),'b006/xx070')
            logger.info('Locations: tank1 at %s tank2 at %s'%(tank1,tank2))
            with NamedDir(self._dirname,keep=not self.scrub,logger=logger):
                numtry=self.confint('numofcheck',1)
                timeinv=self.confint('checksecinv',300)
                stime=timeinv/2
                n=1
                while n<=numtry:
                    if isnonempty(tank1):
                        logger.info('tank1 exist')
                        make_symlink(tank1,'tldplrbufr',force=True,logger=logger)
                        self.readensdatrigger(self._stormid,ymdh)
                    if not isnonempty('runensda') and ymdh[8:10]=='00' \
                        and isnonempty(tank2):
                        make_symlink(tank2,'tldplrbufr',force=True,logger=logger)
                        self.readensdatrigger(self._stormid,ymdh)
                    if n<numtry:
                        if wait_for_files(
                            'runensda',logger,
                            maxwait=timeinv,sleeptime=stime,min_size=1,
                            min_mtime_age=5,min_atime_age=None,
                            min_ctime_age=None,min_fraction=1.0):
                            logger.info('found trigger file')
                            n=numtry+1
                    n+=1
                if isnonempty('runensda'):
                    return True
                else:
                    totalwait=timeinv*(numtry-1)/60.0
                    logger.info('waited for %s minutes, ensda trigger'
                                ' is not found'%str(totalwait))
                    return False
        else:
            logger.warning('%s does not exist. This is not wcoss.'
                        'real-time ensda trigger can only be run on wcoss'%dcom)
            return False

    def readensdatrigger(self,stmidin,tgtcycle):
        """!Runs the hwrf_readtdrtrigger program.

        Runs the hwrf_readtdrtrigger program to decide if the TDR
        trigger file is for this storm or not.
        @param stmidin the storm
        @param tgtcycle the cycle of interest"""
        self.log().info('readensdatrigger')
        logger=self.log()
        fprog = 'hwrf_readtdrtrigger'
        prog  = self.getexe(fprog)
        cmd = produtil.run.exe(prog) << stmidin+" "+tgtcycle
        produtil.run.checkrun(cmd,logger=logger)

########################################################################
class AlwaysRunENSDA(CycleTDRCheck):
    """!Used in place of CycleTDRCheck to force ENSDA to always run.

    This subclass of CycleTDRCheck instructs ENSDA to run whether
    TDR is available or not."""
    def should_run_ensda(self):
        """!Returns True.

        Always returns True, indicating that ENSDA should always be
        run even if the world is imploding and pigs are raining from
        clouds of cotton candy.
        @returns True"""
        return True
    def _actually_run(self):
        """!Runs the TDR check and ignores its results.

        Calls the superclass _actually_run, so that the TDR check is
        done, and then returns True regardless of the TDR
        availability.  Logs a warning about this to the
        produtil.log.jlogger.
        @returns True"""
        if not super(AlwaysRunENSDA,self)._actually_run():
            msg="OVERRIDE: Will run ENSDA anyway due to "\
                "configuration settings."
            self.log().warning(msg)
            jlogger.warning(msg)
        return True

########################################################################
def enada_pre_object_for(ds,conf,section,next_cycle):
    """!Generates a CycleTDRCheck or AlwaysRunENSDA based on
    configuration settings.

    Reads the [config] section ensda_when option to decide what TDR
    check class should be used.
      * ensda_when="tdr_next_cycle" - create a CycleTDRCheck
      * ensda_when="always" - create an AlwaysRunENSDA
      * ensda_when=anything else - raise an exception
    @param ds,conf,section,next_cycle - passed to the constructor"""
    ensda_when=conf.getstr('config','ensda_when','tdr_next_cycle').lower()
    if ensda_when=='tdr_next_cycle':
        return hwrf.ensda.CycleTDRCheck(
            ds,conf,'tdrcheck',next_cycle)
    elif ensda_when=='storm1':
        return hwrf.ensda.Storm1Ensda(ds,conf,'tdrcheck')
    elif ensda_when=='always':
        return hwrf.ensda.AlwaysRunENSDA(
            ds,conf,'tdrcheck',next_cycle)
    else:
        raise ValueError('The ensda_when option must be set to tdr_next_cycle or always (case-insensitive) not %s.'%(repr(ensda_when),))

