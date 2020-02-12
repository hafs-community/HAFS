"""!This module implements Python classes that run the 2014 HWRF Relocation.

The HWRF relocation is divided into four parts:

* Stage1 --- remove the prior cycle's vortex

* Stage2 --- remove the parent vortex

* Stage3 --- Relocate and paste the fields together

* Merge --- merge the relocation output and GSI.  This is only used when
GSI is enabled.

In addition, due to the requirement of splitting the relocation into
three stages, there are "relocation info" files for passing
information between the three.  The RelocationInfo object provides a
means to read and write these files."""

##@var __all__
# List of symbols to export via "from hwrf.relocate import *"
__all__=['Stage1', 'Stage2', 'Stage3', 'Merge', 'RelocationInfo',
         'WARM', 'COLD']

import os, shutil
import glob
import time
import math
import re
import itertools
import string
import urlparse
import datetime
import pdb
import subprocess
import logging

import hwrf.namelist
import hwrf.exceptions
import produtil.datastore
import produtil.run
import produtil.locking
import produtil.fileop
from produtil.ecflow import set_ecflow_label

from ConfigParser       import ConfigParser, SafeConfigParser, RawConfigParser
from hwrf.hwrftask      import HWRFTask
from produtil.run       import checkrun, run,exe, bigexe, alias
from hwrf.numerics      import partial_ordering, TimeArray, to_timedelta, \
                               within_dt_epsilon, to_datetime_rel,        \
                               to_datetime, to_fraction
from produtil.datastore import FileProduct, COMPLETED, FAILED, RUNNING
from produtil.cd   import TempDir, NamedDir
from produtil.fileop    import deliver_file, make_symlink, isnonempty, \
                               remove_file
from hwrf.exceptions    import RelocationError, RelocateOutputMissing, \
                               StormRadiusError

##@var WARM
# A constant used by the RelocationInfo to represent warm starts.
WARM=object()

##@var COLD
# A constant used by the RelocationInfo to represent cold starts.
COLD=object()

class RelocationInfo(object):
    """!Passes information about relocation status between relocation stages.

    This class is used to pass information between the relocate
    stages.  There are three public member variables that ARE meant to
    be written depending on logic within the relocation Stage1, Stage2
    and Stage3 classes.  One then writes out the relocation status
    information to an intermediate file at the end of each relocation
    stage using self.write_info.  It is read in by a later step of the
    relocation, or by the merge, via RelocationInfo(filename,...)

    * iflag_cold = 1 or 0 used by several of the fortran relocation
      programs

    * warm_cold_flag = the constants WARM, COLD or None, depending on
      whether this is a warm start, a cold start, or an unknown state.

    * cold_ok = True only if the relocation intentionally vetoed a
      warm start, such as for a weak storm

    *  initopt = 0, full vortex initialization; 1, relocation only

    *  ensda_relocate = True relcocation for ensemble member"""
    def __init__(self,filename=None):
        """!Creates a new RelocationInfo object by reading in the
        specified *.info file."""
        self.iflag_cold=0
        self.warm_cold_flag=None
        self.cold_ok=False
        self.initopt=0
        self.ensda_relocate=False
        self.ensda_relocate_continue=False
        if filename is not None:
            self.read_info(filename)
        self.from_file=filename
        return

    ##@var from_file
    # The file that was read in.

    ##@var initopt
    # Initialization flag variable for the relocation.

    ##@var iflag_cold
    # An int 0 or 1 used by several of the Fortran relocation programs
    # to trigger based on warm or cold starts.

    ##@var warm_cold_flag
    # The constants WARM, COLD or None to indicate warm vs. cold starts.
    # * WARM --- this is a warm start (prior cycle vortex is in use)
    # * COLD --- this is a cold start (no prior cycle vortex in use)
    # * None --- it is not known whether this is a cold or warm start.

    ##@var cold_ok
    # Set to True if the relocation intentionally vetoes warm starting.
    # This is done, for example, if the storm is weak or shallow.
    def __str__(self):
        """!A Pythonic string representation of this object."""
        return 'RelocationInfo(iflag_cold=%s,warm_cold_flag=%s,cold_ok=%s,'\
            'initopt=%s,ensda_relocate=%s,ensda_relocate_continue=%s)'\
            %(repr(self.iflag_cold),self.warm_cold_str(),repr(self.cold_ok),
              repr(self.initopt),repr(self.ensda_relocate),
              repr(self.ensda_relocate_continue))
    def make_warm_cold(self,value,logger=None):
        """!Returns the module-level constants COLD or WARM, or None,
        for the specified string value.

        @param value A string that is "WARM", "COLD" or "NONE" and is
        case-insensitive.  If it has any other value, None is returned
        and a message is logged at ERROR level (if a logger is given).
        @param logger a logging.Logger for log messages."""
        v=str(value).upper()
        if v=='COLD':   return COLD
        elif v=='WARM': return WARM
        elif v=='NONE': return None
        else:
            if logger is not None:
                logger.error(
                    'Invalid value %s (string %s) for warm_cold_flag.  '
                    'Assuming None' %(repr(value),repr(v)))
            return None
    def warm_cold_str(self):
        """!This is the opposite of the make_warm_cold routine: it
        returns "COLD", "WARM", or "None" for the constants COLD, WARM
        or None based on self.warm_cold_flag"""
        if   self.warm_cold_flag is COLD: return 'COLD'
        elif self.warm_cold_flag is WARM: return 'WARM'
        else:                             return 'None'
    def read_info(self,filename,logger=None):
        """!Reads the relocation information into this object from the
        specified filename.  This is called automatically by the
        constructor.  Error messages are logged to the given logger,
        if one is provided.
        @param filename the name of the file to read
        @param logger a logging.Logger for log messages"""
        if not isinstance(filename,basestring):
            raise TypeError(
                'RelocationInfo.read_info expects a string for its '
                'filename parameter.  You provided a %s %s.'
                %(type(filename).__name__,repr(filename)))
        with open(filename,'rt') as f:
            scp=SafeConfigParser(defaults={
                    'iflag_cold':'0', 'warm_cold_flag':'None',
                    'cold_ok':'False', 'initopt':'0',
                    'ensda_relocate':'False',
                    'ensda_relocate_continue':'True'})
            scp.readfp(f)
        self.iflag_cold=scp.getint('info','iflag_cold')
        self.warm_cold_flag=self.make_warm_cold(
            scp.get('info','warm_cold_flag','None'))
        self.cold_ok=scp.getboolean('info','cold_ok')
        self.initopt=scp.getint('info','initopt')
        self.ensda_relocate=scp.getboolean('info','ensda_relocate')
        self.ensda_relocate_continue=scp.getboolean('info','ensda_relocate_continue')

    def write_info(self,filename,logger=None):
        """!Writes this object's relocation information to the
        specified *.info file.  Logs errors to the specified logger,
        if one is present.
        @param filename the file to write
        @param logger a logging.Logger for log messages"""
        if logger is not None:
            logger.info('Generating in-memory ConfigParser object for '
                        'RelocationInfo')
        filename=str(filename)
        thedir=os.path.dirname(filename)
        produtil.fileop.makedirs(thedir,logger=logger)
        c=RawConfigParser()
        c.add_section('info')
        c.set('info','iflag_cold',str(self.iflag_cold))
        c.set('info','warm_cold_flag',self.warm_cold_str())
        c.set('info','cold_ok',str(self.cold_ok))
        c.set('info','initopt',str(self.initopt))
        c.set('info','ensda_relocate',str(self.ensda_relocate))
        c.set('info','ensda_relocate_continue',str(self.ensda_relocate_continue))

        if logger is not None:
            logger.info('Writing RelocationInfo to file: %s'%(filename,))
        with open(filename,'wt') as f:
            c.write(f)

class RelocationTask(HWRFTask):
    """!This is a HWRF task that forms the base class for all vortex
    relocation tasks, including the Merge.  It exists solely to reduce
    code complexity."""
    def __init__(self,dstore,conf,section,sim,domains,taskname=None,
                 modin='GDAS1',wrfanl=None,wrfghost=None,wrfinput=None,
                 parentTrack=None,trackName='track0',ghost_domains=None,
                 dest_dir=None,gsi_d02=None,gsi_d03=None,
                 gsi_d01=None,cycling_interval=-6*3600,info=None,
                 fgat_times=None,centrack=None,ensda=None,**kwargs):
        """!RelocationTask constructor.

        @param dstore the produtil.datastore.Datastore for database storage
        @param conf the hwrf.config.HWRFConfig for configuration info
        @param section the configuration section to use
        @param sim the hwrf.wrf.WRFSimulation describing the simulation being relocated
        @param domains the hwrf.wrf.WRFDomains being relocated
        @param taskname the taskname in the database
        @param modin input model: "GFS" or "GDAS1".
        @param wrfanl the wrfanl input source, which should have a get_wrfanl() function
           that accepts an hwrf.wrf.WRFDomain and returns an hwrf.datastore.Product
        @param wrfghost the ghost file input source, which should have a get_ghost() function
           that accepts an hwrf.wrf.WRFDomain and returns an hwrf.datastore.Product
        @param wrfinput the wrfinput datasource, which must have a get_wrfinput() function
           that returns an hwrf.datastore.Product for the wrfinput file
        @param parentTrack the parent track file input source, which must have a products()
           function that takes the track product name and returns the produtil.datastore.Product
        @param trackName the parent track name to pass to parentTrack.products()
        @param ghost_domains the list of ghost domains for relocation input
        @param dest_dir output directory for the relocation
        @param gsi_d02 the hwrf.gsi.FGATGSI that will run the GSI for the intermediate domain.
        @param gsi_d03 the hwrf.gsi.FGATGSI that will run the GSI for the innermost domain.
        @param gsi_d01 the hwrf.gsi.GSIBase that will run GSI for the outermost domain.
        @param cycling_interval negative number of seconds between cycles (-6*3600)
        @param info the RelocationInfo object to use
        @param fgat_times the list of FGAT times, datetime.datetime objects
        @param centrack the product for the center FGAT time track
        @param kwargs passed to hwrf.hwrftask.HWRFTask.__init__"""
        assert(not isinstance(domains,basestring))
        self.__rinfo=None
        if taskname is None:
            taskname=section
        self.info = RelocationInfo() if (info is None) else info
        if 'location' not in kwargs:
            kwargs['location']=os.path.join(conf.getdir('intercom'),taskname)

        self.fgat_times=fgat_times

        cycling_interval=-cycling_interval
        if cycling_interval>0:
            cycling_interval=-cycling_interval

        self.cycling_interval=to_timedelta(cycling_interval)

        super(RelocationTask,self).__init__(dstore,conf,section,taskname,
                                            **kwargs)
        self._ensda=ensda

        logger=self.log()

        if fgat_times is not None:
            parent_cycle=to_datetime_rel(cycling_interval,self.conf.cycle)
            for t in fgat_times:
                t=to_fraction(t-parent_cycle)
                t=int(float(round(t)))
                if t<0:
                    raise ValueError(
                        'In RelocationTask.__init__, all fgat_times must '
                        'be 0 or greater (times relative to the parent '
                        'model).  You gave: %s.'%(repr(t),))

        self.sim = sim
        logger.debug('domains: '+repr(domains))
        self.domains=[sim[domain] for domain in domains]
        self.dt_epsilon=to_timedelta(300) # five minutes
        self._gsi_d01 = gsi_d01
        self._gsi_d02 = gsi_d02
        self._gsi_d03 = gsi_d03

        if wrfanl is not None:
            self._wrfanl_d02    = wrfanl.get_wrfanl(self.domains[1])
            self._wrfanl_d03    = wrfanl.get_wrfanl(self.domains[2])
        else:
            self._wrfanl_d02=None
            self._wrfanl_d03=None

        if self._ensda is not None:
            for domain in self.domains:
                if domain.is_moad():
                    self._ensda_wrfinput_d01 = self._ensda.get_wrfinput(
                                               domain,atime=self.conf.cycle)
                else:
                    self._ensda_wrfinput_d02 = self._ensda.get_wrfanl(
                                               domain,atime=self.conf.cycle)
                    assert(self._ensda_wrfinput_d02 is not None)
        else:
            self._ensda_wrfinput_d01=None
            self._ensda_wrfinput_d02=None

        self._wrfanl        = wrfanl

        if wrfinput is not None:
            self._wrfinput      = wrfinput.get_wrfinput()
        else:
            self._wrfinput=None

        if wrfghost is not None:
            logger.debug('ghost domains: '+repr(ghost_domains))
            logger.debug('ghost domains iterated: '
                         +repr([ d for d in wrfghost.wrf() ]))
            self.ghost_domains = [wrfghost.wrf()[domain]
                                  for domain in ghost_domains]
            self._ghost_d02     = wrfghost.get_ghost(ghost_domains[1])
            assert(self._ghost_d02 is not None)
            self._ghost_d03     = wrfghost.get_ghost(ghost_domains[2])
            assert(self._ghost_d03 is not None)
        self._wrfghost      = wrfghost
        self._parentTrack   = parentTrack
        self._trackName     = trackName
        self._centrack = centrack

        if dest_dir is None:
            logger.debug('self._wrfanl='+repr(self._wrfanl))
            logger.debug('self._wrfanl.wrf()='+repr(self._wrfanl.wrf()))
            logger.debug(
                'self._wrfanl.wrf().simstart()='+
                repr(self._wrfanl.wrf().simstart()))
            logger.debug(
                'self._wrfanl.wrf().simstart().strftime(%%Y%%m%%d%%H)='
                +repr(self._wrfanl.wrf().simstart().strftime('%Y%m%d%H')))
            dest_dir=os.path.join(
                self.getdir('WORKhwrf'),modin.lower()+'.'+
                self._wrfanl.wrf().simstart().strftime('%Y%m%d%H'))
        self.dest_dir=dest_dir
        logger.debug('self.dest_dir='+self.dest_dir)

        # fhr used as input arguments
        dt = self.sim.simstart() - self.conf.cycle
        self._fhr = 6 + dt.days*24 + dt.seconds //3600

        # Set the DA mode input
        self._modin=modin

        # Figure out if we are a cold start or cycled run.
        oldcom=self.getdir('oldcom')
        oldsid=self.getdir('oldsid')
        if os.path.isdir(oldcom):
            self._warm = True
            self._prev_cycle_dir = oldcom
            self._prev_cycle_sid = oldsid
        else:
            self._warm = False
            self._prev_cycle_dir = ''
            self._prev_cycle_sid = ''


       # Figure out the vortex origin model/status.
        if self.modin == 'GFS':
            self._vortex = 'GFS'
        elif self.modin == 'ENKF':
            self._vortex = 'ENKF'
        else:
            if self._gsi_d02 is not None or self._gsi_d03 is not None:
                self._vortex = 'GDAS'
            else:
                self._vortex = 'HDAS'

    ##@var modin
    # The input model: GFS or GDAS1

    def get_wrfinput(self,domain=None):

        """!Returns the wrfinput output Product for the specified
        domain, or None if no such domain is known.
        @returns None
        @param domain the domain of interest
        @note This is a abstract function that should be replaced in subclasses."""
        return None

    ##@var info
    # A RelocationInfo object to trade relocation information with other stages of the relocation.

    ##@var fgat_times
    # The list of FGAT hours.

    ##@var cycling_interval
    # The positive datetime.timedelta time between cycles.

    ##@var sim
    # The hwrf.wrf.WRFSimulation describing the WRF simulation

    ##@var domains
    # The list of domains from sim that match the domains with the same name provided to
    # the constructor.

    ##@var ghost_domains
    # The list of ghost domains passed to the constructor.

    ##@var dt_epsilon
    # An epsilon value for time equality comparisons

    ##@var dest_dir
    # Delivery directory for outputs.

    def get_wrfanl(self,domain):
        """!Returns the wrfanl output Product for this Task for the
        specified domain or None if no such product exists
        @param domain the domain of interest
        @returns None
        @note This is a abstract function that should be replaced in subclasses."""
        return None

    def get_ghost(self,domain):
        """!Returns the wrfghost output Product for this Task for the
        specified domain
        @param domain the domain of interest
        @note This is a abstract function that should be replaced in subclasses."""
        return None

    def wrfinput_at_time(self,atime,domain):
        """!Returns the wrfinput output file for the specified time and
        domain, or returns None if no such file exists.
        @param atime the time of interest
        @param domain the domain of interest"""
        if atime is not None and  \
                not within_dt_epsilon(atime,self.sim.simstart(),
                                      self.dt_epsilon):
            self.log().info(
                'wrfinput_at_time: atime=%s is not near my time %s'
                %(atime.strftime('%Y%m%d%H'),domain.strftime('%Y%m%d%H')))
            return None
        return self.get_wrfinput(domain)

    def wrfanl_at_time(self,atime,domain):
        """!Returns the wrfanl output file for the specified time and
        domain, or None if no such file exists.
        @param atime the time of interest
        @param domain the domain of interest"""
        if atime is not None and  \
                not within_dt_epsilon(atime,self.sim.simstart(),
                                      self.dt_epsilon):
            self.log().info(
                'wrfanl_at_time: atime=%s is not near my time %s'
                %(atime.strftime('%Y%m%d%H'),domain.strftime('%Y%m%d%H')))
            return None

    def copy_fixed(self):
        """!Copies the fixed files to the local directory."""
        logger = self.log()
        tbl=self.confstr('tbl','')
        if tbl=='':
            tbl=self.confstrinterp('{FIXhwrf}/hwrf_eta_micro_lookup.dat')
        assert(tbl)
        assert(isnonempty(tbl))
        make_symlink(tbl,"eta_micro_lookup.dat",
                     force=True, logger=logger)

    def delete_temp(self):
        """!Deletes all temporary files created by the relocation jobs."""
        logger=self.log()
        dd=self.dest_dir
        if dd is None:
            logger.warning('Internal error: dest_dir is None.  Cannot '
                           'delete temporary files.')
        elif dd=='':
            logger.warning('Internal error: dest_dir is the empty '
                           'string.  Cannot delete temporary files.')
        else:
            logger.warning(str(dd)+': deleting this directory tree.')
            if not os.path.exists(dd):
                logger.warning(
                    str(dd)+': does not exist; nothing to delete.')
                return
            def rmerr(function,path,exc_info):
                logger.info('%s: exception while deleting file: %s %s'
                            %(str(path),str(function),str(exc_info)))
            try:
                shutil.rmtree(dd,onerror=rmerr)
            except EnvironmentError as ee:
                logger.warning(
                    str(dd)+': unable to delete this directory tree; '
                    'continuing anyway: '+str(e),exc_info=True)
            except Exception as e:
                logger.warning(
                    str(dd)+': unhandled exception deleting this '
                    'directory tree: '+str(e),exc_info=True)
                raise

    def products(self):
        """!Iterates over all products generated by this task.
        @note This is a abstract function that should be replaced in subclasses."""
        if False: yield 'hello' # to ensure this is an interator

    def deliver_products(self,missing=None,logger=None,keep=False,
                         frominfo=None,**kwargs):
        """!Delivers products to intercom via Product.deliver.  Any
        keyword arguments are passed on to Product.deliver.  By
        default, keep=False, which means the local copy of the file
        may no longer exists.  If frominfo is specified, it will be
        ignored.
        @param keep If True, then the file may be moved to the destination
          instead of copying.
        @param frominfo Ignored.
        @param kwargs Passed to produtil.datastore.FileProduct.deliver()
        @param logger a logging.Logger for log messages
        @param missing a function called if the file to deliver does not exist.
          It is passed the product and the basename of the file."""
        if logger is None: logger=self.log()
        logger.warning('Delivering products for %s'%(self.taskname,))
        produtil.fileop.makedirs(self.location,logger=logger)
        for p in self.products():
            loc=p.location
            bloc=os.path.basename(loc)
            if os.path.exists(bloc):
                logger.warning('%s: deliver product from ./%s'%(p.did,bloc))
                p.deliver(frominfo=bloc,keep=keep,logger=logger,**kwargs)
            else:
                logger.warning('%s: ./%s does not exist.  Cannot deliver.'
                               %(p.did,bloc))
                if missing is not None:
                    missing(p,bloc)

    def write_vitals(self,filename='tcvitals.as',logger=None):
        """!Writes the tcvitals (from self.storminfo) to the specified
        file.
        @param filename the file to write
        @param logger a logging.Logger for log messages"""
        if logger is None: logger=self.log()
        logger.info('Writing tcvitals to %s'%(repr(filename),))
        with open(filename,'wt') as f:
            f.write(self.storminfo.as_tcvitals()+"\n")
        assert(os.path.exists(filename))

    def make_ghost_namelist(self,filename,logger=None):
        """!Writes the ghost namelist to namelist_ghost.input.  Note
        that this overwrites, and then deletes, namelist.input and
        fort.12.  It will also create the domain.center and
        storm.center files and fill them with correct locations.
        @param filename the file to write
        @param logger a logging.Logger for log messages"""
        if logger is None: logger=self.log()
        if hasattr(self._wrfghost,'make_ghost_namelist'):
            self._wrfghost.make_ghost_namelist(filename,logger=logger)
        else:
            self._wrfghost.make_namelist('namelist_ghost.input')
            produtil.fileop.remove_file('namelist.input',logger=logger)
            produtil.fileop.remove_file('fort.12',logger=logger)

    def make_analysis_namelist(self,filename,logger=None):
        """!Writes the analysis namelist to namelist_analysis.input.
        Note that this overwrites, and then deletes, namelist.input
        and fort.12.  It will also create the domain.center and
        storm.center files and fill them with correct locations.
        @param filename the file to write
        @param logger a logging.Logger for log messages"""
        if logger is None: logger=self.log()
        if hasattr(self._wrfanl,'make_analysis_namelist'):
            self._wrfanl.make_analysis_namelist(filename,logger=logger)
        else:
            self._wrfanl.make_namelist('namelist_analysis.input')
            produtil.fileop.remove_file('namelist.input',logger=logger)
            produtil.fileop.remove_file('fort.12',logger=logger)

    @property
    def parent_track(self):
        """!The Product object for the parent track file."""
        assert(self._parentTrack is not None)
        assert(self._trackName is not None)
        assert(self._trackName)
        return self._parentTrack.product(self._trackName)

    @property
    def modin(self):
        """!The DA mode."""
        return self._modin

    @property
    def vortex(self):
        """!The vortex origin status."""
        return self._vortex

    @property
    def warm(self):
        """!The status of the current cycle.
        True if it is a cycled run.
        False if it is a cold start."""
        return self._warm

    @property
    def prev_cycle_dir(self):
        """!The previous cycle's COM directory."""
        return self._prev_cycle_dir
    @property
    def prev_cycle_sid(self):
        return self._prev_cycle_sid

    @property
    def storm_intensity(self):
        """!The storm intensity."""
        return self.storminfo.wmax

    @property
    def storm_basin(self):
        """!The storm basin."""
        return self.storminfo.pubbasin2

    @property
    def center_lat(self):
        """!The domain center latitude."""
        return self.conf.getfloat('config','domlat')

    @property
    def center_lon(self):
        """!The domain center longitude."""
        return self.conf.getfloat('config','domlon')

    @property
    def storm_id(self):
        """!The storm ID."""
        return self.storminfo.stormid3

    @property
    def fhr(self):
        """!The forecast hour."""
        return self._fhr

    @property
    def rinfo(self):
        """!The RelocationInfo."""
        if self.info is not None and self.info.from_file is not None:
            return self.info
        if self.__rinfo is not None:
            return self.__rinfo
        tn=type(self).__name__.lower()
        stage='stage3'
        if tn.find('stage1')>0: stage='stage1'
        if tn.find('stage2')>0: stage='stage2'
        filename=os.path.join(self.outdir,stage+'.info')
        rinfo=RelocationInfo(filename)
        assert(rinfo.from_file is not None)
        rinfo.read_info(filename,self.log("info"))
        self.__rinfo=rinfo
        return self.__rinfo

    def _make_plist_and_names(self):
        """!Internal function to generate input product lists and names.

        This is an internal implementation function that should not be
        called directly.  It returns a three-element tuple containing
        a list of products, and a dict mapping from product to the
        local filename, and a dict mapping from product to the copy
        method.  This is used to implement copy_inputs, to copy input
        files to the local directory from remote tasks' Products."""
        def copier(p,name,logger,*args):
            deliver_file(p.location,name,logger=logger,keep=True)
        def linker(p,name,logger,*args):
            make_symlink(p.location,name,force=True,logger=logger)
        names=dict()
        names[self._wrfinput]='wrfinput_d01'
        names[self._wrfanl_d02]='wrfinput_d02'
        names[self._wrfanl_d03]='wrfinput_d03'
        names[self._ghost_d02]='wrfghost_d02'
        names[self._ghost_d03]='wrfghost_d03'
        plist=[ k for k in names.iterkeys() ]
        actions=dict( (n,copier) for n in names.iterkeys() )
        return ( plist, names, actions )

    def copy_inputs(self):
        """!Copies, or makes, one or more input files."""
        logger=self.log()
        (plist,names,action)=self._make_plist_and_names()
        def namer(p,logger,*args):      return names[p]
        def actor(p,name,logger,*args): action[p](p,name,logger,*args)
        for p in plist:
            logger.info("Need product %s at location=%s, available=%s"%(
                    p.did,repr(p.location),p.available))
        # Loop over all provided products and copy them.  Note that we
        # do not wait for them (maxtime=2) we just use
        # wait_for_products to do the looping for us:
        if len(plist)!=produtil.datastore.wait_for_products(plist,logger,namer,actor,maxtime=2):
            raise hwrf.exceptions.RelocationInputError("Some inputs to the relocation are missing.  The previous init job must have failed.")
        #deliver_file('wrfghost_d02', 'wrfghost_d02_orig', keep=True)
        #deliver_file('wrfghost_d03', 'wrfghost_d03_orig', keep=True)

        # Lastly, write the tcvitals:
        self.write_vitals('tcvitals.as')

    def set_ensda(self,ensda):
        self._ensda=ensda

    def get_centrack(self):
        """!Returns the Product for the center FGAT time track file if
        available, or otherwise the parent track file Product."""
        if self._centrack is None:
            return self.parent_track
        return self._centrack

    def set_centrack(self,centrack):
        """!Sets the Product for the center FGAT time track file.
        @param centrack the center FGAT track product"""
        if (centrack is None):
            raise hwrf.exceptions.RelocationConfigurationError(
                "You must specify central track file")
        self._centrack=centrack

    def del_centrack(self):
        """!Unsets the center FGAT time track file so that
        get_centrack() will return the parent track file instead."""
        self._centrack=None

    ##@property centrack
    # The track file for the center FGAT hour.
    centrack=property(get_centrack,set_centrack,None,
                      """The track file for the center FGAT hour.""")

    def create_atcf(self,case):
        """!Gets the parent vortex track file, either from a specified
        directory or from the tracker, run by a previous
        hwrf.init.HWRFInit object's tracker member.
        @param case 1 or 2: 1 for creating the atcfunix file, 2 for
        the atcfunix_cen file."""
        logger=self.log()
        if case == 1:
            tp=self.parent_track
        else:
            tp=self.centrack
        logger.info('case %d:tp %s'%(int(case),repr(tp)))

        produtil.datastore.wait_for_products(tp,logger,maxtime=2,
              renamer=lambda p,l: 'gfs-anl.atcfunix',
              action=lambda p,n,l: deliver_file(p.location,n,logger=l))

        start    = self.sim.simstart().strftime("%Y%m%d%H")
        gfs_atcf = self.confstr('gfs_atcf','')

        if tp.available and tp.location:
            ta_atcf=tp.location
            logger.warning('Using parent vortex from provided tracker data.')
            logger.warning('%s (%s): parent vortex location'
                           %(tp.did, ta_atcf))
            if case == 1:
                deliver_file(ta_atcf, 'atcfunix',logger=logger)
            else:
                deliver_file(ta_atcf, 'atcfunix_cen',logger=logger)
        elif gfs_atcf is not None and gfs_atcf!='':
            logger.warning(
                "Using parent vortex from parent model's own track file")
            basin    = self.conf.syndat.pubbasin2
            fields   = self.confstr('track_name','AVNO|PRE1|PRD1')
            patn     = '(?=%s)(?=%s)(?=%s)' %(start, basin, fields)
            logger.warning("%s: track file"%( gfs_atcf, ))
            with open(gfs_atcf, 'r') as ifile:
                logger.info("Parsing track for: start=%s basin=%s fields=%s"
                            %(repr(start),repr(basin),repr(fields)))
                with open('atcfunix', 'w') as ofile:
                    for line in file:
                        if re.search(patn, line):
                            logger.info('Keep   : %s\n'%(repr(line),))
                            ofile.write(line)
                        else:
                            logger.info('Discard: %s\n'%(repr(line),))
            logger.info('Done parsing track file.')
        else:
            # Send this to .error so it will go to the NCEP-wide jlogfile:
            logger.error(
                'Could not find a track file for parent vortex location.')
            logger.error(
                'Will proceed assuming parent vortex is at tcvitals.')
            with open('atcfunix', 'a') as o: pass

    def run_ext(self, cmd, echo=None, inputs=None, incopies=None,
                outputs=None, opt_outputs=None):
        """!Helper function for running Fortran programs that need
        fort.* files for inputs and outputs.

        Run an external command linking in fort.X files for input and
        output.  If self.redirect=True, redirect logs to a separate
        file.

        @param cmd The command to execute.  This function will use
        "self.getexe()" on the command to find the external program to
        execute.
        @param echo If a list is passed in as the echo variable, then
        the contents will be sent to the stdin of the program as a
        string.

        @param inputs Input dictionary for files to link.  See below.

        @param incopies Input dictionary for files to copy.  See below.

        @param outputs Output dictionary for files to link. See below.
        If the output is not present, a message is logged at ERROR
        level.

        @param opt_outputs Optional outputs dictionary for files to
        link.  See below.  If the outputs are not present, it is not
        considered an error.

        The dictionary arguments should consist of a fortran file
        number and the source file.

        @code
          inputs = {11:tcvitals, 12:wrfout_d01}
        @endcode

        would produce symbolic links:
        @code{.unformatted}
           fort.11 -> tcvitals
           fort.12 -> wrfout_d01
        @endcode

        input files can also be copied using incopies:
        @code
           incopies = {11:tcvitals, 12:wrfout_d01}
        @endcode

        would create files instead of links.

        The outputs and opt_outputs (optional outputs) should be of the
        dictionary as the inputs. As in:
           outputs = {56:new_data_4x, 85:storm_radius}
        this would mean the "fort.56" file would be renamed to "new_data_4x"
        and the "fort.85" renamed to "storm_radius".

        If opt_outputs is given then the fortran file is tested to see if it
        exists and only if it does is it renamed to the output filename.

        A log file will be created consisting of the stdout and stderr of the
        command run. It will be named consisting of the taskname and command.
        For example, if this is relocation stage 1 and the command is
        hwrf_pert_ct then the log file is "rel_stage_1_hwrf_pert_ct.log" """

        cmdname=str(cmd)
        logger = self.log()
        prog   = self.getexe(cmdname)
        logf   = '%s/logs/%s_%s.log' %(self.dest_dir,
                                       self.__class__.__name__, cmdname)

        # Build up the command
        if echo:
            echostr=""
            for s in echo:
                if isinstance(s,float): echostr+="%g "%(s,)
                elif isinstance(s,int): echostr+="%d "%(s,)
                else:                   echostr+="%s "%(str(s),)
            logger.info(
                'Converted %s to %s for stdin input to fortran command.'
                %(repr(echo),repr(echostr)))
            echostr+="\n"
            cmd = produtil.run.openmp(produtil.run.bigexe(prog)) << echostr
        else:
            cmd = produtil.run.openmp(produtil.run.bigexe(prog))

        # If redirection is requested, do so:
        if self.redirect: cmd = cmd >= logf

        # Clean up all the fortran inputs and outputs
        empty={}
        if inputs is None: inputs=empty
        if outputs is None: outputs=empty
        if incopies is None: incopies=empty
        iof = dict(itertools.chain(inputs.items(), outputs.items(),
                                   incopies.items()))
        for k in iof:
            produtil.fileop.remove_file('fort.'+str(k),logger=logger)

        # Link the inputs
        if inputs:
            produtil.fileop.fortlink(inputs, force=True,logger=logger)

        if incopies:
            produtil.fileop.fortcopy(incopies, force=True,
                                     only_log_errors=True, logger=logger)

        logger.warning(repr(cmd)) # use logger.warning so it is in stderr
        produtil.run.checkrun(cmd, logger=logger)

        # Rename the outputs
        if outputs:
            for k, v in outputs.iteritems():
                ffile='fort.'+str(k)
                if os.path.exists(ffile):
                    deliver_file(ffile, v, keep=False,logger=logger)
                else:
                    logger.error('%s: did not make file %s (would mv to %s)'
                                 %(cmdname,ffile,str(v)))

        # Rename the optional outputs if they exist
        if opt_outputs:
            for k, v in opt_outputs.iteritems():
                ffile = 'fort.' + str(k)
                if os.path.exists(ffile):
                    deliver_file(ffile, v, keep=False,logger=logger)
                else:
                    logger.warning(
                        '%s: did not make file %s (would mv to %s).'
                        %(cmdname,ffile,str(v)))

        # Clean up the input links
        for k,v in inputs.iteritems():
            if os.path.islink('fort.'+str(k)):
                logger.info('%s: deleting input fort file (symlink to %s)'
                            %('fort.'+str(k),v))
                produtil.fileop.remove_file('fort.'+str(k),logger=logger)

        # Clean up the input copies
        for k,v in incopies.iteritems():
            if os.path.exists('fort.'+str(k)):
                logger.info('%s: deleting input fort file (copy of %s)'
                            %('fort.'+str(k),v))
                produtil.fileop.remove_file('fort.'+str(k),logger=logger)

########################################################################
class Stage1(RelocationTask):
    """!This is a HWRF task that encapsulates stage 1 of the vortex
    relocation."""

    def __init__(self,dstore,conf,section,sim,domains,taskname=None,**kwargs):
        """!Stage1 constructor.

        @param dstore,conf,section,sim,domains,taskname,kwargs
          Passed to RelocationTask.__init__()"""
        super(Stage1,self).__init__(dstore,conf,section,sim,domains,
                                    taskname,**kwargs)

    def run(self):
        """!Runs the stage 1 of the relocation."""
        logger=self.log()
        # NOTE: some of these are sent as postmsg instead of
        # logger.info.  That ensures they are in the jlogfile, which
        # contains information across all cycles of all storms and all
        # models.  That way, we can find unexpected cold starts.
        read_info=False
        try:
            dest_dir=self.dest_dir
            if os.path.exists(dest_dir):
                self.delete_temp()
            produtil.fileop.makedirs(dest_dir)
            self.info.initopt=self.confint('initopt',0)
            tdrflagfile=self.conf.strinterp('dir','{com}/{stormlabel}.tdr')
            if self._ensda is None and self.confbool('tdrconditionalvinit',False):
                if isnonempty(tdrflagfile):
                    self.info.initopt=1
                else:
                    self.info.initopt=0
            self.info.iflag_cold=0
            with NamedDir(dest_dir) as dir:
                self.postmsg('Stage 1 running in directory: '+os.getcwd())
                assert(not re.match('\A/tmp',os.getcwd()))

                produtil.fileop.makedirs(dest_dir+'/logs')
                if self._wrfghost is not None or self._wrfanl is not None:
                    self.copy_namelist()

                if self._ensda is None:
                    have_prior=self.check_prior_cycle()

                    if not have_prior:
                        self.postmsg('Prior cycle missing.  Cold start.  '
                                     'Continue from Stage 2.')
                        self.info.iflag_cold=1
                        self.info.warm_cold_flag=COLD
                        expect=self.confbool('expect_cold_start')
                        if expect:
                            self.postmsg('No prior cycle exists, and no prior '
                                         'cycle was expected.  Cold start.  '
                                         'Continue from Stage 2.')
                            set_ecflow_label('cycling','COLD start: no prior cycle',logger)
                            self.info.cold_ok=True
                        else:
                            set_ecflow_label('cycling','COLD start: prior cycle missing data',logger)
                            msg='UNEXPECTED COLD START.  Prior cycle data was '\
                                'missing.  Continue from Stage 2.  To override '\
                                'this error, set expect_cold_start=yes or '\
                                'allow_fallbacks=yes in the conf file for this '\
                                'cycle.'
                            logger.critical(msg)
                            self.info.cold_ok=False
                            if not self.fallback('unexpected_cold_start',
                                                 'A prior cycle existed for this storm, but critical cycling data was missing.'):
                                raise hwrf.exceptions.UnexpectedColdStart(msg)

                    else:
                        if self.storminfo.wmax<14:
                            # NOTE: If you change this, change the
                            # weak_invest default in rocoto/run_hwrf.py.
                            set_ecflow_label('cycling','COLD start: storm is weak',logger)
                            self.postmsg('Storm is a weak storm.  This is a '
                                         'cold start.  Continue from Stage 2.')
                            self.info.iflag_cold=1
                            self.info.warm_cold_flag=COLD
                            self.info.cold_ok=True
                        else:
                            self.write_vitals('tcvitals.as')
                            self.copy_fixed()

                            self.relocate_storm()
                            self.merge_nest()
                            self.create_atcf(1)
                            if not self.copy_hdas():
                                set_ecflow_label('cycling','COLD start: no prior cycle track',logger)
                                self.postmsg('Cannot get prior cycle track.  '
                                             'This is a cold start.')
                                self.info.cold_warm_flag=COLD
                                self.info.cold_ok=False
                            elif self.check_atcf_hours():
                                self.postmsg('This is a warm start.')
                                set_ecflow_label('cycling','WARM start',logger)
                                self.guess_track()
                                self.split_wrf()
                                self.pert_ct()
                            else:
                                set_ecflow_label('cycling','COLD start: hdas_atcfunix is missing times',logger)
                                self.postmsg('The hdas_atcfunix does not have '
                                               'all expected forecast hours.')
                                self.postmsg('The create_trak_guess cannot '
                                               'continue with relocation.')
                                self.postmsg('This is a cold start.')
                                self.info.warm_cold_flag=COLD
                                self.info.cold_ok=True

                    self.postmsg('Stage 1 completed in directory: '
                                 +os.getcwd())
                else:
                    self.info.iflag_cold=1
                    self.info.initopt=1
                    self.info.ensda_relocate=True
                    self.write_vitals('tcvitals.as')
                    self.relocate_storm()
                    self.merge_nest()
                    if not self.copy_ensda_track():
                        self.postmsg('Cannot get prior cycle ensda track.')
                        raise hwrf.exceptions.EnsdaTrackerMissing('ensda track missing')
                    if self.check_atcf_hours():
                        self.guess_track()
                        self.split_wrf()
                        self.pert_ct()
                    else:
                        self.info.ensda_relocate_continue=False
                        self.postmsg('The ensda_atcfunix does not have '
                                       'all expected forecast hours.')
                        self.postmsg('Will not perform relocation for this member')

            self.state=COMPLETED
        except Exception as e:
            logger.critical('Stage 1 failed: '+str(e),exc_info=True)
            raise
        finally:
            self.info.write_info(os.path.join(self.outdir,'stage1.info'),
                                 logger=logger)
    def copy_namelist(self):
        """!Copy the namelist files from the preceding steps"""
        self.log().info('stage1 copy_namelist')
        if self._wrfghost is not None:
            self.make_ghost_namelist('namelist.input.ghost')
        if self._wrfanl is not None:
            self.make_analysis_namelist('namelist.input.analysis')

    def check_prior_cycle(self):
        """!Checks to see if all data is present from the prior cycle."""
        logger=self.log()
        logger.warning('Checking for prior cycle data.')
        # NOTE: we assume wrfout files use colons between date
        # components.  The copywrf.py ensures this when copying to
        # com.
        ftimestr=self.sim.simstart().strftime("%Y-%m-%d_%H:%M:%S")
        if self.prev_cycle_sid is False:
            sid=self.storm_id.lower()
        else:
            sid=self.prev_cycle_sid.lower()

        for d in self.domains:
            id    = d.get_grid_id()
            ifile = "%s/%s.wrfout_d%02d_%s" %(self.prev_cycle_dir,
                        sid, id,ftimestr)
            logger.info('ifile is %s' %ifile)
            if not isnonempty(ifile):
                logger.warning(
                    'Prior cycle %s forecast does not exist.  This is a '
                    'cold start.'%(ftimestr,))
                return False
        logger.warning('Prior cycle data is present for time %s'%(ftimestr,))
        if self.fgat_times is not None:
            for t in self.fgat_times:
                ftimestr2=t.strftime("%Y-%m-%d_%H:%M:%S")
                for d in self.domains:
                    id    = d.get_grid_id()
                    ifile = "%s/%s.wrfout_d%02d_%s" %(self.prev_cycle_dir,
                            sid, id,ftimestr2)
                    if not isnonempty(ifile):
                        logger.warning(
                            'Fgat wrfout_d%02d file does not exist for forecast time '
                            '%s.  This is a cold start.'%(id,ftimestr2,))
                        return False
                    else:
                        logger.info('Have wrfout_d%02d file for forecast time '
                                    '%s.'%(id,ftimestr2,))
            logger.warning('Prior cycle data is present for all fgat times')
        else:
            logger.warning('No fgat_times specified.  Prior %s forecast is '
                           'available.'%(ftimestr,))
        return True

    def relocate_storm(self):
        """!Runs the hwrf_3dvar to paste the relocated storm."""
        self.log().info('stage1 relocate_storm')
        fprog = 'hwrf_3dvar'
        logger=self.log()
        ftimestr=self.sim.simstart().strftime("%Y-%m-%d_%H:%M:%S")
        ncks=self.getexe('ncks','')
        if  not ncks:
            ncks=produtil.fileop.find_exe('ncks',raise_missing=False)
        if ncks:
            def copier(s,t,x):
                produtil.fileop.remove_file(t,logger)
                checkrun(bigexe(ncks)['-6','-O',s,t],logger=logger)
        else:
            copier=None

        domains=[ d for d in self.domains ]
        if self.prev_cycle_sid is False:
            sid=self.storm_id.lower()
        else:
            sid=self.prev_cycle_sid.lower()

        for domain in domains:
            if self._ensda is None:
                id    = domain.get_grid_id()
                ifile = "%s/%s.wrfout_d%02d_%s" %(self.prev_cycle_dir,
                            sid, id,ftimestr)
            else:
                did=int(domain.get_grid_id())
                if domain.is_moad():
                    id=1
                    prod=self._ensda.get_wrfinput(domain=domain,atime=self.conf.cycle)
                else:
                    id=2
                    prod=self._ensda.get_wrfanl(domain=domain,atime=self.conf.cycle)
                logger.info('domain %s prod %s'%(str(domain),prod.did))
                ifile=prod.location

            wrfout = "wrfout_d%02d" %id
            old_wrfout = "old_hwrf_d%02d"%id
            if produtil.fileop.netcdfver(ifile)=='HDF5':
                logger.info('%s: file is HDF5, so I will assume it is '
                            'compressed and convert back to 64-bit indexing '
                            'NetCDF3.'%(ifile,))
                if copier is None:
                    logger.critical('ncks not found; things will probably break')

            deliver_file(ifile,wrfout,keep=True,logger=logger,copier=copier)

            prog = self.getexe(fprog)
            log = '%s/logs/%s_%s_d%02d.log' %(
                self.dest_dir, self.__class__.__name__, fprog, id)
            cmd = produtil.run.exe(prog)['storm_relocate', wrfout, 'flnm3',
                                         old_wrfout]
            if self.redirect: cmd = cmd >= log
            produtil.run.checkrun(cmd,logger=self.log())

            if os.path.isfile('fort.50'):
                os.remove('fort.50')

            if os.path.isfile('fort.73'):
                os.remove('fort.73')

    def merge_nest(self):
        """!Runs the fortran merge_nest program."""
        self.log().info('stage1 merge_nest')
        if self._ensda is None:
            fprog = 'hwrf_merge_nest'
        else:
            fprog = 'hwrf_merge_enkf'
        evars = [ 6,
                  self.storm_intensity,
                  0,
                  self.center_lat,
                  self.center_lon,
                ]
        if self._ensda is None:
            ins   = { 11:'tcvitals.as',
                      26:'old_hwrf_d01',
                      36:'old_hwrf_d02',
                      46:'old_hwrf_d03',
                    }
        else:
            ins   = { 11:'tcvitals.as',
                      26:'old_hwrf_d01',
                      36:'old_hwrf_d02',
                    }

        ous   = { 56:'data_4x_hwrf',
                  66:'roughness1',
                }
        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)

    def copy_hdas(self):
        """!Get the previous cycle's HDAS track."""
        logger=self.log()
        logger.info('stage1 copy_hdas')
        if self.storm_intensity > 10:
            dom = 'combine'
        else:
            dom = 'parent'

        if self.prev_cycle_sid is False:
            sid=self.storm_id.lower()
        else:
            sid=self.prev_cycle_sid.lower()

        prev_time = to_datetime_rel(self.cycling_interval,self.conf.cycle)
        hdas_atcf = "%s/%s.trak.hwrf.atcfunix.%s.%s" \
            %(self.prev_cycle_dir, sid,
              prev_time.strftime("%Y%m%d%H"), dom)
        if not os.path.exists(hdas_atcf):
            # Should not get here.  This means the prior cycle's
            # wrfout file exists for hour 6, but the 12hr track file
            # does not.  Either the workflow management system
            # submitted this cycle WAY too early, or the prior cycle's
            # tracker failed.  This should not be possible in
            # operations.
            logger.warning('Prior cycle atcf does not exist: '+hdas_atcf)
            logger.warning('Will use an empty track.')

            # Log an error at CRITICAL level to alert the operator:
            logger.critical('PRIOR HWRF CYCLE PROBABLY FAILED!!')
            logger.critical('Prior cycle has wrfout files, but no 12hr '
                            'track file here: %s'
                            %(hdas_atcf,))
            logger.critical('Check the prior cycle JHWRF_PRODUCTS job for '
                            'errors.  Check to see if NHC received the '
                            'track file.')

            with open('hdas_atcfunix','wt'):
                pass
            return False
        deliver_file(hdas_atcf, 'hdas_atcfunix', keep=True,logger=self.log())
        return True

    def copy_ensda_track(self):
        """Get the previous cycle's ensemble forecast track."""
        logger=self.log()
        logger.info('stage1 copy_ensda_track')

        ensda_atcf=self._ensda.get_track(atime=self.conf.cycle)
        if ensda_atcf is None:
            logger.info('No track for member %s.'%(self._ensda.__enkfmem))
            return False
        else:
            fromfile=ensda_atcf.location
            deliver_file(fromfile,'ensda_atcfunix',keep=True,logger=logger)
            return True

    def check_atcf_hours(self):
        """!Checks to see if all FGAT hours have tracks of a required
        minimum length."""
        logger=self.log()
        seen=set()
        icyc=round(to_fraction(-self.cycling_interval))
        found_icyc=False
        logger.info('self.info.ensda_relocate is %s'%(repr(self.info.ensda_relocate)))
        if self.info.ensda_relocate:
            atcfunix='ensda_atcfunix'
        else:
            atcfunix='hdas_atcfunix'
        with open(atcfunix,'rt') as ha:
            for line in ha:
                rline=line.rstrip()
                try:
                    hour=int(rline[30:33])
                    seen.add(hour*3600)
                    if abs(icyc-hour*3600)<30:
                        logger.info('Found cycling interval hour %d in %s'
                                    %(icyc,atcfunix))
                        found_icyc=True
                    logger.info(
                        'Found hour %s in %s'%(repr(hour),atcfunix))
                except (IndexError,ValueError,TypeError) as e:
                    logger.warning(
                        'Cannot parse hour from %s line: %s'
                        %(atcfunix,rline,))
        if self.fgat_times is None or self.info.ensda_relocate:
            if found_icyc:
                return True
            else:
                logger.warning('Did not find cycling interval hour %d in '
                               'hdas_atcfunix.  This is a cold start.')
                return False

        if not self.info.ensda_relocate:
            """only 3D relocation for ensemble members"""
            all_found=True
            parent_atime=to_datetime_rel(self.cycling_interval,self.conf.cycle)
            for ftime in self.fgat_times:
                dt=to_datetime(ftime)-parent_atime
                fdt=to_fraction(dt)
                fgath=int(float(round(fdt)))
                if fgath not in seen:
                    logger.warning(
                        'Could not find hour %s in hdas_atcfunix'%(repr(fgath),))
                    all_found=False
            if all_found:
                logger.info('All FGAT hours found in hdas_atcfunix')
                return True
            else:
                logger.warning('Some FGAT hours not found in hdas_atcfunix')
                self.info.warm_cold_flag=COLD
                self.cold_ok=True
                return False

    ##@var cold_ok
    # Set to True if the relocation intentionally vetoes warm starting.
    # This is done, for example, if the storm is weak or shallow.

    def guess_track(self):
        """!Runs the fortran hwrf_trk_guess program"""
        self.log().info('stage1 guess_track')
        fprog = 'hwrf_trk_guess'
        evars = [ self.storm_id,
                  '%02d'%(self.storminfo.when.hour,),
                ]
        if self.info.ensda_relocate:
            ins   = { 11:'tcvitals.as',
                      12:'ensda_atcfunix',
                    }
        else:
            ins   = { 11:'tcvitals.as',
                      12:'hdas_atcfunix',
                    }
        ous   = { 30:'trak.fnl.all' }
        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)

    def split_wrf(self):
        """!Runs the fortran wrf_split program."""
        self.log().info('stage1 split_wrf')
        fprog = 'hwrf_wrf_split'
        if self.info.ensda_relocate:
            crfactor=3.0
        else:
            crfactor=1.0

        evars = [ self.fhr,
                  0,
                  self.storm_intensity,
                  self.storm_basin,
                  int(self.info.iflag_cold),
                  crfactor,
                ]
        ins   = { 11:'tcvitals.as',
                  26:'data_4x_hwrf',
                  30:'trak.fnl.all',
                  46:'old_hwrf_d01',
                }
        ous   = { 56:'wrf_env',
                  71:'storm_pert',
                  85:'storm_radius',
                }
        otime = self.sim.simstart().strftime("%Y%m%d%H")
        opts  = { 25:'disturbance.dat',
                  52:'rel_inform.'+ otime,
                  55:'vital_syn.' + otime,
                }
        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous,
                     opt_outputs=opts)
        assert(produtil.fileop.isnonempty('./storm_radius'))

    def pert_ct(self):
        """!Runs the fortran hwrf_pert_ct program."""
        self.log().info('stage1 pert_ct')
        fprog = 'hwrf_pert_ct'
        if self.info.ensda_relocate:
            evars = [ 6,
                      self.storm_basin,
                      1,
                    ]
        else:
            evars = [ 6,
                      self.storm_basin,
                      0,
                    ]

        ins   = { 11:'tcvitals.as',
                  26:'wrf_env',
                  46:'roughness1',
                  65:'storm_radius',
                  71:'storm_pert',
                }
        ous   = { 14:'storm_size_p',
                  23:'storm_sym',
                  58:'storm_pert_new',
                }
        opts  = { 35:'storm_pert_step1_1' }
        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous,
                     opt_outputs=opts)

########################################################################
class Stage2(RelocationTask):
    """!This is a HWRF task that encapsulates stage 2 of the vortex
    relocation which removes the parent model's vortex."""

    def __init__(self,dstore,conf,section,sim,domains,taskname=None,**kwargs):
        """!Stage2 constructor
        @param dstore,conf,section,sim,domains,taskname,kwargs
          Passed to the RelocationTask.__init__() """
        super(Stage2,self).__init__(dstore,conf,section,sim,domains,
                                    taskname,**kwargs)

    def run(self):
        """!Runs stage 2 of the relocation."""
        read_info=False
        logger=self.log()
        try:
            produtil.fileop.makedirs(self.dest_dir)
            with NamedDir(self.dest_dir) as dir:
                self.postmsg('Stage 2 starting in directory: '+os.getcwd())
                assert(not re.match('\A/tmp',os.getcwd()))

                self.info.read_info(
                    os.path.join(self.outdir,'stage1.info'),logger=logger)
                read_info=True

                produtil.fileop.makedirs(self.dest_dir+'/logs')
                self.write_vitals()
                self.copy_fixed()
                self.copy_inputs()
                self.relocate_storm()
#                self.create_nest()
                self.create_atcf(1)
                self.create_track()
                remove_file('roughness',logger=logger)
                remove_file('roughness2',logger=logger)
                self.merge_nests()
                self.wrf_split()
                self.postmsg('Stage 2 completed in directory: '+os.getcwd())
            self.state=COMPLETED
        except Exception as e:
            logger.critical('Stage 2 failed: '+str(e),exc_info=True)
            raise
        finally:
            if read_info:
                self.info.write_info(os.path.join(
                        self.outdir,'stage2.info'),logger=logger)

    def relocate_storm(self):
        """!Runs the hwrf_diffwrf_3dvar program on all inputs to
        create binary file for input to the Fortran programs."""
        self.log().info('stage2 relocate_storm')
        fprog = 'hwrf_3dvar'
        icom_dir = self.conf.getdir('intercom')
        logger=self.log()
        prog = self.getexe('hwrf_3dvar')
        for d in self.domains:
            id = d.get_grid_id()

            fin = "wrfinput_d%02d" %id
            fou = "new_gfs_d%02d"  %id

            log = '%s/logs/%s_%s_d%02d.log' %(
                self.dest_dir, self.__class__.__name__, fprog, id)
            cmd = produtil.run.exe(prog)['storm_relocate', fin, 'flnm3', fou]
            if self.redirect: cmd = cmd >= log
            produtil.run.checkrun(cmd,logger=logger)

            if os.path.isfile('fort.50'):
                os.remove('fort.50')

            if os.path.isfile('fort.73'):
                os.remove('fort.73')

        for id in range(2, 4):

            fin = "wrfghost_d%02d" %id
            fou = "new_ght_d%02d"  %id

            log = '%s/logs/%s_%s_ghost_d%02d.log' %(
                self.dest_dir, self.__class__.__name__, fprog, id)
            cmd = produtil.run.exe(prog)['storm_relocate', fin, 'flnm3', fou]
            if self.redirect: cmd = cmd >= log

            produtil.run.checkrun(cmd,logger=logger)

            if os.path.isfile('fort.50'):
                os.remove('fort.50')

            if os.path.isfile('fort.73'):
                os.remove('fort.73')

    def create_nest(self):
        """!Runs the fortran hwrf_create_nest program."""
        fprog = 'hwrf_create_nest'
        evars = [ 6,
                  self.storm_basin,
                ]
        ins   = { 26:'new_gfs_d01',
                  46:'new_gfs_d02',
                }
        ous   = { 57:'new_data_d01' }
        opts  = { 56:'new_data_1x' }
        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous,
                     opt_outputs=opts)
        logger=self.log()
        deliver_file('new_gfs_d01', 'new_gfs_d01_org',
                     keep=False, logger=logger)
        deliver_file('new_data_d01','new_gfs_d01',
                     keep=False, logger=logger)

    def create_track(self):
        """!Runs the fortran create_trak_fnl program."""
        self.log().info('stage2 create_track')
        fprog = 'hwrf_create_trak_fnl'
        evars = [ self.storm_id,
                  self.sim.simstart().strftime("%Y"),
                  self.storm_basin,
                ]
        ins   = { 11:'tcvitals.as',
                  12:'atcfunix',
                }
        ous   = { 30:'trak.fnl.all_gfs' }
        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)

    def merge_nests(self):
        """!Runs the fortran merge_nest program."""
        self.log().info('stage2 merge_nests')
        fprog = 'hwrf_merge_nest'
        evars = [ 6,
                  0,
                  1,
                  self.center_lat,
                  self.center_lon,
                  self.storm_basin,
                ]
        ins   = { 11:'tcvitals.as',
                  26:'new_gfs_d01',
                  36:'new_gfs_d02',
                  46:'new_gfs_d03',
                }
        ous   = { 56:'data_4x_gfs',
                  66:'roughness2',
                }
        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)

    def wrf_split(self):
        """!Runs the fortran split_wrf program."""
        self.log().info('stage2 wrf_split')

        if os.path.isfile("storm_pert_new"):
            ibgs = 1
        else:
            ibgs = 2

        rel   = 'rel_inform_gfs.%s' %(
            self.sim.simstart().strftime("%Y%m%d%H"))
        vital = 'vital_syn_gfs.%s'  %(
            self.sim.simstart().strftime("%Y%m%d%H"))

        fprog = 'hwrf_wrf_split'
        evars = [ self.fhr,
                  ibgs,
                  self.storm_intensity,
                  self.storm_basin,
                  self.info.iflag_cold,
                  1.0,
                ]
        ins   = { 11:'tcvitals.as',
                  26:'data_4x_gfs',
                  30:'trak.fnl.all_gfs',
                  46:'new_gfs_d01',
                  65:'storm_radius',
                }
        ous   = { 52:rel,
                  56:'gfs_env',
                  71:'storm_pert_gfs',
                  85:'storm_radius_gfs',
                }
        opts  = { 55:vital }

        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous,
                     opt_outputs=opts)


########################################################################
class Stage3(RelocationTask):
    """!This is a HWRF task that encapsulates stage 3 of the vortex
    relocation which relocates and pastes the vortexes together from
    various sources."""

    def __init__(self,dstore,conf,section,sim,domains,taskname=None,**kwargs):
        """!Stage3 constructor.
        @param dstore,conf,section,sim,domains,taskname,kwargs
          Passed to the RelocationTask.__init__()"""
        super(Stage3,self).__init__(dstore,conf,section,sim,domains,
                                    taskname,**kwargs)
        with dstore.transaction() as t:
            self._prod_ghost_d02=FileProduct(dstore,'wrfghost_d02',
                 self.taskname,location=os.path.join(self.location,
                'wrfghost_d02'))
            self._prod_ghost_d03=FileProduct(dstore,'wrfghost_d03',
                self.taskname,location=os.path.join(self.location,
                'wrfghost_d03'))
            self._prod_wrfinput=FileProduct(dstore,'wrfinput_d01',
                self.taskname,location=os.path.join(self.location,
                'wrfinput_d01'))
            self._prod_wrfanl_d02=FileProduct(dstore,'wrfinput_d02',
                self.taskname,location=os.path.join(self.location,
                'wrfinput_d02'))
            self._prod_wrfanl_d03=FileProduct(dstore,'wrfinput_d03',
                self.taskname,location=os.path.join(self.location,
                'wrfinput_d03'))
            self._prod_storm_radius=FileProduct(dstore,'storm_radius',
                self.taskname,location=os.path.join(self.location,
                'storm_radius'))
            self._prod_ens_wrfout_d01=FileProduct(dstore,'wrfout_d01',
                self.taskname,location=os.path.join(self.location,
                'wrfout_d01'))
            self._prod_ens_wrfout_d02=FileProduct(dstore,'wrfout_d02',
                self.taskname,location=os.path.join(self.location,
                'wrfout_d02'))

    def get_ghost(self,domain):
        """!Returns Product objects for the ghost domain output file
        for the specified domain.
        @param domain the domain of interest"""
        logger=self.log()
        logger.debug('get_ghost',repr(domain))
        if domain==self.ghost_domains[1]:
            if self._prod_ghost_d02 is not None:
                logger.debug('is domain 2',repr(self.ghost_domains[1]))
                return self._prod_ghost_d02
            else:
                logger.debug('no prod_ghost_d02')
        else:
            pass # print 'is not domain 2',repr(self.ghost_domains[1])
        if domain==self.ghost_domains[2]:
            if self._prod_ghost_d03 is not None:
                logger.debug('is domain 3',repr(self.ghost_domains[2]))
                return self._prod_ghost_d03
            else:
                logger.debug('no prod_ghost_d03',repr(self.ghost_domains[2]))
        else:
            logger.debug('is not domain 3',repr(self.ghost_domains[2]))
        logger.info('get_ghost: no ghost for domain '+str(domain))

    def get_wrfout(self,domain):
        logger=self.log()
        if not domain in self.domains:
            logger.info('Invalid domain: %s not in %s'%(
                    str(domain), ', '.join([str(x) for x in self.omains])))
            return None
        logger.debug('get_wrfout',repr(domain))
        if domain==self.domains[0]:
            if self._prod_ens_wrfout_d01 is not None:
                logger.debug('is domain 1',repr(self.domains[0]))
                return self._prod_ens_wrfout_d01
            else:
                logger.debug('no prod_ens_wrfout_d01')
        else:
            pass # print 'is not domain 1',repr(self.ghost_domains[0])
        if domain==self.domains[1]:
            if self._prod_ens_wrfout_d02 is not None:
                logger.debug('is domain 2',repr(self.domains[1]))
                return self._prod_ens_wrfout_d02
            else:
                logger.debug('no prod_ens_wrfout_d0',repr(self.domains[1]))
        else:
            logger.debug('is not domain 2',repr(self.domains[1]))
        logger.info('get_wrfout: no wrfout for domain '+str(domain))

    def wrfinput_at_time(self,atime,domain):
        """!Returns a Product object for the wrfinput output file for
        the specified domain if the atime matches this object's
        self.sim.simstart()
        @param atime the time of interest
        @param domain the domain of interest"""
        if not domain in self.sim: return None
        domain=self.sim[domain]
        if atime is not None and  \
                not within_dt_epsilon(atime,self.sim.simstart(),
                                      self.dt_epsilon):
            self.log().info(
                'wrfinput_at_time: atime=%s is not near my time %s'
                %(atime.strftime('%Y%m%d%H'),domain.strftime('%Y%m%d%H')))
            return None
        return self.get_wrfinput(domain)

    def wrfanl_at_time(self,atime,domain):
        """!Returns a Product object for the wrfanl output file for the
        specified domain if the atime matches this objects'
        self.sim.simstart().
        @param atime the time of interest
        @param domain the domain of interest"""
        if atime is not None and  \
                not within_dt_epsilon(atime,self.sim.simstart(),
                                      self.dt_epsilon):
            self.log().info(
                'wrfanl_at_time: atime=%s is not near my time %s'
                %(atime.strftime('%Y%m%d%H'),domain.strftime('%Y%m%d%H')))
            return None
        return self.get_wrfanl(domain)

    def get_wrfanl(self,domain):
        """!Returns a Product object for the wrfanl output file for the
        specified domain.
        @param domain the domain of interest"""
        if domain==self.domains[1] and self._wrfanl_d02 is not None:
            return self._prod_wrfanl_d02
        if domain==self.domains[2] and self._wrfanl_d03 is not None:
            return self._prod_wrfanl_d03

    def get_wrfinput(self,domain=None):
        """!Returns a Product object for the wrfinput output file.  If
        a domain is specified, and is not the correct MOAD, then None
        is returned.
        @param domain the domain of interest"""
        if domain is not None and domain!=self.domains[0]: return None
        return self._prod_wrfinput

    def get_storm_radius(self):
        """!Returns a Product for the storm radius file."""
        return self._prod_storm_radius

    def get_track(self):
        """!Returns a Product for the track file."""
        return self.parent_track

    def products(self,domains=None):
        """!Iterates over all products, or all selected products.
        @param domains If an iterable of domains is given, only
        iterates over products for those domains."""
        logger=self.log()
        if domains is None:
            yield self.get_storm_radius()
            domains=set()
            for d in self.domains: domains.add(d)
            if self._wrfghost is not None:
                for d in self.ghost_domains: domains.add(d)
        hit=set()
        for d in domains:
            # Only iterate over a product once:
            if not d in hit:
                hit.add(d)
            else:
                continue
            if self._ensda is None:
                if d==self.domains[0] and self._wrfinput is not None:
                    yield self._prod_wrfinput
                if d==self.domains[1] and self._wrfanl_d02 is not None:
                    yield self._prod_wrfanl_d02
                if d==self.domains[2] and self._wrfanl_d03 is not None:
                    yield self._prod_wrfanl_d03
                if d==self.ghost_domains[1] and self._ghost_d02 is not None:
                    yield self._prod_ghost_d02
                if d==self.ghost_domains[2] and self._ghost_d03 is not None:
                    yield self._prod_ghost_d03
            else:
                if d==self.domains[0] and self._ensda_wrfinput_d01 is not None:
                    yield self._prod_ens_wrfout_d01
                if d==self.domains[1] and self._ensda_wrfinput_d02 is not None:
                    yield self._prod_ens_wrfout_d02


    def _missing_product(self,prod,basename):
        """!Internal function that raises an exception when a product is missing.

        This is an internal implementation function.  It should not be
        called directly.  This is called by deliver_products when an
        expected input file is missing.  It either returns, or raises
        an exception.  See deliver_products for details.
        @param prod the Product
        @param basename the basename of the missing file"""
        raise RelocateOutputMissing(
            'Mandatory output file %s is missing'%(repr(basename),))

    def run(self):
        """!Runs stage 3 of the vortex relocation."""
        logger=self.log()
        read_info=False
        try:
            produtil.fileop.makedirs(self.dest_dir)
            with NamedDir(self.dest_dir) as dir:
                self.postmsg('Stage 3 running in directory: '+os.getcwd())
                assert(not re.match('\A/tmp',os.getcwd()))

                self.info.read_info(
                    os.path.join(self.outdir,'stage1.info'),logger=logger)
                read_info=True

                if not self.info.ensda_relocate:
                    self.info.read_info(
                        os.path.join(self.outdir,'stage2.info'),logger=logger)
                    read_info=True

                produtil.fileop.makedirs(self.dest_dir+"/logs")
                self.copy_fixed()

                if self.info.ensda_relocate:
                    self.gfs_flag = 1
                    if os.path.exists('storm_pert_new'):
                        logger.info('have storm_pert_new (check 1)')
                        self.ensda_relocate_run()
                else:
                    self.gfs_flag = 6
                    if os.path.exists('storm_pert_new'):
                        logger.info('have storm_pert_new (check 1)')
                        self.info.warm_cold_flag=WARM
                    else:
                        logger.info('do not have storm_pert_new (check 1)')
                        self.info.warm_cold_flag=COLD
                        logger.info('storm intensity is %04d'%(self.storm_intensity))
                        if self.storm_intensity<20 or self.info.initopt==1:
                            logger.info('intensity <20 or initopt=1')
                            self.weak_cold_run()
                            self.gfs_flag=0

                    if os.path.exists('storm_pert_new'):
                        logger.info('have storm_pert_new (check 2)')
                        self.cycled_or_weak_run()
                    else:
                        logger.info('do not have storm_pert_new (check 2)')
                        self.anl_bogus_10m()

                    self.inter_2to2()
                    self.inter_2to2_again()
                    remove_file('flag_file')
                    self.inter_4to6()

                self.update_3dvar()
                self.deliver_products(missing=self._missing_product)
                self.postmsg('Stage 3 completed in directory: '+os.getcwd())
            self.state=COMPLETED
        except Exception as e:
            logger.critical('Stage 3 failed: '+str(e),exc_info=True)
            raise
        finally:
            if read_info: self.info.write_info(os.path.join(
                    self.outdir,'stage3.info'),logger=logger)

    ##@var gfs_flag
    # Initialization flag variable relating to parent model vortex usage.

    ##@var modin
    # Input model: GFS or GDAS

    def weak_cold_run(self):
        """!Runs the portion of the relocation that is used for weak,
        cold storms."""
        self.log().info('stage3 cold_run')
        #if self.storm_intensity < 20:
        self.gfs_flag = 0
        self.pert_ct_weak()
        self.create_atcf(2)
        self.create_track()
        #self.anl_bogus_10m()
        #else:
        #    self.gfs_flag = 6
        #    self.anl_bogus_10m()

    def cycled_or_weak_run(self):
        """!Runs the portion of the relocation that is run for cycled
        or weak storms."""
        self.log().info('stage3 cycled_or_weak_run')
        self.anl_4x(case=1)
        if self.gfs_flag>2 and os.path.isfile('flag_file2') and \
                self.modin == 'GFS':
            self.log().info('gfs_flag>2, have flag_file2 and modin is GFS')
            self.gfs_flag=0
            self.pert_ct_gfs()
            self.anl_4x(case=2)
        if os.path.isfile('flag_file'):
            self.log().info('have flag_file')
            self.anl_cs_10m()
        if os.path.isfile('flag_file2'):
            self.log().info('have flag_file2')
            self.anl_bogus_10m()

    def ensda_relocate_run(self):
        """Runs relocation for ensemble member"""
        self.log().info('stage3 cycled_or_weak_run')
        self.anl_4x(case=3)

    def anl_4x(self, case=2):
        """!Runs the anl_4x programs.
        @param case 1 or 2: why is anl_4x being run."""
        self.log().info('stage3 anl_4x')
        self.log().info('self.fhr %s'%(self.fhr))
        fprog = 'hwrf_anl_4x'
        remove_file('flag_file')
        remove_file('flag_file2')
        evars = [ self.fhr,
                  self.storm_basin,
                  self.gfs_flag,
                  self.info.initopt,
                ]
        if case==1:
            ins   = { 11:'tcvitals.as',
                      12:'hdas_atcfunix',
                      14:'storm_size_p',
                      23:'storm_sym',
                      26:'gfs_env',
                      46:'roughness1',
                      71:'storm_pert_new',
                    }
            if isnonempty('trak.fnl.all'):
                ins[30] = 'trak.fnl.all'
            else:
                ins[30] = 'trak.fnl.all_gfs_cen'
        elif case==2:
            ins   = { 11:'tcvitals.as',
                      12:'atcfunix',
                      14:'storm_size_p',
                      23:'storm_sym',
                      26:'gfs_env',
                      46:'roughness2',
                      71:'storm_pert_new',
                    }
        else:
            ins   = { 11:'tcvitals.as',
                      12:'ensda_atcfunix',
                      14:'storm_size_p',
                      23:'storm_sym',
                      26:'wrf_env',
                      46:'roughness1',
                      71:'storm_pert_new',
                    }
        ous   = { 36:'wrf_env_new' }
        oous  = { 56:'new_data_4x' }

        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous,
                     opt_outputs=oous)

        deliver_file('storm_radius', 'storm_radius_1', keep=True,
                     logger=self.log())
        assert(produtil.fileop.isnonempty('storm_radius'))
        if not os.path.exists('flag_file'):
            self.log().warning(
                'NO FLAG FILE!!! The hwrf_anl_4x program did not make '
                'the flag_file.')

    def anl_cs_10m(self):
        """!Runs the anl_cs_10m fortran program."""
        self.log().info('stage3 anl_cs_10m')
        fprog = 'hwrf_anl_cs'
        assert(self.info.iflag_cold is not None)
        evars = [ 6,
                  self.storm_basin,
                  int(self.info.iflag_cold),
                ]
        axisy_47 = '%s/hwrf_storm_cyn_axisy_47'%(self.getdir('FIXhwrf'))
        storm_20 = '%s/hwrf_storm_20'%(self.getdir('FIXhwrf'))
        ins   = { 11:'tcvitals.as',
                  23:'storm_sym',
                  26:'wrf_env_new',
                  85:'storm_radius',
                  46:'roughness1',
                  }
        inc = {   71:axisy_47,
                  72:axisy_47,
                  73:axisy_47,
                  74:axisy_47,
                  75:axisy_47,
                  76:storm_20,
                  77:storm_20,
                  78:axisy_47,
                }
        ous   = { 56:'new_data_4x' }
        assert(produtil.fileop.isnonempty('storm_radius'))

        self.run_ext(fprog, echo=evars, inputs=ins, incopies=inc, outputs=ous)
        if not os.path.exists('flag_file'):
            self.log().warning(
                'NO FLAG FILE!!! The hwrf_anl_cs_10m program did not make '
                'the flag_file.')

    def anl_bogus_10m(self):
        """!Runs the anl_bogus_10m fortran program."""
        self.log().info('stage3 anl_bogus_10m')
        fprog = 'hwrf_anl_bogus'
        evars = [ 6,
                  self.storm_basin,
                ]
        axisy_47 = '%s/hwrf_storm_cyn_axisy_47'%(self.getdir('FIXhwrf'))
        storm_20 = '%s/hwrf_storm_20'%(self.getdir('FIXhwrf'))
        ins   = { 11:'tcvitals.as',
                  26:'gfs_env',
                  36:'data_4x_gfs',
                  46:'roughness2',
                  61:'storm_pert_gfs',
                  85:'storm_radius_gfs',
                  }
        inc   = { 71:axisy_47,
                  72:axisy_47,
                  73:axisy_47,
                  74:axisy_47,
                  75:axisy_47,
                  76:storm_20,
                  77:storm_20,
                  78:axisy_47,
                }
        ous   = { 56:'new_data_4x' }

        self.run_ext(fprog, echo=evars, inputs=ins, incopies=inc, outputs=ous)

    def pert_ct_weak(self):
        """!Runs hwrf_pert_ct for the weak storm case."""
        self.log().info('stage3 pert_ct_weak')
        logger=self.log()
        def cp(a,b):
            if os.path.exists(a):
                deliver_file(a,b,keep=True,logger=logger)
            else:
                logger.warning(
                    '%s: does not exist; will not copy to %s'%(a,b))

        cp('storm_pert_gfs','storm_pert')
        cp('storm_radius_gfs','storm_radius')
        cp('atcfunix','hdas_atcfunix')
        cp('roughness2','roughness1')

        fprog = 'hwrf_pert_ct'
        evars = [ 6,
                  self.storm_basin,
                  0,
                ]
        ins   = { 11:'tcvitals.as',
                  12:'atcfunix',
                  26:'gfs_env',
                  46:'roughness1',
                  65:'storm_radius',
                  71:'storm_pert',
                }
        ous   = { 14:'storm_size_p',
                  23:'storm_sym',
                  58:'storm_pert_new',
                }

        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)
        assert(produtil.fileop.isnonempty('storm_radius'))

    def pert_ct_gfs(self):
        """!Runs hwrf_pert_ct for the gfs vortex case."""
        self.log().info('stage3 pert_ct_gfs')
        logger=self.log()
        def cp(a,b):
            if os.path.exists(a):
                deliver_file(a,b,keep=True,logger=logger)
            else:
                logger.warning(
                    '%s: does not exist; will not copy to %s'%(a,b))

        remove_file('flag_file',logger=logger)
        remove_file('storm_pert_new',logger=logger)
        remove_file('flag_file2',logger=logger)

        cp('storm_pert_gfs','storm_pert')
        cp('storm_radius_gfs','storm_radius')
        cp('atcfunix','hdas_atcfunix')
        cp('roughness2','roughness1')

        fprog = 'hwrf_pert_ct'
        evars = [ 6,
                  self.storm_basin,
                  0,
                ]
        ins   = { 11:'tcvitals.as',
                  12:'atcfunix',
                  26:'gfs_env',
                  46:'roughness1',
                  65:'storm_radius',
                  71:'storm_pert',
                }
        ous   = {
                  23:'storm_sym',
                  58:'storm_pert_new',
                }

        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)
        assert(produtil.fileop.isnonempty('storm_radius'))

    def inter_2to2(self):
        """!Runs the hwrf_inter_2to2 program."""
        self.log().info('stage3 iter_2to2')
        fprog = 'hwrf_inter_2to2'
        evars = [ 6,
                  1,
                ]
        ins   = { 11:'tcvitals.as',
                  26:'new_data_4x',
                  46:'new_gfs_d01',
                }
        if self._gsi_d02 is not None:
            ins[36] = 'new_ght_d02'
            ous   = { 56:'data_merge_g02' }
        else:
            self.log().info('gsi_d02 not run, interpolate to d02')
            ins[36] = 'new_gfs_d02'
            ous   = { 56:'data_merge_d02' }

        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)

    def inter_2to2_again(self):
        """!Runs the hwrf_inter_2to2 program again."""
        self.log().info('stage3 inter_2to2again')
        fprog = 'hwrf_inter_2to2'
        evars = [ 6,
                  1,
                ]
        ins   = { 11:'tcvitals.as',
                  26:'new_data_4x',
                  46:'new_gfs_d01',
                }
        if self._gsi_d03 is not None:
            self.log().info('stage3 inter_2to2')
            ins[36] = 'new_ght_d03'
            ous   = { 56:'data_merge_g03' }
        else:
            self.log().info('gsi_d03 not run, interpolate to d03')
            ins[36] = 'new_gfs_d03'
            ous   = { 56:'data_merge_d03' }

        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)

    def inter_4to6(self):
        """!Runs the hwrf_inter_4to6 program."""
        self.log().info('stage3 4to6')
        fprog='hwrf_inter_4to6'
        evars = [ 6,
                  self.storm_basin,
                ]
        ins   = { 11:'tcvitals.as',
                  26:'new_gfs_d01',
                  36:'new_data_4x',
                  46:'new_gfs_d01',
                  85:'storm_radius_gfs',
                }
        ous   = { 56:'data_merge_d01' }
        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)
        deliver_file('storm_radius_gfs','storm_radius',
                     keep=True,logger=self.log())

    def update_3dvar(self):
        """!Runs the hwrf_diffwrf_3dvar to update the output files."""
        self.log().info('stage3 update_3dvar')
        logger=self.log()
        fprog = 'hwrf_3dvar'
        prog = self.getexe(fprog)

        for d in self.domains:
            self.log().info('stage3 update_3dvar domain='+str(d))
            id = d.get_grid_id()

            if not self.info.ensda_relocate:
                ifile = 'data_merge_d%02d' %id
                ofile = 'wrfinput_d%02d'   %id

                gsi = '_gsi_d%02d'%id
                if getattr(self, gsi, None) is not None:
                    ifile = 'data_merge_g%02d' %id
                    ofile = 'wrfghost_d%02d'   %id
            else:
                if d.is_moad():
                    continue
                ifile = 'new_data_4x'
                ofile = 'wrfout_d02'

            log = '%s/logs/%s_%s_d%02d.log' %(
                self.dest_dir, self.__class__.__name__, fprog, id)
            cmd = produtil.run.exe(prog)['3dvar_update', ofile, ifile]
            if self.redirect: cmd = cmd >= log
            produtil.run.checkrun(cmd,logger=logger)

    def create_track(self):
        """!Runs the create_trak_fnl program."""
        logger=self.log()
        logger.info('stage3 create_track')
        fprog = 'hwrf_create_trak_fnl'
        evars = [ self.storm_id,
                  self.sim.simstart().strftime("%Y"),
                  self.storm_basin,
                ]
        ins   = { 11:'tcvitals.as',
                  12:'atcfunix_cen',
                }
        ous   = { 30:'trak.fnl.all_gfs_cen' }

        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)

########################################################################

class Relocation(HWRFTask):
    """!This represents all three stages of the relocate.  The
    individual stages may be accessed by rstage1, rstage2 and rstage3.
    The RelocationInfo object that is shared between them can be
    accessed by the "info" member variable"""
    def __init__(self,dstore,conf,section,sim,domains,
                 taskname_pattern=None,**kwargs):
        """!Relocation constructor.  Creates Stage1, Stage2 and Stage3

        @param dstore the produtil.datastore.Datastore for database storage
        @param conf the hwrf.config.HWRFConfig for configuration info
        @param section the configuration section to use
        @param sim the hwrf.wrf.WRFSimulation describing the simulation being relocated
        @param domains the hwrf.wrf.WRFDomains being relocated
        @param taskname_pattern Pattern for generating the subtask tasknames,
          which is passed through make_taskname()
        @param kwargs passed to hwrf.hwrftask.HWRFTask.__init__()
        """
        assert('info' not in kwargs)
        self.info=RelocationInfo()

        tn=self.make_taskname(taskname_pattern,1)
        self.rstage1=Stage1(dstore,conf,section,sim,domains,taskname=tn,
                            info=self.info,**kwargs)

        tn=self.make_taskname(taskname_pattern,2)
        self.rstage2=Stage2(dstore,conf,section,sim,domains,taskname=tn,
                            info=self.info,**kwargs)

        tn=self.make_taskname(taskname_pattern,3)
        self.rstage3=Stage3(dstore,conf,section,sim,domains,taskname=tn,
                            info=self.info,**kwargs)

    ##@var info
    # The RelocationInfo with relocation information to trade between
    # stages

    ##@var rstage1
    # Stage1 of the relocation

    ##@var rstage2
    # Stage2 of the relocaiton

    ##@var rstage3
    # Stage3 of the relocation

    def make_taskname(self,taskname_pattern,istage):
        """!Creates the task name for relocation stage istage based on
        the pattern taskname_pattern.

        @param taskname_pattern The string format for the taskname,
        which must contain exactly one %d.
        @param istage the integer 1, 2 or 3 to substitute into taskname_pattern"""
        istage=int(istage)
        taskname_pattern=str(taskname_pattern)
        return taskname_pattern % (istage)

########################################################################
class Merge(RelocationTask):
    """!This is a HWRF task that merges the WRF analysis files."""

    def __init__(self,dstore,conf,section,relocate,wrfinput,wrfanl,
                 taskname=None,gsi_d01=None,gsi_d02=None,gsi_d03=None,
                 ges_d02=None,ges_d03=None,**kwargs):
        """!Merge constructor
        @param dstore the produtil.datastore.Datastore for database storage
        @param conf the hwrf.config.HWRFConfig for configuration info
        @param section the configuration section to use
        @param relocate the Stage3 of the middle FGAT time relocation
        @param wrfinput The source of parent model data wrfinput files.
        @param wrfanl The source of parent model data wrfanl files.
        @param taskname the task name in the database
        @param gsi_d01,gsi_d02,gsi_d03 hwrf.gsi.FGATGSI classes for the
          GSI
        @param ges_d02,ges_d03 Ghost files for the first guess to GSI.
        @param kwargs passed to hwrf.hwrftask.HWRFTask.__init__        """
        domains=relocate.domains
        ghost_domains=relocate.ghost_domains
        assert(ghost_domains is not None)
        assert(domains is not None)
        self._gsi_d01_input=None if(gsi_d01 is None) else \
            gsi_d01.get_wrfinput()
        self._gsi_d02_ghost=None if(gsi_d02 is None) else \
            gsi_d02.get_ghost(ghost_domains[1])
        self._gsi_d03_ghost=None if(gsi_d03 is None) else \
            gsi_d03.get_ghost(ghost_domains[2])
        self.set_ges(ges_d02,ges_d03)
        assert(self._gsi_d02_ghost is not None or self._gsi_d03_ghost is not None)
        if relocate is not None:
            super(Merge,self).__init__(
                dstore,conf,section,relocate.sim,relocate.domains,taskname,
                modin=relocate._modin,wrfanl=wrfanl,
                wrfghost=relocate._wrfghost,wrfinput=wrfinput,
                ghost_domains=relocate.ghost_domains,
                gsi_d01=gsi_d01,gsi_d02=gsi_d02,gsi_d03=gsi_d03,**kwargs)
            self._input_storm_radius=relocate.get_storm_radius()
        else:
            super(Merge,self).__init__(dstore,conf,section,kwargs['sim'],
                                       kwargs['domains'],taskname,**kwargs)
            self._input_storm_radius=None
        with dstore.transaction() as t:
            self._prod_wrfinput=FileProduct(dstore,'wrfinput_d01',
                self.taskname,location=os.path.join(self.location,
                'wrfinput_d01'))
            self._prod_wrfanl_d02=FileProduct(dstore,'wrfinput_d02',
                self.taskname,location=os.path.join(self.location,
                'wrfinput_d02'))
            self._prod_wrfanl_d03=FileProduct(dstore,'wrfinput_d03',
                self.taskname,location=os.path.join(self.location,
                'wrfinput_d03'))
    def set_ges(self,ges_d02,ges_d03):
        """!Sets the ges_d02 and ges_d03 first guess ghost file sources."""
        if (ges_d02 is None) != (ges_d03 is None):
            raise hwrf.exceptions.RelocationConfigurationError(
                "You must specify both d02 and d03 first guess files "
                "OR neither.  You cannot specify only one of the files.")
        self._ges_d02=ges_d02
        self._ges_d03=ges_d03
    def get_wrfinput(self,domain):
        """!Returns the wrfinput output product for the specified domain
        or None if no such data is available
        @param domain the domain of interest"""
        if domain is not None and domain!=self.domains[0]: return None
        return self._prod_wrfinput

    def wrfanl_at_time(self,atime,domain):
        """!Returns the wrfanl output product for the specified domain
        and time or None if no such data is available.
        @param atime the time of interest
        @param domain the domain of interest"""
        return self.get_wrfanl(domain)

    def get_wrfanl(self,domain):
        """!Returns the wrfanl product for the specified domain or None
        if no such data is available.
        @param domain the domain of interest"""
        if domain==self.domains[0]:
            self.log.error(
                'Requested domain %s, which is the moad.  The MOAD has '
                'no wrfanl file.'%(repr(domain),))
        if domain==self.domains[1]: return self._prod_wrfanl_d02
        if domain==self.domains[2]: return self._prod_wrfanl_d03
        self.log.error('ERROR: requested domain %s, which is not in '
                       'self.domains=%s'%(repr(domain),repr(self.domains)))
    def check_storm_radius(self):
        """!If no relocate was given, gets the storm radius file from a
        fix file.  Also checks to see if the storm_radius file is
        present and non-empty, regardless of whether it came from the
        fix or relocate."""
        logger=self.log()
        if self._input_storm_radius is None:
            storm_radius=os.path.join(self.getdir('FIXhwrf'),
                                      'hwrf_storm_radius')
            logger.warning(
                'Could not get storm_radius from the relocate jobs.')
            logger.warning(
                'Will use the fix file $FIXhwrf/hwrf_storm_radius instead.')
            make_symlink(storm_radius,'storm_radius',force=True,logger=logger)
        if not isnonempty('storm_radius'):
            msg='storm_radius file is missing'
            logger.error(msg)
            raise StormRadiusError(msg)

    def blend_gsi(self):
        """!Runs the hwrf_blend_gsi program if first guess data was
        supplied to the constructor."""
        logger=self.log()
        if self._ges_d02 is None or self._ges_d03 is None:
            logger.warning("First guess not supplied to Merge.__init__.  "
                           "Disabling hwrf_blend_gsi.")
            return False
        elif self._gsi_d03_ghost is not None:
            gsidomain=[ 3 ]
        else:
            gsidomain=[ 2 ]

        prog = self.getexe('hwrf_3dvar')

        for d in gsidomain:
            diffme=[ ['gsiges_d0%d'%d,'new_ges_d0%d'%d],
                     ['wrfghost_d0%d'%d,'anl_ght_d0%d'%d] ]
            for infile,outfile in diffme:
                log = '%s/logs/%s_%s_blend_gsi_diff.log' %(
                    self.dest_dir, self.__class__.__name__, outfile)
                cmd = produtil.run.exe(prog)['storm_relocate', infile,
                                             'flnm3', outfile]
                if self.redirect: cmd = cmd >= log
                produtil.run.checkrun(cmd,logger=logger)
            self.run_ext('hwrf_blend_gsi',[6,self.storm_basin],
                         inputs={11:'tcvitals.as',
                                 26:'anl_ght_d0%d'%d,
                                 36:'new_ges_d0%d'%d},
                         outputs={56:'new_ght_d0%d'%d})
            log = '%s/logs/%s_blend_gsi_update_d0%d.log' %(
                self.dest_dir, self.__class__.__name__,d)
            infile='wrfghost_d0%d'%d
            outfile='new_ght_d0%d'%d
            cmd = produtil.run.exe(prog)['3dvar_update', infile,outfile]
            if self.redirect: cmd = cmd >= log
            produtil.run.checkrun(cmd,logger=logger)
        return True

    def products(self,domains=None):
        """!Iterates over output products
        @param domains if present, only the products for these listed domains will
        be iterated."""
        if domains is None: domains=self.domains
        for d in domains:
            if d==self.domains[0]: yield self._prod_wrfinput
            if d==self.domains[1]: yield self._prod_wrfanl_d02
            if d==self.domains[2]: yield self._prod_wrfanl_d03

    def run(self):
        """!Runs the merge."""
        logger=self.log()
        try:
            if os.path.exists(self.dest_dir):
                shutil.rmtree(self.dest_dir)
            produtil.fileop.makedirs(self.dest_dir)
            with NamedDir(self.dest_dir,keep=not self.scrub,
                          logger=self.log()) as dir:
                self.postmsg('Merge running in directory: '+os.getcwd())
                assert(not re.match('\A/tmp',os.getcwd()))

                produtil.fileop.makedirs(self.dest_dir+"/logs")

                self.copy_inputs()
                if self.conf.getbool('config','blend_innercore') \
                    and self.storminfo.wmax > self.conffloat('blend_wmax',0.0):
                    blended=self.blend_gsi()
                    if blended:
                        self.postmsg('Ran GSI blending.')
                    else:
                        self.postmsg('Skipped GSI blending.')
                self.check_storm_radius()
                self.relocate_storm()
                if self._gsi_d03 is not None:
                    self.inter_2to1(3)
                produtil.fileop.remove_file('flag_file',logger=logger)
                if self._gsi_d02 is not None:
                    if self._gsi_d03 is not None:
                        self.inter_3to2()
                    else:
                        self.inter_2to3()
                    self.inter_2to1ges(2)
                    self.inter_2to1(2)
                else:
                    self.inter_2to2()
                if self._gsi_d02 is not None or self._gsi_d03 is not None:
                    self.inter_2to6()
                else:
                    logger.warning('Not running inter_2to6 because GSI is '
                                   'disabled for domains 2 & 3')

                self.update_3dvar()
                self.deliver_products()
                self.postmsg('Merge running in directory: '+os.getcwd())
        except Exception as e:
            logger.critical('Merge failed: '+str(e),exc_info=True)
            raise

    def _make_plist_and_names(self):
        """!Internal function to generate input product lists and names.

        This is an internal implementation function that should not be
        called directly.  It returns a three-element tuple containing
        a list of products, and a dict mapping from product to the
        local filename, and a dict mapping from product to the copy
        method.  This is used to implement copy_inputs, to copy input
        files to the local directory from remote tasks' Products.

        This overrides the superclass _make_plist_and_names to add the
        guess and wrfghost products."""
        logger=self.log()
        def copier(p,name,logger,*args):
            deliver_file(p.location,name,logger=logger,keep=True)
        def linker(p,name,logger,*args):
            make_symlink(p.location,name,force=True,logger=logger)
        names=dict()

        names[self._wrfinput]='wrfinput_d01'
        names[self._wrfanl_d02]='wrfinput_d02'
        names[self._wrfanl_d03]='wrfinput_d03'
        if self._input_storm_radius is not None:
            names[self._input_storm_radius]='storm_radius'

        if self._ges_d02 is not None:
            names[self._ges_d02]='gsiges_d02'

        if self._ges_d03 is not None:
            names[self._ges_d03]='gsiges_d03'

        if self._gsi_d02_ghost is not None:
            # Use gsi output
            names[self._gsi_d02_ghost]='wrfghost_d02'
        else:
            # Use original ghost
            assert(False)
            names[self._ghost_d02]='wrfghost_d02'

        if self._gsi_d03_ghost is not None:
            # Use gsi output
            names[self._gsi_d03_ghost]='wrfghost_d03'

        plist=[ k for k in names.iterkeys() ]
        actions=dict( (n,copier) for n in names.iterkeys() )
        return ( plist, names, actions )

    def relocate_storm(self):
        """!Runs the hwrf_diffwrf_3dvar for all domains."""
        logger=self.log()
        logger.info('relocate storm')
        fprog = 'hwrf_3dvar'
        icom_dir = self.conf.getdir('intercom')
        prog = self.getexe('hwrf_3dvar')

        for id in range(2, 4):

            fin = "gsiges_d%02d" %id
            fou = "new_ges_d%02d"  %id

            if not isnonempty(fou):
                log = '%s/logs/%s_%s_gsiges_d%02d.log' %(
                    self.dest_dir, self.__class__.__name__, fprog, id)
                cmd = produtil.run.exe(prog)['storm_relocate', fin, 'flnm3', fou]
                if self.redirect: cmd = cmd >= log
                produtil.run.checkrun(cmd,logger=logger)

                if os.path.isfile('fort.50'):
                    os.remove('fort.50')

                if os.path.isfile('fort.73'):
                    os.remove('fort.73')

        for d in self.domains:
            id = d.get_grid_id()

            fin = "wrfinput_d%02d" %id
            fou = "new_gfs_d%02d"  %id
            if id == 1:
                fou = 'new_hdas_d01'

            log = '%s/logs/%s_%s_d%02d.log' %(
                self.dest_dir, self.__class__.__name__, fprog, id)
            cmd = produtil.run.exe(prog)['storm_relocate', fin, 'flnm3', fou]
            if self.redirect: cmd = cmd >= log
            produtil.run.checkrun(cmd,logger=logger)

            if os.path.isfile('fort.50'):
                os.remove('fort.50')

            if os.path.isfile('fort.73'):
                os.remove('fort.73')

        for id in range(2, 4):

            fin = "wrfghost_d%02d" %id
            fou = "new_ght_d%02d"  %id

            log = '%s/logs/%s_%s_ghost_d%02d.log' %(
                self.dest_dir, self.__class__.__name__, fprog, id)
            cmd = produtil.run.exe(prog)['storm_relocate', fin, 'flnm3', fou]
            if self.redirect: cmd = cmd >= log
            produtil.run.checkrun(cmd,logger=logger)

            if os.path.isfile('fort.50'):
                os.remove('fort.50')

            if os.path.isfile('fort.73'):
                os.remove('fort.73')

    def inter_2to1(self, domain):
        """!Runs the hwrf_inter_2to1 Fortran program to interpolate fields."""
        self.log().info('inter_2to1')
        fprog = 'hwrf_inter_2to1'
        evars = [ 6,
                  1,
                ]
        ins   = { 26:'new_ght_d%02d' %domain,
                  36:'new_gfs_d%02d' %domain,
                  85:'storm_radius',
                }
        ous   = { 56:'data_merge_d%02d' %domain }
        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)

    def inter_2to1ges(self,domain):
        """!Runs the hwrf_inter_2to1 Fortran program to interpolate fields."""
        self.log().info('inter_2to1ges')
        logger=self.log()
        fprog = 'hwrf_inter_2to1'
        evars = [ 6,
                  0,
                ]
        ins   = { 26:'new_ges_d%02d' %domain,
                  36:'new_gfs_d%02d' %domain,
                  85:'storm_radius',
                }
        ous   = { 56:'ges_merge_d%02d' %domain }
        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)
        deliver_file('wrfinput_d02', 'wrfges_d02', keep=True)

        fprog = 'hwrf_3dvar'
        prog = self.getexe(fprog)

        ifile = 'ges_merge_d02'
        ofile = 'wrfges_d02'
        cmd = produtil.run.exe(prog)['3dvar_update', ofile, ifile]
        if self.redirect: cmd = cmd >= log
        produtil.run.checkrun(cmd,logger=logger)

    def inter_2to2(self):
        """!Runs the hwrf_inter_2to2 Fortran program to interpolate fields."""
        self.log().info('inter_2to2')
        fprog = 'hwrf_inter_2to2'
        evars = [ 6,
                  1,
                ]
        ins   = { 26:'new_ght_d03',
                  36:'new_gfs_d02',
                  46:'new_hdas_d01',
                }
        ous   = { 56:'data_merge_d02' }
        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)

    def inter_2to3(self):
        """!Interpolates gsi_d02 analysis increment to d03 and
        adds the increment to d03 first guess"""
        self.log().info('inter_2to3')
        logger=self.log()

        fprog = 'hwrf_inter_2to2'
        evars = [ 6,
                  2,
                ]
        ins   = { 21:'new_ges_d02',
                  26:'new_ght_d02',
                  36:'new_ges_d03',
                  46:'new_hdas_d01',
                }
        ous   = { 56:'data_merge_d03' }
        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)

    def inter_2to6(self):
        """!Runs the hwrf_inter_2to6 Fortran program to interpolate fields."""
        self.log().info('inter_2to6')
        fprog = 'hwrf_inter_2to6'
        evars = [ 6,
                  1,
                  1,
                ]
        ins   = { 26:'new_gfs_d02',
                  46:'new_hdas_d01',
                  85:'storm_radius',
                }
        ous   = { 56:'data_merge_d01' }
        if self._gsi_d02 is not None:
            ins[36] = 'new_ght_d02'
        else:
            ins[36] = 'new_ght_d03'
        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)

    def inter_3to2(self):
        """!Runs the hwrf_inter_3to2 Fortran program to interpolate fields."""
        self.log().info('inter_3to2')
        fprog = 'hwrf_inter_2to6'
        evars = [ 6,
                  1,
                  2,
                ]
        ins   = { 26:'new_gfs_d03',
                  36:'new_ght_d03',
                  46:'new_ght_d02',
                  85:'storm_radius',
                }
        ous   = { 56:'data_merge_g02' }
        self.run_ext(fprog, echo=evars, inputs=ins, outputs=ous)
        deliver_file('data_merge_g02', 'new_ght_d02', keep=True)
        deliver_file('wrfghost_d02', 'newghost_d02', keep=True)

        fprog = 'hwrf_3dvar'
        prog = self.getexe(fprog)

        logger=self.log()
        ifile = 'new_ght_d02'
        ofile = 'newghost_d02'
        cmd = produtil.run.exe(prog)['3dvar_update', ofile, ifile]
        if self.redirect: cmd = cmd >= log
        produtil.run.checkrun(cmd,logger=logger)

    def update_3dvar(self):
        """!Runs the hwrf_diffwrf_3dvar program to update the output
        domains."""
        self.log().info('update_3dvar')
        logger=self.log()
        fprog = 'hwrf_3dvar'
        prog = self.getexe(fprog)

        for d in self.domains:
            if d==self.domains[0] and self._gsi_d02 is None \
                    and self._gsi_d03 is None:
                logger.warning(
                    'Not updating MOAD: GSI is disabled for domains 2 & 3')
                continue
            id = d.get_grid_id()

            ifile = 'data_merge_d%02d' %id
            ofile = 'wrfinput_d%02d'   %id

            log = '%s/logs/%s_%s_d%02d.log' %(
                self.dest_dir, self.__class__.__name__, fprog, id)
            cmd = produtil.run.exe(prog)['3dvar_update', ofile, ifile]
            if self.redirect: cmd = cmd >= log
            produtil.run.checkrun(cmd,logger=logger)

