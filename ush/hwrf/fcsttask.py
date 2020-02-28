"""!Runs the real_nmm or wrf executables.

This module, contrary to its name, implements HWRFTask classes to
run the real_nmm and WRF, including tasks to make the the wrfanl and
ghost files.  The uncoupled forecast is found in the WRFAtmos class.
The coupled forecast is not in this module: it is hwrf.mpipomtc
instead.

All classes you will want to use derive from WRFTaskBase and FcstTask,
classes that exist solely to simplify later code.  These are the
classes you will want to use:

* RealNMM - to run the real_nmm program
* WRFAnl, WRFAnl4Trak - 1 minute simulation to generate wrfanl files
* WRFGhost, WRFGhostForPost - 1 minute simulation to generate wrfghost files
* WRFAtmos - full-length uncoupled forecast
* AnalysisCycle - short (1-6 hour) forecast that outputs wrfinput and wrfghost files, suitable for input to another AnalysisCycle
* hwrf.mpipomtc.WRFCoupledPOM - coupled WRF+POM forecast, in the hwrf.mpipomtc module

All of these classes take input from one another, or from other
sources.  The abstract base class FcstTask keeps track of those
through its FcstTask.add_input() function.  The WRFTaskBase class
provides simple functions to call add_input for you.  Those inputs are
stored in objects of class Input2Fcst and its subclasses:

* Geog2WRF (WRFTaskBase.add_geogrid()) - geogrid data from hwrf.wps.Geogrid
* Met2WRF (WRFTaskBase.add_metgrid()) - metgrid data from hwrf.wps.Metgrid
* WRFInput2WRF (WRFTaskBase.add_wrfinput()) - wrfinput data from RealNMM, AnalysisCycle, hwrf.relocate.Merge or hwrf.ensda.FromPriorCycle
* Fort652WRF (WRFTaskBase.add_fort65()) - coupler fort.65 file from RealNMM
* WRFAnl2WRF (WRFTaskBase.add_wrfanl()) - wrf analysis (time 0 restart) files from WRFAnl, WRFAnl4Trak, WRFGhost, WRFGhostForPost, AnalysisCycle, hwrf.relocate.Merge or hwrf.relocate.RelocationTask (and its subclasses)
* WRFBdy2WRF (WRFTaskBase.add_wrfbdy()) - wrf boundary (wrfbdy) files from RealNMM
* Prep2WRF (WRFTaskBase.add_prep_hybrid()) - prep_hybrid files from hwrf.prep.PrepHybrid
"""

## @var __all__
# List of variables imported by "from hwrf.fcsttask import *"
__all__=['FcstTask']

import os, collections, glob, re,math, fractions, shutil, sys, time,logging
import produtil.cd, produtil.fileop, produtil.datastore, produtil.run
import produtil.prog
import hwrf.hwrftask, hwrf.exceptions, hwrf.numerics
import hwrf.wrf, hwrf.numerics, hwrf.wrfbase

from produtil.cd import NamedDir
from produtil.fileop import realcwd, deliver_file, make_symlink, \
    check_last_lines, isnonempty, remove_file
from produtil.datastore import FileProduct, UpstreamFile, UNSTARTED
from produtil.run import checkrun, mpi, mpirun, bigexe, ExitStatusException,\
    run, batchexe
from hwrf.exceptions import WRFGeogridMissing, WRFMetgridMissing, \
    WRFPrepMissing, WRFInputMissing, WRFBdyMissing, WRFAnlMissing, \
    ForecastInputError, SetNestFailed, RealNMMError
from hwrf.numerics import to_fraction, within_dt_epsilon, to_datetime_rel, \
    to_timedelta
from hwrf.wrf import ExternalWRFTask

########################################################################

class Input2Fcst(object):
    """!abstract base class of wrf/real_nmm input providers

    This is the abstract base class of anything that gets, or creates,
    input files for a WRF simulation without running another Task.
    For example, something that copies the geogrid output would be a
    subclass of Input2Fcst"""
    def __init__(self,src): 
        """!creates a new Input2Fcst with the specified src.  

        The src is implementation-dependent and can be interpreted as
        desired by subclasses.  If no "src" is needed, None is
        acceptable."""
        self.src=src
    def get_inputs(self,just_check=False,**kwargs): 
        """!copies or links input files.
        
        This function is unimplemented: subclasses are expected to
        replace it.  If just_check=True, checks to see if inputs are
        available, returning True if they are and False otherwise.  If
        just_check=False, then the files are actually copied or linked.

        This default implementation does nothing and returns True.

        @return True on success, False otherwise"""
        return True
    def link_product(self,product,excclass,logger,target=None,
                     just_check=False):
        """!helper function that links data

        If just_check=True, checks to see if data is available,
        returning True if it is, and False otherwise.  If just_check
        is False, then the file is linked from the given product to
        the target location (basename(product.location) if no target
        is provided).  If the product is not yet available or has no
        location, then the given exception class excclass is raised.

        @return True on success, False otherwise
        @param product     the produtil.datastore.Product to link
        @param excclass    the class of Exception to raise on error
        @param logger      the logging.Logger to use for logging
        @param just_check  if True, just check for data, but link nothing
        @param target      the name of the link
        """
        if logger is None:
            logger=logging.getLogger('hwrf.fcsttask')
        assert(isinstance(product,produtil.datastore.Product))
        (L,A)=(product.location,product.available)
        if not A:
            product.check()
            (L,A)=(product.location,product.available)
        if L and A:
            if not target:
                target=product.meta('basename','')
                if not target:
                    target=os.path.basename(L)
            if just_check:
                if isnonempty(L):
                    return True
                elif logger is not None:
                    msg='%s: file is empty or non-existent'%(L,)
                    logger.warning(msg)
                return False
            make_symlink(L, target, logger=logger,force=True)
            return True
        msg='%s: unavailable (available=%s location=%s)'%(
            str(product.did),repr(A),repr(L))
        if logger is not None: logger.warning(msg)
        if just_check: return False
        raise excclass(msg)
    ##@var src
    #    the implementation-defined source, used by subclasses
    #    @protected

########################################################################
class Geog2WRF(Input2Fcst):
    """!Links Geogrid data to the current directory for a wrf or
    real_nmm run.

    This class links geogrid data to the current directory for a wrf
    or real_nmm run, taken from some source (src) object sent to
    __init__.  That src must have a geodat(domain) function that can
    provide a Product with geogrid output data (such as
    hwrf.wps.Geogrid.geodat())"""
    def get_inputs(self,logger,domain,just_check=False,**kwargs):
        """!Links geogrid data if any is available for the specified domain.

        If just_check=True, checks for geogrid input data.  Otherwise,
        links the data using Input2Fcst.link_product.  Raises
        WRFGeogridMissing if the data is missing."""
        if self.src is not None and domain is not None:
            p=self.src.geodat(domain)
            if p:
                if not p.available:      p.check()
                return self.link_product(p,WRFGeogridMissing,logger,
                                           just_check=just_check)
            return False

########################################################################
class Met2WRF(Input2Fcst):
    """!Links Metgrid data to the current directory for a wrf or
    real_nmm run.

    This class links metgrid.exe output data to the current directory
    for a wrf or real_nmm execution.  The output data is taken from
    some source object, sent to the src argument of __init__.  That
    object must have a met_at_time(ftime) function that can provide a
    Product for metgrid data at a specific forecast time (such as
    hwrf.wps.Metgrid.met_at_time())"""
    def get_inputs(self,logger,ftime,just_check=False,**kwargs):
        """!Links metgrid data if any is available for the specified time.

        If just_check=True, checks for metgrid input data.  Otherwise,
        links the data using Input2Fcst.link_product.  Raises
        WRFMetgridMissing if the data is missing."""
        if self.src is not None and ftime is not None:
            p=self.src.met_at_time(ftime)
            if p:
                if not p.available:      p.check()
                return self.link_product(p,WRFMetgridMissing,logger,
                                           just_check=just_check)
            return False

########################################################################
class WRFInput2WRF(Input2Fcst):
    """!Links real_nmm wrfinput_d01 files the current directory for a
    wrf run.

    This class links real_nmm wrfinput_d01 files the current directory
    for a wrf run.  It gets those files from a source object specified
    in the src argument to __init__.  The src must have a
    wrfinput_at_time(ftime) function that can provide a Product for
    metgrid data at a specific forecast time (such as
    hwrf.fcsttask.RealNMM.wrfinput_at_time())."""
    def get_inputs(self,logger,atime,domain,just_check=False,**kwargs):
        """!Links wrfinput_d01 files.

        If just_check=True, checks for wrfinput_d01 data.  Otherwise,
        links the data using Input2Fcst.link_product.  Raises
        WRFInputMissing if the data is missing."""
        if self.src is not None and atime is not None and domain is not None:
            p=self.src.wrfinput_at_time(atime,domain)
            if p:
                if not p.available:      p.check()
                return self.link_product(p,WRFInputMissing,logger,
                                         target='wrfinput_d01',
                                         just_check=just_check)
            return False

########################################################################
class Fort652WRF(Input2Fcst):
    """!Links real_nmm fort.65 files the current directory for a wrf run.

    This class links real_nmm fort.65 files the current directory for
    a wrf run.  The files are taken from some source object, specified
    in the src argument to __init__.  The src must have a
    fort65_at_time(atime,domain) function that provides a Product for
    the fort.65 file.  See hwrf.fcsttask.RealNMM.fort65_at_time() for
    an example."""
    def get_inputs(self,logger,atime,domain,just_check=False,**kwargs):
        """!Links coupler input data for the specified domain and time.

        If just_check=True, checks for a coupler "fort.65" input
        product.  Otherwise, links the data using
        Input2Fcst.link_product to the filename "fort.65" in the
        current working directory.  Raises WRFInputMissing if the data
        is missing.
        @param atime    the analysis time
        @param domain   the wrf domain (a number, string name or WRFDomain)
        @param logger   a logging.Logger for logging messages
        @param just_check  if True, just check for data, otherwise link it
        @param kwargs   ignored"""
        if self.src is not None and atime is not None and domain is not None:
            p=self.src.fort65_at_time(atime,domain)
            if p: 
                if not p.available:      p.check()
                return self.link_product(p,WRFInputMissing,logger,
                                         target='fort.65',
                                         just_check=just_check)
            return False

########################################################################
class WRFAnl2WRF(Input2Fcst):
    """!Links wrfanl or ghost files the current directory for a wrf run.

    This class links wrfanl or ghost files the current directory for a
    wrf run.  The files come from some source object, sent to the src
    argument of __init__.  That object must have a
    wrfanl_at_time(atime,domain) function like
    hwrf.fcsttask.WRFAnl.wrfanl_at_time()"""
    def __init__(self,src,domain):
        """!creates a WRFAnl2WRF for the specified source and domain"""
        super(WRFAnl2WRF,self).__init__(src)
        self.domain=domain
    def get_inputs(self,logger,atime,domain,wrfanl='wrfanl',
                   just_check=False,**kwargs):
        """!Links wrfanl files for the specified domain and time.

        If just_check=True, checks to see if there should be wrfanl
        files for the specified domain and time.  Otherwise, links the
        data using Input2Fcst.link_product to the filename "fort.65"
        in the current working directory.  Raises WRFAnlMissing if the
        data is missing.
        @param atime    the analysis time
        @param domain   the wrf domain (a number, string name or WRFDomain)
        @param logger   a logging.Logger for logging messages
        @param wrfanl   the beginning of the link filename.  Usually this is
                        "wrfanl", "wrfghost" or "ghost".
        @param just_check  if True, just check for data, otherwise link it
        @param kwargs   ignored"""
        if self.src is not None and atime is not None and domain is not None\
                and domain==self.domain: # yay for run-on sentences!
            p=self.src.wrfanl_at_time(atime,domain)
            if p:
                if not p.available:      p.check()
                localname=hwrf.wrfbase.parse_wrf_outname(
                    str(wrfanl)+'_d<domain>_<date>',domain.get_grid_id(),
                    atime,domain.nocolons)
                return self.link_product(
                    p,WRFAnlMissing,logger,target=localname,
                    just_check=just_check)
            return False
    ## @var domain 
    # the domain for which this object provides wrfanl files, or None
    # if it provides wrfanl files for all domains except the MOAD
    # @protected

########################################################################
class WRFBdy2WRF(Input2Fcst):
    """!Links real_nmm wrfbdy_d01 files the current directory for a wrf
    run.

    This class links real_nmm wrfbdy_d01 files the current directory
    for a wrf run.  Those files are taken from some source object
    specified in the src argument to __init__.  The source must have a
    wrfbdy_at_time(atime,domain) function like
    hwrf.fcsttask.RealNMM.wrfbdy_at_time() """
    def get_inputs(self,logger,atime,domain,just_check=False,**kwargs):
        """!Links a wrfbdy file for the specified domain and time.

        If just_check=True, checks for a wrfbdy input product.
        Otherwise, links the data using Input2Fcst.link_product to the
        filename "fort.65" in the current working directory.  Raises
        WRFBdyMissing if the data is missing.
        @param atime    the analysis time
        @param domain   the wrf domain (a number, string name or WRFDomain)
        @param logger   a logging.Logger for logging messages
        @param just_check  if True, just check for data, otherwise link it
        @param kwargs   ignorex"""
        if self.src is not None and atime is not None and domain is not None:
            p=self.src.wrfbdy_at_time(atime,domain)
            if p:
                if not p.available:      p.check()
                return self.link_product(p,WRFBdyMissing,logger,
                                         target='wrfbdy_d01',
                                         just_check=just_check)
            return False
########################################################################
class Prep2WRF(Input2Fcst):
    """!Links prep_hybrid files to the current directory for a real_nmm
    run.

    This class links prep_hybrid files to the current directory for a
    real_nmm run.  The files must come from some source object,
    specified in the src argument to __init__.  That object must have
    a products() function that behaves like
    hwrf.prep.PrepHybrid.products() when called with a time= and name=
    argument."""
    def get_inputs(self,logger,times,just_check=False,**kwargs):
        """!Links prep_hybrid output files for the specified time index.

        If just_check=True, checks for the input product's
        availability.  Otherwise, links the data using
        Input2Fcst.link_product to the filename "fort.65" in the
        current working directory.  Raises WRFPrepMissing if the data
        is missing.
        @param times    a list of integers, 0 for the initial time, 1 for the first boundary input time, and so on.
        @param logger   a logging.Logger for logging messages
        @param just_check  if True, just check for data, otherwise link it
        @param kwargs   ignored"""
        if self.src is not None and times:
            for i in xrange(len(times)):
                logger.info('Look for prep at time '+str(i))
                t=times[i]
                what = 'init' if(i==0) else 'bdy'
                prod=[p for p in self.src.products(time=t,name=what)]
                if not prod:
                    if just_check: return False
                    raise WRFPrepMissing('No prep %s data for t=%s'%(
                            what,t.strftime('%Y%m%d-%H%M%S')))
                prod=prod[0]
                if not prod.available:      prod.check()
                if not self.link_product(prod,WRFPrepMissing,logger,
                                         'fort.%03d'%(i+900),
                                         just_check=just_check):
                    if just_check: return False
        else:
            logger.warning(
                'When looking for prep data, src is none or times is false:'
                'src=%s times=%s'%(repr(src),repr(times)))
        if just_check: return False
        return True

########################################################################
class FcstTask(hwrf.hwrftask.HWRFTask):
    """!abstract base class of anything that runs or prepares input
    for a forecast model

    Abstract base class of anything that runs a forecast model, or
    prepares input to a forecast model.  This should not be
    instantiated directly.  It exists just to simplify code in
    subclasses."""
    def __init__(self,dstore,conf,section,outdir=None,taskname=None,
                 **kwargs):
        """!Creates a FcstTask object.

        Creates a new FcstTask object.  All parameters are passed to the 
        superclass constructor, hwrf.hwrftask.HWRFTask.__init__()"""
        assert(taskname is not None)
        if taskname is None: taskname=section
        super(FcstTask,self).__init__(dstore,conf,section,taskname=taskname,
                                      outdir=outdir,**kwargs)
        self.inputs=collections.defaultdict(list)
    def has_input(self,typename):
        """!is there any input data of this type to this task?

        Returns True if there is at least one input source for the
        specified input type.  That is, if someone called
        add_input(typename,(something)).  Returns False otherwise.

        @param typename   the typename value that was sent to add_input
        @returns True if add_input was called with that typename, or
        False otherwise."""
        return self.inputs.has_key(typename)
    def add_input(self,typename,inobj):
        """!add input of a specified type

        Adds an input of the given type typename that should be
        provided by the given object.  The object should be a subclass
        of Input2Fcst.
        @param typename  the type of input
        @param inobj     the Input2Fcst object that will provide the input"""
        self.inputs[typename].append(inobj)
        return self
    def check_input(self,typenames,**kwargs):
        """!check if input data is available

        Checks all inputs of the given typenames to make sure
        link_input would work if called with the same parameters.
        Returns True if link_input should succeed, and False if it
        would fail.  This is a simple wrapper around link_input with
        just_check=True.  However, subclasses may override this to
        perform additional checks, such as for a coupled model.

        @param typenames a list of typenames that were sent to add_input
        @return True if all inputs are available, and False otherwise
        @param kwargs passed to link_input()"""
        return self.link_input(typenames,just_check=True,**kwargs)
    def link_input(self,typenames,just_check=False,**kwargs):
        """!link or check for inputs

        Links all inputs of types given in typenames (an iterable) by
        calling obj.get_inputs on anything sent to self.add_input.  If
        multiple input sources are available for a given input type,
        then only the first one that has input is used.

        Do not use the just_check option: it is part of the internal
        implementation of check_input; if you need to just check the
        inputs, use check_input instead.  If just_check=True, then
        nothing is linked.  Instead, the routine just checks to see if
        the inputs are available.  That is the same as calling
        check_input.  However, subclasses may override check_input to
        check additional inputs as part of a coupled model.

        @param typenames a list of typenames to check, as sent to
          add_inputs()
        @param kwargs passed to Input2Fcst.get_inputs()
        @param just_check if True, just check for data, do not link.
          Do not use this argument - it is part of the internal
          implementation of this class.  If you want to check for
          inputs, call check_input() instead, as subclasses may
          override that function to provide additional checks."""
        if isinstance(typenames,basestring): typenames=( typenames, )
        logger=self.log()
        for typename in typenames:
            logger.info('Look for input of type %s with kwargs=%s'
                        %(typename,repr(kwargs)))
            if typename in self.inputs:
                thelist=self.inputs[typename]
                found=False
                itry=0
                for inputter in thelist:
                    logger.info('Check %s for input of type %s'
                                %(inputter,typename))
                    itry+=1
                    try:
                        found=(inputter.get_inputs(
                                logger,just_check=just_check,**kwargs)
                               is True )
                        if found:
                            logger.info(
                                'Found input type %s in inputter #%d (%s)'
                                %(repr(typename),itry,repr(inputter)))
                            break
                        else:
                            logger.warning(
                                'Could not get input type %s in inputter'
                                ' #%d (%s)'
                                %(repr(typename),itry,repr(inputter)))
                    except (ForecastInputError,KeyError) as e:
                        logger.warning(
                            'cannot get %s files due to exception: %s'
                            %(typename,str(e)),exc_info=True)
                    if found: break
                if not found:
                    msg='%s: could not find input files of this type.  '\
                        'Giving up.'%(typename,)
                    if just_check:
                        logger.warning(msg)
                        return False
                    else:
                        logger.error(msg)
                        raise ForecastInputError(msg)
        return True
    ##@var inputs
    # a mapping of typename to a list of input objects
    # @protected

########################################################################
class WRFTaskBase(FcstTask):
    """!base class of classes that run wrf or real_nmm

    This is the abstract base class of tasks that run real or WRF.
    The purpose of this class is simply to reduce code duplication."""
    def __init__(self,dstore,conf,section,wrf,keeprun=True,**kwargs):
        """!constructor

        Creates a WRFTaskBase for the specified datastore, conf and
        section.  The wrf argument is a WRFSimulation.  It will not be
        used directly: instead the wrf is copied, and the copy is
        used.  If keeprun=True, then the output of the simulation,
        after running it, will not be scrubbed.  Other keyword
        arguments are passed to the superclass FcstTask.

        @param dstore the produtil.datastore.Datastore to use
        @param conf the hwrf.config.HWRFConfig that provides configuration ifnormation
        @param section the config section in conf
        @param wrf the hwrf.wrf.WRFSimulation object that is being run
        @param keeprun if True, the simulation temporary files are not deleted
        @param kwargs passed unmodified to FcstTask.__init__()"""
        super(WRFTaskBase,self).__init__(dstore,conf,section,**kwargs)
        self.__wrf=self.make_wrf(wrf)
        self.make_products()
        self.dt_epsilon=wrf.bdyepsilon()
        self.__run_exe_callbacks=list()
        self.keeprun=keeprun
    def _set_wrf_proc_config(self,wrf,logger=None):
        """!sets nproc_x, nproc_y and I/O server settings

        This is a protected member function meant to be used by the
        make_wrf implementation in subclasses.  This class does not
        use it.  It sets the WRF nest_pes_x and nest_pes_y, and sets I/O
        server settings if desired based on config options:

        @param nest_pes_x,nest_pes_y compute grid dimensions per domain
        @param nproc_x,nproc_y compute grid dimensions
        @param nio_groups      number of I/O server groups
        @param nio_tasks_per_group  number of servers per group
        @param poll_servers set to True to poll I/O servers.  This
            generally decreases the number of I/O server groups needed
            for a given WRF run.
        """
        if logger is None: logger=self.log()
        assert(wrf is not None)
        nio_groups=max(0,self.confint('nio_groups',1))

        # Multistorm - jtf
        # nio_tasks_per_group may now specify values for each domain.
        # One value for all domains, as before, or a value for each domain.
        # ie. 4 or 4,4,2,4,2... or 12,12,8,8 (that is, double or n digits)
        nio_tasks_per_group=self.confstr('nio_tasks_per_group','0').strip().strip(',').strip()
        if ',' in nio_tasks_per_group:
            nio_tpg_split=nio_tasks_per_group.split(',') #['4','2','4',...]
        else: #in case space seperated.
            nio_tpg_split=nio_tasks_per_group.split() #['4','2','4',...]
            nio_tasks_per_group=','.join(nio_tpg_split) #'4,2,4, ...'

        total_nio_tpg=0
        if len(nio_tpg_split) > 1:
            for num in nio_tpg_split:
                total_nio_tpg+=int(num)
        else:
            nio_tasks_per_group=max(0,self.confint('nio_tasks_per_group',0))
            total_nio_tpg=nio_tasks_per_group

        poll_servers=self.confbool('poll_servers',True)

        if nio_groups*total_nio_tpg > 0:
            if len(nio_tpg_split) > 1:
                logger.debug(
                    'Using %d nio_groups. With a possible nio_tasks_per_group '
                    'domain scheme up to %s domains with poll_servers=%s'%(
                        nio_groups,repr(nio_tasks_per_group),repr(poll_servers)))
            else:
                logger.debug(
                    'Using %d groups of %d io tasks with poll_servers=%s'%(
                        nio_groups,nio_tasks_per_group,repr(poll_servers)))

            wrf.set_io_servers(nio_tasks_per_group,nio_groups,poll_servers)
        else:
            logger.debug('Not setting io_server settings.')

        # With the multistorm implementation the wrf executable added
        # a new namelist called dm_task_split which takes precedence
        # over  nproc_x and nproc_y, where  nest_pes_x and nest_pes_y
        # is a list of values, for each domain. ie. namelist.input example
        # &dm_task_split
        #   comm_start = 0,0,0,96,96,192,192,288,288,384,384,
        #   nest_pes_x = 12, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        #   nest_pes_y = 16, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,

        nproc_x=self.confint('nproc_x',-999)
        nproc_y=self.confint('nproc_y',-999)

        multi_sids=self.conf.getstr('config','multistorm_sids','-999').strip().strip(',').strip()
        comm_start=self.confstr('dm_task_split.comm_start','-999').strip().strip(',').strip()
        nest_pes_x=self.confstr('dm_task_split.nest_pes_x','-999').strip().strip(',').strip()
        nest_pes_y=self.confstr('dm_task_split.nest_pes_y','-999').strip().strip(',').strip()

        # Get the integer value of the d01 comm_start, nest_pes x and y and the number of storms 
        if nest_pes_x=='-999' and nest_pes_y=='-999' and comm_start=='-999':
            nest_pes_x_d01=int(nest_pes_x)
            nest_pes_y_d01=int(nest_pes_y)
            comm_start_d01=int(comm_start)
        else:
            if ',' in comm_start:
                comm_start_split=comm_start.split(',')
            else: #in case space seperated.
                comm_start_split=comm_start.split()
                comm_start=','.join(comm_start_split)
            comm_start_d01=int(comm_start_split[0])

            if ',' in nest_pes_x:
                nest_pes_x_split=nest_pes_x.split(',')
            else: #in case space seperated.
                nest_pes_x_split=nest_pes_x.split()
                nest_pes_x=','.join(nest_pes_x_split)
            nest_pes_x_d01=int(nest_pes_x_split[0])

            if ',' in nest_pes_y:
                nest_pes_y_split=nest_pes_y.split(',')
            else: #in case space seperated.
                nest_pes_y_split=nest_pes_y.split()
                nest_pes_y=','.join(nest_pes_y_split)
            nest_pes_y_d01=int(nest_pes_y_split[0])
          
        if multi_sids=='-999':
            storms=-1
        else:
            if ',' in multi_sids:
                storms=len(multi_sids.split(','))
            else: #in case space seperated.
                storms=len(multi_sids.split())

        # If comm_start, nest_pes_x and y are Not defined, that is -999, try nproc_x and nproc_y
        if (nest_pes_x_d01>0 and nest_pes_y_d01>0 and comm_start>=0) \
                or (nest_pes_x_d01==-1 and nest_pes_y_d01==-1 and comm_start_d01==-1):
            if storms >0 and nest_pes_x_d01>0 and nest_pes_y_d01>0:
                inest_pes_x=self.conf.getstr(
                    'wrf_%sstorm'%storms,'nest_pes_x','-999').strip().strip(',').strip()
                inest_pes_y=self.conf.getstr(
                    'wrf_%sstorm'%storms,'nest_pes_y','-999').strip().strip(',').strip()
                if inest_pes_x=='-999' and inest_pes_y=='-999':
                    logger.error("Trying to set nest_pes x and y for %s storms, "
                                 "but no '[wrf_%sstorm]' section in conf file. "
                                 "Will set dm_task_split values to -1, which will "
                                 "cause wrf to decide the task mesh ."%
                                 (repr(storms),repr(storms)))
                    comm_start_d01=-1
                    nest_pes_x_d01=-1
                    nest_pes_y_d01=-1 
                else:
                    nest_pes_x=inest_pes_x
                    nest_pes_y=inest_pes_y
                    if ',' in nest_pes_x:
                        nest_pes_x_split=nest_pes_x.split(',')
                    else: #in case space seperated.
                        nest_pes_x_split=nest_pes_x.split()
                        nest_pes_x=','.join(nest_pes_x_split)
                    nest_pes_x_d01=int(nest_pes_x_split[0])

                    if ',' in nest_pes_y:
                        nest_pes_y_split=nest_pes_y.split(',')
                    else: #in case space seperated.
                        nest_pes_y_split=nest_pes_y.split()
                        nest_pes_y=','.join(nest_pes_y_split)
                    nest_pes_y_d01=int(nest_pes_y_split[0])

            logger.debug('Setting dm_task_split comm_start=%s, nest_pes_x=%s, nest_pes_y=%s'%
                        (repr(comm_start),repr(nest_pes_x),repr(nest_pes_y)))
            wrf.set_dm_task_split(comm_start, nest_pes_x, nest_pes_y,
                                  comm_start_d01=comm_start_d01, nest_pes_x_d01=nest_pes_x_d01,
                                  nest_pes_y_d01=nest_pes_y_d01)
        else:
            if (nproc_x>0 and nproc_y>0) or (nproc_x==-1 and nproc_y==-1):
                logger.debug('No dm_task_split so Setting nproc_x=%d nproc_y=%d'%(nproc_x,nproc_y))
                wrf.set_nprocs(nproc_x,nproc_y)
            else:
                logger.debug('No dm_task_split and Not setting nproc_x or nproc_y (%s,%s)'%(
                        repr(nproc_x),repr(nproc_y)))
  

    def make_wrf(self,wrf): 
        """!creates a WRFSimulation object for this class

        Given the wrf object passed to the constructor, creates a new
        WRFSimulation that is modified for this object.  For example,
        this class may need to add output of a restart and wrfinput
        file at the end of the simulation, or enable hourly output of
        history files.
        @param wrf the wrf argument to __init__"""
        return wrf.copy()
    def add_geogrid(self,g):
        """!adds a geogrid input source

        Adds an input source (via self.add_input) that will provide
        the output of WPS geogrid.exe.  The given object must have a
        geodat member function which takes a WRFDomain as its argument
        and returns a Product to link.  Returns self.

        @param g the src argument to Geog2WRF.__init__()
        @returns self"""
        return self.add_input('geogrid',Geog2WRF(g))
    def add_metgrid(self,m):
        """!adds a metgrid input source

        Adds an input source (via self.add_input) that will provide
        the output of WPS metgrid.exe.  The given object must have a
        met_at_time function that returns a Product to link for a
        specified forecast time.  Returns self.

        @param m the src argument to Met2WRF.__init__()
        @returns self"""
        return self.add_input('metgrid',Met2WRF(m))
    def add_prep_hybrid(self,p):
        """!adds a prep_hybrid input source

        Adds an input source (via self.add_input) that will provide
        the output of the prep_hybrid program.  The given object must
        have a products function that iterates over products for a
        given name='bdy' or name='init' and a time=F for a given
        forecast time F.  Returns self.
        
        @param p the src argument to Prep2WRF.__init__()
        @returns self"""
        return self.add_input('prep',Prep2WRF(p))
    @property
    def use_prep_hybrid(self):
        """!returns True if prep_hybrid was requested, and False
        otherwise."""
        return self.has_input('prep')
    def add_wrfinput(self,r):
        """!adds a wrfinput_d01 input source

        Adds an input source (via self.add_input) that will provide
        the wrfinput output file from real_nmm.  The given object must
        have a wrfinput_at_time(atime,domain) function that returns a
        Product for a given analysis time and WRFDomain object.
        Returns self.

        @param r the src argument to WRFInput2WRF.__init__()
        @returns self"""
        return self.add_input('wrfinput',WRFInput2WRF(r))
    def add_wrfbdy(self,r):
        """!adds a wrfbdy_d01 input source

        Adds an input source (via self.add_input) that will provide
        the wrfbdy output of a real_nmm run.  The given object must
        have a wrfbdy_at_time(atime,domain) function that returns a
        Product to link for a specified analysis time and WRFDomain
        object.  Returns self.

        @param r the src argument to WRFBdy2WRF.__init__()
        @returns self"""
        return self.add_input('wrfbdy',WRFBdy2WRF(r))
    def add_fort65(self,r):
        """!adds a coupler fort.65 input source

        Adds an input source (via self.add_input) that will provide
        the fort.65 output from real_nmm. given object must have a
        fort65_at_time(atime,domain) function that returns a Product
        to link for a specified analysis time and WRFDomain object.
        Returns self.

        @param r the src argument to Fort652WRF.__init__ """
        return self.add_input('fort65',Fort652WRF(r))
    def add_real(self,r):
        """!add a fort.65, wrfinput_d01 and wrfbdy_d01 input source

        This is a convenience function that simply passes its argument
        to self.add_fort65, add_wrfinput and add_wrfbdy in that order.
        Returns self.

        @param r the src argument to Fort652WRF.__init__(),
            WRFBdy2WRF.__init__() and WRFInput2WRF.__init__()
        @return self"""
        self.add_fort65(r)
        self.add_wrfinput(r)
        return self.add_wrfbdy(r)
    def add_wrfanl(self,r,domain):
        """!add a wrfanl input source for a specific domain

        Adds an input source (via self.add_input) that will provide
        the wrfanl output file from a prior run of wrf.exe.  The given
        object must have a wrfanl_at_time function that returns a
        Product to link for a specified analysis time and domain.
        Returns self.

        @param r the src argument to WRFAnl2WRF.__init__()
        @param domain the domain for whom this is the wrfanl product
        @return self"""
        name='wrfanl-%s'%(domain.name,)
        return self.add_input(name,WRFAnl2WRF(r,domain))
    def make_products(self):
        """!called from constructor, creates Products

        This is called from the WRFTaskBase constructor.  Subclasses
        should re-implement this method to generate internal
        information about what products this class can provide, so
        that self.products() and self.deliver_products() will function
        properly.  The default implementation does nothing."""
    def link_fix(self):
        """!links or copies fixed data files

        Links or copies fix files needed by WRF.  Will copy if the
        link_wrf_fix=no in this task's config section.  Otherwise, the
        files are linked."""
        link_files=self.confbool('link_wrf_fix',True)
        act='link' if link_files else 'copy'
        for confitem in ('fix.eta_lookup', 'fix.track', 'fix.wrf_other'):
            pattern=self.confstr(confitem,'')
            logger=self.log()
            if not pattern: 
                logger.warning(
                    '%s: no conf entry for this fix file or set of fix files'
                    %(confitem,))
                continue
            logger.info('Will %s WRF fix files from %s to current directory.'
                        %(act,pattern))
            for src in glob.glob(pattern):
                tgt=re.sub('^hwrf_','',os.path.basename(src))
                if link_files or os.path.isdir(src):
                    make_symlink(src,tgt,logger=logger,force=True)
                else:
                    deliver_file(src,tgt,logger=logger)
    def check_all_inputs(self):
        """!Checks to see if all needed input is available."""
        return self.link_all_inputs(just_check=True)
    def link_all_inputs(self,just_check=False):
        """!Links all inputs provided by the various add_* member
        functions.  

        @param just_check if True, just check to see if data is
        available.  If False, actually copy."""
        use_prep = ('prep' in self.inputs)
        okay=True
        if use_prep:
            okay=okay and self.link_input(
                'prep',times=[t for t in self.__wrf.bdytimes()],
                just_check=just_check)
        for domain in self.__wrf:
            okay=okay and self.link_input(
                'geogrid',domain=domain,just_check=just_check)
        for ftime in self.__wrf.bdytimes():
            okay=okay and self.link_input(
                'metgrid',ftime=ftime,just_check=just_check)
            if not self.need_all_metgrid() or use_prep: break
        if 'wrfinput' in self.inputs:
            okay=okay and self.link_input(
                'wrfinput',domain=self.__wrf.get_moad(),
                atime=self.__wrf.simstart(),just_check=just_check)
        if 'wrfbdy' in self.inputs:
            okay=okay and self.link_input(
                'wrfbdy',domain=self.__wrf.get_moad(),
                atime=self.__wrf.simstart(),just_check=just_check)
        if 'fort65' in self.inputs:
            okay=okay and self.link_input(
                'fort65',domain=self.__wrf.get_moad(),
                atime=self.__wrf.simstart(),just_check=just_check)
        MOAD=self.__wrf.get_moad()
        for domain in self.__wrf:
            if domain!=MOAD:
                name='wrfanl-%s'%(domain.name,)
                if name in self.inputs:
                    okay=okay and self.link_input(
                        name,domain=domain,atime=self.__wrf.simstart(),
                        just_check=just_check)
        return okay
    def need_all_metgrid(self): 
        """!Returns True if all metgrid files are needed as input to
        this Task"""
        return False
    def run_exe_callback(self,callback):
        """!Adds a callable object exe=f(self,exe) to the list of calls to
        make in run_exe() before executing the command.

        @param callback a function f(task,exe,comp,ranks) that takes a
          Runner "exe" as input and returns a Runner as output.  The
          resulting Runner will be run.  The comp and ranks are a list
          of coupled components, and a dict mapping from component to
          the number of ranks for that component.  The special value 0
          for ranks means "use all ranks".
        """
        self.__run_exe_callbacks.append(callback)
    def call_run_exe_callbacks(self,exe,comp,ranks):
        """!Calls the callbacks provided to run_exe_callback, in the order they
        were provided, giving each the return value of the last.
        @param exe the Runner to execute
        @param comp a list of coupled component names
        @param ranks a dict mapping from component name to the number of ranks.
           The special value 0 for ranks means "use all ranks"
        @returns The Runner returned by the last callback function."""
        logger=self.log()
        logger.info('Calling run_exe callbacks.')
        assert(isinstance(exe,produtil.prog.Runner))
        for c in self.__run_exe_callbacks:
            logger.info('Calling callback %s on %s...'%(repr(c),repr(exe)))
            exe=c(self,exe,comp,ranks)
            assert(isinstance(exe,produtil.prog.Runner))
            logger.info('Result of callback is %s'%(repr(exe),))
        logger.info('Final result is %s'%(repr(exe),))
        return exe
    def run_exe(self,exename='wrf',not_allranks=False,runner=None,
                sleeptime=None):
        """!runs the executable

        Runs the executable this task is responsible for running.
        Determines if the program ran correctly.  The exename is the
        name of the argument in the [exe] section of the
        HWRFConfig.  Options:

        @param not_allranks By default (False) all ranks are used to
          run the executable.  Pass not_allranks=True to run on only
          one rank.

        @param runner pass a produtil.prog.Runner object if desired.
          This overrides any decision of what to run: the exename and
          not_allranks will be ignored, and whatever is supplied in
          runner is simply passed to produtil.run.run.

        @param sleeptime passed to produtil.run.run to determine how
          often to check the child process.  By default, the sleeptime
          option in this task's config section is used, or if that is
          absent, 30 seconds.

        @param exename if no runner is specified, self.getexe(exename)
          is called to get the executable path, and it is run as an
          MPI program.

        @note The call_run_exe_callbacks() will NOT be called if the
        runner argument is specified and non-null.        """
        if sleeptime is None:
            sleeptime=self.conffloat('sleeptime',30)
        logger=self.log()
        if runner is None:
            exe=self.getexe(exename)
            if not_allranks:
                runner=mpirun(mpi(exe)) # run in one rank
            else:
                runner=mpirun(mpi(exe),allranks=True)
            runner=self.call_run_exe_callbacks(runner,['wrf'],{'wrf':0})
        stat=run(runner,logger=logger,sleeptime=sleeptime)
        logger.info('%s: exit status %d'%(exename,stat))
        if not check_last_lines('rsl.out.0000','SUCCESS COMPLETE',
                                logger=logger):
            msg='%s: did not see SUCCESS COMPLETE in rsl.out.0000'%(exename,)
            logger.error(msg)
            raise RealNMMError(msg)
        else:
            logger.info('%s: SUCCESS COMPLETE in rsl.out.0000'%(exename,))
    def final_prerun(self): 
        """! last step before running executable

        Called by self.run() just before calling run_exe.  The
        default implementation does nothing.  This is intended to be
        overridden by subclasses."""
    def initial_prerun(self): 
        """!called immediately after cding to a scrub directory

        Called by self.run() after CDing to the new directory, but
        before doing anything else.  The default implementation does
        nothing.  This is intended to be overridden by subclasses."""
    def run(self):
        """!run the wrf or real_nmm

        Performs all work needed to run the program.  Creates the work
        directory, CD's to it, runs the initial_prerun(), link_fix(),
        link_all_inputs(), make_namelist(), final_prerun(), run_exe(),
        postrun() and deliver_products()."""
        logger=self.log()
        runhere=self.workdir
        if os.path.exists(runhere):
            logger.warning('%s: directory exists; will delete'%(runhere,))
            assert(not os.path.samefile(self.getdir('WORKhwrf'),runhere))
            shutil.rmtree(runhere)
        atime=self.__wrf.simstart()
        with NamedDir(runhere,keep=self.keeprun,logger=logger,
                      keep_on_error=True) as rundir:
            try:
                logger.info('%s running in directory %s'%(
                        self.taskname,realcwd()))
                self.location=runhere
                self.initial_prerun()
                self.link_fix()
                self.link_all_inputs()
                self.make_namelist()
                self.final_prerun()
                self.run_exe()
                self.postrun()
                self.deliver_products()
            except Exception as e:
                logger.critical('%s failed: %s'%(self.taskname,str(e)),
                                exc_info=True)
                raise
        self.postmsg('%s: completed'%(self.taskname,))
    def postrun(self): 
        """!called just after run_exe() succeeds.  

        Called by self.run() just after run_exe returns successfully.
        The default implementation does nothing; this is intended to
        be overridden in subclasses."""
    def deliver_products(self):
        """!delivers products to their destinations

        Called by self.run() after postrun(), just before CDing out of
        the work directory.  This should deliver products to their
        destinations.  The default implementation raises
        NotImplementedError.  This MUST be overridden in subclasses."""
        raise NotImplementedError('A WRFTaskBase subclass did not '
                                  'implement deliver_products')
    def make_namelist(self,filename='namelist.input',logger=None):
        """!generates the wrf namelist.input file

        Runs set_ij_start (swcorner_dynamic) to generate the i & j
        start locations for domain 2, then generates the namelist.
        @param filename the namelist.input filename
        @param logger the logging.Logger to log messages (optional)"""
        if logger is None: logger=self.log()
        domlat=self.conf.getfloat('config','domlat')
        domlon=self.conf.getfloat('config','domlon')
        s=self.wrf().swcorner_dynamic(self.getexe('swcorner_dynamic'),
                                      self.storminfo, domlat,domlon,logger)
        with open(filename,'wt') as nlin:
            nlin.write(s)
    def wrf(self): 
        """!Returns the WRFSimulation object used by this task."""
        return self.__wrf
    @property
    def sim(self): 
        """!Returns the WRFSimulation object used by this task.  

        This property has the same effect as self.wrf(), but this is a
        property instead.  Hence, you can type self.sim instead of
        self.wrf()"""
        return self.__wrf

    ## @var dt_epsilon
    #  epsilon value for time equality comparisons

    ## @var keeprun
    #  if False, temporary files are deleted, otherwise they're kept

########################################################################
class RealNMM(WRFTaskBase):
    """!a HWRFTask subclass that runs real_nmm

    This subclass of WRFTaskBase runs the real_nmm to generate inputs
    for the provided WRFSimulation."""
    def make_products(self):
        """!prepares products for deliver_products() and products()

        Generates produtil.datastore.FileProduct objects for the files
        that should be delivered in deliver_products() and listed by
        products()."""
        self.prod_wrfinput=FileProduct(self.dstore,'wrfinput_d01',
            self.taskname,location=os.path.join(self.outdir,'wrfinput_d01'))
        self.prod_wrfbdy=FileProduct(self.dstore,'wrfbdy_d01',self.taskname,
            location=os.path.join(self.outdir,'wrfbdy_d01'))
        self.prod_log=FileProduct(self.dstore,'rsl.out.0000',self.taskname,
            location=os.path.join(self.outdir,'rsl.out.0000'))
        self.prod_fort65=FileProduct(self.dstore,'fort.65',self.taskname,
            location=os.path.join(self.outdir,'fort.65'))
    def add_prep_hybrid(self,p):
        """!adds a prep_hybrid data source

        Modifies the simulation namelist generator so that it
        specifies use_prep_hybrid=T.  Calls
        WRFTaskBase.add_prep_hybrid(p) to add the specified object as
        a prep_hybrid data source.

        @param p the src parameter to Prep2WRF.__init__()
        @return self"""
        ret=super(RealNMM,self).add_prep_hybrid(p)
        self.sim.nl.nl_set('domains','use_prep_hybrid',True)
        return ret
    def make_wrf(self,wrf):
        """!creates a WRFSimulation object for this class

        Creates a copy of the specified WRFSimulation wrf.  The copy
        has a simulation length of one minute or one physics timestep,
        whichever is larger.  Also, it produces wrfanl files for all
        domains.
        @param wrf the wrf argument to __init__"""
        wrf=wrf.copy()
        wrf.nl.nl_set('domains','use_prep_hybrid',self.use_prep_hybrid)
        return wrf
    def need_all_metgrid(self): 
        """!returns True if all metgrid files are needed as input to
        this Task"""
        return True
    def initial_prerun(self):
        """!deletes the wrfinput_d01 and wrfbdy_d01."""
        logger=self.log()
        for f in ['wrfinput_d01','wrfbdy_d01']:
            try:
                remove_file(f,logger)
            except(EnvironmentError) as e:
                logger.warning('%s: cannot remove, but continuing anyway.  '
                               'Error was: %s'%(f,str(e)),exc_info=True)
    def run_exe(self,exename='real_nmm',sleeptime=30):
        """!runs the real_nmm program

        Tries several times to run the real_nmm.  This is a workaround
        for problems encountered on Zeus when running the experimental
        9:3:1 HWRF.  The real_nmm would exit for no apparent reason
        about 50% of the time.  Eventually that was tracked down to a
        memory error caused by NetCDF, which forced us to use intio
        for some files, NetCDF for others, and PNetCDF for yet more
        files.

        @param exename the name of the real_nmm executable in the
        configuration file "exe" section.
        @param sleeptime passed to produtil.run.run to determine how
          often to check the child process."""
        logger=self.log()
        logger.info('real section is '+repr(self.section))
        maxtries=self.confint('max_tries',-999)
        logger.info('real max_tries is '+repr(maxtries))
        maxtries=max(1,maxtries)
        logger.info('after max(1,...), real max_tries is '+str(maxtries))
        
        logger.info('Will try to run real %d times'%(maxtries,))
        for trynum in xrange(maxtries):
            try:
                super(RealNMM,self).run_exe(exename,'prep' in 
                                            self.inputs,sleeptime=sleeptime)
                logger.info('Real succeeded.  Hurrah!')
                break
            except(EnvironmentError,RealNMMError,ExitStatusException) as e:
                if(trynum+1<maxtries):
                    logger.warning(
                        'Real failed %d time(s).  Will retry after %d '
                        'second sleep.  Error: %s'%(
                            trynum+1,sleeptime,str(e)),exc_info=True)
                    time.sleep(sleeptime)
                    logger.info(
                        'Returned from sleeping.  Will now retry real.')
                else:
                    logger.error(
                        'Real failed %d time(s).  Aborting.  Error: %s'
                        %(maxtries,str(e)),exc_info=True)
                    raise
    def get_wrfinput(self):
        """!returns the wrfinput file regardless of the time or
        domain"""
        return self.prod_wrfinput
    def wrfinput_at_time(self,atime=None,domain=None):
        """!returns the wrfinput file for a specified time and domain
        @param atime the time
        @param domain the WRF domain (an integer, string name or WRFDomain)
        @return  the wrfinput file for the specified time and
        domain.  Returns None if that time and domain are not
        valid."""
        if domain and domain!=self.wrf().get_moad(): return None
        if atime is not None and  \
                within_dt_epsilon(atime,self.wrf().simstart(),self.
                                  dt_epsilon):
            return self.prod_wrfinput
        return None
    def wrfbdy_at_time(self,atime,domain=None):
        """!returns the wrfbdy file for a specified time and domain
        @param atime the time
        @param domain the WRF domain (an integer, string name or WRFDomain)
        @return  the wrfbdy file for the specified time and
        domain.  Returns None if that time and domain are not
        valid."""
        if domain and domain!=self.wrf().get_moad(): return None
        if atime is not None and  \
                within_dt_epsilon(atime,self.wrf().simstart(),
                                  self.dt_epsilon):
            return self.prod_wrfbdy
        return None
    def fort65_at_time(self,atime,domain=None):
        """!returns the coupler fort.65 file for a specified time and domain
        @param atime the time
        @param domain the WRF domain (an integer, string name or WRFDomain)
        @return  the fort.65 file for the specified time and
        domain.  Returns None if that time and domain are not
        valid."""
        if domain and domain!=self.wrf().get_moad(): return None
        if atime is not None and  \
                within_dt_epsilon(atime,self.wrf().simstart(),
                                  self.dt_epsilon):
            return self.prod_fort65
        return None
    def deliver_products(self):
        """!delivers products that were identified by make_products()"""
        produtil.fileop.makedirs(os.path.dirname(
                self.prod_wrfinput.location))
        self.prod_wrfinput.deliver(frominfo='wrfinput_d01',keep=False)
        self.prod_wrfbdy.deliver(frominfo='wrfbdy_d01',keep=False)
        self.prod_log.deliver(frominfo='rsl.out.0000',keep=False)
        self.prod_fort65.deliver(frominfo='fort.65',keep=False)
    def products(self):
        """!iterates over all products
        Iterates over all of this Task's products
        (produtil.datastore.FileProduct objects) created by
        make_products()"""
        yield self.prod_wrfinput
        yield self.prod_wrfbdy
        yield self.prod_log
        yield self.prod_fort65
    def make_namelist(self,filename='namelist.input',logger=None):
        """!Writes the namelist for real.  

        Writes the namelist.input file for a call to real_nmm.  This
        does the same as its parent class's implementation, except
        that the num_metgrid_levels is also overridden to match
        whatever WPS actually created (which may not match the
        original namelist).
        @param filename the name of the namelist.input file
        @param logger the logging.Logger object"""
        if logger is None: logger=self.log()
        exepath=self.getexe('hwrf_metgrid_levels')
        metfile=None
        for x in glob.glob('met_nmm.d01*.nc'):
            metfile=x
            self.wrf().set_metgrid_levels_from(exepath,metfile,logger)
            break
        if metfile is None:
            raise RealNMMError(
                'Could not find a met_nmm.d01*.nc file for running '
                'hwrf_metgrid_levels.')
        super(RealNMM,self).make_namelist(filename,logger)

    ## @var prod_wrfinput
    #  the wrfinput output product
    #  @protected

    ## @var prod_wrfbdy
    #  the wrfbdy output product
    #  @protected

    ## @var prod_log
    #  the rsl.out.0000 output file product
    #  @protected

    ## @var prod_fort65
    #  the output product for the coupler fort.65 input file
    #  @protected

########################################################################

## tricks wrf into outputting child domains' data
# This is used by WRFAnl.make_wrf() to modify the simulation end time in
# order to trick the WRF into outputting child domains' data.
anl_fudge_factor=fractions.Fraction(2,3)

########################################################################
class WRFAnl(WRFTaskBase):
    """!runs a short WRF simulation to generate wrfanl files"""
    def make_wrf(self,wrf):
        """!creates the WRFSimulation object (self.wrf())

        Creates a copy of the specified WRFSimulation wrf.  The copy
        has a simulation length of one minute or one physics timestep,
        whichever is larger.  Calls the _set_wrf_proc_config to set
        I/O server and compute grid dimensions based on this
        HWRFTask's config section.  Configures the WRF to produce
        wrfanl files for all domains.

        @param wrf the wrf argument to __init__
        @returns the new WRFSimulation"""
        wrf=wrf.copy()
        self._set_wrf_proc_config(wrf)

        # Determine the length of a physics timestep:
        MOAD=wrf.get_moad()
        endtime=to_fraction(MOAD.nl.nl_get('physics','nphs')) * MOAD.dt
        floored=max(60.0,float(math.floor(endtime*anl_fudge_factor)))
        assert(floored>0)
        # Set the simulation end time and history output frequency to
        # the physics timestep:
        wrf.analysis_out()
        wrf.set_timing(end=floored)
        assert(wrf.simstart()<wrf.simend())
        wrf.add_output('history',step=floored)
        return wrf
    @property
    def anltime(self):
        """!the time for which analysis files are generated."""
        return self.wrf().simstart()
    def make_products(self):
        """!creates FileProduct objects for all wrfanl files."""
        self._products=dict()
        MOAD=self.wrf().get_moad()
        for domain in self.wrf():
            if domain==MOAD: continue
            pname=self.wrf().analysis_name(domain)
            loc=os.path.join(self.outdir,pname)
            self._products[domain]=FileProduct(self.dstore,pname,
                self.taskname,location=loc)
    def get_wrfanl(self,domain):
        """!returns the wrfanl product for the specified domain
        @param domain the domain for whom the wrfanl file is requested""" 
        if domain is None: return None
        if not domain in self.sim: return None
        domain=self.sim[domain]
        if domain.is_moad(): return None
        return self._products[domain]
    def wrfanl_at_time(self,atime,domain):
        """!get a wrfanl file for a specified domain and analysis time

        Returns the wrfanl product for the specified domain and analysis
        time, if one exists, and otherwise, None.
        @param domain the domain (an integer, string name or WRFDomain)
        @param atime the analysis time """
        if atime!=self.anltime: return None
        if domain not in self.wrf(): return None
        d=self.wrf()[domain]
        if d.is_moad() :return None
        return self._products[d]
    def deliver_products(self):
        """!delivers products during a call to run()

        Delivers the products (FileProduct objects) that were
        identified by make_products."""
        logger=self.log()
        logger.info('%s: make directory'%(self.outdir,))
        first=True
        for domain in self.wrf():
            if domain not in self._products: continue # skip MOAD
            p=self._products[domain]
            if first:
                first=False
                produtil.fileop.makedirs(os.path.dirname(p.location))
            p.deliver(frominfo=os.path.basename(p.location),logger=logger,
                      keep=False)
    def products(self,domain=None):
        """!iterates over products
        Iterates over all Product objects that were identified by
        make_products.
        @param domain if provided, only this domain's Products are yielded"""
        if domain:
            if self._products.has_key(domain):
                yield self._products[domain]
        else:
            for domain in self.wrf():
                if self._products.has_key(domain):
                    yield self._products[domain]

########################################################################
class WRFGhost(WRFAnl):
    """!runs a short WRF simulation to generate wrfanl
    files named "ghost"

    This is the same as the superclass, WRFAnl, but it renames wrfanl
    files to "ghost" instead of "wrfanl".  Also, history output is
    disabled."""
    def make_wrf(self,wrf):
        """!makes the WRFSimulation object for this class.  

        Creates a copy of the WRFSimulation object wrf.  This first
        calls the WRFAnl.make_wrf, and then disables the history
        stream.
        @param wrf the wrf argument to __init__"""
        wrf=super(WRFGhost,self).make_wrf(wrf)
        wrf.set_wrfanl_outname('ghost_d<domain>')
        wrf.add_output('history',step=3600*3,end=9*3600,start=3*3600)
        return wrf
    def get_ghost(self,domain):
        """!same as get_wrfanl()
        @param domain the domain of interest"""
        if domain is None: return None
        if not domain in self.sim: return None
        domain=self.sim[domain]
        if domain.is_moad(): return None
        return self._products[domain]

########################################################################

##request only the outermost domain
#
#Special value for the trakdoms argument to WRFAnl4Trak.__init__().
#Requests that only the outermost domain be used in generating the
#parent model track
JUST_MOAD=object()

##request all domains
#
#Special value for the trakdoms argument to WRFAnl4Trak.__init__().
#Requests that all domains be used in generating the parent model track
ALL_DOMS=object()

########################################################################
class WRFAnl4Trak(WRFAnl):
    """!runs wrfanl and generates a track file

    This subtask of WRFAnl modifies the time of the 1 minute forecast
    wrfout file from the outer domain to have the analysis time
    instead so that the tracker can be run to get the initial storm
    location."""
    def __init__(self,dstore,conf,section,wrf,trakdoms=JUST_MOAD,
                 trakname='trankin_d<domain>',**kwargs):
        """!constructor
        
        Constructor for WRFAnl4Trak.  
        @param trakdoms what domains should be used for the tracks?
        either JUST_MOAD or ALL_DOMS or a list of WRFDomain objects
        @param trakname the track output filename.
        @param dstore,conf,section,wrf,kwargs passed to superclass constructor"""
        self._trakdoms=trakdoms
        self._trackin_name_pattern=trakname
        super(WRFAnl4Trak,self).__init__(dstore,conf,section,wrf,**kwargs)
    def track_domains(self):
        """! iterates over track input domains

        Iterates over domains specified by the trakdoms parameter to
        __init__()."""
        if self._trakdoms is JUST_MOAD:
            yield self.sim.get_moad()
        elif self._trakdoms is ALL_DOMS:
            for domain in self.sim: yield domain
        else:
            for domain in self._trakdoms: yield self.sim[domain]
    def make_products(self):
        """!creates Product objects for deliver_products() and products()

        Called from constructor.  This make_products adds all products
        produced by WRFAnl.make_products, but adds a product for the
        outer domain one minute forecast history stream (wrfout) file
        called "trackin_d<domain>".  That file is intended to be used
        to generate the parent domain vortex information."""
        WRFAnl.make_products(self)
        
        sim=self.sim
        self.trackin_name=dict()
        self.trackin_prod=dict()
        trackin_name_pattern=self._trackin_name_pattern
        for domain in self.track_domains():
            idom=sim[domain].get_grid_id()
            name=hwrf.wrfbase.parse_wrf_outname(
                trackin_name_pattern,idom,
                sim.simstart(),sim.get_nocolons())
            self.trackin_name[domain]=name
            loc=os.path.join(self.outdir,name)
            prod=FileProduct(self.dstore,name,self.taskname,location=loc)
            prod.location=loc
            prod['stream']='history'
            self.trackin_prod[domain]=prod

    def postrun(self):
        """!modifies trakin_* files to say they're at time 0

        Calls retime_wrfout for all domains whose trackin_d0X files
        are requested.  This produces the modified 1 minute forecast
        wrfout files that lie and say they're from the analysis time."""
        for domain in self.track_domains():
            self.retime_wrfout(domain)

    def retime_wrfout(self,domain):
        """!modifies a trakin file's internal timestamp

        If possible, modifies the stated output time in the one minute
        forecast trackin_d<domain> file to the analysis time.  Does
        this for one domain.  For intio files, it does not modify the
        file, but instead simply renames it.  That is done because, at
        last test, the post does not actually look in an intio wrfout
        file for the time, so no modification is necessary.
        @param domain the domain whose trakin file is being modified"""
        stream='history'
        logger=self.log()
        sim=self.wrf()
        dom=sim[domain]
        name=self.trackin_name[domain]
        logger.info('simend = '+repr(self.wrf().simend())+' = '+str(self.wrf().simend()))
        infile=dom.get_output(stream,self.wrf().simend(),logger=logger)
        logger.info('infile = '+str(infile.path()))
        io_form=sim.io_form_for(stream)%100
        if io_form == 1:
            logger.warning('%s: renaming instead of running wrfout_newtime '
                           'since file is (probably) not NetCDF: '
                           'io_form=%d'%(infile,io_form))
            os.rename(infile.path(),name)
            return
        try:
            shutil.copy2(infile.path(),name)
            checkrun(bigexe(self.getexe('wrfout_newtime'))[name,
                sim.simstart().strftime('%Y-%m-%d_%H:%M:%S')])
        except Exception as e:
            logger.error('%s (from %s): unable to modify time in NetCDF '
                         'file: %s'%(infile.path(), name, str(e)))
            raise

    def deliver_products(self):
        """!delivers products

        Delivers all products (FileProduct objects) created by
        make_products, including the new trackin_d<domain> added by
        this subclass, and all products added by the superclass
        WRFAnl."""
        super(WRFAnl4Trak,self).deliver_products()
        for d,p in self.trackin_prod.iteritems():
            p.deliver(frominfo=self.trackin_name[d],
                      logger=self.log(),keep=False)

    def get_trackin(self,domain=None):
        """!returns a trakin (trackin) Product

        Returns a trakin (trackin) Product.  If a domain is specified,
        returns the trackin file for that domain.  Otherwise, the
        MOAD (outermost domain) is assumed.
        @param domain None, or the domain of interest
        @return a Product for the trakin file for the specified domain"""
        if domain is None: 
            domain=self.sim.get_moad()
        if domain in self.trackin_prod:
            return self.trackin_prod[domain]
        return None

    def products(self,domain=None,domains=None,time=None,stream=None):
        """!iterates over all products from this task.  

        Iterates over all products from this task.  This class adds
        the trackin_d0* files.

        @param domain the domain of interest
        @param domains a list of domains of interest
        @param time the analysis time
        @param stream the output stream"""
        if not domains and not time and not stream:
            for p in WRFAnl.products(self,domain=domain): yield p

        if stream and stream!='history': return
        if time and time!=self.wrf().simstart(): return

        for d,p in self.trackin_prod.iteritems():
            simd=self.sim[d]
            if domains and simd not in domains: continue
            if domain is not None and domain!=simd: continue
            yield p
    ##@var trackin_name 
    # mapping from WRFDomain to product name for trakin files
    # @protected

    ##@var trackin_prod
    # mapping from WRFDomain to product for trakin files
    # @protected

########################################################################
class WRFGhostForPost(WRFAnl4Trak):
    """!runs wrf to generate ghost (wrfanl) and wrfout files

    This class generates wrfghost files, and wrfout files, for each
    domain.  The wrfout files happen at the end of a roughly one
    minute forecast so that they can be correctly post-processed.
    However, their internal timestamp has been changed to be for the
    analysis time.  This class derives from WRFAnl4Trak instead of
    WRFGhost to reuse the wrfout renamer functionality, but it may be
    used in place of a WRFGhost object.

    Note that a wrf ghost file is a wrfanl file.  There is no
    difference whatsoever, except in the nomenclature in the HWRF
    initialization."""
    def __init__(self,dstore,conf,section,wrf,trakdoms=ALL_DOMS,
                 trakname='ghout_d<domain>',**kwargs):
        """!constructor

        Creates a WRFGhostForPost, passing all arguments to
        WRFAnl4Trak.__init__()"""
        super(WRFGhostForPost,self).__init__(
            dstore,conf,section,wrf,trakdoms,trakname,**kwargs)
    def make_wrf(self,wrf):
        """!creates a WRFSimulation that calls its wrfanl files "ghost"
        files instead."""
        wrf=super(WRFGhostForPost,self).make_wrf(wrf)
        wrf.set_wrfanl_outname('ghost_d<domain>')
        return wrf

    def get_ghout(self,domain):
        """!returns the ghost wrfout product for the specified domain."""
        assert(domain is not None)
        return self.get_trackin(domain)

    def get_ghost(self,domain):
        """!same as get_wrfanl()

        This works exactly like get_wrfanl()
        @param domain the domain of interest"""
        if domain is None: return None
        if not domain in self.sim: return None
        domain=self.sim[domain]
        if domain.is_moad(): return None
        return self._products[domain]

########################################################################
class WRFAtmos(WRFTaskBase):
    """!The uncoupled HWRF forecast Task.

    This class runs an atmosphere-only WRF run, using wrfanl files
    from a prior WRFAnl simulation.  It encapsulates an
    ExternalWRFTask, which performs the actual tracking of products.
    This allows the post-processors and wrf copy tasks to keep track
    of the model's output while the model is running.  That subtask
    shows up as ".mon" (for "monitor") relative to this task (so if
    this task is wrfatmos, then the external task is wrfatmos.mon)."""
    def __init__(self,dstore,conf,section,wrf,keeprun=True,
                 wrfdiag_stream='auxhist1',**kwargs):
        """!WRFAtmos constructor

        The constructor for WRFAtmos.
        @param dstore the produtil.datastore.Datastore object
        @param conf the HWRFConfig object for configuration information
        @param section the section name within that HWRFConfig object
        @param wrf the hwrf.wrf.WRFSimulation that is to be executed
        @param keeprun True means the output directory should NOT be deleted
        @param wrfdiag_stream stream that generates wrfdiag files
        @param kwargs passed to the parent class constructor."""
        self._wrfdiag_stream=wrfdiag_stream
        # Create the WRFTaskBase first since it creates a copy of the
        # wrf, modifying the namelist in various ways:
        super(WRFAtmos,self).__init__(dstore,conf,section,wrf,
                                      keeprun=keeprun,**kwargs)
        # Create an ExternalWRFTask object to handle actual product
        # generation:
        self.__ex=hwrf.wrf.ExternalWRFTask(dstore=self.dstore,conf=self.conf,
            section=self.section,wrf=self.wrf(),
            taskname=self.taskname+'.mon',
            location=self.workdir,outdir=self.workdir)
        self.__ex['outdir']=self.workdir # make sure it is set even on rerun
    def make_wrf(self,wrf):
        """!creates a WRFSimulation for an uncoupled forecast run

        Creates a WRFSimulation for an uncoupled forecast run.  Runs
        the superclass WRFAtmos.make_wrf.  Instructs the resulting
        WRFSimulation to take analysis files as input, and calls the
        _set_wrf_proc_config to set I/O server and compute grid
        dimensions.
        @param wrf the wrf argument to __init__"""
        wrf=super(WRFAtmos,self).make_wrf(wrf)
        wrf.analysis_in()
        logger=self.log()
        self._set_wrf_proc_config(wrf,logger)
        return wrf
    def unrun(self): 
        """!deletes output files

        Deletes output files.  See the
        hwrf.wrf.ExternalWRFTask.unrun for details."""
        self.__ex.unrun()
    def run(self):
        """!runs the uncoupled WRF forecast

        Performs all work needed to run the program.  Sets the state
        to produtil.datastore.UNSTARTED.  Creates the work directory,
        CD's to it, runs the initial_prerun, link_fix,
        link_all_inputs, make_namelist, final_prerun, run_exe, postrun
        and deliver_products."""
        self.state=UNSTARTED
        self.__ex.state=UNSTARTED
        super(WRFAtmos,self).run()
    def last_completed_time(self,streams,check=False):
        """!Determines the last output time at which all streams have
        completed their output.
        
        Asks the underlying hwrf.wrf.ExternalWRFTask.last_completed_time()
        the last time at which all streams have completed their
        output.  If check=True, then all products are checked,
        otherwise cached information is used.
        @param streams a list of WRF stream names
        @param check if True, call produtil.datastore.Datum.check() on 
          any products that are unavailable.  Otherwise, cached information
          is used.
        @returns None if no times are complete, or a datetime.datetime
          of the last forecast time at which all streams are complete.
        """
        return self.__ex.last_completed_time(streams,check=check)
    def products(self,**kwargs): 
        """!iterates over all WRF products.  

        Iterates over all WRF products.  See the
        hwrf.wrf.ExternalWRFTask.products() for details.
        @param kwargs passed to hwrf.wrf.ExternalWRFTask.products()"""
        for p in self.__ex.products(**kwargs): 
            yield p
    def exproducts(self,**kwargs): 
        """!iterates over all WRF products.  

        Synonym for products().  Iterates over all WRF products.  See the
        hwrf.wrf.ExternalWRFTask.products() for details.
        @param kwargs passed to hwrf.wrf.ExternalWRFTask.products()"""
        for p in self.__ex.products(**kwargs): 
            yield p
    def wrf_check(self,**kwargs): 
        """!checks the status of the WRF simulation.  

        Checks the status of the WRF simulation.  Should only be
        used while the simulation is running.  This is intended to be
        run by jobs other than the WRF job, such as the
        post-processors, to monitor the WRF simulation as it
        progresses.

        @param kwargs passed to hwrf.wrf.ExternalWRFTask.wrf_check()"""
        self.__ex.wrf_check(**kwargs)
    def run_exe(self,exename='wrf',not_allranks=False,runner=None,
                sleeptime=None):
        """!runs the wrf program

        Called from run(), this runs the wrf program after the
        directory is set up. 

        @param exename sent to getexe() to get the path to wrf if
           runner is unspecified
        @param not_allranks if True, only use one MPI rank.  Do not use.
        @param runner an alternative produtil.prog.Runner to execute.
        @param sleeptime seconds to sleep between checks of the wrf
           executables completion.  The default sleeptime, if none is
           specified is 60 seconds rather than the usual 30."""
        if sleeptime is None:
            sleeptime=self.conffloat('sleeptime',60)
        super(WRFAtmos,self).run_exe(
            exename=exename,not_allranks=not_allranks,
            runner=runner,sleeptime=sleeptime)
    def update_state(self): 
        """!checks the wrf state and updates it in the HWRF database file

        Checks the state of the WRF simulation and copies that
        information to self.state in the produtil.datastore.Datastore.
        See hwrf.wrf.ExternalWRFTask.update_state() for details."""
        with self.dstore.transaction() as t:
            self.__ex.update_state()
            self.state=self.__ex.state
    def deliver_products(self,*args,**kwargs): 
        """!does nothing

        Has no effect.  This is present only because it is a
        requirement of the parent class.  No delivery is required
        because the products are all UpstreamFile objects, so the
        delivery state is set by the post-processors when calling the
        "check" function of each product.
        @param args,kwargs ignored"""

########################################################################
class AnalysisCycle(WRFGhost):
    """!runs wrf for an analysis cycle

    This class is similar to a WRFGhost run, except that it runs a
    longer simulation (typically 1-6 hours), and provides wrfghost and
    wrfinput files at the end of it.  It also requires wrfghost and
    wrfinput files as input.  Hence, one can run another AnalysisCycle
    after this one, using the output of the previous.  Note that this
    implementation relies on the fact that wrfghost, wrfanl and
    restart files are all exactly the same file format (just different
    times and domains)."""
    def __init__(self,dstore,conf,section,wrf,simlen=None,
                 keeprun=False,**kwargs):
        """!constructor for AnalysisCycle

        Constructor for the AnalysisCycle class.
        @param dstore the produtil.datastore.Datastore to use
        @param conf the hwrf.config.HWRFConfig that provides configuration ifnormation
        @param section the config section in conf
        @param wrf the hwrf.wrf.WRFSimulation object that is being run
        @param keeprun if True, the simulation temporary files are not deleted
        @param simlen simulation length in seconds
        @param kwargs passed unmodified to FcstTask.__init__()"""
        self.__prodcache=dict()
        if simlen is not None:
            simlen=to_timedelta(simlen)
        self.__simlen=simlen
        super(AnalysisCycle,self).__init__(dstore,conf,section,wrf,
                                           keeprun,**kwargs)
    ## @var __simlen
    #  The simulation length as a datetime.timedelta
    #  @private
    def make_products(self):
        """!called from constructor, creates Products

        This is called from the WRFTaskBase constructor.  It creates
        products that will be used by deliver_products() and
        products()"""
        self._products=dict()
        MOAD=self.wrf().get_moad()
        logger=self.log()

        for domain in self.wrf():
            pname='wrfinput_d%02d'%(domain.get_grid_id(),)
            loc=os.path.join(self.outdir,pname)
            if domain==MOAD:
                self.prod_wrfinput=FileProduct(
                    self.dstore,pname,self.taskname,location=loc)
            else:
                self._products[domain]=FileProduct(
                    self.dstore,pname,self.taskname,location=loc)

    ## @var prod_wrfinput
    #  the wrfinput output file at the end of the simulation
    #  @protected

    ## @var _products
    #  a mapping from WRFDomain to FileProduct for products other than wrfinput
    # @protected

    @property
    def anlouttime(self):
        """!analysis cycle end time

        The time at end of the analysis cycle, at which the output
        wrfanl and wrfinput files are available."""
        return self.wrf().simend()

    @property
    def anlintime(self):
        """!analysis cycle start time

        The time at beginning of the analysis cycle, at which the
        input wrfanl and wrfinput files must be provided."""
        return self.wrf().simstart()

    @property
    def simlen(self):
        """!simulation length

        The requested length of the simulation.  Note that this may
        not be the same as the actual simulation length
        (self.anlouttime-self.anlintime) due to the model timestep."""
        if self.__simlen is None:
            self.__simlen=to_timedelta(self.confstr('simlen'))
        return self.__simlen

    def make_wrf(self,wrf):
        """!creates the WRFSimulation object

        Called from the constructor.  Generates and returns a new
        WRFSimulation object that meets these requirements:
        1. Read in wrfanl and wrfinput files
        2. Output a restart and wrfinput file after self.simlen time
        3. Disable history and auxhist 1-3.
        @param wrf the wrf argument to __init__"""

        wrf=wrf.copy()

        # Read in analysis files:
        wrf.analysis_in()

        # Get the simulation length that was requested in either the
        # constructor or conf section:
        simlen=to_timedelta(self.simlen)

        # Change the simulation end time to the specified time:
        wrf.set_timing(end=simlen)

        # Output a restart file at the end of the simulation:
        wrf.add_output('restart',step=simlen,start=simlen,end=simlen)

        # Output an input file at the end of the simulation.  The rest
        # of this code assumes the file is written only once, with
        # only one output frame.
        wrf.add_output('inputout',step=simlen,start=simlen,end=simlen,
                       outname='wrfinputout_d<domain>')
        input_outname=wrf.nl.nl_get('time_control','input_outname')
        assert(input_outname=='wrfinputout_d<domain>')

        for domain in wrf:
            assert(domain.has_output('inputout'))
            assert(domain.has_output('restart'))

        # Make the domain not move by setting ntrack to a huge value
        for domain in wrf:
            domain.nl.nl_set('physics','ntrack',1000)

        # Disable output for four streams by moving the stream output
        # start time to after the end of the simulation.  This is to
        # work around buggy code that wants the WRF history stream
        # frequency to decide how to accumulate certain variables.
        logger=self.log()
        if not wrf.has_output('history'):
            wrf.add_output('history',start=simlen*2,step=simlen,end=simlen*3)
        for stream in [ 'auxhist1', 'auxhist2', 'auxhist3', 'history' ]:
            io_form=wrf.io_form_for(stream)
            for domain in wrf:
                if domain.has_output(stream):
                    domain.hide_output(stream)

        return wrf
    def wrfinput_at_time(self,atime=None,domain=None):
        """!the wrfinput_d01 output Product for the specified time and domain

        @return the wrfinput output Product for the specified time and
        domain or None if that time and domain are not valid.

        @param atime the time
        @param domain the domain (integer, string name or WRFDomain)"""
        if domain and domain!=self.wrf().get_moad(): return None
        if atime is not None and  \
                within_dt_epsilon(atime,self.anltime,self.dt_epsilon):
            return self.prod_wrfinput
        return None
    def get_wrfinput(self):
        """!the wrfinput_d01 output product

        @returns the output wrfinput Product"""
        return self.prod_wrfinput
    def deliver_products(self):
        """!delivers products from temporary areas
        
        Copies output products from temporary areas to long-term
        storage locations."""
        MOAD=self.wrf().get_moad()
        logger=self.log()
        when=self.anlouttime

        logger.info('Deliver products: rst and wrfinput at time %s'%(repr(when),))
        produtil.fileop.makedirs(self.outdir,logger=logger)

        for domain in self.wrf():
            pname='wrfinput_d%02d'%(domain.get_grid_id(),)
            loc=os.path.join(self.outdir,pname)
            if domain==MOAD:
                fromloc=domain.get_output('inputout',when,logger=logger).path()
                logger.info('Deliver moad %s from %s'%(str(domain),fromloc))
                self.prod_wrfinput.deliver(
                    frominfo=fromloc,logger=logger,keep=False)
            else:
                fromloc=domain.get_output('restart',when,logger=logger).path()
                logger.info('Deliver nest %s from %s'%(str(domain),fromloc))
                self._products[domain].deliver(
                    frominfo=fromloc,logger=logger,keep=False)
                for out in domain.get_outputs('history'):
                    fromloc=out.path()
                    logger.info('Deliver nest %s from %s'%(str(domain),fromloc))
                    toloc=os.path.join(self.outdir,fromloc)
                    logger.info('Deliver nest %s to %s'%(str(domain),toloc))
                    produtil.fileop.deliver_file(
                        fromloc,toloc,keep=True,logger=logger)
    def as_product(self,wrfout,relocate=False):
        """Converts a WRFOutput to a Product.

        Returns a Product for a WRFOutput.  The Product is cached in
        self.__prodcache and as_product() will return that cached
        value if one is available.  Otherwise, a new one is created.
        Call clear_cached_products() to clear the cache.."""
        if self.__prodcache.has_key(wrfout):
            return self.__prodcache[wrfout]
        rel=wrfout.path()
        #outdir=os.path.join(self['outdir'],rel)
        outdir=self['outdir']
        assert(outdir is not None)
        loc=os.path.join(outdir,os.path.basename(wrfout.path()))
        with self.dstore.transaction() as t:
            uf=UpstreamFile(self.dstore,category=self.taskname,
                            prodname=rel,location=loc)
            uf['stream']=wrfout.stream()
            uf['location']=loc
        if relocate:    uf.location=loc
        self.__prodcache[wrfout]=uf
        return uf
    def products(self,domain=None,domains=None,stream=None,time=None,relocate=False):
        """!iterates over all selected products

        Iterates over all products for the specified domain, or all
        products if the domain is unspecified or None.
        @param domain the requested domain (an integer, string name or WRFDomain)
        @param domains a list of domains of interest
        @param time the analysis time
        @param stream the output stream"""
        if domains is not None:
            for d in domains:
                if stream is None:
                    for out in d.get_all_outputs(time):
                        yield self.as_product(out,relocate=relocate)
                elif time is None:
                    for out in d.get_outputs(stream):
                        yield self.as_product(out,relocate=relocate)
                else:
                    yield self.as_product(d.get_output(stream,time),
                                          relocate=relocate)
        else:
            if domain is None:
                yield self.prod_wrfinput
            elif domain==self.wrf().get_moad():
                yield self.prod_wrfinput
                return
            elif domain is None:
                yield self.prod_wrfinput 
            for p in super(AnalysisCycle,self).products(domain):
                yield p
