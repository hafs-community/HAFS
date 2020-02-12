import os, shutil, math, logging
import produtil.datastore, produtil.fileop, produtil.cd, produtil.run
import produtil.rusage
import hwrf.hwrftask, hwrf.numerics, hwrf.exceptions, hwrf.fcsttask

from produtil.rusage import setrlimit, rusage, getrlimit
from produtil.fileop import isnonempty, make_symlink, deliver_file
from produtil.run import mpirun, mpi
from hwrf.numerics import to_fraction,to_datetime_rel,TimeMapping

from produtil.log import jlogger

########################################################################
class Component(object):
    """This is a simple utility class that stores information about
    coupler components (wrf, pom, hycom, wavewatch3, coupler, etc.)
    for the CoupledWRF class.  You should never instantiate a
    Component -- only CoupledWRF should do that.  It will return one
    of these when you run CoupledWRF's "component" function.

    You can safely modify the exe, rankname, rankdefault and initer
    elements of this class, and it will modify the corresponding
    information in the CoupledWRF.  Do not modify the name and order.
    Modifying the name or order will break the internal data
    structures in CoupledWRF"""
    def __init__(self,name,exe,rankname,rankdefault,initer):
        """Creates a Component with the given characteristics.
        Initializes the order to None.  The order must be overwritten
        externally, and is done in CoupledWRF.couple."""
        (self._name,self.exe,self.rankname,self.rankdefault,self.initer)=\
            (name,exe,rankname,rankdefault,initer)
        self._order=None
    @property
    def name(self): return self._name
    @property 
    def order(self): return self._order

########################################################################
class ComponentIniter(object):
    """This class should be subclassed to make coupling component
    initializers for CoupledWRF.  That is, you should implement a
    subclass and send it to the CoupledWRF.couple(...,initer=).  Its
    job is to provide input data and initialization checks for the
    various coupling components such as ocean and wave.  Note that the
    CoupledWRF itself initializes the WRF and coupler, so you do not
    need a ComponentIniter for those components."""
    def check_coupled_inputs(self,logger):
        """This function implements a very fast (<1 second) check to
        see if the components initialization was complete.  Returns
        True on success or False on failure."""
        raise NotImplementedError(
            "Subclass forgot to implement check_coupled_inputs.")
    def link_coupled_inputs(self,just_check,logger):
        """If just_check is True, then this function implements a more
        expensive check of whether the initialization for this
        component succeeded.  If just_check is False, then this
        component should link or copy all files needed to run a
        coupled forecast.  The default implementation calls
        check_coupled_inputs if just_check is True, and simply returns
        True if just_check is False.  Subclasses should re-implement
        this function."""
        if just_check:
            return self.check_coupled_inputs(logger)
        return True
    def make_exe(self,task,exe,ranks):
        """Called when it is time to actually run the coupled forecast
        executables.  Returns a produtil.mpiprog.MPIRanksBase for the
        MPI ranks for this component of the coupling.  By default,
        this returns mpi(task.getexe(exe))*ranks.  However, the caller
        can re-implement this function to replace the executable
        depending on the results of the initialization.  If that is
        desired, the task.getexe (and other task.get*) should be used
        so the executable locations can be overridden in the config
        files."""
        return mpi(task.getexe(exe))*ranks

class TurboIniter(ComponentIniter):
    def check_coupled_inputs(self,logger):
        return True
    def make_exe(self,task,exe,ranks):
        """!Turns on Intel turbo mode for the specified executable, if the
        local MPI implementation knows how."""
        m=super(TurboIniter,self).make_exe(task,exe,ranks)
        m.turbomode=True
        logging.getLogger('coupling.py').info(
            'exe for wrf compute is %s'%(repr(m),))
        assert(m.turbomode)
        return m

########################################################################
class CoupledWRF(hwrf.fcsttask.WRFAtmos):
    """Runs the NCEP coupler and WRF, coupled to at least one ocean or
    wave model.  This class is not thread-safe due to how self.couple
    is implemented."""
    def __init__(self,dstore,conf,section,wrf,keeprun=True,
                 wrfdiag_stream='auxhist1',**kwargs):
        super(CoupledWRF,self).__init__(dstore,conf,section,wrf,keeprun,
                                           wrfdiag_stream,**kwargs)
        self.__components=dict()
        self.__order=list()
        #self.couple('coupler','hwrf_wm3c','wm3c_ranks',1,None)
        #self.couple('pom','hwrf_ocean_fcst','pom_ranks',None,None)
        #self.couple('wrf','hwrf_wrf','wrf_ranks',None,None)
        self._coupled_products=dict()
        self.__coupled=None
        self._default_coupling=True
    
    def check_all_inputs(self,coupled=None):
        """Returns True if all inputs needed to run the forecast are
        present, and false otherwise.  If coupled=True (the default),
        then coupled components inputs are also checked, otherwise
        only WRF inputs are checked."""
        if coupled is not None: self.__coupled=self._default_coupling
        logger=self.log()
        okay=super(CoupledWRF,self).check_all_inputs()
        if okay and coupled:
            for c in self.coupleiter():
                if c.initer is not None:
                    okay=c.initer.check_coupled_inputs(logger=self.log())
        return okay

    def link_all_inputs(self,just_check=False,coupled=None):
        """If just_check=True, links or copies inputs required by all
        components of the coupled simulation.  If just_check=False,
        runs through all steps required to link or copy inputs without
        actually linking or copying.  That mode is intended to be an
        expensive but more thurough check than check_all_inputs.

        Returns True if all inputs were linked, and False otherwise."""
        logger=self.log()
        okay=super(CoupledWRF,self).link_all_inputs(
            just_check=just_check)
        if just_check:
            if okay:
                logger.info('WRF inputs are present.')
            else:
                logger.error('FAIL: WRF inputs are missing.')
        if coupled is None: coupled=self.__coupled
        if coupled is None: coupled=self._default_coupling
        if not coupled:
            logger.info(
                'Not copying ocean, wave and coupler inputs because an '
                'uncoupled simulation was requested.')
            return okay
        if not just_check:
            self.make_coupler_namelist('cpl_nml')
        for c in self.coupleiter():
            if c.initer is not None:
                okay=okay and c.initer.link_coupled_inputs(just_check,
                                                  logger=logger)
                if not okay:
                    msg='%s inputs are missing.'%(c.name,)
                    logger.error(msg)
                    if not just_check:
                        raise hwrf.exceptions.OceanInitFailed(msg)
                    return False
                else:
                    logger.info('%s inputs are all present.'%(c.name,))
        return okay

    def run(self,coupled=None):
        """Runs the coupled simulation.  Sets the internal coupled
        vs. uncoupled flag to the specified value.  Default: True
        (coupled)."""
        if coupled is None: coupled=self._default_coupling
        self.__coupled=coupled
        super(CoupledWRF,self).run()

    def run_exe(self):
        """Runs the MPI command for the coupled coupled simulation.
        Do not call this directly: call self.run instead."""
        logger=self.log()
        setrlimit(logger=logger,stack=6e9,ignore=True)
        if self.__coupled is None: self.__coupled=self._default_coupling
        if self.__coupled:
            logger.info('Starting the coupled wrf simulation')
            cmd=None
            callback_args=dict()
            callback_components=list()
            for c in self.coupleiter():
                logger.info(
                    'Component #%d is %s: exe=%s ranks from %s (default %s)'
                    %(c.order,c.name,c.exe,c.rankname,repr(c.rankdefault)))
                exe=c.exe
                ranks=self.confint(c.rankname,c.rankdefault)
                if not isinstance(ranks,int):
                    raise TypeError(
                        'Somehow ended up with a non-int for ranks in '
                        'CoupledWRF.run_exe.  Check inputs.  Ranks is a %s '
                        '%s.'%(type(ranks).__name__,repr(ranks)))
                if not ranks>0:
                    raise ValueError(
                        'Ranks (%d) is not >0 in CoupledWRF.run_exe.  Check '
                        'config files and scripts.'%ranks)
                if c.initer is not None:
                    mpiified=c.initer.make_exe(self,exe,ranks)
                else:
                    mpiified=mpi(self.getexe(exe))*ranks

                logger.info('Appending ranks %s with turbo=%s'%(
                    repr(mpiified),repr(mpiified.turbomode)))
                
                callback_args[c.name]=ranks
                callback_components.append(c.name)
                cmd = mpiified if(cmd is None) else cmd+mpiified
            if cmd is None:
                raise hwrf.exceptions.NoCoupledComponents(
                    'No coupled components specified in CoupledWRF.'
                    '  You must call CoupledWRF.couple(...)')
            #logfile=self.confstrinterp('{WORKhwrf}/cpl.out')
            logfile=self.confstrinterp('{coupled_log}')
            jlogger.info('%s: will log coupled forecast stdout here'%(
                    logfile,))
            for rank,count in cmd.expand_iter(expand=False):
                logger.info('Will run rank %s * %s with turbo=%s'%(
                    repr(rank),repr(count),repr(rank.turbomode)))
                if count==1248:
                    assert(rank.turbomode)
            cmd=mpirun(cmd) > logfile
            cmd=self.call_run_exe_callbacks(
                cmd,callback_components,callback_args)
            #assert(isnonempty('pom.nml'))
            if not isnonempty('cpl_nml'):
                raise hwrf.exceptions.EmptyCouplerNamelist(
                    'Logic error: somehow the cpl_nml is empty or missing.')
            super(CoupledWRF,self).run_exe('wrf',None,cmd)
        else:
            logger.info('Starting the uncoupled wrf simulation')
            super(CoupledWRF,self).run_exe('wrf',False,None)

    def make_coupler_namelist(self,filename='cpl_nml'):
        """Makes the namelist for the NCEP Coupler"""
        logger=self.log()
        logger.info('section is %s'%(self.section,))
        
        cplsec=self.confstr('cpl_nml')
        dt_c=self.confint('dt_c')
        f_dt_c=to_fraction(dt_c)
        simlen=to_fraction(self.sim.simend()-self.sim.simstart())
        cstepmax=int(math.ceil(float(simlen/f_dt_c)))
        morevars=dict(cstepmax=cstepmax, dt_c=dt_c)
        
        logger.info('dt_c=%s f_dt_c=%s simlen=%s cstepmax=%s'%(
                repr(dt_c),repr(f_dt_c),repr(simlen),repr(cstepmax)))
        
        cplnml=hwrf.namelist.Conf2Namelist(
            conf=self.conf,section=cplsec,morevars=morevars)
        with open(filename,'wt') as cn:
            nmlstr=cplnml.make_namelist(morevars=morevars)
            cn.write(nmlstr)

    ####################################################################
    # Component manipulation
    def coupleiter(self):
        """Iterates over all Component objects that describe coupling
        components."""
        i=-1
        for name in self.__order:
            i+=1
            c=self.__components[name]
            yield c
    def couple(self,name,exe,rankname,rankdefault=None,initer=None):
        """Adds the specified coupling component.  Returns self.
           name -- a user-defined name of this component, must be a string.
           exe -- option in the [exe] section with the executable path
           rankname -- name of the option in this task's section that
              has the number of ranks
           rankdefault -- number of ranks if the rankname option is
              missing or empty.  Can be None, in which case the
              rankname option must be set.
           initer -- the object that will link inputs ot the working
              directory before the forecast begins.  Can be None.
        Note that the superclass, WRFAtmos, initializes the WRF 
        component and this class initializes the coupler, so you can
        pass None for those components' "initer" objects.

        This subroutine is not thread-safe."""
        if not isinstance(name,basestring):
            raise TypeError(
                'The "name" argument to CoupledWRF.couple must be a string.  '
                'You passed a %s %s.'%(type(name).__name__,repr(name)))
        if name in ('output','history','restart','restartin','input',
                    'inputout') or name.find('auxhist')==0 or \
                    name.find('auxinput')==0:
            raise ValueError(
                'Component name cannot be the same as any WRF stream name: '
                'output, history, restart, restartin, input, inputout, '
                'auxhist* or auxinput*.')
        if rankdefault is not None and not isinstance(rankdefault,int):
            raise TypeError(
                'The "rankdefault" argument to CoupledWRF.couple must be '
                'None or an int.  You passed a %s %s.'
                %(type(rankdefault).__name__,repr(rankdefault)))
        if not isinstance(exe,basestring):
            raise TypeError(
                'The "exe" argument to CoupledWRF.couple must be a string.  '
                'You plassed a %s %s.'%(type(exe).__name__,repr(exe)))
        if not isinstance(rankname,basestring):
            raise TypeError(
                'The "rankname" argument to CoupledWRF.couple must be '
                'a string.  You plassed a %s %s.'
                %(type(rankname).__name__,repr(rankname)))
        rd=None if (rankdefault is None) else str(rankdefault)
        if name not in self.__components:
            self.__order.append(name)
        c=Component(name,exe,rankname,rankdefault,initer)
        c._order=self.order(name)
        self.__components[name]=c
        return self

    def couplewrf(self):
        """!Wrapper around one or two calls to couple() to add the WRF.

        Attempts to add the WRF to the coupling in two different sets of
        MPI ranks: one for WRF compute, and one for WRF I/O.  This is
        done to allow the local MPI implementation to set up the 
        compute nodes differently for the two blocks of processors, 
        thereby speeding up the forecast."""
        wrf=self.wrf()
        nio_pergroup=wrf.nio_tasks_per_group
        nio_groups=wrf.nio_groups

        # NOTE: wrf_ranks is now just the default if wrf_compute_ranks
        # is unspecified.  You should use wrf_compute_ranks in the
        # conf instead.
        wrf_ranks=self.confint('wrf_ranks',0)

        if isinstance(nio_pergroup,list) or isinstance(nio_pergroup,tuple):
            nio=nio_groups*sum(nio_pergroup)
        else:
            maxdom=wrf.maxdom()
            nio=nio_groups*nio_pergroup*maxdom

        if nio>0:
            # I/O servers enabled, so split into two groups.

            # Get the number of compute (non-I/O) ranks:
            if wrf_ranks>0:
                wrf_compute_ranks=wrf_ranks-nio
            else:
                wrf_compute_ranks=None

            # Report compute ranks first:
            self.couple('wrf_compute','wrf','wrf_compute_ranks',wrf_compute_ranks,TurboIniter())

            # Report I/O ranks second:
            self.couple('wrf_io','wrf','wrf_io_ranks',nio)
        elif wrf_ranks>0:
            # No I/O servers.
            self.couple('wrf_compute','wrf','wrf_compute_ranks',wrf_ranks,TurboIniter())

    def remove_wave(self): pass
    def remove_ocean(self): pass

    def uncouple(self,which=None):
        """!Removes a component, or all components, from the coupling.
        @param which the name of the component to remove.  If None or 
           unspecified, run totally uncoupled.
        @returns self"""
        if which is None:
            # Uncouple everything
            self._default_coupling=False
            return self
        del self.__components[which]
        order=list()
        for w in self.__order:
            if w!=which:
                order.append(w)
        self.__order=order
        for c in self.coupleiter():
            c._order=self.order(c.name)
        if len(order) == 2:
            # Special case: only coupler and wrf are left.
            self.uncouple()
        return self

    def component(self,which):
        """Returns information about the component with the specified
        name (a string) or at the specified zero-based index (an
        int).  Returns a mutable object of class Component."""
        if isinstance(which,basestring):
            return self.__components[which]
        elif isinstance(which,int):
            name=self.__order[which]
            return self.__components[name]
        else:
            raise TypeError('CoupledWRF.component requires an int or string '
                            'argument.  You passed a %s %s.'%
                            (type(which).__name__,repr(which)))
    def order(self,name):
        """Components of coupling are ordered by where they show up in
        the MPI execution.  The NCEP coupler must be first.  This
        function returns the zero-based order.  Hence, if one does:

            self.couple("coupler",...).couple("hycom",...).couple("wrf",...)

        then:

            self.order("coupler") = 0
            self.order("hycom")   = 1
            self.order("wrf")     = 2 

        Other values will raise KeyError."""
        i=-1
        for o in self.__order:
            i+=1
            if o==name: return i
        raise KeyError(name)
    ####################################################################
    # Products
    def add_coupled_stream(self,stream,times):
        """Adds a new non-WRF output stream."""
        if stream in ('output','history','restart','restartin','input',
                    'inputout') or stream.find('auxhist')==0 or \
                    stream.find('auxinput')==0:
            raise ValueError(
                'Component stream name (%s) cannot be the same as any WRF '
                'stream name: output, history, restart, restartin, input, '
                'inputout, auxhist* or auxinput*.'%(stream,))
        if stream not in self._coupled_products:
            a=self.sim.simstart()
            timerel=[to_datetime_rel(t,a) for t in times]
            self._coupled_products[stream]=TimeMapping(timerel,list)
        return self
    def add_coupled_product(self,stream,time,product):
        """Adds a product for a non-WRF output stream at a given time."""
        self._coupled_products[stream][time].append(product)
        return self
    def coupled_products(self,stream=None,time=None):
        """Iterates over non-WRF products."""
        if stream is None:
            if time is None:
                for (s,tprod) in self._coupled_products.iteritems():
                    for (t,prods) in tprod.iteritems():
                        for p in prods: yield p
            else:
                for (s,tprod) in self._coupled_products.iteritems():
                    prods=tprod.get(time,None)
                    if prods is None: continue
                    for p in prods: yield p
        elif stream in self._coupled_products:
            tprod=self._coupled_products[stream]
            if time is None:
                for (t,prods) in tprod.iteritems():
                    for p in prods: yield p
            else:
                prods=tprod.get(time,None)
                if prods is not None:
                    for p in prods: yield p

    def products(self,domains=None,stream=None,time=None,**kwargs):
        """Iterates over all products, both WRF and non-WRF."""
        if stream is None:
            coupled=True
            wrf=True
        else:
            coupled=stream in self._coupled_products
            wrf=not coupled

        if wrf:
            for p in super(CoupledWRF,self).products(
                  domains=domains,stream=stream,time=time,**kwargs):
                yield p
        if coupled:
            for p in self.coupled_products(stream,time):
                yield p

########################################################################
class CouplingStatus(object):
    """Maintains the ocean and wave status files in the COM directory."""
    def __init__(self,conf,section,logger=None):
        self.__conf=conf
        self.__section=section
        if not isinstance(section,basestring):
            raise TypeError('The section argument to CouplingStatus.__init__ must be a string, not a %s %s.'%(type(section).__name__,repr(section)))
        if logger is None:
            logger=conf.log(section)
        self.__logger=logger
    @property
    def logger(self):
        """The logging.Logger to use for logging messages."""
        return self.__logger
    @property
    def conf(self): 
        """The configuration object, a subclass of hwrf.config.HWRFConfig"""
        return self.__conf
    @property
    def section(self): 
        """The section in self.conf to use for configuration information."""
        return self.__section
    def unset(self,logger=None):
        """Delete the coupling status files.  If the logger is
        not specified, the section name is used for the logging domain"""
        if logger is None: logger=self.logger
        for ocstat in ( self.section, self.section+'2' ):
            ocstat=self.conf.getstr(self.section,ocstat)
            ocstatfile=os.path.join(self.conf.getdir('com'),ocstat)
            produtil.fileop.remove_file(ocstatfile,info=True,logger=logger)
    def fileiter(self):
        yield self.conf.strinterp(self.section,'{'+self.section+'}')
        yield self.conf.strinterp(self.section,'{'+self.section+'2}')
    def set(self,coupling_flag,logger=None,morelines=None):
        """Set RUN_COUPLED=YES (true) or =NO (false) depending on the
        value of coupled_flag.  If the logger is not specified, the
        section name is used as the logging domain."""
        if logger is None: logger=self.log()
        coupling_flag=bool(coupling_flag)
        for ocstatfile in self.fileiter():
            strflag='YES' if coupling_flag else 'NO'
            logger.info('Setting RUN_COUPLED=%s in status file %s'%(
                    strflag,ocstatfile))
            with open(ocstatfile,'wt') as f:
                f.write('RUN_COUPLED='+strflag+"\n")
                if morelines is not None:
                    for line in morelines:
                        logger.info('%s: write %s'%(ocstatfile,line))
                        f.write(line+'\n')
    def read(self,logger=None):
        """Reads the first coupling status file (identified by {section}=)
        and returns the contents as an array of lines."""
        if logger is None: logger=self.logger
        ocstatfile=self.conf.strinterp(self.section,'{'+self.section+'}')
        if not os.path.exists(ocstatfile):
            return list()
        with open(ocstatfile,'rt') as f:
            lines=f.readlines(2**20)
        return lines

    def get(self,logger=None):
        """Checks the coupling status file.  If the file does not exist or
        cannot be opened or read, then False is returned.  Otherwise, the
        file is scanned for RUN_COUPLED=YES or RUN_COUPLED=NO (case
        insensitive).  The last of those RUN_COUPLED lines is used:
        NO=return False, YES=return True.  If the logger is not specified,
        the section name is used as the logging domain"""
        if logger is None: logger=self.logger
        ocstatfile=self.conf.strinterp(self.section,'{'+self.section+'}')
        success=None

        logger.info('%s: scan status file for RUN_COUPLED=YES or NO'%(
                ocstatfile))

        try:
            with open(ocstatfile,'rt') as f:
                for line in f:
                    if line.upper().find('RUN_COUPLED=YES')>=0:
                        success=True
                        logger.info(
                            'Status file says: RUN_COUPLED=YES')
                    elif line.upper().find('RUN_COUPLED=NO')>=0:
                        success=False
                        logger.warning(
                            'Status file says: RUN_COUPLED=NO')
        except EnvironmentError as e:
            logger.error(
                'Error checking status file: %s'
                %(str(e),),exc_info=True)
        except Exception as ee:
            logger.error(
                'Unhandled exception while checking status file: %s'
                %(str(ee),),exc_info=True)
            raise

        if success is None:
            logger.warning('Could not scan status file for RUN_COUPLED=YES'
                           ' or NO.  Assuming RUN_COUPLED=NO.')
            success=False
        elif success:
            logger.info(
                'RUN_COUPLED=YES: status file says coupled component init succeeded.')
        else:
            logger.warning(
                'RUN_COUPLED=NO: status file says coupled component init failed.')

        return success
    
