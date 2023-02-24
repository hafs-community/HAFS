## @namespace produtil.mpi_impl.pbs_cray_intel
# Adds support for PBSPro+mpiexec with the Intel OpenMP to produtil.run
# This module is part of the mpi_impl package -- see produtil.mpi_impl
# for details.
# Similar to lsf_cray_intel.py but for WCOSS2's PBSPro and Cray combination 
# Biju Thomas on 3/9/2022
#

import os, socket, logging, sys
import produtil.fileop,produtil.prog,produtil.mpiprog, produtil.run

from .mpi_impl_base import MPIMixed,CMDFGen,ImplementationBase

class Implementation(ImplementationBase):
    """!Adds support for PBS+mpiexec with the Intel OpenMP to produtil.run

    This module is part of the mpi_impl package -- see produtil.mpi_impl
    for details.  This implements the combination of PBS, Cray
    mpiexec with Intel OpenMP."""

    @staticmethod
    def name():
        return 'pbs_cray_intel'

    @staticmethod
    def synonyms():
        yield None

    @staticmethod
    def detect(mpiexec_path=None,total_tasks=None,nodesize=None,hyperthreads=None,
               logger=None,force=False,p_state_turbo=None,silent=False,**kwargs):
        """!Determines if Cray mpiexec should be used to run MPI programs by
        looking for the mpiexec program in $PATH

        @param mpiexec_path Optional.  The path to mpiexec.
        
        @param total_tasks Optional: the number of slots available to
          run processes.  This is the maximum value for
          MPI_COMM_WORLD*OMP_NUM_THREADS.

        @param nodesize The number of slots available for MPI and
        OpenMP processes on a single compute node (as with
        total_tasks, but for one node).

        @param hyperthreads The number of hyperthreads per core.

        @param logger a logging.Logger for messages

        @param p_state_turbo the --p-state argument to send to mpiexec
        to request turbo mode.

        @param force Optional: if True, then detect() will always succeed,
          and will use "mpirun" as the mpirun path if mpirun_path is missing"""

        if mpiexec_path is None:
            mpiexec_path=produtil.fileop.find_exe('mpiexec',raise_missing=not force)
            if force: mpiexec_path='mpiexec'
        detected=mpiexec_path is not None
        if detected:
            impl=Implementation(mpiexec_path,total_tasks,nodesize,hyperthreads,
                              logger,p_state_turbo,silent)
            if not force:
                if not silent:
                    logger.info('pbs_cray_intel detected; replace '
                                'produtil.fileop.ln_sf')
                produtil.fileop.ln_sf = impl.mpiexec_ln_sf
            return impl

    ##@var mpiexec_path
    # Path to the mpiexec program, or None if it isn't found.

    ##@var p_state_turbo
    # Value to send to mpiexec --p-state option for Intel Turbo Mode.  
    #
    # This is the largest value printed out by 
    #
    #     cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_frequencies
    #
    # Which is either the highest allowed sustained clock speed, or the magic
    # number for Turbo Mode.

    def __init__(self,mpiexec_path=None,total_tasks=None,nodesize=None,hyperthreads=None,
                 logger=None,p_state_turbo=None,silent=False):

        if p_state_turbo:
            self.p_state_turbo=int(p_state_turbo)
        else:
            self.p_state_turbo=None

        if logger is None:
            logger=logging.getLogger('mpi_impl')
        self.logger=logger

        if mpiexec_path is None:
            mpiexec_path=produtil.fileop.find_exe('mpiexec',raise_missing=True)
        self.mpiexec_path=mpiexec_path
        
        if not hyperthreads:
            hyperthreads=os.environ.get('PRODUTIL_RUN_HYPERTHREADS','1')
            hyperthreads=int(hyperthreads)
        self.hyperthreads=hyperthreads

        if not total_tasks:
            total_tasks=os.environ['TOTAL_TASKS']
            total_tasks=int(total_tasks)
        self.total_tasks=total_tasks

        if not nodesize:
            nodesize=os.environ.get('PRODUTIL_RUN_NODESIZE','128')
            nodesize=int(nodesize)
        self.nodesize=nodesize

        self.silent=bool(silent)

    def mpiexec_ln_sf(self,source,target,content,logger=None):
        if logger is None: logger=self.logger
        msg="Filesystem failure (will retry with mpiexec ln -sf): Cannot symlink \"%s\" -> \"%s\".  Instead, the symlink is to \"%s\"."%(
            target,source,content)
        logger.error(msg)
        cmd=produtil.run.batchexe(self.mpiexec_path)[
            '-q',"-n","1","/bin/ln","-sf",
            str(source),str(target)]
        status=produtil.run.run(cmd)

        logger.warning('mpiexec ln -sf %s %s = %s'%(
            str(source),str(target),repr(status)))

        content=os.readlink(target)
        if content==source:
            if not self.silent:
                logger.info('mpiexec ln -sf %s %s SUCCESS'%(
                        str(source),str(target)))
            return True
        msg="FILESYSTEM FAILURE: Python and mpiexec ln -sf both cannot symlink \"%s\" -> \"%s\".  Instead, the symlink is to \"%s\"."%(
                target,source,content)
        logger.critical(msg)
        raise produtil.fileop.WrongSymlink(msg,target)

    def get_p_state_turbo(self):
        """!Value to send to mpiexec --p-state option for Intel Turbo Mode.  
        
        This is the largest value printed out by 
        
             cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_frequencies
    
        Which is either the highest allowed sustained clock speed, or the magic
        number for Turbo Mode."""
        if self.p_state_turbo is not None:
            return self.p_state_turbo
        states=list()
        with open('/sys/devices/system/cpu/cpu0/cpufreq/scaling_available_frequencies','rt') as saf:
            for line in saf.readlines():
                if not self.silent:
                    self.logger.info('saf line: '+repr(line))
                splat=line.split()
                if not self.silent:
                    self.logger.info('saf line split: '+repr(splat))
                for s in splat:
                    try:
                        if not self.silent:
                            self.logger.info('saf entry: '+repr(s))
                        states.append(int(s))
                    except ValueError as ve:
                        pass
        states.sort()
        if not self.silent:
            self.logger.info('Possible --p-states options: '+repr(states))
        self.p_state_turbo=states[-1]
        if not self.silent:
            self.logger.info('Turbo Mode --p-states option: %d'%self.p_state_turbo)
        assert(self.p_state_turbo>5e5)
        return self.p_state_turbo
    
    def runsync(self,logger=None):
        """!Runs the "sync" command as an exe()."""
        if logger is None: logger=self.logger
        if not self.silent:
            logger.info('Not running sync.')
        return
    
    def openmp(self,arg,threads):
        """!Adds OpenMP support to the provided object
    
        @param arg An produtil.prog.Runner or
        produtil.mpiprog.MPIRanksBase object tree
        @param threads the number of threads, or threads per rank, an
        integer"""
        
        if threads is None:
            try:
                ont=os.environ.get('OMP_NUM_THREADS','')
                ont=int(ont)
                if ont>0:
                    threads=ont
            except (KeyError,TypeError,ValueError) as e:
                pass
    
        if threads is None:
            nodesize=self.nodesize
            threads=max(1,nodesize-1)
            
        assert(threads>0)
        threads=int(threads)
        if hasattr(arg,'argins'):
            if not self.silent:
                self.logger.info('Threaded with %s threads so add --depth --cpu-bind core.'%(
                        repr(threads),))
            for a in reversed(['--depth','%d'%int(threads),'--cpu-bind','core']):
                arg=arg.argins(1,a)
            arg=arg.env(OMP_NUM_THREADS=threads)
    
        if hasattr(arg,'threads'):
            arg.threads=threads

        return arg
    
    def can_run_mpi(self,):
        """!Does this class represent an MPI implementation? Returns True."""
        return True
    
    def make_bigexe(self,exe,**kwargs): 
        """!Returns an ImmutableRunner that will run the specified program.
        @returns an empty list
        @note This function does NOT search $PATH.  That ensures the $PATH
          will be expanded on the compute node instead.  Use
          produtil.fileop.find_exe() if you want to explicitly search the
          PATH before execution
        @param exe The executable to run on compute nodes.
        @param kwargs Ignored."""
        exe=str(exe)    
        inside_mpiexec=int(os.environ.get('INSIDE_APRUN','0'))
        inside_mpiexec+=1
        r=produtil.prog.Runner(['mpiexec','-n','1',exe])\
            .env(INSIDE_APRUN=str(inside_mpiexec))
        return produtil.prog.ImmutableRunner(r)
    
    def mpirunner(self,arg,allranks=False,logger=None,**kwargs):
        if logger is None:
            logger=self.logger
        m=self.mpirunner2(arg,allranks=allranks,logger=logger,**kwargs)
        if not self.silent:
            logger.info('mpirunner: %s => %s'%(repr(arg),repr(m)))
        return m

    def per_prog_options(self,rank,count,pernode):
        seenS=False
        seenPS=False
        seenCC=False
        seenJ=False
        seenN=False
        arglist=['-n','%d'%count]
        #Zhan
        if rank.threads:
            arglist.extend(['--depth','%d'%int(rank.threads),'--cpu-bind','core'])
            seenCC=True
        if rank.haslocalopts():
            localopts=[ str(x) for x in rank.localoptiter() ]
            runner=runner[localopts]
            seenS='-S' in localopts
        if not seenCC:
            arglist.extend(['--depth','1','--cpu-bind','core'])
        return arglist
    
    def mpirunner2(self,arg,allranks=False,logger=None,**kwargs):
        """!Turns a produtil.mpiprog.MPIRanksBase tree into a produtil.prog.Runner
        @param arg a tree of produtil.mpiprog.MPIRanksBase objects
        @param allranks if True, and only one rank is requested by arg, then
          all MPI ranks will be used
        @param logger a logging.Logger for log messages
        @param kwargs passed to produtil.mpi_impl.mpi_impl_base.CMDFGen
          when mpiserial is in use.
        @returns a produtil.prog.Runner that will run the selected MPI program"""
        if logger is None:
            logger=self.logger
        assert(isinstance(arg,produtil.mpiprog.MPIRanksBase))
        (serial,parallel)=arg.check_serial()
        if serial and parallel:
            raise MPIMixed(
                'Cannot mix serial and parallel MPI ranks in the same '
                'MPI program.')
        nodesize=self.nodesize
        hyperthreads=self.hyperthreads
        maxtasks=self.total_tasks
    
        # The returned runner object.  We'll add to this below:
        runner=produtil.prog.Runner([self.mpiexec_path])
   
        # Set up the INSIDE_APRUN variable so the executed MPI program
        # will be able to run serial programs.
        inside_mpiexec=int(os.environ.get('INSIDE_APRUN','0'))
        inside_mpiexec+=1
        runner=runner.env(INSIDE_APRUN=str(inside_mpiexec))
    
        if arg.nranks()==1 and allranks:
            for rank,count in arg.expand_iter(expand=False):
                pernode=min(maxtasks,nodesize//arg.nonzero_threads)
                rpn=rank.ranks_per_node
                if rpn: pernode=min(pernode,rpn)
                runner=runner[self.per_prog_options(rank,maxtasks,pernode)]
                runner=runner[rank.args()]
                return runner
        elif allranks:
            raise MPIAllRanksError(
                "When using allranks=True, you must provide an mpi program "
                "specification with only one MPI rank (to be duplicated across "
                "all ranks).")
        elif serial:
            for rank,count in arg.expand_iter(False):
                try:
                    if rank.runner.getenv('INSIDE_APRUN'):
                        raise MPIMixed('Trying to run mpiexec within mpiexec.  In mpiserial, you must run batchexe() programs only.')
                except KeyError as ke: pass
            lines=[a for a in arg.to_arglist(to_shell=True,expand=True)]
            if produtil.fileop.find_exe('mpiserial') is None:
                raise MPISerialMissing(
                    'Attempting to run a serial program via mpiexec mpiserial but '
                    'the mpiserial program is not in your $PATH.')


            first=True
            arg=produtil.mpiprog.collapse(arg)

            prior_arglist=[]
            total_count=0
            
            for rank,count in arg.expand_iter(expand=False):
                pernode=min(maxtasks,nodesize//rank.nonzero_threads)
                rpn=rank.ranks_per_node
                if rpn: pernode=min(pernode,rpn)

                arglist=self.per_prog_options(rank,maxtasks,pernode)

            if prior_arglist==arglist:
                total_count+=count
            elif not total_count or not prior_arglist:
                total_count=count
                prior_arglist=arglist
            else:
                if not first: runner=runner[':']
                runner=runner[prior_arglist]
                runner=runner['-n',str(total_count),'mpiserial']
                first=False
                prior_arglist=arglist
                total_count=count

            if prior_arglist==arglist and total_count:
                if not first: runner=runner[':']
                runner=runner[prior_arglist]
                runner=runner['-n',str(total_count),'mpiserial']

            cmdfgen=CMDFGen('serialcmdf',lines,**kwargs)
            runner=runner.prerun(cmdfgen)
            return runner
        else:
            first=True
            
            maxtasks=1
            for rank,count in arg.expand_iter(expand=False):
                maxtasks=max(count,maxtasks)
                
            for rank,count in arg.expand_iter(expand=False):
                threads=rank.nonzero_threads
                pernode=min(count,maxtasks,nodesize//threads)
                rpn=rank.ranks_per_node
                if rpn: pernode=min(pernode,rpn)
                if not first: runner=runner[':']
                runner=runner[self.per_prog_options(rank,count,pernode)]
                runner=runner[rank.to_shell()]
                first=False
            return runner

