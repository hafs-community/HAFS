##@namespace produtil.mpi_impl.srun 
# Adds SLURM srun support to produtil.run
#
# This module is part of the mpi_impl package -- see produtil.mpi_impl
# for details.  This translates produtil.run directives to SLURM srun
# commands.

import os, logging, sys
import produtil.fileop,produtil.prog,produtil.mpiprog,produtil.pipeline

from .mpi_impl_base import MPIMixed,CMDFGen,ImplementationBase,MPIError, \
                           MPIThreadsMixed,MPILocalOptsMixed,MPITooManyRanks, \
                           MPIMissingEnvironment,MPIEnvironmentInvalid
from produtil.pipeline import NoMoreProcesses
from produtil.mpiprog import MIXED_VALUES

class Implementation(ImplementationBase):
    """Adds SLURM srun support to produtil.run
    
    This module is part of the mpi_impl package -- see produtil.mpi_impl
    for details.  This translates produtil.run directives to SLURM srun
    commands."""

    ##@var srun_path
    # Path to the srun program

    def get_int_env(self,varname):
        if varname not in os.environ:
            raise MPIMissingEnvironment(
                'Could not find %s in the environment'%(varname,))
        try:
            return int(os.environ[varname],10)
        except ValueError as ve:
            raise MPIEnvironmentInvalid(
                '%s is not an integer (is \"%s\" instead)'%(
                    varname,os.environ[varname]))
    
    def get_pack_group_sizes_from_environment(self):
        if 'SLURM_PACK_SIZE' not in os.environ:
            return [self.get_int_env('SLURM_NPROCS')]
        pack_size=self.get_int_env('SLURM_PACK_SIZE')
        pack_group_sizes=[0] * pack_size
        for ipack in range(pack_size):
            group_size=self.get_int_env('SLURM_NPROCS_PACK_GROUP_%d'%ipack)
            pack_group_sizes[ipack]=group_size
        return pack_group_sizes
    
    def fake_pack_group_sizes(self, mpiprog):
        return [ count for rank,count in mpiprog.expand_iter(False) ]

    def get_pack_group_sizes(self,mpiprog=None,scheduler_distribution=None):
        if mpiprog is not None and self.force:
            if scheduler_distribution is not None:
                # Nproc is nodes*ppn for each group in the
                # distribution specified to the scheduler:
                return [n[0]*n[1] for n in scheduler_distribution ]
            try:
                return self.get_pack_group_sizes_from_environment()
            except (KeyError,ValueError,MPIError) as e:
                return self.fake_pack_group_sizes(mpiprog)
        return self.get_pack_group_sizes_from_environment()

    @staticmethod
    def name():
        return 'srun'

    @staticmethod
    def detect(srun_path=None,mpiserial_path=None,logger=None,force=False,silent=False,**kwargs):
        """!Detects whether the SLURM srun command is available by
        looking for it in the $PATH.  Also requires the SLURM_NODELIST
        variable.  This is to detect the case where srun is available,
        but no slurm resources are available."""

        # Are pack groups in use?
        if 'SLURM_PACK_SIZE' not in os.environ and not force:
            return None

        # Are we in a SLURM job?
        if 'SLURM_NODELIST' not in os.environ and not force:
            return None

        # Is srun available?
        if srun_path is None:
            if force:
                srun_path='srun'
            else:
                srun_path=produtil.fileop.find_exe('srun',raise_missing=True)

        return Implementation(srun_path,mpiserial_path,logger,silent,force)

    def __init__(self,srun_path,mpiserial_path,logger,silent,force):
        super(Implementation,self).__init__(logger)
        if mpiserial_path or force:
            self.mpiserial_path=mpiserial_path
        self.srun_path=srun_path
        self.silent=silent
        self.force=force

    def runsync(self,logger=None):
        """!Runs the "sync" command as an exe()."""
        if logger is None: logger=self.logger
        sync=produtil.prog.Runner(['/bin/sync'])
        p=produtil.pipeline.Pipeline(sync,capture=True,logger=logger)
        version=p.to_string()
        status=p.poll()

    def openmp(self,arg,threads):
        """!Adds OpenMP support to the provided object
    
        @param arg An produtil.prog.Runner or
        produtil.mpiprog.MPIRanksBase object tree
        @param threads the number of threads, or threads per rank, an
        integer"""
        # Origin note: this is almost a verbatim copy of
        # lsf_cray_intel.Implementation.openmp()
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
            
        threads=int(threads)
        if hasattr(arg,'argins'):
            if not self.silent:
                self.logger.info('Threaded with %s threads so add ntasks, nodes, and cpus-per-task arguments.'%(
                        repr(threads),))
            for a in reversed(["--export=ALL","--ntasks=1","--nodes=1",
                               "--cpus-per-task=%d"%int(threads)]):
                arg=arg.argins(1,a)
            arg=arg.env(KMP_NUM_THREADS=threads,OMP_NUM_THREADS=threads)
        if hasattr(arg,'threads'):
            arg.threads=threads
        return arg
    
    def can_run_mpi(self):
        """!Does this module represent an MPI implementation? Returns True."""
        return True
    
    def make_bigexe(self,exe,**kwargs): 
        """!Returns an ImmutableRunner that will run the specified program.
        @returns an empty list
        @param exe The executable to run on compute nodes.
        @param kwargs Ignored."""
        return produtil.prog.ImmutableRunner([
            self.srun_path,'--ntasks','1',str(exe)],**kwargs)
    
    def mpirunner(self,arg,allranks=False,scheduler_distribution=None,**kwargs):
        """!Turns a produtil.mpiprog.MPIRanksBase tree into a produtil.prog.Runner
        @param arg a tree of produtil.mpiprog.MPIRanksBase objects
        @param allranks if True, and only one rank is requested by arg, then
          all MPI ranks will be used
        @param kwargs passed to produtil.mpi_impl.mpi_impl_base.CMDFGen
          when mpiserial is in use.
        @returns a produtil.prog.Runner that will run the selected MPI program"""
        f=self.mpirunner_impl(arg,allranks,scheduler_distribution,**kwargs)
        if not self.silent:
            logging.getLogger('srun').info("%s => %s"%(repr(arg),repr(f)))
        return f
    
    def mpirunner_impl(self,arg,allranks,scheduler_distribution,**kwargs):
        """!This is the underlying implementation of mpirunner and should
        not be called directly."""
        assert(isinstance(arg,produtil.mpiprog.MPIRanksBase))
        (serial,parallel)=arg.check_serial()
        if serial and parallel:
            raise MPIMixed('Cannot mix serial and parallel MPI ranks in the '
                           'same MPI program.')
    

        if arg.mixedlocalopts():
            raise MPILocalOptsMixed(
                'Cannot mix different local options for different '
                'executables or blocks of MPI ranks in slurm')
        if arg.threads==MIXED_VALUES:
            raise MPIThreadsMixed(
                'Cannot mix different thread counts for different '
                'executables or blocks of MPI ranks in slurm')

        srun_args=[self.srun_path,'--export=ALL']
    
        if arg.nranks()==1 and allranks:
            pack_group_sizes=self.get_pack_group_sizes(arg,scheduler_distribution)
            pack_size=len(pack_group_sizes)
            srun_args=[self.srun_path]
            for i in range(pack_size):
                if i>0: srun_args += [':']
                srun_args+=[a for a in arg.args()]
            result=produtil.prog.Runner(srun_args)
        elif allranks:
            raise MPIAllRanksError(
                "When using allranks=True, you must provide an mpi program "
                "specification with only one MPI rank (to be duplicated across "
                "all ranks).")
        elif serial:
            arg=produtil.mpiprog.collapse(arg)
            lines=[str(a) for a in arg.to_arglist(to_shell=True,expand=True)]
            included_ranks=0
            desired_ranks=arg.nranks()
            pack_group_sizes=self.get_pack_group_sizes(arg,scheduler_distribution)
            srun_args=[self.srun_path]
            for igroup in range(len(pack_group_sizes)):
                group_size=pack_group_sizes[igroup]
                use_ranks=max(0,min(group_size,desired_ranks-included_ranks))
                included_ranks+=use_ranks
                if not use_ranks:
                    break # do not request use of 0 members of a pack group
                if igroup>0:
                    srun_args += [':']
                srun_args += ['--export=ALL','--ntasks=%d'%use_ranks,
                              self.mpiserial_path]
            result=produtil.prog.Runner(srun_args,prerun=CMDFGen(
                'serialcmdf',lines,silent=self.silent,**kwargs))
        else:
            included_ranks=0
            desired_ranks=arg.nranks()
            pack_group_sizes=self.get_pack_group_sizes(arg,scheduler_distribution)
            next_unused_group=0
            first=True
            
            srun_args=[self.srun_path]

            for rank_group,nranks in arg.expand_iter(False):

                remaining_ranks=nranks
                for pack_group_index in range(
                        next_unused_group,len(pack_group_sizes)):
                    group_size=pack_group_sizes[pack_group_index]
                    use_ranks=max(0,min(group_size,remaining_ranks))
                    remaining_ranks-=use_ranks
                    next_unused_group+=1
                    if not use_ranks: break
                    if first:
                        first=False
                    else:
                        srun_args+=[':']
                    srun_args += ['--export=ALL','--ntasks=%d'%use_ranks]
                    for rank_arg in rank_group.args():
                        srun_args.append(rank_arg)
                    if not remaining_ranks:
                        break
                if remaining_ranks:
                    raise MPITooManyRanks('Too many MPI programs or MPI ranks for the requested program; request more pack groups or more ranks per group.')
            result=produtil.prog.Runner(srun_args)
    
        if arg.threads:
            result.env(OMP_NUM_THREADS=arg.threads,
                       KMP_NUM_THREADS=arg.threads)
        return result
