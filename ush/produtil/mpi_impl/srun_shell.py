##@namespace produtil.mpi_impl.srun 
# Adds SLURM srun support to produtil.run
#
# This module is part of the mpi_impl package -- see produtil.mpi_impl
# for details.  This translates produtil.run directives to SLURM srun
# commands.

import os, logging, re
import produtil.fileop,produtil.prog,produtil.mpiprog,produtil.pipeline

from .mpi_impl_base import MPIMixed,CMDFGen,ImplementationBase, \
                           MPIThreadsMixed,MPILocalOptsMixed,MPITooManyRanks
from produtil.pipeline import NoMoreProcesses
from produtil.mpiprog import MIXED_VALUES

class RunShellCommand(object):
    def __init__(self,command):
        self.command=command
    def to_shell(self,runner,logger=None):
        return self.command,runner
    def __call__(self,*args,**kwargs):
        raise NotImplementedError('Cannot run srun_shell at execution time.  Only use this package to create shell commands before executing the batch job.')

class Implementation(ImplementationBase):
    """Adds SLURM srun support to produtil.run
    
    This module is part of the mpi_impl package -- see produtil.mpi_impl
    for details.  This translates produtil.run directives to SLURM srun
    commands."""

    ##@var srun_path
    # Path to the srun program
    
    @staticmethod
    def name():
        return 'srun_shell'

    @staticmethod
    def detect(logger=None,force=False,silent=False,nodesize=None,**kwargs):
        """!Returns None unless force=True.  This means that srun_shell will
        only be used if it is specifically requested."""
        if force:
            return Implementation(logger,silent,force,nodesize)
        return None

    def __init__(self,logger,silent,force,nodesize):
        super(Implementation,self).__init__(logger)
        self.silent=silent
        self.nodesize=nodesize

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
        assert(arg is not None)
        if threads is not None:
            arg.threads=threads
            return arg.env(OMP_NUM_THREADS=threads,KMP_NUM_THREADS=threads,
                           KMP_AFFINITY='scatter')
        else:
            del arg.threads
            return arg
    
    def can_run_mpi(self):
        """!Does this module represent an MPI implementation? Returns True."""
        return True
    
    def make_bigexe(self,exe,**kwargs): 
        """!Returns an ImmutableRunner that will run the specified program.
        @returns an empty list
        @param exe The executable to run on compute nodes.
        @param kwargs Ignored."""
        return produtil.prog.ImmutableRunner([str(exe)],**kwargs)
    
    def mpirunner(self,arg,allranks=False,nodesize=None,**kwargs):
        """!Turns a produtil.mpiprog.MPIRanksBase tree into a produtil.prog.Runner
        @param arg a tree of produtil.mpiprog.MPIRanksBase objects
        @param allranks if True, and only one rank is requested by arg, then
          all MPI ranks will be used
        @param kwargs passed to produtil.mpi_impl.mpi_impl_base.CMDFGen
          when mpiserial is in use.
        @returns a produtil.prog.Runner that will run the selected MPI program"""
        f=self.mpirunner_impl(arg,allranks=allranks,nodesize=nodesize,**kwargs)
        if not self.silent:
            logging.getLogger('srun').info("%s => %s"%(repr(arg),repr(f)))
        return f
    
    def mpirunner_impl(self,arg,allranks=False,nodesize=None,label_io=False,**kwargs):
        """!This is the underlying implementation of mpirunner and should
        not be called directly."""
        if not nodesize:
            nodesize=self.nodesize
        if not nodesize:
            nodesize=int(os.environ['PRODUTIL_RUN_NODESIZE'],10)

        assert(isinstance(arg,produtil.mpiprog.MPIRanksBase))
        (serial,parallel)=arg.check_serial()
        if serial and parallel:
            raise MPIMixed('Cannot mix serial and parallel MPI ranks in the '
                           'same MPI program.')
    

        if arg.mixedlocalopts():
            raise MPILocalOptsMixed('Cannot mix different local options for different executables or blocks of MPI ranks in impi')
        if arg.threads==MIXED_VALUES:
            raise MPIThreadsMixed('Cannot mix different thread counts for different executables or blocks of MPI ranks in impi')


        srun_args=['srun','--export=ALL','--cpu_bind=core']

        if label_io:
            srun_args.append('--label')

        arbitrary_pl=list()
        layout_pl=list()
    
        if arg.nranks()==1 and allranks:
            srun_args.append('--distribution=block:block')
            arglist=[ str(a) for a in arg.to_arglist(
                    pre=srun_args,before=[],between=[])]
            return produtil.prog.Runner(arglist)
        elif allranks:
            raise MPIAllRanksError(
                "When using allranks=True, you must provide an mpi program "
                "specification with only one MPI rank (to be duplicated across "
                "all ranks).")
        elif serial:
            srun_args.append('--distribution=block:block')
            arg=produtil.mpiprog.collapse(arg)
            lines=[str(a) for a in arg.to_arglist(to_shell=True,expand=True)]
            return produtil.prog.Runner(
                ['srun','--ntasks','%s'%(arg.nranks()),'mpiserial'],
                prerun=CMDFGen('serialcmdf',lines,silent=self.silent,**kwargs))

        srun_args.extend(['--distribution','arbitrary'])

        cmdfile=list()

        total_nodes=0
        total_tasks=0

        for rank,count in arg.expand_iter(expand=False):
            if count<1: next

            total_tasks+=count

            if rank.ranks_per_node:
                rpn=max(min(nodesize,rank.ranks_per_node),1)
            else:
                rpn=max(nodesize,1)
            need_nodes=max(1,(count+rpn-1)//rpn)

            total_nodes += need_nodes

            # Split ranks evenly among nodes:
            min_rpn=count//need_nodes
            nodes_with_extra_rank=count-need_nodes*min_rpn
            smaller_nodes=need_nodes-nodes_with_extra_rank

            if smaller_nodes:
                arbitrary_pl.append('%dx%d'%(
                    min_rpn,smaller_nodes))
                layout_pl.append( [ '-n', '%d'%int(min_rpn*smaller_nodes) ] + \
                                  [ y for y in rank.args() ] )
            if nodes_with_extra_rank:
                arbitrary_pl.append('%dx%d'%(
                    min_rpn+1,nodes_with_extra_rank))
                layout_pl.append( [ '-n', '%d'%int(nodes_with_extra_rank*(min_rpn+1)) ] + \
                                  [ y for y in rank.args() ] )

        srun_args.extend(['--ntasks','%d'%total_tasks])
        srun_args.extend(['--nodes','%d'%total_nodes])

        arbitrary_pl_text=','.join(arbitrary_pl)
        layout_pl_text=' : '.join([ ' '.join(k) for k in layout_pl ])

        prerun=RunShellCommand(
            'rm -f arbitrary_pl_out layout_pl_out ; '
            'arbitrary.pl '+arbitrary_pl_text+' > arbitrary_pl_out ; ' \
            'layout.pl '+layout_pl_text+' > layout_pl_out ; ')

        srun_args.extend(['-w','./arbitrary_pl_out'])
        srun_args.extend(['--multi-prog','./layout_pl_out'])

        return produtil.prog.Runner(srun_args,prerun=prerun)
