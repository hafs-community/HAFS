#! /usr/bin/env python3
################################################################################
# Script Name: no_mpi.py
# Authors: NECP/EMC Hurricane Project Team 
# Abstract:
#   Stub funcitons to allow produtil.mpi_impl  to run when MPI is unavailable.
#   This module is part of the produtil.mpi_impl package.  It underlies
#   the produtil.run.openmp, produtil.run.mpirun , and
#   produtil.run.mpiserial functions, providing the implementation
#   needed to run when MPI is unavailable.
# History:
#   06/28/2021: Initial version for HAFS applicaton (adapted from HWRF/HMON)
# Condition codes:
#   == 0 : success
#   != 0 : fatal error encounted
################################################################################

##@namespace produtil.mpi_impl.no_mpi 

import os, logging
import produtil.prog,produtil.pipeline
from .mpi_impl_base import MPIDisabled,ImplementationBase
module_logger=logging.getLogger('lsf_cray_intel')

class Implementation(ImplementationBase):
    @staticmethod
    def name():
        """Returns the string "no_mpi" to indicate MPI is not available."""
        return 'no_mpi'
    @staticmethod
    def detect(logger=None,force=False,**kwargs):
        """!Returns a new instrance of this class to indicate that
        the "no MPI" implementation of MPI is always available"""
        return Implementation()
    def __init__(self,logger=None):
        super(Implementation,self).__init__(logger=logger)
