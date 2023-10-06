module module_mpi

!-----------------------------------------------------------------------------
!  PURPOSE: This module provides routines for parallelizing.
!------------------------------------------------------------------------------
   use mpi
   integer :: nprocs, my_proc_id, comm, ierr
   integer, dimension(mpi_status_size) :: status

   contains

!-----------------------------------------------------------------------------
!  PURPOSE: to basically set up a communicator for a rectangular mesh.
!------------------------------------------------------------------------------
   subroutine parallel_start()

   implicit none

   integer :: mpi_rank, mpi_size
   integer :: mpi_ierr

   ! Find out our rank and the total number of processors
   call mpi_init(mpi_ierr)
   call MPI_Comm_rank(MPI_COMM_WORLD, mpi_rank, mpi_ierr)
   call MPI_Comm_size(MPI_COMM_WORLD, mpi_size, mpi_ierr)
   comm = MPI_COMM_WORLD
   nprocs = mpi_size
   my_proc_id = mpi_rank

   end subroutine parallel_start

!-----------------------------------------------------------------------------
!  PURPOSE: Free up, deallocate, and for MPI, finalize.
!------------------------------------------------------------------------------
   subroutine parallel_finish()

   implicit none

   !integer :: mpi_ierr

   call MPI_Finalize(ierr)

   end subroutine parallel_finish

end module module_mpi
