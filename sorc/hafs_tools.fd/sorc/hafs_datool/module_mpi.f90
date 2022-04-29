

module module_mpi

!-----------------------------------------------------------------------------
!  PURPOSE: This module provides routines for parallelizing.
!
!------------------------------------------------------------------------------


   use MPI


   integer, parameter :: IO_NODE = 0
   integer :: nprocs, my_proc_id, comm, ierr, request, nprocs_mod, tag
   double precision   :: time_start, time_end
   integer, dimension(MPI_STATUS_SIZE)   :: status

   real                                  :: mpisend
   real, allocatable, dimension(:  )     :: mpirecv
  
   real, allocatable, dimension(:  )     :: mpisend1d
   real, allocatable, dimension(:,:)     :: mpirecv1d

   integer :: prev, next

   contains

!-----------------------------------------------------------------------------
!  PURPOSE: to basically set up a communicator for a rectangular mesh.
!------------------------------------------------------------------------------
   subroutine parallel_start()

      implicit none

      ! Arguments

      ! Local variables

      integer :: mpi_rank, mpi_size
      integer :: mpi_ierr

      ! Find out our rank and the total number of processors
      call MPI_Init(mpi_ierr)
      call MPI_Comm_rank(MPI_COMM_WORLD, mpi_rank, mpi_ierr)
      call MPI_Comm_size(MPI_COMM_WORLD, mpi_size, mpi_ierr)
      time_start = MPI_Wtime()

      comm = MPI_COMM_WORLD
      nprocs = mpi_size
      my_proc_id = mpi_rank






   end subroutine parallel_start

!-----------------------------------------------------------------------------
!  PURPOSE: Free up, deallocate, and for MPI, finalize.
!------------------------------------------------------------------------------
   subroutine parallel_finish()

      implicit none

      ! Arguments

      ! Local variables

      integer :: mpi_ierr

      time_end = MPI_Wtime()
      call MPI_Finalize(mpi_ierr)


   end subroutine parallel_finish


end module module_mpi
