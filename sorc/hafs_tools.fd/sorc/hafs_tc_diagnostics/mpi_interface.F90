module mpi_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: mpi_interface
  ! Copyright (C) 2018 Henry R. Winterbottom

  ! Email: henry.winterbottom@noaa.gov

  ! This program is free software: you can redistribute it and/or
  ! modify it under the terms of the GNU General Public License as
  ! published by the Free Software Foundation, either version 3 of the
  ! License, or (at your option) any later version.

  ! This program is distributed in the hope that it will be useful,
  ! but WITHOUT ANY WARRANTY; without even the implied warranty of
  ! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  ! General Public License for more details.

  ! You should have received a copy of the GNU General Public License
  ! along with this program.  If not, see
  ! <http://www.gnu.org/licenses/>.

  !=======================================================================

  ! Define associated modules and subroutines
  
  use kinds_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: mpi_comm_world
  public :: mpi_double
  public :: mpi_ierror
  public :: mpi_integer
  public :: mpi_interface_gettaskid
  public :: mpi_interface_gettasklev
  public :: mpi_interface_finalize
  public :: mpi_interface_initialize
  public :: mpi_interface_partition
  public :: mpi_interface_waitall
  public :: mpi_logical
  public :: mpi_masternode
  public :: mpi_nprocs
  public :: mpi_noproc_assign
  public :: mpi_procid
  public :: mpi_real
  public :: mpi_sum
  public :: mpi_taskgrid
  public :: mpi_taskgrid_struct
  include "mpif.h"

  ! Define local variables

  type mpi_taskgrid_struct
     integer,                   dimension(:,:),             allocatable :: level
     integer,                   dimension(:),               allocatable :: procnlevs
     integer,                   dimension(:),               allocatable :: begin
     integer,                   dimension(:),               allocatable :: end
     integer                                                            :: maxprocid
     integer                                                            :: nprocs
     integer                                                            :: ncoords
     integer                                                            :: nlevs
  end type mpi_taskgrid_struct    ! type mpi_taskgrid_struct
  type(mpi_taskgrid_struct)                                             :: mpi_taskgrid
  logical                                                               :: mpi_abort
  integer                                                               :: mpi_masternode
  integer                                                               :: mpi_ierror
  integer                                                               :: mpi_procid
  integer                                                               :: mpi_nprocs
  integer                                                               :: mpi_noproc_assign = -9999

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! mpi_interface_gettasklev.f90

  ! DESCRIPTION:

  ! This subroutine returns the level assigned to the respective
  ! task/node using the FORTRAN mpi_taskgrid_struct variable
  ! attributes.

  ! INPUT VARIABLES:

  ! * procid; a FORTRAN integer specifying the MPI task
  !   identification; this assumes C-indexing (e.g., the minimum
  !   allowed value is 0).

  ! * zidx; a FORTRAN integer specifying the array element
  !   corresponding to a respective level.

  ! * lev; a FORTRAN integer to contain the respective level assigned
  !   to the task/node.

  ! OUTPUT VARIABLES:

  ! * lev; a FORTRAN integer to containing the respective level
  !   assigned to the task/node.

  !-----------------------------------------------------------------------

  subroutine mpi_interface_gettasklev(procid,zidx,lev)

    ! Define variables passed to routine
 
    integer                                                             :: procid
    integer                                                             :: lev
    integer                                                             :: zidx

    ! Define variables computed within routine

    integer                                                             :: id

    !=====================================================================

    ! Define local variables

    id  = procid + 1
    lev = mpi_taskgrid%level(id,zidx)

    !=====================================================================

  end subroutine mpi_interface_gettasklev

  !=======================================================================

  ! SUBROUTINE:

  ! mpi_interface_gettaskid.f90

  ! DESCRIPTION:

  ! This subroutine returns the processor identification assigned to
  ! the respective task/node using the FORTRAN mpi_taskgrid_struct
  ! variable attributes.

  ! INPUT VARIABLES:

  ! * procid; a FORTRAN integer specifying the MPI task
  !   identification; this assumes C-indexing (e.g., the minimum
  !   allowed value is 0).

  ! * id; a FORTRAN integer to contain the respective processor
  !   identification.

  ! OUTPUT VARIABLES:

  ! * id; a FORTRAN integer containing the respective processor
  !   identification.

  !-----------------------------------------------------------------------

  subroutine mpi_interface_gettaskid(procid,id)

    ! Define variables passed to routine
 
    integer                                                             :: procid
    integer                                                             :: id

    !=====================================================================

    ! Define local variables

    id  = procid + 1

    !=====================================================================

  end subroutine mpi_interface_gettaskid

  !=======================================================================
  
  ! SUBROUTINE: 

  ! mpi_interface_initialize.f90

  ! DESCRIPTION:

  ! This subroutine acts as the initial interface to the Message
  ! Passing Interface (MPI) libraries; it defines the master task
  ! (mpi_masternode) and initializes necessary local variables (i.e.,
  ! mpi_abort).

  !-----------------------------------------------------------------------

  subroutine mpi_interface_initialize()

    !=====================================================================

    ! Define local variables

    call mpi_init(mpi_ierror)
    call mpi_comm_rank(mpi_comm_world,mpi_procid,mpi_ierror)
    call mpi_comm_size(mpi_comm_world,mpi_nprocs,mpi_ierror)
    mpi_masternode = 0
    mpi_abort      = .false.

    !=====================================================================

  end subroutine mpi_interface_initialize

  !=======================================================================

  ! SUBROUTINE: 

  ! mpi_interface_finalize.f90

  ! DESCRIPTION:

  ! This subroutine acts as the final interface to the Message Passing
  ! Interface (MPI) libraries.

  !-----------------------------------------------------------------------

  subroutine mpi_interface_finalize()

    !=====================================================================

    ! Define local variables

    call mpi_finalize(mpi_ierror)

    !=====================================================================
    
  end subroutine mpi_interface_finalize

  !=======================================================================

  ! SUBROUTINE:

  ! mpi_interface_partition.f90

  ! DESCRIPTION:

  ! This subroutine partitions the simulation grid both in the
  ! horizontal and the vertical in accordance with the number of
  ! allocated processors provided to the respective executable; the
  ! global FORTRAN mpi_taskgrid_struct variable is defined
  ! accordingly.

  ! INPUT VARIABLES:

  ! * debug; a FORTRAN logical value specifying whether to include
  !   debug information during execution.

  !-----------------------------------------------------------------------

  subroutine mpi_interface_partition(debug)

    ! Define variables passed to routine

    logical                                                             :: debug

    !=====================================================================
    
    ! Compute local variables

    call mpi_define_taskgrid(debug)
    call mpi_define_proclevassign(debug)

    !=====================================================================

  end subroutine mpi_interface_partition

  !=======================================================================

  ! SUBROUTINE: 

  ! mpi_interface_waitall.f90

  ! DESCRIPTION:

  ! This subroutine acts as wrapper around the respective Message
  ! Passing Interface (MPI) distribution mpi_barrier subroutine.

  !-----------------------------------------------------------------------

  subroutine mpi_interface_waitall()

    !=====================================================================

    ! Define local variables

    call mpi_barrier(mpi_comm_world,mpi_ierror)

    !=====================================================================

  end subroutine mpi_interface_waitall

  !=======================================================================

  ! SUBROUTINE: 

  ! mpi_define_proclevassign.f90

  ! DESCRIPTION:

  ! This subroutine assigns a simulation (grid_struct FORTRAN
  ! variable) level to a processor allocated for computation; the
  ! global FORTRAN mpi_taskgrid_struct variable is defined
  ! accordingly.

  ! INPUT VARIABLES:

  ! * debug; a FORTRAN logical value specifying whether to include
  !   debug information during execution.

  !-----------------------------------------------------------------------

  subroutine mpi_define_proclevassign(debug)

    ! Define variables passed to routine

    logical                                                             :: debug

    ! Define variables computed within routine

    integer                                                             :: level
    integer                                                             :: nlev

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    mpi_taskgrid%level = mpi_noproc_assign

    ! Check local variable and proceed accordingly

    if(mpi_procid .eq. mpi_masternode) then

       ! Define local variable

       level = 1
       nlev  = 1

       ! Check local variable and proceed accordingly

       do while(level .le. mpi_taskgrid%nlevs)

          ! Loop through local variable

          do i = 1, mpi_taskgrid%nprocs

             ! Define local variables

             mpi_taskgrid%level(i,nlev) = level
             level                      = level + 1
             if(level .gt. mpi_taskgrid%nlevs) goto 1000

          end do ! do i = 1, mpi_taskgrid%nprocs

          ! Define local variables

          nlev = nlev + 1

       end do ! do while(level .le. mpi_taskgrid%nlevs)

       ! Define local variables

1000   continue

    end if ! if(mpi_procid .eq. mpi_masternode)

    ! Define local variables

    call mpi_interface_waitall()
    call mpi_bcast(mpi_taskgrid%level,(mpi_taskgrid%nprocs*                &
         & mpi_taskgrid%nlevs),mpi_integer,mpi_masternode,                 &
         & mpi_comm_world,mpi_ierror)

    ! Check local variable and proceed accordingly

    if(mpi_procid .eq. mpi_masternode) then

       ! Check local variable and proceed accordingly

       if(debug) then

          ! Define local variables

          write(6,*) ' '

          ! Loop through local variable
          
          do j = 1, mpi_taskgrid%nlevs
          
             ! Loop through local variable

             do i = 1, mpi_taskgrid%nprocs

                ! Check local variable and proceed accordingly

                if(mpi_taskgrid%level(i,j) .ne. mpi_noproc_assign) then

                   ! Define local variables

                   write(6,500) i, j, mpi_taskgrid%level(i,j)
                
                end if ! if(mpi_taskgrid%level(i,j)
                       ! .ne. mpi_noproc_assign)

             end do ! do i = 1, mpi_taskgrid%nprocs

          end do ! do j = 1, mpi_taskgrid%nlevs

       end if ! if(debug)

    end if ! if(mpi_procid .eq. mpi_masternode)

    ! Define local variables

    call mpi_interface_waitall()
500 format('MPI_DEFINE_PROCLEVASSIGN (task ID/task level/grid level) :: ', &
         & i6, 2(i9))

    !=====================================================================

  end subroutine mpi_define_proclevassign

  !=======================================================================

  ! SUBROUTINE: 

  ! mpi_define_taskgrid.f90

  ! DESCRIPTION:

  ! This subroutine partitions the horizontal computational grid
  ! relative to the total number of computational cores allocated for
  ! the respective executable; the global FORTRAN mpi_taskgrid_struct
  ! variable is defined accordingly.

  ! INPUT VARIABLES:

  ! * debug; a FORTRAN logical value specifying whether to include
  !   debug information during execution.

  !-----------------------------------------------------------------------

  subroutine mpi_define_taskgrid(debug)

    ! Define variables passed to routine

    logical                                                             :: debug

    ! Define variables computed within routine

    integer                                                             :: cnt_interval
    integer                                                             :: count

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(mpi_taskgrid%nprocs .le. 1) then

       ! Compute local variables

       cnt_interval = mpi_taskgrid%ncoords

       ! Define local variables

       mpi_taskgrid%begin(1) = 1
       mpi_taskgrid%end(1)   = cnt_interval

       ! Check local variable and proceed accordingly

       if(debug) then

          ! Define local variables

          write(6,*) ' '
          write(6,500) 1, mpi_taskgrid%begin(1), mpi_taskgrid%end(1)

       end if ! if(debug)

    end if ! if(mpi_taskgrid%nprocs .le. 1)

    ! Check local variable and proceed accordingly

    if(mpi_taskgrid%nprocs .gt. 1) then
       
       ! Compute local variables

       cnt_interval = mpi_taskgrid%ncoords/mpi_taskgrid%nprocs
    
       ! Define local variables
    
       mpi_taskgrid%begin    = mpi_noproc_assign
       mpi_taskgrid%end      = mpi_noproc_assign
       mpi_taskgrid%begin(1) = 1
       mpi_taskgrid%end(1)   = mpi_taskgrid%begin(1) + cnt_interval

       ! Loop through local variable
    
       do i = 2, mpi_taskgrid%nprocs

          ! Define local variables
       
          mpi_taskgrid%begin(i) = mpi_taskgrid%end(i-1) + 1
          mpi_taskgrid%end(i)   = mpi_taskgrid%begin(i) + cnt_interval

          ! Check local variable and proceed accordingly

          if(mpi_taskgrid%begin(i) .gt. mpi_taskgrid%ncoords) then

             ! Define local variables

             mpi_taskgrid%begin(i)  = mpi_taskgrid%ncoords
             mpi_taskgrid%end(i)    = mpi_taskgrid%begin(i)
             mpi_taskgrid%maxprocid = i
             goto 1000
             
          end if ! if(mpi_taskgrid%begin(i) .gt. mpi_taskgrid%ncoords)
          
          ! Check local variable and proceed accordingly

          if(mpi_taskgrid%end(i) .gt. mpi_taskgrid%ncoords) then

             ! Define local variables

             mpi_taskgrid%end(i)    = mpi_taskgrid%ncoords
             mpi_taskgrid%maxprocid = i
             goto 1000

          end if ! if(mpi_taskgrid%end(i) .gt. mpi_taskgrid%ncoords

       end do ! do i = 2, mpi_taskgrid%nprocs

       ! Define local variables
    
1000   continue

       ! Check local variable and proceed accordingly

       if(mpi_procid .eq. mpi_masternode) then

          ! Check local variable and proceed accordingly

          if(debug) then

             ! Define local variables

             write(6,*) ' '

             ! Loop through local variable

             do i = 1, mpi_taskgrid%nprocs

                ! Check local variable and proceed accordingly

                if(mpi_taskgrid%begin(i) .ne. mpi_masternode .and.         &
                     & mpi_taskgrid%end(i) .ne. mpi_masternode) then

                   ! Define local variables

                   write(6,500) i, mpi_taskgrid%begin(i),                  &
                        & mpi_taskgrid%end(i)

                end if ! if(mpi_taskgrid%begin(i) .ne. mpi_masternode
                       ! .and. mpi_taskgrid%end(i)
                       ! .ne. mpi_masternode)

             end do ! do i = 1, mpi_taskgrid%nprocs

          end if ! if(debug)

       end if ! if(mpi_procid .eq. mpi_masternode)

    end if ! if(mpi_taskgrid%nprocs .gt. 1)

    ! Define local variables

    call mpi_interface_waitall()
500 format('MPI_DEFINE_TASKGRID (task ID/tile min/tile max) :: ', i6,      &
         & 2(i9))

    !=====================================================================

  end subroutine mpi_define_taskgrid

  !=======================================================================

end module mpi_interface
