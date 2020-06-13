module gridtrans_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! gridtrans :: gridtrans_interface
  ! Copyright (C) 2020 Henry R. Winterbottom

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

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: fv3_gridtrans_struct
  public :: gridtrans_intkind
  public :: gridtrans_realkind
  public :: gridtrans_spval
  public :: windrotate_cleanup
  public :: windrotate_setup
  public :: windrotate_struct
  
  ! Define local variables

  integer,                                                    parameter :: gridtrans_intkind  = 4
  integer,                                                    parameter :: gridtrans_realkind = 4
  type windrotate_struct
     real(gridtrans_realkind),  dimension(:,:),             allocatable :: cang
     real(gridtrans_realkind),  dimension(:,:),             allocatable :: cangu
     real(gridtrans_realkind),  dimension(:,:),             allocatable :: cangv
     real(gridtrans_realkind),  dimension(:,:),             allocatable :: sang
     real(gridtrans_realkind),  dimension(:,:),             allocatable :: sangu
     real(gridtrans_realkind),  dimension(:,:),             allocatable :: sangv
     integer(gridtrans_intkind)                                         :: ncoords
     integer(gridtrans_intkind)                                         :: nx
     integer(gridtrans_intkind)                                         :: ny
  end type windrotate_struct      ! type windrotate_struct
  type fv3_gridtrans_struct
     real(gridtrans_realkind),  dimension(:,:),             allocatable :: lat     
     real(gridtrans_realkind),  dimension(:,:),             allocatable :: latt
     real(gridtrans_realkind),  dimension(:,:),             allocatable :: lon     
     real(gridtrans_realkind),  dimension(:,:),             allocatable :: lont
     real(gridtrans_realkind),  dimension(:),               allocatable :: lat_in
     real(gridtrans_realkind),  dimension(:),               allocatable :: lat_out
     real(gridtrans_realkind),  dimension(:),               allocatable :: lon_in
     real(gridtrans_realkind),  dimension(:),               allocatable :: lon_out
     real(gridtrans_realkind)                                           :: grid_ratio
     real(gridtrans_realkind)                                           :: lat_cen
     real(gridtrans_realkind)                                           :: lon_cen
     integer(gridtrans_intkind)                                         :: ncoords
     integer(gridtrans_intkind)                                         :: nx
     integer(gridtrans_intkind)                                         :: ny
  end type fv3_gridtrans_struct   ! type fv3_gridtrans_struct
  real(gridtrans_realkind),                                   parameter :: gridtrans_spval    = huge(1.0)

  !-----------------------------------------------------------------------

contains
  
  !=======================================================================

  ! SUBROUTINE: 

  ! windrotate_cleanup.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! windrotate_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN windrotate_struct variable.

  !-----------------------------------------------------------------------

  subroutine windrotate_cleanup(grid)

    ! Define variables passed to routine

    type(windrotate_struct)                                             :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%cang))  deallocate(grid%cang)
    if(allocated(grid%cangu)) deallocate(grid%cangu)
    if(allocated(grid%cangv)) deallocate(grid%cangv)
    if(allocated(grid%sang))  deallocate(grid%sang)
    if(allocated(grid%sangu)) deallocate(grid%sangu)
    if(allocated(grid%sangv)) deallocate(grid%sangv)
    
    !=====================================================================

  end subroutine windrotate_cleanup
  
  !=======================================================================

  ! SUBROUTINE: 

  ! windrotate_setup.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! windrotate_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN windrotate_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN windrotate_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine windrotate_setup(grid)

    ! Define variables passed to routine

    type(windrotate_struct)                                             :: grid

    !=====================================================================

    ! Define local variables
  
    grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%cang))                                         &
         & allocate(grid%cang(grid%nx,grid%ny))
    if(.not. allocated(grid%cangu))                                        &
         & allocate(grid%cangu(grid%nx,(grid%ny+1)))
    if(.not. allocated(grid%cangv))                                        &
         & allocate(grid%cangv((grid%nx+1),grid%ny))
    if(.not. allocated(grid%sang))                                         &
         & allocate(grid%sang(grid%nx,grid%ny))
    if(.not. allocated(grid%sangu))                                        &
         & allocate(grid%sangu(grid%nx,(grid%ny+1)))
    if(.not. allocated(grid%sangv))                                        &
         & allocate(grid%sangv((grid%nx+1),grid%ny))

    ! Define local variables

    grid%cang  = gridtrans_spval
    grid%cangu = gridtrans_spval
    grid%cangv = gridtrans_spval
    grid%sang  = gridtrans_spval
    grid%sangu = gridtrans_spval
    grid%sangv = gridtrans_spval
    
    !=====================================================================

  end subroutine windrotate_setup
  
  !=======================================================================

end module gridtrans_interface
