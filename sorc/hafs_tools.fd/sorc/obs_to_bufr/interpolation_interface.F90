module interpolation_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! obs-to-bufr :: interpolation_interface
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

  use constants_interface
  use kinds_interface
  use namelist_interface
  use slint
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: interpolation_interface_init
  interface interpolation_interface_init
     module procedure init_slint
  end interface interpolation_interface_init

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE: 

  ! init_slint.f90

  ! DESCRIPTION: 

  ! This subroutine initializes and computes the remapping
  ! coefficients and nearest neighbor locations using the geographical
  ! locations passed using the source (src) and destination (dst)
  ! grid_struct FORTRAN structures; all geographical locations are
  ! assumed to be units of degrees.

  ! INPUT VARIABLES:

  ! * src_grid; a FORTRAN grid_struct variable containing the source
  !   grid information; all geographical location units are degrees.

  ! * dst_grid; a FORTRAN grid_struct variable containing the
  !   destination grid information; all geographical location units
  !   are degrees.

  ! * remap_grid; a FORTRAN interp_xy_slint_struct variable.

  ! OUTPUT VARIABLES:

  ! * remap_grid; a FORTRAN interp_xy_slint_struct containing the
  !   remapping coefficients and nearest neighbor indices determined
  !   from the source and destination grid_struct FORTRAN structures.

  !-----------------------------------------------------------------------

  subroutine init_slint(src_grid,dst_grid,remap_grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: src_grid
    type(grid_struct)                                                   :: dst_grid
    type(slint_struct)                                                  :: remap_grid

    ! Define variables computed within routine

    type(grid)                                                          :: slint_grid
    real(r_kind),               dimension(:,:),             allocatable :: grid1
    real(r_kind),               dimension(:,:),             allocatable :: grid2

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid1))                                             &
         & allocate(grid1(src_grid%ncoords,2))
    if(.not. allocated(grid2))                                             &
         & allocate(grid2(dst_grid%ncoords,2))

    ! Define local variables

    grid1(:,1) = src_grid%lat*deg2rad
    grid1(:,2) = src_grid%lon*deg2rad
    grid2(:,1) = dst_grid%lat*deg2rad
    grid2(:,2) = dst_grid%lon*deg2rad

    ! Compute local variables

    call slint_init_compute(grid1,src_grid%ncoords,grid2,                  &
         & dst_grid%ncoords,slint_grid)

    ! Define local variables

    remap_grid%coeffs = slint_grid%coeffs
    remap_grid%nn     = slint_grid%nn

    ! Deallocate memory for local variables

    if(allocated(grid2)) deallocate(grid2)
    if(allocated(grid1)) deallocate(grid1)

    !=====================================================================

  end subroutine init_slint

  !=======================================================================

end module interpolation_interface
