module grid_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: grid_methods_interface
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

  ! Define associated modules and subroutines

  use constants_interface
  use kinds_interface
  use math_methods_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: grid_methods_radialdist
  interface grid_methods_radialdist
     module procedure fv3grid_radialdist
  end interface grid_methods_radialdist

  !-----------------------------------------------------------------------

contains

  !=======================================================================
  
  ! SUBROUTINE:

  ! fv3grid_radialdist.f90

  ! DESCRIPTION:

  ! This subroutine computes the radial distance (meters) relative to
  ! a reference location using the Euclidean L2 norm metric.

  ! INPUT VARIABLES:

  ! * fv3grid; a FORTRAN fv3grid_struct variable.

  ! OUTPUT VARIABLES:

  ! * fv3grid; a FORTRAN fv3grid_struct variable containing the
  !   computed Euclidean L2 norm (radial distance) relative to the
  !   specified reference location.

  !-----------------------------------------------------------------------

  subroutine fv3grid_radialdist(fv3grid)
  
    ! Define variables passed to routine

    type(fv3grid_struct)                                                :: fv3grid

    ! Define variables computed within routine

    type(kdtree_struct)                                                 :: kdtree

    !=====================================================================

    ! Define local variables
    
    kdtree%dst_ncoords = 1
    kdtree%nn          = fv3grid%ncoords
    kdtree%src_ncoords = fv3grid%ncoords
    call variable_interface_setup_struct(kdtree)
    kdtree%dst_lat     = fv3grid%clat
    kdtree%dst_lon     = fv3grid%clon
    kdtree%src_lat     = fv3grid%lat
    kdtree%src_lon     = fv3grid%lon
    
    ! Compute local variables
    
    call math_methods_euclidean_norm(kdtree)
    
    ! Define local variables

    fv3grid%radius = sqrt(kdtree%r2dist(:,1))

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(kdtree)
       
    !=====================================================================

  end subroutine fv3grid_radialdist
  
  !=======================================================================
  
end module grid_methods_interface
