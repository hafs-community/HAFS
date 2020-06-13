module grid_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! post-utils :: grid_methods_interface
  ! Copyright (C) 2019 Henry R. Winterbottom

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

  use gridprojs_interface
  use kinds_interface
  use variable_interface

  ! Define interfaces and attributes for module routines

  implicit none
  private
  public :: grid_methods_define_remap

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! grid_methods_define_remap.f90

  ! DESCRIPTION:

  ! This subroutine is the driver-level routine to define the
  ! remapping grid in accordance with the user-specified
  ! configuration.

  ! INPUT VARIABLES:

  ! * remap; a FORTRAN remap_struct variable.

  ! OUTPUT VARIABLES:

  ! * remap; a FORTRAN remap_struct variable containing the
  !   initialized and defined latitude and longitude arrays.

  !-----------------------------------------------------------------------

  subroutine grid_methods_define_remap(remap)

    ! Define variables passed to routine

    type(remap_struct)                                                  :: remap

    !=====================================================================

    ! Compute local variables

    if(remap%is_global)   call remap_global_define(remap)
    if(remap%is_regional) call remap_regional_define(remap)

    !=====================================================================

  end subroutine grid_methods_define_remap

  !=======================================================================

  ! SUBROUTINE:

  ! remap_global_define.f90

  ! DESCRIPTION:

  ! This subroutine defines the attributes of a FORTRAN remap_struct
  ! variable in accordance with the user specifications for a
  ! global-type grid.

  ! INPUT VARIABLES:

  ! * remap; a FORTRAN remap_struct variable.

  ! OUTPUT VARIABLES:

  ! * remap; a FORTRAN remap_struct variable containing the
  !   initialized and defined latitude and longitude arrays.

  !-----------------------------------------------------------------------

  subroutine remap_global_define(remap)

    ! Define variables passed to routine

    type(remap_struct)                                                  :: remap

    ! Define variables computed within routine

    type(specgrids_grid)                                                :: specgrid

    !=====================================================================

    ! Define local variables

    specgrid%ntrunc = remap%jcap

    ! Compute local variables

    call specgrids_compute(specgrid)

    ! Define local variables

    remap%nx   = specgrid%nlons
    remap%ny   = specgrid%nlats
    call variable_interface_setup_struct(remap)
    remap%lats = specgrid%lats
    remap%lons = specgrid%lons

    ! Deallocate memory for local variables

    call specgrids_cleanup(specgrid)

    !=====================================================================

  end subroutine remap_global_define

  !=======================================================================

  ! SUBROUTINE:

  ! remap_regional_define.f90

  ! DESCRIPTION:

  ! This subroutine defines the attributes of a FORTRAN remap_struct
  ! variable in accordance with the user specifications for a
  ! regional-type grid.

  ! INPUT VARIABLES:

  ! * remap; a FORTRAN remap_struct variable.

  ! OUTPUT VARIABLES:

  ! * remap; a FORTRAN remap_struct variable containing the
  !   initialized and defined latitude and longitude arrays.

  !-----------------------------------------------------------------------

  subroutine remap_regional_define(remap)

    ! Define variables passed to routine

    type(remap_struct)                                                  :: remap

    ! Define variables computed within routine

    real(r_kind)                                                        :: lat_max
    real(r_kind)                                                        :: lat_min 
    real(r_kind)                                                        :: lon_max
    real(r_kind)                                                        :: lon_min 
    integer                                                             :: idx

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    remap%lat_max = min(remap%cen_lat + remap%lat2,90.0)
    remap%lat_min = max(remap%cen_lat + remap%lat1,-90.0)
    remap%lon_max = min(remap%cen_lon + remap%lon2,180.0)
    remap%lon_min = max(remap%cen_lon + remap%lon1,-180.0)
    remap%nx      = (remap%lon_max - remap%lon_min)/remap%dlon + 1
    remap%ny      = (remap%lat_max - remap%lat_min)/remap%dlat + 1
    call variable_interface_setup_struct(remap)
    idx           = 0

    ! Loop through local variable

    do j = 1, remap%ny

       ! Loop through local variable

       do i = 1, remap%nx

          ! Define local variables

          idx             = idx + 1
          remap%lats(idx) = remap%lat_min + (j - 1)*remap%dlat
          remap%lons(idx) = remap%lon_min + (i - 1)*remap%dlon

       end do ! do i = 1, remap%nx

    end do ! do j = 1, remap%ny

    !=====================================================================

  end subroutine remap_regional_define

  !=======================================================================

end module grid_methods_interface
