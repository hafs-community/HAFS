module grid_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tempdrop-sonde :: grid_methods_interface
  ! Copyright (C) 2017 Henry R. Winterbottom

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
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: grid_methods_gcgeo

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! grid_methods_gcgeo.f90

  ! DESCRIPTION:

  ! This subroutine computes the longitude and latitude (e.g.,
  ! geographical) coordinates, assuming a great-circle distance, of an
  ! advected position provided an initial geographical position
  ! (defined by the longitude and latitude coordinates) and a distance
  ! and heading.

  ! REFERENCES:

  ! http://www.edwilliams.org/avform.htm

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the initial
  !   geographical position (lon and lat) and the heading (head;
  !   degrees) and distance (dist; meters); all geographical position
  !   units are assumed to be degrees.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the newly
  !   computed geographical position (lon and lat).

  !-----------------------------------------------------------------------

  subroutine grid_methods_gcgeo(grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid

    ! Define variables computed within routine

    real(r_kind)                                                        :: lon
    real(r_kind)                                                        :: lat
    real(r_kind)                                                        :: dlon
    real(r_kind)                                                        :: head
    real(r_kind)                                                        :: dist

    !=====================================================================

    ! Define local variables

    lat  = grid%lat*deg2rad
    lon  = grid%lon*deg2rad
    head = grid%head*deg2rad
    dist = (grid%dist/rearth_equator)

    ! Compute local variables

    lat  = asin(sin(lat)*cos(dist) + cos(lat)*sin(dist)*cos(head))
    dlon = atan2(sin(head)*sin(dist)*cos(lat),cos(dist) - sin(lat)*        &
         & sin(lat))
    lon  = mod(lon - dlon + pi, 2.0*pi) - pi

    ! Define local variables

    grid%lon = lon*rad2deg
    grid%lat = lat*rad2deg

    !=====================================================================

  end subroutine grid_methods_gcgeo

  !=======================================================================

end module grid_methods_interface
