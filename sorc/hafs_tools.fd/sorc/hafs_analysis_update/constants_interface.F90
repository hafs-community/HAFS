module constants_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: constants_interface
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

  use kinds_interface

  ! Define interfaces and attributes for module routines

  implicit none
  private
  public :: cearth_equator
  public :: constants_interface_init
  public :: cp_mass
  public :: deg2rad
  public :: dlat_equator
  public :: dlon_equator
  public :: earth_omega
  public :: grav
  public :: grav_std
  public :: ltnthtvpr
  public :: pi
  public :: rad2deg
  public :: rd
  public :: rd_over_cp_mass
  public :: rearth_equator
  public :: rv

  !-----------------------------------------------------------------------

  ! DESCRIPTION (alphabetized):

  ! * cearth_equator; a FORTRAN 4-byte real value specifying the
  !   circumference of the Earth at the equator; units are meters.

  ! * cp_mass; a FORTRAN 4-byte real value specifying the specific
  !   heat capacity of water vapor at constant pressure; units are
  !   Joules per kilogram per Kelvin.

  ! * deg2rad; a FORTRAN 4-byte real value specifying the mathematical
  !   constant to convert from degrees to radians; unitless.

  ! * dlat_equator; a FORTRAN 4-byte real value specifying the
  !   distance between consecutive latitude points at the equator;
  !   this value is for reference since latitude distance values do
  !   not vary from equator to pole; units are meters.

  ! * dlon_equator; a FORTRAN 4-byte real value specifying the
  !   distance between consecutive longitude points at the equator;
  !   units are meters.

  ! * earth_omega; a FORTRAN 4-byte real value specifying the rotation
  !   rate of the Earth; units are radians per second.

  ! * grav; a FORTRAN 4-byte real value containing the acceleration
  !   due to gravity; units are meters per second per second.

  ! * grav_std; a FORTRAN 4-byte real value containing the standard
  ! * gravity; units are meters per second per second.

  ! * ltnthtvpr; a FORTRAN 4-byte real value containing the latent
  !   heat of vaporization; units are Joules per kilogram.

  ! * pi; a FORTRAN 4-byte real value containing the (machine
  !   precision truncated) mathematical constant for atan(-1.0);
  !   unitless.

  ! * rad2deg; a FORTRAN 4-byte real value specifying the mathematical
  !   constant to convert from radians to degrees; unitless.

  ! * rd; a FORTRAN 4-byte real value specifying the gas constant for
  !   dry air; units are Joules per Kelvin per kilogram.

  ! * rd_over_cp_mass; a FORTRAN 4-byte real value designating the
  !   ratio rd/cp_mass.

  ! * rearth_equator; a FORTRAN 4-byte real value specifying the Earth
  !   equatorial radius; units are meters.

  ! * rv; a FORTRAN 4-byte real value specifying the gas constant for
  !   water vapor; units are Joules per Kelvin per kilogram.

  !-----------------------------------------------------------------------

  ! Define local variables 

  real(r_kind)                                                             &
       & cearth_equator
  real(r_kind)                                                             &
       & dlat_equator
  real(r_kind)                                                             &
       & dlon_equator
  real(r_kind),                                               parameter :: &
       & cp_mass = 1004.67_r_kind
  real(r_kind),                                               parameter :: &
       & earth_omega = 7.292115e-5_r_kind
  real(r_kind),                                               parameter :: &
       & grav = 9.80665e0_r_kind
  real(r_kind),                                               parameter :: &
       & grav_std = 9.80665e0_r_kind
  real(r_kind),                                               parameter :: &
       & ltnthtvpr = 2.5e6_r_kind
  real(r_kind),                                               parameter :: &
       & pi = acos(-1.0)
  real(r_kind),                                               parameter :: &
       & deg2rad = pi/180.0_r_kind
  real(r_kind),                                               parameter :: &
       & rad2deg = 1.0/deg2rad
  real(r_kind),                                               parameter :: &
       & rd = 2.8705e2_r_kind
  real(r_kind),                                               parameter :: &
       & rd_over_cp_mass = rd/cp_mass
  real(r_kind),                                               parameter :: &
       & rearth_equator = 6.37813662e6_r_kind 
  real(r_kind),                                               parameter :: &
       & rv = 4.6150e2_r_kind

  !-----------------------------------------------------------------------

contains
  
  !=======================================================================

  ! SUBROUTINE:

  ! constants_interface_init.f90

  ! DESCRIPTION:

  ! This subroutine initializes (e.g., computes) all constant values
  ! with constant value dependencies.

  !-----------------------------------------------------------------------

  subroutine constants_interface_init()

    !=====================================================================

    ! Compute local variables
  
    cearth_equator = 2.0*pi*rearth_equator
    dlat_equator   = cearth_equator/360.0
    dlon_equator   = cearth_equator/360.0

    !=====================================================================

  end subroutine constants_interface_init

  !=======================================================================

end module constants_interface
