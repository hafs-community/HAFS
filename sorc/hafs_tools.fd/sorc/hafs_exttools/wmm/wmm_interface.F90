module wmm_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! wmm :: wmm_interface
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
  public :: wmm_compute
  public :: wmm_struct

  ! Define local variables

  integer,                                                    parameter :: wmm_intkind  = 4
  integer,                                                    parameter :: wmm_realkind = 4  
  type wmm_struct
     character(len=500)                                                 :: coeff_filepath
     real(wmm_realkind)                                                 :: alt
     real(wmm_realkind)                                                 :: dec
     real(wmm_realkind)                                                 :: dip
     real(wmm_realkind)                                                 :: glat
     real(wmm_realkind)                                                 :: glon
     real(wmm_realkind)                                                 :: gv
     real(wmm_realkind)                                                 :: ti
     real(wmm_realkind)                                                 :: year
  end type wmm_struct
  integer(wmm_intkind)                                                  :: maxdeg       = 12

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! wmm_compute.f90

  ! DESCRIPTION:

  ! This subroutine computes the attributes of the geomagnetic field
  ! using the World Magnetic Model (WMM).

  ! INPUT VARIABLES:

  ! * wmm; a FORTRAN wmm_struct variable containing the year for which
  !   to derive the geomagnetic field attributes (year), the filepath
  !   for the WMM coefficients (coeff_filepath), the reference
  !   geographical location (described by glat and glon) and altitude
  !   relative to the Earth's surface (alt).

  ! OUTPUT VARIABLES:

  ! * wmm; a FORTRAN wmm_struct variable containing the geomagnetic
  !   field attributes for declination (magnetic variation; dec),
  !   inclination (dip), the total intensity (ti), and the grid
  !   variation (gv).

  !-----------------------------------------------------------------------

  subroutine wmm_compute(wmm)

    ! Define variables passed to routine

    type(wmm_struct)                                                    :: wmm

    !=====================================================================

    ! Define local variables

    call geomag(maxdeg,wmm%year,wmm%coeff_filepath)
    
    ! Compute local variables

    call geomg1(wmm%alt,wmm%glat,wmm%glon,wmm%year,wmm%dec,wmm%dip,        &
         & wmm%ti,wmm%gv,wmm%year)

    !=====================================================================

  end subroutine wmm_compute

  !=======================================================================
  
end module wmm_interface
