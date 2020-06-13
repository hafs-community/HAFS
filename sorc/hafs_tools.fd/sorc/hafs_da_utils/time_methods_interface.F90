module time_methods_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! da-utils :: time_methods_interface
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

  use kinds_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: time_methods_julian_day

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! time_methods_julian_day.f90

  ! DESCRIPTION:

  ! This subroutine determines the Julian date corresponding the
  ! Gregorian calendar attributes.

  ! REFERENCES:

  ! http://aa.usno.navy.mil/faq/docs/JD_Formula.php

  ! INPUT VARIABLES:

  ! * yyyy; a FORTRAN integer value specifying the Gregorian calendar
  !   year.

  ! * mm; a FORTRAN integer value specifying the Gregorian calendar
  !   month.

  ! * dd; a FORTRAN integer value specifying the Gregorian calendar
  !   day.

  ! * julian_day; a FORTRAN 8-byte real value to define the Julian
  !   date.

  ! OUTPUT VARIABLES:

  ! * julian_day; a FORTRAN 8-byte real value specifying the Julian
  !   date.

  !-----------------------------------------------------------------------

  subroutine time_methods_julian_day(yyyy,mm,dd,julian_day)

    ! Define variables passed to routine

    real(r_double)                                                      :: julian_day
    integer                                                             :: yyyy
    integer                                                             :: mm
    integer                                                             :: dd

    !=====================================================================

    ! Compute local variables

    julian_day = dble(dd - 32075 + 1461*(yyyy + 4800 + (mm - 14)/12)/4 +   &
         & 367*(mm - 2 - (mm - 14)/12*12)/12 - 3*((yyyy + 4900 +           &
         & (mm - 14)/12)/100)/4)

    !=====================================================================

  end subroutine time_methods_julian_day

  !=======================================================================

end module time_methods_interface
