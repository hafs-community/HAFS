module time_methods_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-to-bufr :: time_methods_interface
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

  ! Review the README, within the top-level directory, which provides
  ! relevant instructions and (any) references cited by algorithms
  ! within this software suite.

  !=======================================================================

  ! Define associated modules and subroutines

  use kinds_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: time_methods_date_attributes
  public :: time_methods_julian_day

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! time_methods_date_attributes.f90

  ! DESCRIPTION:

  ! This subroutine returns the integer attributes from the user
  ! specified integer defining the date to be parsed.

  ! INPUT VARIABLES:

  ! * dateint; a FORTRAN integer defining the date to be parsed.

  ! * yyyy; a FORTRAN integer value to specify the year for the parsed
  !   date.

  ! * mm; a FORTRAN integer value to specify the month for the parsed
  !   date.

  ! * dd; a FORTRAN integer value to specify the day for the parsed
  !   date.

  ! * hh; a FORTRAN integer value to specify the hour of day for the
  !   parsed date.

  ! * nn; a FORTRAN integer value to specify the minute of hour for
  !   the parsed date.

  ! * ss; a FORTRAN integer value to specify the second of minute for
  !   the parsed date.


  ! OUTPUT VARIABLES:

  ! * yyyy; a FORTRAN integer value specifying the year for the parsed
  !   date.

  ! * mm; a FORTRAN integer value specifying the month for the parsed
  !   date.

  ! * dd; a FORTRAN integer value specifying the day for the parsed
  !   date.

  ! * hh; a FORTRAN integer value specifying the hour of day for the
  !   parsed date.

  ! * nn; a FORTRAN integer value specifying the minute of hour for
  !   the parsed date.

  ! * ss; a FORTRAN integer value specifying the second of minute for
  !   the parsed date.

  !-----------------------------------------------------------------------

  subroutine time_methods_date_attributes(datestr,yyyy,mm,dd,hh,nn,ss)

    ! Define variables passed to routine

    character(len=19)                                                   :: datestr 
    integer                                                             :: yyyy
    integer                                                             :: mm
    integer                                                             :: dd
    integer                                                             :: hh
    integer                                                             :: nn
    integer                                                             :: ss

    !=====================================================================

    ! Define local variables

    read(datestr(1:4),  '(i4.4)') yyyy
    read(datestr(6:7),  '(i2.2)') mm
    read(datestr(9:10), '(i2.2)') dd
    read(datestr(12:13),'(i2.2)') hh
    read(datestr(15:16),'(i2.2)') nn
    read(datestr(18:19),'(i2.2)') ss

    !=====================================================================

  end subroutine time_methods_date_attributes

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

  ! * hh; a FORTRAN integer value specifying the Gregorian calendar
  !   hour of day.

  ! * nn; a FORTRAN integer value specifying the Gregorian calendar
  !   minute of hour.

  ! * ss; a FORTRAN integer value specifying the Gregorian calendar
  !   second of minute.

  ! * julian_day; a FORTRAN 8-byte real value to define the Julian
  !   date.

  ! OUTPUT VARIABLES:

  ! * julian_day; a FORTRAN 8-byte real value specifying the Julian
  !   date.

  !-----------------------------------------------------------------------

  subroutine time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,julian_day)

    ! Define variables passed to routine

    real(r_double)                                                      :: julian_day
    integer                                                             :: yyyy
    integer                                                             :: mm
    integer                                                             :: dd
    integer                                                             :: hh
    integer                                                             :: nn
    integer                                                             :: ss

    ! Define variables computed within routine

    real(r_double)                                                      :: frac_day

    !=====================================================================

    ! Compute local variables

    julian_day = dble(dd - 32075 + 1461*(yyyy + 4800 + (mm - 14)/12)/4 +   &
         & 367*(mm - 2 - (mm - 14)/12*12)/12 - 3*((yyyy + 4900 +           &
         & (mm - 14)/12)/100)/4)

    ! Define local variables

    frac_day   = julian_day*86400.0
    frac_day   = frac_day + dble(hh*3600.0)
    frac_day   = frac_day + dble(nn*60.0)
    frac_day   = frac_day + dble(ss)
    julian_day = frac_day/86400.0

    !=====================================================================

  end subroutine time_methods_julian_day

  !=======================================================================

end module time_methods_interface
