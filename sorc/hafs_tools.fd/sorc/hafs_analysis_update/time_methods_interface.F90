module time_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: time_methods_interface
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

  ! Define associate modules and subroutines

  use kinds_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: time_methods_interface_attrs
  public :: time_methods_interface_dseconds
  public :: time_methods_interface_julian

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! date_attributes.f90

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

  subroutine date_attributes(datestr,yyyy,mm,dd,hh,nn,ss)

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

  end subroutine date_attributes

  !=======================================================================

  ! SUBROUTINE:

  ! julian_day.f90

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

  ! * jday; a FORTRAN 8-byte real value to define the Julian date.

  ! OUTPUT VARIABLES:

  ! * jday; a FORTRAN 8-byte real value specifying the Julian date.

  !-----------------------------------------------------------------------

  subroutine julian_day(yyyy,mm,dd,hh,nn,ss,jday)

    ! Define variables passed to routine

    real(r_double)                                                      :: jday
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

    jday = dble(dd - 32075 + 1461*(yyyy + 4800 + (mm - 14)/12)/4 +         &
         & 367*(mm - 2 - (mm - 14)/12*12)/12 - 3*((yyyy + 4900 +           &
         & (mm - 14)/12)/100)/4)

    ! Define local variables

    frac_day = jday*86400.0
    frac_day = frac_day + dble(hh*3600.0)
    frac_day = frac_day + dble(nn*60.0)
    frac_day = frac_day + dble(ss)
    jday     = frac_day/86400.0

    !=====================================================================

  end subroutine julian_day

  !=======================================================================

  ! SUBROUTINE:

  ! time_methods_interface_attrs.f90

  ! DESCRIPTION:

  ! This subroutine defines the time-attributes in accordance with the
  ! timestamp (e.g., datestr) passed by the user.

  ! INPUT VARIABLES:

  ! * timeinfo; a FORTRAN timeinfo_struct variable containing the
  !   time-stamp (e.g., the 'timestamp' attribute) to be parsed to
  !   obtain the corresponding time-attributes.

  ! OUTPUT VARIABLES:

  ! * timeinfo; a FORTRAN timeinfo_struct variable containing the
  !   time-attributes corresponding to the time-stamp (e.g., the
  !   'timestamp' attribute). 

  !-----------------------------------------------------------------------

  subroutine time_methods_interface_attrs(timeinfo)

    ! Define variables passed to routine

    type(timeinfo_struct)                                               :: timeinfo

    ! Define variables computed within routine

    integer                                                             :: yyyy
    integer                                                             :: mm
    integer                                                             :: dd
    integer                                                             :: hh
    integer                                                             :: nn
    integer                                                             :: ss    

    !=====================================================================

    ! Define local variables

    call date_attributes(timeinfo%timestamp,yyyy,mm,dd,hh,nn,ss)
    timeinfo%yy = yyyy
    timeinfo%mm = mm
    timeinfo%dd = dd
    timeinfo%hh = hh
    timeinfo%nn = nn
    timeinfo%ss = ss
    
    !=====================================================================

  end subroutine time_methods_interface_attrs
  
  !=======================================================================

  ! SUBROUTINE:

  ! time_methods_interface_dseconds.f90

  ! DESCRIPTION:

  ! This subroutine computes the time-offset (in seconds) between two
  ! time-levels specified by the user.

  ! INPUT VARIABLES:

  ! * datestr1; a FORTRAN character string variable of length 19
  !   containing the first time-level; format is (assuming the
  !   standard UNIX convention) %Y-%m-%d_%H:%M:%S.

  ! * datestr2; a FORTRAN character string variable of length 19
  !   containing the second time-level; format is (assuming the
  !   standard UNIX convention) %Y-%m-%d_%H:%M:%S.

  ! * dseconds; a FORTRAN 4-byte real value to specify the time-offset
  !   (in seconds) between the two time-levels specified by the user.

  ! OUTPUT VARIABLES:

  ! * dseconds; a FORTRAN 4-byte real value specifying the time-offset
  !   (in seconds) between the two time-levels specified by the user.

  !-----------------------------------------------------------------------

  subroutine time_methods_interface_dseconds(datestr1,datestr2,dseconds)

    ! Define variables passed to routine

    character(len=19)                                                   :: datestr1
    character(len=19)                                                   :: datestr2
    real(r_kind)                                                        :: dseconds

    ! Define variables computed within routine

    real(r_double)                                                      :: jday1
    real(r_double)                                                      :: jday2
    integer                                                             :: yyyy
    integer                                                             :: mm
    integer                                                             :: dd
    integer                                                             :: hh
    integer                                                             :: nn
    integer                                                             :: ss    

    !=====================================================================

    ! Define local variables

    call date_attributes(datestr1,yyyy,mm,dd,hh,nn,ss)
    call julian_day(yyyy,mm,dd,hh,nn,ss,jday1)
    call date_attributes(datestr2,yyyy,mm,dd,hh,nn,ss)
    call julian_day(yyyy,mm,dd,hh,nn,ss,jday2)

    ! Compute local variables

    dseconds = (real(jday1 - jday2))*86400.0

    !=====================================================================

  end subroutine time_methods_interface_dseconds

  !=======================================================================

  ! SUBROUTINE:

  ! time_methods_interface_julian.f90

  ! DESCRIPTION:

  ! This subroutine computes the Julian date corresponding to the
  ! specified time-stamp.

  ! INPUT VARIABLES:

  ! * datestr; a FORTRAN character string variable of length 19
  !   containing the time-stamp; format is (assuming the standard UNIX
  !   convention) %Y-%m-%d_%H:%M:%S.

  ! * jday; a FORTRAN 4-byte real value to contain the Julian date.

  ! OUTPUT VARIABLES:

  ! * jday; a FORTRAN 4-byte real value to containing the Julian date.

  !-----------------------------------------------------------------------

  subroutine time_methods_interface_julian(datestr,jday)

    ! Define variables passed to routine

    character(len=19)                                                   :: datestr
    real(r_double)                                                      :: jday

    ! Define variables computed within routine

    integer                                                             :: yyyy
    integer                                                             :: mm
    integer                                                             :: dd
    integer                                                             :: hh
    integer                                                             :: nn
    integer                                                             :: ss    

    !=====================================================================

    ! Define local variables

    call date_attributes(datestr,yyyy,mm,dd,hh,nn,ss)
    call julian_day(yyyy,mm,dd,hh,nn,ss,jday)

    !=====================================================================

  end subroutine time_methods_interface_julian

  !=======================================================================

end module time_methods_interface
