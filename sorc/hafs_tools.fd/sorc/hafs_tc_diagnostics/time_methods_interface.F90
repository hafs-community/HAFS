module time_methods_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: time_methods_interface
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
  public :: time_methods_date_format

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! time_methods_date_format.f90

  ! DESCRIPTION:

  ! This subroutine ingests a character string specifying a timestamp
  ! and returns a formatted timestamp string.

  ! INPUT VARIABLES:

  ! * datestr; a FORTRAN character string specifying the timestamp to
  !   be formatted; format is (assuming UNIX convention)
  !   'ccyymmddHHMMSS'.

  ! * tsstr; a FORTRAN character string variable to contain the
  !   formatted timestamp.

  ! OUTPUT VARIABLES:

  ! * tsstr; a FORTRAN character string variable containing the
  !   formatted timestamp.

  !-----------------------------------------------------------------------

  subroutine time_methods_date_format(datestr,tsstr)

    ! Define variables passed to routine

    character(len=19)                                                   :: tsstr
    character(len=14)                                                   :: datestr

    ! Define variables computed within routine

    integer                                                             :: yyyy
    integer                                                             :: mm
    integer                                                             :: dd
    integer                                                             :: hh
    integer                                                             :: nn
    integer                                                             :: ss

    !=====================================================================

    ! Define local variables

    read(datestr(1:4),  '(i4.4)') yyyy
    read(datestr(5:6),  '(i2.2)') mm
    read(datestr(7:8),  '(i2.2)') dd
    read(datestr(9:10), '(i2.2)') hh
    read(datestr(11:12),'(i2.2)') nn
    read(datestr(13:14),'(i2.2)') ss
    write(tsstr,500) yyyy, mm, dd, hh, nn, ss
500 format(i4.4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2,':',i2.2)

    !=====================================================================

  end subroutine time_methods_date_format

  !=======================================================================

end module time_methods_interface
