module obs_to_bufr_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-to-bufr :: obs_to_bufr_interface
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

  use diagnostics_interface
  use kinds_interface
  use namelist_interface
  use observation_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: obs_to_bufr

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! obs_to_bufr.f90

  ! DESCRIPTION:

  ! This subroutine is the driver routine to write observation-types
  ! to the BUFR format.

  !-----------------------------------------------------------------------

  subroutine obs_to_bufr()

    ! Define variables computed within routine

    real(r_kind)                                                        :: time_start

    !=====================================================================

    ! Define local variables

    call diagnostics_time_start(time_start)
    call namelist()

    ! Compute local variables

    call observations()

    ! Define local variables

    call diagnostics_time_stop(time_start)

    !=====================================================================

  end subroutine obs_to_bufr

  !=======================================================================

end module obs_to_bufr_interface
