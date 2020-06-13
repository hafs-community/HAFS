module post_utils_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! post-utils :: post_utils_interface
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

  use diagnostics_interface
  use fv3_post_interface
  use kinds_interface
  use namelist_interface

  ! Define interfaces and attributes for module routines

  implicit none
  private
  public :: post_utils

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! post_utils.f90

  ! DESCRIPTION:

  ! This subroutine is the driver-level routine for all post_utils
  ! routines.

  !-----------------------------------------------------------------------

  subroutine post_utils()

    ! Define variables computed within routine

    real(r_kind)                                                        :: time_start

    !=====================================================================

    ! Define local variables

    call diagnostics_time_start(time_start)
    call namelist()

    ! Compute local variables

    if(is_fv3) call fv3_post()

    ! Define local variables

    call diagnostics_time_stop(time_start)

    !=====================================================================

  end subroutine post_utils

  !=======================================================================

end module post_utils_interface
