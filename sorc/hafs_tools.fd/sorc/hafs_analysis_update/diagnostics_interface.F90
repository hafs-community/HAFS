module diagnostics_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: diagnostics_interface
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
  public :: diagnostics_time_start
  public :: diagnostics_time_stop

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! diagnostics_time_start.f90

  ! DESCRIPTION:

  ! This subroutine calls the intrinsic subroutine cpu_time and
  ! defines the elapsed CPU time in seconds.

  ! INPUT VARIABLES:

  ! * time_start; a FORTRAN 4-byte real value.

  ! OUTPUT VARIABLES:

  ! * time_start; a FORTRAN 4-byte real value specifying the elapsed
  !   CPU time; units are seconds.

  !-----------------------------------------------------------------------

  subroutine diagnostics_time_start(time_start) 

    ! Define variables passed to routine

    real(r_kind)                                                        :: time_start

    ! Define variables computed within routine

    character(len=100)                                                  :: cmd

    !=====================================================================

    ! Define local variables

    call get_command_argument(0,cmd)
    call cpu_time(time_start)
    write(6,500) trim(adjustl(cmd))
    write(6,*) ''
500 format('BEGINNING EXECUTION OF PROGRAM ',a,'.')

    !=====================================================================

  end subroutine diagnostics_time_start

  !=======================================================================

  ! SUBROUTINE:

  ! diagnostics_time_stop.f90

  ! DESCRIPTION:

  ! This subroutine computes the total time since the user specified
  ! starting time and prints a message indicating the time in
  ! accordance with the optional variables provided by the user.

  ! INPUT VARIABLES:

  ! * time_start; a FORTRAN 4-byte real specifying the value returned
  !   by subroutine diagnostics_time_start.f90.

  !-----------------------------------------------------------------------

  subroutine diagnostics_time_stop(time_start)

    ! Define variables passed to routine

    real(r_kind)                                                        :: time_start

    ! Define variables computed within routine

    character(len=100)                                                  :: cmd
    real(r_kind)                                                        :: time_stop

    !=====================================================================

    ! Define local variables
    
    call get_command_argument(0,cmd)
    call cpu_time(time_stop)
    write(6,500) (time_stop - time_start)
    write(6,*) ''
    write(6,501) trim(adjustl(cmd))
500 format('Execution time: ', f13.5, ' seconds.')
501 format('COMPLETED EXECUTION OF PROGRAM ',a,'.')

    !=====================================================================

  end subroutine diagnostics_time_stop

  !=======================================================================

end module diagnostics_interface
