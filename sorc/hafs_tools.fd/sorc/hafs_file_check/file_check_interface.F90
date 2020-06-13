module file_check_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! file-check :: file_check_interface
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

  use diagnostics_interface
  use kinds_interface
  use namelist_interface
  use nemsio_module

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: file_check

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! file_check.f90

  ! DESCRIPTION:

  ! This subroutine is the driver-level subroutine to perform input
  ! file status checks.

  !-----------------------------------------------------------------------

  subroutine file_check()

    ! Define variables computed within routine

    character(len=500)                                                  :: fileout
    real(r_kind)                                                        :: time_start
    integer                                                             :: iret

    !=====================================================================

    ! Define local variables

    call diagnostics_time_start(time_start)
    call namelist()
    
    ! Check local variable and proceed accordingly

    if(is_nemsio) call nemsio_file_check(iret)

    ! Define local variables

    fileout = trim(adjustl(datapath))//'file-check.output'
    open(99,file=fileout,form='formatted')
    write(99,500) iret
    close(99)
    call diagnostics_time_stop(time_start)
500 format('FILE_CHECK_STATUS: ',i)

    !=====================================================================

  end subroutine file_check
  
  !=======================================================================

  ! SUBROUTINE:

  ! nemsio_file_check.f90

  ! DESCRIPTION:

  ! This subroutine opens a NEMSIO-formatted file for reading and
  ! returns the I/O status value (e.g., iret).

  ! INPUT VARIABLES:

  ! * iret; a FORTRAN integer valued variable.
  
  ! OUTPUT VARIABLES:

  ! * iret; a FORTRAN integer valued variable containing the I/O
  !   status value.

  !-----------------------------------------------------------------------

  subroutine nemsio_file_check(iret)

    ! Define variables passed to routine

    integer                                                             :: iret

    ! Define variables computed within routine

    type(nemsio_gfile)                                                  :: gfile
    integer(nemsio_intkind)                                             :: nemsio_iret

    !=====================================================================

    ! Define local variables

    call nemsio_init(iret=nemsio_iret)
    call nemsio_open(gfile,trim(adjustl(filename)),'read',                 &
         & iret=nemsio_iret)
    iret = nemsio_iret
    call nemsio_close(gfile,iret=nemsio_iret)

    !=====================================================================

  end subroutine nemsio_file_check
    
  !=======================================================================
  
end module file_check_interface
