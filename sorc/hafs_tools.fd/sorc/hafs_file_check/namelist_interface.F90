module namelist_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! file-check :: namelist_interface
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

  !-----------------------------------------------------------------------

  ! DESCRIPTION (alphabetized):

  ! * datapath; a FORTRAN character string specifying the full-path to
  !   the directory to contain output files written by the respective
  !   routines.

  ! * debug; a FORTRAN logical value specifying whether to include
  !   debug information during execution.

  ! * filename; a FORTRAN character string specifying the input file
  !   name path for the file to be examined.

  ! * is_nemsio; a FORTRAN logical variable specifying whether the
  !   input file is a NEMSIO format.
  
  !-----------------------------------------------------------------------

  ! Define local variables

  character(len=500)                                                    :: &
       & datapath = './'
  character(len=500)                                                    :: &
       & filename = 'NOT USED' 
  logical                                                               :: &
       & debug = .false.
  logical                                                               :: &
       & is_nemsio = .false.
  namelist /share/    datapath, debug, filename, is_nemsio
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE: 

  ! namelist.f90

  ! DESCRIPTION: 

  ! This subroutine acts as the interface to the namelist file,
  ! provided as 'file-check.input' by the user.

  !-----------------------------------------------------------------------

  subroutine namelist()

    ! Define variables computed within routine

    character(len=500)                                                  :: nml_filename
    logical                                                             :: is_it_there
    integer                                                             :: unit_nml

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================
    
    ! Define local variables

    nml_filename = './file-check.input'
    unit_nml     = 9
    is_it_there  = .false.
    inquire(file = trim(adjustl(nml_filename)),exist = is_it_there)

    ! Check local variable and proceed accordingly

    if(is_it_there) then

       ! Define local variables

       open(file   = trim(adjustl(nml_filename)),                          &
            unit   = unit_nml,                                             &
            status = 'old',                                                &
            form   = 'formatted',                                          &
            action = 'read')
       read(unit_nml,NML = share)
       close(unit_nml)

    else  ! if(is_it_there)

       ! Define local variables

       write(6,500) trim(adjustl(nml_filename))
       
    end if ! if(is_it_there)
       
    ! Define local variables
    
    write(6,*) '&SHARE'
    write(6,*) 'DATAPATH                      = ',                         &
         & trim(adjustl(datapath))
    write(6,*) 'DEBUG                         = ', debug
    write(6,*) 'FILENAME                      = ',                         &
         & trim(adjustl(filename))
    write(6,*) 'IS_NEMSIO                     = ', is_nemsio
    write(6,*) '/'
500 format('NAMELISTPARAMS: ', a, ' not found in the current working ',    &
         & 'directory. ABORTING!!!!')

    !=====================================================================

  end subroutine namelist

  !=======================================================================

end module namelist_interface
