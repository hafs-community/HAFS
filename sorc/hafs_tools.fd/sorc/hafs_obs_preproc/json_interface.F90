module json_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! obs-preproc :: json_interface
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

  use fson
  use fson_value_m
  use kinds_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: json_interface_nrecs
  public :: json_interface_read
  interface json_interface_read
     module procedure read_bufr_info
  end interface json_interface_read

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! json_interface_nrecs.f90

  ! DESCRIPTION:

  ! This subroutine defines the number of records within the external
  ! JSON formatted file specified by the user using the FSON API
  ! utilities.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   a JSON formatted file.

  !-----------------------------------------------------------------------

  subroutine json_interface_nrecs(filename,json_size)

    ! Define variables passed to routine

    character(len=500)                                                  :: filename
    integer                                                             :: json_size

    ! Define variables computed within routine
    
    type(fson_value),                                           pointer :: json_file

    !=====================================================================

    ! Define local variables

    json_file => fson_parse(trim(adjustl(filename)))
    json_size = fson_value_count(json_file)

    !=====================================================================

  end subroutine json_interface_nrecs

  !=======================================================================

  ! SUBROUTINE:

  ! read_bufr_flag.f90

  ! DESCRIPTION:

  ! This subroutine parses a JSON formatted file and defines the BUFR
  ! record attributes as specified by the user.  

  ! INPUT VARIABLES:

  ! * bufr_info; a FORTRAN bufr_info_struct variable.

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the user specified JSON formatted file containing the BUFR
  !   record attributes.

  ! OUTPUT VARIABLES:

  ! * bufr_info; a FORTRAN bufr_info_struct variable containing the
  !   BUFR record attributes specified by the user.

  !-----------------------------------------------------------------------

  subroutine read_bufr_info(bufr_info)

    ! Define variables passed to routine

    type(bufr_info_struct)                                              :: bufr_info
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(fson_value),                                           pointer :: json_data
    type(fson_value),                                           pointer :: json_item
    integer                                                             :: json_size

    !=====================================================================

    ! Define local variables

    json_data => fson_parse(bufr_info%filename)
    json_item => fson_value_get(json_data,1)
    call fson_get(json_item,'subset',bufr_info%subset)     
    call fson_get(json_item,'obs_type_mass',bufr_info%obs_type_mass)       
    call fson_get(json_item,'obs_type_wind',bufr_info%obs_type_wind) 
    call fson_destroy(json_data)

    !=====================================================================

  end subroutine read_bufr_info

  !=======================================================================

end module json_interface
