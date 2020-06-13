module json_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: json_interface
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

  !=======================================================================

  ! Define associated modules and subroutines

  use fson
  use fson_value_m
  use kinds_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: json_interface_read
  interface json_interface_read
     module procedure read_grib
  end interface json_interface_read

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! json_count_records.f90

  ! DESCRIPTION:

  ! This subroutine defines the number of records within the external
  ! JSON formatted file specified by the user using the FSON API
  ! utilities.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   a JSON formatted file.

  !-----------------------------------------------------------------------

  subroutine json_count_records(filename,json_size)

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

  end subroutine json_count_records

  !=======================================================================

  ! SUBROUTINE:

  ! read_grib.f90

  ! DESCRIPTION:

  ! This subroutine parses a JSON formatted file and defines elements
  ! of the json_grib_struct variable (json_array) necessary to parse
  ! the JSON formatted variable table (vtable) specfied by the user
  ! and corresponding to the GRIB options within the driver routine.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   a JSON formatted file.

  ! * json; a FORTRAN json_grib_struct variable.

  ! OUTPUT VARIABLES:

  ! * json; a FORTRAN json_grib_struct variable, now of dimension
  !   'json_size' containing all elements required of the GRIB options
  !   within the driver routine.

  !-----------------------------------------------------------------------

  subroutine read_grib(filename,json)

    ! Define variables passed to routine

    type(json_grib_struct),     dimension(:),               allocatable :: json
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(fson_value),                                           pointer :: json_file  
    type(fson_value),                                           pointer :: json_item  
    integer                                                             :: json_size
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call json_count_records(filename,json_size)

    ! Allocate memory for local variables

    if(.not. allocated(json)) allocate(json(json_size))

    ! Define local variables

    json_file => fson_parse(trim(adjustl(filename)))

    ! Loop through local variable

    do i = 1, json_size

       ! Define local variables

       json_item => fson_value_get(json_file,i)
       call fson_get(json_item,'gribvar',json(i)%gribVar)
       call fson_get(json_item,'indicatoroftypeoflevel',                   &
            & json(i)%indicatorOfTypeOfLevel)
       call fson_get(json_item,'indicatorofparameter',                     &
            & json(i)%indicatorOfParameter)
       call fson_get(json_item,'shortname',json(i)%shortName)
       call fson_get(json_item,'levtype',json(i)%levtype)
       
    end do ! do i = 1, json_size

    !=====================================================================

  end subroutine read_grib

  !=======================================================================

end module json_interface
