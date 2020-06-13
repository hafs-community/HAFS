module json_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! post-utils :: json_interface
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
     module procedure read_fv3_glbl
     module procedure read_fv3_rgnl
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

  ! read_fv3_glbl.f90

  ! DESCRIPTION:

  ! This subroutine parses a JSON formatted file and defines elements
  ! of the fv3_json_glbl_struct variable necessary to parse the JSON
  ! formatted variable table (vtable) specfied by the user.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   a JSON formatted file.

  ! * json_size; a FORTRAN integer to define the size of the
  !   fv3_json_struct variable array (below).

  ! * json; a FORTRAN fv3_json_glbl_struct variable.

  ! OUTPUT VARIABLES:

  ! * json_size; a FORTRAN integer defining the size of the
  !   fv3_json_glbl_struct variable array.

  ! * json; a FORTRAN fv3_json_glbl_struct variable containing the
  !   contents of the user JSON formatted file.

  !-----------------------------------------------------------------------

  subroutine read_fv3_glbl(filename,json_size,json)

    ! Define variables passed to routine

    integer                                                             :: json_size
    type(fv3_json_glbl_struct)                                          :: json(json_size)
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(fson_value),                                           pointer :: json_file  
    type(fson_value),                                           pointer :: json_item
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    json_file => fson_parse(trim(adjustl(filename)))

    ! Loop through local variable

    do i = 1, json_size

       ! Define local variables

       json_item => fson_value_get(json_file,i)
       call fson_get(json_item,'filetype',json(i)%filetype)
       call fson_get(json_item,'intrptype',json(i)%intrptype)
       call fson_get(json_item,'is_z_staggered',json(i)%is_z_staggered)
       call fson_get(json_item,'levtype',json(i)%levtype)
       call fson_get(json_item,'nems_variable_name',                       &
            & json(i)%nems_variable_name)
       call fson_get(json_item,'nems_vcoord_name',                         &
            & json(i)%nems_vcoord_name)
       call fson_get(json_item,'nc_variable_name',json(i)%variable_name)
       
    end do ! do i = 1, json_size

    ! Define local variables

    call fson_destroy(json_file)

    !=====================================================================

  end subroutine read_fv3_glbl

  !=======================================================================

  ! SUBROUTINE:

  ! read_fv3_rgnl.f90

  ! DESCRIPTION:

  ! This subroutine parses a JSON formatted file and defines elements
  ! of the fv3_json_rgnl_struct variable necessary to parse the JSON
  ! formatted variable table (vtable) specfied by the user.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   a JSON formatted file.

  ! * json_size; a FORTRAN integer to define the size of the
  !   fv3_json_struct variable array (below).

  ! * json; a FORTRAN fv3_json_rgnl_struct variable.

  ! OUTPUT VARIABLES:

  ! * json_size; a FORTRAN integer defining the size of the
  !   fv3_json_rgnl_struct variable array.

  ! * json; a FORTRAN fv3_json_rgnl_struct variable containing the
  !   contents of the user JSON formatted file.

  !-----------------------------------------------------------------------

  subroutine read_fv3_rgnl(filename,json_size,json)

    ! Define variables passed to routine

    integer                                                             :: json_size
    type(fv3_json_rgnl_struct)                                          :: json(json_size)
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(fson_value),                                           pointer :: json_file  
    type(fson_value),                                           pointer :: json_item
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    json_file => fson_parse(trim(adjustl(filename)))

    ! Loop through local variable

    do i = 1, json_size

       ! Define local variables

       json_item => fson_value_get(json_file,i)
       call fson_get(json_item,'intrptype',json(i)%intrptype)
       call fson_get(json_item,'is_z_staggered',json(i)%is_z_staggered)
       call fson_get(json_item,'fill_value',json(i)%fill_value)
       call fson_get(json_item,'levtype',json(i)%levtype)
       call fson_get(json_item,'remap_variable_name',                      &
            & json(i)%remap_variable_name)
       call fson_get(json_item,'units',json(i)%units)
       call fson_get(json_item,'variable_name',json(i)%variable_name)
       
    end do ! do i = 1, json_size

    ! Define local variables

    call fson_destroy(json_file)

    !=====================================================================

  end subroutine read_fv3_rgnl

  !=======================================================================

end module json_interface
