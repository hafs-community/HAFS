module json_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: json_interface
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

  use fson
  use fson_value_m
  use kinds_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: json_interface_read
  interface json_interface_read
     module procedure read_fv3var
     module procedure read_pattern
  end interface json_interface_read

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! json_nrecs.f90

  ! DESCRIPTION:

  ! This subroutine defines the number of records within the external
  ! JSON formatted file specified by the user using the FSON API
  ! utilities.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   a JSON formatted file.

  !-----------------------------------------------------------------------

  subroutine json_nrecs(filename,json_size)

    ! Define variables passed to routine

    character(len=500)                                                  :: filename
    integer                                                             :: json_size

    ! Define variables computed within routine
    
    type(fson_value),                                           pointer :: json_file

    !=====================================================================

    ! Define local variables

    json_file => fson_parse(trim(adjustl(filename)))
    json_size = fson_value_count(json_file)
    call fson_destroy(json_file)
    
    !=====================================================================

  end subroutine json_nrecs

  !=======================================================================

  ! SUBROUTINE:

  ! read_fv3var.f90

  ! DESCRIPTION:

  ! This subroutine parses a JSON formatted file and defines elements
  ! of the json_fv3var_struct variable necessary to parse the JSON
  ! formatted variable table (vtable) specfied by the user.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   a JSON formatted file.

  ! * json; a FORTRAN json_fv3var_struct variable.

  ! OUTPUT VARIABLES:

  ! * json; a FORTRAN json_fv3var_struct variable containing the
  !   contents of the user JSON formatted file.

  !-----------------------------------------------------------------------

  subroutine read_fv3var(filename,json)

    ! Define variables passed to routine

    type(json_fv3var_struct),   dimension(:),               allocatable :: json
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(fson_value),                                           pointer :: json_file  
    type(fson_value),                                           pointer :: json_item
    integer                                                             :: json_size
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call json_nrecs(filename,json_size)

    ! Allocate memory for local variables

    if(.not. allocated(json)) allocate(json(json_size))

    ! Define local variables
    
    json_file => fson_parse(trim(adjustl(filename)))

    ! Loop through local variable

    do i = 1, json_size

       ! Define local variables

       json_item => fson_value_get(json_file,i)
       call fson_get(json_item,'clip',json(i)%clip)
       call fson_get(json_item,'gridtype',json(i)%gridtype)
       call fson_get(json_item,'interp_bilinear',json(i)%interp_bilinear)
       call fson_get(json_item,'interp_nrstnghbr',                         &
            & json(i)%interp_nrstnghbr)
       call fson_get(json_item,'levtype',json(i)%levtype)       
       call fson_get(json_item,'variable_name',json(i)%variable_name)
       call fson_get(json_item,'z_staggered',json(i)%z_staggered)
       
    end do ! do i = 1, json_size

    ! Define local variables

    call fson_destroy(json_file)

    !=====================================================================

  end subroutine read_fv3var

  !=======================================================================

  ! SUBROUTINE:

  ! read_pattern.f90

  ! DESCRIPTION:

  ! This subroutine parses a JSON formatted file and defines elements
  ! of the pattern_json_struct variable necessary to parse the JSON
  ! formatted variable table (vtable) specfied by the user.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   a JSON formatted file.

  ! * json; a FORTRAN pattern_json_struct variable.

  ! OUTPUT VARIABLES:

  ! * json; a FORTRAN pattern_json_struct variable containing the
  !   contents of the user JSON formatted file.

  !-----------------------------------------------------------------------

  subroutine read_pattern(filename,json)

    ! Define variables passed to routine

    type(json_pattern_struct),  dimension(:),               allocatable :: json
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(fson_value),                                           pointer :: json_file  
    type(fson_value),                                           pointer :: json_item
    integer                                                             :: json_size
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call json_nrecs(filename,json_size)

    ! Allocate memory for local variables

    if(.not. allocated(json)) allocate(json(json_size))
    
    ! Define local variables

    json_file => fson_parse(trim(adjustl(filename)))

    ! Loop through local variable

    do i = 1, json_size

       ! Define local variables

       json_item => fson_value_get(json_file,i)
       call fson_get(json_item,'clip',json(i)%clip)
       call fson_get(json_item,'levtype',json(i)%levtype)
       call fson_get(json_item,'max_lev',json(i)%max_lev)
       call fson_get(json_item,'min_lev',json(i)%min_lev)
       call fson_get(json_item,'vari_thresh',json(i)%vari_thresh)
       call fson_get(json_item,'variable_name',json(i)%variable_name)
       
    end do ! do i = 1, json_size

    ! Define local variables

    call fson_destroy(json_file)

    !=====================================================================

  end subroutine read_pattern

  !=======================================================================

end module json_interface
