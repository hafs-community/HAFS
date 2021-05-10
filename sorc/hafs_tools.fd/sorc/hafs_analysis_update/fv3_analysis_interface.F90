module fv3_analysis_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: fv3_analysis_interface
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

  ! Define associate modules and subroutines

  use fileio_interface
  use kinds_interface
  use namelist_interface
  use update_state_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: fv3_analysis

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! fv3_analysis.f90

  ! DESCRIPTION:

  ! This subroutine is the driver-level routine for all fv3_analysis
  ! routines.

  !-----------------------------------------------------------------------

  subroutine fv3_analysis()

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(is_bcupdate) call nest_bcupdate()
    if(is_merge)    call nest_parent_merge()
    
    !=====================================================================

  end subroutine fv3_analysis

  !=======================================================================

  ! SUBROUTINE:

  ! nest_bcupdate.f90

  ! DESCRIPTION:

  ! This subroutine relaxes the data-assimilation derived prognostic
  ! variable increment values to zero as specified by the user
  ! relaxation mask criteria.

  !-----------------------------------------------------------------------

  subroutine nest_bcupdate()

    ! Define variables computed within routine

    type(json_fv3var_struct),   dimension(:),               allocatable :: json_fv3var
    type(fv3grid_struct)                                                :: fv3grid(3)
    type(fv3var_struct)                                                 :: fv3var_anl
    type(fv3var_struct)                                                 :: fv3var_bkgrd
    
    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================

    ! Define local variables

    call fileio_interface_read(json_fv3var)
    call fileio_interface_read(fv3_gridspec_filename,                      &
         & fv3_atmos_static_nest_filename,fv3grid)

    ! Compute local variables

    call update_state_init(fv3grid)

    ! Loop through local variable

    do i = 1, size(json_fv3var)

       ! Define local variables

       call fileio_interface_read(fv3_analysis_filename,fv3var_anl,        &
            & json_fv3var(i))
       call fileio_interface_read(fv3_background_filename,fv3var_bkgrd,    &
            & json_fv3var(i))

       ! Compute local variables       
    
       call update_state_nest_lbcs(fv3var_anl,fv3var_bkgrd,                &
            & fv3grid(json_fv3var(i)%gridtype),json_fv3var(i))

       ! Define local variables

       call fileio_interface_write(fv3_analysis_filename,fv3var_anl,       &
            & json_fv3var(i))

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(fv3var_anl)
       call variable_interface_cleanup_struct(fv3var_bkgrd)
       
    end do ! do i = 1, size(json_fv3var)
       
    ! Deallocate memory for local variables

    if(allocated(json_fv3var)) deallocate(json_fv3var)
    
    ! Loop through local variable

    do i = 1, size(fv3grid)

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(fv3grid(i))
       
    end do ! do i = 1, size(fv3grid)

    !=====================================================================
    
  end subroutine nest_bcupdate

  !=======================================================================

  ! SUBROUTINE:

  ! nest_parent_merge.f90

  ! DESCRIPTION:

  ! This subroutine merges a FV3 nested-domain into the respective
  ! parent tile within which the nested-domain is embedded.

  !-----------------------------------------------------------------------

  subroutine nest_parent_merge()

    ! Define variables computed within routine

    type(json_fv3var_struct),   dimension(:),               allocatable :: json_fv3var
    type(fv3grid_struct)                                                :: fv3grid_nest(3)
    type(fv3grid_struct)                                                :: fv3grid_parent(3)
    type(interp_struct)                                                 :: interp(3)
    type(fv3var_struct)                                                 :: fv3var_nest
    type(fv3var_struct)                                                 :: fv3var_parent
    type(remappres_struct)                                              :: remappres

    ! Define counting variables

    integer                                                             :: i
  
    !=====================================================================
    
    ! Define local variables

    call fileio_interface_read(json_fv3var)
    call fileio_interface_read(fv3_atmos_static_nest_filename,             &
         & fv3_gridspec_nest_filename,fv3grid_nest)
    call fileio_interface_read(fv3_atmos_static_parent_filename,           &
         & fv3_gridspec_parent_filename,fv3grid_parent)

    ! Compute local variables

    call update_state_init(fv3grid_nest,fv3grid_parent,interp)
       
    ! Loop through local variable

    do i = 1, size(json_fv3var)

       ! Define local variables

       call fileio_interface_read(fv3_var_nest_filename,fv3var_nest,       &
            & json_fv3var(i))
       fv3var_nest%pres   = fv3grid_nest(json_fv3var(i)%gridtype)%pres
       call fileio_interface_read(fv3_var_parent_filename,fv3var_parent,   &
            & json_fv3var(i))
       fv3var_parent%pres = fv3grid_parent(json_fv3var(i)%gridtype)%pres

       ! Compute local variables

       call update_state_nest_parent_merge(fv3var_nest,fv3var_parent,      &
            & fv3grid_nest(json_fv3var(i)%gridtype),                       &
            & fv3grid_parent(json_fv3var(i)%gridtype),json_fv3var(i),      &
            & interp(json_fv3var(i)%gridtype))

       ! Define local variables

       call fileio_interface_write(fv3_var_parent_filename,fv3var_parent,  &
            & json_fv3var(i))
       
       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(fv3var_nest)
       call variable_interface_cleanup_struct(fv3var_parent)
       
    end do ! do i = 1, size(json_fv3var)

    ! Deallocate memory for local variables

    if(allocated(json_fv3var)) deallocate(json_fv3var)
    
    ! Loop through local variable

    do i = 1, size(fv3grid_nest)

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(fv3grid_nest(i))
       call variable_interface_cleanup_struct(fv3grid_parent(i))
       call variable_interface_cleanup_struct(interp(i))
       
    end do ! do i = 1, size(fv3grid_nest)
    
    !=====================================================================

  end subroutine nest_parent_merge
    
  !=======================================================================

end module fv3_analysis_interface
