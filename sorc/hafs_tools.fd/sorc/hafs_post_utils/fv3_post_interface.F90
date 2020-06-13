module fv3_post_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! post-utils :: fv3_post_interface
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

  use constants_interface
  use fileio_interface
  use grid_methods_interface
  use interpolation_interface
  use kinds_interface
  use namelist_interface
  use variable_interface

  ! Define interfaces and attributes for module routines

  implicit none
  private
  public :: fv3_post

  ! Define local variables

  type(slint_struct)                                                    :: slint
  logical                                                               :: is_init_slint

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! fv3_post.f90

  ! DESCRIPTION:

  ! This is the driver-level routine for all Finite Volume
  ! Cubed-sphere (FV3) post-processing utilities.

  !-----------------------------------------------------------------------

  subroutine fv3_post()

    !=====================================================================

    ! Compute local variables

    if(is_global)   call global_fv3_post()
    if(is_regional) call regional_fv3_post()

    !=====================================================================

  end subroutine fv3_post

  !=======================================================================

  ! SUBROUTINE:

  ! global_fv3_post.f90

  ! DESCRIPTION:

  ! This subroutine is the driver-level routine for all FV3 global
  ! model application post-processing methods.

  !-----------------------------------------------------------------------

  subroutine global_fv3_post()

    ! Define variables computed within routine

    type(fv3_json_glbl_struct), dimension(:),               allocatable :: fv3_json

    !=====================================================================

    ! Define local variables

    call fileio_interface_read(fv3_json_vtable,fv3_json)

    ! Compute local variables

    if(is_upp) call upp_global(fv3_json)

    ! Deallocate memory for local variables

    if(allocated(fv3_json)) deallocate(fv3_json)

    !=====================================================================
    
  end subroutine global_fv3_post

  !=======================================================================

  ! SUBROUTINE:

  ! intrp_slint.f90

  ! DESCRIPTION:

  ! This subroutine performs horizontal interpolation using the SLINT
  ! methodology; if entering this subroutine for the first time, a
  ! check is performed to determine whether the remapping attributes
  ! have already been computed and if so, they are not re-computed; in
  ! addition, this subroutine enables the user to specify an external
  ! file which already contains the remapping attributes such that
  ! they do not need to be computed; this capability (drastically)
  ! improves run-time.

  ! INPUT VARIABLES:

  ! * src_grid; a FORTRAN grid_struct variable containing the source
  !   grid attributes, including the variable to be interpolated.

  ! * dst_grid; a FORTRAN grid_struct variable containing the
  !   destination grid attributes for the geographical locations
  !   (e.g., latitude and longitude).

  ! OUTPUT VARIABLES:

  ! * dst_grid; a FORTRAN grid_struct variable containing the
  !   interpolated variable (within the var attribute).

  !-----------------------------------------------------------------------

  subroutine intrp_slint(src_grid,dst_grid,intrptype)

    ! Define variables passed to routine
    
    type(grid_struct)                                                   :: src_grid
    type(grid_struct)                                                   :: dst_grid
    integer                                                             :: intrptype

    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================

    ! Check local variable and proceed accordingly

    if(is_init_slint) then

       ! Check local variable and proceed accordingly

       if(is_read_remap) then

          ! Define local variables

          call fileio_interface_read(remap_filename,slint)

       end if ! if(is_read_remap)

       ! Check local variable and proceed accordingly

       if(.not. is_read_remap) then

          ! Define local variables
          
          slint%dst_ncoords = dst_grid%ncoords
          slint%dst_nx      = dst_grid%nx
          slint%dst_ny      = dst_grid%ny
          slint%src_ncoords = src_grid%ncoords
          call variable_interface_setup_struct(slint)
          slint%dst_lat     = dst_grid%lat
          slint%dst_lon     = dst_grid%lon
          slint%src_lat     = src_grid%lat
          slint%src_lon     = src_grid%lon

          ! Compute local variables
          
          call interpolation_interface_init(src_grid,dst_grid,slint)

          ! Define local variables
          
          slint%mask = -1.0

          ! Loop through local variable

          do i = 1, slint%dst_ncoords

             ! Check local variable and proceed accordingly

             if((slint%coeffs(1,i) .ne. dble(1.0)) .and.                   &
                  & (slint%coeffs(2,i) .ne. dble(0.0)) .and.               &
                  & (slint%coeffs(3,i) .ne. dble(0.0))) then

                ! Define local variables
                
                slint%mask(i) = 1.0

             end if ! if((slint%coeffs(1,i) .eq. dble(1.0))
                    ! .and. (slint%coeffs(2,i) .eq. dble(0.0))
                    ! .and. (slint%coeffs(3,i) .eq. dble(0.0)))

          end do ! do i = 1, slint%dst_ncoords

          ! Compute local variables

          call interpolation_interface_mask(slint)

          ! Check local variable and proceed accordingly

          if(is_write_remap) call fileio_interface_write(remap_filename,   &
               & slint)

       end if ! if(.not. is_read_remap)

       ! Define local variables
       
       is_init_slint = .false.

    end if ! if(is_init_slint)

    ! Check local variable and proceed accordingly

    if(intrptype .eq. 1) then

       ! Compute local variables

       call interpolation_interface_nnghbr(slint,src_grid%var)

    end if ! if(intrptype .eq. 1)

    ! Check local variable and proceed accordingly

    if(intrptype .eq. 2) then

       ! Compute local variables

       call interpolation_interface_bilinear(slint,src_grid%var)

    end if ! if(intrptype .eq. 2)

    ! Define local variables
    
    dst_grid%var = slint%var
 
    !=====================================================================

  end subroutine intrp_slint

  !=======================================================================

  ! SUBROUTINE:

  ! regional_fv3_post.f90

  ! DESCRIPTION:

  ! This subroutine is the driver-level routine for all FV3 regional
  ! model application post-processing methods.

  !-----------------------------------------------------------------------

  subroutine regional_fv3_post()

    ! Define variables computed within routine

    type(fv3_json_rgnl_struct), dimension(:),               allocatable :: fv3_json

    !=====================================================================

    ! Define local variables

    call fileio_interface_read(fv3_json_vtable,fv3_json)

    ! Compute local variables

    if(is_upp) call upp_regional(fv3_json)

    ! Deallocate memory for local variables

    if(allocated(fv3_json)) deallocate(fv3_json)
    
    !=====================================================================
    
  end subroutine regional_fv3_post

  !=======================================================================

  ! SUBROUTINE:

  ! read_fv3_global_grid.f90

  ! DESCRIPTION:

  ! This subroutine reads FV3 files containing a global grid tiles and
  ! returns the geographical mapping attributes.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the FV3 global
  !   grid geographical mapping attributes.

  !-----------------------------------------------------------------------

  subroutine read_fv3_global_grid(grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid

    ! Define variables computed within routine

    character(len=500)                                                  :: filename(6)

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Loop through local variable

    do i = 1, 6

       ! Define local variables

       filename(i) = fv3_grid_filename(i)

    end do ! do i = 1, 6

    ! Define local variables

    call fileio_interface_read(filename(:),grid)

    !=====================================================================

  end subroutine read_fv3_global_grid

  !=======================================================================

  ! SUBROUTINE:

  ! read_fv3_regional_grid.f90

  ! DESCRIPTION:

  ! This subroutine reads a FV3 file containing a regional grid (i.e.,
  ! single-tile) and returns the geographical mapping attributes.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the FV3 regional
  !   grid geographical mapping attributes.

  !-----------------------------------------------------------------------

  subroutine read_fv3_regional_grid(grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid

    ! Define variables computed within routine

    character(len=500)                                                  :: filename(1)

    !=====================================================================

    ! Define local variables

    filename(1) = fv3_grid_filename(1)
    call fileio_interface_read(filename(:),grid)

    !=====================================================================

  end subroutine read_fv3_regional_grid

  !=======================================================================

  ! SUBROUTINE:

  ! upp_global.f90

  ! DESCRIPTION:

  ! This subroutine post-processes a FV3 global grid (i.e., six
  ! tiles), interpolates the variables to a standard
  ! latitude-longitude grid, and prepares a file to be ingested by the
  ! NCEP UPP.

  ! INPUT VARIABLES:

  ! * fv3_json; a FORTRAN fv3_json_glbl_struct variable containing the
  !   user-specified variables to be interpolated and placed within
  !   the output file to be passed to the NCEP UPP.

  !-----------------------------------------------------------------------

  subroutine upp_global(fv3_json)

    ! Define variables passed to routine

    type(fv3_json_glbl_struct)                                          :: fv3_json(:)

    ! Define variables computed within routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(nemsiometa_struct)                                             :: nemsiometa
    type(nemsiovar_struct)                                              :: nemsiovar
    type(upp_fv3_glblgrd_struct)                                        :: fv3_input
    real(r_kind),               dimension(:,:),             allocatable :: var

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    call fileio_interface_read(fv3_dyns_filename(1),fv3_static_filename,   &
         & fv3_input)
    nemsiometa%dimz     = fv3_input%nz
    nemsiometa%filename = fv3_output_filename
    nemsiometa%ntrunc   = jcap
    nemsiometa%nsoil    = fv3_nsoil
    call fileio_interface_write(nemsiometa,fv3_json)
    call read_fv3_global_grid(src_grid)
    dst_grid%nx         = nemsiometa%dimx
    dst_grid%ny         = nemsiometa%dimy
    call variable_interface_setup_struct(dst_grid)
    dst_grid%lat        = nemsiometa%lat
    dst_grid%lon        = nemsiometa%lon
    nemsiovar%ncoords   = dst_grid%ncoords
    call variable_interface_setup_struct(nemsiovar)

    ! Check local variable and proceed accordingly

    if(is_slint) is_init_slint = .true.

    ! Loop through local variable

    do i = 1, size(fv3_json)

       ! Check local variable and proceed accordingly

       if(fv3_json(i)%filetype .eq. 1) then

          ! Define local variables

          call fileio_interface_read(fv3_json(i),fv3_dyns_filename,var)

       end if ! if(fv3_json(i)%filetype .eq. 1)

       ! Check local variable and proceed accordingly

       if(fv3_json(i)%filetype .eq. 2) then

          ! Define local variables

          call fileio_interface_read(fv3_json(i),fv3_phys_filename,var)

       end if ! if(fv3_json(i)%filetype .eq. 2)

       ! Check local variable and proceed accordingly

       if(debug) write(6,500) trim(adjustl(fv3_json(i)%variable_name)),    &
            & minval(var), maxval(var)

       ! Define local variables
       
       nemsiovar%nems_variable_name = fv3_json(i)%nems_variable_name
       nemsiovar%nems_vcoord_name   = fv3_json(i)%nems_vcoord_name

       ! Loop through local variable

       do j = 1, size(var(1,:))

          ! Define local variables

          src_grid%var = var(:,j)

          ! Compute local variables

          if(is_slint) call intrp_slint(src_grid,dst_grid,                 &
               & fv3_json(i)%intrptype)
          
          ! Define local variables

          nemsiovar%level = size(var(1,:)) - j + 1
          nemsiovar%var   = dst_grid%var
          call fileio_interface_write(nemsiometa,fv3_json,nemsiovar)

       end do ! do j = 1, size(var(1,:))

       ! Deallocate memory for local variables

       if(allocated(var)) deallocate(var)

    end do ! do i = 1, size(fv3_json)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(dst_grid)
    call variable_interface_cleanup_struct(fv3_input)
    call variable_interface_cleanup_struct(nemsiometa)
    call variable_interface_cleanup_struct(nemsiovar)
    call variable_interface_cleanup_struct(slint)
    call variable_interface_cleanup_struct(src_grid)

    ! Define local variables

500 format('UPP_GLOBAL: Variable name/minimum/maximum = ', a10,            &
         & 2(1x,f13.5))

    !=====================================================================

  end subroutine upp_global
  
  !=======================================================================

  ! SUBROUTINE:

  ! upp_regional.f90

  ! DESCRIPTION:

  ! This subroutine post-processes a FV3 regional grid (i.e., single
  ! tile), interpolates the variables to a standard latitude-longitude
  ! grid, and prepares a file to be ingested by the NCEP UPP.

  ! INPUT VARIABLES:

  ! * fv3_json; a FORTRAN fv3_json_struct variable containing the
  !   user-specified variables to be interpolated and placed within
  !   the output file to be passed to the NCEP UPP.

  !-----------------------------------------------------------------------

  subroutine upp_regional(fv3_json)

    ! Define variables passed to routine

    type(fv3_json_rgnl_struct)                                          :: fv3_json(:)

    ! Define variables computed within routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(netcdf_var_struct)                                             :: netcdf_var
    type(netcdf_var_struct)                                             :: netcdf_var_local
    type(remap_struct)                                                  :: remap
    type(upp_fv3_rgnlgrd_struct)                                        :: fv3_input
    type(upp_fv3_rgnlgrd_struct)                                        :: fv3_output

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    remap%is_regional = .true.
    remap%cen_lat     = cen_lat
    remap%cen_lon     = cen_lon
    remap%dlat        = dlat
    remap%dlon        = dlon
    remap%lat1        = lat1
    remap%lat2        = lat2
    remap%lon1        = lon1
    remap%lon2        = lon2
    
    ! Compute local variables

    call grid_methods_define_remap(remap)

    ! Define local variables

    remap%lons         = 360.0 + remap%lons
    call fileio_interface_read(fv3_input_filename,fv3_static_filename,     &
         & fv3_input)
    fv3_output%nx      = remap%nx
    fv3_output%ny      = remap%ny
    fv3_output%nz      = fv3_input%nz
    fv3_output%nt      = fv3_input%nt
    call variable_interface_setup_struct(fv3_output)
    fv3_output%grid_xt = reshape(dble(remap%lons*deg2rad),                 &
         & shape(fv3_output%grid_xt))
    fv3_output%grid_yt = reshape(dble(remap%lats*deg2rad),                 &
         & shape(fv3_output%grid_yt))
    fv3_output%pfull   = fv3_input%pfull
    fv3_output%phalf   = fv3_input%phalf
    fv3_output%ak      = fv3_input%ak
    fv3_output%bk      = fv3_input%bk
    call fileio_interface_write(fv3_output_filename,fv3_output,remap,      &
         & fv3_json)
    call read_fv3_regional_grid(src_grid)
    dst_grid%nx        = remap%nx
    dst_grid%ny        = remap%ny
    call variable_interface_setup_struct(dst_grid)
    dst_grid%lat       = remap%lats
    dst_grid%lon       = remap%lons

    ! Check local variable and proceed accordingly

    if(is_slint) is_init_slint = .true.

    ! Loop through local variable

    do i = 1, size(fv3_json)

       ! Define local variables

       call fileio_interface_read(fv3_json(i),fv3_input_filename,          &
            & netcdf_var)
       netcdf_var_local%nx = remap%nx
       netcdf_var_local%ny = remap%ny
       netcdf_var_local%nz = netcdf_var%nz
       netcdf_var_local%nt = netcdf_var%nt
       call variable_interface_setup_struct(netcdf_var_local)

       ! Loop through local variable

       do j = 1, netcdf_var%nz

          ! Define local variables

          src_grid%var = reshape(netcdf_var%var(:,:,j,1),                  &
               & shape(src_grid%var))

          ! Check local variable and proceed accordingly

          if(is_slint) then 

             ! Compute local variables

             call intrp_slint(src_grid,dst_grid,fv3_json(i)%intrptype)

             ! Define local variables

             where(slint%mask .eq. -1.0) dst_grid%var =                    &
                  & fv3_json(i)%fill_value

          end if ! if(is_slint)

          ! Define local variables

          netcdf_var_local%var(:,:,j,1) = reshape(dst_grid%var,            &
               & shape(netcdf_var_local%var(:,:,j,1)))

       end do ! do j = 1, netcdf_var%nz

       ! Define local variables

       call fileio_interface_write(fv3_json(i),fv3_output_filename,        &
            & netcdf_var_local)

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(netcdf_var)
       call variable_interface_cleanup_struct(netcdf_var_local)

    end do ! do i = 1, size(fv3_json)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(dst_grid)
    call variable_interface_cleanup_struct(fv3_input)
    call variable_interface_cleanup_struct(fv3_output)
    call variable_interface_cleanup_struct(remap)
    call variable_interface_cleanup_struct(slint)
    call variable_interface_cleanup_struct(src_grid)

    !=====================================================================

  end subroutine upp_regional

  !=======================================================================

end module fv3_post_interface
