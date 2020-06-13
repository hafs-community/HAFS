module update_state_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: update_state_interface
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
  public :: update_state_init
  public :: update_state_nest_lbcs
  public :: update_state_nest_parent_merge
  interface update_state_init
     module procedure fv3_nest_parent_merge_init
     module procedure fv3_nest_lbcs_init
  end interface update_state_init
  interface update_state_nest_lbcs
     module procedure fv3_nest_lbcs
  end interface update_state_nest_lbcs
  interface update_state_nest_parent_merge
     module procedure fv3_nest_parent_merge
  end interface update_state_nest_parent_merge
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! fv3_nest_lbcs.f90

  ! DESCRIPTION:

  ! This subroutine computes the data-assimilation derived increment
  ! values and relaxes the increment values to a value of 0.0 as the
  ! edges of the nested forecast domain are approached using a
  ! relaxation mask defined by the user specified attributes; optional
  ! clipping of negative (e.g., non-physical) values is performed if
  ! specified.

  ! INPUT VARIABLES:

  ! * fv3var_anl; a FORTRAN fv3var_struct variable containing the
  !   data-assimilation "analysis" variable attributes.

  ! * fv3var_bkgrd; a FORTRAN fv3var_struct variable containing the
  !   data-assimilation "background" (e.g., first-guess) variable
  !   attributes.

  ! * fv3grid; a FORTRAN fv3grid_struct variable containing (at
  !   minimum) the increment relaxation mask values as determined by
  !   the user specified relaxation region thresholds.

  ! * json_fv3var; a FORTRAN json_fv3var_struct variable containing
  !   the attributes for the respective FV3 variable.

  ! OUTPUT VARIABLES:

  ! * fv3var_anl; a FORTRAN fv3var_struct variable containing the
  !   updated "analysis" variable values determined by the relaxation
  !   region thresholds (see fv3grid) and the respective variable
  !   attributes (see json_fv3var).

  !-----------------------------------------------------------------------

  subroutine fv3_nest_lbcs(fv3var_anl,fv3var_bkgrd,fv3grid,json_fv3var)

    ! Define variables passed to routine

    type(fv3var_struct)                                                 :: fv3var_anl
    type(fv3var_struct)                                                 :: fv3var_bkgrd
    type(fv3grid_struct)                                                :: fv3grid
    type(json_fv3var_struct)                                            :: json_fv3var
    
    ! Define variables computed within routine

    type(fv3var_struct)                                                 :: fv3var_incr

    ! Define counting variable

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    fv3var_incr%nx = fv3var_anl%nx
    fv3var_incr%ny = fv3var_anl%ny
    fv3var_incr%nz = fv3var_anl%nz
    call variable_interface_setup_struct(fv3var_incr)
    
    ! Compute local variables

    fv3var_incr%var = fv3var_anl%var - fv3var_bkgrd%var

    ! Define local variables

    fv3var_anl%var = 0.0
    
    ! Loop through local variable

    do i = 1, fv3var_incr%nz

       ! Compute local variables

       fv3var_anl%var(:,i) = fv3var_bkgrd%var(:,i) +                       &
            & fv3var_incr%var(:,i)*fv3grid%mask
       
    end do ! do i = 1, fv3var_incr%nz

    ! Check local variable and proceed accordingly

    if(json_fv3var%clip) then
       
       ! Define local variables
       
       where(fv3var_anl%var .lt. 0.0) fv3var_anl%var = clpval

    end if ! if(json_fv3var%clip)
    
    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(fv3var_incr)

    !=====================================================================

  end subroutine fv3_nest_lbcs
  
  !=======================================================================

  ! SUBROUTINE:

  ! fv3_nest_lbcs_init.f90

  ! DESCRIPTION:

  ! This subroutine defines the relaxation mask to be applied for the
  ! data-assimilation computed increments (e.g., analysis variable
  ! minus background variable) such that the increments relax to a
  ! value of 0.0 within a user specified relaxation region as the
  ! edges of the respective FV3 forecast grid are reached.

  ! INPUT VARIABLES:

  ! * fv3grid; an array of FORTRAN fv3grid_struct variables of
  !   dimension 3 containing the defined and computed attributes of
  !   the FV3 Arakawa-D grid; the mass variable projections are
  !   contained in array 1, the zonal- (U-) wind aligned variable
  !   projections are contained in array 2, and the meridional- (V-)
  !   wind aligned variable projections are contained in array 3.

  ! OUTPUT VARIABLES:

  ! * fv3grid; an array of FORTRAN fv3grid_struct variables now
  !   containing the increment relaxation mask values as determined by
  !   the user specified relaxation region thresholds.

  !-----------------------------------------------------------------------

  subroutine fv3_nest_lbcs_init(fv3grid)

    ! Define variables passed to routine

    type(fv3grid_struct)                                                :: fv3grid(:)

    ! Define variables computed within routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(interp_struct)                                                 :: interp
    real(r_kind),               dimension(:,:),             allocatable :: lat
    real(r_kind),               dimension(:,:),             allocatable :: lon
    real(r_kind),               dimension(:,:),             allocatable :: mask
    real(r_kind),               dimension(:,:),             allocatable :: work
    integer                                                             :: nmx_nomask
    integer                                                             :: nmy_nomask
    integer                                                             :: npx_nomask
    integer                                                             :: npy_nomask
    
    ! Define counting variable

    integer                                                             :: i, j, k

    !=====================================================================

    ! Loop through local variable

    do k = 1, size(fv3grid)
    
       ! Allocate memory for local variables

       if(.not. allocated(lat))                                            &
            & allocate(lat(fv3grid(k)%nx,fv3grid(k)%ny))
       if(.not. allocated(lon))                                            &
            & allocate(lon(fv3grid(k)%nx,fv3grid(k)%ny))
       if(.not. allocated(mask))                                           &
            & allocate(mask(fv3grid(k)%nx,fv3grid(k)%ny))
       if(.not. allocated(work))                                           &
            & allocate(work(fv3grid(k)%nx,fv3grid(k)%ny))
    
       ! Define local variables

       lat        = reshape(fv3grid(k)%lat,shape(lat))
       lon        = reshape(fv3grid(k)%lon,shape(lon))
       mask       = spval
       work       = spval
       nmx_nomask = min((npad_cells - 1),fv3grid(k)%nx)
       nmy_nomask = min((npad_cells - 1),fv3grid(k)%ny)
       npx_nomask = max((fv3grid(k)%nx - (npad_cells + 1)),1)
       npy_nomask = max((fv3grid(k)%ny - (npad_cells + 1)),1)
    
       ! Loop through local variable

       do j = 1, fv3grid(k)%ny

          ! Loop through local variable

          do i = 1, fv3grid(k)%nx

             ! Check local variable and proceed accordingly

             if((i .le. nmx_nomask) .or. (i .ge. npx_nomask)) then 
             
                ! Define local variables
             
                mask(i,:) = 4.0

             end if ! if((i .le. nmx_nomask) .or. (i .ge. npx_nomask))

             ! Check local variable and proceed accordingly
          
             if((j .le. nmy_nomask) .or. (j .ge. npy_nomask)) then

                ! Define local variables
             
                mask(:,j) = 4.0

             end if ! if((j .le. nmy_nomask) .or. (j .ge. npy_nomask))
          
          end do ! do i = 1, fv3grid(k)%nx
          
       end do ! do j = 1, fv3grid(k)%ny

       ! Define local variables

       nmx_nomask = min((nmx_nomask + 1),fv3grid(k)%nx)
       nmy_nomask = min((nmy_nomask + 1),fv3grid(k)%ny)
       npx_nomask = max((npx_nomask + 1),1)
       npy_nomask = max((npy_nomask + 1),1)
    
       ! Loop through local variable

       do j = 1, fv3grid(k)%ny

          ! Loop through local variable

          do i = 1, fv3grid(k)%nx

             ! Check local variable and proceed accordingly

             if((i .ge. nmx_nomask) .and. (i .le. npx_nomask) .and.         &
                  & (j .ge. nmy_nomask) .and. (j .le. npy_nomask)) then 

                ! Define local variables
             
                mask(i,j) = 3.0

             end if ! if((i .ge. nmx_nomask) .and. (i .le. npx_nomask)
                    ! .and. (j .ge. nmy_nomask) .and. (j
                    ! .le. npy_nomask))
                    
          end do ! do i = 1, fv3grid(k)%nx
          
       end do ! do j = 1, fv3grid(k)%ny
    
       ! Define local variables

       nmx_nomask = min((nmx_nomask + 1),fv3grid(k)%nx)
       nmy_nomask = min((nmy_nomask + 1),fv3grid(k)%ny)
       npx_nomask = max((npx_nomask - 1),1)
       npy_nomask = max((npy_nomask - 1),1)
    
       ! Loop through local variable

       do j = 1, fv3grid(k)%ny

          ! Loop through local variable

          do i = 1, fv3grid(k)%nx

             ! Check local variable and proceed accordingly

             if((i .ge. nmx_nomask) .and. (i .le. npx_nomask) .and.        &
                  & (j .ge. nmy_nomask) .and. (j .le. npy_nomask)) then 

                ! Define local variables
             
                mask(i,j) = 2.0

             end if ! if((i .ge. nmx_nomask) .and. (i .le. npx_nomask)
                    ! .and. (j .ge. nmy_nomask) .and. (j
                    ! .le. npy_nomask))
             
          end do ! do i = 1, fv3grid(k)%nx

       end do ! do j = 1, fv3grid(k)%ny

       ! Define local variables

       nmx_nomask = min((nmx_nomask + nrelax_cells),fv3grid(k)%nx)
       nmy_nomask = min((nmy_nomask + nrelax_cells),fv3grid(k)%ny)
       npx_nomask = max((npx_nomask - nrelax_cells),1)
       npy_nomask = max((npy_nomask - nrelax_cells),1)
    
       ! Loop through local variable

       do j = 1, fv3grid(k)%ny

          ! Loop through local variable

          do i = 1, fv3grid(k)%nx

             ! Check local variable and proceed accordingly

             if((i .ge. nmx_nomask) .and. (i .le. npx_nomask) .and.       &
                  & (j .ge. nmy_nomask) .and. (j .le. npy_nomask)) then 

                ! Define local variables
             
                mask(i,j) = 1.0

             end if ! if((i .ge. nmx_nomask) .and. (i .le. npx_nomask)
                    ! .and. (j .ge. nmy_nomask) .and. (j
                    ! .le. npy_nomask))
                    
          end do ! do i = 1, fv3grid(k)%nx

       end do ! do j = 1, fv3grid(k)%ny

       ! Define local variables

       nmx_nomask = min((nmx_nomask + 1),fv3grid(k)%nx)
       nmy_nomask = min((nmy_nomask + 1),fv3grid(k)%ny)
       npx_nomask = max((npx_nomask - 1),1)
       npy_nomask = max((npy_nomask - 1),1)
    
       ! Loop through local variable

       do j = 1, fv3grid(k)%ny

          ! Loop through local variable

          do i = 1, fv3grid(k)%nx

             ! Check local variable and proceed accordingly

             if((i .ge. nmx_nomask) .and. (i .le. npx_nomask) .and.     &
                  & (j .ge. nmy_nomask) .and. (j .le. npy_nomask)) then 

                ! Define local variables
             
                mask(i,j) = 0.0

             end if ! if((i .ge. nmx_nomask) .and. (i .le. npx_nomask)
                    ! .and. (j .ge. nmy_nomask) .and. (j
                    ! .le. npy_nomask))
                    
          end do ! do i = 1, fv3grid(k)%nx
          
       end do ! do j = 1, fv3grid(k)%ny

       ! Define local variables

       dst_grid%ncoords = count(mask .eq. 2.0)
       dst_grid%nz      = 1
       call variable_interface_setup_struct(dst_grid)
       src_grid%ncoords = count(mask .ne. 2.0)
       src_grid%nz      = 1
       call variable_interface_setup_struct(src_grid)
       dst_grid%ncoords = 0
       src_grid%ncoords = 0
       
       ! Loop through local variable

       do j = 1, fv3grid(k)%ny

          ! Loop through local variable

          do i = 1, fv3grid(k)%nx

             ! Check local variable and proceed accordingly

             if(mask(i,j) .eq. 2.0) then

                ! Define local variables

                dst_grid%ncoords = dst_grid%ncoords + 1
                dst_grid%lat(dst_grid%ncoords) = lat(i,j)
                dst_grid%lon(dst_grid%ncoords) = lon(i,j)

             end if ! if(mask(i,j) .eq. 2.0)

             ! Check local variable and proceed accordingly

             if((mask(i,j) .ne. 2.0)) then 

                ! Define local variables

                src_grid%ncoords = src_grid%ncoords + 1
                src_grid%lat(src_grid%ncoords) = lat(i,j)
                src_grid%lon(src_grid%ncoords) = lon(i,j)

                ! Check local variable and proceed accordingly

                if(mask(i,j) .gt. 2.0) src_grid%var(src_grid%ncoords,1) = 1.0
                if(mask(i,j) .lt. 2.0) src_grid%var(src_grid%ncoords,1) = 0.0
             
             end if ! if((mask(i,j) .eq. 3.0) .or. (mask(i,j) .eq. 1.0))
          
          end do ! do i = 1, fv3grid(k)%nx

       end do ! do j = 1, fv3grid(k)%ny

       ! Deallocate memory for local variables
       
       if(allocated(lat)) deallocate(lat)
       if(allocated(lon)) deallocate(lon)    

       ! Compute local variables

       call interpolation_interface_init(src_grid,dst_grid,interp)
       call interpolation_interface_interp(src_grid,dst_grid,interp,2)

       ! Define local variables

       dst_grid%ncoords = 0

       ! Loop through local variable

       do j = 1, fv3grid(k)%ny

          ! Loop through local variable

          do i = 1, fv3grid(k)%nx

             ! Check local variable and proceed accordingly

             if(mask(i,j) .gt. 2.0) work(i,j) = 1.0
             if(mask(i,j) .lt. 2.0) work(i,j) = 0.0

             ! Check local variable and proceed accordingly

             if(mask(i,j) .eq. 2.0) then

                ! Define local variables

                dst_grid%ncoords = dst_grid%ncoords + 1                
                work(i,j)        = dst_grid%var(dst_grid%ncoords,1)

             end if ! if(mask(i,j) .eq. 2.0)

          end do ! do i = 1, fv3grid(k)%nx
    
       end do ! do j = 1, fv3grid(k)%ny

       ! Define local variables

       where(work .gt. 1.0) work = 1.0
       where(work .lt. 0.0) work = 0.0
       mask = 1.0 - work
       fv3grid(k)%mask = reshape(mask,shape(fv3grid(k)%mask))
    
       ! Deallocate memory for local variables

       if(allocated(mask)) deallocate(mask)
       if(allocated(work)) deallocate(work)
       call variable_interface_cleanup_struct(dst_grid)
       call variable_interface_cleanup_struct(interp)
       call variable_interface_cleanup_struct(src_grid)

    end do ! do k = 1, size(fv3grid)
       
    !=====================================================================

  end subroutine fv3_nest_lbcs_init
    
  !=======================================================================
  
  ! SUBROUTINE:

  ! fv3_nest_parent_merge.f90

  ! DESCRIPTION:

  ! This subroutine merges a FV3 nested-grid variable into it's
  ! corresponding FV3 parent tile using linear relaxation and/or
  ! nearest-neighbor interpolation methods.

  ! INPUT VARIABLES:

  ! * fv3var_nest; a FORTRAN fv3var_struct variable containing the FV3
  !   nested-grid variable attributes.

  ! * fv3var_parent; a FORTRAN fv3var_struct variable containing the
  !   FV3 parent tile variable attributes.

  ! * fv3grid_nest; an array of dimension 3 of FORTRAN fv3grid_struct
  !   variables containing the FV3 nested-grid variable Arakawa D-grid
  !   geographical location attributes.

  ! * fv3grid_parent; an array of dimension 3 of FORTRAN
  !   fv3grid_struct variables containing the FV3 nested-grid parent
  !   tile variable Arakawa D-grid geographical location attributes.

  ! * json_fv3var; a FORTRAN json_fv3var_struct variable containing
  !   the attributes for the respective FV3 variable.
  
  ! * interp; a FORTRAN interp_struct variable containing the
  !   remapping attributes required to interpolated the FV3
  !   nested-grid variable values to the corresponding FV3 parent tile
  !   variable.

  ! OUTPUT VARIABLES:

  ! * fv3var_parent; a FORTRAN fv3var_struct variable containing the
  !   merged FV3 nested-grid and parent tile variable values.

  !-----------------------------------------------------------------------

  subroutine fv3_nest_parent_merge(fv3var_nest,fv3var_parent,fv3grid_nest,  &
       & fv3grid_parent,json_fv3var,interp)
  
    ! Define variables passed to routine

    type(fv3var_struct)                                                 :: fv3var_nest
    type(fv3var_struct)                                                 :: fv3var_parent
    type(fv3grid_struct)                                                :: fv3grid_nest
    type(fv3grid_struct)                                                :: fv3grid_parent
    type(interp_struct)                                                 :: interp
    type(json_fv3var_struct)                                            :: json_fv3var

    ! Define variables computed within routine

    type(remapvar_struct)                                               :: remapvar
    real(r_kind),               dimension(:,:),             allocatable :: radius
    real(r_kind),               dimension(:),               allocatable :: mask
    real(r_kind)                                                        :: max_radius
    real(r_kind)                                                        :: min_radius
    
    ! Define counting variable

    integer                                                             :: i
  
    !=====================================================================

    ! Compute local variables

    call interpolation_interface_interp(fv3var_nest,fv3var_parent,         &
         & json_fv3var,interp,remapvar)
    call grid_methods_radialdist(fv3grid_nest)

    ! Define local variables

    fv3grid_parent%clat = fv3grid_nest%clat
    fv3grid_parent%clon = fv3grid_nest%clon

    ! Compute local variables
    
    call grid_methods_radialdist(fv3grid_parent)

    ! Define local variables

    where(remapvar%remap_var .eq. spval) remapvar%remap_var =              &
         & remapvar%fixed_var
    
    ! Allocate memory for local variables

    if(.not. allocated(radius))                                            &
         & allocate(radius(fv3grid_nest%nx,fv3grid_nest%ny))
    if(.not. allocated(mask))                                              &
         & allocate(mask(fv3grid_parent%ncoords))

    ! Define local variables

    radius     = reshape(fv3grid_nest%radius,shape(radius))
    max_radius = rearth_equator

    ! Loop through local variable

    do i = 1, fv3grid_parent%nx

       ! Define local variables
       
       max_radius = min(max_radius,maxval(radius(i,:)))

    end do ! do i = 1, fv3grid_parent%nx
    
    ! Loop through local variable

    do i = 1, fv3grid_parent%ny

       ! Define local variables
       
       max_radius = min(max_radius,maxval(radius(:,i)))

    end do ! do i = 1, fv3grid_parent%ny

    ! Deallocate memory for local variables

    if(allocated(radius)) deallocate(radius)
    
    ! Define local variables
       
    min_radius = (domain_blend_ratio*max_radius)

    ! Compute local variables

    mask = (fv3grid_parent%radius - min_radius)/(max_radius - min_radius)

    ! Define local variables

    where(mask .lt. 0.0) mask = 0.0
    where(mask .gt. 1.0) mask = 1.0
    mask = 1.0 - mask

    ! Loop through local variable

    do i = 1, fv3var_parent%nz
       
       ! Compute local variables

       fv3var_parent%var(:,i) = remapvar%remap_var(:,i)*mask +             &
            & remapvar%fixed_var(:,i)*(1.0 - mask)

       ! Define local variables

       if(debug) write(6,500) trim(adjustl(json_fv3var%variable_name)),    &
            & i, minval(fv3var_parent%var(:,i)),                           &
            & maxval(fv3var_parent%var(:,i))
       
    end do ! do i = 1, fv3var_parent%nz

    ! Deallocate memory for local variables

    if(allocated(mask)) deallocate(mask)
    call variable_interface_cleanup_struct(remapvar)

    ! Define local variables

500 format('FV3_NEST_PARENT_MERGE: Variable ',a,1x,'level ',i3.3,1x,       &
         & 'min/max = ',2f13.5)
    
    !=====================================================================

  end subroutine fv3_nest_parent_merge

  !=======================================================================

  ! SUBROUTINE:

  ! fv3_nest_parent_merge_init.f90

  ! DESCRIPTION:

  ! This subroutine initializes the remapping variable FORTRAN objects
  ! for the remapping of nested-grid variables to the corresponding
  ! parent-grid variable.

  ! INPUT VARIABLES:

  ! * fv3grid_nest; an array of dimension 3 of FORTRAN fv3grid_struct
  !   variables containing the FV3 nested-grid variable Arakawa D-grid
  !   geographical location attributes.

  ! * fv3grid_parent; an array of dimension 3 of FORTRAN
  !   fv3grid_struct variables containing the FV3 nested-grid parent
  !   tile variable Arakawa D-grid geographical location attributes.

  ! * interp; a FORTRAN interp_struct variable.
  
  ! OUTPUT VARIABLES:

  ! * interp; a FORTRAN interp_struct variable containing the
  !   remapping attributes required to interpolated the FV3
  !   nested-grid variable values to the corresponding FV3 parent tile
  !   variable.

  !-----------------------------------------------------------------------
  
  subroutine fv3_nest_parent_merge_init(fv3grid_nest,fv3grid_parent,       &
       & interp)

    ! Define variables passed to routine
    
    type(fv3grid_struct)                                                :: fv3grid_nest(:)
    type(fv3grid_struct)                                                :: fv3grid_parent(:)
    type(interp_struct)                                                 :: interp(:)

    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================
    
    ! Loop through local variable

    do i = 1, size(interp)

       ! Compute local variables

       call interpolation_interface_init(fv3grid_nest(i),                  &
            & fv3grid_parent(i),interp(i))

    end do ! do i = 1, size(interp)

    ! Compute local variables

    call fv3_remappres_init(fv3grid_nest,fv3grid_parent,interp)

    !=====================================================================
    
  end subroutine fv3_nest_parent_merge_init

  !=======================================================================

  ! SUBROUTINE:

  ! fv3_remappres_init.f90

  ! DESCRIPTION:

  ! This subroutine computes the updated the pressure profile
  ! following the interpolation and calculation of the nested-domain
  ! pressure profile to the respective parent-domain pressure profile.

  ! INPUT VARIABLES:

  ! * fv3grid_nest; an array of dimension 3 of FORTRAN fv3grid_struct
  !   variables containing the FV3 nested-grid variable Arakawa D-grid
  !   geographical location attributes.

  ! * fv3grid_parent; an array of dimension 3 of FORTRAN
  !   fv3grid_struct variables containing the FV3 nested-grid parent
  !   tile variable Arakawa D-grid geographical location attributes.

  ! * interp; a FORTRAN interp_struct variable.

  ! OUTPUT VARIABLES:

  ! * fv3grid_nest; an array of dimension 3 of FORTRAN fv3grid_struct
  !   variables now containing the FV3 nested-tile pressure profile
  !   for each grid-type.

  ! * fv3grid_parent; an array of dimension 3 of FORTRAN
  !   fv3grid_struct variables now containing the FV3 parent-tile
  !   pressure profile for each grid-type.

  !-----------------------------------------------------------------------

  subroutine fv3_remappres_init(fv3grid_nest,fv3grid_parent,interp)

    ! Define variables passed to routine

    type(fv3grid_struct)                                                :: fv3grid_nest(:)
    type(fv3grid_struct)                                                :: fv3grid_parent(:)
    type(interp_struct)                                                 :: interp(:)
    
    ! Define variables computed within routine

    type(fv3var_struct)                                                 :: fv3var_nest
    type(fv3var_struct)                                                 :: fv3var_parent
    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(interp_struct)                                                 :: interp_local
    type(json_fv3var_struct)                                            :: json_fv3var
    type(remappres_struct)                                              :: remappres_nest
    type(remappres_struct)                                              :: remappres_parent

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call fileio_interface_read(fv3_presvar_parent_filename,                &
         & fv3grid_parent(1),remappres_parent)
    if(debug) write(6,500) 'Parent', minval(remappres_parent%psfc),        &
         & maxval(remappres_parent%psfc)
    call fileio_interface_read(fv3_presvar_nest_filename,                  &
         & fv3grid_nest(1),remappres_nest)
    if(debug) write(6,500) 'Nested', minval(remappres_nest%psfc),          &
         & maxval(remappres_nest%psfc)
    remappres_parent%pres = 0.0
    remappres_nest%pres   = 0.0

    ! Loop through local variable
    
    do i = 1, remappres_parent%nz

       ! Compute local variables

       remappres_parent%pres(:,i) = fv3grid_parent(1)%ak(i) +              &
            & fv3grid_parent(1)%bk(i)*remappres_parent%psfc
       
       ! Define local variables

       if(debug) write(6,501) 'Parent', (remappres_parent%nz - i + 1),     &
            & minval(remappres_parent%pres(:,i)),                          &
            & maxval(remappres_parent%pres(:,i))

    end do ! do i = 1, remappres_parent%nz

    ! Loop through local variable
    
    do i = 1, remappres_nest%nz

       ! Compute local variables

       remappres_nest%pres(:,i) = fv3grid_nest(1)%ak(i) +                  &
            & fv3grid_nest(1)%bk(i)*remappres_nest%psfc
       
       ! Define local variables

       if(debug) write(6,501) 'Nest', (remappres_nest%nz - i + 1),         &
            & minval(remappres_nest%pres(:,i)),                            &
            & maxval(remappres_nest%pres(:,i))

    end do ! do i = 1, remappres_nest%nz

    ! Define local variables

    json_fv3var%variable_name = 'pres'
    fv3var_nest%nx            = fv3grid_nest(1)%nx
    fv3var_nest%ny            = fv3grid_nest(1)%ny
    fv3var_nest%nz            = fv3grid_nest(1)%nz
    call variable_interface_setup_struct(fv3var_nest)
    fv3var_nest%var           = remappres_nest%pres
    fv3var_parent%nx          = fv3grid_parent(1)%nx
    fv3var_parent%ny          = fv3grid_parent(1)%ny
    fv3var_parent%nz          = fv3grid_parent(1)%nz
    call variable_interface_setup_struct(fv3var_parent)
    fv3var_nest%var           = remappres_nest%pres
    fv3var_parent%var         = remappres_parent%pres

    ! Compute local variables

    call fv3_nest_parent_merge(fv3var_nest,fv3var_parent,fv3grid_nest(1),  &
         & fv3grid_parent(1),json_fv3var,interp(1))

    ! Define local variables

    fv3grid_nest(1)%pres   = fv3var_nest%var
    fv3grid_parent(1)%pres = fv3var_parent%var

    ! Check local variable and proceed accordingly

    if(debug) then

       ! Loop through local variable

       do i = 1, fv3grid_parent(1)%nz

          ! Define local variables
          
          write(6,502) 'nest', i, 'T',                                     &
               & minval(fv3grid_nest(1)%pres(:,i)),                        &
               & maxval(fv3grid_nest(1)%pres(:,i))

       end do ! do i = 1, fv3grid_parent(1)%nz
          
    end if ! if(debug)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(fv3var_nest)
    call variable_interface_cleanup_struct(remappres_nest)

    ! Define local variables
    
    src_grid%nx      = fv3grid_nest(1)%nx
    src_grid%ny      = fv3grid_nest(1)%ny
    src_grid%ncoords = (src_grid%nx*src_grid%ny)
    src_grid%nz      = fv3grid_nest(1)%nz
    call variable_interface_setup_struct(src_grid)
    src_grid%lat     = fv3grid_nest(1)%lat
    src_grid%lon     = fv3grid_nest(1)%lon
    src_grid%var     = fv3grid_nest(1)%pres
    dst_grid%nx      = fv3grid_nest(2)%nx
    dst_grid%ny      = fv3grid_nest(2)%ny
    dst_grid%ncoords = (dst_grid%nx*dst_grid%ny)
    dst_grid%nz      = fv3grid_nest(2)%nz
    call variable_interface_setup_struct(dst_grid)
    dst_grid%lat     = fv3grid_nest(2)%lat
    dst_grid%lon     = fv3grid_nest(2)%lon

    ! Compute local variables

    call interpolation_interface_init(src_grid,dst_grid,interp_local)
    call interpolation_interface_interp(src_grid,dst_grid,interp_local,2)

    ! Define local variables

    fv3grid_nest(2)%pres = dst_grid%var

    ! Check local variable and proceed accordingly

    if(debug) then

       ! Loop through local variable

       do i = 1, fv3grid_nest(2)%nz

          ! Define local variables

          write(6,502) 'nest', i, 'U', minval(fv3grid_nest(2)%pres(:,i)),   &
               & maxval(fv3grid_nest(2)%pres(:,i))

       end do ! do i = 1, fv3grid_nest(2)%nz
          
    end if ! if(debug)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(dst_grid)
    call variable_interface_cleanup_struct(interp_local)

    ! Define local variables
    
    dst_grid%nx      = fv3grid_nest(3)%nx
    dst_grid%ny      = fv3grid_nest(3)%ny
    dst_grid%ncoords = (dst_grid%nx*dst_grid%ny)
    dst_grid%nz      = fv3grid_nest(3)%nz
    call variable_interface_setup_struct(dst_grid)
    dst_grid%lat     = fv3grid_nest(3)%lat
    dst_grid%lon     = fv3grid_nest(3)%lon

    ! Compute local variables

    call interpolation_interface_init(src_grid,dst_grid,interp_local)
    call interpolation_interface_interp(src_grid,dst_grid,interp_local,2)

    ! Define local variables

    fv3grid_nest(3)%pres = dst_grid%var

    ! Check local variable and proceed accordingly

    if(debug) then

       ! Loop through local variable

       do i = 1, fv3grid_nest(3)%nz

          ! Define local variables

          write(6,502) 'nest', i, 'V', minval(fv3grid_nest(3)%pres(:,i)),   &
               & maxval(fv3grid_nest(3)%pres(:,i))

       end do ! do i = 1, fv3grid_nest(3)%nz
          
    end if ! if(debug)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(dst_grid)
    call variable_interface_cleanup_struct(interp_local)
    
    ! Define local variables
    
    src_grid%nx      = fv3grid_parent(1)%nx
    src_grid%ny      = fv3grid_parent(1)%ny
    src_grid%ncoords = (src_grid%nx*src_grid%ny)
    src_grid%nz      = fv3grid_parent(1)%nz
    call variable_interface_setup_struct(src_grid)
    src_grid%lat     = fv3grid_parent(1)%lat
    src_grid%lon     = fv3grid_parent(1)%lon
    src_grid%var     = fv3grid_parent(1)%pres
    dst_grid%nx      = fv3grid_parent(2)%nx
    dst_grid%ny      = fv3grid_parent(2)%ny
    dst_grid%ncoords = (dst_grid%nx*dst_grid%ny)
    dst_grid%nz      = fv3grid_parent(2)%nz
    call variable_interface_setup_struct(dst_grid)
    dst_grid%lat     = fv3grid_parent(2)%lat
    dst_grid%lon     = fv3grid_parent(2)%lon

    ! Compute local variables

    call interpolation_interface_init(src_grid,dst_grid,interp_local)
    call interpolation_interface_interp(src_grid,dst_grid,interp_local,2)

    ! Define local variables

    fv3grid_parent(2)%pres = dst_grid%var

    ! Check local variable and proceed accordingly

    if(debug) then

       ! Loop through local variable

       do i = 1, fv3grid_parent(2)%nz

          ! Define local variables

          write(6,502) 'parent', i, 'U',                                   &
               & minval(fv3grid_parent(2)%pres(:,i)),                      &
               & maxval(fv3grid_parent(2)%pres(:,i))

       end do ! do i = 1, fv3grid_parent(2)%nz
          
    end if ! if(debug)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(dst_grid)
    call variable_interface_cleanup_struct(interp_local)

    ! Define local variables

    dst_grid%nx      = fv3grid_parent(3)%nx
    dst_grid%ny      = fv3grid_parent(3)%ny
    dst_grid%ncoords = (dst_grid%nx*dst_grid%ny)
    dst_grid%nz      = fv3grid_parent(3)%nz
    call variable_interface_setup_struct(dst_grid)
    dst_grid%lat     = fv3grid_parent(3)%lat
    dst_grid%lon     = fv3grid_parent(3)%lon

    ! Compute local variables

    call interpolation_interface_init(src_grid,dst_grid,interp_local)
    call interpolation_interface_interp(src_grid,dst_grid,interp_local,2)

    ! Define local variables

    fv3grid_parent(3)%pres = dst_grid%var

    ! Check local variable and proceed accordingly

    if(debug) then

       ! Loop through local variable

       do i = 1, fv3grid_parent(3)%nz

          ! Define local variables

          write(6,502) 'parent', i, 'V',                                   &
               & minval(fv3grid_parent(3)%pres(:,i)),                      &
               & maxval(fv3grid_parent(3)%pres(:,i))

       end do ! do i = 1, fv3grid_parent(3)%nz
          
    end if ! if(debug)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(dst_grid)
    call variable_interface_cleanup_struct(fv3var_parent)
    call variable_interface_cleanup_struct(interp_local)
    call variable_interface_cleanup_struct(remappres_parent)
    call variable_interface_cleanup_struct(src_grid)
    
    ! Define local variables

500 format('FV3_REMAPPRES_INIT: ',a,' tile surface pressure min/max = ',   &
         & 2f13.5)
501 format('FV3_REMAPPRES_INIT: ',a,' tile level ',i3.3,' pressure ',      &
         & ' min/max = ', 2f13.5)
502 format('FV3_REMAPPRES_INIT: Remapped ', a, ' tile level ', i3.3,       &
         & ' grid-type ', a, ' min/max = ', 2f13.5)
       
    !=====================================================================
    
  end subroutine fv3_remappres_init
    
  !=======================================================================
  
end module update_state_interface
