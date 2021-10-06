module interpolation_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: interpolation_interface
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
  use kinds_interface
  use math_methods_interface
  use namelist_interface
  use slint
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: interpolation_interface_init
  public :: interpolation_interface_interp
  interface interpolation_interface_init
     module procedure fv3_nest_parent_init
     module procedure general_init
  end interface interpolation_interface_init
  interface interpolation_interface_interp
     module procedure fv3_nest_parent_interp
     module procedure general_interp
  end interface interpolation_interface_interp

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE: 

  ! bilinear.f90

  ! DESCRIPTION: 

  ! This subroutine interpolates a variable to a destination grid
  ! using bi-linear interpolation via the user-specified algorithm.

  ! INPUT VARIABLES:

  ! * src_grid; a FORTRAN grid_struct variable containing the source
  !   grid variable values to be interpolated to the destination grid.

  ! * dst_grid; a FORTRAN grid_struct variable.

  ! * interp; a FORTRAN interp_struct variable containing the
  !   remapping/interpolation attributes as defined by the
  !   user-specified method/algorithm.
  
  ! OUTPUT VARIABLES:

  ! * dst_grid; a FORTRAN grid_struct variable containing the source
  !   grid variable values interpolated to the destination grid
  !   mapping.

  !-----------------------------------------------------------------------

  subroutine bilinear(src_grid,dst_grid,interp)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(interp_struct)                                                 :: interp
    
    ! Define variables computed within routine

    real(r_double)                                                      :: c(3)
    real(r_double)                                                      :: v(3)

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(is_slint) then
    
       ! Loop through local variable
       
       do j = 1, dst_grid%nz
          
          ! Loop through local variable
          
          do i = 1, dst_grid%ncoords
             
             ! Define local variables
             
             c    = interp%slint_coeffs(:,i)
             v(1) = dble(src_grid%var(interp%slint_nn(1,i),j))
             v(2) = dble(src_grid%var(interp%slint_nn(2,i),j))
             v(3) = dble(src_grid%var(interp%slint_nn(3,i),j))
             
             ! Compute local variables
             
             dst_grid%var(i,j) = real(c(1)*v(1) + c(2)*v(2) + c(3)*v(3))
             
          end do ! do i = 1, dst_grid%ncoords
          
       end do ! do j = 1, dst_grid%nz

    end if
       
    !=====================================================================

  end subroutine bilinear  

  !=======================================================================

  ! SUBROUTINE:

  ! fv3_nest_parent_init.f90

  ! DESCRIPTION:

  ! This subroutine initializes the FORTRAN remapping variable objects
  ! in accordance with the interpolation method/algorithm specified by
  ! the user.

  ! INPUT VARIABLES:

  ! * fv3grid_nest; a FORTRAN fv3grid_struct variable containing the
  !   FV3 nested tile grid attributes.

  ! * fv3grid_parent; a FORTRAN fv3grid_struct variable containing the
  !   FV3 parent tile grid attributes.

  ! * interp; a FORTRAN interp_struct variable.
  
  ! OUTPUT VARIABLES:

  ! * interp; a FORTRAN interp_struct variable containing the
  !   remapping/interpolation attributes defined in accordance with
  !   the interpolation method/algorithm specified by the user.

  !-----------------------------------------------------------------------

  subroutine fv3_nest_parent_init(fv3grid_nest,fv3grid_parent,interp)

    ! Define variables passed to routine

    type(fv3grid_struct)                                                :: fv3grid_nest
    type(fv3grid_struct)                                                :: fv3grid_parent
    type(interp_struct)                                                 :: interp

    ! Define variables computed within routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid    

    !=====================================================================

    ! Define local variables

    dst_grid%ncoords = fv3grid_parent%ncoords
    dst_grid%nz      = 1
    call variable_interface_setup_struct(dst_grid)
    src_grid%ncoords = fv3grid_nest%ncoords
    src_grid%nz      = 1
    call variable_interface_setup_struct(src_grid)
    dst_grid%lat     = fv3grid_parent%lat
    dst_grid%lon     = fv3grid_parent%lon
    src_grid%lat     = fv3grid_nest%lat
    src_grid%lon     = fv3grid_nest%lon
    
    ! Compute local variables

    if(is_slint) call init_slint(src_grid,dst_grid,interp)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(dst_grid)
    call variable_interface_cleanup_struct(src_grid)

    !=====================================================================

  end subroutine fv3_nest_parent_init
    
  !=======================================================================

  ! SUBROUTINE:

  ! fv3_nest_parent_interp.f90

  ! DESCRIPTION:

  ! This subroutine 

  ! INPUT VARIABLES:

  ! * fv3grid_nest; a FORTRAN fv3grid_struct variable containing the
  !   FV3 nested tile grid attributes.

  ! * fv3grid_parent; a FORTRAN fv3grid_struct variable containing the
  !   FV3 parent tile grid attributes.

  ! * json_fv3var; a FORTRAN json_fv3var_struct variable containing
  !   the respective FV3 attributes, specifically those pertaining to
  !   variable and remapping types.

  ! * interp; a FORTRAN interp_struct variable containing the
  !   remapping/interpolation attributes defined in accordance with
  !   the interpolation method/algorithm specified by the user.

  ! * remapvar; a FORTRAN remapvar_struct variable.
  
  ! OUTPUT VARIABLES:

  ! * remapvar; a FORTRAN remapvar_struct variable containing the
  !   source grid variable remapped to the destination grid mapping.

  !-----------------------------------------------------------------------

  subroutine fv3_nest_parent_interp(fv3var_nest,fv3var_parent,             &
       & json_fv3var,interp,remapvar)
  
    ! Define variables passed to routine

    type(fv3var_struct)                                                 :: fv3var_nest
    type(fv3var_struct)                                                 :: fv3var_parent
    type(json_fv3var_struct)                                            :: json_fv3var
    type(interp_struct)                                                 :: interp
    type(remapvar_struct)                                               :: remapvar

    ! Define variables computed within routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(json_interp_struct)                                            :: json_interp
    
    !=====================================================================

    ! Define local variables

    remapvar%ncoords             = fv3var_parent%ncoords
    remapvar%nz                  = fv3var_parent%nz
    call variable_interface_setup_struct(remapvar)
    remapvar%fixed_var           = fv3var_parent%var
    dst_grid%ncoords             = fv3var_parent%ncoords
    dst_grid%nz                  = fv3var_parent%nz
    call variable_interface_setup_struct(dst_grid)
    dst_grid%pres                = fv3var_parent%pres
    dst_grid%var                 = fv3var_parent%var
    src_grid%ncoords             = fv3var_parent%ncoords
    src_grid%nz                  = fv3var_parent%nz
    call variable_interface_setup_struct(src_grid)
    src_grid%pres                = fv3var_nest%pres
    src_grid%var                 = fv3var_nest%var
    json_interp%interp_bilinear  = json_fv3var%interp_bilinear
    json_interp%interp_nrstnghbr = json_fv3var%interp_nrstnghbr
    
    ! Compute local variables

    if(is_slint) call interp_slint(src_grid,dst_grid,interp,json_interp)

    ! Define local variables

    remapvar%remap_var = dst_grid%var
    
    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(dst_grid)
    call variable_interface_cleanup_struct(src_grid)
    
    !=====================================================================
    
  end subroutine fv3_nest_parent_interp

  !=======================================================================

  ! SUBROUTINE:

  ! general_init.f90

  ! DESCRIPTION:

  ! This subroutine initializes the FORTRAN interp_struct variable
  ! remapping attributes in accordance with the user specified source
  ! and destination FORTRAN grid_struct (src_grid and dst_grid,
  ! respectively) variables.

  ! INPUT VARIABLES:

  ! * src_grid; a FORTRAN grid_struct variable containing the source
  !   grid geographical locations; units are degrees.

  ! * dst_grid; a FORTRAN grid_struct variable containing the
  !   destination grid geographical locations; units are degrees.

  ! * interp; a FORTRAN interp_struct variable.
  
  ! OUTPUT VARIABLES:

  ! * interp; a FORTRAN interp_struct variable containing the
  !   remapping/interpolation attributes as defined by the user
  !   specified interpolation method/algorithm.

  !-----------------------------------------------------------------------

  subroutine general_init(src_grid,dst_grid,interp)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(interp_struct)                                                 :: interp

    !=====================================================================

    ! Compute local variables

    if(is_slint) call init_slint(src_grid,dst_grid,interp)

    !=====================================================================

  end subroutine general_init

  !=======================================================================

  ! SUBROUTINE:

  ! general_interp.f90

  ! DESCRIPTION:

  ! This is the driver-level subroutine to interpolate a source grid
  ! set of values to a destination grid mapping using the user
  ! specified interpolation algorithms.

  ! INPUT VARIABLES:

  ! * src_grid; a FORTRAN grid_struct variable containing the source
  !   grid variable values to be interpolated to the destination grid.

  ! * dst_grid; a FORTRAN grid_struct variable.

  ! * interp; a FORTRAN interp_struct variable containing the
  !   remapping/interpolation attributes as defined by the user
  !   specified method/algorithm.

  ! * interp_type; a FORTRAN integer specifying the interpolation
  !   methodology; 1 = nearest-neighbor interpolation; 2 = bi-linear
  !   interpolation.

  ! OUTPUT VARIABLES:

  ! * dst_grid; a FORTRAN grid_struct variable containing the source
  !   grid variable values interpolated to the destination grid
  !   mapping.

  !-----------------------------------------------------------------------

  subroutine general_interp(src_grid,dst_grid,interp,interp_type)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(interp_struct)                                                 :: interp
    integer                                                             :: interp_type
    
    !=====================================================================

    ! Compute local variables

    if(interp_type .eq. 1) call nrstnghbr(src_grid,dst_grid,interp)
    if(interp_type .eq. 2) call bilinear(src_grid,dst_grid,interp)

    !=====================================================================

  end subroutine general_interp
  
  !=======================================================================

  ! SUBROUTINE:

  ! init_slint.f90

  ! DESCRIPTION:

  ! This subroutine computes the remapping attributes assuming the
  ! SLINT interpolation method/algorithm.

  ! INPUT VARIABLES:

  ! * src_grid; a FORTRAN grid_struct variable containing the source
  !   grid geographical locations; units are degrees.

  ! * dst_grid; a FORTRAN grid_struct variable containing the
  !   destination grid geographical locations; units are degrees.

  ! * interp; a FORTRAN interp_struct variable.
  
  ! OUTPUT VARIABLES:

  ! * interp; a FORTRAN interp_struct variable containing the
  !   remapping/interpolation attributes as defined by the SLINT
  !   method/algorithm.

  !-----------------------------------------------------------------------

  subroutine init_slint(src_grid,dst_grid,interp)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(interp_struct)                                                 :: interp

    ! Define variables computed within routine

    type(grid)                                                          :: grid
    real(r_kind),               dimension(:,:),             allocatable :: grid1
    real(r_kind),               dimension(:,:),             allocatable :: grid2

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid1))                                             &
         & allocate(grid1(src_grid%ncoords,2))
    if(.not. allocated(grid2))                                             &
         & allocate(grid2(dst_grid%ncoords,2))

    ! Define local variables

    grid1(:,1) = src_grid%lat*deg2rad
    grid1(:,2) = src_grid%lon*deg2rad
    grid2(:,1) = dst_grid%lat*deg2rad
    grid2(:,2) = dst_grid%lon*deg2rad

    ! Compute local variables
    
    call slint_init_compute(grid1,src_grid%ncoords,grid2,                  &
         & dst_grid%ncoords,grid)

    ! Define local variables

    interp%ncoords      = src_grid%ncoords
    call variable_interface_setup_struct(interp)
    interp%slint_coeffs = grid%coeffs
    interp%slint_nn     = grid%nn
    if(debug) write(6,500) minval(interp%slint_coeffs),                    &
         & maxval(interp%slint_coeffs)

    ! Deallocate memory for local variables

    if(allocated(grid2)) deallocate(grid2)
    if(allocated(grid1)) deallocate(grid1)

    ! Define local variables

500 format('INIT_SLINT: SLINT coeffs min/max = ',2f13.5)

    !=====================================================================

  end subroutine init_slint

  !=======================================================================

  ! SUBROUTINE:

  ! interp_slint.f90

  ! DESCRIPTION:

  ! This is the driver-level subroutine to interpolate a source grid
  ! set of values to a destination grid mapping using the SLINT
  ! interpolation algorithms.

  ! INPUT VARIABLES:

  ! * src_grid; a FORTRAN grid_struct variable containing the source
  !   grid variable values to be interpolated to the destination grid.

  ! * dst_grid; a FORTRAN grid_struct variable.

  ! * interp; a FORTRAN interp_struct variable containing the
  !   remapping/interpolation attributes as defined by the SLINT
  !   method/algorithm.

  ! * json_interp; a FORTRAN json_interp_struct variable containing
  !   the interpolation instructions for the respective source grid
  !   variable.
  
  ! OUTPUT VARIABLES:

  ! * dst_grid; a FORTRAN grid_struct variable containing the source
  !   grid variable values interpolated to the destination grid
  !   mapping.

  !-----------------------------------------------------------------------

  subroutine interp_slint(src_grid,dst_grid,interp,json_interp)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(interp_struct)                                                 :: interp
    type(json_interp_struct)                                            :: json_interp

    !=====================================================================

    ! Compute local variables

    if(json_interp%interp_bilinear)  call slint_bilinear(src_grid,         &
         & dst_grid,interp)
    if(json_interp%interp_nrstnghbr) call slint_nrstnghbr(src_grid,        &
         & dst_grid,interp)

    !=====================================================================
    
  end subroutine interp_slint

  !=======================================================================

  ! SUBROUTINE:

  ! interp_spline.f90

  ! DESCRIPTION:

  !

  ! INPUT VARIABLES:

  !

  ! OUTPUT VARIABLES:

  !

  !----------------------------------------------------------------------- 

  subroutine interp_spline(spline)

    ! Define variables passed to routine

    type(spline_struct)                                                 :: spline

    !=====================================================================

    ! Compute local variables

    call math_methods_spline(spline)

    !=====================================================================
    
  end subroutine interp_spline

  !=======================================================================

  ! SUBROUTINE: 

  ! nrstnghbr.f90

  ! DESCRIPTION: 

  ! This subroutine interpolates a variable to a destination grid
  ! using nearest-neighbor interpolation via the user-specified
  ! algorithm.

  ! INPUT VARIABLES:

  ! * src_grid; a FORTRAN grid_struct variable containing the source
  !   grid variable values to be interpolated to the destination grid.

  ! * dst_grid; a FORTRAN grid_struct variable.

  ! * interp; a FORTRAN interp_struct variable containing the
  !   remapping/interpolation attributes as defined by the
  !   user-specified method/algorithm.
  
  ! OUTPUT VARIABLES:

  ! * dst_grid; a FORTRAN grid_struct variable containing the source
  !   grid variable values interpolated to the destination grid
  !   mapping.

  !-----------------------------------------------------------------------

  subroutine nrstnghbr(src_grid,dst_grid,interp)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(interp_struct)                                                 :: interp
    
    ! Define variables computed within routine

    real(r_double)                                                      :: v(1)

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(is_slint) then
    
       ! Loop through local variable
       
       do j = 1, dst_grid%nz
          
          ! Loop through local variable
          
          do i = 1, dst_grid%ncoords
             
             ! Define local variables

             v(1) = dble(src_grid%var(interp%slint_nn(1,i),j))
             
             ! Compute local variables
             
             dst_grid%var(i,j) = real(v(1))
             
          end do ! do i = 1, dst_grid%ncoords
          
       end do ! do j = 1, dst_grid%nz

    end if
       
    !=====================================================================

  end subroutine nrstnghbr
  
  !=======================================================================

  ! SUBROUTINE: 

  ! slint_bilinear.f90

  ! DESCRIPTION: 

  ! This subroutine interpolates a variable to a destination grid
  ! using bi-linear interpolation via the SLINT algorithm.

  ! INPUT VARIABLES:

  ! * src_grid; a FORTRAN grid_struct variable containing the source
  !   grid variable values to be interpolated to the destination grid.

  ! * dst_grid; a FORTRAN grid_struct variable.

  ! * interp; a FORTRAN interp_struct variable containing the
  !   remapping/interpolation attributes as defined by the SLINT
  !   method/algorithm.
  
  ! OUTPUT VARIABLES:

  ! * dst_grid; a FORTRAN grid_struct variable containing the source
  !   grid variable values interpolated to the destination grid
  !   mapping.

  !-----------------------------------------------------------------------

  subroutine slint_bilinear(src_grid,dst_grid,interp)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(interp_struct)                                                 :: interp
    
    ! Define variables computed within routine

    type(spline_struct)                                                 :: spline
    real(r_double)                                                      :: c(3)
    real(r_double)                                                      :: v(3)

    ! Define counting variables

    integer                                                             :: i, j, k

    !=====================================================================

    ! Loop through local variable

    do j = 1, dst_grid%nz
    
       ! Loop through local variable

       do i = 1, dst_grid%ncoords

          ! Check local variable and proceed accordingly

          if((interp%slint_nn(1,i) .ne. interp%slint_nn(2,i)) .and.        &
               & (interp%slint_nn(1,i) .ne. interp%slint_nn(3,i)) .and.    &
               & (interp%slint_nn(2,i) .ne. interp%slint_nn(3,i))) then

             ! Define local variables

             c = interp%slint_coeffs(:,i)
             
             ! Check local variable and proceed accordingly

             if(is_remap_pres .and. (dst_grid%nz .gt. 1)) then

                ! Loop through local variable

                do k = 1, size(v)

                   ! Define local variables

                   spline%n  = dst_grid%nz
                   call variable_interface_setup_struct(spline)
                   spline%xa = src_grid%pres(interp%slint_nn(k,i),:)
                   spline%ya = src_grid%var(interp%slint_nn(k,i),:)
                   spline%x  = dst_grid%pres(i,j)

                   ! Check local variable and proceed accordingly

                   if(is_interp_llp) then

                      ! Define local variables

                      spline%xa = log(spline%xa)
                      spline%x  = log(spline%x)
                      
                   end if ! if(is_interp_llp)

                   ! Compute local variables

                   call interp_spline(spline)

                   ! Check local variable and proceed accordingly

                   if(spline%y .eq. spval) then

                      ! Define local variables

                      v(k) = dble(src_grid%var(interp%slint_nn(k,i),j))

                   else   ! if(spline%y .eq. spval)
                      
                      ! Define local variables

                      v(k) = dble(spline%y)

                   end if ! if(spline%y .eq. spval)
                      
                   ! Deallocate memory for local variables

                   call variable_interface_cleanup_struct(spline)

                end do ! do k = 1, size(v)
                
             else   ! if(is_remap_pres .and. (dst_grid%nz .gt. 1))
          
                ! Define local variables
                
                v(1) = dble(src_grid%var(interp%slint_nn(1,i),j))
                v(2) = dble(src_grid%var(interp%slint_nn(2,i),j))
                v(3) = dble(src_grid%var(interp%slint_nn(3,i),j))

             end if ! if(is_remap_pres .and. (dst_grid%nz .gt. 1))
                
             ! Compute local variables

             dst_grid%var(i,j) = real(c(1)*v(1) + c(2)*v(2) + c(3)*v(3))

          else   ! if((interp%slint_nn(1,i) .ne. interp%slint_nn(2,i))
                 ! .and. (interp%slint_nn(1,i)
                 ! .ne. interp%slint_nn(3,i))
                 ! .and. (interp%slint_nn(2,i)
                 ! .ne. interp%slint_nn(3,i)))

             ! Define local variables

             dst_grid%var(i,j) = spval

          end if ! if((interp%slint_nn(1,i) .ne. interp%slint_nn(2,i))
                 ! .and. (interp%slint_nn(1,i)
                 ! .ne. interp%slint_nn(3,i))
                 ! .and. (interp%slint_nn(2,i)
                 ! .ne. interp%slint_nn(3,i)))
          
       end do ! do i = 1, dst_grid%ncoords

    end do ! do j = 1, dst_grid%nz

    !=====================================================================

  end subroutine slint_bilinear

  !=======================================================================

  ! SUBROUTINE: 
  
  ! slint_nrstnghbr.f90

  ! DESCRIPTION:

  ! This subroutine interpolates a variable to a destination grid
  ! using nearest-neighbor interpolation via the SLINT algorithm.

  ! INPUT VARIABLES:

  ! * src_grid; a FORTRAN grid_struct variable containing the source
  !   grid variable values to be interpolated to the destination grid.

  ! * dst_grid; a FORTRAN grid_struct variable.

  ! * interp; a FORTRAN interp_struct variable containing the
  !   remapping/interpolation attributes as defined by the SLINT
  !   method/algorithm.
  
  ! OUTPUT VARIABLES:

  ! * dst_grid; a FORTRAN grid_struct variable containing the source
  !   grid variable values interpolated to the destination grid
  !   mapping.

  !-----------------------------------------------------------------------

  subroutine slint_nrstnghbr(src_grid,dst_grid,interp)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(interp_struct)                                                 :: interp

    ! Define variables computed within routine

    real(r_double)                                                      :: v(1)

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Loop through local variable

    do j = 1, dst_grid%nz
    
       ! Loop through local variable

       do i = 1, dst_grid%ncoords

          ! Check local variable and proceed accordingly

          if((interp%slint_nn(1,i) .ne. interp%slint_nn(2,i)) .and.        &
               & (interp%slint_nn(1,i) .ne. interp%slint_nn(3,i)) .and.    &
               & (interp%slint_nn(2,i) .ne. interp%slint_nn(3,i))) then
             
             ! Define local variables

             v(1) = dble(src_grid%var(interp%slint_nn(1,i),j))

             ! Compute local variables

             dst_grid%var(i,j) = real(v(1))

          else   ! if((interp%slint_nn(1,i) .ne. interp%slint_nn(2,i))
                 ! .and. (interp%slint_nn(1,i)
                 ! .ne. interp%slint_nn(3,i))
                 ! .and. (interp%slint_nn(2,i)
                 ! .ne. interp%slint_nn(3,i)))

             ! Define local variables

             dst_grid%var(i,j) = spval

          end if ! if((interp%slint_nn(1,i) .ne. interp%slint_nn(2,i))
                 ! .and. (interp%slint_nn(1,i)
                 ! .ne. interp%slint_nn(3,i))
                 ! .and. (interp%slint_nn(2,i)
                 ! .ne. interp%slint_nn(3,i)))
             
       end do ! do i = 1, dst_grid%ncoords

    end do ! do j = 1, dst_grid%nz

    !=====================================================================

  end subroutine slint_nrstnghbr
  
  !=======================================================================
  
end module interpolation_interface
