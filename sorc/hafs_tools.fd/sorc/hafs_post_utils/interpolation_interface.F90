module interpolation_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! post-utils :: interpolation_interface
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
  use kinds_interface
  use slint
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: interpolation_interface_bilinear
  public :: interpolation_interface_init
  public :: interpolation_interface_mask
  public :: interpolation_interface_nnghbr
  interface interpolation_interface_bilinear
     module procedure slint_bilinear
  end interface interpolation_interface_bilinear
  interface interpolation_interface_init
     module procedure init_slint
  end interface interpolation_interface_init
  interface interpolation_interface_mask
     module procedure slint_mask
  end interface interpolation_interface_mask
  interface interpolation_interface_nnghbr
     module procedure slint_nnghbr
  end interface interpolation_interface_nnghbr

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE: 

  ! init_slint.f90

  ! DESCRIPTION: 

  ! This subroutine initializes and computes the remapping
  ! coefficients and nearest neighbor locations using the geographical
  ! locations passed using the source (src) and destination (dst)
  ! grid_struct FORTRAN structures; all geographical locations are
  ! assumed to be units of degrees.

  ! INPUT VARIABLES:

  ! * src_grid; a FORTRAN grid_struct variable containing the source
  !   grid information; all geographical location units are degrees.

  ! * dst_grid; a FORTRAN grid_struct variable containing the
  !   destination grid information; all geographical location units
  !   are degrees.

  ! * remap_grid; a FORTRAN slint_struct variable.

  ! OUTPUT VARIABLES:

  ! * remap_grid; a FORTRAN slint_struct containing the remapping
  !   coefficients and nearest neighbor indices determined from the
  !   source and destination grid_struct FORTRAN structures.

  !-----------------------------------------------------------------------

  subroutine init_slint(src_grid,dst_grid,remap_grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: src_grid
    type(grid_struct)                                                   :: dst_grid
    type(slint_struct)                                                  :: remap_grid

    ! Define variables computed within routine

    type(grid)                                                          :: slint_grid
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
         & dst_grid%ncoords,slint_grid)

    ! Define local variables

    remap_grid%coeffs      = slint_grid%coeffs
    remap_grid%nn          = slint_grid%nn
    remap_grid%src_ncoords = src_grid%ncoords

    ! Deallocate memory for local variables

    if(allocated(grid2)) deallocate(grid2)
    if(allocated(grid1)) deallocate(grid1)

    !=====================================================================

  end subroutine init_slint

  !=======================================================================

  ! SUBROUTINE: 

  ! slint_bilinear.f90

  ! DESCRIPTION: 

  ! This subroutine interpolates a variable to a destination grid
  ! using bi-linear interpolation means via the SLINT algorithm.

  ! INPUT VARIABLES:

  ! * remap_grid; a FORTRAN slint_struct variable containing the
  !   remapping (from source to destination) coefficients and nearest
  !   neighbors.

  ! * var; a FORTRAN 4-byte real value containing the gridded variable
  !   on the source grid.

  ! OUTPUT VARIABLES:

  ! * remap_grid; a FORTRAN slint_struct variable containing the
  !   gridded source variable interpolated to the destination grid.

  !-----------------------------------------------------------------------

  subroutine slint_bilinear(remap_grid,var)

    ! Define variables passed to routine

    type(slint_struct)                                                  :: remap_grid
    real(r_kind)                                                        :: var(remap_grid%src_ncoords)

    ! Define variables computed within routine

    real(r_double)                                                      :: c(3)
    real(r_double)                                                      :: v(3)

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    remap_grid%var = 0.0

    ! Check local variable and proceed accordingly

    if(minval(var) .ne. spval .and. maxval(var) .ne. spval) then

       ! Loop through local variable

       do i = 1, remap_grid%dst_ncoords

          ! Define local variables

          c    = remap_grid%coeffs(:,i)
          v(1) = var(remap_grid%nn(1,i))
          v(2) = var(remap_grid%nn(2,i))
          v(3) = var(remap_grid%nn(3,i))

          ! Compute local variables

          remap_grid%var(i) = c(1)*v(1) + c(2)*v(2) + c(3)*v(3)

       end do ! do i = 1, remap_grid%dst_ncoords

    else   ! if(minval(var) .ne. spval .and. maxval(var) .ne. spval)

       ! Define local variables

       remap_grid%var = spval

    end if ! if(minval(var) .ne. spval .and. maxval(var) .ne. spval)

    !=====================================================================

  end subroutine slint_bilinear

  !=======================================================================

  ! SUBROUTINE:

  ! slint_mask.f90

  ! DESCRIPTION:

  ! This subroutine determines the edges of the masked region (e.g.,
  ! where the local values are greater than 0.0) and fills all values
  ! within that array interval with the mask value.

  ! INPUT VARIABLES:

  ! * remap_grid; a FORTRAN slint_struct variable.

  ! OUTPUT VARIABLES:

  ! * remap_grid; a FORTRAN slint_struct variable where the mask
  !   attribute has been define accordingly.

  !-----------------------------------------------------------------------

  subroutine slint_mask(remap_grid)

    ! Define variables passed to routine

    type(slint_struct)                                                  :: remap_grid

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: grid_mask
    real(r_kind)                                                        :: mxval
    integer                                                             :: mnxidx
    integer                                                             :: mxxidx
    integer                                                             :: mnyidx
    integer                                                             :: mxyidx

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid_mask))                                         &
         & allocate(grid_mask(remap_grid%dst_nx,remap_grid%dst_ny))

    ! Define local variables

    grid_mask = reshape(remap_grid%mask,shape(grid_mask))

    ! Loop through local variable

    do j = 1, remap_grid%dst_ny

       ! Define local variables

       mxval = maxval(grid_mask(:,j))

       ! Check local variable and proceed accordingly

       if(mxval .gt. 0.0) then

          ! Define local variables

          mnxidx = remap_grid%dst_nx
          mxxidx = 1

          ! Loop through local variable

          do i = 1, remap_grid%dst_nx

             ! Check local variable and proceed accordingly

             if(grid_mask(i,j) .gt. 0.0) then

                ! Define local variables

                mnxidx = min(mnxidx,i)
                mxxidx = max(mxxidx,i)

             end if ! if(grid_mask(i,j) .gt. 0.0)

          end do ! do i = 1, remap_grid%dst_nx

          ! Define local variables

          grid_mask(mnxidx:mxxidx,j) = 1.0

       end if ! if(mxval .gt. 0.0)

    end do ! do j = 1, remap_grid%dst_ny

    ! Loop through local variable

    do i = 1, remap_grid%dst_nx

       ! Define local variables

       mxval = maxval(grid_mask(i,:))

       ! Check local variable and proceed accordingly

       if(mxval .gt. 0.0) then

          ! Define local variables

          mnyidx = remap_grid%dst_ny
          mxyidx = 1

          ! Loop through local variable

          do j = 1, remap_grid%dst_ny

             ! Check local variable and proceed accordingly

             if(grid_mask(i,j) .gt. 0.0) then

                ! Define local variables

                mnyidx = min(mnyidx,j)
                mxyidx = max(mxyidx,j)

             end if ! if(grid_mask(i,j) .gt. 0.0)

          end do ! do j = 1, remap_grid%dst_ny

          ! Define local variables

          grid_mask(i,mnyidx:mxyidx) = 1.0
          
       end if ! if(mxval .gt. 0.0)

    end do ! do i = 1, remap_grid%dst_nx

    ! Define local variables

    remap_grid%mask = reshape(grid_mask,shape(remap_grid%mask))

    ! Deallocate memory for local variables

    if(allocated(grid_mask)) deallocate(grid_mask)

    !=====================================================================

  end subroutine slint_mask

  !=======================================================================

  ! SUBROUTINE: 
  
  ! slint_nnghbr.f90

  ! DESCRIPTION:

  ! This subroutine interpolates a variable to a destination grid
  ! using nearest-neighbor interpolation means via the SLINT
  ! algorithm.

  ! INPUT VARIABLES:

  ! * remap_grid; a FORTRAN slint_struct variable containing the
  !   remapping (from source to destination) coefficients and nearest
  !   neighbors.

  ! * var; a FORTRAN 4-byte real value containing the gridded variable
  !   on the source grid.

  ! OUTPUT VARIABLES:

  ! * remap_grid; a FORTRAN slint_struct variable containing the
  !   gridded source variable interpolated to the destination grid.

  !-----------------------------------------------------------------------

  subroutine slint_nnghbr(remap_grid,var)

    ! Define variables passed to routine

    type(slint_struct)                                                  :: remap_grid
    real(r_kind)                                                        :: var(remap_grid%src_ncoords)

    ! Define variables computed within routine

    real(r_double)                                                      :: v(3)

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    remap_grid%var = 0.0

    ! Loop through local variable

    do i = 1, remap_grid%dst_ncoords

       ! Define local variables

       v(1)              = var(remap_grid%nn(1,i))
       remap_grid%var(i) = v(1)

    end do ! do i = 1, remap_grid%dst_ncoords

    !=====================================================================

  end subroutine slint_nnghbr

  !=======================================================================

end module interpolation_interface
