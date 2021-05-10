module gridtrans_fv3_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! gridtrans :: gridtrans_fv3_interface
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
  
  use gridtrans_interface
  
  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: fv3_gridtrans_anlgrid
  public :: fv3_gridtrans_cleanup
  public :: fv3_gridtrans_rotate
  public :: fv3_gridtrans_setup
  public :: fv3_windrotate_compute

  ! Define local variables

  real(gridtrans_realkind),                                   parameter :: half               = 0.5_gridtrans_realkind
  real(gridtrans_realkind),                                   parameter :: one                = 1.0_gridtrans_realkind
  real(gridtrans_realkind),                                   parameter :: pi                 = acos(-1.0)
  real(gridtrans_realkind),                                   parameter :: quarter            = 0.25_gridtrans_realkind
  real(gridtrans_realkind),                                   parameter :: deg2rad            = pi/180.0
  real(gridtrans_realkind),                                   parameter :: rad2deg            = 1.0/deg2rad
  real(gridtrans_realkind),                                   parameter :: sq180              = 180._gridtrans_realkind**2
  real(gridtrans_realkind),                                   parameter :: two                = 2.0_gridtrans_realkind
 
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! fv3_gridtrans_anlgrid.f90

  ! DESCRIPTION:

  ! This subroutine computes a rotated latitude- and longitude
  ! analysis grid centered on the respective Cubed-Sphere Finite
  ! Volume (FV3) tile and completely covering the tile.

  ! INPUT VARIABLES:

  ! * fv3_gridtrans; a FORTRAN fv3_gridtrans_struct variable
  !   containing (at minimum) the FV3 tile corner latitude and
  !   longitude (latt and lont, respectively) values.

  ! OUTPUT VARIABLES:

  ! * fv3_gridtrans; a FORTRAN fv3_gridtrans_struct variable now
  !   containing the rotated latitude- and longitude analysis grid
  !   values (lat_out and lon_out, respectively) centered on the
  !   respective FV3 tile.

  !-----------------------------------------------------------------------

  subroutine fv3_gridtrans_anlgrid(fv3_gridtrans)

    ! Define variables passed to routine

    type(fv3_gridtrans_struct)                                          :: fv3_gridtrans

    ! Define variables computed within routine

    real(gridtrans_realkind),   dimension(:,:),             allocatable :: xc
    real(gridtrans_realkind),   dimension(:,:),             allocatable :: yc
    real(gridtrans_realkind),   dimension(:,:),             allocatable :: zc
    real(gridtrans_realkind)                                            :: rnorm
    real(gridtrans_realkind)                                            :: xcent
    real(gridtrans_realkind)                                            :: ycent
    real(gridtrans_realkind)                                            :: zcent
    
    ! Define counting variables

    integer                                                             :: i, j
    
    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(xc))                                                &
         & allocate(xc(fv3_gridtrans%nx,fv3_gridtrans%ny))
    if(.not. allocated(yc))                                                &
         & allocate(yc(fv3_gridtrans%nx,fv3_gridtrans%ny))    
    if(.not. allocated(zc))                                                &
         & allocate(zc(fv3_gridtrans%nx,fv3_gridtrans%ny))

    ! Loop through local variable

    do j = 1, fv3_gridtrans%ny

       ! Loop through local variable

       do i = 1, fv3_gridtrans%nx

          ! Compute local variables

          xc(i,j) = cos(fv3_gridtrans%latt(i,j)*deg2rad)*                  &
               & cos(fv3_gridtrans%lont(i,j)*deg2rad)
          yc(i,j) = cos(fv3_gridtrans%latt(i,j)*deg2rad)*                  &
               & sin(fv3_gridtrans%lont(i,j)*deg2rad)
          zc(i,j) = sin(fv3_gridtrans%latt(i,j)*deg2rad)
          
       end do ! do i = 1, fv3_gridtrans%nx
          
    end do ! do j = 1, fv3_gridtrans%ny

    ! Compute local variables

    xcent                 = quarter*(xc(1,1) + xc(1,fv3_gridtrans%ny) +    &
         & xc(fv3_gridtrans%nx,1) + xc(fv3_gridtrans%nx,                   &
         & fv3_gridtrans%ny))
    ycent                 = quarter*(yc(1,1) + yc(1,fv3_gridtrans%ny) +    &
         & yc(fv3_gridtrans%nx,1) + yc(fv3_gridtrans%nx,                   &
         & fv3_gridtrans%ny))    
    zcent                 = quarter*(zc(1,1) + zc(1,fv3_gridtrans%ny) +    &
         & zc(fv3_gridtrans%nx,1) + zc(fv3_gridtrans%nx,                   &
         & fv3_gridtrans%ny))
    rnorm                 = one/sqrt(xcent*xcent + ycent*ycent +           &
         & zcent*zcent)
    xcent                 = rnorm*xcent
    ycent                 = rnorm*ycent
    zcent                 = rnorm*zcent
    fv3_gridtrans%lat_cen = asin(zcent)*rad2deg
    fv3_gridtrans%lon_cen = atan2(ycent,xcent)*rad2deg

    ! Define local variables

    fv3_gridtrans%lat_in = reshape(fv3_gridtrans%latt,                     &
         & shape(fv3_gridtrans%lat_in))
    fv3_gridtrans%lon_in = reshape(fv3_gridtrans%lont,                     &
         & shape(fv3_gridtrans%lon_in))
    
    ! Compute local variables

    call fv3_gridtrans_rotate(fv3_gridtrans)
    call fv3_gridtrans_grid(fv3_gridtrans)
    call fv3_gridtrans_unrotate(fv3_gridtrans)
    
    ! Deallocate memory for local variables

    if(allocated(xc)) deallocate(xc)
    if(allocated(yc)) deallocate(yc)
    if(allocated(zc)) deallocate(zc)
    
    !=====================================================================
    
  end subroutine fv3_gridtrans_anlgrid

  !=======================================================================

  ! SUBROUTINE: 

  ! fv3_gridtrans_cleanup.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! fv3_gridtrans_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN fv3_gridtrans_struct variable.

  !-----------------------------------------------------------------------

  subroutine fv3_gridtrans_cleanup(grid)

    ! Define variables passed to routine

    type(fv3_gridtrans_struct)                                          :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%lat))     deallocate(grid%lat)
    if(allocated(grid%latt))    deallocate(grid%latt)
    if(allocated(grid%lat_in))  deallocate(grid%lat_in)
    if(allocated(grid%lat_out)) deallocate(grid%lat_out)
    if(allocated(grid%lon))     deallocate(grid%lon)
    if(allocated(grid%lont))    deallocate(grid%lont)
    if(allocated(grid%lon_in))  deallocate(grid%lon_in)
    if(allocated(grid%lon_out)) deallocate(grid%lon_out)
    
    !=====================================================================

  end subroutine fv3_gridtrans_cleanup

  !=======================================================================

  ! SUBROUTINE:

  ! fv3_gridtrans_grid.f90

  ! DESCRIPTION:

  ! This subroutine computes the grid to be used for all coordinate
  ! transforms.

  ! INPUT VARIABLES:

  ! * fv3_gridtrans; a FORTRAN fv3_gridtrans_struct variable
  !   containing (at minimum) the rotated latitude and longitude
  !   coordinate values (lat_out and lon_out, respectively).

  ! OUTPUT VARIABLES:

  ! * fv3_gridtrans; a FORTRAN fv3_gridtrans_struct variable
  !   containing the grid to be applied for all coordinate transforms
  !   (see lat_in and lon_in, respectively).

  !-----------------------------------------------------------------------

  subroutine fv3_gridtrans_grid(fv3_gridtrans)

    ! Define variables passed to routine

    type(fv3_gridtrans_struct)                                          :: fv3_gridtrans

    ! Define variables computed within routine

    real(gridtrans_realkind),   dimension(:,:),             allocatable :: lats
    real(gridtrans_realkind),   dimension(:,:),             allocatable :: lons
    real(gridtrans_realkind)                                            :: alat
    real(gridtrans_realkind)                                            :: alon
    real(gridtrans_realkind)                                            :: adlat
    real(gridtrans_realkind)                                            :: adlon
    real(gridtrans_realkind)                                            :: clat
    real(gridtrans_realkind)                                            :: clon
    real(gridtrans_realkind)                                            :: dlat
    real(gridtrans_realkind)                                            :: dlon
    integer(gridtrans_intkind)                                          :: nlath
    integer(gridtrans_intkind)                                          :: nlonh

    ! Define counting variables

    integer                                                             :: i, j
    
    !=====================================================================
    
    ! Define local variables

    dlat  = (maxval(fv3_gridtrans%lat_out) -                               &
         & minval(fv3_gridtrans%lat_out))/(fv3_gridtrans%ny - 1)
    dlon  = (maxval(fv3_gridtrans%lon_out) -                               &
         & minval(fv3_gridtrans%lon_out))/(fv3_gridtrans%nx - 1)
    adlat = dlat*fv3_gridtrans%grid_ratio
    adlon = dlon*fv3_gridtrans%grid_ratio
    nlath = fv3_gridtrans%ny/2
    nlonh = fv3_gridtrans%nx/2

    ! Check local variable and proceed accordingly

    if((nlonh*2) .eq. fv3_gridtrans%nx) then

       ! Define local variables

       clon = (adlon/two)

    else   ! if((nlonh*2) .eq. fv3_gridtrans%nx)

       ! Define local variables

       clon = adlon

    end if ! if((nlonh*2) .eq. fv3_gridtrans%nx)

    ! Check local variable and proceed accordingly

    if((nlath*2) .eq. fv3_gridtrans%ny) then

       ! Define local variables

       clat = (adlat/two)

    else   ! if((nlath*2) .eq. fv3_gridtrans%ny)

       ! Define local variables

       clat = adlat

    end if ! if((nlath*2) .eq. fv3_gridtrans%ny)

    ! Allocate memory for local variables

    if(.not. allocated(lats))                                              &
         & allocate(lats(fv3_gridtrans%nx,fv3_gridtrans%ny))
    if(.not. allocated(lons))                                              &
         & allocate(lons(fv3_gridtrans%nx,fv3_gridtrans%ny))

    ! Loop through local variable

    do j = 1, fv3_gridtrans%ny

       ! Loop through local variable

       do i = 1, fv3_gridtrans%nx

          ! Compute local variables

          lats(i,j) = (j - nlath)*adlat - clat
          lons(i,j) = (i - nlonh)*adlon - clon

       end do ! do i = 1, fv3_gridtrans%nx

    end do ! do j = 1, fv3_gridtrans%ny

    ! Define local variables

    fv3_gridtrans%lat_in = reshape(lats,shape(fv3_gridtrans%lat_in))
    fv3_gridtrans%lon_in = reshape(lons,shape(fv3_gridtrans%lon_in))
    
    ! Deallocate memory for local variables

    if(allocated(lats)) deallocate(lats)
    if(allocated(lons)) deallocate(lons)
    
    !=====================================================================

  end subroutine fv3_gridtrans_grid

  !=======================================================================

  ! SUBROUTINE:

  ! fv3_gridtrans_rotate.f90

  ! DESCRIPTION:

  ! This subroutine rotates a right-handed spherical coordinate grid
  ! to a new right-handed spherical coordinate grid as follows:

  ! 1. Define a x-, y-, and z-coordinate system with an origin at the
  !    center of the sphere from the input latitude and longitude
  !    attributes.

  ! 2. Rotate the (x,y,z) coordinate system about the z-axis by amount
  !    cen_lon (see fv3_gridtrans_struct); define a new coordinate
  !    system (xt,yt,zt).

  ! 3. Rotate the (xt,yt,zt) coordinate system about the yt-axis by
  !    amount cen_lat (see fv3_gridtrans_struct); define a new
  !    coordinate system (xtt,ytt,ztt).

  ! 4. Compute the output latitude and longitude coordinates from the
  !    (xtt,ytt,ztt) coordinate system.

  ! INPUT VARIABLES:

  ! * fv3_gridtrans; a FORTRAN fv3_gridtrans_struct variable
  !   containing (at minimum) the input latitude and longitude arrays
  !   (lat_in and lon_in, respectively) and the grid central latitude
  !   and longitude coordinate pair (lat_cen and lon_cen,
  !   respectively).

  ! OUTPUT VARIABLES:

  ! * fv3_gridtrans; a FORTRAN fv3_gridtrans_struct variable now
  !   containing the rotated coordinate system latitude and longitude
  !   values (lat_out and lon_out, respectively.)
  
  !-----------------------------------------------------------------------

  subroutine fv3_gridtrans_rotate(fv3_gridtrans)
  
    ! Define variables passed to routine

    type(fv3_gridtrans_struct)                                          :: fv3_gridtrans

    ! Define variables computed within routine

    real(gridtrans_realkind)                                            :: x
    real(gridtrans_realkind)                                            :: xt
    real(gridtrans_realkind)                                            :: xtt
    real(gridtrans_realkind)                                            :: y
    real(gridtrans_realkind)                                            :: yt
    real(gridtrans_realkind)                                            :: ytt
    real(gridtrans_realkind)                                            :: z
    real(gridtrans_realkind)                                            :: zt
    real(gridtrans_realkind)                                            :: ztt

    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================

    ! Loop through local variable

    do i = 1, fv3_gridtrans%ncoords

       ! Compute local variables

       x   = cos(fv3_gridtrans%lat_in(i)*deg2rad)*                         &
            & cos(fv3_gridtrans%lon_in(i)*deg2rad)
       y   = cos(fv3_gridtrans%lat_in(i)*deg2rad)*                         &
            & sin(fv3_gridtrans%lon_in(i)*deg2rad)
       z   = sin(fv3_gridtrans%lat_in(i)*deg2rad)
       xt  = x*cos(fv3_gridtrans%lon_cen*deg2rad) +                        &
            & y*sin(fv3_gridtrans%lon_cen*deg2rad)
       yt  = -x*sin(fv3_gridtrans%lon_cen*deg2rad) +                       &
            & y*cos(fv3_gridtrans%lon_cen*deg2rad)
       zt  = z
       xtt = xt*cos(fv3_gridtrans%lat_cen*deg2rad) +                       &
            & zt*sin(fv3_gridtrans%lat_cen*deg2rad)
       ytt = yt
       ztt = -xt*sin(fv3_gridtrans%lat_cen*deg2rad) +                      &
            & zt*cos(fv3_gridtrans%lat_cen*deg2rad)

       ! Define local variables

       fv3_gridtrans%lat_out(i) = asin(ztt)*rad2deg
       fv3_gridtrans%lon_out(i) = atan2(ytt,xtt)*rad2deg
       
    end do ! do i = 1, fv3_gridtrans%ncoords

    !=====================================================================
    
  end subroutine fv3_gridtrans_rotate

  !=======================================================================

  ! SUBROUTINE: 

  ! fv3_gridtrans_setup.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! fv3_gridtrans_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN fv3_gridtrans_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN fv3_gridtrans_struct variable where all arrays
  !   are allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine fv3_gridtrans_setup(grid)

    ! Define variables passed to routine

    type(fv3_gridtrans_struct)                                          :: grid

    !=====================================================================

    ! Define local variables
  
    grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat((grid%nx+1),(grid%ny+1)))
    if(.not. allocated(grid%latt))                                         &
         & allocate(grid%latt(grid%nx,grid%ny))
    if(.not. allocated(grid%lat_in))                                       &
         & allocate(grid%lat_in(grid%ncoords))
    if(.not. allocated(grid%lat_out))                                      &
         & allocate(grid%lat_out(grid%ncoords))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon((grid%nx+1),(grid%ny+1)))    
    if(.not. allocated(grid%lont))                                         &
         & allocate(grid%lont(grid%nx,grid%ny))    
    if(.not. allocated(grid%lon_in))                                       &
         & allocate(grid%lon_in(grid%ncoords))
    if(.not. allocated(grid%lon_out))                                      &
         & allocate(grid%lon_out(grid%ncoords))

    !=====================================================================

  end subroutine fv3_gridtrans_setup

  !=======================================================================

  ! SUBROUTINE:

  ! fv3_gridtrans_unrotate.f90

  ! DESCRIPTION:

  ! This subroutine rotates a right-handed spherical coordinate grid
  ! to a new right-handed spherical coordinate grid as follows:

  ! 1. Rotate the (xtt,ytt,ztt) coordinate system about the -axis by
  !    amount cen_lat (see fv3_gridtrans_struct); define a new
  !    coordinate system (xt,yt,zt).

  ! 2. Rotate the (xtt,ytt,ztt) coordinate system about the ytt-axis
  !    by amount cen_lon (see fv3_gridtrans_struct); define a new
  !    coordinate system (xt,yt,zt).

  ! 3. Define a x-, y-, and z-coordinate system with an origin at the
  !    center of the sphere from the (xt,yt,zt) coordinate system.

  ! INPUT VARIABLES:

  ! * fv3_gridtrans; a FORTRAN fv3_gridtrans_struct variable now
  !   containing the rotated coordinate system latitude and longitude
  !   values (lat_out and lon_out, respectively.)

  ! OUTPUT VARIABLES:

  ! * fv3_gridtrans; a FORTRAN fv3_gridtrans_struct variable
  !   containing (at minimum) the input latitude and longitude arrays
  !   (lat_in and lon_in, respectively) and the grid central latitude
  !   and longitude coordinate pair (lat_cen and lon_cen,
  !   respectively).

  !-----------------------------------------------------------------------

  subroutine fv3_gridtrans_unrotate(fv3_gridtrans)
  
    ! Define variables passed to routine

    type(fv3_gridtrans_struct)                                          :: fv3_gridtrans

    ! Define variables computed within routine

    real(gridtrans_realkind)                                            :: x
    real(gridtrans_realkind)                                            :: xt
    real(gridtrans_realkind)                                            :: xtt
    real(gridtrans_realkind)                                            :: y
    real(gridtrans_realkind)                                            :: yt
    real(gridtrans_realkind)                                            :: ytt
    real(gridtrans_realkind)                                            :: z
    real(gridtrans_realkind)                                            :: zt
    real(gridtrans_realkind)                                            :: ztt

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Loop through local variable

    do i = 1, fv3_gridtrans%ncoords

       ! Compute local variables

       xtt = cos(fv3_gridtrans%lat_out(i)*deg2rad)*                        &
            & cos(fv3_gridtrans%lon_out(i)*deg2rad)
       ytt = cos(fv3_gridtrans%lat_out(i)*deg2rad)*                        &
            & sin(fv3_gridtrans%lon_out(i)*deg2rad)
       ztt = sin(fv3_gridtrans%lat_out(i)*deg2rad)
       xt  = xtt*cos(fv3_gridtrans%lat_cen*deg2rad) - ztt*                 &
            & sin(fv3_gridtrans%lat_cen*deg2rad)
       yt  = ytt
       zt  = xtt*sin(fv3_gridtrans%lat_cen*deg2rad) + ztt*                 &
            & cos(fv3_gridtrans%lat_cen*deg2rad)
       x   = xt*cos(fv3_gridtrans%lon_cen*deg2rad) -                       &
            & yt*sin(fv3_gridtrans%lon_cen*deg2rad)
       y   = xt*sin(fv3_gridtrans%lon_cen*deg2rad) +                       &
            & yt*cos(fv3_gridtrans%lon_cen*deg2rad)
       z   = zt

       ! Define local variables

       fv3_gridtrans%lat_in(i) = asin(z)*rad2deg
       fv3_gridtrans%lon_in(i) = atan2(y,x)*rad2deg + 360.0
       
    end do ! do i = 1, fv3_gridtrans%ncoords

    !=====================================================================

  end subroutine fv3_gridtrans_unrotate

  !=======================================================================

  ! SUBROUTINE:

  ! fv_windrotate_compute.f90

  ! DESCRIPTION:

  ! This subroutine computes the coordinate transform values required
  ! to remap wind vector values from model space to Earth-relative
  ! (e.g., observation) space.

  ! INPUT VARIABLES:

  ! * fv3_gridtrans; a FORTRAN fv3_gridtrans_struct variable
  !   containing (at minimum) the rotated coordinate system latitude
  !   and longitude values.

  ! * windrotate; a FORTRAN windrotate_struct variable.

  ! OUTPUT VARIABLES:

  ! * windrotate; a FORTRAN windrotate_struct variable now containing
  !   the coordinate transform variables required to rotate FV3 vector
  !   winds from a model space projection to a Earth-relative (e.g.,
  !   observation) space.

  !-----------------------------------------------------------------------

  subroutine fv3_windrotate_compute(fv3_gridtrans,windrotate)

    ! Define variables passed to routine

    type(fv3_gridtrans_struct)                                          :: fv3_gridtrans
    type(windrotate_struct)                                             :: windrotate

    ! Define variables computed within routine

    real(gridtrans_realkind),   dimension(:,:),             allocatable :: xc
    real(gridtrans_realkind),   dimension(:,:),             allocatable :: yc
    real(gridtrans_realkind),   dimension(:,:),             allocatable :: zc
    real(gridtrans_realkind)                                            :: diff
    real(gridtrans_realkind)                                            :: ewval
    real(gridtrans_realkind)                                            :: nsval
    real(gridtrans_realkind)                                            :: rlat
    real(gridtrans_realkind)                                            :: rlon
    real(gridtrans_realkind)                                            :: uval
    real(gridtrans_realkind)                                            :: vval
    real(gridtrans_realkind)                                            :: xr
    real(gridtrans_realkind)                                            :: yr
    real(gridtrans_realkind)                                            :: zr
    real(gridtrans_realkind)                                            :: xu
    real(gridtrans_realkind)                                            :: yu
    real(gridtrans_realkind)                                            :: zu
    real(gridtrans_realkind)                                            :: xv
    real(gridtrans_realkind)                                            :: yv
    real(gridtrans_realkind)                                            :: zv
    
    ! Define counting variables

    integer                                                             :: i, j
    
    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(xc))                                                &
         & allocate(xc((fv3_gridtrans%nx + 1),(fv3_gridtrans%ny + 1)))
    if(.not. allocated(yc))                                                &
         & allocate(yc((fv3_gridtrans%nx + 1),(fv3_gridtrans%ny + 1)))
    if(.not. allocated(zc))                                                &
         & allocate(zc((fv3_gridtrans%nx + 1),(fv3_gridtrans%ny + 1)))
    
    ! Loop through local variable

    do j = 1, (fv3_gridtrans%ny + 1)

       ! Loop through local variable

       do i = 1, (fv3_gridtrans%nx + 1)

          ! Compute local variables

          xc(i,j) = cos(fv3_gridtrans%lat(i,j)*deg2rad)*                   &
               & cos(fv3_gridtrans%lon(i,j)*deg2rad)
          yc(i,j) = cos(fv3_gridtrans%lat(i,j)*deg2rad)*                   &
               & sin(fv3_gridtrans%lon(i,j)*deg2rad)
          zc(i,j) = sin(fv3_gridtrans%lat(i,j)*deg2rad)
          
       end do ! do i = 1, (fv3_gridtrans%nx + 1)
       
    end do ! do j = 1, (fv3_gridtrans%ny + 1)

    ! Loop through local variable

    do j = 1, (fv3_gridtrans%ny + 1)

       ! Loop through local variable

       do i = 1, fv3_gridtrans%nx

          ! Compute local variables

          rlat = half*(fv3_gridtrans%lat(i,j) + fv3_gridtrans%lat(i+1,j))
          diff = (fv3_gridtrans%lon(i,j) - fv3_gridtrans%lon(i+1,j))**2

          ! Check local variable and proceed accordingly

          if(diff .lt. sq180) then

             ! Compute local variables

             rlon = half*(fv3_gridtrans%lon(i,j) +                         &
                  & fv3_gridtrans%lon(i+1,j))

          else   ! if(diff .lt. sq180)

             ! Compute local variables

             rlon = half*(fv3_gridtrans%lon(i,j) +                         &
                  & fv3_gridtrans%lon(i+1,j) - 360._gridtrans_realkind)

          end if ! if(diff .lt. sq180)

          ! Compute local variables

          xr                    = cos(rlat*deg2rad)*cos(rlon*deg2rad)
          yr                    = cos(rlat*deg2rad)*sin(rlon*deg2rad)
          zr                    = sin(rlat*deg2rad)
          xu                    = xc(i+1,j) - xc(i,j)
          yu                    = yc(i+1,j) - yc(i,j)
          zu                    = zc(i+1,j) - zc(i,j)
          uval                  = sqrt((xu**2 + yu**2 + zu**2))
          ewval                 = sqrt((xr**2 + yr**2))
          nsval                 = sqrt((xr*zr)**2 + (zr*yr)**2 + (xr*xr +  &
               & yr*yr)**2)
          windrotate%cangu(i,j) = (-yr*xu + xr*yu)/ewval/uval
          windrotate%sangu(i,j) = (-xr*zr*xu - zr*yr*yu + (xr*xr +         &
               & yr*yr)*zu)/nsval/uval
          
       end do ! do i = 1, fv3_gridtrans%nx

    end do ! do j = 1, (fv3_gridtrans%ny + 1)

    ! Loop through local variable

    do j = 1, fv3_gridtrans%ny

       ! Loop through local variable

       do i = 1, (fv3_gridtrans%nx + 1)

          ! Compute local variables

          rlat = half*(fv3_gridtrans%lat(i,j) + fv3_gridtrans%lat(i,j+1))
          diff = (fv3_gridtrans%lon(i,j) - fv3_gridtrans%lon(i,j+1))**2

          ! Check local variable and proceed accordingly

          if(diff .lt. sq180) then

             ! Compute local variables

             rlon = half*(fv3_gridtrans%lon(i,j) +                         &
                  & fv3_gridtrans%lon(i,j+1))

          else   ! if(diff .lt. sq180)

             ! Compute local variables

             rlon = half*(fv3_gridtrans%lon(i,j) +                         &
                  & fv3_gridtrans%lon(i,j+1) - 360._gridtrans_realkind)

          end if ! if(diff .lt. sq180)

          ! Compute local variables

          xr                    = cos(rlat*deg2rad)*cos(rlon*deg2rad)
          yr                    = cos(rlat*deg2rad)*sin(rlon*deg2rad)
          zr                    = sin(rlat*deg2rad)
          xv                    = xc(i,j+1) - xc(i,j)
          yv                    = yc(i,j+1) - yc(i,j)
          zv                    = zc(i,j+1) - zc(i,j)
          vval                  = sqrt((xv**2 + yv**2 + zv**2))
          ewval                 = sqrt((xr**2 + yr**2))
          nsval                 = sqrt((xr*zr)**2 + (zr*yr)**2 + (xr*xr +  &
               & yr*yr)**2)
          windrotate%cangv(i,j) = (-yr*xv + xr*yv)/ewval/vval
          windrotate%sangv(i,j) = (-xr*zr*xv - zr*yr*yv + (xr*xr +         &
               & yr*yr)*zv)/nsval/vval
          
       end do ! do i = 1, (fv3_gridtrans%nx + 1)

    end do ! do j = 1, fv3_gridtrans%ny   

    ! Deallocate memory for local variables

    if(allocated(xc)) deallocate(xc)
    if(allocated(yc)) deallocate(yc)
    if(allocated(zc)) deallocate(zc)
    
    !=====================================================================
    
  end subroutine fv3_windrotate_compute
  
  !=======================================================================
  
end module gridtrans_fv3_interface
