module interpolation_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: interpolation_interface
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

  use constants_interface
  use grid_methods_interface
  use kinds_interface
  use math_methods_interface
  use mpi_interface
  use namelist_interface
  use slint
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: interpolation_interface_bilinear
  public :: interpolation_interface_init
  public :: interpolation_interface_nnghbr
  public :: interpolation_interface_recenter
  public :: interpolation_interface_spline
  public :: interpolation_interface_vertical
  interface interpolation_interface_bilinear
     module procedure slint_bilinear
  end interface interpolation_interface_bilinear
  interface interpolation_interface_init
     module procedure init_recenter
     module procedure init_slint
  end interface interpolation_interface_init
  interface interpolation_interface_nnghbr
     module procedure slint_nnghbr
  end interface interpolation_interface_nnghbr

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! init_recenter.f90

  ! DESCRIPTION:

  ! This subroutine initializes and defines the grid variable used to
  ! recenter a variable field from a Cartesian (e.g., longitude/latitude
  ! grid) to a coordinate system recentered relative to a user
  ! specified location.

  ! INPUT VARIABLES:

  ! * recenter; a FORTRAN recenter_struct variable.

  ! OUTPUT VARIABLES:

  ! * recenter; a FORTRAN recenter_struct variable containing the
  !   radii and angles for the coordinate projection recentered
  !   relative to the user specified position.

  !-----------------------------------------------------------------------  

  subroutine init_recenter(recenter)

    ! Define variables passed to routine

    type(recenter_struct)                                               :: recenter

    ! Define variables computed within routine

    type(grid_struct)                                                   :: grid
    real(r_kind)                                                        :: latmin
    real(r_kind)                                                        :: latmax
    real(r_kind)                                                        :: lonmin
    real(r_kind)                                                        :: lonmax

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    latmin           = max(recenter%clat - recenter%area,-90.0)
    latmax           = min(recenter%clat + recenter%area,90.0)
    lonmin           = max(recenter%clon - recenter%area,0.0)
    lonmax           = min(recenter%clon + recenter%area,360.0)
    recenter%nx      = ((lonmax - lonmin)/recenter%darea) + 1
    recenter%ny      = ((latmax - latmin)/recenter%darea) + 1
    call variable_interface_setup_struct(recenter)
    recenter%ncoords = 0
    
    ! Loop through local variable

    do j = 1, recenter%ny

       ! Loop through local variable

       do i = 1, recenter%nx
          
          ! Define local variables

          recenter%ncoords               = recenter%ncoords + 1
          recenter%lon(recenter%ncoords) = (lonmin +                        &
               & (i-1)*recenter%darea)
          recenter%lat(recenter%ncoords) = (latmin +                        &
               & (j-1)*recenter%darea)

       end do ! do i = 1, recenter%nx

    end do ! do j = 1, recenter%ny

    ! Define local variables

    grid%nx      = recenter%nx
    grid%ny      = recenter%ny
    grid%ncoords = (grid%nx*grid%ny)
    call variable_interface_setup_struct(grid)
    grid%lon     = recenter%lon
    grid%lat     = recenter%lat

    ! Compute local variables

    call grid_methods_polarcoords(grid,recenter%clon,recenter%clat)

    ! Define local variables

    recenter%radius = grid%radius
    recenter%angle  = grid%angle

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(grid)

    !=====================================================================

  end subroutine init_recenter

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

  ! * remap_grid; a FORTRAN slint_struct variable containing the
  !   source and destination grid geographical coordinates (e.g.,
  !   latitudes and longitudes); all geographical location units are
  !   degrees.

  ! OUTPUT VARIABLES:

  ! * remap_grid; a FORTRAN slint_struct containing the remapping
  !   coefficients and nearest neighbor indices determined from the
  !   source and destination grid_struct FORTRAN structures.

  !-----------------------------------------------------------------------

  subroutine init_slint(remap_grid)

    ! Define variables passed to routine

    type(slint_struct)                                                  :: remap_grid

    ! Define variables computed within routine

    type(grid)                                                          :: slint_grid
    real(r_kind),               dimension(:,:),             allocatable :: grid1
    real(r_kind),               dimension(:,:),             allocatable :: grid2

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid1))                                             &
         & allocate(grid1(remap_grid%src_ncoords,2))
    if(.not. allocated(grid2))                                             &
         & allocate(grid2(remap_grid%dst_ncoords,2))

    ! Define local variables

    grid1(:,1) = remap_grid%src_lat*deg2rad
    grid1(:,2) = remap_grid%src_lon*deg2rad
    grid2(:,1) = remap_grid%dst_lat*deg2rad
    grid2(:,2) = remap_grid%dst_lon*deg2rad

    ! Compute local variables

    call slint_init_compute(grid1,remap_grid%src_ncoords,grid2,            &
         & remap_grid%dst_ncoords,slint_grid)

    ! Define local variables

    remap_grid%coeffs = slint_grid%coeffs
    remap_grid%nn     = slint_grid%nn

    ! Deallocate memory for local variables

    if(allocated(grid2)) deallocate(grid2)
    if(allocated(grid1)) deallocate(grid1)

    !=====================================================================

  end subroutine init_slint

  !=======================================================================

  ! SUBROUTINE:

  ! interpolation_interface_recenter.f90

  ! DESCRIPTION:

  ! This subroutine interpolates a Cartesian grid to grid where the
  ! center is defined by the reference longitude and latitude
  ! coordinate specified by the user; the original grid (i.e., grid)
  ! may be of any allowed projection; the recentered grid (i.e.,
  ! remap_grid) is defined by computing a longitude and latitude grid,
  ! centered at the user specified reference coordinates, and
  ! remapping via bi-linear interpolation to original (e.g.,
  ! Cartesian) grid to the new grid.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the Cartesian
  !   grid longitude and latitude values and the variable values to be
  !   interpolated.

  ! * recenter; a FORTRAN recenter_struct variable containing the
  !   reference longitude and latitude coordinates for the new grid
  !   remapping.

  ! OUTPUT VARIABLES:

  ! * recenter; a FORTRAN recenter_struct variable containing the user
  !   specified variable field remapped to the grid projection
  !   centered at the reference location specified by the user.

  !-----------------------------------------------------------------------

  subroutine interpolation_interface_recenter(grid,recenter)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid
    type(recenter_struct)                                               :: recenter

    ! Define variables computed within routine

    type(slint_struct)                                                  :: remap_slint

    !=====================================================================
    
    ! Define local variables

    call interpolation_interface_init(recenter)
    remap_slint%dst_ncoords = (recenter%nx*recenter%ny)
    remap_slint%src_ncoords = (grid%nx*grid%ny)
    call variable_interface_setup_struct(remap_slint)
    remap_slint%dst_lat     = recenter%lat
    remap_slint%dst_lon     = recenter%lon
    remap_slint%src_lat     = grid%lat
    remap_slint%src_lon     = grid%lon
    call interpolation_interface_init(remap_slint)

    ! Compute local variables

    call interpolation_interface_bilinear(remap_slint,grid%var)

    ! Define local variables

    recenter%var = remap_slint%var

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(remap_slint)

    !=====================================================================

  end subroutine interpolation_interface_recenter

  !=======================================================================

  ! SUBROUTINE:

  ! interpolation_interface_spline.f90

  ! DESCRIPTION:

  ! This subroutine interpolates, using cubic splines, to find the
  ! value of a variable at a location specified by the user; this
  ! subroutine implements the sort2 subroutine of SLATEC and the
  ! spline_cubic_set and spline_cubic_val subroutines of SPLINE.

  ! REFERENCES:

  ! de Boor, C., 1978: A practical guide to splines. Springer-Verlag,
  ! 346pp.

  ! https://people.sc.fsu.edu/~jburkardt/f_src/spline/spline.html

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN spline_struct containing the location (xa) and
  !   variable (ya) arrays as well as the interpolation location (x).

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN interp_spline_struct containing the interpolated
  !   value (y).

  !-----------------------------------------------------------------------

  subroutine interpolation_interface_spline(grid)

    ! Define variables passed to routine

    type(spline_struct)                                                 :: grid

    ! Define variables computed within routine

    type(spline_struct)                                                 :: gridl
    real(r_double),             dimension(:),               allocatable :: xa
    real(r_double),             dimension(:),               allocatable :: ya
    real(r_double),             dimension(:),               allocatable :: y2a
    real(r_double)                                                      :: x
    real(r_double)                                                      :: y
    real(r_double)                                                      :: yp
    real(r_double)                                                      :: ypp
    real(r_double)                                                      :: yp1
    real(r_double)                                                      :: ypn
    integer                                                             :: n

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    n       = count(grid%xa .ne. spval)
    x       = dble(grid%x)
    y       = spval
    gridl%n = n
    call variable_interface_setup_struct(gridl)

    ! Check local variable and proceed accordingly

    if(n .ge. 2) then

       ! Allocate memory for local variables
       
       if(.not. allocated(xa))  allocate(xa(n))
       if(.not. allocated(ya))  allocate(ya(n))
       if(.not. allocated(y2a)) allocate(y2a(n))

       ! Define local variables

       j = 0

       ! Loop through local variable

       do i = 1, size(grid%xa)

          ! Check local variable and proceed accordingly

          if(grid%xa(i) .ne. spval .and. grid%ya(i) .ne. spval) then

             ! Define local variables
             
             j     = j + 1
             xa(j) = dble(grid%xa(i))
             ya(j) = dble(grid%ya(i))

          end if ! if(grid%xa(i) .ne. spval .and. grid%ya(i)
                 ! .ne. spval)

       end do ! do i = 1, size(grid%xa)

       ! Define local variables

       gridl%xa = real(xa)
       gridl%ya = real(ya)
       call math_methods_unique_array(gridl)
       call math_methods_sort_array(gridl,.true.,.false.)
       n        = gridl%n

       ! Deallocate memory for local variables

       if(allocated(xa))  deallocate(xa)
       if(allocated(ya))  deallocate(ya)       
       if(allocated(y2a)) deallocate(y2a)

       ! Allocate memory for local variables
       
       if(.not. allocated(xa))  allocate(xa(n))
       if(.not. allocated(ya))  allocate(ya(n))
       if(.not. allocated(y2a)) allocate(y2a(n))
       
       ! Define local variables

       xa(1:n) = dble(gridl%xa(1:n))
       ya(1:n) = dble(gridl%ya(1:n))
       yp1     = dble(0.0)
       ypn     = dble(0.0)

       ! Check local variable and proceed accordingly

       if(n .lt. 2) goto 1000 

       ! Compute local variables

       call spline_cubic_set(n,xa(1:n),ya(1:n),3,yp1,3,ypn,y2a)

       ! Check local variable and proceed accordingly

       if(minval(xa(1:n)) .le. x .and. maxval(xa(1:n)) .ge. x) then

          ! Compute local variables

          call spline_cubic_val(n,xa(1:n),ya(1:n),y2a(1:n),x,y,yp,ypp)

       end if ! if(minval(xa(1:n)) .le. x .and. maxval(xa(1:n))
              ! .ge. x)

       ! Deallocate memory for local variables

       if(allocated(xa))  deallocate(xa)
       if(allocated(ya))  deallocate(ya)       
       if(allocated(y2a)) deallocate(y2a)

    end if ! if(n .ge. 2)

    ! Define local variables

1000 continue

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(gridl)

    ! Define local variables

    grid%y = real(y)

    !=====================================================================

  end subroutine interpolation_interface_spline

  !=======================================================================

  ! SUBROUTINE:

  ! interpolation_interface_vertical.F90

  ! DESCRIPTION:

  ! This subroutine is the interface layer to the respective vertical
  ! interpolation methods.

  ! INPUT VARIABLES:

  ! * vertgrid; a FORTRAN vertgrid_struct variable; it is assumed that
  !   the respective arrays have been allocated and the source and
  !   destination vertical levels and source grid variable have been
  !   defined.

  ! OPTIONAL INPUT VARIABLES:

  ! * linear; a FORTRAN logical variable specifying whether the
  !   interpolation is to be performed using linear-interpolation
  !   methods.

  ! * llp; a FORTRAN logical variable specifying whether the
  !   interpolation is to be performed assuming linear-log pressure
  !   interpolation methods.

  ! OUTPUT VARIABLES:

  ! * vertgrid; a FORTRAN vertgrid_struct variable containing the
  !   interpolated variable within the destination variable (dst_var)
  !   array.

  !-----------------------------------------------------------------------

  subroutine interpolation_interface_vertical(vertgrid,linear,llp)

    ! Define variables passed to routine

    type(vertgrid_struct)                                               :: vertgrid
    logical, optional                                                   :: linear
    logical, optional                                                   :: llp

    ! Define variables computed within routine

    logical                                                             :: is_linear
    logical                                                             :: is_llp

    !=====================================================================

    ! Define local variables

    is_llp    = .false.
    is_linear = .false.

    ! Check local variable and proceed accordingly

    if(present(linear)) is_linear = linear
    if(present(llp))    is_llp = llp

    ! Compute local variables

    if(is_linear) call linear_interp_vertical(vertgrid)
    if(is_llp)    call linear_log_pressure_interp_vertical(vertgrid)

    !=====================================================================

  end subroutine interpolation_interface_vertical

  !=======================================================================

  ! SUBROUTINE:

  ! linear_interp_vertical.f90

  ! DESCRIPTION:

  ! This subroutine interpolates from a source grid coordinate to a
  ! destination grid vertical coordinate using linear interpolation;
  ! any destination levels that are above or below the source profile
  ! are assigned the highest or lowest source profile value,
  ! respectively (i.e., extrapolation is not performed).

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN vertgrid_struct variable containing the source
  !   and destination coordinate profiles as well as the source grid
  !   variable to be interpolated.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN vertgrid_struct variable where the 'var'
  !   variable has been updated in accordance with the interpolation
  !   to destination grid coordinate profile.

  !-----------------------------------------------------------------------

  subroutine linear_interp_vertical(grid)

    ! Define variables passed to routine

    type(vertgrid_struct)                                               :: grid

    ! Define variables computed within routine

    real(r_kind)                                                        :: denom
    real(r_kind)                                                        :: numer
    real(r_kind)                                                        :: w1
    real(r_kind)                                                        :: w2
    integer                                                             :: idxb
    integer                                                             :: idxt

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================
    
    ! Loop through local variable
    
    do j = 1, grid%dst_nz

       ! Define local variables
       
       idxb = 0
       idxt = 0
       
       ! Loop through local variable
       
       do i = 1, (grid%src_nz - 1)

          ! Check local variable and proceed accordingly

          if((grid%dst_coord(j) .le. grid%src_coord(i)) .and.              &
               & (grid%dst_coord(j) .gt. grid%src_coord(i+1))) then
             
             ! Define local variables
             
             idxb = i
             idxt = (i + 1)
             
          end if ! if((grid%dst_coord(j) .le. grid%src_coord(i))
                 ! .and. (grid%dst_coord(j) .gt. grid%src_coord(i+1)))

       end do ! do i = 1, (grid%src_nz - 1)

       ! Check local variable and proceed accordingly
       
       if((idxb .eq. 0) .or. (idxt .eq. 0)) then
          
          ! Define local variables
          
          idxb = 1
          idxt = grid%src_nz
          
       end if ! if((idxb .eq. 0) .or. (idxt .eq. 0))

       ! Define local variables

       denom = (grid%src_coord(idxt) - grid%src_coord(idxb))
       numer = (grid%src_coord(idxt) - grid%dst_coord(j))
       w1    = 1.0

       if(denom .gt. 0.0) then

          ! Compute local variables

          w1 = (grid%src_coord(idxt) - grid%dst_coord(j))/                 &
               & (grid%src_coord(idxt) - grid%src_coord(idxb))

       end if ! if(denom .gt. 0.0)

       ! Compute local variables

       w2 = 1.0 - w1

       ! Define local variables

       grid%dst_var(j) = spval

       ! Check local variable and proceed accordingly

       if((grid%src_var(idxb) .ne. spval) .and. (grid%src_var(idxt) .ne.   &
            & spval)) then

          ! Compute local variables

          grid%dst_var(j) = w1*grid%src_var(idxb) + w2*grid%src_var(idxt)
       
       end if ! if((grid%src_var(idxb) .ne. spval)
              ! .and. (grid%src_var(idxt) .ne. spval))

    end do !  do j = 1, grid%dst_nz
    
    !=====================================================================

  end subroutine linear_interp_vertical

  !=======================================================================

  ! SUBROUTINE:

  ! linear_log_pressure_interp_vertical.f90

  ! DESCRIPTION:

  ! This subroutine interpolates from a source grid vertical pressure
  ! coordinate to a destination grid vertical pressure coordinate
  ! using linear-log pressure (llp) interpolation; any destination
  ! levels that are above or below the source profile are assigned the
  ! highest or lowest source profile value, respectively (i.e.,
  ! extrapolation is not performed).

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN vertgrid_struct variable containing the source
  !   and destination pressure profiles as well as the source grid
  !   variable to be interpolated.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN vertgrid_struct variable where the 'var'
  !   variable has been updated in accordance with the interpolation
  !   to destination grid pressure profile.

  !-----------------------------------------------------------------------

  subroutine linear_log_pressure_interp_vertical(grid)

    ! Define variables passed to routine

    type(vertgrid_struct)                                               :: grid

    ! Define variables computed within routine

    integer                                                             :: idxb
    integer                                                             :: idxt

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================
    
    ! Loop through local variable
    
    do j = 1, grid%dst_nz
       
       ! Define local variables
       
       idxb = 0
       idxt = 0
       
       ! Loop through local variable
       
       do i = 2, grid%src_nz
          
          ! Check local variable and proceed accordingly
          
          if((grid%dst_prs(j) .le. grid%src_prs(i-1)) .and.                &
               & (grid%dst_prs(j) .gt. grid%src_prs(i))) then
             
             ! Define local variables
             
             idxb = i - 1
             idxt = i
             
          end if ! if((grid%dst_prs(j) .le. grid%src_prs(i-1))
                 ! .and. (grid%dst_prs(j) .gt. grid%src_prs(i)))
          
       end do ! do i = 2, grid%src_nz
       
       ! Check local variable and proceed accordingly
       
       if((idxb .eq. 0) .or. (idxt .eq. 0)) then
          
          ! Define local variables
          
          idxb = 1
          idxt = grid%src_nz
          
       end if ! if((idxb .eq. 0) .or. (idxt .eq. 0))
       
       ! Compute local variables
       
       grid%dst_var(j) = grid%src_var(idxb) + (grid%src_var(idxt) -        &
            & grid%src_var(idxb))*((log(grid%dst_prs(j)) -                 &
            & log(grid%src_prs(idxb)))/(log(grid%src_prs(idxt)) -          &
            & log(grid%src_prs(idxb))))
       
    end do !  do j = 1, grid%dst_nz
    
    !=====================================================================

  end subroutine linear_log_pressure_interp_vertical

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

    ! Loop through local variable

    do i = 1, remap_grid%dst_ncoords
       
       ! Define local variables
       
       c    = remap_grid%coeffs(:,i)
       v(1) = var(remap_grid%nn(1,i))
       v(2) = var(remap_grid%nn(2,i))
       v(3) = var(remap_grid%nn(3,i))

       ! Check local variable and proceed accordingly

       if((v(1) .ne. spval) .and. (v(2) .ne. spval) .and. (v(3) .ne.        &
            & spval)) then
       
          ! Compute local variables

          remap_grid%var(i) = c(1)*v(1) + c(2)*v(2) + c(3)*v(3)

       else   ! if(minval(var) .ne. spval .and. maxval(var) .ne. spval)

          ! Define local variables

          remap_grid%var(i) = spval

       end if ! if(minval(var) .ne. spval .and. maxval(var) .ne. spval)

    end do ! do i = 1, remap_grid%dst_ncoords
       
    !=====================================================================

  end subroutine slint_bilinear

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
