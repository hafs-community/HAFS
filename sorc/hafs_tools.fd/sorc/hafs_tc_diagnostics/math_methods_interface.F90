! Define include files

include 'mkl_dfti.f90'
include 'mkl_poisson.f90'

module math_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: math_methods_interface
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
  use iso_c_binding
  use kdtree2_module
  use kinds_interface
  use mkl_dfti
  use mkl_poisson
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
#ifdef DARWIN
  include 'fftw3.f03'
#endif
#ifdef LINUX
  include 'fftw3.f'
#endif
  public :: math_methods_9ptsmthr
  public :: math_methods_fftw_dft
  public :: math_methods_kdtree
  public :: math_methods_poisson
  public :: math_methods_pyththrm
  public :: math_methods_recentergrid
  public :: math_methods_sort_array
  public :: math_methods_stats
  public :: math_methods_unique_array
  public :: math_methods_wnd
  interface math_methods_fftw_dft
     module procedure fftw_dft_c2c_2d
  end interface math_methods_fftw_dft
  interface math_methods_sort_array
     module procedure sort_array_linterp
     module procedure sort_array_spline
  end interface math_methods_sort_array
  interface math_methods_unique_array
     module procedure unique_array_linterp
     module procedure unique_array_spline
  end interface math_methods_unique_array  
  interface math_methods_wnd
     module procedure wvnbrdcmp2d
  end interface math_methods_wnd

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! fftw_dft_r2c_2d.f90

  ! DESCRIPTION:

  ! This subroutine computes the discrete Fast Fourier transform (FFT)
  ! for a 2-dimensional real-valued variable using the Fastest Fourier
  ! Transform in the West (FFTW) version 3 software.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN fft2d_struct variable containing, at minimum,
  !   the input/output array and the appropriately defined array
  !   dimensions; it is assumed this FORTRAN structure is initialized
  !   prior to calling this subroutine.

  ! * frwd; a FORTRAN logical variable specifying whether to computed
  !   the forward FFT.

  ! * bkwd; a FORTRAN logical variable specifying whether to computed
  !   the backward FFT.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN fft2d_struct variable containing the output
  !   variable (frwd = .true.) or the input variable (bkwd = .true.).

  !-----------------------------------------------------------------------

  subroutine fftw_dft_c2c_2d(grid,frwd,bkwd)

    ! Define variables passed to routine

    type(fft2d_struct)                                                  :: grid
    logical                                                             :: frwd
    logical                                                             :: bkwd

    ! Define variables computed within routine

    integer(i_long)                                                     :: fftw_frwd
    integer(i_long)                                                     :: fftw_bkwd

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(frwd) then

       ! Define local variables

       call dfftw_plan_dft_2d(fftw_frwd,grid%nx,grid%ny,grid%in,grid%out,   &
            & FFTW_FORWARD,FFTW_ESTIMATE)

       ! Compute local variables

       call dfftw_execute(fftw_frwd)

    end if ! if(frwd)

    ! Check local variable and proceed accordingly

    if(bkwd) then

       ! Define local variables

       call dfftw_plan_dft_2d(fftw_bkwd,grid%nx,grid%ny,grid%out,grid%in,   &
            & FFTW_BACKWARD,FFTW_ESTIMATE)

       ! Compute local variables

       call dfftw_execute(fftw_bkwd)

       ! Define local variables

       grid%in = grid%in/dble(grid%nx*grid%ny)

    end if ! if(bkwd)

    !=====================================================================

  end subroutine fftw_dft_c2c_2d

  !=======================================================================

  ! SUBROUTINE: 

  ! init_stats.f90

  ! DESCRIPTION:

  ! This subroutine initializes a statgrid_struct variable.

  ! INPUT VARIABLES:

  ! * statgrid; an uninitialized FORTRAN statgrid_struct variable.

  ! OUTPUT VARIABLES:

  ! * statgrid; an initialized FORTRAN statgrid_struct variable.

  !-----------------------------------------------------------------------

  subroutine init_stats(statgrid)

    ! Define variables passed to routine

    type(statgrid_struct)                                               :: statgrid

    !=====================================================================

    ! Define local variables

    statgrid%varmin = spval
    statgrid%varmax = spval
    statgrid%mean   = spval
    statgrid%vari   = spval
    statgrid%nvals  = 0

    !=====================================================================

  end subroutine init_stats

  !=======================================================================

  ! SUBROUTINE: 

  ! math_methods_9ptsmthr.f90

  ! DESCRIPTION:

  ! This subroutine applies a 9-point smoother to a user specified
  ! variable grid; the 9-point smoother is applied in accordance
  ! within the range of index positions (e.g., the minx, maxx, miny,
  ! maxy attributes).

  ! INPUT VARIABLES:

  ! * smthr9pt; a FORTRAN smthr9pt_struct variable containing the
  !   variable to be smooth and the index positions.

  ! OUTPUT VARIABLES:

  ! * smthr9pt; a FORTRAN smthr9pt_struct variable containing the
  !   smoothed variable.

  !-----------------------------------------------------------------------

  subroutine math_methods_9ptsmthr(smthr9pt)

    ! Define variables passed to routine

    type(smthr9pt_struct)                                               :: smthr9pt

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: ingrid
    real(r_kind),               dimension(:,:),             allocatable :: outgrid
    integer                                                             :: xmin
    integer                                                             :: xmax
    integer                                                             :: ymin
    integer                                                             :: ymax

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(ingrid))                                            &
         & allocate(ingrid(smthr9pt%nx,smthr9pt%ny))
    if(.not. allocated(outgrid))                                           &
         & allocate(outgrid(smthr9pt%nx,smthr9pt%ny))
    
    ! Define local variables

    ingrid  = reshape(smthr9pt%var,shape(ingrid))
    outgrid = ingrid

    ! Loop through local variable
    
    do j = smthr9pt%miny, smthr9pt%maxy
          
       ! Loop throught local variable
       
       do i = smthr9pt%minx, smthr9pt%maxx

          ! Check local variable and proceed accordingly

          if((ingrid(i-1,j-1) .ne. spval) .and. (ingrid(i,j-1) .ne.        &
               & spval) .and. (ingrid(i+1,j-1) .ne. spval) .and.           &
               & (ingrid(i-1,j) .ne. spval) .and. (ingrid(i,j) .ne.        &
               & spval) .and. (ingrid(i+1,j) .ne. spval) .and.             &
               & (ingrid(i-1,j+1) .ne. spval) .and. (ingrid(i,j+1) .ne.    &
               & spval) .and. (ingrid(i+1,j+1) .ne. spval)) then
             
             ! Compute local variables

             outgrid(i,j) = ((ingrid(i-1,j-1) + ingrid(i,j-1) +            &
                  & ingrid(i+1,j-1) + ingrid(i-1,j) + ingrid(i,j) +        &
                  & ingrid(i+1,j) + ingrid(i-1,j+1) + ingrid(i,j+1) +      &
                  & ingrid(i+1,j+1))/9.0)

          else   ! if((ingrid(i-1,j-1) .ne. spval)
                 ! .and. (ingrid(i,j-1) .ne. spval)
                 ! .and. (ingrid(i+1,j-1) .ne. spval)
                 ! .and. (ingrid(i-1,j) .ne. spval) .and. (ingrid(i,j)
                 ! .ne. spval) .and. (ingrid(i+1,j) .ne. spval)
                 ! .and. (ingrid(i-1,j+1) .ne. spval)
                 ! .and. (ingrid(i,j+1) .ne. spval)
                 ! .and. (ingrid(i+1,j+1) .ne. spval))

             ! Define local variables

             outgrid(i,j) = spval

          end if ! if((ingrid(i-1,j-1) .ne. spval)
                 ! .and. (ingrid(i,j-1) .ne. spval)
                 ! .and. (ingrid(i+1,j-1) .ne. spval)
                 ! .and. (ingrid(i-1,j) .ne. spval) .and. (ingrid(i,j)
                 ! .ne. spval) .and. (ingrid(i+1,j) .ne. spval)
                 ! .and. (ingrid(i-1,j+1) .ne. spval)
                 ! .and. (ingrid(i,j+1) .ne. spval)
                 ! .and. (ingrid(i+1,j+1) .ne. spval))

       end do ! do i = smthr9pt%minx, smthr9pt%maxx

    end do ! do j = smthr9pt%miny, smthr9pt%maxy

    ! Define local variables

    smthr9pt%var = reshape(outgrid,shape(smthr9pt%var))

    ! Deallocate memory for local variables

    if(allocated(ingrid))  deallocate(ingrid)
    if(allocated(outgrid)) deallocate(outgrid)

    !=====================================================================

  end subroutine math_methods_9ptsmthr
  
  !=======================================================================

  ! SUBROUTINE:

  ! math_methods_kdtree.f90

  ! DESCRIPTION:

  ! This subroutine implements a KD-tree search algorithm to locate
  ! the N-nearest neighbor locations for a user specified geographical
  ! location.

  ! REFERENCES:

  ! Kennel, M. B., 2004: KDTREE2: Fortran 95 and C++ software to
  ! efficiently search for near neighbors in a multi-dimensional
  ! Euclidean space. 

  ! http://arxiv.org/PScache/phvsics/pdf/0408/0408067.pdf.

  ! INPUT VARIABLES:
  
  ! * src_grid; a FORTRAN grid_struct variable containing the
  !   Euclidean space geographical locations within which to find
  !   nearest-neighbors; geographical locations (e.g., latitudes and
  !   longitudes) are assumed to have units of degrees.

  ! * dst_grid; a FORTRAN grid_struct variable containing the
  !   geographical locations for which to find the N-nearest neighbor
  !   locations; geographical locations (e.g., latitudes and
  !   longitudes) are assumed to have units of degrees.

  ! * kdtree; a FORTRAN kdtree_struct variable containing (at minimum)
  !   the number of coordinate values (the ncoords attribute should be
  !   equal to the dst_grid variable ncoords attribute) and the number
  !   of nearest-neighbors to seek (the nn attribute should be less
  !   than or equal to the src_grid variable ncoords attribute).

  ! OUTPUT VARIABLES:

  ! * kdtree; a FORTRAN kdtree_struct variable containing the
  !   N-nearest neighbor R^2 distances and src_grid variable
  !   coordinate values (r2dist and idx attributes, respectively); the
  !   units for the r2dist attribute are meters squared.

  !-----------------------------------------------------------------------

  subroutine math_methods_kdtree(src_grid,dst_grid,kdtree)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(kdtree_struct)                                                 :: kdtree

    ! Define variables computed within routine

    type(kdtree2), pointer                                              :: kdtree2
    type(kdtree2_result)                                                :: sresults(kdtree%nn)
    real(r_kind),               dimension(:,:),             allocatable :: src_grdloc
    real(r_kind)                                                        :: dst_grdloc(3)

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(src_grdloc))                                        &
         & allocate(src_grdloc(3,src_grid%ncoords))

    ! Loop through local variables

    do i = 1, src_grid%ncoords

       ! Compute local variables

       src_grdloc(1,i) = rearth_equator*cos(src_grid%lat(i)*deg2rad)*      &
            & cos(src_grid%lon(i)*deg2rad)
       src_grdloc(2,i) = rearth_equator*cos(src_grid%lat(i)*deg2rad)*      &
            & sin(src_grid%lon(i)*deg2rad)
       src_grdloc(3,i) = rearth_equator*sin(src_grid%lat(i)*deg2rad)

    end do ! do i = 1, src_grid%ncoords

    ! Compute local variables

    kdtree2 => kdtree2_create(src_grdloc,sort=.true.,rearrange=.true.)

    ! Loop through local variables

    do i = 1, dst_grid%ncoords

       ! Compute local variables

       dst_grdloc(1) = rearth_equator*cos(dst_grid%lat(i)*deg2rad)*        &
            & cos(dst_grid%lon(i)*deg2rad)
       dst_grdloc(2) = rearth_equator*cos(dst_grid%lat(i)*deg2rad)*        &
            & sin(dst_grid%lon(i)*deg2rad)
       dst_grdloc(3) = rearth_equator*sin(dst_grid%lat(i)*deg2rad)
       
       ! Define local variables

       call kdtree2_n_nearest(tp=kdtree2,qv=dst_grdloc,nn=kdtree%nn,       &
            & results=sresults)
       kdtree%r2dist(i,1:kdtree%nn) = sresults(1:kdtree%nn)%dis
       kdtree%idx(i,1:kdtree%nn)    = sresults(1:kdtree%nn)%idx

    end do ! do i = 1, dst_grid%ncoords

    ! Deallocate memory for local variables

    call kdtree2_destroy(kdtree2)
    if(allocated(src_grdloc)) deallocate(src_grdloc)

    !=====================================================================

  end subroutine math_methods_kdtree

  !=======================================================================

  ! SUBROUTINE:

  ! math_methods_poisson.f90

  ! DESCRIPTION:

  ! This subroutine is the interface/wrapper to the Intel MKL
  ! Fast-Poisson Solver routines; all computations within are
  ! double-precision.

  ! INPUT VARIABLES:

  ! * poisson; a FORTRAN poisson_struct variable containing the
  !   boundary condition type (the bctype attribute), the boundary
  !   condition values (the attributes bd_a[xy] and bd_b[xy]), the
  !   grid information (the [xy][min/max] attributes), and the
  !   right-hand side of the equation (the rhs attribute).

  ! OUTPUT VARIABLES:

  ! * poisson; a FORTRAN poisson_struct variable containing the
  !   solution to the Poisson equation specified by the user (within
  !   the sol attribute).

  !-----------------------------------------------------------------------

  subroutine math_methods_poisson(poisson)

    ! Define variables passed to routine

    type(poisson_struct)                                                :: poisson

    ! Define variables computed within routine

    type(DFTI_DESCRIPTOR), pointer                                      :: xhandle
    type(DFTI_DESCRIPTOR), pointer                                      :: yhandle
    real(r_double),             dimension(:),               allocatable :: dpar
    integer                                                             :: ipar(128)
    integer                                                             :: stat

    !=====================================================================

    ! Allocate memory for local variables
    
    call mkl_free_buffers()
    if(.not. allocated(dpar)) allocate(dpar(13*poisson%nx/2 + 7))

    ! Define local variables
    
    ipar        = 0
    poisson%sol = poisson%rhs
    
    ! Compute local variables

    call d_init_helmholtz_2d(poisson%xmin,poisson%xmax,poisson%ymin,       &
         & poisson%ymax,poisson%nx,poisson%ny,poisson%bctype,poisson%q,    &
         & ipar,dpar,stat)
    call d_commit_helmholtz_2d(poisson%sol,poisson%bd_ax,poisson%bd_bx,    &
         & poisson%bd_ay,poisson%bd_by,xhandle,ipar,dpar,stat)
    call d_helmholtz_2d(poisson%sol,poisson%bd_ax,poisson%bd_bx,           &
         & poisson%bd_ay,poisson%bd_by,xhandle,ipar,dpar,stat)

    ! Define local variables

    call free_helmholtz_2d(xhandle,ipar,stat)

    ! Deallocate memory for local variables

    if(allocated(dpar)) deallocate(dpar)

    !=====================================================================
    
  end subroutine math_methods_poisson

  !=======================================================================

  ! SUBROUTINE:

  ! math_methods_pyththrm.f90

  ! DESCRIPTION:

  ! This subroutine computes the attributes of a right triangle
  ! assuming the Pythagorean theorem.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN pyththrm_struct variable; all provided distance
  !   values are assumed in meters and all angle values are assumed in
  !   degrees.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN pyththrm_struct variable containing all
  !   attributes for a right triangle computed assuming the attributes
  !   of the Pythagorean theorem.

  !-----------------------------------------------------------------------

  subroutine math_methods_pyththrm(grid)

    ! Define variables passed to routine

    type(pyththrm_struct)                                               :: grid

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(grid%a .ne. spval .and. grid%b .ne. spval) then

       ! Compute local variables

       grid%c = sqrt(grid%a*grid%a + grid%b*grid%b)

    else if(grid%a .ne. spval .and. grid%c .ne. spval) then

       ! Compute local variables

       grid%b = sqrt(grid%c*grid%c - grid%a*grid%a)

    else if(grid%b .ne. spval .and. grid%c .ne. spval) then

       ! Compute local variables

       grid%a = sqrt(grid%c*grid%c - grid%b*grid%b)

    else if(grid%anga .ne. spval .and. grid%c .ne. spval) then

       ! Compute local variables

       grid%a = grid%c*cos(grid%anga*deg2rad)
       grid%b = sqrt(grid%c*grid%c - grid%a*grid%a)

    else if(grid%angc .ne. spval .and. grid%c .ne. spval) then

       ! Compute local variables

       grid%a = grid%c*sin(grid%angc*deg2rad)
       grid%b = sqrt(grid%c*grid%c - grid%a*grid%a)

    else

       ! Define local variables

       write(6,500)
       stop

    end if ! if(grid%a .ne. spval .and. grid%b .ne. spval)

    ! Compute local variables

    grid%anga = asin(grid%b/grid%c)*rad2deg
    
    ! Check local variable and proceed accordingly

    if(grid%a .le. 0.0) grid%a = 1.e-6
    if(grid%b .le. 0.0) grid%b = 1.e-6

    ! Compute local variables

    grid%angb = acos(0./(grid%a*grid%b))*rad2deg
    grid%angc = asin(grid%a/grid%c)*rad2deg

    ! Define local variables

500 format('MATH_METHODS_PYTHTHRM: Not enough information has been ',      &
         & 'provided to compute the attributes of the Pythagorean ',       &
         & 'Theorem. Aborting!!!')

    !=====================================================================

  end subroutine math_methods_pyththrm

  !=======================================================================

  ! SUBROUTINE:

  ! math_methods_recentergrid.f90

  ! DESCRIPTION:

  ! This subroutine recenters a user specified variable relative to
  ! the center grid-coordinate(s).

  ! INPUT VARIABLES:

  ! * recenter; a FORTRAN recenter_struct variable containing the grid
  !   x- and y-indices (idx and jdx, respectively) and the variable to
  !   be transformed (within the var attribute) in accordance with the
  !   optional variables (see below).

  ! OPTIONAL INPUT VARIABLES:

  ! * frwd; a FORTRAN logical variable specifying whether to
  !   remap/recenter the variable values to the center of the grid.

  ! * bkwd; a FORTRAN logical variable specifying whether to remap
  !   from a recentered variable to the original variable projection.

  ! OUTPUT VARIABLES:

  ! * recenter; a FORTRAN recenter_struct variable containing the
  !   transformed variable grid in accordance with the optional
  !   variables (frwd and bkwd).

  !-----------------------------------------------------------------------

  subroutine math_methods_recentergrid(recenter,frwd,bkwd)

    ! Define variables passed to routine

    type(recenter_struct)                                               :: recenter
    logical, optional                                                   :: frwd
    logical, optional                                                   :: bkwd

    ! Define variables computed within routine

    logical                                                             :: is_frwd
    logical                                                             :: is_bkwd
    real(r_kind),               dimension(:,:),             allocatable :: var
    integer                                                             :: xc
    integer                                                             :: xd
    integer                                                             :: yc
    integer                                                             :: yd

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(recenter%ridx .eq. 0) goto 1000

    ! Allocate memory for local variables

    if(.not. allocated(var)) allocate(var(recenter%nx,recenter%ny))

    ! Define local variables

    is_frwd = .false.
    is_bkwd = .false.
    if(present(frwd)) is_frwd = frwd
    if(present(bkwd)) is_bkwd = bkwd
    var     = reshape(recenter%var,shape(var))
    xd      = nint(recenter%nx/2.0) - recenter%idx(recenter%ridx)
    yd      = nint(recenter%ny/2.0) - recenter%jdx(recenter%ridx)

    ! Check local variable and proceed accordingly

    if(is_frwd) then

       ! Loop through local variable

       do i = 1, recenter%ncoords

          ! Define local variables

          xc = recenter%idx(i) + xd
          yc = recenter%jdx(i)

          ! Check local variable and proceed accordingly

          if(xc .gt. recenter%nx) xc = (xc - recenter%nx)
          if(xc .lt. 1)           xc = (recenter%nx + xc)

          ! Define local variables

          var(xc,yc) = recenter%var(i)

       end do ! do i = 1, recenter%ncoords

    end if ! if(is_frwd)

    ! Check local variable and proceed accordingly

    if(is_bkwd) then

       ! Loop through local variable
       
       do i = 1, recenter%ncoords
          
          ! Define local variables
          
          xc = recenter%idx(i) - xd
          yc = recenter%jdx(i)

          ! Check local variable and proceed accordingly

          if(xc .gt. recenter%nx) xc = (xc - recenter%nx)
          if(xc .lt. 1)           xc = (recenter%nx + xc)

          ! Define local variables

          var(xc,yc) = recenter%var(i)

       end do ! do i = 1, recenter%ncoords

    end if ! if(is_bkwd)

    ! Define local variables

    recenter%var = reshape(var,shape(recenter%var))

    ! Deallocate memory for local variables

    if(allocated(var)) deallocate(var)

    ! Define local variables

1000 continue

    !=====================================================================

  end subroutine math_methods_recentergrid
  
  !=======================================================================

  ! SUBROUTINE: 

  ! math_methods_stats.f90

  ! DESCRIPTION:

  ! This subroutine defines/computes the attributes of a variable
  ! array and returns the respective attibutes.

  ! INPUT VARIABLES:

  ! * vargrid; a FORTRAN vargrid_struct variable.

  ! OUTPUT VARIABLES:

  ! * statgrid; a FORTRAN statgrid_struct variable.

  !-----------------------------------------------------------------------

  subroutine math_methods_stats(vargrid,statgrid)

    ! Define variables passed to routine

    type(vargrid_struct)                                                :: vargrid
    type(statgrid_struct)                                               :: statgrid

    ! Define variables computed within routine

    real(r_kind)                                                        :: sum
    real(r_kind)                                                        :: sumsq
    real(r_kind)                                                        :: varmin
    real(r_kind)                                                        :: varmax
    integer                                                             :: count

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call init_stats(statgrid)
    varmin = spval
    varmax = -spval
    sum    = 0.0
    count  = 0

    ! Loop through local variable

    do i = 1, vargrid%nvals

       ! Check local variable and proceed accordingly

       if(vargrid%var(i) .ne. spval) then

          ! Define local variables

          varmin = min(varmin,vargrid%var(i))
          varmax = max(varmax,vargrid%var(i))

          ! Compute local variables

          sum   = sum + vargrid%var(i)
          count = count + 1

       end if ! if(vargrid%var(i) .ne. spval)
       
    end do ! do i = 1, vargrid%nvals

    ! Check local variable and proceed accordingly

    if(abs(varmax) .eq. spval) varmax          = spval
    if(varmin .ne. spval)      statgrid%varmin = varmin
    if(varmax .ne. spval)      statgrid%varmax = varmax
    if(count .gt. 0)           statgrid%mean   = sum/real(count)
    if(statgrid%mean .ne. spval) then
    
       ! Define local variables

       sum   = 0.0
       sumsq = 0.0

       ! Loop through local variable

       do i = 1, vargrid%nvals

          ! Check local variable and proceed accordingly

          if(vargrid%var(i) .ne. spval) then

             ! Compute local variables

             sum   = sum   + (vargrid%var(i) - statgrid%mean)
             sumsq = sumsq + ((vargrid%var(i) - statgrid%mean)*           &
                  & (vargrid%var(i) - statgrid%mean))

          end if ! if(vargrid%var(i) .ne. spval)

       end do ! do i = 1, vargrid%nvals

       ! Check local variable and proceed accordingly

       if(count .gt. 1) then

          ! Compute local variables

          statgrid%vari  = (sumsq - (sum*sum)/count)/(count - 1)
          statgrid%stdev = sqrt(statgrid%vari)

       end if ! if(count .gt. 1)

    end if ! if(statgrid%mean .ne. spval)

    ! Define local variables

    statgrid%nvals = count

    !=====================================================================

  end subroutine math_methods_stats

  !=======================================================================

  ! SUBROUTINE:

  ! sort_array_linterp.f90

  ! DESCRIPTION:

  ! This subroutine implements the SLATEC ssort routine to sort a
  ! dependent array (ya) relative to a sorted independent array (xa);
  ! the arrays may be sorted in either the ascending or descending
  ! direction.

  ! REFERENCES:

  ! Singleton, R.C., 1969. Algorithm 347: an efficient algorithm for
  ! sorting with minimal storage [M1]. Communications of the ACM,
  ! 12(3), pp.185-186.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN linterp_struct containing the independent (xa)
  !   and dependent (ya) variable arrays to be sorted.

  ! * ascend; a FORTRAN logical variable specifying whether the arrays
  !   are to be sorted in the ascending direction.

  ! * descend; a FORTRAN logical variable specifying whether the
  !   arrays are to be sorted in the descending direction.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN linterp_struct containing the sorted independent
  !   (xa) and dependent (ya) variable arrays.

  !-----------------------------------------------------------------------

  subroutine sort_array_linterp(grid,ascend,descend)

    ! Define variables passed to routine

    type(linterp_struct)                                                :: grid
    logical                                                             :: ascend
    logical                                                             :: descend

    ! Define variables computed within routine

    real(r_kind),               dimension(:),               allocatable :: xa
    real(r_kind),               dimension(:),               allocatable :: ya
    integer                                                             :: kflag
    integer                                                             :: n

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(ascend)  kflag = 2
    if(descend) kflag = -2

    ! Define local variables

    n = grid%n

    ! Check local variable and proceed accordingly

    if(n .ge. 2) then

       ! Allocate memory for local variables
       
       if(.not. allocated(xa)) allocate(xa(n))
       if(.not. allocated(ya)) allocate(ya(n))

       ! Define local variables

       xa      = grid%xa
       ya      = grid%ya
       call ssort(xa,ya,n,kflag)
       grid%xa = xa
       grid%ya = ya

       ! Deallocate memory for local variables
       
       if(allocated(xa)) deallocate(xa)
       if(allocated(ya)) deallocate(ya)

    end if ! if(n .ge. 2)

    !=====================================================================

  end subroutine sort_array_linterp  

  !=======================================================================

  ! SUBROUTINE:

  ! sort_array_spline.f90

  ! DESCRIPTION:

  ! This subroutine implements the SLATEC ssort routine to sort a
  ! dependent array (ya) relative to a sorted independent array (xa);
  ! the arrays may be sorted in either the ascending or descending
  ! direction.

  ! REFERENCES:

  ! Singleton, R.C., 1969. Algorithm 347: an efficient algorithm for
  ! sorting with minimal storage [M1]. Communications of the ACM,
  ! 12(3), pp.185-186.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN spline_struct containing the independent (xa)
  !   and dependent (ya) variable arrays to be sorted.

  ! * ascend; a FORTRAN logical variable specifying whether the arrays
  !   are to be sorted in the ascending direction.

  ! * descend; a FORTRAN logical variable specifying whether the
  !   arrays are to be sorted in the descending direction.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN spline_struct containing the sorted independent
  !   (xa) and dependent (ya) variable arrays.

  !-----------------------------------------------------------------------

  subroutine sort_array_spline(grid,ascend,descend)

    ! Define variables passed to routine

    type(spline_struct)                                                 :: grid
    logical                                                             :: ascend
    logical                                                             :: descend

    ! Define variables computed within routine

    real(r_kind),               dimension(:),               allocatable :: xa
    real(r_kind),               dimension(:),               allocatable :: ya
    integer                                                             :: kflag
    integer                                                             :: n

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(ascend)  kflag = 2
    if(descend) kflag = -2

    ! Define local variables

    n = grid%n

    ! Check local variable and proceed accordingly

    if(n .ge. 2) then

       ! Allocate memory for local variables
       
       if(.not. allocated(xa)) allocate(xa(n))
       if(.not. allocated(ya)) allocate(ya(n))

       ! Define local variables

       xa      = grid%xa
       ya      = grid%ya
       call ssort(xa,ya,n,kflag)
       grid%xa = xa
       grid%ya = ya

       ! Deallocate memory for local variables
       
       if(allocated(xa)) deallocate(xa)
       if(allocated(ya)) deallocate(ya)

    end if ! if(n .ge. 2)

    !=====================================================================

  end subroutine sort_array_spline

  !=======================================================================

  ! SUBROUTINE:

  ! unique_array_linterp.f90

  ! DESCRIPTION:

  ! This subroutine returns an array containing only unique values;
  ! this method is utilitized by linear interpolation routines which
  ! require absolute ascending/descending arrays/lists in order to
  ! perform their respective algorithms.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN linterp_struct variable containing the values
  !   defined by the user for spline calculations;

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN linterp_struct variable which is a modified
  !   version of the input FORTRAN linterp_struct variable assuming
  !   the FORTRAN linterp_struct variable does not contain entirely
  !   unique values.

  !-----------------------------------------------------------------------

  subroutine unique_array_linterp(grid)

    ! Define variables passed to routine

    type(linterp_struct)                                                :: grid

    ! Define variables computed within routine

    type(linterp_struct)                                                :: gridl
    real(r_kind)                                                        :: max_val
    real(r_kind)                                                        :: min_val
    integer                                                             :: nn

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    gridl%n  = grid%n
    call variable_interface_setup_struct(gridl)
    gridl%xa = spval
    gridl%ya = spval
    max_val  = maxval(grid%xa)
    min_val  = minval(grid%xa)
    nn       = 0

    ! Loop through local variable

    do while(min_val .lt. max_val)

       ! Define local variables

       nn           = nn + 1
       min_val      = minval(grid%xa,mask=(grid%xa .gt. min_val))
       gridl%xa(nn) = min_val

       ! Loop through local variable

       do i = 1, grid%n

          ! Check local variable and proceed accordingly

          if(gridl%xa(nn) .eq. grid%xa(i)) then

             ! Define local variables

             gridl%ya(nn) = grid%ya(i)
             goto 1000

          end if ! if(gridl%xa(nn) .eq. grid%xa(i))

       end do ! do i = 1, grid%n
       
       ! Define local variables
       
1000   continue
       
    end do ! do while(min_val .lt. max_val)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(grid)

    ! Define local variables

    grid%n  = count(gridl%xa .ne. spval)
    call variable_interface_setup_struct(grid)
    grid%xa = gridl%xa(1:grid%n)
    grid%ya = gridl%ya(1:grid%n)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(gridl)

    !=====================================================================

  end subroutine unique_array_linterp

  !=======================================================================

  ! SUBROUTINE:

  ! unique_array_spline.f90

  ! DESCRIPTION:

  ! This subroutine returns an array containing only unique values;
  ! this method is utilitized by spline routines which require
  ! absolute ascending/descending arrays/lists in order to perform
  ! their respective algorithms.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN spline_struct variable containing the values
  !   defined by the user for spline calculations;

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN spline_struct variable which is a modified
  !   version of the input FORTRAN spline_struct variable assuming the
  !   FORTRAN spline_struct variable does not contain entirely unique
  !   values.

  !-----------------------------------------------------------------------

  subroutine unique_array_spline(grid)

    ! Define variables passed to routine

    type(spline_struct)                                                 :: grid

    ! Define variables computed within routine

    type(spline_struct)                                                 :: gridl
    real(r_kind)                                                        :: max_val
    real(r_kind)                                                        :: min_val
    integer                                                             :: nn

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    gridl%n  = grid%n
    call variable_interface_setup_struct(gridl)
    gridl%xa = spval
    gridl%ya = spval
    max_val  = maxval(grid%xa)
    min_val  = minval(grid%xa)
    nn       = 0

    ! Loop through local variable

    do while(min_val .lt. max_val)

       ! Define local variables

       nn           = nn + 1
       min_val      = minval(grid%xa,mask=(grid%xa .gt. min_val))
       gridl%xa(nn) = min_val

       ! Loop through local variable

       do i = 1, grid%n

          ! Check local variable and proceed accordingly

          if(gridl%xa(nn) .eq. grid%xa(i)) then

             ! Define local variables

             gridl%ya(nn) = grid%ya(i)
             goto 1000

          end if ! if(gridl%xa(nn) .eq. grid%xa(i))

       end do ! do i = 1, grid%n
       
       ! Define local variables
       
1000   continue
       
    end do ! do while(min_val .lt. max_val)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(grid)

    ! Define local variables

    grid%n  = count(gridl%xa .ne. spval)
    call variable_interface_setup_struct(grid)
    grid%xa = gridl%xa(1:grid%n)
    grid%ya = gridl%ya(1:grid%n)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(gridl)

    !=====================================================================

  end subroutine unique_array_spline  

  !=======================================================================

  ! SUBROUTINE: 

  ! wvnbrdcmp2d.f90

  ! DESCRIPTION:

  ! This subroutine computes the Fast Fourier transform (FFT) of a
  ! 2-dimensional variable field and subsequently decomposes the
  ! respective variable field into it's wave number components; the
  ! total number of wave numbers is equal to (nx/2 + 1).

  ! INPUT VARIABLES:

  ! * wnd2d; a FORTRAN wnd2d_struct variable where the zonal- and
  !   meridional-dimensions (i.e., nx and ny, respectively) and the
  !   total wavenumber (nh) are defined in addition to the
  !   2-dimensional variable field to be decomposed (var).

  ! * recenter; a FORTRAN recenter_struct variable containing the
  !   radial and azimuthal (i.e., polar-projection) components of the
  !   analysis variable grid to be utilized by the FFT.

  ! * grid; a FORTRAN grid_struct variable.

  ! OUTPUT VARIABLES:

  ! * wnd2d; a FORTRAN wnd2d_struct variable containing the wavenumber
  !   decomposition (wnvar) of the variable field specified by the
  !   user (var).

  ! * grid; a FORTRAN grid_struct variable containing the grid
  !   projection information required to remap the respective
  !   wavenumbers to the original grid projection.

  !-----------------------------------------------------------------------

  subroutine wvnbrdcmp2d(wnd2d,recenter,grid)

    ! Define variables passed to routine

    type(wnd2d_struct)                                                  :: wnd2d
    type(recenter_struct)                                               :: recenter
    type(grid_struct)                                                   :: grid

    ! Define variables computed within routine

    type(fft2d_struct)                                                  :: fft2d
    type(vargrid_struct)                                                :: vargrid
    type(statgrid_struct)                                               :: statgrid
    complex(r_double),          dimension(:,:),             allocatable :: fftsave
    complex(r_double),          dimension(:,:),             allocatable :: fftvar
    real(r_kind),               dimension(:),               allocatable :: var
    real(r_kind)                                                        :: radius_min
    real(r_kind)                                                        :: radius_max
    real(r_kind)                                                        :: angle_min
    real(r_kind)                                                        :: angle_max
    integer                                                             :: ncoord

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    fft2d%nx      = wnd2d%nr
    fft2d%ny      = wnd2d%na
    call variable_interface_setup_struct(fft2d)
    vargrid%nx    = wnd2d%nx
    vargrid%ny    = wnd2d%ny
    vargrid%nvals = (vargrid%nx*vargrid%ny)
    call variable_interface_setup_struct(vargrid) 
    grid%nx       = fft2d%nx
    grid%ny       = fft2d%ny
    grid%ncoords  = (grid%nx*grid%ny)
    call variable_interface_setup_struct(grid)

    ! Allocate memory for local variables

    if(.not. allocated(fftsave)) allocate(fftsave(fft2d%nx,fft2d%ny))
    if(.not. allocated(fftvar))  allocate(fftvar(fft2d%nx,fft2d%ny))
    if(.not. allocated(var))     allocate(var(wnd2d%nx*wnd2d%ny))

    ! Define local variables

    ncoord = 0

    ! Loop through local variable
    
    do j = 1, fft2d%ny

       ! Loop through local variable
       
       do i = 1, fft2d%nx

          ! Define local variables

          ncoord      = ncoord + 1
          vargrid%var = spval
          var         = spval
          radius_min  = 0.0 + (i-1)*wnd2d%dradius
          radius_max  = radius_min + wnd2d%dradius
          angle_min   = 0.0 + (j-1)*wnd2d%dangle
          angle_max   = angle_min + wnd2d%dangle
          grid%gcdist = radius_max
          grid%gchead = 0.5*(angle_max + angle_min) + 90.0
          grid%gclon  = recenter%clon
          grid%gclat  = recenter%clat
          where(recenter%radius .ge. radius_min .and.                     &
               & recenter%radius .lt. radius_max .and.                    &
               & recenter%angle .ge. angle_min .and.                      &
               & recenter%angle .lt. angle_max) var = wnd2d%var
          vargrid%var = var

          ! Compute local variables

          call grid_methods_gcgeo(grid)
          call math_methods_stats(vargrid,statgrid)
          if(statgrid%mean .eq. spval) statgrid%mean = 0.0

          ! Define local variables

          grid%lon(ncoord) = grid%grlon
          grid%lat(ncoord) = grid%grlat
          fftvar(i,j)      = cmplx(dble(statgrid%mean),dble(0.0),         &
               & kind=r_double)

       end do ! do i = 1, fft2d%nx

    end do ! do j = 1, fft2d%ny

    ! Define local variables

    fft2d%in = fftvar

    ! Compute local variables

    call math_methods_fftw_dft(fft2d,.true.,.false.)

    ! Define local variables

    fftsave = fft2d%out

    ! Loop through local variable

    do i = 1, wnd2d%nh

       ! Define local variables

       fft2d%out      = cmplx(0.0,0.0,kind=r_double)
       fft2d%in       = cmplx(0.0,0.0,kind=r_double)
       fft2d%out(:,i) = fftsave(:,i)

       ! Compute local variables

       call math_methods_fftw_dft(fft2d,.false.,.true.)

       ! Define local variables

       wnd2d%wnvar(i,:,:) = real(fft2d%in)

    end do ! do i = 1, wnd2d%nh

    ! Deallocate memory for local variables

    if(allocated(fftsave)) deallocate(fftsave)
    if(allocated(fftvar))  deallocate(fftvar)
    if(allocated(var))     deallocate(var)
    call variable_interface_cleanup_struct(fft2d)
    call variable_interface_cleanup_struct(vargrid)

    !=====================================================================

  end subroutine wvnbrdcmp2d

  !=======================================================================

end module math_methods_interface
