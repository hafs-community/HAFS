module math_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! obs-preproc :: math_methods_interface
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
  use kdtree2_module
  use kinds_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: math_methods_kdtree_nn
  public :: math_methods_kdtree_r2
  public :: math_methods_llp_interp
  public :: math_methods_normalize_values
  public :: math_methods_remove_duplicates
  public :: math_methods_sort_array
  public :: math_methods_spline_interp
  public :: math_methods_stats

  !-----------------------------------------------------------------------

contains

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

  ! math_methods_kdtree_nn.f90

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

  subroutine math_methods_kdtree_nn(src_grid,dst_grid,kdtree)

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

    kdtree2 => kdtree2_create(src_grdloc,sort=.false.,rearrange=.false.)

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

  end subroutine math_methods_kdtree_nn  

  !=======================================================================

  ! SUBROUTINE:

  ! math_methods_kdtree_r2.f90

  ! DESCRIPTION:

  ! This subroutine finds the N nearest-neighbors within a user
  ! specified radius of a given location.

  ! REFERENCES:

  ! Kennel, M. B., 2004: KDTREE2: Fortran 95 and C++ software to
  ! efficiently search for near neighbors in a multi-dimensional
  ! Euclidean space. 

  ! http://arxiv.org/PScache/phvsics/pdf/0408/0408067.pdf.

  ! INPUT VARIABLES:
  
  ! * dst_grid; a FORTRAN grid_struct variable containing the
  !   Euclidean space geographical locations within which to find
  !   nearest-neighbors; geographical locations (e.g., latitudes and
  !   longitudes) are assumed to have units of degrees.

  ! * src_grid; a FORTRAN grid_struct variable containing the
  !   geographical locations for which to find the N-nearest neighbor
  !   locations; geographical locations (e.g., latitudes and
  !   longitudes) are assumed to have units of degrees.

  ! * kdtree; a FORTRAN kdtree_struct variable containing (at minimum)
  !   the number of coordinate values (the ncoords attribute should be
  !   equal to the dst_grid variable ncoords attribute) and the radial
  !   distance within which to seek the nearest neighbors.

  ! OUTPUT VARIABLES:

  ! * kdtree; a FORTRAN kdtree_struct variable containing the
  !   N-nearest neighbors within the R^2 distance specified by the
  !   user.

  !-----------------------------------------------------------------------

  subroutine math_methods_kdtree_r2(src_grid,dst_grid,kdtree)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(kdtree_struct)                                                 :: kdtree

    ! Define variables computed within routine

    type(kdtree2), pointer                                              :: kdtree2
    type(kdtree2_result)                                                :: sresults(kdtree%nalloc)
    real(r_kind),               dimension(:,:),             allocatable :: src_grdloc
    real(r_kind)                                                        :: dst_grdloc(3)

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(src_grdloc))                                        &
         & allocate(src_grdloc(3,dst_grid%ncoords))

    ! Loop through local variables

    do i = 1, dst_grid%ncoords

       ! Compute local variables

       src_grdloc(1,i) = rearth_equator*cos(dst_grid%lat(i)*deg2rad)*      &
            & cos(dst_grid%lon(i)*deg2rad)
       src_grdloc(2,i) = rearth_equator*cos(dst_grid%lat(i)*deg2rad)*      &
            & sin(dst_grid%lon(i)*deg2rad)
       src_grdloc(3,i) = rearth_equator*sin(dst_grid%lat(i)*deg2rad)

    end do ! do i = 1, dst_grid%ncoords

    ! Compute local variables

    kdtree2 => kdtree2_create(src_grdloc,sort=.true.,rearrange=.true.)
    dst_grdloc(1) = rearth_equator*cos(src_grid%gclat*deg2rad)*            &
         & cos(src_grid%gclon*deg2rad)
    dst_grdloc(2) = rearth_equator*cos(src_grid%gclat*deg2rad)*            &
         & sin(src_grid%gclon*deg2rad)
    dst_grdloc(3) = rearth_equator*sin(src_grid%gclat*deg2rad)
       
    ! Define local variables

    call kdtree2_r_nearest(tp=kdtree2,qv=dst_grdloc,r2=kdtree%r2,nfound=   &
         & kdtree%nfound,nalloc=kdtree%nalloc,results=sresults)
    kdtree%r2dist(1,1:kdtree%nfound) = sresults(1:kdtree%nfound)%dis
    kdtree%idx(1,1:kdtree%nfound)    = sresults(1:kdtree%nfound)%idx

    ! Deallocate memory for local variables

    call kdtree2_destroy(kdtree2)
    if(allocated(src_grdloc)) deallocate(src_grdloc)

    !=====================================================================

  end subroutine math_methods_kdtree_r2  

  !=======================================================================

  ! SUBROUTINE: 

  ! math_methods_llp_interp.f90

  ! DESCRIPTION:

  ! This subroutine interpolates a vertical profile from a source
  ! profile to a destination profile using linear-log pressure
  ! interpolation.

  ! INPUT VARIABLES:

  ! * interp_p; a FORTRAN interp_p_struct variable containing the
  !   source pressure and variable profiles as well as the surface
  !   pressure.

  ! * dstp; a FORTRAN 4-byte real value specifying the pressure level
  !   for which to interpolate the respective source profile variable.

  ! OUTPUT VARIABLES:

  ! * dstvar; a FORTRAN 4-byte real value specifying the interpolated
  !   profile variable value.

  !-----------------------------------------------------------------------

  subroutine math_methods_llp_interp(interp_p,dstp,dstvar)

    ! Define variables passed to routine

    type(interp_p_struct)                                               :: interp_p
    real(r_kind)                                                        :: dstp
    real(r_kind)                                                        :: dstvar

    ! Define variables computed within routine

    integer                                                             :: idx_b
    integer                                                             :: idx_t

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables
    
    dstvar = spval
    idx_b  = 0
    idx_t  = 0

    ! Loop through local variable

    do i = 2, interp_p%nz

       ! Check local variable and proceed accordingly

       if(dstp .le. interp_p%p(i-1) .and. dstp .gt. interp_p%p(i)) then

          ! Define local variables

          idx_b = i - 1
          idx_t = i
          goto 1000

       end if ! if(dstp .le. interp_p%p(i-1) .and. dstp
              ! .gt. interp_p%p(i))

    end do ! do i = 2, interp_p%nz

    ! Define local variables

1000 continue

    ! Check local variable and proceed accordingly

    if(idx_b .eq. 0 .and. idx_t .eq. 0) then

       ! Define local variables

       idx_b = interp_p%nz
       idx_t = 1

    end if ! if(idx_b .eq. 0 .and. idx_t .eq. 0)

    ! Compute local variables

    dstvar = interp_p%var(idx_b) + (interp_p%var(idx_t) -                  &
         & interp_p%var(idx_b))*((log(dstp) - log(interp_p%p(idx_b)))/     &
         & (log(interp_p%p(idx_t)) - log(interp_p%p(idx_b))))

    !=====================================================================

  end subroutine math_methods_llp_interp

  !=======================================================================

  ! SUBROUTINE:

  ! math_methods_normalize_values.f90

  ! DESCRIPTION:

  ! This subroutine normalizes a variable array relative to the
  ! respective variables attributes.

  ! REFERENCES:

  ! https://en.wikipedia.org/wiki/Feature_scaling

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN statgrid_struct variable containing the variable
  !   array to be normalized.

  ! * norma; the minumum value about which to standardize the array.

  ! * normb; the maximum value about which to standardize the array.

  !-----------------------------------------------------------------------

  subroutine math_methods_normalize_values(grid,norma,normb)

    ! Define variables passed to routine

    type(statgrid_struct)                                               :: grid
    real(r_kind)                                                        :: norma
    real(r_kind)                                                        :: normb 

    ! Define variables computed within routine

    real(r_kind),               dimension(:),               allocatable :: var

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(var)) allocate(var(grid%n))

    ! Compute local variables

    call math_methods_stats(grid)

    ! Loop through local variable

    do i = 1, grid%n

       ! Compute local variables

       var(i) = norma + (((grid%var(i) - grid%varmin)*(normb - norma))/    &
            & (grid%varmax - grid%varmin))

    end do ! do i = 1, grid%n

    ! Define local variables

    grid%var = var

    ! Deallocate memory for local variables

    if(allocated(var)) deallocate(var)

    !=====================================================================

  end subroutine math_methods_normalize_values

  !=======================================================================

  ! SUBROUTINE:

  ! math_methods_remove_duplicates.f90

  ! DESCRIPTION:

  ! This subroutine ingests a FORTRAN interp_spline_struct variable
  ! and removes duplicate values from the independent variable array.

  ! INPUT VARIABLES:

  ! * grid_in; a FORTRAN interp_spline_struct variable containing
  !   possible duplicate values within the independent variable array,
  !   xa.

  ! * grid_out; a FORTRAN interp_spline_struct variable to contain the
  !   ingested interp_spline_struct variable grid_in, but devoid of
  !   duplicate values.

  ! OUTPUT VARIABLES:

  ! * grid_out; a FORTRAN interp_spline_struct variable, initialized
  !   as grid_in but no longer containing duplicate values within the
  !   independent variable array, xa.

  !-----------------------------------------------------------------------

  subroutine math_methods_remove_duplicates(grid_in,grid_out)

    ! Define variables passed to routine

    type(interp_spline_struct)                                          :: grid_in
    type(interp_spline_struct)                                          :: grid_out

    ! Define variables computed within routine

    logical,                    dimension(:),               allocatable :: mask
    integer                                                             :: num

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(mask)) allocate(mask(grid_in%n))

    ! Define local variables

    mask = .false.

    ! Loop through local variable

    do i = 1, grid_in%n

       ! Define local variables

       num = count(grid_in%xa(i)==grid_in%xa)

       ! Check local variable and proceed accordingly

       if(num == 1) then

          ! Define local variables

          mask(i) = .true.

       else   ! if(num == 1)

          ! Check local variable and proceed accordingly

          if(.not. any(grid_in%xa(i)==grid_in%xa .and. mask)) then

             ! Define local variables

             mask(i) = .true.

          end if ! if(.not. any(grid_in%xa(i)==grid_in%xa .and. mask))

       end if ! if(num == 1)

    end do ! do i = 1, grid_in%n

    ! Define local variables

    grid_out%n  = count(mask)
    call variable_interface_setup_struct(grid_out)
    grid_out%xa = pack(grid_in%xa,mask)
    grid_out%ya = pack(grid_in%ya,mask)

    ! Deallocate memory for local variables

    if(allocated(mask)) deallocate(mask)

    !=====================================================================

  end subroutine math_methods_remove_duplicates

  !=======================================================================

  ! SUBROUTINE:

  ! math_methods_sort_array.f90

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

  ! * grid; a FORTRAN interp_spline_struct containing the independent
  !   (xa) and dependent (ya) variable arrays to be sorted.

  ! * ascend; a FORTRAN logical variable specifying whether the arrays
  !   are to be sorted in the ascending direction.

  ! * descend; a FORTRAN logical variable specifying whether the
  !   arrays are to be sorted in the descending direction.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN interp_spline_struct containing the sorted
  !   independent (xa) and dependent (ya) variable arrays.

  !-----------------------------------------------------------------------

  subroutine math_methods_sort_array(grid,ascend,descend)

    ! Define variables passed to routine

    type(interp_spline_struct)                                          :: grid
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

  end subroutine math_methods_sort_array

  !=======================================================================

  ! SUBROUTINE:

  ! math_methods_spline_interp.f90

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

  ! * grid; a FORTRAN interp_spline_struct containing the location
  !   (xa) and variable (ya) arrays as well as the interpolation
  !   location (x).

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN interp_spline_struct containing the interpolated
  !   value (y).

  !-----------------------------------------------------------------------

  subroutine math_methods_spline_interp(grid)

    ! Define variables passed to routine

    type(interp_spline_struct)                                          :: grid

    ! Define variables computed within routine

    type(interp_spline_struct)                                          :: gridl
    real(r_double),             dimension(:),               allocatable :: xa
    real(r_double),             dimension(:),               allocatable :: ya
    real(r_double),             dimension(:),               allocatable :: y2a
    real(r_double)                                                      :: yp
    real(r_double)                                                      :: ypp
    real(r_double)                                                      :: yp1
    real(r_double)                                                      :: ypn
    real(r_double)                                                      :: x
    real(r_double)                                                      :: y
    integer                                                             :: n

    !=====================================================================

    ! Define local variables

    call math_methods_remove_duplicates(grid,gridl)
    x = dble(grid%x)
    n = gridl%n

    ! Allocate memory for local variables

    if(.not. allocated(xa))  allocate(xa(n))
    if(.not. allocated(ya))  allocate(ya(n))
    if(.not. allocated(y2a)) allocate(y2a(n))

    ! Check local variable and proceed accordingly

    if(gridl%n .ge. 2) then

       ! Define local variables

       call math_methods_sort_array(gridl,.true.,.false.)
       yp1 = dble(0.0)
       ypn = dble(0.0)
       xa  = dble(gridl%xa)
       ya  = dble(gridl%ya)

       ! Compute local variables

       call spline_cubic_set(n,xa(1:n),ya(1:n),3,yp1,3,ypn,y2a)
       call spline_cubic_val(n,xa(1:n),ya(1:n),y2a(1:n),x,y,yp,ypp)

    end if ! if(gridl%n .ge. 2)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(gridl)
    if(allocated(xa))  deallocate(xa)
    if(allocated(ya))  deallocate(ya)
    if(allocated(y2a)) deallocate(y2a)

    ! Define local variables

    grid%y = real(y)

    !=====================================================================

  end subroutine math_methods_spline_interp

  !=======================================================================

  ! SUBROUTINE: 

  ! math_methods_stats.f90

  ! DESCRIPTION:

  ! This subroutine defines/computes the attributes of a variable
  ! array and returns the respective attibutes.

  ! INPUT VARIABLES:

  ! * statgrid; a FORTRAN statgrid_struct variable.

  ! OUTPUT VARIABLES:

  ! * statgrid; a FORTRAN statgrid_struct variable.

  !-----------------------------------------------------------------------

  subroutine math_methods_stats(statgrid)

    ! Define variables passed to routine

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

    do i = 1, statgrid%n

       ! Check local variable and proceed accordingly

       if(statgrid%var(i) .ne. spval) then

          ! Define local variables

          varmin = min(varmin,statgrid%var(i))
          varmax = max(varmax,statgrid%var(i))

          ! Compute local variables

          sum   = sum + statgrid%var(i)
          count = count + 1

       end if ! if(vargrid%var(i) .ne. spval)
       
    end do ! do i = 1, statgrid%n

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

       do i = 1, statgrid%n

          ! Check local variable and proceed accordingly

          if(statgrid%var(i) .ne. spval) then

             ! Compute local variables

             sum   = sum + (statgrid%var(i) - statgrid%mean)
             sumsq = sumsq + ((statgrid%var(i) - statgrid%mean)*           &
                  & (statgrid%var(i) - statgrid%mean))

          end if ! if(statgrid%var(i) .ne. spval)

       end do ! do i = 1, statgrid%n

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

end module math_methods_interface
