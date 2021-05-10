module math_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: math_methods_interface
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

  use constants_interface
  use kdtree2_module
  use kinds_interface
  use namelist_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: math_methods_euclidean_norm
  public :: math_methods_kdtree_r2
  public :: math_methods_pca
  public :: math_methods_pca_vari_cutoff
  public :: math_methods_radialdist
  public :: math_methods_rnorm
  public :: math_methods_spline
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

  ! math_methods_euclidean_norm.f90

  ! DESCRIPTION:

  ! This subroutine computes the Euclidean vector (L2) norm from an
  ! array of fixed reference locations (dst_grdloc) to all fixed grid
  ! location (src_grdloc); the resulting L2 norm is squared such that
  ! it complies with traditional KD-tree type results.

  ! INPUT VARIABLES:

  ! * kdtree; a FORTRAN kdtree_struct variable containing the
  !   destination and source grid (dst_ and src_, respectively)
  !   geographical locations from which to compute the L2 norm.

  ! OUTPUT VARIABLES:

  ! * kdtree; a FORTRAN kdtree_struct variable now containing the
  !   squared L2 norm Euclidean distance from the specified fixed
  !   (reference) locations (dst_) to all points on the target grid
  !   (src_).

  !-----------------------------------------------------------------------

  subroutine math_methods_euclidean_norm(kdtree)

    ! Define variables passed to routine

    type(kdtree_struct)                                                 :: kdtree

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: src_grdloc
    real(r_kind)                                                        :: dst_grdloc(3)
    
    ! Define counting variables

    integer                                                             :: i, j
    
    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(src_grdloc))                                        &
         & allocate(src_grdloc(3,kdtree%src_ncoords))

    ! Loop through local variable

    do i = 1, kdtree%src_ncoords
       
       ! Compute local variables

       src_grdloc(1,i) = rearth_equator*cos(kdtree%src_lat(i)*deg2rad)*    &
            & cos(kdtree%src_lon(i)*deg2rad)
       src_grdloc(2,i) = rearth_equator*cos(kdtree%src_lat(i)*deg2rad)*    &
            & sin(kdtree%src_lon(i)*deg2rad)
       src_grdloc(3,i) = rearth_equator*sin(kdtree%src_lat(i)*deg2rad)

    end do ! do i = 1, kdtree%src_ncoords

    ! Loop through local variable

    do j = 1, kdtree%dst_ncoords

       ! Compute local variables
       
       dst_grdloc(1) = rearth_equator*cos(kdtree%dst_lat(j)*deg2rad)*      &
            & cos(kdtree%dst_lon(j)*deg2rad)
       dst_grdloc(2) = rearth_equator*cos(kdtree%dst_lat(j)*deg2rad)*      &
            & sin(kdtree%dst_lon(j)*deg2rad)
       dst_grdloc(3) = rearth_equator*sin(kdtree%dst_lat(j)*deg2rad)

       ! Loop through local variable

       do i = 1, kdtree%src_ncoords
       
          ! Define local variables

          kdtree%r2dist(i,j) = norm2((src_grdloc(:,i) - dst_grdloc))**2.0

       end do ! do i = 1, kdtree%src_ncoords
          
    end do ! do i = 1, kdtree%dst_ncoords

    ! Define local variables

    if(debug) write(6,500) minval(kdtree%r2dist(:,1:kdtree%dst_ncoords)),  &
         & maxval(kdtree%r2dist(:,1:kdtree%dst_ncoords))
    
    ! Deallocate memory for local variables

    if(allocated(src_grdloc)) deallocate(src_grdloc)

    ! Define local variables

500 format('MATH_METHODS_EUCLIDEAN_NORM: min/max r2dist = ',2e)

    !=====================================================================
    
  end subroutine math_methods_euclidean_norm

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
    dst_grdloc(1) = rearth_equator*cos(src_grid%clat*deg2rad)*             &
         & cos(src_grid%clon*deg2rad)
    dst_grdloc(2) = rearth_equator*cos(src_grid%clat*deg2rad)*             &
         & sin(src_grid%clon*deg2rad)
    dst_grdloc(3) = rearth_equator*sin(src_grid%clat*deg2rad)
       
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

  ! math_methods_pca.f90

  ! DESCRIPTION:

  ! This subroutine deconstructs and/or reconstructs a user specified
  ! variable using singular value decomposition (SVD) of a general
  ! rectangular (m-by-n) maxtrix (svd%a).

  ! NOTE:

  ! The FORTRAN svd_struct variable attributes 'dcnstrct' and
  ! 'rcnstrct' denote deconstruction via SVD or reconstruct via the
  ! component matrices, respectively.

  ! If the deconstruction of a user specified variable is compute, the
  ! output FORTRAN svd_struct variable contains the transpose of the
  ! m-by-m orthogonal matrix component matrix 'vt' within the variable
  ! attribute 'v'.

  ! INPUT VARIABLES:

  ! For deconstruction:

  ! * svd; a FORTRAN svd_struct variable containing the matrix to be
  !   decomposed as the 'a' attribute; the component matrices are
  !   returned within the 'u', 'vt', 'v', and 's' variable attributes.

  ! For reconstruction:

  ! * svd; a FORTRAN svd_struct variable containing the component
  !   matrices 'u', 'v', and 's'. 

  ! OUTPUT VARIABLES:

  ! For deconstruction:

  ! * svd; a FORTRAN svd_struct variable containing the component
  !   matrices 'u', 'v', 'vt', and 's'.

  ! For reconstruction:

  ! * svd; a FORTRAN svd_struct variable containing the reconstructed
  !   matrix as the 'a' attribute.

  !-----------------------------------------------------------------------

  subroutine math_methods_pca(svd)

    ! Define variables passed to routine

    type(svd_struct)                                                    :: svd
    
    !=====================================================================

    ! Check local variable and proceed accordingly

    if(svd%dcnstrct) call pca_deconstruct(svd)
    if(svd%rcnstrct) call pca_reconstruct(svd)

    !=====================================================================
    
  end subroutine math_methods_pca

  !=======================================================================

  ! SUBROUTINE:

  ! math_methods_pca_vari_cutoff.f90

  ! DESCRIPTION:

  ! This subroutine returns the array indice representing the first
  ! index which exceeds the variance explained cutoff threshold
  ! specified by the user.

  ! INPUT VARIABLES:

  ! * svd; a FORTRAN svd_struct variable containing the variance
  !   explained by each principle component (e.g., singular value).

  ! * cutoff; a FORTRAN 4-byte real-valued variable specifying the
  !   variance explained by the principle component summation
  !   threshold value; this is a percentage value (e.g., 0.9 => 90%).

  ! * idx; a FORTRAN integer value.

  ! * idxp1; a FORTRAN integer value.

  ! OUTPUT VARIABLES:

  ! * idx; a FORTRAN integer representing the first index which is
  !   less than or equal to the variance explained cutoff threshold
  !   specified by the user.

  ! * idxp1; a FORTRAN integer representing the index which exceeds
  !   the variance explained cutoff threshold specified by the user.

  !-----------------------------------------------------------------------

  subroutine math_methods_pca_vari_cutoff(svd,cutoff,idx,idxp1)
  
    ! Define variables passed to routine

    type(svd_struct)                                                    :: svd
    real(r_kind)                                                        :: cutoff
    integer                                                             :: idx
    integer                                                             :: idxp1

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    idx = 1

    ! Loop through local variable

    do i = 1, size(svd%s)

       ! Check local variable and proceed accordingly

       if(sum(svd%svarex(1:i)) .le. cutoff) then

          ! Define local variables

          idx = i

       end if ! ! Check local variable and proceed accordingly

    end do ! do i = 1, size(svd%s)

    ! Define local variables

    idxp1 = min((idx + 1),size(svd%s))

    !=====================================================================

  end subroutine math_methods_pca_vari_cutoff

  !=======================================================================

  ! SUBROUTINE: 

  ! math_methods_radialdist.f90

  ! DESCRIPTION:

  ! This subroutine computes the radial distance, using the
  ! Earth-bound geographical coordinates, between two locations
  ! assuming the equitorial radius of the Earth.

  ! INPUT VARIABLES:

  ! * lon1; a FORTRAN 4-byte real value specifying the geographical
  !   longitude coordinate of location 1; units are degrees.

  ! * lat1; a FORTRAN 4-byte real value specifying the geographical
  !   latitude coordinate of location 1; units are degrees.

  ! * lon2; a FORTRAN 4-byte real value specifying the geographical
  !   longitude coordinate of location 2; units are degrees.

  ! * lat2; a FORTRAN 4-byte real value specifying the geographical
  !   latitude coordinate of location 2; units are degrees.

  ! OUTPUT VARIABLES:

  ! * dist; a FORTRAN 4-byte real value specifying the radial distance
  !   between two locations assuming the equitorial radius of the
  !   Earth; units are meters.

  !-----------------------------------------------------------------------

  subroutine math_methods_radialdist(lon1,lat1,lon2,lat2,dist)

    ! Define variables passed to routine

    real(r_kind)                                                        :: lon1
    real(r_kind)                                                        :: lat1
    real(r_kind)                                                        :: lon2
    real(r_kind)                                                        :: lat2
    real(r_kind)                                                        :: dist

    ! Define variables computed within routine

    real(r_double)                                                      :: lat1_deg
    real(r_double)                                                      :: lat2_deg
    real(r_double)                                                      :: lat1_rad
    real(r_double)                                                      :: lat2_rad
    real(r_double)                                                      :: lon1_deg
    real(r_double)                                                      :: lon2_deg
    real(r_double)                                                      :: lon1_rad
    real(r_double)                                                      :: lon2_rad
    real(r_double)                                                      :: x1
    real(r_double)                                                      :: y1
    real(r_double)                                                      :: z1
    real(r_double)                                                      :: x2
    real(r_double)                                                      :: y2
    real(r_double)                                                      :: z2
    real(r_kind)                                                        :: dr

    !=====================================================================

    ! Compute local variables

    lon1_deg = lon1
    lon1_deg = mod(lon1_deg,360.d0)
    if(lon1_deg .lt. 0.d0) then
       lon1_deg = lon1_deg + 360.d0
    end if ! if(lon1_deg .lt. 0.d0)
    lon2_deg = lon2
    lon2_deg = mod(lon2_deg,360.d0)
    if(lon2_deg .lt. 0.d0) then
       lon2_deg = lon2_deg + 360.d0
    end if ! if(lon2_deg .lt. 0.d0)
    if(lat1 .lt. lat2) then
       lon1_rad = lon1_deg*deg2rad
       lat1_rad = (90.d0 - lat1)*deg2rad
       lon2_rad = lon2_deg*deg2rad
       lat2_rad = (90.d0 - lat2)*deg2rad
    else if(lat1 .eq. lat2 .and. lon1_deg .le. lon2_deg) then
       lon1_rad = lon1_deg*deg2rad
       lat1_rad = (90.d0 - lat1)*deg2rad
       lon2_rad = lon2_deg*deg2rad
       lat2_rad = (90.d0 - lat2)*deg2rad
    else
       lon1_rad = lon2_deg*deg2rad
       lat1_rad = (90.d0 - lat2)*deg2rad
       lon2_rad = lon1_deg*deg2rad
       lat2_rad = (90.d0 - lat1)*deg2rad
    end if ! if(lat1 .lt. lat2)
    x1   = sin(lat1_rad)*cos(lon1_rad)
    y1   = sin(lat1_rad)*sin(lon1_rad)
    z1   = cos(lat1_rad)
    x2   = sin(lat2_rad)*cos(lon2_rad)
    y2   = sin(lat2_rad)*sin(lon2_rad)
    z2   = cos(lat2_rad)
    dr   = acos(min(1.d0,x1*x2 + y1*y2 + z1*z2))
    dist = dr*rearth_equator

    !=====================================================================

  end subroutine math_methods_radialdist  

  !=======================================================================

  ! FUNCTION: 

  ! math_methods_rnorm.f90

  ! DESCRIPTION:

  ! This function generates a random normal deviate using the polar
  ! method.

  ! REFERENCES:

  ! Marsaglia, G. and Bray, T.A., 1964. A convenient method for
  ! generating normal variables. SIAM review, 6(3), pp.260-264.

  ! OUTPUT VARIABLES:

  ! * fnval; a random normal deviate computed via the Marsaglia et
  !   al., [1964] polar method.

  !-----------------------------------------------------------------------

  function math_methods_rnorm() result(fnval)

    ! Define variables computed within routine

    real(r_kind)                                                        :: fnval
    real(r_kind)                                                        :: u
    real(r_kind)                                                        :: v
    real(r_kind)                                                        :: sum
    real(r_kind)                                                        :: sln
    real(r_kind)                                                        :: vsmall

    !=====================================================================

    ! Define local variables

    vsmall = tiny(1.0)
    sum    = spval

    ! Loop throgh local variable

    do while(sum .ge. 1.0)

       ! Define local variables

       call random_number(u)
       call random_number(v)

       ! Compute local variables

       u   = scale(u,1) - 1.0
       v   = scale(v,1) - 1.0
       sum = u*u + v*v + vsmall

    end do ! do while(sum .ge. 1.0)

    ! Compute local variables

    sln   = sqrt(-1.0*scale(log(sum),1)/sum)
    fnval = u*sln

    !=====================================================================

  end function math_methods_rnorm

  !=======================================================================

  ! SUBROUTINE:

  ! math_methods_spline.f90

  ! DESCRIPTION:

  ! This method interpolates, using cubic-splines, to estimate the
  ! value along a profile given a value, presumably within the range
  ! of profile values.

  ! INPUT VARIABLES:

  ! * spline; a FORTRAN spline_struct variable that contains the
  !   reference profile and variable as well as the location (within
  !   the profile) to remap the variable values.

  ! OUTPUT VARIABLES:

  ! * spline; a FORTRAN spline_struct variable containing the remapped
  !   variable value; if the value is equal to the spval designiaton,
  !   this implies that the location for which to remap was outside
  !   the bounds of the available profile values (e.g., no
  !   extrapolation is performed).
  
  !-----------------------------------------------------------------------

  subroutine math_methods_spline(spline)

    ! Define variables passed to routine

    type(spline_struct)                                                 :: spline

    ! Define variables computed within routine

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

    call sort_array(spline,.true.,.false.)
    n   = count(spline%xa .ne. spval)
    x   = dble(spline%x)
    y   = spval
    yp1 = dble(0.0)
    ypn = dble(0.0)

    ! Check local variable and proceed accordingly

    if(n .ge. 2) then

       ! Allocate memory for local variables
       
       if(.not. allocated(xa))  allocate(xa(n))
       if(.not. allocated(ya))  allocate(ya(n))
       if(.not. allocated(y2a)) allocate(y2a(n))

       ! Define local variables

       j = 0

       ! Loop through local variable

       do i = 1, size(spline%xa)

          ! Check local variable and proceed accordingly

          if(spline%xa(i) .ne. spval .and. spline%ya(i) .ne. spval) then

             ! Define local variables
             
             j     = j + 1
             xa(j) = dble(spline%xa(i))
             ya(j) = dble(spline%ya(i))

          end if ! if(spline%xa(i) .ne. spval .and. spline%ya(i)
                 ! .ne. spval)

       end do ! do i = 1, size(spline%xa)
          
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

    spline%y = real(y)

    !=====================================================================

  end subroutine math_methods_spline
  
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

  ! pca_deconstruct.f90

  ! DESCRIPTION:

  ! This subroutine computes the principle component and singular
  ! vector (U, S, and VT) matrices via descontruction of a matrix A.

  ! INPUT VARIABLES:

  ! * svd; a FORTRAN svd_struct variable containing the matrix to be
  !   decomposed as the 'a' attribute.

  ! OUTPUT VARIABLES:

  ! * svd; a FORTRAN svd_struct variable containing the component
  !   matrices 'u', 'vt', and 's'.

  !-----------------------------------------------------------------------

  subroutine pca_deconstruct(svd)

    ! Define variables passed to routine

    type(svd_struct)                                                    :: svd

    ! Define variables computed within routine

    type(statgrid_struct)                                               :: statgrid
    type(vargrid_struct)                                                :: vargrid
    character(len=1)                                                    :: jobu
    character(len=1)                                                    :: jobvt
    real(dp_intel),             dimension(:),               allocatable :: work
    integer                                                             :: lwmax
    integer                                                             :: lwork
    integer                                                             :: info

    !=====================================================================

    ! Define local variables

    vargrid%nx  = svd%nx
    vargrid%ny  = svd%ny
    call variable_interface_setup_struct(vargrid)
    vargrid%var = reshape(real(svd%a),(/size(vargrid%var)/))
       
    ! Compute local variables

    call math_methods_stats(vargrid,statgrid)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(vargrid)

    ! Define local variables

    svd%mean = dble(statgrid%mean)
    jobu     = 'A'
    jobvt    = 'A'
    lwmax    = max(3*min(svd%nx,svd%ny)+max(svd%nx,svd%ny),                &
         & 5*min(svd%nx,svd%ny)-4)
    lwork    = -1
       
    ! Allocate memory for local variables
    
    if(.not. allocated(work)) allocate(work(lwmax))
    
    ! Compute local variables
    
    call dgesvd(jobu,jobvt,svd%nx,svd%ny,svd%a,svd%lda,svd%s,svd%u,        &
         & svd%ldu,svd%vt,svd%ldvt,work,lwork,info)
    
    ! Define local variables
    
    lwork = int(work(1))
    svd%a = svd%a - svd%mean

    ! Deallocate memory for local variables
    
    if(allocated(work)) deallocate(work)

    ! Allocate memory for local variables

    if(.not. allocated(work)) allocate(work(lwork))

    ! Compute local variables
    
    call dgesvd(jobu,jobvt,svd%nx,svd%ny,svd%a,svd%lda,svd%s,svd%u,        &
         & svd%ldu,svd%vt,svd%ldvt,work,lwork,info)

    ! Define local variables
    
    svd%v = transpose(svd%vt)
    
    ! Deallocate memory for local variables
    
    if(allocated(work)) deallocate(work)

    ! Compute local variables

    call pca_variexpl(svd)

    !=====================================================================

  end subroutine pca_deconstruct

  !=======================================================================

  ! SUBROUTINE:

  ! pca_reconstruct.f90

  ! DESCRIPTION:

  ! This subroutine reconstructs a matrix A from the principle
  ! component and singular vector (U, S, and VT) matrices.

  ! INPUT VARIABLES:

  ! * svd; a FORTRAN svd_struct variable containing the component
  !   matrices defined within the 'u', 'vt', and 's' attributes.

  ! OUTPUT VARIABLES:

  ! * svd; a FORTRAN svd_struct variable containing the reconstructed
  !   matrix as the 'a' attribute.

  !-----------------------------------------------------------------------

  subroutine pca_reconstruct(svd)

    ! Define variables passed to routine

    type(svd_struct)                                                    :: svd

    ! Define variables computed within routine

    character(len=1)                                                    :: transa
    character(len=1)                                                    :: transb
    real(dp_intel),             dimension(:,:),             allocatable :: sxvt
    real(dp_intel),             dimension(:,:),             allocatable :: uxsvt
    real(dp_intel),             dimension(:,:),             allocatable :: sdiag
    real(dp_intel)                                                      :: alpha
    real(dp_intel)                                                      :: beta
    integer                                                             :: argm
    integer                                                             :: argn
    integer                                                             :: argk

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(sxvt))  allocate(sxvt(svd%nx,size(svd%s)))
    if(.not. allocated(sdiag)) allocate(sdiag(size(svd%s),size(svd%s)))

    ! Define local variables

    sdiag = dble(0.0)

    ! Loop through local variable
    
    do i = 1, size(svd%s)

       ! Define local variables
       
       sdiag(i,i) = svd%s(i)
       
    end do ! do i = 1, size(svd%s)

    ! Define local variables
    
    transa = 'N'
    transb = 'N'
    alpha  = dble(1.0) 
    beta   = dble(0.0) 
    argm   = size(sdiag(:,1))
    argn   = svd%ny
    argk   = size(sdiag(1,:))
    sxvt   = dble(0.0)
    
    ! Compute local variables

    call dgemm(transa,transb,argm,argn,argk,alpha,sdiag,size(sdiag(:,1)),  &
         & svd%vt,svd%ldvt,beta,sxvt,argk)

    ! Deallocate memory for local variables

    if(allocated(sdiag)) deallocate(sdiag)

    ! Allocate memory for local variables

    if(.not. allocated(uxsvt)) allocate(uxsvt(svd%nx,svd%ny))
    
    ! Define local variables
    
    transa = 'N'
    transb = 'N'
    alpha  = dble(1.0)
    beta   = dble(0.0)
    argm   = size(svd%u(:,1))
    argn   = size(sxvt(1,:))
    argk   = size(svd%u(1,:))
    uxsvt  = dble(0.0)

    ! Compute local variables

    call dgemm(transa,transb,argm,argn,argk,alpha,svd%u,size(svd%u(:,1)),  &
         & sxvt,size(sxvt(:,1)),beta,uxsvt,argk)   

    ! Define local variables
    
    svd%a = uxsvt + svd%mean
    
    ! Deallocate memory for local variables

    if(allocated(uxsvt)) deallocate(uxsvt)
    if(allocated(sxvt))  deallocate(sxvt)

    !=====================================================================

  end subroutine pca_reconstruct

  !=======================================================================

  ! SUBROUTINE:

  ! pca_variexpl.f90

  ! DESCRIPTION:

  ! This subroutine computes the variance explained by each singular
  ! value (e.g., principle component).

  ! INPUT VARIABLES:

  ! * svd; a FORTRAN svd_struct variable containing the singular
  !   values (e.g., principle components) within the 's' attribute.

  ! OUTPUT VARIABLES:

  ! * svd; a FORTRAN svd_struct variable containing the variance
  !   explained by each of the principle components within the
  !   'svarex' attribute.

  !-----------------------------------------------------------------------

  subroutine pca_variexpl(svd)

    ! Define variables passed to routine

    type(svd_struct)                                                    :: svd

    ! Define variables computed within routine

    real(r_double)                                                      :: sumsq

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    sumsq = dble(0.0)

    ! Loop through local variable

    do i = 1, size(svd%s)

       ! Compute local variables

       sumsq = sumsq + svd%s(i)*svd%s(i)

    end do ! do i = 1, size(svd%s)

    ! Loop through local variable

    do i = 1, size(svd%s)
    
       ! Compute local variables

       svd%svarex(i) = (svd%s(i)*svd%s(i))/sumsq

    end do ! do i = 1, size(svd%s)

    !=====================================================================

  end subroutine pca_variexpl

  !=======================================================================

  ! SUBROUTINE:

  ! sort_array.f90

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

  ! * spline; a FORTRAN spline_struct containing the independent (xa)
  !   and dependent (ya) variable arrays to be sorted.

  ! * ascend; a FORTRAN logical variable specifying whether the arrays
  !   are to be sorted in the ascending direction.

  ! * descend; a FORTRAN logical variable specifying whether the
  !   arrays are to be sorted in the descending direction.

  ! OUTPUT VARIABLES:

  ! * spline; a FORTRAN spline_struct containing the sorted
  !   independent (xa) and dependent (ya) variable arrays.

  !-----------------------------------------------------------------------

  subroutine sort_array(spline,ascend,descend)

    ! Define variables passed to routine

    type(spline_struct)                                                 :: spline
    logical                                                             :: ascend
    logical                                                             :: descend

    ! Define variables computed within routine

    real(r_kind),               dimension(:),               allocatable :: xa
    real(r_kind),               dimension(:),               allocatable :: ya
    integer                                                             :: kflag
    integer                                                             :: n

    !=====================================================================

    ! Define local variables

    call unique_array_spline(spline)
    
    ! Check local variable and proceed accordingly

    if(ascend)  kflag = 2
    if(descend) kflag = -2

    ! Define local variables

    n = spline%n

    ! Check local variable and proceed accordingly

    if(n .ge. 2) then

       ! Allocate memory for local variables
       
       if(.not. allocated(xa)) allocate(xa(n))
       if(.not. allocated(ya)) allocate(ya(n))

       ! Define local variables

       xa        = spline%xa
       ya        = spline%ya
       call ssort(xa,ya,n,kflag)
       spline%xa = xa
       spline%ya = ya

       ! Deallocate memory for local variables
       
       if(allocated(xa)) deallocate(xa)
       if(allocated(ya)) deallocate(ya)

    end if ! if(n .ge. 2)

    !=====================================================================

  end subroutine sort_array

  !=======================================================================

  ! SUBROUTINE:

  ! unique_array_spline.f90

  ! DESCRIPTION:

  ! This subroutine returns an array containing only unique values;
  ! this method is utilitized by spline routines which require
  ! absolute ascending/descending arrays/lists in order to perform
  ! their respective algorithms.

  ! INPUT VARIABLES:

  ! * spline; a FORTRAN spline_struct variable containing the values
  !   defined by the user for spline calculations;

  ! OUTPUT VARIABLES:

  ! * spline; a FORTRAN spline_struct variable which is a modified
  !   version of the input FORTRAN spline_struct variable assuming the
  !   FORTRAN spline_struct variable does not contain entirely unique
  !   values.

  !-----------------------------------------------------------------------

  subroutine unique_array_spline(spline)

    ! Define variables passed to routine

    type(spline_struct)                                                 :: spline

    ! Define variables computed within routine

    type(spline_struct)                                                 :: splinel
    real(r_kind)                                                        :: max_val
    real(r_kind)                                                        :: min_val
    integer                                                             :: nn

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    splinel%n  = spline%n
    call variable_interface_setup_struct(splinel)
    splinel%xa = spval
    splinel%ya = spval
    max_val    = maxval(spline%xa)
    min_val    = minval(spline%xa)
    nn         = 0

    ! Loop through local variable

    do while(min_val .lt. max_val)

       ! Define local variables

       nn             = nn + 1
       min_val        = minval(spline%xa,mask=(spline%xa .gt. min_val))
       splinel%xa(nn) = min_val

       ! Loop through local variable

       do i = 1, spline%n

          ! Check local variable and proceed accordingly

          if(splinel%xa(nn) .eq. spline%xa(i)) then

             ! Define local variables

             splinel%ya(nn) = spline%ya(i)
             goto 1000

          end if ! if(splinel%xa(nn) .eq. spline%xa(i))

       end do ! do i = 1, spline%n
       
       ! Define local variables
       
1000   continue
       
    end do ! do while(min_val .lt. max_val)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(spline)

    ! Define local variables

    spline%n  = count(splinel%xa .ne. spval)
    call variable_interface_setup_struct(spline)
    spline%xa = splinel%xa(1:spline%n)
    spline%ya = splinel%ya(1:spline%n)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(splinel)

    !=====================================================================

  end subroutine unique_array_spline
  
  !=======================================================================
  
end module math_methods_interface
