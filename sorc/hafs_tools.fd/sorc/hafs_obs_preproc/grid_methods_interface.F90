module grid_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! obs-preproc :: grid_methods_interface
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
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: grid_methods_anglegrid
  public :: grid_methods_gcgeo
  public :: grid_methods_polarcoords
  public :: grid_methods_radialgrid
  public :: grid_methods_rotang

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE: 

  ! grid_methods_anglegrid.f90

  ! DESCRIPTION:

  ! This subroutine computes a grid of angle values relative to a
  ! reference Earth-bound geographical location and a grid of
  ! longitude and latitude values assuming the equitorial radius of
  ! the Earth.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN tcgrid_struct variable containing grid longitude
  !   and latitude values; units for longitude and latitude are
  !   degrees.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN tcgrid_struct variable containing the computed
  !   angle relative to a reference Earth-bound geographical location
  !   assuming the equitorial radius of the Earth; units are degrees.

  !-----------------------------------------------------------------------

  subroutine grid_methods_anglegrid(grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid

    ! Define variables computed within routine

    real(r_kind)                                                        :: ecircum
    real(r_kind)                                                        :: dlon
    real(r_kind)                                                        :: dlat
    real(r_kind)                                                        :: hyp_dist
    real(r_kind)                                                        :: opp_dist
    real(r_kind)                                                        :: adj_dist
    real(r_kind)                                                        :: sin_value
    real(r_kind)                                                        :: cos_value
    real(r_kind)                                                        :: sin_angle
    real(r_kind)                                                        :: cos_angle
    real(r_kind)                                                        :: tmp_angle    

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Compute local variables

    ecircum = 2.0*pi*(rearth_equator/1000.0)

    ! Define local variables

    grid%angle = 0.0

    ! Loop through local variable

    do i = 1, grid%ncoords

       ! Compute local variables

       dlon = abs(grid%gclon - grid%lon(i))
       dlat = abs(grid%gclat - grid%lat(i))
       call radialdist(grid%gclon,grid%gclat,grid%lon(i),grid%lat(i),      &
            & hyp_dist)

       ! Check local variable and proceed accordingly
          
       if(dlon .eq. 0.0 .and. dlat .gt. 0.0) then
          
          ! Define local variables

          if(grid%gclat .gt. grid%lat(i)) grid%angle(i) = 180.0
          if(grid%gclat .lt. grid%lat(i)) grid%angle(i) = 0.0

       else if(dlon .gt. 0.0 .and. dlat .eq. 0.0) then
                
          ! Define local variables

          if(grid%gclon .gt. grid%lon(i)) grid%angle(i) = 270.0
          if(grid%gclon .lt. grid%lon(i)) grid%angle(i) = 90.0
                
       else   ! if(dlon .eq. 0.0 .and. dlat .gt. 0.0)

          ! Compute local variables

          opp_dist  = dlat/360.0*ecircum
          sin_value = opp_dist/(hyp_dist/1000.0)      
          call radialdist(grid%gclon,grid%gclat,grid%lon(i),grid%gclat,    &
               & adj_dist)
          cos_value = (adj_dist/1000.0)/(hyp_dist/1000.0)

          ! Define local variables

          if(sin_value .gt. 1.0) sin_value = 0.99999
          if(cos_value .gt. 1.0) cos_value = 0.99999

          ! Compute local variables

          sin_angle = asin(sin_value)/deg2rad
          cos_angle = acos(cos_value)/deg2rad
          tmp_angle = 0.5*(sin_angle + cos_angle)

          ! Check local variable and proceed accordingly

          if(grid%gclat .le. grid%lat(i) .and. grid%gclon .le.             &
               & grid%lon(i)) then

             ! Compute local variables

             grid%angle(i) = 90.0 - tmp_angle
                   
          else if(grid%gclat .gt. grid%lat(i) .and. grid%gclon .le.        &
               & grid%lon(i)) then

             ! Compute local variables

             grid%angle(i) = 90.0 + tmp_angle

          else if(grid%gclat .ge. grid%lat(i) .and. grid%gclon .ge.        &
               & grid%lon(i)) then
             
             ! Compute local variables

             grid%angle(i) = 270.0 - tmp_angle
             
          else if(grid%gclat .lt. grid%lat(i) .and. grid%gclon .ge.        &
               & grid%lon(i)) then

             ! Compute local variables

             grid%angle(i) = 270.0 + tmp_angle

          end if ! if(grid%gclat .le. grid%lat(i) .and. grid%gclon
                 ! .le. grid%lon(i))

       end if ! if(dlon .eq. 0.0 .and. dlat .gt. 0.0)

    end do ! do i = 1, grid%ncoords

    !=====================================================================
    
  end subroutine grid_methods_anglegrid
  
  !=======================================================================

  ! SUBROUTINE:

  ! grid_methods_gcgeo.f90

  ! DESCRIPTION:

  ! This subroutine computes the longitude and latitude (e.g.,
  ! geographical) coordinates, assuming a great-circle distance, of an
  ! advected position provided an initial geographical position
  ! (defined by the longitude and latitude coordinates) and a distance
  ! and heading.

  ! REFERENCES:

  ! http://www.edwilliams.org/avform.htm

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the initial
  !   geographical position (gclon and gclat) and the heading (gchead;
  !   degrees) and distance (gcdist; meters); all geographical
  !   position units are assumed to be degrees.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the newly
  !   computed geographical position (gclon and gclat).

  !-----------------------------------------------------------------------

  subroutine grid_methods_gcgeo(grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid

    ! Define variables computed within routine

    real(r_kind)                                                        :: lon
    real(r_kind)                                                        :: lat
    real(r_kind)                                                        :: dlon
    real(r_kind)                                                        :: head
    real(r_kind)                                                        :: dist

    !=====================================================================

    ! Define local variables

    lat  = grid%gclat*deg2rad
    lon  = grid%gclon*deg2rad
    head = grid%gchead*deg2rad
    dist = (grid%gcdist/rearth_equator)

    ! Compute local variables

    lat  = asin(sin(lat)*cos(dist) + cos(lat)*sin(dist)*cos(head))
    dlon = atan2(sin(head)*sin(dist)*cos(lat),cos(dist) - sin(lat)*        &
         & sin(lat))
    lon  = mod(lon - dlon + pi, 2.0*pi) - pi

    ! Define local variables

    grid%gclon = lon*rad2deg
    grid%gclat = lat*rad2deg

    !=====================================================================

  end subroutine grid_methods_gcgeo

  !=======================================================================

  ! SUBROUTINE:

  ! grid_methods_polarcoords.f90

  ! DESCRIPTION:

  ! This subroutine will compute the polar coordinate values of radial
  ! distance and azimuthal angle relative to a reference geographical
  ! location.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing grid longitude
  !   and latitude values and the reference longitude and latitude
  !   coordinates (gclon and gclat, respectively); units for longitude
  !   and latitude are degrees.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the computed
  !   radial distance relative to a reference Earth-bound geographical
  !   location assuming the equitorial radius of the Earth (units are
  !   meters) and the angle relative to a reference Earth-bound
  !   geographical location assuming the equitorial radius of the
  !   Earth (units are degrees).

  !-----------------------------------------------------------------------

  subroutine grid_methods_polarcoords(grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid

    !=====================================================================

    ! Compute local variables

    call grid_methods_radialgrid(grid)
    call grid_methods_anglegrid(grid)
    call grid_methods_rotang(grid)
    
    !=====================================================================

  end subroutine grid_methods_polarcoords  

  !=======================================================================

  ! SUBROUTINE: 

  ! grid_methods_radialgrid.f90

  ! DESCRIPTION:

  ! This subroutine computes a grid of radial distance values relative
  ! to a reference Earth-bound geographical location and a grid of
  ! longitude and latitude values assuming the equitorial radius of
  ! the Earth.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the grids of
  !   longitude and latitude values, as well as the reference
  !   geographical locations (gclon and gclat); units for all
  !   longitude and latitude values are degrees.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the computed
  !   radial distance relative to a reference Earth-bound geographical
  !   location assuming the equitorial radius of the Earth; units are
  !   meters.

  !-----------------------------------------------------------------------

  subroutine grid_methods_radialgrid(grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Loop through local variable

    do i = 1, grid%ncoords

       ! Compute local variables

       call radialdist(grid%gclon,grid%gclat,grid%lon(i),grid%lat(i),      &
            & grid%radius(i))

    end do ! i = 1, grid%ncoords

    !=====================================================================
    
  end subroutine grid_methods_radialgrid

  !=======================================================================

  ! SUBROUTINE:

  ! grid_methods_rotang.f90

  ! DESCRIPTION:

  !

  ! INPUT VARIABLES:

  !

  ! OUTPUT VARIABLES:

  !

  !-----------------------------------------------------------------------

  subroutine grid_methods_rotang(grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: alpha
    real(r_kind),               dimension(:,:),             allocatable :: lat
    real(r_kind),               dimension(:,:),             allocatable :: lon
    real(r_kind)                                                        :: dlon

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(alpha)) allocate(alpha(grid%nx,grid%ny))    
    if(.not. allocated(lat))   allocate(lat(grid%nx,grid%ny))
    if(.not. allocated(lon))   allocate(lon(grid%nx,grid%ny))

    ! Define local variables

    lat = reshape(grid%lat,shape(lat))
    lon = reshape(grid%lon,shape(lon))

    ! Loop through local variable

    do i = 1, grid%nx

       ! Loop through local variable

       do j = 2, (grid%ny - 1)

          ! Compute local variables

          dlon = lon(i,j+1) - lon(i,j-1)

          ! Check local variable and proceed accordingly

          if(dlon .gt. 180.0) then

             ! Define local variables

             dlon = dlon - 360.0

          else if(dlon .lt. -180.0) then
                
             ! Define local variables

             dlon = dlon + 360.0

          end if ! if(dlon .gt. 180.0)

          ! Compute local variables

          alpha(i,j) = atan2(-cos(lat(i,j)*deg2rad)*(dlon*deg2rad),        &
               & ((lat(i,j+1) - lat(i,j-1))*deg2rad))

       end do ! do j = 2, (grid%ny - 1)

    end do ! do i = 1, grid%nx

    ! Loop through local variable

    do i = 1, grid%nx

       ! Define local variables

       dlon = lon(i,2) - lon(i,1)

       ! Check local variable and proceed accordingly
       
       if(dlon .gt. 180.0) then
          
          ! Define local variables
          
          dlon = dlon - 360.0
          
       else if(dlon .lt. -180.0) then
          
          ! Define local variables
          
          dlon = dlon + 360.0
          
       end if ! if(dlon .gt. 180.0)

       ! Compute local variables

       alpha(i,1) = atan2(-cos(lat(i,1)*deg2rad)*(dlon*deg2rad),           &
            & ((lat(i,2) - lat(i,1))*deg2rad))

       ! Define local variables

       dlon = lon(i,grid%ny) - lon(i,(grid%ny - 1))

       ! Check local variable and proceed accordingly
       
       if(dlon .gt. 180.0) then
          
          ! Define local variables
          
          dlon = dlon - 360.0
          
       else if(dlon .lt. -180.0) then
          
          ! Define local variables
          
          dlon = dlon + 360.0
          
       end if ! if(dlon .gt. 180.0)

       ! Compute local variables

       alpha(i,grid%ny) = atan2(-cos(lat(i,grid%ny)*deg2rad)*              &
            & (dlon*deg2rad),((lat(i,grid%ny) -                            &
            & lat(i,(grid%ny - 1)))*deg2rad))

    end do ! do i = 1, grid%nx

    ! Define local variables

    grid%rotang = reshape(alpha,shape(grid%rotang))*rad2deg

    ! Deallocate memory for local variables

    if(allocated(alpha)) deallocate(alpha)
    if(allocated(lat))   deallocate(lat)
    if(allocated(lon))   deallocate(lon)

    !=====================================================================

  end subroutine grid_methods_rotang

  !=======================================================================

  ! SUBROUTINE: 

  ! radialdist.f90

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

  subroutine radialdist(lon1,lat1,lon2,lat2,dist)

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

  end subroutine radialdist
  
  !=======================================================================

end module grid_methods_interface
