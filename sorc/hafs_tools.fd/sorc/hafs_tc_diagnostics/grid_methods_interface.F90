module grid_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: grid_methods_interface
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
  use kinds_interface
  use namelist_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: grid_methods_anglegrid
  public :: grid_methods_gcdist
  public :: grid_methods_gcgeo
  public :: grid_methods_geodist
  public :: grid_methods_grdbld
  public :: grid_methods_mapfactor
  public :: grid_methods_polarcoords
  public :: grid_methods_radialdist
  public :: grid_methods_radialgrid

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

  ! * grid; a FORTRAN grid_struct variable containing grid longitude
  !   and latitude values; units for longitude and latitude are
  !   degrees.

  ! * reflat; a FORTRAN 4-byte real value specifying the geographical
  !   latitude coordinate of the reference location; units are
  !   degrees.

  ! * reflon; a FORTRAN 4-byte real value specifying the geographical
  !   longitude coordinate of the reference location; units are
  !   degrees.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the computed
  !   angle relative to a reference Earth-bound geographical location
  !   assuming the equitorial radius of the Earth; units are degrees.

  !-----------------------------------------------------------------------

  subroutine grid_methods_anglegrid(grid,reflon,reflat)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid
    real(r_kind)                                                        :: reflon
    real(r_kind)                                                        :: reflat

    ! Define variables computed within routine

    logical                                                             :: check_nan
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

    do i = 1, (grid%nx*grid%ny)

       ! Compute local variables

       dlon = abs(reflon - grid%lon(i))
       dlat = abs(reflat - grid%lat(i))
       call grid_methods_radialdist(reflon,reflat,grid%lon(i),              &
            & grid%lat(i),hyp_dist)

       ! Check local variable and proceed accordingly

       check_nan = variable_interface_check_nan(hyp_dist)

       ! Check local variable and proceed accordingly

       if(dlon .eq. 0.0 .and. dlat .gt. 0.0) then

          ! Define local variables

          if(reflat .gt. grid%lat(i)) grid%angle(i) = 180.0
          if(reflat .lt. grid%lat(i)) grid%angle(i) = 0.0

       else if(dlon .gt. 0.0 .and. dlat .eq. 0.0) then
                
          ! Define local variables

          if(reflon .gt. grid%lon(i)) grid%angle(i) = 270.0
          if(reflon .lt. grid%lon(i)) grid%angle(i) = 90.0
                
       else   ! if(dlon .eq. 0.0 .and. dlat .gt. 0.0)

          ! Check local variable and proceed accordingly

          if((.not. check_nan) .and. (hyp_dist .ne. 0.0)) then

             ! Compute local variables

             opp_dist  = dlat/360.0*ecircum
             sin_value = opp_dist/(hyp_dist/1000.0)
             call grid_methods_radialdist(reflon,reflat,grid%lon(i),       &
                  & reflat,adj_dist)
             cos_value = (adj_dist/1000.0)/(hyp_dist/1000.0)

          else if((check_nan) .or. (hyp_dist .eq. 0.0)) then

             ! Define local variables

             sin_value = 0.0
             cos_value = 0.0

          end if ! if((.not. check_nan) .and. (hyp_dist .ne. 0.0))

          ! Define local variables

          if(sin_value .gt. 1.0) sin_value = 0.99999
          if(cos_value .gt. 1.0) cos_value = 0.99999

          ! Compute local variables

          sin_angle = asin(sin_value)/deg2rad
          cos_angle = acos(cos_value)/deg2rad
          tmp_angle = 0.5*(sin_angle + cos_angle)

          ! Check local variable and proceed accordingly

          if(reflat .le. grid%lat(i) .and. reflon .le. grid%lon(i)) then

             ! Compute local variables

             grid%angle(i) = 90.0 - tmp_angle
                   
          else if(reflat .gt. grid%lat(i) .and. reflon .le. grid%lon(i))   &
               & then

             ! Compute local variables

             grid%angle(i) = 90.0 + tmp_angle

          else if(reflat .ge. grid%lat(i) .and. reflon .ge. grid%lon(i))   &
               & then

             ! Compute local variables

             grid%angle(i) = 270.0 - tmp_angle
             
          else if(reflat .lt. grid%lat(i) .and. reflon .ge. grid%lon(i))   &
               & then

             ! Compute local variables

             grid%angle(i) = 270.0 + tmp_angle

          end if ! if(reflat .le. grid%lat(i) .and. reflon
                 ! .le. grid%lon(i))

       end if ! if(dlon .eq. 0.0 .and. dlat .gt. 0.0)

    end do ! i = 1, (grid%nx*grid%ny)

    !=====================================================================
    
  end subroutine grid_methods_anglegrid

  !=======================================================================
  
  ! SUBROUTINE:

  ! grid_methods_gcdist.f90

  ! DESCRIPTION:

  ! This subroutine estimates the respective grid grid-cell resolution
  ! using the grid spacing (specified by the user) and the estimated
  ! longitude and latitude grid spacings (assuming a spherical Earth)
  ! at the equator).

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable.

  ! * dx; a FORTRAN 4-byte real value specifying the zonal (e.g., x-)
  !   coordinate resolution; units are degrees.

  ! * dy; a FORTRAN 4-byte real value specifying the meridional (e.g.,
  !   y-) coordinate resolution; units are degrees.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable where the attributes dx and
  !   dy have been define accordingly.

  !-----------------------------------------------------------------------

  subroutine grid_methods_gcdist(grid,dx,dy)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid
    real(r_kind)                                                        :: dx
    real(r_kind)                                                        :: dy

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call constants_interface_init()
    grid%dy = dlat_equator*dy
    
    ! Loop through local variable

    do i = 1, (grid%nx*grid%ny)

       ! Compute local variables

       grid%dx(i) = dlon_equator*cos(grid%lat(i)*deg2rad)*dx

    end do ! do i = 1, (grid%nx*grid%ny)

    !=====================================================================

  end subroutine grid_methods_gcdist

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
  !   geographical position (gclon and gclat) and the heading (head;
  !   degrees) and distance (dist; meters); all geographical position
  !   units are assumed to be degrees.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the newly
  !   computed geographical position (grlon and grlat).

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

    grid%grlon = lon*rad2deg
    grid%grlat = lat*rad2deg

    !=====================================================================

  end subroutine grid_methods_gcgeo

  !=======================================================================

  ! SUBROUTINE:

  ! grid_methods_geodist.f90

  ! DESCRIPTION:

  ! This subroutine computes the distances in the x- and y-directions
  ! (longitudinal and latitudinal, respectively) as a function of
  ! latitude assuming that the Earth is an ellipical geoid in units of
  ! meters; the returned 'dx' and 'dy' arrays within the grid_struct
  ! FORTRAN structure may then be multiplied by the respective grid
  ! resolution (usually in units of degrees).

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable where the dx and dy
  !   variables now contain the number of meters per degree longitude
  !   and latitude, respectively.

  !-----------------------------------------------------------------------

  subroutine grid_methods_geodist(grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid

    ! Define variables computed within routine

    real(r_kind)                                                        :: lat

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Loop through local variable

    do i = 1, (grid%nx*grid%ny)
    
       ! Define local variables

       lat = grid%lat(i)*deg2rad

       ! Compute local variables
       
       grid%dx(i) = 111412.84*cos(lat) - 93.5*cos(3.0*lat)
       grid%dy(i) = 111132.92 - 559.82*cos(2.0*lat) + 1.175*cos(4.0*lat)

       ! Check local variable and proceed accordingly

       if(grid%dx(i) .lt. 0.0) grid%dx(i) = epsilon(1.0)

    end do ! do i = 1, (grid%nx*grid%ny)

    !=====================================================================

  end subroutine grid_methods_geodist

  !=======================================================================

  ! SUBROUTINE:

  ! grid_methods_grdbld.f90

  ! DESCRIPTION:

  ! This subroutine computes a grid to be used for various
  ! higher-level computations.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable now containing the relevant
  !   grid attributes for the various higher-level computations.

  !-----------------------------------------------------------------------

  subroutine grid_methods_grdbld(grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid

    ! Define variables computed within routine

    integer                                                             :: ncoord

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    grid%gchead = 0.0
    call grid_methods_gcgeo(grid)
    grid%mxlat  = grid%grlat
    grid%gchead = 90.0
    call grid_methods_gcgeo(grid)
    grid%mnlon  = grid%grlon
    grid%gchead = 180.0
    call grid_methods_gcgeo(grid)
    grid%mnlat  = grid%grlat
    grid%gchead = 270.0
    call grid_methods_gcgeo(grid)
    grid%mxlon  = grid%grlon
    grid%nx     = ((grid%mxlon - grid%mnlon)/grid%dlon) + 1
    grid%ny     = ((grid%mxlat - grid%mnlat)/grid%dlat) + 1
    call variable_interface_setup_struct(grid)
    ncoord      = 0
    
    ! Loop through local variable

    do j = 1, grid%ny

       ! Loop through local variable

       do i = 1, grid%nx

          ! Define local variables

          ncoord            = ncoord + 1
          grid%lat(ncoord)  = grid%mnlat + (j - 1)*grid%dlat
          grid%lon(ncoord)  = grid%mnlon + (i - 1)*grid%dlon
          grid%cori(ncoord) = 2.0*earth_omega*(grid%lat(ncoord)*deg2rad)

       end do ! do i = 1, grid%nx

    end do ! do j = 1, grid%ny

    ! Compute local variables

    call grid_methods_polarcoords(grid,grid%gclon,grid%gclat)
    
    !=====================================================================

  end subroutine grid_methods_grdbld

  !=======================================================================

  ! SUBROUTINE:

  ! grid_methods_mapfactor.f90

  ! DESCRIPTION:

  ! This subroutine computes the grid distortion (e.g., map factor) as
  ! a function of latitude, longitude, and grid projection.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing (at minimum) the
  !   latitude and longitude (e.g., lat and lon attributes,
  !   respectively).

  ! * grid_type; a FORTRAN integer value specifying the
  !   grid-projection type as follows:
  
  !   1. Regular latitude-longitude grid projection.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the
  !   defined/computed map-distortion factor.

  !-----------------------------------------------------------------------

  subroutine grid_methods_mapfactor(grid,gridtype)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid
    integer                                                             :: gridtype

    !=====================================================================

    ! Compute local variables

    if(gridtype .eq. 1) call mapfactor_regll(grid)

    !=====================================================================

  end subroutine grid_methods_mapfactor

  !=======================================================================

  ! SUBROUTINE:

  ! grid_methods_polarcoords.f90

  ! DESCRIPTION:

  ! This subroutine will compute the polar coordinate values of radial
  ! distance and azimuthal angle relative to a reference geographical
  ! location.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing grid longitude
  !   and latitude values; units for longitude and latitude are
  !   degrees.

  ! * reflon; a FORTRAN 4-byte real value specifying the geographical
  !   longitude coordinate of the reference location; units are
  !   degrees.

  ! * reflat; a FORTRAN 4-byte real value specifying the geographical
  !   latitude coordinate of the reference location; units are
  !   degrees.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the computed
  !   radial distance relative to a reference Earth-bound geographical
  !   location assuming the equitorial radius of the Earth (units are
  !   meters) and the angle relative to a reference Earth-bound
  !   geographical location assuming the equitorial radius of the
  !   Earth (units are degrees).

  !-----------------------------------------------------------------------

  subroutine grid_methods_polarcoords(grid,reflon,reflat)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid
    real(r_kind)                                                        :: reflon
    real(r_kind)                                                        :: reflat

    !=====================================================================

    ! Compute local variables

    call grid_methods_radialgrid(grid,reflon,reflat)
    call grid_methods_anglegrid(grid,reflon,reflat)

    !=====================================================================

  end subroutine grid_methods_polarcoords

  !=======================================================================

  ! SUBROUTINE: 

  ! grid_methods_radialdist.f90

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

  subroutine grid_methods_radialdist(lon1,lat1,lon2,lat2,dist)

    ! Define variables passed to routine

    real(r_kind)                                                        :: lon1
    real(r_kind)                                                        :: lat1
    real(r_kind)                                                        :: lon2
    real(r_kind)                                                        :: lat2
    real(r_kind)                                                        :: dist

    ! Define variables computed within routine

    real(r_kind)                                                        :: lat1_deg
    real(r_kind)                                                        :: lat2_deg
    real(r_kind)                                                        :: lat1_rad
    real(r_kind)                                                        :: lat2_rad
    real(r_kind)                                                        :: lon1_deg
    real(r_kind)                                                        :: lon2_deg
    real(r_kind)                                                        :: lon1_rad
    real(r_kind)                                                        :: lon2_rad
    real(r_kind)                                                        :: x1
    real(r_kind)                                                        :: y1
    real(r_kind)                                                        :: z1
    real(r_kind)                                                        :: x2
    real(r_kind)                                                        :: y2
    real(r_kind)                                                        :: z2
    real(r_kind)                                                        :: dr

    !=====================================================================

    ! Compute local variables

    lon1_deg = lon1
    lon1_deg = mod(lon1_deg,360.0)
    if(lon1_deg .lt. 0.0) then
       lon1_deg = lon1_deg + 360.0
    end if ! if(lon1_deg .lt. 0.0)
    lon2_deg = lon2
    lon2_deg = mod(lon2_deg,360.0)
    if(lon2_deg .lt. 0.0) then
       lon2_deg = lon2_deg + 360.0
    end if ! if(lon2_deg .lt. 0.0)
    if(lat1 .lt. lat2) then
       lon1_rad = lon1_deg*deg2rad
       lat1_rad = (90.0 - lat1)*deg2rad
       lon2_rad = lon2_deg*deg2rad
       lat2_rad = (90.0 - lat2)*deg2rad
    else if(lat1 .eq. lat2 .and. lon1_deg .le. lon2_deg) then
       lon1_rad = lon1_deg*deg2rad
       lat1_rad = (90.0 - lat1)*deg2rad
       lon2_rad = lon2_deg*deg2rad
       lat2_rad = (90.0 - lat2)*deg2rad
    else
       lon1_rad = lon2_deg*deg2rad
       lat1_rad = (90.0 - lat2)*deg2rad
       lon2_rad = lon1_deg*deg2rad
       lat2_rad = (90.0 - lat1)*deg2rad
    end if ! if(lat1 .lt. lat2)
    x1   = sin(lat1_rad)*cos(lon1_rad)
    y1   = sin(lat1_rad)*sin(lon1_rad)
    z1   = cos(lat1_rad)
    x2   = sin(lat2_rad)*cos(lon2_rad)
    y2   = sin(lat2_rad)*sin(lon2_rad)
    z2   = cos(lat2_rad)
    dr   = (acos(min(1.0,x1*x2 + y1*y2 + z1*z2)))
    dist = dr*rearth_equator

    !=====================================================================

  end subroutine grid_methods_radialdist

  !=======================================================================

  ! SUBROUTINE: 

  ! grid_methods_radialgrid.f90

  ! DESCRIPTION:

  ! This subroutine computes a grid of radial distance values relative
  ! to a reference Earth-bound geographical location and a grid of
  ! longitude and latitude values assuming the equitorial radius of
  ! the Earth.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing grid longitude
  !   and latitude values; units for longitude and latitude are
  !   degrees.

  ! * reflat; a FORTRAN 4-byte real value specifying the geographical
  !   latitude coordinate of the reference location; units are
  !   degrees.

  ! * reflon; a FORTRAN 4-byte real value specifying the geographical
  !   longitude coordinate of the reference location; units are
  !   degrees.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the computed
  !   radial distance relative to a reference Earth-bound geographical
  !   location assuming the equitorial radius of the Earth; units are
  !   meters.

  !-----------------------------------------------------------------------

  subroutine grid_methods_radialgrid(grid,reflon,reflat)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid
    real(r_kind)                                                        :: reflon
    real(r_kind)                                                        :: reflat
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================
    
    ! Loop through local variable

    do i = 1, (grid%nx*grid%ny)

       ! Compute local variables

       call grid_methods_radialdist(reflon,reflat,grid%lon(i),             &
            & grid%lat(i),grid%radius(i))

    end do ! i = 1, (grid%nx*grid%ny)

    !=====================================================================
    
  end subroutine grid_methods_radialgrid

  !=======================================================================

  ! SUBROUTINE:

  ! mapfactor_regll.f90

  ! DESCRIPTION:

  ! This subroutine defines the map distortion factor (e.g., mapfac)
  ! for a regular latitude/longitude grid.

  ! INPUT VARIBLES:

  ! * grid; a FORTRAN grid_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the map
  !   distortion factor within the 'mapfac' attribute.

  !-----------------------------------------------------------------------

  subroutine mapfactor_regll(grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    grid%mapfac = 1.0

    !=====================================================================

  end subroutine mapfactor_regll

  !=======================================================================

end module grid_methods_interface
