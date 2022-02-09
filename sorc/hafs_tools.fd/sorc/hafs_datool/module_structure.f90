module constants
!-----------------------------------------------------------------------------
! define all parameter and constants
! Yonghui Weng, 20210204
!-----------------------------------------------------------------------------
  implicit none
  real, parameter :: g=9.81,                       &! gravitational constant
                     r_earth=6374.,                &! radius of the earth
                     pi = 3.1415927,               &!
                     deg2rad = pi/180.0,           &!
                     rad2deg = 1.0/deg2rad,        &!
                     cp=1004.5,                    &! specific heat
                     rd=287.,                      &! dry air gas constant
                     rv=461.6,                     &! moist air gas constant
                     es_alpha = 611.2,             &! saturation vaporpressure
                     es_beta = 17.67,              &!
                     es_gamma = 243.5,             &!
                     missing=9.999e+20               ! missing value
! parameter to determine interpolation method
!  integer, parameter :: CONSERVE = 1
!  integer, parameter :: BILINEAR = 2
!  integer, parameter :: SPHERICA = 3
!  integer, parameter :: BICUBIC  = 4

end module constants

!==============================================================================
module var_type
!-----------------------------------------------------------------------------
! define var
! Yonghui Weng, 20210204
!-----------------------------------------------------------------------------
 type grid2d_info
      integer :: grid_x, grid_y, ntime, grid_xt, grid_yt
      real*8, allocatable, dimension(:)  :: times
      character(len=80)     :: times_unit
      real, allocatable, dimension(:,:)  :: grid_lon, grid_lat, grid_lont, grid_latt, grid_area
 end type grid2d_info

 type gridmap_info
      integer :: src_points, dst_points
      integer, allocatable, dimension(:) :: src_x, src_y   !position in source grid
      integer, allocatable, dimension(:) :: dst_x, dst_y   !dst grids will be used for smooth, to reduce the gaps
      real, allocatable, dimension(:)  :: src_weight, dst_weight
 end type gridmap_info

 type grid_weight_info
      integer  :: max_points, relaxzone
      type(gridmap_info),allocatable,dimension(:,:) :: gwt_t, gwt_u, gwt_v
      real, allocatable, dimension(:,:) :: cangu, sangu, cangv, sangv
 end type grid_weight_info

 type(grid_weight_info) :: gwt

 type tc_track_info
      integer               :: vortexrep  ! 1=tc vortex-replacement, others are not
      real                  :: lat, lon, pmin, vmax
      real, dimension(2)    :: vortexreplace_r 
 end type tc_track_info
 type(tc_track_info)    :: tc

 integer                :: debug_level  ! default is 1, only print basic information
                                        !            2-9: 

end module var_type
 
