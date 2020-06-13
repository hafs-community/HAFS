module variable_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: variable_interface
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

  use kinds_interface
  use mpi_interface
  use namelist_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: bkgrdfltr_struct
  public :: clpval
  public :: fft2d_struct
  public :: filter_struct
  public :: filterdiag_struct
  public :: grib_struct
  public :: grid_struct
  public :: json_grib_struct
  public :: kdtree_struct
  public :: linterp_struct
  public :: lyrmn_struct
  public :: meteo_struct
  public :: moisture_struct
  public :: ntcs
  public :: nwind_vars
  public :: poisson_struct
  public :: pyththrm_struct
  public :: recenter_struct
  public :: slint_struct
  public :: smthr9pt_struct
  public :: spline_struct
  public :: spval
  public :: statgrid_struct
  public :: subregion_struct
  public :: tccps_struct
  public :: tcdiag_struct
  public :: tcenv_struct
  public :: tcincr_struct
  public :: tcmdiag_struct
  public :: tcmpi_struct
  public :: tcmsi_struct
  public :: tcv_struct
  public :: variable_interface_check_nan
  public :: variable_interface_cleanup_struct
  public :: variable_interface_setup_struct
  public :: varinfo_struct
  public :: vargrid_struct
  public :: vertgrid_struct
  public :: vrtxinfo_struct
  public :: wnd2d_struct
  interface variable_interface_check_nan
     module procedure check_nan_real
  end interface variable_interface_check_nan
  interface variable_interface_cleanup_struct
     module procedure finalize_bkgrdfltr_struct
     module procedure finalize_fft2d_struct
     module procedure finalize_filter_struct
     module procedure finalize_grib_struct
     module procedure finalize_grid_struct
     module procedure finalize_kdtree_struct
     module procedure finalize_linterp_struct
     module procedure finalize_lyrmn_struct
     module procedure finalize_meteo_struct
     module procedure finalize_moisture_struct
     module procedure finalize_mpi_taskgrid_struct
     module procedure finalize_poisson_struct
     module procedure finalize_recenter_struct
     module procedure finalize_slint_struct
     module procedure finalize_smthr9pt_struct
     module procedure finalize_spline_struct
     module procedure finalize_subregion_struct
     module procedure finalize_tccps_struct
     module procedure finalize_tcenv_struct
     module procedure finalize_tcincr_struct
     module procedure finalize_tcmpi_struct
     module procedure finalize_tcmsi_struct
     module procedure finalize_vargrid_struct
     module procedure finalize_varinfo_struct
     module procedure finalize_vertgrid_struct
     module procedure finalize_wnd2d_struct
  end interface variable_interface_cleanup_struct
  interface variable_interface_setup_struct
     module procedure initialize_bkgrdfltr_struct
     module procedure initialize_fft2d_struct
     module procedure initialize_filter_struct
     module procedure initialize_grib_struct
     module procedure initialize_grid_struct
     module procedure initialize_linterp_struct
     module procedure initialize_lyrmn_struct
     module procedure initialize_kdtree_struct
     module procedure initialize_meteo_struct
     module procedure initialize_moisture_struct
     module procedure initialize_mpi_taskgrid_struct
     module procedure initialize_poisson_struct
     module procedure initialize_recenter_struct
     module procedure initialize_slint_struct
     module procedure initialize_smthr9pt_struct
     module procedure initialize_spline_struct
     module procedure initialize_subregion_struct
     module procedure initialize_tccps_struct
     module procedure initialize_tcenv_struct
     module procedure initialize_tcincr_struct
     module procedure initialize_tcmpi_struct
     module procedure initialize_tcmsi_struct
     module procedure initialize_vargrid_struct
     module procedure initialize_varinfo_struct
     module procedure initialize_vertgrid_struct
     module procedure initialize_wnd2d_struct
  end interface variable_interface_setup_struct

  ! Define local variables

  real(r_kind), parameter                                               :: clpval = tiny(0.0)
  real(r_kind), parameter                                               :: spval  = huge(0.0)
  type bkgrdfltr_struct
     real(r_kind),              dimension(:,:),             allocatable :: divg
     real(r_kind),              dimension(:,:),             allocatable :: filter
     real(r_kind),              dimension(:,:),             allocatable :: mask
     real(r_kind),              dimension(:,:),             allocatable :: q
     real(r_kind),              dimension(:,:),             allocatable :: radius
     real(r_kind),              dimension(:,:),             allocatable :: t
     real(r_kind),              dimension(:,:),             allocatable :: vort
     real(r_kind),              dimension(:,:),             allocatable :: whrmu
     real(r_kind),              dimension(:,:),             allocatable :: whrmv
     real(r_kind),              dimension(:,:),             allocatable :: z
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: pmsl
     integer,                   dimension(:),               allocatable :: cidx
     integer,                   dimension(:),               allocatable :: minx
     integer,                   dimension(:),               allocatable :: maxx
     integer,                   dimension(:),               allocatable :: miny
     integer,                   dimension(:),               allocatable :: maxy
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz
  end type bkgrdfltr_struct       ! type bkgrdfltr_struct
  type fft2d_struct
     complex(r_double),         dimension(:,:),             allocatable :: out
     complex(r_double),         dimension(:,:),             allocatable :: in
     integer                                                            :: nx
     integer                                                            :: ny
  end type fft2d_struct           ! type fft2d_struct
  type filter_struct
     real(r_kind),              dimension(:,:),             allocatable :: angle
     real(r_kind),              dimension(:,:),             allocatable :: kfltr
     real(r_kind),              dimension(:,:),             allocatable :: radius
     real(r_kind),              dimension(:,:),             allocatable :: tfltr
     real(r_kind),              dimension(:,:),             allocatable :: tngwnd
     real(r_kind),              dimension(:),               allocatable :: clat
     real(r_kind),              dimension(:),               allocatable :: clon
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: plev
     real(r_kind)                                                       :: maxradius
     integer,                   dimension(:),               allocatable :: cidx
     integer,                   dimension(:),               allocatable :: kxmin
     integer,                   dimension(:),               allocatable :: kxmax
     integer,                   dimension(:),               allocatable :: kymin
     integer,                   dimension(:),               allocatable :: kymax
     integer,                   dimension(:),               allocatable :: txmin
     integer,                   dimension(:),               allocatable :: txmax
     integer,                   dimension(:),               allocatable :: tymin
     integer,                   dimension(:),               allocatable :: tymax
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz
     integer                                                            :: ncoords
  end type filter_struct          ! type filter_struct
  type filterdiag_struct
     real(r_kind)                                                       :: dangle
     real(r_kind)                                                       :: dradius
     real(r_kind)                                                       :: dwndthrsh
     real(r_kind)                                                       :: kinfltfac
     real(r_kind)                                                       :: lg_radii
     real(r_kind)                                                       :: maxradius
     real(r_kind)                                                       :: mn_dist
     real(r_kind)                                                       :: mx_dist
     real(r_kind)                                                       :: ptop
     real(r_kind)                                                       :: sm_radii
     real(r_kind)                                                       :: tinfltfac
     real(r_kind)                                                       :: twndthrsh
     real(r_kind)                                                       :: wndthrsh
  end type filterdiag_struct      ! type filterdiag_struct
  type grib_struct
     character(len=500)                                                 :: gribVar
     character(len=500)                                                 :: shortName
     character(len=19)                                                  :: date
     real(r_kind),              dimension(:,:),             allocatable :: lat
     real(r_kind),              dimension(:,:),             allocatable :: lon
     real(r_kind),              dimension(:,:),             allocatable :: values
     real(r_kind),              dimension(:),               allocatable :: levs
     real(r_kind)                                                       :: errorValue
     real(r_kind)                                                       :: dlat
     real(r_kind)                                                       :: dlon
     integer                                                            :: endStep
     integer                                                            :: gridtype
     integer                                                            :: indicatorOfTypeOfLevel
     integer                                                            :: indicatorOfParameter
     integer                                                            :: level
     integer                                                            :: levtype
     integer                                                            :: startStep
     integer                                                            :: validityDate
     integer                                                            :: validityTime
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz
  end type grib_struct            ! type grib_struct
  type grid_struct
     real(r_kind),              dimension(:),               allocatable :: angle
     real(r_kind),              dimension(:),               allocatable :: cori
     real(r_kind),              dimension(:),               allocatable :: dx
     real(r_kind),              dimension(:),               allocatable :: dy
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: mapfac
     real(r_kind),              dimension(:),               allocatable :: radius
     real(r_kind),              dimension(:),               allocatable :: var
     real(r_kind)                                                       :: dlat
     real(r_kind)                                                       :: dlon
     real(r_kind)                                                       :: gcdist
     real(r_kind)                                                       :: gchead
     real(r_kind)                                                       :: gclat
     real(r_kind)                                                       :: gclon
     real(r_kind)                                                       :: grlat
     real(r_kind)                                                       :: grlon
     real(r_kind)                                                       :: mnlat
     real(r_kind)                                                       :: mnlon
     real(r_kind)                                                       :: mxlat
     real(r_kind)                                                       :: mxlon
     integer,                   dimension(:),               allocatable :: idx
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
  end type grid_struct            ! type grid_struct
  type json_grib_struct
     character(len=500)                                                 :: gribVar
     character(len=500)                                                 :: shortName
     integer                                                            :: indicatorOfTypeOfLevel
     integer                                                            :: indicatorOfParameter
     integer                                                            :: levType
  end type json_grib_struct       ! type json_grib_struct
  type kdtree_struct
     real(r_kind),              dimension(:,:),             allocatable :: r2dist
     integer,                   dimension(:,:),             allocatable :: idx
     integer                                                            :: ncoords
     integer                                                            :: nn
  end type kdtree_struct          ! type kdtree_struct
  type linterp_struct
     real(r_kind),              dimension(:),               allocatable :: xa
     real(r_kind),              dimension(:),               allocatable :: ya
     real(r_kind)                                                       :: x
     real(r_kind)                                                       :: y
     integer                                                            :: n
  end type linterp_struct         ! type linterp_struct
  type lyrmn_struct
     real(r_kind),              dimension(:,:),             allocatable :: p
     real(r_kind),              dimension(:,:),             allocatable :: var
     real(r_kind),              dimension(:),               allocatable :: varmean
     real(r_kind)                                                       :: pmax
     real(r_kind)                                                       :: pmin
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz
  end type lyrmn_struct           ! type lyrmn_struct
  type meteo_struct
     real(r_kind),              dimension(:,:),             allocatable :: chi
     real(r_kind),              dimension(:,:),             allocatable :: divg
     real(r_kind),              dimension(:,:),             allocatable :: p
     real(r_kind),              dimension(:,:),             allocatable :: pott
     real(r_kind),              dimension(:,:),             allocatable :: psi
     real(r_kind),              dimension(:,:),             allocatable :: pv
     real(r_kind),              dimension(:,:),             allocatable :: q
     real(r_kind),              dimension(:,:),             allocatable :: rh
     real(r_kind),              dimension(:,:),             allocatable :: t
     real(r_kind),              dimension(:,:),             allocatable :: tngwnd
     real(r_kind),              dimension(:,:),             allocatable :: u
     real(r_kind),              dimension(:,:),             allocatable :: v
     real(r_kind),              dimension(:,:),             allocatable :: virtt     
     real(r_kind),              dimension(:,:),             allocatable :: vort
     real(r_kind),              dimension(:,:),             allocatable :: wdivu
     real(r_kind),              dimension(:,:),             allocatable :: wdivv
     real(r_kind),              dimension(:,:),             allocatable :: whrmu
     real(r_kind),              dimension(:,:),             allocatable :: whrmv
     real(r_kind),              dimension(:,:),             allocatable :: wndspd
     real(r_kind),              dimension(:,:),             allocatable :: wrotu
     real(r_kind),              dimension(:,:),             allocatable :: wrotv
     real(r_kind),              dimension(:,:),             allocatable :: wvmxrt
     real(r_kind),              dimension(:,:),             allocatable :: z
     real(r_kind),              dimension(:),               allocatable :: cori
     real(r_kind),              dimension(:),               allocatable :: dx
     real(r_kind),              dimension(:),               allocatable :: dy
     real(r_kind),              dimension(:),               allocatable :: land
     real(r_kind),              dimension(:),               allocatable :: mapfac
     real(r_kind),              dimension(:),               allocatable :: orog
     real(r_kind),              dimension(:),               allocatable :: pmsl
     real(r_kind),              dimension(:),               allocatable :: psfc
     real(r_kind),              dimension(:),               allocatable :: tsfc
     real(r_kind),              dimension(:),               allocatable :: u10m
     real(r_kind),              dimension(:),               allocatable :: v10m
     real(r_kind),              dimension(:),               allocatable :: xlat
     real(r_kind),              dimension(:),               allocatable :: xlon
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz     
  end type meteo_struct           ! type meteo_struct
  type moisture_struct
     real(r_kind),              dimension(:,:),             allocatable :: p    
     real(r_kind),              dimension(:,:),             allocatable :: q
     real(r_kind),              dimension(:,:),             allocatable :: rh
     real(r_kind),              dimension(:,:),             allocatable :: svp
     real(r_kind),              dimension(:,:),             allocatable :: swvmxrt
     real(r_kind),              dimension(:,:),             allocatable :: t 
     real(r_kind),              dimension(:,:),             allocatable :: vp
     real(r_kind),              dimension(:,:),             allocatable :: wvmxrt
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz 
  end type moisture_struct        ! type moisture_struct
  type poisson_struct
     character(len=4)                                                   :: bctype
     real(r_double),            dimension(:,:),             allocatable :: rhs
     real(r_double),            dimension(:,:),             allocatable :: sol
     real(r_double),            dimension(:),               allocatable :: bd_ax
     real(r_double),            dimension(:),               allocatable :: bd_bx
     real(r_double),            dimension(:),               allocatable :: bd_ay
     real(r_double),            dimension(:),               allocatable :: bd_by
     real(r_double)                                                     :: q
     real(r_double)                                                     :: xmin
     real(r_double)                                                     :: xmax
     real(r_double)                                                     :: ymin
     real(r_double)                                                     :: ymax
     integer                                                            :: nx
     integer                                                            :: ny
  end type poisson_struct         ! type poisson_struct
  type pyththrm_struct
     real(r_kind)                                                       :: a    = spval
     real(r_kind)                                                       :: b    = spval
     real(r_kind)                                                       :: c    = spval
     real(r_kind)                                                       :: anga = spval
     real(r_kind)                                                       :: angb = spval
     real(r_kind)                                                       :: angc = spval
  end type pyththrm_struct        ! type pyththrm_struct  
  type recenter_struct
     real(r_kind),              dimension(:),               allocatable :: angle
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: radius
     real(r_kind),              dimension(:),               allocatable :: var
     real(r_kind)                                                       :: area
     real(r_kind)                                                       :: clat
     real(r_kind)                                                       :: clon
     real(r_kind)                                                       :: darea
     real(r_kind)                                                       :: dradius
     integer,                   dimension(:),               allocatable :: idx
     integer,                   dimension(:),               allocatable :: jdx
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: ridx
  end type recenter_struct        ! type recenter_struct
  type slint_struct
     real(r_double),            dimension(:,:),             allocatable :: coeffs
     real(r_kind),              dimension(:),               allocatable :: var
     real(r_kind),              dimension(:),               allocatable :: dst_lon
     real(r_kind),              dimension(:),               allocatable :: dst_lat
     real(r_kind),              dimension(:),               allocatable :: src_lon
     real(r_kind),              dimension(:),               allocatable :: src_lat
     integer,                   dimension(:,:),             allocatable :: nn
     integer                                                            :: ncoeffs
     integer                                                            :: dst_ncoords
     integer                                                            :: src_ncoords
  end type slint_struct           ! type slint_struct
  type smthr9pt_struct
     real(r_kind),              dimension(:),               allocatable :: filter
     real(r_kind),              dimension(:),               allocatable :: var
     real(r_kind)                                                       :: thrshratio
     integer                                                            :: cidx
     integer                                                            :: minx
     integer                                                            :: maxx
     integer                                                            :: miny
     integer                                                            :: maxy
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
  end type smthr9pt_struct        ! smthr9pt_struct  
  type spline_struct
     real(r_kind),              dimension(:),               allocatable :: xa
     real(r_kind),              dimension(:),               allocatable :: ya
     real(r_kind)                                                       :: x
     real(r_kind)                                                       :: y
     integer                                                            :: n
  end type spline_struct          ! type spline_struct
  type statgrid_struct
     real(r_kind)                                                       :: mean
     real(r_kind)                                                       :: vari
     real(r_kind)                                                       :: stdev
     real(r_kind)                                                       :: varmin
     real(r_kind)                                                       :: varmax
     integer                                                            :: nvals
  end type statgrid_struct        ! type statgrid_struct
  type subregion_struct
     integer,                   dimension(:),               allocatable :: idx
     integer                                                            :: ncoords
  end type subregion_struct       ! type subregion_struct  
  type tccps_struct
     real(r_kind),              dimension(:,:),             allocatable :: p
     real(r_kind),              dimension(:,:),             allocatable :: z
     real(r_kind),              dimension(:),               allocatable :: lats
     real(r_kind),              dimension(:),               allocatable :: lons
     real(r_kind),              dimension(:),               allocatable :: radius
     real(r_kind)                                                       :: vlt
     real(r_kind)                                                       :: vut
     real(r_kind)                                                       :: clon
     real(r_kind)                                                       :: clat
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz
  end type tccps_struct           ! type tccps_struct
  type tcenv_struct
     character(len=4),          dimension(:),               allocatable :: tcid
     character(len=19)                                                  :: valid_time
     real(r_kind),              dimension(:),               allocatable :: lats
     real(r_kind),              dimension(:),               allocatable :: lons
     real(r_kind),              dimension(:),               allocatable :: tclat
     real(r_kind),              dimension(:),               allocatable :: tclon
     real(r_kind),              dimension(:),               allocatable :: u850_200
     real(r_kind),              dimension(:),               allocatable :: u850_200_shear
     real(r_kind),              dimension(:),               allocatable :: u925
     real(r_kind),              dimension(:),               allocatable :: u935
     real(r_kind),              dimension(:),               allocatable :: u945
     real(r_kind),              dimension(:),               allocatable :: u955
     real(r_kind),              dimension(:),               allocatable :: u965
     real(r_kind),              dimension(:),               allocatable :: u975
     real(r_kind),              dimension(:),               allocatable :: u985
     real(r_kind),              dimension(:),               allocatable :: u995
     real(r_kind),              dimension(:),               allocatable :: u1005
     real(r_kind),              dimension(:),               allocatable :: v850_200
     real(r_kind),              dimension(:),               allocatable :: v850_200_shear
     real(r_kind),              dimension(:),               allocatable :: v925
     real(r_kind),              dimension(:),               allocatable :: v935
     real(r_kind),              dimension(:),               allocatable :: v945
     real(r_kind),              dimension(:),               allocatable :: v955
     real(r_kind),              dimension(:),               allocatable :: v965
     real(r_kind),              dimension(:),               allocatable :: v975
     real(r_kind),              dimension(:),               allocatable :: v985
     real(r_kind),              dimension(:),               allocatable :: v995
     real(r_kind),              dimension(:),               allocatable :: v1005
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
  end type tcenv_struct           ! type tcenv_struct
  type tcdiag_struct
     real(r_kind)                                                       :: dangle
     real(r_kind)                                                       :: dpmsl
     real(r_kind)                                                       :: dradius
     real(r_kind)                                                       :: maxradius
     real(r_kind)                                                       :: pvu_tcdpth
     real(r_kind)                                                       :: srcharea
     real(r_kind)                                                       :: srchradius
     real(r_kind)                                                       :: tcdpth_pbot
     real(r_kind)                                                       :: tcdpth_ptop
     real(r_kind)                                                       :: thetasfc
     real(r_kind)                                                       :: vrtxsz
  end type tcdiag_struct          ! type tcdiag_struct
  type tcincr_struct
     real(r_kind),              dimension(:,:),             allocatable :: q
     real(r_kind),              dimension(:,:),             allocatable :: t
     real(r_kind),              dimension(:,:),             allocatable :: u
     real(r_kind),              dimension(:,:),             allocatable :: v
     real(r_kind),              dimension(:,:),             allocatable :: z
     real(r_kind),              dimension(:),               allocatable :: pmsl
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz    
  end type tcincr_struct          ! type tcincr_struct  
  type tcmdiag_struct
     real(r_kind)                                                       :: area
     real(r_kind)                                                       :: dangle
     real(r_kind)                                                       :: darea
     real(r_kind)                                                       :: dradius
     real(r_kind)                                                       :: roci
     real(r_kind)                                                       :: xlat
     real(r_kind)                                                       :: xlon
     integer                                                            :: mxwvn
  end type tcmdiag_struct         ! type tcmdiag_struct
  type tcmpi_struct
     character(len=4),          dimension(:),               allocatable :: tcid
     character(len=19)                                                  :: valid_time
     real(r_kind),              dimension(:,:),             allocatable :: mxrt
     real(r_kind),              dimension(:,:),             allocatable :: p
     real(r_kind),              dimension(:,:),             allocatable :: t
     real(r_kind),              dimension(:),               allocatable :: lats
     real(r_kind),              dimension(:),               allocatable :: lons
     real(r_kind),              dimension(:),               allocatable :: pmin
     real(r_kind),              dimension(:),               allocatable :: pmsl
     real(r_kind),              dimension(:),               allocatable :: sst
     real(r_kind),              dimension(:),               allocatable :: tclat
     real(r_kind),              dimension(:),               allocatable :: tclon
     real(r_kind),              dimension(:),               allocatable :: vmax
     integer,                   dimension(:),               allocatable :: ifl
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz 
  end type tcmpi_struct           ! type tcmpi_struct
  type tcmsi_struct
     real(r_kind),              dimension(:,:,:),           allocatable :: wndvar
     real(r_kind),              dimension(:,:),             allocatable :: lats
     real(r_kind),              dimension(:,:),             allocatable :: lons
     real(r_kind),              dimension(:,:),             allocatable :: radius
     real(r_kind),              dimension(:,:),             allocatable :: var
     real(r_kind),              dimension(:,:),             allocatable :: w10m
     real(r_kind)                                                       :: ike_hur
     real(r_kind)                                                       :: ike_ts
     real(r_kind)                                                       :: r34kt
     real(r_kind)                                                       :: r50kt
     real(r_kind)                                                       :: r64kt
     real(r_kind)                                                       :: rmw
     real(r_kind)                                                       :: roci
     real(r_kind)                                                       :: vmax
     real(r_kind)                                                       :: vwn0
     real(r_kind)                                                       :: vwn1
     real(r_kind)                                                       :: vwn0pwn1
     real(r_kind)                                                       :: veps
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nh
  end type tcmsi_struct           ! type tcmsi_struct
  type tcv_struct
     character(len=19)                                                  :: valid_time
     character(len=4)                                                   :: tcid
     real(r_kind)                                                       :: reflon  
     real(r_kind)                                                       :: reflat
  end type tcv_struct             ! type tcv_struct
  type vargrid_struct
     real(r_kind),              dimension(:),               allocatable :: var
     real(r_kind)                                                       :: cutoff
     integer                                                            :: xmin
     integer                                                            :: xmax
     integer                                                            :: ymin
     integer                                                            :: ymax
     integer                                                            :: nvals
     integer                                                            :: nx
     integer                                                            :: ny
  end type vargrid_struct         ! type vargrid_struct
  type varinfo_struct
     character(len=500),        dimension(:,:,:),           allocatable :: varattrs
     character(len=100),        dimension(:),               allocatable :: varname
     character(len=10),         dimension(:),               allocatable :: dimname
     character(len=10),         dimension(:),               allocatable :: vartype
     integer,                   dimension(:,:),             allocatable :: vardimid
     integer,                   dimension(:),               allocatable :: dimid
     integer,                   dimension(:),               allocatable :: dimval
     integer,                   dimension(:),               allocatable :: varid
     integer,                   dimension(:),               allocatable :: varndims
     integer,                   dimension(:),               allocatable :: varnattrs
     integer                                                            :: nvars
     integer                                                            :: ndims
     integer                                                            :: nattrs
  end type varinfo_struct         ! type varinfo_struct
  type vertgrid_struct        
     real(r_kind),              dimension(:),               allocatable :: dst_coord
     real(r_kind),              dimension(:),               allocatable :: dst_prs
     real(r_kind),              dimension(:),               allocatable :: dst_var
     real(r_kind),              dimension(:),               allocatable :: src_coord
     real(r_kind),              dimension(:),               allocatable :: src_prs
     real(r_kind),              dimension(:),               allocatable :: src_var
     integer                                                            :: dst_nz
     integer                                                            :: src_nz
  end type vertgrid_struct        ! type vertgrid_struct  
  type vrtxinfo_struct 
     character(len=4)                                                   :: id
     real(r_kind)                                                       :: ike_hur
     real(r_kind)                                                       :: ike_ts
     real(r_kind)                                                       :: mslp
     real(r_kind)                                                       :: obslat
     real(r_kind)                                                       :: obslon
     real(r_kind)                                                       :: poci
     real(r_kind)                                                       :: r34kt
     real(r_kind)                                                       :: r50kt
     real(r_kind)                                                       :: r64kt
     real(r_kind)                                                       :: reflat
     real(r_kind)                                                       :: reflon
     real(r_kind)                                                       :: rmw
     real(r_kind)                                                       :: roci
     real(r_kind)                                                       :: strdpth
     real(r_kind)                                                       :: tltangle
     real(r_kind)                                                       :: tltdist
     real(r_kind)                                                       :: vmax
     real(r_kind)                                                       :: vrtxdpth
     real(r_kind)                                                       :: vrtxsz
  end type vrtxinfo_struct        ! type vrtxinfo_struct
  type wnd2d_struct
     real(r_kind),              dimension(:,:,:),           allocatable :: wnvar
     real(r_kind),              dimension(:),               allocatable :: var
     real(r_kind)                                                       :: dangle
     real(r_kind)                                                       :: dradius
     real(r_kind)                                                       :: maxradius
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nh
     integer                                                            :: na
     integer                                                            :: nr
  end type wnd2d_struct           ! type wnd2d_struct
  integer                                                               :: ntcs
  integer                                                               :: nwind_vars = 6

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! FUNCTION:

  ! check_nan_real.f90

  ! DESCRIPTION:

  ! This function reads a real-valued variable value and returns a
  ! logical variable indicating whether the value is a NaN or +/-
  ! Infinity.

  ! INPUT VARIABLES:

  ! varval; a FORTRAN 4-byte real-valued variable.

  !-----------------------------------------------------------------------

  logical function check_nan_real(varval)

    ! Define variables passed to routine

    real(r_kind)                                                        :: varval

    !=====================================================================

    ! Define local variables

    check_nan_real = .not. ((varval .ge. -1.0*huge(varval)) .and.          &
         & (varval .le. huge(varval)))

    !=====================================================================

  end function check_nan_real

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_bkgrdfltr_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! bkgrdfltr_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN bkgrdfltr_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_bkgrdfltr_struct(grid)

    ! Define variables passed to routine

    type(bkgrdfltr_struct)                                              :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%divg))   deallocate(grid%divg)
    if(allocated(grid%filter)) deallocate(grid%filter)
    if(allocated(grid%mask))   deallocate(grid%mask)
    if(allocated(grid%q))      deallocate(grid%q)
    if(allocated(grid%radius)) deallocate(grid%radius)
    if(allocated(grid%t))      deallocate(grid%t)
    if(allocated(grid%vort))   deallocate(grid%vort)
    if(allocated(grid%whrmu))  deallocate(grid%whrmu)
    if(allocated(grid%whrmv))  deallocate(grid%whrmv)
    if(allocated(grid%z))      deallocate(grid%z)
    if(allocated(grid%lat))    deallocate(grid%lat)
    if(allocated(grid%lon))    deallocate(grid%lon)
    if(allocated(grid%pmsl))   deallocate(grid%pmsl)
    if(allocated(grid%cidx))   deallocate(grid%cidx)
    if(allocated(grid%minx))   deallocate(grid%minx)
    if(allocated(grid%maxx))   deallocate(grid%maxx)
    if(allocated(grid%miny))   deallocate(grid%miny)
    if(allocated(grid%maxy))   deallocate(grid%maxy)

    !=====================================================================

  end subroutine finalize_bkgrdfltr_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_fft2d_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! fft2d_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN fft2d_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_fft2d_struct(grid)

    ! Define variables passed to routine

    type(fft2d_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%out)) deallocate(grid%out)
    if(allocated(grid%in))  deallocate(grid%in)

    !=====================================================================

  end subroutine finalize_fft2d_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_filter_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! filter_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN filter_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_filter_struct(grid)

    ! Define variables passed to routine

    type(filter_struct)                                                 :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(allocated(grid%angle))  deallocate(grid%angle)
    if(allocated(grid%kfltr))  deallocate(grid%kfltr)
    if(allocated(grid%radius)) deallocate(grid%radius)
    if(allocated(grid%tfltr))  deallocate(grid%tfltr)
    if(allocated(grid%tngwnd)) deallocate(grid%tngwnd)
    if(allocated(grid%clat))   deallocate(grid%clat)
    if(allocated(grid%clon))   deallocate(grid%clon)
    if(allocated(grid%lat))    deallocate(grid%lat)
    if(allocated(grid%lon))    deallocate(grid%lon)
    if(allocated(grid%plev))   deallocate(grid%plev)
    if(allocated(grid%cidx))   deallocate(grid%cidx)
    if(allocated(grid%kxmin))  deallocate(grid%kxmin)
    if(allocated(grid%kxmax))  deallocate(grid%kxmax)
    if(allocated(grid%kymin))  deallocate(grid%kymin)
    if(allocated(grid%kymax))  deallocate(grid%kymax)
    if(allocated(grid%txmin))  deallocate(grid%txmin)
    if(allocated(grid%txmax))  deallocate(grid%txmax)
    if(allocated(grid%tymin))  deallocate(grid%tymin)
    if(allocated(grid%tymax))  deallocate(grid%tymax)

    !=====================================================================

  end subroutine finalize_filter_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_grib_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! grib_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grib_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_grib_struct(grid)

    ! Define variables passed to routine

    type(grib_struct)                                                   :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%lat))    deallocate(grid%lat)
    if(allocated(grid%lon))    deallocate(grid%lon)
    if(allocated(grid%values)) deallocate(grid%values)
    if(allocated(grid%levs))   deallocate(grid%levs)

    !=====================================================================

  end subroutine finalize_grib_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_grid_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! grid_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_grid_struct(grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%angle))  deallocate(grid%angle)
    if(allocated(grid%cori))   deallocate(grid%cori)
    if(allocated(grid%dx))     deallocate(grid%dx)
    if(allocated(grid%dy))     deallocate(grid%dy)
    if(allocated(grid%lat))    deallocate(grid%lat)
    if(allocated(grid%lon))    deallocate(grid%lon)
    if(allocated(grid%mapfac)) deallocate(grid%mapfac)
    if(allocated(grid%radius)) deallocate(grid%radius)
    if(allocated(grid%var))    deallocate(grid%var)
    if(allocated(grid%idx))    deallocate(grid%idx)

    !=====================================================================

  end subroutine finalize_grid_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_kdtree_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! kdtree_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN kdtree_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_kdtree_struct(grid)

    ! Define variables passed to routine

    type(kdtree_struct)                                                 :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%r2dist)) deallocate(grid%r2dist)
    if(allocated(grid%idx))    deallocate(grid%idx)

    !=====================================================================

  end subroutine finalize_kdtree_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_linterp_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! linterp_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN linterp_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_linterp_struct(grid)

    ! Define variables passed to routine

    type(linterp_struct)                                                :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%xa)) deallocate(grid%xa)
    if(allocated(grid%ya)) deallocate(grid%ya)

    !=====================================================================
    
  end subroutine finalize_linterp_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_lyrmn_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! lyrmn_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN lyrmn_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_lyrmn_struct(grid)

    ! Define variables passed to routine

    type(lyrmn_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%p))       deallocate(grid%p)
    if(allocated(grid%var))     deallocate(grid%var)
    if(allocated(grid%varmean)) deallocate(grid%varmean)
    
    !=====================================================================
    
  end subroutine finalize_lyrmn_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_meteo_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! meteo_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_meteo_struct(grid)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%chi))    deallocate(grid%chi)
    if(allocated(grid%divg))   deallocate(grid%divg)
    if(allocated(grid%p))      deallocate(grid%p)
    if(allocated(grid%pott))   deallocate(grid%pott)
    if(allocated(grid%psi))    deallocate(grid%psi)
    if(allocated(grid%pv))     deallocate(grid%pv)
    if(allocated(grid%q))      deallocate(grid%q)
    if(allocated(grid%rh))     deallocate(grid%rh)
    if(allocated(grid%t))      deallocate(grid%t)
    if(allocated(grid%tngwnd)) deallocate(grid%tngwnd)
    if(allocated(grid%u))      deallocate(grid%u)
    if(allocated(grid%v))      deallocate(grid%v)
    if(allocated(grid%virtt))  deallocate(grid%virtt)
    if(allocated(grid%vort))   deallocate(grid%vort)
    if(allocated(grid%wdivu))  deallocate(grid%wdivu)
    if(allocated(grid%wdivv))  deallocate(grid%wdivv)
    if(allocated(grid%whrmu))  deallocate(grid%whrmu)
    if(allocated(grid%whrmv))  deallocate(grid%whrmv)
    if(allocated(grid%wndspd)) deallocate(grid%wndspd)
    if(allocated(grid%wrotu))  deallocate(grid%wrotu)
    if(allocated(grid%wrotv))  deallocate(grid%wrotv)
    if(allocated(grid%wvmxrt)) deallocate(grid%wvmxrt)
    if(allocated(grid%z))      deallocate(grid%z)
    if(allocated(grid%cori))   deallocate(grid%cori)
    if(allocated(grid%dy))     deallocate(grid%dy)
    if(allocated(grid%dx))     deallocate(grid%dx)
    if(allocated(grid%land))   deallocate(grid%land)
    if(allocated(grid%mapfac)) deallocate(grid%mapfac)
    if(allocated(grid%orog))   deallocate(grid%orog)
    if(allocated(grid%pmsl))   deallocate(grid%pmsl)
    if(allocated(grid%psfc))   deallocate(grid%psfc)
    if(allocated(grid%tsfc))   deallocate(grid%tsfc)
    if(allocated(grid%u10m))   deallocate(grid%u10m)
    if(allocated(grid%v10m))   deallocate(grid%v10m)
    if(allocated(grid%xlat))   deallocate(grid%xlat)
    if(allocated(grid%xlon))   deallocate(grid%xlon)

    !=====================================================================

  end subroutine finalize_meteo_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_moisture_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! FORTRAN moisture_struct variable.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN moisture_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_moisture_struct(grid)

    ! Define variables passed to routine

    type(moisture_struct)                                               :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%p))       deallocate(grid%p)
    if(allocated(grid%q))       deallocate(grid%q)
    if(allocated(grid%rh))      deallocate(grid%rh)
    if(allocated(grid%svp))     deallocate(grid%svp)
    if(allocated(grid%swvmxrt)) deallocate(grid%swvmxrt)
    if(allocated(grid%t))       deallocate(grid%t)
    if(allocated(grid%vp))      deallocate(grid%vp)
    if(allocated(grid%wvmxrt))  deallocate(grid%wvmxrt)

    !=====================================================================

  end subroutine finalize_moisture_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_mpi_taskgrid_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! mpi_taskgrid_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN mpi_taskgrid_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_mpi_taskgrid_struct(grid)

    ! Define variables passed to routine

    type(mpi_taskgrid_struct)                                           :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%level))     deallocate(grid%level)
    if(allocated(grid%begin))     deallocate(grid%begin)
    if(allocated(grid%end))       deallocate(grid%end)
    if(allocated(grid%procnlevs)) deallocate(grid%procnlevs)

    !=====================================================================

  end subroutine finalize_mpi_taskgrid_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_poisson_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! poisson_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN poisson_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_poisson_struct(grid)

    ! Define variables passed to routine

    type(poisson_struct)                                                :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%rhs))   deallocate(grid%rhs)
    if(allocated(grid%sol))   deallocate(grid%sol)
    if(allocated(grid%bd_ax)) deallocate(grid%bd_ax)
    if(allocated(grid%bd_bx)) deallocate(grid%bd_bx)
    if(allocated(grid%bd_ay)) deallocate(grid%bd_ay)
    if(allocated(grid%bd_by)) deallocate(grid%bd_by)

    !=====================================================================

  end subroutine finalize_poisson_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_recenter_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! recenter_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN recenter_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_recenter_struct(grid)

    ! Define variables passed to routine

    type(recenter_struct)                                               :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%angle))  deallocate(grid%angle)
    if(allocated(grid%lat))    deallocate(grid%lat)
    if(allocated(grid%lon))    deallocate(grid%lon)
    if(allocated(grid%radius)) deallocate(grid%radius)
    if(allocated(grid%var))    deallocate(grid%var)
    if(allocated(grid%idx))    deallocate(grid%idx)
    if(allocated(grid%jdx))    deallocate(grid%jdx)
    
    !=====================================================================

  end subroutine finalize_recenter_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_slint_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! slint_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN slint_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_slint_struct(grid)

    ! Define variables passed to routine

    type(slint_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%coeffs))  deallocate(grid%coeffs)
    if(allocated(grid%nn))      deallocate(grid%nn)
    if(allocated(grid%var))     deallocate(grid%var)
    if(allocated(grid%dst_lon)) deallocate(grid%dst_lon)
    if(allocated(grid%dst_lat)) deallocate(grid%dst_lat)
    if(allocated(grid%src_lon)) deallocate(grid%src_lon)
    if(allocated(grid%src_lat)) deallocate(grid%src_lat)

    !=====================================================================

  end subroutine finalize_slint_struct
  
  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_smthr9pt_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! smthr9pt_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN smthr9pt_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_smthr9pt_struct(grid)

    ! Define variables passed to routine

    type(smthr9pt_struct)                                               :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%filter)) deallocate(grid%filter)
    if(allocated(grid%var))    deallocate(grid%var)

    !=====================================================================
    
  end subroutine finalize_smthr9pt_struct
  
  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_spline_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! spline_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN spline_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_spline_struct(grid)

    ! Define variables passed to routine

    type(spline_struct)                                                 :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%xa)) deallocate(grid%xa)
    if(allocated(grid%ya)) deallocate(grid%ya)

    !=====================================================================
    
  end subroutine finalize_spline_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_subregion_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! subregion_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN subregion_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_subregion_struct(grid)

    ! Define variables passed to routine

    type(subregion_struct)                                              :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%idx)) deallocate(grid%idx)

    !=====================================================================
    
  end subroutine finalize_subregion_struct  

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_tccps_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! tccps_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN tccps_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_tccps_struct(grid)

    ! Define variables passed routine

    type(tccps_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%z))      deallocate(grid%z)
    if(allocated(grid%p))      deallocate(grid%p)
    if(allocated(grid%lats))   deallocate(grid%lats)
    if(allocated(grid%lons))   deallocate(grid%lons)
    if(allocated(grid%radius)) deallocate(grid%radius)

    !=====================================================================

  end subroutine finalize_tccps_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_tcenv_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! tcenv_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN tcenv_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_tcenv_struct(grid)

    ! Define variables passed routine

    type(tcenv_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%lats))           deallocate(grid%lats)
    if(allocated(grid%lons))           deallocate(grid%lons)
    if(allocated(grid%tcid))           deallocate(grid%tcid)
    if(allocated(grid%tclat))          deallocate(grid%tclat)
    if(allocated(grid%tclon))          deallocate(grid%tclon)
    if(allocated(grid%u850_200))       deallocate(grid%u850_200)
    if(allocated(grid%u850_200_shear)) deallocate(grid%u850_200_shear)
    if(allocated(grid%u925))           deallocate(grid%u925)
    if(allocated(grid%u935))           deallocate(grid%u935)
    if(allocated(grid%u945))           deallocate(grid%u945)    
    if(allocated(grid%u955))           deallocate(grid%u955)
    if(allocated(grid%u965))           deallocate(grid%u965)
    if(allocated(grid%u975))           deallocate(grid%u975)
    if(allocated(grid%u985))           deallocate(grid%u985)
    if(allocated(grid%u995))           deallocate(grid%u995)
    if(allocated(grid%u1005))          deallocate(grid%u1005)
    if(allocated(grid%v850_200))       deallocate(grid%v850_200)
    if(allocated(grid%v850_200_shear)) deallocate(grid%v850_200_shear)
    if(allocated(grid%v925))           deallocate(grid%v925)
    if(allocated(grid%v935))           deallocate(grid%v935)
    if(allocated(grid%v945))           deallocate(grid%v945)    
    if(allocated(grid%v955))           deallocate(grid%v955)
    if(allocated(grid%v965))           deallocate(grid%v965)
    if(allocated(grid%v975))           deallocate(grid%v975)
    if(allocated(grid%v985))           deallocate(grid%v985)
    if(allocated(grid%v995))           deallocate(grid%v995)
    if(allocated(grid%v1005))          deallocate(grid%v1005)
    
    !=====================================================================

  end subroutine finalize_tcenv_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_tcincr_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! FORTRAN tcincr_struct variable.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN tcincr_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_tcincr_struct(grid)

    ! Define variables passed to routine

    type(tcincr_struct)                                                 :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%q))    deallocate(grid%q)
    if(allocated(grid%t))    deallocate(grid%t)
    if(allocated(grid%u))    deallocate(grid%u)
    if(allocated(grid%v))    deallocate(grid%v)
    if(allocated(grid%z))    deallocate(grid%z)
    if(allocated(grid%pmsl)) deallocate(grid%pmsl)

    !=====================================================================

  end subroutine finalize_tcincr_struct  

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_tcmpi_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! tcmpi_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN tcmpi_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_tcmpi_struct(grid)

    ! Define variables passed to routine

    type(tcmpi_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%tcid))  deallocate(grid%tcid)
    if(allocated(grid%mxrt))  deallocate(grid%mxrt)
    if(allocated(grid%p))     deallocate(grid%p)
    if(allocated(grid%t))     deallocate(grid%t)
    if(allocated(grid%lats))  deallocate(grid%lats)
    if(allocated(grid%lons))  deallocate(grid%lons)
    if(allocated(grid%pmin))  deallocate(grid%pmin)
    if(allocated(grid%pmsl))  deallocate(grid%pmsl)
    if(allocated(grid%sst))   deallocate(grid%sst)
    if(allocated(grid%vmax))  deallocate(grid%vmax)
    if(allocated(grid%tclat)) deallocate(grid%tclat)
    if(allocated(grid%tclon)) deallocate(grid%tclon)
    if(allocated(grid%ifl))   deallocate(grid%ifl)

    !=====================================================================

  end subroutine finalize_tcmpi_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_tcmsi_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! tcmsi_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN tcmsi_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_tcmsi_struct(grid)

    ! Define variables passed to routine

    type(tcmsi_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%wndvar)) deallocate(grid%wndvar)
    if(allocated(grid%lats))   deallocate(grid%lats)
    if(allocated(grid%lons))   deallocate(grid%lons)
    if(allocated(grid%radius)) deallocate(grid%radius)
    if(allocated(grid%var))    deallocate(grid%var)
    if(allocated(grid%w10m))   deallocate(grid%w10m)

    !=====================================================================

  end subroutine finalize_tcmsi_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_vargrid_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! vargrid_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN vargrid_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_vargrid_struct(grid)

    ! Define variables passed to routine

    type(vargrid_struct)                                                :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%var)) deallocate(grid%var)

    !=====================================================================

  end subroutine finalize_vargrid_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_varinfo_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! varinfo_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN varinfo_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_varinfo_struct(grid)

    ! Define variables passed to routine

    type(varinfo_struct)                                                :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%varattrs))  deallocate(grid%varattrs)
    if(allocated(grid%varname))   deallocate(grid%varname)
    if(allocated(grid%vartype))   deallocate(grid%vartype)
    if(allocated(grid%dimname))   deallocate(grid%dimname)
    if(allocated(grid%dimval))    deallocate(grid%dimval)
    if(allocated(grid%dimid))     deallocate(grid%dimid)
    if(allocated(grid%vardimid))  deallocate(grid%vardimid)
    if(allocated(grid%varid))     deallocate(grid%varid)
    if(allocated(grid%varndims))  deallocate(grid%varndims)
    if(allocated(grid%varnattrs)) deallocate(grid%varnattrs)

    !=====================================================================

  end subroutine finalize_varinfo_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_vertgrid_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! vertgrid_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN vertgrid_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_vertgrid_struct(grid)

    ! Define variables passed to routine

    type(vertgrid_struct)                                               :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%dst_coord)) deallocate(grid%dst_coord)
    if(allocated(grid%dst_prs))   deallocate(grid%dst_prs)
    if(allocated(grid%dst_var))   deallocate(grid%dst_var)
    if(allocated(grid%src_coord)) deallocate(grid%src_coord)
    if(allocated(grid%src_prs))   deallocate(grid%src_prs)
    if(allocated(grid%src_var))   deallocate(grid%src_var)

    !=====================================================================

  end subroutine finalize_vertgrid_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_wnd2d_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! wnd2d_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN wnd2d_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_wnd2d_struct(grid)

    ! Define variables passed to routine

    type(wnd2d_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%wnvar)) deallocate(grid%wnvar)
    if(allocated(grid%var))   deallocate(grid%var)

    !=====================================================================

  end subroutine finalize_wnd2d_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_bkgrdfltr_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! bkgrdfltr_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN bkgrdfltr_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN bkgrdfltr_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_bkgrdfltr_struct(grid)

    ! Define variables passed to routine

    type(bkgrdfltr_struct)                                              :: grid

    !=====================================================================

    ! Define local variables

    if(grid%ncoords .eq. 0) grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%divg))                                         &
         & allocate(grid%divg(grid%ncoords,grid%nz))
    if(.not. allocated(grid%filter))                                       &
         & allocate(grid%filter(grid%ncoords,grid%nz))
    if(.not. allocated(grid%mask))                                         &
         & allocate(grid%mask(grid%ncoords,grid%nz))
    if(.not. allocated(grid%q))                                            &
         & allocate(grid%q(grid%ncoords,grid%nz))
    if(.not. allocated(grid%radius))                                       &
         & allocate(grid%radius(grid%ncoords,grid%nz))
    if(.not. allocated(grid%t))                                            &
         & allocate(grid%t(grid%ncoords,grid%nz))
    if(.not. allocated(grid%vort))                                         &
         & allocate(grid%vort(grid%ncoords,grid%nz))
    if(.not. allocated(grid%whrmu))                                        &
         & allocate(grid%whrmu(grid%ncoords,grid%nz))
    if(.not. allocated(grid%whrmv))                                        &
         & allocate(grid%whrmv(grid%ncoords,grid%nz))
    if(.not. allocated(grid%z))                                            &
         & allocate(grid%z(grid%ncoords,grid%nz))
    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%ncoords))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%ncoords))
    if(.not. allocated(grid%pmsl))                                         &
         & allocate(grid%pmsl(grid%ncoords))
    if(.not. allocated(grid%cidx))                                         &
         & allocate(grid%cidx(grid%nz))
    if(.not. allocated(grid%minx))                                         &
         & allocate(grid%minx(grid%nz))
    if(.not. allocated(grid%maxx))                                         &
         & allocate(grid%maxx(grid%nz))
    if(.not. allocated(grid%miny))                                         &
         & allocate(grid%miny(grid%nz))
    if(.not. allocated(grid%maxy))                                         &
         & allocate(grid%maxy(grid%nz))

    !=====================================================================

  end subroutine initialize_bkgrdfltr_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_fft2d_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! fft2d_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN fft2d_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN fft2d_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_fft2d_struct(grid)

    ! Define variables passed to routine

    type(fft2d_struct)                                                  :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%out)) allocate(grid%out(grid%nx,grid%ny))
    if(.not. allocated(grid%in))  allocate(grid%in(grid%nx,grid%ny))

    !=====================================================================

  end subroutine initialize_fft2d_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_filter_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays and initializes
  ! all variables within the filter_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN filter_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN filter_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_filter_struct(grid)

    ! Define variables passed to routine

    type(filter_struct)                                                 :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%angle))                                        &
         & allocate(grid%angle(grid%ncoords,grid%nz))
    if(.not. allocated(grid%kfltr))                                        &
         & allocate(grid%kfltr(grid%ncoords,grid%nz))
    if(.not. allocated(grid%radius))                                       &
         & allocate(grid%radius(grid%ncoords,grid%nz))
    if(.not. allocated(grid%tfltr))                                        &
         & allocate(grid%tfltr(grid%ncoords,grid%nz))
    if(.not. allocated(grid%tngwnd))                                       &
         & allocate(grid%tngwnd(grid%ncoords,grid%nz))
    if(.not. allocated(grid%clat))                                         &
         & allocate(grid%clat(grid%nz))
    if(.not. allocated(grid%clon))                                         &
         & allocate(grid%clon(grid%nz))
    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%ncoords))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%ncoords))
    if(.not. allocated(grid%plev))                                         &
         & allocate(grid%plev(grid%nz))
    if(.not. allocated(grid%cidx))                                         &
         & allocate(grid%cidx(grid%nz))
    if(.not. allocated(grid%kxmin))                                        &
         & allocate(grid%kxmin(grid%nz))
    if(.not. allocated(grid%kxmax))                                        &
         & allocate(grid%kxmax(grid%nz))
    if(.not. allocated(grid%kymin))                                        &
         & allocate(grid%kymin(grid%nz))
    if(.not. allocated(grid%kymax))                                        &
         & allocate(grid%kymax(grid%nz))
    if(.not. allocated(grid%txmin))                                        &
         & allocate(grid%txmin(grid%nz))
    if(.not. allocated(grid%txmax))                                        &
         & allocate(grid%txmax(grid%nz))
    if(.not. allocated(grid%tymin))                                        &
         & allocate(grid%tymin(grid%nz))
    if(.not. allocated(grid%tymax))                                        &
         & allocate(grid%tymax(grid%nz))

    ! Define local variables

    grid%tngwnd = 0.0

    !=====================================================================

  end subroutine initialize_filter_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_grib_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays and initializes
  ! all variables within the grib_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grib_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grib_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_grib_struct(grid)

    ! Define variables passed to routine

    type(grib_struct)                                                   :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%ncoords,grid%nz))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%ncoords,grid%nz))
    if(.not. allocated(grid%values))                                       &
         & allocate(grid%values(grid%ncoords,grid%nz))
    if(.not. allocated(grid%levs))                                         &
         & allocate(grid%levs(grid%nz))

    !=====================================================================

  end subroutine initialize_grib_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_grid_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! grid_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_grid_struct(grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid

    !=====================================================================

    ! Define local variables

    if(grid%ncoords .le. 0) grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%angle))                                        &
         & allocate(grid%angle(grid%ncoords))
    if(.not. allocated(grid%cori))                                         &
         & allocate(grid%cori(grid%ncoords))
    if(.not. allocated(grid%dx))                                           &
         & allocate(grid%dx(grid%ncoords))
    if(.not. allocated(grid%dy))                                           &
         & allocate(grid%dy(grid%ncoords))
    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%ncoords))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%ncoords))
    if(.not. allocated(grid%mapfac))                                       &
         & allocate(grid%mapfac(grid%ncoords))
    if(.not. allocated(grid%radius))                                       &
         & allocate(grid%radius(grid%ncoords))
    if(.not. allocated(grid%var))                                          &
         & allocate(grid%var(grid%ncoords))
    if(.not. allocated(grid%idx))                                          &
         & allocate(grid%idx(grid%ncoords))

    !=====================================================================

  end subroutine initialize_grid_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_kdtree_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! kdtree_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN kdtree_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN kdtree_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_kdtree_struct(grid)

    ! Define variables passed to routine

    type(kdtree_struct)                                                 :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%r2dist))                                       &
         & allocate(grid%r2dist(grid%ncoords,grid%nn))
    if(.not. allocated(grid%idx))                                          &
         & allocate(grid%idx(grid%ncoords,grid%nn))

    !=====================================================================

  end subroutine initialize_kdtree_struct

  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_linterp_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! linterp_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN linterp_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN linterp_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_linterp_struct(grid)

    ! Define variables passed to routine

    type(linterp_struct)                                                :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%xa)) allocate(grid%xa(grid%n))
    if(.not. allocated(grid%ya)) allocate(grid%ya(grid%n))

    !=====================================================================
    
  end subroutine initialize_linterp_struct

  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_lyrmn_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! lyrmn_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN lyrmn_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN lyrmn_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_lyrmn_struct(grid)

    ! Define variables passed to routine

    type(lyrmn_struct)                                                  :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%p))                                            &
         & allocate(grid%p(grid%ncoords,grid%nz))
    if(.not. allocated(grid%var))                                          &
         & allocate(grid%var(grid%ncoords,grid%nz))
    if(.not. allocated(grid%varmean))                                      &
         & allocate(grid%varmean(grid%ncoords))
    
    !=====================================================================
    
  end subroutine initialize_lyrmn_struct
    
  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_meteo_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! meteo_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable.

  !-----------------------------------------------------------------------

  subroutine initialize_meteo_struct(grid)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%chi))                                          &
         & allocate(grid%chi(grid%ncoords,grid%nz))
    if(.not. allocated(grid%divg))                                         &
         & allocate(grid%divg(grid%ncoords,grid%nz))
    if(.not. allocated(grid%p))                                            &
         & allocate(grid%p(grid%ncoords,grid%nz))
    if(.not. allocated(grid%pott))                                         &
         & allocate(grid%pott(grid%ncoords,grid%nz))
    if(.not. allocated(grid%psi))                                          &
         & allocate(grid%psi(grid%ncoords,grid%nz))
    if(.not. allocated(grid%pv))                                           &
         & allocate(grid%pv(grid%ncoords,grid%nz))
    if(.not. allocated(grid%q))                                            &
         & allocate(grid%q(grid%ncoords,grid%nz))
    if(.not. allocated(grid%rh))                                           &
         & allocate(grid%rh(grid%ncoords,grid%nz))
    if(.not. allocated(grid%t))                                            &
         & allocate(grid%t(grid%ncoords,grid%nz))
    if(.not. allocated(grid%tngwnd))                                       &
         & allocate(grid%tngwnd(grid%ncoords,grid%nz))
    if(.not. allocated(grid%u))                                            &
         & allocate(grid%u(grid%ncoords,grid%nz))
    if(.not. allocated(grid%v))                                            &
         & allocate(grid%v(grid%ncoords,grid%nz))
    if(.not. allocated(grid%virtt))                                        &
         & allocate(grid%virtt(grid%ncoords,grid%nz))
    if(.not. allocated(grid%vort))                                         &
         & allocate(grid%vort(grid%ncoords,grid%nz))
    if(.not. allocated(grid%wdivu))                                        &
         & allocate(grid%wdivu(grid%ncoords,grid%nz))
    if(.not. allocated(grid%wdivv))                                        &
         & allocate(grid%wdivv(grid%ncoords,grid%nz))
    if(.not. allocated(grid%whrmu))                                        &
         & allocate(grid%whrmu(grid%ncoords,grid%nz))
    if(.not. allocated(grid%whrmv))                                        &
         & allocate(grid%whrmv(grid%ncoords,grid%nz))
    if(.not. allocated(grid%wndspd))                                       &
         & allocate(grid%wndspd(grid%ncoords,grid%nz))
    if(.not. allocated(grid%wrotu))                                        &
         & allocate(grid%wrotu(grid%ncoords,grid%nz))
    if(.not. allocated(grid%wrotv))                                        &
         & allocate(grid%wrotv(grid%ncoords,grid%nz))
    if(.not. allocated(grid%wvmxrt))                                       &
         & allocate(grid%wvmxrt(grid%ncoords,grid%nz))
    if(.not. allocated(grid%z))                                            &
         & allocate(grid%z(grid%ncoords,grid%nz))
    if(.not. allocated(grid%cori))                                         &
         & allocate(grid%cori(grid%ncoords))
    if(.not. allocated(grid%dx))                                           &
         & allocate(grid%dx(grid%ncoords))
    if(.not. allocated(grid%dy))                                           &
         & allocate(grid%dy(grid%ncoords))
    if(.not. allocated(grid%land))                                         &
         & allocate(grid%land(grid%ncoords))
    if(.not. allocated(grid%mapfac))                                       &
         & allocate(grid%mapfac(grid%ncoords))
    if(.not. allocated(grid%orog))                                         &
         & allocate(grid%orog(grid%ncoords))
    if(.not. allocated(grid%pmsl))                                         &
         & allocate(grid%pmsl(grid%ncoords))
    if(.not. allocated(grid%psfc))                                         &
         & allocate(grid%psfc(grid%ncoords))
    if(.not. allocated(grid%tsfc))                                         &
         & allocate(grid%tsfc(grid%ncoords))
    if(.not. allocated(grid%u10m))                                         &
         & allocate(grid%u10m(grid%ncoords))
    if(.not. allocated(grid%v10m))                                         &
         & allocate(grid%v10m(grid%ncoords))
    if(.not. allocated(grid%xlat))                                         &
         & allocate(grid%xlat(grid%ncoords))
    if(.not. allocated(grid%xlon))                                         &
         & allocate(grid%xlon(grid%ncoords))

    ! Define local variables

    grid%chi    = spval
    grid%cori   = spval
    grid%divg   = spval
    grid%pott   = spval
    grid%psi    = spval
    grid%pv     = spval
    grid%q      = spval
    grid%rh     = spval
    grid%virtt  = spval
    grid%vort   = spval
    grid%wdivu  = spval
    grid%wdivv  = spval
    grid%whrmu  = spval
    grid%whrmv  = spval
    grid%wrotu  = spval
    grid%wrotv  = spval
    grid%wvmxrt = spval

    !=====================================================================

  end subroutine initialize_meteo_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_moisture_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory and initializes (if applicable)
  ! all arrays within the FORTRAN moisture_struct variable.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN moisture_struct variable.
  
  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN moisture_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_moisture_struct(grid)

    ! Define variables passed to routine

    type(moisture_struct)                                               :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%p))                                            &
         & allocate(grid%p(grid%ncoords,grid%nz))
    if(.not. allocated(grid%q))                                            &
         & allocate(grid%q(grid%ncoords,grid%nz))
    if(.not. allocated(grid%rh))                                           &
         & allocate(grid%rh(grid%ncoords,grid%nz))
    if(.not. allocated(grid%svp))                                          &
         & allocate(grid%svp(grid%ncoords,grid%nz))
    if(.not. allocated(grid%swvmxrt))                                      &
         & allocate(grid%swvmxrt(grid%ncoords,grid%nz))
    if(.not. allocated(grid%t))                                            &
         & allocate(grid%t(grid%ncoords,grid%nz))
    if(.not. allocated(grid%vp))                                           &
         & allocate(grid%vp(grid%ncoords,grid%nz))
    if(.not. allocated(grid%wvmxrt))                                       &
         & allocate(grid%wvmxrt(grid%ncoords,grid%nz))

    ! Define local variables

    grid%q       = spval
    grid%rh      = spval
    grid%vp      = spval
    grid%wvmxrt  = spval

    !=====================================================================

  end subroutine initialize_moisture_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_mpi_taskgrid_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! mpi_taskgrid_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN mpi_taskgrid_struct variable.

  !-----------------------------------------------------------------------

  subroutine initialize_mpi_taskgrid_struct(grid)

    ! Define variables passed to routine

    type(mpi_taskgrid_struct)                                           :: grid
    
    !=====================================================================

    ! Define local variables
    
    mpi_taskgrid%nprocs = mpi_nprocs

    ! Allocate memory for local variables

    if(.not. allocated(grid%level))                                        &
         & allocate(grid%level(grid%nprocs,grid%nlevs))
    if(.not. allocated(grid%begin))                                        &
         & allocate(grid%begin(grid%nprocs))
    if(.not. allocated(grid%end))                                          &
         & allocate(grid%end(grid%nprocs))
    if(.not. allocated(grid%procnlevs))                                    &
         & allocate(grid%procnlevs(grid%nprocs))

    !=====================================================================

  end subroutine initialize_mpi_taskgrid_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_poisson_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! poisson_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN poisson_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN poisson_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_poisson_struct(grid)

    ! Define variables passed to routine

    type(poisson_struct)                                                :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%rhs))                                          &
         & allocate(grid%rhs((grid%nx + 1),(grid%ny + 1)))
    if(.not. allocated(grid%sol))                                          &
         & allocate(grid%sol((grid%nx + 1),(grid%ny + 1)))
    if(.not. allocated(grid%bd_ax))                                        &
         & allocate(grid%bd_ax((grid%ny + 1)))
    if(.not. allocated(grid%bd_bx))                                        &
         & allocate(grid%bd_bx((grid%ny + 1)))
    if(.not. allocated(grid%bd_ay))                                        &
         & allocate(grid%bd_ay((grid%nx + 1)))
    if(.not. allocated(grid%bd_by))                                        &
         & allocate(grid%bd_by((grid%nx + 1)))

    !=====================================================================

  end subroutine initialize_poisson_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_recenter_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! recenter_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN recenter_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN recenter_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_recenter_struct(grid)

    ! Define variables passed to routine

    type(recenter_struct)                                               :: grid

    ! Define variables computed within routine

    integer                                                             :: count

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%angle))  allocate(grid%angle(grid%ncoords))
    if(.not. allocated(grid%lat))    allocate(grid%lat(grid%ncoords))
    if(.not. allocated(grid%lon))    allocate(grid%lon(grid%ncoords))
    if(.not. allocated(grid%radius)) allocate(grid%radius(grid%ncoords))
    if(.not. allocated(grid%var))    allocate(grid%var(grid%ncoords))
    if(.not. allocated(grid%idx))    allocate(grid%idx(grid%ncoords))
    if(.not. allocated(grid%jdx))    allocate(grid%jdx(grid%ncoords))

    ! Define local variables

    count = 0

    ! Loop through local variable

    do j = 1, grid%ny

       ! Loop through local variable

       do i = 1, grid%nx

          ! Define local variables

          count           = count + 1
          grid%idx(count) = i
          grid%jdx(count) = j

       end do ! do i = 1, grid%nx

    end do ! do j = 1, grid%ny

    !=====================================================================
    
  end subroutine initialize_recenter_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_slint_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! slint_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN slint_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN slint_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_slint_struct(grid)

    ! Define variables passed to routine

    type(slint_struct)                                                  :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoeffs = 3

    ! Allocate memory for local variables

    if(.not. allocated(grid%coeffs))                                       &
         & allocate(grid%coeffs(grid%ncoeffs,grid%dst_ncoords))
    if(.not. allocated(grid%nn))                                           &
         & allocate(grid%nn(grid%ncoeffs,grid%dst_ncoords))
    if(.not. allocated(grid%var))                                          &
         & allocate(grid%var(grid%dst_ncoords))
    if(.not. allocated(grid%dst_lon))                                      &
         & allocate(grid%dst_lon(grid%dst_ncoords))
    if(.not. allocated(grid%dst_lat))                                      &
         & allocate(grid%dst_lat(grid%dst_ncoords))
    if(.not. allocated(grid%src_lon))                                      &
         & allocate(grid%src_lon(grid%src_ncoords))
    if(.not. allocated(grid%src_lat))                                      &
         & allocate(grid%src_lat(grid%src_ncoords))

    !=====================================================================
    
  end subroutine initialize_slint_struct

  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_smthr9pt_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! smthr9pt_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN smthr9pt_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN smthr9pt_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_smthr9pt_struct(grid)

    ! Define variables passed to routine

    type(smthr9pt_struct)                                               :: grid

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(grid%ncoords .eq. 0) grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%filter)) allocate(grid%filter(grid%ncoords))
    if(.not. allocated(grid%var))    allocate(grid%var(grid%ncoords))

    !=====================================================================
    
  end subroutine initialize_smthr9pt_struct

  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_spline_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! spline_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN spline_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN spline_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_spline_struct(grid)

    ! Define variables passed to routine

    type(spline_struct)                                                 :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%xa)) allocate(grid%xa(grid%n))
    if(.not. allocated(grid%ya)) allocate(grid%ya(grid%n))

    !=====================================================================
    
  end subroutine initialize_spline_struct

  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_subregion_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! subregion_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN subregion_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN subregion_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_subregion_struct(grid)

    ! Define variables passed to routine

    type(subregion_struct)                                              :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%idx)) allocate(grid%idx(grid%ncoords))

    !=====================================================================
    
  end subroutine initialize_subregion_struct  

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_tccps_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! tccps_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN tccps_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN tccps_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_tccps_struct(grid)

    ! Define variables passed to routine

    type(tccps_struct)                                                  :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%z))                                            &
         & allocate(grid%z(grid%ncoords,grid%nz))
    if(.not. allocated(grid%p))                                            &
         & allocate(grid%p(grid%ncoords,grid%nz))
    if(.not. allocated(grid%lats))                                         &
         & allocate(grid%lats(grid%ncoords))
    if(.not. allocated(grid%lons))                                         &
         & allocate(grid%lons(grid%ncoords))
    if(.not. allocated(grid%radius))                                       &
         & allocate(grid%radius(grid%ncoords))
    
    !=====================================================================

  end subroutine initialize_tccps_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_tcenv_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! tcenv_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN tcenv_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN tcenv_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_tcenv_struct(grid)

    ! Define variables passed to routine

    type(tcenv_struct)                                                  :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%lats))                                         &
         & allocate(grid%lats(grid%ncoords))
    if(.not. allocated(grid%lons))                                         &
         & allocate(grid%lons(grid%ncoords))
    if(.not. allocated(grid%tcid))                                         &
         & allocate(grid%tcid(ntcs))
    if(.not. allocated(grid%tclat))                                        &
         & allocate(grid%tclat(ntcs))
    if(.not. allocated(grid%tclon))                                        &
         & allocate(grid%tclon(ntcs))    
    if(.not. allocated(grid%u850_200))                                     &
         & allocate(grid%u850_200(grid%ncoords))
    if(.not. allocated(grid%u850_200_shear))                               &
         & allocate(grid%u850_200_shear(grid%ncoords))
    if(.not. allocated(grid%u925))                                         &
         & allocate(grid%u925(grid%ncoords))
    if(.not. allocated(grid%u935))                                         &
         & allocate(grid%u935(grid%ncoords))
    if(.not. allocated(grid%u945))                                         &
         & allocate(grid%u945(grid%ncoords))
    if(.not. allocated(grid%u955))                                         &
         & allocate(grid%u955(grid%ncoords))
    if(.not. allocated(grid%u965))                                         &
         & allocate(grid%u965(grid%ncoords))
    if(.not. allocated(grid%u975))                                         &
         & allocate(grid%u975(grid%ncoords))
    if(.not. allocated(grid%u985))                                         &
         & allocate(grid%u985(grid%ncoords))
    if(.not. allocated(grid%u995))                                         &
         & allocate(grid%u995(grid%ncoords))
    if(.not. allocated(grid%u1005))                                        &
         & allocate(grid%u1005(grid%ncoords))
    if(.not. allocated(grid%v850_200))                                     &
         & allocate(grid%v850_200(grid%ncoords))    
    if(.not. allocated(grid%v850_200_shear))                               &
         & allocate(grid%v850_200_shear(grid%ncoords))
    if(.not. allocated(grid%v925))                                         &
         & allocate(grid%v925(grid%ncoords))
    if(.not. allocated(grid%v935))                                         &
         & allocate(grid%v935(grid%ncoords))
    if(.not. allocated(grid%v945))                                         &
         & allocate(grid%v945(grid%ncoords))
    if(.not. allocated(grid%v955))                                         &
         & allocate(grid%v955(grid%ncoords))
    if(.not. allocated(grid%v965))                                         &
         & allocate(grid%v965(grid%ncoords))
    if(.not. allocated(grid%v975))                                         &
         & allocate(grid%v975(grid%ncoords))
    if(.not. allocated(grid%v985))                                         &
         & allocate(grid%v985(grid%ncoords))
    if(.not. allocated(grid%v995))                                         &
         & allocate(grid%v995(grid%ncoords))
    if(.not. allocated(grid%v1005))                                        &
         & allocate(grid%v1005(grid%ncoords))
    
    !=====================================================================

  end subroutine initialize_tcenv_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_tcincr_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory and initializes (if applicable)
  ! all arrays within the FORTRAN tcincr_struct variable.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN tcincr_struct variable.
  
  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN tcincr_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_tcincr_struct(grid)

    ! Define variables passed to routine

    type(tcincr_struct)                                                 :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)
    
    ! Aallocate memory for local variables

    if(.not. allocated(grid%q))    allocate(grid%q(grid%ncoords,grid%nz))
    if(.not. allocated(grid%t))    allocate(grid%t(grid%ncoords,grid%nz))
    if(.not. allocated(grid%u))    allocate(grid%u(grid%ncoords,grid%nz))
    if(.not. allocated(grid%v))    allocate(grid%v(grid%ncoords,grid%nz))
    if(.not. allocated(grid%z))    allocate(grid%z(grid%ncoords,grid%nz))
    if(.not. allocated(grid%pmsl)) allocate(grid%pmsl(grid%ncoords))

    ! Define local variables

    grid%q    = 0.0
    grid%t    = 0.0
    grid%u    = 0.0
    grid%v    = 0.0
    grid%z    = 0.0
    grid%pmsl = 0.0

    !=====================================================================

  end subroutine initialize_tcincr_struct  

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_tcmpi_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocated memory for all arrays within the
  ! tcmpi_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN tcmpi_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN tcmpi_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_tcmpi_struct(grid)

   ! Define variables passed to routine

    type(tcmpi_struct)                                                  :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%tcid))                                         &
         & allocate(grid%tcid(ntcs))
    if(.not. allocated(grid%mxrt))                                         &
         & allocate(grid%mxrt(grid%ncoords,grid%nz))
    if(.not. allocated(grid%p))                                            &
         & allocate(grid%p(grid%ncoords,grid%nz))
    if(.not. allocated(grid%t))                                            &
         & allocate(grid%t(grid%ncoords,grid%nz))
    if(.not. allocated(grid%lats))                                         &
         & allocate(grid%lats(grid%ncoords))
    if(.not. allocated(grid%lons))                                         &
         & allocate(grid%lons(grid%ncoords))
    if(.not. allocated(grid%pmin))                                         &
         & allocate(grid%pmin(grid%ncoords))
    if(.not. allocated(grid%pmsl))                                         &
         & allocate(grid%pmsl(grid%ncoords))
    if(.not. allocated(grid%sst))                                          &
         & allocate(grid%sst(grid%ncoords))
    if(.not. allocated(grid%vmax))                                         &
         & allocate(grid%vmax(grid%ncoords))
    if(.not. allocated(grid%tclat))                                        &
         & allocate(grid%tclat(ntcs))
    if(.not. allocated(grid%tclon))                                        &
         & allocate(grid%tclon(ntcs))
    if(.not. allocated(grid%ifl))                                          &
         & allocate(grid%ifl(grid%ncoords))

    !=====================================================================

  end subroutine initialize_tcmpi_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_tcmsi_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocated memory for all arrays within the
  ! tcmsi_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN tcmsi_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN tcmsi_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_tcmsi_struct(grid)

   ! Define variables passed to routine

    type(tcmsi_struct)                                                  :: grid

    !=====================================================================

    ! Allocate memory for local variables
    
    if(.not. allocated(grid%wndvar))                                       &
         & allocate(grid%wndvar(grid%nh,grid%nx,grid%ny))
    if(.not. allocated(grid%lats))                                         &
         & allocate(grid%lats(grid%nx,grid%ny))
    if(.not. allocated(grid%lons))                                         &
         & allocate(grid%lons(grid%nx,grid%ny))
    if(.not. allocated(grid%radius))                                       &
         & allocate(grid%radius(grid%nx,grid%ny))
    if(.not. allocated(grid%var))                                          &
         & allocate(grid%var(grid%nx,grid%ny))
    if(.not. allocated(grid%w10m))                                         &
         & allocate(grid%w10m(grid%nx,grid%ny))
    
    !=====================================================================

  end subroutine initialize_tcmsi_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_vargrid_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! vargrid_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN vargrid_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN vargrid_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_vargrid_struct(grid)

    ! Define variables passed to routine

    type(vargrid_struct)                                                :: grid

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(grid%nvals .eq. 0) then

       ! Define local variables

       grid%nvals = (grid%nx*grid%ny)

    end if ! if(grid%nvals .eq. 0)

    ! Allocate memory for local variables

    if(.not. allocated(grid%var)) allocate(grid%var(grid%nvals))
    
    !=====================================================================

  end subroutine initialize_vargrid_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_varinfo_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! varinfo_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN varinfo_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_varinfo_struct(grid)

    ! Define variables passed to routine

    type(varinfo_struct)                                                :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%varattrs))                                     &
         & allocate(grid%varattrs(grid%nvars,grid%nattrs,2))
    if(.not. allocated(grid%vardimid))                                     &
         & allocate(grid%vardimid(grid%nvars,grid%ndims))
    if(.not. allocated(grid%varname))                                      &
         & allocate(grid%varname(grid%nvars))
    if(.not. allocated(grid%vartype))                                      &
         & allocate(grid%vartype(grid%nvars))
    if(.not. allocated(grid%varndims))                                     &
         & allocate(grid%varndims(grid%nvars))
    if(.not. allocated(grid%varnattrs))                                    &
         & allocate(grid%varnattrs(grid%nvars))
    if(.not. allocated(grid%varid))                                        &
         & allocate(grid%varid(grid%nvars))
    if(.not. allocated(grid%dimval))                                       &
         & allocate(grid%dimval(grid%ndims))
    if(.not. allocated(grid%dimname))                                      &
         & allocate(grid%dimname(grid%ndims))
    if(.not. allocated(grid%dimid))                                        &
         & allocate(grid%dimid(grid%ndims))

    !=====================================================================

  end subroutine initialize_varinfo_struct
  
  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_vertgrid_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! vertgrid_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN vertgrid_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN vertgrid_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_vertgrid_struct(grid)

    ! Define variables passed to routine

    type(vertgrid_struct)                                               :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%dst_coord))                                    &
         & allocate(grid%dst_coord(grid%dst_nz))
    if(.not. allocated(grid%dst_prs))                                      &
         & allocate(grid%dst_prs(grid%dst_nz))
    if(.not. allocated(grid%dst_var))                                      &
         & allocate(grid%dst_var(grid%dst_nz))    
    if(.not. allocated(grid%src_coord))                                    &
         & allocate(grid%src_coord(grid%src_nz))
    if(.not. allocated(grid%src_prs))                                      &
         & allocate(grid%src_prs(grid%src_nz))
    if(.not. allocated(grid%src_var))                                      &
         & allocate(grid%src_var(grid%src_nz))

    !=====================================================================

  end subroutine initialize_vertgrid_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_wnd2d_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! wnd2d_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN wnd2d_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN wnd2d_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_wnd2d_struct(grid)

    ! Define variables passed to routine

    type(wnd2d_struct)                                                  :: grid

    !=====================================================================

    ! Define local variables

    grid%na = 360.0/grid%dangle
    grid%nr = grid%maxradius/grid%dradius
    grid%nh = ((grid%na/2) + 1)

    ! Allocate memory for local variables

    if(.not. allocated(grid%wnvar))                                        &
         & allocate(grid%wnvar(grid%nh,grid%nr,grid%na))
    if(.not. allocated(grid%var))                                          &
         & allocate(grid%var(grid%nx*grid%ny))

    !=====================================================================

  end subroutine initialize_wnd2d_struct
  
  !=======================================================================

end module variable_interface
