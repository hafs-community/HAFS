module variable_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: variable_interface
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

  ! Review the README, within the top-level directory, which provides
  ! relevant instructions and (any) references cited by algorithms
  ! within this software suite.

  !=======================================================================

  ! Define associated modules and subroutines

  use kinds_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: bufr_info_struct
  public :: bufr_mxlv
  public :: bufr_mxmn
  public :: bufr_spval
  public :: bufr_struct
  public :: bufrhdr_struct
  public :: fcstmdl_struct
  public :: fv3_struct
  public :: grid_struct
  public :: hsa_spval
  public :: hsa_struct
  public :: interp_p_struct
  public :: interp_spline_struct
  public :: kdtree_struct
  public :: meteo_struct
  public :: sonde_struct
  public :: spval
  public :: statgrid_struct
  public :: tcinfo_struct
  public :: tdr_struct
  public :: timeinfo_struct
  public :: variable_interface_cleanup_struct
  public :: variable_interface_setup_struct
  public :: varinfo_struct
  public :: vdm_spval
  public :: vdm_struct
  interface variable_interface_cleanup_struct
     module procedure finalize_bufr_struct
     module procedure finalize_bufrhdr_struct
     module procedure finalize_fcstmdl_struct
     module procedure finalize_fv3_struct
     module procedure finalize_grid_struct
     module procedure finalize_hsa_struct
     module procedure finalize_interp_p_struct
     module procedure finalize_interp_spline_struct
     module procedure finalize_kdtree_struct
     module procedure finalize_meteo_struct
     module procedure finalize_sonde_struct
     module procedure finalize_statgrid_struct
     module procedure finalize_tdr_struct
     module procedure finalize_varinfo_struct
     module procedure finalize_vdm_struct
  end interface variable_interface_cleanup_struct
  interface variable_interface_setup_struct
     module procedure initialize_bufr_struct
     module procedure initialize_bufrhdr_struct
     module procedure initialize_fcstmdl_struct
     module procedure initialize_fv3_struct
     module procedure initialize_grid_struct
     module procedure initialize_hsa_struct
     module procedure initialize_interp_p_struct
     module procedure initialize_interp_spline_struct
     module procedure initialize_kdtree_struct
     module procedure initialize_meteo_struct
     module procedure initialize_sonde_struct
     module procedure initialize_statgrid_struct
     module procedure initialize_tdr_struct
     module procedure initialize_varinfo_struct
     module procedure initialize_vdm_struct
  end interface variable_interface_setup_struct

  ! Define local variables

  real(r_double), parameter                                             :: bufr_spval = 10.e10
  real(r_kind),   parameter                                             :: hsa_spval  = -99.0
  real(r_kind),   parameter                                             :: spval      = huge(0.0)
  real(r_kind),   parameter                                             :: vdm_spval  = -9.e30
  integer,        parameter                                             :: bufr_mxlv  = 200
  integer,        parameter                                             :: bufr_mxmn  = 35
  type bufr_struct
     character(len=80)                                                  :: obstr
     character(len=80)                                                  :: hdstr
     character(len=80)                                                  :: qcstr
     character(len=80)                                                  :: oestr
     character(len=19)                                                  :: cdate
     character(len=8)                                                   :: subset
     real(r_double),            dimension(:,:),             allocatable :: obs
     real(r_double),            dimension(:,:),             allocatable :: qcf
     real(r_double),            dimension(:,:),             allocatable :: oer
     real(r_double),            dimension(:),               allocatable :: hdr
     integer                                                            :: idate
     integer                                                            :: mxmn
     integer                                                            :: mxlv
     integer                                                            :: nrecs
  end type bufr_struct            ! type bufr_struct
  type bufr_info_struct
     character(len=500)                                                 :: filename
     character(len=8)                                                   :: subset
     integer                                                            :: obs_type_mass
     integer                                                            :: obs_type_wind
  end type bufr_info_struct       ! type bufr_info_struct
   type bufrhdr_struct
     real(r_double),            dimension(:,:),             allocatable :: hdr
     integer                                                            :: mxmn
     integer                                                            :: nrecs     
  end type bufrhdr_struct         ! type bufrhdr_struct
  type fcstmdl_struct
     logical,                   dimension(:,:),             allocatable :: usage
     real(r_kind),              dimension(:,:),             allocatable :: p
     real(r_kind),              dimension(:,:),             allocatable :: q
     real(r_kind),              dimension(:,:),             allocatable :: t
     real(r_kind),              dimension(:,:),             allocatable :: u
     real(r_kind),              dimension(:,:),             allocatable :: v
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: orog
     real(r_kind),              dimension(:),               allocatable :: slmsk     
     real(r_kind),              dimension(:),               allocatable :: idx
     real(r_kind)                                                       :: clat
     real(r_kind)                                                       :: clon
     integer                                                            :: nobs
     integer                                                            :: nz
  end type fcstmdl_struct         ! type fcstmdl_struct
  type fv3_struct
     real(r_kind),              dimension(:,:,:),           allocatable :: u
     real(r_kind),              dimension(:,:,:),           allocatable :: v
     real(r_kind),              dimension(:,:),             allocatable :: p
     real(r_kind),              dimension(:,:),             allocatable :: q
     real(r_kind),              dimension(:,:),             allocatable :: t
     real(r_kind),              dimension(:,:),             allocatable :: ua
     real(r_kind),              dimension(:,:),             allocatable :: va
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: orog
     real(r_kind),              dimension(:),               allocatable :: psfc
     real(r_kind),              dimension(:),               allocatable :: slmsk
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz
  end type fv3_struct             ! type fv3_struct
  type grid_struct
     real(r_kind),              dimension(:),               allocatable :: angle
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: radius
     real(r_kind),              dimension(:),               allocatable :: rotang
     real(r_kind)                                                       :: gcdist
     real(r_kind)                                                       :: gchead
     real(r_kind)                                                       :: gclat
     real(r_kind)                                                       :: gclon
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
  end type grid_struct            ! type grid_struct
  type hsa_struct
     character(len=4),          dimension(:),               allocatable :: tail
     character(len=500)                                                 :: filename
     logical                                                            :: process
     real(r_double)                                                     :: rel_julian
     real(r_double)                                                     :: spg_julian
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: fallrate
     real(r_kind),              dimension(:),               allocatable :: p
     real(r_kind),              dimension(:),               allocatable :: rh
     real(r_kind),              dimension(:),               allocatable :: t
     real(r_kind),              dimension(:),               allocatable :: u
     real(r_kind),              dimension(:),               allocatable :: v
     real(r_kind),              dimension(:),               allocatable :: z
     real(r_kind),              dimension(:),               allocatable :: time 
     real(r_kind),              dimension(:),               allocatable :: yymmdd
     real(r_kind)                                                       :: pmax
     real(r_kind)                                                       :: pmin
     real(r_kind)                                                       :: psfc
     real(r_kind)                                                       :: rellon
     real(r_kind)                                                       :: rellat
     real(r_kind)                                                       :: spglon
     real(r_kind)                                                       :: spglat                                                     
     integer,                   dimension(:),               allocatable :: gmt
     integer,                   dimension(:),               allocatable :: wx
     integer                                                            :: dd
     integer                                                            :: hh
     integer                                                            :: logtime
     integer                                                            :: mm
     integer                                                            :: nmnlevs
     integer                                                            :: nn
     integer                                                            :: nz
     integer                                                            :: rel_hmsts
     integer                                                            :: reltime
     integer                                                            :: spgtime
     integer                                                            :: ss
     integer                                                            :: yyyy
  end type hsa_struct             ! type hsa_struct
  type interp_p_struct 
     real(r_kind),              dimension(:),               allocatable :: var
     real(r_kind),              dimension(:),               allocatable :: p
     real(r_kind)                                                       :: psfc
     integer                                                            :: nz
  end type interp_p_struct        ! type interp_p_struct
  type interp_spline_struct
     real(r_kind),              dimension(:),               allocatable :: xa
     real(r_kind),              dimension(:),               allocatable :: ya
     real(r_kind)                                                       :: x
     real(r_kind)                                                       :: y
     integer                                                            :: n    
  end type interp_spline_struct   ! type interp_spline_struct
  type kdtree_struct
     real(r_kind),              dimension(:,:),             allocatable :: r2dist
     real(r_kind)                                                       :: r2
     integer,                   dimension(:,:),             allocatable :: idx
     integer                                                            :: nalloc
     integer                                                            :: ncoords
     integer                                                            :: nfound
     integer                                                            :: nn
  end type kdtree_struct          ! type kdtree_struct    
  type meteo_struct
     character(len=500)                                                 :: tempdrop_name
     character(len=5)                                                   :: acid
     character(len=2)                                                   :: obnum
     real(r_double),            dimension(:),               allocatable :: dwpt
     real(r_double),            dimension(:),               allocatable :: jdate
     real(r_double),            dimension(:),               allocatable :: lat
     real(r_double),            dimension(:),               allocatable :: lon
     real(r_double),            dimension(:),               allocatable :: p
     real(r_double),            dimension(:),               allocatable :: q
     real(r_double),            dimension(:),               allocatable :: rh
     real(r_double),            dimension(:),               allocatable :: t
     real(r_double),            dimension(:),               allocatable :: thta
     real(r_double),            dimension(:),               allocatable :: thte
     real(r_double),            dimension(:),               allocatable :: thtv
     real(r_double),            dimension(:),               allocatable :: u
     real(r_double),            dimension(:),               allocatable :: v
     real(r_double),            dimension(:),               allocatable :: wdir
     real(r_double),            dimension(:),               allocatable :: wspd
     real(r_double),            dimension(:),               allocatable :: wvmxrt
     real(r_double),            dimension(:),               allocatable :: z
     real(r_kind),              dimension(:),               allocatable :: dist
     real(r_kind),              dimension(:),               allocatable :: head
     real(r_double)                                                     :: psfc
     integer                                                            :: nz
  end type meteo_struct           ! type meteo_struct
  type sonde_struct
     character(len=500),        dimension(:),               allocatable :: filename
     integer                                                            :: nsondes
  end type sonde_struct           ! type sonde_struct
  type statgrid_struct
     real(r_kind),              dimension(:),               allocatable :: var
     real(r_kind)                                                       :: mean
     real(r_kind)                                                       :: vari
     real(r_kind)                                                       :: stdev
     real(r_kind)                                                       :: varmin
     real(r_kind)                                                       :: varmax
     integer                                                            :: n
     integer                                                            :: nvals
  end type statgrid_struct        ! type statgrid_struct
  type tcinfo_struct
     character(len=3)                                                   :: id
     real(r_kind)                                                       :: mdl_clat
     real(r_kind)                                                       :: mdl_clon
     real(r_kind)                                                       :: mdl_pcen
     real(r_kind)                                                       :: mdl_vmax
     real(r_kind)                                                       :: obs_clat
     real(r_kind)                                                       :: obs_clon
     real(r_kind)                                                       :: obs_pcen
     real(r_kind)                                                       :: obs_vmax
  end type tcinfo_struct          ! type tcinfo_struct
  type tdr_struct
     character(len=3),          dimension(:),               allocatable :: stmid
     character(len=19)                                                  :: file_timestamp
     real(r_kind),              dimension(:),               allocatable :: time_max
     real(r_kind),              dimension(:),               allocatable :: time_min
     integer,                   dimension(:),               allocatable :: flag
     integer,                   dimension(:),               allocatable :: nrecs
     integer                                                            :: nstmid
  end type tdr_struct             ! type tdr_struct
  type timeinfo_struct
     character(len=10)                                                  :: idatestr
     real(r_double)                                                     :: jday
     real(r_double)                                                     :: maxjday
     real(r_double)                                                     :: minjday
     integer                                                            :: idate
  end type timeinfo_struct        ! type timeinfo_struct
  type varinfo_struct
     character(len=500),        dimension(:,:,:),           allocatable :: varattrs
     character(len=25),         dimension(:),               allocatable :: varname
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
  type vdm_struct
     character(len=19),         dimension(:,:),             allocatable :: obs_time
     character(len=500),        dimension(:),               allocatable :: filename
     character(len=19),         dimension(:),               allocatable :: fix_time
     real(r_kind),              dimension(:,:),             allocatable :: obs_alt
     real(r_kind),              dimension(:,:),             allocatable :: obs_dist
     real(r_kind),              dimension(:,:),             allocatable :: obs_head
     real(r_kind),              dimension(:,:),             allocatable :: obs_lat
     real(r_kind),              dimension(:,:),             allocatable :: obs_lon
     real(r_kind),              dimension(:,:),             allocatable :: obs_plev
     real(r_kind),              dimension(:,:),             allocatable :: obs_u
     real(r_kind),              dimension(:,:),             allocatable :: obs_v
     real(r_kind),              dimension(:,:),             allocatable :: obs_wdir
     real(r_kind),              dimension(:,:),             allocatable :: obs_wspd
     real(r_kind),              dimension(:),               allocatable :: fix_lat                               
     real(r_kind),              dimension(:),               allocatable :: fix_lon
     integer                                                            :: nobs
     integer                                                            :: nvdm
  end type vdm_struct             ! type vdm_struct

    !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_bufr_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! bufr_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN bufr_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_bufr_struct(grid)

    ! Define variables passed routine

    type(bufr_struct)                                                   :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%obs)) deallocate(grid%obs)
    if(allocated(grid%qcf)) deallocate(grid%qcf)
    if(allocated(grid%oer)) deallocate(grid%oer)
    if(allocated(grid%hdr)) deallocate(grid%hdr)

    !=====================================================================

  end subroutine finalize_bufr_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_bufrhdr_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! bufrhdr_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN bufrhdr_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_bufrhdr_struct(grid)

    ! Define variables passed routine

    type(bufrhdr_struct)                                                :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%hdr)) deallocate(grid%hdr)

    !=====================================================================

  end subroutine finalize_bufrhdr_struct
  
  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_fcstmdl_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! fcstmdl_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN fcstmdl_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_fcstmdl_struct(grid)

    ! Define variables passed to routine

    type(fcstmdl_struct)                                                :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%usage)) deallocate(grid%usage)
    if(allocated(grid%p))     deallocate(grid%p)
    if(allocated(grid%q))     deallocate(grid%q)
    if(allocated(grid%t))     deallocate(grid%t)
    if(allocated(grid%u))     deallocate(grid%u)
    if(allocated(grid%v))     deallocate(grid%v)
    if(allocated(grid%lat))   deallocate(grid%lat)
    if(allocated(grid%lon))   deallocate(grid%lon)
    if(allocated(grid%orog))  deallocate(grid%orog)
    if(allocated(grid%slmsk)) deallocate(grid%slmsk)
    if(allocated(grid%idx))   deallocate(grid%idx)
    
    !=====================================================================    

  end subroutine finalize_fcstmdl_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_fv3_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! fv3_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN fv3_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_fv3_struct(grid)

    ! Define variables passed to routine

    type(fv3_struct)                                                    :: grid
    
    !=====================================================================
    
    ! Deallocate memory for local variables

    if(allocated(grid%u))     deallocate(grid%u)
    if(allocated(grid%v))     deallocate(grid%v)
    if(allocated(grid%p))     deallocate(grid%p)
    if(allocated(grid%q))     deallocate(grid%q)
    if(allocated(grid%t))     deallocate(grid%t)
    if(allocated(grid%ua))    deallocate(grid%ua)
    if(allocated(grid%va))    deallocate(grid%va)
    if(allocated(grid%lat))   deallocate(grid%lat)
    if(allocated(grid%lon))   deallocate(grid%lon)
    if(allocated(grid%orog))  deallocate(grid%orog)
    if(allocated(grid%psfc))  deallocate(grid%psfc)
    if(allocated(grid%slmsk)) deallocate(grid%slmsk)
     
    !=====================================================================

  end subroutine finalize_fv3_struct

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
    if(allocated(grid%lat))    deallocate(grid%lat)
    if(allocated(grid%lon))    deallocate(grid%lon)
    if(allocated(grid%radius)) deallocate(grid%radius)
    if(allocated(grid%rotang)) deallocate(grid%rotang)
    
    !=====================================================================

  end subroutine finalize_grid_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_hsa_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! hsa_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN hsa_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_hsa_struct(grid)

    ! Define variables passed to routine

    type(hsa_struct)                                                    :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%tail))     deallocate(grid%tail)
    if(allocated(grid%lat))      deallocate(grid%lat)
    if(allocated(grid%lon))      deallocate(grid%lon)
    if(allocated(grid%fallrate)) deallocate(grid%fallrate)
    if(allocated(grid%p))        deallocate(grid%p)
    if(allocated(grid%rh))       deallocate(grid%rh)
    if(allocated(grid%t))        deallocate(grid%t)
    if(allocated(grid%u))        deallocate(grid%u)
    if(allocated(grid%v))        deallocate(grid%v)
    if(allocated(grid%yymmdd))   deallocate(grid%yymmdd)
    if(allocated(grid%z))        deallocate(grid%z)
    if(allocated(grid%gmt))      deallocate(grid%gmt)
    if(allocated(grid%wx))       deallocate(grid%wx)
    if(allocated(grid%time))     deallocate(grid%time)

    !=====================================================================
    
  end subroutine finalize_hsa_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_interp_p_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! interp_p_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN interp_p_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_interp_p_struct(grid)

    ! Define variables passed to routine

    type(interp_p_struct)                                               :: grid
    logical                                                             :: debug

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%p))   deallocate(grid%p)
    if(allocated(grid%var)) deallocate(grid%var)

    !=====================================================================
    
  end subroutine finalize_interp_p_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_interp_spline_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! interp_spline_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN interp_spline_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_interp_spline_struct(grid)

    ! Define variables passed to routine

    type(interp_spline_struct)                                          :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%xa)) deallocate(grid%xa)
    if(allocated(grid%ya)) deallocate(grid%ya)

    !=====================================================================
    
  end subroutine finalize_interp_spline_struct

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

    if(allocated(grid%dwpt))   deallocate(grid%dwpt)
    if(allocated(grid%jdate))  deallocate(grid%jdate)
    if(allocated(grid%lat))    deallocate(grid%lat)
    if(allocated(grid%lon))    deallocate(grid%lon)
    if(allocated(grid%p))      deallocate(grid%p)
    if(allocated(grid%q))      deallocate(grid%q)
    if(allocated(grid%rh))     deallocate(grid%rh)
    if(allocated(grid%t))      deallocate(grid%t)
    if(allocated(grid%thta))   deallocate(grid%thta)
    if(allocated(grid%thte))   deallocate(grid%thte)
    if(allocated(grid%thtv))   deallocate(grid%thtv)
    if(allocated(grid%u))      deallocate(grid%u)
    if(allocated(grid%v))      deallocate(grid%v)
    if(allocated(grid%wdir))   deallocate(grid%wdir)
    if(allocated(grid%wspd))   deallocate(grid%wspd)
    if(allocated(grid%wvmxrt)) deallocate(grid%wvmxrt)
    if(allocated(grid%z))      deallocate(grid%z)

    !=====================================================================
    
  end subroutine finalize_meteo_struct  

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_sonde_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! sonde_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN sonde_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_sonde_struct(grid)

    ! Define variables passed to routine

    type(sonde_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%filename)) deallocate(grid%filename)
    
    !=====================================================================

  end subroutine finalize_sonde_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_statgrid_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! statgrid_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN statgrid_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_statgrid_struct(grid)

    ! Define variables passed to routine

    type(statgrid_struct)                                               :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%var)) deallocate(grid%var)

    !=====================================================================
    
  end subroutine finalize_statgrid_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_tdr_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! tdr_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN tdr_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_tdr_struct(grid)

    ! Define variables passed to routine

    type(tdr_struct)                                                    :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%stmid))    deallocate(grid%stmid)
    if(allocated(grid%time_max)) deallocate(grid%time_max)
    if(allocated(grid%time_min)) deallocate(grid%time_min)
    if(allocated(grid%flag))     deallocate(grid%flag)
    if(allocated(grid%nrecs))    deallocate(grid%nrecs)
    
    !=====================================================================

  end subroutine finalize_tdr_struct

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

  ! finalize_vdm_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! vdm_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN vdm_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_vdm_struct(grid)

    ! Define variables passed to routine

    type(vdm_struct)                                                    :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%filename)) deallocate(grid%filename)
    if(allocated(grid%fix_lat))  deallocate(grid%fix_lat)
    if(allocated(grid%fix_lon))  deallocate(grid%fix_lon)
    if(allocated(grid%fix_time)) deallocate(grid%fix_time)    
    if(allocated(grid%obs_alt))  deallocate(grid%obs_alt)
    if(allocated(grid%obs_dist)) deallocate(grid%obs_dist)
    if(allocated(grid%obs_head)) deallocate(grid%obs_head)
    if(allocated(grid%obs_lat))  deallocate(grid%obs_lat)
    if(allocated(grid%obs_lon))  deallocate(grid%obs_lon)
    if(allocated(grid%obs_plev)) deallocate(grid%obs_plev)
    if(allocated(grid%obs_time)) deallocate(grid%obs_time)
    if(allocated(grid%obs_u))    deallocate(grid%obs_u)
    if(allocated(grid%obs_v))    deallocate(grid%obs_v)
    if(allocated(grid%obs_wdir)) deallocate(grid%obs_wdir)
    if(allocated(grid%obs_wspd)) deallocate(grid%obs_wspd)

    !=====================================================================

  end subroutine finalize_vdm_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_bufr_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! bufr_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN bufr_struct variable containing the variables
  !   necessary to allocate and initialize the respective variable
  !   arrays.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN bufr_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_bufr_struct(grid)

    ! Define variables passed routine

    type(bufr_struct)                                                   :: grid

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(grid%mxmn .eq. 0) grid%mxmn = bufr_mxmn       
    if(grid%mxlv .eq. 0) grid%mxlv = bufr_mxlv

    ! Allocate memory for local variables

    if(.not. allocated(grid%obs)) allocate(grid%obs(grid%mxmn,grid%mxlv))
    if(.not. allocated(grid%qcf)) allocate(grid%qcf(grid%mxmn,grid%mxlv))
    if(.not. allocated(grid%oer)) allocate(grid%oer(grid%mxmn,grid%mxlv))
    if(.not. allocated(grid%hdr)) allocate(grid%hdr(grid%mxmn))

    ! Define local variables

    grid%obs = bufr_spval
    grid%qcf = bufr_spval
    grid%oer = bufr_spval
    grid%hdr = bufr_spval

    !=====================================================================

  end subroutine initialize_bufr_struct
  
  !=======================================================================

  ! SUBROUTINE:

  ! initialize_bufrhdr_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! bufrhdr_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN bufrhdr_struct variable containing the variables
  !   necessary to allocate and initialize the respective variable
  !   arrays.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN bufrhdr_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_bufrhdr_struct(grid)

    ! Define variables passed routine

    type(bufrhdr_struct)                                                :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%hdr)) allocate(grid%hdr(grid%mxmn,grid%nrecs))

    !=====================================================================
    
  end subroutine initialize_bufrhdr_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_fcstmdl_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! fcstmdl_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN fcstmdl_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN fcstmdl_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_fcstmdl_struct(grid)

    ! Define variables passed to routine

    type(fcstmdl_struct)                                                :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(.not. allocated(grid%usage))                                        &
         allocate(grid%usage(grid%nobs,grid%nz))
    if(.not. allocated(grid%p))                                            &
         & allocate(grid%p(grid%nobs,grid%nz))
    if(.not. allocated(grid%q))                                            &
         & allocate(grid%q(grid%nobs,grid%nz))
    if(.not. allocated(grid%t))                                            &
         & allocate(grid%t(grid%nobs,grid%nz))
    if(.not. allocated(grid%u))                                            &
         & allocate(grid%u(grid%nobs,grid%nz))
    if(.not. allocated(grid%v))                                            &
         & allocate(grid%v(grid%nobs,grid%nz))
    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%nobs))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%nobs))
    if(.not. allocated(grid%orog))                                         &
         & allocate(grid%orog(grid%nobs))
    if(.not. allocated(grid%slmsk))                                        &
         & allocate(grid%slmsk(grid%nobs))
    if(.not. allocated(grid%idx))                                          &
         & allocate(grid%idx(grid%nobs))

    ! Initialize local variables

    grid%usage = .false.
    
    !=====================================================================    

  end subroutine initialize_fcstmdl_struct
  
  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_fv3_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! fv3_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN fv3_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN fv3_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_fv3_struct(grid)

    ! Define variables passed to routine

    type(fv3_struct)                                                    :: grid  
  
    !=====================================================================

    ! Define local variables

    if(grid%ncoords .le. 0) grid%ncoords = (grid%nx*grid%ny)
       
    ! Allocate memory for local variables
    
    if(.not. allocated(grid%u))                                            &
         & allocate(grid%u(grid%nx,(grid%ny+1),grid%nz))
    if(.not. allocated(grid%v))                                            &
         & allocate(grid%v((grid%nx+1),grid%ny,grid%nz))    
    if(.not. allocated(grid%p))                                            &
         & allocate(grid%p(grid%ncoords,grid%nz))
    if(.not. allocated(grid%q))                                            &
         & allocate(grid%q(grid%ncoords,grid%nz))
    if(.not. allocated(grid%t))                                            &
         & allocate(grid%t(grid%ncoords,grid%nz))
    if(.not. allocated(grid%ua))                                           &
         & allocate(grid%ua(grid%ncoords,grid%nz))
    if(.not. allocated(grid%va))                                           &
         & allocate(grid%va(grid%ncoords,grid%nz))
    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%ncoords))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%ncoords))
    if(.not. allocated(grid%orog))                                         &
         & allocate(grid%orog(grid%ncoords))
    if(.not. allocated(grid%psfc))                                         &
         & allocate(grid%psfc(grid%ncoords))
    if(.not. allocated(grid%slmsk))                                        &
         & allocate(grid%slmsk(grid%ncoords))

    !=====================================================================

  end subroutine initialize_fv3_struct

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
    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%ncoords))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%ncoords))
    if(.not. allocated(grid%radius))                                       &
         & allocate(grid%radius(grid%ncoords))
    if(.not. allocated(grid%rotang))                                       &
         & allocate(grid%rotang(grid%ncoords))
    
    !=====================================================================

  end subroutine initialize_grid_struct
  
  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_hsa_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! hsa_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN hsa_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN hsa_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_hsa_struct(grid)

    ! Define variables passed to routine

    type(hsa_struct)                                                    :: grid    

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%tail))     allocate(grid%tail(grid%nz))
    if(.not. allocated(grid%lat))      allocate(grid%lat(grid%nz))
    if(.not. allocated(grid%lon))      allocate(grid%lon(grid%nz))
    if(.not. allocated(grid%fallrate)) allocate(grid%fallrate(grid%nz))
    if(.not. allocated(grid%p))        allocate(grid%p(grid%nz))
    if(.not. allocated(grid%rh))       allocate(grid%rh(grid%nz))
    if(.not. allocated(grid%t))        allocate(grid%t(grid%nz))
    if(.not. allocated(grid%u))        allocate(grid%u(grid%nz))
    if(.not. allocated(grid%v))        allocate(grid%v(grid%nz))
    if(.not. allocated(grid%yymmdd))   allocate(grid%yymmdd(grid%nz))
    if(.not. allocated(grid%z))        allocate(grid%z(grid%nz))
    if(.not. allocated(grid%gmt))      allocate(grid%gmt(grid%nz))
    if(.not. allocated(grid%wx))       allocate(grid%wx(grid%nz))
    if(.not. allocated(grid%time))     allocate(grid%time(grid%nz))

    ! Define local variables

    grid%process = .true.

    !=====================================================================
    
  end subroutine initialize_hsa_struct

  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_interp_p_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! interp_p_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN interp_p_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN interp_p_struct variable containing allocated
  !   and initialized variable arrays.
  
  !-----------------------------------------------------------------------

  subroutine initialize_interp_p_struct(grid)

    ! Define variables passed to routine

    type(interp_p_struct)                                               :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%p))   allocate(grid%p(grid%nz))
    if(.not. allocated(grid%var)) allocate(grid%var(grid%nz))

    !=====================================================================
    
  end subroutine initialize_interp_p_struct

  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_interp_spline_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! interp_spline_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN interp_spline_struct variable.
  
  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN interp_spline_struct variable containing
  !   allocated and initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_interp_spline_struct(grid)

    ! Define variables passed to routine

    type(interp_spline_struct)                                          :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%xa)) allocate(grid%xa(grid%n))
    if(.not. allocated(grid%ya)) allocate(grid%ya(grid%n))

    !=====================================================================
    
  end subroutine initialize_interp_spline_struct

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

  ! initialize_meteo_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! meteo_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_meteo_struct(grid)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%dwpt))   allocate(grid%dwpt(grid%nz))
    if(.not. allocated(grid%jdate))  allocate(grid%jdate(grid%nz))
    if(.not. allocated(grid%lat))    allocate(grid%lat(grid%nz))
    if(.not. allocated(grid%lon))    allocate(grid%lon(grid%nz))
    if(.not. allocated(grid%p))      allocate(grid%p(grid%nz))
    if(.not. allocated(grid%q))      allocate(grid%q(grid%nz))
    if(.not. allocated(grid%rh))     allocate(grid%rh(grid%nz))
    if(.not. allocated(grid%t))      allocate(grid%t(grid%nz))
    if(.not. allocated(grid%thta))   allocate(grid%thta(grid%nz))
    if(.not. allocated(grid%thte))   allocate(grid%thte(grid%nz))
    if(.not. allocated(grid%thtv))   allocate(grid%thtv(grid%nz))
    if(.not. allocated(grid%u))      allocate(grid%u(grid%nz))
    if(.not. allocated(grid%v))      allocate(grid%v(grid%nz))
    if(.not. allocated(grid%wdir))   allocate(grid%wdir(grid%nz))
    if(.not. allocated(grid%wspd))   allocate(grid%wspd(grid%nz))
    if(.not. allocated(grid%wvmxrt)) allocate(grid%wvmxrt(grid%nz))
    if(.not. allocated(grid%z))      allocate(grid%z(grid%nz))

    ! Define local variables

    grid%dwpt   = spval
    grid%jdate  = spval
    grid%lat    = spval
    grid%lon    = spval
    grid%p      = spval
    grid%q      = spval
    grid%rh     = spval
    grid%t      = spval
    grid%thta   = spval
    grid%thte   = spval
    grid%thtv   = spval
    grid%u      = spval
    grid%v      = spval
    grid%wdir   = spval
    grid%wspd   = spval
    grid%wvmxrt = spval
    grid%z      = spval
    grid%psfc   = spval

    !=====================================================================
    
  end subroutine initialize_meteo_struct

  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_sonde_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! sonde_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN sonde_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN sonde_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_sonde_struct(grid)

    ! Define variables passed to routine

    type(sonde_struct)                                                  :: grid    

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%filename))                                     &
         & allocate(grid%filename(grid%nsondes))

    !=====================================================================

  end subroutine initialize_sonde_struct

  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_statgrid_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! statgrid_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN statgrid_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN statgrid_struct variable containing allocated
  !   and initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_statgrid_struct(grid)

    ! Define variables passed to routine

    type(statgrid_struct)                                               :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%var)) allocate(grid%var(grid%n))

    !=====================================================================
    
  end subroutine initialize_statgrid_struct

  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_tdr_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! tdr_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN tdr_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN tdr_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_tdr_struct(grid)

    ! Define variables passed to routine

    type(tdr_struct)                                                    :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%stmid))                                        &
         & allocate(grid%stmid(grid%nstmid))
    if(.not. allocated(grid%time_max))                                     &
         & allocate(grid%time_max(grid%nstmid))
    if(.not. allocated(grid%time_min))                                     &
         & allocate(grid%time_min(grid%nstmid))
    if(.not. allocated(grid%flag))                                         &
         & allocate(grid%flag(grid%nstmid))
    if(.not. allocated(grid%nrecs))                                        &
         & allocate(grid%nrecs(grid%nstmid))
    
    !=====================================================================
    
  end subroutine initialize_tdr_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_varinfo_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! varinfo_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN varinfo_struct variable containing allocated and
  !   initialized variable arrays.

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

  ! initialize_vdm_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! vdm_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN vdm_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN vdm_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_vdm_struct(grid)

    ! Define variables passed to routine

    type(vdm_struct)                                                    :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%obs_alt))                                      &
         & allocate(grid%obs_alt(grid%nvdm,grid%nobs))  
    if(.not. allocated(grid%obs_dist))                                     &
         & allocate(grid%obs_dist(grid%nvdm,grid%nobs))
    if(.not. allocated(grid%obs_head))                                     &
         & allocate(grid%obs_head(grid%nvdm,grid%nobs))
    if(.not. allocated(grid%obs_lat))                                      &
         & allocate(grid%obs_lat(grid%nvdm,grid%nobs))
    if(.not. allocated(grid%obs_lon))                                      &
         & allocate(grid%obs_lon(grid%nvdm,grid%nobs))
    if(.not. allocated(grid%obs_plev))                                     &
         & allocate(grid%obs_plev(grid%nvdm,grid%nobs))
    if(.not. allocated(grid%obs_time))                                     &
         & allocate(grid%obs_time(grid%nvdm,grid%nobs))
    if(.not. allocated(grid%obs_u))                                        &
         & allocate(grid%obs_u(grid%nvdm,grid%nobs))
    if(.not. allocated(grid%obs_v))                                        &
         & allocate(grid%obs_v(grid%nvdm,grid%nobs))    
    if(.not. allocated(grid%obs_wdir))                                     &
         & allocate(grid%obs_wdir(grid%nvdm,grid%nobs))
    if(.not. allocated(grid%obs_wspd))                                     &
         & allocate(grid%obs_wspd(grid%nvdm,grid%nobs))
    if(.not. allocated(grid%filename))                                     &
         & allocate(grid%filename(grid%nvdm))
    if(.not. allocated(grid%fix_lat))                                      &
         & allocate(grid%fix_lat(grid%nvdm))
    if(.not. allocated(grid%fix_lon))                                      &
         & allocate(grid%fix_lon(grid%nvdm))
    if(.not. allocated(grid%fix_time))                                     &
         & allocate(grid%fix_time(grid%nvdm))

    ! Define local variables

    grid%obs_dist = spval
    grid%obs_head = spval
    grid%obs_plev = spval
    grid%obs_lat  = spval
    grid%obs_lon  = spval
    grid%obs_time = '0000-00-00_00:00:00'
    grid%obs_u    = spval
    grid%obs_v    = spval
    grid%obs_wdir = spval
    grid%obs_wspd = spval
    
    !=====================================================================

  end subroutine initialize_vdm_struct

  !=======================================================================
  
end module variable_interface
