module variable_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! obs-to-bufr :: variable_interface
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

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: bufr_info_struct
  public :: bufr_mxlv
  public :: bufr_mxmn
  public :: bufr_spval
  public :: bufr_struct
  public :: error_struct
  public :: fcst_model_struct
  public :: givtdruv_struct
  public :: grid_struct
  public :: hsa_spval
  public :: hsa_struct
  public :: meteo_struct
  public :: nhcgtcm_struct
  public :: obs_flag_struct
  public :: remap_struct
  public :: slint_struct
  public :: spline_struct
  public :: spval
  public :: tcm_struct
  public :: topogrid_struct
  public :: variable_interface_cleanup_struct
  public :: variable_interface_setup_struct
  public :: varinfo_struct
  interface variable_interface_cleanup_struct
     module procedure finalize_bufr_struct
     module procedure finalize_error_struct
     module procedure finalize_fcst_model_struct
     module procedure finalize_givtdruv_struct
     module procedure finalize_grid_struct
     module procedure finalize_hsa_struct
     module procedure finalize_nhcgtcm_struct
     module procedure finalize_obs_flag_struct
     module procedure finalize_remap_struct
     module procedure finalize_slint_struct
     module procedure finalize_spline_struct
     module procedure finalize_tcm_struct
     module procedure finalize_topogrid_struct
     module procedure finalize_varinfo_struct
  end interface variable_interface_cleanup_struct
  interface variable_interface_setup_struct
     module procedure initialize_bufr_struct
     module procedure initialize_error_struct
     module procedure initialize_fcst_model_struct
     module procedure initialize_givtdruv_struct
     module procedure initialize_grid_struct
     module procedure initialize_hsa_struct
     module procedure initialize_nhcgtcm_struct
     module procedure initialize_obs_flag_struct
     module procedure initialize_remap_struct
     module procedure initialize_slint_struct
     module procedure initialize_spline_struct
     module procedure initialize_tcm_struct
     module procedure initialize_topogrid_struct
     module procedure initialize_varinfo_struct
  end interface variable_interface_setup_struct

  ! Define local variables

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
  type error_struct
     real(r_kind),              dimension(:),               allocatable :: plev
     real(r_kind),              dimension(:),               allocatable :: p
     real(r_kind),              dimension(:),               allocatable :: t
     real(r_kind),              dimension(:),               allocatable :: q
     real(r_kind),              dimension(:),               allocatable :: z
     real(r_kind),              dimension(:),               allocatable :: uv
     integer                                                            :: nz     
  end type error_struct           ! type error_struct
  type fcst_model_struct
     real(r_kind),              dimension(:,:),             allocatable :: p
     real(r_kind),              dimension(:,:),             allocatable :: q
     real(r_kind),              dimension(:,:),             allocatable :: t
     real(r_kind),              dimension(:,:),             allocatable :: u
     real(r_kind),              dimension(:,:),             allocatable :: v
     real(r_kind),              dimension(:,:),             allocatable :: z
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: lmsk
     real(r_kind),              dimension(:),               allocatable :: psfc
     integer                                                            :: nobs
     integer                                                            :: nz 
  end type fcst_model_struct      ! type fcst_model_struct
  type givtdruv_struct
     character(len=19)                                                  :: timestamp
     real(r_kind),              dimension(:),               allocatable :: u
     real(r_kind),              dimension(:),               allocatable :: v
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: level
     real(r_kind)                                                       :: msngvl
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz 
  end type givtdruv_struct        ! type givtdruv_struct
  type grid_struct
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: ncoords
  end type grid_struct            ! type grid_struct
  type hsa_struct
     character(len=4),          dimension(:),               allocatable :: tail
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: p
     real(r_kind),              dimension(:),               allocatable :: rh
     real(r_kind),              dimension(:),               allocatable :: t
     real(r_kind),              dimension(:),               allocatable :: u
     real(r_kind),              dimension(:),               allocatable :: v
     real(r_kind),              dimension(:),               allocatable :: z
     real(r_kind),              dimension(:),               allocatable :: yymmdd
     integer,                   dimension(:),               allocatable :: gmt
     integer,                   dimension(:),               allocatable :: wx     
     integer                                                            :: nz
  end type hsa_struct             ! type hsa_struct
  type meteo_struct
     real(r_double)                                                     :: p
     real(r_double)                                                     :: q
     real(r_double)                                                     :: rh
     real(r_double)                                                     :: t
     real(r_double)                                                     :: wvmxrt
  end type meteo_struct           ! type meteo_struct
  type nhcgtcm_struct
     character(len=19)                                                  :: analysis_date
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: u10m
     real(r_kind),              dimension(:),               allocatable :: v10m
     real(r_kind)                                                       :: spval
     integer                                                            :: nvals
  end type nhcgtcm_struct         ! type nhcgtcm_struct
  type obs_flag_struct
     character(len=500)                                                 :: filename
     character(len=10),         dimension(:),               allocatable :: mneumonic
     character(len=8),          dimension(:),               allocatable :: subset
     real(r_kind),              dimension(:),               allocatable :: val
     integer,                   dimension(:),               allocatable :: obs_type
     integer                                                            :: nflag
  end type obs_flag_struct        ! type obs_flag_struct
  type remap_struct
     real(r_kind),              dimension(:),               allocatable :: hgt
     real(r_kind),              dimension(:),               allocatable :: lat     
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: mask
     integer                                                            :: ncoords      
  end type remap_struct           ! type remap_struct
  type slint_struct
     real(r_double),            dimension(:,:),             allocatable :: coeffs
     integer,                   dimension(:,:),             allocatable :: nn
     integer                                                            :: ncoeffs
     integer                                                            :: ncoords
  end type slint_struct           ! type slint_struct
  type spline_struct
     real(r_kind),              dimension(:),               allocatable :: xa
     real(r_kind),              dimension(:),               allocatable :: ya
     real(r_kind)                                                       :: x
     real(r_kind)                                                       :: y
     integer                                                            :: n    
  end type spline_struct          ! type spline_struct
  type tcm_struct
     character(len=19)                                                  :: analysis_date
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: psfc
     real(r_kind),              dimension(:),               allocatable :: u
     real(r_kind),              dimension(:),               allocatable :: v  
     real(r_kind)                                                       :: spval
     integer                                                            :: nvals
     integer                                                            :: nx
     integer                                                            :: ny
  end type tcm_struct             ! type tcm_struct
  type topogrid_struct
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: topo
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
  end type topogrid_struct        ! type topogrid_struct
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
  real(r_double), parameter                                             :: bufr_spval = 10.e10
  real(r_double), parameter                                             :: spval      = huge(1.0)
  real(r_kind),   parameter                                             :: hsa_spval  = -99.0
  integer,        parameter                                             :: bufr_mxlv  = 200
  integer,        parameter                                             :: bufr_mxmn  = 35

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

  ! finalize_error_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! error_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN error_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_error_struct(grid)

    ! Define variables passed routine

    type(error_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%plev)) deallocate(grid%plev)
    if(allocated(grid%p))    deallocate(grid%p)
    if(allocated(grid%t))    deallocate(grid%t)
    if(allocated(grid%q))    deallocate(grid%q)
    if(allocated(grid%z))    deallocate(grid%z)
    if(allocated(grid%uv))   deallocate(grid%uv)

    !=====================================================================

  end subroutine finalize_error_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_fcst_model_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! fcst_model_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN fcst_model_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_fcst_model_struct(grid)

    ! Define variables passed routine

    type(fcst_model_struct)                                             :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%p))    deallocate(grid%p)
    if(allocated(grid%q))    deallocate(grid%q)
    if(allocated(grid%t))    deallocate(grid%t)
    if(allocated(grid%u))    deallocate(grid%u)
    if(allocated(grid%v))    deallocate(grid%v)
    if(allocated(grid%z))    deallocate(grid%z)
    if(allocated(grid%lat))  deallocate(grid%lat)
    if(allocated(grid%lon))  deallocate(grid%lon)
    if(allocated(grid%lmsk)) deallocate(grid%lmsk)
    if(allocated(grid%psfc)) deallocate(grid%psfc)

    !=====================================================================

  end subroutine finalize_fcst_model_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_givtdruv_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! givtdruv_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN givtdruv_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_givtdruv_struct(grid)

    ! Define variables passed to routine

    type(givtdruv_struct)                                               :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%u))     deallocate(grid%u)
    if(allocated(grid%v))     deallocate(grid%v)
    if(allocated(grid%lat))   deallocate(grid%lat)
    if(allocated(grid%lon))   deallocate(grid%lon)
    if(allocated(grid%level)) deallocate(grid%level)

    !=====================================================================
    
  end subroutine finalize_givtdruv_struct

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

    if(allocated(grid%lon)) deallocate(grid%lon)
    if(allocated(grid%lat)) deallocate(grid%lat)

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

    ! Define variables passed routine

    type(hsa_struct)                                                    :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%tail))   deallocate(grid%tail)
    if(allocated(grid%lat))    deallocate(grid%lat)
    if(allocated(grid%lon))    deallocate(grid%lon)
    if(allocated(grid%p))      deallocate(grid%p)
    if(allocated(grid%rh))     deallocate(grid%rh)
    if(allocated(grid%t))      deallocate(grid%t)
    if(allocated(grid%u))      deallocate(grid%u)
    if(allocated(grid%v))      deallocate(grid%v)
    if(allocated(grid%z))      deallocate(grid%z)
    if(allocated(grid%yymmdd)) deallocate(grid%yymmdd)
    if(allocated(grid%gmt))    deallocate(grid%gmt)
    if(allocated(grid%wx))     deallocate(grid%wx)

    !=====================================================================

  end subroutine finalize_hsa_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_nhcgtcm_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! nhcgtcm_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN nhcgtcm_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_nhcgtcm_struct(grid)

    ! Define variables passed routine

    type(nhcgtcm_struct)                                                :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%lat))  deallocate(grid%lat)
    if(allocated(grid%lon))  deallocate(grid%lon)
    if(allocated(grid%u10m)) deallocate(grid%u10m)
    if(allocated(grid%v10m)) deallocate(grid%v10m)

    !=====================================================================

  end subroutine finalize_nhcgtcm_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_obs_flag_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! obs_flag_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN obs_flag_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_obs_flag_struct(grid)

    ! Define variables passed routine

    type(obs_flag_struct)                                               :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%mneumonic)) deallocate(grid%mneumonic)
    if(allocated(grid%subset))    deallocate(grid%subset)
    if(allocated(grid%val))       deallocate(grid%val)
    if(allocated(grid%obs_type))  deallocate(grid%obs_type)

    !=====================================================================

  end subroutine finalize_obs_flag_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_remap_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! remap_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN remap_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_remap_struct(grid)

    ! Define variables passed routine

    type(remap_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%hgt))  deallocate(grid%hgt)
    if(allocated(grid%lat))  deallocate(grid%lat)
    if(allocated(grid%lon))  deallocate(grid%lon)
    if(allocated(grid%mask)) deallocate(grid%mask)

    !=====================================================================

  end subroutine finalize_remap_struct

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

    if(allocated(grid%coeffs)) deallocate(grid%coeffs)
    if(allocated(grid%nn))     deallocate(grid%nn)

    !=====================================================================

  end subroutine finalize_slint_struct

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

  ! finalize_tcm_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! tcm_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN tcm_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_tcm_struct(grid)

    ! Define variables passed routine

    type(tcm_struct)                                                    :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%lat))  deallocate(grid%lat)
    if(allocated(grid%lon))  deallocate(grid%lon)
    if(allocated(grid%psfc)) deallocate(grid%psfc)
    if(allocated(grid%u))    deallocate(grid%u)
    if(allocated(grid%v))    deallocate(grid%v)

    !=====================================================================

  end subroutine finalize_tcm_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_topogrid_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! topogrid_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN topogrid_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_topogrid_struct(grid)

    ! Define variables passed routine

    type(topogrid_struct)                                               :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%lat))  deallocate(grid%lat)
    if(allocated(grid%lon))  deallocate(grid%lon)
    if(allocated(grid%topo)) deallocate(grid%topo)

    !=====================================================================

  end subroutine finalize_topogrid_struct

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

  ! initialize_error_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! error_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN error_struct variable containing the variables
  !   necessary to allocate and initialize the respective variable
  !   arrays.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN error_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_error_struct(grid)

    ! Define variables passed routine

    type(error_struct)                                                  :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%plev)) allocate(grid%plev(grid%nz))
    if(.not. allocated(grid%p))    allocate(grid%p(grid%nz))
    if(.not. allocated(grid%t))    allocate(grid%t(grid%nz))
    if(.not. allocated(grid%q))    allocate(grid%q(grid%nz))
    if(.not. allocated(grid%z))    allocate(grid%z(grid%nz))
    if(.not. allocated(grid%uv))   allocate(grid%uv(grid%nz))

    !=====================================================================

  end subroutine initialize_error_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_fcst_model_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! fcst_model_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN fcst_model_struct variable containing the
  !   variables necessary to allocate and initialize the respective
  !   variable arrays.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN fcst_model_struct variable containing allocated
  !   and initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_fcst_model_struct(grid)

    ! Define variables passed routine

    type(fcst_model_struct)                                             :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%p))    allocate(grid%p(grid%nobs,grid%nz))
    if(.not. allocated(grid%q))    allocate(grid%q(grid%nobs,grid%nz))
    if(.not. allocated(grid%t))    allocate(grid%t(grid%nobs,grid%nz))
    if(.not. allocated(grid%u))    allocate(grid%u(grid%nobs,grid%nz))
    if(.not. allocated(grid%v))    allocate(grid%v(grid%nobs,grid%nz))
    if(.not. allocated(grid%z))    allocate(grid%z(grid%nobs,grid%nz))
    if(.not. allocated(grid%lat))  allocate(grid%lat(grid%nobs))
    if(.not. allocated(grid%lon))  allocate(grid%lon(grid%nobs))
    if(.not. allocated(grid%lmsk)) allocate(grid%lmsk(grid%nobs))
    if(.not. allocated(grid%psfc)) allocate(grid%psfc(grid%nobs))

    !=====================================================================

  end subroutine initialize_fcst_model_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_givtdruv_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! givtdruv_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN givtdruv_struct variable containing the
  !   variables necessary to allocate and initialize the respective
  !   variable arrays.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN givtdruv_struct variable containing allocated
  !   and initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_givtdruv_struct(grid)

    ! Define variables passed routine

    type(givtdruv_struct)                                               :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny*grid%nz)

    ! Allocate memory for local variables

    if(.not. allocated(grid%u))     allocate(grid%u(grid%ncoords))
    if(.not. allocated(grid%v))     allocate(grid%v(grid%ncoords))
    if(.not. allocated(grid%lat))   allocate(grid%lat(grid%ncoords))
    if(.not. allocated(grid%lon))   allocate(grid%lon(grid%ncoords))
    if(.not. allocated(grid%level)) allocate(grid%level(grid%ncoords))

    !=====================================================================

  end subroutine initialize_givtdruv_struct

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

    if(.not. allocated(grid%lat)) allocate(grid%lat(grid%ncoords))
    if(.not. allocated(grid%lon)) allocate(grid%lon(grid%ncoords))

    !=====================================================================

  end subroutine initialize_grid_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_hsa_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! hsa_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN hsa_struct variable containing the variables
  !   necessary to allocate and initialize the respective variable
  !   arrays.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN hsa_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_hsa_struct(grid)

    ! Define variables passed routine

    type(hsa_struct)                                                    :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%tail))   allocate(grid%tail(grid%nz))
    if(.not. allocated(grid%lat))    allocate(grid%lat(grid%nz))
    if(.not. allocated(grid%lon))    allocate(grid%lon(grid%nz))
    if(.not. allocated(grid%p))      allocate(grid%p(grid%nz))
    if(.not. allocated(grid%rh))     allocate(grid%rh(grid%nz))
    if(.not. allocated(grid%t))      allocate(grid%t(grid%nz))
    if(.not. allocated(grid%u))      allocate(grid%u(grid%nz))
    if(.not. allocated(grid%v))      allocate(grid%v(grid%nz))
    if(.not. allocated(grid%z))      allocate(grid%z(grid%nz))
    if(.not. allocated(grid%yymmdd)) allocate(grid%yymmdd(grid%nz))
    if(.not. allocated(grid%gmt))    allocate(grid%gmt(grid%nz))
    if(.not. allocated(grid%wx))     allocate(grid%wx(grid%nz))

    ! Define local variables

    grid%lat = hsa_spval
    grid%lon = hsa_spval
    grid%p   = hsa_spval
    grid%rh  = hsa_spval
    grid%t   = hsa_spval
    grid%u   = hsa_spval
    grid%v   = hsa_spval
    grid%z   = hsa_spval

    !=====================================================================

  end subroutine initialize_hsa_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_nhcgtcm_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory (an initializes, when necessary)
  ! for all arrays within the nhcgtcm_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN nhcgtcm_struct variable containing the variables
  !   necessary to allocate and initialize the respective variable
  !   arrays.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN nhcgtcm_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_nhcgtcm_struct(grid)

    ! Define variables passed routine

    type(nhcgtcm_struct)                                                :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%lat))  allocate(grid%lat(grid%nvals))
    if(.not. allocated(grid%lon))  allocate(grid%lon(grid%nvals))
    if(.not. allocated(grid%u10m)) allocate(grid%u10m(grid%nvals))
    if(.not. allocated(grid%v10m)) allocate(grid%v10m(grid%nvals))

    !=====================================================================

  end subroutine initialize_nhcgtcm_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_obs_flag_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! obs_flag_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN obs_flag_struct variable containing the
  !   variables necessary to allocate and initialize the respective
  !   variable arrays.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN obs_flag_struct variable containing allocated
  !   and initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_obs_flag_struct(grid)

    ! Define variables passed routine

    type(obs_flag_struct)                                               :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%mneumonic))                                    &
         & allocate(grid%mneumonic(grid%nflag))
    if(.not. allocated(grid%subset))                                       &
         & allocate(grid%subset(grid%nflag))
    if(.not. allocated(grid%val))                                          &
         & allocate(grid%val(grid%nflag))
    if(.not. allocated(grid%obs_type))                                     &
         & allocate(grid%obs_type(grid%nflag))

    !=====================================================================

  end subroutine initialize_obs_flag_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_remap_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! remap_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN remap_struct variable containing the variables
  !   necessary to allocate and initialize the respective variable
  !   arrays.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN remap_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_remap_struct(grid)

    ! Define variables passed routine

    type(remap_struct)                                                  :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%hgt))  allocate(grid%hgt(grid%ncoords))
    if(.not. allocated(grid%lat))  allocate(grid%lat(grid%ncoords))
    if(.not. allocated(grid%lon))  allocate(grid%lon(grid%ncoords))
    if(.not. allocated(grid%mask)) allocate(grid%mask(grid%ncoords))
    
    ! Define local variables

    grid%mask = 1.0

    !=====================================================================

  end subroutine initialize_remap_struct

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
         & allocate(grid%coeffs(grid%ncoeffs,grid%ncoords))
    if(.not. allocated(grid%nn))                                           &
         & allocate(grid%nn(grid%ncoeffs,grid%ncoords))

    !=====================================================================
    
  end subroutine initialize_slint_struct

  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_spline_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! spline_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN spline_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN spline_struct variable containing allocated
  !   variable arrays.

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

  ! initialize_tcm_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! tcm_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN tcm_struct variable containing the variables
  !   necessary to allocate and initialize the respective variable
  !   arrays.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN tcm_struct variable containing allocated and
  !   initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_tcm_struct(grid)

    ! Define variables passed routine

    type(tcm_struct)                                                    :: grid

    !=====================================================================

    ! Define local variables

    grid%nvals = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%lat))  allocate(grid%lat(grid%nvals))
    if(.not. allocated(grid%lon))  allocate(grid%lon(grid%nvals))
    if(.not. allocated(grid%psfc)) allocate(grid%psfc(grid%nvals))
    if(.not. allocated(grid%u))    allocate(grid%u(grid%nvals))
    if(.not. allocated(grid%v))    allocate(grid%v(grid%nvals))

    !=====================================================================

  end subroutine initialize_tcm_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_topogrid_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! topogrid_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN topogrid_struct variable containing the
  !   variables necessary to allocate and initialize the respective
  !   variable arrays.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN topogrid_struct variable containing allocated
  !   and initialized variable arrays.

  !-----------------------------------------------------------------------

  subroutine initialize_topogrid_struct(grid)

    ! Define variables passed routine

    type(topogrid_struct)                                               :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%lat))  allocate(grid%lat(grid%ncoords))
    if(.not. allocated(grid%lon))  allocate(grid%lon(grid%ncoords))
    if(.not. allocated(grid%topo)) allocate(grid%topo(grid%ncoords))

    !=====================================================================

  end subroutine initialize_topogrid_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_varinfo_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! varinfo_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN varinfo_struct variable.

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

end module variable_interface
