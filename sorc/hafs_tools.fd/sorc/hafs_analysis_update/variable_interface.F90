module variable_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: variable_interface
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

  use kinds_interface
  use namelist_interface
  use nemsio_module

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: clpval
  public :: fv3grid_struct
  public :: fv3var_struct
  public :: grid_struct
  public :: interp_struct
  public :: json_fv3var_struct
  public :: json_interp_struct
  public :: json_pattern_struct
  public :: kdtree_struct
  public :: meteo_struct
  public :: nems_struct
  public :: pattern_struct
  public :: remappres_struct
  public :: remapvar_struct
  public :: spline_struct
  public :: spval
  public :: statgrid_struct
  public :: svd_struct
  public :: tcv_struct
  public :: timeinfo_struct
  public :: vargrid_struct
  public :: variable_interface_cleanup_struct
  public :: variable_interface_setup_struct
  public :: variable_interface_vtable_lookup
  public :: varinfo_struct
  public :: winds_struct
  interface variable_interface_cleanup_struct
     module procedure finalize_fv3grid_struct
     module procedure finalize_fv3var_struct
     module procedure finalize_grid_struct
     module procedure finalize_interp_struct
     module procedure finalize_kdtree_struct
     module procedure finalize_meteo_struct
     module procedure finalize_nems_struct
     module procedure finalize_pattern_struct
     module procedure finalize_remappres_struct
     module procedure finalize_remapvar_struct
     module procedure finalize_spline_struct
     module procedure finalize_svd_struct
     module procedure finalize_vargrid_struct
     module procedure finalize_varinfo_struct
     module procedure finalize_winds_struct
  end interface variable_interface_cleanup_struct
  interface variable_interface_setup_struct
     module procedure initialize_fv3grid_struct
     module procedure initialize_fv3var_struct
     module procedure initialize_grid_struct
     module procedure initialize_interp_struct
     module procedure initialize_kdtree_struct
     module procedure initialize_meteo_struct
     module procedure initialize_nems_struct
     module procedure initialize_pattern_struct
     module procedure initialize_remappres_struct
     module procedure initialize_remapvar_struct
     module procedure initialize_spline_struct
     module procedure initialize_svd_struct
     module procedure initialize_vargrid_struct
     module procedure initialize_varinfo_struct
     module procedure initialize_winds_struct
  end interface variable_interface_setup_struct
  interface variable_interface_vtable_lookup
     module procedure pattern_vtable_lookup_nemsio
  end interface variable_interface_vtable_lookup

  ! Define local variables

  type fv3grid_struct
     real(r_kind),              dimension(:,:),             allocatable :: pres
     real(r_kind),              dimension(:),               allocatable :: ak
     real(r_kind),              dimension(:),               allocatable :: bk
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: mask
     real(r_kind),              dimension(:),               allocatable :: radius
     real(r_kind)                                                       :: clat
     real(r_kind)                                                       :: clon
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz
  end type fv3grid_struct         ! type fv3grid_struct
  type fv3var_struct
     real(r_kind),              dimension(:,:),             allocatable :: pres
     real(r_kind),              dimension(:,:),             allocatable :: var
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz
  end type fv3var_struct          ! type fv3var_struct
  type grid_struct
     real(r_kind),              dimension(:,:),             allocatable :: pres
     real(r_kind),              dimension(:,:),             allocatable :: var
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind)                                                       :: clat
     real(r_kind)                                                       :: clon
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz
  end type grid_struct            ! type grid_struct
  type interp_struct
     real(r_double),            dimension(:,:),             allocatable :: slint_coeffs
     real(r_kind),              dimension(:),               allocatable :: var
     integer,                   dimension(:,:),             allocatable :: slint_nn
     integer                                                            :: ncoeffs
     integer                                                            :: ncoords
  end type interp_struct          ! type interp_struct  
  type json_fv3var_struct
     character(len=20)                                                  :: variable_name
     logical                                                            :: clip
     logical                                                            :: z_staggered
     logical                                                            :: interp_bilinear
     logical                                                            :: interp_nrstnghbr
     integer                                                            :: gridtype
     integer                                                            :: levtype
  end type json_fv3var_struct     ! type json_fv3var_struct
  type json_interp_struct
     logical                                                            :: interp_bilinear
     logical                                                            :: interp_nrstnghbr
  end type json_interp_struct     ! type json_interp_struct
  type json_pattern_struct
     character(len=20)                                                  :: variable_name
     logical                                                            :: clip
     real(r_kind)                                                       :: vari_thresh
     integer                                                            :: max_lev
     integer                                                            :: min_lev
     integer                                                            :: levtype
  end type json_pattern_struct    ! type json_pattern_struct 
  type kdtree_struct
     real(r_kind),              dimension(:,:),             allocatable :: r2dist
     real(r_kind),              dimension(:),               allocatable :: dst_lat
     real(r_kind),              dimension(:),               allocatable :: dst_lon
     real(r_kind),              dimension(:),               allocatable :: src_lat
     real(r_kind),              dimension(:),               allocatable :: src_lon
     real(r_kind)                                                       :: r2
     integer,                   dimension(:,:),             allocatable :: idx
     integer                                                            :: dst_ncoords
     integer                                                            :: nalloc
     integer                                                            :: nfound
     integer                                                            :: nn
     integer                                                            :: src_ncoords
  end type kdtree_struct          ! type kdtree_struct
  type meteo_struct
     real(r_kind),              dimension(:,:),             allocatable :: u
     real(r_kind),              dimension(:,:),             allocatable :: v
     real(r_kind),              dimension(:,:),             allocatable :: wdir
     real(r_kind),              dimension(:,:),             allocatable :: wspd
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz     
  end type meteo_struct           ! type meteo_struct
  type nems_struct
     character(nemsio_charkind)                                         :: varname
     character(nemsio_charkind)                                         :: levtype
     real(r_double)                                                     :: jday
     real(nemsio_realkind),     dimension(:),               allocatable :: lat
     real(nemsio_realkind),     dimension(:),               allocatable :: lon
     real(nemsio_realkind),     dimension(:),               allocatable :: var
     integer(nemsio_intkind)                                            :: idate(7)
     integer(nemsio_intkind)                                            :: jcap
     integer(nemsio_intkind)                                            :: lev
     integer(nemsio_intkind)                                            :: nx
     integer(nemsio_intkind)                                            :: ny
     integer(nemsio_intkind)                                            :: nz
     integer                                                            :: ncoords
  end type nems_struct            ! type nems_struct
  type pattern_struct
     character(len=3)                                                   :: type
     complex(r_kind),           dimension(:),               allocatable :: noise
     real(r_kind),              dimension(:,:),             allocatable :: lat
     real(r_kind),              dimension(:,:),             allocatable :: lon
     real(r_kind),              dimension(:,:),             allocatable :: rnp
     real(r_kind),              dimension(:),               allocatable :: lap
     real(r_kind),              dimension(:),               allocatable :: varspec
     real(r_kind),              dimension(:),               allocatable :: varspec1d
     real(r_kind)                                                       :: dt
     real(r_kind)                                                       :: lengthscale
     real(r_kind)                                                       :: normfact
     real(r_kind)                                                       :: phi
     real(r_kind)                                                       :: rnp_max_scale
     real(r_kind)                                                       :: rnp_min_scale
     real(r_kind)                                                       :: stdev
     real(r_kind)                                                       :: tau
     real(r_kind)                                                       :: vari
     integer,                   dimension(:),               allocatable :: degree
     integer,                   dimension(:),               allocatable :: order
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: ntrunc
     integer                                                            :: ndimspec
     integer                                                            :: seed
  end type pattern_struct         ! type pattern_struct
  type remappres_struct
     real(r_kind),              dimension(:,:),             allocatable :: pres
     real(r_kind),              dimension(:),               allocatable :: psfc
     real(r_kind),              dimension(:),               allocatable :: ak
     real(r_kind),              dimension(:),               allocatable :: bk
     integer                                                            :: ncoords
     integer                                                            :: nz     
  end type remappres_struct       ! type remappres_struct
  type remapvar_struct
     real(r_kind),              dimension(:,:),             allocatable :: fixed_var
     real(r_kind),              dimension(:,:),             allocatable :: remap_var
     integer                                                            :: ncoords
     integer                                                            :: nz
  end type remapvar_struct        ! type remapvar_struct
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
  type svd_struct
     logical                                                            :: dcnstrct
     logical                                                            :: rcnstrct
     real(dp_intel),            dimension(:,:),             allocatable :: a
     real(dp_intel),            dimension(:,:),             allocatable :: u
     real(dp_intel),            dimension(:,:),             allocatable :: v
     real(dp_intel),            dimension(:,:),             allocatable :: vt
     real(dp_intel),            dimension(:),               allocatable :: s
     real(dp_intel),            dimension(:),               allocatable :: svarex
     real(dp_intel)                                                     :: mean
     integer                                                            :: lda
     integer                                                            :: ldu
     integer                                                            :: ldvt
     integer                                                            :: nx
     integer                                                            :: ny
  end type svd_struct             ! type svd_struct
  type tcv_struct
     character(len=9)                                                   :: name
     character(len=4)                                                   :: center
     character(len=3)                                                   :: id
     character(len=1)                                                   :: depth
     character(len=1)                                                   :: latns
     character(len=1)                                                   :: lonew
     real(r_kind)                                                       :: area_mnlat
     real(r_kind)                                                       :: area_mnlon
     real(r_kind)                                                       :: area_mxlat
     real(r_kind)                                                       :: area_mxlon
     real(r_kind)                                                       :: lat
     real(r_kind)                                                       :: lon
     integer                                                            :: century
     integer                                                            :: hhmm
     integer                                                            :: lati
     integer                                                            :: loni
     integer                                                            :: pcen
     integer                                                            :: penv
     integer                                                            :: penvrad
     integer                                                            :: r15ne
     integer                                                            :: r15se
     integer                                                            :: r15sw
     integer                                                            :: r15nw
     integer                                                            :: stdir
     integer                                                            :: stspd
     integer                                                            :: vmax
     integer                                                            :: vmaxrad
     integer                                                            :: yymmdd
  end type tcv_struct             ! type tcv_struct
  type timeinfo_struct
     character(len=19)                                                  :: timestamp
     real(r_double)                                                     :: jday
     integer                                                            :: dd
     integer                                                            :: hh
     integer                                                            :: mm
     integer                                                            :: nn
     integer                                                            :: ss
     integer                                                            :: yy
  end type timeinfo_struct        ! type timeinfo_struct  
  type vargrid_struct
     real(r_kind),              dimension(:),               allocatable :: var
     real(r_kind)                                                       :: cutoff
     integer                                                            :: xmin
     integer                                                            :: xmax
     integer                                                            :: ymin
     integer                                                            :: ymax
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nvals
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
  type winds_struct
     real(r_kind),              dimension(:),               allocatable :: dir
     real(r_kind),              dimension(:),               allocatable :: spd
     real(r_kind),              dimension(:),               allocatable :: u
     real(r_kind),              dimension(:),               allocatable :: v
     integer                                                            :: ncoords
     integer                                                            :: nx
     integer                                                            :: ny
  end type winds_struct           ! type winds_struct  
  real(r_kind), parameter                                               :: clpval = tiny(0.0)
  real(r_kind), parameter                                               :: spval  = huge(0.0) 
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_fv3grid_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! fv3grid_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN fv3grid_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_fv3grid_struct(grid)

    ! Define variables passed to routine

    type(fv3grid_struct)                                                :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%pres))   deallocate(grid%pres)
    if(allocated(grid%ak))     deallocate(grid%ak)
    if(allocated(grid%bk))     deallocate(grid%bk)
    if(allocated(grid%lat))    deallocate(grid%lat)
    if(allocated(grid%lon))    deallocate(grid%lon)
    if(allocated(grid%mask))   deallocate(grid%mask)
    if(allocated(grid%radius)) deallocate(grid%radius)
    
    !=====================================================================

  end subroutine finalize_fv3grid_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_fv3var_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! fv3var_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN fv3var_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_fv3var_struct(grid)

    ! Define variables passed to routine

    type(fv3var_struct)                                                 :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%pres)) deallocate(grid%pres)
    if(allocated(grid%var))  deallocate(grid%var)

    !=====================================================================

  end subroutine finalize_fv3var_struct

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

    if(allocated(grid%pres)) deallocate(grid%pres)
    if(allocated(grid%var))  deallocate(grid%var)
    if(allocated(grid%lat))  deallocate(grid%lat)
    if(allocated(grid%lon))  deallocate(grid%lon)
    
    !=====================================================================

  end subroutine finalize_grid_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_interp_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! interp_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN interp_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_interp_struct(grid)

    ! Define variables passed to routine

    type(interp_struct)                                                 :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%slint_coeffs)) deallocate(grid%slint_coeffs)
    if(allocated(grid%slint_nn))     deallocate(grid%slint_nn)
    if(allocated(grid%var))          deallocate(grid%var)

    !=====================================================================

  end subroutine finalize_interp_struct
  
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

    if(allocated(grid%r2dist))   deallocate(grid%r2dist)
    if(allocated(grid%dst_lat))  deallocate(grid%dst_lat)
    if(allocated(grid%dst_lon))  deallocate(grid%dst_lon)
    if(allocated(grid%src_lat))  deallocate(grid%src_lat)
    if(allocated(grid%src_lon))  deallocate(grid%src_lon)
    if(allocated(grid%idx))      deallocate(grid%idx)
    
    !=====================================================================

  end subroutine finalize_kdtree_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_meteo_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! FORTRAN meteo_struct variable.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_meteo_struct(grid)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%u))    deallocate(grid%u)
    if(allocated(grid%v))    deallocate(grid%v)
    if(allocated(grid%wdir)) deallocate(grid%wdir)
    if(allocated(grid%wspd)) deallocate(grid%wspd)

    !=====================================================================

  end subroutine finalize_meteo_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! finalize_nems_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! nems_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN nems_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_nems_struct(grid)

    ! Define variables passed to routine

    type(nems_struct)                                                   :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%lat)) deallocate(grid%lat)
    if(allocated(grid%lon)) deallocate(grid%lon)
    if(allocated(grid%var)) deallocate(grid%var)
    
    !=====================================================================

  end subroutine finalize_nems_struct
  
  !=======================================================================

  ! SUBROUTINE:

  ! finalize_pattern_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocated memory for all arrays within the
  ! pattern_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN pattern_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_pattern_struct(grid)

    ! Define variables passed to routine

    type(pattern_struct)                                                :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%noise))     deallocate(grid%noise)
    if(allocated(grid%lat))       deallocate(grid%lat)
    if(allocated(grid%lon))       deallocate(grid%lon)
    if(allocated(grid%rnp))       deallocate(grid%rnp)
    if(allocated(grid%degree))    deallocate(grid%degree)
    if(allocated(grid%order))     deallocate(grid%order)
    if(allocated(grid%lap))       deallocate(grid%lap)
    if(allocated(grid%varspec))   deallocate(grid%varspec)
    if(allocated(grid%varspec1d)) deallocate(grid%varspec1d)

    !=====================================================================

  end subroutine finalize_pattern_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_remappres_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! remappres_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN remappres_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_remappres_struct(grid)

    ! Define variables passed to routine

    type(remappres_struct)                                              :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%pres)) deallocate(grid%pres)
    if(allocated(grid%psfc)) deallocate(grid%psfc)
    if(allocated(grid%ak))   deallocate(grid%ak)
    if(allocated(grid%bk))   deallocate(grid%bk)
    
    !=====================================================================

  end subroutine finalize_remappres_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_remapvar_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! remapvar_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN remapvar_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_remapvar_struct(grid)

    ! Define variables passed to routine

    type(remapvar_struct)                                               :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%fixed_var)) deallocate(grid%fixed_var)
    if(allocated(grid%remap_var)) deallocate(grid%remap_var)

    !=====================================================================

  end subroutine finalize_remapvar_struct

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

  ! finalize_svd_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! svd_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN svd_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_svd_struct(grid)

    ! Define variables passed to routine

    type(svd_struct)                                                    :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%a))      deallocate(grid%a)
    if(allocated(grid%u))      deallocate(grid%u)
    if(allocated(grid%v))      deallocate(grid%v)
    if(allocated(grid%vt))     deallocate(grid%vt)
    if(allocated(grid%s))      deallocate(grid%s)
    if(allocated(grid%svarex)) deallocate(grid%svarex)

    !=====================================================================

  end subroutine finalize_svd_struct

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

  ! finalize_winds_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! winds_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN winds_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_winds_struct(grid)

    ! Define variables passed to routine

    type(winds_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%dir)) deallocate(grid%dir)
    if(allocated(grid%spd)) deallocate(grid%spd)
    if(allocated(grid%u))   deallocate(grid%u)
    if(allocated(grid%v))   deallocate(grid%v)
    
    !=====================================================================

  end subroutine finalize_winds_struct
  
  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_fv3grid_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! fv3grid_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN fv3grid_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN fv3grid_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_fv3grid_struct(grid)

    ! Define variables passed to routine

    type(fv3grid_struct)                                                :: grid

    ! Define variables computed within routine

    integer                                                             :: ncoord

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)
    
    ! Allocate memory for local variables

    if(.not. allocated(grid%pres))                                         &
         & allocate(grid%pres(grid%ncoords,grid%nz))
    if(.not. allocated(grid%ak))                                           &
         & allocate(grid%ak(grid%nz))
    if(.not. allocated(grid%bk))                                           &
         & allocate(grid%bk(grid%nz))
    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%ncoords))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%ncoords))
    if(.not. allocated(grid%mask))                                         &
         & allocate(grid%mask(grid%ncoords))
    if(.not. allocated(grid%radius))                                       &
         & allocate(grid%radius(grid%ncoords))
    
    !=====================================================================

  end subroutine initialize_fv3grid_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_fv3var_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! fv3var_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN fv3var_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN fv3var_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_fv3var_struct(grid)

    ! Define variables passed to routine

    type(fv3var_struct)                                                 :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)
    
    ! Allocate memory for local variables

    if(.not. allocated(grid%pres))                                         &
         & allocate(grid%pres(grid%ncoords,grid%nz))
    if(.not. allocated(grid%var))                                          &
         & allocate(grid%var(grid%ncoords,grid%nz))

    !=====================================================================

  end subroutine initialize_fv3var_struct

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

    ! Allocate memory for local variables

    if(.not. allocated(grid%pres))                                         &
         & allocate(grid%pres(grid%ncoords,grid%nz))
    if(.not. allocated(grid%var))                                          &
         & allocate(grid%var(grid%ncoords,grid%nz))
    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%ncoords))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%ncoords))
    
    !=====================================================================

  end subroutine initialize_grid_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_interp_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! interp_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN interp_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN interp_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_interp_struct(grid)

    ! Define variables passed to routine

    type(interp_struct)                                                 :: grid

    !=====================================================================

    ! Define local variables

    if(is_slint) grid%ncoeffs = 3

    ! Allocate memory for local variables

    if(.not. allocated(grid%slint_coeffs))                                 &
         & allocate(grid%slint_coeffs(grid%ncoeffs,grid%ncoords))
    if(.not. allocated(grid%slint_nn))                                     &
         & allocate(grid%slint_nn(grid%ncoeffs,grid%ncoords))
    if(.not. allocated(grid%var))                                          &
         & allocate(grid%var(grid%ncoords))

    !=====================================================================
    
  end subroutine initialize_interp_struct  
  
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

    ! Define local variables

    grid%nalloc = grid%src_ncoords
    
    ! Allocate memory for local variables
  
    if(.not. allocated(grid%r2dist))                                       &
         & allocate(grid%r2dist(grid%src_ncoords,grid%dst_ncoords))
    if(.not. allocated(grid%idx))                                          &
         & allocate(grid%idx(grid%src_ncoords,grid%dst_ncoords))
    if(.not. allocated(grid%dst_lat))                                      &
         & allocate(grid%dst_lat(grid%dst_ncoords))
    if(.not. allocated(grid%dst_lon))                                      &
         & allocate(grid%dst_lon(grid%dst_ncoords))
    if(.not. allocated(grid%src_lat))                                      &
         & allocate(grid%src_lat(grid%src_ncoords))
    if(.not. allocated(grid%src_lon))                                      &
         & allocate(grid%src_lon(grid%src_ncoords))
    
    !=====================================================================

  end subroutine initialize_kdtree_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_meteo_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory and initializes (if applicable)
  ! all arrays within the FORTRAN meteo_struct variable.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable.
  
  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_meteo_struct(grid)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)
    
    ! Allocate memory for local variables

    if(.not. allocated(grid%u))                                            &
         & allocate(grid%u(grid%ncoords,grid%nz))
    if(.not. allocated(grid%v))                                            &
         & allocate(grid%v(grid%ncoords,grid%nz))
    if(.not. allocated(grid%wdir))                                         &
         & allocate(grid%wdir(grid%ncoords,grid%nz))
    if(.not. allocated(grid%wspd))                                         &
         & allocate(grid%wspd(grid%ncoords,grid%nz))

    ! Define local variables

    grid%u    = spval
    grid%v    = spval
    grid%wdir = spval
    grid%wspd = spval

    !=====================================================================

  end subroutine initialize_meteo_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_nems_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays and initializes
  ! all variables within the nems_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN nems_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN nems_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_nems_struct(grid)

    ! Define variables passed to routine

    type(nems_struct)                                                   :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%lat)) allocate(grid%lat(grid%ncoords))
    if(.not. allocated(grid%lon)) allocate(grid%lon(grid%ncoords))
    if(.not. allocated(grid%var)) allocate(grid%var(grid%ncoords))

    ! Define local variables

    grid%idate = -9999_nemsio_intkind

    !=====================================================================

  end subroutine initialize_nems_struct  

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_pattern_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! pattern_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN pattern_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN pattern_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_pattern_struct(grid)

    ! Define variables passed to routine

    type(pattern_struct)                                                :: grid

    !=====================================================================
    
    ! Define local variables

    grid%ndimspec = (grid%ntrunc + 1)*(grid%ntrunc + 2)/2
    
    ! Allocate memory for local variables

    if(.not. allocated(grid%noise))                                        &
         & allocate(grid%noise(grid%ndimspec))
    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%nx,grid%ny))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%nx,grid%ny))
    if(.not. allocated(grid%rnp))                                          &
         & allocate(grid%rnp(grid%nx,grid%ny))
    if(.not. allocated(grid%degree))                                       &
         & allocate(grid%degree(grid%ndimspec))
    if(.not. allocated(grid%order))                                        &
         & allocate(grid%order(grid%ndimspec))
    if(.not. allocated(grid%lap))                                          &
         & allocate(grid%lap(grid%ndimspec))    
    if(.not. allocated(grid%varspec))                                      &
         & allocate(grid%varspec(grid%ndimspec))
    if(.not. allocated(grid%varspec1d))                                    &
         & allocate(grid%varspec1d(0:grid%ntrunc))

    !=====================================================================

  end subroutine initialize_pattern_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_remappres_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! remappres_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN remappres_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN remappres_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_remappres_struct(grid)

    ! Define variables passed to routine

    type(remappres_struct)                                              :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%pres))                                         &
         & allocate(grid%pres(grid%ncoords,grid%nz))
    if(.not. allocated(grid%psfc))                                         &
         & allocate(grid%psfc(grid%ncoords))
    if(.not. allocated(grid%ak))                                           &
         & allocate(grid%ak(grid%nz))
    if(.not. allocated(grid%bk))                                           &
         & allocate(grid%bk(grid%nz))
    
    !=====================================================================

  end subroutine initialize_remappres_struct

  !=======================================================================

  ! SUBROUTINE: 

  ! initialize_remapvar_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! remapvar_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN remapvar_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN remapvar_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_remapvar_struct(grid)

    ! Define variables passed to routine

    type(remapvar_struct)                                               :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%fixed_var))                                    &
         & allocate(grid%fixed_var(grid%ncoords,grid%nz))
    if(.not. allocated(grid%remap_var))                                    &
         & allocate(grid%remap_var(grid%ncoords,grid%nz))
  
    !=====================================================================

  end subroutine initialize_remapvar_struct

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
  
  ! initialize_svd_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! svd_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN svd_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN svd_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_svd_struct(grid)

    ! Define variables passed to routine

    type(svd_struct)                                                    :: grid

    !=====================================================================

    ! Define local variables

    grid%lda  = grid%nx
    grid%ldu  = grid%nx
    grid%ldvt = grid%ny

    ! Allocate memory for local variables

    if(.not. allocated(grid%a))      allocate(grid%a(grid%lda,grid%ny))
    if(.not. allocated(grid%u))      allocate(grid%u(grid%ldu,grid%nx))
    if(.not. allocated(grid%v))      allocate(grid%v(grid%ny,grid%ldvt))
    if(.not. allocated(grid%vt))     allocate(grid%vt(grid%ldvt,grid%ny))
    if(.not. allocated(grid%s))      allocate(grid%s(grid%ny))
    if(.not. allocated(grid%svarex)) allocate(grid%svarex(grid%ny))

    ! Define local variables

    grid%u      = dble(0.0) 
    grid%v      = dble(0.0)
    grid%vt     = dble(0.0)
    grid%s      = dble(0.0)
    grid%svarex = dble(0.0)

    !=====================================================================

  end subroutine initialize_svd_struct

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

  ! initialize_winds_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! winds_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN winds_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN winds_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_winds_struct(grid)

    ! Define variables passed to routine

    type(winds_struct)                                                  :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)

    ! Allocate memory for local variables

    if(.not. allocated(grid%dir)) allocate(grid%dir(grid%ncoords))
    if(.not. allocated(grid%spd)) allocate(grid%spd(grid%ncoords))
    if(.not. allocated(grid%u))   allocate(grid%u(grid%ncoords))
    if(.not. allocated(grid%v))   allocate(grid%v(grid%ncoords))
    
    !=====================================================================

  end subroutine initialize_winds_struct

  !=======================================================================

  ! SUBROUTINE:

  ! pattern_vtable_lookup_nemsio.f90

  ! DESCRIPTION:

  ! This subroutine returns the NEMSIO file name corresponding to the
  ! json_pattern_struct variable name and level-type.

  ! INPUT VARIABLES:

  ! * json; a FORTRAN json_pattern_struct variable.

  ! * nemsio; a FORTRAN nems_struct variable.

  ! OUTPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable containing the variable
  !   name within the NEMSIO formatted file in accordance with the
  !   corresponding json_pattern_struct variable name.

  !-----------------------------------------------------------------------

  subroutine pattern_vtable_lookup_nemsio(json,nemsio)

    ! Define variables passed to routine

    type(json_pattern_struct)                                           :: json
    type(nems_struct)                                                   :: nemsio

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(json%levtype .eq. 3) then

       ! Check local variable and proceed accordingly

       if(trim(adjustl(json%variable_name)) .eq. 'clw') then

          ! Define local variables

          nemsio%varname = 'clwmr'
          nemsio%levtype = 'mid layer'

       end if ! if(trim(adjustl(json%variable_name)) .eq. 'clw')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(json%variable_name)) .eq. 'ozone') then

          ! Define local variables

          nemsio%varname = 'o3mr'
          nemsio%levtype = 'mid layer'

       end if ! if(trim(adjustl(json%variable_name)) .eq. 'ozone')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(json%variable_name)) .eq. 'q') then

          ! Define local variables

          nemsio%varname = 'spfh'
          nemsio%levtype = 'mid layer'

       end if ! if(trim(adjustl(json%variable_name)) .eq. 'q')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(json%variable_name)) .eq. 't') then

          ! Define local variables

          nemsio%varname = 'tmp'
          nemsio%levtype = 'mid layer'

       end if ! if(trim(adjustl(json%variable_name)) .eq. 't')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(json%variable_name)) .eq. 'uv') then

          ! Define local variables

          nemsio%levtype = 'mid layer'

       end if ! if(trim(adjustl(json%variable_name)) .eq. 'uv')

    end if ! if(json%levtype .eq. 3)

    !=====================================================================

  end subroutine pattern_vtable_lookup_nemsio  
  
  !=======================================================================
  
end module variable_interface
