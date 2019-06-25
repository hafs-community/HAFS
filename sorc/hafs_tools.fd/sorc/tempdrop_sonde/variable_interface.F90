module variable_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tempdrop-sonde :: variable_interface
  ! Copyright (C) 2017 Henry R. Winterbottom

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
  public :: grid_struct
  public :: hsa_struct
  public :: interp_p_struct
  public :: interp_spline_struct
  public :: meteo_struct
  public :: spval
  public :: statgrid_struct
  public :: variable_interface_cleanup_struct
  public :: variable_interface_setup_struct
  public :: varinfo_struct
  interface variable_interface_cleanup_struct
     module procedure finalize_hsa_struct
     module procedure finalize_interp_p_struct
     module procedure finalize_interp_spline_struct
     module procedure finalize_meteo_struct
     module procedure finalize_statgrid_struct
     module procedure finalize_varinfo_struct
  end interface variable_interface_cleanup_struct
  interface variable_interface_setup_struct
     module procedure initialize_hsa_struct
     module procedure initialize_interp_p_struct
     module procedure initialize_interp_spline_struct
     module procedure initialize_meteo_struct
     module procedure initialize_statgrid_struct
     module procedure initialize_varinfo_struct
  end interface variable_interface_setup_struct

  ! Define local variables

  real(r_kind), parameter                                               :: spval = huge(0.0)
  type grid_struct
     real(r_kind)                                                       :: lon
     real(r_kind)                                                       :: lat
     real(r_kind)                                                       :: dist
     real(r_kind)                                                       :: head
  end type grid_struct            ! type grid_struct
  type hsa_struct
     character(len=4),          dimension(:),               allocatable :: tail
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
     real(r_kind)                                                       :: psfc
     real(r_kind)                                                       :: rellon
     real(r_kind)                                                       :: rellat
     real(r_kind)                                                       :: spglon
     real(r_kind)                                                       :: spglat                                                     
     integer,                   dimension(:),               allocatable :: gmt
     integer,                   dimension(:),               allocatable :: wx
     integer                                                            :: yyyy
     integer                                                            :: mm
     integer                                                            :: dd
     integer                                                            :: logtime
     integer                                                            :: reltime
     integer                                                            :: spgtime
     integer                                                            :: nz
  end type hsa_struct             ! type hsa_struct
  type interp_spline_struct
     real(r_kind),              dimension(:),               allocatable :: xa
     real(r_kind),              dimension(:),               allocatable :: ya
     real(r_kind)                                                       :: x
     real(r_kind)                                                       :: y
     integer                                                            :: n    
  end type interp_spline_struct   ! type interp_spline_struct
  type interp_p_struct 
     real(r_kind),              dimension(:),               allocatable :: var
     real(r_kind),              dimension(:),               allocatable :: p
     real(r_kind)                                                       :: psfc
     integer                                                            :: nz
  end type interp_p_struct        ! type interp_p_struct  
  type meteo_struct
     character(len=500)                                                 :: tempdrop_name
     character(len=5)                                                   :: acid
     character(len=2)                                                   :: obnum
     real(r_double),            dimension(:),               allocatable :: dwpt
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
     real(r_double),            dimension(:),               allocatable :: z
     real(r_double),            dimension(:),               allocatable :: jdate
     real(r_kind),              dimension(:),               allocatable :: dist
     real(r_kind),              dimension(:),               allocatable :: head
     real(r_double)                                                     :: psfc
     integer                                                            :: nz
  end type meteo_struct           ! type meteo_struct
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

  !-----------------------------------------------------------------------

contains

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

    if(allocated(grid%dwpt))  deallocate(grid%dwpt)  
    if(allocated(grid%lat))   deallocate(grid%lat)
    if(allocated(grid%lon))   deallocate(grid%lon)
    if(allocated(grid%p))     deallocate(grid%p)
    if(allocated(grid%q))     deallocate(grid%q)
    if(allocated(grid%rh))    deallocate(grid%rh)
    if(allocated(grid%t))     deallocate(grid%t)
    if(allocated(grid%thta))  deallocate(grid%thta)
    if(allocated(grid%thte))  deallocate(grid%thte)
    if(allocated(grid%thtv))  deallocate(grid%thtv)
    if(allocated(grid%u))     deallocate(grid%u)
    if(allocated(grid%v))     deallocate(grid%v)
    if(allocated(grid%wdir))  deallocate(grid%wdir)
    if(allocated(grid%wspd))  deallocate(grid%wspd)
    if(allocated(grid%z))     deallocate(grid%z)
    if(allocated(grid%jdate)) deallocate(grid%jdate)

    !=====================================================================
    
  end subroutine finalize_meteo_struct

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
  
  ! initialize_hsa_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! hsa_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN hsa_struct variable.

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

    ! Allocate memory for local variables

    if(.not. allocated(grid%dwpt))  allocate(grid%dwpt(grid%nz))  
    if(.not. allocated(grid%lat))   allocate(grid%lat(grid%nz))
    if(.not. allocated(grid%lon))   allocate(grid%lon(grid%nz))
    if(.not. allocated(grid%p))     allocate(grid%p(grid%nz))
    if(.not. allocated(grid%q))     allocate(grid%q(grid%nz))
    if(.not. allocated(grid%rh))    allocate(grid%rh(grid%nz))
    if(.not. allocated(grid%t))     allocate(grid%t(grid%nz))
    if(.not. allocated(grid%thta))  allocate(grid%thta(grid%nz))
    if(.not. allocated(grid%thte))  allocate(grid%thte(grid%nz))
    if(.not. allocated(grid%thtv))  allocate(grid%thtv(grid%nz))
    if(.not. allocated(grid%u))     allocate(grid%u(grid%nz))
    if(.not. allocated(grid%v))     allocate(grid%v(grid%nz))
    if(.not. allocated(grid%wdir))  allocate(grid%wdir(grid%nz))
    if(.not. allocated(grid%wspd))  allocate(grid%wspd(grid%nz))
    if(.not. allocated(grid%z))     allocate(grid%z(grid%nz))
    if(.not. allocated(grid%jdate)) allocate(grid%jdate(grid%nz))
    
    ! Define local variables

    grid%dwpt  = spval
    grid%lat   = spval
    grid%lon   = spval
    grid%p     = spval
    grid%q     = spval
    grid%rh    = spval
    grid%t     = spval
    grid%thta  = spval
    grid%thte  = spval
    grid%thtv  = spval
    grid%u     = spval
    grid%v     = spval
    grid%wdir  = spval
    grid%wspd  = spval
    grid%z     = spval
    grid%jdate = spval
    grid%psfc  = spval

    !=====================================================================
    
  end subroutine initialize_meteo_struct

  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_statgrid_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! statgrid_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN statgrid_struct variable.

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
