module variable_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! da-utils :: variable_interface
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

  use kinds_interface
  use nemsio_module

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: bufr_spval
  public :: convbufr_struct
  public :: gsidiagisovar_struct
  public :: gsidiagprfvar_struct
  public :: json_nems_struct
  public :: nems_struct
  public :: obscode_spval
  public :: satbufr_struct
  public :: spval
  public :: statgrid_struct
  public :: vargrid_struct
  public :: variable_interface_cleanup_struct
  public :: variable_interface_setup_struct
  public :: varinfo_struct
  interface variable_interface_cleanup_struct
     module procedure finalize_convbufr_struct
     module procedure finalize_gsidiagisovar_struct
     module procedure finalize_gsidiagprfvar_struct
     module procedure finalize_nems_struct
     module procedure finalize_satbufr_struct
     module procedure finalize_vargrid_struct
     module procedure finalize_varinfo_struct
  end interface variable_interface_cleanup_struct
  interface variable_interface_setup_struct
     module procedure initialize_convbufr_struct
     module procedure initialize_gsidiagisovar_struct
     module procedure initialize_gsidiagprfvar_struct
     module procedure initialize_nems_struct
     module procedure initialize_satbufr_struct
     module procedure initialize_vargrid_struct
     module procedure initialize_varinfo_struct
  end interface variable_interface_setup_struct

  ! Define local variables

  real(r_double), parameter                                             :: bufr_spval = 10.e10
  real(r_kind),   parameter                                             :: spval = huge(0.0)
  integer                                                               :: obscode_spval = -9999
  type convbufr_struct
     character(len=500)                                                 :: bufr_filename
     character(len=19)                                                  :: analdate
     real(r_kind),              dimension(:),               allocatable :: elev
     real(r_kind),              dimension(:),               allocatable :: offset_seconds
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: subtype
     real(r_kind)                                                       :: obstype
     integer                                                            :: idate
     integer                                                            :: nrecs
  end type convbufr_struct        ! type convbufr_struct
  type gsidiagisovar_struct
     character(len=500)                                                 :: gsidiag_filename
     character(len=10)                                                  :: analdate
     character(len=8)                                                   :: sid
     character(len=3)                                                   :: obstype
     real(r_kind),              dimension(:),               allocatable :: hgt
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: obs
     real(r_kind),              dimension(:),               allocatable :: omf
     real(r_kind),              dimension(:),               allocatable :: pres
     real(r_kind),              dimension(:),               allocatable :: time
     real(r_kind)                                                       :: bias
     real(r_kind)                                                       :: mean
     real(r_kind)                                                       :: rmse
     real(r_kind)                                                       :: vari
     integer,                   dimension(:),               allocatable :: code
     integer                                                            :: ncount
     integer                                                            :: nobs
     integer                                                            :: obscode
  end type gsidiagisovar_struct   ! type gsidiagisovar_struct
  type gsidiagprfvar_struct
     character(len=500)                                                 :: gsidiag_filename
     character(len=10)                                                  :: analdate
     character(len=8)                                                   :: sid
     character(len=3)                                                   :: obstype
     real(r_kind),              dimension(:,:),             allocatable :: hgt
     real(r_kind),              dimension(:,:),             allocatable :: lat
     real(r_kind),              dimension(:,:),             allocatable :: lon
     real(r_kind),              dimension(:,:),             allocatable :: obs
     real(r_kind),              dimension(:,:),             allocatable :: omf
     real(r_kind),              dimension(:,:),             allocatable :: pres
     real(r_kind),              dimension(:,:),             allocatable :: time
     real(r_kind),              dimension(:),               allocatable :: bias
     real(r_kind),              dimension(:),               allocatable :: mean
     real(r_kind),              dimension(:),               allocatable :: plevs
     real(r_kind),              dimension(:),               allocatable :: rmse
     real(r_kind),              dimension(:),               allocatable :: vari
     integer,                   dimension(:,:),             allocatable :: code
     integer,                   dimension(:),               allocatable :: ncount
     integer                                                            :: nlevs
     integer                                                            :: nobs
     integer                                                            :: obscode
  end type gsidiagprfvar_struct   ! type gsidiagprfvar_struct
  type json_nems_struct
     character(nemsio_charkind)                                         :: levtyp
     character(len=25)                                                  :: nems_name
     character(len=25)                                                  :: ncdf_name
     character(len=25)                                                  :: title
     character(len=25)                                                  :: units
     logical                                                            :: prfvar
     logical                                                            :: sfcvar
  end type json_nems_struct       ! type json_nems_struct
  type nems_struct
     character(nemsio_charkind)                                         :: varname
     real(nemsio_realkind),     dimension(:,:),             allocatable :: infcprs
     real(nemsio_realkind),     dimension(:,:),             allocatable :: mdlyprs
     real(nemsio_realkind),     dimension(:,:),             allocatable :: prfvar
     real(nemsio_realkind),     dimension(:),               allocatable :: ak
     real(nemsio_realkind),     dimension(:),               allocatable :: bk
     real(nemsio_realkind),     dimension(:),               allocatable :: ck
     real(nemsio_realkind),     dimension(:),               allocatable :: lat
     real(nemsio_realkind),     dimension(:),               allocatable :: lon
     real(nemsio_realkind),     dimension(:),               allocatable :: orog
     real(nemsio_realkind),     dimension(:),               allocatable :: psfc
     real(nemsio_realkind),     dimension(:),               allocatable :: sfcvar
     integer(nemsio_intkind)                                            :: idate(7)
     integer(nemsio_intkind)                                            :: jcap
     integer(nemsio_intkind)                                            :: lev
     integer(nemsio_intkind)                                            :: nx
     integer(nemsio_intkind)                                            :: ny
     integer(nemsio_intkind)                                            :: nz
     integer                                                            :: ncoords
     integer                                                            :: nvars
  end type nems_struct            ! type nems_struct
  type satbufr_struct
     character(len=500)                                                 :: bufr_filename
     character(len=19)                                                  :: analdate
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     real(r_kind),              dimension(:),               allocatable :: offset_seconds
     real(r_kind),              dimension(:),               allocatable :: said
     real(r_kind),              dimension(:),               allocatable :: usaids
     integer                                                            :: idate
     integer                                                            :: nrecs
     integer                                                            :: nsats
  end type satbufr_struct         ! type satbufr_struct
  type statgrid_struct
     real(r_kind)                                                       :: bias
     real(r_kind)                                                       :: mean
     real(r_kind)                                                       :: rmse
     real(r_kind)                                                       :: stdev
     real(r_kind)                                                       :: vari
     integer                                                            :: nvals
  end type statgrid_struct        ! type statgrid_struct
  type vargrid_struct
     real(r_kind),              dimension(:),               allocatable :: var
     integer                                                            :: nvals
  end type vargrid_struct         ! type vargrid_struct
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

  ! finalize_convbufr_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! convbufr_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN convbufr_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_convbufr_struct(grid)

    ! Define variables passed to routine

    type(convbufr_struct)                                               :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%elev))           deallocate(grid%elev)
    if(allocated(grid%lat))            deallocate(grid%lat)
    if(allocated(grid%lon))            deallocate(grid%lon)
    if(allocated(grid%offset_seconds)) deallocate(grid%offset_seconds)
    if(allocated(grid%subtype))        deallocate(grid%subtype)

    !=====================================================================

  end subroutine finalize_convbufr_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_gsidiagisovar_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! gsidiagisovar_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN gsidiagisovar_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_gsidiagisovar_struct(grid)

    ! Define variables passed to routine

    type(gsidiagisovar_struct)                                          :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%hgt))  deallocate(grid%hgt)
    if(allocated(grid%lat))  deallocate(grid%lat)
    if(allocated(grid%lon))  deallocate(grid%lon)
    if(allocated(grid%obs))  deallocate(grid%obs)
    if(allocated(grid%omf))  deallocate(grid%omf)
    if(allocated(grid%pres)) deallocate(grid%pres)
    if(allocated(grid%time)) deallocate(grid%time)
    if(allocated(grid%code)) deallocate(grid%code)

    !=====================================================================

  end subroutine finalize_gsidiagisovar_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_gsidiagprfvar_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! gsidiagprfvar_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN gsidiagprfvar_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_gsidiagprfvar_struct(grid)

    ! Define variables passed to routine

    type(gsidiagprfvar_struct)                                          :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%hgt))  deallocate(grid%hgt)
    if(allocated(grid%lat))    deallocate(grid%lat)
    if(allocated(grid%lon))    deallocate(grid%lon)
    if(allocated(grid%obs))    deallocate(grid%obs)
    if(allocated(grid%omf))    deallocate(grid%omf)
    if(allocated(grid%pres))   deallocate(grid%pres)
    if(allocated(grid%time))   deallocate(grid%time)
    if(allocated(grid%bias))   deallocate(grid%bias)
    if(allocated(grid%mean))   deallocate(grid%mean)
    if(allocated(grid%plevs))  deallocate(grid%plevs)
    if(allocated(grid%rmse))   deallocate(grid%rmse)
    if(allocated(grid%vari))   deallocate(grid%vari)
    if(allocated(grid%code))   deallocate(grid%code)
    if(allocated(grid%ncount)) deallocate(grid%ncount)

    !=====================================================================

  end subroutine finalize_gsidiagprfvar_struct

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

    if(allocated(grid%infcprs)) deallocate(grid%infcprs)
    if(allocated(grid%mdlyprs)) deallocate(grid%mdlyprs)
    if(allocated(grid%prfvar))  deallocate(grid%prfvar)
    if(allocated(grid%ak))      deallocate(grid%ak)
    if(allocated(grid%bk))      deallocate(grid%bk)
    if(allocated(grid%ck))      deallocate(grid%ck)
    if(allocated(grid%lat))     deallocate(grid%lat)
    if(allocated(grid%lon))     deallocate(grid%lon)
    if(allocated(grid%orog))    deallocate(grid%orog)
    if(allocated(grid%psfc))    deallocate(grid%psfc)
    if(allocated(grid%sfcvar))  deallocate(grid%sfcvar)
    
    !=====================================================================

  end subroutine finalize_nems_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_satbufr_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! satbufr_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN satbufr_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_satbufr_struct(grid)

    ! Define variables passed to routine

    type(satbufr_struct)                                                :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%lat))            deallocate(grid%lat)
    if(allocated(grid%lon))            deallocate(grid%lon)
    if(allocated(grid%offset_seconds)) deallocate(grid%offset_seconds)
    if(allocated(grid%said))           deallocate(grid%said)
    if(allocated(grid%usaids))         deallocate(grid%usaids)

    !=====================================================================

  end subroutine finalize_satbufr_struct

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

  ! initialize_convbufr_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! convbufr_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN convbufr_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN convbufr_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_convbufr_struct(grid)

    ! Define variables passed to routine

    type(convbufr_struct)                                               :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(.not. allocated(grid%elev))                                         &
         & allocate(grid%elev(grid%nrecs))
    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%nrecs))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%nrecs))
    if(.not. allocated(grid%offset_seconds))                               &
         & allocate(grid%offset_seconds(grid%nrecs))
    if(.not. allocated(grid%subtype))                                      &
         & allocate(grid%subtype(grid%nrecs))

    !=====================================================================

  end subroutine initialize_convbufr_struct

  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_gsidiagisovar_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! gsidiagisovar_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN gsidiagisovar_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN gsidiagisovar_struct variable where all arrays
  !   are allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_gsidiagisovar_struct(grid)

    ! Define variables passed to routine

    type(gsidiagisovar_struct)                                          :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%hgt))  allocate(grid%hgt(grid%nobs))
    if(.not. allocated(grid%lat))  allocate(grid%lat(grid%nobs))
    if(.not. allocated(grid%lon))  allocate(grid%lon(grid%nobs))
    if(.not. allocated(grid%obs))  allocate(grid%obs(grid%nobs))
    if(.not. allocated(grid%omf))  allocate(grid%omf(grid%nobs))
    if(.not. allocated(grid%pres)) allocate(grid%pres(grid%nobs))
    if(.not. allocated(grid%time)) allocate(grid%time(grid%nobs))
    if(.not. allocated(grid%code)) allocate(grid%code(grid%nobs))

    ! Define local variables

    grid%hgt     = spval
    grid%lat     = spval
    grid%lon     = spval
    grid%obs     = spval
    grid%omf     = spval
    grid%pres    = spval
    grid%time    = spval
    grid%bias    = spval
    grid%mean    = spval
    grid%rmse    = spval
    grid%vari    = spval
    grid%code    = -9999
    grid%ncount  = 0

    !=====================================================================

  end subroutine initialize_gsidiagisovar_struct

  !=======================================================================

  ! SUBROUTINE: 
  
  ! initialize_gsidiagprfvar_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! gsidiagprfvar_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN gsidiagprfvar_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN gsidiagprfvar_struct variable where all arrays
  !   are allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_gsidiagprfvar_struct(grid)

    ! Define variables passed to routine

    type(gsidiagprfvar_struct)                                          :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%hgt))                                          &
         & allocate(grid%hgt(grid%nobs,grid%nlevs))
    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%nobs,grid%nlevs))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%nobs,grid%nlevs))
    if(.not. allocated(grid%obs))                                          &
         & allocate(grid%obs(grid%nobs,grid%nlevs))
    if(.not. allocated(grid%omf))                                          &
         & allocate(grid%omf(grid%nobs,grid%nlevs))
    if(.not. allocated(grid%pres))                                         &
         & allocate(grid%pres(grid%nobs,grid%nlevs))
    if(.not. allocated(grid%time))                                         &
         & allocate(grid%time(grid%nobs,grid%nlevs))
    if(.not. allocated(grid%bias))                                         &
         & allocate(grid%bias(grid%nlevs))
    if(.not. allocated(grid%mean))                                         &
         & allocate(grid%mean(grid%nlevs))
    if(.not. allocated(grid%plevs))                                        &
         & allocate(grid%plevs(grid%nlevs))
    if(.not. allocated(grid%rmse))                                         &
         & allocate(grid%rmse(grid%nlevs))
    if(.not. allocated(grid%vari))                                         &
         & allocate(grid%vari(grid%nlevs))
    if(.not. allocated(grid%code))                                         &
         & allocate(grid%code(grid%nobs,grid%nlevs))
    if(.not. allocated(grid%ncount))                                       &
         & allocate(grid%ncount(grid%nlevs))

    ! Define local variables

    grid%hgt     = spval
    grid%lat     = spval
    grid%lon     = spval
    grid%obs     = spval
    grid%omf     = spval
    grid%pres    = spval
    grid%time    = spval
    grid%bias    = spval
    grid%mean    = spval
    grid%rmse    = spval
    grid%vari    = spval
    grid%code    = -9999
    grid%ncount  = 0

    !=====================================================================

  end subroutine initialize_gsidiagprfvar_struct

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

    if(.not. allocated(grid%infcprs))                                      &
         & allocate(grid%infcprs(grid%ncoords,(grid%nz + 1)))
    if(.not. allocated(grid%mdlyprs))                                      &
         & allocate(grid%mdlyprs(grid%ncoords,grid%nz))
    if(.not. allocated(grid%prfvar))                                       &
         & allocate(grid%prfvar(grid%ncoords,grid%nz))
    if(.not. allocated(grid%ak))                                           &
         & allocate(grid%ak((grid%nz + 1)))
    if(.not. allocated(grid%bk))                                           &
         & allocate(grid%bk((grid%nz + 1)))
    if(.not. allocated(grid%ck))                                           &
         & allocate(grid%ck((grid%nz + 1)))
    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%ncoords))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%ncoords))
    if(.not. allocated(grid%orog))                                         &
         & allocate(grid%orog(grid%ncoords))
    if(.not. allocated(grid%psfc))                                         &
         & allocate(grid%psfc(grid%ncoords))
    if(.not. allocated(grid%sfcvar))                                       &
         & allocate(grid%sfcvar(grid%ncoords))
    
    !=====================================================================

  end subroutine initialize_nems_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_satbufr_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! satbufr_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN satbufr_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN satbufr_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_satbufr_struct(grid)

    ! Define variables passed to routine

    type(satbufr_struct)                                                :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%nrecs))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%nrecs))
    if(.not. allocated(grid%offset_seconds))                               &
         & allocate(grid%offset_seconds(grid%nrecs))
    if(.not. allocated(grid%said))                                         &
         & allocate(grid%said(grid%nrecs))
    if(.not. allocated(grid%usaids))                                       &
         & allocate(grid%usaids(grid%nsats))

    ! Define local variables

    grid%usaids = spval

    !=====================================================================

  end subroutine initialize_satbufr_struct

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

end module variable_interface
