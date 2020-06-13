module variable_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! post-utils :: variables_interface
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
  public :: fv3_json_glbl_struct
  public :: fv3_json_rgnl_struct
  public :: grid_struct
  public :: nemsiometa_struct
  public :: nemsiovar_struct
  public :: netcdf_var_struct
  public :: remap_struct
  public :: slint_struct
  public :: spval
  public :: upp_fv3_glblgrd_struct
  public :: upp_fv3_rgnlgrd_struct
  public :: upp_spval
  public :: variable_interface_cleanup_struct
  public :: variable_interface_setup_struct
  public :: varinfo_struct
  interface variable_interface_cleanup_struct
     module procedure finalize_grid_struct
     module procedure finalize_nemsiometa_struct
     module procedure finalize_nemsiovar_struct
     module procedure finalize_netcdf_var_struct
     module procedure finalize_remap_struct
     module procedure finalize_slint_struct
     module procedure finalize_upp_fv3_glblgrd_struct
     module procedure finalize_upp_fv3_rgnlgrd_struct
     module procedure finalize_varinfo_struct
  end interface variable_interface_cleanup_struct
  interface variable_interface_setup_struct
     module procedure initialize_grid_struct
     module procedure initialize_nemsiometa_struct
     module procedure initialize_nemsiovar_struct
     module procedure initialize_netcdf_var_struct
     module procedure initialize_remap_struct
     module procedure initialize_slint_struct
     module procedure initialize_upp_fv3_glblgrd_struct
     module procedure initialize_upp_fv3_rgnlgrd_struct
     module procedure initialize_varinfo_struct
  end interface variable_interface_setup_struct

  ! Define local variables

  real(r_kind), parameter                                               :: nems_spval = -1.e10
  real(r_kind), parameter                                               :: spval      = huge(0.0)
  real(r_kind), parameter                                               :: upp_spval  = -1.0e10
  type fv3_json_glbl_struct
     character(len=25)                                                  :: nems_variable_name
     character(len=25)                                                  :: nems_vcoord_name
     character(len=25)                                                  :: variable_name
     logical                                                            :: is_z_staggered
     integer                                                            :: filetype
     integer                                                            :: intrptype
     integer                                                            :: levtype
  end type fv3_json_glbl_struct   ! type fv3_json_glbl_struct
  type fv3_json_rgnl_struct
     character(len=25)                                                  :: remap_variable_name
     character(len=25)                                                  :: units
     character(len=25)                                                  :: variable_name
     logical                                                            :: is_z_staggered
     real(r_kind)                                                       :: fill_value
     integer                                                            :: intrptype
     integer                                                            :: levtype
  end type fv3_json_rgnl_struct   ! type fv3_json_rgnl_struct
  type grid_struct
     real(r_kind),              dimension(:),               allocatable :: var
     real(r_kind),              dimension(:),               allocatable :: lat
     real(r_kind),              dimension(:),               allocatable :: lon
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: ncoords
  end type grid_struct            ! type grid_struct
  type nemsiometa_struct
     type(nemsio_gfile)                                                 :: gfile
     character(len=500)                                                 :: filename    
     character(nemsio_charkind),dimension(:),               allocatable :: recname	     
     character(nemsio_charkind),dimension(:),               allocatable :: reclevtyp
     character(nemsio_charkind),dimension(:),               allocatable :: aryiname  
     character(16),             dimension(:),               allocatable :: variname 
     character(16),             dimension(:),               allocatable :: varrname
     character(nemsio_charkind8)                                        :: gdatatype      
     character(nemsio_charkind8)                                        :: modelname
     real(nemsio_realkind),     dimension(:,:,:),           allocatable :: vcoord
     real(nemsio_realkind)                                              :: rlon_min	     
     real(nemsio_realkind)                                              :: rlon_max	     
     real(nemsio_realkind)                                              :: rlat_min	     
     real(nemsio_realkind)                                              :: rlat_max	     
     real(nemsio_realkind),     dimension(:),               allocatable :: lon     
     real(nemsio_realkind),     dimension(:),               allocatable :: lat
     real(nemsio_realkind),     dimension(:),               allocatable :: varrval
     integer(nemsio_intkind),   dimension(:,:),             allocatable :: aryival
     integer(nemsio_intkind),   dimension(:),               allocatable :: aryilen	     
     integer(nemsio_intkind),   dimension(:),               allocatable :: reclev	     
     integer(nemsio_intkind),   dimension(:),               allocatable :: varival
     integer(nemsio_intkind)                                            :: idate(7)
     integer(nemsio_intkind)                                            :: dimx     
     integer(nemsio_intkind)                                            :: dimy     
     integer(nemsio_intkind)                                            :: dimz
     integer(nemsio_intkind)                                            :: fhour
     integer(nemsio_intkind)                                            :: idrt
     integer(nemsio_intkind)                                            :: idsl        
     integer(nemsio_intkind)                                            :: idvc     
     integer(nemsio_intkind)                                            :: idvm     
     integer(nemsio_intkind)                                            :: ncldt  
     integer(nemsio_intkind)                                            :: nreo_vc	     
     integer(nemsio_intkind)                                            :: nrec	     
     integer(nemsio_intkind)                                            :: nmeta	     
     integer(nemsio_intkind)                                            :: nmetavari      
     integer(nemsio_intkind)                                            :: nmetaaryi      
     integer(nemsio_intkind)                                            :: nmetavarr
     integer(nemsio_intkind)                                            :: nfhour	     
     integer(nemsio_intkind)                                            :: nfminute
     integer(nemsio_intkind)                                            :: nframe  
     integer(nemsio_intkind)                                            :: nfsecondd 
     integer(nemsio_intkind)                                            :: nfsecondn      
     integer(nemsio_intkind)                                            :: nsoil      
     integer(nemsio_intkind)                                            :: ntrac 
     integer(nemsio_intkind)                                            :: version
     integer                                                            :: ncoords
     integer                                                            :: ntrunc
  end type nemsiometa_struct      ! type nemsiometa_struct
  type nemsiovar_struct
     character(len=25)                                                  :: nems_variable_name
     character(len=25)                                                  :: nems_vcoord_name
     real(nemsio_realkind),     dimension(:),               allocatable :: var
     integer(nemsio_intkind)                                            :: level
     integer                                                            :: ncoords
  end type nemsiovar_struct       ! type nemsiovar_struct
  type netcdf_var_struct
     character(len=20)                                                  :: name
     real(r_kind),              dimension(:,:,:,:),         allocatable :: var
     integer                                                            :: nt
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz
  end type netcdf_var_struct      ! type netcdf_var_struct
  type remap_struct
     logical                                                            :: is_global
     logical                                                            :: is_regional
     real(r_kind),              dimension(:),               allocatable :: lats
     real(r_kind),              dimension(:),               allocatable :: lons
     real(r_kind)                                                       :: cen_lat
     real(r_kind)                                                       :: cen_lon
     real(r_kind)                                                       :: dlat
     real(r_kind)                                                       :: dlon
     real(r_kind)                                                       :: lat_max
     real(r_kind)                                                       :: lat_min
     real(r_kind)                                                       :: lat1
     real(r_kind)                                                       :: lat2
     real(r_kind)                                                       :: lon_max
     real(r_kind)                                                       :: lon_min
     real(r_kind)                                                       :: lon1
     real(r_kind)                                                       :: lon2
     integer                                                            :: jcap
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: ncoords
  end type remap_struct           ! type remap_struct
  type slint_struct
     real(r_double),            dimension(:,:),             allocatable :: coeffs
     real(r_kind),              dimension(:),               allocatable :: dst_lon
     real(r_kind),              dimension(:),               allocatable :: dst_lat
     real(r_kind),              dimension(:),               allocatable :: mask
     real(r_kind),              dimension(:),               allocatable :: src_lon
     real(r_kind),              dimension(:),               allocatable :: src_lat
     real(r_kind),              dimension(:),               allocatable :: var
     integer,                   dimension(:,:),             allocatable :: nn
     integer                                                            :: ncoeffs
     integer                                                            :: dst_ncoords
     integer                                                            :: dst_nx
     integer                                                            :: dst_ny
     integer                                                            :: src_ncoords
  end type slint_struct           ! type slint_struct
  type upp_fv3_glblgrd_struct
     real(r_kind),              dimension(:),               allocatable :: ak
     real(r_kind),              dimension(:),               allocatable :: bk
     integer                                                            :: nt
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz
  end type upp_fv3_glblgrd_struct ! type upp_fv3_glblgrd_struct
  type upp_fv3_rgnlgrd_struct
     real(r_double),            dimension(:,:),             allocatable :: grid_xt 
     real(r_double),            dimension(:,:),             allocatable :: grid_yt
     real(r_double),            dimension(:),               allocatable :: pfull
     real(r_double),            dimension(:),               allocatable :: phalf
     real(r_double),            dimension(:),               allocatable :: time
     real(r_kind),              dimension(:),               allocatable :: ak
     real(r_kind),              dimension(:),               allocatable :: bk
     integer                                                            :: nt
     integer                                                            :: nx
     integer                                                            :: ny
     integer                                                            :: nz
  end type upp_fv3_rgnlgrd_struct ! type upp_fv3_rgnlgrd_struct
  type varinfo_struct
     character(len=500),        dimension(:,:,:),           allocatable :: varattrs
     character(len=25),         dimension(:),               allocatable :: varname
     character(len=15),         dimension(:),               allocatable :: dimname
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

    if(allocated(grid%lat)) deallocate(grid%lat)
    if(allocated(grid%lon)) deallocate(grid%lon)
    if(allocated(grid%var)) deallocate(grid%var)

    !=====================================================================

  end subroutine finalize_grid_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_nemsiometa_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! nemsiometa_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN nemsiometa_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_nemsiometa_struct(grid)

    ! Define variables passed to routine

    type(nemsiometa_struct)                                             :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%recname))   deallocate(grid%recname)
    if(allocated(grid%reclevtyp)) deallocate(grid%reclevtyp)    
    if(allocated(grid%aryiname))  deallocate(grid%aryiname)
    if(allocated(grid%variname))  deallocate(grid%variname)
    if(allocated(grid%varrname))  deallocate(grid%varrname)
    if(allocated(grid%vcoord))    deallocate(grid%vcoord)
    if(allocated(grid%lat))       deallocate(grid%lat)
    if(allocated(grid%lon))       deallocate(grid%lon)
    if(allocated(grid%varrval))   deallocate(grid%varrval)
    if(allocated(grid%aryival))   deallocate(grid%aryival)
    if(allocated(grid%aryilen))   deallocate(grid%aryilen)
    if(allocated(grid%reclev))    deallocate(grid%reclev)
    if(allocated(grid%varival))   deallocate(grid%varival)
    
    !=====================================================================

  end subroutine finalize_nemsiometa_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_nemsiovar_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! nemsiovar_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN nemsiovar_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_nemsiovar_struct(grid)

    ! Define variables passed to routine

    type(nemsiovar_struct)                                              :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%var)) deallocate(grid%var)

    !=====================================================================

  end subroutine finalize_nemsiovar_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_netcdf_var_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! netcdf_var_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN netcdf_var_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_netcdf_var_struct(grid)

    ! Define variables passed to routine

    type(netcdf_var_struct)                                             :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%var)) deallocate(grid%var)

    !=====================================================================

  end subroutine finalize_netcdf_var_struct

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

    ! Define variables passed to routine

    type(remap_struct)                                                  :: grid

    !=====================================================================

    ! Deallocate memory for local variables

    if(allocated(grid%lats)) deallocate(grid%lats)
    if(allocated(grid%lons)) deallocate(grid%lons)

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

    if(allocated(grid%coeffs))  deallocate(grid%coeffs)
    if(allocated(grid%nn))      deallocate(grid%nn)
    if(allocated(grid%dst_lon)) deallocate(grid%dst_lon)
    if(allocated(grid%dst_lat)) deallocate(grid%dst_lat)
    if(allocated(grid%mask))    deallocate(grid%mask)
    if(allocated(grid%src_lon)) deallocate(grid%src_lon)
    if(allocated(grid%src_lat)) deallocate(grid%src_lat)
    if(allocated(grid%var))     deallocate(grid%var)

    !=====================================================================

  end subroutine finalize_slint_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_upp_fv3_glblgrd_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! upp_fv3_glblgrd_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN upp_fv3_glblgrd_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_upp_fv3_glblgrd_struct(grid)

    ! Define variables passed to routine

    type(upp_fv3_glblgrd_struct)                                        :: grid

    !=====================================================================
    
    ! Deallocate memory for local variables

    if(allocated(grid%ak)) deallocate(grid%ak)
    if(allocated(grid%bk)) deallocate(grid%bk)

    !=====================================================================

  end subroutine finalize_upp_fv3_glblgrd_struct

  !=======================================================================

  ! SUBROUTINE:

  ! finalize_upp_fv3_rgnlgrd_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! upp_fv3_rgnlgrd_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN upp_fv3_rgnlgrd_struct variable.

  !-----------------------------------------------------------------------

  subroutine finalize_upp_fv3_rgnlgrd_struct(grid)

    ! Define variables passed to routine

    type(upp_fv3_rgnlgrd_struct)                                        :: grid

    !=====================================================================
    
    ! Deallocate memory for local variables

    if(allocated(grid%grid_xt)) deallocate(grid%grid_xt)
    if(allocated(grid%grid_yt)) deallocate(grid%grid_yt)
    if(allocated(grid%pfull))   deallocate(grid%pfull)
    if(allocated(grid%phalf))   deallocate(grid%phalf)
    if(allocated(grid%time))    deallocate(grid%time)
    if(allocated(grid%ak))      deallocate(grid%ak)
    if(allocated(grid%bk))      deallocate(grid%bk)

    !=====================================================================

  end subroutine finalize_upp_fv3_rgnlgrd_struct

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
    if(.not. allocated(grid%var)) allocate(grid%var(grid%ncoords))

    !=====================================================================

  end subroutine initialize_grid_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_nemsiometa_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! nemsiometa_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN nemsiometa_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN nemsiometa_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_nemsiometa_struct(grid)

    ! Define variables passed to routine

    type(nemsiometa_struct)                                             :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%dimx*grid%dimy)
    
    ! Allocate memory for local variables

    if(.not. allocated(grid%recname))                                      &
         & allocate(grid%recname(grid%nrec))
    if(.not. allocated(grid%reclevtyp))                                    &
         & allocate(grid%reclevtyp(grid%nrec))    
    if(.not. allocated(grid%aryiname))                                     &
         & allocate(grid%aryiname(grid%nmetaaryi))
    if(.not. allocated(grid%variname))                                     &
         & allocate(grid%variname(grid%nmetavari))
    if(.not. allocated(grid%varrname))                                     &
         & allocate(grid%varrname(grid%nmetavarr))
    if(.not. allocated(grid%vcoord))                                       &
         & allocate(grid%vcoord((grid%dimz + 1),3,2))
    if(.not. allocated(grid%lat))                                          &
         & allocate(grid%lat(grid%ncoords))
    if(.not. allocated(grid%lon))                                          &
         & allocate(grid%lon(grid%ncoords))
    if(.not. allocated(grid%varrval))                                      &
         & allocate(grid%varrval(grid%nmetavarr))
    if(.not. allocated(grid%aryival))                                      &
         & allocate(grid%aryival(grid%dimy/2,grid%nmetaaryi))
    if(.not. allocated(grid%aryilen))                                      &
         & allocate(grid%aryilen(grid%nmetaaryi))
    if(.not. allocated(grid%reclev))                                       &
         & allocate(grid%reclev(grid%nrec))
    if(.not. allocated(grid%varival))                                      &
         & allocate(grid%varival(grid%nmetavari))
    
    !=====================================================================

  end subroutine initialize_nemsiometa_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_nemsiovar_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! nemsiovar_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN nemsiovar_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN nemsiovar_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_nemsiovar_struct(grid)

    ! Define variables passed to routine

    type(nemsiovar_struct)                                              :: grid

    !=====================================================================  

    ! Allocate memory for local variables

    if(.not. allocated(grid%var)) allocate(grid%var(grid%ncoords))

    !=====================================================================

  end subroutine initialize_nemsiovar_struct
    
  !=======================================================================

  ! SUBROUTINE:

  ! initialize_netcdf_var_struct.f90

  ! This subroutine allocates memory for all arrays within the
  ! netcdf_var_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN netcdf_var_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN netcdf_var_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_netcdf_var_struct(grid)

    ! Define variables passed to routine

    type(netcdf_var_struct)                                             :: grid

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(grid%var))                                          &
         & allocate(grid%var(grid%nx,grid%ny,grid%nz,grid%nt))

    !=====================================================================

  end subroutine initialize_netcdf_var_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_remap_struct.f90

  ! DESCRIPTION:

  ! This subroutine allocates memory for all arrays within the
  ! remap_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN remap_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN remap_struct variable where all arrays are
  !   allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_remap_struct(grid)

    ! Define variables passed to routine

    type(remap_struct)                                                  :: grid

    !=====================================================================

    ! Define local variables

    grid%ncoords = (grid%nx*grid%ny)

    ! Deallocate memory for local variables

    if(.not. allocated(grid%lats)) allocate(grid%lats(grid%ncoords))
    if(.not. allocated(grid%lons)) allocate(grid%lons(grid%ncoords))

    ! Define local variables

    grid%lats = spval
    grid%lons = spval

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
         & allocate(grid%coeffs(grid%ncoeffs,grid%dst_ncoords))
    if(.not. allocated(grid%nn))                                           &
         & allocate(grid%nn(grid%ncoeffs,grid%dst_ncoords))
    if(.not. allocated(grid%dst_lon))                                      &
         & allocate(grid%dst_lon(grid%dst_ncoords))
    if(.not. allocated(grid%dst_lat))                                      &
         & allocate(grid%dst_lat(grid%dst_ncoords))
    if(.not. allocated(grid%mask))                                         &
         & allocate(grid%mask(grid%dst_ncoords))
    if(.not. allocated(grid%src_lon))                                      &
         & allocate(grid%src_lon(grid%src_ncoords))
    if(.not. allocated(grid%src_lat))                                      &
         & allocate(grid%src_lat(grid%src_ncoords))
    if(.not. allocated(grid%var))                                          &
         & allocate(grid%var(grid%dst_ncoords))

    !=====================================================================
    
  end subroutine initialize_slint_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_upp_fv3_glblgrd_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! upp_fv3_glblgrd_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN upp_fv3_glblgrd_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN upp_fv3_glblgrd_struct variable where all arrays
  !   are allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_upp_fv3_glblgrd_struct(grid)

    ! Define variables passed to routine

    type(upp_fv3_glblgrd_struct)                                        :: grid

    !=====================================================================
    
    ! Deallocate memory for local variables

    if(.not. allocated(grid%ak)) allocate(grid%ak((grid%nz + 1)))
    if(.not. allocated(grid%bk)) allocate(grid%bk((grid%nz + 1)))

    !=====================================================================

  end subroutine initialize_upp_fv3_glblgrd_struct

  !=======================================================================

  ! SUBROUTINE:

  ! initialize_upp_fv3_rgnlgrd_struct.f90

  ! DESCRIPTION:

  ! This subroutine deallocates memory for all arrays within the
  ! upp_fv3_rgnlgrd_struct FORTRAN structure.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN upp_fv3_rgnlgrd_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN upp_fv3_rgnlgrd_struct variable where all arrays
  !   are allocated and initialized (when necessary).

  !-----------------------------------------------------------------------

  subroutine initialize_upp_fv3_rgnlgrd_struct(grid)

    ! Define variables passed to routine

    type(upp_fv3_rgnlgrd_struct)                                        :: grid

    !=====================================================================
    
    ! Deallocate memory for local variables

    if(.not. allocated(grid%grid_xt))                                      &
         & allocate(grid%grid_xt(grid%nx,grid%ny))
    if(.not. allocated(grid%grid_yt))                                      &
         & allocate(grid%grid_yt(grid%nx,grid%ny))
    if(.not. allocated(grid%pfull))                                        &
         & allocate(grid%pfull(grid%nz))
    if(.not. allocated(grid%phalf))                                        &
         & allocate(grid%phalf(grid%nz + 1))
    if(.not. allocated(grid%time))                                         &
         & allocate(grid%time(grid%nt))
    if(.not. allocated(grid%ak))                                           &
         & allocate(grid%ak((grid%nz + 1)))
    if(.not. allocated(grid%bk))                                           &
         & allocate(grid%bk((grid%nz + 1)))

    !=====================================================================

  end subroutine initialize_upp_fv3_rgnlgrd_struct

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
