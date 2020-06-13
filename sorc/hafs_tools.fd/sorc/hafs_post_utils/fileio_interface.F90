module fileio_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! post-utils :: fileio_interface
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

  ! Define associate modules and subroutines

  use json_interface
  use kinds_interface
  use namelist_interface
  use nemsio_interface
  use netcdf
  use netcdf_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: fileio_interface_read
  public :: fileio_interface_write
  interface fileio_interface_read_init
     module procedure init_fv3_glbl_netcdf_upp_read
     module procedure init_fv3_glbl_netcdf_var_read
     module procedure init_fv3_json_glbl_read
     module procedure init_fv3_json_rgnl_read
     module procedure init_fv3_rgnl_netcdf_upp_read
     module procedure init_fv3_rgnl_netcdf_var_read
     module procedure init_slint_remap_read
  end interface fileio_interface_read_init
  interface fileio_interface_read
     module procedure read_fv3_grid
     module procedure read_fv3_json_glbl
     module procedure read_fv3_json_rgnl
     module procedure read_fv3_glbl_netcdf_upp
     module procedure read_fv3_glbl_netcdf_var
     module procedure read_fv3_rgnl_netcdf_upp
     module procedure read_fv3_rgnl_netcdf_var
     module procedure read_slint_remap
  end interface fileio_interface_read
  interface fileio_interface_varinfo
     module procedure varinfo_fv3_rgnl_netcdf_upp
     module procedure varinfo_slint_remap
  end interface fileio_interface_varinfo
  interface fileio_interface_write
     module procedure write_fv3_nems_meta
     module procedure write_fv3_nems_var
     module procedure write_fv3_netcdf_upp
     module procedure write_fv3_netcdf_var
     module procedure write_slint_remap
  end interface fileio_interface_write

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! init_fv3_glbl_netcdf_upp_read.f90

  ! DESCRIPTION:

  ! This subroutine initializes all variables within the FORTRAN
  ! upp_fv3_glblgrd_struct variable in accordance with the specified
  ! file (filename) attributes.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * grid; a FORTRAN upp_fv3_glblgrd_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN upp_fv3_glblgrd_struct variable containing
  !   allocated and initialized arrays.

  !-----------------------------------------------------------------------

  subroutine init_fv3_glbl_netcdf_upp_read(filename,grid)

    ! Define variables passed to routine

    type(upp_fv3_glblgrd_struct)                                        :: grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: dimname
    
    !=====================================================================

    ! Define local variables
    
    dimname = 'grid_xt'
    call netcdf_interface_getdim(filename,dimname,grid%nx)
    grid%nx = 6*grid%nx
    dimname = 'grid_yt'
    call netcdf_interface_getdim(filename,dimname,grid%ny)
    grid%ny = 6*grid%ny
    dimname = 'pfull'
    call netcdf_interface_getdim(filename,dimname,grid%nz)
    dimname = 'time'
    call netcdf_interface_getdim(filename,dimname,grid%nt)
    call variable_interface_setup_struct(grid)

    !=====================================================================

  end subroutine init_fv3_glbl_netcdf_upp_read

  !=======================================================================

  ! SUBROUTINE:

  ! init_fv3_glbl_netcdf_var_read.f90

  ! DESCRIPTION:

  ! This subroutine initializes all variables within the FORTRAN
  ! netcdf_var_struct variable in accordance with the specified file
  ! (filename) attributes.

  ! INPUT VARIABLES:

  ! * json; a FORTRAN fv3_json_glbl_struct variable.

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * grid; a FORTRAN netcdf_var_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN netcdf_var_struct variable containing allocated
  !   and initialized arrays.

  !-----------------------------------------------------------------------

  subroutine init_fv3_glbl_netcdf_var_read(json,filename,grid)

    ! Define variables passed to routine

    type(fv3_json_glbl_struct)                                          :: json
    type(netcdf_var_struct)                                             :: grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: dimname
    
    !=====================================================================

    ! Define local variables
    
    dimname = 'grid_xt'
    call netcdf_interface_getdim(filename,dimname,grid%nx)
    dimname = 'grid_yt'
    call netcdf_interface_getdim(filename,dimname,grid%ny)
    dimname = 'pfull'
    call netcdf_interface_getdim(filename,dimname,grid%nz)
    dimname = 'time'
    call netcdf_interface_getdim(filename,dimname,grid%nt)

    ! Check local variable and proceed accordingly

    if(json%levtype .eq. 2) grid%nz = 1
    if(json%is_z_staggered) grid%nz = grid%nz + 1

    ! Define local variables

    call variable_interface_setup_struct(grid)

    !=====================================================================

  end subroutine init_fv3_glbl_netcdf_var_read

  !=======================================================================

  ! SUBROUTINE:

  ! init_fv3_json_glbl_read.f90

  ! DESCRIPTION:

  ! This subroutine initializes the FORTRAN fv3_json_glbl_struct
  ! variable in accordance with the variables provided in the user
  ! specified JSON variable table filename.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the JSON file.

  ! * nvars; a FORTRAN integer to define the size of the
  !   fv3_json_glbl_struct variable array (below).

  ! * json; a FORTRAN fv3_json_glbl_struct variable array.

  ! OUTPUT VARIABLES:

  ! * json; a FORTRAN fv3_json_glbl_struct variable array containing
  !   the respective variable record attributes.

  !-----------------------------------------------------------------------

  subroutine init_fv3_json_glbl_read(filename,nvars,json)

    ! Define variables passed to routine

    type(fv3_json_glbl_struct), dimension(:),               allocatable :: json
    character(len=500)                                                  :: filename
    integer                                                             :: nvars

    !=====================================================================
    
    ! Define local variables

    call json_interface_nrecs(filename,nvars)

    ! Allocate memory for local variables

    if(.not. allocated(json)) allocate(json(nvars))

    !=====================================================================
    
  end subroutine init_fv3_json_glbl_read

  !=======================================================================

  ! SUBROUTINE:

  ! init_fv3_json_rgnl_read.f90

  ! DESCRIPTION:

  ! This subroutine initializes the FORTRAN fv3_json_rgnl_struct
  ! variable in accordance with the variables provided in the user
  ! specified JSON variable table filename.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the JSON file.

  ! * nvars; a FORTRAN integer to define the size of the
  !   fv3_json_rgnl_struct variable array (below).

  ! * json; a FORTRAN fv3_json_rgnl_struct variable array.

  ! OUTPUT VARIABLES:

  ! * json; a FORTRAN fv3_json_rgnl_struct variable array containing
  !   the respective variable record attributes.

  !-----------------------------------------------------------------------

  subroutine init_fv3_json_rgnl_read(filename,nvars,json)

    ! Define variables passed to routine

    type(fv3_json_rgnl_struct), dimension(:),               allocatable :: json
    character(len=500)                                                  :: filename
    integer                                                             :: nvars

    !=====================================================================
    
    ! Define local variables

    call json_interface_nrecs(filename,nvars)

    ! Allocate memory for local variables

    if(.not. allocated(json)) allocate(json(nvars))

    !=====================================================================
    
  end subroutine init_fv3_json_rgnl_read

  !=======================================================================

  ! SUBROUTINE:

  ! init_fv3_rgnl_netcdf_upp_read.f90

  ! DESCRIPTION:

  ! This subroutine initializes all variables within the FORTRAN
  ! upp_fv3_rgnlgrd_struct variable in accordance with the specified
  ! file (filename) attributes.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * grid; a FORTRAN upp_fv3_rgnlgrd_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN upp_fv3_rgnlgrd_struct variable containing
  !   allocated and initialized arrays.

  !-----------------------------------------------------------------------

  subroutine init_fv3_rgnl_netcdf_upp_read(filename,grid)

    ! Define variables passed to routine

    type(upp_fv3_rgnlgrd_struct)                                        :: grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: dimname
    
    !=====================================================================

    ! Define local variables
    
    dimname = 'grid_xt'
    call netcdf_interface_getdim(filename,dimname,grid%nx)
    dimname = 'grid_yt'
    call netcdf_interface_getdim(filename,dimname,grid%ny)
    dimname = 'pfull'
    call netcdf_interface_getdim(filename,dimname,grid%nz)
    dimname = 'time'
    call netcdf_interface_getdim(filename,dimname,grid%nt)
    call variable_interface_setup_struct(grid)

    !=====================================================================

  end subroutine init_fv3_rgnl_netcdf_upp_read

  !=======================================================================

  ! SUBROUTINE:

  ! init_fv3_rgnl_netcdf_var_read.f90

  ! DESCRIPTION:

  ! This subroutine initializes all variables within the FORTRAN
  ! netcdf_var_struct variable in accordance with the specified file
  ! (filename) attributes.

  ! INPUT VARIABLES:

  ! * json; a FORTRAN fv3_json_rgnl_struct variable.

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * grid; a FORTRAN netcdf_var_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN netcdf_var_struct variable containing allocated
  !   and initialized arrays.

  !-----------------------------------------------------------------------

  subroutine init_fv3_rgnl_netcdf_var_read(json,filename,grid)

    ! Define variables passed to routine

    type(fv3_json_rgnl_struct)                                          :: json
    type(netcdf_var_struct)                                             :: grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: dimname
    
    !=====================================================================

    ! Define local variables
    
    dimname = 'grid_xt'
    call netcdf_interface_getdim(filename,dimname,grid%nx)
    dimname = 'grid_yt'
    call netcdf_interface_getdim(filename,dimname,grid%ny)
    dimname = 'pfull'
    call netcdf_interface_getdim(filename,dimname,grid%nz)
    dimname = 'time'
    call netcdf_interface_getdim(filename,dimname,grid%nt)

    ! Check local variable and proceed accordingly

    if(json%levtype .eq. 2) grid%nz = 1
    if(json%is_z_staggered) grid%nz = grid%nz + 1

    ! Define local variables

    call variable_interface_setup_struct(grid)

    !=====================================================================

  end subroutine init_fv3_rgnl_netcdf_var_read

  !=======================================================================

  ! SUBROUTINE:

  ! init_slint_remap_read.f90

  ! DESCRIPTION:

  ! This subroutine initializes all variables within the FORTRAN slint
  ! variable in accordance with the specified file (filename)
  ! attributes.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * remap; a FORTRAN slint_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN slint_struct variable containing allocated and
  !   initialized arrays.

  !-----------------------------------------------------------------------

  subroutine init_slint_remap_read(filename,remap)

    ! Define variables passed to routine

    type(slint_struct)                                                  :: remap
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: dimname
    
    !=====================================================================

    ! Define local variables
    
    dimname = 'dst_nx'
    call netcdf_interface_getdim(filename,dimname,remap%dst_nx)
    dimname = 'dst_ny'
    call netcdf_interface_getdim(filename,dimname,remap%dst_ny)
    dimname = 'dst_ncoords'
    call netcdf_interface_getdim(filename,dimname,remap%dst_ncoords)
    dimname = 'src_ncoords'
    call netcdf_interface_getdim(filename,dimname,remap%src_ncoords)
    dimname = 'ncoeffs'
    call netcdf_interface_getdim(filename,dimname,remap%ncoeffs)

    ! Define local variables

    call variable_interface_setup_struct(remap)

    !=====================================================================

  end subroutine init_slint_remap_read

  !=======================================================================

  ! SUBROUTINE:

  ! read_fv3_grid.f90

  ! DESCRIPTION:

  ! This subroutine reads the FV3 grid attributes from an external
  ! netcdf file.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * grid; a FORTRAN grid_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN grid_struct variable containing the geographical
  !   values collected from the netcdf file.

  !-----------------------------------------------------------------------

  subroutine read_fv3_grid(filename,grid)

    ! Define variables passed to routine

    type(grid_struct)                                                   :: grid
    character(len=500)                                                  :: filename(:)

    ! Define variables computed within routine

    type(grid_struct)                                                   :: grid_local
    character(len=100)                                                  :: dimname
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:),             allocatable :: work_lat
    real(r_kind),               dimension(:,:),             allocatable :: work_lon
    integer                                                             :: idx

    ! Define counting variables

    integer                                                             :: i, j, k

    !=====================================================================

    ! Define local variables

    dimname      = 'grid_xt'
    call netcdf_interface_getdim(filename(1),dimname,grid_local%nx)
    dimname      = 'grid_yt'
    call netcdf_interface_getdim(filename(1),dimname,grid_local%ny)
    call variable_interface_setup_struct(grid_local)
    grid%nx      = grid_local%nx
    grid%ny      = grid_local%ny
    grid%ncoords = size(filename)*grid_local%nx*grid_local%ny
    call variable_interface_setup_struct(grid)
    idx          = 0

    ! Allocate memory for local variables

    if(.not. allocated(work_lat))                                          &
         allocate(work_lat(grid_local%nx,grid_local%ny))
    if(.not. allocated(work_lon))                                          &
         allocate(work_lon(grid_local%nx,grid_local%ny))

    ! Loop through local variable

    do k = 1, size(filename)

       ! Define local variables

       varname = 'grid_latt'
       call netcdf_interface_getvar(filename(k),varname,work_lat)
       varname = 'grid_lont'
       call netcdf_interface_getvar(filename(k),varname,work_lon)

       ! Loop through local variable

       do j = 1, grid_local%ny

          ! Loop through local variable

          do i = 1, grid_local%nx

             ! Define local variables

             idx           = idx + 1
             grid%lat(idx) = work_lat(i,j)
             grid%lon(idx) = work_lon(i,j)
       
          end do ! do i = 1, grid_local%nx

       end do ! do j = 1, grid_local%ny

    end do ! do k = 1, size(filename)

    ! Deallocate memory for local variables

    if(allocated(work_lat)) deallocate(work_lat)
    if(allocated(work_lon)) deallocate(work_lon)
    call variable_interface_cleanup_struct(grid_local)

    !=====================================================================

  end subroutine read_fv3_grid

  !=======================================================================

  ! SUBROUTINE:

  ! read_fv3_json_glbl.f90

  ! DESCRIPTION:

  ! This subroutine reads the contents of a user specified JSON
  ! formatted file and fills the json variable array.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the JSON file.

  ! * json; a FORTRAN fv3_json_glbl_struct variable array.

  ! OUTPUT VARIABLES:

  ! * json; a FORTRAN fv3_json_glbl_struct variable array containing
  !   the respective variable record attributes.

  !-----------------------------------------------------------------------

  subroutine read_fv3_json_glbl(filename,json)

    ! Define variables passed to routine

    type(fv3_json_glbl_struct), dimension(:),               allocatable :: json
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    integer                                                             :: nvars

    !=====================================================================

    ! Define local variables

    call fileio_interface_read_init(filename,nvars,json) 
    call json_interface_read(filename,nvars,json)

    !=====================================================================

  end subroutine read_fv3_json_glbl

  !=======================================================================

  ! SUBROUTINE:

  ! read_fv3_json_rgnl.f90

  ! DESCRIPTION:

  ! This subroutine reads the contents of a user specified JSON
  ! formatted file and fills the json variable array.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the JSON file.

  ! * json; a FORTRAN fv3_json_rgnl_struct variable array.

  ! OUTPUT VARIABLES:

  ! * json; a FORTRAN fv3_json_rgnl_struct variable array containing
  !   the respective variable record attributes.

  !-----------------------------------------------------------------------

  subroutine read_fv3_json_rgnl(filename,json)

    ! Define variables passed to routine

    type(fv3_json_rgnl_struct), dimension(:),               allocatable :: json
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    integer                                                             :: nvars

    !=====================================================================

    ! Define local variables

    call fileio_interface_read_init(filename,nvars,json) 
    call json_interface_read(filename,nvars,json)

    !=====================================================================

  end subroutine read_fv3_json_rgnl

  !=======================================================================

  ! SUBROUTINE:

  ! read_fv3_glbl_netcdf_upp.f90

  ! DESCRIPTION:

  ! This subroutine reads a netcdf file and defines the FORTRAN
  ! upp_fv3_glblgrd_struct variable attributes for the grid-projection
  ! and vertical levels.

  ! INPUT VARIABLES:

  ! * grid_filename; a FORTRAN character string specifying the
  !   full-path to the netcdf file containing the grid attributes.

  ! * static_filename; a FORTRAN character string specifying the
  !   full-path to the netcdf file containing the static (e.g.,
  !   vertical coordinate) attributes.

  ! * grid; a FORTRAN upp_fv3_glblgrd_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN upp_fv3_glblgrd_struct variable containing
  !   initialized arrays for the grid-projection and vertical levels.

  !-----------------------------------------------------------------------

  subroutine read_fv3_glbl_netcdf_upp(grid_filename,static_filename,grid)

    ! Define variables passed to routine

    type(upp_fv3_glblgrd_struct)                                        :: grid
    character(len=500)                                                  :: grid_filename
    character(len=500)                                                  :: static_filename

    ! Define variables computed within routine

    character(len=100)                                                  :: attrname
    character(len=100)                                                  :: varname
    
    !=====================================================================

    ! Define local variables

    call fileio_interface_read_init(grid_filename,grid)
    varname = 'pk'
    call netcdf_interface_getvar(static_filename,varname,grid%ak)
    varname = 'bk'
    call netcdf_interface_getvar(static_filename,varname,grid%bk)

    !=====================================================================

  end subroutine read_fv3_glbl_netcdf_upp

  !=======================================================================

  ! SUBROUTINE:

  ! read_fv3_glbl_netcdf_var.f90

  ! DESCRIPTION:

  ! This subroutine reads a variable from an external netcdf file.

  ! INPUT VARIABLES:

  ! * json; a FORTRAN fv3_json_glbl_struct variable.

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * grid; a FORTRAN netcdf_var_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN netcdf_var_struct variable containing the values
  !   read the netcdf file.

  !-----------------------------------------------------------------------

  subroutine read_fv3_glbl_netcdf_var(json,filename,var)

    ! Define variables passed to routine

    type(fv3_json_glbl_struct)                                          :: json
    character(len=500)                                                  :: filename(6)
    real(r_kind),               dimension(:,:),             allocatable :: var

    ! Define variables computed within routine

    type(netcdf_var_struct)                                             :: grid
    type(netcdf_var_struct)                                             :: grid_local
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:,:,:),         allocatable :: work
    integer                                                             :: idx

    ! Define counting variables

    integer                                                             :: i, j, k, l

    !=====================================================================

    ! Define local variables

    call fileio_interface_read_init(json,filename(1),grid)
    grid_local%nx = grid%nx
    grid_local%ny = grid%ny
    varname       = json%variable_name 

    ! Check local variable and proceed accordingly

    if(json%levtype .eq. 2) grid_local%nz = 1
    if(json%levtype .eq. 3) grid_local%nz = grid%nz

    ! Allocate memory for local variables

    if(.not. allocated(work))                                              &
         & allocate(work(6,grid%nx,grid%ny,grid_local%nz))
    if(.not. allocated(var))                                               &
         & allocate(var(6*grid_local%nx*grid_local%ny,grid_local%nz))

    ! Loop through local variable

    do i = 1, size(filename)

       ! Define local variables

       call netcdf_interface_getvar(filename(i),varname,work(i,:,:,:))

    end do ! do i = 1, size(filename)

    ! Loop through local variable

    do l = 1, grid_local%nz 

       ! Define local variables

       idx = 0

       ! Loop through local variable

       do k = 1, size(filename)
       
          ! Loop through local variable

          do j = 1, grid%ny

             ! Loop through local variable

             do i = 1, grid%nx
                
                ! Define local variables

                idx        = idx + 1
                var(idx,l) = work(k,i,j,l)
             
             end do ! do i = 1, grid%nx
             
          end do ! do j = 1, grid%ny
       
       end do ! do k = 1, size(filename)

    end do ! do l = 1, grid_local%nz

    ! Deallocate memory for local variables

    if(allocated(work)) deallocate(work)
    call variable_interface_cleanup_struct(grid)
    call variable_interface_cleanup_struct(grid_local)

    !=====================================================================

  end subroutine read_fv3_glbl_netcdf_var

  !=======================================================================

  ! SUBROUTINE:

  ! read_fv3_rgnl_netcdf_upp.f90

  ! DESCRIPTION:

  ! This subroutine reads a netcdf file and defines the FORTRAN
  ! upp_fv3_rgnlgrd_struct variable attributes for the
  ! grid-projection, vertical levels, and time.

  ! INPUT VARIABLES:

  ! * grid_filename; a FORTRAN character string specifying the
  !   full-path to the netcdf file containing the grid attributes.

  ! * static_filename; a FORTRAN character string specifying the
  !   full-path to the netcdf file containing the static (e.g.,
  !   vertical coordinate) attributes.

  ! * grid; a FORTRAN upp_fv3_rgnlgrd_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN upp_fv3_rgnlgrd_struct variable containing
  !   initialized arrays for the grid-projection, vertical levels, and
  !   time.

  !-----------------------------------------------------------------------

  subroutine read_fv3_rgnl_netcdf_upp(grid_filename,static_filename,grid)

    ! Define variables passed to routine

    type(upp_fv3_rgnlgrd_struct)                                        :: grid
    character(len=500)                                                  :: grid_filename
    character(len=500)                                                  :: static_filename

    ! Define variables computed within routine

    character(len=100)                                                  :: attrname
    character(len=100)                                                  :: varname
    
    !=====================================================================

    ! Define local variables

    call fileio_interface_read_init(grid_filename,grid)
    varname = 'grid_xt'
    call netcdf_interface_getvar(grid_filename,varname,grid%grid_xt)
    varname = 'grid_yt'
    call netcdf_interface_getvar(grid_filename,varname,grid%grid_yt)
    varname = 'pfull'
    call netcdf_interface_getvar(grid_filename,varname,grid%pfull)
    varname = 'phalf'
    call netcdf_interface_getvar(grid_filename,varname,grid%phalf)
    varname = 'time'
    call netcdf_interface_getvar(grid_filename,varname,grid%time)
    varname = 'pk'
    call netcdf_interface_getvar(static_filename,varname,grid%ak)
    varname = 'bk'
    call netcdf_interface_getvar(static_filename,varname,grid%bk)

    !=====================================================================

  end subroutine read_fv3_rgnl_netcdf_upp

  !=======================================================================

  ! SUBROUTINE:

  ! read_fv3_rgnl_netcdf_var.f90

  ! DESCRIPTION:

  ! This subroutine reads a variable from an external netcdf file.

  ! INPUT VARIABLES:

  ! * json; a FORTRAN fv3_json_rgnl_struct variable.

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * grid; a FORTRAN netcdf_var_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN netcdf_var_struct variable containing the values
  !   read the netcdf file.

  !-----------------------------------------------------------------------

  subroutine read_fv3_rgnl_netcdf_var(json,filename,grid)

    ! Define variables passed to routine

    type(fv3_json_rgnl_struct)                                          :: json
    type(netcdf_var_struct)                                             :: grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: varname

    !=====================================================================

    ! Define local variables

    call fileio_interface_read_init(json,filename,grid)
    varname = json%variable_name
    call netcdf_interface_getvar(filename,varname,grid%var)

    !=====================================================================

  end subroutine read_fv3_rgnl_netcdf_var

  !=======================================================================

  ! SUBROUTINE:

  ! read_slint_remap.f90

  ! DESCRIPTION:

  ! This subroutine reads a netcdf file and defines the FORTRAN
  ! slint_struct variable attributes.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * remap; a FORTRAN slint_struct variable.

  ! OUTPUT VARIABLES:

  ! * remap; a FORTRAN slint_struct variable containing initialized
  !   and/or defined arrays.

  !-----------------------------------------------------------------------

  subroutine read_slint_remap(filename,remap)

    ! Define variables passed to routine

    type(slint_struct)                                                  :: remap
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: varname
    
    !=====================================================================

    ! Define local variables

    call fileio_interface_read_init(filename,remap)
    varname = 'coeffs'
    call netcdf_interface_getvar(filename,varname,remap%coeffs)
    varname = 'nn'
    call netcdf_interface_getvar(filename,varname,remap%nn)
    varname = 'mask'
    call netcdf_interface_getvar(filename,varname,remap%mask)
    varname = 'dst_lat'
    call netcdf_interface_getvar(filename,varname,remap%dst_lat)
    varname = 'dst_lon'
    call netcdf_interface_getvar(filename,varname,remap%dst_lon)
    varname = 'src_lat'
    call netcdf_interface_getvar(filename,varname,remap%src_lat)
    varname = 'src_lon'
    call netcdf_interface_getvar(filename,varname,remap%src_lon)

    !=====================================================================

  end subroutine read_slint_remap

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_fv3_rgnl_netcdf_upp.f90

  ! DESCRIPTION:

  ! This subroutine writes the netcdf directives (e.g., varinfo) for
  ! the user specified netcdf formatted file to be written.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN upp_fv3_rgnlgrd_struct variable.

  ! * json; a FORTRAN fv3_json_rgnl_struct variable array.

  ! * varinfo; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_fv3_rgnl_netcdf_upp(grid,json,varinfo)

    ! Define variables passed to routine

    type(fv3_json_rgnl_struct)                                          :: json(:)
    type(upp_fv3_rgnlgrd_struct)                                        :: grid
    type(varinfo_struct)                                                :: varinfo

    ! Define variables computed within routine

    character(len=15),          dimension(:),               allocatable :: dimname
    integer,                    dimension(:),               allocatable :: dimval
    integer,                    dimension(:),               allocatable :: dimid 

    ! Define counting variable

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    varinfo%ndims  = 5
    varinfo%nvars  = 5 + size(json)
    varinfo%nattrs = 5
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Define local variables

    dimname(1) = 'grid_xt'
    dimname(2) = 'grid_yt'
    dimname(3) = 'pfull'
    dimname(4) = 'phalf'
    dimname(5) = 'time'
    dimval(1)  = grid%nx
    dimval(2)  = grid%ny
    dimval(3)  = grid%nz
    dimval(4)  = (grid%nz + 1)
    dimval(5)  = grid%nt

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    ! Define local variables

    varinfo%varattrs(1,1,1) = 'long_name'
    varinfo%varattrs(1,1,2) = 'T-cell longitude'
    varinfo%varattrs(1,2,1) = 'units'
    varinfo%varattrs(1,2,2) = 'degrees_E'
    varinfo%varattrs(1,3,1) = 'cartesian_axis'
    varinfo%varattrs(1,3,2) = 'X'
    varinfo%varname(1)      = 'grid_xt'
    varinfo%varndims(1)     = 2
    varinfo%varnattrs(1)    = 3
    varinfo%vartype(1)      = 'double'
    varinfo%vardimid(1,1)   = dimid(1)
    varinfo%vardimid(1,2)   = dimid(2)
    varinfo%varattrs(2,1,1) = 'long_name'
    varinfo%varattrs(2,1,2) = 'T-cell latitude'
    varinfo%varattrs(2,2,1) = 'units'
    varinfo%varattrs(2,2,2) = 'degrees_N'
    varinfo%varattrs(2,3,1) = 'cartesian_axis'
    varinfo%varattrs(2,3,2) = 'Y'
    varinfo%varname(2)      = 'grid_yt'
    varinfo%varndims(2)     = 2
    varinfo%varnattrs(2)    = 3
    varinfo%vartype(2)      = 'double'
    varinfo%vardimid(2,1)   = dimid(1)
    varinfo%vardimid(2,2)   = dimid(2)
    varinfo%varattrs(3,1,1) = 'long_name'
    varinfo%varattrs(3,1,2) = 'ref full pressure level'
    varinfo%varattrs(3,2,1) = 'units'
    varinfo%varattrs(3,2,2) = 'mb'
    varinfo%varattrs(3,3,1) = 'cartesian_axis'
    varinfo%varattrs(3,3,2) = 'Z'
    varinfo%varattrs(3,4,1) = 'positive'
    varinfo%varattrs(3,4,2) = 'down'    
    varinfo%varattrs(3,5,1) = 'edges'
    varinfo%varattrs(3,5,2) = 'phalf'
    varinfo%varname(3)      = 'pfull'
    varinfo%varndims(3)     = 1
    varinfo%varnattrs(3)    = 5
    varinfo%vartype(3)      = 'double'
    varinfo%vardimid(3,1)   = dimid(3)
    varinfo%varattrs(4,1,1) = 'long_name'
    varinfo%varattrs(4,1,2) = 'ref half pressure level'
    varinfo%varattrs(4,2,1) = 'units'
    varinfo%varattrs(4,2,2) = 'mb'
    varinfo%varattrs(4,3,1) = 'cartesian_axis'
    varinfo%varattrs(4,3,2) = 'Z'
    varinfo%varattrs(4,4,1) = 'positive'
    varinfo%varattrs(4,4,2) = 'down'
    varinfo%varname(4)      = 'phalf'
    varinfo%varndims(4)     = 1
    varinfo%varnattrs(4)    = 4
    varinfo%vartype(4)      = 'double'
    varinfo%vardimid(4,1)   = dimid(4)
    varinfo%varattrs(5,1,1) = 'long_name'
    varinfo%varattrs(5,1,2) = 'time'
    varinfo%varattrs(5,2,1) = 'units'
    write(varinfo%varattrs(5,2,2),500) analdate(1:10), analdate(12:19)
    varinfo%varattrs(5,3,1) = 'cartesian_axis'
    varinfo%varattrs(5,3,2) = 'T'
    varinfo%varattrs(5,4,1) = 'calendar_type'
    varinfo%varattrs(5,4,2) = 'JULIAN'
    varinfo%varattrs(5,5,1) = 'calendar'
    varinfo%varattrs(5,5,2) = 'JULIAN'
    varinfo%varname(5)      = 'time'
    varinfo%varndims(5)     = 1
    varinfo%varnattrs(5)    = 5
    varinfo%vartype(5)      = 'double'
    varinfo%vardimid(5,1)   = dimid(5)

    ! Loop through local variable

    do i = 1, size(json)

       ! Define local variables

       varinfo%varnattrs(5+i)    = 1
       varinfo%varattrs(5+i,1,1) = 'units'
       varinfo%varattrs(5+i,1,2) = json(i)%units
       varinfo%vartype(5+i)      = 'float'
       varinfo%varname(5+i)      = json(i)%remap_variable_name

       ! Check local variable and proceed accordingly

       if(json(i)%levtype .eq. 2) then

          ! Define local variables

          varinfo%varndims(5+i)   = 3
          varinfo%vardimid(5+i,1) = dimid(1)
          varinfo%vardimid(5+i,2) = dimid(2)
          varinfo%vardimid(5+i,3) = dimid(5)

       end if ! if(json(i)%levtype .eq. 2)

       ! Check local variable and proceed accordingly

       if(json(i)%levtype .eq. 3) then

          ! Define local variables

          varinfo%varndims(5+i)   = 3
          varinfo%vardimid(5+i,1) = dimid(1)
          varinfo%vardimid(5+i,2) = dimid(2)
          varinfo%vardimid(5+i,4) = dimid(5)

          ! Check local variable and proceed accordingly

          if(json(i)%is_z_staggered) then

             ! Define local variables

             varinfo%vardimid(5+i,3) = dimid(4)

          else   ! if(json(i)%is_z_staggered)

             ! Define local variables

             varinfo%vardimid(5+i,3) = dimid(3)

          end if ! if(json(i)%is_z_staggered)

       end if ! if(json(i)%levtype .eq. 3)

    end do ! do i = 1, size(json)

    ! Deallocate memory for local variables

    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)

    ! Define local variables

500 format('hours since ',a10,1x,a8)

    !=====================================================================

  end subroutine varinfo_fv3_rgnl_netcdf_upp

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_slint_remap.f90

  ! DESCRIPTION:

  ! This subroutine writes the netcdf directives (e.g., varinfo) for
  ! the user specified netcdf formatted file to be written.

  ! INPUT VARIABLES:

  ! * remap; a FORTRAN slint_struct variable.

  ! * varinfo; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_slint_remap(remap,varinfo)

    ! Define variables passed to routine

    type(slint_struct)                                                  :: remap
    type(varinfo_struct)                                                :: varinfo

    ! Define variables computed within routine

    character(len=15),          dimension(:),               allocatable :: dimname
    integer,                    dimension(:),               allocatable :: dimval
    integer,                    dimension(:),               allocatable :: dimid 

    ! Define counting variable

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    varinfo%ndims  = 5
    varinfo%nvars  = 7
    varinfo%nattrs = 2
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Define local variables

    dimname(1) = 'dst_nx'
    dimname(2) = 'dst_ny'
    dimname(3) = 'dst_ncoords'
    dimname(4) = 'src_ncoords'
    dimname(5) = 'ncoeffs'
    dimval(1)  = remap%dst_nx
    dimval(2)  = remap%dst_ny
    dimval(3)  = remap%dst_ncoords
    dimval(4)  = remap%src_ncoords
    dimval(5)  = 3

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    ! Define local variables

    varinfo%varattrs(1,1,1) = 'title'
    varinfo%varattrs(1,1,2) = 'Remapping coefficients.'
    varinfo%varname(1)      = 'coeffs'
    varinfo%varndims(1)     = 2
    varinfo%varnattrs(1)    = 1
    varinfo%vartype(1)      = 'double'
    varinfo%vardimid(1,1)   = dimid(5)
    varinfo%vardimid(1,2)   = dimid(3)
    varinfo%varattrs(2,1,1) = 'title'
    varinfo%varattrs(2,1,2) = 'Nearest neighbors.'
    varinfo%varname(2)      = 'nn'
    varinfo%varndims(2)     = 2
    varinfo%varnattrs(2)    = 1
    varinfo%vartype(2)      = 'integer'
    varinfo%vardimid(2,1)   = dimid(5)
    varinfo%vardimid(2,2)   = dimid(3)
    varinfo%varattrs(3,1,1) = 'title'
    varinfo%varattrs(3,1,2) = 'Remapping mask.'
    varinfo%varname(3)      = 'mask'
    varinfo%varndims(3)     = 1
    varinfo%varnattrs(3)    = 1
    varinfo%vartype(3)      = 'float'
    varinfo%vardimid(3,1)   = dimid(3)
    varinfo%varattrs(4,1,1) = 'title'
    varinfo%varattrs(4,1,2) = 'Source grid latitude.'
    varinfo%varattrs(4,2,1) = 'units'
    varinfo%varattrs(4,2,2) = 'Degrees.'
    varinfo%varname(4)      = 'src_lat'
    varinfo%varndims(4)     = 1
    varinfo%varnattrs(4)    = 2
    varinfo%vartype(4)      = 'float'
    varinfo%vardimid(4,1)   = dimid(4)
    varinfo%varattrs(5,1,1) = 'title'
    varinfo%varattrs(5,1,2) = 'Source grid longitude.'
    varinfo%varattrs(5,2,1) = 'units'
    varinfo%varattrs(5,2,2) = 'Degrees.'
    varinfo%varname(5)      = 'src_lon'
    varinfo%varndims(5)     = 1
    varinfo%varnattrs(5)    = 2
    varinfo%vartype(5)      = 'float'
    varinfo%vardimid(5,1)   = dimid(4)
    varinfo%varattrs(6,1,1) = 'title'
    varinfo%varattrs(6,1,2) = 'Destination grid latitude.'
    varinfo%varattrs(6,2,1) = 'units'
    varinfo%varattrs(6,2,2) = 'Degrees.'
    varinfo%varname(6)      = 'dst_lat'
    varinfo%varndims(6)     = 1
    varinfo%varnattrs(6)    = 2
    varinfo%vartype(6)      = 'float'
    varinfo%vardimid(6,1)   = dimid(3)
    varinfo%varattrs(7,1,1) = 'title'
    varinfo%varattrs(7,1,2) = 'Destination grid longitude.'
    varinfo%varattrs(7,2,1) = 'units'
    varinfo%varattrs(7,2,2) = 'Degrees.'
    varinfo%varname(7)      = 'dst_lon'
    varinfo%varndims(7)     = 1
    varinfo%varnattrs(7)    = 2
    varinfo%vartype(7)      = 'float'
    varinfo%vardimid(7,1)   = dimid(3)

    ! Deallocate memory for local variables

    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)

    ! Define local variables

500 format(f13.5)

    !=====================================================================

  end subroutine varinfo_slint_remap

  !=======================================================================

  ! SUBROUTINE:

  ! write_fv3_nems_meta.f90

  ! DESCRIPTION:

  ! This subroutine opens a NEMS I/O-formatted file and writes the
  ! meta data variables to the file header.

  ! INPUT VARIABLES:

  ! * nemsiometa; a FORTRAN nemsiometa_struct variable; it is assumed
  !   that the number of vertical levels (dimz), the remapping
  !   projection spectral truncation (ntrunc), and the filename
  !   (filename) attributes have been defined prior to calling this
  !   routine.

  ! * json; a FORTRAN fv3_json_glbl_struct variable array.

  !-----------------------------------------------------------------------

  subroutine write_fv3_nems_meta(nemsiometa,json)

    ! Define variables passed to routine

    type(fv3_json_glbl_struct)                                          :: json(:)
    type(nemsiometa_struct)                                             :: nemsiometa

    ! Define variables computed within routine

    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:),             allocatable :: vcoord

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(vcoord)) allocate(vcoord((nemsiometa%dimz + 1),2))

    ! Define local variables

    varname = 'pk'
    call netcdf_interface_getvar(fv3_static_filename,varname,vcoord(:,1))
    varname = 'bk'
    call netcdf_interface_getvar(fv3_static_filename,varname,vcoord(:,2))
    call nemsio_interface_open(nemsiometa,json,vcoord=vcoord,              &
         & is_write=.true.)
    call nemsio_interface_close(nemsiometa)

    ! Deallocate memory for local variables

    if(allocated(vcoord)) deallocate(vcoord)

    !=====================================================================

  end subroutine write_fv3_nems_meta

  !=======================================================================

  ! SUBROUTINE:

  ! write_fv3_nems_var.f90

  ! DESCRIPTION:

  ! This subroutine opens a NEMS I/O-formatted file and writes the
  ! meta data variables to the file header.

  ! INPUT VARIABLES:

  ! * nemsiometa; a FORTRAN nemsiometa_struct variable; it is assumed
  !   that the number of vertical levels (dimz), the remapping
  !   projection spectral truncation (ntrunc), and the filename
  !   (filename) attributes have been defined prior to calling this
  !   routine.

  ! * json; a FORTRAN fv3_json_glbl_struct variable array.

  !-----------------------------------------------------------------------

  subroutine write_fv3_nems_var(nemsiometa,json,nemsiovar)

    ! Define variables passed to routine

    type(fv3_json_glbl_struct)                                          :: json(:)
    type(nemsiometa_struct)                                             :: nemsiometa
    type(nemsiovar_struct)                                              :: nemsiovar

    !=====================================================================

    ! Define local variables
    
    call nemsio_interface_open(nemsiometa,json,is_rdwr=.true.)
    call nemsio_interface_write(nemsiometa,nemsiovar)
    call nemsio_interface_close(nemsiometa)

    !=====================================================================

  end subroutine write_fv3_nems_var
    
  !=======================================================================

  ! SUBROUTINE:

  ! write_fv3_netcdf_upp.f90

  ! DESCRIPTION:

  ! This subroutine writes a netcdf file for the FV3 regional grid to
  ! be used by the NCEP UPP software.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * grid; a FORTRAN upp_fv3_rgnlgrd_struct variable.

  ! * remap; a FORTRAN remap_struct variable containing the attributes
  !   of the remapping grid.

  ! * json; a FORTRAN fv3_json_rgnl_struct variable array.

  !-----------------------------------------------------------------------

  subroutine write_fv3_netcdf_upp(filename,grid,remap,json)

    ! Define variables passed to routine

    type(fv3_json_rgnl_struct)                                          :: json(:)
    type(remap_struct)                                                  :: remap
    type(upp_fv3_rgnlgrd_struct)                                        :: grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    character(len=100)                                                  :: varname
    
    !=====================================================================

    ! Define local variables

    call netcdf_interface_open(filename,.false.,.false.,.true.)
    call fileio_interface_varinfo(grid,json,varinfo)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'grid','latlon')
    ncstatus = nf90_put_att(ncfileid,nf90_global,'cen_lat',remap%cen_lat)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'cen_lon',remap%cen_lon)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'dlat',remap%dlat)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'dlon',remap%dlon)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'lat1',remap%lat_min)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'lat2',remap%lat_max)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'lon1',remap%lon_min)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'lon2',remap%lon_max)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'ak',grid%ak)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'bk',grid%bk)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'nsoil',fv3_nsoil)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'ncld',fv3_ncld)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'imp_physics',            &
         & fv3_imp_physics)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'dtp',fv3_dtp)
    call netcdf_interface_writedef(varinfo,fill_value=json(1)%fill_value)
    varname  = 'grid_xt'
    call netcdf_interface_putvar(filename,varname,grid%grid_xt)
    varname  = 'grid_yt'
    call netcdf_interface_putvar(filename,varname,grid%grid_yt)
    varname  = 'pfull'
    call netcdf_interface_putvar(filename,varname,grid%pfull)
    varname  = 'phalf'
    call netcdf_interface_putvar(filename,varname,grid%phalf)
    varname  = 'time'

    ! Check local variable and proceed accordingly

    if(is_rezero_time) then

       ! Define local variables

       grid%time = dble(0.0)

    end if ! if(is_rezero_time)

    ! Define local variables

    call netcdf_interface_putvar(filename,varname,grid%time)
    call netcdf_interface_close()

    !=====================================================================

  end subroutine write_fv3_netcdf_upp

  !=======================================================================

  ! SUBROUTINE:

  ! write_fv3_netcdf_var.f90

  ! DESCRIPTION:

  ! This subroutine updates/writes a user specified variable to an
  ! existing netcdf file.

  ! INPUT VARIABLES:

  ! * json; a FORTRAN fv3_json_rgnl_struct variable.

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * grid; a FORTRAN netcdf_var_struct variable containing the values
  !   to be written to the netcdf file.

  !-----------------------------------------------------------------------

  subroutine write_fv3_netcdf_var(json,filename,grid)

    ! Define variables passed to routine

    type(fv3_json_rgnl_struct)                                          :: json
    type(netcdf_var_struct)                                             :: grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: varname

    !=====================================================================

    ! Define local variables

    call netcdf_interface_open(filename,.false.,.true.,.false.)
    varname = json%remap_variable_name
    call netcdf_interface_putvar(filename,varname,grid%var)
    call netcdf_interface_close()

    !=====================================================================

  end subroutine write_fv3_netcdf_var

  !=======================================================================

  ! SUBROUTINE:

  ! write_slint_remap.f90

  ! DESCRIPTION:

  ! This subroutine writes a netcdf file containing the attributes of
  ! the SLINT remapping software.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * remap; a FORTRAN slint_struct variable.

  !-----------------------------------------------------------------------

  subroutine write_slint_remap(filename,remap)

    ! Define variables passed to routine

    type(slint_struct)                                                  :: remap
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    character(len=100)                                                  :: varname
    
    !=====================================================================

    ! Define local variables

    call netcdf_interface_open(filename,.false.,.false.,.true.)
    call fileio_interface_varinfo(remap,varinfo)
    call netcdf_interface_writedef(varinfo)
    varname  = 'coeffs'
    call netcdf_interface_putvar(filename,varname,remap%coeffs)
    varname  = 'nn'
    call netcdf_interface_putvar(filename,varname,remap%nn)
    varname  = 'mask'
    call netcdf_interface_putvar(filename,varname,remap%mask)
    varname  = 'src_lat'
    call netcdf_interface_putvar(filename,varname,remap%src_lat)
    varname  = 'src_lon'
    call netcdf_interface_putvar(filename,varname,remap%src_lon)
    varname  = 'dst_lat'
    call netcdf_interface_putvar(filename,varname,remap%dst_lat)
    varname  = 'dst_lon'
    call netcdf_interface_putvar(filename,varname,remap%dst_lon)
    call netcdf_interface_close()

    !=====================================================================

  end subroutine write_slint_remap

  !=======================================================================

end module fileio_interface
