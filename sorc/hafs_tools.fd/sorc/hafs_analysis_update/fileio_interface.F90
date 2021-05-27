module fileio_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: fileio_interface
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

  ! Define associate modules and subroutines

  use json_interface
  use kinds_interface  
  use namelist_interface
  use nemsio_interface
  use netcdf_interface
  use time_methods_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: fileio_interface_date_read
  public :: fileio_interface_date_write  
  public :: fileio_interface_read
  public :: fileio_interface_write
  interface fileio_interface_date_read
     module procedure nemsdate_read
  end interface fileio_interface_date_read
  interface fileio_interface_date_write
     module procedure nemsdate_write
  end interface fileio_interface_date_write  
  interface fileio_interface_read
     module procedure fv3grid_read
     module procedure fv3psfc_read
     module procedure fv3var_read
     module procedure json_fv3var_read
     module procedure json_pattern_read
     module procedure nemsvar_read
     module procedure pattern_read
     module procedure tcv_read
  end interface fileio_interface_read
  interface fileio_interface_varinfo
     module procedure varinfo_pattern
  end interface fileio_interface_varinfo  
  interface fileio_interface_write
     module procedure fv3var_write
     module procedure nemsvar_write
     module procedure pattern_write
  end interface fileio_interface_write

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! fv3grid_read.f90

  ! DESCRIPTION:

  ! This subroutine defines and computes the attributes of the FV3
  ! Arakawa-D grid.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string containing the path to the
  !   external file containing the FV3 grid specifications/attributes.

  ! * fv3grid; an array of FORTRAN fv3grid_struct variables of
  !   dimension 3.

  ! OUTPUT VARIABLES:

  ! * fv3grid; an array of FORTRAN fv3grid_struct variables of
  !   dimension 3 containing the defined and computed attributes of
  !   the FV3 Arakawa-D grid; the mass variable projections are
  !   contained in array 1, the zonal- (U-) wind aligned variable
  !   projections are contained in array 2, and the meridional- (V-)
  !   wind aligned variable projections are contained in array 3.

  !-----------------------------------------------------------------------

  subroutine fv3grid_read(filename_static,filename_grid,fv3grid)

    ! Define variables passed to routine

    type(fv3grid_struct)                                                :: fv3grid(3)
    character(len=500)                                                  :: filename_static
    character(len=500)                                                  :: filename_grid
        
    ! Define variables computed within routine

    character(len=100)                                                  :: dimname
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:),             allocatable :: dlat
    real(r_kind),               dimension(:,:),             allocatable :: dlon
    real(r_kind),               dimension(:,:),             allocatable :: lat
    real(r_kind),               dimension(:,:),             allocatable :: latt
    real(r_kind),               dimension(:,:),             allocatable :: lat_work
    real(r_kind),               dimension(:,:),             allocatable :: lon
    real(r_kind),               dimension(:,:),             allocatable :: lont
    real(r_kind),               dimension(:,:),             allocatable :: lon_work
    real(r_kind),               dimension(:),               allocatable :: ak
    real(r_kind),               dimension(:),               allocatable :: bk
    integer                                                             :: grid_x
    integer                                                             :: grid_xt
    integer                                                             :: grid_y
    integer                                                             :: grid_yt
    integer                                                             :: pfull
    
    ! Define counting variables

    integer                                                             :: i, j
    
    !=====================================================================

    ! Define local variables

    dimname = 'grid_x'
    call netcdf_interface_getdim(filename_grid,dimname,grid_x)
    dimname = 'grid_xt'
    call netcdf_interface_getdim(filename_grid,dimname,grid_xt)
    dimname = 'grid_y'
    call netcdf_interface_getdim(filename_grid,dimname,grid_y)
    dimname = 'grid_yt'
    call netcdf_interface_getdim(filename_grid,dimname,grid_yt)
    dimname = 'pfull'
    call netcdf_interface_getdim(filename_static,dimname,pfull)

    ! Allocate memory for local variables

    if(.not. allocated(dlat)) allocate(dlat(grid_xt,grid_yt))
    if(.not. allocated(dlon)) allocate(dlon(grid_xt,grid_yt))      
    if(.not. allocated(lat))  allocate(lat(grid_x,grid_y))
    if(.not. allocated(latt)) allocate(latt(grid_xt,grid_yt))
    if(.not. allocated(lon))  allocate(lon(grid_x,grid_y))
    if(.not. allocated(lont)) allocate(lont(grid_xt,grid_yt))
    if(.not. allocated(ak))   allocate(ak(pfull))
    if(.not. allocated(bk))   allocate(bk(pfull))
    
    ! Define local variables

    varname = 'grid_lat'
    call netcdf_interface_getvar(filename_grid,varname,lat)
    varname = 'grid_latt'
    call netcdf_interface_getvar(filename_grid,varname,latt)
    varname = 'grid_lon'
    call netcdf_interface_getvar(filename_grid,varname,lon)
    varname = 'grid_lont'
    call netcdf_interface_getvar(filename_grid,varname,lont)
    varname = 'hyam'
    call netcdf_interface_getvar(filename_static,varname,ak)
    varname = 'hybm'
    call netcdf_interface_getvar(filename_static,varname,bk)
    dlat    = 0.0
    dlon    = 0.0
    
    ! Loop through local variables

    do j = 1, (grid_yt - 1)

       ! Loop through local variable

       do i = 1, grid_xt

          ! Compute local variables

          dlat(i,j) = latt(i,j) - latt(i,j+1)

       end do ! do i = 1, grid_xt

    end do ! do j = 1, (grid_yt - 1)

    ! Loop through local variables

    do j = 1, grid_yt

       ! Loop through local variable

       do i = 1, (grid_xt - 1)

          ! Compute local variables

          dlon(i,j) = lont(i,j) - lont(i+1,j)

       end do ! do i = 1, (grid_xt - 1)

    end do ! do j = 1, grid_yt

    ! Define local variables
          
    fv3grid(1)%nx   = grid_xt
    fv3grid(1)%ny   = grid_yt
    fv3grid(1)%nz   = pfull
    call variable_interface_setup_struct(fv3grid(1))
    fv3grid(1)%lat  = reshape(latt,shape(fv3grid(1)%lat))
    fv3grid(1)%lon  = reshape(lont,shape(fv3grid(1)%lon))
    if(debug) write(6,500) 'T', minval(fv3grid(1)%lat),                    &
         & maxval(fv3grid(1)%lat)
    if(debug) write(6,501) 'T', minval(fv3grid(1)%lon),                    &
         & maxval(fv3grid(1)%lon)
    fv3grid(1)%clat = latt(nint(fv3grid(1)%nx/2.0),                        &
         & nint(fv3grid(1)%ny/2.0))
    fv3grid(1)%clon = lont(nint(fv3grid(1)%nx/2.0),                        &
         & nint(fv3grid(1)%ny/2.0))
    fv3grid(1)%ak   = ak*1.e5
    fv3grid(1)%bk   = bk
    if(debug) write(6,502) 'T', fv3grid(1)%clat, fv3grid(1)%clon
    fv3grid(2)%nx   = grid_xt
    fv3grid(2)%ny   = grid_y
    fv3grid(2)%nz   = pfull
    call variable_interface_setup_struct(fv3grid(2))

    ! Allocate memory for local variables

    if(.not. allocated(lat_work))                                          &
         & allocate(lat_work(fv3grid(2)%nx,fv3grid(2)%ny))
    if(.not. allocated(lon_work))                                          &
         & allocate(lon_work(fv3grid(2)%nx,fv3grid(2)%ny))

    ! Define local variables

    lon_work(1:fv3grid(1)%nx,1:fv3grid(2)%ny) =                            &
         & lon(1:fv3grid(1)%nx,1:fv3grid(2)%ny)
    
    ! Loop through local variable

    do j = 1, fv3grid(1)%ny
       
       ! Loop through local variable

       do i = 1, fv3grid(1)%nx
          
          ! Compute local variables

          lat_work(i,j) = latt(i,j) + 0.5*dlat(i,j)
          lon_work(i,j) = lont(i,j)
          
       end do ! do i = 1, fv3grid(1)%nx
    
    end do ! do j = 1, fv3grid(1)%ny

    ! Define local variables

    lat_work(1:fv3grid(2)%nx,fv3grid(2)%ny) =                              &
         & lat(1:fv3grid(2)%nx,fv3grid(2)%ny)
    fv3grid(2)%lat                          =                              &
         & reshape(lat_work,shape(fv3grid(2)%lat))
    fv3grid(2)%lon                          =                              &
         & reshape(lon_work,shape(fv3grid(2)%lon))    
    if(debug) write(6,500) 'U', minval(fv3grid(2)%lat),                    &
         & maxval(fv3grid(2)%lat)
    if(debug) write(6,501) 'U', minval(fv3grid(2)%lon),                    &
         & maxval(fv3grid(2)%lon)
    fv3grid(2)%clat                         =                              &
         & lat_work(nint(fv3grid(2)%nx/2.0),nint(fv3grid(2)%ny/2.0))
    fv3grid(2)%clon                         =                              &
         & lon_work(nint(fv3grid(2)%nx/2.0),nint(fv3grid(2)%ny/2.0))
    fv3grid(2)%ak                           = ak
    fv3grid(2)%bk                           = bk
    if(debug) write(6,502) 'U', fv3grid(2)%clat, fv3grid(2)%clon

    ! Deallocate memory for local variables

    if(allocated(lat_work)) deallocate(lat_work)
    if(allocated(lon_work)) deallocate(lon_work)

    ! Define local variables
    
    fv3grid(3)%nx = grid_x
    fv3grid(3)%ny = grid_yt
    fv3grid(3)%nz = pfull
    call variable_interface_setup_struct(fv3grid(3))

    ! Allocate memory for local variables

    if(.not. allocated(lat_work))                                          &
         & allocate(lat_work(fv3grid(3)%nx,fv3grid(3)%ny))
    if(.not. allocated(lon_work))                                          &
         & allocate(lon_work(fv3grid(3)%nx,fv3grid(3)%ny))

    ! Define local variables

    lat_work(1:fv3grid(3)%nx,1:fv3grid(1)%ny) =                            &
         & lat(1:fv3grid(3)%nx,1:fv3grid(1)%ny)
    
    ! Loop through local variable

    do j = 1, fv3grid(1)%ny
       
       ! Loop through local variable

       do i = 1, fv3grid(1)%nx
          
          ! Compute local variables

          lat_work(i,j) = latt(i,j)
          lon_work(i,j) = lont(i,j) + 0.5*dlon(i,j)
          
       end do ! do i = 1, fv3grid(1)%nx
    
    end do ! do j = 1, fv3grid(1)%ny

    ! Define local variables

    lon_work(fv3grid(3)%nx,1:fv3grid(3)%ny) =                              &
         & lon(fv3grid(3)%nx,1:fv3grid(3)%ny)
    fv3grid(3)%lat                          =                              &
         & reshape(lat_work,shape(fv3grid(3)%lat))
    fv3grid(3)%lon                          =                              &
         & reshape(lon_work,shape(fv3grid(3)%lon))
    if(debug) write(6,500) 'V', minval(fv3grid(3)%lat),                    &
         & maxval(fv3grid(3)%lat)
    if(debug) write(6,501) 'V', minval(fv3grid(3)%lon),                    &
         & maxval(fv3grid(3)%lon)
    fv3grid(3)%clat                         =                              &
         & lat_work(nint(fv3grid(3)%nx/2.0),nint(fv3grid(3)%ny/2.0))
    fv3grid(3)%clon                         =                              &
         & lon_work(nint(fv3grid(3)%nx/2.0),nint(fv3grid(3)%ny/2.0))
    fv3grid(3)%ak                           = ak
    fv3grid(3)%bk                           = bk
    if(debug) write(6,502) 'V', fv3grid(3)%clat, fv3grid(3)%clon
    
    ! Deallocate memory for local variables

    if(allocated(dlat))     deallocate(dlat)
    if(allocated(dlon))     deallocate(dlon)
    if(allocated(lat_work)) deallocate(lat_work)
    if(allocated(lon_work)) deallocate(lon_work)
    if(allocated(lat))      deallocate(lat)
    if(allocated(latt))     deallocate(latt)
    if(allocated(lon))      deallocate(lon)
    if(allocated(lont))     deallocate(lont)
    if(allocated(ak))       deallocate(ak)
    if(allocated(bk))       deallocate(bk)
    
    ! Define local variables

500 format('FV3GRID_READ: ',a1,1x,'grid; min/max latitude: ',2f13.5)
501 format('FV3GRID_READ: ',a1,1x,'grid; min/max longitude: ',2f13.5)
502 format('FV3GRID_READ: ',a1,1x,'grid; center latitude/longitude: ',     &
         & 2f13.5)
    
    !=====================================================================
    
  end subroutine fv3grid_read

  !=======================================================================

  ! SUBROUTINE:

  ! fv3psfc_read.f90

  ! DESCRIPTION:

  ! This subroutine computes the surface pressure (psfc) by
  ! integrating the FV3 'delp' variable values along the respective
  ! profile.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string containing the path to the
  !   external file containing the FV3 'delp' variable values and/or
  !   attributes.

  ! * fv3grid; a FORTRAN fv3grid_struct variable containing the grid
  !   attribute variables and values for the grid corresponding to the
  !   'delp' variable collected from filename (see above).

  ! * remappres; a FORTRAN remappres_struct variable.
  
  ! OUTPUT VARIABLES:

  ! * remappres; a FORTRAN remappres_struct variable containing the
  !   surface pressure attribute (psfc) computed from the vertical
  !   integral of 'delp' from filename (see above).

  !-----------------------------------------------------------------------

  subroutine fv3psfc_read(filename,fv3grid,remappres)

    ! Define variables passed to routine

    type(fv3grid_struct)                                                :: fv3grid
    type(remappres_struct)                                              :: remappres
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:,:),           allocatable :: delp
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(delp))                                              &
         & allocate(delp(fv3grid%nx,fv3grid%ny,fv3grid%nz))
   
    ! Define local variables

    remappres%ncoords = fv3grid%ncoords
    remappres%nz      = fv3grid%nz
    call variable_interface_setup_struct(remappres)
    varname           = 'delp'
    call netcdf_interface_getvar(filename,varname,delp)
    remappres%psfc    = 0.0

    ! Loop through local variable

    do i = 1, fv3grid%nz

       ! Define local variables

       remappres%psfc = remappres%psfc + reshape(delp(:,:,i),              &
            & shape(remappres%psfc))

    end do ! do i = 1, fv3grid%nz
       
    ! Deallocate memory for local variables

    if(allocated(delp)) deallocate(delp)
    
    !=====================================================================

  end subroutine fv3psfc_read

  !=======================================================================

  ! SUBROUTINE:

  ! fv3var_read.f90

  ! DESCRIPTION:

  ! This subroutine reads a FV3 variable from a FV3 file and populates
  ! the attributes within a FORTRAN fv3var_struct variable specified
  ! by the user.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string containing the path to the
  !   external file containing the FV3 variable values and/or
  !   attributes.

  ! * fv3var; a FORTRAN fv3var_struct variable.

  ! * json_fv3var; a FORTRAN json_fv3var_struct variable containing
  !   (at minimum) the I/O attributes for the respective FV3 variable.
  
  ! OUTPUT VARIABLES:

  ! * fv3var; a FORTRAN fv3var_struct variable containing the user
  !   specified FV3 variable values and attributes.

  !-----------------------------------------------------------------------

  subroutine fv3var_read(filename,fv3var,json_fv3var)

    ! Define variables passed to routine

    type(fv3var_struct)                                                 :: fv3var
    type(json_fv3var_struct)                                            :: json_fv3var
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: dimname
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:,:),           allocatable :: ncvar
    integer                                                             :: xaxis_1
    integer                                                             :: xaxis_2
    integer                                                             :: yaxis_1
    integer                                                             :: yaxis_2
    integer                                                             :: zaxis_1

    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================  

    ! Define local variables

    dimname = 'xaxis_1'
    call netcdf_interface_getdim(filename,dimname,xaxis_1)
    dimname = 'xaxis_2'
    call netcdf_interface_getdim(filename,dimname,xaxis_2)
    dimname = 'yaxis_1'
    call netcdf_interface_getdim(filename,dimname,yaxis_1)
    dimname = 'yaxis_2'
    call netcdf_interface_getdim(filename,dimname,yaxis_2)
    dimname = 'zaxis_1'
    call netcdf_interface_getdim(filename,dimname,zaxis_1)

    ! Check local variable and proceed accordingly

    if(json_fv3var%gridtype .eq. 1) then

       ! Check local variable and proceed accordingly

       if(is_fv3_tracers) then

          ! Define local variables

          fv3var%nx = xaxis_1
          fv3var%ny = yaxis_1

       else   ! if(is_fv3_tracers)
       
          ! Define local variables

          fv3var%nx = xaxis_1
          fv3var%ny = yaxis_2

       end if ! if(is_fv3_tracers)
          
    end if ! if(json_fv3var%gridtype .eq. 1)

    ! Check local variable and proceed accordingly

    if(json_fv3var%gridtype .eq. 2) then

       ! Define local variables

       fv3var%nx = xaxis_1
       fv3var%ny = yaxis_1

    end if ! if(json_fv3var%gridtype .eq. 2)    

    ! Check local variable and proceed accordingly

    if(json_fv3var%gridtype .eq. 3) then

       ! Define local variables

       fv3var%nx = xaxis_2
       fv3var%ny = yaxis_2

    end if ! if(json_fv3var%gridtype .eq. 3)

    ! Check local variable and proceed accordingly

    if(json_fv3var%levtype .eq. 1) then

       ! Define local variables

       fv3var%nz = 1

    end if ! if(json_fv3var%levtype .eq. 1)

    ! Check local variable and proceed accordingly

    if(json_fv3var%levtype .eq. 3) then

       ! Define local variables

       fv3var%nz = zaxis_1

       ! Check local variable and proceed accordingly

       if(json_fv3var%z_staggered) then

          ! Define local variables

          fv3var%nz = fv3var%nz + 1
          
       end if ! if(json_fv3var%z_staggered)
       
    end if ! if(json_fv3var%levtype .eq. 3)

    ! Allocate memory for local variables

    if(.not. allocated(ncvar))                                             &
         & allocate(ncvar(fv3var%nx,fv3var%ny,fv3var%nz))

    ! Define local variables

    call variable_interface_setup_struct(fv3var)
    varname = trim(adjustl(json_fv3var%variable_name))
    call netcdf_interface_getvar(filename,varname,ncvar)

    ! Loop through local variable

    do i = 1, fv3var%nz

       ! Define local variables

       fv3var%var(:,i) = reshape(ncvar(:,:,i),shape(fv3var%var(:,i)))
       if(debug) write(6,500) trim(adjustl(varname)), i,                   &
            & minval(fv3var%var(:,i)), maxval(fv3var%var(:,i))
       
    end do ! do i = 1, fv3var%nz

    ! Deallocate memory for local variables

    if(allocated(ncvar)) deallocate(ncvar)
    
    ! Define local variables

500 format('FV3VAR_READ: Variable ',a,1x,', Level ',i3.3,1x,'min/max = ',  &
         & 2f13.5)
    
    !=====================================================================  
    
  end subroutine fv3var_read

  !=======================================================================

  ! SUBROUTINE:

  ! fv3var_write.f90

  ! DESCRIPTION:

  ! This subroutine writes a FV3 variable to a FV3 file using the
  ! attributes within a FORTRAN fv3var_struct variable specified by
  ! the user.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string containing the path to the
  !   external file containing the FV3 variable values and/or
  !   attributes.

  ! * fv3var; a FORTRAN fv3var_struct variable.

  ! * json_fv3var; a FORTRAN json_fv3var_struct variable containing
  !   (at minimum) the I/O attributes for the respective FV3 variable.

  !-----------------------------------------------------------------------

  subroutine fv3var_write(filename,fv3var,json_fv3var)

    ! Define variables passed to routine

    type(fv3var_struct)                                                 :: fv3var
    type(json_fv3var_struct)                                            :: json_fv3var
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: dimname
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:,:),           allocatable :: ncvar3d
    real(r_kind),               dimension(:,:),             allocatable :: ncvar2d
  
    !=====================================================================

    ! Define local variables

    varname = trim(adjustl(json_fv3var%variable_name))
    if(debug) write(6,500) trim(adjustl(varname)), fv3var%nx, fv3var%ny,   &
         & fv3var%nz 
    
    ! Check local variable and proceed accordingly

    if(json_fv3var%levtype .eq. 1) then

       ! Allocate memory for local variables

       if(.not. allocated(ncvar2d))                                        &
            & allocate(ncvar2d(fv3var%nx,fv3var%ny))

       ! Define local variables

       ncvar2d = reshape(fv3var%var(:,1),shape(ncvar2d))
       call netcdf_interface_putvar(filename,varname,ncvar2d)

       ! Deallocate memory for local variables

       if(allocated(ncvar2d)) deallocate(ncvar2d)

    end if ! if(json_fv3var%levtype .eq. 1)

    ! Check local variable and proceed accordingly

    if(json_fv3var%levtype .eq. 3) then

       ! Allocate memory for local variables

       if(.not. allocated(ncvar3d))                                        &
            & allocate(ncvar3d(fv3var%nx,fv3var%ny,fv3var%nz))

       ! Define local variables

       ncvar3d = reshape(fv3var%var,shape(ncvar3d))
       call netcdf_interface_putvar(filename,varname,ncvar3d)

       ! Deallocate memory for local variables

       if(allocated(ncvar3d)) deallocate(ncvar3d)

    end if ! if(json_fv3var%levtype .eq. 3)

    ! Define local variables

500 format('FV3VAR_WRITE: Writing variable ', a,1x,'of dimension ',3i)

    !===================================================================== 

  end subroutine fv3var_write
    
  !=======================================================================

  ! SUBROUTINE:

  ! json_fv3var_read.f90

  ! DESCRIPTION:

  ! This subroutine defines the contents of the json_fv3var_struct
  ! variable.

  ! INPUT VARIABLES:

  ! * json_fv3var; an array of FORTRAN json_fv3var_struct variables.

  ! OUTPUT VARIABLES:

  ! * json_fv3var; an array of FORTRAN json_fv3var_struct variables
  !   containing the contents of the user specified JSON-formatted
  !   file.

  !-----------------------------------------------------------------------

  subroutine json_fv3var_read(json_fv3var)

    ! Define variables passed to routine

    type(json_fv3var_struct),   dimension(:),               allocatable :: json_fv3var

    !=====================================================================    

    ! Define local variables

    call json_interface_read(json_fv3var_filename,json_fv3var)

    !=====================================================================

  end subroutine json_fv3var_read

  !=======================================================================

  ! SUBROUTINE:

  ! json_pattern_read.f90

  ! DESCRIPTION:

  ! This subroutine reads the contents of a user specified JSON
  ! formatted file and fills the json variable array.

  ! INPUT VARIABLES:

  ! * json_pattern; a FORTRAN json_pattern_struct variable.

  ! OUTPUT VARIABLES:

  ! * json_pattern; a FORTRAN json_pattern_struct variable array
  !   containing the respective variable record attributes.

  !-----------------------------------------------------------------------

  subroutine json_pattern_read(json_pattern)

    ! Define variables passed to routine

    type(json_pattern_struct),  dimension(:),               allocatable :: json_pattern

    !=====================================================================

    ! Define local variables

    call json_interface_read(ensgen_json_vtable,json_pattern)

    !=====================================================================

  end subroutine json_pattern_read
  
  !=======================================================================

  ! SUBROUTINE:

  ! nemsdate_read.f90

  ! DESCRIPTION:

  ! This subroutine reads a NCEP NEMS-formatted file to collect the
  ! idate variable array, defines a time-stamp value, and returns the
  ! corresponding Julian date within the 'jdate' attribute.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the path to the
  !   NCEP NEMS formatted file.

  ! * nemsio; a FORTRAN nems_struct variable.

  ! OUTPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable containing the Julian
  !   date corresponding to the idate variable array values.

  !-----------------------------------------------------------------------

  subroutine nemsdate_read(filename,nemsio)

    ! Define variables passed to routine
    
    type(nems_struct)                                                   :: nemsio
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(timeinfo_struct)                                               :: timeinfo

    !=====================================================================

    ! Define local variables

    call nemsio_interface_date_read(filename,nemsio)
    write(timeinfo%timestamp,500) nemsio%idate(1), nemsio%idate(2),        &
         & nemsio%idate(3), nemsio%idate(4), nemsio%idate(5),              &
         & nemsio%idate(6)

    ! Compute local variables

    call time_methods_interface_julian(timeinfo%timestamp,timeinfo%jday)

    ! Define local variables

    nemsio%jday = timeinfo%jday
500 format(i4.4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2,':',i2.2)

    !=====================================================================
    
  end subroutine nemsdate_read

  !=======================================================================

  ! SUBROUTINE:

  ! nemsdate_write.f90

  ! DESCRIPTION:

  ! This subroutine writes the idate variable array to NCEP
  ! NEMS-formatted; the idate variable array contents are derived from
  ! the timeinfo_struct variable 'timestamp' attribute.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the path to the
  !   NCEP NEMS formatted file.

  ! * nemsio; a FORTRAN nems_struct variable.

  !-----------------------------------------------------------------------

  subroutine nemsdate_write(filename,timeinfo,nemsio)

    ! Define variables passed to routine
    
    type(nems_struct)                                                   :: nemsio
    type(timeinfo_struct)                                               :: timeinfo
    character(len=500)                                                  :: filename

    !=====================================================================

    ! Compute local variables

    call time_methods_interface_attrs(timeinfo)
    
    ! Define local variables

    nemsio%idate(1) = timeinfo%yy
    nemsio%idate(2) = timeinfo%mm
    nemsio%idate(3) = timeinfo%dd
    nemsio%idate(4) = timeinfo%hh
    nemsio%idate(5) = timeinfo%nn
    nemsio%idate(6) = timeinfo%ss
    call nemsio_interface_date_write(filename,nemsio)

    !=====================================================================
    
  end subroutine nemsdate_write

  !=======================================================================

  ! SUBROUTINE:

  ! nemsvar_read.f90

  ! DESCRIPTION:

  ! This subroutine parses a NCEP NEMS formatted file and returns a
  ! FORTRAN structure (nems) containing the gridded variable.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the path to the
  !   NCEP NEMS formatted file.

  ! * nemsio; a FORTRAN nems_struct variable.

  ! OUTPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable containing the gridded
  !   NEMS variable.

  !-----------------------------------------------------------------------

  subroutine nemsvar_read(filename,nemsio)

    ! Define variables passed to routine

    type(nems_struct)                                                   :: nemsio
    character(len=500)                                                  :: filename
    
    !=====================================================================

    ! Define local variables

    call nemsio_interface_init(filename,nemsio)
    call nemsio_interface_read(filename,nemsio)

    !=====================================================================

  end subroutine nemsvar_read

  !=======================================================================

  ! SUBROUTINE:

  ! nemsvar_write.f90

  ! DESCRIPTION:

  ! This subroutine writes a gridded variable to a NCEP NEMS formatted
  ! file.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the path to the
  !   NCEP NEMS formatted file.

  ! * nemsio; a FORTRAN nems_struct variable.

  !-----------------------------------------------------------------------

  subroutine nemsvar_write(filename,nemsio)

    ! Define variables passed to routine

    type(nems_struct)                                                   :: nemsio
    character(len=500)                                                  :: filename
    
    !=====================================================================

    ! Define local variables

    call nemsio_interface_write(filename,nemsio)

    !=====================================================================

  end subroutine nemsvar_write

  !=======================================================================

  ! SUBROUTINE:

  ! pattern_read.f90

  ! DESCRIPTION:

  ! This subroutine reads an external file containing the random
  ! pattern attributes applied for the generation of ensemble member
  ! analyses.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * pattern; a FORTRAN pattern_struct variable.

  ! OUTPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable containing the random
  !   number pattern within the rnp attribute.

  !-----------------------------------------------------------------------

  subroutine pattern_read(filename,pattern)

    ! Define variables passed to routine

    type(pattern_struct)                                                :: pattern
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: dimname
    character(len=100)                                                  :: varname

    !=====================================================================

    ! Define local variables

    dimname = 'nx'
    call netcdf_interface_getdim(filename,dimname,pattern%nx)
    dimname = 'ny'
    call netcdf_interface_getdim(filename,dimname,pattern%ny)
    call variable_interface_setup_struct(pattern)
    varname = 'lat'
    call netcdf_interface_getvar(filename,varname,pattern%lat)
    varname = 'lon'
    call netcdf_interface_getvar(filename,varname,pattern%lon)
    varname = 'rnp'
    call netcdf_interface_getvar(filename,varname,pattern%rnp)

    !=====================================================================

  end subroutine pattern_read

  !=======================================================================

  ! SUBROUTINE:

  ! pattern_write.f90

  ! DESCRIPTION:

  ! This subroutine writes an external file containing the random
  ! pattern attributes applied for the generation of ensemble member
  ! analyses.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * pattern; a FORTRAN pattern_struct variable containing the random
  !   pattern attributes.

  !-----------------------------------------------------------------------

  subroutine pattern_write(filename,pattern)
    
    ! Define variables passed to routine

    type(pattern_struct)                                                :: pattern
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    character(len=100)                                                  :: attrname
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:,:,:),         allocatable :: work

    !=====================================================================

    ! Define local variables

    call netcdf_interface_open(filename,.false.,.false.,.true.)
    call fileio_interface_varinfo(pattern,varinfo)
    attrname = 'adv_dtime'
    call netcdf_interface_putattr(attrname,pattern%dt)
    attrname = 'corr_length'
    call netcdf_interface_putattr(attrname,pattern%lengthscale)
    attrname = 'corr_stdev'
    call netcdf_interface_putattr(attrname,pattern%stdev)
    attrname = 'corr_tau'
    call netcdf_interface_putattr(attrname,pattern%tau)
    attrname = 'rng_seed'
    call netcdf_interface_putattr(attrname,pattern%seed)
    call netcdf_interface_writedef(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(work)) allocate(work(1,pattern%nx,pattern%ny,1))

    ! Define local variables

    work(1,:,:,1) = reshape(pattern%lat,shape(work(1,:,:,1)))
    varname       = varinfo%varname(1)
    call netcdf_interface_putvar(filename,varname,work(1,:,:,1))
    work(1,:,:,1) = reshape(pattern%lon,shape(work(1,:,:,1)))
    varname       = varinfo%varname(2)
    call netcdf_interface_putvar(filename,varname,work(1,:,:,1))
    work(1,:,:,1) = reshape(pattern%rnp,shape(work(1,:,:,1)))
    varname       = varinfo%varname(3)
    call netcdf_interface_putvar(filename,varname,work(1,:,:,1))

    ! Deallocate memory for local variables

    if(allocated(work)) deallocate(work)
    call variable_interface_cleanup_struct(varinfo)

    ! Define local variables

    call netcdf_interface_close()

    !=====================================================================

  end subroutine pattern_write

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_pattern.f90

  ! DESCRIPTION:

  ! This subroutine writes the netcdf directives (e.g., varinfo) for
  ! the user specified netcdf formatted file to be written.

  ! INPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable.

  ! * varinfo; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_pattern(pattern,varinfo)

    ! Define variables passed to routine

    type(pattern_struct)                                                :: pattern
    type(varinfo_struct)                                                :: varinfo

    ! Define variables computed within routine

    character(len=10),          dimension(:),               allocatable :: dimname
    integer,                    dimension(:),               allocatable :: dimval
    integer,                    dimension(:),               allocatable :: dimid   

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================
 
    ! Define local variables

    varinfo%ndims  = 2
    varinfo%nvars  = 3
    varinfo%nattrs = 2
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Define local variables

    dimname(1) = 'nx'
    dimname(2) = 'ny'
    dimval(1)  = pattern%nx
    dimval(2)  = pattern%ny

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    varinfo%varattrs(1,1,1) = 'title'
    varinfo%varattrs(1,1,2) = 'latitude'
    varinfo%varattrs(1,2,1) = 'units'
    varinfo%varattrs(1,2,2) = 'degrees'
    varinfo%varname(1)      = 'lat'
    varinfo%varndims(1)     = 2
    varinfo%varnattrs(1)    = 2
    varinfo%vartype(1)      = 'float'
    varinfo%vardimid(1,1)   = dimid(1)
    varinfo%vardimid(1,2)   = dimid(2)
    varinfo%varattrs(2,1,1) = 'title'
    varinfo%varattrs(2,1,2) = 'longitude'
    varinfo%varattrs(2,2,1) = 'units'
    varinfo%varattrs(2,2,2) = 'degrees'
    varinfo%varname(2)      = 'lon'
    varinfo%varndims(2)     = 2
    varinfo%varnattrs(2)    = 2
    varinfo%vartype(2)      = 'float'
    varinfo%vardimid(2,1)   = dimid(1)
    varinfo%vardimid(2,2)   = dimid(2)
    varinfo%varattrs(3,1,1) = 'title'
    varinfo%varattrs(3,1,2) = 'random number pattern'
    varinfo%varname(3)      = 'rnp'
    varinfo%varndims(3)     = 2
    varinfo%varnattrs(3)    = 1
    varinfo%vartype(3)      = 'float'
    varinfo%vardimid(3,1)   = dimid(1)
    varinfo%vardimid(3,2)   = dimid(2)

    ! Deallocate memory for local variables

    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)

    !=====================================================================

  end subroutine varinfo_pattern  

  !=======================================================================

  ! SUBROUTINE:

  ! tcv_read.f90

  ! DESCRIPTION:

  ! This subroutine ingests an external file containing tropical
  ! cyclone attributes (e.g., TC-vitals).

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the path to the
  !   TCV file.

  ! * tcv; a FORTRAN tcv_struct variable.

  ! OUTPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable now containing the tropical
  !   cyclone attributes retrieved from the user specified file.

  !-----------------------------------------------------------------------

  subroutine tcv_read(filename,tcv)

    ! Define variables passed to routine

    type(tcv_struct),           dimension(:),               allocatable :: tcv
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=1)                                                    :: dummy
    real(r_kind)                                                        :: lat_scale
    real(r_kind)                                                        :: lon_scale
    integer                                                             :: ntcs

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    ntcs = 0
    open(99,file=trim(adjustl(filename)),form='formatted')
1000 read(99,*,end=1001) dummy
    ntcs = ntcs + 1
    goto 1000
1001 continue
    close(99)

    ! Allocate memory for local variables

    if(.not. allocated(tcv)) allocate(tcv(ntcs))

    ! Define local variables

    open(99,file=trim(adjustl(filename)),form='formatted')

    ! Loop through local variable

    do i = 1, ntcs

       ! Define local variables

       read(99,500) tcv(i)%center, tcv(i)%id, tcv(i)%name, tcv(i)%century, &
            & tcv(i)%yymmdd, tcv(i)%hhmm, tcv(i)%lati, tcv(i)%latns,       &
            & tcv(i)%loni, tcv(i)%lonew, tcv(i)%stdir, tcv(i)%stspd,       &
            & tcv(i)%pcen, tcv(i)%penv, tcv(i)%penvrad, tcv(i)%vmax,       &
            & tcv(i)%vmaxrad, tcv(i)%r15ne, tcv(i)%r15se, tcv(i)%r15sw,    &
            & tcv(i)%r15nw, tcv(i)%depth
       lat_scale = 1.0/10.0
       lon_scale = 1.0/10.0

       ! Check local variable and proceed accordingly

       if(tcv(i)%latns .eq. 'S') lat_scale = -1.0*lat_scale
       if(tcv(i)%lonew .eq. 'W') lon_scale = -1.0*lon_scale

       ! Compute local variables

       tcv(i)%lat = real(tcv(i)%lati)*lat_scale
       tcv(i)%lon = real(tcv(i)%loni)*lon_scale

       ! Define local variables

       tcv(i)%lon = tcv(i)%lon + 360.0

    end do ! do i = 1, ntcs

    ! Define local variables

    close(99)
500 format(a4,1x,a3,1x,a9,1x,i2,i6,1x,i4,1x,i3,a1,1x,i4,a1,1x,i3,1x,i3,    &
         & 3(1x,i4),1x,i2,1x,i3,1x,4(i4,1x),a1)

    !=====================================================================

  end subroutine tcv_read
  
  !=======================================================================
  
end module fileio_interface
