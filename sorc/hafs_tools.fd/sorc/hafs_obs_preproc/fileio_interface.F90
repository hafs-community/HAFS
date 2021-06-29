module fileio_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: fileio_interface
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

  use gridtrans_interface
  use gridtrans_fv3_interface
  use json_interface
  use kinds_interface
  use namelist_interface
  use netcdf_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: fileio_interface_read
  public :: fileio_interface_write
  interface fileio_interface_read
     module procedure read_bufr_info
     module procedure read_fv3
     module procedure read_hsa
     module procedure read_sonde_filenames
     module procedure read_tcinfo
     module procedure read_vdm
  end interface fileio_interface_read
  interface fileio_interface_varinfo
     module procedure varinfo_bufrlocs
     module procedure varinfo_sonde_meteo
  end interface fileio_interface_varinfo
  interface fileio_interface_write
     module procedure write_bufrlocs
     module procedure write_hsa
     module procedure write_sonde_drift_error
     module procedure write_sonde_decode_table
     module procedure write_sonde_meteo
  end interface fileio_interface_write
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! read_bufr_info.f90

  ! DESCRIPTION:

  ! This subroutine parses a JSON formatted file and defines the BUFR
  ! record attributes as specified by the user.  

  ! INPUT VARIABLES:

  ! * bufr_info; a FORTRAN bufr_info_struct variable containing the
  !   BUFR information JSON file path.

  ! OUTPUT VARIABLES:

  ! * bufr_info; a FORTRAN bufr_info_struct variable containing the
  !   BUFR record attributes specified by the user.

  !-----------------------------------------------------------------------

  subroutine read_bufr_info(bufr_info)

    ! Define variables passed to routine

    type(bufr_info_struct)                                              :: bufr_info

    !=====================================================================

    ! Define local variables

    call json_interface_read(bufr_info)

    !=====================================================================

  end subroutine read_bufr_info

  !=======================================================================

  ! SUBROUTINE:

  ! read_fv3.f90

  ! DESCRIPTION:

  ! This subroutine reads Network Common Data Format (netcdf) files
  ! produced by the Finite Volume Cubed Sphere (FV3) model and defines
  ! the variables within the FORTRAN fv3_struct variable.

  ! INPUT VARIABLES:

  ! * fv3; a FORTRAN fv3_struct variable.

  ! * windrotate; a FORTRAN windrotate variable.

  ! OUTPUT VARIABLES:

  ! * fv3; a FORTRAN fv3_struct variable containing both the contents
  !   for the ingested FV3 netcdf files and the variables computed
  !   from the respective ingested variables.

  ! * windrotate; a FORTRAN windrotate variable containing the
  !   coordinate rotation values to translate the forecast model winds
  !   from model space to a Earth-relative coordinate system.  

  !-----------------------------------------------------------------------

  subroutine read_fv3(fv3,windrotate)

    ! Define variables passed to routine

    type(fv3_struct)                                                    :: fv3
    type(windrotate_struct)                                             :: windrotate

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(is_global)   call read_fv3_global(fv3)
    if(is_regional) call read_fv3_regional(fv3,windrotate)

    !=====================================================================
    
  end subroutine read_fv3

  !=======================================================================

  ! SUBROUTINE:

  ! read_fv3_global.f90

  ! DESCRIPTION:

  ! This subroutine reads Network Common Data Format (netcdf) files
  ! produced by the Finite Volume Cubed Sphere (FV3) global model grid
  ! configuration and defines the variables within the FORTRAN
  ! fv3_struct variable.

  ! NOTE: The FV3 3-dimensional variables are ordered from the bottom
  ! to top (e.g., the top-most level within the array is the surface);
  ! this subroutine reorders the 3-dimensional variables such that the
  ! surface is the first level within the array.

  ! INPUT VARIABLES:

  ! * fv3; a FORTRAN fv3_struct variable.

  ! OUTPUT VARIABLES:

  ! * fv3; a FORTRAN fv3_struct variable containing both the contents
  !   for the ingested FV3 netcdf files and the variables computed
  !   from the respective ingested variables.

  !-----------------------------------------------------------------------

  subroutine read_fv3_global(fv3)

    ! Define variables passed to routine

    type(fv3_struct)                                                    :: fv3

    ! Define variables computed within routine

    character(len=100)                                                  :: dimname
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:,:),           allocatable :: nc_real_3d
    real(r_kind),               dimension(:,:),             allocatable :: nc_real_2d
    real(r_kind),               dimension(:),               allocatable :: ak
    real(r_kind),               dimension(:),               allocatable :: bk
    real(r_kind),               dimension(:),               allocatable :: lat
    real(r_kind),               dimension(:),               allocatable :: lon
    integer                                                             :: stop_coord
    integer                                                             :: strt_coord
    integer                                                             :: nx
    integer                                                             :: ny
    integer                                                             :: nz
    integer                                                             :: zcoord
    
    ! Define counting variables

    integer                                                             :: i, j, k, l
    
    !=====================================================================    

    ! Define local variables
    
    dimname     = 'lon'
    call netcdf_interface_getdim(fv3_orog_filename(1),dimname,nx)
    fv3%nx      = nx
    dimname     = 'lat'
    call netcdf_interface_getdim(fv3_orog_filename(1),dimname,ny)
    fv3%ny      = ny
    dimname     = 'phalf'
    call netcdf_interface_getdim(fv3_static_filename,dimname,nz)
    fv3%nz      = (nz - 1)
    fv3%ncoords = (size(fv3_orog_filename)*fv3%nx*fv3%ny)
    call variable_interface_setup_struct(fv3)

    ! Allocate memory for local variables

    if(.not. allocated(nc_real_3d)) allocate(nc_real_3d(nx,ny,fv3%nz))
    if(.not. allocated(nc_real_2d)) allocate(nc_real_2d(nx,ny))
    if(.not. allocated(ak))         allocate(ak(fv3%nz))
    if(.not. allocated(bk))         allocate(bk(fv3%nz))
    if(.not. allocated(lat))        allocate(lat(fv3%ny))
    if(.not. allocated(lon))        allocate(lon(fv3%nx))
    
    ! Loop through local variable

    do j = 1, size(fv3_tracer_filename)
    
       ! Define local variables

       strt_coord = (j - 1)*(fv3%nx*fv3%ny) + 1
       stop_coord = j*(fv3%nx*fv3%ny)
       if(debug) write(6,500) trim(adjustl(fv3_tracer_filename(j)))
       varname = 'sphum'
       call netcdf_interface_getvar(fv3_tracer_filename(j),varname,        &
            & nc_real_3d)
    
       ! Loop through local variable

       do i = 1, fv3%nz

          ! Define local variables

          zcoord                              = (fv3%nz - i) + 1
          fv3%q(strt_coord:stop_coord,zcoord) =                            &
               & reshape(nc_real_3d(:,:,i),                                &
               & shape(fv3%q(strt_coord:stop_coord,zcoord)))

       end do ! do i = 1, fv3%nz
       
    end do ! do j = 1, size(fv3_tracer_filename)
    
    ! Check local variable and proceed accordingly
    
    if(debug) then

       ! Loop through local variable

       do i = 1, fv3%nz

          ! Define local variables

          zcoord = (fv3%nz - i) + 1 
          write(6,501), trim(adjustl(varname)), zcoord,                    &
               & minval(fv3%q(:,zcoord)), maxval(fv3%q(:,zcoord))

       end do ! do i = 1, fv3%nz

    end if ! if(debug)

    ! Loop through local variable

    do j = 1, size(fv3_dyns_filename)

       ! Define local variables

       strt_coord = (j - 1)*(fv3%nx*fv3%ny) + 1
       stop_coord = j*(fv3%nx*fv3%ny)
       if(debug) write(6,500) trim(adjustl(fv3_dyns_filename(j)))
       varname = 'T'
       call netcdf_interface_getvar(fv3_dyns_filename(j),varname,          &
            & nc_real_3d)
    
       ! Loop through local variable

       do i = 1, fv3%nz

          ! Define local variables

          zcoord                              = (fv3%nz - i) + 1       
          fv3%t(strt_coord:stop_coord,zcoord) =                            &
               & reshape(nc_real_3d(:,:,i),                                &
               & shape(fv3%t(strt_coord:stop_coord,zcoord)))

       end do ! do i = 1, fv3%nz
       
    end do ! do j = 1, size(fv3_dyns_filename)      

    ! Check local variable and proceed accordingly
    
    if(debug) then

       ! Loop through local variable

       do i = 1, fv3%nz

          ! Define local variables

          zcoord = (fv3%nz - i) + 1 
          write(6,501), trim(adjustl(varname)), zcoord,                    &
               & minval(fv3%t(:,zcoord)), maxval(fv3%t(:,zcoord))

       end do ! do i = 1, fv3%nz

    end if ! if(debug)

    ! Loop through local variable

    do j = 1, size(fv3_dyns_filename)

       ! Define local variables

       strt_coord = (j - 1)*(fv3%nx*fv3%ny) + 1
       stop_coord = j*(fv3%nx*fv3%ny)
       if(debug) write(6,500) trim(adjustl(fv3_dyns_filename(j)))
       varname = 'ua'
       call netcdf_interface_getvar(fv3_dyns_filename(j),varname,          &
            & nc_real_3d)
    
       ! Loop through local variable

       do i = 1, fv3%nz

          ! Define local variables

          zcoord                              = (fv3%nz - i) + 1       
          fv3%ua(strt_coord:stop_coord,zcoord) =                           &
               & reshape(nc_real_3d(:,:,i),                                &
               & shape(fv3%ua(strt_coord:stop_coord,zcoord)))

       end do ! do i = 1, fv3%nz
       
    end do ! do j = 1, size(fv3_dyns_filename)      

    ! Check local variable and proceed accordingly
    
    if(debug) then

       ! Loop through local variable

       do i = 1, fv3%nz

          ! Define local variables

          zcoord = (fv3%nz - i) + 1 
          write(6,501), trim(adjustl(varname)), zcoord,                    &
               & minval(fv3%ua(:,zcoord)), maxval(fv3%ua(:,zcoord))

       end do ! do i = 1, fv3%nz

    end if ! if(debug)

    ! Loop through local variable

    do j = 1, size(fv3_dyns_filename)

       ! Define local variables

       strt_coord = (j - 1)*(fv3%nx*fv3%ny) + 1
       stop_coord = j*(fv3%nx*fv3%ny)
       if(debug) write(6,500) trim(adjustl(fv3_dyns_filename(j)))
       varname = 'va'
       call netcdf_interface_getvar(fv3_dyns_filename(j),varname,          &
            & nc_real_3d)
    
       ! Loop through local variable

       do i = 1, fv3%nz

          ! Define local variables

          zcoord                              = (fv3%nz - i) + 1       
          fv3%va(strt_coord:stop_coord,zcoord) =                           &
               & reshape(nc_real_3d(:,:,i),                                &
               & shape(fv3%va(strt_coord:stop_coord,zcoord)))

       end do ! do i = 1, fv3%nz
       
    end do ! do j = 1, size(fv3_dyns_filename)      

    ! Check local variable and proceed accordingly
    
    if(debug) then

       ! Loop through local variable

       do i = 1, fv3%nz

          ! Define local variables

          zcoord = (fv3%nz - i) + 1 
          write(6,501), trim(adjustl(varname)), zcoord,                    &
               & minval(fv3%va(:,zcoord)), maxval(fv3%va(:,zcoord))

       end do ! do i = 1, fv3%nz

    end if ! if(debug)

    ! Define local variables

    fv3%psfc = 0.0

    ! Loop through local variable

    do j = 1, size(fv3_dyns_filename)

       ! Define local variables

       strt_coord = (j - 1)*(fv3%nx*fv3%ny) + 1
       stop_coord = j*(fv3%nx*fv3%ny)
       if(debug) write(6,500) trim(adjustl(fv3_dyns_filename(j)))
       varname = 'delp'
       call netcdf_interface_getvar(fv3_dyns_filename(j),varname,          &
            & nc_real_3d)
    
       ! Loop through local variable

       do i = 1, fv3%nz

          ! Define local variables
     
          fv3%psfc(strt_coord:stop_coord) =                                &
               & fv3%psfc(strt_coord:stop_coord) +                         &
               & reshape(nc_real_3d(:,:,i),                                &
               & shape(fv3%psfc(strt_coord:stop_coord)))

       end do ! do i = 1, fv3%nz
       
    end do ! do j = 1, size(fv3_dyns_filename)

    ! Deallocate memory for local variables

    if(allocated(nc_real_3d)) deallocate(nc_real_3d)

    ! Define local variables

    if(debug) write(6,502) 'psfc', minval(fv3%psfc), maxval(fv3%psfc)    
    if(debug) write(6,500) trim(adjustl(fv3_static_filename))
    varname = 'hyam'
    call netcdf_interface_getvar(fv3_static_filename,varname,ak)
    varname = 'hybm'
    call netcdf_interface_getvar(fv3_static_filename,varname,bk)

    ! Allocate memory for local variables

    if(.not. allocated(nc_real_3d)) allocate(nc_real_3d(nx,(ny+1),fv3%nz))
    
    ! Loop through local variable

    do j = 1, size(fv3_dyns_filename)

       ! Define local variables

       strt_coord = (j - 1)*(fv3%nx*fv3%ny) + 1
       stop_coord = j*(fv3%nx*fv3%ny)
       if(debug) write(6,500) trim(adjustl(fv3_dyns_filename(j)))
       varname = 'u'
       call netcdf_interface_getvar(fv3_dyns_filename(j),varname,          &
            & nc_real_3d)
    
       ! Loop through local variable

       do i = 1, fv3%nz

          ! Define local variables

          zcoord            = (fv3%nz - i) + 1
          fv3%u(:,:,zcoord) = nc_real_3d(:,:,i)

       end do ! do i = 1, fv3%nz
       
    end do ! do j = 1, size(fv3_dyns_filename)      

    ! Check local variable and proceed accordingly
    
    if(debug) then

       ! Loop through local variable

       do i = 1, fv3%nz

          ! Define local variables

          zcoord = (fv3%nz - i) + 1 
          write(6,501), trim(adjustl(varname)), zcoord,                    &
               & minval(fv3%u(:,:,zcoord)), maxval(fv3%u(:,:,zcoord))

       end do ! do i = 1, fv3%nz

    end if ! if(debug)

    ! Deallocate memory for local variables

    if(allocated(nc_real_3d)) deallocate(nc_real_3d)

    ! Allocate memory for local variables

    if(.not. allocated(nc_real_3d)) allocate(nc_real_3d((nx+1),ny,fv3%nz))

    ! Loop through local variable

    do j = 1, size(fv3_dyns_filename)

       ! Define local variables

       strt_coord = (j - 1)*(fv3%nx*fv3%ny) + 1
       stop_coord = j*(fv3%nx*fv3%ny)
       if(debug) write(6,500) trim(adjustl(fv3_dyns_filename(j)))
       varname = 'v'
       call netcdf_interface_getvar(fv3_dyns_filename(j),varname,          &
            & nc_real_3d)
    
       ! Loop through local variable

       do i = 1, fv3%nz

          ! Define local variables

          zcoord            = (fv3%nz - i) + 1       
          fv3%v(:,:,zcoord) = nc_real_3d(:,:,i)
          
       end do ! do i = 1, fv3%nz
       
    end do ! do j = 1, size(fv3_dyns_filename)      

    ! Check local variable and proceed accordingly
    
    if(debug) then

       ! Loop through local variable

       do i = 1, fv3%nz

          ! Define local variables

          zcoord = (fv3%nz - i) + 1 
          write(6,501), trim(adjustl(varname)), zcoord,                    &
               & minval(fv3%v(:,:,zcoord)), maxval(fv3%v(:,:,zcoord))

       end do ! do i = 1, fv3%nz

    end if ! if(debug)

    ! Deallocate memory for local variables

    if(allocated(nc_real_3d)) deallocate(nc_real_3d)
    
    ! Loop through local variable

    do i = 1, fv3%nz

       ! Define local variables
       
       zcoord = (fv3%nz - i) + 1
       
       ! Compute local variables

       fv3%p(:,zcoord) = ak(i)*1.0e5 + bk(i)*fv3%psfc(:)       
       if(debug) write(6,501), 'P', zcoord, minval(fv3%p(:,zcoord)),       &
            & maxval(fv3%p(:,zcoord))

    end do ! do i = 1, fv3%nz

    ! Loop through local variable

    do i = 1, size(fv3_orog_filename)

       ! Define local variables

       strt_coord = (i - 1)*(fv3%nx*fv3%ny) + 1
       stop_coord = i*(fv3%nx*fv3%ny)
       if(debug) write(6,500) trim(adjustl(fv3_orog_filename(i)))
       varname    = 'lat'
       call netcdf_interface_getvar(fv3_orog_filename(i),varname,lat)
       varname    = 'lon'
       call netcdf_interface_getvar(fv3_orog_filename(i),varname,lon)       
       l          = strt_coord

       ! Loop through local variable

       do j = 1, fv3%ny

          ! Loop through local variable

          do k = 1, fv3%nx

             ! Define local variables

             fv3%lat(l) = lat(j)
             fv3%lon(l) = lon(k)
             l          = l + 1

          end do ! do k = 1, fv3%nx

       end do ! do j = 1, fv3%ny

       ! Define local variables

       varname                          = 'orog_raw'
       call netcdf_interface_getvar(fv3_orog_filename(i),varname,          &
            & nc_real_2d)
       fv3%orog(strt_coord:stop_coord)  =                                  &
            & reshape(nc_real_2d,shape(fv3%orog(strt_coord:stop_coord)))
       varname                          = 'slmsk'
       call netcdf_interface_getvar(fv3_orog_filename(i),varname,          &
            & nc_real_2d)
       fv3%slmsk(strt_coord:stop_coord) =                                  &
            & reshape(nc_real_2d,shape(fv3%slmsk(strt_coord:stop_coord)))

    end do ! do i = 1, size(fv3_orog_filename)

    ! Define local variables
    
    if(debug) write(6,502) 'lat', minval(fv3%lat), maxval(fv3%lat)
    if(debug) write(6,502) 'lon', minval(fv3%lon), maxval(fv3%lon)
    if(debug) write(6,502) 'orog', minval(fv3%orog), maxval(fv3%orog)
    if(debug) write(6,502) 'slmsk', minval(fv3%slmsk), maxval(fv3%slmsk)
    
    ! Deallocate memory for local variables

    if(allocated(nc_real_3d)) deallocate(nc_real_3d)
    if(allocated(nc_real_2d)) deallocate(nc_real_2d)
    if(allocated(ak))         deallocate(ak)
    if(allocated(bk))         deallocate(bk)
    if(allocated(lat))        deallocate(lat)
    if(allocated(lon))        deallocate(lon)

    ! Define local variables

500 format(/,'READ_FV3_GLOBAL: Reading file ',a,'.',/)
501 format('READ_FV3_GLOBAL: Variable/Level/Min/Max: ',a,1x,i3.3,          &
         & 2(1x,f13.5))
502 format('READ_FV3_GLOBAL: Variable/Min/Max: ',a,2(1x,f13.5))
    
    !=====================================================================
    
  end subroutine read_fv3_global
  
  !=======================================================================

  ! SUBROUTINE:

  ! read_fv3_regional.f90

  ! DESCRIPTION:

  ! This subroutine reads Network Common Data Format (netcdf) files
  ! produced by the Finite Volume Cubed Sphere (FV3) regional model
  ! grid configuration and defines the variables within the FORTRAN
  ! fv3_struct variable.

  ! NOTE: The FV3 3-dimensional variables are ordered from the bottom
  ! to top (e.g., the top-most level within the array is the surface);
  ! this subroutine reorders the 3-dimensional variables such that the
  ! surface is the first level within the array.

  ! INPUT VARIABLES:

  ! * fv3; a FORTRAN fv3_struct variable.

  ! * windrotate; a FORTRAN windrotate variable.

  ! OUTPUT VARIABLES:

  ! * fv3; a FORTRAN fv3_struct variable containing both the contents
  !   for the ingested FV3 netcdf files and the variables computed
  !   from the respective ingested variables.

  ! * windrotate; a FORTRAN windrotate variable containing the
  !   coordinate rotation values to translate the forecast model winds
  !   from model space to a Earth-relative coordinate system.

  !-----------------------------------------------------------------------

  subroutine read_fv3_regional(fv3,windrotate)

    ! Define variables passed to routine

    type(fv3_struct)                                                    :: fv3
    type(windrotate_struct)                                             :: windrotate

    ! Define variables computed within routine

    type(fv3_gridtrans_struct)                                          :: fv3_gridtrans
    character(len=100)                                                  :: dimname
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:,:),           allocatable :: nc_real_3d
    real(r_kind),               dimension(:,:),             allocatable :: nc_real_2d
    real(r_kind),               dimension(:),               allocatable :: ak
    real(r_kind),               dimension(:),               allocatable :: bk
    integer                                                             :: nx
    integer                                                             :: ny
    integer                                                             :: nz
    integer                                                             :: zcoord
    
    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================    

    ! Define local variables
    
    dimname = 'lon'
    call netcdf_interface_getdim(fv3_orog_filename(1),dimname,nx)
    fv3%nx  = nx
    dimname = 'lat'
    call netcdf_interface_getdim(fv3_orog_filename(1),dimname,ny)
    fv3%ny  = ny
    dimname = 'phalf'
    call netcdf_interface_getdim(fv3_static_filename,dimname,nz)
    fv3%nz  = (nz - 1)
    call variable_interface_setup_struct(fv3)

    ! Allocate memory for local variables

    if(.not. allocated(nc_real_3d)) allocate(nc_real_3d(nx,ny,fv3%nz))
    if(.not. allocated(nc_real_2d)) allocate(nc_real_2d(nx,ny))
    if(.not. allocated(ak))         allocate(ak(fv3%nz))
    if(.not. allocated(bk))         allocate(bk(fv3%nz))   
    
    ! Define local variables

    if(debug) write(6,500) trim(adjustl(fv3_tracer_filename(1)))
    varname = 'sphum'
    call netcdf_interface_getvar(fv3_tracer_filename(1),varname,           &
         & nc_real_3d)
    
    ! Loop through local variable

    do i = 1, fv3%nz

       ! Define local variables

       zcoord          = (fv3%nz - i) + 1
       fv3%q(:,zcoord) = reshape(nc_real_3d(:,:,i),                        &
            & shape(fv3%q(:,zcoord)))
       if(debug) write(6,501), trim(adjustl(varname)), zcoord,             &
            & minval(fv3%q(:,zcoord)), maxval(fv3%q(:,zcoord))

    end do ! do i = 1, fv3%nz

    ! Define local variables

    if(debug) write(6,500) trim(adjustl(fv3_dyns_filename(1)))
    varname = 'T'
    call netcdf_interface_getvar(fv3_dyns_filename(1),varname,             &
         & nc_real_3d)
    
    ! Loop through local variable

    do i = 1, fv3%nz

       ! Define local variables

       zcoord          = (fv3%nz - i) + 1       
       fv3%t(:,zcoord) = reshape(nc_real_3d(:,:,i),                        &
            & shape(fv3%t(:,zcoord)))
       if(debug) write(6,501), trim(adjustl(varname)), zcoord,             &
            & minval(fv3%t(:,zcoord)), maxval(fv3%t(:,zcoord))

    end do ! do i = 1, fv3%nz

    ! Define local variables

    if(debug) write(6,500) trim(adjustl(fv3_dyns_filename(1)))
    varname = 'ua'
    call netcdf_interface_getvar(fv3_dyns_filename(1),varname,             &
         & nc_real_3d)
    
    ! Loop through local variable

    do i = 1, fv3%nz

       ! Define local variables

       zcoord           = (fv3%nz - i) + 1       
       fv3%ua(:,zcoord) = reshape(nc_real_3d(:,:,i),                        &
            & shape(fv3%ua(:,zcoord)))
       if(debug) write(6,501), trim(adjustl(varname)), zcoord,             &
            & minval(fv3%ua(:,zcoord)), maxval(fv3%ua(:,zcoord))

    end do ! do i = 1, fv3%nz

    ! Define local variables

    if(debug) write(6,500) trim(adjustl(fv3_dyns_filename(1)))
    varname = 'va'
    call netcdf_interface_getvar(fv3_dyns_filename(1),varname,             &
         & nc_real_3d)
    
    ! Loop through local variable

    do i = 1, fv3%nz

       ! Define local variables

       zcoord           = (fv3%nz - i) + 1       
       fv3%va(:,zcoord) = reshape(nc_real_3d(:,:,i),                        &
            & shape(fv3%va(:,zcoord)))
       if(debug) write(6,501), trim(adjustl(varname)), zcoord,             &
            & minval(fv3%va(:,zcoord)), maxval(fv3%va(:,zcoord))

    end do ! do i = 1, fv3%nz  

    ! Define local variables

    if(debug) write(6,500) trim(adjustl(fv3_dyns_filename(1)))
    varname  = 'delp'
    call netcdf_interface_getvar(fv3_dyns_filename(1),varname,             &
         & nc_real_3d)
    fv3%psfc = 0.0
    
    ! Loop through local variable

    do i = 1, fv3%nz

       ! Define local variables

       fv3%psfc = fv3%psfc + reshape(nc_real_3d(:,:,i),shape(fv3%psfc))

    end do ! do i = 1, fv3%nz    

    ! Define local variables

    if(debug) write(6,502) 'psfc', minval(fv3%psfc), maxval(fv3%psfc)

    ! Deallocate memory for local variables

    if(allocated(nc_real_3d)) deallocate(nc_real_3d)

    ! Allocate memory for local variables

    if(.not. allocated(nc_real_3d))                                        &
         & allocate(nc_real_3d(fv3%nx,(fv3%ny + 1),fv3%nz))

    ! Define local variables

    if(debug) write(6,500) trim(adjustl(fv3_dyns_filename(1)))
    varname = 'u'
    call netcdf_interface_getvar(fv3_dyns_filename(1),varname,             &
         & nc_real_3d)
    
    ! Loop through local variable

    do i = 1, fv3%nz

       ! Define local variables

       zcoord            = (fv3%nz - i) + 1       
       fv3%u(:,:,zcoord) = nc_real_3d(:,:,i)
       if(debug) write(6,501), trim(adjustl(varname)), zcoord,             &
            & minval(fv3%u(:,:,zcoord)), maxval(fv3%u(:,:,zcoord))

    end do ! do i = 1, fv3%nz

    ! Deallocate memory for local variables

    if(allocated(nc_real_3d)) deallocate(nc_real_3d)
    
    ! Allocate memory for local variables

    if(.not. allocated(nc_real_3d))                                        &
         & allocate(nc_real_3d((fv3%nx + 1),fv3%ny,fv3%nz))

    ! Define local variables

    if(debug) write(6,500) trim(adjustl(fv3_dyns_filename(1)))
    varname = 'v'
    call netcdf_interface_getvar(fv3_dyns_filename(1),varname,             &
         & nc_real_3d)
    
    ! Loop through local variable

    do i = 1, fv3%nz

       ! Define local variables

       zcoord            = (fv3%nz - i) + 1       
       fv3%v(:,:,zcoord) = nc_real_3d(:,:,i)
       if(debug) write(6,501), trim(adjustl(varname)), zcoord,             &
            & minval(fv3%v(:,:,zcoord)), maxval(fv3%v(:,:,zcoord))

    end do ! do i = 1, fv3%nz

    ! Deallocate memory for local variables

    if(allocated(nc_real_3d)) deallocate(nc_real_3d)

    ! Define local variables

    if(debug) write(6,500) trim(adjustl(fv3_static_filename))
    varname = 'hyam'
    call netcdf_interface_getvar(fv3_static_filename,varname,ak)
    varname = 'hybm'
    call netcdf_interface_getvar(fv3_static_filename,varname,bk)

    ! Loop through local variable

    do i = 1, fv3%nz

       ! Define local variables

       zcoord = (fv3%nz - i) + 1
    
       ! Compute local variables

       fv3%p(:,zcoord) = ak(i)*1.0e5 + bk(i)*fv3%psfc(:)       
       if(debug) write(6,501), 'P', zcoord, minval(fv3%p(:,zcoord)),       &
            & maxval(fv3%p(:,zcoord))

    end do ! do i = 1, fv3%nz

    ! Define local variables

    fv3_gridtrans%nx         = fv3%nx
    fv3_gridtrans%ny         = fv3%ny
    call fv3_gridtrans_setup(fv3_gridtrans)
    if(debug) write(6,500) trim(adjustl(fv3_gridspec_filename(1)))
    varname                  = 'grid_lat'
    call netcdf_interface_getvar(fv3_gridspec_filename(1),varname,         &
         & fv3_gridtrans%lat)
    varname                  = 'grid_latt'
    call netcdf_interface_getvar(fv3_gridspec_filename(1),varname,         &
         & fv3_gridtrans%latt)
    varname                  = 'grid_lon'
    call netcdf_interface_getvar(fv3_gridspec_filename(1),varname,         &
         & fv3_gridtrans%lon)
    varname                  = 'grid_lont'
    call netcdf_interface_getvar(fv3_gridspec_filename(1),varname,         &
         & fv3_gridtrans%lont)
    fv3_gridtrans%grid_ratio = grid_ratio

    ! Compute local variables

    call fv3_gridtrans_anlgrid(fv3_gridtrans)

    ! Define local variables

    windrotate%nx = fv3_gridtrans%nx
    windrotate%ny = fv3_gridtrans%ny
    call windrotate_setup(windrotate)

    ! Compute local variables

    call fv3_windrotate_compute(fv3_gridtrans,windrotate)

    ! Define local variables

    fv3%lat = fv3_gridtrans%lat_in
    if(debug) write(6,502) 'latitude', minval(fv3%lat), maxval(fv3%lat)
    fv3%lon = fv3_gridtrans%lon_in
    if(debug) write(6,502) 'longitude', minval(fv3%lon), maxval(fv3%lon)

    ! Deallocate memory for local variables

    call fv3_gridtrans_cleanup(fv3_gridtrans)
    
    ! Define local variables
    
    if(debug) write(6,500) trim(adjustl(fv3_orog_filename(1)))
    varname   = 'slmsk'
    call netcdf_interface_getvar(fv3_orog_filename(1),varname,nc_real_2d)
    fv3%slmsk = reshape(nc_real_2d,shape(fv3%slmsk))
    if(debug) write(6,502) trim(adjustl(varname)), minval(fv3%slmsk),      &
         & maxval(fv3%slmsk)
    varname   = 'orog_raw'
    call netcdf_interface_getvar(fv3_orog_filename(1),varname,nc_real_2d)
    fv3%orog  = reshape(nc_real_2d,shape(fv3%slmsk))
    if(debug) write(6,502) trim(adjustl(varname)), minval(fv3%orog),       &
         & maxval(fv3%orog)
    
    ! Deallocate memory for local variables

    if(allocated(nc_real_3d)) deallocate(nc_real_3d)
    if(allocated(nc_real_2d)) deallocate(nc_real_2d)
    if(allocated(ak))         deallocate(ak)
    if(allocated(bk))         deallocate(bk)

    ! Define local variables

500 format(/,'READ_FV3_REGIONAL: Reading file ',a,'.',/)
501 format('READ_FV3_REGIONAL: Variable/Level/Min/Max: ',a,1x,i3.3,        &
         & 2(1x,f13.5))
502 format('READ_FV3_REGIONAL: Variable/Min/Max: ',a,2(1x,f13.5))
    
    !=====================================================================
    
  end subroutine read_fv3_regional
  
  !=======================================================================

  ! SUBROUTINE:

  ! read_hsa.f90

  ! DESCRIPTION:

  ! This subroutine reads a National Oceanic and Atmospheric
  ! Administration (NOAA) Atlantic Oceanographic and Meteorological
  ! Laboratory (AOML) Hurricane Research Division (HRD) HRD Spline
  ! Analysis (HSA) formatted observation file.

  ! INPUT VARIABLES:

  ! * hsa; a FORTRAN hsa_struct variable.

  ! OUTPUT VARIABLES:

  ! * hsa; a FORTRAN hsa_struct variable containing the HSA file
  !   contents.

  !-----------------------------------------------------------------------

  subroutine read_hsa(hsa)

    ! Define variables passed to routine

    type(hsa_struct)                                                    :: hsa

    ! Define variables computed within routine

    real(r_kind)                                                        :: dummy

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    hsa%nz = 0
    open(99,file=trim(adjustl(hsa%filename)),form='formatted')
1000 read(99,*,end=1001) dummy
    hsa%nz = hsa%nz + 1
    goto 1000
1001 continue
    close(99)
    call variable_interface_setup_struct(hsa)
    open(99,file=trim(adjustl(hsa%filename)),form='formatted')

    ! Loop through local variable

    do i = 1, hsa%nz

       ! Define local variables

       read(99,500,err=1002) hsa%wx(i), hsa%yymmdd(i), hsa%gmt(i),        &
            & hsa%lat(i), hsa%lon(i), hsa%p(i), hsa%t(i), hsa%rh(i),      &
            & hsa%z(i), hsa%u(i), hsa%v(i), hsa%tail(i)

    end do ! do i = 1, hsa%nz

    ! Define local variables

    close(99)
    return
1002 continue
500 format(i2,1x,f7.0,1x,i4,1x,2(f7.3,1x),3(f6.1,1x),f7.1,2(f6.1,1x),a4)

    !=====================================================================

  end subroutine read_hsa
  
  !=======================================================================

  ! SUBROUTINE:

  ! read_sonde_filenames.f90

  ! DESCRIPTION:

  ! This subroutine reads into an array sonde filename pathes.

  ! INPUT VARIABLES:

  ! * sonde; a FORTRAN sonde_struct variable.

  ! OUTPUT VARIABLES:

  ! * sonde; a FORTRAN sonde_struct variable containing an array of
  !   sonde file pathes.
  
  !-----------------------------------------------------------------------

  subroutine read_sonde_filenames(sonde)

    ! Define variables passed to routine

    type(sonde_struct)                                                  :: sonde

    ! Define variables computed within routine

    character(len=500)                                                  :: dummy
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    sonde%nsondes = 0
    open(99,file=trim(adjustl(sonde_filelist)),form='formatted')
1000 read(99,*,end=1001) dummy
    sonde%nsondes = sonde%nsondes + 1
    goto 1000
1001 continue
    close(99)

    ! Define local variables

    call variable_interface_setup_struct(sonde)

    ! Define local variables

    open(99,file=trim(adjustl(sonde_filelist)),form='formatted')

    ! Loop through local variable

    do i = 1, sonde%nsondes

       ! Define local variables

       read(99,*) sonde%filename(i)
       if(debug) write(6,500) trim(adjustl(sonde%filename(i)))

    end do ! do i = 1, sonde%nsondes

    ! Define local variables

    close(99)
500 format('READ_SONDE_FILENAMES: Reading in file ',a,' to be processed.')

    !=====================================================================

  end subroutine read_sonde_filenames

  !=======================================================================

  ! SUBROUTINE:

  ! read_tcinfo.f90

  ! DESCRIPTION:

  ! This subroutine ingests an external file containing tropical
  ! cyclone information required to generate synthetic, relocated
  ! observations.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the path to the
  !   TC information file.

  ! * tcinfo; a FORTRAN tcinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * tcinfo; a FORTRAN tcinfo_struct variable now containing the
  !   tropical cyclone attributes retrieved from the user specified
  !   file.

  !-----------------------------------------------------------------------

  subroutine read_tcinfo(filename,tcinfo)

    ! Define variables passed to routine

    type(tcinfo_struct),        dimension(:),               allocatable :: tcinfo
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=1)                                                    :: dummy
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

    if(.not. allocated(tcinfo)) allocate(tcinfo(ntcs))

    ! Define local variables

    open(99,file=trim(adjustl(filename)),form='formatted')

    ! Loop through local variable

    do i = 1, ntcs

       ! Define local variables

       read(99,*) tcinfo(i)%id, tcinfo(i)%obs_clat, tcinfo(i)%mdl_clat,    &
            & tcinfo(i)%obs_clon, tcinfo(i)%mdl_clon, tcinfo(i)%obs_pcen,  &
            & tcinfo(i)%mdl_pcen, tcinfo(i)%obs_vmax, tcinfo(i)%mdl_vmax

       ! Check local variable and proceed accordingly
       
       if(debug) then

          ! Define local variables

          write(6,'(/)')
          write(6,501)
          write(6,500) tcinfo(i)%id, tcinfo(i)%obs_clat,                   &
               & tcinfo(i)%mdl_clat, tcinfo(i)%obs_clon,                   &
               & tcinfo(i)%mdl_clon, tcinfo(i)%obs_pcen,                   &
               & tcinfo(i)%mdl_pcen, tcinfo(i)%obs_vmax,                   &
               & tcinfo(i)%mdl_vmax 
          
       end if ! if(debug)

       ! Check local variable and proceed accordingly

       if(is_fv3) then

          ! Define local variables

          tcinfo(i)%mdl_clon = tcinfo(i)%mdl_clon + 360.0
          tcinfo(i)%obs_clon = tcinfo(i)%obs_clon + 360.0

       end if ! if(is_fv3)

    end do ! do i = 1, ntcs

    ! Define local variables

    close(99)
500 format(a3,8(1x,f13.5))
501 format('READ_TCINFO: Read the following TC information record:')

    !=====================================================================

  end subroutine read_tcinfo

  !=======================================================================

  ! SUBROUTINE:

  ! read_vdm.f90

  ! DESCRIPTION:

  ! This subroutine ingests formatted vortex data message (VDM) files
  ! and populates the FORTRAN vdm_struct variable arrays.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the path to the
  !   external file containing the vortex data messages.

  ! * vdm; a FORTRAN vdm_struct variable.

  ! OUTPUT VARIABLES:

  ! * vdm; a FORTRAN vdm_struct variable now containing the vortex
  !   data message (VDM) attributes retrieved from the user specified
  !   files.

  !-----------------------------------------------------------------------

  subroutine read_vdm(filename,vdm)

    ! Define variables passed to routine

    type(vdm_struct)                                                    :: vdm
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=500)                                                  :: vdm_filename
    character(len=1)                                                    :: dummy
    integer                                                             :: nobs
    
    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    vdm%nvdm = 0
    vdm%nobs = 0
    open(99,file=trim(adjustl(filename)),form='formatted')
1000 read(99,*,end=1003) vdm_filename
    vdm%nvdm = vdm%nvdm + 1
    nobs = 0
    open(98,file=trim(adjustl(vdm_filename)),form='formatted')
1001 read(98,*,end=1002) dummy
    nobs = nobs + 1
    goto 1001
1002 continue
    vdm%nobs = max(vdm%nobs,(nobs - 1))
    goto 1000
1003 continue
    close(99)
    call variable_interface_setup_struct(vdm)
    open(99,file=trim(adjustl(filename)),form='formatted')

    ! Check local variable and proceed accordingly

    if(debug) write(6,'(/)')
    
    ! Loop through local variable

    do i = 1, size(vdm%filename)

       ! Define local variables

       read(99,*) vdm%filename(i)

       ! Check local variable and proceed accordingly
       
       if(debug) write(6,500) trim(adjustl(vdm%filename(i)))
       
    end do ! do i = 1, size(vdm%filename)

    ! Check local variable and proceed accordingly

    if(debug) write(6,'(/)')

    ! Define local variables

    close(99)

    ! Loop through local variable

    do i = 1, size(vdm%filename)

       ! Define local variables

       open(99,file=trim(adjustl(vdm%filename(i))),form='formatted')
       read(99,*) vdm%fix_time(i), vdm%fix_lat(i), vdm%fix_lon(i)

       ! Check local variable and proceed accordingly

       if(vdm%fix_lat(i) .eq. vdm_spval) vdm%fix_lat(i) = spval
       if(vdm%fix_lon(i) .eq. vdm_spval) vdm%fix_lon(i) = spval
       
       ! Loop through local variable

       do j = 1, vdm%nobs

          ! Define local variables

          read(99,*,end=1004) vdm%obs_time(i,j), vdm%obs_plev(i,j),       &
               & vdm%obs_alt(i,j), vdm%obs_dist(i,j), vdm%obs_head(i,j),  &
               & vdm%obs_wdir(i,j), vdm%obs_wspd(i,j)

          ! Check local variable and proceed accordingly

          if(vdm%obs_plev(i,j) .eq. vdm_spval) vdm%obs_plev(i,j) = spval
          if(vdm%obs_alt(i,j)  .eq. vdm_spval) vdm%obs_alt(i,j)  = spval
          if(vdm%obs_dist(i,j) .eq. vdm_spval) vdm%obs_dist(i,j) = spval
          if(vdm%obs_head(i,j) .eq. vdm_spval) vdm%obs_head(i,j) = spval
          if(vdm%obs_wdir(i,j) .eq. vdm_spval) vdm%obs_wdir(i,j) = spval
          if(vdm%obs_wspd(i,j) .eq. vdm_spval) vdm%obs_wspd(i,j) = spval
          
       end do ! do j = 1, vdm%nobs

       ! Define local variables

1004   continue
       close(99)
       
    end do ! do i = 1, size(vdm%filename)
    
500 format('READ_VDM: Found vortex data message file ', a,'.')

    !=====================================================================

  end subroutine read_vdm

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_bufrlocs.f90

  ! DESCRIPTION:

  ! This subroutine defines the FORTRAN varinfo_struct variable used
  ! to write the appropriate variables contained within the FORTRAN
  ! fcstmdl_struct variable to external file formats.

  ! INPUT VARIABLES:

  ! * fcstmdl; a FORTRAN fcstmdl_struct variable.

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_bufrlocs(fcstmdl,varinfo)

    ! Define variables passed to routine

    type(fcstmdl_struct)                                                :: fcstmdl
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
    varinfo%nvars  = 2
    varinfo%nattrs = 2
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Define local variables

    dimname(1) = 'nobs'
    dimval(1)  = fcstmdl%nobs
    dimname(2) = 'nz'
    dimval(2)  = fcstmdl%nz

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    ! Define local variables

    varinfo%varattrs(1,1,1) = 'title'
    varinfo%varattrs(1,1,2) = 'Longitude'
    varinfo%varattrs(1,2,1) = 'units'
    varinfo%varattrs(1,2,2) = 'degrees'   
    varinfo%varname(1)      = 'lons'
    varinfo%varndims(1)     = 2
    varinfo%varnattrs(1)    = 2
    varinfo%vartype(1)      = 'float'
    varinfo%vardimid(1,1)   = dimid(1)
    varinfo%vardimid(1,2)   = dimid(2)    
    varinfo%varattrs(2,1,1) = 'title'
    varinfo%varattrs(2,1,2) = 'Latitude'
    varinfo%varattrs(2,2,1) = 'units'
    varinfo%varattrs(2,2,2) = 'degrees'   
    varinfo%varname(2)      = 'lats'
    varinfo%varndims(2)     = 2
    varinfo%varnattrs(2)    = 2
    varinfo%vartype(2)      = 'float'
    varinfo%vardimid(2,1)   = dimid(1)
    varinfo%vardimid(2,2)   = dimid(2)
    
    ! Deallocate memory for local variables

    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)

    !=====================================================================
    
  end subroutine varinfo_bufrlocs

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_sonde_meteo.f90

  ! DESCRIPTION:

  ! This subroutine defines the FORTRAN varinfo_struct variable used
  ! to write the appropriate variables contained within the FORTRAN
  ! meteo_struct variable to external file formats.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_sonde_meteo(meteo,varinfo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo
    type(varinfo_struct)                                                :: varinfo

    ! Define variables computed within routine

    character(len=10),          dimension(:),               allocatable :: dimname
    integer,                    dimension(:),               allocatable :: dimval
    integer,                    dimension(:),               allocatable :: dimid

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    varinfo%ndims  = 1
    varinfo%nvars  = 14
    varinfo%nattrs = 2
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Define local variables

    dimname(1) = 'nz'
    dimval(1)  = meteo%nz

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    ! Define local variables

    varinfo%varattrs(1,1,1)  = 'title'
    varinfo%varattrs(1,1,2)  = 'Dewpoint Temperature'
    varinfo%varattrs(1,2,1)  = 'units'
    varinfo%varattrs(1,2,2)  = 'Kelvin'   
    varinfo%varname(1)       = 'dwpt'
    varinfo%varndims(1)      = 1
    varinfo%varnattrs(1)     = 2
    varinfo%vartype(1)       = 'double'
    varinfo%vardimid(1,1)    = dimid(1)
    varinfo%varattrs(2,1,1)  = 'title'
    varinfo%varattrs(2,1,2)  = 'Latitude'
    varinfo%varattrs(2,2,1)  = 'units'
    varinfo%varattrs(2,2,2)  = 'degrees' 
    varinfo%varname(2)       = 'lat'
    varinfo%varndims(2)      = 1
    varinfo%varnattrs(2)     = 2
    varinfo%vartype(2)       = 'double'
    varinfo%vardimid(2,1)    = dimid(1)
    varinfo%varattrs(3,1,1)  = 'title'
    varinfo%varattrs(3,1,2)  = 'Longitude'
    varinfo%varattrs(3,2,1)  = 'units'
    varinfo%varattrs(3,2,2)  = 'degrees'   
    varinfo%varname(3)       = 'lon'
    varinfo%varndims(3)      = 1
    varinfo%varnattrs(3)     = 2
    varinfo%vartype(3)       = 'double'
    varinfo%vardimid(3,1)    = dimid(1)
    varinfo%varattrs(4,1,1)  = 'title'
    varinfo%varattrs(4,1,2)  = 'Pressure'
    varinfo%varattrs(4,2,1)  = 'units'
    varinfo%varattrs(4,2,2)  = 'Pascals' 
    varinfo%varname(4)       = 'p'
    varinfo%varndims(4)      = 1
    varinfo%varnattrs(4)     = 2
    varinfo%vartype(4)       = 'double'
    varinfo%vardimid(4,1)    = dimid(1)
    varinfo%varattrs(5,1,1)  = 'title'
    varinfo%varattrs(5,1,2)  = 'Water Vapor Mixing Ratio'
    varinfo%varattrs(5,2,1)  = 'units'
    varinfo%varattrs(5,2,2)  = 'kilograms per kilogram'
    varinfo%varname(5)       = 'q'
    varinfo%varndims(5)      = 1
    varinfo%varnattrs(5)     = 2
    varinfo%vartype(5)       = 'double'
    varinfo%vardimid(5,1)    = dimid(1)
    varinfo%varattrs(6,1,1)  = 'title'
    varinfo%varattrs(6,1,2)  = 'Relative Humidity'
    varinfo%varattrs(6,2,1)  = 'units'
    varinfo%varattrs(6,2,2)  = '%'   
    varinfo%varname(6)       = 'rh'
    varinfo%varndims(6)      = 1
    varinfo%varnattrs(6)     = 2
    varinfo%vartype(6)       = 'double'
    varinfo%vardimid(6,1)    = dimid(1)
    varinfo%varattrs(7,1,1)  = 'title'
    varinfo%varattrs(7,1,2)  = 'Temperature'
    varinfo%varattrs(7,2,1)  = 'units'
    varinfo%varattrs(7,2,2)  = 'Kelvin'   
    varinfo%varname(7)       = 't'
    varinfo%varndims(7)      = 1
    varinfo%varnattrs(7)     = 2
    varinfo%vartype(7)       = 'double'
    varinfo%vardimid(7,1)    = dimid(1)
    varinfo%varattrs(8,1,1)  = 'title'
    varinfo%varattrs(8,1,2)  = 'Zonal Wind'
    varinfo%varattrs(8,2,1)  = 'units'
    varinfo%varattrs(8,2,2)  = 'meters per second'     
    varinfo%varname(8)       = 'u'
    varinfo%varndims(8)      = 1
    varinfo%varnattrs(8)     = 2
    varinfo%vartype(8)       = 'double'
    varinfo%vardimid(8,1)    = dimid(1)
    varinfo%varattrs(9,1,1)  = 'title'
    varinfo%varattrs(9,1,2)  = 'Meridional Wind'
    varinfo%varattrs(9,2,1)  = 'units'
    varinfo%varattrs(9,2,2)  = 'meters per second'    
    varinfo%varname(9)       = 'v'
    varinfo%varndims(9)      = 1
    varinfo%varnattrs(9)     = 2
    varinfo%vartype(9)       = 'double'
    varinfo%vardimid(9,1)    = dimid(1)
    varinfo%varattrs(10,1,1) = 'title'
    varinfo%varattrs(10,1,2) = 'Wind Direction'
    varinfo%varattrs(10,2,1) = 'units'
    varinfo%varattrs(10,2,2) = 'degrees from North'     
    varinfo%varname(10)      = 'wdir'
    varinfo%varndims(10)     = 1
    varinfo%varnattrs(10)    = 2
    varinfo%vartype(10)      = 'double'
    varinfo%vardimid(10,1)   = dimid(1)
    varinfo%varattrs(11,1,1) = 'title'
    varinfo%varattrs(11,1,2) = 'Wind Speed Magnitude'
    varinfo%varattrs(11,2,1) = 'units'
    varinfo%varattrs(11,2,2) = 'meters per second'    
    varinfo%varname(11)      = 'wspd'
    varinfo%varndims(11)     = 1
    varinfo%varnattrs(11)    = 2
    varinfo%vartype(11)      = 'double'
    varinfo%vardimid(11,1)   = dimid(1)
    varinfo%varattrs(12,1,1) = 'title'
    varinfo%varattrs(12,1,2) = 'Geometric Height'
    varinfo%varattrs(12,2,1) = 'units'
    varinfo%varattrs(12,2,2) = 'meters'    
    varinfo%varname(12)      = 'z'
    varinfo%varndims(12)     = 1
    varinfo%varnattrs(12)    = 2
    varinfo%vartype(12)      = 'double'
    varinfo%vardimid(12,1)   = dimid(1)
    varinfo%varattrs(13,1,1) = 'title'
    varinfo%varattrs(13,1,2) = 'Julian Date'
    varinfo%varattrs(13,2,1) = 'units'
    varinfo%varattrs(13,2,2) = 'Number of days since commencement of Julian calendar'    
    varinfo%varname(13)      = 'jdate'
    varinfo%varndims(13)     = 1
    varinfo%varnattrs(13)    = 2
    varinfo%vartype(13)      = 'double'
    varinfo%vardimid(13,1)   = dimid(1)
    varinfo%varattrs(14,1,1) = 'title'
    varinfo%varattrs(14,1,2) = 'Surface Pressure'
    varinfo%varattrs(14,2,1) = 'units'
    varinfo%varattrs(14,2,2) = 'Pascals'
    varinfo%varname(14)      = 'psfc'
    varinfo%varndims(14)     = 0
    varinfo%varnattrs(14)    = 2
    varinfo%vartype(14)      = 'double'

    ! Deallocate memory for local variables

    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)

    !=====================================================================

  end subroutine varinfo_sonde_meteo

 !=======================================================================

  ! SUBROUTINE:

  ! write_bufrlocs.f90

  ! DESCRIPTION:

  ! This subroutine writes the contents of the FORTRAN fcstmdl_struct
  ! variable to an external Network Common Data Format (netCDF) file.

  ! INPUT VARIABLES:
  
  ! * fcstmdl; a FORTRAN fcstmdl_struct variable.

  ! * filename; a FORTRAN character string containing the full-path to
  !   the netCDF file to be created.

  !-----------------------------------------------------------------------

  subroutine write_bufrlocs(fcstmdl,filename)

    ! Define variables passed to routine

    type(fcstmdl_struct)                                                :: fcstmdl
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    character(len=100)                                                  :: attrname
    character(len=100)                                                  :: varname
    real(r_double),             dimension(:,:,:),           allocatable :: work

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    call netcdf_interface_open(filename,.false.,.false.,.true.)
    call fileio_interface_varinfo(fcstmdl,varinfo)
    attrname = '_FillValue'
    call netcdf_interface_putattr(attrname,dble(spval))
    call netcdf_interface_writedef(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(work)) allocate(work(2,fcstmdl%nobs,fcstmdl%nz))

    ! Define local variables

    work = spval

    ! Loop through local variable

    do j = 1, fcstmdl%nz

       ! Loop through local variable

       do i = 1, fcstmdl%nobs

          ! Check local variable and proceed accordingly

          if(fcstmdl%usage(i,j)) then

             ! Define local variables

             work(1,i,j) = fcstmdl%lon(i)
             work(2,i,j) = fcstmdl%lat(i)

          end if ! if(fcstmdl%usage(i,j))

       end do ! do i = 1, fcstmdl%nobs

    end do ! do j = 1, fcstmdl%nz
       
    ! Loop through local variable

    do i = 1, size(work(:,1,1))

       ! Define local variables

       varname = varinfo%varname(i)
       call netcdf_interface_putvar(filename,varname,work(i,:,:))
       
    end do ! do i = 1, n1dv

    ! Deallocate memory for local variables
    
    if(allocated(work)) deallocate(work)
    call variable_interface_cleanup_struct(varinfo)

    ! Define local variables

    call netcdf_interface_close()

    !=====================================================================

  end subroutine write_bufrlocs

  !=======================================================================

  ! SUBROUTINE:

  ! write_hsa.f90

  ! DESCRIPTION:

  ! This subroutine writes a National Oceanic and Atmospheric
  ! Administration (NOAA) Atlantic Oceanographic and Meteorological
  ! Laboratory (AOML) Hurricane Research Division (HRD) HRD Spline
  ! Analysis (HSA) formatted observation file.

  ! INPUT VARIABLES:

  ! * hsa; a FORTRAN hsa_struct variable.

  !-----------------------------------------------------------------------

  subroutine write_hsa(hsa)

    ! Define variables passed to routine

    type(hsa_struct)                                                    :: hsa

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    if(debug) write(6,500) trim(adjustl(hsa%filename))
    open(99,file=trim(adjustl(hsa%filename)),form='formatted')
    
    ! Loop through local variable

    do i = 1, hsa%nz

       ! Define local variables

       write(99,501) hsa%wx(i), hsa%yymmdd(i), hsa%gmt(i), hsa%lat(i),    &
            & hsa%lon(i), hsa%p(i), hsa%t(i), hsa%rh(i), hsa%z(i),        &
            & hsa%u(i), hsa%v(i), hsa%tail(i)       

    end do ! do i = 1, hsa%nz

    ! Define local variables

    close(99)    
500 format('WRITE_HSA: Writing file ',a,'.')
501 format(i2,1x,f7.0,1x,i4,1x,2(f7.3,1x),3(f6.1,1x),f7.1,2(f6.1,1x),a4)    

    !=====================================================================

  end subroutine write_hsa

  !=======================================================================

  ! SUBROUTINE:

  ! write_sonde_drift_error.f90

  ! DESCRIPTION:

  ! This subroutine writes the values for the TEMP-DROP formatted
  ! sonde message splash location (e.g., spg attributes) and the
  ! computed/estimated splash locations from the drift-corrected sonde
  ! trajectories; the format is as follows:

  ! <observation longitude> <observation latitude> <observation fall time>
  ! <drift-corrected longitude> <drift-corrected latitude> <drift-         
  ! corrected fall time>

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the filename
  !   path to the output file.

  ! * hsa; a FORTRAN hsa_struct variable; this should contain the
  !   contents of the decoded TEMP-DROP message without the
  !   application of the drift-correction.

  ! * meteo; a FORTRAN meteo_struct variable.
  
  !-----------------------------------------------------------------------

  subroutine write_sonde_drift_error(filename,hsa,meteo)

    ! Define variables passed to routine

    type(hsa_struct)                                                    :: hsa
    type(meteo_struct)                                                  :: meteo
    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables

    open(99,file=trim(adjustl(filename)),form='formatted')
    write(99,500) -1.0*hsa%spglon, hsa%spglat, (hsa%spg_julian -          &
         & hsa%rel_julian)*86400.0, meteo%lon(1), meteo%lat(1),           &
         & (meteo%jdate(1) - meteo%jdate(meteo%nz))*86400.0
    close(99)
500 format(6(f13.5,1x))
    
    !=====================================================================

  end subroutine write_sonde_drift_error
  
  !=======================================================================

  ! SUBROUTINE:

  ! write_sonde_decode_table.f90

  ! DESCRIPTION:

  ! This subroutine writes a column-delimted table containing the
  ! TEMP-DROP encoded observation file and the corresponding decoded
  ! HSA-formatted observation file.

  ! INPUT VARIABLES:

  ! * table_filename; a FORTRAN character string specifying the
  !   filename path for the column-delimited table; if this file does
  !   not exist upon call to this routine, it will be created; if the
  !   file does exist upon call to this routine, it will be appended.

  ! * sonde_filename; a FORTRAN character string specifying the
  !   filename path for the TEMP-DROP encoded observation file.

  ! * hsa; a FORTRAN hsa_struct variable containing (at minimum) the
  !   filename path for the decoded HSA-formatted observation file
  !   (within the attribute 'filename').
  
  !-----------------------------------------------------------------------

  subroutine write_sonde_decode_table(table_filename,sonde_filename,hsa)

    ! Define variables passed to routine

    type(hsa_struct)                                                    :: hsa
    character(len=500)                                                  :: sonde_filename
    character(len=500)                                                  :: table_filename

    ! Define variables computed within routine

    logical                                                             :: exist
    
    !=====================================================================

    ! Define local variables

    inquire(file = trim(adjustl(table_filename)),exist = exist)

    ! Check local variable and proceed accordingly

    if(exist) then

       ! Define local variables

       open(99,file=trim(adjustl(table_filename)),form='formatted',       &
            & status='old',position='append',action='write')
       
    else   ! if(exist)

       ! Define local variables

       open(99,file=trim(adjustl(table_filename)),form='formatted',       &
            & status='new',action='write')
       
    end if ! if(exist)

    ! Define local variables

    write(99,500) trim(adjustl(sonde_filename)),                          &
         & trim(adjustl(hsa%filename))
    if(debug) write(6,501) trim(adjustl(sonde_filename)),                 &
         & trim(adjustl(hsa%filename))
    close(99)
500 format(a,1x,a)
501 format('WRITE_SONDE_DECODE_TABLE: Mapping observation file ',a,' to ' &
         & 'decoded observation file ',a,'.')

    !=====================================================================
  
  end subroutine write_sonde_decode_table

  !=======================================================================

  ! SUBROUTINE:

  ! write_sonde_meteo.f90

  ! DESCRIPTION:

  ! This subroutine writes the contents of the FORTRAN meteo_struct
  ! variable to an external Network Common Data Format (netCDF) file.

  ! INPUT VARIABLES:

  ! * sonde; a FORTRAN sonde_struct variable.
  
  ! * meteo; a FORTRAN meteo_struct variable.

  ! * filename; a FORTRAN character string containing the full-path to
  !   the netCDF file to be created.

  !-----------------------------------------------------------------------

  subroutine write_sonde_meteo(sonde,meteo,filename)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo
    type(sonde_struct)                                                  :: sonde
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    character(len=100)                                                  :: attrname
    character(len=100)                                                  :: varname
    real(r_double),             dimension(:,:),             allocatable :: work
    integer                                                             :: n1dv
    integer                                                             :: n0dv

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call netcdf_interface_open(filename,.false.,.false.,.true.)
    call fileio_interface_varinfo(meteo,varinfo)
    attrname = 'aircraft_id'
    call netcdf_interface_putattr(attrname,meteo%acid)
    attrname = 'obs_number'
    call netcdf_interface_putattr(attrname,meteo%obnum)
    attrname = 'tempdrop_file'
    call netcdf_interface_putattr(attrname,                                &
         & trim(adjustl(meteo%tempdrop_name)))
    attrname = '_FillValue'
    call netcdf_interface_putattr(attrname,dble(spval))
    call netcdf_interface_writedef(varinfo)
    n1dv = 13
    n0dv = 1

    ! Allocate memory for local variables

    if(.not. allocated(work)) allocate(work(n1dv,meteo%nz))

    ! Define local variables

    work(1,:)  = meteo%dwpt
    work(2,:)  = meteo%lat
    work(3,:)  = meteo%lon
    work(4,:)  = meteo%p
    work(5,:)  = meteo%q
    work(6,:)  = meteo%rh
    work(7,:)  = meteo%t
    work(8,:)  = meteo%u
    work(9,:)  = meteo%v
    work(10,:) = meteo%wdir
    work(11,:) = meteo%wspd
    work(12,:) = meteo%z
    work(13,:) = meteo%jdate

    ! Loop through local variable

    do i = 1, n1dv

       ! Define local variables

       varname = varinfo%varname(i)
       call netcdf_interface_putvar(filename,varname,work(i,:))
       
    end do ! do i = 1, n1dv

    ! Deallocate memory for local variables
    
    if(allocated(work)) deallocate(work)

    ! Allocate memory for local variables

    if(.not. allocated(work)) allocate(work(n0dv,1))  
    
    ! Define local variables

    work(1,1) = meteo%psfc

    ! Loop through local variable

    do i = 1, n0dv

       ! Define local variables

       varname = varinfo%varname(n1dv+i)
       call netcdf_interface_putvar(filename,varname,work(i,1))
       
    end do ! do i = 1, n0dv

    ! Deallocate memory for local variables
    
    if(allocated(work)) deallocate(work)
    call variable_interface_cleanup_struct(varinfo)

    ! Define local variables

    call netcdf_interface_close()

    !=====================================================================

  end subroutine write_sonde_meteo
  
  !=======================================================================

end module fileio_interface
