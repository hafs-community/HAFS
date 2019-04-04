module fileio_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! obs-to-bufr :: fileio_interface
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

  use bufrio_interface
  use json_interface
  use kinds_interface
  use namelist_interface
  use netcdf
  use netcdf_interface
  use variable_interface

  ! Define interfaces and attributes for module routines

  implicit none
  private
  public :: fileio_interface_read
  public :: fileio_interface_write
  public :: nfiles
  public :: obs_filename
  interface fileio_interface_read
     module procedure read_bufr_info
     module procedure read_convobserr
     module procedure read_fcst_model
     module procedure read_givtdruv
     module procedure read_givtdruv_obs_filenames
     module procedure read_hsa
     module procedure read_nhcgtcm
     module procedure read_obs_filenames
     module procedure read_obs_flag_json
     module procedure read_tcm
     module procedure read_topogrid
  end interface fileio_interface_read
  interface fileio_interface_varinfo
  end interface fileio_interface_varinfo
  interface fileio_interface_write
  end interface fileio_interface_write

  ! Define local variables

  character(len=500),           dimension(:),               allocatable :: obs_filename
  integer                                                               :: nfiles

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

  ! read_convobserr.f90

  ! DESCRIPTION:

  ! This subroutine reads the user specified external file containing
  ! the observation-type, to be written to the BUFR file, observation
  ! error profiles (see the readme files accompanying this
  ! repository).
  
  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the external file containing the observation-type observation
  !   error profiles.

  ! * grid; a FORTRAN error_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN error_struct variable containing the
  !   observation-type observation error profiles.

  !-----------------------------------------------------------------------

  subroutine read_convobserr(filename,grid)

    ! Define variables passed to routine

    type(error_struct)                                                  :: grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=500)                                                  :: dummy

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    grid%nz = 0
    open(99,file=trim(adjustl(filename)),form='formatted')
1000 read(99,*,end=1001) dummy
    grid%nz = grid%nz + 1
    goto 1000
1001 continue
    close(99)
    call variable_interface_setup_struct(grid)
    open(99,file=trim(adjustl(filename)),form='formatted')

    ! Loop through local variable

    do i = 1, grid%nz

       ! Define local variables

       read(99,*) grid%plev(i), grid%p(i), grid%t(i), grid%q(i),           &
            & grid%z(i), grid%uv(i)

    end do ! do i = 1, grid%nz

    ! Define local variables

    close(99)

    !=====================================================================

  end subroutine read_convobserr

  !=======================================================================

  ! SUBROUTINE:

  ! read_fcst_model.f90

  ! DESCRIPTION:

  ! This subroutine ingests a netcdf file containing forecast model
  ! synthetic observations.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file containing the forecast model synthetic
  !   observations.

  ! * grid; a FORTRAN fcst_model_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN fcst_model_struct variable containing the
  !   forecast model file synthetic observation contents.

  !-----------------------------------------------------------------------

  subroutine read_fcst_model(filename,grid)

    ! Define variables passed to routine

    type(fcst_model_struct)                                             :: grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: dimname
    character(len=100)                                                  :: varname

    !=====================================================================

    ! Define local variables

    dimname = 'nobs'
    call netcdf_interface_getdim(filename,dimname,grid%nobs)
    dimname = 'nz'
    call netcdf_interface_getdim(filename,dimname,grid%nz)
    call variable_interface_setup_struct(grid)
    varname = 'p'
    call netcdf_interface_getvar(filename,varname,grid%p)
    varname = 'q'
    call netcdf_interface_getvar(filename,varname,grid%q)
    varname = 't'
    call netcdf_interface_getvar(filename,varname,grid%t)
    varname = 'u'
    call netcdf_interface_getvar(filename,varname,grid%u)
    varname = 'v'
    call netcdf_interface_getvar(filename,varname,grid%v)
    varname = 'z'
    call netcdf_interface_getvar(filename,varname,grid%z)
    varname = 'lat'
    call netcdf_interface_getvar(filename,varname,grid%lat)
    varname = 'lon'
    call netcdf_interface_getvar(filename,varname,grid%lon)
    varname = 'lmsk'
    call netcdf_interface_getvar(filename,varname,grid%lmsk)
    varname  = 'psfc'
    call netcdf_interface_getvar(filename,varname,grid%psfc)

    !=====================================================================

  end subroutine read_fcst_model

  !=======================================================================

  ! SUBROUTINE:

  ! read_givtdruv.f90

  ! DESCRIPTION:

  ! This subroutine ingests a netcdf file containing National Oceanic
  ! and Atmospheric Administration (NOAA) Atlantic Oceanographic and
  ! Meteorological Laboratory (AOML) Hurricane Research Division (HRD)
  ! GIV Tail-Doppler Radar (TDR) u- and v-wind observations.

  ! REFERENCE:

  ! ftp://ftp.aoml.noaa.gov/pub/hrd/gamache/realtime_netcdf/

  ! NOTE:

  ! The files contained within the URL referenced above are appended
  ! with the global attribute TIMESTAMP which denotes the time for
  ! which the observations contained within the respective file are
  ! valid; the addendum is performed via the HAFS workflow management
  ! system scripts.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file containing the NOAA/AOML/HRD GIV-TDR u- and
  !   v-wind observations.

  ! * grid; a FORTRAN givtdruv_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN givtdruv_struct variable containing the
  !   NOAA/AOML/HRD GIV-TDR u- and v-wind observations.

  !-----------------------------------------------------------------------

  subroutine read_givtdruv(filename,grid)

    ! Define variables passed to routine

    type(givtdruv_struct)                                               :: grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: attrname
    character(len=100)                                                  :: dimname
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:,:,:),         allocatable :: u
    real(r_kind),               dimension(:,:,:,:),         allocatable :: v
    real(r_kind),               dimension(:),               allocatable :: lats
    real(r_kind),               dimension(:),               allocatable :: level
    real(r_kind),               dimension(:),               allocatable :: lons
    integer                                                             :: ncoord

    ! Define counting variables

    integer                                                             :: i, j, k

    !=====================================================================

    ! Define local variables

    dimname = 'lons'
    call netcdf_interface_getdim(filename,dimname,grid%nx)
    dimname = 'lats'
    call netcdf_interface_getdim(filename,dimname,grid%ny)
    dimname = 'level'
    call netcdf_interface_getdim(filename,dimname,grid%nz)
    call variable_interface_setup_struct(grid)

    ! Allocate memory for local variables

    if(.not. allocated(u))     allocate(u(1,grid%nz,grid%ny,grid%nx))
    if(.not. allocated(v))     allocate(v(1,grid%nz,grid%ny,grid%nx))
    if(.not. allocated(lons))  allocate(lons(grid%nx))
    if(.not. allocated(lats))  allocate(lats(grid%ny))
    if(.not. allocated(level)) allocate(level(grid%nz))

    ! Define local variables

    varname  = 'U'
    call netcdf_interface_getvar(filename,varname,u)
    attrname = 'missing_value'
    call netcdf_interface_getattr(filename,attrname,grid%msngvl,varname=  &
         & varname)
    varname  = 'V'
    call netcdf_interface_getvar(filename,varname,v)
    varname  = 'lons'
    call netcdf_interface_getvar(filename,varname,lons)
    varname  = 'lats'
    call netcdf_interface_getvar(filename,varname,lats)
    varname  = 'level'
    call netcdf_interface_getvar(filename,varname,level)
    ncoord   = 0

    ! Loop through local variable

    do k = 1, grid%nz

       ! Loop through local variable

       do j = 1, grid%ny

          ! Loop through local variable

          do i = 1, grid%nx

             ! Define local variables

             ncoord             = ncoord + 1
             grid%u(ncoord)     = u(1,k,j,i)
             grid%v(ncoord)     = v(1,k,j,i)
             grid%lon(ncoord)   = lons(i)
             grid%lat(ncoord)   = lats(j)
             grid%level(ncoord) = level(k)

             ! Check local variable and proceed accordingly

             if(level(k) .le. 0.0) then

                ! Define local variables

                grid%u(ncoord) = grid%msngvl
                grid%v(ncoord) = grid%msngvl

             end if ! if(level(k) .le. 0.0)

          end do ! do i = 1, grid%nx

       end do ! do j = 1, grid%ny
 
    end do ! do k = 1, grid%nz

    ! Deallocate memory for local variables

    if(allocated(lons))  deallocate(lons)
    if(allocated(lats))  deallocate(lats)
    if(allocated(level)) deallocate(level)

    !=====================================================================

  end subroutine read_givtdruv

  !=======================================================================

  ! SUBROUTINE:

  ! read_givtdruv_obs_filenames.f90

  ! DESCRIPTION:

  ! This subroutine reads an external file list containing G-IV
  ! tail-Doppler radar (TDR) u- and v-wind observation files; the
  ! respective file list also contains the timestamp string denoting
  ! when the observations within the respective file are valid; the
  ! format of the file is follows:

  ! <valid date; formatted (assuming UNIX convention) as
  ! %Y-%m-%d_%H:%M:%S> <path to observation file>

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string containing the list of
  !   observation filename pathes.

  ! * grid; a FORTRAN array of FORTRAN givtdruv_struct variables.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN array of FORTRAN givtdruv_struct variables where
  !   the timestamp attribute (timestamp) has been assigned in
  !   accordance with the valid observation time specified within the
  !   file.

  !-----------------------------------------------------------------------

  subroutine read_givtdruv_obs_filenames(filename,grid)

    ! Define variables passed to routine

    type(givtdruv_struct),      dimension(:),               allocatable :: grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=500)                                                  :: dummy
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    nfiles = 0
    open(99,file=trim(adjustl(filename)),form='formatted')
1000 read(99,*,end=1001) dummy
    nfiles = nfiles + 1
    goto 1000
1001 continue
    close(99)

    ! Allocate memory for local variables

    if(.not. allocated(grid))         allocate(grid(nfiles))
    if(.not. allocated(obs_filename)) allocate(obs_filename(nfiles))

    ! Define local variables

    open(99,file=trim(adjustl(filename)),form='formatted')

    ! Loop through local variable

    do i = 1, nfiles

       ! Define local variables

       read(99,500) grid(i)%timestamp, obs_filename(i)

    end do ! do i = 1, nfiles

    ! Define local variables

    close(99)
500 format(a19,a500)

    !=====================================================================

  end subroutine read_givtdruv_obs_filenames

  !=======================================================================

  ! SUBROUTINE:

  ! read_hsa.f90

  ! DESCRIPTION:

  ! This subroutine reads a National Oceanic and Atmospheric
  ! Administration (NOAA) Atlantic Oceanographic and Meteorological
  ! Laboratory (AOML) Hurricane Research Division (HRD) HRD Spline
  ! Analysis (HSA) formatted observation files.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the HSA formatted file.

  ! * grid; a FORTRAN hsa_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN hsa_struct variable containing the HSA file
  !   contents.

  !-----------------------------------------------------------------------

  subroutine read_hsa(filename,grid)

    ! Define variables passed to routine

    type(hsa_struct)                                                    :: grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    real(r_kind)                                                        :: dummy

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    grid%nz = 0
    open(99,file=trim(adjustl(filename)),form='formatted')
1000 read(99,*,end=1001) dummy
    grid%nz = grid%nz + 1
    goto 1000
1001 continue
    close(99)
    call variable_interface_setup_struct(grid)
    open(99,file=trim(adjustl(filename)),form='formatted')

    ! Loop through local variable

    do i = 1, grid%nz

       ! Define local variables

       read(99,500,err=1002) grid%wx(i), grid%yymmdd(i), grid%gmt(i),     &
            & grid%lat(i), grid%lon(i), grid%p(i), grid%t(i), grid%rh(i), &
            & grid%z(i), grid%u(i), grid%v(i), grid%tail(i)

    end do ! do i = 1, grid%nz

    ! Define local variables

    close(99)
    return
1002 continue

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(grid)

    ! Define local variables

    call variable_interface_setup_struct(grid)
500 format(i2,1x,f7.0,1x,i4,1x,2(f7.3,1x),3(f6.1,1x),f7.1,2(f6.1,1x),a4)

    !=====================================================================

  end subroutine read_hsa

  !=======================================================================

  ! SUBROUTINE:

  ! read_nhcgtcm.f90

  ! DESCRIPTION:

  ! This subroutine ingests a netcdf file, typically created by the
  ! routine tc_synthetic_obs.exe, and defines the FORTRAN
  ! nhcgtcm_struct variable attributes accordingly.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the NHC GTCM netcdf file.

  ! * grid; a FORTRAN nhcgtcm_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN nhcgtcm_struct variable containing the NHC GTCM
  !   netcdf file contents.

  !-----------------------------------------------------------------------

  subroutine read_nhcgtcm(filename,grid)

    ! Define variables passed to routine

    type(nhcgtcm_struct)                                                :: grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: dimname
    character(len=100)                                                  :: varname
    character(len=100)                                                  :: attrname   

    !=====================================================================

    ! Define local variables

    dimname  = 'nvals'
    call netcdf_interface_getdim(filename,dimname,grid%nvals)
    call variable_interface_setup_struct(grid)
    varname  = 'lon'
    call netcdf_interface_getvar(filename,varname,grid%lon)
    varname  = 'lat'
    call netcdf_interface_getvar(filename,varname,grid%lat)
    varname  = 'u10m'
    call netcdf_interface_getvar(filename,varname,grid%u10m)
    varname  = 'v10m'
    call netcdf_interface_getvar(filename,varname,grid%v10m)
    attrname = 'analysis_date'
    call netcdf_interface_getattr(filename,attrname,grid%analysis_date)
    attrname = 'missing_data'
    call netcdf_interface_getattr(filename,attrname,grid%spval)

    !=====================================================================

  end subroutine read_nhcgtcm

  !=======================================================================

  ! SUBROUTINE:

  ! read_obs_filenames.f90

  ! DESCRIPTION:

  ! This subroutine defines the global variables 'nfiles' and
  ! 'obs_filename' array.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string containing the list of
  !   observation filename pathes.

  !-----------------------------------------------------------------------

  subroutine read_obs_filenames(filename)

    ! Define variables passed to routine

    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=500)                                                  :: dummy
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    nfiles = 0
    open(99,file=trim(adjustl(filename)),form='formatted')
1000 read(99,*,end=1001) dummy
    nfiles = nfiles + 1
    goto 1000
1001 continue
    close(99)

    ! Allocate memory for local variables

    if(.not. allocated(obs_filename)) allocate(obs_filename(nfiles))

    ! Define local variables

    open(99,file=trim(adjustl(filename)),form='formatted')

    ! Loop through local variable

    do i = 1, nfiles

       ! Define local variables

       read(99,*) obs_filename(i)

    end do ! do i = 1, nfiles

    ! Define local variables

    close(99)

    !=====================================================================

  end subroutine read_obs_filenames

  !=======================================================================

  ! SUBROUTINE:

  ! read_obs_flag_json.f90

  ! DESCRIPTION:

  ! This subroutine ingests a JSON formatted file containing the user
  ! specified observation flagging attributes and returns the
  ! respective attributes within the FORTRAN obs_flag variable.

  ! INPUT VARIABLES:

  ! * obs_flag; a FORTRAN obs_flag_struct variable containing the
  !   observation JSON vtable file path.

  ! OUTPUT VARIABLES:

  ! * obs_flag; a FORTRAN obs_flag_struct variable containing the
  !   observation JSON vtable attributes.

  !-----------------------------------------------------------------------

  subroutine read_obs_flag_json(obs_flag)

    ! Define variables passed to routine

    type(obs_flag_struct)                                               :: obs_flag

    !=====================================================================

    ! Define local variables

    call json_interface_read(obs_flag)

    !=====================================================================

  end subroutine read_obs_flag_json

  !=======================================================================

  ! SUBROUTINE:

  ! read_tcm.f90

  ! DESCRIPTION:

  ! This subroutine ingests a netcdf file, typically created by the
  ! routine tc_synthetic_obs.exe, and defines the FORTRAN tcm_struct
  ! variable attributes accordingly.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the TCM netcdf file.

  ! * grid; a FORTRAN tcm_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN tcm_struct variable containing the TCM netcdf
  !   file contents.

  !-----------------------------------------------------------------------

  subroutine read_tcm(filename,grid)

    ! Define variables passed to routine

    type(tcm_struct)                                                    :: grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: dimname
    character(len=100)                                                  :: varname
    character(len=100)                                                  :: attrname        
    real(r_kind),               dimension(:,:),             allocatable :: var

    !=====================================================================

    ! Define local variables

    dimname = 'nx'
    call netcdf_interface_getdim(filename,dimname,grid%nx)
    dimname = 'ny'
    call netcdf_interface_getdim(filename,dimname,grid%ny)
    call variable_interface_setup_struct(grid)

    ! Allocate memory for local variables

    if(.not. allocated(var)) allocate(var(grid%nx,grid%ny))

    ! Define local variables

    varname   = 'longitude'
    call netcdf_interface_getvar(filename,varname,var)
    grid%lon  = reshape(var,shape(grid%lon))
    varname   = 'latitude'
    call netcdf_interface_getvar(filename,varname,var)
    grid%lat  = reshape(var,shape(grid%lat))
    varname   = 'sfcprs'
    call netcdf_interface_getvar(filename,varname,var)
    grid%psfc = reshape(var,shape(grid%psfc))
    varname   = 'u'
    call netcdf_interface_getvar(filename,varname,var)
    grid%u    = reshape(var,shape(grid%u))
    varname   = 'v'
    call netcdf_interface_getvar(filename,varname,var)
    grid%v    = reshape(var,shape(grid%v))

    ! Deallocate memory for local variables

    if(allocated(var)) deallocate(var)

    ! Define local variables

    attrname = 'analysis_date'
    call netcdf_interface_getattr(filename,attrname,grid%analysis_date)
    attrname = 'missing_data'
    call netcdf_interface_getattr(filename,attrname,grid%spval)
    call netcdf_interface_close()

    !=====================================================================

  end subroutine read_tcm

  !=======================================================================

  ! SUBROUTINE:

  ! read_temis.f90

  ! DESCRIPTION:

  ! This subroutine reads a netcdf file containing TEMIS topography
  ! data within the user specified file.

  ! INPUT VARIABLES:

  ! * topogrid; a FORTRAN topogrid_struct variable.

  ! OUTPUT VARIABLES:

  ! * topogrid; a FORTRAN topogrid_struct variable containing the
  !   TEMIS topography variable values from the user specified file.w

  !-----------------------------------------------------------------------

  subroutine read_temis(topogrid)

    ! Define variables passed to routine

    type(topogrid_struct)                                               :: topogrid

    ! Define variables computed within routine

    character(len=500)                                                  :: filename
    character(len=100)                                                  :: dimname
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:),             allocatable :: elevation
    real(r_kind),               dimension(:),               allocatable :: latitude
    real(r_kind),               dimension(:),               allocatable :: longitude
    integer                                                             :: count
    
    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    filename = topo_filepath
    dimname  = 'nlon'
    call netcdf_interface_getdim(filename,dimname,topogrid%nx)
    dimname  = 'nlat'
    call netcdf_interface_getdim(filename,dimname,topogrid%ny)
    call variable_interface_setup_struct(topogrid)

    ! Allocate memory for local variables

    if(.not. allocated(elevation))                                        &
         & allocate(elevation(topogrid%nx,topogrid%ny))
    if(.not. allocated(latitude))                                         &
         & allocate(latitude(topogrid%ny))
    if(.not. allocated(longitude))                                        &
         & allocate(longitude(topogrid%nx))

    ! Define local variables

    varname = 'elevation'
    call netcdf_interface_getvar(filename,varname,elevation)
    varname = 'latitude'
    call netcdf_interface_getvar(filename,varname,latitude)
    varname = 'longitude'
    call netcdf_interface_getvar(filename,varname,longitude)
    count   = 0

    ! Loop through local variable

    do j = 1, topogrid%ny

       ! Loop through local variable

       do i = 1, topogrid%nx

          ! Define local variables

          count                = count + 1
          topogrid%topo(count) = elevation(i,j)
          topogrid%lat(count)  = latitude(j)
          topogrid%lon(count)  = longitude(i)

       end do ! do i = 1, topogrid%nx

    end do ! do j = 1, topogrid%ny

    ! Deallocate memory for local variables

    if(allocated(elevation)) deallocate(elevation)
    if(allocated(latitude))  deallocate(latitude)
    if(allocated(longitude)) deallocate(longitude)

    ! Define local variables

    call netcdf_interface_close()

    !=====================================================================

  end subroutine read_temis

  !=======================================================================

  ! SUBROUTINE:

  ! read_topogrid.f90

  ! DESCRIPTION:

  ! This subroutine ingests a user specified file containing
  ! topography information and defines and returns the FORTRAN
  ! topogrid_struct variable.

  ! INPUT VARIABLES:

  ! * topogrid; a FORTRAN topogrid_struct variable.

  ! OUTPUT VARIABLES:

  ! * topogrid; a FORTRAN topogrid_struct variable containing the user
  !   specified file topography information.

  !-----------------------------------------------------------------------

  subroutine read_topogrid(topogrid)

    ! Define variables passed to routine

    type(topogrid_struct)                                               :: topogrid

    !=====================================================================

    ! Define local variables

    if(is_temis) call read_temis(topogrid)

    !=====================================================================

  end subroutine read_topogrid
  
  !=======================================================================

end module fileio_interface
