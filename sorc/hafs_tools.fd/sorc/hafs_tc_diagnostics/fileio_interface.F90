module fileio_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: fileio_interface
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

  use constants_interface
  use grib_api_interface
  use grid_methods_interface
  use json_interface
  use meteo_methods_interface
  use mpi_interface
  use namelist_interface
  use netcdf_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: fileio_interface_read
  public :: fileio_interface_write
  interface fileio_interface_read
     module procedure read_grib
     module procedure read_json_grib
     module procedure read_meteo
     module procedure read_nceptrckrinfo
  end interface fileio_interface_read
  interface fileio_interface_varinfo
     module procedure varinfo_filter
     module procedure varinfo_meteo
     module procedure varinfo_tccps
     module procedure varinfo_tcenv
     module procedure varinfo_tcmpi
     module procedure varinfo_tcmsi
  end interface fileio_interface_varinfo
  interface fileio_interface_write
     module procedure write_filter
     module procedure write_meteo
     module procedure write_tccps
     module procedure write_tcenv
     module procedure write_tcmpi
     module procedure write_tcmsi
  end interface fileio_interface_write
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! compute_meteo.f90

  ! DESCRIPTION:

  ! This subroutine computes all meteorological variable values
  ! required by external routines and methods.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the computed
  !   variable values required by external routines and methods.

  !-----------------------------------------------------------------------

  subroutine compute_meteo(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    !=====================================================================

    ! Define local variables

    mpi_taskgrid%ncoords = meteo%ncoords
    mpi_taskgrid%nlevs   = meteo%nz
    call variable_interface_setup_struct(mpi_taskgrid)
    call mpi_interface_partition(debug)

    ! Compute local variables

    call meteo_methods_diagnostics(meteo)

    ! Deallocate memory for local variables
    
    call variable_interface_cleanup_struct(mpi_taskgrid)

    !=====================================================================

  end subroutine compute_meteo

  !=======================================================================

  ! SUBROUTINE:

  ! read_grib.f90

  ! DESCRIPTION:

  ! This subroutine parses a WMO GRIB-1 formatted file and returns a
  ! FORTRAN structure (grib) containing the variable attributes (e.g.,
  ! geographical position and values) for the GRIB variable specified
  ! by the user GRIB API keys.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the path to the
  !   WMO GRIB-1 formatted file.

  ! * grib; a FORTRAN grib_struct variable containing (at minimum) the
  !   respective variable GRIB API keys.

  ! OUTPUT VARIABLES:

  ! * grib; a FORTRAN grib_struct variable containing the respective
  !   variable attributes (e.g., geographical locations and values)
  !   for the user specified variable (via the GRIB API keys) 

  !-----------------------------------------------------------------------

  subroutine read_grib(filename,grib)

    ! Define variables passed to routine

    type(grib_struct)                                                   :: grib
    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables

    call grib_api_interface_read(filename,grib)

    !=====================================================================

  end subroutine read_grib

  !=======================================================================

  ! SUBROUTINE:

  ! read_json_grib.f90

  ! DESCRIPTION:

  ! This subroutine parses a JSON formatted file and returns a FORTRAN
  ! json_grib_struct variable (json) containing all records within the
  ! user specified file.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the path to the
  !   JSON formatted file.

  ! * json; a FORTRAN json_grib_struct variable.

  ! OUTPUT VARIABLES:

  ! * json; a FORTRAN json_grib_struct variable containing the
  !   contents of the user specified JSON formatted file.

  !-----------------------------------------------------------------------

  subroutine read_json_grib(filename,json)

    ! Define variables passed to routine

    type(json_grib_struct),     dimension(:),               allocatable :: json
    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables

    call json_interface_read(filename,json)

    !=====================================================================

  end subroutine read_json_grib

  !=======================================================================

  ! SUBROUTINE:

  ! read_meteo.f90

  ! DESCRIPTION:

  ! This subroutine reads a GRIB (1) formatted file and populates the
  ! following variable arrays:

  ! * grid projection attributes
  ! * land/sea mask
  ! * topography/orography
  ! * surface pressure 
  ! * sea-level pressure
  ! * surface temperature
  ! * 10-meter wind components
  ! * pressure profile
  ! * geopotential height profile
  ! * specific humidity profile
  ! * temperature profile
  ! * wind component profiles

  ! This subroutine than derives the remainder of the meteorological
  ! variable values required by external methods and routines.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the path to the
  !   GRIB formatted file to be parsed.

  ! * meteo; a FORTRAN meteo_struct variable.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the variable
  !   values parsed from the user specified GRIB-formatted file as
  !   well as the derived meteorological variable values.

  !-----------------------------------------------------------------------

  subroutine read_meteo(filename,meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(json_grib_struct),     dimension(:),               allocatable :: json
    type(grib_struct),          dimension(:),               allocatable :: grib
    type(grid_struct)                                                   :: grid

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    call fileio_interface_read(grib_json_vtable,json)

    ! Allocate memory for local variables

    if(.not. allocated(grib)) allocate(grib(size(json)))

    ! Check local variable and proceed accordingly

    if(mpi_procid .eq. mpi_masternode) then

       ! Loop through local variable
       
       do i = 1, size(json)

          ! Define local variables

          grib(i)%shortName              = json(i)%shortName
          grib(i)%indicatorOfTypeOfLevel = json(i)%indicatorOfTypeOfLevel
          grib(i)%indicatorOfParameter   = json(i)%indicatorOfParameter
          grib(i)%levType                = json(i)%levType
          call fileio_interface_read(grib_filename,grib(i))
       
          ! Check local variable and proceed accordingly

          if(trim(adjustl(json(i)%gribVar)) .eq. 'pslp') then

             ! Define local variables

             meteo%nx = grib(i)%nx
             meteo%ny = grib(i)%ny

          end if ! if(trim(adjustl(json(i)%gribVar)) .eq. 'pslp')

          ! Check local variable and proceed accordingly

          if(trim(adjustl(json(i)%gribVar)) .eq. 'temp') then

             ! Define local variables

             meteo%nz = grib(i)%nz

          end if ! if(trim(adjustl(json(i)%gribVar)) .eq. 'temp')

       end do ! do i = 1, size(json)

    end if ! if(mpi_procid .eq. mpi_masternode)

    ! Define local variables

    call mpi_interface_waitall()
    call mpi_bcast(meteo%nx,1,mpi_integer,mpi_masternode,mpi_comm_world,   &
         & mpi_ierror)
    call mpi_bcast(meteo%ny,1,mpi_integer,mpi_masternode,mpi_comm_world,   &
         & mpi_ierror)
    call mpi_bcast(meteo%nz,1,mpi_integer,mpi_masternode,mpi_comm_world,   &
         & mpi_ierror)
    call variable_interface_setup_struct(meteo)

    ! Check local variable and proceed accordingly

    if(mpi_procid .eq. mpi_masternode) then

       ! Define local variables

       grid%nx = meteo%nx
       grid%ny = meteo%ny
       call variable_interface_setup_struct(grid)

       ! Loop through local variable

       do i = 1, size(json)

          ! Check local variable and proceed accordingly

          if(trim(adjustl(json(i)%gribVar)) .eq. 'pslp') then
          
             ! Define local variables

             grid%lat   = grib(i)%lat(:,1)
             grid%lon   = grib(i)%lon(:,1)
             meteo%xlat = grid%lat
             meteo%xlon = grid%lon

             ! Compute local variables

             call grid_methods_mapfactor(grid,grib(i)%gridtype)
             call grid_methods_geodist(grid)

             ! Define local variables

             meteo%mapfac = grid%mapfac
             meteo%dx     = grid%dx*grib(i)%dlon
             meteo%dy     = grid%dy*grib(i)%dlat

          end if ! if(trim(adjustl(json(i)%gribVar)) .eq. 'pslp')

          ! Check local variable and proceed accordingly

          if(trim(adjustl(json(i)%gribVar)) .eq. 'temp') then

             ! Loop through local variable

             do j = 1, grib(i)%nz
          
                ! Define local variables

                meteo%p(:,j) = grib(i)%levs(j)*100.0

             end do ! do j = 1, grib(i)%nz

          end if ! if(trim(adjustl(json(i)%gribVar)) .eq. 'temp')
       
       end do ! do i = 1, size(json)
   
       ! Loop through local variable

       do i = 1, size(json)

          ! Check local variable and proceed accordingly
             
          if(json(i)%levType .eq. 2) then

             ! Define local variables

             if(trim(adjustl(json(i)%gribVar)) .eq. 'land')               &
                  & meteo%land = grib(i)%values(:,1)
             if(trim(adjustl(json(i)%gribVar)) .eq. 'orog')               &
                  & meteo%orog = grib(i)%values(:,1)
             if(trim(adjustl(json(i)%gribVar)) .eq. 'psfc')               &
                  & meteo%psfc = grib(i)%values(:,1)
             if(trim(adjustl(json(i)%gribVar)) .eq. 'pslp')               &
                  & meteo%pmsl = grib(i)%values(:,1)
             if(trim(adjustl(json(i)%gribVar)) .eq. 'tsfc')               &
                  & meteo%tsfc = grib(i)%values(:,1)
             if(trim(adjustl(json(i)%gribVar)) .eq. 'u10m')               &
                  & meteo%u10m = grib(i)%values(:,1)
             if(trim(adjustl(json(i)%gribVar)) .eq. 'v10m')               &
                  & meteo%v10m = grib(i)%values(:,1)

          end if ! if(json(i)%levType .eq. 2)

          ! Check local variable and proceed accordingly
             
          if(json(i)%levType .eq. 3) then

             ! Loop through local variable

             do j = 1, grib(i)%nz

                ! Define local variables

                if(trim(adjustl(json(i)%gribVar)) .eq. 'geoh')            &
                     & meteo%z(:,j) = grib(i)%values(:,j)
                if(trim(adjustl(json(i)%gribVar)) .eq. 'shmd')            &
                     & meteo%q(:,j) = grib(i)%values(:,j)
                if(trim(adjustl(json(i)%gribVar)) .eq. 'temp')            &
                     & meteo%t(:,j) = grib(i)%values(:,j)
                if(trim(adjustl(json(i)%gribVar)) .eq. 'uwnd')            &
                     & meteo%u(:,j) = grib(i)%values(:,j)
                if(trim(adjustl(json(i)%gribVar)) .eq. 'vwnd')            &
                     & meteo%v(:,j) = grib(i)%values(:,j)

             end do ! do j = 1, grib(i)%nz

          end if ! if(json(i)%levType .eq. 3)

       end do ! do i = 1, size(json)

       ! Define local variables

       meteo%p(:,1) = meteo%psfc
       meteo%z(:,1) = meteo%orog

    end if ! if(mpi_procid .eq. mpi_masternode)

    ! Define local variables

    call mpi_interface_waitall()
    call mpi_bcast(meteo%p,(meteo%ncoords*meteo%nz),mpi_real,             &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%q,(meteo%ncoords*meteo%nz),mpi_real,             &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%t,(meteo%ncoords*meteo%nz),mpi_real,             &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%u,(meteo%ncoords*meteo%nz),mpi_real,             &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%v,(meteo%ncoords*meteo%nz),mpi_real,             &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%z,(meteo%ncoords*meteo%nz),mpi_real,             &
         & mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%dx,meteo%ncoords,mpi_real,mpi_masternode,        &
         & mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%dy,meteo%ncoords,mpi_real,mpi_masternode,        &
         & mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%land,meteo%ncoords,mpi_real,mpi_masternode,      &
         & mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%orog,meteo%ncoords,mpi_real,mpi_masternode,      &
         & mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%mapfac,meteo%ncoords,mpi_real,mpi_masternode,    &
         & mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%psfc,meteo%ncoords,mpi_real,mpi_masternode,      &
         & mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%pmsl,meteo%ncoords,mpi_real,mpi_masternode,      &
         & mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%tsfc,meteo%ncoords,mpi_real,mpi_masternode,      &
         & mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%u10m,meteo%ncoords,mpi_real,mpi_masternode,      &
         & mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%v10m,meteo%ncoords,mpi_real,mpi_masternode,      &
         & mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%xlat,meteo%ncoords,mpi_real,mpi_masternode,      &
         & mpi_comm_world,mpi_ierror)
    call mpi_bcast(meteo%xlon,meteo%ncoords,mpi_real,mpi_masternode,      &
         & mpi_comm_world,mpi_ierror)

    ! Deallocate memory for local variables

    if(allocated(grib)) deallocate(grib)
    if(allocated(json)) deallocate(json)

    ! Compute local variables

    call compute_meteo(meteo)

    !=====================================================================

  end subroutine read_meteo

  !=======================================================================

  ! SUBROUTINE:

  ! read_nceptrckrinfo.f90

  ! DESCRIPTION:

  ! This subroutine reads information specific to TC events parsed
  ! from the NCEP tracker output.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string containing the file to be
  !   read.

  ! * tcv; a FORTRAN tcv_struct variable.

  ! OUTPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable containing the attributes of
  !   the parsed NCEP tracker records from the ingested file.

  !-----------------------------------------------------------------------

  subroutine read_nceptrckrinfo(filename,tcv)

    ! Define variables passed to routine

    type(tcv_struct),           dimension(:),               allocatable :: tcv
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=100)                                                  :: dummy

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    ntcs = 0
    open(99,file=trim(adjustl(filename)),form='formatted')
100 read(99,*,end=101) dummy
    ntcs = ntcs + 1
    goto 100
101 continue
    close(99)

    ! Allocate memory for local variables

    if(.not. allocated(tcv)) allocate(tcv(ntcs))

    ! Define local variables

    open(99,file=trim(adjustl(filename)),form='formatted')

    ! Loop through local variable

    do i = 1, ntcs

       ! Define local variables

       read(99,*) tcv(i)%tcid, tcv(i)%valid_time, tcv(i)%reflat,           &
            & tcv(i)%reflon
       
       ! Check local variable and proceed accordingly

       if(tcv(i)%reflon .lt. 0.0) tcv(i)%reflon = 360.0 + tcv(i)%reflon

    end do ! do i = 1, ntcs

    ! Define local variables

    close(99)

    !=====================================================================

  end subroutine read_nceptrckrinfo

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_filter.f90

  ! DESCRIPTION:

  ! This subroutine defines the FORTRAN varinfo_struct variable used
  ! to write the appropriate variables contained within the FORTRAN
  ! filter_struct variable to an external Network Common Data Format
  ! (netcdf) file.

  ! INPUT VARIABLES:

  ! * filter; a FORTRAN filter_struct variable.

  ! * varinfo; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_filter(filter,varinfo)

    ! Define variables passed to routine

    type(filter_struct)                                                 :: filter
    type(varinfo_struct)                                                :: varinfo

    ! Define variables computed within routine

    character(len=10),          dimension(:),               allocatable :: dimname
    integer,                    dimension(:),               allocatable :: dimval
    integer,                    dimension(:),               allocatable :: dimid

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    varinfo%ndims  = 3
    varinfo%nvars  = 9
    varinfo%nattrs = 2
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Define local variables

    dimname(1) = 'lons'
    dimname(2) = 'lats'
    dimname(3) = 'nz'
    dimval(1)  = filter%nx
    dimval(2)  = filter%ny
    dimval(3)  = filter%nz

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    ! Define local variables

    varinfo%varattrs(1,1,1) = 'title'
    varinfo%varattrs(1,1,2) = 'Centroid latitude.'
    varinfo%varattrs(1,2,1) = 'units'
    varinfo%varattrs(1,2,2) = 'degrees'
    varinfo%varname(1)      = 'clat'
    varinfo%varndims(1)     = 1
    varinfo%varnattrs(1)    = 2
    varinfo%vartype(1)      = 'float'
    varinfo%vardimid(1,1)   = dimid(3)
    varinfo%varattrs(2,1,1) = 'title'
    varinfo%varattrs(2,1,2) = 'Centroid longitude.'
    varinfo%varattrs(2,2,1) = 'units'
    varinfo%varattrs(2,2,2) = 'degrees'
    varinfo%varname(2)      = 'clon'
    varinfo%varndims(2)     = 1
    varinfo%varnattrs(2)    = 2
    varinfo%vartype(2)      = 'float'
    varinfo%vardimid(2,1)   = dimid(3)
    varinfo%varattrs(3,1,1) = 'title'
    varinfo%varattrs(3,1,2) = 'Latitude grid.'
    varinfo%varattrs(3,2,1) = 'units'
    varinfo%varattrs(3,2,2) = 'degrees'
    varinfo%varname(3)      = 'lats'
    varinfo%varndims(3)     = 2
    varinfo%varnattrs(3)    = 2
    varinfo%vartype(3)      = 'float'
    varinfo%vardimid(3,1)   = dimid(1)
    varinfo%vardimid(3,2)   = dimid(2)
    varinfo%varattrs(4,1,1) = 'title'
    varinfo%varattrs(4,1,2) = 'Longitude grid.'
    varinfo%varattrs(4,2,1) = 'units'
    varinfo%varattrs(4,2,2) = 'degrees'
    varinfo%varname(4)      = 'lons'
    varinfo%varndims(4)     = 2
    varinfo%varnattrs(4)    = 2
    varinfo%vartype(4)      = 'float'
    varinfo%vardimid(4,1)   = dimid(1)
    varinfo%vardimid(4,2)   = dimid(2)
    varinfo%varattrs(5,1,1) = 'title'
    varinfo%varattrs(5,1,2) = 'Azimuth relative to centroid.'
    varinfo%varattrs(5,2,1) = 'units'
    varinfo%varattrs(5,2,2) = 'degrees'
    varinfo%varname(5)      = 'angle'
    varinfo%varndims(5)     = 3
    varinfo%varnattrs(5)    = 2
    varinfo%vartype(5)      = 'float'
    varinfo%vardimid(5,1)   = dimid(1)
    varinfo%vardimid(5,2)   = dimid(2)
    varinfo%vardimid(5,3)   = dimid(3)
    varinfo%varattrs(6,1,1) = 'title'
    varinfo%varattrs(6,1,2) = 'Momentum variable filter region.'
    varinfo%varattrs(6,2,1) = 'units'
    varinfo%varattrs(6,2,2) = 'unitless'
    varinfo%varname(6)      = 'kfltr'
    varinfo%varndims(6)     = 3
    varinfo%varnattrs(6)    = 2
    varinfo%vartype(6)      = 'float'
    varinfo%vardimid(6,1)   = dimid(1)
    varinfo%vardimid(6,2)   = dimid(2)
    varinfo%vardimid(6,3)   = dimid(3)
    varinfo%varattrs(7,1,1) = 'title'
    varinfo%varattrs(7,1,2) = 'Radial distance relative to centroid.'
    varinfo%varattrs(7,2,1) = 'units'
    varinfo%varattrs(7,2,2) = 'meters'
    varinfo%varname(7)      = 'radius'
    varinfo%varndims(7)     = 3
    varinfo%varnattrs(7)    = 2
    varinfo%vartype(7)      = 'float'
    varinfo%vardimid(7,1)   = dimid(1)
    varinfo%vardimid(7,2)   = dimid(2)
    varinfo%vardimid(7,3)   = dimid(3)
    varinfo%varattrs(8,1,1) = 'title'
    varinfo%varattrs(8,1,2) = 'Tangential wind.'
    varinfo%varattrs(8,2,1) = 'units'
    varinfo%varattrs(8,2,2) = 'meters per second'
    varinfo%varname(8)      = 'tngwnd'
    varinfo%varndims(8)     = 3
    varinfo%varnattrs(8)    = 2
    varinfo%vartype(8)      = 'float'
    varinfo%vardimid(8,1)   = dimid(1)
    varinfo%vardimid(8,2)   = dimid(2)
    varinfo%vardimid(8,3)   = dimid(3)
    varinfo%varattrs(9,1,1) = 'title'
    varinfo%varattrs(9,1,2) = 'Mass variable filter region.'
    varinfo%varattrs(9,2,1) = 'units'
    varinfo%varattrs(9,2,2) = 'unitless'
    varinfo%varname(9)      = 'tfltr'
    varinfo%varndims(9)     = 3
    varinfo%varnattrs(9)    = 2
    varinfo%vartype(9)      = 'float'
    varinfo%vardimid(9,1)   = dimid(1)
    varinfo%vardimid(9,2)   = dimid(2)
    varinfo%vardimid(9,3)   = dimid(3)

    ! Deallocate memory for local variables

    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)

    !=====================================================================

  end subroutine varinfo_filter

    !=======================================================================

  ! SUBROUTINE:

  ! varinfo_meteo.f90

  ! DESCRIPTION:

  ! This subroutine defines the FORTRAN varinfo_struct variable used
  ! to write the appropriate variables contained within the FORTRAN
  ! meteo_struct variable to an external Network Common Data Format
  ! (netcdf) file.

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable.

  ! * varinfo; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_meteo(meteo,varinfo)

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

    varinfo%ndims  = 3
    varinfo%nvars  = 21
    varinfo%nattrs = 2
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Define local variables

    dimname(1) = 'lons'
    dimname(2) = 'lats'
    dimname(3) = 'nz'
    dimval(1)  = meteo%nx
    dimval(2)  = meteo%ny
    dimval(3)  = meteo%nz

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    ! Define local variables

    varinfo%varattrs(1,1,1)  = 'title'
    varinfo%varattrs(1,1,2)  = 'Common latitude grid.'
    varinfo%varattrs(1,2,1)  = 'units'
    varinfo%varattrs(1,2,2)  = 'degrees'
    varinfo%varname(1)       = 'lats'
    varinfo%varndims(1)      = 2
    varinfo%varnattrs(1)     = 2
    varinfo%vartype(1)       = 'float'
    varinfo%vardimid(1,1)    = dimid(1)
    varinfo%vardimid(1,2)    = dimid(2)
    varinfo%varattrs(2,1,1)  = 'title'
    varinfo%varattrs(2,1,2)  = 'Common longitude grid.'
    varinfo%varattrs(2,2,1)  = 'units'
    varinfo%varattrs(2,2,2)  = 'degrees'
    varinfo%varname(2)       = 'lons'
    varinfo%varndims(2)      = 2
    varinfo%varnattrs(2)     = 2
    varinfo%vartype(2)       = 'float'
    varinfo%vardimid(2,1)    = dimid(1)
    varinfo%vardimid(2,2)    = dimid(2)
    varinfo%varattrs(3,1,1)  = 'title'
    varinfo%varattrs(3,1,2)  = 'Orography.'
    varinfo%varattrs(3,2,1)  = 'units'
    varinfo%varattrs(3,2,2)  = 'meters'
    varinfo%varname(3)       = 'orog'
    varinfo%varndims(3)      = 2
    varinfo%varnattrs(3)     = 2
    varinfo%vartype(3)       = 'float'
    varinfo%vardimid(3,1)    = dimid(1)
    varinfo%vardimid(3,2)    = dimid(2)
    varinfo%varattrs(4,1,1)  = 'title'
    varinfo%varattrs(4,1,2)  = 'Sea-level pressure.'
    varinfo%varattrs(4,2,1)  = 'units'
    varinfo%varattrs(4,2,2)  = 'Pascals'
    varinfo%varname(4)       = 'pmsl'
    varinfo%varndims(4)      = 2
    varinfo%varnattrs(4)     = 2
    varinfo%vartype(4)       = 'float'
    varinfo%vardimid(4,1)    = dimid(1)
    varinfo%vardimid(4,2)    = dimid(2)
    varinfo%varattrs(5,1,1)  = 'title'
    varinfo%varattrs(5,1,2)  = 'Velocity potential.'
    varinfo%varattrs(5,2,1)  = 'units'
    varinfo%varattrs(5,2,2)  = 'meters^2 per second'
    varinfo%varname(5)       = 'chi'
    varinfo%varndims(5)      = 3
    varinfo%varnattrs(5)     = 2
    varinfo%vartype(5)       = 'float'
    varinfo%vardimid(5,1)    = dimid(1)
    varinfo%vardimid(5,2)    = dimid(2)
    varinfo%vardimid(5,3)    = dimid(3)
    varinfo%varattrs(6,1,1)  = 'title'
    varinfo%varattrs(6,1,2)  = 'Divergence.'
    varinfo%varattrs(6,2,1)  = 'units'
    varinfo%varattrs(6,2,2)  = 'rotations per second'
    varinfo%varname(6)       = 'divg'
    varinfo%varndims(6)      = 3
    varinfo%varnattrs(6)     = 2
    varinfo%vartype(6)       = 'float'
    varinfo%vardimid(6,1)    = dimid(1)
    varinfo%vardimid(6,2)    = dimid(2)
    varinfo%vardimid(6,3)    = dimid(3)
    varinfo%varattrs(7,1,1)  = 'title'
    varinfo%varattrs(7,1,2)  = 'Streamfunction.'
    varinfo%varattrs(7,2,1)  = 'units'
    varinfo%varattrs(7,2,2)  = 'meters^2 per second'
    varinfo%varname(7)       = 'psi'
    varinfo%varndims(7)      = 3
    varinfo%varnattrs(7)     = 2
    varinfo%vartype(7)       = 'float'
    varinfo%vardimid(7,1)    = dimid(1)
    varinfo%vardimid(7,2)    = dimid(2)
    varinfo%vardimid(7,3)    = dimid(3)
    varinfo%varattrs(8,1,1)  = 'title'
    varinfo%varattrs(8,1,2)  = 'Potential vorticity.'
    varinfo%varattrs(8,2,1)  = 'units'
    varinfo%varattrs(8,2,2)  = 'meters^2 per second, Kelvin per kilogram'
    varinfo%varname(8)       = 'pv'
    varinfo%varndims(8)      = 3
    varinfo%varnattrs(8)     = 2
    varinfo%vartype(8)       = 'float'
    varinfo%vardimid(8,1)    = dimid(1)
    varinfo%vardimid(8,2)    = dimid(2)
    varinfo%vardimid(8,3)    = dimid(3)
    varinfo%varattrs(9,1,1)  = 'title'
    varinfo%varattrs(9,1,2)  = 'Vorticity.'
    varinfo%varattrs(9,2,1)  = 'units'
    varinfo%varattrs(9,2,2)  = 'rotations per second'
    varinfo%varname(9)       = 'vort'
    varinfo%varndims(9)      = 3
    varinfo%varnattrs(9)     = 2
    varinfo%vartype(9)       = 'float'
    varinfo%vardimid(9,1)    = dimid(1)
    varinfo%vardimid(9,2)    = dimid(2)
    varinfo%vardimid(9,3)    = dimid(3)
    varinfo%varattrs(10,1,1) = 'title'
    varinfo%varattrs(10,1,2) = 'Divergent wind zonal component.'
    varinfo%varattrs(10,2,1) = 'units'
    varinfo%varattrs(10,2,2) = 'meters per second'
    varinfo%varname(10)      = 'wdivu'
    varinfo%varndims(10)     = 3
    varinfo%varnattrs(10)    = 2
    varinfo%vartype(10)      = 'float'
    varinfo%vardimid(10,1)   = dimid(1)
    varinfo%vardimid(10,2)   = dimid(2)
    varinfo%vardimid(10,3)   = dimid(3)
    varinfo%varattrs(11,1,1) = 'title'
    varinfo%varattrs(11,1,2) = 'Divergent wind meridional component.'
    varinfo%varattrs(11,2,1) = 'units'
    varinfo%varattrs(11,2,2) = 'meters per second'
    varinfo%varname(11)      = 'wdivv'
    varinfo%varndims(11)     = 3
    varinfo%varnattrs(11)    = 2
    varinfo%vartype(11)      = 'float'
    varinfo%vardimid(11,1)   = dimid(1)
    varinfo%vardimid(11,2)   = dimid(2)
    varinfo%vardimid(11,3)   = dimid(3)
    varinfo%varattrs(12,1,1) = 'title'
    varinfo%varattrs(12,1,2) = 'Harmonic wind zonal component.'
    varinfo%varattrs(12,2,1) = 'units'
    varinfo%varattrs(12,2,2) = 'meters per second'
    varinfo%varname(12)      = 'whrmu'
    varinfo%varndims(12)     = 3
    varinfo%varnattrs(12)    = 2
    varinfo%vartype(12)      = 'float'
    varinfo%vardimid(12,1)   = dimid(1)
    varinfo%vardimid(12,2)   = dimid(2)
    varinfo%vardimid(12,3)   = dimid(3)
    varinfo%varattrs(13,1,1) = 'title'
    varinfo%varattrs(13,1,2) = 'Harmonic wind meridional component.'
    varinfo%varattrs(13,2,1) = 'units'
    varinfo%varattrs(13,2,2) = 'meters per second'
    varinfo%varname(13)      = 'whrmv'
    varinfo%varndims(13)     = 3
    varinfo%varnattrs(13)    = 2
    varinfo%vartype(13)      = 'float'
    varinfo%vardimid(13,1)   = dimid(1)
    varinfo%vardimid(13,2)   = dimid(2)
    varinfo%vardimid(13,3)   = dimid(3)
    varinfo%varattrs(14,1,1) = 'title'
    varinfo%varattrs(14,1,2) = 'Rotational wind zonal component.'
    varinfo%varattrs(14,2,1) = 'units'
    varinfo%varattrs(14,2,2) = 'meters per second'
    varinfo%varname(14)      = 'wrotu'
    varinfo%varndims(14)     = 3
    varinfo%varnattrs(14)    = 2
    varinfo%vartype(14)      = 'float'
    varinfo%vardimid(14,1)   = dimid(1)
    varinfo%vardimid(14,2)   = dimid(2)
    varinfo%vardimid(14,3)   = dimid(3)
    varinfo%varattrs(15,1,1) = 'title'
    varinfo%varattrs(15,1,2) = 'Rotational wind meridional component.'
    varinfo%varattrs(15,2,1) = 'units'
    varinfo%varattrs(15,2,2) = 'meters per second'
    varinfo%varname(15)      = 'wrotv'
    varinfo%varndims(15)     = 3
    varinfo%varnattrs(15)    = 2
    varinfo%vartype(15)      = 'float'
    varinfo%vardimid(15,1)   = dimid(1)
    varinfo%vardimid(15,2)   = dimid(2)
    varinfo%vardimid(15,3)   = dimid(3)
    varinfo%varattrs(16,1,1) = 'title'
    varinfo%varattrs(16,1,2) = 'Pressure.'
    varinfo%varattrs(16,2,1) = 'units'
    varinfo%varattrs(16,2,2) = 'Pascals'
    varinfo%varname(16)      = 'p'
    varinfo%varndims(16)     = 3
    varinfo%varnattrs(16)    = 2
    varinfo%vartype(16)      = 'float'
    varinfo%vardimid(16,1)   = dimid(1)
    varinfo%vardimid(16,2)   = dimid(2)
    varinfo%vardimid(16,3)   = dimid(3)
    varinfo%varattrs(17,1,1) = 'title'
    varinfo%varattrs(17,1,2) = 'Water Vapor Mixing Ratio.'
    varinfo%varattrs(17,2,1) = 'units'
    varinfo%varattrs(17,2,2) = 'kg per kg'
    varinfo%varname(17)      = 'q'
    varinfo%varndims(17)     = 3
    varinfo%varnattrs(17)    = 2
    varinfo%vartype(17)      = 'float'
    varinfo%vardimid(17,1)   = dimid(1)
    varinfo%vardimid(17,2)   = dimid(2)
    varinfo%vardimid(17,3)   = dimid(3)
    varinfo%varattrs(18,1,1) = 'title'
    varinfo%varattrs(18,1,2) = 'Temperature.'
    varinfo%varattrs(18,2,1) = 'units'
    varinfo%varattrs(18,2,2) = 'Kelvin'
    varinfo%varname(18)      = 't'
    varinfo%varndims(18)     = 3
    varinfo%varnattrs(18)    = 2
    varinfo%vartype(18)      = 'float'
    varinfo%vardimid(18,1)   = dimid(1)
    varinfo%vardimid(18,2)   = dimid(2)
    varinfo%vardimid(18,3)   = dimid(3)
    varinfo%varattrs(19,1,1) = 'title'
    varinfo%varattrs(19,1,2) = 'Zonal Wind.'
    varinfo%varattrs(19,2,1) = 'units'
    varinfo%varattrs(19,2,2) = 'meters per second'
    varinfo%varname(19)      = 'u'
    varinfo%varndims(19)     = 3
    varinfo%varnattrs(19)    = 2
    varinfo%vartype(19)      = 'float'
    varinfo%vardimid(19,1)   = dimid(1)
    varinfo%vardimid(19,2)   = dimid(2)
    varinfo%vardimid(19,3)   = dimid(3)
    varinfo%varattrs(20,1,1) = 'title'
    varinfo%varattrs(20,1,2) = 'Meridional Wind.'
    varinfo%varattrs(20,2,1) = 'units'
    varinfo%varattrs(20,2,2) = 'meters per second'
    varinfo%varname(20)      = 'v'
    varinfo%varndims(20)     = 3
    varinfo%varnattrs(20)    = 2
    varinfo%vartype(20)      = 'float'
    varinfo%vardimid(20,1)   = dimid(1)
    varinfo%vardimid(20,2)   = dimid(2)
    varinfo%vardimid(20,3)   = dimid(3)
    varinfo%varattrs(21,1,1) = 'title'
    varinfo%varattrs(21,1,2) = 'Geopotential Height.'
    varinfo%varattrs(21,2,1) = 'units'
    varinfo%varattrs(21,2,2) = 'meters'
    varinfo%varname(21)      = 'z'
    varinfo%varndims(21)     = 3
    varinfo%varnattrs(21)    = 2
    varinfo%vartype(21)      = 'float'
    varinfo%vardimid(21,1)   = dimid(1)
    varinfo%vardimid(21,2)   = dimid(2)
    varinfo%vardimid(21,3)   = dimid(3)

    ! Deallocate memory for local variables

    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)

    !=====================================================================

  end subroutine varinfo_meteo

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_tccps.f90

  ! DESCRIPTION:

  ! This subroutine defines the FORTRAN varinfo_struct variable used
  ! to write the appropriate variables contained within the FORTRAN
  ! grid_struct and FORTRAN tccps_struct variables to external file
  ! formats.

  ! INPUT VARIABLES:

  ! * tccps; a FORTRAN tccps_struct variable.

  ! * varinfo; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_tccps(tccps,varinfo)

    ! Define variables passed to routine

    type(tccps_struct)                                                  :: tccps
    type(varinfo_struct)                                                :: varinfo

    ! Define variables computed within routine

    character(len=10),          dimension(:),               allocatable :: dimname
    integer,                    dimension(:),               allocatable :: dimval
    integer,                    dimension(:),               allocatable :: dimid

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    varinfo%ndims  = 3
    varinfo%nvars  = 5
    varinfo%nattrs = 2
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Define local variables

    dimname(1) = 'lons'
    dimname(2) = 'lats'
    dimname(3) = 'nz'
    dimval(1)  = tccps%nx
    dimval(2)  = tccps%ny
    dimval(3)  = tccps%nz

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    ! Define local variables

    varinfo%varattrs(1,1,1)  = 'title'
    varinfo%varattrs(1,1,2)  = 'Longitude grid.'
    varinfo%varattrs(1,2,1)  = 'units'
    varinfo%varattrs(1,2,2)  = 'degrees'
    varinfo%varname(1)       = 'lons'
    varinfo%varndims(1)      = 2
    varinfo%varnattrs(1)     = 2
    varinfo%vartype(1)       = 'float'
    varinfo%vardimid(1,1)    = dimid(1)
    varinfo%vardimid(1,2)    = dimid(2)
    varinfo%varattrs(2,1,1)  = 'title'
    varinfo%varattrs(2,1,2)  = 'Latitude grid.'
    varinfo%varattrs(2,2,1)  = 'units'
    varinfo%varattrs(2,2,2)  = 'degrees'
    varinfo%varname(2)       = 'lats'
    varinfo%varndims(2)      = 2
    varinfo%varnattrs(2)     = 2
    varinfo%vartype(2)       = 'float'
    varinfo%vardimid(2,1)    = dimid(1)
    varinfo%vardimid(2,2)    = dimid(2)
    varinfo%varattrs(3,1,2)  = 'Radial distance.'
    varinfo%varattrs(3,2,1)  = 'units'
    varinfo%varattrs(3,2,2)  = 'meters'
    varinfo%varname(3)       = 'raddist'
    varinfo%varndims(3)      = 2
    varinfo%varnattrs(3)     = 2
    varinfo%vartype(3)       = 'float'
    varinfo%vardimid(3,1)    = dimid(1)
    varinfo%vardimid(3,2)    = dimid(2)
    varinfo%varattrs(4,1,1)  = 'title'
    varinfo%varattrs(4,1,2)  = 'Pressure.'
    varinfo%varattrs(4,2,1)  = 'units'
    varinfo%varattrs(4,2,2)  = 'Pascals (Pa)'
    varinfo%varname(4)       = 'pres'
    varinfo%varndims(4)      = 3
    varinfo%varnattrs(4)     = 2
    varinfo%vartype(4)       = 'float'
    varinfo%vardimid(4,1)    = dimid(1)
    varinfo%vardimid(4,2)    = dimid(2)
    varinfo%vardimid(4,3)    = dimid(3)
    varinfo%varattrs(5,1,1)  = 'title'
    varinfo%varattrs(5,1,2)  = 'Height.'
    varinfo%varattrs(5,2,1)  = 'units'
    varinfo%varattrs(5,2,2)  = 'meters (m)'
    varinfo%varname(5)       = 'hgt'
    varinfo%varndims(5)      = 3
    varinfo%varnattrs(5)     = 2
    varinfo%vartype(5)       = 'float'
    varinfo%vardimid(5,1)    = dimid(1)
    varinfo%vardimid(5,2)    = dimid(2)
    varinfo%vardimid(5,3)    = dimid(3)

    ! Deallocate memory for local variables

    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)

    !=====================================================================

  end subroutine varinfo_tccps

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_tcenv.f90

  ! DESCRIPTION:

  ! This subroutine defines the FORTRAN varinfo_struct variable used
  ! to write the appropriate variables contained within the FORTRAN
  ! grid_struct and FORTRAN tcenv_struct variables to external file
  ! formats.

  ! INPUT VARIABLES:

  ! * tcenv; a FORTRAN tcenv_struct variable.

  ! * varinfo; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_tcenv(tcenv,varinfo)

    ! Define variables passed to routine

    type(tcenv_struct)                                                  :: tcenv
    type(varinfo_struct)                                                :: varinfo

    ! Define variables computed within routine

    character(len=10),          dimension(:),               allocatable :: dimname
    integer,                    dimension(:),               allocatable :: dimval
    integer,                    dimension(:),               allocatable :: dimid

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    varinfo%ndims  = 4
    varinfo%nvars  = 27
    varinfo%nattrs = 2
    print*, ''
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Define local variables

    dimname(1) = 'lons'
    dimname(2) = 'lats'
    dimname(3) = 'ntcs'
    dimname(4) = 'tcidstrlen'
    dimval(1)  = tcenv%nx
    dimval(2)  = tcenv%ny
    dimval(3)  = ntcs
    dimval(4)  = 4

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    ! Define local variables

    varinfo%varattrs(1,1,1)   = 'title'
    varinfo%varattrs(1,1,2)   = 'Longitude grid.'
    varinfo%varattrs(1,2,1)   = 'units'
    varinfo%varattrs(1,2,2)   = 'degrees'
    varinfo%varname(1)        = 'lons'
    varinfo%varndims(1)       = 2
    varinfo%varnattrs(1)      = 2
    varinfo%vartype(1)        = 'float'
    varinfo%vardimid(1,1)     = dimid(1)
    varinfo%vardimid(1,2)     = dimid(2)
    varinfo%varattrs(2,1,1)   = 'title'
    varinfo%varattrs(2,1,2)   = 'Latitude grid.'
    varinfo%varattrs(2,2,1)   = 'units'
    varinfo%varattrs(2,2,2)   = 'degrees'
    varinfo%varname(2)        = 'lats'
    varinfo%varndims(2)       = 2
    varinfo%varnattrs(2)      = 2
    varinfo%vartype(2)        = 'float'
    varinfo%vardimid(2,1)     = dimid(1)
    varinfo%vardimid(2,2)     = dimid(2)
    varinfo%varattrs(3,1,1)   = 'title'
    varinfo%varattrs(3,1,2)   = 'SHIPS 850-200 hPa zonal wind.'
    varinfo%varattrs(3,2,1)   = 'units'
    varinfo%varattrs(3,2,2)   = 'meters per second'
    varinfo%varname(3)        = 'u850_200_shear'
    varinfo%varndims(3)       = 2
    varinfo%varnattrs(3)      = 2
    varinfo%vartype(3)        = 'float'
    varinfo%vardimid(3,1)     = dimid(1)
    varinfo%vardimid(3,2)     = dimid(2)
    varinfo%varattrs(4,1,1)   = 'title'
    varinfo%varattrs(4,1,2)   = 'SHIPS 850-200 hPa meridional wind.'
    varinfo%varattrs(4,2,1)   = 'units'
    varinfo%varattrs(4,2,2)   = 'meters per second'
    varinfo%varname(4)        = 'v850_200_shear'
    varinfo%varndims(4)       = 2
    varinfo%varnattrs(4)      = 2
    varinfo%vartype(4)        = 'float'
    varinfo%vardimid(4,1)     = dimid(1)
    varinfo%vardimid(4,2)     = dimid(2)
    varinfo%varattrs(5,1,1)   = 'title'
    varinfo%varattrs(5,1,2)   = '<925 hPa TC intensity zonal wind.'
    varinfo%varattrs(5,2,1)   = 'units'
    varinfo%varattrs(5,2,2)   = 'meters per second'
    varinfo%varname(5)        = 'u925'
    varinfo%varndims(5)       = 2
    varinfo%varnattrs(5)      = 2
    varinfo%vartype(5)        = 'float'
    varinfo%vardimid(5,1)     = dimid(1)
    varinfo%vardimid(5,2)     = dimid(2)
    varinfo%varattrs(6,1,1)   = 'title'
    varinfo%varattrs(6,1,2)   = '<925 hPa TC intensity meridional wind.'
    varinfo%varattrs(6,2,1)   = 'units'
    varinfo%varattrs(6,2,2)   = 'meters per second'
    varinfo%varname(6)        = 'v925'
    varinfo%varndims(6)       = 2
    varinfo%varnattrs(6)      = 2
    varinfo%vartype(6)        = 'float'
    varinfo%vardimid(6,1)     = dimid(1)
    varinfo%vardimid(6,2)     = dimid(2)
    varinfo%varattrs(7,1,1)   = 'title'
    varinfo%varattrs(7,1,2)   = '935-945 hPa TC intensity zonal wind.'
    varinfo%varattrs(7,2,1)   = 'units'
    varinfo%varattrs(7,2,2)   = 'meters per second'
    varinfo%varname(7)        = 'u935'
    varinfo%varndims(7)       = 2
    varinfo%varnattrs(7)      = 2
    varinfo%vartype(7)        = 'float'
    varinfo%vardimid(7,1)     = dimid(1)
    varinfo%vardimid(7,2)     = dimid(2)
    varinfo%varattrs(8,1,1)   = 'title'
    varinfo%varattrs(8,1,2)   = '935-945 hPa TC intensity meridional wind.'
    varinfo%varattrs(8,2,1)   = 'units'
    varinfo%varattrs(8,2,2)   = 'meters per second'
    varinfo%varname(8)        = 'v935'
    varinfo%varndims(8)       = 2
    varinfo%varnattrs(8)      = 2
    varinfo%vartype(8)        = 'float'
    varinfo%vardimid(8,1)     = dimid(1)
    varinfo%vardimid(8,2)     = dimid(2)
    varinfo%varattrs(9,1,1)   = 'title'
    varinfo%varattrs(9,1,2)   = '945-955 hPa TC intensity zonal wind.'
    varinfo%varattrs(9,2,1)   = 'units'
    varinfo%varattrs(9,2,2)   = 'meters per second'
    varinfo%varname(9)        = 'u945'
    varinfo%varndims(9)       = 2
    varinfo%varnattrs(9)      = 2
    varinfo%vartype(9)        = 'float'
    varinfo%vardimid(9,1)     = dimid(1)
    varinfo%vardimid(9,2)     = dimid(2)
    varinfo%varattrs(10,1,1)  = 'title'
    varinfo%varattrs(10,1,2)  = '945-955 hPa TC intensity meridional wind.'
    varinfo%varattrs(10,2,1)  = 'units'
    varinfo%varattrs(10,2,2)  = 'meters per second'
    varinfo%varname(10)       = 'v945'
    varinfo%varndims(10)      = 2
    varinfo%varnattrs(10)     = 2
    varinfo%vartype(10)       = 'float'
    varinfo%vardimid(10,1)    = dimid(1)
    varinfo%vardimid(10,2)    = dimid(2)
    varinfo%varattrs(11,1,1)  = 'title'
    varinfo%varattrs(11,1,2)  = '955-965 hPa TC intensity zonal wind.'
    varinfo%varattrs(11,2,1)  = 'units'
    varinfo%varattrs(11,2,2)  = 'meters per second'
    varinfo%varname(11)       = 'u955'
    varinfo%varndims(11)      = 2
    varinfo%varnattrs(11)     = 2
    varinfo%vartype(11)       = 'float'
    varinfo%vardimid(11,1)    = dimid(1)
    varinfo%vardimid(11,2)    = dimid(2)
    varinfo%varattrs(12,1,1)  = 'title'
    varinfo%varattrs(12,1,2)  = '955-965 hPa TC intensity meridional wind.'
    varinfo%varattrs(12,2,1)  = 'units'
    varinfo%varattrs(12,2,2)  = 'meters per second'
    varinfo%varname(12)       = 'v955'
    varinfo%varndims(12)      = 2
    varinfo%varnattrs(12)     = 2
    varinfo%vartype(12)       = 'float'
    varinfo%vardimid(12,1)    = dimid(1)
    varinfo%vardimid(12,2)    = dimid(2)
    varinfo%varattrs(13,1,1)  = 'title'
    varinfo%varattrs(13,1,2)  = '965-975 hPa TC intensity zonal wind.'
    varinfo%varattrs(13,2,1)  = 'units'
    varinfo%varattrs(13,2,2)  = 'meters per second'
    varinfo%varname(13)       = 'u965'
    varinfo%varndims(13)      = 2
    varinfo%varnattrs(13)     = 2
    varinfo%vartype(13)       = 'float'
    varinfo%vardimid(13,1)    = dimid(1)
    varinfo%vardimid(13,2)    = dimid(2)
    varinfo%varattrs(14,1,1)  = 'title'
    varinfo%varattrs(14,1,2)  = '965-975 hPa TC intensity meridional wind.'
    varinfo%varattrs(14,2,1)  = 'units'
    varinfo%varattrs(14,2,2)  = 'meters per second'
    varinfo%varname(14)       = 'v965'
    varinfo%varndims(14)      = 2
    varinfo%varnattrs(14)     = 2
    varinfo%vartype(14)       = 'float'
    varinfo%vardimid(14,1)    = dimid(1)
    varinfo%vardimid(14,2)    = dimid(2)
    varinfo%varattrs(15,1,1)  = 'title'
    varinfo%varattrs(15,1,2)  = '975-985 hPa TC intensity zonal wind.'
    varinfo%varattrs(15,2,1)  = 'units'
    varinfo%varattrs(15,2,2)  = 'meters per second'
    varinfo%varname(15)       = 'u975'
    varinfo%varndims(15)      = 2
    varinfo%varnattrs(15)     = 2
    varinfo%vartype(15)       = 'float'
    varinfo%vardimid(15,1)    = dimid(1)
    varinfo%vardimid(15,2)    = dimid(2)
    varinfo%varattrs(16,1,1)  = 'title'
    varinfo%varattrs(16,1,2)  = '975-985 hPa TC intensity meridional wind.'
    varinfo%varattrs(16,2,1)  = 'units'
    varinfo%varattrs(16,2,2)  = 'meters per second'
    varinfo%varname(16)       = 'v975'
    varinfo%varndims(16)      = 2
    varinfo%varnattrs(16)     = 2
    varinfo%vartype(16)       = 'float'
    varinfo%vardimid(16,1)    = dimid(1)
    varinfo%vardimid(16,2)    = dimid(2)
    varinfo%varattrs(17,1,1)  = 'title'
    varinfo%varattrs(17,1,2)  = '985-995 hPa TC intensity zonal wind.'
    varinfo%varattrs(17,2,1)  = 'units'
    varinfo%varattrs(17,2,2)  = 'meters per second'
    varinfo%varname(17)       = 'u985'
    varinfo%varndims(17)      = 2
    varinfo%varnattrs(17)     = 2
    varinfo%vartype(17)       = 'float'
    varinfo%vardimid(17,1)    = dimid(1)
    varinfo%vardimid(17,2)    = dimid(2)
    varinfo%varattrs(18,1,1)  = 'title'
    varinfo%varattrs(18,1,2)  = '985-995 hPa TC intensity meridional wind.'
    varinfo%varattrs(18,2,1)  = 'units'
    varinfo%varattrs(18,2,2)  = 'meters per second'
    varinfo%varname(18)       = 'v985'
    varinfo%varndims(18)      = 2
    varinfo%varnattrs(18)     = 2
    varinfo%vartype(18)       = 'float'
    varinfo%vardimid(18,1)    = dimid(1)
    varinfo%vardimid(18,2)    = dimid(2)
    varinfo%varattrs(19,1,1)  = 'title'
    varinfo%varattrs(19,1,2)  = '995-1005 hPa TC intensity zonal wind.'
    varinfo%varattrs(19,2,1)  = 'units'
    varinfo%varattrs(19,2,2)  = 'meters per second'
    varinfo%varname(19)       = 'u995'
    varinfo%varndims(19)      = 2
    varinfo%varnattrs(19)     = 2
    varinfo%vartype(19)       = 'float'
    varinfo%vardimid(19,1)    = dimid(1)
    varinfo%vardimid(19,2)    = dimid(2)
    varinfo%varattrs(20,1,1)  = 'title'
    varinfo%varattrs(20,1,2)  = '995-1005 hPa TC intensity meridional wind.'
    varinfo%varattrs(20,2,1)  = 'units'
    varinfo%varattrs(20,2,2)  = 'meters per second'
    varinfo%varname(20)       = 'v995'
    varinfo%varndims(20)      = 2
    varinfo%varnattrs(20)     = 2
    varinfo%vartype(20)       = 'float'
    varinfo%vardimid(20,1)    = dimid(1)
    varinfo%vardimid(20,2)    = dimid(2)
    varinfo%varattrs(21,1,1)  = 'title'
    varinfo%varattrs(21,1,2)  = '> 1005 hPa TC intensity zonal wind.'
    varinfo%varattrs(21,2,1)  = 'units'
    varinfo%varattrs(21,2,2)  = 'meters per second'
    varinfo%varname(21)       = 'u1005'
    varinfo%varndims(21)      = 2
    varinfo%varnattrs(21)     = 2
    varinfo%vartype(21)       = 'float'
    varinfo%vardimid(21,1)    = dimid(1)
    varinfo%vardimid(21,2)    = dimid(2)
    varinfo%varattrs(22,1,1)  = 'title'
    varinfo%varattrs(22,1,2)  = '> 1005 hPa TC intensity meridional wind.'
    varinfo%varattrs(22,2,1)  = 'units'
    varinfo%varattrs(22,2,2)  = 'meters per second'
    varinfo%varname(22)       = 'v1005'
    varinfo%varndims(22)      = 2
    varinfo%varnattrs(22)     = 2
    varinfo%vartype(22)       = 'float'
    varinfo%vardimid(22,1)    = dimid(1)
    varinfo%vardimid(22,2)    = dimid(2)
    varinfo%varattrs(23,1,1)  = 'title'
    varinfo%varattrs(23,1,2)  = '850-200 hPa zonal wind.'
    varinfo%varattrs(23,2,1)  = 'units'
    varinfo%varattrs(23,2,2)  = 'meters per second'
    varinfo%varname(23)       = 'u850_200'
    varinfo%varndims(23)      = 2
    varinfo%varnattrs(23)     = 2
    varinfo%vartype(23)       = 'float'
    varinfo%vardimid(23,1)    = dimid(1)
    varinfo%vardimid(23,2)    = dimid(2)
    varinfo%varattrs(24,1,1)  = 'title'
    varinfo%varattrs(24,1,2)  = '850-200 hPa meridional wind.'
    varinfo%varattrs(24,2,1)  = 'units'
    varinfo%varattrs(24,2,2)  = 'meters per second'
    varinfo%varname(24)       = 'v850_200'
    varinfo%varndims(24)      = 2
    varinfo%varnattrs(24)     = 2
    varinfo%vartype(24)       = 'float'
    varinfo%vardimid(24,1)    = dimid(1)
    varinfo%vardimid(24,2)    = dimid(2)
    varinfo%varattrs(25,1,1) = 'title'
    varinfo%varattrs(25,1,2) = 'TC identifiers.'
    varinfo%varname(25)      = 'tcid'
    varinfo%varndims(25)     = 2
    varinfo%varnattrs(25)    = 1
    varinfo%vartype(25)      = 'char'
    varinfo%vardimid(25,1)   = dimid(4)
    varinfo%vardimid(25,2)   = dimid(3)
    varinfo%varattrs(26,1,1) = 'title'
    varinfo%varattrs(26,1,2) = 'TC latitudes.'
    varinfo%varname(26)      = 'tclat'
    varinfo%varndims(26)     = 1
    varinfo%varnattrs(26)    = 1
    varinfo%vartype(26)      = 'float'
    varinfo%vardimid(26,1)   = dimid(3)
    varinfo%varattrs(27,1,1) = 'title'
    varinfo%varattrs(27,1,2) = 'TC longitudes.'
    varinfo%varname(27)      = 'tclon'
    varinfo%varndims(27)     = 1
    varinfo%varnattrs(27)    = 1
    varinfo%vartype(27)      = 'float'
    varinfo%vardimid(27,1)   = dimid(3)
    
    ! Deallocate memory for local variables
    
    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)
    
    !=====================================================================

  end subroutine varinfo_tcenv

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_tcmpi.f90

  ! DESCRIPTION:

  ! This subroutine defines the FORTRAN varinfo_struct variable used
  ! to write the appropriate variables contained within the FORTRAN
  ! grid_struct and FORTRAN tcmpi_struct variables to external file
  ! formats.

  ! INPUT VARIABLES:

  ! * tcmpi; a FORTRAN tcmpi_struct variable.

  ! * varinfo; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_tcmpi(tcmpi,varinfo)

    ! Define variables passed to routine

    type(tcmpi_struct)                                                  :: tcmpi
    type(varinfo_struct)                                                :: varinfo

    ! Define variables computed within routine

    character(len=10),          dimension(:),               allocatable :: dimname
    integer,                    dimension(:),               allocatable :: dimval
    integer,                    dimension(:),               allocatable :: dimid

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    varinfo%ndims  = 5
    varinfo%nvars  = 13
    varinfo%nattrs = 2
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Define local variables

    dimname(1) = 'lons'
    dimname(2) = 'lats'
    dimname(3) = 'nz'
    dimname(4) = 'ntcs'
    dimname(5) = 'tcidstrlen'
    dimval(1)  = tcmpi%nx
    dimval(2)  = tcmpi%ny
    dimval(3)  = tcmpi%nz
    dimval(4)  = ntcs
    dimval(5)  = 4

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    ! Define local variables

    varinfo%varattrs(1,1,1)  = 'title'
    varinfo%varattrs(1,1,2)  = 'Longitude grid.'
    varinfo%varattrs(1,2,1)  = 'units'
    varinfo%varattrs(1,2,2)  = 'degrees'
    varinfo%varname(1)       = 'lons'
    varinfo%varndims(1)      = 2
    varinfo%varnattrs(1)     = 2
    varinfo%vartype(1)       = 'float'
    varinfo%vardimid(1,1)    = dimid(1)
    varinfo%vardimid(1,2)    = dimid(2)
    varinfo%varattrs(2,1,1)  = 'title'
    varinfo%varattrs(2,1,2)  = 'Latitude grid.'
    varinfo%varattrs(2,2,1)  = 'units'
    varinfo%varattrs(2,2,2)  = 'degrees'
    varinfo%varname(2)       = 'lats'
    varinfo%varndims(2)      = 2
    varinfo%varnattrs(2)     = 2
    varinfo%vartype(2)       = 'float'
    varinfo%vardimid(2,1)    = dimid(1)
    varinfo%vardimid(2,2)    = dimid(2)
    varinfo%varattrs(3,1,1)  = 'title'
    varinfo%varattrs(3,1,2)  = 'MPI maximum wind speed.'
    varinfo%varattrs(3,2,1)  = 'units'
    varinfo%varattrs(3,2,2)  = 'meters per second (m/s)'
    varinfo%varname(3)       = 'vmax'
    varinfo%varndims(3)      = 2
    varinfo%varnattrs(3)     = 2
    varinfo%vartype(3)       = 'float'
    varinfo%vardimid(3,1)    = dimid(1)
    varinfo%vardimid(3,2)    = dimid(2)
    varinfo%varattrs(4,1,1)  = 'title'
    varinfo%varattrs(4,1,2)  = 'MPI maximum wind speed.'
    varinfo%varattrs(4,2,1)  = 'units'
    varinfo%varattrs(4,2,2)  = 'hectoPascals (hPa)'
    varinfo%varname(4)       = 'pmin'
    varinfo%varndims(4)      = 2
    varinfo%varnattrs(4)     = 2
    varinfo%vartype(4)       = 'float'
    varinfo%vardimid(4,1)    = dimid(1)
    varinfo%vardimid(4,2)    = dimid(2)
    varinfo%varattrs(5,1,1)  = 'title'
    varinfo%varattrs(5,1,2)  = 'MPI convergence flag.'
    varinfo%varname(5)       = 'ifl'
    varinfo%varndims(5)      = 2
    varinfo%varnattrs(5)     = 2
    varinfo%vartype(5)       = 'integer'
    varinfo%vardimid(5,1)    = dimid(1)
    varinfo%vardimid(5,2)    = dimid(2)
    varinfo%varattrs(6,1,1)  = 'title'
    varinfo%varattrs(6,1,2)  = 'Sea-level pressure.'
    varinfo%varattrs(6,2,1)  = 'units'
    varinfo%varattrs(6,2,2)  = 'Pascals (Pa)'
    varinfo%varname(6)       = 'pmsl'
    varinfo%varndims(6)      = 2
    varinfo%varnattrs(6)     = 2
    varinfo%vartype(6)       = 'float'
    varinfo%vardimid(6,1)    = dimid(1)
    varinfo%vardimid(6,2)    = dimid(2)
    varinfo%varattrs(7,1,1)  = 'title'
    varinfo%varattrs(7,1,2)  = 'Surface temperature.'
    varinfo%varattrs(7,2,1)  = 'units'
    varinfo%varattrs(7,2,2)  = 'Kelvin (K)'
    varinfo%varname(7)       = 'tsfc'
    varinfo%varndims(7)      = 2
    varinfo%varnattrs(7)     = 2
    varinfo%vartype(7)       = 'float'
    varinfo%vardimid(7,1)    = dimid(1)
    varinfo%vardimid(7,2)    = dimid(2)
    varinfo%varattrs(8,1,1)  = 'title'
    varinfo%varattrs(8,1,2)  = 'Mixing ratio.'
    varinfo%varattrs(8,2,1)  = 'units'
    varinfo%varattrs(8,2,2)  = 'kilogram per kilogram (kg/kg)'
    varinfo%varname(8)       = 'mxrt'
    varinfo%varndims(8)      = 3
    varinfo%varnattrs(8)     = 2
    varinfo%vartype(8)       = 'float'
    varinfo%vardimid(8,1)    = dimid(1)
    varinfo%vardimid(8,2)    = dimid(2)
    varinfo%vardimid(8,3)    = dimid(3)
    varinfo%varattrs(9,1,1)  = 'title'
    varinfo%varattrs(9,1,2)  = 'Pressure.'
    varinfo%varattrs(9,2,1)  = 'units'
    varinfo%varattrs(9,2,2)  = 'Pascals (Pa)'
    varinfo%varname(9)       = 'pres'
    varinfo%varndims(9)      = 3
    varinfo%varnattrs(9)     = 2
    varinfo%vartype(9)       = 'float'
    varinfo%vardimid(9,1)    = dimid(1)
    varinfo%vardimid(9,2)    = dimid(2)
    varinfo%vardimid(9,3)    = dimid(3)
    varinfo%varattrs(10,1,1) = 'title'
    varinfo%varattrs(10,1,2) = 'Temperature.'
    varinfo%varattrs(10,2,1) = 'units'
    varinfo%varattrs(10,2,2) = 'Kelvin (K)'
    varinfo%varname(10)      = 'temp'
    varinfo%varndims(10)     = 3
    varinfo%varnattrs(10)    = 2
    varinfo%vartype(10)      = 'float'
    varinfo%vardimid(10,1)   = dimid(1)
    varinfo%vardimid(10,2)   = dimid(2)
    varinfo%vardimid(10,3)   = dimid(3)
    varinfo%varattrs(11,1,1) = 'title'
    varinfo%varattrs(11,1,2) = 'TC identifiers.'
    varinfo%varname(11)      = 'tcid'
    varinfo%varndims(11)     = 2
    varinfo%varnattrs(11)    = 1
    varinfo%vartype(11)      = 'char'
    varinfo%vardimid(11,1)   = dimid(5)
    varinfo%vardimid(11,2)   = dimid(4)
    varinfo%varattrs(12,1,1) = 'title'
    varinfo%varattrs(12,1,2) = 'TC latitudes.'
    varinfo%varname(12)      = 'tclat'
    varinfo%varndims(12)     = 1
    varinfo%varnattrs(12)    = 1
    varinfo%vartype(12)      = 'float'
    varinfo%vardimid(12,1)   = dimid(4)
    varinfo%varattrs(13,1,1) = 'title'
    varinfo%varattrs(13,1,2) = 'TC longitudes.'
    varinfo%varname(13)      = 'tclon'
    varinfo%varndims(13)     = 1
    varinfo%varnattrs(13)    = 1
    varinfo%vartype(13)      = 'float'
    varinfo%vardimid(13,1)   = dimid(4)

    ! Deallocate memory for local variables

    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)

    !=====================================================================

  end subroutine varinfo_tcmpi

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_tcmsi.f90

  ! DESCRIPTION:

  ! This subroutine defines the FORTRAN varinfo_struct variable used
  ! to write the appropriate variables contained within the FORTRAN
  ! grid_struct and FORTRAN tcmsi_struct variables to external file
  ! formats.

  ! INPUT VARIABLES:

  ! * tcmsi; a FORTRAN tcmsi_struct variable.

  ! * varinfo; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_tcmsi(tcmsi,varinfo)

    ! Define variables passed to routine

    type(tcmsi_struct)                                                  :: tcmsi
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
    varinfo%nvars  = 4 + tcmsi%nh
    varinfo%nattrs = 2
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Define local variables

    dimname(1) = 'lons'
    dimname(2) = 'lats'
    dimval(1)  = tcmsi%nx
    dimval(2)  = tcmsi%ny

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    ! Define local variables

    varinfo%varattrs(1,1,1) = 'title'
    varinfo%varattrs(1,1,2) = 'Longitude grid.'
    varinfo%varattrs(1,2,1) = 'units'
    varinfo%varattrs(1,2,2) = 'degrees'
    varinfo%varname(1)      = 'lons'
    varinfo%varndims(1)     = 2
    varinfo%varnattrs(1)    = 2
    varinfo%vartype(1)      = 'float'
    varinfo%vardimid(1,1)   = dimid(1)
    varinfo%vardimid(1,2)   = dimid(2)
    varinfo%varattrs(2,1,1) = 'title'
    varinfo%varattrs(2,1,2) = 'Latitude grid.'
    varinfo%varattrs(2,2,1) = 'units'
    varinfo%varattrs(2,2,2) = 'degrees'
    varinfo%varname(2)      = 'lats'
    varinfo%varndims(2)     = 2
    varinfo%varnattrs(2)    = 2
    varinfo%vartype(2)      = 'float'
    varinfo%vardimid(2,1)   = dimid(1)
    varinfo%vardimid(2,2)   = dimid(2)
    varinfo%varattrs(3,1,1) = 'title'
    varinfo%varattrs(3,1,2) = 'Radial distance grid.'
    varinfo%varattrs(3,2,1) = 'units'
    varinfo%varattrs(3,2,2) = 'meters (m)'
    varinfo%varname(3)      = 'radius'
    varinfo%varndims(3)     = 2
    varinfo%varnattrs(3)    = 2
    varinfo%vartype(3)      = 'float'
    varinfo%vardimid(3,1)   = dimid(1)
    varinfo%vardimid(3,2)   = dimid(2)
    varinfo%varattrs(4,1,1) = 'title'
    varinfo%varattrs(4,1,2) = '10-meter wind speed.'
    varinfo%varattrs(4,2,1) = 'units'
    varinfo%varattrs(4,2,2) = 'meters per second (m/s)'
    varinfo%varname(4)      = 'wnd10m'
    varinfo%varndims(4)     = 2
    varinfo%varnattrs(4)    = 2
    varinfo%vartype(4)      = 'float'
    varinfo%vardimid(4,1)   = dimid(1)
    varinfo%vardimid(4,2)   = dimid(2)

    ! Loop through local variable

    do i = 1, tcmsi%nh

       ! Define local variables

       varinfo%varattrs(4+i,1,1) = 'title'
       write(varinfo%varattrs(4+i,1,2),500) (i-1)
       varinfo%varattrs(4+i,2,1) = 'units'
       varinfo%varattrs(4+i,2,2) = 'meters per second (m/s)'
       write(varinfo%varname(4+i),501) (i-1)
       varinfo%varndims(4+i)     = 2
       varinfo%varnattrs(4+i)    = 2
       varinfo%vartype(4+i)      = 'float'
       varinfo%vardimid(4+i,1)   = dimid(1)
       varinfo%vardimid(4+i,2)   = dimid(2)

    end do ! do i = 1, tcmsi%nh

    ! Deallocate memory for local variables

    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)

    ! Define local variables

500 format('Wavenumber ',i3.3,' wind speed.')
501 format('wn'i3.3)

    !=====================================================================

  end subroutine varinfo_tcmsi

  !=======================================================================

  ! SUBROUTINE:

  ! write_filter.f90

  ! DESCRIPTION:

  ! This subroutine writes the appropriate variables contained within
  ! the FORTRAN filter_struct variables to an external netcdf file.

  ! INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable.

  ! * filter; a FORTRAN filter_struct variable.

  !-----------------------------------------------------------------------

  subroutine write_filter(tcv,filter)

    ! Define variables passed to routine

    type(filter_struct)                                                 :: filter
    type(tcv_struct)                                                    :: tcv

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:,:,:),         allocatable :: work
    integer                                                             :: n1dvar
    integer                                                             :: n2dvar
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(mpi_procid .eq. mpi_masternode) then

       ! Define local variables

       call fileio_interface_varinfo(filter,varinfo)
       filename = trim(adjustl(datapath))//'filter.'//tcv%tcid//'.'//      &
            & analdate//'.nc'
       call netcdf_interface_open(filename,.false.,.false.,.true.)
       call netcdf_interface_writedef(varinfo)
       call netcdf_interface_close()

       ! Allocate memory for local variables

       if(.not. allocated(work)) allocate(work(2,1,1,filter%nz))
       
       ! Loop through local variable
       
       do i = 1, filter%nz
          
          ! Define local variables
          
          work(1,1,1,i) = filter%clat(i)
          work(2,1,1,i) = filter%clon(i)

       end do ! do i = 1, filter%nz

       ! Define local variables

       n1dvar = size(work(:,1,1,1))

       ! Loop through local variable

       do i = 1, n1dvar

          ! Define local variables

          varname = varinfo%varname(i)
          call netcdf_interface_putvar(filename,varname,work(i,1,1,:))

       end do ! do i = 1, n1dvar

       ! Deallocate memory for local variables

       if(allocated(work)) deallocate(work)

       ! Allocate memory for local variables

       if(.not. allocated(work)) allocate(work(2,filter%nx,filter%ny,1))

       ! Define local variables

       work(1,:,:,1) = reshape(filter%lat,shape(work(1,:,:,1)))
       work(2,:,:,1) = reshape(filter%lon,shape(work(2,:,:,1)))
       n2dvar        = size(work(:,1,1,1))

       ! Loop through local variable

       do i = 1, n2dvar

       ! Define local variables

          varname = varinfo%varname(n1dvar+i)
          call netcdf_interface_putvar(filename,varname,work(i,:,:,1))

       end do ! do i = 1, n2dvar

       ! Deallocate memory for local variables

       if(allocated(work)) deallocate(work)

       ! Allocate memory for local variables

       if(.not. allocated(work))                                           &
            & allocate(work(5,filter%nx,filter%ny,filter%nz))

       ! Loop through local variable

       do i = 1, filter%nz

          ! Define local variables

          work(1,:,:,i) = reshape(filter%angle(:,i),shape(work(1,:,:,i)))
          work(2,:,:,i) = reshape(filter%kfltr(:,i),shape(work(2,:,:,i)))
          work(3,:,:,i) = reshape(filter%radius(:,i),shape(work(3,:,:,i)))
          work(4,:,:,i) = reshape(filter%tngwnd(:,i),shape(work(4,:,:,i)))
          work(5,:,:,i) = reshape(filter%tfltr(:,i),shape(work(5,:,:,i)))

       end do ! do i = 1, filter%nz

       ! Loop through local variable

       do i = 1, size(work(:,1,1,1))

          ! Define local variables
          
          varname = varinfo%varname((n1dvar + n2dvar) + i)
          call netcdf_interface_putvar(filename,varname,work(i,:,:,:))

       end do ! do i = 1, size(work(:,1,1,1))
       
       ! Deallocate memory for local variables

       if(allocated(work)) deallocate(work)
       call variable_interface_cleanup_struct(varinfo)

    end if ! if(mpi_procid .eq. mpi_masternode)

    ! Define local variables

    call mpi_interface_waitall()

    !=====================================================================

  end subroutine write_filter

  !=======================================================================

  ! SUBROUTINE:

  ! write_meteo.f90

  ! DESCRIPTION:

  ! This subroutine writes the appropriate variables contained within
  ! the FORTRAN meteo_struct variables to an external netcdf file.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string containing the file to be
  !   written to.

  ! * meteo; a FORTRAN meteo_struct variable.
  
  !-----------------------------------------------------------------------

  subroutine write_meteo(filename,meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo
    character(len=500)                                                  :: filename

    ! Define variables computed within routine
    
    type(varinfo_struct)                                                :: varinfo
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:,:,:),         allocatable :: work
    integer                                                             :: n2dvar

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(mpi_procid .eq. mpi_masternode) then

       ! Define local variables

       call netcdf_interface_open(filename,.false.,.false.,.true.)
       call fileio_interface_varinfo(meteo,varinfo)
       call netcdf_interface_writedef(varinfo)
       call netcdf_interface_close()

       ! Allocate memory for local variables

       if(.not. allocated(work)) allocate(work(4,meteo%nx,meteo%ny,1))

       ! Define local variables

       work(1,:,:,1) = reshape(meteo%xlat,shape(work(1,:,:,1)))
       work(2,:,:,1) = reshape(meteo%xlon,shape(work(2,:,:,1)))
       work(3,:,:,1) = reshape(meteo%orog,shape(work(3,:,:,1)))
       work(4,:,:,1) = reshape(meteo%pmsl,shape(work(4,:,:,1)))
       n2dvar        = size(work(:,1,1,1))

       ! Loop through local variable

       do i = 1, n2dvar

          ! Define local variables

          varname = varinfo%varname(i)
          call netcdf_interface_putvar(filename,varname,work(i,:,:,1))

       end do ! do i = 1, n2dvar
          
       ! Deallocate memory for local variables

       if(allocated(work)) deallocate(work)

       ! Allocate memory for local variables

       if(.not. allocated(work))                                           &
            & allocate(work(17,meteo%nx,meteo%ny,meteo%nz))

       ! Loop through local variable

       do i = 1, meteo%nz

          ! Define local variables

          work(1,:,:,i)  = reshape(meteo%chi(:,i),shape(work(1,:,:,i)))
          work(2,:,:,i)  = reshape(meteo%divg(:,i),shape(work(2,:,:,i)))
          work(3,:,:,i)  = reshape(meteo%psi(:,i),shape(work(3,:,:,i)))
          work(4,:,:,i)  = reshape(meteo%pv(:,i),shape(work(4,:,:,i)))
          work(5,:,:,i)  = reshape(meteo%vort(:,i),shape(work(5,:,:,i)))
          work(6,:,:,i)  = reshape(meteo%wdivu(:,i),shape(work(6,:,:,i)))
          work(7,:,:,i)  = reshape(meteo%wdivv(:,i),shape(work(7,:,:,i)))
          work(8,:,:,i)  = reshape(meteo%whrmu(:,i),shape(work(8,:,:,i)))
          work(9,:,:,i)  = reshape(meteo%whrmv(:,i),shape(work(9,:,:,i)))
          work(10,:,:,i) = reshape(meteo%wrotu(:,i),shape(work(10,:,:,i)))
          work(11,:,:,i) = reshape(meteo%wrotv(:,i),shape(work(11,:,:,i)))
          work(12,:,:,i) = reshape(meteo%p(:,i),shape(work(12,:,:,i)))
          work(13,:,:,i) = reshape(meteo%q(:,i),shape(work(13,:,:,i)))
          work(14,:,:,i) = reshape(meteo%t(:,i),shape(work(14,:,:,i)))
          work(15,:,:,i) = reshape(meteo%u(:,i),shape(work(15,:,:,i)))
          work(16,:,:,i) = reshape(meteo%v(:,i),shape(work(16,:,:,i)))
          work(17,:,:,i) = reshape(meteo%z(:,i),shape(work(17,:,:,i)))

       end do ! do i = 1, meteo%nz

       ! Loop through local variable
       
       do i = 1, size(work(:,1,1,1))

          ! Define local variables

          varname = varinfo%varname(n2dvar+i)
          call netcdf_interface_putvar(filename,varname,work(i,:,:,:))
          
       end do ! do i = 1, size(work(:,1,1,1))
      
       ! Deallocate memory for local variables

       if(allocated(work)) deallocate(work)
       call variable_interface_cleanup_struct(varinfo)

    end if ! if(mpi_procid .eq. mpi_masternode)

    ! Define local variables

    call mpi_interface_waitall()
       
    !=====================================================================

  end subroutine write_meteo
  
  !=======================================================================

  ! SUBROUTINE:

  ! write_tccps.f90

  ! DESCRIPTION:

  ! This subroutine writes the appropriate variables contained within
  ! the FORTRAN tccps_struct variables to an external netcdf file.

  ! INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable.

  ! * tccps; a FORTRAN tccps_struct variable.

  !-----------------------------------------------------------------------

  subroutine write_tccps(tcv,tccps)

    ! Define variables passed to routine

    type(tccps_struct)                                                  :: tccps
    type(tcv_struct)                                                    :: tcv

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: attrname
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:,:,:),         allocatable :: work
    integer                                                             :: n2dvar

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(mpi_procid .eq. mpi_masternode) then

       ! Define local variables

       call fileio_interface_varinfo(tccps,varinfo)
       filename = trim(adjustl(datapath))//'tccps.'//tcv%tcid//'.'//       &
            & analdate//'.nc'
       call netcdf_interface_open(filename,.false.,.false.,.true.)
       attrname = 'vlt'
       call netcdf_interface_putattr(attrname,tccps%vlt)
       attrname = 'vut'
       call netcdf_interface_putattr(attrname,tccps%vut)
       attrname = 'reflat'
       call netcdf_interface_putattr(attrname,tcv%reflat)
       attrname = 'reflon'
       call netcdf_interface_putattr(attrname,tcv%reflon)
       call netcdf_interface_writedef(varinfo)
       call netcdf_interface_close()

       ! Allocate memory for local variables

       if(.not. allocated(work)) allocate(work(3,tccps%nx,tccps%ny,1))

       ! Define local variables

       work(1,:,:,1) = reshape(tccps%lons,(/tccps%nx,tccps%ny/))
       work(2,:,:,1) = reshape(tccps%lats,(/tccps%nx,tccps%ny/))
       work(3,:,:,1) = reshape(tccps%radius,(/tccps%nx,tccps%ny/))
       n2dvar        = size(work(:,1,1,1))

       ! Loop through local variable

       do i = 1, n2dvar

          ! Define local variables

          varname = varinfo%varname(i)
          call netcdf_interface_putvar(filename,varname,work(i,:,:,1))

       end do ! do i = 1, n2dvar

       ! Deallocate memory for local variables

       if(allocated(work)) deallocate(work)

       ! Allocate memory for local variables

       if(.not. allocated(work))                                           &
            & allocate(work(2,tccps%nx,tccps%ny,tccps%nz))

       ! Loop through local variable

       do i = 1, tccps%nz

          ! Define local variables

          work(1,:,:,i) = reshape(tccps%p(:,i),(/tccps%nx,tccps%ny/))          
          work(2,:,:,i) = reshape(tccps%z(:,i),(/tccps%nx,tccps%ny/))

       end do ! do i = 1, tccps%nz

       ! Loop through local variable
       
       do i = 1, size(work(:,1,1,1))

          ! Define local variables

          varname = varinfo%varname(n2dvar + i)
          call netcdf_interface_putvar(filename,varname,work(i,:,:,:))
          
       end do ! do i = 1, size(work(:,1,1,1))

       ! Deallocate memory for local variables

       if(allocated(work)) deallocate(work)
       call variable_interface_cleanup_struct(varinfo)

    end if ! if(mpi_procid .eq. mpi_masternode)

    ! Define local variables

    call mpi_interface_waitall()

    !=====================================================================

  end subroutine write_tccps

  !=======================================================================

  ! SUBROUTINE:

  ! write_tcenv.f90

  ! DESCRIPTION:

  ! This subroutine writes the appropriate variables contained within
  ! the FORTRAN tcenv_struct variables to an external netcdf file.

  ! INPUT VARIABLES:

  ! * tcenv; a FORTRAN tcenv_struct variable.

  !-----------------------------------------------------------------------

  subroutine write_tcenv(tcenv)

    ! Define variables passed to routine

    type(tcenv_struct)                                                  :: tcenv

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    character(len=4),           dimension(:),               allocatable :: tcid
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: attrname
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:,:,:),         allocatable :: work
    integer                                                             :: n2dvar
    integer                                                             :: nvar
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(mpi_procid .eq. mpi_masternode) then

       ! Define local variables

       call fileio_interface_varinfo(tcenv,varinfo)
       filename = trim(adjustl(datapath))//'tcenv.'//analdate//'.nc'
       call netcdf_interface_open(filename,.false.,.false.,.true.)
       attrname = 'valid_time'
       call netcdf_interface_putattr(attrname,tcenv%valid_time)
       call netcdf_interface_writedef(varinfo)
       call netcdf_interface_close()

       ! Allocate memory for local variables

       if(.not. allocated(work)) allocate(work(24,tcenv%nx,tcenv%ny,1))

       ! Define local variables

       work(1,:,:,1)  = reshape(tcenv%lons,(/tcenv%nx,tcenv%ny/))
       work(2,:,:,1)  = reshape(tcenv%lats,(/tcenv%nx,tcenv%ny/))
       work(3,:,:,1)  = reshape(tcenv%u850_200_shear,                      &
            & (/tcenv%nx,tcenv%ny/))
       work(4,:,:,1)  = reshape(tcenv%v850_200_shear,                      &
            & (/tcenv%nx,tcenv%ny/))       
       work(5,:,:,1)  = reshape(tcenv%u925,(/tcenv%nx,tcenv%ny/))
       work(6,:,:,1)  = reshape(tcenv%v925,(/tcenv%nx,tcenv%ny/))
       work(7,:,:,1)  = reshape(tcenv%u935,(/tcenv%nx,tcenv%ny/))
       work(8,:,:,1)  = reshape(tcenv%v935,(/tcenv%nx,tcenv%ny/))       
       work(9,:,:,1)  = reshape(tcenv%u945,(/tcenv%nx,tcenv%ny/))
       work(10,:,:,1) = reshape(tcenv%v945,(/tcenv%nx,tcenv%ny/))
       work(11,:,:,1) = reshape(tcenv%u955,(/tcenv%nx,tcenv%ny/))
       work(12,:,:,1) = reshape(tcenv%v955,(/tcenv%nx,tcenv%ny/))
       work(13,:,:,1) = reshape(tcenv%u965,(/tcenv%nx,tcenv%ny/))
       work(14,:,:,1) = reshape(tcenv%v965,(/tcenv%nx,tcenv%ny/))
       work(15,:,:,1) = reshape(tcenv%u975,(/tcenv%nx,tcenv%ny/))
       work(16,:,:,1) = reshape(tcenv%v975,(/tcenv%nx,tcenv%ny/))       
       work(17,:,:,1) = reshape(tcenv%u985,(/tcenv%nx,tcenv%ny/))
       work(18,:,:,1) = reshape(tcenv%v985,(/tcenv%nx,tcenv%ny/))
       work(19,:,:,1) = reshape(tcenv%u995,(/tcenv%nx,tcenv%ny/))
       work(20,:,:,1) = reshape(tcenv%v995,(/tcenv%nx,tcenv%ny/))
       work(21,:,:,1) = reshape(tcenv%u1005,(/tcenv%nx,tcenv%ny/))
       work(22,:,:,1) = reshape(tcenv%v1005,(/tcenv%nx,tcenv%ny/))
       work(23,:,:,1) = reshape(tcenv%u850_200,(/tcenv%nx,tcenv%ny/))
       work(24,:,:,1) = reshape(tcenv%v850_200,(/tcenv%nx,tcenv%ny/)) 
       n2dvar         = size(work(:,1,1,1))

       ! Loop through local variable

       do i = 1, n2dvar

          ! Define local variables

          varname = varinfo%varname(i)
          call netcdf_interface_putvar(filename,varname,work(i,:,:,1))

       end do ! do i = 1, n2dvar

       ! Deallocate memory for local variables

       if(allocated(work)) deallocate(work)

       ! Allocate memory for local variables

       if(.not. allocated(tcid)) allocate(tcid(ntcs))
       if(.not. allocated(work)) allocate(work(2,ntcs,1,1))       

       ! Loop through local variable

       do i = 1, ntcs

          ! Define local variables

          tcid(i)       = tcenv%tcid(i)
          work(1,i,1,1) = tcenv%tclat(i)
          work(2,i,1,1) = tcenv%tclon(i)

       end do ! do i = 1, ntcs

       ! Define local variables

       nvar    = n2dvar + 1
       varname = varinfo%varname(nvar)
       call netcdf_interface_putvar(filename,varname,tcid)

       ! Loop through local variable

       do i = 1, size(work(:,1,1,1))
          
          ! Define local variables

          varname = varinfo%varname(nvar + i)
          call netcdf_interface_putvar(filename,varname,work(i,:,1,1))
          
       end do ! do i = 1, size(work(:,1,1,1))

       ! Deallocate memory for local variables

       if(allocated(tcid)) deallocate(tcid)
       if(allocated(work)) deallocate(work)
       call variable_interface_cleanup_struct(varinfo)       
       
    end if ! if(mpi_procid .eq. mpi_masternode)

    ! Define local variables

    call mpi_interface_waitall()

    !=====================================================================

  end subroutine write_tcenv

  !=======================================================================

  ! SUBROUTINE:

  ! write_tcmpi.f90

  ! DESCRIPTION:

  ! This subroutine writes the appropriate variables contained within
  ! the FORTRAN grid_struct and FORTRAN tcmpi_struct variables to an
  ! external netcdf file.

  ! INPUT VARIABLES:

  ! * tcmpi; a FORTRAN tcmpi_struct variable.

  !-----------------------------------------------------------------------

  subroutine write_tcmpi(tcmpi)

    ! Define variables passed to routine

    type(tcmpi_struct)                                                  :: tcmpi

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    character(len=4),           dimension(:),               allocatable :: tcid
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: attrname
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:,:,:),         allocatable :: work
    integer                                                             :: nvar

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(mpi_procid .eq. mpi_masternode) then

       ! Define local variables

       call fileio_interface_varinfo(tcmpi,varinfo)
       filename = trim(adjustl(datapath))//'tcmpi.'//analdate//'.nc'
       call netcdf_interface_open(filename,.false.,.false.,.true.)
       attrname = 'valid_time'
       call netcdf_interface_putattr(attrname,tcmpi%valid_time)
       call netcdf_interface_writedef(varinfo)
       call netcdf_interface_close()
       
       ! Allocate memory for local variables

       if(.not. allocated(work)) allocate(work(7,tcmpi%nx,tcmpi%ny,1))

       ! Define local variables

       work(1,:,:,1) = reshape(tcmpi%lons,(/tcmpi%nx,tcmpi%ny/))
       work(2,:,:,1) = reshape(tcmpi%lats,(/tcmpi%nx,tcmpi%ny/))
       work(3,:,:,1) = reshape(tcmpi%vmax,(/tcmpi%nx,tcmpi%ny/))
       work(4,:,:,1) = reshape(tcmpi%pmin,(/tcmpi%nx,tcmpi%ny/))
       work(5,:,:,1) = reshape(tcmpi%ifl,(/tcmpi%nx,tcmpi%ny/))
       work(6,:,:,1) = reshape(tcmpi%pmsl,(/tcmpi%nx,tcmpi%ny/))
       work(7,:,:,1) = reshape(tcmpi%sst,(/tcmpi%nx,tcmpi%ny/))
       nvar          = size(work(:,1,1,1))

       ! Loop through local variable

       do i = 1, nvar

          ! Define local variables

          varname = varinfo%varname(i)
          call netcdf_interface_putvar(filename,varname,work(i,:,:,1))

       end do ! do i = 1, nvar

       ! Deallocate memory for local variables

       if(allocated(work)) deallocate(work)

       ! Allocate memory for local variables

       if(.not. allocated(work))                                           &
            & allocate(work(3,tcmpi%nx,tcmpi%ny,tcmpi%nz))

       ! Loop through local variable

       do i = 1, tcmpi%nz

          ! Define local variables

          work(1,:,:,i) = reshape(tcmpi%mxrt(:,i),(/tcmpi%nx,tcmpi%ny/))
          work(2,:,:,i) = reshape(tcmpi%p(:,i),(/tcmpi%nx,tcmpi%ny/))          
          work(3,:,:,i) = reshape(tcmpi%t(:,i),(/tcmpi%nx,tcmpi%ny/))

       end do ! do i = 1, tcmpi%nz

       ! Loop through local variable

       do i = 1, size(work(:,1,1,1))
          
          ! Define local variables

          varname = varinfo%varname(nvar + i)
          call netcdf_interface_putvar(filename,varname,work(i,:,:,:))
          
       end do ! do i = 1, size(work(:,1,1,1))

       ! Define local variables

       nvar = nvar + size(work(:,1,1,1)) + 1

       ! Deallocate memory for local variables

       if(allocated(work)) deallocate(work)

       ! Allocate memory for local variables

       if(.not. allocated(tcid)) allocate(tcid(ntcs))
       if(.not. allocated(work)) allocate(work(2,ntcs,1,1))

       ! Loop through local variable

       do i = 1, ntcs

          ! Define local variables

          tcid(i)       = tcmpi%tcid(i)
          work(1,i,1,1) = tcmpi%tclat(i)
          work(2,i,1,1) = tcmpi%tclon(i)

       end do ! do i = 1, ntcs

       ! Define local variables
       
       varname = varinfo%varname(nvar)
       call netcdf_interface_putvar(filename,varname,tcid)

       ! Loop through local variable

       do i = 1, size(work(:,1,1,1))
          
          ! Define local variables

          varname = varinfo%varname(nvar + i)
          call netcdf_interface_putvar(filename,varname,work(i,:,1,1))
          
       end do ! do i = 1, size(work(:,1,1,1))
       
       ! Deallocate memory for local variables

       if(allocated(tcid)) deallocate(tcid)
       if(allocated(work)) deallocate(work)
       call variable_interface_cleanup_struct(varinfo)

    end if ! if(mpi_procid .eq. mpi_masternode)

    ! Define local variables

    call mpi_interface_waitall()

    !=====================================================================

  end subroutine write_tcmpi

  !=======================================================================

  ! SUBROUTINE:

  ! write_tcmsi.90

  ! DESCRIPTION:

  ! This subroutine writes the appropriate variables contained within
  ! the FORTRAN tcmsi_struct variables to an external netcdf file.

  ! INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable.

  ! * tcmsi; a FORTRAN tcmsi_struct variable.

  !-----------------------------------------------------------------------

  subroutine write_tcmsi(tcv,tcmsi)

    ! Define variables passed to routine

    type(tcmsi_struct)                                                  :: tcmsi
    type(tcv_struct)                                                    :: tcv

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: attrname
    character(len=100)                                                  :: varname
    real(r_kind),               dimension(:,:,:),           allocatable :: work

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

   ! Check local variable and proceed accordingly

    if(mpi_procid .eq. mpi_masternode) then

       ! Define local variables

       call fileio_interface_varinfo(tcmsi,varinfo)
       filename = trim(adjustl(datapath))//'tcmsi.'//tcv%tcid//'.'//        &
            & analdate//'.nc'
       call netcdf_interface_open(filename,.false.,.false.,.true.)
       attrname = 'maxwn'
       call netcdf_interface_putattr(attrname,tcmsi%nh)
       attrname = 'ike_hur_TJ'
       call netcdf_interface_putattr(attrname,tcmsi%ike_hur)
       attrname = 'ike_ts_TJ'
       call netcdf_interface_putattr(attrname,tcmsi%ike_ts)
       attrname = 'r34kt_m'
       call netcdf_interface_putattr(attrname,tcmsi%r34kt)
       attrname = 'r50kt_m'
       call netcdf_interface_putattr(attrname,tcmsi%r50kt)
       attrname = 'r64kt_m'
       call netcdf_interface_putattr(attrname,tcmsi%r64kt)       
       attrname = 'rmw_m'
       call netcdf_interface_putattr(attrname,tcmsi%rmw)
       attrname = 'roci_m'
       call netcdf_interface_putattr(attrname,tcmsi%roci)
       attrname = 'vmax_mps'
       call netcdf_interface_putattr(attrname,tcmsi%vmax)
       attrname = 'wn0_vmax_mps'
       call netcdf_interface_putattr(attrname,tcmsi%vwn0)
       attrname = 'wn1_vmax_mps'
       call netcdf_interface_putattr(attrname,tcmsi%vwn1)
       attrname = 'wn0p1_vmax_mps'
       call netcdf_interface_putattr(attrname,tcmsi%vwn0pwn1)
       attrname = 'eps_vmax_mps'
       call netcdf_interface_putattr(attrname,tcmsi%veps)
       attrname = 'reflat_deg'
       call netcdf_interface_putattr(attrname,tcv%reflat)
       attrname = 'reflon_deg'
       call netcdf_interface_putattr(attrname,tcv%reflon)
       attrname = 'tcid'
       call netcdf_interface_putattr(attrname,tcv%tcid)
       attrname = 'valid_time'
       call netcdf_interface_putattr(attrname,tcv%valid_time)
       call netcdf_interface_writedef(varinfo)
       call netcdf_interface_close()

       ! Allocate memory for local variables

       if(.not. allocated(work))                                            &
            & allocate(work(varinfo%nvars,tcmsi%nx,tcmsi%ny))

       ! Define local variables

       work(1,:,:) = tcmsi%lons
       work(2,:,:) = tcmsi%lats
       work(3,:,:) = tcmsi%radius
       work(4,:,:) = tcmsi%w10m
       
       ! Loop through local variable

       do i = 5, varinfo%nvars

          ! Define local variables

          work(i,:,:) = tcmsi%wndvar(i-4,:,:)

       end do ! do i = 5, varinfo%nvars

       ! Loop through local variable

       do i = 1, varinfo%nvars

          ! Define local variables

          varname = varinfo%varname(i)
          call netcdf_interface_putvar(filename,varname,work(i,:,:))

       end do ! do i = 1, varinfo%nvars

       ! Deallocate memory for local variables

       if(allocated(work)) deallocate(work)
       call variable_interface_cleanup_struct(varinfo)

    end if ! if(mpi_procid .eq. mpi_masternode)

    ! Define local variables

    call mpi_interface_waitall()

    !=====================================================================

  end subroutine write_tcmsi

  !=======================================================================

end module fileio_interface
