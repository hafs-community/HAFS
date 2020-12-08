module fileio_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! tempdrop-sonde :: fileio_interface
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

  ! Review the README, within the top-level directory, which provides
  ! relevant instructions and (any) references cited by algorithms
  ! within this software suite.

  !=======================================================================

  ! Define associated modules and subroutines

  use kinds_interface
  use netcdf
  use netcdf_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: fileio_interface_read
  public :: fileio_interface_write
  public :: nfiles
  public :: sonde_filename
  interface fileio_interface_read
     module procedure read_sonde_filenames
  end interface fileio_interface_read
  interface fileio_interface_varinfo
     module procedure varinfo_meteo
  end interface fileio_interface_varinfo
  interface fileio_interface_write
     module procedure write_meteo_output
  end interface fileio_interface_write

  ! Define local variables

  character(len=500),           dimension(:),               allocatable :: sonde_filename
  integer                                                               :: nfiles

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! read_sonde_filenames.f90

  ! DESCRIPTION:

  ! This subroutine defines the global variables 'nfiles' and
  ! 'sonde_filename' array.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string containing the list of
  !   TEMPDROP sonde filename pathes.

  !-----------------------------------------------------------------------

  subroutine read_sonde_filenames(filename)

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

    if(.not. allocated(sonde_filename)) allocate(sonde_filename(nfiles))

    ! Define local variables

    open(99,file=trim(adjustl(filename)),form='formatted')

    ! Loop through local variable

    do i = 1, nfiles

       ! Define local variables

       read(99,*) sonde_filename(i)

    end do ! do i = 1, nfiles

    ! Define local variables

    close(99)

    !=====================================================================

  end subroutine read_sonde_filenames

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_meteo.f90

  ! DESCRIPTION:

  ! This subroutine defines the FORTRAN varinfo_struct variable used
  ! to write the appropriate variables contained within the FORTRAN
  ! meteo_struct variable to external file formats.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable.

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_meteo(grid,varinfo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: grid
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
    dimval(1)  = grid%nz

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

  end subroutine varinfo_meteo

  !=======================================================================

  ! SUBROUTINE:

  ! write_meteo_output.f90

  ! DESCRIPTION:

  ! This subroutine writes the contents of the FORTRAN meteo_struct
  ! variable to an external Network Common Data Format (netCDF) file.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable.

  ! * filename; a FORTRAN character string containing the full-path to
  !   the netCDF file to be created.

  !-----------------------------------------------------------------------

  subroutine write_meteo_output(grid,filename)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: grid
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    real(r_double),             dimension(:,:),             allocatable :: work
    real(r_kind)                                                        :: time_start
    integer                                                             :: n1dv
    integer                                                             :: n0dv

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call fileio_interface_varinfo(grid,varinfo)
    call netcdf_interface_open(filename,.false.,.false.,.true.)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'aircraft_id',grid%acid)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'obs_number',grid%obnum)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'tempdrop_file',          &
         & trim(adjustl(grid%tempdrop_name)))
    ncstatus = nf90_put_att(ncfileid,nf90_global,'_FillValue',spval)
    call netcdf_interface_writedef(varinfo)
    n1dv = 13
    n0dv = 1

    ! Allocate memory for local variables

    if(.not. allocated(work)) allocate(work(n1dv,grid%nz))

    ! Define local variables

    work(1,:)  = grid%dwpt
    work(2,:)  = grid%lat
    work(3,:)  = grid%lon
    work(4,:)  = grid%p
    work(5,:)  = grid%q
    work(6,:)  = grid%rh
    work(7,:)  = grid%t
    work(8,:)  = grid%u
    work(9,:)  = grid%v
    work(10,:) = grid%wdir
    work(11,:) = grid%wspd
    work(12,:) = grid%z
    work(13,:) = grid%jdate

    ! Loop through local variable

    do i = 1, n1dv

       ! Define local variables
       
       ncstatus = nf90_put_var(ncfileid,varinfo%varid(i),work(i,:))
       
    end do ! do i = 1, n1dv

    ! Deallocate memory for local variables
    
    if(allocated(work)) deallocate(work)

    ! Allocate memory for local variables

    if(.not. allocated(work)) allocate(work(n0dv,1))  
    
    ! Define local variables

    work(1,1) = grid%psfc

    ! Loop through local variable

    do i = 1, n0dv

       ! Define local variables
       
       ncstatus = nf90_put_var(ncfileid,varinfo%varid(n1dv+i),work(i,1))
       
    end do ! do i = 1, n0dv

    ! Deallocate memory for local variables
    
    if(allocated(work)) deallocate(work)
    call variable_interface_cleanup_struct(varinfo)

    ! Define local variables

    call netcdf_interface_close()

    !=====================================================================

  end subroutine write_meteo_output

  !=======================================================================

end module fileio_interface
