module grib_api_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: grib_api_interface
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
  ! along with this program. If not, see
  ! <http://www.gnu.org/licenses/>.

  !=======================================================================

  ! Define associated modules and subroutines

  use grib_api
  use kinds_interface
  use math_methods_interface
  use namelist_interface
  use time_methods_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: grib_api_interface_read

  ! Define local variables

  character(len=500)                                                    :: shortName
  character(len=14)                                                     :: datestr
  real(r_kind)                                                          :: dlat
  real(r_kind)                                                          :: dlon
  integer                                                               :: endStep
  integer                                                               :: grib_ifile
  integer                                                               :: gridtype
  integer                                                               :: igrib
  integer                                                               :: indicatorOfTypeOfLevel
  integer                                                               :: indicatorOfParameter
  integer                                                               :: iret
  integer                                                               :: level
  integer                                                               :: missingValue
  integer                                                               :: startStep
  integer                                                               :: validityDate
  integer                                                               :: validityTime

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! check_api_info.f90

  ! DESCRIPTION:

  ! This subroutine compares the current GRIB record (e.g., igrib) API
  ! values to those of the GRIB record variable specified by the user
  ! (i.e., the FORTRAN grib_struct variable) and returns a FORTRAN
  ! logical variable specifying whether the API values match.

  ! INPUT VARIABLES:

  ! * grib; a FORTRAN grib_struct variable.

  ! * api_match; a FORTRAN logical variable.

  ! OUTPUT VARIABLES:

  ! * api_match; a FORTRAN logical variable indicating whether the
  !   GRIB API keys within the user specified FORTRAN grib_struct
  !   match those of the respective GRIB record.

  !-----------------------------------------------------------------------

  subroutine check_api_info(grib,api_match)

    ! Define variables passed to routine

    type(grib_struct)                                                   :: grib
    logical                                                             :: api_match

    ! Define variables computed within routine

    type(grib_struct)                                                   :: grib_in

    !=====================================================================

    ! Define local variables

    api_match = .false.
    call read_api_info(grib_in)

    ! Check local variable and proceed accordingly

    if((trim(adjustl(shortName)) .eq. trim(adjustl(grib%shortName)))       &
         & .and. (indicatorOfTypeOfLevel .eq.                              &
         & grib%indicatorOfTypeOfLevel) .and. (indicatorOfParameter        &
         & .eq. grib%indicatorOfParameter) .and. (grib%date .eq.           &
         & grib_in%date)) then

       ! Define local variables

       api_match = .true.

    end if ! if((trim(adjustl(shortName))
           ! .eq. trim(adjustl(grib%shortName)))
           ! .and. (indicatorOfTypeOfLevel
           ! .eq. grib%indicatorOfTypeOfLevel)
           ! .and. (indicatorOfParameter
           ! .eq. grib%indicatorOfParameter) .and. (grib%date
           ! .eq. grib_in%date))

    !=====================================================================

  end subroutine check_api_info

  !=======================================================================

  ! SUBROUTINE:

  ! get_grib_dims.f90

  ! DESCRIPTION:

  ! This subroutine determines the maximum array dimensions for the
  ! GRIB records within the user specified file.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the WMO GRIB formatted file.

  ! * grib; a FORTRAN grib_struct variable.

  ! OUTPUT VARIABLES:

  ! * grib; a FORTRAN grib_struct variable containing the maximum
  !   array dimension for the GRIB records within the user specified
  !   file via the attributes (nx, ny, and nz).

  !-----------------------------------------------------------------------

  subroutine get_grib_dims(filename,grib)
  
    ! Define variables passed to routine

    type(grib_struct)                                                   :: grib
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    integer                                                             :: nx
    integer                                                             :: ny
    integer                                                             :: nz
    
    !=====================================================================
    
    ! Define local variables

    nx      = 0
    ny      = 0
    nz      = 0
    grib%nx = 0
    grib%ny = 0
    grib%nz = 1
    call grib_interface_open(filename)
    call grib_new_from_file(grib_ifile,igrib,iret)

    ! Loop through local variable

    do while(iret /= GRIB_END_OF_FILE)

       ! Define local variables

       call read_api_info(grib)

       ! Check local variable and proceed accordingly

       if((trim(adjustl(shortName)) .eq. trim(adjustl(grib%shortName)))    &
            & .and. (indicatorOfTypeOfLevel .eq.                           &
            & grib%indicatorOfTypeOfLevel) .and. (indicatorOfParameter     &
            & .eq. grib%indicatorOfParameter) .and. (grib%date .eq.        &
            & analdate)) then

          ! Define local variables

          call grib_get(igrib,'Ni',nx)
          grib%nx = max(grib%nx,nx)
          call grib_get(igrib,'Nj',ny)
          grib%ny = max(grib%ny,ny)
          nz      = nz + 1

       end if ! if((trim(adjustl(shortName))
              ! .eq. trim(adjustl(grib%shortName)))
              ! .and. (indicatorOfTypeOfLevel
              ! .eq. grib%indicatorOfTypeOfLevel)
              ! .and. (indicatorOfParameter
              ! .eq. grib%indicatorOfParameter) .and. (startStep
              ! .eq. grib%startStep) .and. (endStep
              ! .eq. grib%endStep))

       ! Define local variables

       call grib_release(igrib)
       call grib_new_from_file(grib_ifile,igrib,iret)

    end do ! do while(iret /= GRIB_END_OF_FILE)

    ! Define local variables

    call grib_interface_close()
    grib%nz = max(grib%nz,nz)
    
    !=====================================================================

  end subroutine get_grib_dims

  !=======================================================================

  ! SUBROUTINE:

  ! grib_interface_close.f90

  ! DESCRIPTION:

  ! This subroutine closes an open GRIB formatted file assuming the
  ! identifier is equal to the global variable grib_ifile.

  !-----------------------------------------------------------------------

  subroutine grib_interface_close()

    !=====================================================================

    ! Define local variables

    call grib_close_file(grib_ifile)

    !=====================================================================

  end subroutine grib_interface_close

  !=======================================================================

  ! SUBROUTINE:

  ! grib_interface_open.f90

  ! DESCRIPTION:

  ! This subroutine opens a GRIB formatted file for reading. 

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the GRIB file.

  !-----------------------------------------------------------------------

  subroutine grib_interface_open(filename)

    ! Define variables passed to routine

    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=3)                                                    :: io_opt
    
    !=====================================================================

    ! Define local variables

    io_opt = 'r'
    call grib_open_file(grib_ifile,trim(adjustl(filename)),                &
         & trim(adjustl(io_opt)))
    
    !=====================================================================

  end subroutine grib_interface_open

  !=======================================================================

  ! SUBROUTINE:

  ! grib_api_interface_read.f90

  ! DESCRIPTION:

  ! This subroutine defines the variable arrays within the user
  ! specified GRIB structure variable in accordance with the user
  ! specified GRIB API keys.
  
  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the GRIB file.

  ! * grib; a FORTRAN grib_struct variable containing (at minimum) the
  !   respective variable GRIB API keys.

  ! OUTPUT VARIABLES:

  ! * grib; a FORTRAN grib_struct variable containing the respective
  !   variable attributes (e.g., geographical locations and values)
  !   for the user specified variable (via the GRIB API keys)

  !-----------------------------------------------------------------------

  subroutine grib_api_interface_read(filename,grib)

    ! Define variables passed to routine

    type(grib_struct)                                                   :: grib
    character(len=500)                                                  :: filename

    !=====================================================================
    
    ! Define local variables

    if(grib%levtype .eq. 2) call read_surface(filename,grib)
    if(grib%levtype .eq. 3) call read_profile(filename,grib)
    if(gridtype .eq. 0) grib%gridtype = 1 ! Regular lat/lon grid
    grib%dlat = dlat
    grib%dlon = dlon
    
    !=====================================================================

  end subroutine grib_api_interface_read

  !=======================================================================

  ! SUBROUTINE:

  ! read_api_info.f90

  ! DESCRIPTION:

  ! This subroutine defines the GRIB API keys for the current GRIB
  ! record.

  !-----------------------------------------------------------------------

  subroutine read_api_info(grib)

    ! Define variables passed to routine

    type(grib_struct)                                                   :: grib

    !=====================================================================

    ! Define local variables

    call grib_get(igrib,'shortName',shortName)
    call grib_get(igrib,'indicatorOfTypeOfLevel',indicatorOfTypeOfLevel)
    call grib_get(igrib,'indicatorOfParameter',indicatorOfParameter)
    call grib_get(igrib,'startStep',startStep)
    call grib_get(igrib,'endStep',endStep)
    call grib_get(igrib,'validityDate',validityDate)
    call grib_get(igrib,'validityTime',validityTime)
    call grib_get(igrib,'level',level)
    call grib_get(igrib,'dataRepresentationType',gridtype)
    call grib_get(igrib,'jDirectionIncrementInDegrees',dlat)
    call grib_get(igrib,'iDirectionIncrementInDegrees',dlon)
    write(datestr,'(i8.8,i4.4,i2.2)') validityDate, validityTime, 0
    call time_methods_date_format(datestr,grib%date)

    !=====================================================================
    
  end subroutine read_api_info

  !=======================================================================

  ! SUBROUTINE:

  ! read_api_grid.f90

  ! DESCRIPTION:

  ! This subroutine defines the GRIB record specified by the module
  ! local variable igrib; the respective GRIB record latitude,
  ! longtitude, and variable values are returned.

  !-----------------------------------------------------------------------

  subroutine read_api_grid(lat,lon,values)

    ! Define variables passed to routine

    real(r_kind)                                                        :: lat(:)
    real(r_kind)                                                        :: lon(:)
    real(r_kind)                                                        :: values(:)
    
    !=====================================================================

    ! Define local variables

    call grib_get(igrib,'missingValue',missingValue)
    call grib_get_data(igrib,lat,lon,values)
    
    ! Check local variable and proceed accordingly

    where(values .eq. real(missingValue)) values = spval

    !=====================================================================

  end subroutine read_api_grid
    
  !=======================================================================

  ! SUBROUTINE:

  ! read_profile.f90

  ! DESCRIPTION:

  ! This subroutine will read a GRIB file profile variable; the
  ! respective level (i.e., isobaric elevation) will be returned via
  ! the grib_struct variable attribute 'level'.

  ! INPUT VARIABLES:

  ! * grib; a FORTRAN grib_struct variable containing (at minimum) the
  !   respective variable GRIB API keys.

  ! OUTPUT VARIABLES:

  ! * grib; a FORTRAN grib_struct variable containing the respective
  !   variable attributes (e.g., geographical locations and values)
  !   for the user specified variable (via the GRIB API keys)

  !-----------------------------------------------------------------------

  subroutine read_profile(filename,grib)

    ! Define variables passed to routine

    type(grib_struct)                                                   :: grib
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(spline_struct)                                                 :: spline
    real(r_kind),               dimension(:,:),             allocatable :: lat
    real(r_kind),               dimension(:,:),             allocatable :: lon
    real(r_kind),               dimension(:,:),             allocatable :: values
    real(r_kind),               dimension(:),               allocatable :: levs
    integer,                    dimension(:),               allocatable :: idx
    integer                                                             :: nz

    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================
    
    ! Define local variables

    call get_grib_dims(filename,grib)
    call grib_interface_open(filename)
    call grib_new_from_file(grib_ifile,igrib,iret)
    call variable_interface_setup_struct(grib)
    nz       = 0
    spline%n = grib%nz
    call variable_interface_setup_struct(spline)
    
    ! Allocate memory for local variables

    if(.not. allocated(lat))    allocate(lat(grib%ncoords,grib%nz))
    if(.not. allocated(lon))    allocate(lon(grib%ncoords,grib%nz))
    if(.not. allocated(values)) allocate(values(grib%ncoords,grib%nz))
    if(.not. allocated(levs))   allocate(levs(grib%nz))
    if(.not. allocated(idx))    allocate(idx(grib%nz))

    ! Loop through local variable

    do while(iret /= GRIB_END_OF_FILE)

       ! Define local variables

       call read_api_info(grib)

       ! Check local variable and proceed accordingly

       if((trim(adjustl(shortName)) .eq. trim(adjustl(grib%shortName)))    &
            & .and. (indicatorOfTypeOfLevel .eq.                           &
            & grib%indicatorOfTypeOfLevel) .and. (indicatorOfParameter     &
            & .eq. grib%indicatorOfParameter) .and. (grib%date .eq.        &
            & analdate)) then

          ! Define local variables

          nz       = nz + 1
          call read_api_grid(lat(:,nz),lon(:,nz),values(:,nz))
          levs(nz) = real(level)
          idx(nz)  = nz
          
       end if ! if((trim(adjustl(shortName))
              ! .eq. trim(adjustl(grib%shortName)))
              ! .and. (indicatorOfTypeOfLevel
              ! .eq. grib%indicatorOfTypeOfLevel)
              ! .and. (indicatorOfParameter
              ! .eq. grib%indicatorOfParameter) .and. (startStep
              ! .eq. grib%startStep) .and. (endStep
              ! .eq. grib%endStep))

       ! Define local variables

       call grib_release(igrib)
       call grib_new_from_file(grib_ifile,igrib,iret)

    end do ! do while(iret /= GRIB_END_OF_FILE)

    ! Define local variables

    call grib_interface_close()
    spline%xa = levs
    spline%ya = idx
    call math_methods_sort_array(spline,.false.,.true.)
    levs      = spline%xa
    idx       = int(spline%ya)
    
    ! Loop through local variable

    do i = 1, grib%nz

       ! Define local variables

       grib%lat(:,i)    = lat(:,idx(i))
       grib%lon(:,i)    = lon(:,idx(i))
       grib%values(:,i) = values(:,idx(i))
       grib%levs(i)     = levs(i)

    end do ! do i = 1, grib%nz
    
    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(spline)
    if(allocated(lat))    deallocate(lat)
    if(allocated(lon))    deallocate(lon)
    if(allocated(values)) deallocate(values)
    if(allocated(levs))   deallocate(levs)
    if(allocated(idx))    deallocate(idx)

    !=====================================================================
    
  end subroutine read_profile

  !=======================================================================

  ! SUBROUTINE:

  ! read_surface.f90

  ! DESCRIPTION:

  ! This subroutine will read a GRIB file surface variable; the
  ! respective level (i.e., isobaric elevation) will be returned via
  ! the grib_struct variable attribute 'level'.

  ! INPUT VARIABLES:

  ! * grib; a FORTRAN grib_struct variable containing (at minimum) the
  !   respective variable GRIB API keys.

  ! OUTPUT VARIABLES:

  ! * grib; a FORTRAN grib_struct variable containing the respective
  !   variable attributes (e.g., geographical locations and values)
  !   for the user specified variable (via the GRIB API keys)

  !-----------------------------------------------------------------------

  subroutine read_surface(filename,grib)

    ! Define variables passed to routine

    type(grib_struct)                                                   :: grib
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    real(r_kind),               dimension(:),               allocatable :: lat
    real(r_kind),               dimension(:),               allocatable :: lon
    real(r_kind),               dimension(:),               allocatable :: values
    integer                                                             :: nz
    
    !=====================================================================
    
    ! Define local variables

    call get_grib_dims(filename,grib)
    call grib_interface_open(filename)
    call grib_new_from_file(grib_ifile,igrib,iret)
    call variable_interface_setup_struct(grib)
    nz = 0

    ! Allocate memory for local variables

    if(.not. allocated(lat))    allocate(lat(grib%ncoords))
    if(.not. allocated(lon))    allocate(lon(grib%ncoords))
    if(.not. allocated(values)) allocate(values(grib%ncoords))

    ! Loop through local variable

    do while(iret /= GRIB_END_OF_FILE)

       ! Define local variables

       call read_api_info(grib)

       ! Check local variable and proceed accordingly

       if((trim(adjustl(shortName)) .eq. trim(adjustl(grib%shortName)))    &
            & .and. (indicatorOfTypeOfLevel .eq.                           &
            & grib%indicatorOfTypeOfLevel) .and. (indicatorOfParameter     &
            & .eq. grib%indicatorOfParameter) .and. (grib%date .eq.        &
            & analdate)) then

          ! Define local variables

          nz                = nz + 1
          call read_api_grid(lat,lon,values)
          grib%lat(:,nz)    = lat
          grib%lon(:,nz)    = lon
          grib%values(:,nz) = values

       end if ! if((trim(adjustl(shortName))
              ! .eq. trim(adjustl(grib%shortName)))
              ! .and. (indicatorOfTypeOfLevel
              ! .eq. grib%indicatorOfTypeOfLevel)
              ! .and. (indicatorOfParameter
              ! .eq. grib%indicatorOfParameter) .and. (startStep
              ! .eq. grib%startStep) .and. (endStep
              ! .eq. grib%endStep))

       ! Define local variables

       call grib_release(igrib)
       call grib_new_from_file(grib_ifile,igrib,iret)

    end do ! do while(iret /= GRIB_END_OF_FILE)

    ! Define local variables

    call grib_interface_close()

    ! Deallocate memory for local variables

    if(allocated(lat))    deallocate(lat)
    if(allocated(lon))    deallocate(lon)
    if(allocated(values)) deallocate(values)

    !=====================================================================
    
  end subroutine read_surface
  
  !=======================================================================

end module grib_api_interface
