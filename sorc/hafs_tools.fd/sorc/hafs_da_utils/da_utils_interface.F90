module da_utils_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! da-utils :: da_utils_interface
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

  use diagnostics_interface
  use fileio_interface
  use gsidiags_interface
  use kinds_interface
  use namelist_interface
  use nemsio_module
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: da_utils

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! da_utils.f90

  ! DESCRIPTION:

  ! This subroutine is the driver-level routine for all
  ! data-assimilation utilities.

  !-----------------------------------------------------------------------

  subroutine da_utils()

    ! Define variables computed within routine
  
    real(r_kind)                                                        :: time_start

    !=====================================================================

    ! Define local variables

    call diagnostics_time_start(time_start)
    call namelist()
    if(is_bufrinfo)     call bufrinfo_da_utils()
    if(is_gsiconvdiags) call gsiconvdiags_da_utils()
    if(is_nems2ncdf)    call nems2netcdf()
    call diagnostics_time_stop(time_start)

    !=====================================================================

  end subroutine da_utils

 !=======================================================================

  ! SUBROUTINE:

  ! bufrinfo_da_utils.f90

  ! DESCRIPTION:

  ! This subroutine is the driver routine to produce netcdf files
  ! containing diagnostics for BUFR-formatted observations.

  !-----------------------------------------------------------------------

  subroutine bufrinfo_da_utils()

    !=====================================================================

    ! Compute local variables

    if(is_convobs) call conv_bufrinfo_da_utils()
    if(is_satobs)  call sat_bufrinfo_da_utils()

    !=====================================================================

  end subroutine bufrinfo_da_utils

  !=======================================================================

  ! SUBROUTINE:

  ! conv_bufrinfo_da_utils.f90

  ! DESCRIPTION:

  ! This subroutine is the driver routine to produce netcdf files for
  ! each user-specified conventional observation type within a
  ! user-specified conventional observation (e.g., PREPBUFR)
  ! BUFR-formatted file.

  !-----------------------------------------------------------------------

  subroutine conv_bufrinfo_da_utils()

    ! Define variables computed within routine

    type(convbufr_struct)                                               :: bufr
    integer                                                             :: obstype
    integer                                                             :: nobstypes

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    nobstypes = count(bufr_obstypes .ne. -999)

    ! Loop through local variable

    do i = 1, nobstypes

       ! Define local variables

       obstype = bufr_obstypes(i)
       call fileio_interface_read(bufr_filename,obstype,bufr)
       call fileio_interface_write(bufr)

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(bufr)

    end do ! do i = 1, nobstypes

    !=====================================================================

  end subroutine conv_bufrinfo_da_utils

  !=======================================================================

  ! SUBROUTINE:

  ! gsiconvdiags_da_utils.f90

  ! DESCRIPTION:

  ! This subroutine is the driver routine to compute and produce
  ! netcdf files in the user specified data path which contain all
  ! observations within the respective GSI diagnositic file(s) and
  ! various statistical diagnostics.

  !-----------------------------------------------------------------------

  subroutine gsiconvdiags_da_utils()

    ! Define variables computed within routine

    type(gsidiagisovar_struct)                                          :: gsidiagisovar
    type(gsidiagprfvar_struct)                                          :: gsidiagprfvar
    character(len=500),         dimension(:),               allocatable :: filenames
    integer                                                             :: nobscodes

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    call fileio_interface_read(gsidiags_convobs_listname,filenames)
    gsidiagprfvar%nlevs = count(gsidiags_convobs_plevs .ne. -999)
    nobscodes           = count(gsidiags_obscodes .ne. obscode_spval)

    ! Check local variable and proceed accordingly

    if(nobscodes .eq. 0) then
    
       ! Loop through local variable

       do i = 1, size(filenames)

          ! Define local variables

          gsidiagisovar%gsidiag_filename = filenames(i)
          gsidiagisovar%obstype          = 'ps'
          gsidiagisovar%obscode          = obscode_spval
          call gsidiags_convobs(filenames(i),gsidiagisovar)
          call fileio_interface_write(gsidiagisovar)

          ! Deallocate memory for local variables

          call variable_interface_cleanup_struct(gsidiagisovar)

          ! Define local variables

          gsidiagprfvar%gsidiag_filename = filenames(i)
          gsidiagprfvar%obstype          = 'q'
          gsidiagprfvar%obscode          = obscode_spval
          call gsidiags_convobs(filenames(i),gsidiagprfvar)
          call fileio_interface_write(gsidiagprfvar)

          ! Deallocate memory for local variables

          call variable_interface_cleanup_struct(gsidiagprfvar)

          ! Define local variables

          gsidiagprfvar%gsidiag_filename = filenames(i)
          gsidiagprfvar%obstype          = 't'
          gsidiagprfvar%obscode          = obscode_spval
          call gsidiags_convobs(filenames(i),gsidiagprfvar)
          call fileio_interface_write(gsidiagprfvar)

          ! Deallocate memory for local variables
          
          call variable_interface_cleanup_struct(gsidiagprfvar)
          
          ! Define local variables
          
          gsidiagprfvar%gsidiag_filename = filenames(i)
          gsidiagprfvar%obstype          = 'u'
          gsidiagprfvar%obscode          = obscode_spval
          call gsidiags_convobs(filenames(i),gsidiagprfvar)
          call fileio_interface_write(gsidiagprfvar)
          
          ! Deallocate memory for local variables
          
          call variable_interface_cleanup_struct(gsidiagprfvar)
          
          ! Define local variables
          
          gsidiagprfvar%gsidiag_filename = filenames(i)
          gsidiagprfvar%obstype          = 'v'
          gsidiagprfvar%obscode          = obscode_spval
          call gsidiags_convobs(filenames(i),gsidiagprfvar)
          call fileio_interface_write(gsidiagprfvar)

          ! Deallocate memory for local variables
          
          call variable_interface_cleanup_struct(gsidiagprfvar)

          ! Define local variables
          
          gsidiagprfvar%gsidiag_filename = filenames(i)
          gsidiagprfvar%obstype          = 'rw'
          gsidiagprfvar%obscode          = obscode_spval
          call gsidiags_convobs(filenames(i),gsidiagprfvar)
          call fileio_interface_write(gsidiagprfvar)

          ! Deallocate memory for local variables
          
          call variable_interface_cleanup_struct(gsidiagprfvar)

          ! Define local variables
          
          gsidiagprfvar%gsidiag_filename = filenames(i)
          gsidiagprfvar%obstype          = 'spd'
          gsidiagprfvar%obscode          = obscode_spval
          call gsidiags_convobs(filenames(i),gsidiagprfvar)
          call fileio_interface_write(gsidiagprfvar)

          ! Deallocate memory for local variables
          
          call variable_interface_cleanup_struct(gsidiagprfvar)

          ! Define local variables
          
          gsidiagprfvar%gsidiag_filename = filenames(i)
          gsidiagprfvar%obstype          = 'dw'
          gsidiagprfvar%obscode          = obscode_spval
          call gsidiags_convobs(filenames(i),gsidiagprfvar)
          call fileio_interface_write(gsidiagprfvar)

          ! Deallocate memory for local variables
          
          call variable_interface_cleanup_struct(gsidiagprfvar)
          
       end do ! do i = 1, size(filenames)

    end if ! if(nobscodes .eq. 0)

    ! Check local variable and proceed accordingly

    if(nobscodes .ne. 0) then

       ! Loop through local variable
       
       do j = 1, nobscodes
          
          ! Loop through local variable
          
          do i = 1, size(filenames)
             
             ! Define local variables
             
             gsidiagisovar%gsidiag_filename = filenames(i)
             gsidiagisovar%obstype          = 'ps'
             gsidiagisovar%obscode          = gsidiags_obscodes(j)
             call gsidiags_convobs(filenames(i),gsidiagisovar)
             call fileio_interface_write(gsidiagisovar)
             
             ! Deallocate memory for local variables
             
             call variable_interface_cleanup_struct(gsidiagisovar)
             
             ! Define local variables
             
             gsidiagprfvar%gsidiag_filename = filenames(i)
             gsidiagprfvar%obstype          = 'q'
             gsidiagprfvar%obscode          = gsidiags_obscodes(j)
             call gsidiags_convobs(filenames(i),gsidiagprfvar)
             call fileio_interface_write(gsidiagprfvar)
             
             ! Deallocate memory for local variables
             
             call variable_interface_cleanup_struct(gsidiagprfvar)
             
             ! Define local variables
             
             gsidiagprfvar%gsidiag_filename = filenames(i)
             gsidiagprfvar%obstype          = 't'
             gsidiagprfvar%obscode          = gsidiags_obscodes(j)
             call gsidiags_convobs(filenames(i),gsidiagprfvar)
             call fileio_interface_write(gsidiagprfvar)
             
             ! Deallocate memory for local variables
             
             call variable_interface_cleanup_struct(gsidiagprfvar)
             
             ! Define local variables
             
             gsidiagprfvar%gsidiag_filename = filenames(i)
             gsidiagprfvar%obstype          = 'u'
             gsidiagprfvar%obscode          = gsidiags_obscodes(j)
             call gsidiags_convobs(filenames(i),gsidiagprfvar)
             call fileio_interface_write(gsidiagprfvar)
             
             ! Deallocate memory for local variables
             
             call variable_interface_cleanup_struct(gsidiagprfvar)
             
             ! Define local variables
             
             gsidiagprfvar%gsidiag_filename = filenames(i)
             gsidiagprfvar%obstype          = 'v'
             gsidiagprfvar%obscode          = gsidiags_obscodes(j)
             call gsidiags_convobs(filenames(i),gsidiagprfvar)
             call fileio_interface_write(gsidiagprfvar)
             
             ! Deallocate memory for local variables
             
             call variable_interface_cleanup_struct(gsidiagprfvar)

             ! Define local variables
             
             gsidiagprfvar%gsidiag_filename = filenames(i)
             gsidiagprfvar%obstype          = 'rw'
             gsidiagprfvar%obscode          = gsidiags_obscodes(j)
             call gsidiags_convobs(filenames(i),gsidiagprfvar)
             call fileio_interface_write(gsidiagprfvar)
             
             ! Deallocate memory for local variables
             
             call variable_interface_cleanup_struct(gsidiagprfvar)

             ! Define local variables
             
             gsidiagprfvar%gsidiag_filename = filenames(i)
             gsidiagprfvar%obstype          = 'spd'
             gsidiagprfvar%obscode          = gsidiags_obscodes(j)
             call gsidiags_convobs(filenames(i),gsidiagprfvar)
             call fileio_interface_write(gsidiagprfvar)
             
             ! Deallocate memory for local variables
             
             call variable_interface_cleanup_struct(gsidiagprfvar)

             ! Define local variables
             
             gsidiagprfvar%gsidiag_filename = filenames(i)
             gsidiagprfvar%obstype          = 'dw'
             gsidiagprfvar%obscode          = gsidiags_obscodes(j)
             call gsidiags_convobs(filenames(i),gsidiagprfvar)
             call fileio_interface_write(gsidiagprfvar)
             
             ! Deallocate memory for local variables
             
             call variable_interface_cleanup_struct(gsidiagprfvar)             
             
          end do ! do i = 1, size(filenames)
          
       end do ! do j = 1, nobscodes

    end if ! if(nobscodes .ne. 0)
       
    ! Deallocate memory for local variables

    if(allocated(filenames)) deallocate(filenames)

    !=====================================================================

  end subroutine gsiconvdiags_da_utils

  !=======================================================================

  ! SUBROUTINE:

  ! nems2netcdf.f90

  ! DESCRIPTION:

  ! This subroutine is the driver routine to produce netcdf files for
  ! user specified NEMS-formatted variables; regardless of the user
  ! Vtable requests, the geographical grid (latitude and longitude),
  ! the surface orography, and the model mid-layer and interface
  ! pressure values are written to the netcdf-formatted file specified
  ! by the user; all units within file are assumed MKS.

  !-----------------------------------------------------------------------

  subroutine nems2netcdf()

    ! Define variables computed within routine

    type(json_nems_struct),     dimension(:),               allocatable :: json
    type(nems_struct)                                                   :: nemsio
    character(nemsio_charkind)                                          :: varname

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call fileio_interface_read(nems_filename,nemsio)
    call fileio_interface_read(nems2ncdf_json_vtable,json)
    call fileio_interface_write(nemsio,json)
    
    ! Deallocate memory for local variables

    if(allocated(json)) deallocate(json)
    call variable_interface_cleanup_struct(nemsio)

    !=====================================================================

  end subroutine nems2netcdf

  !=======================================================================

  ! SUBROUTINE:

  ! sat_bufrinfo_da_utils.f90

  ! DESCRIPTION:

  ! This subroutine is the driver routine to produce netcdf files for
  ! each user-specified satellite observations file.

  !-----------------------------------------------------------------------

  subroutine sat_bufrinfo_da_utils()

    ! Define variables computed within routine

    type(satbufr_struct)                                                :: bufr
    character(len=500),         dimension(:),               allocatable :: filenames

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call fileio_interface_read(satbufr_listname,filenames)

    ! Loop through local variable

    do i = 1, size(filenames)

       ! Define local variables

       call fileio_interface_read(filenames(i),bufr)
       call fileio_interface_write(bufr)

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(bufr)

    end do ! do i = 1, size(filenames)

    !=====================================================================

  end subroutine sat_bufrinfo_da_utils

  !=======================================================================

end module da_utils_interface
