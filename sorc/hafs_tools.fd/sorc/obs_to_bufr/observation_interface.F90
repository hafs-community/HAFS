module observation_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-to-bufr :: observation_interface
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

  ! Review the README, within the top-level directory, which provides
  ! relevant instructions and (any) references cited by algorithms
  ! within this software suite.

  !=======================================================================

  ! Define associated modules and subroutines

  use bufrio_interface
  use fileio_interface
  use interpolation_interface
  use kinds_interface
  use math_methods_interface
  use meteo_methods_interface
  use namelist_interface
  use time_methods_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: observations

  ! Define local variables

  type(topogrid_struct)                                                 :: topogrid
  real(r_double)                                                        :: anljday
  real(r_double)                                                        :: obsjday
  integer                                                               :: yyyy
  integer                                                               :: mm
  integer                                                               :: dd
  integer                                                               :: hh
  integer                                                               :: nn
  integer                                                               :: ss

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! observations.f90

  ! DESCRIPTION:

  ! This subroutine is the driver level routine for all observation
  ! types to be written to BUFR formatted files.

  !-----------------------------------------------------------------------

  subroutine observations()

    !=====================================================================

    ! Define local variables

    call time_methods_date_attributes(analdate,yyyy,mm,dd,hh,nn,ss)
    if(obs_flag) call obs_flag_bufr()
    if(mask_land) call fileio_interface_read(topogrid)

    ! Compute local variables

    call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,anljday)
    call obs_profile()
    call obs_surface()

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(topogrid)

    !=====================================================================

  end subroutine observations

  !=======================================================================

  ! SUBROUTINE:

  ! fcst_model_to_bufr.f90

  ! DESCRIPTION:

  ! This subroutine is the interface to observations generated using
  ! forecast-model variables (see
  ! the references for the source code routines necessary to generate
  ! the appropriate netcdf files).

  ! REFERENCES:

  ! https://github.com/hrwinterbottom/tc-synthetic-obs

  !-----------------------------------------------------------------------

  subroutine fcst_model_to_bufr()

    ! Define variables computed within routine

    type(bufr_struct)                                                   :: bufr
    type(bufr_info_struct)                                              :: bufr_info
    type(fcst_model_struct)                                             :: fcst_model
    type(remap_struct)                                                  :: remap
    type(tcm_struct)                                                    :: grid
    character(len=500)                                                  :: lbufr_filepath
    character(len=8)                                                    :: cacobid
    integer(i_long)                                                     :: racobid

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    equivalence(racobid,cacobid)
    bufr_info%filename = fcst_model_bufr_info_filepath
    call fileio_interface_read(bufr_info)
    bufr%hdstr         = 'SID XOB YOB DHR TYP'
    bufr%obstr         = 'POB ZOB QOB TOB UOB VOB'
    bufr%qcstr         = 'PQM ZQM QQM TQM WQM'
    bufr%oestr         = 'POE ZOE QOE TOE WOE'
    bufr%subset        = trim(adjustl(bufr_info%subset))
    bufr%mxmn          = 6
    bufr%mxlv          = 1
    lbufr_filepath     = trim(adjustl(datapath))//'prepbufr.fcstmdl'
    call bufrio_interface_idate(analdate,bufr)
    call bufrio_interface_open(lbufr_filepath,bufr_tblpath,bufr,.false.,   &
         & .false.,.true.)
    call fileio_interface_read(fcst_model_filepath,fcst_model)
    remap%ncoords      = fcst_model%nobs
    call variable_interface_setup_struct(remap)

    ! Check local variable and proceed accordingly
    
    if(mask_land) then
       
       ! Define local variables
       
       remap%lat = fcst_model%lat
       remap%lon = fcst_model%lon
       
       ! Compute local variables
       
       call obs_land_mask(remap)

    end if ! if(mask_land)

    ! Loop through local variable

    do i = 1, fcst_model%nobs

       ! Check local variable and proceed accordingly

       if((remap%mask(i) .eq. spval) .and. (fcst_model%lmsk(i) .eq. 1.0))  &
            then

          ! Define local variables

          remap%mask(i) = spval

       end if ! if((remap%mask(i) .eq. spval)
              ! .and. (fcst_model%lmsk(i) .eq. 1.0))

       ! Check local variable and proceed accordingly

       if((remap%mask(i) .eq. spval) .and. (fcst_model%lmsk(i) .eq. 0.0))  &
            then

          ! Define local variables

          remap%mask(i) = spval

       end if ! if((remap%mask(i) .eq. spval)
              ! .and. (fcst_model%lmsk(i) .eq. 0.0))

       ! Check local variable and proceed accordingly

       if((remap%mask(i) .ne. spval) .and. (fcst_model%lmsk(i) .eq. 1.0))  &
            then

          ! Define local variables

          remap%mask(i) = spval

       end if ! if((remap%mask(i) .ne. spval)
              ! .and. (fcst_model%lmsk(i) .eq. 1.0))

    end do ! do i = 1, fcst_model%nobs

    ! Loop through local variable

    do i = 1, fcst_model%nobs

       ! Define local variables

       write(cacobid,500) i

       ! Loop through local variable

       do j = 1, fcst_model%nz

          ! Define local variables
          
          call variable_interface_setup_struct(bufr)
          bufr%hdr(1)   = dble(racobid)
          bufr%hdr(2)   = dble(fcst_model%lon(i)) 
          bufr%hdr(3)   = dble(fcst_model%lat(i)) 
          bufr%hdr(4)   = 0.0
          bufr%hdr(5)   = bufr_info%obs_type_mass
          bufr%obs(1,1) = fcst_model%p(i,j)/100.0
          bufr%obs(2,1) = fcst_model%z(i,j)
          bufr%obs(3,1) = fcst_model%q(i,j)*1000.0*1000.0
          bufr%obs(4,1) = (fcst_model%t(i,j) - 273.15)
          bufr%qcf(1,1) = 2.0
          bufr%qcf(2,1) = 2.0
          bufr%qcf(3,1) = 2.0
          bufr%qcf(4,1) = 2.0

          ! Check local variable and proceed accordingly

          if(remap%mask(i) .ne. spval) then

             ! Define local variables
             
             call bufrio_interface_write(bufr)

          end if ! if(remap%mask(i) .ne. spval)

          ! Deallocate memory for local variables

          call variable_interface_cleanup_struct(bufr)

          ! Define local variables
          
          call variable_interface_setup_struct(bufr)
          bufr%hdr(1)   = dble(racobid)
          bufr%hdr(2)   = dble(fcst_model%lon(i))
          bufr%hdr(3)   = dble(fcst_model%lat(i))
          bufr%hdr(4)   = 0.0
          bufr%hdr(5)   = bufr_info%obs_type_wind
          bufr%obs(1,1) = fcst_model%p(i,j)/100.0
          bufr%obs(5,1) = fcst_model%u(i,j)
          bufr%obs(6,1) = fcst_model%v(i,j)
          bufr%qcf(1,1) = 2.0
          bufr%qcf(5,1) = 2.0

          ! Check local variable and proceed accordingly

          if(remap%mask(i) .ne. spval) then

             ! Define local variables
             
             call bufrio_interface_write(bufr)

          end if ! if(remap%mask(i) .ne. spval)

          ! Deallocate memory for local variables

          call variable_interface_cleanup_struct(bufr)

       end do ! do j = 1, fcst_model%nz

    end do ! do i = 1, fcst_model%nobs

    ! Define local variables

    call bufrio_interface_close(.false.,.true.)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(fcst_model)
    call variable_interface_cleanup_struct(remap)

    ! Define local variables

500 format(i5.5,'FSM')

    !=====================================================================

  end subroutine fcst_model_to_bufr

  !=======================================================================

  ! SUBROUTINE:

  ! givtdruv_to_bufr.f90

  ! DESCRIPTION:

  ! This subroutine is the interface to the National Oceanic and
  ! Atmospheric Administration (NOAA) Atlantic Oceanographic and
  ! Meteorological Laboratory (AOML) Hurricane Research Division (HRD)
  ! G-IV Tail-Doppler Radar (TDR) u- and v-wind (netcdf) formatted
  ! files.

  !-----------------------------------------------------------------------

  subroutine givtdruv_to_bufr()

    ! Define variables computed within routine

    type(givtdruv_struct),      dimension(:),               allocatable :: grid
    type(bufr_struct)                                                   :: bufr
    type(bufr_info_struct)                                              :: bufr_info
    character(len=500)                                                  :: lbufr_filepath
    character(len=8)                                                    :: cacobid
    integer(i_long)                                                     :: racobid

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    equivalence(racobid,cacobid)
    bufr_info%filename = givtdruv_bufr_info_filepath
    call fileio_interface_read(bufr_info)
    bufr%hdstr         = 'SID XOB YOB DHR ELV TYP'
    bufr%obstr         = 'POB ZOB UOB VOB'
    bufr%qcstr         = 'PQM ZQM WQM'
    bufr%oestr         = 'POE ZOE WOE'
    bufr%subset        = trim(adjustl(bufr_info%subset))
    bufr%mxmn          = 6
    bufr%mxlv          = 1
    lbufr_filepath     = trim(adjustl(datapath))//'prepbufr.givtdruv'
    call bufrio_interface_idate(analdate,bufr)
    call bufrio_interface_open(lbufr_filepath,bufr_tblpath,bufr,.false.,   &
         & .false.,.true.)
    call fileio_interface_read(givtdruv_obs_filepath,grid)

    ! Loop through local variable

    do i = 1, nfiles

       ! Define local variables

       call fileio_interface_read(obs_filename(i),grid(i))
       call time_methods_date_attributes(grid(i)%timestamp,yyyy,mm,dd,hh,  &
            & nn,ss)

       ! Compute local variables

       call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,obsjday)

       ! Loop through local variable

       do j = 1, grid(i)%ncoords

          ! Define local variables

          call variable_interface_setup_struct(bufr)

          ! Check local variable and proceed accordingly

          if((grid(i)%u(j) .ne. grid(i)%msngvl) .and. (grid(i)%v(j) .ne.   &
               & grid(i)%msngvl)) then

             ! Define local variables

             write(cacobid,500) j
             bufr%hdr(1)   = dble(racobid)
             bufr%hdr(2)   = dble(grid(i)%lon(j) + 360.0)
             bufr%hdr(3)   = dble(grid(i)%lat(j))
             bufr%hdr(4)   = (obsjday - anljday)*dble(24.0)
             bufr%hdr(5)   = dble(0.0)
             bufr%hdr(6)   = bufr_info%obs_type_wind
             bufr%obs(2,1) = dble(grid(i)%level(j)*1000.0)
             bufr%obs(3,1) = dble(grid(i)%u(j))
             bufr%obs(4,1) = dble(grid(i)%v(j))
             bufr%qcf(1,1) = 2.0
             bufr%qcf(2,1) = 2.0
             bufr%qcf(3,1) = 2.0
             call bufrio_interface_write(bufr)

          end if ! if((grid(i)%u(j) .ne. grid(i)%msngvl)
                 ! .and. (grid(i)%v(j) .ne. grid(i)%msngvl))

          ! Deallocate memory for local variables

          call variable_interface_cleanup_struct(bufr)

       end do ! do j = 1, grid%ncoords

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(grid(i))

    end do ! do i = 1, nfiles

    ! Define local variables

    call bufrio_interface_close(.false.,.true.)

    ! Deallocate memory for local variables

    if(allocated(grid))         deallocate(grid)
    if(allocated(obs_filename)) deallocate(obs_filename)

    ! Define local variables

500 format(i6.6,'UV')
501 format(a4,'-',a2,'-',a2,'_',a2,':',a2,':',a2)

    !=====================================================================

  end subroutine givtdruv_to_bufr

  !=======================================================================

  ! SUBROUTINE:

  ! hsa_to_bufr.f90

  ! DESCRIPTION:

  ! This subroutine is the interface to the National Oceanic and
  ! Atmospheric Administration (NOAA) Atlantic Oceanographic and
  ! Meteorological Laboratory (AOML) Hurricane Research Division (HRD)
  ! HRD Spline Analysis (HSA) formatted files; the valid variable
  ! values, for 'SIGL' and 'MANL' type levels are written as
  ! individual records to the respective BUFR file such that the drift
  ! of the sonde (if applicable) can be appropriately accounted for.

  !-----------------------------------------------------------------------

  subroutine hsa_to_bufr()

    ! Define variables computed within routine

    type(bufr_struct)                                                   :: bufr
    type(bufr_info_struct)                                              :: bufr_info
    type(error_struct)                                                  :: error
    type(hsa_struct)                                                    :: grid
    type(meteo_struct)                                                  :: meteo
    character(len=500)                                                  :: filename
    character(len=500)                                                  :: lbufr_filepath
    character(len=8)                                                    :: cacobid
    character(len=8)                                                    :: cdate
    character(len=6)                                                    :: ctime
    character(len=6)                                                    :: acid
    character(len=6)                                                    :: obsn
    real(r_kind)                                                        :: obs_err
    integer(i_long)                                                     :: racobid
    integer                                                             :: strstrt
    integer                                                             :: strstop

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    equivalence(racobid,cacobid)
    bufr_info%filename = hsa_bufr_info_filepath
    call fileio_interface_read(bufr_info)
    bufr%hdstr         = 'SID XOB YOB DHR TYP'
    bufr%obstr         = 'POB QOB TOB UOB VOB'
    bufr%qcstr         = 'PQM QQM TQM WQM'
    bufr%oestr         = 'POE QOE TOE WOE'
    bufr%subset        = trim(adjustl(bufr_info%subset))
    bufr%mxmn          = 5
    bufr%mxlv          = 1
    lbufr_filepath     = trim(adjustl(datapath))//'prepbufr.hsa'
    call bufrio_interface_idate(analdate,bufr)
    call bufrio_interface_open(lbufr_filepath,bufr_tblpath,bufr,.false.,   &
         & .false.,.true.)
    call fileio_interface_read(hsa_obserr_filepath,error)
    call fileio_interface_read(hsa_obs_filepath)

    ! Loop through local variable

    do i = 1, nfiles

       ! Define local variables

       call fileio_interface_read(obs_filename(i),grid)
       strstrt  = index(trim(adjustl(obs_filename(i))),'/',back=.true.)    &
            & + 1
       strstop  = len(trim(adjustl(obs_filename(i))))
       filename = trim(adjustl(obs_filename(i)(strstrt:strstop)))
       acid     = filename(1:4) 
       obsn     = filename(7:8) 
       write(cacobid,500) trim(adjustl(acid)), trim(adjustl(obsn))

       ! Loop through local variable

       do j = 1, grid%nz

          ! Check local variable and proceed accordingly

          if(grid%yymmdd(j) .ne. hsa_spval .and. (grid%tail(j) .eq.        &
               & 'SIGL' .or. grid%tail(j) .eq. 'MANL')) then

             ! Define local variables

             call variable_interface_setup_struct(bufr)
             write(cdate,'(i8)')        (20000000 + (int(grid%yymmdd(j))))
             write(ctime,'(i4.4,i2.2)') grid%gmt(j), 0
             write(bufr%cdate,501) cdate(1:4), cdate(5:6), cdate(7:8),     &
                  & ctime(1:2), ctime(3:4), ctime(5:6)
             call time_methods_date_attributes(bufr%cdate,yyyy,mm,dd,hh,   &
                  & nn,ss)

             ! Compute local variables

             call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,obsjday)

             ! Define local variables

             bufr%hdr(1) = dble(racobid)
             bufr%hdr(2) = dble(-1.0*grid%lon(j) + 360.0)
             bufr%hdr(3) = dble(grid%lat(j))
             bufr%hdr(4) = (obsjday - anljday)*dble(24.0)
             bufr%hdr(5) = bufr_info%obs_type_mass

             ! Check local variable and proceed accordingly

             if(grid%t(j) .ne. hsa_spval) then

                ! Define local variables

                bufr%obs(1,1) = grid%p(j)
                bufr%obs(3,1) = grid%t(j)
                call obs_error(error%nz,error%plev,error%t,grid%p(j),      &
                     & hsa_intrp_obserr,obs_err)
                bufr%oer(3,1) = obs_err
                bufr%qcf(1,1) = 2.0
                bufr%qcf(3,1) = 2.0

                ! Check local variable and proceed accordingly

                if(grid%rh(j) .ne. hsa_spval) then

                   ! Define local variables

                   meteo%p  = dble(grid%p(j)*100.0)
                   meteo%rh = dble(grid%rh(j))
                   meteo%t  = dble(grid%t(j) + 273.15)

                   ! Compute local variables

                   call meteo_methods_spechumd(meteo)

                   ! Define local variables
 
                   bufr%obs(2,1) = real(meteo%q)*1000.0*1000.0
                   call obs_error(error%nz,error%plev,error%q,grid%p(j),   &
                        & hsa_intrp_obserr,obs_err)
                   bufr%oer(2,1) = obs_err
                   bufr%qcf(2,1) = 2.0 

                end if ! if(grid%rh(j) .ne. hsa_spval)

                ! Define local variables

                call bufrio_interface_write(bufr)

             end if ! if(grid%t(j) .ne. hsa_spval)

	     ! Deallocate memory for local variables

	     call variable_interface_cleanup_struct(bufr)

	     ! Define local variables

	     call variable_interface_setup_struct(bufr)
             bufr%hdr(1) = dble(racobid)
             bufr%hdr(2) = dble(-1.0*grid%lon(j) + 360.0)
             bufr%hdr(3) = dble(grid%lat(j))
             bufr%hdr(4) = (obsjday - anljday)*dble(24.0)
             bufr%hdr(5) = bufr_info%obs_type_wind

	     ! Check local variable and proceed accordingly

             if(grid%u(j) .ne. hsa_spval .and. grid%v(j) .ne. hsa_spval)   &
	          & then

                ! Define local variables

                bufr%obs(1,1) = grid%p(j)
                bufr%obs(4,1) = grid%u(j)
                bufr%obs(5,1) = grid%v(j)
                call obs_error(error%nz,error%plev,error%uv,grid%p(j),     &
                     & hsa_intrp_obserr,obs_err)
                bufr%oer(4,1) = obs_err
                bufr%qcf(1,1) = 2.0
                bufr%qcf(4,1) = 2.0
                call bufrio_interface_write(bufr)

             end if ! if(grid%u(j) .ne. hsa_spval .and. grid%v(j) .ne.     
                    ! hsa_spval)

             ! Deallocate memory for local variables
          
	     call variable_interface_cleanup_struct(bufr)

	  end if ! if(grid%yymmdd(j) .ne. hsa_spval .and. (grid%tail(j)    &
                 ! .eq. 'SIGL' .or. grid%tail(j) .eq. 'MANL')) 

       end do ! do j = 1, grid%nz

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(grid)

    end do ! do i = 1, nfiles

    ! Define local variables

    call bufrio_interface_close(.false.,.true.)

    ! Deallocate memory for local variables

    if(allocated(obs_filename)) deallocate(obs_filename)
    call variable_interface_cleanup_struct(error)

    ! Define local variables

500 format(a4,a2,'AH')
501 format(a4,'-',a2,'-',a2,'_',a2,':',a2,':',a2)

    !=====================================================================

  end subroutine hsa_to_bufr

  !=======================================================================

  ! SUBROUTINE:

  ! nhcgtcm_to_bufr.f90

  ! DESCRIPTION:

  ! This subroutine is the interface to observations generated from
  ! the National Hurricane Center (NHC) Gridded Tropical Cyclone Model
  ! (GTCM); see the references section for the applicable routines
  ! required to generate the interface files.

  ! REFERENCES:

  ! https://github.com/hrwinterbottom/tc-synthetic-obs

  !-----------------------------------------------------------------------

  subroutine nhcgtcm_to_bufr()

    ! Define variables computed within routine

    type(bufr_struct)                                                   :: bufr
    type(bufr_info_struct)                                              :: bufr_info
    type(error_struct)                                                  :: error
    type(remap_struct)                                                  :: remap
    type(nhcgtcm_struct)                                                :: grid
    character(len=500)                                                  :: lbufr_filepath
    character(len=8)                                                    :: cacobid
    integer(i_long)                                                     :: racobid

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    equivalence(racobid,cacobid)
    bufr_info%filename = nhcgtcm_bufr_info_filepath
    call fileio_interface_read(bufr_info)
    bufr%hdstr         = 'SID XOB YOB DHR ELV TYP'
    bufr%obstr         = 'POB ZOB UOB VOB'
    bufr%qcstr         = 'PQM ZQM WQM'
    bufr%oestr         = 'POE ZOE WOE'
    bufr%subset        = trim(adjustl(bufr_info%subset))
    bufr%mxmn          = 6
    bufr%mxlv          = 1
    lbufr_filepath     = trim(adjustl(datapath))//'prepbufr.nhcgtcm'
    call bufrio_interface_idate(analdate,bufr)
    call bufrio_interface_open(lbufr_filepath,bufr_tblpath,bufr,.false.,   &
         & .false.,.true.)
    call fileio_interface_read(nhcgtcm_obs_filepath)
    call time_methods_date_attributes(analdate,yyyy,mm,dd,hh,nn,ss)

     ! Compute local variables

    call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,anljday)

    ! Loop through local variable

    do i = 1, nfiles

       ! Define local variables

       call fileio_interface_read(obs_filename(i),grid)
       remap%ncoords = grid%nvals
       call variable_interface_setup_struct(remap)

       ! Check local variable and proceed accordingly

       if(mask_land) then

          ! Define local variables
          
          remap%lat = grid%lat
          remap%lon = grid%lon - 360.0
       
          ! Compute local variables

          call obs_land_mask(remap)

       end if ! if(mask_land)

       ! Loop through local variable

       do j = 1, grid%nvals

          ! Define local variables

          write(cacobid,500) j
          call variable_interface_setup_struct(bufr)

          ! Define local variables

          call time_methods_date_attributes(grid%analysis_date,yyyy,mm,    &
               & dd,hh,nn,ss)
          
          ! Compute local variables
          
          call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,obsjday)
          
          ! Define local variables
          
          bufr%hdr(1)   = dble(racobid)
          bufr%hdr(2)   = dble(grid%lon(j))
          bufr%hdr(3)   = dble(grid%lat(j))
          bufr%hdr(4)   = (obsjday - anljday)*dble(24.0)
          bufr%hdr(5)   = remap%hgt(j)
          bufr%hdr(6)   = bufr_info%obs_type_wind
          bufr%obs(2,1) = remap%hgt(j) + 10.0
          bufr%obs(3,1) = grid%u10m(j)
          bufr%obs(4,1) = grid%v10m(j)
          bufr%qcf(1,1) = 2.0
          bufr%qcf(2,1) = 2.0
          bufr%qcf(3,1) = 2.0
          bufr%oer(2,1) = 10.0
          bufr%oer(3,1) = 10.0
          call bufrio_interface_write(bufr)

          ! Deallocate memory for local variables

          call variable_interface_cleanup_struct(bufr)

       end do ! do j = 1, grid%nvals

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(grid)

    end do ! do i = 1, nfiles

    ! Define local variables

    call bufrio_interface_close(.false.,.true.)

    ! Deallocate memory for local variables

    if(allocated(obs_filename)) deallocate(obs_filename)
    call variable_interface_cleanup_struct(error)

    ! Define local variables

500 format(i6.6,'TC')
501 format(a4,'-',a2,'-',a2,'_',a2,':',a2,':',a2)

    !=====================================================================

  end subroutine nhcgtcm_to_bufr

  !=======================================================================

  ! SUBROUTINE:

  ! obs_error.f90

  ! DESCRIPTION:

  ! This subroutine determines the observation error for a given
  ! observation provided the observation isobaric level.

  ! INPUT VARIABLES:

  ! * nz; a FORTRAN integer specifying the total number of levels in
  !   the observation error profile.

  ! * err_plev; a FORTRAN 4-byte real value array, of dimension nz,
  !   containing the isobaric levels for the observation error
  !   profile.

  ! * err_var; a FORTRAN 4-byte real value array, of dimension nz,
  !   containing the respective observation-type variable observation
  !   error profile.

  ! * obs_plev; a FORTRAN 4-byte real value specifying the observation
  !   isobaric level.

  ! * interp; a FORTRAN boolean value specifying whether to use spline
  !   interpolation or nearest-neighbor methods to define the
  !   observation error.

  ! * obs_err; a FORTRAN 4-byte real value to contain the observation
  !   error.

  ! OUTPUT VARIABLES:

  ! * obs_err; a FORTRAN 4-byte real value specifying the observation
  !   variable in accordance with the user instructions (e.g.,
  !   interp).

  !-----------------------------------------------------------------------

  subroutine obs_error(nz,err_plev,err_var,obs_plev,interp,obs_err)

    ! Define variables passed to subroutine

    logical                                                             :: interp
    integer                                                             :: nz
    real(r_kind),               dimension(nz)                           :: err_plev
    real(r_kind),               dimension(nz)                           :: err_var
    real(r_kind)                                                        :: obs_plev
    real(r_kind)                                                        :: obs_err

    ! Define variables computed within routine

    type(spline_struct)                                                 :: spline

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables
    
    spline%n  = nz
    call variable_interface_setup_struct(spline)
    spline%xa = err_plev
    spline%ya = err_var

    ! Check local variable and proceed accordingly

    if(interp) then

       ! Define local variables

       spline%x = obs_plev

       ! Compute local variables

       call math_methods_spline_interp(spline)

       ! Define local variables

       obs_err = spline%y

    end if ! if(interp)

    ! Check local variable and proceed accordingly

    if(.not. interp) then

       ! Define local variables
       
       call math_methods_sort_array(spline,.true.,.false.)

       ! Loop through local variable

       do i = 1, (spline%n - 1)

          ! Check local variable and proceed accordingly

          if(obs_plev .gt. spline%xa(i) .and. obs_plev .gt. obs_plev .le.  &
               & spline%xa(i+1)) then

             ! Define local variables

             obs_err = spline%ya(i+1)

          end if ! if(obs_plev .gt. spline%xa(i) .and. obs_plev
                 ! .gt. obs_plev .le. spline%xa(i+1)

       end do ! do i = 1, (spline%n - 1)

    end if ! if(.not. interp)
    
    ! Deallocate memory for local variables
    
    call variable_interface_cleanup_struct(spline)

    !=====================================================================

  end subroutine obs_error

  !=======================================================================

  ! SUBROUTINE:

  ! obs_flag_bufr.f90

  ! DESCRIPTION:

  ! This subroutine is the driver layer interface to
  ! update/change/flag user specified BUFR messages.

  !-----------------------------------------------------------------------

  subroutine obs_flag_bufr()

    ! Define variables computed within routine

    type(obs_flag_struct)                                               :: obs_flag

    !=====================================================================

    ! Define local variables

    obs_flag%filename = obs_flag_json_vtable
    call fileio_interface_read(obs_flag)
    call bufrio_interface_flag(bufr_filepath,obs_flag)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(obs_flag)

    !=====================================================================

  end subroutine obs_flag_bufr

  !=======================================================================

  ! SUBROUTINE:

  ! obs_land_mask.f90

  ! DESCRIPTION:

  ! This subroutine defines a land-mask using nearest-neighbor
  ! interpolation coefficients between the user specified topography
  ! grid and the user specified observation grid; all ocean points
  ! have a value of 1.0 while the land points are denoted by the
  ! missing value (e.g., spval) attribute of this routine.

  ! INPUT VARIABLES:

  ! * remap; a FORTRAN remap_struct variable; it is assumed that all
  !   arrays have been allocated and the geographical (e.g., latitude
  !   and longitude) coordinates have been assigned from the
  !   observation grid.

  ! OUTPUT VARIABLES:

  ! * remap; a FORTRAN remap_struct variable containing the masked
  !   land (e.g., where topography values are non-zero) grid cells;
  !   non-land points (i.e., ocean) are denoted by a value of 1.0;
  !   land points are assigned the missing value (e.g., spval)
  !   attribute of this routine.

  !-----------------------------------------------------------------------

  subroutine obs_land_mask(remap)

    ! Define variables passed to routine

    type(remap_struct)                                                  :: remap

    ! Define variables computed within routine

    type(grid_struct)                                                   :: dst_grid
    type(grid_struct)                                                   :: src_grid
    type(slint_struct)                                                  :: slint

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================
  
    ! Define local variables

    dst_grid%ncoords = remap%ncoords
    call variable_interface_setup_struct(dst_grid)
    dst_grid%lat     = remap%lat
    dst_grid%lon     = remap%lon
    src_grid%ncoords = topogrid%ncoords
    call variable_interface_setup_struct(src_grid)
    src_grid%lat     = topogrid%lat
    src_grid%lon     = topogrid%lon
    slint%ncoords    = dst_grid%ncoords
    call variable_interface_setup_struct(slint)

    ! Compute local variables

    call interpolation_interface_init(src_grid,dst_grid,slint)

    ! Define local variables

    remap%mask = 1.0

    ! Loop though local variable

    do i = 1, remap%ncoords

       ! Define local variables

       remap%hgt(i) = topogrid%topo(slint%nn(1,i))
       if(topogrid%topo(slint%nn(1,i)) .ne. 0.0) remap%mask(i) = spval

    end do ! do i = 1, remap%ncoords

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(dst_grid)
    call variable_interface_cleanup_struct(src_grid)
    call variable_interface_cleanup_struct(slint)

    !=====================================================================

  end subroutine obs_land_mask

  !=======================================================================

  ! SUBROUTINE:

  ! obs_profile.f90

  ! DESCRIPTION:

  ! This subroutine is the driver level routine for the profile-type
  ! observations.

  !-----------------------------------------------------------------------

  subroutine obs_profile()

    !=====================================================================

    ! Define local variables

    if(is_fcstmdl)  call fcst_model_to_bufr()
    if(is_givtdruv) call givtdruv_to_bufr()
    if(is_hsa)      call hsa_to_bufr()

    !=====================================================================

  end subroutine obs_profile

  !=======================================================================

  ! SUBROUTINE:

  ! obs_surface.f90

  ! DESCRIPTION:

  ! This subroutine is the driver level routine for surface-type
  ! observations.

  !-----------------------------------------------------------------------

  subroutine obs_surface()

    !=====================================================================

    ! Define local variables

    if(is_nhcgtcm) call nhcgtcm_to_bufr()
    if(is_tcm)     call tcm_to_bufr()

    !=====================================================================

  end subroutine obs_surface

  !=======================================================================

  ! SUBROUTINE:

  ! tcm_to_bufr.f90

  ! DESCRIPTION:

  ! This subroutine is the interface to observations generated using
  ! synthetic tropical cyclone (TC) wind-field parameterizations (see
  ! the references for the source code routines necessary to generate
  ! the appropriate netcdf files).

  ! REFERENCES:

  ! https://github.com/hrwinterbottom/tc-synthetic-obs

  !-----------------------------------------------------------------------

  subroutine tcm_to_bufr()

    ! Define variables computed within routine

    type(bufr_struct)                                                   :: bufr
    type(bufr_info_struct)                                              :: bufr_info
    type(error_struct)                                                  :: error
    type(remap_struct)                                                  :: remap
    type(tcm_struct)                                                    :: grid
    character(len=500)                                                  :: lbufr_filepath
    character(len=8)                                                    :: cacobid
    integer(i_long)                                                     :: racobid

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    equivalence(racobid,cacobid)
    bufr_info%filename = tcm_bufr_info_filepath
    call fileio_interface_read(bufr_info)
    bufr%hdstr         = 'SID XOB YOB DHR TYP ELV'
    bufr%obstr         = 'POB ZOB UOB VOB'
    bufr%qcstr         = 'PQM ZQM WQM'
    bufr%oestr         = 'POE NUL WOE'
    bufr%subset        = trim(adjustl(bufr_info%subset))
    bufr%mxmn          = 6
    bufr%mxlv          = 1
    lbufr_filepath     = trim(adjustl(datapath))//'prepbufr.tcm'
    call bufrio_interface_idate(analdate,bufr)
    call bufrio_interface_open(lbufr_filepath,bufr_tblpath,bufr,.false.,   &
         & .false.,.true.)
    call fileio_interface_read(tcm_obs_filepath)

    ! Loop through local variable

    do i = 1, nfiles

       ! Define local variables

       call fileio_interface_read(obs_filename(i),grid)
       remap%ncoords = grid%nvals
       call variable_interface_setup_struct(remap)

       ! Check local variable and proceed accordingly

       if(mask_land) then

          ! Define local variables
          
          remap%lat = grid%lat
          remap%lon = grid%lon
       
          ! Compute local variables

          call obs_land_mask(remap)

       end if ! if(mask_land)

       ! Loop through local variable

       do j = 1, grid%nvals

          ! Define local variables

          write(cacobid,500) j
          call variable_interface_setup_struct(bufr)

          ! Define local variables

          call time_methods_date_attributes(grid%analysis_date,yyyy,mm,    &
               & dd,hh,nn,ss)
          
          ! Compute local variables
          
          call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,obsjday)
          
          ! Define local variables

          bufr%hdr(1)   = dble(racobid)
          bufr%hdr(2)   = dble(grid%lon(j))
          bufr%hdr(3)   = dble(grid%lat(j))
          bufr%hdr(4)   = (obsjday - anljday)*dble(24.0)
          bufr%hdr(5)   = bufr_info%obs_type_wind
          bufr%hdr(6)   = 10.0
          bufr%obs(1,1) = grid%psfc(j)/100.0
          bufr%obs(2,1) = 10.0
          bufr%obs(3,1) = grid%u(j) 
          bufr%obs(4,1) = grid%v(j)
          bufr%qcf(1,1) = 2.0
          bufr%qcf(2,1) = 2.0
          bufr%qcf(3,1) = 2.0
          bufr%qcf(4,1) = 2.0
          
          ! Check local variable and proceed accordingly

          if(remap%mask(j) .ne. spval) then

             ! Define local variables

             call bufrio_interface_write(bufr)

          end if ! if(remap%mask(j) .ne. spval)

          ! Check local variable and proceed accordingly

          if(remap%mask(j) .eq. spval) then

             ! Define local variables

             grid%psfc(j) = spval
             grid%u(j)    = spval
             grid%v(j)    = spval

          end if ! if(remap%mask(j) .eq. spval)

          ! Deallocate memory for local variables

          call variable_interface_cleanup_struct(bufr)

       end do ! do j = 1, grid%nvals

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(grid)
       call variable_interface_cleanup_struct(remap)

    end do ! do i = 1, nfiles

    ! Define local variables

    call bufrio_interface_close(.false.,.true.)

    ! Deallocate memory for local variables

    if(allocated(obs_filename)) deallocate(obs_filename)
    call variable_interface_cleanup_struct(error)

    ! Define local variables

500 format(i5.5,'TCM')

    !=====================================================================

  end subroutine tcm_to_bufr

  !=======================================================================

end module observation_interface
