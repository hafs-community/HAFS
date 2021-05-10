module bufr_obs_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: bufr_obs_interface
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

  use namelist_interface
  use time_methods_interface
  use variable_interface
  
  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: bufr_obs_update

  ! Define local variables

  logical                                                               :: in_bufr_open  = .false.
  logical                                                               :: out_bufr_open = .false.
  integer                                                               :: iret
  integer, parameter                                                    :: unit_in       = 10
  integer, parameter                                                    :: unit_out      = 20
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! bufr_obs_update.f90

  ! DESCRIPTION:

  ! This is the driver-level subroutine to compute the temporal window
  ! for which to parse observations and to subsequently write those
  ! respective observations to an external BUFR-formatted file.

  !-----------------------------------------------------------------------

  subroutine bufr_obs_update()

    ! Define variables computed within routine

    type(timeinfo_struct)                                               :: timeinfo
    
    !=====================================================================

    ! Define local variables

    call valid_times(timeinfo)
    
    ! Compute local variables
    
    call readwrite_bufrobs(bufr_obs_filename,timeinfo)

    !=====================================================================

  end subroutine bufr_obs_update  

  !=======================================================================

  ! SUBROUTINE:

  ! close_bufr.f90

  ! DESCRIPTION:

  ! This subroutine closes all open input and output files designated
  ! by the unit_in and unit_out local variables, respectively.

  !-----------------------------------------------------------------------

  subroutine close_bufr()

    !=====================================================================
    
    ! Define local variables

    close(unit_in)
    call closbf(unit_in)
    close(unit_out)
    call closbf(unit_out)
    
    !=====================================================================

  end subroutine close_bufr

  !=======================================================================

  ! SUBROUTINE:

  ! init_bufr.f90

  ! DESCRIPTION:

  ! This subroutine initializes the BUFR file interfaces for the input
  ! and output files.
  
  ! INPUT VARIABLES:
  
  ! * filename; a FORTRAN character string specifying the full-path to
  !   the BUFR-formatted file to be parsed.

  !-----------------------------------------------------------------------

  subroutine init_bufr(filename)

    ! Define variables passed to routine

    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables

    call datelen(10)
    if(in_bufr_open) call closbf(unit_in)
    open(unit_in,file=trim(adjustl(filename)),form='unformatted',          &
         & status='old')
    call openbf(unit_in,'IN',unit_in)
    in_bufr_open = .true.
    
    ! Check local variable and proceed accordingly
    
    if(.not. out_bufr_open) then

       ! Define local variables

       open(unit_out,file=trim(adjustl(bufr_filepath)),                    &
            & form='unformatted')
       call openbf(unit_out,'OUT',unit_in)
       out_bufr_open = .true.

    end if ! if(.not. out_bufr_open)
    
    !=====================================================================  

  end subroutine init_bufr

  !=======================================================================

  ! SUBROUTINE:

  ! readwrite_bufrobs.f90

  ! DESCRIPTION:

  ! This subroutine parses the BUFR records from a user specified file
  ! and computes the time-stamp attributes; if the observation
  ! time-stamp attributes occur between the user specified threshold
  ! values (relative to the analysis time-stamp), the respective BUFR
  ! record is written to the external file defined by the unit_out
  ! local variable.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the BUFR-formatted file to be parsed.

  ! * timeinfo; a FORTRAN timeinfo_struct variable containing the
  !   Julian day values corresponding to the observation wind
  !   specified by the user.

  !-----------------------------------------------------------------------

  subroutine readwrite_bufrobs(filename,timeinfo)

    ! Define variables passed to routine

    type(timeinfo_struct)                                               :: timeinfo
    character(len=500)                                                  :: filename(:)

    ! Define variables computed within routine

    character(len=10)                                                   :: datestr
    character(len=8)                                                    :: subset
    real(r_double)                                                      :: bufrtype(6)   
    real(r_double)                                                      :: dhr
    real(r_double)                                                      :: obs_jday
    integer                                                             :: dd
    integer                                                             :: hh    
    integer                                                             :: idate
    integer                                                             :: ireadmg
    integer                                                             :: ireadsb
    integer                                                             :: mm
    integer                                                             :: nn
    integer                                                             :: nobs_copy
    integer                                                             :: nobs_total
    integer                                                             :: obs_idate
    integer                                                             :: ss
    integer                                                             :: yyyy
    
    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================
    
    ! Define local variables

    nobs_copy  = 0
    nobs_total = 0

    ! Loop through local variable

    do i = 1, nbufr_obs_files

       ! Define local variables

       if(debug) write(6,500) trim(adjustl(filename(i)))
       call init_bufr(filename(i))
    
       ! Loop through local variable

       do while(ireadmg(unit_in,subset,idate) .eq. 0)

          ! Define local variables

          call openmb(unit_out,subset,timeinfo%idate)
          if(is_gpsrobufr) call maxout(100000)
          
          ! Loop through local variable

          do while(ireadsb(unit_in) .eq. 0)
                
             ! Define local variables
             
             nobs_total = nobs_total + 1

             ! Check local variable and proceed accordingly

             if(is_gpsrobufr) then

                ! Define local variables

                call ufbint(unit_in,bufrtype,size(bufrtype),1,iret,        &
                     & 'YEAR MNTH DAYS HOUR MINU SECO')
                yyyy = int(bufrtype(1))
                mm   = int(bufrtype(2))
                dd   = int(bufrtype(3))
                hh   = int(bufrtype(4))
                nn   = int(bufrtype(5))
                ss   = int(bufrtype(6))

                ! Compute local variables

                call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,          &
                     & obs_jday)
                
             end if ! if(is_gpsrobufr)
             
             ! Check local variable and proceed accordingly

             if(is_prepbufr) then

                ! Define local variables
             
                call ufbint(unit_in,bufrtype,2,1,iret,'DHR TYP')
                write(datestr,'(i10)') idate
                read(datestr,'(i4.4,3(i2.2))') yyyy, mm, dd, hh
                nn = 0
                ss = 0
                
                ! Compute local variables

                call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,          &
                     & obs_jday)
                obs_jday = obs_jday + bufrtype(1)/24.0
                
             end if ! if(is_prepbufr)
             
             ! Check local variable and proceed accordingly
             
             if(is_satbufr) then
          
                ! Define local variables

                call ufbint(unit_in,bufrtype,size(bufrtype),1,iret,        &
                     & 'YEAR MNTH DAYS HOUR MINU SECO')
                yyyy = int(bufrtype(1))
                mm   = int(bufrtype(2))
                dd   = int(bufrtype(3))
                hh   = int(bufrtype(4))
                nn   = int(bufrtype(5))
                ss   = int(bufrtype(6))

                ! Compute local variables

                call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,          &
                     & obs_jday)

             end if ! if(is_satbufr)
             
             ! Check local variable and proceed accordingly

             if((obs_jday .ge. timeinfo%minjday) .and. (obs_jday .le.      &
                  & timeinfo%maxjday)) then 
                
                ! Define local variables

                nobs_copy = nobs_copy + 1
                
                ! Define local variables
                
                call ufbcpy(unit_in,unit_out)

                ! Check local variable and proceed accordingly

                if(is_prepbufr) then

                   ! Compute local variables

                   dhr = (timeinfo%jday - obs_jday)/24.0

                   ! Define local variables

                   bufrtype(1) = dhr
                   call ufbint(unit_out,bufrtype(1),1,1,iret,'DHR')
                   
                end if ! if(is_prepbufr)

                ! Define local variables

                call writsb(unit_out)
                
             end if ! if((obs_jday .ge. timeinfo%minjday)
                    ! .and. (obs_jday .le. timeinfo%maxjday))
                
          end do  ! do while(ireadsb(unit_in) .eq. 0)
          
       end do  ! do while(ireadmg(unit_in,subset,idate) .eq. 0)
       
    end do ! do i = 1, nbufr_obs_files
    
    ! Define local variables

    call close_bufr()
    if(debug) write(6,501) nobs_copy, nobs_total, bufr_obs_mindate,        &
         & bufr_obs_maxdate
500 format('READWRITE_BUFROBS: Reading BUFR file ',a,'.')
501 format('READWRITE_BUFROBS: ',i,1x,'observations of ',i,1x,' are ',     &
         & 'within the time-window ',a19,1x,'to ',a19,1x,'.')
    
    !=====================================================================

  end subroutine readwrite_bufrobs

  !=======================================================================

  ! SUBROUTINE:

  ! valid_times.f90

  ! DESCRIPTION:

  ! This subroutine computes the Julian day values corresponding to
  ! the observation window specified by the user.

  ! INPUT VARIABLES:

  ! * timeinfo; a FORTRAN timeinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * timeinfo; a FORTRAN timeinfo_struct variable containing the
  !   Julian day values corresponding to the observation wind
  !   specified by the user.

  !-----------------------------------------------------------------------

  subroutine valid_times(timeinfo)

    ! Define variables passed to routine

    type(timeinfo_struct)                                               :: timeinfo

    ! Define variables computed within routine

    integer                                                             :: dd
    integer                                                             :: hh
    integer                                                             :: mm
    integer                                                             :: nn
    integer                                                             :: ss
    integer                                                             :: yyyy

    !=====================================================================

    ! Define local variables

    call time_methods_date_attributes(analdate,yyyy,mm,dd,hh,nn,ss)
    call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,timeinfo%jday)
    write(timeinfo%idatestr,'(i4.4,i2.2,i2.2,i2.2)') yyyy, mm, dd, hh
    read(timeinfo%idatestr,'(i10)') timeinfo%idate
    call time_methods_date_attributes(bufr_obs_mindate,yyyy,mm,dd,hh,nn,   &
         & ss)
    call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,timeinfo%minjday)
    call time_methods_date_attributes(bufr_obs_maxdate,yyyy,mm,dd,hh,nn,   &
         & ss)
    call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,timeinfo%maxjday)

    !=====================================================================

  end subroutine valid_times
 
  !=======================================================================

end module bufr_obs_interface
