module recon_tdr_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: recon_tdr_interface
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

  use bufrio_interface
  use namelist_interface
  use time_methods_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: recon_tdr
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! recon_tdr.f90

  ! DESCRIPTION:

  ! This is the driver routine for determining the status and usage
  ! capabilities for Tail-Doppler Radar (TDR) Binary Universal Format
  ! (BUFR) file observations collected during tropical cyclone (TC)
  ! events.

  !-----------------------------------------------------------------------

  subroutine recon_tdr()

    ! Define variables computed within routine

    type(tdr_struct)                                                    :: tdr
    
    !=====================================================================

    ! Define local variables

    call tdr_stmid(tdr)
    call tdr_timeinfo(tdr)
    call tdr_flag(tdr)
    call write_tdr_status(tdr)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(tdr)

    !=====================================================================

  end subroutine recon_tdr

  !=======================================================================

  ! SUBROUTINE:

  ! tdr_flag.f90

  ! DESCRIPTION:

  ! This subroutine determines

  ! INPUT VARIABLES:

  ! * tdr; a FORTRAN tdr_struct variable containing (at minimum)
  !   arrays for the event identifiers (stmid) and the observation
  !   time offsets relative to the file timestamp (time_min and
  !   time_max).

  ! OUTPUT VARIABLES:

  ! * tdr; a FORTRAN tdr_struct variable containing the usage flag for
  !   the TDR observation; 0 = use, 1 = no usage based on
  !   user-specified (namelist-level) time check thresholds.

  !-----------------------------------------------------------------------

  subroutine tdr_flag(tdr)

    ! Define variables passed to routine

    type(tdr_struct)                                                    :: tdr

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Loop through local variable

    do i = 1, size(tdr%stmid)
    
       ! Check local variable and proceed accordingly

       if((tdr%time_min(i) .gt. tdr_min_offset_seconds) .or.               &
            & (tdr%time_max(i) .lt. tdr_max_offset_seconds) .or.           &
            & (abs(tdr%time_max(i) - tdr%time_min(i)) .lt.                 &
            & tdr_offset_dseconds)) then

          ! Define local variables

          tdr%flag(i) = 1

       else   ! if((tdr%time_min(i) .gt. tdr_min_offset_seconds)
              ! .or. (tdr%time_max(i) .lt. tdr_max_offset_seconds)
              ! .or. (abs(tdr%time_max(i) - tdr%time_min(i))
              ! .lt. tdr_offset_dseconds))

          ! Define local variables

          tdr%flag(i) = 0

       end if ! if((tdr%time_min(i) .gt. tdr_min_offset_seconds)
              ! .or. (tdr%time_max(i) .lt. tdr_max_offset_seconds)
              ! .or. (abs(tdr%time_max(i) - tdr%time_min(i))
              ! .lt. tdr_offset_dseconds))

    end do ! do i = 1, size(tdr%stmid)
       
    !=====================================================================

  end subroutine tdr_flag

  !=======================================================================

  ! SUBROUTINE:

  ! tdr_stmid.f90

  ! DESCRIPTION:

  ! This subroutine determines the tropical cyclone (TC) identifier
  ! for which the tail-Doppler radar (TDR) observations are collected.

  ! INPUT VARIABLES:

  ! * tdr; a FORTRAN tdr_struct variable.

  ! OUTPUT VARIABLES:

  ! * tdr; a FORTRAN tdr_struct variable containing the number of TC
  !   event TDR observations within the BUFR-formatted file and the
  !   corresponding TC identifiers.

  !-----------------------------------------------------------------------

  subroutine tdr_stmid(tdr)

    ! Define variables passed to routine

    type(tdr_struct)                                                    :: tdr

    ! Define variables computed within routine

    type(bufr_struct)                                                   :: bufr
    type(bufrhdr_struct)                                                :: bufrhdr
    character(len=3)                                                    :: stmid
    real(r_double)                                                      :: hdr
    integer                                                             :: tcid
    
    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================

    ! Define local variables

    equivalence(hdr,stmid)
    bufr%mxmn  = 1
    bufr%mxlv  = 1
    bufr%hdstr = 'STMID'
    call bufrio_interface_readhdruniq(recon_tdr_filepath,bufr,bufrhdr)
    tdr%nstmid = size(bufrhdr%hdr)
    call variable_interface_setup_struct(tdr)

    ! Loop through local variable

    do i = 1, size(bufrhdr%hdr)

       ! Define local variables

       hdr = bufrhdr%hdr(1,i)
       read(stmid,'(i)') tcid
       call write_tdr_tcid(tcid,tdr%stmid(i))

    end do ! do i = 1, size(bufrhdr%hdr)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(bufrhdr)

    ! Define local variables

500 format(i2.2,'L')
501 format(i2.2,'E')
502 format(i2.2,'C')
    
    !=====================================================================
    
  end subroutine tdr_stmid

  !=======================================================================

  ! SUBROUTINE:

  ! tdr_timeinfo.f90

  ! DESCRIPTION:

  ! This subroutine collects the file and observation time information
  ! for each respective TDR event.

  ! INPUT VARIABLES:

  ! * tdr; a FORTRAN tdr_struct variable containing (at minimum) the
  !   number of TC event TDR observations within the BUFR-formatted
  !   file and the corresponding TC identifiers.

  ! OUTPUT VARIABLES:

  ! * tdr; a FORTRAN tdr_struct variable containing the observation
  !   time offsets (time_min and time_max) relative to the file
  !   timestamp (file_timestamp).
  
  !-----------------------------------------------------------------------

  subroutine tdr_timeinfo(tdr)

    ! Define variables passed to routine

    type(tdr_struct)                                                    :: tdr 

    ! Define variables computed within routine

    type(bufr_struct)                                                   :: bufr
    type(bufrhdr_struct)                                                :: bufrhdr
    character(len=10)                                                   :: datestr
    character(len=3)                                                    :: ctcid
    character(len=3)                                                    :: stmid
    real(r_double)                                                      :: hdr(7)
    real(r_double)                                                      :: jday_file
    real(r_double)                                                      :: jday_obs
    real(r_double)                                                      :: time_diff
    integer                                                             :: dd
    integer                                                             :: hh
    integer                                                             :: mm
    integer                                                             :: nn
    integer                                                             :: ss
    integer                                                             :: tcid
    integer                                                             :: yyyy
    
    ! Define counting variables

    integer                                                             :: i, j
    
    !=====================================================================

    ! Define local variables

    equivalence(hdr(7),stmid)
    bufr%mxmn  = 7
    bufr%mxlv  = 1
    bufr%hdstr = 'YEAR MNTH DAYS HOUR MINU SECO STMID'
    call bufrio_interface_readhdrall(recon_tdr_filepath,bufr,bufrhdr)
    write(datestr,'(i10)') bufr%idate
    read(datestr,'(i4,3i2)') yyyy, mm, dd, hh
    yyyy       = 2000 + yyyy
    nn         = 0
    ss         = 0
    write(tdr%file_timestamp,500) yyyy, mm, dd, hh, nn, ss

    ! Compute local variables

    call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,jday_file)

    ! Loop through local variable

    do i = 1, size(tdr%stmid)

       ! Define local variables

       tdr%time_max(i) = -1.0*spval
       tdr%time_min(i) = spval
       tdr%nrecs(i)    = 0
 
       ! Loop through local variable
    
       do j = 1, bufr%nrecs

          ! Define local variables

          hdr(7) = bufrhdr%hdr(7,j)
          read(stmid,'(i)') tcid
          call write_tdr_tcid(tcid,ctcid)

          ! Check local variable and proceed accordingly

          if(trim(adjustl(ctcid)) .eq. trim(adjustl(tdr%stmid(i)))) then      
          
             ! Define local variables
          
             yyyy = int(bufrhdr%hdr(1,j))
             mm   = int(bufrhdr%hdr(2,j))
             dd   = int(bufrhdr%hdr(3,j))
             hh   = int(bufrhdr%hdr(4,j))
             nn   = int(bufrhdr%hdr(5,j))
             ss   = int(bufrhdr%hdr(6,j))

             ! Compute local variables

             call time_methods_julian_day(yyyy,mm,dd,hh,nn,ss,jday_obs)
             time_diff = (jday_obs - jday_file)*86400
             
             ! Define local variables

             tdr%time_min(i) = min(tdr%time_min(i),time_diff)
             tdr%time_max(i) = max(tdr%time_max(i),time_diff)
             tdr%nrecs(i)    = tdr%nrecs(i) + 1

          end if ! if(trim(adjustl(ctcid))
                 ! .eq. trim(adjustl(tdr%stmid(i))))
             
       end do ! do j = 1, bufr%nrecs

    end do ! do i = 1, size(tdr%stmid)
       
    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(bufrhdr)

    ! Define local variables

500 format(i4.4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2,':',i2.2)

    !=====================================================================

  end subroutine tdr_timeinfo
  
  !=======================================================================

  ! SUBROUTINE:

  ! write_tdr_status.f90

  ! DESCRIPTION:

  ! This subroutine writes an external formatted file for each TDR
  ! event which contains information pertaining to the collected TDR
  ! observations; each file is written to the data path specified by
  ! the user (datapath, namelist level) as 'recon_tdr_status.<TCID>',
  ! where <TCID> is the TDR event identifier.

  ! INPUT VARIABLES:

  ! * tdr; a FORTRAN tdr_struct variable.

  !-----------------------------------------------------------------------

  subroutine write_tdr_status(tdr)

    ! Define variables passed to routine

    type(tdr_struct)                                                    :: tdr

    ! Define variables computed within routine

    character(len=500)                                                  :: filename

    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================

    ! Loop through local variable

    do i = 1, size(tdr%stmid)
    
       ! Define local variables

       write(filename,500) tdr%stmid(i)
       filename = trim(adjustl(datapath))//filename
       open(99,file=trim(adjustl(filename)),form='formatted')
       write(99,501) tdr%stmid(i)
       write(99,502) tdr%file_timestamp
       write(99,503) tdr%time_min(i)
       write(99,504) tdr%time_max(i)
       write(99,505) tdr%nrecs(i)
       write(99,506) tdr%flag(i)
       write(99,507) trim(adjustl(recon_tdr_filepath))
       close(99)

    end do ! do i = 1, size(tdr%stmid)

    ! Define local variables
    
500 format('recon_tdr_status.',a3)
501 format('EVENT: ',a3)
502 format('FILE_TIMESTAMP: ',a19)
503 format('MIN_SECONDS_OFFSET: ',f13.5)
504 format('MAX_SECONDS_OFFSET: ',f13.5)
505 format('NUMBER_OBSERVATIONS: ',i)
506 format('TDR_USAGE_FLAG: ',i1)
507 format('FILE_PATH: ',a)
    
    !=====================================================================

  end subroutine write_tdr_status
  
  !=======================================================================

  ! SUBROUTINE:

  ! write_tdr_tcid.f90

  ! DESCRIPTION:

  ! This subroutine defines a logical character string corresponding
  ! to the TDR event.

  ! INPUT VARIABLES:

  ! * itcid; a FORTRAN integer value containing the TDR event
  !   identifier.

  ! * ctcid; a FORTRAN character string.

  ! OUTPUT VARIABLES:

  ! * ctcid; a FORTRAN character string defining the TC event
  !   corresponding to the TDR event.

  ! NOTES:

  ! 100 <= itcid < 200; North Atlantic Ocean TDR event.

  ! 200 <= itcid < 300; Eastern Pacific Ocean TDR event.

  ! 300 <= itcid; Central Pacific Ocean TDR event.
  
  !-----------------------------------------------------------------------

  subroutine write_tdr_tcid(itcid,ctcid)

    ! Define variables passed to routine
    
    character(len=3)                                                    :: ctcid
    integer                                                             :: itcid
    
    !=====================================================================
    
    ! Check local variable and proceed accordingly

    if((itcid .ge. 100) .and. (itcid .lt. 200)) then

       ! Define local variables

       write(ctcid,500) int(itcid - 100)

    else if((itcid .ge. 200) .and. (itcid .lt. 300)) then

       ! Define local variables

       write(ctcid,501) int(itcid - 200)

    else if(itcid .lt. 300) then

       ! Define local variables

       write(ctcid,502) int(itcid - 300)

    end if ! if((itcid .ge. 100) .and. (itcid .lt. 200))

    ! Define local variables

500 format(i2.2,'L')
501 format(i2.2,'E')
502 format(i2.2,'C')   

    !=====================================================================
    
  end subroutine write_tdr_tcid
    
  !=======================================================================

end module recon_tdr_interface
