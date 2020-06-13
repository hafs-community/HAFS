module bufrio_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! da-utils :: bufrio_interface
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

  use kinds_interface
  use time_methods_interface
  use variable_interface

  ! Define interfaces and attributes for module routines

  implicit none
  private
  public :: conv_bufrio_interface_read
  public :: sat_bufrio_interface_read

  ! Define local variables

  integer, parameter                                                    :: unit_in = 10

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! conv_bufrio_interface_read.f90

  ! DESCRIPTION:

  ! This subroutine parses a BUFR-formatted file and returns a FORTRAN
  ! convbufr_struct variable containing the observation position(s)
  ! and times relative to the analysis time.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the BUFR-formatted file to be parsed.

  ! * obtsype; a FORTRAN integer specifying the observation type.

  ! * bufr; a FORTRAN convbufr_struct variable.

  ! OUTPUT VARIABLES:

  ! * bufr; a FORTRAN convbufr_struct variable containing all
  !   attributes (i.e., geographical position, elevation, time, and
  !   subtype if applicable) for the user specified observation type.

  !-----------------------------------------------------------------------

  subroutine conv_bufrio_interface_read(filename,obstype,bufr)

    ! Define variables passed to routine

    type(convbufr_struct)                                               :: bufr                      
    character(len=500)                                                  :: filename
    integer                                                             :: obstype

    !=====================================================================

    ! Define local variables

    bufr%bufr_filename = filename
    bufr%obstype       = obstype
    call get_convbufr_date(filename,bufr)
    call count_convbufr_obstype(filename,obstype,bufr)
    call variable_interface_setup_struct(bufr)
    call read_convbufr_obstype(filename,obstype,bufr)

    !=====================================================================

  end subroutine conv_bufrio_interface_read

  !=======================================================================

  ! SUBROUTINE:

  ! close_bufr.f90

  ! DESCRIPTION:

  ! This subroutine closes the user specified BUFR file path assigned
  ! to the local module variable unit_in.

  !-----------------------------------------------------------------------

  subroutine close_bufr()

    !=====================================================================

    ! Define local variables

    call closbf(unit_in)

    !=====================================================================

  end subroutine close_bufr

  !=======================================================================

  ! SUBROUTINE:

  ! count_convbufr_obstype.f90

  ! DESCRIPTION:

  ! This subroutine determines the total number of observations within
  ! the BUFR-formatted file that correspond to the user specified
  ! observation type.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the BUFR-formatted file to be parsed.

  ! * obtsype; a FORTRAN integer specifying the observation type.

  ! * bufr; a FORTRAN convbufr_struct variable.

  ! OUTPUT VARIABLES:

  ! * bufr; a FORTRAN convbufr_struct variable now containing the
  !   total number of observations (with the nrecs attribute).

  !-----------------------------------------------------------------------

  subroutine count_convbufr_obstype(filename,obstype,bufr)

    ! Define variables passed to routine

    type(convbufr_struct)                                               :: bufr
    character(len=500)                                                  :: filename
    integer                                                             :: obstype

    ! Define variables computed within routine

    character(len=8)                                                    :: subset
    real(r_double)                                                      :: bufrtype(1)
    integer                                                             :: ireadmg
    integer                                                             :: ireadsb
    integer                                                             :: nlev

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    bufr%nrecs = 0
    open(unit_in,file=trim(adjustl(filename)),form='unformatted',            &
         & convert='big_endian')
    call openbf(unit_in,'IN',unit_in)

    ! Loop through local variable

    msg_report: do while(ireadmg(unit_in,subset,bufr%idate) .eq. 0)

       ! Loop through local variable

       sb_report: do while(ireadsb(unit_in) .eq. 0)

          ! Define local variables

          call ufbint(unit_in,bufrtype,1,1,nlev,'TYP')

          ! Check local variable and proceed accordingly

          if(dble(obstype) .eq. bufrtype(1)) then

             ! Define local variables

             bufr%nrecs = bufr%nrecs + 1

          end if ! if(dble(obstype) .eq. bufrtype)

       end do sb_report ! do while(ireadsb(unit_in) .eq. 0)

    end do msg_report ! do while(ireadmg(unit_in,subset) .eq. 0)

    ! Define local variables

    call closbf(unit_in)

    !=====================================================================

  end subroutine count_convbufr_obstype

  !=======================================================================

  ! SUBROUTINE:

  ! count_satbufr_obstype.f90

  ! DESCRIPTION:

  ! This subroutine determines the total number of observations within
  ! the BUFR-formatted file.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the BUFR-formatted file to be parsed.

  ! * bufr; a FORTRAN satbufr_struct variable.

  ! OUTPUT VARIABLES:

  ! * bufr; a FORTRAN satbufr_struct variable now containing the total
  !   number of observations (with the nrecs attribute).

  !-----------------------------------------------------------------------

  subroutine count_satbufr_obstype(filename,bufr)

    ! Define variables passed to routine

    type(satbufr_struct)                                                :: bufr
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=8)                                                    :: subset
    real(r_double)                                                      :: bufrtype(1)
    integer                                                             :: ireadmg
    integer                                                             :: ireadsb
    integer                                                             :: nlev

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    bufr%nrecs = 0
    open(unit_in,file=trim(adjustl(filename)),form='unformatted',            &
         & convert='big_endian')
    call openbf(unit_in,'IN',unit_in)

    ! Loop through local variable

    msg_report: do while(ireadmg(unit_in,subset,bufr%idate) .eq. 0)

       ! Loop through local variable

       sb_report: do while(ireadsb(unit_in) .eq. 0)
          
          ! Define local variables
          
          bufr%nrecs = bufr%nrecs + 1

       end do sb_report ! do while(ireadsb(unit_in) .eq. 0)
       
    end do msg_report ! do while(ireadmg(unit_in,subset) .eq. 0)
    
    ! Define local variables
    
    call closbf(unit_in)

    !=====================================================================

  end subroutine count_satbufr_obstype

  !=======================================================================

  ! SUBROUTINE:

  ! count_satbufr_saids.f90

  ! DESCRIPTION:

  ! This subroutine returns the total number unique BUFR-formatted
  ! satellite identification numbers for each mission within the
  ! respective file.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the user specified BUFR file path.

  ! * bufr; a FORTRAN satbufr_struct variable.

  ! OUTPUT VARIABLES:

  ! * bufr; a FORTRAN satbufr_struct variable containing the
  !   BUFR-formatted file total number unique BUFR-formatted satellite
  !   identification numbers for each mission within the respective
  !   file (e.g., the nsats attribute).

  !-----------------------------------------------------------------------

  subroutine count_satbufr_saids(filename,bufr)

    ! Define variables passed to routine

    type(satbufr_struct)                                                :: bufr
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=8)                                                    :: subset
    logical                                                             :: said_chk
    real(r_double)                                                      :: saids(100)
    real(r_double)                                                      :: bufrtype(1)
    integer                                                             :: ireadmg
    integer                                                             :: ireadsb
    integer                                                             :: iret
    integer                                                             :: nrecs

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    bufr%nsats = 0
    saids      = -999.9
    nrecs      = 0
    open(unit_in,file=trim(adjustl(filename)),form='unformatted',            &
         & convert='big_endian')
    call openbf(unit_in,'IN',unit_in)

    ! Loop through local variable

    msg_report: do while(ireadmg(unit_in,subset,bufr%idate) .ge. 0)

       ! Loop through local variable

       sb_report: do while(ireadsb(unit_in) .eq. 0)

          ! Define local variables

          said_chk = .false.
          call ufbint(unit_in,bufrtype,size(bufrtype),1,iret,'SAID')
          
          ! Loop through local variable

          do i = 1, size(saids)

             ! Check local variable and proceed accordingly

             if(bufrtype(1) .eq. saids(i)) then

                ! Define local variables

                said_chk = .true.

             end if ! if(bufrtype(1) .eq. saids(i))

          end do ! do i = 1, size(saids)

          ! Check local variable and proceed accordingly

          if(.not. said_chk) then
             
             ! Define local variables

             nrecs        = nrecs + 1
             saids(nrecs) = bufrtype(1)

          end if ! if(.not. said_chk)

       end do sb_report ! do while(ireadsb(unit_in) .eq. 0)

    end do msg_report ! do while(ireadmg(unit_in,subset) .ge. 0)

    ! Define local variables

    call closbf(unit_in)
    bufr%nsats = count(saids .ne. -999.9)

    !=====================================================================

  end subroutine count_satbufr_saids

  !=======================================================================

  ! SUBROUTINE:

  ! get_convbufr_date.f90

  ! DESCRIPTION:

  ! This subroutine returns the BUFR-formatted file idate variable and
  ! defines the FORTRAN convbufr_struct variable analdate attribute.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the user specified BUFR file path.

  ! * bufr; a FORTRAN convbufr_struct variable.

  ! OUTPUT VARIABLES:

  ! * bufr; a FORTRAN convbufr_struct variable containing the
  !   BUFR-formatted file idate variable and the defined analdate
  !   variable (the idate and analdate attributes, respectively).

  !-----------------------------------------------------------------------

  subroutine get_convbufr_date(filename,bufr)

    ! Define variables passed to routine

    type(convbufr_struct)                                               :: bufr
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=10)                                                   :: strdate
    character(len=8)                                                    :: subset
    integer                                                             :: idate
    integer                                                             :: ireadmg

    !=====================================================================

    ! Define local variables

    bufr%nrecs = 0
    open(unit_in,file=trim(adjustl(filename)),form='unformatted',          &
         & convert='big_endian')
    call openbf(unit_in,'IN',unit_in)

    ! Loop through local variable

    msg_report: do while(ireadmg(unit_in,subset,bufr%idate) .eq. 0)

       ! Define local variables

       goto 1000

    end do msg_report ! msg_report: do
                      ! while(ireadmg(unit_in,subset,bufr%idate)
                      ! .eq. 0)

    ! Define local variables

1000 continue
    call closbf(unit_in)
    idate = 2000000000 + bufr%idate 
    write(strdate,500) idate
    write(bufr%analdate,501) strdate(1:4), strdate(5:6), strdate(7:8),     &
         & strdate(9:10)
500 format(i10)
501 format(a4,'-',a2,'-',a2,'_',a2,':00:00')

    !=====================================================================

  end subroutine get_convbufr_date

  !=======================================================================

  ! SUBROUTINE:

  ! get_satbufr_date.f90

  ! DESCRIPTION:

  ! This subroutine returns the BUFR-formatted file idate variable and
  ! defines the FORTRAN satbufr_struct variable analdate attribute.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the user specified BUFR file path.

  ! * bufr; a FORTRAN satbufr_struct variable.

  ! OUTPUT VARIABLES:

  ! * bufr; a FORTRAN satbufr_struct variable containing the
  !   BUFR-formatted file idate variable and the defined analdate
  !   variable (the idate and analdate attributes, respectively).

  !-----------------------------------------------------------------------

  subroutine get_satbufr_date(filename,bufr)

    ! Define variables passed to routine

    type(satbufr_struct)                                                :: bufr
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=10)                                                   :: strdate
    character(len=8)                                                    :: subset
    integer                                                             :: idate
    integer                                                             :: ireadmg

    !=====================================================================

    ! Define local variables

    bufr%nrecs = 0
    open(unit_in,file=trim(adjustl(filename)),form='unformatted',          &
         & convert='big_endian')
    call openbf(unit_in,'IN',unit_in)

    ! Loop through local variable

    msg_report: do while(ireadmg(unit_in,subset,bufr%idate) .eq. 0)

       ! Define local variables

       goto 1000

    end do msg_report ! msg_report: do
                      ! while(ireadmg(unit_in,subset,bufr%idate)
                      ! .eq. 0)

    ! Define local variables

1000 continue
    call closbf(unit_in)
    idate = 2000000000 + bufr%idate 
    write(strdate,500) idate
    write(bufr%analdate,501) strdate(1:4), strdate(5:6), strdate(7:8),     &
         & strdate(9:10)
500 format(i10)
501 format(a4,'-',a2,'-',a2,'_',a2,':00:00')

    !=====================================================================

  end subroutine get_satbufr_date

  !=======================================================================

  ! SUBROUTINE:

  ! open_bufr.f90

  ! DESCRIPTION:

  ! This subroutine opens the user specified BUFR file path and
  ! assigns the open file unit to the local module variable unit_out.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the user specified BUFR file path.

  !-----------------------------------------------------------------------

  subroutine open_bufr(filename)

    ! Define variables passed to routine

    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables

    open(unit_in,file=trim(adjustl(filename)),action='read',form=          &
         & 'unformatted',convert='big_endian')

    !=====================================================================
    
  end subroutine open_bufr

  !=======================================================================

  ! SUBROUTINE:

  ! read_convbufr_obstype.f90

  ! DESCRIPTION:

  ! This subroutine collects the observation attributes within the
  ! BUFR-formatted file that correspond to the user specified
  ! observation type.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the BUFR-formatted file to be parsed.

  ! * obtsype; a FORTRAN integer specifying the observation type.

  ! * bufr; a FORTRAN convbufr_struct variable.

  ! OUTPUT VARIABLES:

  ! * bufr; a FORTRAN convbufr_struct variable now containing the
  !   observation attributes for the user-specifed observation tyep.

  !-----------------------------------------------------------------------

  subroutine read_convbufr_obstype(filename,obstype,bufr)

    ! Define variables passed to routine

    type(convbufr_struct)                                               :: bufr
    character(len=500)                                                  :: filename
    integer                                                             :: obstype

    ! Define variables computed within routine

    character(len=8)                                                    :: subset
    real(r_double)                                                      :: bufrtype(6)
    integer                                                             :: ireadmg
    integer                                                             :: ireadsb
    integer                                                             :: nlev

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    bufr%nrecs = 0
    open(unit_in,file=trim(adjustl(filename)),form='unformatted',          &
         & convert='big_endian')
    call openbf(unit_in,'IN',unit_in)

    ! Loop through local variable

    msg_report: do while(ireadmg(unit_in,subset,bufr%idate) .eq. 0)

       ! Loop through local variable

       sb_report: do while(ireadsb(unit_in) .eq. 0)

          ! Define local variables

          call ufbint(unit_in,bufrtype,6,1,nlev,'TYP XOB YOB DHR ELV TSB')

          ! Check local variable and proceed accordingly

          if(dble(obstype) .eq. bufrtype(1)) then

             ! Define local variables

             bufr%nrecs                      = bufr%nrecs + 1
             bufr%elev(bufr%nrecs)           = real(bufrtype(5))
             bufr%lat(bufr%nrecs)            = real(bufrtype(3))
             bufr%lon(bufr%nrecs)            = real(bufrtype(2))
             bufr%offset_seconds(bufr%nrecs) = real(bufrtype(4))*3600.0
             bufr%subtype(bufr%nrecs)        = real(bufrtype(6))

             ! Check local variable and proceed accordingly

             if(bufr%subtype(bufr%nrecs) .eq. bufr_spval) then

                ! Define local variables

                bufr%subtype(bufr%nrecs) = spval

             else   ! if(bufr%subtype(bufr%nrecs) .eq. bufr_spval)

                ! Define local variables

                bufr%subtype(bufr%nrecs) = real(int(bufrtype(6)/10.0))

             end if ! if(bufr%subtype(bufr%nrecs) .eq. bufr_spval)
             
             ! Check local variable and proceed accordingly

             if(bufr%subtype(bufr%nrecs) .eq. bufr_spval) then

                ! Define local variables

                bufr%subtype(bufr%nrecs) = spval

             end if ! if(bufr%subtype(bufr%nrecs) .eq. bufr_spval)
             
          end if ! if(dble(obstype) .eq. bufrtype)

       end do sb_report ! do while(ireadsb(unit_in) .eq. 0)

    end do msg_report ! do while(ireadmg(unit_in,subset) .eq. 0)

    ! Define local variables

    call closbf(unit_in)

    !=====================================================================

  end subroutine read_convbufr_obstype

  !=======================================================================

  ! SUBROUTINE:

  ! read_satbufr_obstype.f90

  ! DESCRIPTION:

  ! This subroutine collects the observation attributes within the
  ! BUFR-formatted file that correspond to the user specified
  ! observation type.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the BUFR-formatted file to be parsed.

  ! * bufr; a FORTRAN satbufr_struct variable.

  ! OUTPUT VARIABLES:

  ! * bufr; a FORTRAN satbufr_struct variable now containing the
  !   observation attributes for the observations within the file.

  !-----------------------------------------------------------------------

  subroutine read_satbufr_obstype(filename,bufr)

    ! Define variables passed to routine

    type(satbufr_struct)                                                :: bufr
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=8)                                                    :: subset
    logical                                                             :: said_chk
    real(r_double)                                                      :: bufrtype(9)
    real(r_double)                                                      :: analjday
    real(r_double)                                                      :: obsjday
    integer                                                             :: ireadmg
    integer                                                             :: ireadsb
    integer                                                             :: iret
    integer                                                             :: yyyy
    integer                                                             :: mm
    integer                                                             :: dd
    integer                                                             :: hh
    integer                                                             :: nn
    integer                                                             :: ss
    integer                                                             :: hhmmss

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    bufr%nrecs = 0
    bufr%nsats = 0
    open(unit_in,file=trim(adjustl(filename)),form='unformatted',          &
         & convert='big_endian')
    call openbf(unit_in,'IN',unit_in)
    read(bufr%analdate(1:4),'(i4)')   yyyy
    read(bufr%analdate(6:7),'(i2)')   mm
    read(bufr%analdate(9:10),'(i2)')  dd 
    read(bufr%analdate(12:13),'(i2)') hh
    read(bufr%analdate(15:16),'(i2)') nn
    read(bufr%analdate(18:19),'(i2)') ss
    call time_methods_julian_day(yyyy,mm,dd,analjday)
    analjday   = analjday + (hh*3600.0 + nn*60.0 + real(ss))/86400.0

    ! Loop through local variable

    msg_report: do while(ireadmg(unit_in,subset,bufr%idate) .ge. 0)

       ! Loop through local variable

       sb_report: do while(ireadsb(unit_in) .eq. 0)

          ! Define local variables

          said_chk                        = .false.
          call ufbint(unit_in,bufrtype,size(bufrtype),1,iret,              &
               & 'SAID CLAT CLON YEAR MNTH DAYS HOUR MINU SECO')
          bufr%nrecs                      = bufr%nrecs + 1
          bufr%said(bufr%nrecs)           = real(bufrtype(1))
          bufr%lat(bufr%nrecs)            = real(bufrtype(2))
          bufr%lon(bufr%nrecs)            = real(bufrtype(3))
          call time_methods_julian_day(int(bufrtype(4)),int(bufrtype(5)),  &
               & int(bufrtype(6)),obsjday)
          obsjday                         = obsjday + (bufrtype(7)*3600.0  &
               & + bufrtype(8)*60.0 + real(bufrtype(9)))/86400.0
          bufr%offset_seconds(bufr%nrecs) = (obsjday*86400.0) - (analjday* &
               & 86400.0)

          ! Loop through local variable

          do i = 1, size(bufr%usaids)

             ! Check local variable and proceed accordingly

             if(bufr%said(bufr%nrecs) .eq. bufr%usaids(i)) then

                ! Define local variables

                said_chk = .true.
                
             end if ! if(bufr%said(bufr%nrecs) .eq. bufr%usaids(i))

          end do ! do i = 1, size(bufr%usaids)

          ! Check local variable and proceed accordingly

          if(.not. said_chk) then
             
             ! Define local variables

             bufr%nsats              = bufr%nsats + 1
             bufr%usaids(bufr%nsats) = bufr%said(bufr%nrecs)

          end if ! if(.not. said_chk)
          
       end do sb_report ! do while(ireadsb(unit_in) .eq. 0)

    end do msg_report ! do while(ireadmg(unit_in,subset) .ge. 0)

    ! Define local variables

    call closbf(unit_in)

    !=====================================================================

  end subroutine read_satbufr_obstype

  !=======================================================================

  ! SUBROUTINE:

  ! sat_bufrio_interface_read.f90

  ! DESCRIPTION:

  ! This subroutine parses a BUFR-formatted file and returns a FORTRAN
  ! satbufr_struct variable containing the observation position(s)
  ! and times relative to the analysis time.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the BUFR-formatted file to be parsed.

  ! * bufr; a FORTRAN convbufr_struct variable.

  ! OUTPUT VARIABLES:

  ! * bufr; a FORTRAN convbufr_struct variable containing all
  !   attributes (i.e., geographical position, elevation, time, and
  !   subtype if applicable) for the user specified observation type.

  !-----------------------------------------------------------------------

  subroutine sat_bufrio_interface_read(filename,bufr)

    ! Define variables passed to routine

    type(satbufr_struct)                                                :: bufr
    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables

    bufr%bufr_filename = filename
    call get_satbufr_date(filename,bufr)
    call count_satbufr_obstype(filename,bufr)
    call count_satbufr_saids(filename,bufr)
    call variable_interface_setup_struct(bufr)
    call read_satbufr_obstype(filename,bufr)

    !=====================================================================

  end subroutine sat_bufrio_interface_read

  !=======================================================================

end module bufrio_interface
