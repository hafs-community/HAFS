module bufrio_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! obs-preproc :: bufrio_interface
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
  use namelist_interface
  use variable_interface

  ! Define interfaces and attributes for module routines

  implicit none
  private
  public :: bufrio_interface_close
  public :: bufrio_interface_idate
  public :: bufrio_interface_nrecs
  public :: bufrio_interface_open
  public :: bufrio_interface_readhdrall
  public :: bufrio_interface_readhdruniq
  public :: bufrio_interface_write
  public :: bufr_iret

  ! Define local variables

  logical                                                               :: bufr_exist = .false.
  integer, parameter                                                    :: unit_in    = 10
  integer, parameter                                                    :: unit_out   = 20
  integer, parameter                                                    :: unit_tbl   = 30
  integer                                                               :: bufr_iret

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! bufrio_interface_close.f90

  ! DESCRIPTION:

  ! This subroutine closes the user specified BUFR file path assigned
  ! to the local module variable unit_out.

  ! INPUT VARIABLES:

  ! * bufr_read; a FORTRAN logical variable indicating whether the
  !   file to be closed has been opened for reading.

  ! * bufr_write; a FORTRAN logical variable indicating whether the
  !   file to be closed has been opened for writing.

  !-----------------------------------------------------------------------

  subroutine bufrio_interface_close(bufr_read,bufr_write)

    ! Define variables passed to routine

    logical                                                             :: bufr_read
    logical                                                             :: bufr_write

    !=====================================================================

    ! Define local variables

    if(bufr_read) then
       call closbf(unit_in)
    end if ! if(bufr_read)
    if(bufr_write) then
       close(unit_tbl)
       call closmg(unit_out)
       call closbf(unit_out)
    end if ! if(bufr_write)

    !=====================================================================

  end subroutine bufrio_interface_close

  !=======================================================================

  ! SUBROUTINE:

  ! bufio_interface_idate.f90

  ! DESCRIPTION:

  ! This subroutine defines the integer date string for the BUFR file.

  ! INPUT VARIABLES:

  ! * datestr; a FORTRAN character string specifying the analysis
  !   date, formatted as (assuming UNIX convention)
  !   'yyyy-mm-dd_HH:MM:SS'.

  ! * bufr; a FORTRAN bufr_struct variable.

  ! OUTPUT VARIABLES:

  ! * bufr; a FORTRAN bufr_struct variable containing the 'idate'
  !   variable attribute value.

  !-----------------------------------------------------------------------

  subroutine bufrio_interface_idate(datestr,bufr)

    ! Define variables passed to routine

    type(bufr_struct)                                                   :: bufr
    character(len=19)                                                   :: datestr

    ! Define variables computed within routine

    character(len=10)                                                   :: strdate
    integer                                                             :: dateint
    integer                                                             :: strint

    !=====================================================================

    ! Define local variables

    write(strdate,'(a4,a2,a2,a2)') datestr(1:4), datestr(6:7),             &
         & datestr(9:10), datestr(12:13)
    read(strdate,500) dateint
    bufr%idate = dateint
500 format(i10)

    !=====================================================================

  end subroutine bufrio_interface_idate

  !=======================================================================
  
  ! SUBROUTINE:

  ! bufrio_interface_nrecs.f90

  ! DESCRIPTION:

  ! This subroutine counts the number of BUFR messages within the user
  ! specified BUFR formatted file.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the user specified BUFR file path.

  ! * tablename; a FORTRAN character string specifying the full-path
  !   to the file containing the BUFR table.

  ! * bufr; a FORTRAN bufr_struct variable, containing (at minimum)
  !   the variable 'idate'.

  ! * nrecs; a FORTRAN integer variable to define the number of BUFR
  !   messages.

  ! OUTPUT VARIABLES:

  ! * nrecs; a FORTRAN integer variable defining the number of BUFR
  !   messages within the user specified BUFR file.

  !-----------------------------------------------------------------------

  subroutine bufrio_interface_nrecs(filename,tablename,bufr)

    ! Define variables passed to routine

    type(bufr_struct)                                                   :: bufr
    character(len=500)                                                  :: filename
    character(len=500)                                                  :: tablename

    ! Define variables computed within routine

    character(len=8)                                                    :: msgtype
    integer                                                             :: ireadmg
    integer                                                             :: nrecs
    
    !=====================================================================

    ! Define local variables

    nrecs = 0
    call bufrio_interface_open(filename,tablename,bufr,.true.,.false.,     &
         & .false.)

    ! Loop through local variable

    do while(ireadmg(unit_in,msgtype,bufr%idate) == 0)

       ! Define local variables

       nrecs = nrecs + 1

    end do ! do while(ireadmg(unit_in,msgtype,bufr%idate) == 0)

    ! Define local variables

    bufr%nrecs = nrecs
    call bufrio_interface_close(.true.,.false.)

    !=====================================================================

  end subroutine bufrio_interface_nrecs

  !=======================================================================

  ! SUBROUTINE:

  ! bufrio_interface_open.f90

  ! DESCRIPTION:

  ! This subroutine checks for the existence of BUFR formatted file,
  ! specified by the file path provided by the user; the BUFR table
  ! provided by the user is written to the BUFR file and and the
  ! respective file is prepared for the BUFR records to be written.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the user specified BUFR file path.

  ! * tablename; a FORTRAN character string specifying the full-path
  !   to the file containing the BUFR table.

  ! * bufr; a FORTRAN bufr_struct variable, containing (at minimum)
  !   the variable 'idate'.

  ! * bufr_read; a FORTRAN logical variable indicating whether the
  !   file to be closed has been opened for reading.

  ! * bufr_write; a FORTRAN logical variable indicating whether the
  !   file to be closed has been opened for writing.

  !-----------------------------------------------------------------------

  subroutine bufrio_interface_open(filename,tablename,bufr,bufr_read,      &
       & bufr_read_write,bufr_write)

    ! Define variables passed to routine

    type(bufr_struct)                                                   :: bufr
    character(len=500)                                                  :: filename
    character(len=500)                                                  :: tablename
    logical                                                             :: bufr_read
    logical                                                             :: bufr_read_write
    logical                                                             :: bufr_write

    !=====================================================================

    ! Define local variables

    inquire(file = trim(adjustl(filename)),exist = bufr_exist)
    if(bufr_read) then
       call read_bufr_open(filename)
       call openbf(unit_in,'IN',unit_in)
    end if ! if(bufr_read)
    if(bufr_read_write) then
       call read_write_bufr_open(filename)
       call openbf(unit_in,'IN',unit_in)
       call openbf(unit_out,'OUT',unit_in)
    end if ! if(bufr_read_write)
    if(bufr_write) then
       call write_bufr_open(filename)
       call write_bufr_table(tablename)
       call openmb(unit_out,bufr%subset,bufr%idate)
    end if ! if(bufr_write)

    !=====================================================================

  end subroutine bufrio_interface_open

  !=======================================================================

  ! SUBROUTINE:

  ! bufrio_interface_readhdrall.f90

  ! DESCRIPTION:

  ! This subroutine reads the user specified BUFR header string(s) and
  ! returns all values for the respective header string(s).

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the user specified BUFR file path.

  ! * bufr; a FORTRAN bufr_struct variable, containing (at minimum)
  !   the header strings to seek (hdstr), the total number of
  !   mnuemonics (mxmn), and the total number of levels (mxlv).

  ! * bufrhdr; a FORTRAN bufrhdr variable.

  ! OUTPUT VARIABLES:

  ! * bufrhdr; a FORTRAN bufrhdr variable containing the values for
  !   the user specified header string(s).

  !-----------------------------------------------------------------------

  subroutine bufrio_interface_readhdrall(filename,bufr,bufrhdr)

    ! Define variables passed to routine

    type(bufr_struct)                                                   :: bufr
    type(bufrhdr_struct)                                                :: bufrhdr
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=8)                                                    :: subset
    real(r_double)                                                      :: bufrtype(bufr%mxmn)
    integer                                                             :: idate
    integer                                                             :: ireadmg
    integer                                                             :: ireadsb
    integer                                                             :: nlev
    integer                                                             :: nrecs

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================
    
    ! Define local variables

    open(unit_in,file=trim(adjustl(filename)),form='unformatted',          &
         & convert='big_endian')
    call openbf(unit_in,'IN',unit_in)
    call readmg(unit_in,subset,bufr%idate,bufr_iret)
    bufr%nrecs = 0
    
    ! Loop through local variable

    msg_report1: do while(ireadmg(unit_in,subset,idate) .eq. 0)

       ! Loop through local variable

       sb_report1: do while(ireadsb(unit_in) .eq. 0)

          ! Define local variables

          call ufbint(unit_in,bufrtype,bufr%mxmn,bufr%mxlv,nlev,           &
               & trim(adjustl(bufr%hdstr)))
          bufr%nrecs = bufr%nrecs + 1
             
       end do sb_report1 ! do while(ireadsb(unit_in) .eq. 0)

    end do msg_report1 ! do while(ireadmg(unit_in,subset,idate)
                       ! .eq. 0)

    ! Define local variables

    call closbf(unit_in)
    bufrhdr%mxmn  = bufr%mxmn
    bufrhdr%nrecs = bufr%nrecs
    call variable_interface_setup_struct(bufrhdr)
    nrecs         = 0
    open(unit_in,file=trim(adjustl(filename)),form='unformatted',          &
         & convert='big_endian')
    call openbf(unit_in,'IN',unit_in)

    ! Loop through local variable

    msg_report2: do while(ireadmg(unit_in,subset,idate) .eq. 0)

       ! Loop through local variable

       sb_report2: do while(ireadsb(unit_in) .eq. 0)

          ! Define local variables

          call ufbint(unit_in,bufrtype,bufr%mxmn,bufr%mxlv,nlev,           &
               & trim(adjustl(bufr%hdstr)))
          nrecs                             = nrecs + 1
          bufrhdr%hdr(1:bufrhdr%mxmn,nrecs) = bufrtype(1:bufr%mxmn)

       end do sb_report2 ! do while(ireadsb(unit_in) .eq. 0)

    end do msg_report2 ! do while(ireadmg(unit_in,subset,idate)
                       ! .eq. 0)

    ! Define local variables

    call closbf(unit_in)
          
    !=====================================================================

  end subroutine bufrio_interface_readhdrall

  !=======================================================================

  ! SUBROUTINE:

  ! bufrio_interface_readhdruniq.f90

  ! DESCRIPTION:

  ! This subroutine reads the user specified BUFR header string(s) and
  ! returns all unique values for the respective header string(s).

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the user specified BUFR file path.

  ! * bufr; a FORTRAN bufr_struct variable, containing (at minimum)
  !   the header strings to seek (hdstr), the total number of
  !   mnuemonics (mxmn), and the total number of levels (mxlv).

  ! * bufrhdr; a FORTRAN bufrhdr variable.

  ! OUTPUT VARIABLES:

  ! * bufrhdr; a FORTRAN bufrhdr variable containing the unique values
  !   for the user specified header string(s).

  !-----------------------------------------------------------------------

  subroutine bufrio_interface_readhdruniq(filename,bufr,bufrhdr)

    ! Define variables passed to routine

    type(bufr_struct)                                                   :: bufr
    type(bufrhdr_struct)                                                :: bufrhdr
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    character(len=8)                                                    :: subset
    real(r_double),             dimension(:),               allocatable :: bufrhdrarr 
    real(r_double)                                                      :: bufrtype(bufr%mxmn)
    integer                                                             :: ireadmg
    integer                                                             :: ireadsb
    integer                                                             :: nlev
    integer                                                             :: nrecs

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================
    
    ! Define local variables

    open(unit_in,file=trim(adjustl(filename)),form='unformatted',          &
         & convert='big_endian')
    call openbf(unit_in,'IN',unit_in)
    nrecs = 0
    
    ! Loop through local variable

    msg_report1: do while(ireadmg(unit_in,subset,bufr%idate) .eq. 0)

       ! Loop through local variable

       sb_report1: do while(ireadsb(unit_in) .eq. 0)

          ! Define local variables

          call ufbint(unit_in,bufrtype,bufr%mxmn,bufr%mxlv,nlev,           &
               & trim(adjustl(bufr%hdstr)))
          nrecs = nrecs + 1
             
       end do sb_report1 ! do while(ireadsb(unit_in) .eq. 0)

    end do msg_report1 ! do while(ireadmg(unit_in,subset,bufr%idate)
                       ! .eq. 0)

    ! Define local variables

    call closbf(unit_in)

    ! Allocate memory for local variables

    if(.not. allocated(bufrhdrarr)) allocate(bufrhdrarr(nrecs))
    
    ! Define local variables

    open(unit_in,file=trim(adjustl(filename)),form='unformatted',          &
         & convert='big_endian')
    call openbf(unit_in,'IN',unit_in)
    bufrhdrarr = dble(spval)
    nrecs      = 0

    ! Loop through local variable

    msg_report2: do while(ireadmg(unit_in,subset,bufr%idate) .eq. 0)

       ! Loop through local variable

       sb_report2: do while(ireadsb(unit_in) .eq. 0)

          ! Define local variables

          call ufbint(unit_in,bufrtype,bufr%mxmn,bufr%mxlv,nlev,           &
               & trim(adjustl(bufr%hdstr)))

          ! Check local variable and proceed accordingly
    
          if(all(bufrhdrarr(1:nrecs) .ne. bufrtype(1))) then
       
             ! Define local variables
       
             nrecs             = nrecs + 1
             bufrhdrarr(nrecs) = bufrtype(1)
       
          end if ! if(any(bufrhdrarr(1:nrecs) .ne. bufrtype(1)))
             
       end do sb_report2 ! do while(ireadsb(unit_in) .eq. 0)

    end do msg_report2 ! do while(ireadmg(unit_in,subset,bufr%idate)
                       ! .eq. 0)

    ! Define local variables

    call closbf(unit_in)
    bufrhdr%mxmn  = 1
    bufrhdr%nrecs = nrecs
    call variable_interface_setup_struct(bufrhdr)

    ! Loop through local variable

    do i = 1, bufrhdr%nrecs

       ! Define local variables

       bufrhdr%hdr(1:bufrhdr%mxmn,i) = bufrhdrarr(i)

    end do ! do i = 1, bufrhdr%nrecs
    
    ! Deallocate memory for local variables

    if(allocated(bufrhdrarr)) deallocate(bufrhdrarr)
       
    !=====================================================================

  end subroutine bufrio_interface_readhdruniq
    
  !=======================================================================

  ! SUBROUTINE:

  ! bufrio_interface_write.f90

  ! DESCRIPTION:

  ! This subroutine writes a BUFR formatted record to an external,
  ! open BUFR file.

  ! INPUT VARIABLES:

  ! * bufr; a FORTRAN bufr_struct variable containing the BUFR
  !   variables describing the respective BUFR record.

  !-----------------------------------------------------------------------

  subroutine bufrio_interface_write(bufr)

    ! Define variables passed to routine

    type(bufr_struct)                                                   :: bufr

    !=====================================================================

    ! Define local variables

    call ufbint(unit_out,bufr%hdr,bufr%mxmn,1,bufr_iret,bufr%hdstr)
    call ufbint(unit_out,bufr%obs,bufr%mxmn,bufr%mxlv,bufr_iret,           &
         & bufr%obstr)
    call ufbint(unit_out,bufr%oer,bufr%mxmn,bufr%mxlv,bufr_iret,           &
         & bufr%oestr)
    call ufbint(unit_out,bufr%qcf,bufr%mxmn,bufr%mxlv,bufr_iret,           &
         & bufr%qcstr)
    call writsb(unit_out)

    !=====================================================================

  end subroutine bufrio_interface_write

  !=======================================================================

  ! SUBROUTINE:

  ! read_bufr_open.f90

  ! DESCRIPTION:

  ! This subroutine opens the user specified BUFR file path and
  ! assigns the open file unit to the local module variable unit_out.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the user specified BUFR file path.

  !-----------------------------------------------------------------------

  subroutine read_bufr_open(filename)

    ! Define variables passed to routine

    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables

    open(unit_in,file=trim(adjustl(filename)),action='read',form=          &
         & 'unformatted',convert='big_endian')

    !=====================================================================
    
  end subroutine read_bufr_open

  !=======================================================================

  ! SUBROUTINE:

  ! read_write_bufr_open.f90

  ! DESCRIPTION:

  ! This subroutine opens the user specified BUFR file path and
  ! assigns the open file unit to the local module variable unit_out.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the user specified BUFR file path.

  !-----------------------------------------------------------------------

  subroutine read_write_bufr_open(filename)

    ! Define variables passed to routine

    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables

    open(unit_in,file=trim(adjustl(filename)),form='unformatted',          &
         & action='readwrite',convert='big_endian')

    !=====================================================================
    
  end subroutine read_write_bufr_open

  !=======================================================================

  ! SUBROUTINE:

  ! write_bufr_tbl.f90

  ! DESCRIPTION:

  ! This subroutine opens the user specified BUFR table and write the
  ! respective BUFR table to the user specified BUFR file path.

  ! INPUT VARIABLES:

  ! * tablename; a FORTRAN character string specifying the full-path
  !   to the file containing the BUFR table.

  !-----------------------------------------------------------------------

  subroutine write_bufr_table(tablename)

    ! Define variables passed to routine

    character(len=500)                                                  :: tablename

    !=====================================================================

    ! Define local variables

    open(unit_tbl,file = trim(adjustl(tablename)),action='read')
    if(bufr_exist)       call openbf(unit_out,'APN',unit_tbl)
    if(.not. bufr_exist) call openbf(unit_out,'OUT',unit_tbl)

    !=====================================================================

  end subroutine write_bufr_table

  !=======================================================================

  ! SUBROUTINE:

  ! write_bufr_open.f90

  ! DESCRIPTION:

  ! This subroutine opens the user specified BUFR file path and
  ! assigns the open file unit to the local module variable unit_out.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the user specified BUFR file path.

  !-----------------------------------------------------------------------

  subroutine write_bufr_open(filename)

    ! Define variables passed to routine

    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables

    open(unit_out,file=trim(adjustl(filename)),action='write',form=        &
         & 'unformatted',convert='big_endian')

    !=====================================================================

  end subroutine write_bufr_open

  !=======================================================================

end module bufrio_interface
