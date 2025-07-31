MODULE TDR_BUFR

USE TDR_COMMON

IMPLICIT NONE

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE get_bufr_tbl(bfrin_filename,bfrtbl_filename)
!
! Description:
!     Dumps BUFR table from incoming BUFR file
!
! Input:
!     bfrin_filename  - name of BUFR file to read
!     bfrtbl_filename - name of new file containing dumped BUFR table
!
implicit none

!------ Variable declarations ------
integer                       :: bfrin_unit, bfrtbl_unit, iost
character(120), intent(in)    :: bfrin_filename
character(len=11), intent(in) :: bfrtbl_filename
!------ End variable declarations ------

bfrin_unit = 11
bfrtbl_unit = 13

open(unit=bfrin_unit, file=trim(bfrin_filename), form='unformatted', status='old', iostat=iost)
call check_iostat_open('get_bufr_tbl',bfrin_filename,iost)
call openbf(bfrin_unit, 'IN', bfrin_unit)

open(unit=bfrtbl_unit, file=bfrtbl_filename, status='replace')
call check_iostat_open('get_bufr_tbl',bfrtbl_filename,iost)

call dxdump(bfrin_unit, bfrtbl_unit)

call closbf(bfrin_unit)
close(unit=bfrtbl_unit)
close(unit=bfrin_unit)

END SUBROUTINE get_bufr_tbl

!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE bufr_decode_tdr(bfrin_filename,target_tailno,ascii_filename)
!
! Description:
!     Reads tail Doppler radar BUFR file
!
! Input:
!     bfrin_filename - BUFR file to read
!     target_tailno  - NOAA aircraft tail number to process
!     ascii_filename - ASCII file for decoded BUFR output
!
implicit none

!------ Variable declarations ------
!
! File names/handles
character(120),intent(in)    :: bfrin_filename
character(120),intent(in)    :: ascii_filename
integer                      :: lnbufr
integer                      :: lnascii
integer                      :: iost
!
! BUFR-related variables
character(80)                   :: hdrstr(2), datstr(1)
character(8)                    :: subset
character(2),intent(in)         :: target_tailno
character(8)                    :: this_tailno
integer                                        :: iret, maxlevs, levs, irec, nmrecs
integer                                        :: idate
double precision                               :: iyr, imo, idy, ihr, imn, isc, obdate
double precision, dimension(12)                :: hdr
double precision, dimension(4,1500)            :: tdr_obs
real              :: this_azimuth, this_tilt
real              :: this_stalat, this_stalon, this_stahgt
real              :: vr_missing
!
integer           :: k
!
!------ End variable declarations ------

! Define file units
lnbufr = 10
lnascii = 11

! Define BUFR header and data strings
data hdrstr(1) / 'PTID YEAR MNTH DAYS HOUR MINU SECO CLAT CLON FLVLST ANAZ ANEL' /
data hdrstr(2) / 'ACRN' /
data datstr(1) / 'DIST HREF DMVR DVSW' /

! Initialize record counter
maxlevs = 4000
vr_missing  = -888.
nmrecs=0
irec = 0

! Open BUFR file and begin reading
write(6,*) 'Decoding BUFR file ',trim(bfrin_filename)
open(lnbufr,file=trim(bfrin_filename),form='unformatted',iostat=iost)
call check_iostat_open('bufr_decode_tdr',bfrin_filename,iost)

call openbf(lnbufr,'IN',lnbufr)

call datelen(10)

call readmg(lnbufr,subset,idate,iret)
if(iret .eq. 0) then
  !write(6,*) 'subset, idate, iret = ',subset,idate,iret
  read_bufr_loop: do

    ! Read BUFR subset
    call readsb(lnbufr,iret)
    !write(6,*) 'readsb iret=',iret
    if(iret .ne. 0) then
      call readmg(lnbufr,subset,idate,iret)
      if(iret .ne. 0) exit read_bufr_loop
      cycle read_bufr_loop
    endif

    ! Read header and extract station information
    call ufbint(lnbufr,hdr,12,1,iret,hdrstr(1))

    iyr = hdr(2)
    imo = hdr(3)
    idy = hdr(4)
    ihr = hdr(5)
    imn = hdr(6)
    isc = hdr(7)
    obdate = iyr*10000000000. + imo*100000000. + idy*1000000. + ihr*10000. + imn*100. + isc

    this_stalat = hdr(8)
    this_stalon = hdr(9)
    this_stahgt = hdr(10)
    this_azimuth = hdr(11)
    this_tilt = hdr(12)

    ! Go through the data levels
    call ufbint(lnbufr,tdr_obs,4,maxlevs,levs,datstr(1))
    do k = 1,levs
      ! Replace 10.E10 missing value in data array with -888.88
      if (tdr_obs(3,k) .ge. 10.E10) then
        tdr_obs(3,k) = vr_missing
      endif

    enddo

    ! Get aircraft tail number and save data
    call ufbint(lnbufr,this_tailno,1,1,iret,hdrstr(2))
    if (this_tailno .eq. target_tailno) then

      ! Add one to BUFR record counters
      nmrecs = nmrecs+1
      irec = irec+1

      if (irec .eq. 1) then
        open(unit=lnascii, file=trim(ascii_filename), status='replace', action='write', iostat=iost)
        call check_iostat_open('bufr_decode_tdr',ascii_filename,iost)
        !if (iost /= 0) then
        !  write(6,*) 'ERROR (bufr_decode_tdr): Could not open file ',trim(ascii_filename),' iostat=',iost
        !  stop
        !else
        write(lnascii,'(I3)',iostat=iost) levs
        call check_iostat_write('bufr_decode_tdr', ascii_filename, iost)
        write(lnascii,'(8(F10.2,1X))',iostat=iost) (tdr_obs(1,k),k=1,levs) ! assume no change in radius for same aircraft
        call check_iostat_write('bufr_decode_tdr', ascii_filename, iost)
        !endif
      endif ! if (irec .eq 1)

      write(lnascii,'(A2,1X,F15.0,1X,2(F6.2,1X),(F7.1,1X),2(F6.2,1X),100(F7.2,1X))',iostat=iost) trim(this_tailno),&
        obdate,this_stalat,this_stalon,this_stahgt,this_azimuth,this_tilt,(tdr_obs(3,k),k=1,levs)
      call check_iostat_write('bufr_decode_tdr', ascii_filename, iost)

    endif ! if (this_tailno .eq. target_tailno)

  enddo read_bufr_loop

  close(lnascii)

else
  write(6,*) '-- ERROR (bufr_decode_tdr): Could not read BUFR message from file ',trim(bfrin_filename)
  stop

endif ! if (iret .eq. 0)

call closbf(lnbufr)
close(lnbufr)

END SUBROUTINE bufr_decode_tdr

!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE bufr_encode_tdr(soascii_filename,bfrout_filename,bfrtbl_filename)
!
! Description:
!     Encode ASCII tail Doppler radar superobs into BUFR file
!
! Input:
!   ascii_filename - ASCII file with superobs
!   bfrout_filename - new BUFR file containing superobs
!   bfrtbl_filename - BUFR table to use for encoding
!
implicit none

!--- Variable Declarations ---
!
! File names/handles
character(120), intent(in) :: soascii_filename, bfrout_filename
character(11), intent(in)  :: bfrtbl_filename
integer                    :: unit_sobfr
integer                    :: unit_bfrtbl
integer                    :: unit_soascii
!
! BUFR variables
character(8)          :: subset
integer               :: mxmn, mxlv, nlv
double precision, allocatable, dimension(:,:) :: r8arr
integer               :: iost                          ! iostat
integer               :: obs_read, obs_written
integer               :: idate
integer, dimension(6) :: sodate                        ! YYYY MM DD HH mm SS date elements
real, dimension(3)    :: rpos                          ! lat lon alt of radar
real                  :: eazm, eev, radius, theta      ! ob position
real                  :: rv!, rv_rms, refl             ! ob and ob stats
integer               :: acrn                          ! aircraft tail number (42/43 for P-3, 49 for G-IV)
integer               :: ptid                          ! platform id (3 = P-3, 2 = G-IV)
!
!------ End variable declarations ------

write(6,*) ' '
write(6,*) 'BUFR_ENCODE_TDR: Converting ',trim(soascii_filename),' to ',trim(bfrout_filename)
write(6,*) ' '

! Set constants
subset = 'NC006070' ! for TDR
unit_sobfr   = 11
unit_bfrtbl  = 12
unit_soascii = 13
mxmn = 15
mxlv = 255
allocate(r8arr(mxmn, mxlv))

! Open new so bufr file and existing tdr bufr table, associate them with bufrlib
open(unit = unit_sobfr, file = bfrout_filename, status = 'replace', iostat=iost)
call check_iostat_open('bufr_encode_tdr',bfrout_filename,iost)
!call check_iostat(iost)
open(unit = unit_bfrtbl, file = bfrtbl_filename, status = 'old', iostat=iost)
call check_iostat_open('bufr_encode_tdr',bfrtbl_filename,iost)

call datelen (10)
call openbf(unit_sobfr, 'OUT', unit_bfrtbl)

! Open ASCII so file (filename passed in from command line
open(unit=unit_soascii, file=trim(soascii_filename), status='old', action='read', iostat=iost)
call check_iostat_open('bufr_encode_tdr',soascii_filename,iost)
!call check_iostat(iost)

! Read obs from so ascii file and load into bufr file
obs_read = 0
obs_written = 0

do while (iost .eq. 0)
  read (unit=unit_soascii, fmt='(I2,1X,I4,5I2,1X,8(f12.3))', iostat=iost) acrn, sodate(1:6), rpos(1:3), &
    eazm, eev, radius, rv, theta !, refl
  call check_iostat_read('bufr_encode_tdr',soascii_filename,iost,.false.)
  !call check_iostat(iost)

  if (eazm < 0) then
    eazm = eazm + 360.
    !print *, 'eazm_converted:',eazm
  endif

  if ((acrn == 42) .or. (acrn == 43)) then
    ptid = 3
  elseif (acrn == 49) then
    ptid = 2
  else
    write(6,*) '-- ERROR (bufr_encode_tdr): Encountered unknown aircraft tail number ',acrn,'. Stopping...'
    stop
  endif

  obs_read = obs_read + 1
  !print *, 'On ob #',obs_read

  ! Construct message date (down to hours)
  idate = (sodate(1) * 1000000) + (sodate(2) * 10000) + (sodate(3) * 100) + sodate(4)

  ! Open a report for TDR subset (NC006070) at idate time
  call openmb(unit_sobfr, subset, idate)

    ! Platform/aircraft/storm info
    !r8arr (1,1) = acid   ! | ACID     | 001006 | AIRCRAFT FLIGHT NUMBER
    r8arr(1,1) = acrn     ! | ACRN     | 001008 | AIRCRAFT REGISTRATION (TAIL) NUMBER
    !r8arr (3,1) = stmid  ! | STMID    | 001025 | STORM IDENTIFIER
    r8arr(2,1) = ptid     ! | PTID     | 001050 | PLATFORM TRANSMITTER ID  NUMBER (ANTENNA NUMBER)
    call ufbint(unit_sobfr, r8arr(1:2,1), mxmn, 1, nlv, 'ACRN PTID')

    ! Date of report
    r8arr(1,1) = sodate(1)  ! YEAR
    r8arr(2,1) = sodate(2)  ! MNTH
    r8arr(3,1) = sodate(3)  ! DAYS
    call ufbseq(unit_sobfr, r8arr(1:3,1), mxmn, 1, nlv, 'YYMMDD')  ! | YYMMDD   | YEAR  MNTH  DAYS

    ! Time of report
    r8arr(1,1) = sodate(4)  ! HOUR
    r8arr(2,1) = sodate(5)  ! MINU
    r8arr(3,1) = sodate(6)  ! SECO
    call ufbseq(unit_sobfr, r8arr(1:3,1), mxmn, 1, nlv, 'HHMMSS')  ! | HHMMSS   | HOUR  MINU  SECO

    ! Radar location
    r8arr(1,1) = rpos(1) ! CLAT (deg)
    r8arr(2,1) = rpos(2) ! CLON (deg)
    call ufbseq(unit_sobfr, r8arr(1:2,1), mxmn, 1, nlv, 'LTLONC')  ! | LTLONC   | CLAT  CLON
    r8arr(1,1) = rpos(3) ! FLVLST (m)
    call ufbint(unit_sobfr, r8arr(1,1), mxmn, 1, nlv, 'FLVLST')  ! | FLVLST   | 007010 | FLIGHT LEVEL (m)

    ! Ob position
    r8arr(1,1) = eazm   ! ANAZ (deg)
    r8arr(2,1) = eev   !  ANEL (deg)
    call ufbint(unit_sobfr, r8arr(1,1), mxmn, 1, nlv, 'ANAZ ANEL')

    ! Observations
    !| DIST     | 006021 | DISTANCE (FROM ANTENNA TO GATE CENTER) ()
    !| HREF     | 021001 | HORIZONTAL REFLECTIVITY
    !| DMVR     | 021014 | DOPPLER MEAN RADIAL VELOCITY
    !| DVSW     | 021017 | DOPPLER VELOCITY SPECTRAL WIDTH
    r8arr(1,1) = radius*1000.   ! DIST (m)
    r8arr(2,1) = rv             ! DMVR (m/s)
    !r8arr(2,1) = refl           ! HREF
    !r8arr(3,1) = rv             ! DMVR
    !call ufbint (unit_sobfr, r8arr(1:2,1), mxmn, 1, nlv, 'DIST HREF DMVR')  ! | NP3RW    | DIST  HREF  DMVR  DVSW
    call ufbint(unit_sobfr, r8arr(1:2,1), mxmn, 1, nlv, 'DIST DMVR')

    ! Encode subset
    call writsb(unit_sobfr)

  ! Close report
  call closmg(unit_sobfr)

  obs_written = obs_written + 1

enddo

! Close out files
call closbf(unit_sobfr)
close(unit_sobfr)
close(unit_bfrtbl)

write(6,*) '-- Number superobs read from ASCII: ',obs_read
write(6,*) '-- Number superobs written to BUFR: ',obs_written

END SUBROUTINE bufr_encode_tdr

END MODULE TDR_BUFR
