PROGRAM TDR_SO_MAIN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! TDR Superob Tool
!
! Written by:
!   Brittany Dahl (Univ. of Miami/CIMAS)
!
! Description:
!   Superobs NOAA aircraft tail Doppler radar (TDR) observations and
!   generates new BUFR file containing superobs
!
! Input arguments:
!   bfrin_filename - Original TDR BUFR file to process
!   bfrso_filename - New TDR BUFR file containing superobs
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

USE TDR_BUFR
USE TDR_COMMON
USE TDR_SUPEROB

IMPLICIT NONE

!------ Variable declarations ------
! File names/units
character(len=120)            :: bfrin_filename       ! BUFR file to superobs
character(len=120)            :: bfrso_filename       ! New BUFR file containing superobs
character(len=120)            :: decoded_filename     ! ASCII file containing decoded BUFR obs
character(len=120)            :: asciiso_filename     ! ASCII file containing all superobs from all available aircraft
character(len=11)             :: nml_filename         ! Namelist
character(len=11)             :: bfrtbl_filename      ! BUFR table
integer                       :: bfrin_unit, bfrso_unit, decoded_unit, asciiso_unit, nml_unit, bfrtbl_unit
real                          :: start_time, end_time ! To check code timing

! NOAA TDR aircraft list
character(len=2),dimension(3) :: tdr_aircraft  ! Tail no. of all NOAA aircraft w/ TDR (P-3 = 42 and 43, G-IV = 49)

! Debug setting
! 0=quiet, 1=ASCII files for each sweep, 2=sweep ASCII + extra debug files in calc_superobs
integer                       :: debug_level
namelist /debug/ debug_level

! Misc.
integer                       :: cmd_count
integer                       :: i, iost
logical                       :: fileexists
!------ End variable declarations ------

! Set constants
nml_filename     = "tdr_so.nml"
bfrtbl_filename  = "tdr_obs.tbl"
asciiso_filename = "asciiso.txt"
bfrin_unit       = 10
decoded_unit     = 11
bfrso_unit       = 12
nml_unit         = 13
bfrtbl_unit      = 14
asciiso_unit     = 15

write(6,*) '----------------------'
write(6,*) 'TDR SUPEROB TOOL BEGIN'
write(6,*) '----------------------'
write(6,*) ' '

! Get names of BUFR file to superob and new superobbed BUFR file
cmd_count = command_argument_count()
if (cmd_count .ne. 2) then
  write(6,*) 'ERROR: Incorrect number of arguments (',cmd_count,')'
  write(6,*) 'Usage: ./tdr_superob_tool.exe name_of_incoming_bufr name_of_new_bufr'
  write(6,*) 'Stopping...'
  stop
endif
call get_command_argument(1, bfrin_filename)
call get_command_argument(2, bfrso_filename)

! Get debug setting from namelist
call check_file_missing('main',nml_filename)
open(unit=nml_unit, file=nml_filename, status='old', access='sequential', iostat=iost)
call check_iostat_open('main',nml_filename,iost)
read(unit=nml_unit, nml=debug, iostat=iost)
call check_iostat_read('main',nml_filename,iost,.true.)
close(nml_unit)
write(6,*) 'Info (main): debug_level = ',debug_level
write(6,*) ' '

! Get BUFR table from incoming BUFR file to use for encoding superobs
call get_bufr_tbl(bfrin_filename, bfrtbl_filename)
call check_file_missing('main',bfrtbl_filename)

! Check for existing ASCII superob file and delete
call check_delete_existing('main',asciiso_filename)

! Define list of all possible aircraft to process
tdr_aircraft = ["42", "43", "49"]

! Process one aircraft at a time
aircraft_loop: do i = 1, size(tdr_aircraft)

  write(6,*) ' '
  write(6,*) 'PROCESSING NOAA',tdr_aircraft(i)
  write(6,*) ' '

  ! Decode BUFR data for current aircraft
  decoded_filename = 'decoded.'//tdr_aircraft(i)//'.txt'
  call check_delete_existing('main',decoded_filename)
  call cpu_time(start_time)
  call bufr_decode_tdr(bfrin_filename,tdr_aircraft(i),decoded_filename)
  call cpu_time(end_time)
  write(6,*) 'Time (s) to run bufr_decode_tdr = ',end_time-start_time
  write(6,*) ' '

  ! Split TDR data into individual sweeps and calculate superobs
  inquire(file=decoded_filename, exist=fileexists)
  if (fileexists) then
    call cpu_time(start_time)
    call split_bufr_sweeps(decoded_filename,asciiso_filename,debug_level)
    call cpu_time(end_time)
    write(6,*) 'Time (s) to run split_bufr_sweeps = ', end_time-start_time
    write(6,*) ' '
  else
    write(6,*) '--> Decoded BUFR file ',trim(decoded_filename),' not generated. Skipping aircraft NOAA', &
      tdr_aircraft(i),'...'
    write(6,*) ' '
  endif

enddo aircraft_loop

! Make sure all expected files were created
call check_file_missing('main',asciiso_filename)
call check_file_missing('main',bfrtbl_filename)

! Convert ASCII superobs to BUFR format
call bufr_encode_tdr(asciiso_filename, bfrso_filename, bfrtbl_filename)

write(6,*) ' '
write(6,*) '----------------------'
write(6,*) 'TDR SUPEROB TOOL END'
write(6,*) '----------------------'
write(6,*) ' '

END PROGRAM TDR_SO_MAIN
