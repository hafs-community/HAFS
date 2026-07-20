program track_file
!--------------------------------------------------------------
! This fortran90 program reads a best trak file (or any atcfunix 
! format files including adecks and bdecks and reformats 
! it for a track file to be plotted on the HWRF movie plots.
! It also removes lines with same date and storm position, but
! wind speed at different distance from center of storm  
! YOU MUST SPECIFY VALUES BETWEEN **** LINES IN PROGRAM 
!*******************
!   unit 10 input file name in open statement, atcf_unix file , 
!           example bep172006.dat trak.hwrf.atcfunix.2005082612
!   unit 90 output filename in open statement, reformatted data,
!           example PAUL.track 
!   the first line of the output file 
! These are indicated in program between lines 
!*******************
!--------------------------------------------------------------
implicit none
character*1 EW,SN
character*2 DD,HH
character*10 YYYYMMDDHH,CHECK_DATE
integer LAT,LON,WSPD,PMIN,IRAD34,IFHR,kstats_6
real alat,alon,hour,psmin,wspd10 
!----------------------------------------------
!**************************************************
! specify input and output file names and header
! open the best track file 
open(unit=10,form='formatted',status='old', &
             file='hwrf.atcfunix') 
! open the output file to be written to
open(unit=90,form='formatted',status='unknown', &
             file='hwrf.stats')  
!
! specify the first line header of output file 
! write the first line header of the output file 
!write(90,'(a18)') '2004090218 OBS 09L' 
kstats_6=90
!*************************************************
! read and process the first line of the best track file
read(10,'(8x,a10,11x,i4,1x,i4,a1,1x,i5,a1,1x,i4,1x,i5,5x,i4)',end=99)   &
!     YYYYMMDDHH,IFHR,LAT,LON, WSPD,PMIN,IRAD34
     YYYYMMDDHH,IFHR,LAT,SN,LON,EW, WSPD,PMIN,IRAD34
print *, YYYYMMDDHH,IFHR,LAT,SN,LON,EW, WSPD,PMIN,IRAD34
CHECK_DATE=YYYYMMDDHH 
DD=YYYYMMDDHH(7:8)
HH=YYYYMMDDHH(9:10) 
! convert integer LAT,LON to real alat,alon with west lon negative 
alat=float(LAT)/10.0
if( SN == "S") alat=-float(LAT)/10.0
alon=-float(LON)/10.0 
if( EW == "E" ) alon=float(LON)/10.0

!!write(90,'(f5.1,1x,f6.1,2x,a2,2x,a2,4i5)') alat,alon,HH,DD,IFHR,WSPD,PMIN,IRAD34
print *, alat,alon,HH,DD,IFHR,WSPD,PMIN,IRAD34

          hour=float(IFHR)
          psmin=float(pmin)
          wspd10=float(WSPD)
          write (kstats_6,390) hour, alon,   alat, psmin, wspd10

390   format('HOUR:',F5.1,'  LONG:',F8.2,'  LAT:',F7.2,         &
       '  MIN PRESS (hPa): ',F7.2,'   MAX SURF WIND (KNOTS):',F6.2)
!
! This DO forever loop reads from line 2 to end of best trak file,
! If date is same as previous line it is skipped,
! if date is new the line is reformatted and written to output
! This removes lines with same date and position but diffenent speeds
!
do 
!! read(10,'(8x,a10,16x,i4,2x,i5)',end=99) YYYYMMDDHH,LAT,LON
!read(10,'(8x,a10,11x,i4,1x,i4,2x,i5,2x,i4,1x,i5,5x,i4)',end=99)   &
!     YYYYMMDDHH,IFHR,LAT,LON, WSPD,PMIN,IRAD34
read(10,'(8x,a10,11x,i4,1x,i4,a1,1x,i5,a1,1x,i4,1x,i5,5x,i4)',end=99)   &
     YYYYMMDDHH,IFHR,LAT,SN,LON,EW, WSPD,PMIN,IRAD34
print *, YYYYMMDDHH,IFHR,LAT,SN,LON,EW, WSPD,PMIN,IRAD34
!
 if(IRAD34.ne.34) then  
!   it's a duplicate time, do not save 
    write(6,'(a20,a10)') 'reject duplicate ',YYYYMMDDHH
 else 
!   it's not a duplicate, 
!   reformat and write to output file and reset CHECK_DATE 
    DD=YYYYMMDDHH(7:8)
    HH=YYYYMMDDHH(9:10) 
!   convert integer LAT,LON to real alat,alon with west lon negative 
alat=float(LAT)/10.0
if( SN == "S") alat=-float(LAT)/10.0
 write(*,*)'SN=,alat=',SN,alat
alon=-float(LON)/10.0 
if( EW == "E" ) alon=float(LON)/10.0
!!    write(90,'(f5.1,1x,f6.1,2x,a2,2x,a2)') alat,alon,HH,DD
!!write(90,'(f5.1,1x,f6.1,2x,a2,2x,a2,4i5)') alat,alon,HH,DD,IFHR,WSPD,PMIN,IRAD34
          hour=float(IFHR)
          psmin=float(pmin)
          wspd10=float(WSPD)
          write (kstats_6,390) hour, alon,   alat, psmin, wspd10
    CHECK_DATE=YYYYMMDDHH
 endif 
!
enddo 
99 continue
!----------------------------------------------
close(90)
close(10) 
end program track_file
