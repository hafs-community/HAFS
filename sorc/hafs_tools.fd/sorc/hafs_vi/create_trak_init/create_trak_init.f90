!---------------------------------------------------------------------------------------------------------
! This code is designed to replace create_trak_fnl.f90 of HWRF: 2022 JungHoon Shin
! Usage example:
!   ./hafs_vi_create_trak_init.x storm_id
!   e.g., ./hafs_vi_create_trak_init.x 13L
! Input:
!   fort.11: tcvitals message for the current forecast cycle
!   fort.12: ATCF track file tracked from GFS and GDAS at FGAT time
! Output:
!   fort.30: contains TC center of GFS and GDAS at FGAT time, needed for splitting or merging GFS/GDAS vortex
!
! Output (fort.30) has the format as shown in below
! AAA&BBB (lat/lon) is the TC center from atcfunix file. It REPEATS same storm center information
! 72HDAS[cycle]  AAA BBB AAA BBB AAA BBB AAA BBB AAA BBB AAA BBB AAA BBB 0   0   0   0   0   0 [storm ID]
! For example, for the Laura 13L at 2020081918 at FGAT-3 looks like this..
! 72HDAS20081915 124 443 124 443 124 443 124 443 124 443 124 443 124 443 0   0   0   0   0   0 13L
!---------------------------------------------------------------------------------------------------------
program create_trak_init
  implicit none
  character(len=2)      :: part1,num,basin
  character(len=3)      :: part2,storm_id
  character(len=1)      :: ns,ew
  integer               :: ihour,idat,ifh,lat,lon,stat

  integer               :: iargc

  if (iargc() .lt. 1) then
     write(*,*)' usage: ./hafs_vi_create_trak_init.x storm_id'
     write(*,*)' example: ./hafs_vi_create_trak_init.x 13L'
     stop
  else
     call getarg(1, storm_id)
  endif

  if(storm_id(3:3).eq.'L') basin='AL'
  if(storm_id(3:3).eq.'W') basin='WP'
  if(storm_id(3:3).eq.'E') basin='EP'
  if(storm_id(3:3).eq.'C') basin='CP'
  if(storm_id(3:3).eq.'A') basin='AA'
  if(storm_id(3:3).eq.'B') basin='BB'
  if(storm_id(3:3).eq.'P') basin='SP'
  if(storm_id(3:3).eq.'S') basin='SI'

  ! Set null vaules for the output (fort.30)
  lat=9999
  lon=9999
  ifh=-1

  ! Reading fort.12 (atcfunix: GFS/GDAS vortex center)
  do
    read(12,65,iostat=stat) part1,num,idat,ihour,ifh,lat,ns,lon,ew
    if(ns.eq.'S')lat=-lat
    ! If we find the correct information, finish the do loop, with stat=0
    if(part1.eq.basin .and. num.eq.storm_id(1:2) .and. ifh.eq.0) exit
    ! exit the do loop if stat is NOT 0
    if( stat /= 0 ) exit
  enddo

  ! Checking the status of fort.12.
  ! If lat & lon values are good (i.e., stat=0), which is normal case, we use the fort.12 as is.
  ! If lat & lon are odd in the fort.12 (as shown in below if statement),
  ! OR fort.12 (atcfunix) does not have correct information (i.e., stat is NOT 0)
  ! this code read and output lat & lon values from tcvitals (fort.11) for trak.fnl.all_gfs (fort.30).
  ! But don't worry. Normally fort.12 are good & below part will not be used in most or normal case

  if(abs(lat).eq.9999 .or. abs(lon).eq.9999 .or. stat /= 0) then
    if( stat /= 0 )then
      print*, 'Using tcvital information because atcfunix does not have correct information'
    else
      print*, 'Using tcvital information because lat and lon values of atcfunix is odd'
    endif
    read(11,13) part2,idat,ihour,lat,ns,lon,ew
    if(ns.eq.'S')lat=-lat
  endif

  write(*,15) idat,ihour,lat,lon,lat,lon,lat,lon,lat,lon,lat,lon,lat,lon,lat,lon,storm_id
  write(30,15) idat,ihour,lat,lon,lat,lon,lat,lon,lat,lon,lat,lon,lat,lon,lat,lon,storm_id

  13 format(5x,A3,13x,I6,1x,I2,3x,I3,A1,1x,I4,A1)                 ! Input fort.11(tcvital) format
  65 format(A2,2x,A2,4x,I6,I2,12x,I3,2x,I3,A1,2x,I4,A1)           ! Input fort.12(ATCF) format
  15 format('72HDAS',I6,I2,14I5,'   0   0   0   0   0   0',1x,3A) ! Output(trak.fnl.all_gfs) format

end program create_trak_init
