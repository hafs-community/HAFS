!---------------------------------------------------------------------------------------------------------
! This code is designed to replace create_trak_guess.f90 of HWRF: 2022 JungHoon Shin   
! Usage example:
!   ./hafs_vi_create_trak_guess.x storm_id
!   e.g., ./hafs_vi_create_trak_guess.x 13L
! Input:
!   fort.11: tcvitals message for the current forecast cycle
!   fort.12: ATCF track file from previous cycle 6-h HAFS forecast
! Output:
!   fort.30: contains the HAFS storm position at 3-, 6-, and 9-h, needed for splitting or merging HAFS vortex
!
! Output (fort.30: trak.fnl.all) looks like below
! 72HDAS[cycle]  AAA BBB9999999999999999 CCC DDD9999999999999999 EEE FFF   0   0   0   0   0   0 [storm ID]
! AAA&BBB (lat/lon): TC center at 3-h, CCC&DDD (lat/lon): TC center at 6-h, EEE&FFF (lat/lon): TC center at 9-h
! For example, for the cycle 2020082500 of Laura 13L looks like this..
! 72HDAS20082418 217 8199999999999999999 222 8269999999999999999 227 836   0   0   0   0   0   0 13L
!---------------------------------------------------------------------------------------------------------
program create_trak_guess
  implicit none
  character(len=2)      :: part1,num
  character(len=3)      :: storm_id
  character(len=1)      :: ns,ew
  integer               :: idat,ihour,ifh,lat,lon,ih,j,stat
  integer,dimension(7)  :: lathr,lonhr

  integer               :: iargc

  if (iargc() .lt. 1) then
     write(*,*)' usage: ./hafs_vi_create_trak_guess.x storm_id'
     write(*,*)' example: ./hafs_vi_create_trak_guess.x 13L'
     stop
  else
     call getarg(1, storm_id)
  endif

  ! Set null vaules for the output (fort.30)
  lathr=9999
  lonhr=9999

  ! Reading fort.12 (previous HAFS forecast ATCF file)
  do
    read(12,65,iostat=stat) part1,num,idat,ihour,ifh,lat,ns,lon,ew
    if(ns.eq.'S')lat=-lat
    if(ew.eq.'E')lon=3600-lon
    if(ifh.ge.3 .and. ifh.le.9) then ! We only need 3-,6-,9-h HAFS storm postion
      lathr(ifh-2)=lat
      lonhr(ifh-2)=lon
    end if
    !If we find 9-h information or reach the end of file WITHOUT 9-h data, exit the do loop
    if(stat /= 0 .or. ifh.eq.9) exit 
  enddo

  ! If there is no 9-h data in the ATCF file, we need to make sure 6-h position is avaliable in
  ! the previous HAFS forecast ATCF file. But Normally 6-h position exist, so don't worry

  if(stat /= 0) then
    if(lathr(4).eq.9999 .or. lonhr(4).eq.9999) then
      print*,'There is no 6 hour storm position in the previous HAFS cycle'
      stop
    else if(lathr(4).eq.0 .and. lathr(4).eq.0) then
      print*,'There is no 6 hour storm position in the previous HAFS cycle'
      stop
    endif
  endif

  print*, part1,num,idat,ifh,lat,ns,lon,ew,storm_id
  write(30,15) idat,ihour,(lathr(j),lonhr(j),j=1,7),storm_id

  13 format(5x,A3,13x,I6,1x,I2,3x,I3,A1,1x,I4,A1)                 ! Input fort.11 (tcvital) format
  65 format(A2,2x,A2,4x,I6,I2,12x,I3,2x,I3,A1,2x,I4,A1)           ! Input fort.12 (ATCF) format
  15 format('72HDAS',I6,I2,14I4,'   0   0   0   0   0   0',1x,3A) ! Output(trak.fnl.all) format

end program create_trak_guess 

