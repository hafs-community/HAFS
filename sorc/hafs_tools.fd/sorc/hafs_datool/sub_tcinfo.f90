!=======================================================================================
  subroutine get_tc_info(vortex_position_file, tcvital_file, besttrackfile, file_date, &
                         vortexradius)

  use var_type

  implicit none
  character (len=*), intent(in) :: vortex_position_file, tcvital_file, besttrackfile, file_date, &
                                   vortexradius
  character (len=13)            :: time
  real, dimension(4)            :: center  !lat, lon, pmin, vmax
  character (len=300)           :: temp
  integer                       :: i, j, n
  real                          :: xtemp
 
  !---get info from file
  center = -9999999.0
  if ( len_trim(vortex_position_file) > 1 ) then
     !--get user defind vortex center
     call rd_vortex_position(trim(vortex_position_file), center(1:2))
  else
     !--need to get tc info from tcvital or best-track
     if ( len_trim(file_date) < 11 ) then !20210312.09xx
        time='99999999.9999'
     elseif ( len_trim(file_date) >= 11 .and. len_trim(file_date) < 13 ) then
        time=file_date(1:11)//'00'
     else
        time=file_date(1:13)
     endif

     if ( len_trim(tcvital_file) > 1 ) then
        call rd_interp_besttrack(time, trim(tcvital_file), center)
     else if ( len_trim(besttrackfile) > 1 ) then
        call rd_interp_besttrack(time, trim(besttrackfile), center)
     endif 
  endif

  call check_tc_lon_lat(center(1), center(2))
  if ( center(1) >= -85. .and. center(1) <= 85. ) tc%lat = center(1)
  if ( center(2) > -180. .and. center(2) < 360. ) tc%lon = center(2)
  if ( center(3) > 800. .and. center(3) < 1050. ) tc%pmin = center(3)
  if ( center(4) > 0. .and. center(4) < 250. ) tc%vmax = center(4)

  !---get tc%vortexreplace_r
  if ( len_trim(vortexradius) > 3 ) then
     temp=trim(vortexradius)
     n=0
     do i = 1, len_trim(temp)
        if ( temp(i:i) == ':' .or. temp(i:i) == '-' ) then
           temp(i:i) = ' '
           n=n+1
        endif
     enddo
     if ( n > 0 ) then
        read(temp,*)i,j
     else
        read(temp,*)i
        j=i
     endif
     if ( i > 5 .and. i < 8000 ) tc%vortexreplace_r(1) = real(i)
     if ( j > 5 .and. j < 8000 ) tc%vortexreplace_r(2) = real(j)
     if ( tc%vortexreplace_r(1) > 5 .and. tc%vortexreplace_r(1) < 8000 .and. &
          tc%vortexreplace_r(2) > 5 .and. tc%vortexreplace_r(2) < 8000 .and. &
          tc%vortexreplace_r(1) > tc%vortexreplace_r(2) ) then
          xtemp = tc%vortexreplace_r(1)
          tc%vortexreplace_r(1) = tc%vortexreplace_r(2)
          tc%vortexreplace_r(2) = xtemp
     endif
  endif

  write(*,'(a,2f8.2,2x,a)')'---vortex replacement info: ', tc%lon, tc%lat, trim(vortexradius)

  return
  end subroutine get_tc_info 

!=======================================================================================
  subroutine check_tc_lon_lat(lat, lon)

  implicit none
  real, intent(inout)   :: lon, lat
  real                  :: temp1

  if ( ((lat>85. .and. lat<=360.) .or. (lat<-85. .and. lat>= -180.)) .and. (lon>-65. .and. lon<65.) ) then
     temp1 = lon
     lon = lat
     lat = temp1
  endif

  return
  end subroutine check_tc_lon_lat 

!=======================================================================================
  subroutine rd_vortex_position (filename, center )

!-----------------------------------------------------------------------------
! read user-define hurricane track information, i.e., 
! -86.5 23.8
!-----------------------------------------------------------------------------

  implicit none
  character (len=*), intent(in)         :: filename
  real, dimension(2), intent(out)       :: center  !lat, lon 
  real, dimension(2)                    :: dat
  integer                               :: iost

  open(39, file=trim(filename), status='old', form = 'formatted', iostat = iost )
  if( iost .ne. 0 ) then
     write(*,'(a)')'Cannot find the hurricane track file '//trim(filename)
     center = -9999999.0
     return
  endif

  do_get_track_loop : do
     read(39, *, iostat = iost) dat(1:2)  ! lat, lon
     if ( iost .ne. 0 ) exit do_get_track_loop
     center = dat
  end do do_get_track_loop
  close(39)
  write(*,*)'vortex position is:',center

  end subroutine rd_vortex_position
!=======================================================================================

  subroutine rd_interp_besttrack ( times, filename, center )

  implicit none
  character (len=*), intent(in)        :: times  !20210312.0900
  character (len=*), intent(in)         :: filename
  real, dimension(4), intent(out)       :: center

  integer               :: iost, hours_from_date, nrecord
  integer, dimension(3) :: year, month, day, hour, dhour     !1=current, 2=pre, 3=post
  real, dimension(3)    :: lat, lon, wsp, slp
  integer               :: iyear, imonth, iday, ihour, ilat, ilon, iwsp, islp, diff_hour
  real                  :: awsp
  character (len=1)     :: clat, clon
  character (len=3)     :: nhc

  dhour(1:3)=-999999
  read(times, '(i4, i2, i2, 1x, i2)')year(1), month(1), day(1), hour(1)
  dhour(1) = hours_from_date ( year(1), month(1), day(1), hour(1))

  open(39, file=trim(filename), status='old', form = 'formatted', iostat = iost )
  if ( iost .ne. 0 ) then
     center = -9999999.0
     write(*,*)'CANNOT find best-track file!!!!!!'
     return
  end if

  read(39, '(a3)', iostat = iost)nhc
  backspace(39)
  nrecord=0
  do_get_track_loop : do
        if (nhc == "NHC" .or. nhc == "JTW" ) then   !tcvital
           !-----tcvitals
           read(39, '(19x, i4,2i2,1x,i2,2x,i4, a1, i5, a1,9x,i4, 10x, i3)', iostat = iost)  &
                   iyear, imonth, iday, ihour, ilat, clat, ilon, clon, islp, iwsp
        else if (nhc == "AL," .or. nhc == "WP," .or. nhc == "EP" .or. nhc == "IO," .or. nhc == "SH," ) then   ! best track
           !-----Best Track --b-deck
           read(39, '(8x, i4,3i2, 16x, i4, a1, 1x, i5, a1, 1x, i4, 1x, i5)', iostat = iost)  &
                   iyear, imonth, iday, ihour, ilat, clat, ilon, clon, iwsp, islp
        end if
        awsp = iwsp*0.514444
        !awsp = iwsp*1.0
        if( iost .ne. 0 ) exit
        if (clat == 'S') ilat = -ilat
        if (clon == 'W') ilon = -ilon
        if ( year(1) == 9999 .and. month(1) == 99 .and. day(1) == 99 .and. hour(1) == 99 ) then
           !---use the last record
           lat(2) = ilat/10.
           lon(2) = ilon/10.
           wsp(2) = awsp
           slp(2) = islp*1.0
           dhour(2) = 99999
           lat(3) = ilat/10.
           lon(3) = ilon/10.
           wsp(3) = awsp
           slp(3) = islp*1.0
           dhour(3) = 99999
        else
           diff_hour = hours_from_date ( iyear, imonth, iday, ihour)
           if (diff_hour == dhour(1) ) then
              lat(2) = ilat/10.
              lon(2) = ilon/10.
              wsp(2) = awsp
              slp(2) = islp*1.0
              dhour(2) = 99999
              lat(3) = ilat/10.
              lon(3) = ilon/10.
              wsp(3) = awsp
              slp(3) = islp*1.0
              dhour(3) = 99999
              exit do_get_track_loop
           else if (diff_hour < dhour(1) ) then
              nrecord=nrecord+1
              if ( nrecord <= 1 ) then 
                 lat(2) = ilat/10.
                 lon(2) = ilon/10.
                 wsp(2) = awsp
                 slp(2) = islp*1.0
                 dhour(2) = diff_hour
                 lat(3) = ilat/10.
                 lon(3) = ilon/10.
                 wsp(3) = awsp
                 slp(3) = islp*1.0
                 dhour(3) = diff_hour
              else
                 lat(2) = lat(3) 
                 lon(2) = lon(3)
                 wsp(2) = wsp(3)
                 slp(2) = slp(3)
                 dhour(2) = dhour(3)
                 lat(3) = ilat/10.
                 lon(3) = ilon/10.
                 wsp(3) = awsp
                 slp(3) = islp*1.0
                 dhour(3) = diff_hour
              endif
           else if (diff_hour > dhour(1) ) then
              lat(3) = ilat/10.
              lon(3) = ilon/10.
              wsp(3) = awsp
              slp(3) = islp*1.0
              dhour(3) = diff_hour
              exit do_get_track_loop
           end if
        endif
  end do do_get_track_loop
  close(39)

  if (dhour(2) < -9999 .or. dhour(3) < -9999 ) then
     center(1:4) = -999999.0
  else if ( abs(dhour(3)-dhour(2)) < 1 ) then
     center(1) = lat(3)
     center(2) = lon(3)
     center(3) = slp(3)
     center(4) = wsp(3)
  else
     !center(1) = lat(2)+(lat(3)-lat(2))*abs(dhour(2))/(abs(dhour(2))+abs(dhour(3)))
     !center(2) = lon(2)+(lon(3)-lon(2))*abs(dhour(2))/(abs(dhour(2))+abs(dhour(3)))
     !center(3) = slp(2)+(slp(3)-slp(2))*abs(dhour(2))/(abs(dhour(2))+abs(dhour(3)))
     !center(4) = wsp(2)+(wsp(3)-wsp(2))*abs(dhour(2))/(abs(dhour(2))+abs(dhour(3)))
     center(1) = lat(3) - (lat(3)-lat(2))/(dhour(3)-dhour(2))*(dhour(3)-dhour(1))
     center(2) = lon(3) - (lon(3)-lon(2))/(dhour(3)-dhour(2))*(dhour(3)-dhour(1))
     center(3) = slp(3) - (slp(3)-slp(2))/(dhour(3)-dhour(2))*(dhour(3)-dhour(1))
     center(4) = wsp(3) - (wsp(3)-wsp(2))/(dhour(3)-dhour(2))*(dhour(3)-dhour(1))
  end if

  write(*,'(a,4f10.2)')'hurricane center from obs is:',center
  return

  end subroutine rd_interp_besttrack

!==============================================================================
   function hours_from_date(iyear, imonth, iday, ihour)
   integer :: iyear, imonth, iday, ihour
   integer :: hours_from_date
   integer :: y, m, d, h

   m = mod(imonth+9, 12)
   y = iyear - int(m/10)
   hours_from_date = 365*y + int(y/4) + int(y/100) + int(y/400) + int((m*306+5)/10) + iday - 1
   hours_from_date = hours_from_date*24 + ihour
   return
   end function hours_from_date

!=======================================================================================

