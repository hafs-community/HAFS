!-----------------------------------------------------------------------+
  function date2second1970 (yyyy, mm, dd, hh, minute, second) !result(second1970)
  implicit none

  integer,intent(in) :: yyyy, mm, dd, hh, minute, second
  real*8             :: date2second1970

  integer :: julian, julian_1970_01_01
  julian_1970_01_01 = 2440588

  julian = dd -32075 + 1461*(yyyy + 4800 + (mm - 14)/12)/4 + &
               367*(mm - 2 - ((mm - 14)/12)*12)/12 - &
               3*((yyyy + 4900 + (mm - 14)/12)/100)/4
  !write(*,*)'julian =',julian

  date2second1970 = (julian-julian_1970_01_01)*3600*24 + hh*3600 + minute*60 + second
  return
  end function date2second1970

!-----------------------------------------------------------------------+
  function date2second1970_str(cdate) 
  implicit none

  character (len=*),intent(in) :: cdate
  real*8                       :: date2second1970_str

  integer            :: julian, julian_1970_01_01
  integer            :: yyyy, mm, dd, hh, minute, second
  julian_1970_01_01 = 2440588
 
  read(cdate,'(i4,5i2)')yyyy, mm, dd, hh, minute, second
  
  julian = dd -32075 + 1461*(yyyy + 4800 + (mm - 14)/12)/4 + &
               367*(mm - 2 - ((mm - 14)/12)*12)/12 - &
               3*((yyyy + 4900 + (mm - 14)/12)/100)/4
  !write(*,*)'julian =',julian

  date2second1970_str = (julian-julian_1970_01_01)*3600*24 + hh*3600 + minute*60 + second
  return
  end function date2second1970_str

!-----------------------------------------------------------------------+
  subroutine second19702date(second1970, hdate, formatstr)

  !---from http://fortranwiki.org/fortran/show/m_time

  implicit none
  real*8,intent(in) :: second1970
  character(len=*),intent(in)  :: formatstr
  character(len=*),intent(out) :: hdate  !YYYYMMDDHHmmss

  integer :: year, month, day, hour, minute, second

  real*8   :: julian
  real    :: secday
  integer :: ijul, julian_1970_01_01, jalpha,ja,jb,jc,jd,je

  secday = 24.0*3600.
  julian_1970_01_01 = 2440588

  julian = dble(second1970/secday)+dble(julian_1970_01_01)
  ijul = int(julian)

  second = int((julian-ijul)*secday+0.5)
  minute=int(second/60.0)                      ! Integral minutes from beginning of day
  second=second-minute*60                      ! Seconds from beginning of minute
  hour=minute/60                               ! Integral hours from beginning of day
  minute=minute-hour*60                        ! Integral minutes from beginning of hour

  !---------------------------------------------
  jalpha=idint((dble(ijul-1867216)-0.25d0)/36524.25d0) ! Correction for Gregorian Calendar
  ja=ijul+1+jalpha-idint(0.25d0*dble(jalpha))
  !---------------------------------------------

  jb=ja+1524
  jc=idint(6680.d0+(dble(jb-2439870)-122.1d0)/365.25d0)
  jd=365*jc+idint(0.25d0*dble(jc))
  je=idint(dble(jb-jd)/30.6001d0)
  day=jb-jd-idint(30.6001d0*dble(je))
  month=je-1

  if(month.gt.12)then
     month=month-12
  endif

  year=jc-4715
  if(month.gt.2)then
     year=year-1
  endif

  if(year.le.0)then
     year=year-1
  endif

  if ( trim(formatstr) == "YYYYMMDDHHmmss" ) then
     write(hdate,'(i4.4,5i2.2)')year, month, day, hour, minute, second
  else if ( trim(formatstr) == "YYYY-MM-DD hh:mm:ss" ) then
     write(hdate,'(i4.4,5(a1,i2.2))')year, '-', month, '-', day, ' ', hour, ':', minute, ':', second
  else if ( trim(formatstr) == "mm/dd/yyyy hh:mm") then
     write(hdate,'(i,a1,i,a1,i,a1,i,a3)')month,'/',day,'/',year,' ',hour,':',minute
  endif

  return
  end subroutine second19702date

!-----------------------------------------------------------------------+
  real function earth_dist(lon1, lat1, lon2, lat2)
!   earth_dist - function that calculates the earth distance between
!                two points.
!    lon1 - longitude of point 1
!    lat1 - latitude of point 1
!    lon2 - longitude of point 2
!    lat2 - latitude of point 2

  real,intent(in) :: lon1, lat1, lon2, lat2
  real :: pro
  real, parameter :: R_earth = 6374.*1e3, pid=3.14159/180.

  pro = sin(lat1*pid) * sin(lat2*pid) +           &
        cos(lat1*pid) * cos(lat2*pid) *            &
        cos((lon2-lon1)*pid)

  if ( abs(pro) .gt. 1.001 ) then
     print*,'In earth_dist, pro > 1, returning',pro
     earth_dist = -9999999.
  elseif ( abs(pro) .gt. 1 ) then
     earth_dist = 0.
  else
     earth_dist = R_earth * acos(pro)
  endif

  return
  end function


!-----------------------------------------------------------------------+
  integer function binarysearch(length, array, value, delta)
  ! Given an array and a value, returns the index of the element that
  ! is closest to, but less than, the given value.
  ! Uses a binary search algorithm.
  ! "delta" is the tolerance used to determine if two values are equal
  ! if ( abs(x1 - x2) <= delta) then
  !    assume x1 = x2
  ! endif

  implicit none
  integer, intent(in) :: length
  real, dimension(length), intent(in) :: array
  !f2py depend(length) array
  real, intent(in) :: value
  real, intent(in), optional :: delta

  integer :: left, middle, right
  real :: d

  if (present(delta) .eqv. .true.) then
      d = delta
  else
      d = 1e-9
  endif

  if ( array(1) < array(length) ) then
     left = 1
     right = length
     do
         if (left > right) then
             exit
         endif
         middle = nint((left+right) / 2.0)
         if ( abs(array(middle) - value) <= d) then
             binarySearch = middle
             return
         else if (array(middle) > value) then
             right = middle - 1
         else
             left = middle + 1
         end if
     end do
     binarysearch = right
  else
     right = 1
     left = length
     do
       if (right > left) then
           exit
       endif
       middle = nint((left+right) / 2.0)
       if ( abs(array(middle) - value) <= d) then
           binarySearch = middle
           return
       else if (array(middle) > value) then
            right = middle + 1
         else
             left = middle -1
         end if
     end do
     binarysearch = left
  endif

  end function binarysearch

!-----------------------------------------------------------------------+
  integer function beaufortscale(windspeed)

  real, intent(in) :: windspeed   !m/s
  ! ref to: https://www.wikiwand.com/zh-cn/%E8%92%B2%E7%A6%8F%E6%B0%8F%E9%A2%A8%E7%B4%9A
  !         https://www.skybrary.aero/index.php/Beaufort_wind_force_scale

  beaufortscale = -99
  if ( windspeed <= 0.2 ) then
     beaufortscale = 0
  else if ( windspeed > 0.3 .and. windspeed <= 1.5 ) then
     beaufortscale = 1
  else if ( windspeed > 1.6 .and. windspeed <= 3.3 ) then
     beaufortscale = 2
  else if ( windspeed > 3.4 .and. windspeed <= 5.4 ) then
     beaufortscale = 3
  else if ( windspeed > 5.4 .and. windspeed <= 7.9 ) then
     beaufortscale = 4
  else if ( windspeed > 7.9 .and. windspeed <= 10.7 ) then
     beaufortscale = 5
  else if ( windspeed > 10.7 .and. windspeed <= 13.8 ) then
     beaufortscale = 6
  else if ( windspeed > 13.8 .and. windspeed <= 17.1 ) then
     beaufortscale = 7
  else if ( windspeed > 17.1 .and. windspeed <= 20.7 ) then
     beaufortscale = 8
  else if ( windspeed > 20.7 .and. windspeed <= 24.4 ) then
     beaufortscale = 9
  else if ( windspeed > 24.4 .and. windspeed <= 28.4 ) then
     beaufortscale = 10
  else if ( windspeed > 28.4 .and. windspeed <= 32.6 ) then
     beaufortscale = 11
  else if ( windspeed > 32.6 .and. windspeed <= 36.9 ) then
     beaufortscale = 12
  else if ( windspeed > 36.9 .and. windspeed <= 41.4 ) then
     beaufortscale = 13
  else if ( windspeed > 41.4 .and. windspeed <= 46.1 ) then
     beaufortscale = 14
  else if ( windspeed > 46.1 .and. windspeed <= 50.9 ) then
     beaufortscale = 15
  else if ( windspeed > 50.9 .and. windspeed <= 56.0 ) then
     beaufortscale = 16
  else if ( windspeed > 56.0 .and. windspeed <= 61.2 ) then
     beaufortscale = 17
  else if ( windspeed > 61.2 ) then
     beaufortscale = 18
  endif

  return
  end function

!-----------------------------------------------------------------------+
  subroutine interp_fill_nan_1d(nlen, dat, radius, value_min, value_max)

  integer, intent(in)                  :: nlen, radius
  real, dimension(nlen), intent(inout) :: dat
  real, intent(in)                     :: value_min, value_max

  integer :: i, j, i1, i2, nbefore, nafter

  do i = 1, nlen
     if ( dat(i) < value_min .or. dat(i) > value_max ) then
        !----search avialable points before/after i
        i1=0
        do_search_before: do j = 1, radius
           if ( i-j < 1 ) exit do_search_before
           if ( dat(i-j) >= value_min .and. dat(i-j) <= value_max ) then
              i1 = i - j
              exit do_search_before
           endif 
        enddo do_search_before
        i2=0
        do_search_after: do j = 1, radius
           if ( i+j > nlen ) exit do_search_after
           if ( dat(i+j) >= value_min .and. dat(i+j) <= value_max ) then
              i2 = i + j
              exit do_search_after
           endif
        enddo do_search_after

        !---interp
        if ( i1 >= 1 .and. i1 < nlen .and. i2 > 1 .and. i2 <= nlen ) then
           dat(i) = ((i2-i)*dat(i1) + (i-i1)*dat(i2))/(i2-i1)
        endif
     endif
  enddo

  return
  end subroutine interp_fill_nan_1d

!========================================================================================
  subroutine cal_src_dst_grid_weight(grid_src, grid_dst)

  use module_mpi
  use var_type

  implicit none

  type(grid2d_info), intent(in) :: grid_src, grid_dst
  integer, allocatable, dimension(:,:) :: x_oini, y_oini
  real, allocatable, dimension(:,:)     :: lat_src, lon_src, lat_dst, lon_dst
  integer   :: i, j

!------------------------------------------------------------------------------
! 1 --- T-grid position in nearest source grid
  i=int(grid_dst%grid_xt/2);j=int(grid_dst%grid_yt/2)
  if ( debug_level > 10 ) then
     write(*,'(a,i0,a,i0)')'---- processing t-cell: ', grid_dst%grid_xt,':',grid_dst%grid_yt
     write(*,'(a)')' ---    i     j        lon        lat'
     write(*,'(a3,2i6, 2f11.2,2i11, 2f11.2)')' t:', i, j, grid_dst%grid_lont(i,j), grid_dst%grid_latt(i,j)
  endif

  allocate(x_oini(grid_dst%grid_xt, grid_dst%grid_yt), y_oini(grid_dst%grid_xt, grid_dst%grid_yt))
  call search_nearst_grid(grid_src%grid_xt, grid_src%grid_yt, grid_src%grid_latt, grid_src%grid_lont, &
                          grid_dst%grid_xt, grid_dst%grid_yt, grid_dst%grid_latt, grid_dst%grid_lont, &
                          x_oini, y_oini)
  if ( debug_level > 10 ) then
     write(*,'(a)')' ---    i     j        lon        lat          x          y m(i,j)_lon m(i,j)_lat '
     write(*,'(a3,2i6)')' t:', i, j
     write(*,'(a3,2i6, 2f11.2,2i11, 2f11.2)')' t:', i, j, grid_dst%grid_lont(i,j), grid_dst%grid_latt(i,j), x_oini(i,j), y_oini(i,j), &
        grid_src%grid_lont(x_oini(i,j),y_oini(i,j)), grid_src%grid_latt(x_oini(i,j),y_oini(i,j))
  endif
  !do j1=1,grid_dst%grid_yt; do i1=1,grid_dst%grid_xt
  !   write(98,'(4i6)')i1,j1,x_oini(i1,j1),y_oini(i1,j1)
  !enddo;enddo

  !---find (x_oini(i,j), y_oini(i,j)) and its neighboring 8 points, and calculate distance and then weights
  allocate(gwt%gwt_t(grid_dst%grid_xt, grid_dst%grid_yt))  !grid_weight_info
  call cal_grid_weight(grid_src%grid_xt, grid_src%grid_yt, grid_src%grid_latt, grid_src%grid_lont, &
                       grid_dst%grid_xt, grid_dst%grid_yt, grid_dst%grid_latt, grid_dst%grid_lont, &
                       x_oini, y_oini, gwt%max_points, gwt%gwt_t)
  deallocate(x_oini, y_oini)
  write(*,'(a,30i6)')'    gwt%gwt_t @ ', i, j, gwt%gwt_t(i,j)%src_points, gwt%gwt_t(i,j)%src_x(1:gwt%gwt_t(i,j)%src_points), &
                     gwt%gwt_t(i,j)%src_y(1:gwt%gwt_t(i,j)%src_points)

  !---so for T-grid variables:
  ! merged-grid = (1.0-dst_weight)*sum(gwt%gwt_t(i,j)%src_weight(:)*SRCVAR(gwt%gwt_t(i,j)%src_x(:), gwt%gwt_t(i,j)%src_y(:))) +
  !               dst_weight*sum(gwt%gwt_t(i,j)%dst_weight(:)*DSTVAR(gwt%gwt_t(i,j)%dst_x(:),gwt%gwt_t(i,j)%dst_y(:)))

  !---4.2, U-grid (grid_dst%grid_xt, grid_dst%grid_y)
  write(*,'(a)')'---- processing u-cell ----'
  allocate(x_oini(grid_dst%grid_xt, grid_dst%grid_y), y_oini(grid_dst%grid_xt, grid_dst%grid_y))
  allocate(lon_src(grid_src%grid_xt, grid_src%grid_y), lat_src(grid_src%grid_xt, grid_src%grid_y))
  if ( grid_src%grid_x-grid_src%grid_xt >= 1 ) then
     lon_src(1:grid_src%grid_xt,1:grid_src%grid_y) = (grid_src%grid_lon(1:grid_src%grid_xt,1:grid_src%grid_y) + &
                                                      grid_src%grid_lon(2:grid_src%grid_x ,1:grid_src%grid_y))/2.0
     lat_src(1:grid_src%grid_xt,1:grid_src%grid_y) = (grid_src%grid_lat(1:grid_src%grid_xt,1:grid_src%grid_y) + &
                                                      grid_src%grid_lat(2:grid_src%grid_x ,1:grid_src%grid_y))/2.0
  else
     lon_src(1:grid_src%grid_xt,1:grid_src%grid_y) = grid_src%grid_lon(1:grid_src%grid_xt,1:grid_src%grid_y)
     lat_src(1:grid_src%grid_xt,1:grid_src%grid_y) = grid_src%grid_lat(1:grid_src%grid_xt,1:grid_src%grid_y)
  endif
  !write(*,'(a)')'last row src lat before: ------------------'
  !write(*,'(15f8.2)')grid_src%grid_lat(488:grid_src%grid_xt,grid_src%grid_y-1)

  allocate(lon_dst(grid_dst%grid_xt, grid_dst%grid_y), lat_dst(grid_dst%grid_xt, grid_dst%grid_y))
  if ( grid_dst%grid_x-grid_dst%grid_xt >= 1 ) then
     lon_dst(1:grid_dst%grid_xt,1:grid_dst%grid_y) = (grid_dst%grid_lon(1:grid_dst%grid_xt,1:grid_dst%grid_y) + &
                                                      grid_dst%grid_lon(2:grid_dst%grid_x ,1:grid_dst%grid_y))/2.0
     lat_dst(1:grid_dst%grid_xt,1:grid_dst%grid_y) = (grid_dst%grid_lat(1:grid_dst%grid_xt,1:grid_dst%grid_y) + &
                                                      grid_dst%grid_lat(2:grid_dst%grid_x ,1:grid_dst%grid_y))/2.0
  else
     lon_dst(1:grid_dst%grid_xt,1:grid_dst%grid_y) = grid_dst%grid_lon(1:grid_dst%grid_xt,1:grid_dst%grid_y)
     lat_dst(1:grid_dst%grid_xt,1:grid_dst%grid_y) = grid_dst%grid_lat(1:grid_dst%grid_xt,1:grid_dst%grid_y)
  endif

  !if (debug) then
  write(*,'(a,2i8,4f10.3)')'src grid_lat: ', grid_src%grid_x , grid_src%grid_y, grid_src%grid_lat(1,1), &
                           grid_src%grid_lat(grid_src%grid_x,1), grid_src%grid_lat(grid_src%grid_x,grid_src%grid_y), &
                           grid_src%grid_lat(1,grid_src%grid_y)
  write(*,'(a,2i8,4f10.3)')'src    u lat: ', grid_src%grid_xt, grid_src%grid_y, lat_src(1,1),lat_src(grid_src%grid_xt,1), &
                           lat_src(grid_src%grid_xt,grid_src%grid_y), lat_src(1,grid_src%grid_y)
  write(*,'(a,f7.2,a,f7.2,a,f7.2,a,f7.2)')'src  grid range:',minval(grid_src%grid_lon),':',maxval(grid_src%grid_lon), &
                           '; ',minval(grid_src%grid_lat),':',maxval(grid_src%grid_lat)
  write(*,'(a,f7.2,a,f7.2,a,f7.2,a,f7.2)')'src u-grid range:',minval(lon_src),':',maxval(lon_src),'; ',minval(lat_src),':',maxval(lat_src)
  write(*,'(a,2i8,4f10.3)')'dst grid_lat: ', grid_dst%grid_x , grid_dst%grid_y, grid_dst%grid_lat(1,1), &
                           grid_dst%grid_lat(grid_dst%grid_x,1), grid_dst%grid_lat(grid_dst%grid_x,grid_dst%grid_y), &
                           grid_dst%grid_lat(1,grid_dst%grid_y)
  write(*,'(a,2i8,4f10.3)')'dst    u lat: ', grid_dst%grid_xt, grid_dst%grid_y, lat_dst(1,1),lat_dst(grid_dst%grid_xt,1),&
                           lat_dst(grid_dst%grid_xt,grid_dst%grid_y), lat_dst(1,grid_dst%grid_y)
  write(*,'(a,f7.2,a,f7.2,a,f7.2,a,f7.2)')'dst  grid range:',minval(grid_dst%grid_lon),':',maxval(grid_dst%grid_lon), &
                           '; ',minval(grid_dst%grid_lat),':',maxval(grid_dst%grid_lat)
  write(*,'(a,f7.2,a,f7.2,a,f7.2,a,f7.2)')'dst u-grid range:',minval(lon_dst),':',maxval(lon_dst),'; ',minval(lat_dst),':',maxval(lat_dst)
  !endif !if (debug) then

  call search_nearst_grid(grid_src%grid_xt, grid_src%grid_y, lat_src, lon_src, &
                          grid_dst%grid_xt, grid_dst%grid_y, lat_dst, lon_dst, x_oini, y_oini)

  write(*,'(a3, 2i6, 2f11.2,2i11, 2f11.2)')' u:', i, j, lon_dst(i,j), lat_dst(i,j), x_oini(i,j), y_oini(i,j), &
     lon_src(x_oini(i,j),y_oini(i,j)), lat_src(x_oini(i,j),y_oini(i,j))

  allocate(gwt%gwt_u(grid_dst%grid_xt, grid_dst%grid_y))  !grid_weight_info
  call cal_grid_weight(grid_src%grid_xt, grid_src%grid_y, lat_src, lon_src, &
                       grid_dst%grid_xt, grid_dst%grid_y, lat_dst, lon_dst, x_oini, y_oini, gwt%max_points, gwt%gwt_u)
  deallocate(x_oini, y_oini, lon_src, lat_src, lon_dst, lat_dst)

  !---4.3, V-grid
  write(*,'(a)')'---- processing v-cell ----'
  allocate(x_oini(grid_dst%grid_x, grid_dst%grid_yt), y_oini(grid_dst%grid_x, grid_dst%grid_yt))
  allocate(lon_src(grid_src%grid_x, grid_src%grid_yt), lat_src(grid_src%grid_x , grid_src%grid_yt))
  if ( grid_src%grid_y-grid_src%grid_yt >= 1 ) then
     lon_src(1:grid_src%grid_x,1:grid_src%grid_yt) = (grid_src%grid_lon(1:grid_src%grid_x,1:grid_src%grid_yt) + &
                                                      grid_src%grid_lon(1:grid_src%grid_x,2:grid_src%grid_y))/2.0
     lat_src(1:grid_src%grid_x,1:grid_src%grid_yt) = (grid_src%grid_lat(1:grid_src%grid_x,1:grid_src%grid_yt) + &
                                                      grid_src%grid_lat(1:grid_src%grid_x,2:grid_src%grid_y))/2.0
  else
     lon_src(1:grid_src%grid_x,1:grid_src%grid_yt) = grid_src%grid_lon(1:grid_src%grid_x,1:grid_src%grid_yt)
     lat_src(1:grid_src%grid_x,1:grid_src%grid_yt) = grid_src%grid_lat(1:grid_src%grid_x,1:grid_src%grid_yt)
  endif

  allocate(lon_dst(grid_dst%grid_x, grid_dst%grid_yt), lat_dst(grid_dst%grid_x , grid_dst%grid_yt))
  if ( grid_dst%grid_y-grid_dst%grid_yt >= 1 ) then
     lon_dst(1:grid_dst%grid_x,1:grid_dst%grid_yt) = (grid_dst%grid_lon(1:grid_dst%grid_x,1:grid_dst%grid_yt) + &
                                                      grid_dst%grid_lon(1:grid_dst%grid_x,2:grid_dst%grid_y))/2.0
     lat_dst(1:grid_dst%grid_x,1:grid_dst%grid_yt) = (grid_dst%grid_lat(1:grid_dst%grid_x,1:grid_dst%grid_yt) + &
                                                      grid_dst%grid_lat(1:grid_dst%grid_x,2:grid_dst%grid_y))/2.0
  else
     lon_dst(1:grid_dst%grid_x,1:grid_dst%grid_yt) = grid_dst%grid_lon(1:grid_dst%grid_x,1:grid_dst%grid_yt)
     lat_dst(1:grid_dst%grid_x,1:grid_dst%grid_yt) = grid_dst%grid_lat(1:grid_dst%grid_x,1:grid_dst%grid_yt)
  endif
  call search_nearst_grid(grid_src%grid_x, grid_src%grid_yt, lat_src, lon_src, &
                          grid_dst%grid_x, grid_dst%grid_yt, lat_dst, lon_dst, x_oini, y_oini)

  write(*,'(a3,2i6, 2f11.2,2i11, 2f11.2)')' v:', i, j, lon_dst(i,j), lat_dst(i,j), x_oini(i,j), y_oini(i,j), &
     lon_src(x_oini(i,j),y_oini(i,j)), lat_src(x_oini(i,j),y_oini(i,j))

  allocate(gwt%gwt_v(grid_dst%grid_x, grid_dst%grid_yt))  !grid_weight_info
  call cal_grid_weight(grid_src%grid_x, grid_src%grid_yt, lat_src, lon_src, &
                       grid_dst%grid_x, grid_dst%grid_yt, lat_dst, lon_dst, x_oini, y_oini, gwt%max_points, gwt%gwt_v)
  deallocate(x_oini, y_oini, lon_src, lat_src, lon_dst, lat_dst)

  return
  end subroutine cal_src_dst_grid_weight
!========================================================================================
!-----------------------------------------------------------------------+  
  subroutine search_nearst_grid0(src_ix, src_jx, src_lat, src_lon, dst_ix, dst_jx, &
             dst_lat, dst_lon, dst_in_src_x, dst_in_src_y)

  implicit none
  
  integer, intent(in)                  :: src_ix, src_jx, dst_ix, dst_jx
  real, dimension(src_ix, src_jx), intent(in) :: src_lat, src_lon
  real, dimension(dst_ix, dst_jx), intent(in) :: dst_lat, dst_lon
  integer, dimension(dst_ix, dst_jx), intent(out) :: dst_in_src_x, dst_in_src_y

  real, allocatable, dimension(:)  :: max_dx, max_dy
  real    :: dis, dis0
  integer :: i, j, i1, j1

  allocate(max_dy(src_jx))
  do j = 1, src_jx
     max_dy(j) = maxval(src_lat(:,j))-minval(src_lat(:,j))
  enddo
  allocate(max_dx(src_ix))
  do i = 1, src_ix
     max_dx(i) = maxval(src_lon(i,:))-minval(src_lon(i,:))
  enddo

  !out_ave_dx=abs( (dst_lon(1,int(dst_jx/2))-dst_lon(dst_ix,dst_jx))/real(dst_ix))
  !out_ave_dy=abs( (dst_lat(int(dst_jx/2),1)-dst_lat(dst_ix,dst_jx))/real(dst_jx))
  dst_in_src_x=-99; dst_in_src_y=-99
  write(*,'(a,2f12.6)')'max dst grid dx/dy in deg:', maxval(max_dx), maxval(max_dy)

  do j = 1,dst_jx; do i = 1,dst_ix
     dis0=99999.0
     do_search_src_grid_y: do j1 = 1, src_jx
         !if( abs(dst_lat(i,j)-src_lat(int(src_ix/2),j1)) > 2.0*out_ave_dy ) cycle do_search_src_grid_y
         !if( abs(dst_lat(i,j)-src_lat(int(src_ix/2),j1)) > 20.0*out_ave_dy .and.  &
         !    abs(dst_lat(i,j)-src_lat(1,j1)) > 20.0*out_ave_dy .and. &
         !    abs(dst_lat(i,j)-src_lat(src_ix,j1)) > 20.0*out_ave_dy ) cycle do_search_src_grid_y 
         if ( abs(dst_lat(i,j)-src_lat(int(src_ix/2),j1)) > 2.0*max_dy(j1) ) cycle do_search_src_grid_y
         do_search_src_grid_x: do i1 = 1, src_ix
            !if ( abs(dst_lon(i,j)-src_lon(i1,j1)) > 1.0*out_ave_dx ) cycle do_search_src_grid_x
            if ( abs(dst_lon(i,j)-src_lon(i1,j1)) > max_dx(i1) ) cycle do_search_src_grid_x 
            dis=(dst_lon(i,j)-src_lon(i1,j1))**2.0+(dst_lat(i,j)-src_lat(i1,j1))**2.0
            if ( dis <= dis0 ) then
               dis0 = dis
               dst_in_src_x(i,j)=i1
               dst_in_src_y(i,j)=j1
            endif
         enddo do_search_src_grid_x
     enddo do_search_src_grid_y

  enddo; enddo

  deallocate(max_dy, max_dx)

  return
  end subroutine search_nearst_grid0

!-----------------------------------------------------------------------+
  subroutine search_nearst_grid(src_ix, src_jx, src_lat, src_lon, dst_ix, dst_jx, &
             dst_lat, dst_lon, dst_in_src_x, dst_in_src_y)

  implicit none

  integer, intent(in)                  :: src_ix, src_jx, dst_ix, dst_jx
  real, dimension(src_ix, src_jx), intent(in) :: src_lat, src_lon
  real, dimension(dst_ix, dst_jx), intent(in) :: dst_lat, dst_lon
  integer, dimension(dst_ix, dst_jx), intent(out) :: dst_in_src_x, dst_in_src_y

  integer, allocatable, dimension(:,:,:)  :: src_points_lon, src_points_lat
  integer, allocatable, dimension(:,:) :: src_points
  real, allocatable, dimension(:)      :: ll_lon, ll_lat

  integer :: i, j, ll_ix, ll_jx, i1, j1, max_points, i2, j2, i3, j3, n
  real    :: min_lon, min_lat, max_lon, max_lat, d_ll, dis, dis0, dis_src, dis_dst
  logical :: src_in_bin
  real    :: earth_dist

  !---define serach bins
  d_ll = 0.1   !default bin size is 0.1 degx0.1 deg
  i1=int(src_ix/2); j1=int(src_jx/2);
  dis=sqrt((src_lon(i1,j1)-src_lon(i1+1,j1+1))**2.0+(src_lat(i1,j1)-src_lat(i1+1,j1+1))**2.0)
  dis_src=int(dis*100.)/100.
  d_ll=max(dis_src,d_ll)
  i1=int(dst_ix/2); j1=int(dst_jx/2);
  dis=sqrt((dst_lon(i1,j1)-dst_lon(i1+1,j1+1))**2.0+(dst_lat(i1,j1)-dst_lat(i1+1,j1+1))**2.0)
  dis_dst=int(dis*100.)/100.
  d_ll=max(dis_dst,d_ll)
  !---determine max_points in each bin
  max_points=min(max(10, 30*(int(d_ll/min(dis_src,dis_dst))+1)**2), 100000)

  min_lon = min(minval(src_lon), minval(dst_lon)) - 5.*d_ll
  min_lat = min(minval(src_lat), minval(dst_lat)) - 5.*d_ll
  max_lon = max(maxval(src_lon), maxval(dst_lon)) + 5.*d_ll
  max_lat = max(maxval(src_lat), maxval(dst_lat)) + 5.*d_ll
  min_lon = int(min_lon*10)/10.0
  max_lon = int((max_lon+0.5)*10)/10.0
  min_lat = int(min_lat*10)/10.0
  max_lat = int((max_lat+0.5)*10)/10.0
 
  ll_ix = int((max_lon - min_lon)/d_ll) + 1
  ll_jx = int((max_lat - min_lat)/d_ll) + 1
  write(*,'(a,f,i )')'ll bin size d_ll and max_points:', d_ll, max_points
  write(*,'(a,3f  )')'min lon for src dst min:', minval(src_lon), minval(dst_lon), min_lon
  write(*,'(a,3f,i)')'max lon for src dst max:', maxval(src_lon), maxval(dst_lon), max_lon, ll_ix
  write(*,'(a,3f  )')'min lat for src dst min:', minval(src_lat), minval(dst_lat), min_lat
  write(*,'(a,3f,i)')'max lat for src dst max:', maxval(src_lat), maxval(dst_lat), max_lat, ll_jx
  allocate ( ll_lon(ll_ix), ll_lat(ll_jx))
  do i = 1, ll_ix
     ll_lon(i) = min_lon + (i-1)*d_ll
  enddo
  do j = 1, ll_jx
     ll_lat(j) = min_lat + (j-1)*d_ll
  enddo 
  write(*,'(a,f10.3,a,f10.3,a,f10.3,a,f10.3)')'search grids: ', ll_lon(1),'-->',ll_lon(ll_ix),' : ', ll_lat(1), '-->', ll_lat(ll_jx)

  !---sign src grids to search bins
  allocate ( src_points(ll_ix,ll_jx))
  allocate ( src_points_lon(ll_ix,ll_jx,max_points), src_points_lat(ll_ix,ll_jx,max_points))
  src_points=0
  
  !--- may need halo
  write(*,'(a)')'---sign src to ll grids'
  do j = 1, src_jx; do i = 1, src_ix
     i1 = int((src_lon(i,j) - ll_lon(1))/d_ll) + 1
     j1 = int((src_lat(i,j) - ll_lat(1))/d_ll) + 1
     do i2 = -1, 1; do j2 = -1, 1;   !add halo
        i3=i1+i2; j3=j1+j2  
        if ( i3 >= 1 .and. i3 <= ll_ix .and. j3 >= 1 .and. j3 <= ll_jx ) then
           if ( src_points(i3,j3) < max_points ) then
              src_in_bin=.false.
              do n=1,src_points(i3,j3)
                 if ( i .eq. src_points_lon(i3,j3,n) .and. &
                      j .eq. src_points_lat(i3,j3,n) ) then
                    !---src point already included, skip
                    src_in_bin=.true.
                    exit
                 endif
              enddo
              !---add the src point into the ll grid bin if it is not yet included
              if ( .not. src_in_bin ) then
                 src_points(i3,j3) = src_points(i3,j3) + 1
                 src_points_lon(i3,j3,src_points(i3,j3)) = i
                 src_points_lat(i3,j3,src_points(i3,j3)) = j
              endif
           else
              write(*,'(a,6i6,2f10.3)') 'WARNING: src_points(i3,j3) >= max_points at i3, j3, i, j, src_lon(i,j), src_lat(i,j)', &
                 src_points(i3,j3), max_points, i3, j3, i, j, src_lon(i,j), src_lat(i,j)
           endif
           !---may need add halo: 
        endif
     enddo; enddo
  enddo; enddo

  !---search nearest src grid for each dst grid
  write(*,'(a)')'---search nearest src grid for each dst grid'
  do j = 1, dst_jx; do i = 1, dst_ix
     !---calculate dst grid position in search-bin
     i1 = int((dst_lon(i,j) - ll_lon(1))/d_ll) + 1
     j1 = int((dst_lat(i,j) - ll_lat(1))/d_ll) + 1
     if ( i1 < 1 .or. j1 < 1 ) then
        write(*,'(a,4i6,4f10.3)')'i1j1---',i,j,i1,j1,ll_lon(1),ll_lat(1),dst_lon(i,j),dst_lat(i,j)
        stop
     endif
     if ( src_points(i1,j1) > 0 .and. src_points(i1,j1) <= max_points ) then
        !i2=i+1; if ( i2 > dst_ix) i2=i-1
        !j2=j+1; if ( j2 > dst_jx) j2=j-1
        dis0=999999.0
        do n = 1, src_points(i1,j1)
           if ( src_points_lon(i1,j1,n) < 1 .or. src_points_lon(i1,j1,n) > src_ix .or. src_points_lat(i1,j1,n) < 1 .or. src_points_lat(i1,j1,n) > src_jx ) then
              write(*,'(7i8)')n, i, j, i1, j1, src_points_lon(i1,j1,n), src_points_lat(i1,j1,n)
           endif
           !dis=(dst_lon(i,j)-src_lon(src_points_lon(i1,j1,n),src_points_lat(i1,j1,n)))**2.0 + &
           !    (dst_lat(i,j)-src_lat(src_points_lon(i1,j1,n),src_points_lat(i1,j1,n)))**2.0
           !---calculate great circle earth distance in km
           dis=earth_dist(src_lon(src_points_lon(i1,j1,n),src_points_lat(i1,j1,n)), &
                          src_lat(src_points_lon(i1,j1,n),src_points_lat(i1,j1,n)), &
                          dst_lon(i,j),dst_lat(i,j))/1000.
           if ( dis < dis0 ) then
              dis0 = dis
              dst_in_src_x(i,j)=src_points_lon(i1,j1,n)
              dst_in_src_y(i,j)=src_points_lat(i1,j1,n)
           endif
        enddo
     else
        dst_in_src_x(i,j)=-99
        dst_in_src_y(i,j)=-99 
     endif
  enddo; enddo

  !---clean up
  deallocate(ll_lon, ll_lat, src_points, src_points_lon, src_points_lat)
  write(*,'(a)')'---search_nearst_grid finished'

  return
  end subroutine search_nearst_grid 

!-----------------------------------------------------------------------+
  subroutine cal_grid_weight(src_ix, src_jx, src_lat, src_lon, dst_ix, dst_jx, &
             dst_lat, dst_lon, dst_in_src_x, dst_in_src_y, max_points, gw )

  use var_type
  implicit none
  integer, intent(in)                  :: src_ix, src_jx, dst_ix, dst_jx, max_points
  real, dimension(src_ix, src_jx), intent(in) :: src_lat, src_lon
  real, dimension(dst_ix, dst_jx), intent(in) :: dst_lat, dst_lon
  integer, dimension(dst_ix, dst_jx), intent(in) :: dst_in_src_x, dst_in_src_y
  type(gridmap_info), dimension(dst_ix, dst_jx), intent(inout) :: gw  !gridweight

  integer :: i, j, k, n, i1, j1, ixs, jxs, ixi, jxi, min_ij_src, min_ij_dst
  real    :: dst_weight, earth_dist, dis, lon180_1, lon180_2

  do j = 1,dst_jx; do i = 1,dst_ix
     ixs=dst_in_src_x(i,j)  !the position in src grids
     jxs=dst_in_src_y(i,j)

     gw(i,j)%src_points=0
     allocate(gw(i,j)%src_x(max_points), gw(i,j)%src_y(max_points), gw(i,j)%src_weight(max_points))
     if ( ixs >= 1 .and. ixs <= src_ix .and. jxs >= 1 .and. jxs <= src_jx ) then
        n=0  !gw(i,j)%src_points=0
        do j1 = -1, 1; do i1 = -1, 1;
        !do j1 = 0, 0; do i1 = 0, 0
           if ( i1*j1 == 0 ) then  !5-point
              ixi=ixs+i1; jxi=jxs+j1
              !---change left to center
              if ( ixi < 1 ) ixi=3
              if ( ixi > src_ix ) ixi=src_ix-2
              if ( jxi < 1 ) jxi=3
              if ( jxi > src_jx ) jxi=src_jx-2
              if ( ixi >= 1 .and. ixi <= src_ix .and. jxi >= 1 .and. jxi <= src_jx ) then
                 n=n+1
                 gw(i,j)%src_x(n)=ixi
                 gw(i,j)%src_y(n)=jxi
                 !gw(i,j)%src_weight(n)=earth_dist(src_lon(ixi,jxi),src_lat(ixi,jxi),dst_lon(i,j),dst_lat(i,j))
                 !nolinear dis weight
                 !gw(i,j)%src_weight(n)=gw(i,j)%src_weight(n)*gw(i,j)%src_weight(n)
                 !---use the weighting function based on exp(-r/r_scale); here r_scale is hardcoded as 2km currently
                 gw(i,j)%src_weight(n)=exp(-earth_dist(src_lon(ixi,jxi),src_lat(ixi,jxi),dst_lon(i,j),dst_lat(i,j))/2000.)
              endif  !if ( ixi >= 1 .and. ixi <= src_ix .and. jxi >= 1 .and. jxi <= src_jx ) then
           endif
        enddo; enddo

        !---convert earth_dist to weightening
        gw(i,j)%src_points=n
        if ( n > 0 ) then
           dis=0.
           do k = 1, n
              dis=dis+gw(i,j)%src_weight(k)
           enddo
           if ( dis == 0. ) then
              gw(i,j)%src_weight(1:n)=1.0/real(n)
           else if (dis > 0. .and. dis < 9000000000. ) then
              !gw(i,j)%src_weight(1:n)=gw(i,j)%src_weight(1:n)/dis
              if ( n <= 1 ) then
                 gw(i,j)%src_weight(1:n)=1.0
              else
                 !gw(i,j)%src_weight(1:n)=(dis-gw(i,j)%src_weight(1:n))/((n-1)*dis)
                 gw(i,j)%src_weight(1:n)=gw(i,j)%src_weight(1:n)/dis
              endif
           else
              write(*,'(a)')'earth_dist calculation is wrong'
           endif
        endif
     endif

     !---select dst grids for smooth if needed
     gw(i,j)%dst_points=0
     dst_weight=0.0
     allocate(gw(i,j)%dst_x(max_points), gw(i,j)%dst_y(max_points), gw(i,j)%dst_weight(max_points))
   
     !if ( ixs > 1 .and. ixs < src_ix .and. jxs > 1 .and. jxs < src_jx ) then
     !   !---when the grid is inside of src-domain, no dst grid is needed
     !   !dst_weight=0.0
        gw(i,j)%dst_points=1
        gw(i,j)%dst_x(1)=i
        gw(i,j)%dst_y(1)=j
        gw(i,j)%dst_weight(1)=1.0
     !else if ( (i==1 .and. ixs==1) .or. (i==dst_ix .and. ixs==src_ix) .or. (j==1 .and. jxs==1) .or. (j==dst_jx .and. jxs==src_jx) ) then
     !   !---when the dst domain is the same as the src domain, no dst grid is needed too
     !   !--- so just the outest circle
     !   !dst_weight=0.0
     !   gw(i,j)%dst_points=1
     !   gw(i,j)%dst_x(1)=i
     !   gw(i,j)%dst_y(1)=j
     !   gw(i,j)%dst_weight(1)=1.0
     !else
     !   !if ( ixs <= 2 .or. ixs >= src_ix-1 .or. jxs >= 2 .or. jxs <= src_jx-1 ) then
     !   !---add self
     !   n=1
     !   gw(i,j)%dst_x(n) = i
     !   gw(i,j)%dst_y(n) = j
     !   gw(i,j)%dst_weight(n) = 1.0
     !   !dst_weight=0.3

     !   if ( ixs == 1 .or. ixs == src_ix .or. jxs == 1 .or. jxs == src_jx ) then
     !      !---if the grid is on the edge of the src domain, then add 9-point smooth.
     !      do j1 = -1, 1; do i1 = -1, 1
     !         if ( i1 /= 0 .and. j1 /= 0 ) then
     !            ixi=i+i1; jxi=j+j1
     !            if ( ixi >= 1 .and. ixi <= dst_ix .and. jxi >=1 .and. jxi <= dst_jx ) then
     !               n=n+1; gw(i,j)%dst_x(n)=ixi; gw(i,j)%dst_y(n)=jxi; gw(i,j)%dst_weight(n)=0.5
     !            endif
     !         endif
     !      enddo; enddo
     !      !dst_weight=0.5
     !   !elseif ( ixs < 1 .or. ixs > src_ix .or. jxs < 1 .or. jxs > src_jx ) then
     !      !dst_weight=1.0
     !   endif

     !   gw(i,j)%dst_points=n
     !   gw(i,j)%dst_weight(1:n)=gw(i,j)%dst_weight(1:n)/sum(gw(i,j)%dst_weight(1:n))
     !endif

     if ( ixs>=1 .and. ixs<=src_ix .and. jxs>=1 .and. jxs<=src_jx) then  !inside src-domain
        dst_weight=0.0
        if ( gwt%relaxzone < 0 ) gwt%relaxzone = min(30, int(min(src_ix, src_jx, dst_ix, dst_jx)/10))
        !--- find relaxzone: min (i,j) or max (i,j) grids to src (1,1) and src(ix,jx)
        !  ixs, jxs  
        min_ij_src = min(ixs, jxs, src_ix-ixs, src_jx-jxs) ! shortest distance (grid not earth-distance) from SRC domain edge 
        min_ij_dst = min(i, j, dst_ix-i, dst_jx-j) 
        if ( min_ij_src <= gwt%relaxzone .and. min_ij_dst > 2 .and. gwt%relaxzone > 0 ) then
           dst_weight = real(gwt%relaxzone - min_ij_src)/real(gwt%relaxzone)
           if ( dst_weight < 0.0 .or. dst_weight > 1.0 ) then
              write(*,'(a,7i6,f10.4)')'---relaxzone dst_weight:', i,j,ixs,jxs,min_ij_src,min_ij_dst,gwt%relaxzone,dst_weight
              stop
           endif
        endif
     else
        dst_weight=1.0
     endif

     !--- find TC vortex relax zone
     if ( tc%vortexrep==1 .and. tc%lat>-85. .and. tc%lat<85. .and. tc%lon>=-180. .and. tc%lon<=360. ) then
        lon180_1=src_lon(ixs,jxs)
        if ( lon180_1 > 180. ) lon180_1 = lon180_1 -360.
        lon180_2=tc%lon
        if ( lon180_2 > 180. ) lon180_2 = lon180_2 - 360.
        dis = earth_dist(lon180_1,src_lat(ixs,jxs),lon180_2,tc%lat)/1000.

        if ( abs(tc%vortexreplace_r(1)-tc%vortexreplace_r(2)) < 1.0 .or. tc%vortexreplace_r(1) < 1.0 .or. tc%vortexreplace_r(2) < 1.0 ) then
           tc%vortexreplace_r(1)=600.
           tc%vortexreplace_r(2)=900.
        endif
        if ( dis < tc%vortexreplace_r(1) ) then
           dst_weight=0.0
        else if ( dis > tc%vortexreplace_r(2) ) then
           dst_weight=1.0
        else
           dst_weight=(dis-tc%vortexreplace_r(1))/(tc%vortexreplace_r(2)-tc%vortexreplace_r(1))
        endif
        !write(*,*)'----tc zone', tc%lon, tc%lat, dis, tc%vortexreplace_r(1:2), dst_weight
        if ( dst_weight < 0.0 .or. dst_weight > 1.0 ) then
           write(*,'(a,4i6,4f10.2)')'---vortex dst_weight:', i,j,ixs,jxs,tc%vortexreplace_r(1:2), dis, dst_weight
           stop
        endif
     !else
     !   write(*,*)'----tc zone', tc%lon, tc%lat, dis, tc%vortexreplace_r(1:2), dst_weight
     !   stop
     endif  

     !---combine src and dst weight
     if ( gw(i,j)%src_points > 0 ) then
        gw(i,j)%src_weight(1:gw(i,j)%src_points)=(1.0-dst_weight)*gw(i,j)%src_weight(1:gw(i,j)%src_points)
        gw(i,j)%dst_weight(1:gw(i,j)%dst_points)=     dst_weight *gw(i,j)%dst_weight(1:gw(i,j)%dst_points)
     else
        gw(i,j)%src_weight = 0.
     endif

  enddo; enddo
 
  return
  end subroutine cal_grid_weight

!-----------------------------------------------------------------------+
  subroutine combine_grids_for_remap(ixi, jxi, kxi, txi, fdat_src, ixo, jxo, kxo, txo, fdat_dst, gw, fdat_out)

! --- remap: (src U dst) --> src
! --- merge: (src U dst) --> dst

  use constants
  use var_type
  implicit none
  integer, intent(in)                  :: ixi, jxi, kxi, txi, ixo, jxo, kxo, txo
  real, dimension(ixi, jxi, kxi, txi), intent(in) :: fdat_src
  real, dimension(ixo, jxo, kxo, txo), intent(in) :: fdat_dst
  type(gridmap_info), dimension(ixo, jxo), intent(in) :: gw
  real, dimension(ixo, jxo, kxo, txo), intent(out) :: fdat_out

  integer   :: i, j, k, n, i1, j1, k1, n1, ncount

  do n = 1, txo; do k = 1, kxo; do j = 1, jxo; do i = 1, ixo
     fdat_out(i,j,k,n)=0.0
     ncount=0
     if ( gw(i,j)%src_points > 0 ) then
        do_src_points_loop: do n1 = 1, gw(i,j)%src_points 
           i1=gw(i,j)%src_x(n1)
           j1=gw(i,j)%src_y(n1)
           if ( i1 < 1 .or. i1 > ixi .or. j1 < 1 .or. j1 > jxi ) cycle do_src_points_loop
           if ( gw(i,j)%src_weight(n1) <= 0. .or. gw(i,j)%src_weight(n1) > 1.0 ) cycle do_src_points_loop
           if ( fdat_src(i1,j1,k,n) < -99999. .or. fdat_src(i1,j1,k,n) > 90000000. ) cycle do_src_points_loop
           fdat_out(i,j,k,n)=fdat_out(i,j,k,n)+gw(i,j)%src_weight(n1)*fdat_src(i1,j1,k,n)
           ncount=ncount+1
        enddo do_src_points_loop
     endif
     if ( gw(i,j)%dst_points > 0 ) then
        do_dst_points_loop: do n1 = 1, gw(i,j)%dst_points
           i1=gw(i,j)%dst_x(n1)
           j1=gw(i,j)%dst_y(n1)
           if ( i1 < 1 .or. i1 > ixo .or. j1 < 1 .or. j1 > jxo ) cycle do_dst_points_loop
           if ( gw(i,j)%dst_weight(n1) <= 0. .or. gw(i,j)%dst_weight(n1) > 1.0 ) cycle do_dst_points_loop
           if ( fdat_dst(i1,j1,k,n) < -99999. .or. fdat_dst(i1,j1,k,n) > 90000000. ) cycle do_dst_points_loop
           fdat_out(i,j,k,n)=fdat_out(i,j,k,n)+gw(i,j)%dst_weight(n1)*fdat_dst(i1,j1,k,n)
           ncount=ncount+1
        enddo do_dst_points_loop
     endif
     if (ncount < 1 ) fdat_out(i,j,k,n)=missing

     !---debug
     !if ( (i == int(ixo/4) .or. i == int(ixo/2) .or. i == ixo-1) .and. &
     !     (j == int(jxo/4) .or. j == int(jxo/2) .or. j == jxo-1) .and. k==1 .and. n==1 ) then
     if ( i == int(ixo/2) .and.j == int(jxo/2) .and. k==1 .and. n==1 ) then
        write(*,'(a,   5i10)')'--combine_grids_for_remap: ',i,j, gw(i,j)%src_points, gw(i,j)%dst_points, ncount 
        write(*,'(a,  90i10)')'--             src_points: ', ((gw(i,j)%src_x(n1), gw(i,j)%src_y(n1)),n1=1,gw(i,j)%src_points)
        write(*,'(a,90f)')    '--             src_weight: ', ((gw(i,j)%src_weight(n1)),n1=1,gw(i,j)%src_points)
        write(*,'(a,90f)')    '--             src_values: ', ( fdat_src(gw(i,j)%src_x(n1),gw(i,j)%src_y(n1),k,n),n1=1,gw(i,j)%src_points)
        if ( gw(i,j)%dst_points > 0 ) then
           write(*,'(a,  90i10)')'--             dst_points: ', ((gw(i,j)%dst_x(n1), gw(i,j)%dst_y(n1)),n1=1,gw(i,j)%dst_points)
           write(*,'(a,90f10.4)')'--             dst_weight: ', ((gw(i,j)%dst_weight(n1)),n1=1,gw(i,j)%dst_points)
           write(*,'(a,  e)')    '--             dst_values: ', ( fdat_dst(gw(i,j)%dst_x(n1),gw(i,j)%dst_y(n1),k,n),n1=1,gw(i,j)%dst_points)
        else
           write(*,'(a)')     '--             no dst point'
        endif
        write(*,'(a,  f)')    '--          remaped value: ', fdat_out(i,j,k,n) 
     endif 
    
  enddo; enddo; enddo; enddo

  return
  end subroutine combine_grids_for_remap

!-----------------------------------------------------------------------+
  subroutine combine_grids_for_merge(ixi, jxi, kxi, txi, fdat_src, ixo, jxo, kxo, txo, fdat_dst, gw, fdat_out)

! --- remap: (src U dst) --> src
! --- merge: (src U dst) --> dst

  use constants
  use var_type
  implicit none
  integer, intent(in)                  :: ixi, jxi, kxi, txi, ixo, jxo, kxo, txo
  real, dimension(ixi, jxi, kxi, txi), intent(in) :: fdat_src
  real, dimension(ixo, jxo, kxo, txo), intent(in) :: fdat_dst
  type(gridmap_info), dimension(ixo, jxo), intent(in) :: gw
  real, dimension(ixo, jxo, kxo, txo), intent(out) :: fdat_out

  integer   :: i, j, k, n, i1, j1, k1, n1, ncount

  do n = 1, txo; do k = 1, kxo; do j = 1, jxo; do i = 1, ixo
     fdat_out(i,j,k,n)=0.0
     ncount=0
     if ( gw(i,j)%dst_points > 0 ) then
        do_dst_points_loop: do n1 = 1, gw(i,j)%dst_points
           i1=gw(i,j)%dst_x(n1)
           j1=gw(i,j)%dst_y(n1)
           if ( i1 < 1 .or. i1 > ixo .or. j1 < 1 .or. j1 > jxo ) cycle do_dst_points_loop
           !if ( gw(i,j)%dst_weight(n1) <= 0. .or. gw(i,j)%dst_weight(n1) > 1.0 ) cycle do_dst_points_loop
           if ( fdat_dst(i1,j1,k,n) < -99999. .or. fdat_dst(i1,j1,k,n) > 90000000. ) cycle do_dst_points_loop
           !fdat_out(i,j,k,n)=fdat_out(i,j,k,n)+gw(i,j)%dst_weight(n1)*fdat_dst(i1,j1,k,n)
           fdat_out(i,j,k,n)=fdat_out(i,j,k,n)+fdat_dst(i1,j1,k,n)
           ncount=ncount+1
        enddo do_dst_points_loop
     endif
     if ( ncount > 0 ) then
        fdat_out(i,j,k,n)=fdat_out(i,j,k,n)/real(ncount)
     else
        if ( gw(i,j)%src_points > 0 ) then
           do_src_points_loop: do n1 = 1, gw(i,j)%src_points
              i1=gw(i,j)%src_x(n1)
              j1=gw(i,j)%src_y(n1)
              if ( i1 < 1 .or. i1 > ixi .or. j1 < 1 .or. j1 > jxi ) cycle do_src_points_loop
              if ( gw(i,j)%src_weight(n1) <= 0. .or. gw(i,j)%src_weight(n1) > 1.0 ) cycle do_src_points_loop
              if ( fdat_src(i1,j1,k,n) < -99999. .or. fdat_src(i1,j1,k,n) > 90000000. ) cycle do_src_points_loop
              fdat_out(i,j,k,n)=fdat_out(i,j,k,n)+gw(i,j)%src_weight(n1)*fdat_src(i1,j1,k,n)
              ncount=ncount+1
           enddo do_src_points_loop
        endif
     endif
     if (ncount < 1 ) fdat_out(i,j,k,n)=missing

     !---debug
     !if ( (i == int(ixo/4) .or. i == int(ixo/2) .or. i == ixo-1) .and. &
     !     (j == int(jxo/4) .or. j == int(jxo/2) .or. j == jxo-1) .and. k==1 .and. n==1 ) then
     if ( i == int(ixo/2) .and.j == int(jxo/2) .and. k==1 .and. n==1 ) then
        write(*,'(a,   5i10)')'--combine_grids_for_merge: ',i,j, gw(i,j)%src_points, gw(i,j)%dst_points, ncount
        write(*,'(a,  90i10)')'--             src_points: ', ((gw(i,j)%src_x(n1), gw(i,j)%src_y(n1)),n1=1,gw(i,j)%src_points)
        write(*,'(a,90f)')    '--             src_values: ', ( fdat_src(gw(i,j)%src_x(n1),gw(i,j)%src_y(n1),k,n),n1=1,gw(i,j)%src_points)
        if ( gw(i,j)%dst_points > 0 ) then
           write(*,'(a,  90i10)')'--             dst_points: ', ((gw(i,j)%dst_x(n1), gw(i,j)%dst_y(n1)),n1=1,gw(i,j)%dst_points)
           write(*,'(a,  e)')    '--             dst_values: ', ( fdat_dst(gw(i,j)%dst_x(n1),gw(i,j)%dst_y(n1),k,n),n1=1,gw(i,j)%dst_points)
        else
           write(*,'(a)')     '--             no dst point'
        endif
        write(*,'(a,  f)')    '--          remaped value: ', fdat_out(i,j,k,n)
     endif

  enddo; enddo; enddo; enddo

  return
  end subroutine combine_grids_for_merge

!-----------------------------------------------------------------------+             
  function uppercase (cs)

  implicit none
  character(len=*), intent(in) :: cs
  character(len=len(cs)),target       :: uppercase
  integer                      :: k,tlen
  character, pointer :: ca
  integer, parameter :: co=iachar('A')-iachar('a') ! case offset
  !The transfer function truncates the string with xlf90_r
  tlen = len_trim(cs)
  if (tlen <= 0) then      ! catch IBM compiler bug
     uppercase = cs  ! simply return input blank string
  else
    uppercase = cs(1:tlen)
!#if defined _CRAYX1
!    do k=1, tlen
!       if(uppercase(k:k) >= "a" .and. uppercase(k:k) <= 'z') uppercase(k:k) = achar(ichar(uppercase(k:k))+co)
!    end do
!#else
    do k=1, tlen
       ca => uppercase(k:k)
       if(ca >= "a" .and. ca <= "z") ca = achar(ichar(ca)+co)
    enddo
!#endif
  endif
  end function uppercase

!-----------------------------------------------------------------------+             
  function lowercase (cs)

  implicit none
  character(len=*), intent(in) :: cs
  character(len=len(cs)),target       :: lowercase
  integer, parameter :: co=iachar('a')-iachar('A') ! case offset
  integer                        :: k,tlen
  character, pointer :: ca
  !  The transfer function truncates the string with xlf90_r
  tlen = len_trim(cs)
  if (tlen <= 0) then      ! catch IBM compiler bug
     lowercase = cs  ! simply return input blank string
  else
     lowercase = cs(1:tlen)
!#if defined _CRAYX1
!     do k=1, tlen
!        if(lowercase(k:k) >= "A" .and. lowercase(k:k) <= 'Z') lowercase(k:k) = achar(ichar(lowercase(k:k))+co)
!     end do
!#else
     do k=1, tlen
        ca => lowercase(k:k)
        if(ca >= "A" .and. ca <= "Z") ca = achar(ichar(ca)+co)
     enddo
!#endif
  endif
  end function lowercase
         
!-----------------------------------------------------------------------+             
