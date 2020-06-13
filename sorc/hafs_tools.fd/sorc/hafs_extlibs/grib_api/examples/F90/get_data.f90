! Copyright 2005-2018 ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! 
! In applying this licence, ECMWF does not waive the privileges and immunities granted to it by
! virtue of its status as an intergovernmental organisation nor does it submit to any jurisdiction.
!
!
! Description: how to get lat/lon/values.
!
!
program get_data
use grib_api
implicit none
  integer            :: ifile
  integer            :: iret,i
  real(kind=8),dimension(:),allocatable     :: lats,lons,values
  integer,dimension(:),allocatable          :: bitmap
  integer(4)         :: numberOfPoints
  logical            :: is_missing_value
  integer            :: count1=0, count2=0, bitmapPresent=0, bmp_len=0
  character(len=256) :: filename

  ! Message identifier.
  integer            :: igrib

  ifile=5

  call grib_open_file(ifile, &
       '../../data/reduced_latlon_surface.grib1','R')

  ! Loop on all the messages in a file.
  call grib_new_from_file(ifile,igrib,iret)

  do while (iret/=GRIB_END_OF_FILE)
    count1=count1+1
    print *, "===== Message #",count1
    call grib_get(igrib,'numberOfPoints',numberOfPoints)
    call grib_get(igrib,'bitmapPresent',bitmapPresent)

    allocate(lats(numberOfPoints))
    allocate(lons(numberOfPoints))
    allocate(values(numberOfPoints))
    if (bitmapPresent == 1) then
      ! get the bitmap
      call grib_get_size(igrib, 'bitmap', bmp_len)
      allocate(bitmap(bmp_len))
      call grib_get(igrib,'bitmap', bitmap)
    end if

    call grib_get_data(igrib,lats,lons,values)

    do i=1,numberOfPoints
      ! Consult bitmap to see if the i'th value is missing
      is_missing_value=.false.
      if (bitmapPresent == 1 .and. bitmap(i) == 0) then
        is_missing_value=.true.
      end if
      ! Only print non-missing values
      if (.not. is_missing_value) then
        print *, lats(i),lons(i),values(i)
        count2=count2+1
      end if
    enddo
    print *, 'count of non-missing values=',count2
    if (count2 /= 214661) then
      call grib_check(-2, 'incorrect number of missing', '')
    end if

    deallocate(lats)
    deallocate(lons)
    deallocate(values)

    call grib_release(igrib)
    call grib_new_from_file(ifile,igrib, iret)

  end do 

  call grib_close_file(ifile)

end program 
