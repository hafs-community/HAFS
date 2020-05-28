module mod_hytime
 private
 public hytime,jdn
contains
!
!==========================================================================
!
   subroutine hytime(date,hdate)
    implicit none
    real,intent(out) :: hdate
    character, intent(in):: date*(*)
    integer :: iyear,imonth,iday,ihour,jdn,day1
    integer,save  :: day0=-1
    external jdn
! ---   model day is calendar days since 12/31/1900
    if(day0<0) call julday(day0,1900,12,31)
    read(date(1:10),'(i4,i2,i2,i2)') iyear,imonth,iday,ihour
    call julday(day1,iyear,imonth,iday)
    hdate=day1-day0+ihour/24.d0
    return
  end subroutine hytime
!
!==========================================================================
!
   subroutine julday(jdn,iyear,month,iday)
      implicit none
      integer,intent(in) :: iyear,month,iday
      integer,intent(out) :: jdn
      jdn  =    iday - 32075 &
 &          + 1461 * (iyear + 4800 + (month - 14) / 12) / 4 &
 &          + 367 * (month - 2 - (month -14) / 12 * 12) / 12 &
 &          - 3 * ((iyear + 4900 + (month - 14) / 12) / 100) / 4
      return
  end subroutine julday
!
!===========================================================================================
!
end module mod_hytime
