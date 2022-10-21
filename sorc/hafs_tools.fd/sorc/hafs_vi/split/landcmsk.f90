subroutine landcmsk(IK,JK,GLON,GLAT,ZDATG,IFLAG,lsflag,kst)
use tr, only: ING, JNG, IB
use nhc2
use nhc3
use stname
use fileconst
implicit none
integer,intent(in)::IK,JK
integer::lsflag,iflag,i,j,iw,jw,kst
real,DIMENSION(ik,jk):: ZDATG,GLON,GLAT

lsflag = 1

DO I = 1,IB
  IW = ING(I)
  JW = JNG(I)
  IF(ZDATG(IW,JW).gt.500.)then
    iflag = 1
!cnew    MDX=0
!cnew    MDY=0
!cnew    AMDX=0.
!cnew    AMDY=0.
    print*,' Filter domain topography height > 500 m' &
          ,', storm name = ', ST_NAME(KST),          &
          ', forecast time = ',ITIM,'h',             &
     ', only wind field is relocated'
    exit
  END IF
END DO

return
end subroutine landcmsk

