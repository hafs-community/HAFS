module mod_dump
 private
 public dumpr,dumpi
contains
!
!========================================================================
!
  subroutine dumpr(lu,a,nx,ny,fname)
    implicit none
    integer, intent(in) :: lu,nx,ny
    real, intent(in) :: a(nx,ny)
    integer :: i,j
    character*(*) fname
    write(*,*)'dumpr: '//trim(fname)//': max,min=',maxval(a),minval(a)
    open(unit=lu,file=trim(fname),form="formatted")
    write(lu,'(4f15.6)') ((a(i,j),i=1,nx),j=1,ny)
    close (lu)
    return
  end subroutine 
!
!========================================================================
!
  subroutine dumpi(lu,a,nx,ny,fname)
    implicit none
    integer, intent(in) :: lu,nx,ny
    integer, intent(in) :: a(nx,ny)
    integer :: i,j
    character*(*) fname
    write(*,*)'dumpi: '//trim(fname)//': max,min=',maxval(a),minval(a)
    open(unit=lu,file=trim(fname),form="formatted")
    write(lu,'(30i2)') ((a(i,j),i=1,nx),j=1,ny)
    close (lu)
  return
  end subroutine 
!
!===========================================================================================
!
end module mod_dump
