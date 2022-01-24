subroutine open_grads(label,nlon,nlat,nsig,startx,starty,xinc,yinc)

  character(*) label
  integer nlon,nlat,nsig
  integer i,k,ntime
  real(4) undef
  character(1) blank
  character(80) datdes(15)
  integer unit_des
  character(80) label_dat
  character(80) label_des
  real(4) startx,starty,startp,xinc,yinc,pinc

  blank=' '
  undef=-9.99e33

! create names of grads control and data files

  write(label_des,'(a,".ctl")')trim(label)
  write(label_dat,'(a)')trim(label)

! find unused unit number

  unit_des=80
  print *,'startx, starty, xinc, yinc=', startx, starty, xinc, yinc

! initialize counters for this set of output fields

  startp=1.
  pinc=1.
  ntime=1
  do i=1,15
     write(datdes(i),'(80a1)')(blank,k=1,80)
  end do
  write(datdes(1),'("DSET ",a)')trim(label_dat)
  write(datdes(2),'("options big_endian sequential")')
  write(datdes(3),'("TITLE ",a)')trim(label)
  write(datdes(4),'("UNDEF ",e11.2)')undef
  write(datdes(5),'("XDEF ",i5," LINEAR ",f7.2,f7.2)')nlon,startx,xinc
  write(datdes(6),'("YDEF ",i5," LINEAR ",f7.2,f7.2)')nlat,starty,yinc
  write(datdes(7),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')nsig,startp,pinc
  write(datdes(8),'("TDEF ",i5," LINEAR 0Z23may1992 24hr")')ntime
  write(datdes(9),'("VARS 5")')
  write(datdes(10),'("u    ",i5," 99 u    ")')nsig
  write(datdes(11),'("v    ",i5," 99 v    ")')nsig
  write(datdes(12),'("t    ",i5," 99 t    ")')nsig
  write(datdes(13),'("q    ",i5," 99 q    ")')nsig
  write(datdes(14),'("slp  ",i5," 99 slp  ")')1
  write(datdes(15),'("ENDVARS")')
! write out datdes

  write(*,'(a80)')datdes(7)

  open(unit_des,file=label_des,form='formatted')
  rewind unit_des
  write(unit_des,'(a80)')datdes
  close(unit_des)

end subroutine open_grads

subroutine load(a,ni,nj,nk,n,dum)

  integer :: ni,nj,nk,n,i,j
  real(4) :: a(ni,nj,nk)
  real(4) :: dum(ni,nj)

  do j=1,nj
     do i=1,ni
        dum(i,j)=a(i,j,n)
     end do
  end do

  return
end subroutine load

