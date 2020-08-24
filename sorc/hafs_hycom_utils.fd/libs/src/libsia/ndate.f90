program ndate
  use sia_cmdarg
  use sia_time
  type(args) :: argv
  type(tm) :: t,t2
  integer :: nhours, ret
  character(len=10) :: s
  call argv%getargs()
  if(size(argv%a)>2) then
     call usage("ZERO, ONE OR TWO ARGUMENTS ARE REQUIRED")
  endif

  if(size(argv%a)<1) then
     call t%localtime()
     call t%strftime('%Y%m%d%H',s)
  else
     ret=t%strptime("%Y%m%d%H",argv%a(size(argv%a))%s)
     if(ret<8) then
        print *,ret,argv%a(size(argv%a))%s
        call usage("DATE MUST BE AT LEAST 8 DIGITS")
     endif
     if(size(argv%a)>1) then
        nhours=0
        read(argv%a(1)%s,'(I8)') nhours
        t2=t%add(nhours*3600)
        call t2%strftime('%Y%m%d%H',s)
     else
        call t%strftime("%Y%m%d%H",s)
     endif
  endif
  print '(A)',s
contains
  subroutine usage(why)
    character*(*) :: why
10  format(A)
    write(0,10) 'ERROR: Format: ndate [hours] YYYYMMDDHH'
    write(0,10) why
    stop 1
  end subroutine usage
end program ndate
