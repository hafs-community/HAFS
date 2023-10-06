program time_test
  use sia_time
  implicit none
  real(kind=8) :: when
  type(tm) :: t
  character*100 :: s,fmt
  integer :: nargs

  nargs=command_argument_count()

  when=hrtime()
  print 19,when
19 format('Seconds since 1970 began: ',F23.9)
  if(nargs>=1) then
     call get_command_argument(1,fmt)
  else
     fmt='%Y-%m-%d %H:%M:%S'
  endif

14 format('gmtime(',I0,')    = "',A,'"')
24 format('localtime(',I0,') = "',A,'"')

  call t%gmtime(int(when,kind=8))
  call t%strftime(fmt,s)
  print 14,int(when,kind=8),trim(s)

  call t%localtime(int(when,kind=8))
  call t%strftime(fmt,s)
  print 24,int(when,kind=8),trim(s)
end program time_test
