program glob_test
  use iso_c_binding
  use sia_glob
  implicit none
  character(kind=c_char,len=1000) :: cglob
  integer :: nargs,iarg
  logical :: success

  nargs=command_argument_count()
  success=.true.
  do iarg=1,nargs
     call get_command_argument(iarg,cglob)
     success=success .and. globby(trim(cglob))
  end do
  if( .not. success) stop 1

contains

  logical function globby(cglob)
    type(glob) :: g
    character(kind=c_char,len=*) :: cglob
    integer(kind=c_int64_t) :: n,i
    globby=g%glob(trim(cglob),0)
    if(.not.globby) goto 1000
    call g%matches()
    do i=1,g%nmatches
       print 44,i,g%match(i)%m
    enddo
44  format('match ',I0,' = "',A,'"')

1000 continue
    call g%free()
  end function globby

end program glob_test
