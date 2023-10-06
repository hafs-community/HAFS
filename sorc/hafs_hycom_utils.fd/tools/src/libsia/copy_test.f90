program copy_test
  use iso_c_binding
  use sia_fileop
  implicit none
  character*1000 :: fromfile,tofile, op
  integer :: nargs,ierr, ito, ifrom

  nargs=command_argument_count()
  if(nargs==3) then
     ifrom=2
     ito=3
     call get_command_argument(1,op)
  elseif(nargs==2) then
     ifrom=1
     ito=2
     op="copy"
  else
     call usage('Exactly two arguments are required.')
  endif

  call get_command_argument(ifrom,fromfile)
  call get_command_argument(ito,tofile)

8 format('Will ',A,' from "',A,'" to "',A,'".')
  print 8,trim(op),trim(fromfile),trim(tofile)

  select case(trim(op))
  case('copy')
     ierr=copy(trim(fromfile),trim(tofile))
  case('symlink')
     ierr=symlink(trim(fromfile),trim(tofile))
  case('hardlink')
     ierr=hardlink(trim(fromfile),trim(tofile))
  case default
     call usage('Invalid operation "'//trim(op)//'"')
  end select

  if(ierr/=0) then
9    format(A,'=>',A,': could not ',A,'. Status ',I0,'.')
     print 9,trim(fromfile),trim(tofile),trim(op),ierr
     stop 1
  endif
contains
  subroutine usage(why)
    character*(*) :: why
    print 2,'Usage: copy_test fromfile tofile'
    print 2,'  Copies fromfile to tofile.  Will also copy timestamps, group'
    print 2,'  and mode (permissions).'
    print 2,trim(why)
2   format(A)
    stop 1
  end subroutine usage
end program copy_test
