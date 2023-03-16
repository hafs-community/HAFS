program file_test
  use iso_c_binding
  use sia_fileop
  implicit none
  character*1000 :: filename,op
  integer :: nargs,arglen,iarg
  logical :: success

  op='none'
  nargs=command_argument_count()
  success=.true.
  if(nargs<2) &
       call usage('ABORTING: At least two arguments are required')
  do iarg=1,nargs
     call get_command_argument(iarg,filename)
     if(len(filename)>=4 .and. filename(1:3)=='op=') then
        op=filename(4:)
     else
        select case(trim(op))
        case('umask')
           call do_umask(trim(filename))
        case('mkdir') 
           success=success .and. do_mkdir(trim(filename))
        case('makedirs') 
           success=success .and. do_makedirs(trim(filename))
        case('rmdir')
           success=success .and. do_rmdir(trim(filename))
        case('rm')
           success=success .and. do_rm(trim(filename))
        case('chmod')
           success=success .and. do_chmod(trim(filename))
        case('none')
           call usage('You must specify the operation before the operands.')
        case default
           call usage('Invalid operation "'//trim(op)//'"')
        end select
     endif
  end do
  if( .not. success) stop 1

contains

  logical function do_mkdir(arg)
    character(len=*),intent(in) :: arg
    integer :: mode,iname
2   format('mkdir mode 0',O0,' ',A)
    call modename(arg,mode,iname)
    if(iname>len(arg) .or. iname<=0) &
       call usage('Invalid mkdir operand: "'//arg//'"')
    print 2,mode,arg(iname:len(arg))
    do_mkdir= 0==mkdir(arg(iname:len(arg)),mode)
  end function do_mkdir

  logical function do_makedirs(arg)
    character(len=*),intent(in) :: arg
    integer :: mode,iname
2   format('makedirs mode 0',O0,' ',A)
    call modename(arg,mode,iname)
    if(iname>len(arg) .or. iname<=0) &
       call usage('Invalid makedirs operand: "'//arg//'"')
    print 2,mode,arg(iname:len(arg))
    do_makedirs= 0==makedirs(arg(iname:len(arg)),mode)
  end function do_makedirs

  logical function do_chmod(arg)
    character(len=*),intent(in) :: arg
    integer :: mode,iname
2   format('chmod mode 0',O0,' ',A)
    call modename(arg,mode,iname)
    if(iname>len(arg) .or. iname<=0) &
       call usage('Invalid chmod operand: "'//arg//'"')
    print 2,mode,arg(iname:len(arg))
    do_chmod= 0==chmod(arg(iname:len(arg)),mode)
  end function do_chmod

  subroutine do_umask(str)
    character(len=*), intent(in) :: str
    integer :: imode,omode
    imode=mode(trim(str))
    if(imode<0) then
       call usage('Invalid mode "'//str//'"')
    endif
    omode=umask(imode)
2   format('Changed umask from 0',O0,' to 0',O0)
    print 2,omode,imode
  end subroutine do_umask

  integer function mode(str)
    character(len=*), intent(in) :: str
    integer, parameter :: zero=48, nine=57
    integer :: i,c
    print *,'Get mode from "',str,'"'
    mode=0
    do i=1,len(str)
       c=ichar(str(i:i))
       if(c<zero .or. c>nine) then
9         format('Bad mode char "',A1,'"')
          print 9,c
          mode=-1
          return
       endif
       mode=mode*8 + c-zero
    enddo
    print '("Mode is 0",O0)',mode
  end function mode

  subroutine modename(str,omode,iname)
    character(len=*), intent(in) :: str
    integer, intent(inout) :: omode,iname

    iname=index(str,':')+1
    if(iname<=0) return

    omode=mode(str(1:iname-2))
9   format('Got mode back as 0',O0)
    print 9,omode
    if(omode<0) then
3      format('Mode 0',O0,'<0 so invalid string')
       print 3,omode
       iname=-1
       return
    endif
    print '(A," = 0",O0)',str(1:iname-2),omode
  end subroutine modename

  logical function do_rm(arg)
    character(len=*), intent(in) :: arg
2   format('rm ',A)
    print 2,arg
    do_rm = 0==rm(arg)
  end function do_rm

  logical function do_rmdir(arg)
    character(len=*), intent(in) :: arg
2   format('rmdir ',A)
    print 2,arg
    do_rmdir = 0==rmdir(arg)
  end function do_rmdir

  subroutine usage(why)
    character(len=*) :: why
1   format(A)
    print 1,'Usage: file_test op=operation file1 [file2 [...]] \ '
    print 1,'   [op=operation2 file1 [file2 [...]] [...] ]'
    print 1,'Operations:'
    print 1,'  op=mkdir mode:dir    --- make directory with specified mode'
    print 1,'  op=makedirs mode:dir --- make directory and parents with specified mode'
    print 1,'  op=rmdir dir         --- delete empty directory'
    print 1,'  op=rm    filename    --- delete file'
    print 1,'  op=chmod mode:path   --- change file mode'
    print 1,'  op=umask mask        --- change process umask'
    print 1,why
    stop 1
  end subroutine usage

end program file_test
