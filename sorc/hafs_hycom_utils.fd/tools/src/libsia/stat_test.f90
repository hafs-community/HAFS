program stat_test
  use sia_stat
  use sia_usrgrp
  use sia_time
  use sia_const
  implicit none

  character*1000 :: filename
  integer :: nargs,iarg
  logical :: success

  nargs=command_argument_count()
  success=.true.
  do iarg=1,nargs
     call get_command_argument(iarg,filename)
     success=success .and. test_file(trim(filename))
  end do
  if( .not. success) stop 1

contains
  logical function test_file(filename)
    use iso_c_binding, only: c_char
    character*(*) :: filename
    type(group) :: gr
    type(tm) :: t
    type(statbuf) :: s
    type(passwd) :: pw
    character(len=10) :: mode
    character(len=100) :: groupname,username,astring,mstring,cstring
    character(kind=c_char,len=9999) :: groupbuf
    integer :: imember

    test_file=s%stat(filename)
    if(.not.test_file) then
19     format(A,': cannot stat')
       write(0,19) filename
       return
    end if

    call modestring(s%mode,mode)

    if(s%gid >= 0) then
       call gr%alloc()
       if(gr%getgrgid(s%gid)) then
          groupname=gr%name
       else
          write(groupname,'(I0)') s%gid
       endif
    else
          write(groupname,'(I0)') s%gid
    endif

    if(s%uid >= 0) then
       call pw%alloc()
       if(pw%getpwuid(s%uid)) then
          username=pw%name
       else
          write(username,'(I0)') s%uid
       endif
    else
          write(username,'(I0)') s%uid
    endif

    call t%localtime(s%atime)
    call t%strftime('%Y-%m-%d %H:%M:%S',astring)
    call t%localtime(s%mtime)
    call t%strftime('%Y-%m-%d %H:%M:%S',mstring)
    call t%localtime(s%ctime)
    call t%strftime('%Y-%m-%d %H:%M:%S',cstring)

1   format("  File `",A,"'")
15  format(" Mode: 0x",Z0,"/",O10.10)
2   format("  Size: ",I0,"      Blocks: ",I0,"    IO Block: ",I0,"  ",A)
3   format("Device: ",Z0," Inode: ",I0,"   Links: ",I0)
4   format("Access: (",O4.4,"/",A,")  Uid: ",I0,"(",A,")  Gid: ",I0,' (',A,')')
5   format("Access: ",I0,' = ',A)
6   format("Modify: ",I0,' = ',A)
7   format("Change: ",I0,' = ',A)

    print 1,filename
    print 15,s%mode,s%mode
    print 2,s%size,s%blocks,s%blksize,s%strkind()
    print 3,s%dev,s%ino,s%nlink
    print 4,s%perms(),mode,s%uid,trim(username),s%gid,trim(groupname)
    print 5,s%atime,trim(astring)
    print 6,s%mtime,trim(mstring)
    print 7,s%ctime,trim(cstring)

    if(s%gid>=0) call gr%free()

  end function test_file
end program stat_test
