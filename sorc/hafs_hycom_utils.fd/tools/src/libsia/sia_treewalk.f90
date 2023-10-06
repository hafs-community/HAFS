module sia_treewalk
  use iso_c_binding
  use sia_const
  use sia_stat
  use sia_dir
  use sia_fileop
  use sia_usrgrp
  use sia_time
  implicit none
  private
  public :: treewalk, treeprint, treedelete, deltree, listtree

  integer, parameter :: max_path = 10240

  type treewalk
     integer(kind=c_int64_t), private, pointer :: touched(:,:) => NULL()
     logical, private :: am_walking=.false.
   contains
     procedure :: walk => treewalk_walk
     procedure :: free => treewalk_free

     procedure, private :: walk_dir => treewalk_walk_dir

     ! These three should be overridden by subclasses:
     procedure :: enterdir => treewalk_enterdir
     procedure :: exitdir => treewalk_exitdir
     procedure :: hitfile => treewalk_hitfile
  end type treewalk

  type, extends(treewalk) :: treeprint
   contains
     procedure :: enterdir => printdir
     procedure :: hitfile => printfile
  end type treeprint

  type, extends(treewalk) :: treedelete
     logical :: rmerror=.false.
   contains
     procedure :: hitfile => delfile
     procedure :: exitdir => deldir
  end type treedelete

contains

  logical function deltree(path)
    character(len=*), intent(in) :: path
    type(treedelete) :: td
    logical :: finish,error
    call td%walk(path,finish,error)
    deltree=.not.error .and. .not.td%rmerror
    call td%free()
  end function deltree

  logical function listtree(path)
    character(len=*), intent(in) :: path
    type(treeprint) :: tp
    logical :: finish,error
    call tp%walk(path,finish,error)
    listtree=.not.error
    call tp%free()
  end function listtree

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Private implementation functions

  subroutine treewalk_walk(self,path,finish,error)
    class(treewalk) :: self
    character(len=*), intent(in) :: path
    logical, optional, intent(out) :: finish,error
    logical :: ifinish,ierror,enter
    character(kind=c_char, len=:), pointer :: filepath
    type(statbuf) :: lstat
    integer :: i
    ifinish=.false.
    ierror=.false.
    allocate(character(kind=c_char,len=len(path)) :: filepath)
    do i=1,len(path)
       filepath(i:i)=path(i:i)
    enddo
    enter=.true.
    if(lstat%lstat(trim(path))) then
       if(iand(lstat%mode,S_IFMT) == S_IFDIR) then
          call self%enterdir(trim(path),enter,ifinish,lstat,c_null_ptr)
          if(.not.ifinish .and. enter) then
             call self%walk_dir(trim(path),ifinish,ierror)
          endif
       else
          call self%hitfile(trim(path),ifinish,lstat,c_null_ptr)
       endif
    else
       error=.true.
    endif
    if(present(finish)) finish=ifinish
    if(present(error)) error=ierror
    deallocate(filepath)
  end subroutine treewalk_walk

  recursive subroutine treewalk_walk_dir(self,path,finish,error)
    class(treewalk) :: self
    type(DIR) :: d
    character(kind=c_char,len=*), intent(in) :: path
    character(kind=c_char, len=max_path), target :: filepath
    logical, intent(inout) :: finish,error
    integer :: i, filestart, filemax
    character(kind=c_char, len=:), pointer :: direntbuf, filename, filepart
    type(statbuf) :: stat,lstat
    logical :: enter
    character(len=16) :: what

    finish=.false.

    if(.not.d%open(path)) then
9      format(A,': ERROR: cannot open directory.')
       !write(0,9) path
       error=.true.
       return
    endif
17  format('Dirent size is ',I0)
    !print 17,d%dirent_size
    allocate(character(kind=c_char, len=(d%dirent_size)) :: direntbuf)

    do i=1,len(path)
       filepath(i:i)=path(i:i)
    enddo
    filepath(i:i)='/'
    filestart=i+1
    filemax=max_path-filestart+1

18  format('Allocated direntbuf size is ',I0)
    print 18,d%dirent_size

    do while(d%read(filename,direntbuf)>0)
       if(len(filename)>filemax) then
110       format(A,': ERROR: filename too long')
          ! Filename too long
          error=.true.
          cycle
       endif
       if(filename == '.' .or. filename=='..') then
33        format(A,': skip special file')
          !print 33,filename
          cycle
       endif
       filepath(filestart:filestart+len(filename)-1) = filename
       filepart => filepath(1:filestart+len(filename)-1)
88     format(A,': lstat')
       !print 88,filepart
       what=lstat%strkind()
       if(.not.lstat%lstat(filepart)) then
          ! Cannot lstat the file
22        format(A,': ERROR: cannot lstat')
          !write(0,22) filepart
          error=.true.
          cycle
       else
          what=lstat%strkind()
          !print 104,filepart,lstat%size,lstat%mtime,trim(what)
104       format(A,': size=',I0,' mtime=',I0,' type=',A)
       endif
       if(iand(lstat%mode,S_IFMT) == S_IFDIR) then
99        format(A,': is dir.')
          !print 99,filepart
          finish=.false.
          enter=.false.
          call self%enterdir(filepart,enter,finish,lstat,c_null_ptr)
          if(finish) goto 10000 ! Cleanup and return
          if(enter) then
11           format(A,': RECURSE')
             !print 11,filepart
             call self%walk_dir(filepart,finish,error)
          endif
       else
199        format(A,': is not dir.')
           !print 199,filepart
          call self%hitfile(filepart,finish,lstat,c_null_ptr)
       endif
    enddo
    call self%exitdir(path,finish)

10000 continue ! Clean up and return
    deallocate(direntbuf)
    call d%close()
  end subroutine treewalk_walk_dir

  subroutine treewalk_free(self)
    class(treewalk) :: self
    if(self%am_walking) then
       ! We are presently walking the tree, so free nothing.
       return
    endif
    if(associated(self%touched)) then
       deallocate(self%touched)
       nullify(self%touched)
    endif
  end subroutine treewalk_free

  subroutine treewalk_enterdir(self,path,enter,finish,lstat,lstatptr)
    class(treewalk) :: self
    character*(*), intent(in) :: path
    logical, intent(out) :: enter,finish
    class(statbuf), intent(in) :: lstat
    type(c_ptr), intent(in) :: lstatptr
    enter=.true.
    finish=.false.
  end subroutine treewalk_enterdir

  subroutine treewalk_exitdir(self,path,finish)
    class(treewalk) :: self
    character*(*), intent(in) :: path
    logical, intent(out) :: finish
    finish=.false.
  end subroutine treewalk_exitdir

  subroutine treewalk_hitfile(self,path,finish,lstat,lstatptr)
    class(treewalk) :: self
    character*(*), intent(in) :: path
    logical, intent(out) :: finish
    class(statbuf), intent(in) :: lstat
    type(c_ptr), intent(in) :: lstatptr
    finish=.false.
  end subroutine treewalk_hitfile

  subroutine printdir(self,path,enter,finish,lstat,lstatptr)
    class(treeprint) :: self
    character*(*), intent(in) :: path
    logical, intent(out) :: enter,finish
    class(statbuf), intent(in) :: lstat
    type(c_ptr), intent(in) :: lstatptr
    enter=.true.
    finish=.false.
    call self%hitfile(path,finish,lstat,lstatptr)
  end subroutine printdir

  subroutine printfile(self,path,finish,lstat,lstatptr)
    class(treeprint) :: self
    character*(*), intent(in) :: path
    logical, intent(out) :: finish
    class(statbuf), intent(in) :: lstat
    type(c_ptr), intent(in) :: lstatptr

    character(len=100) :: groupname,username,astring,mstring,cstring
    character(kind=c_char,len=9999) :: groupbuf
    character(len=10) :: mode
    type(passwd) :: pw
    type(group) :: gr
    type(tm) :: t

    finish=.false.

    call modestring(lstat%mode,mode)

    if(lstat%gid >= 0) then
       call gr%alloc()
       if(gr%getgrgid(lstat%gid)) then
          groupname=gr%name
       else
          print *,'no grgid for ',lstat%gid
          write(groupname,'(I0)') lstat%gid
       endif
    else
       print *,'lstat gid less than 0 ',lstat%gid
          write(groupname,'(I0)') lstat%gid
    endif

    if(lstat%uid >= 0) then
       call pw%alloc()
       if(pw%getpwuid(lstat%uid)) then
          username=pw%name
       else
          write(username,'(I0)') lstat%uid
       endif
    else
          write(username,'(I0)') lstat%uid
    endif

    call t%localtime(lstat%mtime)
    call t%strftime('%Y-%m-%d %H:%M:%S',mstring)

18  format(A10,' ',A13,' ',A7,' ',I11,' ',A19,' ',A)
    print 18,mode,trim(username),trim(groupname),lstat%size,trim(mstring),path

    call pw%free()
    call gr%free()
  end subroutine printfile

  subroutine deldir(self,path,finish)
    class(treedelete) :: self
    character*(*), intent(in) :: path
    logical, intent(out) :: finish
    finish=.false.
8   format(A,': rmdir')
    print 8,path
    if(0/=rmdir(path)) then
       self%rmerror=.true.
       print *,'ERROR: RMDIR FAILED'
    end if
  end subroutine deldir
  
  subroutine delfile(self,path,finish,lstat,lstatptr)
    class(treedelete) :: self
    character*(*), intent(in) :: path
    logical, intent(out) :: finish
    class(statbuf), intent(in) :: lstat
    type(c_ptr), intent(in) :: lstatptr
    finish=.false.
8   format(A,': rm')
    print 8,path
    if(0/=rm(path)) then
       self%rmerror=.true.
       print *,'ERROR: RM FAILED'
    endif
  end subroutine delfile
end module sia_treewalk
