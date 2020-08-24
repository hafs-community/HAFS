module sia_fileop
  use iso_c_binding
  implicit none
  private
  public :: rm, rmdir, mkdir, chmod, umask, copy, symlink, hardlink
  public :: makedirs, basename, dirname, pathjoin, rename, chdir
  public :: mkdtemp

  integer(kind=c_int64_t), parameter :: COPY_GID=1, COPY_TIME=2, COPY_MODE=4
  integer(kind=c_int64_t), parameter :: FILE_COPY_DEFAULT=6 ! bitwise or of all other COPY_*

  integer(kind=c_int64_t), parameter :: FOLLOW_LINK=256, IN_PLACE=512
  integer(kind=c_int64_t), parameter :: TREE_COPY_DEFAULT=FILE_COPY_DEFAULT

  character(len=1), target :: dot='.'

  interface
     subroutine sia_c_copy(fromfile,tofile,flags,blocksize,ierr) bind(c)
       use iso_c_binding
       character(kind=c_char) :: fromfile(*),tofile(*)
       integer(kind=c_int64_t) :: ierr
       integer(kind=c_int64_t),value :: flags,blocksize
     end subroutine sia_c_copy
  end interface

  interface
     subroutine sia_c_symlink(fromfile,tofile,ierr) bind(c)
       use iso_c_binding
       character(kind=c_char) :: fromfile(*),tofile(*)
       integer(kind=c_int64_t) :: ierr
     end subroutine sia_c_symlink
  end interface

  interface
     subroutine sia_c_hardlink(fromfile,tofile,ierr) bind(c)
       use iso_c_binding
       character(kind=c_char) :: fromfile(*),tofile(*)
       integer(kind=c_int64_t) :: ierr
     end subroutine sia_c_hardlink
  end interface

  interface ! unlink = remove file in unix-speak
     subroutine sia_c_unlink(filename,ierr) bind(c)
       use iso_c_binding
       character(kind=c_char) :: filename(*)
       integer(kind=c_int64_t) :: ierr
     end subroutine sia_c_unlink
  end interface

  interface
     subroutine sia_c_mkdir(filename,mode,ierr) bind(c)
       use iso_c_binding
       character(kind=c_char) :: filename(*)
       integer(kind=c_int64_t),value :: mode
       integer(kind=c_int64_t) :: ierr
     end subroutine sia_c_mkdir
  end interface

  interface
     subroutine sia_c_makedirs(filename,mode,ierr) bind(c)
       use iso_c_binding
       character(kind=c_char) :: filename(*)
       integer(kind=c_int64_t),value :: mode
       integer(kind=c_int64_t) :: ierr
     end subroutine sia_c_makedirs
  end interface

  interface
     subroutine sia_c_chmod(filename,mode,ierr) bind(c)
       use iso_c_binding
       character(kind=c_char) :: filename(*)
       integer(kind=c_int64_t),value :: mode
       integer(kind=c_int64_t) :: ierr
     end subroutine sia_c_chmod
  end interface

  interface
     subroutine sia_c_rmdir(filename,ierr) bind(c)
       use iso_c_binding
       character(kind=c_char) :: filename(*)
       integer(kind=c_int64_t) :: ierr
     end subroutine sia_c_rmdir
  end interface

  interface
     function sia_c_umask(mask) bind(c)
       use iso_c_binding
       integer(kind=c_int64_t),value :: mask
       integer(kind=c_int64_t) :: sia_c_umask
     end function sia_c_umask
  end interface

  interface
     subroutine sia_c_mkdtemp(templ,ierr) bind(c)
       use iso_c_binding
       character(kind=c_char) :: templ(*)
       integer(kind=c_int64_t) :: ierr
     end subroutine sia_c_mkdtemp
  end interface

  interface
     subroutine sia_c_chdir(todir,ierr) bind(c)
       use iso_c_binding
       character(kind=c_char) :: todir(*)
       integer(kind=c_int64_t) :: ierr
     end subroutine sia_c_chdir
  end interface

  interface
     subroutine sia_c_rename(fromname,toname,ierr) bind(c)
       use iso_c_binding
       character(kind=c_char) :: fromname(*),toname(*)
       integer(kind=c_int64_t) :: ierr
     end subroutine sia_c_rename
  end interface

contains

  function symlink(fromfile,tofile)
    character*(*), intent(in) :: fromfile,tofile
    character(kind=c_char) :: cfrom(len(fromfile)+1)
    character(kind=c_char) :: cto(len(tofile)+1)
    integer(kind=c_int64_t) :: symlink
    integer :: i

    do i=1,len(fromfile)
       cfrom(i)=fromfile(i:i)
    enddo
    cfrom(i)=c_null_char

    do i=1,len(tofile)
       cto(i)=tofile(i:i)
    enddo
    cto(i)=c_null_char

    call sia_c_symlink(cfrom,cto,symlink)
  end function symlink

  function hardlink(fromfile,tofile)
    character*(*), intent(in) :: fromfile,tofile
    character(kind=c_char) :: cfrom(len(fromfile)+1)
    character(kind=c_char) :: cto(len(tofile)+1)
    integer(kind=c_int64_t) :: hardlink
    integer :: i

    do i=1,len(fromfile)
       cfrom(i)=fromfile(i:i)
    enddo
    cfrom(i)=c_null_char

    do i=1,len(tofile)
       cto(i)=tofile(i:i)
    enddo
    cto(i)=c_null_char

    call sia_c_hardlink(cfrom,cto,hardlink)
  end function hardlink

  function copy(fromfile,tofile,flags,blocksize)
    character*(*), intent(in) :: fromfile,tofile
    integer(kind=c_int64_t), intent(in), optional :: flags,blocksize
    integer(kind=c_int64_t) :: cflags,cblocksize
    character(kind=c_char) :: cfrom(len(fromfile)+1)
    character(kind=c_char) :: cto(len(tofile)+1)
    integer(kind=c_int64_t) :: copy
    integer :: i

    do i=1,len(fromfile)
       cfrom(i)=fromfile(i:i)
    enddo
    cfrom(i)=c_null_char

    do i=1,len(tofile)
       cto(i)=tofile(i:i)
    enddo
    cto(i)=c_null_char

    if(present(flags)) then
       cflags=flags
    else
       cflags=FILE_COPY_DEFAULT
    endif

    if(present(blocksize)) then
       cblocksize=blocksize
    else
       cblocksize=-1  ! -1 = choose automatically
    endif

13  format('Copy from="',A,'" to "',A,'" flags ',I0,' block size ',I0,'.')
    !print 13,fromfile,tofile,cflags,cblocksize
    call sia_c_copy(cfrom,cto,cflags,cblocksize,copy)
  end function copy

  subroutine chdir(todir,ierr)
    use iso_c_binding
    character(len=*), intent(in) :: todir
    integer(kind=c_int64_t), intent(out) :: ierr
    character :: todir0(len(todir)+1)
    integer(kind=c_int64_t) :: i
    do i=1,len(todir)
       todir0(i)=todir(i:i)
    enddo
    todir0(i)=c_null_char
    call sia_c_chdir(todir0,ierr)
  end subroutine chdir

  subroutine mkdtemp(templ,ierr)
    use iso_c_binding
    character(len=*), intent(inout) :: templ
    integer(kind=c_int64_t), intent(out) :: ierr
    character :: templ0(len(templ)+1)
    integer(kind=c_int64_t) :: i
    do i=1,len(templ)
       templ0(i)=templ(i:i)
    enddo
    templ0(i)=c_null_char
    call sia_c_mkdtemp(templ0,ierr)
    do i=1,len(templ)
       templ(i:i)=templ0(i)
    enddo
  end subroutine mkdtemp

  function umask(mask)
    integer,intent(in) :: mask
    integer :: umask
    integer(kind=c_int64_t) :: imask,omask
    imask=mask
    omask=sia_c_umask(imask)
    umask=omask
  end function umask

  function mkdir(filename,mode)
    use iso_c_binding
    character*(*), intent(in) :: filename
    integer, intent(in) :: mode

    character(kind=c_char) :: fn0(len(filename)+1)
    integer(kind=c_int64_t) :: ierr
    integer(kind=c_int64_t) :: mkdir,imode
    integer ::i

    do i=1,len(filename)
       fn0(i)=filename(i:i)
    enddo
    fn0(i)=c_null_char
    imode=mode
    call sia_c_mkdir(fn0,imode,ierr)
    mkdir=ierr
  end function mkdir

  function makedirs(filename,mode)
    use iso_c_binding
    character*(*), intent(in) :: filename
    integer, intent(in) :: mode

    character(kind=c_char) :: fn0(len(filename)+1)
    integer(kind=c_int64_t) :: ierr
    integer(kind=c_int64_t) :: makedirs,imode
    integer ::i

    do i=1,len(filename)
       fn0(i)=filename(i:i)
    enddo
    fn0(i)=c_null_char
    imode=mode
    call sia_c_makedirs(fn0,imode,ierr)
    makedirs=ierr
  end function makedirs

  function chmod(filename,mode)
    use iso_c_binding
    character*(*), intent(in) :: filename
    integer, intent(in) :: mode

    character(kind=c_char) :: fn0(len(filename)+1)
    integer(kind=c_int64_t) :: ierr
    integer(kind=c_int64_t) :: chmod,imode
    integer :: i

    do i=1,len(filename)
       fn0(i)=filename(i:i)
    enddo
    fn0(i)=c_null_char
    imode=mode
    call sia_c_chmod(fn0,imode,ierr)
    chmod=ierr
  end function chmod

  subroutine rename(fromname,toname,ierr)
    use iso_c_binding
    integer(kind=c_int64_t) :: ierr,i
    character*(*), intent(in) :: fromname,toname
    character :: fromname0(len(fromname)+1)
    character :: toname0(len(toname)+1)

    do i=1,len(fromname)
       fromname0(i) = fromname(i:i)
    enddo
    fromname0(i)=c_null_char

    do i=1,len(toname)
       toname0(i) = toname(i:i)
    enddo
    toname0(i)=c_null_char

    call sia_c_rename(fromname0,toname0,ierr)
  end subroutine rename

  function rm(filename)
    use iso_c_binding
    character*(*), intent(in) :: filename
    character(kind=c_char) :: fn0(len(filename)+1)
    integer(kind=c_int64_t) :: ierr
    integer(kind=c_int64_t) :: rm
    integer i
    do i=1,len(filename)
       fn0(i)=filename(i:i)
    end do
    fn0(i)=c_null_char
    call sia_c_unlink(fn0,ierr)
    rm=ierr
  end function rm

  function rmdir(filename)
    use iso_c_binding
    character*(*), intent(in) :: filename
    character(kind=c_char) :: fn0(len(filename)+1)
    integer(kind=c_int64_t) :: ierr
    integer(kind=c_int64_t) :: rmdir
    integer i
    do i=1,len(filename)
       fn0(i)=filename(i:i)
    end do
    fn0(i)=c_null_char
    call sia_c_rmdir(fn0,ierr)
    rmdir=ierr
  end function rmdir

  function basename(fullname)
    character*(*), intent(in), target :: fullname
    character(len=:), pointer :: basename, noslash
    integer islash,iflen,i
    iflen=len(fullname)

    ! Find the last non-/ character in fullname:
    i=iflen
    do while(fullname(i:i)=='/')
       i=i-1
       if(i<1) exit
    end do

    ! In the special case of a fullname of all "/" return "/":
    if(i==0) then
       basename=>fullname(1:1)
       return
    endif

    noslash=>fullname(1:i)

    islash=index(noslash,'/',.true.)
    if(islash==0) then            ! one => one
       basename=>noslash
    else                          ! /one/two/three => three
       basename=>fullname(islash+1:i)
    endif
  end function basename

  function pathjoin(dirname,basename)
    character*(*), intent(in) :: dirname,basename
    character(len=:), pointer :: pathjoin
    integer :: dlen,blen,jlen
    dlen=len(dirname)
    blen=len(basename)
    if(blen<1) then
       if(dlen<1) then
          ! Special case: everything is blank.  Return .
          allocate(character(len=1) :: pathjoin)
          pathjoin='.'
       else
          ! Special case: no basename.  Return a copy of dirname.
          allocate(character(len=dlen) :: pathjoin)
          pathjoin=dirname
       endif
    elseif(basename(1:1)=='/') then
       ! Special case: basename is absolute.  Return a copy of basename.
       allocate(character(len=blen) :: pathjoin)
       pathjoin=basename
    else
       ! Append basename to dirname, possibly appending a slash
       if(dirname(dlen:dlen)=='/') then
          ! Do not need to append a slash; the dirname has one.
          jlen=dlen+blen
       else
          ! Must append a slash.
          jlen=dlen+blen+1
       endif
       allocate(character(len=jlen) :: pathjoin)
       pathjoin(1:jlen-blen-1)=dirname(1:jlen-blen-1)
       pathjoin(jlen-blen:jlen-blen)='/'
       pathjoin(jlen-blen+1:jlen)=basename
    endif
  end function pathjoin

  function dirname(fullname)
    character*(*), intent(in), target :: fullname
    character(len=:), pointer :: dirname, noslash
    integer islash,iflen,i,inotslash
    iflen=len(fullname)

    if(iflen<1) then
       dirname=>dot
       return
    endif

    ! Find the last non-/ character in fullname:
    i=iflen
    do while(fullname(i:i)=='/')
       i=i-1
       if(i<1) exit
    end do

    ! In the special case of a fullname of all "/" return "/":
    if(i==0) then
       dirname=>fullname(1:1)
       return
    endif

    noslash=>fullname(1:i)

    islash=index(noslash,'/',.true.)
    do inotslash=islash,1,-1
       if(noslash(inotslash:inotslash)/='/') then
          exit
       endif
    enddo
    if(islash==0) then            ! one => .
       dirname=>dot
    elseif(inotslash==0) then
       ! Special case: direct child of root
       dirname=>fullname(islash:islash)
    else                          ! /one/two/three => /one/two
       dirname=>fullname(1:inotslash)
    endif
  end function dirname
end module sia_fileop
