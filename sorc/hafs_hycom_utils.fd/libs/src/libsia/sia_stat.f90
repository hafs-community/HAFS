module sia_stat
  use iso_c_binding
  use sia_const
  private

  public :: modestring

  type, public :: statbuf ! C struct stat and associated routines
     integer(kind=c_int64_t) :: dev=-1,ino=-1,mode=-1,nlink=0,uid=-1,gid=-1,&
          rdev=0,size=0,blksize=0,blocks=0,atime=-1,mtime=-1,ctime=-1
     ! Real versions of timestamps, with fractional part if needed:
     real(kind=c_double) :: artime=0, mrtime=0, crtime=0
     logical :: valid=.false.
   contains
     procedure :: stat=>statbuf_stat
     procedure :: lstat=>statbuf_lstat
     procedure :: clear=>statbuf_clear
     procedure :: isreg=>statbuf_isreg
     procedure :: islnk=>statbuf_islnk
     procedure :: isdir=>statbuf_isdir
     procedure :: perms=>statbuf_permissions
     procedure :: strkind=>statbuf_strkind
  end type statbuf

  interface
     subroutine c_stat_lstat(filename,n,statptr,ierr, &
          dev,ino,mode,nlink,uid,gid,rdev,size,blksize,blocks,&
          atime,mtime,ctime,artime,mrtime,crtime) bind(c)
       use iso_c_binding
       character(kind=c_char) :: filename(*)
       type(c_ptr) :: statptr
       integer(kind=c_int64_t),value,intent(in) :: n ! = 1 for lstat, = 0 for stat
       integer(kind=c_int64_t),intent(out) :: ierr
       real(kind=c_double), intent(out) :: artime,mrtime,crtime
       integer(kind=c_int64_t),intent(out) :: &
            dev,ino,mode,nlink,uid,gid,rdev,size,blksize,blocks,&
            atime,mtime,ctime
     end subroutine c_stat_lstat
  end interface
contains

  subroutine modestring(mode,string)
    use iso_c_binding, only: c_int64_t
    integer(kind=c_int64_t), intent(in) :: mode
    character(len=10), intent(out) :: string
    string='----------'

    if(0/=iand(mode,S_IRUSR)) string(2:2)='r'
    if(0/=iand(mode,S_IRGRP)) string(5:5)='r'
    if(0/=iand(mode,S_IROTH)) string(8:8)='r'

    if(0/=iand(mode,S_IWUSR)) string(3:3)='w'
    if(0/=iand(mode,S_IWGRP)) string(6:6)='w'
    if(0/=iand(mode,S_IWOTH)) string(9:9)='w'

    if(0/=iand(mode,S_IXUSR)) then
       if(0/=iand(mode,S_ISUID)) then
          string(4:4)='s'
       else
          string(4:4)='x'
       endif
    elseif(0/=iand(mode,S_ISUID)) then
       string(4:4)='S'
    endif

    if(0/=iand(mode,S_IXGRP)) then
       if(0/=iand(mode,S_ISGID)) then
          string(7:7)='s'
       else
          string(7:7)='x'
       endif
    elseif(0/=iand(mode,S_ISGID)) then
       string(7:7)='S'
    endif

    if(0/=iand(mode,S_IXOTH)) then
       if(0/=iand(mode,S_ISVTX)) then
          string(10:10)='t'
       else
          string(10:10)='x'
       endif
    elseif(0/=iand(mode,S_ISVTX)) then
       string(10:10)='T'
    endif

    if(S_IFDIR==iand(mode,S_IFMT)) then
       string(1:1)='d' ! Directory
    elseif(S_IFLNK==iand(mode,S_IFMT)) then
       string(1:1)='l' ! Symbolic link
    elseif(S_IFCHR==iand(mode,S_IFMT)) then
       string(1:1)='c' ! Character device
    elseif(S_IFBLK==iand(mode,S_IFMT)) then
       string(1:1)='b' ! Block device
    elseif(S_IFREG==iand(mode,S_IFMT)) then
       string(1:1)='-' ! Regular file
    elseif(S_IFIFO==iand(mode,S_IFMT)) then
       string(1:1)='p' ! FIFO (named pipe)
    elseif(S_IFSOCK==iand(mode,S_IFMT)) then
       string(1:1)='s' ! Socket
    else
       string(1:1)='?' ! Some other file type
    endif
  end subroutine modestring

  function statbuf_strkind(s) result(k)
    character(len=16) :: k
    character(len=16), parameter :: &
         invalid_str="stat failed     ",&
         regular_str="regular file    ",&
         link_str   ="symbolic link   ",&
         socket_str ="socket          ",&
         char_str   ="character device",&
         block_str  ="block device    ",&
         fifo_str   ="fifo/named pipe ",&
         dir_str    ="directory       ",&
         unknown_str="unknown type    "
    class(statbuf), intent(in) :: s
    if(.not.s%valid) then                       ; k=invalid_str
    elseif(iand(s%mode,S_IFMT)==S_IFSOCK) then  ; k=socket_str
    elseif(iand(s%mode,S_IFMT)==S_IFDIR) then   ; k=dir_str
    elseif(iand(s%mode,S_IFMT)==S_IFLNK) then   ; k=link_str
    elseif(iand(s%mode,S_IFMT)==S_IFREG) then   ; k=regular_str
    elseif(iand(s%mode,S_IFMT)==S_IFBLK) then   ; k=block_str
    elseif(iand(s%mode,S_IFMT)==S_IFCHR) then   ; k=char_str
    elseif(iand(s%mode,S_IFMT)==S_IFIFO) then   ; k=fifo_str
    else                                        ; k=unknown_str
    endif
  end function statbuf_strkind

  function statbuf_permissions(s)
    use iso_c_binding, only: c_null_char,c_char,c_int64_t
    class(statbuf), intent(inout) :: s
    integer(kind=c_int64_t) :: statbuf_permissions
    statbuf_permissions=iand(s%mode,4095)
  end function statbuf_permissions

  logical function statbuf_isreg(s)
    class(statbuf), intent(inout) :: s
    statbuf_isreg = s%valid .and. 0/=iand(s%mode,S_IFREG)
  end function statbuf_isreg

  logical function statbuf_isdir(s)
    class(statbuf), intent(inout) :: s
    statbuf_isdir = s%valid .and. 0/=iand(s%mode,S_IFDIR)
  end function statbuf_isdir

  logical function statbuf_islnk(s)
    class(statbuf), intent(inout) :: s
    statbuf_islnk = s%valid .and. 0/=iand(s%mode,S_IFLNK)
  end function statbuf_islnk

  logical function statbuf_stat(s,filename,statptr)
    use iso_c_binding
    class(statbuf), intent(inout) :: s
    character(len=*) :: filename
    character(kind=c_char) :: filename0(len(filename)+1)
    integer(kind=c_int64_t) :: ierr,n,i
    type(c_ptr), intent(out), optional :: statptr
    type(c_ptr) :: istatptr
    do i=1,len(filename)
       filename0(i)=filename(i:i)
    enddo
    filename0(i)=c_null_char
    istatptr=c_null_ptr
    n=0
    if(present(statptr)) n=n+2
    call c_stat_lstat(filename0,n,istatptr,ierr,&
         s%dev,s%ino,s%mode,s%nlink,s%uid,s%gid,s%rdev,s%size,s%blksize,&
         s%blocks,s%atime,s%mtime,s%ctime,s%artime,s%mrtime,s%crtime)
    if(present(statptr)) statptr=istatptr
    s%valid = ierr==0
    statbuf_stat=s%valid
  end function statbuf_stat

  logical function statbuf_lstat(s,filename,lstatptr)
    use iso_c_binding
    class(statbuf), intent(inout) :: s
    character(len=*) :: filename
    character(kind=c_char) :: filename0(len(filename)+1)
    integer(kind=c_int64_t) :: ierr,n,i
    type(c_ptr), intent(out), optional :: lstatptr
    type(c_ptr) :: istatptr
18  format(A,': f stat len=',I0)
    !print 18,filename,len(filename)
    do i=1,len(filename)
       filename0(i)=filename(i:i)
    enddo
    filename0(i)=c_null_char
    istatptr=c_null_ptr
    n=1
    if(present(lstatptr)) n=n+2
    call c_stat_lstat(filename0,n,istatptr,ierr,&
         s%dev,s%ino,s%mode,s%nlink,s%uid,s%gid,s%rdev,s%size,s%blksize,&
         s%blocks,s%atime,s%mtime,s%ctime,s%artime,s%mrtime,s%crtime)
    if(present(lstatptr)) lstatptr=istatptr
    s%valid = ierr==0
    statbuf_lstat=s%valid
13  format(A,': lstat failed: ',I0)
23  format(A,': lstat success: ',I0)
    if(statbuf_lstat) then
       !print 23,filename,ierr
    else
       !print 13,filename,ierr
    endif
  end function statbuf_lstat

  subroutine statbuf_clear(s)
    class(statbuf), intent(out) :: s
    integer :: ierr
    s%valid=.false.
    s%dev=-1
    s%ino=-1
    s%mode=0
    s%nlink=0
    s%uid=-1
    s%gid=-1
    s%rdev=0
    s%size=0
    s%blksize=0
    s%blocks=0
    s%atime=-1
    s%mtime=-1
    s%ctime=-1 
  end subroutine statbuf_clear


end module sia_stat
