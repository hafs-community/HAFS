module sia_usrgrp
  use iso_c_binding
  implicit none
  private

  type, public :: groupmem
     character(kind=c_char,len=:), pointer :: p
  end type groupmem

  type, public :: group
     character(kind=c_char,len=:), pointer :: buf  => NULL()
     character(kind=c_char,len=:), pointer :: name => NULL()
     integer(kind=c_int64_t) :: gid = -1
     type(groupmem), pointer :: members(:) => NULL()
     integer(kind=c_int64_t) :: nmembers=-1,nbuf=-1
   contains
     procedure getgrgid => group_getgrgid
     procedure free => group_free
     procedure alloc => group_alloc
  end type group

  type, public :: passwd
     character(kind=c_char,len=:), pointer :: buf  => NULL()
     integer(kind=c_int64_t) :: nbuf=-1, uid=-1
     character(kind=c_char,len=:), pointer :: name,passwd,gecos,dir,shell
   contains
     procedure getpwuid => passwd_getpwuid
     procedure free => passwd_free
     procedure alloc => passwd_alloc
  end type passwd

  integer(kind=c_int64_t) :: groupmax=-1, passwdmax = -1

  interface
     subroutine sia_c_passwdmax(n) bind(c)
       use iso_c_binding
       implicit none
       integer(kind=c_int64_t) :: n
     end subroutine sia_c_passwdmax
  end interface

  interface
     subroutine sia_c_groupmax(n) bind(c)
       use iso_c_binding
       implicit none
       integer(kind=c_int64_t) :: n
     end subroutine sia_c_groupmax
  end interface
  
  interface 
     subroutine sia_c_getgrgid(n,buf,gid,n1,n2,pmembers,nmembers,ierr) bind(c)
       use iso_c_binding
       implicit none
       integer(kind=c_int64_t),value :: n,gid
       integer(kind=c_int64_t) :: n1,n2,ierr,nmembers
       character(kind=c_char) :: buf
       type(c_ptr) :: pmembers
     end subroutine sia_c_getgrgid
  end interface
  
  interface 
     subroutine sia_c_getpwuid(n,buf,uid,idex,ierr) bind(c)
       use iso_c_binding
       implicit none
       integer(kind=c_int64_t),value :: uid,n
       integer(kind=c_int64_t) :: idex(10),ierr,nmembers
       character(kind=c_char) :: buf
       type(c_ptr) :: pmembers
     end subroutine sia_c_getpwuid
  end interface

  interface 
     subroutine sia_c_grmember(buf,pmembers,imember,n1,n2) bind(c)
       use iso_c_binding
       implicit none
       integer(kind=c_int64_t),value :: imember
       integer(kind=c_int64_t) :: n1,n2
       character(kind=c_char) :: buf
       type(c_ptr),value :: pmembers
     end subroutine sia_c_grmember
  end interface

contains

  subroutine group_alloc(g,size)
    integer(kind=c_int64_t), intent(in),optional :: size
    class(group),intent(inout) :: g
    integer(kind=c_int64_t) :: isize
    call g%free()
    if(present(size)) then
       isize=size
       allocate(character(len=isize) :: g%buf)
       g%nbuf=isize
    else
       if(groupmax<=0) then
          call sia_c_groupmax(groupmax)
       endif
       allocate(character(len=groupmax) :: g%buf)
       g%nbuf=groupmax
    endif
  end subroutine group_alloc

  subroutine group_free(g)
    class(group),intent(inout) :: g
    if(associated(g%members)) deallocate(g%members)
    if(associated(g%buf)) deallocate(g%buf)
    g%nbuf=-1
    g%nmembers=-1
    g%gid=-1
    nullify(g%buf,g%members,g%name)
  end subroutine group_free

  logical function group_getgrgid(g,gid)
    class(group),intent(inout) :: g
    integer(kind=c_int64_t), intent(in) :: gid
    integer(kind=c_int64_t) :: ierr,n1,n2,imember
    type(c_ptr) :: pmembers

    if(associated(g%members)) then
       deallocate(g%members)
    endif
    g%nmembers=0
    g%gid=gid
    nullify(g%name,g%members)

    call sia_c_getgrgid(g%nbuf,g%buf,gid,n1,n2,pmembers,g%nmembers,ierr)
    group_getgrgid = ierr==0
    if(.not.group_getgrgid) return

    g%name => g%buf(n1:n2)
    allocate(g%members(g%nmembers))
    do imember=1,g%nmembers
       call sia_c_grmember(g%buf,pmembers,imember,n1,n2)
       g%members(imember)%p => g%buf(n1:n2)
    enddo
  end function group_getgrgid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine passwd_alloc(g,size)
    integer(kind=c_int64_t), intent(in),optional :: size
    class(passwd),intent(inout) :: g
    integer(kind=c_int64_t) :: isize
    call g%free()
    if(present(size)) then
       isize=size
       allocate(character(len=isize) :: g%buf)
       g%nbuf=isize
    else
       if(passwdmax<=0) then
          call sia_c_passwdmax(passwdmax)
       endif
       allocate(character(len=passwdmax) :: g%buf)
       g%nbuf=passwdmax
    endif
  end subroutine passwd_alloc

  subroutine passwd_free(g)
    class(passwd),intent(inout) :: g
    if(associated(g%buf)) deallocate(g%buf)
    g%nbuf=-1
    g%uid=-1
    nullify(g%name,g%passwd,g%gecos,g%dir,g%shell,g%buf)
  end subroutine passwd_free

  logical function passwd_getpwuid(g,uid)
    class(passwd),intent(inout) :: g
    integer(kind=c_int64_t), intent(in) :: uid
    integer(kind=c_int64_t) :: ierr,idex(10)
    type(c_ptr) :: pmembers

    g%uid=uid
    nullify(g%name,g%passwd,g%gecos,g%dir,g%shell)

    call sia_c_getpwuid(g%nbuf,g%buf,uid,idex,ierr)
    passwd_getpwuid = ierr==0
    if(.not.passwd_getpwuid) return

    g%name   => g%buf(idex(1):idex(2))
    g%passwd => g%buf(idex(3):idex(4))
    g%gecos  => g%buf(idex(5):idex(6))
    g%dir    => g%buf(idex(7):idex(8))
    g%shell  => g%buf(idex(9):idex(10))
  end function passwd_getpwuid

end module sia_usrgrp
