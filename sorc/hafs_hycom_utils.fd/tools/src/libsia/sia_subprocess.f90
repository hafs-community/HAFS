module sia_subprocess
  use sia_const
  use iso_c_binding
  private

  public :: init_sia_subprocess, subprocess, subprocess_start, &
       subprocess_check, subprocess_kill, subprocess_free

  logical :: initialized=.false.

  integer, public, parameter :: IS_UNINITIALIZED = -1
  integer, public, parameter :: IS_RUNNING = 0
  integer, public, parameter :: IS_ERROR = 1
  integer, public, parameter :: IS_EXITED = 2
  integer, public, parameter :: IS_TERMINATED = 3
  integer, public, parameter :: IS_STOPPED = 4
  integer, public, parameter :: IS_CONTINUED = 5

  type subprocess
     integer(kind=c_int64_t) :: procid, errno, state, statearg
   contains
     procedure start => subprocess_start
     procedure check => subprocess_check
     procedure kill => subprocess_kill
     procedure free => subprocess_free
  end type subprocess

  interface
     subroutine sia_c_subprocess_init() bind(c)
     end subroutine sia_c_subprocess_init
  end interface

  interface
     subroutine sia_c_waitpid(pid,errno,state,statearg) bind(c)
       use iso_c_binding
       integer(kind=c_int64_t),value :: pid
       integer(kind=c_int64_t),intent(out) :: errno,state,statearg
     end subroutine sia_c_waitpid
  end interface

  interface
     subroutine sia_c_background(cmd,procid,errno) bind(c)
       use iso_c_binding
       character(kind=c_char),intent(in) :: cmd(*)
       integer(kind=c_int64_t),intent(out) :: procid,errno
     end subroutine sia_c_background
  end interface

  interface
     subroutine sia_c_kill(pid,sig,errno) bind(c)
       use iso_c_binding
       integer(kind=c_int64_t),value :: pid,sig
       integer(kind=c_int64_t),intent(out) :: errno
     end subroutine sia_c_kill
  end interface

contains !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine init_sia_subprocess()
    call sia_c_subprocess_init()
    initialized=.true.
  end subroutine init_sia_subprocess

  function subprocess_kill(self,isig)
    class(subprocess) :: self
    integer(kind=c_int64_t) :: isig, subprocess_kill
    if(.not. initialized) call init_sia_subprocess()
    if(self%procid<=0) then
       subprocess_kill=0
       return
    endif
    call sia_c_kill(self%procid,isig,subprocess_kill)
  end function subprocess_kill

  subroutine subprocess_check(self)
    class(subprocess) :: self
    if(self%state==IS_UNINITIALIZED .or. &
       self%state==IS_EXITED .or. &
       self%state==IS_TERMINATED) then
       ! The process is completed OR the process was never started.
       ! Either way, there is no more information to retrieve.
       return
    endif
    if(.not. initialized) call init_sia_subprocess()
    call sia_c_waitpid(self%procid,self%errno,self%state,self%statearg)
  end subroutine subprocess_check

  subroutine subprocess_start(self,cmd)
    use iso_c_binding
    class(subprocess) :: self
    character(len=*) :: cmd
    character(kind=c_char) :: cmd0(len(cmd)+1)
    integer :: i

    if(.not. initialized) call init_sia_subprocess()

    call self%free()

17  format('f: ',A,': start this')
    print 17,cmd

    do i=1,len(cmd)
       cmd0(i)=cmd(i:i)
    end do
    cmd0(i)=c_null_char
    self%procid=0
    call sia_c_background(cmd0,self%procid,self%errno)
    if(self%procid>0) then
       call self%check()
    endif
  end subroutine subprocess_start

  subroutine subprocess_free(self)
    class(subprocess) :: self
    integer(kind=c_int64_t) :: ignore
    if(.not. initialized) call init_sia_subprocess()
    if(self%state==IS_RUNNING) then
       ignore=self%kill(SIGTERM)
    endif
    self%procid=0
    self%errno=0
    self%state=0
    self%statearg=0
  end subroutine subprocess_free
end module sia_subprocess
