module sia_cmdarg
  implicit none
  private

  type, public :: arg
     character(len=:), pointer :: s => NULL()
   contains
     procedure getargs => arg_getargs
     procedure free => arg_free
  end type arg
  type, public :: args
     type(arg), dimension(:), pointer :: a => NULL()
   contains
     procedure getargs => args_getargs
     procedure free => args_free
  end type args

  ! type, extends(args), public :: getopt
  !    private
  !    integer :: nleft=-1,iarg=-1,ichr=-1
  !  contains
  !    procedure getopts => getopt_getopts
  !    procedure free => getopt_free
  ! end type getopt

contains

  subroutine arg_free(a)
    class(arg) :: a
    if(associated(a%s)) then
       deallocate(a%s)
       nullify(a%s)
    endif
  end subroutine arg_free

  subroutine arg_getargs(a,iarg)
    class(arg) :: a
    integer, intent(in) :: iarg
    integer :: alen
    call a%free()
    call get_command_argument(iarg,length=alen)
    if(alen>=0) then
       allocate(character(len=alen) :: a%s)
       call get_command_argument(iarg,a%s)
    endif
  end subroutine arg_getargs

  subroutine args_free(as)
    class(args) :: as
    integer :: i,n
    if(associated(as%a)) then
       n=size(as%a)
       do i=1,n
          call as%a(i)%free()
       enddo
       deallocate(as%a)
       nullify(as%a)
    endif
  end subroutine args_free

  subroutine args_getargs(as)
    class(args) :: as
    integer :: i,n
    n=command_argument_count()
    call as%free()
    if(n<1) return ! No arguments to read.
    allocate(as%a(n))
    do i=1,n
       call as%a(i)%getargs(i)
    enddo
  end subroutine args_getargs

  ! subroutine getopt_free(go)
  !   class(getopt) :: go
  !   integer i,n
  !   nullify(go%acur)
  !   nullify(go%scur)
  !   if(associated(go%full)) then
  !      do i=1,size(go%full)
  !         call go%full(i)%free()
  !      enddo
  !      deallocate(go%full)
  !      nullify(go%full)
  !   endif
  !   call args_free(go)
  ! end subroutine getopt_free

  ! function getopt_getopt(go,fmt,opt,arg)
  !   class(getopt) :: go
  !   character*(*), intent(in) :: fmt
  !   character(len=:), pointer :: arg,opt

  !   if(.not.associated(go%a)) then
  !      nullify(arg)
  !      nullify(opt)
  !      return
  !   endif

  !   if(go%nleft<0 .or. go%iarg<1 .or. go%ichr<1) then
  !      ! First call to getopt.
  !      go%nleft=size(getopt%a)
  !      go%iarg=1
  !      go%ichr=1
  !   endif

  ! end function getopt_getopt
end module sia_cmdarg
