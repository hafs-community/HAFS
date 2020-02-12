module sia_glob
  use iso_c_binding
  use sia_const
  implicit none
  private
  public :: glob,globmatch

  type globmatch
     character(kind=c_char,len=:), pointer :: m
  end type globmatch

  type glob
     type(c_ptr) :: glob_t                  =  c_null_ptr
     integer(kind=c_int64_t) :: nmatches    =  -1
     integer(kind=c_int64_t) :: ierr        =  -1
     type(globmatch), pointer :: match(:)   => NULL()
   contains
     procedure free => glob_free
     procedure glob => glob_glob
     procedure matches => glob_matches
     procedure free_matches => glob_free_matches
  end type glob

  interface
     subroutine sia_c_glob_free(g) bind(c)
       use iso_c_binding
       type(c_ptr), value :: g
     end subroutine sia_c_glob_free
  end interface

  interface
     subroutine sia_c_glob_glob(g,s,flags,ierr) bind(c)
       use iso_c_binding
       type(c_ptr) :: g
       character(kind=c_char) :: s
       integer(kind=c_int64_t), value :: flags
       integer(kind=c_int64_t) :: ierr
     end subroutine sia_c_glob_glob
  end interface

  interface
     subroutine sia_c_glob_nmatches(g,nmatches) bind(c)
       use iso_c_binding
       type(c_ptr), value :: g
       integer(kind=c_int64_t) :: nmatches
     end subroutine sia_c_glob_nmatches
  end interface

  interface
     subroutine sia_c_glob_match(g,i,cp,np) bind(c)
       use iso_c_binding
       type(c_ptr), value :: g
       type(c_ptr) :: cp
       integer(kind=c_int64_t),value :: i
       integer(kind=c_int64_t) :: np
     end subroutine sia_c_glob_match
  end interface

contains !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine glob_free(g)
    use iso_c_binding
    class(glob), intent(inout) :: g
    if(c_associated(g%glob_t)) then
       call sia_c_glob_free(g%glob_t)
    endif
    call g%free_matches()
  end subroutine glob_free

  subroutine glob_free_matches(g)
    use iso_c_binding
    class(glob), intent(inout) :: g
    if(associated(g%match)) then
       deallocate(g%match)
       nullify(g%match)
    endif
    g%nmatches=-1
  end subroutine glob_free_matches

  integer function glob_glob(g,s,flags)
    use iso_c_binding
    class(glob), intent(inout) :: g
    character(kind=c_char,len=*) :: s
    character(kind=c_char) :: s0(len(s)+1)
    integer(kind=c_int64_t) :: flags
    integer :: i
33  format('Glob for "',A,'" len ',I0,' flags ',I0)
    1print 33,s,len(s),flags
    ! do i=1,len(s)
    !    s0(i)=s(i:i)
    ! enddo
    ! s0(i)=c_null_char
    !write(0,*) 'call sia_c_glob_glob'
    call sia_c_glob_glob(g%glob_t,s//c_null_char,flags,g%ierr)
    !write(0,*) 'ierr=',g%ierr
    glob_glob=g%ierr
  end function glob_glob

  subroutine glob_matches(g)
    use iso_c_binding
    class(glob), intent(inout) :: g
    character(kind=c_char),pointer :: cs(:)
    type(c_ptr) :: cp
    integer(kind=c_int64_t) :: np,i,ci
    call g%free_matches()
    call sia_c_glob_nmatches(g%glob_t,g%nmatches)
    if(g%nmatches>0) then
       allocate(g%match(g%nmatches))
    endif
    do i=1,g%nmatches
       call sia_c_glob_match(g%glob_t,i,cp,np)
       if(.not.c_associated(cp) .or. np<1) then
          nullify(g%match(i)%m)
          cycle
       endif
       call c_f_pointer(cp,cs,(/np/))
       !print *,cs
       allocate(character(len=np,kind=c_char) :: g%match(i)%m)
       do ci=1,np
          g%match(i)%m(ci:ci) = cs(ci)
       enddo
    end do
  end subroutine glob_matches
end module sia_glob
