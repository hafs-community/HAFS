module sia_dir
  use iso_c_binding
  implicit none
  private
  public :: DIR

  type DIR
     type(c_ptr) :: dptr=c_null_ptr
     integer(kind=c_int64_t) :: dirent_size=-1, name_offset=-1
     logical :: valid=.false.
   contains
     procedure :: open => opendir
     procedure :: close => closedir
     procedure :: tell => telldir
     procedure :: rewind => rewinddir
     !procedure :: scan => scandir
     procedure :: seek => seekdir
     procedure :: read => readdir
  end type DIR

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  interface
     function c_telldir(dp) bind(c)
       use iso_c_binding
       integer(kind=c_int64_t) :: c_telldir
       type(c_ptr),value :: dp
     end function c_telldir
  end interface

  interface
     subroutine c_rewinddir(dp) bind(c)
       use iso_c_binding
       type(c_ptr),value :: dp
     end subroutine c_rewinddir
  end interface

  interface
     subroutine c_seekdir(dp,offset) bind(c)
       use iso_c_binding
       integer(kind=c_int64_t),value :: offset
       type(c_ptr),value :: dp
     end subroutine c_seekdir
  end interface

  interface
     function c_opendir(fn,dirent_size,name_offset) bind(c)
       use iso_c_binding
       type(c_ptr) :: c_opendir
       character(kind=c_char) :: fn
       integer(kind=c_int64_t) :: dirent_size,name_offset
     end function c_opendir
  end interface

  interface
     function c_readdir(dp,direntbuf) bind(c)
       use iso_c_binding
       type(c_ptr),value :: dp
       character(kind=c_char) :: direntbuf
       integer(kind=c_int64_t) :: c_readdir
     end function c_readdir
  end interface

  interface
     function c_closedir(dp) bind(c)
       use iso_c_binding
       type(c_ptr),value :: dp
       integer(kind=c_int64_t) :: c_closedir
     end function c_closedir
  end interface

contains !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function readdir(d,ofilename,direntbuf)
    use iso_c_binding
    class(DIR), intent(inout) :: d
    character(len=:,kind=c_char),pointer :: direntbuf
    character(len=:,kind=c_char),pointer :: ofilename
    integer(kind=c_int64_t) :: readdir
17  format('In readdir dirent size is ',I0)
    !print 17,len(direntbuf)
    readdir=c_readdir(d%dptr,direntbuf)
    if(readdir>0) then
       ofilename=>direntbuf(d%name_offset:(readdir+d%name_offset-1))
    else
       nullify(ofilename)
    endif
  end function readdir

  logical function opendir(d,filename)
    use iso_c_binding
    class(DIR), intent(inout) :: d
    character*(*),intent(in) :: filename
    call d%close()
    d%dptr=c_opendir(filename//c_null_char,d%dirent_size,d%name_offset)
    d%valid = c_associated(d%dptr) .and. d%dirent_size>4 .and. d%name_offset>3
    opendir=d%valid
  end function opendir

  subroutine seekdir(d,offset)
    use iso_c_binding
    integer(kind=c_int64_t), intent(in) :: offset
    class(DIR), intent(inout) :: d
    call c_seekdir(d%dptr,offset)
  end subroutine seekdir

  subroutine rewinddir(d)
    use iso_c_binding
    class(DIR), intent(inout) :: d
    call c_rewinddir(d%dptr)
  end subroutine rewinddir

  function telldir(d)
    use iso_c_binding
    integer(kind=c_int64_t)  :: telldir
    class(DIR), intent(inout) :: d
    telldir=c_telldir(d%dptr)
  end function telldir

  subroutine closedir(d)
    use iso_c_binding
    class(DIR), intent(inout) :: d
    integer(kind=c_int64_t) :: c
    if(d%valid) then
       c=c_closedir(d%dptr)
    end if
    d%valid=.false.
    d%name_offset=-1
    d%dirent_size=-1
    d%dptr=c_null_ptr
  end subroutine closedir

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module sia_dir
