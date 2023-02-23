module sia_string
  use iso_c_binding
  implicit none
  type string
     ! Do not change these pointers or the code will break.

     ! You can, and should, access str and change the characters it
     ! points to.  Just don't change the pointer itself.

     character(kind=c_char,len=:) :: str=>NULL() ! The string
     character(kind=c_char,len=:) :: buf=>NULL() ! Storage buffer
   contains
     procedure initstr => string_initstr
     procedure initbuf => string_initbuf
     procedure free => string_free
     procedure length => string_length
     procedure trim => string_trim
  end type string

contains

  subroutine string_ltrim(self)
    class(string) :: self
    integer i,n
    n=len(self%str)
    do i=1,n
       if(self%str(i:i)/=' ') exit
    enddo
    self%str=>self%str(i:n)
  end subroutine string_ltrim

  subroutine string_rtrim(self)
    class(string) :: self
    integer i,n
    n=len(self%str)
    do i=n,1,-1
       if(self%str(i:i)/=' ') exit
    enddo
    self%str=>self%str(1:i)
  end subroutine string_rtrim

  subroutine string_lrtrim(self)
    class(string) :: self
    integer n,s,e
    n=len(self%str)
    do s=1,n
       if(self%str(s:s)/=' ') exit
    enddo
    do e=n,1,-1
       if(self%str(e:e)/=' ') exit
    enddo
    if(s>e) then
       self%str=>self%str(1:0)
    else
       self%str=>self%str(s:e)
    endif
  end subroutine string_lrtrim

  subroutine string_append(self,other)
    class(string) :: self
    
  end subroutine string_append

  subroutine string_initstr(self,other)
    class(string) :: self
    character(len=*) :: other
    integer(kind=c_int64_t) :: i
    allocate(character(kind=c_char,len=len(other)+1) :: self%buf)
    do i=1,len(other)
       self%buf(i:i)=other(i:i)
    enddo
    self%buf(i)=c_null_char
    self%str=>self%buf(i:len(other))
  end subroutine string_initstr

  subroutine string_initlen(self,length)
    class(string) :: self
    integer(kind=c_int64_t) :: length
    integer(kind=c_int64_t) :: i
    allocate(character(kind=c_char,len=length+1) :: self%buf)
    do i=1,length
       self%buf(i)=' '
    enddo
    self%buf(i)=c_null_char
    self%str=>self%buf(i:i+length-1)
  end subroutine string_initlen

  subroutine string_free(self)
    class(string) :: self
    if(associated(self%buf)) then
       deallocate(self%buf)
    endif
    nullify(self%buf)
    nullify(self%str)
  end subroutine string_free

  subroutine string_length(self,length,fill)
    class(string) :: self
    integer(kind=c_int64_t) :: length,i,j,len0
    character(kind=c_char,len=:) :: newbuf,oldbuf
    character :: fill
    len0=max(length,0)
    if(.not.associated(self%buf)) then
       call self%initlen(len0)
    elseif(len0>len(self%buf)-1) then
       !! Need to allocate a new, larger buffer
       allocate(character(kind=c_char,len=len0+1) :: newbuf)
       do i=1,len(self%buf)-1
          newbuf(i:i)=self%buf(i:i)
       enddo
       do j=1,len0+1
          newbuf(i:i)=c_null_char
       enddo
       oldbuf=>self%buf
       self%buf=>newbuf
       self%str=>newbuf(1:len0)
       deallocate(oldbuf)
    elseif(len0<len(self%str)) then
       self%buf(len0+1)=c_null_char
       self%str=>self%buf(1:len0)
    endif
  end subroutine string_length
end module sia_string
