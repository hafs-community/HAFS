module sia_stream
  use iso_c_binding
  implicit none

  type stream
     integer(kind=c_int64_t) :: fno
     character(kind=c_char, len=:), pointer :: line=>NULL()
   contains
     procedure close => stream_close
     procedure open => stream_open
  end type stream
contains

  subroutine init_sia_stream

  subroutine open(self,filename,flags)
end module sia_stream
