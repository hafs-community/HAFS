program dir_test
  use iso_c_binding
  use sia_dir
  implicit none
  type(DIR) :: d
  character*1000 :: filename
  character(len=:), allocatable :: direntbuf,dirname
  integer :: nargs,arglen,iarg
  logical :: success

  nargs=command_argument_count()
  success=.true.
  do iarg=1,nargs
     call get_command_argument(iarg,filename)
     success=success .and. list_dir(trim(filename))
  end do
  if( .not. success) stop 1

contains

  logical function list_dir(dirname)
    type(DIR) :: d
    character*(*), intent(in) :: dirname
    integer :: flen,ifile
    integer(kind=c_int64_t) :: dloc,threeloc
    character(len=:),pointer :: direntbuf,filename

    if(.not.d%open(dirname)) then
38     format(A,': cannot open directory')
       write(0,38) dirname
       list_dir=.false.
       return
    endif

    allocate(character(len=d%dirent_size,kind=c_char) :: direntbuf)

    ifile=0
    do 
       nullify(filename)
       flen=d%read(filename,direntbuf)
       ifile=ifile+1
       dloc=d%tell()
       if(ifile==3) then
          threeloc=dloc
       endif
79     format(A,': location = ',I0)
       print 79,dirname,dloc
       if(flen<0) then
39        format(A,': error scanning directory')
          write(0,38) dirname
          list_dir=.false.
          goto 1000 ! Clean up and exit
       elseif(flen==0) then
          ! End of directory reached.
          exit
       endif
       if(dirname(len(dirname):len(dirname))=='/') then
41        format('"',A,A,'"')
          print 41,dirname,filename
       else
40        format('"',A,'/',A,'"')
          print 40,dirname,filename
       endif
    end do

    if(ifile>=3) then
       call d%seek(threeloc)
       if(d%read(filename,direntbuf)>0) then
          print *,'Third file was: ',filename,' at ',d%tell()
       else
          print *,'Cannot read after seeking.'
          list_dir=.false.
          goto 1000
       endif
    else
       print *,'DO not have three files, so skipping seek test'
    endif

    call d%rewind()
    if(d%read(filename,direntbuf)>0) then
       print *,'First file was: ',filename,' at ',d%tell()
    else
       print *,'Cannot read after rewinding.'
       list_dir=.false.
       goto 1000
    endif

    list_dir=.true.

1000 continue ! Exceptions jump here for cleanup
    call d%close()
    deallocate(direntbuf)
  end function list_dir
end program dir_test
