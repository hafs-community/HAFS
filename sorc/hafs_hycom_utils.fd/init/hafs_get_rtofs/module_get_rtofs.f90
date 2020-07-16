module module_get_rtofs
  use sia_mpi_worklist
  use sia_subprocess
  use sia_time
  implicit none
  private

  public :: rtofs_getter, mpi_worklist

  integer, parameter :: MAX_INPUT_FMTS=3
  integer, parameter :: DONT_WORK = -999999, TIME_0_RESTART_FILE=-1000000

  type :: ptrwrap
     character(len=:), pointer :: s
  end type ptrwrap

  type, extends(mpi_worklist) :: rtofs_getter
     type(subprocess) :: proc
     real(kind=8) :: naptime=5.0_8

     integer :: debug
     character(len=:), pointer :: dtemplate,dtemp
     type(ptrwrap) :: archv_fmt(MAX_INPUT_FMTS)
     character(len=:), pointer :: ainfile,binfile,aoutfile,boutfile,&
                                  outfmt,infmt,filetype,tgzinfile
     type(tm) :: atime,atimeM1
     integer :: tries(MAX_INPUT_FMTS,2), starthr, endhr, stephr
     integer :: last_lead_time_today
   contains

     procedure :: read_namelist => rtofs_getter_read_namelist

     procedure :: start_work => rtofs_getter_start_work
     procedure :: check_work => rtofs_getter_check_work
     procedure :: idle_action => rtofs_getter_idle_action

     procedure :: make_tempdir => rtofs_getter_make_tempdir
     procedure :: next_try => rtofs_getter_next_try
     procedure :: start_tar => rtofs_getter_start_tar
     procedure :: copy_outputs => rtofs_getter_copy_outputs
     procedure :: clean_up_tempdir => rtofs_getter_clean_up_tempdir
     procedure :: make_infile_outfile => rtofs_getter_make_infile_outfile
  end type rtofs_getter

contains !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine rtofs_getter_read_namelist(list,filename,comm,ierr)
    use iso_c_binding
    use mpi
    use sia_time
    implicit none
    class(rtofs_getter), target :: list
    character*(*), intent(in) :: filename
    integer, intent(in) :: comm
    integer,intent(inout) :: ierr

    character(len=300) :: outfmt,infmt(MAX_INPUT_FMTS),dtemplate
    integer :: starthr,endhr,stephr,nwork,i,goodfmt,debug
    integer :: last_lead_time_today
    character(len=8) :: atime
    logical :: good
    namelist/get_rtofs/starthr,endhr,stephr,outfmt,infmt,atime,debug,last_lead_time_today

    call list%free()

    debug=1
    starthr=DONT_WORK
    endhr=DONT_WORK
    stephr=0
    last_lead_time_today=DONT_WORK
    outfmt='*'
    infmt='*'
    atime='*'
    dtemplate='get_rtofs.tmp.XXXXXX'

    open(unit=10,file=filename,form='FORMATTED',status='OLD')
    read(unit=10,nml=get_rtofs,end=100,err=200)
    close(unit=10)

9   format('namelist error: ',A)
    good=.true.

    if(trim(outfmt)=='*') then
       write(0,9) 'You must specify outfmt: the output filename format.'
       good=.false.
    else
       list%outfmt=>alloctrim(outfmt)
    endif

    goodfmt=0
    do i=1,MAX_INPUT_FMTS
       if(trim(infmt(i))/='*') then
          list%archv_fmt(i)%s=>alloctrim(infmt(i))
          goodfmt=goodfmt+1
       else
          nullify(list%archv_fmt(i)%s)
       endif
    enddo
    if(goodfmt<1) then
       write(0,9) 'You must specify at least one infmt.'
       good=.false.
    endif

    if(last_lead_time_today==DONT_WORK) then
       write(0,9) 'You must specify the last forecast hour available to the current day of RTOFS data (last_lead_time_today)'
       good=.false.
    else
       list%last_lead_time_today=last_lead_time_today
    endif

    if(trim(atime)=='*') then
       write(0,9) 'You must specify atime: the YYYYMMDD date'
       good=.false.
    else
       if(8/=list%atime%strptime('%Y%m%d',atime)) then
          write(0,9) 'Analysis time is invalid.  It must have the format YYYYMMDD'
          good=.false.
       else
          list%atimeM1=list%atime%add(int(-24*3600,c_Int64_t))
       endif
    endif

    if(starthr==DONT_WORK) then
       write(0,9) 'You must specify starthr: the first rtofs time in hours relative to the analysis time.'
       good=.false.
    endif

    if(index(dtemplate,'XXXXXX')<=0) then
       write(0,9) 'The dtemplate must contain "XXXXXX" (the letter X repeated six times).'
       good=.false.
    else
       list%dtemplate=>alloctrim(dtemplate)
       list%dtemp=>alloctrim(dtemplate)
    endif

    if(endhr==DONT_WORK) then
       write(0,9) 'You must specify starthr: the last rtofs time in hours relative to the analysis time.'
       good=.false.
    endif
    if(stephr<1) then
       write(0,9) 'You must specify stephr: the time step between rtofs times.'
       good=.false.
    endif

    if(.not.good) then
       write(0,9) 'NAMELIST ERRORS: correct your namelist.  See above for details.'
       ierr=1
       return
    endif

    ierr=0
    nullify(list%ainfile)
    nullify(list%tgzinfile)
    nullify(list%binfile)
    nullify(list%aoutfile)
    nullify(list%boutfile)
    list%tries=0
    list%endhr=endhr
    list%starthr=starthr
    list%stephr=stephr

    ! Number of archv times:
    nwork=(endhr-starthr)/stephr
    if(nwork*stephr+starthr == endhr) nwork=nwork+1

    nwork=nwork+1 ! Add one more piece of work for the restart file.

    call list%init(comm,nwork,ierr)
    return

100 continue
101 format(A,' "',A,'"')
    write(0,101) 'Namelist &get_rtofs not found in',filename
    ierr=1
    call list%free()
    return

200 continue
201 format(A,'"',A,'"')
    write(0,201) 'Error reading &get_rtofs from',filename
    ierr=1
    call list%free()
    return
  end subroutine rtofs_getter_read_namelist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  subroutine rtofs_getter_idle_action(list,irank,csize,comm)
    use sia_time, only: nanosleep
    implicit none
    class(rtofs_getter), target :: list
    integer,intent(in) :: irank,csize,comm
    call nanosleep(list%naptime)
  end subroutine rtofs_getter_idle_action

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine rtofs_getter_start_work(list,mywork,irank,csize,comm,status)
    implicit none
    class(rtofs_getter), target :: list
    integer,intent(in) :: mywork,irank,csize,comm
    integer,intent(inout) :: status
    status=STATUS_RUNNING
    list%tries=0
    call list%next_try(mywork,irank,csize,comm,status)
  end subroutine rtofs_getter_start_work

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine rtofs_getter_check_work(list,mywork,irank,csize,comm,status)
    implicit none
    class(rtofs_getter), target :: list
    integer,intent(in) :: mywork,irank,csize,comm
    integer,intent(inout) :: status

    if(status/=STATUS_RUNNING) return ! already complete/failed

    call list%next_try(mywork,irank,csize,comm,status)
  end subroutine rtofs_getter_check_work

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  logical function check_file_and_whine_about_it(irank,filename,minsize)
    use sia_stat
    type(statbuf) :: s
    integer, intent(in) :: minsize,irank
    character*(*), intent(in) :: filename

    check_file_and_whine_about_it=.false.
    if(s%stat(filename)) then
       if(.not.s%isreg()) then
          print 22,irank,filename,'is not a regular file'
          return
       endif
       if(s%size<minsize) then
          print 22,irank,filename,'is too small'
          return
       endif
    else
       print 22,irank,filename,'does not exist'
       return
    endif
    check_file_and_whine_about_it=.true.
22  format(I0,': ',A,': ',A)
  end function check_file_and_whine_about_it

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine rtofs_getter_next_try(list,mywork,irank,csize,comm,status)
    use iso_c_binding, only: c_int64_t
    use sia_fileop
    use sia_stat
    use mpi
    implicit none

    class(rtofs_getter), target :: list
    integer,intent(in) :: mywork,irank,csize,comm
    integer,intent(inout) :: status

    ! Locals
    type(statbuf) :: s
    logical :: have_a, have_b, have_tgz
    integer :: ifmt, itime, ierr
    integer(kind=c_int64_t) :: ierr8
    character(len=:), pointer :: fmt
    integer :: lead_time

    if(mywork==1) then
       ! Work #1 is the restart file.
       list%filetype=>alloctrim('restart')
    else
       ! Work #2 onward are archv files.
       list%filetype=>alloctrim('archv')
    endif

    dotime: do itime=1,2 ! Loop over current day (1) and previous day (2)
       if(mywork==1) then
          ! Restart file is always n00.
          lead_time=0
       else
          ! Archv files lead time depends on RTOFS analysis time.
          lead_time=6*(itime-1)+list%starthr+(mywork-2)*list%stephr
       endif
 
       ! Make sure we are allowed to access this lead time from the
       ! current day.
       if(itime==1) then
          if (lead_time>list%last_lead_time_today) then
20123        format(I0,': desired lead time too late: ',I0,'hr > ',I0,'hr (last hour today)')
             if(irank==0) print 20123,irank,lead_time,list%last_lead_time_today
             cycle dotime
          endif
       endif
       
       dofmt: do ifmt=1,MAX_INPUT_FMTS
          fmt=>list%archv_fmt(ifmt)%s
          if(fmt=='*' .and. list%debug>10) then
18           format(I0,': infmt',I0,' is not specified.  Skip.')
             print 18,irank,ifmt
          endif
          
          if(list%tries(ifmt,itime)>1) cycle ! already failed
          
          status=STATUS_RUNNING ! Reset failure for this check

          if_running: if(list%tries(ifmt,itime)==1) then
             if(list%debug>10) then
19              format(I0,': try infmt',I0,' for atime + ',I0)
                print 19,irank,ifmt,lead_time
             endif

             if(list%debug>5) then
                print '(I0,A)',irank,': We have started the process for this try.'
             endif

             if(list%proc%state/=IS_EXITED .and. list%proc%state/=IS_TERMINATED) then
                call list%proc%check()
             endif
             if(list%proc%state==IS_EXITED .or. list%proc%state==IS_TERMINATED) then
                if(list%debug>5) then
                   print '(I0,A)',irank,': Have not verified success or failure of a running process.'
                endif
                if(list%proc%state==IS_EXITED .and. list%proc%statearg==0) then
                   if(list%debug>0) then
                      print '(I0,A)',irank,': The untar succeeded.  Now try to get the files out.'
                      print 20,irank,ifmt,lead_time
                   endif
                   call list%copy_outputs(irank,ierr)
                   if(ierr/=0) then
                      print 21,irank,'tar file had no data',ifmt,lead_time
                      call list%clean_up_tempdir(irank,ierr)
                   else
                      call list%clean_up_tempdir(irank,ierr)
                      if(ierr/=0) then
                         print 21,irank,'could not clean up tempdir',ifmt,lead_time
                         print '(I0,A)',irank,': The tar file did not have what we were looking for.  Try the next one.'
                         list%tries(ifmt,itime)=2
                      else
                         print '(I0,A)',irank,': Success.  We are done with this time.'
                         status=STATUS_COMPLETE
                         return
                      endif
                   endif
                else
                   print '(I0,A)',irank,': The untar failed.'
                   print 21,irank,'untar failed',ifmt,lead_time
                   list%tries(ifmt,itime)=2
                   call list%clean_up_tempdir(irank,ierr)
                endif
             else
                ! Process is still running.
                if(list%debug>1) then
                   print '(I0,A)', irank,": process is still running"
                endif
                list%tries(ifmt,itime)=1
                status=STATUS_RUNNING
                return
             endif
          else
             print '(I0,A)',irank,': Process has not been started.'
             if(itime==1) then
                call list%make_infile_outfile(&
                     fmt,list%outfmt,irank,fmt,list%atime,lead_time,mywork)
             else
                call list%make_infile_outfile(&
                     fmt,list%outfmt,irank,fmt,list%atimeM1,lead_time,mywork)
             endif

             have_b=check_file_and_whine_about_it(irank,list%binfile,100)
             have_a=check_file_and_whine_about_it(irank,list%ainfile,100)

             if(.not.have_b) then ! Cannot go on without "b" file
                list%tries(ifmt,itime)=2
                cycle
             endif
             if(.not.have_a) then
                have_tgz=check_file_and_whine_about_it(irank,list%tgzinfile,100)
                if(.not.have_tgz) then
                   ! No "a" file option
                   list%tries(ifmt,itime)=2
                   cycle
                endif
             endif

             ierr8=copy(list%binfile,list%boutfile)
             if(ierr8/=0) then
                print 22,irank,list%binfile,'Could not copy b infile'
                list%tries(ifmt,itime)=2
                cycle
             endif

             use_a: if(have_a) then
                ierr8=rm(list%aoutfile)
                ierr8=symlink(list%ainfile,list%aoutfile)
                if(ierr8/=0) then
                   print 22,irank,list%aoutfile,'Could not make symlink.'
                   list%tries(ifmt,itime)=2
                   cycle
                endif
                status=STATUS_COMPLETE
                return
             endif use_a

             use_tgz: if(have_tgz) then
                ! We get here if there is no .a but there is a .a.tgz.
                ! We create a temporary directory and start the tar
                ! -xpzvf
                if(.not.list%make_tempdir(irank)) then
                   print 22,irank,list%aoutfile,'Could not make the temporary directory for this file.  Skipping.'
                   list%tries(ifmt,itime)=2
                   cycle
                endif

                call list%start_tar(irank,list%tgzinfile,ierr)
                if(ierr==0) then
                   list%tries(ifmt,itime)=1
                   status=STATUS_RUNNING
                   return
                else
                   ! Could not start tar, so we clean up the temporary
                   ! directory.
                   call list%clean_up_tempdir(irank,ierr)
                endif
             else
                ! We get here if there is no .a and no .a.tgz
                list%tries(ifmt,itime)=2
                cycle
             endif use_tgz
          endif if_running
       enddo dofmt
    enddo dotime

    ! We get here if we fail on all tries.
    status=STATUS_FAILED
    print 22,irank,'All tries have failed.  Giving up on this time.'
    
22  format(I0,': ',A,': ',A)
20  format(I0,': succeeded with infmt',I0,' for atime + ',I0)
21  format(I0,': failed (',A,') with infmt',I0,' for atime + ',I0)
  end subroutine rtofs_getter_next_try

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine rtofs_getter_clean_up_tempdir(list,irank,oerr)
    use iso_c_binding
    use mpi
    use sia_fileop
    use sia_dir
    use sia_stat
    use sia_treewalk
    implicit none
    class(rtofs_getter), target :: list
    integer,intent(in) :: irank
    integer, intent(inout) :: oerr
    integer :: iierr
    integer(kind=c_int64_t) :: ierr
    type(statbuf) :: s

    if(list%mystatus/=STATUS_RUNNING) then
33     format(I0,': Assertion failure.  Tried to call clean_up_tempdir twice.')
       write(0,33) irank
       call MPI_Abort(MPI_COMM_WORLD,16,iierr)
    endif

    oerr=0

    if(list%debug>0) then
4      format(I0,': cd ..')
       print 4,irank
    endif
    call chdir('..',ierr=ierr)
    if(ierr/=0) then
       ! There is no way to recover from this error.  We have to MPI_Abort.
13     format(I0,': ERROR: cannot cd ..')
       write(0,13) irank
       call MPI_Abort(MPI_COMM_WORLD,6,iierr)
    endif

    if(list%debug>0) then
3      format(I0,': deltree "',A,'"')
       print 3,irank,list%dtemp
    endif
    if(.not.deltree(list%dtemp)) then
       if(s%stat(list%dtemp)) then ! check for a false alarm
15        format(I0,': ',A,': cannot delete!  Will continue anyway.')
          write(0,15) irank,list%dtemp
          return
       endif
    endif

9   format(I0,': ',A,': deleted.')
    print 9,irank,list%dtemp

    if(list%debug>1) then
99     format(I0,': At bottom of clean_up_tempdir, oerr=',I0)
       print 99,irank,oerr
    endif
  end subroutine rtofs_getter_clean_up_tempdir

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine rtofs_getter_copy_outputs(list,irank,oerr)
    use iso_c_binding
    use sia_dir
    use sia_stat
    use sia_fileop
    implicit none
    integer, intent(in) :: irank
    integer, intent(inout) :: oerr
    class(rtofs_getter), target :: list

    ! Locals
    integer(kind=c_int64_t) :: ierr, biggest
    type(DIR) :: d
    type(statbuf) :: s
    character(len=16) :: strkind
    character(len=:), pointer :: direntbuf, filename, ddfile, bigfile
    
    oerr=0

    nullify(bigfile)
    biggest=-1

    if(list%debug>1) then
7      format(I0,': opendir(".")')
       print 7,irank
    endif

    if(.not.d%open('.')) then
71     format(I0,': ERROR: could not opendir(".")')
       print 71,irank
       oerr=1
       return
    endif

    ! Allocate space for one directory entry:
    allocate(character(len=d%dirent_size) :: direntbuf)

    ! Loop over all files in the directory:
    do while(d%read(filename,direntbuf)>0)

       if(list%debug>1) then
8         format(I0,': filename="',A,'"')
          print 8,irank,filename
       endif

       ! Do not use the special files . or ..
       if(filename=='.' .or. filename=='..') then
          if(list%debug>2) then
81           format(I0,': ',A,': skip special file')
             print 81,irank,filename
          endif
          cycle
       endif

       ! ddfile = Get the full path:
       ddfile=>pathjoin('.',filename)

       ! See if this is the biggest file so far
       if(.not.s%stat(ddfile)) then
          if(list%debug>0) then
             print 83,irank,ddfile
83           format(I0,': ',A,': cannot stat.  Skip file.')
          endif
          cycle
       elseif(.not.s%isreg()) then
          strkind=s%strkind()
          if(list%debug>2) then
             print 82,irank,filename,trim(strkind)
82           format(I0,': ',A,': skip file of type "',A,'"')
          endif
          cycle
       endif
       if(.not.associated(bigfile) .or. s%size>biggest) then
          if(associated(bigfile)) then
             deallocate(bigfile)
             nullify(bigfile)
          endif
          bigfile=>ddfile
          biggest=s%size
       else
          deallocate(ddfile)
       endif
    end do

    if(associated(bigfile)) then
       if(list%debug>0) then
6         format(I0,': mv "',A,'" "',A,'"')
          print 6,irank,bigfile,list%aoutfile
       endif
       call rename(bigfile,list%aoutfile,ierr)

       if(ierr/=0) then
9         format(I0,': ERROR: cannot mv "',A,'" "',A,'"')
          print 9,irank,bigfile,list%aoutfile
          oerr=1
       endif
    else
       print 91,irank,list%tgzinfile
       oerr=1
91     format(I0,': ',A,': no files found in archive.')
    endif

    deallocate(direntbuf)
    if(list%debug>2) then
5      format(I0,': closedir')
       print 5,irank
    endif
    call d%close()
  end subroutine rtofs_getter_copy_outputs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine rtofs_getter_start_tar(list,irank,infile,oerr)
    implicit none
    class(rtofs_getter), target :: list
    character(len=:), pointer :: infile
    integer, intent(inout) :: oerr
    integer, intent(in) :: irank

    character(len=len(infile)+13) :: cmd
    oerr=0

4   format('tar -xpvzf "',A,'"')
    write(cmd,4) infile

    if(list%debug>0) then
5      format(I0,': RUN [',A,']')
       print 5,irank,cmd
    endif

    call list%proc%start(cmd)
    call list%proc%check()
  end subroutine rtofs_getter_start_tar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  logical function rtofs_getter_make_tempdir(list,irank)
    use iso_c_binding
    use mpi
    use sia_dir
    use sia_fileop
    implicit none
    class(rtofs_getter), target :: list
    integer, intent(in) :: irank

    integer(kind=c_int64_t) :: ierr
    integer :: mpierr

    if(len(list%dtemplate)/=len(list%dtemp)) then
       write(0,*) 'Assertion failure: in make_tempdir, dtemplate and dtemp must be the same length.'
       call MPI_Abort(MPI_COMM_WORLD,102,mpierr)
    endif

    list%dtemp=list%dtemplate

    if(list%debug>0) then
2      format(I0,': mkdtemp "',A,'"')
       print 2,irank,list%dtemp
    endif
    call mkdtemp(list%dtemp,ierr)
    if(ierr/=0) then
21     format(I0,': mkdtemp failed.')
       print 21,irank
       goto 1000
    endif

    if(list%debug>0) then
3      format(I0,': cd "',A,'"')
       print 3,irank,list%dtemp
    endif

    call chdir(list%dtemp,ierr)
    if(ierr/=0) then
31     format(I0,': ',A,': cannot cd')
       print 31,irank,list%dtemp
       goto 1000
    endif

    rtofs_getter_make_tempdir=.true.
    return
1000 rtofs_getter_make_tempdir=.false.
  end function rtofs_getter_make_tempdir

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine make_filename(ext,fmt,outs,atime,ftime,lead_time,filetype)
    use iso_c_binding
    use sia_time
    use mpi
    implicit none
    type(tm) :: atime,ftime
    integer :: lead_time
    character(len=*),intent(in) :: ext
    character(len=:), pointer :: fmt,outs,filetype, var
    character(len=20) :: buf, fortfmt
    character(len=2) :: cfmt
    integer :: i,o,e,n,ierr
    integer(kind=c_int64_t) :: used,dt,intlen

    integer :: npt

    if(len(ext)<1) then
       write(0,*) 'no extension'
       call MPI_Abort(MPI_COMM_WORLD,47,ierr)
    endif

    o=1 ! Output index
    i=0 ! Input index
    parsefmt: do while(o<=len(outs) .and. i<len(fmt))
       i=i+1
       if(fmt(i:i)=='[' .and. i<len(fmt)) then
          ! Variable specification
          nullify(var)
          findvar: do e=i+1,len(fmt)
             if(fmt(e:e) == ']') then
                var=>fmt(i+1:e-1)
                exit findvar
             elseif(e==len(fmt)) then
                var=>fmt(i+1:len(fmt))
                exit findvar
             endif
          enddo findvar
          !print *,'var is ',var
          if(.not. associated(var)) then
             print *,'Empty variable specification: [] or [ at end of string'
             cycle parsefmt
          endif
          if(len(var)<2) then
             print *,'Invalid variable'
             cycle parsefmt
          endif
          !print *,'If we get here, the variable specification has the right syntax.'
          
          ! Ensure we skip over the variable in the next parsefmt
          ! iteration:
          i=i+len(var)-1+2

          if(var == 'type') then
             n=min(len(outs)-o+1,len(filetype))
             outs(o:o+n-1) = filetype(1:n)
             o=o+n
          elseif(var == 'ext') then
             n=min(len(outs)-o+1,len(ext))
             outs(o:o+n-1) = ext(1:n)
             o=o+n
          elseif(len(var)==2 .and. (var(1:1)=='A' .or. var(1:1)=='a')) then
             cfmt(1:1)='%'
             cfmt(2:2)=var(2:2)
             call atime%strftime(cfmt,buf)
             used=max(0,min(len(outs)-o+1,len_trim(buf)))
             if(used>0) then
                outs(o:o+used-1) = buf(1:used)
                o=o+used
             endif
          elseif(len(var)==2 .and. (var(1:1)=='F' .or. var(1:1)=='f')) then
             cfmt(1:1)='%'
             cfmt(2:2)=var(2:2)
             call ftime%strftime(cfmt,buf)
             used=max(0,min(len(outs)-o+1,len_trim(buf)))
             if(used>0) then
                outs(o:o+used-1) = buf(1:used)
                o=o+used
             endif
          elseif(len(var)>=2 .and. (var(1:1)=='L' .or. var(1:1)=='l')) then
             ! Forecast lead time (forecast minus analysis)
             if(len(var)==3) then
                if(var(2:2)=='f' .or. var(2:2)=='F') then
                   if(var(3:3)=='n' .or. var(3:3)=='N') then
                      ! Special case.  User wants the odd RTOFS n-06
                      ! n00 f72 format.
                      dt=lead_time ! type conversion
                      if(lead_time==0) then
                         buf='n00'
                      elseif(lead_time<0) then
                         call zeropad(dt,3,buf(2:len(buf)))
                         buf(1:1)='n'
                      else
                         call zeropad(dt,2,buf(2:len(buf)))
                         buf(1:1)='f'
                      endif
                      n=min(len(outs)-o+1,len_trim(buf))
                      outs(o:o+n-1)=buf(1:n)
                      o=o+n
                   endif
                endif
             endif
             select case(var(len(var):len(var)))
             case('H')
                dt=lead_time
             case('d')
                dt=lead_time/24
             case('M')
                dt=lead_time*60
             case('S')
                dt=lead_time*3600
             case default
                ! Invalid variable specification.
                cycle parsefmt
             end select

             intlen=1
             if(len(var)>2) then
                ! Length is specified
3000            format('(I',I0,')')
                write(fortfmt,3000) len(var)-2
                read(var(2:len(var)-1),fmt=trim(fortfmt)) intlen
                intlen=min(max(intlen,1),10)
             endif
             call zeropad(dt,intlen,buf)

             n=min(len(outs)-o+1,len_trim(buf))
             outs(o:o+n-1)=buf(1:n)
             o=o+n
          endif
       else
          ! Not a variable specification.
          outs(o:o)=fmt(i:i)
          o=o+1
       endif
    enddo parsefmt
    if(o<=len(outs)) then
       outs(o:len(outs))=' '
    endif
!<-hsk 2016: to replace archs in outs to archv 
!    npt=index(outs,'archs')
!    print*,'hsk: before replacement at lead time,npt ',outs,lead_time,npt
!    if (npti /= 0) then
!       outs(npt:npt+4)='archv'
!    endif
!    print*,'hsk: after replacement ',outs

  end subroutine make_filename

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine rtofs_getter_make_infile_outfile(&
       list,infmt,outfmt,irank,fmt,atime,lead_time,mywork)
    use iso_c_binding
    use sia_time
    implicit none
    class(rtofs_getter), target :: list
    character(len=*), intent(in) :: fmt
    character(len=:), pointer, intent(in) :: infmt,outfmt
    type(tm) :: atime
    integer, intent(in) :: mywork, lead_time,irank

    integer(kind=c_int64_t) :: ahr64
    character(len=1000) :: satime,sftime
    type(tm) :: ftime

    if(associated(list%ainfile)) then
       deallocate(list%ainfile)
       nullify(list%ainfile)
    endif
    if(associated(list%tgzinfile)) then
       deallocate(list%tgzinfile)
       nullify(list%tgzinfile)
    endif
    if(associated(list%binfile)) then
       deallocate(list%binfile)
       nullify(list%binfile)
    endif
    if(associated(list%aoutfile)) then
       deallocate(list%aoutfile)
       nullify(list%aoutfile)
    endif
    if(associated(list%boutfile)) then
       deallocate(list%boutfile)
       nullify(list%boutfile)
    endif

    allocate(character(len=len(infmt)*2+100) :: list%ainfile)
    allocate(character(len=len(infmt)*2+100) :: list%tgzinfile)
    allocate(character(len=len(infmt)*2+100) :: list%binfile)
    allocate(character(len=len(outfmt)*2+100) :: list%aoutfile)
    allocate(character(len=len(outfmt)*2+100) :: list%boutfile)

    ahr64=lead_time
!    ftime=atime%add(ahr64*3600)
! Biju Thomas
! On Orion, atime%add is not working correctly. If ahr6=0, ftime and atime are
! not same on Orion. Temporary fix for ahr64=0. 
    if (ahr64 .ne. 0) then
      ftime=atime%add(ahr64*3600)
    else
      ftime=atime
    endif

!<-hsk 2016: for HYCOM v11 
    if (list%filetype=='archv'.and.((mod(lead_time,6)==3).or.(mod(lead_time,6)==-3))) then
       list%filetype='archs'
    endif    
!-> hsk
    call make_filename(fmt=infmt,outs=list%tgzinfile,atime=atime,ftime=ftime,lead_time=lead_time,filetype=list%filetype,ext='a.tgz')
    call make_filename(fmt=infmt,outs=list%ainfile,atime=atime,ftime=ftime,lead_time=lead_time,filetype=list%filetype,ext='a')
    call make_filename('a',outfmt,list%aoutfile,atime,ftime,lead_time,list%filetype)

    call make_filename('b',infmt,list%binfile,atime,ftime,lead_time,list%filetype)
    call make_filename('b',outfmt,list%boutfile,atime,ftime,lead_time,list%filetype)

    call dealloctrim(list%ainfile)
    call dealloctrim(list%tgzinfile)
    call dealloctrim(list%aoutfile)
    call dealloctrim(list%binfile)
    call dealloctrim(list%boutfile)
    
    call atime%strftime('%Y-%m-%d %H:%M:%S',satime)
    call ftime%strftime('%Y-%m-%d %H:%M:%S',sftime)
10  format(I0,': atime ',A,' + ',I0,' = ftime ',A,/,'  untar "',A, &
         '" or link "',A,'" "',/,'    => "',A,'"',/,'  copy "',A, &
         '" => "',A,'"')
    if(list%debug>0) then
       print 10, irank,trim(satime),lead_time,trim(sftime),list%tgzinfile,list%ainfile,list%aoutfile,list%binfile,list%boutfile
    endif
  end subroutine rtofs_getter_make_infile_outfile

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function alloctrim(s)
    implicit none
    character(len=:), pointer :: alloctrim
    character*(*) :: s
    integer :: len
    len=len_trim(s)
    allocate(character(len=len) :: alloctrim)
    alloctrim=trim(s)
  end function alloctrim

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dealloctrim(s)
    character(len=:), pointer :: s,s2
    s2=>alloctrim(s)
    deallocate(s)
    s=>s2
  end subroutine dealloctrim

end module module_get_rtofs
