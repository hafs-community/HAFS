module module_test_worklist
  use sia_mpi_worklist
  type, extends(mpi_worklist) :: worklist_tester
     integer :: counter=0
   contains
     procedure :: init_work => test_initer
     procedure :: start_work => test_starter
     procedure :: check_work => test_checker
     procedure :: idle_action => test_idler
     procedure :: completion_action => test_completer
  end type worklist_tester

contains

  subroutine test_initer(list,comm,nwork,stat)
    implicit none
    class(worklist_tester), target :: list
    integer,intent(in) :: comm,nwork
    integer,intent(inout) :: stat
    integer :: mywork,irank,csize,comm2

    call mpi_worklist_init(list,comm,nwork,stat)
    if(.not. list%valid) return ! superclass constructor failed

    call list%comm_info(mywork,irank,csize,comm2)

1   format(I0,': test_starter rank ',I0,' of ',I0)
    print 1,irank,irank,csize
  end subroutine test_initer

  subroutine test_checker(list,mywork,irank,csize,comm,status)
    implicit none
    class(worklist_tester), target :: list
    integer,intent(in) :: mywork,irank,csize,comm
    integer,intent(inout) :: status
    list%counter=list%counter+1
    if(list%counter>2) then
       status=STATUS_COMPLETE
    else
       status=STATUS_RUNNING
    endif
1   format(I0,': test_checker rank ',I0,' of ',I0,' work=',I0,' counter=',I0,' status=',I0)
    print 1,irank,irank,csize,  mywork,list%counter,status
  end subroutine test_checker

  subroutine test_starter(list,mywork,irank,csize,comm,status)
    implicit none
    class(worklist_tester), target :: list
    integer,intent(in) :: mywork,irank,csize,comm
    integer,intent(inout) :: status
1   format(I0,': test_starter rank ',I0,' of ',I0,' work ',I0)
    print 1,irank,irank,csize,mywork
    status=STATUS_RUNNING
    list%counter=0
  end subroutine test_starter

  subroutine test_idler(list,irank,csize,comm)
    use sia_time, only: nanosleep
    implicit none
    class(worklist_tester), target :: list
    integer,intent(in) :: irank,csize,comm
1   format(I0,': test_idler rank ',I0,' of ',I0,' idling')
    print 1,irank,irank,csize
    call nanosleep(5.0_8)
  end subroutine test_idler

  subroutine test_completer(list,irank,csize,comm)
    use sia_time
    implicit none
    class(worklist_tester), target :: list
    integer,intent(in) :: irank,csize,comm
1   format(I0,': test_completer rank ',I0,' of ',I0,' complete')
    print 1,irank,irank,csize
  end subroutine test_completer
end module module_test_worklist

program test_worklist
  use module_test_worklist
  use mpi
  implicit none

  type(worklist_tester) :: workers
  integer :: ierr

  ierr=0
  call MPI_Init(ierr)
  call workers%init(MPI_COMM_WORLD,65,ierr)
  do while(ierr==0 .and. workers%n_left>0)
     call workers%step(ierr)
  enddo

  if(ierr/=0) then
10   format(I0,'/',I0,' failed')
     print 10,workers%n_failed,workers%n_work
     stop 1
  else
     print 20,workers%n_work
20   format('All ',I0,' completed successfully.')
  endif
end program test_worklist
