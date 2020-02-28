module sia_mpi_worklist
  implicit none
  private

  public :: mpi_worklist, mpi_worklist_init, mpi_worklist_step, &
            mpi_worklist_free

  integer, public, parameter :: STATUS_UNSTARTED=0, STATUS_RUNNING=10
  integer, public, parameter :: STATUS_COMPLETE=20, STATUS_FAILED=30

  type mpi_worklist
     integer :: irank=-1,csize=0,comm=-1 ! MPI Comm rank, size and ID
     integer :: next_work ! Next piece of work to give out (rank 0 only)

     ! The piece of work this rank is working on, and its status:
     integer :: mywork=-1, mystatus=STATUS_UNSTARTED

     ! Status of all pieces of work:
     integer, pointer, dimension(:) :: workstatus=>NULL()

     ! Communication workspace array:
     integer, pointer, dimension(:,:) :: commspace=>NULL()

     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !!! Public variables

     logical :: valid=.false.  ! Is this object filled with valid data?
     logical :: active=.false. ! Is this worklist running? 

     ! Diagnostic variables filled in by the comm_update() subroutine:
     integer :: n_idle, n_running, n_complete, n_failed, n_left

     ! Number of pieces of work, filled in by constructor:
     integer :: n_work
   contains
     procedure :: init => mpi_worklist_init
     procedure :: step => mpi_worklist_step
     procedure :: free => mpi_worklist_free

     procedure :: comm_update => mpi_worklist_comm_update
     procedure :: comm_info => mpi_worklist_comm_info

     procedure :: idle_action => mpi_worklist_noop
     procedure :: completion_action => mpi_worklist_noop
     procedure :: start_work => mpi_worklist_start_work
     procedure :: check_work => mpi_worklist_check_work

     procedure :: send_work => mpi_worklist_send_work
     procedure :: recv_work => mpi_worklist_recv_work

  end type mpi_worklist

contains

  subroutine mpi_worklist_comm_info(list,mywork,irank,csize,comm)
    class(mpi_worklist), target :: list
    integer,intent(out) :: mywork,irank,csize,comm
    mywork=list%mywork
    irank=list%irank
    csize=list%csize
    comm=list%comm
  end subroutine mpi_worklist_comm_info

  subroutine mpi_worklist_noop(list,irank,csize,comm)
    use mpi
    implicit none
    class(mpi_worklist), target :: list
    integer,intent(in) :: irank,csize,comm
    integer :: ierr
  end subroutine mpi_worklist_noop

  subroutine mpi_worklist_check_work(list,mywork,irank,csize,comm,status)
    class(mpi_worklist), target :: list
    integer, intent(in) :: mywork,irank,csize,comm
    integer,intent(inout) :: status
    status=STATUS_COMPLETE
  end subroutine mpi_worklist_check_work

  subroutine mpi_worklist_start_work(list,mywork,irank,csize,comm,status)
    class(mpi_worklist), target :: list
    integer, intent(in) :: mywork,irank,csize,comm
    integer,intent(inout) :: status
    status=STATUS_RUNNING
  end subroutine mpi_worklist_start_work

  subroutine mpi_worklist_free(list)
    class(mpi_worklist), target :: list
    if(associated(list%workstatus)) deallocate(list%workstatus)
    if(associated(list%commspace)) deallocate(list%commspace)
    list%irank=-1
    list%csize=0
    list%comm=-1
    list%mywork=-1
    list%mystatus=STATUS_UNSTARTED
    list%n_left=0
    list%n_idle=0
    list%n_failed=0
    list%n_complete=0
    list%n_running=0
    list%n_work=-1
    list%next_work=0
    nullify(list%workstatus,list%commspace)
  end subroutine mpi_worklist_free

  subroutine mpi_worklist_comm_update(list,stat)
    use mpi
    implicit none
    class(mpi_worklist), target :: list
    integer,intent(inout) :: stat

    integer :: myinfo(4),suminfo(4)

    stat=0

    myinfo=0
    select case(list%mystatus)
    case(STATUS_RUNNING)
       myinfo(2)=1
    case(STATUS_COMPLETE)
       myinfo(3)=1
    case(STATUS_FAILED)
       myinfo(4)=1
    case default
       myinfo(1)=1
    end select

    call MPI_Allreduce(myinfo,suminfo,4,MPI_INTEGER,MPI_SUM,list%comm,stat)
    if(stat/=0) return

    call MPI_Gather(myinfo,4,MPI_INTEGER,&
                    list%commspace,4,MPI_INTEGER,&
                    0,list%comm,stat)
    if(stat/=0) return

    list%n_idle=suminfo(1)
    list%n_running=suminfo(2)
    list%n_complete=list%n_complete + suminfo(3)
    list%n_failed=list%n_failed + suminfo(4)
    list%n_left=list%n_work-list%n_complete-list%n_failed-list%n_running

    if(list%mystatus/=STATUS_RUNNING) then
       list%mystatus=STATUS_UNSTARTED
    endif

30  format(I0,': comm_update work=',I0,' workers=',I0,' idle=',I0,&
         ' running=',I0,' complete=',I0,' failed=',I0,' left=',I0,&
         ' mywork=',I0,' mystatus=',I0)
    if(list%irank==0) then
       print 30,list%irank,list%n_work,list%csize,list%n_idle,&
            list%n_running,list%n_complete,list%n_failed,list%n_left,&
            list%mywork,list%mystatus
    endif
  end subroutine mpi_worklist_comm_update

  subroutine mpi_worklist_step(list,stat)
    use mpi
    class(mpi_worklist), target :: list
    integer,intent(inout) :: stat
    integer :: r

    stat=0

    if(.not.list%active) return ! Already finished.

    ! Check any piece of work I am working on.
    if(list%mystatus==STATUS_RUNNING) then
       call list%check_work(list%mywork,list%irank,list%csize,list%comm,list%mystatus)
    endif

    ! Update status of all work and processes.
    call list%comm_update(stat)
    if(stat/=0) return ! The comm_update failed.

    ! Are all tasks done running?
    if(list%n_left==0 .and. list%n_running==0) then
       list%active=.false.
       ! We are done.
98     format(I0,': Complete.  Left=',I0,' running=',I0,'.  Call completion_action.')
       !print 98,list%irank,list%n_left,list%n_running
       call list%completion_action(list%irank,list%csize,list%comm)
       return
    endif

    ! Are all ranks that can do work already running something?
    if(list%n_running==list%csize .or. list%n_left==0) then
       call list%idle_action(list%irank,list%csize,list%comm)
       return
    endif

    if(list%irank/=0 .and. list%mystatus==STATUS_RUNNING) then
       ! I do not need to send or receive work.
       call list%idle_action(list%irank,list%csize,list%comm)
       return
    endif

    ! Dole out work to tasks that need work to do.
    !   r = rank under analysis
    ! Rank 0 sends work.  All idle ranks receive work.
    do r=0,list%csize-1
       if(list%irank==0) then
          if(list%commspace(2,r)==0) then
             ! Send work to rank "r" which may or may not be myself.
             call list%send_work(r,stat)
             if(stat/=0) cycle ! send failed; go to next workier
          else
             ! Rank "r" is already working.
          endif
       elseif(list%irank==r) then
          if(list%mystatus/=STATUS_RUNNING) then
             ! Receive work remotely from rank 0.
             call list%recv_work(stat)
             if(stat/=0) cycle ! receive failed; go to next worker
          else
             ! I, rank "r," am already working.
          endif
       endif

       ! Call the start_work function to start whatever it is the rank
       ! needs to do.
       if(list%irank==r) then
          if(list%mystatus/=STATUS_RUNNING) then
             if(list%mywork>0) then
                call list%start_work(list%mywork,list%irank,list%csize,list%comm,list%mystatus)
             endif
          endif
       endif
    enddo

    call list%idle_action(list%irank,list%csize,list%comm)
  end subroutine mpi_worklist_step

  subroutine mpi_worklist_send_work(list,dest,stat)
    use mpi
    implicit none
    ! Called on rank 0 to send work to rank "dest" which may or may
    ! not be rank 0.
    class(mpi_worklist), target :: list
    integer,intent(in) :: dest
    integer,intent(inout) :: stat
    integer :: ierr, no_work

    if(list%next_work > list%n_work) then
       no_work=-1
       call MPI_Send(no_work,1,MPI_INTEGER,dest,0,list%comm,stat)
9      format(I0,': sent no work to ',I0,' stat ',I0)
       !print 2,list%irank,dest,stat
       return
    endif

    stat=0
    if(dest==0) then
       ! Special case: send work to myself.
1      format(I0,': sent work ',I0,' to self')
       !print 1,list%irank,list%next_work
       list%mywork=list%next_work
    else
       stat=0
       call MPI_Send(list%next_work,1,MPI_INTEGER,dest,0,list%comm,stat)
2      format(I0,': sent work ',I0,' to ',I0,' stat ',I0)
       !print 2,list%irank,list%next_work,dest,stat
       if(stat/=0) return
    endif
    list%next_work=list%next_work+1
  end subroutine mpi_worklist_send_work

  subroutine mpi_worklist_recv_work(list,stat)
    use mpi
    implicit none
    ! Called to receive work from rank 0.  Will never be called from
    ! rank 0.
    class(mpi_worklist), target :: list
    integer mpistatus(MPI_STATUS_SIZE)    
    integer,intent(inout) :: stat

    stat=0
    call MPI_Recv(list%mywork,1,MPI_INTEGER,0,0,list%comm,mpistatus,stat)
1   format(I0,': recv_work ',I0,' stat ',I0)
    !print 1,list%irank,list%mywork,stat
  end subroutine mpi_worklist_recv_work

  subroutine mpi_worklist_init(list,comm,nwork,stat)
    use mpi
    class(mpi_worklist), target :: list
    integer,intent(in) :: comm,nwork
    integer,intent(inout) :: stat

    integer :: ierr,istat,ostat

    call list%free()
    list%comm=comm
    list%n_work=nwork
    list%next_work=1

    ierr=0
    call MPI_Comm_rank(comm,list%irank,ierr)
    if(ierr/=0) goto 1000 ! cleanup: MPI failed

    ierr=0
    call MPI_Comm_size(comm,list%csize,ierr)
    if(ierr/=0) goto 1000 ! cleanup: MPI failed

    ierr=0
    if(list%irank==0) then
       allocate(list%workstatus(list%csize),stat=ierr)
       if(ierr/=0) then
          nullify(list%workstatus)
          istat=1
          goto 2000 ! cleanup: tell other ranks we could not allocate memory
       endif
       list%workstatus=STATUS_UNSTARTED
    endif

    ierr=0
    allocate(list%commspace(4,0:list%csize-1),stat=ierr)
    if(ierr/=0) then
       nullify(list%commspace)
       istat=1
       goto 2000 ! cleanup: tell other ranks we could not allocate memory
    endif

    list%commspace=0
    istat=0

2000 continue     ! GOTO here if MPI works but allocate failed
    
    ierr=0
    call MPI_Allreduce(istat,ostat,1,MPI_INTEGER,MPI_MAX,comm,ierr)
    if(ierr/=0) then
       stat=ierr
    else
       stat=ostat
    endif
    
    if(stat==0) then
       list%valid=.true.
       list%active=.true.

       list%n_left=list%n_work
       list%n_idle=list%csize
       list%n_failed=0
       list%n_complete=0
       list%n_running=0
       return
    endif

1000 continue    ! Could not get MPI communicator information.

    call list%free()
    stat=1
  end subroutine mpi_worklist_init

end module sia_mpi_worklist
