      MODULE CMP_COMM

      implicit none

! MPI variables
      include 'mpif.h'
 
      integer Coupler_id /0/   ! this is Coupler's id, used to address
                               ! Coupler. This is a default value,
                               ! possibly to be redefined later
C
C     Make Coupler's id 0 if it is active (i.e. communnicating with
C the Component.) Otherwise, make it a negative integer; in this case,
C the Component is standalone.
C

      integer ibuffer_size
      parameter (ibuffer_size=10)
      integer Coupler_rank,my_id,COMM_local,
     >component_master_rank_global,process_rank_global,
     >component_master_rank_local,process_rank_local,
     >component_nprocs,FlexLev,ibuffer(ibuffer_size),nprocs_global

      integer kind_REAL,kind_INTEGER,MPI_kind_REAL,
     >kind_alt_REAL,MPI_kind_alt_REAL
      parameter (kind_REAL=8,kind_INTEGER=4)
      parameter (kind_alt_REAL=12-kind_REAL)
C       kind_INTEGER must be number of bytes equal to number of bytes
C     implied by MPI_INTEGER MPI constant; all integers sent/received
C     are of this kind. No value other than 4 is anticipated as of now
C       kind_REAL is type of real data to communicate. The corresponding
C     MPI data type variable MPI_kind_REAL is assigned in CMP_INIT.
C       kind_alt_REAL is alternative type of real data to communicate. 
C     The corresponding MPI data type variable MPI_kind_alt_REAL is
C     assigned in CMP_INIT. (It is used in subroutines CMP_alt_SEND
C     and CMP_alt_RECV,)

      save

      END MODULE CMP_COMM
C
C***********************************************************************
C
      SUBROUTINE CMP_INIT(id,flex)
!                         in  in
C
C     This subroutine must be called by every Component right upon
C     calling MPI_INIT. It assigns a value to the Component communicator
C     COMM_local (which is a global variable in module CMP), to be 
C     thereafter used by the Component in place of
C     MPI_COMM_WORLD wherever it is used by the Component's
C     standalone version. Besides, it stores the Component's id,
c     the process's ranks, and the "flexibility level" (flex) requested
C     by the Component in glob. variables. (The latter parameter affects
C     the mode of communications; for its description, see CMP_SEND and
C     CMP_RECV.) Finally, it starts handshaking with Coupler, receiving
C     the unique (global, i.e. in MPI_COMM_WORLD) Coupler process 
C     rank Coupler_rank from Coupler
                                        ! ibuffer may include additional
                                        ! info to be received
C
      USE CMP_COMM

      implicit none

      integer id,flex

      integer ierr,color,key,status(MPI_STATUS_SIZE),tag,dummy
      character*10 s
      logical izd
C

C        Determine if MPI is initialized, if not initialize
      call MPI_INITIALIZED(izd,ierr)
      if (.not.izd) call MPI_INIT(ierr)

C        Determine MPI send/receive types according to prescribed
C        types for arrays to be communicated
      if (kind_REAL.eq.8) then
        MPI_kind_REAL=MPI_REAL8
        MPI_kind_alt_REAL=MPI_REAL4
      else if (kind_REAL.eq.4) then
        MPI_kind_REAL=MPI_REAL4
        MPI_kind_alt_REAL=MPI_REAL8
      else
        write(s,'(i0)') kind_REAL
        call GLOB_ABORT(1,
     >  'CMP_INIT: illegal value of kind_REAL='//s,1)
      end if
      if (kind_INTEGER.ne.4) then
        write(s,'(i0)') kind_INTEGER
        call GLOB_ABORT(1,
     >  'CMP_INIT: illegal value of kind_INTEGER='//s,1)
      end if

C        Store the Component's id
C
      my_id=id

C        Store the Component's "flexibility level"
C
      FlexLev=flex

C        Assign a value to the Component communicator
C        COMM_local, to be thereafter used by the Component in place of
C        MPI_COMM_WORLD wherever it is used by the Component's
C        standalone version
C
      color=id
      key=1
c           print*,'CMP_INIT: to call MPI_COMM_SPLIT, color=',color
      call MPI_COMM_SPLIT(MPI_COMM_WORLD,color,key,COMM_local,ierr)
      call GLOB_ABORT(ierr,'CMP_INIT: error in MPI_COMM_SPLIT',1)

C        Store the process's global and local ranks
C
c           print*,'CMP_INIT: to call MPI_COMM_RANK for global rank'
      call MPI_COMM_RANK(MPI_COMM_WORLD,process_rank_global,ierr)
      call GLOB_ABORT(ierr,
     >'CMP_INIT: error in MPI_COMM_RANK(MPI_COMM_WORLD...)',1)
c           print*,'CMP_INIT: to call MPI_COMM_RANK for local rank'
      call MPI_COMM_RANK(COMM_local,process_rank_local,ierr)
      call GLOB_ABORT(ierr,
     >'CMP_INIT: error in MPI_COMM_RANK(COMM_local...)',1)

C        Store component_nprocs - component's number of processes;
C        calculate global number number of processes;
C        determine whether it is standalone mode and if it is, make
C        Coupler's id negative and return
C
      call MPI_COMM_SIZE(COMM_local,component_nprocs,ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs_global,ierr)
      if (component_nprocs.eq.nprocs_global) then
        if(process_rank_local.eq.0) print*,'CMP_INIT: standalone mode'
        Coupler_id=-1
        RETURN
      end if

C        Start handshaking with Coupler (all processes):
C        receive the unique (global, i.e. in MPI_COMM_WORLD) Coupler 
C        process rank Coupler_rank from Coupler
C
      tag=Coupler_id+23456
c           print*,'CMP_INIT: to call MPI_RECV'
      call MPI_RECV(ibuffer,ibuffer_size,MPI_INTEGER,MPI_ANY_SOURCE,tag,
     >MPI_COMM_WORLD,status,ierr)
      call GLOB_ABORT(ierr,'CMP_INIT: error in MPI_RECV',1)
      Coupler_rank=ibuffer(2)
      if (ibuffer(1).ne.Coupler_id) then
        print*,'CMP_INIT: stopped, rcvd ibuffer(1) value is not C id: ',
     >  ibuffer(1)
        CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)
      end if
      if (ibuffer(3).ne.ibuffer_size) then
        print*,'CMP_INIT: stopped, rcvd ibuffer(3) value ',ibuffer(3),
     >  ' is not ibuffer_size=',ibuffer_size
        CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)
      end if

C        Inform Coupler that this components exists and is active
C
      call MPI_GATHER(id,1,MPI_INTEGER,dummy,1,MPI_INTEGER,
     >Coupler_rank,MPI_COMM_WORLD,ierr)

C
c     print*,
c    >'CMP_INIT: ranks: process local, global, Coupler; Coupler_id: ',
c    >process_rank_local,process_rank_global,Coupler_rank,Coupler_id

      return
      END
C
C***********************************************************************
C
      SUBROUTINE CMP_INTRO(master_rank_local)
!                                in
C       This routine must be called by all Component's processes
C       which must all know the local rank of Component's master
C       process (master_rank_local)
C          Alternatively, SUBROUTINE CMP_INTRO_m can be called
C      from Component's master process only, and SUBROUTINE CMP_INTRO_s
C      from all other processes. In this case, the local rank of
C      Component's master process will be determined and broadcast
C      automatically

      USE CMP_COMM

      implicit none
 
      integer master_rank_local,ierr,ibuf(3),color,key,tag
C

c     print*,'CMP_INTRO: entered ',master_rank_local,process_rank_local
c    >,Coupler_rank

      component_master_rank_local=master_rank_local

      if (Coupler_id.lt.0) return    !   <- standalone mode

C        If this process is the Component's master process,
C        complete handshaking with Coupler:
C        "register", i.e. send Component master process global rank 
C        to Coupler. Also, send the requested "flexibility level".
C        (Sending Component's id (in ibuf(1)) is for double-check only.)
C
      if (process_rank_local.eq.master_rank_local) then
        component_master_rank_global=process_rank_global
        ibuf(1)=my_id  ! redundant, sent for control only
        ibuf(2)=process_rank_global
        ibuf(3)=FlexLev
        tag=my_id+54321
            print*,'CMP_INTRO: to call MPI_SEND ',process_rank_local,
     >      process_rank_global
        call MPI_SEND(ibuf,3,MPI_INTEGER,Coupler_rank,tag,
     >  MPI_COMM_WORLD,ierr)
        if (ierr.ne.0) then
          print*,'CMP_INTRO: error in MPI_SEND, process ',
     >    process_rank_global
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)
        end if
      end if
c           print*,'CMP_INTRO: returning ',process_rank_local,
c    >      process_rank_global,Coupler_rank
      return
      END
C
C***********************************************************************
C
      SUBROUTINE CMP_INTRO_m
!
C      This routine must be called by Component's master process (only),
C      if CMP_INTRO is not called (see comments in CMP_INTRO)

      USE CMP_COMM

      implicit none
 
      integer ierr,ibuf(3),color,key,tag,i
C

c     print*,'CMP_INTRO_m: entered, process_rank_local=',
c    >process_rank_local

      component_master_rank_local=process_rank_local
      component_master_rank_global=process_rank_global

      tag=abs(my_id)+12345
      do i=0,component_nprocs-1
        if (i.ne.component_master_rank_local) then
          ibuf(1)=component_master_rank_local
          ibuf(2)=component_master_rank_global
          call MPI_SEND(ibuf,2,MPI_INTEGER,i,tag,COMM_local,ierr)
          if (ierr.ne.0) then
            print*,'CMP_INTRO_m: error in 1st MPI_SEND, i=',i
            CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)
          end if
        end if
      end do

      if (Coupler_id.lt.0) return    !   <- standalone mode

C        Complete handshaking with Coupler:
C        "register", i.e. send Component master process global rank 
C        to Coupler. Also, send the requested "flexibility level".
C        (Sending Component's id (in ibuf(1)) is for double-check only.)
C
      tag=my_id+54321
      ibuf(1)=my_id  ! redundant, sent for control only
      ibuf(2)=process_rank_global
      ibuf(3)=FlexLev
c         print*,'CMP_INTRO_m: to call MPI_SEND ',process_rank_local,
c    >    process_rank_global
      call MPI_SEND(ibuf,3,MPI_INTEGER,Coupler_rank,tag,
     >MPI_COMM_WORLD,ierr)
      if (ierr.ne.0) then
        print*,'CMP_INTRO_m: error in MPI_SEND, process ',
     >  process_rank_global
        CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)
      end if
c         print*,'CMP_INTRO_m: returning ',process_rank_local,
c    >    process_rank_global
      return
      END
C
C***********************************************************************
C
      SUBROUTINE CMP_INTRO_s
!
C      This routine must be called by all Component's processes other
C      than master process,
C      if CMP_INTRO is not called (see comments in CMP_INTRO)

      USE CMP_COMM

      implicit none
 
      integer ierr,ibuf(3),color,key,tag,i,status(MPI_STATUS_SIZE)
C

c     print*,'CMP_INTRO_s: entered, process_rank_local=',
c    >process_rank_local

      tag=abs(my_id)+12345
      call MPI_RECV(ibuf,2,MPI_INTEGER,MPI_ANY_SOURCE,tag,
     >COMM_local,status,ierr)
      if (ierr.ne.0) then
        print*,'CMP_INTRO_s: error in MPI_RECV ',process_rank_local
        CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)
      end if
      component_master_rank_local=ibuf(1)
      component_master_rank_global=ibuf(2)
c WtF?      do i=0,component_nprocs-1
c WtF?        if (i.ne.component_master_rank_local) then
c WtF?          ibuf(1)=component_master_rank_local
c WtF?          ibuf(2)=component_master_rank_global
c WtF?          call MPI_SEND(ibuf,2,MPI_INTEGER,i,tag,COMM_local,ierr)
c WtF?        end if
c WtF?      end do

c         print*,'CMP_INTRO_s: returning ',process_rank_local,
c    >    process_rank_global,component_master_rank_local,
c    >    component_master_rank_global
      return
      END
C
C***********************************************************************
C
      SUBROUTINE CMP_SEND(F,N)
C
      USE CMP_COMM

      implicit none
 
      integer N,ierr,tag
      real(kind=kind_REAL) F(N)
C
      if (Coupler_id.lt.0) return    !   <- standalone mode

c           call CMP_DBG_CR(6,'CMP_SEND: entered')

      if (process_rank_local.ne.component_master_rank_local) then
        if (FlexLev.eq.0) then
C         With "flexibility level" FlexLev=0, only Component master 
C         process is supposed to call this subroutine.
          print '("*** CMP_SEND: process_rank_local=",i4,"  ***"/
     >    "*** and component_master_rank_local=",i4," differ:  ***"/
     >    "*** STOPPED ***")',
     >    process_rank_local,component_master_rank_local
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)
        else if (FlexLev.eq.1) then
C         With "flexibility level" FlexLev=1, any Component process is 
C         allowed to call this subroutine but only the Component
C         master process can actually send data (so the
C         others just make a dummy call), as the Coupler process only 
C         receives data from the Component master process.
          return
        else if (FlexLev.ne.2 .and. FlexLev.ne.3) then
          print '("*** CMP_SEND: illegal value of FlexLev",i9/
     >    "*** STOPPED")',FlexLev
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)
        end if
C         With "flexibility level" FlexLev=2 or FlexLev=3, any 
C         Component process is allowed to actually send data.
C         [In this case, the Coupler process (in CPL_RECV) receives 
C         from MPI_ANY_SOURCE rather than component_master_rank_global,
C         and it is only identification by  tag  which enables Coupler
C         to receive the data from the right source.]
C         But in any case only one Component process may actually be
C         engaged in a particular exchange of data with Coupler.
      end if

      tag=my_id

      call MPI_SEND(F,N,MPI_kind_REAL,Coupler_rank,tag,
     >MPI_COMM_WORLD,ierr)
      call GLOB_ABORT(ierr,'CMP_SEND: error in MPI_SEND',1)

c           call CMP_DBG_CR(6,'CMP_SEND: exiting')
      return
      END
C
C***********************************************************************
C
      SUBROUTINE CMP_alt_SEND(F,N)
C
      USE CMP_COMM

      implicit none
 
      integer N,ierr,tag
      real(kind=kind_alt_REAL) F(N)
C
      if (Coupler_id.lt.0) return    !   <- standalone mode

c           call CMP_DBG_CR(6,'CMP_alt_SEND: entered')

      if (process_rank_local.ne.component_master_rank_local) then
        if (FlexLev.eq.0) then
C         With "flexibility level" FlexLev=0, only Component master 
C         process is supposed to call this subroutine.
          print '("*** CMP_SEND: process_rank_local=",i4,"  ***"/
     >    "*** and component_master_rank_local=",i4," differ:  ***"/
     >    "*** STOPPED ***")',
     >    process_rank_local,component_master_rank_local
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)
        else if (FlexLev.eq.1) then
C         With "flexibility level" FlexLev=1, any Component process is 
C         allowed to call this subroutine but only the Component
C         master process can actually send data (so the
C         others just make a dummy call), as the Coupler process only 
C         receives data from the Component master process.
          return
        else if (FlexLev.ne.2 .and. FlexLev.ne.3) then
          print '("*** CMP_SEND: illegal value of FlexLev",i9/
     >    "*** STOPPED")',FlexLev
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)
        end if
C         With "flexibility level" FlexLev=2 or FlexLev=3, any 
C         Component process is allowed to actually send data.
C         [In this case, the Coupler process (in CPL_RECV) receives 
C         from MPI_ANY_SOURCE rather than component_master_rank_global,
C         and it is only identification by  tag  which enables Coupler
C         to receive the data from the right source.]
C         But in any case only one Component process may actually be
C         engaged in a particular exchange of data with Coupler.
      end if

      tag=my_id

      call MPI_SEND(F,N,MPI_kind_alt_REAL,Coupler_rank,tag,
     >MPI_COMM_WORLD,ierr)
      call GLOB_ABORT(ierr,'CMP_SEND: error in MPI_SEND',1)

c           call CMP_DBG_CR(6,'CMP_SEND: exiting')
      return
      END
C
C***********************************************************************
C
      SUBROUTINE CMP_gnr_SEND(F,N,MPI_DATATYPE)
C
      USE CMP_COMM

      implicit none
 
      integer N,MPI_DATATYPE
      integer F(1)

      integer ierr,tag
C

      if (Coupler_id.lt.0) return    !   <- standalone mode

c           call CMP_DBG_CR(6,'CMP_alt_SEND: entered')

      if (process_rank_local.ne.component_master_rank_local) then
        if (FlexLev.eq.0) then
C         With "flexibility level" FlexLev=0, only Component master 
C         process is supposed to call this subroutine.
          print '("*** CMP_SEND: process_rank_local=",i4,"  ***"/
     >    "*** and component_master_rank_local=",i4," differ:  ***"/
     >    "*** STOPPED ***")',
     >    process_rank_local,component_master_rank_local
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)
        else if (FlexLev.eq.1) then
C         With "flexibility level" FlexLev=1, any Component process is 
C         allowed to call this subroutine but only the Component
C         master process can actually send data (so the
C         others just make a dummy call), as the Coupler process only 
C         receives data from the Component master process.
          return
        else if (FlexLev.ne.2 .and. FlexLev.ne.3) then
          print '("*** CMP_SEND: illegal value of FlexLev",i9/
     >    "*** STOPPED")',FlexLev
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)
        end if
C         With "flexibility level" FlexLev=2 or FlexLev=3, any 
C         Component process is allowed to actually send data.
C         [In this case, the Coupler process (in CPL_RECV) receives 
C         from MPI_ANY_SOURCE rather than component_master_rank_global,
C         and it is only identification by  tag  which enables Coupler
C         to receive the data from the right source.]
C         But in any case only one Component process may actually be
C         engaged in a particular exchange of data with Coupler.
      end if

      tag=my_id

      call MPI_SEND(F,N,MPI_DATATYPE,Coupler_rank,tag,
     >MPI_COMM_WORLD,ierr)
      call GLOB_ABORT(ierr,'CMP_SEND: error in MPI_SEND',1)

c           call CMP_DBG_CR(6,'CMP_SEND: exiting')
      return
      END
C
C***********************************************************************
C
      SUBROUTINE CMP_INTEGER_SEND(F,N)
C
      USE CMP_COMM

      implicit none
 
      integer N,ierr,tag
      integer F(N)
C
      if (Coupler_id.lt.0) return    !   <- standalone mode

c           print*,'CMP_INTEGER_SEND: entered with N=',N,' F=',F,
c    >      '; my_id=',my_id,'Coupler_rank=',Coupler_rank

      if (process_rank_local.ne.component_master_rank_local) then
        if (FlexLev.eq.0) then
C         With "flexibility level" FlexLev=0, only Component master 
C         process is supposed to call this subroutine.
          print '("*** CMP_SEND: process_rank_local=",i4,"  ***"/
     >    "*** and component_master_rank_local=",i4," differ:  ***"/
     >    "*** STOPPED ***")',
     >    process_rank_local,component_master_rank_local
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)
        else if (FlexLev.eq.1) then
C         With "flexibility level" FlexLev=1, any Component process is 
C         allowed to call this subroutine but only the Component
C         master process can actually send data (so the
C         others just make a dummy call), as the Coupler process only 
C         receives data from the Component master process.
          return
        else if (FlexLev.ne.2 .and. FlexLev.ne.3) then
          print '("*** CMP_SEND: illegal value of FlexLev",i9/
     >    "*** STOPPED")',FlexLev
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)
        end if
C         With "flexibility level" FlexLev=2 or FlexLev=3, any 
C         Component process is allowed to actually send data.
C         [In this case, the Coupler process (in CPL_RECV) receives 
C         from MPI_ANY_SOURCE rather than component_master_rank_global,
C         and it is only identification by  tag  which enables Coupler
C         to receive the data from the right source.]
C         But in any case only one Component process may actually be
C         engaged in a particular exchange of data with Coupler.
      end if

      tag=my_id
            print*,'CMP_INTEGER_SEND: to call MPI_SEND; F=',
     >      F,' N=',N,' Coupler_rank=',Coupler_rank,' tag=',tag
      call MPI_SEND(F,N,MPI_INTEGER,Coupler_rank,tag,
     >MPI_COMM_WORLD,ierr)
      call GLOB_ABORT(ierr,'CMP_INTEGER_SEND: error in MPI_SEND',1)
            print*,'CMP_INTEGER_SEND: to return'

      return
      END
C
C***********************************************************************
C
      SUBROUTINE CMP_RECV(F,N)
C
      USE CMP_COMM

      implicit none
 
      integer N,ierr,tag,ibuf(3),status(MPI_STATUS_SIZE)
      real(kind=kind_REAL) F(N)
C
      if (Coupler_id.lt.0) return    !   <- standalone mode

c           call CMP_DBG_CR(6,'CMP_RECV: entered')

      if (process_rank_local.ne.component_master_rank_local) then

        if (FlexLev.eq.0) then

C         With "flexibility level" FlexLev=0, only Component master 
C         process is supposed to call this subroutine.

          print '("*** CMP_RECV: process_rank_local=",i4,"  ***"/
     >    "*** and component_master_rank_local=",i4," differ:  ***"/
     >    "*** STOPPED ***")',
     >    process_rank_local,component_master_rank_local
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)

        else if (FlexLev.eq.1 .or. FlexLev.eq.2) then

C         With "flexibility level" FlexLev=1 or FlexLev=2, any 
C         Component process is allowed to call this subroutine but 
C         only the Component master process is supposed to actually 
C         receive data (so the others just make a dummy call), as
C         the Coupler process only sends data to the Component master
C         process.

          return

        else if (FlexLev.eq.3) then

C         With "flexibility level" FlexLev=3, any Component process
C         may actually receive data.
C         [In this case, the Coupler process (in CPL_SEND) first
C         receives the Component process global rank 
C         (process_rank_global) from this subroutine, the source being
C         MPI_ANY_SOURCE, so it is only identification by  tag  which 
C         enables Coupler to receive process_rank_global from the right
C         source. Upon the receipt, the Coupler process (in CPL_SEND)
C         sends the data to this Component process, rather than to 
C         the Component master process as is the case with lower 
C         "flexibility levels".]
C         But in any case only one Component process may actually be
C         engaged in a particular exchange of data with Coupler.

          ibuf(1)=my_id
          ibuf(2)=process_rank_global
          tag=my_id
          call MPI_SEND(ibuf,2,MPI_INTEGER,Coupler_rank,tag,
     >    MPI_COMM_WORLD,ierr)
          call GLOB_ABORT(ierr,'CMP_RECV: error in MPI_SEND',1)

        else

          print '("*** CMP_RECV: illegal value of FlexLev",i9/
     >    "*** STOPPED")',FlexLev
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)

        end if

      end if

      tag=my_id
      call MPI_RECV(F,N,MPI_kind_REAL,Coupler_rank,tag,
     >MPI_COMM_WORLD,status,ierr)
      call GLOB_ABORT(ierr,'CMP_RECV: error in MPI_RECV',1)

c           call CMP_DBG_CR(6,'CMP_RECV: exiting')

      return
      END
C
C***********************************************************************
C
      SUBROUTINE CMP_alt_RECV(F,N)
C
      USE CMP_COMM

      implicit none
 
      integer N,ierr,tag,ibuf(3),status(MPI_STATUS_SIZE)
      real(kind=kind_alt_REAL) F(N)
C
      if (Coupler_id.lt.0) return    !   <- standalone mode

c           call CMP_DBG_CR(6,'CMP_alt_RECV: entered')

      if (process_rank_local.ne.component_master_rank_local) then

        if (FlexLev.eq.0) then

C         With "flexibility level" FlexLev=0, only Component master 
C         process is supposed to call this subroutine.

          print '("*** CMP_alt_RECV: process_rank_local=",i4,"  ***"/
     >    "*** and component_master_rank_local=",i4," differ:  ***"/
     >    "*** STOPPED ***")',
     >    process_rank_local,component_master_rank_local
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)

        else if (FlexLev.eq.1 .or. FlexLev.eq.2) then

C         With "flexibility level" FlexLev=1 or FlexLev=2, any 
C         Component process is allowed to call this subroutine but 
C         only the Component master process is supposed to actually 
C         receive data (so the others just make a dummy call), as
C         the Coupler process only sends data to the Component master
C         process.

          return

        else if (FlexLev.eq.3) then

C         With "flexibility level" FlexLev=3, any Component process
C         may actually receive data.
C         [In this case, the Coupler process (in CPL_SEND) first
C         receives the Component process global rank 
C         (process_rank_global) from this subroutine, the source being
C         MPI_ANY_SOURCE, so it is only identification by  tag  which 
C         enables Coupler to receive process_rank_global from the right
C         source. Upon the receipt, the Coupler process (in CPL_SEND)
C         sends the data to this Component process, rather than to 
C         the Component master process as is the case with lower 
C         "flexibility levels".]
C         But in any case only one Component process may actually be
C         engaged in a particular exchange of data with Coupler.

          ibuf(1)=my_id
          ibuf(2)=process_rank_global
          tag=my_id
          call MPI_SEND(ibuf,2,MPI_INTEGER,Coupler_rank,tag,
     >    MPI_COMM_WORLD,ierr)
          call GLOB_ABORT(ierr,'CMP_alt_RECV: error in MPI_SEND',1)

        else

          print '("*** CMP_alt_RECV: illegal value of FlexLev",i9/
     >    "*** STOPPED")',FlexLev
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)

        end if

      end if

      tag=my_id
      call MPI_RECV(F,N,MPI_kind_alt_REAL,Coupler_rank,tag,
     >MPI_COMM_WORLD,status,ierr)
      call GLOB_ABORT(ierr,'CMP_alt_RECV: error in MPI_RECV',1)

c           call CMP_DBG_CR(6,'CMP_alt_RECV: exiting')

      return
      END
C
C***********************************************************************
C
      SUBROUTINE CMP_gnr_RECV(F,N,MPI_DATATYPE)
C
      USE CMP_COMM

      implicit none
 
      integer N,MPI_DATATYPE
      integer F(1)

      integer ierr,tag,ibuf(3),status(MPI_STATUS_SIZE)
C

      if (Coupler_id.lt.0) return    !   <- standalone mode

c           call CMP_DBG_CR(6,'CMP_gnr_RECV: entered')

      if (process_rank_local.ne.component_master_rank_local) then

        if (FlexLev.eq.0) then

C         With "flexibility level" FlexLev=0, only Component master 
C         process is supposed to call this subroutine.

          print '("*** CMP_gnr_RECV: process_rank_local=",i4,"  ***"/
     >    "*** and component_master_rank_local=",i4," differ:  ***"/
     >    "*** STOPPED ***")',
     >    process_rank_local,component_master_rank_local
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)

        else if (FlexLev.eq.1 .or. FlexLev.eq.2) then

C         With "flexibility level" FlexLev=1 or FlexLev=2, any 
C         Component process is allowed to call this subroutine but 
C         only the Component master process is supposed to actually 
C         receive data (so the others just make a dummy call), as
C         the Coupler process only sends data to the Component master
C         process.

          return

        else if (FlexLev.eq.3) then

C         With "flexibility level" FlexLev=3, any Component process
C         may actually receive data.
C         [In this case, the Coupler process (in CPL_SEND) first
C         receives the Component process global rank 
C         (process_rank_global) from this subroutine, the source being
C         MPI_ANY_SOURCE, so it is only identification by  tag  which 
C         enables Coupler to receive process_rank_global from the right
C         source. Upon the receipt, the Coupler process (in CPL_SEND)
C         sends the data to this Component process, rather than to 
C         the Component master process as is the case with lower 
C         "flexibility levels".]
C         But in any case only one Component process may actually be
C         engaged in a particular exchange of data with Coupler.

          ibuf(1)=my_id
          ibuf(2)=process_rank_global
          tag=my_id
          call MPI_SEND(ibuf,2,MPI_INTEGER,Coupler_rank,tag,
     >    MPI_COMM_WORLD,ierr)
          call GLOB_ABORT(ierr,'CMP_gnr_RECV: error in MPI_SEND',1)

        else

          print '("*** CMP_gnr_RECV: illegal value of FlexLev",i9/
     >    "*** STOPPED")',FlexLev
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)

        end if

      end if

      tag=my_id
      call MPI_RECV(F,N,MPI_DATATYPE,Coupler_rank,tag,
     >MPI_COMM_WORLD,status,ierr)
      call GLOB_ABORT(ierr,'CMP_gnr_RECV: error in MPI_RECV',1)

c           call CMP_DBG_CR(6,'CMP_gnr_RECV: exiting')

      return
      END
C
C***********************************************************************
C
      SUBROUTINE CMP_ANNOUNCE(nunit,s)
C
      USE CMP_COMM

      implicit none

      character*(*) s
 
      integer nunit,ierr
C

      if (process_rank_local.eq.component_master_rank_local) then
        write(nunit,*) trim(s)
      else if (FlexLev.eq.0) then

C         With "flexibility level" FlexLev=0, only Component master 
C         process is supposed to call this subroutine.

          print '("*** CMP_ANNOUNCE: process_rank_local=",i4,"  ***"/
     >    "*** and component_master_rank_local=",i4," differ:  ***"/
     >    "*** STOPPED ***")',
     >    process_rank_local,component_master_rank_local
          CALL MPI_ABORT(MPI_COMM_WORLD,2,ierr)

      end if

      return
      END
C
C***********************************************************************
C
      SUBROUTINE CMP_STDOUT(s)
C
c     USE CMP_COMM, ONLY: Coupler_id,process_rank_global
        ! <- These values may not have the right value by this moment,
        ! as this routine may be called before CMP_INIT  - 02/23/05

      implicit none

      character*(*) s
      integer ios
      character*4 mess
C

C -> For debugging:
      OPEN(12345,
     >file='/nfsuser/g01/wx20ds/C/cmp.stdout',
     >form='formatted',status='old',iostat=ios)
      if (ios.eq.0) then
        read(12345,*) mess
        if (mess.eq.'mess') then
c         print*,'CMP_STDOUT: unit 6 left alone, process ',
c    >    process_rank_global
        ! <- process_rank_global may be undefined by this moment, as
        !    this routine may be called before CMP_INIT  - 02/23/05
          RETURN
        end if
        CLOSE(12345)
      end if
C <- for debugging

c     if (Coupler_id.lt.0) RETURN    ! Nothing is to occur if there is
                                     ! no communication with Coupler,
                                     ! i.e. if Component is standalone
        ! <- Coupler_id may not have the right value by this moment,
        ! as this routine may be called before CMP_INIT  - 02/23/05

      if (len_trim(s).eq.0) RETURN

      close(6)
      
      open(6,file=trim(s),form='formatted',status='unknown')

      print*,'CMP_STDOUT: unit 6 closed, reopened as '//trim(s)

      return
      END
C
C***********************************************************************
C
      SUBROUTINE CMP_DBG_CR(nunit,s)
C
C       Debugging routine: mainly, prints Coupler_rank
C
      USE CMP_COMM

      implicit none

      character*(*) s
      integer nunit

      integer ncall/0/,ncallmax/5000/
      save
C

      if (s(5:6).eq.'m:') then
        if (process_rank_local .ne. component_master_rank_local) RETURN
      end if

      if (ncall.ge.ncallmax) RETURN
      ncall=ncall+1

      write(nunit,*)process_rank_global,ncall,Coupler_id,Coupler_rank,s

! The following assumes that Coupler_rank must be =0, comment out if
! this is not the case
      call GLOB_ABORT(Coupler_rank,
     >'CMP_DBG_CR: Coupler_rank.ne.0, aborting',1)

      return
      END
C
C***********************************************************************
C
      SUBROUTINE CMP_FLUSH(nunit)

      USE CMP_COMM

      implicit none

      integer nunit

      integer i,ierr,rc
C

      do i=0,component_nprocs-1
        call MPI_BARRIER(COMM_local,ierr)
        call GLOB_ABORT(ierr,'CMP_FLUSH: MPI_BARRIER failed, aborting',
     >  rc)
        if (i.eq.process_rank_local) call FLUSH(nunit)
      end do

      return
      END
C
C***********************************************************************
C
      subroutine CMP_FINALIZE(izd,ierr)

      USE CMP_COMM

      implicit none

      logical izd
      integer ierr

      integer ierr1,ierr2
C

      ierr=0
      ierr2=0
      call MPI_INITIALIZED(izd,ierr1)
      if (izd) call MPI_FINALIZE(ierr2)
      ierr=abs(ierr1)+abs(ierr2)

      return
      END
