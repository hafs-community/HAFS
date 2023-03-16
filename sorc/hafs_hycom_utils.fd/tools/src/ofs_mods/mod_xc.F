      module mod_xc
      implicit none
c
c --- HYCOM communication interface.
c --- A subset of the serial interface for setup only.
c
c --- tital array dimensions
      integer, public, save :: idm,jdm
c
c --- halo size always zero for setup
      integer    nbdy
      parameter (nbdy=0)
c
c --- line printer unit (stdout)
      integer        lp
      common/linepr/ lp
      save  /linepr/
c
c --- tile number (counting from 1)
      integer, public, save :: mnproc
c
c --- xcsync stdout flushing options
      logical, public, parameter :: flush_lp=.true.,
     &                              no_flush=.false.
c
c --- private timer variables, see xctmri
      character*6, private, dimension(97), save :: cc
      integer,     private, dimension(97), save :: nc
      real*8,      private, dimension(97), save :: tc,t0
c
c --- actual module subroutines
      contains

      subroutine xcspmd
      implicit none
c
c**********
c*
c  1) initialize data structures that identify the domain and tiles.
c
c  2) data structures:
c      idm     -     1st total array dimension
c      jdm     -     2nd total array dimension
c      mnproc  -     1-D node index
c
c  3) Total array dimensions from regional.grid.b
c*
c**********
c
      character cvarin*6
c
c     shared memory version, mnproc=1.
c
      mnproc = 1
      lp     = 6
c
c     total array dimensions from regional.grid.b
c
      open(unit=11,file='regional.grid.b',form='formatted',
     &     status='old',action='read')
c
      read( 11,*) idm,cvarin
      if (cvarin.ne.'idm   ') then
        write(lp,*)
        write(lp,*) 'error in xcspmd - regional.grid.b input ',cvarin,
     &                        ' but should be idm   '
        write(lp,*)
        stop
      endif
      read( 11,*) jdm,cvarin
      if (cvarin.ne.'jdm   ') then
        write(lp,*)
        write(lp,*) 'error in xcspmd - regional.grid.b input ',cvarin,
     &                        ' but should be jdm   '
        write(lp,*)
        call flush(lp)
        stop
      endif
c
      write(lp,'(/ a,2i5 /)') 'xcspmd: idm,jdm =',idm,jdm
c
      close(unit=11)
c
c     initialize timers.
c
      call xctmri
      return
      end subroutine xcspmd

      subroutine xcstop(cerror)
      implicit none
c
      character*(*), intent(in) :: cerror
c
c**********
c*
c  1) stop all processes.
c
c  2) all processes must call this routine.
c     use 'xchalt' for emergency stops.
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    cerror          char*(*)       input     error message
c*
c**********
c
c     print active timers.
c
      call xctmrp
c
c     shared memory version, just stop.
c
      if     (cerror.ne.' ') then
        write(lp,*) '**************************************************'
        write(lp,*) cerror
        write(lp,*) '**************************************************'
        call flush(lp)
      endif
      stop '(xcstop)'
      end subroutine xcstop

      subroutine xcsync(lflush)
      implicit none
c
      logical, intent(in) :: lflush
c
c**********
c*
c  1) barrier, no processor exits until all arrive (and flush stdout).
c
c  2) some MPI implementations only flush stdout as a collective
c     operation, and hence the lflush=.true. option to flush stdout.
c
c  3) Only one processor, so the barrier is a no-op in this case.
c*
c**********
c
      if     (lflush) then
        call flush(lp)
      endif
      return
      end subroutine xcsync

      subroutine xctmri
      implicit none
c
c
c**********
c*
c  1) initialize timers.
c
c  2) timers  1:32 are for message passing routines,
c     timers 33:80 are for general hycom routines,
c     timers 81:96 are for user selected routines.
c     timer     97 is the total time.
c
c  3) call xctmri    to initialize timers (called in xcspmd),
c     call xctmr0(n) to start timer n,
c     call xctmr1(n) to stop  timer n and add event to timer sum,
c     call xctnrn(n,cname) to register a name for timer n,
c     call xctmrp to printout timer statistics (called by xcstop).
c*
c**********
c
      integer i
c
      real*8     zero8
      parameter (zero8=0.0)
c
      do 110 i= 1,97
        cc(i) = '      '
        nc(i) = 0
        tc(i) = zero8
  110 continue
c
      call xctmrn(97,'total ')
      call xctmr0(97)
      return
      end subroutine xctmri

      subroutine xctmr0(n)
      implicit none
c
      integer, intent(in) :: n
c
c**********
c*
c  1) start timer n.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    n               integer        input     timer number
c*
c**********
c
      real*8 wtime
c
#if defined(DEBUG_TIMER)
      if     (n.gt.24 .and. cc(n).ne.'      ') then
        write(lp,*) 'call ',cc(n)
        call flush(lp)
      endif
#endif
      t0(n) = wtime()
      return
      end subroutine xctmr0

      subroutine xctmr1(n)
      implicit none
c
      integer, intent(in) :: n
c
c**********
c*
c  1) add time since call to xctim0 to timer n.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    n               integer        input     timer number
c*
c**********
c
      real*8  wtime
c
      nc(n) = nc(n) + 1
      tc(n) = tc(n) + (wtime() - t0(n))
#if defined(DEBUG_TIMER)
      if     (n.gt.24 .and. cc(n).ne.'      ') then
        write(lp,*) 'exit ',cc(n)
        call flush(lp)
      endif
#endif
      return
      end subroutine xctmr1

      subroutine xctmrn(n,cname)
      implicit none
c
      character*6, intent(in) :: cname
      integer,     intent(in) :: n
c
c**********
c*
c  1) register name of timer n.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    n               integer        input     timer number
c    cname           char*(8)       input     timer name
c*
c**********
c
      cc(n) = cname
      return
      end subroutine xctmrn

      subroutine xctmrp
      implicit none
c
c**********
c*
c  1) print all active timers.
c
c  2) on exit all timers are reset to zero.
c*
c**********
c
      integer i
c
      real*8     zero8
      parameter (zero8=0.0)
c
c     get total time.
c
      call xctmr1(97)
c
c     print timers.
c
      write(lp,6000)
      do i= 1,97
        if     (nc(i).ne.0) then
          if     (cc(i).ne.'      ') then
            write(lp,6100) cc(i),nc(i),tc(i),tc(i)/nc(i)
          else
            write(lp,6150)    i, nc(i),tc(i),tc(i)/nc(i)
          endif
        endif
      enddo
      write(lp,6200)
      call flush(lp)
c
c     reset timers to zero.
c
      do i= 1,97
        nc(i) = 0
        tc(i) = zero8
      enddo
c
c     start a new total time measurement.
c
      call xctmr0(97)
      return
c
 6000 format(/ /
     +    4x,' timer statistics ' /
     +    4x,'------------------' /)
 6100 format(5x,a6,
     +   '   calls =',i9,
     +   '   time =',f11.5,
     +   '   time/call =',f14.8)
 6150 format(5x,'   #',i2,
     +   '   calls =',i9,
     +   '   time =',f11.5,
     +   '   time/call =',f14.8)
 6200 format(/ /)
      end subroutine xctmrp

      end module mod_xc
