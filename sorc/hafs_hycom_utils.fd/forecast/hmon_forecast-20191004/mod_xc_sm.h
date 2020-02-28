c
c-----------------------------------------------------------------------
c
c     auxillary routines that involve off-processor communication.
c     shared memory version, contained in module mod_xc.
c
c     author:  Alan J. Wallcraft,  NRL.
c
c-----------------------------------------------------------------------
c
      subroutine xcaget(aa, a, mnflg)
      implicit none
c
      real,    intent(out)   :: aa(itdm,jtdm)
      real,    intent(in)    :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) convert an entire 2-D array from tiled to non-tiled layout.
c
c  3) mnflg selects which nodes must return the array
c        = 0; all nodes
c        = n; node number n (mnproc=n)
c
c  4) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    aa              real           output    non-tiled target array
c    a               real           input     tiled source array
c    mnflg           integer        input     node return flag
c*
c**********
c
      integer j
#if defined(TIMER)
c
c     call xctmr0( 1)
#endif
c
c     use xclget for now.
c
      do j= 1,jtdm
        call xclget(aa(1,j),itdm, a, 1,j,1,0, mnflg)
      enddo
#if defined(TIMER)
c
c     call xctmr1( 1)
#endif
      return
      end subroutine xcaget

      subroutine xcaput(aa, a, mnflg)
      implicit none
c
      real,    intent(inout) :: aa(itdm,jtdm)
      real,    intent(out)   :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) convert an entire 2-D array from non-tiled to tiled layout.
c
c  3) mnflg selects which nodes must contain the non-tiled array
c        = 0; all nodes
c        = n; node number n (mnproc=n)
c
c  4) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    aa              real           input     non-tiled source array
c    a               real           output    tiled target array
c    mnflg           integer        input     node source flag
c*
c**********
c
      integer j
#if defined(TIMER)
c
c     call xctmr0( 4)
#endif
c
c     use xclput for now.
c
      do j= 1,jtdm
        call xclput(aa(1,j),itdm, a, 1,j,1,0)
      enddo
#if defined(TIMER)
c
c     call xctmr1( 4)
#endif
      return
      end subroutine xcaput

      subroutine xcastr(a, mnflg)
      implicit none
c
      real,    intent(inout) :: a(:)
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) broadcast array a to all tiles.
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c    mnflg           integer        input     node originator flag
c*
c**********
c
#if defined(TIMER)
c
      call xctmr0( 9)
#endif
c
c     single node version - do nothing.
#if defined(TIMER)
c
      call xctmr1( 9)
#endif
      return
      end subroutine xcastr

      subroutine xceget(aelem, a, ia,ja, mnflg)
      implicit none
c
      real,    intent(out)   ::         aelem
      real,    intent(in)    :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
      integer, intent(in)    ::         ia,ja
      integer, intent(in), optional ::  mnflg
c
c**********
c*
c  1) find the value of a(ia,ja) on the non-tiled 2-D grid.
c
c  2) mnflg selects which nodes must return the line
c        = 0; all nodes (default)
c        = n; node number n (mnproc=n)
c     mnflg is ignored here (only have a single node).
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    aelem           real           output    required element
c    a               real           input     source array
c    ia              integer        input     1st index into a
c    ja              integer        input     2nd index into a
c    mnflg           integer        input     node return flag
c*
c**********
#if defined(TIMER)
c
      call xctmr0( 2)
#endif
c
c     single node version - trivial indexing.
c
      aelem = a(ia,ja)
#if defined(TIMER)
c
      call xctmr1( 2)
#endif
      return
      end subroutine xceget

      subroutine xceput(aelem, a, ia,ja, mnflg)
      implicit none
c
      integer, intent(in), optional ::  mnflg
      integer, intent(in)    ::         ia,ja
      real,    intent(in)    ::         aelem
      real,    intent(inout) :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
c
c**********
c*
c  1) fill a single element in the non-tiled 2-D grid.
c
c  2) mnflg selects which nodes hold the element on entry
c        = 0; all nodes (default)
c        = n; node number n (mnproc=n)
c     mnflg is ignored here (only have a single node).
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    aelem           real           input     element value
c    a               real           in/out    target array
c    ia              integer        input     1st index into a
c    ja              integer        input     2nd index into a
c    mnflg           integer        input     node source flag
c*
c**********
#if defined(TIMER)
c
      call xctmr0( 4)
#endif
c
c     single node version - trivial indexing.
c
      a(ia,ja) = aelem
#if defined(TIMER)
c
      call xctmr1( 4)
#endif
      return
      end subroutine xceput

      subroutine xchalt(cerror)
      implicit none
c
      character*(*), intent(in) :: cerror
c
c**********
c*
c  1) stop all processes.
c
c  2) only one processes need call this routine, i.e. it is for
c      emergency stops.  use 'xcstop' for ordinary stops called
c      by all processes.
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    cerror          char*(*)       input     error message
c*
c**********
c
c     shared memory version, just stop.
c
      if     (cerror.ne.' ') then
        write(lp,*) '**************************************************'
        write(lp,*) cerror
        write(lp,*) '**************************************************'
        call flush(lp)
      endif
      stop '(xchalt)'
      end subroutine xchalt

      subroutine xciget(alist,nl, a, ia,ja, mnflg)
      implicit none
c
      integer, intent(in)    ::         nl
      real,    intent(out)   ::         alist(nl)
      real,    intent(in)    :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
      integer, intent(in)    ::         ia(nl),ja(nl)
      integer, intent(in), optional ::  mnflg
c
c**********
c*
c  1) find the value of a(ia(:),ja(:)) on the non-tiled 2-D grid.
c
c  2) mnflg selects which nodes must return the list of elements
c        = 0; all nodes (default)
c        = n; node number n (mnproc=n)
c     mnflg is ignored here (only have a single node).
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    alist           real           output    required elements
c    nl              integer        input     dimension of alist
c    a               real           input     source array
c    ia              integer        input     1st indexes into a
c    ja              integer        input     2nd indexes into a
c    mnflg           integer        input     node return flag
c*
c**********
c
      integer i
c
#if defined(TIMER)
c
      call xctmr0( 2)
#endif
c
c     single node version - trivial indexing.
c
      do i= 1,nl
        alist(i) = a(ia(i),ja(i))
      enddo !i
#if defined(TIMER)
c
      call xctmr1( 2)
#endif
      return
      end subroutine xciget

      subroutine xciget_sm(alist,nl, a, ia,ja, mnflg)
      implicit none
c
      integer, intent(in)    ::         nl
      real,    intent(out)   ::         alist(nl)
      real,    intent(in)    :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
      integer, intent(in)    ::         ia(nl),ja(nl)
      integer, intent(in), optional ::  mnflg
c
c**********
c*
c  1) find the value of a(ia(:),ja(:)) on the non-tiled 2-D grid.
c     identical to xciget and never invoked (for compatibility only)
c
c  2) mnflg selects which nodes must return the list of elements
c        = 0; all nodes (default)
c        = n; node number n (mnproc=n)
c     mnflg is ignored here (only have a single node).
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    alist           real           output    required elements
c    nl              integer        input     dimension of alist
c    a               real           input     source array
c    ia              integer        input     1st indexes into a
c    ja              integer        input     2nd indexes into a
c    mnflg           integer        input     node return flag
c*
c**********
c
      integer i
c
#if defined(TIMER)
c
      call xctmr0( 2)
#endif
c
c     single node version - trivial indexing.
c
      do i= 1,nl
        alist(i) = a(ia(i),ja(i))
      enddo !i
#if defined(TIMER)
c
      call xctmr1( 2)
#endif
      return
      end subroutine xciget_sm

      subroutine xciput(alist,nl, a, ia,ja, mnflg)
      implicit none
c
      integer, intent(in)    ::         nl
      integer, intent(in), optional ::  mnflg
      integer, intent(in)    ::         ia(nl),ja(nl)
      real,    intent(inout) ::         alist(nl)
      real,    intent(inout) :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
c
c**********
c*
c  1) fill a list of elements in the non-tiled 2-D grid.
c
c  2) mnflg selects which nodes hold the elements on entry
c        = 0; all nodes (default)
c        = n; node number n (mnproc=n)
c     mnflg is ignored here (only have a single node).
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    alist           real           in/out    element values
c    nl              integer        input     dimension of alist
c    a               real           in/out    target array
c    ia              integer        input     1st indexes into a
c    ja              integer        input     2nd indexes into a
c    mnflg           integer        input     node source flag
c*
c**********
c
      integer i
#if defined(TIMER)
c
      call xctmr0( 4)
#endif
c
c     single node version - trivial indexing.
c
      do i= 1,nl
        a(ia(i),ja(i)) = alist(i)
      enddo !1
#if defined(TIMER)
c
      call xctmr1( 4)
#endif
      return
      end subroutine xciput

      subroutine xclget(aline,nl, a, i1,j1,iinc,jinc, mnflg)
      implicit none
c
      integer, intent(in)    ::  nl,i1,j1,iinc,jinc,mnflg
      real,    intent(out)   ::  aline(nl)
      real,    intent(in)    ::  a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
c
c**********
c*
c  1) extract a line of elements from the non-tiled 2-D grid.
c
c  2) aline(i) = a(i1+iinc*(i-1),j1+jinc*(i-1)), for i=1...nl.
c     iinc and jinc can each be -1, 0, or +1.
c
c     if jinc=0, j1 can be between jtdm+1 and jtdm+nbdy to return
c     values from the top halo.  This is for debugging the arctic
c     patch halo exchange only.
c
c  3) mnflg selects which nodes must return the line
c        =-n; node number n (mnproc=n), nl,i1,j1 only on node n
c        = 0; all nodes
c        = n; node number n (mnproc=n)
c     normally all integer arguments must be identical on all nodes,
c     but a negative mnflg indicates that only the target node is
c     providing the nl,i1,j1 values.  These are broadcast to all other
c     nodes, and returned in nl,i1,j1 by all of them.
c
c     mnflg is ignored here (only have a single node).
c
c  4) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    aline           real           output    required line of elements
c    nl              integer        in/out    dimension of aline
c    a               real           input     source array
c    i1              integer        in/out    1st index into a
c    j1              integer        in/out    2nd index into a
c    iinc            integer        input     1st index increment
c    jinc            integer        input     2nd index increment
c    mnflg           integer        input     node return flag
c
c    nl,i1,j1 are input only unless mnflg is negative.
c*
c**********
c
      integer i
#if defined(TIMER)
c
      call xctmr0( 3)
#endif
c
c     single node version - trivial indexing and no error checking.
c
      if     (jinc.eq.0) then
        do i= 1,nl
          aline(i) = a(i1+iinc*(i-1),j1)
        enddo
      elseif (iinc.eq.0) then
        do i= 1,nl
          aline(i) = a(i1,j1+jinc*(i-1))
        enddo
      else
        do i= 1,nl
          aline(i) = a(i1+iinc*(i-1),j1+jinc*(i-1))
        enddo
      endif
#if defined(TIMER)
c
      call xctmr1( 3)
#endif
      return
      end subroutine xclget

      subroutine xclput(aline,nl, a, i1,j1,iinc,jinc, mnflg)
      implicit none
c
      integer, intent(in), optional ::  mnflg
      integer, intent(in)    ::         nl,i1,j1,iinc,jinc
      real,    intent(inout) ::         aline(nl)
      real,    intent(inout) ::  a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
c
c**********
c*
c  1) fill a line of elements in the non-tiled 2-D grid.
c
c  2) aline(i) = a(i1+i1*(i-1),j1+j1*(i-1)), for i=1...nl.
c     one of iinc and jinc must be 0, and the other must be 1.
c
c  3) mnflg selects which nodes hold the line on entry
c        = 0; all nodes (default)
c        = n; node number n (mnproc=n)
c     mnflg is ignored here (only have a single node).
c
c  4) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    aline           real           in/out    line of element values
c    nl              integer        input     dimension of aline
c    a               real           in/out    target array
c    i1              integer        input     1st index into a
c    j1              integer        input     2nd index into a
c    iinc            integer        input     1st index increment
c    jinc            integer        input     2nd index increment
c    mnflg           integer        input     node source flag
c*
c**********
c
      integer i
#if defined(TIMER)
c
      call xctmr0( 4)
#endif
c
c     single node version - trivial indexing.
c
      if     (jinc.eq.0) then
        do i= 1,nl
          a(i1+i-1,j1) = aline(i)
        enddo
      elseif (iinc.eq.0) then
        do i= 1,nl
          a(i1,j1+i-1) = aline(i)
        enddo
      endif
#if defined(TIMER)
c
      call xctmr1( 4)
#endif
      return
      end subroutine xclput

      subroutine xcmaxr_0(a, mnflg)
      implicit none
c
      real,    intent(inout) :: a
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) replace scalar a with its element-wise maximum over all tiles.
c
c  2) mnflg selects which nodes must return the minimum
c        = 0; all nodes
c        = n; node number n (mnproc=n)
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target variable
c    mnflg           integer        input     node return flag
c*
c**********
c
#if defined(TIMER)
c
      call xctmr0(10)
#endif
c
c     single node version - do nothing.
#if defined(TIMER)
c
      call xctmr1(10)
#endif
      return
      end subroutine xcmaxr_0

      subroutine xcmaxr_1(a, mnflg)
      implicit none
c
      real,    intent(inout) :: a(:)
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) replace array a with its element-wise maximum over all tiles.
c
c  2) mnflg selects which nodes must return the minimum
c        = 0; all nodes
c        = n; node number n (mnproc=n)
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c    mnflg           integer        input     node return flag
c*
c**********
c
#if defined(TIMER)
c
      call xctmr0(10)
#endif
c
c     single node version - do nothing.
#if defined(TIMER)
c
      call xctmr1(10)
#endif
      return
      end subroutine xcmaxr_1

      subroutine xcmaxr_0o(a)
      implicit none
c
      real, intent(inout) :: a
c
c**********
c*
c  1) replace scalar a with its element-wise maximum over all tiles.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target variable
c*
c**********
c
#if defined(TIMER)
c
      call xctmr0(10)
#endif
c
c     single node version - do nothing.
#if defined(TIMER)
c
      call xctmr1(10)
#endif
      return
      end subroutine xcmaxr_0o

      subroutine xcmaxr_1o(a)
      implicit none
c
      real, intent(inout) :: a(:)
c
c**********
c*
c  1) replace array a with its element-wise maximum over all tiles.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c*
c**********
c
#if defined(TIMER)
c
      call xctmr0(10)
#endif
c
c     single node version - do nothing.
#if defined(TIMER)
c
      call xctmr1(10)
#endif
      return
      end subroutine xcmaxr_1o

      subroutine xcminr_0(a, mnflg)
      implicit none
c
      real,    intent(inout) :: a
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) replace scalar a with its element-wise minimum over all tiles.
c
c  2) mnflg selects which nodes must return the minimum
c        = 0; all nodes
c        = n; node number n (mnproc=n)
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target variable
c    mnflg           integer        input     node return flag
c*
c**********
c
#if defined(TIMER)
c
      call xctmr0(10)
#endif
c
c     single node version - do nothing.
#if defined(TIMER)
c
      call xctmr1(10)
#endif
      return
      end subroutine xcminr_0

      subroutine xcminr_1(a, mnflg)
      implicit none
c
      real,    intent(inout) :: a(:)
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) replace array a with its element-wise minimum over all tiles.
c
c  2) mnflg selects which nodes must return the minimum
c        = 0; all nodes
c        = n; node number n (mnproc=n)
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c    mnflg           integer        input     node return flag
c*
c**********
c
#if defined(TIMER)
c
      call xctmr0(10)
#endif
c
c     single node version - do nothing.
#if defined(TIMER)
c
      call xctmr1(10)
#endif
      return
      end subroutine xcminr_1

      subroutine xcminr_0o(a)
      implicit none
c
      real, intent(inout) :: a
c
c**********
c*
c  1) replace scalar a with its element-wise minimum over all tiles.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target variable
c*
c**********
c
#if defined(TIMER)
c
      call xctmr0(10)
#endif
c
c     single node version - do nothing.
#if defined(TIMER)
c
      call xctmr1(10)
#endif
      return
      end subroutine xcminr_0o

      subroutine xcminr_1o(a)
      implicit none
c
      real, intent(inout) :: a(:)
c
c**********
c*
c  1) replace array a with its element-wise minimum over all tiles.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c*
c**********
c
#if defined(TIMER)
c
      call xctmr0(10)
#endif
c
c     single node version - do nothing.
#if defined(TIMER)
c
      call xctmr1(10)
#endif
      return
      end subroutine xcminr_1o

      subroutine xcspmd
      implicit none
c
c**********
c*
c  1) initialize data structures that identify the tiles.
c
c  2) data structures:
c      ipr     - 1st 2-D node dimension
c      jpr     - 2nd 2-D node dimension
c      ijpr    -     1-D node dimension (ipr*jpr)
c      mproc   - 1st 2-D node index
c      nproc   - 2nd 2-D node index
c      mnproc  -     1-D node index
c      i0      -     1st dimension tile offset
c      ii      -     1st dimension tile extent
c      j0      -     2nd dimension tile offset
c      jj      -     2nd dimension tile extent
c      nreg    -     region type
c      vland   -     fill value for land (standard value 0.0)
c
c  3) ipr,jpr,ijpr are global (tile independent) values.
c     all other values depend on the processor number, 
c     but in this case there is only one processor.
c*
c**********
c
c     shared memory version, mproc=nproc=1.
c
      if     (iqr.ne.1 .or. jqr.ne.1 .or. ijqr.ne.1) then
        call xcstop('Error in xcspmd: must have iqr=jqr=ijqr=1')
               stop '(xcspmd)'
      endif
c
      lp = 6
c
      ipr    = 1
      jpr    = 1
      ijpr   = 1
      mnproc = 1
      mproc  = 1
      nproc  = 1
c
#if defined(RELO)
c --- region's horizontal dimensions are from blkdat.input.
c
      itdm = -1
      jtdm = -1
#endif
c
      i0  = 0
      ii  = itdm
      j0  = 0
      jj  = jtdm
c
      nreg   = -1  ! unknown region type
c
      vland  = 0.0
      vland4 = 0.0
c
c     initialize timers.
c
      call xctmri
#if defined(TIMER)
      call xctmrn( 1,'xcaget')
      call xctmrn( 2,'xceget')
      call xctmrn( 3,'xclget')
      call xctmrn( 4,'xcXput')
      call xctmrn( 5,'xcsum ')
      call xctmrn( 6,'xcrang')
      call xctmrn( 9,'xcastr')
      call xctmrn(10,'xcmaxr')
      call xctmrn(12,'xctilr')
#if defined(ARCTIC)
      call xctmrn(13,'xctila')
#endif
#endif
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
#if defined(STOP2003)
c --- Fortran 2003 STOP outputs many lines, replace with exit
      call exit(0)
#else
      stop '(xcstop)' 
#endif
      end subroutine xcstop

      subroutine xcsum(sum, a,mask)
      implicit none
c
      real*8,  intent(out)   :: sum
      real,    intent(inout) :: a(   1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
      integer, intent(in)    :: mask(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
c
c**********
c*
c  1) sum a 2-d array, where mask==1
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    sum             real*8         output    sum of a
c    a               real           input     source array
c    mask            integer        input     mask array
c
c  3) sum is bit for bit reproducable for the same halo size, nbdy.
c*
c**********
c
      real*8     zero8
      parameter (zero8=0.0)
c
      real*8  sum8,sum8p,sum8j(jdm)
      integer i,i1,j
#if defined(TIMER)
c
      call xctmr0( 5)
#endif
c
c     row sums in 2*nbdy+1 wide strips.
c
!$OMP PARALLEL DO PRIVATE(j,i1,i,sum8,sum8p)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jdm
        sum8 = zero8
        do i1=1,idm,2*nbdy+1
          sum8p = zero8
          do i= i1,min(i1+2*nbdy,idm)
            if     (mask(i,j).eq.1) then
              sum8p = sum8p + a(i,j)
            endif
          enddo
          sum8 = sum8 + sum8p
        enddo
        sum8j(j) = sum8  ! use of sum8 minimizes false sharing of sum8j
      enddo
!$OMP END PARALLEL DO
c
c     serial sum of rwo-sum loop.
c
      sum8 = sum8j(1)
      do j=2,jdm
        sum8 = sum8 + sum8j(j)
      enddo
      sum = sum8
#if defined(TIMER)
c
      call xctmr1( 5)
#endif
      return
      end subroutine xcsum

      subroutine xcsumj(sumj, a,mask)
      implicit none
c
      real*8,  intent(out)   :: sumj(jtdm)
      real,    intent(inout) :: a(   1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
      integer, intent(in)    :: mask(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
c
c**********
c*
c  1) rwo-sum of a 2-d array, where mask==1, on first processor only
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    sumj            real*8         output    row-sum of a
c    a               real           input     source array
c    mask            integer        input     mask array
c
c  3) sum is bit for bit reproducable for the same halo size, nbdy.
c*
c**********
c
      real*8     zero8
      parameter (zero8=0.0)
c
      real*8  sum8,sum8p
      integer i,i1,j
#if defined(TIMER)
c
      call xctmr0( 5)
#endif
c
c     row sums in 2*nbdy+1 wide strips.
c
!$OMP PARALLEL DO PRIVATE(j,i1,i,sum8,sum8p)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jdm
        sum8 = zero8
        do i1=1,idm,2*nbdy+1
          sum8p = zero8
          do i= i1,min(i1+2*nbdy,idm)
            if     (mask(i,j).eq.1) then
              sum8p = sum8p + a(i,j)
            endif
          enddo
          sum8 = sum8 + sum8p
        enddo
        sumj(j) = sum8  ! use of sum8 minimizes false sharing of sumj
      enddo
!$OMP END PARALLEL DO
#if defined(TIMER)
c
      call xctmr1( 5)
#endif
      return
      end subroutine xcsumj

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

#if defined(ARCTIC)
      subroutine xctila(a,l1,ld,itype)
      implicit none
c
      integer, intent(in)    :: l1,ld,itype
      real,    intent(inout) :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,ld)
c
c**********
c*
c  1) update the top row of a real array.
c     only needed when importing a tripole grid array.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c    l1              integer        input     3rd dim. start index
c    ld              integer        input     3rd dimension of a
c    itype           integer        input     grid and field type
c
c  3) itype selects both the grid and field type
c        itype= 1; p-grid, scalar field
c        itype=11; p-grid, vector field
c
c  4) this version for a global tripole (arctic bipolar patch) grid
c*
c**********
c
      integer i,io,k
#if defined(TIMER)
c
      call xctmr0(13)
#endif
      do k= l1,ld
        if     (itype.eq.1) then
c
c         scalar on p-grid
c
          do i= 1,ii
            io = ii-mod(i-1,ii)
            a(i,jj,k) = a(io,jj-1,k)
          enddo
        else
c
c         vector p-grid field, swap sign
c
          do i= 1,ii
            io = ii-mod(i-1,ii)
            a(i,jj,k) = -a(io,jj-1,k)
          enddo
        endif
      enddo
#if defined(TIMER)
c
      call xctmr1(13)
#endif
      return
      end subroutine xctila
      subroutine xctilr(a,l1,ld,mh,nh,itype)
      implicit none
c
      integer, intent(in)    :: l1,ld,mh,nh,itype
      real,    intent(inout) :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,ld)
c
c**********
c*
c  1) update the tile overlap halo of a real array.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c    l1              integer        input     3rd dim. start index
c    ld              integer        input     3rd dimension of a
c    mh              integer        input     1st (EW) update halo size
c    nh              integer        input     2nd (NS) update halo size
c    itype           integer        input     grid and field type
c
c  3) itype selects both the grid and field type
c        itype= 1; p-grid, scalar field
c        itype= 2; q-grid, scalar field
c        itype= 3; u-grid, scalar field
c        itype= 4; v-grid, scalar field
c        itype=11; p-grid, vector field
c        itype=12; q-grid, vector field
c        itype=13; u-grid, vector field
c        itype=14; v-grid, vector field
c
c  4) this version for a global grid that includes the arctic ocean
c*
c**********
c
      integer i,io,j,k,mhl,nhl
#if defined(TIMER)
c
      call xctmr0(12)
#endif
c
      mhl = max(0,min(mh,nbdy))
      nhl = max(0,min(nh,nbdy))
c
      if     (nhl.gt.0) then
        do k= l1,ld
c
c         southern boundary is closed.
c
          do j= 1,nhl
            do i= 1,ii
              a(i,1-j,k) = vland
            enddo
          enddo
c
          if     (itype.lt.10) then
c
c           scalar field
c
            if     (itype.eq. 1) then
c
c             p-grid
c
              do j= 1,nhl
                do i= 1,ii
                  io = ii-mod(i-1,ii)
                  a(i,jj+j,k) = a(io,jj-1-j,k)
                enddo
              enddo
            elseif (itype.eq. 2) then
c
c             q-grid
c
              do j= 1,nhl
                do i= 1,ii
                  io = mod(ii-(i-1),ii)+1
                  a(i,jj+j,k) = a(io,jj-j,k)
                enddo
              enddo
            elseif (itype.eq. 3) then
c
c             u-grid
c
              do j= 1,nhl
                do i= 1,ii
                  io = mod(ii-(i-1),ii)+1
                  a(i,jj+j,k) = a(io,jj-1-j,k)
                enddo
              enddo
            else
c
c             v-grid
c
              do j= 1,nhl
                do i= 1,ii
                  io = ii-mod(i-1,ii)
                  a(i,jj+j,k) = a(io,jj-j,k)
                enddo
              enddo
            endif
          else
c
c           vector field, swap sign
c
            if     (itype.eq.11) then
c
c             p-grid
c
              do j= 1,nhl
                do i= 1,ii
                  io = ii-mod(i-1,ii)
                  a(i,jj+j,k) = -a(io,jj-1-j,k)
                enddo
              enddo
            elseif (itype.eq.12) then
c
c             q-grid
c
              do j= 1,nhl
                do i= 1,ii
                  io = mod(ii-(i-1),ii)+1
                  a(i,jj+j,k) = -a(io,jj-j,k)
                enddo
              enddo
            elseif (itype.eq.13) then
c
c             u-grid
c
              do j= 1,nhl
                do i= 1,ii
                  io = mod(ii-(i-1),ii)+1
                  a(i,jj+j,k) = -a(io,jj-1-j,k)
                enddo
              enddo
            else
c
c             v-grid
c
              do j= 1,nhl
                do i= 1,ii
                  io = ii-mod(i-1,ii)
                  a(i,jj+j,k) = -a(io,jj-j,k)
                enddo
              enddo
            endif
          endif
        enddo
      endif
c
      if     (mhl.gt.0) then
        do k= 1,ld
          do j= 1-nhl,jj+nhl
            do i= 1,mhl
              a( 1-i,j,k) = a(ii+1-i,j,k)
              a(ii+i,j,k) = a(     i,j,k)
            enddo
          enddo
        enddo
      endif
#if defined(TIMER)
c
      call xctmr1(12)
#endif
      return
      end subroutine xctilr
#else /* !ARCTIC */
      subroutine xctilr(a,l1,ld,mh,nh,itype)
      implicit none
c
      integer, intent(in)    :: l1,ld,mh,nh,itype
      real,    intent(inout) :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,ld)
c
c**********
c*
c  1) update the tile overlap halo of a real   array.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c    l1              integer        input     3rd dim. start index
c    ld              integer        input     3rd dimension of a
c    mh              integer        input     1st (EW) update halo size
c    nh              integer        input     2nd (NS) update halo size
c    itype           integer        input     grid and field type
c
c  3) itype selects both the grid and field type
c        itype= 1; p-grid, scalar field
c        itype= 2; q-grid, scalar field
c        itype= 3; u-grid, scalar field
c        itype= 4; v-grid, scalar field
c        itype=11; p-grid, vector field
c        itype=12; q-grid, vector field
c        itype=13; u-grid, vector field
c        itype=14; v-grid, vector field
c     it is ignored here because all types are the same unless
c      the grid includes the arctic ocean
c*
c**********
c
      integer i,j,k,mhl,nhl
#if defined(TIMER)
c
      call xctmr0(12)
#endif
c
      mhl = max(0,min(mh,nbdy))
      nhl = max(0,min(nh,nbdy))
c
      if     (nhl.gt.0) then
        if     (nreg.le.2) then  ! closed in latitude
          do k= l1,ld
            do j= 1,nhl
              do i= 1,ii
                a(i, 1-j,k) = vland
                a(i,jj+j,k) = vland
              enddo
            enddo
          enddo
        else  ! periodic (f-plane) in latitude
          do k= l1,ld
            do j= 1,nhl
              do i= 1,ii
                a(i, 1-j,k) = a(i,jj+1-j,k)
                a(i,jj+j,k) = a(i,     j,k)
              enddo
            enddo
          enddo
        endif
      endif
c
      if     (mhl.gt.0) then
        if     (nreg.eq.0 .or. nreg.eq.4) then  ! closed in longitude
          do k= l1,ld
            do j= 1-nhl,jj+nhl
              do i= 1,mhl
                a( 1-i,j,k) = vland
                a(ii+i,j,k) = vland
              enddo
            enddo
          enddo
        else  ! periodic in longitude
          do k= l1,ld
            do j= 1-nhl,jj+nhl
              do i= 1,mhl
                a( 1-i,j,k) = a(ii+1-i,j,k)
                a(ii+i,j,k) = a(     i,j,k)
              enddo
            enddo
          enddo
        endif
      endif
#if defined(TIMER)
c
      call xctmr1(12)
#endif
      return
      end subroutine xctilr
#endif /* ARCTIC:else */

      subroutine xctmri
      implicit none
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
c
c  3) time every 50-th event above 5,000.
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
      if     (timer_on) then
        if     (mod(nc(n),50).eq.0 .or. nc(n).le.5000) then
          t0(n) = wtime()
        endif
      endif !timer_on
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
c                                                                        
c  3) time every 50-th event above 5,000.                 
c*
c**********
c
      real*8  wtime
c
      if     (timer_on) then
        if     (nc(n).gt.5000) then
          if     (mod(nc(n),50).eq.0) then
            tc(n) = tc(n) + 50.0*(wtime() - t0(n))
          endif                                   
        else                                      
          tc(n) = tc(n) + (wtime() - t0(n))
        endif                              
        nc(n) = nc(n) + 1                  
      endif !timer_on    
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
      real*8  tci
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
          if     (nc(i).le.5000) then
            tci = tc(i)
          else !correct for timing every 50th event
            tci = (tc(i)*nc(i))/(nc(i)-mod(nc(i),50))
          endif
          if     (cc(i).ne.'      ') then
            write(lp,6100) cc(i),nc(i),tci,tci/nc(i)
          else
            write(lp,6150)    i, nc(i),tci,tci/nc(i)
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
c
c  Revision history:
c
c> Nov. 2011 - time every 50-th event above 5,000 (was 1,000).
c> Mar. 2012 - added optional mnflg to xclput
c> Apr. 2012 - added optional mnflg to xceget and xceput
c> Apr. 2012 - added xciget and xciput
