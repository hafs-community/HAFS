      subroutine blkinr(rvar,cvar,cfmt)
      implicit none
c
      real      rvar
      character cvar*6,cfmt*(*)
c
      integer       lp
      common/linepr/lp
c
c     read in one real value from stdin
c
      character*6 cvarin
c
      read(*,*) rvar,cvarin
      write(lp,cfmt) cvarin,rvar
      call flush(lp)
c
      if     (cvar.ne.cvarin) then
        write(lp,*) 
        write(lp,*) 'error in blkinr - input ',cvarin,
     +                      ' but should be ',cvar
        write(lp,*) 
        call flush(lp)
        stop
      endif
      return
      end
      subroutine blkini(ivar,cvar)
      implicit none
c
      integer     ivar
      character*6 cvar
c
      integer       lp
      common/linepr/lp
c
c     read in one integer value from stdin
c
      character*6 cvarin
c
      read(*,*) ivar,cvarin
      write(lp,6000) cvarin,ivar
      call flush(lp)
c
      if     (cvar.ne.cvarin) then
        write(lp,*) 
        write(lp,*) 'error in blkini - input ',cvarin,
     +                      ' but should be ',cvar
        write(lp,*) 
        call flush(lp)
        stop
      endif
      return
 6000 format('blkini: ',a6,' =',i6)
      end
      subroutine blkini2(ivar,nvar,cvar1,cvar2)
      implicit none
c
      integer     ivar,nvar
      character*6 cvar1,cvar2
c
      integer       lp
      common/linepr/lp
c
c     read in one integer value from stdin
c     identified as either cvar1 (return nvar=1) or cvar1 (return nvar=2)
c
      character*6 cvarin
c
      read(*,*) ivar,cvarin
      write(lp,6000) cvarin,ivar
      call flush(lp)
c
      if     (cvar1.eq.cvarin) then
        nvar = 1
      elseif (cvar2.eq.cvarin) then
        nvar = 2
      else
        write(lp,*) 
        write(lp,*) 'error in blkini - input ',cvarin,
     +                      ' but should be ',cvar1,' or ',cvar2
        write(lp,*) 
        call flush(lp)
        stop
      endif
      return
 6000 format('blkini: ',a6,' =',i6)
      end
      subroutine blkinl(lvar,cvar)
      implicit none
c
      logical     lvar
      character*6 cvar
c
      integer       lp
      common/linepr/lp
c
c     read in one logical value from stdin
c     due to a SGI bug for logical I/O: read in an integer 0=F,1=T
c
      character*6 cvarin
      integer     ivar
c
      read(*,*) ivar,cvarin
      lvar = ivar .ne. 0
      write(lp,6000) cvarin,lvar
      call flush(lp)
c
      if     (cvar.ne.cvarin) then
        write(lp,*) 
        write(lp,*) 'error in blkinr - input ',cvarin,
     +                      ' but should be ',cvar
        write(lp,*) 
        call flush(lp)
        stop
      endif
      return
 6000 format('blkinl: ',a6,' =',l6)
      end
