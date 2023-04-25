      subroutine blkind(dvar,cvar,cfmt)
      implicit none
c
      double precision dvar
      character        cvar*6,cfmt*(*)
c
      integer       lp
      common/linepr/lp
c
c     read in one d.p. value from stdin
c
      character*6 cvarin
c
      read(*,*) dvar,cvarin
      write(lp,cfmt) cvarin,dvar
      call flush(lp)
c
      if     (cvar.ne.cvarin) then
        write(lp,*) 
        write(lp,*) 'error in blkind - input ',cvarin,
     +                      ' but should be ',cvar
        write(lp,*) 
        call flush(lp)
        stop
      endif
      return
      end
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
      subroutine blkinr2(rvar,nvar,cvar1,cfmt1,cvar2,cfmt2)
      implicit none
c
      real      rvar
      integer   nvar
      character cvar1*6,cvar2*6,cfmt1*(*),cfmt2*(*)
c
      integer       lp
      common/linepr/lp
c
c     read in one real value from stdin,
c     identified as either cvar1 (return nvar=1) or cvar2 (return nvar=2)
c
      character*6 cvarin
c
      read(*,*) rvar,cvarin
c
      if     (cvar1.eq.cvarin) then
        nvar = 1
        write(lp,cfmt1) cvarin,rvar
        call flush(lp)
      elseif (cvar2.eq.cvarin) then
        nvar = 2
        write(lp,cfmt2) cvarin,rvar
        call flush(lp)
      else
        write(lp,*) 
        write(lp,*) 'error in blkinr2 - input ',cvarin,
     +                      ' but should be ',cvar1,' or ',cvar2
        write(lp,*) 
        call flush(lp)
        stop
      endif
      return
      end
      subroutine blkinr3(rvar,nvar,cvar1,cfmt1,cvar2,cfmt2,cvar3,cfmt3)
      implicit none
c
      real      rvar
      integer   nvar
      character cvar1*6,cvar2*6,cvar3*6,cfmt1*(*),cfmt2*(*),cfmt3*(*)
c
      integer       lp
      common/linepr/lp
c
c     read in one of three possible real values from stdin,
c     identified as either cvar1 (return nvar=1) or
c                          cvar2 (return nvar=2) or
c                          cvar3 (return nvar=3)
c
      call blkinr9(rvar,nvar,cvar1,cfmt1,cvar2,cfmt2,cvar3,cfmt3,
     &             'XXXXXX','("blkinr: ",a6," =",f11.4," ")',
     &             'XXXXXX','("blkinr: ",a6," =",f11.4," ")',
     &             'XXXXXX','("blkinr: ",a6," =",f11.4," ")',
     &             'XXXXXX','("blkinr: ",a6," =",f11.4," ")',
     &             'XXXXXX','("blkinr: ",a6," =",f11.4," ")',
     &             'XXXXXX','("blkinr: ",a6," =",f11.4," ")')
      return
      end
      subroutine blkinr9(rvar,nvar,
     &                   cvar1,cfmt1,cvar2,cfmt2,cvar3,cfmt3,
     &                   cvar4,cfmt4,cvar5,cfmt5,cvar6,cfmt6,
     &                   cvar7,cfmt7,cvar8,cfmt8,cvar9,cfmt9)
      implicit none
c
      real      rvar
      integer   nvar
      character cvar1*6,cvar2*6,cvar3*6,cfmt1*(*),cfmt2*(*),cfmt3*(*)
      character cvar4*6,cvar5*6,cvar6*6,cfmt4*(*),cfmt5*(*),cfmt6*(*)
      character cvar7*6,cvar8*6,cvar9*6,cfmt7*(*),cfmt8*(*),cfmt9*(*)
c
      integer       lp
      common/linepr/lp
c
c     read in one of nine possible real values from stdin,
c     identified as either cvar1 (return nvar=1) or
c                          cvar2 (return nvar=2) or
c                          ...
c                          cvar9 (return nvar=9)
c
      character*6 cvarin
c
      read(*,*) rvar,cvarin
c
      if     (cvar1.eq.cvarin) then
        nvar = 1
        write(lp,cfmt1) cvarin,rvar
        call flush(lp)
      elseif (cvar2.eq.cvarin) then
        nvar = 2
        write(lp,cfmt2) cvarin,rvar
        call flush(lp)
      elseif (cvar3.eq.cvarin) then
        nvar = 3
        write(lp,cfmt3) cvarin,rvar
        call flush(lp)
      elseif (cvar4.eq.cvarin) then
        nvar = 4
        write(lp,cfmt4) cvarin,rvar
        call flush(lp)
      elseif (cvar5.eq.cvarin) then
        nvar = 5
        write(lp,cfmt5) cvarin,rvar
        call flush(lp)
      elseif (cvar6.eq.cvarin) then
        nvar = 6
        write(lp,cfmt6) cvarin,rvar
        call flush(lp)
      elseif (cvar7.eq.cvarin) then
        nvar = 7
        write(lp,cfmt7) cvarin,rvar
        call flush(lp)
      elseif (cvar8.eq.cvarin) then
        nvar = 8
        write(lp,cfmt8) cvarin,rvar
        call flush(lp)
      elseif (cvar9.eq.cvarin) then
        nvar = 9
        write(lp,cfmt9) cvarin,rvar
        call flush(lp)
      else
        write(lp,*) 
        write(lp,*) 'error in blkinr9 - input ',cvarin,
     +              ' but should be:'
        write(lp,*) cvar1,' or ',cvar2,' or ',cvar3,' or'
        write(lp,*) cvar4,' or ',cvar5,' or ',cvar6,' or'
        write(lp,*) cvar7,' or ',cvar8,' or ',cvar9
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
c     identified as either cvar1 (return nvar=1) or cvar2 (return nvar=2)
c
      character*6 cvarin
c
      read(*,*) ivar,cvarin
      write(lp,6000) cvarin,ivar
      call flush(lp)
c
      if     (cvarin.eq.cvar1) then
        nvar = 1
      elseif (cvarin.eq.cvar2) then
        nvar = 2
      else
        write(lp,*) 
        write(lp,*) 'error in blkini2 - input ',cvarin,
     +                      ' but should be ',cvar1,' or ',cvar2
        write(lp,*) 
        call flush(lp)
        stop
      endif
      return
 6000 format('blkini: ',a6,' =',i6)
      end
      subroutine blkini3(ivar,nvar,cvar1,cvar2,cvar3)
      implicit none
c
      integer     ivar,nvar
      character*6 cvar1,cvar2,cvar3
c
      integer       lp
      common/linepr/lp
c
c     read in one of three possible integer values from stdin
c     identified as either cvar1 (return nvar=1) or
c                          cvar2 (return nvar=2) or
c                          cvar3 (return nvar=3)
c
      call blkini9(ivar,nvar,cvar1,cvar2,cvar3,
     &                       'XXXXXX','XXXXXX','XXXXXX',
     &                       'XXXXXX','XXXXXX','XXXXXX')
      return
      end
      subroutine blkini4(ivar,nvar,cvar1,cvar2,cvar3,cvar4)
      implicit none
c
      integer     ivar,nvar
      character*6 cvar1,cvar2,cvar3,cvar4
c
      integer       lp
      common/linepr/lp
c
c     read in one of four possible integer values from stdin
c     identified as either cvar1 (return nvar=1) or
c                          cvar2 (return nvar=2) or
c                          cvar3 (return nvar=3) or
c                          cvar4 (return nvar=4)
c
      call blkini9(ivar,nvar,cvar1,   cvar2,   cvar3,
     &                       cvar4,   'XXXXXX','XXXXXX',
     &                       'XXXXXX','XXXXXX','XXXXXX')
      return
      end
      subroutine blkini5(ivar,nvar,cvar1,cvar2,cvar3,cvar4,cvar5)
      implicit none
c
      integer     ivar,nvar
      character*6 cvar1,cvar2,cvar3,cvar4,cvar5
c
      integer       lp
      common/linepr/lp
c
c     read in one of five possible integer values from stdin
c     identified as either cvar1 (return nvar=1) or
c                          cvar2 (return nvar=2) or
c                          cvar3 (return nvar=3) or
c                          cvar4 (return nvar=4) or
c                          cvar5 (return nvar=5)
c
      call blkini9(ivar,nvar,cvar1,   cvar2,   cvar3,
     &                       cvar4,   cvar5,   'XXXXXX',
     &                       'XXXXXX','XXXXXX','XXXXXX')
      return
      end
      subroutine blkini9(ivar,nvar,cvar1,cvar2,cvar3,
     &                             cvar4,cvar5,cvar6,
     &                             cvar7,cvar8,cvar9)
      implicit none
c
      integer     ivar,nvar
      character*6 cvar1,cvar2,cvar3
      character*6 cvar4,cvar5,cvar6
      character*6 cvar7,cvar8,cvar9
c
      integer       lp
      common/linepr/lp
c
c     read in one of nine possible integer values from stdin
c     identified as either cvar1 (return nvar=1) or
c                          cvar2 (return nvar=2) or
c                          ...
c                          cvar9 (return nvar=9)
c                          cvar3 (return nvar=3) or
c
      character*6 cvarin
c
      read(*,*) ivar,cvarin
      write(lp,6000) cvarin,ivar
      call flush(lp)
c
      if     (cvarin.eq.cvar1) then
        nvar = 1
      elseif (cvarin.eq.cvar2) then
        nvar = 2
      elseif (cvarin.eq.cvar3) then
        nvar = 3
      elseif (cvar4.eq.cvarin) then
        nvar = 4
      elseif (cvar5.eq.cvarin) then
        nvar = 5
      elseif (cvar6.eq.cvarin) then
        nvar = 6
      elseif (cvar7.eq.cvarin) then
        nvar = 7
      elseif (cvar8.eq.cvarin) then
        nvar = 8
      elseif (cvar9.eq.cvarin) then
        nvar = 9
      else
        write(lp,*) 
        write(lp,*) 'error in blkini9 - input ',cvarin,
     +              ' but should be:'
        write(lp,*) cvar1,' or ',cvar2,' or ',cvar3,' or'
        write(lp,*) cvar4,' or ',cvar5,' or ',cvar6,' or'
        write(lp,*) cvar7,' or ',cvar8,' or ',cvar9
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
        write(lp,*) 'error in blkinl - input ',cvarin,
     +                      ' but should be ',cvar
        write(lp,*) 
        call flush(lp)
        stop
      endif
      return
 6000 format('blkinl: ',a6,' =',l6)
      end
      subroutine blkinl_99(lvar,cvar)
      implicit none
c
      logical     lvar
      character*6 cvar
c
      integer       lp
      common/linepr/lp
c
c     read in one logical value from unit 99
c     due to a SGI bug for logical I/O: read in an integer 0=F,1=T
c
      character*6 cvarin
      integer     ivar
c
      read(99,*) ivar,cvarin
      lvar = ivar .ne. 0
      write(lp,6000) cvarin,lvar
      call flush(lp)
c
      if     (cvar.ne.cvarin) then
        write(lp,*) 
        write(lp,*) 'error in blkinl_99 - input ',cvarin,
     +                      ' but should be ',cvar
        write(lp,*) 
        call flush(lp)
        stop
      endif
      return
 6000 format('blkinl: ',a6,' =',l6)
      end
      subroutine blkinl2_99(lvar,nvar,cvar1,cvar2)
      implicit none
c
      logical     lvar
      integer     nvar
      character*6 cvar1,cvar2
c
      integer       lp
      common/linepr/lp
c
c     read in one logical value from unit 99
c     identified as either cvar1 (return nvar=1) or cvar2 (return nvar=2)
c     due to a SGI bug for logical I/O: read in an integer 0=F,1=T
c
      character*6 cvarin
      integer     ivar
c
      read(99,*) ivar,cvarin
      lvar = ivar .ne. 0
      write(lp,6000) cvarin,lvar
      call flush(lp)
c
      if     (cvarin.eq.cvar1) then
        nvar = 1
      elseif (cvarin.eq.cvar2) then
        nvar = 2
      else
        write(lp,*) 
        write(lp,*) 'error in blkinl2_99 - input ',cvarin,
     +                      ' but should be ',cvar1,' or ',cvar2
        write(lp,*) 
        call flush(lp)
        stop
      endif
      return
 6000 format('blkinl: ',a6,' =',l6)
      end
