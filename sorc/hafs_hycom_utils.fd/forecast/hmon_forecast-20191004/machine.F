c
c --- machine-specific Fortran routines
c
      subroutine machine()
c
c --- always called once at the start of the program.
c
#if defined(SGI)
      call zunder()  ! C-wrapper to flush underflow to zero on R10000
#endif
      end
#if defined(AIX)
      subroutine flush(iunit)
      implicit none
      integer iunit
c
c --- wrapper for flush system call under AIX.
c
      integer*4 iunit4
c
      iunit4=iunit
      call flush_(iunit4)
      return
      end
#endif /* AIX */
#if defined(X1)
      subroutine x1flush(iunit)
      implicit none
      integer iunit
c
c --- wrapper for flush system call on the Cray X1.
c
      integer ierr
c
      call FLUSH(iunit,ierr)
      return
      end
#endif /* X1 */
#if defined(IFC)
      subroutine flush(iunit)
      implicit none
      integer iunit
c
c --- disable the flush system call under Intel's IFC compiler.
c
      return
      end
#endif /* IFC */
#if defined(SUN)
      subroutine ieee_retrospective()
c
c     dummy routine to turn off ieee warning messages on a Sun.
c
      end
#endif /* SUN */
#if defined(T3E) || defined(YMP) || defined(X1)
      subroutine getenv(cname, cvalue)
      implicit none
c
      character*(*) cname,cvalue
c
c     this subroutine provides getenv functionality
c     on the t3e, using pxfgetenv.
c
      integer iname,ivalue,ierr
c
      iname = 0
      ierr  = 0
      call pxfgetenv(cname,iname, cvalue,ivalue, ierr)
      if     (ierr.ne.0) then
        cvalue = ' '
      endif
      return
c     end of getenv.
      end
#endif /* T3E || YMP || X1 */
