#if defined(T3E)
# include "zh_t3e.f"
#elif defined(YMP)
# include "zh_ymp.f"
#else
# include "zh_sun.f"
#endif
# if defined(AIX)
      subroutine flush(iunit)
      implicit none
      integer iunit
c
c --- wrapper for flush system call under AIX.
c
      call flush_(iunit)
      return
      end
#endif /* AIX */
# if defined(IFC)
      subroutine flush(iunit)
      implicit none
      integer iunit
c
c --- disable the flush system call under Intel's IFC compiler.
c
      return
      end
#endif /* IFC */
