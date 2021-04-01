#if defined(T3E)
# include "zh_t3e.f"
#elif defined(YMP)
# include "zh_ymp.f"
#elif defined(X1)
# include "zh_x1.f"
#elif defined(HPUX)
# include "zh_hp.f"
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
#endif
# if defined(OSX)
      subroutine flush(iunit)
      implicit none
      integer iunit
c
c --- wrapper for flush system call under OSX.
c
      call flush_(iunit)
      return
      end

      subroutine fdate(stm)
      implicit none
      character*24 stm
c
c --- wrapper for fdate (SunOS) system call under OSX.
c --- An example of SunOS fdate output is: "Mon Aug 1 09:24:21 1994".
c
      character*26 str
c
      call fdate_(str)
      stm = str(1:24)
      return
      end

      real*4 function etime(time)
      implicit none
      real*4 time(2)
c
c --- wrapper for etime (SunOS) function system call under OSX.
c
      real(4) etime_
      type tb_type
        sequence
        real(4) usrtime
        real(4) systime
      end type
      type (tb_type) etime_struct
c
      etime   = etime_(etime_struct)
      time(1) = etime_struct%usrtime
      time(2) = etime_struct%systime
      return
      end
#endif
