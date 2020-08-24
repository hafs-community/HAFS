      module mod_zb
      implicit none
c
c --- HYCOM I/O interface.
c --- Special serial version, for subregioning only.
c --- Used in conjuction with "za????" routines for the full region.
c --- Serial version, for setup only.
c
      integer, save, private              :: iarec(999)
      real*4,  save, private, allocatable :: w(:)
c
c --- idm    = 1st dimension of sub-region
c --- jdm    = 2nd dimension of sub-region
c     n2drec = size of output 2-d array, multiple of 4096
c     spval  = data void marker, 2^100 or about 1.2676506e30
c
      integer, save, private              :: idm,jdm,n2drec
      real*4,        private, parameter   :: spval=2.0**100
c
      private zbiordd,zbiowrd

      contains

c
c-----------------------------------------------------------------------
c
c     machine dependent I/O routines.
c     single processor version, contained in mod_zb.
c
c     author:  Alan J. Wallcraft,  NRL.
c
c-----------------------------------------------------------------------
c
      subroutine zbiopn(cstat, iaunit)
      implicit none
c
      integer,       intent(in)    :: iaunit
      character*(*), intent(in)    :: cstat
c
c**********
c*
c  1) machine specific routine for opening a file for array i/o.
c
c     must call zbiost before first call to zbiopn.
c     see also 'zbiope' and 'zbiopf'.
c
c  2) the filename is taken from the environment variable FORxxxA,
c       where xxx = iaunit, with default fort.xxxa.
c
c     array i/o is fortran real*4 direct access i/o to unit iaunit+1000.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     cstat indicates the file type, it can be 'scratch', 'old', or
c      'new'.
c     all i/o to iaunit must be performed by zbiord and zbiowr.
c     the file should be closed using zbiocl.
c*
c**********
c
      integer   ios,nrecl
      character cfile*256,cenv*7
      character cact*9
c
c     test file state.
c
      if     (iarec(iaunit).ne.-1) then
        write(6,9000) iaunit
        call flush(6)
        stop
      endif
c
c     get filename.
c
      write(cenv,1000) iaunit
      cfile = ' '
      call getenv(cenv,cfile)
      if     (cfile.eq.' ') then
        write(cfile,1100) iaunit
      endif
*     write(6,*) 'zbiopn - iaunit = ',iaunit
*     call flush(6)
c
c     open file.
c
      inquire(iolength=nrecl) w
c
      if     (cstat.eq.'OLD' .or.
     &        cstat.eq.'old'     ) then
        cact = 'READ'
      elseif (cstat.eq.'NEW' .or.
     &        cstat.eq.'new'     ) then
        cact = 'WRITE'
      else
        cact = 'READWRITE'
      endif
#if defined(YMP)
      if     (mod(nrecl,16384).eq.0 .and. nrecl.gt.16384*4) then
       call asnunit(iaunit+1000,'-F syscall -N ieee',ios)
      else
        call asnunit(iaunit+1000,'-F cachea:8:16:2 -N ieee',ios)
      endif
      if     (ios.ne.0) then
        write(6,9050) iaunit
        write(6,*) 'ios = ',ios
        call flush(6)
        stop
      endif
#endif
      if     (cstat.eq.'scratch' .or.
     &        cstat.eq.'SCRATCH'     ) then
        open(unit=iaunit+1000,             
     &       form='unformatted', status='scratch',
     &       access='direct', recl=nrecl, action=cact, iostat=ios)
      else
        open(unit=iaunit+1000, file=cfile, 
     &       form='unformatted', status=cstat,
     &       access='direct', recl=nrecl, action=cact, iostat=ios)
      endif
      if     (ios.ne.0) then
        write(6,9100) iaunit
        write(6,*) 'ios = ',ios
        call flush(6)
        stop
      endif
      iarec(iaunit) = 0
      return
c
 1000 format('FOR',i3.3,'A')
 1100 format('fort.',i3.3,'a')
 9000 format(/ /10x,'error in zbiopn -  array I/O unit ',
     &   i3,' is not marked as available.'/ /)
#if defined(YMP)
 9050 format(/ /10x,'error in zbiopn -  can''t asnunit ',i3,
     &   ', for array I/O.'/ /)
#endif
 9100 format(/ /10x,'error in zbiopn -  can''t open unit ',i3,
     &   ', for array I/O.'/ /)
      end subroutine zbiopn

      subroutine zbiope(cenv,cstat, iaunit)
      implicit none
c
      integer,       intent(in)    :: iaunit
      character*(*), intent(in)    :: cenv,cstat
c
c**********
c*
c  1) machine specific routine for opening a file for array i/o.
c
c     must call zbiost before first call to zbiope.
c     see also 'zbiopn' and 'zbiopf'.
c
c  2) the filename is taken from environment variable 'cenv'.
c
c     array i/o is fortran real*4 direct access i/o to unit iaunit+1000.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     cstat indicates the file type, it can be 'scratch', 'old', or
c      'new'.
c     all i/o to iaunit must be performed by zbiord and zbiowr.
c      arrays passed to these routines must conform to 'h'.
c     the file should be closed using zbiocl.
c*
c**********
c
      integer   ios,nrecl
      character cfile*256
      character cact*9
c
c     test file state.
c
      if     (iarec(iaunit).ne.-1) then
        write(6,9000) iaunit
        call flush(6)
        stop
      endif
c
c     get filename.
c
      cfile = ' '
      call getenv(cenv,cfile)
      if     (cfile.eq.' ') then
        write(6,9300) cenv(1:len_trim(cenv))
        write(6,*) 'iaunit = ',iaunit
        call flush(6)
        stop
      endif
c
c     open file.
c
*     write(6,*) 'zbiope - iaunit = ',iaunit
*     call flush(6)
*
      inquire(iolength=nrecl) w
c
      if     (cstat.eq.'OLD' .or.
     &        cstat.eq.'old'     ) then
        cact = 'READ'
      elseif (cstat.eq.'NEW' .or.
     &        cstat.eq.'new'     ) then
        cact = 'WRITE'
      else
        cact = 'READWRITE'
      endif
c
#if defined(YMP)
      if     (mod(nrecl,16384).eq.0 .and. nrecl.gt.16384*4) then
       call asnunit(iaunit+1000,'-F syscall -N ieee',ios)
      else
        call asnunit(iaunit+1000,'-F cachea:8:16:2 -N ieee',ios)
      endif
      if     (ios.ne.0) then
        write(6,9050) iaunit,cfile(1:len_trim(cfile))
        write(6,*) 'ios = ',ios
        write(6,*) 'cenv = ',cenv(1:len_trim(cenv))
        call flush(6)
        stop
      endif
#endif
      open(unit=iaunit+1000, file=cfile, 
     &     form='unformatted', status=cstat,
     &     access='direct', recl=nrecl, action=cact, iostat=ios)
      if     (ios.ne.0) then
        write(6,9100) iaunit,cfile(1:len_trim(cfile))
        write(6,*) 'ios  = ',ios
        write(6,*) 'cenv = ',cenv(1:len_trim(cenv))
        call flush(6)
        stop
      endif
      iarec(iaunit) = 0
      return
c
 9000 format(/ /10x,'error in zbiope -  array I/O unit ',
     &   i3,' is not marked as available.'/ /)
#if defined(YMP)
 9050 format(/ /10x,'error in zbiope -  can''t asnunit ',i3,
     &   ', for array I/O.' /
     &   10x,'cfile = ',a/ /)
#endif
 9100 format(/ /10x,'error in zbiope -  can''t open unit ',i3,
     &   ', for array I/O.' /
     &   10x,'cfile = ',a/ /)
 9300 format(/ /10x,'error in zbiope -  environment variable ',a,
     &   ' not defined'/ /)
      end subroutine zbiope

      subroutine zbiopf(cfile,cstat, iaunit)
      implicit none
c
      integer,       intent(in)    :: iaunit
      character*(*), intent(in)    :: cfile,cstat
c
c**********
c*
c  1) machine specific routine for opening a file for array i/o.
c
c     must call zbiost before first call to zbiopf.
c     see also 'zbiopn' and 'zbiope'.
c
c  2) the filename is taken from 'cfile'.
c
c     array i/o is fortran real*4 direct access i/o to unit iaunit+1000.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     cstat indicates the file type, it can be 'scratch', 'old', or
c      'new'.
c     all i/o to iaunit must be performed by zbiord and zbiowr.
c      arrays passed to these routines must conform to 'h'.
c     the file should be closed using zbiocl.
c*
c**********
c
      integer   ios,nrecl
      character cact*9
c
c     test file state.
c
      if     (iarec(iaunit).ne.-1) then
        write(6,9000) iaunit
        call flush(6)
        stop
      endif
c
c     open file.
c
*     write(6,*) 'zbiopf - iaunit = ',iaunit
*     call flush(6)
*
      inquire(iolength=nrecl) w
c
      if     (cstat.eq.'OLD' .or.
     &        cstat.eq.'old'     ) then
        cact = 'READ'
      elseif (cstat.eq.'NEW' .or.
     &        cstat.eq.'new'     ) then
        cact = 'WRITE'
      else
        cact = 'READWRITE'
      endif
c
#if defined(YMP)
      if     (mod(nrecl,16384).eq.0 .and. nrecl.gt.16384*4) then
       call asnunit(iaunit+1000,'-F syscall -N ieee',ios)
      else
        call asnunit(iaunit+1000,'-F cachea:8:16:2 -N ieee',ios)
      endif
      if     (ios.ne.0) then
        write(6,9050) iaunit,cfile(1:len_trim(cfile))
        write(6,*) 'ios   = ',ios
        call flush(6)
        stop
      endif
#endif
      open(unit=iaunit+1000, file=cfile, 
     &     form='unformatted', status=cstat,
     &     access='direct', recl=nrecl, action=cact, iostat=ios)
      if     (ios.ne.0) then
        write(6,9100) iaunit,cfile(1:len_trim(cfile))
        write(6,*) 'ios  = ',ios
        call flush(6)
        stop
      endif
      iarec(iaunit) = 0
      return
c
 9000 format(/ /10x,'error in zbiopf -  array I/O unit ',
     &   i3,' is not marked as available.'/ /)
#if defined(YMP)
 9050 format(/ /10x,'error in zbiopf -  can''t asnunit ',i3,
     &   ', for array I/O.' /
     &   10x,'cfile = ',a/ /)
#endif
 9100 format(/ /10x,'error in zbiopf -  can''t open unit ',i3,
     &   ', for array I/O.' /
     &   10x,'cfile = ',a/ /)
      end subroutine zbiopf

      subroutine zbiopi(lopen, iaunit)
      implicit none
c
      logical, intent(out)   :: lopen
      integer, intent(in)    :: iaunit
c
c**********
c*
c  1) is an array i/o unit open?
c
c  2) must call zbiost before first call to zbiopi.
c*
c**********
c
      lopen = iarec(iaunit).ne.-1
      return
      end subroutine zbiopi

      subroutine zbiost(idm_sub,jdm_sub)
      implicit none
c
      integer, intent(in)    :: idm_sub,jdm_sub
c
c**********
c*
c  1) machine specific routine for initializing array i/o.
c
c  2) see also zbiopn, zbiord, zbiowr, and zbiocl.
c*
c**********
c
      if     (.not. allocated(w) ) then
c
c       idm    = 1st dimension of sub-region
c       jdm    = 2nd dimension of sub-region
c       n2drec = size of output 2-d array, multiple of 4096
c
        idm    = idm_sub
        jdm    = jdm_sub
        n2drec = ((idm*jdm+4095)/4096)*4096
c
c       initialize I/O buffer
c
        allocate( w(n2drec) )
c
c       initialize record counters
c
        iarec(:) = -1
      endif
      return
      end subroutine zbiost

      subroutine zbiocl(iaunit)
      implicit none
c
      integer, intent(in)    :: iaunit
c
c**********
c*
c  1) machine specific routine for array i/o file closing.
c
c     must call zbiopn for this array unit before calling zbiocl.
c
c  2) array i/o is fortran real*4 direct access i/o to unit iaunit+1000.
c*
c**********
c
      integer ios
c
*     write(6,*) 'zbiocl - iaunit = ',iaunit
*     call flush(6)
      if     (iarec(iaunit).lt.0) then
        write(6,9000) iaunit
        call flush(6)
        stop
      endif
c
      close(unit=iaunit+1000, status='keep')
#if defined(T3E) || defined(YMP) || defined(X1)
      call asnunit(iaunit+1000,'-R',ios)
#endif
      iarec(iaunit) = -1
      return
c
 9000 format(/ /10x,'error in zbiocl -  array I/O unit ',
     &   i3,' is not marked as open.'/ /)
      end subroutine zbiocl

      subroutine zbiofl(iaunit)
      implicit none
c
      integer, intent(in)    :: iaunit
c
c**********
c*
c  1) machine specific routine for array i/o buffer flushing.
c
c     must call zbiopn for this array unit before calling zbiocl.
c
c  2) array i/o is fortran real*4 direct access i/o to unit iaunit+1000.
c*
c**********
c
      integer   irlen
      character cfile*256
c
      if     (iarec(iaunit).lt.0) then
        write(6,9000) iaunit
        call flush(6)
        stop
      endif
c
      inquire(unit=iaunit+1000, name=cfile, recl=irlen)
      close(  unit=iaunit+1000, status='keep')
      open(   unit=iaunit+1000, file=cfile, form='unformatted', 
     &        access='direct', recl=irlen)
      return
c
 9000 format(/ /10x,'error in zbiofl -  array I/O unit ',
     &   i3,' is not marked as open.'/ /)
      end subroutine zbiofl

      subroutine zbiorw(iaunit)
      implicit none
c
      integer, intent(in)    :: iaunit
c
c**********
c*
c  1) machine specific routine for array i/o file rewinding.
c
c     must call zbiopn for this array unit before calling zbiocl.
c
c  2) array i/o is fortran real*4 direct access i/o to unit iaunit+1000.
c*
c**********
c
      if     (iarec(iaunit).lt.0) then
        write(6,9000) iaunit
        call flush(6)
        stop
      endif
c
      iarec(iaunit) = 0
*     write(6,*) 'zbiorw - iaunit,rec = ',iaunit,iarec(iaunit)
*     call flush(6)
      return
c
 9000 format(/ /10x,'error in zbiorw -  array I/O unit ',
     &   i3,' is not marked as open.'/ /)
      end subroutine zbiorw

      subroutine zbiord3(h, l, mask,lmask, hmin,hmax,  iaunit)
      implicit none
c
      logical, intent(in)    :: lmask
      integer, intent(in)    :: l,iaunit
      integer, dimension (1:idm,1:jdm),
     &         intent(in)    :: mask
#if defined(REAL4)
      real*4,  intent(out)   :: hmin(l),hmax(l)
      real*4,  dimension (1:idm,1:jdm,l),
     &         intent(out)   :: h
#else
      real,    intent(out)   :: hmin(l),hmax(l)
      real,    dimension (1:idm,1:jdm,l),
     &         intent(out)   :: h
#endif
c
c**********
c*
c  1) machine specific routine for 3-d array reading.
c
c     must call zbiopn for this array unit before calling zbiord.
c
c  2) array i/o is fortran real*4 direct access i/o to unit iaunit+1000.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     the array, 'h',  must conform to that passed in the associated
c      call to zbiopn.
c
c  4) hmin,hmax are returned as the minimum and maximum value in the 
c     array, ignoring array elements set to 2.0**100.  
c     if lmask==.true. the range is calculated only where mask.ne.0,
c     with all other values unchanged in h on exit.  It is then an
c     error if mask.ne.0 anywhere the input is 2.0**100.
c*
c**********
c
c     this version just calls zbiord l times.
c
      integer k
c
      do k= 1,l
        call zbiord(h(1,1,k), mask,lmask,
     &              hmin(k),hmax(k), iaunit)
      enddo
c
      return
      end subroutine zbiord3

      subroutine zbiord(h, mask,lmask, hmin,hmax,  iaunit)
      implicit none
c
      logical, intent(in)    :: lmask
      integer, intent(in)    :: iaunit
      integer, dimension (1:idm,1:jdm),
     &         intent(in)    :: mask
#if defined(REAL4)
      real*4,  intent(out)   :: hmin,hmax
      real*4,  dimension (1:idm,1:jdm),
     &         intent(out)   :: h
#else
      real,    intent(out)   :: hmin,hmax
      real,    dimension (1:idm,1:jdm),
     &         intent(out)   :: h
#endif
c
c**********
c*
c  1) machine specific routine for array reading.
c
c     must call zbiopn for this array unit before calling zbiord.
c
c  2) array i/o is fortran real*4 direct access i/o to unit iaunit+1000.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     the array, 'h',  must conform to that passed in the associated
c      call to zbiopn.
c
c  4) hmin,hmax are returned as the minimum and maximum value in the 
c     array, ignoring array elements set to 2.0**100.  
c     if lmask==.true. the range is calculated only where mask.ne.0,
c     with all other values unchanged in h on exit.  It is then an
c     error if mask.ne.0 anywhere the input is 2.0**100.
c*
c**********
c
      integer   ios, i,j
      real*4    wmin,wmax
c
*     write(6,*) 'zbiord - iaunit,rec = ',iaunit,iarec(iaunit)
*     call flush(6)
      if     (iarec(iaunit).lt.0) then
        write(6,9000) iaunit
        call flush(6)
        stop
      endif
c
      iarec(iaunit) = iarec(iaunit) + 1
      call zbiordd(w,n2drec, iaunit+1000,iarec(iaunit),ios)
      if     (ios.ne.0) then
        write(6,9100) iarec(iaunit),iaunit
        write(6,*) 'ios = ',ios
        call flush(6)
        stop
      endif
      wmin =  spval 
      wmax = -spval 
      if     (lmask) then
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&              REDUCTION(MIN:wmin) REDUCTION(MAX:wmax)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j= 1,jdm
          do i= 1,idm
            if     (mask(i,j).ne.0) then
              h(i,j) = w(i+(j-1)*idm)
              wmin = min( wmin, w(i+(j-1)*idm) )
              wmax = max( wmax, w(i+(j-1)*idm) )
            endif
          enddo
        enddo
        if     (wmax.eq.spval) then
          write(6,9200) iarec(iaunit),iaunit
          call flush(6)
          stop
        endif
      else
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&              REDUCTION(MIN:wmin) REDUCTION(MAX:wmax)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j= 1,jdm
          do i= 1,idm
            h(i,j) = w(i+(j-1)*idm)
            if     (w(i+(j-1)*idm).ne.spval) then
              wmin = min( wmin, w(i+(j-1)*idm) )
              wmax = max( wmax, w(i+(j-1)*idm) )
            endif
          enddo
        enddo
      endif
      hmin = wmin
      hmax = wmax
c
      return
c
 9000 format(/ /10x,'error in zbiord -  array I/O unit ',
     &   i3,' is not marked as open.'/ /)
 9100 format(/ /10x,'error in zbiord -  can''t read record',
     &   i4,' on array I/O unit ',i3,'.'/ /)
 9200 format(/ /10x,'error in zbiord -  record',
     &   i4,' on array I/O unit ',i3,
     &   ' has 2.0**100 outside masked region.'/ /)
      end subroutine zbiord

      subroutine zbiordd(a,n, iunit,irec,ios)
      implicit none
c
      integer, intent(in)    :: n,iunit,irec
      integer, intent(out)   :: ios
      real*4,  intent(out)   :: a(n)
c
c**********
c*
c 1)  direct access read a single record.
c
c 2)  expressed as a subroutine because i/o with 
c     implied do loops can be slow on some machines.
c*
c**********
c
      read(unit=iunit, rec=irec, iostat=ios) a
#if defined(ENDIAN_IO)
      call zaio_endian(a,n)
#endif
      return
      end subroutine zbiordd

      subroutine zbiosk(iaunit)
      implicit none
c
      integer, intent(in)    :: iaunit
c
c**********
c*
c  1) machine specific routine for skipping an array read.
c
c     must call zbiopn for this array unit before calling zbiosk.
c
c  2) array i/o is fortran real*4 direct access i/o to unit iaunit+1000.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     the array, 'h',  must conform to that passed in the associated
c      call to zbiopn.
c*
c**********
c
*     write(6,*) 'zbiosk - iaunit,rec = ',iaunit,iarec(iaunit)
*     call flush(6)
      if     (iarec(iaunit).lt.0) then
        write(6,9000) iaunit
        call flush(6)
        stop
      endif
c
      iarec(iaunit) = iarec(iaunit) + 1
      return
c
 9000 format(/ /10x,'error in zbiosk -  array I/O unit ',
     &   i3,' is not marked as open.'/ /)
      end subroutine zbiosk

      subroutine zbiowr3(h, l, mask,lmask, hmin,hmax, iaunit, lreal4)
      implicit none
c
      logical, intent(in)    :: lmask,lreal4
      integer, intent(in)    :: l,iaunit
      integer, dimension (1:idm,1:jdm),
     &         intent(in)    :: mask
#if defined(REAL4)
      real*4,  intent(out)   :: hmin(l),hmax(l)
      real*4,  dimension (1:idm,1:jdm,l),
     &         intent(inout) :: h
#else
      real,    intent(out)   :: hmin(l),hmax(l)
      real,    dimension (1:idm,1:jdm,l),
     &         intent(inout) :: h
#endif
c
c**********
c*
c  1) machine specific routine for 3-d array writing.
c
c     must call zbiopn for this array unit before calling zbiord.
c
c  2) array i/o is fortran real*4 direct access i/o to unit iaunit+1000.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     the array, 'h',  must conform to that passed in the associated
c      call to zbiopn.
c
c  4) hmin,hmax are returned as the minimum and maximum value in the array.
c     if lmask==.true. the range is only where mask.ne.0, with all other
c     values output as 2.0**100.
c
c  5) If lreal4==.true. then h is overwritten on exit with real*4 version
c     of the same array.  This is typically used for reproducability on
c     restart.
c*
c**********
c
c     this version just calls zbiowr l times.
c
      integer k
c
      do k= 1,l
        call zbiowr(h(1,1,k), mask,lmask,
     &              hmin(k),hmax(k), iaunit, lreal4)
      enddo
      return
      end subroutine zbiowr3

      subroutine zbiowr(h, mask,lmask, hmin,hmax,  iaunit, lreal4)
      implicit none
c
      logical, intent(in)    :: lmask,lreal4
      integer, intent(in)    :: iaunit
      integer, dimension (1:idm,1:jdm),
     &         intent(in)    :: mask
#if defined(REAL4)
      real*4,  intent(out)   :: hmin,hmax
      real*4,  dimension (1:idm,1:jdm),
     &         intent(inout) :: h
#else
      real,    intent(out)   :: hmin,hmax
      real,    dimension (1:idm,1:jdm),
     &         intent(inout) :: h
#endif
c
c**********
c*
c  1) machine specific routine for array writing.
c
c     must call zbiopn for this array unit before calling zbiord.
c
c  2) array i/o is fortran real*4 direct access i/o to unit iaunit+1000.
c
c  3) iaunit+1000 is the i/o unit used for arrays.  array i/o might not
c      use fortran i/o units, but, for compatability, assume that
c      iaunit+1000 refers to a fortran i/o unit anyway.
c     the array, 'h',  must conform to that passed in the associated
c      call to zbiopn.
c
c  4) hmin,hmax are returned as the minimum and maximum value in the array.
c     if lmask==.true. the range is only where mask.ne.0, with all other
c     values output as 2.0**100.
c
c  5) If lreal4==.true. then h is overwritten on exit with real*4 version
c     of the same array.  This is typically used for reproducability on
c     restart.
c*
c**********
c
      integer   ios, i,j
      real*4    wmin,wmax
c
      if     (iarec(iaunit).lt.0) then
        write(6,9000) iaunit
        call flush(6)
        stop
      endif
c
      wmin =  spval
      wmax = -spval
      if     (lreal4) then
        if     (lmask) then
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&                REDUCTION(MIN:wmin) REDUCTION(MAX:wmax)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j= 1,jdm
            do i= 1,idm
              if     (mask(i,j).ne.0) then
                w(i+(j-1)*idm) = h(i,j)
                wmin = min( wmin, w(i+(j-1)*idm) )
                wmax = max( wmax, w(i+(j-1)*idm) )
              else
                w(i+(j-1)*idm) = spval
              endif
#if defined(REAL4)
! ---         h(i,j) = w(i+(j-1)*idm)  ! h is already real*4
#else
              h(i,j) = w(i+(j-1)*idm)  ! h is not real*4, so update it
#endif
            enddo
          enddo
        else
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&                REDUCTION(MIN:wmin) REDUCTION(MAX:wmax)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j= 1,jdm
            do i= 1,idm
              w(i+(j-1)*idm) = h(i,j)
              if     (w(i+(j-1)*idm).ne.spval) then
                wmin = min( wmin, w(i+(j-1)*idm) )
                wmax = max( wmax, w(i+(j-1)*idm) )
              endif
#if defined(REAL4)
! ---         h(i,j) = w(i+(j-1)*idm)  ! h is already real*4
#else
              h(i,j) = w(i+(j-1)*idm)  ! h is not real*4, so update it
#endif
            enddo
          enddo
        endif
      else
        if     (lmask) then
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&                REDUCTION(MIN:wmin) REDUCTION(MAX:wmax)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j= 1,jdm
            do i= 1,idm
              if     (mask(i,j).ne.0) then
                w(i+(j-1)*idm) = h(i,j)
                wmin = min( wmin, w(i+(j-1)*idm) )
                wmax = max( wmax, w(i+(j-1)*idm) )
              else
                w(i+(j-1)*idm) = spval
              endif
            enddo
          enddo
        else
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&                REDUCTION(MIN:wmin) REDUCTION(MAX:wmax)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j= 1,jdm
            do i= 1,idm
              w(i+(j-1)*idm) = h(i,j)
              if     (w(i+(j-1)*idm).ne.spval) then
                wmin = min( wmin, w(i+(j-1)*idm) )
                wmax = max( wmax, w(i+(j-1)*idm) )
              endif
            enddo
          enddo
        endif
      endif
      do i= idm*jdm+1,n2drec
        w(i) = spval
      enddo
      hmin = wmin
      hmax = wmax
      iarec(iaunit) = iarec(iaunit) + 1
      call zbiowrd(w,n2drec, iaunit+1000,iarec(iaunit),ios)
      if     (ios.ne.0) then
        write(6,9100) iarec(iaunit),iaunit
        call flush(6)
        stop
      endif
      return
c
 9000 format(/ /10x,'error in zbiowr -  array I/O unit ',
     &   i3,' is not marked as open.'/ /)
 9100 format(/ /10x,'error in zbiowr -  can''t write record',
     &   i4,' on array I/O unit ',i3,'.'/ /)
      end subroutine zbiowr

      subroutine zbiowrd(a,n, iunit,irec,ios)
      implicit none
c
      integer, intent(in)    :: n,iunit,irec
      integer, intent(out)   :: ios
      real*4,  intent(in)    :: a(n)
c
c**********
c*
c 1)  direct access write a single record.
c
c 2)  expressed as a subroutine because i/o with 
c     implied do loops can be slow on some machines.
c*
c**********
c
#if defined(ENDIAN_IO)
      call zaio_endian(a,n)  ! overwrites a
#endif
      write(unit=iunit, rec=irec, iostat=ios) a
      return
      end subroutine zbiowrd

      end module mod_zb
