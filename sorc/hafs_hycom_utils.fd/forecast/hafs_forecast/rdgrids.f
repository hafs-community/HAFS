c===================================================
      subroutine rdgrids(tlon,tlat,ulon,ulat,vlon,vlat,nx,ny)

      implicit none

      REAL*4 tlat(nx,ny),tlon(nx,ny)
      REAL*4 ulat(nx,ny),ulon(nx,ny)
      REAL*4 vlat(nx,ny),vlon(nx,ny)
      REAL*4,  allocatable :: pad(:)

      integer npad
      integer ios,nrecl
      integer i,j
      CHARACTER*240 cfilea
      integer nx,ny

      cfilea = 'regional.grid.a'

      npad = 4096 - MOD(nx*ny,4096)
      if(npad.eq.4096) npad=0

      allocate(pad(npad))

      INQUIRE( IOLENGTH=nrecl) tlon,pad
c      print *,"nx,ny=",nx,ny
c      print *,"npad=",npad
c      print *,"nrecl=",nrecl


      open(unit=11,file=cfilea, form='unformatted', status='old',
     *         access='direct', recl=nrecl, iostat=ios)

      IF (ios.ne.0) THEN
        print *,"error in reading regional.grid.a"
        call exit(1)
      endif

      read(11,rec=1,iostat=ios) tlon
      if (ios.ne.0) then
        print *,"error in reading regional.grid.a, tlon"
        call exit(2)
      endif

      read(11,rec=2,iostat=ios) tlat
      if (ios.ne.0) then
        print *,"error in reading regional.grid.a, tlat"
        call exit(3)
      endif
     
      read(11,rec=5,iostat=ios) ulon
      if (ios.ne.0) then
        print *,"error in reading regional.grid.a, ulon"
        call exit(2)
      endif

      read(11,rec=6,iostat=ios) ulat
      if (ios.ne.0) then
        print *,"error in reading regional.grid.a, ulat"
        call exit(3)
      endif

      read(11,rec=7,iostat=ios) vlon
      if (ios.ne.0) then
        print *,"error in reading regional.grid.a, ulon"
        call exit(2)
      endif

      read(11,rec=8,iostat=ios) vlat
      if (ios.ne.0) then
        print *,"error in reading regional.grid.a, ulat"
        call exit(3)
      endif

 
      do j=1,ny
      do i=1,nx
        if(tlon(i,j).ge.360) tlon(i,j)=tlon(i,j)-360.
        if(ulon(i,j).ge.360) ulon(i,j)=ulon(i,j)-360.
        if(vlon(i,j).ge.360) vlon(i,j)=vlon(i,j)-360.
      enddo
      enddo

      print *,'rdgrids:tlon, min,max=',minval(tlon),maxval(tlon)
      print *,'rdgrids:tlat, min,max=',minval(tlat),maxval(tlat)
c      print *,'rdgrids:ulon, min,max=',minval(ulon),maxval(ulon)
c      print *,'rdgrids:ulat, min,max=',minval(ulat),maxval(ulat)
c      print *,'rdgrids:vlon, min,max=',minval(vlon),maxval(vlon)
c      print *,'rdgrids:vlat, min,max=',minval(vlat),maxval(vlat)

      if(allocated(pad)) deallocate(pad)

      return
      end subroutine rdgrids

