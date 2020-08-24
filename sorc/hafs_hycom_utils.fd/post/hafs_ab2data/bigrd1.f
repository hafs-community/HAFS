      subroutine bigrd1(depth)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
c
      real depth(0:ii,0:jj)
c
c --- determine i and j indices of first (ifd,jfd) and last (ild,jld)
c --- points on each contiguous diagonal section of interior q points.
c --- these interior diagonal indices are required for poisson solver
c
      integer, allocatable :: ip1(:,:),iq1(:,:),id(:,:),jd(:,:),npts(:)
c
c --- mpd = maximum number of points on a diagonal
      mpd=min(ii,jj)
c
      allocate(   ip1( ii,   jj) )
      allocate(   iq1( ii,   jj) )
      allocate(    id(mpd,ii+jj) )
      allocate(    jd(mpd,ii+jj) )
      allocate(  npts(    ii+jj) )
c
      ip1 = 0
      iq1 = 0
c
c --- mass points are defined where water depth is greater than zero
      do 2 i=1,ii1
      do 2 j=1,jj1
      if (depth(i,j).gt.0.) then
        ip1(i,j)=1
      endif
  2   continue
c
c --- 'interior' q points require water on all 4 sides.
      do 5 j=2,jj1
      do 5 i=2,ii1
      if (min0(ip1(i,j),ip1(i-1,j),ip1(i,j-1),ip1(i-1,j-1)).gt.0) then
        iq1(i,j)=1
      endif
  5   continue
c
      do 40 npt=1,mpd
      do 40 ndiag=1,ii+jj
      id(npt,ndiag)=0
 40   jd(npt,ndiag)=0
      do 41 ndiag=1,ii+jj
 41   npts(ndiag)=0
      do 42 ndiag=1,ii+jj
      nsec(ndiag)=0
      do 42 k=1,msd
      ifd(ndiag,k)=0
      jfd(ndiag,k)=0
      ild(ndiag,k)=0
 42   jld(ndiag,k)=0
      ndiag=0
      do 31 i=1,ii
      ndiag=ndiag+1
      i1=i
      j1=1
      npt=0
      k=0
 33   ik=i1-k
      jk=j1+k
      if (ik.lt.1.or.jk.gt.jj) go to 31
      if (iq1(ik,jk).ne.1) go to 32
      npt=npt+1
      if (npt.gt.mpd) stop 'bigrid-1'
      id(npt,ndiag)=ik
      jd(npt,ndiag)=jk
      npts(ndiag)=npt
 32   k=k+1
      go to 33
 31   continue
      do 34 j=2,jj
      ndiag=ndiag+1
      i1=ii
      j1=j
      npt=0
      k=0
 36   ik=i1-k
      jk=j1+k
      if (ik.lt.1.or.jk.gt.jj) go to 34
      if (iq1(ik,jk).ne.1) go to 35
      npt=npt+1
      if (npt.gt.mpd) stop 'bigrid-2'
      id(npt,ndiag)=ik
      jd(npt,ndiag)=jk
      npts(ndiag)=npt
 35   k=k+1
      go to 36
 34   continue
      do 44 nd=1,ndiag
      if (npts(nd).eq.0) go to 44
      k=1
      npt=1
 47   ifd(nd,k)=id(npt,nd)
      jfd(nd,k)=jd(npt,nd)
 46   npt=npt+1
      if (npt.gt.npts(nd)) go to 45
      if (jd(npt,nd).eq.jd(npt-1,nd)+1) go to 46
      ild(nd,k)=id(npt-1,nd)
      jld(nd,k)=jd(npt-1,nd)
      k=k+1
      if (k.gt.msd) then
        write (lp,'(a,2i5,a,2i5)')
     .    'error in bigrid -- msd too small at i,j =',id(npt,nd),
     .    jd(npt,nd),'  in diagnonal passing through i,j =',
     .    id(1,nd),jd(1,nd)
        stop '(bigrid)'
      end if
      go to 47
 45   ild(nd,k)=id(npt-1,nd)
      jld(nd,k)=jd(npt-1,nd)
      nsec(nd)=k
 44   continue
c
      deallocate( ip1, iq1, id, jd, npts )
      return
      end
