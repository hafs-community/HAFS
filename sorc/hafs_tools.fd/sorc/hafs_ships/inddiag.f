      program inddiag
c**********************************************************************
c     This program takes the center, grid, and field information for
c      an individual time and calculates diagnostic parameters,
c      then produces a parameter file for that time.
c
c     Needs: diag_util.f
c            lsdiags.f
c
c     Last Modified: 03/02/2012, version 2.0
c**********************************************************************
c 
      USE diag_util
      IMPLICIT NONE
c 
      !begin variable declaration
      integer, parameter :: imiss = -9999, imissd = 9999
      real, parameter :: rmiss = -999.9
      integer :: ierrc, istat, n, irepos, inest, inestuse
      character(len=80) :: fncn, fnpg, fnng, fnpf, fnnf, fnou
      character(len=80) :: fnin, fnpl
      integer :: lucn, lupg, lung, lupf, lunf, luou, luin, lupl
      integer :: ilatc, ilonc, ivmax, ipmin
      real :: slon, slat
c 
      !set up the dimensional variables
      integer :: nlevs, nvart
      integer :: nxp, nyp, nxn, nyn
      real :: latmaxp, latminp, latintp, latmaxn, latminn, latintn
      real :: lonmaxp, lonminp, lonintp, lonmaxn, lonminn, lonintn
      integer :: npointsp, npointsn
c      integer, parameter :: nlevs = 21
c      real, dimension(nlevs) :: usnd=rmiss, vsnd=rmiss
c      real, dimension(nlevs) :: tsnd=rmiss, zsnd=rmiss
c      real, dimension(nlevs) :: rhsnd=rmiss, plevs
c      real, dimension(nlevs) :: usndt=rmiss, vsndt=rmiss
c      real, dimension(nlevs) :: tsndt=rmiss, zsndt=rmiss
c      real, dimension(nlevs) :: rhsndt=rmiss
      real :: usfc=rmiss, vsfc=rmiss, tsfc=rmiss
      real :: rhsfc=rmiss, psfc=rmiss
      real :: usfct=rmiss, vsfct=rmiss, tsfct=rmiss
      real :: rhsfct=rmiss, psfct=rmiss
c 
      !Variables for storm data section
      integer, parameter :: nvar = 16
      real, dimension(nvar) :: diagvar = rmiss, diagvart = rmiss
      integer, dimension(nvar) :: idiagvar = imissd
      character(len=50), dimension(nvar) :: diaglab
c 
      !allocatable sounding arrays
      integer, allocatable, dimension(:) :: iplevs
      real, allocatable, dimension(:) :: plevs
      real, allocatable, dimension(:) :: usnd, vsnd
      real, allocatable, dimension(:) :: tsnd, zsnd, rhsnd
      real, allocatable, dimension(:) :: usndt, vsndt
      real, allocatable, dimension(:) :: tsndt, zsndt, rhsndt
c 
      !start the allocatable arrays for the parent grid
      real, allocatable, dimension(:) :: rlat, rlon
      real, allocatable, dimension(:,:) :: us, vs, ts, rhs, ps
      real, allocatable, dimension(:,:) :: sst, ohc, tpw
      real, allocatable, dimension(:,:,:) :: u, v, t, rh, z
c 
      !set up additional allocatable arrays for the nested grid
      real, allocatable, dimension(:) :: rlatn, rlonn
      real, allocatable, dimension(:,:) :: usn, vsn, tsn, rhsn, psn
      real, allocatable, dimension(:,:) :: sstn, ohcn, tpwn
      real, allocatable, dimension(:,:,:) :: un, vn, tn, rhn, zn
c 
      !end variable declaration
c 
      !set plevs array
c      plevs = (/1013.0, 1000.0, 950.0, 900.0, 850.0, 800.0, 750.0,
c     +          700.0, 650.0, 600.0, 550.0, 500.0, 450.0, 400.0,
c     +          350.0,300.0, 250.0, 200.0, 150.0, 100.0, 50.0/)
c 
      !set flags
      irepos=0   !do not reposition center when calling lsdiags
      ierrc=0
      istat=0
c 
      !set files
      fncn='center.txt'
      lucn=30
      fnpg='temp_gridp.txt'
      lupg=31
      fnng='temp_gridn.txt'
      lung=32
      fnpf='temp_fieldp.txt'
      lupf=33
      fnnf='temp_fieldn.txt'
      lunf=34
      fnin='diaginfo.txt'
      luin=35
      fnpl='input.plvls'
      lupl=36
      fnou='params.txt'
      luou=38
c 
      open(unit=lucn,file=fncn,form='formatted',status='old',err=900)
      open(unit=lupg,file=fnpg,form='formatted',status='old',err=900)
      open(unit=lupf,file=fnpf,form='formatted',status='old',err=900)
      open(unit=luin,file=fnin,form='formatted',status='old',err=900)
      open(unit=lupl,file=fnpl,form='formatted',status='old',err=900)
      open(unit=luou,file=fnou,form='formatted',status='replace',
     +     err=900)
c 
      !read in nlevs and inestuse from diaginfo.txt
      read(luin,*) nlevs
      read(luin,*) inestuse
c
      !open nest files if inestuse is set
      if (inestuse .eq. 1) then
         open(unit=lung,file=fnng,form='formatted',status='old',err=900)
         open(unit=lunf,file=fnnf,form='formatted',status='old',err=900)
      endif
c
      !allocate and initialize sounding arrays
      allocate(iplevs(nlevs),STAT=istat)
      allocate(plevs(nlevs),STAT=istat)
      allocate(usnd(nlevs),STAT=istat)
      allocate(vsnd(nlevs),STAT=istat)
      allocate(tsnd(nlevs),STAT=istat)
      allocate(zsnd(nlevs),STAT=istat)
      allocate(rhsnd(nlevs),STAT=istat)
      allocate(usndt(nlevs),STAT=istat)
      allocate(vsndt(nlevs),STAT=istat)
      allocate(tsndt(nlevs),STAT=istat)
      allocate(zsndt(nlevs),STAT=istat)
      allocate(rhsndt(nlevs),STAT=istat)
      iplevs=imiss
      plevs=rmiss
      usnd=rmiss
      vsnd=rmiss
      tsnd=rmiss
      zsnd=rmiss
      rhsnd=rmiss
      usndt=rmiss
      vsndt=rmiss
      tsndt=rmiss
      zsndt=rmiss
      rhsndt=rmiss
c 
      !read in iplevs array from input.plvls, convert to plevs
      do n=1,nlevs
         read(lupl,*) iplevs(n)
         plevs(n) = float(iplevs(n))
c         write(*,'(i6,f6.1)') iplevs(n),plevs(n)
      enddo
c 
      !read in ilatc, ilonc, ivmax, ipmin from center.txt
      read(lucn,'(4(i6))') ilatc,ilonc,ivmax,ipmin
      slon=float(ilonc)/10.0
      slat=float(ilatc)/10.0
c 
      !debugging test, write out each input
c      write(*,*) ilatc,ilonc,ivmax,ipmin
c 
      !read in, in order: nxp, nyp, latmaxp, latminp, latintp,
      ! lonminp, lonmaxp, lonintp, and npointsp from temp_gridp.txt
      read(lupg,*) nxp
      read(lupg,*) nyp
      read(lupg,*) latminp
      read(lupg,*) latmaxp
      read(lupg,*) latintp
      read(lupg,*) lonminp
      read(lupg,*) lonmaxp
      read(lupg,*) lonintp
      read(lupg,*) npointsp
c 
      !debugging test, write out each input
c      write(*,*) nxp, nyp, latmaxp, latminp, latintp
c      write(*,*) lonminp, lonmaxp, lonintp, npointsp
c 
      !allocate and calculate rlat, rlon
      allocate(rlat(nyp),STAT=istat)
      allocate(rlon(nxp),STAT=istat)
      rlat=rmiss
      rlon=rmiss
      do n=1,nyp
         rlat(n)=latminp+latintp*(n-1)
      enddo
      do n=1,nxp
         rlon(n)=lonminp+lonintp*(n-1)
      enddo
c      write(*,*) rlat
c 
      !allocate and initialize other parent grid arrays
      !2D grids
      allocate(us(nxp,nyp),STAT=istat)
      allocate(vs(nxp,nyp),STAT=istat)
      allocate(ts(nxp,nyp),STAT=istat)
      allocate(rhs(nxp,nyp),STAT=istat)
      allocate(ps(nxp,nyp),STAT=istat)
      allocate(sst(nxp,nyp),STAT=istat)
      allocate(ohc(nxp,nyp),STAT=istat)
      allocate(tpw(nxp,nyp),STAT=istat)
      us=rmiss
      vs=rmiss
      ts=rmiss
      rhs=rmiss
      ps=rmiss
      sst=rmiss
      ohc=rmiss
      tpw=rmiss
      !3D grids (sounding data)
      allocate(u(nxp,nyp,nlevs),STAT=istat)
      allocate(v(nxp,nyp,nlevs),STAT=istat)
      allocate(t(nxp,nyp,nlevs),STAT=istat)
      allocate(rh(nxp,nyp,nlevs),STAT=istat)
      allocate(z(nxp,nyp,nlevs),STAT=istat)
      u=rmiss
      v=rmiss
      t=rmiss
      rh=rmiss
      z=rmiss
c 
      !call readgrid to get parent grid fields
      call readgrid(lupf,nxp,nyp,nlevs,iplevs,rmiss,ierrc,
     +              u,v,t,rh,z,us,vs,ts,rhs,ps,
     +              sst,ohc,tpw)
c 
c 
      !call lsdiags to calculate parameters for parent grid
      !set inest=0 to represent the parent grid
      inest = 0
      call lsdiags(nxp,nyp,nlevs,nxp,nyp,nlevs,inest,u,v,t,z,rh,
     +             us,vs,ts,ps,rhs,sst,ohc,tpw,
     +             rlon,rlat,plevs,slon,slat,irepos,rmiss,
     +             usnd,vsnd,tsnd,zsnd,rhsnd,
     +             usfc,vsfc,tsfc,psfc,rhsfc,
     +             nvar,nvart,diagvar,diaglab,ierrc)
c 
      !repeat process for nested grid - if inestuse is set
      if (inestuse .eq. 1) then
      !read in, in order: nxn, nyn, latmaxn, latminn, latintn,
      ! lonminn, lonmaxn, lonintn, and npointsn from temp_gridn.txt
         read(lung,*) nxn
         read(lung,*) nyn
         read(lung,*) latmaxn
         read(lung,*) latminn
         read(lung,*) latintn
         read(lung,*) lonminn
         read(lung,*) lonmaxn
         read(lung,*) lonintn
         read(lung,*) npointsn
c 
      !debugging test, write out each input
      write(6,*) nxn, nyn, latmaxn, latminn, latintn
      write(6,*) lonminn, lonmaxn, lonintn, npointsn
c 
      !allocate and calculate rlat, rlon
         allocate(rlatn(nyn),STAT=istat)
         allocate(rlonn(nxn),STAT=istat)
         rlatn=rmiss
         rlonn=rmiss
         do n=1,nyn
            rlatn(n)=latminn+latintn*(n-1)
         enddo
         do n=1,nxn
            rlonn(n)=lonminn+lonintn*(n-1)
         enddo
         write(6,*) rlatn
c 
      !allocate and initialize other nested grid arrays
      !2D grids
         allocate(usn(nxn,nyn),STAT=istat)
         allocate(vsn(nxn,nyn),STAT=istat)
         allocate(tsn(nxn,nyn),STAT=istat)
         allocate(rhsn(nxn,nyn),STAT=istat)
         allocate(psn(nxn,nyn),STAT=istat)
         allocate(sstn(nxn,nyn),STAT=istat)
         allocate(ohcn(nxn,nyn),STAT=istat)
         allocate(tpwn(nxn,nyn),STAT=istat)
         usn=rmiss
         vsn=rmiss
         tsn=rmiss
         rhsn=rmiss
         psn=rmiss
         sstn=rmiss
         ohcn=rmiss
         tpwn=rmiss
         !3D grids (sounding data - alll will be rmiss)
         allocate(un(nxn,nyn,nlevs),STAT=istat)
         allocate(vn(nxn,nyn,nlevs),STAT=istat)
         allocate(tn(nxn,nyn,nlevs),STAT=istat)
         allocate(rhn(nxn,nyn,nlevs),STAT=istat)
         allocate(zn(nxn,nyn,nlevs),STAT=istat)
         un=rmiss
         vn=rmiss
         tn=rmiss
         rhn=rmiss
         zn=rmiss
c 
      !call readgrid to get nest grid fields
         call readgrid(lunf,nxn,nyn,nlevs,iplevs,rmiss,ierrc,
     +                 un,vn,tn,rhn,zn,usn,vsn,tsn,rhsn,psn,
     +                 sstn,ohcn,tpwn)
c 
c 
      !call lsdiags to calculate those parameters on the nest
      !set inest=1 to represent the smaller grid
         inest = 1
         call lsdiags(nxn,nyn,nlevs,nxn,nyn,nlevs,inest,un,vn,tn,zn,
     +                rhn,usn,vsn,tsn,psn,rhsn,sstn,ohcn,tpwn,
     +                rlonn,rlatn,plevs,slon,slat,irepos,rmiss,
     +                usndt,vsndt,tsndt,zsndt,rhsndt,
     +                usfct,vsfct,tsfct,psfct,rhsfct,
     +                nvar,nvart,diagvart,diaglab,ierrc)
c 
      !combine for full version of diagvar, idiagvar
         diagvar(4)=diagvart(4)       !replace RMW
         diagvar(10)=diagvart(10)     !replace SST

      !deallocate the nested fields
         deallocate(rlatn,STAT=istat)
         deallocate(rlonn,STAT=istat)
         deallocate(usn,STAT=istat)
         deallocate(vsn,STAT=istat)
         deallocate(tsn,STAT=istat)
         deallocate(rhsn,STAT=istat)
         deallocate(psn,STAT=istat)
         deallocate(sstn,STAT=istat)
         deallocate(ohcn,STAT=istat)
         deallocate(tpwn,STAT=istat)
         deallocate(un,STAT=istat)
         deallocate(vn,STAT=istat)
         deallocate(tn,STAT=istat)
         deallocate(rhn,STAT=istat)
         deallocate(zn,STAT=istat)
      endif  !nested grid
c 
      do n=1,nvar
         idiagvar(n)=nint(diagvar(n))
         if (diagvar(n) .le. rmiss) idiagvar(n)=imissd
         if (n .eq. 1) idiagvar(n)=nint(diagvar(n)*10.0)
         if (n .eq. 2) idiagvar(n)=nint(diagvar(n)*10.0)
         if (n .eq. 3) idiagvar(n)=ivmax
         if (n .eq. 5) idiagvar(n)=ipmin
      enddo
c 
      !call writeparams
      call writeparams(luou,ierrc,rmiss,imiss,imissd,
     +                 nvar,diagvar,idiagvar,nlevs,
     +                 usnd,vsnd,tsnd,rhsnd,zsnd,
     +                 usfc,vsfc,tsfc,rhsfc,psfc)
c 
      !deallocate the parent fields
      deallocate(rlat,STAT=istat)
      deallocate(rlon,STAT=istat)
      deallocate(us,STAT=istat)
      deallocate(vs,STAT=istat)
      deallocate(ts,STAT=istat)
      deallocate(rhs,STAT=istat)
      deallocate(ps,STAT=istat)
      deallocate(sst,STAT=istat)
      deallocate(ohc,STAT=istat)
      deallocate(tpw,STAT=istat)
      deallocate(u,STAT=istat)
      deallocate(v,STAT=istat)
      deallocate(t,STAT=istat)
      deallocate(rh,STAT=istat)
      deallocate(z,STAT=istat)
      !deallocate the sounding fields
      deallocate(iplevs,STAT=istat)
      deallocate(plevs,STAT=istat)
      deallocate(usnd,STAT=istat)
      deallocate(vsnd,STAT=istat)
      deallocate(tsnd,STAT=istat)
      deallocate(zsnd,STAT=istat)
      deallocate(rhsnd,STAT=istat)
      deallocate(usndt,STAT=istat)
      deallocate(vsndt,STAT=istat)
      deallocate(tsndt,STAT=istat)
      deallocate(zsndt,STAT=istat)
      deallocate(rhsndt,STAT=istat)
c 
      close(lucn)
      close(lupg)
      close(lupf)
      close(luin)
      close(lupl)
      close(luou)
      if (inestuse .eq. 1) then
         close(lung)
         close(lunf)
      endif
c 
c      return
      goto 950
c 
  900 continue
      stop 'Error during file open for inddiag'
c 
  950 continue
      end
