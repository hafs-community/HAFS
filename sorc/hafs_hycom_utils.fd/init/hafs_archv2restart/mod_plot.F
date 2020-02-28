      module mod_plot
      implicit none
c
c --- HYCOM plot: array allocation interface.
c
c --- ii     = 1st dimension   of plotted subgrid (ii<=idm)
c --- jj     = 2nd dimension   of plotted subgrid (jj<=jdm)
c --- iorign = 1st index start of plotted subgrid (1<iorign<=idm)
c --- jorign = 2nd index start of plotted subgrid (1<jorign<=jdm)
c --- kk     = actual  number of layers
c --- kkmax  = maximum number of layers (usually kk)
c --- ntracr = number of tracers (to plot)
c
      integer, save :: ii,ii1,ii2,iorign,jj,jj1,jj2,jorign,kk,kkmax
      integer, save :: ntracr = 0  ! default to support legacy programs
c
c --- ms-1  = max. number of interruptions of any grid row or column by land
c --- msd-1 = max. number of interruptions of diagonal rows/columns by land
c
      integer, parameter :: ms=99, msd=99
c
c --- input file names
c
      character, save :: dpthfil*64
c
c --- archive header
c
      character, save :: ctitle(4)*80
      integer,   save :: nstep,sigver
c
c --- arrays:
c
      real,    save, allocatable, dimension (:,:,:,:) :: 
     &   trcr
c
      real,    save, allocatable, dimension (:,:,:) :: 
     &   u,v,ke,temp,saln,th3d, dp,dpsd,p
c
      real,    save, allocatable, dimension (:,:,:) :: 
     &   tracer  !never allocated, allows legacy programs to work
c
      real,    save, allocatable, dimension (:,:)   :: 
     &   ubaro,vbaro,pbaro,kebaro, montg,srfht,steric,dpbl,dpmixl,
     &   tmix,smix,thmix,umix,vmix,kemix, 
     &   depths,coast,plon,plat,qlon,qlat,ulon,ulat,vlon,vlat,
     &   surflx,salflx, ttrend,strend,emnp, covice,thkice,temice,
     &   scux,scvx,scpx,scuy,scvy,scpy,pang,
     &   field
c
      real,    save, allocatable, dimension (:)     :: 
     &   theta
c
      integer, save, allocatable, dimension (:,:)   ::
     &   ip,iq,iu,iv,
     &   ifp,ilp,jfp,jlp,
     &   ifq,ilq,jfq,jlq,
     &   ifu,ilu,jfu,jlu,
     &   ifv,ilv,jfv,jlv,
     &   ifd,ild,jfd,jld
c
      integer, save, allocatable, dimension (:)     ::
     &   isp,jsp, isq,jsq, isu,jsu, isv,jsv, nsec, itrcr_type
c
c --- module subroutines
c
      contains

      subroutine plot_alloc
      implicit none
c
c --- initialize allocatable arrays.
c
      if     (kk.ne.0) then  ! usual case, except for new_archiv.
        kkmax = kk
      endif
c
      ii1 = ii - 1
      ii2 = ii - 2
      jj1 = jj - 1
      jj2 = jj - 2
c
      if     (ntracr.gt.0) then
        allocate( trcr(ii,jj,kkmax,ntracr) )
        allocate(         itrcr_type(ntracr) )
      endif
c
      allocate(      u(ii,jj,kkmax) )
      allocate(      v(ii,jj,kkmax) )
*     allocate(     ke(ii,jj,kkmax) );  ke = 0.0  !in getdat.f
      allocate(   temp(ii,jj,kkmax) )
      allocate(   saln(ii,jj,kkmax) )
      allocate(   th3d(ii,jj,kkmax) )
      allocate(     dp(ii,jj,kkmax)   )
*     allocate(   dpsd(ii,jj,kkmax)   )             !in getdat.f
      allocate(      p(ii,jj,kkmax+1) )
c
      allocate(  ubaro(ii,jj) )
      allocate(  vbaro(ii,jj) )
      allocate(  pbaro(ii,jj) )
*     allocate( kebaro(ii,jj) );  kebaro = 0.0      !in getdat.f
      allocate(  montg(ii,jj) )
      allocate(  srfht(ii,jj) )
      allocate( steric(ii,jj) )
      allocate(   dpbl(ii,jj) )
      allocate( dpmixl(ii,jj) )
      allocate(   tmix(ii,jj) )
      allocate(   smix(ii,jj) )
      allocate(  thmix(ii,jj) )
      allocate(   umix(ii,jj) )
      allocate(   vmix(ii,jj) )
*     allocate(  kemix(ii,jj) );  kemix  = 0.0      !in getdat.f
c
      allocate( depths(0:ii,0:jj) )
c
      allocate(  coast(ii,jj) )
      allocate(   plon(ii,jj) )
      allocate(   plat(ii,jj) )
*     allocate(   qlon(ii,jj) )
*     allocate(   qlat(ii,jj) )
*     allocate(   ulon(ii,jj) )
*     allocate(   ulat(ii,jj) )
*     allocate(   vlon(ii,jj) )
*     allocate(   vlat(ii,jj) )
      allocate(   pang(ii,jj) )
      allocate( surflx(ii,jj) )
      allocate( salflx(ii,jj) )
      allocate( ttrend(ii,jj) )
      allocate( strend(ii,jj) )
      allocate(   emnp(ii,jj) )
      allocate( covice(ii,jj) )
      allocate( thkice(ii,jj) )
      allocate( temice(ii,jj) )
      allocate(   scux(ii,jj) )
      allocate(   scvx(ii,jj) )
      allocate(   scpx(ii,jj) )
      allocate(   scuy(ii,jj) )
      allocate(   scvy(ii,jj) )
      allocate(   scpy(ii,jj) )
c
      allocate(     ip(ii,jj) )
      allocate(     iq(ii,jj) )
      allocate(     iu(ii,jj) )
      allocate(     iv(ii,jj) )
c
      allocate(    ifp(jj,ms) )
      allocate(    ilp(jj,ms) )
      allocate(    jfp(ii,ms) )
      allocate(    jlp(ii,ms) )
      allocate(    ifq(jj,ms) )
      allocate(    ilq(jj,ms) )
      allocate(    jfq(ii,ms) )
      allocate(    jlq(ii,ms) )
      allocate(    ifu(jj,ms) )
      allocate(    ilu(jj,ms) )
      allocate(    jfu(ii,ms) )
      allocate(    jlu(ii,ms) )
      allocate(    ifv(jj,ms) )
      allocate(    ilv(jj,ms) )
      allocate(    jfv(ii,ms) )
      allocate(    jlv(ii,ms) )
c
      allocate(    ifd(ii+jj,msd) )
      allocate(    ild(ii+jj,msd) )
      allocate(    jfd(ii+jj,msd) )
      allocate(    jld(ii+jj,msd) )
c
      allocate(  theta(kkmax) )
c
      allocate(    isp(jj) )
      allocate(    jsp(ii) )
      allocate(    isq(jj) )
      allocate(    jsq(ii) )
      allocate(    isu(jj) )
      allocate(    jsu(ii) )
      allocate(    isv(jj) )
      allocate(    jsv(ii) )
c
      allocate(   nsec(ii+jj) )

      end subroutine plot_alloc

      subroutine plot_alloc_field
      implicit none
c
c --- initialize allocatable arrays for field plots.
c
      ii1 = ii - 1
      ii2 = ii - 2
      jj1 = jj - 1
      jj2 = jj - 2
c
      allocate( depths(0:ii,0:jj) )
c
      allocate(  coast(ii,jj) )
      allocate(   plon(ii,jj) )
      allocate(   plat(ii,jj) )
      allocate(  field(ii,jj) )
      allocate(     ip(ii,jj) )

      end subroutine plot_alloc_field

      end module mod_plot
