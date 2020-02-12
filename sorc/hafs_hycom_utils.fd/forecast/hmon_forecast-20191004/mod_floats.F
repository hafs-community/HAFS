#if defined(ROW_LAND)
#define SEA_P .true.
#define SEA_U .true.
#define SEA_V .true.
#elif defined(ROW_ALLSEA)
#define SEA_P allip(j).or.ip(i,j).ne.0
#define SEA_U alliu(j).or.iu(i,j).ne.0
#define SEA_V alliv(j).or.iv(i,j).ne.0
#else
#define SEA_P ip(i,j).ne.0
#define SEA_U iu(i,j).ne.0
#define SEA_V iv(i,j).ne.0
#endif
      module mod_floats
      use mod_xc    ! HYCOM communication interface
      use mod_pipe  ! HYCOM debugging interface
#if defined(STOKES)
      use mod_stokes   !  Stokes Drift Velocity Module
#endif
c
      implicit none
c
c --- HYCOM synthetic floats, drifters and moorings
c --- See subroutine floats (below) for more information
c
      integer, parameter, public ::  nfldim=400  !maximum number of synthetic floats

      real, allocatable, dimension(:,:,:), save, public ::  
     &   wveli,        ! interface vertical velocity
     &   uold2,
     &   vold2,
     &   wold2u,
     &   wold2d

      real, allocatable, dimension(:,:),   save, public ::
     &   dlondx,
     &   dlondy,
     &   dlatdx,
     &   dlatdy

      real, allocatable, dimension(:,:,:), save, private ::
     &   wvelup,
     &   wveldn

      real, allocatable, dimension(:,:),   save, private :: 
     &  dpdxup,
     &  dpdyup,
     &  dpdxdn,
     &  dpdydn

      real,    save :: flt(nfldim,13),
     &                 deltfl,fldepm,tbvar,tdecri,dtturb,uturb0

      integer, save :: kfloat(nfldim),iflnum(nfldim),ifltyp(nfldim),
     &                 nflsam,nfldta,fltflg,nfladv,intpfl,iturbv,ismpfl,
     &                 nflout,nfl,nflt,nstepfl

      logical, save :: synflt,turbvel,samplfl,wvelfl,hadvfl,nonlatlon

      character(48), save :: flnmflti,flnmflto,flnmfltio

      contains

      subroutine floats_init(m,n,time0)
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
      integer m,n
      real    time0
c
c --- read initial float data
c --- initialize parameters and arrays required for floats
c
      integer i,j,k,l,margin
      integer nbcday,nsmday,ityp
c
      include 'stmt_fns.h'
c
c --- initialize flags
c
c --- wvelfl: calculate vertical velocity?
      wvelfl=.false.
c
c --- hadvfl: horizontally advect the float?
      hadvfl=.false.
c
c --- nonlatlon: does the model grid cross latitude/longitude lines anywhere
c --- in the domain?
      nonlatlon=.false.
c
c --- initialize file names
      flnmflti = 'floats.input'
      flnmflto = 'floats_out'
      flnmfltio= 'floats.input_out'
c
c -------------------------
c --- set timing parameters
c -------------------------
c
c --- must have an integer number of baroclinic time steps per day
      nbcday=nint(86400.0/baclin)
      if (real(nbcday).ne.86400.0/baclin) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - need integer no. of bacl. time steps/day'
        call flush(lp)
        endif !1st tile
        call xcstop('(floats_init)')
               stop '(floats_init)'
      endif
c
c --- parameter nfladv is entered in blkdat.input as the number of
c --- baroclinic time steps between updates of float position. at model
c --- time nstep when the float is to be advected, velocity fields saved
c --- at times nstep, nstep-nfladv/2, and nstep-nfladv are used to perform
c --- the runga-kutta time interpolation.
c
c --- nfladv must be set so the float is advected every 2-4 hours, but
c --- also must be no smaller than 4. This ensures that the runga-
c --- kutta time interpolation is performed with a delta-time of 1-2 hours.
c
c --- if all floats are stationary (synthetic moorings), then nfladv
c --- must still be no smaller than 4, but the above time restrection
c --- is not necessary. this allows the user to sample at very high
c --- frequency. for example, if baclin is 360 seconds, then setting
c --- nfladv to 10 will allow water properties to be sampled once per hour
c
c --- set variable nfldta to nfladv/2 so that it equals the number of time
c --- steps separating the velocity fields input into the runga-kutta
c --- interpolation
c
      nfldta=nfladv/2
c
c --- make sure that the float will be advected at an integer number of
c --- temporal points per day
      nsmday=nbcday/nfladv
      if (int(real(nbcday)/real(2*nfldta)).ne.nsmday) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - need integer no. of adv. times per day'
        call flush(lp)
        endif !1st tile
        call xcstop('(floats_init)')
               stop '(floats_init)'
      endif
c
c --- test to make sure that the advection time interval is between 2
c --- and 4 hours. only stop the program later if there are floats to
c --- be advected
      if (nfladv.ne.4 .and. (nsmday.gt.12  .or. nsmday.lt.6)) then
        nfladv=-nfladv
      endif
c
c --- initialize number of time steps between float output
      if (nflsam.gt.0) then
        nflsam=nflsam*nbcday
      elseif (nflsam.eq.0) then
        nflsam=abs(nfladv)
      endif
c
c --- calculate the time interval for the runga-kutta interpolation
c --- as 1/2 of the advection time interval
      deltfl=nfldta*baclin
c
c --- set minimum float depth (m)
      fldepm=1.0
c
c --- -------------------------------------------------------------
c --- initialize synthetic drifters, floats, and moored instruments
c --- -------------------------------------------------------------
c
      open(unit=uoff+99,file=trim(flnminp)//flnmflti,status='old') !on all nodes
c
c --- first line in float input file is the number of floats
      read(uoff+99,*) nflt
      if (nflt.gt.nfldim) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - increase nfldim in mod_floats.F'
        call flush(lp)
        endif !1st tile
        call xcstop('(floats_init)')
               stop '(floats_init)'
      endif
      if (nflt.eq.0) then
        synflt=.false.
      endif
c
c --- second line in float input file is the initial float time step
c --- this number is set to zero for the first model run segment
      read(uoff+99,*) nstepfl
c
      do nfl=1,nflt
c
c --- read input file containing the following information for each float:
c ---
c ---   column 1 - initial sequential float number
c ---   column 2 - float type
c ---              1 = 3-d lagrangian (vertically advected by diagnosed w)
c ---              2 = isopycnic
c ---              3 = isobaric (surface drifter when released in sfc. layer)
c ---              4 = stationary (synthetic moored instrument)
c ---   column 3 - deployment time (days from model start, 0.0 = immediate)
c ---   column 4 - termination time (days from model start, 0.0 = forever)
c ---   column 5 - initial longitude (must be between minimum and maximum
c ---                                 longitudes defined in regional.grid.b)
c ---   column 6 - initial latitude  (must be between minimum and maximum
c ---                                 latitudes defined in regional.grid.b)
c ---   column 7 - initial depth (or reference sigma for isopycnic floats)
c
        read(uoff+99,*) iflnum(nfl),ifltyp(nfl),flt(nfl,8),flt(nfl,9),
     &                  flt(nfl,1),flt(nfl,2),flt(nfl,3)
        if (ifltyp(nfl).eq.1 .or. ifltyp(nfl).eq.4) then
          wvelfl=.true.
        endif
        if (ifltyp(nfl).ne.4) then
          hadvfl=.true.
c
c --- since this float is to be advected, stop if the advection time interval
c --- is not between 2 and 4 hr
          if (nfladv.lt.0) then
            if (mnproc.eq.1) then
            write(lp,'(/ a /)')
     &        'error - set nfladv to advect floats every 2-4 hr'
            call flush(lp)
            endif !1st tile
            call xcstop('(floats_init)')
                   stop '(floats_init)'
          endif
c
        endif
c
        flt(nfl,4)=0.0
        flt(nfl,5)=0.0
        flt(nfl,6)=0.0
        flt(nfl,7)=0.0
        if(ifltyp(nfl).eq.2) then
          flt(nfl,7)=flt(nfl,3)
          flt(nfl,3)=0.0
        endif
        flt(nfl,10)=0.0
        flt(nfl,11)=0.0
        flt(nfl,12)=0.0
        flt(nfl,13)=0.0
c
        kfloat(nfl)=-1
c
      enddo
c
      nfladv=abs(nfladv)
c
      close(unit=uoff+99) !file='float.input'
c
      if (mnproc.eq.1) then
      write(lp,955) nflt
      write(lp,956) nfladv
      write(lp,957) nflsam
      write(lp,958) min(nflt,10)
  955 format(' read initial data for',i6,' floats, time step',i9)
  956 format(' float advected every',i7,' baroclinic time steps')
  957 format(' float sampled every',i8,' baroclinic time steps'/)
  958 format(' input data for first',i3,' floats')
      do nfl=1,min(nflt,10)
        write(lp,959) nfl,(flt(nfl,i),i=1,9)
  959   format(3x,i2,1p,5e13.5/5x,4e13.5/)
      enddo
      call flush(lp)
      endif !1st tile
c
c ---------------------------------------------------------------------------
c --- allocate arrays for floats
c ---------------------------------------------------------------------------
c
      allocate( wvelup(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm),
     &          wveldn(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm),
     &          dpdxup(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy), 
     &          dpdyup(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          dpdxdn(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          dpdydn(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      allocate( wveli( 1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,  kdm+1),
     &          uold2( 1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2*kdm),
     &          vold2( 1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2*kdm),
     &          wold2u(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2*kdm),
     &          wold2d(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2*kdm),
     &          dlondx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          dlondy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          dlatdx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          dlatdy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( 11*(idm+2*nbdy)*(jdm+2*nbdy)*kdm )
      call mem_stat_add(  9*(idm+2*nbdy)*(jdm+2*nbdy) )
c
      if (synflt) then
!$OMP   PARALLEL DO PRIVATE(j,i,k)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1-nbdy,jj+nbdy
          do i=1-nbdy,ii+nbdy
            do k=1,kk
              uold2( i,j,k   )=hugel
              uold2( i,j,k+kk)=hugel
              vold2( i,j,k   )=hugel
              vold2( i,j,k+kk)=hugel
              wold2u(i,j,k   )=hugel
              wold2u(i,j,k+kk)=hugel
              wold2d(i,j,k   )=hugel
              wold2d(i,j,k+kk)=hugel
              wveli( i,j,k)   =hugel
            enddo !k
            wveli(i,j,kk+1)=hugel
          enddo
        enddo
!$OMP   END PARALLEL DO
      endif !synflt
c
c ---------------------------------------------------------------------------
c --- initialize old horizontal velocities for runga-kutta time interpolation
c --- calculate dlondx, dlondy, dlatdx, dlatdy required to convert u, v to
c --- longitude/time and latitude/time for float advection
c ---------------------------------------------------------------------------
c
      margin = nbdy - 1
c
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            do k=1,kk
              uold2(i,j,k   )=u(i,j,k,n)+ubavg(i,j,n)
              uold2(i,j,k+kk)=u(i,j,k,n)+ubavg(i,j,n)
#if defined(STOKES)                
              uold2(i,j,k   )=uold2(i,j,k   )+usd(i,j,k)
              uold2(i,j,k+kk)=uold2(i,j,k+kk)+usd(i,j,k)
#endif              
            enddo !k
            dlondx(i,j)=(plon(i  ,j)*scpy(i  ,j)- 
     &                   plon(i-1,j)*scpy(i-1,j))/scu2(i,j)
            dlatdx(i,j)=(plat(i  ,j)*scpy(i  ,j)-
     &                   plat(i-1,j)*scpy(i-1,j))/scu2(i,j)
            if (plat(i,j)-plat(i-1,j).ne.0.0) then
              nonlatlon=.true.
            endif
          endif !iu
          if (SEA_V) then
            do k=1,kk
              vold2(i,j,k   )=v(i,j,k,n)+vbavg(i,j,n)
              vold2(i,j,k+kk)=v(i,j,k,n)+vbavg(i,j,n)
#if defined(STOKES)                
              vold2(i,j,k   )=vold2(i,j,k   )+vsd(i,j,k)
              vold2(i,j,k+kk)=vold2(i,j,k+kk)+vsd(i,j,k)
#endif                  
            enddo !k
            dlondy(i,j)=(plon(i,j  )*scpx(i,j  )-
     &                   plon(i,j-1)*scpx(i,j-1))/scv2(i,j)
            dlatdy(i,j)=(plat(i,j  )*scpx(i,j  )-
     &                   plat(i,j-1)*scpx(i,j-1))/scv2(i,j)
            if (plon(i,j)-plon(i,j-1).ne.0.0) then
              nonlatlon=.true.
            endif
          endif !iv
          if (SEA_P) then
            do k=1,kk
              wold2u(i,j,k   )=0.0
              wold2d(i,j,k   )=0.0
              wold2u(i,j,k+kk)=0.0
              wold2d(i,j,k+kk)=0.0
              wveli (i,j,k   )=0.0
            enddo !k
            wveli(i,j,kk+1)=0.0
          endif !ip
        enddo !i
      enddo !j
c
c ----------------------------------------
c --- turbulent horizontal velocity option
c ----------------------------------------
c
c --- initialize turbulence constants
c
c --- tdecri is in inverse days and dtturb is the turbulent time step in days
c
c --- dtturb is set to the runga-kutta interpolation time step, which must
c --- be within 1 and 2 hours
c
      if (turbvel) then
        dtturb=deltfl/86400.0
        uturb0=sqrt(2.*tbvar*tdecri*dtturb)
      endif
c
      return
      end subroutine floats_init

      subroutine floats_restart
      implicit none
c
c -----------------------------
c --- output float restart file
c -----------------------------
c
      integer i,k,l
c
      if     (mnproc.eq.1) then
c
c ---   first remove floats that have run aground or left the domain
        i=0
        do l=1,nflt
          if (flt(l,1).gt.-999.0) then
            i=i+1
          endif
        enddo !1
c
        write(lp,*) 'writing new float input file for restart'
        call flush(lp)
c
        open(unit=uoff+802,file=flnmfltio,
     &                     form='formatted',status='new')
        write(uoff+802,970) i
        write(uoff+802,970) nstepfl
        do l=1,nflt
          if (flt(l,1).gt.-999.0) then
            if (ifltyp(l).ne.2) then
            write(uoff+802,971) iflnum(l),ifltyp(l),flt(l,8),flt(l,9),
     &                          (flt(l,k),k=1,3)
            else
            write(uoff+802,971) iflnum(l),ifltyp(l),flt(l,8),flt(l,9),
     &                          (flt(l,k),k=1,2),flt(l,7)
            endif
          endif
        enddo !l
        close(uoff+802)
      endif !1st tile

  970 format(i9)
  971 format(i6,i2,2f9.2,3f15.9)

      return
      end subroutine floats_restart

      subroutine floats(m,n,timefl,ioflag)
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none 
c
      integer, parameter :: nfl_debug = -1  !no debugging
c     integer, parameter :: nfl_debug =  6  !debug float nfl_debug
c
      integer m,n,ioflag
      real    timefl
c
c --- local variables
      integer i,j,k,l,margin
      real    tflt,sflt,thlo,thhi,thflt,
     &        uflt1,uflt2,uflt3,uflt4,uflt,
     &        vflt1,vflt2,vflt3,vflt4,vflt,
     &        wflt1,wflt2,wflt3,wflt4,wflt,
     &        vkflt,tkflt,skflt,xpos0,xpos1,xpos2,xpos3,
     &        ypos0,ypos1,ypos2,ypos3,depflt,plo,phi,q,
     &        depiso,wvhi,wvlo,uvfctr,qisop,ufltm,vfltm,
     &        alomin,alomax,alamin,alamax
      real    uturb,vturb,uturb1,vturb1,dlodx,dlody,dladx,dlady
      real*4  rtab(200,2)
      integer ifltll(3),jfltll(3),ngood(3)
      integer kflt,k1,k2,k3,ier,ngrid,kold1w,kold2w,kold1,kold2,
     &        ntermn,i1,j1,ngoodi
      integer ied,iede,ifladv
c
      integer*4  seed,numran,inisee,iflag
      integer*4  iseed1,iseed2
      equivalence ( iseed1, iseed2, seed ) 
c
c --- local velocity component storage for synthetic moorings
      real fltloc(nflt,3)
c
c --- 2-d local arrays used for the 16-point box horizontal interpolation
      real    varb2d(4,4),ptlon(4,4,3),ptlat(4,4,3)
      logical maskpt(4,4,3),maskpi(4,4)
c
c --- arrays required for parallelization
      real    flt1(12*nflt),flt3(17*nflt)
      integer iproc
c
      real   timer1, timer2, totime
      real*8  wtime
c
      include 'stmt_fns.h'
c
c -------------------------------------------------------------------------
c --- floats.f (hycom version 2.2)
c -------------------------------------------------------------------------
c
c --- synthetic floats, drifters, and mooring instruments
c
c --- optionally samples time series of dynamical/thermodynamical variables
c --- at the location of each float
c
c --- four float types are presently supported:
c ---    3-d lagrangian (vertically advected by diagnosed vertical velocity)
c ---    isopycnic      (remains on specified density surface)
c ---    isobaric       (remains at the pressure depth where it was released)
c ---    stationary     (synthetic instrument/mooring)
c
c --- horizontal advection, along with vertical advection of lagrangian floats,
c --- is performed using the MICOM runga-kutta-4 time interpolation algorithm
c --- developed by Zulema Garraffo
c
c --- works on any curvilinear grid
c
c --- float position is stored as longitude and latitude
c
c --- to horizontal advect the float, u and v are first converted to
c --- d(longitude)/dt and d(latitude)/dt as follows:
c
c ---   u_lon = u*dlondx + v*dlondy
c ---   v_lat = u*dlatdx + v*dlatdy
c
c --- to horizontally advect the float, terms u*dlondx and u*dlatdx are
c --- calculated on u grid points while terms v*dlondy and v*dlatdy are
c --- calculated on v grid ponts. these terms are then interpolated to
c --- the float locations from the surrounding u and v grid points,
c --- respectively
c
c --- since the terms v*dlondy and u*dlatdx are always zero when the model
c --- x,y axes are everywhere lines of constant latitude and longitude,
c --- respectively, logical variable 'nonlatlon' prevents horizontal
c --- interpolation of these terms in this case
c
c --- horizontal interpolation to the float location follows the MICOM
c --- procedure of Zulema Garraffo - second-order interpolation from the 16
c --- surrounding grid points is performed unless fewer than nptmin (presently
c --- set to 10 in subroutine intrph) water points are available, in which
c --- case nearest-neighbor interpolation is used.
c
c --- variables are horizontally interpolated from their native grid (p, u,
c --- or v)
c
c --- adapted for hycom by george halliwell
c --- code parallelized by remy baraille
c
c --- the second-order interpolation subroutine included here is the one
c --- included in the mariano and brown parameter matrix objective analysis
c --- algorithm to estimate the large scale trend surface - it is different
c --- from the algorithm used by Garraffo in MICOM
c
c --- float initialization is performed in floats_init - information
c --- about the input file containing initial float information is
c --- presented in that subroutine.
c
c --- variables in array flt(nfl,n) are:
c
c ---   n = 1  longitude
c ---   n = 2  latitude
c ---   n = 3  float depth 
c ---   n = 4  water depth
c ---   n = 5  temperature
c ---   n = 6  salinity
c ---   n = 7  water density
c ---   n = 8  float start time (in days from start of model run)
c ---   n = 9  float end   time (in days from start of model run)
c
c --- time series of several variables are interpolated to the location of each
c --- float and saved every nflsam time steps when ioflag is set to 1
c
c --- variables output onto file float.out for each float are:
c
c ---   1. initial sequential float number
c ---   2. time (in days from start of model run)
c ---   3. model layer number that contains the float
c ---   4. longitude   (u for synthetic moorings)
c ---   5. latitude    (v for synthetic moorings)
c ---   6. float depth (w for synthetic moorings)
c ---   7. water depth
c ---   8. temperature
c ---   9. salinity
c ---  10. water density (remains constant for isopycnic floats)
c
      call xctilr(dp(     1-nbdy,1-nbdy,1,n),1,kk, 6,6, halo_ps)
      call xctilr(dpu(    1-nbdy,1-nbdy,1,n),1,kk, 6,6, halo_vs)
      call xctilr(dpv(    1-nbdy,1-nbdy,1,n),1,kk, 6,6, halo_vs)
      call xctilr(u(      1-nbdy,1-nbdy,1,n),1,kk, 6,6, halo_uv)
      call xctilr(v(      1-nbdy,1-nbdy,1,n),1,kk, 6,6, halo_vv)
      call xctilr(ubavg(  1-nbdy,1-nbdy,  n),1, 1, 6,6, halo_uv)
      call xctilr(vbavg(  1-nbdy,1-nbdy,  n),1, 1, 6,6, halo_vv)
      call xctilr(temp(   1-nbdy,1-nbdy,1,n),1,kk, 6,6, halo_ps)
      call xctilr(saln(   1-nbdy,1-nbdy,1,n),1,kk, 6,6, halo_ps)
      call xctilr(th3d(   1-nbdy,1-nbdy,1,n),1,kk, 6,6, halo_ps)
c
c --- calculate vertical velocity at interfaces as the sum of the
c --- vertical interface velocity estimated in cnuity.f and the
c --- advective component due to flow parallel to interfaces.
c --- wvelup and wveldn represent velocity at the top and bottom of
c --- layer k
c
      margin = 6
c
c --- set pressure array at p points
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
            do k=1,kk
              k1=k+1
              p(i,j,k+1)=p(i,j,k)+dp(i,j,k,n)
            enddo !k
          endif !ip
        enddo !i
      enddo !j
c
c --- set pressure array at u and v points
c --- calculate dpdx, dpdy at interfaces above and below layer k for
c --- estimating wvel within layer k
c
      do k=1,kk
c
        margin = 5
c
        do j=1-margin,jj+margin
          do i=1-margin,ii+margin
            if (SEA_U) then
              pu(i,j,k+1)=pu(i,j,k)+dpu(i,j,k,n)
              if (wvelfl) then
                dpdxup(i,j)=
     &            (p(i,j,k  )*scpy(i,j)-p(i-1,j,k  )*scpy(i-1,j))
     &            /scu2(i,j)
                dpdxdn(i,j)=
     &            (p(i,j,k+1)*scpy(i,j)-p(i-1,j,k+1)*scpy(i-1,j))
     &            /scu2(i,j)
              endif
            endif !iu
            if (SEA_V) then
              pv(i,j,k+1)=pv(i,j,k)+dpv(i,j,k,n)
              if (wvelfl) then
                dpdyup(i,j)=
     &            (p(i,j,k  )*scpx(i,j)-p(i,j-1,k  )*scpx(i,j-1))
     &            /scv2(i,j)
                dpdydn(i,j)=
     &            (p(i,j,k+1)*scpx(i,j)-p(i,j-1,k+1)*scpx(i,j-1))
     &            /scv2(i,j)
              endif
            endif !iv
          enddo !i
        enddo !j
c
c --- calculate the vertical velocity arrays
c
        margin = 4
c
        do j=1-margin,jj+margin
          do i=1-margin,ii+margin
            if (SEA_P) then
              if (wvelfl) then
                wveli(i,j,k+1)=deltfl*wveli(i,j,k+1)/delt1
                if (dp(i,j,k,n).gt.tencm) then
                    wvelup(i,j,k)=-0.5*deltfl*
#if defined(STOKES)             
     &         ((u(i  ,j  ,k,n)+usd(i  ,j  ,k)+
     &           ubavg(i  ,j  ,n))*dpdxup(i  ,j  )+
     &          (u(i+1,j  ,k,n)+usd(i+1,j  ,k)+
     &           ubavg(i+1,j  ,n))*dpdxup(i+1,j  )+
     &          (v(i  ,j  ,k,n)+vsd(i  ,j  ,k)+
     &           vbavg(i  ,j  ,n))*dpdyup(i  ,j  )+
     &          (v(i  ,j+1,k,n)+vsd(i  ,j+1,k)+
     &           vbavg(i  ,j+1,n))*dpdyup(i  ,j+1))
#else             
     &            ((u(i  ,j  ,k,n)+ubavg(i  ,j  ,n))*dpdxup(i  ,j  )+
     &             (u(i+1,j  ,k,n)+ ubavg(i+1,j  ,n))*dpdxup(i+1,j  )+
     &             (v(i  ,j  ,k,n)+vbavg(i  ,j  ,n) )*dpdyup(i  ,j  )+
     &             (v(i  ,j+1,k,n)+vbavg(i  ,j+1,n))*dpdyup(i  ,j+1))
#endif
     &             /onem+wveli(i,j,k  )
                  wveldn(i,j,k)=-0.5*deltfl*
#if defined(STOKES)
     &      ((u(i  ,j  ,k,n)+usd(i  ,j  ,k)+
     &        ubavg(i  ,j  ,n))*dpdxdn(i  ,j  )+
     &       (u(i+1,j  ,k,n)+usd(i+1,j  ,k)+
     &        ubavg(i+1,j,n))*dpdxdn(i+1,j  )+
     &       (v(i  ,j  ,k,n)+vsd(i  ,j  ,k)+
     &        vbavg(i  ,j  ,n))*dpdydn(i  ,j  )+
     &       (v(i  ,j+1,k,n)+vsd(i  ,j+1,k)+
     &        vbavg(i  ,j+1,n))*dpdydn(i  ,j+1))
#else 
     &            ((u(i  ,j  ,k,n)+ubavg(i  ,j  ,n))*dpdxdn(i  ,j  )+
     &             (u(i+1,j  ,k,n)+ubavg(i+1,j  ,n))*dpdxdn(i+1,j  )+
     &             (v(i  ,j  ,k,n)+vbavg(i  ,j  ,n))*dpdydn(i  ,j  )+
     &             (v(i  ,j+1,k,n)+vbavg(i  ,j+1,n))*dpdydn(i  ,j+1))
#endif
     &             /onem+wveli(i,j,k+1)
                else
                  wvelup(i,j,k)=0.0
                  wveldn(i,j,k)=0.0
                endif
              endif
            endif !ip
          enddo !i
        enddo !j
c
      enddo !k
c
*     call xctilr(wvelup(1-nbdy,1-nbdy,1),1, kk, 4,4, halo_ps)
*     call xctilr(wveldn(1-nbdy,1-nbdy,1),1, kk, 4,4, halo_ps)
c
c --- set old velocity indices used for interpolation of float position
c --- perform full float update every two times this subroutine is called;
c --- at the intermediate times, just store old velocity fields for the next
c --- call
c
      if (mod(nstepfl,nfladv).eq.nfldta) then
        kold1 =kk
        kold1w=kk+1
        kold2 =0
        kold2w=0
        go to 10
      else
        kold1 =0
        kold1w=0
        kold2 =kk
        kold2w=kk+1
      endif
c
c --- turbulent horizontal velocity option
c
c --- to customize the parameter settings in blkdat.input that control
c --- the calculated turbulent velocity (tbvar, tdecri), refer to
c --- Griffa (1996), "Aplications of stochastic particles models to
c --- oceanographic problems" in "Stochatic Modelling in Physical
c --- Oceanography", pp. 114-140, Adler, Muller, and Rozovoskii, editors
c
      if (turbvel) then
c
c --- initialize random seeds and create random number table
c --- if iflag=1 (iflag=0), then time() is (is not) used to generate seeds
c
        iflag=1
        call system_clock(inisee)
        seed = 414957000-inisee
        iede = iseed1
        ied  = iseed2
        if (iede .lt. 0) iede = abs(iede)
        if (ied  .lt. 0) ied  = abs(ied)
c
        call rantab_ini(rtab,iseed1,iseed2,iflag)
      endif
c
c --- get particle locations from processor 1
      if     (mnproc.eq.1) then
        do nfl=1,nflt
          do i=1,9
            flt1( i+12*(nfl-1)) = flt(nfl,i)
          enddo
          flt1(10+12*(nfl-1)) = kfloat(nfl)
          flt1(11+12*(nfl-1)) = iflnum(nfl)
          flt1(12+12*(nfl-1)) = ifltyp(nfl)
        enddo
      endif !1st tile
      call xcastr(flt1(1:12*nflt), 1)
      if     (mnproc.ne.1) then
        do nfl=1,nflt
          do i=1,9
            flt(nfl,i) = flt1( i+12*(nfl-1))
          enddo
          kfloat(nfl) = flt1(10+12*(nfl-1))
          iflnum(nfl) = flt1(11+12*(nfl-1))
          ifltyp(nfl) = flt1(12+12*(nfl-1))
        enddo
      endif !1st tile
c
c ----------------------
c --- begin float loop
c ----------------------
c
      do nfl=1,nflt
c
c --- skip if float has previously run aground or exceeded termination time
        if(flt(nfl,1).le.-999.) then
          go to 22
        endif
c
c --- ier    = error flag for horizontal interpolation
c --- ntermn = float termination flag
c ---             -10 => reached termination time
c ---              -5 => exited domain
c ---              >0 => ran aground
c --- ifladv = float horizontal advection flag
        ier=0
        ntermn=0
        ifladv=1
c
c --- suppress float advection if this is the first time step so that
c --- the initial position on the output file is exactly the position
c --- specified on the input file
        if (nstepfl.eq.1) then
          ifladv=0
        endif
c
c --- check if model time has reached float deployment time
c --- when it does, again suppress float advection during first pass
        if (timefl-flt(nfl,8).lt.-0.001) then
          go to 22
        endif
        if (kfloat(nfl).lt.0) then
          ifladv=0
        endif
c
c --- check if model time has reached float termination time
        if (       flt(nfl,9).gt.0.0 .and.
     &      timefl-flt(nfl,9).gt.0.001    ) then
          ntermn=-10
        endif
c
c ---------------------------------------------------------------------------
c --- search for the processor controlling the tile containing the grid point
c --- search for the surrounding 16 grid points on the p, u, and v grids
c ---------------------------------------------------------------------------
c
c --- processeur sur lequel va se derouler le calcul
c
        iproc=0
c
        margin = 0
c
        do j=1-margin,jj+margin
          do i=1-margin,ii+margin
            if (SEA_P) then
c
c --- search for the surrounding p-grid points
              if (i+i0.lt.itdm .and. j+j0.lt.jtdm) then
                alomin=min(plon(i  ,j),plon(i  ,j+1),
     &                     plon(i+1,j),plon(i+1,j+1))
                alomax=max(plon(i  ,j),plon(i  ,j+1),
     &                     plon(i+1,j),plon(i+1,j+1))
                alamin=min(plat(i  ,j),plat(i  ,j+1),
     &                     plat(i+1,j),plat(i+1,j+1))
                alamax=max(plat(i  ,j),plat(i  ,j+1),
     &                     plat(i+1,j),plat(i+1,j+1))
                if (flt(nfl,1).ge.alomin.and.flt(nfl,1).lt.alomax.and.
     &              flt(nfl,2).ge.alamin.and.flt(nfl,2).lt.alamax) then
                  ifltll(1)=i
                  jfltll(1)=j
c
c --- determine if float has exited domain
                  if (i+i0.lt.2 .or. i+i0.gt.itdm-1 .or.
     &                j+j0.lt.2 .or. j+j0.gt.jtdm-1) then
                    ntermn=-5
                  endif
c
c --- search for the surrounding u-grid points
                  do i1=i-1,i+1
                    do j1=j-1,j+1
                      if (i1+i0.lt.itdm .and. j1+j0.lt.jtdm) then
                        alomin=min(ulon(i1  ,j1),ulon(i1  ,j1+1),
     &                             ulon(i1+1,j1),ulon(i1+1,j1+1))
                        alomax=max(ulon(i1  ,j1),ulon(i1  ,j1+1),
     &                             ulon(i1+1,j1),ulon(i1+1,j1+1))
                        alamin=min(ulat(i1  ,j1),ulat(i1  ,j1+1),
     &                             ulat(i1+1,j1),ulat(i1+1,j1+1))
                        alamax=max(ulat(i1  ,j1),ulat(i1  ,j1+1),
     &                            ulat(i1+1,j1),ulat(i1+1,j1+1))
                        if (flt(nfl,1).ge.alomin .and.
     &                      flt(nfl,1).lt.alomax .and.
     &                      flt(nfl,2).ge.alamin .and.
     &                      flt(nfl,2).lt.alamax) then  
                          ifltll(2)=i1
                          jfltll(2)=j1
c
c --- determine if float has exited domain
                          if (i1+i0.lt.2 .or. i1+i0.gt.itdm-1 .or.
     &                       j1+j0.lt.2 .or. j1+j0.gt.jtdm-1) then
                            ntermn=-5
                          endif
                          go to 11
                        endif
                      endif
                    enddo !j1
                  enddo !i1
 11               continue
c
c --- search for the surrounding v-grid points
                  do i1=i-1,i+1
                    do j1=j-1,j+1
                      if (i1+i0.lt.itdm .and. j1+j0.lt.jtdm) then
                        alomin=min(vlon(i1  ,j1),vlon(i1  ,j1+1),
     &                             vlon(i1+1,j1),vlon(i1+1,j1+1))
                        alomax=max(vlon(i1  ,j1),vlon(i1  ,j1+1),
     &                             vlon(i1+1,j1),vlon(i1+1,j1+1))
                        alamin=min(vlat(i1  ,j1),vlat(i1  ,j1+1),
     &                             vlat(i1+1,j1),vlat(i1+1,j1+1))
                        alamax=max(vlat(i1  ,j1),vlat(i1  ,j1+1),
     &                             vlat(i1+1,j1),vlat(i1+1,j1+1))
                        if (flt(nfl,1).ge.alomin .and.
     &                      flt(nfl,1).lt.alomax .and.
     &                      flt(nfl,2).ge.alamin .and.
     &                      flt(nfl,2).lt.alamax) then      
                          ifltll(3)=i1
                          jfltll(3)=j1
c
c --- determine if float has exited domain
                          if (i1+i0.lt.2 .or. i1+i0.gt.itdm-1 .or.
     &                        j1+j0.lt.2 .or. j1+j0.gt.jtdm-1) then
                            ntermn=-5
                          endif
                          go to 12
                        endif
                      endif
                    enddo !j1
                  enddo !i1
 12               continue
c
c --- set processor number for the tile containing the float and exit grid
c --- point search
                  iproc=mnproc
                  go to 13
                endif
              endif
            endif !ip
          enddo !i
        enddo !j
c
 13     continue
c
c --- float nfl is now updated by the processor running the tile containing
c --- the float
c
        if (iproc.gt.0) then
c
          if     (nfl.eq.nfl_debug) then
            write(lp,100) nfl,nflt,nstep,nstepfl
            write(lp,101) nfl,ntermn,flt(nfl,1),flt(nfl,2),
     &                   (ifltll(i)+i0,jfltll(i)+j0,i=1,3),
     &                    mnproc,
     &                    plon(ifltll(1),jfltll(1)),
     &                    plat(ifltll(1),jfltll(1))
 100        format(/'diagnostics for float',i6,' of',i6,', time step',
     &              i9/'float time step',i9)
 101        format('float',i6,' ntermn:',i6,' position:',2(1pe12.4)/
     &             'lower left points, p,u,v:',3(2i5,2x)/
     &             'mnproc =',i4,'  plon,plat =',1pe12.4,1pe12.4)
            call flush(lp)
          endif !nfl_debug
c
c --- if float has exited domain or run aground as determined previously,
c --- jump ahead to the float termination code bloci
          if (ntermn.eq.-5 .or. ntermn.eq.1) then
            go to 30
          endif
c
c --- set ptlon, ptlat for all horizontal interpolations from each grid
          ngrid=1
          do i1=1,4
            i=i1+ifltll(ngrid)-2
            do j1=1,4
              j=j1+jfltll(ngrid)-2
              ptlon(i1,j1,ngrid)=plon(i,j)
              ptlat(i1,j1,ngrid)=plat(i,j)
            enddo
          enddo
c
          ngrid=2
          do i1=1,4
            i=i1+ifltll(ngrid)-2
            do j1=1,4
              j=j1+jfltll(ngrid)-2
              ptlon(i1,j1,ngrid)=ulon(i,j)
              ptlat(i1,j1,ngrid)=ulat(i,j)
            enddo
          enddo
c
          ngrid=3
          do i1=1,4
            i=i1+ifltll(ngrid)-2
            do j1=1,4
              j=j1+jfltll(ngrid)-2
              ptlon(i1,j1,ngrid)=vlon(i,j)
              ptlat(i1,j1,ngrid)=vlat(i,j)
            enddo
          enddo
c
c --- the float is assumed to remain within the same layer that it was in
c --- during the previous update unless it is the first advection time step
c --- for the float, in which case it is initially assumed to be in layer 1
c
          k=max(1,kfloat(nfl))
c
c --- mask p grid points that are on land or where the layer containing the
c --- float is a zero or nearly-zero thickness layer at the bottom
          ngrid=1
          ngood(ngrid)=0
          do i1=1,4
            i=i1+ifltll(ngrid)-2
            do j1=1,4
              j=j1+jfltll(ngrid)-2
              if (depths(i,j).lt.0.01 .or. depths(i,j).eq.hugel .or.
     &            depths(i,j)*onem-p(i,j,k).lt.tencm) then
                maskpt(i1,j1,ngrid)=.true.
              else
                maskpt(i1,j1,ngrid)=.false.
                ngood(ngrid)=ngood(ngrid)+1
              endif
            enddo
          enddo
c
          if     (nfl.eq.nfl_debug) then
            write(lp,102) ngood(1)
 102        format('initial masking: no. of good p points',i4)
          endif !nfl_debug
c
c --------------------------------------------------
c --- determine the model layer containing the float
c --------------------------------------------------
c
c --- if this is not the first advection time step for the float, determine
c --- if the float has transited to another model layer
c
          if (kfloat(nfl).ge.1) then
c
c --- check if the float is deeper than the interpolated bottom
c --- depth - if so, reset the depth to 0.1 m above the bottom - this
c --- prevents from running aground too frequently in coastal regions
            if (ifltyp(nfl).ne.4) then
              ngrid=1
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=p(i,j,kk+1)
                enddo
              enddo
              call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                    flt(nfl,1),flt(nfl,2),maskpt(1,1,ngrid),
     &                    ngood(ngrid),ngrid,intpfl,radian,plo,ier)
              flt(nfl,3)=max(fldepm,min(flt(nfl,3),(plo-tencm)/onem))
            endif
c
c --- has the float remained in the same layer?
            ngrid=1
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                varb2d(i1,j1)=p(i,j,k)
              enddo
            enddo
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  flt(nfl,1),flt(nfl,2),maskpt(1,1,ngrid),
     &                  ngood(ngrid),ngrid,intpfl,radian,phi,ier)
c
c --- terminate float because it has run aground
            if (ier.eq.999) then
              ntermn=2
              go to 30
            endif
c
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                varb2d(i1,j1)=p(i,j,k+1)
              enddo
            enddo
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  flt(nfl,1),flt(nfl,2),maskpt(1,1,ngrid),
     &                  ngood(ngrid),ngrid,intpfl,radian,plo,ier)
            phi=phi/onem
            plo=plo/onem
c
c --- if the float is in the same model layer, skip ahead
            if (flt(nfl,3).gt.phi .and. flt(nfl,3).le.plo) then
              kflt=kfloat(nfl)
              go to 40
            endif
c
c --- determine the new model layer containing the float
c
c --- did the float move up one layer?
            if (flt(nfl,3).le.phi .and. kfloat(nfl).gt.1) then
              k=kfloat(nfl)-1
              plo=phi
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=p(i,j,k)
                enddo
              enddo
              call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                    flt(nfl,1),flt(nfl,2),maskpt(1,1,ngrid),
     &                    ngood(ngrid),ngrid,intpfl,radian,phi,ier)
              phi=phi/onem
              if (flt(nfl,3).gt.phi .and. flt(nfl,3).le.plo) then
                kflt=k
                go to 40
              endif
            endif
c
c --- did the float move down one layer?
            if (flt(nfl,3).gt.plo .and. kfloat(nfl).lt.kk) then
              k=kfloat(nfl)+1
              phi=plo
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=p(i,j,k)
                enddo
              enddo
              call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                    flt(nfl,1),flt(nfl,2),maskpt(1,1,ngrid),
     &                    ngood(ngrid),ngrid,intpfl,radian,plo,ier)
              plo=plo/onem
              if (flt(nfl,3).gt.phi .and. flt(nfl,3).le.plo) then
                kflt=k
                go to 40
              endif
            endif
c
          endif
c
c --- if the float did not move up or down one layer, or if this is the
c --- first time step after float initialization, search for the layer
c --- containg the float from top to bottom
          ngrid=1
          plo=0.
          k2=0
          k3=0
          do k=1,kk
            k1=min(k+3,kk+1)
c
c --- if the depth of interface k1 is not deeper than the float at any
c --- surrounding grid points, skip to end of k-loop
            if (k2.eq.0) then
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  if (.not.maskpt(i1,j1,ngrid) .and.
     &                p(i,j,k1)/onem.gt.flt(nfl,3)) then
                    k2=1
                  endif
                enddo
              enddo
            endif
            if (k2.eq.0) then
              go to 42
            endif
c
c --- mask points where the layer being tested rests on the bottom
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                if (k2.eq.1) then
                  maskpi(i1,j1)=maskpt(i1,j1,ngrid)
                endif
                if (.not.maskpi(i1,j1) .and.
     &              depths(i,j)*onem-p(i,j,k).lt.tencm) then
                  maskpi(i1,j1)=.true.
                  ngood(ngrid)=ngood(ngrid)-1
                endif
              enddo
            enddo
            k2=2
c
c --- if too few good points remain, float has run aground
            if (ngood(ngrid).le.2) then
              ntermn=3
              go to 30
            endif
c
c --- interpolate to find plo
            phi=plo
            ngrid=1
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                varb2d(i1,j1)=p(i,j,k+1)
              enddo
            enddo
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  flt(nfl,1),flt(nfl,2),maskpt(1,1,ngrid),
     &                  ngood(ngrid),ngrid,intpfl,radian,plo,ier)
            plo=plo/onem
            if(plo-phi.gt.0.1) then
              k3=k
            else
              kflt=k3
              do i1=1,4
                do j1=1,4
                  maskpt(i1,j1,ngrid)=maskpi(i1,j1)
                enddo
              enddo
              go to 40
            endif
            if (flt(nfl,3).lt.plo) then
              kflt=k
              do i1=1,4
                do j1=1,4
                  maskpt(i1,j1,ngrid)=maskpi(i1,j1)
                enddo
              enddo
              go to 40
            endif
 42         continue
          enddo
c
c --- if layer selection fails, assume float has run aground
          print *,'warning - selection of float layer failed'
          ntermn=4
          go to 30
c
c --- we now know the model layer containing the float
 40       kfloat(nfl)=kflt
c
c ------------------------------------------
c --- set grid masks for u and v grid points
c ------------------------------------------
c
          k=kflt
          ngrid=2
          ngood(ngrid)=0
          do i1=1,4
            i=i1+ifltll(ngrid)-2
            do j1=1,4
              j=j1+jfltll(ngrid)-2
              if (depthu(i,j).lt.onecm .or. depthu(i,j).eq.hugel .or.
     &            depthu(i,j)-pu(i,j,k).lt.tencm) then
                maskpt(i1,j1,ngrid)=.true.
              else
                maskpt(i1,j1,ngrid)=.false.
                ngood(ngrid)=ngood(ngrid)+1
              endif
            enddo
          enddo
c
          ngrid=3
          ngood(ngrid)=0
          do i1=1,4
            i=i1+ifltll(ngrid)-2
            do j1=1,4
              j=j1+jfltll(ngrid)-2
              if (depthv(i,j).lt.onecm .or. depthv(i,j).eq.hugel .or.
     &            depthv(i,j)-pv(i,j,k).lt.tencm) then
                maskpt(i1,j1,ngrid)=.true.
              else
                maskpt(i1,j1,ngrid)=.false.
                ngood(ngrid)=ngood(ngrid)+1
              endif
            enddo
          enddo
c
c --- if too few good p, u, v grid points, assume float has run aground
          if (ngood(1).le.2 .or. ngood(2).le.2 .or. ngood(3).le.2) then
            ntermn=5
            go to 30
          endif
c
          if     (nfl.eq.nfl_debug) then
            write(lp,103) nfl,k,phi,flt(nfl,3),plo,q
            write(lp,104) ngood
 103        format('nfl,k,phi,pflt,plo,q',i6,i3,1p,4e12.4)
 104        format('number of good points, p,u,v',3i4)
          endif !nfl_debug
c
c --- set vertical indices, and also set q for vertical interpolation
          k=kfloat(nfl)
          k1=max(1,k-1)
          k2=min(kk,k+1)
          q=(plo-flt(nfl,3))/max(0.001,plo-phi)
c
c ----------------------------------------------------------
c --- advect the float horizontally, then move it vertically
c --- do not move float if it is a mooring instrument
c ----------------------------------------------------------
c
c ----------------------------------
c --- horizontal advection of floats
c ----------------------------------
c
          if (ifltyp(nfl).ne.4) then
c
c --- interpolate u,v to float location - velocities at the new time and
c --- at two earlier times are used for the runga-kutta time interpolation
c
            xpos0=flt(nfl,1)
            ypos0=flt(nfl,2)
c
            ngrid=2
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                varb2d(i1,j1)=dlondx(i,j)*uold2(i,j,k+kold2)
              enddo
            enddo
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  xpos0,ypos0,maskpt(1,1,ngrid),
     &                  ngood(ngrid),ngrid,intpfl,radian,uflt1,ier)
c
c --- terminate float because it has run aground
            if (ier.eq.999) then
              ntermn=6
              go to 30
            endif
c
            if (nonlatlon) then
              ngrid=3
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=dlondy(i,j)*vold2(i,j,k+kold2)
                enddo
              enddo
              if (l.eq.1) then
                call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                      xpos0,ypos0,maskpt(1,1,ngrid),
     &                      ngood(ngrid),ngrid,intpfl,radian,ufltm,ier)
                uflt1=uflt1+ufltm
              endif
c
c --- terminate float because it has run aground
              if (ier.eq.999) then
                ntermn=7
                go to 30
              endif
            endif
c
            ngrid=3
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                varb2d(i1,j1)=dlatdy(i,j)*vold2(i,j,k+kold2)
              enddo
            enddo
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  xpos0,ypos0,maskpt(1,1,ngrid),
     &                  ngood(ngrid),ngrid,intpfl,radian,vflt1,ier)
c
c --- terminate float because it has run aground
            if (ier.eq.999) then
              ntermn=8
              go to 30
            endif
c
            if (nonlatlon) then
              ngrid=2
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=dlatdx(i,j)*uold2(i,j,k+kold2)
                enddo
              enddo
              call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                    xpos0,ypos0,maskpt(1,1,ngrid),
     &                    ngood(ngrid),ngrid,intpfl,radian,vfltm,ier)
              vflt1=vflt1+vfltm
c
c --- terminate float because it has run aground
              if (ier.eq.999) then
                ntermn=9
                go to 30
              endif
            endif
c
            xpos1=flt(nfl,1)+uflt1
            ypos1=flt(nfl,2)+vflt1
c
            ngrid=2
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                varb2d(i1,j1)=dlondx(i,j)*uold2(i,j,k+kold1)
              enddo
            enddo
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  xpos1,ypos1,maskpt(1,1,ngrid),
     &                  ngood(ngrid),ngrid,intpfl,radian,uflt2,ier)
c
            if (nonlatlon) then
              ngrid=3
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=dlondy(i,j)*vold2(i,j,k+kold1)
                enddo
              enddo
              call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                    xpos0,ypos0,maskpt(1,1,ngrid),
     &                    ngood(ngrid),ngrid,intpfl,radian,ufltm,ier)
              uflt2=uflt2+ufltm
            endif
c
            ngrid=3
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                varb2d(i1,j1)=dlatdy(i,j)*vold2(i,j,k+kold1)
              enddo
            enddo
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  xpos1,ypos1,maskpt(1,1,ngrid),
     &                  ngood(ngrid),ngrid,intpfl,radian,vflt2,ier)
c
            if (nonlatlon) then
              ngrid=2
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=dlatdx(i,j)*uold2(i,j,k+kold1)
                enddo
              enddo
             call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                   xpos0,ypos0,maskpt(1,1,ngrid),
     &                   ngood(ngrid),ngrid,intpfl,radian,vfltm,ier)
              vflt2=vflt2+vfltm
            endif
c
            xpos2=flt(nfl,1)+uflt2
            ypos2=flt(nfl,2)+vflt2
            ngrid=2
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                varb2d(i1,j1)=dlondx(i,j)*uold2(i,j,k+kold1)
              enddo
            enddo
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  xpos2,ypos2,maskpt(1,1,ngrid),
     &                  ngood(ngrid),ngrid,intpfl,radian,uflt3,ier)
c
            if (nonlatlon) then
              ngrid=3
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=dlondy(i,j)*vold2(i,j,k+kold1)
                enddo
              enddo
              call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                    xpos0,ypos0,maskpt(1,1,ngrid),
     &                    ngood(ngrid),ngrid,intpfl,radian,ufltm,ier)
              uflt3=uflt3+ufltm
            endif
c
            ngrid=3
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                varb2d(i1,j1)=dlatdy(i,j)*vold2(i,j,k+kold1)
              enddo
            enddo
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  xpos2,ypos2,maskpt(1,1,ngrid),
     &                  ngood(ngrid),ngrid,intpfl,radian,vflt3,ier)
c
            if (nonlatlon) then
              ngrid=2
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=dlatdx(i,j)*uold2(i,j,k+kold1)
                enddo
              enddo
             call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                   xpos0,ypos0,maskpt(1,1,ngrid),
     &                   ngood(ngrid),ngrid,intpfl,radian,vfltm,ier)
              vflt3=vflt3+vfltm
            endif
c
          endif
c
          if (ifltyp(nfl).ne.4) then
            xpos3=flt(nfl,1)+2.0*uflt3
            ypos3=flt(nfl,2)+2.0*vflt3
          else
            xpos3=flt(nfl,1)
            ypos3=flt(nfl,2)
          endif
c
          ngrid=2
          do i1=1,4
            i=i1+ifltll(ngrid)-2
            do j1=1,4
              j=j1+jfltll(ngrid)-2
              if (ifltyp(nfl).ne.4) then
#if defined(STOKES)
                 varb2d(i1,j1)=dlondx(i,j)*(u(i,j,k,n)+usd(i,j,k)+
     &              ubavg(i,j,n))
              else
      varb2d(i1,j1)=u(i,j,k,n)+usd(i,j,k)+ubavg(i,j,n)
#else
                varb2d(i1,j1)=dlondx(i,j)*(u(i,j,k,n)+ubavg(i,j,n))
              else
                varb2d(i1,j1)=u(i,j,k,n)+ubavg(i,j,n)
#endif                
                
              endif
            enddo
          enddo
         call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &               xpos3,ypos3,maskpt(1,1,ngrid),
     &               ngood(ngrid),ngrid,intpfl,radian,uflt4,ier)
c
          if (nonlatlon .and. ifltyp(nfl).ne.4) then
            ngrid=3
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                
#if defined(STOKES)
                varb2d(i1,j1)=dlondy(i,j)*(v(i,j,k,n)+vsd(i,j,k)+
     &                  vbavg(i,j,n))
#else                
                varb2d(i1,j1)=dlondy(i,j)*(v(i,j,k,n)+vbavg(i,j,n))
#endif                          
              enddo
            enddo
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  xpos0,ypos0,maskpt(1,1,ngrid),
     &                  ngood(ngrid),ngrid,intpfl,radian,ufltm,ier)
            uflt4=uflt4+ufltm
          endif
c
          ngrid=3
          do i1=1,4
            i=i1+ifltll(ngrid)-2
            do j1=1,4
              j=j1+jfltll(ngrid)-2
              if (ifltyp(nfl).ne.4) then
                  
#if defined(STOKES)
               varb2d(i1,j1)=dlatdy(i,j)*(v(i,j,k,n)+vsd(i,j,k)+
     &             vbavg(i,j,n))
              else
       varb2d(i1,j1)=v(i,j,k,n)+vsd(i,j,k)+vbavg(i,j,n)
#else
                varb2d(i1,j1)=dlatdy(i,j)*(v(i,j,k,n)+vbavg(i,j,n))
              else
                varb2d(i1,j1)=v(i,j,k,n)+vbavg(i,j,n)
#endif                      
              endif
            enddo
          enddo
          call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                xpos3,ypos3,maskpt(1,1,ngrid),
     &                ngood(ngrid),ngrid,intpfl,radian,vflt4,ier)
c
          if (nonlatlon .and. ifltyp(nfl).ne.4) then
            ngrid=2
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                
#if defined(STOKES)
                varb2d(i1,j1)=dlatdx(i,j)*(u(i,j,k,n)+usd(i,j,k)+
     &                   ubavg(i,j,n))
#else           
                varb2d(i1,j1)=dlatdx(i,j)*(u(i,j,k,n)+ubavg(i,j,n))
#endif                
                
              enddo
            enddo
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  xpos0,ypos0,maskpt(1,1,ngrid),
     &                  ngood(ngrid),ngrid,intpfl,radian,vfltm,ier)
            vflt4=vflt4+vfltm
          endif
c
          if (ifltyp(nfl).ne.4) then
            uflt=(uflt1+2.0*uflt2+2.0*uflt3+uflt4)/6.0
            vflt=(vflt1+2.0*vflt2+2.0*vflt3+vflt4)/6.0
c
c --- add turbulent velocity to u and v if requested
c
            if (turbvel) then
              call rantab(rtab,iseed1,iseed2,numran)
              uturb=uturb0*rtab(numran, 2)
              call rantab(rtab,iseed1,iseed2,numran)
              vturb=uturb0*rtab(numran, 2)
c
c --- convert uturb, vturb to d(longitude)/dt, d(latitude)/dt
c --- interpolate dlondx, dlondy, dlatdx, dlatdy to the float location
c
              ngrid=2
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=dlondx(i,j)
                enddo
              enddo
              call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                    xpos0,ypos0,maskpt(1,1,ngrid),
     &                    ngood(ngrid),ngrid,intpfl,radian,dlodx,ier)
              if (nonlatlon) then
                do i1=1,4
                  i=i1+ifltll(ngrid)-2
                  do j1=1,4
                    j=j1+jfltll(ngrid)-2
                    varb2d(i1,j1)=dlatdx(i,j)
                  enddo
                enddo
               call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                     xpos0,ypos0,maskpt(1,1,ngrid),
     &                     ngood(ngrid),ngrid,intpfl,radian,dladx,ier)
                ngrid=3
                do i1=1,4
                  i=i1+ifltll(ngrid)-2
                  do j1=1,4
                    j=j1+jfltll(ngrid)-2
                    varb2d(i1,j1)=dlondy(i,j)
                  enddo
                enddo
                call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                      xpos0,ypos0,maskpt(1,1,ngrid),
     &                      ngood(ngrid),ngrid,intpfl,radian,dlody,ier)
              else
                dladx=0.0
                dlody=0.0
              endif
              ngrid=3
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=dlatdy(i,j)
                enddo
              enddo
              call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                    xpos0,ypos0,maskpt(1,1,ngrid),
     &                    ngood(ngrid),ngrid,intpfl,radian,dlady,ier)
c
c --- add turbulent velocity to uflt, vflt
c
              uturb1=uturb*dlodx+vturb*dlody
              vturb1=uturb*dladx+vturb*dlady
c
      if     (nfl.eq.nfl_debug) then
        write(lp,105) nfl,uflt,uturb1,vflt,vturb1
 105    format('nfl,uflt,uturb1,vflt,vturb1',i5,1p,4e12.4)
      endif !nfl_debug
c
              uflt=(1.0-dtturb*tdecri)*uflt+uturb1
              vflt=(1.0-dtturb*tdecri)*vflt+vturb1
c
            endif
c
c --- update float horizontal position
c
            if (ifladv.eq.1) then
              flt(nfl,1)=flt(nfl,1)+2.0*deltfl*uflt
              flt(nfl,2)=flt(nfl,2)+2.0*deltfl*vflt
            endif
c
      if     (nfl.eq.nfl_debug) then
        write(lp,106) nfl,flt(nfl,1),xpos1,xpos2,xpos3,
     &                uflt1,uflt2,uflt3,uflt4,2.0*deltfl*uflt
        write(lp,107) nfl,flt(nfl,2),ypos1,ypos2,ypos3,
     &                vflt1,vflt2,vflt3,vflt4,2.0*deltfl*vflt
        write(lp,108) nfl,uflt,vflt,flt(nfl,1),flt(nfl,2)
 106    format('nfl,x-position',i6,1p,4e12.4/'    u-velocity',6x,5e12.4)
 107    format('nfl,y-position',i6,1p,4e12.4/'    v-velocity',6x,5e12.4)
 108    format('nfl,udeg,vdeg',i6,1p,2e13.5/'new position',2e13.5)
      endif !nfl_debug
c
          endif
c
c -----------------------------------------------
c --- vertical advection of 3-d lagrangian floats
c -----------------------------------------------
c
c --- horizontally interpolate diagnosed vertical velocity to the float
c --- location at the new time and at two earlier times to execute the
c --- runga-kutta time interpolation
c
          if (wvelfl .and. ifltyp(nfl).eq.1) then
c
            ngrid=1
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                wvhi=wold2u(i,j,k+kold2)
                wvlo=wold2d(i,j,k+kold2)
                varb2d(i1,j1)=q*wvhi+(1.0-q)*wvlo
              enddo
            enddo
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  xpos0,ypos0,maskpt(1,1,ngrid),
     &                  ngood(ngrid),ngrid,intpfl,radian,wflt1,ier)
c
            ngrid=1
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                wvhi=wold2u(i,j,k+kold1)
                wvlo=wold2d(i,j,k+kold1)
                varb2d(i1,j1)=q*wvhi+(1.0-q)*wvlo
              enddo
            enddo
           call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                 xpos1,ypos1,maskpt(1,1,ngrid),
     &                 ngood(ngrid),ngrid,intpfl,radian,wflt2,ier)
c
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  xpos2,ypos2,maskpt(1,1,ngrid),
     &                  ngood(ngrid),ngrid,intpfl,radian,wflt3,ier)
c
          endif
c
          if (wvelfl) then
            ngrid=1
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                wvhi=wvelup(i,j,k)
                wvlo=wveldn(i,j,k)
                varb2d(i1,j1)=q*wvhi+(1.0-q)*wvlo
              enddo
            enddo
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  xpos3,ypos3,maskpt(1,1,ngrid),
     &                  ngood(ngrid),ngrid,intpfl,radian,wflt4,ier)
          endif
c
          if (ifltyp(nfl).eq.1) then
c
c --- dividing by 3 gives total vertical displacement over interval 2*deltfl
            wflt=(wflt1+2.0*wflt2+2.0*wflt3+wflt4)/3.0
            if (ifladv.eq.1) then
              flt(nfl,3)=max(fldepm,flt(nfl,3)-wflt)
            endif
            if     (nfl.eq.nfl_debug) then
              write(lp,109) nfl,wflt,flt(nfl,3)
 109          format('nfl,w,new depth',i6,1p,2e13.5)
            endif !nfl_debug
c
          elseif (ifltyp(nfl).eq.2) then
c
c ---------------------------------------
c --- vertical motion of isopycnic floats
c ---------------------------------------
c
c --- set float depth to the vertically interpolated depth of the specified
c --- density interface
c
c --- assume float runs aground if no water denser than the specified density
c --- exists in the water column
c
c --- limit float depth to fldepm if no water lighter than the specified
c --- density exists in the water column
c
c --- find smallest k where float density is exceeded by the density in
c --- layer k at at least one of the good 16 surrounding grid points
            thflt=flt(nfl,7)-thbase
            ngrid=1
            ngoodi=ngood(ngrid)
            do k=1,kk
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  maskpi(i1,j1)=maskpt(i1,j1,ngrid)
                  if (.not.maskpi(i1,j1)) then
                    if (th3d(i,j,k,n).gt.thflt) then
                      kflt=k
                      go to 33
                    endif
                  endif
                enddo
              enddo
            enddo
   33       continue
c
c --- move downward from layer kflt to find the two layers with densities
c --- bracketing the assigned float density
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                if (.not.maskpi(i1,j1)) then
                  if (kflt.eq.1) then
                    phi=0.
                    plo=.5*dp(i,j,kflt,n)/onem
                    thflt=flt(nfl,7)-thbase
                    if(thflt.le.th3d(i,j,kflt,n)) then
                      varb2d(i1,j1)=max(fldepm,plo)
                      go to 50
                    endif
                  endif
                  do k=max(2,kflt),kk
                    phi=p(i,j,k-1)/onem
                    plo=p(i,j,k  )/onem
                    thhi=th3d(i,j,k-1,n)
                    thlo=th3d(i,j,k  ,n)
                    if(thflt.gt.thhi .and. thflt.le.thlo) then
                      qisop=max(0.0,min(1.0,(thlo-thflt)/
     &                      max(epsil,thlo-thhi)))
                      varb2d(i1,j1)=max(fldepm,qisop*phi+(1.-qisop)*plo)
                      if (depths(i,j)-varb2d(i1,j1).lt.0.1) then
                        maskpi(i1,j1)=.true.
                        ngoodi=ngoodi-1
                      endif
                      go to 50
                    endif
                  enddo
 50               continue
                endif
              enddo
            enddo
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  flt(nfl,1),flt(nfl,2),maskpi,
     &                  ngoodi,ngrid,intpfl,radian,depiso,ier)
            flt(nfl,3)=max(fldepm,depiso)
            if     (nfl.eq.nfl_debug) then
              write(lp,110) nfl,flt(nfl,3)
 110          format('nfl,new isopycnic depth',i6,1p,e13.5)
            endif !nfl_debug
c
          elseif (ifltyp(nfl).eq.4) then
c
            wflt=wflt4/deltfl
c
          endif
c
c --------------------------------------------------------------
c --- interpolate water properties to the present float location
c --------------------------------------------------------------
c
          if (ioflag.eq.1 .and. samplfl) then          
c
c --- recalculate q. assume float does not leave the layer that it was
c --- originally diagnosed to be within.
c
            q=max(0.0,min(1.0,(plo-flt(nfl,3))/max(0.001,plo-phi)))
c
c ---   water depth interpolation
            if (ifltyp(nfl).ne.4 .or. flt(nfl,4).eq.0.0) then
              ngrid=1
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=depths(i,j)
                enddo
              enddo
              call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                    flt(nfl,1),flt(nfl,2),maskpt(1,1,ngrid),
     &                    16,ngrid,intpfl,radian,depflt,ier)
              flt(nfl,4)=depflt
            endif
c
c --- temperature interpolation
            ngrid=1
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                if (ifltyp(nfl).eq.4) then
                  k1=max(1 ,k-1)
                  k2=min(kk,k+1)
                  varb2d(i1,j1)=
     &                0.5*(     q *(temp(i,j,k1,n)+temp(i,j,k,n))+
     &                     (1.0-q)*(temp(i,j,k2,n)+temp(i,j,k,n)))
                else
                  varb2d(i1,j1)=temp(i,j,k,n)
                endif
              enddo
            enddo
           call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                 flt(nfl,1),flt(nfl,2),maskpt(1,1,ngrid),
     &                 ngood(ngrid),ngrid,intpfl,radian,tflt,ier)
            flt(nfl,5)=tflt
c
c --- salinity interpolation
            ngrid=1
            do i1=1,4
              i=i1+ifltll(ngrid)-2
              do j1=1,4
                j=j1+jfltll(ngrid)-2
                if (ifltyp(nfl).eq.4) then
                  k1=max(1 ,k-1)
                  k2=min(kk,k+1)
                  varb2d(i1,j1)=
     &                0.5*(     q *(saln(i,j,k1,n)+saln(i,j,k,n))+
     &                     (1.0-q)*(saln(i,j,k2,n)+saln(i,j,k,n)))
                else
                  varb2d(i1,j1)=saln(i,j,k,n)
                endif
              enddo
            enddo
            call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                  flt(nfl,1),flt(nfl,2),maskpt(1,1,ngrid),
     &                  ngood(ngrid),ngrid,intpfl,radian,sflt,ier)
            flt(nfl,6)=sflt
c
c --- calculate density
            if (ifltyp(nfl).ne.2) then
              thflt=sig(tflt,sflt)
              flt(nfl,7)=thflt
            else
              tflt=tofsig(flt(nfl,7),sflt)
              flt(nfl,5)=tflt
            endif
c
c --- interpolate model fields for float type 4 (stationary)
            if (ifltyp(nfl).eq.4) then
c
c --- 3-d u and v interpolation
              ngrid=2
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  k1=max(1 ,k-1)
                  k2=min(kk,k+1)
                  
#if defined(STOKES)
                  varb2d(i1,j1)=ubavg(i,j,n)+
     &                0.5*(     q *(u(i,j,k1,n)+u(i,j,k,n)+
     &                            usd(i,j,k1)+usd(i,j,k))+
     &                     (1.0-q)*(u(i,j,k2,n)+u(i,j,k,n)+
     &                            usd(i,j,k2)+usd(i,j,k)))
#else                 
                  varb2d(i1,j1)=ubavg(i,j,n)+
     &                0.5*(     q *(u(i,j,k1,n)+u(i,j,k,n))+
     &                     (1.0-q)*(u(i,j,k2,n)+u(i,j,k,n)))
#endif                
                  
                enddo
              enddo
              call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                    flt(nfl,1),flt(nfl,2),maskpt(1,1,ngrid),
     &                    ngood(ngrid),ngrid,intpfl,radian,uflt,ier)
c
              ngrid=3
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  k1=max(1 ,k-1)
                  k2=min(kk,k+1)
                  
#if defined(STOKES)
                  varb2d(i1,j1)=vbavg(i,j,n)+
     &                0.5*(     q *(v(i,j,k1,n)+v(i,j,k,n)+
     &                            vsd(i,j,k1)+vsd(i,j,k))+
     &                     (1.0-q)*(v(i,j,k2,n)+v(i,j,k,n)+
     &                            vsd(i,j,k2)+vsd(i,j,k)))
#else                 
                  varb2d(i1,j1)=vbavg(i,j,n)+
     &                0.5*(     q *(v(i,j,k1,n)+v(i,j,k,n))+
     &                     (1.0-q)*(v(i,j,k2,n)+v(i,j,k,n)))
#endif                  
                  
                enddo
              enddo
              call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                    flt(nfl,1),flt(nfl,2),maskpt(1,1,ngrid),
     &                    ngood(ngrid),ngrid,intpfl,radian,vflt,ier)
c
c --- wveli interpolation 
              ngrid=1
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=q*wveli(i,j,k)+(1.0-q)*wveli(i,j,k+1)
                enddo
              enddo
              call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                    flt(nfl,1),flt(nfl,2),maskpt(1,1,ngrid),
     &                    ngood(ngrid),ngrid,intpfl,radian,wflt1,ier)
              flt(nfl,10)=wflt1/deltfl
c
c --- vertical viscosity coefficient interpolation
              ngrid=1
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=q*vcty(i,j,k)+(1.0-q)*vcty(i,j,k+1)
                enddo
              enddo
              call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                    flt(nfl,1),flt(nfl,2),maskpt(1,1,ngrid),
     &                    ngood(ngrid),ngrid,intpfl,radian,vkflt,ier)
              flt(nfl,11)=max(1.0e-4,vkflt)
c
c --- vertical temperature diffusivity coefficient interpolation
              ngrid=1
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=q*dift(i,j,k)+(1.0-q)*dift(i,j,k+1)
                enddo
              enddo
              call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                    flt(nfl,1),flt(nfl,2),maskpt(1,1,ngrid),
     &                    ngood(ngrid),ngrid,intpfl,radian,tkflt,ier)
              flt(nfl,12)=max(1.0e-5,tkflt)
c
c --- vertical scalar diffusivity coefficient interpolation
              ngrid=1
              do i1=1,4
                i=i1+ifltll(ngrid)-2
                do j1=1,4
                  j=j1+jfltll(ngrid)-2
                  varb2d(i1,j1)=q*difs(i,j,k)+(1.0-q)*difs(i,j,k+1)
                enddo
              enddo
              call intrph(varb2d,ptlon(1,1,ngrid),ptlat(1,1,ngrid),
     &                    flt(nfl,1),flt(nfl,2),maskpt(1,1,ngrid),
     &                    ngood(ngrid),ngrid,intpfl,radian,skflt,ier)
              flt(nfl,13)=max(1.0e-5,skflt)
c
            endif                        ! float type 4
c
            if     (nfl.eq.nfl_debug) then
              write(lp,111) nfl,tflt,sflt,thflt
 111          format('new t,s,th, float',i5,2x,3f9.3)
            endif !nfl_debug
c
          endif                          ! ioflag.eq.1
c
c ----------------------------------------------------
c --- flag floats that have run aground or left domain
c ----------------------------------------------------
c
c --- jump ahead if the float has not reached its termination time
          if (ntermn.gt.-10) go to 20
c
c --- the following code block is entered when the previous code has
c --- detected that the float needs to be terminated
 30       flt(nfl,1)=-999.
          flt(nfl,2)=-999.
          if (ntermn.gt.0) then
            write(lp,112) nfl,nstep,nstepfl,ier,ntermn,ngood
 112        format('float',i6,' aground, time step',i9,
     &             ' float time step',i9/'   ier ',i3,' exit point',
     &              i3,' ngood 1-4',4i3)
          elseif (ntermn.eq.-5) then
            write(lp,113) nfl
 113        format('float',i6,' exits domain')
          elseif (ntermn.eq.-10) then
            write(lp,114) nfl
 114        format('float',i6,' reaches termination time')
          endif
c
 20       continue
c
c ----------------------------------------------------
c --- store velocity components for synthetic moorings
c ----------------------------------------------------
c
          if (ifltyp(nfl).eq.4) then
            fltloc(nfl,1)=uflt
            fltloc(nfl,2)=vflt
            fltloc(nfl,3)=wflt
          else
            fltloc(nfl,1)=0.0
            fltloc(nfl,2)=0.0
            fltloc(nfl,3)=0.0
          endif
c
c--- store updated particle location
c
          do i=1,13
            flt3(   i+17*(nfl-1)) = flt(   nfl,i)
          enddo
          do i=1,3
            flt3(13+i+17*(nfl-1)) = fltloc(nfl,i)
          enddo
          flt3(17+17*(nfl-1)) = kfloat(nfl)
c
c --- else du premier if de la boucle
        else  !float is not on this tile, so disable flt3
          do i=1,17
            flt3(i+17*(nfl-1)) = hugel
          enddo
        endif
c
        go to 222
c
 22     continue
c
c ---   float is unchanged
        do i=1,17
          flt3(i+17*(nfl-1)) = hugel
        enddo
c
 222    continue
c
c ---------------------
c --- end of float loop
c ---------------------
c
      enddo !nfl
c
c --- copy floats back to 1st processor
      call xcminr(flt3(1:17*nflt), 1)
      if (mnproc.eq.1) then
        do nfl=1,nflt
          if     (flt3(1+17*(nfl-1)).ne.hugel) then
            do i=1,13
              flt(   nfl,i) = flt3(   i+17*(nfl-1))
            enddo
            do i=1,3
              fltloc(nfl,i) = flt3(13+i+17*(nfl-1))
            enddo
            kfloat(nfl) = flt3(17+17*(nfl-1))
         endif !updated float
        enddo !nfl
      endif !1st tile
c
c -----------------------------------
c --- output float array for analysis
c -----------------------------------
c
      if (ioflag.eq.1 .and. mnproc.eq.1) then
c
        write(lp,*) 'storing float parameters'
        open(unit=uoff+801,file=flnmflto,status='unknown',
     &       form='formatted',position='append')
        do nfl=1,nflt
          if (       flt(nfl,1).gt.-999. .and.
     &        timefl-flt(nfl,8).ge.-0.001     ) then
c
            if (samplfl) then
              if (ifltyp(nfl).ne.4) then
                write(uoff+801,901) iflnum(nfl),timefl,kfloat(nfl),
     &                             (flt(nfl,j),j=1,7)
 901            format(i6,f12.4,i3,2f10.4,f14.4,f8.2,3f7.3)
              else
                write(uoff+801,902) iflnum(nfl),timefl,kfloat(nfl),
     &                              fltloc(nfl,1),fltloc(nfl,2),
     &                              fltloc(nfl,3),flt(nfl,10),
     &                             (flt(nfl,j),j=4,7),
     &                             (flt(nfl,j),j=11,13)
 902            format(i6,f12.4,i3,2f10.6,2f14.10,f8.2,3f7.3,1p,3e12.4)
              endif
            else
              if (ifltyp(nfl).ne.4) then
                write(uoff+801,901) iflnum(nfl),timefl,kfloat(nfl),
     &                             (flt(nfl,j),j=1,3)
              else
                write(uoff+801,902) iflnum(nfl),timefl,kfloat(nfl),
     &                              fltloc(nfl,1),fltloc(nfl,2),
     &                              fltloc(nfl,3),flt(nfl,10)
              endif
            endif
          endif
        enddo
        close(uoff+801)
      endif
c
c ----------------------------------------------------
c --- store old velocities for next time interpolation
c ----------------------------------------------------
c
 10   continue
c
      if (hadvfl) then
c
        margin = 6
c
        do j=1-margin,jj+margin
          do i=1-margin,ii+margin
            if (SEA_U) then
              do k=1,kk
                uold2(i,j,k+kold2)=u(i,j,k,n)+ubavg(i,j,n)
              enddo !k
            endif !iu
            if (SEA_V) then
              do k=1,kk
                vold2(i,j,k+kold2)=v(i,j,k,n)+vbavg(i,j,n)
              enddo !k
            endif !iv
            if (SEA_P) then
              do k=1,kk
                wold2u(i,j,k+kold2)=wvelup(i,j,k)
                wold2d(i,j,k+kold2)=wveldn(i,j,k)
              enddo !k
            endif !ip
          enddo !i
        enddo !j
c
      endif
c
      return
      end subroutine floats

      subroutine intrph(varb2d,ptlon,ptlat,fllon,fllat,maskpt,
     &                  ngood,ngrid,intpfl,radian,vrbint,ier)
c
c ----------------------------------------------------
c --- 2-d polynomial or nearest neighbor interpolation
c ----------------------------------------------------
c
c --- polynomial interpolation code is adapted from the parameter matrix
c --- objective analysis algorithm of mariano and brown (1994), specifically
c --- the code used to calculate the large scale 2-d trend surface
c
c --- nptmin is presently set to 9
c
      implicit none
      real, dimension(4,4)   ::  varb2d,ptlon,ptlat
      real fllon,fllat,fllon2,fllat2,vfbint
      real wghtx,wghty,vrbint,suma,wghts,xcen,ycen,xo,yo,xsd,ysd,var
      real dx,dy,dxflt,dyflt,det,denom,radian
      real, dimension(16)    :: at,xt,yt
      real, dimension(12)    :: parm
      real, dimension(10,10) :: rtr,rtri
      real, dimension(10)    :: rta
      integer iarea(200)
      integer ngood,ngrid,intpfl,ier,iflag,ict,nptmin
      integer i,j,nd,nds,np,na
      logical maskpt(4,4)
c
c --- nptmin is the minimum number of points required for 2-d interpolation
c --- to be performed - otherwise, nearest neighbor interpolation is used
c
      data nptmin /9/
c
c --- na is the row dimension of rtr
      data na /10/
c
c --- set the 1-d arrays of field values and horizontal position
c --- find and remove central latitude and longitude of the good points
c --- correct horizontal position for earth's curvature to lowest order
c
      ict=0
      xcen=0.0
      ycen=0.0
      do i=1,4
        do j=1,4
          if (.not.maskpt(i,j) .or. ngood.eq.16) then
            ict=ict+1
            at(ict)=varb2d(i,j)
            xt(ict)=ptlon(i,j)
            xcen=xcen+xt(ict)
            yt(ict)=ptlat(i,j)
            ycen=ycen+yt(ict)
          endif
        enddo
      enddo
c
      np=ict
c
      if(np.le.0) then
        vrbint=0.0
        ier=999
        return
      endif
c
c --- set interpolated value to zero if all input values are zero
      iflag=0
      do ict=1,np
        if(at(ict).ne.0.) iflag=1
      enddo
      if(iflag.eq.0) then
        vrbint=0.0
        return
      endif
c
      xcen=xcen/np
      ycen=ycen/np
      denom=cos(ycen*radian)
c
      do ict=1,np
        xt(ict)=(xt(ict)-xcen)*cos(yt(ict)*radian)/denom
        yt(ict)=(yt(ict)-ycen)*cos(0.5*(yt(ict)+ycen)*radian)/denom
c
cdiag   write(6,101) ngrid,ngood,ict,at(ict),xt(ict),yt(ict)
 101    format('poly. - ngrid,ngood,ict,at,xt,yt:'
     &       /10x,3i4,1p,3e13.5)
c
      enddo
c
      fllon2=(fllon-xcen)*cos(fllat*radian)/denom
      fllat2=(fllat-ycen)*cos(0.5*(fllat+ycen)*radian)/denom
c
c --- perform the interpolation
c
      ier=0
c
      if (intpfl.eq.0 .and. ngood.ge.nptmin) then
c
c --- 2-d, second order polynomial interpolation
c
c --- set parameters for quadratic polynomial and zero the parm array
        nds=2
        nd=6
        do i=1,12
          parm(i)=0.0
        enddo
c
c --- calculate xo, the mean of xt, and yo, the mean of yt and their
c --- standard deviations from np observations 
c
        call f_stat(xt,np,xo,var,xsd)
        call f_stat(yt,np,yo,var,ysd)
c   
        do i=1,nd
          rta(i)=0.0
          do j=1,nd
            rtr(i,j)=0.0
          enddo
        enddo
c
c --- the next part of the code is a little messy, but we save alot of
c --- disk space by doing it this way
c
        do 20 ict=1,np
          dx=xt(ict)-xo
          dy=yt(ict)-yo
          rtr(1,2)=rtr(1,2) + dx
          rtr(1,3)=rtr(1,3) + dy
          rtr(2,2)=rtr(2,2) + dx**2
          rtr(2,3)=rtr(2,3) + dx*dy
          rtr(3,3)=rtr(3,3) + dy**2
          rta(1)=rta(1) + at(ict)
          rta(2)=rta(2) + dx*at(ict)
          rta(3)=rta(3) + dy*at(ict)
          if(nds.eq.1) go to 20
          rtr(2,4)=rtr(2,4) + (dx**3)/2.
          rtr(3,5)=rtr(3,5) + (dy**3)/2.
          rtr(4,4)=rtr(4,4) + (dx**4)/4.
          rtr(5,5)=rtr(5,5) + (dy**4)/4.
          rtr(3,4)=rtr(3,4) + (dx**2)*dy/2.
          rtr(2,5)=rtr(2,5) + (dy**2)*dx/2.
          rtr(4,6)=rtr(4,6) + (dx**3)*dy/2.
          rtr(5,6)=rtr(5,6) + (dy**3)*dx/2.
          rtr(6,6)=rtr(6,6) + (dx**2)*(dy**2)
          rta(4)=rta(4) + (dx**2)*at(ict)/2.
          rta(5)=rta(5) + (dy**2)*at(ict)/2.
          rta(6)=rta(6) + dx*dy*at(ict)
          if(nds.ne.3) go to 20
          rtr(4,7)=rtr(4,7) + (dx**5)/12.
          rtr(4,8)=rtr(4,8) + (dx**2)*(dy**3)/12.
          rtr(4,9)=rtr(4,9) + (dx**4)*dy/4.
          rtr(4,10)=rtr(4,10) + (dx**3)*(dy**2)/4.
          rtr(5,8)=rtr(5,8) + (dy**5)/12.
          rtr(5,10)=rtr(5,10) + dx*(dy**4)/4.
          rtr(6,7)=rtr(6,7) + dy*(dx**4)/6.
          rtr(7,7)=rtr(7,7) + (dx**6)/36.
          rtr(7,8)=rtr(7,8) + (dx**3)*(dy**3)/36.
          rtr(7,9)=rtr(7,9) + (dx**5)*dy/12.
          rtr(7,10)=rtr(7,10) + (dx**4)*(dy**2)/12.
          rtr(8,8)=rtr(8,8) + (dy**6)/36.
          rtr(8,9)=rtr(8,9) + (dx**2)*(dy**4)/12.
          rtr(8,10)=rtr(8,10) + dx*(dy**5)/12.
          rta(7)=rta(7) + (dx**3)*at(ict)/6.
          rta(8)=rta(8) + (dy**3)*at(ict)/6.
          rta(9)=rta(9) + (dx**2)*dy*at(ict)/2.
          rta(10)=rta(10) + (dy**2)*dx*at(ict)/2.
 20     continue
        rtr(1,1)=real(np)
        rtr(2,1)=rtr(1,2)
        rtr(3,2)=rtr(2,3)
        rtr(3,1)=rtr(1,3)
        if(nds.eq.1) go to 21
        rtr(1,4)=rtr(2,2)/2.
        rtr(1,5)=rtr(3,3)/2.
        rtr(1,6)=rtr(2,3)
        rtr(2,6)=rtr(3,4)*2.
        rtr(3,6)=rtr(2,5)*2.
        rtr(4,5)=rtr(6,6)/4.
        rtr(4,1)=rtr(1,4)
        rtr(4,2)=rtr(2,4)
        rtr(4,3)=rtr(3,4)
        rtr(5,1)=rtr(1,5)
        rtr(5,2)=rtr(2,5)
        rtr(5,3)=rtr(3,5)
        rtr(5,4)=rtr(4,5)
        rtr(6,1)=rtr(1,6)
        rtr(6,2)=rtr(2,6)
        rtr(6,3)=rtr(3,6)
        rtr(6,4)=rtr(4,6)
        rtr(6,5)=rtr(5,6)
        if(nds.ne.3) go to 21
        rtr(1,7)=rtr(2,4)/3.
        rtr(1,8)=rtr(3,5)/3.
        rtr(1,9)=rtr(2,6)/2.
        rtr(1,10)=rtr(2,5)
        rtr(2,7)=2.*rtr(4,4)/3.
        rtr(2,8)=rtr(5,6)/3.
        rtr(2,9)=rtr(4,6)
        rtr(2,10)=2.*rtr(4,5)
        rtr(3,7)=rtr(4,6)/3.
        rtr(3,8)=2.*rtr(5,5)/3.
        rtr(3,9)=2.*rtr(4,5)
        rtr(3,10)=3.*rtr(2,8)
        rtr(5,7)=rtr(4,10)/3.
        rtr(5,9)=rtr(4,8)/3.
        rtr(6,8)=2.*rtr(5,10)/3.
        rtr(6,9)=2.*rtr(4,10)
        rtr(6,10)=2.*rtr(5,9)
        rtr(7,1)=rtr(1,7)
        rtr(7,2)=rtr(2,7)
        rtr(7,3)=rtr(3,7)
        rtr(7,4)=rtr(4,7)
        rtr(7,5)=rtr(5,7)
        rtr(7,6)=rtr(6,7)
        rtr(8,1)=rtr(1,8)
        rtr(8,2)=rtr(2,8)
        rtr(8,3)=rtr(3,8)
        rtr(8,4)=rtr(4,8)
        rtr(8,5)=rtr(5,8)
        rtr(8,6)=rtr(6,8)
        rtr(8,7)=rtr(7,8)
        rtr(9,1)=rtr(1,9)
        rtr(9,2)=rtr(2,9)
        rtr(9,3)=rtr(3,9)
        rtr(9,4)=rtr(4,9)
        rtr(9,5)=rtr(5,9)
        rtr(9,6)=rtr(6,9)
        rtr(9,7)=rtr(7,9)
        rtr(9,8)=rtr(8,9)
        rtr(9,9)=3.*rtr(7,10)
        rtr(9,10)=9.*rtr(7,8)
        rtr(10,1)=rtr(1,10)
        rtr(10,2)=rtr(2,10)
        rtr(10,3)=rtr(3,10)
        rtr(10,4)=rtr(4,10)
        rtr(10,5)=rtr(5,10)
        rtr(10,6)=rtr(6,10)
        rtr(10,7)=rtr(7,10)
        rtr(10,8)=rtr(8,10)
        rtr(10,9)=rtr(9,10)
        rtr(10,10)=3.*rtr(8,9)
 21     continue
c 
c --- invert rtr and store in rtri, na is the row dimension of rtr
c --- and rtri, det is the determinant, nd is the number of coeff.
c --- to be estimated, i.e. the number of independent variables in
c --- rtr, iarea is a work area and ier is an error code. if ier
c --- is greater than 0, there is an inversion error
c
        call f_invmtx(rtr,na,rtri,na,nd,det,iarea,ier)
        if(ier.gt.0) print *, 'WARNING: INVERSION ERROR, ier= ',ier
c
c --- calculate the regression coeff. - parm
c
        do i=1,nd
          do j=1,nd
            parm(i)=parm(i) + rtri(i,j)*rta(j)
          enddo
        enddo
c
        parm(11)=xo
        parm(12)=yo
 85     continue
c
c --- interpolate the field to the float location
        dxflt=fllon2-parm(11)
        dyflt=fllat2-parm(12)
        vrbint=parm(1)+parm(2)*dxflt+parm(3)*dyflt
     &        +parm(4)*(dxflt**2)/2.+parm(5)*(dyflt**2)/2.+
     &         parm(6)*dxflt*dyflt
        return
c
      else
c
c --- 2-d nearest neighbor interpolation if the number of available water
c --- points is smaller than nptmin. variable xt is inverse distance
c
        wghts=0.
        do ict=1,np
          xt(ict)=1.0/sqrt(max(1.0e-20,(xt(ict)-fllon2)**2+
     &                                 (yt(ict)-fllat2)**2))
          wghts=wghts+xt(ict)
cdiag     write(6,102) ngrid,ngood,i,j,ict,at(ict),xt(ict)
 102      format('nearest neighbor - ngrid,ngood,i,j,ict,at,xt:'
     &           /10x,5i4,1p,2e13.5)
        enddo
c
        suma=0.
        do ict=1,np
          suma=suma+at(ict)*xt(ict)
        enddo
        vrbint=suma/wghts
        return
      endif
c
      end subroutine intrph
c
c
      SUBROUTINE F_INVMTX (A,NA,V,NV,N,D,IP,IER)
      implicit none
c
c --- this matrix inversion code is used in the parameter matrix
c --- objective analysis algorithm of mariano and brown (1994), specifically
c --- in the code used to calculate the large scale 2-d trend surface
c
      INTEGER NA,NV,N,IP(2*N),IER
      REAL D
      REAL, DIMENSION(NA,N) ::  A
      REAL, DIMENSION(NV,N) ::  V
c     DATA IEXMAX/75/
      integer, parameter :: iexmax=75
      integer j,m,i,iex, k, l, npm
      real    vmax,ch, pvt,pvtmx, hold, vh
  115 FORMAT(28H0*MATRIX SINGULAR IN INVMTX*)
  116 FORMAT(34H0*DETERMINANT TOO LARGE IN INVMTX*)
      IER = IERINV_F(N,NA,NV)
      IF (IER .NE. 0) RETURN
      DO 102 J=1,N
         IP(J) = 0
         DO 101 I=1,N
            V(I,J) = A(I,J)
  101    CONTINUE
  102 CONTINUE
      D = 1.
      IEX = 0
      DO 110 M=1,N
         VMAX = 0.
         DO 104 J=1,N
            IF (IP(J) .NE. 0) GO TO 104
            DO 103 I=1,N
               IF (IP(I) .NE. 0) GO TO 103
               VH =ABS(V(I,J))
               IF (VMAX .GT. VH) GO TO 103
               VMAX = VH
               K = I
               L = J
  103       CONTINUE
  104    CONTINUE
         IP(L) = K
         NPM = N+M
         IP(NPM) = L
         D = D*V(K,L)
  105    IF (ABS(D) .LE. 1.0) GO TO 106
         D = D*0.1
         IEX = IEX+1
         GO TO 105
  106    CONTINUE
         PVT = V(K,L)
         IF (M .EQ. 1) PVTMX = ABS(PVT)
         IF (ABS(PVT/REAL(M))+PVTMX .EQ. PVTMX) GO TO 113
         V(K,L) = 1.
         DO 107 J=1,N
            HOLD = V(K,J)
            V(K,J) = V(L,J)
            V(L,J) = HOLD/PVT
  107    CONTINUE
         DO 109 I=1,N
            IF (I .EQ. L) GO TO 109
            HOLD = V(I,L)
            V(I,L) = 0.
            DO 108 J=1,N
               V(I,J) = V(I,J)-V(L,J)*HOLD
  108       CONTINUE
  109    CONTINUE
  110 CONTINUE
      M = N+N+1
      DO 112 J=1,N
         M = M-1
         L = IP(M)
         K = IP(L)
         IF (K .EQ. L) GO TO 112
         D = -D
         DO 111 I=1,N
            HOLD = V(I,L)
            V(I,L) = V(I,K)
            V(I,K) = HOLD
  111    CONTINUE
  112 CONTINUE
      IF (IEX .GT. IEXMAX) GO TO 114
      D = D*10.**IEX
      RETURN
  113 IER = 33
c      PRINT 115
      RETURN
  114 IER = 1
      D = REAL(IEX)
      PRINT 116
      RETURN
      END SUBROUTINE F_INVMTX
c
      INTEGER FUNCTION IERINV_F (N,NA,NV)
      implicit none
      INTEGER N, NA, NV
  103 FORMAT(23H0* N .LT. 1 IN INVMTX *)
  104 FORMAT(24H0* NA .LT. N IN INVMTX *)
  105 FORMAT(24H0* NV .LT. N IN INVMTX *)
      IERINV_F = 0
      IF (N .GE. 1) GO TO 101
      IERINV_F = 34
      PRINT 103
      RETURN
  101 IF (NA .GE. N) GO TO 102
      IERINV_F = 35
      PRINT 104
      RETURN
  102 IF (NV .GE. N) RETURN
      IERINV_F = 36
      PRINT 105
      RETURN
      END FUNCTION IERINV_F

      subroutine f_stat(ser,ls,amean,var,std)
      implicit none
c
c --- computes mean, variance, standard deviation of data sequence
      real, dimension(16) :: ser
      integer ls
      real amean,var,std
      real sum,value
      integer j
      sum=0.0
      do j=1,ls
        sum=sum+ser(j)
      enddo
      amean=sum/ls
      sum=0.0
      do j=1,ls
        value=ser(j)-amean
        sum=sum+value*value
      enddo
      var=sum/ls
      std=sqrt(var)
c
      return
      end subroutine f_stat
c
c
c --- following are subprograms provided by Geoff Samuels, and inserted
c --- by Nasseer Idrisi (10/5/01)

      subroutine rantab_ini(rtab,iseed1,iseed2,iflag)
      implicit none
c
      real*4     rtab(200,2)
      integer*4  iseed1,iseed2
      integer*4  iflag
      integer    ic, inisee, itime
c
c --- get the two seeds
c --- iseed1 is for the 'usual'    random number generator  (udev_rt)
c --- iseed2 is for the 'gaussian' random number generator (grand_rt)
c
      integer*4 seed
      integer*2 isd2(2)
      equivalence (isd2,seed)
c
      call system_clock(inisee)
cdiag write(*,*)'calc itime',time()
      itime = inisee
      seed = 123456789
      iseed1 = isd2(1)
      if (iseed1 .lt. 0) then
        iseed1 = abs(iseed1)
      endif
      iseed1 = iseed1 + itime*iflag
cdiag write(*,*)'iseed1',iseed1
      seed = 987654321
      iseed2 = isd2(2)
      if (iseed2 .lt. 0) then
        iseed2 = abs(iseed1)
      endif
      iseed2 = iseed2 + itime*iflag
cdiag write(*,*)'iseed2',iseed2
c
c --- set seeds to negative values if iflag=1
c
      if (iflag .eq. 1) then
        iseed1 = -iseed1
        iseed2 = -iseed2
      endif
c
c --- load the table with a sequence of 200 random numbers.
      do  ic = 1,200
        rtab(ic,1) =  udev_rt(iseed1)
        rtab(ic,2) = grand_rt(iseed2)
      enddo
cdiag write(*,*)'rtabs',rtab(100,1),rtab(100,2)
c
      return
      end subroutine rantab_ini

      subroutine rantab (rtab,iseed1,iseed2,numran)
c
c --- each time this routine is called one of the 200 records in "rtab"
c --- is randomly selected and new random numbers are inserted
c
      real*4     rtab(200,2)
      integer*4  iseed1,iseed2,numran
c
c --- pick a number between 1 and 200
      numran = nint(1+199*udev_rt(iseed1))
      if (numran .gt. 200) then
        numran = 200
      endif
c
c --- get new random numbers
      rtab(numran,1) =  udev_rt(iseed1)
      rtab(numran,2) = grand_rt(iseed2)
c
      return
      end subroutine rantab
c
      real function grand_rt(idum)
      implicit none
c
c provides random number from gaussian distribution, mean 0, varaince 1,
c for finding random component of turbulent u
c
c from arthur mariano using routines from numerical recipes
c
      integer*4  idum
      integer*4  iset
      real*4     gset,v1,v2,r,fac
c      data iset/0/
c      save iset,gset
c 
      iset = 0
      gset=0.0
      if (iset.eq.0) then
 1      v1=2.*udev_rt(idum)-1.0
        v2=2.*udev_rt(idum)-1.0
        r=v1**2+v2**2
        if (r.ge.1.0)go to 1
        fac=sqrt(-2.*log(r)/r)
        gset=v1*fac
        grand_rt=v2*fac
        iset=1
      else
        grand_rt=gset
        iset=0
      endif
c
      return
      end function grand_rt
c
      real function udev_rt(iseed)
      
      integer*4  iseed
      integer, parameter :: m=714025,ia=1366,ic=150889
      real  rm
      integer, dimension(97) :: ir
      integer iy, iff,j
c
      iff = 0
      rm = 1./m
      if (iseed .lt. 0 .or. iff .eq. 0) then
        iff = 1
        iseed = mod(ic-iseed,m)
        do j=1,97
          iseed = mod(ia*iseed+ic,m)
          ir(j) = iseed
        enddo
        iseed = mod(ia*iseed+ic,m)
      else
        iseed = mod(iseed,m)
      endif
      iy=iseed
      j = min(97,1+(97*iy)/m)
      iy = ir(j)
      udev_rt = iy*rm
      iseed = mod(ia*iseed+ic,m)
      ir(j) = iseed
      return
      end function udev_rt
      end module mod_floats
c
c
c> Revision history:
c>
c> Nov  2006 - 1st module version
c> Nov  2012 - implicit none added by Till Andreas Rasmussen
c> May  2014 - use land/sea masks (e.g. ip) to skip land
