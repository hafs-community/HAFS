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
      module mod_tides
      use mod_xc  ! HYCOM communication interface
c
      implicit none
c
c --- HYCOM tides
c
      integer, parameter, public  :: ncon=8  !number of tidal consituents
c
      logical, parameter, private :: debug_tides=.false.  !usually .false.
c
      integer, save, public  ::
     & tidflg,   ! 0:notide,1:bdy.;2:body;3:body&bdy.
     & tidcon,   ! 1 digit per constituent (Q1K2P1N2O1K1S2M2), 0=off,1=on
     & tidein,   ! tide input flag: 0=no; 1=yes; 2=sal
     & tiddrg,   ! tidal drag flag: 0:no; -1,1=scalar; 2=tensor
     & nhrly     ! number of valid hourly samples (0 to 49)
c
      logical, save, public  ::
     & tidgen    ! generic time (don't correct tides for actual year)
c
      real*8,  save, public  ::
     & ramp_orig, ! tide ramp origin  (model day)
     & time_8     ! model time for tides
c
      real,    save, public  ::
     & tidsal,    ! scalar self attraction and loading factor (beta)
     & ramp_time  ! tide ramping time (days)
c
      real,    allocatable, dimension(:,:),
     &         save, public  ::
     &  etide,   ! body tide, in m
     & untide,   ! de-tided u-velocity, filtered from uhrly
     & vntide    ! de-tided u-velocity, filtered from vhrly
c
      real,    allocatable, dimension(:,:,:),
     &         save, public  ::
     & uhrly,    ! hourly u-velocity samples
     & vhrly     ! hourly v-velocity samples
c
      logical, save, private ::
     & tide_on(ncon)
c
      real,    allocatable, dimension(:,:,:),
     &         save, private ::
     & atide,    ! real      complex amplitude coefficents for body tide
     & btide,    ! imaginary complex amplitude coefficents for body tide
     & etidei    ! input body tide, in m

      real,    save, private ::
     &  wt0,
     &  wt1

      real*8,  save, private ::
     &  amp(ncon),omega(ncon),timeref,
     &  pu8(ncon),pf8(ncon),arg8(ncon),time_mjd

      contains

      subroutine tides_set(flag)
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer flag  !0 on initial call only
c
c --- body force tide setup
c
      integer iyear,idyold,iday,ihour,inty
      integer i,ihr,j,k,nleap,tidcon1
      real*8  t,h0,s0,p0,db,year8
      real*8  rad 
      real    alpha2q1,alpha2o1,alpha2p1,alpha2k1
      real    alpha2m2,alpha2s2,alpha2n2,alpha2k2
      real    diur_cos,diur_sin,semi_cos,semi_sin
      data rad/  0.0174532925199432d0 /
      save idyold,rad

      if     (tidflg.gt.0) then

        if(flag.eq.0) then
          tidcon1 = tidcon
          do i =1,ncon
            tide_on(i) = mod(tidcon1,10) .eq. 1
            tidcon1    =     tidcon1/10  ! shift by one decimal digit
          enddo
        endif

        if     (yrflag.eq.3) then
          call  forday(time_8,yrflag,iyear,iday,ihour)
          if     (flag.eq.0) then
            idyold=iday-1  !.ne.iday
          endif
c ---     update once per model day
          if     (iday.ne.idyold) then  !.or. flag.eq.0
            idyold=iday
c
c           time_mjd is in modified julian days, with zero on Nov 17 0:00 1858 
c           timeref  is in HYCOM    julian days, with zero on Dec 31 0:00 1900
 
            nleap = (iyear-1901)/4
            if(iyear.lt.1900)then
              inty = (iyear-1857)/4
            else
              inty = ((iyear-1857)/4)-1 !there was no leap year in 1900
            endif

            timeref  = 365.d0*(iyear-1901) + nleap 
     &               + iday
            time_mjd = 365.d0*(iyear-1858) + inty 
     &               - (31+28+31+30+31+30+31+31+30+31+17)
     &               + iday

*           if     (mnproc.eq.1) then
*           write (lp,*) 'tide_set: calling tides_nodal for a new day'
*           endif !1st tile
*           call xcsync(flush_lp)
            call tides_nodal
*           if (mnproc.eq.1) then
*             year8 = iyear + (iday - 1)/365.25d0
*             write(6,'(a,f11.5,8f8.4)') '#arg8 =',year8,arg8(1:8)
*             write(6,'(a,f11.5,8f8.4)') '#pu8  =',year8, pu8(1:8)
*             write(6,'(a,f11.5,8f8.4)') '#pf8  =',year8, pf8(1:8)
*           endif !1st tile
            call xcsync(flush_lp)
          endif  !iday.ne.idyold (.or. flag.eq.0)
        endif  !yrflag.eq.3

        if(flag.eq.0) then
           if     (mnproc.eq.1) then
           write (lp,*) ' now initializing tidal body forcing ...'
           write (lp,'(/a,i8.8/)') ' Q1K2P1N2O1K1S2M2 = ',tidcon
           endif !1st tile
           call xcsync(flush_lp)
c
c ---      amp is in m, and omega in 1/day.
c
           amp  ( 3)=   0.1424079984D+00
           omega( 3)=   0.6300387913D+01  ! K1
           amp  ( 4)=   0.1012659967D+00
           omega( 4)=   0.5840444971D+01  ! O1
           amp  ( 6)=   0.4712900147D-01
           omega( 6)=   0.6265982327D+01  ! P1
           amp  ( 8)=   0.1938699931D-01
           omega( 8)=   0.5612418128D+01  ! Q1
           amp  ( 1)=   0.2441020012D+00
           omega( 1)=   0.1214083326D+02  ! M2
           amp  ( 2)=   0.1135720015D+00
           omega( 2)=   0.1256637061D+02  ! S2
           amp  ( 5)=   0.4673499987D-01
           omega( 5)=   0.1191280642D+02  ! N2
           amp  ( 7)=   0.3087499924D-01
           omega( 7)=   0.1260077583D+02  ! K2
 
           if     (tidein.eq.1) then
             allocate( etidei(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2),
     &                  etide(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)   )
             call mem_stat_add( 3*(idm+2*nbdy)*(jdm+2*nbdy) )
             etidei(:,:,:) = 0.0
              etide(:,:)   = 0.0
             wt0 = -99.0
             wt1 = -99.0
             call tides_forfun(time_8)
           else
             allocate( atide(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,ncon),
     &                 btide(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,ncon),
     &                 etide(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)      )
             call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy)*ncon )
             call mem_stat_add(   (idm+2*nbdy)*(jdm+2*nbdy) )
             if     (tidein.eq.2) then
               call tides_forfun_sal  !input tidal SAL complex amplitudes
c ---          subtract SAL forcing
               atide(:,:,:) = -atide(:,:,:)
               btide(:,:,:) = -btide(:,:,:)
             else
               atide(:,:,:) = 0.0
               btide(:,:,:) = 0.0
             endif

c ---        alpha2=(1+k-h)g; Love numbers k,h  taken from 
c ---                         Foreman et al. JGR,98,2509-2532,1993
             alpha2q1=1.0+0.298-0.603
             alpha2o1=1.0+0.298-0.603
             alpha2p1=1.0+0.287-0.581
             alpha2k1=1.0+0.256-0.520
             alpha2m2=1.0+0.302-0.609
             alpha2s2=alpha2m2
             alpha2n2=alpha2m2
             alpha2k2=alpha2m2         
!$OMP      PARALLEL DO PRIVATE(j,i,semi_cos,semi_sin,diur_cos,diur_sin)
!$OMP&              SCHEDULE(STATIC,jblk)
           do j= 1-nbdy,jj+nbdy
             do i= 1-nbdy,ii+nbdy
               semi_cos=cos(rad*plat(i,j))**2*cos(rad*2*plon(i,j))
               semi_sin=cos(rad*plat(i,j))**2*sin(rad*2*plon(i,j))
               diur_cos=sin(2.*rad*plat(i,j))*cos(rad*plon(i,j))
               diur_sin=sin(2.*rad*plat(i,j))*sin(rad*plon(i,j))

               atide(i,j,3)=atide(i,j,3)+amp(3)*alpha2k1*diur_cos
               btide(i,j,3)=btide(i,j,3)+amp(3)*alpha2k1*diur_sin
               atide(i,j,4)=atide(i,j,4)+amp(4)*alpha2o1*diur_cos
               btide(i,j,4)=btide(i,j,4)+amp(4)*alpha2o1*diur_sin
               atide(i,j,6)=atide(i,j,6)+amp(6)*alpha2p1*diur_cos
               btide(i,j,6)=btide(i,j,6)+amp(6)*alpha2p1*diur_sin
               atide(i,j,8)=atide(i,j,8)+amp(8)*alpha2q1*diur_cos
               btide(i,j,8)=btide(i,j,8)+amp(8)*alpha2q1*diur_sin

               atide(i,j,1)=atide(i,j,1)+amp(1)*alpha2m2*semi_cos
               btide(i,j,1)=btide(i,j,1)+amp(1)*alpha2m2*semi_sin
               atide(i,j,2)=atide(i,j,2)+amp(2)*alpha2s2*semi_cos
               btide(i,j,2)=btide(i,j,2)+amp(2)*alpha2s2*semi_sin
               atide(i,j,5)=atide(i,j,5)+amp(5)*alpha2n2*semi_cos
               btide(i,j,5)=btide(i,j,5)+amp(5)*alpha2n2*semi_sin
               atide(i,j,7)=atide(i,j,7)+amp(7)*alpha2k2*semi_cos
               btide(i,j,7)=btide(i,j,7)+amp(7)*alpha2k2*semi_sin

               etide(i,j)  = 0.0
            enddo !i
          enddo  !j

          call xctilr(atide(1-nbdy,1-nbdy,1),1,ncon, nbdy,nbdy, halo_ps)
          call xctilr(btide(1-nbdy,1-nbdy,1),1,ncon, nbdy,nbdy, halo_ps)
          endif !tidein.eq.1:else

          if     (mnproc.eq.1) then
          write (lp,*) ' ...finished initializing tidal body forcing'
          endif !1st tile
          call xcsync(flush_lp)

        endif !flag.eq.0

      endif !tidflg.gt.0
 
      if(flag.eq.0) then
        if     (.not.allocated(uhrly)) then
c ---      restart_in did not allocate/input [uv]hrly
           allocate(  uhrly(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,49),
     &                vhrly(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,49),
     &               untide(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &               vntide(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)    )
           call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy)*50 )
           do j= 1-nbdy,jj+nbdy
             do i= 1-nbdy,ii+nbdy
               do ihr= 1,49
                 uhrly(i,j,ihr) = 0.0
                 vhrly(i,j,ihr) = 0.0
               enddo !ihr
             enddo !i
           enddo  !j
           nhrly = 0
        endif  !.not.allocated
        call tides_detide(1, .false.)  !initialise 49-hour filter
      endif !flag.eq.0

      return
      end subroutine tides_set

      subroutine tides_detide(n, update)
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer n       !time level index
      logical update  !.true. -> update hrly filter; .false. -> initialization
c
c --- form 49-hour filter
c
      integer i,ihr,j,k
      real    pthkbl,pbop,phi,plo,ubot,vbot,fc
      real*8  usum,vsum,fsum
c
      real    fhrly(49)
      save    fhrly
c
c --- a diurnal low pass filter,
c ---  covering 49 hours and lagged by 24 hours.
c
c --- the filter is the convolution of a 21 hr (10 point) 2nd-order
c ---  Savitzky-Golay smoothing filter and a 24.842 hr boxcar filter.
c --- it passes 0.02% of semi-diurnal and 3.2% of diurnal tides 
c ---  (1.2% of the total tides).
c
      real    f2hrly(0:24)
      save    f2hrly
      data    f2hrly / 9.297873 ,
     &                 9.297873 , 9.338932 , 9.835878     ,
     &                 10.04647 , 10.00111 , 9.730179     ,
     &                 9.264084 , 8.633219 , 7.867976     ,
     &                 6.998751 , 6.055939 , 5.069937     ,
     &                 4.071137 , 3.089936 , 2.156729     ,
     &                 1.301912 , 0.5558784,-5.0975680E-02,
     &                -0.4882553,-0.7255653,-0.7325106    ,
     &                -0.4786960, 0.0      , 0.0           /
c
      if     (update) then
c ---   calculate new hrly fields
        if     (thkdrg.ne.0.0) then
c ---     average over thkdrg from the bottom on u and v grids
!$OMP     PARALLEL DO PRIVATE(j,i,k,
!$OMP&                        pthkbl,pbop,phi,plo,ubot,vbot)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jj
            do i=1,ii
              util5(i,j)=0.0 !probably not needed
              util6(i,j)=0.0 !probably not needed
            enddo !i
            do i=1,ii
            if (SEA_U) then
              pthkbl=thkdrg*onem                  !thknss of bot. b.l.
              pbop=depthu(i,j)-pthkbl                !top of bot. b.l.
              phi =max(0.5*(p(i,j,1)+p(i+1,j,1)),pbop)
              ubot=0.0
              do k=1,kk
                plo =phi           ! max(0.5*(p(i,j,k)  +p(i-1,j,k)),pbop)
                phi =max(min(depthu(i,j),0.5*(p(i,j,k+1)+p(i-1,j,k+1))),
     &                   pbop)
                ubot=ubot + u(i,j,k,n)*(phi-plo)
              enddo !k
              util5(i,j)=ubot/min(pthkbl,depthu(i,j)) + ubavg(i,j,n)
            endif !ip
            enddo !i
            do i=1,ii
            if (SEA_V) then
              pthkbl=thkdrg*onem                  !thknss of bot. b.l.
              pbop=depthv(i,j)-pthkbl                !top of bot. b.l.
              phi =max(0.5*(p(i,j,1)+p(i+1,j,1)),pbop)
              vbot=0.0
              do k=1,kk
                plo =phi           ! max(0.5*(p(i,j,k)  +p(i-1,j,k)),pbop)
                phi =max(min(depthv(i,j),0.5*(p(i,j,k+1)+p(i-1,j,k+1))),
     &                   pbop)
                vbot=vbot + v(i,j,k,n)*(phi-plo)
              enddo !k
              util6(i,j)=vbot/min(pthkbl,depthv(i,j)) + vbavg(i,j,n)
            endif !ip
            enddo !i
          enddo !j
          call xctilr(util5,1,1, nbdy,nbdy, halo_uv)
          call xctilr(util6,1,1, nbdy,nbdy, halo_vv)
        else !thkdrg.eq.0.0
c ---     barotropic velocity on p-grid
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jj
            do i=1,ii
              util5(i,j)=0.0 !probably not needed
              util6(i,j)=0.0 !probably not needed
            enddo !i
            do i=1,ii
            if (SEA_P) then
              util5(i,j) = 0.5*( ubavg(i,j,n) + ubavg(i+1,j,n) )
              util6(i,j) = 0.5*( vbavg(i,j,n) + vbavg(i,j+1,n) )
            endif !ip
            enddo !i
          enddo !j
          call xctilr(util5,1,1, nbdy,nbdy, halo_pv)
          call xctilr(util6,1,1, nbdy,nbdy, halo_pv)
        endif
        if     (nhrly.eq.49) then
c ---     shift fields one hour, save new hour, form 49-hour filter
          do j= 1-nbdy,jj+nbdy
            do i= 1-nbdy,ii+nbdy
              usum = 0.0
              vsum = 0.0
              do ihr= 1,48
                uhrly(i,j,ihr) = uhrly(i,j,ihr+1)
                vhrly(i,j,ihr) = vhrly(i,j,ihr+1)
                usum = usum + fhrly(ihr)*uhrly(i,j,ihr)
                vsum = vsum + fhrly(ihr)*vhrly(i,j,ihr)
              enddo !ihr
                uhrly(i,j,49) = util5(i,j)
                vhrly(i,j,49) = util6(i,j)
                usum = usum + fhrly(49)*uhrly(i,j,49) 
                vsum = vsum + fhrly(49)*vhrly(i,j,49) 
              untide(i,j) = usum
              vntide(i,j) = vsum
            enddo !i
          enddo  !j
        else
c ---     save one more hour
          nhrly = nhrly + 1
          do j= 1-nbdy,jj+nbdy
            do i= 1-nbdy,ii+nbdy
              uhrly(i,j,nhrly) = util5(i,j)
              vhrly(i,j,nhrly) = util6(i,j)
              if     (nhrly.eq.49) then
c ---           form 49-hour filter
                usum = fhrly(1)*uhrly(i,j,1)
                vsum = fhrly(1)*vhrly(i,j,1)
                do ihr= 2,49
                  usum = usum + fhrly(ihr)*uhrly(i,j,ihr)
                  vsum = vsum + fhrly(ihr)*vhrly(i,j,ihr)
                enddo !ihr
                untide(i,j) = usum
                vntide(i,j) = vsum
              else
c ---           not enough hours for 49-hour filter yet
                untide(i,j) = 0.0
                vntide(i,j) = 0.0
              endif !nhrly
            enddo !i
          enddo  !j
        endif !nhrly:else
      else !.not.update -> initialize
c ---   normalize the filter weights
        fsum = f2hrly(0)            
        do ihr=1,24                 
          fsum = fsum + 2.0*f2hrly(ihr)
        enddo                          
        fc = 1.0/fsum                  
        fhrly(25) = fc*f2hrly(0)
        do ihr=1,24  
          fhrly(25+ihr) = fc*f2hrly(ihr)
          fhrly(25-ihr) = fc*f2hrly(ihr)
        enddo
c
        if     (nhrly.eq.49) then
c ---     form 49-hour filter, from restart input
          do j= 1-nbdy,jj+nbdy
            do i= 1-nbdy,ii+nbdy
              usum = fhrly(1)*uhrly(i,j,1)
              vsum = fhrly(1)*vhrly(i,j,1)
              do ihr= 2,49
                usum = usum + fhrly(ihr)*uhrly(i,j,ihr)
                vsum = vsum + fhrly(ihr)*vhrly(i,j,ihr)
              enddo !ihr
              untide(i,j) = usum
              vntide(i,j) = vsum
            enddo !i
          enddo  !j
        else
c ---     not enough hours for 49-hour filter from restart input
          do j= 1-nbdy,jj+nbdy
            do i= 1-nbdy,ii+nbdy
              untide(i,j) = 0.0
              vntide(i,j) = 0.0
            enddo !i
          enddo  !j
        endif !nhrly:else
      endif !update:initialize

      if (debug_tides) then
        if     (itest.gt.0 .and. jtest.gt.0) then
          write (lp,'(i9,2i5,3x,a,i2.2,a,2f10.6)')
     &      nstep,itest+i0,jtest+j0,
     &        ' hr',nhrly,' = ',
     &         uhrly(itest,jtest,nhrly), vhrly(itest,jtest,nhrly)
          write (lp,'(i9,2i5,3x,a,2f10.6)')
     &      nstep,itest+i0,jtest+j0,
     &        'ntide = ',
     &         untide(itest,jtest),     vntide(itest,jtest)
        endif
        call xcsync(flush_lp)
      endif  !debug_tides
      return
      end subroutine tides_detide

      subroutine tides_force(ll)
      use mod_xc  ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none

      integer ll

c
c --- calculate body tide
c

      integer i,j
      real*8  timef,timet,timermp
      real    ramp
      real    etide1,etide2,etide3,etide4,etide5,etide6,etide7,etide8

c     ramp-up of tide signal
      ramp   =1.0
      timermp=time_8+(ll*dlt/86400.d0)
      if(ramp_time.gt.0.0 ) then
         if(timermp .ge.ramp_orig)then
            timermp=(timermp-ramp_orig)/ramp_time
            ramp=ramp*(1.0-exp(-5.0*timermp))
         else
            ramp=0.0
         endif
      endif !ramp_time

      if     (.not.tidgen) then
        call tides_set(1)
      else
        arg8(1:ncon) = 0.0  !no correction for a specific year
         pu8(1:ncon) = 0.0  !no correction for a specific year
         pf8(1:ncon) = 1.0  !no correction for a specific year
      endif  !standard:generic
c
c --- Early return?
c
      if     (tidflg.lt.2) then
        RETURN
      endif
c
      if     (yrflag.eq.3) then
        timet=time_8+(ll*dlt/86400.d0)-timeref    !time from 00Z today
      else
        timet=time_8+(ll*dlt/86400.d0)-ramp_orig  !time since ramp_orig
      endif

      if    (tidein.eq.1) then
        timef=timeref+timet
        call tides_forfun(timef)
        do j= 1-nbdy,jj+nbdy
          do i= 1-nbdy,ii+nbdy
            etide(i,j)=ramp*(wt0*etidei(i,j,1)+wt1*etidei(i,j,2))
          enddo !i
        enddo !j
      else  !generate the body tide here
        etide1 = 0.0
        etide2 = 0.0
        etide3 = 0.0
        etide4 = 0.0
        etide5 = 0.0
        etide6 = 0.0
        etide7 = 0.0
        etide8 = 0.0
!$OMP PARALLEL DO PRIVATE(j,i,
!$OMP&         etide1,etide2,etide3,etide4,etide5,etide6,etide7,etide8)
!$OMP&         SCHEDULE(STATIC,jblk)
        do j= 1-nbdy,jj+nbdy
          do i= 1-nbdy,ii+nbdy
            if     (tide_on(1)) then
              etide1=               
     &        atide(i,j,1)*pf8(1)*cos(omega(1)*timet+arg8(1)+pu8(1))-
     &        btide(i,j,1)*pf8(1)*sin(omega(1)*timet+arg8(1)+pu8(1))
            endif                                              
            if     (tide_on(2)) then
              etide2=
     &        atide(i,j,2)*pf8(2)*cos(omega(2)*timet+arg8(2)+pu8(2))-
     &        btide(i,j,2)*pf8(2)*sin(omega(2)*timet+arg8(2)+pu8(2))
            endif
            if     (tide_on(3)) then
              etide3=
     &        atide(i,j,3)*pf8(3)*cos(omega(3)*timet+arg8(3)+pu8(3))-
     &        btide(i,j,3)*pf8(3)*sin(omega(3)*timet+arg8(3)+pu8(3))
            endif
            if     (tide_on(4)) then
              etide4=
     &        atide(i,j,4)*pf8(4)*cos(omega(4)*timet+arg8(4)+pu8(4))-
     &        btide(i,j,4)*pf8(4)*sin(omega(4)*timet+arg8(4)+pu8(4))
            endif
            if     (tide_on(5)) then
              etide5=
     &        atide(i,j,5)*pf8(5)*cos(omega(5)*timet+arg8(5)+pu8(5))-
     &        btide(i,j,5)*pf8(5)*sin(omega(5)*timet+arg8(5)+pu8(5))
            endif
            if     (tide_on(6)) then
              etide6=
     &        atide(i,j,6)*pf8(6)*cos(omega(6)*timet+arg8(6)+pu8(6))-
     &        btide(i,j,6)*pf8(6)*sin(omega(6)*timet+arg8(6)+pu8(6))
            endif
            if     (tide_on(7)) then
              etide7=
     &        atide(i,j,7)*pf8(7)*cos(omega(7)*timet+arg8(7)+pu8(7))-
     &        btide(i,j,7)*pf8(7)*sin(omega(7)*timet+arg8(7)+pu8(7))
            endif
            if     (tide_on(8)) then
              etide8=
     &        atide(i,j,8)*pf8(8)*cos(omega(8)*timet+arg8(8)+pu8(8))-
     &        btide(i,j,8)*pf8(8)*sin(omega(8)*timet+arg8(8)+pu8(8))
            endif

            etide(i,j)= etide1
     &                 +etide2
     &                 +etide3
     &                 +etide4
     &                 +etide5
     &                 +etide6
     &                 +etide7
     &                 +etide8
            etide(i,j)=ramp*etide(i,j)
          enddo !i
        enddo !j
      endif !tidein.eq.1:else

      if (debug_tides) then
        if     (itest.gt.0 .and. jtest.gt.0) then
          write (lp,'(i9,f14.6,2i5,3x,a,f10.6)')
     &      nstep,timeref+timet,itest+i0,jtest+j0,
     &        ' etide = ',etide(itest,jtest)
        endif
        call xcsync(flush_lp)
      endif  !debug_tides
      return
      end subroutine tides_force

      subroutine tides_forfun(dtime)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      real*8    dtime
c
c --- tidal body forcing field processing.
c
c --- units of etide are m, and it is on the p grid.
c
c --- I/O and array I/O units 917 are reserved for the entire run.
c
c --- all input fields much be defined at all grid points
c
      real*8    dtime0,dtime1
      save      dtime0,dtime1
c
      character preambl(5)*79,cline*80
      integer   i,ios,j,lgth,nrec
c
c --- wt0 negative on first call only.
      if     (wt0.lt.-1.0) then
c
c ---   initialize forcing fields
c
        if      (tidein.ne.1) then
          if     (mnproc.eq.1) then
          write(lp,*)
          write(lp,*) 'error in tides_forfun - tidein must be 1'
          write(lp,*)
          endif !1st tile
          call xcstop('(tides_forfun)')
                 stop '(tides_forfun)'
        endif
c
c ---   open all forcing files.
        if     (mnproc.eq.1) then
        write (lp,*) ' now initializing tidal forcing fields ...'
        endif !1st tile
        call xcsync(flush_lp)
c
        lgth = len_trim(flnmfor)
c
        call zaiopf(flnmfor(1:lgth)//'forcing.tidpot.a', 'old', 917)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+917,file=flnmfor(1:lgth)//'forcing.tidpot.b',
     &        status='old', action='read')
        read (uoff+917,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
c
c ---   skip ahead to the start time.
        nrec   = 0
        dtime1 = huge(dtime1)
        do  ! infinate loop, with exit at end
          dtime0 = dtime1
          nrec   = nrec + 1
          call zagetc(cline,ios, uoff+917)
          if     (ios.ne.0) then
            if     (mnproc.eq.1) then
              write(lp,*)
              write(lp,*) 'error in tides_forfun - hit end of input'
              write(lp,*) 'dtime0,dtime1 = ',dtime0,dtime1
              write(lp,*) 'dtime = ',dtime
              write(lp,*)
            endif !1st tile
            call xcstop('(tides_forfun)')
                   stop '(tides_forfun)'
          endif !ios
          i = index(cline,'=')
          read (cline(i+1:),*) dtime1
          if     (nrec.eq.1 .and. dtime1.lt.1462.0d0) then
c
c ---       must start after wind day 1462.0, 01/01/1905.
            if     (mnproc.eq.1) then
            write(lp,'(a)')  cline
            write(lp,'(/ a,a / a,g15.6 /)')
     &        'error in tides_forfun - actual forcing',
     &        ' must start after wind day 1462',
     &        'dtime1 = ',dtime1
            endif !1st tile
            call xcstop('(tides_forfun)')
                   stop '(tides_forfun)'
          endif !before wind day 1462.0
          if     (dtime0.le.dtime .and. dtime1.gt.dtime) then
            exit
          endif
        enddo   ! infinate loop, with exit above
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          rewind(unit=uoff+917)
          read (uoff+917,'(a79)') preambl
        endif
        call rdpall1(etidei,dtime0,917,.true.)
        call rdpall1(etidei,dtime1,917,.true.)
        call xctilr( etidei,1,2, nbdy,nbdy, halo_ps)
        if     (mnproc.eq.1) then
        write (lp,*) 
        write (lp,*) ' dtime,dtime0,dtime1 = ',dtime,dtime0,dtime1
        write (lp,*) 
        write (lp,*) ' ...finished initializing tidal forcing fields'
        endif !1st tile
        call xcsync(flush_lp)
      endif  ! initialization
c
      if     (dtime.gt.dtime1) then
c
c ---   get the next set of fields.
*           if     (mnproc.eq.1) then
*           write(lp,*) 'enter rdpall - ',dtime,dtime0,dtime1
*           endif !1st tile
*           call xcsync(flush_lp)
        dtime0 = dtime1
        call rdpall1(etidei,dtime1,917,.true.)
        call xctilr( etidei(1-nbdy,1-nbdy,2),1,1, nbdy,nbdy, halo_ps)
*           if     (mnproc.eq.1) then
*           write(lp,*) ' exit rdpall1 - ',dtime,dtime0,dtime1
*           endif !1st tile
*           call xcsync(flush_lp)
      endif
c
c --- linear interpolation in time.
      wt0 = (dtime1-dtime)/(dtime1-dtime0)
      wt1 = 1.0 - wt0
*           if     (mnproc.eq.1) then
*           write(lp,*) 'rdpall - dtime,wt0,wt1 = ',dtime,wt0,wt1
*           endif !1st tile
*           call xcsync(flush_lp)
      return
      end subroutine tides_forfun

      subroutine tides_forfun_sal
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
c --- initialize real and imaginary tidal SAL complex amplitudes
c
c --- units of atide and btide are m on the p-grid.
c
c --- I/O and array I/O unit 925 used here, but not reserved.
c
c --- all input fields much be defined at all grid points
c
      integer   i,j,k,lgth
c
      if     (mnproc.eq.1) then
      write (lp,*) ' now opening salReIm fields  ...'
      endif !1st tile
      call xcsync(flush_lp)
c
      lgth = len_trim(flnmfor)
c
      call zaiopf(flnmfor(1:lgth)//'tidal.salReIm.a', 'old', 925)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      open (unit=uoff+925,file=flnmfor(1:lgth)//'tidal.salReIm.b',
     &   status='old', action='read')
      endif !1st tile                
      do k= 1,8
        call rdmonth(atide(1-nbdy,1-nbdy,k), 925)      
        call rdmonth(btide(1-nbdy,1-nbdy,k), 925)      
      enddo !k
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      close (unit=uoff+925)                                  
      endif                                                  
      call zaiocl(925) 
c     
      call xctilr(atide,1,8, nbdy,nbdy, halo_ps)
      call xctilr(btide,1,8, nbdy,nbdy, halo_ps)
c     
      if     (mnproc.eq.1) then
      write (lp,*) ' ...finished reading salReIm fields '
      endif !1st tile
      call xcsync(flush_lp)
c     
      return
      end subroutine tides_forfun_sal

      subroutine tides_ports(dtime,nportpts,zR,zI,zA, port_tide)
      implicit none
c
      real*8  dtime
      integer nportpts
      real    zR(ncon,nportpts,3),zI(ncon,nportpts,3),zA(nportpts)
      real    port_tide(nportpts,3)
c
c --- generate the tidal signal (zuv) at port points
c
c --- On input:
c       dtime     = model time
c       nportpts  = number of port points
c       zR        = Real      zuv tidal reponse for ncon constituents
c       zI        = Imaginary zuv tidal reponse for ncon constituents
c       zA        = Pang (angle of xward wrt eward) at port points, radians
c
c --- On output:
c       port_tide = tidal signal (1:3 is z,u,v)
c
c --- Input  u and v are eastward and northward, but 
c --- Output u and v are x-ward and y-ward.
c --- On a rectilinear grid, zA is 0.0 and eastward==x-ward.
c
      integer n,j,k
      real    ramp,pt(3)
      real*8  timermp
      real*8  timet,Arg_p,ct,st,Ar,Ai

      timet=dtime - timeref

c     ramp-up of tide signal

      ramp   =1.0
      timermp=dtime
      if(ramp_time.gt.0.0 ) then
         if(timermp .ge.ramp_orig)then
            timermp=(timermp-ramp_orig)/ramp_time
            ramp=ramp*(1.0-exp(-5.0*timermp))
         else
            ramp=0.0
         endif
      endif !ramp_time

      port_tide(:,:)=0.0  !initiialize sum over ncom tidal components
      do n=1,ncon
        if(tide_on(n))then
          Arg_p=omega(n)*timet+pu8(n)+arg8(n)
          ct=cos(Arg_p)
          st=sin(Arg_p)
          Ar=pf8(n)*ct
          Ai=pf8(n)*st
          do k=1,3
            do j=1, nportpts
              port_tide(j,k)=port_tide(j,k)+zR(n,j,k)*Ar-zI(n,j,k)*Ai
            enddo !j
          enddo !k
        endif !tide_on
      enddo !n
!DAN-----------------------
!DAN  scale open boundary port tidal signal by ramp
!DAN         
      do j=1, nportpts
        do k=1,3
          pt(k)=ramp*port_tide(j,k)
        enddo !k
        port_tide(j,1)=           pt(1)
        port_tide(j,2)=cos(zA(j))*pt(2)+sin(zA(j))*pt(3)
        port_tide(j,3)=cos(zA(j))*pt(3)-sin(zA(j))*pt(2)
      enddo !j
*     if     (mnproc.eq.1) then
*       write(lp,'(a,f8.4,2f12.5)') 
*    &    'tides_ports: ramp,time =',ramp,dtime,timet
*     endif
      return
      end subroutine tides_ports

      subroutine tides_driver(z1rall,z1iall,dtime,
     &    astroflag,zpredall,start,ijtdm,ndat) !normal
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none

      integer start,ijtdm,ndat
      logical astroflag
      real    z1rall(ijtdm,ncon),z1iall(ijtdm,ncon),zpredall(ijtdm)
      real*8  dtime      

      character*10 cdate
      character*8  ctime
      logical interp_micon
      integer n,m,i,j,k,ic
      integer ind(ncon)
      real    zpred(ndat),z1r(ndat,ncon),z1i(ndat,ncon)
      real    Ai(ncon), Ar(ncon)

c From  ptide
      real ww(17,ncon)

      real*8 ttime

c from make_a
c If l_sal=.true. - NO solid Earth correction is applied
c USING beta_SE coefficients
      logical l_sal
      real beta_SE(ncon)
      real ci(ncon),cr(ncon)

c the succession is: M2,S2,K1,O1,N2,P1,K2,Q1 (corresponding to 
c succession in tidalports_*.input)

       data beta_SE/
     1    0.9540,0.9540,0.9400,0.9400,
     2    0.9540,0.9400,0.9540,0.9400/
       save beta_SE 
       
       l_sal = .TRUE.
       interp_micon =.FALSE.

       do i =1,ndat
         zpred(i) = zpredall(start+i-1)
         do ic =1,ncon
            z1r(i,ic) = z1rall(start+i-1,ic)
            z1i(i,ic) = z1iall(start+i-1,ic)
          enddo
        enddo

        do i =1,ncon
          ind(i) = i
        enddo
       
        ttime=dtime-timeref !days from 00Z today

c ndat is the number of lat/lon pairs
c output is zpred which is the elevations
        do k = 1, ndat

c Currently disabled
c. to include get ww from weights.h
        if(interp_micon)call tides_mkw(interp_micon,ind,ncon,ww)

         do j=1,ncon
           i=ind(j)
           if(i.ne.0)then
             cr(j) = pf8(i)*cos(omega(i)*ttime+arg8(i)+pu8(i))
             ci(j) = pf8(i)*sin(omega(i)*ttime+arg8(i)+pu8(i))
           endif
         enddo

c .true. means NO solid Earth correction will be applied in make_a
c  remove solid Earth tide
         if(.not.l_sal)then
           do j=1,ncon
             Ar(j)=0. 
             Ai(j)=0.
             if(ind(j).ne.0) then
             Ar(j)=cr(j)*beta_SE(ind(j))
             Ai(j)=ci(j)*beta_SE(ind(j))
             endif

           enddo
         else
           do j=1,ncon 
             Ar(j)=cr(j)*1.
             Ai(j)=ci(j)*1.
           enddo
         endif


         if(ncon.eq.0)then
           zpred(k)=0
         else
           zpred(k)=0
            do i=1,ncon
              zpred(k)=zpred(k)+z1r(k,i)*Ar(i)
     *            -z1i(k,i)*Ai(i)
            enddo
         endif
  
         zpredall(start+k-1) = zpred(k)
 
       enddo

        
       close(1)    


       return
      end subroutine tides_driver

        subroutine tides_mkw(interp,ind,nc,wr)
        real wr(17,ncon)
        logical interp
        integer i,j,nc,ind(nc)
        real w(17,ncon)
        data w(1,:)/1.0,  .00,  .00,  .00,  .00,  .00,  .00,  .00/
        data w(2,:)/0.0, 1.00,  .00,  .00,  .00,  .00,  .00,  .00/
        data w(3,:)/0.0,  .00,  1.0,  .00,  .00,  .00,  .00,  .00/
        data w(4,:)/0.0,  .00,  .00, 1.00,  .00,  .00,  .00,  .00/
        data w(5,:)/0.0,  .00,  .00,  .00, 1.00,  .00,  .00,  .00/
        data w(6,:)/0.0,  .00,  .00,  .00,  .00, 1.00,  .00,  .00/
        data w(7,:)/0.0,  .00,  .00,  .00,  .00,  .00, 1.00,  .00/
        data w(8,:)/0.0,  .00,  .00,  .00,  .00,  .00,  .00, 1.00/
        data w(9,:)/-0.0379, .0,.00,  .00,  .30859 ,0.0, .03289,.0/
        data w(10,:)/-0.03961,.0,.00,  .00,  .34380, 0.0, .03436,.0/
        data w(11,:)/.00696,  .0,.00,  .00,  .15719, 0.0, -.00547,.0/
        data w(12,:)/.02884,  .0,.00,  .00, -.05036, 0.0,  .07424,.0/
        data w(13,:)/.00854,  .0,.00,  .00, -.01913, 0.0,  .17685,.0/
        data w(14,:)/.0,  .0, -.00571, .11234, .0, .05285, .0, -.26257/
        data w(15,:)/.0,  .0,  .00749, .07474, .0, .03904, .0, -.12959/
        data w(16,:)/.0,  .0, -.03748, .12419, .0, .05843, .0, -.29027/
        data w(17,:)/.0,  .0,  .00842, .01002, .0,-.03064, .0,  .15028/
        save w
        wr=w
        if(interp)return
c
        do j=1,nc
         if(ind(j).ne.0)wr(ind(j),:)=0.
        enddo
	return
        end subroutine tides_mkw


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c argUMENTS and ASTROL subroutines SUPPLIED by RICHARD RAY, March 1999
c attached to OTIS by Lana Erofeeva (subroutine nodal.f)
c NOTE - "no1" in constit.h corresponds to "M1" in arguments
ciris	subroutine nodal(dtime,pu8,pf8,arg8)

        subroutine tides_nodal
        implicit none

        integer ncmx
        parameter(ncmx = 21)
c 21 put here instead of ncmx for compatability with old constit.h
        integer index(ncmx),i
        real*8 latitude,pu(ncmx),pf(ncmx)
        real*8 arg(53),f(53),u(53),pi
        

        data pi/3.14159265358979/
c index gives correspondence between constit.h and Richard's subroutines
c constit.h:       M2,S2,K1,O1,N2,P1,K2,q1,2N2,mu2,nu2,L2,t2,
c                  J1,M1(no1),OO1,rho1,Mf,Mm,SSA,M4
         data index/30,35,19,12,27,17,37,10,25,26,28,33,34,
     *             23,14,24,11,5,3,2,45/

        call tidal_arguments(time_mjd,arg,f,u)
        do i=1,ncmx
c u is returned by "tidal_arguments" in degrees
         pu(i)=u(index(i))*pi/180.d0
         pf(i)=f(index(i))
c         write(*,*)pu(i),pf(i)
        enddo

        do i =1,ncon
          pu8(i) = pu(i)
          pf8(i) = pf(i)
          arg8(i)= arg(index(i))*pi/180.d0
        enddo

        return
        end subroutine tides_nodal

      subroutine tidal_arguments( time1, arg, f, u)
      implicit none
 
      real*8 time1, arg(*), f(*), u(*)
*
*   Kernel routine for subroutine hat53.    Calculate tidal arguments.
*
      real*8 xi
      real*8 shpn(4),s,h,p,omega,pp,hour,t1,t2
      real*8 tmp1,tmp2,temp1,temp2
      real*8 cosn,cos2n,sinn,sin2n,sin3n
      real*8 zero,one,two,three,four,five
      real*8 fiften,thirty,ninety
      real*8 pi, rad
      parameter       (pi=3.141592654d0, rad=pi/180.d0)
      parameter   (zero=0.d0, one=1.d0)
      parameter   (two=2.d0, three=3.d0, four=4.d0, five=5.d0)
      parameter   (fiften=15.d0, thirty=30.d0, ninety=90.d0)
      parameter   (pp=282.94) ! solar perigee at epoch 2000.
      equivalence (shpn(1),s),(shpn(2),h),(shpn(3),p),(shpn(4),omega)
*
*     Determine equilibrium arguments
*     -------------------------------
      call tides_astrol( time1, shpn )
      hour = (time1 - int(time1))*24.d0
      t1 = fiften*hour
      t2 = thirty*hour
      arg( 1) = h - pp                                  ! Sa
      arg( 2) = two*h                                   ! Ssa
      arg( 3) = s - p                                   ! Mm
      arg( 4) = two*s - two*h                           ! MSf
      arg( 5) = two*s                                   ! Mf
      arg( 6) = three*s - p                             ! Mt
      arg( 7) = t1 - five*s + three*h + p - ninety      ! alpha1
      arg( 8) = t1 - four*s + h + two*p - ninety        ! 2Q1
      arg( 9) = t1 - four*s + three*h - ninety          ! sigma1
      arg(10) = t1 - three*s + h + p - ninety           ! q1
      arg(11) = t1 - three*s + three*h - p - ninety     ! rho1
      arg(12) = t1 - two*s + h - ninety                 ! o1
      arg(13) = t1 - two*s + three*h + ninety           ! tau1
      arg(14) = t1 - s + h + ninety                     ! M1
      arg(15) = t1 - s + three*h - p + ninety           ! chi1
      arg(16) = t1 - two*h + pp - ninety                ! pi1
      arg(17) = t1 - h - ninety                         ! p1
      arg(18) = t1 + ninety                             ! s1
      arg(19) = t1 + h + ninety                         ! k1
      arg(20) = t1 + two*h - pp + ninety                ! psi1
      arg(21) = t1 + three*h + ninety                   ! phi1
      arg(22) = t1 + s - h + p + ninety                 ! theta1
      arg(23) = t1 + s + h - p + ninety                 ! J1
      arg(24) = t1 + two*s + h + ninety                 ! OO1
      arg(25) = t2 - four*s + two*h + two*p             ! 2N2
      arg(26) = t2 - four*s + four*h                    ! mu2
      arg(27) = t2 - three*s + two*h + p                ! n2
      arg(28) = t2 - three*s + four*h - p               ! nu2
      arg(29) = t2 - two*s + h + pp                     ! M2a
      arg(30) = t2 - two*s + two*h                      ! M2
      arg(31) = t2 - two*s + three*h - pp               ! M2b
      arg(32) = t2 - s + p + 180.d0                     ! lambda2
      arg(33) = t2 - s + two*h - p + 180.d0             ! L2
      arg(34) = t2 - h + pp                             ! t2
      arg(35) = t2                                      ! S2
      arg(36) = t2 + h - pp + 180.d0                    ! R2
      arg(37) = t2 + two*h                              ! K2
      arg(38) = t2 + s + two*h - pp                     ! eta2
      arg(39) = t2 - five*s + 4.0*h + p                 ! MNS2
      arg(40) = t2 + two*s - two*h                      ! 2SM2
      arg(41) = 1.5*arg(30)                             ! M3
      arg(42) = arg(19) + arg(30)                       ! MK3
      arg(43) = three*t1                                ! S3
      arg(44) = arg(27) + arg(30)                       ! MN4
      arg(45) = two*arg(30)                             ! M4
      arg(46) = arg(30) + arg(35)                       ! MS4
      arg(47) = arg(30) + arg(37)                       ! MK4
      arg(48) = four*t1                                 ! S4
      arg(49) = five*t1                                 ! S5
      arg(50) = three*arg(30)                           ! M6
      arg(51) = three*t2                                ! S6
      arg(52) = 7.0*t1                                  ! S7
      arg(53) = four*t2                                 ! S8
*
*     determine nodal corrections f and u 
*     -----------------------------------
      sinn = sin(omega*rad)
      cosn = cos(omega*rad)
      sin2n = sin(two*omega*rad)
      cos2n = cos(two*omega*rad)
      sin3n = sin(three*omega*rad)
      f( 1) = one                                     ! Sa
      f( 2) = one                                     ! Ssa
      f( 3) = one - 0.130*cosn                        ! Mm
      f( 4) = one                                     ! MSf
      f( 5) = 1.043 + 0.414*cosn                      ! Mf
      f( 6) = sqrt((one+.203*cosn+.040*cos2n)**2 + 
     *              (.203*sinn+.040*sin2n)**2)        ! Mt

      f( 7) = one                                     ! alpha1
      f( 8) = sqrt((1.+.188*cosn)**2+(.188*sinn)**2)  ! 2Q1
      f( 9) = f(8)                                    ! sigma1
      f(10) = f(8)                                    ! q1
      f(11) = f(8)                                    ! rho1
      f(12) = sqrt((1.0+0.189*cosn-0.0058*cos2n)**2 +
     *             (0.189*sinn-0.0058*sin2n)**2)      ! O1
      f(13) = one                                     ! tau1
ccc   tmp1  = 2.*cos(p*rad)+.4*cos((p-omega)*rad)
ccc   tmp2  = sin(p*rad)+.2*sin((p-omega)*rad)         ! Doodson's
      tmp1  = 1.36*cos(p*rad)+.267*cos((p-omega)*rad)  ! Ray's
      tmp2  = 0.64*sin(p*rad)+.135*sin((p-omega)*rad)
      f(14) = sqrt(tmp1**2 + tmp2**2)                 ! M1
      f(15) = sqrt((1.+.221*cosn)**2+(.221*sinn)**2)  ! chi1
      f(16) = one                                     ! pi1
      f(17) = one                                     ! P1
      f(18) = one                                     ! S1
      f(19) = sqrt((1.+.1158*cosn-.0029*cos2n)**2 + 
     *             (.1554*sinn-.0029*sin2n)**2)       ! K1
      f(20) = one                                     ! psi1
      f(21) = one                                     ! phi1
      f(22) = one                                     ! theta1
      f(23) = sqrt((1.+.169*cosn)**2+(.227*sinn)**2)  ! J1
      f(24) = sqrt((1.0+0.640*cosn+0.134*cos2n)**2 +
     *             (0.640*sinn+0.134*sin2n)**2 )      ! OO1
      f(25) = sqrt((1.-.03731*cosn+.00052*cos2n)**2 +
     *             (.03731*sinn-.00052*sin2n)**2)     ! 2N2
      f(26) = f(25)                                   ! mu2
      f(27) = f(25)                                   ! N2
      f(28) = f(25)                                   ! nu2
      f(29) = one                                     ! M2a
      f(30) = f(25)                                   ! M2
      f(31) = one                                     ! M2b
      f(32) = one                                     ! lambda2
      temp1 = 1.-0.25*cos(two*p*rad)
     *        -0.11*cos((two*p-omega)*rad)-0.04*cosn
      temp2 = 0.25*sin(two*p)+0.11*sin((two*p-omega)*rad)
     *        + 0.04*sinn
      f(33) = sqrt(temp1**2 + temp2**2)               ! L2
      f(34) = one                                     ! t2
      f(35) = one                                     ! S2
      f(36) = one                                     ! R2
      f(37) = sqrt((1.+.2852*cosn+.0324*cos2n)**2 +
     *             (.3108*sinn+.0324*sin2n)**2)       ! K2
      f(38) = sqrt((1.+.436*cosn)**2+(.436*sinn)**2)  ! eta2
      f(39) = f(30)**2                                ! MNS2
      f(40) = f(30)                                   ! 2SM2
      f(41) = one   ! wrong                           ! M3
      f(42) = f(19)*f(30)                             ! MK3
      f(43) = one                                     ! S3
      f(44) = f(30)**2                                ! MN4
      f(45) = f(44)                                   ! M4
      f(46) = f(44)                                   ! MS4
      f(47) = f(30)*f(37)                             ! MK4
      f(48) = one                                     ! S4
      f(49) = one                                     ! S5
      f(50) = f(30)**3                                ! M6
      f(51) = one                                     ! S6
      f(52) = one                                     ! S7
      f(53) = one                                     ! S8

         u( 1) = zero                                    ! Sa
         u( 2) = zero                                    ! Ssa
         u( 3) = zero                                    ! Mm
         u( 4) = zero                                    ! MSf
         u( 5) = -23.7*sinn + 2.7*sin2n - 0.4*sin3n      ! Mf
         u( 6) = atan(-(.203*sinn+.040*sin2n)/
     *                 (one+.203*cosn+.040*cos2n))/rad   ! Mt
         u( 7) = zero                                    ! alpha1
         u( 8) = atan(.189*sinn/(1.+.189*cosn))/rad      ! 2Q1
         u( 9) = u(8)                                    ! sigma1
         u(10) = u(8)                                    ! q1
         u(11) = u(8)                                    ! rho1
         u(12) = 10.8*sinn - 1.3*sin2n + 0.2*sin3n       ! O1
         u(13) = zero                                    ! tau1
         u(14) = atan2(tmp2,tmp1)/rad                    ! M1
         u(15) = atan(-.221*sinn/(1.+.221*cosn))/rad     ! chi1
         u(16) = zero                                    ! pi1
         u(17) = zero                                    ! P1
         u(18) = zero                                    ! S1
         u(19) = atan((-.1554*sinn+.0029*sin2n)/
     *                (1.+.1158*cosn-.0029*cos2n))/rad   ! K1
         u(20) = zero                                    ! psi1
         u(21) = zero                                    ! phi1
         u(22) = zero                                    ! theta1
         u(23) = atan(-.227*sinn/(1.+.169*cosn))/rad     ! J1
         u(24) = atan(-(.640*sinn+.134*sin2n)/
     *                (1.+.640*cosn+.134*cos2n))/rad     ! OO1
         u(25) = atan((-.03731*sinn+.00052*sin2n)/ 
     *                (1.-.03731*cosn+.00052*cos2n))/rad ! 2N2
         u(26) = u(25)                                   ! mu2
         u(27) = u(25)                                   ! N2
         u(28) = u(25)                                   ! nu2
         u(29) = zero                                    ! M2a
         u(30) = u(25)                                   ! M2
         u(31) = zero                                    ! M2b
         u(32) = zero                                    ! lambda2
         u(33) = atan(-temp2/temp1)/rad                  ! L2
         u(34) = zero                                    ! t2
         u(35) = zero                                    ! S2
         u(36) = zero                                    ! R2
         u(37) = atan(-(.3108*sinn+.0324*sin2n)/ 
     *                (1.+.2852*cosn+.0324*cos2n))/rad   ! K2
         u(38) = atan(-.436*sinn/(1.+.436*cosn))/rad     ! eta2
         u(39) = u(30)*two                               ! MNS2
         u(40) = u(30)                                   ! 2SM2
         u(41) = 1.5d0*u(30)                             ! M3
         u(42) = u(30) + u(19)                           ! MK3
         u(43) = zero                                    ! S3
         u(44) = u(30)*two                               ! MN4
         u(45) = u(44)                                   ! M4
         u(46) = u(30)                                   ! MS4
         u(47) = u(30)+u(37)                             ! MK4
         u(48) = zero                                    ! S4
         u(49) = zero                                    ! S5
         u(50) = u(30)*three                             ! M6
         u(51) = zero                                    ! S6
         u(52) = zero                                    ! S7
         u(53) = zero                                    ! S8

      return
      end subroutine tidal_arguments


      SUBROUTINE TIDES_ASTROL( time, SHPN )     
*
*  Computes the basic astronomical mean longitudes  s, h, p, N.
*  Note N is not N', i.e. N is decreasing with time.
*  These formulae are for the period 1990 - 2010, and were derived
*  by David Cartwright (personal comm., Nov. 1990).
*  time is UTC in decimal MJD.
*  All longitudes returned in degrees.
*  R. D. Ray    Dec. 1990
*
*  Non-vectorized version.
*
c      IMPLICIT REAL*8 (A-H,O-Z)
      real*8 circle,shpn,t,time
      DIMENSION  SHPN(4)
      PARAMETER  (CIRCLE=360.0D0)
*
      T = time - 51544.4993D0
*
*     mean longitude of moon
*     ----------------------
      SHPN(1) = 218.3164D0 + 13.17639648D0 * T
*
*     mean longitude of sun
*     ---------------------
      SHPN(2) = 280.4661D0 +  0.98564736D0 * T
*
*     mean longitude of lunar perigee
*     -------------------------------
      SHPN(3) =  83.3535D0 +  0.11140353D0 * T
*
*     mean longitude of ascending lunar node
*     --------------------------------------
      SHPN(4) = 125.0445D0 -  0.05295377D0 * T

      SHPN(1) = MOD(SHPN(1),CIRCLE)
      SHPN(2) = MOD(SHPN(2),CIRCLE)
      SHPN(3) = MOD(SHPN(3),CIRCLE)
      SHPN(4) = MOD(SHPN(4),CIRCLE)

      IF (SHPN(1).LT.0.D0) SHPN(1) = SHPN(1) + CIRCLE
      IF (SHPN(2).LT.0.D0) SHPN(2) = SHPN(2) + CIRCLE
      IF (SHPN(3).LT.0.D0) SHPN(3) = SHPN(3) + CIRCLE
      IF (SHPN(4).LT.0.D0) SHPN(4) = SHPN(4) + CIRCLE
      RETURN
      END SUBROUTINE TIDES_ASTROL

c
      end module mod_tides
c
c
c> Revision history:
c>
c> Nov  2006 - 1st module version
c> Mar  2009 - remove   tides_detide, 25-hr mean near bottom velocities
c> Apr  2009 - put back tides_detide, 25-hr mean near bottom velocities
c> Jun  2011 - added nodal corrections, recalculated daily
c> Aug  2011 - use 49-hr filtered near bottom velocities
c> Aug  2011 - added tides_ports
c> Mar  2012 - added zA to tides_ports, for curvilinear grids
c> Apr  2012 - added the option to read in tidal body forcing
c> May  2012 - added the option to read in SAL complex amplitudes
c> May  2012 - tidein in place of tidef
c> Jul  2012 - use a 49-hr diurnal filter
c> Jan  2013 - added tiddrg and the barotropic and tensor drag options
c> May  2014 - use land/sea masks (e.g. ip) to skip land
