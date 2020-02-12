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
      subroutine mxkrtm(m,n)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n
c
c --- hycom version 1.0 (adapted from micom version 2.8)
c
#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     & sdot
c
      integer i,j,k
      real delp,q,thk
ccc   integer kmax
ccc   real totem,tosal,tndcyt,tndcys,work(3)
#if defined(RELO)
c
      if     (.not.allocated(sdot)) then
        allocate(
     &            sdot(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
        call mem_stat_add( (idm+2*nbdy)*(jdm+2*nbdy) )
                  sdot = r_init
      endif
#endif
c
!$OMP PARALLEL DO PRIVATE(j,k,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        do k=1,kk
          do i=1,ii
            if (SEA_P) then
              p(i,j,k+1)=p(i,j,k)+dp(i,j,k,n)
            endif !ip
          enddo !i
        enddo !k
      enddo !j
!$OMP END PARALLEL DO
c
 103  format (i9,2i5,a/(33x,i3,2f8.3,f8.3,0p,f8.2,f8.1))
cdiag if (itest.gt.0 .and. jtest.gt.0) write (lp,103) nstep,itest,jtest,
cdiag.  '  entering mxlayr:  temp    saln    dens    thkns    dpth',
cdiag.  (k,temp(itest,jtest,k,n),saln(itest,jtest,k,n),
cdiag.  th3d(itest,jtest,k,n)+thbase,dp(itest,jtest,k,n)*qonem,
cdiag.  p(itest,jtest,k+1)*qonem,k=1,kk)
c
      if (thermo .or. sstflg.gt.0 .or. srelax) then
c
c --- -----------------------------------
c --- mixed layer entrainment/detrainment
c --- -----------------------------------
c
!$OMP PARALLEL DO PRIVATE(j)
!$OMP&             SHARED(m,n,sdot)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        call mxkrtmaj(m,n, sdot, j)
      enddo
!$OMP END PARALLEL DO
c
      else !.not.thermo ...
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do  j=1,jj
        do i=1,ii
          if (SEA_P) then
            surflx(i,j)=0.
            salflx(i,j)=0.
            sdot(i,j)=dp(i,j,1,n)
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
      end if !thermo.or.sstflg.gt.0.or.srelax:else
c
cdiag if (itest.gt.0.and.jtest.gt.0.and.turgen(itest,jtest).lt.0.)
cdiag.  write (lp,'(i9,2i5,a,f8.2)') nstep,itest,jtest,
cdiag.  '  monin-obukhov length (m):',sdot(itest,jtest)*qonem
c
c --- store 'old' t/s column integral in totem/tosal (diagnostic use only)
ccc   totem=0.
ccc   tosal=0.
ccc   do k=1,kk
ccc   if (max(dp(itest,jtest,1,n)+sdot(itest,jtest),thkmin*onem).gt.
ccc  .  p(itest,jtest,k) .or. max(th3d(itest,jtest,1,m),th3d(itest,
ccc  .  jtest,1,n)) +sigjmp.ge.th3d(i,j,k,n)) then
ccc     kmax=k
ccc     totem=totem+temp(itest,jtest,k,n)*dp(itest,jtest,k,n)
ccc     tosal=tosal+saln(itest,jtest,k,n)*dp(itest,jtest,k,n)
ccc   end if
ccc   end do
c
!$OMP PARALLEL DO PRIVATE(j)
!$OMP&             SHARED(m,n,sdot)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        call mxkrtmbj(m,n, sdot, j)
      enddo
!$OMP END PARALLEL DO
c
c --- compare 'old' with 'new' t/s column integral (diagnostic use only)
c
ccc   tndcyt=-totem
ccc   tndcys=-tosal
ccc   do k=kmax,1,-1
ccc     tndcyt=tndcyt+temp(itest,jtest,k,n)*dp(itest,jtest,k,n)
ccc     tndcys=tndcys+saln(itest,jtest,k,n)*dp(itest,jtest,k,n)
ccc   end do
ccc   write (lp,'(i9,2i5,i3,3x,a,1p,3e10.2/25x,a,3e10.2)') nstep,itest,
ccc  .  jtest,kmax,'total saln,srf.flux,tndcy:',tosal/g,salflx(itest,
ccc  .  jtest)*delt1,tndcys/g,'total temp,srf.flux,tndcy:',totem/g,
ccc  .  surflx(itest,jtest)*delt1,tndcyt*spcifh/g
c
c --- store 'old' interface pressures in -pu,pv-
c
!$OMP PARALLEL DO PRIVATE(j,k,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
      do k=2,kk+1
      do i=1,ii
      if (SEA_U) then
      pu(i,j,k)=min(depthu(i,j),.5*(p(i,j,k)+p(i-1,j,k)))
      endif !iu
      enddo !i
c
      do i=1,ii
      if (SEA_V) then
      pv(i,j,k)=min(depthv(i,j),.5*(p(i,j,k)+p(i,j-1,k)))
      endif !iv
      enddo !i
      enddo !k
      enddo !j
!$OMP END PARALLEL DO
c
c --- store 'new' layer thicknesses in -dpu,dpv-
c
!$OMP PARALLEL DO PRIVATE(j,k,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        do k=1,kk
          do i=1,ii
            if (SEA_P) then
              p(i,j,k+1)=p(i,j,k)+dp(i,j,k,n)
            endif !ip
          enddo !i
        enddo !k
      enddo !j
!$OMP END PARALLEL DO
c
      call dpudpv(dpu(1-nbdy,1-nbdy,1,n),
     &            dpv(1-nbdy,1-nbdy,1,n),
     &            p,depthu,depthv, 0,0)
c
c --- redistribute momentum in the vertical.
c --- homogenize (u,v) over depth range defined in -util1,util2-
c
c --- thk>0 activates momentum diffusion across mixed-layer interface
      thk=vertmx*onem*delt1
c
!$OMP PARALLEL DO PRIVATE(j,i,k,delp,q)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
c
      do i=1,ii
      if (SEA_U) then

      util1(i,j)=max(dpu(i,j,1,n),pu(i,j,2)+thk)
      uflux(i,j)=0.
      util3(i,j)=0.
c
      do k=1,kk
      delp=max(0.,min(util1(i,j),pu(i,j,k+1))
     .           -min(util1(i,j),pu(i,j,k  )))
      uflux(i,j)=uflux(i,j)+u(i,j,k,n)*delp
      util3(i,j)=util3(i,j)            +delp
      enddo !k
c
      u(i,j,1,n)=uflux(i,j)/util3(i,j)
      endif !iu
      enddo !i
c
      do i=1,ii
      if (SEA_V) then
      util2(i,j)=max(dpv(i,j,1,n),pv(i,j,2)+thk)
      vflux(i,j)=0.
      util4(i,j)=0.
c
      do k=1,kk
      delp=max(0.,min(util2(i,j),pv(i,j,k+1))
     .           -min(util2(i,j),pv(i,j,k  )))
      vflux(i,j)=vflux(i,j)+v(i,j,k,n)*delp
      util4(i,j)=util4(i,j)            +delp
      enddo !k
c
      v(i,j,1,n)=vflux(i,j)/util4(i,j)
      endif !iv
      enddo !i
c
      do k=2,kk
c
      do i=1,ii
      if (SEA_U) then
      pu(i,j,k)=pu(i,j,k-1)+dpu(i,j,k-1,n)
      q=max(0.,min(1.,(util1(i,j)-pu(i,j,k))/(dpu(i,j,k,n)+epsil)))
      u(i,j,k,n)=u(i,j,1,n)*q+u(i,j,k,n)*(1.-q)
      endif !iu
      enddo !i
c
      do i=1,ii
      if (SEA_V) then
      pv(i,j,k)=pv(i,j,k-1)+dpv(i,j,k-1,n)
      q=max(0.,min(1.,(util2(i,j)-pv(i,j,k))/(dpv(i,j,k,n)+epsil)))
      v(i,j,k,n)=v(i,j,1,n)*q+v(i,j,k,n)*(1.-q)
      endif !iv
      enddo !i
      enddo !k
      enddo !j
!$OMP END PARALLEL DO
c
cdiag if (itest.gt.0 .and. jtest.gt.0) write (lp,103) nstep,itest,jtest,
cdiag.  '  exiting  mxlayr:  temp    saln    dens    thkns    dpth',
cdiag.  (k,temp(itest,jtest,k,n),saln(itest,jtest,k,n),
cdiag.  th3d(itest,jtest,k,n)+thbase,dp(itest,jtest,k,n)*qonem,
cdiag.  p(itest,jtest,k+1)*qonem,k=1,kk)
      return
      end

      subroutine mxkrtmaj(m,n, sdot, j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n, j
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
     & sdot
c
c --- hycom version 1.0 (adapted from micom version 2.8)
c
      integer i,k,ka
c
      real thknss,ustar3,dpth,ekminv,obuinv,buoyfl,dsgdt,tmn,smn,
     .     ex,alf1,alf2,cp1,cp3,ape,cc4,spe,pnew,alfadt,betads,thet
c
      real ea1, ea2, em1, em2, em3, em4, em5
      data ea1, ea2, em1, em2, em3, em4, em5
     .   /0.60,0.30,0.45,2.60,1.90,2.30,0.60/          ! Gaspar coefficients
c
      include 'stmt_fns.h'
c
      locsig=.true.
c
c --- -----------------------------------
c --- mixed layer entrainment/detrainment
c --- -----------------------------------
c
      do i=1,ii
      if (SEA_P) then
c
c --- determine turb.kin.energy generation due to wind stirring
c --- ustar computed in subr. -thermf-
c --- buoyancy flux (m**2/sec**3), all fluxes into the ocean
c --- note: surface density increases (column is destabilized) if buoyfl < 0
      thknss=dp(i,j,1,n)
      ustar3=ustar(i,j)**3
      tmn=.5*(temp(i,j,1,m)+temp(i,j,1,n))
      smn=.5*(saln(i,j,1,m)+saln(i,j,1,n))
      dsgdt=dsigdt(tmn,smn)
      buoyfl=-g*thref*(dsigds(tmn,smn)*salflx(i,j)*thref+
     &                 dsgdt          *surflx(i,j)*thref/spcifh)
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c --- option 1 :   k r a u s  -  t u r n e r    mixed-layer t.k.e.  closure
c
ccc   em=0.8*exp(-p(i,j,2)/(50.*onem))  !   hadley centre choice (orig.: 1.25)
ccc   en=0.15                           !   hadley centre choice (orig.: 0.4)
ccc   thermg=-.5*g*((en+1.)*buoyfl+(en-1.)*abs(buoyfl))*qthref
ccc   turgen(i,j)=delt1*(2.*em*g*ustar3*qthref+thknss*thermg)*qthref**2
c
c --- find monin-obukhov length in case of receding mixed layer (turgen < 0).
c --- the monin-obukhov length is found by stipulating turgen = 0.
c --- store temporarily in 'sdot'.
c
ccc   if (turgen(i,j).lt.0.) then
ccc     sdot(i,j)=-2.*em*g*ustar3/min(-epsil,thref*thermg)
ccc   else
ccc     sdot(i,j)=thknss
ccc   end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c --- option 2 :    g a s p a r   mixed-layer t.k.e. closure
c
      dpth=thknss*qonem
      ekminv=1./hekman(i,j)
      obuinv=buoyfl/max(epsil,ustar3)
      ex=exp(min(50.,dpth*obuinv))
      alf1=ea1+ea2*max(1.,2.5*dpth*ekminv)*ex
      alf2=ea1+ea2*ex
      cp1=((1.-em5)*(alf1/alf2)+.5*em4)*athird
      cp3=max(0.,(em4*(em2+em3)-(alf1/alf2)*(em2+em3-em3*em5))*athird)
      ape=cp3*ustar3-cp1*dpth*buoyfl
c
      if(ape.lt.0.) then                                       ! detrainment
      turgen(i,j)=(g*delt1*qthref**3)*ape
      sdot(i,j)=max(thkmin*onem,min(thknss,g*cp3/
     .(thref*cp1*max(epsil,obuinv))))
c
      else                                                     ! entrainment
      cc4=2.*em4/(em1*em1) * alf1*alf1
      spe=(em2+em3)*ustar3-0.5*dpth*buoyfl
      turgen(i,j)=(g*delt1*qthref**3)*(sqrt((.5*ape-cp1*spe)**2
     .                 +2.*cc4*ape*spe)-(.5*ape+cp1*spe))/(cc4-cp1)
      sdot(i,j)=thknss
      end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c --- util1,util2 are used to evaluate pot.energy changes during entrainment
      util1(i,j)=th3d(i,j,1,n)*thknss
      util2(i,j)=th3d(i,j,1,n)*thknss**2
c
c --- find pnew in case of mixed layer deepening (turgen > 0). store in 'sdot'.
c --- entrain as many layers as needed to deplete -turgen-.
c
      do k=2,kk
      ka=k-1
      if (k.eq.2) then
        thstar(i,j,ka,1)=th3d(i,j,ka,n)
      endif
      if (locsig) then
        alfadt=0.5*
     &        (dsiglocdt(temp(i,j,ka,n),saln(i,j,ka,n),p(i,j,k))+
     &         dsiglocdt(temp(i,j,k ,n),saln(i,j,k ,n),p(i,j,k)))*
     &        (temp(i,j,ka,n)-temp(i,j,k,n))
        betads=0.5*
     &        (dsiglocds(temp(i,j,ka,n),saln(i,j,ka,n),p(i,j,k))+
     &         dsiglocds(temp(i,j,k ,n),saln(i,j,k ,n),p(i,j,k)))*
     &        (saln(i,j,ka,n)-saln(i,j,k,n))
        thstar(i,j,k,1)=thstar(i,j,ka,1)-alfadt-betads
        thet=thstar(i,j,k,1)
      else
        thet=th3d(i,j,k,n)
      endif
      pnew=(2.*turgen(i,j)+thet*p(i,j,k)**2-util2(i,j))/
     .           max(epsil,thet*p(i,j,k)   -util1(i,j))
c --- stop iterating for 'pnew' as soon as pnew < k-th interface pressure
      if (pnew.lt.p(i,j,k)) pnew=sdot(i,j)
c --- substitute 'pnew' for monin-obukhov length if mixed layer is deepening
      if (turgen(i,j).ge.0.) sdot(i,j)=pnew
c
      util1(i,j)=util1(i,j)+thet*dp(i,j,k,n)
      util2(i,j)=util2(i,j)+thet*(p(i,j,k+1)**2-p(i,j,k)**2)
      enddo !k
      endif !ip
      enddo !i
      return
      end

      subroutine mxkrtmbj(m,n, sdot, j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n, j
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
     & sdot
c
c --- hycom version 1.0 (adapted from micom version 2.8)
c
      integer i,k,ktr,num
c
      real tdp(idm),sdp(idm)
      real pnew,thknss,t1,s1,tmxl,smxl,
     .     dpno,sn,tn,dtemp,dsaln,tnew,snew,z,s_up,a,e,b,f,d,c1msig,
     .     cc0,cc3,cc1,cc2,x
c
      real ccubq,ccubr,ccubqr,ccubs1,ccubs2,ccubrl,ccubim,root,root1,
     .     root2,root3
c
      include 'stmt_fns.h'
c
c --- cubic eqn. solver used in mixed-layer detrainment
      ccubq(s)=athird*(cc1/cc3-athird*(cc2/cc3)**2)
      ccubr(s)=athird*(.5*(cc1*cc2)/(cc3*cc3)-1.5*cc0/cc3)
     .       -(athird*cc2/cc3)**3
      ccubqr(s)=sqrt(abs(ccubq(s)**3+ccubr(s)**2))
      ccubs1(s)=sign(abs(ccubr(s)+ccubqr(s))**athird,ccubr(s)+ccubqr(s))
      ccubs2(s)=sign(abs(ccubr(s)-ccubqr(s))**athird,ccubr(s)-ccubqr(s))
      root(s)=ccubs1(s)+ccubs2(s)-athird*cc2/cc3
      ccubrl(s)=sqrt(max(0.,-ccubq(s)))
     .          *cos(athird*atan2(ccubqr(s),ccubr(s)))
      ccubim(s)=sqrt(max(0.,-ccubq(s)))
     .          *sin(athird*atan2(ccubqr(s),ccubr(s)))
      root1(s)=2.*ccubrl(s)-athird*cc2/cc3
      root2(s)=-ccubrl(s)+sqrt(3.)*ccubim(s)-athird*cc2/cc3
      root3(s)=-ccubrl(s)-sqrt(3.)*ccubim(s)-athird*cc2/cc3
c
      do i=1,ii
      if (SEA_P) then
c --- store (pnew - pold) in 'sdot'.
c --- don't allow mixed layer to get too deep or too shallow.
      sdot(i,j)=min(p(i,j,kk+1),max(thkmin*onem,sdot(i,j)))-
     .          dp(i,j,1,n)
      klist(i,j)=2
      tdp(i)=0.
      sdp(i)=0.
c
      do k=2,kk
      pnew=dp(i,j,1,n)+sdot(i,j)
c --- 'tdp,sdp' will be needed for temp./salin. mixing during entrainment
      tdp(i)=tdp(i)+temp(i,j,k,n)*(min(pnew,p(i,j,k+1))
     .                           -min(pnew,p(i,j,k  )))
      sdp(i)=sdp(i)+saln(i,j,k,n)*(min(pnew,p(i,j,k+1))
     .                           -min(pnew,p(i,j,k  )))
c
c --- if sdot > 0, remove water from layers about to be entrained.
      dpo(i,j,k,n)=dp(i,j,k,n)					! diapyc.flux
      dp( i,j,k,n)=max(p(i,j,k+1),pnew)-max(p(i,j,k),pnew)
      diaflx(i,j,k)=diaflx(i,j,k)+(dp(i,j,k,n)-dpo(i,j,k,n))	! diapyc.flux
      if (pnew.ge.p(i,j,k+1)) then
        do ktr= 1,ntracr
          tracer(i,j,k,n,ktr)=0.
        enddo !ktr
      endif
c
c --- if sdot < 0, mixed layer water will be detrained into isopycnic layer
c --- defined in -klist-. to prevent odd/even time step decoupling of mixed-
c --- layer depth, determine -klist- from layer one -th3d- at 2 consecutive
c --- time levels
c
      if (max(th3d(i,j,1,m),th3d(i,j,1,n))+sigjmp.ge.th3d(i,j,k,n)) 
     . klist(i,j)=k+1
c
c --- set t/s in massless layers. step 1: copy salinity from layer(s) above
c
      saln(i,j,k,n)=(saln(i,j,k,n)*dp(i,j,k,n)+saln(i,j,k-1,n)*epsil)/
     .              (              dp(i,j,k,n)+                epsil)
      enddo !k
c
c --- set t/s in massless layers. step 2: copy salinity from layer(s) below
c
      do k=kk-1,2,-1
      saln(i,j,k,n)=(saln(i,j,k,n)*dp(i,j,k,n)+saln(i,j,k+1,n)*epsil)/
     .              (              dp(i,j,k,n)+                epsil)
      enddo !k
c
c --- set t/s in massless layers. step 3: increase salinity where water
c --- is too fresh to fit into layer k
c 
      do k=2,kk
      if (saln(i,j,k,n).lt.salmin(k)) then
        saln(i,j,k,n)=salmin(k)
        temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,saln(i,j,k,n))
      end if
      enddo !k
c
c --- redistribute temp. and salin. during both de- and entrainment
c
      thknss=dp(i,j,1,n)
      pnew=thknss+sdot(i,j)
      t1=temp(i,j,1,n)
      s1=saln(i,j,1,n)
c
      tmxl=t1+surflx(i,j)*delt1*g/(spcifh*thknss)
      smxl=s1+salflx(i,j)*delt1*g/        thknss
c
cdiag if (i.eq.itest.and.j.eq.jtest) write (lp,'(i9,2i5,a,3f7.3,f8.2)')
cdiag.  nstep,i,j,'  t,s,sig,dp after diab.forcing',tmxl,smxl,
cdiag.  sig(tmxl,smxl),thknss*qonem
c
      if (sdot(i,j).ge.0.) then
c
c --- (mixed layer  d e e p e n s)
c
cdiag if (i.eq.itest.and.j.eq.jtest) write (lp,'(i9,2i5,a,f9.3,a)')
cdiag.  nstep,i,j,'  entrain',sdot(i,j)*qonem,' m of water'
c
      tmxl=(tmxl*thknss+tdp(i))/pnew
      smxl=(smxl*thknss+sdp(i))/pnew
      dp(i,j,1,n)=pnew
      diaflx(i,j,1)=diaflx(i,j,1)+sdot(i,j)			! diapyc.flux
c
      else if (sdot(i,j).lt.-onecm.and.surflx(i,j).ge.0.) then  ! sdot < 0
c
c --- (mixed layer  r e c e d e s)
c
      k=klist(i,j)
      if (k.gt.kk) go to 27
c
cdiag if (i.eq.itest.and.j.eq.jtest)
cdiag. write (lp,'(i9,2i5,a,i2,a,3p,2f7.3)') nstep,i,j,
cdiag. '  sig\*(1),sig\*(',k,') =',th3d(i,j,1,n)+thbase,
cdiag. th3d(i,j,k,n)+thbase
c
      dpno=max(dp(i,j,k,n),0.)
      sn=saln(i,j,k,n)
      tn=temp(i,j,k,n)
c
cdiag if (i.eq.itest.and.j.eq.jtest)
cdiag.  write (lp,'(i9,2i5,i3,a,2f9.4,f8.2)') nstep,i,j,k,
cdiag.  '  t,s,dp before detrainment',tn,sn,dpno*qonem
c
c --- distribute last time step's heating and freshwater flux over depth range
c --- 'pnew' (monin-obukhov length). split fossil mixed layer (depth= -sdot=
c --- thknss-pnew) into lower part ('lo') of depth z cooled and detrained into
c --- layer k, and an upper part ('up') heated to match temperature rise in
c --- mixed layer. transfer as much salinity as possible from sublayer 'up' to
c --- sublayer 'lo' without creating new maxima/minima in water column.
c
      dtemp=delt1*g*surflx(i,j)/(spcifh*pnew)
      dsaln=delt1*g*salflx(i,j)/        pnew
c
      tnew=    t1+dtemp
      snew=max(s1+dsaln,0.0)  !must be non-negative
c
      if (s1.le.sn .and. t1.gt.tn) then
c
c --- scenario 1: transfer t/s so as to achieve t_lo = t_k, s_lo = s_k
c
        z=-sdot(i,j)*min(1.,dtemp/max(epsil,tnew-tn))*qonem
        s_up=s1+(s1-sn)*dtemp/max(epsil*dtemp,t1-tn)
c --- is scenario 1 feasible?
        if (s_up.ge.min(snew,s1)) go to 24
      end if				! s_1 < s_n
c
c --- scenario 2: (t_lo,s_lo) differ from (tn,sn). main problem now is in
c --- maintaining density in layer k during detrainment. This requires solving
c --- 3rd deg. polynomial cc3*z**3 + cc2*z**2 + cc1*z + cc0 = 0 for z.
c
      s_up=min(s1,snew)
c --- new (t,s) in layer  k  will be t=(a*z+b)/(z+d), s=(e*z+f)/(z+d).
      a=tnew
      e=s_up
      b=(tn*dpno+    dtemp*sdot(i,j))*qonem
      f=(sn*dpno+(s_up-s1)*sdot(i,j))*qonem
      d=dpno*qonem
c
      c1msig=c1-(th3d(i,j,k,n)+thbase)
      cc0=d*d*(d*c1msig+b*c2+f*c3)+b*(d*f*c5+b*(d*c4+b*c6+f*c7))
      cc3=    (  c1msig+a*c2+e*c3)+a*(  e*c5+a*(  c4+a*c6+e*c7))
      cc1=d*(3.  *d*c1msig+(2.*b  +a*d)*c2+(2.  *f+d*e)*c3)+b*((2.*a*d
     .    +b  )*c4+3.*a*b*c6+(2.*a*f+b*e)*c7)+(a*d*f+b*(d*e+  f))*c5
      cc2=  (3.  *d*c1msig+(2.*a*d+b  )*c2+(2.*d*e+  f)*c3)+a*((2.*b
     .    +a*d)*c4+3.*a*b*c6+(2.*b*e+a*f)*c7)+(b  *e+a*(  f+d*e))*c5
c --- bound cc3 away from zero
      cc3=sign(max(1.e-6,abs(cc3)),cc3)
c
      x=0.0  ! dummy argument that is never used
      if (ccubq(x)**3+ccubr(x)**2.gt.0.) then
c --- one real root
      num=1
      z=root(x)
      else
c --- three real roots
      num=3
      z=root1(x)
      end if
c
cdiag if (i.eq.itest.and.j.eq.jtest) then
cdiag   work(1)=z
cdiag   if (num.eq.3) then
cdiag     work(2)=root2(x)
cdiag     work(3)=root3(x)
cdiag   end if
cdiag   write (lp,100) nstep,i,j,' t,s,dp( 1)=',tnew,snew,
cdiag.   thknss*qonem,'sdot,z=',sdot(i,j)*qonem,z,'t,s,dp(',k,')=',tn,
cdiag.   sn,dpno*qonem,'real root(s):',(work(nu),nu=1,num)
cdiag end if
 100  format (i9,2i5,a,2f7.3,f8.2,3x,a,2f8.2/20x,a,i2,a,2f7.3,f8.2,
     . 3x,a,1p3e11.4)
c
c --- does root fall into appropriate range?
      if (z.le.0.005) go to 27
c
c --- ready to detrain lowest 'z' meters from mixed layer
c
      temp(i,j,k,n)=(a*z+b)/(z+d)
      saln(i,j,k,n)=(e*z+f)/(z+d)
c
 24   continue
      sdot(i,j)=max(sdot(i,j),-z*onem)
      dp(i,j,1,n)=thknss+sdot(i,j)
      dp(i,j,k,n) =dpno  -sdot(i,j)
      smxl=(snew*pnew+s_up*(dp(i,j,1,n)-pnew))/dp(i,j,1,n)
      tmxl=tnew
      diaflx(i,j,1)=diaflx(i,j,1)+sdot(i,j)			! diapyc.flux
      diaflx(i,j,k)=diaflx(i,j,k)-sdot(i,j)			! diapyc.flux
c
c --- inject 'ventilation' tracer into layer k
      do ktr= 1,ntracr
        tracer(i,j,k,n,ktr)=(tracer(i,j,k,n,ktr)*dpno-sdot(i,j))
     &                               /(dpno-sdot(i,j))
      enddo !ktr
c
cdiag if (i.eq.itest.and.j.eq.jtest)
cdiag.  write (lp,'(i9,2i5,i3,a,2f9.4,f8.2)') nstep,i,j,k,
cdiag.  '  t,s,dp after  detrainment',temp(i,j,k,n),saln(i,j,k,n),
cdiag.  dp(i,j,k,n)*qonem
c
      end if				!  sdot > or < 0
c
 27   continue
      temp(i,j,1,n)=tmxl
      saln(i,j,1,n)=smxl
      th3d(i,j,1,n)=sig(tmxl,smxl)-thbase
      do ktr= 1,ntracr
        tracer(i,j,1,n,ktr)=1.0
      enddo !ktr
c
      dpmixl(i,j,n)=dp(  i,j,1,n)
      dpbl(  i,j)  =dp(  i,j,1,n)
      tmix(  i,j)  =temp(i,j,1,n)
      smix(  i,j)  =saln(i,j,1,n)
      thmix( i,j)  =th3d(i,j,1,n)
c
cdiag if (i.eq.itest.and.j.eq.jtest) write
cdiag.  (lp,'(i9,2i5,i3,a,2f9.4,f8.2)') nstep,i,j,1,
cdiag.  ' final mixed-layer t,s,dp ',tmxl,smxl,dp(i,j,1,n)*qonem
c
      endif !ip
      enddo !i
      return
      end
c
c
c> Revision history:
c>
c> June 1995 - removed restriction  'klist(i,j) .le. kk'
c> June 1995 - added code for setting t/s in massless layers below mix.layer
c> Oct. 1995 - removed bug created while changing klist (June 1995 revision):
c>             'if (k.gt.kk) go to 26' now reads 'if (k.gt.kk) go to 27'
c> May  1997 - changed -sdot- into local array
c> Mar. 1998 - added -th3d-
c> Nov. 1998 - fixed bug in computing tnew,snew in situations where z < 0.005
c> Dec. 1998 - replaced dsaln by (s_up-s1) in definition of 'f'
c> Feb. 1999 - limited 'tofsig' call in loop 45 to cases where saln < salmin
c> Aug. 2000 - adapted from micom 2.8 to run within hycom 1.0
c> May  2002 - buoyfl (into the ocean), calculated here
c> Aug  2011 - replaced dpold,dpoldm with dpo
c> May  2014 - use land/sea masks (e.g. ip) to skip land
