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
      subroutine mxkrta(m,n)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n
c
c --- hycom version 1.0
c --- original slab mixed layer
c
#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     & depnew
c
      integer i,j
cdiag integer k
cdiag real    totem,tosal,tndcyt,tndcys
c
c --- store 'old' t/s column integral in totem/tosal (diagnostic use only)
c
cdiag totem=0.
cdiag tosal=0.
cdiag do k=1,kk
cdiag   totem=totem+temp(itest,jtest,k,n)*dp(itest,jtest,k,n)
cdiag   tosal=tosal+saln(itest,jtest,k,n)*dp(itest,jtest,k,n)
cdiag end do
c
 103  format (i9,2i5,a/(32x,i3,2f8.2,f8.2,2f8.1))
cdiag write (lp,103) nstep,itest+i0,jtest+j0,
cdiag.  '  entering  mxkrt:  temp    saln    dens   thkns    dpth',
cdiag.  (k,temp(itest,jtest,k,n),saln(itest,jtest,k,n),
cdiag.  th3d(itest,jtest,k,n)+thbase,dp(itest,jtest,k,n)*qonem,
cdiag.  p(itest,jtest,k+1)*qonem,k=1,kk)
#if defined(RELO)
c
      if     (.not.allocated(depnew)) then
        allocate(
     &          depnew(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
        call mem_stat_add( (idm+2*nbdy)*(jdm+2*nbdy) )
                depnew = r_init
      endif
#endif
c --- ---------------
c --- new mixed layer
c --- ---------------
c
!$OMP PARALLEL DO PRIVATE(j)
!$OMP&             SHARED(m,n)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        call mxkrtaaj(m,n, j, depnew)
      enddo
!$OMP END PARALLEL DO
c
cdiag write (lp,103) nstep,itest,jtest,
cdiag.  '  exiting  mxkrta:  temp    saln    dens    thkns    dpth',
cdiag.  (k,temp(itest,jtest,k,n),saln(itest,jtest,k,n),
cdiag.  th3d(itest,jtest,k,n)+thbase,dp(itest,jtest,k,n)*qonem,
cdiag.  p(itest,jtest,k+1)*qonem,k=1,kk)
c
c --- compare 'old' with 'new' t/s column integral (diagnostic use only)
c
cdiag tndcyt=-totem
cdiag tndcys=-tosal
cdiag do k=1,kk
cdiag   tndcyt=tndcyt+temp(itest,jtest,k,n)*dp(itest,jtest,k,n)
cdiag   tndcys=tndcys+saln(itest,jtest,k,n)*dp(itest,jtest,k,n)
cdiag end do
cdiag write (lp,'(i9,2i5,3x,a,1p,3e12.4/22x,a,3e12.4)')
cdiag.  nstep,itest+i0,jtest+j0,
cdiag.  'total saln,srf.flux,tndcy:',tosal/g,salflx(itest,
cdiag.  jtest)*delt1,tndcys/g,'total temp,srf.flux,tndcy:',totem/g,
cdiag.  surflx(itest,jtest)*delt1,tndcyt*spcifh/g
c
c --- ---------------
c --- momentum mixing
c --- ---------------
c
!$OMP PARALLEL DO PRIVATE(j)
!$OMP&             SHARED(m,n)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        call mxkrtabj(m,n, j, depnew)
      enddo
!$OMP END PARALLEL DO
c
c --- fill mixed layer arrays
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        do i=1,ii
          if (SEA_P) then
            dpbl( i,j)=dpmixl(i,j, n)
            tmix( i,j)=temp(i,j,1,n)
            smix( i,j)=saln(i,j,1,n)
            thmix(i,j)=th3d(i,j,1,n)
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
      return
      end
      subroutine mxkrtaaj(m,n, j, depnew)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n,j
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
     & depnew
c
c --- hycom version 1.0
c --- single row, part A.
c
      integer i,k,ka,k0,k1,ktr
c
      real tdp(idm),sdp(idm),dtemp(idm),dsaln(idm)
      real dpth,ekminv,obuinv,ex,alf1,alf2,cp1,cp3,ape,cc4,spe,
     .     thknss,ustar3,buoyfl,dsgdt,tmn,smn,tup,sup,
     .     dtemp2,q,swfold,thet,alfadt,betads,
     .     swfrac,sflux1,tmin,tmax,smin,smax,trmin,trmax,
     .     thkold,thknew,t1,t2,s1,s2,tr1,tr2,dp1,dp2,dtrmax,
     &     chl
c
      real ea1, ea2, em1, em2, em3, em4, em5
      data ea1, ea2, em1, em2, em3, em4, em5
     .   /0.60,0.30,0.45,2.60,1.90,2.30,0.60/          ! Gaspar coefficients
c
      include 'stmt_fns.h'
c
c --- ---------------------
c --- set the vertical grid
c --- ---------------------
c
c --- store in -p- a set of interfaces that depict stratification the way a 
c --- "pure" isopycnic model would. -dpmixl- is physical mixed layer depth.
c --- store variables averaged over -dpmixl- in layer 1.
c
      do i=1,ii
      if (SEA_P) then
c
      klist(i,j)=-1
c
c --- start building up integral of t and s over mixed layer depth
      tdp(i)=temp(i,j,1,n)*dp(i,j,1,n)
      sdp(i)=saln(i,j,1,n)*dp(i,j,1,n)
      util1(i,j)=dp(i,j,1,n)
      util3(i,j)=th3d(i,j,1,n)
      p(i,j,2)=dp(i,j,1,n)
      pu(i,j,2)=dp(i,j,1,m)
c
      do k=2,kk
      p(i,j,k+1)=p(i,j,k)+dp(i,j,k,n)
      pu(i,j,k+1)=pu(i,j,k)+dp(i,j,k,m)
c
c --- if mixed layer base is very close to interface, move it there
      if (abs(p(i,j,k+1)-dpmixl(i,j,n)).lt.
     .    max(onecm,.001*dp(i,j,k,n))       ) then
        dpmixl(i,j,n)=p(i,j,k+1)
      endif
c
c --- watch for density decrease with depth (convective adjustment of
c --- the mixed layer) - convection occurs for both time steps to
c --- prevent mid-time and new mixed layer thicknesses from diverging
      if (klist(i,j).le.-1            .and.
     &    p(i,j,k+1).gt.dpmixl(i,j,n) .and.
     &    p(i,j,k  ).le.dpmixl(i,j,n)      ) then
        if (locsig) then
          tup=tdp(i)/util1(i,j)
          sup=sdp(i)/util1(i,j)
          alfadt=0.5*
     &          (dsiglocdt(tup,sup,util1(i,j))+
     &           dsiglocdt(temp(i,j,k,n),saln(i,j,k,n),util1(i,j)))*
     &          (tup-temp(i,j,k,n))
          betads=0.5*
     &          (dsiglocds(tup,sup,util1(i,j))+
     &           dsiglocds(temp(i,j,k,n),saln(i,j,k,n),util1(i,j)))*
     &          (sup-saln(i,j,k,n))
          if(alfadt+betads.gt.0.0) then
            dpmixl(i,j,n)=p (i,j,k+1)
            klist(i,j)=-2
          end if
        else
          th3d(i,j,1,n)=sig(tdp(i)/util1(i,j),sdp(i)/util1(i,j))
     &                 -thbase
          if(th3d(i,j,1,n).gt.th3d(i,j,k,n)) then
            dpmixl(i,j,n)=p (i,j,k+1)
            klist(i,j)=-2
          endif
        end if
      end if
c
      if (p(i,j,k+1).le.dpmixl(i,j,n)) then
        tdp(i)=tdp(i)+dp(i,j,k,n)*temp(i,j,k,n)
        sdp(i)=sdp(i)+dp(i,j,k,n)*saln(i,j,k,n)
        util1(i,j)=util1(i,j)+dp(i,j,k,n)
c
      else if (p(i,j,k).lt.dpmixl(i,j,n)) then
        klist(i,j)=k
      end if
      enddo !k
c
      temp(i,j,1,n)=tdp(i)/util1(i,j)
      saln(i,j,1,n)=sdp(i)/util1(i,j)
      th3d(i,j,1,n)=sig(temp(i,j,1,n),saln(i,j,1,n))-thbase
*     if (klist(i,j).eq.-2) then
*       util3(i,j)=th3d(i,j,1,n)
*       do k1=2,kk
*       if (p(i,j,k1+1).le.dpmixl(i,j,n)) then
*         th3d(i,j,k1,n)=th3d(i,j,1,n)
*       endif
*       enddo !k1
*     end if
c
c --- unmix t, s, and tracer
c
c --- the first guesses for upper sublayer values are the old-time mixed
c --- layer values saved in hybgen plus all changes that have occurred
c --- since then
c
c --- prevent spurious maxima or minima from being generated in the lower
c --- sublayer, then adjust upper sublayer values if necessary to conserve
c --- vertical averages 
c
      if(klist(i,j).ge.2) then
        k=klist(i,j)
        k0=min(k+1,kk)
        dp1=dpmixl(i,j,n)-p(i,j,k)
        dp2=p(i,j,k+1)-dpmixl(i,j,n)
        q=-dp1/dp2
        if(k.eq.nmlb(i,j,n)) then
          t1=t1sav(i,j,n)+temp(i,j,k,n)-tmlb(i,j,n)
          s1=s1sav(i,j,n)+saln(i,j,k,n)-smlb(i,j,n)
        else
          t1=temp(i,j,k-1,n)
          s1=saln(i,j,k-1,n)
          nmlb(i,j,n)=k
        end if
        tmin=min(t1,temp(i,j,k,n),temp(i,j,k0,n))
        tmax=max(t1,temp(i,j,k,n),temp(i,j,k0,n))
        smin=min(s1,saln(i,j,k,n),saln(i,j,k0,n))
        smax=max(s1,saln(i,j,k,n),saln(i,j,k0,n))
        t2=temp(i,j,k,n)+q*(t1-temp(i,j,k,n))
        s2=saln(i,j,k,n)+q*(s1-saln(i,j,k,n))
        temp(i,j,k,n)=min(tmax,max(tmin,t2))
        saln(i,j,k,n)=min(smax,max(smin,s2))
        util4(i,j)=th3d(i,j,k,n)
        th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
        t1=t1+(t2-temp(i,j,k,n))*dp2/dp1
        s1=s1+(s2-saln(i,j,k,n))*dp2/dp1
        tdp(i)=tdp(i)+t1*dp1
        sdp(i)=sdp(i)+s1*dp1
        temp(i,j,1,n)=tdp(i)/dpmixl(i,j,n)
        saln(i,j,1,n)=sdp(i)/dpmixl(i,j,n)
        th3d(i,j,1,n)=sig(temp(i,j,1,n),saln(i,j,1,n))-thbase
        do ktr= 1,ntracr
          tr1=1.0  ! THIS MAY BE WRONG FOR MULTIPLE TRACERS
          trmin=min(tr1,tracer(i,j,k,n,ktr),tracer(i,j,k0,n,ktr))
          trmax=max(tr1,tracer(i,j,k,n,ktr),tracer(i,j,k0,n,ktr))
          tr2=tracer(i,j,k,n,ktr)+q*(tr1-tracer(i,j,k,n,ktr))
          tracer(i,j,k,n,ktr)=min(trmax,max(trmin,tr2))
        enddo
      end if
c
c --- set the new grid
c
      do k=1,kk
      p(i,j,k+1)=max(dpmixl(i,j,n),p(i,j,k+1))
      enddo !k
c
      endif !ip
      enddo !i
c
c --- ----------------------------------------
c --- slab mixed layer entrainment/detrainment
c --- ----------------------------------------
c
      do i=1,ii
      if (SEA_P) then
c
c --- determine turb.kin.energy generation due to wind stirring
c --- ustar computed in subr. -thermf-
c --- buoyancy flux (m**2/sec**3), all fluxes into the ocean
c --- note: surface density increases (column is destabilized) if buoyfl < 0
      thkold=dpmixl(i,j,n)
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
ccc   thermg=-0.5*((en+1.)*buoyfl+(en-1.)*abs(buoyfl))
ccc   turgen(i,j)=delt1*(2.*em*g*ustar3*qthref+thkold*thermg)*qthref**2
c
c --- find monin-obukhov length in case of receding mixed layer (turgen < 0).
c --- the monin-obukhov length is found by stipulating turgen = 0.
c
ccc   if (turgen(i,j).lt.0.) then
ccc     depnew(i,j)=-2.*em*g*ustar3/min(-epsil,thref*thermg)
ccc   else
ccc     depnew(i,j)=thkold
ccc   end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c --- option 2 :    g a s p a r   mixed-layer t.k.e. closure
c
      dpth=thkold*qonem
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
      depnew(i,j)=min(thkold,g*cp3/(thref*cp1*max(epsil,obuinv)))
c
      else                                                     ! entrainment
      cc4=2.*em4/(em1*em1) * alf1*alf1
      spe=(em2+em3)*ustar3-0.5*dpth*buoyfl
      turgen(i,j)=(g*delt1*qthref**3)*(sqrt((.5*ape-cp1*spe)**2
     .                 +2.*cc4*ape*spe)-(.5*ape+cp1*spe))/(cc4-cp1)
      depnew(i,j)=thkold
      end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c --- util1,util2 are used to evaluate pot.energy changes during entrainment
      util1(i,j)=util3(i,j)*dp(i,j,1,n)
      util2(i,j)=util3(i,j)*dp(i,j,1,n)**2
      pu(i,j,2)=dp(i,j,1,n)
c
c --- find thknew in case of mx.layer deepening (turgen>0). store in -depnew-.
c --- entrain as many layers as needed to deplete -turgen-.
c
      do k=2,kk
      ka=k-1
      pu(i,j,k+1)=pu(i,j,k)+dp(i,j,k,n)
      if (k.eq.2) then
        thstar(i,j,ka,1)=util3(i,j)
      endif
      if (locsig) then
        alfadt=0.5*
     &        (dsiglocdt(temp(i,j,ka,n),saln(i,j,ka,n),pu(i,j,k))+
     &         dsiglocdt(temp(i,j,k ,n),saln(i,j,k ,n),pu(i,j,k)))*
     &        (temp(i,j,ka,n)-temp(i,j,k,n))
        betads=0.5*
     &        (dsiglocds(temp(i,j,ka,n),saln(i,j,ka,n),pu(i,j,k))+
     &         dsiglocds(temp(i,j,k ,n),saln(i,j,k ,n),pu(i,j,k)))*
     &        (saln(i,j,ka,n)-saln(i,j,k,n))
        thstar(i,j,k,1)=thstar(i,j,ka,1)-alfadt-betads
        thet=thstar(i,j,k,1)
      else
        if (k.ne.klist(i,j)) then
          thet=th3d(i,j,k,n)
        else
          thet=util4(i,j)
        endif
      endif
      thknew=max(dpmixl(i,j,n),min(pu(i,j,k+1),
     .       (2.0*turgen(i,j)+thet*pu(i,j,k)**2-util2(i,j))/
     .              max(epsil,thet*pu(i,j,k)   -util1(i,j))))
c --- stop iterating for 'thknew' as soon as thknew < k-th interface pressure
      if (thknew.lt.pu(i,j,k)) thknew=depnew(i,j)
c --- substitute 'thknew' for monin-obukhov length if mixed layer is deepening
      if (turgen(i,j).ge.0.) then
        depnew(i,j)=thknew
      endif
c
      util1(i,j)=util1(i,j)+thet*(pu(i,j,k+1)   -pu(i,j,k)   )
      util2(i,j)=util2(i,j)+thet*(pu(i,j,k+1)**2-pu(i,j,k)**2)
      enddo !k
      endif !ip
      enddo !i
c
      dtrmax = (onem*dtrate/86400.0) * delt1
      do i=1,ii
      if (SEA_P) then
c
cdiag if (i.eq.itest.and.j.eq.jtest) then
cdiag   if (turgen(i,j).lt.0.) then
cdiag     write (lp,'(i9,2i5,a,1p,2e13.5)') nstep,i+i0,j+j0,
cdiag.    '  m-o length (m), turgen:',depnew(i,j)*qonem,turgen(i,j)
cdiag   else
cdiag     write (lp,'(i9,2i5,a,1p,2e13.5)') nstep,i+i0,j+j0,
cdiag.    '  new depth (m), turgen:',depnew(i,j)*qonem,turgen(i,j)
cdiag   endif
cdiag endif
c
c --- don't allow mixed layer to get too deep or too shallow. mixed layer
c --- detrainment rate limited to dtrate m/day
      depnew(i,j)=min(p(i,j,kk+1)-onem,
     .            max(thkmin*onem,pu(i,j,3),dp(i,j,1,n)+onemm,
     .            depnew(i,j),dpmixl(i,j,n)-dtrmax))
c
      do k=2,kk
      thknew=depnew(i,j)
c --- integrate t/s over depth range slated for entrainment into mixed layer
      tdp(i)=tdp(i)+temp(i,j,k,n)*(min(thknew,p(i,j,k+1))
     .                           -min(thknew,p(i,j,k  )))
      sdp(i)=sdp(i)+saln(i,j,k,n)*(min(thknew,p(i,j,k+1))
     .                           -min(thknew,p(i,j,k  )))
      enddo !k
c
      thkold=p(i,j,2)
      thknew=depnew(i,j)
      thknss=max(thknew,thkold)
c
cdiag if (i.eq.itest.and.j.eq.jtest) write (lp,'(i9,2i5,a,2f10.4)')
cdiag.  nstep,i+i0,j+j0,
cdiag.  '  old/new mixed layer depth:',thkold*qonem,thknew*qonem
c
c --- distribute thermohaline forcing over new mixed layer depth
c --- flux positive into ocean
      if(pensol) then
c ---   penetrating solar radiation
        if     (jerlv0.le.0) then  !KPAR or CHL
          chl =  akpar(i,j,lk0)*wk0+akpar(i,j,lk1)*wk1
     &          +akpar(i,j,lk2)*wk2+akpar(i,j,lk3)*wk3
        endif
        call swfrml_ij(chl,thknew,p(i,j,kk+1),qonem,jerlov(i,j),swfrac)
        sflux1=surflx(i,j)-sswflx(i,j)
        dtemp(i)=(sflux1+(1.-swfrac)*sswflx(i,j))*delt1*g/
     &           (spcifh*thknew)
        dsaln(i)=salflx(i,j)                     *delt1*g/
     &                   thknew
cdiag if (i.eq.itest.and.j.eq.jtest) then
cdiag   write(lp,104) nstep,i+i0,j+j0,k,0.,1.-swfrac,dtemp(i),dsaln(i)
cdiag endif
 104  format(i9,2i5,i3,'absorbup,dn,dtemp,dsaln ',2f6.3,2f10.6)
c
      else
c
        dtemp(i)=surflx(i,j)*delt1*g/(spcifh*thknew)
        dsaln(i)=salflx(i,j)*delt1*g/        thknew
c
      end if !pensol:else
c
c --- calculate average temp, saln over max(old,new) mixed layer depth
      temp(i,j,1,n)=tdp(i)/thknss
      saln(i,j,1,n)=sdp(i)/thknss
      p(i,j,2)=dp(i,j,1,n)
      endif !ip
      enddo !i
c
c --- homogenize water mass properties down to max(old,new) mixed layer depth
c --- Asselin time smoothing of mixed layer depth
c
      do i=1,ii
      if (SEA_P) then
      thknss=max(depnew(i,j),dpmixl(i,j,n))
      dpmixl(i,j,n)=depnew(i,j)
      depnew(i,j)=thknss
      dpmixl(i,j,m)=(1.0-    ra2fac)* dpmixl(i,j,m)+
     .                   0.5*ra2fac *(dpmold(i,j)  +
     .                                dpmixl(i,j,n) )
c
      do k=2,kk
      p(i,j,k+1)=p(i,j,k)+dp(i,j,k,n)
      q=max(0.,min(1.,(depnew(i,j)-p(i,j,k))/(dp(i,j,k,n)+epsil)))
      temp(i,j,k,n)=temp(i,j,k,n)+q*(temp(i,j,1,n)-temp(i,j,k,n))
      saln(i,j,k,n)=saln(i,j,k,n)+q*(saln(i,j,1,n)-saln(i,j,k,n))
      do ktr= 1,ntracr
        tracer(i,j,k,n,ktr)=tracer(i,j,k,n,ktr)
     &    +q*(tracer(i,j,1,n,ktr)-tracer(i,j,k,n,ktr))
      enddo
      enddo !k
      endif !ip
      enddo !i
c
c --- add in surface thermohaline forcing over the new mixed layer depth
c --- add penetrating solar radiation
      do i=1,ii
      if (SEA_P) then
      do k=1,kk
      thknss=dpmixl(i,j,n)
      q=max(0.,min(1.,(thknss-p(i,j,k))/(dp(i,j,k,n)+epsil)))
      if(q.eq.1.) then
        temp(i,j,k,n)=    temp(i,j,k,n)+dtemp(i)
        saln(i,j,k,n)=max(saln(i,j,k,n)+dsaln(i),0.0)  !must be non-negative
        th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
      else
        temp(i,j,k,n)=    temp(i,j,k,n)+q*dtemp(i)
        saln(i,j,k,n)=max(saln(i,j,k,n)+q*dsaln(i),0.0)
        if(pensol) then
c
c ---     heat layers beneath mixed layer due to 
c ---     penetrating solar radiation (all redfac in mixed layer)
          if     (jerlv0.le.0) then
            chl =  akpar(i,j,lk0)*wk0+akpar(i,j,lk1)*wk1
     &            +akpar(i,j,lk2)*wk2+akpar(i,j,lk3)*wk3
          endif
          call swfrml_ij(chl,max(thknss,p(i,j,k  )),
     &                       p(i,j,kk+1),qonem,jerlov(i,j),swfold)
          call swfrml_ij(chl,           p(i,j,k+1),
     &                       p(i,j,kk+1),qonem,jerlov(i,j),swfrac)
          dtemp2=(swfold-swfrac)*sswflx(i,j)*delt1*g/
     &           (spcifh*max(onemm,p(i,j,k+1)-max(thknss,p(i,j,k))))
          temp(i,j,k,n)=temp(i,j,k,n)+(1.-q)*dtemp2
cdiag     if (i.eq.itest.and.j.eq.jtest) write (lp,104) nstep,i,j,1,
cdiag&    1.-swfold,1.-swfrac,(1.-q)*dtemp2
        end if !pensol
        th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
      end if
      enddo !k
      endif !ip
      enddo !i
      return
      end
      subroutine mxkrtabj(m,n, j, depnew)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n,j
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
     & depnew
c
c --- hycom version 1.0
c --- single row, part B.
c
      integer i,k,k1
c
      real    dp1,dp2,q,uv1,uv2,uvmin,uvmax
c
c --- ---------------
c --- momentum mixing
c --- ---------------
c
c --- homogenize -u- down to max(old,new) mixed layer depth
c
      do i=1,ii
      if (SEA_U) then
      util1(i,j)=min(depthu(i,j)-onem,max(dpu(i,j,1,n),thkmin*onem,
     .          .5*(depnew(i,j)+depnew(i-1,j))))
c
c --- if mixed layer base is very close to interface, move it there
      if (abs(util1(i,j)-dpu(i,j,1,n)).lt..001*dpu(i,j,1,n)) then
        util1(i,j)=dpu(i,j,1,n)+onecm
      endif
c
      uflux(i,j)=u(i,j,1,n)*dpu(i,j,1,n)
      util2(i,j)=dpu(i,j,1,n)
      pu(i,j,2)=dpu(i,j,1,n)
c
      do k=2,kk
      pu(i,j,k+1)=pu(i,j,k)+dpu(i,j,k,n)
c
c --- if mixed layer base is very close to interface, move it there
      if (abs(pu(i,j,k+1)-util1(i,j)).lt.
     .    max(onecm,.001*dpu(i,j,k,n))    ) then
        util1(i,j)=pu(i,j,k+1)
      endif
c
      if (pu(i,j,k+1).le.util1(i,j)) then
        uflux(i,j)=uflux(i,j)+u(i,j,k,n)*dpu(i,j,k,n)
        util2(i,j)=util2(i,j)+          dpu(i,j,k,n)
      end if
      enddo !k
c
      u(i,j,1,n)=uflux(i,j)/util2(i,j)
c
c --- unmix u
c --- first guess for upper sublayer value is the value from the layer
c --- immediately above the one containing the mixed layer base
      do k=2,kk
      k1=min(k+1,kk)
      if (pu(i,j,k  ).lt.util1(i,j) .and.
     .    pu(i,j,k+1).gt.util1(i,j)      ) then
        if(k.ge.3) then
          dp1=util1(i,j)-pu(i,j,k)
          dp2=pu(i,j,k+1)-util1(i,j)
          uv1=u(i,j,k-1,n)
          uvmin=min(uv1,u(i,j,k,n),u(i,j,k1,n))
          uvmax=max(uv1,u(i,j,k,n),u(i,j,k1,n))
          uv2=u(i,j,k,n)-(uv1-u(i,j,k,n))*dp1/dp2
          u(i,j,k,n)=min(uvmax,max(uvmin,uv2))
          uv1=uv1+(uv2-u(i,j,k,n))*dp2/dp1
          u(i,j,1,n)=(uflux(i,j)+uv1*dp1)/util1(i,j)
        end if
      end if
      enddo !k
c
      do k=2,kk
cdiag uold=u(i,j,k,n)
      q=max(0.,min(1.,(util1(i,j)-pu(i,j,k))/(dpu(i,j,k,n)+epsil)))
      u(i,j,k,n)=u(i,j,k,n)+q*(u(i,j,1,n)-u(i,j,k,n))
cdiag if (i.eq.itest .and. j.eq.jtest) write
cdiag.   (lp,'(i9,2i5,i3,a,f9.3,2f8.3)') nstep,i+i0,j+j0,k,
cdiag.   ' dpu, old/new u ',dpu(i,j,k,n)*qonem,uold,u(i,j,k,n)
      enddo !k
      endif !iu
      enddo !i
c
c --- homogenize -v- down to max(old,new) mixed layer depth
c
      do i=1,ii
      if (SEA_V) then
      util1(i,j)=min(depthv(i,j)-onem,max(dpv(i,j,1,n),thkmin*onem,
     .           .5*(depnew(i,j)+depnew(i,j-1))))
c
c --- if mixed layer base is very close to interface, move it there
      if (abs(util1(i,j)-dpv(i,j,1,n)).lt..001*dpv(i,j,1,n)) then
        util1(i,j)=dpv(i,j,1,n)+onecm
      endif
c
      vflux(i,j)=v(i,j,1,n)*dpv(i,j,1,n)
      util2(i,j)=dpv(i,j,1,n)
      pv(i,j,2)=dpv(i,j,1,n)
c
      do k=2,kk
      pv(i,j,k+1)=pv(i,j,k)+dpv(i,j,k,n)
c
c --- if mixed layer base is very close to interface, move it there
      if (abs(pv(i,j,k+1)-util1(i,j)).lt.
     .    max(onecm,.001*dpv(i,j,k,n))    ) then
        util1(i,j)=pv(i,j,k+1)
      endif
c
      if (pv(i,j,k+1).le.util1(i,j)) then
        vflux(i,j)=vflux(i,j)+v(i,j,k,n)*dpv(i,j,k,n)
        util2(i,j)=util2(i,j)+          dpv(i,j,k,n)
      end if
      enddo !k
c
      v(i,j,1,n)=vflux(i,j)/util2(i,j)
c
c --- unmix v
c --- first guess for upper sublayer value is the value from the layer
c --- immediately above the one containing the mixed layer base
      do k=2,kk
      k1=min(k+1,kk)
      if (pv(i,j,k  ).lt.util1(i,j) .and.
     .    pv(i,j,k+1).gt.util1(i,j)      ) then
        if(k.ge.3) then
          dp1=util1(i,j)-pv(i,j,k)
          dp2=pv(i,j,k+1)-util1(i,j)
          uv1=v(i,j,k-1,n)
          uvmin=min(uv1,v(i,j,k,n),v(i,j,k1,n))
          uvmax=max(uv1,v(i,j,k,n),v(i,j,k1,n))
          uv2=v(i,j,k,n)-(uv1-v(i,j,k,n))*dp1/dp2
          v(i,j,k,n)=min(uvmax,max(uvmin,uv2))
          uv1=uv1+(uv2-v(i,j,k,n))*dp2/dp1
          v(i,j,1,n)=(vflux(i,j)+uv1*dp1)/util1(i,j)
        end if
      end if
      enddo !k
c
      do k=2,kk
cdiag vold=v(i,j,k,n)
      q=max(0.,min(1.,(util1(i,j)-pv(i,j,k))/(dpv(i,j,k,n)+epsil)))
      v(i,j,k,n)=v(i,j,k,n)+q*(v(i,j,1,n)-v(i,j,k,n))
cdiag if (i.eq.itest .and. j.eq.jtest) write
cdiag.   (lp,'(i9,2i5,i3,a,f9.3,2f8.3)') nstep,i+i0,j+j0,k,
cdiag.   ' dpv, old/new v ',dpv(i,j,k,n)*qonem,vold,v(i,j,k,n)
      enddo !k
      endif !iv
      enddo !i
c
      return
      end
c
      subroutine mxkrtb(m,n)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n
c
c --- hycom version 1.0 -- alternative slab mixed layer model
c
      integer j
c
!$OMP PARALLEL DO PRIVATE(j)
!$OMP&             SHARED(m,n)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        call mxkrtbaj(m,n, j)
      enddo
!$OMP END PARALLEL DO
c
!$OMP PARALLEL DO PRIVATE(j)
!$OMP&             SHARED(m,n)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        call mxkrtbbj(m,n, j)
      enddo
!$OMP END PARALLEL DO
c
      return
      end
c
      subroutine mxkrtbaj(m,n, j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n,j
c
c --- hycom version 1.0 -- alternative slab mixed layer model
c --- single row, part A.
c
      real    dpth,ekminv,obuinv,ex,alf1,alf2,cp1,cp3,ape,cc4,spe,
     &        ustar3,thkold,thknew,value,q,tdp,sdp,trdp(mxtrcr),
     &        tem,sal,rho,thet,alfadt,betads,
     &        ttem(kdm),ssal(kdm),ttrc(kdm,mxtrcr),dens(kdm),densl(kdm),
     &        pres(kdm+1),delp(kdm),sum1,sum2,buoyfl,dsgdt,tmn,smn
cdiag real    totem,tosal,tndcyt,tndcys
      integer kmxbot
      integer i,k,ka,ktr
c
c --- abs.bound (m/day) and rel.bound (percent/day) on detrainment rate:
ccc   real bound1, bound2
ccc   data bound1, bound2 /200.0, 0.10/
c
      real ea1, ea2, em1, em2, em3, em4, em5
      data ea1, ea2, em1, em2, em3, em4, em5
     .   /0.60,0.30,0.45,2.60,1.90,2.30,0.60/          ! Gaspar coefficients
c
      include 'stmt_fns.h'
c
      do i=1,ii
      if (SEA_P) then
c
c --- extract single column from 3-d fields
      pres(1)=p(i,j,1)
      do k=1,kk
      ttem(k)=temp(i,j,k,n)
      ssal(k)=saln(i,j,k,n)
      dens(k)=th3d(i,j,k,n)
      do ktr= 1,ntracr
        ttrc(k,ktr)=tracer(i,j,k,n,ktr)
      enddo
      delp(k)=dp(i,j,k,n)
      pres(k+1)=pres(k)+delp(k)
      enddo !k
c
 103  format (i9,2i5,a/(33x,i3,2f8.3,f8.3,f8.2,f8.1))
cdiag if (i.eq.itest .and. j.eq.jtest)
cdiag. write (lp,103) nstep,itest+i0,jtest+j0,
cdiag. '  entering mxlayr:  temp    saln    dens    thkns    dpth',(k,
cdiag.ttem(k),ssal(k),dens(k)+thbase,delp(k)*qonem,pres(k+1)*qonem,k=1,kk)
c
c --- store 'old' t/s column integral in totem/tosal (diagnostic use only)
cdiag totem=0.
cdiag tosal=0.
cdiag do k=1,kk
cdiag   totem=totem+ttem(k)*delp(k)
cdiag   tosal=tosal+ssal(k)*delp(k)
cdiag enddo !k
c
      tdp=ttem(1)*delp(1)
      sdp=ssal(1)*delp(1)
      do ktr= 1,ntracr
        trdp(ktr)=delp(1)
      enddo !ktr
c
      kmxbot=1
      do k=2,kk
c
c --- watch for density decrease with depth (convective adjustment)
      tem=(tdp+ttem(k)*delp(k))/pres(k+1)
      sal=(sdp+ssal(k)*delp(k))/pres(k+1)
      rho=sig(tem,sal)-thbase
      if (locsig) then
        alfadt=0.5*(dsiglocdt(tem,sal,pres(k+1))+
     &              dsiglocdt(ttem(k),ssal(k),pres(k+1)))*(tem-ttem(k))
        betads=0.5*(dsiglocds(tem,sal,pres(k+1))+
     &              dsiglocds(ttem(k),ssal(k),pres(k+1)))*(sal-ssal(k))
        if(alfadt+betads.gt.0.0) then
          ttem(1)=tem
          ssal(1)=sal
          dens(1)=rho
          tdp=tdp+ttem(k)*delp(k)
          sdp=sdp+ssal(k)*delp(k)
          do ktr= 1,ntracr
            trdp(ktr)=trdp(ktr)+ttrc(k,ktr)*delp(k)
          enddo
          kmxbot=k
        end if
      else
        if (rho.le.dens(1)) then
          ttem(1)=tem
          ssal(1)=sal
          dens(1)=rho
          tdp=tdp+ttem(k)*delp(k)
          sdp=sdp+ssal(k)*delp(k)
          do ktr= 1,ntracr
            trdp(ktr)=trdp(ktr)+ttrc(k,ktr)*delp(k)
          enddo
          kmxbot=k
        end if
      endif
      if (k.gt.kmxbot) then
        exit
      endif
      enddo !k
c
      do k=2,kmxbot
      ttem(k)=ttem(1)
      ssal(k)=ssal(1)
      dens(k)=dens(1)
      do ktr= 1,ntracr
        ttrc(k,ktr)=ttrc(1,ktr)
      enddo !ktr

      enddo !k
c
c --- ----------------------------------------
c --- slab mixed layer entrainment/detrainment
c --- ----------------------------------------
c
c --- determine turb.kin.energy generation due to wind stirring
c --- ustar computed in subr. -thermf-
c --- buoyancy flux (m**2/sec**3), all fluxes into the ocean
c --- note: surface density increases (column is destabilized) if buoyfl < 0
      thkold=pres(kmxbot+1)
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
ccc   em=0.8*exp(-pres(2)/(50.*onem))   !   hadley centre choice (orig.: 1.25)
ccc   en=0.15                           !   hadley centre choice (orig.: 0.4)
ccc   thermg=0.5*((en+1.)*buoyfl+(en-1.)*abs(buoyfl))
ccc   turgen(i,j)=delt1*(2.*em*g*ustar3*qthref+thkold*thermg)*qthref**2
c
c --- find monin-obukhov length in case of receding mixed layer (turgen < 0).
c --- the monin-obukhov length is found by stipulating turgen = 0.
c
ccc   if (turgen(i,j).lt.0.) then
ccc     thknew=-2.*em*g*ustar3/min(-epsil,thref*thermg)
ccc   else
ccc     thknew=thkold
ccc   end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c --- option 2 :    g a s p a r   mixed-layer t.k.e. closure
c
      dpth=thkold*qonem
      ekminv=abs(corio(i,j))/max(epsil,ustar(i,j))
      obuinv=buoyfl/max(epsil,ustar3)
      ex=exp(min(50.,dpth*obuinv))
      alf1=ea1+ea2*max(1.,2.5*dpth*ekminv)*ex
      alf2=ea1+ea2*ex
      cp1=((1.-em5)*(alf1/alf2)+.5*em4)*athird
      cp3=max(0.,(em4*(em2+em3)-(alf1/alf2)*(em2+em3-em3*em5))*athird)
      ape=cp3*ustar3+cp1*dpth*buoyfl
c
      if(ape.lt.0.) then                                       ! detrainment
      turgen(i,j)=(g*delt1*qthref**3)*ape
      thknew=min(thkold,g*cp3/(thref*cp1*max(epsil,obuinv)))
c
      else                                                     ! entrainment
      cc4=2.*em4/(em1*em1) * alf1*alf1
      spe=(em2+em3)*ustar3+0.5*dpth*buoyfl
      turgen(i,j)=(g*delt1*qthref**3)*(sqrt((.5*ape-cp1*spe)**2
     .            +2.*cc4*ape*spe)-(.5*ape+cp1*spe))/(cc4-cp1)
      thknew=thkold
      end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c --- sum1,sum2 are used to evaluate pot.energy changes during entrainment
      sum1=dens(1)*thkold
      sum2=dens(1)*thkold**2
c
c --- find thknew in case of mx.layer deepening (turgen>0). store in -thknew-.
c --- entrain as many layers as needed to deplete -turgen-.
c
      do k=2,kk
      ka=k-1
      if (locsig) then
        if (k.eq.2) then
          densl(ka)=dens(ka)
        endif
        alfadt=0.5*
     &        (dsiglocdt(ttem(ka),ssal(ka),pres(k))+
     &         dsiglocdt(ttem(k ),ssal(k ),pres(k)))*(ttem(ka)-ttem(k))
        betads=0.5*
     &        (dsiglocds(ttem(ka),ssal(ka),pres(k))+
     &         dsiglocds(ttem(k ),ssal(k ),pres(k)))*(ssal(ka)-ssal(k))
        densl(k)=densl(ka)-alfadt-betads
        thet=densl(k)
      else
        thet=dens (k)
      endif
      if (pres(k+1).gt.thkold) then
        value=(2.*turgen(i,j)+thet*pres(k)**2-sum2)/
     &              max(epsil,thet*pres(k)   -sum1)
c --- stop iterating for 'thknew' as soon as thknew < k-th interface pressure
        if (value.lt.pres(k)) then
          value=thknew
        endif
c --- substitute 'thknew' for monin-obukhov length if mixed layer is deepening
        if (turgen(i,j).ge.0.) then
          thknew=value
        endif
c
        sum1=sum1+thet*(pres(k+1)   -max(pres(k),thkold)   )
        sum2=sum2+thet*(pres(k+1)**2-max(pres(k),thkold)**2)
      end if
      enddo !k
c
cdiag if (i.eq.itest .and. j.eq.jtest .and. turgen(i,j).lt.0.)
cdiag.  write (lp,'(i9,2i5,a,f8.2,1p,e13.3)') nstep,itest+i0,jtest+j0,
cdiag.  '  monin-obukhov length (m),turgen:',thknew*qonem,turgen(i,j)
c
c --- don't allow mixed layer to get too deep or too shallow.
ccc      q=max(bound1*onem,thkold*bound2)*delt1/86400.
ccc      thknew=min(pres(kk+1),max(thkmin*onem,delp(1),thknew,thkold-q))
      thknew=min(pres(kk+1),max(thkmin*onem,delp(1),thknew))
c
c --- integrate t/s over new mixed layer depth
c
      tdp=ttem(1)*delp(1)
      sdp=ssal(1)*delp(1)
c
      do k=2,kk
      if (pres(k).lt.thknew) then
        q=min(thknew,pres(k+1))-min(thknew,pres(k))
        tdp=tdp+ttem(k)*q
        sdp=sdp+ssal(k)*q
      end if
      enddo !k
c
cdiag if (i.eq.itest.and.j.eq.jtest) write (lp,'(i9,2i5,a,2f9.3)')
cdiag.  nstep,i+i0,j+j0,
cdiag.  '  old/new mixed layer depth:',thkold*qonem,thknew*qonem
c
c --- distribute thermohaline forcing over new mixed layer depth
c
      ttem(1)=(tdp+surflx(i,j)*delt1*g/spcifh)/thknew
      ssal(1)=(sdp+salflx(i,j)*delt1*g       )/thknew
      dens(1)=sig(ttem(1),ssal(1))-thbase
c
c --- homogenize water mass properties down to new mixed layer depth
c
      do k=2,kk
      if (pres(k+1).le.thknew) then
        ttem(k)=ttem(1)
        ssal(k)=ssal(1)
        dens(k)=dens(1)
        do ktr= 1,ntracr
          ttrc(k,ktr)=ttrc(1,ktr)
        enddo
      else if (pres(k).lt.thknew) then
c
cdiag     if (i.eq.itest.and.j.eq.jtest)
cdiag.    write (lp,'(i9,2i5,i3,a,3f9.3,25x,2f9.3)')
cdiag.    nstep,i+i0,j+j0,k,
cdiag.    '  p_k,thknew,p_k+1,t_1,t_k=',pres(k)*qonem,thknew*qonem,
cdiag.    pres(k+1)*qonem,ttem(1),ttem(k)
c
        ttem(k)=(ttem(1)*(thknew-pres(k))
     .          +ttem(k)*(pres(k+1)-thknew))/delp(k)
        ssal(k)=(ssal(1)*(thknew-pres(k))
     .          +ssal(k)*(pres(k+1)-thknew))/delp(k)
        dens(k)=sig(ttem(k),ssal(k))-thbase
        do ktr= 1,ntracr
          ttrc(k,ktr)=(ttrc(1,ktr)*(thknew-pres(k))
     &                +ttrc(k,ktr)*(pres(k+1)-thknew))/delp(k)
        enddo
      end if
      enddo !k
c
cdiag if (i.eq.itest .and. j.eq.jtest) write (lp,103) nstep,itest,jtest,
cdiag.'  exiting mxlayr:   temp    saln    dens    thkns    dpth',(k,
cdiag.ttem(k),ssal(k),dens(k)+thbase,delp(k)*qonem,pres(k+1)*qonem,k=1,kk)
c
c --- compare 'old' with 'new' t/s column integral (diagnostic use only)
c
cdiag if     (i.eq.itest .and. j.eq.jtest) then
cdiag   tndcyt=-totem
cdiag   tndcys=-tosal
cdiag   do k=1,kk
cdiag     tndcyt=tndcyt+ttem(k)*delp(k)
cdiag     tndcys=tndcys+ssal(k)*delp(k)
cdiag   enddo !k
cdiag   tndcyt=tndcyt-surflx(i,j)*delt1*g/spcifh
cdiag   tndcys=tndcys-salflx(i,j)*delt1*g
cdiag   write (lp,'(2i5,a,1p,2e16.8,e9.1)') i+i0,j+j0,
cdiag.  '  mxlyr temp.col.intgl.:',totem,tndcyt,tndcyt/totem
cdiag   write (lp,'(2i5,a,1p,2e16.8,e9.1)') i+i0,j+j0,
cdiag.  '  mxlyr saln.col.intgl.:',tosal,tndcys,tndcys/tosal
cdiag   write (lp,'(i9,2i5,3x,a,1p,3e10.2/22x,a,3e10.2)')
cdiag.  nstep,i+i0,j+j0,'total saln,srf.flux,tndcy:',tosal/g,
cdiag.  salflx*delt1,tndcys/g,'total temp,srf.flux,tndcy:',
cdiag.  totem/g,surflx*delt1,tndcyt*spcifh/g
cdiag endif
c
c --- put single column back into 3-d fields
      do k=1,kk
      temp(i,j,k,n)=ttem(k)
      saln(i,j,k,n)=ssal(k)
      th3d(i,j,k,n)=dens(k)
      do ktr= 1,ntracr
        tracer(i,j,k,n,ktr)=ttrc(k,ktr)
      enddo !ktr
      enddo !k
c
      dpmixl(i,j,n)=thknew
c
c --- fill mixed layer arrays
c
      dpbl(i,j)=dpmixl(i,j,n)
      tmix(i,j)=temp(i,j,1,n)
      smix(i,j)=saln(i,j,1,n)
      thmix(i,j)=th3d(i,j,1,n)

      endif !ip
      enddo !i
      return
      end
c
      subroutine mxkrtbbj(m,n, j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n, j
c
c --- hycom version 1.0 -- alternative slab mixed layer model
c --- single row, part B.
c
      real zup,zlo,s1,s2,s3,smax,smin,sup,slo,q
      integer i,k,ja,km
c
      real       small
      parameter (small=1.e-4)
c
c --- ---------------
c --- momentum mixing
c --- ---------------
c
c --- homogenize -u- down to new mixed layer depth
c
      ja=mod(j-2+jj,jj)+1
c
      do i=1,ii
      if (SEA_U) then
      klist(i,j)=-1
      util1(i,j)=max(dpu(i,j,1,n),.5*(dpmixl(i,j,n)+dpmixl(i-1,j,n)))
      uflux(i,j)=u(i,j,1,n)*dpu(i,j,1,n)
      util2(i,j)=dpu(i,j,1,n)
      pu(i,j,2)=dpu(i,j,1,n)
c
      do k=2,kk
      pu(i,j,k+1)=pu(i,j,k)+dpu(i,j,k,n)
      if (pu(i,j,k+1).le.util1(i,j)) then
        uflux(i,j)=uflux(i,j)+u(i,j,k,n)*dpu(i,j,k,n)
        util2(i,j)=util2(i,j)+          dpu(i,j,k,n)
      else if (pu(i,j,k).lt.util1(i,j)) then
c --- divide layer k into 2 sublayers. upper one belongs to mixed layer
        zup=util1(i,j)-pu(i,j,k)
        zlo=dpu(i,j,k,n)-zup
        s1=u(i,j,k-1,n)
        s2=u(i,j,k,  n)
        if (k.eq.kk .or. (k.lt.kk .and. dpu(i,j,k+1,n).lt.onemm)) then
          s3=2.*s2-s1
        else
          s3=u(i,j,k+1,n)
        end if
c --- define 'bounding box'
        smax=max(s1,s2,s3)
        smin=min(s1,s2,s3)
        if (s2.lt.smin+small .or. s2.gt.smax-small) then
          sup=s2
          slo=s2
        else
          slo=s3
          sup=(s2*dpu(i,j,k,n)-slo*zlo)/zup
          if (sup.gt.smin-small .and. sup.lt.smax+small) then
            go to 36
          endif
          sup=s1
          slo=(s2*dpu(i,j,k,n)-sup*zup)/zlo
          if (slo.gt.smin-small .and. slo.lt.smax+small) then
            go to 36
          endif
cdiag     write (lp,100)
cdiag.      nstep,i+i0,j+j0,'  possible',' error in unmixing u',
cdiag.      dpu(i,j,k,n)*qonem,zup*qonem,zlo*qonem,s1,s2,s3,
cdiag.      (s2*dpu(i,j,k,n)-slo*zlo)/zup,(s2*dpu(i,j,k,n)-sup*zup)/zlo
          sup=s2
          slo=s2
        end if
 36     continue
        uflux(i,j)=uflux(i,j)+sup*zup
        util2(i,j)=util2(i,j)+    zup
        util3(i,j)=u(i,j,k,n)
        u(i,j,k,n)=slo
        klist(i,j)=k
      end if
      enddo !k
 100  format (i9,2i5,2a,3f9.3/3f10.4,2(2x,2f10.4))
c
      u(i,j,1,n)=uflux(i,j)/util2(i,j)
c
      do k=2,kk
      q=max(0.,min(1.,(util1(i,j)-pu(i,j,k))/(dpu(i,j,k,n)+epsil)))
      if (q.eq.0. .and. k.eq.klist(i,j)) then
        u(i,j,k,n)=util3(i,j)
      else
        u(i,j,k,n)=u(i,j,1,n)*q+u(i,j,k,n)*(1.-q)
      end if
      enddo !k
      endif !iu
      enddo !i
c
c --- homogenize -v- down to new mixed layer depth
c
      do i=1,ii
      if (SEA_U) then
      klist(i,j)=-1
      util1(i,j)=max(dpv(i,j,1,n),.5*(dpmixl(i,j,n)+dpmixl(i,ja ,n)))
      vflux(i,j)=v(i,j,1,n)*dpv(i,j,1,n)
      util2(i,j)=dpv(i,j,1,n)
      pv(i,j,2)=dpv(i,j,1,n)
c
      do k=2,kk
      pv(i,j,k+1)=pv(i,j,k)+dpv(i,j,k,n)
      if (pv(i,j,k+1).le.util1(i,j)) then
        vflux(i,j)=vflux(i,j)+v(i,j,k,n)*dpv(i,j,k,n)
        util2(i,j)=util2(i,j)+          dpv(i,j,k,n)
      else if (pv(i,j,k).lt.util1(i,j)) then
c --- divide layer k into 2 sublayers. upper one belongs to mixed layer
        zup=util1(i,j)-pv(i,j,k)
        zlo=dpv(i,j,k,n)-zup
        s1=v(i,j,k-1,n)
        s2=v(i,j,k,  n)
        if (k.eq.kk .or. (k.lt.kk .and. dpv(i,j,k+1,n).lt.onemm)) then
          s3=2.*s2-s1
        else
          s3=v(i,j,k+1,n)
        end if
c --- define 'bounding box'
        smax=max(s1,s2,s3)
        smin=min(s1,s2,s3)
        if (s2.lt.smin+small .or. s2.gt.smax-small) then
          sup=s2
          slo=s2
        else
          slo=s3
          sup=(s2*dpv(i,j,k,n)-slo*zlo)/zup
          if (sup.gt.smin-small .and. sup.lt.smax+small) then
            go to 56
          endif
          sup=s1
          slo=(s2*dpv(i,j,k,n)-sup*zup)/zlo
          if (slo.gt.smin-small .and. slo.lt.smax+small) then
            go to 56
          endif
cdiag     write (lp,100)
cdiag.      nstep,i+i0,j+j0,'  possible',' error in unmixing v',
cdiag.      dpv(i,j,k,n)*qonem,zup*qonem,zlo*qonem,s1,s2,s3,
cdiag.      (s2*dpv(i,j,k,n)-slo*zlo)/zup,(s2*dpv(i,j,k,n)-sup*zup)/zlo
          sup=s2
          slo=s2
        end if
 56     vflux(i,j)=vflux(i,j)+sup*zup
        util2(i,j)=util2(i,j)+    zup
        util3(i,j)=v(i,j,k,n)
        v(i,j,k,n)=slo
        klist(i,j)=k
      end if
      enddo !k
c
      v(i,j,1,n)=vflux(i,j)/util2(i,j)
c
      do k=2,kk
      q=max(0.,min(1.,(util1(i,j)-pv(i,j,k))/(dpv(i,j,k,n)+epsil)))
      if (q.eq.0. .and. k.eq.klist(i,j)) then
        v(i,j,k,n)=util3(i,j)
      else
        v(i,j,k,n)=v(i,j,1,n)*q+v(i,j,k,n)*(1.-q)
      end if
      enddo !k
      endif !iv
      enddo !i
c
      return
      end
c
c> Revision history:
c>
c> May  2000 - conversion to SI units
c> May  2000 - changed dimensions of turgen in light of its use in loop 85
c> Oct  2000 - added mxkrtaaj and mxkrtabj to simplify OpenMP logic
c> Nov  2000 - added alternative slab mixed layer model (mxkrtb*)
c> May  2002 - buoyfl (into the ocean), calculated here
c> Aug  2011 - replaced wts[12] with ra2fac
c> Oct. 2013 - added jerlv0=-1 and calls to swfrml_ij
c> May  2014 - use land/sea masks (e.g. ip) to skip land
