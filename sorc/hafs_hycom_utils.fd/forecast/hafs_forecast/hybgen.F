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
      subroutine hybgen(m,n, hybgen_raflag)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_pipe       ! HYCOM debugging interface
c
c --- hycom version 1.0
      implicit none
c
      logical hybgen_raflag
      integer m,n
c
c --- ---------------------
c --- hybrid grid generator
c --- ---------------------
c
      logical, parameter :: lpipe_hybgen=.false.  !for debugging
c
      integer   i,j,k
      character text*12
c
 103  format (i9,2i5,a/(33x,i3,2f8.3,f8.3,f9.3,f9.2))
cdiag if (itest.gt.0 .and. jtest.gt.0) then
cdiag   write (lp,103) nstep,itest+i0,jtest+j0,
cdiag.  '  entering hybgen:  temp    saln    dens     thkns     dpth',
cdiag.  (k,temp(itest,jtest,k,n),saln(itest,jtest,k,n),
cdiag.  th3d(itest,jtest,k,n)+thbase,dp(itest,jtest,k,n)*qonem,
cdiag.  p(itest,jtest,k+1)*qonem,k=1,kk)
cdiag endif
c
      call xctilr(dpmixl( 1-nbdy,1-nbdy,  n),1, 1, 1,1, halo_ps)
c
!$OMP PARALLEL DO PRIVATE(j)
!$OMP&             SHARED(m,n)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        call hybgenaj(m,n, j)
      enddo
!$OMP END PARALLEL DO
c
c --- vertical momentum flux across moving interfaces (the s-dot term in the
c --- momentum equation) - required to locally conserve momentum when hybgen
c --- moves vertical coordinates first, store old interface pressures in
c --- -pu-, -pv-
c
!$OMP PARALLEL DO PRIVATE(j,i,k)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        do i=1,ii
          if (SEA_U) then
            pu(i,j,1)=0.0
            pu(i,j,2)=dpu(i,j,1,n)
          endif !iu
          if (SEA_V) then
            pv(i,j,1)=0.0
            pv(i,j,2)=dpv(i,j,1,n)
          endif !iv
        enddo !i
        do k=2,kk
          do i=1,ii
            if (SEA_U) then
              pu(i,j,k+1)=pu(i,j,k)+dpu(i,j,k,n)
            endif !iu
            if (SEA_V) then
              pv(i,j,k+1)=pv(i,j,k)+dpv(i,j,k,n)
            endif !iv
          enddo !i
        enddo !k
      enddo !j
!$OMP END PARALLEL DO
c
c --- update layer thickness at -u,v- points.
      call dpudpv(dpu(1-nbdy,1-nbdy,1,n),
     &            dpv(1-nbdy,1-nbdy,1,n),
     &            p,depthu,depthv, 0,0)
c
      if     (lpipe .and. lpipe_hybgen) then
c ---   compare two model runs.
c ---   exit (if any) in next call to pipe_compare_all
c
        call pipe_fatal_off
        do k= 1,kk+1
          write (text,'(a9,i3)') 'p      k=',k
          call pipe_compare(p( 1-nbdy,1-nbdy,k),ip,text)
          write (text,'(a9,i3)') 'pu     k=',k
          call pipe_compare(pu(1-nbdy,1-nbdy,k),iu,text)
          write (text,'(a9,i3)') 'pv     k=',k
          call pipe_compare(pv(1-nbdy,1-nbdy,k),iv,text)
        enddo
        do k= 1,kk
          write (text,'(a9,i3)') 'dp     k=',k
          call pipe_compare(dp( 1-nbdy,1-nbdy,k,n),ip,text)
          write (text,'(a9,i3)') 'dpu    k=',k
          call pipe_compare(dpu(1-nbdy,1-nbdy,k,n),iu,text)
          write (text,'(a9,i3)') 'dpv    k=',k
          call pipe_compare(dpv(1-nbdy,1-nbdy,k,n),iv,text)
        enddo
        call pipe_fatal_on
      endif
c
!$OMP PARALLEL DO PRIVATE(j)
!$OMP&             SHARED(n)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        call hybgenbj(n, j)  !update velocity at time level t+1
      enddo
!$OMP END PARALLEL DO
c
      if     (hybgen_raflag) then
c
c ---   Apply Robert-Asselin update to layer thickness at time level t
c
!$OMP   PARALLEL DO PRIVATE(j)
!$OMP&               SHARED(m,n)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          call hybgencj(m,n, j)
        enddo
!$OMP   END PARALLEL DO
c
!$OMP   PARALLEL DO PRIVATE(j,i,k)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
            if (SEA_U) then
              pu(i,j,1)=0.0
              pu(i,j,2)=dpu(i,j,1,m)
            endif !iu
            if (SEA_V) then
              pv(i,j,1)=0.0
              pv(i,j,2)=dpv(i,j,1,m)
            endif !iv
          enddo !i
          do k=2,kk
            do i=1,ii
              if (SEA_U) then
                pu(i,j,k+1)=pu(i,j,k)+dpu(i,j,k,m)
              endif !iu
              if (SEA_V) then
                pv(i,j,k+1)=pv(i,j,k)+dpv(i,j,k,m)
              endif !iv
            enddo !i
          enddo !k
        enddo !j
!$OMP   END PARALLEL DO
c
        call dpudpv(dpu(1-nbdy,1-nbdy,1,m),
     &              dpv(1-nbdy,1-nbdy,1,m),
     &              p,depthu,depthv, 0,0)
c
        if     (lpipe .and. lpipe_hybgen) then
c ---     compare two model runs.
          do k= 1,kk+1
            write (text,'(a9,i3)') 'p      k=',k
            call pipe_compare(p( 1-nbdy,1-nbdy,k),ip,text)
            write (text,'(a9,i3)') 'pu     k=',k
            call pipe_compare(pu(1-nbdy,1-nbdy,k),iu,text)
            write (text,'(a9,i3)') 'pv     k=',k
            call pipe_compare(pv(1-nbdy,1-nbdy,k),iv,text)
          enddo
          do k= 1,kk
            write (text,'(a9,i3)') 'dp     k=',k
            call pipe_compare(dp( 1-nbdy,1-nbdy,k,m),ip,text)
            write (text,'(a9,i3)') 'dpu    k=',k
            call pipe_compare(dpu(1-nbdy,1-nbdy,k,m),iu,text)
            write (text,'(a9,i3)') 'dpv    k=',k
            call pipe_compare(dpv(1-nbdy,1-nbdy,k,m),iv,text)
          enddo
        endif
c
!$OMP   PARALLEL DO PRIVATE(j)
!$OMP&               SHARED(m)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          call hybgenbj(m, j)  !update velocity at time level t
        enddo
!$OMP   END PARALLEL DO
      endif !hybgen_raflag
c
      return
      end subroutine hybgen

      subroutine hybgenaj(m,n,j )
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n, j
c
c --- --------------------------------------------
c --- hybrid grid generator, single j-row (part A).
c --- --------------------------------------------
c
      logical, parameter :: lunmix=.true.     !unmix a too light deepest layer
      logical, parameter :: lconserve=.false. !explicitly conserve each column
      integer, parameter :: ndebug_tracer=0   !tracer to debug, usually 0 (off)
c
      double precision asum(  mxtrcr+4,3)
      real             offset(mxtrcr+4)
c
      logical lcm(kdm)             !use PCM for some layers?
      real    s1d(kdm,mxtrcr+4),   !original scalar fields
     &        f1d(kdm,mxtrcr+4),   !final    scalar fields
     &        c1d(kdm,mxtrcr+4,3), !interpolation coefficients
     &        dpi( kdm),           !original layer thicknesses, >= dpthin
     &        dprs(kdm),           !original layer thicknesses
     &        pres(kdm+1),         !original layer interfaces
     &        prsf(kdm+1),         !final    layer interfaces
     &        qhrlx( kdm+1),       !relaxation coefficient, from qhybrlx
     &        dp0ij( kdm),         !minimum layer thickness
     &        dp0cum(kdm+1)        !minimum interface depth
      real    p_hat,p_hat0,p_hat2,p_hat3,hybrlx,
     &        delt,deltm,dels,delsm,q,qdep,qtr,qts,thkbop,
     &        zthk,dpthin
      integer i,k,ka,kp,ktr,fixall,fixlay,nums1d
      character*12 cinfo
c
      double precision, parameter ::   zp5=0.5    !for sign function
c
c --- c u s h i o n   function (from Bleck & Benjamin, 1992):
c --- if delp >= qqmx*dp0 >>  dp0, -cushn- returns -delp-
c --- if delp <= qqmn*dp0 << -dp0, -cushn- returns  -dp0-
c
      real       qqmn,qqmx,cusha,cushb
      parameter (qqmn=-4.0, qqmx=2.0)  ! shifted range
*     parameter (qqmn=-2.0, qqmx=4.0)  ! traditional range
*     parameter (qqmn=-4.0, qqmx=6.0)  ! somewhat wider range
      parameter (cusha=qqmn**2*(qqmx-1.0)/(qqmx-qqmn)**2)
      parameter (cushb=1.0/qqmn)
c
      real qq,cushn,delp,dp0
      include 'stmt_fns.h'
      qq(   delp,dp0)=max(qqmn, min(qqmx, delp/dp0))
      cushn(delp,dp0)=dp0*
     &                (1.0+cusha*(1.0-cushb*qq(delp,dp0))**2)*
     &                max(1.0, delp/(dp0*qqmx))
c
      dpthin = 0.001*onemm
      thkbop = thkbot*onem
      hybrlx = 1.0/qhybrlx
c
      if (mxlmy) then
        nums1d = ntracr + 4
      else
        nums1d = ntracr + 2
      endif
c
      if     (.not.isopcm) then
c ---   lcm the same for all points
        do k=1,nhybrd
          lcm(k) = .false.  !use same remapper for all layers
        enddo !k
        do k=nhybrd+1,kk
          lcm(k) = .true.   !purely isopycnal layers use PCM
        enddo !k
      endif
c
      do i=1,ii
      if (SEA_P) then
c
c --- terrain following starts at depth dpns and ends at depth dsns
      qdep = max( 0.0, min( 1.0,
     &                      (depths(i,j) - dsns)/
     &                      (dpns        - dsns)  ) )
c
      if     (qdep.lt.1.0) then
c ---   terrain following, qhrlx=1 and ignore dp00
        p(i,j, 1)=0.0
        dp0cum(1)=0.0
        qhrlx( 1)=1.0
        dp0ij( 1)=qdep*dp0k(1) + (1.0-qdep)*ds0k(1)
cdiag       if (i.eq.itest .and. j.eq.jtest) then
cdiag         k=1
cdiag         write (lp,*) 'qdep = ',qdep
cdiag         write (lp,'(a/i6,1x,4f9.3/a)')
cdiag.        '     k     dp0ij     ds0k     dp0k        p',
cdiag.        k,dp0ij(k)*qonem,ds0k(1)*qonem,dp0k(1)*qonem,
cdiag.                                 p(i,j,k)*qonem,
cdiag.        '     k     dp0ij    p-cum        p   dp0cum'
cdiag       endif !debug
        dp0cum(2)=dp0cum(1)+dp0ij(1)
        qhrlx( 2)=1.0
        p(i,j, 2)=p(i,j,1)+dp(i,j,1,n)
        do k=2,kk
          qhrlx( k+1)=1.0
          dp0ij( k)  =qdep*dp0k(k) + (1.0-qdep)*ds0k(k)
          dp0cum(k+1)=dp0cum(k)+dp0ij(k)
          p(i,j, k+1)=p(i,j,k)+dp(i,j,k,n)
cdiag         if (i.eq.itest .and. j.eq.jtest) then
cdiag           write (lp,'(i6,1x,4f9.3)')
cdiag.          k,dp0ij(k)*qonem,p(i,j,k)*qonem-dp0cum(k)*qonem,
cdiag.                           p(i,j,k)*qonem,dp0cum(k)*qonem
cdiag         endif !debug
        enddo !k
      else
c ---   not terrain following
        p(i,j, 1)=0.0
        dp0cum(1)=0.0
        qhrlx( 1)=1.0 !no relaxation in top layer
        dp0ij( 1)=dp0k(1)
cdiag       if (i.eq.itest .and. j.eq.jtest) then
cdiag         k=1
cdiag         write (lp,*) 'qdep = ',qdep
cdiag         write (lp,'(a/i6,1x,f9.3)')
cdiag.  '     k     dp0ij     dp0k        q    p-cum        p   dp0cum',
cdiag.        k,dp0ij(k)*qonem
cdiag       endif !debug
        dp0cum(2)=dp0cum(1)+dp0ij(1)
        qhrlx( 2)=1.0 !no relaxation in top layer
        p(i,j, 2)=p(i,j,1)+dp(i,j,1,n)
        do k=2,kk
c ---     q is dp0k(k) when in surface fixed coordinates
c ---     q is dp00i   when much deeper than surface fixed coordinates
          if     (dp0k(k).le.dp00i) then
            q  =      dp0k(k)
            qts=      0.0     !0 at dp0k
          else
            q  = max( dp00i,
     &                dp0k(k) * dp0k(k)/
     &                          max( dp0k( k),
     &                               p(i,j,k)-dp0cum(k) ) )
            qts= 1.0 - (q-dp00i)/(dp0k(k)-dp00i)  !0 at dp0k, 1 at dp00i
          endif
          qhrlx( k+1)=1.0/(1.0 + qts*(hybrlx-1.0))  !1 at  dp0k, qhybrlx at dp00i
          dp0ij( k)  =min( q, dp0k(k) )
          dp0cum(k+1)=dp0cum(k)+dp0ij(k)
          p(i,j, k+1)=p(i,j,k)+dp(i,j,k,n)
cdiag         if (i.eq.itest .and. j.eq.jtest) then
cdiag           write (lp,'(i6,1x,6f9.3)')
cdiag.          k,dp0ij(k)*qonem,dp0k(k)*qonem,q*qonem,
cdiag.            p(i,j,k)*qonem-dp0cum(k)*qonem,
cdiag.            p(i,j,k)*qonem,dp0cum(k)*qonem
cdiag         endif !debug
        enddo !k
      endif !qdep<1:else
c
c --- identify the current fixed coordinate layers
      fixlay = 1  !layer 1 always fixed
      do k= 2,nhybrd
        if     (dp0cum(k).ge.topiso(i,j)) then
          exit  !layers k to nhybrd might be isopycnal
        endif
c ---   top of layer is above topiso, i.e. always fixed coordinate layer
        qhrlx(k+1) = 1.0  !no relaxation in fixed layers
        fixlay     = fixlay+1
      enddo !k
cdiag      if (i.eq.itest .and. j.eq.jtest) then
cdiag        write(lp,'(a,i3)')
cdiag     &        'hybgen, always-fixed coordinate layers: 1 to ',
cdiag     &        fixlay
cdiag        call flush(lp)
cdiag      endif !debug
c
      fixall = fixlay
      do k= fixall+1,nhybrd
cdiag        if (i.eq.itest .and. j.eq.jtest) then
cdiag          write (lp,'(i6,1x,2f9.3)')
cdiag     .      k,p(i,j,k+1)*qonem,dp0cum(k+1)*qonem
cdiag          call flush(lp)
cdiag        endif !debug
        if     (p(i,j,k+1).gt.dp0cum(k+1)+0.1*dp0ij(k)) then
          if     (fixlay.gt.fixall) then
c ---       should the previous layer remain fixed?
            if     (p(i,j,k).gt.dp0cum(k)) then
              fixlay = fixlay-1
            endif
          endif
          exit  !layers k to nhybrd might be isopycnal
        endif
c ---   sometimes fixed coordinate layer
        qhrlx(k) = 1.0  !no relaxation in fixed layers
        fixlay   = fixlay+1
      enddo !k
cdiag      if (i.eq.itest .and. j.eq.jtest) then
cdiag        write(lp,'(a,i3)')
cdiag     &        'hybgen,        fixed coordinate layers: 1 to ',
cdiag     &        fixlay
cdiag        call flush(lp)
cdiag      endif !debug
c
cdiag      if (i.eq.itest .and. j.eq.jtest) then
cdiag        write (lp,'(a/(i6,1x,2f8.3,2f9.3,f9.3))')
cdiag     .  'hybgen:   thkns  minthk     dpth  mindpth   hybrlx',
cdiag     .  (k,dp(i,j,k,n)*qonem,   dp0ij(k)*qonem,
cdiag     .      p(i,j,k+1)*qonem,dp0cum(k+1)*qonem,
cdiag     .      1.0/qhrlx(k+1),
cdiag     .   k=1,kk)
cdiag      endif !debug
c
c --- identify the deepest layer kp with significant thickness (> dpthin)
c
      kp = 2  !minimum allowed value
      do k=kk,3,-1
        if (p(i,j,k+1)-p(i,j,k).ge.dpthin) then
          kp=k
          exit
        endif
      enddo !k
c
cdiag      if (i.eq.itest .and. j.eq.jtest) then
cdiag        write(lp,'(a,i3)')
cdiag     &        'hybgen, deepest inflated layer:',kp
cdiag        call flush(lp)
cdiag      endif !debug
c
      k  = kp  !at least 2
      ka = max(k-2,1)  !k might be 2
c
      if     (k.gt.fixlay+1 .and. qdep.eq.1.0 .and.  !layer not fixed depth
     &        p(i,j,k)-p(i,j,k-1).ge.dpthin   .and.  !layer above not too thin
     &        theta(i,j,k)-epsil.gt.th3d(i,j,k,n) .and.
     &         th3d(i,j,k-1,n)  .gt.th3d(i,j,k,n) .and.
     &         th3d(i,j,ka, n)  .gt.th3d(i,j,k,n)      ) then
c
c ---   water in the deepest inflated layer with significant thickness
c ---   (kp) is too light, and it is lighter than the two layers above.
c ---
c ---   this should only occur when relaxing or nudging layer thickness
c ---   and is a bug (bad interaction with tsadvc) even in those cases
c ---
c ---   entrain the entire layer into the one above
c---    note the double negative in T=T-q*(T-T'), equiv. to T=T+q*(T'-T)
        q = (p(i,j,k+1)-p(i,j,k))/(p(i,j,k+1)-p(i,j,k-1))
        if     (hybflg.eq.0) then  !T&S
          temp(i,j,k-1,n)=temp(i,j,k-1,n)-q*(temp(i,j,k-1,n) -
     &                                       temp(i,j,k,  n)  )
          saln(i,j,k-1,n)=saln(i,j,k-1,n)-q*(saln(i,j,k-1,n) -
     &                                       saln(i,j,k,  n)  )
          th3d(i,j,k-1,n)=sig(temp(i,j,k-1,n),saln(i,j,k-1,n))-thbase
        elseif (hybflg.eq.1) then  !th&S
          th3d(i,j,k-1,n)=th3d(i,j,k-1,n)-q*(th3d(i,j,k-1,n) -
     &                                       th3d(i,j,k,  n)  )
          saln(i,j,k-1,n)=saln(i,j,k-1,n)-q*(saln(i,j,k-1,n) -
     &                                       saln(i,j,k,  n)  )
          temp(i,j,k-1,n)=tofsig(th3d(i,j,k-1,n)+thbase,saln(i,j,k-1,n))
        elseif (hybflg.eq.2) then  !th&T
          th3d(i,j,k-1,n)=th3d(i,j,k-1,n)-q*(th3d(i,j,k-1,n) -
     &                                       th3d(i,j,k,  n)  )
          temp(i,j,k-1,n)=temp(i,j,k-1,n)-q*(temp(i,j,k-1,n) -
     &                                       temp(i,j,k,  n)  )
          saln(i,j,k-1,n)=sofsig(th3d(i,j,k-1,n)+thbase,temp(i,j,k-1,n))
        endif
          if (ndebug_tracer.gt.0 .and. ndebug_tracer.le.ntracr .and.
     &        i.eq.itest .and. j.eq.jtest) then
            ktr = ndebug_tracer
            write(lp,'(a,i3,f6.3,f9.4)')
     &        'hybgen, 11(+):',
     &        k-1,0.0,tracer(i,j,k-1,n,ktr)
            call flush(lp)
          endif !debug_tracer
        do ktr= 1,ntracr
          tracer(i,j,k-1,n,ktr)=tracer(i,j,k-1,n,ktr)-
     &                               q*(tracer(i,j,k-1,n,ktr) -
     &                                  tracer(i,j,k,  n,ktr)  )
        enddo !ktr
          if (ndebug_tracer.gt.0 .and. ndebug_tracer.le.ntracr .and.
     &        i.eq.itest .and. j.eq.jtest) then
            ktr = ndebug_tracer
            write(lp,'(a,i3,f6.3,f9.4)')
     &        'hybgen, 11(+):',
     &        k-1,q,tracer(i,j,k-1,n,ktr)
            write(lp,'(a,i3,f6.3,f9.4)')
     &        'hybgen, 11(+):',
     &        k,q,tracer(i,j,k,n,ktr)
            write(lp,'(a,i3)')
     &            'hybgen, deepest inflated layer:',kp
            call flush(lp)
          endif !debug_tracer
        if (mxlmy) then
          q2( i,j,k-1,n)=q2( i,j,k-1,n)-
     &                     q*(q2( i,j,k-1,n)-q2( i,j,k,n))
          q2l(i,j,k-1,n)=q2l(i,j,k-1,n)-
     &                     q*(q2l(i,j,k-1,n)-q2l(i,j,k,n))
        endif
c ---   entrained the entire layer into the one above, so now kp=kp-1
        p(i,j,k) = p(i,j,k+1)
        kp = k-1
cdiag        if (i.eq.itest .and. j.eq.jtest) then
cdiag          write(lp,'(a,i3,f6.3,5f8.3)')
cdiag     &      'hybgen, 11(+):',
cdiag     &      k-1,q,temp(i,j,k-1,n),saln(i,j,k-1,n),
cdiag     &          th3d(i,j,k-1,n)+thbase,theta(i,j,k-1)+thbase
cdiag          write(lp,'(a,i3)')
cdiag     &          'hybgen, deepest inflated layer:',kp
cdiag          call flush(lp)
cdiag        endif !debug
      elseif (k.gt.fixlay+1 .and. qdep.eq.1.0 .and.  !layer not fixed depth
     &        p(i,j,k)-p(i,j,k-1).ge.dpthin   .and.  !layer above not too thin
     &        theta(i,j,k)-epsil.gt.th3d(i,j,k,n) .and.
     &         th3d(i,j,k-1,n)  .gt.th3d(i,j,k,n)      ) then
c
c ---   water in the deepest inflated layer with significant thickness
c ---   (kp) is too light, and it is lighter than the layer above.
c ---
c ---   swap the entire layer with the one above.
cdiag        if (i.eq.itest .and. j.eq.jtest) then
cdiag          write(lp,'(a,i3,f8.5,5f10.5)')
cdiag     &      'hybgen, original:',
cdiag     &      k-1,0.0,temp(i,j,k-1,n),saln(i,j,k-1,n),
cdiag     &          th3d(i,j,k-1,n)+thbase,theta(i,j,k-1)+thbase
cdiag          write(lp,'(a,i3,f8.5,5f10.5)')
cdiag     &      'hybgen, original:',
cdiag     &      k,0.0,temp(i,j,k,  n),saln(i,j,k,  n),
cdiag     &          th3d(i,j,k,  n)+thbase,theta(i,j,k  )+thbase
cdiag        endif !debug
        if     (p(i,j,k+1)-p(i,j,k).le.p(i,j,k)-p(i,j,k-1)) then
c ---     bottom layer is thinner, take entire bottom layer
c---      note the double negative in T=T-q*(T-T'), equiv. to T=T+q*(T'-T)
          s1d(k-1,1) = temp(i,j,k-1,n)
          s1d(k-1,2) = saln(i,j,k-1,n)
          s1d(k-1,3) = th3d(i,j,k-1,n)
          q = (p(i,j,k+1)-p(i,j,k))/(p(i,j,k)-p(i,j,k-1))  !<=1.0
          if     (hybflg.eq.0) then  !T&S
            temp(i,j,k-1,n)=temp(i,j,k-1,n)-q*(temp(i,j,k-1,n) -
     &                                         temp(i,j,k,  n)  )
            saln(i,j,k-1,n)=saln(i,j,k-1,n)-q*(saln(i,j,k-1,n) -
     &                                         saln(i,j,k,  n)  )
            th3d(i,j,k-1,n)=sig(temp(i,j,k-1,n),saln(i,j,k-1,n))-thbase
          elseif (hybflg.eq.1) then  !th&S
            th3d(i,j,k-1,n)=th3d(i,j,k-1,n)-q*(th3d(i,j,k-1,n) -
     &                                         th3d(i,j,k,  n)  )
            saln(i,j,k-1,n)=saln(i,j,k-1,n)-q*(saln(i,j,k-1,n) -
     &                                         saln(i,j,k,  n)  )
            temp(i,j,k-1,n)=tofsig(th3d(i,j,k-1,n)+thbase,
     &                             saln(i,j,k-1,n))
          elseif (hybflg.eq.2) then  !th&T
            th3d(i,j,k-1,n)=th3d(i,j,k-1,n)-q*(th3d(i,j,k-1,n) -
     &                                         th3d(i,j,k,  n)  )
            temp(i,j,k-1,n)=temp(i,j,k-1,n)-q*(temp(i,j,k-1,n) -
     &                                         temp(i,j,k,  n)  )
            saln(i,j,k-1,n)=sofsig(th3d(i,j,k-1,n)+thbase,
     &                             temp(i,j,k-1,n))
          endif
          temp(i,j,k,n) = s1d(k-1,1)
          saln(i,j,k,n) = s1d(k-1,2)
          th3d(i,j,k,n) = s1d(k-1,3)
          do ktr= 1,ntracr
            s1d(k-1,2+ktr)        = tracer(i,j,k-1,n,ktr)
            tracer(i,j,k-1,n,ktr) = tracer(i,j,k-1,n,ktr)-
     &                                   q*(tracer(i,j,k-1,n,ktr) -
     &                                      tracer(i,j,k,  n,ktr)  )
            tracer(i,j,k,  n,ktr) = s1d(k-1,2+ktr)
          enddo !ktr
          if (mxlmy) then
            s1d(k-1,ntracr+3) = q2( i,j,k-1,n)
            s1d(k-1,ntracr+4) = q2l(i,j,k-1,n)
            q2( i,j,k-1,n)    = q2( i,j,k-1,n)-
     &                            q*(q2( i,j,k-1,n)-q2( i,j,k,n))
            q2l(i,j,k-1,n)    = q2l(i,j,k-1,n)-
     &                            q*(q2l(i,j,k-1,n)-q2l(i,j,k,n))
            q2( i,j,k,  n)    = s1d(k-1,ntracr+3)
            q2l(i,j,k,  n)    = s1d(k-1,ntracr+4)
          endif
        else
c ---     bottom layer is thicker, take entire layer above
          s1d(k,1) = temp(i,j,k,n)
          s1d(k,2) = saln(i,j,k,n)
          s1d(k,3) = th3d(i,j,k,n)
          q = (p(i,j,k)-p(i,j,k-1))/(p(i,j,k+1)-p(i,j,k))  !<1.0
          if     (hybflg.eq.0) then  !T&S
            temp(i,j,k,n)=temp(i,j,k,n)+q*(temp(i,j,k-1,n) -
     &                                     temp(i,j,k,  n)  )
            saln(i,j,k,n)=saln(i,j,k,n)+q*(saln(i,j,k-1,n) -
     &                                       saln(i,j,k,  n)  )
            th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
          elseif (hybflg.eq.1) then  !th&S
            th3d(i,j,k,n)=th3d(i,j,k,n)+q*(th3d(i,j,k-1,n) -
     &                                     th3d(i,j,k,  n)  )
            saln(i,j,k,n)=saln(i,j,k,n)+q*(saln(i,j,k-1,n) -
     &                                     saln(i,j,k,  n)  )
            temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,saln(i,j,k,n))
          elseif (hybflg.eq.2) then  !th&T
            th3d(i,j,k,n)=th3d(i,j,k,n)+q*(th3d(i,j,k-1,n) -
     &                                     th3d(i,j,k,  n)  )
            temp(i,j,k,n)=temp(i,j,k,n)+q*(temp(i,j,k-1,n) -
     &                                     temp(i,j,k,  n)  )
            saln(i,j,k,n)=sofsig(th3d(i,j,k,n)+thbase,temp(i,j,k,n))
          endif
          temp(i,j,k-1,n) = s1d(k,1)
          saln(i,j,k-1,n) = s1d(k,2)
          th3d(i,j,k-1,n) = s1d(k,3)
          do ktr= 1,ntracr
            s1d(k,2+ktr)          = tracer(i,j,k,n,ktr)
            tracer(i,j,k,  n,ktr) = tracer(i,j,k,n,ktr)+
     &                                   q*(tracer(i,j,k-1,n,ktr) -
     &                                      tracer(i,j,k,  n,ktr)  )
            tracer(i,j,k-1,n,ktr) = s1d(k,2+ktr)
          enddo !ktr
          if (mxlmy) then
            s1d(k,ntracr+3)   = q2( i,j,k,n)
            s1d(k,ntracr+4)   = q2l(i,j,k,n)
            q2( i,j,k,  n)    = q2( i,j,k,n)-
     &                            q*(q2( i,j,k-1,n)-q2( i,j,k,n))
            q2l(i,j,k,  n)    = q2l(i,j,k,n)-
     &                            q*(q2l(i,j,k-1,n)-q2l(i,j,k,n))
            q2( i,j,k-1,n)    = s1d(k,ntracr+3)
            q2l(i,j,k-1,n)    = s1d(k,ntracr+4)
          endif
        endif !bottom too light
cdiag        if (i.eq.itest .and. j.eq.jtest) then
cdiag          write(lp,'(a,i3,f8.5,5f10.5)')
cdiag     &      'hybgen, overturn:',
cdiag     &      k-1,q,temp(i,j,k-1,n),saln(i,j,k-1,n),
cdiag     &          th3d(i,j,k-1,n)+thbase,theta(i,j,k-1)+thbase
cdiag          write(lp,'(a,i3,f8.5,5f10.5)')
cdiag     &      'hybgen, overturn:',
cdiag     &      k,  q,temp(i,j,k,  n),saln(i,j,k,  n),
cdiag     &          th3d(i,j,k,  n)+thbase,theta(i,j,k  )+thbase
cdiag          call flush(lp)
cdiag        endif !debug
      endif
c
      k  = kp  !at least 2
      ka = max(k-2,1)  !k might be 2
c
      if     (lunmix        .and. !usually .true.
     &        k.gt.fixlay+1 .and. qdep.eq.1.0 .and.  !layer not fixed depth
     &        p(i,j,k)-p(i,j,k-1).ge.dpthin   .and.  !layer above not too thin
     &         theta(i,j,k)-epsil.gt.th3d(i,j,k,  n) .and.
     &         theta(i,j,k-1)    .lt.th3d(i,j,k,  n) .and.
     &     abs(theta(i,j,k-1)-       th3d(i,j,k-1,n)).lt.hybiso .and.
     &        ( th3d(i,j,k,n)-       th3d(i,j,k-1,n)).gt.
     &        (theta(i,j,k)  -      theta(i,j,k-1)  )*0.001  ) then
c
c ---   water in the deepest inflated layer with significant thickness
c ---   (kp) is too light, with the layer above near-isopycnal
c ---
c ---   split layer into 2 sublayers, one near the desired density
c ---   and one exactly matching the T&S properties of layer k-1.
c ---   To prevent "runaway" T or S, the result satisfies either
c ---     abs(T.k - T.k-1) <= abs(T.k-N - T.k-1) or
c ---     abs(S.k - S.k-1) <= abs(S.k-N - S.k-1) where
c ---     th3d.k-1 - th3d.k-N is at least theta(k-1) - theta(k-2)
c ---   It is also limited to a 50% change in layer thickness.
c
cdiag        if (i.eq.itest .and. j.eq.jtest) then
cdiag          write(lp,'(a,i3)')
cdiag     &      'hybgen, deepest inflated layer too light   (stable):',k
cdiag          call flush(lp)
cdiag        endif !debug
c
        ka = 1
        do ktr= k-2,2,-1
          if     ( th3d(i,j,k-1,n)- th3d(i,j,ktr,n).ge.
     &            theta(i,j,k-1)  -theta(i,j,k-2)     ) then
            ka = ktr  !usually k-2
            exit
          endif
        enddo !ktr
c
        delsm=abs(saln(i,j,ka, n)-saln(i,j,k-1,n))
        dels =abs(saln(i,j,k-1,n)-saln(i,j,k,  n))
        deltm=abs(temp(i,j,ka, n)-temp(i,j,k-1,n))
        delt =abs(temp(i,j,k-1,n)-temp(i,j,k,  n))
c ---   sanity check on deltm and delsm
        q=min(temp(i,j,ka, n),temp(i,j,k-1,n),temp(i,j,k,n))
        if     (q.gt. 6.0) then
          deltm=min( deltm,  6.0*(theta(i,j,k)-theta(i,j,k-1)) )
        else  !(q.le. 6.0)
          deltm=min( deltm, 10.0*(theta(i,j,k)-theta(i,j,k-1)) )
        endif
        delsm=min( delsm, 1.3*(theta(i,j,k)-theta(i,j,k-1)) )
        qts=0.0
        if     (delt.gt.epsil) then
          qts=max(qts, (min(deltm, 2.0*delt)-delt)/delt)  ! qts<=1.0
        endif
        if     (dels.gt.epsil) then
          qts=max(qts, (min(delsm, 2.0*dels)-dels)/dels)  ! qts<=1.0
        endif
        q=(theta(i,j,k)-th3d(i,j,k,  n))/
     &    (theta(i,j,k)-th3d(i,j,k-1,n))
        q=min(q,qts/(1.0+qts))  ! upper sublayer <= 50% of total
        q=qhrlx(k)*q
c ---   qhrlx is relaxation coefficient (inverse baroclinic time steps)
        p_hat=q*(p(i,j,k+1)-p(i,j,k))
        p(i,j,k)=p(i,j,k)+p_hat
        if     (hybflg.eq.0) then  !T&S
          temp(i,j,k,n)=temp(i,j,k,n)+(q/(1.0-q))*(temp(i,j,k,n)  -
     &                                             temp(i,j,k-1,n) )
          saln(i,j,k,n)=saln(i,j,k,n)+(q/(1.0-q))*(saln(i,j,k,n)  -
     &                                             saln(i,j,k-1,n) )
          th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
        elseif (hybflg.eq.1) then  !th&S
          th3d(i,j,k,n)=th3d(i,j,k,n)+(q/(1.0-q))*(th3d(i,j,k,n)  -
     &                                             th3d(i,j,k-1,n) )
          saln(i,j,k,n)=saln(i,j,k,n)+(q/(1.0-q))*(saln(i,j,k,n)  -
     &                                             saln(i,j,k-1,n) )
          temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,saln(i,j,k,n))
        elseif (hybflg.eq.2) then  !th&T
          th3d(i,j,k,n)=th3d(i,j,k,n)+(q/(1.0-q))*(th3d(i,j,k,n)  -
     &                                             th3d(i,j,k-1,n) )
          temp(i,j,k,n)=temp(i,j,k,n)+(q/(1.0-q))*(temp(i,j,k,n)  -
     &                                             temp(i,j,k-1,n) )
          saln(i,j,k,n)=sofsig(th3d(i,j,k,n)+thbase,temp(i,j,k,n))
        endif
        if     (ntracr.gt.0 .and. p_hat.ne.0.0) then
            if (ndebug_tracer.gt.0 .and. ndebug_tracer.le.ntracr .and.
     &          i.eq.itest .and. j.eq.jtest) then
              ktr = ndebug_tracer
              write(lp,'(a,i3,f6.3,f9.4)')
     &          'hybgen, 10(+):',
     &          k-1,0.0,tracer(i,j,k-1,n,ktr)
              call flush(lp)
            endif !debug_tracer
c ---     fraction of new upper layer from old lower layer
          qtr=p_hat/max(p_hat,p(i,j,k)-p(i,j,k-1))  !between 0 and 1
          do ktr= 1,ntracr
            if     (trcflg(ktr).eq.2) then !temperature tracer
              tracer(i,j,k,n,ktr)=tracer(i,j,k,n,ktr)+
     &                           (q/(1.0-q))*(tracer(i,j,k,  n,ktr)-
     &                                        tracer(i,j,k-1,n,ktr))
            else !standard tracer - not split into two sub-layers
              tracer(i,j,k-1,n,ktr)=tracer(i,j,k-1,n,ktr)+
     &                                   qtr*(tracer(i,j,k,  n,ktr)-
     &                                        tracer(i,j,k-1,n,ktr))
cdiag              if (i.eq.itest .and. j.eq.jtest) then
cdiag                write(lp,'(a,i4,i3,5e12.3)')
cdiag     &            'hybgen, 10(+):',
cdiag     &            k,ktr,p_hat,p(i,j,k),p(i,j,k-1),
cdiag     &            qtr,tracer(i,j,k-1,n,ktr)
cdiag                call flush(lp)
cdiag              endif !debug
            endif !trcflg
          enddo !ktr
            if (ndebug_tracer.gt.0 .and. ndebug_tracer.le.ntracr .and.
     &          i.eq.itest .and. j.eq.jtest) then
              ktr = ndebug_tracer
              write(lp,'(a,i3,f6.3,f9.4)')
     &          'hybgen, 10(+):',
     &          k-1,qtr,tracer(i,j,k-1,n,ktr)
              write(lp,'(a,i3,f6.3,f9.4)')
     &          'hybgen, 10(+):',
     &          k,qtr,tracer(i,j,k,n,ktr)
              write(lp,'(a,i3)')
     &              'hybgen, deepest inflated layer:',kp
              call flush(lp)
            endif !debug_tracer
        endif !tracers
        if (mxlmy .and. p_hat.ne.0.0) then
c ---     fraction of new upper layer from old lower layer
          qtr=p_hat/max(p_hat,p(i,j,k)-p(i,j,k-1))  !between 0 and 1
          q2( i,j,k-1,n)=q2( i,j,k-1,n)+
     &                     qtr*(q2( i,j,k,n)-q2( i,j,k-1,n))
          q2l(i,j,k-1,n)=q2l(i,j,k-1,n)+
     &                     qtr*(q2l(i,j,k,n)-q2l(i,j,k-1,n))
cdiag              if (i.eq.itest .and. j.eq.jtest) then
cdiag               write(lp,'(a,i4,i3,6e12.3)')
cdiag     &            'hybgen, 10(+):',
cdiag     &            k,0,p_hat,p(i,j,k)-p(i,j,k-1),p(i,j,k+1)-p(i,j,k),
cdiag     &            qtr,q2(i,j,k-1,n),q2l(i,j,k-1,n)
cdiag               call flush(lp)
cdiag              endif !debug
        endif
cdiag        if (i.eq.itest .and. j.eq.jtest) then
cdiag          write(lp,'(a,i3,f6.3,5f8.3)')
cdiag     &      'hybgen, 10(+):',
cdiag     &      k,q,temp(i,j,k,n),saln(i,j,k,n),
cdiag     &          th3d(i,j,k,n)+thbase,theta(i,j,k)+thbase
cdiag          call flush(lp)
cdiag        endif !debug
cdiag        if (i.eq.itest .and. j.eq.jtest) then
cdiag          write(lp,'(a,i3,f6.3,5f8.3)')
cdiag     &      'hybgen, 10(-):',
cdiag     &      k,0.0,temp(i,j,k,n),saln(i,j,k,n),
cdiag     &          th3d(i,j,k,n)+thbase,theta(i,j,k)+thbase
cdiag          call flush(lp)
cdiag        endif !debug
      endif !too light
c
c --- massless or near-massless (thickness < dpthin) layers
c
      do k=kp+1,kk
        if (k.le.nhybrd) then
c ---     fill thin and massless layers on sea floor with fluid from above
          th3d(i,j,k,n)=th3d(i,j,k-1,n)
          saln(i,j,k,n)=saln(i,j,k-1,n)
          temp(i,j,k,n)=temp(i,j,k-1,n)
        elseif (th3d(i,j,k,n).ne.theta(i,j,k)) then
          if (hybflg.ne.2) then
c ---       fill with saln from above
            th3d(i,j,k,n)=max(theta(i,j,k), th3d(i,j,k-1,n))
            saln(i,j,k,n)=saln(i,j,k-1,n)
            temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,saln(i,j,k,n))
            saln(i,j,k,n)=sofsig(th3d(i,j,k,n)+thbase,temp(i,j,k,n))
          else
c ---       fill with temp from above
            th3d(i,j,k,n)=max(theta(i,j,k), th3d(i,j,k-1,n))
            temp(i,j,k,n)=temp(i,j,k-1,n)
            saln(i,j,k,n)=sofsig(th3d(i,j,k,n)+thbase,temp(i,j,k,n))
          endif
        endif
        do ktr= 1,ntracr
          tracer(i,j,k,n,ktr)=tracer(i,j,k-1,n,ktr)
        enddo !ktr
            if (ndebug_tracer.gt.0 .and. ndebug_tracer.le.ntracr .and.
     &          i.eq.itest .and. j.eq.jtest) then
              ktr = ndebug_tracer
              write(lp,'(a,i3,f9.4)')
     &          'hybgen, massless:',
     &          k,tracer(i,j,k,n,ktr)
              call flush(lp)
            endif !debug_tracer
        if (mxlmy) then
          q2 (i,j,k,n)=q2( i,j,k-1,n)
          q2l(i,j,k,n)=q2l(i,j,k-1,n)
        endif
      enddo !k
c
c --- store one-dimensional arrays of -temp-, -saln-, and -p- for the 'old'
c --- vertical grid before attempting to restore isopycnic conditions
      pres(1)=p(i,j,1)
      do k=1,kk
        if     (hybflg.eq.0) then  !T&S
          s1d(k,1) = temp(i,j,k,n)
          s1d(k,2) = saln(i,j,k,n)
        elseif (hybflg.eq.1) then  !th&S
          s1d(k,1) = th3d(i,j,k,n)
          s1d(k,2) = saln(i,j,k,n)
        elseif (hybflg.eq.2) then  !th&T
          s1d(k,1) = th3d(i,j,k,n)
          s1d(k,2) = temp(i,j,k,n)
        endif
        do ktr= 1,ntracr
          s1d(k,2+ktr) = tracer(i,j,k,n,ktr)
        enddo !ktr
        if (mxlmy) then
          s1d(k,ntracr+3) = q2( i,j,k,n)
          s1d(k,ntracr+4) = q2l(i,j,k,n)
        endif
        pres(k+1)=p(i,j,k+1)
        dprs(k)  =pres(k+1)-pres(k)
        dpi( k)  =max(dprs(k),dpthin)
c
        if     (isopcm) then
          if     (k.le.fixlay) then
            lcm(k) = .false.  !fixed layers are never PCM
          else
c ---       thin and isopycnal layers remapped with PCM.
            lcm(k) = k.gt.nhybrd
     &               .or. dprs(k).le.dpthin
     &               .or. abs(th3d(i,j,k,n)-theta(i,j,k)).lt.hybiso
          endif !k<=fixlay:else
        endif !isopcm
      enddo !k
c
c --- try to restore isopycnic conditions by moving layer interfaces
c --- qhrlx(k) are relaxation coefficients (inverse baroclinic time steps)
c
      if (fixlay.ge.1) then
c
c ---   maintain constant thickness, layer k = 1
        k=1
        p_hat=p(i,j,k)+dp0ij(k)
        p(i,j,k+1)=p_hat
        do k=2,kk
          if     (p(i,j,k+1).ge.p_hat) then
            exit  ! usually get here quickly
          endif
          p(i,j,k+1)=p_hat
        enddo !k
      endif
c
      do k=2,nhybrd
c
cdiag   if (i.eq.itest .and. j.eq.jtest) then
cdiag     write(cinfo,'(a9,i2.2,1x)') '  do 88 k=',k
cdiag     write(lp,'(i9,2i5,a,a)') nstep,itest+i0,jtest+j0,
cdiag.      cinfo,':     othkns    odpth    nthkns    ndpth'
cdiag     do ka=1,kk
cdiag       if     (pres(ka+1).eq.p(itest,jtest,ka+1) .and.
cdiag.              pres(ka  ).eq.p(itest,jtest,ka  )      ) then
cdiag         write(lp,'(i9,8x,a,a,i3,f10.3,f9.3)')
cdiag.         nstep,cinfo,':',ka,
cdiag.        (pres(ka+1)-
cdiag.         pres(ka)   )*qonem,
cdiag.         pres(ka+1)  *qonem
cdiag       else
cdiag         write(lp,'(i9,8x,a,a,i3,f10.3,f9.3,f10.3,f9.3)')
cdiag.         nstep,cinfo,':',ka,
cdiag.        (pres(ka+1)-
cdiag.         pres(ka)   )*qonem,
cdiag.         pres(ka+1)  *qonem,
cdiag.        (p(itest,jtest,ka+1)-
cdiag.         p(itest,jtest,ka)   )*qonem,
cdiag.         p(itest,jtest,ka+1)  *qonem
cdiag       endif
cdiag     enddo !ka
cdiag     call flush(lp)
cdiag   endif !debug
c
        if (k.le.fixlay) then
c
c ---     maintain constant thickness, k <= fixlay
          if     (k.lt.kk) then  !p.kk+1 not changed
            p(i,j,k+1)=min(dp0cum(k+1),p(i,j,kk+1))
            if     (k.eq.fixlay) then
c ---         enforce interface order (may not be necessary).
              do ka= k+2,kk
                if     (p(i,j,ka).ge.p(i,j,k+1)) then
                  exit  ! usually get here quickly
                else
                  p(i,j,ka) = p(i,j,k+1)
                endif
              enddo !ka
            endif !k.eq.fixlay
          endif !k.lt.kk
c
cdiag     if (i.eq.itest .and. j.eq.jtest) then
cdiag       write(lp,'(a,i3.2,f8.2)') 'hybgen, fixlay :',
cdiag&                                k+1,p(i,j,k+1)*qonem
cdiag       call flush(lp)
cdiag     endif !debug
        else
c
c ---     do not maintain constant thickness, k > fixlay
c
          if     (th3d(i,j,k,n).gt.theta(i,j,k)+epsil .and.
     &            k.gt.fixlay+1) then 
c
c ---       water in layer k is too dense
c ---       try to dilute with water from layer k-1
c ---       do not move interface if k = fixlay + 1
c
            if (th3d(i,j,k-1,n).ge.theta(i,j,k-1) .or.
     &          p(i,j,k).le.dp0cum(k)+onem .or.
     &          p(i,j,k+1)-p(i,j,k).le.p(i,j,k)-p(i,j,k-1)) then
c
c ---         if layer k-1 is too light, thicken the thinner of the two,
c ---         i.e. skip this layer if it is thicker.
c
cdiag         if (i.eq.itest .and. j.eq.jtest) then
cdiag           write(lp,'(a,3x,i2.2,1pe13.5)')
cdiag&                'hybgen, too dense:',k,th3d(i,j,k,n)-theta(i,j,k)
cdiag         call flush(lp)
cdiag         endif !debug
c 
              if     ((theta(i,j,k)-th3d(i,j,k-1,n)).le.epsil) then
c               layer k-1 much too dense, take entire layer
                p_hat=p(i,j,k-1)+dp0ij(k-1)
              else
                q=(theta(i,j,k)-th3d(i,j,k,  n))/
     &            (theta(i,j,k)-th3d(i,j,k-1,n))         ! -1 <= q < 0
                p_hat0=p(i,j,k)+q*(p(i,j,k+1)-p(i,j,k))  ! <p(i,j,k)
                if     (k.eq.fixlay+2) then
c ---             treat layer k-1 as fixed.
                  p_hat =p(i,j,k-1)+  max(p_hat0-p(i,j,k-1),dp0ij(k-1))
                else
c ---             maintain minimum thickess of layer k-1.
                  p_hat =p(i,j,k-1)+cushn(p_hat0-p(i,j,k-1),dp0ij(k-1))
                endif !fixlay+2:else
              end if
              p_hat=min(p_hat,p(i,j,kk+1))
c
c ---         if isopycnic conditions cannot be achieved because of a blocking
c ---         layer in the interior ocean, move interface k-1 (and k-2 if
c ---         necessary) upward
c
              if     (k.le.fixlay+2) then
c ---           do nothing.
              else if (p_hat.ge.p(i,j,k) .and.
     &                 p(i,j,k-1).gt.dp0cum(k-1)+tenm .and.
     &                (p(i,j,kk+1)-p(i,j,k-1).lt.thkbop .or.
     &                 p(i,j,k-1) -p(i,j,k-2).gt.qqmx*dp0ij(k-2))) then ! k.gt.2
                if     (k.eq.fixlay+3) then
c ---             treat layer k-2 as fixed.
                  p_hat2=p(i,j,k-2)+
     &                     max(p(i,j,k-1)-p_hat+p_hat0-p(i,j,k-2),
     &                         dp0ij(k-2))
                else
c ---             maintain minimum thickess of layer k-2.
                  p_hat2=p(i,j,k-2)+
     &                   cushn(p(i,j,k-1)-p_hat+p_hat0-p(i,j,k-2),
     &                         dp0ij(k-2))
                endif !fixlay+3:else
                if (p_hat2.lt.p(i,j,k-1)-onemm) then
                  p(i,j,k-1)=(1.0-qhrlx(k-1))*p(i,j,k-1) +
     &                            qhrlx(k-1) *max(p_hat2,
     &                                    2.0*p(i,j,k-1)-p_hat)
cdiag             if (i.eq.itest .and. j.eq.jtest) then
cdiag               write(lp,'(a,i3.2,f8.2)') 'hybgen,  1blocking :',
cdiag&                    k-1,p(i,j,k-1)*qonem
cdiag               call flush(lp)
cdiag             endif !debug
                  p_hat=p(i,j,k-1)+cushn(p_hat0-p(i,j,k-1),dp0ij(k-1))
                elseif (k.le.fixlay+3) then
c ---             do nothing.
                elseif (p(i,j,k-2).gt.dp0cum(k-2)+tenm .and.
     &                 (p(i,j,kk+1)-p(i,j,k-2).lt.thkbop .or.
     &                  p(i,j,k-2) -p(i,j,k-3).gt.qqmx*dp0ij(k-3))) then
                  if     (k.eq.fixlay+4) then
c ---               treat layer k-3 as fixed.
                    p_hat3=p(i,j,k-3)+  max(p(i,j,k-2)-p_hat+
     &                                p_hat0-p(i,j,k-3),
     &                                dp0ij(k-3))
                  else
c ---               maintain minimum thickess of layer k-3.
                    p_hat3=p(i,j,k-3)+cushn(p(i,j,k-2)-p_hat+
     &                                p_hat0-p(i,j,k-3),
     &                                dp0ij(k-3))
                  endif !fixlay+4:else
                  if (p_hat3.lt.p(i,j,k-2)-onemm) then
                    p(i,j,k-2)=(1.0-qhrlx(k-2))*p(i,j,k-2) +
     &                              qhrlx(k-2)*max(p_hat3,
     &                                      2.0*p(i,j,k-2)-p(i,j,k-1))
cdiag               if (i.eq.itest .and. j.eq.jtest) then
cdiag                 write(lp,'(a,i3.2,f8.2)') 'hybgen,  2blocking :',
cdiag&                      k-2,p(i,j,k-2)*qonem
cdiag                 call flush(lp)
cdiag               endif !debug
                    p_hat2=p(i,j,k-2)+cushn(p(i,j,k-1)-p_hat+
     &                                      p_hat0-p(i,j,k-2),
     &                                      dp0ij(k-2))
                    if (p_hat2.lt.p(i,j,k-1)-onemm) then
                      p(i,j,k-1)=(1.0-qhrlx(k-1))*p(i,j,k-1) +
     &                                qhrlx(k-1) *max(p_hat2,
     &                                        2.0*p(i,j,k-1)-p_hat)
cdiag                 if (i.eq.itest .and. j.eq.jtest) then
cdiag                   write(lp,'(a,i3.2,f8.2)') 'hybgen,  3blocking :',
cdiag&                             k-1,p(i,j,k-1)*qonem
cdiag                   call flush(lp)
cdiag                 endif !debug
                      p_hat=p(i,j,k-1)+cushn(p_hat0-p(i,j,k-1),
     &                                       dp0ij(k-1))
                    endif !p_hat2
                  endif !p_hat3
                endif !p_hat2:blocking
              endif !blocking
c
              if (p_hat.lt.p(i,j,k)) then
c ---           entrain layer k-1 water into layer k, move interface up.
                p(i,j,k)=(1.0-qhrlx(k))*p(i,j,k) +
     &                        qhrlx(k) *p_hat
              endif !entrain
c
cdiag         if (i.eq.itest .and. j.eq.jtest) then
cdiag           write(lp,'(a,i3.2,f8.2)') 'hybgen, entrain(k) :',
cdiag&                                    k,p(i,j,k)*qonem
cdiag           call flush(lp)
cdiag         endif !debug
c
            endif  !too-dense adjustment
c
          elseif (th3d(i,j,k,n).lt.theta(i,j,k)-epsil) then   ! layer too light
c
c ---       water in layer k is too light
c ---       try to dilute with water from layer k+1
c ---       do not entrain if layer k touches bottom
c
            if (p(i,j,k+1).lt.p(i,j,kk+1)) then  ! k<kk
              if (th3d(i,j,k+1,n).le.theta(i,j,k+1) .or.
     &            p(i,j,k+1).le.dp0cum(k+1)+onem    .or.
     &            p(i,j,k+1)-p(i,j,k).lt.p(i,j,k+2)-p(i,j,k+1)) then
c
c ---           if layer k+1 is too dense, thicken the thinner of the 
c ---           two, i.e. skip this layer (never get here) if it is not 
c ---           thinner than the other.
c
cdiag           if (i.eq.itest .and. j.eq.jtest) then
cdiag             write(lp,'(a,3x,i2.2,1pe13.5)')
cdiag&                 'hybgen, too light:',k,
cdiag&                  theta(i,j,k)-th3d(i,j,k,n)
cdiag             call flush(lp)
cdiag           endif !debug
c
                if     ((th3d(i,j,k+1,n)-theta(i,j,k)).le.epsil) then
c                 layer k-1 too light, take entire layer
                  p_hat=p(i,j,k+2)
                else
                  q=(th3d(i,j,k,  n)-theta(i,j,k))/
     &              (th3d(i,j,k+1,n)-theta(i,j,k))          !-1 <= q < 0
                  p_hat=p(i,j,k+1)+q*(p(i,j,k)-p(i,j,k+1))  !>p(i,j,k+1)
                endif
c
c ---           if layer k+1, or layer k+2, does not touch the bottom
c ---           then maintain minimum thicknesses of layers k and k+1 as
c ---           much as possible. otherwise, permit layers to collapse
c ---           to zero thickness at the bottom.  
c
                if     (p(i,j,min(k+3,kk+1)).lt.p(i,j,kk+1)) then
                  if     (p(i,j,kk+1)-p(i,j,k).gt.
     &                    dp0ij(k)+dp0ij(k+1)     ) then
                    p_hat=p(i,j,k+2)-cushn(p(i,j,k+2)-p_hat,dp0ij(k+1))
                  endif
                  p_hat=p(i,j,k)+max(p_hat-p(i,j,k),dp0ij(k))
                  p_hat=min(p_hat,
     &                      max(0.5*(p(i,j,k+1)+p(i,j,k+2)),
     &                               p(i,j,k+2)-dp0ij(k+1)))
                else
                  p_hat=min(p(i,j,k+2),p_hat)
                endif !p.k+2<p.kk+1
                if (p_hat.gt.p(i,j,k+1)) then
c ---             entrain layer k+1 water into layer k.
                  p(i,j,k+1)=(1.0-qhrlx(k+1))*p(i,j,k+1) +
     &                            qhrlx(k+1) *p_hat
                endif !entrain
c
cdiag           if (i.eq.itest .and. j.eq.jtest) then
cdiag             write(lp,'(a,i3.2,f8.2)')
cdiag&                 'hybgen, entrain(k+):',k,p(i,j,k+1)*qonem
cdiag             call flush(lp)
cdiag           endif !debug
c
              endif !too-light adjustment
            endif !above bottom
          endif !too dense or too light
c
c ---     if layer above is still too thin, move interface down.
          p_hat0=min(p(i,j,k-1)+dp0ij(k-1),p(i,j,kk+1))
          if (p_hat0.gt.p(i,j,k)) then
            p_hat =(1.0-qhrlx(k-1))*p(i,j,k)+
     &                  qhrlx(k-1) *p_hat0
            p(i,j,k)=min(p_hat,p(i,j,k+1))
c
cdiag       if (i.eq.itest .and. j.eq.jtest) then
cdiag         write(lp,'(a,i3.2,f8.2)')
cdiag&             'hybgen, min. thknss (k+):',k-1,p(i,j,k)*qonem
cdiag         call flush(lp)
cdiag       endif !debug
          endif
c
        endif !k.le.fixlay:else
c
      enddo !k  vertical coordinate relocation
c
c --- remap scalar field profiles from the 'old' vertical
c --- grid onto the 'new' vertical grid.
c
      if     (lconserve) then  !usually .false.
        do ktr=1,nums1d
          asum(ktr,1) = 0.d0
          asum(ktr,2) = 0.d0
          asum(ktr,3) = 0.d0
        enddo !ktr
      endif
c
      prsf(1) = p(i,j,1)
      do k=1,kk
        dp(i,j,k,n) = max( p(i,j,k+1)-prsf(k), 0.0 )  !enforce interface order
c ---   to avoid roundoff errors in -dpudpv- after restart, make sure p=p(dp)
        prsf(k+1)   = prsf(k) + dp(i,j,k,n)
        p(i,j,k+1)  = prsf(k+1)
      enddo !k
      if     (hybmap.eq.0) then !PCM
        call hybgen_pcm_remap(s1d,pres,dprs,
     &                        f1d,prsf,kk,kk,nums1d,dpthin)
      elseif (hybmap.eq.1) then !PLM (as in 2.1.08)
        call hybgen_plm_coefs(s1d,     dprs,lcm,c1d,
     &                                 kk,   nums1d,dpthin)
        call hybgen_plm_remap(s1d,pres,dprs,    c1d,
     &                        f1d,prsf,kk,kk,nums1d,dpthin)
      elseif (hybmap.eq.2) then !PPM
        call hybgen_ppm_coefs(s1d,     dpi, lcm,c1d,
     &                                 kk,   nums1d,dpthin)
        call hybgen_ppm_remap(s1d,pres,dprs,    c1d,
     &                        f1d,prsf,kk,kk,nums1d,dpthin)
      elseif (hybmap.eq.3) then !WENO-like
        call hybgen_weno_coefs(s1d,     dpi, lcm,c1d,
     &                                  kk,   nums1d,dpthin)
        call hybgen_weno_remap(s1d,pres,dprs,    c1d,
     &                         f1d,prsf,kk,kk,nums1d,dpthin)
      endif
      do k=1,kk
        if     (hybflg.eq.0) then  !T&S
          temp(i,j,k,n) = f1d(k,1)
          saln(i,j,k,n) = f1d(k,2)
          th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
        elseif (hybflg.eq.1) then  !th&S
          th3d(i,j,k,n) = f1d(k,1)
          saln(i,j,k,n) = f1d(k,2)
          temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,
     &                         saln(i,j,k,n))
*         saln(i,j,k,n)=sofsig(th3d(i,j,k,n)+thbase,
*    &                         temp(i,j,k,n))
        elseif (hybflg.eq.2) then  !th&T
          th3d(i,j,k,n) = f1d(k,1)
          temp(i,j,k,n) = f1d(k,2)
          saln(i,j,k,n)=sofsig(th3d(i,j,k,n)+thbase,
     &                         temp(i,j,k,n))
        endif
        do ktr= 1,ntracr
          tracer(i,j,k,n,ktr) = f1d(k,2+ktr)
        enddo !ktr
            if (ndebug_tracer.gt.0 .and. ndebug_tracer.le.ntracr .and.
     &          i.eq.itest .and. j.eq.jtest) then
              ktr = ndebug_tracer
              write(lp,'(a,i3,2f9.4)')
     &          'hybgen,  old2new:',
     &          k,s1d(k,2+ktr),tracer(i,j,k,n,ktr)
              call flush(lp)
            endif !debug_tracer
        if (mxlmy) then
          q2( i,j,k,n) = f1d(k,ntracr+3)
          q2l(i,j,k,n) = f1d(k,ntracr+4)
        endif
c
        if     (lconserve) then  !usually .false.
          zthk = dp(i,j,k,n)
          do ktr= 1,nums1d
            asum(ktr,1) = asum(ktr,1) + s1d(k,ktr)*dprs(k)
            asum(ktr,2) = asum(ktr,2) + f1d(k,ktr)*zthk
          enddo !ktr
        endif !lconserve
c
      enddo !k
c
cdiag if (i.eq.itest .and. j.eq.jtest) then
*       write (lp,'(i9,3a/(i9,3f23.17))')
*    &  nstep,
*    &  '                   dens',
*    &  '                  thkns',
*    &  '                   dpth',
*    &  (k,tthe(k,1),    dprs(k)*qonem,    pres(k+1)*qonem,
*    &   k,th3d(i,j,k,n),dp(i,j,k,n)*qonem,p(i,j,k+1)*qonem,
*    &  k=1,kk)
cdiag   write (lp,'(i9,3a/(i9,3f23.17))')
cdiag&  nstep,
cdiag&  '               tracer.1',
cdiag&  '                  thkns',
cdiag&  '                   dpth',
cdiag&  (k,ttrc(      k,1,1),  dprs(k)*qonem,   pres(k+1)*qonem,
cdiag&   k,tracer(i,j,k,n,1),dp(i,j,k,n)*qonem,p(i,j,k+1)*qonem,
cdiag&  k=1,kk)
cdiag   call flush(lp)
cdiag endif !debug
c
      if     (lconserve) then  !usually .false.
c
c ---   enforce water column conservation
c
        do ktr=1,nums1d
          q = asum(ktr,1)-asum(ktr,2)
          if     (q.eq.0.0) then
            offset(ktr) = 0.0
          elseif (abs(asum(ktr,2)).lt.2.0*abs(q)) then
            offset(ktr) = sign(zp5,q*asum(ktr,2))  !        -0.5 or  +0.5
          else
            offset(ktr) =          q/asum(ktr,2)   !between -0.5 and +0.5
          endif
        enddo !ktr
        do k=1,kk
          if     (hybflg.eq.0) then  !T&S
            temp(i,j,k,n)=temp(i,j,k,n)*(1.0+offset(1))
            saln(i,j,k,n)=saln(i,j,k,n)*(1.0+offset(2))
            th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
          elseif (hybflg.eq.1) then  !th&S
            th3d(i,j,k,n)=th3d(i,j,k,n)*(1.0+offset(1))
            saln(i,j,k,n)=saln(i,j,k,n)*(1.0+offset(2))
            temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,
     &                           saln(i,j,k,n))
          elseif (hybflg.eq.2) then  !th&T
            th3d(i,j,k,n)=th3d(i,j,k,n)*(1.0+offset(1))
            temp(i,j,k,n)=temp(i,j,k,n)*(1.0+offset(2))
            saln(i,j,k,n)=sofsig(th3d(i,j,k,n)+thbase,
     &                           temp(i,j,k,n))
          endif
          do ktr= 1,ntracr
            tracer(i,j,k,n,ktr)=tracer(i,j,k,n,ktr)*(1.0+offset(ktr+2))
          enddo !ktr
          if (mxlmy) then
            q2( i,j,k,n)=q2( i,j,k,n)*(1.0+offset(ntracr+3))
            q2l(i,j,k,n)=q2l(i,j,k,n)*(1.0+offset(ntracr+4))
          endif
c
          if     (.false.) then !debugging
            zthk = dp(i,j,k,n)
            if     (hybflg.eq.0) then  !T&S
              asum(1,3) = asum(1,3) + temp(i,j,k,n)*zthk
              asum(2,3) = asum(2,3) + saln(i,j,k,n)*zthk
            elseif (hybflg.eq.1) then  !th&S
              asum(1,3) = asum(1,3) + th3d(i,j,k,n)*zthk
              asum(2,3) = asum(2,3) + saln(i,j,k,n)*zthk
            elseif (hybflg.eq.2) then  !th&T
              asum(1,3) = asum(1,3) + th3d(i,j,k,n)*zthk
              asum(2,3) = asum(2,3) + temp(i,j,k,n)*zthk
            endif
            do ktr= 1,ntracr
              asum(ktr+2,3) = asum(ktr+2,3) + tracer(i,j,k,n,ktr)*zthk
            enddo !ktr
            if (mxlmy) then
              asum(ntracr+3,3) = asum(ntracr+3,3) +  q2( i,j,k,n)*zthk
              asum(ntracr+4,3) = asum(ntracr+4,3) +  q2l(i,j,k,n)*zthk
            endif
          endif !debuging
        enddo !k
c
        if     (.false. .and. !debugging
     &          i.eq.itest .and. j.eq.jtest) then
          do ktr= 1,nums1d
            write(lp,'(a,1p4e16.8,i3)')
     &        'hybgen,sum:',
     &        asum(ktr,1)/p(i,j,kk+1),
     &        asum(ktr,2)/p(i,j,kk+1),
     &        asum(ktr,3)/p(i,j,kk+1),
     &        offset(ktr),ktr
          enddo !ktr
        endif !debugging .and. i.eq.itest .and. j.eq.jtest
        if     (.false. .and. !debugging
     &          j.eq.jtest) then
          ktr=1
*         if     (abs(offset(ktr)).gt.1.e-08) then
          if     (abs(offset(ktr)).gt.1.e-12) then
            write(lp,'(a,1p4e16.8,i3)')
     &        'hybgen,sum:',
     &        asum(ktr,1)/p(i,j,kk+1),
     &        asum(ktr,2)/p(i,j,kk+1),
     &        asum(ktr,3)/p(i,j,kk+1),
     &        offset(ktr),i
          endif !large offset
        endif !debugging .and. j.eq.jtest
      endif !lconserve
c
cdiag if (i.eq.itest .and. j.eq.jtest) then
*       write (lp,'(i9,3a/(i9,3f23.17))')
*    &  nstep,
*    &  '                   dens',
*    &  '                  thkns',
*    &  '                   dpth',
*    &  (k,tthe(k,1),    dprs(k)*qonem,    pres(k+1)*qonem,
*    &   k,th3d(i,j,k,n),dp(i,j,k,n)*qonem,p(i,j,k+1)*qonem,
*    &  k=1,kk)
cdiag   write (lp,'(i9,3a/(i9,3f23.17))')
cdiag&  nstep,
cdiag&  '               tracer.1',
cdiag&  '                  thkns',
cdiag&  '                   dpth',
cdiag&  (k,ttrc(      k,1,1),  dprs(k)*qonem,   pres(k+1)*qonem,
cdiag&   k,tracer(i,j,k,n,1),dp(i,j,k,n)*qonem,p(i,j,k+1)*qonem,
cdiag&  k=1,kk)
cdiag   call flush(lp)
cdiag endif
c
cdiag 103  format (i9,2i5,a/(33x,i3,2f8.3,f8.3,f9.3,f9.2))
cdiag      if (i.eq.itest .and. j.eq.jtest) then
cdiag       if     (hybflg.eq.0) then  !T&S
cdiag        write (lp,103) nstep,itest+i0,jtest+j0,
cdiag     &  '    hybgen, do 22:  temp    saln    dens     thkns     dpth',
cdiag     &  (k,s1d(k,1),s1d(k,2),0.0,
cdiag     &   (pres(k+1)-pres(k))*qonem,pres(k+1)*qonem,
cdiag     &   k,temp(i,j,k,n),saln(i,j,k,n),
cdiag     &   th3d(i,j,k,n)+thbase,dp(i,j,k,n)*qonem,
cdiag     &   p(i,j,k+1)*qonem,
cdiag     &  k=1,kk)
cdiag       elseif (hybflg.eq.1) then  !th&S
cdiag        write (lp,103) nstep,itest+i0,jtest+j0,
cdiag     &  '    hybgen, do 22:  temp    saln    dens     thkns     dpth',
cdiag     &  (k,0.0,s1d(k,2),s1d(k,1)+thbase,
cdiag     &   (pres(k+1)-pres(k))*qonem,pres(k+1)*qonem,
cdiag     &   k,temp(i,j,k,n),saln(i,j,k,n),
cdiag     &   th3d(i,j,k,n)+thbase,dp(i,j,k,n)*qonem,
cdiag     &   p(i,j,k+1)*qonem,
cdiag     &  k=1,kk)
cdiag       elseif (hybflg.eq.2) then  !th&T
cdiag        write (lp,103) nstep,itest+i0,jtest+j0,
cdiag     &  '    hybgen, do 22:  temp    saln    dens     thkns     dpth',
cdiag     &  (k,s1d(k,2),0.0,s1d(k,1)+thbase,
cdiag     &   (pres(k+1)-pres(k))*qonem,pres(k+1)*qonem,
cdiag     &   k,temp(i,j,k,n),saln(i,j,k,n),
cdiag     &   th3d(i,j,k,n)+thbase,dp(i,j,k,n)*qonem,
cdiag     &   p(i,j,k+1)*qonem,
cdiag     &  k=1,kk)
cdiag       endif
cdiag       call flush(lp)
cdiag      endif !debug
c
      endif !ip
      enddo !i
c
      return
      end subroutine hybgenaj

      subroutine hybgenbj(nl, j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer nl, j
c
c --- --------------------------------------------
c --- hybrid grid generator, single j-row (part B).
c --- --------------------------------------------
c
      integer i,k
      logical lcm(kdm)      !use PCM for some layers? (always .false.)
      real    s1d(kdm),     !original scalar fields
     &        f1d(kdm),     !final    scalar fields
     &        c1d(kdm,3),   !interpolation coefficients
     &        dpi( kdm),    !original layer thicknesses, >= dpthin
     &        dprs(kdm),    !original layer thicknesses
     &        pres(kdm+1),  !original layer interfaces
     &        prsf(kdm+1)   !final    layer interfaces
      real    dpthin
c
c --- vertical momentum flux across moving interfaces (the s-dot term in the
c --- momentum equation) - required to locally conserve momentum when hybgen
c --- moves vertical coordinates.
c
      dpthin = 0.001*onemm
c
c --- always use high order remapping for velocity
      do k=1,kk
        lcm(k) = .false.  !use same remapper for all layers
      enddo !k
c
      do i=1,ii
        if (SEA_U) then
c
c ---     store one-dimensional arrays of -u- and -p- for the 'old' vertical grid
          pres(1)=pu(i,j,1)
          do k=1,kk
            s1d(k)   =u(i,j,k,nl)
            pres(k+1)=pu(i,j,k+1)
            dprs(k)  =pres(k+1)-pres(k)
            dpi( k)  =max(dprs(k),dpthin)
          enddo !k
c
c ---     remap -u- profiles from the 'old' vertical grid onto the
c ---     'new' vertical grid.
c
          prsf(1) = pu(i,j,1)
          do k=1,kk
            pu(i,j,k+1) = pu(i,j,k) + dpu(i,j,k,nl)  !new vertical grid
            prsf(k+1)   = pu(i,j,k+1)
          enddo
          if     (hybmap.eq.0) then !PCM
            call hybgen_pcm_remap(s1d,pres,dprs,
     &                            f1d,prsf,kk,kk,1, dpthin)
          elseif (hybmap.eq.1 .and. hybiso.gt.2.0) then !PLM (as in 2.1.08)
            call hybgen_plm_coefs(s1d,     dprs,lcm,c1d,
     &                                     kk,   1, dpthin)
            call hybgen_plm_remap(s1d,pres,dprs,    c1d,
     &                            f1d,prsf,kk,kk,1, dpthin)
          else !WENO-like (even if scalar fields are PLM or PPM)
            call hybgen_weno_coefs(s1d,     dpi, lcm,c1d,
     &                                      kk,   1, dpthin)
            call hybgen_weno_remap(s1d,pres,dprs,    c1d,
     &                             f1d,prsf,kk,kk,1, dpthin)
          endif !hybmap
          do k=1,kk
            if     (dpi(k).gt.dpthin .or.
     &              prsf(k).le.prsf(kk+1)-onemm) then
              u(i,j,k,nl) = f1d(k)
            else
* ---         thin near-bottom layer, zero total current
              u(i,j,k,nl) = -ubavg(i,j,nl)
            endif
          enddo !k
c
 104  format (i9,2i5,a/(33x,i3,f8.3,f9.3,f9.2))
cdiag if (i.eq.itest .and. j.eq.jtest) then
cdiag   write (lp,104) nstep,itest+i0,jtest+j0,
cdiag&  '   hybgen, do 412:  u       thkns     dpth',
cdiag&  (k,s1d(k,1),
cdiag&   (pres(k+1)-pres(k))*qonem,pres(k+1)*qonem,
cdiag&   k,u(i,j,k,nl),
cdiag&   dpu(i,j,k,nl)*qonem,pu(i,j,k+1)*qonem,
cdiag&   k=1,kk)
cdiag endif !debug
c
        endif !iu
      enddo !i
c
      do i=1,ii
        if (SEA_V) then
c
c ---     store one-dimensional arrays of -v- and -p- for the 'old' vertical grid
          pres(1)=pv(i,j,1)
          do k=1,kk
            s1d(k)   =v(i,j,k,nl)
            pres(k+1)=pv(i,j,k+1)
            dprs(k)  =pres(k+1)-pres(k)
            dpi( k)  =max(dprs(k),dpthin)
          enddo !k
c
c ---     remap -v- profiles from the 'old' vertical grid onto the
c ---     'new' vertical grid.
c
          prsf(1) = pv(i,j,1)
          do k=1,kk
            pv(i,j,k+1) = pv(i,j,k) + dpv(i,j,k,nl)  !new vertical grid
            prsf(k+1)   = pv(i,j,k+1)
          enddo !k
          if     (hybmap.eq.0) then !PCM
            call hybgen_pcm_remap(s1d,pres,dprs,
     &                            f1d,prsf,kk,kk,1, dpthin)
          elseif (hybmap.eq.1 .and. hybiso.gt.2.0) then !PLM (as in 2.1.08)
            call hybgen_plm_coefs(s1d,     dprs,lcm,c1d,
     &                                     kk,   1, dpthin)
            call hybgen_plm_remap(s1d,pres,dprs,    c1d,
     &                            f1d,prsf,kk,kk,1, dpthin)
          else !WENO-like (even if scalar fields are PLM or PPM)
            call hybgen_weno_coefs(s1d,     dpi, lcm,c1d,
     &                                      kk,   1, dpthin)
            call hybgen_weno_remap(s1d,pres,dprs,    c1d,
     &                             f1d,prsf,kk,kk,1, dpthin)
          endif !hybmap
          do k=1,kk
            if     (dpi(k).gt.dpthin .or.
     &              prsf(k).le.prsf(kk+1)-onemm) then
              v(i,j,k,nl) = f1d(k)
            else
* ---         thin near-bottom layer, zero total current
              v(i,j,k,nl) = -vbavg(i,j,nl)
            endif
          enddo !k
c
cdiag if (i.eq.itest .and. j.eq.jtest) then
cdiag   write (lp,104) nstep,itest+i0,jtest+j0,
cdiag&  '   hybgen, do 512:  v       thkns     dpth',
cdiag&  (k,s1d(k,1),
cdiag&   (pres(k+1)-pres(k))*qonem,pres(k+1)*qonem,
cdiag&   k,v(i,j,k,nl),
cdiag&   dpv(i,j,k,nl)*qonem,pv(i,j,k+1)*qonem,
cdiag&   k=1,kk)
cdiag endif !debug
c
        endif !iv
      enddo !i
c
      return
      end subroutine hybgenbj

      subroutine hybgencj(m,n,j )
      use mod_xc  ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n, j
c
c --- ----------------------------------------------------
c --- hybrid grid generator, single j-row (part C).
c --- Robert-Asselin update to time level t scalar fields.
c --- ----------------------------------------------------
c
      logical, parameter :: lconserve=.false. !explicitly conserve each column
c
      double precision asum(  mxtrcr+4,3)
      real             offset(mxtrcr+4)
c
      logical lcm(kdm)             !use PCM for some layers?
      real    s1d(kdm,mxtrcr+4),   !original scalar fields
     &        f1d(kdm,mxtrcr+4),   !final    scalar fields
     &        c1d(kdm,mxtrcr+4,3), !interpolation coefficients
     &        dpi( kdm),           !original layer thicknesses, >= dpthin
     &        dprs(kdm),           !original layer thicknesses
     &        pres(kdm+1),         !original layer interfaces
     &        prsf(kdm+1),         !final    layer interfaces
     &        dp0ij( kdm),         !minimum layer thickness
     &        dp0cum(kdm+1)        !minimum interface depth
      real    dpold,dpmid,dpnew,q,qdep,zthk,dpthin
      integer i,k,ktr,fixlay,nums1d
      character*12 cinfo
c
      double precision, parameter ::   zp5=0.5    !for sign function
c
      include 'stmt_fns.h'
c
      dpthin = 0.001*onemm
c
      if (mxlmy) then
        nums1d = ntracr + 4
      else
        nums1d = ntracr + 2
      endif
c
      if     (.not.isopcm) then
c ---   lcm the same for all points
        do k=1,nhybrd
          lcm(k) = .false.  !use same remapper for all layers
        enddo !k
        do k=nhybrd+1,kk
          lcm(k) = .true.   !purely isopycnal layers use PCM
        enddo !k
      endif
c
      do i=1,ii
      if (SEA_P) then
c
c --- terrain following starts at depth dpns and ends at depth dsns
      qdep = max( 0.0, min( 1.0,
     &                      (depths(i,j) - dsns)/
     &                      (dpns        - dsns)  ) )
c
      dp0cum(1)=0.0
      dp0ij( 1)=qdep*dp0k(1) + (1.0-qdep)*ds0k(1)
      dp0cum(2)=dp0cum(1)+dp0ij(1)
      do k=2,kk
c ---   q is dp0k(k) when in surface fixed coordinates
c ---   q is dp00i   when much deeper than surface fixed coordinates
        if     (dp0k(k).le.dp00i) then
          q  =      dp0k(k)
        else
          q  = max( dp00i,
     &              dp0k(k) * dp0k(k)/
     &                        max( dp0k( k),
     &                             p(i,j,k)-dp0cum(k) ) )
        endif
        dp0ij( k)  =min( q, qdep*dp0k(k) + (1.0-qdep)*ds0k(k) )
        dp0cum(k+1)=dp0cum(k)+dp0ij(k)
      enddo !k
c
c --- identify the always-fixed coordinate layers
      fixlay = 1  !layer 1 always fixed
      do k= 2,nhybrd
        if     (dp0cum(k).ge.topiso(i,j)) then
          exit  !layers k to nhybrd can be isopycnal
        endif
        fixlay = fixlay+1
      enddo !k
c
c --- store one-dimensional arrays for the 'old' vertical grid,
c --- and the new pressures
      pres(1)=0.0
      prsf(1)=0.0
      do k=1,kk
        if     (hybflg.eq.0) then  !T&S
          s1d(k,1) = temp(i,j,k,m)
          s1d(k,2) = saln(i,j,k,m)
        elseif (hybflg.eq.1) then  !th&S
          s1d(k,1) = th3d(i,j,k,m)
          s1d(k,2) = saln(i,j,k,m)
        elseif (hybflg.eq.2) then  !th&T
          s1d(k,1) = th3d(i,j,k,m)
          s1d(k,2) = temp(i,j,k,m)
        endif
        do ktr= 1,ntracr
          s1d(k,2+ktr) = tracer(i,j,k,m,ktr)
        enddo !ktr
        if (mxlmy) then
          s1d(k,ntracr+3) = q2( i,j,k,m)
          s1d(k,ntracr+4) = q2l(i,j,k,m)
        endif
c
        dprs(k)   = dp(i,j,k,m)  !t, after cnuity RA filter
        dpi( k)   = max(dprs(k),dpthin)
        pres(k+1) = pres(k) + dp(i,j,k,m)
c
c ---   Robert-Asselin time filter of thickness field after hybgenaj
c ---   If hybgenaj left dp.n unchanged, then dp.m is also unchanged
        dpold = dpo(i,j,k,n)  !t-1
        dpmid = dpo(i,j,k,m)  !t,   before cnuity RA filter
        dpnew = dp( i,j,k,n)  !t+1, after hybgenaj
        q     = 0.5*ra2fac*(dpold+dpnew-2.0*dpmid)
        dp(i,j,k,m) = dpmid + q !t & hybgen RA
        prsf(k+1)   = prsf(k) + dp(i,j,k,m)
        p(i,j,k+1)  = prsf(k+1)
c
        if     (isopcm) then
          if     (k.le.fixlay) then
            lcm(k) = .false.  !fixed layers are never PCM
          else
c ---       thin and isopycnal layers remapped with PCM.
            lcm(k) = k.gt.nhybrd
     &               .or. dprs(k).le.dpthin
     &               .or. abs(th3d(i,j,k,m)-theta(i,j,k)).lt.hybiso
          endif !k<=fixlay:else
        endif !isopcm
      enddo !k
c
c --- remap scalar field profiles from the 'old' vertical
c --- grid onto the 'new' vertical grid.
c
      if     (lconserve) then  !usually .false.
        do ktr=1,nums1d
          asum(ktr,1) = 0.d0
          asum(ktr,2) = 0.d0
          asum(ktr,3) = 0.d0
        enddo !ktr
      endif
      if     (hybmap.eq.0) then !PCM
        call hybgen_pcm_remap(s1d,pres,dprs,
     &                        f1d,prsf,kk,kk,nums1d,dpthin)
      elseif (hybmap.eq.1) then !PLM (as in 2.1.08)
        call hybgen_plm_coefs(s1d,     dprs,lcm,c1d,
     &                                 kk,   nums1d,dpthin)
        call hybgen_plm_remap(s1d,pres,dprs,    c1d,
     &                        f1d,prsf,kk,kk,nums1d,dpthin)
      elseif (hybmap.eq.2) then !PPM
        call hybgen_ppm_coefs(s1d,     dpi, lcm,c1d,
     &                                 kk,   nums1d,dpthin)
        call hybgen_ppm_remap(s1d,pres,dprs,    c1d,
     &                        f1d,prsf,kk,kk,nums1d,dpthin)
      elseif (hybmap.eq.3) then !WENO-like
        call hybgen_weno_coefs(s1d,     dpi, lcm,c1d,
     &                                  kk,   nums1d,dpthin)
        call hybgen_weno_remap(s1d,pres,dprs,    c1d,
     &                         f1d,prsf,kk,kk,nums1d,dpthin)
      endif
      do k=1,kk
        if     (hybflg.eq.0) then  !T&S
          temp(i,j,k,m) = f1d(k,1)
          saln(i,j,k,m) = f1d(k,2)
          th3d(i,j,k,m)=sig(temp(i,j,k,m),saln(i,j,k,m))-thbase
        elseif (hybflg.eq.1) then  !th&S
          th3d(i,j,k,m) = f1d(k,1)
          saln(i,j,k,m) = f1d(k,2)
          temp(i,j,k,m)=tofsig(th3d(i,j,k,m)+thbase,
     &                         saln(i,j,k,m))
*         saln(i,j,k,m)=sofsig(th3d(i,j,k,m)+thbase,
*    &                         temp(i,j,k,m))
        elseif (hybflg.eq.2) then  !th&T
          th3d(i,j,k,m) = f1d(k,1)
          temp(i,j,k,m) = f1d(k,2)
          saln(i,j,k,m)=sofsig(th3d(i,j,k,m)+thbase,
     &                         temp(i,j,k,m))
        endif
        do ktr= 1,ntracr
          tracer(i,j,k,m,ktr) = f1d(k,2+ktr)
        enddo !ktr
        if (mxlmy) then
          q2( i,j,k,m) = f1d(k,ntracr+3)
          q2l(i,j,k,m) = f1d(k,ntracr+4)
        endif
c
        if     (lconserve) then  !usually .false.
          zthk = dp(i,j,k,m)
          do ktr= 1,nums1d
            asum(ktr,1) = asum(ktr,1) + s1d(k,ktr)*dprs(k)
            asum(ktr,2) = asum(ktr,2) + f1d(k,ktr)*zthk
          enddo !ktr
        endif !lconserve
c
      enddo !k
c
cdiag if (i.eq.itest .and. j.eq.jtest) then
*       write (lp,'(i9,3a/(i9,3f23.17))')
*    &  nstep,
*    &  '                   dens',
*    &  '                  thkns',
*    &  '                   dpth',
*    &  (k,tthe(k,1),    dprs(k)*qonem,    pres(k+1)*qonem,
*    &   k,th3d(i,j,k,m),dp(i,j,k,m)*qonem,p(i,j,k+1)*qonem,
*    &  k=1,kk)
cdiag   write (lp,'(i9,3a/(i9,3f23.17))')
cdiag&  nstep,
cdiag&  '               tracer.1',
cdiag&  '                  thkns',
cdiag&  '                   dpth',
cdiag&  (k,ttrc(      k,1,1),  dprs(k)*qonem,   pres(k+1)*qonem,
cdiag&   k,tracer(i,j,k,m,1),dp(i,j,k,m)*qonem,p(i,j,k+1)*qonem,
cdiag&  k=1,kk)
cdiag   call flush(lp)
cdiag endif !debug
c
      if     (lconserve) then  !usually .false.
c
c ---   enforce water column conservation
c
        do ktr=1,nums1d
          q = asum(ktr,1)-asum(ktr,2)
          if     (q.eq.0.0) then
            offset(ktr) = 0.0
          elseif (abs(asum(ktr,2)).lt.2.0*abs(q)) then
            offset(ktr) = sign(zp5,q*asum(ktr,2))  !        -0.5 or  +0.5
          else
            offset(ktr) =          q/asum(ktr,2)   !between -0.5 and +0.5
          endif
        enddo !ktr
        do k=1,kk
          if     (hybflg.eq.0) then  !T&S
            temp(i,j,k,m)=temp(i,j,k,m)*(1.0+offset(1))
            saln(i,j,k,m)=saln(i,j,k,m)*(1.0+offset(2))
            th3d(i,j,k,m)=sig(temp(i,j,k,m),saln(i,j,k,m))-thbase
          elseif (hybflg.eq.1) then  !th&S
            th3d(i,j,k,m)=th3d(i,j,k,m)*(1.0+offset(1))
            saln(i,j,k,m)=saln(i,j,k,m)*(1.0+offset(2))
            temp(i,j,k,m)=tofsig(th3d(i,j,k,m)+thbase,
     &                           saln(i,j,k,m))
          elseif (hybflg.eq.2) then  !th&T
            th3d(i,j,k,m)=th3d(i,j,k,m)*(1.0+offset(1))
            temp(i,j,k,m)=temp(i,j,k,m)*(1.0+offset(2))
            saln(i,j,k,m)=sofsig(th3d(i,j,k,m)+thbase,
     &                           temp(i,j,k,m))
          endif
          do ktr= 1,ntracr
            tracer(i,j,k,m,ktr)=tracer(i,j,k,m,ktr)*(1.0+offset(ktr+2))
          enddo !ktr
          if (mxlmy) then
            q2( i,j,k,m)=q2( i,j,k,m)*(1.0+offset(ntracr+3))
            q2l(i,j,k,m)=q2l(i,j,k,m)*(1.0+offset(ntracr+4))
          endif
c
          if     (.false.) then !debugging
            zthk = dp(i,j,k,m)
            if     (hybflg.eq.0) then  !T&S
              asum(1,3) = asum(1,3) + temp(i,j,k,m)*zthk
              asum(2,3) = asum(2,3) + saln(i,j,k,m)*zthk
            elseif (hybflg.eq.1) then  !th&S
              asum(1,3) = asum(1,3) + th3d(i,j,k,m)*zthk
              asum(2,3) = asum(2,3) + saln(i,j,k,m)*zthk
            elseif (hybflg.eq.2) then  !th&T
              asum(1,3) = asum(1,3) + th3d(i,j,k,m)*zthk
              asum(2,3) = asum(2,3) + temp(i,j,k,m)*zthk
            endif
            do ktr= 1,ntracr
              asum(ktr+2,3) = asum(ktr+2,3) + tracer(i,j,k,m,ktr)*zthk
            enddo !ktr
            if (mxlmy) then
              asum(ntracr+3,3) = asum(ntracr+3,3) +  q2( i,j,k,m)*zthk
              asum(ntracr+4,3) = asum(ntracr+4,3) +  q2l(i,j,k,m)*zthk
            endif
          endif !debuging
        enddo !k
c
        if     (.false. .and. !debugging
     &          i.eq.itest .and. j.eq.jtest) then
          do ktr= 1,nums1d
            write(lp,'(a,1p4e16.8,i3)')
     &        'hybgen,sum:',
     &        asum(ktr,1)/p(i,j,kk+1),
     &        asum(ktr,2)/p(i,j,kk+1),
     &        asum(ktr,3)/p(i,j,kk+1),
     &        offset(ktr),ktr
          enddo !ktr
        endif !debugging .and. i.eq.itest .and. j.eq.jtest
        if     (.false. .and. !debugging
     &          j.eq.jtest) then
          ktr=1
*         if     (abs(offset(ktr)).gt.1.e-08) then
          if     (abs(offset(ktr)).gt.1.e-12) then
            write(lp,'(a,1p4e16.8,i3)')
     &        'hybgen,sum:',
     &        asum(ktr,1)/p(i,j,kk+1),
     &        asum(ktr,2)/p(i,j,kk+1),
     &        asum(ktr,3)/p(i,j,kk+1),
     &        offset(ktr),i
          endif !large offset
        endif !debugging .and. j.eq.jtest
      endif !lconserve
c
cdiag if (i.eq.itest .and. j.eq.jtest) then
*       write (lp,'(i9,3a/(i9,3f23.17))')
*    &  nstep,
*    &  '                   dens',
*    &  '                  thkns',
*    &  '                   dpth',
*    &  (k,tthe(k,1),    dprs(k)*qonem,    pres(k+1)*qonem,
*    &   k,th3d(i,j,k,m),dp(i,j,k,m)*qonem,p(i,j,k+1)*qonem,
*    &  k=1,kk)
cdiag   write (lp,'(i9,3a/(i9,3f23.17))')
cdiag&  nstep,
cdiag&  '               tracer.1',
cdiag&  '                  thkns',
cdiag&  '                   dpth',
cdiag&  (k,ttrc(      k,1,1),  dprs(k)*qonem,   pres(k+1)*qonem,
cdiag&   k,tracer(i,j,k,m,1),dp(i,j,k,m)*qonem,p(i,j,k+1)*qonem,
cdiag&  k=1,kk)
cdiag   call flush(lp)
cdiag endif
c
cdiag 103  format (i9,2i5,a/(33x,i3,2f8.3,f8.3,f9.3,f9.2))
cdiag      if (i.eq.itest .and. j.eq.jtest) then
cdiag       if     (hybflg.eq.0) then  !T&S
cdiag        write (lp,103) nstep,itest+i0,jtest+j0,
cdiag     &  '    hybgen, do 22:  temp    saln    dens     thkns     dpth',
cdiag     &  (k,s1d(k,1),s1d(k,2),0.0,
cdiag     &   (pres(k+1)-pres(k))*qonem,pres(k+1)*qonem,
cdiag     &   k,temp(i,j,k,m),saln(i,j,k,m),
cdiag     &   th3d(i,j,k,m)+thbase,dp(i,j,k,m)*qonem,
cdiag     &   p(i,j,k+1)*qonem,
cdiag     &  k=1,kk)
cdiag       elseif (hybflg.eq.1) then  !th&S
cdiag        write (lp,103) nstep,itest+i0,jtest+j0,
cdiag     &  '    hybgen, do 22:  temp    saln    dens     thkns     dpth',
cdiag     &  (k,0.0,s1d(k,2),s1d(k,1)+thbase,
cdiag     &   (pres(k+1)-pres(k))*qonem,pres(k+1)*qonem,
cdiag     &   k,temp(i,j,k,m),saln(i,j,k,m),
cdiag     &   th3d(i,j,k,m)+thbase,dp(i,j,k,m)*qonem,
cdiag     &   p(i,j,k+1)*qonem,
cdiag     &  k=1,kk)
cdiag       elseif (hybflg.eq.2) then  !th&T
cdiag        write (lp,103) nstep,itest+i0,jtest+j0,
cdiag     &  '    hybgen, do 22:  temp    saln    dens     thkns     dpth',
cdiag     &  (k,s1d(k,2),0.0,s1d(k,1)+thbase,
cdiag     &   (pres(k+1)-pres(k))*qonem,pres(k+1)*qonem,
cdiag     &   k,temp(i,j,k,m),saln(i,j,k,m),
cdiag     &   th3d(i,j,k,m)+thbase,dp(i,j,k,m)*qonem,
cdiag     &   p(i,j,k+1)*qonem,
cdiag     &  k=1,kk)
cdiag       endif
cdiag       call flush(lp)
cdiag      endif !debug
c
      endif !ip
      enddo !i
c
      return
      end subroutine hybgencj

      subroutine hybgen_pcm_remap(si,pi,dpi,
     &                            so,po,ki,ko,ks,thin)
      implicit none
c
      integer ki,ko,ks
      real    si(ki,ks),pi(ki+1),dpi(ki),
     &        so(ko,ks),po(ko+1),thin
c
c-----------------------------------------------------------------------
c  1) remap from one set of vertical cells to another.
c     method: piecewise constant across each input cell
c             the output is the average of the interpolation
c             profile across each output cell.
c
c     PCM (donor cell) is the standard 1st order upwind method.
c
c  2) input arguments:
c       si    - initial scalar fields in pi-layer space
c       pi    - initial layer interface depths (non-negative)
c                  pi(   1) is the surface
c                  pi(ki+1) is the bathymetry
c                  pi(k+1) >= pi(k)
c       dpi   - initial layer thicknesses (dpi(k)=pi(k+1)-pi(k))
c       ki    - number of  input layers
c       ko    - number of output layers
c       ks    - number of fields
c       po    - target interface depths (non-negative)
c                  po(   1) is the surface
c                  po(ko+1) is the bathymetry (== pi(ki+1))
c                  po(k+1) >= po(k)
c       thin  - layer thickness (>0) that can be ignored
c
c  3) output arguments:
c       so    - scalar fields in po-layer space
c
c  4) Tim Campbell, Mississippi State University, October 2002.
c     Alan J. Wallcraft,  Naval Research Laboratory,  Aug. 2007.
c-----------------------------------------------------------------------
c
      integer i,k,l,lb,lt
      real    dpb,dpt,xb,xt,zb,zt,zx,o
      real*8  sz
      real    si_min(ks),si_max(ks)
c
c --- inforce minval(si(:,i)) <= minval(so(:,i)) and
c ---         maxval(si(:,i)) >= maxval(so(:,i)) for i=1:ks
c --- in particular this inforces non-negativity, e.g. of tracers
c --- only required due to finite precision
c
      do i= 1,ks
        si_min(i) = minval(si(:,i))
        si_max(i) = maxval(si(:,i))
      enddo !i
c
      zx=pi(ki+1) !maximum depth
      zb=max(po(1),pi(1))
      lb=1
      do while (pi(lb+1).lt.zb .and. lb.lt.ki)
        lb=lb+1
      enddo
      do k= 1,ko  !output layers
        zt = zb
        zb = min(po(k+1),zx)
*       write(lp,*) 'k,zt,zb = ',k,zt,zb
        lt=lb !top will always correspond to bottom of previous
              !find input layer containing bottom output interface
        do while (pi(lb+1).lt.zb .and. lb.lt.ki)
          lb=lb+1
        enddo
        if     (zb-zt.le.thin .or. zt.ge.zx) then
          if     (k.ne.1) then
c
c ---       thin or bottomed layer, values taken from layer above
c
            do i= 1,ks
              so(k,i) = so(k-1,i)
            enddo !i
          else !thin surface layer
            do i= 1,ks
              so(k,i) = si(k,i)
            enddo !i
          endif
        else
c
c         form layer averages.
c         use PPM-like logic (may not have minimum operation count)
c
*         if     (pi(lb).gt.zt) then
*           write(lp,*) 'bad lb = ',lb
*           stop
*         endif
          if     (lt.ne.lb) then  !multiple layers
            xt=(zt-pi(lt))/max(dpi(lt),thin)
            xb=(zb-pi(lb))/max(dpi(lb),thin)
            dpt=pi(lt+1)-zt
            dpb=zb-pi(lb)
            do i= 1,ks
              o  = si((lt+lb)/2,i)  !offset to reduce round-off
              sz = dpt*(si(lt,i)-o)
              do l=lt+1,lb-1
                sz = sz+dpi(l)*(si(l,i)-o)
              enddo !l
              sz = sz+dpb*(si(lb,i)-o)
              so(k,i) = o + sz/(zb-zt)  !zb-zt>=thin
              so(k,i) = max( si_min(i), so(k,i) )
              so(k,i) = min( si_max(i), so(k,i) )
            enddo !i
          else  !single layer
            do i= 1,ks
              so(k,i) = si(lt,i)
            enddo !i
          endif
        endif !thin:std layer
      enddo !k
      return
      end subroutine hybgen_pcm_remap

      subroutine hybgen_plm_coefs(si,dpi,lc,ci,kk,ks,thin)
      implicit none
c
      integer kk,ks
      logical lc(kk)
      real    si(kk,ks),dpi(kk),ci(kk,ks),thin
c
c-----------------------------------------------------------------------
c  1) coefficents for remaping from one set of vertical cells to another.
c     method: piecewise linear across each input cell with
c             monotonized central-difference limiter.
c
c     van Leer, B., 1977, J. Comp. Phys., 23 276-299.
c
c  2) input arguments:
c       si    - initial scalar fields in pi-layer space
c       dpi   - initial layer thicknesses (dpi(k)=pi(k+1)-pi(k))
c       lc    - use PCM for selected layers
c       kk    - number of layers
c       ks    - number of fields
c       thin  - layer thickness (>0) that can be ignored
c
c  3) output arguments:
c       ci    - coefficents (slopes) for hybgen_plm_remap
c                profile(y)=si+ci*(y-1),  0<=y<=1
c
c  4) Tim Campbell, Mississippi State University, October 2002.
c     Alan J. Wallcraft,  Naval Research Laboratory,  Aug. 2007.
c-----------------------------------------------------------------------
c
      integer k,i
      real    qcen,zbot,zcen,ztop
c
      do i= 1,ks
        ci(1, i) = 0.0
        ci(kk,i) = 0.0
      enddo !i
      do k= 2,kk-1
        if     (lc(k) .or. dpi(k).le.thin) then  !use PCM
          do i= 1,ks
            ci(k,i) = 0.0
          enddo !i
        else
c ---     use qcen in place of 0.5 to allow for non-uniform grid
          qcen = dpi(k)/(dpi(k)+0.5*(dpi(k-1)+dpi(k+1)))  !dpi(k)>thin
          do i= 1,ks
c ---       PLM (non-zero slope, but no new extrema)
c ---       layer value is si-0.5*ci at top    interface,
c ---                  and si+0.5*ci at bottom interface.
c
c ---       monotonized central-difference limiter (van Leer, 1977,
c ---       JCP 23 pp 276-299).  For a discussion of PLM limiters, see
c ---       Finite Volume Methods for Hyperbolic Problems by R.J. Leveque.
            ztop = 2.0*(si(k,  i)-si(k-1,i))
            zbot = 2.0*(si(k+1,i)-si(k,  i))
            zcen =qcen*(si(k+1,i)-si(k-1,i))
            if     (ztop*zbot.gt.0.0) then !ztop,zbot are the same sign
              ci(k,i)=sign(min(abs(zcen),abs(zbot),abs(ztop)),zbot)
            else
              ci(k,i)=0.0  !local extrema, so no slope
            endif
          enddo !i
        endif  !PCM:PLM
      enddo !k
      return
      end subroutine hybgen_plm_coefs

      subroutine hybgen_plm_remap(si,pi,dpi,ci,
     &                            so,po,ki,ko,ks,thin)
      implicit none
c
      integer ki,ko,ks
      real    si(ki,ks),pi(ki+1),dpi(ki),ci(ki,ks),
     &        so(ko,ks),po(ko+1),thin
c
c-----------------------------------------------------------------------
c  1) remap from one set of vertical cells to another.
c     method: piecewise linear across each input cell
c             the output is the average of the interpolation
c             profile across each output cell.
c
c     van Leer, B., 1977, J. Comp. Phys., 23 276-299.
c
c  2) input arguments:
c       si    - initial scalar fields in pi-layer space
c       pi    - initial layer interface depths (non-negative)
c                  pi(   1) is the surface
c                  pi(ki+1) is the bathymetry
c                  pi(k+1) >= pi(k)
c       dpi   - initial layer thicknesses (dpi(k)=pi(k+1)-pi(k))
c       ci    - coefficents (slopes) from hybgen_plm_coefs
c                profile(y)=si+ci*(y-1),  0<=y<=1
c       ki    - number of  input layers
c       ko    - number of output layers
c       ks    - number of fields
c       po    - target interface depths (non-negative)
c                  po(   1) is the surface
c                  po(ko+1) is the bathymetry (== pi(ki+1))
c                  po(k+1) >= po(k)
c       thin  - layer thickness (>0) that can be ignored
c
c  3) output arguments:
c       so    - scalar fields in po-layer space
c
c  4) Tim Campbell, Mississippi State University, October 2002.
c     Alan J. Wallcraft,  Naval Research Laboratory,  Aug. 2007.
c-----------------------------------------------------------------------
c
      integer i,k,l,lb,lt
      real    c0,qb0,qb1,qt0,qt1,xb,xt,zb,zt,zx,o
      real*8  sz
      real    si_min(ks),si_max(ks)
c
c --- inforce minval(si(:,i)) <= minval(so(:,i)) and
c ---         maxval(si(:,i)) >= maxval(so(:,i)) for i=1:ks
c --- in particular this inforces non-negativity, e.g. of tracers
c --- only required due to finite precision
c
      do i= 1,ks
        si_min(i) = minval(si(:,i))
        si_max(i) = maxval(si(:,i))
      enddo !i
c
      zx=pi(ki+1) !maximum depth
      zb=max(po(1),pi(1))
      lb=1
      do while (pi(lb+1).lt.zb .and. lb.lt.ki)
        lb=lb+1
      enddo
      do k= 1,ko  !output layers
        zt = zb
        zb = min(po(k+1),zx)
*       write(lp,*) 'k,zt,zb = ',k,zt,zb
        lt=lb !top will always correspond to bottom of previous
              !find input layer containing bottom output interface
        do while (pi(lb+1).lt.zb .and. lb.lt.ki)
          lb=lb+1
        enddo
        if     (zb-zt.le.thin .or. zt.ge.zx) then
          if     (k.ne.1) then
c
c ---       thin or bottomed layer, values taken from layer above
c
            do i= 1,ks
              so(k,i) = so(k-1,i)
            enddo !i
          else !thin surface layer
            do i= 1,ks
              so(k,i) = si(k,i)
            enddo !i
          endif
        else
c
c         form layer averages.
c         use PPM-like logic (may not have minimum operation count)
c
*         if     (pi(lb).gt.zt) then
*           write(lp,*) 'bad lb = ',lb
*           stop
*         endif
          xt=(zt-pi(lt))/max(dpi(lt),thin)
          xb=(zb-pi(lb))/max(dpi(lb),thin)
          if     (lt.ne.lb) then  !multiple layers
            qt0 = (1.0-xt)
            qt1 = (1.0-xt**2)*0.5
            qb0 =      xb
            qb1 =      xb**2 *0.5
            do i= 1,ks
              o  = si((lt+lb)/2,i)  !offset to reduce round-off
              c0 = si(lt,i) - o - 0.5*ci(lt,i)
              sz=  dpi(lt)*(c0*qt0 + ci(lt,i)*qt1)
              do l=lt+1,lb-1
                sz = sz+dpi(l)*(si(l,i) - o)
              enddo !l
              c0 = si(lb,i) - o - 0.5*ci(lb,i)
              sz = sz+dpi(lb)*(c0*qb0 + ci(lb,i)*qb1)
              so(k,i) = o + sz/(zb-zt)  !zb-zt>=thin
              so(k,i) = max( si_min(i), so(k,i) )
              so(k,i) = min( si_max(i), so(k,i) )
            enddo !i
          else  !single layer
            qt1 = (xb**2-xt**2 - (xb-xt))*0.5
            do i= 1,ks
              sz = dpi(lt)*(ci(lt,i)*qt1)
              so(k,i) = si(lt,i) + sz/(zb-zt)  !zb-zt>=thin
              so(k,i) = max( si_min(i), so(k,i) )
              so(k,i) = min( si_max(i), so(k,i) )
            enddo !i
          endif
        endif !thin:std layer
      enddo !k
      return
      end subroutine hybgen_plm_remap

      subroutine hybgen_ppm_coefs(s,dp,lc,ci,kk,ks,thin)
      implicit none
c
      integer kk,ks
      logical lc(kk)
      real    s(kk,ks),dp(kk),ci(kk,ks,3),thin
c
c-----------------------------------------------------------------------
c  1) coefficents for remaping from one set of vertical cells to another.
c     method: monotonic piecewise parabolic across each input cell
c
c     Colella, P. & P.R. Woodward, 1984, J. Comp. Phys., 54, 174-201.
c
c  2) input arguments:
c       s     - initial scalar fields in pi-layer space
c       dp    - initial layer thicknesses (>=thin)
c       lc    - use PCM for selected layers
c       kk    - number of layers
c       ks    - number of fields
c       thin  - layer thickness (>0) that can be ignored
c
c  3) output arguments:
c       ci    - coefficents for hybgen_ppm_remap
c                profile(y)=ci.1+ci.2*y+ci.3*y^2, 0<=y<=1
c
c  4) Tim Campbell, Mississippi State University, October 2002.
c     Alan J. Wallcraft,  Naval Research Laboratory,  Aug. 2007.
c-----------------------------------------------------------------------
c
      integer j,i
      real    da,a6,slj,scj,srj
      real    as(kk),al(kk),ar(kk)
      real     dpjp(kk), dp2jp(kk), dpj2p(kk),
     &        qdpjp(kk),qdp2jp(kk),qdpj2p(kk),dpq3(kk),qdp4(kk)
c
      !compute grid metrics
      do j=1,kk-1
         dpjp( j) = dp(j)   + dp(j+1)
         dp2jp(j) = dp(j)   + dpjp(j)
         dpj2p(j) = dpjp(j) + dp(j+1)
        qdpjp( j) = 1.0/dpjp( j)
        qdp2jp(j) = 1.0/dp2jp(j)
        qdpj2p(j) = 1.0/dpj2p(j)
      enddo !j
         dpq3(2) = dp(2)/(dp(1)+dpjp(2))
      do j=3,kk-1
         dpq3(j) = dp(j)/(dp(j-1)+dpjp(j)) !dp(j)/      (dp(j-1)+dp(j)+dp(j+1))
         qdp4(j) = 1.0/(dpjp(j-2)+dpjp(j)) !1.0/(dp(j-2)+dp(j-1)+dp(j)+dp(j+1))
      enddo !j
c
      do i= 1,ks
        !Compute average slopes: Colella, Eq. (1.8)
        as(1)=0.
        do j=2,kk-1
          if     (lc(j) .or. dp(j).le.thin) then  !use PCM
            as(j) = 0.0
          else
            slj=s(j,  i)-s(j-1,i)
            srj=s(j+1,i)-s(j,  i)
            if (slj*srj.gt.0.) then
              scj=dpq3(j)*( dp2jp(j-1)*srj*qdpjp(j)
     &                     +dpj2p(j)  *slj*qdpjp(j-1) )
              as(j)=sign(min(abs(2.0*slj),abs(scj),abs(2.0*srj)),scj)
            else
              as(j)=0.
            endif
          endif  !PCM:PPM
        enddo !j
        as(kk)=0.
        !Compute "first guess" edge values: Colella, Eq. (1.6)
        al(1)=s(1,i)  !1st layer PCM
        ar(1)=s(1,i)  !1st layer PCM
        al(2)=s(1,i)  !1st layer PCM
        do j=3,kk-1
          al(j)=s(j-1,i)+dp(j-1)*(s(j,i)-s(j-1,i))*qdpjp(j-1)
     &         +qdp4(j)*(
     &            2.*dp(j)*dp(j-1)*qdpjp(j-1)*(s(j,i)-s(j-1,i))*
     &            ( dpjp(j-2)*qdp2jp(j-1)
     &             -dpjp(j)  *qdpj2p(j-1) )
     &            -dp(j-1)*as(j)  *dpjp(j-2)*qdp2jp(j-1)
     &            +dp(j)  *as(j-1)*dpjp(j)  *qdpj2p(j-1)
     &              )
          ar(j-1)=al(j)
        enddo !j
        ar(kk-1)=s(kk,i)  !last layer PCM
        al(kk)  =s(kk,i)  !last layer PCM
        ar(kk)  =s(kk,i)  !last layer PCM
        !Impose monotonicity: Colella, Eq. (1.10)
        do j=2,kk-1
          if     (lc(j) .or. dp(j).le.thin) then  !use PCM
            al(j)=s(j,i)
            ar(j)=s(j,i)
          elseif ((s(j+1,i)-s(j,i))*(s(j,i)-s(j-1,i)).le.0.) then !local extremum
            al(j)=s(j,i)
            ar(j)=s(j,i)
          else
            da=ar(j)-al(j)
            a6=6.0*s(j,i)-3.0*(al(j)+ar(j))
            if     (da*a6 .gt.  da*da) then !peak in right half of zone
              al(j)=3.0*s(j,i)-2.0*ar(j)
            elseif (da*a6 .lt. -da*da) then !peak in left half of zone
              ar(j)=3.0*s(j,i)-2.0*al(j)
            endif
          endif
        enddo !j
        !Set coefficients
        do j=1,kk
          if     (al(j).ne.ar(j)) then
            ci(j,i,1)=al(j)
            ci(j,i,2)=ar(j)-al(j)
            ci(j,i,3)=6.0*s(j,i)-3.0*(al(j)+ar(j))
          else !PCM
            ci(j,i,1)=al(j)
            ci(j,i,2)=0.0
            ci(j,i,3)=0.0
          endif
        enddo !j
      enddo !i
      return
      end subroutine hybgen_ppm_coefs

      subroutine hybgen_ppm_remap(si,pi,dpi,ci,
     &                            so,po,ki,ko,ks,thin)
      implicit none
c
      integer ki,ko,ks
      real    si(ki,ks),pi(ki+1),dpi(ki),ci(ki,ks,3),
     &        so(ko,ks),po(ko+1),thin
c
c-----------------------------------------------------------------------
c  1) remap from one set of vertical cells to another.
c     method: monotonic piecewise parabolic across each input cell
c             the output is the average of the interpolation
c             profile across each output cell.
c     Colella, P. & P.R. Woodward, 1984, J. Comp. Phys., 54, 174-201.
c
c  2) input arguments:
c       si    - initial scalar fields in pi-layer space
c       pi    - initial layer interface depths (non-negative)
c                  pi(   1) is the surface
c                  pi(ki+1) is the bathymetry
c                  pi(k+1) >= pi(k)
c       dpi   - initial layer thicknesses (dpi(k)=pi(k+1)-pi(k))
c       ci    - coefficents from hybgen_ppm_coefs
c                profile(y)=ci.1+ci.2*y+ci.3*y^2, 0<=y<=1
c       ki    - number of  input layers
c       ko    - number of output layers
c       ks    - number of fields
c       po    - target interface depths (non-negative)
c                  po(   1) is the surface
c                  po(ko+1) is the bathymetry (== pi(ki+1))
c                  po(k+1) >= po(k)
c       thin  - layer thickness (>0) that can be ignored
c
c  3) output arguments:
c       so    - scalar fields in po-layer space
c
c  4) Tim Campbell, Mississippi State University, October 2002.
c     Alan J. Wallcraft,  Naval Research Laboratory,  Aug. 2007.
c-----------------------------------------------------------------------
c
      integer i,k,l,lb,lt
      real    qb0,qb1,qb2,qt0,qt1,qt2,xb,xt,zb,zt,zx,o
      real*8  sz
      real    si_min(ks),si_max(ks)
c
c --- inforce minval(si(:,i)) <= minval(so(:,i)) and
c ---         maxval(si(:,i)) >= maxval(so(:,i)) for i=1:ks
c --- in particular this inforces non-negativity, e.g. of tracers
c --- only required due to finite precision
c
      do i= 1,ks
        si_min(i) = minval(si(:,i))
        si_max(i) = maxval(si(:,i))
      enddo !i
c
      zx=pi(ki+1) !maximum depth
      zb=max(po(1),pi(1))
      lb=1
      do while (pi(lb+1).lt.zb .and. lb.lt.ki)
        lb=lb+1
      enddo
      do k= 1,ko  !output layers
        zt = zb
        zb = min(po(k+1),zx)
*       write(lp,*) 'k,zt,zb = ',k,zt,zb
        lt=lb !top will always correspond to bottom of previous
              !find input layer containing bottom output interface
        do while (pi(lb+1).lt.zb .and. lb.lt.ki)
          lb=lb+1
        enddo
        if     (zb-zt.le.thin .or. zt.ge.zx) then
          if     (k.ne.1) then
c
c ---       thin or bottomed layer, values taken from layer above
c
            do i= 1,ks
              so(k,i) = so(k-1,i)
            enddo !i
          else !thin surface layer
            do i= 1,ks
              so(k,i) = si(k,i)
            enddo !i
          endif
        else
c
c         form layer averages.
c
*         if     (pi(lb).gt.zt) then
*           write(lp,*) 'bad lb = ',lb
*           stop
*         endif
          xt=(zt-pi(lt))/max(dpi(lt),thin)
          xb=(zb-pi(lb))/max(dpi(lb),thin)
          if     (lt.ne.lb) then  !multiple layers
            qt0 = (1.0-xt)
            qt1 = (1.-xt**2)*0.5
            qt2 = (1.-xt**3)/3.0
            qb0 =     xb
            qb1 =     xb**2 *0.5
            qb2 =     xb**3 /3.0
            do i= 1,ks
              o  = si((lt+lb)/2,i)  !offset to reduce round-off
              sz = dpi(lt)*(  (ci(lt,i,1)-o)*qt0
     &                       +(ci(lt,i,2)+
     &                         ci(lt,i,3) ) *qt1
     &                        -ci(lt,i,3)   *qt2 )
              do l=lt+1,lb-1
                sz = sz+dpi(l)*(si(l,i)-o)
              enddo !l
              sz = sz+dpi(lb)*( (ci(lb,i,1)-o)*qb0
     &                         +(ci(lb,i,2)+
     &                           ci(lb,i,3) ) *qb1
     &                          -ci(lb,i,3)   *qb2 )
              so(k,i) = o + sz/(zb-zt)  !zb-zt>=thin
              so(k,i) = max( si_min(i), so(k,i) )
              so(k,i) = min( si_max(i), so(k,i) )
            enddo !i
          else  !single layer
            qt0 = (xb-xt)
            qt1 = (xb**2-xt**2)*0.5
            qt2 = (xb**3-xt**3)/3.0
            do i= 1,ks
              o  = si( lt,i)  !offset to reduce round-off
              sz = dpi(lt)*( (ci(lt,i,1)-o)*qt0
     &                      +(ci(lt,i,2)+
     &                        ci(lt,i,3) ) *qt1
     &                       -ci(lt,i,3)   *qt2 )
              so(k,i) = o + sz/(zb-zt)  !zb-zt>=thin
              so(k,i) = max( si_min(i), so(k,i) )
              so(k,i) = min( si_max(i), so(k,i) )
            enddo !i
          endif
        endif !thin:std layer
      enddo !k
      return
      end subroutine hybgen_ppm_remap

      subroutine hybgen_weno_coefs(s,dp,lc,ci,kk,ks,thin)
      implicit none
c
      integer kk,ks
      logical lc(kk)
      real    s(kk,ks),dp(kk),ci(kk,ks,2),thin
c
c-----------------------------------------------------------------------
c  1) coefficents for remaping from one set of vertical cells to another.
c     method: monotonic WENO-like alternative to PPM across each input cell
c             a second order polynomial approximation of the profiles
c             using a WENO reconciliation of the slopes to compute the 
c             interfacial values 
c
c     REFERENCE?
c
c  2) input arguments:
c       s     - initial scalar fields in pi-layer space
c       dp    - initial layer thicknesses (>=thin)
c       lc    - use PCM for selected layers
c       kk    - number of layers
c       ks    - number of fields
c       thin  - layer thickness (>0) that can be ignored
c
c  3) output arguments:
c       ci    - coefficents for hybgen_weno_remap
c                ci.1 is value at interface above
c                ci.2 is value at interface below
c
c  4) Laurent Debreu, Grenoble.
c     Alan J. Wallcraft,  Naval Research Laboratory,  July 2008.
c-----------------------------------------------------------------------
c
      real, parameter :: dsmll=1.0e-8
c
      integer j,i
      real    q,q01,q02,q001,q002
      real    qdpjm(kk),qdpjmjp(kk),dpjm2jp(kk)
      real    zw(kk+1,3)

      !compute grid metrics
      do j=2,kk-1
        qdpjm(  j) = 1.0/(dp(j-1) +     dp(j))
        qdpjmjp(j) = 1.0/(dp(j-1) +     dp(j) + dp(j+1))
        dpjm2jp(j) =      dp(j-1) + 2.0*dp(j) + dp(j+1)
      enddo !j
      j=kk
        qdpjm(  j) = 1.0/(dp(j-1) +     dp(j))
c
      do i= 1,ks
        do j=2,kk
          zw(j,3) = qdpjm(j)*(s(j,i)-s(j-1,i))
        enddo !j
          j = 1  !PCM first layer
            ci(j,i,1) = s(j,i)
            ci(j,i,2) = s(j,i)
            zw(j,  1) = 0.0
            zw(j,  2) = 0.0
        do j=2,kk-1
          if     (lc(j) .or. dp(j).le.thin) then  !use PCM
            ci(j,i,1) = s(j,i)
            ci(j,i,2) = s(j,i)
            zw(j,  1) = 0.0
            zw(j,  2) = 0.0
          else
            q001 = dp(j)*zw(j+1,3)
            q002 = dp(j)*zw(j,  3)
            if (q001*q002 < 0.0) then
              q001 = 0.0
              q002 = 0.0
            endif
            q01 = dpjm2jp(j)*zw(j+1,3)
            q02 = dpjm2jp(j)*zw(j,  3)
            if     (abs(q001) > abs(q02)) then
              q001 = q02
            endif
            if     (abs(q002) > abs(q01)) then
              q002 = q01
            endif
            q    = (q001-q002)*qdpjmjp(j)
            q001 = q001-q*dp(j+1)
            q002 = q002+q*dp(j-1)

            ci(j,i,2) = s(j,i)+q001
            ci(j,i,1) = s(j,i)-q002
            zw(  j,1) = (2.0*q001-q002)**2
            zw(  j,2) = (2.0*q002-q001)**2
          endif  !PCM:WEND
        enddo !j
          j = kk  !PCM last layer
            ci(j,i,1) = s(j,i)
            ci(j,i,2) = s(j,i)
            zw(j,  1) = 0.0
            zw(j,  2) = 0.0

        do j=2,kk
          q002 = max(zw(j-1,2),dsmll)
          q001 = max(zw(j,  1),dsmll)
          zw(j,3) = (q001*ci(j-1,i,2)+q002*ci(j,i,1))/(q001+q002)
        enddo !j
          zw(   1,3) = 2.0*s( 1,i)-zw( 2,3)  !not used?
          zw(kk+1,3) = 2.0*s(kk,i)-zw(kk,3)  !not used?

        do j=2,kk-1
          if     (.not.(lc(j) .or. dp(j).le.thin)) then  !don't use PCM
            q01  = zw(j+1,3)-s(j,i)
            q02  = s(j,i)-zw(j,3)
            q001 = 2.0*q01
            q002 = 2.0*q02
            if     (q01*q02 < 0.0) then
              q01 = 0.0
              q02 = 0.0
            elseif (abs(q01) > abs(q002)) then
              q01 = q002
            elseif (abs(q02) > abs(q001)) then
              q02 = q001
            endif
            ci(j,i,1) = s(j,i)-q02
            ci(j,i,2) = s(j,i)+q01
          endif  !PCM:WEND
        enddo !j
      enddo !i
      return
      end subroutine hybgen_weno_coefs

      subroutine hybgen_weno_remap(si,pi,dpi,ci,
     &                             so,po,ki,ko,ks,thin)
      implicit none
c
      integer ki,ko,ks
      real    si(ki,ks),pi(ki+1),dpi(ki),ci(ki,ks,2),
     &        so(ko,ks),po(ko+1),thin
c
c-----------------------------------------------------------------------
c  1) remap from one set of vertical cells to another.
c     method: monotonic WENO-like alternative to PPM across each input cell
c             a second order polynomial approximation of the profiles
c             using a WENO reconciliation of the slopes to compute the 
c             interfacial values 
c             the output is the average of the interpolation
c             profile across each output cell.
c
c     REFERENCE?
c
c  2) input arguments:
c       si    - initial scalar fields in pi-layer space
c       pi    - initial layer interface depths (non-negative)
c                  pi(   1) is the surface
c                  pi(ki+1) is the bathymetry
c                  pi(k+1) >= pi(k)
c       dpi   - initial layer thicknesses (dpi(k)=pi(k+1)-pi(k))
c       ci    - coefficents from hybgen_weno_coefs
c                ci.1 is value at interface above
c                ci.2 is value at interface below
c       ki    - number of  input layers
c       ko    - number of output layers
c       ks    - number of fields
c       po    - target interface depths (non-negative)
c                  po(   1) is the surface
c                  po(ko+1) is the bathymetry (== pi(ki+1))
c                  po(k+1) >= po(k)
c       thin  - layer thickness (>0) that can be ignored
c
c  3) output arguments:
c       so    - scalar fields in po-layer space
c
c  4) Laurent Debreu, Grenoble.
c     Alan J. Wallcraft,  Naval Research Laboratory,  Aug. 2007.
c-----------------------------------------------------------------------
c
      integer i,k,l,lb,lt
      real    dpb,dpt,qb0,qb1,qb2,qt0,qt1,qt2,xb,xt,zb,zt,zx,o
      real*8  sz
      real    si_min(ks),si_max(ks)
c
c --- inforce minval(si(:,i)) <= minval(so(:,i)) and
c ---         maxval(si(:,i)) >= maxval(so(:,i)) for i=1:ks
c --- in particular this inforces non-negativity, e.g. of tracers
c --- only required due to finite precision
c
      do i= 1,ks
        si_min(i) = minval(si(:,i))
        si_max(i) = maxval(si(:,i))
      enddo !i
c
      zx=pi(ki+1) !maximum depth
      zb=max(po(1),pi(1))
      lb=1
      do while (pi(lb+1).lt.zb .and. lb.lt.ki)
        lb=lb+1
      enddo
      do k= 1,ko  !output layers
        zt = zb
        zb = min(po(k+1),zx)
*       write(lp,*) 'k,zt,zb = ',k,zt,zb
        lt=lb !top will always correspond to bottom of previous
              !find input layer containing bottom output interface
        do while (pi(lb+1).lt.zb .and. lb.lt.ki)
          lb=lb+1
        enddo
        if     (zb-zt.le.thin .or. zt.ge.zx) then
          if     (k.ne.1) then
c
c ---       thin or bottomed layer, values taken from layer above
c
            do i= 1,ks
              so(k,i) = so(k-1,i)
            enddo !i
          else !thin surface layer
            do i= 1,ks
              so(k,i) = si(k,i)
            enddo !i
          endif
        else
c
c         form layer averages.
c
*         if     (pi(lb).gt.zt) then
*           write(lp,*) 'bad lb = ',lb
*           stop
*         endif
          xt=(zt-pi(lt))/max(dpi(lt),thin)
          xb=(zb-pi(lb))/max(dpi(lb),thin)
          if     (lt.ne.lb) then  !multiple layers
            dpt = pi(lt+1)-zt
            dpb = zb-pi(lb)
            qt1 = xt*(xt-1.0)
            qt2 = qt1+xt
            qt0 = 1.0-qt1-qt2
            qb1 = (xb-1.0)**2
            qb2 = qb1-1.0+xb
            qb0 = 1.0-qb1-qb2
            do i= 1,ks
              o = si((lt+lb)/2,i)  !offset to reduce round-off
              sz = dpt*(qt0*(si(lt,i)  -o) +
     &                  qt1*(ci(lt,i,1)-o) +
     &                  qt2*(ci(lt,i,2)-o)  )
              do l=lt+1,lb-1
                sz = sz+dpi(l)*(si(l,i) - o)
              enddo !l
              sz  = sz + dpb*(qb0*(si(lb,i)  -o) +
     &                        qb1*(ci(lb,i,1)-o) +
     &                        qb2*(ci(lb,i,2)-o)  )
              so(k,i) = o + sz/(zb-zt)  !zb-zt>=thin
              so(k,i) = max( si_min(i), so(k,i) )
              so(k,i) = min( si_max(i), so(k,i) )
            enddo !i
          else !single layer
            qt1 = xb**2 + xt**2 + xb*xt + 1.0 - 2.0*(xb+xt)
            qt2 = qt1 - 1.0 + (xb+xt)
            do i= 1,ks
              o =     si(lt,i)  !offset to reduce round-off
              sz=qt1*(ci(lt,i,1)-o) +
     &           qt2*(ci(lt,i,2)-o) 
              so(k,i) = o + sz
              so(k,i) = max( si_min(i), so(k,i) )
              so(k,i) = min( si_max(i), so(k,i) )
            enddo !i
          endif !layers
        endif !thin:std layer
      enddo !k
      return
      end subroutine hybgen_weno_remap

c
c
c> Revision history:
c>
c> Feb. 2000 -- total rewrite to convert to 'newzp' approach
c> Jul. 2000 -- added hybgenj for OpenMP parallelization
c> Oct. 2000 -- added hybgenbj to simplify OpenMP logic
c> Nov. 2000 -- fill massless layers on sea floor with salinity from above
c> Nov. 2000 -- unmixing of deepest inflated layer uses th&T&S from above
c> Nov. 2000 -- ignored isopycnic variance is now 0.002
c> Nov. 2000 -- iterate to correct for cabbeling
c> Nov. 2000 -- allow for "blocking" interior layers
c> Nov. 2000 -- hybflg selects conserved fields (any two of T/S/th)
c> Nov. 2002 -- replace PCM remapping with PLM when non-isopycnal
c> Apr. 2003 -- added dp00i for thinner minimum layers away from the surface
c> Dec. 2003 -- fixed tracer bug when deepest inflated layer is too light
c> Dec. 2003 -- improved water column conservation
c> Dec. 2003 -- compile time option for explicit water column conservation
c> Dec. 2003 -- ignored isopycnic variance is now 0.0001
c> Jan. 2004 -- shifted qqmn,qqmx range now used in cushion function
c> Mar. 2004 -- minimum thickness no longer enforced in near-bottom layers
c> Mar. 2004 -- ignored isopycnic variance is now epsil (i.e. very small)
c> Mar. 2004 -- relaxation to isopycnic layers controled via hybrlx
c> Mar. 2004 -- relaxation removes the need to correct for cabbeling
c> Mar. 2004 -- modified unmixing selection criteria
c> Mar. 2004 -- added isotop (topiso) for isopycnal layer minimum depths
c> Jun. 2005 -- hybrlx (qhybrlx) now input via blkdat.input
c> Jan. 2007 -- hybrlx now only active below "fixed coordinate" surface layers
c> Aug. 2007 -- removed mxlkta logic
c> Sep. 2007 -- added hybmap and hybiso for PCM,PLM,PPM remaper selection
c> Jan. 2008 -- updated logic for two layers (one too dense, other too light)
c> Jul. 2008 -- Added WENO-like, and bugfix to PPM for lcm==.true.
c> Aug. 2008 -- Use WENO-like (vs PPM) for most velocity remapping
c> Aug. 2008 -- Switch more thin near-isopycnal layers to PCM remapping
c> May  2009 -- New action when deepest inflated layer is very light
c> Oct  2010 -- updated test for deepest inflated layer too light
c> Oct  2010 -- updated sanity check on deltm
c> July 2010 -- Maintain vertical minval and maxval in remapping
c> Aug  2011 -- Option to apply Robert-Asselin filter to hybgen's updated dp
c> Mar  2012 -- Replaced dssk with dpns and dsns, see blkdat.F for info
c> July 2012 -- Bugfix for tracer in too-light deepest inflated layer
c> Sep  2012 -- Added ndebug_tracer for tracer debuging
c> Sep  2012 -- Don't unmix in terrain-following regime
c> Apr. 2013 -- Detect all fixed coordinate layers
c> Apr. 2013 -- bugfix for constant thickness layer k = 1
c> May  2014 - use land/sea masks (e.g. ip) to skip land
c> Feb. 2015 - when too light, include layer k+2 in "near bottom" logic
c> Apr. 2015 - fixed a k+3 for k=kk-1 (k+3=kk+2) bug
c> Aug. 2015 - changed k-2 to k-N in lowest layer "runaway" unmixing test
c> Aug. 2015 - overturn with the layer above if bottom layer is very light
c> Aug. 2015 - entrain into too dense layer (only move upper interface up)
c> Aug. 2015 - allow entrainment to increase fixlay by 1
