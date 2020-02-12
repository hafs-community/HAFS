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
      subroutine convch(m,n)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
c
c --- hycom version 1.0
      implicit none
c
      integer m,n
c
cdiag real    sigup,uup,vup,siglo,ulo,vlo,coluin(idm),colout(idm)
      real    q,tem,sal,thet,trc(mxtrcr)
      real    dthet,plev,alfadt,betads
      integer i,iter,j,k,k1,ks,kp,ktr,margin
      logical llayer
c
      integer, parameter :: itmax=5
c
      include 'stmt_fns.h'
c
c --- ---------------------
c --- convective adjustment
c --- ---------------------
c
 103  format (i9,2i5,a/(33x,i3,2f8.3,f8.3,f8.2,f8.1))
cdiag if (itest.gt.0 .and. jtest.gt.0) then
cdiag   write (lp,103) nstep,itest+i0,jtest+i0,
cdiag&  '  entering convec:  temp    saln    dens    thkns    dpth',
cdiag&  (k,temp(itest,jtest,k,n),saln(itest,jtest,k,n),
cdiag&  th3d(itest,jtest,k,n)+thbase,dp(itest,jtest,k,n)*qonem,
cdiag&  p(itest,jtest,k+1)*qonem,k=1,kk)
cdiag  endif
c
      call xctilr(th3d(1-nbdy,1-nbdy,1,n),1,kk, 1,1, halo_ps)
c
c --- set thstar, note there is no thermobaric correction
c
      margin = 1
c
!$OMP PARALLEL DO PRIVATE(j,k,i,q)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
      do k=2,kk
      k1=k-1
      do i=1-margin,ii+margin
      if (SEA_P) then
      p(i,j,k)=p(i,j,k-1)+dp(i,j,k,n)
      if (.not.mxlkta .and. p(i,j,k).lt.dpmixl(i,j,n)) then
        nmlb(i,j,n)=k
      endif
      if (locsig) then
        p(i,j,k)=p(i,j,k-1)+dp(i,j,k,n)
        if (k.eq.2) then
          thstar(i,j,k,1)=th3d(i,j,k1,n)
        else
          if (k1.gt.nmlb(i,j,n)) then
            alfadt=dsiglocdt(ahalf*(temp(i,j,k1,n)+
     &                              temp(i,j,k ,n) ),
     &                       ahalf*(saln(i,j,k1,n)+
     &                              saln(i,j,k ,n) ),p(i,j,k))*
     &                             (temp(i,j,k1,n)-
     &                              temp(i,j,k, n) )
            betads=dsiglocds(ahalf*(temp(i,j,k1,n)+
     &                              temp(i,j,k ,n) ),
     &                       ahalf*(saln(i,j,k1,n)+
     &                              saln(i,j,k ,n) ),p(i,j,k))*
     &                             (saln(i,j,k1,n)-
     &                              saln(i,j,k, n) )
            thstar(i,j,k,1)=thstar(i,j,k1,1)-alfadt-betads
          else
            thstar(i,j,k,1)=thstar(i,j,k1,1)
          endif
        endif
      else
        thstar(i,j,k,1)=th3d(i,j,k,n)
      endif
      endif !ip
      enddo !i
      enddo !k
      enddo !j
!$OMP END PARALLEL DO
c
c --- convective adjustment
c
!$OMP PARALLEL DO PRIVATE(j,k,i,q)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
c
c --- convection of u
c
      do k=2,kk
      do i=1,ii
      if (SEA_U) then
      pu(i,j,k+1)=pu(i,j,k)+dpu(i,j,k,n)
      if (pu(i,j,k+1).lt.depthu(i,j)-onemm .and.
     &    thstar(i,j,k  ,1)+thstar(i-1,j,k  ,1).lt.
     &    thstar(i,j,k-1,1)+thstar(i-1,j,k-1,1)    ) then
cdiag   if (i.eq.itest .and. j.eq.jtest) then
cdiag   uup=u(i,j,k-1,n)
cdiag   ulo=u(i,j,k,  n)
cdiag   endif
        q=1.0-max(dpu(i,j,k,n),0.)/
     &   (max(dpu(i,j,k,n),0.)+max(dpu(i,j,k-1,n),onemm))
        u(i,j,k,  n)=u(i,j,k,n)+q*(u(i,j,k-1,n)-u(i,j,k,n))
        u(i,j,k-1,n)=u(i,j,k,n)
cdiag   if (i.eq.itest .and. j.eq.jtest) then
cdiag     write (lp,100) nstep,i+i0,j+j0,k,
cdiag&      1,'  upr,lwr,final u:',uup,ulo,u(i,j,k,n),q
cdiag   endif
      end if
      endif !ip
      enddo !i
      enddo !k
c
c --- convection of v
c
      do k=2,kk
      do i=1,ii
      if (SEA_V) then
      pv(i,j,k+1)=pv(i,j,k)+dpv(i,j,k,n)
      if (pv(i,j,k+1).lt.depthv(i,j)-onemm .and.
     &    thstar(i,j,k  ,1)+thstar(i,j-1,k  ,1).lt.
     &    thstar(i,j,k-1,1)+thstar(i,j-1,k-1,1)    ) then
cdiag   vup=v(i,j,k-1,n)
cdiag   vlo=v(i,j,k,  n)
        q=1.0-max(dpv(i,j,k,n),0.)/
     &   (max(dpv(i,j,k,n),0.)+max(dpv(i,j,k-1,n),onemm))
        v(i,j,k,  n)=v(i,j,k,n)+q*(v(i,j,k-1,n)-v(i,j,k,n))
        v(i,j,k-1,n)=v(i,j,k,n)
cdiag   if (i.eq.itest .and. j.eq.jtest) then
cdiag      write (lp,100) nstep,i+i0,j+i0,k,
cdiag&       1,'  upr,lwr,final v:',vup,vlo,v(i,j,k,n),q
cdiag   endif
      end if
      endif !ip
      enddo !i
      enddo !k
      enddo !j
!$OMP END PARALLEL DO
c
c --- convection of thermodynamical variables and tracer
c
!$OMP PARALLEL DO PRIVATE(j,i,k,ktr,iter,ks,kp,
!$OMP&                    q,tem,sal,thet,trc)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
      do i=1,ii
      if (SEA_P) then
c
ccc      coluin(i)=0.
ccc      colout(i)=0.
c
      do 12 k=1,kk
ccc      coluin(i)=coluin(i)+temp(i,j,k,n)*dp(i,j,k,n)
      p(i,j,k+1)=p(i,j,k)+dp(i,j,k,n)
 12   continue
c
      do 6 iter=1,itmax
c
      klist(i,j)=1
      util1(i,j)=dp(i,j,1,n)
c
      do 6 k=2,kk
      k1=k-1
c
      ks=klist(i,j)
      if (locsig) then
        plev=0.5*(p(i,j,ks+1)+p(i,j,k))
        alfadt=0.5*
     &        (dsiglocdt(temp(i,j,ks,n),saln(i,j,ks,n),plev)+
     &         dsiglocdt(temp(i,j,k ,n),saln(i,j,k ,n),plev))*
     &        (temp(i,j,ks,n)-temp(i,j,k,n))
        betads=0.5*
     &        (dsiglocds(temp(i,j,ks,n),saln(i,j,ks,n),plev)+
     &         dsiglocds(temp(i,j,k ,n),saln(i,j,k ,n),plev))*
     &        (saln(i,j,ks,n)-saln(i,j,k,n))
        dthet=alfadt+betads
      else
        dthet=th3d(i,j,ks,n)-th3d(i,j,k,n)
      endif
      if (dthet.gt.0.0) then
        if (dp(i,j,k,n).lt.onemm) then
          if (locsig) then
            alfadt=dsiglocdt(ahalf*(temp(i,j,k1,n)+
     &                              temp(i,j,k ,n) ),
     &                       ahalf*(saln(i,j,k1,n)+
     &                              saln(i,j,k ,n) ),p(i,j,k))*
     &                             (temp(i,j,k1,n)-
     &                              temp(i,j,k, n) )
            betads=dsiglocds(ahalf*(temp(i,j,k1,n)+
     &                              temp(i,j,k ,n) ),
     &                       ahalf*(saln(i,j,k1,n)+
     &                              saln(i,j,k ,n) ),p(i,j,k))*
     &                             (saln(i,j,k1,n)-
     &                              saln(i,j,k, n) )
            dthet=alfadt+betads
          else
            dthet=th3d(i,j,k1,n)-th3d(i,j,k,n)
          endif
          if(dthet.ge.0.0) then
            saln(i,j,k,n)=saln(i,j,k1,n)
            temp(i,j,k,n)=temp(i,j,k1,n)
            th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
            do ktr= 1,ntracr
              tracer(i,j,k,n,ktr)=tracer(i,j,k1,n,ktr)
            enddo
          end if
        else				!  dp > onemm
          if (iter.eq.itmax) then
            if (locsig) then
              plev=0.5*(p(i,j,ks+1)+p(i,j,k))
              alfadt=0.5*
     &              (dsiglocdt(temp(i,j,ks,n),saln(i,j,ks,n),plev)+
     &               dsiglocdt(temp(i,j,k ,n),saln(i,j,k ,n),plev))*
     &              (temp(i,j,ks,n)-temp(i,j,k,n))
              betads=0.5*
     &              (dsiglocds(temp(i,j,ks,n),saln(i,j,ks,n),plev)+
     &               dsiglocds(temp(i,j,k ,n),saln(i,j,k ,n),plev))*
     &              (saln(i,j,ks,n)-saln(i,j,k,n))
              dthet=alfadt+betads
            else
              dthet=th3d(i,j,ks,n)-th3d(i,j,k,n)
            endif
            if (dthet.gt.sigjmp .and. dp(i,j,k,n).gt.onem) then
              ! only write out the lowest unstable layer
              llayer = k.eq.kk
              if     (.not.llayer) then
                llayer =   dp(i,j,k+1,n).lt.onem .or.
     &                   th3d(i,j,k+1,n).ge.th3d(i,j,ks,n)-sigjmp
              endif
              if     (llayer) then
!$OMP           CRITICAL
                write (lp,'(i9,2i5,i3,a,i3,a,i3,a,2f10.4)')
     &           nstep,i+i0,j+j0,k,
     &           ' colmn unstbl (wrt',ks,') after',
     &           iter-1,' its',
     &           th3d(i,j,ks,n)+thbase,th3d(i,j,k,n)+thbase
!$OMP           END CRITICAL
              endif
            endif
          else				!  it < itmax
cdiag       sigup=th3d(i,j,ks,n)
cdiag       siglo=th3d(i,j,k, n)
            util1(i,j)=util1(i,j)+dp(i,j,k,n)
            q=1.0-max(dp(i,j,k,n),0.5*onemm)/max(util1(i,j),onemm)
            tem=temp(i,j,k,n)+q*(temp(i,j,ks,n)-temp(i,j,k,n))
            sal=saln(i,j,k,n)+q*(saln(i,j,ks,n)-saln(i,j,k,n))
            do ktr= 1,ntracr
              trc(ktr)=tracer(i,j,k,n,ktr)+q*(tracer(i,j,ks,n,ktr)-
     &                                        tracer(i,j,k,n,ktr))
            enddo
            thet=sig(tem,sal)-thbase
            do 10 kp=ks,k
            temp(i,j,kp,n)=tem
            saln(i,j,kp,n)=sal
            th3d(i,j,kp,n)=thet
            do ktr= 1,ntracr
              tracer(i,j,kp,n,ktr)=trc(ktr)
            enddo
 10         continue
c
cdiag if (i.eq.itest .and. j.eq.jtest) then
cduag   write (lp,100) nstep,i+i0,j+j0,k,iter,
cdiag&    '  upr,lwr,final dens:',(sigup+thbase),
cdiag&    (siglo+thbase),(th3d(i,j,k,n)+thbase),q
cdiag endif
 100    format (i9,2i5,i3,'  it',i2,a,3f8.3,f5.2)
c
          end if
        end if
      else				!  th3d(kn) > th3d(ksn)
        klist(i,j)=k
        util1(i,j)=dp(i,j,k,n)
      end if
 6    continue
c
ccc      do k=1,kk
ccc      colout(i)=colout(i)+temp(i,j,k,n)*dp(i,j,k,n)
ccc      enddo !k
ccc      if (abs((colout(i)-coluin(i))/coluin(i)).gt.1.e-6)
ccc     .  write (lp,'(i9,2i5,a/1p,3e14.6)') nstep,i,j,
ccc     .  '  column integral not conserved in convec:',
ccc     .  coluin(i),colout(i),(colout(i)-coluin(i))/coluin(i)
      endif !ip
      enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
cdiag if (itest.gt.0 .and. jtest.gt.0) then
cdiag   write (lp,103) nstep,itest+i0,jtest+j0,
cdiag&  '  exiting  convec:  temp    saln    dens    thkns    dpth',
cdiag&  (k,temp(itest,jtest,k,n),saln(itest,jtest,k,n),
cdiag&  th3d(itest,jtest,k,n)+thbase,dp(itest,jtest,k,n)*qonem,
cdiag&  p(itest,jtest,k+1)*qonem,k=1,kk)
cdiag endif
c
      return
      end
c
      subroutine convcm(m,n)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
c
c --- hycom version 1.0 (adapted from micom version 2.8)
      implicit none
c
      integer m,n
c
      integer i,j,k,k1
      real    dthet,delp,alfadt,betads
c
      include 'stmt_fns.h'
c
 103  format (i9,2i5,a/(33x,i3,2f8.3,3p,f8.3,0p,f8.2,f8.1))
cdiag if (itest.gt.0 .and. jtest.gt.0) then
cdiag   write (lp,103) nstep,itest+i0,jtest+j0,
cdiag&  '  entering convec:  temp    saln    dens    thkns    dpth',
cdiag&  (k,temp(itest,jtest,k,n),saln(itest,jtest,k,n),
cdiag&  th3d(itest,jtest,k,n)+thbase,dp(itest,jtest,k,n)*qonem,
cdiag&  p(itest,jtest,k+1)*qonem,k=1,kk)
cdiag endif
c
c --- -------------------------------------------------------------
c --- entrain water lighter than mixed-layer water into mixed layer
c --- -------------------------------------------------------------
c
!$OMP PARALLEL DO PRIVATE(j,k,i,dthet,delp)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
      do i=1,ii
      if (SEA_P) then
      klist(i,j)=0
      p(i,j,2)=dp(i,j,1,n)
      dpo(i,j,1,n)=dp(i,j,1,n)
c
      do k=2,kk
      p(i,j,k+1)=p(i,j,k)+dp(i,j,k,n)
      dpo(i,j,k,n)=0.
      enddo !k
c
      do k=2,kk
      k1=k-1
c
      p(i,j,k+1)=p(i,j,k)+dp(i,j,k,n)
      if (dp(i,j,k,n).le.0. .or.
     &   (klist(i,j).gt.0 .and. k.gt.klist(i,j)+1)) exit !do k
      if (locsig) then
        alfadt=dsiglocdt(ahalf*(temp(i,j,k1,n)+
     &                          temp(i,j,k ,n) ),
     &                   ahalf*(saln(i,j,k1,n)+
     &                          saln(i,j,k ,n) ),p(i,j,k))*
     &                         (temp(i,j,k1,n)-
     &                          temp(i,j,k, n) )
        betads=dsiglocds(ahalf*(temp(i,j,k1,n)+
     &                          temp(i,j,k ,n) ),
     &                   ahalf*(saln(i,j,k1,n)+
     &                          saln(i,j,k ,n) ),p(i,j,k))*
     &                         (saln(i,j,k1,n)-
     &                          saln(i,j,k, n) )
        dthet=-alfadt-betads
      else
        dthet=th3d(i,j,k,n)-th3d(i,j,1,n)
      endif
      if (dthet.lt.0. .and. p(i,j,k+1).le.p(i,j,kk+1)) then
c
cdiag   if (i.eq.itest .and. j.eq.jtest) then
cdiag   write (lp,100) nstep,i+i0,j+j0,' convec',1,th3d(i,j,1,n)
cdiag&  +thbase,dp(i,j,1,n)*qonem,temp(i,j,1,n),saln(i,j,1,n),k,
cdiag&  th3d(i,j,k,n)+thbase,dp(i,j,k,n)*qonem,temp(i,j,k,n),saln(i,j,k,n)
cdiag endif
 100    format (i9,2i5,a,i3,'  th3d,dp,t,s =',3pf7.3,0pf7.1,2f8.3
     &    /26x,i3,15x,3pf7.3,0pf7.1,2f8.3)
c
c --- layer -k- contains mass less dense than mixed layer. entrain it.
        delp=dp(i,j,1,n)+dp(i,j,k,n)
        saln(i,j,1,n)=(saln(i,j,1,n)*dp(i,j,1,n)
     &                +saln(i,j,k, n)*dp(i,j,k, n))/delp
        temp(i,j,1,n)=(temp(i,j,1,n)*dp(i,j,1,n)
     &                +temp(i,j,k, n)*dp(i,j,k, n))/delp
        th3d(i,j,1,n)=sig(temp(i,j,1,n),saln(i,j,1,n))-thbase
c
        diaflx(i,j,1)=diaflx(i,j,1)+dp(i,j,k,n)			! diapyc.flux
        diaflx(i,j,k)=diaflx(i,j,k)-dp(i,j,k,n)			! diapyc.flux
c
c --- mass in layer -k- transferred to mixed layer is stored in -dpo-.
        dp(i,j,1,n)=delp
        dpo(i,j,k,n)=dp(i,j,k,n)
        dp(i,j,k,n)=0.
        klist(i,j)=k
      end if                 !  dthet < 0
c
      enddo !k
      endif !ip
      enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
      do j=1,jj
        do i=1,ii
          if (ip(i,j).eq.1) then
            util1(i,j) = klist(i,j)
          else
            util1(i,j) = 0.0
          endif
        enddo !j
      enddo !i
      call xctilr(util1,                 1, 1, 1,1, halo_ps)
      call xctilr(dpo(1-nbdy,1-nbdy,1,n),1,kk, 1,1, halo_ps)
      do j=0,jj+1
        do i=0,ii+1
          klist(i,j) = util1(i,j)
        enddo !j
      enddo !i
c
!$OMP PARALLEL DO PRIVATE(j,k,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
c
      do i=1,ii
      if (SEA_P) then
      dpmixl(i,j,n)=dp(i,j,1,n)
      endif !ip
c
c --- entrain -u- momentum
c
      if (SEA_U) then
      util2(i,j)=min(0.5*(dpo(i,j,1,n)+dpo(i-1,j,1,n)),depthu(i,j))
      do k=2,max(klist(i,j),klist(i-1,j))
      util1(i,j)=util2(i,j)
      util2(i,j)=min(util2(i,j)+0.5*(dpo(i,j,k,n)+dpo(i-1,j,k,n)),
     &               depthu(i,j))
      u(i,j,1,n)=(u(i,j,1,n)*util1(i,j)
     &           +u(i,j,k,n)*(util2(i,j)-util1(i,j)))/util2(i,j)
      enddo !k
      endif !iu
c
c --- entrain -v- momentum
c
      if (SEA_V) then
      util2(i,j)=min(0.5*(dpo(i,j,1,n)+dpo(i,j-1,1,n)),
     &               depthv(i,j))
      do k=2,max(klist(i,j),klist(i,j-1))
      util1(i,j)=util2(i,j)
      util2(i,j)=min(util2(i,j)+0.5*(dpo(i,j,k,n)+dpo(i,j-1,k,n)),
     &               depthv(i,j))
      util2(i,j)=min(util2(i,j)+0.5*(dpo(i,j,k,n)+dpo(i,j-1,k,n)),
     &               depthv(i,j))
      v(i,j,1,n)=(v(i,j,1,n)*util1(i,j)
     &           +v(i,j,k,n)*(util2(i,j)-util1(i,j)))/util2(i,j)
      enddo !k
      endif !ip
      enddo !i
c
      enddo !j
!$OMP END PARALLEL DO
c
cdiag if (itest.gt.0 .and. jtest.gt.0) then
cdiag   write (lp,103) nstep,itest+i0,jtest+j0,
cdiag&  '  exiting  convec:  temp    saln    dens    thkns    dpth',
cdiag&  (k,temp(itest,jtest,k,n),saln(itest,jtest,k,n),
cdiag&  th3d(itest,jtest,k,n)+thbase,dp(itest,jtest,k,n)*qonem,
cdiag&  p(itest,jtest,k+1)*qonem,k=1,kk)
cdiag endif
c
      return
      end
c
c
c> Revision history:
c>
c> July 1997 - deleted final pressure calculation loop (moved to to thermf.f)
c> Apr. 1999 - added calculation of -th3d-
c> Aug. 2000 - convcm: adapted from micom 2.8 to run within hycom 1.0
c> Oct. 1999 - convch: convection of u and v added
c> Oct. 2009 - convcm: MPI bug fix.
c> Oct  2010 - replaced two calls to dsiglocdX with one call at mid-point
c> Aug  2011 - replaced dpold,dpoldm with dpo
c> May  2014 - use land/sea masks (e.g. ip) to skip land
