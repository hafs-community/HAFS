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
      subroutine thermf_oi(m,n)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n
c
c --- ----------------------------------------------------------
c --- thermal forcing - combine ocean and sea ice surface fluxes
c ---                 - complete surface salinity forcing
c ---                 - allow for ice shelves (no surface flux)
c --- ----------------------------------------------------------
c
      integer i,j,k,l
      real*8  d1,d2
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        if     (ishelf.ne.0) then
          do i=1,ii
            if (SEA_P) then
              if     (ishlf(i,j).eq.1) then  !standard ocean point
                sswflx(i,j) = (1.0-covice(i,j))*sswflx(i,j) +
     &                                          fswice(i,j)   !cell average
                surflx(i,j) = (1.0-covice(i,j))*surflx(i,j) +
     &                                          flxice(i,j)   !cell average
                sstflx(i,j) = (1.0-covice(i,j))*sstflx(i,j)   !relax over ocean
                salflx(i,j) = (1.0-covice(i,j))*salflx(i,j) +
     &                                          sflice(i,j) + !cell average
     &                                          rivflx(i,j) + !rivers everywhere
     &                                          sssflx(i,j)   !relax  everywhere
              else  !under an ice shelf
                sswflx(i,j) = 0.0
                surflx(i,j) = 0.0
                sstflx(i,j) = 0.0
                salflx(i,j) = 0.0
              endif
               util1(i,j) = surflx(i,j)*scp2(i,j)
               util2(i,j) = salflx(i,j)*scp2(i,j)
            endif !ip
          enddo !i
        elseif (iceflg.ne.0) then
          do i=1,ii
            if (SEA_P) then
              sswflx(i,j) = (1.0-covice(i,j))*sswflx(i,j) +
     &                                        fswice(i,j)   !cell average
              surflx(i,j) = (1.0-covice(i,j))*surflx(i,j) +
     &                                        flxice(i,j)   !cell average
              sstflx(i,j) = (1.0-covice(i,j))*sstflx(i,j)   !relax over ocean
              salflx(i,j) = (1.0-covice(i,j))*salflx(i,j) +
     &                                        sflice(i,j) + !cell average
     &                                        rivflx(i,j) + !rivers everywhere
     &                                        sssflx(i,j)   !relax  everywhere
               util1(i,j) = surflx(i,j)*scp2(i,j)
               util2(i,j) = salflx(i,j)*scp2(i,j)
            endif !ip
          enddo !i
        else !covice(:,:)==0.0
          do i=1,ii
            if (SEA_P) then
              salflx(i,j) = salflx(i,j) + rivflx(i,j) + sssflx(i,j)
               util1(i,j) = surflx(i,j)*scp2(i,j)
               util2(i,j) = salflx(i,j)*scp2(i,j)
            endif !ip
          enddo !i
        endif !iceflg


        if     (epmass) then
          do i=1,ii
            if (SEA_P) then    
c ---         change total water depth by the water exchanged with the atmos.
              if     (btrlfr) then
                pbavg(i,j,n) = pbavg(i,j,n)-
     &                         onem*    delt1*salflx(i,j)/
     &                                         (saln(i,j,1,n)*qthref)
              else
                pbavg(i,j,n) = pbavg(i,j,n)-
     &                         onem*0.5*delt1*salflx(i,j)/
     &                                         (saln(i,j,1,n)*qthref)
              endif !btrlfr:else
            endif !ip
          enddo !i
        endif !epmass
      enddo !j

!$OMP END PARALLEL DO
c
      call xcsum(d1, util1,ipa)
      call xcsum(d2, util2,ipa)
      watcum=watcum+d1
      empcum=empcum+d2
c
cdiag if     (itest.gt.0 .and. jtest.gt.0) then
cdiag   write(lp,'(i9,2i5,a/19x,4f10.4)')
cdiag&    nstep,i0+itest,j0+jtest,
cdiag&    '    sswflx    surflx    sstflx    salflx',
cdiag&    sswflx(itest,jtest),
cdiag&    surflx(itest,jtest),
cdiag&    sstflx(itest,jtest),
cdiag&    salflx(itest,jtest)
cdiag endif !test
      return
      end subroutine thermf_oi

      subroutine thermf(m,n, dtime)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n
      real*8  dtime
c
c --- ---------------
c --- thermal forcing
c --- note: on exit flux is for ocean fraction of each grid cell
c --- ---------------
c
      integer i,j,k,ktr,nm,l, iyear,iday,ihour
      real    day365,pwl,q,utotij,vtotij
      real*8  t1mean,s1mean,tmean,smean,pmean,rmean,
     &        rareac,runsec,secpyr
      real*8  d1,d2,d3,d4
c
      real    pwij(kk+1),trwij(kk,ntracr),
     &        prij(kk+1),trcij(kk,ntracr)
c
      real*8  tmean0,smean0,rmean0
      save    tmean0,smean0,rmean0
c
      double precision dtime_diurnl
      save             dtime_diurnl
      data             dtime_diurnl / -99.d0 /
c
      include 'stmt_fns.h'
c
!$OMP PARALLEL DO PRIVATE(j,k,i,ktr)
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
c --- ----------------------------
c --- thermal forcing at nestwalls
c --- ----------------------------
c
      if (nestfq.ne.0.0 .and. delt1.ne.baclin) then  !not on very 1st time step
c
!$OMP PARALLEL DO PRIVATE(j,i,k,pwl,q)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        do i=1,ii
          if (ip(i,j).eq.1 .and. rmunp(i,j).ne.0.0) then
c ---       Newtonian relaxation with implict time step,
c ---         result is positive if source and nest are positive
c ---       Added by Remy Baraille, SHOM
            k=1
            saln(i,j,k,n)=( saln(i,j,k,n)+delt1*rmunp(i,j)*
     &           (snest(i,j,k,ln0)*wn0+snest(i,j,k,ln1)*wn1) )/
     &                    (1.0+delt1*rmunp(i,j))
            temp(i,j,k,n)=( temp(i,j,k,n)+delt1*rmunp(i,j)*
     &           (tnest(i,j,k,ln0)*wn0+tnest(i,j,k,ln1)*wn1) )/
     &                    (1.0+delt1*rmunp(i,j))
            th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
c
            if     (hybrid) then
              do k=kk,2,-1
                pwl=pnest(i,j,k,ln0)*wn0+pnest(i,j,k,ln1)*wn1
                if     (pwl.gt.p(i,j,kk+1)-tencm) then
                  pwl=p(i,j,kk+1)
                endif
                p(i,j,k)=min(p(i,j,k+1),
     &                       ( p(i,j,k)+delt1*rmunp(i,j)*pwl )/
     &                       (1.0+delt1*rmunp(i,j)))
                dp(i,j,k,n)=p(i,j,k+1)-p(i,j,k)
c
                if     (pwl.lt.p(i,j,kk+1)) then
                  saln(i,j,k,n)=( saln(i,j,k,n)+delt1*rmunp(i,j)*
     &                 (snest(i,j,k,ln0)*wn0+snest(i,j,k,ln1)*wn1) )/
     &                          (1.0+delt1*rmunp(i,j))
                  if     (k.le.nhybrd) then
                    temp(i,j,k,n)=( temp(i,j,k,n)+delt1*rmunp(i,j)*
     &                   (tnest(i,j,k,ln0)*wn0+tnest(i,j,k,ln1)*wn1) )/
     &                            (1.0+delt1*rmunp(i,j))
                    th3d(i,j,k,n)=sig(temp(i,j,k,n),
     &                                saln(i,j,k,n))-thbase
                  else
                    th3d(i,j,k,n)=       theta(i,j,k)
                    temp(i,j,k,n)=tofsig(theta(i,j,k)+thbase,
     &                                   saln(i,j,k,n))
                  endif
                endif
              enddo  !k
              dp(i,j,1,n)=p(i,j,2)-p(i,j,1)
            else  ! isopyc
              do k=kk,2,-1
                saln(i,j,k,n)=( saln(i,j,k,n)+delt1*rmunp(i,j)*
     &               (snest(i,j,k,ln0)*wn0+snest(i,j,k,ln1)*wn1) )/
     &                        (1.0+delt1*rmunp(i,j))
                temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,saln(i,j,k,n))
                if (k.ge.3) then
                  pwl=pnest(i,j,k,ln0)*wn0+pnest(i,j,k,ln1)*wn1
                  pwl=max(p(i,j,2),pwl)
                  if     (pwl.gt.p(i,j,kk+1)-tencm) then
                    pwl=p(i,j,kk+1)
                  endif
                  p(i,j,k)=min(p(i,j,k+1),
     &                         ( p(i,j,k)+delt1*rmunp(i,j)*pwl )/
     &                         (1.0+delt1*rmunp(i,j)))
                endif
                dp(i,j,k,n)=p(i,j,k+1)-p(i,j,k)
              enddo  !k
            endif  ! hybrid:isopyc
c
c ---       minimal tracer support (non-negative in buffer zone).
            do ktr= 1,ntracr
              tracer(i,j,k,n,ktr)=max(tracer(i,j,k,n,ktr),0.0)
            enddo
          endif  !ip.eq.1 .and. rmunp.ne.0.0
c
          if (iu(i,j).eq.1) then
            q  =max(rmunv(i,j),rmunv(i-1,j))
            if     (q.ne.0.0) then
              do k= 1,kk
                pwl=u(i,j,k,n)
                u(i,j,k,n)=( u(i,j,k,n)+delt1*q*
     &             (unest(i,j,k,ln0)*wn0+unest(i,j,k,ln1)*wn1) )/
     &                     (1.0+delt1*q)
              enddo  !k
            endif !rmunv.ne.0.0
          endif  !iu.eq.1
c
          if (iv(i,j).eq.1) then
            q  =max(rmunv(i,j),rmunv(i,j-1))
            if     (q.ne.0.0) then
              do k= 1,kk
                pwl=v(i,j,k,n)
                v(i,j,k,n)=( v(i,j,k,n)+delt1*q*
     &             (vnest(i,j,k,ln0)*wn0+vnest(i,j,k,ln1)*wn1) )/
     &                     (1.0+delt1*q)
              enddo  !k
            endif  !rmunv.ne.0.0
          endif  !iv.eq.1
        enddo  !i
      enddo  !j
!$OMP END PARALLEL DO
c
      endif  !  nestfq.ne.0.0
c
c --- ----------------------------
c --- thermal forcing at sidewalls
c --- ----------------------------
c
      if (relax .and. delt1.ne.baclin) then  !not on very 1st time step
c
!$OMP PARALLEL DO PRIVATE(j,i,k,pwl)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
      do i=1,ii
      if (SEA_P) then
        if (rmu(i,j).ne.0.0) then
c ---     Newtonian relaxation with implict time step,
c ---       result is positive if source and wall are positive
          k=1
          saln(i,j,k,n)=( saln(i,j,k,n)+delt1*rmu(i,j)*
     &       ( swall(i,j,k,lc0)*wc0+swall(i,j,k,lc1)*wc1
     &        +swall(i,j,k,lc2)*wc2+swall(i,j,k,lc3)*wc3) )/
     &                  (1.0+delt1*rmu(i,j))
          if     (lwflag.eq.2 .or. sstflg.gt.2   .or.
     &            icmflg.eq.2 .or. ticegr.eq.0.0     ) then
c ---       use seatmp, since it is the best available SST
            if     (natm.eq.2) then
              temp(i,j,k,n)=( temp(i,j,k,n)+delt1*rmu(i,j)*
     &           ( seatmp(i,j,l0)*w0+seatmp(i,j,l1)*w1) )/
     &                      (1.0+delt1*rmu(i,j))
            else
              temp(i,j,k,n)=( temp(i,j,k,n)+delt1*rmu(i,j)*
     &           ( seatmp(i,j,l0)*w0+seatmp(i,j,l1)*w1
     &            +seatmp(i,j,l2)*w2+seatmp(i,j,l3)*w3) )/
     &                      (1.0+delt1*rmu(i,j))
            endif !natm
          else
            temp(i,j,k,n)=( temp(i,j,k,n)+delt1*rmu(i,j)*
     &         ( twall(i,j,k,lc0)*wc0+twall(i,j,k,lc1)*wc1
     &          +twall(i,j,k,lc2)*wc2+twall(i,j,k,lc3)*wc3) )/
     &                    (1.0+delt1*rmu(i,j))
          endif
          th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
c
          if     (hybrid) then
            do k=kk,2,-1
              pwl=pwall(i,j,k,lc0)*wc0+pwall(i,j,k,lc1)*wc1
     &           +pwall(i,j,k,lc2)*wc2+pwall(i,j,k,lc3)*wc3
              if     (pwl.gt.p(i,j,kk+1)-tencm) then
                pwl=p(i,j,kk+1)
              endif
              p(i,j,k)=min( p(i,j,k+1),
     &                      ( p(i,j,k)+delt1*rmu(i,j)*pwl )/
     &                      (1.0+delt1*rmu(i,j)) )
              dp(i,j,k,n)=p(i,j,k+1)-p(i,j,k)
c
              if     (pwl.lt.p(i,j,kk+1)) then
                saln(i,j,k,n)=( saln(i,j,k,n)+delt1*rmu(i,j)*
     &             ( swall(i,j,k,lc0)*wc0+swall(i,j,k,lc1)*wc1
     &              +swall(i,j,k,lc2)*wc2+swall(i,j,k,lc3)*wc3) )/
     &                        (1.0+delt1*rmu(i,j))
                if     (k.le.nhybrd) then
                  temp(i,j,k,n)=( temp(i,j,k,n)+delt1*rmu(i,j)*
     &               ( twall(i,j,k,lc0)*wc0+twall(i,j,k,lc1)*wc1
     &                +twall(i,j,k,lc2)*wc2+twall(i,j,k,lc3)*wc3) )/
     &                          (1.0+delt1*rmu(i,j))
                  th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
                else
                  th3d(i,j,k,n)=       theta(i,j,k)
                  temp(i,j,k,n)=tofsig(theta(i,j,k)+thbase,
     &                                 saln(i,j,k,n))
                endif !hybrid:else
              endif !pwl.lt.p(i,j,kk+1)
            enddo !k
            dp(i,j,1,n)=p(i,j,2)-p(i,j,1)
          else  ! isopyc
            do k=kk,2,-1
              saln(i,j,k,n)=( saln(i,j,k,n)+delt1*rmu(i,j)*
     &           ( swall(i,j,k,lc0)*wc0+swall(i,j,k,lc1)*wc1
     &            +swall(i,j,k,lc2)*wc2+swall(i,j,k,lc3)*wc3) )/
     &                      (1.0+delt1*rmu(i,j))
              temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,saln(i,j,k,n))
              if (k.ge.3) then
                pwl=pwall(i,j,k,lc0)*wc0+pwall(i,j,k,lc1)*wc1
     &             +pwall(i,j,k,lc2)*wc2+pwall(i,j,k,lc3)*wc3
                pwl=max(p(i,j,2),pwl)
                if     (pwl.gt.p(i,j,kk+1)-tencm) then
                  pwl=p(i,j,kk+1)
                endif
                p(i,j,k)=min(p(i,j,k+1),
     &                       p(i,j,k)+delt1*rmu(i,j)*(pwl-p(i,j,k)))
              endif !k.ge.3
              dp(i,j,k,n)=p(i,j,k+1)-p(i,j,k)
            enddo !k
          endif !hybrid:isopyc
        endif !rmu(i,j).ne.0.0
      endif !ip
      enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
      endif  !  relax = .true.
c
c --- ----------------------------
c --- tracer forcing at sidewalls
c --- ----------------------------
c
      if (trcrlx .and. delt1.ne.baclin) then  !not on very 1st time step
c
!$OMP   PARALLEL DO PRIVATE(j,i,k,ktr,pwij,trwij,prij,trcij)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
            if (SEA_P) then
              if     (rmutra(i,j).ne.0.0) then !at least one mask is non-zero
                prij(1)=0.0
                do k=1,kk
                  prij(k+1) =  prij(k)+dp(i,j,k,n)
                  pwij(k)   =  pwall(i,j,k,lc0)*wc0
     &                        +pwall(i,j,k,lc1)*wc1
     &                        +pwall(i,j,k,lc2)*wc2
     &                        +pwall(i,j,k,lc3)*wc3
                  do ktr= 1,ntracr
                    trwij(k,ktr) =  trwall(i,j,k,lc0,ktr)*wc0
     &                             +trwall(i,j,k,lc1,ktr)*wc1
     &                             +trwall(i,j,k,lc2,ktr)*wc2
     &                             +trwall(i,j,k,lc3,ktr)*wc3
                  enddo !ktr
                enddo !k
                pwij(kk+1)=prij(kk+1)
*               call plctrc(trwij,pwij,kk,ntracr,
*    &                      trcij,prij,kk        )
                call plmtrc(trwij,pwij,kk,ntracr,
     &                      trcij,prij,kk        )
                do ktr= 1,ntracr
                  if     (rmutr(i,j,ktr).ne.0.0) then
                    do k=1,kk
                      tracer(i,j,k,n,ktr) = ( tracer(i,j,k,n,ktr)+
     &                          delt1*rmutr(i,j,ktr)*trcij(k,ktr) )/
     &                                      (1.0+delt1*rmutr(i,j,ktr))
                    enddo !k
                  endif !rmutr.ktr.ne.0.0
                enddo !ktr
              endif !rmutra.ne.0.0
            endif !ip
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
c
      endif  !  trcrlx = .true.
c
c --- ---------------------------------------------------------
c --- Update dpu,dpv, and rebalance velocity, if dp has changed
c --- ---------------------------------------------------------
c
      if ((nestfq.ne.0.0 .and. delt1.ne.baclin) .or.
     &    (relax         .and. delt1.ne.baclin)     ) then
        call dpudpv(dpu(1-nbdy,1-nbdy,1,n),
     &              dpv(1-nbdy,1-nbdy,1,n),
     &              p,depthu,depthv, 0,0)
c
!$OMP   PARALLEL DO PRIVATE(j,i,k,utotij,vtotij)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
            if (iu(i,j).eq.1 .and.
     &          max(rmunv(i,j),rmunv(i-1,j),
     &              rmu(  i,j),rmu(  i-1,j) ).ne.0.0) then
              utotij = 0.0                                     
              do k=1,kk                                        
                utotij = utotij + u(i,j,k,n)*dpu(i,j,k,n)
              enddo ! k
              utotij=utotij/depthu(i,j)
              do k=1,kk
                u(i,j,k,n) = u(i,j,k,n) - utotij
              enddo ! k
            endif  !rebalance u
c
            if (iv(i,j).eq.1 .and.
     &          max(rmunv(i,j),rmunv(i,j-1),
     &              rmu(  i,j),rmu(  i,j-1) ).ne.0.0) then
              vtotij = 0.0
              do k=1,kk
                vtotij = vtotij + v(i,j,k,n)*dpv(i,j,k,n)
              enddo ! k
              vtotij=vtotij/depthv(i,j)
              do k=1,kk
                v(i,j,k,n) = v(i,j,k,n) - vtotij
              enddo ! k
            endif  !rebalance v
          enddo  !i
        enddo  !j
!$OMP   END PARALLEL DO
      endif !update dpu,dpv and rebalance u,v
c
c --- --------------------------------
c --- thermal forcing of ocean surface
c --- --------------------------------
c
c
      if (thermo .or. sstflg.gt.0 .or. srelax) then
c
      if     (dswflg.eq.1 .and. dtime-dtime_diurnl.gt.1.0) then
c ---   update diurnal factor table
        call forday(dtime,yrflag, iyear,iday,ihour)
        day365 = mod(iday+364,365)
        call thermf_diurnal(diurnl, day365)
        dtime_diurnl = dtime
cdiag       if     (mnproc.eq.1) then
cdiag       write (lp,'(a)') 'diurnl updated'
cdiag       endif !1st tile
      endif
c
!$OMP PARALLEL DO PRIVATE(j)
!$OMP&             SHARED(m,n)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        call thermfj(m,n,dtime, j)
      enddo
!$OMP END PARALLEL DO
c
c<-hsk
c      if     (itest.gt.0 .and. jtest.gt.0) then
c        write(lp,'(i9,2i5,a/22x,4f10.4)')
c     &    nstep,i0+itest,j0+jtest,
c     &    ' therm-L491    sstflx     ustar    hekman    surflx',
c     &    sstflx(itest,jtest),
c     &    ustar( itest,jtest),
c     &    hekman(itest,jtest),
c     &    surflx(itest,jtest)
cdiag   write(lp,'(i9,2i5,a/19x,4f10.4)')
cdiag&    nstep,i0+itest,j0+jtest,
cdiag&    '    sswflx     salflx   rivflx    sssflx',
cdiag&    sswflx(itest,jtest),
cdiag&    salflx(itest,jtest),
cdiag&    rivflx(itest,jtest),
cdiag&    sssflx(itest,jtest)
c      endif !hsk 
c
c --- smooth surface fluxes?
c
      if     (flxsmo) then
        call psmooth_ice(surflx, 0,0, ishlf, util1)  !uses covice
        call psmooth_ice(salflx, 0,0, ishlf, util1)  !uses covice
      endif
c
      if (nstep.eq.nstep1+1 .or. diagno) then
        if (nstep.eq.nstep1+1) then
          nm=m
        else
          nm=n
        endif
!$OMP   PARALLEL DO PRIVATE(j,k,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
            if (SEA_P) then
              util1(i,j)=temp(i,j,1,nm)*scp2(i,j)
              util2(i,j)=saln(i,j,1,nm)*scp2(i,j)
            endif !ip
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
        call xcsum(d1, util1,ipa)
        call xcsum(d2, util2,ipa)
        t1mean=d1
        s1mean=d2
c
!$OMP   PARALLEL DO PRIVATE(j,k,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          k=1
            do i=1,ii
              if (SEA_P) then
                util1(i,j)=               dp(i,j,k,nm)*scp2(i,j)
                util2(i,j)=temp(i,j,k,nm)*dp(i,j,k,nm)*scp2(i,j)
                util3(i,j)=saln(i,j,k,nm)*dp(i,j,k,nm)*scp2(i,j)
                util4(i,j)=th3d(i,j,k,nm)*dp(i,j,k,nm)*scp2(i,j)
              endif !ip
            enddo !i
          do k=2,kk
            do i=1,ii
              if (SEA_P) then
                util1(i,j)=util1(i,j)+    dp(i,j,k,nm)*scp2(i,j)
                util2(i,j)=util2(i,j)+
     &                     temp(i,j,k,nm)*dp(i,j,k,nm)*scp2(i,j)
                util3(i,j)=util3(i,j)+
     &                     saln(i,j,k,nm)*dp(i,j,k,nm)*scp2(i,j)
                util4(i,j)=util4(i,j)+
     &                     th3d(i,j,k,nm)*dp(i,j,k,nm)*scp2(i,j)
              endif !ip
            enddo !i
          enddo !k
        enddo !j
!$OMP   END PARALLEL DO
        call xcsum(d1, util1,ipa)
        call xcsum(d2, util2,ipa)
        call xcsum(d3, util3,ipa)
        call xcsum(d4, util4,ipa)
        pmean=d1
        tmean=d2/pmean
        smean=d3/pmean
        rmean=d4/pmean
        if     (mnproc.eq.1) then
        write (lp,'(i9,a,3f10.3)') 
     &    nstep,' mean basin temp, saln, dens ',
     &    tmean,smean,rmean+thbase
        endif !1st tile
        if     (nstep.eq.nstep1+1) then
c
c ---     save initial basin means.
          tmean0=tmean
          smean0=smean
          rmean0=rmean
        else
c
c ---     diagnostic printout of fluxes.
          rareac=1.0/(area*(nstep-nstep1))
          runsec=   baclin*(nstep-nstep1)
          if      (yrflag.eq.0) then
            secpyr=360.00d0*86400.0d0
          elseif (yrflag.lt.3) then
            secpyr=366.00d0*86400.0d0
          elseif (yrflag.ge.3) then
            secpyr=365.25d0*86400.0d0
          endif
          if     (mnproc.eq.1) then
          write (lp,'(i9,a,2f10.3)') 
     &     nstep,' mean surface temp and saln  ',
     &     t1mean/area,s1mean/area
          write (lp,'(i9,a,2f10.3,a)') 
     &     nstep,' energy residual (atmos,tot) ',
     &     watcum*rareac,
     &     (tmean-tmean0)*(spcifh*avgbot*qthref)/runsec,
     &    ' (W/m^2)'
c ---     note that empcum is now salflx cum.
          write (lp,'(i9,a,2f10.3,a)')
     &     nstep,'  e - p residual (atmos,tot) ',
     &     empcum*(thref/saln0)*rareac*100.0*secpyr,
     &     (smean-smean0)/(saln0*runsec)*avgbot*100.0*secpyr,
     &    ' (cm/year)'
          write (lp,'(i9,a,2f10.3)') 
     &     nstep,' temp drift per century      ',
     &     (watcum*rareac/(spcifh*avgbot*qthref))*(secpyr*100.0d0),
     &     (tmean-tmean0)*(secpyr*100.0d0)/runsec
          write (lp,'(i9,a,2f10.3)') 
     &     nstep,' saln drift per century      ',
     &     (empcum*rareac/(       avgbot*qthref))*(secpyr*100.0d0),
     &     (smean-smean0)*(secpyr*100.0d0)/runsec
          write (lp,'(i9,a,9x,f10.3)') 
     &     nstep,' dens drift per century      ',
     &     (rmean-rmean0)*(secpyr*100.0d0)/runsec
          endif !1st tile
          call xcsync(flush_lp)
        endif !master
      endif !diagno
cc
      endif   !  thermo .or.  sstflg.gt.0 .or. srelax
c
c CL add flx into cumulative fluxes
c      do j=1,jj
c        do l=1,isp(j)
c          do i=max(1,ifp(j,l)),min(ii,ilp(j,l))
c            flxcumsw(i,j)= flxcumsw(i,j)+sswflx(i,j)*delt1
c            flxcumht(i,j)= flxcumht(i,j)+surflx(i,j)*delt1
c*           flxcumsl(i,j)= flxcumsl(i,j)+salflx(i,j)*delt1 !update salflx in thermf_oi
c          enddo
c        enddo
c      enddo
c      flxcumti=flxcumti+delt1
cCL
      return
      end subroutine thermf
c
      subroutine thermfj(m,n,dtime, j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
#if defined(STOKES)
      use mod_stokes  !    HYCOM Stokes Drift
#endif
      implicit none
c
      integer m,n, j
      real*8  dtime
c
c --- thermal forcing of ocean surface, for row j.
c
      integer i,ihr,it_a,ilat
      real    radfl,swfl,sstrlx,wind,airt,vpmx,prcp,xtau,ytau,
     &        evap,evape,emnp,esst,sssf,
     &        snsibl,dsgdt,sssc,sssdif,sstdif,rmus,rmut
      real    cd0,clh,cl0,cl1,csh,
     &        pair,rair,slat,ssen,tdif,tsur,wsph,
     &        tamts,q,qva,va
      real    swscl,xhr,xlat
      real    u10,v10,uw10,uw
      real    cd_n10,cd_n10_rt,ce_n10,ch_n10,cd_rt,stab,
     &        tv,tstar,qstar,bstar,zeta,x2,x,xx,
     &        psi_m,psi_h,z0,qrair,zi
      real    cd10,ce10,ch10,ustar1
      real*8  dloc
      integer k
c
c --- 'ustrmn' = minimum ustar
c --- 'cormn4' = 4 times minimum coriolis magnitude
c --- 'csubp'  = specific heat of air at constant pressure (j/kg/deg)
c --- 'evaplh' = latent heat of evaporation (j/kg)
c --- 'csice'  = ice-air sensible exchange coefficient
c
      real       ustrmn,cormn4,csubp,evaplh,csice
      parameter (ustrmn=1.0e-5, 
     &           cormn4=4.0e-5,  ! corio(4N) is about 1.e-5
     &           csubp =1005.7,
     &           evaplh=2.47e6,
     &           csice =0.0006)
c
c --- parameter for lwflag=-1
c --- 'sb_cst' = Stefan-Boltzman constant
      real       sb_cst
      parameter (sb_cst=5.67e-8)
c
c --- parameters primarily for flxflg=1 (ustflg=1)
c --- 'airdns' = air density at sea level (kg/m**3)
c --- 'cd'     = drag coefficient
c --- 'ctl'    = thermal transfer coefficient (latent)
c --- 'cts1'   = thermal transfer coefficient (sensible, stable)
c --- 'cts2'   = thermal transfer coefficient (sensible, unstable)
c
      real       airdns,cd,ctl,cts1,cts2
      parameter (airdns=1.2)
      parameter (cd  =0.0013, ctl =0.0012,
     &           cts1=0.0012, cts2=0.0012)
c
c --- parameters primarily for flxflg=2 (ustflg=2)
c --- 'pairc'  = air pressure (Pa) or (mb) * 100
c --- 'rgas'   = gas constant (j/kg/k)
c --- 'tzero'  = celsius to kelvin temperature offset
c --- 'clmin'  = minimum allowed cl
c --- 'clmax'  = maximum allowed cl
c --- 'wsmin'  = minimum allowed wind speed (for cl and cd)
c --- 'wsmax'  = maximum allowed wind speed (for cl and cd)
c
      real       pairc,rgas,tzero,clmin,clmax,wsmin,wsmax
      parameter (pairc=1013.0*100.0,
     &           rgas =287.1,   tzero=273.16, 
     &           clmin=0.0003,  clmax=0.002,
     &           wsmin=3.5,     wsmax=27.5)
c
c --- parameters primarily for flxflg=4
c --- 'lvtc'   = include a virtual temperature correction
c --- 'vamin'  = minimum allowed wind speed (for cl)
c --- 'vamax'  = maximum allowed wind speed (for cl)
c --- 'tdmin'  = minimum allowed Ta-Ts      (for cl)
c --- 'tdmax'  = maximum allowed Ta-Ts      (for cl)
c
c --- 'as0_??' =  stable Ta-Ts  polynominal coefficients, va<=5m/s
c --- 'as5_??' =  stable Ta-Ts  polynominal coefficients, va>=5m/s
c --- 'au0_??' = unstable Ta-Ts polynominal coefficients, va<=5m/s
c --- 'au5_??' = unstable Ta-Ts polynominal coefficients, va>=5m/s
c --- 'an0_??' =  neutral Ta-Ts polynominal coefficients, va<=5m/s
c --- 'an5_??' =  neutral Ta-Ts polynominal coefficients, va>=5m/s
c --- 'ap0_??' =    +0.75 Ta-Ts polynominal coefficients, va<=5m/s
c --- 'ap5_??' =    +0.75 Ta-Ts polynominal coefficients, va>=5m/s
c --- 'am0_??' =    -0.75 Ta-Ts polynominal coefficients, va<=5m/s
c --- 'am5_??' =    -0.75 Ta-Ts polynominal coefficients, va>=5m/s
c
      logical, parameter :: lvtc =.true.
      real,    parameter :: vamin= 1.2, vamax=34.0
      real,    parameter :: tdmin=-8.0, tdmax= 7.0
c
      real, parameter ::
     &  as0_00=-2.925e-4,   as0_10= 7.272e-5,  as0_20=-6.948e-6,
     &  as0_01= 5.498e-4,   as0_11=-1.740e-4,  as0_21= 1.637e-5,
     &  as0_02=-5.544e-5,   as0_12= 2.489e-5,  as0_22=-2.618e-6
      real, parameter ::
     &  as5_00= 1.023e-3,   as5_10=-2.672e-6,  as5_20= 1.546e-6,
     &  as5_01= 9.657e-6,   as5_11= 2.103e-4,  as5_21=-6.228e-5,
     &  as5_02=-2.281e-8,   as5_12=-5.329e-3,  as5_22= 5.094e-4
      real, parameter ::
     &  au0_00= 2.077e-3,   au0_10=-2.899e-4,  au0_20=-1.954e-5,
     &  au0_01=-3.933e-4,   au0_11= 7.350e-5,  au0_21= 5.483e-6,
     &  au0_02= 3.971e-5,   au0_12=-6.267e-6,  au0_22=-4.867e-7
      real, parameter ::
     &  au5_00= 1.074e-3,   au5_10= 6.912e-6,  au5_20= 1.849e-7,
     &  au5_01= 5.579e-6,   au5_11=-2.244e-4,  au5_21=-2.167e-6,
     &  au5_02= 5.263e-8,   au5_12=-1.027e-3,  au5_22=-1.010e-4
      real, parameter ::
     &  an0_00= 1.14086e-3, an5_00= 1.073e-3,
     &  an0_01=-3.120e-6,   an5_01= 5.531e-6,
     &  an0_02=-9.300e-7,   an5_02= 5.433e-8
      real, parameter ::
     &  ap0_00= as0_00 + as0_10*0.75 + as0_20*0.75**2,
     &  ap0_01= as0_01 + as0_11*0.75 + as0_21*0.75**2,
     &  ap0_02= as0_02 + as0_12*0.75 + as0_22*0.75**2
      real, parameter ::
     &  ap5_00= as5_00 + as5_10*0.75 + as5_20*0.75**2,
     &  ap5_01= as5_01,
     &  ap5_02= as5_02,
     &  ap5_11=          as5_11*0.75 + as5_21*0.75**2,
     &  ap5_12=          as5_12*0.75 + as5_22*0.75**2
      real, parameter ::
     &  am0_00= au0_00 - au0_10*0.75 + au0_20*0.75**2,
     &  am0_01= au0_01 - au0_11*0.75 + au0_21*0.75**2,
     &  am0_02= au0_02 - au0_12*0.75 + au0_22*0.75**2
      real, parameter ::
     &  am5_00= au5_00 - au5_10*0.75 + au5_20*0.75**2,
     &  am5_01= au5_01,
     &  am5_02= au5_02,
     &  am5_11=        - au5_11*0.75 + au5_21*0.75**2,
     &  am5_12=        - au5_12*0.75 + au5_22*0.75**2
c
c --- parameters primarily for flxflg=4
      real, parameter :: vonkar=0.4         !Von Karmann constant
      real, parameter :: cpcore=1000.5      !specific heat of air (j/kg/deg)
c
      real satvpr,qsatur6,qsatur,qsatur5,t6,p6,f6,qra
      include 'stmt_fns.h'  !declares t
c
c --- saturation vapor pressure (Pa),
c --- from a polynominal approximation (lowe, j.appl.met., 16, 100-103, 1976)
      satvpr(t)=  100.0*(6.107799961e+00+t*(4.436518521e-01
     &               +t*(1.428945805e-02+t*(2.650648471e-04
     &               +t*(3.031240396e-06+t*(2.034080948e-08
     &               +t* 6.136820929e-11))))))
c
c --- pressure dependent saturation mixing ratio (kg/kg)
c --- p6 is pressure in Pa, f6 is fractional depression from SSS
      qsatur6(t6,p6,f6)=0.622*(f6*satvpr(t6)/(p6-f6*satvpr(t6)))
c
c --- saturation mixing ratio (kg/kg), from a polynominal approximation
c --- for saturation vapor pressure (lowe, j.appl.met., 16, 100-103, 1976)
c --- assumes that (mslprs-satvpr(t)) is 1.e5 Pa
      qsatur(t)=.622e-3*(6.107799961e+00+t*(4.436518521e-01
     &               +t*(1.428945805e-02+t*(2.650648471e-04
     &               +t*(3.031240396e-06+t*(2.034080948e-08
     &               +t* 6.136820929e-11))))))
c
c --- saturation specific humidity (flxflg=5)
c --- qra is 1/rair
      qsatur5(t,qra)= 0.98*qra*6.40380e5*exp(-5107.4/(t+tzero))
c
c --- salinity relaxation coefficient
      rmus=1./(30.0*86400.0)  !1/30 days
c
c --- temperature relaxation coefficient
      rmut=1./(30.0*86400.0)  !1/30 days
c
c --- ------------------------------------------------------
c --- thermal forcing of ocean surface (positive into ocean)
c --- ------------------------------------------------------
c
      do i=1,ii
      if (SEA_P) then
      if     (flxflg.gt.0) then
c ---   wind = wind, or wind-ocean, speed (m/s)
        if     (flxflg.eq.6) then
          wind=wndocn(i,j)  !magnitude of wind minus ocean current
        elseif (natm.eq.2) then
          wind=wndspd(i,j,l0)*w0+wndspd(i,j,l1)*w1
        else
          wind=wndspd(i,j,l0)*w0+wndspd(i,j,l1)*w1
     &        +wndspd(i,j,l2)*w2+wndspd(i,j,l3)*w3
        endif !natm
c ---   swfl = shortwave radiative thermal flux (W/m^2) +ve into ocean/ice
c ---          Qsw includes the atmos. model's surface albedo,
c ---          i.e. it already allows for sea-ice&snow where it is observed.
        if     (natm.eq.2) then
          swfl=swflx (i,j,l0)*w0+swflx (i,j,l1)*w1
        else
          swfl=swflx (i,j,l0)*w0+swflx (i,j,l1)*w1
     &        +swflx (i,j,l2)*w2+swflx (i,j,l3)*w3
        endif !natm
        if     (dswflg.eq.1) then
c ---     daily to diurnal shortwave correction to swfl and radfl.
          dloc  = dtime + plon(i,j)/360.0
          xhr   = (dloc - int(dloc))*24.0  !local time of day
          ihr   = int(xhr)
          xhr   =     xhr - ihr
          if     (plat(i,j).ge.0.0) then
            ilat  = int(plat(i,j))
            xlat  =     plat(i,j) - ilat
          else
            ilat  = int(plat(i,j)) - 1
            xlat  =     plat(i,j) - ilat
          endif
          swscl = (1.0-xhr)*(1.0-xlat)*diurnl(ihr,  ilat  ) +
     &            (1.0-xhr)*     xlat *diurnl(ihr,  ilat+1) +
     &                 xhr *(1.0-xlat)*diurnl(ihr+1,ilat  ) +
     &                 xhr *     xlat *diurnl(ihr+1,ilat+1)
          radfl = (swscl-1.0)*swfl  !diurnal correction only
          swfl  =  swscl     *swfl
cdiag         if     (i.eq.itest.and.j.eq.jtest) then
cdiag           write(lp,'(i9,a,2i5,2f8.5)')
cdiag.            nstep,', hr,lat =',ihr,ilat,xhr,xlat
cdiag           write(lp,'(i9,a,5f8.5)')
cdiag.            nstep,', swscl  =',swscl,diurnl(ihr,  ilat  ),
cdiag.                                     diurnl(ihr,  ilat+1),
cdiag.                                     diurnl(ihr+1,ilat  ),
cdiag.                                     diurnl(ihr+1,ilat+1)
cdiag           call flush(lp)
cdiag         endif !test
        else
          radfl = 0.0 !no diurnal correction
c<-hsk
#if defined(USE_SCOUPLER)
          if(radflx_(i,j).ne.cflag)radfl=radflx_(i,j)
          if(swflx_(i,j).ne.cflag)swfl=swflx_(i,j)
#endif
        endif !dswflg
c ---   radfl= net       radiative thermal flux (W/m^2) +ve into ocean/ice
c ---        = Qsw+Qlw across the atmosphere to ocean or sea-ice interface
c ---   existing radfl is the diurnal Qsw correction only
        if     (natm.eq.2) then
          radfl=radfl + (radflx(i,j,l0)*w0+radflx(i,j,l1)*w1)
        else
          radfl=radfl + (radflx(i,j,l0)*w0+radflx(i,j,l1)*w1
     &                  +radflx(i,j,l2)*w2+radflx(i,j,l3)*w3)
        endif !natm
        if     (lwflag.eq.-1) then
c ---     input radflx is Qlwdn, convert to Qlw + Qsw
          radfl = radfl - sb_cst*(temp(i,j,1,n)+tzero)**4 + swfl
          sstflx(i,j) = 0.0
        elseif (lwflag.gt.0) then
c ---     over-ocean longwave correction to radfl (Qsw+Qlw).
          tsur = temp(i,j,1,n)
          if     (lwflag.eq.1) then !from climatology
            tdif = tsur -
     &             ( twall(i,j,1,lc0)*wc0+twall(i,j,1,lc1)*wc1
     &              +twall(i,j,1,lc2)*wc2+twall(i,j,1,lc3)*wc3)
          else !w.r.t. atmospheric model's sst
            if     (natm.eq.2) then
              tdif = tsur -
     &               ( surtmp(i,j,l0)*w0+surtmp(i,j,l1)*w1)
            else
              tdif = tsur -
     &               ( surtmp(i,j,l0)*w0+surtmp(i,j,l1)*w1
     &                +surtmp(i,j,l2)*w2+surtmp(i,j,l3)*w3)
            endif !natm
          endif
          !correction is blackbody radiation from tdif at tsur
          radfl = radfl - (4.506+0.0554*tsur) * tdif
          !count the correction as a relaxation term
          sstflx(i,j) = - (4.506+0.0554*tsur) * tdif
        else
          sstflx(i,j) = 0.0
        endif
c
        if     (pcipf) then
c ---     prcp = precipitation (m/sec; positive into ocean)
c ---     note that if empflg==3, this is actually P-E
          if     (natm.eq.2) then
            prcp=precip(i,j,l0)*w0+precip(i,j,l1)*w1
          else
            prcp=precip(i,j,l0)*w0+precip(i,j,l1)*w1
     &          +precip(i,j,l2)*w2+precip(i,j,l3)*w3
          endif !natm
        endif
c<-hsk NCEP-coupler
#if defined(USE_SCOUPLER)
        if(precip_(i,j).ne.cflag)prcp=precip_(i,j)
#endif
c-> hsk
        if     (empflg.lt.0) then  !observed (or NWP) SST
          if     (natm.eq.2) then
            esst = seatmp(i,j,l0)*w0+seatmp(i,j,l1)*w1
          else
            esst = seatmp(i,j,l0)*w0+seatmp(i,j,l1)*w1+
     &             seatmp(i,j,l2)*w2+seatmp(i,j,l3)*w3
          endif !natm
        endif
        if     (flxflg.ne.3) then
          if     (natm.eq.2) then
            airt=airtmp(i,j,l0)*w0+airtmp(i,j,l1)*w1
            vpmx=vapmix(i,j,l0)*w0+vapmix(i,j,l1)*w1
          else
c ---       airt = air temperature (C)
            airt=airtmp(i,j,l0)*w0+airtmp(i,j,l1)*w1
     &          +airtmp(i,j,l2)*w2+airtmp(i,j,l3)*w3
c ---       vpmx = water vapor mixing ratio (kg/kg)
            vpmx=vapmix(i,j,l0)*w0+vapmix(i,j,l1)*w1
     &          +vapmix(i,j,l2)*w2+vapmix(i,j,l3)*w3
          endif !natm
        endif
c ---   pair = msl pressure (Pa)
        if     (mslprf .or. flxflg.eq.6) then
          if     (natm.eq.2) then
            pair=mslprs(i,j,l0)*w0+mslprs(i,j,l1)*w1
     &          +prsbas
          else
            pair=mslprs(i,j,l0)*w0+mslprs(i,j,l1)*w1
     &          +mslprs(i,j,l2)*w2+mslprs(i,j,l3)*w3
     &          +prsbas
          endif !natm
        else
          pair=pairc
        endif
c<-hsk NCEP-coupler
#if defined(USE_SCOUPLER)
       if(mslprs_(i,j).ne.cflag) pair=mslprs_(i,j)+prsbas
#endif
c-> hsk
c ---   ustar = U* (sqrt(N.m/kg))                 
        if     (ustflg.eq.3) then !ustar from input
          if     (natm.eq.2) then
            ustar(i,j)=ustara(i,j,l0)*w0+ustara(i,j,l1)*w1
          else
            ustar(i,j)=ustara(i,j,l0)*w0+ustara(i,j,l1)*w1
     &                +ustara(i,j,l2)*w2+ustara(i,j,l3)*w3
          endif !natm
        elseif (ustflg.eq.1) then !ustar from wndspd, constant cd
          ustar(i,j)=sqrt(thref*cd*airdns)*wind
        elseif (ustflg.eq.2) then !ustar from wndspd, variable cd
          wsph = min( wsmax, max( wsmin, wind ) )
          cd0  = 0.862e-3 + 0.088e-3 * wsph - 0.00089e-3 * wsph**2
          rair = pair / (rgas * ( tzero + airt ))
          ustar(i,j)=sqrt(thref*cd0*rair)*wind
        elseif (ustflg.eq.4) then !ustar from surface stress, see montum_hs
          ustar(i,j)=sqrt(thref*sqrt(surtx(i,j)**2+surty(i,j)**2))
        endif !ustflg
        ustar( i,j)=max(ustrmn,ustar(i,j))
        hekman(i,j)=ustar(i,j)*(cekman*4.0)/
     &               max( cormn4,
     &                    abs(corio(i,j  ))+abs(corio(i+1,j  ))+
     &                    abs(corio(i,j+1))+abs(corio(i+1,j+1)) )
c<-hsk2018
c        if(i.eq.itest.and.j.eq.jtest) then
c         write(lp,'(i9,a,4f12.5)') nstep,'hsk-ustar,surtx/y,radfl=',
c     &    ustar(i,j),
c     &    surtx(i,j),surty(i,j),
c     &    radfl
c         call flush(lp)
c        endif
c->hsk
      else !flxlfg==0, i.e. no flux
        swfl=0.0
        sstflx(i,j)=0.0
        mixflx(i,j)=0.0
        buoflx(i,j)=0.0
        bhtflx(i,j)=0.0
        ustar( i,j)=0.0
        hekman(i,j)=0.0
      endif !flxflg
c
      if     (flxflg.eq.1) then
c
c ---   MICOM bulk air-sea flux parameterization
c ---   (constant Cl and constant stable/unstable Cs)
c
        if (temp(i,j,1,n).lt.airt) then
          csh=cts1  !stable
        else
          csh=cts2  !unstable
        endif
c ---   evap   = evaporation (W/m^2) into atmos from ocean.
c ---   snsibl = sensible heat flux  into atmos from ocean.
        if     (empflg.lt.0) then
          evape = ctl*airdns*evaplh*wind*
     &            max(0.,0.97*qsatur(esst)-vpmx)
        endif
        evap  =ctl*airdns*evaplh*wind*
     &         max(0.,0.97*qsatur(temp(i,j,1,n))-vpmx)
        snsibl=csh*airdns*csubp*wind*(temp(i,j,1,n)-airt)
c ---   surflx = thermal energy flux (W/m^2) into ocean
        surflx(i,j)=radfl - snsibl - evap
      elseif (flxflg.eq.2) then
c
c ---    Cl (and Cs) depend on wind speed and Ta-Ts.
c ---    Kara, A. B., P. A. Rochford, and H. E. Hurlburt, 2002:
c ---    Air-sea flux estimates and the 1997-1998 ENSO event.
c ---    Bound.-Layer Meteor., 103, 439-458.
c ---    http://www7320.nrlssc.navy.mil/pubs.php
c
        rair = pair / (rgas * ( tzero + airt ))
        slat = evaplh*rair
        ssen = csubp *rair
c
        tdif = temp(i,j,1,n) - airt
        wsph = min( wsmax, max( wsmin, wind ) )
        cl0  =  0.885e-3 + 0.0748e-3 * wsph - 0.00143e-3 * wsph**2
        cl1  = -0.113e-4 + 4.89e-4   / wsph
        clh  = min( clmax, max( clmin, cl0 + cl1 * tdif ) )
        csh  = 0.9554*clh
c
c ---   evap   = evaporation         (W/m^2) into atmos from ocean.
c ---   snsibl = sensible heat flux  (W/m^2) into atmos from ocean.
c ---   surflx = thermal energy flux (W/m^2) into ocean
        if     (empflg.lt.0) then
          evape = slat*clh*wind*(0.97*qsatur(esst)-vpmx)
        endif
        evap   = slat*clh*wind*(0.97*qsatur(temp(i,j,1,n))-vpmx)
        snsibl = ssen*csh*wind* tdif
        surflx(i,j) = radfl - snsibl - evap
c
cdiag   if     (i.eq.itest.and.j.eq.jtest) then
cdiag     write(lp,'(i9,2i5,a,4f8.5)')
cdiag.    nstep,i0+i,j0+j,' cl0,cl,cs,cd    = ',cl0,clh,csh,cd0
cdiag     write(lp,'(i9,2i5,a,2f8.2,f8.5)')
cdiag.    nstep,i0+i,j0+j,' wsph,tdif,ustar = ',wsph,tdif,ustar(i,j)
cdiag     call flush(lp)
cdiag   endif
      elseif (flxflg.eq.4) then
c
c ---   Similar to flxflg.eq.2, but with Cl based on an approximation
c ---   to values from the COARE 3.0 algorithm (Fairall et al., 2003), 
c ---   for Cl over the global ocean in the range 1m/s <= Va <= 40m/s
c ---   and -8degC <= Ta-Ts <= 7degC, that is quadratic in Ta-Ts and
c ---   quadratic in either Va or 1/Va (Kara et al.,  2005).
c
c ---   Fairall, C. W., E. F. Bradley, J. E. Hare, A. A. Grachev, and J. B.
c ---   Edson, 2003:  Bulk parameterization of air-sea fluxes:  Updates 
c ---   and verification for the COARE algorithm.  J. Climate, 16, 571-591.
c
c ---   Kara, A. B., H. E. Hurlburt, and A. J. Wallcraft, 2005:
c ---   Stability-dependent exchange coefficients for air-sea fluxes.
c ---   J. Atmos. Oceanic. Technol., 22, 1080-1094. 
c ---   http://www7320.nrlssc.navy.mil/pubs.php
c
        rair = pair / (rgas * ( tzero + airt ))
        slat = evaplh*rair
        ssen = csubp *rair
c
        tdif  = temp(i,j,1,n) - airt
        if     (lvtc) then !correct tamts for 100% humidity
          tamts = -tdif - 0.61*(airt+tzero)*(qsatur(airt)-vpmx)
          tamts = min( tdmax, max( tdmin, tamts ) )
        else
          tamts = min( tdmax, max( tdmin, -tdif ) )
        endif !lvtc:else
        va    = min( vamax, max( vamin,  wind ) )
        if     (va.le.5.0) then
          if     (tamts.gt. 0.75) then !stable
            clh =   (as0_00 + as0_01* va + as0_02* va**2)
     &            + (as0_10 + as0_11* va + as0_12* va**2)*tamts
     &            + (as0_20 + as0_21* va + as0_22* va**2)*tamts**2
          elseif (tamts.lt.-0.75) then !unstable
            clh =   (au0_00 + au0_01* va + au0_02* va**2)
     &            + (au0_10 + au0_11* va + au0_12* va**2)*tamts
     &            + (au0_20 + au0_21* va + au0_22* va**2)*tamts**2
          elseif (tamts.ge.-0.098)  then
            q = (tamts-0.75)/0.848  !linear between  0.75 and -0.098
            q = q**2  !favor  0.75
            clh = (1.0-q)*(ap0_00 + ap0_01* va + ap0_02* va**2)
     &               + q *(an0_00 + an0_01* va + an0_02* va**2)
          else
            q = (tamts+0.75)/0.652  !linear between -0.75 and -0.098
            q = q**2  !favor -0.75
            clh = (1.0-q)*(am0_00 + am0_01* va + am0_02* va**2)
     &               + q *(an0_00 + an0_01* va + an0_02* va**2)
          endif !tamts
        else !va>5
          qva = 1.0/va
          if     (tamts.gt. 0.75) then !stable
            clh =   (as5_00 + as5_01* va + as5_02* va**2)
     &            + (as5_10 + as5_11*qva + as5_12*qva**2)*tamts
     &            + (as5_20 + as5_21*qva + as5_22*qva**2)*tamts**2
          elseif (tamts.lt.-0.75) then !unstable
            clh =   (au5_00 + au5_01* va + au5_02* va**2)
     &            + (au5_10 + au5_11*qva + au5_12*qva**2)*tamts
     &            + (au5_20 + au5_21*qva + au5_22*qva**2)*tamts**2
          elseif (tamts.ge.-0.098)  then
            q = (tamts-0.75)/0.848  !linear between  0.75 and -0.098
            q = q**2  !favor  0.75
            clh = (1.0-q)*(ap5_00 + ap5_01* va + ap5_02* va**2
     &                            + ap5_11*qva + ap5_12*qva**2)
     &               + q *(an5_00 + an5_01* va + an5_02* va**2)
          else
            q = (tamts+0.75)/0.652  !linear between -0.75 and -0.098
            q = q**2  !favor -0.75
            clh = (1.0-q)*(am5_00 + am5_01* va + am5_02* va**2
     &                            + am5_11*qva + am5_12*qva**2)
     &               + q *(an5_00 + an5_01* va + an5_02* va**2)
          endif !tamts
        endif !va
        csh  = 0.9554*clh
c
c ---   evap   = evaporation         (W/m^2) into atmos from ocean.
c ---   snsibl = sensible heat flux  (W/m^2) into atmos from ocean.
c ---   surflx = thermal energy flux (W/m^2) into ocean
        if     (empflg.lt.0) then
          evape = slat*clh*wind*(0.97*qsatur(esst)-vpmx)
        endif
        evap   = slat*clh*wind*(0.97*qsatur(temp(i,j,1,n))-vpmx)
        snsibl = ssen*csh*wind* tdif
        surflx(i,j) = radfl - snsibl - evap
c
cdiag   if     (i.eq.itest.and.j.eq.jtest) then
cdiag     write(lp,'(i9,2i5,a,3f8.5)')
cdiag.    nstep,i0+i,j0+j,' cl,cs,cd    = ',clh,csh,cd0
cdiag     write(lp,'(i9,2i5,a,2f8.2,f8.5)')
cdiag.    nstep,i0+i,j0+j,' va,tamst,ustar = ',va,tamts,ustar(i,j)
cdiag     call flush(lp)
cdiag   endif
      elseif (flxflg.eq.6) then
c
c ---   Similar to flxflg.eq.4, but with more pressure dependance,
c ---   wind-ocean speed, and a SSS dependent depression of satvpr
c
c ---   Cl based on an approximation
c ---   to values from the COARE 3.0 algorithm (Fairall et al., 2003), 
c ---   for Cl over the global ocean in the range 1m/s <= Va <= 40m/s
c ---   and -8degC <= Ta-Ts <= 7degC, that is quadratic in Ta-Ts and
c ---   quadratic in either Va or 1/Va (Kara et al.,  2005).
c
c ---   Fairall, C. W., E. F. Bradley, J. E. Hare, A. A. Grachev, and J. B.
c ---   Edson, 2003:  Bulk parameterization of air-sea fluxes:  Updates 
c ---   and verification for the COARE algorithm.  J. Climate, 16, 571-591.
c
c ---   Kara, A. B., H. E. Hurlburt, and A. J. Wallcraft, 2005:
c ---   Stability-dependent exchange coefficients for air-sea fluxes.
c ---   J. Atmos. Oceanic. Technol., 22, 1080-1094. 
c ---   http://www7320.nrlssc.navy.mil/pubs.php
c
c ---   use virtual temperature for density
        rair = pair / (rgas * ( tzero + airt ) * (1.0+0.608*vpmx) )
        slat = evaplh*rair
        ssen = csubp *rair
c
        tdif  = temp(i,j,1,n) - airt
c ---   correct tamts for 100% humidity
        sssf  = 1.0
        tamts = -tdif - 0.608*(airt+tzero)*
     &                        (qsatur6(airt,pair,sssf)-vpmx)
        tamts = min( tdmax, max( tdmin, tamts ) )
        va    = min( vamax, max( vamin,  wind ) )  !wind=samo
        if     (va.le.5.0) then
          if     (tamts.gt. 0.75) then !stable
            clh =   (as0_00 + as0_01* va + as0_02* va**2)
     &            + (as0_10 + as0_11* va + as0_12* va**2)*tamts
     &            + (as0_20 + as0_21* va + as0_22* va**2)*tamts**2
          elseif (tamts.lt.-0.75) then !unstable
            clh =   (au0_00 + au0_01* va + au0_02* va**2)
     &            + (au0_10 + au0_11* va + au0_12* va**2)*tamts
     &            + (au0_20 + au0_21* va + au0_22* va**2)*tamts**2
          elseif (tamts.ge.-0.098)  then
            q = (tamts-0.75)/0.848  !linear between  0.75 and -0.098
            q = q**2  !favor  0.75
            clh = (1.0-q)*(ap0_00 + ap0_01* va + ap0_02* va**2)
     &               + q *(an0_00 + an0_01* va + an0_02* va**2)
          else
            q = (tamts+0.75)/0.652  !linear between -0.75 and -0.098
            q = q**2  !favor -0.75
            clh = (1.0-q)*(am0_00 + am0_01* va + am0_02* va**2)
     &               + q *(an0_00 + an0_01* va + an0_02* va**2)
          endif !tamts
        else !va>5
          qva = 1.0/va
          if     (tamts.gt. 0.75) then !stable
            clh =   (as5_00 + as5_01* va + as5_02* va**2)
     &            + (as5_10 + as5_11*qva + as5_12*qva**2)*tamts
     &            + (as5_20 + as5_21*qva + as5_22*qva**2)*tamts**2
          elseif (tamts.lt.-0.75) then !unstable
            clh =   (au5_00 + au5_01* va + au5_02* va**2)
     &            + (au5_10 + au5_11*qva + au5_12*qva**2)*tamts
     &            + (au5_20 + au5_21*qva + au5_22*qva**2)*tamts**2
          elseif (tamts.ge.-0.098)  then
            q = (tamts-0.75)/0.848  !linear between  0.75 and -0.098
            q = q**2  !favor  0.75
            clh = (1.0-q)*(ap5_00 + ap5_01* va + ap5_02* va**2
     &                            + ap5_11*qva + ap5_12*qva**2)
     &               + q *(an5_00 + an5_01* va + an5_02* va**2)
          else
            q = (tamts+0.75)/0.652  !linear between -0.75 and -0.098
            q = q**2  !favor -0.75
            clh = (1.0-q)*(am5_00 + am5_01* va + am5_02* va**2
     &                            + am5_11*qva + am5_12*qva**2)
     &               + q *(an5_00 + an5_01* va + an5_02* va**2)
          endif !tamts
        endif !va
        csh  = 0.9554*clh
c
c ---   sssf = fractional depression of satvpr due to SSS
c ---            Sud and Walker (1997), from Witting (1908)
c ---   evap = evaporation (W/m^2) into atmos from ocean.
        sssf = 1.0 - (0.001/1.805)*max(saln(i,j,1,n)-0.03,0.0)
        if     (empflg.lt.0) then
          evape = slat*clh*wind*(qsatur6(esst,pair,sssf)-vpmx)
        endif
        evap   = slat*clh*wind*(qsatur6(temp(i,j,1,n),pair,sssf)-vpmx)
c
c ---   snsibl = sensible heat flux  (W/m^2) into atmos from ocean.
c ---   surflx = thermal energy flux (W/m^2) into ocean
        snsibl = ssen*csh*wind* tdif         !wind=samo
        surflx(i,j) = radfl - snsibl - evap
c
cdiag   if     (i.eq.itest.and.j.eq.jtest) then
cdiag     write(lp,'(i9,2i5,a,3f8.5)')
cdiag.    nstep,i0+i,j0+j,' cl,cs,cd    = ',clh,csh,cd0
cdiag     write(lp,'(i9,2i5,a,2f8.2,f8.5)')
cdiag.    nstep,i0+i,j0+j,' va,tamst,ustar = ',va,tamts,ustar(i,j)
cdiag     call flush(lp)
cdiag   endif
      elseif (flxflg.eq.5) THEN
c ---   CORE v2 Large and Yeager 2009 CLym. Dyn.: The global climatology 
c ---    of an interannually varying air-sea flux dataset.
c ---   The bulk formulae effectively transform the problem of specifying
c ---   the turbulent surface fluxes (at 10m) into one of describing the
c ---   near surface atmospheric state (wind, temperature and humidity).
c
        rair = pairc / (rgas * ( tzero + airt ))  !always uses pairc
        qrair=1.0/rair
c
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
! Over-ocean fluxes following Large and Yeager (used in NCAR models)
! !
! Coded by Mike Winton (Michael.Winton@noaa.gov) in 2004
!
! A bug was found by Laurent Brodeau (brodeau@gmail.com) in 2007.
! Stephen.Griffies@noaa.gov updated the code with the bug fix.
! Script to find the cd,ch,ce to transform fluxes at 10m to surface
! fluxes
! See Large and Yeager 2004 for equations :
! "Diurnal to Decadal Global Forcing For Ocean and Sea-Ice Models:The
! Data Sets
!  and Flux Climatologies", NCAR Technical report.
! The  code below is for values at zi = 10m high
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
        zi=10.0  ! height in m
        tv = (airt+tzero)*(1.0+0.608*vpmx) !in Kelvin
        uw = max(wind, 0.5)      !0.5 m/s floor on wind (undocumented NCAR)
        uw10 = uw                !first guess 10m wind
   
        cd_n10 = (2.7/uw10+0.142+0.0764*uw10)*1.e-3         !L-Y eqn. 6a
        cd_n10_rt = sqrt(cd_n10)
        ce_n10 = 34.6 *cd_n10_rt*1.e-3                      !L-Y eqn. 6b
        stab   = 0.5 + sign(0.5,airt-temp(i,j,1,n))
        ch_n10 = (18.0*stab+32.7*(1-stab))*cd_n10_rt*1.e-3  !L-Y eqn. 6c

        cd10 = cd_n10  !first guess for exchange coeff's at z
        ch10 = ch_n10
        ce10 = ce_n10
        do it_a= 1,3  !Monin-Obukhov iteration
          cd_rt = sqrt(cd10)
          ustar1 = cd_rt*uw                              !L-Y eqn. 7a
          tstar = (ch10/cd_rt)*(airt-temp(i,j,1,n))      !L-Y eqn. 7b
          qstar = (ce10/cd_rt)*(vpmx-qsatur5(temp(i,j,1,n),qrair))  !L-Y eqn. 7c
          bstar = g*(tstar/tv+qstar/(vpmx+1.0/0.608))
          zeta  = vonkar*bstar*zi/(ustar1*ustar1)        !L-Y eqn. 8a
          zeta  = sign( min(abs(zeta),10.0), zeta )      !undocumented NCAR
          x2 = sqrt(abs(1.0-16.0*zeta))                  !L-Y eqn. 8b
          x2 = max(x2, 1.0)                              !undocumented NCAR
          x  = sqrt(x2)
          if (zeta > 0.0) then
              psi_m = -5.0*zeta  !L-Y eqn. 8c
              psi_h = -5.0*zeta  !L-Y eqn. 8c
          else
              psi_m = log((1.0+2.0*x+x2)*(1.0+x2)/8.0)
     &                 -2.0*(atan(x)-atan(1.0))  !L-Y eqn. 8d
              psi_h = 2.0*log((1.0+x2)/2.0)      !L-Y eqn. 8e
          end if
          uw10 = uw/(1.0+cd_n10_rt*(log(zi/10)-psi_m)/vonkar)  !L-Y eqn. 9
          cd_n10 = (2.7/uw10+0.142+0.0764*uw10)*1.e-3          !L-Y eqn. 6a again
          cd_n10_rt = sqrt(cd_n10)
          ce_n10 = 34.6*cd_n10_rt*1.e-3                        !L-Y eqn. 6b again
          stab   = 0.5 + sign(0.5,zeta)
          ch_n10 = (18.0*stab+32.7*(1-stab))*cd_n10_rt*1.e-3   !L-Y eqn. 6c again
          z0 = 10*exp(-vonkar/cd_n10_rt) ! diagnostic
          xx   = (log(zi/10.0)-psi_m)/vonkar
          cd10 = cd_n10/(1.0+cd_n10_rt*xx)**2         !L-Y 10a
          xx   = (log(zi/10.0)-psi_h)/vonkar
          ch10 = ch_n10/(1.0+ch_n10*xx/cd_n10_rt) *
     &                        sqrt(cd10/cd_n10)       !L-Y 10b
          ce10 = ce_n10/(1.0+ce_n10*xx/cd_n10_rt) *
     &                        sqrt(cd10/cd_n10)       !L-Y 10c
        end do !it_a

c ---  Latent Heat flux
        slat = evaplh*rair
        evap = slat*ce10*wind*(qsatur5(temp(i,j,1,n),qrair)-vpmx)
        if     (empflg.lt.0) then
          evape = slat*ce10*wind*(qsatur5(esst,qrair)-vpmx)
        endif

c --- Sensible Heat flux
        ssen   = cpcore*rair
        snsibl = ssen*ch10*wind*(temp(i,j,1,n)-airt)

c --- Total surface fluxes
        surflx(i,j) = radfl - snsibl - evap
      elseif (flxflg.eq.3) then
c
c ---   input radiation flux is the net flux.
c
        evap=0.0
        surflx(i,j)=radfl
      else  ! no flux
        evap=0.0
        surflx(i,j)=0.0  
      endif  ! flxflg
c
#if defined(USE_SCOUPLER)
        if(latent_(i,j).ne.cflag)  evap=latent_(i,j)
        if(snsibl_(i,j).ne.cflag)snsibl=snsibl_(i,j)
        if(radflx_(i,j).ne.cflag) radfl=radflx_(i,j)
        surflx(i,j) = radfl - snsibl - evap
#endif

c --- add a time-invarient net heat flux offset
      if     (flxoff) then
        surflx(i,j)=surflx(i,j)+offlux(i,j)
      endif
c
c --- relax to surface temperature
      if     (sstflg.ge.1) then 
c ---   use a reference relaxation thickness (min. mixed layer depth)
c ---   in shallow water, thkmlt is replaced by the total depth
c ---   actual e-folding time is (dpmixl(i,j,n)/(thkmlt*onem))/rmut
c ---   in shallow water this is (dpmixl(i,j,n)/p(i,j,kk+1)  )/rmut
        if     (sstflg.eq.1) then !climatological sst
        if     (natm.eq.2) then
          sstdif = ( twall(i,j,1,lc0)*wc0+twall(i,j,1,lc1)*wc1) -
     &             temp(i,j,1,n)
          else
          sstdif = ( twall(i,j,1,lc0)*wc0+twall(i,j,1,lc1)*wc1
     &              +twall(i,j,1,lc2)*wc2+twall(i,j,1,lc3)*wc3) -
     &             temp(i,j,1,n)
          endif !natm
        else  !synoptic sst
          if     (natm.eq.2) then
            sstdif = ( seatmp(i,j,l0)*w0+seatmp(i,j,l1)*w1) -
     &               temp(i,j,1,n)
          else
            sstdif = ( seatmp(i,j,l0)*w0+seatmp(i,j,l1)*w1
     &                +seatmp(i,j,l2)*w2+seatmp(i,j,l3)*w3) -
     &               temp(i,j,1,n)
          endif !natm
        endif
        sstrlx=(rmut*spcifh*min(p(i,j,kk+1),thkmlt*onem)/g)*sstdif
        surflx(i,j)=surflx(i,j)+sstrlx
        sstflx(i,j)=sstflx(i,j)+sstrlx
      endif !sstflg
c --- sswflx = shortwave radiative energy flux (W/m^2) into ocean
      sswflx(i,j)=swfl
c --- emnp = evaporation minus precipitation   (m/sec) into atmos.
      if     (.not.pcipf) then
        prcp = 0.0
        emnp = 0.0   !no E-P
      elseif (empflg.eq.3) then
        emnp = -prcp !input prcp is P-E
      elseif (empflg.gt.0) then !E based on model SST
        emnp = evap *thref/evaplh - prcp  !input prcp is P
      else  !E based on observed SST
        emnp = evape*thref/evaplh - prcp  !input prcp is P
      endif
c --- salflx = salt flux (psu m/s kg/m**3) into ocean
      salflx(i,j)=emnp*(saln(i,j,1,n)*qthref)
c --- allow for rivers as a precipitation bogas
      if     (priver) then
        rivflx(i,j) = - ( rivers(i,j,lr0)*wr0+rivers(i,j,lr1)*wr1
     &                   +rivers(i,j,lr2)*wr2+rivers(i,j,lr3)*wr3) *
     &                  (saln(i,j,1,n)*qthref)
*       salflx(i,j) = salflx(i,j)+rivflx(i,j) !update rivflx in thermf_oi
      else
        rivflx(i,j) = 0.0
      endif
c --- relax to surface salinity
      if     (srelax) then
c ---   use a reference relaxation thickness (min. mixed layer depth)
c ---   in shallow water, thkmls is replaced by the total depth
c ---   actual e-folding time is (dpmixl(i,j,n)/(thkmls*onem))/rmus
c ---   in shallow water this is (dpmixl(i,j,n)/p(i,j,kk+1)  )/rmus
c
c ---   sssrmx is the maximum SSS difference for relaxation (psu)
c ---          set negative to stop relaxing entirely at -sssrlx
c ---          the default (sssflg=1) is 99.9, i.e. no limit
c ---          always fully relax when ice covered or when
c ---          fresher than half climatological sss.
c
        sssc   =  swall(i,j,1,lc0)*wc0+swall(i,j,1,lc1)*wc1
     &           +swall(i,j,1,lc2)*wc2+swall(i,j,1,lc3)*wc3
        sssdif = sssc - saln(i,j,1,n)
        if     (saln(i,j,1,n).gt.0.5*sssc .and.
     &          abs(sssdif).gt.abs(sssrmx(i,j))) then  !large sss anomaly
          if     (sssrmx(i,j).lt.0.0) then
            sssdif = covice(i,j)*sssdif !turn off relaxation except under ice
          elseif (sssdif.lt.0.0) then !sssdif < -sssrmx < 0
            sssdif =      covice(i,j)*   sssdif +
     &               (1.0-covice(i,j))*(-sssrmx(i,j))  !limit relaxation
          else !sssdif > sssrmx > 0
            sssdif =      covice(i,j)*    sssdif +
     &               (1.0-covice(i,j))*   sssrmx(i,j)  !limit relaxation
          endif
        endif
        sssflx(i,j)=(rmus*min(p(i,j,kk+1),thkmls*onem)/g)*sssdif
*       salflx(i,j)=salflx(i,j)+sssflx(i,j) !update salflx in thermf_oi
      else
        sssflx(i,j)=0.0
c
      endif !srelax
      endif !ip
      enddo !i
      return
      end subroutine thermfj

      subroutine thermf_diurnal(diurnal, date)
      implicit none
c
      real        diurnal(0:24,-91:91),date
c
c --- Calculate a table of latitude vs hourly scale factors
c --- for the distribution of daily averaged solar radiation
c --- the clear sky insolation formula of Lumb (1964) is used with 
c --- correction for the seasonally varying earth-sun distance.
c --- According to reed (1977) the lumb formula gives values in close
c --- agreement with the daily mean values of the seckel and beaudry 
c --- (1973) formulae derived from data in the smithsonian
c --- meteorological tables --- (list, 1958).
c
c --- Lumb, F. E., 1964: The influence of cloud on hourly amounts of
c --- total solar radiation at sea surface.Quart. J. Roy. Meteor. Soc.
c --- 90, pp43-56.
c
c ---   date = julian type real date - 1.0 (range 0. to 365.), 
c ---          where 00z jan 1 = 0.0.
c
c --- Base on "QRLUMB" created 2-4-81 by Paul J Martin. NORDA Code 322.
c
      real, parameter ::     pi = 3.14159265
      real, parameter :: raddeg = pi/180.0
c
      integer lat,ihr
      real    sindec,cosdec,alatrd,fd,ourang,sinalt,ri,qsum
      real*8  sum
c
c     calc sin and cosin of the declination angle of the sun.
      call declin(date,sindec,cosdec)
c
c     loop through latitudes
      do lat= -90,90
c       calc latitude of site in radians.
        alatrd = lat*raddeg
c
c       loop through hours
        sum = 0.0
        do ihr= 0,23
c         calc hour angle of the sun (the angular distance of the sun
c         from the site, measured to the west) in radians.
          fd     = real(ihr)/24.0
          ourang = (fd-0.5)*2.0*pi
c         calc sine of solar altitude.
          sinalt = sin(alatrd)*sindec+cos(alatrd)*cosdec*cos(ourang)
c
c         calc clear-sky solar insolation from lumb formula.
          if     (sinalt.le.0.0) then
            diurnal(ihr,lat) = 0.0
          else
            ri=1.00002+.01671*cos(0.01720242*(date-2.1))
            diurnal(ihr,lat) = 2793.0*ri*ri*sinalt*(.61+.20*sinalt)
          endif
          sum = sum + diurnal(ihr,lat)
        enddo !ihr
        if     (sum.gt.0.0) then
c         rescale so that sum is 24.0 (daily average to diurnal factor)
          qsum = 24.0/sum
          do ihr= 0,23
            diurnal(ihr,lat) = diurnal(ihr,lat)*qsum
          enddo !ihr
        endif
        diurnal(24,lat) = diurnal(0,lat) !copy for table lookup
      enddo !lat
      do ihr= 0,24
        diurnal(ihr,-91) = diurnal(ihr,-90) !copy for table lookup
        diurnal(ihr, 91) = diurnal(ihr, 90) !copy for table lookup
      enddo !ihr
      return
c
      contains
        subroutine declin(date,sindec,cosdec)
        implicit none
c
        real date,sindec,cosdec
c
c  subroutine to calc the sin and cosin of the solar declination angle
c  as a function of the date.
c       date = julian type real date - 1.0 (range 0. to 365.), where 00z
c              jan 1 = 0.0.
c       sindec = returned sin of the declination angle.
c       cosdec = returned cosin of the declination angle.
c  formula is from fnoc pe model.
c  created 10-7-81.   paul j martin.   norda code 322.
c
        real a
c
        a=date
        sindec=.39785*sin(4.88578+.0172*a+.03342*sin(.0172*a)-
     &  .001388*cos(.0172*a)+.000348*sin(.0344*a)-.000028*cos(.0344*a))
        cosdec=sqrt(1.-sindec*sindec)
        return
        end subroutine declin
      end subroutine thermf_diurnal

      subroutine swfrac_ij(akpar,zz,kz,zzscl,jerlov,swfrac)
      implicit none
c
      integer kz,jerlov
      real    akpar,zz(kz),zzscl,swfrac(kz)
c
c --- calculate fraction of shortwave flux remaining at depths zz
c
c --- akpar  = CHL (jerlov=-1) or KPAR (jerlov=0)
c --- zzscl  = scale factor to convert zz to m
c --- jerlov = 1-5 for jerlov type, or 0 for KPAR or -1 for CHL
c
c --- zz(kz) must be the bottom, so swfrac(kz)=0.0 and
c --- any residual which would otherwise be at the bottom is 
c --- uniformly distrubuted across the water column
c
c --- betard is jerlov water types 1 to 5 red  extinction coefficient
c --- betabl is jerlov water types 1 to 5 blue extinction coefficient
c --- redfac is jerlov water types 1 to 5 fract. of penetr. red light
      real, parameter, dimension(5) ::
     &  betard = (/ 1.0/0.35, 1.0/0.6, 1.0,     1.0/1.5, 1.0/1.4  /),
     &  betabl = (/ 1.0/23.0, 1.0/20.0,1.0/17.0,1.0/14.0,1.0/ 7.9 /),
     &  redfac = (/ 0.58,     0.62,    0.67,    0.77,    0.78     /)
c
c --- parameters for ZP LEE et al., 2005 SW attenuation scheme
      real, parameter ::  jjx0 = -0.057
      real, parameter ::  jjx1 =  0.482
      real, parameter ::  jjx2 =  4.221
      real, parameter ::  jjc0 =  0.183
      real, parameter ::  jjc1 =  0.702
      real, parameter ::  jjc2 = -2.567
c
c --- local variables for ZP LEE et al., 2005 SW attenuation scheme
      real chl                  ! surface chlorophyll value (mg m-3)
      real clog                 ! log10 transformed chl
      real a490                 ! total absorption coefficient 490 nm
      real bp550                ! particle scattering coefficient 550 nm
      real v1                   ! scattering emprical constant
      real bbp490               ! particle backscattering 490 nm
      real bb490                ! total backscattering coefficient 490 nm
      real k1                   ! internal vis attenuation term
      real k2                   ! internal vis attenuation term
c
      integer k,knot0
      real    beta_b,beta_r,frac_r,frac_b,d,swfbot
c
      if     (jerlov.ge.0) then
        if     (jerlov.gt.0) then
c ---     standard Jerlov
          beta_r = betard(jerlov)
          beta_b = betabl(jerlov)
          frac_r = redfac(jerlov)
          frac_b = 1.0 - frac_r
        else
c ---     Jerlov-like scheme, from Kpar
c ---       A. B. Kara, A. B., A. J. Wallcraft and H. E. Hurlburt, 2005:
c ---       A New Solar Radiation Penetration Scheme for Use in Ocean 
c ---       Mixed Layer Studies: An Application to the Black Sea Using
c ---       a Fine-Resolution Hybrid Coordinate Ocean Model (HYCOM)
c ---       Journal of Physical Oceanography vol 35, 13-32
          beta_r = 1.0/0.5
          beta_b = akpar
          beta_b = max( betabl(1), beta_b)  !time interp. kpar might be -ve
          frac_b = max( 0.27, 0.695 - 5.7*beta_b )
          frac_r = 1.0 - frac_b
        endif
c
c ---   how much SW nominally reaches the bottom
        d = zz(kz)*zzscl
c
        if     (-d*beta_r.gt.-10.0) then
          swfbot=frac_r*exp(-d*beta_r)+
     &           frac_b*exp(-d*beta_b)
        elseif (-d*beta_b.gt.-10.0) then
          swfbot=frac_b*exp(-d*beta_b)
        else
          swfbot=0.0
        endif
c
c ---   spread swfbot uniformly across the water column
        swfbot = swfbot/d
c
c ---   no SW actually left at the bottom
        knot0 = 0
        do k= kz,1,-1
          if     (zz(k).ge.zz(kz)) then
            swfrac(k) = 0.0
          else
            knot0 = k  !deepest level not on the bottom
            exit
          endif
        enddo !k
c
c ---   how much SW reaches zz
        do k= 1,knot0
          d = zz(k)*zzscl
c
          if     (-d*beta_r.gt.-10.0) then
            swfrac(k)=frac_r*exp(-d*beta_r)+
     &                frac_b*exp(-d*beta_b)-swfbot*d
          elseif (-d*beta_b.gt.-10.0) then
            swfrac(k)=frac_b*exp(-d*beta_b)-swfbot*d
          else
            swfrac(k)=0.0-swfbot*d
          endif
        enddo !k
      else   !jerlov.eq.-1
c
c --- ---------------------------------------------------------------------
c ---   shortwave attneuation scheme from:
c ---    Lee, Z., K. Du, R. Arnone, S. Liew, and B. Penta (2005),
c ---     Penetration of solar radiation in the upper ocean:
c ---     A numerical model for oceanic and coastal waters,
c ---     J. Geophys. Res., 110, C09019, doi:10.1029/2004JC002780.
c ---   This is a 2-band scheme with "frac_r" fixed. However,
c ---    "beta_b" and "beta_r" are now depth dependent.
c ---   Required input to the scheme is the total absorption coefficient
c ---    at the surface for 490 nm waveband (a490, m-1) and the
c ---    total backscattering coefficient at the surface at the same
c ---    waveband (bb490, m-1). 
c ---   However, here simple "CASE 1" relationships between these surface
c ---    optical properties and surface chlorophyll-a (mg m-3) are assumed.
c ---   These assumptions are considered valid for global, basin-scale
c ---    oceanography. However, coastal and regional applications tend
c ---    to be more complex, and a490 and bb490 should be determined
c ---    directly from the satellite data.
c ---   Authored by Jason Jolliff, NRL; week of 14 November 2011
c --- ---------------------------------------------------------------------
c
        frac_r = 0.52
        frac_b = 1.0 - frac_r
c
c ---   a490 as a function of chl, adapted from Morel et al 2007 Kd(490)
c ---   valid range for chl is 0.01 to 100 mg m-3
        chl  = akpar
        chl  = max(chl,  0.01)
        chl  = min(chl,100.0)
        clog = LOG10(chl)
        a490 = 10.0**(clog*clog*clog*(-0.016993) +
     &                clog*clog*0.0756296 +
     &                clog*0.55420 - 1.14881)
c
c ---   bb490 as a function of chl, from Morel and Maritorania 2001;
c ---   0.0012 is the pure water backscatter
c ---   valid range is restricted to 0.02 - 3.0 mg m-3 chl
        chl   = akpar
        chl   = max(chl,0.02)
        chl   = min(chl,3.0)
        clog  = LOG10(chl)
        bp550 = 0.416*chl**0.766
        if (chl .lt. 2.0) then
          v1 = 0.5*(clog-0.3)
        else
          v1 = 0.0
        endif
        bbp490 = (0.002 + 0.01*(0.50 - 0.25*clog))
     &         * (490.0/550.0)**v1 * bp550
        bb490 = bbp490 + 0.0012
c
c ---   functions of a490 and bb490 for beta_b
        k1 = jjx0 + jjx1*sqrt(a490) + jjx2*bb490
        k2 = jjc0 + jjc1*     a490  + jjc2*bb490
c
c ---   how much SW nominally reaches the bottom
        d = zz(kz)*zzscl
c
        beta_r = 0.560 + 2.34/((0.001 + d)**0.65)
        beta_b = k1    + k2 * 1.0/sqrt(1.0 + d)
        if     (-d*beta_b.gt.-10.0) then
          swfbot = frac_r*exp(-d*beta_r)+
     &             frac_b*exp(-d*beta_b)
        else
          swfbot = 0.0
        endif
c
c ---   spread swfbot uniformly across the water column
        swfbot = swfbot/d
c
c ---   no SW actually left at the bottom
        knot0 = 0
        do k= kz,1,-1
          if     (zz(k).ge.zz(kz)) then
            swfrac(k) = 0.0
          else
            knot0 = k
            exit
          endif
        enddo !k
c
c ---   how much SW reaches zz
        do k= 1,knot0
          d = zz(k)*zzscl
c
          beta_r = 0.560 + 2.34/((0.001 + d)**0.65)
          beta_b = k1    + k2 * 1.0/sqrt(1.0 + d)
          if     (-d*beta_b.gt.-10.0) then
            swfrac(k) = frac_r*exp(-d*beta_r)+
     &                  frac_b*exp(-d*beta_b)-swfbot*d
          else
            swfrac(k) = 0.0-swfbot*d
          endif
        enddo !k
      endif
      end
      subroutine swfrml_ij(akpar,hbl,bot,zzscl,jerlov,swfrml)
      implicit none
c
      integer jerlov
      real    akpar,hbl,bot,zzscl,swfrml
c
c --- calculate fraction of shortwave flux remaining at depth hbl
c
c --- akpar  = CHL (jerlov=-1) or KPAR (jerlov=0)
c --- zzscl  = scale factor to convert hbl and bot to m
c --- jerlov = 1-5 for jerlov type, or 0 for KPAR or -1 for CHL
c
      real zz(2),swf(2)
c
      if     (hbl.ge.bot) then
        swfrml = 0.0
      else
        zz(1) = hbl
        zz(2) = bot
        call swfrac_ij(akpar,zz,2,zzscl,jerlov,swf)
        swfrml = swf(1)
      endif
      return
      end
c
c
c> Revision history:
c>
c> Oct. 1999 - surface flux calculations modified for kpp mixed layer model,
c>             including penetrating solar radiation based on jerlov water type
c> Apr. 2000 - conversion to SI units
c> Oct  2000 - added thermfj to simplify OpenMP logic
c> Dec  2000 - modified fluxes when ice is present
c> Dec  2000 - added Kara bulk air-sea flux parameterization (flxflg=2)
c> May  2002 - buoyfl now calculated in mixed layer routine
c> Aug  2002 - added nested velocity relaxation
c> Nov  2002 - separate sss and sst relaxation time scales (thkml[st])
c> Nov  2002 - save sssflx and sstflx for diagnostics
c> Mar  2003 - longwave radiation correction for model vs "longwave" SST
c> May  2003 - use seatmp in place of twall.1, when available
c> Mar  2003 - add option to smooth surface fluxes
c> Mar  2004 - added epmass for treating E-P as a mass exchange
c> Mar  2005 - limit thkml[st] to no more than the actual depth
c> Mar  2005 - added empflg
c> Mar  2005 - replaced qsatur with 97% of qsatur in evap calculation
c> Mar  2005 - added ustflg
c> Mar  2005 - added flxoff
c> Apr  2005 - add a virtual temperature correction to Ta-Ts for flxflg=4.
c> Jun  2006 - explicit separation of ocean and sea ice surface fluxes
c> Jun  2007 - rebalance velocity after sidewall and nestwall relaxation
c> Oct  2008 - add dswflg
c> Jun  2009 - add sssrmx
c> Apr  2010 - change sssrmx to an array
c> Nov  2010 - added empflg<0 for using observed SST in E
c> Nov  2011 - ignore sssrmx, i.e. fully relax to sss, under ice
c> July 2013 - vamax set to 34 m/s, same as for Cd (momtum.f)
c> Oct. 2013 - added subroutine swfrac_ij, called in mixed layer routines
c> Oct. 2013 - added subroutine swfrml_ij, called in mixed layer routines
c> Nov. 2013 - added rivflx, so that rivers under sea ice are handled correctly
c> Nov  2013 - added lwflag=-1 for input radflx=Qlwdn
c> Nov. 2013 - added flxflg=5 for the CORE v2 bulk parameterization
c> Jan. 2014 - tv in Kelvin (flxflg=5)
c> Jan. 2014 - added pair for time varying msl pressure (mslprf)
c> Jan. 2014 - added natm
c> Apr. 2014 - added ice shelf logic (ishelf)
c> Apr. 2014 - replace ip with ipa for mass sums
c> May  2014 - use land/sea masks (e.g. ip) to skip land
c> Jun. 2014 - always call thermfj
c> Oct. 2014 - added flxflg=6, similar to flxflg=4
c> Oct. 2014 - flxflg=6 uses sea level pressure optimally
c> Oct. 2014 - flxflg=6 replaces wind speed with wind-ocean speed
c> Oct. 2014 - Newtonian relaxation now uses an implict time step
c> Oct. 2014 - always fully relax when fresher than half climatological sss
