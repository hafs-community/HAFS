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
      subroutine initrc(mnth)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_pipe       ! HYCOM debugging interface
      implicit none
c
      integer mnth
c
c --- --------------------------
c --- initializatize all tracers
c --- --------------------------
c
      logical    lpipe_initrc
      parameter (lpipe_initrc=.false.)
c
      character ptxt*12,cformat*99
      integer   i,ibio,nbio,j,k,ktr
      real      bio_n,bio_p,zk
      real      pwij(kk+1),trwij(kk,ntracr),
     &          prij(kk+1),trcij(kk,ntracr)
      real      chl,swfrac(kdm+1)
c
      if (ntracr.eq.0) then
        return  ! no tracer
      endif
c
c --- expand trcflg to allow for number of biology fields.
c
      nbio = 0
      ibio = 0
      do ktr= 1,ntracr+1
        if     (ktr.ne.ntracr+1 .and.
     &          trcflg(min(ktr,ntracr)).eq.9) then
          if     (ibio.eq.0) then !start biology
            ibio = ktr
          endif
        elseif (ibio.ne.0) then !end biology
          nbio = ktr-ibio
          if     (nbio.eq.3) then
c ---       Franks NPZ.
            trcflg(ibio)   =  903
            trcflg(ibio+1) = -903
            trcflg(ibio+2) = -903
            ibio = 0
          elseif (nbio.eq.3) then
c ---       Two Franks NPZ.
            trcflg(ibio)   =  903
            trcflg(ibio+1) = -903
            trcflg(ibio+2) = -903
            trcflg(ibio+3) =  903
            trcflg(ibio+4) = -903
            trcflg(ibio+5) = -903
            ibio = 0
          elseif (nbio.eq.4) then
c ---       Lima/Idrisi NPZD.
            trcflg(ibio)   =  904
            trcflg(ibio+1) = -904
            trcflg(ibio+2) = -904
            trcflg(ibio+3) = -904
            ibio = 0
          elseif (nbio.eq.7) then
c ---       Lima/Idrisi NPZD and Franks NPZ.
            trcflg(ibio)   =  904
            trcflg(ibio+1) = -904
            trcflg(ibio+2) = -904
            trcflg(ibio+3) = -904
            trcflg(ibio+4) =  903
            trcflg(ibio+5) = -903
            trcflg(ibio+6) = -903
            ibio = 0
          elseif (nbio.eq.8) then
c ---       Two Lima/Idrisi NPZD.
            trcflg(ibio)   =  904
            trcflg(ibio+1) = -904
            trcflg(ibio+2) = -904
            trcflg(ibio+3) = -904
            trcflg(ibio+4) =  904
            trcflg(ibio+5) = -904
            trcflg(ibio+6) = -904
            trcflg(ibio+7) = -904
            ibio = 0
          elseif (nbio.eq.9) then
c ---       Chai 9-component.
*           trcflg(ibio)   =  909
*           trcflg(ibio+1) = -909
*           trcflg(ibio+2) = -909
*           trcflg(ibio+3) = -909
*           trcflg(ibio+4) = -909
*           trcflg(ibio+5) = -909
*           trcflg(ibio+6) = -909
*           trcflg(ibio+7) = -909
*           trcflg(ibio+8) = -909
*           ibio = 0
c ---       not yet implemented
            if (mnproc.eq.1) then
            write(lp,'(/ 3a /)')
     &        'error - trcflg=9 (standard biology) configured',
     &        ' with 9 consecutive tracers, but Chai scheme is',
     &        ' not yet implemented'
            call flush(lp)
            endif !1st tile
            call xcstop('(trcini)')
                   stop '(trcini)'
          else
c ---       unknown standard biology.
            if (mnproc.eq.1) then
            write(lp,'(/ 2a,i3 /)')
     &        'error - trcflg=9 (standard biology) expects',
     &        ' 3/4/6/7/8 consecutive tracers but have',nbio
*    &        ' 3/4/6/7/8/9 consecutive tracers but have',nbio
            call flush(lp)
            endif !1st tile
            call xcstop('(trcini)')
                   stop '(trcini)'
          endif
        endif
      enddo
c
      if (mnproc.eq.1) then
      write(lp,*)
      do k= 1,ntracr
        write(lp,'(a,i3,i6)') 'initrc: k,trcflg =',k,trcflg(k)
      enddo
      write(lp,*)
      endif !1st tile
c
      if     (nbio.gt.0) then
c
c ---   input bio-tracer parameters.
c ---   note that multiple sets of bio-tracers are allowed,
c ---   each is read from tracer.input in tracer order.
c
        open(unit=uoff+99,file=trim(flnminp)//'tracer.input')
        do ktr= 1,ntracr
          if     (trcflg(ktr).eq.903) then
c ---       NPZ
            call trcupd_903(1,2, -ktr)
          elseif (trcflg(ktr).eq.904) then
c ---       NPZD
            call trcupd_904(1,2, -ktr)
*         elseif (trcflg(ktr).eq.909) then
* ---       Chai 9-component.
*           call trcupd_909(1,2, -ktr)
          endif
        enddo
        close(unit=uoff+99)
      endif
c
      if     (trcrin) then
        return  ! tracer from restart
      endif
c
      if     (iniflg.eq.2) then  ! use climatology
        call rdrlax(mnth,1)
!$OMP   PARALLEL DO PRIVATE(j,i,k,ktr,pwij,trwij,prij,trcij)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
            if (SEA_P) then
              prij(1)=0.0
              do k=1,kk
                prij(k+1)=prij(k)+dp(i,j,k,1)
                pwij(k)  =pwall(i,j,k,1)
                do ktr= 1,ntracr
                  trwij(k,ktr)=trwall(i,j,k,1,ktr)
                enddo !ktr
              enddo !k
              pwij(kk+1)=prij(kk+1)
*             call plctrc(trwij,pwij,kk,ntracr,
*    &                    trcij,prij,kk        )
              call plmtrc(trwij,pwij,kk,ntracr,
     &                    trcij,prij,kk        )
              do k=1,kk
                do ktr= 1,ntracr
                  tracer(i,j,k,1,ktr)=trcij(k,ktr)
                  tracer(i,j,k,2,ktr)=trcij(k,ktr)
                enddo !ktr
              enddo !k
            endif !ip
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
      else ! analytic inititalization
!$OMP   PARALLEL DO PRIVATE(j,i,k,ktr,prij,chl,swfrac,zk,bio_n,bio_p)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
            if (SEA_P) then
              prij(1)=0.0
              do k=1,kk
                prij(k+1)=prij(k)+dp(i,j,k,1)
              enddo !k
              do ktr= 1,ntracr
                if (trcflg(ktr).eq.1) then !need the euphotic zone
                  if     (jerlv0.le.0) then
                    chl =  akpar(i,j,lk0)*wk0+akpar(i,j,lk1)*wk1
     &                    +akpar(i,j,lk2)*wk2+akpar(i,j,lk3)*wk3
                  endif
                  call swfrac_ij(chl,prij,kk+1,qonem,jerlov(i,j),swfrac)
                  exit  !only calculate swfrac once
                endif !trcflg==1
              enddo !ktr
              do k=1,kk
                do ktr= 1,ntracr
                  if     (trcflg(ktr).eq.0) then !100% in the mixed layer
                    if     (prij(k).le.dpmixl(i,j,1)) then
                      tracer(i,j,k,1,ktr)=10.0
                      tracer(i,j,k,2,ktr)=10.0
                    else
                      tracer(i,j,k,1,ktr)=0.0
                      tracer(i,j,k,2,ktr)=0.0
                    endif
                  elseif (trcflg(ktr).eq.1) then !20 below euphotic zone
                    if     (swfrac(k).gt.0.01) then
                      tracer(i,j,k,1,ktr)=0.0
                      tracer(i,j,k,2,ktr)=0.0
                    else
                      tracer(i,j,k,1,ktr)=20.0  ! mg/m^3
                      tracer(i,j,k,2,ktr)=20.0  ! mg/m^3
                    endif
                  elseif (trcflg(ktr).eq.2) then !temperature
                    tracer(i,j,k,1,ktr)=temp(i,j,k,1)
                    tracer(i,j,k,2,ktr)=temp(i,j,k,1)
                  elseif (trcflg(ktr).eq.3) then !fully passive
                    tracer(i,j,k,1,ktr)=0.0 !should never get here
                    tracer(i,j,k,2,ktr)=0.0 !should never get here
                  elseif (trcflg(ktr).eq.904 .or.
     &                    trcflg(ktr).eq.903     ) then !NPZD or NPZ
                    zk = 0.5*(prij(k+1)+prij(k))*qonem
                    if     (zk.le.300.0) then
                      ! 0.1 at 300m, 1.0 at 100m, 2.025 at 0m
                      bio_p = 0.1 + (300.0-zk)**2 * (0.9/200.0**2)
                    elseif (zk.le.900.0) then
                      ! 0.1 at 300m, 0.0 at 900m
                      bio_p = (900.0-zk) * 0.1/600.0
                    else
                      bio_p = 0.0
                    endif
                    if     (temp(i,j,k,1).lt. 6.0) then
                      bio_n = 37.0
                    elseif (temp(i,j,k,1).gt.27.0) then
                      bio_n =  0.0
                    else
*                     bio_n = (27.0-temp(i,j,k,1)) * 37.0/21.0
                      bio_n = 39.3116-1.335*temp(i,j,k,1)
                    endif
                    tracer(i,j,k,1,ktr  )=bio_n  !N
                    tracer(i,j,k,2,ktr  )=bio_n
                    tracer(i,j,k,1,ktr+1)=bio_p  !P
                    tracer(i,j,k,2,ktr+1)=bio_p
                    tracer(i,j,k,1,ktr+2)=bio_p  !Z=P
                    tracer(i,j,k,2,ktr+2)=bio_p
                    if     (trcflg(ktr).eq.904) then
                      tracer(i,j,k,1,ktr+3)=bio_p + 1.0  !D=P+1
                      tracer(i,j,k,2,ktr+3)=bio_p + 1.0
                    endif
                  endif !trcflg
                enddo !ktr
              enddo !k
            endif !ip
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
      endif !iniflg.eq.2:else
c
      if     (lpipe .and. lpipe_initrc) then
         do ktr= 1,ntracr
           do k= 1,kk
             write (ptxt,'(a4,i2.2,a3,i3)') 'trc.',ktr,' k=',k
             call pipe_compare_sym1(tracer(1-nbdy,1-nbdy,k,1,ktr),
     &                              ip,ptxt)
           enddo !k
         enddo !ktr
       endif !lpipe.and.lpipe_initrc
c
      if     (itest.gt.0 .and. jtest.gt.0) then
         prij(1)=0.0
         do k=1,kk
           prij(k+1)=prij(k)+dp(itest,jtest,k,1)
         enddo !k
         write(cformat,'(a,i2,a,i2,a)')
     &     '(i9,2i5,a,',ntracr,
     &     'a / (23x,i3,2f8.2,', ntracr,'f8.4))'
          write (lp,cformat)
     &     nstep,i0+itest,j0+jtest,
     &     '  istate:  thkns    dpth',
     &     ('  tracer',ktr=1,ntracr),
     &     (k,
     &      dp(itest,jtest,k,1)*qonem,
     &      (prij(k+1)+prij(k))*0.5*qonem,
     &      (tracer(itest,jtest,k,1,ktr),ktr=1,ntracr),
     &      k=1,kk)
         write(lp,'(23x,a,8x,f8.2)') 'bot',depths(itest,jtest)
      endif !test tile
      call xcsync(flush_lp)
c
      return
      end

      subroutine trcupd(m,n)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n
c
c --- -----------------------------------------------------------
c --- tracer-specific operations (side-wall relaxation in thermf)
c --- -----------------------------------------------------------
c
      integer i,j,k,ktr
      real    chl,pij(kdm+1),swfrac(kdm+1),q
c
      do ktr= 1,ntracr
        if     (trcflg(ktr).eq.0) then
          if (trcrlx) then
c ---       tracer always trwall, when non-zero, at surface
!$OMP       PARALLEL DO PRIVATE(j,k,i,ktr,q)
!$OMP&               SCHEDULE(STATIC,jblk)
            do j=1,jj
              do i=1,ii
                if (SEA_P) then
c ---             constant in time trwall remains exactly constant
                  q =                        trwall(i,j,1,lc0,ktr)
     &               +(trwall(i,j,1,lc1,ktr)-trwall(i,j,1,lc0,ktr))*wc1
     &               +(trwall(i,j,1,lc2,ktr)-trwall(i,j,1,lc0,ktr))*wc2
     &               +(trwall(i,j,1,lc3,ktr)-trwall(i,j,1,lc0,ktr))*wc3
                  if     (q.gt.0.0) then
                    tracer(i,j,1,n,ktr) = q
                  endif
                endif !ip
              enddo !i
            enddo !j
!$OMP       END PARALLEL DO
          elseif (.not. trcrlx) then
c ---       tracer always 10.0 at surface
!$OMP       PARALLEL DO PRIVATE(j,k,i,ktr)
!$OMP&               SCHEDULE(STATIC,jblk)
            do j=1,jj
              do i=1,ii
                if (SEA_P) then
                  tracer(i,j,1,n,ktr) = 10.0
                endif !ip
              enddo !i
            enddo !j
!$OMP       END PARALLEL DO
          endif !trcrlx:else
        elseif (trcflg(ktr).eq.1) then
c ---     psudo-silicate, half-life of 30 days in euphotic zone
          q = 1.0-delt1/(30.0*86400.0)
!$OMP     PARALLEL DO PRIVATE(j,k,i,ktr,chl,pij,swfrac)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1,jj
            do i=1,ii
              if (SEA_P) then
                pij(1)=0.0
                do k=1,kk
                  pij(k+1) = pij(k)+dp(i,j,k,n)
                enddo !k
                if     (jerlv0.le.0) then
                  chl =  akpar(i,j,lk0)*wk0+akpar(i,j,lk1)*wk1
     &                  +akpar(i,j,lk2)*wk2+akpar(i,j,lk3)*wk3
                endif
                call swfrac_ij(chl,pij,kk+1,qonem,jerlov(i,j),swfrac)
                do k=1,kk
                  if     (0.5*(swfrac(k)+swfrac(k+1)).gt.0.01) then
                    tracer(i,j,k,n,ktr) = q*tracer(i,j,k,n,ktr)
                  else
                    exit  !too deep
                  endif
                enddo !k
              endif !ip
            enddo !i
          enddo !j
!$OMP     END PARALLEL DO
        elseif (trcflg(ktr).eq.2) then
c ---     temperature-like (do nothing, heat flux forcing in mixed layer)
        elseif (trcflg(ktr).eq.3) then
c ---     fully passive    (do nothing)
        elseif (trcflg(ktr).eq.903) then
c ---     NPZ
          call trcupd_903(m,n, ktr)
        elseif (trcflg(ktr).eq.904) then
c ---     NPZD
          call trcupd_904(m,n, ktr)
*       elseif (trcflg(ktr).eq.909) then
* ---     Chai 9-component.
*         call trcupd_909(m,n, ktr)
        endif
      enddo !ktr
      return
      end subroutine trcupd

      subroutine trcupd_903(m,n, ibio)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n,ibio
c
c --- -------------------------------------------------
c --- tracer-specific operations for Franks NPZ biology
c --- -------------------------------------------------
c
      real,    save, dimension(mxtrcr) ::
     & bup,   ! maximum growth  rate of phytoplankton (1/d).
     & bgz,   ! maximum grazing rate of zooplankton   (1/d).
     & bdp,   ! senescence (death) rate of phytoplankton (1/d).
     & bdz,   ! death rate of zooplankton (1/d).
     & buk,   ! = half-saturation coefficient for phytoplankton (mg/m^3)
     & asim,  ! assimilation efficiency of zooplankton.
     & glam   ! Ivlev parameter for grazing efficiency of zooplankton.
c
      integer i,j,k
      real    bm_n,bm_p,bm_z,bn_n,bn_p,bn_z,bu_n,bu_p,bu_z,
     &        uptake,grazin,pdeath,zdeath,
     &        chl,par,pij(kdm+1),swfrac(kdm+1)
c
      if     (ibio.lt.0) then !initialize only
c
c ---   read from tracer_NN.input:
c ---   'biotyp' = type (90X=std.bio,X=3,4,9) must be 903
c ---   'bup   ' = maximum growth  rate of phytoplankton (1/d).
c ---   'bgz   ' = maximum grazing rate of zooplankton   (1/d).
c ---   'bdp   ' = senescence (death) rate of phytoplankton (1/d).
c ---   'bdz   ' = death rate of zooplankton (1/d).
c ---   'buk   ' = half-saturation coefficient for phytoplankton (mg/m^3)
c ---   'asim  ' = assimilation efficiency of zooplankton.
c ---   'glam  ' = Ivlev parameter for grazing efficiency of zooplankton.
c
        i = -ibio
        if (mnproc.eq.1) then
        write(lp,'(/ a,i3,a,i3,a)')
     &    'Franks NPZ parameters for tracers',i,' to',i+2,':'
        endif !1st tile
c
        call blkini(k, 'biotyp')
        if     (k.ne.903) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)')
     &        'error - biotyp must be 903'
          call flush(lp)
          endif !1st tile
          call xcstop('(trcini)')
                 stop '(trcini)'
        endif !biotyp.ne.903
c
        call blkinr(bup(   i), 'bup   ','(a6," =",f10.4," 1/d")')
        call blkinr(bgz(   i), 'bgz   ','(a6," =",f10.4," 1/d")')
        call blkinr(bdp(   i), 'bdp   ','(a6," =",f10.4," 1/d")')
        call blkinr(bdz(   i), 'bdz   ','(a6," =",f10.4," 1/d")')
        call blkinr(buk(   i), 'buk   ','(a6," =",f10.4," mg/m^3")')
        call blkinr(asim(  i), 'asim  ','(a6," =",f10.4," ")')
        call blkinr(glam(  i), 'glam  ','(a6," =",f10.4," ")')
c
        if (mnproc.eq.1) then
        write(lp,*)
        endif !1st tile
        return
      endif !ibio.lt.0
c
c --- leapfrog time step.
c
!$OMP PARALLEL DO PRIVATE(j,i,k,chl,pij,par,swfrac,
!$OMP&                    bm_n,bm_p,bm_z,bn_n,bn_p,bn_z,
!$OMP&                    bu_n,bu_p,bu_z,
!$OMP&                    uptake,grazin,pdeath,zdeath)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        do i=1,ii
          if (SEA_P) then
            pij(1)=0.0
            do k=1,kk
              pij(k+1) = pij(k)+dp(i,j,k,n)
            enddo !k
            if     (jerlv0.le.0) then
              chl =  akpar(i,j,lk0)*wk0+akpar(i,j,lk1)*wk1
     &              +akpar(i,j,lk2)*wk2+akpar(i,j,lk3)*wk3
            endif
            call swfrac_ij(chl,pij,kk+1,qonem,jerlov(i,j),swfrac)
            do k=1,kk
              par   = 0.5*(swfrac(k)+swfrac(k+1))
c
              bm_n = tracer(i,j,k,m,ibio)
              bm_p = tracer(i,j,k,m,ibio+1)
              bm_z = tracer(i,j,k,m,ibio+2)
              bn_n = tracer(i,j,k,n,ibio)
              bn_p = tracer(i,j,k,n,ibio+1)
              bn_z = tracer(i,j,k,n,ibio+2)
c
              uptake = bup(ibio)*bm_p*bm_n*par/(buk(ibio)+bm_n)
              grazin = bgz(ibio)*bm_z*(1.0-exp(-glam(ibio)*bm_p))
              pdeath = bdp(ibio)*bm_p
              zdeath = bdz(ibio)*bm_z
              ! limit negative terms to 10% of total per single time step
              grazin = min(grazin,bn_p*0.2*86400.0/delt1)
              uptake = min(uptake,bn_n*0.2*86400.0/delt1)
c
              bu_p =                 -grazin       +uptake-pdeath
              bu_z =      asim(ibio) *grazin-zdeath
              bu_n = (1.0-asim(ibio))*grazin+zdeath-uptake+pdeath
c
              tracer(i,j,k,n,ibio)   = bn_n + delt1/86400.0 * bu_n
              tracer(i,j,k,n,ibio+1) = bn_p + delt1/86400.0 * bu_p
              tracer(i,j,k,n,ibio+2) = bn_z + delt1/86400.0 * bu_z
c
c ---         fields must be non-negative
c ---         note: only round-off should make a field negative
c
              if     (tracer(i,j,k,n,ibio+1).lt.0.0) then !PtoN
                tracer(i,j,k,n,ibio)   = tracer(i,j,k,n,ibio)   - 
     &                                   tracer(i,j,k,n,ibio+1)
                tracer(i,j,k,n,ibio+1) = 0.0
              endif
              if     (tracer(i,j,k,n,ibio+2).lt.0.0) then !ZtoN
                tracer(i,j,k,n,ibio)   = tracer(i,j,k,n,ibio)   - 
     &                                   tracer(i,j,k,n,ibio+2)
                tracer(i,j,k,n,ibio+2) = 0.0
              endif
              if     (tracer(i,j,k,n,ibio)  .lt.0.0) then !NtoPZ (do last)
                tracer(i,j,k,n,ibio+1) = tracer(i,j,k,n,ibio+1) - 
     &                                   tracer(i,j,k,n,ibio)*0.5
                tracer(i,j,k,n,ibio+2) = tracer(i,j,k,n,ibio+2) - 
     &                                   tracer(i,j,k,n,ibio)*0.5
                tracer(i,j,k,n,ibio)   = 0.0
              endif
            enddo !k
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
      return
      end subroutine trcupd_903

      subroutine trcupd_904(m,n, ibio)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n,ibio
c
c --- -------------------------------------------------------
c --- tracer-specific operations for Lima/Idrisi NPZD biology
c --- -------------------------------------------------------
c
      real,    save, dimension(mxtrcr) ::
     &  pp,   ! zoopl: preference term for phytoplankton
     &  pz,   ! zoopl: preference term for zooplankton
     &  pd,   ! zoopl: preference term for detritus
     &  aa,   ! zoopl: assimilation efficiency
     &  am,   ! zoopl: metabolic    efficiency
     &  fkz,  ! zoopl: half-saturation coefficient (mg/m^3)
     &  gmax, ! zoopl: maximum growth rate (1/day)
     &  zmor  ! zoopl: mortality (1/day)
c
      real,    save, dimension(mxtrcr) ::
*    &  ik,   ! phyto: light absorption efficiency scalar (einst/m^2/h)
     &  fkp,  ! phyto: half-saturation coefficient (mg/m^3)
     &  pmax, ! phyto: maximum growth rate (1/day)
     &  psen  ! phyto: senescence (1/day)
c
      real,    save, dimension(mxtrcr) ::
     &  remn  ! detri: remineralization (1/day)
c
      integer, save, dimension(mxtrcr) ::
     & spcflg ! tmpfn: species type (0=none,1=cold-water,2=warm-water)
c
      real, parameter ::  ! temperature function for cold-water species
     &                    ! thornton and lessem (1978)
     &  theta1 = 16.0,    ! dependence on lower  optimum temperature curve
     &  theta2 =  9.0,    ! dependence on higher optimum temperature curve
     &  theta3 = 11.0,    ! maximum temperature (upper tolerance level)
     &  q10l   =  2.0,    ! the metabolic q10 for temperature response
     &  xk1    =  0.5,    ! scalar constant
     &  xk2    =  0.98,   ! scalar constant
     &  xk3    =  0.01,   ! scalar constant
     &  xk4    =  0.01    ! scalar constant
c
      real, parameter ::  ! temperature function for warm-water species
     &  tmax   = 27.0,    ! Tfunc: maximum tolerated temperature
     &  topt   = 25.0,    ! Tfunc: optimum temperature
     &  q10w   =  2.0     ! Tfunc: the metabolic q10 for temperature response
c
      integer i,j,k
      real    bm_n,bm_p,bm_z,bm_d,bn_n,bn_p,bn_z,bn_d,
     &        bu_n,bu_p,bu_z,bu_d,
     &        gamma1,gamma2,xnum,xkatheta,ynum,xkbtheta,
     &        tijk,tfn,vw,xw,yw,zw, 
     &        pgrw,zgrw,pref,prf2,qprf,ztgx,dofz,pofz,zofz,
     &        chl,par,pij(kdm+1),swfrac(kdm+1)
c
      if     (ibio.lt.0) then !initialize only
c
c ---   read from tracer.input:
c ---   'biotyp' = type (90X=std.bio,X=3,4,9) must be 904
c
c ---   'pp    ' = zoopl: preference term for phytoplankton
c ---   'pz    ' = zoopl: preference term for zooplankton
c ---   'pd    ' = zoopl: preference term for detritus
c ---   'aa    ' = zoopl: assimilation efficiency
c ---   'am    ' = zoopl: metabolic    efficiency
c ---   'fkz   ' = zoopl: half-saturation coefficient (mg/m^3)
c ---   'gmax  ' = zoopl: maximum growth rate (1/day)
c ---   'zmor  ' = zoopl: mortality (1/day)
c
* ---   'ik    ' = phyto: light absorption efficiency scalar (einst/m^2/h)
c ---   'fkp   ' = phyto: half-saturation coefficient (mg/m^3)
c ---   'pmax  ' = phyto: maximum growth rate (1/day)
c ---   'psen  ' = phyto: senescence (1/day)
c
c ---   'remn  ' = detri: remineralization (1/day)
c
c ---   'spcflg' = tmpfn: species type (0=none,1=cold-water,2=warm-water)
c
        i = -ibio
        if (mnproc.eq.1) then
        write(lp,'(/ a,i3,a,i3,a)')
     &    'Lima/Idrisi NPZD parameters for tracers',i,' to',i+3,':'
        endif !1st tile
c
        call blkini(k, 'biotyp')
        if     (k.ne.904) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)')
     &        'error - biotyp must be 904'
          call flush(lp)
          endif !1st tile
          call xcstop('(trcini)')
                 stop '(trcini)'
        endif !biotyp.ne.904
c
        call blkinr(pp(    i), 'pp    ','(a6," =",f10.4," ")')
        call blkinr(pz(    i), 'pz    ','(a6," =",f10.4," ")')
        call blkinr(pd(    i), 'pd    ','(a6," =",f10.4," ")')
        call blkinr(aa(    i), 'aa    ','(a6," =",f10.4," ")')
        call blkinr(am(    i), 'am    ','(a6," =",f10.4," ")')
        call blkinr(fkz(   i), 'fkz   ','(a6," =",f10.4," mg/m^3")')
        call blkinr(gmax(  i), 'gmax  ','(a6," =",f10.4," 1/day")')
        call blkinr(zmor(  i), 'zmor  ','(a6," =",f10.4," 1/day")')
c
        call blkinr(fkp(   i), 'fkp   ','(a6," =",f10.4," mg/m^3")')
        call blkinr(pmax(  i), 'pmax  ','(a6," =",f10.4," 1/day")')
        call blkinr(psen(  i), 'psen  ','(a6," =",f10.4," 1/day")')
c
        call blkinr(remn(  i), 'remn  ','(a6," =",f10.4," 1/day")')
c
        call blkini(spcflg(i),'spcflg')
c
        if (mnproc.eq.1) then
        write(lp,*)
        endif !1st tile
        return
      endif !ibio.lt.0
c
c --- leapfrog time step.
c
!$OMP PARALLEL DO PRIVATE(j,i,k,chl,pij,par,swfrac,
!$OMP&                    bm_n,bm_p,bm_z,bm_d,bn_n,bn_p,bn_z,bn_d,
!$OMP&                    bu_n,bu_p,bu_z,bu_d,
!$OMP&                    gamma1,gamma2,xnum,xkatheta,ynum,xkbtheta,
!$OMP&                    tijk,tfn,vw,xw,yw,zw,
!$OMP&                    pgrw,zgrw,pref,prf2,qprf,ztgx,dofz,pofz,zofz)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        do i=1,ii
          if (SEA_P) then
            pij(1)=0.0
            do k=1,kk
              pij(k+1) = pij(k)+dp(i,j,k,n)
            enddo !k
            if     (jerlv0.le.0) then
              chl =  akpar(i,j,lk0)*wk0+akpar(i,j,lk1)*wk1
     &              +akpar(i,j,lk2)*wk2+akpar(i,j,lk3)*wk3
            endif
            call swfrac_ij(chl,pij,kk+1,qonem,jerlov(i,j),swfrac)
            do k=1,kk
              par   = 0.5*(swfrac(k)+swfrac(k+1))
c
              bm_n = tracer(i,j,k,m,ibio)
              bm_p = tracer(i,j,k,m,ibio+1)
              bm_z = tracer(i,j,k,m,ibio+2)
              bm_d = tracer(i,j,k,m,ibio+3)
              bn_n = tracer(i,j,k,n,ibio)
              bn_p = tracer(i,j,k,n,ibio+1)
              bn_z = tracer(i,j,k,n,ibio+2)
              bn_d = tracer(i,j,k,n,ibio+3)
c
              if (spcflg(ibio).eq.1) then
c ---           cold-water species temperature dependance
                tijk     = temp(i,j,k,n)
                gamma1   = 1.0/(theta2-q10l) *
     &                     log((xk2*(1.0-xk1))/(xk1*(1.0-xk2)))
                gamma2   = 1.0/(theta1-theta3) *
     &                     log((xk2*(1.0-xk3))/(xk4*(1.0-xk2)))
                xnum     = exp(gamma1*(tijk-q10l))
                xkatheta = (xk1*xnum)/(1.0+xk1*(xnum-1.0))
                ynum     = exp(gamma2*(theta1-tijk))
                xkbtheta = (xk4*ynum)/(1.0+xk3*(ynum-1.0))
                tfn      = xkatheta*xkbtheta
              elseif (spcflg(ibio).eq.2) then
c ---           warm-water species temperature dependance
                tijk     = temp(i,j,k,n)
                if (tijk.le.tmax) then
                  vw  = (tmax-tijk)/(tmax-topt)
                  yw  = log(q10w)*(tmax-topt+2.0)
                  zw  = log(q10w)*(tmax-topt)
                  xw  = (zw**2 * (1.0+sqrt(1.0+40.0/yw))**2)/400.0
                  tfn = vw**xw * exp(xw*(1.0-vw))
                else
                  tfn=0.0
                endif
              else
c ---           no temperature dependance
                tfn=1.0
              endif !spcflg
c
              pref = pp(ibio)*bm_p +
     &               pd(ibio)*bm_d +
     &               pz(ibio)*bm_z
              prf2 = pp(ibio)*bm_p**2 +
     &               pd(ibio)*bm_d**2 +
     &               pz(ibio)*bm_z**2
              qprf = 1.0/(fkz(ibio)*pref + prf2 + epsil)  !epsil prevents 1/0
              ztgx = bm_z*tfn*gmax(ibio)
c
              pgrw = bm_p*tfn*pmax(ibio)*bm_n*par/(fkp(ibio)+bm_n)
              zgrw = ztgx*(prf2            *qprf)*aa(ibio)*am(ibio)
              pofz = ztgx*(pp(ibio)*bm_p**2*qprf)
              zofz = ztgx*(pz(ibio)*bm_z**2*qprf)
              dofz = ztgx*(pd(ibio)*bm_d**2*qprf)
c
              ! limit negative terms to 10% of total per single time step
              pgrw = min(pgrw,bn_n*0.2*86400.0/delt1)
              zgrw = min(zgrw,bn_n*0.2*86400.0/delt1)
              pofz = min(pofz,bn_p*0.2*86400.0/delt1)
              zofz = min(zofz,bn_z*0.2*86400.0/delt1)
              dofz = min(dofz,bn_d*0.2*86400.0/delt1)
c
              bu_p =   pgrw
     &               - pofz
     &               - bm_p*psen(ibio)
              bu_z =   zgrw
     &               - zofz
     &               - bm_z*zmor(ibio)
              bu_d =   bm_p*psen(ibio)
     &               + bm_z*zmor(ibio)
     &               + (pofz+zofz+dofz)*(1.0-aa(ibio))
     &               - dofz
     &               - bm_d*remn(ibio)
              bu_n =   bm_d*remn(ibio)
     &               + (pofz+zofz+dofz)*     aa(ibio)
     &               - zgrw
     &               - pgrw
c
              tracer(i,j,k,n,ibio)   = bn_n + delt1/86400.0 * bu_n
              tracer(i,j,k,n,ibio+1) = bn_p + delt1/86400.0 * bu_p
              tracer(i,j,k,n,ibio+2) = bn_z + delt1/86400.0 * bu_z
              tracer(i,j,k,n,ibio+3) = bn_d + delt1/86400.0 * bu_d
c
c ---         fields must be non-negative
c ---         note: only round-off should make a field negative
c
              if     (tracer(i,j,k,n,ibio+1).lt.0.0) then !PtoN
                tracer(i,j,k,n,ibio)   = tracer(i,j,k,n,ibio)   - 
     &                                   tracer(i,j,k,n,ibio+1)
                tracer(i,j,k,n,ibio+1) = 0.0
              endif
              if     (tracer(i,j,k,n,ibio+2).lt.0.0) then !ZtoN
                tracer(i,j,k,n,ibio)   = tracer(i,j,k,n,ibio)   - 
     &                                   tracer(i,j,k,n,ibio+2)
                tracer(i,j,k,n,ibio+2) = 0.0
              endif
              if     (tracer(i,j,k,n,ibio+3).lt.0.0) then !DtoN
                tracer(i,j,k,n,ibio)   = tracer(i,j,k,n,ibio)   - 
     &                                   tracer(i,j,k,n,ibio+3)
                tracer(i,j,k,n,ibio+3) = 0.0
              endif
              if     (tracer(i,j,k,n,ibio)  .lt.0.0) then !NtoD (do last)
                tracer(i,j,k,n,ibio+3) = tracer(i,j,k,n,ibio+3) - 
     &                                   tracer(i,j,k,n,ibio)
                tracer(i,j,k,n,ibio)   = 0.0
              endif
            enddo
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
      return
      end subroutine trcupd_904

      subroutine pcmtrc(si,pi,ki,ks, so,po,ko)
      implicit none
c
      integer ki,ks,ko
      real    si(ki,ks),pi(ki+1),
     &        so(ko,ks),po(ko+1)
c
c**********
c*
c  1) remap from one set of vertical cells to another.
c     method: piecewise constant across each input cell
c             the output is the average of the interpolation
c             profile across each output cell.
c
c  2) input arguments:
c       si    - scalar fields in pi-layer space
c       pi    - layer interface depths (non-negative m)
c                 pi(   1) is the surface
c                 pi(ki+1) is the bathymetry
c       ki    - 1st dimension of si     (number of  input layers)
c       ks    - 2nd dimension of si,so  (number of fields)
c       po    - target interface depths (non-negative m)
c                 po(k+1) >= po(k)
c       ko    - 1st dimension of so     (number of output layers)
c
c  3) output arguments:
c       so    - scalar fields in po-layer space
c
c  4) except at data voids, must have:
c           pi(   1) == zero (surface)
c           pi( l+1) >= pi(l)
c           pi(ki+1) == bathymetry
c           0 <= po(k) <= po(k+1)
c      output layers completely below the bathymetry inherit values
c      from the layer above.
c
c  5) Alan J. Wallcraft,  Naval Research Laboratory,  Sep. 2002 (Aug. 2005).
c*
c**********
c
      real       thin 
      parameter (thin=1.e-6)  ! minimum layer thickness (no division by 0.0)
c
      integer i,k,l,lf
      real    q,zb,zt,sok(ks)
c
        lf=1
        zb=po(1)
        do k= 1,ko
          zt = zb
          zb = po(k+1)
*         WRITE(6,*) 'k,zt,zb = ',k,zt,zb
          if     (zb-zt.lt.thin .or. zt.ge.pi(ki+1)) then
c
c ---       thin or bottomed layer, values taken from layer above
c
            do i= 1,ks
              so(k,i) = so(k-1,i)
            enddo !i
          else
c
c           form layer averages.
c
            if     (pi(lf).gt.zt) then
              WRITE(6,*) 'bad lf = ',lf
              stop
            endif
            do i= 1,ks
              sok(i) = 0.0
            enddo !i
            do l= lf,ki
              if     (pi(l).gt.zb) then
*               WRITE(6,*) 'l,lf= ',l,lf,l-1
                lf = l-1
                exit
              elseif (pi(l).ge.zt .and. pi(l+1).le.zb) then
c
c               the input layer is completely inside the output layer
c
                q   = max(pi(l+1)-pi(l),thin)/(zb-zt)
                do i= 1,ks
                  sok(i) = sok(i) + q*si(l,i)
                enddo !i
*               WRITE(6,*) 'L,q = ',l,q
              else
c
c               the input layer is partially inside the output layer
c
                q   = max(min(pi(l+1),zb)-max(pi(l),zt),thin)/(zb-zt)
                do i= 1,ks
                  sok(i) = sok(i) + q*si(l,i)
                enddo !i
*               WRITE(6,*) 'l,q = ',l,q
              endif
            enddo !l
            do i= 1,ks
              so(k,i) = sok(i)
            enddo !i
          endif
        enddo !k
      return
      end subroutine pcmtrc

      subroutine plmtrc(si,pi,ki,ks, so,po,ko)
      implicit none
c
      integer ki,ks,ko
      real    si(ki,ks),pi(ki+1),
     &        so(ko,ks),po(ko+1),flag
c
c**********
c*
c  1) remap from one set of vertical cells to another.
c     method: piecewise linear across each input cell
c             the output is the average of the interpolation
c             profile across each output cell.
c
c  2) input arguments:
c       si    - scalar fields in pi-layer space
c       pi    - layer interface depths (non-negative m)
c                 pi(   1) is the surface
c                 pi(ki+1) is the bathymetry
c       ki    - 1st dimension of si     (number of  input layers)
c       ks    - 2nd dimension of si,so  (number of fields)
c       po    - target interface depths (non-negative m)
c                 po(k+1) >= po(k)
c       ko    - 1st dimension of so     (number of output layers)
c       flag  - data void (land) marker
c
c  3) output arguments:
c       so    - scalar fields in po-layer space
c
c  4) except at data voids, must have:
c           pi(   1) == zero (surface)
c           pi( l+1) >= pi(l)
c           pi(ki+1) == bathymetry
c           0 <= po(k) <= po(k+1)
c      output layers completely below the bathymetry inherit values
c      from the layer above.
c
c  5) Tim Campbell, Mississippi State University, October 2002.
C     Alan J. Wallcraft,  Naval Research Laboratory,  Aug. 2005.
c*
c**********
c
      real,parameter :: thin=1.e-6  !minimum layer thickness
c
      integer i,k,l,lf
      real    q,qc,zb,zc,zt,sok(ks)
      real    sis(ki,ks),pit(ki+1)
      real    si_min(ks),si_max(ks)
c
c ---   inforce minval(si(:,i)) <= minval(so(:,i)) and
c ---           maxval(si(:,i)) >= maxval(so(:,i)) for i=1:ks
c ---   in particular this inforces non-negativity, e.g. of tracers
c ---   only required due to finite precision
c
        do i= 1,ks
          si_min(i) = minval(si(:,i))
          si_max(i) = maxval(si(:,i))
        enddo !i
c
c ---   compute PLM slopes for input layers
        do k=1,ki
          pit(k)=max(pi(k+1)-pi(k),thin)
        enddo
        call plmtrcx(pit,si,sis,ki,ks)
c ---   compute output layer averages
        lf=1
        zb=po(1)
        do k= 1,ko
          zt = zb
          zb = po(k+1)
*         WRITE(6,*) 'k,zt,zb = ',k,zt,zb
          if     (zb-zt.lt.thin .or. zt.ge.pi(ki+1)) then
c
c ---       thin or bottomed layer, values taken from layer above
c
            do i= 1,ks
              so(k,i) = so(k-1,i)
            enddo !i
          else
c
c           form layer averages.
c
            if     (pi(lf).gt.zt) then
              WRITE(6,*) 'bad lf = ',lf
              stop
            endif
            do i= 1,ks
              sok(i) = 0.0
            enddo !i
            do l= lf,ki
              if     (pi(l).gt.zb) then
*               WRITE(6,*) 'l,lf= ',l,lf,l-1
                lf = l-1
                exit
              elseif (pi(l).ge.zt .and. pi(l+1).le.zb) then
c
c               the input layer is completely inside the output layer
c
                q   = max(pi(l+1)-pi(l),thin)/(zb-zt)
                do i= 1,ks
                  sok(i) = sok(i) + q*si(l,i)
                enddo !i
*               WRITE(6,*) 'L,q = ',l,q
              else
c
c               the input layer is partially inside the output layer
c               average of linear profile is its center value
c
                q   = max( min(pi(l+1),zb)-max(pi(l),zt), thin )/(zb-zt)
                zc  = 0.5*(min(pi(l+1),zb)+max(pi(l),zt))
                qc  = (zc-pi(l))/pit(l) - 0.5
                do i= 1,ks
                  sok(i) = sok(i) + q*(si(l,i) + qc*sis(l,i))
                enddo !i
*               WRITE(6,*) 'l,q,qc = ',l,q,qc
              endif
            enddo !l
            do i= 1,ks
              so(k,i) = sok(i)
              so(k,i) = max( si_min(i), so(k,i) )
              so(k,i) = min( si_max(i), so(k,i) )
            enddo !i
          endif
        enddo !k
      return
      end subroutine plmtrc

      subroutine plmtrcx(pt, s,ss,ki,ks)
      implicit none
c
      integer ki,ks
      real    pt(ki+1),s(ki,ks),ss(ki,ks)
c
c**********
c*
c  1) generate a monotonic PLM interpolation of a layered field
c
c  2) input arguments:
c       pt    - layer interface thicknesses (non-zero)
c       s     - scalar fields in layer space
c       ki    - 1st dimension of s (number of layers)
c       ks    - 2nd dimension of s (number of fields)
c
c  3) output arguments:
c       ss    - scalar field slopes for PLM interpolation
c
c  4) except at data voids, must have:
c           pi(   1) == zero (surface)
c           pi( l+1) >= pi(:,:,l)
c           pi(ki+1) == bathymetry
c
c  5) Tim Campbell, Mississippi State University, September 2002.
c*
c**********
c
      integer l
      real    ql(ki),qc(ki),qr(ki)
c
      !compute grid spacing ratios for slope computations
      ql(1)=0.0
      qc(1)=0.0
      qr(1)=0.0
      do l=2,ki-1
        ql(l)=2.0*pt(l)/(pt(l-1)+pt(l))
        qc(l)=2.0*pt(l)/(pt(l-1)+2.0*pt(l)+pt(l+1))
        qr(l)=2.0*pt(l)/(pt(l)+pt(l+1))
      enddo
      ql(ki)=0.0
      qc(ki)=0.0
      qr(ki)=0.0
      !compute normalized layer slopes
      do l=1,ks
        call plmtrcs(ql,qc,qr,s(1,l),ss(1,l),ki)
      enddo
      return
      end subroutine plmtrcx

      subroutine plmtrcs(rl,rc,rr,a,s,n)
      implicit none
c
      integer,intent(in)  :: n
      real,   intent(in)  :: rl(n),rc(n),rr(n),a(n)
      real,   intent(out) :: s(n)
c
c**********
c*
c  1) generate slopes for monotonic piecewise linear distribution
c
c  2) input arguments:
c       rl   - left grid spacing ratio
c       rc   - center grid spacing ratio
c       rr   - right grid spacing ratio
c       a    - scalar field zone averages
c       n    - number of zones
c
c  3) output arguments:
c       s    - zone slopes
c
c  4) Tim Campbell, Mississippi State University, September 2002.
c*
c**********
c
      integer,parameter :: ic=2, im=1, imax=100
      real,parameter :: fracmin=1e-6, dfac=0.5
c
      integer i,j
      real    sl,sc,sr
      real    dnp,dnn,dl,dr,ds,frac
c
c Compute zone slopes
c Campbell Eq(15) -- nonuniform grid
c
      s(1)=0.0
      do j=2,n-1
        sl=rl(j)*(a(j)-a(j-1))
        sr=rr(j)*(a(j+1)-a(j))
        if (sl*sr.gt.0.) then
          s(j)=sign(min(abs(sl),abs(sr)),sl)
        else
          s(j)=0.0
        endif
      enddo
      s(n)=0.0
c
c Minimize discontinuities between zones
c Apply single pass discontinuity minimization: Campbell Eq(19)
c
      do j=2,n-1
        if(s(j).ne.0.0) then
          dl=-0.5*(s(j)+s(j-1))+a(j)-a(j-1)
          dr=-0.5*(s(j+1)+s(j))+a(j+1)-a(j)
          ds=sign(min(abs(dl),abs(dr)),dl)
          s(j)=s(j)+2.0*ds
        endif
      enddo
      return
      end subroutine plmtrcs
c
c
c> Revision history:
c>
c> Aug  2002 - new routine to put all tracer interactions in one place
c> Dec. 2003 - inforce non-negative bio-tracers
c> Aug. 2005 - interpolate trwall to actual layer structure
c> Aug. 2012 - constant in time trwall remains exactly constant
c> Oct. 2013 - added jerlv0=-1 and calls to swfrac_ij
c> May  2014 - use land/sea masks (e.g. ip) to skip land
