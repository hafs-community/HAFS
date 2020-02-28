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
      subroutine mxkprf(m,n)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_pipe       ! HYCOM debugging interface
c
c --- hycom version 2.1
      implicit none
c
      integer m,n
c
c ---------------------------------------------------------
c --- k-profile vertical mixing models
c ---   a) large, mc williams, doney kpp vertical diffusion
c ---   b) mellor-yamada 2.5 vertical diffusion
c ---   c) giss vertical diffusion
c ---------------------------------------------------------
c
      logical, parameter :: lpipe_mxkprf =.false.
      logical, parameter :: ldebug_dpmixl=.false.
c
      real      delp,dpmx,hblmax,sigmlj,thsur,thtop,alfadt,betads,zintf,
     &          thjmp(kdm),thloc(kdm)
      integer   i,j,k
      character text*12
c
      include 'stmt_fns.h'
c
      if (mxlmy) then
        call xctilr(u(      1-nbdy,1-nbdy,1,m),1,kk, 1,1, halo_uv)
        call xctilr(u(      1-nbdy,1-nbdy,1,n),1,kk, 1,1, halo_uv)
        call xctilr(v(      1-nbdy,1-nbdy,1,m),1,kk, 1,1, halo_vv)
        call xctilr(v(      1-nbdy,1-nbdy,1,n),1,kk, 1,1, halo_vv)
        call xctilr(p(      1-nbdy,1-nbdy,2  ),1,kk, 1,1, halo_ps)
        call xctilr(ubavg(  1-nbdy,1-nbdy,  m),1, 1, 1,1, halo_uv)
        call xctilr(vbavg(  1-nbdy,1-nbdy,  m),1, 1, 1,1, halo_vv)
      else
        call xctilr(u(      1-nbdy,1-nbdy,1,n),1,kk, 1,1, halo_uv)
        call xctilr(v(      1-nbdy,1-nbdy,1,n),1,kk, 1,1, halo_vv)
        call xctilr(p(      1-nbdy,1-nbdy,2  ),1,kk, 1,1, halo_ps)
      endif
c
c --- except for KPP, surface boundary layer is the mixed layer
      if (mxlgiss .or. mxlmy) then
        hblmax = bldmax*onem
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
            if (SEA_P) then
              dpbl(i,j) = 0.5*(dpmixl(i,j,n)+
     &                         dpmixl(i,j,m) )     !reduce time splitting
              dpbl(i,j) = min( dpbl(i,j), hblmax ) !may not be needed
            endif !ip
          enddo !i
        enddo !j
      endif !mxlgiss,mxlmy
c
c --- diffisuvity/viscosity calculation
c
!$OMP PARALLEL DO PRIVATE(j)
!$OMP&             SHARED(m,n)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        call mxkprfaj(m,n, j)
      enddo
!$OMP END PARALLEL DO
c
c --- optional spatial smoothing of viscosity and diffusivities on interior
c --- interfaces.
c
      if     (difsmo.gt.0) then
        util6(1:ii,1:jj) = klist(1:ii,1:jj)
        call xctilr(util6,                1,   1, 2,2, halo_ps)
c ---   update halo on all layers for simplicity
        call xctilr(dift(1-nbdy,1-nbdy,2),1,kk-1, 2,2, halo_ps)
        call xctilr(difs(1-nbdy,1-nbdy,2),1,kk-1, 2,2, halo_ps)
        call xctilr(vcty(1-nbdy,1-nbdy,2),1,kk-1, 2,2, halo_ps)
        do k=2,min(difsmo+1,kk)
          call psmooth_dif(dift(1-nbdy,1-nbdy,k),util6,k,2,0,ip, util1)
          call psmooth_dif(difs(1-nbdy,1-nbdy,k),util6,k,2,0,ip, util1)
          call psmooth_dif(vcty(1-nbdy,1-nbdy,k),util6,k,2,1,ip, util1)
        enddo
        call xctilr(vcty(1-nbdy,1-nbdy,kk+1),1, 1, 1,1, halo_ps)
      else
        call xctilr(vcty(1-nbdy,1-nbdy,   2),1,kk, 1,1, halo_ps)
      endif
c
      if     (lpipe .and. lpipe_mxkprf) then
c ---   compare two model runs.
        util6(1:ii,1:jj) = klist(1:ii,1:jj)
        write (text,'(a12)') 'klist       '
        call pipe_compare_sym1(util6,ip,text)
        if (mxlmy) then
          do k= 0,kk+1
            write (text,'(a9,i3)') 'q2     k=',k
            call pipe_compare_sym1(q2( 1-nbdy,1-nbdy,k,n),ip,text)
            write (text,'(a9,i3)') 'q2l    k=',k
            call pipe_compare_sym1(q2l(1-nbdy,1-nbdy,k,n),ip,text)
            write (text,'(a9,i3)') 'difqmy k=',k
            call pipe_compare_sym1(difqmy(1-nbdy,1-nbdy,k),ip,text)
            write (text,'(a9,i3)') 'diftmy k=',k
            call pipe_compare_sym1(diftmy(1-nbdy,1-nbdy,k),ip,text)
            write (text,'(a9,i3)') 'vctymy k=',k
            call pipe_compare_sym1(vctymy(1-nbdy,1-nbdy,k),ip,text)
          enddo
        endif
        do k= 1,kk+1
          write (text,'(a9,i3)') 'dift   k=',k
          call pipe_compare_sym1(dift(1-nbdy,1-nbdy,k),ip,text)
          write (text,'(a9,i3)') 'difs   k=',k
          call pipe_compare_sym1(difs(1-nbdy,1-nbdy,k),ip,text)
          write (text,'(a9,i3)') 'vcty   k=',k
          call pipe_compare_sym1(vcty(1-nbdy,1-nbdy,k),ip,text)
        enddo
      endif
c
c ---   final mixing of variables at p points
c
!$OMP PARALLEL DO PRIVATE(j)
!$OMP&             SHARED(m,n)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        call mxkprfbj(m,n, j)
      enddo
!$OMP END PARALLEL DO
c
c --- final velocity mixing at u,v points
c
!$OMP PARALLEL DO PRIVATE(j)
!$OMP&             SHARED(m,n)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        call mxkprfcj(m,n, j)
      enddo
!$OMP END PARALLEL DO
c
c --- mixed layer diagnostics
c
      if (diagno .or. mxlgiss .or. mxlmy) then
c
c --- diagnose new mixed layer depth based on density jump criterion
!$OMP   PARALLEL DO PRIVATE(j,i,k,
!$OMP&                      sigmlj,thsur,thtop,alfadt,betads,zintf,
!$OMP&                      thjmp,thloc)
!$OMP&         SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
            if (SEA_P) then
c
c ---       depth of mixed layer base set to interpolated depth where
c ---       the density jump is equivalent to a tmljmp temperature jump.
c ---       this may not vectorize, but is used infrequently.
              if (locsig) then
                sigmlj = -tmljmp*dsiglocdt(temp(i,j,1,n),
     &                                     saln(i,j,1,n),p(i,j,1))
              else
                sigmlj = -tmljmp*dsigdt(temp(i,j,1,n),saln(i,j,1,n))
              endif
              sigmlj = max(sigmlj,tmljmp*0.03)  !cold-water fix
*
              if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
                write (lp,'(i9,2i5,i3,a,2f7.4)')
     &            nstep,i+i0,j+j0,k,
     &            '   sigmlj =',
     &            -tmljmp*dsigdt(temp(i,j,1,n),saln(i,j,1,n)),
     &            sigmlj
              endif
*
              thloc(1)=th3d(i,j,1,n)
              do k=2,klist(i,j)
                if (locsig) then
                  alfadt=dsiglocdt(ahalf*(temp(i,j,k-1,n)+
     &                                    temp(i,j,k,  n) ),
     &                             ahalf*(saln(i,j,k-1,n)+
     &                                    saln(i,j,k,  n) ),
     &                                       p(i,j,k)       )*
     &                   (temp(i,j,k-1,n)-temp(i,j,k,  n))
                  betads=dsiglocds(ahalf*(temp(i,j,k-1,n)+
     &                                    temp(i,j,k,  n) ),
     &                             ahalf*(saln(i,j,k-1,n)+
     &                                    saln(i,j,k,  n) ),
     &                                       p(i,j,k)       )*
     &                   (saln(i,j,k-1,n)-saln(i,j,k,  n))
                  thloc(k)=thloc(k-1)-alfadt-betads
                else
                  thloc(k)=th3d(i,j,k,n)
                endif
              enddo !k
              dpmixl(i,j,n) = -zgrid(i,j,klist(i,j)+1)*onem  !bottom
              thjmp(1) = 0.0
              thsur = thloc(1)
              do k=2,klist(i,j)
                thsur    = min(thloc(k),thsur)  !ignore surface inversion
                thjmp(k) = max(thloc(k)-thsur,
     &                         thjmp(k-1)) !stable profile simplifies the code
*
                if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
                  write (lp,'(i9,2i5,i3,a,2f7.3,f7.4,f9.2)')
     &              nstep,i+i0,j+j0,k,
     &              '   th,thsur,jmp,zc =',
     &              thloc(k),thsur,thjmp(k),-zgrid(i,j,k)
                endif
c
                if (thjmp(k).ge.sigmlj) then
c             
c ---             find the density on the interface between layers
c ---             k-1 and k, using the same cubic polynominal as PQM
c
                  if     (k.eq.2) then
c ---               linear between cell centers
                    thtop = thjmp(1) + (thjmp(2)-thjmp(1))*
     &                                 dp(i,j,1,n)/
     &                                 max( dp(i,j,1,n)+
     &                                      dp(i,j,2,n) ,
     &                                      onemm )
                  elseif (k.eq.klist(i,j)) then
c ---               linear between cell centers
                    thtop = thjmp(k) + (thjmp(k-1)-thjmp(k))*
     &                                   dp(i,j,k,n)/
     &                                   max( dp(i,j,k,  n)+
     &                                        dp(i,j,k-1,n) ,
     &                                        onemm )
                  else                                           
                    thsur      = min(thloc(k+1),thsur)           
                    thjmp(k+1) = max(thloc(k+1)-thsur,
     &                               thjmp(k))        
                    zintf = zgrid(i,j,k-1) - 0.5*dp(i,j,k-1,n)*qonem
                    thtop = thjmp(k-2)*
     &                        ((zintf         -zgrid(i,j,k-1))*
     &                         (zintf         -zgrid(i,j,k  ))*
     &                         (zintf         -zgrid(i,j,k+1)) )/
     &                        ((zgrid(i,j,k-2)-zgrid(i,j,k-1))* 
     &                         (zgrid(i,j,k-2)-zgrid(i,j,k  ))* 
     &                         (zgrid(i,j,k-2)-zgrid(i,j,k+1)) ) +
     &                      thjmp(k-1)*
     &                        ((zintf         -zgrid(i,j,k-2))*    
     &                         (zintf         -zgrid(i,j,k  ))*    
     &                         (zintf         -zgrid(i,j,k+1)) )/
     &                        ((zgrid(i,j,k-1)-zgrid(i,j,k-2))* 
     &                         (zgrid(i,j,k-1)-zgrid(i,j,k  ))* 
     &                         (zgrid(i,j,k-1)-zgrid(i,j,k+1)) ) +
     &                      thjmp(k  )*
     &                        ((zintf         -zgrid(i,j,k-2))*    
     &                         (zintf         -zgrid(i,j,k-1))*    
     &                         (zintf         -zgrid(i,j,k+1)) )/
     &                        ((zgrid(i,j,k  )-zgrid(i,j,k-2))* 
     &                         (zgrid(i,j,k  )-zgrid(i,j,k-1))* 
     &                         (zgrid(i,j,k  )-zgrid(i,j,k+1)) ) +
     &                      thjmp(k+1)*
     &                        ((zintf         -zgrid(i,j,k-2))*
     &                         (zintf         -zgrid(i,j,k-1))*
     &                         (zintf         -zgrid(i,j,k  )) )/
     &                        ((zgrid(i,j,k+1)-zgrid(i,j,k-2))*
     &                         (zgrid(i,j,k+1)-zgrid(i,j,k-1))*
     &                         (zgrid(i,j,k+1)-zgrid(i,j,k  )) )
                    thtop = max( thjmp(k-1), min( thjmp(k), thtop ) )
*
                    if (ldebug_dpmixl .and.
     &                  i.eq.itest.and.j.eq.jtest) then
                      write (lp,'(i9,2i5,i3,a,2f7.3,f7.4,f9.2)')
     &                  nstep,i+i0,j+j0,k,
     &                  '  thi,thsur,jmp,zi =',
     &                  thtop,thsur,thjmp(k),-zintf
                    endif
                  endif !k.eq.2:k.eq.klist:else
c
                  if      (thtop.ge.sigmlj) then
c
c ---               in bottom half of layer k-1
c
                    dpmixl(i,j,n) =
     &                -zgrid(i,j,k-1)*onem +
     &                         0.5*dp(i,j,k-1,n)*
     &                         (sigmlj+epsil-thjmp(k-1))/
     &                         (thtop +epsil-thjmp(k-1))
                  else
c
c ---               in top half of layer k
c
                    dpmixl(i,j,n) =
     &                -zgrid(i,j,k)*onem -
     &                         0.5*dp(i,j,k,n)*
     &                         (1.0-(sigmlj  +epsil-thtop)/
     &                              (thjmp(k)+epsil-thtop) )
                  endif !part of layer
*
                  if (ldebug_dpmixl .and.
     &                i.eq.itest.and.j.eq.jtest) then
                    write (lp,'(i9,2i5,i3,a,f7.3,f7.4,f9.2)')
     &                nstep,i+i0,j+j0,k,
     &                '   thsur,top,dpmixl =',
     &                thsur,thtop,dpmixl(i,j,n)*qonem
                  endif
*
                  exit  !calculated dpmixl
                endif  !found dpmixl layer
              enddo !k
            endif !ip
          enddo !i
        enddo !j
c
!$OMP   END PARALLEL DO
c
c ---   smooth the mixed layer (might end up below the bottom).
        call psmooth(dpmixl(1-nbdy,1-nbdy,n),0,0,ip, util1)
*
        if (ldebug_dpmixl) then
          call xcsync(flush_lp)
        endif
*
      endif  !diagno .or. mxlgiss .or. mxlmy
c
      if (diagno) then
c
c --- calculate bulk mixed layer t, s, theta
c
!$OMP   PARALLEL DO PRIVATE(j,i,k,delp)
!$OMP&         SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
            if (SEA_P) then
              dpmixl(i,j,n)=min(dpmixl(i,j,n),p(i,j,kk+1))
              dpmixl(i,j,m)=    dpmixl(i,j,n)
              delp=min(p(i,j,2),dpmixl(i,j,n))
              tmix(i,j)=delp*temp(i,j,1,n)
              smix(i,j)=delp*saln(i,j,1,n)
              do k=2,kk
                delp=min(p(i,j,k+1),dpmixl(i,j,n))
     &              -min(p(i,j,k  ),dpmixl(i,j,n))
                tmix(i,j)=tmix(i,j)+delp*temp(i,j,k,n)
                smix(i,j)=smix(i,j)+delp*saln(i,j,k,n)
              enddo
              tmix(i,j)=tmix(i,j)/dpmixl(i,j,n)
              smix(i,j)=smix(i,j)/dpmixl(i,j,n)
              thmix(i,j)=sig(tmix(i,j),smix(i,j))-thbase
*
              if (ldebug_dpmixl .and.
     &            i.eq.itest.and.j.eq.jtest) then
                write (lp,'(i9,2i5,i3,a,f9.2)')
     &            nstep,i+i0,j+j0,k,
     &            '   dpmixl =',
     &            dpmixl(i,j,n)*qonem
              endif
*
           endif !ip
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
c
        call xctilr(p(     1-nbdy,1-nbdy,2),1,kk, 1,1, halo_ps)
        call xctilr(dpmixl(1-nbdy,1-nbdy,n),1, 1, 1,1, halo_ps)
c
!$OMP   PARALLEL DO PRIVATE(j,i,k,delp,dpmx)
!$OMP&         SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
c
c ---       calculate bulk mixed layer u
c
            if (SEA_U) then
              dpmx=dpmixl(i,j,n)+dpmixl(i-1,j,n)
              delp=min(p(i,j,2)+p(i-1,j,2),dpmx)
              umix(i,j)=delp*u(i,j,1,n)
              do k=2,kk
                delp= min(p(i,j,k+1)+p(i-1,j,k+1),dpmx)
     &               -min(p(i,j,k  )+p(i-1,j,k  ),dpmx)
                umix(i,j)=umix(i,j)+delp*u(i,j,k,n)
              enddo !k
              umix(i,j)=umix(i,j)/dpmx
            endif !iu
c
c ---       calculate bulk mixed layer v
c
            if (SEA_V) then
              dpmx=dpmixl(i,j,n)+dpmixl(i,j-1,n)
              delp=min(p(i,j,2)+p(i,j-1,2),dpmx)
              vmix(i,j)=delp*v(i,j,1,n)
              do k=2,kk
                delp= min(p(i,j,k+1)+p(i,j-1,k+1),dpmx)
     &               -min(p(i,j,k  )+p(i,j-1,k  ),dpmx)
                vmix(i,j)=vmix(i,j)+delp*v(i,j,k,n)
              enddo !k
              vmix(i,j)=vmix(i,j)/dpmx
            endif !iv
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
      endif                                           ! diagno
c
      return
      end
      subroutine mxkprfaj(m,n, j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n, j
c
c --- calculate viscosity and diffusivity
c
      integer i
c
      if (mxlkpp) then
        do i=1,ii
          if (SEA_P) then
            call mxkppaij(m,n, i,j)
          endif !ip
        enddo !i
      else if (mxlmy) then
        do i=1,ii
          if (SEA_P) then
            call mxmyaij(m,n, i,j)
          endif !ip
        enddo !i
      else if (mxlgiss) then
        do i=1,ii
          if (SEA_P) then
            call mxgissaij(m,n, i,j)
          endif !ip
        enddo !i
      endif
c
      return
      end
      subroutine mxkprfbj(m,n, j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n, j
c
c --- final mixing at p points
c
      integer i
c
      do i=1,ii
        if (SEA_P) then
          call mxkprfbij(m,n, i,j)
        endif
      enddo
c
      return
      end
      subroutine mxkprfcj(m,n, j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n, j
c
c --- final velocity mixing at u,v points
c
      integer i
c
      do i=1,ii
        if (SEA_U) then
          call mxkprfciju(m,n, i,j)
        endif !iu
        if (SEA_V) then
          call mxkprfcijv(m,n, i,j)
        endif !iv
      enddo !i
c
      return
      end
c
      subroutine mxkppaij(m,n, i,j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
c
c --- hycom version 1.0
      implicit none
c
      integer m,n, i,j
c
c -------------------------------------------------------------
c --- kpp vertical diffusion, single j-row (part A)
c --- vertical coordinate is z negative below the ocean surface
c
c --- Large, W.C., J.C. McWilliams, and S.C. Doney, 1994: Oceanic
c --- vertical mixing: a review and a model with a nonlocal
c --- boundary layer paramterization. Rev. Geophys., 32, 363-403.
c
c --- quadratic interpolation and variable Cv from a presentation
c --- at the March 2003 CCSM Ocean Model Working Group Meeting
c --- on KPP Vertical Mixing by Gokhan Danabasoglu and Bill Large
c --- http://www.ccsm.ucar.edu/working_groups/Ocean/agendas/030320.html
c --- quadratic interpolation implemented here by 3-pt collocation,
c --- which is slightly different to the Danabasoglu/Large approach.
c -------------------------------------------------------------
c
      real, parameter :: difmax = 9999.0e-4  !maximum diffusion/viscosity
      real, parameter :: dp0bbl =   20.0     !truncation dist. for bot. b.l.
      real, parameter :: ricrb  =    0.45    !critical bulk Ri for bot. b.l.
      real, parameter :: cv_max =    2.1     !maximum cv
      real, parameter :: cv_min =    1.7     !minimum cv
      real, parameter :: cv_bfq =  200.0     !cv scale factor
c
c local variables for kpp mixing
      real delta               ! fraction hbl lies beteen zgrid neighbors
      real zrefmn              ! nearsurface reference z, minimum
      real zref                ! nearsurface reference z
      real wref,qwref          ! nearsurface reference width,inverse
      real uref                ! nearsurface reference u
      real vref                ! nearsurface reference v
      real bref                ! nearsurface reference buoyancy
      real swfrac(kdm+1)       ! fractional surface shortwave radiation flux
      real shsq(kdm+1)         ! velocity shear squared
      real alfadt(kdm+1)       ! t contribution to density jump
      real betads(kdm+1)       ! s contribution to density jump
      real swfrml              ! fractional surface sw rad flux at ml base
      real ritop(kdm)          ! numerator of bulk richardson number
      real dbloc(kdm+1)        ! buoyancy jump across interface 
      real dvsq(kdm)           ! squared current shear for bulk richardson no.
      real zgridb(kdm+1)       ! zgrid for bottom boundary layer
      real hwide(kdm)          ! layer thicknesses in m (minimum 1mm)
      real dpmm(kdm)           !     max(onemm,dp(i,j,:,n))
      real qdpmm(kdm)          ! 1.0/max(onemm,dp(i,j,:,n))
      real pij(kdm+1)          ! local copy of p(i,j,:)
      real case                ! 1 in case A; =0 in case B
      real hbl                 ! boundary layer depth
      real hbbl                ! bottom boundary layer depth
      real rib(3)              ! bulk richardson number
      real rrho                ! double diffusion parameter
      real diffdd              ! double diffusion diffusivity scale
      real prandtl             ! prandtl number
      real rigr                ! local richardson number
      real fri                 ! function of Rig for KPP shear instability
      real stable              ! = 1 in stable forcing; =0 in unstable
      real dkm1(3)             ! boundary layer diffusions at nbl-1 level
      real gat1(3)             ! shape functions at dnorm=1
      real dat1(3)             ! derivative of shape functions at dnorm=1
      real blmc(kdm+1,3)       ! boundary layer mixing coefficients
      real bblmc(kdm+1,3)      ! boundary layer mixing coefficients
      real wm                  ! momentum velocity scale
      real ws                  ! scalar velocity scale
      real dnorm               ! normalized depth
      real tmn                 ! time averaged SST
      real smn                 ! time averaged SSS
      real dsgdt               ! dsigdt(tmn,smn)
      real buoyfs              ! salinity  surface buoyancy (into atmos.)
      real buoyfl              ! total     surface buoyancy (into atmos.)
      real buoysw              ! shortwave surface buoyancy (into atmos.)
      real bfsfc               ! surface buoyancy forcing   (into atmos.)
      real bfbot               ! bottom buoyancy forcing
      real hekmanb             ! bottom ekman layer thickness
      real cormn4              ! = 4 x min. coriolis magnitude (at 4N, 4S)
      real dflsiw              ! internal wave diffusivity
      real dflmiw              ! internal wave viscosity
      real dflbot(kdm+1)       ! bottom intensified background viscosity
      real bfq                 ! buoyancy frequency
      real cvk                 ! ratio of buoyancy frequencies
      real ahbl,bhbl,chbl,dhbl ! coefficients for quadratic hbl calculation
c
      logical lhbl             ! safe to use quadratic hbl calculation
c
      integer nbl              ! layer containing boundary layer base
      integer nbbl             ! layer containing bottom boundary layer base
      integer kup2,kdn2,kup,kdn! bulk richardson number indices
c
c --- local 1-d arrays for matrix solution
      real u1do(kdm+1),u1dn(kdm+1),v1do(kdm+1),v1dn(kdm+1),t1do(kdm+1),
     &     t1dn(kdm+1),s1do(kdm+1),s1dn(kdm+1),
     &     diffm(kdm+1),difft(kdm+1),diffs(kdm+1),
     &     ghat(kdm+1),zm(kdm+1),hm(kdm),dzb(kdm)
c
c --- local 1-d arrays for iteration loops
      real uold(kdm+1),vold (kdm+1),told (kdm+1),
     &     sold(kdm+1),thold(kdm+1)
c
c --- tridiagonal matrix solution arrays
      real tri(kdm,0:1)      ! dt/dz/dz factors in trid. matrix
      real tcu(kdm),         ! upper coeff for (k-1) on k line of trid.matrix
     &     tcc(kdm),         ! central ...     (k  ) ..
     &     tcl(kdm),         ! lower .....     (k-1) ..
     &     rhs(kdm)          ! right-hand-side terms
c
      real dtemp,dsaln,wq,wt,wz0,wz1,wz2,ratio,q,ghatflux,
     &     dvdzup,dvdzdn,viscp,difsp,diftp,f1,sigg,aa1,aa2,aa3,gm,gs,gt,
     &     dkmp2,dstar,hblmin,hblmax,sflux1,vtsq,
     &     vctyh,difsh,difth,zrefo,qspcifh,hbblmin,hbblmax,
     &     chl,
     &     x0,x1,x2,y0,y1,y2
c
      integer k,k1,ka,kb,nlayer,ksave,iter,jrlv
c
      integer iglobal,jglobal
c
      include 'stmt_fns.h'
c
      cormn4 = 4.0e-5  !4 x min. coriolis magnitude (at 4N, 4S)
c
      iglobal=i0+i !for debugging
      jglobal=j0+j !for debugging
      if     (iglobal+jglobal.eq.-99) then
        write(lp,*) iglobal,jglobal  !prevent optimization
      endif
c
c --- internal wave diffusion/viscosity
      dflsiw = diws(i,j)
      dflmiw = diwm(i,j)
c
c --- locate lowest substantial mass-containing layer.
      pij(1)=p(i,j,1)
      do k=1,kk
        dpmm( k)  =max(onemm,dp(i,j,k,n))
        qdpmm(k)  =1.0/dpmm(k)
        pij(  k+1)=pij(k)+dp(i,j,k,n)
        p(i,j,k+1)=pij(k+1)
      enddo
      do k=kk,1,-1
        if (dpmm(k).gt.tencm) then
          exit
        endif
      enddo
      klist(i,j)=max(k,2)  !always consider at least 2 layers
c
c --- dflbot (Prandtl number of one, i.e. diffusion = viscosity)
      if     (botdiw) then
c ---   diffusion coefficent profile is: K = Kb / (1 + h/h0)**2
c ---    where diwbot = Kb; diwqh0 = 1/h0 (input as h0, see forfun.f)
c ---     Decloedt T. and D.S. Luther, 2009: On a Simple Empirical 
c ---     Parameterization of Topography-Catalyzed Diapycnal Mixing
c ---     in the Abyssal Ocean. JPO, 40, pp 487-508.
c ---   use Simpson's rule to estimate the average K across each layer
        wz0 = 1.0 / (1.0 + (pij(kk+1)-     pij(1)  )*diwqh0(i,j))**2
        wz1 = 1.0 / (1.0 + (pij(kk+1)-0.5*(pij(1)+
     &                                     pij(2) ))*diwqh0(i,j))**2
        wz2 = 1.0 / (1.0 + (pij(kk+1)-     pij(2)  )*diwqh0(i,j))**2
        wq  = wz0 + wz2 + 4.0*wz1  !6 times the average value over layer 1
        do k= 2,kk
          wt  = wq
          wz0 = wz2
          wz1 = 1.0 / (1.0 + (pij(kk+1)-0.5*(pij(k)  +
     &                                       pij(k+1) ))*diwqh0(i,j))**2
          wz2 = 1.0 / (1.0 + (pij(kk+1)-     pij(k+1)  )*diwqh0(i,j))**2
          wq  = wz0 + wz2 + 4.0*wz1  !6 times the average value over layer k
          dflbot(k) = (0.5/6.0)*(wt+wq)*diwbot(i,j)
        enddo
        dflbot(kk+1) = dflbot(kk)
      else
        do k= 2,kk+1
          dflbot(k) = 0.0
        enddo
      endif !botdiw
c
c --- forcing of t,s by surface fluxes. flux positive into ocean.
c
      qspcifh=1.0/spcifh
c
      if (thermo .or. sstflg.gt.0 .or. srelax) then
c ---   shortwave flux penetration depends on kpar or chl or jerlov water type.
        if     (jerlv0.le.0) then
          chl =  akpar(i,j,lk0)*wk0+akpar(i,j,lk1)*wk1
     &          +akpar(i,j,lk2)*wk2+akpar(i,j,lk3)*wk3
        endif
        call swfrac_ij(chl,pij,klist(i,j)+1,qonem,jerlov(i,j),swfrac)
      else
        swfrac(1)  = 1.0
        swfrac(2:) = 0.0
      endif
c
      do k=1,kk
        if (thermo .or. sstflg.gt.0 .or. srelax) then
          if (k.eq.1) then
            sflux1=surflx(i,j)-sswflx(i,j)
            dtemp=(sflux1+(1.-swfrac(k+1))*sswflx(i,j))*
     &            delt1*g*qspcifh*qdpmm(k)
            dsaln=salflx(i,j)*
     &            delt1*g*        qdpmm(k) 
cdiag       if (i.eq.itest.and.j.eq.jtest) then
cdiag         write (lp,101) nstep,i+i0,j+j0,k, 
cdiag&          1.0,swfrac(k+1),dtemp,dsaln
cdiag         call flush(lp)
cdiag       endif
          elseif (k.le.klist(i,j)) then
            dtemp=(swfrac(k)-swfrac(k+1))*sswflx(i,j)*
     &            delt1*g*qspcifh*qdpmm(k)
            dsaln=0.0
cdiag       if (i.eq.itest.and.j.eq.jtest) then
cdiag         write (lp,101) nstep,i+i0,j+j0,k,
cdiag&          swfrac(k),swfrac(k+1),dtemp
cdiag         call flush(lp)
cdiag       endif
          else !k.gt.klist(i,j)
            dtemp=0.0
            dsaln=0.0
          endif 
        else !.not.thermo ...
          dtemp=0.0
          dsaln=0.0
        endif !thermo.or.sstflg.gt.0.or.srelax:else
c
c --- modify t and s; set old value arrays at p points for initial iteration
        if (k.le.klist(i,j)) then
          temp(i,j,k,n)=    temp(i,j,k,n)+dtemp
          saln(i,j,k,n)=max(saln(i,j,k,n)+dsaln,0.0)  !must be non-negative
          th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
          told (k)=temp(i,j,k,n)
          sold (k)=saln(i,j,k,n)
          if (locsig) then
            if (k.eq.1) then
              thold(k)=th3d(i,j,k,n)
            else
              ka=k-1
              alfadt(k)=dsiglocdt(ahalf*(told(ka)+told(k)),
     &                            ahalf*(sold(ka)+sold(k)),
     &                                           p(i,j,k)  )*
     &                                  (told(ka)-told(k))
              betads(k)=dsiglocds(ahalf*(told(ka)+told(k)),
     &                            ahalf*(sold(ka)+sold(k)),
     &                                           p(i,j,k)  )*
     &                                  (sold(ka)-sold(k))
              thold(k)=thold(ka)-alfadt(k)-betads(k)
            endif
          else
            thold(k)=th3d(i,j,k,n)
          endif
          uold (k)=.5*(u(i,j,k,n)+u(i+1,j  ,k,n))
          vold (k)=.5*(v(i,j,k,n)+v(i  ,j+1,k,n))
        endif
      enddo !k
c
      k=klist(i,j)
      ka=k+1
      kb=min(ka,kk)
      told (ka)=temp(i,j,kb,n)
      sold (ka)=saln(i,j,kb,n)
      if (locsig) then
        alfadt(ka)=dsiglocdt(ahalf*(told(k)+told(ka)),
     &                       ahalf*(sold(k)+sold(ka)),
     &                                     p(i,j,ka)  )*
     &                             (told(k)-told(ka))
        betads(ka)=dsiglocds(ahalf*(told(k)+told(ka)),
     &                       ahalf*(sold(k)+sold(ka)),
     &                                     p(i,j,ka)  )*
     &                             (sold(k)-sold(ka))
        thold(ka)=thold(k)-alfadt(ka)-betads(ka)
      else
        thold(ka)=th3d(i,j,kb,n)
      endif
      uold (ka)=.5*(u(i,j,k,n)+u(i+1,j  ,k,n))
      vold (ka)=.5*(v(i,j,k,n)+v(i  ,j+1,k,n))
c
c --- calculate z at vertical grid levels - this array is the z values in m
c --- at the mid-depth of each micom layer except for index klist+1, where it
c --- is the z value of the bottom
c
c --- calculate layer thicknesses in m
      do k=1,kk
        if (k.eq.1) then
          hwide(k)=dpmm(k)*qonem
          zgrid(i,j,k)=-.5*hwide(k)
        else if (k.lt.klist(i,j)) then
          hwide(k)=dpmm(k)*qonem
          zgrid(i,j,k)=zgrid(i,j,k-1)-.5*(hwide(k-1)+hwide(k))
        else if (k.eq.klist(i,j)) then
          hwide(k)=dpmm(k)*qonem
          zgrid(i,j,k)=zgrid(i,j,k-1)-.5*(hwide(k-1)+hwide(k))
          zgrid(i,j,k+1)=zgrid(i,j,k)-.5*hwide(k)
        else
          hwide(k)=0.
        endif
      enddo
c
c --- perform niter iterations to execute the semi-implicit solution of the
c --- diffusion equation. at least two iterations are recommended
c
      do iter=1,niter
c
c --- calculate layer variables required to estimate bulk richardson number
c
c --- calculate nearsurface reference variables,
c --- averaged over -2*epsilon*zgrid, but no more than 8m.
        zrefmn = -4.0
        zrefo  =  1.0  ! impossible value
        do k=1,klist(i,j)
          zref=max(epsilon*zgrid(i,j,k),zrefmn)  ! nearest to zero
          if     (zref.ne.zrefo) then  ! new zref
            wref =-2.0*zref
            qwref=1.0/wref
            wq=min(hwide(1),wref)*qwref
            uref=uold(1)*wq
            vref=vold(1)*wq
            bref=-g*thref*(thold(1)+thbase)*wq
            wt=0.0
            do ka=2,k
              wt=wt+wq
              if (wt.ge.1.0) then
                exit
              endif
              wq=min(1.0-wt,hwide(ka)*qwref)
              uref=uref+uold(ka)*wq
              vref=vref+vold(ka)*wq
              bref=bref-g*thref*(thold(ka)+thbase)*wq
            enddo
          endif
          zrefo=zref
c
          ritop(k)=(zref-zgrid(i,j,k))*
     &             (bref+g*thref*(thold(k)+thbase))
          dvsq(k)=(uref-uold(k))**2+(vref-vold(k))**2
*
*         if (i.eq.itest.and.j.eq.jtest) then
*           if     (k.eq.1) then
*             write(lp,'(3a)')
*    &          ' k        z  zref',
*    &          '      u   uref      v   vref',
*    &          '      b   bref    ritop   dvsq'
*           endif
*           write(lp,'(i2,f9.2,f6.2,4f7.3,2f7.3,f9.4,f7.4)')
*    &         k,zgrid(i,j,k),zref,
*    &         uold(k),uref,vold(k),vref,
*    &         -g*thref*(thold(k)+thbase),bref,
*    &         ritop(k),dvsq(k)
*           call flush(lp)
*         endif
cdiag     if (i.eq.itest.and.j.eq.jtest) then
cdiag       write (lp,'(i9,2i5,i3,a,f8.2,f8.3)')
cdiag&          nstep,i+i0,j+j0,k,
cdiag&          '  z,swfrac =',zgrid(i,j,k),swfrac(k)
cdiag       call flush(lp)
cdiag     endif
        enddo  !k=1,klist
c
c --- calculate interface variables required to estimate interior diffusivities
        do k=1,klist(i,j)
          k1=k+1
          ka=min(k1,kk)
          shsq  (k1)=(uold(k)-uold(k1))**2+(vold(k)-vold(k1))**2
          if (.not.locsig) then
            alfadt(k1)=dsigdt(ahalf*(told(k)+told(k1)),
     &                        ahalf*(sold(k)+sold(k1)) )*
     &                              (told(k)-told(k1))
            betads(k1)=dsigds(ahalf*(told(k)+told(k1)),
     &                        ahalf*(sold(k)+sold(k1)) )*
     &                              (sold(k)-sold(k1))
            dbloc(k1)=-g* thref*(thold(k)-thold(ka))
          else
            dbloc(k1)=-g*thref*(alfadt(k1)+betads(k1))
          endif
        enddo
c
c --- zero 1-d arrays for viscosity/diffusivity calculations
c
        do k=1,kk+1
          vcty (i,j,k)  =0.0
          dift (i,j,k)  =0.0
          difs (i,j,k)  =0.0
          ghats(i,j,k)  =0.0
          blmc(     k,1)=0.0
          blmc(     k,2)=0.0
          blmc(     k,3)=0.0
          bblmc(    k,1)=0.0
          bblmc(    k,2)=0.0
          bblmc(    k,3)=0.0
        enddo
c
c --- determine interior diffusivity profiles throughout the water column
c
c --- shear instability plus background internal wave contributions
        do k=2,klist(i,j)+1
          if (shinst) then
            q   =zgrid(i,j,k-1)-zgrid(i,j,k) !0.5*(hwide(k-1)+hwide(k))
            rigr=max(0.0,dbloc(k)*q/(shsq(k)+epsil))
            ratio=min(rigr*qrinfy,1.0)
            fri=(1.0-ratio*ratio)
            fri=fri*fri*fri
            vcty(i,j,k)=min(difm0*fri+dflmiw+dflbot(k),difmax)
            difs(i,j,k)=min(difs0*fri+dflsiw+dflbot(k),difmax)
          else
            vcty(i,j,k)=dflmiw+dflbot(k)
            difs(i,j,k)=dflsiw+dflbot(k)
          endif
          dift(i,j,k)=difs(i,j,k)
        enddo 
c
c --- double-diffusion (salt fingering and diffusive convection)
        if (dbdiff) then
          do k=2,klist(i,j)+1
c
c --- salt fingering case
            if (-alfadt(k).gt.betads(k) .and. betads(k).gt.0.) then
              rrho= min(-alfadt(k)/betads(k),rrho0)
              diffdd=1.-((rrho-1.)/(rrho0-1.))**2
              diffdd=dsfmax*diffdd*diffdd*diffdd
              dift(i,j,k)=dift(i,j,k)+0.7*diffdd
              difs(i,j,k)=difs(i,j,k)+diffdd
c
c --- diffusive convection case
            else if ( alfadt(k).gt.0.0 .and. betads(k).lt.0.0
     &         .and. -alfadt(k).gt.betads(k)) then
              rrho=-alfadt(k)/betads(k)
              diffdd=1.5e-6*9.*.101*exp(4.6*exp(-.54*(1./rrho-1.)))
              if (rrho.gt.0.5) then
                prandtl=(1.85-.85/rrho)*rrho
              else
                prandtl=.15*rrho
              endif
              dift(i,j,k)=dift(i,j,k)+diffdd
              difs(i,j,k)=difs(i,j,k)+prandtl*diffdd
            endif
          enddo
        endif
c
cdiag   if (i.eq.itest.and.j.eq.jtest) then
cdiag      write (lp,102) (nstep,iter,i+i0,j+j0,k,
cdiag&     hwide(k),1.e4*vcty(i,j,k),1.e4*dift(i,j,k),1.e4*difs(i,j,k),
cdiag&       k=1,kk+1)
cdiag      call flush(lp)
cdiag   endif
c
c --- calculate boundary layer diffusivity profiles and match these to the
c --- previously-calculated interior diffusivity profiles
c
c --- diffusivities within the surface boundary layer are parameterized
c --- as a function of boundary layer thickness times a depth-dependent
c --- turbulent velocity scale (proportional to ustar) times a third-order
c --- polynomial shape function of depth. boundary layer diffusivities depend
c --- on surface forcing (the magnitude of this forcing and whether it is
c --- stabilizing or de-stabilizing) and the magnitude and gradient of interior
c --- mixing at the boundary layer base. boundary layer diffusivity profiles
c --- are smoothly matched to interior diffusivity profiles at the boundary
c --- layer base (the profiles and their first derivatives are continuous
c --- at z=-hbl). the turbulent boundary layer depth is diagnosed first, the
c --- boundary layer diffusivity profiles are calculated, then the boundary
c --- and interior diffusivity profiles are combined.
c
c --- minimum hbl is    top mid-layer + 1 cm or bldmin,
c --- maximum hbl is bottom mid-layer - 1 cm or bldmax.
c
        hblmin=max(              hwide(1)+0.01,bldmin)
        hblmax=min(-zgrid(i,j,klist(i,j))-0.01,bldmax)
c
c --- buoyfl = total buoyancy flux (m**2/sec**3) into atmos.
c --- note: surface density increases (column is destabilized) if buoyfl > 0
c --- buoysw = shortwave radiation buoyancy flux (m**2/sec**3) into atmos.
c --- salflx, sswflx and surflx are positive into the ocean
      tmn=.5*(temp(i,j,1,m)+temp(i,j,1,n))
      smn=.5*(saln(i,j,1,m)+saln(i,j,1,n))
      dsgdt=          dsigdt(tmn,smn)
      buoyfs=g*thref*(dsigds(tmn,smn)*salflx(i,j)*thref)
      buoyfl=buoyfs+
     &       g*thref*(dsgdt          *surflx(i,j)*thref/spcifh)
      buoysw=g*thref*(dsgdt          *sswflx(i,j)*thref/spcifh)
c 
c --- diagnose the new boundary layer depth as the depth where a bulk
c --- richardson number exceeds ric
c
c --- initialize hbl and nbl to bottomed out values
        kup2=1
        kup =2
        kdn =3
        rib(kup2)=0.0
        rib(kup) =0.0
        nbl=klist(i,j)
        hbl=hblmax
c
c --- diagnose hbl and nbl
        do k=2,nbl
          case=-zgrid(i,j,k)
          bfsfc=buoyfl-swfrac(k)*buoysw
          if     (bfsfc.le.0.0) then
            stable=1.0
            dnorm =1.0
          else
            stable=0.0
            dnorm =epsilon
          endif
c
c --- compute turbulent velocity scales at dnorm, for
c --- hbl = case = -zgrid(i,j,k)
          call wscale(i,j,case,dnorm,bfsfc,wm,ws,1)
c
c --- compute the turbulent shear contribution to rib
          if     (max(dbloc(k),dbloc(k+1)).gt.0.0) then
            bfq=0.5*(dbloc(k  )/(zgrid(i,j,k-1)-zgrid(i,j,k  ))+
     &               dbloc(k+1)/(zgrid(i,j,k  )-zgrid(i,j,k+1)) )
            if     (bfq.gt.0.0) then
              bfq=sqrt(bfq)
            else
              bfq=0.0  !neutral or unstable
            endif
          else
            bfq=0.0  !neutral or unstable
          endif
          if     (bfq.gt.0.0) then
            if     (cv.ne.0.0) then
              cvk=cv
            else !frequency dependent version
              cvk=max(cv_max-cv_bfq*bfq,cv_min) !between cv_min and cv_max
            endif
            vtsq=-zgrid(i,j,k)*ws*bfq*vtc*cvk
          else
            vtsq=0.0
          endif !bfq>0:else
c
c --- compute bulk richardson number at new level
          rib(kdn)=ritop(k)/(dvsq(k)+vtsq+epsil)
          if (nbl.eq.klist(i,j).and.rib(kdn).ge.ricr) then
c ---       interpolate to find hbl as the depth where rib = ricr
            if     (k.eq.2 .or. hblflg.eq.0) then  !nearest interface
              hbl = -zgrid(i,j,k-1)+0.5*hwide(k-1)
            elseif (k.lt.4 .or. hblflg.eq.1) then  !linear
              hbl = -zgrid(i,j,k-1)+
     &                 (zgrid(i,j,k-1)-zgrid(i,j,k))*
     &                 (ricr-rib(kup))/(rib(kdn)-rib(kup)+epsil)
            else !quadratic
c
c ---         Determine the coefficients A,B,C of the polynomial
c ---           Y(X) = A * (X-X2)**2 + B * (X-X2) + C
c ---         which goes through the data: (X[012],Y[012])
c
              x0 = zgrid(i,j,k-2)
              x1 = zgrid(i,j,k-1)
              x2 = zgrid(i,j,k)
              y0 = rib(kup2)
              y1 = rib(kup)
              y2 = rib(kdn)
              ahbl = ( (y0-y2)*(x1-x2) -
     &                 (y1-y2)*(x0-x2)  )/
     &               ( (x0-x2)*(x1-x2)*(x0-x1) )
              bhbl = ( (y1-y2)*(x0-x2)**2 -
     &                 (y0-y2)*(x1-x2)**2  ) /
     &               ( (x0-x2)*(x1-x2)*(x0-x1) )
              if     (abs(bhbl).gt.epsil) then
                lhbl = abs(ahbl)/abs(bhbl).gt.epsil
              else
                lhbl = .true.
              endif
              if     (lhbl) then !quadratic
c ---           find root of Y(X)-RICR nearest to X2
                chbl = y2 - ricr
                dhbl = bhbl**2 - 4.0*ahbl*chbl
                if     (dhbl.lt.0.0) then !linear
                  hbl = -(x2 + (x1-x2)*(y2-ricr)/(y2-y1+epsil))
                else
                  dhbl = sqrt(dhbl)
                  if     (abs(bhbl+dhbl).ge.
     &                    abs(bhbl-dhbl)    ) then
                    hbl = -(x2 - 2.0*chbl/(bhbl+dhbl))
                  else
                    hbl = -(x2 - 2.0*chbl/(bhbl-dhbl))
                  endif !nearest root
                endif !bhbl**2-4.0*ahbl*chbl.lt.0.0:else
              else !linear
                hbl = -(x2 + (x1-x2)*(y2-ricr)/(y2-y1+epsil))
              endif !quadratic:linear
            endif !linear:quadratic
            nbl=k
            if (hbl.lt.hblmin) then
              hbl=hblmin
              nbl=2
            endif
            if (hbl.gt.hblmax) then
              hbl=hblmax
              nbl=klist(i,j)
            endif
            exit !k-loop
          endif
c
          ksave=kup2
          kup2=kup
          kup =kdn
          kdn =ksave
        enddo  !k=1,nbl
c
c --- calculate swfrml, the fraction of solar radiation left at depth hbl
        call swfrml_ij(chl,hbl*onem,pij(kdm+1),qonem,jerlov(i,j),swfrml)
cdiag   if (i.eq.itest.and.j.eq.jtest) then
cdiag     write (lp,'(i9,2i5,i3,a,f8.2,f6.3)')
cdiag&        nstep,i+i0,j+j0,nbl,
cdiag&        '  hbl,swfrml =',hbl,swfrml
cdiag     call flush(lp)
cdiag   endif
c
c --- limit check on hbl for negative (stablizing) surface buoyancy forcing
        bfsfc=buoyfl-swfrml*buoysw
        if (bfsfc.le.0.0) then
          bfsfc=bfsfc-epsil  !insures bfsfc never=0
          hmonob(i,j)=min(-cmonob*ustar(i,j)**3/(vonk*bfsfc), hblmax)
          hbl=max(hblmin,
     &            min(hbl,
     &                hekman(i,j),
     &                hmonob(i,j)))
        else
          hmonob(i,j)=hblmax
        endif
c
c --- find new nbl and re-calculate swfrml
        nbl=klist(i,j)
        do k=2,klist(i,j)
          if (-zgrid(i,j,k).gt.hbl) then
            nbl=k
            exit
          endif
        enddo
        call swfrml_ij(chl,hbl*onem,pij(kdm+1),qonem,jerlov(i,j),swfrml)
cdiag   if (i.eq.itest.and.j.eq.jtest) then
cdiag     write (lp,'(i9,2i5,i3,a,f8.2,f6.3)')
cdiag&        nstep,i+i0,j+j0,nbl,
cdiag&        '  hbl,swfrml =',hbl,swfrml
cdiag     call flush(lp)
cdiag   endif
c
c --- find forcing stability and buoyancy forcing for final hbl values
c --- determine case (for case=0., hbl lies between -zgrid(i,j,nbl)
c --- and the interface above. for case=1., hbl lies between 
c --- -zgrid(i,j,nbl-1) and the interface below)
c
c --- velocity scales at hbl
        bfsfc=buoyfl-swfrml*buoysw
        if     (bfsfc.le.0.0) then
          bfsfc=bfsfc-epsil  !insures bfsfc never=0
          stable=1.0
          dnorm =1.0
        else
          stable=0.0
          dnorm =epsilon
        endif
        case=.5+sign(.5,-zgrid(i,j,nbl)-.5*hwide(nbl)-hbl)
c
        buoflx(i,j)=bfsfc                          !mixed layer buoyancy
        bhtflx(i,j)=bfsfc-buoyfs                   !buoyancy from heat flux
        mixflx(i,j)=surflx(i,j)-swfrml*sswflx(i,j) !mixed layer heat flux
c
        call wscale(i,j,hbl,dnorm,bfsfc,wm,ws,1)
c
c --- compute the boundary layer diffusivity profiles. first, find interior
c --- viscosities and their vertical derivatives at hbl
        ka=nint(case)*(nbl-1)+(1-nint(case))*nbl
        q=(hbl*onem-p(i,j,ka))*qdpmm(ka)
        vctyh=vcty(i,j,ka)+q*(vcty(i,j,ka+1)-vcty(i,j,ka))
        difsh=difs(i,j,ka)+q*(difs(i,j,ka+1)-difs(i,j,ka))
        difth=dift(i,j,ka)+q*(dift(i,j,ka+1)-dift(i,j,ka))
c
        q=(hbl+zgrid(i,j,nbl-1))/(zgrid(i,j,nbl-1)-zgrid(i,j,nbl))
        dvdzup=(vcty(i,j,nbl-1)-vcty(i,j,nbl  ))/hwide(nbl-1)
        dvdzdn=(vcty(i,j,nbl  )-vcty(i,j,nbl+1))/hwide(nbl  )
        viscp=.5*((1.-q)*(dvdzup+abs(dvdzup))+q*(dvdzdn+abs(dvdzdn)))
        dvdzup=(difs(i,j,nbl-1)-difs(i,j,nbl  ))/hwide(nbl-1)
        dvdzdn=(difs(i,j,nbl  )-difs(i,j,nbl+1))/hwide(nbl  )
        difsp=.5*((1.-q)*(dvdzup+abs(dvdzup))+q*(dvdzdn+abs(dvdzdn)))
        dvdzup=(dift(i,j,nbl-1)-dift(i,j,nbl  ))/hwide(nbl-1) 
        dvdzdn=(dift(i,j,nbl  )-dift(i,j,nbl+1))/hwide(nbl  )
        diftp=.5*((1.-q)*(dvdzup+abs(dvdzup))+q*(dvdzdn+abs(dvdzdn)))
c
        f1=-stable*c11*bfsfc/(ustar(i,j)**4+epsil) 
c
        gat1(1)=vctyh/hbl/(wm+epsil)
        dat1(1)=min(0.,-viscp/(wm+epsil)+f1*vctyh)
c
        gat1(2)=difsh/hbl/(ws+epsil)
        dat1(2)=min(0.,-difsp/(ws+epsil)+f1*difsh) 
c
        gat1(3)=difth/hbl/(ws+epsil)
        dat1(3)=min(0.,-diftp/(ws+epsil)+f1*difth)
c
c --- compute turbulent velocity scales on the interfaces
        do k=2,kk+1
          if (k.le.min(nbl,klist(i,j))) then
            sigg=p(i,j,k)/(hbl*onem)
            dnorm=stable*sigg+(1.-stable)*min(sigg,epsilon)
c
            call wscale(i,j,hbl,dnorm,bfsfc,wm,ws,1)
c
c --- compute the dimensionless shape functions at the interfaces
            aa1=sigg-2.
            aa2=3.-2.*sigg
            aa3=sigg-1.
c
            gm=aa1+aa2*gat1(1)+aa3*dat1(1) 
            gs=aa1+aa2*gat1(2)+aa3*dat1(2)
            gt=aa1+aa2*gat1(3)+aa3*dat1(3)
c
c --- compute boundary layer diffusivities at the interfaces
            blmc(k,1)=hbl*wm*sigg*(1.+sigg*gm)
            blmc(k,2)=hbl*ws*sigg*(1.+sigg*gs)
            blmc(k,3)=hbl*ws*sigg*(1.+sigg*gt)
c
c --- compute nonlocal transport forcing term = ghats * <ws>o
            if (nonloc) then
#if defined(STOKES)
c ---         To include Langmuir turbulence effects, multiply ghats
c ---         by a factor of Lgam (McWilliam & Sullivan 2001)
              ghats(i,j,k)=1.04 * (1.-stable)*cg/(ws*hbl+epsil)
#else
              ghats(i,j,k)=       (1.-stable)*cg/(ws*hbl+epsil)
#endif
            endif
          endif !k.le.min(nbl,klist)
        enddo !k
c
c --- enhance diffusivities on the interface closest to hbl
c
c --- first compute diffusivities at nbl-1 grid level 
        sigg=-zgrid(i,j,nbl-1)/hbl
        dnorm=stable*sigg+(1.-stable)*min(sigg,epsilon)
c
        call wscale(i,j,hbl,dnorm,bfsfc,wm,ws,1)
c
        sigg=-zgrid(i,j,nbl-1)/hbl
        aa1=sigg-2.
        aa2=3.-2.*sigg
        aa3=sigg-1.
        gm=aa1+aa2*gat1(1)+aa3*dat1(1)
        gs=aa1+aa2*gat1(2)+aa3*dat1(2)
        gt=aa1+aa2*gat1(3)+aa3*dat1(3)
        dkm1(1)=hbl*wm*sigg*(1.+sigg*gm)
        dkm1(2)=hbl*ws*sigg*(1.+sigg*gs)
        dkm1(3)=hbl*ws*sigg*(1 +sigg*gt)
c
c --- now enhance diffusivity at interface nbl
c
c --- this procedure was altered for hycom to reduce diffusivity enhancement
c --- if the interface in question is located more than dp0enh below hbl.
c --- this prevents enhanced boundary layer mixing from penetrating too far
c --- below hbl when hbl is located in a very thick layer
        k=nbl-1
        ka=k+1
        delta=(hbl+zgrid(i,j,k))/(zgrid(i,j,k)-zgrid(i,j,ka))
c
        dkmp2=case*vcty(i,j,ka)+(1.-case)*blmc(ka,1)
        dstar=(1.-delta)**2*dkm1(1)+delta**2*dkmp2      
        blmc(ka,1)=(1.-delta)*vcty(i,j,ka)+delta*dstar
c
        dkmp2=case*difs(i,j,ka)+(1.-case)*blmc(ka,2)
        dstar=(1.-delta)**2*dkm1(2)+delta**2*dkmp2    
        blmc(ka,2)=(1.-delta)*difs(i,j,ka)+delta*dstar
c
        dkmp2=case*dift(i,j,ka)+(1.-case)*blmc(ka,3)
        dstar=(1.-delta)**2*dkm1(3)+delta**2*dkmp2     
        blmc(ka,3)=(1.-delta)*dift(i,j,ka)+delta*dstar
c
        if (case.eq.1.) then
          q=1.-case*max(0.,min(1.,(p(i,j,ka)-hbl*onem-dp0enh)/dp0enh))
          blmc(ka,1)=max(vcty(i,j,ka),q*blmc(ka,1))
          blmc(ka,2)=max(difs(i,j,ka),q*blmc(ka,2))
          blmc(ka,3)=max(dift(i,j,ka),q*blmc(ka,3))
        endif
c
        if (nonloc) then
          ghats(i,j,ka)=(1.-case)*ghats(i,j,ka)
        endif
c
c --- combine interior and boundary layer coefficients and nonlocal term
        if (.not.bblkpp) then
          do k=2,nbl
            vcty(i,j,k)=max(vcty(i,j,k),min(blmc(k,1),difmax))
            difs(i,j,k)=max(difs(i,j,k),min(blmc(k,2),difmax))
            dift(i,j,k)=max(dift(i,j,k),min(blmc(k,3),difmax))
          enddo
          do k=nbl+1,klist(i,j)
            ghats(i,j,k)=0.0
          enddo
          do k=klist(i,j)+1,kk+1
            vcty(i,j,k)=dflmiw+dflbot(k)
            difs(i,j,k)=dflsiw+dflbot(k)
            dift(i,j,k)=dflsiw+dflbot(k)
            ghats(i,j,k)=0.0
          enddo
        endif !.not.bblkpp
c
cdiag   if (i.eq.itest.and.j.eq.jtest) then
cdiag     write (lp,103) (nstep,iter,i+i0,j+j0,k,
cdiag&    hwide(k),1.e4*vcty(i,j,k),1.e4*dift(i,j,k),1.e4*difs(i,j,k),
cdiag&      ghats(i,j,k),k=1,kk+1)
cdiag     call flush(lp)
cdiag   endif
c
c --- save array dpbl=onem*hbl for ice, output and diagnosis
        dpbl(i,j)=onem*hbl
c
        if (bblkpp) then
c
c ------------------------------------------------
c
c --- begin bottom boundary layer parameterization
c
c ------------------------------------------------
c
c --- this bottom boundary algorithm follows the kpp algorithm included
c --- in the rutgers roms model. it is essentially an adaptation of the
c --- algorithm used for the surface boundary layer, involving diagnosis
c --- of the bottom boundary layer thickness hbbl using a bulk
c --- richardson number
c
c --- calculate zgridb
        do k=klist(i,j)+1,1,-1
          zgridb(k)=zgrid(i,j,k)-zgrid(i,j,klist(i,j)+1)
        enddo
c
c --- calculate bottom boundary layer diffusivity profiles and match these
c --- to the existing profiles
c
c --- minimum hbbl is 1 m, maximum is distance between bottom and one meter
c --- below the base of model layer 1
c
        hbblmin=1.0
        hbblmax=zgridb(2)
*       hbblmax=min(-hbl-zgrid(i,j,klist(i,j)+1),2.0*thkbot)
c       
c --- buoyfl = buoyancy flux (m**2/sec**3) into bottom due to heating by
c ---          the penetrating shortwave radiation
c --- note: bottom density increases (column is destabilized) if buoyfl < 0
c --- buoysw = shortwave radiation buoyancy flux (m**2/sec**3) at the surface
c
c --- NOTE: the convention for the bottom bl in the roms model was to use the
c --- surface net (turbulent plus radiative) heat flux to represent buoyfl
c --- this convention is not used here - instead, bottom turbulent heat
c --- flux arises entirely due to heating of the bottom by the penetrating
c --- shortwave radiation. as a result, net heat flux (buoyfl) at the bottom
c --- is zero since upward turbulent heat flux due to bottom heating is
c --- opposed by the downward shortwave radiative heat flux (it is presently
c --- assumed that no heat is absorbed by the bottom). Moving upward from
c --- the bottom, penetrating shortwave radiation acts to stabilize the
c --- water column.
c
        if (locsig) then
          dsgdt=dsiglocdt(temp(i,j,klist(i,j),n),
     &                    saln(i,j,klist(i,j),n),
     &                  ahalf*(pij(klist(i,j)  )+
     &                         pij(klist(i,j)+1) ))
        else
          dsgdt=dsigdt(   temp(i,j,klist(i,j),n),
     &                    saln(i,j,klist(i,j),n) )
        endif
        buoysw=-g*thref*dsgdt*sswflx(i,j)*thref/spcifh
        buoyfl=0.0
c       
c --- diagnose the new boundary layer depth as the depth where a bulk
c --- richardson number exceeds ric
c
c --- initialize hbbl and nbbl to extreme values
        kdn2=1
        kdn =2
        kup =3
        rib(kdn2)=0.0
        rib(kdn) =0.0
        nbbl=2
        hbbl=hbblmax
c
c --- nearbottom reference values of model variables are handled
c --- differently from the surface layer because the surface
c --- procedure does not work properly with the highly-uneven
c --- layer thicknesses often present near the bottom.
c --- reference values are chosen as the values present at the
c --- bottom = hence, uref, vref are zero and not used while
c --- bottom buoyancy is estimated assuming a linear vertical
c --- profile across the bottom layer
c
        bref=g*thref*(0.5*(3.0*thold(klist(i,j))-
     &       thold(klist(i,j)-1))+thbase)
c       
c --- diagnose hbbl and nbbl
        do k=klist(i,j),nbbl,-1
          ritop(k)=max(zgridb(k)*(bref-g*thref*(thold(k)+thbase)),
     &                 epsil)
          dvsq(k)=uold(k)**2+vold(k)**2
c
          case=zgridb(k)
          bfbot=0.0
          stable=1.0
          dnorm =1.0
c
c --- compute turbulent velocity scales at dnorm, for
c --- hbbl = case = zgridb(k)
          call wscale(i,j,case,dnorm,bfbot,wm,ws,2)
c
c --- compute the turbulent shear contribution to rib
          if     (max(dbloc(k),dbloc(k+1)).gt.0.0) then
            bfq=0.5*(dbloc(k  )/(zgrid(i,j,k-1)-zgrid(i,j,k  ))+
     &               dbloc(k+1)/(zgrid(i,j,k  )-zgrid(i,j,k+1)) )
            if     (bfq.gt.0.0) then
              bfq=sqrt(bfq)
            else
              bfq=0.0  !neutral or unstable
            endif
          else
            bfq=0.0  !neutral or unstable
          endif
          if     (bfq.gt.0.0) then
            if     (cv.ne.0.0) then
              cvk=cv
            else !frequency dependent version
              cvk=max(cv_max-cv_bfq*bfq,cv_min) !between cv_min and cv_max
            endif
            vtsq=zgridb(k)*ws*bfq*vtc*cvk
          else
            vtsq=0.0
          endif !bfq>0:else
c
c --- compute bulk richardson number at new level
c --- interpolate to find hbbl as the depth where rib = ricrb
c --- in stable or neutral conditions, hbbl can be no thicker than the
c --- bottom ekman layer
c --- ustarb is estimated in momtum.f
c
          rib(kup)=ritop(k)/(dvsq(k)+vtsq+epsil)
          if (rib(kup).ge.ricrb) then
            hekmanb=ustarb(i,j)*(cekman*4.0)/max( cormn4,
     &              abs(corio(i,j  ))+abs(corio(i+1,j  ))+
     &              abs(corio(i,j+1))+abs(corio(i+1,j+1)))
            if     (hblflg.eq.0) then                         !nearest intf.
              hbbl = zgridb(k+1)-0.5*hwide(k+1)
            elseif (k.gt.klist(i,j)-2 .or. hblflg.eq.1) then  !linear
              hbbl = zgridb(k+1)-
     &                 (zgridb(k+1)-zgridb(k))*
     &                 (ricrb-rib(kdn))/(rib(kup)-rib(kdn)+epsil)
            else                                              !quadratic
c
c ---         Determine the coefficients A,B,C of the polynomial
c ---           Y(X) = A * (X-X2)**2 + B * (X-X2) + C
c ---         which goes through the data: (X[012],Y[012])
c
              x0 = -zgridb(k+2)
              x1 = -zgridb(k+1)
              x2 = -zgridb(k)
              y0 = rib(kdn2)
              y1 = rib(kdn)
              y2 = rib(kup)
              ahbl = ( (y0-y2)*(x1-x2) -
     &                 (y1-y2)*(x0-x2)  )/
     &               ( (x0-x2)*(x1-x2)*(x0-x1) )
              bhbl = ( (y1-y2)*(x0-x2)**2 -
     &                 (y0-y2)*(x1-x2)**2  ) /
     &               ( (x0-x2)*(x1-x2)*(x0-x1) )
              if     (abs(bhbl).gt.epsil) then
                lhbl = abs(ahbl)/abs(bhbl).gt.epsil
              else
                lhbl = .true.
              endif
              if     (lhbl) then !quadratic
c ---           find root of Y(X)-RICR nearest to X2
                chbl = y2 - ricrb
                dhbl = bhbl**2 - 4.0*ahbl*chbl
                if     (dhbl.lt.0.0) then !linear
                  hbbl = -(x2 + (x1-x2)*(y2-ricrb)/(y2-y1+epsil))
                else
                  dhbl = sqrt(dhbl)
                  if     (abs(bhbl+dhbl).ge.
     &                    abs(bhbl-dhbl)    ) then
                    hbbl = -(x2 - 2.0*chbl/(bhbl+dhbl))
                  else
                    hbbl = -(x2 - 2.0*chbl/(bhbl-dhbl))
                  endif !nearest root
                endif !bhbl**2-4.0*ahbl*chbl.lt.0.0:else
              else !linear
                hbbl = -(x2 + (x1-x2)*(y2-ricrb)/(y2-y1+epsil))
              endif !quadratic:linear
            endif
            hbbl=max(hbblmin,min(hekmanb,hbblmax,hbbl))
            exit !k-loop
          endif
c
          ksave=kdn2
          kdn2=kdn
          kdn =kup
          kup =ksave
        enddo  !k=klist(i,j),nbbl,-1
c
c--- find new nbbl
        nbbl=2
        do k=klist(i,j),2,-1
          if (zgridb(k).gt.hbbl) then
            nbbl=k
            exit
          endif
        enddo  !k=klist(i,j),2,-1
c
c --- do not execute the remaining bottom boundary layer algorithm if
c --- vertical resolution is not available in the boundary layer
c
*       if (hbbl.lt.0.5*hwide(klist(i,j))) then
        if (hbbl.lt.0.5*    hwide(klist(i,j))+
     &              0.5*min(hwide(klist(i,j)  ),
     &                      hwide(klist(i,j)-1) )) then
          go to 201  !one grid point in bbl and probably not a "plume"
        endif
c
c --- calculate swfrml, the fraction of solar radiation absorbed by depth hbbl
        call swfrml_ij(chl,hbbl*onem,pij(kdm+1),qonem,jerlov(i,j),
     &                 swfrml)
c
c --- find forcing stability and buoyancy forcing for final hbbl values
c --- determine case (for case=0., hbbl lies between -zgridb(nbbl)
c --- and the interface below. for case=1., hbbl lies between 
c --- -zgrid(nbbl+1) and the interface above)
c
c --- velocity scales at hbbl
        bfbot=buoyfl+swfrml*buoysw
        if     (bfbot.ge.0.0) then
          bfbot=bfbot+epsil  !insures bfbot never=0
          stable=1.0
          dnorm =1.0
        else
          stable=0.0
          dnorm =epsilon
        endif
        case=.5+sign(.5,zgridb(nbbl)-.5*hwide(nbbl)-hbbl)
c
        call wscale(i,j,hbbl,dnorm,bfbot,wm,ws,2)
c
c --- compute the boundary layer diffusivity profiles. first, find interior
c --- viscosities and their vertical derivatives at hbbl
        ka=nint(case)*(nbbl+1)+(1-nint(case))*nbbl
        q=(pij(klist(i,j)+1)-pij(ka)-hbbl*onem)*qdpmm(ka)
        vctyh=vcty(i,j,ka)+q*(vcty(i,j,ka+1)-vcty(i,j,ka))
        difsh=difs(i,j,ka)+q*(difs(i,j,ka+1)-difs(i,j,ka))
        difth=dift(i,j,ka)+q*(dift(i,j,ka+1)-dift(i,j,ka))
c
        q=(hbbl-zgridb(nbbl+1))/(zgridb(nbbl)-zgridb(nbbl+1))
        dvdzup=-(vcty(i,j,nbbl  )-vcty(i,j,nbbl+1))/hwide(nbbl  )
        dvdzdn=-(vcty(i,j,nbbl+1)-vcty(i,j,nbbl+2))/hwide(nbbl+1)
        viscp=.5*((1.-q)*(dvdzup+abs(dvdzup))+q*(dvdzdn+abs(dvdzdn)))
        dvdzup=-(difs(i,j,nbbl  )-difs(i,j,nbbl+1))/hwide(nbbl  )
        dvdzdn=-(difs(i,j,nbbl+1)-difs(i,j,nbbl+2))/hwide(nbbl+1)
        difsp=.5*((1.-q)*(dvdzup+abs(dvdzup))+q*(dvdzdn+abs(dvdzdn)))
        dvdzup=-(dift(i,j,nbbl  )-dift(i,j,nbbl+1))/hwide(nbbl) 
        dvdzdn=-(dift(i,j,nbbl+1)-dift(i,j,nbbl+2))/hwide(nbbl+1)
        diftp=.5*((1.-q)*(dvdzup+abs(dvdzup))+q*(dvdzdn+abs(dvdzdn)))
c
        f1=stable*c11*bfbot/(ustarb(i,j)**4+epsil) 
c
        gat1(1)=vctyh/hbbl/(wm+epsil)
        dat1(1)=min(0.,-viscp/(wm+epsil)+f1*vctyh)
c
        gat1(2)=difsh/hbbl/(ws+epsil)
        dat1(2)=min(0.,-difsp/(ws+epsil)+f1*difsh) 
c
        gat1(3)=difth/hbbl/(ws+epsil)
        dat1(3)=min(0.,-diftp/(ws+epsil)+f1*difth)
c
c --- compute turbulent velocity scales on the interfaces
        do k=klist(i,j),nbbl+1,-1
          sigg=(pij(klist(i,j)+1)-pij(k))/(hbbl*onem)
          dnorm=stable*sigg+(1.-stable)*min(sigg,epsilon)
c
          call wscale(i,j,hbbl,dnorm,bfbot,wm,ws,2)
c
c --- compute the dimensionless shape functions at the interfaces
          aa1=sigg-2.
          aa2=3.-2.*sigg
          aa3=sigg-1.
c
          gm=aa1+aa2*gat1(1)+aa3*dat1(1) 
          gs=aa1+aa2*gat1(2)+aa3*dat1(2)
          gt=aa1+aa2*gat1(3)+aa3*dat1(3)
c
c --- compute boundary layer diffusivities at the interfaces
          bblmc(k,1)=hbbl*wm*sigg*(1.+sigg*gm)
          bblmc(k,2)=hbbl*ws*sigg*(1.+sigg*gs)
          bblmc(k,3)=hbbl*ws*sigg*(1.+sigg*gt)
        enddo !k=klist,nbbl+1,-1
c
c --- if the model interface nbbl+1 is located more than the distance
c --- dp0bbl above hbbl, reduce diffusivity to prevent bottom mixing
c --- from penetrating too far into the interior
c
        k=nbbl+1
        delta=(pij(klist(i,j)+1)-pij(nbbl+1))*qonem-hbbl
        if (delta.gt.dp0bbl) then
          dstar=max(0.0,1.0+(dp0bbl-delta)/dp0bbl)
          bblmc(k,1)=bblmc(k,1)*dstar
          bblmc(k,2)=bblmc(k,2)*dstar
          bblmc(k,3)=bblmc(k,3)*dstar
        endif
c
 201    continue  ! skip bbl algorithm due to poor vertical resolution
c
c --- save array dpbbl=onem*hbbl for output and diagnosis, and for momtum.f
        dpbbl(i,j)=onem*hbbl
c
c --- select maximum viscosity/diffusivity at all interfaces
        do k=2,klist(i,j)
          if (k.le.klist(i,j)) then
            vcty(i,j,k)=min(difmax,
     &                      max(vcty(i,j,k),blmc(k,1),bblmc(k,1)))
            difs(i,j,k)=min(difmax,
     &                      max(difs(i,j,k),blmc(k,2),bblmc(k,2)))
            dift(i,j,k)=min(difmax,
     &                      max(dift(i,j,k),blmc(k,3),bblmc(k,3)))
          else
            vcty(i,j,k)=dflmiw+dflbot(k)
            difs(i,j,k)=dflsiw+dflbot(k)
            dift(i,j,k)=dflsiw+dflbot(k)
          endif
          if (k.ge.nbl+1) then
            ghats(i,j,k)=0.0
          endif
        enddo
c
cdiag   if (i.eq.itest.and.j.eq.jtest) then
cdiag     write (lp,103) (nstep,iter,i+i0,j+j0,k,
cdiag&    hwide(k),1.e4*vcty(i,j,k),1.e4*dift(i,j,k),1.e4*difs(i,j,k),
cdiag&    ghats(i,j,k),k=kk,1,-1)
cdiag     call flush(lp)
cdiag   endif
cdiag   if(i.eq.itest.and.j.eq.jtest.and.mod(nstep,20).eq.0) then
cdiag     print *,'nbbl,hbbl',nbbl,hbbl
cdiag   endif
c
        endif    ! bblkpp
c
        if (iter.lt.niter) then
c
c --- perform the vertical mixing at p points
c
          do k=1,klist(i,j)
            difft(k+1)=dift(i,j,k+1)
            diffs(k+1)=difs(i,j,k+1)
            diffm(k+1)=vcty(i,j,k+1)
            ghat(k+1)=ghats(i,j,k+1)
            t1do(k)=temp(i,j,k,n)
            s1do(k)=saln(i,j,k,n)
            u1do(k)=.5*(u(i,j,k,n)+u(i+1,j  ,k,n))
            v1do(k)=.5*(v(i,j,k,n)+v(i  ,j+1,k,n))
            hm(k)=hwide(k)
            zm(k)=zgrid(i,j,k)
          enddo
c
          nlayer=klist(i,j)
          k=nlayer+1
          ka=min(k,kk)
          difft(k)=0.0
          diffs(k)=0.0
          diffm(k)=0.0
          ghat(k)=0.0
          t1do(k)=temp(i,j,ka,n)
          s1do(k)=saln(i,j,ka,n)
          u1do(k)=u1do(k-1)
          v1do(k)=v1do(k-1)
          zm(k)=zgrid(i,j,k)
c
c --- compute factors for coefficients of tridiagonal matrix elements.
c         tri(k=1:NZ,0) : dt/hwide(k)/ dzb(k-1)=z(k-1)-z(k)=dzabove)
c         tri(k=1:NZ,1) : dt/hwide(k)/(dzb(k  )=z(k)-z(k+1)=dzbelow)
c
          do k=1,nlayer
            dzb(k)=zm(k)-zm(k+1)
          enddo
c
          tri(1,1)=delt1/(hm(1)*dzb(1))
          tri(1,0)=0.
          do k=2,nlayer
            tri(k,1)=delt1/(hm(k)*dzb(k))
            tri(k,0)=delt1/(hm(k)*dzb(k-1))
          enddo
c
c --- solve the diffusion equation
c --- salflx, sswflx and surflx are positive into the ocean
c
c --- t solution
          ghatflux=-(surflx(i,j)-sswflx(i,j))*thref/spcifh
          call tridcof(difft,tri,nlayer,tcu,tcc,tcl)
          call tridrhs(hm,t1do,difft,ghat,ghatflux,tri,nlayer,rhs)
          call tridmat(tcu,tcc,tcl,nlayer,hm,rhs,t1do,t1dn,difft, i,j)
c
c --- s solution
          ghatflux=-salflx(i,j)*thref
          call tridcof(diffs,tri,nlayer,tcu,tcc,tcl)
          call tridrhs(hm,s1do,diffs,ghat,ghatflux,tri,nlayer,rhs)
          call tridmat(tcu,tcc,tcl,nlayer,hm,rhs,s1do,s1dn,diffs, i,j)
c
cdiag     if (i.eq.itest.and.j.eq.jtest) then
cdiag       write (lp,104) (nstep,iter,i+i0,j+j0,k,
cdiag&        hm(k),t1do(k),t1dn(k),s1do(k),s1dn(k),
cdiag&        0.0,0.0,
cdiag&        k=1,nlayer)
cdiag       call flush(lp)
cdiag     endif
c
c --- u solution
          call tridcof(diffm,tri,nlayer,tcu,tcc,tcl)
          do k=1,nlayer
            rhs(k)=u1do(k)
          enddo
          call tridmat(tcu,tcc,tcl,nlayer,hm,rhs,u1do,u1dn,diffm, i,j)
c
c --- v solution
          do k=1,nlayer
            rhs(k)=v1do(k)
          enddo
          call tridmat(tcu,tcc,tcl,nlayer,hm,rhs,v1do,v1dn,diffm, i,j)
c
cdiag     if (i.eq.itest.and.j.eq.jtest) then
cdiag       write (lp,105) (nstep,iter,i+i0,j+j0,k,
cdiag&        hm(k),u1do(k),u1dn(k),v1do(k),v1dn(k),k=1,nlayer)
cdiag       call flush(lp)
cdiag     endif
c
c --- reset old variables in preparation for next iteration
          do k=1,nlayer+1
            told(k)=t1dn(k)
            sold(k)=s1dn(k)
            if (locsig) then
              if (k.eq.1) then
                thold(k)=sig(told(k),sold(k))-thbase
              else
                ka=k-1
                alfadt(k)=dsiglocdt(ahalf*(told(ka)+told(k)),
     &                              ahalf*(sold(ka)+sold(k)),
     &                                             p(i,j,k)  )*
     &                                    (told(ka)-told(k))
                betads(k)=dsiglocds(ahalf*(told(ka)+told(k)),
     &                              ahalf*(sold(ka)+sold(k)),
     &                                             p(i,j,k)  )*
     &                                    (sold(ka)-sold(k))
                thold(k)=thold(ka)-alfadt(k)-betads(k)
              endif
            else
              thold(k)=sig(told(k),sold(k))-thbase
            endif
            if (iter.lt.niter) then
              uold(k)=u1dn(k)
              vold(k)=v1dn(k)
            endif
          enddo
        endif                         ! iter < niter
c
      enddo                           ! iteration loop
c
 101  format(i9,2i5,i3,'swfrac,dn,dtemp,dsaln ',2f8.3,2f12.6)
 102  format(25x,'   thick      viscty    t diff    s diff  '
     &     /(i9,i2,2i5,i3,2x,4f10.2))
 103  format(25x,'   thick      viscty    t diff    s diff   nonlocal'
     &     /(i9,i2,2i5,i3,2x,4f10.2,f11.6))
 104  format(25x,
     &     '  thick   t old   t new   s old   s new trc old trc new'
     &     /(i9,i2,2i5,i3,1x,f9.2,4f8.3,2f7.4))
 105  format(25x,'   thick   u old   u new   v old   v new'
     &     /(i9,i2,2i5,i3,1x,f10.2,4f8.3))
c
      return
      end
c
      subroutine mxmyaij(m,n, i,j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
c
c --- hycom version 2.1
      implicit none
c
      integer m,n, i,j
c
c ---------------------------------------------------------------
c --- mellor-yamada 2.5 vertical diffusion, single j-row (part A)
c --- vertical coordinate is z negative below the ocean surface
c ---------------------------------------------------------------
c
c --- arrays q2 and q2l are prognostic variables representing tke and
c --- tke multiplied by the turbulent eddy length scale. these arrays
c --- are calculated on interfaces in a special vertical grid where
c --- interfaces are centered at the surface, bottom, and at mid-depths
c --- of each hycom layer (kdm+2 interfaces). this enables the q2 and
c --- q2l arrays to be advected and diffused in subroutine tsadvc.
c
      real, parameter :: difmax = 9999.0e-4  !maximum diffusion/viscosity
c
c --- local variables for my2.5 mixing
c
      real sm(kdm+2),sh(kdm+2),prod(kdm+2),stf(kdm+2)
      real dtef(kdm+2),gh(kdm+2),ee(kdm+2),gg(kdm+2),turlen(kdm+2)
      real z(kdm+2),zz(kdm+2),dz(kdm+2),dzz(kdm+2)
      real th1d(kdm+1),u1d(kdm+1),v1d(kdm+1)
      real alfadt(kdm+1),betads(kdm+1),delth(kdm+1)
c
      real dbloc(kdm+2)        ! buoyancy jump across interface
      real swfrac(kdm+1)       ! fractional surface shortwave radiation flux
      real dpmm(kdm)           !     max(onemm,dp(i,j,:,[nm]))
      real qdpmm(kdm)          ! 1.0/max(onemm,dp(i,j,:,[nm]))
      real pij(kdm+1)          ! local copy of p(i,j,:)
c
      real dflsiw              ! internal wave diffusivity
      real dflmiw              ! internal wave viscosity
      real dflbot(kdm+1)       ! bottom intensified background viscosity
c
      real dh,akn,coef1,coef2,coef3,dtemp,dsaln,sflux1,pmid,div
      real wusurf,wvsurf,wubot,wvbot,delu,delv,ubav,vbav,qspcifh,q,
     &     chl,wt,wq,wz0,wz1,wz2
c
      real buoyfl,buoyfs,buoysw,dsgdt,smn,tmn,swfrml
c
      integer k,ka,k1,khy1,khy2,kmy1,kmy2,jrlv
c
      integer iglobal,jglobal
c
      include 'stmt_fns.h'
c
      iglobal=i0+i !for debugging
      jglobal=j0+j !for debugging
      if     (iglobal+jglobal.eq.-99) then
        write(lp,*) iglobal,jglobal  !prevent optimization
      endif
c
c --- internal wave diffusion/viscosity
      dflsiw = diws(i,j)
      dflmiw = diwm(i,j)
c
c --- set mid-time pressure array
c --- locate lowest substantial mass-containing layer.
      pij(1)=p(i,j,1)
      do k=1,kk
        dpmm( k)  =max(onemm,dp(i,j,k,m))
        pij(  k+1)=pij(k)+dp(i,j,k,m)
        p(i,j,k+1)=pij(k+1)
      enddo
      do k=kk,1,-1
        if (dpmm(k).gt.tencm) then
          exit
        endif
      enddo
      klist(i,j)=max(k,2)  !always consider at least 2 layers
c
      dpbl(i,j)=min(dpbl(i,j),pij(kk+1))
c
      dh=depths(i,j)+srfhgt(i,j)/(thref*onem)  !total depth, in m
c
c --- dflbot (Prandtl number of one, i.e. diffusion = viscosity)
      if     (botdiw) then
c ---   diffusion coefficent profile is: K = Kb / (1 + h/h0)**2
c ---    where diwbot = Kb; diwqh0 = 1/h0 (input as h0, see forfun.f)
c ---     Decloedt T. and D.S. Luther, 2009: On a Simple Empirical 
c ---     Parameterization of Topography-Catalyzed Diapycnal Mixing
c ---     in the Abyssal Ocean. JPO, 40, pp 487-508.
c ---   use Simpson's rule to estimate the average K across each layer
        wz0 = 1.0 / (1.0 + (pij(kk+1)-     pij(1)  )*diwqh0(i,j))**2
        wz1 = 1.0 / (1.0 + (pij(kk+1)-0.5*(pij(1)+
     &                                     pij(2) ))*diwqh0(i,j))**2
        wz2 = 1.0 / (1.0 + (pij(kk+1)-     pij(2)  )*diwqh0(i,j))**2
        wq  = wz0 + wz2 + 4.0*wz1  !6 times the average value over layer 1
        do k= 2,kk
          wt  = wq
          wz0 = wz2
          wz1 = 1.0 / (1.0 + (pij(kk+1)-0.5*(pij(k)  +
     &                                       pij(k+1) ))*diwqh0(i,j))**2
          wz2 = 1.0 / (1.0 + (pij(kk+1)-     pij(k+1)  )*diwqh0(i,j))**2
          wq  = wz0 + wz2 + 4.0*wz1  !6 times the average value over layer k
          dflbot(k) = (0.5/6.0)*(wt+wq)*diwbot(i,j)
        enddo
        dflbot(kk+1) = dflbot(kk)
      else
        do k= 2,kk+1
          dflbot(k) = 0.0
        enddo
      endif !botdiw
c
c --- generate the scaled m-y vertical grid
c --- calculate z (interface z), dz, zz (central layer z), dzz in m
      khy1=klist(i,j)
      khy2=khy1+1
      kmy1=khy2
      kmy2=kmy1+1
c
      z(1)=0.0
      dz(1)=0.5*dpmm(1)/pij(khy2)
      z(2)=-dz(1)
      do k=3,kmy1
        dz(k-1)=0.5*(dpmm(k-2)+dpmm(k-1))/pij(khy2)
        z(k)=z(k-1)-dz(k-1)
      enddo
      dz(kmy1)=0.5*dpmm(khy1)/pij(khy2)
      z(kmy2)=z(kmy1)-dz(kmy1)
      dz(kmy2)=0.0
c
      do k=1,kmy1
        zz(k)=0.5*(z(k)+z(k+1))
      enddo
      zz(kmy2)=2.0*z(kmy2)-zz(kmy1)
c
      do k=1,kmy1
        dzz(k)=zz(k)-zz(k+1)
      enddo
      dzz(kmy2)=0.0
c
c --- calculate alfadt, betads if locally referenced potential density
c --- is used to estimate buoyancy gradient
      if (locsig) then
        do k=2,khy1
          ka=k-1
          alfadt(k)=dsiglocdt(ahalf*(temp(i,j,ka,m)+
     &                               temp(i,j,k ,m) ),
     &                        ahalf*(saln(i,j,ka,m)+
     &                               saln(i,j,k ,m) ),
     &                                  p(i,j,k)      )*
     &               (temp(i,j,ka,m)-temp(i,j,k, m))
          betads(k)=dsiglocds(ahalf*(temp(i,j,ka,m)+
     &                               temp(i,j,k ,m) ),
     &                        ahalf*(saln(i,j,ka,m)+
     &                               saln(i,j,k ,m) ),
     &                                  p(i,j,k)      )*
     &               (saln(i,j,ka,m)-saln(i,j,k, m))
          if (k.eq.2) then
            alfadt(1)=alfadt(2)
            betads(1)=betads(2)
          endif
          delth(k)=0.5*(alfadt(ka)+alfadt(k)+betads(ka)+betads(k))
          if (p(i,j,k-1).gt.dpbl(i,j)) then
            delth(k)=min(0.0,delth(k))
          endif
          if (k.eq.khy1) then
            delth(k  )=min(0.0,delth(k))
            delth(k+1)=        delth(k)
          endif
        enddo
      endif
c
c --- calculate 1-d arrays on m-y vertical grid
      if (.not.locsig) then
        th1d(1)=th3d(i,j,1,m)
      endif
      ubav=0.5*(ubavg(i,j,m)+ubavg(i+1,j  ,m))
      vbav=0.5*(vbavg(i,j,m)+vbavg(i  ,j+1,m))
      u1d(1)=0.5*(u(i,j,1,m)+u(i+1,j  ,1,m))+ubav
      v1d(1)=0.5*(v(i,j,1,m)+v(i  ,j+1,1,m))+vbav
      do k=2,khy1
        if (.not.locsig) then
          th1d(k)=0.5*(th3d(i,j,k-1,m)+th3d(i,j,k,m))
        endif
        u1d(k)=0.25*(u(i  ,j  ,k-1,m)+u(i  ,j  ,k,m)
     &              +u(i+1,j  ,k-1,m)+u(i+1,j  ,k,m))+ubav
        v1d(k)=0.25*(v(i  ,j  ,k-1,m)+v(i  ,j  ,k,m)
     &              +v(i  ,j+1,k-1,m)+v(i  ,j+1,k,m))+vbav
      enddo
      if (.not.locsig) then
        th1d(kmy1)=th3d(i,j,khy1,m)
      endif
      u1d(kmy1)=0.5*(u(i,j,khy1,m)+u(i+1,j  ,khy1,m))+ubav
      v1d(kmy1)=0.5*(v(i,j,khy1,m)+v(i  ,j+1,khy1,m))+vbav
c
c --- make sure background tke maintains minimum value
      do k=0,kk+1
        q2( i,j,k,m)=max(smll,q2( i,j,k,m))
        q2l(i,j,k,m)=max(smll,q2l(i,j,k,m))
        q2( i,j,k,n)=max(smll,q2( i,j,k,n))
        q2l(i,j,k,n)=max(smll,q2l(i,j,k,n))
      enddo
c
c --- calculate sm,sh coefficients
      do k=2,kmy1
        sm(k)=-delt1*0.5*(difqmy(i,j,k-1)+difqmy(i,j,k  )+2.0*dflsiw)
     &       /(dzz(k-1)*dz(k  )*dh*dh)
        sh(k)=-delt1*0.5*(difqmy(i,j,k-2)+difqmy(i,j,k-1)+2.0*dflsiw)
     &       /(dzz(k-1)*dz(k-1)*dh*dh)
      enddo
c
c ------------------------------------------------------------------
c --- solve delt1*(difq*q2(n)')' - q2(n)*(2.*delt1*dtef+1.) = -q2(m)
c --- for q2(n) (tke)
c ------------------------------------------------------------------
c
c --- surface and bottom stress boundary conditions
      if (windf) then
        wusurf=thref*surtx(i,j)
        wvsurf=thref*surty(i,j)
      else
        wusurf=0.0
        wvsurf=0.0
      endif
      wubot=-0.5*thkbot*onem*u1d(kmy1)*drag(i,j)*thref/g
      wvbot=-0.5*thkbot*onem*v1d(kmy1)*drag(i,j)*thref/g
      ee(1)=0.0
      gg(1)=const1*sqrt(wusurf*wusurf+wvsurf*wvsurf)
      q2(i,j,kmy1,n)=const1*sqrt(wubot*wubot+wvbot*wvbot)
c
c --- calculate vertical buoyancy gradient at interfaces
      dbloc(1)=0.0
      do k=2,kmy1
        ka=k-1
        if (locsig) then
          dbloc(k)=thref*g*delth(k)/(dzz(ka)*dh)
        else
          if (p(i,j,k-1).gt.dpbl(i,j)) then
            dbloc(k)=thref*g*min(0.0,th1d(ka)-th1d(k))/(dzz(ka)*dh)
          else
            dbloc(k)=thref*g*   (    th1d(ka)-th1d(k))/(dzz(ka)*dh)
          endif
        endif
      enddo
      dbloc(kmy2)=0.0
c
c --- calculate turbulent length scale and richardson number gh at interfaces
c
      turlen(1)=0.0
      gh(1)=0.
      do k=2,kmy1
        turlen(k)=q2l(i,j,k-1,m)/q2(i,j,k-1,m)
        gh(k)=min(0.028,turlen(k)**2/q2(i,j,k-1,m)*dbloc(k))
      enddo
      turlen(kmy2)=0.   
      gh(kmy2)=0.
c
c --- calculate tke production at interfaces
      prod(1)=0.0
      do k=2,kmy1
        delu=u1d(k)-u1d(k-1)
        delv=v1d(k)-v1d(k-1)
        prod(k)=diftmy(i,j,k-1)*dbloc(k)+vctymy(i,j,k-1)*sef
     &         *(delu**2+delv**2)/(dzz(k-1)*dh)**2
      enddo
      prod(kmy2)=0.0
c
c --- solve the equation
      do k=1,kmy2
        stf(k)=1.0
        if (gh(k).lt.0. ) stf(k)=1.0-0.9*(gh(k)/ghc)**1.5
        if (gh(k).lt.ghc) stf(k)=0.1
        dtef(k)=q2(i,j,k-1,m)**1.5/(b1my*q2l(i,j,k-1,m)+smll)*stf(k)
      enddo
      do k=2,kmy1
        gg(k)=1.0/(sm(k)+sh(k)*(1.0-ee(k-1))-(2.0*delt1*dtef(k)+1.0))
        ee(k)=sm(k)*gg(k)
        gg(k)=(-2.0*delt1*prod(k)+sh(k)*gg(k-1)-q2(i,j,k-1,n))*gg(k)
      enddo
      do k=kmy1,1,-1
        q2(i,j,k-1,n)=ee(k)*q2(i,j,k,n)+gg(k)
      enddo
c
c ------------------------------------------------------------------
c --- solve delt1*(difq*q2l(n)')' - q2l(n)*(delt1*dtef+1.) = -q2l(m)
c --- for q2l(n) (tke times turbulent length scale)
c ------------------------------------------------------------------
c
            !min(1,kkmy25+1) always 1 if this routine is called
      q2(i,j,min(1,kkmy25+1),n)=max(smll,q2(i,j,min(1,kkmy25+1),n))
      ee(2)=0.0
      gg(2)=-vonk*z(2)*dh*q2(i,j,min(1,kkmy25+1),n)
      q2l(i,j,kmy1,n)=0.0
      do k=3,kmy1
        dtef(k)=dtef(k)*(1.+e2my*((1.0/abs(z(k)-z(1))+
     &         1.0/abs(z(k)-z(kmy2)))*turlen(k)/(dh*vonk))**2)
        gg(k)=1.0/(sm(k)+sh(k)*(1.0-ee(k-1))-(delt1*dtef(k)+1.0))
        ee(k)=sm(k)*gg(k)
        gg(k)=(delt1*(-prod(k)*turlen(k)*e1my)+sh(k)*gg(k-1)
     &       -q2l(i,j,k-1,n))*gg(k)
      enddo
      do k=kmy1,2,-1
        q2l(i,j,k-1,n)=ee(k)*q2l(i,j,k,n)+gg(k)
      enddo
      do k=0,kmy1
        if (q2(i,j,k,n).lt.smll .or. q2l(i,j,k,n).lt.smll) then
          q2 (i,j,k,n)=smll
          q2l(i,j,k,n)=smll
        endif
      enddo
c
c ----------------------------------------------------
c --- calculate the viscosity and diffusivity profiles
c ----------------------------------------------------
c
c --- note that sm and sh limit to infinity when gh approaches 0.0288
      do k=1,kmy2
        coef1=a2my*(1.0-6.0*a1my/b1my*stf(k))
        coef2=3.0*a2my*b2my/stf(k)+18.0*a1my*a2my
        coef3=a1my*(1.0-3.0*c1my-6.0*a1my/b1my*stf(k))
        sh(k)=coef1/(1.0-coef2*gh(k))
        sm(k)=coef3+sh(k)*coef4*gh(k)
        sm(k)=sm(k)/(1.0-coef5*gh(k))
        akn=turlen(k)*sqrt(abs(q2(i,j,k-1,n)))
        difqmy(i,j,k-1)=max((akn*0.41*sh(k)+difqmy(i,j,k-1))*0.5,
     &                      dflsiw+dflbot(k))
        vctymy(i,j,k-1)=max((akn*     sm(k)+vctymy(i,j,k-1))*0.5,
     &                      dflmiw+dflbot(k))
        diftmy(i,j,k-1)=max((akn*     sh(k)+diftmy(i,j,k-1))*0.5,
     &                      dflsiw+dflbot(k))
      enddo
c
c --- set diffusivity/viscosty on hycom interfaces
      vcty(i,j,1)=0.0
      dift(i,j,1)=0.0
      difs(i,j,1)=0.0
      do k=2,khy1
        vcty(i,j,k)=min(0.5*(vctymy(i,j,k-1)+vctymy(i,j,k)),difmax)
        dift(i,j,k)=min(0.5*(diftmy(i,j,k-1)+diftmy(i,j,k)),difmax)
        difs(i,j,k)=dift(i,j,k)
      enddo
c
      tmn=.5*(temp(i,j,1,m)+temp(i,j,1,n))
      smn=.5*(saln(i,j,1,m)+saln(i,j,1,n))
c
c --- set new time pressure array
c --- locate lowest substantial mass-containing layer.
      pij(1)=p(i,j,1)
      do k=1,kk
        dpmm( k)  =max(onemm,dp(i,j,k,n))
        qdpmm(k)  =1.0/dpmm(k)
        pij(  k+1)=pij(k)+dp(i,j,k,n)
        p(i,j,k+1)=pij(k+1)
      enddo
      do k=kk,1,-1
        if (dpmm(k).gt.tencm) then
          exit
        endif
      enddo
      klist(i,j)=min(klist(i,j),   !minimum of m and n levels
     &               max(k,2)   )  !always consider at least 2 layers
c
      khy1=klist(i,j)
      khy2=klist(i,j)+1
c
      do k=khy2,kk+1
        vcty(i,j,k)=dflmiw+dflbot(k)
        difs(i,j,k)=dflsiw+dflbot(k)
        dift(i,j,k)=dflsiw+dflbot(k)
      enddo
c
cdiag if (i.eq.itest.and.j.eq.jtest) then
cdiag    write (lp,101) (nstep,i+i0,j+j0,k,pij(k)*qonem,
cdiag&   1.e4*vcty(i,j,k),1.e4*dift(i,j,k),1.e4*difs(i,j,k),
cdiag&   1.e4*difqmy(i,j,k-1),k=1,khy2)
cdiag    call flush(lp)
cdiag endif
c
c --- calculate zgrid
      zgrid(i,j,1)=-0.5*dpmm(1)*qonem
      do k=2,khy1
        zgrid(i,j,k)=zgrid(i,j,k-1)-0.5*(dpmm(k-1)+dpmm(k))*qonem
      enddo
      zgrid(i,j,khy2)=-pij(khy2)*qonem
c
c --- forcing of t,s by surface fluxes. flux positive into ocean.
c
      qspcifh=1.0/spcifh
c
      if (thermo .or. sstflg.gt.0 .or. srelax) then
c ---   shortwave flux penetration depends on kpar or chl or jerlov water type.
        if     (jerlv0.le.0) then
          chl =  akpar(i,j,lk0)*wk0+akpar(i,j,lk1)*wk1
     &          +akpar(i,j,lk2)*wk2+akpar(i,j,lk3)*wk3
        endif
        call swfrac_ij(chl,pij,klist(i,j)+1,qonem,jerlov(i,j),swfrac)
      else
        swfrac(1)  = 1.0
        swfrac(2:) = 0.0
      endif
c
      do k=1,khy1
        if (thermo .or. sstflg.gt.0 .or. srelax) then
          if (k.eq.1) then
            sflux1=surflx(i,j)-sswflx(i,j)
            dtemp=(sflux1+(1.-swfrac(k+1))*sswflx(i,j))*
     &            delt1*g*qspcifh*qdpmm(k)
            dsaln=salflx(i,j)*
     &            delt1*g*        qdpmm(k)
cdiag       if (i.eq.itest .and. j.eq.jtest) then
cdiag         write (lp,102) nstep,i+i0,j+j0,k, 
cdiag&          0.,1.-swfrac(k+1),dtemp,dsaln
cdiag         call flush(lp)
cdiag       endif
          else
            dtemp=(swfrac(k)-swfrac(k+1))*sswflx(i,j)*
     &            delt1*g*qspcifh*qdpmm(k)
            dsaln=0.
cdiag       if (i.eq.itest .and. j.eq.jtest) then
cdiag          write (lp,102) nstep,i+i0,j+j0,k,
cdiag&         1.-swfrac(k),1.-swfrac(k+1),dtemp
cdiag         call flush(lp)
cdiag       endif
          endif
        else !.not.thermo ...
          dtemp=0.0
          dsaln=0.0
        endif !thermo.or.sstflg.gt.0.or.srelax:else
        temp(i,j,k,n)=    temp(i,j,k,n)+dtemp
        saln(i,j,k,n)=max(saln(i,j,k,n)+dsaln,0.0)  !must be non-negative
        th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
      enddo !k
c
c --- calculate swfrml, the fraction of solar radiation left at depth dpbl(i,j)
      call swfrml_ij(chl,dpbl(i,j),pij(kdm+1),qonem,jerlov(i,j),swfrml)
c
c --- buoyfl = total buoyancy flux (m**2/sec**3) into atmos.
c --- buoysw = shortwave radiation buoyancy flux (m**2/sec**3) into atmos.
      dsgdt=          dsigdt(tmn,smn)
      buoyfs=g*thref*(dsigds(tmn,smn)*salflx(i,j)*thref)
      buoyfl=buoyfs+
     &       g*thref*(dsgdt          *surflx(i,j)*thref/spcifh)
      buoysw=g*thref*(dsgdt          *sswflx(i,j)*thref/spcifh)
      buoflx(i,j)=buoyfl     -swfrml*buoysw      !mixed layer buoyancy
      mixflx(i,j)=surflx(i,j)-swfrml*sswflx(i,j) !mixed layer heat flux
      bhtflx(i,j)=buoflx(i,j)-buoyfs             !buoyancy from heat flux
c
 101  format(25x,'   thick      viscty    t diff    s diff    q diff  '
     &     /(i9,2i5,i3,2x,5f10.2))
 102  format(i9,2i5,i3,'absorbup,dn,dtemp,dsaln ',2f6.3,2f10.6)
c
      return   
      end
c
      subroutine mxgissaij(m,n, i,j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
c
c --- hycom version 2.1
      implicit none
c
      integer m,n, i,j
c--------------------------------------------------------------------
c --- nasa giss vertical mixing model, single j-row (part A)
c
c --- V.M. Canuto, A. Howard, Y. Cheng, and M.S. Dubovikov, 2001:
c --- Ocean turbulence, part I: One-point closure model -- momentum
c --- and heat vertical diffusivities.  JPO, 31, 1413-1426.
c --- V.M. Canuto, A. Howard, Y. Cheng, and M.S. Dubovikov, 2002:
c --- Ocean turbulence, part II: Vertical diffusivities of momentum,
c --- heat, salt, and passive tracers.  JPO, 32, 240-264.
c
c --- Modified by Armando Howard to implement the latitude dependent
c --- background mixing due to waves formula from:
c --- Gregg et. al. (2003): Reduced mixing from the breaking of
c --- internal waves in equatorial waters, Nature 422 pp 513-515.
c--------------------------------------------------------------------
c
c     1D turbulence calculation routine adapted by A.Romanou from A.Howard
c     from the 2000 original model.
c
c     For a discussion of the turbulence model used here see "Ocean
c     Turbulence. Part II" referenced above.
c
c     In general ria(k),rid(k) are calculated from the difference between
c     level k and k+1 and ak{m,h,s}(k) should be used to mix levels k and k+1.
c     n is (nlayers-1) because there are nlayers-1 ocean interfaces to
c     be mixed.
c
c     In the mixed layer the model diffusivity for each field is a product of 
c     a dimensionless function of the two variables ria and rid 
c     and the Shear and the square of a lengthscale.
c     The lengthscale is proportional to depth near the surface but asymptotes
c     towards a fixed fraction of the Mixed Layer Depth deeper in the mixed
c     layer. The MLD is thus a necessary ingredient for calculating model
c     diffusivities.
c
c     latitude dependent deep background mixing ("botdiw") option:
c       background mixing depends on |f| and N,
c       where f is the coriolis parameter and N brunt vaisala frequency.
c       note Gregg et al. use "f" when they mean the absolute value of
c       the coriolis parameter.
c       additional factor multiplying the `epsilon/N^2' for deep mixing
c       based on the formula cited as from Henyey et. al,
c       JGR vol.91 8487-8495,1986) in Gregg et al. where it is shown
c       confirmed by observations for lower latitudes except for being
c       low very near the equator. 
c       I place a minimum, "eplatidepmin", on the Gregg et al. factor, "L".
c       Note that Gregg et. al.'s formula:
c         L(\theta,N) = 
c         (|f| cosh^{-1} (N/|f|))/(f_30^o cosh^{-1} (N_0/f_30^o)
c       is only defined as a real number when N > |f|, since arccosh
c       can only be defined as a real for arguments of at least 1.
c       At 1 arccosh is zero.
c       I decide to set "L(\theta,N)" to "eplatidepmin"FOR (N/f < 1).
c       This corresponds to setting a floor of 1 on (N/f).
c       for foreground mixing at depth detached from the mixed-layer
c       I revert to the "deep" lengthscale, which uses density gradients,
c       in case (N/f)<1 to try not to make deep arctic&subarctic mixing 
c       too small.
c     note: botdiw is defined differently than for KPP and Mellor-Yamada.
c
c-----------------------------------------------------------------------------
c --- this is a level 2 turbulence model
c --- sm and sh depends only on the richardson number
c --- In salinity model case level 2 means S_M,S_H,S_C depend only on Ria,Ri_d
c-----------------------------------------------------------------------------
c                                                                             
      real, parameter :: difmax = 9999.0e-4  !maximum diffusion/viscosity     
      real, parameter :: acormin= 2.5453e-6  !minimum abs(corio), i.e. 1 degN     
c
c --- local variables for giss mixing
c
      real z1d(kdm),th1d(kdm),u1d(kdm),v1d(kdm)
      real ria(kdm),rid(kdm),s2(kdm),v_back(kdm),
     &     t_back(kdm),s_back(kdm),dtemp,dsaln,sflux1,
     &     alfadt,betads,al0,ri1,rid1,slq2,sm,sh,ss,akz,al,al2,anlq2,
     &     back_ri1,back_rid1,back_ra_r1,back_rit1,back_ric1,rit,ric,
     &     ra_r,theta_r,theta_r0,theta_r1,theta_r_deg,deltheta_r1,
     &     delback_ra_r,dback_ra_r_o_dtheta,slq2_back,sm_back,sh_back,
     &     ss_back,delsm_back,dsm_back_o_dtheta,delsh_back,
     &     dsh_back_o_dtheta,delss_back,dss_back_o_dtheta,delslq2_back,
     &     dslq2_back_o_dtheta,s2_back,al0_back,al_back,
     &     al2_back,anlq2_back,tmp_back,tmp,delz,delth,del2th,
     &     dzth,d2zth,rdzlndzth,al0deep,thsum,dens,
     &     chl,qspcifh,hbl,
     &     epson2,epson2_bot,eplatidep,gatmbs,gatpbs
      real akm(kdm),akh(kdm),aks(kdm),aldeep(kdm),tmpk(kdm)
      real an2(kdm),an,acorio,zbot
c
      real swfrac(kdm+1)       ! fractional surface shortwave radiation flux
      real swfrml              ! fractional surface sw rad flux at ml base
      real hwide(kdm)          ! layer thicknesses in m (minimum 1mm)
      real dpmm(kdm)           !     max(onemm,dp(i,j,:,n))
      real qdpmm(kdm)          ! 1.0/max(onemm,dp(i,j,:,n))
      real pij(kdm+1)          ! local copy of p(i,j,:)
c
      real buoyfl,buoyfs,buoysw,dsgdt,smn,tmn
c
      integer ifbelow,ifrafglt,jtheta_r
      integer ifnofsmall
c
      integer            jtheta_r0,jtheta_r1,itheta_r0,itheta_r1
      common/mxgissij_b/ jtheta_r0,jtheta_r1,itheta_r0,itheta_r1
      save  /mxgissij_b/ !bugfix, reduces optimization of *theta_r*
c
      integer k,k1,jrlv
c
      integer iglobal,jglobal
c
      real    acosh1,xx
      include 'stmt_fns.h'
      acosh1(xx) = log(xx+sqrt((xx**2)-1.0))
c
      iglobal=i0+i !for debugging
      jglobal=j0+j !for debugging
      if     (iglobal+jglobal.eq.-99) then
        write(lp,*) iglobal,jglobal  !prevent optimization
      endif
c
c --- set mid-time pressure array
c --- locate lowest substantial mass-containing layer.
      dpmm( 1)=max(onemm,dp(i,j,1,n))
      qdpmm(1)=1.0/dpmm(1)
      pij(  1)=p(i,j,1)
      pij(  2)=pij(1)+dp(i,j,1,n)
      p(i,j,2)=pij(2)
      do k=2,kk
        dpmm( k)  =max(onemm,dp(i,j,k,n))
        qdpmm(k)  =1.0/dpmm(k)
        pij(  k+1)=pij(k)+dp(i,j,k,n)
        p(i,j,k+1)=pij(k+1)
      enddo
      do k=kk,1,-1
        if (dpmm(k).gt.tencm) then
          exit
        endif
      enddo
      klist(i,j)=max(k,2)  !always consider at least 2 layers
c
c --- nominal surface bld is the mld, note that this depends on tmljmp
      dpbl(i,j)=min(dpbl(i,j),pij(kk+1))
      hbl=dpbl(i,j)
c
c --- forcing of t,s by surface fluxes. flux positive into ocean.
c
      qspcifh=1.0/spcifh
c
      if (thermo .or. sstflg.gt.0 .or. srelax) then
c ---   shortwave flux penetration depends on kpar or chl or jerlov water type.
        if     (jerlv0.le.0) then
          chl =  akpar(i,j,lk0)*wk0+akpar(i,j,lk1)*wk1
     &          +akpar(i,j,lk2)*wk2+akpar(i,j,lk3)*wk3
        endif
        call swfrac_ij(chl,pij,klist(i,j)+1,qonem,jerlov(i,j),swfrac)
      else
        swfrac(1)  = 1.0
        swfrac(2:) = 0.0
      endif
c
      tmn=.5*(temp(i,j,1,m)+temp(i,j,1,n))
      smn=.5*(saln(i,j,1,m)+saln(i,j,1,n))
c
      do k=1,kk
        k1=k+1
c
        if (thermo .or. sstflg.gt.0 .or. srelax) then
          if (k.eq.1) then
            sflux1=surflx(i,j)-sswflx(i,j)
            dtemp=(sflux1+(1.-swfrac(k+1))*sswflx(i,j))*
     &            delt1*g*qspcifh*qdpmm(k)
            dsaln=salflx(i,j)*
     &            delt1*g*        qdpmm(k)
cdiag       if (i.eq.itest.and.j.eq.jtest) then
cdiag         write (lp,101) nstep,i+i0,j+j0,k,
cdiag&          0.,1.-swfrac(k+1),dtemp,dsaln
cdiag         call flush(lp)
cdiag       endif
          elseif (k.le.klist(i,j)) then
            dtemp=(swfrac(k)-swfrac(k+1))*sswflx(i,j)*
     &            delt1*g*qspcifh*qdpmm(k)
            dsaln=0.0
cdiag       if (i.eq.itest.and.j.eq.jtest) then
cdiag         write (lp,101) nstep,i+i0,j+j0,k,
cdiag&          1.-swfrac(k),1.-swfrac(k+1),dtemp
cdiag         call flush(lp)
cdiag       endif
          else !k.gt.klist(i,j)
            dtemp=0.0
            dsaln=0.0
          endif
        else !.not.thermo ...
          dtemp=0.0
          dsaln=0.0
        endif !thermo.or.sstflg.gt.0.or.srelax:else
c
c --- modify t and s
        temp(i,j,k,n)=    temp(i,j,k,n)+dtemp
        saln(i,j,k,n)=max(saln(i,j,k,n)+dsaln,0.0)  !must be non-negative
        th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
c
      enddo !k
c
c --- calculate swfrml, the fraction of solar radiation left at depth hbl
      call swfrml_ij(chl,hbl,pij(kdm+1),qonem,jerlov(i,j),swfrml)
c
c --- buoyfl = total buoyancy flux (m**2/sec**3) into atmos.
c --- buoysw = shortwave radiation buoyancy flux (m**2/sec**3) into atmos.
      dsgdt=          dsigdt(tmn,smn)
      buoyfs=g*thref*(dsigds(tmn,smn)*salflx(i,j)*thref)
      buoyfl=buoyfs+
     &       g*thref*(dsgdt          *surflx(i,j)*thref/spcifh)
      buoysw=g*thref*(dsgdt          *sswflx(i,j)*thref/spcifh)
      buoflx(i,j)=buoyfl     -swfrml*buoysw      !mixed layer buoyancy
      mixflx(i,j)=surflx(i,j)-swfrml*sswflx(i,j) !mixed layer heat flux
      bhtflx(i,j)=buoflx(i,j)-buoyfs             !buoyancy from heat flux
c
c --- calculate z at vertical grid levels - this array is the z values in m
c --- at the mid-depth of each model layer except for index klist+1, where it
c --- is the z value of the bottom
c
c --- calculate layer thicknesses
      do k=1,kk
        if (k.eq.1) then
          hwide(k)=dpmm(k)*qonem
          zgrid(i,j,k)=-.5*hwide(k)
        else if (k.lt.klist(i,j)) then
          hwide(k)=dpmm(k)*qonem
          zgrid(i,j,k)=zgrid(i,j,k-1)-.5*(hwide(k-1)+hwide(k))
        else if (k.eq.klist(i,j)) then
          hwide(k)=dpmm(k)*qonem
          zgrid(i,j,k)=zgrid(i,j,k-1)-.5*(hwide(k-1)+hwide(k))
          zgrid(i,j,k+1)=zgrid(i,j,k)-.5*hwide(k)
        else
          hwide(k)=0.
        endif
c
c --- set 1-d array values; use cgs units
        if (k.le.klist(i,j)) then
          z1d (k)=-100.0*zgrid(i,j,k)
          u1d (k)=50.0*(u(i,j,k,n)+u(i+1,j  ,k,n))
          v1d (k)=50.0*(v(i,j,k,n)+v(i  ,j+1,k,n))
          if (k.eq.1) then
            thsum=0.001*th3d(i,j,k,n)
            th1d(k)=thsum  !offset by 1+0.001*thbase
          else
            k1=k-1
            delz=z1d(k)-z1d(k1)  !50.0*(hwide(k)+hwide(k1))
            s2 (k1)=((u1d(k1)-u1d(k))**2+(v1d(k1)-v1d(k))**2)/
     &              (delz*delz)
            if (locsig) then
              alfadt=0.001*dsiglocdt(ahalf*(temp(i,j,k1,n)+
     &                                      temp(i,j,k, n) ),
     &                               ahalf*(saln(i,j,k1,n)+
     &                                      saln(i,j,k ,n) ),
     &                                         p(i,j,k)      )*
     &                      (temp(i,j,k1,n)-temp(i,j,k, n))
              betads=0.001*dsiglocds(ahalf*(temp(i,j,k1,n)+
     &                                      temp(i,j,k, n) ),
     &                               ahalf*(saln(i,j,k1,n)+
     &                                      saln(i,j,k ,n) ),
     &                                         p(i,j,k)      )*
     &                      (saln(i,j,k1,n)-saln(i,j,k, n))
              thsum=thsum-alfadt-betads
              th1d(k)=thsum  !offset by 1+0.001*thbase
            else
              alfadt=0.001*dsigdt(ahalf*(temp(i,j,k1,n)+
     &                                   temp(i,j,k, n) ),
     &                            ahalf*(saln(i,j,k1,n)+
     &                                   saln(i,j,k ,n) ) )*
     &                   (temp(i,j,k1,n)-temp(i,j,k, n))
              betads=0.001*dsigds(ahalf*(temp(i,j,k1,n)+
     &                                   temp(i,j,k, n) ),
     &                            ahalf*(saln(i,j,k1,n)+
     &                                   saln(i,j,k ,n) ) )*
     &                   (saln(i,j,k1,n)-saln(i,j,k, n))
              th1d(k)=0.001*th3d(i,j,k,n)  !offset by 1+0.001*thbase
            endif
            dens=1.0+0.001*(th3d(i,j,k,n)+thbase)
            gatpbs=-980.0*(alfadt+betads)
            gatmbs=-980.0*(alfadt-betads)
            if     (gatpbs.ne.gatmbs) then !usual case
              an2(k1)=gatpbs/(delz*dens)
              ria(k1)=gatpbs/(delz*dens*max(epsil,s2(k1)))
              rid(k1)=gatmbs/(delz*dens*max(epsil,s2(k1)))
            else !must have ria(k1)==rid(k1)
              an2(k1)=gatpbs/(delz*dens)
              ria(k1)=gatpbs/(delz*dens*max(epsil,s2(k1)))
              rid(k1)=ria(k1)
            endif
          endif
        endif
      enddo
      k=klist(i,j)
      s2 (k)=0.0
      ria(k)=0.0
      rid(k)=0.0
c
cdiag if (i.eq.itest .and. j.eq.jtest) then
cdiag   do k=1,klist(i,j)
cdiag   write(6,'(a,i9,i3,2f10.3,f9.1,f8.5,2f8.2)') 'giss1din1',
cdiag&        nstep,k,zgrid(i,j,k),hwide(k),z1d(k),
cdiag&        th1d(k),u1d(k),v1d(k)
cdiag   enddo
cdiag   write(6,'(a,a9,a3,3a13)') 
cdiag&    'giss1din2','    nstep','  k',
cdiag&    '           s2','          ria','          rid'
cdiag   do k=1,klist(i,j)
cdiag   write(6,'(a,i9,i3,1p,3e13.5)') 'giss1din2',
cdiag&        nstep,k,s2(k),ria(k),rid(k)
cdiag   enddo
cdiag endif
c
      al0=0.17*hbl/onecm
c
c --- Write internal turbulence quantities to fort.91 when writing enabled.
c ---  Headers for each outputstep.
c ---  Add S_M,H,S to outputs.
cdiag if (i.eq.itest .and. j.eq.jtest) then
cdiag   write(lp,'(a,i9)') 'nstep = ',nstep
cdiag   write(lp,*) 'hbl,al0 = ',hbl/onecm,al0
cdiag   write(lp,*) 'b1 = ',b1
cdiag   write(lp,'(a)')    "  z          al         slq2       "//
cdiag&                     "ri1        rid1       "//
cdiag&                     "sm         sh         ss         "//
cdiag&                     "v_back     t_back     s_back     "
cdiag endif
c
      if     (botdiw) then
        acorio = max(acormin, !f(1degN)
     &               0.25*(abs(corio(i,j  ))+abs(corio(i+1,j  ))+
     &                     abs(corio(i,j+1))+abs(corio(i+1,j+1)) ))
        zbot = -100.0*zgrid(i,j,klist(i,j)+1)  !sea depth
      endif !botdiw
c
c --- START OF FIRST LOOP THROUGH LEVELS
c
      if (ifepson2.eq.2) then
c ---   Initialize switch for sub(background-only) depth. 
        ifbelow=0      
      endif
c
c --- depth-grid dooloop starts here
      do 22 k=1,klist(i,j)-1
        ri1=ria(k)
c
c --- Use Ri_d = Ri_C - Ri_T in salinity-temperature turbulence model.
        rid1=rid(k)
c
c --- Interpolate 2D table for salinity-temperature model case.
        call interp2d_expabs(ri1,rid1,slq2,sm,sh,ss,mt,mt0,dri,rri)
c
c --- Check that "slq2" has been set to 0 where it might have been negative.
      if (slq2.lt.0.) then
        write(lp,*) "************************************************"
        write(lp,*) "Error detected in turbulence module." 
        write(lp,*) "'slq2' negative in turb_2 subroutine"
     &               //" after interpolation."
        write(lp,*) "k=",k,"     slq2=",slq2
        write(lp,*) "sm=",sm,"   sh=",sh,"   ss=",ss
        write(lp,*) "ri1=",ri1,"    rid1=",rid1
        write(lp,*) "dri=",dri
        write(lp,*) "Program will stop."
        call flush(lp)
        call xchalt('(mxgissaij)')
               stop '(mxgissaij)'
      endif
c
c
c --- Assume region contiguous with surface where foreground model is
c --- realizable has ended when get 0 "slq2".
      if (slq2.eq.0.) then
        ifbelow = 1
      endif
c
      akz=0.4*z1d(k)
      al=akz*al0/(al0+akz)
      al2=al*al
c
c --- Do not use Deardorff limitation when use (\epsilon/N^2) dimensionalization.
      if (.NOT.((ifepson2.EQ.2).AND.(ifbelow.EQ.1))) then
c --- length scale reduction by buoyancy
          if(ri1.gt.0.) then
            anlq2=slq2*ri1
            if(anlq2.gt.0.281) then  !0.281=0.53**2
              al2=0.281/anlq2*al2
              slq2=0.281/(ri1+1.E-20)
            endif
          endif
      endif !length scale reduction by buoyancy
c
      if     (.not.botdiw) then
c ---   use constant epson2 from inigiss.
        epson2 =  epson2_ref
      else
c
c ---   latitude dependent internal wave diffusion/viscosity
c ---   Gregg et. al. (2003): Reduced mixing from the breaking of
c ---   internal waves in equatorial waters, Nature 422 pp 513-515.
c
        if     (an2(k).le.0.0) then
          eplatidep  = eplatidepmin
        else
          an = SQRT(an2(k)) !N from N^2
          if    (an/acorio.lt.1.0) then   !arccosh(N/|f|) undefined
            eplatidep = eplatidepmin
          else
            eplatidep = (acorio*acosh1(an/acorio))/wave_30
            eplatidep = max(eplatidep,eplatidepmin)
          endif
        endif
        epson2 = epson2_ref*eplatidep
c
        if     (an2(k).gt.epsil) then
c ---     enhanced bottom mixing
          epson2_bot = eps_bot0/an2(k) * exp((z1d(k) - zbot)/scale_bot)
          epson2     = max(epson2,epson2_bot)
        endif !an2
      endif !botdiw
c
c------------------------------------------------------------------------
c
c --- BEGIN SECTION .or.SALINITY MODEL BACKGROUND DIFFUSIVITY CALCULATION.
      if (ifsali.gt.0) then
c
      if (ifsalback.ge.4) then
c --- Change ALL THREE BACKGROUND DIFFUSIVITIES from input values to 
c --- diffusivities calculated using the turbulence model
c --- with Ri and l_0 replaced by constants 'ri_internal' and 'back_l_0' 
c --- and S^2 replaced by  (N^2 / Ri_internal) for N^2>=0 and 0 for N^2 <0
c --- to represent a modified Dubovikov internal wave generated turbulence
c --- with constant Richardson number for ifsalback=4 case.
c
c --- Use a constant background Ri estimate. 
      if (ifsalback.EQ.4) then
        back_rit1 = 0.
        back_ric1 = 0.
        back_ri1  = ri_internal
        back_rid1 = (rid1/ri1)*ri_internal
      else
c
c --- Change ALL THREE BACKGROUND DIFFUSIVITIES from input values to 
c --- diffusivities calculated using the turbulence model
c --- with l_0 replaced by a constant 'back_l_0' and 
c --- Ri by a function of Ri_d
c --- and S^2 replaced by  (N^2 / Ri_internal) for N^2>=0 and 0 for N^2 <0
c --- to represent a modified Dubovikov internal wave generated turbulence
c --- with stability-ratio dependent Richardson number for ifsalback>4 case.
c
c --- When Ri_T = 0 and Ri_C \ne 0,
c --- correctly set the angle 'theta_r' in the (Ri_T,Ri_C) plane to 'pi'/2 . 
c
c --- Skip background ra_r calculation in unstable .or.NEUTRAL* case.
c --- Set background ra_r arbitrarily to zero in these cases.
          if (ria(k).le.0.) then
            back_ra_r1 = 0.
            back_rit1  = 0.
            back_ric1  = 0.
            back_ri1   = 0.
            back_rid1  = 0.
            go to 19
          endif
c
c --- Linearly interpolate back_ra_r array to this angle in (Ri_C,Ri_T) space.
c --- Ri \equiv Ri_T + Ri_C 	; Ri_d \equiv Ri_T - Ri_C .
          rit = (ria(k) + rid(k))/2.
          ric = (ria(k) - rid(k))/2.
          ra_r = sqrt((rit**2) + (ric**2))
c
c ---  use newer better treatment of zero thermal gradient case.
c ---  find \theta_r for the Ri_T = 0 case. Treat "0/0 = 1".
          if(rit.eq.0.0) then
            if(ric.eq.0.0) then
              theta_r = atan(1.0)
            else
              theta_r = pi/2.0       ! Arctangent of infinity.
            endif
          else
            theta_r = atan(ric/rit)
          endif
c
c --- Make sure the right choice of arctan(Ri_C/Ri_T) [\theta_r] is made.
c --- Arctan only covers the range (-pi/2,pi/2) which theta_r may be outside.
c --- Want to consider statically stable case only: Ri > 0.
          if (abs(theta_r).gt.(pi/2.)) then
            write(lp,*) 
     &       "************************************************"
            write(lp,*) "Error detected in turbulence module." 
            write(lp,*) "theta_r (=",abs(theta_r),") too large"
            call flush(lp)
            call xchalt('(mxgissaij)')
                   stop '(mxgissaij)'
          endif
          if (theta_r.lt.(-pi)/4.) then
            theta_r = theta_r + pi
          endif
c
c --- MAKE 'jtheta' A NON-NEGATIVE INDEX - ZERO AT THETA = -PI/4 .
c --- The fortran function "INT" rounds to the integer *NEAREST TO ZERO*
c --- **I.E. ROUNDS **UP** .or.NEGATIVE NUMBERS**, DOWN ONLY .or.POSITIVES.
          jtheta_r0 = INT((theta_r + (pi/4.))/deltheta_r)
          jtheta_r1 = jtheta_r0+1
c
c --- INTRODUCE 'itheta' HERE .or.THE INDEX THAT IS ZERO AT THETA=0.
          itheta_r0 = jtheta_r0 - n_theta_r_oct
          itheta_r1 = itheta_r0+1
c
c --- ***WHEN THE ANGLE IS BETWEEN THE ANGLE .or.REALIZABILITY AT INFINITY***
c --- ***AND THE LAST TABLE ANGLE BE.or. THAT CRITICAL ANGLE, *** 
c --- ***SET IT TO THE LAST TABLE ANGLE BE.or. THE CRITICAL ANGLE.****
          theta_r0 = itheta_r0*deltheta_r
          theta_r1 = itheta_r1*deltheta_r
c
          if     ((theta_r0.le.theta_rcrp).AND.
     &            (theta_r .gt.theta_rcrp)     ) then
            theta_r = theta_r1
            theta_r0 = theta_r1
            itheta_r0 = itheta_r1 
            itheta_r1 = itheta_r1+1
            theta_r1 = theta_r1 + deltheta_r
          elseif ((theta_r1.ge.theta_rcrn).AND.
     &            (theta_r .lt.theta_rcrn)     ) then
            theta_r = theta_r0
            theta_r1 = theta_r0
            itheta_r1 = itheta_r0 
            itheta_r0 = itheta_r0-1
            theta_r0 = theta_r0 - deltheta_r
          endif
c
c --- Angle in degrees.
          theta_r_deg = theta_r*180./pi
c
c --- Sound the alarm if have unrealizability outside expected range in angle.
          if ((itheta_r1.gt.3*n_theta_r_oct).or.
     &        (itheta_r0.lt. -n_theta_r_oct)    ) then
               write(lp,*) 
     &         "************************************************"
            write(lp,*) "Problem in turbulence module!"
            write(lp,*) "Unrealizability outside Ri>0 region. "
            write(lp,*) "slq2=",slq2,"    sm=",sm," sh=",sh," ss=",ss
            write(lp,*) "k=",k,"  ria(k)=",ria(k),"  rid(k)=",rid(k)
            write(lp,*) "rit=",rit,"ric=",ric,"    theta_r=",theta_r
            write(lp,*) "theta_r_deg =",theta_r_deg
            write(lp,*) "itheta_r0=",itheta_r0," itheta_r1=",itheta_r1
            write(lp,*) "n_theta_r_oct=",n_theta_r_oct 
            write(lp,*) " "
            write(lp,*) "i,j=",i+i0,j+j0
            write(lp,*) "Program will stop."
            call flush(lp)
            call xchalt('(mxgissaij)')
                   stop '(mxgissaij)'
          endif
c
          deltheta_r1 = theta_r - theta_r0
          delback_ra_r = back_ra_r(itheta_r1) - back_ra_r(itheta_r0)
          dback_ra_r_o_dtheta = delback_ra_r/deltheta_r
          back_ra_r1 = back_ra_r(itheta_r0) + 
     &                   deltheta_r1*dback_ra_r_o_dtheta
c
c --- In case choose ifrafgmax=1, ra_r is at maximum the ForeGround ra_r
c --- at the "strong" double diffusive \theta_r's 
c --- where have turbulence as Ri+> infinity. 
         ifrafglt=0
         if (ifrafgmax.EQ.1) then
           if ((theta_r.le.theta_rcrp).or.(theta_r.ge.theta_rcrn)) then
             if (back_ra_r1.gt.ra_r) then
               ifrafglt=1
               back_ra_r1=ra_r
             endif
           endif
         endif
c
        if (back_ra_r1.lt.0.) then
          write(lp,*) 
     &       "************************************************"
          write(lp,*) "Problem in turbulence module!"
          write(lp,*) "Negative bg ra_r \\equiv (Ri_T^2+Ri_C^2)^(1/2)"
          write(lp,*) "back_ra_r1 =", back_ra_r1
          write(lp,*) "theta_r =", theta_r
          write(lp,*) " "
          write(lp,*) "slq2=",slq2,"    sm=",sm," sh=",sh," ss=",ss
          write(lp,*) "k=",k,"  ria(k)=",ria(k),"  rid(k)=",rid(k)
          write(lp,*) "rit=",rit,"ric=",ric
          write(lp,*) "itheta_r0=",itheta_r0," itheta_r1=",itheta_r1
          write(lp,*) "jtheta_r0=",jtheta_r0," jtheta_r1=",jtheta_r1
          write(lp,*) "theta_r_deg =",theta_r_deg
          write(lp,*) "n_theta_r_oct=",n_theta_r_oct 
          write(lp,*) " "
          write(lp,*) "i,j=",i+i0,j+j0
          write(lp,*) "Program will stop."
          call flush(lp)
          call xchalt('(mxgissaij)')
                 stop '(mxgissaij)'
        endif 
c
c --- Calculate the background Ri and Ri_d .
        back_rit1 = cos(theta_r)*back_ra_r1
        back_ric1 = sin(theta_r)*back_ra_r1
        back_ri1  = back_rit1 + back_ric1
        back_rid1 = back_rit1 - back_ric1
c
      endif !ifsalback.EQ.4:else
c
c --- CALCULATE THE BACKGROUND DIMENSIONLESS TURBULENCE FUNCTIONS
c --- USING TABLE OF VALUES .or.BACKGROUND "\theta_r"'S 
c --- .or."ifbg_theta_interp"=1.
c --- Can only use theta_r table when do *not* reduce ra_r_BackGround
c --- to a smaller ra_r_ForeGround.
      if ((ifbg_theta_interp.EQ.0).or.(ifrafglt.EQ.1)) then
c
c --- Use the calculated background Ri and Ri_d in the turbulence model.
c --- Interpolate 2D table for salinity-temperature model case.
c 
        call interp2d_expabs(back_ri1,back_rid1,
     &               slq2_back,sm_back,sh_back,ss_back,mt,mt0,dri,rri)
c 
      elseif(ifbg_theta_interp.EQ.1) then
c --- Interpolate 1D table of background vs. theta_r instead.
*       if     (mnproc.eq.-99) then !always .false.
*         ! bugfix, potential I/O reduces the level of optimization
*       if     ((iglobal.eq.344.and.jglobal.eq.  1) .or.
*    &          (iglobal.eq.378.and.jglobal.eq. 32)     ) then
*         write(lp,*) 'i,j,k   = ',iglobal,jglobal,k
*         write(lp,*) '  ithet = ',itheta_r0,itheta_r1
*         write(lp,*) '  theta = ',theta_r,deltheta_r
*         write(lp,*) '  sm_r  = ',sm_r1(itheta_r0),sm_r1(itheta_r1)
*         write(lp,*) '  sh_r  = ',sh_r1(itheta_r0),sh_r1(itheta_r1)
*         write(lp,*) '  ss_r  = ',ss_r1(itheta_r0),ss_r1(itheta_r1)
*         write(lp,*) 'slq2_r  = ',slq2_r1(itheta_r0),slq2_r1(itheta_r1)
*         call flush(lp)
*       endif
        deltheta_r1 = theta_r - itheta_r0*deltheta_r
        delsm_back = sm_r1(itheta_r1) - sm_r1(itheta_r0)
        dsm_back_o_dtheta = delsm_back/deltheta_r
        sm_back = sm_r1(itheta_r0) + 
     &                   deltheta_r1*dsm_back_o_dtheta
        delsh_back = sh_r1(itheta_r1) - sh_r1(itheta_r0)
        dsh_back_o_dtheta = delsh_back/deltheta_r
        sh_back = sh_r1(itheta_r0) + 
     &                   deltheta_r1*dsh_back_o_dtheta
        delss_back = ss_r1(itheta_r1) - ss_r1(itheta_r0)
        dss_back_o_dtheta = delss_back/deltheta_r
        ss_back = ss_r1(itheta_r0) + 
     &                   deltheta_r1*dss_back_o_dtheta
        delslq2_back = slq2_r1(itheta_r1) - slq2_r1(itheta_r0)
        dslq2_back_o_dtheta = delslq2_back/deltheta_r
        slq2_back = slq2_r1(itheta_r0) + 
     &                   deltheta_r1*dslq2_back_o_dtheta
      else
        write(lp,*) "Problem with choice of background interpolation."
        write(lp,*) "ifbg_theta_interp=",ifbg_theta_interp
        write(lp,*) "ifrafglt=",ifrafglt
        write(lp,*) "Program is stopping."
        call flush(lp)
        call xchalt('(mxgissaij)')
               stop '(mxgissaij)'
      endif
c
c --- Calculate the square of the shear from the background Richardson number.
c --- s2_back   = N^2 / ri_internal = (N^2 / S_ext^2) (S_ext^2 /ri_internal) 
c ---           = (Ri_ext / ri_internal) S_ext^2
        s2_back = (ri1/back_ri1)*s2(k)
c
c --- Set square of shear to zero for unstable density stratification.
   19   continue
        if (ri1.le.0.) then
          s2_back = 0.
        endif
c
c --- Set ill-defined S_M,H,S for unstable density stratification to zero.
        if (ri1.lt.0.) then
c
          sm_back = 0
          sh_back = 0
          ss_back = 0
        endif
c
        if     (  sm_back.lt.0.0 .or.
     &            sh_back.lt.0.0 .or.
     &            ss_back.lt.0.0 .or.
     &          slq2_back.lt.0.0     ) then
           v_back=2.0e-1
           t_back=5.0e-2
           s_back=5.0e-2
*          write(lp,'(i9,a,2i5,i3,a)')
*    &       nstep,' i,j,k=',i+i0,j+j0,k,' GISS neg. sX_back'
c
c --- Skip background lengthscale calculation when using K_X/(epsilon/N^2) .
        elseif (ifepson2.eq.0) then
c
c --- Use the constant background l_0 lengthscale in the turbulence model.
          al0_back = back_l_0
          akz=0.4*z1d(k)
          al_back=akz*al0_back/(al0_back+akz)
          al2_back=al_back*al_back
c --- length scale reduction by buoyancy
          if(back_ri1.gt.0.) then
            anlq2_back=slq2_back*back_ri1
            if(anlq2_back.gt.0.281) then  !0.281=0.53**2
              al2_back=0.281/anlq2_back*al2_back
              slq2_back=0.281/(back_ri1+1.E-20)
            endif
          endif
c
c --- Calculate the background diffusivities.
          tmp_back=0.5*b1*al2_back*sqrt(s2_back/(slq2_back+1.E-40))
          v_back(k)=tmp_back*sm_back
          t_back(k)=tmp_back*sh_back
          s_back(k)=tmp_back*ss_back
c
c --- Use K_X = K_X/(\epsilon/N^2) * (\epsilon/N^2)    
c --- From NBp.000215-5, Volume IX : 
c --- K_X/(\epsilon/N^2) = (1/2) B_1 Ri (S l/q)^2 S_X  .
c --- K_X = (((1/2) B_1^2 Ri (S l/q)^2)* (\epsilon/N^2)) * S_X 
        else !if(ifepson2.gt.0) then
          tmp_back=0.5*b1**2*back_ri1*slq2_back*epson2
          v_back(k)=tmp_back*sm_back
          t_back(k)=tmp_back*sh_back
          s_back(k)=tmp_back*ss_back
        endif
c
c --- Stop if background diffusivities are negative.
        if ((v_back(k).lt.0.).or.
     &      (t_back(k).lt.0.).or.
     &      (s_back(k).lt.0.)    ) then
               write(lp,*) 
     &         "************************************************"
            write(lp,*) "Problem in turbulence module!"
            write(lp,*) "Negative Background Diffusivity."
            write(lp,*) "v_back=",v_back(k)
            write(lp,*) "t_back=",t_back(k)
            write(lp,*) "s_back=",s_back(k)
            write(lp,*) " "
            write(lp,*) "slq2_back=",slq2_back
            write(lp,*) "sm_back=",sm_back
            write(lp,*) "sh_back=",sh_back
            write(lp,*) "ss_back=",ss_back
            write(lp,*) " "
            write(lp,*) "back_ra_r1 =", back_ra_r1
            write(lp,*) "theta_r =", theta_r,
     &                   "   theta_r_deg=",theta_r_deg
            write(lp,*) "back_rit1=",back_rit1,"back_ric1=",back_ric1
            write(lp,*) "back_ri1=",back_ri1,"back_rid1=",back_rid1
            write(lp,*) " "
            write(lp,*) "k=",k,"  ria(k)=",ria(k),"  rid(k)=",rid(k)
            write(lp,*) "rit=",rit,"ric=",ric
            write(lp,*) " "
            write(lp,*) "i,j=",i+i0,j+j0
            write(lp,*) "Program will stop."
            call flush(lp)
            call xchalt('(mxgissaij)')
                   stop '(mxgissaij)'
          endif
c
c --- Stop if background diffusivities are zero at positive Ri.
          if ((ria(k).gt.0.).and.
     &        ((v_back(k).eq.0.).or.
     &         (t_back(k).EQ.0.).or.
     &         (s_back(k).EQ.0.)    )) then
c
               write(lp,*) 
     &         "************************************************"
            write(lp,*) "Problem in turbulence module!"
            write(lp,*) "Zero Background Diffusivity in stable case."
            write(lp,*) "v_back=",v_back(k),
     &                   " t_back=",t_back(k),
     &                   " s_back=",s_back(k)
c Natassa
              write(lp,*) "tmp_back=",tmp_back
              write(lp,*) "b1=",b1
              write(lp,*) "back_ri1=",back_ri1
              write(lp,*) "epson2=",epson2
              write(lp,*) "ri1=", ri1
c
c
            write(lp,*) " "
            write(lp,*) "slq2_back=",slq2_back
            write(lp,*) "sm_back=",sm_back,
     &                   " sh_back=",sh_back,
     &                   " ss_back=",ss_back
            write(lp,*) " "
            write(lp,*) "slq2_r1(itheta_r0)=",slq2_r1(itheta_r0),
     &                   " slq2_r1(itheta_r1)=",slq2_r1(itheta_r1)
            write(lp,*) "sm_r1(itheta_r0)=",sm_r1(itheta_r0),
     &                   " sm_r1(itheta_r1)=",sm_r1(itheta_r1)
            write(lp,*) "sh_r1(itheta_r0)=",sh_r1(itheta_r0),
     &                   " sh_r1(itheta_r1)=",sh_r1(itheta_r1)
            write(lp,*) "ss_r1(itheta_r0)=",ss_r1(itheta_r0),
     &                   " ss_r1(itheta_r1)=",ss_r1(itheta_r1)
            write(lp,*) " "
            write(lp,*) "back_ra_r1 =", back_ra_r1
            write(lp,*) "theta_r =", theta_r
            write(lp,*) "theta_r_deg =", theta_r_deg
            write(lp,*) "back_rit1=",back_rit1,"back_ric1=",back_ric1
            write(lp,*) "back_ri1=",back_ri1,"back_rid1=",back_rid1
            write(lp,*) "itheta_r0=",itheta_r0," itheta_r1=",itheta_r1
            write(lp,*) "jtheta_r0=",jtheta_r0," jtheta_r1=",jtheta_r1
            write(lp,*) "n_theta_r_oct=",n_theta_r_oct 
            write(lp,*) "deltheta_r=",deltheta_r
            write(lp,*) " "
            write(lp,*) "k=",k,"  ria(k)=",ria(k),"  rid(k)=",rid(k)
            write(lp,*) "rit=",rit,"ric=",ric
            write(lp,*) " "
            write(lp,*) "i,j=",i+i0,j+j0
            write(lp,*) "Program will stop."
            call flush(lp)
            call xchalt('(mxgissaij)')
                   stop '(mxgissaij)'
          endif
        endif
 20     continue
c
      endif !ifsali.gt.0
c
c --- end SECTION .or.SALINITY MODEL BACKGROUND DIFFUSIVITY CALCULATION.
c
c
c --- Write internal turbulence quantities
c --- Add S_M,H,S to outputs
cdiag if (i.eq.itest .and. j.eq.jtest) then
cdiag   write(lp,9000) z1d(k),al,slq2,ri1,rid1,sm,sh,ss,
cdiag&                 v_back(k),t_back(k),s_back(k)
cdiag endif
c
c --- introduce foreground minimum shear squared due to internal waves
c --- to allow mixing in the unstable zero shear case
c --- Reversion to ifsalback=3 model for this purpose,
c --- based on Gargett et. al. JPO Vol.11 p.1258-71 "deep record".
         s2(k) = max(s2(k),back_s2)
c
c --- In the case where the model is realizable at 
c --- the Ri obtained from the external Shear,
c --- *but* there is a level above where it is NOT thus realizable, 
c --- USE THE "epsilon/(N^2)" DIMENSIONALIZATION .or."ifepson=2".
c --- EXCEPT if  "Ri<0" do *NOT* USE "epsilon/(N^2)" DIMENSIONALIZATION
c --- BECAUSE IT PRODUCES NEGATIVE DIFFUSIVITIES IN THIS CASE.
c --- *INSTEAD USE "l_deep^2 S", WHERE "l_deep" IS DERIVED FROM "rho" PROFILE.
c --- "|{{d \rho / dz} \over {d2 \rho / dz^2}}| takes place of MLD in l_deep".
c --- BUT *REVERT* TO "MLD" IN CASES OF FIRST TWO LEVELS.
cdiag   aldeep(k)=0.0
        if ((ifepson2.EQ.2).AND.(ifbelow.EQ.1)) then
          if (ri1.ge.0) then
              tmp=0.5*b1**2*ri1*slq2*epson2
          elseif(k.gt.2) then
                delz   =   z1d(k+1) -  z1d(k-1)  !original version
                delth  =  th1d(k+1) - th1d(k-1)
                del2th = (th1d(k+1) + th1d(k-1)) - 2.*th1d(k)
              dzth = delth/delz
              d2zth = 4.*del2th/(delz**2)
c
c --- rdzlndzth = *Reciprocal* of Dz_{ln(Dz_{th})} = Dz_{th}/Dz2_{th} .
              if (d2zth.eq.0.0) d2zth=epsil
              rdzlndzth = dzth/d2zth
c
c --- introduce deep foreground minimum length scale due to internal waves
c --- to prevent zero lengthscale in deep zero density gradient case
c --- reversion to ifsalback=3 model for this purpose
              al0deep=max(0.17*abs(rdzlndzth),back_l_0)
              akz=0.4*z1d(k)
              aldeep(k)=akz*al0deep/(al0deep+akz)
              al2=aldeep(k)*aldeep(k)
              tmp=0.5*b1*al2*sqrt(s2(k)/(slq2+1.E-40))
          else
              tmp=0.5*b1*al2*sqrt(s2(k)/(slq2+1.E-40))
          endif
        else
            tmp=0.5*b1*al2*sqrt(s2(k)/(slq2+1.E-40))
        endif
          akm(k)=tmp*sm+v_back(k)
          akh(k)=tmp*sh+t_back(k)
          aks(k)=tmp*ss+s_back(k)
c
cdiag     tmpk(k)=tmp
c
 22   continue
c
c --- stop if DIFFUSIVITY IS NEGATIVE.
      do k =1,klist(i,j)-1
      if ((akm(k).lt.0.).or.(akh(k).lt.0.).or.(aks(k).lt.0.)) then
          write(lp,*) "Diffusivity is negative."
        write(lp,*) "k=",k
        write(lp,*) "z[cm]      tem[C]     sal[ppt]   rho[g/cm3] "//
     &                "Ri         Ri_d	   S^2[/s2]   "//
     &                "K_M[cm2/s] K_H[cm2/s] K_S[cm2/s] "
          write(*,9000) z1d(k),th1d(k),ria(k),rid(k),s2(k),
     &                  akm(k),akh(k),aks(k)
        write(lp,*) " "
        write(lp,*) "Program will stop."
        call flush(lp)
        call xchalt('(mxgissaij)')
               stop '(mxgissaij)'
      endif
      enddo
c
c --- store new k values in the 3-d arrays
      do k=1,kk
        k1=k+1
        if(k.lt.klist(i,j)) then
          vcty(i,j,k1)=min(akm(k)*1.0e-4,difmax)
          dift(i,j,k1)=min(akh(k)*1.0e-4,difmax)
          difs(i,j,k1)=min(aks(k)*1.0e-4,difmax)
        else
          vcty(i,j,k1)=vcty(i,j,klist(i,j))
          dift(i,j,k1)=dift(i,j,klist(i,j))
          difs(i,j,k1)=difs(i,j,klist(i,j))
        endif
      enddo
cdiag if (i.eq.itest .and. j.eq.jtest) then
cdiag   write(6,'(a,a9,a3,5a13)') 
cdiag&    'giss1dout','    nstep','  k',
cdiag&    '          tmp','       aldeep',
cdiag&    '          akm','          akh','          aks'
cdiag   do k=1,klist(i,j)
cdiag     write(6,'(a,i9,i3,1p,6e13.5)') 'giss1dout',
cdiag&          nstep,k,tmpk(k),aldeep(k),akm(k),akh(k),aks(k)
cdiag   enddo
cdiag endif
c
 9000 format(12(1pe11.3))
c
 101  format(i9,3i4,'absorbup,dn,dtemp,dsaln ',2f6.3,2f10.6)
c
      return   
      end
c
      subroutine mxkprfbij(m,n, i,j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
c
c --- hycom version 1.0
      implicit none
c
      integer m,n, i,j
c
c -------------------------------------------------------------
c --- k-profile vertical diffusion, single j-row (part B)
c --- vertical coordinate is z negative below the ocean surface
c -------------------------------------------------------------
c
c --- perform the final vertical mixing at p points
c
c --- local 1-d arrays for matrix solution
      real t1do(kdm+1),t1dn(kdm+1),s1do(kdm+1),s1dn(kdm+1),
     &     tr1do(kdm+1,mxtrcr),tr1dn(kdm+1,mxtrcr),
     &     difft(kdm+1),diffs(kdm+1),difftr(kdm+1),
     &     ghat(kdm+1),zm(kdm+1),hm(kdm),dzb(kdm)
c
c --- tridiagonal matrix solution arrays
      real tri(kdm,0:1)      ! dt/dz/dz factors in trid. matrix
      real tcu(kdm),         ! upper coeff for (k-1) on k line of trid.matrix
     &     tcc(kdm),         ! central ...     (k  ) ..
     &     tcl(kdm),         ! lower .....     (k-1) ..
     &     rhs(kdm)          ! right-hand-side terms
c
      real    ghatflux
      integer k,ka,ktr,nlayer
c
      real, parameter :: difriv =   60.0e-4  !river diffusion
c
      include 'stmt_fns.h'
c
      nlayer=klist(i,j)
c
      do k=1,nlayer
        difft( k+1)=dift(i,j,k+1)
        diffs( k+1)=difs(i,j,k+1)
        difftr(k+1)=difs(i,j,k+1)
        ghat(k+1)= ghats(i,j,k+1)
        t1do(k)=temp(i,j,k,n)
        s1do(k)=saln(i,j,k,n)
        do ktr= 1,ntracr
          tr1do(k,ktr)=tracer(i,j,k,n,ktr)
        enddo
        hm(k)=max(onemm,dp(i,j,k,n))*qonem
        zm(k)=zgrid(i,j,k)
      enddo !k
c
      k=nlayer+1
      ka=min(k,kk)
      difft( k)=0.0
      diffs( k)=0.0
      difftr(k)=0.0
      ghat(k)=0.0
      t1do(k)=temp(i,j,ka,n)
      s1do(k)=saln(i,j,ka,n)
      do ktr= 1,ntracr
        tr1do(k,ktr)=tracer(i,j,ka,n,ktr)
      enddo
      zm(k)=zm(k-1)-0.001
c
cdiag if (i.eq.itest.and.j.eq.jtest) then
cdiag     write (lp,102) (nstep,i+i0,j+j0,k,
cdiag&      hm(k),t1do(k),temp(i,j,k,n),s1do(k),saln(i,j,k,n),
cdiag&      k=1,nlayer)
cdiag     call flush(lp)
 102    format(25x,
     &     '  thick   t old   t ijo   s old   s ijo'
     &     /(i9,2i5,i3,2x,f9.2,4f8.3))
cdiag endif !test
c
c --- do rivers here because difs is also used for tracers.
      if     (thkriv.gt.0.0 .and. rivers(i,j,1).ne.0.0) then
        do k=1,nlayer-1
          if     (-zm(k)+0.5*hm(k).lt.thkriv) then !interface<thkriv
            diffs(k+1) = max(diffs(k+1),difriv)
          endif
        enddo !k
      endif !river
c
c --- compute factors for coefficients of tridiagonal matrix elements.
c     tri(k=1:NZ,0) : dt/hwide(k)/ dzb(k-1)=z(k-1)-z(k)=dzabove)
c     tri(k=1:NZ,1) : dt/hwide(k)/(dzb(k  )=z(k)-z(k+1)=dzbelow)
c
      do k=1,nlayer
        dzb(k)=zm(k)-zm(k+1)
      enddo
c
      tri(1,1)=delt1/(hm(1)*dzb(1))
      tri(1,0)=0.
      do k=2,nlayer
        tri(k,1)=delt1/(hm(k)*dzb(k))
        tri(k,0)=delt1/(hm(k)*dzb(k-1))
      enddo
c
c --- solve the diffusion equation
c --- salflx, sswflx and surflx are positive into the ocean
c
c --- t solution
      ghatflux=-(surflx(i,j)-sswflx(i,j))*thref/spcifh
      call tridcof(difft,tri,nlayer,tcu,tcc,tcl)
      call tridrhs(hm,t1do,difft,ghat,ghatflux,tri,nlayer,rhs)
      call tridmat(tcu,tcc,tcl,nlayer,hm,rhs,t1do,t1dn,difft, i,j)
c
c --- t-like tracer solution
      do ktr= 1,ntracr
        if     (trcflg(ktr).eq.2) then
          ghatflux=-(surflx(i,j)-sswflx(i,j))*thref/spcifh
          call tridrhs(hm,
     &                 tr1do(1,ktr),difft,ghat,ghatflux,tri,nlayer,rhs)
          call tridmat(tcu,tcc,tcl,nlayer,hm,rhs,
     &                 tr1do(1,ktr),tr1dn(1,ktr),difft, i,j)
        endif
      enddo
c
c --- s solution
      ghatflux=-salflx(i,j)*thref
      call tridcof(diffs,tri,nlayer,tcu,tcc,tcl)
      call tridrhs(hm,s1do,diffs,ghat,ghatflux,tri,nlayer,rhs)
      call tridmat(tcu,tcc,tcl,nlayer,hm,rhs,s1do,s1dn,diffs, i,j)
c
cdiag if (i.eq.itest.and.j.eq.jtest) then
cdiag     write (lp,103) (nstep,i+i0,j+j0,k,
cdiag&    hm(max(1,k-1)),1.e4*difft(k),1.e4*diffs(k),
cdiag&      ghat(k),k=1,nlayer+1)
cdiag     write (lp,104) (nstep,i+i0,j+j0,k,
cdiag&      hm(k),t1do(k),t1dn(k),s1do(k),s1dn(k),
cdiag&      k=1,nlayer)
cdiag     call flush(lp)
 103    format(25x,'   thick    t diff    s diff   nonlocal'
     &     /(i9,2i5,i3,1x,3f10.2,f11.6))
 104    format(25x,
     &     '  thick   t old   t new   s old   s new'
     &     /(i9,2i5,i3,2x,f9.2,4f8.3))
cdiag endif !test
c
c --- standard tracer solution
      if     (ntracr.gt.0) then
        call tridcof(difftr,tri,nlayer,tcu,tcc,tcl)
      endif
      do ktr= 1,ntracr
        if     (trcflg(ktr).ne.2) then
          ghatflux=0.
          call tridrhs(hm,
     &                 tr1do(1,ktr),difftr,ghat,ghatflux,tri,nlayer,rhs)
          call tridmat(tcu,tcc,tcl,nlayer,hm,rhs,
     &                 tr1do(1,ktr),tr1dn(1,ktr),difftr, i,j)
        endif
      enddo
c
c --- adjust t, s, th, arrays
c
      if     ((tofset.eq.0.0            .and.
     &         sofset.eq.0.0                 ) .or.
     &        (mod(nstep  ,tsofrq).ne.0 .and.
     &         mod(nstep+1,tsofrq).ne.0      )     ) then
        do k=1,klist(i,j)
          temp(i,j,k,n)=t1dn(k)
          saln(i,j,k,n)=s1dn(k)
          th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
          do ktr= 1,ntracr
            tracer(i,j,k,n,ktr)=tr1dn(k,ktr)
          enddo !ktr
        enddo !k
      else  !include [ts]ofset drift correction
        do k=1,klist(i,j)
          temp(i,j,k,n)=t1dn(k) + baclin*max(2,tsofrq)*tofset
          saln(i,j,k,n)=s1dn(k) + baclin*max(2,tsofrq)*sofset
          th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
          do ktr= 1,ntracr
            tracer(i,j,k,n,ktr)=tr1dn(k,ktr)
          enddo !ktr
        enddo !k
      endif !without:with [ts]ofset
c
      return
      end
c
      subroutine mxkprfciju(m,n, i,j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
c
c --- hycom version 1.0
      implicit none
c
      integer m,n, i,j
c
c -------------------------------------------------------------------------
c --- k-profile vertical diffusion, single j-row, momentum at u grid points
c --- vertical coordinate is z negative below the ocean surface
c -------------------------------------------------------------------------
c
c local variables for kpp mixing
c
c --- local 1-d arrays for matrix solution
      real u1do(kdm+1),u1dn(kdm+1),
     &     diffm(kdm+1),zm(kdm+1),hm(kdm),dzb(kdm)
c
c --- tridiagonal matrix solution arrays
      real tri(kdm,0:1)      ! dt/dz/dz factors in trid. matrix
      real tcu(kdm),         ! upper coeff for (k-1) on k line of trid.matrix
     &     tcc(kdm),         ! central ...     (k  ) ..
     &     tcl(kdm),         ! lower .....     (k-1) ..
     &     rhs(kdm)          ! right-hand-side terms
c
      real    presu,dpbot,ubot
      integer k,ka,nlayer
c
      nlayer=1
      presu=0.
      do k=1,kk
        if (presu.lt.depthu(i,j)-tencm) then
          diffm(k+1)=.5*(vcty(i,j,k+1)+vcty(i-1,j,k+1))
          u1do(k)=u(i,j,k,n)
          hm(k)=max(onemm,dpu(i,j,k,n))*qonem
          if (k.eq.1) then
            zm(k)=-.5*hm(k)
          else
            zm(k)=zm(k-1)-.5*(hm(k-1)+hm(k))
          endif
          presu=presu+dpu(i,j,k,n)
          nlayer=k
        else
          exit
        endif
      enddo !k
      k=nlayer+1
      diffm(k)=0.
      u1do(k)=u1do(k-1)
      zm(k)=zm(k-1)-.5*hm(k-1)
c
c --- compute factors for coefficients of tridiagonal matrix elements.
      do k=1,nlayer
        dzb(k)=zm(k)-zm(k+1)
      enddo
c
      tri(1,1)=delt1/(hm(1)*dzb(1))
      tri(1,0)=0.
      do k=2,nlayer
        tri(k,1)=delt1/(hm(k)*dzb(k))
        tri(k,0)=delt1/(hm(k)*dzb(k-1))
      enddo
c
c --- solve the diffusion equation
      call tridcof(diffm,tri,nlayer,tcu,tcc,tcl)
      do k=1,nlayer
        rhs(k)= u1do(k)
      enddo
      call tridmat(tcu,tcc,tcl,nlayer,hm,rhs,u1do,u1dn,diffm, i,j)
      do k=1,nlayer
        u(i,j,k,n)=u1dn(k)
      enddo
c
c --- homogenize the bottom 10 cm
      if     (nlayer.ne.kk) then
        k = nlayer
          dpbot   =         dpu(i,j,k,n)
           ubot   =         dpu(i,j,k,n)*u(i,j,k,n)
        do k= nlayer+1,kk
          dpbot   = dpbot + dpu(i,j,k,n)
           ubot   =  ubot + dpu(i,j,k,n)*u(i,j,k,n)
          u1do(k) =                      u(i,j,k,n)
        enddo !k
        if     (dpbot.gt.onemm) then  !always true?
          ubot = ubot / dpbot
          do k= nlayer,kk
            u(i,j,k,n) = ubot
          enddo !k
        endif !dpbot
      endif !nlayer
c
cdiag if ((i.eq.itest.or.i.eq.itest+1).and.j.eq.jtest) then
cdiag   write (lp,106) nlayer,(nstep,i+i0,j+j0,k,
cdiag&    dpu(i,j,k,n)*qonem,u1do(k),u(i,j,k,n),k=1,kk)
cdiag   call flush(lp)
cdiag endif
      return
 106  format( 'ciju ',23x,'   thick   u old   u new (nlayer =',i3,')' /
     &       ('ciju ',i9,2i5,i3,1x,f10.3,2f8.3))
      end
c
      subroutine mxkprfcijv(m,n, i,j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
c
c --- hycom version 1.0
      implicit none
c
      integer m,n, i,j
c
c --------------------------------------------------------------------------
c --- k-profile vertical diffusion, single j-row , momentum at v grid points
c --- vertical coordinate is z negative below the ocean surface
c --------------------------------------------------------------------------
c
c local variables for kpp mixing
c
c --- local 1-d arrays for matrix solution
      real v1do(kdm+1),v1dn(kdm+1),
     &     diffm(kdm+1),zm(kdm+1),hm(kdm),dzb(kdm)
c
c --- tridiagonal matrix solution arrays
      real tri(kdm,0:1)      ! dt/dz/dz factors in trid. matrix
      real tcu(kdm),         ! upper coeff for (k-1) on k line of trid.matrix
     &     tcc(kdm),         ! central ...     (k  ) ..
     &     tcl(kdm),         ! lower .....     (k-1) ..
     &     rhs(kdm)          ! right-hand-side terms
c
      real    presv,dpbot,vbot
      integer k,ka,nlayer
c
      nlayer=1
      presv=0.
      do k=1,kk
        if (presv.lt.depthv(i,j)-tencm) then
          diffm(k+1)=.5*(vcty(i,j,k+1)+vcty(i,j-1,k+1))
          v1do(k)=v(i,j,k,n)
          hm(k)=max(onemm,dpv(i,j,k,n))*qonem
          if (k.eq.1) then
            zm(k)=-.5*hm(k)
          else
            zm(k)=zm(k-1)-.5*(hm(k-1)+hm(k))
          endif
          presv=presv+dpv(i,j,k,n)
          nlayer=k
        else
          exit
        endif
      enddo !k
      k=nlayer+1
      diffm(k)=0.
      v1do(k)=v1do(k-1)
      zm(k)=zm(k-1)-.5*hm(k-1)
c
c --- compute factors for coefficients of tridiagonal matrix elements.
c
      do k=1,nlayer
        dzb(k)=zm(k)-zm(k+1)
      enddo
c
      tri(1,1)=delt1/(hm(1)*dzb(1))
      tri(1,0)=0.
      do k=2,nlayer
        tri(k,1)=delt1/(hm(k)*dzb(k))
        tri(k,0)=delt1/(hm(k)*dzb(k-1))
      enddo
c
c --- solve the diffusion equation
      call tridcof(diffm,tri,nlayer,tcu,tcc,tcl)
      do k=1,nlayer
        rhs(k)=v1do(k)
      enddo
      call tridmat(tcu,tcc,tcl,nlayer,hm,rhs,v1do,v1dn,diffm, i,j)
      do k=1,nlayer
        v(i,j,k,n)=v1dn(k)
      enddo
c
c --- homogenize the bottom 10 cm
      if     (nlayer.ne.kk) then
        k = nlayer
          dpbot   =         dpv(i,j,k,n)
           vbot   =         dpv(i,j,k,n)*v(i,j,k,n)
        do k= nlayer+1,kk
          dpbot   = dpbot + dpv(i,j,k,n)
           vbot   =  vbot + dpv(i,j,k,n)*v(i,j,k,n)
          v1do(k) =                      v(i,j,k,n)
        enddo !k
        if     (dpbot.gt.onemm) then  !always true?
          vbot = vbot / dpbot
          do k= nlayer,kk
            v(i,j,k,n) = vbot
          enddo !k
        endif !dpbot
      endif !nlayer
c
cdiag if (i.eq.itest.and.(j.eq.jtest.or.j.eq.jtest+1)) then
cdiag   write (lp,107) nlayer,(nstep,i+i0,j+j0,k,
cdiag&    dpv(i,j,k,n)*qonem,v1do(k),v(i,j,k,n),k=1,kk)
cdiag   call flush(lp)
cdiag endif
      return
 107  format( 'cijv ',23x,'   thick   v old   v new (nlayer =',i3,')' /
     &       ('cijv ',i9,2i5,i3,1x,f10.3,2f8.3))
      return
      end
c
      subroutine wscale(i,j,zlevel,dnorm,bflux,wm,ws,isb)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
#if defined(STOKES)
      use mod_stokes     ! HYCOM Stokes drift
#endif
c
      implicit none
c
      integer i,j,isb
      real    zlevel,dnorm,bflux,wm,ws
c
c -------------------------------------------------------------------------
c --- subroutine to compute turbulent velocity scales for kpp mixing scheme
c --- vertical coordinate is z negative below the ocean surface
c -------------------------------------------------------------------------
c
c --- isb determines whether the calculation is for the surface or
c --- bottom boundary layer
c
c --- see inikpp for initialization of /kppltr/ and other constants.
c
      logical, parameter :: ldebug_wscale=.false.  !usually .false.
c
      integer, parameter :: nzehat=890
      integer, parameter :: nustar=192
c
      real, dimension (0:nzehat+1,0:nustar+1) ::
     & wmt            ! momentum velocity scale table
     &,wst            ! scalar   velocity scale table
      common/kppltr/ wmt,wst
      save  /kppltr/
c
#if defined(STOKES)
      real    cw,flang,ustk2,wcube
#endif
      real    zdiff,udiff,zfrac,ufrac,ust,
     &        wam,wbm,was,wbs,ucube,zehat
      integer iz,izp1,ju,jup1
c
c --- use lookup table for zehat < zmax  only;  otherwise use stable formulae
c
      if(isb.eq.1) then
        ust=ustar(i,j)
c ---   note: surface density increases (column is destabilized) if bflux > 0
        zehat=-vonk*dnorm*zlevel*bflux
      else
        ust=ustarb(i,j)
        zehat= vonk*dnorm*zlevel*bflux
      endif
      if (zehat.le.zmax) then
        zdiff=zehat-zmin
        iz=int(zdiff/deltaz)
        iz=max(min(iz,nzehat),0)
        izp1=iz+1
c
        udiff=ust-umin
        ju=int(udiff/deltau)
        ju=max(min(ju,nustar),0)
        jup1=ju+1
c
        zfrac=zdiff/deltaz-iz
        ufrac=udiff/deltau-ju
c
        wam=(1.-zfrac)*wmt(iz,jup1)+zfrac*wmt(izp1,jup1)
        wbm=(1.-zfrac)*wmt(iz,ju  )+zfrac*wmt(izp1,ju  )
        wm =(1.-ufrac)*wbm         +ufrac*wam
c
        was=(1.-zfrac)*wst(iz,jup1)+zfrac*wst(izp1,jup1)
        wbs=(1.-zfrac)*wst(iz,ju  )+zfrac*wst(izp1,ju  )
        ws =(1.-ufrac)*wbs         +ufrac*was
c
      else
c
        ucube=ust**3
        wm=vonk*ust*ucube/(ucube+c11*zehat)
        ws=wm
c
      endif
c
#if defined(STOKES)
      if     (isb.eq.1 .and. langmr.gt.0) then
c ---   Langmuir turbulence enhancement factor
        ustk2=usds(i,j)**2 + vsds(i,j)**2
        select case (langmr)
          case (1) ! McWilliams & Sullivan (2001)
                   !   Spill Sci Technol Bull 6: 225-237
                   !   Vertical mixing by Langmuir circulations.
                   ! flang=sqrt( 1. + 0.080 / la_t**4 )
            flang = sqrt( 1. + 0.080 * ustk2 /(ust*ust + epsil)) 
          case (2) ! Smyth et al, 2002 Ocean Dynamics 52, pp 104-115
                   !   Nonlocal fluxes and Stokes drift effects in the KPP
                   ! flang=max(1,min(5,sqrt(1+cw/La**4)))
            ucube=ust**3
c ---   note: surface density increases (column is destabilized) if bflux > 0
            wcube=-vonk*bflux*zlevel
            cw   =0.15*(ucube/max(ucube + 0.6*wcube, epsil))**2
            flang=sqrt(1+cw*ustk2/(ust*ust + epsil))
            flang=max(1.0, flang)
            flang=min(5.0, flang)  !original used 5.0
          case (3) ! McWilliams & Sullivan (2001) and
                   ! Harcourt & D'Asaro (2008) J. Phys. Oceanogr., 38,
                   !   Large-Eddy Simulation of Langmuir Turbulence
                   !   in Pure Wind Seas
                   ! flang=sqrt( 1. + 0.098 / la_t**2 )
            flang = sqrt( 1. + 0.098*sqrt(ustk2)/(ust + epsil) ) 
          case (4) ! Takaya et al (2010), J. Geophys. Res., 115, C06009,
                   !   Refinements to a prognostic scheme of 
                   !   skin sea surface temperature
                   ! flang=max(1,la_t**(-0.667))
            flang = max( sqrt(ust/(sqrt(ustk2) + epsil))**(-0.667), 1. )
          case default
            flang = 1.
        end select
        ws=ws*flang
        wm=wm*flang
*
              if (ldebug_wscale .and. i.eq.itest.and.j.eq.jtest) then
                write (lp,'(i9,2i5,a,1p3e12.3)')
     &            nstep,i+i0,j+j0,
     &            '   ustk2  =',ustk2,usds(i,j),vsds(i,j)
                write (lp,'(i9,2i5,a,3f12.8)')
     &            nstep,i+i0,j+j0,
     &            '    La    =',sqrt(sqrt((ust*ust)/(ustk2 + epsil))),
     &                          ust,sqrt(ustk2)
                write (lp,'(i9,2i5,a,f12.5,1p2e12.4,f12.5)')
     &            nstep,i+i0,j+j0,
     &            '   flang  =',flang,ucube,wcube,cw
              endif !ldebug_wscale
*
      endif !langmr
#endif
c
      return
      end
c
c
c> Revision history:
c>
c> Jun  2000 - conversion to SI units.
c> Jul  2000 - included wscale in this file to facilitate in-lining
c> May  2002 - buoyfl (into the atmos.), calculated here
c> Nov  2002 - added kPAR based turbidity
c> Nov  2002 - hmonob,mixflx,buoflx,bhtflx saved for diagnostics
c> Mar  2003 - added GISS mixed layer
c> May  2003 - added bldmin and bldmax to KPP
c> Jan  2004 - added latdiw to KPP and MY
c> Jan  2004 - added bblkpp to KPP (bottom boundary layer)
c> Jan  2004 - cv can now depend on bouyancy freqency
c> Jan  2004 - added hblflg to KPP
c> Mar. 2004 - added thkriv river support
c> Mar  2004 - updated hblflg for KPP
c> Mar. 2005 - added [ts]ofset
c> Dec. 2008 - difsmo now a layer count
c> Feb. 2009 - modified latdiw to use array diwlat
c> Oct. 2009 - KPP bug fix for nbl==klist case
c> Mar. 2010 - added diwbot
c> Apr. 2010 - botdiw based on Decloedt&Luther in KPP&MY, removed latdiw
c> May  2010 - KPP bug fix for bottom turbulent heat flux
c> May  2010 - bug fix in mxkprfcij[uv] for nlayer=kk
c> Oct  2010 - replaced two calls to dsiglocdX with one call at mid-point
c> July 2013 - added diws and diwm
c> Aug. 2013 - added langmr
c> Oct. 2013 - added jerlv0=-1 and calls to swfrac_ij and swfrml_ij
c> Jan. 2014 - homogenize the bottom 10 cm velocities
c> May  2014 - use land/sea masks (e.g. ip) to skip land
c> Sep  2014 - added 3 new options for langmr
