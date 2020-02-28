!  This is a collection of subroutines that are required to pass to/from WW3,
!  especially when it requires interpolation in vertical to pass a set of currents
!  for a depth of interest.
!
!  Author: Hyun-Sook Kim on 6/2/2016
!
c
      subroutine get_depcur(ucdp,vcdp,ntl,z4dpc,itype)
c ---
c     Extract a set of (u,v) at a depth of interest (z4dpc)
c     from layer2z.f using additional 4 levels (2 for above and below the z4dpc).
c     by Hyun-Sook Kim @6/9/2016
c
c ---  integer, parameter :: itype=1	! linear interpolation between layer interfaces

      use mod_cb_arrays 	! HYCOM saved arrays
      implicit none
c
      real, allocatable, dimension(:,:) :: ucdp, !  for ocean current at a depth (wdpth)
     >                                     vcdp
c
      real z4dpc	! depth of interest for currents to wave
c
      integer  ntl, i, j, k, mbdy, itype
      real     depthu0, dpu0, depthv0, dpv0
      real     depthu1, dpu1, depthv1, dpv1
c
      real, parameter :: bot=0.0
      real, parameter :: flag = 2.0**100
c
      integer, parameter :: kz=5 ! additional two sample depths to a depth of interest
c
      real,  allocatable, dimension (:) :: zz
c
      real,  allocatable, dimension (:,:,:) :: utilk
     >               utmp, vtmp
c
      allocate ( ucdp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      allocate ( vcdp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy) )
c
      allocate ( utmp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kz) )
      allocate ( vtmp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kz) )
c
      allocate ( utilk(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kk+1) )
c
      mbdy=6
      margin = mbdy-1
c
c     build a set of levels for interpolation
c
      allocate ( zz(kz) )
      zz=0.
      do k=-2,2
        zz(k)=z4dpc+k*z4dpc/4.
      enddo 
c
c --- -------------------
c --- u-velocity
c --- -------------------
c
      do k= 1,kk
        do j=1-margin,jj+margin
           do i=1-margin,ii+margin
              if (ip(i,j).ne.0) then  
                if     (dp(i,j,k,n).gt.0.1 .and.
     &                  p(i,j,k).lt.p(i,j,kk+1)-bot) then
c ---             flux form for better results from mean archives
                  if (ip(i-1,j).ne.0) then
                    depthu0 = min(p(i,j,kk+1), p(i-1,j,kk+1))
                    dpu0    = max(0.0,
     &                min(depthu0,0.5*(p(i,j,k+1)+p(i-1,j,k+1)))-
     &                min(depthu0,0.5*(p(i,j,k  )+p(i-1,j,k  ))))
                  else
                    dpu0    = dp(i,j,k,ntl)
                  endif
                  if (ip(i+1,j).ne.0) then
                    depthu1 = min(p(i,j,kk+1), p(i+1,j,kk+1))
                    dpu1    = max(0.0,
     &                min(depthu1,0.5*(p(i,j,k+1)+p(i+1,j,k+1)))-
     &                min(depthu1,0.5*(p(i,j,k  )+p(i+1,j,k  ))))
                  else
                    dpu1    = dp(i,j,k,ntl)
                  endif
                  utilk(i,j,k)=(dpu0*u(i,  j,k,ntl)+
     &                        dpu1*u(i+1,j,k,ntl) )/
     &                        max(2.0*dp(i,j,k,ntl),dpu0+dpu1)
                else
                  utilk(i,j,k)=utilk(i,j,k-1)
                endif ! dp 
              else
                utilk(i,j,k)=flag
              endif
            enddo
        enddo
      enddo
      call layer2z(utilk,p,utmp,zz,flag,ii,jj,kk,kz,itype)
c
c --- -------------------
c --- v-velocity
c --- -------------------
c
      do k= 1,kk
        do j=1-margin,jj+margin
           do i=1-margin,ii+margin
             if (ip(i,j).ne.0) then  
               if     (dp(i,j,k,n).gt.0.1 .and.
     &                  p(i,j,k).lt.p(i,j,kk+1)-bot) then
c ---             flux form for better results from mean archives
                  if (ip(i,j-1).ne.0) then
                    depthv0 = min(p(i,j,kk+1), p(i,j-1,kk+1))
                    dpv0    = max(0.0,
     &                min(depthv0,0.5*(p(i,j,k+1)+p(i,j-1,k+1)))-
     &                min(depthv0,0.5*(p(i,j,k  )+p(i,j-1,k  ))))
                  else
                    dpv0    = dp(i,j,k,ntl)
                  endif
                  if (ip(i,j+1).ne.0) then
                    depthv1 = min(p(i,j,kk+1), p(i,j+1,kk+1))
                    dpv1    = max(0.0,
     &                min(depthv1,0.5*(p(i,j,k+1)+p(i,j+1,k+1)))-
     &                min(depthv1,0.5*(p(i,j,k  )+p(i,j+1,k  ))))
                  else
                    dpv1    = dp(i,j,k,ntl)
                  endif
                  utilk(i,j,k)=(dpv0*v(i,j,  k,ntl)+
     &                         dpv1*v(i,j+1,k,ntl) )/
     &                         max(2.0*dp(i,j,k,ntl),dpv0+dpv1)
                else
                  utilk(i,j,k)=utilk(i,j,k-1)
               endif
              else
                utilk(i,j,k)=flag
             endif
           enddo
        enddo
      enddo
      call layer2z(utilk,p,vtmp,zz,flag,ii,jj,kk,kz,itype)
c
      ucdp=utmp(:,:,3)
      vcdp=vtmp(:,:,3)
     
      return
      end

c
c
c --- set up vertical profiles from the surface Stokes Drift velocities ---
c
c    project the surface Stokes Drift to the vertical
c
c    edited by Hyun-Sook Kim         6/25/2016
c------------------------------------------------------------------------------
c -- build Stokes Drift velocity profiles
c    based on mean-wave length and a set of velocity
c  - designed for dynamic coupling between HYCOM and WW3
c
c     Hyun-Sook Kim @June 2016

      subroutine stokes_profile(wlen)

      use mod_cb_array
      use mod_stokes
      implicit none
c
      integer i,j,k, nbdy
      real wlen, scl1, scl2, sum_u, sum_v, dpthin
      real pzb, pzt
c
c      real, dimensions(:,:) :: usd2, vsd2
c
      integer, parameter ::
c     > nsdzi=10,       ! Number of fixed interface depths for Stokes input
     > n=1              ! Time level
c
c     data sdzi/0.,1.,3.,5.,7.,9.,12.,15.,20.,25./      ! fixed depth levels
c
c++++++++++++++
c
c  1) input parameters:
c        usds   - surface Stokes Drift velocity east-components in 2D on u/v-grid
c        vsds   - surface Stokes Drift velocity north-components in 2D on u/v-grid
c        mnwlen - wave length associated with Stokes Drift
c
c  2) output arguments:
c        usdz(:,:,1,:)   - Stokes Drift 3D velocity east-components estimated by exponential with depth
c        vsdz(:,:,1,:)   - Stokes Drift 3D velocity north-components estimated by exponential with depth
c
      sdzi(1)=0.
      sdzi(2)=1.
      sdzi(3)=3.
      sdzi(4)=5.
      sdzi(5)=7.
      sdzi(6)=9.
      sdzi(7)=12.
      sdzi(8)=15.
      sdzi(9)=20.
      sdzi(10)=25.      ! fixed depth levels
c
      nbdy=6
c
      allocate( usdz(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2,nsdzi),
     &            vsdz(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2,nsdzi))
      call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy)*2*nsdzi )
      usdz(:,:,1,:) = 0.0
      vsdz(:,:,1,:) = 0.0
c
      allocate ( sdzi(nsdzi) )
c
      do k=1,nsdzi
        do j= 1-nbdy,jj+nbdy
        do i= 1-nbdy,ii+nbdy
           if(usd2(i,j).ne.cflag)usdz(i,j,1,k)=
     & usd2(i,j)*exp(-1*wlen(i,j)*p(i,j,k))
           if(vsd2(i,j).ne.cflag)vsdz(i,j,1,k)=
     & vsd2(i,j)*exp(-1*wlen(i,j)*p(i,j,k))
        enddo
        enddo
      enddo
c
      dpthin = 0.001*onemm
      if (sdzi(1).ne.0. .or. sdzi(2).lt.dpthin) then
            write(lp,*)
            write(lp,*) 'error in stokes_prof -' //
     &                  ' 1st Stokes depth must be 0 and' //
     &                  ' 2nd must be positive'
            write(lp,*)
          call xcstop('(stokes_prof)')
                 stop '(stokes_prof)'
      endif !1st tile
c
c ---   [uv]sdz.1 is exactly at the surface,
c ---   convert it to a very thin layer by stealing some of layer 2.
      sdzi(1) = dpthin
      scl2    = sdzi(2)/(sdzi(2)-dpthin)  !>1.0
      scl1    = scl2-1.0                  !positive, near zero
      usdz(:,:,1,2)=scl2*usdz(:,:,1,2)-scl1*usdz(:,:,1,1)
      vsdz(:,:,1,2)=scl2*vsdz(:,:,1,2)-scl1*vsdz(:,:,1,1)
c
c   The -k- layer Average Velocity arrays at the -p- points
c
!$OMP   PARALLEL DO PRIVATE(j)
!$OMP&               SHARED(n)
!$OMP&           SCHEDULE(STATIC,jblk)
      do j= 1,jj
        call stokes_vertical_j(n,j)
      enddo !j
!$OMP END PARALLEL DO
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  All Stokes Drift Velocity Arrays are now on 'private' -p-  grids
c  Now calculate the U velocity components on -U- grids
c                and V velocity Components on -V- grids
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
      call xctilr(usdp(1-nbdy,1-nbdy,1),1,kk, 1,1, halo_pv)
      call xctilr(vsdp(1-nbdy,1-nbdy,1),1,kk, 1,1, halo_pv)
c
!$OMP PARALLEL DO PRIVATE(j,i,k,sum_u,sum_v)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        do i= 1,ii
          if (SEA_U) then
            sum_u = 0.0
            do k=1,kk
              usd(i,j,k)=0.5*(usdp(i,j,k)+usdp(i-1,j,k))
              sum_u = sum_u + usd(i,j,k)*dpu(i,j,k,n)
            enddo !k
            usdbavg(i,j) = sum_u/depthu(i,j)
          endif !iu
        enddo !i
        do i= 1,ii
          if (SEA_V) then
            sum_v = 0.0
            do k=1,kk
              vsd(i,j,k)=0.5*(vsdp(i,j,k)+vsdp(i,j-1,k))
              sum_v = sum_v + vsd(i,j,k)*dpv(i,j,k,n)
            enddo !k
            vsdbavg(i,j) = sum_v/depthv(i,j)
          endif !iv
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
      if     (debug_stokes) then
 103    format (i9,2i5,a)
 104    format (30x,i3,2f8.4,f9.3,f9.2)
        if (itest.gt.0 .and. jtest.gt.0) then
          write (lp,103) nstep,itest+i0,jtest+j0,
     .    '  stokes_forfun:  usdz    vsdz    thkns     dpth'
          pzb = 0.0
          do k= 1,nsdzi
            pzt = pzb
            pzb = min(sdzi(k),p(itest,jtest,kk+1))*qonem
            write (lp,104)
     .      k,
     .      ws0*usdz(itest,jtest,1,k)+ws1*usdz(itest,jtest,2,k),
     .      ws0*vsdz(itest,jtest,1,k)+ws1*vsdz(itest,jtest,2,k),
     .      pzb-pzt,pzb
            if     (pzt.eq.p(itest,jtest,kk+1)*qonem) then
              exit
            endif
          enddo !k
c
          write (lp,103) nstep,itest+i0,jtest+j0,
     .    '  stokes_forfun:  usdp    vsdp     thkns     dpth'
          pzb = 0.0
          do k= 1,kk
            pzt = pzb
            pzb = min(pzt+dp(itest,jtest,k,n)*qonem,
     &                    p(itest,jtest,kk+1)*qonem)
            write (lp,104)
     .      k,
     .      usdp(itest,jtest,k),vsdp(itest,jtest,k),
     .      pzb-pzt,pzb
            if     (pzt.eq.p(itest,jtest,kk+1)*qonem) then
              exit
            endif
          enddo !k
c
          write (lp,103) nstep,itest+i0,jtest+j0,
     .    '  stokes_forfun:   usd     vsd     thkns     dpth'
          write (lp,104)
     .    0,usdbavg(itest,jtest),vsdbavg(itest,jtest),
     .    0.0,p(itest,jtest,kk+1)*qonem
          pzb = 0.0
          do k= 1,kk
            pzt = pzb
            pzb = min(pzt+dp(itest,jtest,k,n)*qonem,
     &                    p(itest,jtest,kk+1)*qonem)
            write (lp,104)
     .      k,
     .      usd(itest,jtest,k),vsd(itest,jtest,k),
     .      pzb-pzt,pzb
            if     (pzt.eq.p(itest,jtest,kk+1)*qonem) then
              exit
            endif
          enddo !k
        endif !test
      endif !debug
c
c
c   Now ensure  all Stokes Drift Velocity Fields are properly defined on Halos
c
      call xctilr(usd(    1-nbdy,1-nbdy,1),1,kk, 6,6, halo_uv)
      call xctilr(vsd(    1-nbdy,1-nbdy,1),1,kk, 6,6, halo_vv)
      call xctilr(usdbavg(1-nbdy,1-nbdy)  ,1, 1, 6,6, halo_uv)
      call xctilr(vsdbavg(1-nbdy,1-nbdy)  ,1, 1, 6,6, halo_vv)
c
      return
      end subroutine stokes_profile

