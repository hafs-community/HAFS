c
c --- set up vertical profiles from the surface Stokes Drift velocities ---
c
c    project the surface Stokes Drift to the vertical
c
c    edited by Hyun-Sook Kim         6/25/2016
c------------------------------------------------------------------------------
c -- build Stokes Drift velocity profiles
c    for a set of 5 (u,v) and wavelengths.
c
c -- designed for dynamic coupling between HYCOM and WW3
c
c     Modified by Hyun-Sook Kim 10-28-2016
c

      subroutine stokes_profile(usdsfc,vsdsfc,wlen,usd,vsd)
c     
c --- output: real,allocatable,dimension(:,:,:):: usd, vsd (same as in mod_stokes.F)
c     input:  real,allocatable,dimension(:,:):: usdsfc,vsdsfc (usds/vsds in mod_stokes.F)
c
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

