      subroutine geopar
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
#if defined(USE_CCSM3)
      use ccsm3_grid, only : ANGLET
#endif
c
c --- set up model parameters related to geography
c
c --- hycom version 2.1
c
      implicit none
c
      real      dp0kf,dpm,dpms,ds0kf,dsm,dsms
      real      hmina,hminb,hmaxa,hmaxb
      real*8    sum_ip,sum_is,sum_isa
      integer   i,ios,j,k,ktr,l,nishlf
      character preambl(5)*79,cline*80
#if defined(USE_CCSM3)
      real      plinei(itdm),plinej(jtdm)
      save      plinei,plinej
#endif
c
      real       aspmax
      parameter (aspmax=2.0)  ! maximum grid aspect ratio for diffusion
*     parameter (aspmax=1.0)  ! ignore  grid aspect ratio in  diffusion
c
c --- read grid location,spacing,coriolis arrays
c
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
        write (lp,'(3a)') ' reading grid file from ',
     &                         trim(flnmgrd),'.[ab]'
        open (unit=uoff+9,file=trim(flnmgrd)//'.b',
     &        status='old')
      endif
      call xcsync(flush_lp)
      call zagetc(cline,ios, uoff+9)
      if     (ios.ne.0) then
        if     (mnproc.eq.1) then
          write(lp,'(/ a,i4,i9 /)')
     &      'geopar: I/O error from zagetc, iunit,ios = ',uoff+9,ios
        endif !1st tile
        call xcstop('(geopar)')
               stop '(geopar)'
      endif
      read(cline,*) i
c
      call zagetc(cline,ios, uoff+9)
      if     (ios.ne.0) then
        if     (mnproc.eq.1) then
          write(lp,'(/ a,i4,i9 /)')
     &      'geopar: I/O error from zagetc, iunit,ios = ',uoff+9,ios
        endif !1st tile
        call xcstop('(geopar)')
               stop '(geopar)'
      endif
      read (cline,*) j
c
      if     (i.ne.itdm .or. j.ne.jtdm) then
        if     (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - wrong array size in grid file'
        endif
        call xcstop('(geopar)')
               stop '(geopar)'
      endif
      call zagetc(cline,ios, uoff+9)
      if     (ios.ne.0) then
        if     (mnproc.eq.1) then
          write(lp,'(/ a,i4,i9 /)')
     &      'geopar: I/O error from zagetc, iunit,ios = ',uoff+9,ios
        endif !1st tile
        call xcstop('(geopar)')
               stop '(geopar)'
      endif
      if     (mnproc.eq.1) then
      write (lp,'(a)') trim(cline)
      endif
      read (cline,*) mapflg
c
      call zaiopf(trim(flnmgrd)//'.a','old', 9)
c
      do k= 1,15
        call zagetc(cline,ios, uoff+9)
        if     (ios.ne.0) then
          if     (mnproc.eq.1) then
            write(lp,'(/ a,i4,i9 /)')
     &        'geopar: I/O error from zagetc, iunit,ios = ',uoff+9,ios
          endif !1st tile
          call xcstop('(geopar)')
                 stop '(geopar)'
        endif
        i = index(cline,'=')
        read (cline(i+1:),*) hminb,hmaxb
        if     (mnproc.eq.1) then
        write (lp,'(a)') trim(cline)
        endif
        call xcsync(flush_lp)
c
        if     (k.eq.1) then
          call zaiord(plon, ip,.false., hmina,hmaxa, 9)
        elseif (k.eq.2) then
          call zaiord(plat, ip,.false., hmina,hmaxa, 9)
          do i= 1,2  !skip qlon,qlat
            call zagetc(cline,ios, uoff+9)
            if     (ios.ne.0) then
              if     (mnproc.eq.1) then
                write(lp,'(/ a,i4,i9 /)')
     &            'geopar: I/O error from zagetc, iunit,ios = ',
     &            uoff+9,ios
              endif !1st tile
              call xcstop('(geopar)')
                     stop '(geopar)'
            endif
            call zaiosk(9)
          enddo
        elseif (k.eq.3) then
          call zaiord(ulon, ip,.false., hmina,hmaxa, 9)
        elseif (k.eq.4) then
          call zaiord(ulat, ip,.false., hmina,hmaxa, 9)
        elseif (k.eq.5) then
          call zaiord(vlon, ip,.false., hmina,hmaxa, 9)
        elseif (k.eq.6) then
          call zaiord(vlat, ip,.false., hmina,hmaxa, 9)
          call zagetc(cline,ios, uoff+9)
          if     (ios.ne.0) then
            if     (mnproc.eq.1) then
              write(lp,'(/ a,i4,i9 /)')
     &          'geopar: I/O error from zagetc, iunit,ios = ',uoff+9,ios
            endif !1st tile
            call xcstop('(geopar)')
                   stop '(geopar)'
          endif
#if defined(USE_CCSM3)
c         pang in ANGLET
          i = index(cline,'=')
          read (cline(i+1:),*) hminb,hmaxb
          if     (mnproc.eq.1) then
          write (lp,'(a)') trim(cline)
          endif
          call xcsync(flush_lp)
          call zaiord(ANGLET, ip,.false., hmina,hmaxa, 9)
#elif defined(ENABLE_ATM)
c         pang
          i = index(cline,'=')
          read (cline(i+1:),*) hminb,hmaxb
          if     (mnproc.eq.1) then
          write (lp,'(a)') trim(cline)
          endif
          call xcsync(flush_lp)
          call zaiord(pang, ip,.false., hmina,hmaxa, 9)
#else
c         skip pang
          call zaiosk(9)
#endif
        elseif (k.eq.7) then
          call zaiord(scpx, ip,.false., hmina,hmaxa, 9)
        elseif (k.eq.8) then
          call zaiord(scpy, ip,.false., hmina,hmaxa, 9)
        elseif (k.eq.9) then
          call zaiord(scqx, iq,.false., hmina,hmaxa, 9)
        elseif (k.eq.10) then
          call zaiord(scqy, iq,.false., hmina,hmaxa, 9)
        elseif (k.eq.11) then
          call zaiord(scux, iu,.false., hmina,hmaxa, 9)
        elseif (k.eq.12) then
          call zaiord(scuy, iu,.false., hmina,hmaxa, 9)
        elseif (k.eq.13) then
          call zaiord(scvx, iv,.false., hmina,hmaxa, 9)
        elseif (k.eq.14) then
          call zaiord(scvy, iv,.false., hmina,hmaxa, 9)
        else
          call zaiord(corio,iq,.false., hmina,hmaxa, 9)
        endif
c
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          endif
          call xcstop('(geopar)')
                 stop '(geopar)'
        endif
      enddo
c
      call zaiocl(9)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
        close(unit=uoff+9)
      endif
c
      if (itest.gt.0 .and. jtest.gt.0) then
        i=itest
        j=jtest
        write (lp,'(/ a,2i5,a,f8.3,a,f12.9,2f10.2/)')
     &   ' i,j=',i+i0,j+j0,
     &   ' plat=',plat(i,j),
     &   ' corio,scux,vy=',corio(i,j),scux(i,j),scvy(i,j)
      endif
      call xcsync(flush_lp)
#if defined(USE_CCSM3)
c --- printout similar to ccsm ice model
      call xclget(plinei,itdm, plon, 1,1, +1, 0, 1)
      call xclget(plinej,jtdm, plat, 1,1,  0,+1, 1)
      if     (mnproc.eq.1) then
        write (lp,*)
        write (lp,'(a,4f9.3,a,4f9.3)')
     &    '(domain) plon(:,1): ',
     &    plinei(1:4),' ...', plinei(itdm-2:itdm)
        write (lp,'(a,4f9.3,a,4f9.3)')
     &    '(domain) plat(1,:): ',
     &    plinej(1:4),' ...', plinej(jtdm-2:jtdm)
        write (lp,*)
      endif
      call xcsync(flush_lp)
#endif
c
c --- read basin depth array
c
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
        write (lp,'(3a)') ' reading bathymetry file from ',
     &                         trim(flnmdep),'.[ab]'
        open (unit=uoff+9,file=trim(flnmdep)//'.b',
     &        status='old')
        read (     uoff+9,'(a79)')  preambl
      endif
      call xcsync(flush_lp)
      call zagetc(cline,ios, uoff+9)
      if     (ios.ne.0) then
        if     (mnproc.eq.1) then
          write(lp,'(/ a,i4,i9 /)')
     &      'geopar: I/O error from zagetc, iunit,ios = ',uoff+9,ios
        endif !1st tile
        call xcstop('(geopar)')
               stop '(geopar)'
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)   hminb,hmaxb
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
        close(unit=uoff+9)
        write (lp,'(/(1x,a))') preambl,cline
      endif
c
      call zaiopf(trim(flnmdep)//'.a','old', 9)
      call zaiord(depths,ip,.false., hmina,hmaxa, 9)
      call zaiocl(9)
c
      if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &        abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
        if     (mnproc.eq.1) then
        write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &    'error - .a and .b files not consistent:',
     &    '.a,.b min = ',hmina,hminb,hmina-hminb,
     &    '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
        endif
        call xcstop('(geopar)')
               stop '(geopar)'
      endif
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j= 1,jj
        do i= 1,ii
          if     (depths(i,j).gt.0.5*hugel) then
            depths(i,j) = 0.0
          endif
        enddo
      enddo
c
c --- determine do-loop limits for u,v,p,q points, and update halo for depths
      call bigrid(depths, mapflg, util1,util2,util3)
ccc      call prtmsk(ip,depths,util1,idm,ii,jj,0.0,1.0,
ccc     &     'bottom depth (m)')
c
c     now safe to apply halo to arrays.
c
      vland = 1.0
      call xctilr(plon,  1,1, nbdy,nbdy, halo_ps)
      call xctilr(plat,  1,1, nbdy,nbdy, halo_ps)
#if defined(USE_CCSM3)
      call xctilr(ANGLET,1,1, nbdy,nbdy, halo_ps)
#endif
#if defined(ENABLE_ATM)
      call xctilr(pang,  1,1, nbdy,nbdy, halo_ps)
#endif
      call xctilr(scpx,  1,1, nbdy,nbdy, halo_ps)
      call xctilr(scpy,  1,1, nbdy,nbdy, halo_ps)
      call xctilr(ulon,  1,1, nbdy,nbdy, halo_us)
      call xctilr(ulat,  1,1, nbdy,nbdy, halo_us)
      call xctilr(scux,  1,1, nbdy,nbdy, halo_us)
      call xctilr(scuy,  1,1, nbdy,nbdy, halo_us)
      call xctilr(vlon,  1,1, nbdy,nbdy, halo_vs)
      call xctilr(vlat,  1,1, nbdy,nbdy, halo_vs)
      call xctilr(scvx,  1,1, nbdy,nbdy, halo_vs)
      call xctilr(scvy,  1,1, nbdy,nbdy, halo_vs)
      call xctilr(corio, 1,1, nbdy,nbdy, halo_qs)
      call xctilr(scqx,  1,1, nbdy,nbdy, halo_qs)
      call xctilr(scqy,  1,1, nbdy,nbdy, halo_qs)
      vland = 0.0
c
c --- area of grid cells (length x width) at u,v,p,q points resp.
c
******!$OMP PARALLEL DO PRIVATE(j,i)
******!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-nbdy,jj+nbdy
        do i=1-nbdy,ii+nbdy
          scu2(i,j)=scux(i,j)*scuy(i,j)
          scv2(i,j)=scvx(i,j)*scvy(i,j)
          scp2(i,j)=scpx(i,j)*scpy(i,j)
          scq2(i,j)=scqx(i,j)*scqy(i,j)
c
          scuxi(i,j)=1.0/max(scux(i,j),epsil)
          scvyi(i,j)=1.0/max(scvy(i,j),epsil)
          scp2i(i,j)=1.0/max(scp2(i,j),epsil)
          scq2i(i,j)=1.0/max(scq2(i,j),epsil)
c
c ---     largest grid spacing (within limits) used in all diffusion
c ---     coefficients: min(max(sc?x,sc?y),sc?x*aspmax,sc?y*aspmax)
          aspux(i,j)=min(max(scux(i,j),scuy(i,j)),
     &                   min(scux(i,j),scuy(i,j))*aspmax)
     &               /max(scux(i,j),epsil)
          aspuy(i,j)=min(max(scux(i,j),scuy(i,j)),
     &                   min(scux(i,j),scuy(i,j))*aspmax)
     &               /max(scuy(i,j),epsil)
          aspvx(i,j)=min(max(scvx(i,j),scvy(i,j)),
     &                   min(scvx(i,j),scvy(i,j))*aspmax)
     &               /max(scvx(i,j),epsil)
          aspvy(i,j)=min(max(scvx(i,j),scvy(i,j)),
     &                   min(scvx(i,j),scvy(i,j))*aspmax)
     &               /max(scvy(i,j),epsil)
c
          util1(i,j)=depths(i,j)*scp2(i,j)
        enddo
      enddo
c
c --- read ice shelf depth array
c
      if     (ishelf.eq.0) then
        ishlf(:,:) = ip(:,:)  !no ice shelf
      else
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          write (lp,'(3a)') ' reading ice shelf file from ',
     &                           trim(flnmshlf),'.[ab]'
          open (unit=uoff+9,file=trim(flnmshlf)//'.b',
     &          status='old')
          read (     uoff+9,'(a79)')  preambl
        endif
        call xcsync(flush_lp)
        call zagetc(cline,ios, uoff+9)
        if     (ios.ne.0) then
          if     (mnproc.eq.1) then
            write(lp,'(/ a,i4,i9 /)')
     &        'geopar: I/O error from zagetc, iunit,ios = ',uoff+9,ios
          endif !1st tile
          call xcstop('(geopar)')
                 stop '(geopar)'
        endif
        i = index(cline,'=')
        read (cline(i+1:),*)   hminb,hmaxb
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          close(unit=uoff+9)
          write (lp,'(/(1x,a))') preambl,cline
        endif
c
        call zaiopf(trim(flnmshlf)//'.a','old', 9)
        call zaiord(util3,ip,.false., hmina,hmaxa, 9)
        call zaiocl(9)
c
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          endif
          call xcstop('(geopar)')
                 stop '(geopar)'
        endif
c
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j= 1,jj
          do i= 1,ii
            if     (ip(i,j).eq.0) then
              util3(i,j) = 0.0  !land
            elseif (util3(i,j).gt.0.5*hugel) then
              util3(i,j) = 0.0  !ice shelf over ocean
            elseif (util3(i,j).le.0.0) then
              util3(i,j) = 0.0  !ice shelf over ocean
            else
              util3(i,j) = 1.0  !open ocean
            endif
          enddo
        enddo
        call xctilr(util3,1,1, nbdy,nbdy, halo_ps)
        ishlf(:,:) = 0  !for jj:jdm and ii:idm
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j= 1-nbdy,jj+nbdy
          do i= 1-nbdy,ii+nbdy
            ishlf(i,j) = util3(i,j)
            util2(i,j) = ip(i,j)
          enddo
        enddo
c
        call xcsum(sum_is,  util3,ip)
        call xcsum(sum_ip,  util2,ip)
        call xcsum(sum_isa, scp2, ishlf)
        call xcsum(area,    scp2, ip)
        nishlf = nint(sum_ip) - nint(sum_is)
        if     (mnproc.eq.1) then
        write (lp,'(/a,i9,f10.2)')
     &         ' number of ice shelf points and area (10^6 km^2):',
     &         nishlf,(area-sum_isa)*1.d-12
        endif
        call xcsync(flush_lp)
      endif !ishelf
c
c --- In arctic (tripole) domain, top row of mass points is redundent,
c ---  so always use ipa, based on ishlf, for mass sums
#if defined(ARCTIC)
      ipa(:,:) = ishlf(:,:)
      if     (jj+j0.eq.jtdm) then
c ---   mask top row of mass points
        ipa(:,jj:jj+nbdy) = 0
      endif
#else
c --- Not a tripole domain, so ipa=ishlf
      ipa(:,:) = ishlf(:,:)
#endif
c
      call xcsum(avgbot, util1,ipa)
      call xcsum(area,   scp2, ipa)
      avgbot=avgbot/area
      if     (mnproc.eq.1) then
      write (lp,'(/a,f9.1,f10.2)')
     &       ' mean basin depth (m) and area (10^6 km^2):',
     &       avgbot,area*1.e-12
      endif
      call xcsync(flush_lp)
c
c --- calculate dp0k and ds0k?
      if     (dp00.lt.0.0) then
c ---   dp0k and ds0k already input
        dp00 =onem*dp0k(1)
        dp00x=onem*dp0k(kk-1)
        dp00i=onem*dp00i
        dpms = 0.0
        do k=1,kk
          dpm     = dp0k(k)
          dpms    = dpms + dpm
          dp0k(k) = dp0k(k)*onem
          if     (mnproc.eq.1) then
          write(lp,135) k,dp0k(k)*qonem,dpm,dpms
          endif
          if     (mnproc.eq.-99) then  ! bugfix that prevents optimization
            write(6,*) 'geopar: dp0k   = ',dp0k(k),k,mnproc
          endif
          call xcsync(flush_lp)
        enddo !k
        dsms = 0.0
        do k=1,nsigma
          dsm     = ds0k(k)
          dsms    = dsms + dsm
          ds0k(k) = ds0k(k)*onem
          if     (mnproc.eq.1) then
          write(lp,130) k,ds0k(k)*qonem,dsm,dsms
          endif
          if     (mnproc.eq.-99) then  ! bugfix that prevents optimization
            write(6,*) 'geopar: ds0k   = ',ds0k(k),k,mnproc
          endif
          call xcsync(flush_lp)
        enddo !k
        if     (mnproc.eq.1) then
        write(lp,*)
        endif
      else
c ---   calculate dp0k and ds0k
c
c ---   logorithmic k-dependence of dp0 (deep z's)
        dp00 =onem*dp00
        dp00x=onem*dp00x
        dp00i=onem*dp00i
        if     (isopyc) then
          dp0k(1)=thkmin*onem
        else
          dp0k(1)=dp00
        endif
        dpm  = dp0k(1)*qonem
        dpms = dpm
        if     (mnproc.eq.1) then
        write(lp,*)
        write(lp,135) 1,dp0k(1)*qonem,dpm,dpms
        endif
 135    format('dp0k(',i2,') =',f7.2,' m',
     &            '    thkns =',f7.2,' m',
     &            '    depth =',f8.2,' m')
        call xcsync(flush_lp)
c
        dp0kf=1.0
        do k=2,kk
          dp0kf=dp0kf*dp00f
          if     (k.le.nhybrd) then
            if     (dp00f.ge.1.0) then
              dp0k(k)=min(dp00*dp0kf,dp00x)
            else
              dp0k(k)=max(dp00*dp0kf,dp00x)
            endif
          else
            dp0k(k)=0.0
          endif
          dpm  = dp0k(k)*qonem
          dpms = dpms + dpm
          if     (mnproc.eq.1) then
          write(lp,135) k,dp0k(k)*qonem,dpm,dpms
          endif
          if     (mnproc.eq.-99) then  ! bugfix that prevents optimization
            write(6,*) 'geopar: dp0kf  = ',dp0kf,    mnproc
            write(6,*) 'geopar: dp0k   = ',dp0k(k),k,mnproc
          endif
          call xcsync(flush_lp)
        enddo !k
c
c ---   logorithmic k-dependence of ds0 (shallow z-s)
        ds00 =onem*ds00
        ds00x=onem*ds00x
        if     (isopyc) then
          ds0k(1)=thkmin*onem
        else
          ds0k(1)=ds00
        endif
        dsm  = ds0k(1)*qonem
        dsms = dsm
        if     (mnproc.eq.1) then
        write(lp,*)
        write(lp,130) 1,ds0k(1)*qonem,dsm,dsms
        endif
 130    format('ds0k(',i2,') =',f7.2,' m',
     &            '    thkns =',f7.2,' m',
     &            '    depth =',f8.2,' m')
        call xcsync(flush_lp)
c
        ds0kf=1.0
        do k=2,nsigma
          ds0kf=ds0kf*ds00f
          if     (ds00f.ge.1.0) then
            ds0k(k)=min(ds00*ds0kf,ds00x)
          else
            ds0k(k)=max(ds00*ds0kf,ds00x)
          endif
          dsm  = ds0k(k)*qonem
          dsms = dsms + dsm
          if     (mnproc.eq.1) then
          write(lp,130) k,ds0k(k)*qonem,dsm,dsms
          endif
          if     (mnproc.eq.-99) then  ! bugfix that prevents optimization
            write(6,*) 'geopar: ds0kf  = ',ds0kf,    mnproc
            write(6,*) 'geopar: ds0k   = ',ds0k(k),k,mnproc
          endif
          call xcsync(flush_lp)
        enddo !k
        if     (mnproc.eq.1) then
        write(lp,*)
        endif
      endif !input:calculate dp0k,ds0k
c
c --- start and stop depths for terrain following coordinate
      if     (nsigma.eq.0) then
        dpns    = dp0k(1)
        dsns    = 0.0
        ds0k(1) = dp0k(1)
        do k= 2,kk
          ds0k(k)=0.0
        enddo !k
      else
        dpns = 0.0
        dsns = 0.0
        do k=1,nsigma
          dpns = dpns + dp0k(k)
          dsns = dsns + ds0k(k)
        enddo !k
        do k= nsigma+1,kk
          ds0k(k)=0.0
        enddo !k
      endif !nsigma
      dpns = dpns*qonem  !depths is in m
      dsns = dsns*qonem  !depths is in m
c
      if     (mnproc.eq.1) then
      write(lp,131) nsigma,dpns,dsns
      endif
 131  format('nsigma = ',i2,
     &       '    deep    =',f8.2,' m',
     &       '    shallow =',f8.2,' m' )
      call flush(lp)
c
c --- initialize thermobaric reference state arrays.
c
      if     (kapref.eq.-1) then
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          write (lp,'(3a)') ' reading thermobaric reference file from ',
     &                           trim(flnmforw), 'tbaric.[ab]'
          open (unit=uoff+9,file=trim(flnmforw)//'tbaric.b',
     &          status='old')
          read (     uoff+9,'(a79)')  preambl
        endif
        call xcsync(flush_lp)
        call zagetc(cline,ios, uoff+9)
        if     (ios.ne.0) then
          if     (mnproc.eq.1) then
            write(lp,'(/ a,i4,i9 /)')
     &        'geopar: I/O error from zagetc, iunit,ios = ',uoff+9,ios
          endif !1st tile
          call xcstop('(geopar)')
                 stop '(geopar)'
        endif
        i = index(cline,'=')
        read (cline(i+1:),*)   hminb,hmaxb
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          close(unit=uoff+9)
          write (lp,'(/(1x,a))') preambl,cline
        endif
c
c ---   input field is between 1.0 and 3.0 and indicates the
c ---   relative strength of the two nearest reference states,
c ---     e.g. 1.7 is 70% ref2 and 30% ref1
c ---     and  2.3 is 70% ref2 and 30% ref3.
c
        call zaiopf(trim(flnmforw)//'tbaric.a','old', 9)
        call zaiord(util1,ip,.false., hmina,hmaxa, 9)
        call zaiocl(9)
c
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          endif
          call xcstop('(geopar)')
                 stop '(geopar)'
        endif
c
        do j= 1,jj
          do i= 1,ii
            if     (ip(i,j).eq.0) then
              util1(i,j) = 1.0 !land
            endif
          enddo
        enddo
c
        vland = 1.0
        call xctilr(util1,  1,1, nbdy,nbdy, halo_ps)
        vland = 0.0
c
c       kapi is the 2nd reference state (1st is always 2)
c       skap is the scale factor (0.0-1.0) for the 1st reference state
c
c       assumes that reference states 1 and 3 are never next to each other.
c
        do j= 1,jj
          do i= 1,ii
            if     (max(util1(i,  j),
     &                  util1(i-1,j),
     &                  util1(i+1,j),
     &                  util1(i,  j-1),
     &                  util1(i,  j+1) ).gt.2.0) then
              util2(i,j) = 3.0              !kapi
               skap(i,j) = 3.0 - util1(i,j)
            else
              util2(i,j) = 1.0              !kapi
               skap(i,j) = util1(i,j) - 1.0
            endif
          enddo
        enddo
        vland = 1.0
        call xctilr(util2, 1,1, nbdy,nbdy, halo_ps)
        call xctilr(skap,  1,1, nbdy,nbdy, halo_ps)
        vland = 0.0
c
        kapi(:,:) = util2(:,:)
      else
        skap(:,:) = 1.0     !for diagnostics only
        kapi(:,:) = kapref  !for diagnostics only
      endif !kapref.eq.-1:else
c
c --- initialize some arrays
c --- set depthu,dpu,utotn,pgfx,depthv,dpv,vtotn,pgfy to zero everywhere,
c --- so that they can be used at "lateral neighbors" of u and v points.
c --- similarly for pbot,dp at neighbors of q points.
c
      disp_count=0
c
!$OMP PARALLEL DO PRIVATE(j,i,k,ktr)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-nbdy,jj+nbdy
        do i=1-nbdy,ii+nbdy
          p(     i,j,1)=0.0
          pu(    i,j,1)=0.0
          pv(    i,j,1)=0.0
          utotn( i,j)=0.0
          vtotn( i,j)=0.0
          pgfx(  i,j)=0.0
          pgfy(  i,j)=0.0
          gradx( i,j)=0.0
          grady( i,j)=0.0
          depthu(i,j)=0.0
          depthv(i,j)=0.0
          pbot(  i,j)=0.0
c
          displd_mn(i,j)=0.0
          dispqd_mn(i,j)=0.0
          tidepg_mn(i,j)=0.0
c
          psikk( i,j,1)=0.0
          psikk( i,j,2)=0.0
          thkk(  i,j,1)=0.0
          thkk(  i,j,2)=0.0
c
          ubavg( i,j,1)=hugel
          ubavg( i,j,2)=hugel
          ubavg( i,j,3)=hugel
          vbavg( i,j,1)=hugel
          vbavg( i,j,2)=hugel
          vbavg( i,j,3)=hugel
          utotm( i,j)=hugel
          vtotm( i,j)=hugel
          uflux( i,j)=hugel
          vflux( i,j)=hugel
          uflux1(i,j)=hugel
          vflux1(i,j)=hugel
          uflux2(i,j)=hugel
          vflux2(i,j)=hugel
          uflux3(i,j)=hugel
          vflux3(i,j)=hugel
          uja(   i,j)=hugel
          ujb(   i,j)=hugel
          via(   i,j)=hugel
          vib(   i,j)=hugel
          do k=1,kk
            dp( i,j,k,1)=0.0
            dp( i,j,k,2)=0.0
            dpu(i,j,k,1)=0.0
            dpu(i,j,k,2)=0.0
            dpv(i,j,k,1)=0.0
            dpv(i,j,k,2)=0.0
c
            u(  i,j,k,1)=hugel
            u(  i,j,k,2)=hugel
            v(  i,j,k,1)=hugel
            v(  i,j,k,2)=hugel
c
            uflx(  i,j,k)=hugel
            vflx(  i,j,k)=hugel
c
            dpav(  i,j,k)=0.0
            uflxav(i,j,k)=0.0
            vflxav(i,j,k)=0.0
            diaflx(i,j,k)=0.0
c
            do ktr= 1,ntracr
              tracer(i,j,k,1,ktr)=0.0
              tracer(i,j,k,2,ktr)=0.0
            enddo
          enddo
        enddo
      enddo
!$OMP END PARALLEL DO
c
!$OMP PARALLEL DO PRIVATE(j,l,i,k)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        do l=1,isp(j) !ok
          do i=max(1,ifp(j,l)),min(ii,ilp(j,l)+1)
            ubavg(i,j,1)=0.0
            ubavg(i,j,2)=0.0
            ubavg(i,j,3)=0.0
            utotm (i,j)=0.0
            uflux (i,j)=0.0
            uflux2(i,j)=0.0
            uflux3(i,j)=0.0
            uja(i,j)=0.0
            ujb(i,j)=0.0
c
            do k=1,kk
              uflx(i,j,k)=0.0
              u(i,j,k,1)=0.0
              u(i,j,k,2)=0.0
            enddo
          enddo
        enddo
      enddo
c
      call xctilr(ubavg,    1,   3, nbdy,nbdy, halo_us)  ! note scalar
      call xctilr(utotm,    1,   1, nbdy,nbdy, halo_us)  ! note scalar
      call xctilr(uflux,    1,   1, nbdy,nbdy, halo_us)  ! note scalar
      call xctilr(uflux2,   1,   1, nbdy,nbdy, halo_us)  ! note scalar
      call xctilr(uflux3,   1,   1, nbdy,nbdy, halo_us)  ! note scalar
      call xctilr(uja,      1,   1, nbdy,nbdy, halo_us)
      call xctilr(ujb,      1,   1, nbdy,nbdy, halo_us)
      call xctilr(uflx,     1,  kk, nbdy,nbdy, halo_us)  ! note scalar
      call xctilr(u,        1,2*kk, nbdy,nbdy, halo_us)  ! note scalar
c
!$OMP PARALLEL DO PRIVATE(i,l,j,k)
!$OMP&         SCHEDULE(STATIC)
      do i=1,ii
        do l=1,jsp(i) !ok
          do j=max(1,jfp(i,l)),min(jj,jlp(i,l)+1)
            vbavg(i,j,1)=0.0
            vbavg(i,j,2)=0.0
            vbavg(i,j,3)=0.0
            vtotm (i,j)=0.0
            vflux (i,j)=0.0
            vflux2(i,j)=0.0
            vflux3(i,j)=0.0
            via(i,j)=0.0
            vib(i,j)=0.0
c
            do k=1,kk
              vflx(i,j,k)=0.0
              v(i,j,k,1)=0.0
              v(i,j,k,2)=0.0
            enddo
          enddo
        enddo
      enddo
c
      call xctilr(vbavg,    1,   3, nbdy,nbdy, halo_vs)  ! note scalar
      call xctilr(vtotm,    1,   1, nbdy,nbdy, halo_vs)  ! note scalar
      call xctilr(vflux,    1,   1, nbdy,nbdy, halo_vs)  ! note scalar
      call xctilr(vflux2,   1,   1, nbdy,nbdy, halo_vs)  ! note scalar
      call xctilr(vflux3,   1,   1, nbdy,nbdy, halo_vs)  ! note scalar
      call xctilr(via,      1,   1, nbdy,nbdy, halo_vs)  ! note scalar
      call xctilr(vib,      1,   1, nbdy,nbdy, halo_vs)  ! note scalar
      call xctilr(vflx,     1,  kk, nbdy,nbdy, halo_vs)  ! note scalar
      call xctilr(v,        1,2*kk, nbdy,nbdy, halo_vs)  ! note scalar
c
      return
      end
c
c
c> Revision history:
c>
c> May  1997 - extended list of variables set to 'hugel' on land
c> Oct. 1999 - added code that defines the vertical distribution of dp0
c>             used in hybgen
c> Jan. 2000 - added mapflg logic for different projections
c> Feb. 2000 - added dp00f for logorithmic z-level spacing
c> Mar. 2000 - added dp00s for sigma-spacing in shallow water
c> May  2000 - conversion to SI units (still wrong corio)
c> Feb. 2001 - removed rotated grid option
c> Jan. 2002 - more flexible Z-sigma-Z vertical configuration
c> Jan. 2002 - all grids now via array input
c> Sep. 2004 - define kapi and skap for thermobaricity
c> Oct. 2008 - dp0k and ds0k can now be input, see blkdat.F
c> Mar. 2012 - replaced dssk with dpns and dsns
c> Apr. 2014 - added ishlf
c> Apr. 2014 - added ipa
c> Feb. 2015 - added pang for coupled cases
