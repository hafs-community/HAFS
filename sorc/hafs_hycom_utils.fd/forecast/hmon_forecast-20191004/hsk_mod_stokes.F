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
      module mod_stokes
      use mod_xc  ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
c
      implicit none
c
c --- HYCOM Stokes Drift from external data files
c
      logical, parameter, private :: debug_stokes=.false.  !usually .false.
c
c     Logical flags for Stokes Drift Effects
c     The modification of the turbulent viscosity near the surface 
c     has been implemented  following:
c          - Craig & Banner + Ardhuin-Filipot-Perenne
c          - Uchiyama-McWilliams-Shchepetkin
c
      logical, save, public ::
     & stdflg,   ! Stokes Drift Velocities:           TRUE/FALSE
     & stdsur,   ! Stokes Drift Surface Stresses:     TRUE/FALSE (dissipation due to breaking and rolling)
     & stdbot,   ! Stokes Drift Bottom Stress:        TRUE/FALSE (dissipation due to bottom friction)
     & stdarc    ! Stokes Drift Velocities in Archive TRUE/FALSE
c
c    Constant values
c     Number of fixed interface depths for Stokes input
c
      integer,   save, public   ::
     &  nsdzi,   ! Number of fixed interface depths for Stokes input 
     & langmr    ! Langmuir turb enhancement (KPP) 0: None 1:McWilliams-Sulliva 2:Smyth 3:McWilliams-Harcourt 4:Takaya

c     Arrays holding Surface Stokes Velocities

      real,    allocatable, dimension(:,:),
     &         save, public  ::
     & usds,    ! Surface Stokes Drift U Velocity, p-grid
     & vsds     ! Surface Stokes Drift V Velocity, p-grid

c     -U- & -V- grid Arrays holding Vertically averaged Stokes Velocities

      real,    allocatable, dimension(:,:),
     &         save, public  ::
     & usdbavg,    ! Vertical average Stokes Drift U Velocity, u-grid
     & vsdbavg     ! Vertical average Stokes Drift V Velocity, v-grid

c     Arrays holding Stokes Drift Velocities on U and V grids

      real,    allocatable, dimension(:,:,:),
     &         save, public  ::
     & usd,    ! Stokes Drift U Velocity, u-grid
     & vsd     ! Stokes Drift V Velocity, v-grid

c     Arrays holding Stokes Drift Velocities on -p- grids

      real,    allocatable, dimension(:,:,:),
     &         save, public  ::
     & usdp,    ! Stokes Drift U Velocity, p-grid
     & vsdp     ! Stokes Drift V Velocity, p-grid


c     Arrays to hold Input Stokes Drift Velocity

      real,    allocatable, dimension(:),
     &         save, private ::
     & sdzi       ! Input Stokes Drift layer depths

      real,    allocatable, dimension(:,:,:,:),
     &         save, private ::
     & usdz,    ! Stokes Drift U Velocity for fixed layers, p-grid
     & vsdz     ! Stokes Drift V Velocity for fixed layers, p-grid

c     Array holding the wave-induced mean pressure

      real,    allocatable, dimension(:,:,:),
     &         save, public  ::
     & sj     ! wave-induced mean pressure on pressure grid
c     Arrays holding the horizontal derivatives of the vertical position of the middle of the layer
c
c      real,    allocatable, dimension(:,:,:),
c     &         save, public  ::
c     & dzdx,    ! derivative of z with respect to x
c     & dzdy     ! derivative of z with respect to y

c     Arrays used to calculate the dissipations

      real,    allocatable, dimension(:,:,:),
     &         save, public  ::
     & sdka,        ! frequency as a function ot time and space (=sdk)
     & wave_dir,    ! main wave propagation direction
     & h_sig,       ! significant wave height
     & eps_brk      ! wave breaking dissipation rate

c     Dissipations (bottom and breakin)

      real,    allocatable, dimension(:,:,:),
     &         save, public  ::
     & wave_bdx(:,:,:),     ! Bottom dissipation  
     & wave_bdy(:,:,:),     ! Bottom dissipation  
     & wave_brkx(:,:,:),     ! Dissipation  by breaking
     & wave_brky(:,:,:)      ! Dissipation  by breaking

c     Arrays used for the parameterization of the vertical mixing

      real,    allocatable, dimension(:,:,:),
     &         save, public  ::
     & phi_ocw,  ! Turbulent kinetic energy flux (notation from WW3)
     & z0topw,   ! wind sea significant height(if stz0tp=1) or surface roughness
     & ustarw3w, ! Velocity scale from WW3
     & difx_wave ! Vertical diffusion enhancement due to wave breaking (Uchiyama et al.)
      real,    allocatable, dimension(:,:),
     &         save, public  ::
     & phi_oc,   ! Turbulent kinetic energy flux (notation from WW3)
     & alpha,    ! Wave energy factor from WW3
     & z0top,    ! Surface roughness from WW3
     & ustarw3   ! Velocity scale from WW3
c

c    Weights for holding values during a barotropic time step.

      real,    save, public ::
     &  ws0,
     &  ws1


c    Stokes indexes
      integer,    save, public ::
     &  ls0,
     &  ls1

      contains 
c*********************************************************************************************
     

      subroutine stokes_set(dtime)
      implicit none
c
      real*8 dtime
c
c --- Stokes Drift velocity setup

      if     (mnproc.eq.1) then
      write(6,*)'=================================================='
      write(6,*)'  In mod_stokes.F  allocating arrays!'
      write(6,*)'nbdy,idm,jdm,kdm = ',nbdy,idm,jdm,kdm
      write(6,*)'=================================================='
      endif !1st tile

c
c --- Allocate Stokes Drift Velocity arrays used in rest of HYCOM
c
      allocate( usd(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm),
     &          vsd(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm))
      call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy)*kdm )
      usd(:,:,:) = 0.0
      vsd(:,:,:) = 0.0

      allocate( usdp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm),
     &          vsdp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm))
      call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy)*kdm )
      usdp(:,:,:) = 0.0
      vsdp(:,:,:) = 0.0

      allocate( usdbavg(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          vsdbavg(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy))
      call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy) )
      usdbavg(:,:) = 0.0
      vsdbavg(:,:) = 0.0

      allocate( usds(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          vsds(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy))
      call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy) )
      usds(:,:) = 0.0
      vsds(:,:) = 0.0

c      allocate( dzdx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm),
c     &          dzdy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm))
c      call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy)*kdm )
c      dzdx(:,:,:) = 0.0
c      dzdy(:,:,:) = 0.0

c
      ws0 = -99.0
      ws1 = -99.0

      if     (stdflg) then
        ls0 = 1
        ls1 = 2
c
c ---   Read in Stokes Drift profile arrays for current model start time
c
        allocate( sdzi(nsdzi) )

        allocate( usdz(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2,nsdzi),
     &            vsdz(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2,nsdzi))
        call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy)*2*nsdzi )
        usdz(:,:,:,:) = 0.0
        vsdz(:,:,:,:) = 0.0

        call stokes_forfun(dtime,1)

        if     (mnproc.eq.1) then
          write (lp,*) '...finished initializing Stokes Drift ',
     &                 'velocity Fields'
        endif !1st tile
        call xcsync(flush_lp)
      else
        nsdzi=0
        if     (mnproc.eq.1) then
          write (lp,*)'Stokes drift version of HYCOM called with ',
     &                'stdflg == 0'
          write (lp,*)'All Stokes Drift fields are set to zero!'
          write (lp,*)'No attempt is made to read WW3 Stokes Drift ',
     &                'Data'
        endif !1st tile
        call xcsync(flush_lp)
      endif     !stdflg:else
      return
      end subroutine stokes_set

      subroutine stokes_forfun(dtime,n)
      use mod_xc  ! HYCOM communication interface
      use mod_za  ! HYCOM I/O interface
      implicit none
c
      real*8    dtime
      integer   n
c
c     Stokes Drift Velocity Fields 
c
c --- input for fixed interface depths on the p-grid.
c
c --- I/O and array I/O units 927 and 928 are reserved for the entire run.
c
c --- all input fields much be defined at all grid points
c
      real*8    stime(927:928)
c
      real*8    dtime0,dtime1
      save      dtime0,dtime1
c
      real      scl1,scl2
      save      scl1,scl2
c
      character preambl(5)*79,clinex*80,cliney*80
      integer   i,ios,iunit,j,lgth,nrec,k,f,fmax
      logical   sdprnt
      real      dpthin,sum_u,sum_v,pzb,pzt
c
c --- ws0 negative on first call only.
      if     (ws0.lt.-1.0) then
c
c ---   initialize Stokes fields
c
c ---   open all stokes files.
c
        if     (mnproc.eq.1) then
        write (lp,*) ' now initializing Stokes Drift fields ...'
        endif !1st tile
        call xcsync(flush_lp)
c
        lgth = len_trim(flnmfor)
c
        call zaiopf(flnmfor(1:lgth)//'forcing.stokex.a', 'old', 927)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+927,file=flnmfor(1:lgth)//'forcing.stokex.b',
     &        status='old', action='read')
        read (uoff+927,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
c
        call zaiopf(flnmfor(1:lgth)//'forcing.stokey.a', 'old', 928)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+928,file=flnmfor(1:lgth)//'forcing.stokey.b',
     &        status='old', action='read')
        read (uoff+928,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
c
c ---   read in the Stokes Drift interface depths
c
        do k=1,nsdzi
          call zagetc(clinex,ios, uoff+927)
          if     (ios.ne.0) then
            if     (mnproc.eq.1) then
              write(lp,*)
              write(lp,*) 'error in stokes_forfun -' //
     &                    ' hit end of input forcing.stokex'
              write(lp,*) 'while reading the stokes depths'
              write(lp,*)
            endif !1st tile
            call xcstop('(stokes_forfun)')
                   stop '(stokes_forfun)'
          endif !ios
          call zagetc(cliney,ios, uoff+928)
          if     (ios.ne.0) then
            if     (mnproc.eq.1) then
              write(lp,*)
              write(lp,*) 'error in stokes_forfun -' //
     &                    ' hit end of input forcing.stokey'
              write(lp,*) 'while reading the stokes depths'
              write(lp,*)
            endif !1st tile
            call xcstop('(stokes_forfun)')
                   stop '(stokes_forfun)'
          endif !ios
c
          if     (clinex.ne.cliney) then
            if     (mnproc.eq.1) then
              write(lp,*)
              write(lp,*) 'error in stokes_forfun -' //
     &                    ' x and y depths are different'
              write(lp,*) 'for interface number ',k
              write(lp,'(a)')  trim(clinex)
              write(lp,'(a)')  trim(cliney)
              write(lp,*)
            endif !1st tile
            call xcstop('(stokes_forfun)')
                   stop '(stokes_forfun)'
          endif !clinex.ne.cliney
          read (clinex,*,iostat=ios) sdzi(k)
          if     (ios.ne.0) then
            if     (mnproc.eq.1) then
              write(lp,*)
              write(lp,*) 'error in stokes_forfun -' //
     &                    ' hit end of stokes depths in forcing.stokex'
              write(lp,*)
            endif !1st tile
            call xcstop('(stokes_forfun)')
                   stop '(stokes_forfun)'
          endif !ios
          if     (mnproc.eq.1) then
            write(lp,'(a)')  trim(clinex)
          endif !1st tile
          sdzi(k) = sdzi(k)*onem  !pressure units
          if     (sdzi(k).lt.sdzi(max(k-1,1))) then
            if     (mnproc.eq.1) then
            write(lp,*)
            write(lp,*) 'error in stokes_forfun -' //
     &                  ' Stokes depths must be non-decreasing'
            write(lp,*)
            endif !1st tile
            call xcstop('(stokes_forfun)')
                   stop '(stokes_forfun)'
          endif !zi.k<zi.k-1
        enddo  !k
        call xcsync(flush_lp)
c
        dpthin  = 0.001*onemm
        if     (sdzi(1).ne.0.0 .or. sdzi(2).lt.dpthin) then
          if     (mnproc.eq.1) then
            write(lp,*)
            write(lp,*) 'error in stokes_forfun -' //
     &                  ' 1st Stokes depth must be 0 and' //
     &                  ' 2nd must be positive'
            write(lp,*)
          endif !1st tile
          call xcstop('(stokes_forfun)')
                 stop '(stokes_forfun)'
        endif !sdzi(1:2)?
c
c ---   [uv]sdz.1 is exactly at the surface,
c ---   convert it to a very thin layer by stealing some of layer 2.
        sdzi(1) = dpthin
        scl2    = sdzi(2)/(sdzi(2)-dpthin)  !>1.0
        scl1    = scl2-1.0                  !positive, near zero
c
c ---   skip ahead to the start time.
c
        nrec   = 0
        dtime1 = huge(dtime1)
        do  ! infinite loop, with exit at end
          dtime0 = dtime1
          nrec   = nrec + 1
          do k=1,nsdzi
            call zagetc(clinex,ios, uoff+927)
            if     (ios.ne.0) then
              if     (mnproc.eq.1) then
              write(lp,*)
              write(lp,*) 'error in stokes_forfun -' //
     &                    ' hit end of input forcing.stokex'
              write(lp,*) 'dtime0,dtime1 = ',dtime0,dtime1
              write(lp,*) 'dtime = ',dtime
              write(lp,*)
              endif !1st tile
              call xcstop('(stokes_forfun)')
                     stop '(stokes_forfun)'
            endif !ios
            call zagetc(cliney,ios, uoff+928)
            if     (ios.ne.0) then
              if     (mnproc.eq.1) then
              write(lp,*)
              write(lp,*) 'error in stokes_forfun -' //
     &                    ' hit end of input forcing.stokey'
              write(lp,*) 'dtime0,dtime1 = ',dtime0,dtime1
              write(lp,*) 'dtime = ',dtime
              write(lp,*)
              endif !1st tile
              call xcstop('(stokes_forfun)')
                     stop '(stokes_forfun)'
            endif !ios
          enddo  !k
c
          i = index(clinex,'=')
          read (clinex(i+1:),*) dtime1
          if     (nrec.eq.1 .and. dtime1.lt.1462.0d0) then
c
c ---       must start after wind day 1462.0, 01/01/1905.
            if     (mnproc.eq.1) then
            write(lp,'(a)')  clinex
            write(lp,'(/ a,a / a,g15.6 /)')
     &        'error in stokes_forfun - actual forcing',
     &        ' must start after wind day 1462',
     &        'dtime1 = ',dtime1
            endif !1st tile
            call xcstop('(stokes_forfun)')
                   stop '(stokes_forfun)'
          endif !before wind day 1462.0
          if     (dtime0.le.dtime .and. dtime1.gt.dtime) then
            exit
          endif
        enddo   ! infinite loop, with exit above
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          rewind(unit=uoff+927)
          read (uoff+927,'(a79)') preambl
          do k=1,nsdzi
            read (uoff+927,'(a)') clinex
          enddo !k
          rewind(unit=uoff+928)
          read (uoff+928,'(a79)') preambl
          do k=1,nsdzi
            read (uoff+928,'(a)') cliney
          enddo !k
        endif
        do iunit= 927,928
          do i= 1,nrec-2
            do k=1,nsdzi
              call skmonth(iunit)
            enddo
          enddo
        enddo
        do k=1,nsdzi
          sdprnt = .true.
*         sdprnt = mod(nstep,nsdzi).eq.k-1
          call rdpall1(usdz(1-nbdy,1-nbdy,1,k),stime(927),927,sdprnt)
          call rdpall1(vsdz(1-nbdy,1-nbdy,1,k),stime(928),928,sdprnt)
        enddo !k
        dtime0 = dtime1
        dtime1 = stime(927)
        do k=1,nsdzi
          sdprnt = .true.
*         sdprnt = mod(nstep,nsdzi).eq.k-1
          call rdpall1(usdz(1-nbdy,1-nbdy,1,k),stime(927),927,sdprnt)
          call rdpall1(vsdz(1-nbdy,1-nbdy,1,k),stime(928),928,sdprnt)
          call xctilr( usdz(1-nbdy,1-nbdy,1,k),1,2, nbdy,nbdy, halo_pv)
          call xctilr( vsdz(1-nbdy,1-nbdy,1,k),1,2, nbdy,nbdy, halo_pv)
        enddo !k
c ---   [uv]sdz.1 is exactly at the surface,
c ---   convert it to a very thin layer (modifies [uv]sdz.2).
        usdz(:,:,:,2) = scl2*usdz(:,:,:,2) - scl1*usdz(:,:,:,1)
        vsdz(:,:,:,2) = scl2*vsdz(:,:,:,2) - scl1*vsdz(:,:,:,1)
c
        dtime0 = dtime1
        dtime1 = stime(927)
      
        if     (mnproc.eq.1) then
        write (lp,*) 
        write (lp,*) ' dtime,dtime0,dtime1 = ',dtime,dtime0,dtime1
        write (lp,*) 
        write (lp,*) ' ...finished initializing Stokes Drift fields'
        endif !1st tile
        call xcsync(flush_lp)
      endif  ! initialization
c
      if     (dtime.gt.dtime1) then
c
c ---   get the next set of fields.
cdiag           if     (mnproc.eq.1) then
cdiag           write(lp,*) 'enter rdpall - ',time,dtime0,dtime1
cdiag           endif !1st tile
cdiag           call xcsync(flush_lp)
        do k=1,nsdzi
          sdprnt = mod(nstep,nsdzi).eq.k-1
          call rdpall1(usdz(1-nbdy,1-nbdy,1,k),stime(927),927,sdprnt)
          call rdpall1(vsdz(1-nbdy,1-nbdy,1,k),stime(928),928,sdprnt)
          call xctilr( usdz(1-nbdy,1-nbdy,2,k),1,1, nbdy,nbdy, halo_pv)
          call xctilr( vsdz(1-nbdy,1-nbdy,2,k),1,1, nbdy,nbdy, halo_pv)
        enddo !k
c ---   [uv]sdz.1 is exactly at the surface,
c ---   convert it to a very thin layer (modifies [uv]sdz.2).
        usdz(:,:,2,2) = scl2*usdz(:,:,2,2) - scl1*usdz(:,:,2,1)
        vsdz(:,:,2,2) = scl2*vsdz(:,:,2,2) - scl1*vsdz(:,:,2,1)
c
        dtime0 = dtime1
        dtime1 = stime(927)
cdiag           if     (mnproc.eq.1) then
cdiag           write(lp,*) ' exit rdpall1 - ',time,dtime0,dtime1
cdiag           endif !1st tile
cdiag           call xcsync(flush_lp)
      endif
c
c --- linear interpolation in time.
      ws0 = (dtime1-dtime)/(dtime1-dtime0)
      ws1 = 1.0 - ws0
      if     (debug_stokes) then
        if     (mnproc.eq.1) then
        write(lp,'(a,i2,f20.10,2f10.7)')
     &            'stokes_forfun - n,dtime,ws0,ws1 = ',
     &            n,dtime,ws0,ws1
        endif !1st tile
        call xcsync(flush_lp)
      endif !debug
c--------------------------------------------------------------------
c  Calculate the p-grid Stokes Drift Arrays
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c   1. First the Surface Velocity arrays for the Mixed Layer Theories
c
      do j=1-nbdy,jj+nbdy
        do i=1-nbdy,ii+nbdy
          usds(i,j) = ws0*usdz(i,j,1,1) +
     &                ws1*usdz(i,j,2,1)
          vsds(i,j) = ws0*vsdz(i,j,1,1) +
     &                ws1*vsdz(i,j,2,1)
        enddo !i
      enddo !j
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c   2. The -k- layer Average Velocity arrays at the -p- points
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
c   Now ensure  all Stokes Drift Velocity Fields are properly defined on Halos
c
      call xctilr(usd(    1-nbdy,1-nbdy,1),1,kk, 6,6, halo_uv)
      call xctilr(vsd(    1-nbdy,1-nbdy,1),1,kk, 6,6, halo_vv)
      call xctilr(usdbavg(1-nbdy,1-nbdy)  ,1, 1, 6,6, halo_uv)
      call xctilr(vsdbavg(1-nbdy,1-nbdy)  ,1, 1, 6,6, halo_vv)
      return
      end subroutine stokes_forfun

      subroutine stokes_vertical_j(n,j)
      use mod_xc  ! HYCOM communication interface
      use mod_za  ! HYCOM I/O interface
      implicit none
c
      integer   n,j
c
c --- --------------------------------------------
c --- interpolate Stokes in vertical, single j-row
c --- --------------------------------------------
c
      integer i,k
      real    dpthin
      logical lcm(nsdzi)      !use PCM for some layers?
      real    s1d(nsdzi,2),   !input Stokes fields
     &        f1d(kdm,  2),   !model Stokes fields
     &        c1d(nsdzi,2),   !interpolation coefficients
     &        dpi( nsdzi),    !input layer thicknesses, >= dpthin
     &        dprs(nsdzi),    !input layer thicknesses
     &        pres(nsdzi+1),  !input layer interfaces
     &        prsf(kdm+1)     !model layer interfaces
c
      dpthin = 0.001*onemm
      lcm(:) = .false.
c
      do i= 1,ii
        if (SEA_P) then
c ---     1-d existing arrays
          pres(1)=0.0
          do k=1,nsdzi
            s1d(k,1) = ws0*usdz(i,j,1,k) +
     &                 ws1*usdz(i,j,2,k)
            s1d(k,2) = ws0*vsdz(i,j,1,k) +
     &                 ws1*vsdz(i,j,2,k)
            pres(k+1)=min(sdzi(k),p(i,j,kk+1))
            dprs(k)  =pres(k+1)-pres(k)
            dpi( k)  =max(dprs(k),dpthin)
          enddo !k 1:nsdzi
          prsf(1)=0.0
          do k=1,kk
            prsf(k+1) = min(prsf(k)+dp(i,j,k,n),p(i,j,kk+1))
          enddo !k 1:kk
c ---     remap
          call hybgen_plm_coefs(s1d,     dpi, lcm,c1d,
     &                                   nsdzi,   2,dpthin)
          call hybgen_plm_remap(s1d,pres,dprs,    c1d,
     &                          f1d,prsf,nsdzi,kk,2,dpthin)
c ---     1-d new arrays
          do k=1,kk
            usdp(i,j,k) = f1d(k,1)
            vsdp(i,j,k) = f1d(k,2)
          enddo !k 1:kk
        endif !ip
      enddo !i
      return
      end subroutine stokes_vertical_j

c
c
c---------------------------------------------------------------------------------------------
c modified subroutine to set up 3D Stokes Drift velocities passed from a coupler
c by Hyun-Sook Kim @ 11/2016.
c
      subroutine stokes3d
      use mod_xc  ! HYCOM communication interface
      use mod_za  ! HYCOM I/O interface
      implicit none
c
      integer   n
c
c
c --- set up vertical profiles from the surface Stokes Drift velocities ---
c     by projecting the surface Stokes Drift to the vertical, using input wavelength
c
c  - designed for dynamic coupling between HYCOM and WW3
c
c    edited by Hyun-Sook Kim         6/25/2016
c------------------------------------------------------------------------------

c
      integer i,j,m, nbdy
      real lambda1, en, scl1, scl2, sum_u, sum_v, dpthin
c
      real, dimensions(:,:) :: sdx1, sdy1
c
      integer, parameter ::
     > nsdzi=10        ! Number of fixed interface depths for Stokes input

      data sdzi/0.,1.,3.,5.,7.,9.,12.,15.,20.,25./      ! fixed depth levels
c
c++++++++++++++
c
c  1) input parameters:
c        sdx1    - the surface Stokes Drift velocity east-components in 2D on p-grid
c        sdy1    - the surface Stokes Drift velocity north-components in 2D on p-grid
c        lambda1 - the wave lengths for the Stokes Drifts
c
c  2) output arguments:
c        usd(:,:,:)   - Stokes Drift 3D velocity east-components estimated by exponential with depth
c        vsd(:,:,:)   - Stokes Drift 3D velocity north-components estimated by exponential with depth
c
c
      integer   i,ios,iunit,j,lgth,nrec,k,f,fmax
      logical   sdprnt
      real      dpthin,sum_u,sum_v,pzb,pzt
c
c --- receive surface stokes drift (2D) from a coupler and 
c     produce profile at a given set of depths for each wavelength,
c     followed by adding all the components:
      usdz=0.0
      vsdz=0.0
      do m=1,5
        do j=1-nbdy,jj+nbdy
          do i=1-nbdy,ii+nbdy

          call OC_RECV_STKDFT(sdx1,sdy1,lambda1)
          call stokes_profile(sdx1,sdy1,lambda1,usdz1,vsdz1)
          usdz(i,j,:)=usdz(i,j,:)+usdz1(i,j,:)
          vsdz(i,j,:)=vsdz(i,j,:)+vsdz1(i,j,:)
      enddo
      enddo
      enddo
c
c--------------------------------------------------------------------
c  Calculate the p-grid Stokes Drift Arrays
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c   1. First the Surface Velocity arrays for the Mixed Layer Theories
c
      do j=1-nbdy,jj+nbdy
        do i=1-nbdy,ii+nbdy
          usds(i,j) = ws0*usdz(i,j,1) +
     &                ws1*usdz(i,j,2)
          vsds(i,j) = ws0*vsdz(i,j,1) +
     &                ws1*vsdz(i,j,2)
        enddo !i
      enddo !j
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c   2. The -k- layer Average Velocity arrays at the -p- points
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
c   Now ensure  all Stokes Drift Velocity Fields are properly defined on Halos
c
      call xctilr(usd(    1-nbdy,1-nbdy,1),1,kk, 6,6, halo_uv)
      call xctilr(vsd(    1-nbdy,1-nbdy,1),1,kk, 6,6, halo_vv)
      call xctilr(usdbavg(1-nbdy,1-nbdy)  ,1, 1, 6,6, halo_uv)
      call xctilr(vsdbavg(1-nbdy,1-nbdy)  ,1, 1, 6,6, halo_vv)
      return
      end subroutine stokes3d

      end module mod_stokes


c> May  2014 - use land/sea masks (e.g. ip) to skip land
c> Aug  2015 - added stdarc
