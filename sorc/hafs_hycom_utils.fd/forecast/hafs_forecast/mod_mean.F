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
      module mod_mean
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
#if defined(STOKES)
      use mod_stokes     ! HYCOM Stokes drift
#endif
c
      implicit none
c
c --- HYCOM time means
c
      real*8,  save, public  :: 
     &   time_min,  !start  of averaging interval, set by mean_zero
     &   time_ave,  !middle of averaging interval, set by mean_end
     &   time_max   !end    of averaging interval, set by mean_end
c
      integer, save, private :: 
     &   nmean    ! mean sum counter
      real,    save, private :: 
     &   snmean   ! mean sum counter
c
      real,    allocatable, dimension(:,:,:,:),
     &         save, private ::
     &   tracer_m
c
      real,    allocatable, dimension(:,:,:),
     &         save, private ::
     &   u_m,v_m,ke_m,temp_m,saln_m,th3d_m,dp_m
#if defined(STOKES)
     &  ,usdp_m,vsdp_m
#endif
c
      real,    allocatable, dimension(:,:),
     &         save, private ::
     &   ubaro_m,vbaro_m,kebaro_m,
     &   montg_m,srfht_m,steric_m,dpbl_m,dpmixl_m,
     &   surflx_m,salflx_m, covice_m,thkice_m,temice_m

      contains

      subroutine mean_allocate
c
c --- Allocate mean fields.
c
      if     (ntracr.gt.0) then
        allocate( tracer_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,ntracr) )
        call mem_stat_add(       (idm+2*nbdy)*(jdm+2*nbdy)*kdm*ntracr )
      endif
c
      allocate(      u_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm) )
      allocate(      v_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm) )
      allocate(     ke_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm) )
      allocate(   temp_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm) )
      allocate(   saln_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm) )
      allocate(   th3d_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm) )
      allocate(     dp_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm) )
      call mem_stat_add(     7*(idm+2*nbdy)*(jdm+2*nbdy)*kdm )
#if defined(STOKES)
      if     (stdarc) then
        allocate(   usdp_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm) )
        allocate(   vsdp_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm) )
        call mem_stat_add(     2*(idm+2*nbdy)*(jdm+2*nbdy)*kdm )
      endif !stdarc
#endif
c
      allocate(  ubaro_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      allocate(  vbaro_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      allocate( kebaro_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      allocate(  montg_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      allocate( steric_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )  !not always output
      allocate(  srfht_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      allocate(   dpbl_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      allocate( dpmixl_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      allocate( surflx_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      allocate( salflx_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      allocate( covice_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      allocate( thkice_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      allocate( temice_m(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add(     13*(idm+2*nbdy)*(jdm+2*nbdy) )
c
      return
      end subroutine mean_allocate

      subroutine mean_zero(time_now)
c
      real*8 time_now
c
c --- Zero all mean fields
c
      integer i,j,k,ktr
c
      snmean   = 0.0
      nmean    = 0
      time_min = time_now
c
!$OMP PARALLEL DO PRIVATE(j,i,k,ktr)
      do j=1,jj
        do i=1,ii
          kebaro_m(i,j) = 0.0
           montg_m(i,j) = 0.0
          steric_m(i,j) = 0.0
           srfht_m(i,j) = 0.0
            dpbl_m(i,j) = 0.0
          dpmixl_m(i,j) = 0.0
          surflx_m(i,j) = 0.0
          salflx_m(i,j) = 0.0
          covice_m(i,j) = 0.0
          thkice_m(i,j) = 0.0
          temice_m(i,j) = 0.0
           ubaro_m(i,j) = 0.0
           vbaro_m(i,j) = 0.0
        enddo !i
        do k= 1,kk
          do i=1,ii
              dp_m(i,j,k) = 0.0
            temp_m(i,j,k) = 0.0
            saln_m(i,j,k) = 0.0
            th3d_m(i,j,k) = 0.0
              ke_m(i,j,k) = 0.0
               u_m(i,j,k) = 0.0
               v_m(i,j,k) = 0.0
            do ktr= 1,ntracr
              tracer_m(i,j,k,ktr) = 0.0
            enddo !ktr
#if defined(STOKES)
            if     (stdarc) then
              usdp_m(i,j,k) = 0.0
              vsdp_m(i,j,k) = 0.0
            endif !stdarc
#endif
          enddo !i
        enddo !k
      enddo !j
c
      return
      end subroutine mean_zero

      subroutine mean_add(n, s)
c
      integer n
      real    s
c
c --- Add to mean fields
c --- s is 1.0 or 0.5
c
      integer i,j,k,ktr
      real    q,ke
c
      snmean = snmean + s
      nmean  = nmean  + 1
c
c --- assume dp,dpu,dpv are up to date
c --- halos only needed for kinetic energy
c
      call xctilr(u(      1-nbdy,1-nbdy,1,n),1,kk, 1,1, halo_uv)
      call xctilr(ubavg(  1-nbdy,1-nbdy,  n),1, 1, 1,1, halo_uv)
      call xctilr(v(      1-nbdy,1-nbdy,1,n),1,kk, 1,1, halo_vv)
      call xctilr(vbavg(  1-nbdy,1-nbdy,  n),1, 1, 1,1, halo_vv)
c
!$OMP PARALLEL DO PRIVATE(j,i,k,ktr,q,ke)
      do j=1,jj
        do i=1,ii
          if (SEA_P) then
                       ke = 0.5*((0.5*(ubavg(i,  j,n) +
     &                                 ubavg(i+1,j,n)  ))**2 +
     &                           (0.5*(vbavg(i,j,  n) +
     &                                 vbavg(i,j+1,n)  ))**2  )
            kebaro_m(i,j) = kebaro_m(i,j) + s*ke
             montg_m(i,j) =  montg_m(i,j) + s*montg1(i,j)
            steric_m(i,j) = steric_m(i,j) + s*steric(i,j)
             srfht_m(i,j) =  srfht_m(i,j) + s*srfhgt(i,j)
              dpbl_m(i,j) =   dpbl_m(i,j) + s*  dpbl(i,j)
            dpmixl_m(i,j) = dpmixl_m(i,j) + s*dpmixl(i,j,n)
            surflx_m(i,j) = surflx_m(i,j) + s*surflx(i,j)
            salflx_m(i,j) = salflx_m(i,j) + s*salflx(i,j)
            covice_m(i,j) = covice_m(i,j) + s*covice(i,j)
            thkice_m(i,j) = thkice_m(i,j) + s*thkice(i,j)
            temice_m(i,j) = temice_m(i,j) + s*temice(i,j)
          endif !ip
          if (SEA_U) then
            ubaro_m(i,j) = ubaro_m(i,j) + s*ubavg(i,j,n)
          endif !iu
          if (SEA_V) then
            vbaro_m(i,j) = vbaro_m(i,j) + s*vbavg(i,j,n)
          endif !ip
        enddo !i
        do k= 1,kk
          do i=1,ii
            if (SEA_P) then
              ke = 0.5*((0.5*(u(i,  j,k,n) + ubavg(i,  j,n) +
     &                        u(i+1,j,k,n) + ubavg(i+1,j,n)  ))**2 +
     &                  (0.5*(v(i,j  ,k,n) + vbavg(i,j,  n) +
     &                        v(i,j+1,k,n) + vbavg(i,j+1,n)  ))**2  )
c
                          q =                   s*dp(i,j,k,n)
                dp_m(i,j,k) =   dp_m(i,j,k) +                   q
              temp_m(i,j,k) = temp_m(i,j,k) +   temp(i,j,k,n) * q
              saln_m(i,j,k) = saln_m(i,j,k) +   saln(i,j,k,n) * q
              th3d_m(i,j,k) = th3d_m(i,j,k) +   th3d(i,j,k,n) * q
                ke_m(i,j,k) =   ke_m(i,j,k) +     ke          * q
              do ktr= 1,ntracr
                tracer_m(i,j,k,ktr) = tracer_m(i,j,k,  ktr) +
     &                                  tracer(i,j,k,n,ktr) * q
              enddo !ktr
#if defined(STOKES)
              if     (stdarc) then
                usdp_m(i,j,k) = usdp_m(i,j,k) +   usdp(i,j,k) * q
                vsdp_m(i,j,k) = vsdp_m(i,j,k) +   vsdp(i,j,k) * q
              endif !stdarc
#endif
            endif !ip
            if (SEA_U) then
                       q = s*dpu(i,j,k,n)
              u_m(i,j,k) = u_m(i,j,k) + q*(u(i,j,k,n) + ubavg(i,j,n))
            endif !iu
            if (SEA_V) then
                       q = s*dpv(i,j,k,n)
              v_m(i,j,k) = v_m(i,j,k) + q*(v(i,j,k,n) + vbavg(i,j,n))
            endif !iv
          enddo !i
        enddo !k
      enddo !j
c
      return
      end subroutine mean_add

      subroutine mean_end(time_now)
c
      real*8 time_now
c
c --- Reduce sums to their mean.
c
      integer i,j,k,ktr
      real    dpthin,q,qdp
      real*8  time_int
c
      q        = 1.0/snmean
      time_max = time_now
      time_ave = 0.5*(time_min + time_max)
      if     (nint(snmean).ne.nmean) then
c ---   1st and last sample scaled by 1/2.
        nmean    = nint(snmean)
        time_int = (time_max - time_min)*q
        time_min = time_min + 0.5*time_int
        time_max = time_max - 0.5*time_int
      endif
c
      dpthin   = 0.001*onemm
c
!$OMP PARALLEL DO PRIVATE(j,i,k,qdp)
      do j=1,jj
        do i=1,ii
          if (SEA_P) then
            p(i,j,1) = 0.0
            do k= 1,kk
              p(i,j,k+1) = p(i,j,k) + q*dp_m(i,j,k)
            enddo !k
          endif !ip
        enddo !i
      enddo !j
c
      call xctilr(p(1-nbdy,1-nbdy,2),1,kk, 1,1, halo_ps)
c
!$OMP PARALLEL DO PRIVATE(j,i,k,qdp)
      do j=1,jj
        do i=1,ii
          if (SEA_P) then
            kebaro_m(i,j) = kebaro_m(i,j) * q
             montg_m(i,j) =  montg_m(i,j) * q
            steric_m(i,j) = steric_m(i,j) * q
             srfht_m(i,j) =  srfht_m(i,j) * q
              dpbl_m(i,j) =   dpbl_m(i,j) * q
            dpmixl_m(i,j) = dpmixl_m(i,j) * q
            surflx_m(i,j) = surflx_m(i,j) * q
            salflx_m(i,j) = salflx_m(i,j) * q
            covice_m(i,j) = covice_m(i,j) * q
            thkice_m(i,j) = thkice_m(i,j) * q
            temice_m(i,j) = temice_m(i,j) * q
          endif !ip
          if (SEA_U) then
            ubaro_m(i,j) = ubaro_m(i,j) * q
          endif !iu
          if (SEA_V) then
            vbaro_m(i,j) = vbaro_m(i,j) * q
          endif !iv
        enddo !i
        do k= 1,kk
          do i=1,ii
            if (SEA_P) then
              dp_m(i,j,k) = dp_m(i,j,k) * q
              if     (dp_m(i,j,k).ge.dpthin) then
                          qdp = q/dp_m(i,j,k)
                temp_m(i,j,k) = temp_m(i,j,k) * qdp
                saln_m(i,j,k) = saln_m(i,j,k) * qdp
                th3d_m(i,j,k) = th3d_m(i,j,k) * qdp
                  ke_m(i,j,k) =   ke_m(i,j,k) * qdp
                do ktr= 1,ntracr
                  tracer_m(i,j,k,ktr) = tracer_m(i,j,k,  ktr) * qdp
                enddo !ktr
#if defined(STOKES)
                if     (stdarc) then
                  usdp_m(i,j,k) = usdp_m(i,j,k) * qdp
                  vsdp_m(i,j,k) = vsdp_m(i,j,k) * qdp
                endif !stdarc
#endif
              else  !project into zero thickness layers
                temp_m(i,j,k) = temp_m(i,j,k-1)
                saln_m(i,j,k) = saln_m(i,j,k-1)
                th3d_m(i,j,k) = th3d_m(i,j,k-1)
                  ke_m(i,j,k) =   ke_m(i,j,k-1)
                do ktr= 1,ntracr
                  tracer_m(i,j,k,ktr) = tracer_m(i,j,k-1,ktr)
                enddo !ktr
#if defined(STOKES)
                if     (stdarc) then
                  usdp_m(i,j,k) = usdp_m(i,j,k-1)
                  vsdp_m(i,j,k) = vsdp_m(i,j,k-1)
                endif !stdarc
#endif
              endif !dpthin:else
            endif !ip
            if (SEA_U) then
              qdp = min(depthu(i,j), 0.5*(p(i,j,k+1)+p(i-1,j,k+1))) -
     &              min(depthu(i,j), 0.5*(p(i,j,k  )+p(i-1,j,k  )))
              if     (qdp.ge.dpthin) then
                qdp = q/qdp
                u_m(i,j,k) = u_m(i,j,k) * qdp
              else 
                u_m(i,j,k) = u_m(i,j,k-1)
              endif
            endif !iu
            if (SEA_V) then
              qdp = min(depthv(i,j), 0.5*(p(i,j,k+1)+p(i,j-1,k+1))) -
     &              min(depthv(i,j), 0.5*(p(i,j,k  )+p(i,j-1,k  )))
              if     (qdp.ge.dpthin) then
                qdp = q/qdp
                v_m(i,j,k) = v_m(i,j,k) * qdp
              else 
                v_m(i,j,k) = v_m(i,j,k-1)
              endif
            endif !iv
          enddo !i
        enddo !k
      enddo !j
c
      return
      end subroutine mean_end


#if defined(USE_CCSM3)
      subroutine mean_archiv(n, iyear,imonth,iday,ihour)
      use mod_za  ! HYCOM I/O interface
c
      integer n, iyear,imonth,iday,ihour
#else
      subroutine mean_archiv(n, iyear,iday,ihour)
      use mod_za  ! HYCOM I/O interface
c
      integer n, iyear,iday,ihour
#endif
c
c --- write a mean archive file.
c
      character*80 cformat
      integer      i,j,k,ktr,ldot,nop,nopa
      integer      itst1,jtst1
      real         ssha,sshn,sshs,sssc,sstc,ubpnt,upnt,vbpnt,vpnt
#if defined(STOKES)
      real         ustk,vstk
#endif
      real         coord,xmin,xmax

c
      ldot = index(flnmarcm,'.',back=.true.)
      if     (ldot.eq.0) then
        if     (mnproc.eq.1) then
        write (lp,*) 'need decimal point in flnmarcm'
        write (lp,*) 'flnmarcm = ',trim(flnmarcm)
        endif
        call xcstop('(flnmarcm)')
               stop '(flnmarcm)'
      endif
      ldot = min(ldot,len(flnmarcm)-11)  !need 11 characters for archive date
c
c --- indicate the archive date
#if defined(USE_CCSM3)
      if     (imonth.eq. 1) then
        write(flnmarcm(ldot+1:ldot+8),'(i4.4,a2,i2.2)')
     &    iyear-1,'_m',12
      else
        write(flnmarcm(ldot+1:ldot+8),'(i4.4,a2,i2.2)')
     &    iyear,  '_m',imonth-1
      endif
      ldot=ldot+8
#else
      write(flnmarcm(ldot+1:ldot+11),'(i4.4,a1,i3.3,a1,i2.2)') 
     &   iyear,'_',iday,'_',ihour
      ldot=ldot+11
#endif
      nopa=13
      nop =13+uoff
c
c --- no .[ab] files for 1-D cases (<=6x6).
c
      if     (max(itdm,jtdm).gt.6) then  !not 1-D output
c
      call zaiopf(flnmarcm(1:ldot)//'.a', 'new', nopa)
      if     (mnproc.eq.1) then
      open (unit=nop,file=flnmarcm(1:ldot)//'.b',status='new') !uoff+13
      write(nop,116) ctitle,iversn,iexpt,yrflag,itdm,jtdm
      call flush(nop)
      endif !1st tile
 116  format (a80/a80/a80/a80/
     & i5,4x,'''iversn'' = hycom version number x10'/
     & i5,4x,'''iexpt '' = experiment number x10'/
     & i5,4x,'''yrflag'' = days in year flag'/
     & i5,4x,'''idm   '' = longitudinal array size'/
     & i5,4x,'''jdm   '' = latitudinal  array size'/
     & 'field       time step   mean day',
     & '  k  dens        min              max')
c
c --- surface fields
c
      coord=0.
c
      call zaiowr(montg_m,ip,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'montg1  ',nmean,time_min,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(srfht_m,ip,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'srfhgt  ',nmean,time_max,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      if     (sshflg.ne.0) then
c ---   write out steric SSH.
        call zaiowr(steric_m,ip,.true.,
     &              xmin,xmax, nopa, .false.)
        if     (mnproc.eq.1) then
        write (nop,117) 'steric  ',nmean,time_ave,0,coord,xmin,xmax
        call flush(nop)
        endif !1st tile
      endif !sshflg
c
      call zaiowr(surflx_m,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'surflx  ',nmean,time_ave,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(salflx_m,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'salflx  ',nmean,time_ave,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
c
      call zaiowr(dpbl_m,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'bl_dpth ',nmean,time_ave,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(dpmixl_m,ip,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'mix_dpth',nmean,time_ave,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(temp_m,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'tmix    ',nmean,time_ave,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(saln_m,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'smix    ',nmean,time_ave,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(th3d_m,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'thmix   ',nmean,time_ave,0,thbase,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(u_m,iu,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'umix    ',nmean,time_ave,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(v_m,iv,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'vmix    ',nmean,time_ave,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(ke_m,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'kemix   ',nmean,time_ave,0,thbase,xmin,xmax
      call flush(nop)
      endif !1st tile
      if     (iceflg.ne.0) then
        call zaiowr(covice_m,ip,.true., xmin,xmax, nopa, .false.)
        if     (mnproc.eq.1) then
        write (nop,117) 'covice  ',nmean,time_ave,0,coord,xmin,xmax
        call flush(nop)
        endif !1st tile
        call zaiowr(thkice_m,ip,.true., xmin,xmax, nopa, .false.)
        if     (mnproc.eq.1) then
        write (nop,117) 'thkice  ',nmean,time_ave,0,coord,xmin,xmax
        call flush(nop)
        endif !1st tile
        call zaiowr(temice_m,ip,.true., xmin,xmax, nopa, .false.)
        if     (mnproc.eq.1) then
        write (nop,117) 'temice  ',nmean,time_ave,0,coord,xmin,xmax
        call flush(nop)
        endif !1st tile
      endif  !write ice fields
c
c --- depth averaged fields
c
      call zaiowr(ubaro_m,iu,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'u_btrop ',nmean,time_ave,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(vbaro_m,iv,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'v_btrop ',nmean,time_ave,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(kebaro_m,ip,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'kebtrop ',nmean,time_ave,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
c
c --- layer loop.
c
      do 75 k=1,kk
      coord=sigma(k)
      call zaiowr(u_m(1-nbdy,1-nbdy,k),iu,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'u-vel.  ',nmean,time_ave,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(v_m(1-nbdy,1-nbdy,k),iv,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'v-vel.  ',nmean,time_ave,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(ke_m(1-nbdy,1-nbdy,k),ip,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'k.e.    ',nmean,time_ave,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(dp_m(1-nbdy,1-nbdy,k),ip,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'thknss  ',nmean,time_ave,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(temp_m(1-nbdy,1-nbdy,k),ip,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'temp    ',nmean,time_ave,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(saln_m(1-nbdy,1-nbdy,k),ip,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'salin   ',nmean,time_ave,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(th3d_m(1-nbdy,1-nbdy,k),ip,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'density ',nmean,time_ave,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      do ktr= 1,ntracr
        call zaiowr(tracer_m(1-nbdy,1-nbdy,k,ktr),ip,.true.,
     &              xmin,xmax, nopa, .false.)
        if     (mnproc.eq.1) then
        write (nop,117) 'tracer  ',nmean,time_ave,k,coord,xmin,xmax
        call flush(nop)
        endif !1st tile
      enddo !ktr
#if defined(STOKES)
      if     (stdarc) then
        call zaiowr(usdp_m(1-nbdy,1-nbdy,k),ip,.true.,
     &              xmin,xmax, nopa, .false.)
        if     (mnproc.eq.1) then
        write (nop,117) 'tracer  ',nmean,time_ave,k,coord,xmin,xmax
        call flush(nop)
        endif !1st tile
        call zaiowr(vsdp_m(1-nbdy,1-nbdy,k),ip,.true.,
     &              xmin,xmax, nopa, .false.)
        if     (mnproc.eq.1) then
        write (nop,117) 'tracer  ',nmean,time_ave,k,coord,xmin,xmax
        call flush(nop)
        endif !1st tile
      endif !stdarc
#endif
 75   continue
c
 117  format (a8,' =',i11,f11.3,i3,f7.3,1p2e16.7)
c
      close (unit=nop)
      call zaiocl(nopa)
c
      call xcsync(no_flush)
c
      endif  !not 1-D
c
      if     (itest.gt.0 .and. jtest.gt.0) then
        open (unit=nop,file=flnmarcm(1:ldot)//'.txt',status='new') !uoff+13
        write (nop,'(3a / a,6i7,f9.3,f8.3,i7,i5.4,i4.3,i3.2)')
     &      '##   expt    idm    jdm    kdm',
     &        '   iloc   jloc   lonloc  latloc',
     &        ' yrflag year day hr',
     &      '##',iexpt,  itdm,  jtdm,   kdm,
     &          ittest,jttest,
     &          mod(plon(itest,jtest),360.0),plat(itest,jtest),
     &          yrflag, iyear,  iday, ihour
c
        ssha = srfht_m(itest,jtest)
        if     (sshflg.ne.0) then
          sshs = steric_m(itest,jtest)
        else
          sshs = ssha  !assume all is steric
        endif
        sshn = ssha - sshs
c
        if     (relaxf .and. sstflg.le.1) then
          sstc = twall(itest,jtest,1,lc0)*wc0+
     &           twall(itest,jtest,1,lc1)*wc1+
     &           twall(itest,jtest,1,lc2)*wc2+
     &           twall(itest,jtest,1,lc3)*wc3
        else !synoptic observed sst
          if     (natm.eq.2) then
            sstc = seatmp(itest,jtest,l0)*w0+
     &             seatmp(itest,jtest,l1)*w1
          else
            sstc = seatmp(itest,jtest,l0)*w0+
     &             seatmp(itest,jtest,l1)*w1+
     &             seatmp(itest,jtest,l2)*w2+
     &             seatmp(itest,jtest,l3)*w3
          endif !natm
        endif
        sssc = swall(itest,jtest,1,lc0)*wc0+
     &         swall(itest,jtest,1,lc1)*wc1+
     &         swall(itest,jtest,1,lc2)*wc2+
     &         swall(itest,jtest,1,lc3)*wc3
c
c ---   interpolate to the p-grid, but only if it requres no halo points
        itst1 = min(itest+1,ii)  !either itest+1 or itest
        jtst1 = min(jtest+1,jj)  !either jtest+1 or jtest
        ubpnt = 0.5*(ubaro_m(itest,jtest)  +ubaro_m(itst1,jtest))
        vbpnt = 0.5*(vbaro_m(itest,jtest)  +vbaro_m(itest,jtst1))
        upnt  = 0.5*(    u_m(itest,jtest,1)+    u_m(itst1,jtest,1))
        vpnt  = 0.5*(    v_m(itest,jtest,1)+    v_m(itest,jtst1,1))
c
c ---   order is not optimal, constrained by ALL/bin/hycom_profile_list
c ---   which only includes the fields up to vbaro (or up to nsterc)

        write (nop,'(6a)')
     &    '## model-day  srfhgt  surflx',
     &    '     dpbl   dpmixl    tmix    smix   thmix',
     &    '    umix    vmix   ubavg   vbavg  steric  nsterc',
     &    '   tclim   sclim',
     &    '  sswflx  mixflx  sstflx',
     &    '      E-P'
        write (nop,'(a,f11.4,f8.2,f8.1, 2f9.3,3f8.4, 6f8.2,
     &               2f8.4, 3f8.1, f9.2)')
     &    '#',time_ave,                                            !model-day
     &    ssha*100.0/g,                                            !cm
     &    surflx_m(itest,jtest),                                   !W/m**2
     &    min(  dpbl_m(itest,jtest)*qonem, 9999.999),              !m
     &    min(dpmixl_m(itest,jtest)*qonem, 9999.999),              !m
     &    temp_m(itest,jtest,1),                                   !degC
     &    saln_m(itest,jtest,1),                                   !psu
     &    th3d_m(itest,jtest,1)+thbase,                            !SigmaT
     &    max(-999.99,min(999.99, upnt*100.0)),                    !cm/s
     &    max(-999.99,min(999.99, vpnt*100.0)),                    !cm/s
     &    max(-999.99,min(999.99,ubpnt*100.0)),                    !cm/s
     &    max(-999.99,min(999.99,vbpnt*100.0)),                    !cm/s
     &    sshs*100.0/g,                                            !cm
     &    sshn*100.0/g,                                            !cm
     &    sstc,                                                    !degC
     &    sssc,                                                    !psu
     &    0.0,                                                     !W/m**2
     &    0.0,                                                     !W/m**2
     &    0.0,                                                     !W/m**2
     &    salflx_m(itest,jtest)*thref*8.64E7/saln_m(itest,jtest,1) !mm/day
        if     (iceflg.ne.0) then
          write (nop,'(2a / a,f11.4, 3f8.2,2f8.1,f9.2)')
     &    '## model-day',
     &    '  covice  thkice  temice  flxice  fswice   iceE-P',
     &    '#',time_ave,                                            !model-day
     &      covice_m(itest,jtest)*100.0,                             !%
     &      thkice_m(itest,jtest),                                   !m
     &      temice_m(itest,jtest),                                   !degC
     &      0.0,  !flxice_m(itest,jtest),                            !W/m**2
     &      0.0,  !fswice_m(itest,jtest),                            !W/m**2
     &      0.0   !sflice_m(itest,jtest)*thref*8.64E7/saln_m(itest,jtest,1) !mm/day
        endif !iceflg
#if defined(STOKES)
        if     (ntracr.eq.0) then
          write(cformat,'(a)')      '(4a)'
        else
          write(cformat,'(a,i2,a)') '(4a,', ntracr, 'a)'
        endif
        write (nop,cformat)
     &      '#  k',
     &      '    utot    vtot  p.temp    saln  p.dens',
     &      '    thkns      dpth  viscty  t-diff  s-diff',
     &      '  usdtot  vsdtot',
     &      ('  tracer',ktr=1,ntracr)
        if     (ntracr.eq.0) then
          write(cformat,'(a)')
     &      '(i4,2f8.2,3f8.4,f9.3,f10.3,3f8.2,2f8.2)'
        else
          write(cformat,'(a,i2,a)')
     &      '(i4,2f8.2,3f8.4,f9.3,f10.3,3f8.2,2f8.2,', ntracr, 'f8.3)'
        endif
#else
        if     (ntracr.eq.0) then
          write(cformat,'(a)')      '(3a)'
        else
          write(cformat,'(a,i2,a)') '(3a,', ntracr, 'a)'
        endif
        write (nop,cformat)
     &      '#  k',
     &      '    utot    vtot  p.temp    saln  p.dens',
     &      '    thkns      dpth  viscty  t-diff  s-diff',
     &      ('  tracer',ktr=1,ntracr)
        if     (ntracr.eq.0) then
          write(cformat,'(a)')
     &      '(i4,2f8.2,3f8.4,f9.3,f10.3,3f8.2)'
        else
          write(cformat,'(a,i2,a)')
     &      '(i4,2f8.2,3f8.4,f9.3,f10.3,3f8.2,', ntracr, 'f8.3)'
        endif
#endif
        do k= 1,kk
          upnt  = 0.5*(u_m(itest,jtest,k)+u_m(itst1,jtest,k))
          vpnt  = 0.5*(v_m(itest,jtest,k)+v_m(itest,jtst1,k))
#if defined(STOKES)
          if     (stdarc) then
            ustk = usdp_m(itest,jtest,k)
            vstk = vsdp_m(itest,jtest,k)
          else
            ustk = 0.0
            vstk = 0.0
          endif
#endif
          write (nop,cformat)
     &       k,
     &       max(-999.99,min(999.99,upnt*100.0)),                    !cm/s
     &       max(-999.99,min(999.99,vpnt*100.0)),                    !cm/s
     &       temp_m(itest,jtest,k),                                  !degC
     &       saln_m(itest,jtest,k),                                  !psu
     &       th3d_m(itest,jtest,k)+thbase,                           !SigmaT
     &         dp_m(itest,jtest,k)*qonem,                            !m
     &           (p(itest,jtest,k+1)+p(itest,jtest,k))*0.5*qonem,    !m
     &       0.0, !vcty(itest,jtest,k+1)*1.e4,                       !cm**2/s
     &       0.0, !dift(itest,jtest,k+1)*1.e4,                       !cm**2/s
     &       0.0, !difs(itest,jtest,k+1)*1.e4,                       !cm**2/s
#if defined(STOKES)
     &       max(-999.99,min(999.99,ustk*100.0)),                    !cm/s
     &       max(-999.99,min(999.99,vstk*100.0)),                    !cm/s
#endif
     &       (tracer_m(itest,jtest,k,ktr),ktr=1,ntracr)              !0-999?
        enddo !k
        close (unit=nop)
      endif !test point tile
c
      call xcsync(no_flush)
      return
      end subroutine mean_archiv

      end module mod_mean
c
c
c> Revision history:
c>
c> Apr  2007 - 1st version
c> Dec. 2012 - fixed array out of bounds bug
c> May  2014 - use land/sea masks (e.g. ip) to skip land
