      module mod_archiv
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
c
      implicit none
c
c --- HYCOM archive file processing.
c
      character*240, allocatable, dimension(:),
     &               save, private ::
     &   cpnts         ! names of profile locations
      integer,       allocatable, dimension(:),
     &               save, private ::
     &   ipnts,        ! i-indexes of profile locations
     &   jpnts         ! j-indexes of profile locations
      integer,       save, private ::
     &   npnts         ! number of profile locations
      logical,       save, private ::
     &   fpnts         ! initialize profile location files
c
      integer, parameter, private :: nfields=19  !no. fields in surface archive
      character*6, save,  private :: c_arch(nfields) !field names
      logical,     save,  private :: l_arch(nfields) !field output flags
c
      private archiv_prof_out

      contains

      subroutine archiv_init
c
c --- initialize surface archive output flags
c
      logical      lexist
      integer      ios,k_arch
c
      if     (mnproc.eq.1) then
      write (lp,*) ' now processing archv.input ...'
      endif !1st tile
      call xcsync(flush_lp)
c
c --- check that archv.input exists
c
      inquire(file=trim(flnminp)//'archv.input',exist=lexist)
      if     (.not.lexist) then
        if     (mnproc.eq.1) then
        write (lp,*) ' output 17 standard surface archive fields.'
        endif !1st tile
        call xcsync(flush_lp)
c
        l_arch( 1:17) = .true.
        l_arch(18:19) = .false.  !surtx,surty must be explicitly selected
        return
      endif
c
c --- list of field names, 6 character versions of 8 character names
c
      c_arch( 1) = 'montg1'
      c_arch( 2) = 'srfhgt'
      c_arch( 3) = 'steric'
      c_arch( 4) = 'surflx'
      c_arch( 5) = 'salflx'
      c_arch( 6) = 'bldpth'
      c_arch( 7) = 'mldpth'
      c_arch( 8) = 'covice'
      c_arch( 9) = 'thkice'
      c_arch(10) = 'temice'
      c_arch(11) = 'ubtrop'
      c_arch(12) = 'vbtrop'
      c_arch(13) = 'u-vel.'
      c_arch(14) = 'v-vel.'
      c_arch(15) = 'thknss'
      c_arch(16) = 'temp  '
      c_arch(17) = 'salin '
      c_arch(18) = 'surtx '  !output after salflx
      c_arch(19) = 'surty '
c
c --- read in archv.input.
c
      open(unit=uoff+99,file=trim(flnminp)//'archv.input')
      do k_arch= 1,nfields
        call blkinl(l_arch(k_arch),c_arch(k_arch))
      enddo
      close (unit=uoff+99)
      call xcsync(flush_lp)
      return
      end subroutine archiv_init

      subroutine archiv(n, kkout, iyear,iday,ihour, intvl)
#if defined(STOKES)
      use mod_stokes  ! Stokes Drift Velocity Module
#endif
c
      integer   n, kkout, iyear,iday,ihour
      character intvl*3
c
      include 'stmt_fns.h'
c
c --- write an archive file.
c
      character*80 cformat,flnmarcvs
      integer      i,j,k,ktr,ldot,nop,nopa
      real         coord,xmin,xmax
c
      if     (kkout.eq.1) then
        flnmarcvs = flnmarcs
      else
        flnmarcvs = flnmarc
      endif
      ldot = index(flnmarcvs,'.',back=.true.)
      if     (ldot.eq.0) then
        if     (mnproc.eq.1) then
        write (lp,*) 'need decimal point in flnmarcvs'
        write (lp,*) 'flnmarcvs = ',trim(flnmarcvs)
        endif
        call xcstop('(flnmarcvs)')
               stop '(flnmarcvs)'
      endif
      ldot = min(ldot,len(flnmarcvs)-11)  !need 11 characters for archive date
c
      if     ((kkout.eq.1 .and. dsurfq.ge.1.0/24.0) .or.
     &        (kkout.gt.1 .and. diagfq.ge.1.0/24.0)     ) then
c ---   indicate the archive date
        write(flnmarcvs(ldot+1:ldot+11),'(i4.4,a1,i3.3,a1,i2.2)') 
     &   iyear,'_',iday,'_',ihour
        ldot=ldot+11
      else
c ---   indicate the archive time step
        write(flnmarcvs(ldot+1:ldot+11),'(i11.11)') nstep
        ldot=ldot+11
      endif
      nopa=13
      nop =13+uoff
c
c --- no .[ab] files for 1-D cases (<=6x6) or for dsur1p surface cases.
c
      if     (max(itdm,jtdm).gt.6 .and.
     &        .not.(dsur1p .and. kkout.eq.1)) then  !not 1-D output
c
      call zaiopf(flnmarcvs(1:ldot)//'.a', 'new', nopa)
      if     (mnproc.eq.1) then
      open (unit=nop,file=flnmarcvs(1:ldot)//'.b',status='new') !uoff+13
      write(nop,116) ctitle,iversn,iexpt,yrflag,itdm,jtdm
      call flush(nop)
      endif !1st tile
 116  format (a80/a80/a80/a80/
     & i5,4x,'''iversn'' = hycom version number x10'/
     & i5,4x,'''iexpt '' = experiment number x10'/
     & i5,4x,'''yrflag'' = days in year flag'/
     & i5,4x,'''idm   '' = longitudinal array size'/
     & i5,4x,'''jdm   '' = latitudinal  array size'/
     & 'field       time step  model day',
     & '  k  dens        min              max')
c
c --- surface fields
c
c --- identify the equation of state (sigver,thbase) on the first record
      k    =sigver
      coord=thbase
c
      if     (kkout.gt.1 .or. l_arch(1)) then
      call zaiowr(montg1,ip,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'montg1  ',nstep,time,k,coord,xmin,xmax
      k    =0
      coord=0.0
      call flush(nop)
      endif !1st tile
      endif !l_arch
      if     (kkout.gt.1 .or. l_arch(2)) then
      call zaiowr(srfhgt,ip,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'srfhgt  ',nstep,time,k,coord,xmin,xmax
      k    =0
      coord=0.0
      call flush(nop)
      endif !1st tile
      endif !l_arch
      if     (sshflg.ne.0) then
c ---   write out steric SSH.
        if     (kkout.gt.1 .or. l_arch(3)) then
        call zaiowr(steric,ip,.true.,
     &              xmin,xmax, nopa, .false.)
        if     (mnproc.eq.1) then
        write (nop,117) 'steric  ',nstep,time,k,coord,xmin,xmax
        k    =0
        coord=0.0
        call flush(nop)
        endif !1st tile
        endif !l_arch
      endif !sshflg
c
      if     (kkout.gt.1 .or. l_arch(4)) then
      call zaiowr(surflx,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'surflx  ',nstep,time,k,coord,xmin,xmax
      k    =0
      coord=0.0
      call flush(nop)
      endif !1st tile
      endif !l_arch
      if     (kkout.gt.1 .or. l_arch(5)) then
      call zaiowr(salflx,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'salflx  ',nstep,time,k,coord,xmin,xmax
      k    =0
      coord=0.0
      call flush(nop)
      endif !1st tile
      endif !l_arch
c
c --- surtx and surty only output when selected in archv.input
      if     (kkout.eq.1 .and. l_arch(18)) then
      call zaiowr(surtx, ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'surtx   ',nstep,time,k,coord,xmin,xmax
      k    =0
      coord=0.0
      call flush(nop)
      endif !1st tile
      endif !l_arch
      if     (kkout.eq.1 .and. l_arch(19)) then
      call zaiowr(surty, ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'surty   ',nstep,time,k,coord,xmin,xmax
      k    =0
      coord=0.0
      call flush(nop)
      endif !1st tile
      endif !l_arch
c
      if     (kkout.gt.1 .or. l_arch(6)) then
      call zaiowr(dpbl,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'bl_dpth ',nstep,time,k,coord,xmin,xmax
      k    =0
      coord=0.0
      call flush(nop)
      endif !1st tile
      endif !l_arch
      if     (kkout.gt.1 .or. l_arch(7)) then
      call zaiowr(dpmixl(1-nbdy,1-nbdy,n),ip,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'mix_dpth',nstep,time,k,coord,xmin,xmax
      k    =0
      coord=0.0
      call flush(nop)
      endif !1st tile
      endif !l_arch
c
c<-hsk add variables in mixed layer depth (11/3/2016)
c
      call zaiowr(tmix,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'tmix    ',nstep,time,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(smix,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'smix    ',nstep,time,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(thmix,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'thmix   ',nstep,time,0,thbase,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(umix,iu,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'umix    ',nstep,time,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(vmix,iv,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'vmix    ',nstep,time,0,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
c
c->end hsk's add:
c
      if     (iceflg.ne.0) then
        if     (kkout.gt.1 .or. l_arch(8)) then
        call zaiowr(covice,ip,.true., xmin,xmax, nopa, .false.)
        if     (mnproc.eq.1) then
        write (nop,117) 'covice  ',nstep,time,k,coord,xmin,xmax
        k    =0
        coord=0.0
        call flush(nop)
        endif !1st tile
        endif !l_arch
        if     (kkout.gt.1 .or. l_arch(9)) then
        call zaiowr(thkice,ip,.true., xmin,xmax, nopa, .false.)
        if     (mnproc.eq.1) then
        write (nop,117) 'thkice  ',nstep,time,k,coord,xmin,xmax
        k    =0
        coord=0.0
        call flush(nop)
        endif !1st tile
        endif !l_arch
        if     (kkout.gt.1 .or. l_arch(10)) then
        call zaiowr(temice,ip,.true., xmin,xmax, nopa, .false.)
        if     (mnproc.eq.1) then
        write (nop,117) 'temice  ',nstep,time,k,coord,xmin,xmax
        k    =0
        coord=0.0
        call flush(nop)
        endif !1st tile
        endif !l_arch
      endif  !write ice fields
c
c --- depth averaged fields
c
      if     (kkout.gt.1 .or. l_arch(11)) then
      call zaiowr(ubavg(1-nbdy,1-nbdy,n),iu,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'u_btrop ',nstep,time,k,coord,xmin,xmax
      k    =0
      coord=0.0
      call flush(nop)
      endif !1st tile
      endif !l_arch
      if     (kkout.gt.1 .or. l_arch(12)) then
      call zaiowr(vbavg(1-nbdy,1-nbdy,n),iv,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'v_btrop ',nstep,time,k,coord,xmin,xmax
      k    =0
      coord=0.0
      call flush(nop)
      endif !1st tile
      endif !l_arch
c
c --- dissipation fields
c
      if     (kkout.eq.1 .and. disp_count.gt.0) then
      displd_mn(:,:) = displd_mn(:,:)/real(disp_count)
      call zaiowr(displd_mn,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      k    =0
      coord=0.0
      write (nop,117) 'disp_ld ',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      dispqd_mn(:,:) = dispqd_mn(:,:)/real(disp_count)
      call zaiowr(dispqd_mn,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      k    =0
      coord=0.0
      write (nop,117) 'disp_qd ',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      tidepg_mn(:,:) = tidepg_mn(:,:)/real(disp_count)
      call zaiowr(tidepg_mn,ip,.true., xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      k    =0
      coord=0.0
      write (nop,117) 'tide_pg ',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
c
      displd_mn(:,:) = 0.0
      dispqd_mn(:,:) = 0.0
      tidepg_mn(:,:) = 0.0
      disp_count     = 0
      endif !linear and quadratic drag dissipation
c
c --- layer loop.
c
      do 75 k=1,kkout
      coord=sigma(k)
      if     (kkout.gt.1 .or. l_arch(13)) then
      call zaiowr(u(1-nbdy,1-nbdy,k,n),iu,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'u-vel.  ',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      endif !l_arch
      if     (kkout.gt.1 .or. l_arch(14)) then
      call zaiowr(v(1-nbdy,1-nbdy,k,n),iv,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'v-vel.  ',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      endif !l_arch
      if     (kkout.gt.1 .or. l_arch(15)) then
      call zaiowr(dp(1-nbdy,1-nbdy,k,n),ip,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'thknss  ',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      endif !l_arch
      if     (kkout.gt.1 .or. l_arch(16)) then
      call zaiowr(temp(1-nbdy,1-nbdy,k,n),ip,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'temp    ',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      endif !l_arch
      if     (kkout.gt.1 .or. l_arch(17)) then
      call zaiowr(saln(1-nbdy,1-nbdy,k,n),ip,.true.,
     &            xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'salin   ',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      endif !l_arch
c
c --- no tracers or diffusion for single layer case
c
      if     (kkout.gt.1) then
        do ktr= 1,ntracr
          call zaiowr(tracer(1-nbdy,1-nbdy,k,n,ktr),ip,.true.,
     &                xmin,xmax, nopa, .false.)
          if     (mnproc.eq.1) then
          write (nop,117) 'tracer  ',nstep,time,k,coord,xmin,xmax
          call flush(nop)
          endif !1st tile
        enddo !ktr
#if defined(STOKES)
        if     (stdarc) then
          call zaiowr(usdp(1-nbdy,1-nbdy,k),ip,.true.,
     &                xmin,xmax, nopa, .false.)
          if     (mnproc.eq.1) then
          write (nop,117) 'x-stokes',nstep,time,k,coord,xmin,xmax
          call flush(nop)
          endif !1st tile
          call zaiowr(vsdp(1-nbdy,1-nbdy,k),ip,.true.,
     &                xmin,xmax, nopa, .false.)
          if     (mnproc.eq.1) then
          write (nop,117) 'y-stokes',nstep,time,k,coord,xmin,xmax
          call flush(nop)
          endif !1st tile
        endif !stdarc
#endif
        if     (difout) then
          call zaiowr(vcty(1-nbdy,1-nbdy,k+1),ip,.true.,
     &                xmin,xmax, nopa, .false.)
          if     (mnproc.eq.1) then
          write (nop,117) 'viscty  ',nstep,time,k,coord,xmin,xmax
          call flush(nop)
          endif !1st tile
          call zaiowr(dift(1-nbdy,1-nbdy,k+1),ip,.true.,
     &                xmin,xmax, nopa, .false.)
          if     (mnproc.eq.1) then
          write (nop,117) 't-diff  ',nstep,time,k,coord,xmin,xmax
          call flush(nop)
          endif !1st tile
          call zaiowr(difs(1-nbdy,1-nbdy,k+1),ip,.true.,
     &                xmin,xmax, nopa, .false.)
          if     (mnproc.eq.1) then
          write (nop,117) 's-diff  ',nstep,time,k,coord,xmin,xmax
          call flush(nop)
          endif !1st tile
        endif !difout
      endif !kkout>1
 75   continue
c
 117  format (a8,' =',i11,f11.3,i3,f7.3,1p2e16.7)
c
c
c<-hsk: coupled ATM fluxes (11/3/2016)
c
#if defined(USE_SCOUPLER)
      coord=0.0
      k=0
      call zaiowr(taux_(1-nbdy,1-nbdy),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'stress_x',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(tauy_(1-nbdy,1-nbdy),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'stress_y',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(precip_(1-nbdy,1-nbdy),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if (mnproc.eq.1) then
      write (nop,117) 'precip  ',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(radflx_(1-nbdy,1-nbdy),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'rad-flux',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(swflx_(1-nbdy,1-nbdy),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'swr-flux',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(mslprs_(1-nbdy,1-nbdy),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'atm-mslp',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(snsibl_(1-nbdy,1-nbdy),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'sns-flux',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(latent_(1-nbdy,1-nbdy),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'lat-flux',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st file
c<- end hsk's coupled ATM fluxes
c
c<- hsk: GFS fluxes
c
      call zaiowr(taux_g(1-nbdy,1-nbdy),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'gfs_strx',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(tauy_g(1-nbdy,1-nbdy),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'gfs_stry',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(precip_g(1-nbdy,1-nbdy),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'gfs_prcp',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(radflx_g(1-nbdy,1-nbdy),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'gfs_rdfx',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(swflx_g(1-nbdy,1-nbdy),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'gfs_swfx',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(mslprs_g(1-nbdy,1-nbdy),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'gfs_mslp',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
#else
      call zaiowr(taux(1-nbdy,1-nbdy,n),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'gfs_strx',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(tauy(1-nbdy,1-nbdy,n),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'gfs_stry',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(precip(1-nbdy,1-nbdy,n),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'gfs_prcp',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(radflx(1-nbdy,1-nbdy,n),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'gfs_rdfx',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(swflx(1-nbdy,1-nbdy,n),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'gfs_swfx',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
      call zaiowr(mslprs(1-nbdy,1-nbdy,n),ip,.true.,
     &                xmin,xmax, nopa, .false.)
      if     (mnproc.eq.1) then
      write (nop,117) 'gfs_mslp',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      endif !1st tile
#endif
c
c<-end hsk's GFS fluxes
c
c --- output time-averaged mass fluxes, if required
c
      if (.not. (mxlkpp .or. mxlmy .or. mxlgiss) .and. kkout.eq.kk) then
        do k=1,kk
          coord=sigma(k)
          call zaiowr(diaflx(1-nbdy,1-nbdy,k),ip,.true.,
     &                xmin,xmax, nopa, .false.)
          if     (mnproc.eq.1) then
          write (nop,118) 'diafx',intvl,nstep,time,k,coord,xmin,xmax
          call flush(nop)
          endif !1st tile
 118      format (a5,a3,' =',i11,f11.3,i3,f7.3,1p2e16.7)
        enddo
      endif !diaflx
c
      close (unit=nop)
      call zaiocl(nopa)
c
      call xcsync(no_flush)
c
      endif  !not 1-D
c
      if     (itest.gt.0 .and. jtest.gt.0) then
        open (unit=nop,file=flnmarcvs(1:ldot)//'.txt',status='new') !uoff+13
        call archiv_prof_out(n, iyear,iday,ihour, ittest,jttest, nop)
        close (unit=nop)
      endif !test point tile
c
      call xcsync(no_flush)
cccc
cccc --- output to line printer
cccc
ccc      call prtmsk(ip,srfhgt,util3,idm,ii,jj,0.,100.0/g,
ccc     .     'sea surface height (cm)')
ccc      if(mxlkpp) call prtmsk(ip,dpbl,util3,idm,ii,jj,0.,1.*qonem,
ccc     .     'turb. b.l. depth (m)')
ccc      call prtmsk(ip,dpmixl,util3,idm,ii,jj,0.,1.*qonem,
ccc     .     'mixed layer depth (m)')
ccc      call prtmsk(ip,tmix,util3,idm,ii,jj,0.,10.,
ccc     .     'mix.layer temp. (.1 deg)')
ccc      call prtmsk(ip,smix,util3,idm,ii,jj,35.,100.,
ccc     .     'mx.lay. salin. (.01 mil)')
ccc!$OMP PARALLEL DO PRIVATE(j,i)
ccc!$OMP&         SCHEDULE(STATIC,jblk)
ccc      do j=1-margin,jj+margin
ccc        do i=1-margin,ii+margin
ccc          if (iu(i,j).ne.0) then
ccc            util1(i,j)=umix(i,j)+ubavg(i,j,n)
ccc          endif !iu
ccc          if (iv(i,j).ne.0) then
ccc            util2(i,j)=vmix(i,j)+vbavg(i,j,n)
ccc          endif !iv
ccc        enddo !i
ccc      enddo !j
ccc!$OMP END PARALLEL DO
ccc      call prtmsk(iu(2,1),util1(2,1),util3,idm,ii-2,jj,0.,1000.,
ccc     .     'mix.layer u vel. (mm/s)')
ccc      call prtmsk(iv(1,2),util2(1,2),util3,idm,ii,jj-2,0.,1000.,
ccc     .     'mix.layer v vel. (mm/s)')
ccc      call prtmsk(iu(2,1),ubavg(2,1,n),util3,idm,ii-2,jj,0.,1000.,
ccc     .     'barotrop. u vel. (mm/s)')
ccc      call prtmsk(iv(2,1),vbavg(1,2,n),util3,idm,ii,jj-2,0.,1000.,
ccc     .     'barotrop. v vel. (mm/s)')
      return
      end subroutine archiv

      subroutine archiv_prof_init
c
c --- initialize for multi-location profile output.
c
      logical      lexist
      integer      ios,kpnt
c
c --- check that the requried directory exists
c
      inquire(file='ARCHP/archv.dummy.txt',exist=lexist)
      if     (.not.lexist) then
        if     (mnproc.eq.1) then
        write (lp,*) 'file ARCHP/archv.dummy.txt must exist'
        endif
        call xcstop('(archv_prof_init)')
               stop '(archv_prof_init)'
      endif
c
c --- count the number of locations
c
      open(unit=uoff+99,file=trim(flnminp)//'profile.input')
      do kpnt= 1,999999
        read(uoff+99,*,iostat=ios)
        if     (ios.ne.0) then
          exit
        endif
      enddo !kpnt
      npnts = kpnt - 1
      if     (npnts.eq.0) then
        if     (mnproc.eq.1) then
        write (lp,*) 'profile.input is empty'
        endif
        call xcstop('(archv_prof_init)')
               stop '(archv_prof_init)'
      endif
c
      allocate( ipnts(npnts), jpnts(npnts), cpnts(npnts) )
c
c --- profile.input contains one line per profile with 3 entries
c --- i and j array indexes followed by the name of the profile
c
      rewind(unit=uoff+99)
      do kpnt= 1,npnts
        read(uoff+99,*) ipnts(kpnt),jpnts(kpnt),cpnts(kpnt)
      enddo !kpnt
      close (unit=uoff+99)
c
      fpnts = .true.  !initialize profile location files
      return
      end subroutine archiv_prof_init

      subroutine archiv_prof(n, kkout, iyear,iday,ihour)
c
      integer   n, kkout, iyear,iday,ihour
c
      include 'stmt_fns.h'
c
c --- multi-location profile output.
c
      character*81, save :: flnmarcp  !1 extra character for trailing "_"
      integer,      save :: ldot
      integer ::            ipnt,jpnt,kpnt,nop
c
      if     (npnts.eq.0) then
        return
      endif
c
      if     (fpnts) then  ! initialize profile location files
        flnmarcp = flnmarc
        ldot = index(flnmarcp,'.',back=.true.)
        if     (ldot.eq.0) then
          if     (mnproc.eq.1) then
          write (lp,*) 'need decimal point in flnmarcp'
          write (lp,*) 'flnmarcp = ',trim(flnmarcp)
          endif
          call xchalt('(flnmarcp)')
                 stop '(flnmarcp)'
        endif
        ldot = min(ldot,len(flnmarcp)-12)  !need 12 characters for archive date
c
        if     (proffq.ge.1.0/24.0) then
c ---     indicate the archive date
          write(flnmarcp(ldot+1:ldot+12),'(i4.4,a1,i3.3,a1,i2.2,a1)') 
     &    iyear,'_',iday,'_',ihour,'_'
          ldot=ldot+12
        else
c ---     indicate the archive time step
          write(flnmarcp(ldot+1:ldot+12),'(i11.11,a1)') nstep,'_'
          ldot=ldot+12
        endif
      endif !fpnts
      nop =13+uoff
c
      do kpnt= 1,npnts
        ipnt = ipnts(kpnt) - i0
        jpnt = jpnts(kpnt) - j0
c
        if     (ipnt.gt.0 .and. ipnt.le.ii .and.
     &          jpnt.gt.0 .and. jpnt.le.jj      ) then
          if     (fpnts) then  ! initialize profile location files
            open (unit=nop,status='new',
     &       action='write',form='formatted',
     &       file='ARCHP/'//flnmarcp(1:ldot)//trim(cpnts(kpnt))//'.txt')
          else  !write at the end of existing profile files
            open (unit=nop,status='old',position='append',
     &       action='write',form='formatted',
     &       file='ARCHP/'//flnmarcp(1:ldot)//trim(cpnts(kpnt))//'.txt')
          endif !fpnts
          call archiv_prof_out(n, iyear,iday,ihour,
     &                         ipnts(kpnt),jpnts(kpnt), nop)
          close (unit=nop)
        endif !test point tile
      enddo !kpnt
c
      fpnts = .false.  ! use existing profile location files
c

      call xcsync(no_flush)  !called on all tiles
      return
      end subroutine archiv_prof

      subroutine archiv_prof_out(n, iyear,iday,ihour, ipoint,jpoint,nop)
#if defined(STOKES)
      use mod_stokes  ! Stokes Drift Velocity Module
#endif
c
      integer   n, iyear,iday,ihour, ipoint,jpoint,nop
c
      include 'stmt_fns.h'
c
c --- on the owning tile only: write a text profile file to unit nop.
c --- open and close of the I/O unit is done outside this routine.
c
      character*80 cformat
      integer      ipnt,ipnt1,jpnt,jpnt1,k,ktr
      real         ssha,sshn,sshs,sssc,sstc,ubpnt,upnt,vbpnt,vpnt
#if defined(STOKES)
      real         ubstk,ustk,ust0,vbstk,vstk,vst0
#endif
c
      ipnt = ipoint - i0
      jpnt = jpoint - j0
c
      if     (ipnt.gt.0 .and. ipnt.le.ii .and.
     &        jpnt.gt.0 .and. jpnt.le.jj      ) then
c ---   owning tile.
        write (nop,'(3a / a,6i7,f9.3,f8.3,i7,i5.4,i4.3,i3.2)')
     &      '##   expt    idm    jdm    kdm',
     &        '   iloc   jloc   lonloc  latloc',
     &        ' yrflag year day hr',
     &      '##',iexpt,  itdm,  jtdm,   kdm,
     &          ipoint,jpoint,
     &          mod(plon(ipnt,jpnt),360.0),plat(ipnt,jpnt),
     &          yrflag,iyear,iday,ihour
c
        ssha = srfhgt(ipnt,jpnt)
        if     (sshflg.ne.0) then
          sshs = steric(ipnt,jpnt)
        else
          sshs = ssha  !assume all is steric
        endif
        sshn = ssha - sshs
c
        if     (sstflg.le.1) then
          if     (relaxf) then
            sstc = twall(ipnt,jpnt,1,lc0)*wc0+
     &             twall(ipnt,jpnt,1,lc1)*wc1+
     &             twall(ipnt,jpnt,1,lc2)*wc2+
     &             twall(ipnt,jpnt,1,lc3)*wc3
          else
            sstc = 99.9999
          endif !relaxf
        else !synoptic observed sst
          if     (natm.eq.2) then
            sstc = seatmp(ipnt,jpnt,l0)*w0+
     &             seatmp(ipnt,jpnt,l1)*w1
          else
            sstc = seatmp(ipnt,jpnt,l0)*w0+
     &             seatmp(ipnt,jpnt,l1)*w1+
     &             seatmp(ipnt,jpnt,l2)*w2+
     &             seatmp(ipnt,jpnt,l3)*w3
          endif !natm
        endif !sstflg
        if     (relaxf) then
          sssc  = swall(ipnt,jpnt,1,lc0)*wc0+
     &            swall(ipnt,jpnt,1,lc1)*wc1+
     &            swall(ipnt,jpnt,1,lc2)*wc2+
     &            swall(ipnt,jpnt,1,lc3)*wc3
        else
          sssc = 99.9999
        endif !relaxf
c
c ---   interpolate to the p-grid, but only if it requres no halo points
        ipnt1 = min(ipnt+1,ii)  !either ipnt+1 or ipnt
        jpnt1 = min(jpnt+1,jj)  !either jpnt+1 or jpnt
        ubpnt = 0.5*(ubavg(ipnt,jpnt,n)+ubavg(ipnt1,jpnt, n))
        vbpnt = 0.5*(vbavg(ipnt,jpnt,n)+vbavg(ipnt, jpnt1,n))
        upnt  = 0.5*( umix(ipnt,jpnt)  + umix(ipnt1,jpnt )  ) + ubpnt
        vpnt  = 0.5*( vmix(ipnt,jpnt)  + vmix(ipnt, jpnt1)  ) + vbpnt
#if defined(STOKES)
        ubstk = 0.5*(usdbavg(ipnt,jpnt)+usdbavg(ipnt1,jpnt ))
        vbstk = 0.5*(vsdbavg(ipnt,jpnt)+vsdbavg(ipnt, jpnt1))
        ust0  = usds(ipnt,jpnt)
        vst0  = vsds(ipnt,jpnt)
c
c ---   order is not optimal, constrained by ALL/bin/hycom_profile_list 
c ---   which only includes the fields up to vbavg (or up to nsterc)
        write (nop,'(8a)')
     &    '## model-day  srfhgt  surflx',
     &    '     dpbl   dpmixl    tmix    smix   thmix',
     &    '    umix    vmix   ubavg   vbavg  steric  nsterc',
     &    '   tclim   sclim',
     &    '  sswflx  mixflx  sstflx',
     &    '      E-P   sssE-P  bhtflx  buoflx',
     &    '    ustar   hekman    dpbbl',
     &    ' usdbave vsdbave    usd0    vsd0'
        write (nop,'(a,f11.4,f8.2,f8.1, 2f9.3,3f8.4, 6f8.2,
     &               2f8.4, 3f8.1, 2f9.2,2f8.4, f9.5, 2f9.3,
     &               4f8.2)')
     &    '#',time,                                            !model-day
     &    ssha*100.0/g,                                        !cm
     &    surflx(ipnt,jpnt),                                   !W/m**2
     &    min(  dpbl(ipnt,jpnt)  *qonem, 9999.999),            !m
     &    min(dpmixl(ipnt,jpnt,n)*qonem, 9999.999),            !m
     &      tmix(ipnt,jpnt),                                   !degC
     &      smix(ipnt,jpnt),                                   !psu
     &     thmix(ipnt,jpnt)+thbase,                            !SigmaT
     &    max(-999.99,min(999.99, upnt*100.0)),                !cm/s
     &    max(-999.99,min(999.99, vpnt*100.0)),                !cm/s
     &    max(-999.99,min(999.99,ubpnt*100.0)),                !cm/s
     &    max(-999.99,min(999.99,vbpnt*100.0)),                !cm/s
     &    sshs*100.0/g,                                        !cm
     &    sshn*100.0/g,                                        !cm
     &    sstc,                                                !degC
     &    sssc,                                                !psu
     &    sswflx(ipnt,jpnt),                                   !W/m**2
     &    mixflx(ipnt,jpnt),                                   !W/m**2
     &    sstflx(ipnt,jpnt),                                   !W/m**2
     &    salflx(ipnt,jpnt)*thref*8.64E7/saln(ipnt,jpnt,1,n),  !mm/day
     &    sssflx(ipnt,jpnt)*thref*8.64E7/saln(ipnt,jpnt,1,n),  !mm/day
     &    bhtflx(ipnt,jpnt)*1.e6,                         !1.e6*m**2/sec**3
     &    buoflx(ipnt,jpnt)*1.e6,                         !1.e6*m**2/sec**3
     &     ustar(ipnt,jpnt),                                   !m/s?
     &    min(hekman(ipnt,jpnt),         9999.999),            !m
     &    min( dpbbl(ipnt,jpnt)  *qonem, 9999.999),            !m
     &    max(-999.99,min(999.99,ubstk*100.0)),                !cm/s
     &    max(-999.99,min(999.99,vbstk*100.0)),                !cm/s
     &    max(-999.99,min(999.99, ust0*100.0)),                !cm/s
     &    max(-999.99,min(999.99, vst0*100.0))                 !cm/s
#else
c
c ---   order is not optimal, constrained by ALL/bin/hycom_profile_list 
c ---   which only includes the fields up to vbavg (or up to nsterc)
        write (nop,'(7a)')
     &    '## model-day  srfhgt  surflx',
     &    '     dpbl   dpmixl    tmix    smix   thmix',
     &    '    umix    vmix   ubavg   vbavg  steric  nsterc',
     &    '   tclim   sclim',
     &    '  sswflx  mixflx  sstflx',
     &    '      E-P   sssE-P  bhtflx  buoflx',
     &    '    ustar   hekman    dpbbl'
        write (nop,'(a,f11.4,f8.2,f8.1, 2f9.3,3f8.4, 6f8.2,
     &               2f8.4, 3f8.1, 2f9.2,2f8.4, f9.5, 2f9.3)')
     &    '#',time,                                            !model-day
     &    ssha*100.0/g,                                        !cm
     &    surflx(ipnt,jpnt),                                   !W/m**2
     &    min(  dpbl(ipnt,jpnt)  *qonem, 9999.999),            !m
     &    min(dpmixl(ipnt,jpnt,n)*qonem, 9999.999),            !m
     &      tmix(ipnt,jpnt),                                   !degC
     &      smix(ipnt,jpnt),                                   !psu
     &     thmix(ipnt,jpnt)+thbase,                            !SigmaT
     &    max(-999.99,min(999.99, upnt*100.0)),                !cm/s
     &    max(-999.99,min(999.99, vpnt*100.0)),                !cm/s
     &    max(-999.99,min(999.99,ubpnt*100.0)),                !cm/s
     &    max(-999.99,min(999.99,vbpnt*100.0)),                !cm/s
     &    sshs*100.0/g,                                        !cm
     &    sshn*100.0/g,                                        !cm
     &    sstc,                                                !degC
     &    sssc,                                                !psu
     &    sswflx(ipnt,jpnt),                                   !W/m**2
     &    mixflx(ipnt,jpnt),                                   !W/m**2
     &    sstflx(ipnt,jpnt),                                   !W/m**2
     &    salflx(ipnt,jpnt)*thref*8.64E7/saln(ipnt,jpnt,1,n),  !mm/day
     &    sssflx(ipnt,jpnt)*thref*8.64E7/saln(ipnt,jpnt,1,n),  !mm/day
     &    bhtflx(ipnt,jpnt)*1.e6,                         !1.e6*m**2/sec**3
     &    buoflx(ipnt,jpnt)*1.e6,                         !1.e6*m**2/sec**3
     &     ustar(ipnt,jpnt),                                   !m/s?
     &    min(hekman(ipnt,jpnt),         9999.999),            !m
     &    min( dpbbl(ipnt,jpnt)  *qonem, 9999.999)             !m
#endif
        if     (iceflg.ne.0) then
          write (nop,'(2a / a,f11.4, 3f8.2,2f8.1,f9.2)')
     &    '## model-day',
     &    '  covice  thkice  temice  flxice  fswice   iceE-P',
     &    '#',time,                                              !model-day
     &      covice(ipnt,jpnt)*100.0,                             !%
     &      thkice(ipnt,jpnt),                                   !m
     &      temice(ipnt,jpnt),                                   !degC
     &      flxice(ipnt,jpnt),                                   !W/m**2
     &      fswice(ipnt,jpnt),                                   !W/m**2
     &      sflice(ipnt,jpnt)*thref*8.64E7/saln(ipnt,jpnt,1,n)   !mm/day
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
          upnt = 0.5*(u(ipnt,jpnt,k,n)+u(ipnt1,jpnt, k,n)) + ubpnt
          vpnt = 0.5*(v(ipnt,jpnt,k,n)+v(ipnt, jpnt1,k,n)) + vbpnt
#if defined(STOKES)
          ustk = 0.5*(usd(ipnt,jpnt,k)+usd(ipnt1,jpnt, k))
          vstk = 0.5*(vsd(ipnt,jpnt,k)+vsd(ipnt, jpnt1,k))
#endif
          write (nop,cformat)
     &       k,
     &       max(-999.99,min(999.99,upnt*100.0)),                  !cm/s
     &       max(-999.99,min(999.99,vpnt*100.0)),                  !cm/s
     &       temp(ipnt,jpnt,k,n),                                  !degC
     &       saln(ipnt,jpnt,k,n),                                  !psu
     &       th3d(ipnt,jpnt,k,n)+thbase,                           !SigmaT
     &         dp(ipnt,jpnt,k,n)*qonem,                            !m
     &         (p(ipnt,jpnt,k+1)+p(ipnt,jpnt,k))*0.5*qonem,        !m
     &       min(9999.99,vcty(ipnt,jpnt,k+1)*1.e4),                !cm**2/s
     &       min(9999.99,dift(ipnt,jpnt,k+1)*1.e4),                !cm**2/s
     &       min(9999.99,difs(ipnt,jpnt,k+1)*1.e4),                !cm**2/s
#if defined(STOKES)
     &       max(-999.99,min(999.99,ustk*100.0)),                  !cm/s
     &       max(-999.99,min(999.99,vstk*100.0)),                  !cm/s
#endif
     &       (tracer(ipnt,jpnt,k,n,ktr),ktr=1,ntracr)              !0-999?
        enddo !k
      else
        write (lp,*) 'archiv_prof_out called on wrong tile'
        write (lp,*) 'ipoint,jpoint = ',ipoint,jpoint
        write (lp,*) 'ipnt,  jpnt   = ',ipnt,  jpnt
        write (lp,*) 
        call xchalt('(archiv_prof_out)')
               stop '(archiv_prof_out)'
      endif !point tile
      return
      end subroutine archiv_prof_out

      subroutine archiv_tile(n, kkout, iyear,iday,ihour)
#if defined(STOKES)
      use mod_stokes  ! Stokes Drift Velocity Module
#endif
c
      integer   n, kkout, iyear,iday,ihour
      real      sssc,sstc
c
      include 'stmt_fns.h'
c
c --- write a partial archive file on a tile by tile basis.
c
      character*12 cdir
      character*80 cformat
      logical      lexist
      integer      i,j,k,ktr,l,ldot,nop,nopa
      real         coord,xmin,xmax
c
c --- only write archive when the corresponing directory exists
c
      write(cdir,'(a6,i5.5,a1)') 'ARCHT/',mnproc,'/'
      inquire(file=cdir//'archt.dummy.txt',exist=lexist)
      if     (.not.lexist) then
        call xcsync(no_flush)  !called on all tiles, see end of routine
        return
      endif
c
      ldot = index(flnmarct,'.',back=.true.)
      if     (ldot.eq.0) then
        if     (mnproc.eq.1) then
        write (lp,*) 'need decimal point in flnmarct'
        write (lp,*) 'flnmarct = ',trim(flnmarct)
        endif
        call xchalt('(flnmarct)')
               stop '(flnmarct)'
      endif
      ldot = min(ldot,len(flnmarct)-11)  !need 11 characters for archive date
c
      if     (tilefq.ge.1.0/24.0) then
c ---   indicate the archive date
        write(flnmarct(ldot+1:ldot+11),'(i4.4,a1,i3.3,a1,i2.2)') 
     &   iyear,'_',iday,'_',ihour
        ldot=ldot+11
      else
c ---   indicate the archive time step
        write(flnmarct(ldot+1:ldot+11),'(i11.11)') nstep
        ldot=ldot+11
      endif
      nopa=13
      nop =13+uoff
c
      call ztiopf(cdir//flnmarct(1:ldot)//'.A', 'new', nopa)
      open (unit=nop,file=cdir//flnmarct(1:ldot)//'.B',status='new') !uoff+13
      write(nop,116) ctitle,iversn,iexpt,yrflag,i0+1,j0+1,ii,jj
      call flush(nop)
 116  format (a80/a80/a80/a80/
     & i5,4x,'''iversn'' = hycom version number x10'/
     & i5,4x,'''iexpt '' = experiment number x10'/
     & i5,4x,'''yrflag'' = days in year flag'/
     & i5,4x,'''i1    '' = longitudinal array starting index'/
     & i5,4x,'''j1    '' = latitudinal  array starting index'/
     & i5,4x,'''ii    '' = longitudinal array size'/
     & i5,4x,'''jj    '' = latitudinal  array size'/
     & 'field       time step  model day',
     & '  k  dens        min              max')
c
c --- surface fields
c
      coord=0.
c
      call ztiowr(montg1,ip,.true.,
     &            xmin,xmax, nopa, .false.)
c --- identify the equation of state on the first record
      write (nop,117) 'montg1  ',nstep,time,sigver,thbase,xmin,xmax
      call flush(nop)
      call ztiowr(srfhgt,ip,.true.,
     &            xmin,xmax, nopa, .false.)
      write (nop,117) 'srfhgt  ',nstep,time,0,coord,xmin,xmax
      call flush(nop)
      if     (sshflg.ne.0) then
c ---   write out steric SSH.
        call ztiowr(steric,ip,.true.,
     &              xmin,xmax, nopa, .false.)
        write (nop,117) 'steric  ',nstep,time,0,coord,xmin,xmax
        call flush(nop)
      endif !sshflg
c
      call ztiowr(surflx,ip,.true., xmin,xmax, nopa, .false.)
      write (nop,117) 'surflx  ',nstep,time,0,coord,xmin,xmax
      call flush(nop)
      call ztiowr(salflx,ip,.true., xmin,xmax, nopa, .false.)
      write (nop,117) 'salflx  ',nstep,time,0,coord,xmin,xmax
      call flush(nop)
c
      call ztiowr(dpbl,ip,.true., xmin,xmax, nopa, .false.)
      write (nop,117) 'bl_dpth ',nstep,time,0,coord,xmin,xmax
      call flush(nop)
      call ztiowr(dpmixl(1-nbdy,1-nbdy,n),ip,.true.,
     &            xmin,xmax, nopa, .false.)
      write (nop,117) 'mix_dpth',nstep,time,0,coord,xmin,xmax
      call flush(nop)
      if     (iceflg.ne.0) then
        call ztiowr(covice,ip,.true., xmin,xmax, nopa, .false.)
        write (nop,117) 'covice  ',nstep,time,0,coord,xmin,xmax
        call flush(nop)
        call ztiowr(thkice,ip,.true., xmin,xmax, nopa, .false.)
        write (nop,117) 'thkice  ',nstep,time,0,coord,xmin,xmax
        call flush(nop)
        call ztiowr(temice,ip,.true., xmin,xmax, nopa, .false.)
        write (nop,117) 'temice  ',nstep,time,0,coord,xmin,xmax
        call flush(nop)
      endif  !write ice fields
c
c --- depth averaged fields
c
      call ztiowr(ubavg(1-nbdy,1-nbdy,n),iu,.true.,
     &            xmin,xmax, nopa, .false.)
      write (nop,117) 'u_btrop ',nstep,time,0,coord,xmin,xmax
      call flush(nop)
      call ztiowr(vbavg(1-nbdy,1-nbdy,n),iv,.true.,
     &            xmin,xmax, nopa, .false.)
      write (nop,117) 'v_btrop ',nstep,time,0,coord,xmin,xmax
      call flush(nop)
c
c --- layer loop.
c
      do 75 k=1,kkout
      coord=sigma(k)
      call ztiowr(u(1-nbdy,1-nbdy,k,n),iu,.true.,
     &            xmin,xmax, nopa, .false.)
      write (nop,117) 'u-vel.  ',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      call ztiowr(v(1-nbdy,1-nbdy,k,n),iv,.true.,
     &            xmin,xmax, nopa, .false.)
      write (nop,117) 'v-vel.  ',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      call ztiowr(dp(1-nbdy,1-nbdy,k,n),ip,.true.,
     &            xmin,xmax, nopa, .false.)
      write (nop,117) 'thknss  ',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      call ztiowr(temp(1-nbdy,1-nbdy,k,n),ip,.true.,
     &            xmin,xmax, nopa, .false.)
      write (nop,117) 'temp    ',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      call ztiowr(saln(1-nbdy,1-nbdy,k,n),ip,.true.,
     &            xmin,xmax, nopa, .false.)
      write (nop,117) 'salin   ',nstep,time,k,coord,xmin,xmax
      call flush(nop)
      do ktr= 1,ntracr
        call ztiowr(tracer(1-nbdy,1-nbdy,k,n,ktr),ip,.true.,
     &              xmin,xmax, nopa, .false.)
        write (nop,117) 'tracer  ',nstep,time,k,coord,xmin,xmax
        call flush(nop)
      enddo !ktr
#if defined(STOKES)
      if     (stdarc) then
        call zaiowr(usdp(1-nbdy,1-nbdy,k),ip,.true.,
     &              xmin,xmax, nopa, .false.)
        if     (mnproc.eq.1) then
        write (nop,117) 'x-stokes',nstep,time,k,coord,xmin,xmax
        call flush(nop)
        endif !1st tile
        call zaiowr(vsdp(1-nbdy,1-nbdy,k),ip,.true.,
     &              xmin,xmax, nopa, .false.)
        if     (mnproc.eq.1) then
        write (nop,117) 'y-stokes',nstep,time,k,coord,xmin,xmax
        call flush(nop)
        endif !1st tile
      endif !stdarc
#endif
      if     (difout) then
        call ztiowr(vcty(1-nbdy,1-nbdy,k+1),ip,.true.,
     &              xmin,xmax, nopa, .false.)
        write (nop,117) 'viscty  ',nstep,time,k,coord,xmin,xmax
        call flush(nop)
        call ztiowr(dift(1-nbdy,1-nbdy,k+1),ip,.true.,
     &              xmin,xmax, nopa, .false.)
        write (nop,117) 't-diff  ',nstep,time,k,coord,xmin,xmax
        call flush(nop)
        call ztiowr(difs(1-nbdy,1-nbdy,k+1),ip,.true.,
     &              xmin,xmax, nopa, .false.)
        write (nop,117) 's-diff  ',nstep,time,k,coord,xmin,xmax
        call flush(nop)
      endif !difout
 75   continue
c
 117  format (a8,' =',i11,f11.3,i3,f7.3,1p2e16.7)
c
      close (unit=nop)
      call ztiocl(nopa)
c
      call xcsync(no_flush)  !called on all tiles, see lexist above
      return
      end subroutine archiv_tile

      end module mod_archiv

c>
c> Revision history
c>
c> Nov  2002 - additional surface data in .txt output
c> Jun  2006 - dsur1p for .txt only surface output
c> Jun  2006 - archi .txt output
c> May  2007 - no diaflx output for K-profile based mixed layer models
c> May  2007 - removed mixed layer fields and th3d from the archive file
c> Feb  2008 - optionally added steric SSH to the archive file
c> Jun  2008 - added archiv_tile for per-tile archive output
c> Jun  2010 - made into a module
c> Jun  2010 - added hycom_prof
c> Jun  2010 - added archiv_init, and archvs.input for surface archives
c> Apr  2011 - added separate surface and 3-D archives, via flnmarcvs
c> Apr  2011 - renamed archvs.input to archs.input
c> Nov. 2012 - added surtx and surty to archs.input
c> Apr. 2013 - added displd_mn, dispqd_mn and tidepg_mn to archs output
c> Aug. 2013 - optionally added Stokes Drift to text profile files
c> Aug. 2015 - optionally added Stokes Drift to archive files (as tracers)
c> Aug. 2015 - defined sstc and sssc when .not.relaxf
c> Sep. 2015 - replace directory checks with dummy file existance checks
c> Feb. 2016 - hycom_prof writes one (multiple snapshot) file per location

