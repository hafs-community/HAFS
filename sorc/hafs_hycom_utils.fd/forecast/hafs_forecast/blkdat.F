      subroutine blkdat
#if defined(USE_CCSM3)
      use ccsm3_io       ! CCSM3 I/O interface, uses mod_cb_arrays
      use mod_xc,        ! HYCOM communication interface
     &    only: xcstop, xcsync, mnproc
#else
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays

#endif
      use mod_incupd     ! HYCOM incremental update (for data assimilation)
      use mod_floats     ! HYCOM synthetic floats, drifters and moorings
      use mod_tides      ! HYCOM tides
#if defined(STOKES)
      use mod_stokes     ! HYCOM Stokes Drift effects from Wave fields
#endif
      implicit none
c
      real      day1,hybrlx,cplifq,frzifq
      integer   k,kdmblk,mlflag,thflag,trcflg1
      integer   lngblk
      character sigfmt*26
c
      include 'stmt_fns.h'
c
c --- initialize common variables.
c
#if defined(USE_CCSM3)
      flnminp = flnmpard
#elif defined(OCEANS2)
      if     (nocean.eq.2) then
c ---   slave HYCOM works from ./OCEAN2
        flnminp = './OCEAN2/'
      else
c ---   master HYCOM
        flnminp = './'
      endif
#else
      flnminp = './'
#endif
      open(unit=uoff+99,file=trim(flnminp)//'blkdat.input')  !on all nodes
c
c --- 'lp' = logical unit number for printer output
c     lp = 6  !now defined in xcspmd
c
c --- four lines (80-characters) describing the simulation
      read( uoff+99,'(a80/a80/a80/a80)') ctitle
      if (mnproc.eq.1) then
      write(lp,*)
      write(lp,'(a80/a80/a80/a80)') ctitle
      call flush(lp)
      endif !1st tile
c
c --- 'iversn' = hycom version number x10
c --- 'iexpt'  = experiment number x10
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkini(iversn,'iversn')
      call blkini(iexpt, 'iexpt ')
c
      if (iversn.lt.22 .or. iversn.gt.22) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,i3,a,i3 /)') 
     &    'error - iversn must be between',22,' and',22
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif !error
c
c --- 'idm   ' = longitudinal array size
c --- 'jdm   ' = latitudinal  array size
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkini(itest ,'idm   ')
      call blkini(jtest ,'jdm   ')
c
      if     (itdm.eq.-1) then !serial relo case only
        itdm = itest
        idm  = itest
        ii   = itest
        jtdm = jtest
        jdm  = jtest
        jj   = jtest
      endif
c
      if     (itest.ne.itdm) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,i5 /)') 
     &    'error - expected idm =',itdm
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif !error
      if     (jtest.ne.jtdm) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,i5 /)') 
     &    'error - expected jdm =',jtdm
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif !error
c
c --- 'itest,jtest' = grid point where detailed diagnostics are desired
c ---                 itest=jtest=0 turns off all detailed diagnostics
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkini(ittest,'itest ')
      call blkini(jttest,'jtest ')
c
      if (ittest.gt.itdm) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,i5 /)') 
     &    'error - maximum itest is',itdm
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif !error
      if (jttest.gt.jtdm) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,i5 /)') 
     &    'error - maximum jtest is',jtdm
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif !error
c
c --- map global ittest,jttest to local itest,jtest
      if     (ittest.gt.i0 .and. ittest.le.i0+ii .and.
     &        jttest.gt.j0 .and. jttest.le.j0+jj      ) then
        itest = ittest - i0
        jtest = jttest - j0
      else
        itest = -99
        jtest = -99
      endif
c
*     if (mnproc.eq.1) then
*     write(lp,*)
*     endif !1st tile
*     call xcsync(flush_lp)
*     do k= 1,ijpr
*       if     (mnproc.eq.k) then
*         write(lp,'(a,3i5)') 'mnproc,[ij]test =',mnproc,itest,jtest
*       endif
*       call xcsync(flush_lp)
*     enddo !k
c
c --- 'kdm   ' = number of layers
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkini(kdmblk,'kdm   ')
c
#if defined(RELO)
      kdm = kdmblk
      kk  = kdmblk
c
      allocate(
     &    dp0k(kdm),
     &    ds0k(kdm),
     &   sigma(kdm),
     &  salmin(kdm) )
      call mem_stat_add( 4*kdm )
#else
      if     (kdmblk.ne.kdm) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,i3 /)') 
     &    'error - expected kdm =',kdm
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif !error
#endif
c
c --- 'nhybrd' = number of hybrid levels (0=all isopycnal)
c --- 'nsigma' = number of sigma  levels (nhybrd-nsigma z-levels)
c --- 'dp00'   = deep    z-level spacing minimum thickness (m)
c --- 'dp00x'  = deep    z-level spacing maximum thickness (m)
c --- 'dp00f'  = deep    z-level spacing stretching factor (1.0=const.z)
c --- 'ds00'   = shallow z-level spacing minimum thickness (m)
c --- 'ds00x'  = shallow z-level spacing maximum thickness (m)
c --- 'ds00f'  = shallow z-level spacing stretching factor (1.0=const.z)
c --- 'dp00i'  = deep iso-pycnal spacing minimum thickness (m)
c --- 'isotop' = shallowest depth for isopycnal layers     (m), <0 from file
c
c --- the above specifies a vertical coord. that is isopycnal or:
c ---  near surface z in    deep water, based on dp00,dp00x,dp00f
c ---  near surface z in shallow water, based on ds00,ds00x,ds00f and nsigma
c ---               sigma between them, based on ds00,ds00x,ds00f and nsigma
c
c --- d[ps].k/d[ps].1 = d[ps]00f**(k-1) unless limited by d[ps]00x.
c --- if d[ps]00f>1, d[ps]00x (> d[ps]00) is the maximum thickness.
c --- if d[ps]00f<1, d[ps]00x (< d[ps]00) is the minimum thickness.
c
c --- near the surface (i.e. shallower than isotop), layers are always fixed
c --- depth (z or sigma).  layer 1 is always fixed, so isotop=0.0 is not
c --- allowed.  if isotop<0.0 then isotop(1:idm,1:jdm) is a spacially
c --- varying array, input from the file iso.top.[ab].
c
c --- away from the surface, the minimum layer thickness is dp00i.
c --- to recover original hybrid behaviour,   set dp00i=dp00x
c --- for z-only, sigma-only or sigma-z only, set dp00i=dp00x
c
c --- for z-only set nsigma=0 (and ds00,ds00x,ds00f=dp00,dp00x,dp00f)
c --- for sigma-z (shallow-deep) use a very small ds00
c ---  (pure sigma-z also has ds00f=dp00f and ds00x=dp00x*ds00/dp00)
c --- for z-sigma (shallow-deep) use a very large dp00 (not recommended)
c --- for sigma-only set nsigma=kdm, dp00 large, and ds00 small
c
c --- for an entirely fixed vertical coordinate (no isopycnal layers), set
c --- isotop large or make all target densities (sigma(k), below) very small.
c
c --- or, in place of 'dp00','dp00x','dp00f','ds00','ds00x','ds00f' specify:
c --- 'dp0k  ' = layer k deep    z-level spacing minimum thickness (m)
c ---              k=1,kdm; dp0k must be zero for k>nhybrd
c --- 'ds0k  ' = layer k shallow z-level spacing minimum thickness (m)
c ---              k=1,nsigma
c
c --- uses version 2.2.58 definition of deep and shallow z-levels.
c --- terrain following starts at depth dpns=sum(dp0k(k),k=1,nsigma) and
c --- ends at depth dsns=sum(ds0k(k),k=1,nsigma), and the depth of the
c --- k-th layer interface varies linearly with total depth between
c --- these two reference depths.
c
c --- previous to 2.2.58, it was layer thickness (not layer interface
c --- depth) that varied linearly with total depth.  These two approachs
c --- are identical for "pure sigma-z", but differ if ds00f/=dp00f.
c
      call blkini(nhybrd,'nhybrd')
      call blkini(nsigma,'nsigma')
      call blkinr2(dp00,k,
     &                   'dp00  ','(a6," =",f10.4," m")',
     &                   'dp0k  ','(a6," =",f10.4," m")' )
      if     (k.eq.1) then !dp00
        call blkinr(dp00x, 'dp00x ','(a6," =",f10.4," m")')
        call blkinr(dp00f, 'dp00f ','(a6," =",f10.4," ")')
        call blkinr(ds00,  'ds00  ','(a6," =",f10.4," m")')
        call blkinr(ds00x, 'ds00x ','(a6," =",f10.4," m")')
        call blkinr(ds00f, 'ds00f ','(a6," =",f10.4," ")')
      else !dp0k
        dp0k(1) = dp00
        dp00    = -1.0  !signal that dp00 is not input
        do k=2,kdm
          call blkinr(dp0k(k), 'dp0k  ','(a6," =",f10.4," m")')
c
          if      (k.gt.nhybrd .and. dp0k(k).ne.0.0) then
            if (mnproc.eq.1) then
            write(lp,'(/ a,i3 /)') 
     &        'error - dp0k must be zero for k>nhybrd'
            call flush(lp)
            endif !1st tile
            call xcstop('(blkdat)')
                   stop '(blkdat)'
          endif !k>nhybrd&dp0k(k)!=0
        enddo !k
        do k=1,nsigma
          call blkinr(ds0k(k), 'ds0k  ','(a6," =",f10.4," m")')
        enddo !k
      endif !dp00:dp0k
      call blkinr(dp00i, 'dp00i ','(a6," =",f10.4," m")')
      call blkinr(isotop,'isotop','(a6," =",f10.4," m")')
c
c --- isopycnal (MICOM-like) iff nhybrd is 0
      isopyc = nhybrd .eq. 0
      hybrid = .not. isopyc
      if (hybrid .and. nsigma.le.1) then
        nsigma=1
        if     (dp00.lt.0.0) then
          ds0k(1) = dp0k(1)
        endif
      endif
      if (isopyc .and. sigver.gt.2) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - MICOM-like requires the 7-term eqn. of state'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      if     (nhybrd.gt.kdm) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,i3 /)') 
     &    'error - maximum nhybrd is kdm =',kdm
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif !error
      if     (nsigma.gt.nhybrd) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,i3 /)') 
     &    'error - maximum nsigma is nhybrd =',nhybrd
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif !error
      if     (dp00.ge.0.0) then
        if (isopyc .and. max(dp00,dp00x).ne.0.0) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'error - must have dp00x==dp00==0.0 for isopycnal case'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif !error
        if (dp00f.eq.1.0 .and. dp00.ne.dp00x) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'error - must have dp00x==dp00 for dp00f==1.0'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif !error
        if (dp00f.gt.1.0 .and. dp00.ge.dp00x) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'error - dp00x must be > dp00 for dp00f>1'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif !error
        if (dp00f.lt.1.0 .and. dp00.le.dp00x) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'error - dp00x must be < dp00 for dp00f<1'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif !error
        if (ds00.gt.dp00) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'error - must have ds00 <= dp00'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif !error
        if (ds00.le.0.0 .and. .not.isopyc) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'error - must have ds00>0.0'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif !error
        if (ds00f.eq.1.0 .and. ds00.ne.ds00x) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'error - must have ds00x==ds00 for ds00f==1.0'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif !error
        if (ds00f.gt.1.0 .and. ds00.ge.ds00x) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'error - ds00x must be > ds00 for ds00f>1'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif !error
        if (ds00f.lt.1.0 .and. ds00.le.ds00x) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'error - ds00x must be < ds00 for ds00f<1'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif !error
        if (isotop.eq.0.0) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'error - isotop cannot be 0.0 (layer 1 never isopycnal)'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif !error
      endif !dp00 used
c
c --- 'saln0'  = initial salinity value (psu), only used for iniflg<2
c --- 'locsig' = locally-referenced potential density for stability (0=F,1=T)
c --- 'kapref' = thermobaric reference state (-1=input,0=none,1,2,3=constant)
c ---              1=Arctic/Antarctic; 2=Atlantic; 3=Mediterranean
c --- 'thflag' = reference pressure flag (0=Sigma-0, 2=Sigma-2)
c ---            this is a check on the compile-time stmt_funcs.h setup.
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkinr(saln0, 'saln0 ','(a6," =",f10.4," psu")')
      call blkinl(locsig,'locsig')
      call blkini(kapref,'kapref')
      call blkini(thflag,'thflag')
c --- kapnum is number of thermobaric reference states (1 or 2)
      if     (kapref.ne.-1) then
        kapnum=1
      else
        kapnum=2
      endif
c
      if     (kapref.lt.-1 .or. kapref.gt.3) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,i1 /)')
     &    'error - kapref must be between -1 and 3'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif !kapref error
      if     (thflag.eq.0) then
        if     (sigver.eq.1) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'equation of state is 7-term sigma-0'
          call flush(lp)
          endif !1st tile
        elseif (sigver.eq.3) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'equation of state is 9-term sigma-0'
          call flush(lp)
          endif !1st tile
        elseif (sigver.eq.5) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'equation of state is 17-term sigma-0'
          call flush(lp)
          endif !1st tile
        elseif (sigver.eq.7) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'equation of state is 12-term sigma-0'
          call flush(lp)
          endif !1st tile
        else
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'error - thflag not consistent with sig()'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif !7-term:9-term:error
      elseif (thflag.eq.2) then
        if     (sigver.eq.2) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'equation of state is 7-term sigma-2'
          call flush(lp)
          endif !1st tile
        elseif (sigver.eq.4) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'equation of state is 9-term sigma-2'
          call flush(lp)
          endif !1st tile
        elseif (sigver.eq.6) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'equation of state is 17-term sigma-2'
          call flush(lp)
          endif !1st tile
        elseif (sigver.eq.8) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'equation of state is 12-term sigma-2'
          call flush(lp)
          endif !1st tile
        else
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'error - thflag not consistent with sig()'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif !7-term:9-term:error
      else
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - thflag must be 0 or 2'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif !thflag
      if     (thflag.eq.0) then
        sigfmt = '(a6," =",f10.4," sigma-0")'
      elseif (thflag.eq.2) then
        sigfmt = '(a6," =",f10.4," sigma-2")'
      endif
c
c --- 'thbase' = reference density (sigma units)
      call blkinr(thbase,'thbase',sigfmt)
c
c --- 'vsigma' = spacially varying isopycnal layer target densities (0=F,1=T)
c ---            if true, target densities input from file iso.sigma.[ab]
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkinl(vsigma,'vsigma')
c
c --- 'sigma ' = isopycnal layer target densities (sigma units)
      do k=1,kdm
        call blkinr(sigma(k),'sigma ',sigfmt)
c
        if     (k.gt.1) then
          if      (sigma(k).le.sigma(k-1)) then
            if (mnproc.eq.1) then
            write(lp,'(/ a,i3 /)') 
     &        'error - sigma is not stabally stratified'
            call flush(lp)
            endif !1st tile
            call xcstop('(blkdat)')
                   stop '(blkdat)'
          endif !sigma(k).le.sigma(k-1)
        endif !k>1
      enddo !k
c
c --- 'iniflg' = initial state flag (0=level,1=zonal,2=climatology)
c --- 'jerlv0' = initial jerlov water type (1 to 5; 0 kpar, -1 chl)
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkini(iniflg,'iniflg')
      call blkini(jerlv0,'jerlv0')
c
      if (iniflg.lt.0 .or. iniflg.gt.3) then  !inicon==3 ok, for old .inputs
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - iniflg must be between 0 and 2'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (jerlv0.lt.-1 .or. jerlv0.gt.5) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - jerlv0 must be -1 or 0 or between 1 and 5'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
c --- 'yrflag' = days in year flag   (0=360,1=366,2=366Jan1,3=actual)
c ---             (-2=366Jan1 with 732-day forcing repeat)
c --- 'sshflg' = diagnostic SSH flag (0=SSH,1=SSH&stericSSH)
c ---             note that sshflg==1 implies reading relax.ssh.a
c --- 'dsurfq' = number of days between model diagnostics at the surface
c ---             (-ve to output only the .txt file at itest,jtest)
c --- 'diagfq' = number of days between model diagnostics
c ---             (-ve same as -diagfq but always write archive at end)
c --- 'proffq' = number of days between model diagnostics at selected locations
c --- 'tilefq' = number of days between model diagnostics on selected tiles
c --- 'meanfq' = number of days between model diagnostics (time averaged)
c --- 'rstrfq' = number of days between model restart output
c ---             (-ve same as -rstrfq but no restart at end)
c --- 'bnstfq' = number of days between baro nesting archive input
c ---             (-ve for mean archives spanning -bnstfq days;
c ---              0.0 if lbflag is not 2 or 4)
c --- 'nestfq' = number of days between 3-d  nesting archive input
c ---             (-ve for mean archives spanning -nestfq days;
c ---              0.0 turns off relaxation to 3-d nesting input)
c --- 'cplifq' = number of days (or time steps) between sea ice coupling
c ---             (positive days or negative time steps)
c --- 'baclin' = baroclinic time step (seconds), int. divisor of 86400
c --- 'batrop' = barotropic time step (seconds), int. divisor of baclin
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkini(yrflag,'yrflag')
      call blkini(sshflg,'sshflg')
      call blkinr(dsurfq,'dsurfq','(a6," =",f10.4," days")')
      call blkinr(diagfq,'diagfq','(a6," =",f10.4," days")')
      call blkinr(proffq,'proffq','(a6," =",f10.4," days")')
      call blkinr(tilefq,'tilefq','(a6," =",f10.4," days")')
      call blkinr(meanfq,'meanfq','(a6," =",f10.4," days")')
      call blkinr(rstrfq,'rstrfq','(a6," =",f10.4," days")')
      call blkinr(bnstfq,'bnstfq','(a6," =",f10.4,
     &                           " days (-ve mean span)")')
      call blkinr(nestfq,'nestfq','(a6," =",f10.4,
     &                           " days (-ve mean span)")')
      call blkinr(cplifq,'cplifq','(a6," =",f10.4,
     &                           " days (-ve time steps)")')
      call blkinr(baclin,'baclin','(a6," =",f10.4," sec")')
      call blkinr(batrop,'batrop','(a6," =",f10.4," sec")')
c
      if     (yrflag.eq.-2) then
        yrflag = 2
        wndrep = 732.0d0  !two year forcing repeat period
      elseif (yrflag.eq. 2) then
        wndrep = 366.0d0  !one year forcing repeat period
      endif
#if defined(RELO)
      if     (yrflag.lt.2) then
        natm = 4  !monthly forcing
      else
        natm = 2  !high-frequency forcing
      endif
#else
      if     (yrflag.lt.2) then
        if (natm.lt.4) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'error - natm must be 4 for monthly forcing (yrflag==0,1)'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif
      else
        if (natm.ne.2) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'warning - natm can be 2 for this forcing (yrflag!=0,1)'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif
      endif
#endif
c
      dsur1p = dsurfq .lt. 0.0
      if     (dsur1p) then
        dsurfq = -dsurfq
      endif
c
      arcend = diagfq.lt.0.0
      diagfq = abs(diagfq)
c
      if     (cplifq.ge.0.0) then
        icpfrq = nint( cplifq*(86400.0/baclin) )
      else
        icpfrq = nint(-cplifq)
      endif
      if (mnproc.eq.1) then
      write(lp,*)
      write(lp,'(a,i10)') 'icpfrq =',icpfrq
      endif !1st tile
c
      if (yrflag.lt.0 .or. yrflag.gt.3) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - yrflag must be between 0 and 3'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if     (yrflag.le.1) then  ! monthly forcing
        if     (abs(nint(86400.0/baclin)-86400.0/baclin).gt.0.01) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)')
     &      'error - baclin not an integer divisor of 24 hours'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        else  ! make it exact
*         write(lp,*) 'old baclin = ',baclin
          baclin = 86400.0/nint(86400.0/baclin)
*         write(lp,*) 'new baclin = ',baclin
        endif
      else  ! high frequency forcing
        if     (abs(nint(21600.0/baclin)-21600.0/baclin).gt.0.01) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)')
     &      'error - baclin not an integer divisor of 6 hours'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        else  ! make it exact
*         write(lp,*) 'old baclin = ',baclin
          baclin = 21600.0/nint(21600.0/baclin)
*         write(lp,*) 'new baclin = ',baclin
        endif
      endif
c --- assume btrlfr (leapfrog time step), converse tested later
      if     (abs(nint(baclin/batrop)-
     &                 baclin/batrop  ).gt.0.01) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - batrop not an integer divisor of baclin'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      else  ! make it exact
*       write(lp,*) 'old batrop = ',batrop
        batrop = baclin/nint(baclin/batrop)
*       write(lp,*) 'new batrop = ',batrop
      endif
      if     (abs(nint((dsurfq*86400.d0)/baclin)-
     &                 (dsurfq*86400.d0)/baclin  ).gt.0.01) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,a /)')
     &    'error - ',
     &    'dsurfq is not a whole number of baroclinic time steps'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      else  ! make it exact
*       write(lp,*) 'old dsurfq = ',dsurfq
        dsurfq = nint((dsurfq*86400.d0)/baclin)*(baclin/86400.d0)
*       write(lp,*) 'new dsurfq = ',dsurfq
      endif
      if     (abs(nint((diagfq*86400.d0)/baclin)-
     &                 (diagfq*86400.d0)/baclin  ).gt.0.01) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,a /)')
     &    'error - ',
     &    'diagfq is not a whole number of baroclinic time steps'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      else  ! make it exact
*       write(lp,*) 'old diagfq = ',diagfq
        diagfq = nint((diagfq*86400.d0)/baclin)*(baclin/86400.d0)
*       write(lp,*) 'new diagfq = ',diagfq
      endif
      if     (abs(nint((proffq*86400.d0)/baclin)-
     &                 (proffq*86400.d0)/baclin  ).gt.0.01) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,a /)')
     &    'error - ',
     &    'proffq is not a whole number of baroclinic time steps'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      else  ! make it exact
*       write(lp,*) 'old proffq = ',proffq
        proffq = nint((proffq*86400.d0)/baclin)*(baclin/86400.d0)
*       write(lp,*) 'new proffq = ',tilefq
      endif
      if     (abs(nint((tilefq*86400.d0)/baclin)-
     &                 (tilefq*86400.d0)/baclin  ).gt.0.01) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,a /)')
     &    'error - ',
     &    'tilefq is not a whole number of baroclinic time steps'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      else  ! make it exact
*       write(lp,*) 'old tilefq = ',tilefq
        tilefq = nint((tilefq*86400.d0)/baclin)*(baclin/86400.d0)
*       write(lp,*) 'new tilefq = ',tilefq
      endif
      if     (abs(nint((meanfq*86400.d0)/baclin)-
     &                 (meanfq*86400.d0)/baclin  ).gt.0.01) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,a /)')
     &    'error - ',
     &    'meanfq is not a whole number of baroclinic time steps'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      else  ! make it exact
*       write(lp,*) 'old meanfq = ',meanfq
        meanfq = nint((meanfq*86400.d0)/baclin)*(baclin/86400.d0)
*       write(lp,*) 'new meanfq = ',meanfq
      endif
#if defined(RELO)
      if (nestfq.ne.0.0) then
        kknest = kdm
      else
        kknest =   1
      endif
#else
      if (kknest.ne.kdm .and. nestfq.ne.0.0) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - kknest (dimensions.h) must be kdm when nesting'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
#endif
c
c --- 'incflg' = incremental update flag (0=no, 1=yes, 2=full-velocity)
c --- 'incstp' = no. timesteps for full update (1=full insertion)
c --- 'incupf' = number of days of incremental updating input
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkini(incflg, 'incflg')
      call blkini(incstp, 'incstp')
      call blkini(incupf, 'incupf')
c
c --- 'ra2fac' = weight for Robert-Asselin time filter
c ---             (old equivalent: wuv2=wts2=0.5*ra2fac,
c ---                              wuv2>wts2 no longer supported)
c --- 'wbaro ' = weight for time smoothing of barotropic fields
c --- 'btrlfr' = leapfrog barotropic time step (0=F,1=T)
c --- 'btrmas' = barotropic is mass conserving (0=F,1=T)
c --- 'hybraf' = HYBGEN:  Robert-Asselin flag  (0=F,1=T)
c ---             (use 'hybraf'=0 to recover pre-2.2.38 behaviour)
c --- 'hybrlx' = HYBGEN: inverse relaxation coefficient (time steps)
c                 (1.0 for no relaxation)
c --- 'hybiso' = HYBGEN: Use PCM if layer is within hybiso of target density
c                 (0.0 for no PCM; large to recover pre-2.2.09 behaviour)
c --- 'hybmap' = HYBGEN:  remapper  flag (0=PCM, 1=PLM,    2=PPM,  3=WENO-like)
c --- 'hybflg' = HYBGEN:  generator flag (0=T&S, 1=th&S,   2=th&T)
c --- 'advflg' = thermal  advection flag (0=T&S, 1=th&S,   2=th&T)
c --- 'advtyp' = scalar   advection type (0=PCM, 1=MPDATA, 2=FCT2, 4=FCT4)
c --- 'momtyp' = momentum advection type (2=2nd order, 4=4th order)
c --- 'slip'   = +1 for free-slip, -1  for non-slip boundary conditions
c --- 'visco2' = deformation-dependent Laplacian  viscosity factor
c --- 'visco4' = deformation-dependent biharmonic viscosity factor
c --- 'facdf4' =       speed-dependent biharmonic viscosity factor
c ---                   (diffusion velocity is facdf4*|v|)
c --- 'veldf2' = diffusion velocity (m/s) for Laplacian  momentum dissipation
c ---             (negative to input spacially varying diffusion velocity)
c --- 'veldf4' = diffusion velocity (m/s) for biharmonic momentum dissipation
c ---             (negative to input spacially varying diffusion velocity)
c --- 'thkdf2' = diffusion velocity (m/s) for Laplacian  thickness diffusion
c --- 'thkdf4' = diffusion velocity (m/s) for biharmonic thickness diffusion
c ---             (negative to input spacially varying diffusion velocity)
c --- 'temdf2' = diffusion velocity (m/s) for Laplacian  temp/saln diffusion
c --- 'temdfc' = temp diffusion conservation (0.0,1.0 all dens,temp resp.)
c --- 'vertmx' = diffusion velocity (m/s) for mom.mixing at mix.layr.base
c ---             (vertmx only used in MICOM-like isopycnal mode)
c --- 'cbar'   = rms flow speed     (m/s) for linear bottom friction
c ---             (negative to input spacially varying rms flow speed)
c --- 'cb'     = coefficient of quadratic bottom friction
c ---             (negative to input spacially varying coefficient)
c --- 'drglim' = limiter for explicit friction (1.0 no limiter, 0.0 implicit)
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkinr(ra2fac,'ra2fac','(a6," =",f10.4," ")')
      call blkinr(wbaro ,'wbaro ','(a6," =",f10.4," ")')
      call blkinl(btrlfr,'btrlfr')
      call blkinl(btrmas,'btrmas')
      call blkinl(hybraf,'hybraf')
      call blkinr(hybrlx,'hybrlx','(a6," =",f10.4," time steps")')
      call blkinr(hybiso,'hybiso','(a6," =",f10.4," kg/m^3")')
      call blkini(hybmap,'hybmap')
      call blkini(hybflg,'hybflg')
      call blkini(advflg,'advflg')
      call blkini(advtyp,'advtyp')
      call blkini(momtyp,'momtyp')
      call blkinr(slip,  'slip  ',
     &            '(a6," =",f10.4," (-1=no-slip, +1=free-slip)")')
      call blkinr(visco2,'visco2','(a6," =",f10.4," ")')
      call blkinr(visco4,'visco4','(a6," =",f10.4," ")')
      call blkinr(facdf4,'facdf4','(a6," =",f10.4," ")')
      call blkinr(veldf2,'veldf2','(a6," =",f10.4,
     &                          " m/s (-ve if variable)")')
      call blkinr(veldf4,'veldf4','(a6," =",f10.4,
     &                          " m/s (-ve if variable)")')
      call blkinr(thkdf2,'thkdf2','(a6," =",f10.4," m/s")')
      call blkinr(thkdf4,'thkdf4','(a6," =",f10.4,
     &                          " m/s (-ve if variable)")')
      call blkinr(temdf2,'temdf2','(a6," =",f10.4," m/s")')
      call blkinr(temdfc,'temdfc',
     &    '(a6," =",f10.4," (0.0,1.0 conserve dens,temp resp.)")')
      call blkinr(vertmx,'vertmx','(a6," =",f10.4," m/s")')
      call blkinr(cbar,  'cbar  ','(a6," =",f10.4,
     &                          " m/s (-ve if variable)")')
      call blkinr(cb,    'cb    ','(a6," =",f10.4,
     &                          "     (-ve if variable)")')
      call blkinr(drglim,'drglim','(a6," =",f10.4," ")')   
c
      if (hybrlx.lt.1.0) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - hybrlx must be at least 1.0'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      qhybrlx = 1.0/hybrlx
c
      if     (.not.btrlfr .and.
     &        abs(nint(0.5*baclin/batrop)-
     &                 0.5*baclin/batrop  ).gt.0.01) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - batrop not an integer divisor of baclin/2'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      if (btrmas) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - btrmas (.true.) not yet implemented'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      isopcm = hybiso.gt.0.0  !use PCM for isopycnal layers?
      if (hybmap.lt.0 .or. hybmap.gt.3) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,a /)') 
     &    'error - hybmap must be ',
     &    '0 (PCM) or 1 (PLM) or 2 (PPM) or 3 (WENO-like)'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (hybflg.lt.0 .or. hybflg.gt.2) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - hybflg must be 0 (T&S) or 1 (th&S) or 2 (th&T)'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (hybflg.ne.0 .and. (sigver.eq.5 .or. sigver.eq.6)) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - hybflg must be 0 (T&S) for 17-term eqn. of state'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (advflg.lt.0 .or. advflg.gt.2) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - advflg must be 0 (T&S) or 1 (th&S) or 2 (th&T)'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (advflg.ne.0 .and. (sigver.eq.5 .or. sigver.eq.6)) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - advflg must be 0 (T&S) for 17-term eqn. of state'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (advflg.eq.2) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - advflg==2 (th&T) not yet implemented'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (advtyp.ne.0 .and. advtyp.ne.1 .and.
     &    advtyp.ne.2 .and. advtyp.ne.4      ) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,a /)') 
     &   'error - advtyp must be 0,1,2,4',
     &   ' (PCM,MPDATA,FCT2,FCT4)'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (momtyp.ne.2 .and. momtyp.ne.4) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &   'error - montyp must be 2 or 4'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (momtyp.eq.2) then
        if (facdf4.ne.0.0) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &     'error - facdf4 must be 0.0 for montyp==2'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif
      endif
      if (momtyp.eq.4) then
        if (visco2.ne.0.0 .or. veldf2.ne.0.0) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &     'error - visco2 and veldf2 must be 0.0 for montyp==4'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif
      endif
      if (slip.ne.-1.0 .and. slip.ne.1.0) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - slip must be -1.0 (no-slip) or +1.0 (free-slip)'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (thkdf2.ne.0.0 .and. thkdf4.ne.0.0) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - only one of thkdf2 and thkdf4 can be non-zero'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (isopyc .and. temdfc.ne.0.0) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - isopycnal mode must have temdfc=0.0'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (temdfc.lt.0.0 .or. temdfc.gt.1.0) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - temdfc must be between 0.0 and 1.0'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (temdfc.ne.1.0 .and. (sigver.eq.5 .or. sigver.eq.6)) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - temdfc must be 1.0 (all T) for 17-term eqn. of state'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
c --- 'thkbot' = thickness of bottom boundary layer (m)
c --- 'sigjmp' = minimum density jump across interfaces  (kg/m**3)
c --- 'tmljmp' = equivalent temperature jump across mixed-layer (degC)
c --- 'thkmls' = reference mixed-layer thickness for SSS relaxation (m)
c --- 'thkmlt' = reference mixed-layer thickness for SST relaxation (m)
c --- 'thkriv' = nominal thickness of river inflow (m)
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkinr(thkbot,'thkbot','(a6," =",f10.4," m")')
      call blkinr(sigjmp,'sigjmp','(a6," =",f10.4," kg/m**3")')
      call blkinr(tmljmp,'tmljmp','(a6," =",f10.4," degC")')
      call blkinr(thkmls,'thkmls','(a6," =",f10.4," m")')
      call blkinr(thkmlt,'thkmlt','(a6," =",f10.4," m")')
      call blkinr(thkriv,'thkriv','(a6," =",f10.4," m")')
c
c --- 'thkcdw' = thickness for near-surface currents in ice-ocean stress (m)
c --- 'thkfrz' = maximum thickness of near-surface freezing zone (m)
c --- 'iceflg' = sea ice model flag (0=none,1=energy loan,1>coupled/esmf)
c ---             2=2-way,3=no IO stress, 4=3+no ocean currents
c ---             also, icmflg=3 for ENLN relaxed to coupler ice concentration
c --- 'tfrz_0' = ice melting point (degC) at S=0psu
c --- 'tfrz_s' = gradient of ice melting point (degC/psu)
c --- 'frzifq' = e-folding time scale back to tfrz
c ---             (positive days or negative time steps)
c --- 'ticegr' = ENLN: vertical temperature gradient inside ice (deg/m)
c ---                   (0.0 to get ice surface temp. from atmos. surtmp)
c --- 'hicemn' = ENLN: minimum ice thickness (m)
c --- 'hicemx' = ENLN: maximum ice thickness (m)
c --- 'ishelf' = ice shelf flag (0=none,1=ice shelf over ocean)
c
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkinr(thkcdw,'thkcdw','(a6," =",f10.4," m")')
      call blkinr(thkfrz,'thkfrz','(a6," =",f10.4," m")')
      call blkini(iceflg,'iceflg')
      icegln = iceflg.eq.1  !ENLN, but see icmflg.eq.3 below
c
      if (iceflg.lt.0 .or. iceflg.gt.4) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - iceflg must be between 0 and 4'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
c --- to recover original ENLN model use:
c --- tfrz_0=-1.8, tfrz_s=0.0,   ticegr=2.0, hicemn=0.5, hicemx=10.0.
c --- for freezing point linear in S, typically use:
c --- tfrz_0=0.0, tfrz_s=-0.054, ticegr=0.0, hicemn=0.5, hicemx=10.0.
c
      call blkinr(tfrz_0,'tfrz_0','(a6," =",f10.4," degC")')
      call blkinr(tfrz_s,'tfrz_s','(a6," =",f10.4," degC/psu")')
      call blkinr(frzifq,'frzifq','(a6," =",f10.4,
     &                           " days (-ve time steps)")')
      call blkinr(ticegr,'ticegr','(a6," =",f10.4," degC/m")')
      call blkinr(hicemn,'hicemn','(a6," =",f10.4," m")')
      call blkinr(hicemx,'hicemx','(a6," =",f10.4," m")')
c
      if     (frzifq.ge.0.0) then
        icefrq = nint( frzifq*(86400.0/baclin) )
      else
        icefrq = nint(-frzifq)
      endif
      if (mnproc.eq.1) then
      write(lp,*)
      write(lp,'(a,i10)') 'icefrq =',icefrq
      endif !1st tile
c
      call blkini(ishelf,'ishelf')
c
c --- 'ntracr' = number of tracers (0=none,negative to initialize)
c --- 'trcflg' = tracer flags      (one digit per tracer, most sig. replicated)
c ---              0: passive, 100% at surface
c ---              1: passive, psudo-silicate
c ---              2: passive, temperature
c ---              3: passive
c ---            4-8: unused
c ---              9: default biology (NPZ-3,NPZD-4,Chai-9)
c
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkini(ntracr,'ntracr')
      trcrin = ntracr.gt.0  ! positive from restart, otherwise initialize
      trcout = ntracr.ne.0
      ntracr = abs(ntracr)
c
      if (ntracr.gt.mxtrcr) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,i3, a /)') 
     &    'error - maximum ntracr is',mxtrcr,
     &    '  (recompile with larger mxtrcr)'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      call blkini(trcflg1,'trcflg')
      do k= 1,ntracr
        trcflg(k) = mod(trcflg1,10)  ! least significant decimal digit
        if     (trcflg1.ge.10) then
          trcflg1 = trcflg1/10  ! shift by one decimal digit
        else
          ! replicate last decimal digit across remaining tracers
        endif
        if (mnproc.eq.1) then
        write(lp,'(a,i3,i2)') '    k,trcflg =',k,trcflg(k)
        endif !1st tile
        if     (trcflg(k).gt.3 .and. trcflg(k).lt.9) then !not 0,1,2,3,9
          if (mnproc.eq.1) then
          write(lp,'(/ a,i3 /)') 
     &      'error - unknown tracer type for tracer',k
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif
      enddo !k
c
c --- 'tsofrq' = number of time steps between anti-drift offset calcs
c --- 'tofset' = temperature anti-drift offset (degC/century)
c --- 'sofset' = salnity     anti-drift offset  (psu/century)
c
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkini(tsofrq,'tsofrq')
      call blkinr(tofset,'tofset','(a6," =",f10.4," degC/century")')
      call blkinr(sofset,'sofset','(a6," =",f10.4,"  psu/century")')
c
c --- convert from per century to per second.
      tofset = tofset / (100.d0*365.d0*86400.d0)
      sofset = sofset / (100.d0*365.d0*86400.d0)
c
c --- 'mlflag' = mixed layer flag  (0=none,1=KPP,2-3=KTa-b,4=PWP,5=MY2.5,6=GISS)
c ---    'mxlkpp' = KPP:     activate mixed layer model (mlflag==1)
c ---    'mxlkrt' = KT:      MICOM or HYCOM Kraus-Turner (mlflag==2,3)
c ---    'mxlkta' = KT:      activate    original mixed layer model (mlflag==2)
c ---    'mxlktb' = KT:      activate alternative mixed layer model (mlflag==3)
c ---    'mxlpwp' = PWP:     activate mixed layer model (mlflag==4)
c ---    'mxlmy ' = MY:      activate mixed layer model (mlflag==5)
c ---   'mxlgiss' = GISS:    activate mixed layer model (mlflag==6)
c --- 'pensol' = KT/PWP   activate penetrating solar radiation (0=F,1=T)
c --- 'dtrate' = KT:      maximum permitted m.l. detrainment rate (m/day)
c --- 'thkmin' = KT/PWP:  minimum mixed-layer thickness (m)
c --- 'dypflg' = KT/PWP:  diapycnal mixing flag (0=none,1=KPP,2=explicit)
c ---                      always none (0) for MY/GISS and explicit (2) for PWP
c --- 'mixfrq' = KT/PWP:  number of time steps between diapycnal mixing calcs
c --- 'diapyc' = KT/PWP:  diapycnal diffusivity x buoyancy freq. (m**2/s**2)
c --- 'rigr'   = PWP:     critical gradient richardson number
c --- 'ribc'   = PWP:     critical bulk     richardson number
c --- 'rinfty' = KPP:     maximum  gradient richardson number (shear inst.)
c --- 'ricr'   = KPP:     critical bulk     richardson number
c --- 'bldmin' = KPP:     minimum surface boundary layer thickness (m)
c --- 'bldmax' = KPROF:   maximum surface boundary layer thickness (m)
c --- 'cekman' = KPP/KT:  scale factor for Ekman depth
c --- 'cmonob' = KPP:     scale factor for Monin-Obukov depth
c --- 'bblkpp' = KPP:     activate bottom boundary layer    (0=F,1=T)
c --- 'shinst' = KPP:     activate shear instability mixing (0=F,1=T)
c --- 'dbdiff' = KPP:     activate double diffusion  mixing (0=F,1=T)
c --- 'nonloc' = KPP:     activate nonlocal b. layer mixing (0=F,1=T)
c --- 'botdiw' = KPROF:   activate bot.enhan.int.wav mixing (0=F,1=T)
c --- 'difout' = KPROF:   output visc/diff coffs in archive (0=F,1=T)
c --- 'difsmo' = KPROF:   number of layers with horiz smooth diff coeffs
c --- 'difm0'  = KPP:     max viscosity   due to shear instability (m**2/s)
c --- 'difs0'  = KPP:     max diffusivity due to shear instability (m**2/s)
c --- 'difmiw' = KPP/MY:  background/internal wave viscosity       (m**2/s)
c ---             (negative to input spacially varying viscosity)
c --- 'difsiw' = KPP/MY:  background/internal wave diffusivity     (m**2/s)
c ---             (negative to input spacially varying diffusivity)
c --- 'dsfmax' = KPP:     salt fingering diffusivity factor        (m**2/s)
c --- 'rrho0'  = KPP:     salt fingering rp=(alpha*delT)/(beta*delS)
c --- 'cs'     = KPP:     value for nonlocal flux term
c --- 'cstar'  = KPP:     value for nonlocal flux term
c --- 'cv'     = KPP:     buoyancy frequency ratio (0.0 to use a fn. of N)
c --- 'c11'    = KPP:     value for turb velocity scale
c --- 'hblflg' = KPP:     b. layer interpolation flag (0=con.,1=lin.,2=quad.)
c --- 'niter'  = KPP:     iterations for semi-implicit soln. (2 recomended)
c --- 'langmr' = KPP:     Langmuir flag (0=no;1=Sul;2=Smy;3=Har;4=Tak)
c ---                       0:None
c ---                       1:McWilliams-Sullivan
c ---                       2:Smyth
c ---                       3:McWilliams-Harcourt
c ---                       4:Takaya
c
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkini(mlflag,'mlflag')
c
      if (mlflag.lt.0 .or. mlflag.gt.6) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - mlflag must be between 0 and 6'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      mxl_no = mlflag.eq.0
c
      if (isopyc .and. mlflag.ne.2) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - isopycnal mode requires KT mixed layer (mlflag=2)'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      mxlkta = mlflag.eq.2 .and. hybrid
      mxlktb = mlflag.eq.3 .and. hybrid
      mxlkrt = mlflag.eq.2 .or. mlflag.eq.3
      call blkinl(pensol,'pensol')
      call blkinr(dtrate,'dtrate','(a6," =",f10.4," m/day")')
      call blkinr(thkmin,'thkmin','(a6," =",f10.4," m")')
      call blkini(dypflg,'dypflg')
      call blkini(mixfrq,'mixfrq')
      call blkinr(diapyc,'diapyc','(a6," =",f10.4," m**2/s**2")')
c
      if (isopyc .and. pensol) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - isopycnal mode not consistent with pensol'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      if (dypflg.lt.0 .or. dypflg.gt.2) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - dypflg must be between 0 and 2'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      if ((mxlkta .or. mxlktb) .and.
     &    dypflg.ne.0 .and. thkriv.gt.0.0) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - thkriv>0 not consistent with KT unless dypflg=1'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      mxlmy = mlflag.eq.5
c
      if (mxlmy) then
#if ! defined(RELO)
        if (kkmy25.ne.kdm) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'error - kkmy25 (dimensions.h) must be kdm when mlflag==5'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif
#endif
        if     (dypflg.ne.0) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'warning - dypflg reset to 0 for M-Y 2.5'
          call flush(lp)
          endif !1st tile
          dypflg = 0
        endif
      endif
c
      mxlgiss = mlflag.eq.6
c
      if (mxlgiss) then
#if ! defined(RELO)
        if (nlgiss.ne.762) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'error - nlgiss (dimensions.h) must be 762 when mlflag==6'
          call flush(lp)
          endif !1st tile
          call xcstop('(blkdat)')
                 stop '(blkdat)'
        endif
#endif
        if     (dypflg.ne.0) then
          if (mnproc.eq.1) then
          write(lp,'(/ a /)') 
     &      'warning - dypflg reset to 0 for GISS'
          call flush(lp)
          endif !1st tile
          dypflg = 0
        endif
      endif
c
      mxlpwp = mlflag.eq.4
      call blkinr(rigc  ,'rigr  ','(a6," =",f10.4," ")')
      call blkinr(ribc  ,'ribc  ','(a6," =",f10.4," ")')
      call blkinr(qrinfy,'rinfty','(a6," =",f10.4," ")')
      qrinfy = 1.0/qrinfy
      call blkinr(ricr  ,'ricr  ','(a6," =",f10.4," ")')
c
      if (mxlpwp .and. dypflg.ne.2) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'warning - dypflg  reset to 2 for PWP mixed layer'
        call flush(lp)
        endif !1st tile
        dypflg = 2
      endif
c
      if (mxlpwp .and. thkriv.gt.0.0) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - thkriv>0 not consistent with PWP mixed layer'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      call blkinr(bldmin,'bldmin','(a6," =",f10.4," m")')
      call blkinr(bldmax,'bldmax','(a6," =",f10.4," m")')
c
      call blkinr(cekman,'cekman','(a6," =",f10.4," ")')
      call blkinr(cmonob,'cmonob','(a6," =",f10.4," ")')
c
      mxlkpp = mlflag.eq.1
      call blkinl(bblkpp,'bblkpp')
      call blkinl(shinst,'shinst')
      call blkinl(dbdiff,'dbdiff')
      call blkinl(nonloc,'nonloc')
      call blkinl(botdiw,'botdiw')
      call blkinl(difout,'difout')
      call blkini(difsmo,'difsmo') !now an integer
      call blkinr(difm0 ,'difm0 ','(a6," =",f10.4," m**2/s")')
      call blkinr(difs0 ,'difs0 ','(a6," =",f10.4," m**2/s")')
      call blkinr(difmiw,'difmiw','(a6," =",f10.4,
     &                          " m**2/s (-ve if variable)")')
      call blkinr(difsiw,'difsiw','(a6," =",f10.4,
     &                          " m**2/s (-ve if variable)")')
      call blkinr(dsfmax,'dsfmax','(a6," =",f10.4," m**2/s")')
      call blkinr(rrho0 ,'rrho0 ','(a6," =",f10.4," ")')
      call blkinr(cs    ,'cs    ','(a6," =",f10.4," ")')
      call blkinr(cstar ,'cstar ','(a6," =",f10.4," ")')
      call blkinr(cv    ,'cv    ','(a6," =",f10.4," ")')
      call blkinr(c11   ,'c11   ','(a6," =",f10.4," ")')
      call blkini(hblflg,'hblflg')
      call blkini(niter ,'niter ')
      call blkini(lngblk,'langmr')  !lngblk is local for testing 0 <= 4
c
#if defined(STOKES)
      langmr = lngblk
#else
      if     (lngblk.ne.0) then
        if (mnproc.eq.1) then
        write(lp,'(/ 2a /)') 
     &    'error - langmr must be =0 unless the macro STOKES',
     &    ' is defined at compile time'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
#endif   
      if (difsiw*difmiw.lt.0.0) then
        if (mnproc.eq.1) then
        write(lp,'(/ 2a /)') 
     &    'error - difmiw and difsiw must either both be positive or',
     &    ' both be negative'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      if (hblflg.lt.0 .or. hblflg.gt.2) then
        if (mnproc.eq.1) then
        write(lp,'(/ 2a /)') 
     &    'error - hblflg must be',
     &    ' 0 (constant) or 1 (linear) or 2 (quadratic)'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      if (mxlkpp) then
c ---   for KPP, diapyc and vertmx are not used
        dypflg = 0
        diapyc = 0.0
        vertmx = 0.0
      endif
      if (mxlmy) then
c ---   for M-Y, diapyc and vertmx are not used
        diapyc = 0.0
        vertmx = 0.0
      endif
      if (mxlgiss) then
c ---   for GISS, diapyc and vertmx are not used
        diapyc = 0.0
        vertmx = 0.0
      endif
      if (mxlpwp) then
c ---   for PWP, vertmx is not used
        vertmx = 0.0
      endif
      if (mxlkta .or. mxlktb) then
c ---   for HYCOM KT, vertmx is not currently used
        vertmx = 0.0
      endif
c
      if     (dypflg.eq.2) then
        if     (max(tofset,sofset).gt.0.0) then
          if     (diapyc.le.0.0) then
            if (mnproc.eq.1) then
            write(lp,'(/ a /)') 
     &        'error - diapyc must be positive if [ts]ofset is'
            call flush(lp)
            endif !1st tile
            call xcstop('(blkdat)')
                   stop '(blkdat)'
          elseif (mixfrq.ne.tsofrq) then
            if (mnproc.eq.1) then
            write(lp,'(/ a /)') 
     &        'error - mixfrq and tsofrq must be equal'
            call flush(lp)
            endif !1st tile
            call xcstop('(blkdat)')
                   stop '(blkdat)'
          endif !error checks
        endif ![ts]ofset>0.0
      endif !dypflg.eq.2
c
c --- 'fltflg' = FLOATS: synthetic float flag (0=no; 1=yes)
c --- 'nfladv' = FLOATS: advect every nfladv bacl. time steps (even, >=4)
c --- 'nflsam' = FLOATS: output (0=every nfladv steps; >0=# of days)
c --- 'intpfl' = FLOATS: horiz. interp. (0=2nd order+bilinear; 1=bilinear)
c --- 'iturbv' = FLOATS: add horiz. turb. advection velocity (0=no; 1=yes)
c --- 'ismpfl' = FLOATS: sample water properties at float (0=no; 1=yes)
c --- 'tbvar'  = FLOATS: horizontal turbulent velocity variance scale
c --- 'tdecri' = FLOATS: inverse decorrelation time scale
c
      call blkini(fltflg,'fltflg')
      synflt = fltflg.ge.1
      call blkini(nfladv,'nfladv')
      call blkini(nflsam,'nflsam')
      call blkini(intpfl,'intpfl')
      call blkini(iturbv,'iturbv')
      turbvel = iturbv.ge.1
      call blkini(ismpfl,'ismpfl')
      samplfl = ismpfl.ge.1
      call blkinr(tbvar ,'tbvar ','(a6," =",f10.4," m**2/s**2")')
      call blkinr(tdecri,'tdecri','(a6," =",f10.4," 1/day")')
c
c --- 'lbflag' = lateral baro. bndy flag (0=none;nest:2=B-K,4=Flather)
c ---             (port: 1=Browning-Kreiss,3=Flather)
c --- 'tidflg' = TIDES: tidal forcing flag    (0=no;1=bdy;2=body;3=bdy&body)
c --- 'tidein' = TIDES: tide field input flag (0=no;1=yes;2=sal)
c --- 'tidcon' = TIDES: 1 digit per constituent (Q1K2P1N2O1K1S2M2), 0=off,1=on
c --- 'tidsal' = TIDES: scalar self attraction and loading factor
c ---             (negative to input spacially varying SAL factor)
c --- 'tiddrg' = TIDES: tidal drag flag (0=no;1=scalar;2=tensor)
c --- 'thkdrg' = TIDES: thickness of bottom boundary layer for tidal drag (m)
c ---             (zero to apply tidal drag to barotropic mode)
c --- 'drgscl' = TIDES: scale factor for tidal drag
c --- 'tidgen' = TIDES: generic time (0=F,1=T)
c --- 'tidrmp' = TIDES:            ramp time (days)
c --- 'tid_t0' = TIDES: origin for ramp time (model day)
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkini(lbflag,    'lbflag')
      call blkini(tidflg,    'tidflg')
      call blkini(tidein,    'tidein')
      call blkini(tidcon,    'tidcon')
      call blkinr(tidsal,    'tidsal','(a6," =",f10.4,
     &                              "  (-ve if variable)")')
      call blkini(tiddrg,    'tiddrg')
      call blkinr(thkdrg,    'thkdrg','(a6," =",f10.4,
     &                              " m (0 if barotropic)")')
      call blkinr(drgscl,    'drgscl','(a6," =",f10.4," ")')
      call blkinl(tidgen,    'tidgen')
      call blkinr(ramp_time ,'tidrmp','(a6," =",f10.4," days")')
      call blkin8(ramp_orig ,'tid_t0','(a6," =",f10.4," model day")')  !real*8
c
      if     (tidflg.lt.0 .or. tidflg.gt.3) then
        if     (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - tidflg must be between 0 and 3'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if     (tidflg.ge.2 .and. .not.tidgen .and. yrflag.ne.3) then
        if     (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - tidgen must be .true. for body tide and yrflag.ne.3'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if     (tidflg.gt.0) then
        if     (abs(nint(3600.0/baclin)-3600.0/baclin).gt.0.01) then
          if (mnproc.eq.1) then                                     
          write(lp,'(/ a /)')                                       
     &      'error - baclin not an integer divisor of 1 hour'
          call flush(lp)                                     
          endif !1st tile                                    
          call xcstop('(blkdat)')
                 stop '(blkdat)' 
        else  ! make it exact    
*         write(lp,*) 'old baclin = ',baclin
          baclin = 3600.0/nint(3600.0/baclin)
*         write(lp,*) 'new baclin = ',baclin 
        endif                                
      endif                                 
      if     (tidein.lt.0 .or. tidein.gt.2) then
        if     (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - tidein must be between 0 and 2'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if     (tidein.gt.0 .and. tidflg.lt.2) then
        if     (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - tidein implies input but there is no body tide'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if     (tiddrg.lt.0 .or. tiddrg.gt.2) then
        if     (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - tiddrg must be between 0 and 2'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if     (tiddrg.eq.0 .and. drgscl.ne.0.0) then
        if     (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - tiddrg=0 must have drgscl=0.0'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if     (tiddrg.ne.0 .and. drgscl.le.0.0) then
        if     (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - tiddrg>0 must have drgscl>0.0'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if     (tiddrg.eq.2 .and. thkdrg.ne.0.0) then
c ---   tensor drag only implemented on barotropic velocity
        if     (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - tiddrg=2 only implemented for thkdrg=0.0'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
c --- 'clmflg' = climatology frequency flag (6=bimonthly,12=monthly)
c --- 'wndflg' = wind stress input flag (0=none,1=uv-grid,2,3=p-grid,4,5=wnd10m)
c ---             (=3 wind speed from wind stress; =4,5 wind stress from wind)
c ---             (=4 for COARE 3.0; =5 for COREv2 bulk parameterization)
c --- 'ustflg' = ustar forcing   flag          (3=input,1,2=wndspd,4=stress)
c --- 'flxflg' = thermal forcing flag   (0=none,3=net_flux,1-2,4-6=sst-based)
c ---             (=1 MICOM bulk parameterization)
c ---             (=2 Kara  bulk parameterization)
c ---             (=4 COARE bulk parameterization, approx.)
c ---             (=5 L&Y   bulk parameterization)
c ---             (=6 COARE bulk parameterization, approx., better pressure)
c --- 'empflg' = E-P     forcing flag   (0=none,3=net_E-P, 1-2,4-6=sst-based_E)
c ---             (flxflg to use model sst, -flxflg to use seatmp)
c --- 'dswflg' = diurnal shortwv flag   (0=none,1=daily to diurnal correction)
c --- 'albflg' = ocean albedo    flag   (0=none,1=const,2=L&Y)
c --- 'sssflg' = SSS relaxation  flag   (0=none,1=clim,-1=clim&rmx)
c ---              see thermf.f for a description of sssrmx
c --- 'lwflag' = longwave corr.  flag   (0=none,1=clim,2=nwp,-1=lwdn), sst-based
c --- 'sstflg' = SST relaxation  flag   (0=none,1=clim,2=atmos,3=observed)
c --- 'icmflg' = ice mask        flag   (0=none,1=clim,2=atmos,3=obs/coupled)
c --- 'prsbas' = msl prs is input field + prsbas (Pa)
c --- 'mslprf' = msl prs forcing flag   (0=F,1=T)
c --- 'stroff' = net strs offset flag   (0=F,1=T)
c --- 'flxoff' = net flux offset flag   (0=F,1=T)
c --- 'flxsmo' = smooth surface fluxes  (0=F,1=T)
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkini(clmflg,'clmflg')
      call blkini(wndflg,'wndflg')
      call blkini(ustflg,'ustflg')
      call blkini(flxflg,'flxflg')
      call blkini(empflg,'empflg')
      call blkini(dswflg,'dswflg')
      call blkini(albflg,'albflg')
      call blkini(sssflg,'sssflg')
      call blkini(lwflag,'lwflag')
      call blkini(sstflg,'sstflg')
      call blkini(icmflg,'icmflg')
      call blkinr(prsbas,'prsbas','(a6," =",f10.1," Pa")')
      call blkinl(mslprf,'mslprf')
      call blkinl(stroff,'stroff')
      call blkinl(flxoff,'flxoff')
      call blkinl(flxsmo,'flxsmo')
c
      if (clmflg.ne.6 .and. clmflg.ne.12) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - clmflg must be 6 or 12'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
#if defined(RELO)
      if (lbflag.eq.1 .or. lbflag.eq.3) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - lbflag 1 and 3 not supported with dynamic memory'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
#endif
      if (lbflag.lt.0 .or. lbflag.gt.4) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - lbflag must be between 0 and 4'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (lbflag.ne.2 .and. lbflag.ne.4 .and. bnstfq.ne.0.0) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - bnstfq must be 0.0 unless lbflag is 2 or 4'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (wndflg.lt.0 .or. wndflg.gt.5) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - wndflg must be between 0 and 5'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (ustflg.lt.1 .or. ustflg.gt.4) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - ustflg must be between 1 and 4'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (flxflg.lt.0 .or. flxflg.gt.6) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - flxflg must be between 0 and 6'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (flxflg.gt.0 .and.
     &    wndflg.eq.0      ) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - wndflg must be non-zero when flxflg>0'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (flxflg.eq.6 .and.
     &    wndflg.ne.4      ) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - wndflg must be 4 when flxflg==6'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (flxflg.eq.3 .and.   
     &    ustflg.eq.4      ) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - ustflg must be 1,2,3 when flxflg==3'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif    
      if (abs(empflg).lt.0 .or. abs(empflg).gt.6) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - empflg must be between 0 and 6'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (flxflg.eq.3 .and. (empflg.ne.0 .and. empflg.ne.3)) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - empflg must be 0 or 3 when flxflg==3'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (abs(empflg).ne.flxflg .and. empflg.ne.0
     &                          .and. empflg.ne.3) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - empflg must be 0 or 3 or +\-flxflg'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (empflg.lt.0 .and. sstflg.lt.2 .and. lwflag.ne.2) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,a /)') 
     &    'error - negative empflg requires sst forcing',
     &    ' (i.e. sstflg=3 or sstflg=2 or lwflag=2)'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (flxflg.eq.0 .and.
     &    empflg.ne.0      ) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - empflg must be 0 when flxflg==0'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (flxflg.eq.3 .and.
     &    dswflg.eq.1      ) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - dswflg must be 0 when flxflg==3'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      if (albflg.gt.0 .and. lwflag.ne.-1) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - albflg>0 requires lwflag=-1'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (lwflag.lt.-1 .or. lwflag.gt.2) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - lwflag must be between -1 and 2'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (lwflag.ne.0 .and.
     &    flxflg.eq.0      ) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - flxflg must non-zero when lwflag is non-zero'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      if (wndflg.eq.0) then
        stroff = .false. !no wind forcing
      endif
c
      if (flxflg.eq.0) then
        flxoff = .false. !no thermal forcing
      endif
c
      if (iceflg.eq.0) then
        icmflg = 0   !no ice
        ticegr = 2.0 !surtmp not needed for ice temperature
      elseif (iceflg.ge.2) then
        icegln = icmflg.eq.3  !ENLN plus relax to coupler ice concentration
      endif
c
      if (icmflg.lt.0 .or. icmflg.gt.3) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - icmflg must be between 0 and 3'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      elseif (icmflg.eq.1) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - icmflg 1 not yet implemented'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      elseif (icmflg.eq.3 .and. iceflg.eq.1) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)')
     &    'error - icmflg 3 not yet implemented for iceflg==1'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      if (icmflg.eq.3 .and. icefrq.lt.icpfrq) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - icefrq (frzifq) smaller than icpfrq (cplifq)'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      if (sstflg.lt.0 .or. sstflg.gt.3) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - sstflg must be between 0 and 3'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (sstflg.gt.1 .and.
     &    yrflag.lt.2      ) then
        if (mnproc.eq.1) then
        write(lp,'(/ a,a /)') 
     &    'error - yrflag must be >1 (high frequency forcing)',
     &    ' when sstflg>1'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      if (sssflg.lt.-1 .or. sssflg.gt.2) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - sssflg must be 0 or 1 or -1'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
c ---  s w i t c h e s    (if set to .true., then...)
c ---  (due to a SGI bug: read in an integer with 0=F,1=T)
c --- windf       use wind stress forcing (wndflg>0)
c --- thermo      use thermodynamic forcing 
c --- pensol      use penetrating solar radiation (input above)
c --- pcipf       use E-P forcing (may be redefined in forfun)
c --- priver      rivers as a precipitation bogas
c --- epmass      treat evap-precip as a mass exchange
c
c --- srelax      activate surface salinity        climatological nudging
c --- trelax      activate surface temperature     climatological nudging
c --- relax       activate lateral boundary T/S/p  climatological nudging
c --- trcrlx      activate lateral boundary tracer climatological nudging
c --- relaxt      input tracer  climatological relaxation fields
c --- relaxf      input T/S/p   climatological relaxation fields
c ---              (note that relaxt implies relaxf)
c --- relaxs      input surface climatological relaxation fields only
c
      windf  = wndflg.ne.0
      thermo = flxflg.ne.0
      pensol = pensol .and. thermo
      pcipf  = empflg.ne.0  ! if .true., might later be set .false. by forfun
c
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
      call blkinl(relax, 'relax ')
      call blkinl(trcrlx,'trcrlx')
      call blkinl(priver,'priver')
      call blkinl(epmass,'epmass')
      if (mnproc.eq.1) then
      write(lp,*)
      endif !1st tile
c
      if     (priver .and. .not.thermo) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - priver must be .false. for flxflg=0'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      if     (epmass .and. .not.thermo) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - epmass must be .false. for flxflg=0'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
c
      srelax = sssflg.eq.1 .or. sssflg.eq.-1
      trelax = sstflg.eq.1
c
      if (.not.(thermo .or. sstflg.gt.0 .or. srelax)) then
        niter=1
      elseif (mxlkta) then
        if (mnproc.eq.1) then
        write(lp,'(/ a / a /)') 
     &    'error - KT mixed layer needs thermal forcing, i.e.',
     &    '        mlflag=2 needs max(sstflg,sstflg,flxflg)>0'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif !.not(thermo ...):mxlkta
c
      relaxt = trcout .and. (trcrlx .or.
     &                       (.not.trcrin .and. iniflg.eq.2))
      relaxf =       relax
     &          .or. srelax
     &          .or. trelax
     &          .or. lwflag.eq.1
     &          .or. relaxt
      relaxs = .not. relax
     &         .and. relaxf
c
c
c ---  list of parameters for scaling atmsopheric forcings 
c
c --- 'radfac' = scale Net Total/ShortWave radiational flux (0. to xx)
c --- 'thefac' = scale Sensible and Latent Heat flux (0. to xx)
c --- 'atmfac' = scale Momentum flux (0. to xx)
c --- 'momfac' = scale Momentum flux (0. to xx)
c
c     applying these each coupling time.
c
      call blkinr(radfac,'radfac','(a6," =",f10.4," ")')
      call blkinr(thefac,'thefac','(a6," =",f10.4," ")')
      call blkinr(atmfac,'atmfac','(a6," =",f10.4," ")')
      call blkinr(momfac,'momfac','(a6," =",f10.4," ")')
c
c
#if defined(STOKES)
c
c ---  s w i t c h e s  for Stokes Drift effects  (0=F, 1=T)
c
c --- 'stdflg'  = STOKES: add Stokes Drift velocities to kinematics and dynamics
c --- 'stdsur'  = STOKES: add Stokes Drift Surface Stresses  to dynamics
c --- 'stdbot'  = STOKES: add Stokes Waves Field bottom friction drag
c --- 'stdarc'  = STOKES: Stokes Drift Velocities in Archive
c
c ---  s w i t c h e s  for Stokes Drift effects  (Integers)
c
c --- 'nsdzi '  = STOKES: number of interfaces in Stokes Drift input
c
c --- since they are at the end of blkdat.input they can optionally be
c --- present even if the macro STOKES is not defined at compile time.
c
      call blkinl(stdflg,'stdflg')
      call blkinl(stdsur,'stdsur') 
      call blkinl(stdbot,'stdbot')
      call blkinl(stdarc,'stdarc')
      call blkini(nsdzi ,'nsdzi ')
c
      if (nsdzi .lt.0) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &  'error - nsdzi  must be 0 (no Stokes drift) or positive'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (nsdzi .eq.0 .and. stdflg) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &  'error - nsdzi  must be > 0 with stdflg'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (nsdzi .eq.0 .and. stdbot) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &  'error - nsdzi  must be > 0 with stdbot'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
      if (nsdzi .eq.0 .and. langmr.ne.0) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &  'error - nsdzi  must be > 0 with langmr >0'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
#endif   
c
      close(unit=uoff+99)  !file='blkdat.input'
c
c --- initialize from climatology (update relaxf and relaxs)?
      if     (iniflg.eq.2) then
#if defined(USE_CCSM3)
        relaxf = .true.
        relaxs = .false.
#else
        open(unit=uoff+99,file=trim(flnminp)//'limits')  !on all nodes
        read(uoff+99,*) day1
        close(unit=uoff+99) !file='limits'
        if     (day1.le.0.0) then
          relaxf = .true.
          relaxs = .false.
        endif !initialize from climatology
#endif /* USE_CCSM3:else */
      endif
c
#if defined(RELO)
      if (relaxf .and. .not. relaxs) then
        kkwall = kdm
      elseif (relaxt) then
        kkwall = kdm  !needed for tracers
      else
        kkwall =   1
      endif
      if (mnproc.eq.1) then
      write(lp,'(a,i10)') 'kkwall =',kkwall
      write(lp,*)
      endif !1st tile
#else
      if (kkwall.ne.kdm .and. relaxf .and. .not. relaxs) then
        if (mnproc.eq.1) then
        write(lp,'(/ a /)') 
     &    'error - kkwall (dimensions.h) must be kdm for 3-d clim'
        call flush(lp)
        endif !1st tile
        call xcstop('(blkdat)')
               stop '(blkdat)'
      endif
#endif
#if defined(USE_CCSM3)
c
c --- i/o file names
c --- directory names (eg, flnmgrdd) are read in from namelist in ccsm3_io
c
      flnmshlf   = trim(flnmdepd)//'regional.iceshelf'
      flnmdep    = trim(flnmdepd)//'regional.depth'
      flnmgrd    = trim(flnmgrdd)//'regional.grid'
      flnmarc    = trim(flnmarcd)//'archv.0000_000_00'
      flnmarcs   = trim(flnmarcd)//'archs.0000_000_00'
      flnmarcm   = trim(flnmarcd)//'archm.0000_000'
      flnmarct   = trim(flnmarcd)//'archt.0000_000_00'
      flnmovr    = trim(flnmovrd)//'ovrtn.txt'
      flnmflx    = 'flxdp'
c     
c --- i/o directory names
c
      flnmforw = trim(flnmrlxd)
      flnmfor  = trim(flnmrlxd)
#elif defined(OCEANS2)
      if     (nocean.eq.2) then
c ---   slave HYCOM works from ./OCEAN2
c
c ---   i/o file names
c
        flnmshlf = './OCEAN2/regional.iceshelf'
        flnmdep  = './OCEAN2/regional.depth'
        flnmgrd  = './OCEAN2/regional.grid'
        flnmarc  = './OCEAN2/archv.0000_000_00'
        flnmarcs = './OCEAN2/archs.0000_000_00'
        flnmarcm = './OCEAN2/archm.0000_000_00'
        flnmarct = './OCEAN2/archt.0000_000_00'
        flnmovr  = './OCEAN2/ovrtn_out'
        flnmflx  = './OCEAN2/flxdp_out'
        flnmrsi  = './OCEAN2/restart_in'
        flnmrso  = './OCEAN2/restart_out'
c
c --- i/o directory names
c
        flnmfor  = './OCEAN2/'
        flnmforw = './OCEAN2/'
      else
c ---   master HYCOM
c
c ---   i/o file names
c
        flnmshlf = 'regional.iceshelf'
        flnmdep  = 'regional.depth'
        flnmgrd  = 'regional.grid'
        flnmarc  = 'archv.0000_000_00'
        flnmarcs = 'archs.0000_000_00'
        flnmarcm = 'archm.0000_000_00'
        flnmarct = 'archt.0000_000_00'
        flnmovr  = 'ovrtn_out'
        flnmflx  = 'flxdp_out'
        flnmrsi  = 'restart_in'
        flnmrso  = 'restart_out'
c
c --- i/o directory names
c
        flnmfor  = './'
        flnmforw = './'
      endif
#else
c
c --- i/o file names
c
      flnmshlf = 'regional.iceshelf'
      flnmdep  = 'regional.depth'
      flnmgrd  = 'regional.grid'
      flnmarc  = 'archv.0000_000_00'
      flnmarcs = 'archs.0000_000_00'
      flnmarcm = 'archm.0000_000_00'
      flnmarct = 'archt.0000_000_00'
      flnmovr  = 'ovrtn_out'
      flnmflx  = 'flxdp_out'
      flnmrsi  = 'restart_in'
      flnmrso  = 'restart_out'
c
c --- i/o directory names
c
      flnmfor  = './'
      flnmforw = './'
#endif /* USE_CCSM3:OCEANS2:else */
      return
      end
      subroutine blkinr(rvar,cvar,cfmt)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      real      rvar
      character cvar*6,cfmt*(*)
c
c     read in one real value
c
      character*6 cvarin
c
      read(uoff+99,*) rvar,cvarin
      if (mnproc.eq.1) then
      write(lp,cfmt) cvarin,rvar
      call flush(lp)
      endif !1st tile
c
      if     (cvar.ne.cvarin) then
        if (mnproc.eq.1) then
        write(lp,*) 
        write(lp,*) 'error in blkinr - input ',cvarin,
     +                       ' but should be ',cvar
        write(lp,*) 
        call flush(lp)
        endif !1st tile
        call xcstop('(blkinr)')
               stop
      endif
      return
      end
      subroutine blkinr2(rvar,nvar,cvar1,cfmt1,cvar2,cfmt2)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      real      rvar
      integer   nvar
      character cvar1*6,cvar2*6,cfmt1*(*),cfmt2*(*)
c
c     read in one real value
c     identified as either cvar1 (return nvar=1) or cvar2 (return nvar=2)
c
      character*6 cvarin
c
      read(uoff+99,*) rvar,cvarin
      if     (cvar1.eq.cvarin) then
        nvar = 1
        if (mnproc.eq.1) then
        write(lp,cfmt1) cvarin,rvar
        call flush(lp)
        endif !1st tile
      elseif (cvar2.eq.cvarin) then
        nvar = 2
        if (mnproc.eq.1) then
        write(lp,cfmt2) cvarin,rvar
        call flush(lp)
        endif !1st tile
      else
        if (mnproc.eq.1) then
        write(lp,*)
        write(lp,*) 'error in blkinr2 - input ',cvarin,
     +                      ' but should be ',cvar1,' or ',cvar2
        write(lp,*)
        call flush(lp)
        endif !1st tile
        call xcstop('(blkinr2)')
               stop
      endif
      return
      end
      subroutine blkin8(rvar,cvar,cfmt)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      real*8    rvar
      character cvar*6,cfmt*(*)
c
c     read in one real*8 value
c
      character*6 cvarin
c
      read(uoff+99,*) rvar,cvarin
      if (mnproc.eq.1) then
      write(lp,cfmt) cvarin,rvar
      call flush(lp)
      endif !1st tile
c
      if     (cvar.ne.cvarin) then
        if (mnproc.eq.1) then
        write(lp,*) 
        write(lp,*) 'error in blkin8 - input ',cvarin,
     +                       ' but should be ',cvar
        write(lp,*) 
        call flush(lp)
        endif !1st tile
        call xcstop('(blkin8)')
               stop
      endif
      return
      end
      subroutine blkini(ivar,cvar)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer     ivar
      character*6 cvar
c
c     read in one integer value
c
      character*6 cvarin
c
      read(uoff+99,*) ivar,cvarin
      if (mnproc.eq.1) then
      write(lp,6000) cvarin,ivar
      call flush(lp)
      endif !1st tile
c
      if     (cvar.ne.cvarin) then
        if (mnproc.eq.1) then
        write(lp,*) 
        write(lp,*) 'error in blkini - input ',cvarin,
     +                       ' but should be ',cvar
        write(lp,*) 
        call flush(lp)
        endif !1st tile
        call xcstop('(blkini)')
               stop
      endif
      return
 6000 format(a6,' =',i10)
      end
      subroutine blkinl(lvar,cvar)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      logical     lvar
      character*6 cvar
c
c     read in one logical value
c     due to a SGI bug for logical I/O: read in an integer 0=F,1=T
c
      character*6 cvarin
      integer     ivar
c
      read(uoff+99,*) ivar,cvarin
      lvar = ivar .ne. 0
      if (mnproc.eq.1) then
      write(lp,6000) cvarin,lvar
      call flush(lp)
      endif !1st tile
c
      if     (cvar.ne.cvarin) then
        if (mnproc.eq.1) then
        write(lp,*) 
        write(lp,*) 'error in blkinl - input ',cvarin,
     +                       ' but should be ',cvar
        write(lp,*) 
        call flush(lp)
        endif !1st tile
        call xcstop('(blkinl)')
               stop
      endif
      return
 6000 format(a6,' =',l10)
      end
c>
c> Revision history
c>
c> Oct. 1999 - added variables to control penetrating solar radiation
c> Oct. 1999 - added switch to select mixed layer model
c> Oct. 1999 - dp00 for hybgen is now set here
c> Dec. 1999 - multiple heat flux transfer coefficients (cts1, cts2, ctl)
c>             replace old coefficient ct
c> Jan. 2000 - changed to subroutine with run-time input
c> May. 2000 - conversion to SI units
c> Nov. 2000 - added kapflg,thflag,hybflg,advflg,wndflg
c> Dec. 2000 - added flxflg
c> Aug. 2001 - added bnstfq,nestfq
c> Aug. 2001 - added mapflg==4 for an f-plane
c> Aug. 2001 - betard and betabl inverted to replace division by multiply
c> May  2002 - map projection via regional.grid file (see geopar.f)
c> May  2002 - vertical coordinate via d[ps]00*
c> May  2002 - diffusion variable names include 2/4 for Laplacian/biharmonic
c> May  2002 - added PWP and MY2.5 mixed layer options
c> May  2002 - added thkmlr, distinct from thkmin
c> Aug  2002 - added ntracr and trcflg to control tracers
c> Nov  2002 - added jerlv0=0 to use kpar-based turbidity
c> Nov  2002 - split thkmlr into thkmls and thkmlt
c> Apr  2003 - added dp00i, vsigma, and priver
c> May  2003 - added bldmin, bldmax, flxsmo, and icmflg
c> Jun  2003 - added locsig and removed thflag=4
c> Oct  2003 - thkdf4 negative now signals spacial variation
c> Nov  2003 - added advtyp
c> Jan  2004 - added latdiw
c> Jan  2004 - added bblkpp
c> Jan  2004 - added hblflg and cv=0.0 option
c> Feb  2004 - added botdiw (and latdiw) for GISS
c> Feb  2004 - added temdfc
c> Mar  2004 - added thkriv and epmass
c> Mar  2004 - added isotop
c> Mar  2005 - added tfrz_0,tfrz_s,ticegr,hicemn,hicemx
c> Mar  2005 - added tsofrq,tofset,sofset
c> Mar  2005 - added empflg
c> Mar  2005 - added ustflg, reordered thermal forcing flags
c> Mar  2005 - added flxoff
c> Apr  2005 - replaced kapflg with kapref
c> Jun  2005 - added hybrlx
c> Jun  2006 - added cplifq, thkfrz and negative dsurfq option
c> Nov  2006 - version 2.2
c> Nov  2006 - added incflg,incstp,incupf
c> Nov  2006 - added FLOATS (fltflg,...)
c> Nov  2006 - added TIDES  (tidflg,tidrmp,tid_t0,lbflag==3)
c> Mar  2007 - added drgscl
c> Mar  2007 - added drglim
c> Mar  2007 - added tidcon,tidsal,tidgen
c> Apr  2007 - implemented meanfq 
c> Apr  2007 - added btrlfr and btrmas (latter not yet implemented)
c> May  2007 - added wbaro
c> Jun  2007 - moved h1 to momtum
c> Jun  2007 - added momtyp and facdf4
c> Sep  2007 - added hybmap and hybiso
c> Feb  2008 - added thkdrg
c> Feb  2008 - added sshflg
c> Jun  2008 - added tilefq
c> Jul  2008 - added hybmap=3 (WENO-like)
c> Oct  2008 - added dp0k and ds0k input option
c> Oct  2008 - added dswflg
c> Dec  2008 - difsmo is now an integer number of layers
c> Jan  2009 - added -ve diagfq (arcend)
c> Jun  2009 - added sssrmx
c> Mar  2010 - added negative bnstfq and nestfq for mean nesting archives
c> Apr  2010 - added sstflg=-1 for sssrmx array input (no sssrmx blkdat entry)
c> Apr  2010 - removed latdiw
c> Apr  2010 - added proffq
c> Oct  2010 - added support for 17-term and 12-term equation of state
c> Nov  2010 - added yrflag=-2
c> Nov  2010 - empflg negative to use seatmp instead of hycom sst
c> Apr  2011 - added negative cbar 
c> Jul  2011 - added negative tidsal
c> Aug  2011 - added ra2fac, removed global wts[12] and wuv[12]
c> Aug  2011 - added hybraf
c> Sep  2011 - added negative cb
c> Nov  2011 - iniflg=2 now active for yrflag=3
c> Nov  2011 - added frzifq
c> Jan  2012 - added thkcdw
c> Jan  2012 - added lbflag=4
c> Mar  2012 - new terrain following method, based on dpns and dsns
c> Apr  2012 - added negative tidflg, to set tidef
c> May  2012 - added tidein, replaces negative tidflg
c> Nov  2012 - added OCEANS2 macro for master/slave HYCOM
c> Nov  2012 - added wndlfg=4 for 10m wind component input
c> Nov  2012 - added stroff, usualy used with wndflg=4
c> Jan  2013 - added tiddrg, set to 0 or 1 for backwards compatibility
c> Jan  2013 - added zero thkdrg to apply tidal drag to barotropic mode
c> June 2013 - added lbflag=6
c> July 2013 - added negative (spacially varying) difmiw and difsiw
c> July 2013 - added 3 Stokes Drift Flags: stkflg,stksur,stkbot
c> Aug  2013 - added langmr flag for KPP and Stokes Drift
c> Sep  2013 - added number of interfaces in Stokes Drift Files 
c> Oct  2013 - added jerlv0=-1 to use chlorophyll-based turbidity
c> Oct  2013 - fixed srelax used before defined bug
c> Nov  2013 - added wndlfg=5 for 10m wind component input, COREv2 stress
c> Nov  2013 - added flxlfg=5 for COREv2 heat flux
c> Nov  2013 - added lwflag=-1 for radflx=Qlwdn
c> Nov  2013 - added albflg for ocean albedo, >0 for shwflx=Qswdn
c> Jan  2014 - added prsbas and mslprf for sea level pressure forcing
c> Jan  2014 - /consts/ replaced by parameters
c> Jan  2014 - kdm    is defined here when macro /* RELO */ is set
c> Jan  2014 - natm   is defined here when macro /* RELO */ is set
c> Jan  2014 - kkwall is defined here when macro /* RELO */ is set
c> Jan  2014 - kknest is defined here when macro /* RELO */ is set
c> Jan  2014 - lbflag 1 and 3 not supported with dynamic memory allocation
c> Apr  2014 - added ishelf and flnmshlf
c> May  2014 - removed lbflag=6
c> Sep  2014 - langmr can now be 0 to 4
c> Oct  2014 - added flxlfg=6, sea level pressure is input even if .not. mslprf
c> May  2015 - when btrlfr is set, batrop must be an int. divisor of baclin
c> Aug  2015 - added stdarc
c> Aug  2015 - added mxl_no
