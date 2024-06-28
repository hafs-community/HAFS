      program archv2data3dz
      use mod_plot         ! HYCOM plot array interface
      use mod_za           ! HYCOM array I/O interface
      use mod_ppsw, only:  ! WHOI CTD functions
     &      PPSW_theta  => theta,
     &      PPSW_p80    => p80

c
c --- hycom/micom to 3-d z-level diagnostic field extractor
c
      real,    allocatable, dimension (:)     ::
     &   zz,zi, ttk,ssk,rrk
      real,    allocatable, dimension (:,:)   ::
     &   uflux,vflux, depthu,depthv,dpdx,dpdy, util1,work,trk
      real,    allocatable, dimension (:,:,:) ::
     &   utilz,utilk,w
c
      common/conrng/ amn,amx
c
      character flnm*240,frmt*80,cline*240
      character ctrc_title(99)*80,ctrc_units(99)*80,
     &          ctrc_lname(99)*80,ctrc_sname(99)*80
      logical   lcell,ltheta,smooth,lsteric,icegln,lperiod,baclin,
     &          xyward,lpvel,intfwv
c
      integer          artype,iexpt,iversn,kkin,yrflag,mxlflg
      integer          itest,jtest
      real             bot,zbot,dudxdn,dudxup,dvdydn,dvdyup,dbar
      double precision time3(3)
      double precision dsumth,dsumdp
c
      real, parameter :: flag = 2.0**100
c
c --- 'trcout' -- tracer input
      logical   trcout
      data      trcout/.false./
c
      real      tenm,onem,temcm,onecm,onemm
      data      tenm/10./,onem/1./,tencm/.1/,onecm/.01/,onemm/.001/
c
      logical   initl
      data      initl /.true. /
      real      thref,spcifh
      data      thref/1.e-3/,spcifh/3990./
      character blank*40
      data      blank/'                                        '/
c
      call xcspmd
      call zaiost
      lp=6
c
c --- read model data
c ---   'flnm  ' = name of file containing the actual data
c ---   'frmt  ' = output format or type (HYCOM, BINARY, netCDF)
c ---                see horout for more details on frmt
c ---   'iexpt ' = experiment number x10  (000=from archive file)
c ---   'yrflag' = days in year flag (0=360J16,1=366J16,2=366J01,3=actual)
c ---   'ntracr' = number of tracers (to output, optional with default 0)
c ---    one name line per tracer: 8-letter plot and units, field, standard_
c ---      or separated by "|" (i.e. plot|units|field|standard).
c ---      the field name must only contain alphanumerics and "_", and 
c ---      the standard_name is either blank or from the CF 1.0 conventions
c ---   'idm   ' = longitudinal array size
c ---   'jdm   ' = latitudinal  array size
c ---   'kdm   ' = number of layers
        read (*,'(a)') flnm
        write (lp,'(2a)') ' input file: ',trim(flnm)
        call flush(lp)
        read (*,'(a)') frmt
        write (lp,'(2a)') 'output type: ',trim(frmt)
        call flush(lp)
        call blkini(iexpt, 'iexpt ')
        call blkini(yrflag,'yrflag')
        call blkini2(i,j,  'ntracr','idm   ')  !read ntracr or idm
        if (j.eq.1) then
          ntracr = i
          do ktr= 1,ntracr
            read(*,'(a)') cline
            i = index(cline,'|')
            if     (i.eq.0) then  !8-letter plot and units, field has no spaces
              ctrc_title(ktr) = cline(1:8)
              ctrc_units(ktr) = cline(9:16)
              cline = cline(17:)
              do
                i = index(cline,' ')
                if     (i.ne.1) then
                  exit
                endif
                cline = cline(2:) !remove a leading space
              enddo
              ctrc_lname(ktr) = cline(1:i-1)
              ctrc_sname(ktr) = cline(i+1:)
            else  !separated by "|" 
              ctrc_title(ktr) = cline(1:i-1)
              cline = cline(i+1:)
              i = index(cline,'|')
              ctrc_units(ktr) = cline(1:i-1)
              cline = cline(i+1:)
              i = index(cline,'|')
              ctrc_lname(ktr) = cline(1:i-1)
              ctrc_sname(ktr) = cline(i+1:)
            endif
            write (lp,'(2x,i2,3a)')
     &        ktr,' title  = "',trim(ctrc_title(ktr)),'"',
     &        ktr,' units  = "',trim(ctrc_units(ktr)),'"',
     &        ktr,' l.name = "',trim(ctrc_lname(ktr)),'"',
     &        ktr,' s.name = "',trim(ctrc_sname(ktr)),'"'
            call flush(lp)
            if     (   index(ctrc_lname(ktr),' ').ne.0 .and.
     &                 index(ctrc_lname(ktr),' ').le.
     &              len_trim(ctrc_lname(ktr))                ) then
              ! does not catch all illegal l.names.
              write(lp,*)
              write(lp,*) 'error - l.name contains spaces'
              write(lp,*)
              call flush(lp)
              stop
            elseif (   index(ctrc_lname(ktr),'-').ne.0) then
              ! still does not catch all illegal l.names.
              write(lp,*)
              write(lp,*) 'error - l.name contains "-"'
              write(lp,*)
              call flush(lp)
              stop
            endif !l.name check
          enddo
          call blkini(ii,  'idm   ')
        else
          ntracr = 0
          ii     = i
        endif
        call blkini(jj,    'jdm   ')
        call blkini(kk,    'kdm   ')
        if     (ii.ne.idm .or. jj.ne.jdm) then
          write(lp,*)
          write(lp,*) 'error - wrong idm or jdm (should be:',
     &                                           idm,jdm,')'
          write(lp,*)
          call flush(lp)
          stop
        endif
c
c ---   'thbase' = reference density (sigma units)
        call blkinr(thbase,
     &             'thbase','("blkinr: ",a6," =",f11.4," sig")')
c
c ---   'smooth' = smooth the layered fields
        call blkinl(smooth,'smooth')
c
c ---   'intfwv' = calculate layer w-vel at interfaces  (0=no:DEFAULT,1=yes)
c ---   'baclin' = extract baroclinic velocity (0=total:DEFAULT,1=baroclinic)
c ---   'xyward' = output original unrotated velocities (0=no:DEFAULT,1=yes)
        call blkini4(i,j,  'baclin','xyward','iorign','intfwv')  !read 1 of 4 
        if (j.eq.4) then 
          intfwv = i.eq.1  !interface vs layer center w-vel
          call blkini3(i,j,  'baclin','xyward','iorign')  !read 1 of 3 
        else
          intfwv = .false. !layer center w-vel
        endif
        if (j.eq.1) then
          baclin = i.eq.1  !baroclinic, vs total, velocity
          call blkini2(i,j,  'xyward','iorign')  !read one of two
          if (j.eq.1) then
            xyward = i.eq.1  !x-y, vs east-north, velocity
            call blkini(iorign,'iorign')
          else
            xyward = .false. !eastward,northward velocity
            iorign = i
          endif
        elseif (j.eq.2) then
          baclin = .false. !total velocity
          xyward = i.eq.1  !x-y, vs east-north, velocity
          call blkini(iorign,'iorign')
        else
          baclin = .false. !total velocity
          xyward = .false. !eastward,northward velocity
          iorign = i
        endif
c
c ---   'iorign' = i-origin of sampled subregion
c ---   'jorign' = j-origin of sampled subregion
c ---   'idmp  ' = i-extent of sampled subregion (<=idm; 0 implies idm)
c ---   'jdmp  ' = j-extent of sampled subregion (<=jdm; 0 implies jdm)
*       call blkini(iorign,'iorign')  !see above
        call blkini(jorign,'jorign')
        call blkini(ii,    'idmp  ')
        call blkini(jj,    'jdmp  ')
        if     (ii.eq.0) then
          ii=idm
        endif
        if     (jj.eq.0) then
          jj=jdm
        endif
c ---   'iorign,jorign' denote the origin of the subgrid to be extracted 
c ---   from the full history grid (dimensioned idm x jdm). 
c ---   The size of the subgrid is determined by ii,jj.
        write (lp,'(/ 2(a,i5),9x,2(a,i5) /)') 'extracting i =',iorign,
     &    ' ...',iorign+ii-1,'j =',jorign,' ...',jorign+jj-1
        call flush(lp)
c
c --- 'itest ' = longitudinal sampled test point (optional, default 0)
c --- 'jtest ' = latitudinal  sampled test point (optional, default 0)
c --- 'inbot ' = read in bot and/or zbot (1=bot,2=zbot,3=bot&zbot)
c ---              optional, default bot=0.0 and zbot=0.0 (no bottom value)
c --- 'bot   ' = ignore layers within bot of the bottom
c --- 'zbot  ' = depth above bottom for bottom value (none for zbot<=0.0)
c --- 'itype ' = interpolation type (0=sample,1=linear,2=parabolic,3=cubic)
c ---             itype=1 is linear between cell centers    for kz,
c ---                    but linear across each layer (PLM) for kzi.
c ---             itype=2 is always parabolic across each layer (PPM).
c ---             itype=3 is PCHIP  between cell centers    for kz,
c ---                      and is not currently implemented for kzi.
      itest = 0
      jtest = 0
      bot   = 0.0
      zbot  = 0.0
      call blkini3(i,j, 'inbot ','itype ','itest ')  !read itest, inbot or itype
      if     (j.eq.3) then !itest
        itest  = i
        call blkini(jtest, 'jtest ')
        call blkini2(i,j, 'inbot ','itype ')  !read inbot or itype
      endif
      if     (j.eq.1) then !inbot
        if     (i.eq.3) then !bot and zbot
          call blkinr(bot,   
     &               'bot   ','("blkinr: ",a6," =",f11.4," m")')
          call blkinr(zbot,   
     &               'zbot  ','("blkinr: ",a6," =",f11.4," m")')
        elseif (i.eq.2) then !zbot only
          call blkinr(zbot,   
     &               'zbot  ','("blkinr: ",a6," =",f11.4," m")')
        else !bot only (default)
          call blkinr(bot,   
     &               'bot   ','("blkinr: ",a6," =",f11.4," m")')
        endif
        call blkini(itype,'itype ')
      else  !itype
        itype = i
      endif
      if     (itype.lt.0 .or. itype.gt.3) then
        write(lp,*)
        write(lp,*) 'error - unknown itype'
        write(lp,*)
        call flush(lp)
        stop
      endif
c
c --- use exactly one of kzi and kz:
c --- 'kzi   ' = number of depths to sample, input sample cell interfaces
c --- 'kz    ' = number of depths to sample, input sample depths
      call blkini2(i,j,  'kz    ','kzi   ')
      kz  = i
      lcell = j.eq.2
      if (.not.lcell) then
        if     (itype.eq.2) then
          itype = -2  !parabolic across each layer (PPM)
        endif
        allocate( zz(kz) )
        do k= 1,kz
c ---     'z     ' = sample depth (follows kz)
          call blkinr(zz(k),
     &               'z     ','("blkinr: ",a6," =",f11.4," m")')
          if (k .gt. 1) then
            if (zz(k) .le. zz(k-1)) then
              write(lp,*)
              write(lp,*) 'error - current z shallower than last z'
              write(lp,*)
              stop
            endif
          endif
        enddo !k
      else !lcell
        if     (intfwv) then
          write(lp,*)
          write(lp,*) 'error - intfwv=.true. not implemented for zi'
          write(lp,*)
          call flush(lp)
          stop
        endif
        if     (itype.eq.3) then
          write(lp,*)
          write(lp,*) 'error - itype=3 not implemented for zi'
          write(lp,*)
          call flush(lp)
          stop
        endif
        allocate( zz(kz), zi(kz+1) )
        do k= 1,kz+1
c ---     'zi    ' = sample-cell interface (follows kzi)
          call blkinr(zi(k),
     &               'zi    ','("blkinr: ",a6," =",f11.4," m")')
          if     (k.gt.1) then
            if     (zi(k).le.zi(k-1)) then
              write(lp,*)
              write(lp,*) 'error - current zi shallower than last zi'
              write(lp,*)
              stop
            endif
            zz(k-1) = 0.5*(zi(k-1) + zi(k))  !cell center
          endif
        enddo !k
        if     (zi(1).eq.0.0) then
          zz(1) = 0.0  !surface cell is nominally at the surface
        endif
      endif
      write(lp,*)
      call flush(lp)
c
c --- 'botio ' = bathymetry       I/O unit (0 no I/O)
c --- 'athio'  = average density  I/O unit (0 no I/O), OPTIONAL
c
c --- 'mltio ' = mix. lay. thick. I/O unit (0 no I/O), choose 2 of 8 kinds:
c ---            note: MLT input order does not control output order
c --- 'tempml' =  temperature jump across mixed-layer (degC,  0 no I/O) or
c --- 'densml' =  pot.density jump across mixed-layer (kg/m3, 0 no I/O) or
c --- 'tmlnav' = NAVO rp33 T  jump across mixed-layer (degC,  0 no I/O) or
c --- 'dmlnav' = NAVO rp33 TH jump across mixed-layer (kg/m3, 0 no I/O) or
c --- 'tmljmp' = equiv. temp. jump across mixed-layer (degC,  0 no I/O)
c ---             PLM mixed layer                                       or
c --- 'tmljmq' = equiv. temp. jump across mixed-layer (degC,  0 no I/O)
c ---             PQM mixed layer                                       or
c --- 'tmlorb' =  Lorbacher temperature   mixed-layer (0.0,  <0 no I/O) or
c --- 'dmlorb' =  Lorbacher pot.density   mixed-layer (0.0,  <0 no I/O)
c
c --- 'sshio ' = total SSH             I/O unit (0 no I/O), OPTIONAL
c --- 'infio ' = interface depths      I/O unit (0 no I/O, <0 layer # label)
c --- 'wvlio ' = w-velocity            I/O unit (0 no I/O)
c --- 'uvlio ' = u-velocity            I/O unit (0 no I/O)
c --- 'vvlio ' = v-velocity            I/O unit (0 no I/O)
c --- 'splio ' = speed                 I/O unit (0 no I/O)
c --- 'istio ' = in-situ   temperature I/O unit (0 no I/O), OPTIONAL
c --- 'temio ' = potential temperature I/O unit (0 no I/O)
c --- 'salio ' = salinity              I/O unit (0 no I/O)
c --- 'tthio ' = potential density     I/O unit (0 no I/O)
      call blkini(iobotin,'botio ')
      call blkini2(ioin,j,  'athio ','mltio ')  !read one of two
      if (j.eq.1) then
        ioathin = ioin
        call blkini(iomltin,'mltio ')
      else
        ioathin = 0
        iomltin = ioin
      endif !athio:else
      tmljmp =  0.0
      tmljmq =  0.0
      tempml =  0.0
      densml =  0.0
      tmlorb = -1.0
      dmlorb = -1.0
      tmlnav =  0.0
      dmlnav =  0.0
      call blkinr9(qqin,i,
     &             'tmljmp','("blkinr: ",a6," =",f11.4," degC")',
     &             'tmljmq','("blkinr: ",a6," =",f11.4," degC")',
     &             'tempml','("blkinr: ",a6," =",f11.4," degC")',
     &             'densml','("blkinr: ",a6," =",f11.4," kg/m3")',
     &             'tmlorb','("blkinr: ",a6," =",f11.4," ")',
     &             'dmlorb','("blkinr: ",a6," =",f11.4," ")',
     &             'tmlnav','("blkinr: ",a6," =",f11.4," degC")',
     &             'dmlnav','("blkinr: ",a6," =",f11.4," kg/m3")',
     &             'XXXXXX','("blkinr: ",a6," =",f11.4," ")')
      if     (i.eq.1) then
        tmljmp = qqin
      elseif (i.eq.2) then
        tmljmq = qqin
      elseif (i.eq.3) then
        tempml = qqin
      elseif (i.eq.4) then
        densml = qqin
      elseif (i.eq.5) then
        tmlorb = qqin
      elseif (i.eq.6) then
        dmlorb = qqin
      elseif (i.eq.7) then
        tmlnav = qqin
      elseif (i.eq.8) then
        dmlnav = qqin
      endif
      call blkinr9(qqin,i,
     &             'tmljmp','("blkinr: ",a6," =",f11.4," degC")',
     &             'tmljmq','("blkinr: ",a6," =",f11.4," degC")',
     &             'tempml','("blkinr: ",a6," =",f11.4," degC")',
     &             'densml','("blkinr: ",a6," =",f11.4," kg/m3")',
     &             'tmlorb','("blkinr: ",a6," =",f11.4," ")',
     &             'dmlorb','("blkinr: ",a6," =",f11.4," ")',
     &             'tmlnav','("blkinr: ",a6," =",f11.4," degC")',
     &             'dmlnav','("blkinr: ",a6," =",f11.4," kg/m3")',
     &             'XXXXXX','("blkinr: ",a6," =",f11.4," ")')
      if     (i.eq.1) then
        tmljmp = qqin
      elseif (i.eq.2) then
        tmljmq = qqin
      elseif (i.eq.3) then
        tempml = qqin
      elseif (i.eq.4) then
        densml = qqin
      elseif (i.eq.5) then
        tmlorb = qqin
      elseif (i.eq.6) then
        dmlorb = qqin
      elseif (i.eq.7) then
        tmlnav = qqin
      elseif (i.eq.8) then
        dmlnav = qqin
      endif
      call blkini2(i,j,  'infio ','sshio ')  !sshio is optional
      if (j.eq.1) then
        ioinfin = i
        iossh   = 0
      else
        iossh   = i
        call blkini(ioinfin,'infio ')
      endif
      call blkini2(i,j,  'wviio ','wvlio ')  !handle obsolete scripts
      if (j.eq.1) then
        write(lp,*) 'warning - wviio no longer used.'
        call blkini(iowvlin,'wvlio ')
      else
        iowvlin=i
      endif
      call blkini(iouvlin,'uvlio ')
      call blkini(iovvlin,'vvlio ')
      call blkini(iosplin,'splio ')
      call blkini2(ioin,j,  'istio ','temio ')  !read one of two
      if (j.eq.1) then
        ioistin = ioin
        call blkini(iopttin,'temio ')
      else !temio
        ioistin = 0
        iopttin = ioin
      endif !istio:temio
      call blkini(iosalin,'salio ')
      call blkini(iotthin,'tthio ')
c
        call getartype(flnm,artype)
      lpvel  = artype.lt.0
      artype = abs(artype)
      if     (artype.ge.2) then  ! mean or std archive
c ---   'keio  ' = kinetic energy I/O unit (0 no I/O)
        call blkini(iokein, 'keio  ')
      endif
c
c --- array allocation
c
      call plot_alloc
c
      allocate(  uflux(ii,jj) )
      allocate(  vflux(ii,jj) )
      allocate(  util1(ii,jj) )
      allocate(   work(ii,jj) )
c
      if     (iowvlin.ne.0) then
        allocate(  dpdx(ii,jj) )
        allocate(  dpdy(ii,jj) )
        allocate(     w(ii,jj,kk) )
      endif
c
      allocate(  utilk(ii,jj,kk+1) )
      allocate(  utilz(ii,jj,kz) )
c
      dpthfil = 'regional.depth'
c
      do j=1,jj
        do i=1,ii
          p(i,j,1)=0.
        enddo
      enddo
c
c --- read the archive file.
c
        if     (artype.ne.3) then
          call getdat( flnm,time3,artype,initl,lsteric,icegln,trcout,
     &                 iexpt,iversn,yrflag,kkin)     ! hycom input
          artype = abs(artype)
        else
          call getdats(flnm,time3,artype,initl,lsteric,icegln,trcout,
     &                 iexpt,iversn,yrflag,kkin)     ! hycom std. input
        endif
        if (kkin.ne.kk) then
          write(lp,*)
          write(lp,*) 'error - kkin must be kdm'
          write(lp,*)
          stop
        endif
c
      if     (yrflag.eq.0) then
        year  = 360.0d0
      elseif (yrflag.lt.3) then
        year  = 366.0d0
      else
        year  = 365.25d0
      endif
c
c --- define grid scale
      write(lp,'(/a,2f8.2/a,2f8.2)') 
     &     'sub-domain longitude range = ',
     &    minval(plon(:,:)),maxval(plon(:,:)),
     &     'sub-domain latitude  range = ',
     &    minval(plat(:,:)),maxval(plat(:,:))
c
      lperiod = ii.eq.idm .and.
     &          maxval(plon(:,:))-minval(plon(:,:)) .gt. 350.0
      if     (lperiod) then
        write(lp,'(/a/)') 'sub-domain assumed to be periodic'
      else
        write(lp,'(/a/)') 'sub-domain assumed to be non-periodic'
      endif
c
      call bigrid(depths)
      call flush(lp)
c
c --- check that bathymetry is consistent with this archive.
c --- only possible with hycom .[ab] file input.
c
      if     (iversn.ge.20) then
        ibadl = 0
        ibads = 0
        do j= 1,jj
          do i= 1,ii
            if     (ip(i,j).eq.1) then
              if     (srfht(i,j).gt.2.0**99) then
                ibads = ibads + 1   ! topo sea, srfht land
              endif
            else
              if     (srfht(i,j).lt.2.0**99) then
                ibadl = ibadl + 1   ! topo land, srfht sea
               endif
            endif
          enddo !i
        enddo !j
        if     (ibads.ne.0) then
          write(lp,*)
          write(lp,*) 'error - wrong bathymetry for this archive file'
          write(lp,*) 'number of topo sea  mismatches = ',ibads
          write(lp,*) 'number of topo land mismatches = ',ibadl
          write(lp,*)
          call flush(lp)
          stop
        endif !ibads.ne.0
        if     (ibadl.ne.0) then
          write(lp,*)
*         write(lp,*) 'error - wrong bathymetry for this archive file'
          write(lp,*) 'warning - wrong bathymetry for this archive file'
          write(lp,*) 'number of topo sea  mismatches = ',ibads
          write(lp,*) 'number of topo land mismatches = ',ibadl
          write(lp,*)
          call flush(lp)
*         stop
        endif !ibadl.ne.0
      endif !iversn.ge.20
c
      do 3 k=1,kkin
      do 3 j=1,jj
      do 3 i=1,ii
c
c --- convert baroclinic to total velocities by adding barotropic component
c --- note that mean archives already contain total velocity
      if     (.not.lpvel) then
        if     (iu(i,j).eq.1) then
          if     (artype.eq.1 .and. .not.baclin) then
            u(i,j,k)=u(i,j,k)+ubaro(i,j)  !total velocity
          elseif (artype.eq.2 .and.      baclin) then
            u(i,j,k)=u(i,j,k)-ubaro(i,j)  !baroclinic velocity
          endif
        else !iu(i,j).ne.1
          u(i,j,k)=0.
        endif !iu
        if     (iv(i,j).eq.1) then
          if     (artype.eq.1 .and. .not.baclin) then
            v(i,j,k)=v(i,j,k)+vbaro(i,j)  !total velocity
          elseif (artype.eq.2 .and.      baclin) then
            v(i,j,k)=v(i,j,k)-vbaro(i,j)  !baroclinic velocity
          endif
        else !iv(i,j).ne.1
          v(i,j,k)=0.
        endif !iv
      else !lpvel
        if     (ip(i,j).eq.1) then
          if     (artype.eq.1 .and. .not.baclin) then
            u(i,j,k)=u(i,j,k)+ubaro(i,j)  !total velocity
            v(i,j,k)=v(i,j,k)+vbaro(i,j)  !total velocity
          elseif (artype.eq.2 .and.      baclin) then
            u(i,j,k)=u(i,j,k)-ubaro(i,j)  !baroclinic velocity
            v(i,j,k)=v(i,j,k)-vbaro(i,j)  !baroclinic velocity
          endif
        else !ip(i,j).ne.1
          u(i,j,k)=0.
          v(i,j,k)=0.
        endif !ip
      endif !.not.lpvel:else
c
c --- convert layer thickness to meters
      if (depths(i,j).gt.0.) then
        dp(i,j,k)=dp(i,j,k)/9806.
        p(i,j,k+1)=p(i,j,k)+dp(i,j,k)
        th3d(i,j,k)=th3d(i,j,k)+thbase
      else
        saln(i,j,k)=flag
        temp(i,j,k)=flag
        th3d(i,j,k)=flag
        if     (artype.gt.1) then
          ke(i,j,k)=flag  !artype.gt.1
        endif
        dp(i,j,k)=flag
        p(i,j,k+1)=flag
        do ktr= 1,ntracr
          trcr(i,j,k,ktr)=flag
        enddo
      endif
 3    continue
c
c --- make p.kk+1 exactly depth
c
      amn   =  huge(amn)
      amx   = -huge(amx)
      ibads = 0
      do j=1,jj
        do i=1,ii
          if (depths(i,j).gt.0.0) then
            q = depths(i,j)-p(i,j,kk+1)
            amn = min(amn, q)
            amx = max(amx, q)
            if     (abs(q).gt.onecm) then
              ibads = ibads + 1
*             write(lp,*) 'large archive depth mismatch: ',i,j,q
*             if     (i+iorign-1.le.idm) then
*               write(lp,*) 'large archive depth ORIGINAL: ',
*    &                      i+iorign-1,    j+jorign-1,depths(i,j)
*             else
*               write(lp,*) 'large archive depth ORIGINAL: ',
*    &                      i+iorign-1-idm,j+jorign-1,depths(i,j)
*             endif !iorign
            endif !large mismatch
            q = depths(i,j)/p(i,j,kk+1)
            do k=1,kkin+1
              p(i,j,k)=q*p(i,j,k)
              if     (p(i,j,k).gt.depths(i,j)-onemm) then
                p(i,j,k)=depths(i,j)
              endif
            enddo !k
          endif
        enddo !i
      enddo !j
      write(lp,*)
      write(lp,*) 'archive depth mismatch range =',amn,amx
      write(lp,*)
      call flush(lp)
      if     (ibads.ne.0) then
        write(lp,*)
        write(lp,*) 'error - wrong bathymetry for this archive file'
        write(lp,*) 'number of depth mismatches = ',ibads
        write(lp,*)
        call flush(lp)
        stop
      endif !ibads.ne.0
c
c --- eddy kinetic energy
      if     (artype.eq.3) then
        do k=1,kkin
          do j= 1,jj
            jp1 = min(j+1,jj)
            do i= 1,ii
              if (depths(i,j).gt.0.) then
                if     (i.ne.ii) then
                  ip1 = i+1
                elseif (lperiod) then !i=ii
                  ip1 = 1
                else !i=ii (non-periodic)
                  ip1 = ii
                endif
c ---           ke = 0.5*( std(u)**2 + std(v)**2 )
                ke(i,j,k)= 0.125*((u(i,j,k)+u(ip1,j,k))**2 +
     &                            (v(i,j,k)+v(i,jp1,k))**2  )
              endif
            enddo !i
          enddo !j
        enddo !k
      endif !std archive
c
c --- fluid vertical velocity
      if     (iowvlin.ne.0) then
      do j= 1,jj-1
        do i= 1,ii-1
          if     (ip(i,j).eq.1) then
            dudxdn=
     &            (u(i+1,j  ,1)*scuy(i+1,j  )-u(i,j,1)*scuy(i,j))
     &            /(scpx(i,j)*scpy(i,j))
            dvdydn=
     &            (v(i  ,j+1,1)*scvx(i  ,j+1)-v(i,j,1)*scvx(i,j))
     &        /(scpx(i,j)*scpy(i,j))
            if     (intfwv) then
              w(i,j,1)=    dp(i,j,1)*(dudxdn+dvdydn)  !interface
            else
              w(i,j,1)=0.5*dp(i,j,1)*(dudxdn+dvdydn)  !layer center
            endif
          else
            w(i,j,1) = flag
          endif
        enddo
      enddo
      if     (itest.gt.0) then
        i=itest
        j=jtest
        if     (intfwv) then
          write(lp,'(a,i3,f10.4,f11.7)')
     &      'wn: ',0,0.0,0.0
          write(lp,'(a,i3,f10.4,f11.7)')
     &      'wn: ',1,dp(i,j,1),w(i,j,1)
        else
          write(lp,'(a,i3,f10.4,f11.7)')
     &      'wn: ',1,0.5*dp(i,j,1),w(i,j,1)
          write(lp,'(a,i3,f10.4,f11.7)')
     &      'wn: ',k,0.5*(p(i,j,k)+p(i,j,k+1)),w(i,j,k)
        endif
      endif !test
      do k= 2,kkin
        do j= 1,jj-1
          do i= 1,ii-1
            if     (iu(i,j).eq.1) then
              dpdx(i,j)=
     &                (p(i,j,k)*scpy(i,j)-p(i-1,j  ,k)*scpy(i-1,j  ))
     &                /(scux(i,j)*scuy(i,j))
            endif
          enddo
        enddo
        do j= 1,jj-1
          do i= 1,ii-1
            if     (iv(i,j).eq.1) then
              dpdy(i,j)=
     &                (p(i,j,k)*scpx(i,j)-p(i  ,j-1,k)*scpx(i  ,j-1))
     &                /(scvx(i,j)*scvy(i,j))
            endif
          enddo
        enddo
        do j=1,jj
          if     (iu(2   ,j).eq.1) then
            dpdx(1 ,j)=dpdx(2   ,j)
          endif
          if     (iu(ii-1,j).eq.1) then
            dpdx(ii,j)=dpdx(ii-1,j)
          endif
        enddo
        do i=1,ii
          if     (iv(i,2   ).eq.1) then
            dpdy(i,1 )=dpdy(i,2   )
          endif
          if     (iv(i,jj-1).eq.1) then
            dpdy(i,jj)=dpdy(i,jj-1)
          endif
        enddo
        do j= 1,jj-1
          do i= 1,ii-1
            if     (ip(i,j).eq.1) then
              dudxup=
     &              (u(i+1,j  ,k-1)*scuy(i+1,j  )-
     &               u(i  ,j  ,k-1)*scuy(i  ,j  ))
     &              /(scpx(i,j)*scpy(i,j))
              dvdyup=
     &              (v(i  ,j+1,k-1)*scvx(i  ,j+1)-
     &               v(i  ,j  ,k-1)*scvx(i  ,j  ))
     &              /(scpx(i,j)*scpy(i,j))
              dudxdn=
     &              (u(i+1,j  ,k  )*scuy(i+1,j  )-
     &               u(i  ,j  ,k  )*scuy(i  ,j  ))
     &              /(scpx(i,j)*scpy(i,j))
              dvdydn=
     &              (v(i  ,j+1,k  )*scvx(i  ,j+1)-
     &               v(i  ,j  ,k  )*scvx(i  ,j  ))
     &              /(scpx(i,j)*scpy(i,j))
              if     (intfwv) then
                w(i,j,k)=w(i,j,k-1)+0.5*(2.0*dp(i,j,k)*(dudxdn+dvdydn)-
     &                    (u(i  ,j  ,k)-u(i  ,j  ,k-1))*dpdx(i  ,j  )-
     &                    (u(i+1,j  ,k)-u(i+1,j  ,k-1))*dpdx(i+1,j  )-
     &                    (v(i  ,j  ,k)-v(i  ,j  ,k-1))*dpdy(i  ,j  )-
     &                    (v(i  ,j+1,k)-v(i  ,j+1,k-1))*dpdy(i  ,j+1) )
              else
                w(i,j,k)=w(i,j,k-1)+0.5*(dp(i,j,k-1)*(dudxup+dvdyup)+
     &                                   dp(i,j,k  )*(dudxdn+dvdydn)-
     &                    (u(i  ,j  ,k)-u(i  ,j  ,k-1))*dpdx(i  ,j  )-
     &                    (u(i+1,j  ,k)-u(i+1,j  ,k-1))*dpdx(i+1,j  )-
     &                    (v(i  ,j  ,k)-v(i  ,j  ,k-1))*dpdy(i  ,j  )-
     &                    (v(i  ,j+1,k)-v(i  ,j+1,k-1))*dpdy(i  ,j+1) )
              endif
            else
              w(i,j,k) = flag
            endif
          enddo
        enddo
        do i= 1,ii
          w(i ,jj,k) = flag
        enddo
        do j= 1,jj
          w(ii,j ,k) = flag
        enddo
        if     (itest.gt.0) then
          i=itest
          j=jtest
          if     (intfwv) then
            write(lp,'(a,i3,f10.4,f11.7)')
     &        'wn: ',k,p(i,j,k+1),w(i,j,k)
          else
            write(lp,'(a,i3,f10.4,f11.7)')
     &        'wn: ',k,0.5*(p(i,j,k)+p(i,j,k+1)),w(i,j,k)
          endif
        endif !test
      enddo !k
c
      if     (intfwv) then
        if     (smooth) then
          do k= 1,kkin
            call psmoo(w(1,1,k),work)
          enddo
        endif
      else
c ---   w is noisy - smooth at least once.
        if     (smooth) then
          do k= 1,kkin
            call psmoo(w(1,1,k),work)
            call psmoo(w(1,1,k),work)
          enddo
        else
          do k= 1,kkin
            call psmoo(w(1,1,k),work)
          enddo
        endif
      endif !intfwv
      endif !iowvlin
c
      do 7 j=1,jj
      do 7 i=1,ii
      if (depths(i,j).gt.0.) then
        dpmixl(i,j)=dpmixl(i,j)/9806.  ! m
      else
        dpmixl(i,j)=flag
      end if
 7    continue
c
      dpth=0.5*onecm
c
c --- put vertically averaged t,s values into massless layers
c
      allocate( ttk(kkin), ssk(kkin), rrk(kkin) )
      if     (ktr.gt.0) then
        allocate( trk(kkin,ktr) )
      endif
c
      do 70 j=1,jj
      do 70 i=1,ii
c
      if (depths(i,j).gt.0.) then
        do k= 1,kkin
          ttk(k)=0.
          ssk(k)=0.
          rrk(k)=0.
          do ktr= 1,ntracr
            trk(k,ktr)=0.
          enddo
          pmid=.5*(p(i,j,k)+p(i,j,k+1))
          phi=pmid+dpth
          plo=pmid-dpth
c
          sum=0.
          do k1=1,kkin
            delp=max(0.,min(p(i,j,k1+1),phi)-max(p(i,j,k1),plo))
            sum=sum+delp
            ttk(k)=ttk(k)+temp(i,j,k1)*delp
            ssk(k)=ssk(k)+saln(i,j,k1)*delp
            rrk(k)=rrk(k)+th3d(i,j,k1)*delp
            do ktr= 1,ntracr
              trk(k,ktr)=trk(k,ktr)+trcr(i,j,k1, ktr)*delp
            enddo !ktr
          enddo !k1
c
          ttk(k)=ttk(k)/sum
          ssk(k)=ssk(k)/sum
          rrk(k)=rrk(k)/sum
          do ktr= 1,ntracr
            trk(k,ktr)=trk(k,ktr)/sum
          enddo !ktr
        enddo !k
        do k= 1,kkin
          temp(i,j,k)=ttk(k)
          saln(i,j,k)=ssk(k)
          th3d(i,j,k)=rrk(k)
          do ktr= 1,ntracr
            trcr(i,j,k,ktr)=trk(k,ktr)
          enddo !ktr
        enddo !k
      end if !ip
 70   continue
c
      if (smooth) then
c
c --- smooth mass field variables
c
      call psmoo(temp(1,1,1),work)
      call psmoo(saln(1,1,1),work)
      call psmoo(th3d(1,1,1),work)
      do ktr= 1,ntracr
        call psmoo(trcr(1,1,1,ktr),work)
      enddo
c
      do 38 k=2,kkin
c
      do 76 j=1,jj
      do 76 i=1,ii
      if (depths(i,j).gt.0.) then
        util1(i,j)=max(onemm,dp(i,j,k))
        temp(i,j,k)=temp(i,j,k)*util1(i,j)
        saln(i,j,k)=saln(i,j,k)*util1(i,j)
        th3d(i,j,k)=th3d(i,j,k)*util1(i,j)
        do ktr= 1,ntracr
          trcr(i,j,k,ktr)=trcr(i,j,k,ktr)*util1(i,j)
        enddo
      else
        temp(i,j,k)=flag
        saln(i,j,k)=flag
        th3d(i,j,k)=flag
        do ktr= 1,ntracr
          trcr(i,j,k,ktr)=flag
        enddo
      end if
 76   continue
c
      call psmoo(util1,work)
      call psmoo(temp(1,1,k),work)
      call psmoo(saln(1,1,k),work)
      call psmoo(th3d(1,1,k),work)
      do ktr= 1,ntracr
        call psmoo(trcr(1,1,k,ktr),work)
      enddo
c
      do 38 j=1,jj
      do 38 i=1,ii
      if (depths(i,j).gt.0.) then
        temp(i,j,k)=temp(i,j,k)/util1(i,j)
        saln(i,j,k)=saln(i,j,k)/util1(i,j)
        th3d(i,j,k)=th3d(i,j,k)/util1(i,j)
        do ktr= 1,ntracr
          trcr(i,j,k,ktr)=trcr(i,j,k,ktr)/util1(i,j)
        enddo
      end if
 38   continue
c
c --- smooth velocity and layer thickness fields
c
      do 30 k=1,kkin
c
      if     (.not.lpvel) then
      do 31 j=1,jj1
      do 31 i=2,ii1
 31   uflux(i,j)=u(i,j,k)*max(onecm,dp(i,j,k)+dp(i-1,j,k))
c
      do 32 j=2,jj1
      do 32 i=1,ii1
 32   vflux(i,j)=v(i,j,k)*max(onecm,dp(i,j,k)+dp(i,j-1,k))
c
      call usmoo(uflux,work)
      call vsmoo(vflux,work)
      call psmoo(dp(1,1,k),work)
c --- (warning: smoothed -dp- field unsuitable for deriving interface depths)
c
      do 33 j=1,jj1
      do 33 i=2,ii1
 33   u(i,j,k)=uflux(i,j)/max(onecm,dp(i,j,k)+dp(i-1,j,k))
c
      do 34 j=2,jj1
      do 34 i=1,ii1
 34   v(i,j,k)=vflux(i,j)/max(onecm,dp(i,j,k)+dp(i,j-1,k))
      else !lpvel
        do j=1,jj
          do i=1,ii
            uflux(i,j)=u(i,j,k)*max(onecm,dp(i,j,k))
            vflux(i,j)=v(i,j,k)*max(onecm,dp(i,j,k))
          enddo !i
        enddo !j
c
        call psmoo(uflux,work)
        call psmoo(vflux,work)
        call psmoo(dp(1,1,k),work)
c ---   (warning: smoothed -dp- field unsuitable for deriving interface depths)
        do j=1,jj
          do i=1,ii
            u(i,j,k)=uflux(i,j)/max(onecm,dp(i,j,k))
            v(i,j,k)=vflux(i,j)/max(onecm,dp(i,j,k))
          enddo !i
        enddo !j
      endif !.not.lpvel:else
c
c --- now smooth layer interfaces and find corresponding -dp- field
      if (k.lt.kkin) call psmo1(p(1,1,k+1),work,p(1,1,kk+1))
c --- now smooth mixed layer base
      if (k.eq.1) then
        call psmo1(dpmixl,work,p(1,1,kk+1))
      end if
c
      do 35 j=1,jj1
      do 35 i=1,ii1
      if (depths(i,j).gt.0.) dp(i,j,k)=p(i,j,k+1)-p(i,j,k)
 35   continue
c
 30   continue
c
      end if  !smooth = .true.
c
c --- put vertically averaged u,v values into massless layers
c
      if     (.not.lpvel) then
      do 72 j=1,jj
      do 72 i=2,ii
      if (min(depths(i,j),depths(i-1,j)).gt.0.) then
        do k=2,kkin
          ttk(k)=0.
          pmid=.25*(p(i,j,k)+p(i-1,j,k)+p(i,j,k+1)+p(i-1,j,k+1))
          plo=pmid-dpth
          phi=pmid+dpth
c
          sum=0.
          do k1=1,kkin
            delp=max(0.,min(.5*(p(i,j,k1+1)+p(i-1,j,k1+1)),phi)
     &                 -max(.5*(p(i,j,k1  )+p(i-1,j,k1  )),plo))
            sum=sum+delp
            ttk(k)=ttk(k)+u(i,j,k1)*delp
          enddo !k1
c
          ttk(k)=ttk(k)/sum
        enddo !k
        do k= 2,kkin
          u(i,j,k)=ttk(k)
        enddo !k
      endif !iu
 72   continue
c
      do 74 j=2,jj
      do 74 i=1,ii
      if (min(depths(i,j),depths(i,j-1)).gt.0.) then
        do k=2,kkin
          ttk(k)=0.
          pmid=.25*(p(i,j,k)+p(i,j-1,k)+p(i,j,k+1)+p(i,j-1,k+1))
          plo=pmid-dpth
          phi=pmid+dpth
c
          sum=0.
          do k1=1,kkin
            delp=max(0.,min(.5*(p(i,j,k1+1)+p(i,j-1,k1+1)),phi)
     &                 -max(.5*(p(i,j,k1  )+p(i,j-1,k1  )),plo))
            sum=sum+delp
            ttk(k)=ttk(k)+v(i,j,k1)*delp
          enddo !k1
c
          ttk(k)=ttk(k)/sum
        enddo !k
        do k= 2,kkin
          v(i,j,k)=ttk(k)
        enddo !k
      endif !iv
 74   continue
      else !lpvel
        do j=1,jj
          do i=1,ii
            if     (ip(i,j).eq.1) then
              do k=2,kkin
                ttk(k)=0.
                pmid=.5*(p(i,j,k)+p(i,j,k+1))
                plo=pmid-dpth
                phi=pmid+dpth
c
                sum=0.
                do k1=1,kkin
                  delp=max(0.,min(p(i,j,k1+1),phi)
     &                       -max(p(i,j,k1  ),plo))
                  sum=sum+delp
                  ttk(k)=ttk(k)+u(i,j,k1)*delp
                enddo !k1
c
                ttk(k)=ttk(k)/sum
              enddo !k
              do k= 2,kkin
                u(i,j,k)=ttk(k)
              enddo !k
c
              do k=2,kkin
                ttk(k)=0.
                pmid=.5*(p(i,j,k)+p(i,j,k+1))
                plo=pmid-dpth
                phi=pmid+dpth
c
                sum=0.
                do k1=1,kkin
                  delp=max(0.,min(p(i,j,k1+1),phi)
     &                       -max(p(i,j,k1  ),plo))
                  sum=sum+delp
                  ttk(k)=ttk(k)+v(i,j,k1)*delp
                enddo !k1
c
                ttk(k)=ttk(k)/sum
              enddo !k
              do k= 2,kkin
                v(i,j,k)=ttk(k)
              enddo !k
            endif !ip
          enddo !i
        enddo !j
      endif !.not.lpvel:else
c
c --- ---------------------------------------------------------------------
c --- 3-Z fields first, to allow 2-D and 3-D fields to be added to the file
c --- ---------------------------------------------------------------------
c
c --- -------------------
c --- u-velocity
c --- -------------------
c
c --- 'uvlio ' = u-velocity I/O unit (0 no I/O)
      ioin=iouvlin
      if (ioin.gt.0) then
        do k= 1,kk
          do j=1,jj
            jm1 = max(j-1, 1)
            jp1 = min(j+1,jj)
            do i=1,ii
              if (ip(i,j).ne.0) then  !may be approximate at i=1 and i=ii
                if     (dp(i,j,k).gt.0.1 .and.
     &                  p(i,j,k).lt.p(i,j,kk+1)-bot) then
                  if     (.not.lpvel) then
c ---             flux form for better results from mean archives
                  if     (i.ne.ii) then
                    ip1 = i+1
                  elseif (lperiod) then !i=ii
                    ip1 = 1
                  else !i=ii (non-periodic)
                    ip1 = ii
                  endif
                  if     (i.ne.1) then
                    im1 = i-1
                  elseif (lperiod) then !i=1
                    im1 = ii
                  else !i=1 (non-periodic)
                    im1 = 1
                  endif
                  if (ip(im1,j).ne.0) then
                    depthu0 = min(p(i,j,kk+1), p(im1,j,kk+1))
                    dpu0    = max(0.0,
     &                min(depthu0,0.5*(p(i,j,k+1)+p(im1,j,k+1)))-
     &                min(depthu0,0.5*(p(i,j,k  )+p(im1,j,k  ))))
                  else
                    dpu0    = dp(i,j,k)
                  endif
                  if (ip(ip1,j).ne.0) then
                    depthu1 = min(p(i,j,kk+1), p(ip1,j,kk+1))
                    dpu1    = max(0.0,
     &                min(depthu1,0.5*(p(i,j,k+1)+p(ip1,j,k+1)))-
     &                min(depthu1,0.5*(p(i,j,k  )+p(ip1,j,k  ))))
                  else
                    dpu1    = dp(i,j,k)
                  endif
                  uvp=(dpu0*u(i,  j,k)+
     &                 dpu1*u(ip1,j,k) )/
     &                        max(2.0*dp(i,j,k),dpu0+dpu1)
                  else !lpvel
                  uvp=u(i,j,k)
                  endif !.not.lpvel:else
                  if     (xyward .or. pang(i,j).eq.0.0) then
                    utilk(i,j,k)=uvp
                  else
                    if     (.not.lpvel) then
                    if (ip(i,jm1).ne.0) then
                      depthv0 = min(p(i,j,kk+1), p(i,jm1,kk+1))
                      dpv0    = max(0.0,
     &                  min(depthv0,0.5*(p(i,j,k+1)+p(i,jm1,k+1)))-
     &                  min(depthv0,0.5*(p(i,j,k  )+p(i,jm1,k  ))))
                    else
                      dpv0    = dp(i,j,k)
                    endif
                    if (ip(i,jp1).ne.0) then
                      depthv1 = min(p(i,j,kk+1), p(i,jp1,kk+1))
                      dpv1    = max(0.0,
     &                  min(depthv1,0.5*(p(i,j,k+1)+p(i,jp1,k+1)))-
     &                  min(depthv1,0.5*(p(i,j,k  )+p(i,jp1,k  ))))
                    else
                      dpv1    = dp(i,j,k)
                    endif
                    vvp=(dpv0*v(i,j,  k)+
     &                   dpv1*v(i,jp1,k) )/
     &                           max(2.0*dp(i,j,k),dpv0+dpv1)
                    else !lpvel
                    vvp=v(i,j,k)
                    endif !.not.lpvel:else
                    utilk(i,j,k)=cos( pang(i,j))*uvp +
     &                           sin(-pang(i,j))*vvp
                  endif !pang
                else
                  utilk(i,j,k)=utilk(i,j,k-1)
                endif
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
        enddo
        if     (.not.lcell) then !point interpolation
          call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
        else !cell average
          call layer2c(utilk,p,utilz,zi,flag,ii,jj,kk,kz,itype)
        endif
        if     (baclin) then
        if     (xyward) then
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &              ' x-b.vel.',                   ! plot name
     &              'u_bcl',                       ! ncdf name
     &   'baroclinic_sea_water_x_velocity',        ! ncdf standard_name
     &              'm/s',                         ! units
     &              kz, frmt,ioin)
        else
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &              ' u-b.vel.',                   ! plot name
     &              'u_bcl',                       ! ncdf name
     &   'baroclinic_eastward_sea_water_velocity', ! ncdf standard_name
     &              'm/s',                         ! units
     &              kz, frmt,ioin)
        endif !xyward:else
        else !total velocity
        if     (xyward) then
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &              ' x-veloc.',                   ! plot name
     &              'u',                           ! ncdf name (mersea)
     &              'sea_water_x_velocity',        ! ncdf standard_name
     &              'm/s',                         ! units
     &              kz, frmt,ioin)
        else
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &              ' u-veloc.',                   ! plot name
     &              'u',                           ! ncdf name (mersea)
     &              'eastward_sea_water_velocity', ! ncdf standard_name
     &              'm/s',                         ! units
     &              kz, frmt,ioin)
        endif !xyward:else
        endif !baclin:total
        if     (zbot.gt.0.0) then
          if     (.not.lcell) then !point interpolation
            call layer2z_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          else !cell average
            call layer2c_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          endif
          if     (baclin) then
            if     (.not.xyward) then
            call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &              ' u-bv.bot',                   ! plot name
     &              'bot_u_bcl',                   ! ncdf name
     &   'baroclinic_eastward_sea_water_velocity_at_bottom', ! ncdf standard_name
     &              'm/s',                         ! units
     &              0,.false., frmt,ioin)
            endif !.not.xyward
          else !total velocity
            if     (xyward) then
            call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &              ' x-velbot',                   ! plot name
     &              'bot_u',                       ! ncdf name
     &              'sea_water_x_velocity_at_bottom',        ! ncdf standard_name
     &              'm/s',                         ! units
     &              0,.false., frmt,ioin)
            else
            call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &              ' u-velbot',                   ! plot name
     &              'bot_u',                       ! ncdf name
     &              'eastward_sea_water_velocity_at_bottom', ! ncdf standard_name
     &              'm/s',                         ! units
     &              0,.false., frmt,ioin)
            endif !xyward:else
          endif !baclin:total
        endif !zbot
      endif !u-velocity
c
c --- -------------------
c --- v-velocity
c --- -------------------
c
c --- 'vvlio ' = v-velocity I/O unit (0 no I/O)
      ioin=iovvlin
      if (ioin.gt.0) then
        do k= 1,kk
          do j=1,jj
            jm1 = max(j-1, 1)
            jp1 = min(j+1,jj)
            do i=1,ii
              if (ip(i,j).ne.0) then  !may be approximate at j=1 and j=jj
                if     (dp(i,j,k).gt.0.1 .and.
     &                  p(i,j,k).lt.p(i,j,kk+1)-bot) then
                  if     (.not.lpvel) then
c ---             flux form for better results from mean archives
                  if (ip(i,jm1).ne.0) then
                    depthv0 = min(p(i,j,kk+1), p(i,jm1,kk+1))
                    dpv0    = max(0.0,
     &                min(depthv0,0.5*(p(i,j,k+1)+p(i,jm1,k+1)))-
     &                min(depthv0,0.5*(p(i,j,k  )+p(i,jm1,k  ))))
                  else
                    dpv0    = dp(i,j,k)
                  endif
                  if (ip(i,jp1).ne.0) then
                    depthv1 = min(p(i,j,kk+1), p(i,jp1,kk+1))
                    dpv1    = max(0.0,
     &                min(depthv1,0.5*(p(i,j,k+1)+p(i,jp1,k+1)))-
     &                min(depthv1,0.5*(p(i,j,k  )+p(i,jp1,k  ))))
                  else
                    dpv1    = dp(i,j,k)
                  endif
                  vvp=(dpv0*v(i,j,  k)+
     &                 dpv1*v(i,jp1,k) )/
     &                         max(2.0*dp(i,j,k),dpv0+dpv1)
                  else !lpvel
                  vvp=v(i,j,k)
                  endif !.not.lpvel:else
                  if     (xyward .or. pang(i,j).eq.0.0) then
                    utilk(i,j,k)=vvp
                  else
                    if     (.not.lpvel) then
                    if     (i.ne.ii) then
                      ip1 = i+1
                    elseif (lperiod) then !i=ii
                      ip1 = 1
                    else !i=ii (non-periodic)
                      ip1 = ii
                    endif
                    if     (i.ne.1) then
                      im1 = i-1
                    elseif (lperiod) then !i=1
                      im1 = ii
                    else !i=1 (non-periodic)
                      im1 = 1
                    endif
                    if (ip(im1,j).ne.0) then
                      depthu0 = min(p(i,j,kk+1), p(im1,j,kk+1))
                      dpu0    = max(0.0,
     &                  min(depthu0,0.5*(p(i,j,k+1)+p(im1,j,k+1)))-
     &                  min(depthu0,0.5*(p(i,j,k  )+p(im1,j,k  ))))
                    else
                      dpu0    = dp(i,j,k)
                    endif
                    if (ip(ip1,j).ne.0) then
                      depthu1 = min(p(i,j,kk+1), p(ip1,j,kk+1))
                      dpu1    = max(0.0,
     &                  min(depthu1,0.5*(p(i,j,k+1)+p(ip1,j,k+1)))-
     &                  min(depthu1,0.5*(p(i,j,k  )+p(ip1,j,k  ))))
                    else
                      dpu1    = dp(i,j,k)
                    endif
                    uvp=(dpu0*u(i,  j,k)+
     &                   dpu1*u(ip1,j,k) )/
     &                           max(2.0*dp(i,j,k),dpu0+dpu1)
                    else !lpvel
                    uvp=u(i,j,k)
                    endif !.not.lpvel:else
                    utilk(i,j,k)=cos( pang(i,j))*vvp -
     &                           sin(-pang(i,j))*uvp
                  endif !pang
                else
                  utilk(i,j,k)=utilk(i,j,k-1)
                endif
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
        enddo
        if     (.not.lcell) then !point interpolation
          call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
        else !cell average
          call layer2c(utilk,p,utilz,zi,flag,ii,jj,kk,kz,itype)
        endif
        if     (baclin) then
        if     (xyward) then
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &              ' y-b.vel.',                    ! plot name
     &              'v_bcl',                        ! ncdf name
     &   'baroclinic_sea_water_y_velocity',         ! ncdf standard_name
     &              'm/s',                          ! units
     &              kz, frmt,ioin)
        else
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &              ' v-b.vel.',                    ! plot name
     &              'v_bcl',                        ! ncdf name
     &   'baroclinic_northward_sea_water_velocity', ! ncdf standard_name
     &              'm/s',                          ! units
     &              kz, frmt,ioin)
        endif !xyward:else
        else !total velocity
        if     (xyward) then
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &              ' y-veloc.',                    ! plot name
     &              'v',                            ! ncdf name (mersea)
     &              'sea_water_y_velocity',         ! ncdf standard_name
     &              'm/s',                          ! units
     &              kz, frmt,ioin)
        else
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &              ' v-veloc.',                    ! plot name
     &              'v',                            ! ncdf name (mersea)
     &              'northward_sea_water_velocity', ! ncdf standard_name
     &              'm/s',                          ! units
     &              kz, frmt,ioin)
        endif !xyward:else
        endif !baclin:total
        if     (zbot.gt.0.0) then
          if     (.not.lcell) then !point interpolation
            call layer2z_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          else !cell average
            call layer2c_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          endif
          if     (baclin) then
            if     (.not.xyward) then
            call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &              ' v-bv.bot',                    ! plot name
     &              'bot_v_bcl',                    ! ncdf name
     &   'baroclinic_northward_sea_water_velocity_at_bottom', ! ncdf standard_name
     &              'm/s',                          ! units
     &              0,.false., frmt,ioin)
            endif !.not.xyward
          else !total velocity
            if     (xyward) then
            call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &              ' y-velbot',                    ! plot name
     &              'bot_v',                        ! ncdf name
     &              'sea_water_y_velocity_at_bottom',         ! ncdf standard_name
     &              'm/s',                          ! units
     &              0,.false., frmt,ioin)
            else
            call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &              ' v-velbot',                    ! plot name
     &              'bot_v',                        ! ncdf name
     &              'northward_sea_water_velocity_at_bottom', ! ncdf standard_name
     &              'm/s',                          ! units
     &              0,.false., frmt,ioin)
            endif !xyward:else
          endif !baclin:total
        endif !zbot
      endif !v-velocity
c
c --- -------------------
c --- speed
c --- -------------------
c
c --- 'splio ' = speed I/O unit (0 no I/O)
      ioin=iosplin
      if (ioin.gt.0) then
        do k= 1,kk
          do j=1,jj
            jm1 = max(j-1, 1)
            jp1 = min(j+1,jj)
            do i=1,ii
              if (ip(i,j).ne.0 .and. i.lt.ii .and. j.lt.jj) then
                if     (dp(i,j,k).gt.0.1 .and.
     &                  p(i,j,k).lt.p(i,j,kk+1)-bot) then
                  if     (.not.lpvel) then
c ---             flux form for better results from mean archives
                  if     (i.ne.ii) then
                    ip1 = i+1
                  elseif (lperiod) then !i=ii
                    ip1 = 1
                  else !i=ii (non-periodic)
                    ip1 = ii
                  endif
                  if     (i.ne.1) then
                    im1 = i-1
                  elseif (lperiod) then !i=1
                    im1 = ii
                  else !i=1 (non-periodic)
                    im1 = 1
                  endif
                  if (ip(im1,j).ne.0) then
                    depthu0 = min(p(i,j,kk+1), p(im1,j,kk+1))
                    dpu0    = max(0.0,
     &                min(depthu0,0.5*(p(i,j,k+1)+p(im1,j,k+1)))-
     &                min(depthu0,0.5*(p(i,j,k  )+p(im1,j,k  ))))
                  else
                    dpu0    = dp(i,j,k)
                  endif
                  if (ip(ip1,j).ne.0) then
                    depthu1 = min(p(i,j,kk+1), p(ip1,j,kk+1))
                    dpu1    = max(0.0,
     &                min(depthu1,0.5*(p(i,j,k+1)+p(ip1,j,k+1)))-
     &                min(depthu1,0.5*(p(i,j,k  )+p(ip1,j,k  ))))
                  else
                    dpu1    = dp(i,j,k)
                  endif
                  if (ip(i,jm1).ne.0) then
                    depthv0 = min(p(i,j,kk+1), p(i,jm1,kk+1))
                    dpv0    = max(0.0,
     &                min(depthv0,0.5*(p(i,j,k+1)+p(i,jm1,k+1)))-
     &                min(depthv0,0.5*(p(i,j,k  )+p(i,jm1,k  ))))
                  else
                    dpv0    = dp(i,j,k)
                  endif
                  if (ip(i,jp1).ne.0) then
                    depthv1 = min(p(i,j,kk+1), p(i,jp1,kk+1))
                    dpv1    = max(0.0,
     &                min(depthv1,0.5*(p(i,j,k+1)+p(i,jp1,k+1)))-
     &                min(depthv1,0.5*(p(i,j,k  )+p(i,jp1,k  ))))
                  else
                    dpv1    = dp(i,j,k)
                  endif
                  uvp         =(dpu0*u(i,  j,k)+
     &                          dpu1*u(ip1,j,k) )/
     &                         max(2.0*dp(i,j,k),dpu0+dpu1)
                  vvp         =(dpv0*v(i,j,  k)+
     &                          dpv1*v(i,jp1,k) )/
     &                         max(2.0*dp(i,j,k),dpv0+dpv1)
                  else !lpvel
                  uvp=u(i,j,k)
                  vvp=v(i,j,k)
                  endif !.not.lpvel:else
                  utilk(i,j,k)=sqrt( uvp**2 + vvp**2 )
                else
                  utilk(i,j,k)=utilk(i,j,k-1)
                endif
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
        enddo
        if     (.not.lcell) then !point interpolation
          call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
        else !cell average
          call layer2c(utilk,p,utilz,zi,flag,ii,jj,kk,kz,itype)
        endif
        if     (baclin) then
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &              ' bcl.spd.',                ! plot name
     &              'speed_bcl',                ! ncdf name
     &   'baroclinic_sea_water_speed',          ! ncdf standard_name
     &              'm/s',                      ! units
     &              kz, frmt,ioin)
        else !total velocity
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &              ' speed   ',                ! plot name
     &              'speed',                    ! ncdf name
     &              'sea_water_speed',          ! ncdf standard_name
     &              'm/s',                      ! units
     &              kz, frmt,ioin)
        endif !baclin:total
        if     (zbot.gt.0.0) then
          if     (.not.lcell) then !point interpolation
            call layer2z_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          else !cell average
            call layer2c_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          endif
          if     (baclin) then
            call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &              ' b.spdbot',                ! plot name
     &              'bot_speed_bcl',            ! ncdf name
     &              ' ',                        ! ncdf standard_name
     &              'm/s',                      ! units
     &              0,.false., frmt,ioin)
          else !total velocity
            call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &              ' speedbot',                ! plot name
     &              'bot_speed',                ! ncdf name
     &              ' ',                        ! ncdf standard_name
     &              'm/s',                      ! units
     &              0,.false., frmt,ioin)
          endif !baclin:total
        endif !zbot
      endif !speed
c
c --- -------------------
c --- w-velocity
c --- -------------------
c
c --- 'wvlio ' = w-velocity I/O unit (0 no I/O)
      ioin=iowvlin
      if (ioin.gt.0) then
        do k= 1,kk
          do j=1,jj
            do i=1,ii
              utilk(i,j,k)=w(i,j,k)
            enddo
          enddo
          if     (itest.gt.0 .and. .not.intfwv) then
            i=itest
            j=jtest
            write(lp,'(a,i3,f10.4,f11.7)')
     &        'wl: ',k,0.5*(p(i,j,k)+p(i,j,k+1)),w(i,j,k)
          endif !test
        enddo
        if     (intfwv) then !linear between interfaces
          if     (itest.gt.0) then
          call infac2z_debug(utilk,p,utilz,zz,flag,ii,jj,kk,kz,
     &                       0,1, itest,jtest,lp)
          else
          call infac2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,0,1)
          endif !test
        elseif (.not.lcell) then !point interpolation
          call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
        else !cell average
          call layer2c(utilk,p,utilz,zi,flag,ii,jj,kk,kz,itype)
        endif
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &              ' w-veloc.',                    ! plot name
     &              'w_velocity',                   ! ncdf name
     &              'upward_sea_water_velocity',    ! ncdf standard_name
     &              'm/s',                          ! units
     &              kz, frmt,ioin)
        if     (itest.gt.0) then
          i=itest
          j=jtest
          do k= 1,kz
            if     (utilz(i,j,k).ne.flag) then
              write(lp,'(a,i3,f10.4,f11.7)')
     &          'wz: ',k,zz(k),utilz(i,j,k)
            endif
          enddo !k
        endif !test
      endif
c
c --- ----------------
c --- temperature
c --- ----------------
c
c --- 'istio ' = in-situ temperature I/O unit (0 no I/O)
      ioin=ioistin
      if (ioin.gt.0) then
        do k= 1,kk
          do j=1,jj
            do i=1,ii
              if (ip(i,j).ne.0) then
                if     (p(i,j,k).lt.p(i,j,kk+1)-bot) then
                  dbar = 
     &              PPSW_p80(0.5*(p(i,j,k)+p(i,j,k+1)), plat(i,j))
                  utilk(i,j,k)=
     &              PPSW_theta(saln(i,j,k),temp(i,j,k), 0.0,dbar)
                else
                  utilk(i,j,k)=utilk(i,j,k-1)
                endif
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
        enddo
        if     (.not.lcell) then !point interpolation
          call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
        else !cell average
          call layer2c(utilk,p,utilz,zi,flag,ii,jj,kk,kz,itype)
        endif
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &              '  temp   ',                       ! plot name
     &              'water_temp',                      ! ncdf name
     &              'sea_water_temperature',           ! ncdf standard_name
     &              'degC',                            ! units
     &              kz, frmt,ioin)
        if     (zbot.gt.0.0) then
          if     (.not.lcell) then !point interpolation
            call layer2z_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          else !cell average
            call layer2c_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          endif
          call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &            ' temp.bot',                ! plot name
     &            'bot_temperature',          ! ncdf name
     &            'sea_water_temperature_at_bottom',  ! ncdf standard_name
     &            'degC',                     ! units
     &            0,.false., frmt,ioin)
        endif !zbot
      endif !temperature
c
c --- ---------------------
c --- potential temperature
c --- ---------------------
c
c --- 'temio ' = potential temperature I/O unit (0 no I/O)
      ioin=iopttin
      if (ioin.gt.0) then
        do k= 1,kk
          do j=1,jj
            do i=1,ii
              if (ip(i,j).ne.0) then
                if     (p(i,j,k).lt.p(i,j,kk+1)-bot) then
                  utilk(i,j,k)=temp(i,j,k)
                else
                  utilk(i,j,k)=utilk(i,j,k-1)
                endif
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
        enddo
        if     (.not.lcell) then !point interpolation
          call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
        else !cell average
          call layer2c(utilk,p,utilz,zi,flag,ii,jj,kk,kz,itype)
        endif
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &              '  potT   ',                       ! plot name
     &              'pot_temp',                        ! ncdf name
     &              'sea_water_potential_temperature', ! ncdf standard_name
     &              'degC',                            ! units
     &              kz, frmt,ioin)
        if     (zbot.gt.0.0) then
          if     (.not.lcell) then !point interpolation
            call layer2z_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          else !cell average
            call layer2c_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          endif
          call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &            ' potT.bot',                       ! plot name
     &            'bot_pot_temp',                    ! ncdf name
     &            'sea_water_potential_temperature_at_bottom', ! ncdf standard_name
     &            'degC',                            ! units
     &            0,.false., frmt,ioin)
        endif !zbot
      endif !temperature
c
c --- -------------
c --- salinity
c --- -------------
c
c --- 'salio ' = salinity I/O unit (0 no I/O)
      ioin=iosalin
      if (ioin.gt.0) then
        do k= 1,kk
          do j=1,jj
            do i=1,ii
              if (ip(i,j).ne.0) then
                if     (p(i,j,k).lt.p(i,j,kk+1)-bot) then
                  utilk(i,j,k)=saln(i,j,k)
                else
                  utilk(i,j,k)=utilk(i,j,k-1)
                endif
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
        enddo
        if     (.not.lcell) then !point interpolation
          call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
        else !cell average
          call layer2c(utilk,p,utilz,zi,flag,ii,jj,kk,kz,itype)
        endif
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &              ' salinity',                ! plot name
     &              'salinity',                 ! ncdf name
     &              'sea_water_salinity',       ! ncdf standard_name
     &              'psu',                      ! units
     &              kz, frmt,ioin)
        if     (zbot.gt.0.0) then
          if     (.not.lcell) then !point interpolation
            call layer2z_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          else !cell average
            call layer2c_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          endif
          call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &            ' saln.bot',                ! plot name
     &            'bot_salinity',             ! ncdf name
     &            'sea_water_salinity_at_bottom',       ! ncdf standard_name
     &            'psu',                      ! units
     &            0,.false., frmt,ioin)
        endif !zbot
      endif
c
c --- -----------------
c --- potential density
c --- -----------------
c
c --- 'tthio ' = potential density I/O unit (0 no I/O)
      ioin=iotthin
      if (ioin.gt.0) then
        do k= 1,kk
          do j=1,jj
            do i=1,ii
              if (ip(i,j).ne.0) then
                if     (p(i,j,k).lt.p(i,j,kk+1)-bot) then
                  utilk(i,j,k)=th3d(i,j,k)
                else
                  utilk(i,j,k)=utilk(i,j,k-1)
                endif
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
        enddo
        if     (.not.lcell) then !point interpolation
          call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
        else !cell average
          call layer2c(utilk,p,utilz,zi,flag,ii,jj,kk,kz,itype)
        endif
        call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &              ' p.dens  ',                   ! plot name
     &              'pot_density',                 ! ncdf name
     &              'sea_water_potential_density', ! ncdf standard_name
     &              'sigma',                       ! units
     &              kz, frmt,ioin)
        if     (zbot.gt.0.0) then
          if     (.not.lcell) then !point interpolation
            call layer2z_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          else !cell average
            call layer2c_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          endif
          call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &            ' dens.bot',                   ! plot name
     &            'bot_pot_density',             ! ncdf name
     &            'sea_water_potential_density_at_bottom', ! ncdf standard_name
     &            'sigma',                       ! units
     &            0,.false., frmt,ioin)
        endif !zbot
      endif
c
c ---   -------------
c ---   tracers
c ---   -------------
c
        do ktr= 1,ntracr
c ---   'trcio ' = tracer I/O unit (0 no I/O)
        call blkini(ioin,'trcio ')
        if (ioin.gt.0) then
          do k= 1,kk
            do j=1,jj
              do i=1,ii
                if (ip(i,j).ne.0) then
                  if     (p(i,j,k).lt.p(i,j,kk+1)-bot) then
                    utilk(i,j,k)=trcr(i,j,k,ktr)
                  else
                    utilk(i,j,k)=utilk(i,j,k-1)
                  endif
                else
                  utilk(i,j,k)=flag
                endif
              enddo
            enddo
          enddo
          if     (.not.lcell) then !point interpolation
            call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
          else !cell average
            call layer2c(utilk,p,utilz,zi,flag,ii,jj,kk,kz,itype)
          endif
          call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &                   trim(ctrc_title(ktr)),     ! plot name
     &                   trim(ctrc_lname(ktr)),     ! ncdf name
     &                   trim(ctrc_sname(ktr)),     ! ncdf standard_name
     &                   trim(ctrc_units(ktr)),     ! units
     &                   kz, frmt,ioin)
          if     (zbot.gt.0.0) then
            if     (.not.lcell) then !point interpolation
              call layer2z_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
            else !cell average
              call layer2c_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
            endif
            call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &              trim(ctrc_title(ktr))//".bot",     ! plot name
     &              "bot_"//trim(ctrc_lname(ktr)),     ! ncdf name
     &              ' ',                               ! ncdf standard_name
     &              trim(ctrc_units(ktr)),             ! units
     &              0,.false., frmt,ioin)
         endif !zbot
        endif
        enddo  !ktr= 1,ntracr
c
c --- --------------------------
c --- kinetic energy
c --- --------------------------
c
      if     (artype.ge.2) then  ! mean or std. archive
c ---   'keio  ' = kinetic energy I/O unit (0 no I/O)
        ioin=iokein
        if (ioin.gt.0) then
          do k= 1,kk
            do j=1,jj
              do i=1,ii
                if (ip(i,j).ne.0 .and. i.lt.ii .and. j.lt.jj) then
                  if     (p(i,j,k).lt.p(i,j,kk+1)-bot) then
                    utilk(i,j,k)=ke(i,j,k)
                  else
                    utilk(i,j,k)=utilk(i,j,k-1)
                  endif
                else
                  utilk(i,j,k)=flag
                endif
              enddo
            enddo
          enddo
          if     (.not.lcell) then !point interpolation
            call layer2z(utilk,p,utilz,zz,flag,ii,jj,kk,kz,itype)
          else !cell average
            call layer2c(utilk,p,utilz,zi,flag,ii,jj,kk,kz,itype)
          endif
          call horout_3z(utilz,zz, artype,yrflag,time3,iexpt,.true.,
     &                ' ke/mass ',                  ! plot name
     &                'kinetic_energy_per_mass',    ! ncdf name
     &                ' ',                          ! ncdf standard_name
     &                'm2/s2',                      ! units
     &                kz, frmt,ioin)
        if     (zbot.gt.0.0) then
          if     (.not.lcell) then !point interpolation
            call layer2z_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          else !cell average
            call layer2c_bot(utilk,p,util1,zbot,flag,ii,jj,kk,itype)
          endif
          call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &            ' ke/m.bot',                    ! plot name
     &            'bot_kinetic_energy_per_mass',  ! ncdf name
     &            ' ',                            ! ncdf standard_name
     &            'm2/s2',                        ! units
     &            0,.false., frmt,ioin)
        endif !zbot
        endif !ioin
      endif  ! mean or std. archive
c
c --- -------------------------------------------------------------------
c --- 2-D and 3-D fields last, to allow them to be appended to a 3-Z file
c --- -------------------------------------------------------------------
c
c --- --------------------
c --- bathymetry
c --- --------------------
c
c --- 'botio ' = bathymetry I/O unit (0 no I/O)
      ioin=iobotin
      if (ioin.gt.0) then
        k=0
        ltheta=.false.
        do j=1,jj
          do i=1,ii
            util1(i,j)=p(i,j,kk+1)
          enddo
        enddo
        call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &              ' bathymetry       ',       ! plot name
     &              'bathymetry',               ! ncdf name
     &              ' ',                        ! ncdf standard_name
     &              'm',                        ! units
     &              k,ltheta, frmt,ioin)
      endif !iobotin
c
c --- --------------------
c --- average density
c --- --------------------
c
c --- 'athio'  = average density  I/O unit (0 no I/O), OPTIONAL
      ioin=ioathin
      if (ioin.gt.0) then
        do j=1,jj
          do i=1,ii
            if (ip(i,j).ne.0) then
              dsumth = 0.d0
              dsumdp = 0.d0
              do k=1,kk    
                dsumth = dsumth + dp(i,j,k)*th3d(i,j,k)
                dsumdp = dsumdp + dp(i,j,k)
              enddo !k
              util1(i,j)=dsumth/dsumdp - thbase
            else
              util1(i,j)=flag
            endif
          enddo
        enddo
        k=0
        ltheta=.false.
        call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &              'average HYCOM th3d',       ! plot name
     &              'avth',                     ! ncdf name
     &              ' ',                        ! ncdf standard_name
     &              'kg/m^3',                   ! units
     &              k,ltheta, frmt,ioin)
      endif !ioathin
c
c --- --------------------
c --- mixed layer depth
c --- --------------------
c
c --- 'mltio ' = mix. lay. thick. I/O unit (0 no I/O)
      ioin=iomltin
      if (ioin.gt.0) then
c
c ---   -----------------------
c ---   temperature mixed layer
c ---   -----------------------
c
        if     (tempml.gt.0.0) then
          call mixlay_ild(util1,temp,p,flag,ii,jj,kk, tempml)
          k=0
          ltheta=.false.
          write(cline,'(a,f4.2,a)') 
     &      ' ILT (',tempml,' degC)'
          call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &                trim(cline),                   ! plot name
     &                'ilt',                         ! ncdf name
     &                'ocean_mixed_layer_thickness', ! ncdf standard_name
     &                'm',                           ! units
     &                k,ltheta, frmt,ioin)
        endif !tempml.gt.0.0
c
c ---   -------------------
c ---   density mixed layer
c ---   -------------------
c
        if     (densml.gt.0.0) then
          do j=1,jj
            do i=1,ii
              if (ip(i,j).ne.0) then
                util1(i,j)  =th3d(i,j,1)+densml  !density at the mld
                utilk(i,j,1)=th3d(i,j,1)
                do k= 2,kk
                  utilk(i,j,k)=max( th3d(i,j,k),utilk(i,j,k-1))
                enddo
              else
                do k= 1,kk
                  utilk(i,j,k)=flag
                enddo
                util1(i,j)=flag
              endif
            enddo !i
          enddo !j
          call mixlay(util1,utilk,p,flag,ii,jj,kk)
          k=0
          ltheta=.false.
          write(cline,'(a,f4.2,a)') 
     &      ' MLT (',densml,' kg/m3)'
          call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &                trim(cline),                   ! plot name
     &                'mlt',                         ! ncdf name
     &                'ocean_mixed_layer_thickness', ! ncdf standard_name
     &                'm',                           ! units
     &                k,ltheta, frmt,ioin)
        endif !densml.gt.0.0
c
c ---   ---------------------------------------
c ---   equivalent temperature mixed layer, PLM
c ---   ---------------------------------------
c
        if     (tmljmp.gt.0.0) then
          call mixlay_loc(util1,temp,saln,p,flag,ii,jj,kk, tmljmp)
          k=0
          ltheta=.false.
          write(cline,'(a,f4.2,a)') 
     &      ' MLT (',tmljmp,' degCeq)'
          call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &                trim(cline),                   ! plot name
     &                'mltplm',                      ! ncdf name
     &                'ocean_mixed_layer_thickness', ! ncdf standard_name
     &                'm',                           ! units
     &                k,ltheta, frmt,ioin)
        endif !tmljmp.gt.0.0
c
c ---   ---------------------------------------
c ---   equivalent temperature mixed layer, PQM
c ---   ---------------------------------------
c
        if     (tmljmq.gt.0.0) then
          call mixlay_locppm(util1,temp,saln,p,flag,ii,jj,kk, tmljmq)
          k=0
          ltheta=.false.
          write(cline,'(a,f4.2,a)') 
     &      'pMLT (',tmljmq,' degCeq)'
          call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &                trim(cline),                   ! plot name
     &                'mltppm',                      ! ncdf name
     &                'ocean_mixed_layer_thickness', ! ncdf standard_name
     &                'm',                           ! units
     &                k,ltheta, frmt,ioin)
        endif !tmljmq.gt.0.0
c
c ---   -----------------------------------------
c ---   Lorbacher et. al. temperature mixed layer
c ---   -----------------------------------------
c
        if     (tmlorb.eq.0.0) then
          call mixlay_lorb(util1,temp,p,flag,ii,jj,kk)
          k=0
          ltheta=.false.
          call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &                'ILT Lorbacher,Temp',          ! plot name
     &                'iltlor',                      ! ncdf name
     &                'ocean_mixed_layer_thickness', ! ncdf standard_name
     &                'm',                           ! units
     &                k,ltheta, frmt,ioin)
        endif !tmlorb==0.0
c
c ---   -----------------------------------------------
c ---   Lorbacher et. al. potential density mixed layer
c ---   -----------------------------------------------
c
        if     (dmlorb.eq.0.0) then
          do j=1,jj
            do i=1,ii
              if (ip(i,j).ne.0) then
                offset=temp(i,j,1)+4.0*th3d(i,j,1)
                utilk(i,j,1)=offset-4.0*th3d(i,j,1) !temp.1
                do k= 2,kk
                  utilk(i,j,k)=offset-4.0*th3d(i,j,k)
                enddo
              else
                do k= 1,kk
                  utilk(i,j,k)=flag
                enddo
                util1(i,j)=flag
              endif
            enddo !i
          enddo !j
          call mixlay_lorb(util1,utilk,p,flag,ii,jj,kk)
          k=0
          ltheta=.false.
          call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &                'MLT Lorbacher,Dens',          ! plot name
     &                'mltlor',                      ! ncdf name
     &                'ocean_mixed_layer_thickness', ! ncdf standard_name
     &                'm',                           ! units
     &                k,ltheta, frmt,ioin)
        endif !dmlorb==0.0
c
c ---   ----------------------------
c ---   NAVO temperature mixed layer
c ---   ----------------------------
c
        if     (tmlnav.gt.0.0) then
          call mixlay_rp33(util1,temp,saln,p,flag,ii,jj,kk,tmlnav,1)
          k=0
          ltheta=.false.
          write(cline,'(a,f4.2,a)') 
     &      'nILT (',tmlnav,' degC)'
          call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &                trim(cline),                   ! plot name
     &                'iltnav',                      ! ncdf name
     &                'ocean_mixed_layer_thickness', ! ncdf standard_name
     &                'm',                           ! units
     &                k,ltheta, frmt,ioin)
        endif !tmlnav>0.0
c
c ---   ------------------------
c ---   NAVO density mixed layer
c ---   ------------------------
c
        if     (dmlnav.gt.0.0) then
          call mixlay_rp33(util1,temp,saln,p,flag,ii,jj,kk,dmlnav,2)
          k=0
          ltheta=.false.
          call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &                trim(cline),                   ! plot name
     &                'mltnav',                      ! ncdf name
     &                'ocean_mixed_layer_thickness', ! ncdf standard_name
     &                'm',                           ! units
     &                k,ltheta, frmt,ioin)
        endif !dmlnav>0.0
      endif !ioin.gt.0 (mixed layers)
c
c --- ------------------------
c --- SSH
c --- ------------------------
c
      if     (iossh.gt.0) then
         do j=1,jj
           do i=1,ii
             if (ip(i,j).ne.0) then
               util1(i,j)=srfht(i,j)/(thref*9806.0)  !MKS
             else
               util1(i,j)=flag
             endif
           enddo !i
         enddo !j
         k=0
         ltheta=.false.
         call horout(util1, artype,yrflag,time3,iexpt,.true.,
     &               ' sea surf. height ',       ! plot name
     &               'ssh',                      ! ncdf name (mersea)
     &               'sea_surface_elevation',    ! ncdf standard_name
     &               'm',                        ! units
     &               k,ltheta, frmt,iossh)
      endif !iossh.gt.0
c
c --- --------------------
c --- interface depth
c --- --------------------
c
c --- 'infio ' = interface depths I/O unit (0 no I/O, <0 label with layer #)
      ioin=ioinfin
      if (ioin.ne.0) then
        ltheta = ioin .gt. 0
        ioin   = abs(ioin)
        do k= 1,kk
          do j=1,jj
            do i=1,ii
              if (ip(i,j).ne.0) then
                utilk(i,j,k)=p(i,j,k+1)
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
        enddo
        call horout_3d(utilk, artype,yrflag,time3,iexpt,.true.,
     &              '  i.depth',                ! plot name
     &              'interface_depth',          ! ncdf name
     &              ' ',                        ! ncdf standard_name
     &              'm',                        ! units
     &              1,kk,ltheta, frmt,ioin)
      endif
      stop '(normal)'
      end
