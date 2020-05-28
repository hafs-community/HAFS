      program archv2data2d

      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
c
c --- hycom/micom 2-d diagnostic field extractor
c
      real, allocatable, dimension (:,:)   ::
     &   uflux,vflux,vort,strmf,ubaro_,vbaro_,
     &   depth1, depthu,depthv,dpu,dpv, util1,work
      real, allocatable, dimension (:,:,:) ::
     &   utilk,pw
c
      common/conrng/ amn,amx
c
      character flnm*240,frmt*80
      logical   smooth,mthin,icegln,lperiod
c
      logical plot4(4)
      real    qq4(4),qc4(4)
c
      logical          ltheta
      integer          artype,iexpt,iversn,kkin,yrflag,mxlflg
      double precision time3(3)
c
      real, parameter :: flag = 2.0**100
c
c --- 'lhycom' -- hycom (vs micom) input file
c --- 'trcout' -- tracer input
      logical   lhycom,trcout
      data      lhycom/.true. /, trcout/.false./
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

      logical   insitu
      data      insitu/.true./
      real      depth_m, t_deg

c
      integer   fdate(4), verfhour
      integer   surflg
c
      real      pot2pot0
cdbgz
      real, parameter :: emnp_min=-4000.0
c
      call xcspmd
      call zaiost
      lp=6
      film=onemm
c
c --- read model data
c ---   'flnm  ' = name of file containing the actual data
c ---   'frmt  ' = output format or type (HYCOM, BINARY, netCDF)
c ---                see horout for more details on frmt
c ---   'iexpt ' = experiment number x10  (000=from archive file)
c ---   'yrflag' = days in year flag (0=360J16,1=366J16,2=366J01,3=actual)
c ---   'idm   ' = longitudinal array size
c ---   'jdm   ' = latitudinal  array size
c ---   'kdm   ' = number of layers
        read (*,'(a)') flnm
        write (lp,'(2a)') ' input file: ',trim(flnm)
        call flush(lp)
        read (*,'(a)') frmt
        write (lp,'(2a)') 'output type: ',trim(frmt)
        call flush(lp)

C get date information
c
c ---   'yyyy  ' = year
c ---   'month ' = month
c ---   'day   ' = day
c ---   'hour  ' = hour
c ---   'verfhr' = verification hour
c
        call blkini(fdate(1),'yyyy  ')
        call blkini(fdate(2),'month ')
        call blkini(fdate(3),'day   ')
        call blkini(fdate(4),'hour  ')
        call blkini(verfhour,'verfhr')

        call blkini(iexpt, 'iexpt ')
        call blkini(yrflag,'yrflag')
        call blkini(ii,    'idm   ')
        call blkini(jj,    'jdm   ')
        call blkini(kk,    'kdm   ')
        if     (ii.ne.idm .or. jj.ne.jdm) then
          write(lp,*)
          write(lp,*) 'error - wrong idm or jdm (should be:',
     &                                           idm,jdm,')'
          write(lp,*)
          call flush(lp)
          stop 9
        endif
c
c ---   'thbase' = reference density (sigma units)
        call blkinr(thbase,
     &             'thbase','("blkinr: ",a6," =",f11.4," sig")')
c
c ---   'surflg' =number of  surface heat flux fields [1,3,4]
        call blkini(surflg,'surflg')
        if(surflg.lt.1.or.surflg.gt.4.or.surflg.eq.2) then
                     write(lp,*)
          write(lp,*) 'error - wrong surflg (should be: 1|3|4 '
          write(lp,*)
          call flush(lp)
          stop 9
       endif


c
c ---   'smooth' = smooth fields before output
c ---   'mthin ' = mask thin layers from output
        call blkinl(smooth,'smooth')
        call blkinl(mthin, 'mthin ')
c
c ---   'iorign' = i-origin of sampled subregion
c ---   'jorign' = j-origin of sampled subregion
c ---   'idmp  ' = i-extent of sampled subregion (<=idm; 0 implies idm)
c ---   'jdmp  ' = j-extent of sampled subregion (<=jdm; 0 implies jdm)
        call blkini(iorign,'iorign')
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
        write (lp,'(2(a,i5),9x,2(a,i5))') 'extracting i =',iorign,
     &    ' ...',iorign+ii-1,'j =',jorign,' ...',jorign+jj-1
        call flush(lp)

c
c --- array allocation
c
      call plot_alloc
c
      allocate(  uflux(ii,jj) )
      allocate(  vflux(ii,jj) )
      allocate(   vort(ii,jj) )
      allocate(   ubaro_(ii,jj) )
      allocate(   vbaro_(ii,jj) )
      allocate(  strmf(ii,jj) )
      allocate( depth1(ii,jj) )
      allocate( depthu(ii,jj) )
      allocate( depthv(ii,jj) )
      allocate(    dpu(ii,jj) )
      allocate(    dpv(ii,jj) )
      allocate(  util1(ii,jj) )
      allocate(   work(ii,jj) )
c
      allocate(  utilk(ii,jj,kk) )
      allocate(     pw(ii,jj,kk) )
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
      if (lhycom) then
        call getdat(flnm,time3,artype,initl,icegln,trcout,surflg,
     &              iexpt,iversn,yrflag,kkin)       ! hycom input
        time = time3(3)
      else
        call getdtm(flnm,time,initl, thbase)        ! micom input
        artype = 1
        iversn = 10
      endif
c
      if     (artype.eq.3) then
        smooth = .false.
        mthin  = .false.
      endif
c
      write(lp,'(/a,2f8.2/a,2f8.2)') 
     &     'sub-domain longitude range = ',
     &    minval(plon(:,:)),maxval(plon(:,:)),
     &     'sub-domain latitude  range = ',
     &    minval(plat(:,:)),maxval(plat(:,:))
c
      lperiod = maxval(plon(:,:))-minval(plon(:,:)) .gt. 350.0
      if     (lperiod) then
        write(lp,'(/a/)') 'sub-domain assumed to be periodic'
      else
        write(lp,'(/a/)') 'sub-domain assumed to be non-periodic'
      endif
      call flush(lp)
c
      call bigrid(depths)
      call flush(lp)
c
c --- check that bathymetry is consistent with this archive.
c --- only possible with hycom .[ab] file input.
c
      if     (iversn.ge.20) then
        ibad = 0
        do j= 1,jj
          do i= 1,ii
            if     (ip(i,j).eq.1) then
              if     (srfht(i,j).gt.2.0**99) then
                ibad = ibad + 1   ! topo sea, srfht land
              endif
            else
              if     (srfht(i,j).lt.2.0**99) then
                ibad = ibad + 1   ! topo land, srfht sea
              endif
            endif
          enddo !i
        enddo !j
        if     (ibad.ne.0) then
          write(lp,*)
          write(lp,*) 'error - wrong bathymetry for this archive file'
          write(lp,*) 'number of mismatches = ',ibad
          write(lp,*)
          call flush(lp)
c         stop 9
        endif !ibad.ne.0
      endif !iversn.ge.20
c
c --- cover islands with thin film of water for stream functions
        do 23 j=1,jj1
        do 23 i=1,ii1
 23     depth1(i,j)=depths(i,j)
c
        call sbmerg(depth1,film)
c
        call bigrd1(depth1)
c
      do 3 k=1,kkin
      do 3 j=1,jj
      do 3 i=1,ii
c
c --- convert baroclinic to total velocities by adding barotropic component
      if (k.eq.1) then
        if     (iu(i,j).eq.1 .and. artype.eq.1) then
          umix(i,j)=umix(i,j)+ubaro(i,j)
        elseif (iu(i,j).ne.1) then
          umix(i,j)=0.
        end if
        if     (iv(i,j).eq.1 .and. artype.eq.1) then
          vmix(i,j)=vmix(i,j)+vbaro(i,j)
        elseif (iv(i,j).ne.1) then
          vmix(i,j)=0.
        end if
      endif
      if     (iu(i,j).eq.1 .and. artype.eq.1) then
        u(i,j,2*k)=u(i,j,2*k)+ubaro(i,j)
      elseif (iu(i,j).ne.1) then
        u(i,j,2*k)=0.
      end if
      if     (iv(i,j).eq.1 .and. artype.eq.1) then
        v(i,j,2*k)=v(i,j,2*k)+vbaro(i,j)
      elseif (iv(i,j).ne.1) then
        v(i,j,2*k)=0.
      end if
c
c --- convert layer thickness to meters
      if (depths(i,j).gt.0.) then
        dp(i,j,k)=dp(i,j,k)/9806.
        p(i,j,k+1)=p(i,j,k)+dp(i,j,k)
        th3d(i,j,2*k)=th3d(i,j,2*k)+thbase
      else
        saln(i,j,2*k)=flag
        temp(i,j,2*k)=flag
        th3d(i,j,2*k)=flag
        ke(  i,j,2*k)=flag
        dp(i,j,k)=flag
        p(i,j,k+1)=flag
        if (depths(i,j).eq.film) p(i,j,k+1)=film
      endif
 3    continue
c
c --- interface vertical velocity from continuity equation
      do j= 1,jj
        do i= 1,ii
          depthu(i,j) = min(depths(i,j),depths(i-1,j))  ! depths(0,j) is ok
          depthv(i,j) = min(depths(i,j),depths(i,j-1))  ! depths(i,0) is ok
        enddo
      enddo
      do j= 1,jj-1
        do i= 1,ii-1
          if     (ip(i,j).eq.1) then
            pwk = (ubaro(i+1,j)*depthu(i+1,j)-
     &             ubaro(i  ,j)*depthu(i,  j) )/scpx(i,j) +
     &            (vbaro(i,j+1)*depthv(i,j+1)-
     &             vbaro(i,j  )*depthv(i,j  ) )/scpy(i,j)
            pw(i,j,kkin) = pwk
          endif
        enddo
      enddo
      do k= 1,kkin
        do j= 1,jj
          do i= 1,ii
            if     (iu(i,j).eq.1 .and. i.gt.1) then
              dpu(i,j)=max(0.,
     &           min(depthu(i,j),.5*(p(i,j,k+1)+p(i-1,j,k+1)))-
     &           min(depthu(i,j),.5*(p(i,j,k  )+p(i-1,j,k  ))))
            else
              dpu(i,j) = 0.0
            endif
            if     (iv(i,j).eq.1 .and. j.gt.1) then
              dpv(i,j)=max(0.,
     &             min(depthv(i,j),.5*(p(i,j,k+1)+p(i,j-1,k+1)))-
     &             min(depthv(i,j),.5*(p(i,j,k  )+p(i,j-1,k  ))))
            else
              dpv(i,j) = 0.0
            endif
          enddo
        enddo
        do j= 2,jj-1
          pw(1, j,k) = flag
          do i= 2,ii-1
            if     (ip(i,j).eq.1) then
              pwk = (u(i+1,j,2*k)*dpu(i+1,j)-
     &               u(i  ,j,2*k)*dpu(i,  j) )/scpx(i,j) +
     &              (v(i,j+1,2*k)*dpv(i,j+1)-
     &               v(i,j  ,2*k)*dpv(i,j  ) )/scpy(i,j)
              if     (k.eq.1) then
                pw(i,j,k) =               pwk -
     &                      pw(i,j,kkin)*dp(i,j,k)/depths(i,j)
              else
                pw(i,j,k) = pw(i,j,k-1) + pwk -
     &                      pw(i,j,kkin)*dp(i,j,k)/depths(i,j)
              endif
            else
              pw(i,j,k) = flag
            endif
          enddo
          pw(ii,j,k) = flag
        enddo
        do i= 1,ii
          pw(i, 1,k) = flag
          pw(i,jj,k) = flag
        enddo
      enddo
      if     (smooth) then
        do k= 1,kkin
          call psmoo(pw(1,1,k),work)
        enddo
      endif
c
ccc      x=thrufl(107,209,122,212,'(Drake Passage)')
ccc      x=thrufl(41,199,44,201,'(Florida Straits)')
ccc      x=thrufl(63,76,69,94,'(Indonesia)')
c
      do 7 j=1,jj
      do 7 i=1,ii
      if (depths(i,j).gt.0.) then
        srfht( i,j)=srfht( i,j)/(thref*98.06)  ! cm
        dpbl(  i,j)=dpbl(  i,j)/9806.          ! m
        dpmixl(i,j)=dpmixl(i,j)/9806.          ! m
        thmix( i,j)=thmix( i,j)+thbase         ! SigmaT
        if     (artype.ne.3) then
          ttrend(i,j)=surflx(i,j)*thref*8.64E4
     &                    /spcifh/dpbl(i,j)      ! deg/day
          strend(i,j)=salflx(i,j)*thref*8.64E4
     &                           /dpbl(i,j)      ! psu/day
          emnp(  i,j)=salflx(i,j)*thref*8.64E7
cdbgz
     &                 /max(saln(i,j,2),1.)
c     &                           /saln(i,j,2)    ! mm/day
     &                           /10.            ! cm/day
        else  ! std.dev, archive
          ttrend(i,j)=flag
          strend(i,j)=flag
          emnp(  i,j)=salflx(i,j)*thref*8.64E7
     &                           /35.00          ! mm/day
     &                           /10.            ! cm/day

        endif
cdbgz
        if (emnp(  i,j)<emnp_min) emnp(  i,j)=emnp_min
 
        if     (covice(i,j).eq.0.0) then
          thkice(i,j)= 0.0
          temice(i,j)=-1.8
        endif
      else
        srfht( i,j)=flag
        surflx(i,j)=flag
        surflx_evap(i,j)=flag
        surflx_snsi(i,j)=flag
        surflx_assi(i,j)=flag
        salflx(i,j)=flag
        ttrend(i,j)=flag
        strend(i,j)=flag
          emnp(i,j)=flag
        covice(i,j)=flag
        thkice(i,j)=flag
        temice(i,j)=flag
        dpbl(  i,j)=flag
        dpmixl(i,j)=flag
        kebaro(i,j)=flag
        tmix( i,j)=flag
        smix( i,j)=flag
        thmix(i,j)=flag
        kemix(i,j)=flag
      end if
      if (kkin.eq.1 .or. kkin.lt.kk) then
        if (depths(i,j).gt.0.) then
          p(i,j,kk+1)=depths(i,j)
        else
          p(i,j,kk+1)=flag
        endif
      endif
 7    continue
c
      dpth=0.5*onecm
c
c --- put vertically averaged t,s values into massless layers
c --- only if not plotted horizontally (improves vertical plots)
c
      if (mthin) then
      do 70 k=2,kkin
c
      do 70 j=1,jj
      do 70 i=1,ii
c
      if (depths(i,j).gt.0.) then
        temp(i,j,2*k-1)=0.
        saln(i,j,2*k-1)=0.
        th3d(i,j,2*k-1)=0.
        pmid=.5*(p(i,j,k)+p(i,j,k+1))
        phi=pmid+dpth
        plo=pmid-dpth
c
        sum=0.
        do 71 k1=1,kkin
        delp=max(0.,min(p(i,j,k1+1),phi)-max(p(i,j,k1),plo))
        sum=sum+delp
        temp(i,j,2*k-1)=temp(i,j,2*k-1)+temp(i,j,2*k1)*delp
        saln(i,j,2*k-1)=saln(i,j,2*k-1)+saln(i,j,2*k1)*delp
 71     th3d(i,j,2*k-1)=th3d(i,j,2*k-1)+th3d(i,j,2*k1)*delp
c
        temp(i,j,2*k)=temp(i,j,2*k-1)/sum
        saln(i,j,2*k)=saln(i,j,2*k-1)/sum
        th3d(i,j,2*k)=th3d(i,j,2*k-1)/sum
      end if
 70   continue
      endif !mthin
c
      if (smooth) then
c
c --- smooth mass field variables
c
      call psmoo(temp(1,1,2),work)
      call psmoo(saln(1,1,2),work)
      call psmoo(th3d(1,1,2),work)
      call psmoo(tmix,work)
      call psmoo(smix,work)
      call psmoo(thmix,work)
c
      do 38 k=2,kkin
c
      do 76 j=1,jj1
      do 76 i=1,ii1
      if (depths(i,j).gt.0.) then
        util1(i,j)=max(onemm,dp(i,j,k))
        temp(i,j,2*k-1)=temp(i,j,2*k)*util1(i,j)
        saln(i,j,2*k-1)=saln(i,j,2*k)*util1(i,j)
        th3d(i,j,2*k-1)=th3d(i,j,2*k)*util1(i,j)
      else
        temp(i,j,2*k-1)=flag
        saln(i,j,2*k-1)=flag
        th3d(i,j,2*k-1)=flag
      end if
 76   continue
c
      call psmoo(util1,work)
      call psmoo(temp(1,1,2*k-1),work)
      call psmoo(saln(1,1,2*k-1),work)
      call psmoo(th3d(1,1,2*k-1),work)
c
      do 38 j=1,jj1
      do 38 i=1,ii1
      if (depths(i,j).gt.0.) then
        temp(i,j,2*k)=temp(i,j,2*k-1)/util1(i,j)
        saln(i,j,2*k)=saln(i,j,2*k-1)/util1(i,j)
        th3d(i,j,2*k)=th3d(i,j,2*k-1)/util1(i,j)
      end if
 38   continue
c
c --- smooth velocity and layer thickness fields
c
      do 30 k=1,kkin
c
      do 31 j=1,jj1
      do 31 i=2,ii1
******if(k.eq.1) umix(i,j)=umix(i,j)*(dpmixl(i,j)+dpmixl(i-1,j))
 31   uflux(i,j)=u(i,j,2*k)*max(onecm,dp(i,j,k)+dp(i-1,j,k))
c
      do 32 j=2,jj1
      do 32 i=1,ii1
******if(k.eq.1) vmix(i,j)=vmix(i,j)*(dpmixl(i,j)+dpmixl(i,j-1))
 32   vflux(i,j)=v(i,j,2*k)*max(onecm,dp(i,j,k)+dp(i,j-1,k))
c
      if(k.eq.1) then
        call usmoo(umix,work)
        call vsmoo(vmix,work)
      end if
      call usmoo(uflux,work)
      call vsmoo(vflux,work)
      call psmoo(dp(1,1,k),work)
c --- (warning: smoothed -dp- field unsuitable for deriving interface depths)
c
      do 33 j=1,jj1
      do 33 i=2,ii1
******if(k.eq.1) umix(i,j)=umix(i,j)/(dpmixl(i,j)+dpmixl(i-1,j))
 33   u(i,j,2*k)=uflux(i,j)/max(onecm,dp(i,j,k)+dp(i-1,j,k))
c
      do 34 j=2,jj1
      do 34 i=1,ii1
******if(k.eq.1) vmix(i,j)=vmix(i,j)/(dpmixl(i,j)+dpmixl(i,j-1))
 34   v(i,j,2*k)=vflux(i,j)/max(onecm,dp(i,j,k)+dp(i,j-1,k))
c
c --- now smooth layer interfaces and find corresponding -dp- field
      if (k.lt.kkin) call psmo1(p(1,1,k+1),work,p(1,1,kk+1))
c --- now smooth boundary layer thickness and mixed layer base
      if (k.eq.1) then
        call psmo1(dpbl,work,p(1,1,kk+1))
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
      end if			!  smooth = .true.
c
c --- put vertically averaged u,v values into massless layers
c --- only if not plotted horizontally (improves vertical plots)
c
      if (mthin) then
      do 74 k=2,kkin
c
      do 72 j=1,jj
      do 72 i=2,ii
      if (min(depths(i,j),depths(i-1,j)).gt.0.) then
c
        u(i,j,2*k-1)=0.
        pmid=.25*(p(i,j,k)+p(i-1,j,k)+p(i,j,k+1)+p(i-1,j,k+1))
        plo=pmid-dpth
        phi=pmid+dpth
c
        sum=0.
        do 73 k1=1,kkin
        delp=max(0.,min(.5*(p(i,j,k1+1)+p(i-1,j,k1+1)),phi)
     &             -max(.5*(p(i,j,k1  )+p(i-1,j,k1  )),plo))
        sum=sum+delp
 73     u(i,j,2*k-1)=u(i,j,2*k-1)+u(i,j,2*k1)*delp
c
        u(i,j,2*k)=u(i,j,2*k-1)/sum
      endif
 72   continue
c
      do 74 j=2,jj
      do 74 i=1,ii
      if (min(depths(i,j),depths(i,j-1)).gt.0.) then
c
        v(i,j,2*k-1)=0.
        pmid=.25*(p(i,j,k)+p(i,j-1,k)+p(i,j,k+1)+p(i,j-1,k+1))
        plo=pmid-dpth
        phi=pmid+dpth
c
        sum=0.
        do 75 k1=1,kkin
        delp=max(0.,min(.5*(p(i,j,k1+1)+p(i,j-1,k1+1)),phi)
     &             -max(.5*(p(i,j,k1  )+p(i,j-1,k1  )),plo))
        sum=sum+delp
 75     v(i,j,2*k-1)=v(i,j,2*k-1)+v(i,j,2*k1)*delp
c
        v(i,j,2*k)=v(i,j,2*k-1)/sum
      endif
 74   continue
      endif !mthin
c
      do 97 k=1,kkin
      do 97 j=1,jj
      do 97 i=1,ii
        ke(i,j,2*k-1)=  ke(i,j,2*k)
      temp(i,j,2*k-1)=temp(i,j,2*k)
      saln(i,j,2*k-1)=saln(i,j,2*k)
 97   th3d(i,j,2*k-1)=th3d(i,j,2*k)
c
c --- -------------------------
c --- output non-layered fields
c --- -------------------------
c
      k=0
      ltheta=.false.
c
c --- 'botio ' = bathymetry I/O unit (0 no I/O)
      call blkini(ioin,'botio ')
      if (ioin.gt.0) then
        do j=1,jj
          do i=1,ii
            util1(i,j)=p(i,j,kk+1)
          enddo
        enddo
        call horout(util1, artype,yrflag,time3,iexpt,lhycom,
     &              ' bathymetry       ',       ! plot name
     &              'bathymetry',               ! ncdf name
     &              'm',                        ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
c --- -------------------
c ---  surface fluxes
c --- -------------------
c
c --- 'flxio ' = surf. heat  flux I/O unit (0 no I/O)
      call blkini(ioin,'flxio ')
      if (ioin.gt.0) then
        call horout(surflx,artype,yrflag,time3,iexpt,lhycom,
     &              ' surf. heat flux  ',       ! plot name
     &              'surface_heat_flux',        ! ncdf name
     &              'w/m2',                     ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c --- 'empio ' = surf. evap-pcip I/O unit (0 no I/O)
      call blkini(ioin,'empio ')
      if (ioin.gt.0) then
        write(6,*) '  emnp= ',amn,amx,qq
        call horout(  emnp,artype,yrflag,time3,iexpt,lhycom,
     &              ' surf. evap-precip',                 ! plot name
     &              'surface_evaporation_precipitation',  ! ncdf name
     &              'cm/day',                             ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif

c --- 'evpio ' = evaporative surf. flux I/O unit (0 no I/O)
      call blkini(ioin,'evpio ')
      if (ioin.gt.0) then
        call horout(surflx_evap,artype,yrflag,time3,iexpt,lhycom,
     &              ' evap. surf. heat flux  ',       ! plot name
     &              'evaporative_heat_flux',        ! ncdf name
     &              'w/m2',                     ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c --- 'snsio ' = sensible heat flux I/O unit (0 no I/O)
      call blkini(ioin,'snsio ')
      if (ioin.gt.0) then
        call horout(  surflx_snsi,artype,yrflag,time3,iexpt,lhycom,
     &              ' sensible surf. heat flux',                 ! plot name
     &              'sensible_heat_flux',  ! ncdf name
     &              'w/m2',                     ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c --- 'hasio ' = assimilation heat flux I/O unit (0 no I/O)
      call blkini(ioin,'hasio ')
      if (ioin.gt.0) then
        call horout(  surflx_assi,artype,yrflag,time3,iexpt,lhycom,
     &              ' assim.  surf. heat flux',                 ! plot name
     &              'assimilation_heat_flux',  ! ncdf name
     &              'w/m2',                     ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c --- 'ttrio ' = surf. temp trend I/O unit (0 no I/O)
      call blkini(ioin,'ttrio ')
      if (ioin.gt.0) then
        call horout(ttrend,artype,yrflag,time3,iexpt,lhycom,
     &              ' surf. temp. trend',         ! plot name
     &              'surface_temperature_trend',  ! ncdf name
     &              'degC/day',                   ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c --- 'strio ' = surf. saln trend I/O unit (0 no I/O)
      call blkini(ioin,'strio ')
      if (ioin.gt.0) then
        write(6,*) 'strend= ',amn,amx,qq
        call horout(strend,artype,yrflag,time3,iexpt,lhycom,
     &              ' surf. saln. trend',       ! plot name
     &              'surface_salinity_trend',   ! ncdf name
     &              'psu/day',                  ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
c --- -----------------
c --- output ice fields
c --- -----------------
c
c --- 'icvio ' = ice coverage I/O unit (0 no I/O)
      call blkini(ioin,'icvio ')
      if (ioin.gt.0) then
        call horout(covice,artype,yrflag,time3,iexpt,lhycom,
     &              '     ice coverage ',       ! plot name
     &              'ice_coverage',             ! ncdf name
     &              ' ',                        ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
c --- 'ithio ' = ice thickness I/O unit (0 no I/O)
      call blkini(ioin,'ithio ')
      if (ioin.gt.0) then
        call horout(thkice,artype,yrflag,time3,iexpt,lhycom,
     &              '    ice thickness ',       ! plot name
     &              'ice_thickness',            ! ncdf name
     &              'm',                        ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
c --- 'ictio ' = ice temperature I/O unit (0 no I/O)
      call blkini(ioin,'ictio ')
      if (ioin.gt.0) then
        call horout(temice,artype,yrflag,time3,iexpt,lhycom,
     &              '   ice temperature ',       ! plot name
     &              'ice_temperature',           ! ncdf name
     &              'degC',                     ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
c --- ---------------------
c --- output surface fields
c --- ---------------------
c
c --- 'sshio ' = sea surf. height I/O unit (0 no I/O)
      call blkini(ioin,'sshio ')
      if (ioin.gt.0) then
        call horout(srfht, artype,yrflag,time3,iexpt,lhycom,
     &              ' sea surf. height ',       ! plot name
     &              'sea_surface_height',       ! ncdf name
     &              'cm',                       ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
c
c --- 'mtgio ' = montgomery potential at the surface.  I/O unit (0 no I/O)
      call blkini(ioin,'mtgio ')
      if (ioin.gt.0) then
        call horout(montg,  artype,yrflag,time3,iexpt,lhycom,
     &              ' Montgomery-pot.  ' ,      ! plot name
     &              'montgomery_potential_surf',  ! ncdf name
     &              'm2/s2'     ,                 ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
      if     (artype.gt.1) then  ! mean or std. archive
c ---   'bkeio ' = baro. kinetic energy I/O unit (0 no I/O)
        call blkini(ioin,'bkeio ')
        if (ioin.gt.0) then
          call horout(kebaro,artype,yrflag,time3,iexpt,lhycom,
     &                '    baro.k.e./mass',              ! plot name
     &                'barotropic_kinetic_energy/mass',  ! ncdf name
     &                'm2/s2',                           ! units
     &                k,ltheta, frmt,ioin,fdate,verfhour)
        endif
      endif  ! mean or std. archive
c
c --- 'bsfio ' = baro. strmfn. I/O unit (0 no I/O)
      call blkini(ioin,'bsfio ')
      if (ioin.gt.0) then
        do j= 1,jj
          jm1=max(1,j-1)
          do i= 1,ii
            im1=max(1,i-1)
c
            util1(i,j)=1.
c
            if (iu(i,j).eq.1) then
              ubaro_(i,j)=ubaro(i,j)*min(p(i,j,kk+1),p(im1,j,kk+1))
            else
              ubaro_(i,j)=0.0
            endif
c
            if (iv(i,j).eq.1) then
              vbaro_(i,j)=vbaro(i,j)*min(p(i,j,kk+1),p(i,jm1,kk+1))
            else
              vbaro_(i,j)=0.0
            endif
c
            if (iq(i,j).eq.1) then
              vort(i,j)=
     &         (vbaro_(i,j)*scvy(i,j)-vbaro_(im1,j)*scvy(im1,j)
     &         -ubaro_(i,j)*scux(i,j)+ubaro_(i,jm1)*scux(i  ,jm1))*1.e-6
            else
              vort(i,j)=0.0
            endif
          enddo 
        enddo 
c
        call poisnd(ii,jj,vort,strmf,util1,util1,work)
c
        do j=1,jj
          do i=1,ii
            if (ip(i,j).eq.0) then
              strmf(i,j)=flag
            endif
          enddo
        enddo
c
        call qsmoo(strmf,work)
        call zebra(strmf,ii,ii,jj)
c
        call horout(strmf, artype,yrflag,time3,iexpt,lhycom,
     &              'barotropic strmf. ',         ! plot name
     &              'barotropic_streamfunction',  ! ncdf name
     &              'Sv',                         ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
c --- -------------------------
c --- output mixed layer fields
c --- -------------------------
c
c --- 'uvmio ' = mixed layer u-velocity I/O unit (0 no I/O)
      call blkini(ioin, 'uvmio ')
      if (ioin.gt.0) then
        do j=1,jj
          do i=1,ii
            if (ip(i,j).ne.0 .and. i.lt.ii) then
              util1(i,j)=50.0*(umix(i,j)+umix(i+1,j))
            else
              util1(i,j)=flag
            endif
          enddo
        enddo
        call horout(util1, artype,yrflag,time3,iexpt,lhycom,
     &              'mix.l. u-velocity ',       ! plot name
     &              'mixed_layer_u_velocity',   ! ncdf name
     &              'cm/s',                     ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
c --- 'vvmio ' = mixed layer v-velocity I/O unit (0 no I/O)
      call blkini(ioin, 'vvmio ')
      if (ioin.gt.0) then
        do j=1,jj
          do i=1,ii
            if (ip(i,j).ne.0 .and. j.lt.jj) then
              util1(i,j)=50.0*(vmix(i,j)+vmix(i,j+1))
            else
              util1(i,j)=flag
            endif
          enddo
        enddo
        call horout(util1, artype,yrflag,time3,iexpt,lhycom,
     &              ' mixl. v-velocity ',       ! plot name
     &              'mixed_layer_v_velocity',   ! ncdf name
     &              'cm/s',                     ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
c --- 'spmio ' = mixed layer speed I/O unit (0 no I/O)
      call blkini(ioin, 'spmio ')
      if (ioin.gt.0) then
        do j=1,jj
          do i=1,ii
            if (ip(i,j).ne.0 .and. i.lt.ii .and. j.lt.jj) then
              util1(i,j)=50.0*sqrt( (umix(i,j)+umix(i+1,j))**2 +
     &                              (vmix(i,j)+vmix(i,j+1))**2  )
            else
              util1(i,j)=flag
            endif
          enddo
        enddo
        call horout(util1, artype,yrflag,time3,iexpt,lhycom,
     &              'mixed-layer speed ',       ! plot name
     &              'mixed_layer_speed',        ! ncdf name
     &              'cm/s',                     ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
c --- 'bltio ' = bnd. lay. thick. I/O unit (0 no I/O)
      call blkini(ioin,'bltio ')
      if (ioin.gt.0) then
        call horout(dpbl,  artype,yrflag,time3,iexpt,lhycom,
     &              'bnd.layr.thickness',                ! plot name
     &              'surface_boundary_layer_thickness',  ! ncdf name
     &              'm',                                 ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
c --- 'mltio ' = mix. lay. thick. I/O unit (0 no I/O)
      call blkini(ioin,'mltio ')
      if (ioin.gt.0) then
        call horout(dpmixl,artype,yrflag,time3,iexpt,lhycom,
     &              'mix.layr.thickness',       ! plot name
     &              'mixed_layer_thickness',    ! ncdf name
     &              'm',                        ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
c --- 'sstio ' = mix. lay. temp.  I/O unit (0 no I/O)
      call blkini(ioin,'sstio ')

      if (ioin.gt.0) then
        do j=1,jj
          do i=1,ii
            if (ip(i,j).ne.0 ) then
              if (ip(i,j).ne.0 ) then
               if(insitu) then
                  t_deg=tmix(i,j)
                  depth_m=0.5*dpmixl(i,j)
c                  call pot_t2t(smix(i,j),t_deg,depth_m,util1(i,j))
                  util1(i,j)=pot2pot0(t_deg,smix(i,j))
               else
                  util1(i,j)=tmix(i,j)
               endif
               else
                  util1(i,j)=tmix(i,j)
               endif
            else
              util1(i,j)=flag
            endif
          enddo
        enddo

        call horout(util1,  artype,yrflag,time3,iexpt,lhycom,
     &              'mix.layr.temp     ',       ! plot name
     &              'mixed_layer_temperature',  ! ncdf name
     &              'degC',                     ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
c --- 'sssio ' = mix. lay. saln.  I/O unit (0 no I/O)
      call blkini(ioin,'sssio ')
      if (ioin.gt.0) then
        call horout(smix,  artype,yrflag,time3,iexpt,lhycom,
     &              'mix.layr.saln     ',       ! plot name
     &              'mixed_layer_salinity',     ! ncdf name
     &              'psu',                      ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
c --- 'ssdio ' = mix. lay. dens.  I/O unit (0 no I/O)
      call blkini(ioin,'ssdio ')
      if (ioin.gt.0) then
        call horout(thmix, artype,yrflag,time3,iexpt,lhycom,
     &              'mix.layr.dens     ',       ! plot name
     &              'mixed_layer_density',      ! ncdf name
     &              'sigma',                    ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif

c
c --- 'vubio ' = baro u-vel.  I/O unit (0 no I/O)
      call blkini(ioin,'vubio ')

      if (ioin.gt.0) then
        do j=1,jj
          do i=1,ii
            if (ip(i,j).ne.0 .and. i.lt.ii) then
              util1(i,j)=100.0*ubaro(i,j)
            else
              util1(i,j)=flag
            endif
          enddo
        enddo
        call horout(util1, artype,yrflag,time3,iexpt,lhycom,
     &              ' baro. u-velocity ',       ! plot name
     &              'barotropic_u_velocity ',   ! ncdf name
     &              'cm/s',                     ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c
c --- 'vvbio ' = baro v-vel.  I/O unit (0 no I/O)
      call blkini(ioin,'vvbio ')
      if (ioin.gt.0) then
        do j=1,jj
          do i=1,ii
            if (ip(i,j).ne.0 .and. j.lt.jj) then
              util1(i,j)=100*vbaro(i,j)
            else
              util1(i,j)=flag
            endif
          enddo
        enddo
        call horout(util1, artype,yrflag,time3,iexpt,lhycom,
     &              ' baro. v-velocity ',       ! plot name
     &              'barotropic_v_velocity ',   ! ncdf name
     &              'cm/s',                     ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c


c
      if     (artype.gt.1) then  ! mean or std. archive
c ---   'mkeio ' = m.l. kinetic energy I/O unit (0 no I/O)
        call blkini(ioin,'mkeio ')
        if (ioin.gt.0) then
          call horout(kemix, artype,yrflag,time3,iexpt,lhycom,
     &                '    mixl.k.e./mass',               ! plot name
     &                'mixed_layer_kinetic_energy/mass',  ! ncdf name
     &                'm2/s2',                            ! units
     &                k,ltheta, frmt,ioin,fdate,verfhour)
        endif
      endif  ! mean or std. archive
c
c --- ----------------------
c --- output selected layers
c --- ----------------------
c
      do  !layer loop
c
c ---   'kf    ' = first output layer (=0 end output; <0 label with layer #)
c ---   'kl    ' = last  output layer
        call blkini(kin,'kf    ')
        ltheta = kin.gt.0
        kf     = abs(kin)
        if     (kf.eq.0) then
          exit
        endif
        call blkini(kl, 'kl    ')
        kl = abs(kl)
        if     (kl.gt.kkin) then
          write(lp,'(a)') 'error - kl larger than kdm'
          write(lp,*) 'kl =', kl, 'kkin =', kkin
          exit
        elseif (kl.lt.kf) then
          write(lp,'(a)') 'error - kl smaller than kf'
          exit
        endif
c
c ---   -------------------
c ---   output layer velocity
c ---   -------------------
c
c ---   'uvlio ' = u-velocity I/O unit (0 no I/O)
        call blkini(ioin, 'uvlio ')
        if (ioin.gt.0) then
          do k= kf,kl
          do j=1,jj
            do i=1,ii
              if (ip(i,j).ne.0 .and. i.lt.ii) then
                utilk(i,j,k)=100.0*u(i,j,2*k)
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
          enddo
          call horout_3d(utilk, artype,yrflag,time3,iexpt,lhycom,
     &                ' u-veloc.',                ! plot name
     &                'u_velocity',               ! ncdf name
     &                'cm/s',                     ! units
     &                kf,kl,ltheta, frmt,ioin,fdate,verfhour)
        endif
c
c ---   'vvlio ' = v-velocity I/O unit (0 no I/O)
        call blkini(ioin, 'vvlio ')
        if (ioin.gt.0) then
          do k= kf,kl
          do j=1,jj
            do i=1,ii
              if (ip(i,j).ne.0 .and. j.lt.jj) then
                utilk(i,j,k)=100.0*v(i,j,2*k)
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
          enddo
          call horout_3d(utilk, artype,yrflag,time3,iexpt,lhycom,
     &                ' v-veloc.',                ! plot name
     &                'v_velocity',               ! ncdf name
     &                'cm/s',                     ! units
     &                kf,kl,ltheta, frmt,ioin,fdate,verfhour)
        endif
c
c ---   'splio ' = speed I/O unit (0 no I/O)
        call blkini(ioin, 'splio ')
        if (ioin.gt.0) then
          do k= kf,kl
          do j=1,jj
            do i=1,ii
              if (ip(i,j).ne.0 .and. i.lt.ii .and. j.lt.jj) then
                utilk(i,j,k)=50.0*sqrt( (u(i,j,2*k)+u(i+1,j,2*k))**2 +
     &                                (v(i,j,2*k)+v(i,j+1,2*k))**2  )
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
          enddo
          call horout_3d(utilk, artype,yrflag,time3,iexpt,lhycom,
     &                ' speed   ',                ! plot name
     &                'speed',                    ! ncdf name
     &                'cm/s',                     ! units
     &                kf,kl,ltheta, frmt,ioin,fdate,verfhour)
        endif
c
c --- ---------------------------
c --- interface vertical velocity
c --- ---------------------------
c
c --- 'iwvio ' = intf. k vertical velocity I/O unit (0 no I/O)
        call blkini2(ioin,i,'iwvio ','infio ')
        if     (i.eq.1) then
          infio = -99
        else
          infio = max(ioin,-1)
          ioin  = 0
        endif
        if (ioin.gt.0) then
          do k= kf,kl
          do j=1,jj
            do i=1,ii
              if (pw(i,j,k).ne.flag) then
                utilk(i,j,k)=86400.0*pw(i,j,k)
              else
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
          enddo
          call horout_3d(utilk, artype,yrflag,time3,iexpt,lhycom,
     &                '  i.veloc',                   ! plot name
     &                'interface_vertical_velocity', ! ncdf name
     &                'm/day',                       ! units
     &                kf,kl,ltheta, frmt,ioin,fdate,verfhour)
        endif
c
c ---   --------------------
c ---   interface depth
c ---   --------------------
c
c ---   'infio ' = intf. k depth  I/O unit (0 no I/O)
        if     (infio.eq.-99) then
          call blkini(ioin,'infio ')
        else
          ioin = infio
        endif
        if (ioin.gt.0) then
          do k= kf,kl
          do j=1,jj
            do i=1,ii
              utilk(i,j,k)=p(i,j,k+1)
            enddo
          enddo
          enddo
          call horout_3d(utilk, artype,yrflag,time3,iexpt,lhycom,
     &                '  i.depth',                ! plot name
     &                'interface_depth',          ! ncdf name
     &                'm',                        ! units
     &                kf,kl,ltheta, frmt,ioin,fdate,verfhour)
        endif
c
c ---   --------------------
c ---   layer thickness
c ---   --------------------
c
c ---   'thkio ' = lay.  k thick. I/O unit (0 no I/O)
        call blkini(ioin,'thkio ')
        if (ioin.gt.0) then
          do k= kf,kl
          do j=1,jj
            do i=1,ii
              utilk(i,j,k)=dp(i,j,k)
              if (p(i,j,k)+onecm.gt.p(i,j,kk+1)) then
                utilk(i,j,k)=flag
              endif
            enddo
          enddo
          enddo
          call horout_3d(utilk, artype,yrflag,time3,iexpt,lhycom,
     &                '  thknss ',                ! plot name
     &                'layer_thickness',          ! ncdf name
     &                'm',                        ! units
     &                kf,kl,ltheta, frmt,ioin,fdate,verfhour)
        endif
c
c ---   ----------------
c ---   temperature
c ---   ----------------
c
c ---   'temio ' = layer k temp I/O unit (0 no I/O)
        call blkini(ioin,'temio ')
        if (ioin.gt.0) then
          do k= kf,kl
          do j=1,jj
            do i=1,ii
              if (ip(i,j).ne.0 ) then

               if(insitu) then
                  t_deg=temp(i,j,2*k)
                  depth_m=0.5*(p(i,j,k)+p(i,j,k+1))
c                call pot_t2t(saln(i,j,2*k),t_deg,depth_m,utilk(i,j,k))
                  utilk(i,j,k)=pot2pot0(t_deg,saln(i,j,2*k))
               else
                  utilk(i,j,k)=temp(i,j,2*k)
               endif

               else
                  utilk(i,j,k)=temp(i,j,2*k)
               endif

              if (mthin) then
                if (p(i,j,k)+onecm.gt.p(i,j,k+1)) then
                  utilk(i,j,k)=flag
                endif
              elseif (artype.ne.3) then
                if (p(i,j,k)+onecm.gt.p(i,j,kk+1)) then
                  utilk(i,j,k)=flag
                endif
              endif
            enddo
          enddo
          enddo
          call horout_3d(utilk, artype,yrflag,time3,iexpt,lhycom,
     &                '  temp   ',                ! plot name
     &                'layer_temperature',        ! ncdf name
     &                'degC',                     ! units
     &                kf,kl,ltheta, frmt,ioin,fdate,verfhour)
        endif
c
c ---   -------------
c ---   salinity
c ---   -------------
c
c ---   'salio ' = lay.  k saln. I/O unit (0 no I/O)
        call blkini(ioin,'salio ')
        if (ioin.gt.0) then
          do k= kf,kl
          do j=1,jj
            do i=1,ii
              utilk(i,j,k)=saln(i,j,2*k)
              if (mthin) then
                if (p(i,j,k)+onecm.gt.p(i,j,k+1)) then
                  utilk(i,j,k)=flag
                endif
              elseif (artype.ne.3) then
                if (p(i,j,k)+onecm.gt.p(i,j,kk+1)) then
                  utilk(i,j,k)=flag
                endif
              endif
            enddo
          enddo
          enddo
          call horout_3d(utilk, artype,yrflag,time3,iexpt,lhycom,
     &                ' salinity',                ! plot name
     &                'layer_salinity',           ! ncdf name
     &                'psu',                      ! units
     &                kf,kl,ltheta, frmt,ioin,fdate,verfhour)
        endif
c
c ---   -------------
c ---   density
c ---   -------------
c
c ---   'tthio ' = layer k density I/O unit (0 no I/O)
        call blkini(ioin,'tthio ')
        if (ioin.gt.0) then
          do k= kf,kl
          do j=1,jj
            do i=1,ii
              utilk(i,j,k)=th3d(i,j,2*k)
              if (mthin) then
                if (p(i,j,k)+onecm.gt.p(i,j,k+1)) then
                  utilk(i,j,k)=flag
                endif
              elseif (artype.ne.3) then
                if (p(i,j,k)+onecm.gt.p(i,j,kk+1)) then
                  utilk(i,j,k)=flag
                endif
              endif
            enddo
          enddo
          enddo
          call horout_3d(utilk, artype,yrflag,time3,iexpt,lhycom,
     &                ' density ',                ! plot name
     &                'layer_density',            ! ncdf name
     &                'sigma',                    ! units
     &                kf,kl,ltheta, frmt,ioin,fdate,verfhour)
        endif
c
c ---   --------------------------
c ---   layer kinetic energy
c ---   --------------------------
c
        if     (artype.gt.1) then  ! mean or std. archive
c ---     'keio  ' = kinetic energy I/O unit (0 no I/O)
          call blkini(ioin,'keio  ')
          if (ioin.gt.0) then
            do k= kf,kl
            do j=1,jj
              do i=1,ii
                utilk(i,j,k)=ke(i,j,2*k)
              enddo
            enddo
            enddo
            call horout_3d(utilk, artype,yrflag,time3,iexpt,lhycom,
     &                  ' ke/mass ',                  ! plot name
     &                  'layer_kinetic_energy/mass',  ! ncdf name
     &                  'm2/s2',                      ! units
     &                  kf,kl,ltheta, frmt,ioin,fdate,verfhour)
          endif
        endif  ! mean or std. archive
c
c ---   --------------------------
c ---   layer stream function
c ---   --------------------------
c
c ---   'sfnio ' = layer k strmfn. I/O unit (0 no I/O)
        call blkini(ioin,'sfnio ')
        if (ioin.gt.0) then
          do k= kf,kl
          do j=1,jj
            jm1=max(1,j-1)
            do i= 1,ii
              im1=max(1,i-1)
c
              util1(i,j)=1.
              if (iu(i,j).eq.1) then
                ubaro_(i,j)=u(i,j,2*k)*.5*(dp(i,j,k)+dp(im1,j,k))
              else
                ubaro_(i,j)=0.0
              endif
              if (iv(i,j).eq.1) then
                vbaro_(i,j)=v(i,j,2*k)*.5*(dp(i,j,k)+dp(i,jm1,k))
              else
                vbaro_(i,j)=0.0
              endif
              if (iq(i,j).eq.1) then
                vort(i,j)=
     &           (vbaro_(i,j)*scvy(i,j)-vbaro_(im1,j)*scvy(im1,j)
     &           -ubaro_(i,j)*scux(i,j)+ubaro_(i,jm1)*scux(i  ,jm1))
     &                *1.e-6
              else
                vort(i,j)=0.0
              endif
            enddo 
          enddo 
c
          call poisnd(ii,jj,vort,strmf,util1,util1,work)
c
          do j=1,jj
            do i=1,ii
              if (ip(i,j).eq.0) then
                strmf(i,j)=flag
              endif
            enddo
          enddo
c
          call qsmoo(strmf,work)
ccc       call zebra(strmf,ii,ii,jj)
ccc       write (*,'('' shown above: layer''i3'' stream function'')') k
          do j=1,jj
            do i=1,ii
              utilk(i,j,k) = strmf(i,j)
            enddo
          enddo
          enddo !k=kf,kl
c
          call horout(strmf, artype,yrflag,time3,iexpt,lhycom,
     &                '   strmf ',                ! plot name
     &                'layer_streamfunction',     ! ncdf name
     &                'Sv',                       ! units
     &                k,ltheta, frmt,ioin,fdate,verfhour)
        endif
c
      enddo  !layer loop

c new code for atmos vars
c --- 'stxio ' = 
      call blkini(ioin,'stxio ')
      if (ioin.gt.0) then
        call horout(stressx, artype,yrflag,time3,iexpt,lhycom,
     &              ' stressx ',               ! plot name
     &              'stress_x ',                ! ncdf name
     &              'unknown',                  ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c --- 'stxio ' = 
      call blkini(ioin,'styio ')
      if (ioin.gt.0) then
        call horout(stressy, artype,yrflag,time3,iexpt,lhycom,
     &              ' stressy ',               ! plot name
     &              'stress_y ',                ! ncdf name
     &              'unknown',                  ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c --- 'pcpio ' = 
      call blkini(ioin,'pcpio ')
      if (ioin.gt.0) then
        call horout(precip, artype,yrflag,time3,iexpt,lhycom,
     &              ' precip ',               ! plot name
     &              'precip ',                ! ncdf name
     &              'unknown',                  ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c --- 'rdfio ' = baro v-vel.  I/O unit (0 no I/O)
      call blkini(ioin,'rdfio ')
      if (ioin.gt.0) then
        call horout(rflux, artype,yrflag,time3,iexpt,lhycom,
     &              ' radflux ',               ! plot name
     &              'rad-flux ',                ! ncdf name
     &              'unknown',                  ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c --- 'swfio ' = baro v-vel.  I/O unit (0 no I/O)
      call blkini(ioin,'swfio ')
      if (ioin.gt.0) then
        call horout(swflux, artype,yrflag,time3,iexpt,lhycom,
     &              ' swrflux ',               ! plot name
     &              'swr-flux ',                ! ncdf name
     &              'unknown',                  ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c --- 'atpio ' = baro v-vel.  I/O unit (0 no I/O)
      call blkini(ioin,'atpio ')
      if (ioin.gt.0) then
        call horout(atmpres, artype,yrflag,time3,iexpt,lhycom,
     &              ' atmpres ',               ! plot name
     &              'atm-pres ',                ! ncdf name
     &              'unknown',                  ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c --- 'snfio ' = baro v-vel.  I/O unit (0 no I/O)
      call blkini(ioin,'snfio ')
      if (ioin.gt.0) then
        call horout(ssflux, artype,yrflag,time3,iexpt,lhycom,
     &              ' snsflux ',               ! plot name
     &              'sns-flux ',                ! ncdf name
     &              'unknown',                  ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c --- 'ltfio ' = baro v-vel.  I/O unit (0 no I/O)
      call blkini(ioin,'ltfio ')
      if (ioin.gt.0) then
        call horout(lflux, artype,yrflag,time3,iexpt,lhycom,
     &              ' latflux ',               ! plot name
     &              'lat-flux ',                ! ncdf name
     &              'unknown',                  ! units
     &              k,ltheta, frmt,ioin,fdate,verfhour)
      endif
c end atmos vars
c
c -- close unit if open
c
      print *, 'calling close_unit ',ioin,frmt
      call close_unit(ioin,frmt)

c
      stop '(normal)'
      end
