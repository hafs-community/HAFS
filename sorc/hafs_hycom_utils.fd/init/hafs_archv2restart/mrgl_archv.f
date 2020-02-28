      program mrgl_archv
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
c --- merge layers in a HYCOM 2.0 archive file.
c
      character label*81,text*18,flnm_i*240,flnm_o*240
      logical initl,trcout,icegln

      logical strict
c
      integer          artype,iexpt,iversn,yrflag,kpalet,mxlflg
      integer          flxflg
      integer          i,ibad,j,k,kb,kl,kt,kkin,kkout,l,laybot(0:99)
      real             sigma(99),thbase,dpoij,dpuij,dpvij,
     &                 dij,dpthin,onem,pij
      real             puij,uij,pvij,vij
      double precision time3(3),time,year
c
      real, allocatable :: dpo(:,:,:),dpu(:,:),dpv(:,:)
c
      data trcout/.false./  ! must be .false. (no tracer remapping)
      data initl /.true. /
      data strict/.true./
c

      call xcspmd
      call zaiost
      lp=6
c
      onem=9806.0
c
c
c --- 'flnm_i' = name of original archive file
c --- 'flnm_o' = name of target   archive file
c --- 'iexpt ' = experiment number x10  (000=from archive file)
c --- 'yrflag' = days in year flag (0=360J16,1=366J16,2=366J01,3=actual)
c --- 'idm   ' = longitudinal array size
c --- 'jdm   ' = latitudinal  array size
c --- 'kdmold' = original number of layers
c --- 'kdmnew' = target   number of layers
c
      read (*,'(a)') flnm_i
      write (lp,'(2a)') ' input file: ',flnm_i(1:len_trim(flnm_i))
      call flush(lp)
      read (*,'(a)') flnm_o
      write (lp,'(2a)') 'output file: ',flnm_o(1:len_trim(flnm_o))
      call flush(lp)
      call blkini(iexpt, 'iexpt ')
      call blkini(yrflag,'yrflag')
      call blkini(ii,    'idm   ')
      call blkini(jj,    'jdm   ')
      call blkini(kkin,  'kdmold')
      call blkini(kkout, 'kdmnew')
      call blkini(flxflg,  'flxflg')
      if     (ii.ne.idm .or. jj.ne.jdm) then
        write(lp,*)
        write(lp,*) 'error - wrong idm or jdm (should be:',
     &                                         idm,jdm,')'
        write(lp,*)
        call flush(lp)
        stop
      endif
      iorign = 1
      jorign = 1
c
c --- 'thbase' = reference density (sigma units)
c
      call blkinr(thbase,
     &           'thbase','("blkinr: ",a6," =",f11.4," sig")')
c
c --- new layer conbinations and densities (sigma units)
c
      write(lp,*)
      laybot(0) = 0
      do k=1,kkout
c
c ---   'laybot' = last layer in next combination of layers
c
        call blkini(laybot(k),  'laybot')
        if     (laybot(k).le.laybot(k-1)) then
          write(lp,'(a)') 'error - laybot must be ascending'
          stop
        elseif (k.eq.kkout .and. laybot(k).ne.kkin) then
          write(lp,'(a)') 'error - last laybot must be kkin'
          stop
        elseif (k.ne.kkout .and. laybot(k).ge.kkin) then
          write(lp,'(a)') 'error - laybot reached kkin too soon'
          stop
        endif
        if     (laybot(k).gt.laybot(k-1)+1) then
          call blkinr(sigma(k),
     &                'sigma ','("blkinr: ",a6," =",f11.4," sig")')
        else
          sigma(k) = -1.0  ! get from input archive
        endif
      enddo
c
c --- array allocation
c
      kk    = 0
      kkmax = max(kkin,kkout)
      call plot_alloc
c
      dpthfil = 'regional.depth'
c
      do j=1,jdm
        do i=1,idm
          p(i,j,1)=0.
        enddo
      enddo
c
c --- read the archive file, from "*.[ab]".
c
      kk = kkin
      call getdatb(flnm_i,time3,artype,initl,icegln,trcout,
     &             iexpt,iversn,yrflag,kkin,flxflg)        ! hycom input
      time = time3(3)
c
      if     (artype.eq.3) then
        write(lp,'(/ a /)')
     &    'error - cannot merge std.dev. archive'
        call flush(lp)
        stop
      endif
c
      do k= 1,kkout
        if     (sigma(k).lt.0.0) then
          sigma(k) = theta(laybot(k))
        endif
c
        if     (k.gt.1) then
          if      (sigma(k).le.sigma(k-1)) then
            write(lp,'(/ a,i3,2f11.4 /)')
     &        'error - sigma is not stabally stratified',
     &        k,sigma(k-1),sigma(k)
            call flush(lp)
            stop
          endif
        endif
      enddo
c
c --- land masks.
c
      call bigrid(depths)
c
c --- check that bathymetry is consistent with this archive.
c
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
        enddo
      enddo
      if     (ibad.ne.0) then
        write(lp,*)
        write(lp,*) 'error - wrong bathymetry for this archive file'
        write(lp,*) 'number of mismatches = ',ibad
        write(lp,*)
        call flush(lp)
        stop
      endif
c
c     remap layers.
c
c     fill massless layers on sea floor with fluid from above
c
      dpthin = 0.0001*onem
      do j= 1,jj
        do i= 1,ii
          if     (ip(i,j).eq.1) then
            dij = depths(i,j)*onem - dpthin
            pij = 0.0
            do k= 1,kkin
              pij = pij + dp(i,j,k)
              if     (pij.gt.dij .and. dp(i,j,k).le.dpthin) then
                temp(i,j,2*k) = temp(i,j,2*k-2)
                saln(i,j,2*k) = saln(i,j,2*k-2)
                th3d(i,j,2*k) = th3d(i,j,2*k-2)
              endif
            enddo !k
          endif !ip.eq.1
        enddo !i
      enddo !j
c
c
      allocate( dpo(idm,jdm,kkin), dpu(idm,jdm), dpv(idm,jdm) )
c
      dpo(:,:,1:kkin) = dp(:,:,1:kkin)
c
      do k= 1,kkout
        l  = 2*k-1
        kt = laybot(k-1)+1
        kb = laybot(k)
        write(lp,'(a,i3,i4,i3)') 'k,kt,kb = ',k,kt,kb
        call flush(lp)
c
        if     (kb.eq.kt) then
c
c ---     existing layer.
c
          do j= 1,jdm
            do i= 1,idm
                 u(i,j,l) =    u(i,j,2*kb)
                 v(i,j,l) =    v(i,j,2*kb)
                dp(i,j,k) =  dpo(i,j,  kb)
              temp(i,j,l) = temp(i,j,2*kb)
              saln(i,j,l) = saln(i,j,2*kb)
              th3d(i,j,l) = th3d(i,j,2*kb)
              if     (artype.eq.2) then
                ke(i,j,l) =   ke(i,j,2*kb)
              endif
            enddo
          enddo
        else
c
c ---     combination of layers.
c
          do j= 1,jdm
            do i= 1,idm
              if     (iu(i,j).eq.1) then
                 dpuij          = dpo(i,j,kt) + dpo(i+1,j,kt)
                 if(strict) then

                    dpuij=
     $                   max(0.5*(dpo(i-1,j,kt)+dpo(i,j,kt)),0.0)
                 endif
                 dpu(i,j)       = dpuij
                   u(i,j,l)     = dpuij*   u(i,j,2*kt)
              endif
              if     (iv(i,j).eq.1) then
                 dpvij          = dpo(i,j,kt) + dpo(i,j+1,kt)
                 if(strict) then

                    dpvij=
     $                   max(0.5*(dpo(i,j-1,kt)+dpo(i,j,kt)),0.0)
                 endif
                 dpv(i,j)       = dpvij
                   v(i,j,l)     = dpvij*   v(i,j,2*kt)
              endif
              if     (ip(i,j).eq.1) then
                 dpoij          = dpo(i,j,kt)
                  dp(i,j,k)     = dpoij
                temp(i,j,l)     = dpoij*temp(i,j,2*kt)
                saln(i,j,l)     = dpoij*saln(i,j,2*kt)
                th3d(i,j,l)     = dpoij*th3d(i,j,2*kt)
                if     (artype.eq.2) then
                  ke(i,j,l)     = dpoij*ke(  i,j,2*kt)
                endif
              endif
              do kl= kt+1,kb
                if     (iu(i,j).eq.1) then
                   dpuij          = dpo(i,j,kl) + dpo(i+1,j,kl)
                   if(strict) then
                      dpuij=
     $                     max(0.5*(dpo(i-1,j,kl)+dpo(i,j,kl)),0.0)
                   endif
                   dpu(i,j)       = dpuij                + dpu(i,j)
                     u(i,j,l)     = dpuij*   u(i,j,2*kl) +   u(i,j,l)
                endif
                if     (iv(i,j).eq.1) then
                   dpvij          = dpo(i,j,kl) + dpo(i,j+1,kl)
                   if(strict) then
                      dpvij=
     $                     max(0.5*(dpo(i,j-1,kl)+dpo(i,j,kl)),0.0)
                   endif
                   dpv(i,j)       = dpvij                + dpv(i,j)
                     v(i,j,l)     = dpvij*   v(i,j,2*kl) +   v(i,j,l)
                endif
                if     (ip(i,j).eq.1) then
                   dpoij          = dpo(i,j,kl)
                    dp(i,j,k)     = dpoij                +   dp(i,j,k)
                  temp(i,j,l)     = dpoij*temp(i,j,2*kl) + temp(i,j,l)
                  saln(i,j,l)     = dpoij*saln(i,j,2*kl) + saln(i,j,l)
                  th3d(i,j,l)     = dpoij*th3d(i,j,2*kl) + th3d(i,j,l)
                  if     (artype.eq.2) then
                    ke(i,j,l)     = dpoij*ke(  i,j,2*kl) +   ke(i,j,l)
                  endif
                endif
              enddo
              if     (iu(i,j).eq.1 .and. dpu(i,j).gt.0.0) then
                   u(i,j,l)     = u(i,j,l)/dpu(i,j)
              else  ! allow for land mask and zero thickness layer
                   u(i,j,l)     = u(i,j,2*kb)
              endif
              if     (iv(i,j).eq.1 .and. dpv(i,j).gt.0.0) then
                   v(i,j,l)     = u(i,j,l)/dpv(i,j)
              else  ! allow for land mask and zero thickness layer
                   v(i,j,l)     = v(i,j,2*kb)
              endif
              if     (ip(i,j).eq.1 .and. dp(i,j,k).gt.0.0) then
                temp(i,j,l)     = temp(i,j,l)/dp(i,j,k)
                saln(i,j,l)     = saln(i,j,l)/dp(i,j,k)
                th3d(i,j,l)     = th3d(i,j,l)/dp(i,j,k)
              else  ! allow for land mask and zero thickness layer
                temp(i,j,l)     = temp(i,j,2*kb)
                saln(i,j,l)     = saln(i,j,2*kb)
                th3d(i,j,l)     = th3d(i,j,2*kb)
              endif
            enddo
          enddo
        endif
      enddo  ! k=1,kkout
c
      theta(1:kkout) = sigma(1:kkout)

      if(strict) then
c
c -- internal velocities average to zero
c
         do j= 1,jdm
            do i= 1,idm
               puij=0.0
               uij=0.0
               pvij=0.0
               vij=0.0
               do k=1,kkout
                  if     (iu(i,j).eq.1) then
                     dpuij=
     $                    max(0.5*(dp(i-1,j,k)+dp(i,j,k)),0.0)
                     uij     = dpuij* u(i,j,k)+uij
                     puij=puij+dpuij
                  endif
                  if     (iv(i,j).eq.1) then
                     dpvij=
     $                    max(0.5*(dp(i,j-1,k)+dp(i,j,k)),0.0)
                     vij     = dpvij*   v(i,j,k)+vij
                     pvij=pvij+dpvij
                  endif
               enddo
               if     (iu(i,j).eq.1) then
                  uij=uij/puij
                  do k=1,kkout
                     u(i,j,k)=u(i,j,k)-uij
                  enddo
               endif
               if     (iv(i,j).eq.1) then
                  vij=vij/pvij
                  do k=1,kkout
                     v(i,j,k)=v(i,j,k)-vij
                  enddo
               endif
            enddo
         enddo

      endif
c
c --- write the archive file.
c
      l = len_trim(flnm_o)
      if     (flnm_o(l-1:l).eq.'.a' .or. flnm_o(l-1:l).eq.'.b') then
        flnm_o(l-1:l) = '  '  ! to prevent putdat from using '*.[AB]'
      endif
      kk = kkout
      call putdat(flnm_o,artype,time3,icegln,trcout,
     &            iexpt,iversn,yrflag,kkout,-1,flxflg)
      end
