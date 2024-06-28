      subroutine getartype(flnm,artype)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      character        flnm*(*)
      integer          artype
c
c --- read artype from archive file.
c ---     artype== 1 for normal archive files
c ---     artype==-1 for normal archive files with p-vel
c ---     artype== 2 for   mean archive files
c ---     artype==-2 for   mean archive files with p-vel
c ---     artype== 3 for stddev archive files
c ---     artype== 4 for   diff archive files
c
      character cline*80
      integer   l,ni
c
      data ni/14/
c
      l = len_trim(flnm)
c
      if     (flnm(l-1:l).eq.'.a' .or. flnm(l-1:l).eq.'.b') then
        open (unit=ni,file=flnm(1:l-2)//'.b',form='formatted',
     &        status='old',action='read')
        read( ni,'(a80/a80/a80/a80)') ctitle
        read( ni,'(a)') cline  ! iversn
        read( ni,'(a)') cline  ! iexpt
        read( ni,'(a)') cline  ! yrflag
        read( ni,'(a)') cline  ! idm
        read( ni,'(a)') cline  ! jdm
c
        read( ni,'(a)') cline
        if     (cline(25:28).eq.'mean') then
          artype = 2
        elseif (cline(25:28).eq.'std.') then
          artype = 3
        elseif (cline(25:28).eq.'diff') then
          artype = 4
        else
          artype = 1
        endif
        if     (cline(1:7).eq.'field_p') then
          artype = -artype
        endif
        close(ni)
      else
        artype  = 1
      endif
      return
      end

      subroutine getdat(flnm,time,artype,initl,lsteric,icegln,trcout,
     &                  iexpt,iversn,yrflag,kkin)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      character        flnm*(*)
      double precision time(3)
      logical          initl,lsteric,icegln,trcout
      integer          artype,iexpt,iversn,yrflag,kkin
c
c --- read model fields and extract portion of global fields.
c
      integer l
c
      real,      allocatable :: work(:,:)
      character*240          :: cfile
c
      allocate( work(idm,jdm)    )
c
      l = len_trim(flnm)
c
      if     (flnm(l-1:l).eq.'.a' .or. flnm(l-1:l).eq.'.b') then
c ---   HYCOM 2.0 array I/O archive file.
        cfile = ' '
        call getenv('ARCHVS',cfile)
        if     (cfile.eq.' ') then
c ----    standard archive
          call getdata( flnm,time,artype,.true.,
     &                  initl,lsteric,icegln,
     &                  iexpt,iversn,yrflag,kkin,      work)
        else
c ----    partial surface archive
          call getdata1(flnm,time,artype,.true.,
     &                  initl,lsteric,icegln,
     &                  iexpt,iversn,yrflag,kkin,      work)
        endif
      elseif (flnm(l-3:l).eq.'.txt') then
c ---   HYCOM 2.0 text profile file
        call getdatt(flnm,time(1),
     &               initl,
     &               iexpt,iversn,yrflag,kkin, work)
        lsteric = .false.
        icegln  = .false.
        trcout  = .false.
        time(2) = time(1)
        time(3) = time(1)
        artype  = 1
        sigver  = 0
      else
        write(lp,*)
        write(lp,*) "error in getdatb - not an .[ab] file ",
     &              trim(flnm)
        write(lp,*)
        call flush(lp)
        stop
      endif
      deallocate( work )
      return
      end

      subroutine getdats(flnm,time,artype,initl,lsteric,icegln,trcout,
     &                   iexpt,iversn,yrflag,kkin)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      character        flnm*(*)
      double precision time(3)
      logical          initl,lsteric,icegln,trcout
      integer          artype,iexpt,iversn,yrflag,kkin
c
c --- read model fields and extract portion of global fields.
c --- spectial version for HYCOM std input and "3z" output.
c
      integer l
c
      real,      allocatable :: work(:,:)
c
      allocate( work(idm,jdm)    )
c
      l = len_trim(flnm)
c
      if     (flnm(l-1:l).eq.'.a' .or. flnm(l-1:l).eq.'.b') then
c ---   HYCOM 2.0 array I/O archive file.
        call getdatas(flnm,time,artype,.true.,
     &                initl,lsteric,icegln,
     &                iexpt,iversn,yrflag,kkin,      work)
        sigver  = 0
      else
        write(lp,*)
        write(lp,*) "error in getdatb - not an .[ab] file ",
     &              trim(flnm)
        write(lp,*)
        call flush(lp)
        stop
      endif
      deallocate( work )
      return
      end

      subroutine getdatb(flnm,time,artype,initl,lsteric,icegln,trcout,
     &                   iexpt,iversn,yrflag,kkin)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      character        flnm*(*)
      double precision time(3)
      logical          initl,lsteric,icegln,trcout
      integer          artype,iexpt,iversn,yrflag,kkin
c
c --- read model fields and extract portion of global fields.
c --- ignore ".a" vs ".b" range mismatches.
c
      integer   l
      real,      allocatable :: work(:,:)
      character*240          :: cfile
c
      allocate( work(idm,jdm)    )
c
      l = len_trim(flnm)
c
      if     (flnm(l-1:l).eq.'.a' .or. flnm(l-1:l).eq.'.b') then
c ---   HYCOM 2.0 array I/O archive file.
        cfile = ' '
        call getenv('ARCHVS',cfile)
        if     (cfile.eq.' ') then
c ----    standard archive
          call getdata( flnm,time,artype,.false.,
     &                  initl,lsteric,icegln,
     &                  iexpt,iversn,yrflag,kkin,      work)
        else
c ----    partial surface archive
          call getdata1(flnm,time,artype,.false.,
     &                  initl,lsteric,icegln,
     &                  iexpt,iversn,yrflag,kkin,      work)
        endif
      else
        write(lp,*)
        write(lp,*) "error in getdatb - not an .[ab] file ",
     &              trim(flnm)
        write(lp,*)
        call flush(lp)
        stop
      endif
      deallocate( work )
      return
      end

      subroutine getdata(flnm,time,artype,lrange,initl,lsteric,icegln,
     &                   iexpt,iversn,yrflag,kkin, work)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
c
      character        flnm*(*)
      double precision time(3)
      logical          lrange,initl,lsteric,icegln
      integer          artype,iexpt,iversn,yrflag,kkin
      real             work(idm,jdm)
c
c --- read model fields and extract portion of global fields.
c --- HYCOM 2.0 and 2.2 array I/O archive file.
c --- (no time-averaged fluxes in this version)
c
      character cline*80
      character preambl(5)*79
      character cvarin*6
      real      hminb,hmaxb
      integer   i,j,ios,l,k,ktr,ntr
      logical   nodens,lmix,lke
      double precision timedum
c
      data ni/14/
c
      l = len_trim(flnm)
      open (unit=ni,file=flnm(1:l-2)//'.b',form='formatted',
     &      status='old',action='read')
      call zaiopf(flnm(1:l-2)//'.a','old', ni)
c
      read( ni,'(a80/a80/a80/a80)') ctitle
      write(lp,'(a80/a80/a80/a80)') ctitle
      read( ni,*) iversn,cvarin
      write(lp,*) cvarin,' = ',iversn
      if (cvarin.ne.'iversn') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                          ' but should be iversn'
        write(lp,*)
        call flush(lp)
        stop
      endif
      read( ni,*) iexpt,cvarin
      write(lp,*) cvarin,' = ',iexpt
      if (cvarin.eq.'jexpt ') then
c ---   skip jexpt, artype of 4 set below
        read( ni,*) iexpt,cvarin
        write(lp,*) cvarin,' = ',iexpt
      endif
      if (cvarin.ne.'iexpt ') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be iexpt '
        write(lp,*)
        call flush(lp)
        stop
      endif
      read( ni,*) yrflag,cvarin
      write(lp,*) cvarin,' = ',yrflag
      if (cvarin.ne.'yrflag') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be yrflag'
        write(lp,*)
        call flush(lp)
        stop
      endif
      read( ni,*) idmtst,cvarin
      write(lp,*) cvarin,' = ',idmtst
      if (cvarin.ne.'idm   ') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be idm   '
        write(lp,*)
        call flush(lp)
        stop
      endif
      read( ni,*) jdmtst,cvarin
      write(lp,*) cvarin,' = ',jdmtst
      if (cvarin.ne.'jdm   ') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be jdm   '
        write(lp,*)
        call flush(lp)
        stop
      endif
c
      if (idmtst.ne.idm .or. jdmtst.ne.jdm) then
        write(lp,*)
        write(lp,*) 'error in getdat - input idm,jdm',
     &                        ' not consistent with parameters'
        write(lp,*) 'idm,jdm = ',idm,   jdm,   '  (REGION.h)'
        write(lp,*) 'idm,jdm = ',idmtst,jdmtst,'  (input)'
        write(lp,*)
        call flush(lp)
        stop
      endif
c
c --- artype== 1 for normal archive files
c --- artype==-1 for normal archive files with p-vel
c --- artype== 2 for   mean archive files
c --- artype==-2 for   mean archive files with p-vel
c --- artype== 3 for stddev archive files
c --- artype== 4 for   diff archive files
c
      read( ni,'(a)') cline
      write(lp,'(a)') cline(1:len_trim(cline))
      if     (cline(25:28).eq.'mean') then
        artype = 2
      elseif (cline(25:28).eq.'std.') then
        artype = 3
      elseif (cline(25:28).eq.'diff') then
        artype = 4
      else
        artype = 1
      endif
      if     (cline(1:7).eq.'field_p') then
        artype = -artype
      endif
      write(lp,'(a,i2)') 'artype =',artype
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(1),layer,thet,hminb,hmaxb
      nodens = layer.ne.0  !detect version 2.2 normal archive files
      if     (nodens) then
        sigver = layer
        thbase = thet
      else
        sigver = 0
      endif
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &              montg,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'montg   '
* --- discard montg
*     write(lp,'("input  ",a," into ",a)') cline(1:8),'work    '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(2),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &              srfht,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'srfht   '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      lsteric = cline(1:8).eq.'steric  '
      if     (lsteric) then
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                steric,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'steric  '
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      else
        steric(:,:) = 0.0
      endif
      loneta  = cline(1:8).eq.'oneta   '
      if     (loneta) then
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                oneta,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'oneta   '
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        if     (abs(artype).ge.3) then  !std or dif
          if     (.not. allocated(onetas)) then
            allocate( onetas(ii,jj) )
          endif
          onetas(:,:) = oneta(:,:)
          write(lp,'("copy   ",a," into ",a)') 'oneta   ','onetas  '
          call extrct_p(work,idm,jdm,iorign,jorign, 
     &                  oneta,ii,jj)
          write(lp,'("input  ",a," into ",a)') cline(1:8),'oneta   '
          read (ni,'(a)',end=6) cline
          write(lp,'(a)')       cline(1:len_trim(cline))
          i = index(cline,'=')
          read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
          call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        endif  !std or dif
      else
        oneta(:,:) = 1.0  !not used?
      endif
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              surflx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'surflx  '
c
      call time_hour(time)  !reset, assuming time is on the hour
      write(lp,*) 'time3 = ',time
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      if     (cline(1:8).ne.'wtrflx  ') then
        wtrflx(:,:) = 0.0  !updated later
      else
        lwtrflx = .true.  !output wrtflx if input archive contains it
        call extrct_p(work,idm,jdm,iorign,jorign,
     &                wtrflx,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'wtrflx  '
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      endif
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              salflx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'salflx  '
c
      surtx(:,:) = 0.0
      surty(:,:) = 0.0
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              dpbl,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'dpbl    '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              dpmixl,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'dpmixl  '
c
c --- are there mix fields?
      read (ni,'(a)',iostat=ios) cline
      write(lp,'(a)')            cline(1:len_trim(cline))
      lmix = ios.eq.0 .and. cline(1:8).eq.'tmix    '
      if     (lmix) then
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                tmix,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'tmix    '
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                smix,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'smix    '
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                thmix,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'thmix   '
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
        call extrct_u(work,idm,jdm,iorign,jorign, 
     &                umix,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'umix    '
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
        call extrct_v(work,idm,jdm,iorign,jorign, 
     &                vmix,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'vmix    '
c
        read (ni,'(a)',iostat=ios) cline
        write(lp,'(a)')            cline(1:len_trim(cline))
      endif !lmix:else
c
c --- is there ke?
      lke = artype.gt.1 .and. ios.eq.0 .and. cline(1:8).eq.'kemix   '
      if     (lke) then  ! mean or std. archive with ke
        if     (.not. allocated(kemix)) then
          allocate(  kemix(ii,jj) )
        endif
        if     (.not. allocated(kebaro)) then
          allocate( kebaro(ii,jj) )
        endif
        if     (.not. allocated(ke)) then
          allocate( ke(ii,jj,kkmax) )
        endif
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                kemix,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'kemix   '
        read (ni,'(a)',iostat=ios) cline
        write(lp,'(a)')            cline(1:len_trim(cline))
      endif
c
c --- is there ice?
*     read (ni,'(a)',iostat=ios) cline
*     write(lp,'(a)')            cline(1:len_trim(cline))
      icegln = ios.eq.0 .and. cline(1:8).eq.'covice  '
      if     (icegln) then
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                covice,ii,jj)
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
      else
        covice(:,:) = 0.0
      endif

      icegln = ios.eq.0 .and. cline(1:8).eq.'thkice  '
      if     (icegln) then
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                thkice,ii,jj)
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
      else
        thkice(:,:) = 0.0
      endif

      icegln = ios.eq.0 .and. cline(1:8).eq.'temice  '
      if     (icegln) then
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                temice,ii,jj)
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
      else
        temice(:,:) = 0.0
      endif
c
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      if     (artype.ge.0) then
        call extrct_u(work,idm,jdm,iorign,jorign, 
     &                ubaro,ii,jj)
      else
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                ubaro,ii,jj)
      endif !artype>0:else
      write(lp,'("input  ",a," into ",a)') cline(1:8),'ubaro   '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      if     (artype.ge.0) then
        call extrct_v(work,idm,jdm,iorign,jorign, 
     &                vbaro,ii,jj)
      else
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                vbaro,ii,jj)
      endif !artype>0:else
      write(lp,'("input  ",a," into ",a)') cline(1:8),'vbaro   '
c
      if     (lke) then  ! mean or std. archive with ke
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                kebaro,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'kebaro  '
      endif
c
      kkin=1
      do 14 k=1,kk
      if     (k.eq.2) then
c ---   already input at end of k=1 loop.
      else
        read (ni,'(a)',end=6)   cline
        write(lp,'(a)')         cline(1:len_trim(cline))
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      if     (artype.ge.0) then
        if     (cline(1:8).ne.'u-vel.  ') then
          write(lp,*)
          write(lp,*) 'error in getdat - layer ',k,
     &               ' does not exist (kk= ',kk,')'
          write(lp,*)
          call flush(lp)
          stop
        endif
      else
        if     (cline(1:8).ne.'up-vel. ') then
          write(lp,*)
          write(lp,*) 'error in getdat - layer ',k,
     &               ' does not exist (kk= ',kk,')'
          write(lp,*)
          call flush(lp)
          stop
        endif
      endif
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      if     (artype.ge.0) then
        call extrct_u(work,idm,jdm,iorign,jorign, 
     &                u(1,1,k),ii,jj)
      else
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                u(1,1,k),ii,jj)
      endif !artype>0:else
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'u       ',k
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      if     (artype.ge.0) then
        call extrct_v(work,idm,jdm,iorign,jorign, 
     &                v(1,1,k),ii,jj)
      else
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                v(1,1,k),ii,jj)
      endif !artype>0:else
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'v       ',k
c
      if     (lke) then  ! mean or std. archive with ke
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                ke(1,1,k),ii,jj)
        write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'ke      ',k
      endif
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true., lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              dp(1,1,k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'dp      ',k
c
      if     (cline(1:8).eq.'mnthknss') then
c ---   std. thickness. is next, put in dpsd
        if     (.not. allocated(dpsd)) then
          allocate( dpsd(ii,jj,kkmax) )
        endif
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .true., lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                dpsd(1,1,k),ii,jj)
        write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'dpsd    ',k
      endif
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              temp(1,1,k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'temp    ',k
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              saln(1,1,k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'saln    ',k
c
      if     (k.eq.1 .and. .not.lwtrflx) then
        do j= 1,jj
          do i= 1,ii
            if     (saln(i,j,k).lt.2.0**99) then
              wtrflx(i,j) = -salflx(i,j)/max(saln(i,j,k),0.001)
            endif
          enddo
        enddo
        write(lp,'("conv.  ",a," into ",a)') 'salflx  ','wtrflx  '
      endif !not lwtrflx
c
      if     (.not. nodens) then
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                th3d(1,1,k),ii,jj)
        write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'th3d    ',k
      else
        call th3d_p(temp(1,1,k),saln(1,1,k),
     &              th3d(1,1,k),ii,jj, sigver,thbase)
        write(lp,'("    ",a8,"calculate ",a,i3)') " ",'th3d    ',k
        if     (k.eq.1) then
           tmix(:,:) = temp(:,:,1)
           smix(:,:) = saln(:,:,1)
          thmix(:,:) = th3d(:,:,1)
           umix(:,:) =    u(:,:,1)
           vmix(:,:) =    v(:,:,1)
          write(lp,'("copy   ",a," into ",a)') 'temp.1  ','tmix    '
          write(lp,'("copy   ",a," into ",a)') 'saln.1  ','smix    '
          write(lp,'("copy   ",a," into ",a)') 'th3d.1  ','thmix   '
          write(lp,'("copy   ",a," into ",a)') '   u.1  ','umix    '
          write(lp,'("copy   ",a," into ",a)') '   v.1  ','vmix    '
        endif !k==1
      endif !.not.nodens:else
c
c --- tracers and visc/diff, may be more in archive than processed.
c --- tracers can be labeled 'tracer  ' or 'tracer01','tracer02',...
c
      if     (k.eq.1) then
        do ktr= 1,999
          read (ni,'(a)',iostat=ios) cline
          write(lp,'(a)')            cline(1:len_trim(cline))
          if (ios.ne.0) then
            write(lp,'(a,f9.5)') 'finished reading data for layer',thet
            call flush(lp)
            theta(k)=thet
            goto 114  ! archive containing only 1 layer
          elseif (cline(1:6).ne.'tracer'   .and.
     &            cline(1:8).ne.'viscty  ' .and.
     &            cline(1:8).ne.'t-diff  ' .and.
     &            cline(1:8).ne.'s-diff  '      ) then
            exit !end of tracers and visc/diff
          else
            i = index(cline,'=')
            read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
            call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
            if     (ktr.le.ntracr) then
              call extrct_p(work,idm,jdm,iorign,jorign,
     &                      trcr(1,1,k,ktr),ii,jj)
              write(lp,'("input  ",a," into ",a,2i3)')
     &          cline(1:8),'trcr    ',k,ktr
              if     (cline(1:6).eq.'tracer') then
                itrcr_type = 0
              elseif (cline(1:8).eq.'viscty  ') then
                itrcr_type = 1
              elseif (cline(1:8).eq.'t-diff  ') then
                itrcr_type = 2
              elseif (cline(1:8).eq.'s-diff  ') then
                itrcr_type = 3
              endif
            endif
          endif
        enddo !ktr
        ntr=ktr-1
        if     (ntracr.gt.ntr) then
          write(lp,*)
          write(lp,*) 'error in getdat - fewer tracers than requested'
          write(lp,*)
          call flush(lp)
          stop
        endif
      else !k.gt.1
        do ktr= 1,ntr
          read (ni,'(a)',end=6) cline
          write(lp,'(a)')       cline(1:len_trim(cline))
          i = index(cline,'=')
          read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
          call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
          if     (ktr.le.ntracr) then
            call extrct_p(work,idm,jdm,iorign,jorign,
     &                    trcr(1,1,k,ktr),ii,jj)
            write(lp,'("input  ",a," into ",a,2i3)')
     &        cline(1:8),'trcr    ',k,ktr
          endif
        enddo !ktr
      endif !tracers+visc/diff
c
ccc      write(lp,'(a,i4)') 'shown below: density in layer',k
ccc      call zebra(th3d(1,1,k),ii,ii1,jj1)
c
      write(lp,'(a,f9.5)') 'finished reading data for layer',thet
      call flush(lp)
      theta(k)=thet
 14   continue
      kkin=kk
114   continue
c
      close( unit=ni)
      call zaiocl(ni)
      write(lp,'(a)') 'closed archive file'
      call flush(lp)
c
      if (initl) then
c ---   acquire basin depths and land/sea mask
        call getdepth(work)
      end if
c
*     write(lp,'(a)') 'shown below: sea surface height'
*     call zebra(srfht,ii,ii1,jj1)
*     call flush(lp)
c
      return
c
c --- unexpected end of file
 6    continue
      write (lp,*) '***** unexpected end of archive file *****'
      call flush(lp)
      stop '(e-o-f)'
      end

      subroutine getdata1(flnm,time,artype,lrange,initl,lsteric,icegln,
     &                    iexpt,iversn,yrflag,kkin, work)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
c
      character        flnm*(*)
      double precision time(3)
      logical          lrange,initl,lsteric,icegln
      integer          artype,iexpt,iversn,yrflag,kkin
      real             work(idm,jdm)
c
c --- read model fields and extract portion of global fields.
c --- HYCOM 2.0 and 2.2 array I/O archive file.
c --- (no time-averaged fluxes in this version)
c --- version for partial surface archives
c
      character cline*80
      character preambl(5)*79
      character cvarin*6
      real      hminb,hmaxb
      logical   nodens
      integer   i,j,ios,l,k,ktr,ntr,nvar
c
      integer, parameter :: nfields=20  !no. fields in surface archive
      logical            :: l_arch(nfields) !field output flags
      character*6        :: c_arch(nfields) !field names (archs.input)
      character*8        :: c_arc8(nfields) !field names (archive file)
      character*240      :: cfile
c
      data ni/14/
c
c ---   list of field names
c
      c_arc8( 1) = 'montg1  '
      c_arc8( 2) = 'srfhgt  '
      c_arc8( 3) = 'steric  '
      c_arc8( 4) = 'surflx  '
      c_arc8( 5) = 'wtrflx  '  !used to be 'salflx  '
      c_arc8( 6) = 'bl_dpth '
      c_arc8( 7) = 'mix_dpth'
      c_arc8( 8) = 'covice  '
      c_arc8( 9) = 'thkice  '
      c_arc8(10) = 'temice  '
      c_arc8(11) = 'u_btrop '
      c_arc8(12) = 'v_btrop '
      c_arc8(13) = 'u-vel.  '
      c_arc8(14) = 'v-vel.  '
      c_arc8(15) = 'thknss  '
      c_arc8(16) = 'temp    '
      c_arc8(17) = 'salin   '
      c_arc8(18) = 'salflx  '  !after wtrflx
      c_arc8(19) = 'surtx   '
      c_arc8(20) = 'surty   '
c
c ---   list of field names, 6 character versions of 8 character names
c
      do k= 1,nfields
        c_arch(k) = c_arc8(k)(1:6)
      enddo
      c_arch( 6) = 'bldpth'
      c_arch( 7) = 'mldpth'
      c_arch(11) = 'ubtrop'
      c_arch(12) = 'vbtrop'
c
c ---   read in archvs.input.
c
      call getenv('ARCHVS',cfile)
      open(unit=99,file=cfile)
      do k= 1,4
        call blkinl_99(l_arch(k),c_arch(k))
      enddo
c --- wtrflx or salflx?
      call blkinl2_99(l_arch(5),nvar,c_arch(5),c_arch(18))
      if     (nvar.eq.1) then  !latest version, 20 fields
        do k= 6,nfields
          call blkinl_99(l_arch(k),c_arch(k))
        enddo
      else  !old version, no wtrflx and so only 19 fields
        do k= 6,17
          call blkinl_99(l_arch(k),c_arch(k))
        enddo
        l_arch(18) = l_arch(5)
        l_arch( 5) = .false.  !no wtrflx
        do k= 19,20
          call blkinl_99(l_arch(k),c_arch(k))
        enddo
      endif
      close (unit=99)
c
      l = len_trim(flnm)
      open (unit=ni,file=flnm(1:l-2)//'.b',form='formatted',
     &      status='old',action='read')
      call zaiopf(flnm(1:l-2)//'.a','old', ni)
c
      read( ni,'(a80/a80/a80/a80)') ctitle
      write(lp,'(a80/a80/a80/a80)') ctitle
      read( ni,*) iversn,cvarin
      write(lp,*) cvarin,' = ',iversn
      if (cvarin.ne.'iversn') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                          ' but should be iversn'
        write(lp,*)
        call flush(lp)
        stop
      endif
      read( ni,*) iexpt,cvarin
      write(lp,*) cvarin,' = ',iexpt
      if (cvarin.ne.'iexpt ') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be iexpt '
        write(lp,*)
        call flush(lp)
        stop
      endif
      read( ni,*) yrflag,cvarin
      write(lp,*) cvarin,' = ',yrflag
      if (cvarin.ne.'yrflag') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be yrflag'
        write(lp,*)
        call flush(lp)
        stop
      endif
      read( ni,*) idmtst,cvarin
      write(lp,*) cvarin,' = ',idmtst
      if (cvarin.ne.'idm   ') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be idm   '
        write(lp,*)
        call flush(lp)
        stop
      endif
      read( ni,*) jdmtst,cvarin
      write(lp,*) cvarin,' = ',jdmtst
      if (cvarin.ne.'jdm   ') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be jdm   '
        write(lp,*)
        call flush(lp)
        stop
      endif
c
      if (idmtst.ne.idm .or. jdmtst.ne.jdm) then
        write(lp,*)
        write(lp,*) 'error in getdat - input idm,jdm',
     &                        ' not consistent with parameters'
        write(lp,*) 'idm,jdm = ',idm,   jdm,   '  (REGION.h)'
        write(lp,*) 'idm,jdm = ',idmtst,jdmtst,'  (input)'
        write(lp,*)
        call flush(lp)
        stop
      endif
c
c --- artype== 1 for normal archive files
c --- artype==-1 for normal archive files with p-vel
c --- artype== 2 for   mean archive files
c --- artype==-2 for   mean archive files with p-vel
c --- artype== 3 for stddev archive files
c --- artype== 4 for   diff archive files
c
      read( ni,'(a)') cline
      write(lp,'(a)') cline(1:len_trim(cline))
      if     (cline(25:28).eq.'mean') then
        artype = 2
      elseif (cline(25:28).eq.'std.') then
        artype = 3
      elseif (cline(25:28).eq.'diff') then
        artype = 4
      else
        artype = 1
      endif
      if     (cline(1:7).eq.'field_p') then
        artype = -artype
      endif
      write(lp,'(a,i2)') 'artype =',artype
c
      if (artype.ne.1) then
        write(lp,*)
        write(lp,*) 'error in getdata1 - artype must be 1'
        write(lp,*)
        call flush(lp)
        stop
      endif
c
      if     (.not.l_arch(1)) then
      nodens = .true.
      sigver = 4     !a guess
      thbase = 34.0  !a guess
      montg(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8( 1)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8( 1)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      nodens = layer.ne.0  !detect version 2.2 normal archive files
      if     (nodens) then
        sigver = layer
        thbase = thet
      else
        sigver = 0
      endif
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &              montg,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'montg   '
* --- discard montg
*     write(lp,'("input  ",a," into ",a)') cline(1:8),'work    '
      endif !l_arch
c
      if     (.not.l_arch(2)) then
      srfht(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8( 2)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8( 2)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &              srfht,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'srfht   '
      endif !l_arch
c
      if     (.not.l_arch(3)) then
      steric(:,:) = 0.0
      lsteric = .false.
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8( 3)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8( 3)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      lsteric = .true.
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              steric,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'steric  '
      endif !l_arch
c
      if     (.not.l_arch(4)) then
      surflx(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8( 4)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8( 4)
        call flush(lp)
        stop
      endif
      if     (cline(1:8).ne.c_arc8( 4)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8( 4)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              surflx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'surflx  '
      endif !l_arch
c
      if     (.not.l_arch(5)) then
      wtrflx(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8( 5)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8( 5)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              wtrflx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'wtrflx  '
      endif !l_arch
c
      if     (.not.l_arch(18)) then
      salflx(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8(18)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8( 5)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              salflx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'salflx  '
      endif !l_arch
c
      if     (.not.l_arch(19)) then
      surtx(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8(19)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8(19)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              surtx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'surtx  '
      endif !l_arch
c
      if     (.not.l_arch(20)) then
      surty(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8(20)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8(20)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              surty,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'surty  '
      endif !l_arch
c
      if     (.not.l_arch(6)) then
      dpbl(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8( 6)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8( 6)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              dpbl,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'dpbl    '
      endif !l_arch
c
      if     (.not.l_arch(7)) then
      dpmixl(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8( 7)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8( 7)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              dpmixl,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'dpmixl  '
      endif !l_arch
c
c --- is there ice?
      icegln = l_arch(8) .or. l_arch(9) .or. l_arch(10)
c
      if     (.not.l_arch(8)) then
      covice(:,:) = 0.0
      else
      read (ni,'(a)',iostat=ios) cline
      write(lp,'(a)')            cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8( 8)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8( 8)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              covice,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'covice  '
      endif !l_arch
c
      if     (.not.l_arch(9)) then
      thkice(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8( 9)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8( 9)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              thkice,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'thkice  '
      endif !l_arch
c
      if     (.not.l_arch(10)) then
      temice(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8(10)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8(10)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              temice,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'temice  '
      endif !l_arch
c
      if     (.not.l_arch(11)) then
      ubaro(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8(11)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8(11)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      call extrct_u(work,idm,jdm,iorign,jorign, 
     &              ubaro,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'ubaro   '
      endif !l_arch
c
      if     (.not.l_arch(12)) then
      vbaro(:,:) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8(12)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8(12)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      call extrct_v(work,idm,jdm,iorign,jorign, 
     &              vbaro,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'vbaro   '
      endif !l_arch
c
      kkin=1
      do 14 k=1,kkin
      if     (.not.l_arch(13)) then
      u(:,:,k) = 0.0
      else
      read (ni,'(a)',end=6)   cline
      write(lp,'(a)')         cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8(13)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8(13)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      call extrct_u(work,idm,jdm,iorign,jorign, 
     &              u(1,1,k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'u       ',k
      endif !l_arch
c
      if     (.not.l_arch(14)) then
      v(:,:,k) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8(14)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8(14)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      call extrct_v(work,idm,jdm,iorign,jorign, 
     &              v(1,1,k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'v       ',k
      endif !l_arch
c
      if     (.not.l_arch(15)) then
      dp(:,:,k) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8(15)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8(15)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true., lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              dp(1,1,k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'dp      ',k
      endif !l_arch
c
      if     (.not.l_arch(16)) then
      temp(:,:,k) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8(16)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8(16)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              temp(1,1,k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'temp    ',k
      endif !l_arch
c
      if     (.not.l_arch(17)) then
      saln(:,:,k) = 0.0
      else
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      if     (cline(1:8).ne.c_arc8(17)) then
        write(lp,'("error: input  ",a," but expected ",a)')
     &    cline(1:8),c_arc8(17)
        call flush(lp)
        stop
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              saln(1,1,k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'saln    ',k
c
      endif !l_arch
      if     (l_arch(16) .and. l_arch(17)) then
        call th3d_p(temp(1,1,k),saln(1,1,k),
     &              th3d(1,1,k),ii,jj, sigver,thbase)
        write(lp,'("    ",a8,"calculate ",a,i3)') " ",'th3d    ',k
      else
        th3d(:,:,k) = 0.0
      endif
c
      if     (k.eq.1) then
         tmix(:,:) = temp(:,:,1)
         smix(:,:) = saln(:,:,1)
        thmix(:,:) = th3d(:,:,1)
         umix(:,:) =    u(:,:,1)
         vmix(:,:) =    v(:,:,1)
        write(lp,'("copy   ",a," into ",a)') 'temp.1  ','tmix    '
        write(lp,'("copy   ",a," into ",a)') 'saln.1  ','smix    '
        write(lp,'("copy   ",a," into ",a)') 'th3d.1  ','thmix   '
        write(lp,'("copy   ",a," into ",a)') '   u.1  ','umix    '
        write(lp,'("copy   ",a," into ",a)') '   v.1  ','vmix    '
      endif !k==1
c
      write(lp,'(a,f9.5)') 'finished reading data for layer',thet
      call flush(lp)
      theta(k)=thet
 14   continue
c
      close( unit=ni)
      call zaiocl(ni)
      write(lp,'(a)') 'closed archive file'
      call flush(lp)
c
      time(1) = timedum
      time(2) = timedum
      time(3) = timedum
      call time_hour(time)  !reset, assuming time is on the hour
      write(lp,*) 'time3 = ',time
c
      if (initl) then
c ---   acquire basin depths and land/sea mask
        call getdepth(work)
      end if
c
*     write(lp,'(a)') 'shown below: sea surface height'
*     call zebra(srfht,ii,ii1,jj1)
*     call flush(lp)
c
      return
c
c --- unexpected end of file
 6    continue
      write (lp,*) '***** unexpected end of archive file *****'
      call flush(lp)
      stop '(e-o-f)'
      end

      subroutine getdatas(flnm,time,artype,lrange,initl,lsteric,icegln,
     &                    iexpt,iversn,yrflag,kkin, work)
      use mod_plot  ! HYCOM plot array interface
      use mod_za  ! HYCOM array I/O interface
c
      character        flnm*(*)
      double precision time(3)
      logical          lrange,initl,lsteric,icegln
      integer          artype,iexpt,iversn,yrflag,kkin
      real             work(idm,jdm)
c
c --- read model fields and extract portion of global fields.
c --- HYCOM 2.0 array I/O std archive file.
c --- for "3z", so keep mnthknss and discard thknss.
c --- (no time-averaged fluxes in this version).
c
      character cline*80
      character preambl(5)*79
      character cvarin*6
      real      hminb,hmaxb
      integer   i,j,ios,l,k,ktr,ntr
      logical   lke
      double precision timedum
c
      data ni/14/
c
      l = len_trim(flnm)
      open (unit=ni,file=flnm(1:l-2)//'.b',form='formatted',
     &      status='old',action='read')
      call zaiopf(flnm(1:l-2)//'.a','old', ni)
c
      read( ni,'(a80/a80/a80/a80)') ctitle
      write(lp,'(a80/a80/a80/a80)') ctitle
      read( ni,*) iversn,cvarin
      write(lp,*) cvarin,' = ',iversn
      if (cvarin.ne.'iversn') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                          ' but should be iversn'
        write(lp,*)
        call flush(lp)
        stop
      endif
      read( ni,*) iexpt,cvarin
      write(lp,*) cvarin,' = ',iexpt
      if (cvarin.ne.'iexpt ') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be iexpt '
        write(lp,*)
        call flush(lp)
        stop
      endif
      read( ni,*) yrflag,cvarin
      write(lp,*) cvarin,' = ',yrflag
      if (cvarin.ne.'yrflag') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be yrflag'
        write(lp,*)
        call flush(lp)
        stop
      endif
      read( ni,*) idmtst,cvarin
      write(lp,*) cvarin,' = ',idmtst
      if (cvarin.ne.'idm   ') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be idm   '
        write(lp,*)
        call flush(lp)
        stop
      endif
      read( ni,*) jdmtst,cvarin
      write(lp,*) cvarin,' = ',jdmtst
      if (cvarin.ne.'jdm   ') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be jdm   '
        write(lp,*)
        call flush(lp)
        stop
      endif
c
      if (idmtst.ne.idm .or. jdmtst.ne.jdm) then
        write(lp,*)
        write(lp,*) 'error in getdat - input idm,jdm',
     &                        ' not consistent with parameters'
        write(lp,*) 'idm,jdm = ',idm,   jdm,   '  (REGION.h)'
        write(lp,*) 'idm,jdm = ',idmtst,jdmtst,'  (input)'
        write(lp,*)
        call flush(lp)
        stop
      endif
c
c --- artype== 1 for normal archive files
c --- artype==-1 for normal archive files with p-vel
c --- artype== 2 for   mean archive files
c --- artype==-2 for   mean archive files with p-vel
c --- artype== 3 for stddev archive files
c --- artype== 4 for   diff archive files
c
      read( ni,'(a)') cline
      write(lp,'(a)') cline(1:len_trim(cline))
      if     (cline(25:28).eq.'mean') then
        artype = 2
      elseif (cline(25:28).eq.'std.') then
        artype = 3
      elseif (cline(25:28).eq.'diff') then
        artype = 4
      else
        artype = 1
      endif
      if     (cline(1:7).eq.'field_p') then
        artype = -artype
      endif
      write(lp,'(a,i2)') 'artype =',artype
c
      if (artype.ne.3) then
        write(lp,*)
        write(lp,*) 'error in getdatas - only for artype = 3'
        write(lp,*)
        call flush(lp)
        stop
      endif
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(1),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &              montg,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'montg   '
* --- discard montg
*     write(lp,'("input  ",a," into ",a)') cline(1:8),'work    '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(2),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &              srfht,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'srfht   '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      lsteric = cline(1:8).eq.'steric  '
      if     (lsteric) then
        call extrct_p(work,idm,jdm,iorign,jorign,
     &                steric,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'steric  '
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      else
        steric(:,:) = 0.0
      endif
      loneta  = cline(1:8).eq.'oneta   '
      if     (loneta) then
        call extrct_p(work,idm,jdm,iorign,jorign,
     &                oneta,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'oneta   '
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        if     (abs(artype).ge.3) then  !std or dif
          if     (.not. allocated(onetas)) then
            allocate( onetas(ii,jj) )
          endif
          onetas(:,:) = oneta(:,:)
          write(lp,'("copy   ",a," into ",a)') 'oneta   ','onetas  '
          call extrct_p(work,idm,jdm,iorign,jorign,
     &                  oneta,ii,jj)
          write(lp,'("input  ",a," into ",a)') cline(1:8),'oneta   '
          read (ni,'(a)',end=6) cline
          write(lp,'(a)')       cline(1:len_trim(cline))
          i = index(cline,'=')
          read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
          call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        endif  !std or dif
      else
        oneta(:,:) = 1.0  !not used?
      endif
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              surflx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'surflx  '
      call time_hour(time)  !reset, assuming time is on the hour
      write(lp,*) 'time3 = ',time
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      if     (cline(1:8).ne.'wtrflx  ') then
        wtrflx(:,:) = 0.0
      else
        lwtrflx = .true.
        call extrct_p(work,idm,jdm,iorign,jorign,
     &                wtrflx,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'wtrflx  '
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      endif
      call extrct_p(work,idm,jdm,iorign,jorign,
     &              salflx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'salflx  '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              dpbl,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'dpbl    '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              dpmixl,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'dpmixl  '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              tmix,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'tmix    '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              smix,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'smix    '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              thmix,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'thmix   '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      call extrct_u(work,idm,jdm,iorign,jorign, 
     &              umix,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'umix    '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      call extrct_v(work,idm,jdm,iorign,jorign, 
     &              vmix,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'vmix    '
c
c --- is there ke?
      read (ni,'(a)',iostat=ios) cline
      write(lp,'(a)')            cline(1:len_trim(cline))
      lke = artype.gt.1 .and. ios.eq.0 .and. cline(1:8).eq.'kemix   '
      if     (lke) then  ! mean or std. archive with ke
        if     (.not. allocated(kemix)) then
          allocate(  kemix(ii,jj) )
        endif
        if     (.not. allocated(kebaro)) then
          allocate( kebaro(ii,jj) )
        endif
        if     (.not. allocated(ke)) then
          allocate( ke(ii,jj,kkmax) )
        endif
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                kemix,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'kemix   '
        read (ni,'(a)',iostat=ios) cline
        write(lp,'(a)')            cline(1:len_trim(cline))
      endif
c
c --- is there ice?
*     read (ni,'(a)',iostat=ios) cline
*     write(lp,'(a)')            cline(1:len_trim(cline))
      icegln = ios.eq.0 .and. cline(1:8).eq.'covice  '
      if     (icegln) then
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                covice,ii,jj)
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
      else
        covice(:,:) = 0.0
      endif

      icegln = ios.eq.0 .and. cline(1:8).eq.'thkice  '
      if     (icegln) then
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                thkice,ii,jj)
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
      else
        thkice(:,:) = 0.0
      endif

      icegln = ios.eq.0 .and. cline(1:8).eq.'temice  '
      if     (icegln) then
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                temice,ii,jj)
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
      else
        temice(:,:) = 0.0
      endif
c
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      call extrct_u(work,idm,jdm,iorign,jorign, 
     &              ubaro,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'ubaro   '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      call extrct_v(work,idm,jdm,iorign,jorign, 
     &              vbaro,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'vbaro   '
c
      if     (lke) then  ! mean or std. archive with ke
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                kebaro,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'kebaro  '
      endif
c
      kkin=1
      do 14 k=1,kk
      if     (k.eq.2) then
c ---   already input at end of k=1 loop.
      else
        read (ni,'(a)',end=6)   cline
        write(lp,'(a)')         cline(1:len_trim(cline))
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      if     (cline(1:8).ne.'u-vel.  ') then
        write(lp,*)
        write(lp,*) 'error in getdat - layer ',k,
     &             ' does not exist (kk= ',kk,')'
        write(lp,*)
        call flush(lp)
        stop
      endif
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      call extrct_u(work,idm,jdm,iorign,jorign, 
     &              u(1,1,k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'u       ',k
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      call extrct_v(work,idm,jdm,iorign,jorign, 
     &              v(1,1,k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'v       ',k
c
      if     (lke) then  ! mean or std. archive with ke
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                ke(1,1,k),ii,jj)
        write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'ke      ',k
      endif
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true., lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              dp(1,1,k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'dp      ',k
      if     (cline(1:8).ne.'mnthknss') then
        write(lp,*)
        write(lp,*) 'error in getdat - expected mnthknss'
        write(lp,*)
        call flush(lp)
        stop
      endif
c --- discard std. thickness.
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true., lrange)
      write(lp,'("skip   ",a)') cline(1:8)
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              temp(1,1,k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'temp    ',k
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              saln(1,1,k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'saln    ',k
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              th3d(1,1,k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'th3d    ',k
c
c
c --- tracers and visc/diff, may be more in archive than plotted.
c
      if     (k.eq.1) then
        do ktr= 1,999
          read (ni,'(a)',iostat=ios) cline
          write(lp,'(a)')            cline(1:len_trim(cline))
          if (ios.ne.0) then
            write(lp,'(a,f9.5)') 'finished reading data for layer',thet
            call flush(lp)
            theta(k)=thet
            goto 114  ! archive containing only 1 layer
          elseif (cline(1:6).ne.'tracer'   .and.
     &            cline(1:8).ne.'viscty  ' .and.
     &            cline(1:8).ne.'t-diff  ' .and.
     &            cline(1:8).ne.'s-diff  '      ) then
            exit !end of tracers and visc/diff
          else
            i = index(cline,'=')
            read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
            call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
            if     (ktr.le.ntracr) then
              call extrct_p(work,idm,jdm,iorign,jorign,
     &                      trcr(1,1,k,ktr),ii,jj)
              write(lp,'("input  ",a," into ",a,2i3)')
     &          cline(1:8),'trcr    ',k,ktr
            endif
          endif
        enddo !ktr
        ntr=ktr-1
        if     (ntracr.gt.ntr) then
          write(lp,*)
          write(lp,*) 'error in getdat - fewer tracers than requested'
          write(lp,*)
          call flush(lp)
          stop
        endif
      else !k.gt.1
        do ktr= 1,ntr
          read (ni,'(a)',end=6) cline
          write(lp,'(a)')       cline(1:len_trim(cline))
          i = index(cline,'=')
          read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
          call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
          if     (ktr.le.ntracr) then
            call extrct_p(work,idm,jdm,iorign,jorign,
     &                    trcr(1,1,k,ktr),ii,jj)
            write(lp,'("input  ",a," into ",a,2i3)')
     &        cline(1:8),'trcr    ',k,ktr
          endif
        enddo !ktr
      endif !tracers+visc/diff
c
ccc      write(lp,'(a,i4)') 'shown below: density in layer',k
ccc      call zebra(th3d(1,1,k),ii,ii1,jj1)
c
      write(lp,'(a,f9.5)') 'finished reading data for layer',thet
      call flush(lp)
      theta(k)=thet
 14   continue
      kkin=kk
114   continue
c
      close( unit=ni)
      call zaiocl(ni)
      write(lp,'(a)') 'closed archive file'
      call flush(lp)
c
      if (initl) then
c ---   acquire basin depths and land/sea mask
        call getdepth(work)
      end if
c
*     write(lp,'(a)') 'shown below: sea surface height'
*     call zebra(srfht,ii,ii1,jj1)
*     call flush(lp)
c
      return
c
c --- unexpected end of file
 6    continue
      write (lp,*) '***** unexpected end of archive file *****'
      call flush(lp)
      stop '(e-o-f)'
      end

      subroutine getdatt(flnm,time,initl,
     &                   iexpt,iversn,yrflag,kkin, work)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
c
      character        flnm*(*)
      double precision time
      logical          initl
      integer          artype,iexpt,iversn,yrflag,kkin
      real             work(idm,jdm)
c
c --- read model fields and extract portion of global fields.
c --- HYCOM 2.0 and 2.2 text profile archive file.
c
      character cline*240
      character preambl(5)*79
      character cvarin*6
      real      hminb,hmaxb,dum
      integer   i,idum,j,ios,l,k,ktr,ntr
      logical   lke
      double precision timedum
c
      data ni/14/
c
      write(6,*) 'flnm = ',flnm
      open (unit=ni,file=flnm,form='formatted',
     &      status='old',action='read')
c
c --- header
c
      read( ni,'(a)') cline
      read( ni,'(a)') cline
      read(cline(3:),*) iexpt,idmtst,jdmtst,
     &                  idum,idum,idum,dum,dum,
     &                  yrflag
c
      if (idmtst.ne.idm .or. jdmtst.ne.jdm) then
        write(lp,*)
        write(lp,*) 'error in getdat - input idm,jdm',
     &                        ' not consistent with parameters'
        write(lp,*) 'idm,jdm = ',idm,   jdm,   '  (REGION.h)'
        write(lp,*) 'idm,jdm = ',idmtst,jdmtst,'  (input)'
        write(lp,*)
        call flush(lp)
        stop
      endif
c
      read( ni,'(a)') cline
      read( ni,'(a)') cline
      read(cline(2:),*) time, srfht(1,1),dum,dum,
     &                       surflx(1,1),dum,
     &                       salflx(1,1),dum,dum,dum,dum,dum,dum,
     &                         dpbl(1,1),
     &                       dpmixl(1,1),dum,dum,dum,dum,dum,dum,dum,
     &                        ubaro(1,1),
     &                        vbaro(1,1)
       montg(:,:) =  srfht(1,1)  !not correct
       srfht(:,:) =  srfht(1,1)
      surflx(:,:) = surflx(1,1)
      salflx(:,:) = salflx(1,1)  !not correct?
        dpbl(:,:) =   dpbl(1,1)
      dpmixl(:,:) = dpmixl(1,1)
       ubaro(:,:) =  ubaro(1,1)
       vbaro(:,:) =  vbaro(1,1)
c
      read( ni,'(a)') cline
      kkin=1
      do 14 k=1,kk
        read (ni,'(a)',end=6)   cline
        read(cline,*) idum,u(1,1,k),v(1,1,k),
     &                     temp(1,1,k),saln(1,1,k),th3d(1,1,k),
     &                     dp(1,1,k)
           u(:,:,k) =    u(1,1,k) - ubaro(1,1)
           v(:,:,k) =    v(1,1,k) - vbaro(1,1)
        temp(:,:,k) = temp(1,1,k)
        saln(:,:,k) = saln(1,1,k)
        th3d(:,:,k) = th3d(1,1,k)
          dp(:,:,k) =   dp(1,1,k)*9806.0
c
        if     (k.eq.1) then
           tmix(:,:) = temp(:,:,1)
           smix(:,:) = saln(:,:,1)
          thmix(:,:) = th3d(:,:,1)
           umix(:,:) =    u(:,:,1)
           vmix(:,:) =    v(:,:,1)
        endif !k==1
      write(lp,'(a,i3)') 'finished reading data for layer',k
      call flush(lp)
 14   continue
      kkin=kk
114   continue
c
c --- thbase?
c
      if     (th3d(1,1,kk).lt.33.0) then
        thbase = 25.0
      else
        thbase = 34.0
      endif
      thmix(:,:)   = thmix(:,:)   - thbase
       th3d(:,:,:) =  th3d(:,:,:) - thbase
      close( unit=ni)
      write(lp,'(a)') 'closed archive file'
      call flush(lp)
c
      if (initl) then
c ---   acquire basin depths and land/sea mask
        call getdepth(work)
      end if
c
      return
c
c --- unexpected end of file
 6    continue
      write (lp,*) '***** unexpected end of archive file *****'
      call flush(lp)
      stop '(e-o-f)'
      end

      subroutine getfld(work, iunit, hminb,hmaxb, lzero,lrange)
      use mod_za ! HYCOM array I/O interface
c
c --- read a single array
c
      logical lzero,lrange
      integer iunit
      real    work(idm,jdm), hminb,hmaxb
c
      integer mask(1)  !dummy which is never accessed
      real    hmina,hmaxa
c
      call zaiord(work,mask,.false., hmina,hmaxa, iunit)
c
      if     (lrange) then
        if     (abs(hmina-hminb).gt.max(abs(hmina),
     &                                  abs(hminb))*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.max(abs(hmaxa),
     &                                  abs(hmaxb))*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          call flush(lp)
          stop
        endif
      endif
c
      if     (lzero) then
        do j= 1,jdm
          do i= 1,idm
            if     (work(i,j).gt.2.0**99) then
              work(i,j) = 0.0
            endif
          enddo
        enddo
      endif
      return
      end
      subroutine time_hour(time)
      implicit none
c
      double precision time(3)
c
c --- reset time to an exact hour if very close to an hour.
c
      integer k
      double precision day,hour,ihr
c
      do k= 1,3
        day  = int(time(k))
        hour = (time(k)-day)*24.d0
        ihr  = nint(hour)
        if     (abs(hour-ihr).le.0.15d0) then
          time(k) = day + ihr/24.d0
        endif
      enddo
      return
      end

      subroutine th3d_p(temp,saln,th3d,no,mo,sigver,thbase)
      implicit none
c
      integer no,mo,sigver
      real    temp(no,mo),saln(no,mo),th3d(no,mo),thbase
c
c --- calculate density using the appropriate equation of state.
c
      if     (sigver.eq.1) then
        call th3d_p1(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.2) then
        call th3d_p2(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.3) then
        call th3d_p3(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.4) then
        call th3d_p4(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.5) then
        call th3d_p5(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.6) then
        call th3d_p6(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.7) then
        call th3d_p7(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.8) then
        call th3d_p8(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.46) then
        call th3d_p46(temp,saln,th3d,no,mo,thbase)
      elseif (sigver.eq.48) then
        call th3d_p48(temp,saln,th3d,no,mo,thbase)
      else  !unknown
        th3d(:,:) = 0.0
      endif
      return
      end
      subroutine th3d_p1(temp,saln,th3d,no,mo,thbase)
      implicit none
c
      integer no,mo
      real    temp(no,mo),saln(no,mo),th3d(no,mo),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA0_7term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p2(temp,saln,th3d,no,mo,thbase)
      implicit none
c
      integer no,mo
      real    temp(no,mo),saln(no,mo),th3d(no,mo),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA2_7term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p3(temp,saln,th3d,no,mo,thbase)
      implicit none
c
      integer no,mo
      real    temp(no,mo),saln(no,mo),th3d(no,mo),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA0_9term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p4(temp,saln,th3d,no,mo,thbase)
      implicit none
c
      integer no,mo
      real    temp(no,mo),saln(no,mo),th3d(no,mo),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA2_9term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p5(temp,saln,th3d,no,mo,thbase)
      implicit none
c
      integer no,mo
      real    temp(no,mo),saln(no,mo),th3d(no,mo),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA0_17term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p6(temp,saln,th3d,no,mo,thbase)
      implicit none
c
      integer no,mo
      real    temp(no,mo),saln(no,mo),th3d(no,mo),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA2_17term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p7(temp,saln,th3d,no,mo,thbase)
      implicit none
c
      integer no,mo
      real    temp(no,mo),saln(no,mo),th3d(no,mo),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA0_12term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p8(temp,saln,th3d,no,mo,thbase)
      implicit none
c
      integer no,mo
      real    temp(no,mo),saln(no,mo),th3d(no,mo),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA2_12term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p46(temp,saln,th3d,no,mo,thbase)
      implicit none
c
      integer no,mo
      real    temp(no,mo),saln(no,mo),th3d(no,mo),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA4_17term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine th3d_p48(temp,saln,th3d,no,mo,thbase)
      implicit none
c
      integer no,mo
      real    temp(no,mo),saln(no,mo),th3d(no,mo),thbase
c
c --- calculate density using the equation of state.
c
c     spval  = data void marker, 2^100 or about 1.2676506e30
      real, parameter :: spval=2.0**100
c
      integer i,j
c
      include '../../include/stmt_fns_SIGMA4_12term.h'
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).ne.spval) then
            th3d(i,j) = sig(r8(temp(i,j)),r8(saln(i,j))) - thbase
          else
            th3d(i,j) = spval
          endif
        enddo !i
      enddo !j
      return
      end
