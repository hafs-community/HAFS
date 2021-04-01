      subroutine getartype(flnm,artype)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      character        flnm*(*)
      integer          artype
c
c --- read artype from archive file.
c ---     artype==1 for normal archive files
c ---     artype==2 for   mean archive files
c ---     artype==3 for stddev archive files
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
        else
          artype = 1
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
      character, allocatable :: util(:)*2
      real,      allocatable :: work(:,:)
c
      allocate( util(idm*jdm+14) )
      allocate( work(idm,jdm)    )
c
      l = len_trim(flnm)
c
      if     (flnm(l-1:l).eq.'.a' .or. flnm(l-1:l).eq.'.b') then
c ---   HYCOM 2.0 array I/O archive file.
        call getdata(flnm,time,artype,.true.,
     &               initl,lsteric,icegln,
     &               iexpt,iversn,yrflag,kkin,      work)
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
c ---   HYCOM 1.0 pakked archive file.
        call getdatp(flnm,time(1),
     &               initl,icegln,trcout,
     &               iexpt,iversn,yrflag,kkin, util,work)
        lsteric = .false.
        time(2) = time(1)
        time(3) = time(1)
        artype  = 1
        sigver  = 0
      endif
      deallocate( util, work )
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
      character, allocatable :: util(:)*2
      real,      allocatable :: work(:,:)
c
      allocate( util(idm*jdm+14) )
      allocate( work(idm,jdm)    )
c
      l = len_trim(flnm)
c
      if     (flnm(l-1:l).eq.'.a' .or. flnm(l-1:l).eq.'.b') then
c ---   HYCOM 2.0 array I/O archive file.
        call getdatas(flnm,time,artype,.true.,
     &                initl,icegln,
     &                iexpt,iversn,yrflag,kkin,      work)
        lsteric = .false.
        sigver  = 0
      else
c ---   HYCOM 1.0 pakked archive file.
        call getdatp(flnm,time(1),
     &               initl,icegln,trcout,
     &               iexpt,iversn,yrflag,kkin, util,work)
        lsteric = .false.
        time(2) = time(1)
        time(3) = time(1)
        artype  = 1
        sigver  = 0
      endif
      deallocate( util, work )
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
      character, allocatable :: util(:)*2
      real,      allocatable :: work(:,:)
c
      allocate( util(idm*jdm+14) )
      allocate( work(idm,jdm)    )
c
      l = len_trim(flnm)
c
      if     (flnm(l-1:l).eq.'.a' .or. flnm(l-1:l).eq.'.b') then
c ---   HYCOM 2.0 array I/O archive file.
        call getdata(flnm,time,artype,.false.,
     &               initl,lsteric,icegln,
     &               iexpt,iversn,yrflag,kkin,      work)
      else
c ---   HYCOM 1.0 pakked archive file.
        call getdatp(flnm,time(1),
     &               initl,icegln,trcout,
     &               iexpt,iversn,yrflag,kkin, util,work)
        lsteric = .false.
        time(2) = time(1)
        time(3) = time(1)
        artype  = 1
        sigver  = 0
      endif
      deallocate( util, work )
      return
      end

      subroutine getdatp(flnm,time,initl,icegln,trcout,
     &                   iexpt,iversn,yrflag,kkin, util,work)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
c
      character        flnm*(*)
      double precision time
      logical          initl,icegln,trcout
      integer          iexpt,iversn,yrflag,kkin
      character        util(idm*jdm+14)*2
      real             work(idm,jdm)
c
c --- read model fields and extract portion of global fields.
c --- HYCOM 1.0 pakked archive file.
c --- (no time-averaged fluxes in this version)
c
      character cline*80
      character text*8,preambl(5)*79
      character cvarin*6,c2*2
      real      hminb,hmaxb
      integer   ios
c
      data ni/14/
c
      open (unit=ni,file=flnm,form='formatted',
     &      status='old',action='read')
c
c --- which version of file format?
      read(ni,'(a2)') c2
      rewind(unit=ni)
      if (c2.ne.'::') then
        read (ni,'(a80/a80/a80/a80)') ctitle
        write(lp,'(a80/a80/a80/a80)') ctitle
        read (ni,*) iversn,cvarin
        write(lp,*) cvarin,' = ',iversn
        if (cvarin.ne.'iversn') then
          write(lp,*)
          write(lp,*) 'error in getdat - input ',cvarin,
     &                          ' but should be iversn'
          write(lp,*)
          call flush(lp)
          stop
        endif
        read (ni,*) iexpt,cvarin
        write(lp,*) cvarin,' = ',iexpt
        if (cvarin.ne.'iexpt ') then
          write(lp,*)
          write(lp,*) 'error in getdat - input ',cvarin,
     &                          ' but should be iexpt '
          write(lp,*)
          call flush(lp)
          stop
        endif
        read (ni,*) yrflag,cvarin
        write(lp,*) cvarin,' = ',yrflag
        if (cvarin.ne.'yrflag') then
          write(lp,*)
          write(lp,*) 'error in getdat - input ',cvarin,
     &                          ' but should be yrflag'
          write(lp,*)
          call flush(lp)
          stop
        endif
      endif
c
      do 14 k=1,kk
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
 100  format (' ::',a8,i8,f8.1,f8.3,2i5,i9,1x,14a2)
      if     (text(1:8).ne.'u-vel.  ') then
        write(lp,*)
        write(lp,*) 'error in getdat - layer ',k,
     &             ' does not exist (kk= ',kk,')'
        write(lp,*)
        call flush(lp)
        stop
      endif
      call unpakk(  work(idm-i,1),idm,i,j,util,lgth)
      call extrct_u(work,idm,jdm,iorign,jorign, 
     &              u(1,1,k),ii,jj)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'u       ',k
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work(1,2),idm,i,j,util,lgth)
      call extrct_v(work,idm,jdm,iorign,jorign, 
     &              v(1,1,k),ii,jj)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'v       ',k
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,idm,i,j,util,lgth)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              dp(1,1,k),ii,jj)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'dp      ',k
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,idm,i,j,util,lgth)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              temp(1,1,k),ii,jj)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'temp    ',k
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,idm,i,j,util,lgth)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              saln(1,1,k),ii,jj)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'saln    ',k
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,idm,i,j,util,lgth)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              th3d(1,1,k),ii,jj)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'th3d    ',k
c
      if(trcout) then
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,idm,i,j,util,lgth)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              trcr(1,1,k,1),ii,jj)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'trcr    ',k
      end if
c
ccc      write(lp,'(a,i4)') 'shown below: density in layer',k
ccc      call zebra(th3d(1,1,k),ii,ii1,jj1)
c
      write(lp,'(a,f9.5)') 'finished reading data for layer',thet
      call flush(lp)
      theta(k)=thet
 14   continue
      kkin=kk
c
c --- now unpack barotropic velocity field
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work(idm-i,1),idm,i,j,util,lgth)
      call extrct_u(work,idm,jdm,iorign,jorign, 
     &              ubaro,ii,jj)
      write(lp,'("unpakk ",a," into ",a)') text(1:8),'ubaro   '
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work(1,2),idm,i,j,util,lgth)
      call extrct_v(work,idm,jdm,iorign,jorign, 
     &              vbaro,ii,jj)
      write(lp,'("unpakk ",a," into ",a)') text(1:8),'vbaro   '
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,idm,i,j,util,lgth)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              montg,ii,jj)
      write(lp,'("unpakk ",a," into ",a)') text(1:8),'montg   '
* --- discard montg
*     write(lp,'("unpakk ",a," into ",a)') text(1:8),'work    '
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,idm,i,j,util,lgth)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              srfht,ii,jj)
      write(lp,'("unpakk ",a," into ",a)') text(1:8),'srfht   '
c
      surflx(:,:) = 0.0
      salflx(:,:) = 0.0
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,idm,i,j,util,lgth)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              dpbl,ii,jj)
      write(lp,'("unpakk ",a," into ",a)') text(1:8),'dpbl    '
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,idm,i,j,util,lgth)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              dpmixl,ii,jj)
      write(lp,'("unpakk ",a," into ",a)') text(1:8),'dpmixl  '
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,idm,i,j,util,lgth)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              tmix,ii,jj)
      write(lp,'("unpakk ",a," into ",a)') text(1:8),'tmix    '
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,idm,i,j,util,lgth)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              smix,ii,jj)
      write(lp,'("unpakk ",a," into ",a)') text(1:8),'smix    '
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,idm,i,j,util,lgth)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              thmix,ii,jj)
      write(lp,'("unpakk ",a," into ",a)') text(1:8),'thmix   '
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work(idm-i,1),idm,i,j,util,lgth)
      call extrct_u(work,idm,jdm,iorign,jorign, 
     &              umix,ii,jj)
      write(lp,'("unpakk ",a," into ",a)') text(1:8),'umix    '
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work(1,2),idm,i,j,util,lgth)
      call extrct_v(work,idm,jdm,iorign,jorign, 
     &              vmix,ii,jj)
      write(lp,'("unpakk ",a," into ",a)') text(1:8),'vmix    '
c
c --- is there ice?
      read (ni,'(3x,a,a)',iostat=ios) text,cline
      icegln = ios.eq.0 .and. text.eq.'covice  '
      if     (icegln) then
        read (cline,      *,end=6) nstep,time,thet,i,j,lgth
        read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
        write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,
     &                          (util(l),l=1,14)
        call unpakk(  work,idm,i,j,util,lgth)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                covice,ii,jj)
c
        read (ni,'(3x,a,a)',end=6) text,cline
        read (cline,      *,end=6) nstep,time,thet,i,j,lgth
        read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
        write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,
     &                          (util(l),l=1,14)
        call unpakk(  work,idm,i,j,util,lgth)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                thkice,ii,jj)
c
        read (ni,'(3x,a,a)',end=6) text,cline
        read (cline,      *,end=6) nstep,time,thet,i,j,lgth
        read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
        write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,
     &                          (util(l),l=1,14)
        call unpakk(  work,idm,i,j,util,lgth)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                temice,ii,jj)
      else
        covice(:,:) = 0.0
        thkice(:,:) = 0.0
        temice(:,:) = 0.0
      endif
c
      close(unit=ni)
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

      subroutine getdata(flnm,time,artype,lrange,initl,lsteric,icegln,
     &                   iexpt,iversn,yrflag,kkin, work)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
c
      character        flnm*(*)
      double precision time(3)
      logical          lrange,initl,lsteric,icegln,nodens
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
c --- artype==1 for normal archive files
c --- artype==2 for   mean archive files
c --- artype==3 for stddev archive files
c
      read( ni,'(a)') cline
      write(lp,'(a)') cline(1:len_trim(cline))
      if     (cline(25:28).eq.'mean') then
        artype = 2
      elseif (cline(25:28).eq.'std.') then
        artype = 3
      else
        artype = 1
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
      if     (.not. nodens) then
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
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
      endif !.not. nodens
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
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                thkice,ii,jj)
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                temice,ii,jj)
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
      else
        covice(:,:) = 0.0
        thkice(:,:) = 0.0
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
          elseif (cline(1:8).ne.'tracer  ' .and.
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
              if     (cline(1:8).eq.'tracer  ') then
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

      subroutine getdatas(flnm,time,artype,lrange,initl,icegln,
     &                    iexpt,iversn,yrflag,kkin, work)
      use mod_plot  ! HYCOM plot array interface
      use mod_za  ! HYCOM array I/O interface
c
      character        flnm*(*)
      double precision time(3)
      logical          lrange,initl,icegln
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
c --- artype==1 for normal archive files
c --- artype==2 for   mean archive files
c --- artype==3 for stddev archive files
c
      read( ni,'(a)') cline
      write(lp,'(a)') cline(1:len_trim(cline))
      if     (cline(25:28).eq.'mean') then
        artype = 2
      elseif (cline(25:28).eq.'std.') then
        artype = 3
      else
        artype = 1
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
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                thkice,ii,jj)
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,timedum,layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                temice,ii,jj)
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
      else
        covice(:,:) = 0.0
        thkice(:,:) = 0.0
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
          elseif (cline(1:8).ne.'tracer  ' .and.
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
c --- calculate density using the equation of state.
c
      integer i,j
      real    c1,c2,c3,c4,c5,c6,c7,c8,c9
      real    sig,t,s
c
      sig(t,s)=(c1+s*(c3+s* c8)+
     &             t*(c2+s*(c5+s*c9)+t*(c4+s*c7+t*c6)))
c
      if     (sigver.eq.1) then
c ---   !7-term sigma-0
        c1=-1.364710E-01   !const. coefficent
        c2= 4.681810E-02   !T      coefficent
        c3= 8.070040E-01   !   S   coefficent
        c4=-7.453530E-03   !T^2    coefficent
        c5=-2.944180E-03   !T  S   coefficent
        c6= 3.435700E-05   !T^3    coefficent
        c7= 3.486580E-05   !T^2S   coefficent
        c8= 0.0            !   S^2 coefficent
        c9= 0.0            !T  S^2 coefficent
      elseif (sigver.eq.2) then
c ---   !7-term sigma-2
        c1= 9.770930E+00   !const. coefficent
        c2=-2.264930E-02   !T      coefficent
        c3= 7.898790E-01   !   S   coefficent
        c4=-6.432050E-03   !T^2    coefficent
        c5=-2.629830E-03   !T  S   coefficent
        c6= 2.758350E-05   !T^3    coefficent
        c7= 3.152350E-05   !T^2S   coefficent
        c8= 0.0            !   S^2 coefficent
        c9= 0.0            !T  S^2 coefficent
      elseif (sigver.eq.3) then
c ---   !9-term sigma-0
        c1=-4.311829E-02   !const. coefficent
        c2= 5.429948E-02   !T      coefficent
        c3= 8.011774E-01   !   S   coefficent
        c4=-7.641336E-03   !T^2    coefficent
        c5=-3.258442E-03   !T  S   coefficent
        c6= 3.757643E-05   !T^3    coefficent
        c7= 3.630361E-05   !T^2S   coefficent
        c8= 8.675546E-05   !   S^2 coefficent
        c9= 3.995086E-06   !T  S^2 coefficent
      elseif (sigver.eq.4) then
c ---   !9-term sigma-2
        c1= 9.903308E+00   !const. coefficent
        c2=-1.618075E-02   !T      coefficent
        c3= 7.819166E-01   !   S   coefficent
        c4=-6.593939E-03   !T^2    coefficent
        c5=-2.896464E-03   !T  S   coefficent
        c6= 3.038697E-05   !T^3    coefficent
        c7= 3.266933E-05   !T^2S   coefficent
        c8= 1.180109E-04   !   S^2 coefficent
        c9= 3.399511E-06   !T  S^2 coefficent
      else
c ---   !unknown
        c1= 0.0            !const. coefficent
        c2= 0.0            !T      coefficent
        c3= 0.0            !   S   coefficent
        c4= 0.0            !T^2    coefficent
        c5= 0.0            !T  S   coefficent
        c6= 0.0            !T^3    coefficent
        c7= 0.0            !T^2S   coefficent
        c8= 0.0            !   S^2 coefficent
        c9= 0.0            !T  S^2 coefficent
      endif
c
      do j= 1,mo
        do i= 1,no
          if     (temp(i,j).gt.2.0**99) then
            th3d(i,j) = temp(i,j)
          else
            th3d(i,j) = sig(temp(i,j),saln(i,j)) - thbase
          endif
        enddo !i
      enddo !j
      return
      end
