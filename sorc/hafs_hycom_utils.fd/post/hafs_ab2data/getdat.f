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
     &        action='read')
c     &        status='old',action='read')
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

      subroutine getdat(flnm,time,artype,initl,icegln,trcout,surflg,
     &                  iexpt,iversn,yrflag,kkin)
      use mod_za  ! HYCOM array I/O interface
      implicit none
c
      character        flnm*(*)
      double precision time(3)
      logical          initl,icegln,trcout,lsteric
      integer          artype,iexpt,iversn,yrflag,kkin,surflg
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
      if     (flnm(l-2:l).eq.'v.a' .or. flnm(l-2:l).eq.'v.b') then
c ---   HYCOM 2.0 array I/O archive file.
        call getdata(flnm,time,artype,.true.,
     &               initl,icegln,trcout,surflg,
     &               iexpt,iversn,yrflag,kkin,      work)
      else
c ---   partial surface archive
          call getdata1(flnm,time,artype,.true.,
     &                  initl,lsteric,icegln,
     &                  iexpt,iversn,yrflag,kkin,      work)

      endif
      deallocate( util, work )
      return
      end

      subroutine getdatb(flnm,time,artype,initl,icegln,trcout,surflg,
     &                   iexpt,iversn,yrflag,kkin)
      use mod_za  ! HYCOM array I/O interface
      implicit none
c
      character        flnm*(*)
      double precision time(3)
      logical          initl,icegln,trcout,lsteric
      integer          artype,iexpt,iversn,yrflag,kkin,surflg
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
     &               initl,icegln,trcout,surflg,
     &               iexpt,iversn,yrflag,kkin,      work)
      else
c ---   HYCOM 1.0 pakked archive file.
        call getdatp(flnm,time(1),
     &               initl,icegln,trcout,
     &               iexpt,iversn,yrflag,kkin, util,work)
        time(2) = time(1)
        time(3) = time(1)
        artype  = 1
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
      logical          initl,icegln,trcout,lsteric
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
          call clsgks
          stop 9
        endif
        read (ni,*) iexpt,cvarin
        write(lp,*) cvarin,' = ',iexpt
        if (cvarin.ne.'iexpt ') then
          write(lp,*)
          write(lp,*) 'error in getdat - input ',cvarin,
     &                          ' but should be iexpt '
          write(lp,*)
          call flush(lp)
          call clsgks
          stop 9
        endif
        read (ni,*) yrflag,cvarin
        write(lp,*) cvarin,' = ',yrflag
        if (cvarin.ne.'yrflag') then
          write(lp,*)
          write(lp,*) 'error in getdat - input ',cvarin,
     &                          ' but should be yrflag'
          write(lp,*)
          call flush(lp)
          call clsgks
          stop 9
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
        call clsgks
        stop 9
      endif
      call unpakk(  work(idm-i,1),idm,i,j,util,lgth)
      call extrct_u(work,idm,jdm,iorign,jorign, 
     &              u(1,1,2*k),ii,jj)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'u       ',k
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work(1,2),idm,i,j,util,lgth)
      call extrct_v(work,idm,jdm,iorign,jorign, 
     &              v(1,1,2*k),ii,jj)
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
     &              temp(1,1,2*k),ii,jj)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'temp    ',k
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,idm,i,j,util,lgth)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              saln(1,1,2*k),ii,jj)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'saln    ',k
c
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,idm,i,j,util,lgth)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              th3d(1,1,2*k),ii,jj)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'th3d    ',k
c
      if(trcout) then
      read (ni,'(3x,a,a)',end=6) text,cline
      read (cline,      *,end=6) nstep,time,thet,i,j,lgth
      read (ni,  '(40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,idm,i,j,util,lgth)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              tracer(1,1,k),ii,jj)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'tracer  ',k
      end if
c
ccc      write(lp,'(a,i4)') 'shown below: density in layer',k
ccc      call zebra(th3d(1,1,2*k),ii,ii1,jj1)
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
      surflx_evap(:,:) = 0.0
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
      call clsgks
      stop '(e-o-f)'
      end

      subroutine getdata(flnm,time,artype,lrange,initl,icegln,trcout,
     &                   surflg,
     &                   iexpt,iversn,yrflag,kkin, work)
      use mod_plot  ! HYCOM plot array interface
      use mod_za  ! HYCOM array I/O interface
c
      character        flnm*(*)
      double precision time(3)
      logical          lrange,initl,icegln,trcout,lsteric
      integer          artype,iexpt,iversn,yrflag,kkin,surflg
      real             work(idm,jdm)
c
c --- read model fields and extract portion of global fields.
c --- HYCOM 2.0 array I/O archive file.
c --- (no time-averaged fluxes in this version)
c
      character cline*80
      character preambl(5)*79
      character cvarin*6
      real      hminb,hmaxb
      integer   i,j,ios,l
      logical   lke
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
        call clsgks
        stop 9
      endif
      read( ni,*) iexpt,cvarin
      write(lp,*) cvarin,' = ',iexpt
      if (cvarin.ne.'iexpt ') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be iexpt '
        write(lp,*)
        call flush(lp)
        call clsgks
        stop 9
      endif
      read( ni,*) yrflag,cvarin
      write(lp,*) cvarin,' = ',yrflag
      if (cvarin.ne.'yrflag') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be yrflag'
        write(lp,*)
        call flush(lp)
        call clsgks
        stop 9
      endif
      read( ni,*) idmtst,cvarin
      write(lp,*) cvarin,' = ',idmtst
      if (cvarin.ne.'idm   ') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be idm   '
        write(lp,*)
        call flush(lp)
        call clsgks
        stop 9
      endif
      read( ni,*) jdmtst,cvarin
      write(lp,*) cvarin,' = ',jdmtst
      if (cvarin.ne.'jdm   ') then
        write(lp,*)
        write(lp,*) 'error in getdat - input ',cvarin,
     &                        ' but should be jdm   '
        write(lp,*)
        call flush(lp)
        call clsgks
        stop 9
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
        call clsgks
        stop 9
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
      read (cline(i+1:),*)  nstep,time(2),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &              steric,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'steric  '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              surflx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'oneta  '
      write(lp,*) 'time3 = ',time
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &              surflx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'surflx  '
      write(lp,*) 'time3 = ',time
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &              surflx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'wtrflx  '
      write(lp,*) 'time3 = ',time
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              salflx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'salflx  '
c
      if(surflg.gt.1) then
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &              surflx_evap,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'surflx_evap'

      endif
c
      if(surflg.gt.3) then
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &              surflx_assi,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'surflx_assi'
      endif
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              dpbl,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'dpbl    '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              dpmixl,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'dpmixl  '
c
c --- is there ke?
      read (ni,'(a)',iostat=ios) cline
      write(lp,'(a)')            cline(1:len_trim(cline))
      lke = artype.gt.1 .and. ios.eq.0 .and. cline(1:8).eq.'kemix   '
      if     (lke) then  ! mean or std. archive with ke
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
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
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                covice,ii,jj)
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                thkice,ii,jj)
c
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
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
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      call extrct_u(work,idm,jdm,iorign,jorign, 
     &              ubaro,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'ubaro   '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      call extrct_v(work,idm,jdm,iorign,jorign, 
     &              vbaro,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'vbaro   '
c
      if     (lke) then  ! mean or std. archive with ke
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                kebaro,ii,jj)
        write(lp,'("input  ",a," into ",a)') cline(1:8),'kebaro  '
      endif
c
      kkin=1
      do 14 k=1,kk
      if     (k.ge.2) then
c ---   some archives only contain layer 1.
        read (ni,'(a)',end=114) cline
        write(lp,'(a)')         cline(1:len_trim(cline))
      else
        read (ni,'(a)',end=6)   cline
        write(lp,'(a)')         cline(1:len_trim(cline))
      endif
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      if     (cline(1:8).ne.'u-vel.  ') then
        write(lp,*)
        write(lp,*) 'error in getdat - layer ',k,
     &             ' does not exist (kk= ',kk,')'
        write(lp,*)
        write(lp,*) 'cline ',cline
        call flush(lp)
        call clsgks
        stop 9
      endif
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      call extrct_u(work,idm,jdm,iorign,jorign, 
     &              u(1,1,2*k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'u       ',k
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      call extrct_v(work,idm,jdm,iorign,jorign, 
     &              v(1,1,2*k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'v       ',k
c
      if     (lke) then  ! mean or std. archive with ke
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                ke(1,1,2*k),ii,jj)
        write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'ke      ',k
      endif
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true., lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              dp(1,1,k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'dp      ',k
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              temp(1,1,2*k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'temp    ',k
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              saln(1,1,2*k),ii,jj)
      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'saln    ',k
c
c-hsk2017:
c      read (ni,'(a)',end=6) cline
c      write(lp,'(a)')       cline(1:len_trim(cline))
c      i = index(cline,'=')
c      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c      call extrct_p(work,idm,jdm,iorign,jorign, 
c     &              th3d(1,1,2*k),ii,jj)
c      write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'th3d    ',k
c
      if(trcout) then
        read (ni,'(a)',end=6) cline
        write(lp,'(a)')       cline(1:len_trim(cline))
        i = index(cline,'=')
        read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
        call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                tracer(1,1,k),ii,jj)
        write(lp,'("input  ",a," into ",a,i3)') cline(1:8),'tracer  ',k
      end if
c
ccc      write(lp,'(a,i4)') 'shown below: density in layer',k
ccc      call zebra(th3d(1,1,2*k),ii,ii1,jj1)
c
      write(lp,'(a,f9.5)') 'finished reading data for layer',thet
      call flush(lp)
      theta(k)=thet
 14   continue

cc atmos
c      read (ni,'(a)',end=6) cline
c      write(lp,'(a)')       cline(1:len_trim(cline))
c      i = index(cline,'=')
c      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c      call extrct_p(work,idm,jdm,iorign,jorign,
c     &                    stressx,ii,jj)
c      write(lp,'("input  ",a," into ",a)') cline(1:8),'stressx'
cc
c      read (ni,'(a)',end=6) cline
c      write(lp,'(a)')       cline(1:len_trim(cline))
c      i = index(cline,'=')
c      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c      call extrct_p(work,idm,jdm,iorign,jorign,
c     &                    stressy,ii,jj)
c      write(lp,'("input  ",a," into ",a)') cline(1:8),'stressy'
cc
c      read (ni,'(a)',end=6) cline
c      write(lp,'(a)')       cline(1:len_trim(cline))
c      i = index(cline,'=')
c      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c      call extrct_p(work,idm,jdm,iorign,jorign,
c     &                    precip,ii,jj)
c      write(lp,'("input  ",a," into ",a)') cline(1:8),'precip'
cc
c      read (ni,'(a)',end=6) cline
c      write(lp,'(a)')       cline(1:len_trim(cline))
c      i = index(cline,'=')
c      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c      call extrct_p(work,idm,jdm,iorign,jorign,
c     &                    rflux,ii,jj)
c      write(lp,'("input  ",a," into ",a)') cline(1:8),'rflux'
cc
c      read (ni,'(a)',end=6) cline
c      write(lp,'(a)')       cline(1:len_trim(cline))
c      i = index(cline,'=')
c      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c      call extrct_p(work,idm,jdm,iorign,jorign,
c     &                    swflux,ii,jj)
c      write(lp,'("input  ",a," into ",a)') cline(1:8),'swflux'
cc
c      read (ni,'(a)',end=6) cline
c      write(lp,'(a)')       cline(1:len_trim(cline))
c      i = index(cline,'=')
c      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c      call extrct_p(work,idm,jdm,iorign,jorign,
c     &                    atmpres,ii,jj)
c      write(lp,'("input  ",a," into ",a)') cline(1:8),'atmpres'
cc
c      read (ni,'(a)',end=6) cline
c      write(lp,'(a)')       cline(1:len_trim(cline))
c      i = index(cline,'=')
c      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c      call extrct_p(work,idm,jdm,iorign,jorign,
c     &                    ssflux,ii,jj)
c      write(lp,'("input  ",a," into ",a)') cline(1:8),'ssflux'
cc
c      read (ni,'(a)',end=6) cline
c      write(lp,'(a)')       cline(1:len_trim(cline))
c      i = index(cline,'=')
c      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c      call extrct_p(work,idm,jdm,iorign,jorign,
c     &                    lflux,ii,jj)
c      write(lp,'("input  ",a," into ",a)') cline(1:8),'lflux'
cc
c      read (ni,'(a)',end=6) cline
c      write(lp,'(a)')       cline(1:len_trim(cline))
c      i = index(cline,'=')
c      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c      call extrct_p(work,idm,jdm,iorign,jorign,
c     &                    gfsstx,ii,jj)
c      write(lp,'("input  ",a," into ",a)') cline(1:8),'gfsstx'
cc
c      read (ni,'(a)',end=6) cline
c      write(lp,'(a)')       cline(1:len_trim(cline))
c      i = index(cline,'=')
c      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c      call extrct_p(work,idm,jdm,iorign,jorign,
c     &                    gfssty,ii,jj)
c      write(lp,'("input  ",a," into ",a)') cline(1:8),'gfssty'
cc
c      read (ni,'(a)',end=6) cline
c      write(lp,'(a)')       cline(1:len_trim(cline))
c      i = index(cline,'=')
c      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c      call extrct_p(work,idm,jdm,iorign,jorign,
c     &                    gprcp,ii,jj)
c      write(lp,'("input  ",a," into ",a)') cline(1:8),'gprcp'
cc
c      read (ni,'(a)',end=6) cline
c      write(lp,'(a)')       cline(1:len_trim(cline))
c      i = index(cline,'=')
c      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c      call extrct_p(work,idm,jdm,iorign,jorign,
c     &                    grdfx,ii,jj)
c      write(lp,'("input  ",a," into ",a)') cline(1:8),'grdfx'
cc
c      read (ni,'(a)',end=6) cline
c      write(lp,'(a)')       cline(1:len_trim(cline))
c      i = index(cline,'=')
c      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c      call extrct_p(work,idm,jdm,iorign,jorign,
c     &                    gswfx,ii,jj)
c      write(lp,'("input  ",a," into ",a)') cline(1:8),'gswfx'
cc
c      read (ni,'(a)',end=6) cline
c      write(lp,'(a)')       cline(1:len_trim(cline))
c      i = index(cline,'=')
c      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c      call extrct_p(work,idm,jdm,iorign,jorign,
c     &                    gatps,ii,jj)
c      write(lp,'("input  ",a," into ",a)') cline(1:8),'gatps'
c
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
      call clsgks
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
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          call flush(lp)
          call clsgks
          stop 9
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
      integer, parameter :: nfields=18  !no. fields in surface archive
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
c
      l_arch( 1) = .true.
      l_arch( 2) = .true.
      l_arch( 3) = .true.
      l_arch( 4) = .true.
      l_arch( 5) = .true.
      l_arch( 6) = .true.
      l_arch( 7) = .true.
      l_arch( 8) = .false.
      l_arch( 9) = .false.
      l_arch(10) = .false.
      l_arch(11) = .true.
      l_arch(12) = .true.
      l_arch(13) = .true.
      l_arch(14) = .true.
      l_arch(15) = .true.
      l_arch(16) = .true.
      l_arch(17) = .true.
      l_arch(18) = .false.
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

