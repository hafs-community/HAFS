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
      logical          initl,icegln,trcout
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
      if     (flnm(l-1:l).eq.'.a' .or. flnm(l-1:l).eq.'.b') then
c ---   HYCOM 2.0 array I/O archive file.
        call getdata(flnm,time,artype,.true.,
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

      subroutine getdatb(flnm,time,artype,initl,icegln,trcout,surflg,
     &                   iexpt,iversn,yrflag,kkin)
      use mod_za  ! HYCOM array I/O interface
      implicit none
c
      character        flnm*(*)
      double precision time(3)
      logical          initl,icegln,trcout
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
      surflx_snsi(:,:) = 0.0
      surflx_assi(:,:) = 0.0
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
      logical          lrange,initl,icegln,trcout
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
      write(lp,'("input  ",a," into ",a)') cline(1:8),'surflx  '
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

c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &                    surflx_snsi,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'surflx_snsi'

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
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              tmix,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'tmix    '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              smix,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'smix    '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign, 
     &              thmix,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'thmix   '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .true. ,lrange)
      call extrct_u(work,idm,jdm,iorign,jorign, 
     &              umix,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'umix    '
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
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

c skip diafx000 (20080123 - not written out anymore)
c     do 15 k=1,kk
c       read (ni,'(a)',end=6) cline
c       write(lp,'(a)')       cline(1:len_trim(cline))
c       i = index(cline,'=')
c       read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
c       call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
c 15  continue

c atmos
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &                    stressx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'stressx'
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &                    stressy,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'stressy'
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &                    precip,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'precip'
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &                    rflux,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'rflux'
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &                    swflux,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'swflux'
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &                    atmpres,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'atmpres'
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &                    ssflux,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'ssflux'
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &                    lflux,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'lflux'
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &                    gfsstx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'gfsstx'
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &                    gfssty,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'gfssty'
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &                    gprcp,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'gprcp'
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &                    grdfx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'grdfx'
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &                    gswfx,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'gswfx'
c
      read (ni,'(a)',end=6) cline
      write(lp,'(a)')       cline(1:len_trim(cline))
      i = index(cline,'=')
      read (cline(i+1:),*)  nstep,time(3),layer,thet,hminb,hmaxb
      call getfld(  work, ni, hminb,hmaxb, .false.,lrange)
      call extrct_p(work,idm,jdm,iorign,jorign,
     &                    gatps,ii,jj)
      write(lp,'("input  ",a," into ",a)') cline(1:8),'gatps'
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
