      subroutine getdat4rtofs(flnm,time,artype,initl,icegln,trcout,
     &              surflg,iexpt,iversn,yrflag,kkin)
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
      print*," hsk ---  @getdat4rtofs, flnm=",flnm

      if     (flnm(l-1:l).eq.'.a' .or. flnm(l-1:l).eq.'.b') then
c ---   HYCOM 2.0 array I/O archive file.
        call getdat4rtofsa(flnm,time,artype,.true.,
     &               initl,icegln,trcout,surflg,
     &               iexpt,iversn,yrflag,kkin,      work)
      endif
      deallocate( util, work )
      return
      end

      subroutine getdat4rtofsb(flnm,time,artype,initl,icegln,trcout,
     &            surflg,iexpt,iversn,yrflag,kkin)
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
      print*,'hsk ---- @getdat4rtofsb, flnm=',flnm
c
      if     (flnm(l-1:l).eq.'.a' .or. flnm(l-1:l).eq.'.b') then
c ---   HYCOM 2.0 array I/O archive file.
        call getdat4rtofsa(flnm,time,artype,.false.,
     &               initl,icegln,trcout,surflg,
     &               iexpt,iversn,yrflag,kkin,      work)
      endif
      deallocate( util, work )
      return
      end

      subroutine getdat4rtofsa(flnm,time,artype,lrange,initl,icegln,
     &           trcout,
     &           surflg,iexpt,iversn,yrflag,kkin, work)
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
c
      print*,'hsk --- @getdat4rtofsa, flnm=',flnm
c
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

