      subroutine getdtm(flnm,time,initl, thbase)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
c
      character*(*)    flnm
      double precision time
      logical          initl
      real             thbase
c
c --- read model fields and extract portion of global fields.
c --- (no time-averaged fluxes in this version)
c --- CGS micom input to MKS hycomproc, with grid rotation
c
      character cline*80,text*8,preambl(5)*79
      real      hmina,hminb,hmaxa,hmaxb
c
      character, allocatable :: util(:)*2
      real,      allocatable :: work(:,:),wrk2(:,:)
      integer,   allocatable :: mask(:,:)
c
      data thref/1./,ni/14/
c
      allocate( util(idm*jdm+14) )
      allocate( work(idm,jdm)    )
      allocate( wrk2(idm,jdm)    )
      allocate( mask(idm,jdm)    )
c
      open (unit=ni,file=flnm,form='formatted',
     .      status='old',action='read')
c
      ctitle(1) = 'MICOM archive converted to HYCOM 2.0 '
      ctitle(2) = ' '
      ctitle(3) = ' '
      ctitle(4) = ' '
      do 14 k=1,kk
      read (ni, '(3x,a,a)',end=6) text,cline
      read (cline,       *,end=6) nstep,time,thet,i,j,lgth
*     read (ni,'(1x,64a2)',end=6) (util(l),l=1,lgth)
      read (ni,'(   40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
 100  format (' ::',a8,i8,f8.1,f8.3,2i5,i9,1x,14a2)
      if     (text(1:8).ne.'u-vel.  ') then
        write(lp,*)
        write(lp,*) 'error in getdtm - layer ',k,
     .             ' does not exist (kk= ',kk,')'
        write(lp,*)
        call flush(lp)
        call clsgks
        stop 9
      endif
      call unpakk(  work(jdm-i,1),jdm,i,j,util,lgth)
      call extrot_v(work,idm,jdm,iorign,jorign,
     &              v(1,1,2*k),ii,jj,-0.01, wrk2)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'v       ',k
c
      read (ni, '(3x,a,a)',end=6) text,cline
      read (cline,       *,end=6) nstep,time,thet,i,j,lgth
*     read (ni,'(1x,64a2)',end=6) (util(l),l=1,lgth)
      read (ni,'(   40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,jdm,i,j,util,lgth)
      call extrot_u(work,idm,jdm,iorign,jorign,
     &              u(1,1,2*k),ii,jj, 0.01, wrk2)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'u       ',k
c
      read (ni, '(3x,a,a)',end=6) text,cline
      read (cline,       *,end=6) nstep,time,thet,i,j,lgth
*     read (ni,'(1x,64a2)',end=6) (util(l),l=1,lgth)
      read (ni,'(   40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,jdm,i,j,util,lgth)
      call extrot_p(work,idm,jdm,iorign,jorign,
     &              dp(1,1,k),ii,jj,0.1, wrk2)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'dp      ',k
c
      read (ni, '(3x,a,a)',end=6) text,cline
      read (cline,       *,end=6) nstep,time,thet,i,j,lgth
*     read (ni,'(1x,64a2)',end=6) (util(l),l=1,lgth)
      read (ni,'(   40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,jdm,i,j,util,lgth)
      call extrot_p(work,idm,jdm,iorign,jorign,
     &              temp(1,1,2*k),ii,jj,1.0, wrk2)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'temp    ',k
c
      read (ni, '(3x,a,a)',end=6) text,cline
      read (cline,       *,end=6) nstep,time,thet,i,j,lgth
*     read (ni,'(1x,64a2)',end=6) (util(l),l=1,lgth)
      read (ni,'(   40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,jdm,i,j,util,lgth)
      call extrot_p(work,idm,jdm,iorign,jorign,
     &              saln(1,1,2*k),ii,jj,1.0, wrk2)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'saln    ',k
c
      read (ni, '(3x,a,a)',end=6) text,cline
      read (cline,       *,end=6) nstep,time,thet,i,j,lgth
*     read (ni,'(1x,64a2)',end=6) (util(l),l=1,lgth)
      read (ni,'(   40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,jdm,i,j,util,lgth)
      if (k.eq.1) then
      call extrot_p(work,idm,jdm,iorign,jorign,
     &              th3d(1,1,2*k),ii,jj,1000.0, wrk2)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'th3d    ',k
      do j=1,jj1
        do i=1,ii1
          th3d(i,j,2*k) = th3d(i,j,2*k) - thbase
        enddo
      enddo
      else
      do j=1,jj1
        do i=1,ii1
          th3d(i,j,2*k) = thet - thbase
        enddo
      enddo
      call extrot_p(work,idm,jdm,iorign,jorign,
     &              tracer(1,1,k),ii,jj,1.0, wrk2)
      write(lp,'("unpakk ",a," into ",a,i3)') text(1:8),'tracer  ',k
      endif
c
      write(lp,'(a,f9.5)') 'finished reading data for layer',thet
      call flush(lp)
      theta(k)=thet
 14   continue
c
c --- now unpack barotropic velocity field
c
      read (ni, '(3x,a,a)',end=6) text,cline
      read (cline,       *,end=6) nstep,time,thet,i,j,lgth
*     read (ni,'(1x,64a2)',end=6) (util(l),l=1,lgth)
      read (ni,'(   40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work(jdm-i,1),jdm,i,j,util,lgth)
      call extrot_v(work,idm,jdm,iorign,jorign,
     &              vbaro,ii,jj,-0.01, wrk2)
      write(lp,'("unpakk ",a," into ",a)') text(1:8),'vbaro   '
c
      read (ni, '(3x,a,a)',end=6) text,cline
      read (cline,       *,end=6) nstep,time,thet,i,j,lgth
*     read (ni,'(1x,64a2)',end=6) (util(l),l=1,lgth)
      read (ni,'(   40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,jdm,i,j,util,lgth)
      call extrot_u(work,idm,jdm,iorign,jorign,
     &              ubaro,ii,jj, 0.01, wrk2)
      write(lp,'("unpakk ",a," into ",a)') text(1:8),'ubaro   '
c
      read (ni, '(3x,a,a)',end=6) text,cline
      read (cline,       *,end=6) nstep,time,thet,i,j,lgth
*     read (ni,'(1x,64a2)',end=6) (util(l),l=1,lgth)
      read (ni,'(   40a2)',end=6) (util(l),l=1,lgth)
      write(lp,100) text(1:8),nstep,time,thet,i,j,lgth,(util(l),l=1,14)
      call unpakk(  work,jdm,i,j,util,lgth)
      call extrot_p(work,idm,jdm,iorign,jorign,
     &              srfht,ii,jj,0.0001, wrk2)
      write(lp,'("unpakk ",a," into ",a)') text(1:8),'srfht   '
c
      close(unit=ni)
c
      if (initl) then
c ---   acquire (hycom) basin depths and land/sea mask
        call getdepth(work)
      end if
c
c     layer 1 is the mixed layer.
c
      do j=1,jj1
        do i=1,ii1
          dpbl(  i,j) = dp(  i,j,1)
          dpmixl(i,j) = dp(  i,j,1)
          tmix(  i,j) = temp(i,j,2)
          smix(  i,j) = saln(i,j,2)
          thmix( i,j) = th3d(i,j,2)
          umix(  i,j) = u(   i,j,2)
          vmix(  i,j) = v(   i,j,2)
        enddo
      enddo
c
      write(lp,'(a)') 'shown below: sea surface height'
      call zebra(srfht,ii,ii1,jj1)
      call flush(lp)
c
      deallocate( util, work, wrk2, mask )
c
      return
c
c --- unexpected end of file.
 6    continue
      call flush(lp)
      call clsgks
      stop '(e-o-f)'
      end
