      subroutine getdepth(work)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      real work(idm,jdm)
c
c --- acquire basin depths and land/sea mask (if any)
c
      character cline*80
      character preambl(5)*79
      character cvarin*6
      real      alon,hmina,hmaxa,hminb,hmaxb
      integer   i,j,iversn,ios,l
      logical   lexist
c
c     basin depth.
c
      open (unit=9,file=dpthfil(1:len_trim(dpthfil))//'.b',
     &      form='formatted',status='old',action='read')
      read (9, '(a79)') preambl
      write(lp,'(a79)') preambl
      read (9, '(a)')   cline
      write(lp,'(a)')   cline(1:len_trim(cline))
      call flush(lp)
      i = index(cline,'=')
      read (cline(i+1:),*)   hminb,hmaxb
      close(unit=9)
c
      call zaiopf(dpthfil(1:len_trim(dpthfil))//'.a','old', 9)
      call zaiord(work,ip,.false., hmina,hmaxa, 9)
      call zaiocl(9)
      if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &        abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
        write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &    'error - .a and .b files not consistent:',
     &    '.a,.b min = ',hmina,hminb,hmina-hminb,
     &    '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
        call flush(lp)
        stop
      endif
      do j= 1,jdm
        do i= 1,idm
          if     (work(i,j).gt.2.0**99) then
            work(i,j) = 0.0
          endif
        enddo
      enddo
      if     (iorign.gt.1 .and. jorign.gt.1) then
        call extrct_p(work,idm,jdm,iorign-1,jorign-1, 
     &                depths,ii+1,jj+1)
      elseif (iorign.gt.1 .and. jorign.eq.1) then 
        call extrct_p(work,idm,jdm,iorign-1,jorign, 
     &                depths(0,1),ii+1,jj)
        depths(:,0) = 0.0
      elseif (iorign.eq.1 .and. jorign.gt.1) then 
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                coast,ii,jj)
        depths(1:ii,1:jj) = coast(1:ii,1:jj)
        call extrct_p(work,idm,jdm,iorign,jorign-1, 
     &                coast,ii,1)
        depths(1:ii,0:0 ) = coast(1:ii,1:1)
        if     (ii.eq.idm) then
          depths(0,:) = depths(ii,:)  ! assumed periodic
        else
          depths(0,:) = 0.0
        endif
      else  !(iorign.eq.1 .and. jorign.eq.1)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                coast,ii,jj)
        depths(1:ii,1:jj) = coast(1:ii,1:jj)
        if     (ii.eq.idm) then
          depths(0,:) = depths(ii,:)  ! assumed periodic
          depths(:,0) = 0.0
        else
          depths(0,:) = 0.0
          depths(:,0) = 0.0
        endif
      endif
      write(lp,'("read ",a," into ",a)') preambl(1)(1:8),'depths  '
      call flush(lp)
c
c     basin land/sea mask (0.0 for land, 1.0 for sea).
c
      inquire(file='regional.mask.a',exist=lexist)
      if     (.not.lexist) then  ! from depths
        do j= 1,jj
          do i= 1,ii
            if     (depths(i,j).gt.0.0) then
              coast(i,j) = 1.0  ! sea
            else
              coast(i,j) = 0.0  ! land
            endif
          enddo
        enddo
        write(lp,'("mask ",a," into ",a)') preambl(1)(1:8),'coast   '
        call flush(lp)
      else
        call zaiopf('regional.mask.a','old', 9)
        call zaiord(work,ip,.false., hmina,hmaxa, 9)
        call zaiocl(9)
        call extrct_p(work,idm,jdm,iorign,jorign, 
     &                coast,ii,jj)
        do j= 1,jj
          do i= 1,ii
            if     (depths(i,j).gt.0.0) then  ! model-sea
              coast(i,j) = 1.0  ! sea
            endif
          enddo
        enddo
        write(lp,'("read ",a," into ",a)') 'land/sea','coast   '
        call flush(lp)
      endif
c
c     grid location.
c
      open (unit=9,file='regional.grid.b',
     &      form='formatted',status='old',action='read')
      call zaiopf('regional.grid.a','old', 9)
c
      read(9,*) ! skip idm
      read(9,*) ! skip jdm
      read(9,*) ! skip mapflg
      read(  9,'(a)') cline
      write(lp,'(a)') cline(1:len_trim(cline))
      call flush(lp)
      i = index(cline,'=')
      read(cline(i+1:),*) hminb,hmaxb
      call zaiord(work,ip,.false., hmina,hmaxa, 9)
      if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &        abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
        write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &    'error - .a and .b files not consistent:',
     &    '.a,.b min = ',hmina,hminb,hmina-hminb,
     &    '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
        call flush(lp)
        stop
      endif
      call extrct_p(work,idm,jdm,iorign,jorign, plon,ii,jj)
c
c --- longitude needs to be monotonically increasing for netCDF
c
      do j= 1,jj
        alon = mod(plon(1,j)+1080.0,360.0)
        if     (alon.gt.180.0) then
          alon = alon - 360.0
        endif
        plon(1,j) = alon  !between -180E and 180E
        do i= 2,ii
          alon = mod(plon(i,j)+1080.0,360.0)
          if     (alon.gt.plon(i-1,j)+360.0) then
            plon(i,j) = alon - 360.0
          elseif (alon.lt.plon(i-1,j)-360.0) then
            plon(i,j) = alon + 720.0
          elseif (alon.lt.plon(i-1,j)) then
            plon(i,j) = alon + 360.0
          else
            plon(i,j) = alon
          endif
        enddo !i
      enddo !j
c
      read(  9,'(a)') cline
      write(lp,'(a)') cline(1:len_trim(cline))
      call flush(lp)
      i = index(cline,'=')
      read(cline(i+1:),*) hminb,hmaxb
      call zaiord(work,ip,.false., hmina,hmaxa, 9)
      if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &        abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
        write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &    'error - .a and .b files not consistent:',
     &    '.a,.b min = ',hmina,hminb,hmina-hminb,
     &    '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
        call flush(lp)
        stop
      endif
      call extrct_p(work,idm,jdm,iorign,jorign, plat,ii,jj)
c
c --- need all lon/lat and grid fields for hycomproc only.
c
      if     (allocated(qlon)) then
        read(  9,'(a)') cline
        write(lp,'(a)') cline(1:len_trim(cline))
        call flush(lp)
        i = index(cline,'=')
        read(cline(i+1:),*) hminb,hmaxb
        call zaiord(work,ip,.false., hmina,hmaxa, 9)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          call flush(lp)
          stop
        endif
        call extrct_q(work,idm,jdm,iorign,jorign, qlon,ii,jj)
c
        read(  9,'(a)') cline
        write(lp,'(a)') cline(1:len_trim(cline))
        call flush(lp)
        i = index(cline,'=')
        read(cline(i+1:),*) hminb,hmaxb
        call zaiord(work,ip,.false., hmina,hmaxa, 9)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          call flush(lp)
          stop
        endif
        call extrct_q(work,idm,jdm,iorign,jorign, qlat,ii,jj)
c
        read(  9,'(a)') cline
        write(lp,'(a)') cline(1:len_trim(cline))
        call flush(lp)
        i = index(cline,'=')
        read(cline(i+1:),*) hminb,hmaxb
        call zaiord(work,ip,.false., hmina,hmaxa, 9)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          call flush(lp)
          stop
        endif
        call extrct_us(work,idm,jdm,iorign,jorign, ulon,ii,jj)
c
        read(  9,'(a)') cline
        write(lp,'(a)') cline(1:len_trim(cline))
        call flush(lp)
        i = index(cline,'=')
        read(cline(i+1:),*) hminb,hmaxb
        call zaiord(work,ip,.false., hmina,hmaxa, 9)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          call flush(lp)
          stop
        endif
        call extrct_us(work,idm,jdm,iorign,jorign, ulat,ii,jj)
c
        read(  9,'(a)') cline
        write(lp,'(a)') cline(1:len_trim(cline))
        call flush(lp)
        i = index(cline,'=')
        read(cline(i+1:),*) hminb,hmaxb
        call zaiord(work,ip,.false., hmina,hmaxa, 9)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          call flush(lp)
          stop
        endif
        call extrct_vs(work,idm,jdm,iorign,jorign, vlon,ii,jj)
c
        read(  9,'(a)') cline
        write(lp,'(a)') cline(1:len_trim(cline))
        call flush(lp)
        i = index(cline,'=')
        read(cline(i+1:),*) hminb,hmaxb
        call zaiord(work,ip,.false., hmina,hmaxa, 9)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          call flush(lp)
          stop
        endif
        call extrct_vs(work,idm,jdm,iorign,jorign, vlat,ii,jj)
c
c ---   need pang for archv2datasfl only.
c
        if     (allocated(pang)) then
          read(  9,'(a)') cline
          write(lp,'(a)') cline(1:len_trim(cline))
          call flush(lp)
          i = index(cline,'=')
          read(cline(i+1:),*) hminb,hmaxb
          call zaiord(work,ip,.false., hmina,hmaxa, 9)
          if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &            abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
            write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &        'error - .a and .b files not consistent:',
     &        '.a,.b min = ',hmina,hminb,hmina-hminb,
     &        '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
            call flush(lp)
            stop
          endif
          call extrct_p(work,idm,jdm,iorign,jorign, pang,ii,jj)
        else
          read(  9,'(a)') cline  ! pang
          call zaiosk(9)
        endif
c
        read(  9,'(a)') cline
        write(lp,'(a)') cline(1:len_trim(cline))
        call flush(lp)
        i = index(cline,'=')
        read(cline(i+1:),*) hminb,hmaxb
        call zaiord(work,ip,.false., hmina,hmaxa, 9)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          call flush(lp)
          stop
        endif
        call extrct_p(work,idm,jdm,iorign,jorign, scpx,ii,jj)
c
        read(  9,'(a)') cline
        write(lp,'(a)') cline(1:len_trim(cline))
        call flush(lp)
        i = index(cline,'=')
        read(cline(i+1:),*) hminb,hmaxb
        call zaiord(work,ip,.false., hmina,hmaxa, 9)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          call flush(lp)
          stop
        endif
        call extrct_p(work,idm,jdm,iorign,jorign, scpy,ii,jj)
c
        read(  9,'(a)') cline  ! scqx
        call zaiosk(9)
c
        read(  9,'(a)') cline  ! scqy
        call zaiosk(9)
c
        read(  9,'(a)') cline
        write(lp,'(a)') cline(1:len_trim(cline))
        call flush(lp)
        i = index(cline,'=')
        read(cline(i+1:),*) hminb,hmaxb
        call zaiord(work,ip,.false., hmina,hmaxa, 9)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          call flush(lp)
          stop
        endif
        call extrct_us(work,idm,jdm,iorign,jorign, scux,ii,jj)
c
        read(  9,'(a)') cline
        write(lp,'(a)') cline(1:len_trim(cline))
        call flush(lp)
        i = index(cline,'=')
        read(cline(i+1:),*) hminb,hmaxb
        call zaiord(work,ip,.false., hmina,hmaxa, 9)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          call flush(lp)
          stop
        endif
        call extrct_us(work,idm,jdm,iorign,jorign, scuy,ii,jj)
c
        read(  9,'(a)') cline
        write(lp,'(a)') cline(1:len_trim(cline))
        call flush(lp)
        i = index(cline,'=')
        read(cline(i+1:),*) hminb,hmaxb
        call zaiord(work,ip,.false., hmina,hmaxa, 9)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          call flush(lp)
          stop
        endif
        call extrct_vs(work,idm,jdm,iorign,jorign, scvx,ii,jj)
c
        read(  9,'(a)') cline
        write(lp,'(a)') cline(1:len_trim(cline))
        call flush(lp)
        i = index(cline,'=')
        read(cline(i+1:),*) hminb,hmaxb
        call zaiord(work,ip,.false., hmina,hmaxa, 9)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          call flush(lp)
          stop
        endif
        call extrct_vs(work,idm,jdm,iorign,jorign, scvy,ii,jj)
c
      endif
c
      close(unit=9)
      call zaiocl(9)
      return
      end
