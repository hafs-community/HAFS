      program subregion
      use mod_za  ! HYCOM I/O interface
      use mod_zb  ! HYCOM I/O interface for subregion
      implicit none
c
c     extract a subregion from a full region HYCOM 2.0 archive file.
c
      character*80         :: cline,cline_out
      character*128        :: flnm_in,flnm_out
      integer              :: idm_out,jdm_out,i0_out,j0_out
      integer              :: i,id,ios,itmp,j,jd,l,ni,no
      logical              :: icegln
      real                 :: hmina,hminb,hmaxa,hmaxb,rtmp
      integer, allocatable :: m_in(:,:),m_out(:,:)
      real,    allocatable :: a_in(:,:),a_out(:,:)
c
      call xcspmd
      call zaiost
      call blkdat(idm_out,jdm_out, i0_out,j0_out, icegln,
     &                             flnm_in,flnm_out,cline_out)
      call zbiost(idm_out,jdm_out)
c
      allocate( m_in(idm,jdm), m_out(idm_out,jdm_out) )
      allocate( a_in(idm,jdm), a_out(idm_out,jdm_out) )
c
c     open input and output files.
c
      ni = 14
      l  = len_trim(flnm_in)
      open (unit=ni,file=flnm_in(1:l-2)//'.b',form='formatted',
     .      status='old',action='read')
      call zaiopf(flnm_in(1:l-2)//'.a','old', ni)
c
      no = 15
      l  = len_trim(flnm_out)
      open (unit=no,file=flnm_out(1:l-2)//'.b',form='formatted',
     .      status='new',action='write')
      call zbiopf(flnm_out(1:l-2)//'.a','new', no)
c
c     process the archive header
c
      read( ni,'(a)') cline  ! ctitle(1)
      write(no,'(a)') cline
      write(lp,'(a)') cline
c
      read( ni,'(a)') cline  ! ctitle(2)
      write(no,'(a)') cline
      write(lp,'(a)') cline
c
      read( ni,'(a)') cline  ! ctitle(3)
      write(no,'(a)') cline
      write(lp,'(a)') cline
c
      read( ni,'(a)') cline  ! ctitle(4), replace with cline_out
      write(no,'(a)') cline_out
      write(lp,'(a)') cline_out
c
      read( ni,'(a)') cline  ! iversn
      write(no,'(a)') cline
      write(lp,'(a)') cline
c
      read( ni,'(a)') cline  ! iexpt 
      write(no,'(a)') cline
      write(lp,'(a)') cline
c
      read( ni,'(a)') cline  ! yrflag
      write(no,'(a)') cline
      write(lp,'(a)') cline
c
      read( ni,'(a)') cline  ! idm, replace with idm_out
      write(no,'(i5,4x,a)') idm_out,"'idm   ' = longitudinal array size"
      write(lp,'(i5,4x,a)') idm_out,"'idm   ' = longitudinal array size"
c
      read( ni,'(a)') cline  ! jdm, replace with jdm_out
      write(no,'(i5,4x,a)') jdm_out,"'jdm   ' = latitudinal  array size"
      write(lp,'(i5,4x,a)') jdm_out,"'jdm   ' = latitudinal  array size"
c
      read( ni,'(a)') cline  ! field ...
      write(no,'(a)') cline
      write(lp,'(a)') cline
c
      call flush(no)
      call flush(lp)
c
c     loop through all 2-d fields in archive file.
c
      do
        read( ni,'(a)',iostat=ios) cline
        if     (ios.ne.0) then
          exit
        endif
c
        l = index(cline,'=')
        read (cline(l+1:),*)  itmp,rtmp,itmp,rtmp,hminb,hmaxb
        call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          stop
        endif
c
        if     (.not. icegln .and.
     &          (cline(1:8).eq.'covice  ' .or.
     &           cline(1:8).eq.'thkice  ' .or.
     &           cline(1:8).eq.'temice  '     )) then
          cycle  ! no ice output
        endif
c
        if     (idm_out.ne.2 .or.jdm_out.ne.2) then
          do j= 1,jdm_out
            jd = j0_out+j
            do i= 1,idm_out
              id = mod(i0_out+i-1+2*idm,idm)+1  ! allow periodic wrap
              a_out(i,j) = a_in(id,jd)
            enddo
          enddo
        else  ! 1-D case.
          jd = j0_out+1
          id = i0_out+1
          do j= 1,jdm_out
            do i= 1,idm_out
              a_out(i,j) = a_in(id,jd)
            enddo
          enddo
        endif
c
        call zbiowr(a_out,m_out,.false., hmina,hmaxa, no, .false.)
        write(no,'(a,1p2e16.7)') cline(1:41),hmina,hmaxa
        call flush(no)
        write(lp,'(a,1p2e16.7)') cline(1:41),hmina,hmaxa
        call flush(lp)
      enddo
c
      end program subregion

      subroutine blkdat(idm_out,jdm_out, i0_out,j0_out, icegln,
     &                  flnm_in,flnm_out,cline_out)
      use mod_xc  ! HYCOM communication interface
      implicit none
c
      integer       :: idm_out,jdm_out,i0_out,j0_out
      logical       :: icegln
      character*128 :: flnm_in,flnm_out
      character*80  :: cline_out
c
c --- read blkdat.input for subregion.
c
      integer       :: iceflg,irefi,irefo,jrefi,jrefo
c
c --- 'flnm_in'   = input  filename
c --- 'flnm_out'  = output filename
c --- 'cline_out' = output title line (replaces preambl(5))
c
      read( *,'(a)') flnm_in
      write(6,'(a)') flnm_in
      read( *,'(a)') flnm_out
      write(6,'(a)') flnm_out
      read( *,'(a)') cline_out
      write(6,'(a)') cline_out
      write(6,*)
      call flush(6)
c
c --- 'idm   ' = longitudinal array size
c --- 'jdm   ' = latitudinal  array size
c --- 'irefi ' = longitudinal input  reference location
c --- 'jrefi ' = latitudinal  input  reference location
c --- 'irefo ' = longitudinal output reference location
c --- 'jrefo ' = latitudinal  output reference location
c
      call blkini(idm_out,   'idm   ')
      call blkini(jdm_out,   'jdm   ')
      call blkini(irefi,     'irefi ')
      call blkini(jrefi,     'jrefi ')
      call blkini(irefo,     'irefo ')
      call blkini(jrefo,     'jrefo ')
c
      i0_out = irefi - irefo
      j0_out = jrefi - jrefo
c
      write(6,*)
      write(6,6000) 'i0    ',i0_out
      write(6,6000) 'j0    ',j0_out
      write(6,*)
      call flush(6)
c
c --- 'iceflg' = ice in output archive flag (0=none,1=energy loan model)
      call blkini(iceflg,'iceflg')
      icegln = iceflg.eq.1
c
      return
 6000 format('blkdat: ',a6,' =',i6)
      end subroutine blkdat

      subroutine blkinr(rvar,cvar,cfmt)
      implicit none
c
      real      rvar
      character cvar*6,cfmt*(*)
c
c     read in one real value
c
      character*6 cvarin
c
      read( *,*) rvar,cvarin
      write(6,cfmt) cvarin,rvar
      call flush(6)
c
      if     (cvar.ne.cvarin) then
        write(6,*) 
        write(6,*) 'error in blkinr - input ',cvarin,
     +                      ' but should be ',cvar
        write(6,*) 
        call flush(6)
        stop
      endif
      return
      end subroutine blkinr

      subroutine blkini(ivar,cvar)
      implicit none
c
      integer     ivar
      character*6 cvar
c
c     read in one integer value
c
      character*6 cvarin
c
      read( *,*) ivar,cvarin
      write(6,6000) cvarin,ivar
      call flush(6)
c
      if     (cvar.ne.cvarin) then
        write(6,*) 
        write(6,*) 'error in blkini - input ',cvarin,
     +                      ' but should be ',cvar
        write(6,*) 
        call flush(6)
        stop
      endif
      return
 6000 format(a6,' =',i6)
      end subroutine blkini

      subroutine blkinl(lvar,cvar)
      implicit none
c
      logical     lvar
      character*6 cvar
c
c     read in one logical value
c     due to a SGI bug for logical I/O: read in an integer 0=F,1=T
c
      character*6 cvarin
      integer     ivar
c
      read( *,*) ivar,cvarin
      lvar = ivar .ne. 0
      write(6,6000) cvarin,lvar
      call flush(6)
c
      if     (cvar.ne.cvarin) then
        write(6,*) 
        write(6,*) 'error in blkinr - input ',cvarin,
     +                      ' but should be ',cvar
        write(6,*) 
        call flush(6)
        stop
      endif
      return
 6000 format(a6,' =',l6)
      end subroutine blkinl
