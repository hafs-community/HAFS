      program sub_topog_panam
      use mod_za  ! HYCOM I/O interface
      use mod_zb  ! HYCOM I/O interface for subregion
      implicit none
c
c     extract a subregion bathymetry from a full region HYCOM 2.0 bathymetry.
c     Special version for GLBa2.00 to GLBb2.00 only.
c
      character*79         :: preambl(5)
      character*80         :: cline,cline_out,cline_dup
      character*128        :: flnm_in,flnm_dup,flnm_out,flnm_ref
      integer              :: idm_out,jdm_out,i0_out,j0_out,
     &                        biplon,biplat
      integer              :: i,id,icount,j,jd,l,nd,ni,no,nr
      real                 :: hmina,hminb,hmaxa,hmaxb,rtmp
      integer, allocatable :: m_in(:,:),m_out(:,:)
      real,    allocatable :: a_in(:,:),a_out(:,:),a_ref(:,:)
c
      real,    parameter   :: hspval=0.5*2.0**100
c
      call xcspmd
      call zaiost
      call blkdat(idm_out,jdm_out, i0_out,j0_out,biplon,biplat,
     &            flnm_in,flnm_dup,flnm_out,flnm_ref,
     &            cline_out,cline_dup)
      call zbiost(idm_out,jdm_out)
c
      allocate( m_in(idm,jdm), m_out(idm_out,jdm_out) )
      allocate( a_in(idm,jdm), a_out(idm_out,jdm_out),
     &                         a_ref(idm_out,jdm_out) )
c
c     open input and output files.
c
      l  = len_trim(flnm_ref)
      if     (flnm_ref(1:l).eq.'NONE') then
        nr = 0
      else
        nr = 13
        open (unit=nr,file=flnm_ref(1:l-2)//'.b',form='formatted',
     .        status='old',action='read')
        call zbiopf(flnm_ref(1:l-2)//'.a','old', nr)
      endif
c
      ni = 14
      l  = len_trim(flnm_in)
      open (unit=ni,file= flnm_in(1:l-2)//'.b',form='formatted',
     .      status='old',action='read')
      call zaiopf( flnm_in(1:l-2)//'.a','old', ni)
c
      no = 15
      l  = len_trim(flnm_out)
      open (unit=no,file=flnm_out(1:l-2)//'.b',form='formatted',
     .      status='new',action='write')
      call zbiopf(flnm_out(1:l-2)//'.a','new', no)
c
      nd = 16
      l  = len_trim(flnm_dup)
      open (unit=nd,file=flnm_dup(1:l-2)//'.b',form='formatted',
     .      status='new',action='write')
      call zaiopf(flnm_dup(1:l-2)//'.a','new', nd)
c
c     process the archive header
c
      read( ni,'(a79)') preambl
      write(lp,*)
      write(lp,'(a79)') preambl
      write(lp,*)
      call flush(lp)
c
      preambl(1) = cline_dup(1:79)
      write(nd,'(a79)') preambl
      call flush(nd)
c
      if     (nr.ne.0) then
        read( nr,'(a79)') preambl  ! use the reference header
      endif
      preambl(1) = cline_out(1:79)
      write(no,'(a79)') preambl
      call flush(no)
      write(lp,'(a79)') preambl
      write(lp,*)
      call flush(lp)
c
c     input the full domain bathymetry.
c
      read( ni,'(a)') cline
      l = index(cline,'=')
      read (cline(l+1:),*)  hminb,hmaxb
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &        abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
        write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &    'error - full domain .a and .b files not consistent:',
     &    '.a,.b min = ',hmina,hminb,hmina-hminb,
     &    '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
        stop
      endif
c
c     duplicate any ocean within the arctic patch.
c
      do j= biplat,jdm
        jd = biplat + idm/2 - (j+1-biplat)
        if     (jd.le.jdm) then
          do i= 1,idm
            if     (a_in(i,j).lt.hspval) then
              if     (i.ge.biplon) then
                id = biplon - (i+1-biplon)
              else
                id = biplon - (idm+i+1-biplon)
              endif
              id = mod(id-1+2*idm,idm)+1
              a_in(id,jd) = a_in(i,j)
c
              write(6,'(a,4i5)') 'i,j,id,jd = ',i,j,id,jd
            endif
          enddo
        endif
      enddo
c
      call zaiowr(a_in,m_in,.false., hmina,hmaxa, nd, .false.)
      write(nd,'(a,2f10.3)') 'min,max depth = ',hmina,hmaxa
      call flush(no)
c
      if     (nr.ne.0) then
c
c       input the subregion reference bathymetry.
c
        read( nr,'(a)') cline
        l = index(cline,'=')
        read (cline(l+1:),*)  hminb,hmaxb
        call zbiord(a_ref,m_out,.false., hmina,hmaxa, nr)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - reference .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          stop
        endif
c
        icount = 0
        do j= 1,jdm_out
          jd = j0_out+j
          do i= 1,idm_out
            if     (a_ref(i,j).lt.hspval) then
              id = mod(i0_out+i-1+2*idm,idm)+1
              m_out(i,j) = 1
              a_out(i,j) = a_in(id,jd)
              if     (a_out(i,j).ne.a_ref(i,j)) then
                icount = icount + 1
              endif
            else
              m_out(i,j) = 0
              a_out(i,j) = 0.0
            endif
          enddo
        enddo
c
        call zbiowr(a_out,m_out,.true., hmina,hmaxa, no, .false.)
        write(no,'(a,2f10.3)') 'min,max depth = ',hmina,hmaxa
        call flush(no)
        write(lp,'(a,2f10.3)') 'min,max depth = ',hmina,hmaxa
        write(lp,'(/a,i8/)')   'differences between ref and out =',
     &                         icount
        call flush(lp)
      else  !nr==0
        do j= 1,jdm_out
          jd = j0_out+j
          do i= 1,idm_out
            id = mod(i0_out+i-1+2*idm,idm)+1
            if     (a_in(id,jd).lt.hspval) then
              m_out(i,j) = 1
              a_out(i,j) = a_in(id,jd)
            else
              m_out(i,j) = 0
              a_out(i,j) = 0.0
            endif
          enddo
        enddo
c
        call zbiowr(a_out,m_out,.true., hmina,hmaxa, no, .false.)
        write(no,'(a,2f10.3)') 'min,max depth = ',hmina,hmaxa
        call flush(no)
        write(lp,'(a,2f10.3)') 'min,max depth = ',hmina,hmaxa
        write(lp,'(/a,i8/)')   'no reference bathymetry'
        call flush(lp)
      endif
c
      end program sub_topog_panam

      subroutine blkdat(idm_out,jdm_out, i0_out,j0_out,biplon,biplat,
     &                  flnm_in,flnm_dup,flnm_out,flnm_ref,
     &                  cline_out,cline_dup)
      use mod_xc  ! HYCOM communication interface
      implicit none
      integer       :: idm_out,jdm_out,i0_out,j0_out,biplon,biplat
      character*128 :: flnm_in,flnm_dup,flnm_out,flnm_ref
      character*80  :: cline_out,cline_dup
c
c --- read blkdat.input for topog subregion.
c
      integer       :: irefi,irefo,jrefi,jrefo
c
c --- 'flnm_in'   = input  full region bathymetry filename
c --- 'flnm_dup'  = output full region bathymetry filename
c --- 'flnm_ref'  = input   sub-region bathymetry filename
c --- 'flnm_out'  = output  sub-region bathymetry filename
c --- 'cline_out' = output title line (replaces preambl(5))
c --- 'cline_dup' = output title line (replaces preambl(5))
c
      read( *,'(a)') flnm_in
      write(6,'(a)') flnm_in
      read( *,'(a)') flnm_dup
      write(6,'(a)') flnm_dup
      read( *,'(a)') flnm_ref
      write(6,'(a)') flnm_ref
      read( *,'(a)') flnm_out
      write(6,'(a)') flnm_out
      read( *,'(a)') cline_out
      write(6,'(a)') cline_out
      read( *,'(a)') cline_dup
      write(6,'(a)') cline_dup
      write(6,*)
      call flush(6)
c
c --- 'idm   ' = longitudinal array size
c --- 'jdm   ' = latitudinal  array size
c --- 'irefi ' = longitudinal input  reference location
c --- 'jrefi ' = latitudinal  input  reference location
c --- 'irefo ' = longitudinal output reference location
c --- 'jrefo ' = latitudinal  output reference location
c --- 'biplon' = longitudinal patchseam grid point on pressure grid
c --- 'biplat' = latitudinal  patchseam grid point on pressure grid
c
      call blkini(idm_out,   'idm   ')
      call blkini(jdm_out,   'jdm   ')
      call blkini(irefi,     'irefi ')
      call blkini(jrefi,     'jrefi ')
      call blkini(irefo,     'irefo ')
      call blkini(jrefo,     'jrefo ')
      call blkini(biplon,    'biplon')
      call blkini(biplat,    'biplat')
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
