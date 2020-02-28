      program sub_grid
      use mod_za  ! HYCOM I/O interface
      use mod_zb  ! HYCOM I/O interface for subregion
      implicit none
c
c     extract a subregion grid from a full region HYCOM 2.1 regional.grid.
c
      character*79         :: preambl(5)
      character*80         :: cline,cline_out
      character*128        :: flnm_in,flnm_out
      integer              :: idm_out,jdm_out,i1_out,j1_out
      integer              :: i,icount,id,j,jd,l,ni,no,nr
      real                 :: hmina,hminb,hmaxa,hmaxb,rtmp
      integer, allocatable :: m_in(:,:),m_out(:,:)
      real,    allocatable :: a_in(:,:),a_out(:,:)
c
      real,    parameter   :: hspval=0.5*2.0**100
c
      call xcspmd
      call zaiost
      call blkdat(idm_out,jdm_out, i1_out,j1_out,
     &            flnm_in,flnm_out)

      call zbiost(idm_out,jdm_out)
c
      allocate( m_in(idm,jdm), m_out(idm_out,jdm_out) )
      allocate( a_in(idm,jdm), a_out(idm_out,jdm_out) )
c
c     open input and output files.
c
      ni = 14
      l = len_trim(flnm_in)
      call zaiopf( flnm_in(1:l-2)//'.a','old', ni)
      write(6,'(a,a,a,i4)') 
     &   'opened file: ',flnm_in(1:l-2)//'.a','  on unit: ',ni
c
      no = 15
      l  = len_trim(flnm_out)
      open (unit=no,file=flnm_out(1:l-2)//'.b',form='formatted',
     .      status='new',action='write')
      call zbiopf(flnm_out(1:l-2)//'.a','new', no)
      write(6,'(a,a,a,i4)') 
     &   'opened file: ',flnm_out(1:l-2)//'.a','  on unit: ',no
c
c --- write header.
c
      write(no,'(i5,a)')
     &  idm_out,   "    'idm   ' = longitudinal array size"
      write(no,'(i5,a)')
     &  jdm_out,   "    'jdm   ' = latitudinal  array size"
      write(no,'(i5,a,a)')
     &  -1,        "    'mapflg' = map flag",
     &             " (-1=unknown,0=mercator,2=uniform,4=f-plane)"
      call zhflsh(no)
c
      write(6, *)
      write( 6,'(i5,a)')
     &  idm_out,   "    'idm   ' = longitudinal array size"
      write( 6,'(i5,a)')
     &  jdm_out,   "    'jdm   ' = latitudinal  array size"
      write( 6,'(i5,a,a)')
     &  -1,        "    'mapflg' = map flag",
     &             " (-1=unknown,0=mercator,2=uniform,4=f-plane)"
      call zhflsh( 6)
c
c     each array in turn.
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_p(               a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call newlon(                 a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6100) 'plon',hmina,hmaxa
      write(6, 6100) 'plon',hmina,hmaxa
c
      write(6, *)
      do i= 1,idm_out
        write(6,'(a,i5,2f10.3)')
     &    'i,plon =',i,minval(a_out(i,:)),maxval(a_out(i,:))
      enddo
      write(6, *)
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_p(               a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6100) 'plat',hmina,hmaxa
      write(6, 6100) 'plat',hmina,hmaxa
c
      write(6, *)
      do j= 1,jdm_out
        write(6,'(a,i5,2f10.3)')
     &    'j,plat =',j,minval(a_out(:,j)),maxval(a_out(:,j))
      enddo
      write(6, *)
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_q(               a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call newlon(                 a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6100) 'qlon',hmina,hmaxa
      write(6, 6100) 'qlon',hmina,hmaxa
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_q(               a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6100) 'qlat',hmina,hmaxa
      write(6, 6100) 'qlat',hmina,hmaxa
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_us(              a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call newlon(                 a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6100) 'ulon',hmina,hmaxa
      write(6, 6100) 'ulon',hmina,hmaxa
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_us(              a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6100) 'ulat',hmina,hmaxa
      write(6, 6100) 'ulat',hmina,hmaxa
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_vs(              a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call newlon(                 a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6100) 'vlon',hmina,hmaxa
      write(6, 6100) 'vlon',hmina,hmaxa
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_vs(              a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6100) 'vlat',hmina,hmaxa
      write(6, 6100) 'vlat',hmina,hmaxa
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_p(               a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6100) 'pang',hmina,hmaxa
      write(6, 6100) 'pang',hmina,hmaxa
 6100 format(a,':  min,max = ',2f15.5)
      call zhflsh(61)
      call zhflsh(6)
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_p(               a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6110) 'pscx',hmina,hmaxa
      write(6, 6110) 'pscx',hmina,hmaxa
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_p(               a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6110) 'pscy',hmina,hmaxa
      write(6, 6110) 'pscy',hmina,hmaxa
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_q(               a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6110) 'qscx',hmina,hmaxa
      write(6, 6110) 'qscx',hmina,hmaxa
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_q(               a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6110) 'qscy',hmina,hmaxa
      write(6, 6110) 'qscy',hmina,hmaxa
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_us(              a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6110) 'uscx',hmina,hmaxa
      write(6, 6110) 'uscx',hmina,hmaxa
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_us(              a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6110) 'uscy',hmina,hmaxa
      write(6, 6110) 'uscy',hmina,hmaxa
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_vs(              a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6110) 'vscx',hmina,hmaxa
      write(6, 6110) 'vscx',hmina,hmaxa
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_vs(              a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6110) 'vscy',hmina,hmaxa
      write(6, 6110) 'vscy',hmina,hmaxa
      write(6, *)
 6110 format(a,':  min,max = ',2f15.5)
      call zhflsh(61)
      call zhflsh(6)
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_q(               a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6120) 'cori',hmina,hmaxa
      write(6, 6120) 'cori',hmina,hmaxa
      write(6, *)
 6120 format(a,':  min,max = ',2f15.10)
      call zhflsh(61)
      call zhflsh(6)
c
      call zaiord(a_in,m_in,.false., hmina,hmaxa, ni)
      call extrct_p(               a_in, idm,    jdm, 
     &               i1_out,j1_out,a_out,idm_out,jdm_out)
      call zbiowr(a_out,m_out,.false.,hmina,hmaxa, no, .false.)
      write(no,6130) 'pasp',hmina,hmaxa
      write(6, 6130) 'pasp',hmina,hmaxa
      write(6, *)
 6130 format(a,':  min,max = ',2f15.5)
      call zhflsh(6)
c
      close(unit=no)
      call zbiocl(no)
c
      end program sub_grid

      subroutine newlon(plon,idm,jdm)
      implicit none
      integer idm,jdm
      real    plon(idm,jdm)
c
c --- modify plon to reduce jumps on longitude.
c
      integer i,j
      real    plonij
c
      do j= 1,jdm
        do i= 1,idm
          plonij = plon(i,j)
          if     (i.eq.1) then  !1st column
            if     (j.eq.1) then  !1st point
c ---         between -180 and +180
              if     (plonij.gt. 180.0) then
                plonij = plonij-360.0
              elseif (plonij.lt.-180.0) then
                plonij = plonij+360.0
              endif
              if     (plonij.gt. 180.0) then
                plonij = plonij-360.0
              elseif (plonij.lt.-180.0) then
                plonij = plonij+360.0
              endif
            else
c ---         close to previous row
              if     (plonij-plon(i,j-1).gt. 180.0) then
                plonij = plonij-360.0
              elseif (plonij-plon(i,j-1).lt.-180.0) then
                plonij = plonij+360.0
              endif
              if     (plonij-plon(i-1,j).gt. 180.0) then
                plonij = plonij-360.0
              elseif (plonij-plon(i-1,j).lt.-180.0) then
                plonij = plonij+360.0
              endif
            endif
          else
c ---       close to previous column
            if     (plonij-plon(i-1,j).gt. 180.0) then
              plonij = plonij-360.0
            elseif (plonij-plon(i-1,j).lt.-180.0) then
              plonij = plonij+360.0
            endif
            if     (plonij-plon(i-1,j).gt. 180.0) then
              plonij = plonij-360.0
            elseif (plonij-plon(i-1,j).lt.-180.0) then
              plonij = plonij+360.0
            endif
          endif
          plon(i,j) = plonij
        enddo !i
      enddo !j
      return
      end

      subroutine blkdat(idm_out,jdm_out, i1_out,j1_out,
     &                  flnm_in,flnm_out)
      use mod_xc  ! HYCOM communication interface
      implicit none
      integer       :: idm_out,jdm_out,i1_out,j1_out
      character*128 :: flnm_in,flnm_out
c
c --- read blkdat.input for grid subregion.
c
      integer       :: irefi,irefo,jrefi,jrefo
c
c --- 'flnm_in'   = input  full region grid filename
c --- 'flnm_out'  = output  sub-region grid filename
c
      read( *,'(a)')      flnm_in
      write(6,'(a)') trim(flnm_in)
      read( *,'(a)')      flnm_out
      write(6,'(a)') trim(flnm_out)
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
      i1_out = irefi - irefo + 1
      j1_out = jrefi - jrefo + 1
c
      write(6,*)
      write(6,6000) 'i1_out',i1_out
      write(6,6000) 'j1_out',j1_out
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
