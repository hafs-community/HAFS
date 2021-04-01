      program half_topog
      use mod_za  ! HYCOM I/O interface
      use mod_zb  ! HYCOM I/O interface for subregion
      implicit none
c
c     create a coarser-grid subregion bathymetry from
c     a full region HYCOM 2.0 bathymetry.
c
c     subregion grid must be 2x or 3x coarser than the original grid.
c     subregion p(1,1) must be on the original p-grid.
c
      character*79         :: preambl(5)
      character*80         :: cline,cline_out
      character*128        :: flnm_in,flnm_out
      integer              :: idm_out,jdm_out,jdm_max,
     &                        iref_out,jref_out,iref_in,jref_in,ijgrd,
     &                        nsea
      logical              :: larctic
      integer              :: i,icount,ii,ijland,im1,ip1,j,jj,l,ni,no
      real                 :: hmina,hminb,hmaxa,hmaxb
      integer              :: m_p(-1:1,-1:1)
      real                 :: a_p(-1:1,-1:1)
      integer, allocatable :: m_in(:,:),m_out(:,:)
      real,    allocatable :: a_in(:,:),a_out(:,:)
c
      real,    parameter   :: hspval=0.5*2.0**100
c
      call xcspmd
      call zaiost
      call blkdat(idm_out,jdm_out,
     &            iref_out,jref_out,iref_in,jref_in, ijgrd, nsea,
     &            flnm_in,flnm_out,cline_out)
      call zbiost(idm_out,jdm_out)
c
c     jdm_max needed for arctic dipole patches.
c
      jdm_max = max( jdm, jref_in + (jdm_out-jref_out)*ijgrd + 1 )
      write(6,'(/a,2i6)') 'jdm,jdm_max = ',jdm,jdm_max
c
      allocate( m_in(idm,jdm_max), m_out(idm_out,jdm_out) )
      allocate( a_in(idm,jdm_max), a_out(idm_out,jdm_out) )
c
c     open input and output files.
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
c     process the archive header
c
      read( ni,'(a79)') preambl
      write(6,*)
      write(6,'(a79)') preambl
      write(6,*)
      call flush(6)
c
      preambl(1) = cline_out(1:79)
      write(preambl(2),'(a,2i5)')
     &        'i/jdm =',
     &       idm_out,jdm_out
      preambl(3) = ' '
      preambl(4) = ' '
      preambl(5) = ' '
      write(no,'(a79)') preambl
      call flush(no)
      write(6,'(a79)') preambl
      write(6,*)
      call flush(6)
c
c     input the full domain bathymetry.
c
      read( ni,'(a)') cline
      l = index(cline,'=')
      read (cline(l+1:),*)  hminb,hmaxb
      call zaiord(a_in(1:idm,1:jdm),m_in(1:idm,1:jdm),.false.,
     &            hmina,hmaxa, ni)
      if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &        abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
        write(6,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &    'error - full domain .a and .b files not consistent:',
     &    '.a,.b min = ',hmina,hminb,hmina-hminb,
     &    '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
        stop
      endif
c
      do j= 1,jdm
        do i= 1,idm
          if     (a_in(i,j).gt.hspval .or.
     &            a_in(i,j).le.0.0        ) then
            a_in(i,j) = 0.0
            m_in(i,j) = 0
          else
            m_in(i,j) = 1
          endif
        enddo
      enddo
c
      larctic = maxval(m_in(:,jdm)).eq.1  !sea means an arctic dipole patch
c
      if     (larctic) then
        write(6,'(/a)') ' input has an arctic dipole patch'
        do j= jdm+1,jdm_max
          jj = jdm-1-(j-jdm)
          do i= 1,idm
            ii = idm-mod(i-1,idm)
            a_in(i,j) = a_in(ii,jj)
            m_in(i,j) = m_in(ii,jj)
          enddo
        enddo
      endif
c
c     calculate the output land/sea mask and bathymetry.
c
      do jj= 1,jdm_out
        j = jref_in + (jj-jref_out)*ijgrd
        if     (j.le.1 .or. (j.ge.jdm .and. .not.larctic)) then  !land
          do ii= 1,idm_out
            m_out(ii,jj) = 0
            a_out(ii,jj) = 0.0
          enddo
*         write(6,'(a,2i5)') 'jj,j,land   =',
*    &                        jj,j
        else
*         write(6,'(a,3i5)') 'jj,j,idm_out=',
*    &                        jj,j,idm_out
          do ii= 1,idm_out
            i = mod(iref_in + (ii-iref_out)*ijgrd + idm-1, idm) + 1
*           write(6,'(a,4i5,i3)') 'ii,jj,i,j,m_in   =',
*    &                             ii,jj,i,j,m_in(i,j)
            if     (m_in(i,j).eq.0) then  !always land where input is land
              m_out(ii,jj) = 0
              a_out(ii,jj) = 0.0
              cycle
            endif
            if     (i.eq.  1) then  !periodic also works for closed domains
              im1 = idm
              ip1 = i+1
            elseif (i.eq.idm) then  !periodic also works for closed domains
              im1 = i-1
              ip1 =   1
            else
              im1 = i-1
              ip1 = i+1
            endif
            m_p(-1,-1) = m_in(im1,j-1)
            m_p( 0,-1) = m_in(i,  j-1)
            m_p(+1,-1) = m_in(ip1,j-1)
            m_p(-1, 0) = m_in(im1,j)  
            m_p( 0, 0) = m_in(i,  j)  
            m_p(+1, 0) = m_in(ip1,j)  
            m_p(-1,+1) = m_in(im1,j+1)
            m_p( 0,+1) = m_in(i,  j+1)
            m_p(+1,+1) = m_in(ip1,j+1)
            icount = sum(m_p(:,:))
*           write(6,'(a,4i5,i3)') 'ii,jj,i,j,icount =',
*    &                             ii,jj,i,j,icount
c
            if     (icount.lt.nsea) then !land
              m_out(ii,jj) = 0
              a_out(ii,jj) = 0.0
            elseif (ijgrd.eq.2) then  !ocean, use colocated depth
              m_out(ii,jj) = 1
              a_out(ii,jj) = a_in(i,j)
            else  !(ijgrd.eq.3) then  !ocean, 3x3 average depth
              a_p(-1,-1) = a_in(im1,j-1)
              a_p( 0,-1) = a_in(i,  j-1)
              a_p(+1,-1) = a_in(ip1,j-1)
              a_p(-1, 0) = a_in(im1,j)  
              a_p( 0, 0) = a_in(i,  j)  
              a_p(+1, 0) = a_in(ip1,j)  
              a_p(-1,+1) = a_in(im1,j+1)
              a_p( 0,+1) = a_in(i,  j+1)
              a_p(+1,+1) = a_in(ip1,j+1)
              m_out(ii,jj) = 1
              a_out(ii,jj) = sum(a_p(:,:)*m_p(:,:))/icount
            endif
*           write(6,'(a,4i5,f8.2)') 'ii,jj,i,j,a_out  =',
*    &                               ii,jj,i,j,a_out(ii,jj)
          enddo
        endif !j.le.1.or.j.ge.jdm:else
      enddo !jj
c
c     exact match required across output arctic dipole patch
c
      if     (larctic) then
        if     (abs(jdm-jdm_max).lt.ijgrd*2) then
          write(6,'(a/)') 'output has an arctic dipole patch'
          do i= 1,idm_out
            ii = idm_out-mod(i-1,idm_out)
            m_out(i,jdm_out) = m_out(ii,jdm_out-1)
            a_out(i,jdm_out) = a_out(ii,jdm_out-1)
          enddo
        else
          write(6,'(/a/)') 'output has a closed northern boundary'
          do i= 1,idm_out
            m_out(i,jdm_out) = 0
            a_out(i,jdm_out) = 0.0
          enddo
        endif
      endif !larctic
c
c     output the resulting bathymetry.
c
      call zbiowr(a_out,m_out,.true., hmina,hmaxa, no, .false.)
      write(no,'(a,2f10.3)') 'min,max depth = ',hmina,hmaxa
      call flush(no)
      write(6,'(a,2f10.3)') 'min,max depth = ',hmina,hmaxa
      call flush(6)
c
      end program half_topog

      subroutine blkdat(idm_out,jdm_out,
     &                   iref_out,jref_out,iref_in,jref_in,ijgrd, nsea,
     &                   flnm_in,flnm_out,cline_out)
      use mod_xc  ! HYCOM communication interface
      implicit none
c
      integer       :: idm_out,jdm_out,
     &                 iref_out,jref_out,iref_in,jref_in, ijgrd, nsea
      character*128 :: flnm_in,flnm_out
      character*80  :: cline_out
c
c --- read blkdat.input for topog interpolating subregion.
c
c --- 'flnm_in'   = input  full region bathymetry filename
c --- 'flnm_out'  = output  sub-region bathymetry filename
c --- 'cline_out' = output title line (preambl(1))
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
c --- 'ijgrd ' = integer scale factor between input and output grids
c --- 'nsea  ' = number of input sea points in 3x3 box for sea output
c
      call blkini(idm_out,   'idm   ')
      call blkini(jdm_out,   'jdm   ')
      call blkini(iref_in,   'irefi ')
      call blkini(jref_in,   'jrefi ')
      call blkini(iref_out,  'irefo ')
      call blkini(jref_out,  'jrefo ')
      call blkini(ijgrd,     'ijgrd ')
      call blkini(nsea,      'nsea  ')
c
      if     (ijgrd.ne.2 .and. ijgrd.ne.3) then
        write(6,'(/a,a/)')
     &    '***** error - ',
     &    'ijgrd must be 2 or 3 *****'
        call flush(6)
        stop
      endif
c
      if     (nsea.lt.1 .or. nsea.gt.9) then
        write(6,'(/a,a/)')
     &    '***** error - ',
     &    'nsea must be between 1 and 9 *****'
        call flush(6)
        stop
      endif
c
      return
 6000 format('blkdat: ',a6,' =',i6)
      end subroutine blkdat

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
