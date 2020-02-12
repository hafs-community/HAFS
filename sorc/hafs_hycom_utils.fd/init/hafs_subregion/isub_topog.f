      program isub_topog
      use mod_za  ! HYCOM I/O interface
      use mod_zb  ! HYCOM I/O interface for subregion
      implicit none
c
c     create a finer-grid subregion bathymetry from
c     a full region HYCOM 2.0 bathymetry.
c
c     subregion grid must be an integer multiple of the original grid.
c     subregion p(1,1)             must be on the original p-grid.
c     subregion p(idm_out,jdm_out) must be on the original p-grid, i.e.
c     idm_out-1 and jdm_out-1 must be integer multiples of ijgrd.
c
      character*79         :: preambl(5)
      character*80         :: cline,cline_out
      character*128        :: flnm_in,flnm_out
      integer              :: idm_out,jdm_out,
     &                        iref_out,jref_out,iref_in,jref_in,ijgrd
      integer              :: i,ii,ijland,j,jj,l,ni,no,
     &                        ijsafe,nsmooth
      real                 :: hmina,hminb,hmaxa,hmaxb
      integer, allocatable :: m_in(:,:),m_sm(:,:),m_out(:,:)
      real,    allocatable :: a_in(:,:),aj_in(:), a_out(:,:)
c
      real,    parameter   :: hspval=0.5*2.0**100
c
      call xcspmd
      call zaiost
      call blkdat(idm_out,jdm_out,
     &            iref_out,jref_out,iref_in,jref_in, ijgrd,
     &            flnm_in,flnm_out,cline_out)
      call zbiost(idm_out,jdm_out)
c
      allocate(  m_sm(idm,jdm) )
      allocate(  m_in(idm,jdm), m_out(idm_out,jdm_out) )
      allocate(  a_in(idm,jdm), a_out(idm_out,jdm_out) )
      allocate( aj_in(idm) )
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
      preambl(1) = cline_out(1:79)
      write(no,'(a79)') preambl
      call flush(no)
      write(lp,*)
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
      do j= 1,jdm
        if     (iref_in+(idm_out-iref_out)/ijgrd.gt.idm) then
c
c         periodic case, shift iref_in to 1
c
          ii = 0
          do i= iref_in,idm
            ii = ii+1
            aj_in(ii) = a_in(i,j)
          enddo
          do i= 1,iref_in-1
            ii = ii+1
            aj_in(ii) = a_in(i,j)
          enddo
          do i= 1,idm
            a_in(i,j) = aj_in(i)
          enddo
        endif !periodic
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
      if     (iref_in+(idm_out-iref_out)/ijgrd.gt.idm) then
        iref_in = 1
      endif
c
c     calculate the output land/sea mask.
c
      m_out(:,:)       = 1  ! start with sea everywhere
      if     (ijgrd.ne.1 .or. idm_out.ne.idm) then
        m_out(idm_out,:) = 0  ! exclude the rectange edge, unless entire subset
      endif
      m_out(:,jdm_out) = 0  ! exclude the rectange edge
c
      ijland = ijgrd/2
      do j= 1,jdm
        jj = jref_out + (j-jref_in)*ijgrd
        if     (jj+ijland.ge.1 .and. jj-ijland.le.jdm_out) then
          do i= 1,idm
            ii = iref_out + (i-iref_in)*ijgrd
            if     (m_in(i,j).eq.0 .and.
     &              ii+ijland.ge.1 .and. ii-ijland.le.idm_out) then
              m_out(max(1,ii-ijland):min(idm_out,ii+ijland),
     &              max(1,jj-ijland):min(jdm_out,jj+ijland)) = 0
            endif
          enddo
        endif
      enddo
c
c     extrapolate the bathymetry by one grid point.
c     use m_sm to extrapolate only over the subdomain of interest.
c
      m_sm(:,:) = m_in(:,:)
      nsmooth   = 1
      ijsafe    = ijgrd*(nsmooth+1)
      do j= 1,jdm
        jj = jref_out + (j-jref_in)*ijgrd
        if     (jj.lt.1-ijsafe .or. jj.gt.jdm_out+ijsafe) then
          do i= 1,idm
            ii = iref_out + (i-iref_in)*ijgrd
            if     (ii.lt.1-ijsafe .or. ii.gt.idm_out+ijsafe) then
              m_sm(i,j) = 1  ! ignore (treat as ocean) for smoothing
            endif
          enddo
        endif
      enddo
c
      call landfill(a_in,m_sm,idm,jdm, nsmooth)
c
c     bi-linear interpolation everywhere.
c
      a_out = -99.0   ! should be over-written below
c
      call bilinear_p(a_in, idm,    jdm,
     &                a_out,idm_out,jdm_out,
     &                iref_out,jref_out,iref_in,jref_in, ijgrd)
c
      do j= 1,jdm_out
        do i= 1,idm_out
          if     (a_out(i,j).lt.0.0) then
            m_out(i,j) = 0  !outside the input region
          endif
        enddo
      enddo
c
c     output the resulting bathymetry.
c
      call zbiowr(a_out,m_out,.true., hmina,hmaxa, no, .false.)
      write(no,'(a,2f10.3)') 'min,max depth = ',hmina,hmaxa
      call flush(no)
      write(lp,'(a,2f10.3)') 'min,max depth = ',hmina,hmaxa
      call flush(lp)
c
      end program isub_topog

      subroutine bilinear_p(a_in, idm_in, jdm_in,
     &                      a_out,idm_out,jdm_out,
     &                      iref_out,jref_out,iref_in,jref_in, ijgrd)
      implicit none
c
      integer idm_in, jdm_in,
     &        idm_out,jdm_out,
     &        iref_out,jref_out,iref_in,jref_in, ijgrd
      real    a_in( idm_in, jdm_in ),
     &        a_out(idm_out,jdm_out)
c
c --- interpolate from a_in to a_out, both on p-grid.
c --- a_in(iref_in,jref_in) == a_out(iref_out,jref_out),
c --- with the output grid ijgrd times finer than the input.
c
      integer i,ii,ix,j,jj,jy
      real    dij,s
c
      dij = 1.0/ijgrd
c
c --- linear interpolation everywhere along co-located j lines.
c
      do j= 1,jdm_in
        jj = jref_out + (j-jref_in)*ijgrd
        if     (jj.ge.1 .and. jj.le.jdm_out) then
          do i= 1,idm_in
            ii = iref_out + (i-iref_in)*ijgrd
            do ix= max(ii,1),min(ii+ijgrd-1,idm_out)
              s = dij*(ix-ii)
              a_out(ix,jj) = (1.0-s)*a_in(i,j) + s*a_in(i+1,j)
            enddo
          enddo
        endif
      enddo
c
c --- linear interpolation for all i-lines.
c
      do j= 1,jdm_in-1
        jj = jref_out + (j-jref_in)*ijgrd
        if     (jj.ge.1 .and. jj+ijgrd.le.jdm_out) then
          do jy= jj+1,jj+ijgrd-1
            s = dij*(jy-jj)
            do i= 1,idm_out
              a_out(i,jy) = (1.0-s)*a_out(i,jj) + s*a_out(i,jj+ijgrd)
            enddo
          enddo
        endif
      enddo
      return
      end

      subroutine landfill(a,mask,m,n, npass)
      implicit none
c
      integer m,n,mask(m,n),npass
      real    a(m,n)
c
c --- extrapolate a 1-grid cell into the land mask,
c --- using npass's of a 9-point smoother based extrapolation scheme.
c --- mask == 1 for ocean.
c
      integer, allocatable :: mm(:,:,:)
c
      integer i,ii,ip0,ip1,ipass,j,jj,ki,kj,nup
      real    sa,ss
c
      real s(-1:1,-1:1)
      data s / 1.0,2.0,1.0, 2.0,4.0,2.0, 1.0,2.0,1.0 /
c
      allocate( mm(0:m+1,0:n+1,0:1) )
c
      mm( : , : ,0) = 0
      mm(1:m,1:n,0) = mask
c
      do ipass= 1,npass
        ip0 = mod(ipass+1,2)
        ip1 = mod(ipass,  2)
        mm(:,:,ip1) = mm(:,:,ip0)
        nup = 0
        do j= 1,n
          do i= 1,m
            if     (mm(i,j,ip0).eq.0) then
              sa = 0.0
              ss = 0.0
              do kj= -1,1
                jj = j+kj
                do ki= -1,1
                  ii = i+ki
                  if     (mm(ii,jj,ip0).eq.1) then
                    sa = sa + s(ki,kj)*a(ii,jj)
                    ss = ss + s(ki,kj)
                  endif
                enddo
              enddo
              if     (ss.ne.0.0) then
                a( i,j)     = sa/ss
                mm(i,j,ip1) = 1
                nup         = nup + 1
                if     (mod(nup,1000).eq.1) then
                  write(6,'(a,2i5,f5.1,f10.3)') 
     &              '   i,j,ss,a = ',i,j,ss,a(i,j)
                endif
              endif
            endif
          enddo
        enddo
        write(6,'(a,i4,a,i6,a)') 'landfill: pass',ipass,
     &                           ' filled in',nup,' points'
      enddo  ! ipass=1,npass
      write(6,*)
c
      deallocate( mm )
c
      return
      end

      subroutine blkdat(idm_out,jdm_out,
     &                   iref_out,jref_out,iref_in,jref_in, ijgrd,
     &                   flnm_in,flnm_out,cline_out)
      use mod_xc  ! HYCOM communication interface
      implicit none
c
      integer       :: idm_out,jdm_out,
     &                 iref_out,jref_out,iref_in,jref_in, ijgrd
      character*128 :: flnm_in,flnm_out
      character*80  :: cline_out
c
c --- read blkdat.input for topog interpolating subregion.
c
c --- 'flnm_in'   = input  full region bathymetry filename
c --- 'flnm_out'  = output  sub-region bathymetry filename
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
c --- 'ijgrd ' = integer scale factor between input and output grids
c
      call blkini(idm_out,   'idm   ')
      call blkini(jdm_out,   'jdm   ')
      call blkini(iref_in,   'irefi ')
      call blkini(jref_in,   'jrefi ')
      call blkini(iref_out,  'irefo ')
      call blkini(jref_out,  'jrefo ')
      call blkini(ijgrd,     'ijgrd ')
c
      if     (ijgrd.gt.1) then
        if     (mod(idm_out,ijgrd).ne.1 .or.
     &          mod(jdm_out,ijgrd).ne.1     ) then
          write(6,'(/a,a/)')
     &      '***** error - ',
     &      'subregion p(idm,jdm) not on outer p-grid *****'
          call flush(6)
          stop
        endif
      endif
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
