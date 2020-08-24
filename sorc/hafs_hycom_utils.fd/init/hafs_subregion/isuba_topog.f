      program isuba_topog
      use mod_za  ! HYCOM I/O interface
      use mod_zb  ! HYCOM I/O interface for subregion
      implicit none
c
c     create a different-grid subregion bathymetry from
c     a full region HYCOM 2.0 bathymetry.
c
c     subregion grid is arbitrary, but any part of this grid
c     that is outside the input grid must be land.
c
c     first run isuba_gmap to generate the sub-region grid map.
c
c     Alan J. Wallcraft,  NRL,  July 2002 and January 2009.
c
      character*79         :: preambl(5)
      character*80         :: cline,cline_out
      character*256        :: flnm_in,flnm_out,flnm_map
      logical              :: ldebug
      integer              :: idm_out,jdm_out
      integer              :: i,ii,ip,j,jj,jp,jq,l,ni,nir,no,nor,nsmooth
      integer              :: iidebug,jjdebug
      real                 :: hmina,hminb,hmaxa,hmaxb
      integer, allocatable :: m_in(:,:),m_sm(:,:),m_out(:,:)
      real,    allocatable :: a_in( :,:)
      real,    allocatable :: a_out(:,:)
      integer, allocatable :: i_out(:,:),j_out(:,:)
      real,    allocatable :: x_out(:,:),y_out(:,:)
c
      real,    parameter   :: hspval=0.5*2.0**100
c
      call xcspmd
      call zaiost
      call blkdat(idm_out,jdm_out,
     &            flnm_map,flnm_in,flnm_out,cline_out)
      call zbiost(idm_out,jdm_out)
c
      allocate(    m_sm(idm,jdm) )
      allocate(    m_in(idm,jdm),    m_out(idm_out,jdm_out) )
      allocate(    a_in(idm,jdm),    a_out(idm_out,jdm_out) )
c
      allocate( i_out(idm_out,jdm_out), j_out(idm_out,jdm_out) )
      allocate( x_out(idm_out,jdm_out), y_out(idm_out,jdm_out) )
c
c     open input and output bathymetry files.
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
c     process the bathymetry header
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
c     read the map between input and output grid locations (no error checking).
c
      nor = 25
      call zbiopf(trim(flnm_map),'old', nor)
      call zbiord(x_out,m_out,.false., hmina,hmaxa, nor)
      call zbiord(y_out,m_out,.false., hmina,hmaxa, nor)
      call zbiocl(nor)
c
      do jj= 1,jdm_out
        do ii= 1,idm_out
          if     (x_out(ii,jj).lt.hspval) then
            i_out(ii,jj) = int(x_out(ii,jj))
            x_out(ii,jj) =     x_out(ii,jj) - i_out(ii,jj)
            j_out(ii,jj) = int(y_out(ii,jj))
            y_out(ii,jj) =     y_out(ii,jj) - j_out(ii,jj)
          endif
        enddo !ii
      enddo !jj
c
c     calculate the output land/sea mask and the smooth mask.
c
      ldebug = .false.
*
      iidebug = min(idm_out/5, 500)
      jjdebug = min(jdm_out/5, 500)
c
      m_sm(:,:) = 0
      do jj= 1,jdm_out
        do ii= 1,idm_out
          ldebug = mod(ii,iidebug).eq.1 .and. mod(jj,jjdebug).eq.1
c
          if     (x_out(ii,jj).ge.hspval) then
            m_out(ii,jj) = 0  !outside the input rectangle
            if     (ldebug) then
              write(6,'(a,2i5,i2," (outside)")')
     &          'ii,jj,m_out',
     &           ii,jj,m_out(ii,jj)
              call flush(6)
            endif
          else
            j  = j_out(ii,jj)
            i  = i_out(ii,jj)
            jp = min(j+1,jdm)
            if     (i.ne.idm) then
              ip = i+1
            else  !lperiod
              ip =   1
            endif
c           take land/sea from the nearest input point
            if     (x_out(ii,jj).le.0.5) then !i nearest
              if     (y_out(ii,jj).le.0.5) then  !j nearest
                m_out(ii,jj) = m_in(i, j)
              else
                m_out(ii,jj) = m_in(i, jp)
              endif !yout
            else !i+1 nearest
              if     (y_out(ii,jj).le.0.5) then  !j nearest
                m_out(ii,jj) = m_in(ip,j)
              else
                m_out(ii,jj) = m_in(ip,jp)
              endif !yout
            endif !xout
            if     (m_out(ii,jj).eq.1) then
              m_sm(i, j ) = m_in(i, j )
              m_sm(i, jp) = m_in(i, jp)
              m_sm(ip,j ) = m_in(ip,j )
              m_sm(ip,jp) = m_in(ip,jp)
            endif
c
            if     (ldebug) then
              write(6,'(a,4i5,4i2)')
     &          'ii,jj,i,j,m_in',
     &           ii,jj,i,j,
     &           m_in(i, j ),
     &           m_in(i, jp),
     &           m_in(ip,j ),
     &           m_in(ip,jp)
              write(6,'(a,2i5,i2)')
     &          'ii,jj,m_out',
     &           ii,jj,m_out(ii,jj)
              call flush(6)
            endif
          endif  !m_out(ii,jj).eq.1
        enddo !ii
      enddo !jj
c
*     write(6,'(/a)') 'input    mask:'
*     do j= jdm,1,-1
*       write(6,'(a,i4,3x,100i1)')
*    &   'j =',j,m_in(1:min(idm,100),j)
*     enddo
*     write(6,'(/a)') 'landfill mask:'
*     do j= jdm,1,-1
*       write(6,'(a,i4,3x,100i1)')
*    &   'j =',j,m_sm(1:min(idm,100),j)
*     enddo
*     write(6,'(/a)') 'output   mask:'
*     do j= jdm_out,1,-1
*       write(6,'(a,i4,3x,100i1)')
*    &   'j =',j,m_out(1:min(idm_out,100),j)
*     enddo
*     write(6,*)
c
c     extrapolate the bathymetry by one grid point.
c     use m_sm to extrapolate only over the subdomain of interest.
c
      nsmooth = 1
      call landfill(a_in,m_sm,idm,jdm, nsmooth)
c
c     bi-linear interpolation everywhere.
c
      a_out = -99.0   ! should be over-written below
c
      call bilinear_p(a_in, idm,    jdm,
     &                a_out,idm_out,jdm_out,
     &                m_out,i_out,j_out,x_out,y_out)
c
c     output the resulting bathymetry.
c
      call zbiowr(a_out,m_out,.true., hmina,hmaxa, no, .false.)
      write(no,'(a,2f10.3)') 'min,max depth = ',hmina,hmaxa
      call flush(no)
      write(lp,'(a,2f10.3)') 'min,max depth = ',hmina,hmaxa
      call flush(lp)
c
      if     (hmina.lt.0.0) then
        write(lp,'(/a)') '***** error - minimum depth < 0 *****'
        write(lp,'(a/)') '***** indicates a grid mismatch *****'
      endif
c
      end program isuba_topog

      subroutine bilinear_p(a_in, idm_in, jdm_in,
     &                      a_out,idm_out,jdm_out,
     &                      m_out,i_out,j_out,x_out,y_out)
      implicit none
c
      integer idm_in, jdm_in,
     &        idm_out,jdm_out
      integer m_out(idm_out,jdm_out),
     &        i_out(idm_out,jdm_out),
     &        j_out(idm_out,jdm_out)
      real    a_in( idm_in, jdm_in ),
     &        a_out(idm_out,jdm_out),
     &        x_out(idm_out,jdm_out),
     &        y_out(idm_out,jdm_out)
c
c --- interpolate from a_in to a_out.
c
      integer i,ii,ip,j,jj,jp
      real    sx,sy
c
      do jj= 1,jdm_out
        do ii= 1,idm_out
          if     (m_out(ii,jj).eq.1) then
            sx = x_out(ii,jj)
            sy = y_out(ii,jj)
            i  = i_out(ii,jj)
            if     (i.ne.idm_in) then
              ip = i+1
            else
              ip =   1
            endif
            j  = j_out(ii,jj)
            jp = j+1
c
            a_out(ii,jj) = (1.0-sx)*(1.0-sy)*a_in(i, j ) +
     &                     (1.0-sx)*     sy *a_in(i, jp) +
     &                          sx *(1.0-sy)*a_in(ip,j ) +
     &                          sx *     sy *a_in(ip,jp)
*           if     (a_out(ii,jj).lt.0.0) then
*             write(6,'(a,6i5,2f7.3,5f9.2)')
*    &        'ii,jj,i,ip,j,jp,sx,sy,a_out,a_in',
*    &         ii,jj,i,ip,j,jp,
*    &         sx,sy,a_out(ii,jj),
*    &         a_in(i, j ), 
*    &         a_in(i, jp), 
*    &         a_in(ip,j ), 
*    &         a_in(ip,jp)
*           endif
          endif
        enddo !ii
      enddo !jj
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
     &                   flnm_map,flnm_in,flnm_out,cline_out)
      use mod_xc  ! HYCOM communication interface
      implicit none
c
      integer       :: idm_out,jdm_out
      character*256 :: flnm_map,flnm_in,flnm_out
      character*80  :: cline_out
c
c --- read blkdat.input for topog interpolating subregion.
c
c --- 'flnm_map'  = target  sub-region grid map   filename
c --- 'flnm_in'   = input  full region bathymetry filename
c --- 'flnm_out'  = output  sub-region bathymetry filename
c --- 'cline_out' = output title line (replaces preambl(5))
c
      read( *,'(a)')      flnm_map
      write(6,'(a)') trim(flnm_map)
      write(6,*)
      read( *,'(a)')      flnm_in
      write(6,'(a)') trim(flnm_in)
      read( *,'(a)')      flnm_out
      write(6,'(a)') trim(flnm_out)
      read( *,'(a)')      cline_out
      write(6,'(a)') trim(cline_out)
      write(6,*)
      call flush(6)
c
c --- 'idm   ' = output longitudinal array size
c --- 'jdm   ' = output latitudinal  array size
c
      call blkini(idm_out,   'idm   ')
      call blkini(jdm_out,   'jdm   ')
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
