      program isubaregion
      use mod_za  ! HYCOM I/O interface
      use mod_zb  ! HYCOM I/O interface for subregion
      implicit none
c
c     create a diferent-grid subregion from a full region archive file.
c
c     subregion grid is arbitrary, except that any part of this grid
c     that is outside the input grid must be land.
c
c     same resolution grid is allowed.  this is usually handled by
c     subregion.f, but use this program (or isubregion.f) when the
c     bathymetries are not identical over the subregion.
c
c     subregion same as region is allowed.  this provides a way to
c     restart a run with a different coastline and/or bathymetry,
c     i.e. original archive -> modified archive -> modified restart.
c
c     if the output grid is an integer multiple of the original, with
c     co-located p-grid points, then isubregion.f will be faster and
c     it will also produce more accurate velocity fields.
c
c     first run isuba_gmap to generate the sub-region grid map.
c
c     Alan J. Wallcraft,  NRL,  July 2002 and January 2009.
c
      character*80         :: cline,cline_u,cline_out
      character*128        :: flnm_in, flnm_tin,
     &                        flnm_out,flnm_top,flnm_reg,flnm_map
      integer              :: idm_out,jdm_out
      integer              :: i,ii,ios,ip,itmp,j,jj,jp,jq,l,
     &                        ni,nir,no,nor
      integer              :: k,l0,l1, ibadl,ibads
      integer              :: if_sm,il_sm,jf_sm,jl_sm
      logical              :: lrot(2),lcheck
      logical              :: smooth,icegln
      real                 :: hmina,hminb,hmaxa,hmaxb,
     &                        rtmp,up,vp
      integer, allocatable ::    m_sm(:,:),    iv_sm(:,:)
      integer, allocatable ::    m_in(:,:),    m_out(:,:),
     &                                         m_osm(:,:)
      real,    allocatable ::    a_in(:,:),    a_out(:,:)
      real,    allocatable ::    u_in(:,:),    u_out(:,:)
      real,    allocatable ::    v_in(:,:),    v_out(:,:)
      real,    allocatable ::    t_in(:,:),    t_out(:,:,:)
      real,    allocatable ::    p_in(:,:,:),  p_out(:,:,:)
      real,    allocatable :: pang_in( :,:),pang_out(:,:)
c
      integer, allocatable :: i_out(:,:),j_out(:,:)
      real,    allocatable :: x_out(:,:),y_out(:,:)
c
      real,    parameter   :: hspval=0.5*2.0**100  ! half spval
      real,    parameter   :: onem=9806.0          ! g/thref
      real,    parameter   :: tenm=10.0*onem
c
      call xcspmd
      call zaiost
      call blkdat(idm_out,jdm_out,
     &            smooth,icegln,
     &            flnm_reg,flnm_map,flnm_in,flnm_tin,
     &            flnm_out,flnm_top,cline_out)
      call zbiost(idm_out,jdm_out)
c
      allocate(   iv_sm(jdm,2) )
c
      allocate(    m_sm(idm,jdm),     m_osm(idm_out,jdm_out) )
      allocate(    m_in(idm,jdm),     m_out(idm_out,jdm_out) )
      allocate(    a_in(idm,jdm),     a_out(idm_out,jdm_out) )
      allocate(    u_in(idm,jdm),     u_out(idm_out,jdm_out) )
      allocate(    v_in(idm,jdm),     v_out(idm_out,jdm_out) )
      allocate(    t_in(idm,jdm),     t_out(idm_out,jdm_out,3) )
      allocate(    p_in(idm,jdm,0:1), p_out(idm_out,jdm_out,0:1) )
      allocate( pang_in(idm,jdm),  pang_out(idm_out,jdm_out) )
c                                                            
      allocate( i_out(idm_out,jdm_out), j_out(idm_out,jdm_out) )
      allocate( x_out(idm_out,jdm_out), y_out(idm_out,jdm_out) )
c
c     get the output p-grid mask from the bathymetry.
c
      l  = len_trim(flnm_top)
      open (unit=13,file=flnm_top(1:l-2)//'.b',form='formatted',
     .      status='old',action='read')
      write(lp,'(/a)') 'OUTPUT BATHYMETRY:'
      do i= 1,6
        read( 13,'(a)') cline
        write(lp,'(a)') cline(1:len_trim(cline))
      enddo
      l = index(cline,'=')
      read (cline(l+1:),*)  hminb,hmaxb
      close(unit=13)
c
      l  = len_trim(flnm_top)
      call zbiopf(flnm_top(1:l-2)//'.a','old', 13)
      call zbiord(t_out(1,1,1),m_out,.false., hmina,hmaxa, 13)
      call zbiocl(13)
      if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &        abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
        write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &    'error - output bathymetry .a and .b files not consistent:',
     &    '.a,.b min = ',hmina,hminb,hmina-hminb,
     &    '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
        stop
      endif
c
      do j= 1,jdm_out
        do i= 1,idm_out
          if     (t_out(i,j,1).gt.hspval .or.
     &            t_out(i,j,1).le.0.0        ) then
            t_out(i,j,1) = 0.0
            m_out(i,j)   = 0
          else
            t_out(i,j,1) = t_out(i,j,1)*onem
            m_out(i,j)   = 1
          endif
        enddo
      enddo
c
c     get the input p-grid mask from the bathymetry.
c
      l  = len_trim(flnm_tin)
      open (unit=13,file=flnm_tin(1:l-2)//'.b',form='formatted',
     .      status='old',action='read')
      write(lp,'(/a)') ' INPUT BATHYMETRY:'
      do i= 1,6
        read( 13,'(a)') cline
        write(lp,'(a)') cline(1:len_trim(cline))
      enddo
      l = index(cline,'=')
      read (cline(l+1:),*)  hminb,hmaxb
      close(unit=13)
c
      l  = len_trim(flnm_tin)
      call zaiopf(flnm_tin(1:l-2)//'.a','old', 13)
      call zaiord(t_in,m_in,.false., hmina,hmaxa, 13)
      call zaiocl(13)
      if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &        abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
        write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &    'error - bathymetry .a and .b files not consistent:',
     &    '.a,.b min = ',hmina,hminb,hmina-hminb,
     &    '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
        stop
      endif
c
      do j= 1,jdm
        do i= 1,idm
          if     (t_in(i,j).gt.hspval .or.
     &            t_in(i,j).le.0.0        ) then
            t_in(i,j) = 0.0
            m_in(i,j) = 0
            m_sm(i,j) = 0
          else
            t_in(i,j) = t_in(i,j)*onem
            m_in(i,j) = 1
            m_sm(i,j) = 1
          endif
        enddo
      enddo
c       
c     read the input and output grid locations (no error checking).
c
      nir = 24
      call zaiopf('regional.grid.a','old', nir)
      call zaiosk(nir) !plon
      call zaiosk(nir) !plat
      call zaiosk(nir) !qlon
      call zaiosk(nir) !qlat
      call zaiosk(nir) !ulon
      call zaiosk(nir) !ulat
      call zaiosk(nir) !vlon
      call zaiosk(nir) !vlat
      call zaiord(pang_in,m_in,.false., hmina,hmaxa, nir)
      call zaiocl(nir)
c     
      nor = 25
      call zbiopf(trim(flnm_reg),'old', nor)
      call zbiosk(nor) !plon
      call zbiosk(nor) !plat
      call zbiosk(nor) !qlon
      call zbiosk(nor) !qlat
      call zbiosk(nor) !ulon
      call zbiosk(nor) !ulat
      call zbiosk(nor) !vlon
      call zbiosk(nor) !vlat
      call zbiord(pang_out,m_out,.false., hmina,hmaxa, nor)
      call zbiocl(nor)
c
      nor = 26
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
          else  !update output mask
            m_out(ii,jj) = 0
          endif
        enddo !ii
      enddo !jj
c
      lrot(1) = minval(pang_in(:,:)).ne.0.0 .or.
     &          maxval(pang_in(:,:)).ne.0.0
      if     (.not.lrot(1)) then
        write(6,'(a)') ' input domain is     aligned E-W,N-S'
      else
        write(6,'(a)') ' input domain is not aligned E-W,N-S'
      endif
c
      lrot(2) = minval(pang_out(:,:)).ne.0.0 .or.
     &          maxval(pang_out(:,:)).ne.0.0
      if     (.not.lrot(2)) then
        write(6,'(a)') 'output domain is     aligned E-W,N-S'
      else
        write(6,'(a)') 'output domain is not aligned E-W,N-S'
      endif
c       
c --- form the p-grid smoother mask,
c --- set to 2 if a land point is needed for interpolation.
c --- we are assuming that "2" is never needed outside the
c --- target subregion, which will be the case unless the
c --- subregion rectangle is poorly chosen.
c
      do jj= 1,jdm_out
        do ii= 1,idm_out
          if     (m_out(ii,jj).eq.1) then
            j  = j_out(ii,jj)
            i  = i_out(ii,jj)
            jp = min(j+1,jdm)
            if     (i.ne.idm) then
              ip = i+1            
            else  !lperiod        
              ip =   1    
            endif         
            if     (m_sm(i, j ).eq.0) then
              m_sm(i, j ) = 2
            endif
            if     (m_sm(i, jp).eq.0) then
              m_sm(i, jp) = 2
            endif
            if     (m_sm(ip,j ).eq.0) then
              m_sm(ip,j ) = 2
            endif
            if     (m_sm(ip,jp).eq.0) then
              m_sm(ip,jp) = 2
            endif
          endif
        enddo
      enddo
c
      do j= 1,jdm
        iv_sm(j,1) = idm
        do i= 1,idm
          if     (m_sm(i,j).eq.2) then
            iv_sm(j,1) = i
            exit
          endif
        enddo
        iv_sm(j,2) = 1
        do i= idm,iv_sm(j,1),-1
          if     (m_sm(i,j).eq.2) then
            iv_sm(j,2) = i
            exit
          endif
        enddo
      enddo
      jf_sm = jdm
      do j= 1,jdm
        if     (iv_sm(j,1).le.iv_sm(j,2)) then
          jf_sm = j
          exit
        endif
      enddo
      jl_sm = 1
      do j= jdm,jf_sm,-1
        if     (iv_sm(j,1).le.iv_sm(j,2)) then
          jl_sm = j
          exit
        endif
      enddo
      if_sm = minval(iv_sm(:,1))
      il_sm = maxval(iv_sm(:,2))
c
c     interpolate the input bathymetry to the output grid.
c
      call landfill(  t_in,m_sm,   idm,    jdm,
     &                iv_sm,if_sm,il_sm,jf_sm,jl_sm)
      call bilinear_p(t_in,        idm,    jdm,
     &                t_out(1,1,2),idm_out,jdm_out,
     &                m_out,i_out,j_out,x_out,y_out)
c
c     allow for a bottom boundary layer
c
      do j= 1,jdm_out
        do i= 1,idm_out
          if     (m_out(i,j).eq.1) then
            t_out(i,j,3) = t_out(i,j,2) -
     &                     max( onem, 
     &                          min( tenm,
     &                               t_out(i,j,2)*0.03 ) )
          endif
        enddo
      enddo
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
      write(lp,'(/a)') 'ARCHIVE HEADER:'
c
      read( ni,'(a)') cline  ! ctitle(1)
      write(no,'(a)') cline
      write(lp,'(a)') cline(1:len_trim(cline))
c
      read( ni,'(a)') cline  ! ctitle(2)
      write(no,'(a)') cline
      write(lp,'(a)') cline(1:len_trim(cline))
c
      read( ni,'(a)') cline  ! ctitle(3)
      write(no,'(a)') cline
      write(lp,'(a)') cline(1:len_trim(cline))
c
      read( ni,'(a)') cline  ! ctitle(4), replace with cline_out
      write(no,'(a)') cline_out
      write(lp,'(a)') cline_out(1:len_trim(cline))
c
      read( ni,'(a)') cline  ! iversn
      write(no,'(a)') cline
      write(lp,'(a)') cline(1:len_trim(cline))
c
      read( ni,'(a)') cline  ! iexpt 
      write(no,'(a)') cline
      write(lp,'(a)') cline(1:len_trim(cline))
c
      read( ni,'(a)') cline  ! yrflag
      write(no,'(a)') cline
      write(lp,'(a)') cline(1:len_trim(cline))
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
      write(lp,'(a)') cline(1:len_trim(cline))
c
      call flush(no)
      call flush(lp)
c
c     loop through all 2-d fields in archive file.
c     treat layer thickness as layer interface depth (p).
c
      k = 0
      lcheck = .true.
      p_in( :,:,:) = 0.0
      p_out(:,:,:) = 0.0
c
      do  ! loop until file ends
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
        if     (cline(1:1).eq.'u') then
c
c ---     u-grid, save the field (on the p-grid) for later processing.
c
          do j= 1,jdm
            do i= 1,idm
              if     (a_in(i,j).gt.hspval) then
                a_in(i,j) = 0.0
              endif
            enddo
            do i= 1,idm-1
              u_in(i,j) = 0.5*(a_in(i,j)+a_in(i+1,j))
            enddo
            u_in(idm,j) = 0.5*(a_in(idm,j)+a_in(1,j))
          enddo
          cline_u = cline
c
        elseif (cline(1:1).eq.'v' .and.
     &          cline(1:8).ne.'viscty  ') then
c
c ---     v-grid.  process both u and v fields.
c
          j=jdm
            do i= 1,idm
              if     (a_in(i,j).gt.hspval) then
                a_in(i,j) = 0.0
              endif
              v_in(i,j) = 0.0
            enddo
          do j= jdm-1,1,-1
            do i= 1,idm
              if     (a_in(i,j).gt.hspval) then
                a_in(i,j) = 0.0
              endif
              v_in(i,j) = 0.5*(a_in(i,j)+a_in(i,j+1))
            enddo
          enddo
c
c         u_out (eastwards) and v_out (northwards) on p-grid.
c
          if     (.not.lrot(1)) then
            call bilinear_p(u_in,     idm,    jdm,
     &                      u_out,    idm_out,jdm_out,
     &                      m_out,i_out,j_out,x_out,y_out)
            call bilinear_p(v_in,     idm,    jdm,
     &                      v_out,    idm_out,jdm_out,
     &                      m_out,i_out,j_out,x_out,y_out)
          else  !lrot(1)
            do j= 1,jdm
              do i= 1,idm
                a_in(i,j) = cos(pang_in(i,j))*u_in(i,j) -
     &                      sin(pang_in(i,j))*v_in(i,j)
              enddo
            enddo
            call bilinear_p(a_in,     idm,    jdm,
     &                      u_out,    idm_out,jdm_out,
     &                      m_out,i_out,j_out,x_out,y_out)
c
            do j= 1,jdm
              do i= 1,idm
                a_in(i,j) = cos(pang_in(i,j))*v_in(i,j) +
     &                      sin(pang_in(i,j))*u_in(i,j)
              enddo
            enddo
            call bilinear_p(a_in,     idm,    jdm,
     &                      v_out,    idm_out,jdm_out,
     &                      m_out,i_out,j_out,x_out,y_out)
          endif !.not.lrot(1):else
c
c         re-rotate on output p-grid, if necessary.
c
          if     (lrot(2)) then
            do jj= 1,jdm_out
              do ii= 1,idm_out
                up = u_out(ii,jj)
                vp = v_out(ii,jj)
                u_out(ii,jj) = cos(pang_out(ii,jj))*up +
     &                         sin(pang_out(ii,jj))*vp
                v_out(ii,jj) = cos(pang_out(ii,jj))*vp -
     &                         sin(pang_out(ii,jj))*up
              enddo
            enddo
          endif
c
c         write u-velocity on output u-grid.
c
          do jj= 1,jdm_out
              a_out( 1,jj) = 0.5*(u_out( 1,jj)+u_out(idm_out,jj))
            do ii= 2,idm_out
              a_out(ii,jj) = 0.5*(u_out(ii,jj)+u_out(   ii-1,jj))
            enddo
          enddo
          call zbiowr(a_out,m_out,.false., hmina,hmaxa, no, .false.)
          write(no,'(a,1p2e16.7)') cline_u(1:41),hmina,hmaxa
          call flush(no)
          write(lp,'(a,1p2e16.7)') cline_u(1:41),hmina,hmaxa
          call flush(lp)
c
c         write v-velocity on output v-grid.
c
          jj=1
            do ii= 1,idm_out
              a_out(ii,jj) =      v_out(ii,jj)
            enddo
          do jj= 2,jdm_out
            do ii= 1,idm_out
              a_out(ii,jj) = 0.5*(v_out(ii,jj)+v_out(ii,jj-1))
            enddo
          enddo
          call zbiowr(a_out,m_out,.false., hmina,hmaxa, no, .false.)
          write(no,'(a,1p2e16.7)') cline(1:41),hmina,hmaxa
          call flush(no)
          write(lp,'(a,1p2e16.7)') cline(1:41),hmina,hmaxa
          call flush(lp)
c
        elseif (cline(1:8).eq.'thknss  ') then
c
c ---     layer thickness (on p-grid),
c ---     do interpolation etcetera on interface depth.
c
          k  = k + 1
          l0 = mod(k+1, 2)
          l1 = mod(k,   2)
          p_in(:,:,l0) = p_in(:,:,l1) + a_in
          call landfill(  p_in( 1,1,l0),m_sm,idm,    jdm,
     &                    iv_sm,if_sm,il_sm,jf_sm,jl_sm)
          call bilinear_p(p_in( 1,1,l0),     idm,    jdm,
     &                    p_out(1,1,l0),     idm_out,jdm_out,
     &                    m_out,i_out,j_out,x_out,y_out)
          do j= 1,jdm_out
            do i= 1,idm_out
              if     (m_out(i,j).eq.1) then
                p_out(i,j,l0) = max( p_out(i,j,l1),
     &                               min( t_out(i,j,1),
     &                                    p_out(i,j,l0) ) )
                if     (t_out(i,j,1) .gt.t_out(i,j,2) .and.
     &                  p_out(i,j,l0).gt.t_out(i,j,3)      ) then
c
c                 if within old bottom boundary layer,
c                 extend layer to the new (deeper) bottom.
c
*                 if     (p_out(i,j,l0).ne.t_out(i,j,1)) then
*                   write(lp,'(a,2i5,i3,3f9.2)')
*    &                'deep: ',i,j,k,
*    &                (t_out(i,j,3)-p_out(i,j,l0))/onem,
*    &                p_out(i,j,l0)/onem,t_out(i,j,1)/onem
*                 endif
                  p_out(i,j,l0) = t_out(i,j,1)
                endif
              endif
            enddo
          enddo
          if     (smooth) then
            do j= 1,jdm_out
              do i= 1,idm_out
                if     (m_out(i,j).eq.1 .and.
     &                  p_out(i,j,l0).lt.t_out(i,j,1)) then
                  m_osm(i,j) = 1
                else
                  m_osm(i,j) = 0
                endif
              enddo
            enddo
            call psmooth( p_out(1,1,l0),m_osm,
     &                    p_out(1,1,l1),t_out(1,1,1),idm_out,jdm_out)
          endif
c
          do j= 1,jdm_out
            do i= 1,idm_out
              if     (m_out(i,j).eq.1) then
                a_out(i,j) = p_out(i,j,l0) - p_out(i,j,l1)
              endif
            enddo
          enddo
          call zbiowr(a_out,m_out,.true.,  hmina,hmaxa, no, .false.)
          write(no,'(a,1p2e16.7)') cline(1:41),hmina,hmaxa
          call flush(no)
          write(lp,'(a,1p2e16.7)') cline(1:41),hmina,hmaxa
          call flush(lp)
c
        else
c
c ---     p-grid.
c
          if     (lcheck) then  !1st p-grid field
            lcheck = .false.
c
            ibadl = 0
            ibads = 0
            do j= 1,jdm
              do i= 1,idm
                if     (m_in(i,j).eq.1) then !topo sea
                  if     (a_in(i,j).gt.2.0**99) then !archive land
                    ibads = ibads + 1   ! topo sea, archive land
*                   if     (mod(ibads,100).eq.1) then
*                   if     (mod(ibads, 10).eq.1) then
*                     write(lp,*) 'topo sea, archive land at i,j = ',i,j
*                   endif
                  endif
                else !topo land
                  if     (a_in(i,j).lt.2.0**99) then !archive sea
                    ibadl = ibadl + 1   ! topo land, archive sea
*                   if     (mod(ibadl,100).eq.1) then
*                   if     (mod(ibadl, 10).eq.1) then
*                     write(lp,*) 'topo land, archive sea at i,j = ',i,j
*    &                            ,a_in(i,j)
*                   endif
                  endif
                endif !ip-sea:ip-land
              enddo !i
            enddo !j
            if     (ibads.ne.0) then
              write(lp,*)
              write(lp,*) 'error - wrong bathymetry for this archive'
              write(lp,*) 'number of topo sea  mismatches = ',ibads
              write(lp,*) 'number of topo land mismatches = ',ibadl
              write(lp,*)
              call flush(lp)
              stop
            endif
            if     (ibadl.ne.0) then
              write(lp,*)
*             write(lp,*) 'warning - wrong bathymetry for this archive'
              write(lp,*) 'error - wrong bathymetry for this archive'
              write(lp,*) 'number of topo sea  mismatches = ',ibads
              write(lp,*) 'number of topo land mismatches = ',ibadl
              write(lp,*)
              call flush(lp)
              stop
            endif
          endif !lcheck
c
c ---     p-grid (continued).
c
          call landfill(  a_in,m_sm,idm,    jdm,
     &                    iv_sm,if_sm,il_sm,jf_sm,jl_sm)
          call bilinear_p(a_in,     idm,    jdm,
     &                    a_out,    idm_out,jdm_out,
     &                    m_out,i_out,j_out,x_out,y_out)
          call zbiowr(a_out,m_out,.true.,  hmina,hmaxa, no, .false.)
          write(no,'(a,1p2e16.7)') cline(1:41),hmina,hmaxa
          call flush(no)
          write(lp,'(a,1p2e16.7)') cline(1:41),hmina,hmaxa
          call flush(lp)
        endif
      enddo  !loop until file ends
c
      end program isubaregion

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
        enddo
      enddo
      return
      end

      subroutine landfill(a,mask,m,n, iv,if,il,jf,jl)
      implicit none
c
      integer m,n,mask(m,n), iv(n,2),if,il,jf,jl
      real    a(m,n)
c
c --- extrapolate a 1-grid cell into the land mask,
c ---   mask == 0 for land.  
c ---   mask == 1 for ocean.
c ---   mask == 2 for land to be extrapolated to ocean.
c
      integer, allocatable :: mm(:,:,:)
c
      integer i,ii,ip0,ip1,ipass,j,jj,ki,kj,nleft,nup
      real    sa,ss
c
      logical lfirst
      real    s(-1:1,-1:1)
      save    lfirst,s
c
      data lfirst / .true. /
      data      s / 1.0, 2.0, 1.0,
     &              2.0, 4.0, 2.0,
     &              1.0, 2.0, 1.0 /
c
c     adding a halo to mm simplifies ocean selection logic.
c
      allocate( mm(0:m+1,0:n+1,0:1) )
c
      mm( : , : ,0) = 0
      mm(1:m,1:n,0) = mask
      mm( : , : ,1) = mm(:,:,0)
c
c --- repeated passes of 9-point "smoother" to
c ---  convert all mask==2 points to mask==1.
c --- double-buffering mm allows in-place use of a.
c
      if     (lfirst) then
        write(6,'(/a,6i5/)')
     &    'landfill - m,n,if,il,jf,jl =',m,n,if,il,jf,jl
      endif
      do ipass= 1,n+m
        ip0   = mod(ipass+1,2)
        ip1   = mod(ipass,  2)
        nup   = 0
        nleft = 0
        do j= jf,jl
          do i= iv(j,1),iv(j,2)
            if     (mm(i,j,ip0).eq.2) then
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
c
c               at least one ocean point within stencil.
c
                a( i,j)     = sa/ss
                mm(i,j,ip1) = 1
                nup         = nup + 1
*               if     (mask(i,j).eq.1) then
*                 write(6,*) 'error - i,j,ip0,ip1,mask,mm = ',
*    &              i,j,ip0,ip1,mask(i,j),mm(i,j,ip0)
*                 stop
*               endif
*               if     (mod(nup,1000).eq.1) then
*                 write(6,'(a,2i5,f5.1,f10.3)') 
*    &              '   i,j,ss,a = ',i,j,ss,a(i,j)
*               endif
              else
                nleft = nleft + 1
              endif
            endif
          enddo
        enddo
        if     (lfirst) then
          write(6,'(a,i4,a,i6,a,i6,a)')
     &      'landfill: pass',ipass,
     &      ' filled in',nup,
     &      ' points, with',nleft,' still to fill'
          call flush(6)
        endif
        if     (nup.eq.0) then
          exit
        endif
        mm(if:il,jf:jl,ip0) = mm(if:il,jf:jl,ip1)
      enddo  ! ipass=1,...
      if     (lfirst) then
        write(6,*)
        lfirst = .false.
      endif
      if     (nleft.ne.0) then
        write(6,'(/a,i6,a/a/)')
     &    'error in landfill - ',
     &    nleft,' "mask==2" values are not fillable',
     &    'probably a mismatch between coarse and fine land masks'
        call flush(6)
        do j= jf,jl
          do i= iv(j,1),iv(j,2)
            if     (mm(i,j,ip1).eq.2) then
              write(6,'(a,2i5)') 'mask==2 at (coarse) i,j = ',i,j
            endif
          enddo
        enddo
        write(6,*)
        call flush(6)
        stop
      endif
c
      deallocate( mm )
c
      return
      end subroutine landfill

      subroutine psmooth(a,mask,amn,amx,m,n)
      implicit none
c
      integer m,n,mask(m,n)
      real    a(m,n),amn(m,n),amx(m,n)
c
c --- smooth under mask and within amn,amx range.
c
      integer, allocatable :: mm(:,:)
      real,    allocatable :: aa(:,:)
c
      integer i,ii,j,jj,ki,kj
      real    rss,sa
c
      real    s(-1:1,-1:1)
      save    s
      data    s / 1.0, 2.0, 1.0,
     &            2.0, 4.0, 2.0,
     &            1.0, 2.0, 1.0 /
c
      rss = 1.0/sum(s(:,:))
c
c     local copy of a.
c
      allocate( aa(m,n) )
      aa = a
c
c     adding a halo to mm simplifies ocean selection logic.
c
      allocate( mm(0:m+1,0:n+1) )
      mm(  0, : ) = 0
      mm(m+1, : ) = 0
      mm( : ,  0) = 0
      mm( : ,n+1) = 0
      mm(1:m,1:n) = mask
c
      do j= 1,n
        do i= 1,m
          if     (mm(i,j).eq.1) then
            sa = 0.0
            do kj= -1,1
              jj = j+kj
              do ki= -1,1
                ii = i+ki
                if     (mm(ii,jj).eq.1) then
                  sa = sa + s(ki,kj)*aa(ii,jj)  ! must use local copy of a
                else
                  sa = sa + s(ki,kj)*aa(i ,j )
                endif
              enddo
            enddo
            a(i,j) = max( amn(i,j),
     &                    min( amx(i,j), sa*rss ) )
          endif
        enddo
      enddo
c
      deallocate( aa, mm )
c
      return
      end subroutine psmooth

      subroutine blkdat(idm_out,jdm_out,
     &                  smooth,icegln,
     &                  flnm_reg,flnm_map,flnm_in,flnm_tin,
     &                  flnm_out,flnm_top,
     &                  cline_out)
      use mod_xc  ! HYCOM communication interface
      implicit none
      integer       :: idm_out,jdm_out
      logical       :: smooth,icegln
      character*128 :: flnm_reg,flnm_map,flnm_in,flnm_tin,
     &                 flnm_out,flnm_top
      character*80  :: cline_out
c
c --- read blkdat.input for interpolated subregion.
c
      integer       :: iceflg
c
c --- 'flnm_reg'  = target sub-region grid       filename
c --- 'flnm_map'  = target sub-region grid map   filename
c --- 'flnm_top'  = target bathymetry filename, or 'NONE'
c --- 'flnm_tin'  = input  bathymetry filename, or 'NONE'
c --- 'flnm_in'   = input  archive    filename
c --- 'flnm_out'  = output archive    filename
c --- 'cline_out' = output title line (replaces preambl(5))
c
      read( *,'(a)')      flnm_reg
      write(6,'(a)') trim(flnm_reg)
      read( *,'(a)')      flnm_map
      write(6,'(a)') trim(flnm_map)
      read( *,'(a)')      flnm_top
      write(6,'(a)') trim(flnm_top)
      read( *,'(a)')      flnm_tin
      write(6,'(a)') trim(flnm_tin)
      read( *,'(a)')      flnm_in
      write(6,'(a)') trim(flnm_in)
      read( *,'(a)')      flnm_out
      write(6,'(a)') trim(flnm_out)
      read( *,'(a)')      cline_out
      write(6,'(a)') trim(cline_out)
      write(6,*)
      call flush(6)
c
c --- 'idm   ' = longitudinal array size
c --- 'jdm   ' = latitudinal  array size
c
      call blkini(idm_out,   'idm   ')
      call blkini(jdm_out,   'jdm   ')
c
c --- 'iceflg' = ice in output archive flag (0=none,1=energy loan model)
      call blkini(iceflg, 'iceflg')
      icegln = iceflg.eq.1
c
      write(6,*)
      call flush(6)
c
c --- 'smooth' = smooth interface depths (0=F,1=T)
      call blkinl(smooth,'smooth')
c
      write(6,*)
      call flush(6)
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
