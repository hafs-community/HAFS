      program isuba_gmap
      use mod_za  ! HYCOM I/O interface
      use mod_zb  ! HYCOM I/O interface for subregion
      implicit none
c
c     create an array index map to a diferent-grid subregion from a 
c     full region hycom file.
c
c     subregion grid is arbitrary, except that any part of this grid
c     that is outside the input grid must be land.
c
c     Alan J. Wallcraft,  NRL,  January 2009.
c
      character*80         :: chead
      character*256        :: flnm_map,flnm_reg
      integer              :: idm_out,jdm_out,
     &                        iref_out,jref_out,iref_in,jref_in
      integer              :: i,ii,ijsafe,ios,ip,itmp,ix,j,jj,jp,jq,l,
     &                        ni,nir,no,nor
      integer              :: k,l0,l1
      logical              :: laxis(2),laxis2,
     &                        lperiod(2),lcycle
      real                 :: hmina,hminb,hmaxa,hmaxb,
     &                        deg2rad,dist,distj,dist_max,
     &                        dx,dy,qdx,xp,yp,plat_min,plat_max
      integer, allocatable :: m_in(:,:),  m_out(:,:)
c
      real,    allocatable :: plat_in( :,:),plon_in( :,:)
      real,    allocatable :: plat_out(:,:),plon_out(:,:)
      integer, allocatable :: i_out(:,:),j_out(:,:)
      real,    allocatable :: x_out(:,:),y_out(:,:)
      real,    allocatable :: plat_in_min(:),plat_in_max(:)
c
      real*4,  parameter   ::  spval=    2.0**100
c
      real*8         ztecnf
      external       ztecnf,ztecng,ztecnp,ztecnb
c
      integer        its
      real*8         acc,err,step
      real*8         x2(2),w(6)
c
      logical        ldebug
      real*8         a,b
      common/zaecnb/ a(0:2,0:2),b(0:2,0:2),ldebug
      save  /zaecnb/
c
      call xcspmd
      call zaiost
      call blkdat(idm_out,jdm_out, chead, flnm_reg,flnm_map)
      call zbiost(idm_out,jdm_out)
c
      allocate(   m_in(idm,jdm),     m_out(idm_out,jdm_out) )
      allocate( plat_in(idm,jdm), plat_out(idm_out,jdm_out) )
      allocate( plon_in(idm,jdm), plon_out(idm_out,jdm_out) )
c                                                            
      allocate( plat_in_max(jdm), plat_in_min(jdm) )         
c                                                   
      allocate( i_out(idm_out,jdm_out), j_out(idm_out,jdm_out) )
      allocate( x_out(idm_out,jdm_out), y_out(idm_out,jdm_out) )
c       
c     read the input and output grid locations (no error checking).
c
      nir = 24
      call zaiopf('regional.grid.a','old', nir)
      call zaiord(plon_in,m_in,.false., hmina,hmaxa, nir)
      call zaiord(plat_in,m_in,.false., hmina,hmaxa, nir)
      call zaiocl(nir)
c     
      nor = 25
      call zbiopf(trim(flnm_reg),'old', nor)
      call zbiord(plon_out,m_out,.false., hmina,hmaxa, nor)
      call zbiord(plat_out,m_out,.false., hmina,hmaxa, nor)
      call zbiocl(nor)
c     
c     calculate the output grid location w.r.t. the input grid.
c
      lperiod(1) = maxval(plon_in(:,:))-
     &             minval(plon_in(:,:))  .gt. 350.0
      do j= 1,jdm
        if     (.not.lperiod(1)) then
          exit
        endif
        dy = mod( abs(plon_in(1,j) - plon_in(  3,j)), 360.0 )
        if     (dy.gt.180.0) then
          dy = 360.0 - dy  !abs distance
        endif
        dx = mod( abs(plon_in(1,j) - plon_in(idm,j)), 360.0 )
        if     (dx.gt.180.0) then
          dx = 360.0 - dx  !abs distance
        endif
        lperiod(1) = lperiod(1) .and. dx.lt.dy  !1 and idm closer than 1 and 3
      enddo
      if     (lperiod(1)) then
        write(6,'(a)') ' input domain assumed to be periodic'
      else
        write(6,'(a)') ' input domain assumed to be non-periodic'
      endif
c
      lperiod(2) = maxval(plon_out(:,:))-
     &             minval(plon_out(:,:))   .gt. 350.0
      do j= 1,jdm_out
        if     (.not.lperiod(2)) then
          exit
        endif
        dy = mod( abs(plon_out(1,j) - plon_out(      3,j)), 360.0 )
        if     (dy.gt.180.0) then
          dy = 360.0 - dy  !abs distance
        endif
        dx = mod( abs(plon_out(1,j) - plon_out(idm_out,j)), 360.0 )
        if     (dx.gt.180.0) then
          dx = 360.0 - dx  !abs distance
        endif
        lperiod(2) = lperiod(2) .and. dx.lt.dy  !1 and idm closer than 1 and 3
      enddo
      if     (lperiod(2)) then
        write(6,'(a)') 'output domain assumed to be periodic'
      else
        write(6,'(a)') 'output domain assumed to be non-periodic'
      endif
c       
      laxis(1) = .true.
      do i= 2,idm
        laxis(1) = laxis(1) .and.
     &             maxval(abs(plat_in(1,:)-plat_in(i,:))).le.1.e-2
        if     (.not.laxis(1)) then
          exit
        endif
      enddo
      do j= 2,jdm
        laxis(1) = laxis(1) .and.
     &             maxval(abs(plon_in(:,1)-plon_in(:,j))).le.1.e-2
        if     (.not.laxis(1)) then
          exit
        endif
      enddo
      if     (laxis(1)) then
        write(6,'(a)') ' input domain has 1-d lat/lon axes'
      else
        write(6,'(a)') ' input domain is curvilinear'
      endif
c
      laxis(2) = .true.
      do i= 2,idm_out
        laxis(2) = laxis(2) .and.
     &             maxval(abs(plat_out(1,:)-plat_out(i,:))).le.1.e-2
        if     (.not.laxis(2)) then
          exit
        endif
      enddo
      do j= 2,jdm_out
        laxis(2) = laxis(2) .and.
     &             maxval(abs(plon_out(:,1)-plon_out(:,j))).le.1.e-2
        if     (.not.laxis(2)) then
          exit
        endif
      enddo
      if     (laxis(2)) then
        write(6,'(a)') 'output domain has 1-d lat/lon axes'
      else
        write(6,'(a)') 'output domain is curvilinear'
      endif
      write(6,*)
      call flush(6)
c     
      laxis2 = laxis(1) .and. laxis(2)
c     
      do j= 1,jdm
        plat_in_min(j) = minval(plat_in(:,j))
        plat_in_max(j) = maxval(plat_in(:,j))
      enddo
      plat_min = minval(plat_in_min(:))
      plat_max = maxval(plat_in_max(:))
      dist_max = 0.0
      do j= 1,jdm-1
        do i= 1,idm-1
          dist_max = max( abs(plat_in(i,j)-plat_in(i+1,j)),
     &                    abs(plat_in(i,j)-plat_in(i,j+1)),
     &                    dist_max )
        enddo
      enddo
      dist_max = 2*dist_max  !target must be at least this close in latitude
c
      deg2rad = 4.d0*atan(1.d0)/180.d0  !pi/180
c       
      ldebug = .false.
c     
      do jj= 1,jdm_out
        do ii= 1,idm_out
          ldebug = mod(ii,idm_out/10).eq.1 .and.
     &             mod(jj,jdm_out/10).eq.1
c
          if     (laxis2 .and. ii.ne.1 .and. jj.ne.1) then
c         
c           shortcut for 1-d axes.
c
            i_out(ii,jj) = i_out(ii, 1)
            j_out(ii,jj) = j_out( 1,jj)
            x_out(ii,jj) = x_out(ii, 1)
            y_out(ii,jj) = y_out( 1,jj)
            if     (x_out(ii,jj).eq.spval) then
              y_out(ii,jj) = spval
            elseif (y_out(ii,jj).eq.spval) then !x_out.ne.spval
              x_out(ii,jj) = spval
            endif
            cycle
          endif
c           
c         find the nearest point by exhaustive search, but improve
c         efficiency by using plat_*_min/_max to exclude far away rows.
c
          xp   = plon_out(ii,jj)
          yp   = plat_out(ii,jj)
*         yp   = min(plat_max,max(plat_min,yp))  !in the input latitude range
          qdx  = max(0.001,abs(cos(yp*deg2rad)))
c         
c         start with a nearby point.
c
          if     (ii.eq.1) then
            if     (jj.eq.1) then
              ip = 1
              jp = 1
            else
              ip = i_out(1,jj-1)
              jp = j_out(1,jj-1)
            endif
          endif
          dy =      abs(plat_in(ip,jp) - yp)
          dx = mod( abs(plon_in(ip,jp) - xp), 360.0 )
          if     (dx.gt.180.0) then
            dx = 360.0 - dx
          endif
          dist = qdx*dx+dy
          if     (ldebug) then
            write(6,'(a,4i5,3f9.2)')
     &        'ii,jj,ip,jp,dx,dy,dist = ',
     &         ii,jj,ip,jp,dx,dy,dist
            call flush(6)
          endif
c           
          lcycle = .false.
          do jq= 0,jdm
            if     (jq.eq.0) then
              j = jp  ! search estimated row location first
            else
              j = jq
            endif
            distj = min(dist,dist_max)
            if     (.not. ldebug) then
              if     (yp.lt.plat_in_min(j)-distj .or.
     &                yp.gt.plat_in_max(j)+distj     ) then
                cycle  ! far away row
              endif
            else !debug
              if     (yp.lt.plat_in_min(j)-distj .or.
     &                yp.gt.plat_in_max(j)+distj     ) then
                if     (.not.lcycle) then
                  write(6,'(a,2i5,5x,i5,f9.2)')
     &              'ii,jj,j,dist (cycle-strt)',
     &               ii,jj,j,dist
                  call flush(6)
                elseif (jq.eq.jdm) then
                  write(6,'(a,2i5,5x,i5,f9.2)')
     &              'ii,jj,j,dist (cycle-stop)',
     &               ii,jj,j,dist
                  call flush(6)
                endif
                lcycle = .true.
                cycle  ! far away row
              else
                if     (lcycle) then
                  write(6,'(a,2i5,5x,i5,f9.2)')
     &              'ii,jj,j,dist (cycle-stop)',
     &               ii,jj,j-1,dist
                  call flush(6)
                endif
                lcycle = .false.
              endif
            endif !.not.ldebug;else
            if     (dist.eq.0.0) then
              exit   ! found exact location
            endif
            do i = 1,idm
              dy =      abs(plat_in(i,j) - yp)
              dx = mod( abs(plon_in(i,j) - xp), 360.0 )
              if     (dx.gt.180.0) then
                dx = 360.0 - dx
              endif
              if     (qdx*dx+dy.lt.dist) then
                ip   = i
                jp   = j
                dist = qdx*dx+dy
                if     (ldebug) then
                  write(6,'(a,4i5,3f9.2)')
     &              'ii,jj,ip,jp,dx,dy,dist = ',
     &               ii,jj,ip,jp,dx,dy,dist
                  call flush(6)
                endif
              endif
            enddo !i
          enddo !j
c           
c         convert nearest point into bilinear cell and distances.
c
          if     (dist.eq.0.0) then  !exact location
            if     (ip.eq.idm) then
              i_out(ii,jj) = ip-1
            else
              i_out(ii,jj) = ip
            endif
            x_out(ii,jj) = ip
            if     (jp.eq.jdm) then
              j_out(ii,jj) = jp-1
            else
              j_out(ii,jj) = jp
            endif
            y_out(ii,jj) = jp
          elseif (dist.gt.dist_max) then !outside grid, mark as data void
            if     (ip.eq.idm) then
              i_out(ii,jj) = ip-1
            else
              i_out(ii,jj) = ip
            endif
            x_out(ii,jj) = spval
            if     (jp.eq.jdm) then
              j_out(ii,jj) = jp-1
            else
              j_out(ii,jj) = jp
            endif
            y_out(ii,jj) = spval
          else  !standard case
c       
c           find exact location with napack routine(s).
c           over-kill for rectilinear, but neccessary for curvilinear grids.
c
            if     (ip.eq.1   .and. .not.lperiod(1)) then
              ip = 2
            elseif (ip.eq.idm .and. .not.lperiod(1)) then
              ip = idm-1
            endif
            if     (jp.eq.1) then
              jp = 2
            elseif (jp.eq.jdm) then
              jp = jdm-1
            endif
            do j= 0,2
              do i= 0,2
                ix = ip+i-1
                if     (lperiod(1)) then
                  if     (ix.eq.0) then
                    ix = idm
                  elseif (ix.eq.idm+1) then
                    ix = 1
                  endif
                endif
                b(i,j) =      plat_in(ix,jp+j-1) - yp
                a(i,j) = mod( plon_in(ix,jp+j-1) - xp, 360.0 )
                if     (a(i,j).lt.-180.0) then
                  a(i,j) = 360.0 + a(i,j)
                elseif (a(i,j).gt. 180.0) then
                  a(i,j) = a(i,j) - 360.0
                endif
                a(i,j) = qdx*a(i,j)
              enddo !i
            enddo !j
            if     (b(0,1).eq.b(1,1) .and. b(1,1).eq.b(2,1)) then !rectilinear
              x2(1) = ip - a(1,1)/(a(2,1)-a(1,1))
              x2(2) = jp - b(1,1)/(b(1,2)-b(1,1))
            else  !curvilinear
              step   = 0.0
              x2(1)  = 1.0
              x2(2)  = 1.0
              acc    = 1.e-3
              call cg(x2,err,its,step,acc,10,2,2,
     &                ztecnf,ztecng,ztecnb,ztecnp,w, ldebug)
              if     (its.lt.0) then  !very flat extrema
                x2(1)  = 1.0
                x2(2)  = 1.0
              elseif (min(x2(1),x2(2)).lt.-1.0 .or.
     &                max(x2(1),x2(2)).gt. 3.0     ) then  !very bad cg result
                x2(1)  = 1.0
                x2(2)  = 1.0
              endif
              x2(1) = ip + x2(1)-1.0
              x2(2) = jp + x2(2)-1.0
            endif !rectilinear:curvilinear
            if     (lperiod(1) .and. x2(1).gt.idm) then
              i_out(ii,jj) = idm
              x_out(ii,jj) = min( idm+1.d0, x2(1) )
            else
              i_out(ii,jj) = max( 1,    min( idm-1, int(x2(1)) ) )
              x_out(ii,jj) = max( 1.d0, 
     &                            min( i_out(ii,jj)+1.d0, x2(1) ) )
            endif
            j_out(ii,jj) = max( 1,    min( jdm-1, int(x2(2)) ) )
            y_out(ii,jj) = max( 1.d0,
     &                          min( j_out(ii,jj)+1.d0, x2(2) ) )
          endif !exact point:else
          if     (ldebug) then
            if     (x_out(ii,jj).ne.spval) then
              write(6,'(a,4i5,3f9.2)')
     &          'ii,jj,i_,j_,x_,y_,dist = ',
     &           ii,jj,i_out(ii,jj),j_out(ii,jj),
     &                 x_out(ii,jj),y_out(ii,jj),dist
            else
              write(6,'(a,4i5,2a,f9.2)')
     &          'ii,jj,i_,j_,x_,y_,dist = ',
     &           ii,jj,i_out(ii,jj),j_out(ii,jj),
     &                 '    spval', '    spval', dist
            endif
            call flush(6)
          endif !ldebug
        enddo !ii
      enddo !jj
c
c     output the grid map file.
c
      no = 15
      l  = len_trim(flnm_map)
      open (unit=no,file=flnm_map(1:l-2)//'.b',form='formatted',
     .      status='new',action='write')
      call zbiopf(flnm_map(1:l-2)//'.a','new', no)
c
      write(no,'(a)') trim(chead)
      call flush(no)
      write(6, *)
      write(6, *)
      write(6, '(a)') trim(chead)
      call flush(6)
c
      call zbiowr(x_out,m_out,.false., hmina,hmaxa, no, .false.)
      write(no,'(a,2f12.4)') 'xmap:  min,max =',hmina,hmaxa
      call flush(no)
      write(6, '(a,2f12.4)') 'xmap:  min,max =',hmina,hmaxa
      call flush(6)
c
      call zbiowr(y_out,m_out,.false., hmina,hmaxa, no, .false.)
      write(no,'(a,2f12.4)') 'ymap:  min,max =',hmina,hmaxa
      call flush(no)
      write(6, '(a,2f12.4)') 'ymap:  min,max =',hmina,hmaxa
      call flush(6)
c
      close(no)
      call zbiocl(no)
c
      end program isuba_gmap

      subroutine blkdat(idm_out,jdm_out, chead, flnm_reg,flnm_map)
      use mod_xc  ! HYCOM communication interface
      implicit none
      integer       :: idm_out,jdm_out
      character*80  :: chead
      character*256 :: flnm_reg,flnm_map
c
c --- read blkdat.input for interpolated subregion.
c
c --- 'flnm_reg'  = output grid       filename
c --- 'flnm_map'  = output grid map   filename
c --- 'chead   '  = single line header for grid map file
c
      read( *,'(a)')      flnm_reg
      write(6,'(a)') trim(flnm_reg)
      read( *,'(a)')      flnm_map
      write(6,'(a)') trim(flnm_map)
      write(6,*)
      read( *,'(a)')      chead
      write(6,'(a)') trim(chead)
      write(6,*)
      call flush(6)
c
c --- 'idm   ' = output longitudinal array size
c --- 'jdm   ' = output latitudinal  array size
c
      call blkini(idm_out,   'idm   ')
      call blkini(jdm_out,   'jdm   ')
      write(6,*)
      call flush(6)
c
      return
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

c
c --- user-level routines for napack's cg.
c
      real*8           function ztecnf(x)
      implicit none
c
      real*8           x(2)
c
c     wrapper for ztecmb.
c
      real*8           f,g(2)
c
      call ztecnb(f,g,x)
      ztecnf = f
      return
c     end of ztecnf.
      end
      subroutine ztecng(g,x)
      implicit none
c
      real*8           g(2),x(2)
c
c     wrapper for ztecmb.
c
      real*8           f
c
      call ztecnb(f,g,x)
      return
c     end of ztecng.
      end
      subroutine ztecnp(y,z)
      implicit none
c
      real*8           y(2),z(2)
c
c     null preconditioner
c
      y(1) = z(1)
      y(2) = z(2)
      return
c     end of ztecnp.
      end
      subroutine ztecnb(f,g,x)
cfpp$ noconcur r
      implicit none
c
      real*8         x(2),f,g(2)
c
      logical        ldebug
      real*8         a,b
      common/zaecnb/ a(0:2,0:2),b(0:2,0:2),ldebug
      save  /zaecnb/
c
c**********
c*
c  1) calculates function (f) and its gradient (g) at a point (x).
c
c  2) function defined in [0.,2.]*[0.,2.] via bi-linear fits to
c      a and b (passed via /zaecnb/) with the result abs(a)+abs(b).
c
c     this function is used for compatibility with bi-linear
c      interpolation from array index to lon,lat space.
c
c  3) passed to the minimization routine 'cg'.
c*
c**********
c
      integer ip,jp
      real*8  d1,d2,dx,dy,fx(2),fy(2)
c
c     choose the quadrent.
c
      if     (x(1).ge.1.0) then
        ip = 1
      else
        ip = 0
      endif
      if     (x(2).ge.1.0) then
        jp = 1
      else
        jp = 0
      endif
c
c     f  at  x(1),x(2)
c
      dx = x(1)-ip
      dy = x(2)-jp
      d1 = (1.d0-dx)*(1.d0-dy)*a(ip,  jp  ) +
     &     (1.d0-dx)*      dy *a(ip,  jp+1) +
     &           dx *(1.d0-dy)*a(ip+1,jp  ) +
     &           dx *      dy *a(ip+1,jp+1)
      d2 = (1.d0-dx)*(1.d0-dy)*b(ip,  jp  ) +
     &     (1.d0-dx)*      dy *b(ip,  jp+1) +
     &           dx *(1.d0-dy)*b(ip+1,jp  ) +
     &           dx *      dy *b(ip+1,jp+1)
      f  = sqrt( d1**2 + d2**2 )
c
c     1st derivatives.
c
      dx = x(1)-ip + 0.01
      dy = x(2)-jp
      d1 = (1.d0-dx)*(1.d0-dy)*a(ip,  jp  ) +
     &     (1.d0-dx)*      dy *a(ip,  jp+1) +
     &           dx *(1.d0-dy)*a(ip+1,jp  ) +
     &           dx *      dy *a(ip+1,jp+1)
      d2 = (1.d0-dx)*(1.d0-dy)*b(ip,  jp  ) +
     &     (1.d0-dx)*      dy *b(ip,  jp+1) +
     &           dx *(1.d0-dy)*b(ip+1,jp  ) +
     &           dx *      dy *b(ip+1,jp+1)
      fx(1) = sqrt( d1**2 + d2**2 )
c
      dx = x(1)-ip - 0.01
      dy = x(2)-jp
      d1 = (1.d0-dx)*(1.d0-dy)*a(ip,  jp  ) +
     &     (1.d0-dx)*      dy *a(ip,  jp+1) +
     &           dx *(1.d0-dy)*a(ip+1,jp  ) +
     &           dx *      dy *a(ip+1,jp+1)
      d2 = (1.d0-dx)*(1.d0-dy)*b(ip,  jp  ) +
     &     (1.d0-dx)*      dy *b(ip,  jp+1) +
     &           dx *(1.d0-dy)*b(ip+1,jp  ) +
     &           dx *      dy *b(ip+1,jp+1)
      fx(2) = sqrt( d1**2 + d2**2 )
c
      dx = x(1)-ip
      dy = x(2)-jp + 0.01
      d1 = (1.d0-dx)*(1.d0-dy)*a(ip,  jp  ) +
     &     (1.d0-dx)*      dy *a(ip,  jp+1) +
     &           dx *(1.d0-dy)*a(ip+1,jp  ) +
     &           dx *      dy *a(ip+1,jp+1)
      d2 = (1.d0-dx)*(1.d0-dy)*b(ip,  jp  ) +
     &     (1.d0-dx)*      dy *b(ip,  jp+1) +
     &           dx *(1.d0-dy)*b(ip+1,jp  ) +
     &           dx *      dy *b(ip+1,jp+1)
      fy(1) = sqrt( d1**2 + d2**2 )
c
      dx = x(1)-ip
      dy = x(2)-jp - 0.01
      d1 = (1.d0-dx)*(1.d0-dy)*a(ip,  jp  ) +
     &     (1.d0-dx)*      dy *a(ip,  jp+1) +
     &           dx *(1.d0-dy)*a(ip+1,jp  ) +
     &           dx *      dy *a(ip+1,jp+1)
      d2 = (1.d0-dx)*(1.d0-dy)*b(ip,  jp  ) +
     &     (1.d0-dx)*      dy *b(ip,  jp+1) +
     &           dx *(1.d0-dy)*b(ip+1,jp  ) +
     &           dx *      dy *b(ip+1,jp+1)
      fy(2) = sqrt( d1**2 + d2**2 )
c
      g(1) = (fx(1)-fx(2))/0.02
      g(2) = (fy(1)-fy(2))/0.02
c
*     if     (ldebug) then
*       write(6,*) '***** X,  = ',x(1),x(2)
*       write(6,*) '***** FX  = ',f,fx
*       write(6,*) '***** FY  = ',f,fy
*       write(6,*) '***** F,G = ',f,g(1),g(2)
*       call flush(6)
*     endif
      return
c     end of ztecnb.
      end
c
c      ________________________________________________________
c     |                                                        |
c     |   minimize a function using the fletcher-reeves form   |
c     |            of the conjugate gradient method            |
c     |            with (or without) preconditioning           |
c     |                                                        |
c     |    input:                                              |
c     |                                                        |
c     |         x     --array containing starting guess        |
c     |                                                        |
c     |         step  --starting guess for minimizer in direc- |
c     |                 tion of negative gradient during first |
c     |                 iteration (e. g. step=1) when step=0,  |
c     |                 the program selects a starting guess   |
c     |                                                        |
*     |         t     --computing tolerance (iterations stop   |
*     |                 when max-norm of gradient .le. t)      |
c     |         tt    --computing tolerance (iterations stop   |
c     |                 when function result .le. t)           |
c     |                                                        |
c     |         limit --maximum number of iterations           |
c     |                                                        |
c     |         n     --number of unknowns                     |
c     |                                                        |
c     |         m     --number of iterations until the search  |
c     |                 directions are renormalized along the  |
c     |                 negative gradient (typically, m = n)   |
c     |                                                        |
c     |         value --name of cost evaluation func. routine  |
c     |                 (external in main program)             |
c     |                 value(x) is value of cost at x         |
c     |                                                        |
c     |         grad  --name of gradient evaluation subroutine |
c     |                 (external in main program)             |
c     |                 grad(g,x) puts in g the gradient at x  |
c     |                                                        |
c     |         both  --name subroutine to evaluate both cost  |
c     |                 and its gradient (external in main     |
c     |                 program) both(v,g,x) puts the value in |
c     |                 v and the gradient in g for the point x|
c     |                                                        |
c     |         pre   --name of preconditioning subroutine     |
c     |                 (external in main program)             |
c     |                 pre(y,z) applies the preconditioner to |
c     |                 z, storing the result in y.            |
c     |                 if preconditioning not used set y = z  |
c     |                                                        |
c     |         h     --work array (length at least 3n)        |
c     |                                                        |
c     |    output:                                             |
c     |                                                        |
c     |         x     --minimizer                              |
c     |                                                        |
*     |         e     --max-norm of gradient                   |
c     |         ee    --function result
c     |                                                        |
c     |         it    --number of iterations performed         |
c     |                                                        |
c     |         step  --step size along search direction for   |
c     |                 final iteration                        |
c     |                                                        |
c     |    builtin functions: dabs,dexp,idint,dlog,dsqrt,dmax1,|
c     |                         dmin1,dsign                    |
c     |    package routines: cub,fd,fv,fvd,ins                 |
c     |________________________________________________________|
c
*     subroutine cg(x,e,it,step,t,limit,n,m,value,grad,both,pre,h)
      subroutine cg(x,ee,it,step,tt,limit,n,m,
     &              value,grad,both,pre,h, ldebug)
      implicit none
      logical ldebug
      integer i,iq,it,j,k,l,limit,m,n,na,nb,nc,nd
      real*8 h(n,*),x(*),y(50),z(50),a1,a2,a3,a4,a5,a6,a7,a8,a,b,c,c0,c1
      real*8 d,d0,da,db,e,f,f0,f1,fa,fb,fc,g,l3,p,q,r,s,step,t,v,w
      real*8 tt,ee
      real*8 fv,fd,value
      external both,grad,pre,value
      data a1/.1d0/,a2/.9d0/,a3/5.d0/,a4/.2d0/,a5/10.d0/,a6/.9d0/
      data a7/.3d0/
      a8 = a3 + .01d0
      it = 0
      call both(f,h(1,3),x)
      e = 0.
      do 10 i = 1,n
10         if ( dabs(h(i,3)) .gt. e ) e = dabs(h(i,3))
*     if ( e .le. t ) return
      ee = f
      if (ee .le. tt) return
      l3 = 1./dlog(a3)
      call pre(h(1,2),h(1,3))
      a = step
      if ( a .gt. 0. ) goto 30
      do 20 i = 1,n
20         if ( dabs(x(i)) .gt. a ) a = dabs(x(i))
      a = .01*a/e
      if ( a .eq. 0. ) a = 1.
30    g = 0.
      do 40 i = 1,n
40         g = g + h(i,2)*h(i,3)
      if ( g .lt. 0. ) goto 620
50    l = 0
      do 60 i = 1,n
60         h(i,1) = -h(i,2)
      d = -g
70    fa = fv(a,x,h,n,value)
      c0 = a
      f0 = fa
      j = 2
      y(1) = 0.
      z(1) = f
      y(2) = a
      z(2) = fa
      v = a1*d
      w = a2*d
      iq = 0
      if ( fa .le. f ) goto 80
      c = a
      b = 0.
      a = 0.
      fc = fa
      fb = f
      fa = f
      goto 90
80    c = 0.
      b = 0.
      fc = f
      fb = f
      iq = 1
90    na = 0
      nb = 0
      nc = 0
      nd = 0
      q = (d+(f-f0)/c0)/c0
      if ( q .lt. 0. ) goto 110
      q = a
100   nd = nd + 1
      if ( nd .gt. 25 ) goto 610
      q = a3*q
      p = fv(q,x,h,n,value)
      call ins(q,p,a,b,c,fa,fb,fc,j,y,z)
      if ( p-f .lt. w*q ) goto 100
      goto 260
110   q = .5*d/q
      if ( q .lt. .01*c0 ) q = .01*c0
      p = fv(q,x,h,n,value)
      if ( p .le. f0 ) goto 120
      f1 = f0
      c1 = c0
      f0 = p
      c0 = q
      goto 130
120   f1 = p
      c1 = q
130   call ins(q,p,a,b,c,fa,fb,fc,j,y,z)
135   if ( a .eq. 0. ) goto 140
      if ( fa-f .ge. v*a ) goto 160
      if ( fa-f .lt. w*a ) goto 210
      goto 280
140   q = c0
      if ( c1 .lt. q ) q = c1
150   na = na + 1
      if ( na .gt. 25 ) goto 630
      q = a4*q
      p = fv(q,x,h,n,value)
      call ins(q,p,a,b,c,fa,fb,fc,j,y,z)
      if ( p-f .ge. v*q ) goto 150
      goto 250
160   if ( c0 .gt. c1 ) goto 200
      if ( f0-f .gt. v*c0 ) goto 180
      if ( f0-f .ge. w*c0 ) goto 320
      if ( c1 .le. a5*c0 ) goto 320
      r = dlog(c1/c0)
      s = -idint(r*l3+.999)
      r = .999*dexp(r/s)
      q = c1
170   q = q*r
      if ( q .lt. c0 ) goto 320
      p = fv(q,x,h,n,value)
      call ins(q,p,a,b,c,fa,fb,fc,j,y,z)
      na = na + 1
      if ( na .gt. 25 ) goto 630
      if ( p-f .gt. v*q ) goto 170
      goto 320
180   q = c0
190   na = na + 1
      if ( na .gt. 25 ) goto 630
      q = a4*q
      p = fv(q,x,h,n,value)
      call ins(q,p,a,b,c,fa,fb,fc,j,y,z)
      if ( p-f .ge. v*q ) goto 190
      goto 250
200   q = a
      goto 190
210   if ( c0 .lt. c1 ) goto 290
      if ( f0-f .ge. v*c0 ) goto 230
      if ( f0-f .ge. w*c0 ) goto 250
      q = c0
220   nd = nd  + 1
      if ( nd .gt. 25 ) goto 610
      q = a3*q
      p = fv(q,x,h,n,value)
      call ins(q,p,a,b,c,fa,fb,fc,j,y,z)
      if ( p-f .lt. w*q ) goto 220
      goto 250
230   if ( c0 .le. a5*c1 ) goto 250
      r = dlog(c0/c1)
      s = idint(r*l3+.999)
      r = 1.001*dexp(r/s)
      q = a
240   q = q*r
      if ( q .gt. c0 ) goto 250
      nd = nd + 1
      p = fv(q,x,h,n,value)
      call ins(q,p,a,b,c,fa,fb,fc,j,y,z)
      if ( p-f .lt. w*q ) goto 240
250   if ( iq .eq. 1 ) goto 320
260   if ( b .eq. 0. ) goto 280
      if ( c .eq. 0. ) goto 270
      v = c - a
      w = a - b
      r = 1./v
      s = 1./w
      p = fc - fa
      q = fb - fa
      e = p*r + q*s
      if ( dsign(e,c-b) .ne. e ) goto 320
      if ( e .eq. 0. ) goto 320
      q = (p*r)*w - (q*s)*v
      q = a - .5*q/e
      p = fv(q,x,h,n,value)
      call ins(q,p,a,b,c,fa,fb,fc,j,y,z)
      goto 320
270   r = 1./a
      s = 1./b
      p = r*(fa-f) - d
      q = s*(fb-f) - d
      e = a - b
      v = (r*p-s*q)/e
      w = (a*q*s-b*p*r)/e
      v = w*w-3.*v*d
      if ( v .lt. 0. ) v = 0.
      v = dsqrt(v)
      if ( w+v .eq. 0. ) goto 320
      q = -d/(w+v)
      if ( q .le. 0. ) goto 320
      p = fv(q,x,h,n,value)
      call ins(q,p,a,b,c,fa,fb,fc,j,y,z)
      goto 320
280   if ( iq .eq. 1 ) goto  320
      q = (d+(f-fa)/a)/a
      if ( q .ge. 0. ) goto 320
      q = .5*d/q
      p = fv(q,x,h,n,value)
      call ins(q,p,a,b,c,fa,fb,fc,j,y,z)
      goto 320
290   if ( f0-f .gt. v*c0 ) goto 300
      if ( f0-f .gt. w*c0 ) goto 320
300   q = a
310   nd = nd + 1
      if ( nd .gt. 25 ) goto 610
      q = a3*q
      p = fv(q,x,h,n,value)
      call ins(q,p,a,b,c,fa,fb,fc,j,y,z)
      if ( p-f .lt. w*q ) goto 310
      goto 250
320   da = fd(a,x,h,n,grad)
      if ( da .gt. a6*g ) goto 410
      if ( da .ge. 0. ) goto 560
      r = a
      q = 0.
      do 330 i = 1,j
           if ( y(i) .gt. a ) goto 370
           if ( y(i) .le. q ) goto 330
           if ( y(i) .eq. a ) goto 330
           q = y(i)
330   continue
      if ( a .le. a8*q ) goto 560
      q = a
340   nd = nd + 1
      if ( nd .gt. 25 ) goto 610
      q = a3*q
      p = fv(q,x,h,n,value)
      f1 = fa
      call ins(q,p,a,b,c,fa,fb,fc,j,y,z)
      if ( p .lt. f1 ) goto 340
      if ( a .gt. r ) goto 360
      do 350 i = 1,n
350        h(i,2) = x(i) + a*h(i,1)
      goto 560
360   da = fd(a,x,h,n,grad)
      if ( da .gt. a6*g ) goto 410
      goto 560
370   q = y(i)
      do 380 k = i,j
           if ( y(k) .le. a ) goto 380
           if ( y(k) .lt. q ) q = y(k)
380   continue
      if ( q .le. a5*a ) goto 560
      f0 = dlog(q/a)
      s = idint(f0*l3+.999)
      f0 = 1.001*dexp(f0/s)
      s = a
390   s = s*f0
      if ( s .ge. q ) goto 320
      p = fv(s,x,h,n,value)
      f1 = fa
      call ins(s,p,a,b,c,fa,fb,fc,j,y,z)
      if ( p .lt. f1 ) goto 390
      if ( a .gt. r ) goto 320
      do 400 i = 1,n
400        h(i,2) = x(i) + a*h(i,1)
      goto 560
410   b = 0.
      k = 1
      i = k
420   i = i + 1
      if ( i .gt. j ) goto 430
      if ( y(i) .ge. a ) goto 420
      if ( y(i) .lt. b ) goto 420
      b = y(i)
      k = i
      goto 420
430   fb = z(k)
      db = d
      if ( b .ne. 0. ) db = fd(b,x,h,n,grad)
440   w = 2.*dabs(b-a)
      call cub(c,a,b,fa,fb,da,db)
      nc = 1
      goto 480
450   w = .5*w
      if ( w .lt. dabs(c0-c) ) goto 550
      if ( c0 .lt. c ) goto 460
      if ( d0 .ge. d ) goto 470
      goto 550
460   if ( d0 .gt. d ) goto 550
470   call cub(c,c,c0,f,f0,d,d0)
      nc = nc + 1
      if ( nc .gt. 30 ) goto 600
480   r = dmax1(a,b)
      s = dmin1(a,b)
      if ( c .gt. r ) goto 490
      if ( c .gt. s ) goto 500
      c = s + (s-c)
      s = .5*(a+b)
      if ( c .gt. s ) c = s
      goto 500
490   c = r - (c-r)
      s = .5*(a+b)
      if ( c .lt. s ) c = s
500   c0 = a
      f0 = fa
      d0 = da
      call fvd(f,d,c,x,h,n,both)
      if ( f .lt. fa ) goto 510
      b = c
      fb = f
      db = d
      goto 450
510   if ( c .lt. a ) goto 540
      if ( d .lt. 0. ) goto 530
520   b = a
      fb = fa
      db = da
530   a = c
      fa = f
      da = d
      if ( d .gt. a6*g ) goto 450
      goto 560
540   if ( d .lt. 0. ) goto 520
      goto 530
550   c = .5*(a+b)
      nb = nb + 1
      w = dabs(b-a)
      goto 500
560   e = 0.
      do 570 i = 1,n
           if ( dabs(h(i,3)) .gt. e ) e = dabs(h(i,3))
570        x(i) = h(i,2)
      it = it + 1
*     if ( e .le. t ) goto 660
      ee = f
      if     (ldebug) then
        write(6,'(a,i4,1pg20.10)')
     &    'cg - it,ee =',it,ee
        call flush(6)
      endif
      if ( ee .le. tt ) goto 660
      if ( it .ge. limit ) goto 660
      f = fa
      d = da
      a = a7*a
      call pre(h(1,2),h(1,3))
      r = 0.
      do 580 i = 1,n
580        r = r + h(i,2)*h(i,3)
      if ( r .lt. 0. ) goto 620
      s = r/g
      g = r
      l = l + 1
      if ( l .ge. m ) goto 50
      d = 0.
      do 590 i = 1,n
           h(i,1) = -h(i,2) + s*h(i,1)
590        d = d + h(i,1)*h(i,3)
      goto 70
600   if ( d .lt. g ) goto 560
        if     (ldebug) then
          write(6,*) 'UNABLE TO OBTAIN DESCENT DIRECTION'
        endif
*       stop
        it = -1
        return
610   continue
        if     (ldebug) then
          write(6,*) 'THE FUNCTION DECREASES WITH NO MINIMUM'
        endif
*       stop
        it = -1
        return
620   continue
        if     (ldebug) then
          write(6,*) 'PRECONDITIONER NOT POSITIVE DEFINITE'
        endif
*       stop
        it = -1
        return
630   continue
      q = q*a3**25
      nd = 0
640   nd = nd + 1
      if ( nd .gt. 25 ) goto 650
      q = a3*q
      p = fv(q,x,h,n,value)
      call ins(q,p,a,b,c,fa,fb,fc,j,y,z)
      if ( p-f .gt. v*q ) goto 640
      goto 135
650   continue
        if     (ldebug) then
          write(6,*) 'UNABLE TO SATISFY ARMIJO CONDITION'
        endif
        it = -1
        return
660   continue
      step = a
      return
      end
      real*8 function fv(a,x,h,n,value)
      real*8 h(n,*),x(*),a,value
      external value
      do 10 i = 1 , n
10         h(i,2) = x(i) + a*h(i,1)
      fv = value(h(1,2))
      return
      end
      real*8 function fd(a,x,h,n,grad)
      real*8 h(n,*),x(*),a,d
      external grad
      do 10 i = 1 , n
10         h(i,2) = x(i) + a*h(i,1)
      call grad(h(1,3),h(1,2))
      d = 0.
      do 20 i = 1,n
20         d = d + h(i,1)*h(i,3)
      fd = d
      return
      end
      subroutine fvd(v,d,a,x,h,n,both)
      implicit none
      integer  n
      real*8   h(n,*),x(*),a,d,v
      external both
      integer  i
      do 10 i = 1 , n
10         h(i,2) = x(i) + a*h(i,1)
      call both(v,h(1,3),h(1,2))
      d = 0.
      do 20 i = 1,n
20         d = d + h(i,1)*h(i,3)
      return
      end
      subroutine cub(x,a,b,c,d,e,f)
      implicit none
      real*8 a,b,c,d,e,f,g,v,w,x,y,z
      g = b - a
      if ( g .eq. 0. ) goto 50
      v = e + f - 3*(d-c)/g
      w = v*v-e*f
      if ( w .lt. 0. ) w = 0.
      w = dsign(dsqrt(w),g)
      y = e + v
      z = f + v
      if ( dsign(y,g) .ne. y ) goto 30
      if ( dsign(z,g) .ne. z ) goto 20
      if ( z .eq. 0. ) goto 20
10    x = b - g*f/(z+w)
      return
20    if ( c .lt. d ) x = a
      if ( c .ge. d ) x = b
      return
30    if ( dsign(z,g) .ne. z ) goto 40
      if ( dabs(e) .gt. dabs(f) ) goto 10
40    x = a + g*e/(y-w)
      return
50    x = a
      return
      end
      subroutine ins(s,f,a,b,c,fa,fb,fc,j,y,z)
      implicit none
      real*8 a,b,c,f,fa,fb,fc,s,y(*),z(*)
      integer j
      j = j + 1
      y(j) = s
      z(j) = f
      if ( f .le. fa ) goto 20
      if ( f .le. fb ) goto 10
      if ( f .gt. fc ) return
      c = s
      fc = f
      return
10    c = b
      b = s
      fc = fb
      fb = f
      return
20    c = b
      b = a
      a = s
      fc = fb
      fb = fa
      fa = f
      return
      end
