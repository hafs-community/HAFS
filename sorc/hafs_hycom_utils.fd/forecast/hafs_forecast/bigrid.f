      subroutine bigrid(depth, mapflg, util1,util2,util3)
      use mod_xc  ! HYCOM communication interface
      implicit none
c
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
     &        depth,util1,util2,util3
      integer mapflg
c
c --- set loop bounds for irregular basin in c-grid configuration
c --- q,u,v,p are vorticity, u-velocity, v-velocity, and mass points, resp.
c --- 'depth' = basin depth array, zero values indicate land
c
      integer    nchar
      parameter (nchar=120)
      logical   lperiod,larctic,lfplane
c
      integer   i,j,nzero,isec,ifrst,ilast
      real      rnfill,aline(nchar)
      real      depmax
      character char3*3
c
      character fmt*13
      data fmt/'(i4,1x,120i1)'/
c
c --- is the domain periodic in longitude?
      depmax=0.0
      if     (i0+ii.eq.itdm) then
        do j= 1,jj
          depmax=max(depmax,depth(ii,j))
        enddo
      endif
      call xcmaxr(depmax)
      lperiod=depmax.gt.0.0
c
c --- is the domain periodic in latitude?
c --- only allowed on f-plane or full globe (across the arctic).
      depmax=0.0
      if     (j0+jj.eq.jtdm) then
        do i= 1,ii
          depmax=max(depmax,depth(i,jj))
        enddo
      endif
      call xcmaxr(depmax)
      larctic=depmax.gt.0.0 .and. mapflg.ne.4
      lfplane=depmax.gt.0.0 .and. mapflg.eq.4
c
c --- is this consistent with nreg (from mod_xc)?
      if     (larctic) then
        if     (.not.lperiod) then
          if (mnproc.eq.1) then
            write(lp,'(/a/)') 
     &       'arctic   domain, but non-periodic'
            call flush(lp)
          endif
          call xcstop('(bigrid)')
                 stop '(bigrid)'
        else
          nreg = 2
        endif
      elseif (lperiod) then
        if     (nreg.eq.-1) then  ! TYPE=one or TYPE=omp
          if     (lfplane) then
            nreg = 3  ! periodic/f-plane
          else
            nreg = 1  ! periodic/closed
          endif
        elseif (nreg.eq. 0) then
          if (mnproc.eq.1) then
            write(lp,'(/a/)') 
     &       'periodic domain, but with nreg.eq.0'
            call flush(lp)
          endif
          call xcstop('(bigrid)')
                 stop '(bigrid)'
        endif
      else
        if     (nreg.eq.-1) then  ! TYPE=one or TYPE=omp
          if     (lfplane) then
            nreg = 4  ! closed/f-plane
          else
            nreg = 0  ! closed/closed
          endif
        elseif (nreg.ne. 0) then
          if (mnproc.eq.1) then
            write(lp,'(/a/)') 
     &       'closed   domain, but with nreg.ne.0'
            call flush(lp)
          endif
          call xcstop('(bigrid)')
                 stop '(bigrid)'
        endif
      endif
c
      if     (mnproc.eq.1) then
        write(lp,'(/a,i2)') 'bigrid: nreg =',nreg
        if     (.not.lperiod) then
          if     (lfplane) then
            write(lp,'(a/)') 'bigrid: infinate closed basin'
          elseif (.not.larctic) then
            write(lp,'(a/)') 'bigrid: closed basin'
          else
            write(lp,'(a/)') 'bigrid: closed basin, arctic overlap'
          endif
        else
          if     (lfplane) then
            write(lp,'(a/)') 'bigrid: doubly infinate basin'
          elseif (.not.larctic) then
            write(lp,'(a/)') 'bigrid: periodic basin'
          else
            write(lp,'(a/)') 'bigrid: global basin, arctic overlap'
          endif
        endif
        call flush(lp)
      endif
c
c --- nreg is defined, so now safe to update halo
      call xctilr(depth,1,1, nbdy,nbdy, halo_ps)
c
c --- allow for non-periodic and non-arctic boundaries (part I).
      if     (.not.lfplane .and. j0.eq.0) then
c ---   south boundary is all land.
        do j=1-nbdy,0
          do i=1-nbdy,ii+nbdy
            depth(i,j) = 0.0
          enddo
        enddo
      endif
c
      if     (.not.lfplane .and. .not.larctic .and. j0+jj.eq.jtdm) then
c ---   north boundary is all land.
        do j=jj+1,jj+nbdy
          do i=1-nbdy,ii+nbdy
            depth(i,j) = 0.0
          enddo
        enddo
      endif
c
      if     (.not.lperiod .and. i0.eq.0) then
c ---   west boundary is all land.
        do j=1-nbdy,jj+nbdy
          do i=1-nbdy,0
            depth(i,j) = 0.0
          enddo
        enddo
      endif
c
      if     (.not.lperiod .and. i0+ii.eq.itdm) then
c ---   east boundary is all land.
        do j=1-nbdy,jj+nbdy
          do i=ii+1,ii+nbdy
            depth(i,j) = 0.0
          enddo
        enddo
      endif
c
c --- detect (and abort on) single-width inlets and 1-point seas.
      rnfill=0.0
      do j=1,jj
        do i=1,ii
          nzero=0
          if (depth(i,j).gt.0.0) then
            if (depth(i-1,j).le.0.0) nzero=nzero+1
            if (depth(i+1,j).le.0.0) nzero=nzero+1
            if (depth(i,j-1).le.0.0) nzero=nzero+1
            if (depth(i,j+1).le.0.0) nzero=nzero+1
************if     (nzero.ge.3) then
            if     (nzero.eq.4) then
              write (lp,'(a,i4,a,i4,a,i1,a)')
     &          'error   - dh(',i0+i,',',j0+j,') has ',
     &          nzero,' land nieghbours'
              rnfill=rnfill+1.0
            elseif (nzero.eq.3) then
              write (lp,'(a,i4,a,i4,a,i1,a)')
     &          'warning - dh(',i0+i,',',j0+j,') has ',
     &          nzero,' land nieghbours'
*             rnfill=rnfill+1.0  !only a warning, don't update rnfill
            end if
          end if
        enddo
      enddo
      call xcsync(flush_lp)
      call xcmaxr(rnfill)
      if (rnfill.gt.0.0) then
        if (mnproc.eq.1) then
          write(lp,'(/a/)') 
     &     'Must correct bathymetry before running HYCOM'
          call flush(lp)
        endif
        call xcstop('(bigrid)')
               stop '(bigrid)'
      endif
c
c --- start out with masks as land everywhere
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-nbdy,jdm+nbdy
        do i=1-nbdy,idm+nbdy
          ip(i,j)=0
          iq(i,j)=0
          iu(i,j)=0
          iv(i,j)=0
        enddo
      enddo
c
c --- mass points are defined where water depth is greater than zero
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-nbdy,jj+nbdy
        do i=1-nbdy,ii+nbdy
          if (depth(i,j).gt.0.) then
            ip(i,j)=1
          endif
        enddo
      enddo
c
c --- u,v points are located halfway between any 2 adjoining mass points
c --- 'interior' q points require water on all 4 sides.
c --- 'promontory' q points require water on 3 (or at least 2
c --- diametrically opposed) sides
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        do i=1,ii
          if (ip(i-1,j).gt.0.and.ip(i,j).gt.0) then
            iu(i,j)=1
          endif
          if (ip(i,j-1).gt.0.and.ip(i,j).gt.0) then
            iv(i,j)=1
          endif
          if (min(ip(i,j),ip(i-1,j),ip(i,j-1),ip(i-1,j-1)).gt.0) then
            iq(i,j)=1
          elseif ((ip(i  ,j).gt.0.and.ip(i-1,j-1).gt.0).or.
     &            (ip(i-1,j).gt.0.and.ip(i  ,j-1).gt.0)    ) then
            iq(i,j)=1
          endif
          util1(i,j)=iu(i,j)
          util2(i,j)=iv(i,j)
          util3(i,j)=iq(i,j)
        enddo
      enddo
      call xctilr(util1,1,1, nbdy,nbdy, halo_us)
      call xctilr(util2,1,1, nbdy,nbdy, halo_vs)
      call xctilr(util3,1,1, nbdy,nbdy, halo_qs)
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j= 1-nbdy,jj+nbdy
        do i= 1-nbdy,ii+nbdy
          iu(i,j)=util1(i,j)
          iv(i,j)=util2(i,j)
          iq(i,j)=util3(i,j)
        enddo
      enddo
c
c --- allow for non-periodic and non-arctic boundaries (part II).
      if     (.not.lfplane .and. j0.eq.0) then
c ---   south boundary is all land.
        do j=1-nbdy,0
          do i=1-nbdy,ii+nbdy
            iq(i,j) = 0
            iu(i,j) = 0
            iv(i,j) = 0
          enddo
        enddo
      endif
c
      if     (.not.lfplane .and. .not.larctic .and. j0+jj.eq.jtdm) then
c ---   north boundary is all land.
        do j=jj+1,jj+nbdy
          do i=1-nbdy,ii+nbdy
            iq(i,j) = 0
            iu(i,j) = 0
            iv(i,j) = 0
          enddo
        enddo
      endif
c
      if     (.not.lperiod .and. i0.eq.0) then
c ---   west boundary is all land.
        do j=1-nbdy,jj+nbdy
          do i=1-nbdy,0
            iq(i,j) = 0
            iu(i,j) = 0
            iv(i,j) = 0
          enddo
        enddo
      endif
c
      if     (.not.lperiod .and. i0+ii.eq.itdm) then
c ---   east boundary is all land.
        do j=1-nbdy,jj+nbdy
          do i=ii+1,ii+nbdy
            iq(i,j) = 0
            iu(i,j) = 0
            iv(i,j) = 0
          enddo
        enddo
      endif
c
c --- logical alliX indicates entire row is sea
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-nbdy+1,jj+nbdy-1
        allip(j) = .true.
        alliq(j) = .true.
        alliu(j) = .true.
        alliv(j) = .true.
        do i=1-nbdy,ii+nbdy
c ---     are all iX(:,j) sea points?
          allip(j) = allip(j) .and. ip(i,j).ne.0
          alliq(j) = alliq(j) .and. iq(i,j).ne.0
          alliu(j) = alliu(j) .and. iu(i,j).ne.0
          alliv(j) = alliv(j) .and. iv(i,j).ne.0
        enddo !i
      enddo !j
c
c --- determine sea-only i-1, i+1, j-1, and j+1 indexes
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-nbdy+1,jj+nbdy-1
        do i=1-nbdy+1,ii+nbdy-1
c ---     W,E,S,N if sea, otherwise center point
          if     (ip(i-1,j).ne.0) then
            ipim1(i,j) = i-1
          else
            ipim1(i,j) = i
          endif
          if     (ip(i+1,j).ne.0) then
            ipip1(i,j) = i+1
          else
            ipip1(i,j) = i
          endif
          if     (ip(i,j-1).ne.0) then
            ipjm1(i,j) = j-1
          else
            ipjm1(i,j) = j
          endif
          if     (ip(i,j+1).ne.0) then
            ipjp1(i,j) = j+1
          else
            ipjp1(i,j) = j
          endif
c
c ---     W,E,S,N if sea, otherwise E,W,N,S if sea, otherwise center point
          if     (ip(i-1,j).ne.0) then
            ipim1x(i,j) = i-1
          elseif (ip(i+1,j).ne.0) then
            ipim1x(i,j) = i+1
          else
            ipim1x(i,j) = i
          endif
          if     (ip(i+1,j).ne.0) then
            ipip1x(i,j) = i+1
          elseif (ip(i-1,j).ne.0) then
            ipip1x(i,j) = i-1
          else
            ipip1x(i,j) = i
          endif
          if     (ip(i,j-1).ne.0) then
            ipjm1x(i,j) = j-1
          elseif (ip(i,j+1).ne.0) then
            ipjm1x(i,j) = j+1
          else
            ipjm1x(i,j) = j
          endif
          if     (ip(i,j+1).ne.0) then
            ipjp1x(i,j) = j+1
          elseif (ip(i,j-1).ne.0) then
            ipjp1x(i,j) = j-1
          else
            ipjp1x(i,j) = j
          endif
        enddo !i
      enddo !j
c
c --- determine loop bounds for vorticity points, including interior and
c --- promontory points
      call indxi(iq,ifq,ilq,isq)
      call indxj(iq,jfq,jlq,jsq)
c
c --- determine loop indices for mass and velocity points
      call indxi(ip,ifp,ilp,isp)
      call indxj(ip,jfp,jlp,jsp)
      call indxi(iu,ifu,ilu,isu)
      call indxj(iu,jfu,jlu,jsu)
      call indxi(iv,ifv,ilv,isv)
      call indxj(iv,jfv,jlv,jsv)
c
c --- write out  -ip-  array, if it is not too big
c --- data are written in strips nchar points wide
      if     (max(itdm,jtdm).le.2*nchar) then
        util1(1:ii,1:jj) = ip(1:ii,1:jj)  ! xclget is for real arrays
        isec=(itdm-1)/nchar
        do ifrst=0,nchar*isec,nchar
          ilast=min(itdm,ifrst+nchar)
          write (char3,'(i3)') ilast-ifrst
          fmt(8:10)=char3
          if     (mnproc.eq.1) then
          write (lp,'(a,i5,a,i5)') 
     &      'ip array, cols',ifrst+1,' --',ilast
          endif
          do j= jtdm,1,-1
            call xclget(aline,ilast-ifrst, util1,ifrst+1,j,1,0, 1)
            if     (mnproc.eq.1) then
            write (lp,fmt) j,(10*nint(aline(i)),i=1,ilast-ifrst)
            endif
          enddo
        enddo
        if     (mnproc.eq.1) then
        write (lp,*)
        endif
        call xcsync(flush_lp)
      endif  ! small region
c
      return
      end
c
c
      subroutine indxi(ipt,if,il,is)
      use mod_xc  ! HYCOM communication interface
      implicit none
c
      integer, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
     &         ipt
      integer, dimension (1-nbdy:jdm+nbdy,ms) ::
     &         if,il
      integer, dimension (1-nbdy:jdm+nbdy) ::
     &         is
c
c --- input array ipt contains 1 at grid point locations, 0 elsewhere
c --- output is arrays if, il, is  where
c --- if(j,k) gives row index of first point in column j for k-th section
c --- il(j,k) gives row index of last point
c --- is(j) gives number of sections in column j (maximum: ms)
c
      integer i,j,k,last
c
      do j=1-nbdy,jj+nbdy
        is(j) = 0
        do k=1,ms
          if(j,k) = 0
          il(j,k) = 0
        end do
c
        k=1
        last = ipt(1-nbdy,j)
        if     (last .eq. 1) then
          if(j,k) = 1-nbdy
        endif
        do i=2-nbdy,ii+nbdy
          if      (last .eq. 1 .and. ipt(i,j) .eq. 0) then
            il(j,k) = i-1
            k = k+1
          elseif (last .eq. 0 .and. ipt(i,j) .eq. 1) then
            if     (k .gt. ms) then
              write(lp,'(a,i5)')  'indxi problem on proc ',mnproc
              write(lp,'(a,2i5)') 
     &          ' error in indxi -- ms too small at i,j =',i0+i,j0+j
              call xchalt('(indxi)')
                     stop '(indxi)'
            endif
            if(j,k) = i
          endif
          last = ipt(i,j)
        enddo
        if     (last .eq. 1) then
          il(j,k) = ii+nbdy
          is(j) = k
        else
          is(j) = k-1
        endif
      enddo
      call xcsync(no_flush)
      return
      end
c
      subroutine indxj(jpt,jf,jl,js)
      use mod_xc  ! HYCOM communication interface
      implicit none
c
      integer, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
     &         jpt
      integer, dimension (1-nbdy:idm+nbdy,ms) ::
     &         jf,jl
      integer, dimension (1-nbdy:idm+nbdy) ::
     &         js
c
c --- input array jpt contains 1 at grid point locations, 0 elsewhere
c --- output is arrays jf, jl, js  where
c --- jf(i,k) gives column index of first point in row i for k-th section
c --- jl(i,k) gives column index of last point
c --- js(i) gives number of sections in row i (maximum: ms)
c
      integer i,j,k,last
c
      do i=1-nbdy,ii+nbdy
        js(i) = 0
        do k=1,ms
          jf(i,k) = 0
          jl(i,k) = 0
        end do
c
        k=1
        last = jpt(i,1-nbdy)
        if     (last .eq. 1) then
          jf(i,k) = 1-nbdy
        endif
        do j=2-nbdy,jj+nbdy
          if      (last .eq. 1 .and. jpt(i,j) .eq. 0) then
            jl(i,k) = j-1
            k = k+1
          elseif (last .eq. 0 .and. jpt(i,j) .eq. 1) then
            if     (k .gt. ms) then
              write(lp,'(a,i5)')  'indxj problem on proc ',mnproc
              write(lp,'(a,2i5)')
     &          ' error in indxj -- ms too small at i,j =',i0+i,j0+j
              call xchalt('(indxj)')
                     stop '(indxj)'
            endif
            jf(i,k) = j
          endif
          last = jpt(i,j)
        enddo
        if     (last .eq. 1) then
          jl(i,k) = jj+nbdy
          js(i) = k
        else
          js(i) = k-1
        endif
      enddo
      call xcsync(no_flush)
      return
      end
c>
c> Revision history
c>
c> Nov  2000 - error stop on single-width inlets and 1-point seas
c> Oct  2008 - warning    on single-width inlets
c> May  2014 - added ipim1,ipip1,ipjm1,ipjp1,ipim1x,ipip1x,ipjm1x,ipjp1x
c> May  2014 - added allip,alliq,alliu,alliv
