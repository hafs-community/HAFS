      subroutine bigrid(depth)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
c
      dimension depth(0:ii,0:jj)
c
c --- set loop bounds for irregular basin in c-grid configuration
c --- q,u,v,p are vorticity, u-velocity, v-velocity, and mass points, resp.
c --- 'depth' = basin depth array, zero values indicate land
c
      integer, allocatable, dimension (:,:) ::  ip0
c
      character fmt*12,char2*2
      data fmt/'(i4,1x,75i1)'/
c
      allocate( ip0(0:ii,0:jj) )
c
      write (lp,'(a,6i6)')
     &   'bigrid called with ii,jj,ii1,jj1 =',
     &                       ii,jj,ii1,jj1
c
c --- mass points are defined where water depth is greater than zero
      do i=0,ii
        do j=0,jj
          if (depth(i,j).gt.0.) then
            ip0(i,j)=1
          else
            ip0(i,j)=0
          endif
        enddo
      enddo
c
      do i=1,ii
        do j=1,jj
          ip(i,j)=ip0(i,j)
        enddo
      enddo
c      
c --- write out  -ip-  array
c --- data are written in strips 75 points wide
ccc      jsec=(jj1-1)/75
ccc      do 9 jfrst=0,75*jsec,75
ccc      jlast=min(jj1,jfrst+75)
ccc      write (char2,'(i2)') jlast-jfrst
ccc      fmt(8:9)=char2
ccc      write (lp,'(''ip array, cols'',i5,'' --'',i5)') jfrst+1,jlast
ccc 9    write (lp,fmt) (i,(10*ip(i,j),j=jfrst+1,jlast),i=1,ii1)
c
c --- u,v points are located halfway between any 2 adjoining mass points
      do j=1,jj
        do i=1,ii
          if (ip0(i-1,j).gt.0.and.ip0(i,j).gt.0) then
            iu(i,j)=1
          else
            iu(i,j)=0
          endif
          if (ip0(i,j-1).gt.0.and.ip0(i,j).gt.0) then
            iv(i,j)=1
          else
            iv(i,j)=0
          endif
        enddo
      enddo
c
      do j=1,jj
        do i=1,ii
          iq(i,j)=0
c
c ---     'interior' q points require water on all 4 sides.
          if (min0(ip0(i,j),  ip0(i-1,j),
     &             ip0(i,j-1),ip0(i-1,j-1)).gt.0) iq(i,j)=1
c
c ---     'promontory' q points require water on 3
c ---     (or at least 2 diametrically opposed) sides
          if ((ip0(i  ,j).gt.0.and.ip0(i-1,j-1).gt.0).or.
     &        (ip0(i-1,j).gt.0.and.ip0(i  ,j-1).gt.0)    ) iq(i,j)=1
        enddo
      enddo
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
      deallocate( ip0 )
c
      return
      end
