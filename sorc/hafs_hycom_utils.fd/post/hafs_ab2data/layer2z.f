      subroutine layer2w(u,v,p,depthu,depthv,scpx,scpy,
     &                   ip,iu,iv, kpu,kpv, ii,jj,kk, uflux,vflux,
     &                   ztop,zbot,flag, wz)
      implicit none
c
      integer ii,jj,kk,
     &        ip(ii,jj),iu(ii,jj),iv(ii,jj),kpu(ii,jj),kpv(ii,jj)
      real    u(ii,jj,2*kk),v(ii,jj,2*kk),p(ii,jj,kk+1),
     &        depthu(ii,jj),depthv(ii,jj),
     &        scpx(ii,jj),scpy(ii,jj),uflux(ii,jj),vflux(ii,jj),
     &        ztop,zbot,flag, wz(ii,jj)
c
c**********
c*
c  1) return vertical velocity difference between ztop and zbot.
c
c  2) input arguments:
c       u      - u-velocity in layer space
c       v      - v-velocity in layer space
c       p      - layer interface depths (non-negative m)
c                  p(:,:,   1) is the surface
c                  p(:,:,kk+1) is the bathymetry (i.e. depthp)
c       depthu - depth on u-grid
c       depthv - depth on v-grid
c       scpx   - x grid spacing
c       scpy   - y grid spacing
c       ip     - p-grid mask
c       iu     - u-grid mask
c       iv     - v-grid mask
c       kpu    - u-grid layer containing ztop
c       kpv    - v-grid layer containing ztop
c       ii     - 1st array dimension
c       jj     - 2nd array dimension
c       kk     - 3rd array dimension (number of layers)
c       ztop   - top    of z-level cell
c       zbot   - bottom of z-level cell
c       flag   - data void (land) marker
c
c  3) output arguments:
c       kpu    - u-grid layer containing zbot
c       kpv    - v-grid layer containing zbot
c       uflux  - workspace
c       vflux  - workspace
c       wz     - vertical velocity difference 
c
c  4) except at data voids, must have:
c           p(:,:,   1) == zero (surface)
c           p(:,:, l+1) >= p(:,:,l)
c           p(:,:,kk+1) == bathymetry
c           0 <= ztop <= zbot
c     note that zbot > p(i,j,kk+1) implies that wz(i,j)=flag,
c      since the z-level is then below the bathymetry.
c
c  5) Alan J. Wallcraft, Naval Research Laboratory, February 2002.
c*
c**********
c
      integer i,j,k
      real    puk,pukp,pvk,pvkp,uflx,vflx
c
*     write(6,'(a,2g15.5)')
*    &  'layer2w - ztop,zbot =',ztop,zbot
*     call flush(6)
c
c     u-transport between ztop and zbot.
c
      do j= 1,jj
        do i= 2,ii
          if     (iu(i,j).ne.1 .or. ztop.ge.depthu(i,j)) then
            uflux(i,j) = 0.0
            kpu  (i,j) = kk
            cycle
          endif
          uflx = 0.0
          k    = kpu(i,j)-1
          pukp = min(depthu(i,j),.5*(p(i,j,k+1)+p(i-1,j,k+1)))
          do k= kpu(i,j),kk
            puk  = pukp
            pukp = min(depthu(i,j),.5*(p(i,j,k+1)+p(i-1,j,k+1)))
            if     (k.eq.kpu(i,j)) then
              if     (ztop.lt.puk .or. ztop.gt.pukp) then
                write(6,'(a,i5,a,i5,a,i3, a,f8.2, a,2f8.2,a)')
     &            'error - puk(',i,',',j,') = ',k,
     &            '  but ztop =',ztop,
     &            ' not in that layer (',puk,pukp,')'
                stop 9
              endif
            endif
            uflx = uflx + u(i,j,2*k)*(min(zbot,pukp) - max(ztop,puk))
*           if     (i.eq.ii/2 .and. j.eq.jj/2) then
*             write(6,'(a,i3,3g15.5)')
*    &          'layer2w - k,puk,pukp,uflx =',k,puk,pukp,uflx
*             call flush(6)
*           endif
            if     (zbot.ge.puk .and. zbot.le.pukp) then
              kpu(i,j) = k
              exit
            endif
          enddo
          uflux(i,j) = uflx
        enddo
      enddo
c
c     v-transport between ztop and zbot.
c
      do j= 2,jj
        do i= 1,ii
          if     (iv(i,j).ne.1 .or. ztop.ge.depthv(i,j)) then
            vflux(i,j) = 0.0
            kpv  (i,j) = kk
            cycle
          endif
          vflx = 0.0
          k    = kpv(i,j)-1
          pvkp = min(depthv(i,j),.5*(p(i,j,k+1)+p(i,j-1,k+1)))
          do k= kpv(i,j),kk
            pvk  = pvkp
            pvkp = min(depthv(i,j),.5*(p(i,j,k+1)+p(i,j-1,k+1)))
            if     (k.eq.kpv(i,j)) then
              if     (ztop.lt.pvk .or. ztop.gt.pvkp) then
                write(6,'(a,i5,a,i5,a,i3, a,f8.2, a,2f8.2,a)')
     &            'error - pvk(',i,',',j,') = ',k,
     &            '  but ztop =',ztop,
     &            ' not in that layer (',pvk,pvkp,')'
                stop 9
              endif
            endif
            vflx = vflx + v(i,j,2*k)*(min(zbot,pvkp) - max(ztop,pvk))
*           if     (i.eq.ii/2 .and. j.eq.jj/2) then
*             write(6,'(a,i3,3g15.5)')
*    &          'layer2w - k,pvk,pvkp,vflx =',k,pvk,pvkp,vflx
*             call flush(6)
*           endif
            if     (zbot.ge.pvk .and. zbot.le.pvkp) then
              kpv(i,j) = k
              exit
            endif
          enddo
          vflux(i,j) = vflx
        enddo
      enddo
c
c     vertical velocity.
c
      do j= 2,jj-1
        wz(1, j) = flag
        do i= 2,ii-1
          if     (ip(i,j).eq.1) then
            if     (ztop.lt.p(i,j,kk+1)) then
              wz(i,j) = (uflux(i+1,j)-uflux(i,j))/scpx(i,j) +
     &                  (vflux(i,j+1)-vflux(i,j))/scpy(i,j)
            else
              wz(i,j) = flag
            endif
          else
            wz(i,j) = flag
          endif
        enddo
        wz(ii,j) = flag
      enddo
      do i= 1,ii
        wz(i, 1) = flag
        wz(i,jj) = flag
      enddo
      return
      end
      subroutine layer2z(a,p,az,z,flag,ii,jj,kk,kz,itype)
      implicit none
c
      integer ii,jj,kk,kz,itype
      real    a(ii,jj,kk),p(ii,jj,kk+1),az(ii,jj,kz),z(kz),flag
c
c**********
c*
c  1) interpolate a layered field to fixed z depths
c
c  2) input arguments:
c       a     - scalar field in layer space
c       p     - layer interface depths (non-negative m)
c                 p(:,:,   1) is the surface
c                 p(:,:,kk+1) is the bathymetry
c       z     - target z-level  depths (non-negative m)
c       flag  - data void (land) marker
c       ii    - 1st dimension of a,p,az
c       jj    - 2nd dimension of a,p,az
c       kk    - 3rd dimension of a  (number of layers)
c       kz    - 3rd dimension of az (number of levels)
c       itype - interpolation type
c                 =0; sample the layer spaning each depth
c                 =1; linear interpolation between layer centers
c
c  3) output arguments:
c       az    - scalar field in z-space
c
c  4) except at data voids, must have:
c           p(:,:,   1) == zero (surface)
c           p(:,:, l+1) >= p(:,:,l)
c           p(:,:,kk+1) == bathymetry
c           0 <= z(k) <= z(k+1)
c     note that z(k) > p(i,j,kk+1) implies that az(i,j,k)=flag,
c      since the z-level is then below the bathymetry.
c
c  5) Alan J. Wallcraft, Naval Research Laboratory, February 2002.
c*
c**********
c
      integer i,j,k,l,lf
      real    s,zk,z0,zm,zp
c
      do j= 1,jj
        do i= 1,ii
          if     (a(i,j,1).eq.flag) then
            do k= 1,kz
              az(i,j,k) = flag  ! land
            enddo
          else
            lf=1
            do k= 1,kz
              zk=z(k)
              do l= lf,kk
                if     (p(i,j,l).le.zk .and. p(i,j,l+1).ge.zk) then
c
c                 z(k) is in layer l.
c
                  if     (itype.eq.0) then
c
c                   sample the layer
c
                    az(i,j,k) = a(i,j,l)
                  else
c
c                   linear interpolation between layer centers
c
                    z0 = 0.5*(p(i,j,l)+p(i,j,l+1))
                    if     (zk.le.z0) then
c
c                     z(k) is in the upper half of the layer
c
                      if     (l.eq.1) then
                        az(i,j,k) = a(i,j,1)
                      else
                        zm = 0.5*(p(i,j,l-1)+p(i,j,l))
                        s  = (z0 - zk)/(z0 - zm)
                        az(i,j,k) = s*a(i,j,l-1) + (1.0-s)*a(i,j,l)
                      endif
                    else
c
c                     z(k) is in the lower half of the layer
c
                      if     (p(i,j,l+1).eq.p(i,j,kk+1)) then
                        az(i,j,k) = a(i,j,kk)
                      else
                        zp = 0.5*(p(i,j,l+1)+p(i,j,l+2))
                        s  = (zk - z0)/(zp - z0)
                        az(i,j,k) = s*a(i,j,l+1) + (1.0-s)*a(i,j,l)
                      endif
                    endif
                  endif
                  lf = l
                  exit
                elseif (l.eq.kk) then
                  az(i,j,k) = flag  ! below the bottom
                  lf = l
                  exit
                endif
              enddo !l
            enddo !k
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine infac2z(a,p,az,z,flag,ii,jj,kk,kz,itype)
      implicit none
c
      integer ii,jj,kk,kz,itype
      real    a(ii,jj,kk+1),p(ii,jj,kk+1),az(ii,jj,kz),z(kz),flag
c
c**********
c*
c  1) interpolate a field at layer interfaces to fixed z depths
c
c  2) input arguments:
c       a     - scalar field in layer interface space
c       p     - layer interface depths (non-negative m)
c                 p(:,:,   1) is the surface
c                 p(:,:,kk+1) is the bathymetry
c       z     - target z-level  depths (non-negative m)
c       flag  - data void (land) marker
c       ii    - 1st dimension of a,p,az
c       jj    - 2nd dimension of a,p,az
c       kk    - 3rd dimension of a  (number of layers)
c       kz    - 3rd dimension of az (number of levels)
c       itype - interpolation type
c                 =1; linear interpolation between interfaces
c
c  3) output arguments:
c       az    - scalar field in z-space
c
c  4) except at data voids, must have:
c           p(:,:,   1) == zero (surface)
c           p(:,:, l+1) >= p(:,:,l)
c           p(:,:,kk+1) == bathymetry
c           0 <= z(k) <= z(k+1)
c     note that z(k) > p(i,j,kk+1) implies that az(i,j,k)=flag,
c      since the z-level is then below the bathymetry.
c
c  5) Alan J. Wallcraft, Naval Research Laboratory, February 2002.
c*
c**********
c
      integer i,j,k,l,lf
      real    s,zk,z0,zm,zp
c
      do j= 1,jj
        do i= 1,ii
          if     (a(i,j,1).eq.flag) then
            do k= 1,kz
              az(i,j,k) = flag  ! land
            enddo
          else
            lf=1
            do k= 1,kz
              zk=z(k)
              do l= lf,kk
                if     (p(i,j,l).le.zk .and. p(i,j,l+1).ge.zk) then
c
c                 z(k) is in layer l.
c
*                 if     (itype.eq.1) then
c
c                   linear interpolation between interfaces
c
                    z0 = p(i,j,l+1)
                    zm = p(i,j,l)
                    s  = (z0 - zk)/(z0 - zm)
                    az(i,j,k) = s*a(i,j,l) + (1.0-s)*a(i,j,l+1)
*                 endif
                  lf = l
                  exit
                elseif (l.eq.kk) then
                  az(i,j,k) = flag  ! below the bottom
                  lf = l
                  exit
                endif
              enddo !l
            enddo !k
          endif
        enddo !i
      enddo !j
      return
      end
