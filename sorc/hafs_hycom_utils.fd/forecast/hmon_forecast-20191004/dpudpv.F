#if defined(ROW_LAND)
#define SEA_P .true.
#define SEA_U .true.
#define SEA_V .true.
#elif defined(ROW_ALLSEA)
#define SEA_P allip(j).or.ip(i,j).ne.0
#define SEA_U alliu(j).or.iu(i,j).ne.0
#define SEA_V alliv(j).or.iv(i,j).ne.0
#else
#define SEA_P ip(i,j).ne.0
#define SEA_U iu(i,j).ne.0
#define SEA_V iv(i,j).ne.0
#endif
      subroutine dpudpv(dpu,dpv, p,depthu,depthv,
     &                  margin_p,margin_dpudpv)
      use mod_xc  ! HYCOM communication interface
      implicit none
c
      integer, intent(in)    :: margin_p,margin_dpudpv
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm),
     &         intent(out)   :: dpu,dpv
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm+1),
     &         intent(inout) :: p
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in)    :: depthu,depthv
c
c --- -----------------------------------------------------------------
c --- define layer depth at  u,v  points with halo out to margin_dpudpv
c --- -----------------------------------------------------------------
c
      interface
          subroutine dpudpvj(dpu,dpv, p,depthu,depthv,
     &                       margin_p,margin_dpudpv, j)
          use mod_xc
          integer, intent(in)    :: margin_p,margin_dpudpv,j
          real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm),
     &             intent(out)   :: dpu,dpv
          real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm+1),
     &             intent(in)    :: p
          real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &             intent(in)    :: depthu,depthv
          end subroutine dpudpvj
      end interface
c
      integer j
c
      if     (margin_dpudpv.lt.0    .or.
     &        margin_dpudpv.ge.nbdy     ) then
        if     (mnproc.eq.1) then
          write(lp,'(/ a,i2 /)')
     &      'error: dpudpv called with margin_dpudpv = ',margin_dpudpv
        endif
        call xcstop('dpudpv')
               stop 'dpudpv'
      endif
c
c --- p's halo is valid out to margin_p, is this far enough?
c
      if     (margin_p.lt.margin_dpudpv+1) then
        call xctilr(p(1-nbdy,1-nbdy,2),1,kk,
     &              margin_dpudpv+1,margin_dpudpv+1, halo_ps)
      endif
c
c --- using single row routine fixes SGI OpenMP bug.
!$OMP PARALLEL DO PRIVATE(j)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin_dpudpv,jj+margin_dpudpv
        call dpudpvj(dpu,dpv, p,depthu,depthv,
     &               margin_p,margin_dpudpv, j)
      enddo
      return
      end
      subroutine dpudpvj(dpu,dpv, p,depthu,depthv,
     &                   margin_p,margin_dpudpv, j)
      use mod_xc  ! HYCOM communication interface
      implicit none
c
      integer, intent(in)    :: margin_p,margin_dpudpv,j
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm),
     &         intent(out)   :: dpu,dpv
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm+1),
     &         intent(in)    :: p
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in)    :: depthu,depthv
c
c --- -----------------------------------------------
c --- define layer depth at  u,v  points,  single row
c --- -----------------------------------------------
c
      integer i,k
c
      do k=1,kk
        do i=1-margin_dpudpv,ii+margin_dpudpv
          if (SEA_U) then
            dpu(i,j,k)=max(0.,
     &           min(depthu(i,j),.5*(p(i,j,k+1)+p(i-1,j,k+1)))-
     &           min(depthu(i,j),.5*(p(i,j,k  )+p(i-1,j,k  ))))
          endif !iu
          if (SEA_V) then
            dpv(i,j,k)=max(0.,
     &           min(depthv(i,j),.5*(p(i,j,k+1)+p(i,j-1,k+1)))-
     &           min(depthv(i,j),.5*(p(i,j,k  )+p(i,j-1,k  ))))
          endif !iv
        enddo !i
      enddo !j
      return
      end subroutine dpudpvj
c> May  2014 - use land/sea masks (e.g. ip) to skip land
