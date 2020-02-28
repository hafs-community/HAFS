#if defined(ROW_LAND)
#define SEA_Q .true.
#elif defined(ROW_ALLSEA)
#define SEA_Q alliq(j).or.iq(i,j).ne.0
#else
#define SEA_Q iq(i,j).ne.0
#endif
      subroutine dpthuv
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
c --- define water depth (bottom pressure) at  u,v  points and barotp.pot.vort.
c
      integer i,j,l,margin
c
c --- depth at u,v points
      real uvdep,a,b
      uvdep(a,b)=min(a,b)
c
c --- initialize ports.
c
#if ! defined(RELO)
      if     (lbflag.eq.1) then
        call latbdp( 0)
      elseif (lbflag.eq.3) then
        call latbdf( 0,0)
      endif
#endif
      if     (lbflag.eq.2) then
        call latbdt( 0,0)
      elseif (lbflag.eq.4) then
        call latbdtf(0,0)
      else
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
        do j= 1-nbdy,jj+nbdy
          do i= 1-nbdy,ii+nbdy
            iuopn(i,j) = 0
            ivopn(i,j) = 0
          enddo
        enddo
      endif
c
      call xctilr(pbot, 1,1, nbdy,nbdy, halo_ps)
      call xctilr(corio,1,1, nbdy,nbdy, halo_qs)
c
c --- rhs: pbot+,corio+
c --- lhs: depthu, depthv, pvtrop+
c
      margin = 1
c
      do j=1-margin,jj+margin
        do l=1,isu(j) !ok
          i=ifu(j,l)-1
          if     (i.ge.1-margin) then
            if     (iuopn(i,j).ne.0) then
              depthu(i,j)=pbot(i  ,j)
*             write(lp,*) 'depthu - i,j,d = ',
*    &                    i+i0,j+j0,depthu(i,j)*qonem
            endif
          endif
          i=ilu(j,l)+1
          if     (i.le.ii+margin) then
            if     (iuopn(i,j).ne.0) then
              depthu(i,j)=pbot(i-1,j)
*             write(lp,*) 'depthu - i,j,d = ',
*    &                    i+i0,j+j0,depthu(i,j)*qonem
            endif
          endif
          do i=max(1-margin,ifu(j,l)),min(ii+margin,ilu(j,l))
            depthu(i,j)=uvdep(pbot(i,j),pbot(i-1,j))
            pvtrop(i,j  )=corio(i,j  )*2./(pbot(i,j)+pbot(i-1,j))
            pvtrop(i,j+1)=corio(i,j+1)*2./(pbot(i,j)+pbot(i-1,j))
          enddo
        enddo
      enddo
      call xcsync(flush_lp)
c
      do i=1-margin,ii+margin
        do l=1,jsv(i) !ok
          j=jfv(i,l)-1
          if     (j.ge.1-margin) then
            if     (ivopn(i,j).ne.0) then
              depthv(i,j)=pbot(i,j  )
*             write(lp,*) 'depthv - i,j,d = ',
*    &                    i+i0,j+j0,depthv(i,j)*qonem
            endif
          endif
          j=jlv(i,l)+1
          if     (j.le.jj+margin) then
            if     (ivopn(i,j).ne.0) then
              depthv(i,j)=pbot(i,j-1)
*             write(lp,*) 'depthv - i,j,d = ',
*    &                    i+i0,j+j0,depthv(i,j)*qonem
            endif
          endif
          do j=max(1-margin,jfv(i,l)),min(jj+margin,jlv(i,l))
            depthv(i,j)=uvdep(pbot(i,j),pbot(i,j-1))
            pvtrop(i  ,j)=corio(i  ,j)*2./(pbot(i,j)+pbot(i,j-1))
            pvtrop(i+1,j)=corio(i+1,j)*2./(pbot(i,j)+pbot(i,j-1))
          enddo
        enddo
      enddo
      call xcsync(flush_lp)
c
!$OMP PARALLEL DO PRIVATE(j,l,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        do i=1,ii
          if (SEA_Q) then
            pvtrop(i,j)=corio(i,j)*4./(pbot(i,j  )+pbot(i-1,j  )
     &                                +pbot(i,j-1)+pbot(i-1,j-1))
          endif !iq
        enddo !i
      enddo !j
c
      call xctilr(depthu,1,1, nbdy,nbdy, halo_us)
      call xctilr(depthv,1,1, nbdy,nbdy, halo_vs)
      call xctilr(pvtrop,1,1, nbdy,nbdy, halo_qs)
      return
      end subroutine dpthuv
c> May  2014 -- use land/sea masks (e.g. iq) to skip land
c> May  2014 -- removed lbflag==6 for latbdtc
