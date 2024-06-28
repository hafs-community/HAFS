      subroutine buoflx(bf, mask, hf,wf,sst,sss,n,m, itype)
      implicit none
c
      integer                 :: n,m,itype
      integer, dimension(n,m) :: mask
      real,    dimension(n,m) :: bf, hf,wf,sst,sss
c
c --- calculate buoyancy flux.
c
      real, parameter :: flag = 2.0**100
c
      integer i,j
      real    dsgds,dsgdt,g,thref,spcifh,sf
c
c-----------------------------------------------------------------------------
c
c --- -----------------
c --- equation of state
c --- -----------------
c
      real dsigdt,dsigds
      real s,t
c
c --- coefficients for sigma-0 (based on Brydon & Sun fit)
      real       c1,c2,c3,c4,c5,c6,c7
      parameter (c1=-1.36471E-01, c2= 4.68181E-02, c3= 8.07004E-01,
     &           c4=-7.45353E-03, c5=-2.94418E-03,
     &           c6= 3.43570E-05, c7= 3.48658E-05)
      real       pref
      parameter (pref=0.0)
c
c --- sigma-theta as a function of temp (deg c) and salinity (mil)
c --- (friedrich-levitus 3rd degree polynomial fit)
c
*     sig(t,s)=(c1+c3*s+t*(c2+c5*s+t*(c4+c7*s+c6*t)))
c
c --- d(sig)/dt
      dsigdt(t,s)=(c2+c5*s+2.*t*(c4+c7*s+1.5*c6*t))
c
c --- d(sig)/ds
      dsigds(t,s)=(c3+t*(c5+t*c7))
c-----------------------------------------------------------------------------
c
      spcifh = 3990.0    !specific heat of sea water         (j/kg/deg)
      g      =    9.806  !gravitational acceleration         (m/s**2)
      thref  =    0.001  !reference value of specific volume (m**3/kg)
c
      if     (itype.eq.1) then
c
c ---   temperature (heat) buoyancy flux
c
        do j= 1,m
          do i= 1,n
            if     (mask(i,j).ne.0) then
              dsgdt   = dsigdt(sst(i,j),sss(i,j))
              bf(i,j) = g*thref*(dsgdt*hf(i,j)*thref/spcifh)
            else
              bf(i,j)=flag
            endif
          enddo
        enddo
      elseif (itype.eq.2) then
c
c ---   salinity (E-P) buoyancy flux
c
        do j= 1,m
          do i= 1,n
            if     (mask(i,j).ne.0) then
              dsgds   = dsigds(sst(i,j),sss(i,j))
              sf      = -wf(i,j)*sss(i,j)
              bf(i,j) = g*thref*(dsgds*sf*thref)
            else
              bf(i,j)=flag
            endif
          enddo
        enddo
      else
c
c ---   total buoyancy flux
c
        do j= 1,m
          do i= 1,n
            if     (mask(i,j).ne.0) then
              dsgds   = dsigds(sst(i,j),sss(i,j))
              dsgdt   = dsigdt(sst(i,j),sss(i,j))
              sf      = -wf(i,j)*sss(i,j)
              bf(i,j) = g*thref*(dsgds*sf     *thref) +
     &                  g*thref*(dsgdt*hf(i,j)*thref/spcifh)
            else
              bf(i,j)=flag
            endif
          enddo
        enddo
      endif
      return
      end subroutine buoflx
