      subroutine profile_lat(theta,press,xlat)
      implicit none
c
      real theta,press,xlat
c
      integer        lp
      common/linepr/ lp
      save  /linepr/
c
c --- this routine returns either:
c
c ---    pressure as function of density  and latitude
c ---    density  as function of pressure and latitude
c
c --- set press < 0.0 on input to return pressure
c
c --- typically invoked via either poflat or roflat.
c
      integer ix,kz
      real    p1,p2,pinthi,pintlo,pz,thet,thetlo,thethi,x,xla,z
c
      integer    kdpth,klat
      parameter (kdpth=14,klat=19)  ! kdpth>1, klat>3
c
      real onem,thet1,thet2,dthet,xlat1,xlat2,dlat
      real pdat(kdpth,klat)
c
      data onem/9806./  ! SI units
      data thet1,thet2,dthet/30.0,37.8,0.6/
      data xlat1,xlat2,dlat/-90.,90.,10./
c
c---  depth (m) of isopycnals of potential density 30.0, 30.6, ... , 37.8
c---  at latitudes  90s ... 90n  for GLBa (source: levitus atlas)
c
      data pdat /
     + 0.,0., 0., 0., 0., 0., 1., 23.,158.,257.,575.,1009.,8100.,8100. !90s
     +,0.,0., 0., 0., 0., 0., 1., 23.,158.,257.,575.,1009.,8100.,8100. !80s
     +,0.,0., 0., 0., 0., 0., 1., 23.,158.,257.,575.,1009.,8100.,8100. !70s
     +,0.,0., 0., 0., 0., 0., 1., 23.,158.,257.,575.,1009.,8100.,8100. !60s
     +,0.,0., 0., 0., 0., 0., 1., 23.,158.,257.,575.,1009.,8100.,8100. !50s
     +,0.,0., 0., 0., 0., 0., 1., 23.,158.,257.,575.,1009.,8100.,8100. !40s
     +,0.,0., 0., 0., 0., 0., 1., 23.,158.,257.,575.,1009.,8100.,8100. !30s
     +,0.,0., 0., 0., 0., 0., 1., 40.,159.,233.,478., 913.,8100.,8100. !20s
     +,0.,0., 0., 0., 3.,38.,79., 98.,120.,156.,336.,1033.,8100.,8100. !10s
     +,1.,1., 1., 5.,36.,51.,62., 71., 86.,121.,407., 873.,8100.,8100. ! 0
     +,3.,5.,10.,24.,45.,60.,72., 86.,104.,137.,283., 929.,8100.,8100. !10n
     +,0.,0., 2.,20.,34.,47.,69.,112.,154.,224.,446., 794.,8100.,8100. !20n
     +,0.,0., 1., 3., 6.,15.,24., 42., 77.,193.,557., 761.,8100.,8100. !30n
     +,0.,0., 0., 0., 1., 6., 9., 19., 38., 72.,227., 617.,8100.,8100. !40n
     +,0.,0., 0., 0., 1., 2., 3.,  5.,  8., 28., 78., 353.,8100.,8100. !50n
     +,0.,0., 0., 0., 0., 0., 0.,  0.,  1.,  3., 12., 132.,1367.,8100. !60n
     +,0.,0., 0., 0., 0., 0., 0.,  0.,  1.,  2.,  9.,  32., 239.,8100. !70n
     +,0.,0., 0., 0., 0., 0., 0.,  0.,  1.,  2.,  9.,  32., 239.,8100. !80n
     +,0.,0., 0., 0., 0., 0., 0.,  0.,  1.,  2.,  9.,  32., 239.,8100. !90n
     +/
c
c---  quasi-hermite interpolation function (0 < xx < 1)
c
      real parabl,xx,a,b,c
      parabl(xx,a,b,c)=b+.5*xx*(c-a+xx*(a+c-b-b))
c
      xla=(xlat-xlat1)/dlat+1.
      ix=max(2,min(klat-2,int(xla)))
      x=max(0.,min(1.,xla-real(ix)))
c
      if     (press.lt.0.0) then
c
c ----  pressure from density.
c
        thet=(theta-thet1)/dthet+1.
        if     (thet.lt.1.0) then
        press=0.0
        else  ! normal case
        kz=max(1,min(kdpth-1,int(thet)))
        z=max(0.,min(1.,thet-real(kz)))
c
c ---   horizontal/vertical interpolation: quasi-hermite/linear
c
        p1=parabl(   x,pdat(kz  ,ix-1),pdat(kz  ,ix  ),pdat(kz  ,ix+1))
        p2=parabl(1.-x,pdat(kz  ,ix+2),pdat(kz  ,ix+1),pdat(kz  ,ix  ))
        pintlo=p1*(1.-x)+p2*x
        p1=parabl(   x,pdat(kz+1,ix-1),pdat(kz+1,ix  ),pdat(kz+1,ix+1))
        p2=parabl(1.-x,pdat(kz+1,ix+2),pdat(kz+1,ix+1),pdat(kz+1,ix  ))
        pinthi=p1*(1.-x)+p2*x
        press =(pintlo*(1.-z)+pinthi*z)*onem
        endif
cdiag   write (lp,'('' poflat'',2f7.2,2i5,2f7.2,f7.1)')
cdiag&    theta,xlat,ix,kz,x,z,press/onem
      else
c
c ----  density from pressure.
c
        pz=press/onem
        kz=1
        p1=parabl(   x,pdat(kz,ix-1),pdat(kz,ix  ),pdat(kz,ix+1))
        p2=parabl(1.-x,pdat(kz,ix+2),pdat(kz,ix+1),pdat(kz,ix  ))
        pinthi=p1*(1.-x)+p2*x
        if     (pinthi.ge.pz) then
        theta=thet1
        else  ! normal range
        do kz= 2,kdpth
          pintlo=pinthi
          p1=parabl(   x,pdat(kz,ix-1),pdat(kz,ix  ),pdat(kz,ix+1))
          p2=parabl(1.-x,pdat(kz,ix+2),pdat(kz,ix+1),pdat(kz,ix  ))
          pinthi=p1*(1.-x)+p2*x
          if     (pinthi.ge.pz) then
            exit
          elseif (kz.eq.kdpth) then
            exit
          endif
        enddo
        z=max((pinthi-pz)/(pinthi-pintlo),0.0)
        theta=thet1+(kz-z-1.0)*dthet
        endif
cdiag   write (lp,'('' roflat'',2f7.2,2i5,2f7.2,f7.1)')
cdiag&    theta,xlat,ix,kz,x,z,pz
      endif
      return
      end

      real function poflat(theta,xlat)
      implicit none
c
      real theta,xlat
c
c --- returns pressure as function of density and latitude
c
      real press
      press = -1.0
      call profile_lat(theta,press,xlat)
      poflat = press
      return
      end

      real function roflat(press,xlat)
      implicit none
c
      real press,xlat
c
c --- returns density as function of pressure and latitude
c
      real theta
c
      call profile_lat(theta,press,xlat)
      roflat = theta
      return
      end

c
c> Revision history
c>
c> May  2000 - conversion to SI units
c> Aug  2001 - added roflat and profile_lat to poflat.
