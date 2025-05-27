!-----------------------------------------------------------------------+
! This package of subroutines are used for HAFS grid calculation.
! authors and history:
!      -- 202102, created by Yonghui Weng
!-----------------------------------------------------------------------+
   subroutine rtll(rot_lon,rot_lat,geo_lon,geo_lat,cen_lon,cen_lat)

!-- from https://github.com/NOAA-EMC/fv3atm/blob/cf0a73180b2d9ac55ebfce4785a7270d205423db/io/module_wrt_grid_comp.F90#L3545
!-- called in fv3atm
!   else if ( trim(output_grid) == 'rotated_latlon' ) then
!           do j=lbound(lonPtr,2),ubound(lonPtr,2)
!           do i=lbound(lonPtr,1),ubound(lonPtr,1)
!             rot_lon = lon1 + (lon2-lon1)/(imo-1) * (i-1)
!             rot_lat = lat1 + (lat2-lat1)/(jmo-1) * (j-1)
!             call rtll(rot_lon, rot_lat, geo_lon, geo_lat, dble(cen_lon), dble(cen_lat))
!             if (geo_lon < 0.0) geo_lon = geo_lon + 360.0
!             lonPtr(i,j) = geo_lon
!             latPtr(i,j) = geo_lat
!           enddo
!           Enddo
!
!--------------
  real, intent(in)  :: rot_lon, rot_lat
  real, intent(out) :: geo_lon, geo_lat
  real, intent(in)  :: cen_lat, cen_lon
!
  real, parameter :: pi=3.14159265358979323846
  real, parameter :: dtr=pi/180.0
!
  real :: tph0, ctph0, stph0, tlm, tph, stph, ctph, ctlm, stlm, aph, cph
  real :: xx, yy
!--------------
!
!--- Convert all angles to radians
  tph0=cen_lat*dtr
  ctph0=cos(tph0)
  stph0=sin(tph0)
  tlm=rot_lon*dtr
  tph=rot_lat*dtr
!
  stph=sin(tph)
  ctph=cos(tph)
  ctlm=cos(tlm)
  stlm=sin(tlm)
!
  xx=stph0*ctph*ctlm+ctph0*stph
  xx=max(xx,-1.0)
  xx=min(xx, 1.0)
  aph=asin(xx)
  cph=cos(aph)
!
  xx=(ctph0*ctph*ctlm-stph0*stph)/cph
  xx=max(xx,-1.0)
  xx=min(xx, 1.0)
  xx=acos(xx)/dtr
  yy=ctph*stlm/cph
  xx=sign(xx,yy)
  geo_lon=cen_lon+xx

  geo_lat=aph/dtr
!
  if (geo_lon < 0.0)  geo_lon=geo_lon+360.0
!
  return
  end subroutine rtll

!-----------------------------------------------------------------------+
!--- from hwrf_wps.fd/geogrid/src/module_map_utils.f90
!--- modified
  subroutine ijll_rotlatlon(i, j, phi, lambda, ixdim, jydim, lat1, lon1, stagger, lat,lon)

  implicit none

  ! Arguments
  real, intent(in)    :: i, j
  real, intent(in)    :: phi      ! For Rotated Lat/Lon -- domain half-extent in degrees latitude
  real, intent(in)    :: lambda   ! For Rotated Lat/Lon -- domain half-extend in degrees longitude
  integer, intent(in) :: ixdim    ! For Rotated Lat/Lon -- number of mass points in an odd row
  integer, intent(in) :: jydim    ! For Rotated Lat/Lon -- number of rows
  real, intent(in)    :: lat1     ! SW latitude (1,1) in degrees (-90->90N)
  real, intent(in)    :: lon1     ! SW longitude (1,1) in degrees (-180->180E)
  character (len=*), intent(in) :: stagger  ! For Rotated Lat/Lon -- mass or velocity grid 'VV'/'T'
  real, intent(out)   :: lat, lon

  ! Local variables
  integer :: ih,jh
  real :: jj
  integer :: midcol,midrow,ncol,iadd1,iadd2,imt,jh2,knrow,krem,kv,nrow
  real :: dphd,dlmd !Grid increments, degrees
  real :: arg1,arg2,d2r,fctr,glatr,glatd,glond,pi, &
          r2d,tlatd,tlond,tlatr,tlonr,tlm0,tph0
  real :: col

  jj = j
  if ( (j - int(j)) .gt. 0.999) then
     jj = j + 0.0002
  endif

  jh = int(jj)

  dphd = phi/real((jydim-1)/2)
  dlmd = lambda/real(ixdim-1)

  pi = acos(-1.0)
  d2r = pi/180.
  r2d = 1./d2r
  tph0 = lat1*d2r
  tlm0 = -lon1*d2r

  midrow = int((jydim+1)/2)
  midcol = ixdim

  col = 2*i-1+abs(mod(jh+1,2))
  tlatd = (jj-midrow)*dphd
  tlond = (col-midcol)*dlmd

  if (trim(stagger) == 'VV') then
     if (mod(jh,2) .eq. 0) then
        tlond = tlond - dlmd
     else
        tlond = tlond + dlmd
     end if
  end if

  tlatr = tlatd*d2r
  tlonr = tlond*d2r
  arg1 = sin(tlatr)*cos(tph0)+cos(tlatr)*sin(tph0)*cos(tlonr)
  glatr = asin(arg1)

  glatd = glatr*r2d

  arg2 = cos(tlatr)*cos(tlonr)/(cos(glatr)*cos(tph0))-tan(glatr)*tan(tph0)
  if (abs(arg2) > 1.) arg2 = abs(arg2)/arg2
  fctr = 1.
  if (tlond > 0.) fctr = -1.

  glond = tlm0*r2d+fctr*acos(arg2)*r2d

  lat = glatd
  lon = -glond
  if (lon < 0.) lon = lon + 360.

  return
  end subroutine ijll_rotlatlon

!-----------------------------------------------------------------------+
!--- from vortex_init/hwrf_set_ijstart/swcorner_dynamic.F
!--- modified
  subroutine EARTH_LATLON ( HLAT,HLON,VLAT,VLON,     & !Earth lat,lon at H and V points
                          DLMD1,DPHD1,WBD1,SBD1,   & !input res,west & south boundaries,
                          CENTRAL_LAT,CENTRAL_LON, & ! central lat,lon, all in degrees
                          IDS,IDE,JDS,JDE,KDS,KDE, &
                          IMS,IME,JMS,JME,KMS,KME, &
                          ITS,ITE,JTS,JTE,KTS,KTE  )
!
 IMPLICIT NONE
 INTEGER,    INTENT(IN   )                            :: IDS,IDE,JDS,JDE,KDS,KDE
 INTEGER,    INTENT(IN   )                            :: IMS,IME,JMS,JME,KMS,KME
 INTEGER,    INTENT(IN   )                            :: ITS,ITE,JTS,JTE,KTS,KTE
 REAL,       INTENT(IN   )                            :: DLMD1,DPHD1,WBD1,SBD1
 REAL,       INTENT(IN   )                            :: CENTRAL_LAT,CENTRAL_LON
 REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(OUT)        :: HLAT,HLON,VLAT,VLON

! local


 INTEGER,PARAMETER                           :: KNUM=SELECTED_REAL_KIND(13)
 INTEGER                                     :: I,J
 REAL(KIND=KNUM)                             :: WB,SB,DLM,DPH,TPH0,STPH0,CTPH0
 REAL(KIND=KNUM)                             :: TDLM,TDPH,TLMH,TLMV,TLMH0,TLMV0,TPHH,TPHV,DTR
 REAL(KIND=KNUM)                             :: STPH,CTPH,STPV,CTPV,PI_2
 REAL(KIND=KNUM)                             :: SPHH,CLMH,FACTH,SPHV,CLMV,FACTV
 REAL(KIND=KNUM), DIMENSION(IMS:IME,JMS:JME) :: GLATH,GLONH,GLATV,GLONV
 REAL(KIND=KNUM) :: DLMD8,DPHD8,WBD8,SBD8,CLAT8,CLON8
 REAL(KIND=KNUM) :: CPHH, CPHV
!-------------------------------------------------------------------------
 DLMD8=DLMD1
 DPHD8=DPHD1
 WBD8=WBD1
 SBD8=SBD1
 CLAT8=CENTRAL_LAT
 CLON8=CENTRAL_LON
!
      PI_2 = ACOS(0.)
      DTR  = PI_2/90.
      WB   = WBD8 * DTR                 ! WB:   western boundary in radians
      SB   = SBD8 * DTR                 ! SB:   southern boundary in radians
      DLM  = DLMD8 * DTR                ! DLM:  dlamda in radians
      DPH  = DPHD8 * DTR                ! DPH:  dphi   in radians
      TDLM = DLM + DLM                  ! TDLM: 2.0*dlamda
      TDPH = DPH + DPH                  ! TDPH: 2.0*DPH

!     For earth lat lon only

      TPH0  = CLAT8*DTR                ! TPH0: central lat in radians
      STPH0 = SIN(TPH0)
      CTPH0 = COS(TPH0)

                                                !    .H
      DO J = JTS,MIN(JTE,JDE-1)                 ! H./    This loop takes care of zig-zag
!                                               !   \.H  starting points along j
         TLMH0 = WB - TDLM + MOD(J+1,2) * DLM   !  ./    TLMH (rotated lats at H points)
         TLMV0 = WB - TDLM + MOD(J,2) * DLM     !  H     (//ly for V points)
         TPHH = SB + (J-1)*DPH                  !   TPHH (rotated lons at H points) are simple trans.
         TPHV = TPHH                            !   TPHV (rotated lons at V points) are simple trans.
         STPH = SIN(TPHH)
         CTPH = COS(TPHH)
         STPV = SIN(TPHV)
         CTPV = COS(TPHV)
                                                              !   .H
         DO I = ITS,MIN(ITE,IDE-1)                            !  /
           TLMH = TLMH0 + I*TDLM                              !  \.H   .U   .H
!                                                             !H./ ----><----
           SPHH = CTPH0 * STPH + STPH0 * CTPH * COS(TLMH)     !     DLM + DLM
           CPHH = sqrt(1-SPHH**2)
           GLATH(I,J)=ASIN(SPHH)                              ! GLATH: Earth Lat in radians
           !CLMH = CTPH*COS(TLMH)/(COS(GLATH(I,J))*CTPH0) &
           !     - TAN(GLATH(I,J))*TAN(TPH0)
           CLMH = (CTPH*COS(TLMH)-SPHH*STPH0) / (CPHH*CTPH0)
           IF(CLMH .GT. 1.) CLMH = 1.0
           IF(CLMH .LT. -1.) CLMH = -1.0
           FACTH = 1.
           IF(TLMH .GT. 0.) FACTH = -1.
           GLONH(I,J) = -CLON8*DTR + FACTH*ACOS(CLMH)

         ENDDO

         DO I = ITS,MIN(ITE,IDE-1)
           TLMV = TLMV0 + I*TDLM
           SPHV = CTPH0 * STPV + STPH0 * CTPV * COS(TLMV)
           CPHV = sqrt(1-SPHV**2)
           GLATV(I,J) = ASIN(SPHV)
           !CLMV = CTPV*COS(TLMV)/(COS(GLATV(I,J))*CTPH0) &
           !     - TAN(GLATV(I,J))*TAN(TPH0)
           CLMV = (CTPV*COS(TLMV)-SPHV*STPH0) / (CPHV*CTPH0)
           IF(CLMV .GT. 1.) CLMV = 1.
           IF(CLMV .LT. -1.) CLMV = -1.
           FACTV = 1.
           IF(TLMV .GT. 0.) FACTV = -1.
           GLONV(I,J) = -CLON8*DTR + FACTV*ACOS(CLMV)

         ENDDO

      ENDDO

!     Conversion to degrees (may not be required, eventually)

      DO J = JTS, MIN(JTE,JDE-1)
       DO I = ITS, MIN(ITE,IDE-1)
          HLAT(I,J) = GLATH(I,J) / DTR
          HLON(I,J)= -GLONH(I,J)/DTR
          IF(HLON(I,J) .LT. 0.) HLON(I,J) = HLON(I,J) + 360.
!
          VLAT(I,J) = GLATV(I,J) / DTR
          VLON(I,J) = -GLONV(I,J) / DTR
          IF(VLON(I,J) .LT. 0.) VLON(I,J) = VLON(I,J) + 360.

       ENDDO
      ENDDO

END SUBROUTINE EARTH_LATLON

!-----------------------------------------------------------------------+
 subroutine general_create_llxy_transform(region_lat,region_lon,nlat,nlon,gt)
!
! adopted from hafs sorc/hafs_gsi.fd/src/gsi/general_tll2xy_mod.f90
!
! abstract:  copy routines from gridmod.f90 to make a general purpose module which allows
!     conversion of earth lat lons to grid coordinates for any non-staggered rectangular
!     orthogonal grid with known earth lat and lon of each grid point.
!     set up constants to allow conversion between earth lat lon and analysis grid units.
!     There is no need to specify details of the analysis grid projection.  All that is required
!     is the earth latitude and longitude in radians of each analysis grid point.
!
  use constants
  use var_type

  implicit none

  integer,intent(in   ) :: nlat,nlon
  real   ,intent(in   ) :: region_lat(nlat,nlon),region_lon(nlat,nlon)
  type(llxy_cons),intent(inout) :: gt

  real,parameter:: rbig =1.0e30
  real    :: xbar_min,xbar_max,ybar_min,ybar_max
  real    :: clon,slon,r_of_lat,xbar,ybar
  integer :: i,j,istart0,iend,iinc,itemp,ilast,jlast
  real    :: glats(nlon,nlat),glons(nlon,nlat)
  real,allocatable:: clata(:,:),slata(:,:),clona(:,:),slona(:,:)
  real    :: clat0,slat0,clon0,slon0
  real    :: clat_m1,slat_m1,clon_m1,slon_m1
  real    :: clat_p1,slat_p1,clon_p1,slon_p1
  real    :: x,y,z,xt,yt,zt,xb,yb,zb
  real    :: rlonb_m1,clonb_m1,slonb_m1
  real    :: rlonb_p1,clonb_p1,slonb_p1
  real    :: crot,srot

  do j=1,nlon
     do i=1,nlat
        glats(j,i)=region_lat(i,j)
        glons(j,i)=region_lon(i,j)
     end do
  end do
  gt%pihalf=0.5*pi
  gt%nlon=nlon
  gt%nlat=nlat
  gt%rlon_min_dd=1.0
  gt%rlat_min_dd=1.0
  gt%rlon_max_dd=nlon
  gt%rlat_max_dd=nlat

!  define xtilde, ytilde grid, transform

!  glons,glats are lons, lats of input grid points of dimension nlon,nlat
  call general_get_xytilde_domain(gt,gt%nlon,gt%nlat,glons,glats,gt%nxtilde,gt%nytilde, &
                   xbar_min,xbar_max,ybar_min,ybar_max)
  if(gt%lallocated) then
     deallocate(gt%i0_tilde,gt%j0_tilde,gt%ip_tilde,gt%jp_tilde,gt%xtilde0,gt%ytilde0)
     deallocate(gt%cos_beta_ref,gt%sin_beta_ref,gt%region_lat,gt%region_lon)
     gt%lallocated=.false.
  end if
  allocate(gt%i0_tilde(gt%nxtilde,gt%nytilde),gt%j0_tilde(gt%nxtilde,gt%nytilde))
  allocate(gt%ip_tilde(gt%nxtilde,gt%nytilde),gt%jp_tilde(gt%nxtilde,gt%nytilde))
  allocate(gt%xtilde0(gt%nlon,gt%nlat),gt%ytilde0(gt%nlon,gt%nlat))
  allocate(gt%cos_beta_ref(gt%nlon,gt%nlat),gt%sin_beta_ref(gt%nlon,gt%nlat))
  allocate(gt%region_lat(gt%nlat,gt%nlon),gt%region_lon(gt%nlat,gt%nlon))

  gt%lallocated=.true.

  do j=1,nlon
     do i=1,nlat
        gt%region_lat(i,j)=region_lat(i,j)
        gt%region_lon(i,j)=region_lon(i,j)
     end do
  end do

! define atilde_x, btilde_x, atilde_y, btilde_y

  gt%btilde_x   =(gt%nxtilde -1.0     )/(xbar_max-xbar_min)
  gt%btilde_xinv=(xbar_max-xbar_min)/(gt%nxtilde -1.0     )
  gt%atilde_x   =1.0-gt%btilde_x*xbar_min
  gt%btilde_y   =(gt%nytilde -1.0     )/(ybar_max-ybar_min)
  gt%btilde_yinv=(ybar_max-ybar_min)/(gt%nytilde -1.0     )
  gt%atilde_y   =1.0-gt%btilde_y*ybar_min

! define xtilde0,ytilde0
  do j=1,gt%nlat
     do i=1,gt%nlon
        r_of_lat=gt%pihalf+gt%sign_pole*glats(i,j)
        clon=cos(glons(i,j)+gt%rlambda0)
        slon=sin(glons(i,j)+gt%rlambda0)
        xbar=r_of_lat*clon
        ybar=r_of_lat*slon
        gt%xtilde0(i,j)=gt%atilde_x+gt%btilde_x*xbar
        gt%ytilde0(i,j)=gt%atilde_y+gt%btilde_y*ybar
     end do
  end do

!  now get i0_tilde, j0_tilde, ip_tilde,jp_tilde
  ilast=1 ; jlast=1
  istart0=gt%nxtilde
  iend=1
  iinc=-1
  do j=1,gt%nytilde
     itemp=istart0
     istart0=iend
     iend=itemp
     iinc=-iinc
     ybar=j
     do i=istart0,iend,iinc
        xbar=i
        call general_nearest_3(ilast,jlast,gt%i0_tilde(i,j),gt%j0_tilde(i,j), &
                       gt%ip_tilde(i,j),gt%jp_tilde(i,j),xbar,ybar,gt%nlon,gt%nlat,gt%xtilde0,gt%ytilde0)
     end do
  end do

!   new, more accurate and robust computation of cos_beta_ref and sin_beta_ref which is independent
!     of sign_pole and works for any orientation of grid on sphere (only restriction for now is that
!     x-y coordinate of analysis grid is right handed).
  allocate(clata(gt%nlon,gt%nlat),slata(gt%nlon,gt%nlat),clona(gt%nlon,gt%nlat),slona(gt%nlon,gt%nlat))
  do j=1,gt%nlat
     do i=1,gt%nlon
        clata(i,j)=cos(glats(i,j))
        slata(i,j)=sin(glats(i,j))
        clona(i,j)=cos(glons(i,j))
        slona(i,j)=sin(glons(i,j))
     end do
  end do
  do j=1,gt%nlat
     do i=2,gt%nlon-1

!     do all interior lon points to 2nd order accuracy

!   transform so pole is at rlat0,rlon0 and 0 meridian is tangent to earth latitude at rlat0,rlon0.

        clat0=clata(i,j) ; slat0=slata(i,j) ; clon0=clona(i,j) ; slon0=slona(i,j)

!    now obtain new coordinates for m1 and p1 points.

        clat_m1=clata(i-1,j) ; slat_m1=slata(i-1,j) ; clon_m1=clona(i-1,j) ; slon_m1=slona(i-1,j)
        clat_p1=clata(i+1,j) ; slat_p1=slata(i+1,j) ; clon_p1=clona(i+1,j) ; slon_p1=slona(i+1,j)

        x=clat_m1*clon_m1 ; y=clat_m1*slon_m1 ; z=slat_m1
        xt=x*clon0+y*slon0 ; yt=-x*slon0+y*clon0 ; zt=z
        yb=zt*clat0-xt*slat0
        xb=yt
        zb=xt*clat0+zt*slat0

        rlonb_m1=atan2(-yb,-xb)   !  the minus signs here are so line for m1 is directed same
        clonb_m1=cos(rlonb_m1)
        slonb_m1=sin(rlonb_m1)

        x=clat_p1*clon_p1 ; y=clat_p1*slon_p1 ; z=slat_p1
        xt=x*clon0+y*slon0 ; yt=-x*slon0+y*clon0 ; zt=z
        yb=zt*clat0-xt*slat0
        xb=yt
        zb=xt*clat0+zt*slat0
        rlonb_p1=atan2(yb,xb)
        clonb_p1=cos(rlonb_p1)
        slonb_p1=sin(rlonb_p1)
        crot=0.5*(clonb_m1+clonb_p1)
        srot=0.5*(slonb_m1+slonb_p1)
        gt%cos_beta_ref(i,j)=crot*clon0-srot*slon0
        gt%sin_beta_ref(i,j)=srot*clon0+crot*slon0
     end do
!               now do i=1 and i=gt%nlon at 1st order accuracy
     i=1

!   transform so pole is at rlat0,rlon0 and 0 meridian is tangent to earth latitude at rlat0,rlon0.

        clat0=clata(i,j) ; slat0=slata(i,j) ; clon0=clona(i,j) ; slon0=slona(i,j)
!    now obtain new coordinates for m1 and p1 points.

        clat_p1=clata(i+1,j) ; slat_p1=slata(i+1,j) ; clon_p1=clona(i+1,j) ; slon_p1=slona(i+1,j)

        x=clat_p1*clon_p1 ; y=clat_p1*slon_p1 ; z=slat_p1
        xt=x*clon0+y*slon0 ; yt=-x*slon0+y*clon0 ; zt=z
        yb=zt*clat0-xt*slat0
        xb=yt
        zb=xt*clat0+zt*slat0
        rlonb_p1=atan2(yb,xb)
        clonb_p1=cos(rlonb_p1)
        slonb_p1=sin(rlonb_p1)
        crot=clonb_p1
        srot=slonb_p1
        gt%cos_beta_ref(i,j)=crot*clon0-srot*slon0
        gt%sin_beta_ref(i,j)=srot*clon0+crot*slon0

     i=gt%nlon

!   transform so pole is at rlat0,rlon0 and 0 meridian is tangent to earth latitude at rlat0,rlon0.

        clat0=clata(i,j) ; slat0=slata(i,j) ; clon0=clona(i,j) ; slon0=slona(i,j)

!    now obtain new coordinates for m1 and p1 points.

        clat_m1=clata(i-1,j) ; slat_m1=slata(i-1,j) ; clon_m1=clona(i-1,j) ; slon_m1=slona(i-1,j)

        x=clat_m1*clon_m1 ; y=clat_m1*slon_m1 ; z=slat_m1
        xt=x*clon0+y*slon0 ; yt=-x*slon0+y*clon0 ; zt=z
        yb=zt*clat0-xt*slat0
        xb=yt
        zb=xt*clat0+zt*slat0

        rlonb_m1=atan2(-yb,-xb)   !  the minus signs here are so line for m1 is directed same
        clonb_m1=cos(rlonb_m1)
        slonb_m1=sin(rlonb_m1)

        crot=clonb_m1
        srot=slonb_m1
        gt%cos_beta_ref(i,j)=crot*clon0-srot*slon0
        gt%sin_beta_ref(i,j)=srot*clon0+crot*slon0
  end do
  deallocate(clata,slata,clona,slona)

end subroutine general_create_llxy_transform

!-----------------------------------------------------------------------+
subroutine general_tll2xy(gt,rlon,rlat,x,y,outside)
!
! adopted from hafs sorc/hafs_gsi.fd/src/gsi/general_tll2xy_mod.f90
!
! abstract:  copy routines from gridmod.f90 to make a general purpose module which allows
!     conversion of earth lat lons to grid coordinates for any non-staggered rectangular
!     orthogonal grid with known earth lat and lon of each grid point.
!     general_tll2xy converts earth lon-lat to x-y grid units of a
!           general regional rectangular domain.  Also, decides if
!           point is inside this domain.  As a result, there is
!           no restriction on type of horizontal coordinate for
!           a regional run, other than that it not have periodicity
!           or polar singularities.
!           This is done by first converting rlon, rlat to an
!           intermediate coordinate xtilde,ytilde, which has
!           precomputed pointers and constants for final conversion
!           to the desired x,y via 3 point inverse interpolation.
!           All of the information needed is derived from arrays
!           specifying earth latitude and longitude of every point
!           on the input grid.  Currently, the input x-y grid that
!           this is based on must be non-staggered.  This restriction
!           will eventually be lifted so we can run directly from
!           model grids that are staggered without first resorting
!           to interpolation of the guess to a non-staggered grid.
!
  use var_type

  implicit none

    type(llxy_cons),intent(in) :: gt
    real,intent(in   ) :: rlon
    real,intent(in   ) :: rlat
    real,intent(  out) :: x
    real,intent(  out) :: y
    logical,intent(out):: outside

    real    :: clon,slon,r_of_lat,xtilde,ytilde
    real    :: dtilde,etilde
    real    :: d1tilde,d2tilde,e1tilde,e2tilde,detinv
    integer :: itilde,jtilde
    integer :: i0,j0,ip,jp

!   first compute xtilde, ytilde

    clon=cos(rlon+gt%rlambda0)
    slon=sin(rlon+gt%rlambda0)
    r_of_lat=gt%pihalf+gt%sign_pole*rlat

    xtilde=gt%atilde_x+gt%btilde_x*r_of_lat*clon
    ytilde=gt%atilde_y+gt%btilde_y*r_of_lat*slon

!  next get interpolation information

    itilde=max(1,min(nint(xtilde),gt%nxtilde))
    jtilde=max(1,min(nint(ytilde),gt%nytilde))

    i0     =   gt%i0_tilde(itilde,jtilde)
    j0     =   gt%j0_tilde(itilde,jtilde)
    ip     =i0+gt%ip_tilde(itilde,jtilde)
    jp     =j0+gt%jp_tilde(itilde,jtilde)
    dtilde =xtilde-gt%xtilde0(i0,j0)
    etilde =ytilde-gt%ytilde0(i0,j0)
    d1tilde=(gt%xtilde0(ip,j0)-gt%xtilde0(i0,j0))*(ip-i0)
    d2tilde=(gt%xtilde0(i0,jp)-gt%xtilde0(i0,j0))*(jp-j0)
    e1tilde=(gt%ytilde0(ip,j0)-gt%ytilde0(i0,j0))*(ip-i0)
    e2tilde=(gt%ytilde0(i0,jp)-gt%ytilde0(i0,j0))*(jp-j0)
    detinv =1.0/(d1tilde*e2tilde-d2tilde*e1tilde)
    x = i0+detinv*(e2tilde*dtilde-d2tilde*etilde)
    y = j0+detinv*(d1tilde*etilde-e1tilde*dtilde)

    outside=x < gt%rlon_min_dd .or. x > gt%rlon_max_dd .or. &
            y < gt%rlat_min_dd .or. y > gt%rlat_max_dd

    return

end subroutine general_tll2xy

!-----------------------------------------------------------------------+
subroutine general_txy2ll(gt,x,y,rlon,rlat)
!
! adopted from hafs sorc/hafs_gsi.fd/src/gsi/general_tll2xy_mod.f90
!
! abstract: convert x-y grid units to earth lat-lon coordinates
!
  use var_type

  implicit none

    type(llxy_cons),intent(in) :: gt
    real, intent(in   ) :: x
    real, intent(in   ) :: y
    real, intent(  out) :: rlon
    real, intent(  out) :: rlat

    real   :: r_of_lat,xtilde,ytilde
    real   :: dtilde,etilde,xbar,ybar
    real   :: d1tilde,d2tilde,e1tilde,e2tilde
    integer:: i0,j0,ip,jp

    i0=nint(x)
    j0=nint(y)
    i0=max(1,min(i0,gt%nlon))
    j0=max(1,min(j0,gt%nlat))
    ip=i0+nint(sign(1.0,x-i0))
    jp=j0+nint(sign(1.0,y-j0))
    if(ip<1) then
       i0=2
       ip=1
    end if
    if(jp<1) then
       j0=2
       jp=1
    end if
    if(ip>gt%nlon) then
       i0=gt%nlon-1
       ip=gt%nlon
    end if
    if(jp>gt%nlat) then
       j0=gt%nlat-1
       jp=gt%nlat
    end if
    d1tilde=(gt%xtilde0(ip,j0)-gt%xtilde0(i0,j0))*(ip-i0)
    d2tilde=(gt%xtilde0(i0,jp)-gt%xtilde0(i0,j0))*(jp-j0)
    e1tilde=(gt%ytilde0(ip,j0)-gt%ytilde0(i0,j0))*(ip-i0)
    e2tilde=(gt%ytilde0(i0,jp)-gt%ytilde0(i0,j0))*(jp-j0)
    dtilde =d1tilde*(x-i0) +d2tilde*(y-j0)
    etilde =e1tilde*(x-i0) +e2tilde*(y-j0)
    xtilde =dtilde         +gt%xtilde0(i0,j0)
    ytilde =etilde         +gt%ytilde0(i0,j0)

    xbar=(xtilde-gt%atilde_x)*gt%btilde_xinv
    ybar=(ytilde-gt%atilde_y)*gt%btilde_yinv
    r_of_lat=sqrt(xbar**2+ybar**2)
    rlat=(r_of_lat-gt%pihalf)*gt%sign_pole
    rlon=atan2(ybar,xbar)-gt%rlambda0

end subroutine general_txy2ll

!-----------------------------------------------------------------------+
subroutine general_nearest_3(ilast,jlast,i0,j0,ip,jp,x,y,nx0,ny0,x0,y0)
!
! adopted from hafs sorc/hafs_gsi.fd/src/gsi/general_tll2xy_mod.f90
!
! abstract: find closest 3 points to (x,y) on grid defined by x0,y0
!

  use var_type

  implicit none

  integer, intent(inout) :: ilast,jlast
  integer, intent(  out) :: i0,j0
  integer, intent(  out) :: ip,jp
  integer, intent(in   ) :: nx0,ny0
  real    ,intent(in   ) :: x,y
  real    ,intent(in   ) :: x0(nx0,ny0),y0(nx0,ny0)

  real    :: dista,distb,dist2,dist2min
  integer :: i,inext,j,jnext

  do
     i0=ilast
     j0=jlast
     dist2min=huge(dist2min)
     inext=0
     jnext=0
     do j=max(j0-1,1),min(j0+1,ny0)
        do i=max(i0-1,1),min(i0+1,nx0)
           dist2=(x-x0(i,j))**2+(y-y0(i,j))**2
           if(dist2<dist2min) then
              dist2min=dist2
              inext=i
              jnext=j
           end if
        end do
     end do
     if(inext==i0.and.jnext==j0) exit
     ilast=inext
     jlast=jnext
  end do

!  now find which way to go in x for second point

  ip=0
  if(i0==nx0)  ip=-1
  if(i0==1) ip=1
  if(ip==0) then
     dista=(x-x0(i0-1,j0))**2+(y-y0(i0-1,j0))**2
     distb=(x-x0(i0+1,j0))**2+(y-y0(i0+1,j0))**2
     if(distb<dista) then
        ip=1
     else
        ip=-1
     end if
  end if

!  repeat for y for 3rd point

  jp=0
  if(j0==ny0  ) jp=-1
  if(j0==1 ) jp=1
  if(jp==0) then
     dista=(x-x0(i0,j0-1))**2+(y-y0(i0,j0-1))**2
     distb=(x-x0(i0,j0+1))**2+(y-y0(i0,j0+1))**2
     if(distb<dista) then
        jp=1
     else
        jp=-1
     end if
  end if

  ilast=i0
  jlast=j0

end subroutine general_nearest_3

!-----------------------------------------------------------------------+
subroutine general_get_xytilde_domain(gt,nx0,ny0,rlons0,rlats0, &
                                  nx,ny,xminout,xmaxout,yminout,ymaxout)
!
! adopted from hafs sorc/hafs_gsi.fd/src/gsi/general_tll2xy_mod.f90

  use constants
  use var_type

  implicit none

  type(llxy_cons),intent(inout) :: gt
  integer, intent(in   ) :: nx0,ny0
  real, intent(in   )    :: rlons0(nx0,ny0),rlats0(nx0,ny0)

  integer,intent(  out) :: nx,ny
  real   ,intent(  out) :: xminout,xmaxout,yminout,ymaxout

  real,parameter:: r37=37.0

  real    :: area,areamax,areamin,extra,rlats0max,rlats0min,testlambda
  real    :: xthis,ythis
  integer :: i,ip1,j,jp1,m

  real    :: coslon0(nx0,ny0),sinlon0(nx0,ny0)
  real    :: coslat0(nx0,ny0),sinlat0(nx0,ny0)
  real    :: count,delbar
  real    :: dx,dy,disti,distj,distmin,distmax
  real    :: xmin,xmax,ymin,ymax

!  get range of lats for input grid

  rlats0max=maxval(rlats0) ; rlats0min=minval(rlats0)

!   assign hemisphere ( parameter sign_pole )

  if(rlats0min>-r37*deg2rad) gt%sign_pole=-1.0   !  northern hemisphere xy domain
  if(rlats0max< r37*deg2rad) gt%sign_pole= 1.0   !  southern hemisphere xy domain


!   get optimum rotation angle rlambda0

  areamin= huge(areamin)
  areamax=-huge(areamax)
  do m=0,359
     testlambda=m*deg2rad
     xmax=-huge(xmax)
     xmin= huge(xmin)
     ymax=-huge(ymax)
     ymin= huge(ymin)
     do j=1,ny0,ny0-1
        do i=1,nx0
           xthis=(gt%pihalf+gt%sign_pole*rlats0(i,j))*cos(rlons0(i,j)+testlambda)
           ythis=(gt%pihalf+gt%sign_pole*rlats0(i,j))*sin(rlons0(i,j)+testlambda)
           xmax=max(xmax,xthis)
           ymax=max(ymax,ythis)
           xmin=min(xmin,xthis)
           ymin=min(ymin,ythis)
        end do
     end do
     do j=1,ny0
        do i=1,nx0,nx0-1
           xthis=(gt%pihalf+gt%sign_pole*rlats0(i,j))*cos(rlons0(i,j)+testlambda)
           ythis=(gt%pihalf+gt%sign_pole*rlats0(i,j))*sin(rlons0(i,j)+testlambda)
           xmax=max(xmax,xthis)
           ymax=max(ymax,ythis)
           xmin=min(xmin,xthis)
           ymin=min(ymin,ythis)
        end do
     end do
     area=(xmax-xmin)*(ymax-ymin)
     areamax=max(area,areamax)
     if(area<areamin) then
        areamin =area
        gt%rlambda0=testlambda
        xmaxout =xmax
        xminout =xmin
        ymaxout =ymax
        yminout =ymin
     end if
  end do


!   now determine resolution of input grid and choose nx,ny of xy grid accordingly
!                 (currently hard-wired at 1/2 the average input grid increment)

  do j=1,ny0
     do i=1,nx0
        coslon0(i,j)=cos(1.0*rlons0(i,j)) ; sinlon0(i,j)=sin(1.0*rlons0(i,j))
        coslat0(i,j)=cos(1.0*rlats0(i,j)) ; sinlat0(i,j)=sin(1.0*rlats0(i,j))
     end do
  end do

  delbar=0.0
  count =0.0
  do j=1,ny0-1
     jp1=j+1
     do i=1,nx0-1
        ip1=i+1
        disti=acos(sinlat0(i,j)*sinlat0(ip1,j)+coslat0(i,j)*coslat0(ip1,j)* &
                  (sinlon0(i,j)*sinlon0(ip1,j)+coslon0(i,j)*coslon0(ip1,j)))
        distj=acos(sinlat0(i,j)*sinlat0(i,jp1)+coslat0(i,j)*coslat0(i,jp1)* &
                  (sinlon0(i,j)*sinlon0(i,jp1)+coslon0(i,j)*coslon0(i,jp1)))
        distmax=max(disti,distj)
        distmin=min(disti,distj)
        delbar=delbar+distmax
        count=count+1.0
     end do
  end do
  delbar=delbar/count
  dx=0.5*delbar
  dy=dx

!   add extra space to computational grid to push any boundary problems away from
!     area of interest

  extra=10.0*dx
  xmaxout=xmaxout+extra
  xminout=xminout-extra
  ymaxout=ymaxout+extra
  yminout=yminout-extra
  nx=1+(xmaxout-xminout)/dx
  ny=1+(ymaxout-yminout)/dy

end subroutine general_get_xytilde_domain

!-----------------------------------------------------------------------+
subroutine general_rotate_wind_ll2xy(gt,u0,v0,u,v,rlon0,x,y)
!
! adopted from hafs sorc/hafs_gsi.fd/src/gsi/general_tll2xy_mod.f90
!
! DESCRIPTION: to convert earth vector wind components to corresponding local x,y coordinate

  use var_type

  implicit none

    type(llxy_cons),intent(in) :: gt
    real, intent(in   ) :: u0,v0        ! earth wind component
    real, intent(in   ) :: rlon0        ! earth   lon (radians)
    real, intent(in   ) :: x,y          ! local x,y coordinate (grid units)
    real, intent(  out) :: u,v          ! rotated coordinate of winds

  real    :: beta,delx,delxp,dely,delyp
  real    :: sin_beta,cos_beta
  integer :: ix,iy

!  interpolate departure from longitude part of angle between earth positive east and local positive x

  ix=x
  iy=y
  ix=max(1,min(ix,gt%nlon-1))
  iy=max(1,min(iy,gt%nlat-1))
  delx=x-ix
  dely=y-iy
  delxp=1.0-delx
  delyp=1.0-dely
  cos_beta=gt%cos_beta_ref(ix  ,iy  )*delxp*delyp+gt%cos_beta_ref(ix+1,iy  )*delx *delyp+ &
           gt%cos_beta_ref(ix  ,iy+1)*delxp*dely +gt%cos_beta_ref(ix+1,iy+1)*delx *dely
  sin_beta=gt%sin_beta_ref(ix  ,iy  )*delxp*delyp+gt%sin_beta_ref(ix+1,iy  )*delx *delyp+ &
           gt%sin_beta_ref(ix  ,iy+1)*delxp*dely +gt%sin_beta_ref(ix+1,iy+1)*delx *dely
  beta=atan2(sin_beta,cos_beta)

!  now rotate;

  u= u0*cos(beta-rlon0)+v0*sin(beta-rlon0)
  v=-u0*sin(beta-rlon0)+v0*cos(beta-rlon0)

end subroutine general_rotate_wind_ll2xy

!-----------------------------------------------------------------------+
subroutine general_rotate_wind_xy2ll(gt,u,v,u0,v0,rlon0,x,y)
!
! adopted from hafs sorc/hafs_gsi.fd/src/gsi/general_tll2xy_mod.f90
!
! DESCRIPTION: rotate u,v in local x,y coordinate to u0,v0 in earth lat, lon coordinate

  use var_type

  implicit none

    type(llxy_cons),intent(in) :: gt
    real, intent(in   ) :: u,v         ! rotated coordinate winds
    real, intent(in   ) :: rlon0       ! earth   lon     (radians)
    real, intent(in   ) :: x,y         ! rotated lon/lat (radians)
    real, intent(  out) :: u0,v0       ! earth winds

  real    :: beta,delx,delxp,dely,delyp
  real    :: sin_beta,cos_beta
  integer :: ix,iy

!  interpolate departure from longitude part of angle between earth
!  positive east and local positive x

  ix=x
  iy=y
  ix=max(1,min(ix,gt%nlon-1))
  iy=max(1,min(iy,gt%nlat-1))
  delx=x-ix
  dely=y-iy
  delxp=1.0-delx
  delyp=1.0-dely
  cos_beta=gt%cos_beta_ref(ix  ,iy  )*delxp*delyp+gt%cos_beta_ref(ix+1,iy  )*delx *delyp+ &
           gt%cos_beta_ref(ix  ,iy+1)*delxp*dely +gt%cos_beta_ref(ix+1,iy+1)*delx *dely
  sin_beta=gt%sin_beta_ref(ix  ,iy  )*delxp*delyp+gt%sin_beta_ref(ix+1,iy  )*delx *delyp+ &
           gt%sin_beta_ref(ix  ,iy+1)*delxp*dely +gt%sin_beta_ref(ix+1,iy+1)*delx *dely
  beta=atan2(sin_beta,cos_beta)

!  now rotate;

  u0= u*cos(beta-rlon0)-v*sin(beta-rlon0)
  v0= u*sin(beta-rlon0)+v*cos(beta-rlon0)

end subroutine general_rotate_wind_xy2ll

!-----------------------------------------------------------------------+
