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
  if (geo_lon > 180.0) then
     geo_lon=geo_lon-360.0
  end if
  if (geo_lon < -180.0)  then
     geo_lon=geo_lon+360.0
  end if
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

  if (lon >  180.) lon = lon - 360.
  if (lon < -180.) lon = lon + 360.

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
!============================================================================
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
          IF(HLON(I,J) .GT. 180.) HLON(I,J) = HLON(I,J)  - 360.
          IF(HLON(I,J) .LT. -180.) HLON(I,J) = HLON(I,J) + 360.
!
          VLAT(I,J) = GLATV(I,J) / DTR
          VLON(I,J) = -GLONV(I,J) / DTR
          IF(VLON(I,J) .GT. 180.) VLON(I,J) = VLON(I,J)  - 360.
          IF(VLON(I,J) .LT. -180.) VLON(I,J) = VLON(I,J) + 360.

       ENDDO
      ENDDO

END SUBROUTINE EARTH_LATLON
 
!-----------------------------------------------------------------------+
