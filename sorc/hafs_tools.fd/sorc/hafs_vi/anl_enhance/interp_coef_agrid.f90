SUBROUTINE EARTH_LATLON_AGRID ( HLAT,HLON,VLAT,VLON,     & !Earth lat,lon at H and V points
                                LON1,LAT1,LON2,LAT2,     & !input res,west & south boundaries,
                                CENTRAL_LAT,CENTRAL_LON, & ! central lat,lon, all in degrees
                                IM,JM)
!
!============================================================================
!
 IMPLICIT NONE
 INTEGER,    INTENT(IN)                 :: IM,JM
 REAL(4),    INTENT(IN)                 :: LON1,LAT1,LON2,LAT2
 REAL(4),    INTENT(IN)                 :: CENTRAL_LAT,CENTRAL_LON
 REAL(4), DIMENSION(IM,JM), INTENT(OUT) :: HLAT,HLON,VLAT,VLON

! local

 INTEGER,PARAMETER                 :: KNUM=8
 INTEGER                           :: I,J
 REAL(KNUM)                        :: rot_lon,rot_lat,geo_lon,geo_lat
 REAL(KNUM)                        :: cen_lon, cen_lat
!-------------------------------------------------------------------------

            cen_lon = CENTRAL_LON
            cen_lat = CENTRAL_LAT

            do j=1,jm
            do i=1,im
              rot_lon = lon1 + (lon2-lon1)/(im-1) * (i-1)
              rot_lat = lat1 + (lat2-lat1)/(jm-1) * (j-1)
              call rtll(rot_lon, rot_lat, geo_lon, geo_lat, cen_lon, cen_lat)
!              if (geo_lon <0.0) geo_lon = geo_lon + 360.0
              HLON(i,j) = geo_lon
              HLAT(i,j) = geo_lat
            enddo
            enddo

           VLON = HLON
           VLAT = HLAT

END SUBROUTINE EARTH_LATLON_AGRID

!-----------------------------------------------------------------------------

SUBROUTINE G2T2H_AGRID( IIH,JJH,                     & ! output grid index
                        HBWGT,                       & ! output weights in terms of parent grid
                        HLAT,HLON,                   & ! target (nest) input lat lon in degrees
                        DLMD1,DPHD1,WBD1,SBD1,       & ! parent res, west and south boundaries
                        CENTRAL_LAT,CENTRAL_LON,     & ! parent central lat,lon, all in degrees
                        P_IM,P_JM,                   & ! parent imax and jmax
                        IM,JM)                         ! target (nest) dimensions
!
!============================================================================
!
 IMPLICIT NONE
 INTEGER,    INTENT(IN)                       :: IM,JM
 INTEGER,    INTENT(IN)                       :: P_IM,P_JM
 REAL(4),    INTENT(IN)                       :: DLMD1,DPHD1,WBD1,SBD1
 REAL(4),    INTENT(IN)                       :: CENTRAL_LAT,CENTRAL_LON
 REAL(4),    DIMENSION(IM,JM),   INTENT(IN)   :: HLAT,HLON
 REAL(4),    DIMENSION(IM,JM,4), INTENT(OUT)  :: HBWGT
 INTEGER,    DIMENSION(IM,JM,4), INTENT(OUT)  :: IIH,JJH
! local

 INTEGER                           :: I,J
 INTEGER                           :: I1,I2,J1,J2
 REAL(8)                           :: X,Y,XI,YI,XF,YF
!-------------------------------------------------------------------------

      DO J = 1,JM
       DO I = 1,IM

         CALL TLL(HLON(I,J),HLAT(I,J),X,Y,CENTRAL_LAT,CENTRAL_LON)

         XI = (X-WBD1)/DLMD1 + 1
         YI = (Y-SBD1)/DPHD1 + 1
         IF(abs(XI-1.).lt.0.005)XI=1.0
         I1 = XI
         I2 = I1+1
         IF(abs(YI-1.).lt.0.005)YI=1.0
         J1 = YI
         J2 = J1+1
         XF=XI-I1
         YF=YI-J1
         IIH(I,J,1)=I1
         IIH(I,J,2)=I2
         IIH(I,J,3)=I1
         IIH(I,J,4)=I2
         JJH(I,J,1)=J1
         JJH(I,J,2)=J1
         JJH(I,J,3)=J2
         JJH(I,J,4)=J2
         HBWGT(I,J,1)=(1-XF)*(1-YF)
         HBWGT(I,J,2)=XF*(1-YF)
         HBWGT(I,J,3)=(1-XF)*YF
         HBWGT(I,J,4)=XF*YF

       ENDDO
      ENDDO

END SUBROUTINE G2T2H_AGRID



SUBROUTINE G2T2V_BGRID( IIV,JJV,                     & ! output grid index and weights
                        VBWGT,                       & ! output weights in terms of parent grid
                        VLAT,VLON,                   & ! target (nest) input lat lon in degrees
                        DLMD1,DPHD1,WBD1,SBD1,       & ! parent res, west and south boundaries
                        CENTRAL_LAT,CENTRAL_LON,     & ! parent central lat,lon, all in degrees
                        P_IM,P_JM,                   & ! parent imax and jmax
                        IM,JM)                         ! target (nest) dimensions
!
!============================================================================
!
 IMPLICIT NONE
 INTEGER,    INTENT(IN)                       :: IM,JM
 INTEGER,    INTENT(IN)                       :: P_IM,P_JM
 REAL(4),    INTENT(IN)                       :: DLMD1,DPHD1,WBD1,SBD1
 REAL(4),    INTENT(IN)                       :: CENTRAL_LAT,CENTRAL_LON
 REAL(4),    DIMENSION(IM,JM),   INTENT(IN)   :: VLAT,VLON
 REAL(4),    DIMENSION(IM,JM,4), INTENT(OUT)  :: VBWGT
 INTEGER,    DIMENSION(IM,JM,4), INTENT(OUT)  :: IIV,JJV

! local

 INTEGER                           :: I,J
 INTEGER                           :: I1,I2,J1,J2
 REAL(8)                           :: X,Y,XI,YI,XF,YF
!-------------------------------------------------------------------------

      DO J = 1,JM
       DO I = 1,IM

         CALL TLL(VLON(I,J),VLAT(I,J),X,Y,CENTRAL_LAT,CENTRAL_LON)

         XI = (X-WBD1-0.5*DLMD1)/DLMD1 + 1
         YI = (Y-SBD1-0.5*DPHD1)/DPHD1 + 1
         IF(abs(XI-1.).lt.0.005)XI=1.0
         I1 = XI
         I2 = I1+1
         IF(abs(YI-1.).lt.0.005)YI=1.0
         J1 = YI
         J2 = J1+1
         XF=XI-I1
         YF=YI-J1
         IIV(I,J,1)=I1
         IIV(I,J,2)=I2
         IIV(I,J,3)=I1
         IIV(I,J,4)=I2
         JJV(I,J,1)=J1
         JJV(I,J,2)=J1
         JJV(I,J,3)=J2
         JJV(I,J,4)=J2
         VBWGT(I,J,1)=(1-XF)*(1-YF)
         VBWGT(I,J,2)=XF*(1-YF)
         VBWGT(I,J,3)=(1-XF)*YF
         VBWGT(I,J,4)=XF*YF

       ENDDO
      ENDDO


 RETURN
 END SUBROUTINE G2T2V_BGRID

   subroutine rtll(tlmd,tphd,almd,aphd,tlm0d,tph0d)
!-------------------------------------------------------------------------------
      INTEGER,PARAMETER          :: KIND_R8=8

      real(KIND_R8), intent(in)  :: tlmd, tphd
      real(KIND_R8), intent(out) :: almd, aphd
      real(KIND_R8), intent(in)  :: tph0d, tlm0d
!-------------------------------------------------------------------------------
      real(KIND_R8), parameter   :: pi=3.14159265358979323846
      real(KIND_R8), parameter   :: dtr=pi/180.0
!
      real(KIND_R8) :: tph0, ctph0, stph0, tlm, tph, stph, ctph, ctlm, stlm, aph, cph
      real(KIND_R8) :: xx, yy
!-------------------------------------------------------------------------------
!
      tph0=tph0d*dtr
      ctph0=cos(tph0)
      stph0=sin(tph0)
!
      tlm=tlmd*dtr
      tph=tphd*dtr
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
      almd=tlm0d+xx

      aphd=aph/dtr
!
      if (almd > 180.0) then
         almd=almd-360.0
      end if
      if (almd < -180.0)  then
         almd=almd+360.0
      end if
!
      return
!
     end subroutine rtll


   subroutine tll(almd,aphd,tlmd,tphd,tph0d,tlm0d)
!-------------------------------------------------------------------------------
      implicit none
!-------------------------------------------------------------------------------
      real, intent(in) :: almd, aphd
      real(8), intent(out) :: tlmd, tphd
      real, intent(in) :: tph0d, tlm0d
!-------------------------------------------------------------------------------
      real, parameter :: pi=3.141592654
      real(8), parameter :: dtr=pi/180.0
!
      real(8) :: tph0, ctph0, stph0, relm, srlm, crlm
      real(8) :: aph, sph, cph, cc, anum, denom
!-------------------------------------------------------------------------------
!
      if (tlm0d==0.0.and.tph0d==0.0) then
      tlmd=almd
      tphd=aphd
      else

      tph0=tph0d*dtr
      ctph0=dcos(tph0)
      stph0=dsin(tph0)
!
      relm=(almd-tlm0d)*dtr
      srlm=dsin(relm)
      crlm=dcos(relm)
      aph=aphd*dtr
      sph=dsin(aph)
      cph=dcos(aph)
      cc=cph*crlm
      anum=cph*srlm
      denom=ctph0*cc+stph0*sph
!
      tlmd=datan2(anum,denom)/dtr
      tphd=dasin(ctph0*sph-stph0*cc)/dtr

      end if
!
      return
!
   end subroutine tll


!! subroutine 'get_eta_level' returns the interface and
!! layer-mean pressures for reference.
       subroutine get_eta_level(npz, p_s, pf, ph, ak, bk, pscale)
       integer, intent(in) :: npz
       real, intent(in)  :: p_s            !< unit: pascal
       real, intent(in)  :: ak(npz+1)
       real, intent(in)  :: bk(npz+1)
       real, intent(in), optional :: pscale
       real, intent(out) :: pf(npz)
       real, intent(out) :: ph(npz+1)

       real, parameter   :: RDGAS  = 287.05              !< Gas constant for dry air [J/kg/deg]
       real, parameter   :: CP_AIR = 1004.6              !< Specific heat capacity of dry air at constant pressure [J/kg/deg]
       real, parameter   :: KAPPA  = RDGAS/CP_AIR        !< RDGAS / CP_AIR [dimensionless]
       integer k

       ph(1) = ak(1)
       do k=2,npz+1
         ph(k) = ak(k) + bk(k)*p_s
       enddo

!       if ( present(pscale) ) then
!         do k=1,npz+1
!         ph(k) = pscale*ph(k)
!       enddo
!       endif

       if( ak(1) > 1.E-8 ) then
          pf(1) = (ph(2) - ph(1)) / log(ph(2)/ph(1))
       else
          pf(1) = (ph(2) - ph(1)) * kappa/(kappa+1.)
       endif

       do k=2,npz
         pf(k) = (ph(k+1) - ph(k)) / log(ph(k+1)/ph(k))
       enddo

      end subroutine get_eta_level

