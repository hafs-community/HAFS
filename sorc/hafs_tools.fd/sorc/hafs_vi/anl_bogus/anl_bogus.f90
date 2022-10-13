!??????????????????????????????????????????????????????????
!      SUBROUTINE STORM_RELOCATE(NX,NY,NZ,T1,Q1,U1,V1,Z1,
!     &                          GLON,GLAT,P1,PT,PDTOP,PD,ETA1,ETA2)
! SUBPROGRAM
!   PRGRMMR
!
! ABSTRACT
!
!     DECLARE VARIABLES
!
      INTEGER I,J,K,NX,NY,NZ,ICH
!
      PARAMETER (NST=5,IR=200)
!      PARAMETER (NX=158,NY=329,NZ=42,NST=5)
      PARAMETER (GAMMA=6.5E-3,G=9.8,Rd=287.05,D608=0.608)
      PARAMETER (Cp=1004.)

!      PARAMETER (KMX=2*NZ+1)
!
      PARAMETER (NZ2=121,IR1=201)

! Variables on 4x hybrid coordinate

      REAL(4) DLMD,DPHD,PT,PDTOP
      REAL(4) WBD,SBD,CENTRAL_LON,CENTRAL_LAT
      REAL(4) LON1,LAT1,LON2,LAT2

      REAL(4), ALLOCATABLE :: T1(:,:,:),Q1(:,:,:)
      REAL(4), ALLOCATABLE :: U1(:,:,:),V1(:,:,:),DZDT(:,:,:)
      REAL(4), ALLOCATABLE :: Z1(:,:,:),P1(:,:,:)
      REAL(4), ALLOCATABLE :: GLON(:,:),GLAT(:,:)
      REAL(4), ALLOCATABLE :: PD1(:,:),ETA1(:),ETA2(:)

      REAL(4), ALLOCATABLE :: USCM(:,:),VSCM(:,:)        ! Env. wind at new grids

      REAL(4), ALLOCATABLE :: T4(:,:,:),Q4(:,:,:)

! variables for hurricane component

      REAL(4), ALLOCATABLE :: SLPE(:,:),SLP_1(:,:),TENV(:,:,:)
      REAL(4), ALLOCATABLE :: T_1(:,:,:),Q_1(:,:,:)
      REAL(4), ALLOCATABLE :: U_1(:,:,:),V_1(:,:,:)

      REAL(4), ALLOCATABLE :: T_2(:,:,:),Q_2(:,:,:),SLP_2(:,:)
      REAL(4), ALLOCATABLE :: U_2(:,:,:),V_2(:,:,:),SLPE2(:,:)

      REAL(4), ALLOCATABLE :: U_2SB(:,:),T_2SB(:,:),SLP_2SB(:)
      REAL(4), ALLOCATABLE :: Q_2SB(:,:),temp_e(:),TEK(:),RADIUS2(:)

      REAL(4), ALLOCATABLE :: T_X(:,:,:),Q_X(:,:,:),SLP_X(:,:)
      REAL(4), ALLOCATABLE :: work_1(:),work_2(:)

! Variables for old domain

      REAL(4) DLMD2,DPHD2,PT2,PDTOP2
      REAL(4) WBD2,SBD2,CENTRAL_LON2,CENTRAL_LAT2

! working array

      REAL(4), ALLOCATABLE :: SLP1(:,:),RIJ(:,:)
      REAL(4), ALLOCATABLE :: PMID1(:,:,:),ZMID1(:,:,:)
      REAL(4), ALLOCATABLE :: ZS1(:,:),TS1(:,:),QS1(:,:)

      REAL(4), ALLOCATABLE :: HLON(:,:),HLAT(:,:)
      REAL(4), ALLOCATABLE :: VLON(:,:),VLAT(:,:)

      REAL(4), ALLOCATABLE :: U_S(:,:),V_S(:,:)
      REAL(4), ALLOCATABLE :: U_A(:,:),V_A(:,:)

      REAL(4), ALLOCATABLE :: USC(:,:),VSC(:,:)        ! Env. wind at new grids
      REAL(4), ALLOCATABLE :: USC_1(:,:),VSC_1(:,:)      ! hurricane component wind at z=0
      REAL(4), ALLOCATABLE :: USC1(:,:),VSC1(:,:)        ! Hurricane wind at new grids
      REAL(4), ALLOCATABLE :: SLPV(:,:)

      REAL(4), ALLOCATABLE :: HLON1(:,:),HLAT1(:,:)
      REAL(4), ALLOCATABLE :: VLON1(:,:),VLAT1(:,:)
      REAL(4), ALLOCATABLE :: T21(:,:,:,:),Q21(:,:,:,:)
      REAL(4), ALLOCATABLE :: U21(:,:,:,:),V21(:,:,:,:)
      REAL(4), ALLOCATABLE :: SLP21(:,:,:)
      REAL(4), ALLOCATABLE :: PMV1(:,:,:),PMV2(:,:,:)

      REAL(4), ALLOCATABLE :: A101(:,:),B101(:,:),C101(:,:)

      REAL(4), ALLOCATABLE :: T_4(:,:,:),Q_4(:,:,:)

      REAL(8), ALLOCATABLE :: WRK1(:),WRK2(:),WRK3(:),WRK4(:)

      REAL(4), ALLOCATABLE :: PCST(:),HP(:,:,:),HV(:,:,:)
      REAL(4), ALLOCATABLE :: P_S(:,:),P_A(:,:)
      REAL(4), ALLOCATABLE :: PCST1(:,:,:),PCST2(:)

      REAL(4), ALLOCATABLE ::    HBWGT1(:,:,:),VBWGT1(:,:,:)
      integer(4), ALLOCATABLE :: IIH1(:,:),JJH1(:,:)
      integer(4), ALLOCATABLE :: IIV1(:,:),JJV1(:,:)

      REAL(4), ALLOCATABLE :: dist(:,:)

      integer(4) IH1(4),JH1(4),IV1(4),JV1(4)

      REAL(8) CLON_NEW,CLAT_NEW,CLON_NHC,CLAT_NHC

      REAL(4) th1(IR1),rp(IR1)               ! ,RMN

      REAL(4) zmax,PW(121),PW_S(121),PW_M(121)

      integer Ir_v4(4)
      CHARACTER SN*1,EW*1,DEPTH*1

      DATA PW_S/42*1.0,0.95,0.9,0.85,0.8,0.75,0.7,       &
	        0.65,0.6,0.55,0.5,0.45,0.4,0.35,0.3,     &
                0.25,0.2,0.15,0.1,0.05,60*0./                        ! 850-700mb

      DATA PW_M/121*1.0/
!      DATA PW_M/40*1.0,0.95,0.9,0.8,0.7,          &
!	        0.6,0.5,0.4,0.3,0.2,0.1,35*0./                    ! 850-300mb
!zhang: added basin domain shift option
      CHARACTER*2 basin

      print*,'this is cold start'

      COEF1=Rd/Cp
      COEF3=Rd*GAMMA/G
      COEF2=1./COEF3

      GRD=G/Rd

      pi=4.*atan(1.)
      pi_deg=180./pi
      rad=1./pi_deg

      arad=6.371E6*rad

      READ(5,*)ITIM,basin


! READ NEW GFS Env. DATA (New Domain)

      IUNIT=20+ITIM

      READ(IUNIT) NX,NY,NZ,I360

      print*,'NX,NY,NZ=',NX,NY,NZ,I360

      NX1=NX+1
      NY1=NY+1
      NZ1=NZ+1

      KMX=121

      ALLOCATE ( T1(NX,NY,NZ),Q1(NX,NY,NZ) )
      ALLOCATE ( U1(NX,NY,NZ),V1(NX,NY,NZ),DZDT(NX,NY,NZ) )
      ALLOCATE ( Z1(NX,NY,NZ+1),P1(NX,NY,NZ+1) )
      ALLOCATE ( GLON(NX,NY),GLAT(NX,NY) )
      ALLOCATE ( PD1(NX,NY),ETA1(NZ+1),ETA2(NZ+1) )
      ALLOCATE ( USC(NX,NY),VSC(NX,NY) )        ! Env. wind at new grids

      ALLOCATE ( T4(NX,NY,NZ),Q4(NX,NY,NZ) )    ! orginal data (GFS analysis data)

      ALLOCATE ( TEK(NZ) )

      ALLOCATE ( dist(NX,NY) )

      ALLOCATE ( HLON(NX,NY),HLAT(NX,NY) )
      ALLOCATE ( VLON(NX,NY),VLAT(NX,NY) )
      ALLOCATE ( PMID1(NX,NY,NZ),ZMID1(NX,NY,NZ) )

      READ(IUNIT) LON1,LAT1,LON2,LAT2,CENTRAL_LON,CENTRAL_LAT
      READ(IUNIT) PMID1
      READ(IUNIT) T1
      READ(IUNIT) Q1
      READ(IUNIT) U1
      READ(IUNIT) V1
      READ(IUNIT) DZDT
      READ(IUNIT) Z1
!      READ(IUNIT) GLON,GLAT
      READ(IUNIT) HLON,HLAT,VLON,VLAT
      READ(IUNIT) P1
      READ(IUNIT) PD1
      READ(IUNIT) ETA1
      READ(IUNIT) ETA2

      READ(IUNIT) USC
      READ(IUNIT) VSC

      CLOSE(IUNIT)

      IUNIT=30+ITIM    ! Original GFS initial data

      READ(IUNIT)   ! NX,NY,NZ
      READ(IUNIT)   ! DLMD,DPHD,CENTRAL_LON,CENTRAL_LAT
      READ(IUNIT)   ! PT,PDTOP
      READ(IUNIT) T4
      READ(IUNIT) Q4
      READ(IUNIT)
      READ(IUNIT)
      READ(IUNIT)
      READ(IUNIT)
      READ(IUNIT)
      READ(IUNIT)
      READ(IUNIT)
      READ(IUNIT)

      CLOSE(IUNIT)

      ALLOCATE ( SLP1(NX,NY),RIJ(NX,NY) )
      ALLOCATE ( ZS1(NX,NY),TS1(NX,NY),QS1(NX,NY) )

      ALLOCATE ( USCM(NX,NY),VSCM(NX,NY) )
      ALLOCATE ( P_S(NX,NY),P_A(NX,NY) )

!
! First, compute variables at surface level (SLP1,TS1,QS1)

      DO J=1,NY
      DO I=1,NX
        GLON(I,J)=HLON(I,J)
        GLAT(I,J)=HLAT(I,J)
      END DO
      END DO

       DO K=1,NZ
       DO J=1,NY
       DO I=1,NX
          TV1=T1(I,J,K)*(1.+D608*Q1(I,J,K))
          ZMID1(I,J,K)=(Z1(I,J,K)+Z1(I,J,K+1))*0.5+             &
            0.5*TV1/GAMMA*(2.-(P1(I,J,K)/PMID1(I,J,K))**COEF3-  &
            (P1(I,J,K+1)/PMID1(I,J,K))**COEF3)
!         PMID1(I,J,K)=EXP((ALOG(P1(I,J,K))+ALOG(P1(I,J,K+1)))*0.5)
!         ZMID1(I,J,K)=0.5*(Z1(I,J,K)+Z1(I,J,K+1))
!         THET1(I,J,K)=T1(I,J,K)*(1.E6/PMID1(I,J,K))**COEF1
       ENDDO
       ENDDO
       ENDDO

       DO J=1,NY                                          ! given variables from domain 1
       DO I=1,NX                                          ! in case there is no data from domain 2
         ZS1(I,J)=Z1(I,J,1)
         TS1(I,J) =T1(I,J,1)+GAMMA*(ZMID1(I,J,1)-Z1(I,J,1))
         QS1(I,J) =Q1(I,J,1)
      ENDDO
      ENDDO


!C        COMPUTE SEA LEVEL PRESSURE.
!C
       DO J=1,NY
       DO I=1,NX
         ZSF1 = ZMID1(I,J,1)
         PSF1 = PMID1(I,J,1)
         TV1 = T1(I,J,1)*(1.+D608*Q1(I,J,1))
         A = (GAMMA * ZSF1) / TV1
         SLP1(I,J) = PSF1*(1+A)**COEF2
      ENDDO
      ENDDO

       SLP1_MEAN=0.
       SUM11=0.

       DO J=1,NY
       DO I=1,NX
         SLP1_MEAN=SLP1_MEAN+SLP1(I,J)
	 SUM11=SUM11+1
      ENDDO
      ENDDO

      SLP1_MEAN=SLP1_MEAN/SUM11

! correct to surface pert P

      JM1=0.5*NY
      IM1=0.5*NX

      SLP_AVE=0.
      SLP_SUM=0.
      DO J=JM1-100,JM1+100
      DO I=IM1-50,IM1+50
        SLP_AVE=SLP_AVE+SLP1(I,J)
        SLP_SUM=SLP_SUM+1.
      END DO
      END DO

      print*,'SLP_TT,SLP_SUM=',SLP_AVE,SLP_SUM

      SLP_AVE=SLP_AVE/SLP_SUM

      print*,'SLP_AVE 1 =',SLP_AVE

      SLP_MIN=1.E20
      DO J=JM1-20,JM1+20
      DO I=IM1-10,IM1+10
        IF(SLP_MIN.GT.SLP1(I,J))THEN
          SLP_MIN=SLP1(I,J)
        END IF
      END DO
      END DO

      DP_CT=min(0.,SLP_MIN-SLP_AVE)

! compute 10m wind

      IUNIT=40+ITIM

      READ(IUNIT) JX,JY

      ALLOCATE ( A101(JX,JY),B101(JX,JY),C101(JX,JY) )

      READ(IUNIT) !LON1,LAT1,LON2,LAT2,CENTRAL_LON,CENTRAL_LAT
      READ(IUNIT) !PM1
      READ(IUNIT) !T1
      READ(IUNIT) !Q1
      READ(IUNIT) !U1
      READ(IUNIT) !V1
      READ(IUNIT) !DZDT           ! new
      READ(IUNIT) !Z1
      READ(IUNIT) !HLON,HLAT,VLON,VLAT
      READ(IUNIT) !P1
      READ(IUNIT) !PD           ! surface pressure
      READ(IUNIT) !ETA1
      READ(IUNIT) !ETA2
      READ(IUNIT) A101
      READ(IUNIT) B101
      READ(IUNIT) C101

      CLOSE(IUNIT)

      PRINT*,'JX,JY,NX,NY=',JX,JY,NX,NY


      JX1=JX-1

       DO J=1,NY
       DO I=1,NX
         USC(I,J)=U1(I,J,1)
         VSC(I,J)=V1(I,J,1)
       END DO
       END DO

! finsih compute 10m wind

!      WRITE(62)((SLP1(I,J),I=1,NX),J=1,NY,2)
!      DO K=1,NZ+1
!        WRITE(62)((Z1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ+1
!        WRITE(62)((P1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(62)((T1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(62)((Q1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(62)((U1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(62)((V1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!
      WBD=LON1
      SBD=LAT1

      write(*,*)'DLMD,DPHD,PT,PDTOP=',DLMD,DPHD,PT,PDTOP
      write(*,*)'WBD,SBD,CENTRAL_LON,CENTRAL_LAT=',    &
                 WBD,SBD,CENTRAL_LON,CENTRAL_LAT
      do k=1,nz1
        write(*,*)'K,ETA1,ETA2=',K,ETA1(k),ETA2(k)
      end do

       print*,'CLON,CLAT=',GLON(1+(NX-1)/2,1+(NY-1)/2),   &
                           GLAT(1+(NX-1)/2,1+(NY-1)/2)
       print*,'SLON,SLAT=',GLON(1,1),           &
                           GLAT(1,1)


! LON & LAT at U,V

!       CALL EARTH_LATLON ( HLAT,HLON,VLAT,VLON,        &  !Earth lat,lon at H and V points
!                           DLMD,DPHD,WBD,SBD,          &  !input res,west & south boundaries,
!                           CENTRAL_LAT,CENTRAL_LON,    &  ! central lat,lon, all in degrees
!                           1,NX1,1,NY1,1,1,            &
!                           1,NX ,1,NY ,1,1,            &
!                           1,NX ,1,NY ,1,1         )

       print*,'HLAT,HLON,VLAT,VLON=',                  &
               HLAT(1,1),HLON(1,1),VLAT(1,1),VLON(1,1)


!      write(70,*)
!      write(70,33)((HLON(I,J),I=1,NX,10),J=1,NY,20)
!      write(70,*)
!      write(70,33)((HLAT(I,J),I=1,NX,10),J=1,NY,20)
!      write(70,*)
 33   format(15F8.1)
! 34   format(10F12.1)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!! READ TC vital

      rewind 11

      read(11,11)id_storm,ICLAT,SN,ICLON,EW,Ipsfc,Ipcls,           &
                 Irmax,ivobs,Ir_vobs,(Ir_v4(I),I=1,4),DEPTH
 11   format(5x,I2,26x,I3,A1,I5,A1,9x,I4,1x,I4,1x,I4,I3,I4,4I5,1x,A1)

      rewind 11

      CLAT_NHC=ICLAT*0.1
      CLON_NHC=ICLON*0.1
      vobs=ivobs*1.0       ! m/s
      vobs_o=vobs
      VRmax=Ir_vobs*1.      ! in km

      if(VRmax.lt.19.)VRmax=19.

!      if(id_storm.lt.50.and.Ipsfc.gt.1005)Ipsfc=1005

      psfc_obs=Ipsfc*100.
      psfc_cls=Ipcls*100.

      PRMAX=Irmax*1.
      Rctp=Irmax*1.       ! in km

      cost=cos(CLAT_NHC*rad)

      dp_obs=psfc_cls-psfc_obs

      print*,'Ir_vobs,VRmax=',Ir_vobs,VRmax
      print*,'10m vobs=',vobs,CLON_NHC,CLAT_NHC

      vobs=vobs+0.1

      print*,'VRmax=',VRmax

      z0=(0.085*vobs-0.58)*1.E-3
      delt_z1=0.5*(Z1(NX/2,NY/2,2)-Z1(NX/2,NY/2,1))

! 7.2/1.944=3.7037

       vobs_kt=1.944*vobs
       IF(vobs_kt.gt.60.)then
         vobs=(3.7037+vobs)/1.12*log(delt_z1/z0)/log(10./z0)
       else
         vobs=vobs*log(delt_z1/z0)/log(10./z0)
       end if


      print*,'level 1 vobs,z0=',vobs,z0,delt_z1


      IF(SN.eq.'S')CLAT_NHC=-CLAT_NHC
      IF(EW.eq.'W')CLON_NHC=-CLON_NHC
!wpac      if(I360.eq.360) then
!wpac        IF(CLON_NHC.gt.0.)CLON_NHC=CLON_NHC-360.
!wpac      endif

      PW=1.
      IF((DEPTH.eq.'S').or.(id_storm.ge.90))THEN
        DO k=1,kmx
          PW(k)=PW_S(k)
        END DO
      ELSE IF(DEPTH.eq.'M')THEN
        DO k=1,kmx
          PW(k)=PW_M(k)
        END DO
      ELSE
        PW=1.
      END IF

      do k=1,kmx
        print*,'K,PW=',K,PW(K)
      end do


       distm=1.E20
       do j=1,ny
       do i=1,nx
         distt=((HLON(i,j)-CLON_NHC)*cost)**2+(HLAT(i,j)-CLAT_NHC)**2
         if(distm.gt.distt)then
           distm=distt
           ictr=i
           jctr=j
         end if
       end do
       end do
       imn1=ictr-2
       imx1=ictr+2
       jmn1=jctr-2
       jmx1=jctr+2

       do k=1,nz
	 tek(k)=T1(ictr,jctr,k)
       end do


       vt_c=0.
       vt_n=0.
       do j=jctr-10,jctr+10
       do i=ictr-10,ictr+10
         vt_c=vt_c+sqrt(usc(ictr,jctr)**2+vsc(ictr,jctr)**2)
         vt_n=vt_n+1.
       end do
       end do
       vt_c=vt_c/vt_n

       vd_c=vobs-vt_c

       pt_c=0.
       sum1=0.
       do j=jmn1,jmx1
       do i=imn1,imx1
!         dist=(((HLON(i,j)-CLON_NHC)*cost)**2+            &
!              (HLAT(i,j)-CLAT_NHC)**2)
         dist1=1.
         sum1=sum1+dist1
         pt_c=pt_c+slp1(i,j)*dist1
       end do
       end do

       psfc_env=pt_c/(sum1+1.e-20)

       psfc_obs1=min(-0.01,psfc_obs-psfc_env)

!       something wrong with the data

       print*,'psfc_obs,psfc_env=',psfc_obs,psfc_env

      REWIND(85)
      READ(85)RMN

! READ Hurricane Pert.

      ALLOCATE ( PCST(KMX),HP(NX,NY,KMX),HV(NX,NY,KMX) )

      ALLOCATE ( SLPE(NX,NY),SLP_1(NX,NY),TENV(NX,NY,KMX) )
      ALLOCATE ( T_1(NX,NY,KMX),Q_1(NX,NY,KMX) )
      ALLOCATE ( U_1(NX,NY,KMX),V_1(NX,NY,KMX) )

      ALLOCATE ( U_S(NX,NY),V_S(NX,NY) )
      ALLOCATE ( U_A(NX,NY),V_A(NX,NY) )

      ALLOCATE ( USC_1(NX,NY),VSC_1(NX,NY) )      ! hurricane component wind at z=0
      ALLOCATE ( USC1(NX,NY),VSC1(NX,NY) )        ! Hurricane wind at new grids
      ALLOCATE ( SLPV(NX,NY) )

      ALLOCATE ( T21(NX,NY,KMX,4),Q21(NX,NY,KMX,4) )
      ALLOCATE ( U21(NX,NY,KMX,4),V21(NX,NY,KMX,4) )
      ALLOCATE ( SLP21(NX,NY,4) )
      ALLOCATE ( PMV1(NX,NY,NZ),PMV2(NX,NY,NZ) )

      ALLOCATE ( T_4(NX,NY,KMX),Q_4(NX,NY,KMX) )

      ALLOCATE ( WRK1(KMX),WRK2(KMX),WRK3(KMX),WRK4(KMX) )


      SLP_1=0.
      T_1=0.
      Q_1=0.
      U_1=0.
      V_1=0.

      ALLOCATE ( U_2SB(IR1,KMX),T_2SB(IR1,KMX),SLP_2SB(IR1) )
      ALLOCATE ( Q_2SB(IR1,KMX),temp_e(KMX),RADIUS2(IR1) )
!
! READ the stored symmetric storm. Chanh added a modification
! here for Southern Hemsiphere support.
!
      CALL axisym_xy_new(NX,NY,NZ,KMX,HLON,HLAT,VLON,VLAT,     &
                 CLON_NHC,CLAT_NHC,SLP_1,T_1,Q_1,U_1,V_1,      &
                 TH1,RP,SLPE,TENV,PCST,HP,HV,ZMAX,vd_c,        &
		 dp_obs,vrmax,PRMAX,RMN,                       &
                 U_2SB,T_2SB,SLP_2SB,Q_2SB,temp_e,DEPTH,SN)

      do m=1,IR1
	RADIUS2(m)=RP(m)/arad
      end do

      print*,'RADIUS2(m)=',RADIUS2(1),RADIUS2(2)

      ALLOCATE ( PCST1(NX,NY,KMX),PCST2(KMX) )

      KST=1

      CLON_NEW=CLON_NHC
      CLAT_NEW=CLAT_NHC

      print*,'zmax=',zmax

      USC_1=0.
      VSC_1=0.

      DO J=1,NY
      DO I=1,NX
        USC_1(I,J)=U_1(I,J,1)
        VSC_1(I,J)=V_1(I,J,1)
      END DO
      END DO

!!!



! ENV. wind

       USCM=USC
       VSCM=VSC


! hurricane component (approximate for pert only)

       USC1=USC_1
       VSC1=VSC_1

       d_max=3.5
       IF(vobs.gt.30..and.CLAT_NHC.gt.30.)d_max=4.5

        d_max=min(d_max,0.8*RMN)

        vmax1=0.
        DO J=1,NY
        DO I=1,NX
          vmax2=sqrt(USC1(I,J)**2+VSC1(I,J)**2)*C101(I,J)
          dist(i,j)=sqrt(((VLON(i,j)-CLON_NHC)*cost)**2+       &
                          (VLAT(i,j)-CLAT_NHC)**2)
          if(vmax2.gt.vmax1.and.dist(i,j).lt.d_max)then
            vmax1=vmax2
            imax1=I
            jmax1=j
          end if
        END DO
        END DO

        vmax_s=vmax1

!        crtn=vobs_o/vmax_s

        crtn=1.0

        vmax1=0.
        DO J=1,NY
        DO I=1,NX
          vmax2=sqrt((USC1(I,J)*crtn+USCM(I,J))**2+                &
                (VSC1(I,J)*crtn+VSCM(I,J))**2)*C101(I,J)
          if(vmax2.gt.vmax1.and.dist(i,j).lt.d_max)then
            vmax1=vmax2
            imax1=I
            jmax1=j
          end if
        END DO
        END DO

        vobs=vobs_o/(C101(imax1,jmax1)+1.E-10)

        print*,'I,J,vmax,vobs=',imax1,jmax1,sqrt(vmax1),vobs

        RMX_d=1.2*sqrt((VLON(imax1,jmax1)-CLON_NHC)**2+           &
                   (VLAT(imax1,jmax1)-CLAT_NHC)**2)

        IF(RMX_d.gt.3.5)RMX_d=3.5
        IF(RMX_d.lt.2.0)RMX_d=2.0

!!!!!!!!!!!!!!!!!!!!

       iter=0
       beta=1.0

 876   CONTINUE

       VMAX=0.
       DO J=1,NY
       DO I=1,NX
!        i=imax1
!        j=jmax1
         UUT=beta*USC1(I,J)+USCM(I,J)
         VVT=beta*VSC1(I,J)+VSCM(I,J)
         FF=sqrt(UUT*UUT+VVT*VVT)*C101(I,J)
         R_DIST=sqrt((VLON(I,J)-CLON_NHC)**2+           &
                     (VLAT(I,J)-CLAT_NHC)**2)
         IF(VMAX.LT.FF.and.R_DIST.lt.RMX_d)THEN
           VMAX=FF
           IMV=I
           JMV=J
         END IF
       END DO
       END DO

       vobs=vobs_o/(C101(IMV,JMV)+1.E-10)

       PRINT*,'I,J,USC1,VSC1,USCM,VSCM=',USC1(IMV,JMV),    &
               VSC1(IMV,JMV),USCM(IMV,JMV),VSCM(IMV,JMV)
       PRINT*,'I,J,VMAX=',IMV,JMV,SQRT(VMAX)

       UU11=beta*USC1(IMV,JMV)
       VV11=beta*VSC1(IMV,JMV)
       UUM1=USCM(IMV,JMV)
       VVM1=VSCM(IMV,JMV)
       QQ=sqrt((uu11**2+vv11**2)*vobs**2-(vv11*uum1-uu11*vvm1)**2)

       uv22=sqrt(uu11**2+vv11**2)

       print*,'max hurricane pert=',uv22

!       if(uv22.lt.5.0)then
!         beta1=0.0
!       else
         beta1=(-(uum1*uu11+vvm1*vv11)+QQ)/(uu11**2+vv11**2+1.E-20)
!       end if

       print*,'UU11,VV11,UUM1,VVM1,QQ,beta1=',UU11,VV11,UUM1,VVM1,QQ,beta1

       beta=beta*beta1
       iter=iter+1

       print*,'iter,beta=',iter,beta

!       IF(iter.lt.2)go to 876

!       if(beta.gt.1.25)beta=1.25           ! test

!       if(beta.gt.1.25) beta=max(1.25,sqrt(beta))

        v_min=min(10.,max(8.,0.6*vobs))

        if(abs(CLAT_NHC).lt.15.)then
           v_min=min(10.,max(10.,0.6*vobs))
        end if

!        v_min=8.

        beta=max(beta,v_min/vmax_s)   ! beta*vmax_s >=8 m/s

        print*,'v_min,beta=',v_min,beta

!!!       beta=0.8

!         beta=1.

!  set storm pert T and Q =0

!       T_1=0.
!       Q_1=0.

! now modify the horricane component (by beta)

      T_4=T_1
      Q_4=Q_1

       print*,'CLON_NEW,CLAT_NEW=',CLON_NEW,CLAT_NEW

!       read storm data and compute center surface pressure PS_C1


      ALLOCATE ( T_X(NX,NY,KMX),Q_X(NX,NY,KMX),SLP_X(NX,NY) )


       PS_C1=min(-dp_obs,psfc_obs1)

       ics=1
       fact=1.0
!
!  Chanh added a modification
!  here for Southern Hemsiphere support.
!
       CALL CORT_MAT_2(IR1,NX,NY,NZ,KMX,U_2SB,           &
	       T_2SB,SLP_2SB,Q_2SB,RADIUS2,temp_e,TEK,   &
	       T_X,Q_X,SLP_X,HLON,HLAT,VLON,VLAT,        &
	       CLON_NEW,CLAT_NEW,PS_C1,                  &
	       beta,fact,ics,SN)


       do j=1,NY
       do i=1,NX
!	 SLP_1(i,j)=SLP_1(i,j)+SLP_X(i,j)
	 SLP_1(i,j)=SLP_X(i,j)
         do k=1,KMX
	   TEK1=TENV(I,J,K)+T_1(I,J,K)
  	   U_1(i,j,k)=U_1(i,j,k)*beta*PW(k)
	   V_1(i,j,k)=V_1(i,j,k)*beta*PW(k)
!	   T_1(i,j,k)=(T_1(i,j,k)+T_X(i,j,k))*PW(k)
!	   Q_1(i,j,k)=(Q_1(i,j,k)+Q_X(i,j,k))*PW(k)
	   T_1(i,j,k)=T_X(i,j,k)*PW(k)
!	   Q_1(i,j,k)=Q_X(i,j,k)*PW(k)
	   TEK2=TENV(I,J,K)+T_1(I,J,K)
	   ESRR=exp(4302.645*(TEK2-TEK1)/((TEK2-29.66)*(TEK1-29.66)))
	   Q_1(I,J,K)=ESRR*Q_1(I,J,K)
         end do
       end do
       end do


      ps_min=1.E20
      do j=1,NY
      do i=1,NX
        if(ps_min.gt.SLP_1(i,j))then
          ps_min=SLP_1(i,j)
          i_psm=i
          j_psm=j
        end if
      end do
      end do
      print*,'storm center 4 =',HLON(i_psm,j_psm),HLAT(i_psm,j_psm),ps_min

!      CALL FIND_NEWCT1(NX,NY,U_1(1,1,10),V_1(1,1,10),HLON,HLAT,CLON_NEW,CLAT_NEW)


!      WRITE(25)((SLP_1(I,J),I=1,NX),J=1,NY,2)
!      DO K=1,KMX
!        WRITE(25)((T_1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,KMX
!        WRITE(25)((U_1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,KMX
!        WRITE(25)((V_1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,KMX
!        WRITE(25)((Q_1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO


!        T_1=T_4
!        Q_1=Q_4

!       if(zmax.gt.250.)then
!         T_4=0.
!         Q_4=0.
!       end if


       print*,'complete CORT'


!??????????????????

       DO J=1,NY
       DO I=1,NX
         SLP1(I,J) = SLP1(I,J)+SLP_1(I,J)
         TENV1     = TS1(I,J)
         TS1(I,J)  = TENV1+T_1(I,J,1)
         T_OLD     = T4(I,J,1)
         Q_OLD     = Q4(I,J,1)
         ESRR      = exp(4302.645*(TS1(I,J)-T_OLD)/     &
                   ((TS1(I,J)-29.66)*(T_OLD-29.66)))               ! 4302.645=17.67*243.5
         QS1(I,J)  = Q_OLD + (ESRR-1.)*Q_OLD                       ! Assuming RH=CONST. before & after
       ENDDO
       ENDDO

!       WRITE(25)((SLP1(I,J),I=1,NX),J=1,NY,2)
!       WRITE(25)((HLON(I,J),I=1,NX),J=1,NY,2)
!       WRITE(25)((HLAT(I,J),I=1,NX),J=1,NY,2)

!

! based on Ts, Zs, SLP1 ==> PS1  ==> P1

       DO J=1,NY
       DO I=1,NX
         ZSFC = ZS1(I,J)
         TSFC = TS1(I,J)*(1.+D608*QS1(I,J))
         A = (GAMMA * ZSFC) / TSFC
         P1(I,J,1) = SLP1(I,J)/(1+A)**COEF2
         PD1(I,J)=P1(I,J,1)
       ENDDO
       ENDDO

       allocate (work_1(nz),work_2(nz+1))
       DO J=1,NY
       DO I=1,NX
          call get_eta_level(nz,PD1(I,J),work_1,work_2,eta1,eta2,1.0)
          do k=1,nz
             n=nz-k+1
             PMID1(I,J,K)=work_1(n)
          end do
          do k=1,nz+1
             n=nz-k+2
            P1(I,J,K)=work_2(n)
          end do
      ENDDO
      ENDDO
      deallocate (work_1,work_2)

! PD(I,J)=P1(I,J,1)-PDTOP-PT=PSFC(I,J)-PDTOP-PT
!       DO K=1,NZ+1
!       DO J=1,NY
!       DO I=1,NX
!         P1(I,J,K)=PT+PDTOP*ETA1(K)+PD1(I,J)*ETA2(K)     ! PD(I,J) changed
!       ENDDO
!       ENDDO
!       ENDDO
!       DO K=1,NZ
!       DO J=1,NY
!       DO I=1,NX
!         PMID1(I,J,K)=EXP((ALOG(P1(I,J,K))+ALOG(P1(I,J,K+1)))*0.5)
!       ENDDO
!       ENDDO
!       ENDDO

! add hurricane components


      DO J=1,NY
      DO I=1,NX
        DO N=1,KMX
!          PCST1(I,J,N)=HP(I,J,N)
!          PCST1(I,J,N)=PMID1(I,J,1)*PCST(N)/PCST(1)
          PCST1(I,J,N)=PCST(N)*SLP1_MEAN/PCST(1)
        END DO
      END DO
      END DO

       DO J=1,NY
       DO I=1,NX
         DO K=1,KMX
           WRK1(K) = T_1(I,J,K)
           WRK2(K) = Q_1(I,J,K)
         END DO
         DO N=1,NZ
           TENV1 = T1(I,J,N)
           QENV1 = Q1(I,J,N)
           IF(PMID1(I,J,N).GE.PCST1(I,J,1))THEN            ! Below PCST(1)
             T1(I,J,N)=TENV1+WRK1(1)
             Q1(I,J,N)=QENV1+WRK2(1)
           ELSE IF(PMID1(I,J,N).LE.PCST1(I,J,KMX))THEN
             T1(I,J,N)=TENV1+WRK1(KMX)
             Q1(I,J,N)=QENV1+WRK2(KMX)
           ELSE
             DO K=1,KMX-1
               IF(PMID1(I,J,N).LE.PCST1(I,J,K).and.PMID1(I,J,N).GT.PCST1(I,J,K+1))THEN
                  W1=ALOG(1.*PCST1(I,J,K+1))-ALOG(1.*PCST1(I,J,K))
                  W=(ALOG(1.*PMID1(I,J,N))-ALOG(1.*PCST1(I,J,K)))/W1
                  T1(I,J,N)=TENV1+WRK1(K)*(1.-W)+WRK1(K+1)*W
                  Q1(I,J,N)=QENV1+WRK2(K)*(1.-W)+WRK2(K+1)*W
                  GO TO 887
               END IF
             END DO
           END IF
 887       CONTINUE

           T_OLD     = T4(I,J,N)
           Q_OLD     = Q4(I,J,N)
           ESRR      = exp(4302.645*(T1(I,J,N)-T_OLD)/     &
                     ((T1(I,J,N)-29.66)*(T_OLD-29.66)))               ! 4302.645=17.67*243.5
           Q1_GFS = Q_OLD + (ESRR-1.)*Q_OLD                       ! Assuming RH=CONST. before & after
	   Q1(I,J,N)=0.2*Q1(I,J,N)+0.8*Q1_GFS
         END DO
       ENDDO
       ENDDO

! based on Ts, Zs, SLP1 Recompute ==> PS1  ==> P1

       DO J=1,NY
       DO I=1,NX
         ZSFC = ZS1(I,J)
         TS1(I,J) =T1(I,J,1)+GAMMA*(Z1(I,J,2)-Z1(I,J,1))*0.5
	 QS1(I,J) = Q1(I,J,1)
         TSFC = TS1(I,J)*(1.+D608*QS1(I,J))
         A = (GAMMA * ZSFC) / TSFC
         P1(I,J,1) = SLP1(I,J)/(1+A)**COEF2
         PD1(I,J)=P1(I,J,1)
       ENDDO
       ENDDO

       allocate (work_1(nz),work_2(nz+1))
       DO J=1,NY
       DO I=1,NX
          call get_eta_level(nz,PD1(I,J),work_1,work_2,eta1,eta2,1.0)
          do k=1,nz
             n=nz-k+1
             PMID1(I,J,K)=work_1(n)
          end do
          do k=1,nz+1
             n=nz-k+2
            P1(I,J,K)=work_2(n)
          end do
      ENDDO
      ENDDO
      deallocate (work_1,work_2)

! PD(I,J)=P1(I,J,1)-PDTOP-PT=PSFC(I,J)-PDTOP-PT
!       DO K=1,NZ+1
!       DO J=1,NY
!       DO I=1,NX
!         P1(I,J,K)=PT+PDTOP*ETA1(K)+PD1(I,J)*ETA2(K)     ! PD(I,J) changed
!       ENDDO
!       ENDDO
!       ENDDO
!       DO K=1,NZ
!       DO J=1,NY
!       DO I=1,NX
!         PMID1(I,J,K)=EXP((ALOG(P1(I,J,K))+ALOG(P1(I,J,K+1)))*0.5)
!       ENDDO
!       ENDDO
!       ENDDO

! add hurricane components

! Compute Geopotentital height, INTEGRATE HEIGHT HYDROSTATICLY

      do j = 1,ny
      do i = 1,nx
        Z1(I,J,1)=ZS1(I,J)
        DO L=2,nz+1
          Z1(I,J,L)=Z1(I,J,L-1)+T1(I,J,L-1)*          &
              (Q1(I,J,L-1)*0.608+1.0)*287.04*         &
              (ALOG(P1(I,J,L-1))-ALOG(P1(I,J,L)))/G
        ENDDO
      ENDDO
      END DO

       DO K=1,NZ
       DO J=1,NY
       DO I=1,NX
         ZMID1(I,J,K)=0.5*(Z1(I,J,K)+Z1(I,J,K+1))
       ENDDO
       ENDDO
       ENDDO

! interpolate vertically to P level in new coordinate  (V Points)

       PMV1=PMID1

!       DO J=2,NY-1
!         IF(MOD(J,2).NE.0.)THEN
!           DO K=1,NZ
!           DO I=2,NX-1
!             PMV1(I,J,K)=0.25*(PMID1(I,J,K)+PMID1(I+1,J,K)+            &
!                         PMID1(I,J-1,K)+PMID1(I,J+1,K))
!           END DO
!           END DO
!         ELSE
!           DO K=1,NZ
!           DO I=2,NX-1
!             PMV1(I,J,K)=0.25*(PMID1(I-1,J,K)+PMID1(I,J,K)+            &
!                         PMID1(I,J-1,K)+PMID1(I,J+1,K))
!           END DO
!           END DO
!         END IF
!       END DO



      PRINT*,'test01'

       DO J=1,NY
       DO I=1,NX
         DO K=1,KMX
            WRK1(K) = U_1(I,J,K)
            WRK2(K) = V_1(I,J,K)
         END DO

         DO N=1,KMX
!           PCST2(N)=HV(I,J,N)
           PCST2(N)=PMV1(I,J,1)*PCST(N)/PCST(1)
         END DO

         DO N=1,NZ
           IF(PMV1(I,J,N).GE.PCST2(1))THEN            ! Below PCST(1)
             U1(I,J,N)=U1(I,J,N)+WRK1(1)
             V1(I,J,N)=V1(I,J,N)+WRK2(1)
           ELSE IF(PMV1(I,J,N).LE.PCST2(KMX))THEN
             U1(I,J,N)=U1(I,J,N)+WRK1(KMX)
             V1(I,J,N)=V1(I,J,N)+WRK2(KMX)
           ELSE
             DO K=1,KMX-1
               IF(PMV1(I,J,N).LE.PCST2(K).and.PMV1(I,J,N).GT.PCST2(K+1))THEN
                  W1=ALOG(1.*PCST2(K+1))-ALOG(1.*PCST2(K))
                  W=(ALOG(1.*PMV1(I,J,N))-ALOG(1.*PCST2(K)))/W1
                  U1(I,J,N)=U1(I,J,N)+WRK1(K)*(1.-W)+WRK1(K+1)*W
                  V1(I,J,N)=V1(I,J,N)+WRK2(K)*(1.-W)+WRK2(K+1)*W
                  GO TO 888
               END IF
             END DO
           END IF
 888       CONTINUE
         END DO
       ENDDO
       ENDDO


! based on Ts, Zs, SLP1 ==> PS1  ==> P1
      PRINT*,'test02'

       DO J=1,NY
       DO I=1,NX
         ZSFC = ZS1(I,J)
         TSFC = TS1(I,J)*(1.+D608*QS1(I,J))
         A = (GAMMA * ZSFC) / TSFC
         P1(I,J,1) = SLP1(I,J)/(1+A)**COEF2
         PD1(I,J)=P1(I,J,1)
       ENDDO
       ENDDO

       allocate (work_1(nz),work_2(nz+1))
       DO J=1,NY
       DO I=1,NX
          call get_eta_level(nz,PD1(I,J),work_1,work_2,eta1,eta2,1.0)
          do k=1,nz
             n=nz-k+1
             PMID1(I,J,K)=work_1(n)
          end do
          do k=1,nz+1
             n=nz-k+2
            P1(I,J,K)=work_2(n)
          end do
      ENDDO
      ENDDO
      deallocate (work_1,work_2)

! PD(I,J)=P1(I,J,1)-PDTOP-PT=PSFC(I,J)-PDTOP-PT
!       DO K=1,NZ+1
!       DO J=1,NY
!       DO I=1,NX
!         P1(I,J,K)=PT+PDTOP*ETA1(K)+PD1(I,J)*ETA2(K)     ! PD(I,J) changed
!       ENDDO
!       ENDDO
!       ENDDO

!      WRITE(64)((SLP1(I,J),I=1,NX),J=1,NY,2)
!      DO K=1,NZ+1
!        WRITE(64)((Z1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ+1
!        WRITE(64)((P1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(64)((T1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(64)((Q1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(64)((U1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(64)((V1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      WRITE(64)((USCM(I,J),I=1,NX),J=1,NY,2)
!      WRITE(64)((VSCM(I,J),I=1,NX),J=1,NY,2)

      IUNIT=50+ITIM

      WRITE(IUNIT) NX,NY,NZ,I360
      WRITE(IUNIT) LON1,LAT1,LON2,LAT2,CENTRAL_LON,CENTRAL_LAT
      WRITE(IUNIT) PMID1
      WRITE(IUNIT) T1
      WRITE(IUNIT) Q1
      WRITE(IUNIT) U1
      WRITE(IUNIT) V1
      WRITE(IUNIT) DZDT
      WRITE(IUNIT) Z1
!      WRITE(IUNIT) GLON,GLAT
      WRITE(IUNIT) HLON,HLAT,VLON,VLAT
      WRITE(IUNIT) P1
      WRITE(IUNIT) PD1
      WRITE(IUNIT) ETA1
      WRITE(IUNIT) ETA2

      CLOSE(IUNIT)

! test

!      CALL FIND_NEWCT1(NX,NY,U_1(1,1,10),V_1(1,1,10),HLON,HLAT,CLON_NEW,CLAT_NEW)


!      print*,'storm center 2 =',CLON_NEW,CLAT_NEW

!      CALL FIND_NEWCT1(NX,NY,U_1(1,1,10),V_1(1,1,10),GLON,GLAT,CLON_NEW,CLAT_NEW)


!      print*,'storm center 3 =',CLON_NEW,CLAT_NEW



       END


!=============================================================================
subroutine dbend(nit,x,y)
!=============================================================================
! Evaluate a smooth monotonic increasing blending function y from 0 to 1
! for x in the interval [0,1] having continuity in at least the first nit
! derivatives at the ends of this interval. (nit .ge. 0).
!=============================================================================
implicit none
integer,intent(IN ):: nit
real(8),intent(IN ):: x
real(8),intent(OUT):: y
!-----------------------------------------------------------------------------
integer            :: it
!=============================================================================
y=2*x-1; do it=1,nit; y=y*(3-y*y)/2; enddo; y=(y+1)/2
end subroutine dbend

      SUBROUTINE FIND_NEWCT1(IX,JX,UD,VD,GLON2,GLAT2,    &
                             CLON_NEW1,CLAT_NEW1)

!      PARAMETER (IR=100,IT=24,IX=254,JX=254)
      PARAMETER (IR=30,IT=24)
      PARAMETER (ID=61,JD=61,DTX=0.05,DTY=0.05)    ! Search x-Domain (ID-1)*DTX
      REAL (4) UD(IX,JX),VD(IX,JX),GLON2(IX,JX),GLAT2(IX,JX)
!      DIMENSION RWM(IR+1),TWM(IR+1)
      DIMENSION TNMX(ID,JD),RX(ID,JD),WTM(IR)
      REAL (8) CLON_NEW1,CLAT_NEW1

      PI=ASIN(1.)*2.
      RAD=PI/180.

      ddr=0.05

      pi180=RAD
      cost=cos(clat_new*pi180)

      ix2=ix/2
      jx2=jx/2
      DDS=(((GLON2(ix2+1,jx2)-GLON2(ix2,jx2))*cost)**2+     &
          (GLAT2(ix2,jx2+1)-GLAT2(ix2,jx2))**2)*1.5


       print*,'ix,jx,ix2,jx2=',ix,jx,ix2,jx2
       print*,'CLON_NEW,CLAT_NEW=',CLON_NEW1,CLAT_NEW1
       print*,'GLON2,GLAT2=',GLON2(1,1),GLAT2(1,1)


      XLAT = CLAT_NEW1-(JD-1)*DTY/2.
      XLON = CLON_NEW1-(ID-1)*DTX/2.

!c      print *,'STARTING LAT, LON AT FIND NEW CENTER ',XLAT,XLON

      DO J=1,JD
      DO I=1,ID
      TNMX(I,J) = 0.
      RX(i,j)=0.
      BLON = XLON + (I-1)*DTX
      BLAT = XLAT + (J-1)*DTY

!.. CALCULATE TANGENTIAL WIND EVERY 0.2 deg INTERVAL
!..  10*10 deg AROUND 1ST GUESS VORTEX CENTER

      DO 10 JL=1,IR
      WTS= 0.
      DO 20 IL=1,IT
      DR = JL*ddr
!      DR = JL
      DD = (IL-1)*15*RAD
      DLON = DR*COS(DD)
      DLAT = DR*SIN(DD)
      TLON = BLON + DLON
      TLAT = BLAT + DLAT

!C.. INTERPOLATION U, V AT TLON,TLAT AND CLACULATE TANGENTIAL WIND

      u1=0.
      v1=0.
      sum1=0.
      DO j1=jx2-40,jx2+40
      DO i1=ix2-40,ix2+40
        dist=(((GLON2(i1,j1)-TLON)*cost)**2+(GLAT2(i1,j1)-TLAT)**2)
        if(dist.lt.DDS)THEN
          dist1=1./dist
          sum1=sum1+dist1
          u1=u1+UD(i1,j1)*dist1
          v1=v1+VD(i1,j1)*dist1
        end if
      end do
      end do

      UT=u1/sum1
      VT=v1/sum1

!C.. TANGENTIAL WIND
      WT = -SIN(DD)*UT + COS(DD)*VT
      WTS = WTS+WT
20    CONTINUE
      WTM(JL) = WTS/24.
10    CONTINUE

!C Southern Hemisphere
      IF(CLAT_NEW.LT.0)THEN
        DO JL=1,IR
          WTM(JL)=-WTM(JL)
        END DO
      END IF
!C EnD SH

!      print*,'test1'

      TX = -10000000.
      DO KL = 1,IR
      IF(WTM(KL).GE.TX) THEN
      TX = WTM(KL)
      RRX = KL*ddr
      ENDIF
      ENDDO
!        DO KL=1,IR
!          TWM(KL)=WTM(KL)
!          RWM(KL)=KL*ddr
!        END DO
!        TWM(IR+1)=TX
!        RWM(IR+1)=RRX

      TNMX(I,J) = TX
      RX(I,J)=RRX
      ENDDO
      ENDDO
!C.. FIND NEW CENTER
      TTX = -1000000.
      DO I=1,ID
      DO J=1,JD
      IF(TNMX(I,J).GE.TTX) THEN
      TTX = TNMX(I,J)
      NIC = I
      NJC = J
      ENDIF
      ENDDO
      ENDDO

! QLIU test
!      print*,XLAT+30*DTY,XLON+30*DTX,TNMX(30,30)
      print*,'max WTM=',TTX

      CLAT_NEW1 = XLAT + (NJC-1)*DTY
      CLON_NEW1 = XLON + (NIC-1)*DTX

!      print *,'NEW CENTER,  I, J IS   ',NIC,NJC
      print *,'NEW CENTER, LAT,LON IS ',CLAT_NEW1,CLON_NEW1
!      print *,'MAX TAN. WIND AT NEW CENTER IS ',TTX

      RETURN
      END

