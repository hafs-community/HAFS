!
! ABSTRACT: Adding a small fraction of the composite storm to make
!           the storm intensity match the observation
!
! ORIGINAL AUTHOR: QINGFU LIU, NCEP/EMC, 2007
! REVISED  AUTHOR: QINGFU LIU, 2013
!                : Use observed ROCI and Rmax to correct the composite storm
!                : (old version uses the averaged values from obs and model)
!
!     DECLARE VARIABLES
!
      INTEGER I,J,K,NX,NY,NZ,ICH
!
      PARAMETER (NST=5,IR=200)
!      PARAMETER (NX=158,NY=329,NZ=42,NST=5)
      PARAMETER (GAMMA=6.5E-3,G=9.8,Rd=287.05,D608=0.608)
      PARAMETER (Cp=1004.,IR1=IR+1)

!      PARAMETER (KMX=2*NZ+1)
!
      PARAMETER (NZ2=121)

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

      REAL(4), ALLOCATABLE :: TEK(:),TEK42(:)
      REAL(4), ALLOCATABLE :: PCK(:)

! variables for hurricane component

      REAL(4), ALLOCATABLE :: SLPE(:,:),SLP_1(:,:),TENV(:,:,:)
      REAL(4), ALLOCATABLE :: T_1(:,:,:),Q_1(:,:,:)
      REAL(4), ALLOCATABLE :: U_1(:,:,:),V_1(:,:,:)

      REAL(4), ALLOCATABLE :: T_X(:,:,:),Q_X(:,:,:),SLP_X(:,:)

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
      REAL(4), ALLOCATABLE :: USC11(:,:),VSC11(:,:)        ! Hurricane wind at new grids
      REAL(4), ALLOCATABLE :: USC2(:,:),VSC2(:,:)        ! Hurricane wind at new grids
      REAL(4), ALLOCATABLE :: SLPV(:,:)

      REAL(4), ALLOCATABLE :: HLON1(:,:),HLAT1(:,:)
      REAL(4), ALLOCATABLE :: VLON1(:,:),VLAT1(:,:)
      REAL(4), ALLOCATABLE :: T21(:,:,:,:),Q21(:,:,:,:)
      REAL(4), ALLOCATABLE :: U21(:,:,:,:),V21(:,:,:,:)
      REAL(4), ALLOCATABLE :: SLP21(:,:,:)
      REAL(4), ALLOCATABLE :: PMV1(:,:,:),PMV2(:,:,:)

      REAL(4), ALLOCATABLE :: A101(:,:),B101(:,:),C101(:,:)

      REAL(4), ALLOCATABLE :: T_4(:,:,:),Q_4(:,:,:)

      REAL(4), ALLOCATABLE :: U_2SB(:,:),T_2SB(:,:),SLP_2SB(:)
      REAL(4), ALLOCATABLE :: R_2SB(:,:),temp_e(:),RADIUS2(:)

      REAL(4), ALLOCATABLE :: U_2S(:,:),U_2S1(:,:),U_2S2(:)
      REAL(4), ALLOCATABLE :: T_2S(:,:),SLP_2S(:),Q_2S(:,:)
      REAL(4), ALLOCATABLE :: RKX1(:),RADIUS(:),CFT(:),V_2S(:,:)
      REAL(4), ALLOCATABLE :: strm1(:),strm2(:)

      REAL(8), ALLOCATABLE :: WRK1(:),WRK2(:),WRK3(:),WRK4(:)
      REAL(4), ALLOCATABLE :: work_1(:),work_2(:)

      REAL(4), ALLOCATABLE :: PCST(:),HP(:,:,:),HV(:,:,:)
      REAL(4), ALLOCATABLE :: P_S(:,:),P_A(:,:)
      REAL(4), ALLOCATABLE :: PCST1(:,:,:),PCST2(:),PCSK(:)

      REAL(4), ALLOCATABLE ::    HBWGT1(:,:,:),VBWGT1(:,:,:)
      integer(4), ALLOCATABLE :: IIH1(:,:),JJH1(:,:)
      integer(4), ALLOCATABLE :: IIV1(:,:),JJV1(:,:)

      REAL(4), ALLOCATABLE :: dist(:,:),PW1(:)

      integer(4) IH1(4),JH1(4),IV1(4),JV1(4)

      REAL(8) CLON_NEW,CLAT_NEW,CLON_NHC,CLAT_NHC
      REAL(4) CLON_NEW1,CLAT_NEW1

      REAL(4) th1(IR1),rp(IR1)   ! ,RMN

      REAL(4) zmax,PW(121)
      REAL(4) PW_S(121),PW_M(121)
!      REAL(4) PW_S(85),PW_M(85),PW_D(85)

      integer Ir_v4(4)
      CHARACTER SN*1,EW*1,DEPTH*1
!zhang:added basin domain shift option
      CHARACTER*2 :: basin

!      DATA PW_S/30*1.0,0.95,0.9,0.8,0.7,       &
!                    0.6,0.5,0.4,0.3,0.2,0.1,45*0./
      DATA PW_S/42*1.0,0.95,0.9,0.85,0.8,0.75,0.7,       &
                0.65,0.6,0.55,0.5,0.45,0.4,0.35,0.3,     &
                0.25,0.2,0.15,0.1,0.05,60*0./
!      DATA PW_S/30*1.0,0.95,0.9,0.8,0.7,          &
!                0.6,0.5,0.4,0.3,0.2,0.1,45*0./                        ! 850-700mb
!      DATA PW_M/38*1.0,0.95,0.9,0.8,0.7,          &
!  	        0.6,0.5,0.4,0.3,0.2,0.1,37*0./                    ! 850-400mb
      DATA PW_M/50*1.0,0.95,0.9,0.85,0.8,0.75,0.7,       &
                0.65,0.6,0.55,0.5,0.45,0.4,0.35,0.3,     &
                0.25,0.2,0.15,0.1,0.05,52*0./

      COEF1=Rd/Cp
      COEF3=Rd*GAMMA/G
      COEF2=1./COEF3

      GRD=G/Rd

      pi=4.*atan(1.)
      pi_deg=180./pi
      rad=1./pi_deg

      arad=6.371E6*rad
      DST1=6.371E6*rad

      READ(5,*)ITIM,basin,iflag_cold


! READ NEW GFS Env. DATA (New Domain)

      IUNIT=20+ITIM

      READ(IUNIT) NX,NY,NZ,I360

      print*,'NX,NY,NZ=',NX,NY,NZ,I360

      NX1=NX+1
      NY1=NY+1
      NZ1=NZ+1

      KMX=121
      KMX2=60

      ALLOCATE ( T1(NX,NY,NZ),Q1(NX,NY,NZ) )
      ALLOCATE ( U1(NX,NY,NZ),V1(NX,NY,NZ),DZDT(NX,NY,NZ) )
      ALLOCATE ( Z1(NX,NY,NZ+1),P1(NX,NY,NZ+1) )
      ALLOCATE ( GLON(NX,NY),GLAT(NX,NY) )
      ALLOCATE ( PD1(NX,NY),ETA1(NZ+1),ETA2(NZ+1) )
      ALLOCATE ( USC(NX,NY),VSC(NX,NY) )        ! Env. wind at new grids

      ALLOCATE ( HLON(NX,NY),HLAT(NX,NY) )
      ALLOCATE ( VLON(NX,NY),VLAT(NX,NY) )

      ALLOCATE ( PCK(NZ),TEK(NZ),TEK42(KMX2) )
      ALLOCATE ( dist(NX,NY) )
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

      ALLOCATE ( SLP1(NX,NY),RIJ(NX,NY) )
      ALLOCATE ( ZS1(NX,NY),TS1(NX,NY),QS1(NX,NY) )

      ALLOCATE ( USCM(NX,NY),VSCM(NX,NY) )
      ALLOCATE ( P_S(NX,NY),P_A(NX,NY) )

      ALLOCATE ( T_X(NX,NY,KMX),Q_X(NX,NY,KMX),SLP_X(NX,NY) )

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
!         PMID1(I,J,K)=EXP((ALOG(P1(I,J,K))+ALOG(P1(I,J,K+1)))*0.5)
!         ZMID1(I,J,K)=0.5*(Z1(I,J,K)+Z1(I,J,K+1))
        TV1=T1(I,J,K)*(1.+D608*Q1(I,J,K))
        ZMID1(I,J,K)=(Z1(I,J,K)+Z1(I,J,K+1))*0.5+             &
          0.5*TV1/GAMMA*(2.-(P1(I,J,K)/PMID1(I,J,K))**COEF3-  &
          (P1(I,J,K+1)/PMID1(I,J,K))**COEF3)
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
	SUM11=SUM11+1.
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

      SLP_AVE=SLP_AVE/SLP_SUM

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

       DO J=1,NY
       DO I=1,NX
         USC(I,J)=U1(I,J,1)
         VSC(I,J)=V1(I,J,1)
       END DO
       END DO

! finsih compute 10m wind


      WRITE(62)((SLP1(I,J),I=1,NX),J=1,NY,2)
      DO K=1,NZ+1
        WRITE(62)((Z1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ+1
        WRITE(62)((P1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(62)((T1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(62)((Q1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(62)((U1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(62)((V1(I,J,K),I=1,NX),J=1,NY,2)
      END DO

      WBD=-(NX-1)*DLMD
      SBD=-((NY-1)/2)*DPHD

      write(*,*)'DLMD,DPHD,PT,PDTOP=',DLMD,DPHD,PT,PDTOP
      write(*,*)'WBD,SBD,CENTRAL_LON,CENTRAL_LAT=',    &
                 WBD,SBD,CENTRAL_LON,CENTRAL_LAT
      do k=1,nz1
        write(*,*)'K,ETA1,ETA2=',K,ETA1(k),ETA2(k)
      end do

!       print*,'CLON,CLAT=',GLON(1+(NX-1)/2,1+(NY-1)/2),   &
!                           GLAT(1+(NX-1)/2,1+(NY-1)/2)
       print*,'CLON,CLAT=',GLON(1+(NX-1)/2,1+(NY-1)/2),   &
                           GLAT(1+(NX-1)/2,1+(NY-1)/2)
       print*,'SLON,SLAT=',GLON(1,1),           &
                           GLAT(1,1)


! LON & LAT at U,V

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

      print*,'vobs=',vobs

      OPEN(69,file='flag_file',form='formatted')
        READ(69,*)IFLAG
        READ(69,*)K850
        READ(69,*)TWMAX1,RWMAX1,fact_v,psfc_obs
      CLOSE(69)

      PRINT*,'IFLAG,K850=',IFLAG,K850
      PRINT*,'TWMAX1,RWMAX1=',TWMAX1,RWMAX1

      dp_obs=psfc_cls-psfc_obs

      print*,'Ir_vobs,VRmax=',Ir_vobs,VRmax

      IF(SN.eq.'S')CLAT_NHC=-CLAT_NHC
      IF(EW.eq.'W')CLON_NHC=-CLON_NHC
!wpac      if(I360.eq.360) then
!wpac        IF(CLON_NHC.gt.0.)CLON_NHC=CLON_NHC-360.
!wpac      endif

!      PW=1.
!      IF(DEPTH.eq.'S')THEN
!        DO k=1,kmx
!          PW(k)=PW_S(k)
!        END DO
!      ELSE IF(DEPTH.eq.'M')THEN
!        DO k=1,kmx
!          PW(k)=PW_M(k)
!        END DO
!      ELSE
!        PW=1.
!      END IF

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
         pck(K)=EXP((ALOG(P1(ictr,jctr,K))+ALOG(P1(ictr,jctr,K+1)))*0.5)
       end do

       delt_z1=0.
       pt_c=1.e20
       sum1=0.
       do j=jmn1,jmx1
       do i=imn1,imx1
!         dist=(((HLON(i,j)-CLON_NHC)*cost)**2+            &
!              (HLAT(i,j)-CLAT_NHC)**2)
         dist1=1.
         sum1=sum1+dist1
         if(pt_c.gt.slp1(i,j))pt_c=slp1(i,j)
         delt_z1=delt_z1+0.5*(Z1(i,j,2)-Z1(i,j,1))
       end do
       end do

       delt_z1=delt_z1/float((jmx1-jmn1+1)*(imx1-imn1+1))
       psfc_env=pt_c

       psfc_obs1=min(0.,psfc_obs-psfc_env)

!       something wrong with the data

       print*,'psfc_obs,psfc_env=',psfc_obs,psfc_env

! existing hurricane component (approximate only)

        vobs_kt=1.944*vobs
        vmax1=0.
        d_max=3.5
        IF(vobs.gt.30..and.CLAT_NHC.gt.30.)d_max=4.5

        DO J=1,NY
        DO I=1,NX
!          if(A101(I,J).LT.0.5)then                ! A101 = land sea mask, B101 = ZNT
!            z0=B101(I,J)
!            z0=max(z0,(0.085*vobs-0.58)*1.E-3)
!          else
!            z0=(0.085*vobs-0.58)*1.E-3
!          end if
!          IF(vobs_kt.gt.60.)then
!            ct_10m=1.12*log(10./z0)/log(delt_z1/z0)
!            u10=USC(I,J)*ct_10m-3.7037
!            v10=VSC(I,J)*ct_10m-3.7037
!          else
!            ct_10m=log(10./z0)/log(delt_z1/z0)
!            u10=USC(I,J)*ct_10m
!            v10=VSC(I,J)*ct_10m
!          end if
!          vmax2=u10**2+v10**2
          vmax2=sqrt(USC(I,J)**2+VSC(I,J)**2)*C101(I,J)
          dist(i,j)=sqrt(((VLON(i,j)-CLON_NHC)*cost)**2+            &
                 (VLAT(i,j)-CLAT_NHC)**2)
          if(vmax2.gt.vmax1.and.dist(i,j).lt.d_max)then
            vmax1=vmax2
            imax1=I
            jmax1=j
          end if
        END DO
        END DO

        print*,'I,J,vmax=',imax1,jmax1,vmax1

        RMX_d=1.2*sqrt(((VLON(imax1,jmax1)-CLON_NHC)*cost)**2+    &
                   (VLAT(imax1,jmax1)-CLAT_NHC)**2)


        IF(RMX_d.gt.3.5)RMX_d=3.5
        IF(RMX_d.lt.2.0)RMX_d=2.0
!        IF(RMX_d.lt.3.0)RMX_d=3.0

        IF(vobs.gt.30..and.CLAT_NHC.gt.30.)RMX_d=min(RMX_d,4.5)

! convert 10m wind to lowest level wind

       if(A101(imax1,jmax1).LT.0.5)then                ! A101 = land sea mask, B101 = ZNT
         z0=B101(imax1,jmax1)
         z0=max(z0,(0.085*vobs-0.58)*1.E-3)
       else
         z0=(0.085*vobs-0.58)*1.E-3
       end if

! 7.2/1.944=3.7037

!       vobs_kt=1.944*vobs
!       IF(vobs_kt.gt.60.)then
!         vobs=(3.7037+vobs)/1.12*log(delt_z1/z0)/log(10./z0)
!       else
!         vobs=vobs*log(delt_z1/z0)/log(10./z0)
!       end if

        vmax1=0.
        DO J=1,NY
        DO I=1,NX
          if(dist(i,j).lt.RMX_d)then
            vmax2=sqrt(USC(I,J)**2+VSC(I,J)**2)*C101(I,J)
            if(vmax2.gt.vmax1)then
              vmax1=vmax2
              imax1=I
              jmax1=j
            end if
          end if
        END DO
        END DO

        vmax_s_b=vmax1

       vobs=vobs_o/(C101(imax1,jmax1)+1.E-10)

       vobs=vobs+0.1

       print*,'level 1 vobs, z0=',vobs,z0,A101(imax1,jmax1)

! READ Hurricane Pert.

      ALLOCATE ( PCST(KMX),HP(NX,NY,KMX),HV(NX,NY,KMX) )

      ALLOCATE ( SLPE(NX,NY),SLP_1(NX,NY),TENV(NX,NY,KMX) )
      ALLOCATE ( T_1(NX,NY,KMX),Q_1(NX,NY,KMX) )
      ALLOCATE ( U_1(NX,NY,KMX),V_1(NX,NY,KMX) )

      ALLOCATE ( U_S(NX,NY),V_S(NX,NY) )
      ALLOCATE ( U_A(NX,NY),V_A(NX,NY) )

      ALLOCATE ( USC_1(NX,NY),VSC_1(NX,NY) )      ! hurricane component wind at z=0
      ALLOCATE ( USC1(NX,NY),VSC1(NX,NY) )        ! Hurricane wind at new grids
      ALLOCATE ( USC11(NX,NY),VSC11(NX,NY) )        ! Hurricane wind at new grids
      ALLOCATE ( USC2(NX,NY),VSC2(NX,NY) )        ! Hurricane wind at new grids
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


        RMX_d=RMX_d*fact_v

	print*,'inside hwrf_anl_cs_10m.f RMX_d=',RMX_d

       vobs1=vobs-TWMAX1

!  make the composite storm to have the same radius as the analysis

!      fact=RWMAX1/RWMAX         ! vrmax=RWMAX1

       RWMAX1=vrmax

!      if(fact.gt.1.5)fact=1.5
!      if(fact.lt.0.5)fact=0.5

! READ the stored symmetric storm


      ALLOCATE ( U_2SB(IR1,KMX),T_2SB(IR1,KMX),SLP_2SB(IR1) )
      ALLOCATE ( R_2SB(IR1,KMX),temp_e(KMX),RADIUS2(IR1) )

      REWIND(85)
      READ(85)RMN

      CALL axisym_xy_new(NX,NY,NZ,KMX,HLON,HLAT,VLON,VLAT,     &
                 CLON_NHC,CLAT_NHC,SLP_1,T_1,Q_1,U_1,V_1,      &
                 TH1,RADIUS2,SLPE,TENV,PCST,HP,HV,ZMAX,vobs1,  &
                 dp_obs,psfc_obs,RWMAX1,PRMAX,RMN,             &
		 U_2SB,T_2SB,SLP_2SB,R_2SB,temp_e,DEPTH,SN)

      k850=1        ! use surface wind


      ALLOCATE ( PCST1(NX,NY,KMX),PCST2(KMX),PCSK(KMX2) )

      KST=1

      CLON_NEW=CLON_NHC
      CLAT_NEW=CLAT_NHC

      print*,'zmax=',zmax

!!!!!!!!!!!!!!!!!!!!


      USC_1=0.
      VSC_1=0.

      DO J=1,NY
      DO I=1,NX
        USC_1(I,J)=U_1(I,J,1)
        VSC_1(I,J)=V_1(I,J,1)
      END DO
      END DO

       USC1=USC_1
       VSC1=VSC_1

       DO N=1,KMX2
         PCSK(N)=PCST(2*N)*SLP1_MEAN/PCST(1)
       END DO

       DO N=1,KMX2
         IF(PCSK(N).GE.PCK(1))THEN            ! Below PCK(1)
           TEK42(N)=TEK(1)
         ELSE IF (PCSK(N).LE.PCK(NZ))THEN
           TEK42(N)=TEK(NZ)
         ELSE
!           DO K=1,KMX2-1
           DO K=1,NZ-1
             IF(PCSK(N).LE.PCK(K).and.PCSK(N).GT.PCK(K+1))THEN
               W1=ALOG(1.*PCK(K+1))-ALOG(1.*PCK(K))
               W=(ALOG(1.*PCSK(N))-ALOG(1.*PCK(K)))/W1
               TEK42(N)=TEK(K)*(1.-W)+TEK(K+1)*W
               GO TO 447
             END IF
           END DO
         END IF
 447     CONTINUE
       ENDDO

! ENV. wind

       USCM=USC
       VSCM=VSC

!!!!!!!!!!!!!!!!!!!!

        vmax1=0.
        DO J=1,NY
        DO I=1,NX
          vmax2=sqrt(USC1(I,J)**2+VSC1(I,J)**2)*C101(I,J)
          if(vmax2.gt.vmax1.and.dist(i,j).lt.RMX_d)then
            vmax1=vmax2
          end if
        END DO
        END DO

        vmax_s=vmax1

!        crtn=max(vobs_o-vmax_s_b,0.)/vmax_s
        crtn=1.0

        print*,'hwrf_anl_cs, crtn=',crtn

        vmax1=0.
        DO J=1,NY
        DO I=1,NX
          USC11(I,J)=USC1(I,J)*crtn
          VSC11(I,J)=VSC1(I,J)*crtn
!          vmax2=sqrt((USC11(I,J)+USCM(I,J))**2+                &
!                (VSC11(I,J)+VSCM(I,J))**2)*C101(I,J)
          vmax2=sqrt((USCM(I,J))**2+                &
                (VSCM(I,J))**2)*C101(I,J)
          if(vmax2.gt.vmax1.and.dist(i,j).lt.RMX_d)then
            vmax1=vmax2
            imax1=I
            jmax1=j
          end if
        END DO
        END DO

        vobs=vobs_o/(C101(imax1,jmax1)+1.E-10)

        print*,'I,J,vmax,vobs=',imax1,jmax1,vmax1,vobs

       iter=0
       beta=1.0

 876   CONTINUE

       VMAX=0.
       DO J=1,NY
       DO I=1,NX
!         I=imax1
!         J=jmax1
         UUT=beta*USC11(I,J)+USCM(I,J)
         VVT=beta*VSC11(I,J)+VSCM(I,J)
         FF=SQRT((UUT*UUT+VVT*VVT))*C101(I,J)
!         R_DIST=sqrt(((VLON(I,J)-CLON_NHC)*cost)**2+           &
!                     (VLAT(I,J)-CLAT_NHC)**2)
         R_DIST=dist(i,j)
         IF(VMAX.LT.FF.and.R_DIST.lt.RMX_d)THEN
           VMAX=FF
           IMV=I
           JMV=J
         END IF
       END DO
       END DO

       PRINT*,'I,J,USC11,VSC11,USCM,VSCM=',USC11(IMV,JMV),    &
               VSC11(IMV,JMV),USCM(IMV,JMV),VSCM(IMV,JMV)
       PRINT*,'I,J,VMAX=',IMV,JMV,SQRT(VMAX)

!       IMV=imax1
!       JMV=jmax1

       vobs=vobs_o/(C101(IMV,JMV)+1.E-10)

!       UU11=beta*USC11(IMV,JMV)
!       VV11=beta*VSC11(IMV,JMV)
       UU11=USC11(IMV,JMV)
       VV11=VSC11(IMV,JMV)
       UUM1=USCM(IMV,JMV)
       VVM1=VSCM(IMV,JMV)
       QQ1=(uu11**2+vv11**2)*vobs**2-(vv11*uum1-uu11*vvm1)**2
!       QQ=sqrt((uu11**2+vv11**2)*vobs**2-(vv11*uum1-uu11*vvm1)**2)

       uv22=sqrt(uu11**2+vv11**2)

       print*,'max hurricane pert=',uv22

       if(QQ1.lt.0.)then
         QQ=0.
         beta1=0.0
       else
         QQ=sqrt(QQ1)
         beta1=(-(uum1*uu11+vvm1*vv11)+QQ)/(uu11**2+vv11**2+1.E-20)
       end if

       print*,'UU11,VV11,UUM1,VVM1,QQ,beta1=',UU11,VV11,UUM1,VVM1,QQ,beta1

!       beta=beta*beta1
       beta=beta1
       iter=iter+1
       print*,'iter,beta=',iter,beta

!zhan based in Qingfu's comment       IF(iter.lt.3)go to 876
       IF(iter.lt.1)go to 876

       beta=beta*crtn

       print*,'iter,beta=',iter,beta

       if(beta.lt.0.)beta=0.

!       if(beta.gt.1.25) beta=max(1.25,sqrt(beta))

!!!       beta=0.8


!         if(vobs.lt.24.)then
!            if(abs(CLAT_NHC).lt.15.)beta=max(beta,10./vmax_s)
!         end if

       vmax_1=0.
       DO J=1,NY
       DO I=1,NX
         USC2(I,J)=beta*USC1(I,J)+USCM(I,J)
         VSC2(I,J)=beta*VSC1(I,J)+VSCM(I,J)
         ff=sqrt(USC2(I,J)**2+VSC2(I,J)**2)*C101(I,J)
         IF(vmax_1.lt.ff.and.dist(i,j).lt.RMX_d)THEN
            vmax_1=ff
            imax12=i
            jmax12=j
         end if
       END DO
       END DO

       print*,'after beta correction'
       print*,'10m vmax,i,j=',vmax_1,imax12,jmax12

! now modify the horricane component (by beta)

! for analysis, set T_1=0. Q_1=0.

!      T_1=0.
!      Q_1=0.

!
      T_4=T_1
      Q_4=Q_1

       print*,'CLON_NEW,CLAT_NEW=',CLON_NEW,CLAT_NEW

      ff0=2.*7.292E-5*sin(clat_new*rad)

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
      print*,'storm center 3 =',HLON(i_psm,j_psm),HLAT(i_psm,j_psm),ps_min

!  compute central surface pressure based on the model correlation PS_C1

      rewind(23)

      read(23)IR2,IR_1,JX,JY,JZ,KMX1

      print*,'KMX,KMX1=',KMX,KMX1

      ALLOCATE ( RADIUS(IR2),U_2S(IR2,KMX1),V_2S(IR2,KMX1) )
      ALLOCATE ( T_2S(IR2,KMX1),Q_2S(IR2,KMX1),SLP_2S(IR2) )
      ALLOCATE ( CFT(IR2),RKX1(IR2),U_2S1(IR2,KMX1),U_2S2(IR2) )
      ALLOCATE ( strm1(IR2),strm2(IR2),PW1(KMX1) )

      read(23)RADIUS
      read(23)CLON_NEW1,CLAT_NEW1
      read(23)U_2S,V_2S,U_2S1
      read(23)T_2S,Q_2S,SLP_2S
      read(23)CFT,PW1,beta11
      read(23)

      PW=1.0

      IF((iflag_cold.eq.1).and.DEPTH.eq.'M')THEN
        DO k=1,kmx
          PW(k)=PW_M(k)
        END DO
      END IF
      IF((DEPTH.eq.'S').or.(id_storm.ge.90))THEN
        DO k=1,kmx
          PW(k)=PW_S(k)
        END DO
      END IF

      do k=1,kmx
	print*,'PW(k)=',k,PW(k)
      end do

      RKX1=0.
      DO n=1,IR2
        RKX1(n)=RADIUS(n)*DST1
      END DO

      DO n=1,IR1
        RADIUS2(n)=RADIUS2(n)/DST1
      END DO

      print*,'RADIUS,RADIUS2=',RADIUS(1),RADIUS2(1)

      U_2S2=0.

      JST1=1
      DO J=1,IR2
	DIF=RADIUS2(1)-RADIUS(J)
        IF(DIF.GE.0.)THEN
	  U_2S2(J)=U_2SB(1,1)
	  JST1=J
	END IF
      END DO

      print*,'test JST1=',JST1

      DO J=JST1,IR2
	U_2S3=0.
        DO N=2,IR1
          DIF=RADIUS2(N)-RADIUS(J)
          IF(DIF.GE.0.)THEN
            WT1=(RADIUS(J)-RADIUS2(N-1))/           &
                (RADIUS2(N)-RADIUS2(N-1))
            WT2=1.-WT1
	    U_2S3=WT1*U_2SB(N,1)+WT2*U_2SB(N-1,1)
	    GO TO 55
	  END IF
	END DO
 55     CONTINUE
	U_2S2(J)=U_2S1(J,1)+beta*U_2S3
      END DO

      print*,'test3333'

      k=1
      k1=2

      strm1=0.
      strm2=0.
      DO n=IR2-1,1,-1
	strm11=max(0.,U_2S(n,1))
	strm22=max(0.,U_2S2(n))
	force=(strm11/(RKX1(n)+1.E-20)+ff0)*strm11
	strm1(n)=strm1(n+1)-force*(RKX1(n+1)-RKX1(n))
	force2=(strm22/(RKX1(n)+1.E-20)+ff0)*strm22
	strm2(n)=strm2(n+1)-force2*(RKX1(n+1)-RKX1(n))
      END DO

      print*,'strm2(1),strm1(1)=',strm2(1),strm1(1)

      PS_C2=SLP_2S(1)*strm2(1)/(strm1(1)-1.E-20)

      PS_C1=PS_C2-SLP_2S(1)*cft(1)

      PS_C1=psfc_obs1

      print*,'PS_C1=',PS_C1


      ics=1
      fact=1.0

      DO K=1,KMX
      DO J=1,NY
      DO I=1,NX
	U_1(I,J,K)=U_1(I,J,K)*beta*PW(k)
	V_1(I,J,K)=V_1(I,J,K)*beta*PW(k)
      END DO
      END DO
      END DO


      CALL CORT_MAT_2(IR1,NX,NY,KMX2,KMX,U_2SB,             &
		T_2SB,SLP_2SB,R_2SB,RADIUS2,temp_e,TEK42,   &
                T_X,Q_X,SLP_X,HLON,HLAT,VLON,VLAT,        &
                CLON_NEW,CLAT_NEW,PS_C1,                  &
                beta,fact,ics,SN)

        DO J=1,NY
	DO I=1,NX
!   	  SLP_1(I,J)=SLP_1(I,J)+SLP_X(I,J)
   	  SLP_1(I,J)=SLP_X(I,J)
        END DO
        END DO

! new
!       pt_c1=0.
!       sum1=0.
!       do j=jmn1,jmx1
!       do i=imn1,imx1
!         sum1=sum1+1.
!         pt_c1=pt_c1+SLP_1(I,J)
!       end do
!       end do

!      pt_c1=pt_c1/sum1

        pt_c1=0.
	DO J=1,NY
	DO I=1,NX
	  IF(pt_c1.LT.SLP_1(I,J))THEN
	    pt_c1=SLP_1(I,J)
	  END IF
	END DO
	END DO
!	pt_c1=pt_c1+50.

!      ps_rat=0.5*(psfc_obs1+PS_C1)/PS_C1

!      ps_rat=0.5*(psfc_obs1+pt_c1)/(pt_c1-1.e-20)

      ps_rat=1.0
      if(abs(pt_c1).lt.20.)then
        ps_rat=1.0
      else
        ps_rat=psfc_obs1/(pt_c1-1.e-20)
      end if

      print*,'ps_rat=',ps_rat,psfc_obs1,pt_c1

      if(ps_rat.gt.10.)ps_rat=10.0
      if(ps_rat.lt.(-10.))ps_rat=-10.0

      DO J=1,NY
      DO I=1,NX
        SLP_1(I,J)=SLP_1(I,J)*ps_rat
      END DO
      END DO

        DO J=1,NY
	DO I=1,NX
	  DO K=1,KMX
	    TEK1=TENV(I,J,K)+T_1(I,J,K)
!	    T_1(I,J,K)=(T_1(I,J,K)+T_X(I,J,K))*PW(k)
!            Q_1(I,J,K)=(Q_1(I,J,K)+Q_X(I,J,K))*PW(k)
	    T_1(I,J,K)=T_X(I,J,K)*PW(k)*ps_rat
!            Q_1(I,J,K)=Q_X(I,J,K)*PW(k)
	    TEK2=TENV(I,J,K)+T_1(I,J,K)
	    ESRR=exp(4302.645*(TEK2-TEK1)/((TEK2-29.66)*(TEK1-29.66)))
	    Q_1(I,J,K)=ESRR*Q_1(I,J,K)
	  END DO
	END DO
	END DO

         T_4=0.     ! using guess field temp & water vapor instead of double count
         Q_4=0.


       print*,'complete CORT'



!??????????????????

       DO J=1,NY
       DO I=1,NX
         SLP1(I,J) = SLP1(I,J)+SLP_1(I,J)
         TENV1     = TS1(I,J)
         TS1(I,J)  = TENV1+T_1(I,J,1)
         T_OLD     = TENV1+T_4(I,J,1)
         Q_OLD     = QS1(I,J)+Q_4(I,J,1)
         ESRR      = exp(4302.645*(TS1(I,J)-T_OLD)/     &
                   ((TS1(I,J)-29.66)*(T_OLD-29.66)))               ! 4302.645=17.67*243.5
         QS1(I,J)  = Q_OLD + (ESRR-1.)*Q_OLD                       ! Assuming RH=CONST. before & after
       ENDDO
       ENDDO

!       WRITE(25)((SLP1(I,J),I=1,NX),J=1,NY,2)


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

! add hurricane wind components

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
!	   IF(N.EQ.1)THEN
!	     IF(ZMID(I,J,1).GT.10..and.SLP1(I,J).GT.PMID1(I,J,1))THEN
!	       PRINT*,'before T1(I,J,1)=',T(I,J,1)
!	       T1(I,J,1)=GAMMA*ZMID(I,J,1)/((SLP1(I,J)/PMID1(I,J,1))**COEF3-1.)
!              T1(I,J,1)=T1(I,J,1)/(1.+D608*Q1(I,J,1))
!	       PRINT*,'After T1(I,J,1)=',T(I,J,1)
!	       TS1(I,J)=T1(I,J,1)+GAMMA*(Z1(I,J,2)-Z1(I,J,1))*0.5
!	     END IF
!	   END IF
           ESRR = exp(4302.645*(T1(I,J,N)-TENV1)/       &
                  ((T1(I,J,N)-29.66)*(TENV1-29.66)))
           Q1(I,J,N)=QENV1+(ESRR-1.)*QENV1
         END DO
       ENDDO
       ENDDO

! based on Ts, Zs, SLP1 ==> PS1  ==> P1

       DO J=1,NY
       DO I=1,NX
         ZSFC = ZS1(I,J)
	 TS1(I,J) = T1(I,J,1)+GAMMA*(Z1(I,J,2)-Z1(I,J,1))*0.5
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
!         ZMID1(I,J,K)=0.5*(Z1(I,J,K)+Z1(I,J,K+1))
          TV1=T1(I,J,K)*(1.+D608*Q1(I,J,K))
          ZMID1(I,J,K)=(Z1(I,J,K)+Z1(I,J,K+1))*0.5+             &
            0.5*TV1/GAMMA*(2.-(P1(I,J,K)/PMID1(I,J,K))**COEF3-    &
            (P1(I,J,K+1)/PMID1(I,J,K))**COEF3)
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


       press1=1.E20
       DO J=1,NY
       DO I=1,NX
       if(press1.gt.SLP1(I,J))press1=SLP1(I,J)
       END DO
       END DO

       print*,'surgace pressure=',press1

       DO J=1,NY
       DO I=1,NX
         U1(I,J,1)=USC2(I,J)
         V1(I,J,1)=VSC2(I,J)
       END DO
       END DO

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
real(4),intent(IN ):: x
real(4),intent(OUT):: y
!-----------------------------------------------------------------------------
integer            :: it
!=============================================================================
y=2*x-1; do it=1,nit; y=y*(3-y*y)/2; enddo; y=(y+1)/2
end subroutine dbend
