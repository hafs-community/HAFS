!******************************************************************************
!
! ABSTRACT: Smooth the model vortex, and calculate the axi-symmetric vortex
!
! ORIGINAL AUTHOR: QINGFU LIU, NCEP/EMC, 2007
! REVISED  AUTHOR: Qingfu Liu, 2013
!                : Add the calculation of R34 and Rmax
!                : Calcualte the size correction coeffs: a and b
!
!     DECLARE VARIABLES
!
      INTEGER I,J,K,NX,NY,NZ,IFLAG,NX2
!
!     PARAMETER (NX=215,NY=431,NZ=42,NST=5)
      PARAMETER (NST=5)
      PARAMETER (GAMMA=6.5E-3,G=9.8,Rd=287.05,D608=0.608)
      PARAMETER (Cp=1004.)

!      PARAMETER (KMX=2*NZ+1)
!
! Variables on 4x hybrid coordinate (ENV)

      REAL(4) DLMD,DPHD,PT,PDTOP
      REAL(4) WBD,SBD,CENTRAL_LON,CENTRAL_LAT

      REAL(4), ALLOCATABLE :: T1(:,:,:),Q1(:,:,:)
      REAL(4), ALLOCATABLE :: U1(:,:,:),V1(:,:,:)
      REAL(4), ALLOCATABLE :: Z1(:,:,:),P1(:,:,:)
      REAL(4), ALLOCATABLE :: GLON(:,:),GLAT(:,:)
      REAL(4), ALLOCATABLE :: PD1(:,:),ETA1(:),ETA2(:)

      REAL(4), ALLOCATABLE :: USCT(:,:)
      REAL(4), ALLOCATABLE :: U10(:,:),V10(:,:)

      REAL(4), ALLOCATABLE :: HLON(:,:),HLAT(:,:)
      REAL(4), ALLOCATABLE :: VLON(:,:),VLAT(:,:)

! variables for hurricane component

      REAL(4), ALLOCATABLE :: HLON2(:,:),HLAT2(:,:)
      REAL(4), ALLOCATABLE :: VLON2(:,:),VLAT2(:,:)

      REAL(4), ALLOCATABLE :: SLPE(:,:),SLP_1(:,:),TENV(:,:,:),TEK(:)
!     REAL(4), ALLOCATABLE :: QENV(:,:,:)
      REAL(4), ALLOCATABLE :: T_1(:,:,:),Q_1(:,:,:)
      REAL(4), ALLOCATABLE :: U_1(:,:,:),V_1(:,:,:)

!     REAL(4), ALLOCATABLE :: T_5(:,:,:),Q_5(:,:,:)
!     REAL(4), ALLOCATABLE :: U_5(:,:,:),V_5(:,:,:),SLP_5(:,:)

      REAL(4), ALLOCATABLE :: U_850(:,:),V_850(:,:)

      REAL(4), ALLOCATABLE :: DUM(:,:),DUMA(:,:)

! working array

      REAL(4), ALLOCATABLE :: SLP1(:,:),RIJ(:,:),RIJ2(:,:),ANG2(:,:)
      REAL(4), ALLOCATABLE :: PMID1(:,:,:),ZMID1(:,:,:)
      REAL(4), ALLOCATABLE :: ZS1(:,:),TS1(:,:),QS1(:,:)

      REAL(4), ALLOCATABLE :: U_S(:,:),U_A(:,:)
      REAL(4), ALLOCATABLE :: V_S(:,:),V_A(:,:)

      REAL(4), ALLOCATABLE :: HLON3(:,:),HLAT3(:,:)
      REAL(4), ALLOCATABLE :: VLON3(:,:),VLAT3(:,:)

      REAL(4) CLON0,CLAT0

      REAL(4), ALLOCATABLE :: USC_1(:,:),VSC_1(:,:) ! hurr comp wind at level 1
      REAL(4), ALLOCATABLE :: USC1(:,:),VSC1(:,:) ! Hurricane wind at new grids
      REAL(4), ALLOCATABLE :: SLPV(:,:)

      REAL(4), ALLOCATABLE :: HLON1(:,:),HLAT1(:,:)
      REAL(4), ALLOCATABLE :: VLON1(:,:),VLAT1(:,:)

      REAL(4), ALLOCATABLE :: A101(:,:),B101(:,:),C101(:,:)

      REAL(4), ALLOCATABLE :: U_2(:,:,:),V_2(:,:,:)
      REAL(4), ALLOCATABLE :: T_4(:,:,:),Q_4(:,:,:)

      REAL(4), ALLOCATABLE :: U_2S(:,:),V_2S(:,:)
      REAL(4), ALLOCATABLE :: T_2S(:,:),Q_2S(:,:),SLP_2S(:,:)

      REAL(4), ALLOCATABLE :: U_2S1(:,:)

      REAL(4), ALLOCATABLE :: RKX1(:),RKX2(:),WKX1(:,:),WKX2(:,:)
      REAL(4), ALLOCATABLE :: strm1(:,:),strm2(:,:),strm3(:,:)

      REAL(4), ALLOCATABLE :: PCST(:),HP(:,:,:)

      REAL(4), ALLOCATABLE :: PW(:),v_maxk(:)

      REAL(4), ALLOCATABLE ::    HBWGT1(:,:,:),VBWGT1(:,:,:)
      integer(4), ALLOCATABLE :: IIH1(:,:),JJH1(:,:)
      integer(4), ALLOCATABLE :: IIV1(:,:),JJV1(:,:)

      REAL(4), ALLOCATABLE :: WTCT1(:,:,:,:),WTSM1(:,:)
      REAL(4), ALLOCATABLE :: WTCT2(:,:,:,:),WTSM2(:,:)

      REAL(4), ALLOCATABLE :: RADUS(:),ANGL(:),GLON1(:,:),GLAT1(:,:)
!zhang      REAL(4), ALLOCATABLE :: INDX1(:,:,:),INDY1(:,:,:)
!zhang      REAL(4), ALLOCATABLE :: INDX2(:,:,:),INDY2(:,:,:)
      INTEGER, ALLOCATABLE :: INDX1(:,:,:),INDY1(:,:,:)
      INTEGER, ALLOCATABLE :: INDX2(:,:,:),INDY2(:,:,:)
      REAL(4), ALLOCATABLE :: WTXY1(:,:,:),WTXY2(:,:,:)

!zhang      REAL(4), ALLOCATABLE :: IJ_COUNT1(:,:),IJ_COUNT2(:,:)
      INTEGER, ALLOCATABLE :: IJ_COUNT1(:,:),IJ_COUNT2(:,:)

      REAL(4), ALLOCATABLE :: T_X(:,:,:),Q_X(:,:,:),SLP_X(:,:)
!     REAL(4), ALLOCATABLE :: A11(:,:),B11(:,:),C11(:)
      REAL(4), ALLOCATABLE :: CFT(:)

      REAL(4), ALLOCATABLE :: RADIUS1(:)

      integer(4) IH1(4),JH1(4),IV1(4),JV1(4)

      CHARACTER ST_NAME(NST)*3,SN*1,EW*1,DEPTH*1

!zhang      REAL(8) CLON_NEW,CLAT_NEW,CLON_NHC,CLAT_NHC
!zhang      REAL(8) CLON_NEW1,CLAT_NEW1
      REAL(8) :: CLON_NHC,CLAT_NHC
      REAL(8) :: CLON_NEW,CLAT_NEW

      DIMENSION TWM(101),RWM(101),TH1(200),RP(200)

      REAL(4) zmax

      integer id_storm
      integer Ir_v4(4)
      REAL    R34_obs(4)

      integer I34_F(4),J34_F(4)
      REAL    R34_F(4)

      CHARACTER PART1*2,basin*2,NUM*2
!zhang:added basin domain shift otpion
      CHARACTER*2 :: basin1

      REAL(4) PW_S(85),PW_M(85)

!     DATA PW_S/28*1.0,0.95,0.9,0.8,0.7,          &
!	      0.6,0.5,0.4,0.3,0.2,0.1,47*0./                    ! 850-700mb
!     DATA PW_M/38*1.0,0.95,0.9,0.8,0.7,          &
!	      0.6,0.5,0.4,0.3,0.2,0.1,37*0./                    ! 850-400mb
!      DATA PW_S/36*1.0,0.8,0.6,0.4,0.2,          &
!                45*0./                    ! 600-500mb
      DATA PW_S/38*1.0,0.8,0.6,0.4,0.2,          &
                43*0./                    ! 600-500mb
!2      DATA PW_S/32*1.0,0.8,0.6,0.4,0.2,          &
!2                49*0./                    ! 850-700mb
!1      DATA PW_S/28*1.0,0.95,0.9,0.8,0.7,          &
!1	      0.6,0.5,0.4,0.3,0.2,0.1,47*0./                    ! 850-700mb
      DATA PW_M/32*1.0,0.95,0.9,0.8,0.7,          &
	      0.6,0.5,0.4,0.3,0.2,0.1,43*0./                    ! 850-400mb

      COEF1=Rd/Cp
      COEF3=Rd*GAMMA/G
      COEF2=1./COEF3

      GRD=G/Rd

      pi=4.*atan(1.)
      pi_deg=180./pi
      pi180=1./pi_deg

      DST1=6.371E3*pi180 !* deg -> km

      READ(5,*)ITIM,basin1,INITOPT


! READ 4x area env. data HWRF

      IUNIT=20+ITIM

      READ(IUNIT) NX,NY,NZ,I360

      print*,'NX,NY,NZ,I360=',NX,NY,NZ,I360

      NX1=NX+1
      NY1=NY+1
      NZ1=NZ+1
      IF ( NZ <= 60 ) THEN
        KMX=2*NZ+1
      ELSE
! Warning: currently hard wired to 121 due to memory limit
        KMX=121
      ENDIF

      ALLOCATE ( U1(NX,NY,NZ),V1(NX,NY,NZ) )
      ALLOCATE ( USCT(NX,NY) )
      ALLOCATE ( U10(NX,NY),V10(NX,NY) )

      READ(IUNIT) ! DLMD,DPHD,CENTRAL_LON,CENTRAL_LAT ! Domain res & center (deg)
      READ(IUNIT) ! PT,PDTOP,WBD,SBD
      READ(IUNIT) ! T1
      READ(IUNIT) ! Q1
      READ(IUNIT) U1
      READ(IUNIT) V1
      READ(IUNIT) ! Z1
      READ(IUNIT) ! HLON,HLAT,VLON,VLAT
      READ(IUNIT) ! P1
      READ(IUNIT) ! PD1
      READ(IUNIT) ! ETA1
      READ(IUNIT) ! ETA2

      CLOSE(IUNIT)

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

      cmax=0.
      DO J=1,JY
      DO I=1,JX
        cmax=max(cmax,C101(I,j))
      END DO
      END DO

      if(cmax.lt.0.01)then
        print*,'cmax=',cmax
        stop
      end if

      PRINT*,'JX,JY,NX,NY=',JX,JY,NX,NY

!!!!!!!!!!!!!!!!!!* Read TC vitals ...
      read(11,11)id_storm,ICLAT,SN,ICLON,EW,Ipsfc,Ipcls,      &
                 Irmax,ivobs,Ir_vobs,(Ir_v4(I),I=1,4),DEPTH
 11   format(5x,I2,26x,I3,A1,I5,A1,9x,I4,1x,I4,1x,I4,I3,I4,4I5,1x,A1)

       CLAT_NHC=ICLAT*0.1
       CLON_NHC=ICLON*0.1

       if(SN.eq.'S')CLAT_NHC=-CLAT_NHC
       if(EW.eq.'W')CLON_NHC=-CLON_NHC

!wpac       if(I360.eq.360) then
!wpac          if(CLON_NHC.gt.0.)CLON_NHC=CLON_NHC-360.
!wpac       end if
!
       vobs=ivobs*1.0    !* Vmax (m/s)
       vobs_o= vobs
       VRmax=Ir_vobs*1.  !* RMW  (km)

       if (VRmax.lt.19.) VRmax=19.

       VRmax_deg=VRmax/DST1

!      if(id_storm.lt.50.and.Ipsfc.gt.1005)Ipsfc=1005

       psfc_obs=Ipsfc*100.  !* pmin (Pa)
       psfc_cls=Ipcls*100.  !* pout (Pa)

       PRMAX=Irmax*1.       !* ROCI (km)

       R34obs = 0.
       R34obsm= 0.
       acount = 0.
       R34_obs= 0.
       DO i = 1, 4
!         if ( Ir_v4(i) > 0 ) then
            if ( Ir_v4(i) < 0 ) Ir_v4(i)=0
            R34_obs(i) = Ir_v4(i)
            R34obs = R34obs + Ir_v4(i)
            acount = acount + 1.
            if(R34obsm.lt.R34_obs(i)) R34obsm = R34_obs(i)
!         endif
       ENDDO
       IF ( acount > 0. ) R34obs = R34obs/acount     !* avg R34 [km]

       PRINT*, 'Obsereved Vmax:   vobs [m/s], DEPTH =', vobs, DEPTH
       PRINT*, 'Obsereved RMW:  Ir_vobs, VRmax [km] =', Ir_vobs, VRmax
       PRINT*, 'Obsereved R34: NE,SE,SW,NW,AVG [km] =', Ir_v4, R34obs,R34obsm
       PRINT*, 'Obsereved ROCI:   Irmax, PRmax [km] =', Irmax, PRmax
       PRINT*, 'Obsereved Pressure: pmin, pout [Pa] =', psfc_obs, psfc_cls


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! READ Hurricane Pert.

      ALLOCATE ( HLON2(NX,NY),HLAT2(NX,NY) )
      ALLOCATE ( VLON2(NX,NY),VLAT2(NX,NY) )

      ALLOCATE ( PCST(KMX),HP(NX,NY,KMX) )
      ALLOCATE ( SLPE(NX,NY),SLP_1(NX,NY),TENV(NX,NY,KMX),TEK(KMX) )
!     ALLOCATE ( QENV(NX,NY,KMX) )
      ALLOCATE ( T_1(NX,NY,KMX),Q_1(NX,NY,KMX) )
      ALLOCATE ( U_1(NX,NY,KMX),V_1(NX,NY,KMX) )
      ALLOCATE ( U_850(NX,NY),V_850(NX,NY) )

      ALLOCATE ( U_S(NX,NY),U_A(NX,NY) )
      ALLOCATE ( V_S(NX,NY),V_A(NX,NY) )

      ALLOCATE ( USC_1(NX,NY),VSC_1(NX,NY) )      ! hurr comp wind at level 1
      ALLOCATE ( USC1(NX,NY),VSC1(NX,NY) )        ! Hurricane wind at new grids
      ALLOCATE ( SLPV(NX,NY) )

      ALLOCATE ( RIJ2(NX,NY),ANG2(NX,NY) )

      ALLOCATE ( PW(KMX),v_maxk(KMX) )

      PW=1.

      SLP_1=0.
      T_1=0.
      Q_1=0.
      U_1=0.
      V_1=0.

      NCHT=71
      READ(NCHT)KSTM
      PRINT*,'test1',KSTM

      READ(NCHT)HLAT2,HLON2
      READ(NCHT)VLAT2,VLON2

      print*,'HLAT2,HLON2=',HLAT2(1,1),HLON2(1,1)
      print*,'VLAT2,VLON2=',VLAT2(1,1),VLON2(1,1)

      deltp=1.e20

      READ(NCHT)PCST
      DO K=1,KMX
         PRINT*,'K,PCST=',K,PCST(K)
         deltp1=abs(PCST(K)-85000.)
         IF (deltp1.LT.deltp) THEN
            deltp=deltp1
            k850=k
         END IF
      END DO

      k850=1       ! use the surface wind

      READ(NCHT)HP

!..   DO KST=1,KSTM
      KST=1

      READ(NCHT)ST_NAME(KST)
      PRINT*,'ST_NAME=',ST_NAME(KST)
      READ(NCHT)CLON_NEW,CLAT_NEW
!
      PRINT*,CLON_NEW,CLAT_NEW

      READ(NCHT)zmax

      print*,'zmax=',zmax

      READ(NCHT)IWMIN1,IWMAX1,JWMIN1,JWMAX1
      PRINT*,IWMIN1,IWMAX1,JWMIN1,JWMAX1
      READ(NCHT)((SLPE(I,J),I=1,NX),J=1,NY)          ! SLP
      READ(NCHT)((SLP_1(I,J),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)    ! pert SLP
      WRITE(25)((SLP_1(I,J),I=1,NX),J=1,NY,2)
      PRINT*,'TEST1'
      DO K=1,KMX
         READ(NCHT)((TENV(I,J,K),I=1,NX),J=1,NY)
         READ(NCHT)((T_1(I,J,K),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
         WRITE(25)((T_1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,KMX
         READ(NCHT)((U_1(I,J,K),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
         READ(NCHT)((V_1(I,J,K),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
         WRITE(25)((U_1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,KMX
         WRITE(25)((V_1(I,J,K),I=1,NX),J=1,NY,2)
      END DO

      PRINT*,'TEST31'
      DO K=1,KMX
!!!      READ(NCHT)((QENV(I,J,K),I=1,NX),J=1,NY)
         READ(NCHT)((Q_1(I,J,K),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
         WRITE(25)((Q_1(I,J,K),I=1,NX),J=1,NY,2)
      END DO

!     read(NCHT)imn1,imx1,jmn1,jmx1
!     do k=1,kmx
!        read(NCHT)((U_1(i,j,k),i=imn1,imx1),j=jmn1,jmx1)   ! U_1
!        read(NCHT)((V_1(i,j,k),i=imn1,imx1),j=jmn1,jmx1)   ! V_1
!     end do

!     READ(NCHT)((SLPE(I,J),I=1,NX),J=1,NY)          ! SLP
!     DO K=1,KMX
!        READ(NCHT)((TENV(I,J,K),I=1,NX),J=1,NY)
!     END DO
      PRINT*,'TEST41'
!..   END DO

      CLOSE(NCHT)

! grads output

      NX2=NX*2-1

      ALLOCATE( DUM(NX,NY) )
      ALLOCATE( DUMA(NX2,NY) )

!     call open_grads('storm_pert_in',NX2,NY,KMX,1.0,1.0,1.0,1.0)
      call open_grads('storm_pert_in',NX,NY,KMX,1.0,1.0,1.0,1.0)

      open(91,file='storm_pert_in',form='unformatted')
      rewind 91

      do K=1,KMX
          call load(U_1,NX,NY,KMX,K,DUM)
          write(91) DUM
!         call fill_nmm_gridg(DUM,NX,NY,DUMA,2)
!         write(91) DUMA
      end do
      do K=1,KMX
          call load(V_1,NX,NY,KMX,K,DUM)
          write(91) DUM
!         call fill_nmm_gridg(DUM,NX,NY,DUMA,2)
!         write(91) DUMA
      end do
      do K=1,KMX
          call load(T_1,NX,NY,KMX,K,DUM)
          write(91) DUM
!         call fill_nmm_gridg(DUM,NX,NY,DUMA,2)
!         write(91) DUMA
      end do
      do K=1,KMX
          call load(Q_1,NX,NY,KMX,K,DUM)
          write(91) DUM
!         call fill_nmm_gridg(DUM,NX,NY,DUMA,2)
!         write(91) DUMA
      end do
      write(91) SLP_1
!     call fill_nmm_gridg(SLP_1,NX,NY,DUMA,1)
!     write(91) DUMA

      close(91)

      DEALLOCATE( DUM, DUMA )

! grads output


! save original data for later use

!     ALLOCATE ( T_5(NX,NY,KMX),Q_5(NX,NY,KMX) )
!     ALLOCATE ( U_5(NX,NY,KMX),V_5(NX,NY,KMX),SLP_5(NX,NY) )

      ALLOCATE ( T_X(NX,NY,KMX),Q_X(NX,NY,KMX),SLP_X(NX,NY) )
!     ALLOCATE ( A11(NZ,3),B11(NZ,3),C11(3) )

!     U_5=U_1
!     V_5=V_1
!     T_5=T_1
!     Q_5=Q_1
!     SLP_5=SLP_1
!

! find storm center TENV

      cost=cos(pi180*CLAT_NEW)

      RIJ2=0.
      distm=1.E20
      do j=1,ny
      do i=1,nx
         distt=((HLON2(i,j)-CLON_NEW)*cost)**2+(HLAT2(i,j)-CLAT_NEW)**2
         RIJ2(I,J)=SQRT(distt)
         if (distm.GT.distt) then
            distm=distt
            ictr=i
            jctr=j
         end if
      end do
      end do

      DO K=1,KMX
         TEK(K)=TENV(ictr,jctr,K)
      END DO


      DO K=1,KMX
         v_maxk(k)=0.
         DO J=JWMIN1,JWMAX1
         DO I=IWMIN1,IWMAX1
            v_max1=U_1(I,J,K)*U_1(I,J,K)+V_1(I,J,K)*V_1(I,J,K)
            if (v_maxk(k).LT.v_max1) v_maxk(k)=v_max1
         END DO
         END DO
         v_maxk(k)=sqrt(v_maxk(k))
         print*,'K,v_maxk=',K,v_maxk(k)
      END DO


      DO J=1,NY
      DO I=1,NX
         U_850(I,J)=U_1(I,J,K850)
         V_850(I,J)=V_1(I,J,K850)
      END DO
      END DO

      ismth_01=0

!     IF(IFLAG_Z.LT.10)THEN            !  smooth
!     IF(vobs.lt.24..and.DEPTH.ne.'S')THEN
!!!!!!      IF(vobs.lt.24.)THEN
      IF (vobs .GT. 5.) THEN !* -------------------------------------------->

!     go to 665

! smooth first

      ALLOCATE ( T_4(NX,NY,KMX),Q_4(NX,NY,KMX) )

      N_smth=0
 667  CONTINUE

         IF (INITOPT .EQ. 0 .and. ismth_01.EQ.1) THEN

            print*,'smooth uv'

            T_4=0.
            Q_4=0.           ! borrow array

!$omp parallel do &
!$omp& private(i,j,k)
            DO J=JWMIN1,JWMAX1
              IF(mod(J,2) .ne. 0)THEN
                DO I=IWMIN1,IWMAX1
                DO K=1,KMX
!            T_4(I,J,K)=0.2*(U_1(I,J-1,K)+U_1(I+1,J-1,K)+        &
!                       U_1(I,J,K)+U_1(I,J+1,K)+U_1(I+1,J+1,K))
!            Q_4(I,J,K)=0.2*(V_1(I,J-1,K)+V_1(I+1,J-1,K)+        &
!                       V_1(I,J,K)+V_1(I,J+1,K)+V_1(I+1,J+1,K))
                  T_4(I,J,K)=(U_1(I,J-1,K)+U_1(I+1,J-1,K)+             &
                              U_1(I,J,K)+U_1(I,J+1,K)+U_1(I+1,J+1,K)+  &
                              U_1(I,J-2,K)+U_1(I,J+2,K)+U_1(I-1,J,K)+  &
                              U_1(I+1,J,K))/9.
                  Q_4(I,J,K)=(V_1(I,J-1,K)+V_1(I+1,J-1,K)+             &
                              V_1(I,J,K)+V_1(I,J+1,K)+V_1(I+1,J+1,K)+  &
                              V_1(I,J-2,K)+V_1(I,J+2,K)+V_1(I-1,J,K)+  &
                              V_1(I+1,J,K))/9.
                END DO
                END DO
              ELSE
                DO I=IWMIN1,IWMAX1
                DO K=1,KMX
!            T_4(I,J,K)=0.2*(U_1(I-1,J-1,K)+U_1(I,J-1,K)+        &
!                       U_1(I,J,K)+U_1(I-1,J+1,K)+U_1(I,J+1,K))
!            Q_4(I,J,K)=0.2*(V_1(I-1,J-1,K)+V_1(I,J-1,K)+        &
!                       V_1(I,J,K)+V_1(I-1,J+1,K)+V_1(I,J+1,K))
                  T_4(I,J,K)=(U_1(I-1,J-1,K)+U_1(I,J-1,K)+             &
                              U_1(I,J,K)+U_1(I-1,J+1,K)+U_1(I,J+1,K)+  &
                              U_1(I,J-2,K)+U_1(I,J+2,K)+U_1(I-1,J,K)+  &
                              U_1(I+1,J,K))/9.
                  Q_4(I,J,K)=(V_1(I-1,J-1,K)+V_1(I,J-1,K)+             &
                              V_1(I,J,K)+V_1(I-1,J+1,K)+V_1(I,J+1,K)+  &
                              V_1(I,J-2,K)+V_1(I,J+2,K)+V_1(I-1,J,K)+  &
                              V_1(I+1,J,K))/9.
                END DO
                END DO
              END IF
            END DO

            U_1=T_4
            V_1=Q_4

            print*,'smooth tq'

            T_4=0.
            Q_4=0.           ! borrow array

!$omp parallel do &
!$omp& private(i,j,k)
            DO J=JWMIN1,JWMAX1
              IF(mod(J,2) .ne. 0)THEN
                DO I=IWMIN1,IWMAX1
                DO K=1,KMX
!            T_4(I,J,K)=0.2*(T_1(I-1,J-1,K)+T_1(I,J-1,K)+        &
!                       T_1(I,J,K)+T_1(I-1,J+1,K)+T_1(I,J+1,K))
!            Q_4(I,J,K)=0.2*(Q_1(I-1,J-1,K)+Q_1(I,J-1,K)+        &
!                       Q_1(I,J,K)+Q_1(I-1,J+1,K)+Q_1(I,J+1,K))
                  T_4(I,J,K)=(T_1(I-1,J-1,K)+T_1(I,J-1,K)+             &
                              T_1(I,J,K)+T_1(I-1,J+1,K)+T_1(I,J+1,K)+  &
                              T_1(I,J-2,K)+T_1(I,J+2,K)+T_1(I-1,J,K)+  &
                              T_1(I+1,J,K))/9.
                  Q_4(I,J,K)=(Q_1(I-1,J-1,K)+Q_1(I,J-1,K)+             &
                              Q_1(I,J,K)+Q_1(I-1,J+1,K)+Q_1(I,J+1,K)+  &
                              Q_1(I,J-2,K)+Q_1(I,J+2,K)+Q_1(I-1,J,K)+  &
                              Q_1(I+1,J,K))/9.
                END DO
                END DO
              ELSE
                DO I=IWMIN1,IWMAX1
                DO K=1,KMX
!            T_4(I,J,K)=0.2*(T_1(I,J-1,K)+T_1(I+1,J-1,K)+        &
!                       T_1(I,J,K)+T_1(I,J+1,K)+T_1(I+1,J+1,K))
!            Q_4(I,J,K)=0.2*(Q_1(I,J-1,K)+Q_1(I+1,J-1,K)+        &
!                       Q_1(I,J,K)+Q_1(I,J+1,K)+Q_1(I+1,J+1,K))
                  T_4(I,J,K)=(T_1(I,J-1,K)+T_1(I+1,J-1,K)+             &
                              T_1(I,J,K)+T_1(I,J+1,K)+T_1(I+1,J+1,K)+  &
                              T_1(I,J-2,K)+T_1(I,J+2,K)+T_1(I-1,J,K)+  &
                              T_1(I+1,J,K))/9.
                  Q_4(I,J,K)=(Q_1(I,J-1,K)+Q_1(I+1,J-1,K)+             &
                              Q_1(I,J,K)+Q_1(I,J+1,K)+Q_1(I+1,J+1,K)+  &
                              Q_1(I,J-2,K)+Q_1(I,J+2,K)+Q_1(I-1,J,K)+  &
                              Q_1(I+1,J,K))/9.
                END DO
                END DO
              END IF
            END DO

            T_1=T_4
            Q_1=Q_4

! smooth slp

            T_4=0.

!$omp parallel do &
!$omp& private(i,j)
            DO J=JWMIN1,JWMAX1
              IF(mod(J,2) .ne. 0)THEN
                DO I=IWMIN1,IWMAX1
!            T_4(I,J,1)=0.2*(SLP_1(I-1,J-1)+SLP_1(I,J-1)+        &
!                       SLP_1(I,J)+SLP_1(I-1,J+1)+SLP_1(I,J+1))
                  T_4(I,J,1)=(SLP_1(I-1,J-1)+SLP_1(I,J-1)+             &
                              SLP_1(I,J)+SLP_1(I-1,J+1)+SLP_1(I,J+1)+  &
                              SLP_1(I,J-2)+SLP_1(I,J+2)+SLP_1(I-1,J)+  &
                              SLP_1(I+1,J))/9.
                END DO
              ELSE
                DO I=IWMIN1,IWMAX1
!            T_4(I,J,1)=0.2*(SLP_1(I,J-1)+SLP_1(I+1,J-1)+        &
!                       SLP_1(I,J)+SLP_1(I,J+1)+SLP_1(I+1,J+1))
                  T_4(I,J,1)=(SLP_1(I,J-1)+SLP_1(I+1,J-1)+             &
                              SLP_1(I,J)+SLP_1(I,J+1)+SLP_1(I+1,J+1)+  &
                              SLP_1(I,J-2)+SLP_1(I,J+2)+SLP_1(I-1,J)+  &
                              SLP_1(I+1,J))/9.
                END DO
              END IF
            END DO

            DO J=JWMIN1,JWMAX1
            DO I=IWMIN1,IWMAX1
               SLP_1(I,J)=T_4(I,J,1)
            END DO
            END DO
         END IF

         K=1
!      DO K=1,KMX
         v_maxk(k)=0.
         DO J=JWMIN1,JWMAX1
         DO I=IWMIN1,IWMAX1
            v_max1=U_1(I,J,K)*U_1(I,J,K)+V_1(I,J,K)*V_1(I,J,K)
            if (v_maxk(k).LT.v_max1) then
              v_maxk(k)=v_max1
              I_max1=I
              J_max1=J
            end if
         END DO
         END DO
         v_maxk(k)=sqrt(v_maxk(k))
         print*,'after smooth K,v_maxk=',K,v_maxk(k)
!      END DO

         IF (INITOPT .EQ. 0 .and. ismth_01.EQ.1) THEN

            N_smth=N_smth+1
            print*,'N_smth=',N_smth
            IF(N_smth.lt.2)go to 667
            IF((N_smth.lt.15.and.VRmax_deg.lt.1.5)                     &
               .and.RIJ2(I_max1,J_max1).lt.VRmax_deg)THEN
               print*,'N_smth,RIJ2,VRmax_deg=',N_smth,RIJ2(I_max1,J_max1),VRmax_deg
               go to 667
            END IF

         END IF

      DEALLOCATE ( T_4,Q_4 )

 665  continue

      RMX_d=max(2.0,3.*VRmax/DST1)
!      RMX_d=max(RMX_d,3.0)
      IF(RMX_d.gt.3.5)RMX_d=3.5

      PRINT*,'RMX_d2=',RMX_d

      smax1=0.
      DO J=1,NY
      DO I=1,NX
!         RIJ2(I,J)=SQRT((VLAT(I,J)-CLAT_NEW)**2+((VLON(I,J)-CLON_NEW)*cost)**2)
         R05=RIJ2(I,J)
         smax2=SQRT((U_1(I,J,1)+U1(I,J,1))**2+(V_1(I,J,1)+V1(I,J,1))**2)*C101(I,J)
         if (smax2.gt.smax1.and.R05.LT.RMX_d) then
            smax1=smax2
            i_max=I
            j_max=J
         end if
      END DO
      END DO

      PRINT*,'10m max wind speed=',i_max,j_max,smax1,C101(i_max,j_max)

      IMV=i_max
      JMV=j_max

      vobs=vobs_o/(C101(i_max,j_max)+1.E-10)

      beta=1.0
      UU11=beta*U_1(IMV,JMV,1)
      VV11=beta*V_1(IMV,JMV,1)
      UUM1=U1(IMV,JMV,1)
      VVM1=V1(IMV,JMV,1)
      QQ=sqrt((uu11**2+vv11**2)*vobs**2-(vv11*uum1-uu11*vvm1)**2)

      beta1=(-(uum1*uu11+vvm1*vv11)+QQ)/(uu11**2+vv11**2+1.E-20)

      print*,'UU11,VV11,UUM1,VVM1,QQ,beta1=',UU11,VV11,UUM1,VVM1,QQ,beta1

      beta1=min(1.5,max(0.5,beta1))

      print*,'UU11,VV11,UUM1,VVM1,QQ,beta1=',UU11,VV11,UUM1,VVM1,QQ,beta1

      beta=beta*beta1

! estimated final 10m wind

       DO J=1,NY
       DO I=1,NX
         U10(I,J)=(beta*U_1(I,J,1)+U1(I,J,1))*C101(I,J)
         V10(I,J)=(beta*V_1(I,J,1)+V1(I,J,1))*C101(I,J)
         USCT(I,J)=SQRT((U10(I,J))**2+(V10(I,J))**2)
       END DO
       END DO

       smax1=0.
       DO J=1,NY
       DO I=1,NX
         R05=RIJ2(I,J)
         smax2=USCT(I,J)
!         smax2=SQRT(U_1(I,J,1)**2+V_1(I,J,1)**2)
         if (smax2.gt.smax1.and.R05.LT.RMX_d) then
            smax1=smax2
            i_max=I
            j_max=J
         end if
       END DO
       END DO

       PRINT*,'10m max wind speed after correction=',i_max,j_max,smax1

       JCTRM1=jctr-1
       JCTRP1=jctr+1
       ICTRM1=ictr-1
       ICTRP1=ictr+1

       PRMAX2=0.95*PRMAX

! look for max R34 in first quadrat

       v34kt= 34./1.944        !* m/s
       v50kt= 50./1.944        !* m/s
       v64kt= 64./1.944        !* m/s

       DO J=1,NY
       DO I=1,NX
         RIJ2(I,J)=RIJ2(I,J)*DST1
       END DO
       END DO

       R34_F=0.
       I34_F=0
       J34_F=0
       DO J=JCTRP1,NY
       DO I=ICTRP1,NX
         IF(RIJ2(I,J).LT.PRMAX2)THEN
           IF((USCT(I,J).GT.v34kt).and.(RIJ2(I,J).GT.R34_F(1)))THEN
             R34_F(1)=RIJ2(I,J)
             I34_F(1)=I
             J34_F(1)=J
           END IF
         END IF
       END DO
       END DO

! look for max R34 in second quadrant

       DO J=JCTRP1,NY
       DO I=1,ICTRM1
         IF(RIJ2(I,J).LT.PRMAX2)THEN
           IF((USCT(I,J).GT.v34kt).and.(RIJ2(I,J).GT.R34_F(2)))THEN
             R34_F(2)=RIJ2(I,J)
             I34_F(2)=I
             J34_F(2)=J
           END IF
         END IF
       END DO
       END DO

! look for max R34 in third quadrant

       DO J=1,JCTRM1
       DO I=1,ICTRM1
         IF(RIJ2(I,J).LT.PRMAX2)THEN
           IF((USCT(I,J).GT.v34kt).and.(RIJ2(I,J).GT.R34_F(3)))THEN
             R34_F(3)=RIJ2(I,J)
             I34_F(3)=I
             J34_F(3)=J
           END IF
         END IF
       END DO
       END DO

! look for max R34 in fourth quadrant

       DO J=1,JCTRM1
       DO I=ICTRP1,NX
         IF(RIJ2(I,J).LT.PRMAX2)THEN
           IF((USCT(I,J).GT.v34kt).and.(RIJ2(I,J).GT.R34_F(4)))THEN
             R34_F(4)=RIJ2(I,J)
             I34_F(4)=I
             J34_F(4)=J
           END IF
         END IF
       END DO
       END DO

       print*,'I34_F(1),J34_F(1),R34_F(1)=',I34_F(1),J34_F(1),R34_F(1)
       print*,'I34_F(2),J34_F(2),R34_F(2)=',I34_F(2),J34_F(2),R34_F(2)
       print*,'I34_F(3),J34_F(3),R34_F(3)=',I34_F(3),J34_F(3),R34_F(3)
       print*,'I34_F(4),J34_F(4),R34_F(4)=',I34_F(4),J34_F(4),R34_F(4)

!
! computer correlation using cylindrical grid

      IR1=120
      IR =120-1
      DR1=0.1
!      IT1=180
!      DA1=2.*pi180     ! 1 degree
      IT1=72
      DA1=5.*pi180     ! 1 degree

      IR0=3
      IT2=IT1+1

      ALLOCATE ( RADUS(IR1),ANGL(IT2) )
      ALLOCATE ( GLON1(IR1,IT1),GLAT1(IR1,IT1) )
      ALLOCATE ( INDX1(IR1,IT1,50),INDY1(IR1,IT1,50) )    ! for H grid
      ALLOCATE ( WTXY1(IR1,IT1,50),WTSM1(IR1,IT1) )       ! for H grid
      ALLOCATE ( INDX2(IR1,IT1,50),INDY2(IR1,IT1,50) )    ! for V grid
      ALLOCATE ( WTXY2(IR1,IT1,50),WTSM2(IR1,IT1) )       ! for V grid

      ALLOCATE ( IJ_COUNT1(IR1,IT1),IJ_COUNT2(IR1,IT1) )

      ALLOCATE ( U_2(IR1,IT2,KMX),V_2(IR1,IT2,KMX) )
      ALLOCATE ( U_2S(IR1,KMX),V_2S(IR1,KMX) )
      ALLOCATE ( T_2S(IR1,KMX),Q_2S(IR1,KMX),SLP_2S(IR1,2) )

      ALLOCATE ( U_2S1(IR1,KMX),RADIUS1(IR1),CFT(IR1) )
      ALLOCATE ( RKX1(IR1),RKX2(IR1),WKX1(IR1,KMX),WKX2(IR1,KMX) )
      ALLOCATE ( strm1(IR1,KMX),strm2(IR1,KMX),strm3(IR1,KMX) )

      INDX1=0
      INDY1=0
      WTXY1=0.
      WTSM1=0.

      INDX2=0
      INDY2=0
      WTXY2=0.
      WTSM2=0.

      IJ_COUNT1=0
      IJ_COUNT2=0

      DO I=1,IR1
         RADUS(I)=DR1*I
      END DO
      DO J=1,IT2
         ANGL(J)=DA1*(J-1)
!        print*,'ANGL=',ANGL(J)
      END DO


      DO J=1,IT1
      DO I=1,IR1
         GLON1(I,J)=CLON_NEW+RADUS(I)*COS(ANGL(J))
         GLAT1(I,J)=CLAT_NEW+RADUS(I)*SIN(ANGL(J))
      END DO
      END DO

      cost=cos(pi180*CLAT_NEW)

! compute weight for cylindrical coord

      RCT=0.05
      RCT2=RCT*RCT

!$omp parallel do &
!$omp& private(i,j,i2,j2,DSTX,DSTY,RDT2)
      DO J=1,IT1
      DO I=1,IR1
         DO J2=1,NY
         DO I2=1,NX
!..      DO J2=JWMIN1,JWMAX1
!..      DO I2=IWMIN1,IWMAX1
            DSTX=(GLON1(I,J)-HLON2(I2,J2))*cost                 ! H point
            DSTY=GLAT1(I,J)-HLAT2(I2,J2)
            RDT2=DSTX*DSTX+DSTY*DSTY
            IF (RDT2.LT.RCT2) THEN
               IJ_COUNT1(I,J)=IJ_COUNT1(I,J)+1
               INDX1(I,J,IJ_COUNT1(I,J))=I2
               INDY1(I,J,IJ_COUNT1(I,J))=J2
!              WTXY1(I,J,IJ_COUNT1(I,J))=0.3+0.7*(RCT2-RDT2)/(RCT2+RDT2)
               WTXY1(I,J,IJ_COUNT1(I,J))=1.0
               WTSM1(I,J)=WTSM1(I,J)+WTXY1(I,J,IJ_COUNT1(I,J))
            END IF
         END DO
         END DO
      END DO
      END DO
!$omp parallel do &
!$omp& private(i,j,i2,j2,DSTX,DSTY,RDT2)
      DO J=1,IT1
      DO I=IR0,IR1
         DO J2=1,NY
         DO I2=1,NX
!..      DO J2=JWMIN1,JWMAX1
!..      DO I2=IWMIN1,IWMAX1
            DSTX=(GLON1(I,J)-VLON2(I2,J2))*cost               ! V point
            DSTY=GLAT1(I,J)-VLAT2(I2,J2)
            RDT2=DSTX*DSTX+DSTY*DSTY
            IF (RDT2.LT.RCT2) THEN
               IJ_COUNT2(I,J)=IJ_COUNT2(I,J)+1
               INDX2(I,J,IJ_COUNT2(I,J))=I2
               INDY2(I,J,IJ_COUNT2(I,J))=J2
!              WTXY2(I,J,IJ_COUNT2(I,J))=0.3+0.7*(RCT2-RDT2)/(RCT2+RDT2)
               WTXY2(I,J,IJ_COUNT2(I,J))=1.0
               WTSM2(I,J)=WTSM2(I,J)+WTXY2(I,J,IJ_COUNT2(I,J))
            END IF
         END DO
         END DO
      END DO
      END DO

! compute tangential wind & radial wind

      KM1=3
!      KM1=60

      U_2=0.
      V_2=0.           ! borrow array

!$omp parallel do &
!$omp& private(i,j,k,i2,j2,m1)
      DO J=1,IT1
      DO I=IR0,IR1
      DO M1=1,IJ_COUNT2(I,J)
         I2=INDX2(I,J,M1)
         J2=INDY2(I,J,M1)
         DO K=1,KM1
            U_2(I,J,K)=U_2(I,J,K)+U_1(I2,J2,K)*WTXY2(I,J,M1)
            V_2(I,J,K)=V_2(I,J,K)+V_1(I2,J2,K)*WTXY2(I,J,M1)
         END DO
      END DO
      END DO
      END DO
!$omp parallel do &
!$omp& private(i,j,k)
      DO J=1,IT1
      DO I=IR0,IR1
      DO K=1,KM1
         U_2(I,J,K)=U_2(I,J,K)/(WTSM2(I,J)+1.E-20)
         V_2(I,J,K)=V_2(I,J,K)/(WTSM2(I,J)+1.E-20)
      END DO
      END DO
      END DO

!$omp parallel do &
!$omp& private(i,j,k,UT1,VT1)
      DO K=1,KM1
      DO J=1,IT1
      DO I=IR0,IR1
         UT1=-sin(ANGL(J))*U_2(I,J,K)+cos(ANGL(J))*V_2(I,J,K)
         VT1= cos(ANGL(J))*U_2(I,J,K)+sin(ANGL(J))*V_2(I,J,K)
         U_2(I,J,K)=UT1              ! U_2 becomes tangential wind
         V_2(I,J,K)=VT1              ! V_2 becomes radial wind
      END DO
      END DO
      END DO

! computer axi_symmetry storm

      U_2S=0.
      V_2S=0.
      DO K=1,KM1
      DO I=IR0,IR1
         usum=0.
         vsum=0.
         DO J=1,IT1
            usum=usum+U_2(I,J,K)
            vsum=vsum+V_2(I,J,K)
         END DO
         U_2S(I,K)=usum/float(IT1)
         V_2S(I,K)=vsum/float(IT1)
      END DO
      END DO

      DO I=1,IR0
         r_wt=RADUS(I)/RADUS(IR0)
         DO K=1,KM1
            U_2S(I,K)=U_2S(IR0,K)*r_wt
            V_2S(I,K)=V_2S(IR0,K)*r_wt
         END DO
      END DO

! check the asymmetry of the storm

!      asym_11=0.
!      asym_12=0.
!      asym_21=0.
!      asym_22=0.
!      asym_31=0.
!      asym_32=0.
!      DO I=1,IR1
!        usum=0.
!        DO J=1,IT1
!          usum=usum+abs(U_2(I,J,3)-U_2S(I,3))
!        END DO
!        asym_11=asym_11+usum/IT1
!        asym_12=asym_12+abs(U_2S(I,3))
!        asym_21=asym_21+usum/IT1*sqrt(RADUS(I))
!        asym_22=asym_22+abs(U_2S(I,3))*sqrt(RADUS(I))
!        asym_31=asym_31+abs(U_2S(I,3))*RADUS(I)
!        asym_32=asym_32+abs(U_2S(I,3))*RADUS(I)
!      END DO
!      print*,'T-wind, asym_11,12=',asym_11,asym_12,asym_11/(asym_12+1.E-20)
!      print*,'T-wind, asym_21,22=',asym_21,asym_22,asym_21/(asym_22+1.E-20)
!      print*,'T-wind, asym_31,32=',asym_31,asym_32,asym_31/(asym_32+1.E-20)

!
      print*,'smooth t,r'

      U_2=0.
      V_2=0.           ! borrow array

!$omp parallel do &
!$omp& private(i,j,k,i2,j2,m1)
      DO J=1,IT1
      DO I=1,IR1
      DO M1=1,IJ_COUNT1(I,J)
         I2=INDX1(I,J,M1)
         J2=INDY1(I,J,M1)
         DO K=1,KMX
            U_2(I,J,K)=U_2(I,J,K)+T_1(I2,J2,K)*WTXY1(I,J,M1)
            V_2(I,J,K)=V_2(I,J,K)+Q_1(I2,J2,K)*WTXY1(I,J,M1)
         END DO
      END DO
      END DO
      END DO
!$omp parallel do &
!$omp& private(i,j,k)
      DO J=1,IT1
      DO I=1,IR1
      DO K=1,KMX
         U_2(I,J,K)=U_2(I,J,K)/(WTSM1(I,J)+1.E-20)
         V_2(I,J,K)=V_2(I,J,K)/(WTSM1(I,J)+1.E-20)
      END DO
      END DO
      END DO

! computer axi_symmetry storm

      T_2S=0.
      Q_2S=0.
      DO K=1,KMX
      DO I=1,IR1
         usum=0.
         vsum=0.
         DO J=1,IT1
            usum=usum+U_2(I,J,K)
            vsum=vsum+V_2(I,J,K)
         END DO
         T_2S(I,K)=usum/float(IT1)
         Q_2S(I,K)=vsum/float(IT1)
      END DO
      END DO


! smooth slp

      print*,'test4='

      U_2=0.

!$omp parallel do &
!$omp& private(i,j,i2,j2,m1)
      DO J=1,IT1
      DO I=1,IR1
         DO M1=1,IJ_COUNT1(I,J)
            I2=INDX1(I,J,M1)
            J2=INDY1(I,J,M1)
            U_2(I,J,1)=U_2(I,J,1)+SLP_1(I2,J2)*WTXY1(I,J,M1)
         END DO
      END DO
      END DO

      DO J=1,IT1
      DO I=1,IR1
         U_2(I,J,1)=U_2(I,J,1)/(WTSM1(I,J)+1.E-20)
      END DO
      END DO

! computer axi_symmetry storm

      SLP_2S=0.
      DO I=1,IR1
         usum=0.
         DO J=1,IT1
            usum=usum+U_2(I,J,1)
         END DO
         SLP_2S(I,1)=usum/IT1
         print*,'SLP_2S(I,1)=',I,SLP_2S(I,1)
      END DO

! check the asymmetry of the storm

!      asym_11=0.
!      asym_12=0.
!      asym_21=0.
!      asym_22=0.
!      asym_31=0.
!      asym_32=0.
!      DO I=1,IR1
!        usum=0.
!        DO J=1,IT1
!          usum=usum+abs(U_2(I,J,1)-SLP_2S(I,1))
!        END DO
!        asym_11=asym_11+usum/IT1
!        asym_12=asym_12+abs(SLP_2S(I,1))
!        asym_21=asym_21+usum/IT1*sqrt(RADUS(I))
!        asym_22=asym_22+abs(SLP_2S(I,1))*sqrt(RADUS(I))
!        asym_31=asym_31+usum/IT1*RADUS(I)
!        asym_32=asym_32+abs(SLP_2S(I,1))*RADUS(I)
!      END DO
!      print*,'MSLP asym_11,12=',asym_11,asym_12,asym_11/(asym_12+1.E-20)
!      print*,'MSLP asym_21,22=',asym_21,asym_22,asym_21/(asym_22+1.E-20)
!      print*,'MSLP asym_31,32=',asym_31,asym_32,asym_31/(asym_32+1.E-20)

! computer fact (size)

      print*,'model center pressure=',SLP_2S(1,1)

      pres_ct=(Psfc_obs-Psfc_cls)/SLP_2S(1,1)

      print*,'pres_ct=',pres_ct

      DO I=1,IR1
         SLP_2S(I,2)=SLP_2S(I,1)*pres_ct
      END DO

      IRAD_1=1
      DO I=1,IR1
         IF ( abs(SLP_2S(I,2)) .GT. 20. ) THEN !* Model ROCI ?
            IRAD_1=I
         END IF
      END DO

      RAD_1=(IRAD_1+0.5)*DR1*DST1 !* model ROCI

      fact_p=2.*PRMAX/(PRMAX+RAD_1)

      PRINT*, 'fact_p, PRMAX, RAD_1 = ', fact_p, PRMAX, RAD_1


! compute weight

      END IF !* <------------------------------------------------------------

! small correction to make the wind speed close to observation

!     v_max=0.

!     DO J=JWMIN1,JWMAX1
!     DO I=IWMIN1,IWMAX1
!        v_max1=U_1(I,J,1)*U_1(I,J,1)+V_1(I,J,1)*V_1(I,J,1)
!        if(v_max.lt.v_max1)v_max=v_max1
!     END DO
!     END DO
      v_max=v_maxk(1)/1.15

      print*,'estimate v_max_1=',v_max

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! STORM SIZE CORRECTION

      R34mod=0.
      R34modm=0.
      acount = 0.

      do j=1,4
        if ( R34_F(j).le.10. ) R34_F(j)=0.
!        if ( R34_F(j) > 10. ) then
          R34mod=R34mod+R34_F(j)
          acount = acount + 1.
          if(R34modm.lt.R34_F(j)) R34modm = R34_F(j)
!        end if
      end do
      IF ( acount > 0. ) R34mod = R34mod/acount     !* avg R34 [km]

      print*,'model storm size,R34mod,R34modm=',R34mod,R34modm

      deg2rad= pi/180.
      deg2m = deg2rad*6.371E6 !* deg ->  m
      deg2km = deg2rad*6.371E3 !* deg ->  m

      n_iter=0
      max_iter=10
      ftmin=0.85
!      ftmax=1.15
      ftmax=1.12
!      ctmin=0.85
!      ctmax=1.15
      ctmin=0.85
      ctmax=1.1
      dmin=min(150.,0.8*prmax)/deg2km
      roc_add=max(300.,0.8*prmax)/deg2km

! model RMW & ROCI

!      Rmax_0=RIJ2(i_max,j_max)*1.05
!      Rmax_0=RIJ2(i_max,j_max)*1.1
      Rmax_0=RIJ2(i_max,j_max)

      print*,'vobs,vobs_o,USCT(i_max,j_max)=',vobs,vobs_o,USCT(i_max,j_max)

!      if(USCT(i_max,j_max).gt.v64kt)then
!        roc1=R34mod
!        roc2=R34obs
!      else
!        roc1=R34modm
!        roc2=R34obsm
!      end if
!      roc1=R34modm*1.25
      roc1=R34modm
      roc2=R34obsm

      if(roc1.lt.50..or.roc2.lt.50.)then
        roc1=50.
        roc2=50.
      end if

!*    Observed RMW & ROCI
!*    ~~~~~~~~~~~~~~~~~~~
!      rmw2 = vrmax
!      roc2 = prmax

      rmw1 = max(Rmax_0/deg2km,0.01)
!      rmw2 = (0.3*Rmax_0+0.7*VRmax)/deg2km    !* target RMW  [deg]
      rmw2 = 0.5*(rmw1+VRmax/deg2km)    !* target RMW  [deg]
!      rmw2 = (0.3*rmw1+0.7*VRmax/deg2km)    !* target RMW  [deg]

      rmw_rat=rmw2/rmw1
      rmw2_limit=19./deg2km

      roc1=roc1/deg2km
      roc2=roc2/deg2km
      roc1_t=roc1
      roc2_t=roc2

      roc_rat=roc2/roc1

      roc_max=max(3.3,0.95*prmax/deg2km)

! max corection 15%
      ftmin=0.95-(Rmax_0-20.)*0.005
      if(Rmax_0.GE.40.)then
        ftmin=0.85
      else if(Rmax_0.LE.20.)then
        ftmin=0.95
      end if
!      ftmax=1.1

      IF (DEPTH.eq.'S') THEN
        ftmin=1.0-(Rmax_0-20.)*0.005
        if(Rmax_0.GE.50.)then
          ftmin=0.85
        else if(Rmax_0.LE.30.)then
          ftmin=0.95
        end if
      ELSE IF (DEPTH.eq.'M') THEN
        ftmin=1.0-(Rmax_0-20.)*0.005
        if(Rmax_0.GE.50.)then
          ftmin=0.85
        else if(Rmax_0.LE.30.)then
          ftmin=0.95
        end if
      END IF

      print*,'before adjusted storm size'
      print*,'model    rmw1,roc1=',rmw1,roc1
      print*,'observed rmw2,roc2=',rmw2,roc2

!      if(roc1.lt.15..or.roc2.lt.15..or.vobs_o.lt.v64kt)then
      if(vobs_o.le.v64kt)then
         ftmin=1.0-(Rmax_0-20.)*0.005
         if(Rmax_0.GE.50.)then
           ftmin=0.85
         else if(Rmax_0.LE.30.)then
           ftmin=0.95
         end if
!        rmw2 = 0.5*(rmw1+VRmax/deg2km)    !* target RMW  [deg]
!        if(roc2.gt.1.3*roc1)then
!          roc1=roc_add
!          roc2=roc_rat*roc1
!          if(rmw2.lt.0.9*rmw1)rmw2=0.9*rmw1
!        end if
!        if(roc1_t.gt.0.5.and.roc2_t.gt.0.5)then
!        if(rmw_rat.gt.1.0)then
!          roc2=0.5*(roc_rat+rmw_rat)*roc1
!        end if
!        end if
      end if

      if(Rmax_0.LT.19..and.VRmax.LT.19.)then
        ftmin=1.0
      end if
      if(ftmin.lt.0.85)ftmin=0.85
!      ftmax=1.15
      ftmax=1.12
      roc_rat1=max(0.7,min(1.3,roc1/roc2))
      roc1=roc_rat1*roc2

      if((roc1.lt.(rmw1+dmin)).or.(roc2.lt.(rmw2+dmin)))then
        dmax=max(rmw1-roc1,rmw2-roc2)
        roc1=roc1+dmax+dmin
        roc2=roc2+dmax+dmin
        if(roc1_t.gt.0.5.and.roc2_t.gt.0.5)then
          roc1=max(roc1,(0.7*roc_rat1+0.3*roc1/roc2)*roc2)
!          if(rmw_rat.gt.1.0.and.vobs_o.le.v64kt)then
!            roc2=0.5*(roc_rat+rmw_rat)*roc1
!          end if
        else
          roc1=roc1+dmax+roc_add
          roc2=roc2+dmax+roc_add
        end if
      end if

      print*,'dmax,roc_add=',dmax,roc_add

      print*,'adjusted storm size'
      print*,'model    rmw1,roc1=',rmw1,roc1
      print*,'observed rmw2,roc2=',rmw2,roc2

      rmw1_mod=rmw1
      roc1_mod=roc1
      rmw2_obs=rmw2
      roc2_obs=roc2

      RKX1=0.
      DO n=1,IR1
        RKX1(n)=RADUS(n)*deg2m !* deg -> m
!        print*,'RKX1(n)=',RKX1(n),U_2S(n,1)
      END DO

      k=1
      k1=2
      WKX1=0.
      DO n=1,IR1
         WKX1(n,1)=U_2S(n,1)
!
! Chanh fixed here the dateline issue for SH streamline
!
         if (CLAT_NEW.lt.0) then
          IF(WKX1(n,k).gt.0.)WKX1(n,k)=0.
         else
          IF(WKX1(n,k).lt.0.)WKX1(n,k)=0.
         endif
      END DO

      WKX2=0.
      DO n=1,IR1
         WKX2(n,1)=U_2S(n,1)*beta
!
! Chanh fixed here the dateline issue for SH streamline
!
         if (CLAT_NEW.lt.0) then
          IF(WKX2(n,1).gt.0.)WKX2(n,1)=0.
         else
          IF(WKX2(n,1).lt.0.)WKX2(n,1)=0.
         endif
      END DO

      ff0=2.*7.292E-5*sin(clat_new*pi180)

      strm1=0.
      strm3=0.
      DO n=IR,1,-1
         force=(WKX1(n,k)/(RKX1(n)+1.E-20)+ff0)*WKX1(n,k)
         strm1(n,k)=strm1(n+1,k)-force*(RKX1(n+1)-RKX1(n))
         force3=(WKX2(n,k)/(RKX1(n)+1.E-20)+ff0)*WKX2(n,k)
         strm3(n,k)=strm3(n+1,k)-force3*(RKX1(n+1)-RKX1(n))
      END DO

      SLP_MD=SLP_2S(1,1)*strm3(1,1)/(strm1(1,1)-1.E-20)+SLPE(ictr,jctr)
!      SLP_MD=SLP_1(ictr,jctr)+SLPE(ictr,jctr)

      print*,'SLP_2S(1,1),SLP_1(ictr,jctr)=',SLP_2S(1,1),SLP_1(ictr,jctr)

!      IF((SLP_MD-Psfc_obs).GE.(-100.0))THEN
!        rmw_min=min(rmw1,rmw2_obs)
!        if(rmw1.lt.1.0.and.rmw_min.lt.0.8)then
!          rmw2=rmw2_obs
!          roc2=roc2_obs
!          rmw1=rmw_min
!          roc1=min(roc1,roc2)
!        end if
!      ELSE
!        if(rmw_rat.lt.1.)then
!          roc1=max(roc2/rmw_rat,roc1)
!        else
!          rmw1=max(rmw1,rmw2)
!          roc1=max(roc1,roc2)
!        end if
!        ddd = 1./(roc1*rmw1*(roc1-rmw1))
!        aaa = (rmw2*roc1**2-rmw1**2*roc2)*ddd
!        bbb = 2.*(roc2*rmw1-roc1*rmw2)*ddd
!      END IF

!      if(roc1.gt.roc_max)roc1=roc_max
!      if(roc2.gt.roc_max)roc2=roc_max
      IF(roc1.gt.roc_max.or.roc2.gt.roc_max)THEN
        rocx_rat=roc_max/max(roc1,roc2)
        roc1=rocx_rat*roc1
        roc2=rocx_rat*roc2
        if(roc2.gt.roc1)roc2=0.5*(roc1+roc2)
      ELSE IF(roc1.lt.2.5.or.roc2.lt.2.5)THEN
        rocx_rat=5./(roc1+roc2)
        roc1=rocx_rat*roc1
        roc2=rocx_rat*roc2
        if(roc2.gt.roc1)roc2=0.5*(roc1+roc2)
      ELSE
        if(roc2.gt.roc1)roc2=0.5*(roc1+roc2)
      END IF

      rmw2_0=rmw2
      roc2_0=roc2

       REWIND(65)
       READ(65)RMN_HWRF
       IF(RMN_HWRF.lt.(PRMAX/deg2km))THEN
         if(roc1.gt.roc2)roc2=roc1
         if(rmw1.gt.rmw2)rmw2=rmw1
       END IF

      xxx  = ftmin*rmw1 ; yyy = ftmax*rmw1  !* 50% Constraint
      rmw2 =  max(xxx,min(rmw2,yyy))   !* for bogus stretch
      xxx  = ctmin*roc1 ; yyy = ctmax*roc1  !* 50% Constraint
      roc2 =  max(xxx,min(roc2,yyy))   !* for bogus stretch

!      if(beta.lt.1.)then
!        if(roc1_t.gt.0.5.and.roc2_t.gt.0.5)then
!          if(rmw2.gt.rmw1)rmw2=rmw1
!          if(roc2.gt.roc1)roc2=roc1
!        end if
!      end if

      ddd = 1./(roc1*rmw1*(roc1-rmw1))
      aaa = (rmw2*roc1**2-rmw1**2*roc2)*ddd
      bbb = 2.*(roc2*rmw1-roc1*rmw2)*ddd

      print*,'model    rmw1,roc1=',rmw1,roc1
      print*,'observed rmw2,roc2=',rmw2,roc2
      print*,'First Guess: aaa,bbb=',aaa,bbb

      if(bbb.gt.0.1)then
        bbb=0.1
        roc2=0.5*bbb*roc1*(roc1-rmw1)+roc1*rmw2/rmw1
        aaa=(rmw2*roc1**2-rmw1**2*roc2)*ddd
      else if(bbb.lt.(-0.1))then
        bbb=-0.1
        roc2=0.5*bbb*roc1*(roc1-rmw1)+roc1*rmw2/rmw1
        aaa=(rmw2*roc1**2-rmw1**2*roc2)*ddd
      end if

!      bbb = max(-0.3,min(bbb,0.3))
      aaa = max( 0.85, min(aaa,1.15) )

      print*,'adjusted aaa,bbb=',aaa,bbb

      RKX2=0.
      DO n=1,IR1
        RADIUS1(n) = aaa*RADUS(n) + .5*bbb*RADUS(n)**2
        RKX2(n) = RADIUS1(n)*deg2m  !* deg -> m
      END DO

      k=1
      k1=2
      strm2=0.
      DO n=IR,1,-1
         force2=(WKX2(n,k)/(RKX2(n)+1.E-20)+ff0)*WKX2(n,k)
         strm2(n,k)=strm2(n+1,k)-force2*(RKX2(n+1)-RKX2(n))
      END DO

      SLP_T=SLP_2S(1,1)*strm2(1,1)/(strm1(1,1)-1.E-20)+SLPE(ictr,jctr)

      print*,'SLP_2S(1,1),SLPE(ictr,jctr)=',SLP_2S(1,1),SLPE(ictr,jctr)
      print*,'SLP_T,Psfc_obs,=',SLP_T,Psfc_obs
      print*,'strm2(1,1),strm1(1,1)=',strm2(1,1),strm1(1,1)

      diff_p=SLP_T-Psfc_obs
      diff_p_x=200.
      diff_p_n=-200.
      it_flag=3

      IF(diff_p.lt.diff_p_n)THEN

 969  CONTINUE

      n_iter=n_iter+1

      xxx  = ftmin*rmw1 ; yyy = ftmax*rmw1  !* 50% Constraint
      rmw2 =  max(xxx,min(rmw2,yyy))   !* for bogus stretch
      xxx  = ctmin*roc1 ; yyy = ctmax*roc1  !* 50% Constraint
      roc2 =  max(xxx,min(roc2,yyy))   !* for bogus stretch

      ddd = 1./(roc1*rmw1*(roc1-rmw1))
      aaa = (rmw2*roc1**2-rmw1**2*roc2)*ddd
      bbb = 2.*(roc2*rmw1-roc1*rmw2)*ddd

      print*,'final adjusted storm size', n_iter
      print*,'model    rmw1,roc1=',rmw1,roc1
      print*,'observed rmw2,roc2=',rmw2,roc2

      print*,'aaa,bbb=',aaa,bbb

      bbb = max(-0.1,min(bbb,0.1))
      aaa = max( 0.85, min(aaa,1.15) )

      print*,'after aconstraint: aaa,bbb=',aaa,bbb

      if(n_iter.le.max_iter)then

      RKX2=0.
      DO n=1,IR1
        RADIUS1(n) = aaa*RADUS(n) + .5*bbb*RADUS(n)**2
        RKX2(n) = RADIUS1(n)*deg2m  !* deg -> m
      END DO

      k=1
      k1=2
      strm2=0.
      DO n=IR,1,-1
         force2=(WKX2(n,k)/(RKX2(n)+1.E-20)+ff0)*WKX2(n,k)
         strm2(n,k)=strm2(n+1,k)-force2*(RKX2(n+1)-RKX2(n))
      END DO
! need to define variables

       SLP_T=SLP_2S(1,1)*strm2(1,1)/(strm1(1,1)-1.E-20)+SLPE(ictr,jctr)
       DELT_P=(Psfc_cls-Psfc_obs)
       diff_p=SLP_T-Psfc_obs

       print*,'SLP_2S(1,1),strm2(1,1),strm1(1,1),SLPE(ictr,jctr)=',    &
              SLP_2S(1,1),strm2(1,1),strm1(1,1),SLPE(ictr,jctr)
       print*,'SLP_T,Psfc_obs,Psfc_cls,DELT_P=',SLP_T,Psfc_obs,Psfc_cls,DELT_P

       if(diff_p.gt.diff_p_x.and.it_flag.eq.1)then
         ftmax=ftmax+0.005
         rmw1_new=ftmax*rmw1
         if(rmw1_new.gt.rmw2_0)then
           rmw2=min(rmw1_new,(1.+0.005*n_iter)*rmw2_0)
         end if
         ctmax=ctmax+0.01
         roc1_new=ctmax*roc1
         if(roc1_new.gt.roc2_0)then
           roc2=min(roc1_new,(1.+0.01*n_iter)*roc2_0)
         end if
         go to 969
       else if (diff_p.lt.diff_p_n.and.it_flag.eq.2)then
!         if(Rmax_0.gt.40.)then
           ftmin=ftmin-0.005
           rmw1_new=ftmin*rmw1
           if(rmw1_new.lt.rmw2_0)then
             rmw2=max(rmw1_new,(1.-0.005*n_iter)*rmw2_0)
             rmw2=max(rmw2_limit,rmw2)
           end if
!         end if
         ctmin=ctmin-0.02
         roc1_new=ctmin*roc1
         if(roc1_new.lt.roc2_0)then
           roc2=max(roc1_new,(1.-0.02*n_iter)*roc2_0)
         end if
         go to 969
       end if

       end if

       END IF


       print*,'aaa,bbb=',aaa,bbb
       print*,'ftmin,ftmax,ctmin,ctmax=',ftmin,ftmax,ctmin,ctmax
       print*,'rmw2_0,roc2_0=',rmw2_0,roc2_0
       print*,'model storm size,R34mod,R34modm=',R34mod,R34modm
       print*,'observed storm size,R34obs,R34obsm=',R34obs,R34obsm
       print*,'rmw1,rmw2,roc1,roc2=',rmw1,rmw2,roc1,roc2
       print*,'SLP_T,Psfc_obs,Psfc_cls=',SLP_T,Psfc_obs,Psfc_cls

!       save aaa,bbb,rmw1,rmw2,   will be used in size correction.


      scale1=1.0             ! qingfu test
      beta_ct=1.0      ! qingfu test

      print*,'correction factor scale,beta_ct=',scale1,beta_ct


      beta11=beta_ct*scale1
      fact=1.0

      U_2S1=0.
      DO K=1,KM1
      DO I=1,IR1
         U_2S1(I,K)=U_2S(I,K)*beta11
      END DO
      END DO

      DO I=1,IR1
!        RADIUS1(I)=RADUS(I)*DST*1.E3
         RADIUS1(I)=RADUS(I)
      END DO

      print*,'before call CORT_MAT, beta11=',beta11

!      if(abs(beta11-1.).gt.0.01)then

      CALL CORT_MAT(IR1,NX,NY,NZ,KMX,U_2S,T_2S,Q_2S,SLP_2S(1,1),     &
                    U_2S1,T_X,Q_X,SLP_X,HLON2,HLAT2,VLON2,VLAT2,     &
                    TEK,PCST,RADUS,RADIUS1,CLON_NEW,CLAT_NEW,        &
                    CFT,beta11,IR_1,IFLAG,KM1)

      print*,'after call CORT_MAT'

      IF (INITOPT .EQ. 0) THEN
!$omp parallel do &
!$omp& private(i,j,k,TEK1,TEK2,ESRR)
          DO J=1,NY
          DO I=1,NX
             SLP_1(I,J)=SLP_1(I,J)*beta11+SLP_X(I,J)
             DO K=1,KMX
	        TEK1=TENV(I,J,K)+T_1(I,J,K)
                T_1(I,J,K)=T_1(I,J,K)*beta11+T_X(I,J,K)
    !           Q_1(I,J,K)=Q_1(I,J,K)*beta11+Q_X(I,J,K)
                TEK2=TENV(I,J,K)+T_1(I,J,K)
                ESRR=exp(4302.645*(TEK2-TEK1)/((TEK2-29.66)*(TEK1-29.66)))
	        Q_1(I,J,K)=ESRR*Q_1(I,J,K)
             END DO
          END DO
          END DO
      END IF
!
! save data

      print*,'IR1,IR_1,NX,NY,NZ,KMX=',IR1,IR_1,NX,NY,NZ,KMX
      print*,'RADUS=',RADUS(1),RADUS(2)

! conver Q_2S to orginial unit

      DO K=1,KMX
      DO I=1,IR1
         Q_2S(I,K)=Q_2S(I,K)*1.E-3
      END DO
      END DO

      write(23)IR1,IR_1,NX,NY,NZ,KMX
      write(23)RADUS
      write(23)CLON_NEW,CLAT_NEW
      write(23)U_2S,V_2S,U_2S1
      write(23)T_2S,Q_2S,SLP_2S
      write(23)CFT,PW,beta11
      write(23)TEK,HP
      write(23)aaa,bbb,SLP_T
      write(23)rmw1,rmw2,roc1,roc2
      write(23)ftmin,ftmax,ctmin,ctmax

      v_max=0.

      DO J=JWMIN1,JWMAX1
      DO I=IWMIN1,IWMAX1
         v_max1=U_1(I,J,1)*U_1(I,J,1)+V_1(I,J,1)*V_1(I,J,1)
         if(v_max.lt.v_max1)v_max=v_max1
      END DO
      END DO
      v_max=sqrt(v_max)/1.15

      print*,'v_max_2=',v_max


      write(14,*)fact_p,PRMAX,RAD_1  !* Add PRMAX,RAD_1

      if(INITOPT > 0)PW=1.

      do k=1,kmx
      do J=1,NY
      do I=1,NX
         U_1(I,J,K)=U_1(I,J,K)*PW(k)
         V_1(I,J,K)=V_1(I,J,K)*PW(k)
         T_1(I,J,K)=T_1(I,J,K)*PW(k)
         Q_1(I,J,K)=Q_1(I,J,K)*PW(k)
      end do
      end do
      end do


      print*,'test8='

      NCHT=58
      WRITE(NCHT)KSTM

      WRITE(NCHT)HLAT2,HLON2
      WRITE(NCHT)VLAT2,VLON2

      WRITE(NCHT)PCST

      WRITE(NCHT)HP

      KST=1

      WRITE(NCHT)ST_NAME(KST)
      WRITE(NCHT)CLON_NEW,CLAT_NEW

      WRITE(NCHT)zmax

      WRITE(NCHT)IWMIN1,IWMAX1,JWMIN1,JWMAX1
      WRITE(NCHT)((SLPE(I,J),I=1,NX),J=1,NY)          ! SLP
      WRITE(NCHT)((SLP_1(I,J),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)    ! pert SLP
      PRINT*,'TEST1'
      DO K=1,KMX
         WRITE(NCHT)((TENV(I,J,K),I=1,NX),J=1,NY)
         WRITE(NCHT)((T_1(I,J,K),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
      END DO
      DO K=1,KMX
         WRITE(NCHT)((U_1(I,J,K),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
         WRITE(NCHT)((V_1(I,J,K),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
      END DO

      DO K=1,KMX
         WRITE(NCHT)((Q_1(I,J,K),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
      END DO

      WRITE(NCHT) ((U_850(I,J),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)
      WRITE(NCHT) ((V_850(I,J),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)

      CLOSE(NCHT)

! grads output

      NX2=NX*2-1

      ALLOCATE( DUM(NX,NY) )
      ALLOCATE( DUMA(NX2,NY) )

!     call open_grads('storm_pert_out',NX2,NY,KMX,1.0,1.0,1.0,1.0)
      call open_grads('storm_pert_out',NX,NY,KMX,1.0,1.0,1.0,1.0)

      open(91,file='storm_pert_out',form='unformatted')
      rewind 91

      do K=1,KMX
          call load(U_1,NX,NY,KMX,K,DUM)
          write(91) DUM
!         call fill_nmm_gridg(DUM,NX,NY,DUMA,2)
!         write(91) DUMA
      end do
      do K=1,KMX
          call load(V_1,NX,NY,KMX,K,DUM)
          write(91) DUM
!         call fill_nmm_gridg(DUM,NX,NY,DUMA,2)
!         write(91) DUMA
      end do
      do K=1,KMX
          call load(T_1,NX,NY,KMX,K,DUM)
          write(91) DUM
!         call fill_nmm_gridg(DUM,NX,NY,DUMA,2)
!         write(91) DUMA
      end do
      do K=1,KMX
          call load(Q_1,NX,NY,KMX,K,DUM)
          write(91) DUM
!         call fill_nmm_gridg(DUM,NX,NY,DUMA,2)
!         write(91) DUMA
      end do
      write(91) SLP_1
!     call fill_nmm_gridg(SLP_1,NX,NY,DUMA,1)
!     write(91) DUMA

      close(91)

      DEALLOCATE( DUM, DUMA )

! grads output

!     WRITE(35)((SLP_1(I,J),I=1,NX),J=1,NY,2)
!     DO K=1,KMX
!        WRITE(35)((T_1(I,J,K),I=1,NX),J=1,NY,2)
!     END DO
!     DO K=1,KMX
!        WRITE(35)((U_1(I,J,K),I=1,NX),J=1,NY,2)
!     END DO
!     DO K=1,KMX
!        WRITE(35)((V_1(I,J,K),I=1,NX),J=1,NY,2)
!     END DO
!     DO K=1,KMX
!        WRITE(35)((Q_1(I,J,K),I=1,NX),J=1,NY,2)
!     END DO

      END

!==============================================================================
