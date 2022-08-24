!??????????????????????????????????????????????????????????
!
! ABSTRACT: Split the vortex into environment flow and vortex perturbation
!
! ORIGINAL AUTHOR: QINGFU LIU, NCEP/EMC, 2007
! REVISED  AUTHOR: Qingfu Liu, 2013
!                : Changed the filter domain size
!
!     DECLARE VARIABLES
!
      INTEGER I,J,K,NX,NY,NZ,IBGS,IVOBS
      INTEGER KK
!      REAL(4) DX_R,DY_R,PT,PDTOP,GAMMA
      REAL(4) LON1,LAT1,LON2,LAT2,GAMMA
      REAL(4) crfactor
!
!      PARAMETER (NX=215,NY=431,NZ=42)
      PARAMETER (NST=10)
      PARAMETER (GAMMA=6.5E-3,G=9.8,GI=1./G,D608=0.608)
!
! Variables on hybrid coordinate

      REAL(4), ALLOCATABLE :: T1(:,:,:),Q1(:,:,:)
      REAL(4), ALLOCATABLE :: U1(:,:,:),V1(:,:,:),DZDT(:,:,:)
      REAL(4), ALLOCATABLE :: Z1(:,:,:),P1(:,:,:)
      REAL(4), ALLOCATABLE :: GLON(:,:),GLAT(:,:)
      REAL(4), ALLOCATABLE :: PD(:,:),ETA1(:),ETA2(:)
      REAL(4), ALLOCATABLE :: HGTSFC3(:,:)

      REAL(4), ALLOCATABLE :: US850(:,:),VS850(:,:)

      REAL(4), ALLOCATABLE :: HLON(:,:),HLAT(:,:)
      REAL(4), ALLOCATABLE :: VLON(:,:),VLAT(:,:)

      REAL(4), ALLOCATABLE :: USC(:,:),VSC(:,:)          ! wind at z=0
      REAL(4), ALLOCATABLE :: TMV1(:,:)


      REAL(4),ALLOCATABLE :: ZM1(:,:,:),PM1(:,:,:),HV(:,:,:)
      REAL(4),ALLOCATABLE :: ZMV1(:,:,:),PMV1(:,:,:)
      REAL(4),ALLOCATABLE :: TV1(:,:,:)
!
! Variables on P surface

      REAL(4),ALLOCATABLE :: T2(:,:,:),Q2(:,:,:)
      REAL(4),ALLOCATABLE :: U2(:,:,:),V2(:,:,:)
      REAL(4),ALLOCATABLE :: P2(:),SLP(:,:),Z2(:,:)
      REAL(4),ALLOCATABLE :: SIG(:)
      REAL(4),ALLOCATABLE :: RH(:,:)
      REAL(4),ALLOCATABLE :: HP(:,:,:)

      REAL(4),ALLOCATABLE :: UVTQ(:,:,:,:)
      REAL(4),ALLOCATABLE :: work_1(:),work_2(:)

      real(4) SLON_N,SLAT_N,CLON_N,CLAT_N

      REAL(4) CENTRAL_LON,CENTRAL_LAT,WBD,SBD

      COMMON /NHC/ KSTM,IC_N(NST),JC_N(NST)
      COMMON /NHC1/ SLON_N(NST),SLAT_N(NST),CLON_N(NST),CLAT_N(NST)
      CHARACTER ST_NAME(NST)*3,STMNAME(NST)*3,TCVT(NST)*95
      COMMON /STNAME/ST_NAME,STMNAME
      COMMON /TCVIT/TCVT
      COMMON /RSFC/STRPSF(NST),STVMAX(NST),STRPSF_06(NST)
      integer, external :: omp_get_max_threads

      integer :: ICLAT,ICLON,CLAT_VIT,CLON_VIT
      character :: SN,EW

333   format('Have ',I0,' OpenMP threads.')
      print 333,omp_get_max_threads()
!
      COEF3=287.05*GI*GAMMA
      COEF2=1./COEF3

      pi=4.*atan(1.)
      pi_deg=180./pi
! read data

      READ(5,*)ITIM,IBGS,IVOBS,iflag_cold,crfactor

      IUNIT=26
      KUNIT=56

! read in storm center
      read(11,11)ICLAT,SN,ICLON,EW
  11  format(33x,I3,A1,I5,A1)
      close(11)

      CLAT_VIT=ICLAT*0.1
      CLON_VIT=ICLON*0.1

      IF(SN.eq.'S')CLAT_VIT=-CLAT_VIT
      IF(EW.eq.'W')CLON_VIT=-CLON_VIT

!      I360=180
!      if(abs(CLON_VIT).gt.90.)then
!         I360=360
!      end if
! For HAFS, I360 is newly defined to handle the storms in eastern/western hemisphere 2022 July
      IF(EW.eq.'W') I360=180 ! Western hemisphere TC
      IF(EW.eq.'E') I360=360 ! Eastern hemisphere TC 

      READ(IUNIT) NX,NY,NZ

      print*,'NX,NY,NZ,I360=',NX,NY,NZ,I360
      IF ( NZ <= 60 ) THEN
        KMX=2*NZ+1
      ELSE
! Warning: currently hard wired to 121 due to memory limit
        KMX=121
      ENDIF
      KMAX=KMX
      NX1=NX+1
      NY1=NY+1
      NZ1=NZ+1


      ALLOCATE ( T1(NX,NY,NZ),Q1(NX,NY,NZ) )
      ALLOCATE ( U1(NX,NY,NZ),V1(NX,NY,NZ),DZDT(NX,NY,NZ) )
      ALLOCATE ( Z1(NX,NY,NZ1),P1(NX,NY,NZ1) )
      ALLOCATE ( GLON(NX,NY),GLAT(NX,NY) )
      ALLOCATE ( PD(NX,NY),ETA1(NZ1),ETA2(NZ1) )

      ALLOCATE ( HLON(NX,NY),HLAT(NX,NY) )
      ALLOCATE ( VLON(NX,NY),VLAT(NX,NY) )

      ALLOCATE ( US850(NX,NY),VS850(NX,NY) )

      ALLOCATE ( USC(NX,NY),VSC(NX,NY) )          ! wind at z=0
      ALLOCATE ( TMV1(NX,NY) )

      ALLOCATE ( ZM1(NX,NY,NZ),PM1(NX,NY,NZ),TV1(NX,NY,NZ) )
      ALLOCATE ( ZMV1(NX,NY,NZ),PMV1(NX,NY,NZ) )

      READ(IUNIT) LON1,LAT1,LON2,LAT2,CENTRAL_LON,CENTRAL_LAT
      READ(IUNIT) PM1
      READ(IUNIT) T1
      READ(IUNIT) Q1
      READ(IUNIT) U1
      READ(IUNIT) V1
      READ(IUNIT) DZDT           ! new
      READ(IUNIT) Z1
      READ(IUNIT) HLON,HLAT,VLON,VLAT
      READ(IUNIT) P1
      READ(IUNIT) PD           ! surface pressure
      READ(IUNIT) ETA1
      READ(IUNIT) ETA2
      READ(IUNIT) !land
      READ(IUNIT) !sfrc
      READ(IUNIT) !C101

      CLOSE(IUNIT)

!
        DO J=1,NY
         DO I=1,NX
           GLON(I,J)=HLON(I,J)            ! LON & LAT at T,Q P Z (H) points
           GLAT(I,J)=HLAT(I,J)
         END DO
        END DO

      print*,'CENTRAL_LON,CENTRAL_LAT=',CENTRAL_LON,CENTRAL_LAT

      write(*,*)'K,T1,Q1,U1,V1,Z1,P1='
      do k=1,nz
        write(*,32)K,T1(9,9,K),
     &     Q1(9,9,K),U1(9,9,K),V1(9,9,K),Z1(9,9,K),P1(9,9,K)
      end do
        write(*,*)

      ICT1=1+(NX-1)/2
      JCT1=1+(NY-1)/2

      SDT=1.E20
      DO K=1,NZ
       SDIF=abs(P1(ICT1,JCT1,K)/P1(ICT1,JCT1,1)-0.85)
       IF(SDIF.LT.SDT)THEN
         KS850=K
         SDT=SDIF
       END IF
      END DO

      DO J=1,NY
      DO I=1,NX
        US850(I,J)=U1(I,J,KS850)
        VS850(I,J)=V1(I,J,KS850)
      END DO
      END DO

 32   format(I3,6F12.2)

!
      write(*,*)'PT,PDTOP=',PT,PDTOP
      do k=1,nz1
        write(*,*)'K,ETA1,ETA2=',K,ETA1(k),ETA2(k)
      end do
       print*,'CLON,CLAT=',GLON(1+(NX-1)/2,1+(NY-1)/2),
     &                     GLAT(1+(NX-1)/2,1+(NY-1)/2)
!      write(77,33)HLON
!      write(77,*)
!      write(77,33)HLAT
!      write(77,*)
!      write(77,34)PD
! 33   format(15F8.1)
! 34   format(10F12.1)

!      DO K=1,NZ+1
!        WRITE(61)((Z1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ+1
!        WRITE(61)((P1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(61)((T1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(61)((Q1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(61)((U1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,NZ
!        WRITE(61)((V1(I,J,K),I=1,NX),J=1,NY,2)
!      END DO


      ALLOCATE ( T2(NX,NY,KMX),Q2(NX,NY,KMX) )
      ALLOCATE ( U2(NX,NY,KMX),V2(NX,NY,KMX) )
      ALLOCATE ( P2(KMX),SLP(NX,NY),Z2(NX,NY),SIG(KMX) )
      ALLOCATE ( HP(NX,NY,KMX),RH(NX,NY) )
      ALLOCATE ( HV(NX,NY,KMX) )

      ALLOCATE ( UVTQ(NX,NY,KMX,4) )

! Surface at K=1, top at K=NZ

!$omp parallel do
!$omp& private(i,j,k)
      DO K=1,NZ
      DO J=1,NY
      DO I=1,NX
        TV1(I,J,K)=T1(I,J,K)*(1.+D608*Q1(I,J,K))
      END DO
      END DO
      END DO

!$omp parallel do
!$omp& private(i,j,k)
      DO K=1,NZ
      DO J=1,NY
      DO I=1,NX
        ZM1(I,J,K)=(Z1(I,J,K)+Z1(I,J,K+1))*0.5+
     &     0.5*TV1(I,J,K)/GAMMA*(2.-(P1(I,J,K)/PM1(I,J,K))**COEF3-
     &     (P1(I,J,K+1)/PM1(I,J,K))**COEF3)
!        ZM1(I,J,K)=(Z1(I,J,K)+Z1(I,J,K+1))*0.5
!        PM1(I,J,K)=EXP((ALOG(1.*P1(I,J,K))+ALOG(1.*P1(I,J,K+1)))*0.5)
      END DO
      END DO
      END DO

      ZMV1=ZM1
      PMV1=PM1
!!$omp parallel do
!1$omp& private(i,j,k)
!      DO J=2,NY-1
!        IF(MOD(J,2).NE.0.)THEN
!           DO K=1,NZ
!           DO I=1,NX-1
!             ZMV1(I,J,K)=0.25*(ZM1(I,J,K)+ZM1(I+1,J,K)+
!     &                         ZM1(I,J-1,K)+ZM1(I,J+1,K))
!             PMV1(I,J,K)=0.25*(PM1(I,J,K)+PM1(I+1,J,K)+
!     &                         PM1(I,J-1,K)+PM1(I,J+1,K))
!           END DO
!           END DO
!        ELSE
!           DO K=1,NZ
!           DO I=2,NX
!             ZMV1(I,J,K)=0.25*(ZM1(I-1,J,K)+ZM1(I,J,K)+
!     &                         ZM1(I,J-1,K)+ZM1(I,J+1,K))
!             PMV1(I,J,K)=0.25*(PM1(I-1,J,K)+PM1(I,J,K)+
!     &                         PM1(I,J-1,K)+PM1(I,J+1,K))
!           END DO
!           END DO
!        END IF
!      END DO

      DO J=1,NY
      DO I=1,NX
        TMV1(I,J)=TV1(I,J,1)
      END DO
      END DO
!      K=1
!!$omp parallel do
!!$omp& private(i,j)
!      DO J=2,NY-1
!        IF(MOD(J,2).NE.0.)THEN
!           DO I=1,NX-1
!             TMV1(I,J)=0.25*(TV1(I,J,K)+TV1(I+1,J,K)+
!     &                       TV1(I,J-1,K)+TV1(I,J+1,K))
!           END DO
!         ELSE
!           DO I=2,NX
!             TMV1(I,J)=0.25*(TV1(I-1,J,K)+TV1(I,J,K)+
!     &                       TV1(I,J-1,K)+TV1(I,J+1,K))
!           END DO
!         END IF
!       END DO


! MAX Surface P
      PSFX=0.
      DO J=1,NY
      DO I=1,NX
        IF(PSFX.LT.P1(I,J,1))THEN
           PSFX=P1(I,J,1)
           IPX=I
           JPX=J
        END IF
      END DO
      END DO

      PRINT*,'IPX,JPX,PSFX=',IPX,JPX,PSFX

! constant P
! Double resolution for low levels
      KK=0
      DO K=1,KMX-NZ-1
        KK=2*K-1
        P2(KK)=P1(IPX,JPX,K)
        SIG(KK)=P1(IPX,JPX,K)/PSFX
      END DO
      DO K=1,KMX-NZ-1
        KK=2*K
        P2(KK)=PM1(IPX,JPX,K)
        SIG(KK)=PM1(IPX,JPX,K)/PSFX
      END DO
      DO K=KMX-NZ,NZ+1
        KK=KK+1
        P2(KK)=P1(IPX,JPX,K)
        SIG(KK)=P1(IPX,JPX,K)/PSFX
      END DO

! Height at P2 grids
!$omp parallel do
!$omp& private(i,j,k,n)
        DO J=1,NY
        DO I=1,NX
        CYC_10: DO K=1,KMX
          IF(P2(K).GE.PM1(I,J,1))THEN
            HP(I,J,K)=ZM1(I,J,1)+
     &        TV1(I,J,1)/GAMMA*(1.-(P2(K)/PM1(I,J,1))**COEF3)
          ELSE IF(P2(K).LE.PM1(I,J,NZ))THEN
            HP(I,J,K)=ZM1(I,J,NZ)+
     &        TV1(I,J,NZ)/GAMMA*(1.-(P2(K)/PM1(I,J,NZ))**COEF3)
          ELSE
            DO N=1,NZ-1
              IF(P2(K).LE.PM1(I,J,N).and.P2(K).GT.PM1(I,J,N+1))THEN
                HP(I,J,K)=ZM1(I,J,N)+
     &            TV1(I,J,N)/GAMMA*(1.-(P2(K)/PM1(I,J,N))**COEF3)
                CYCLE CYC_10
              END IF
            END DO
          END IF
        END DO CYC_10
        END DO
        END DO


       HV=HP
!!$omp parallel do
!!$omp& private(i,j,k)
!       DO J=2,NY-1
!         IF(MOD(J,2).NE.0.)THEN
!           DO K=1,KMX
!           DO I=1,NX-1
!             HV(I,J,K)=0.25*(HP(I,J,K)+HP(I+1,J,K)+
!     &                       HP(I,J-1,K)+HP(I,J+1,K))
!           END DO
!           END DO
!         ELSE
!           DO K=1,KMX
!           DO I=2,NX
!             HV(I,J,K)=0.25*(HP(I-1,J,K)+HP(I,J,K)+
!     &                       HP(I,J-1,K)+HP(I,J+1,K))
!           END DO
!           END DO
!         END IF
!       END DO

        K=1
!$omp parallel do
!$omp& private(i,j,DTEMP,ES,QS1)
        DO J=1,NY
        DO I=1,NX
          DTEMP=T1(I,J,K)-273.15
          ES=611.2*EXP(17.67*DTEMP/(DTEMP+243.5))
          QS1=0.622*ES/(PM1(I,J,K)-0.378*ES)
          RH(I,J)=MIN(MAX(Q1(I,J,K)/QS1,0.),1.0)
!          IF(I*J.LT.20)print*,'ES,PM1=',I,J,ES,PM1(I,J,K),QS1,RH(I,J)
        END DO
        END DO

! Iterpolation to constant P
!$omp parallel do
!$omp& private(i,j,k,N,DTEMP,ES,QSK,W1,W)
      DO J=1,NY
      DO I=1,NX
        CYC_12: DO K=1,KMX
          IF(P2(K).GE.PM1(I,J,1))THEN            ! Below PM1(I,J,1)
            T2(I,J,K)=T1(I,J,1)-GAMMA*(HP(I,J,K)-ZM1(I,J,1))
            DTEMP=T2(I,J,K)-273.15
            ES=611.2*EXP(17.67*DTEMP/(DTEMP+243.5))
            QSK=0.622*ES/(P2(K)-0.378*ES)
            Q2(I,J,K)=RH(I,J)*QSK            ! constant RH below Nz=1
          ELSE IF(P2(K).LE.PM1(I,J,NZ))THEN
            T2(I,J,K)=T1(I,J,NZ)-GAMMA*(HP(I,J,K)-ZM1(I,J,NZ))
            Q2(I,J,K)=Q1(I,J,NZ)             ! very small
          ELSE
            DO N=1,NZ-1
              IF(P2(K).LE.PM1(I,J,N).and.P2(K).GT.PM1(I,J,N+1))THEN
                W1=ALOG(1.*PM1(I,J,N+1))-ALOG(1.*PM1(I,J,N))
                W=(ALOG(1.*P2(K))-ALOG(1.*PM1(I,J,N)))/W1
                T2(I,J,K)=T1(I,J,N)+(T1(I,J,N+1)-T1(I,J,N))*W
                Q2(I,J,K)=Q1(I,J,N)+(Q1(I,J,N+1)-Q1(I,J,N))*W
                CYCLE CYC_12
              END IF
            END DO
          END IF
        END DO CYC_12
      END DO
      END DO

!$omp parallel do
!$omp& private(i,j,k,N,W1,W)
      DO J=1,NY
      DO I=1,NX
        CYC_14: DO K=1,KMX
          IF(P2(K).GE.PMV1(I,J,1))THEN            ! Below PMV1(I,J,1)
!            U2(I,J,K)=U1(I,J,1)*(1.-(P2(K)-PMV1(I,J,1))*1.4E-5)
!            V2(I,J,K)=V1(I,J,1)*(1.-(P2(K)-PMV1(I,J,1))*1.4E-5)
            U2(I,J,K)=U1(I,J,1)
            V2(I,J,K)=V1(I,J,1)
          ELSE IF(P2(K).LE.PMV1(I,J,NZ))THEN
            U2(I,J,K)=U1(I,J,NZ)
            V2(I,J,K)=V1(I,J,NZ)
          ELSE
            DO N=1,NZ-1
              IF(P2(K).LE.PMV1(I,J,N).and.P2(K).GT.PMV1(I,J,N+1))THEN
                W1=ALOG(1.*PMV1(I,J,N+1))-ALOG(1.*PMV1(I,J,N))
                W=(ALOG(1.*P2(K))-ALOG(1.*PMV1(I,J,N)))/W1
                U2(I,J,K)=U1(I,J,N)+(U1(I,J,N+1)-U1(I,J,N))*W
                V2(I,J,K)=V1(I,J,N)+(V1(I,J,N+1)-V1(I,J,N))*W
                CYCLE CYC_14
              END IF
            END DO
          END IF
        END DO CYC_14
      END DO
      END DO

      write(*,*)'IPX,JPX,PSFX=',IPX,JPX,PSFX
      do k=1,kmx
        write(*,*)'K,P2,SIG=',K,P2(K),SIG(K)
      end do
        write(*,*)
        write(*,*)'K,T2,Q2,U2,V2,Z2,P2='
      do k=1,kmx
        write(*,32)K,T2(9,9,K),
     &     Q2(9,9,K),U2(9,9,K),V2(9,9,K),HP(9,9,K),P2(K)
      end do

!      DO K=1,KMX
!        WRITE(81)((U2(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,KMX
!        WRITE(81)((V2(I,J,K),I=1,NX),J=1,NY,2)
!      END DO


! save for later use
      DO J=1,NY
      DO I=1,NX
      DO K=1,KMX
        UVTQ(I,J,K,1)=U2(I,J,K)
        UVTQ(I,J,K,2)=V2(I,J,K)
        UVTQ(I,J,K,3)=T2(I,J,K)
        UVTQ(I,J,K,4)=Q2(I,J,K)
      END DO
      END DO
      END DO

!C     MSLP: LOOP OVER HORIZONTAL GRID.
!C
       DO J=1,NY
       DO I=1,NX
         ZSFC = Z1(I,J,1)
         PSFC = P1(I,J,1)
         TSFC = TV1(I,J,1)+GAMMA*(ZM1(I,J,1)-Z1(I,J,1))
!C
!C        COMPUTE SEA LEVEL PRESSURE.
         A = (GAMMA * ZSFC) / TSFC
         SLP(I,J) = PSFC*(1+A)**COEF2
         Z2(I,J)=Z1(I,J,1)
      ENDDO
      ENDDO


       print*,'call before HURR_MESS'

       CALL HURR_MESS(ITIM,IBGS,NX,NY,GLON,GLAT,I360)

       print*,'call after HURR_MESS'


       NCHT=71
       WRITE(NCHT)KSTM

       WRITE(NCHT)HLAT,HLON
       WRITE(NCHT)VLAT,VLON

       WRITE(NCHT)P2
       WRITE(NCHT)HP

!C   Relocate Hurricane

!       DO KST=1,KSTM
       DO KST=1,1

         print*,'KST=',KST

         CALL wrf_move(ITIM,KST,GLON,GLAT,US850,VS850,
     &      KS850,P2,
     &      Z2,T2,Q2,U2,V2,SLP,SIG,HLAT,HLON,VLAT,VLON,
     &  KMX,1,NX,1,NY,IBGS,IVOBS,iflag_cold,I360,crfactor)

        END DO

       print*,'test7'

       DO J=1,NY
       DO I=1,NX
         ZSFC = Z1(I,J,1)
         TSFC = T2(I,J,1)*(1.+D608*Q2(I,J,1))
         A = (GAMMA * ZSFC) / TSFC
         P1(I,J,1) = SLP(I,J)*(1-A)**COEF2
         PD(I,J)=P1(I,J,1)
      ENDDO
      ENDDO


       allocate (work_1(nz),work_2(nz+1))
       DO J=1,NY
       DO I=1,NX
          call get_eta_level(nz,PD(I,J),work_1,work_2,eta1,eta2,1.0)
          do k=1,nz
             n=nz-k+1
             PM1(I,J,K)=work_1(n)
          end do
          do k=1,nz+1
             n=nz-k+2
            P1(I,J,K)=work_2(n)
          end do
      ENDDO
      ENDDO
      deallocate (work_1,work_2)

      do j = 1,ny
      do i = 1,nx
!        Z1(I,J,1)=ZS1(I,J)
        DO L=2,nz+1
          Z1(I,J,L)=Z1(I,J,L-1)+T1(I,J,L-1)*
     &         (Q1(I,J,L-1)*0.608+1.0)*287.04*
     &         (ALOG(1.*P1(I,J,L-1))-ALOG(1.*P1(I,J,L)))/G
        ENDDO
       ENDDO
      END DO

       print*,'test8'

! PD(I,J)=P1(I,J,1)-PDTOP-PT=PSFC(I,J)-PDTOP-PT
!      DO K=1,NZ+1
!      DO J=1,NY
!      DO I=1,NX
!        P1(I,J,K)=PT+PDTOP*ETA1(K)+PD(I,J)*ETA2(K)     ! PD(I,J) changed
!      ENDDO
!      ENDDO
!      ENDDO

!       print*,'test9'

!      DO K=1,NZ
!        DO J=1,NY
!        DO I=1,NX
!          PM1(I,J,K)=EXP((ALOG(1.*P1(I,J,K))+ALOG(1.*P1(I,J,K+1)))*0.5)
!        END DO
!        END DO
!      END DO

! CONVERT OTHER VARIABLES TO Hybrid Coordinate from Constant P

      DO J=1,NY
      DO I=1,NX
      DO K=1,KMX
        UVTQ(I,J,K,1)=U2(I,J,K)-UVTQ(I,J,K,1)
        UVTQ(I,J,K,2)=V2(I,J,K)-UVTQ(I,J,K,2)
        UVTQ(I,J,K,3)=T2(I,J,K)-UVTQ(I,J,K,3)
        UVTQ(I,J,K,4)=Q2(I,J,K)-UVTQ(I,J,K,4)
      END DO
      END DO
      END DO

!      DO K=1,KMX
!        WRITE(82)((U2(I,J,K),I=1,NX),J=1,NY,2)
!      END DO
!      DO K=1,KMX
!        WRITE(82)((V2(I,J,K),I=1,NX),J=1,NY,2)
!      END DO

!$omp parallel do
!$omp& private(i,j,k,N,W1,W)
      DO J=1,NY
      DO I=1,NX
      DO N=1,NZ
          IF(PM1(I,J,N).GE.P2(1))THEN            ! Below PM1(I,J,1)
            T1(I,J,N)=T1(I,J,N)+UVTQ(I,J,1,3)
            Q1(I,J,N)=Q1(I,J,N)+UVTQ(I,J,1,4)
          ELSE IF(PM1(I,J,N).LE.P2(KMX))THEN
            T1(I,J,N)=T1(I,J,N)+UVTQ(I,J,KMX,3)
            Q1(I,J,N)=Q1(I,J,N)+UVTQ(I,J,KMX,4)
          ELSE
            DO K=1,KMX-1
              IF(PM1(I,J,N).LE.P2(K).and.PM1(I,J,N).GT.P2(K+1))THEN
                W1=ALOG(1.*P2(K+1))-ALOG(1.*P2(K))
                W=(ALOG(1.*PM1(I,J,N))-ALOG(1.*P2(K)))/W1
                T1(I,J,N)=T1(I,J,N)+UVTQ(I,J,K,3)+
     &                    (UVTQ(I,J,K+1,3)-UVTQ(I,J,K,3))*W
                Q1(I,J,N)=Q1(I,J,N)+UVTQ(I,J,K,4)+
     &                    (UVTQ(I,J,K+1,4)-UVTQ(I,J,K,4))*W
                GO TO 22
              END IF
            END DO
          END IF
 22       CONTINUE
        END DO
      END DO
      END DO

!$omp parallel do
!$omp& private(i,j,k,N,W1,W)
      DO J=1,NY
      DO I=1,NX
      DO N=1,NZ
          IF(PMV1(I,J,N).GE.P2(1))THEN            ! Below PMV1(I,J,1)
            U1(I,J,N)=U1(I,J,N)+UVTQ(I,J,1,1)
            V1(I,J,N)=V1(I,J,N)+UVTQ(I,J,1,2)
          ELSE IF(PMV1(I,J,N).LE.P2(KMX))THEN
            U1(I,J,N)=U1(I,J,N)+UVTQ(I,J,KMX,1)
            V1(I,J,N)=V1(I,J,N)+UVTQ(I,J,KMX,2)
          ELSE
            DO K=1,KMX-1
              IF(PMV1(I,J,N).LE.P2(K).and.PMV1(I,J,N).GT.P2(K+1))THEN
                W1=ALOG(1.*P2(K+1))-ALOG(1.*P2(K))
                W=(ALOG(1.*PMV1(I,J,N))-ALOG(1.*P2(K)))/W1
                U1(I,J,N)=U1(I,J,N)+UVTQ(I,J,K,1)+
     &                    (UVTQ(I,J,K+1,1)-UVTQ(I,J,K,1))*W
                V1(I,J,N)=V1(I,J,N)+UVTQ(I,J,K,2)+
     &                    (UVTQ(I,J,K+1,2)-UVTQ(I,J,K,2))*W
                GO TO 24
              END IF
            END DO
          END IF
 24       CONTINUE
        END DO
      END DO
      END DO

      DO K=1,NZ+1
        WRITE(61)((Z1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ+1
        WRITE(61)((P1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(61)((T1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(61)((Q1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(61)((U1(I,J,K),I=1,NX),J=1,NY,2)
      END DO
      DO K=1,NZ
        WRITE(61)((V1(I,J,K),I=1,NX),J=1,NY,2)
      END DO

! Compute USC and VSC at Z=0

      DO J=1,NY
      DO I=1,NX
        IF(ZMV1(I,J,1).LT.0.)THEN            ! Below SEA LEVEL
          USC(I,J)=U1(I,J,1)
          VSC(I,J)=V1(I,J,1)
        ELSE
!          DP1=PMV1(I,J,1)*((1.+GAMMA*ZMV1(I,J,1)/TMV1(I,J))**COEF2-1.)
          DP1=0.
          USC(I,J)=U1(I,J,1)*(1.-DP1*1.4E-5)
          VSC(I,J)=V1(I,J,1)*(1.-DP1*1.4E-5)
        END IF
      END DO
      END DO

      rewind KUNIT

      WRITE(KUNIT) NX,NY,NZ,I360
      WRITE(KUNIT) LON1,LAT1,LON2,LAT2,CENTRAL_LON,CENTRAL_LAT
      WRITE(KUNIT) PM1
      WRITE(KUNIT) T1
      WRITE(KUNIT) Q1
      WRITE(KUNIT) U1
      WRITE(KUNIT) V1
      WRITE(KUNIT) DZDT
      WRITE(KUNIT) Z1
      WRITE(KUNIT) HLON,HLAT,VLON,VLAT
      WRITE(KUNIT) P1
      WRITE(KUNIT) PD
      WRITE(KUNIT) ETA1
      WRITE(KUNIT) ETA2
      WRITE(KUNIT) USC
      WRITE(KUNIT) VSC


      DEALLOCATE ( ZM1,PM1,TV1 )

      DEALLOCATE ( ZMV1,PMV1,HV )

      DEALLOCATE ( T2,Q2 )
      DEALLOCATE ( U2,V2 )
      DEALLOCATE ( P2,SLP,Z2,SIG )
      DEALLOCATE ( HP,RH )

      DEALLOCATE ( UVTQ )

      print*,'test11'

!C     END OF ROUTINE.
!C
!      RETURN
      END


      SUBROUTINE HURR_MESS(ITIM,IBGS,IMAX,JMAX,GLON,GLAT,
     &           I360)

! all common blocks are output

      PARAMETER (IRX=41,JRX=41,NST=10)
      PARAMETER (MAXVIT=15)

      REAL(4) GLAT(IMAX,JMAX),GLON(IMAX,JMAX)
      real(4) SLON_N,SLAT_N,CLON_N,CLAT_N

      COMMON /NHC/ KSTM,IC_N(NST),JC_N(NST)
      COMMON /NHC1/ SLON_N(NST),SLAT_N(NST),CLON_N(NST),CLAT_N(NST)
      DIMENSION STMDIR(NST),STMSPD(NST)
      CHARACTER ST_NAME(NST)*3,STMNAME(NST)*3,TCVT(NST)*95
      COMMON /STNAME/ST_NAME,STMNAME
      COMMON /TCVIT/TCVT
      COMMON /RSFC/STRPSF(NST),STVMAX(NST),STRPSF_06(NST)


      CHARACTER BUFIN(95)*1,BUFY2K(95)*1,STMNAM(NST)*12,STMNMZ*9
      CHARACTER FMTVIT(MAXVIT)*6,BUFINZ*100,LATNS*1,LONEW*1

      DIMENSION IVTVAR(MAXVIT),VITVAR(MAXVIT),VITFAC(MAXVIT),
     &          ISTVAR(MAXVIT),IENVAR(MAXVIT)
      DIMENSION ISTMCX1(7,NST),ISTMCY1(7,NST),STMCX(NST),STMCY(NST)
      DATA ISTVAR/20,29,34,39,45,49,53,58,63,68,71,75,80,85,90/
      DATA IENVAR/27,32,36,42,47,51,56,61,66,69,73,78,83,88,93/
      DATA VITFAC/2*1.0,2*0.1,1.0,0.1,9*1.0/
      DATA FMTVIT/'(I8.8)','(I4.4)','(I3.3)','(I4.4)',2*'(I3.3)',
     &            3*'(I4.4)','(I2.2)','(I3.3)',4*'(I4.4)'/

      EQUIVALENCE (BUFIN(37),LATNS),(BUFIN(43),LONEW),
     &            (BUFIN(10),STMNMZ),(BUFIN(1),BUFINZ)
      EQUIVALENCE (IVTVAR(1),IDATEZ),(IVTVAR(2),IUTCZ)
!
      EQUIVALENCE (VITVAR( 3),STMLTZ),(VITVAR( 4),STMLNZ),
     &            (VITVAR( 5),STMDRZ),(VITVAR( 6),STMSPZ),
     1            (VITVAR( 9),RMPSFZ),(VITVAR(10),STMVMX)
!
      DO I=1,10
        SLON_N(I)=0.
        SLAT_N(I)=0.
        CLON_N(I)=0.
        CLAT_N(I)=0.
        IC_N(I)=0
        JC_N(I)=0
!        STMNAME(I)='NUL'
        ST_NAME(I)='NUL'
        STRPSF(I)=3.0
        STVMAX(I)=0.
        STRPSF_06(I)=3.0
      END DO

      INDX1=ITIM-2

      K1STM=0
      DO I=1,10
        STMCX(I)=0.
        STMCY(I)=0.
        STMNAME(I)='NUL'
        READ(30,442,end=436)
     &     (ISTMCY1(J,I),ISTMCX1(J,I),J=1,7),STMNAME(I)
       ! Western hemisphere TCs, converts longitude into negative value
       if(I360.eq.180)then
        do j=1,7
          ISTMCX1(J,I)=-ISTMCX1(J,I)
        end do
       endif  
!wpac        if(I360.eq.180)then
!wpac          do j=1,7
!wpac            IF(ISTMCX1(J,I).LT.-1800)
!wpac     &         ISTMCX1(J,I)=3600+ISTMCX1(J,I)
!wpac          end do
!wpac        end if
        DYM1=0.
        DXM1=0.
        DYP1=0.
        DXP1=0.
        IF(ISTMCY1(1,I)*ISTMCY1(4,I).NE.0)THEN
          DYM1=(ISTMCY1(4,I)-ISTMCY1(1,I))/3.
          DXM1=(ISTMCX1(4,I)-ISTMCX1(1,I))/3.
        END IF
        IF(ISTMCY1(4,I)*ISTMCY1(7,I).NE.0)THEN
          DYP1=(ISTMCY1(7,I)-ISTMCY1(4,I))/3.
          DXP1=(ISTMCX1(7,I)-ISTMCX1(4,I))/3.
        END IF
        IF( ISTMCY1(2,I) == 999) ISTMCY1(2,I)=ISTMCY1(4,I)-DYM1*2.
        IF( ISTMCY1(3,I) == 999) ISTMCY1(3,I)=ISTMCY1(4,I)-DYM1*1.
        IF( ISTMCX1(2,I) == 999) ISTMCX1(2,I)=ISTMCX1(4,I)-DXM1*2.
        IF( ISTMCX1(3,I) == 999) ISTMCX1(3,I)=ISTMCX1(4,I)-DXM1*1.
        IF( ISTMCY1(5,I) == 999) ISTMCY1(5,I)=ISTMCY1(4,I)+DYP1*1.
        IF( ISTMCY1(6,I) == 999) ISTMCY1(6,I)=ISTMCY1(4,I)+DYP1*2.
        IF( ISTMCX1(5,I) == 999) ISTMCX1(5,I)=ISTMCX1(4,I)+DXP1*1
        IF( ISTMCX1(6,I) == 999) ISTMCX1(6,I)=ISTMCX1(4,I)+DXP1*2
!
!        STMCX(I)=360.-ISTMCX1(INDX1,I)*0.1
        STMCX(I)=ISTMCX1(INDX1,I)*0.1
        STMCY(I)=ISTMCY1(INDX1,I)*0.1
        K1STM=K1STM+1
        PRINT*,' CT STORM Model CENTER at ',ITIM,'h = ',
     &          STMNAME(I),STMCX(I),STMCY(I)
      END DO
 442  FORMAT(14x,14i5,25x,A3)
 436  CONTINUE

      REWIND 30

  90  REWIND 11
      KREC=0
      KSTORM=0
      NERROR=0

!  Get the hurricane center from the hurricane message made by NHC

!     READ A RECORD INTO BUFFER

  100 CONTINUE
      READ(11,101,ERR=990,END=200) (BUFIN(NCH),NCH=1,95)
  101 FORMAT(95A1)

      if(BUFIN(35).eq.'N' .or. BUFIN(35).eq.'S')  then

         print *, ' '
         print *, '==> Read in RECORD from tcvitals file -- contains a',
     &    ' 2-digit year "'
         print *, ' '

         BUFY2K(1:19) = BUFIN(1:19)
         IF(BUFIN(20)//BUFIN(21).GT.'20')  THEN
            BUFY2K(20) = '1'
            BUFY2K(21) = '9'
         ELSE
            BUFY2K(20) = '2'
            BUFY2K(21) = '0'
         ENDIF
         BUFY2K(22:95) = BUFIN(20:93)
         BUFIN = BUFY2K

         print *, ' '
         print *, '==> 2-digit year converted to 4-digit year "'
         print *, ' '

      else  if(BUFIN(37).eq.'N' .or. BUFIN(37).eq.'S')  then

         print *, ' '
         print *, '==> Read in RECORD from tcvitals file -- contains a',
     &    ' 4-digit year "'
         print *, ' '

      else

         print *, ' '
         print *, '***** Cannot determine if this record contains ',
     &    'a 2-digit year or a 4-digit year - skip it and try reading ',
     &    'the next record'
         print *, ' '
         go to 100

      end if

      KREC=KREC+1

      DO I=1,3
        ST_NAME(KREC)(I:I)=BUFIN(I+5)
      END DO
      DO I=1,95
        TCVT(KREC)(I:I)=BUFIN(I)
      END DO

!     DECODE DATE AND TIME

      DO 110 IV=1,2
      CALL DECVAR(ISTVAR(IV),IENVAR(IV),IVTVAR(IV),IERDEC,
     &            FMTVIT(IV),BUFINZ)

  110 CONTINUE

      DO 140 IV=3,MAXVIT
      CALL DECVAR(ISTVAR(IV),IENVAR(IV),IVTVAR(IV),IERDEC,
     &            FMTVIT(IV),BUFINZ)
      VITVAR(IV)=REAL(IVTVAR(IV))*VITFAC(IV)
  140 CONTINUE

!          *****************************************************
!          *****************************************************
!          ****            IMPORTANT NOTES:                 ****
!          ****                                             ****
!          ****    ALL STORM LONGITUDES CONVERTED TO        ****
!          ****    0-360 DEGREES, POSITIVE EASTWARD  !!!    ****
!          ****                                             ****
!          ****    ALL STORM SPEEDS ARE IN M/SEC            ****
!          ****                                             ****
!          ****    ALL DISTANCE DATA ARE IN KM              ****
!          ****                                             ****
!          ****    ALL PRESSURE DATA ARE IN HPA (MB)        ****
!          *****************************************************
!          *****************************************************

!     SIGN OF LATITUDE AND CONVERT LONGITUDE

      IF(LATNS .EQ. 'S')  THEN
      STMLTZ=-STMLTZ
      ELSE IF(LATNS .NE. 'N')  THEN
      WRITE(6,153) STMLTZ,STMLNZ,LATNS
  153 FORMAT('******ERROR DECODING LATNS, ERROR RECOVERY NEEDED.',
     &       '  STMLTZ,STMLNZ,LATNS=',2F12.2,2X,A1)
      GO TO 100
      ENDIF

      IF(LONEW .EQ. 'W')  THEN
        STMLNZ=-STMLNZ
!wpac      ELSE IF(LONEW .EQ. 'E')  THEN
!wpac        if(I360.eq.360)then
!wpac          STMLNZ=-360+STMLNZ
!wpac        end if
      ELSE IF(LONEW .NE. 'E')  THEN
      WRITE(6,157) STMLTZ,STMLNZ,LATNS
  157 FORMAT('******ERROR DECODING LONEW, ERROR RECOVERY NEEDED.',
     &       '  STMLTZ,STMLNZ,LATNS=',2F12.2,2X,A1)
      ENDIF

      PRINT*,'STMLTZ,STMLNZ,LATNS=',STMLTZ,STMLNZ,LATNS
      PRINT*,'IDATEZ,IUTCZ=',IDATEZ,IUTCZ

      print*,'STMNAME(1),STMNMZ(KREC)=',STMNAME(1),ST_NAME(KREC)

      DO I=1,10
        IF(STMNAME(I).EQ.ST_NAME(KREC))THEN
          STMCX21=STMCX(I)
          STMCY21=STMCY(I)
          STMLTZ=STMCY(I)
          STMLNZ=STMCX(I)
          go to 785
        END IF
      END DO

 785  continue

      PRINT*,'STMCX21,STMCY21=',STMCX21,STMCY21

      IF(KSTORM .LT. 10)  THEN

      KSTORM=KSTORM+1
      CLAT_N(KSTORM)=STMLTZ
      CLON_N(KSTORM)=STMLNZ
      STMDIR(KSTORM)=STMDRZ
      STMSPD(KSTORM)=STMSPZ
      STMNAM(KSTORM)=STMNMZ
      STRPSF(KSTORM)=RMPSFZ
      STVMAX(KSTORM)=STMVMX
      GO TO 100

      ELSE

  300 WRITE(6,301) KSTORM
  301 FORMAT(/'******KSTORM EXCEEDS AVAILABLE SPACE, KSTORM=',
     &       I5,/,' Results may have serious problem')
      GO TO 200

      ENDIF

  200 IF(KSTORM .GT. 0)  THEN
      WRITE(6,201)KSTORM,KREC
  201 FORMAT(/'...FOUND STORM IN VITALS FILE.',/,4X,I5,
     &       ' TOTAL NUMBER OF RECORDS READ=',I7)
      ELSE
      WRITE(6,202)
  202 FORMAT(/'NO STORM FOUND IN VITALS FILE.')
!      CALL W3TAGE('RELOCATE_MV_NVORTEX_T254L64')
!      CALL ERREXIT(56)
      STOP
      END IF

!  Correct to the storm center position

      PI=ATAN(1.0)*4.E+00
      PI180 = PI/180.
      DT=(float(ITIM)-6.)*3600.                     !  Second
      ONEDEG=360./(2.*PI*6.37E6)                    !  Degree/Meter
      FACT=DT*ONEDEG

      KSTM=KSTORM

      DO I=1,KSTM

      WRITE(*,430)STMNAM(I),CLAT_N(I),CLON_N(I),STMDIR(I),STMSPD(I)
  430 FORMAT(/' STORM NAME: ',A12,/, ' READIN STORM CENTER=',2F12.4,
     &       /,' STORM DIR and SPEED: ',2F12.4)

      PRINT*,'RAD OUTMOST CLOSED ISOBAR= ',STRPSF(I),' km'
      STRPSF(I)=STRPSF(I)*1000.*ONEDEG
      PRINT*,'RAD OUTMOST CLOSED ISOBAR= ',STRPSF(I),' degree'
      PRINT*,'MAX STORM WIND SPEED = ',STVMAX(I),' m/s'

      END DO

!      IF(IBGS.EQ.0)THEN
!        DO I=1,KSTM
!          READ(21,21) Ipsfc,Ipcls,Irmax
!          STRPSF_06(I)=Irmax*1000.*ONEDEG
!        END DO
! 21   format(52x,I4,1x,I4,1x,I4)
!      END IF

      DO I=1,KSTM
        DO K=1,K1STM
          IF(STMNAME(K).EQ.ST_NAME(I))THEN
            IFWRT=0
            DO J=1,7
        IF(ISTMCY1(J,K).EQ.0.and.ISTMCX1(J,K).EQ.0)THEN
              IFWRT=1
            END IF
            END DO
            IF(IFWRT.EQ.0)THEN
              XDIST6H=CLON_N(I)-ISTMCX1(4,K)*0.1
              YDIST6H=CLAT_N(I)-ISTMCY1(4,K)*0.1
              CLON_N(I)=STMCX(K)+XDIST6H
              CLAT_N(I)=STMCY(K)+YDIST6H
            ELSE
              USTM=STMSPD(I)*SIN(PI180*STMDIR(I))
              VSTM=STMSPD(I)*COS(PI180*STMDIR(I))
              CLON_N(I)=CLON_N(I)+USTM*FACT/COS(PI180*CLAT_N(I))
              CLAT_N(I)=CLAT_N(I)+VSTM*FACT
            END IF
            PRINT*, ' CT STORM OBS. CENTER at ',ITIM,'h = ',
     &               STMNAME(K),CLON_N(I),CLAT_N(I)
          END IF
        END DO
      END DO


      DO 900 I=1,KSTM

      CLON=CLON_N(I)
      CLAT=CLAT_N(I)

      AMN = 500.
      DO ILA = 1,JMAX
      DO ILO = 1,IMAX
        DMN = GLAT(ILO,ILA) - CLAT
        OMN = GLON(ILO,ILA) - CLON
        DISTC=DMN*DMN+OMN*OMN
        IF(ILA*ILO.LT.20)PRINT*,'GLON,GLAT=',
     &         GLON(ILO,ILA),GLAT(ILO,ILA),DISTC
        IF (DISTC.LE.AMN) THEN
          AMN = DISTC
          JC  = ILA
          IC  = ILO
        ENDIF
      END DO
      END DO

      IC_N(I)=IC
      JC_N(I)=JC

      PRINT *,'CLON,CLAT=',CLON,CLAT
      PRINT *,'IC,JC=  ',IC,JC,GLON(IC,JC),GLAT(IC,JC)

      SLON_N(I) = floor(GLON(IC,JC)+0.5 - IRX/2)
      SLAT_N(I) = floor(GLAT(IC,JC)+0.5 - JRX/2)
      PRINT *,' '
      PRINT *,'=========================================='
      PRINT *,'SLON,SLAT=',SLON_N(I),SLAT_N(I)


  900 CONTINUE

      RETURN

  990 WRITE(6,991) BUFIN
  991 FORMAT('******ERROR READING STORM RECORD.  BUFIN IS:',/,
     &       ' ******',A95,'******')
      GO TO 100
      RETURN

      END

      SUBROUTINE DECVAR(ISTART,IEND,IVALUE,IERDEC,FMT,BUFF)

      PARAMETER (NCHLIN=130)

      CHARACTER FMT*(*),BUFF*(*),OUTLIN*1

      DIMENSION OUTLIN(NCHLIN)

!c && 2 comments
!CC    WRITE(6,1) FMT,BUFF
!CC  1 FORMAT(/'...FMT=',A10,/,' ...BUFF=',A100)

      READ(BUFF(ISTART:IEND),FMT,ERR=10)  IVALUE
      IERDEC=0
      RETURN

   10 CONTINUE

      OUTLIN=' '

      IERDEC=10
      OUTLIN(ISTART:IEND)='*'

      WRITE(6,31) (OUTLIN(ICH1),ICH1=1,NCHLIN)
      WRITE(6,32) BUFF
   31 FORMAT(/'******ERROR DECODING, BUFF=',/,130A1)
   32 FORMAT(A130)

      RETURN
      END

       subroutine wrf_move(ITIM,KST,grid_lon,grid_lat,
     &      US850,VS850,KS850,P2,zwindow,
     &      twindow,qwindow,uwindow,vwindow,pwindow,SIG,
     &      HLAT,HLON,VLAT,VLON,lmeta,iswin,iewin,
     &     jswin,jewin,IBGS,IVOBS,iflag_cold,I360,crfactor)
!
! ITIM: Time level
! CLON_N,CLAT_N: observed storm center
! CLON_TIM,CLAT_TIM: storm center in the model
! lower-left corner of the 41x41 degree domain
! grid_lon,grid_lat,SIG: grid coordinate
! twindow,qwindow,uwindow,vwindow,pwindow: input data
! iswin,iewin,jswin,jewin,lmeta: coordinate index range

!  insert storm relocation code here

       implicit none

       integer(4) IRX,JRX,MTV4,MTV6,NST,IBGS,IVOBS
       integer    iflag_cold,I360
       integer    IST,IED,JST,JED,KS850,gd_dim3
       parameter (IRX=41,JRX=41,NST=10,gd_dim3=450)

       integer(4) iswin,iewin,jswin,jewin,lmeta
       real(4) twindow(iswin:iewin,jswin:jewin,lmeta)
       real(4) qwindow(iswin:iewin,jswin:jewin,lmeta)
       real(4) uwindow(iswin:iewin,jswin:jewin,lmeta)
       real(4) vwindow(iswin:iewin,jswin:jewin,lmeta)
       real(4) pwindow(iswin:iewin,jswin:jewin)
       real(4) zwindow(iswin:iewin,jswin:jewin)

       real(4) grid_lon(iswin:iewin,jswin:jewin)
       real(4) grid_lat(iswin:iewin,jswin:jewin)
       real(4) sig(lmeta)

       real(4) HLAT(iswin:iewin,jswin:jewin)
       real(4) HLON(iswin:iewin,jswin:jewin)
       real(4) VLAT(iswin:iewin,jswin:jewin)
       real(4) VLON(iswin:iewin,jswin:jewin)

       real(4) US850(iswin:iewin,jswin:jewin)
       real(4) VS850(iswin:iewin,jswin:jewin)

       integer(4) IGD(IRX,JRX,gd_dim3),JGD(IRX,JRX,gd_dim3)
       integer(4) NSUM(IRX,JRX),NGRID
       real  WGD(IRX,JRX,gd_dim3),WSUM(IRX,JRX)

       real(4) U850(IRX,JRX),V850(IRX,JRX)
       real(4) P2(lmeta)

       integer(4) i,j,k,i1,j1,inx,jnx,kmx,ITIM
       integer(4) inx1,jnx1,irx1,jrx1
       integer(4) k1,k2,k3,k4,k5,k6
       integer(4) m,n,m1,n1,n_check
       real(8)    coef1,coef2,coef3
       real(8)    rdst,rdst1,sdat1,dist,cost2
       real(4)    XB1,XB2,YB1,YB2,AB1,AB2,BB1,BB2
       real(4)    crfactor

       real(4),allocatable::vort(:,:,:),divg(:,:,:)
!       real(8),allocatable::coslat(:,:),dx(:,:),dy(:,:)
!       real(4),allocatable::aft(:,:),bft(:,:),cft(:,:)
!       real(4),allocatable::dft(:,:),eft(:,:),fnt(:,:)
!       real(4),allocatable::SF1(:,:,:),SF2(:,:,:)
       real(4),allocatable::HDATN(:,:,:),SDAT(:,:,:)
       real(4),allocatable::BLON(:),BLAT(:),XLON(:,:),XLAT(:,:)
       real(4),allocatable::XLON1(:,:),XLAT1(:,:)

       real(8) pi,pi180,delt1,COSLAT2
!       real(4) rjac,c_latg,c_long

!       real(8) test3,uv_diff,uv_tt

       integer(4) KST

       real(4)    SLON_N,SLAT_N,CLON_N,CLAT_N

       CHARACTER ST_NAME(NST)*3,STMNAME(NST)*3,TCVT(NST)*95

       COMMON /NHC1/ SLON_N(NST),SLAT_N(NST),CLON_N(NST),CLAT_N(NST)
       COMMON /STNAME/ST_NAME,STMNAME
       COMMON /TCVIT/TCVT

       inx=iewin-iswin+1
       jnx=jewin-jswin+1
       kmx=lmeta
       MTV4=4*kmx+2
       MTV6=6*kmx+2

       allocate (vort(inx,jnx,kmx),divg(inx,jnx,kmx))
!       allocate (coslat(inx,jnx),dx(inx,jnx),dy(inx,jnx))
!       allocate (aft(inx,jnx),bft(inx,jnx),cft(inx,jnx))
!       allocate (dft(inx,jnx),eft(inx,jnx),fnt(inx,jnx))
!       allocate (SF1(inx,jnx,kmx),SF2(inx,jnx,kmx))
       allocate (SDAT(IRX,JRX,MTV6))
       allocate (BLON(IRX),BLAT(JRX))
       allocate (XLON(inx,jnx),XLAT(inx,jnx))
       allocate (XLON1(inx,jnx),XLAT1(inx,jnx))

       pi=4.*atan(1.)
       pi180=pi/180.
!       delt1=6.371E6*pi180
!       rjac=cos(pi/jnx)

       do j=1,jnx
       do i=1,inx
          i1=iswin+i-1
          j1=jswin+j-1
!          coslat(i,j)=cos(grid_lat(i1,j1)*pi180)
!          XLON1(i,j)=grid_lon(i1,j1)
!          XLAT1(i,j)=grid_lat(i1,j1)
          XLON1(i,j)=HLON(i1,j1)
          XLAT1(i,j)=HLAT(i1,j1)

          XLON(i,j)=VLON(i1,j1)              ! LON, LAT at U,V grids
          XLAT(i,j)=VLAT(i1,j1)
       end do
       end do

       do i=1,IRX
         BLON(i)=SLON_N(kst)+i-1.
       end do
       do j=1,JRX
         BLAT(j)=SLAT_N(kst)+j-1.
       end do

      XB1=XLON1(1,1)
      XB2=XLON1(INX-1,1)
      DO J=1,JNX,2
        IF(XB1.LT.XLON1(1,J))XB1=XLON1(1,J)
        IF(XB2.GT.XLON1(INX-1,J))XB2=XLON1(INX-1,J)
      END DO
!
      YB1=XLAT1(1,2)
      YB2=XLAT1(1,JNX-1)
      DO I=1,INX
        IF(YB1.LT.XLAT1(I,2))YB1=XLAT1(I,2)
        IF(YB2.GT.XLAT1(I,JNX-1))YB2=XLAT1(I,JNX-1)
      END DO

       RDST=0.25

       XB1=XB1+RDST
       XB2=XB2-RDST
       YB1=YB1+RDST
       YB2=YB2-RDST

       AB1=BLON(1)
       AB2=BLON(IRX)
       BB1=BLAT(1)
       BB2=BLAT(JRX)

       print*,'XB1,XB2,AB1,AB2=',XB1,XB2,AB1,AB2
       print*,'YB1,YB2,BB1,BB2=',YB1,YB2,BB1,BB2

       IF(AB1.LT.XB1.or.AB2.GT.XB2.or.BB1.LT.YB1.or.BB2.GT.YB2)THEN
       CALL CREAT_41X41(ITIM,KST,KMX,MTV6,KS850,U850,V850,SDAT,P2)
!         RDST1=0.75
         IST=8
         IED=33
         JST=8
         JED=33
         print*,'using outer nest data'
       ELSE
!         RDST1=0.06
         IST=1
         IED=IRX
         JST=1
         JED=JRX
       END IF
       print*,'crfactor=',crfactor
       RDST=0.05*crfactor
       print*,'RDST=', RDST
       RDST1=RDST*RDST

       DO J=1,JNX
       DO I=1,INX
         i1=iswin+i-1
         j1=jswin+j-1
         DO K=1,KMX
           vort(i,j,k)=uwindow(i1,j1,k)
           divg(i,j,k)=vwindow(i1,j1,k)
         END DO
       END DO
       END DO


!23456789012345678901234567890123456789012345678901234567890123456789012

       allocate (HDATN(inx,jnx,MTV4))

       DO J=1,JNX
       DO I=1,INX
         i1=iswin+i-1
         j1=jswin+j-1
         HDATN(I,J,1)=zwindow(i1,j1)
         HDATN(I,J,2)=pwindow(i1,j1)
       END DO
       END DO

       DO K=1,KMX
         K1=K+2
         K2=2*(K-1)+1+KMX+2                                   ! div ==> u
         K3=2*(K-1)+2+KMX+2                                   ! vor ==> v
         K4=K+3*KMX+2
         DO J=1,JNX
         DO I=1,INX
           i1=iswin+i-1
           j1=jswin+j-1
           HDATN(I,J,K1)=twindow(i1,j1,K)
           HDATN(I,J,K2)=vort(i,j,K)           ! div ==> u
           HDATN(I,J,K3)=divg(i,j,K)           ! vor ==> v
           HDATN(I,J,K4)=qwindow(i1,j1,K)
         END DO
         END DO
       END DO


!      do k=1,kmx
!        write(27)((twindow(I,J,k),i=iswin,iewin),j=jswin,jewin)
!      end do
!      do k=1,kmx
!        write(27)((vwindow(I,J,k),i=iswin,iewin),j=jswin,jewin)
!      end do

!      write(27)((vort(I,J,33),I=1,INX),J=1,JNX)
!      write(27)((divg(I,J,33),I=1,INX),J=1,JNX)


! fill the center area with inner nest data
! for 6 degree inner nest, using 10 degree data


       print*,'compute weight'

       IGD=0
       JGD=0
       WGD=0
       NSUM=0
       WSUM=0.

       N_CHECK=0

!$omp parallel do
!$omp& private(i,j,k,N,NGRID,COST2,DIST)
       DO J=JST,JED
       DO I=IST,IED
         NGRID=0
         NSUM(I,J)=0.
         WSUM(I,J)=0.
         DO N=1,JNX
         DO K=1,INX
           COST2=(COS((XLAT(K,N)+BLAT(J))*0.5*pi180))**2
           DIST=COST2*(XLON(K,N)-BLON(I))**2+
     &             (XLAT(K,N)-BLAT(J))**2
           IF(DIST.LE.RDST1)THEN
             NGRID=NGRID+1
             IGD(I,J,NGRID)=K
             JGD(I,J,NGRID)=N
             WGD(I,J,NGRID)=EXP(-DIST/RDST1)
             WSUM(I,J)=WSUM(I,J)+WGD(I,J,NGRID)
             NSUM(I,J)=NGRID
           END IF
         END DO
         END DO
       END DO
       END DO

       if(NGRID.GT.gd_dim3)STOP 12

       DO J=JST,JED
       DO I=IST,IED
         IF(NSUM(I,J).GT.5)THEN
           N_CHECK=1
         ELSE
           print*,'QQQ I,J=',I,J,NSUM(I,J),WSUM(I,J),BLON(I),BLAT(J)
!           N_CHECK=0
         END IF
       END DO
       END DO

       IF(N_CHECK.EQ.0)THEN
         print*,'increase RDST1'
         stop
       END IF

       print*,'corners=',XLON(1,1),XLON(INX,JNX)
       print*,'corners=',XLAT(1,1),XLAT(INX,JNX)

!$omp parallel do
!$omp& private(i,j,N,i1,j1,SDAT1)
         DO J=JST,JED
         DO I=IST,IED
           IF(NSUM(I,J).GT.5)THEN
           SDAT1=0.
           DO N=1,NSUM(I,J)
             I1=IGD(I,J,N)
             J1=JGD(I,J,N)
             SDAT1=SDAT1+US850(I1,J1)*WGD(I,J,N)
           END DO
           U850(I,J)=SDAT1/WSUM(I,J)
           SDAT1=0.
           DO N=1,NSUM(I,J)
             I1=IGD(I,J,N)
             J1=JGD(I,J,N)
             SDAT1=SDAT1+VS850(I1,J1)*WGD(I,J,N)
           END DO
           V850(I,J)=SDAT1/WSUM(I,J)
           END IF
         ENDDO
         ENDDO

!$omp parallel do
!$omp& private(i,j,k,N,K2,K3,K5,K6,SDAT1,I1,J1)
        DO K=1,KMX
          K2=4*(K-1)+3+KMX+2
          K3=4*(K-1)+4+KMX+2
          K5=4*(K-1)+1+KMX+2                                   ! div (here U,V, otherwise move to next loop)
          K6=4*(K-1)+2+KMX+2                                   ! vor
          DO J=JST,JED
          DO I=IST,IED
           IF(NSUM(I,J).GT.5)THEN
           SDAT1=0.
           DO N=1,NSUM(I,J)
             I1=IGD(I,J,N)
             J1=JGD(I,J,N)
             SDAT1=SDAT1+uwindow(I1,J1,K)*WGD(I,J,N)
           END DO
           SDAT(I,J,K2)=SDAT1/WSUM(I,J)
           SDAT1=0.
           DO N=1,NSUM(I,J)
             I1=IGD(I,J,N)
             J1=JGD(I,J,N)
             SDAT1=SDAT1+vwindow(I1,J1,K)*WGD(I,J,N)
           END DO
           SDAT(I,J,K3)=SDAT1/WSUM(I,J)
           SDAT(I,J,K5)=SDAT(I,J,K2)
           SDAT(I,J,K6)=SDAT(I,J,K3)
           END IF
         ENDDO
         ENDDO
       ENDDO

!*** FOR T PS points


       IGD=0
       JGD=0
       WGD=0
!$omp parallel do
!$omp& private(i,j,k,N,NGRID,COST2,DIST)
       DO J=JST,JED
       DO I=IST,IED
         NGRID=0
         WSUM(I,J)=0.
         DO N=1,JNX
         DO K=1,INX
           COST2=(COS((XLAT1(K,N)+BLAT(J))*0.5*pi180))**2
           DIST=COST2*(XLON1(K,N)-BLON(I))**2+
     &             (XLAT1(K,N)-BLAT(J))**2
           IF(DIST.LE.RDST1)THEN
             NGRID=NGRID+1
             IGD(I,J,NGRID)=K
             JGD(I,J,NGRID)=N
             WGD(I,J,NGRID)=EXP(-DIST/RDST1)
             WSUM(I,J)=WSUM(I,J)+WGD(I,J,NGRID)
             NSUM(I,J)=NGRID
           END IF
         END DO
         END DO
       END DO
       END DO


!$omp parallel do
!$omp& private(i,j,N,SDAT1,I1,J1)
       DO J=JST,JED
       DO I=IST,IED
         IF(NSUM(I,J).GT.5)THEN
           SDAT1=0.
           DO N=1,NSUM(I,J)
             I1=IGD(I,J,N)
             J1=JGD(I,J,N)
             SDAT1=SDAT1+zwindow(I1,J1)*WGD(I,J,N)
           END DO
           SDAT(I,J,1)=SDAT1/WSUM(I,J)
           SDAT1=0.
           DO N=1,NSUM(I,J)
             I1=IGD(I,J,N)
             J1=JGD(I,J,N)
             SDAT1=SDAT1+pwindow(I1,J1)*WGD(I,J,N)
           END DO
           SDAT(I,J,2)=SDAT1/WSUM(I,J)
         END IF
       END DO
       END DO

!       WRITE(83)((SDAT(I,J,1),I=1,41),J=1,41)
!       WRITE(83)((SDAT(I,J,2),I=1,41),J=1,41)

!$omp parallel do
!$omp& private(i,j,K,K1,K4,N,SDAT1,I1,J1)
       DO K=1,KMX
         K1=K+2
!         K2=4*(K-1)+3+KMX+2
!         K3=4*(K-1)+4+KMX+2
         K4=K+5*KMX+2
!         K5=4*(K-1)+1+KMX+2                                   ! div
!         K6=4*(K-1)+2+KMX+2                                   ! vor
         DO J=JST,JED
         DO I=IST,IED
           IF(NSUM(I,J).GT.5)THEN
           SDAT1=0.
           DO N=1,NSUM(I,J)
             I1=IGD(I,J,N)
             J1=JGD(I,J,N)
             SDAT1=SDAT1+twindow(I1,J1,K)*WGD(I,J,N)
           END DO
           SDAT(I,J,K1)=SDAT1/WSUM(I,J)
           SDAT1=0.
           DO N=1,NSUM(I,J)
             I1=IGD(I,J,N)
             J1=JGD(I,J,N)
             SDAT1=SDAT1+qwindow(I1,J1,K)*WGD(I,J,N)
           END DO
           SDAT(I,J,K4)=SDAT1/WSUM(I,J)
           END IF
         ENDDO
         ENDDO

!        WRITE(83)((SDAT(I,J,K1),I=1,41),J=1,41)

       ENDDO

!       DO K=1,KMX
!         K4=K+5*KMX+2
!         WRITE(83)((SDAT(I,J,K4),I=1,41),J=1,41)
!       END DO

!      write(33)((pwindow(I,J),I=1,INX),J=1,JNX,2)
!      DO K=1,KMX
!        write(33)((twindow(I,J,K),I=1,INX),J=1,JNX,2)
!      END DO
!      DO K=1,KMX
!        write(33)((vort(I,J,K),I=1,INX),J=1,JNX,2)
!      END DO
!      DO K=1,KMX
!        write(33)((divg(I,J,K),I=1,INX),J=1,JNX,2)
!      END DO
!      DO K=1,KMX
!        write(33)((qwindow(I,J,K),I=1,INX),J=1,JNX,2)
!      END DO



       CALL HURR_MOVE(ITIM,KST,INX,JNX,KMX,MTV6,MTV4,SDAT,HDATN,
     &         U850,V850,XLON1,XLAT1,XLON,XLAT,SIG,IBGS,IVOBS,
     &         iflag_cold,I360)


!23456789012345678901234567890123456789012345678901234567890123456789012


       DO J=1,JNX
       DO I=1,INX
         i1=iswin+i-1
         j1=jswin+j-1
         zwindow(i1,j1)=HDATN(I,J,1)
         pwindow(i1,j1)=HDATN(I,J,2)
       END DO
       END DO

       DO K=1,KMX
         K1=K+2
         K2=2*(K-1)+1+KMX+2                                   ! div ==> u
         K3=2*(K-1)+2+KMX+2                                   ! vor ==> v
         K4=K+3*KMX+2
         DO J=1,JNX
         DO I=1,INX
           i1=iswin+i-1
           j1=jswin+j-1
           twindow(i1,j1,K)=HDATN(I,J,K1)
           vort(i,j,K)=HDATN(I,J,K2)           ! div ==> u
           divg(i,j,K)=HDATN(I,J,K3)           ! vor ==> v
           qwindow(i1,j1,K)=HDATN(I,J,K4)
         END DO
         END DO
       END DO

!      write(29)((pwindow(I,J),I=1,INX),J=1,JNX,2)
!      DO K=1,KMX
!        write(29)((twindow(I,J,K),I=1,INX),J=1,JNX,2)
!      END DO
!      DO K=1,KMX
!        write(29)((vort(I,J,K),I=1,INX),J=1,JNX,2)
!      END DO
!      DO K=1,KMX
!        write(29)((divg(I,J,K),I=1,INX),J=1,JNX,2)
!      END DO
!      DO K=1,KMX
!        write(29)((qwindow(I,J,K),I=1,INX),J=1,JNX,2)
!      END DO


!       aft=1.
!       bft=1.
!       do j=1,jnx
!       do i=1,inx
!           COSLAT2=COSLAT(i,j)*COSLAT(i,j)
!           cft(i,j)=COSLAT2
!           dft(i,j)=COSLAT2
!           eft(i,j)=-2.*(1.+COSLAT2)
!       end do
!       end do

!        print*,'compute SF1'

!        SF1=uwindow
!        DO k=1,kmx
!          fnt=0.
!          test3=0.
!          do j=3,jnx-2
!          do i=3,inx-2
!            fnt(i,j)=0.5*((divg(i+1,j,k)-divg(i-1,j,k))-
!     &          (vort(i,j+1,k)-vort(i,j-1,k))*COSLAT(i,j))*
!     &          dy(i,j)*COSLAT(i,j)
!            test3=test3+abs(fnt(i,j))
!          enddo
!          enddo
!!          write(6,*)test3
!          call sor(aft,bft,cft,dft,eft,fnt,SF1(1,1,k),jnx,rjac)
!        enddo

!        SF2=vwindow
!        do k=1,kmx
!          fnt=0.
!          do j=3,jnx-2
!          do i=3,inx-2
!            fnt(i,j)=0.5*((vort(i+1,j,k)-vort(i-1,j,k))+
!     &        (divg(i,j+1,k)-divg(i,j-1,k))*COSLAT(i,j))*
!     &        dy(i,j)*COSLAT(i,j)
!          enddo
!          enddo
!          call sor(aft,bft,cft,dft,eft,fnt,SF2(1,1,k),jnx,rjac)
!        enddo

!        uv_diff=0.
!        uv_tt=0.
!        DO k=1,kmx
!        do j=2,jnx-1
!        do i=2,inx-1
!          i1=iswin+i-1
!          j1=jswin+j-1
!          uv_diff=uv_diff+sqrt((SF1(i,j,k)-uwindow(i1,j1,k))**2+
!     &           (SF2(i,j,k)-vwindow(i1,j1,k))**2)
!          uv_tt=uv_tt+sqrt(uwindow(i1,j1,k)**2+vwindow(i1,j1,k)**2)
!        enddo
!        enddo
!        enddo

!        print*,'uv_diff=',uv_diff,uv_diff/uv_tt

      do j=1,jnx
      do i=1,inx
        i1=iswin+i-1
        j1=jswin+j-1
        do k=1,kmx
          uwindow(i1,j1,k)=vort(i,j,k)
          vwindow(i1,j1,k)=divg(i,j,k)
        end do
      end do
      end do

!       uwindow=SF1
!       vwindow=SF2

!      DO K=1,KMX
!        WRITE(84)((uwindow(I,J,K),I=1,INX),J=1,JNX,2)
!      END DO
!      DO K=1,KMX
!        WRITE(84)((vwindow(I,J,K),I=1,INX),J=1,JNX,2)
!      END DO


       deallocate (vort,divg)
!       deallocate (coslat,dx,dy)
!       deallocate (aft,bft,cft)
!       deallocate (dft,eft,fnt)
!       deallocate (SF1,SF2)
       deallocate (HDATN,SDAT)
       deallocate (BLON,BLAT)
       deallocate (XLON,XLAT)

       return
       end


        SUBROUTINE sor(a,b,c,d,e,f,u,jmax,rjac)
        INTEGER jmax,MAXITS
        real(4) a(jmax,jmax),b(jmax,jmax),
     &       c(jmax,jmax),d(jmax,jmax),e(jmax,jmax),
     &       f(jmax,jmax),u(jmax,jmax)
        PARAMETER (MAXITS=1000,EPS=1.e-3)

!!         Successive overrelaxation solution of equation (19.5.25)
!!         with Chebyshev acceleration. a, b, c, d, e, andfare input
!!         as the coe cients of the equation, each dimensioned to
!!         the grid size JMAX   JMAX. u is input as the initial guess
!!         to the solution, usually zero, and returns with the
!!         nal value. rjac is input as the spectral radius of the
!!         Jacobi iteration, or an estimate of it.
!!         rjac=1.-2*pi/jmax

        INTEGER ipass,j,jsw,l,lsw,n
!!        DOUBLE PRECISION anorm,anormf,omega,resid
!!         Double precision is a good idea for JMAX bigger than about 25.
        anormf=0.d0
!!         Compute initial norm of residual and terminate iteration
!!         when norm has been reduced by a factor EPS.
        do j=4,jmax-3
        do l=4,jmax-3
          anormf=anormf+abs(f(j,l))   !!   Assumes initial u is zero.
        enddo
        enddo
        print*,'test anormf=',anormf
        omega=1.d0
        do n=1,MAXITS
          anorm=0.d0
          jsw=1
          do ipass=1,2     !!   Odd-even ordering.
            lsw=jsw
!            do j=2,jmax-1
!            do l=lsw+1,jmax-1,2
            do j=4,jmax-3
            do l=lsw+3,jmax-3,2
              resid=a(j,l)*u(j+1,l)+b(j,l)*u(j-1,l)+
     &              c(j,l)*u(j,l+1)+d(j,l)*u(j,l-1)+
     &              e(j,l)*u(j,l)-f(j,l)
              anorm=anorm+abs(resid)
              u(j,l)=u(j,l)-omega*resid/e(j,l)
            enddo
            lsw=3-lsw
            enddo
            jsw=3-jsw
            if(n.eq.1.and.ipass.eq.1) then
              omega=1.d0/(1.d0-.5d0*rjac**2)
            else
              omega=1.d0/(1.d0-.25d0*rjac**2*omega)
            endif
          enddo
!          print*,'n,anorm,anormf=',n,anorm,anormf
          if(anorm.lt.EPS*anormf)return
        enddo
        print*,'n,anorm,anormf=',n,anorm,anormf
        print*,'MAXITS exceeded in sor'
        END



      SUBROUTINE hurr_move(ITIM,KST,IMAX,JMAX,KMAX,MTV6,MTV4,SDAT,
     &       HDATN,U850,V850,XLON1,XLAT1,XLON,XLAT,SL,IBGS,IVOBS,
     &       iflag_cold,I360)
! KST is the storm number

      ! From WRF-NMM in R2 HWRF (WRF-NMM 3.2):
      real, parameter :: deg2rad = 3.1415926/180.

      PARAMETER (IX=41,JX=41,NF=11,IT=24,IR=120,IJ=IX*JX)
      PARAMETER (NSG=720000,NST=10)
      PARAMETER (NSG5=NSG/5)

      REAL(4) SDAT(IX,JX,MTV6),HDATN(IMAX,JMAX,MTV4),SL(KMAX)

      REAL(4) U850(IX,JX),V850(IX,JX)

      DIMENSION U(IX,JX),V(IX,JX),US(IX,JX),VS(IX,JX)
      DIMENSION UD(IX,JX),VD(IX,JX),DKY(IX,JX),DKM(IX,JX)
      DIMENSION DKY1(IMAX,JMAX),DKM1(IMAX,JMAX)
!      DIMENSION DKM1(IMAX,JMAX),SKIP(IX,JX)
      DIMENSION XTU(IX,NF),XTV(IX,NF),YTU(IX,JX),YTV(IX,JX)
      DIMENSION RS(IT),R0(IT),RF(IT)
      DIMENSION M(NF),FK(NF),TW(IT,IR)
      DIMENSION ALAT(JX),ALON(IX)
      DIMENSION ING(NSG),JNG(NSG),RRIJ(NSG)
      DIMENSION ING3(NSG),JNG3(NSG)
      DIMENSION ING5(NSG5),JNG5(NSG5)
      DIMENSION ING6(NSG5),JNG6(NSG5)
      DIMENSION ISTMCX1(7,NST),ISTMCY1(7,NST),STMCX(NST),STMCY(NST)
      REAL(4) testu(IMAX,JMAX),testv(IMAX,JMAX)

      REAL(8),allocatable:: DATG(:,:),DATG2(:,:),DDAT(:,:)
      REAL(8),allocatable:: ENV1(:,:,:)

      REAL(4),ALLOCATABLE :: SAVE1(:,:),SAVE2(:,:)

      real(4) XLON1(IMAX,JMAX),XLAT1(IMAX,JMAX)
      real(4) XLON(IMAX,JMAX),XLAT(IMAX,JMAX)
      real(4) GLON(IMAX,JMAX),GLAT(IMAX,JMAX)
      real(4) SLON_N,SLAT_N,CLON_N,CLAT_N

      real(4) zmax

      real mindist,tmplat,tmplon,dist

!      COMMON /egrid/GLON(IMAX,JMAX),GLAT(IMAX,JMAX)                  !input

      COMMON /NHC/ KSTM,IC_N(NST),JC_N(NST)
      COMMON /NHC1/SLON_N(NST),SLAT_N(NST),CLON_N(NST),CLAT_N(NST)   !input
      COMMON /POSIT/CLON_NEW,CLAT_NEW,SLON,SLAT,CLON,CLAT,RAD
      COMMON /vect/R0,XVECT(IT),YVECT(IT)

      CHARACTER ST_NAME(NST)*3,STMNAME(NST)*3,TCVT(NST)*95
      COMMON /STNAME/ST_NAME,STMNAME                 ! need storm name
      COMMON /TCVIT/TCVT                     ! need TC vitals
      COMMON /RSFC/STRPSF(NST),STVMAX(NST),STRPSF_06(NST)

      COMMON /TR/ING,JNG,IB
      COMMON /NHC2/MDX,MDY
      COMMON /NHC3/AMDX,AMDY

      DATA M/2,3,4,2,5,6,7,2,8,9,2/

      allocate (DATG(IMAX,JMAX),DATG2(IMAX,JMAX),DDAT(IMAX,JMAX))
      allocate (ENV1(IMAX,JMAX,MTV4))

      ALLOCATE ( SAVE1(IMAX,JMAX), SAVE2(IMAX,JMAX) )

      SAVE1=0.

      PI=ASIN(1.)*2
      RAD=PI/180.
      pi180=PI/180.

      ING=0
      JNG=0

!      IF(STVMAX(KST).LT.21.)THEN
!        NF1=7
!      ELSE IF(STVMAX(KST).LT.26.)THEN
!        NF1=8
!      ELSE IF(STVMAX(KST).LT.31.)THEN
!        NF1=9
!      ELSE
!        NF1=NF
!      END IF

!      IF(STVMAX(KST).LT.26.)THEN
!        NF1=9
!      ELSE
!        NF1=NF
!      END IF

      NF1=NF

      print*,'IBGS=',IBGS,IVOBS

      CLON_NHC = CLON_N(KST)
      CLAT_NHC = CLAT_N(KST)

      zmax1=0.
      DO J=1,JMAX
      DO I=1,IMAX
        A = XLON1(I,J) - CLON_NHC
        B = XLAT1(I,J) - CLAT_NHC
        R = SQRT(A**2. + B**2.)
        IF(R.LT.12.) THEN
          if(zmax1.lt.HDATN(i,j,1))zmax1=HDATN(i,j,1)
        ENDIF
      END DO
      END DO

      PRINT*,'zmax1=',zmax1

!      if(zmax1.gt.500.)NF1=9

!new      IF(IBGS.eq.1)THEN
!new        NF1=7
!new      END IF

      DO I=1,NF
        IF(I.GE.NF1)M(I)=2                     ! remove 1 degree wave in the last step
        FK(I)=0.5/(1-COS(2.*PI/M(I)))
      ENDDO


! LON, LAT at U,V grids

      GLON=XLON
      GLAT=XLAT

      SLON=SLON_N(KST)
      SLAT=SLAT_N(KST)

      DO I=1,IX
        ALON(I)=SLON+(I-1)              ! specify 41x41 lon and lat
!        PRINT*,'ALON(I)=',I,ALON(I),XLON1(1,1),XLON1(IMAX,JMAX)
      END DO
      DO J=1,JX
        ALAT(J)=SLAT+(J-1)
!        PRINT*,'ALAT(J)=',I,ALAT(J),XLAT1(1,1),XLAT1(IMAX,JMAX)
      END DO

!      CLON=ALON(21)
!      CLAT=ALAT(21)
      CLON = CLON_N(KST)
      CLAT = CLAT_N(KST)

      INDX1=ITIM-2

      K1STM=0
      DO I=1,NST
        STMCX(I)=0.
        STMCY(I)=0.
        STMNAME(I)='NUL'
        READ(30,442,end=436)
     &    (ISTMCY1(J,I),ISTMCX1(J,I),J=1,7),STMNAME(I)
!
        IF(I360.eq.180) STMCX(I)=-1*ISTMCX1(INDX1,I)*0.1  !Western hemisphere TC
        IF(I360.eq.360) STMCX(I)=ISTMCX1(INDX1,I)*0.1     !Eastern hemisphere TC
!wpac        IF(I360.eq.180)THEN
!wpac          IF(STMCX(I).lt.-180.)STMCX(I)=STMCX(I)+360.
!wpac        END IF
        STMCY(I)=ISTMCY1(INDX1,I)*0.1
        K1STM=K1STM+1
        PRINT*,'QLIU test=',STMNAME(I),STMCX(I),STMCY(I)
      END DO
 442  FORMAT(14x,14i5,25x,A3)
 436  CONTINUE

!.. READ U, V at ~850 mb

      K8501=1
      DIST2=ABS(SL(1)-0.85)
      DO K=1,KMAX
        DIST1=ABS(SL(K)-0.85)
        IF(DIST1.LT.DIST2)THEN
          K8501=K
          DIST2=DIST1
        END IF
      END DO

      K850=2+KMAX+4*(K8501-1)+1

      IF(K8501.LT.1.OR.K8501.GT.KMAX)THEN
        PRINT*,'K8501 is out of bound'
        STOP
      END IF

      PRINT*,'QLIUQLIU test',K850,K8501
      print*,SDAT(30,30,K850)
      print*,SDAT(30,30,K850+1)
      print*,SDAT(30,30,K850+2)
      print*,SDAT(30,30,K850+3)

      DO J=1,JX
      DO I=1,IX
!        U(I,J)=SDAT(I,J,K850+2)
!        V(I,J)=SDAT(I,J,K850+3)
        U(I,J)=U850(I,J)
        V(I,J)=V850(I,J)
      END DO
      END DO


!      fact=cos(CLAT*rad)
      fact=1.0
      do j=1,jx
      do i=1,ix
! East-West wind in new coordinate (phi,theta)
! this conversion only affects Hurrican Center determination and R0
        U(I,J)=U(I,J)/fact
      end do
      end do
!.. DO ZONAL FILTER

      DO 100 J=1,JX
      DO N=1,NF1
      XTU(1,N)  = U(1,J)
      XTU(IX,N) = U(IX,J)
      XTV(1,N)  = V(1,J)
      XTV(IX,N) = V(IX,J)
      ENDDO

      DO I=2,IX-1
      XTU(I,1) = U(I,J)+FK(1)*(U(I-1,J)+U(I+1,J)-2.*U(I,J))
      XTV(I,1) = V(I,J)+FK(1)*(V(I-1,J)+V(I+1,J)-2.*V(I,J))
      ENDDO

      DO N=2,NF1
      DO I=2,IX-1
      XTU(I,N)=XTU(I,N-1)+FK(N)*(XTU(I-1,N-1)+XTU(I+1,N-1)-2.
     &         *XTU(I,N-1))
      XTV(I,N)=XTV(I,N-1)+FK(N)*(XTV(I-1,N-1)+XTV(I+1,N-1)-2.
     &         *XTV(I,N-1))
      ENDDO
      ENDDO

      DO I=1,IX
      US(I,J) = XTU(I,NF1)
      VS(I,J) = XTV(I,NF1)
      ENDDO

100   CONTINUE

!.. DO MERIDIONAL FILTER

      DO 200 I=1,IX

      DO N=1,NF1
      YTU(1,N)  = US(I,1)
      YTU(JX,N) = US(I,JX)
      YTV(1,N)  = VS(I,1)
      YTV(JX,N) = VS(I,JX)
      ENDDO

      DO J = 2 , JX-1
      YTU(J,1) = US(I,J) + FK(1)*(US(I,J-1) + US(I,J+1)
     &                          -2.*US(I,J))
      YTV(J,1) = VS(I,J) + FK(1)*(VS(I,J-1) + VS(I,J+1)
     &                          -2.*VS(I,J))
      ENDDO

      DO N = 2 , NF1
      DO J = 2 , JX-1
      YTU(J,N) = YTU(J,N-1) + FK(N)*(YTU(J-1,N-1)  +
     &              YTU(J+1,N-1) - 2.*YTU(J,N-1))
      YTV(J,N) = YTV(J,N-1) + FK(N)*(YTV(J-1,N-1)  +
     &              YTV(J+1,N-1) - 2.*YTV(J,N-1))
      ENDDO
      ENDDO

      DO J = 1 , JX
      US(I,J)   =  YTU(J,NF1)
      VS(I,J)   =  YTV(J,NF1)
      ENDDO
 200  CONTINUE

!.. GET THE DISTURBANCE FIELD

      DO I=1,IX
      DO J=1,JX
      UD(I,J) = U(I,J) - US(I,J)
      VD(I,J) = V(I,J) - VS(I,J)
      ENDDO
      ENDDO

!.. FIND NEW VORTEX CENTER

      ICHEK=0
      CLON_TIM=0.
      CLAT_TIM=0.
      DO I=1,K1STM
        IF(STMNAME(I).EQ.ST_NAME(KST))THEN
          CLON_TIM=STMCX(I)
          CLAT_TIM=STMCY(I)
          ICHEK=1
          DO J=1,7                    ! newly added
            IF(ISTMCY1(J,I).EQ.0.and.ISTMCX1(J,I).EQ.0)THEN
              ICHEK=0
            END IF
          END DO
          GO TO 446
        END IF
      END DO
 446  CONTINUE
      print*,'ICHEK,CLON_TIM=',ICHEK,CLON_TIM
      IF((ICHEK.EQ.1).AND.(ABS(CLON_TIM).LT.359.5))THEN
        CLON_NEW=CLON_TIM
        CLAT_NEW=CLAT_TIM
      ELSE
        CLON_NEW=CLON_NHC
        CLAT_NEW=CLAT_NHC
      ENDIF

      PRINT*,'GFDL CENTER= ',ITIM,'h ',CLON_NEW,CLAT_NEW

!.. CALCULATE TANGENTIAL WIND AROUND CIRCLE
!             24 DIRECTION, RADIALLY 0.1DEG INTERVAL

      CALL TWIND(UD,VD,TW)

!.. CALCULATE STARTING POINT AT EACH DIRECTION

      CALL STRT_PT(RS,TW,RFAVG)

!.. DETERMINE FILTER DOMAIN D0 (=1.25*Rf)


      CALL FILTER(RS,TW,RF,RFAVG,KST,IBGS,IVOBS,iflag_cold)

      AMDX=CLON_NHC-CLON_NEW
      AMDY=CLAT_NHC-CLAT_NEW

      IF(ITIM.EQ.6)THEN
        WRITE(52,65)TCVT(KST)(1:32),
     &              CLON_NHC,CLAT_NHC,CLON_NEW,
     &              CLAT_NEW,CLON_TIM,CLAT_TIM,AMDX,AMDY,
     &              SQRT(AMDX*AMDX+AMDY*AMDY)
        DO I=1,K1STM
          IF(STMNAME(I).EQ.ST_NAME(KST))THEN
            IFWRT=0
            DO J=1,7
              IF(ISTMCY1(J,I).EQ.0.and.ISTMCX1(J,I).EQ.0)THEN
                IFWRT=1
              END IF
            END DO
            IF(IFWRT.EQ.1)THEN
              WRITE(52,76)TCVT(KST)(1:32)
            ELSE
              WRITE(52,77)TCVT(KST)(1:32)
            END IF
 76         FORMAT(/'  STORM: ',A32,10x,' is bogused')
 77         FORMAT(/'  STORM: ',A32,10x)
            WRITE(52,79)
     &       (ISTMCY1(J,I),ISTMCX1(J,I),J=1,7),STMNAME(I)
 79         FORMAT(/'  TRACKER OUTPUT: ',14i4,5x,A3)
          END IF
        END DO
      END IF
 65    FORMAT(/' STORM NAME: ',A32,
     &         /'  OBSERVED CENTER POSITION:     ',2F10.2
     &        ,/'  MODEL CENTER POSITION :       ',2F10.2
     &        ,/'  MODEL CENTER POSITION (TIM):  ',2F10.2
     &        ,/'  RELOCATION DISTANCE (DEGREE): ',3F10.2)

! test by qliu
!      MDX=MDX+50
!      AMDX=AMDX+50*DLN

       KNHC=-99999
       MNHC=-99999

      DO I=1,IMAX-1
      DO J=1,JMAX-1
        IF((CLAT_NHC.GE.GLAT(I,J).and.
     &     CLAT_NHC.LT.GLAT(I,J+1)).and.
     &     (CLON_NHC.GE.GLON(I,J).and.
     &     CLON_NHC.LT.GLON(I+1,J)))THEN
           KNHC=I
           MNHC=J
        END IF
      END DO
      END DO

      if( knhc==-99999 .or. mnhc==-99999 )  then
         ! Should not get here, but it happens once in a blue moon.
         ! The above search for the center point failed.
         ! We'll try again by finding the nearest gridpoint.
         ! This is much slower since it requires calculating great
         ! circle distances.

         ! What we should do is replace this and the above loop with
         ! code that uses the rot-lat-lon projection information.
         ! That isn't available here so we have to use these
         ! dirty tricks instead.

         write(6,*) 'COULD NOT FIND GRIDPOINT OF CENTER IN DOMAIN.'
         write(6,*) 'REVERTING TO NEAREST GRIDPOINT METHOD.'
         write(0,*) 'COULD NOT FIND GRIDPOINT OF CENTER IN DOMAIN.'
         write(0,*) 'REVERTING TO NEAREST GRIDPOINT METHOD.'

         mindist=9e15
         clon_nhc_rad=clon_nhc*deg2rad
         clat_nhc_rad=clat_nhc*deg2rad
         DO I=1,IMAX-1
            DO J=1,JMAX-1
               tmplat=(glat(i,j)+glat(i,j+1))/2*deg2rad
               tmplon=(glon(i,j)+glon(i+1,j))/2*deg2rad
               dist=calc_dist(tmplat,tmplon,
     &                        clat_nhc_rad,clon_nhc_rad)
               if(dist<mindist) then
                  mindist=dist
                  knhc=i
                  mnhc=j
               endif
            END DO
         END DO

         if(mindist>100000) then
            write(6,*) 'NEAREST GRIDPOINT IS AT LEAST',
     &                 '100 KM FROM NHC POINT.  GIVING UP.'
            write(0,*) 'NEAREST GRIDPOINT IS AT LEAST',
     &                 '100 KM FROM NHC POINT.  GIVING UP.'
            stop 47
         endif

      endif

      if( knhc-7<=1 .or. knhc+7>=imax .or.
     &    mnhc-7<=1 .or. mnhc+7>=jmax) then
         write(6,*) 'NHC GRIDPOINT WITHIN 8 POINTS OF EDGE.  ABORT!!'
         write(0,*) 'NHC GRIDPOINT WITHIN 8 POINTS OF EDGE.  ABORT!!'
         write(6,*) 'knhc,mnhc,imax,jmax: ',knhc,mnhc,imax,jmax
         write(0,*) 'knhc,mnhc,imax,jmax: ',knhc,mnhc,imax,jmax
         stop 49
      endif

      IC1=KNHC+1
      JC1=MNHC+1
      MDX=IFIX((CLON_NHC-CLON_NEW)/(GLON(IC1,MNHC)-GLON(KNHC,MNHC)))
      MDY=IFIX((CLAT_NHC-CLAT_NEW)/(GLAT(KNHC,JC1)-GLAT(KNHC,MNHC)))

!234567890123456789012345678901234567890123456789012345678901234567890

      PRINT*,'GLON(1,1),GLAT(1,1)=',GLON(1,1),GLAT(1,1)

      PRINT*,'MDX,MDY,KNHC,MNHC=',MDX,MDY,KNHC,MNHC
      PRINT*,'AMDX,AMDY=',AMDX,AMDY
      PRINT*,'CLON_NHC,CLAT_NHC=',CLON_NHC,CLAT_NHC
      PRINT*,'CLON_NEW,CLAT_NEW=',CLON_NEW,CLAT_NEW
      RDIST2=AMDX*AMDX+AMDY*AMDY

      IB=0
      RRIJ=0.

      IB5=0
      ING5=0
      JNG5=0

      IB6=0
      ING6=0
      JNG6=0

      DO J=1,JMAX
      DO I=1,IMAX

      A = GLON(I,J) - CLON_NEW
      B = GLAT(I,J) - CLAT_NEW
      R = SQRT(A**2. + B**2.)
      IF(R.EQ.0.) GO TO 444
      TH = ACOS(A/R) / RAD
      IF(B.LT.0.) TH = 360-TH

      IF(TH.LE.7.5 .OR. TH.GT.352.5 ) IC = 1
      DO M2=2,24
        IF((TH.GT.(15.*(M2-1)-7.5)).and.
     &          (TH.LE.(15.*M2-7.5)))IC=M2
      END DO

!      print*,'R,R0(IC),ic=',R,R0(IC),ic

      IF(R.LT.R0(IC)) THEN
        IB = IB+1
        ING(IB) = I
        JNG(IB) = J

!        RRIJ(IB)=1.
!        IF(R.GT.0.75*R0(IC))THEN
!          RRIJ(IB)=(4.*(R0(IC)-R)/R0(IC))**2
!        END IF

      ENDIF

!      IF(R.LT.(R0(IC)))THEN
!        IB5=IB5+1
!        ING5(IB5)=I
!        JNG5(IB5)=J
!        RRIJ(IB5)=1.
!        IF(R.GT.(R0(IC)-0.5))THEN
!          WT2=min(1.0,(R0(IC)-R)/0.5)
!          RRIJ(IB5)=WT2*WT2*(3.-2.*WT2)
!        END IF
!      END IF

      IF((R.LT.R0(IC)).and.R.GT.(R0(IC)-0.8))THEN
        IB5=IB5+1
        ING5(IB5)=I
        JNG5(IB5)=J
        WT2=min(1.0,(R0(IC)-R)/0.8)
        RRIJ(IB5)=WT2*WT2*(3.-2.*WT2)
      END IF

      IF((R.LT.R0(IC)).and.R.GT.(R0(IC)-0.2))THEN
        IB6=IB6+1
        ING6(IB6)=I
        JNG6(IB6)=J
      END IF

      GO TO 22

444   IB = IB+1
      ING(IB) = I
      JNG(IB) = J

!      RRIJ(IB)=1.

22    CONTINUE

      ENDDO
      ENDDO


      IBH=0
      zmax=0.

      DO J=1,JMAX
      DO I=1,IMAX

      A = XLON1(I,J) - CLON_NEW
      B = XLAT1(I,J) - CLAT_NEW
      R = SQRT(A**2. + B**2.)
      IF(R.EQ.0.) GO TO 866
      TH = ACOS(A/R) / RAD
      IF(B.LT.0.) TH = 360-TH

      IF(TH.LE.7.5 .OR. TH.GT.352.5 ) IC = 1
      DO M2=2,24
        IF((TH.GT.(15.*(M2-1)-7.5)).and.
     &          (TH.LE.(15.*M2-7.5)))IC=M2
      END DO

!      print*,'R,R0(IC),ic=',R,R0(IC),ic

      IF(R.LT.R0(IC)) THEN
        IBH = IBH+1
        ING3(IBH) = I
        JNG3(IBH) = J
        if(zmax.lt.HDATN(i,j,1))zmax=HDATN(i,j,1)
      ENDIF

      GO TO 877

866   IBH = IBH+1
      ING3(IBH) = I
      JNG3(IBH) = J
877    CONTINUE

      ENDDO
      ENDDO

      print*,'IB,IBH=',IB,IBH,NSG
      IF(IB.GT.NSG.or.IBH.GT.NSG)THEN
        print*,'need to increase NSG'
        stop
      END IF

! temp relocation turned on
      IFLAG = 0

! Check if the syndata need to be called

      IF(ITIM.EQ.3)THEN
        DO I=1,K1STM
          IF(STMNAME(I).EQ.ST_NAME(KST))THEN
            IFWRT=0
            DO J=1,7
            IF(ISTMCY1(J,I).EQ.0.and.ISTMCX1(J,I).EQ.0)THEN
              IFWRT=1
            END IF
            END DO
            IF(IFWRT.EQ.1)THEN
              WRITE(55,101) TCVT(KST)
            END IF
  101       FORMAT(A95)
          END IF
        END DO
      END IF


!      print *,'GAUSSIAN GRID # WITHIN R0 ',IB
!      DO I = 1,IB
!      print *,'GAUSSIAN GRID WITHIN R0, LAT,LON ',
!     1      GLAT(ING(I),JNG(I)),GLON(ING(I),JNG(I))
!      print *,'GAUSSIAN GRID WITHIN R0 ',ING(I),JNG(I)
!      ENDDO

!.. SETTING VALUE for xvect, yvect, a(,), capd2

      call rodist

      call amatrix

      KMP=KMAX
      KDIV1=2+KMP
      KQ1=KDIV1+4*KMP

      NRED1 = 0

      ISE=1

      DO 777 IV = 1,MTV6

      IREM = -22

      IF(IV.GT.KDIV1.AND.IV.LE.KQ1)IREM=MOD(IV-KDIV1,4)
      IF((IV.GE.2.AND.IV.LE.KDIV1).OR.(IV.GT.KQ1).OR.
     &          (IREM.EQ.1.OR.IREM.EQ.2)) THEN
!      print *,'ORIGINAL VARIABLE # IS ',IV

! added by Qingfu Liu
! obtain the disturbance field

      ISE=ISE+1

      DO J=1,JX
      DO I=1,IX
        U(I,J)=SDAT(I,J,IV)
      END DO
      END DO

! First smooth in east-west direction

      DO 107 J=1,JX
      DO N=1,NF1
      XTU(1,N)  = U(1,J)
      XTU(IX,N) = U(IX,J)
      ENDDO

      DO I=2,IX-1
      XTU(I,1) = U(I,J)+FK(1)*(U(I-1,J)+U(I+1,J)-2.*U(I,J))
      ENDDO

      DO N=2,NF1
      DO I=2,IX-1
      XTU(I,N)=XTU(I,N-1)+FK(N)*(XTU(I-1,N-1)+XTU(I+1,N-1)
     &              -2.*XTU(I,N-1))
      ENDDO
      ENDDO

      DO I=1,IX
      US(I,J) = XTU(I,NF1)
      ENDDO

 107  CONTINUE

!.. DO MERIDIONAL FILTER

      DO 207 I=1,IX

      DO N=1,NF1
      YTU(1,N)  = US(I,1)
      YTU(JX,N) = US(I,JX)
      ENDDO

      DO J = 2 , JX-1
      YTU(J,1) = US(I,J) + FK(1)*(US(I,J-1) + US(I,J+1)
     &                  -2.*US(I,J))
      ENDDO

      DO N = 2 , NF1
      DO J = 2 , JX-1
      YTU(J,N) = YTU(J,N-1) + FK(N)*(YTU(J-1,N-1)  +
     &            YTU(J+1,N-1) - 2.*YTU(J,N-1))
      ENDDO
      ENDDO

      DO J = 1 , JX
      US(I,J)   =  YTU(J,NF1)
      ENDDO
 207  CONTINUE


C.. GET THE DISTURBANCE FIELD
C
      DO I=1,IX
      DO J=1,JX
      DKY(I,J) = U(I,J) - US(I,J)
      ENDDO
      ENDDO

!      print*,'before call SEPAR',ISE

      DKM=DKY
      CALL SEPAR(DKY,DKM)

!      print*,'after call SEPAR'


!      print*,'qliu test1'

      DO J=1,JX
      DO I=1,IX
!        SKIP(I,J)=DKM(I,J)
c        SKIP(I,J)=U(I,J)
        DKY(I,J) = DKM(I,J) + US(I,J)
c        DKY(I,J) = U(I,J)
      ENDDO
      ENDDO

! Interpolate DKY to high resolution model grids


       DO J=1,JMAX
       DO I=1,IMAX
         DKY1(I,J)=HDATN(I,J,ISE)
         SAVE1(I,J)=DKY1(I,J)
       ENDDO
       ENDDO

!      write(33)((SAVE1(I,J),I=1,IMAX),J=1,JMAX,2)


       IF(ISE.GT.(KMAX+2).and.ISE.LE.(3*KMAX+2))THEN             ! wind field

!      print*,'qliu test2'

!       DO N=1,JMAX
!       DO K=1,IMAX
       DO M3=1,IB
         N=JNG(M3)
         K=ING(M3)
         DO J=1,JX-1
          IF(GLAT(K,N).GE.ALAT(J).and.GLAT(K,N).LT.ALAT(J+1))THEN
           DO I=1,IX-1
            IF(GLON(K,N).GE.ALON(I).and.GLON(K,N).LT.ALON(I+1))THEN
             COEF1=(GLON(K,N)-ALON(I))/(ALON(I+1)-ALON(I))
             COEF2=(GLAT(K,N)-ALAT(J))/(ALAT(J+1)-ALAT(J))
             COEF3=(1.-COEF1)*(1.-COEF2)
             DKY1(K,N)=COEF3*DKY(I,J)+
     &                   COEF1*(1.-COEF2)*DKY(I+1,J)+
     &                   COEF1*COEF2*DKY(I+1,J+1)+
     &                   (1.-COEF1)*COEF2*DKY(I,J+1)
             GO TO 193
            END IF
           END DO
          END IF
         END DO
 193     CONTINUE
       END DO
!       END DO
!       END DO

       DKM1=DKY1

        DO I = 1,IB6
          IW = ING6(I)
          JW = JNG6(I)
          IF(MOD(JW,2) .NE. 0)THEN
            DKM1(IW,JW)=0.2*(DKY1(IW,JW)+DKY1(IW,JW-1)+DKY1(IW,JW+1)
     1                     +DKY1(IW+1,JW-1)+DKY1(IW+1,JW+1))
          ELSE
            DKM1(IW,JW)=0.2*(DKY1(IW-1,JW-1)+DKY1(IW-1,JW+1)
     1               +DKY1(IW,JW-1)+DKY1(IW,JW+1)+DKY1(IW,JW))
          END IF
        END DO

      DKY1=DKM1

      DO I = 1,IB5
        IW = ING5(I)
        JW = JNG5(I)
        DKY1(IW,JW)=DKM1(IW,JW)*RRIJ(I)+HDATN(IW,JW,ISE)*(1.-RRIJ(I))
      END DO

       DO J=1,JMAX
       DO I=1,IMAX
         SAVE1(I,J)=DKY1(I,J)
       ENDDO
       ENDDO

!      write(34)((SAVE1(I,J),I=1,IMAX),J=1,JMAX,2)

!      print*,'qliu test3'

!.. GET THE DISTURBANCE FIELD (High Resolution)

!      DO I=1,IMAX
!      DO J=1,JMAX
!        DKM1(I,J) = HDATN(I,J,ISE)-DKY1(I,J)
!      ENDDO
!      ENDDO

      ELSE

!      print*,'qliu test4'
!       DO N=1,JMAX
!       DO K=1,IMAX
       DO M3=1,IBH
         N=JNG3(M3)
         K=ING3(M3)
         DO J=1,JX-1
         IF(XLAT1(K,N).GE.ALAT(J).and.XLAT1(K,N).LT.ALAT(J+1))THEN
           DO I=1,IX-1
           IF(XLON1(K,N).GE.ALON(I).and.XLON1(K,N).LT.ALON(I+1))THEN
             COEF1=(XLON1(K,N)-ALON(I))/(ALON(I+1)-ALON(I))
             COEF2=(XLAT1(K,N)-ALAT(J))/(ALAT(J+1)-ALAT(J))
             COEF3=(1.-COEF1)*(1.-COEF2)
             DKY1(K,N)=COEF3*DKY(I,J)+
     &                   COEF1*(1.-COEF2)*DKY(I+1,J)+
     &                   COEF1*COEF2*DKY(I+1,J+1)+
     &                   (1.-COEF1)*COEF2*DKY(I,J+1)

             GO TO 293
           END IF
           END DO
         END IF
         END DO
 293     CONTINUE
       END DO
!       END DO
!       END DO

       DKM1=DKY1

        DO I = 1,IB6
          IW = ING6(I)
          JW = JNG6(I)
          IF(MOD(JW,2) .NE. 0)THEN
            DKM1(IW,JW)=0.2*(DKY1(IW,JW)+DKY1(IW,JW-1)+DKY1(IW,JW+1)
     1                     +DKY1(IW+1,JW-1)+DKY1(IW+1,JW+1))
          ELSE
            DKM1(IW,JW)=0.2*(DKY1(IW-1,JW-1)+DKY1(IW-1,JW+1)
     1               +DKY1(IW,JW-1)+DKY1(IW,JW+1)+DKY1(IW,JW))
          END IF
        END DO

      DKY1=DKM1

      DO I = 1,IB5
        IW = ING5(I)
        JW = JNG5(I)
        DKY1(IW,JW)=DKM1(IW,JW)*RRIJ(I)+HDATN(IW,JW,ISE)*(1.-RRIJ(I))
      END DO

       DO J=1,JMAX
       DO I=1,IMAX
         SAVE1(I,J)=DKY1(I,J)
       ENDDO
       ENDDO

!      print*,'qliu test5'

!      write(35)((SAVE1(I,J),I=1,IMAX),J=1,JMAX,2)

!.. GET THE DISTURBANCE FIELD (High Resolution)

!      DO I=1,IMAX
!      DO J=1,JMAX
!        DKM1(I,J) = HDATN(I,J,ISE)-DKY1(I,J)
!      ENDDO
!      ENDDO

      END IF


      DO J=1,JMAX
      DO I=1,IMAX
        ENV1(I,J,ISE) = DKY1(I,J)
      ENDDO
      ENDDO

      DO J=1,JMAX
      DO I=1,IMAX
      SAVE1(I,J)=ENV1(I,J,ISE)
      ENDDO
      ENDDO

!      write(29)((SAVE1(I,J),I=1,IMAX),J=1,JMAX,2)

      ENDIF

 777  CONTINUE

!      print*,'qliu test3'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       NCHT=71
       WRITE(NCHT)ST_NAME(KST)

       WRITE(NCHT)CLON_NEW,CLAT_NEW,CLON_NHC,CLAT_NHC
       WRITE(NCHT)zmax

        IWMAX=0.
        IWMIN=1000.
        JWMAX=0.
        JWMIN=1000.
        DO I = 1,IB
          IW = ING(I)
          JW = JNG(I)
          IF(IWMAX.LT.IW)IWMAX=IW
          IF(IWMIN.GT.IW)IWMIN=IW
          IF(JWMAX.LT.JW)JWMAX=JW
          IF(JWMIN.GT.JW)JWMIN=JW
        END DO
        IWMAX1=IWMAX+4
        IWMIN1=IWMIN-4
        JWMAX1=JWMAX+4
        JWMIN1=JWMIN-4


      IGU=IMAX
      JGU=JMAX

      KDIV2=2+KMAX
      KQ2=KDIV2+4*KMAX

      ISE = 1
      DO 781 IV = 1,MTV6

      IREM = -22

      IF(IV.GT.KDIV2.AND.IV.LE.KQ2)IREM=MOD(IV-KDIV2,4)
      IF((IV.GE.2.AND.IV.LE.KDIV2).OR.(IV.GT.KQ2).OR.
     &          (IREM.EQ.1.OR.IREM.EQ.2)) THEN
      ISE = ISE+1

      print*,'ISE,IV=',ISE,IV

      DDAT=0.

      DO J=1,JMAX
      DO I=1,IMAX
        DATG(I,J)=ENV1(I,J,ISE)
        DATG2(I,J)=HDATN(I,J,ISE)
      ENDDO
      ENDDO

      DDAT=0.
      SAVE1=0.
!      RDIST2=AMDX*AMDX+AMDY*AMDY
!      IF(RDIST2.GT.0.02)THEN
        IF(ISE.GE.2)THEN
          DO J=1,JMAX
          DO I=1,IMAX
            DDAT(I,J)=DATG2(I,J)-DATG(I,J)
          ENDDO
          ENDDO
        END IF
        DO J=1,JMAX
        DO I=1,IMAX
          SAVE1(I,J)=DDAT(I,J)
        ENDDO
        ENDDO
!        DO I = 1,IB
!          IW = ING(I)
!          JW = JNG(I)
!          SAVE1(IW,JW)=DDAT(IW,JW)
!        END DO

!      END IF

      IF(ISE.EQ.2) THEN
        DO J=1,JMAX
        DO I=1,IMAX
          HDATN(I,J,ISE)=DATG(I,J)
        ENDDO
        ENDDO
      END IF

      IF(ISE.GE.3.and.ISE.LE.(KMAX+2))then

        IF(IFLAG.EQ.1)THEN
          DO J=1,JMAX
          DO I=1,IMAX
            HDATN(I,J,ISE)=DATG2(I,J)
          ENDDO
          ENDDO
          SAVE1=0.
        ELSE
          DO J=1,JMAX
          DO I=1,IMAX
            HDATN(I,J,ISE)=DATG(I,J)
          ENDDO
          ENDDO
        END IF

      END IF

      IF(ISE.GT.(KMAX+2).and.ISE.LE.(3*KMAX+2))THEN

         DO J=1,JMAX
         DO I=1,IMAX
           HDATN(I,J,ISE)=DATG(I,J)
         ENDDO
         ENDDO

      ENDIF

      IF(ISE.GT.(3*KMAX+2))THEN
        IF(IFLAG.EQ.1)THEN
          DO J=1,JMAX
          DO I=1,IMAX
            HDATN(I,J,ISE)=DATG2(I,J)
          ENDDO
          ENDDO
          SAVE1=0.
        ELSE
           DO J=1,JMAX
           DO I=1,IMAX
             HDATN(I,J,ISE)=DATG(I,J)
             SAVE1(I,J)=SAVE1(I,J)*1.E3
           ENDDO
           ENDDO
        END IF
      ENDIF


        IF(ISE.EQ.2)THEN
          WRITE(NCHT)IWMIN1,IWMAX1,JWMIN1,JWMAX1
          DO J=1,JMAX
          DO I=1,IMAX
            SAVE2(I,J)=DATG(I,J)
          END DO
          END DO
          WRITE(NCHT)((SAVE2(I,J),I=1,IMAX),J=1,JMAX)    ! PSL
        END IF
!        IF(ISE.EQ.3)THEN
        IF(ISE.GE.3.and.ISE.LE.(KMAX+2))THEN
          IF(IFLAG.EQ.1)THEN
            DO J=1,JMAX
            DO I=1,IMAX
              SAVE2(I,J)=DATG2(I,J)
            END DO
            END DO
          ELSE
            DO J=1,JMAX
            DO I=1,IMAX
              SAVE2(I,J)=DATG(I,J)
            END DO
            END DO
          END IF
          WRITE(NCHT)((SAVE2(I,J),I=1,IMAX),J=1,JMAX)    ! T1
        END IF
        WRITE(NCHT)((SAVE1(I,J),I=IWMIN1,IWMAX1),J=JWMIN1,JWMAX1)

!        PRINT*,'TEST ISE=',ISE

        WRITE(25)((SAVE1(I,J),I=1,IMAX),J=1,JMAX,2)

      ENDIF

 781  CONTINUE

 788  CONTINUE


      DEALLOCATE ( SAVE1, SAVE2 )
      deallocate (DATG,DATG2,DDAT)
      deallocate (ENV1)

      end

      SUBROUTINE TWIND(UD,VD,TW)
      PARAMETER (IX=41,JX=41,NF=11,IT=24,IR=120)
      DIMENSION UD(IX,JX),VD(IX,JX),TW(IT,IR),R0(IT)
      COMMON /POSIT/CLON_NEW,CLAT_NEW,SLON,SLAT,CLON,CLAT,RAD
      COMMON /vect/R0,XVECT(IT),YVECT(IT)
      print*,'CLON_NEW,CLAT_NEW,RAD=',CLON_NEW,CLAT_NEW,RAD
      DO J=1,IR
      DO I=1,IT
!.. DETERMINE LAT, LON AREOUND CIRCLE
      DR = 0.1*J
      DD = (I-1)*15.*RAD
      DLON = DR*COS(DD)
      DLAT = DR*SIN(DD)
      TLON = CLON_NEW + DLON
      TLAT = CLAT_NEW + DLAT
!.. INTERPOLATION U, V AT TLON,TLAT AND CLACULATE TANGENTIAL WIND
      IDX = floor(TLON) - SLON + 1
      IDY = floor(TLAT) - SLAT + 1
      TW(I,J)=0.
      IF(IDX.GE.1.and.IDX.LT.IX.and.IDY.GE.1.and.IDY.LT.JX)THEN
      DXX  = TLON - floor(TLON)
      DYY  = TLAT - floor(TLAT)
      X1 = UD(IDX  ,IDY+1)*DYY + UD(IDX  ,IDY)*(1-DYY)
      X2 = UD(IDX+1,IDY+1)*DYY + UD(IDX+1,IDY)*(1-DYY)
      Y1 = UD(IDX+1,IDY  )*DXX + UD(IDX,IDY  )*(1-DXX)
      Y2 = UD(IDX+1,IDY+1)*DXX + UD(IDX,IDY+1)*(1-DXX)
      UT = (X1*(1-DXX)+X2*DXX + Y1*(1-DYY)+Y2*DYY)/2.
      IF(I.EQ.0.OR.I.EQ.13) UT = Y1
      IF(I.EQ.7.OR.I.EQ.19) UT = X1
      X1 = VD(IDX  ,IDY+1)*DYY + VD(IDX  ,IDY)*(1-DYY)
      X2 = VD(IDX+1,IDY+1)*DYY + VD(IDX+1,IDY)*(1-DYY)
      Y1 = VD(IDX+1,IDY  )*DXX + VD(IDX,IDY  )*(1-DXX)
      Y2 = VD(IDX+1,IDY+1)*DXX + VD(IDX,IDY+1)*(1-DXX)
      VT = (X1*(1-DXX)+X2*DXX + Y1*(1-DYY)+Y2*DYY)/2.
      IF(I.EQ.0.OR.I.EQ.13) VT = Y1
      IF(I.EQ.7.OR.I.EQ.19) VT = X1
!.. TANGENTIAL WIND
      TW(I,J) = -SIN(DD)*UT + COS(DD)*VT
      END IF
      if(j.eq.1)then
         print*,'TW(I,J),UT,VT=',I,J,TW(I,J),UT,VT,DD
     &          ,IDX,IDY,DLON,TLON,CLON_NEW,SLON
      end if

! for SH
      ENDDO
      ENDDO

      IF(CLAT_NEW.LT.0)THEN
        DO J=1,IR
        DO I=1,IT
          TW(I,J)=-TW(I,J)
        ENDDO
        ENDDO
      END IF
! SH

      RETURN
      END

      SUBROUTINE STRT_PT(RMX,TW,RFAVG)

      PARAMETER (IX=41,JX=41,NF=11,IT=24,IR=120)
      DIMENSION TW(IT,IR),TWM(IR),TMXX(IT),RMX(IT)
      REAL JMX

      DO I=1,IR
      TWM(I) = 0.
      ENDDO

!.. CALCULATE MEAN TANGENTIAL WIND

      DO 10 J=1,IR
      TM=0.
      DO 20 I=1,IT
      TM = TM + TW(I,J)
20    CONTINUE
      TWM(J) = TM/24.
      print *,'MEAN TANGENTIAL WIND ',J,TWM(J)
10    CONTINUE

!.. FIND MAXIMUM TANGENTIAL WIND RADIUS

      TMX=-100000000000.
      DO J=1,IR
      IF(TWM(J).GE.TMX) THEN
      TMX=TWM(J)
      JMX = J*0.1
      ENDIF
      ENDDO

      print *,'MAXIMUM TANGENTIAL WIND RADIUS ',JMX
      JJ=IFIX(JMX*10.)
      print *,'MAXIMUM TANGENTIAL WIND SPEED  ',TWM(JJ)

      JXX = 15 * JMX
!      print *,'JXX, 15*JMX is ',JXX

      ICK = 1
      CNT = 0.000004
!c      print *,'CNT  ',CNT

      DO 30 K=JXX,120
      IF(TWM(K).GE.6..OR.TWM(K).LT.3.) GO TO 30
      DXX = 10000.
      DV = TWM(K) - TWM(K+1)
      DVDR = DV/DXX
      IF(DVDR.LT.CNT) ICK = ICK+1
      IF(ICK.EQ.3) THEN
      RF=K*0.1
      GO TO 40
      ENDIF
30    CONTINUE

40    CONTINUE
      IF(ICK.NE.3) THEN
      DO IK=JXX,120
      IF(TWM(IK).LE.3) THEN
      RF = IK*0.1
      ICK=3
      GO TO 50
      ENDIF
      ENDDO
      ENDIF

50    CONTINUE
      IF(ICK.NE.3) RF = 12.

      RFAVG = RF

!.. CALCULATE Ra, Rb..  REF. KURIHARA ET AL. 1995

      RA = IFIX((0.5 * JMX)*10.)/10.
      RB = IFIX((0.75 * JMX + 0.25 * RF)*10.)/10.
      IRA = IFIX(RA*10.+0.5)
      IRB = IFIX(RB*10.+0.5)

!c      print *,'Ra, Rb, Rf  ', RA,RB,RF

!.. DETERMINE STARTING POINT FOR EVERY 24 DIRECTION

      DO I=1,IT
      TMXX(I) = -100000000.
      DO J=1,IR
      IF(TW(I,J).GE.TMXX(I)) THEN
      TMXX(I) = TW(I,J)
      RMX(I) = J*0.1*1.1
      ENDIF
      ENDDO
      ENDDO

!c      DO I=1,IT
!c      print *,'I, MX TANGENTIAL WIND RADIUS ',I,RMX(I),TMXX(I)
!c      ENDDO

      DO I=1,IT
      IF (RMX(I).GT.RB.OR.RMX(I).LT.RA) THEN
      TMX = -10000000.
      DO KK=IRA,IRB
      IF(TW(I,KK).GE.TMX) RM = KK * 0.1 * 1.1
      ENDDO
      MR = IFIX(RM*10. + 0.5)
      ICL=0
      DO LL = MR,IRB
      IF(TW(I,LL).LT.0.) ICL=ICL+1
      ENDDO
      IF(ICL.EQ.0) RMX(I) = RM*1.1
      ENDIF
      ENDDO

!c      DO I=1,IT
!c      print *,'I, RST ',I,RMX(I)
!c      ENDDO

      RETURN
      END

      SUBROUTINE FILTER(RS,TW,RF,RFAVG,KST,IBGS,IVOBS,iflag_cold)
      PARAMETER (IX=41,JX=41,IT=24,IR=120,NST=10)

      DIMENSION RS(IT),TW(IT,IR),RF(IT),R0(IT),IST(IT)
      DIMENSION R01(IT)
      COMMON /vect/R0,XVECT(IT),YVECT(IT)
      COMMON /RSFC/STRPSF(NST),STVMAX(NST),STRPSF_06(NST)

!      REAL(4) RMN,RMN_HWRF

      ICK = 1
      CNT = 0.000004
!      print *,'CNT  ',CNT

      DO I=1,IT
      IST(I) = IFIX(RS(I)*10)
!c      print *,'STARTING POINT ',I,IST(I)
      ENDDO

      DO 100 I=1,IT
      IS = IST(I)

!CWH      DO 30 K=IS,IR
      DO 30 K=IS,IR-1
      IF(TW(I,K).GE.6..OR.TW(I,K).LT.3.) GO TO 30
      DXX = 10000.
      DV = TW(I,K) - TW(I,K+1)
      DVDR = DV/DXX
      IF(DVDR.LT.CNT) THEN
      ICK = ICK+1
      ENDIF
      IF(ICK.EQ.3) THEN
      RF(I)=K*0.1 + 0.0000001
!c      print *,'1st Catagory ',I
      GO TO 100
      ENDIF
30    CONTINUE

40    CONTINUE
      DO IK=IS,IR
      IF(TW(I,IK).LE.3) THEN
      RF(I) = IK*0.1 + 0.00000001
!c      print *,'2nd Catagory ',I
      GO TO 100
      ENDIF
      ENDDO

50    CONTINUE
!c      print *,'3rd Catagory ',I
      RF(I) = 10.
100   CONTINUE

!c      RMAX=0.
      DO I=1,IT
        print *,'Rf AT EACH DIRECTION ',I,RF(I)
!        RF(I) =max(RF(I),3.)
        RF(I) = max(RF(I),2.2)
!2014        RF(I) = min(RF(I),1.1*STRPSF(KST))
!        RF(I) = min(RF(I),STRPSF(KST))
        RF(I) = min(RF(I),1.1*STRPSF(KST))
      END DO

      RMN=0.
      DO I=1,IT
        RMN=RMN+RF(I)
      END DO
      RMN=RMN/FLOAT(IT)
      DO I=1,IT
        IF(RF(I).GT.2.5*RMN)RF(I)=2.5*RMN
        IF(RF(I).LT.0.4*RMN)RF(I)=0.4*RMN
      END DO

        DO I=2,IT-1
          R01(I)=(RF(I)+RF(I-1)+RF(I+1))/3.
        END DO
        R01(1)=(RF(1)+RF(IT)+RF(2))/3.
        R01(IT)=(RF(IT)+RF(IT-1)+RF(1))/3.

        RF=R01


      IF(IBGS.eq.0)THEN
        DO I=1,IT
!            R0(I) = 1.25 * RF(I)
!2014            R0(I) = 1.1 * RF(I)
            R0(I) = 1.1* RF(I)
!            R0(I)=max(R0(I),3.3)
!            R0(I)=min(R0(I),1.4*STRPSF_06(KST))
            R0(I)=min(R0(I),1.5*STRPSF(KST))
            R0(I)=max(R0(I),2.5)+0.4
            IF(R0(I).GT.11.)R0(I)=11.
            print *,'R0,Rf AT EACH DIRECTION ',I,R0(I),RF(I)
        ENDDO
      ELSE IF(IBGS.eq.2)THEN
        DO I=1,IT
!            R0(I) = 1.25 * RF(I)
!2014            R0(I) = 1.1 * RF(I)
            R0(I) = 1.1* RF(I)
!            R0(I)=min(R0(I),2.5*STRPSF(KST))
!            R0(I)=max(R0(I),3.3)
            R0(I)=min(R0(I),1.5*STRPSF(KST))
            R0(I)=max(R0(I),2.5)+0.4
            IF(R0(I).GT.11.)R0(I)=11.
            print *,'R0,Rf AT EACH DIRECTION ',I,R0(I),RF(I)
        ENDDO
      ELSE
        DO I=1,IT
!          R0(I) = 1.25 * RF(I)
!2014          R0(I) = 1.1 * RF(I)
          R0(I) = 1.1* RF(I)
          R0(I)=min(R0(I),1.5*STRPSF(KST))
          R0(I)=max(R0(I),2.5)+0.4
          IF(R0(I).GT.11.)R0(I)=11.
          print *,'R0,Rf AT EACH DIRECTION ',I,R0(I),RF(I)
        ENDDO
      END IF

!        DO I=2,IT-1
!          R01(I)=(R0(I-1)+R0(I)+R0(I+1))/3.
!        END DO
!        R01(1)=(R0(IT)+R0(1)+R0(2))/3.
!        R01(IT)=(R0(IT-1)+R0(IT)+R0(1))/3.
!        R0=R01


!      IF(IBGS.NE.1)THEN
!        RMIN=1.E20
!        DO I=1,IT
!          IF(RMIN.GT.R0(I))RMIN=R0(I)
!        END DO
!        RMIN=min(RMIN,1.75*STRPSF(KST))
!        R0=RMIN
!      END IF

!  for bogus storm only
!       RF=5.               !
!       R0=6.25             ! R0=4.5 (15m/s), 5.0(20m/s), 6.25 (35 m/s)
!  for bogus storm only

      print *,'STRPSF(KST)=',STRPSF(KST)
!C test for circular domain

      RMN=0.
      DO I=1,IT
        RMN=RMN+R0(I)
      END DO
      RMN=RMN/FLOAT(IT)

      print*,'RMN before adjustment=',RMN

      RMN_HWRF=RMN

      IF(IBGS.eq.1.and.iflag_cold.eq.0)THEN
        REWIND(65)
        READ(65)RMN_HWRF
      END IF
      IF(iflag_cold.ne.0)THEN
        RMN_HWRF=STRPSF(KST)
      END IF

      RMN_FACT=RMN_HWRF/RMN
      DO I=1,IT
        RF(I)=RF(I)*RMN_FACT
        R0(I)=R0(I)*RMN_FACT
      END DO

      RMN=RMN_HWRF

      PRINT*,'MEAN RADIUS=',RMN
      WRITE(85)RMN
!      WRITE(85)RF

      DO I=1,IT
        print *,'R0,Rf AT EACH DIRECTION ',I,R0(I),RF(I)
      ENDDO

      RETURN
      END

!! subroutine 'get_eta_level' returns the interface and
!! layer-mean pressures for reference.
       subroutine get_eta_level(npz, p_s, pf, ph, ak, bk, pscale)
       integer, intent(in) :: npz
       real(4), intent(in)  :: p_s            !< unit: pascal
       real(4), intent(in)  :: ak(npz+1)
       real(4), intent(in)  :: bk(npz+1)
       real(4), intent(in), optional :: pscale
       real(4), intent(out) :: pf(npz)
       real(4), intent(out) :: ph(npz+1)

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

      real function calc_dist(lat1,lon1,lat2,lon2)
      ! Distance using Haversine method for accuracy.  Should be good to
      ! about 100m for 32-bit floating point.

!     Earth radius from WRF-NMM module_model_constants.F as of HWRF R2
!     (WRF-NMM 3.2):
      real, parameter :: Rearth = 6370e3

      real, intent(in) :: lat1,lat2,lon1,lon2
      real :: res,a,b,c

      a=sin((lat2-lat1)/2.0)
      b=cos(lat2)*cos(lat1)
      c=sin((lon2-lon1)/2.0)
      res=sqrt(a*a+b*c*c)

      if(res>1) res=1
      if(res<-1) res=-1
      calc_dist= Rearth*2*asin(res);
      end function calc_dist
