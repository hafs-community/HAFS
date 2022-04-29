        SUBROUTINE Split_Sym_Asy(IX,JX,KX,UD,VD,US,VS,UA,VA,TWM,RWM,  &
                        GLON,GLAT,CLON_NEW,CLAT_NEW,TWMAX,RWMAX,ASYM)


!   IX,JX must be less than IX1,JX1

!        PARAMETER (IX1=2000,JX1=2000)
        PARAMETER (IR=100, IT=24, DDR=0.1)
        PARAMETER (IR1=IR+1)
        REAL(8)   CLON_NEW,CLAT_NEW
        REAL(4)   UD(IX,JX,KX),VD(IX,JX,KX),GLON(IX,JX),GLAT(IX,JX)
        REAL(4)   US(IX,JX,KX),VS(IX,JX,KX),UA(IX,JX,KX),VA(IX,JX,KX)
        DIMENSION RWM(IR1),TWM(IR1)
        real, dimension(:,:), allocatable :: RIJ,W1,W2
        integer, dimension(:,:), allocatable :: IDX1
        integer :: IX1,JX1
!        DIMENSION RIJ(IX1,JX1),W1(IX1,JX1),W2(IX1,JX1)
!        INTEGER   IDX1(IX1,JX1)

        IX1=IX
        JX1=JX

        allocate(RIJ(IX1,JX1),W1(IX1,JX1),W2(IX1,JX1),IDX1(IX1,JX1))

        pi=4.*atan(1.)
        pi180=pi/180.
        cost=cos(clat_new*pi180)

        US=0.
        VS=0.
        IDX1=-1
        W1=0.
        W2=0.

        DO N=1,IR
          RWM(N)=N*DDR
        END DO
        DO J=1,JX
        DO I=1,IX
          RIJ(I,J)=SQRT(((GLON(I,J)-CLON_NEW)*cost)**2+      &
                       (GLAT(I,J)-CLAT_NEW)**2 )
          DO N=1,IR
            DIF=RWM(N)-RIJ(I,J)
            IF(DIF.GT.0.)THEN
              IDX1(I,J)=N
              GO TO 15
            END IF
          END DO
 15       CONTINUE
          IF(IDX1(I,J).GE.2)THEN
            W1(I,J)=(RIJ(I,J)-RWM(IDX1(I,J)-1))/             &
                 (RWM(IDX1(I,J))-RWM(IDX1(I,J)-1))
            W2(I,J)=1.-W1(I,J)
          ELSE IF(IDX1(I,J).EQ.1)THEN
            W1(I,J)=RIJ(I,J)/RWM(IDX1(I,J))
!            W2(I,J)=1.-W1(I,J)
          END IF
        END DO
        END DO


      DO K=1,KX
        CALL FIND_WT1(IX,JX,UD(1,1,K),VD(1,1,K),GLON,GLAT,   &
                     TWM,RWM,ASYM,CLON_NEW,CLAT_NEW)

        IF(K.eq.1)THEN
          TWMAX=TWM(IR1)
          RWMAX=RWM(IR1)
        END IF

        DO J=1,JX
        DO I=1,IX

         IF(IDX1(I,J).GT.0)THEN
          IF(IDX1(I,J).GE.2)THEN
            UTT=W1(I,J)*TWM(IDX1(I,J))+W2(I,J)*TWM(IDX1(I,J)-1)
          ELSE
            UTT=W1(I,J)*TWM(IDX1(I,J))          ! WT=0. at center
          END IF

          DTX=GLON(I,J)-CLON_NEW
          DTY=GLAT(I,J)-CLAT_NEW
          DTR=RIJ(I,J)

          US(I,J,K)=-UTT*DTY/(DTR+1.E-20)
          VS(I,J,K)= UTT*DTX/(DTR+1.E-20)
        END IF

          UA(I,J,K)=UD(I,J,K)-US(I,J,K)
          VA(I,J,K)=VD(I,J,K)-VS(I,J,K)

        END DO
        END DO
      END DO

      print*,'max speed & radius at surface=',TWMAX,RWMAX

      deallocate(RIJ,W1,W2,IDX1)
      RETURN
      END




      SUBROUTINE FIND_WT1(IX,JX,UD,VD,GLON2,GLAT2,TWM,RWM,ASYM,   &
                        CLON_NEW,CLAT_NEW)
!      PARAMETER (IR=75,IT=24,ddr=0.2)
      PARAMETER (IR=100,IT=24,ddr=0.1)
      PARAMETER (IR1=IR+1)
      DIMENSION UD(IX,JX),VD(IX,JX)
      REAL(4)   GLON2(IX,JX),GLAT2(IX,JX)
      DIMENSION WTM(IR),RWM(IR1),TWM(IR1),WTM2(IR,IT),WTM1(IT)
      REAL(8)   CLON_NEW,CLAT_NEW

!
      PI=ASIN(1.)*2.
      RAD=PI/180.
      pi180=RAD
      cost=cos(clat_new*pi180)
!
      ix2=ix/2
      jx2=jx/2
      DDS=(((GLON2(ix2+1,jx2)-GLON2(ix2,jx2))*cost)**2+          &
              (GLAT2(ix2,jx2+1)-GLAT2(ix2,jx2))**2)
!     &         (GLAT2(ix2,jx2+1)-GLAT2(ix2,jx2))**2)*1.5

       print*,'ix,jx,ix2,jx2=',ix,jx,ix2,jx2
       print*,'CLON_NEW,CLAT_NEW=',CLON_NEW,CLAT_NEW
       print*,'GLON2,GLAT2=',GLON2(1,1),GLAT2(1,1)

      WTM=0.
      WTM2=0.

      BLON = CLON_NEW
      BLAT = CLAT_NEW

!.. CALCULATE TANGENTIAL WIND EVERY 0.1 deg INTERVAL
!..  20*20 deg AROUND 1ST GUESS VORTEX CENTER

      DO 10 JL=1,IR
      WTS= 0.
      DR = JL*ddr
      DO 20 IL=1,IT
      DD = (IL-1)*15*RAD
      DLON = DR*COS(DD)
      DLAT = DR*SIN(DD)
      TLON = BLON + DLON
      TLAT = BLAT + DLAT

!.. INTERPOLATION U, V AT TLON,TLAT AND CLACULATE TANGENTIAL WIND

      u1=0.
      v1=0.
      sum1=0.
      DO j=1,JX
      DO i=1,IX
        dist=(((GLON2(i,j)-TLON)*cost)**2+(GLAT2(i,j)-TLAT)**2)
        if(dist.lt.DDS)THEN
          dist1=1./(sqrt(dist)+1.E-20)
          sum1=sum1+dist1
          u1=u1+UD(i,j)*dist1
          v1=v1+VD(i,j)*dist1
        end if
      end do
      end do

      UT=u1/(sum1+1.e-20)
      VT=v1/(sum1+1.e-20)

!.. TANGENTIAL WIND
      WT = -SIN(DD)*UT + COS(DD)*VT
      WTS = WTS+WT
      WTM2(JL,IL)=WT
20    CONTINUE
      WTM(JL) = WTS/24.
!      print*,'JL,WTM(JL)=',JL,WTM(JL)
10    CONTINUE

! Southern Hemisphere
      IF(CLAT_NEW.LT.0)THEN
        DO JL=1,IR
          WTM(JL)=-WTM(JL)
        END DO
      END IF
! EnD SH

      TX = -10000000.
      DO KL = 1,IR
      IF(WTM(KL).GE.TX) THEN
      TX = WTM(KL)
      RRX = KL*ddr
      KLM=KL
      ENDIF
      ENDDO

        DO KL=1,IR
          TWM(KL)=WTM(KL)
          RWM(KL)=KL*ddr
        END DO
        TWM(IR1)=TX
        RWM(IR1)=RRX

      DO IL=1,IT
        WSUM=0.
        DO KL=1,IR
          WSUM=WSUM+WTM2(KL,IL)*RWM(KL)    ! area
        END DO
        WTM1(IL)=WSUM
      END DO

      WT_S=0.
      DO IL=1,IT
         WT_S=WT_S+WTM1(IL)
      END DO
      WT_S=WT_S/24.

      W_MAX=-1.E20
      W_MIN=1.E20
      DO IL=1,IT
        IF(WTM1(IL).GT.W_MAX)W_MAX=WTM1(IL)
        IF(WTM1(IL).LT.W_MIN)W_MIN=WTM1(IL)
      END DO
      ASYM=(W_MAX-W_MIN)/WT_S

      PRINT*,'Asym=',ASYM,W_MAX,W_MIN,WT_S


      RETURN
      END

