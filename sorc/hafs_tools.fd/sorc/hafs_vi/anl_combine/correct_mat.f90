
      SUBROUTINE CORT_MAT_1(IR1,NX,NY,NZ,KMX,             &
                      T_X,Q_X,SLP_X,HLON,HLAT,VLON,VLAT,  &
                      CLON_NEW,CLAT_NEW,                  &
                      beta,fact,aaa,bbb,iparam,ics,SN)

! input:   IR1,NX,NY,KMX
! input:   U_2S,T_2S,Q_2S,SLP_2S variables from guess storm
! input:   HLON,HLAT,VLON,VLAT,TEK,HP
! input:   RADIUS,CLON_NEW,CLAT_NEW,ics
! input:   U_2S1,RADIUS1 - Variables after wind and size correction
! output:  T_X,Q_X,SLP_X   -   new axisymmetric part
! output:  CFT  - correlation coef

      real(4)   HLON(NX,NY),HLAT(NX,NY)
      real(4)   VLON(NX,NY),VLAT(NX,NY)

!zhang      REAL(4)   CLON_NEW,CLAT_NEW
      REAL(8)   CLON_NEW,CLAT_NEW

      real(4)   T_X(NX,NY,KMX),Q_X(NX,NY,KMX),SLP_X(NX,NY)

      REAL(4), ALLOCATABLE :: RADIUS(:),RADIUS1(:)
      REAL(4), ALLOCATABLE :: U_2S(:,:),V_2S(:,:),U_2S1(:,:)
      REAL(4), ALLOCATABLE :: T_2S(:,:),Q_2S(:,:),SLP_2S(:)
      REAL(4), ALLOCATABLE :: TEK(:),HP(:,:,:)

      REAL(4), ALLOCATABLE :: RKX1(:),WKX1(:,:)
      REAL(4), ALLOCATABLE :: RKX2(:),WKX2(:,:)
      REAL(4), ALLOCATABLE :: strm1(:,:),strm2(:,:),strm3(:,:)

      REAL(4), ALLOCATABLE :: rpb(:),rpb1(:),rpb2(:)
      REAL(4), ALLOCATABLE :: rtmp(:,:),rtmp1(:,:),rtmp2(:,:),rtmp3(:,:)
      REAL(4), ALLOCATABLE :: rmix(:,:)
      REAL(4), ALLOCATABLE :: ctmp(:,:),cmix(:,:)

      REAL(4), ALLOCATABLE :: test1(:,:),work1(:,:)

      REAL(4), ALLOCATABLE :: RIJ2(:,:)
!zhang      REAL(4), ALLOCATABLE :: IDX1(:,:),W1(:,:),W2(:,:)
      REAL(4), ALLOCATABLE :: W1(:,:),W2(:,:)
      INTEGER, ALLOCATABLE :: IDX1(:,:)

      REAL(4), ALLOCATABLE :: CFT(:),FUN1(:),PW(:)
      REAL(4), ALLOCATABLE :: NDX1(:),WT1(:),WT2(:)
!Chanh
      CHARACTER SN*1

      IR=IR1-1
      nm=NZ
      kmx1=kmx-1

      NXC=NX/2
      NYC=NY/2

      ALLOCATE ( RADIUS(IR1),RADIUS1(IR1) )
      ALLOCATE ( U_2S(IR1,KMX),V_2S(IR1,KMX),U_2S1(IR1,KMX) )
      ALLOCATE ( T_2S(IR1,KMX),Q_2S(IR1,KMX),SLP_2S(IR1) )
      ALLOCATE ( TEK(KMX),HP(NX,NY,KMX) )

      ALLOCATE (  RKX1(IR1),WKX1(IR1,NZ) )
      ALLOCATE (  RKX2(IR1),WKX2(IR1,NZ) )
      ALLOCATE (  strm1(IR1,nm),strm2(IR1,nm),strm3(IR1,nm) )

      ALLOCATE (  rpb(IR1),rpb1(IR1),rpb2(IR1) )
      ALLOCATE (  rtmp(IR1,NM),rtmp1(IR1,NM),rtmp2(IR1,NM),rtmp3(IR1,NM) )
      ALLOCATE (  rmix(IR1,NM) )
      ALLOCATE (  ctmp(IR1,kmx),cmix(IR1,kmx) )

      ALLOCATE (  CFT(IR1),FUN1(IR1),PW(KMX) )
      ALLOCATE (  NDX1(IR1),WT1(IR1),WT2(IR1) )

      eps4 = 1.E-4
      eps5 = 1.E-5
      pi=4.*atan(1.)
      pi180=pi/180.
      pi_deg=180./pi
      DST1=6.371E6*pi180 !* deg -> m

      cost=cos(clat_new*pi180)

      print*,'inside cort'
      print*,'pi=,kmx= ',pi,kmx

      read(23)IR1,IR_1,JX,JY,JZ,KMX
      print*,'IR1,IR_1,JX,JY,JZ,KMX=',IR1,IR_1,JX,JY,JZ,KMX
      read(23)RADIUS
      print*,'test1=RADIUS',RADIUS(1),RADIUS(2)
      read(23)CLON_NEW,CLAT_NEW
      print*,'test1=CLON_NEW,CLAT_NEW'
      read(23)U_2S,V_2S,U_2S1
      print*,'test1=U_2S,V_2S,U_2S1'
      read(23)T_2S,Q_2S,SLP_2S
      print*,'test1=T_2S,Q_2S,SLP_2S'
      read(23)CFT,PW,beta11
      print*,'test1=CFT,PW,beta11',CFT(1),CFT(2)
      read(23)

      rewind(23)

! check max, min temp

      do k=1,kmx
         tmax=-1.E20
         tmin=1.E20
         DO j=1,IR1
         if (T_2S(j,k).GT.tmax) then
            tmax=T_2S(j,k)
            jmax3=j
         end if
         if (T_2S(j,k).LT.tmin) then
            tmin=T_2S(j,k)
            jmin=j
         end if
         END DO
         print*,'k,tmax,tmin=',k,tmax,tmin
      end do

      print*,'clon_new,clat_new=',clon_new,clat_new

      ff0=2.*7.292E-5*sin(clat_new*pi180)

      print*,'beta,fact,ff0=',beta,fact,ff0

      RKX1=0.
      DO n=1,IR1
         RKX1(n)=RADIUS(n)*DST1 !* deg -> m
         print*,'RKX1(n)=',RKX1(n),U_2S(n,10)
      END DO

      IF ( iparam == 1 ) THEN !* ---------------------------------------

      RKX2=0.
      DO n=1,IR1
         RADIUS1(n) = RADIUS(n)*fact
            RKX2(n) = RADIUS1(n)*DST1  !* deg -> m
      END DO

      PRINT*, 'Adjustment for old storm-size correction with 1 parameter.'

      ELSEIF ( iparam == 2 ) THEN !* -----------------------------------

!      if ( abs(bbb) < eps5 ) then !* - - - - - - - - - - - - - - - -
      if ( abs(bbb) < eps4 ) then !* - - - - - - - - - - - - - - - -

      RKX2=0.
      DO n=1,IR1
         RADIUS1(n) = aaa*RADIUS(n)
            RKX2(n) = RADIUS1(n)*DST1  !* deg -> m
      END DO

      PRINT*, 'Adjustment for storm-size correction reduced to 1 parameter.'

      else  !* - - - - - - - - - - - - - - - - - - - - - - - - - - -

      RKX2=0.
      DO n=1,IR1
         RADIUS1(n) = aaa*RADIUS(n) + .5*bbb*RADIUS(n)**2
            RKX2(n) = RADIUS1(n)*DST1  !* deg -> m
      END DO

      PRINT*, 'Adjustment for storm-size correction with 2 parameters.'

      endif !* - - - - - - - - - - - - - - - - - - - - - - - - - - -

      ENDIF !* ---------------------------------------------------------

      k=1
      k1=2

!
! Chanh added modification here to recompute the stream
! function for the Southern Hemisphere,
!
      WKX1=0.
      DO n=1,IR1
        WKX1(n,k)=U_2S1(n,k1)
        IF(WKX1(n,k).lt.0..and.SN.eq.'N') WKX1(n,k)=0.
        IF(WKX1(n,k).gt.0..and.SN.eq.'S') WKX1(n,k)=0.
      END DO

      WKX2=0.
      DO n=1,IR1
        WKX2(n,1)=U_2S1(n,2)*beta
        IF(WKX2(n,1).lt.0..and.SN.eq.'N') WKX2(n,1)=0.
        IF(WKX2(n,1).gt.0..and.SN.eq.'S') WKX2(n,1)=0.
      END DO

      strm1(IR1,k)=0.
      strm2(IR1,k)=0.
      DO n=IR,1,-1
         force=(WKX1(n,k)/(RKX1(n)+1.E-20)+ff0)*WKX1(n,k)
         strm1(n,k)=strm1(n+1,k)-force*(RKX1(n+1)-RKX1(n))
         force2=(WKX2(n,k)/(RKX2(n)+1.E-20)+ff0)*WKX2(n,k)
         strm2(n,k)=strm2(n+1,k)-force2*(RKX2(n+1)-RKX2(n))
      END DO

! Interpolate from RADIUS1 to RADIUS grid

!     IF (abs(fact-1.0).GT.0.01) THEN
         strm3=0.
         JST1=1
         DO J=1,IR1
	    DIF=RADIUS1(1)-RADIUS(J)
	    IF (DIF.GE.0.) THEN
	       strm3(J,k)=strm2(1,k)
	       JST1=J+1
	    END IF
         END DO
         DO J=JST1,IR1
            DO N=2,IR1
	       DIF=RADIUS1(N)-RADIUS(J)
	       IF (DIF.GE.0.) THEN
  	          WT11=(RADIUS(J)-RADIUS1(N-1))/(RADIUS1(N)-RADIUS1(N-1))
                  WT12=1.-WT11
	          strm3(J,k)=WT11*strm2(n,k)+WT12*strm2(n-1,k)
	          GO TO 55
	       END IF
            END DO
 55         CONTINUE
         END DO
         DO n=1,IR1
            strm2(n,k)=strm3(n,k)
	 END DO
!     END IF

      PRINT*,'finish computing stream function'

      do m=1,IR_1
	 str_cut=max(strm2(m,1),strm1(m,1))
	 IF (str_cut.GT.-10.) THEN
	    str_m_rat=strm2(m,1)/(strm1(m,1)-1.E-20)
	    IR_2=m
	    go to 57
	 END IF
      end do
 57   continue


      print*,'IR_1,IR_2=',IR_1,IR_2

      fun1=0.
      do m=1,IR_2
	 fun1(m)=cft(m)*strm2(m,1)/(strm1(m,1)-1.E-20)
      end do

      do m=IR_2,IR_1
         fun1(m)=cft(m)*min(str_m_rat,strm2(m,1)/(strm1(m,1)-1.E-20))
      end do


      rpb=0.
      do m=1,IR_1
!	 print*,'fun1(m),cft(m),strm2(m,1),strm1(m,1)=',m,fun1(m),cft(m),strm2(m,1),strm1(m,1)
	 rpb(m) =fun1(m)*SLP_2S(m)
      end do

      rtmp=0.
      do k=1,NZ
	 k1=2*k
         if (k1 .le. 121) then
         do m=1,IR_1
            rtmp(m,k) =fun1(m)*T_2S(m,k1)
!           if(k.eq.15)print*,'m,fun1(m),T_2S(m,k1)=',m,fun1(m),T_2S(m,k1)
         end do
         else
         do m=1,IR_1
            rtmp(m,k) = 0.0
         end do
         endif
      end do

!!!!!
! correct mixing ratio

      rmix=0.
      TEK=273.0
      do k=1,NZ
         k1=2*k
         if ( k1 .le. 121 ) then
         do m=1,IR_1
	    TEK1=TEK(k1)+T_2S(m,k1)
	    TEK2=TEK(k1)+rtmp(m,k)                   ! do not change k back to k1
	    TEK3=TEK(k1)+T_2S(m,k1)*cft(m)
	    ESRR=exp(4302.645*(TEK2-TEK1)/((TEK2-29.66)*(TEK1-29.66)))
	    ESR1=exp(4302.645*(TEK3-TEK1)/((TEK3-29.66)*(TEK1-29.66)))
	    rmix(m,k)=ESRR*Q_2S(m,k1)-ESR1*Q_2S(m,k1) ! will recompute
         end do
         else
         do m=1,IR_1
            rmix(m,k)=0.0 ! will recompute
         end do
         endif
      end do

      do m=1,IR_1
         print*,'rpb(m)=',m,rpb(m),cft(m),SLP_2S(m)
         rpb(m) =rpb(m)-cft(m)*SLP_2S(m)*beta
      end do

      do k=1,nz
         k1=2*k
         if ( k1 .le. 121 ) then
         do m=1,IR_1
            rtmp(m,k) =rtmp(m,k)-cft(m)*T_2S(m,k1)*beta
         end do
         else
         do m=1,IR_1
            rtmp(m,k) = 0.0
         end do
         endif
      end do

      ctmp=0.
      cmix=0.

      do m=1,IR_1
	 ctmp(m,1)=rtmp(m,1)
	 ctmp(m,kmx)=rtmp(m,NZ)
	 cmix(m,1)=rmix(m,1)
	 cmix(m,kmx)=rmix(m,NZ)
	 do k=2,kmx1,2
	    k1=k/2
	    ctmp(m,k)=rtmp(m,k1)
	    cmix(m,k)=rmix(m,k1)
	 end do
	 do k=3,kmx1-1,2
	    k1=(k-1)/2
	    ctmp(m,k)=0.5*(rtmp(m,k1)+rtmp(m,k1+1))
	    cmix(m,k)=0.5*(rmix(m,k1)+rmix(m,k1+1))
         end do
      end do

      DO K=1,KMX
      DO m=1,IR_1
         ctmp(m,k)=ctmp(m,k)*PW(k)
         cmix(m,k)=cmix(m,k)*PW(k)
      END DO
      END DO

! interpolate correction to 3D

      ALLOCATE ( RIJ2(NX,NY) )
      ALLOCATE ( IDX1(NX,NY),W1(NX,NY),W2(NX,NY) )

      IDX1=-1

      DO J=1,NY
      DO I=1,NX
         RIJ2(I,J)=SQRT(((HLON(I,J)-CLON_NEW)*cost)**2+     &
                         (HLAT(I,J)-CLAT_NEW)**2 )
         DO N=1,IR_1
            DIF=RADIUS(N)-RIJ2(I,J)
            IF (DIF.GT.0.) THEN
               IDX1(I,J)=N
               GO TO 25
            END IF
         END DO
 25      CONTINUE
         IF (IDX1(I,J).GE.2) THEN
            W1(I,J)=(RIJ2(I,J)-RADIUS(IDX1(I,J)-1))/        &
            (RADIUS(IDX1(I,J))-RADIUS(IDX1(I,J)-1))
            W2(I,J)=1.-W1(I,J)
         ELSE
            W1(I,J)=1.
            W2(I,J)=0.
         END IF
      END DO
      END DO

      SLP_X=0.
      T_X=0.
      Q_X=0.

      DO J=1,NY
      DO I=1,NX
         IF (IDX1(I,J).GT.0) THEN
         if (IDX1(I,J).GE.2) then
            SLP_X(I,J)=W1(I,J)*rpb(IDX1(I,J))+W2(I,J)*rpb(IDX1(I,J)-1)
            DO K=1,KMX
               T_X(I,J,K)=W1(I,J)*ctmp(IDX1(I,J),k)+W2(I,J)*ctmp(IDX1(I,J)-1,k)
               Q_X(I,J,K)=W1(I,J)*cmix(IDX1(I,J),k)+W2(I,J)*cmix(IDX1(I,J)-1,k)
            END DO
         else
            SLP_X(I,J)=rpb(IDX1(I,J))
            DO K=1,KMX
               T_X(I,J,K)=ctmp(IDX1(I,J),k)
               Q_X(I,J,K)=cmix(IDX1(I,J),k)
            END DO
         end if
         END IF
      END DO
      END DO

      RETURN
      END
