
      SUBROUTINE CORT_MAT_2(IR1,NX,NY,NZ,KMX,U_2S,              &
		      T_2S,SLP_2S,Q_2S,RADIUS,temp_e,TEK,       &
		      T_X,Q_X,SLP_X,HLON,HLAT,VLON,VLAT,        &
		      CLON_NEW,CLAT_NEW,PS_C1,                  &
		      beta,fact,ics,SN)

! input:   IR1,NX,NY,KMX
! input:   U_2S,T_2S,Q_2S,SLP_2S variables from guess storm
! input:   HLON,HLAT,VLON,VLAT,TEK,temp_e
! input:   RADIUS,CLON_NEW,CLAT_NEW,ics
! output:  T_X,Q_X,SLP_X   -   new axisymmetric part

      real(4)   HLON(NX,NY),HLAT(NX,NY)
      real(4)   VLON(NX,NY),VLAT(NX,NY)

      REAL(8)   CLON_NEW,CLAT_NEW

      real(4)   T_X(NX,NY,KMX),Q_X(NX,NY,KMX),SLP_X(NX,NY)

      real(4)   RADIUS(IR1)
      real(4)   U_2S(IR1,KMX),T_2S(IR1,KMX)
      real(4)   Q_2S(IR1,KMX),SLP_2S(IR1)

      real(4)   TEK(NZ),temp_e(KMX)

      REAL(4), ALLOCATABLE :: RKX1(:),WKX1(:,:)
      REAL(4), ALLOCATABLE :: RKX2(:),WKX2(:,:)
      REAL(4), ALLOCATABLE :: strm1(:,:),strm2(:,:),funct(:,:)

      REAL(4), ALLOCATABLE :: rpb(:),rpb1(:),rpb2(:)
      REAL(4), ALLOCATABLE :: rmix(:,:)
      REAL(4), ALLOCATABLE :: ctmp(:,:),cmix(:,:)

      REAL(4), ALLOCATABLE :: test1(:,:),work1(:,:)

      REAL(4), ALLOCATABLE :: RIJ2(:,:)
!zhang      REAL(4), ALLOCATABLE :: IDX1(:,:),W1(:,:),W2(:,:)
      REAL(4), ALLOCATABLE :: W1(:,:),W2(:,:)
      INTEGER, ALLOCATABLE :: IDX1(:,:)

      REAL(4), ALLOCATABLE :: FUN1(:)
      REAL(4), ALLOCATABLE :: NDX1(:),WT1(:),WT2(:)
!
! Chanh
!
      CHARACTER SN*1

      IR=IR1-1
      nm=NZ
      kmx1=kmx-1

      NXC=NX/2
      NYC=NY/2

      ALLOCATE (  RKX1(IR1),WKX1(IR1,NZ) )
      ALLOCATE (  RKX2(IR1),WKX2(IR1,NZ) )
      ALLOCATE (  strm1(IR1,nm),strm2(IR1,nm),funct(IR1,nm) )

      ALLOCATE (  rpb(IR1),rpb1(IR1),rpb2(IR1) )
      ALLOCATE (  rmix(IR1,NM) )
      ALLOCATE (  ctmp(IR1,kmx),cmix(IR1,kmx) )

      ALLOCATE (  FUN1(IR1) )
      ALLOCATE (  NDX1(IR1),WT1(IR1),WT2(IR1) )

       pi=4.*atan(1.)
       pi180=pi/180.
       pi_deg=180./pi
       DST1=6.371E6*pi180

       cost=cos(clat_new*pi180)

       print*,'inside cort'
       print*,'pi= ',pi

! check max, min temp

      do k=1,kmx
        tmax=-1.E20
        tmin=1.E20
      do j=1,IR1
        if(T_2S(j,k).gt.tmax)then
           tmax=T_2S(j,k)
           jmax3=j
         end if
         if(T_2S(j,k).lt.tmin)then
           tmin=T_2S(j,k)
           jmin=j
          end if
      end do
       print*,'k,tmax,tmin=',k,tmax,tmin
      end do

      print*,'clon_new,clat_new=',clon_new,clat_new

       ff0=2.*7.292E-5*sin(clat_new*pi180)

      print*,'beta,fact,ff0=',beta,fact,ff0

      RKX1=0.
      DO n=1,IR1
        RKX1(n)=RADIUS(n)*DST1
!        print*,'RKX1(n)=',RKX1(n),U_2S(n,10)
      END DO

      RKX2=0.
      DO n=1,IR1
        RKX2(n)=RKX1(n)
      END DO

      k=1
      k1=2
!
! Chanh added modification here to recompute the stream
! function for the Southern Hemisphere,
!
      WKX1=0.
      DO n=1,IR1
        WKX1(n,k)=U_2S(n,k1)
        IF(WKX1(n,k).lt.0..and.SN.eq.'N') WKX1(n,k)=0.
        IF(WKX1(n,k).gt.0..and.SN.eq.'S') WKX1(n,k)=0.
      END DO

      WKX2=0.
      DO n=1,IR1
        WKX2(n,1)=U_2S(n,2)*beta
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
      end do

      print*,'finish computing stream function'

      IR_1=IR1
      DO n=1,IR1
        sum_str=strm1(n,1)
        if(abs(sum_str).gt.0.01)then
          IR_1=n
        end if
      END DO

      do m=1,IR_1
        str_cut=max(strm2(m,1),strm1(m,1))
	if(str_cut.gt.-10.)then
	  str_m_rat=strm2(m,1)/(strm1(m,1)-1.E-20)
	  IR_2=m
	  go to 57
	end if
      end do

 57   continue

      fun1=0.
      do m=1,IR_2
        fun1(m)=strm2(m,1)/(strm1(m,1)-1.E-20)
      end do

      do m=IR_2,IR_1
        fun1(m)=min(str_m_rat,strm2(m,1)/(strm1(m,1)-1.E-20))
      end do

! adjust func1 based on the center surface pressure

      adj_fun1=PS_C1/(fun1(1)*SLP_2S(1))

      rpb=0.
      do m=1,IR_1
        fun1(m)=fun1(m)*adj_fun1
	rpb(m) =fun1(m)*SLP_2S(m)
!	print*,'fun1(m)=',m,fun1(m),rpb(m),fun1(m)*T_2S(m,1)
      end do

      ctmp=0.
      do k=1,kmx
      do m=1,IR_1
        ctmp(m,k) =fun1(m)*T_2S(m,k)
      end do
      end do

!!!!!
! correct mixing ratio

      rmix=0.
      do k=1,NZ
	k1=2*k
        if (k1 .le. 121) then
	do m=1,IR_1
	  TEK1=temp_e(k1)+T_2S(m,k1)
	  TEK2=TEK(k)+ctmp(m,k1)
	  ESRR=exp(4302.645*(TEK2-TEK1)/((TEK2-29.66)*(TEK1-29.66)))
!	  rmix(m,k)=ESRR*Q_2S(m,k1)-Q_2S(m,k1)
	  rmix(m,k)=ESRR*Q_2S(m,k1)
	end do
        else
        do m=1,IR_1
        rmix(m,k)=0.0
        enddo
        endif
      end do

      do m=1,IR_1
!	rpb(m) =rpb(m)-SLP_2S(m)
	rpb(m) =rpb(m)
      end do

      do k=1,kmx
      do m=1,IR_1
!        ctmp(m,k) =ctmp(m,k)-T_2S(m,k)
        ctmp(m,k) =ctmp(m,k)
      end do
      end do

      cmix=0.
      do m=1,IR_1
	cmix(m,1)=rmix(m,1)
	cmix(m,kmx)=rmix(m,NZ)
	do k=2,kmx1,2
	  k1=k/2
	  cmix(m,k)=rmix(m,k1)
	end do
	do k=3,kmx1-1,2
	  k1=(k-1)/2
	  cmix(m,k)=0.5*(rmix(m,k1)+rmix(m,k1+1))
	end do
      end do

      do m=IR_1,IR1
	rpb(m)=0.
	do k=1,kmx
	  ctmp(m,k)=0.
	  cmix(m,k)=0.
        end do
      end do

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
            IF(DIF.GT.0.)THEN
              IDX1(I,J)=N
              GO TO 25
            END IF
          END DO
 25       CONTINUE
          IF(IDX1(I,J).GE.2)THEN
            W1(I,J)=(RIJ2(I,J)-RADIUS(IDX1(I,J)-1))/           &
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
       IF(IDX1(I,J).GT.0)THEN
         IF(IDX1(I,J).GE.2)THEN
          SLP_X(I,J)=W1(I,J)*rpb(IDX1(I,J))+W2(I,J)*rpb(IDX1(I,J)-1)
          DO K=1,KMX
            T_X(I,J,K)=W1(I,J)*ctmp(IDX1(I,J),k)+W2(I,J)*ctmp(IDX1(I,J)-1,k)
            Q_X(I,J,K)=W1(I,J)*cmix(IDX1(I,J),k)+W2(I,J)*cmix(IDX1(I,J)-1,k)
          END DO
         ELSE
           SLP_X(I,J)=rpb(IDX1(I,J))
           DO K=1,KMX
            T_X(I,J,K)=ctmp(IDX1(I,J),k)
            Q_X(I,J,K)=cmix(IDX1(I,J),k)
          END DO
         END IF
       END IF
      END DO
      END DO

      return
      END
