
      SUBROUTINE CORT_MAT(IR1,NX,NY,NZ,KMX,U_2S,T_2S,Q_2S,SLP_2S,     &
                      U_2S1,T_X,Q_X,SLP_X,HLON,HLAT,VLON,VLAT,        &
                      TEK,PCST,RADIUS,RADIUS1,CLON_NEW,CLAT_NEW,        &
		      CFT,beta11,IR_1,IFLAG,KM1)

! input:   IR1,NX,NY,KMX
! input:   U_2S,T_2S,Q_2S,SLP_2S variables from guess storm
! input:   HLON,HLAT,VLON,VLAT,TEK,PCST
! input:   RADIUS,CLON_NEW,CLAT_NEW,IFLAG
! input:   U_2S1,RADIUS1 - Variables after wind and size correction
! output:  T_X,Q_X,SLP_X   -   new axisymmetric part
! output:  A11,B11,C11  - correlation coef

      real(4)   U_2S(IR1,KMX),U_2S1(IR1,KMX)
      real(4)   T_2S(IR1,KMX),Q_2S(IR1,KMX),SLP_2S(IR1)
      real(4)   HLON(NX,NY),HLAT(NX,NY)
      real(4)   VLON(NX,NY),VLAT(NX,NY)
      real(4)   RADIUS(IR1),RADIUS1(IR1),CFT(IR1)

      REAL(8)   CLON_NEW,CLAT_NEW

      real(4)   T_X(NX,NY,KMX),Q_X(NX,NY,KMX),SLP_X(NX,NY)
      real(4)   TEK(KMX),PCST(KMX)

      real(4)   A11(NZ,3),B11(NZ,3),C11(3)
      real(4)   D11(3,3),f11(3)

      REAL(4), ALLOCATABLE :: RKX1(:),WKX1(:,:)
      REAL(4), ALLOCATABLE :: RKX2(:),WKX2(:,:)
      REAL(4), ALLOCATABLE :: strm1(:,:),strm2(:,:),funct(:,:),RLOGP(:)

      REAL(4), ALLOCATABLE :: rpb(:),rpb1(:),rpb2(:)
      REAL(4), ALLOCATABLE :: rtmp(:,:),rtmp1(:,:),rtmp2(:,:),rtmp3(:,:)
      REAL(4), ALLOCATABLE :: rmix(:,:)
      REAL(4), ALLOCATABLE :: ctmp(:,:),cmix(:,:)

      REAL(4), ALLOCATABLE :: test1(:,:),work1(:,:)

      REAL(4), ALLOCATABLE :: RIJ2(:,:)
      REAL(4), ALLOCATABLE :: IDX1(:,:),W1(:,:),W2(:,:)

      IR=IR1-1
      nm=NZ
      kmx1=kmx-1

      NXC=NX/2
      NYC=NY/2

      ALLOCATE (  RKX1(IR1),WKX1(IR1,NZ) )
      ALLOCATE (  RKX2(IR1),WKX2(IR1,NZ) )
      ALLOCATE (  strm1(IR1,nm),strm2(IR1,nm),funct(IR1,nm),RLOGP(KMX) )

      ALLOCATE (  rpb(IR1),rpb1(IR1),rpb2(IR1) )
      ALLOCATE (  rtmp(IR1,NM),rtmp1(IR1,NM),rtmp2(IR1,NM),rtmp3(IR1,NM) )
      ALLOCATE (  rmix(IR1,NM) )
      ALLOCATE (  ctmp(IR1,kmx),cmix(IR1,kmx) )

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
       print*,'k,tmax,tmin=',k,tmax,tmin,TEK(k)
      end do

      do k=1,kmx
	RLOGP(k)=287.*ALOG(PCST(k))       ! R=287
	print*,'RLOGP(k)=',K,RLOGP(k)
      end do

      print*,'clon_new,clat_new=',clon_new,clat_new

       ff0=2.*7.292E-5*sin(clat_new*pi180)

!CWH      print*,'beta11,fact,ff0=',beta11,fact,ff0
      print*,'beta11,ff0=',beta11,ff0

      RKX1=0.
      DO n=1,IR1
        RKX1(n)=RADIUS(n)*DST1
        RKX2(n)=RADIUS1(n)*DST1
!        print*,'RKX1(n)=',RKX1(n),U_2S(n,10)
      END DO

      print*,'KM1(should be 3)=',KM1
      KM2=KM1/2

      WKX1=0.
!      do k1=2,kmx1,2
      do k1=2,KM1,2
        k=K1/2
        DO n=1,IR1
          WKX1(n,k)=U_2S(n,k1)
          IF(WKX1(n,k).lt.0.)WKX1(n,k)=0.
        END DO
      END DO

      WKX2=0.
      do k1=2,KM1,2
        k=K1/2
        DO n=1,IR1
          WKX2(n,k)=U_2S1(n,k1)
          IF(WKX2(n,k).lt.0.)WKX2(n,k)=0.
        END DO
      END DO


      do k=1,KM2
        strm1(IR1,k)=0.
        strm2(IR1,k)=0.
        DO n=IR,1,-1
          force=(WKX1(n,k)/(RKX1(n)+1.E-20)+ff0)*WKX1(n,k)
          strm1(n,k)=strm1(n+1,k)-force*(RKX1(n+1)-RKX1(n))
          force2=(WKX2(n,k)/(RKX2(n)+1.E-20)+ff0)*WKX2(n,k)
          strm2(n,k)=strm2(n+1,k)-force2*(RKX2(n+1)-RKX2(n))
        end do
      end do

!      do k=1,NZ
!	print*,'strm1(1,k),strm2(1,k)=',k,strm1(1,k),strm2(1,k)
!      end do

      IR_1=IR1

      DO n=1,IR1
        sum_str=0.
        do k=1,KM2
          sum_str=sum_str+strm1(n,k)
        end do
        if(abs(sum_str).gt.0.001)then
          IR_1=n
        end if
!        write(*,222)n,strm2(n,1),strm1(n,1),strm2(n,1)/(-1.e-20+strm1(n,1))
      END DO

      print*,'finish computing stream function'

! rtmp1(n,1) not exactly the same level (half level above K=1 for NZ=42)

      k=1
      k1=2

      DO n=1,IR_1
	str_cut=max(strm2(n,1),strm1(n,1))
	if(str_cut.gt.-10.)then
	  str_m_rat=strm2(n,1)/(strm1(n,1)-1.E-20)
	  IR_2=n
          go to 57
	end if
      end do
 57   continue

      CFT=0.
      DO n=1,IR_2
	CFT(n)=strm2(n,1)/(strm1(n,1)-1.E-20)
      end do

      DO n=IR_2,IR_1
	CFT(n)=min(str_m_rat,strm2(n,1)/(strm1(n,1)-1.E-20))
      end do

      if(abs(beta11-1.).gt.0.01)then

      DO n=1,IR_1
!        rtmp1(n,1)=(strm1(n,2)-strm1(n,1))/(RLOGP(4)-RLOGP(2))
!        rtmp3(n,1)=(strm2(n,2)-strm2(n,1))/(RLOGP(4)-RLOGP(2))
!	CFT(n)=strm2(n,1)/(strm1(n,1)-1.E-20)
        rtmp1(n,1)=T_2S(n,2)
        rtmp3(n,1)=T_2S(n,2)*CFT(n)
!	print*,'n,rtmp1(n,2)=',strm2(n,k),strm1(n,k),strm2(n,k)/(strm1(n,k)-1.E-20)
      END DO
      do k=2,NZ
        k1=2*k
        if (k1 .le. 121) then
        DO n=1,IR_1
!          rtmp1(n,k)=(strm1(n,k+1)-strm1(n,k-1))/(RLOGP(k1+2)-RLOGP(k1-2))
!          rtmp3(n,k)=(strm2(n,k+1)-strm2(n,k-1))/(RLOGP(k1+2)-RLOGP(k1-2))
          rtmp1(n,k)=T_2S(n,k1)
          rtmp3(n,k)=T_2S(n,k1)*CFT(n)
        END DO
        else
        DO n=1,IR_1
        rtmp1(n,k)=0.0
        rtmp3(n,k)=0.0
        enddo
        endif
      end do

! new replacement
      rpb2=0.
      rpb=0.
      do m=1,IR_1
        rpb2(m)=SLP_2S(m)
	rpb(m) =SLP_2S(m)*CFT(m)
      end do

      rtmp2=0.
      rtmp=0.
      do k=1,NZ
	k1=k*2
        do m=1,IR_1
          rtmp2(m,k)=T_2S(m,k1)
!	  rtmp(m,k) =T_2S(m,k1)*rtmp3(m,k)/(rtmp1(m,k)-1.E-20)
	  rtmp(m,k) =T_2S(m,k1)*CFT(m)
        end do
      end do

! new replacement

      pms1=0.
      pms2=0.
      pms3=0.

      do m=1,IR1
        pms1=pms1+slp_2s(m)**2
        pms2=pms2+rpb2(m)**2
        pms3=pms3+(slp_2s(m)-rpb2(m))**2
      end do
      print*,'pms=',sqrt(pms1),sqrt(pms2),sqrt(pms2)/sqrt(pms1)
      print*,'pms3=',sqrt(pms3)

      tmp4=0
      tmp5=0.
      tmp6=0.
      tmpdiff1=0.
      do k=1,NZ
        k1=k*2
        tmp1=0.
        tmp2=0.
        tmp3=0.
      do m=1,IR1
        tmp1=tmp1+T_2S(m,k1)**2
        tmp2=tmp2+rtmp2(m,k)**2
        tmp3=tmp3+(T_2S(m,k1)-rtmp2(m,k))**2
      end do
        tmp4=tmp4+tmp1
        tmp5=tmp5+tmp2
        tmp6=tmp6+tmp3
      print*,'tmp1=',k,sqrt(tmp1),sqrt(tmp2),sqrt(tmp2)/sqrt(tmp1),sqrt(tmp3)
      end do

!      print*,'tmp6=',sqrt(tmp6/tmp4)

! correct mixing ratio

      rmix=0.

      do k=1,nz
	k1=2*k
        if (k1 .le. 121) then
	do m=1,IR_1
	  TEK1=TEK(k1)+T_2S(m,k1)
	  TEK2=TEK(k1)+rtmp(m,k)
	  ESRR=exp(4302.645*(TEK2-TEK1)/((TEK2-29.66)*(TEK1-29.66)))
!	  rmix(m,k)=ESRR*Q_2S(m,k)-Q_2S(m,k)
	  rmix(m,k)=ESRR*Q_2S(m,k)-Q_2S(m,k)*beta11     ! will recompute
	end do
        else
        do m=1,IR_1
        rmix(m,k)=0.0
        enddo
        endif
      end do

      do m=1,IR_1
        print*,'rpb(m),SLP_2S(m)',m,rpb(m),SLP_2S(m),CFT(m)
	rpb(m)=rpb(m)-SLP_2S(m)*beta11
      end do
      do k=1,nz
       k1=2*k
       if (k1 .le. 121) then
      do m=1,IR_1
	rtmp(m,k)=rtmp(m,k)-T_2S(m,k1)*beta11
!	print*,'m,k,rtmp(m,k),T_2S(m,k1)=',m,k,rtmp(m,k),T_2S(m,k1)
      end do
      else
      do m=1,IR_1
      rtmp(m,k)=0.0
      enddo
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

      do m=1,IR1
	print*,'ctmp,cmix,test1111=',m,ctmp(m,1),cmix(m,1)
      end do


! interpolate correction to 3D

       ALLOCATE ( RIJ2(NX,NY) )
       ALLOCATE ( IDX1(NX,NY),W1(NX,NY),W2(NX,NY) )

        IDX1=-1

!        print*,'HLON(1,1),HLAT(1,1),CLON_NEW,CLAT_NEW=',       &
!		HLON(1,1),HLAT(1,1),CLON_NEW,CLAT_NEW
!        print*,'RADIUS(1)=',RADIUS(1)

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
	END DO
	END DO
	DO J=1,NY
	DO I=1,NX
          IF(IDX1(I,J).GE.2)THEN
            W1(I,J)=(RIJ2(I,J)-RADIUS(IDX1(I,J)-1))/           &
                 (RADIUS(IDX1(I,J))-RADIUS(IDX1(I,J)-1))
            W2(I,J)=1.-W1(I,J)
          ELSE IF(IDX1(I,J).EQ.1)THEN
            W1(I,J)=1.
            W2(I,J)=0.
	  ELSE
	    W1(I,J)=0.
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
!	  IF(I.EQ.NX/2)print*,'SLP_X(I,J),W1(I,J),IDX1(I,J)=',    &
!			       SLP_X(I,J),W1(I,J),IDX1(I,J)
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

      else

         SLP_X=0.
         T_X=0.
         Q_X=0.

      end if

      return
      END
