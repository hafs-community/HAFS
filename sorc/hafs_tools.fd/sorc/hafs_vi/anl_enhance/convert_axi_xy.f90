!******************************************************************************

      SUBROUTINE axisym_xy_new(NX,NY,NZ,KMX,                    &
                    HLON2,HLAT2,VLON2,VLAT2,                    &
                    CLON_NHC,CLAT_NHC,                          &
                    SLP_1,T_1,Q_1,U_1,V_1,th1,rp1,              &
                    SLPE,TENV,PCST,HP,HV,ZMAX,vobs,             &
		    dp_obs,p_obs,vrmax,PRMAX,RMN,               &
		    U_2SB,T_2SB,SLP_2SB,R_2SB,temp_e,DEPTH,SN)
! SUBPROGRAM
!   PRGRMMR
!
! ABSTRACT
!
!     All variables have the same vertical dimension (KMAX=121).
!
      INTEGER I,J,K,NX,NY,NZ,ICH
!
      PARAMETER (NST=5)
!     PARAMETER (NX=420,NY=820,NZ=42) !* E-grid dimensions
      PARAMETER (GAMMA=6.5E-3,G=9.8,Rd=287.05,D608=0.608)
      PARAMETER (Cp=1004.)
      PARAMETER (IR=200,KMAX=121)    !* Cylinder dimensions
      PARAMETER (IR1=IR+1)          !* unused extra point

! READ Hurricane Pert.

      REAL(4)  HLON2(NX,NY),HLAT2(NX,NY)
      REAL(4)  VLON2(NX,NY),VLAT2(NX,NY) !* E-grid coord
      REAL(4)  PCST(KMAX),HP(NX,NY,KMAX),HV(NX,NY,KMAX)

      REAL(4)  SLPE(NX,NY),SLP_1(NX,NY),TENV(NX,NY,KMAX)
      REAL(4)  T_1(NX,NY,KMAX),Q_1(NX,NY,KMAX) !* adjusted vortex
      REAL(4)  U_1(NX,NY,KMAX),V_1(NX,NY,KMAX) !* on E-grid

      REAL(8) CLON_NHC,CLAT_NHC !* storm  lon, lat
      REAL(8) delc,thac         !* vortex lon, lat

      DIMENSION rp(IR1),ps(IR),ps1(IR),ps2(IR),ps_1mb(IR)
      DIMENSION q(KMAX),p(KMAX,IR) !* pressure on sigma-levels (q)
      DIMENSION t(KMAX,IR),r(KMAX,IR),ur(KMAX,IR1),th(KMAX,IR1)
      DIMENSION vrad(KMAX),vtan(KMAX) !^ bogus vortex

      REAL(4), ALLOCATABLE :: RIJ1(:,:),RIJ2(:,:)
      REAL(4), ALLOCATABLE :: W1(:,:),W2(:,:)
      INTEGER, ALLOCATABLE :: IDX1(:,:)

      REAL(4) SLPE1,temp_e(KMAX)
      REAL(4) th1(IR1),rp1(IR1)   ! ,RMN  !* min radius for filtering

      REAL(4) wrk1(IR),wrk2(IR),wrk3(IR),wrk4(IR)
      REAL(4) work1(KMAX),WORK2(KMAX)

      REAL(4) U_2SB(IR1,KMAX),T_2SB(IR1,KMAX),SLP_2SB(IR1)
      REAL(4) V_2SB(IR1,KMAX),R_2SB(IR1,KMAX) !* sigma-level

      DIMENSION RF(24)

      CHARACTER DEPTH*1,SN*1
!     CHARACTER SN*1,EW*1

!     rewind 11
!     read(11,11)ICLAT,SN,ICLON,EW
! 11  format(33x,I3,A1,I5,A1)
!     rewind 11

!     CLAT_NHC=ICLAT*0.1
!     CLON_NHC=ICLON*0.1

!     IF(SN.eq.'S')CLAT_NHC=-CLAT_NHC
!     IF(EW.eq.'W')CLON_NHC=-CLON_NHC

!     CLAT_NHC=25.
!     CLON_NHC=-60.

      eps6=1.E-6
      pi=4.*atan(1.0)
      pi180=pi/180.       !* deg -> rad
      arad =6.371E6*pi180 !* deg ->  m
      deg2m=6.371E6*pi180 !* deg ->  m
      cost=cos(CLAT_NHC*pi180)

      zmax=0.

       NHCT=77
       IF(DEPTH.eq.'S')THEN
         NHCT=77
       ELSE IF(DEPTH.eq.'M')THEN
         NHCT=76
       ELSE IF(DEPTH.eq.'D'.and.vobs.gt.20.)THEN
         NHCT=75
       END IF

       if(p_obs.lt.89510.)then
         NHCT=77
         print*,'minimum pressure < 900 mb', p_obs
       end if

      READ(NHCT)delc,thac    !* vortex lon, lat

      READ(NHCT)SLPE1        !* environment SLP (1)
      READ(NHCT)PCST         !* vortex p-levels (kmax)
      READ(NHCT)temp_e       !* environment  T  (kmax)

      SLPE=SLPE1           !* environment SLP (nx,ny)

      print*,'delc,thac=',delc,thac

      cost_old=cos(thac*pi180)     !* vortex cos(lat)

      READ(NHCT)(rp(i),i=1,IR1)      !* vortex radius [deg]

      do i=1,IR1
         rp(i)=arad*rp(i)          !* rp(i) = i*dr   [m]
      end do
      print*,'rp1,2,200=',rp(1),rp(2),rp(IR)

      do k=1,kmax
         READ(NHCT)(ur(k,i),i=1,IR1) !* vortex radial wind
         READ(NHCT)(th(k,i),i=1,IR1) !* vortex tangen wind
         print*,'k,th1,2,200=',k,th(k,1),th(k,2),th(k,IR)
         if(p_obs.lt.89510.)then
           do i=1,IR1
!            ur(k,i)=ur(k,i)*0.1    !* reduce convergence
            ur(k,i)=ur(k,i)*0.5    !* reduce convergence
           end do
         else
           do i=1,IR1
            ur(k,i)=ur(k,i)*0.1    !* reduce convergence
           end do
         end if
      end do

      READ(NHCT)(ps(i),i=1,IR)       !* vortex sfc pressure
      print*,'ps1,2,200=',ps(1),ps(2),ps(IR)

      do k=1,kmax
         READ(NHCT)(t(k,i),i=1,IR)   !* vortex temperature
         print*,'k,t1,2,200=',k,t(k,1),t(k,2),t(k,IR)
      end do

      do k=1,kmax
         READ(NHCT)(r(k,i),i=1,IR)   !* vortex mixing ratio
         print*,'k,r1,2,200=',k,r(k,1),r(k,2),r(k,IR)
      end do

      count_smth=0.

 999  continue  !* smooth vortex for large & weak storm <---------------

      TWMAX=1.e-6
      do i=1,IR
         th1(i)=th(1,i)
!        twsum=th(1,i)**2+ur(1,i)**2
         twsum=th(1,i)**2
         if (twsum.GT.TWMAX) then
            TWMAX=twsum
            RWMAX=rp(i)
         end if
      end do

      TWMAX=sqrt(TWMAX)    !* max tangen wind
      Rmax_0=RWMAX*0.001   !* RMW (m -> km)

      fact_v=vrmax/Rmax_0  !* alfa=RMW*/RMW
!CWH      print*,'fact=',fact,Rmax_0,vrmax,TWMAX

      fact=fact_v
      print*,'fact=',fact,Rmax_0,vrmax,TWMAX

!     fact=sqrt(fact)      ! make it closer to 1

      IF (fact.GT.1.1 .AND. TWMAX.GT.vobs) THEN  ! smooth
         wrk1(1)=(2.*ps(1)+ps(2))/3.
!        wrk1(IR)=(ps(IR-1)+2.*ps(IR))/3.
         wrk1(IR)=0.
         do i=2,IR-1
            wrk1(i)=(ps(i-1)+ps(i)+ps(i+1))/3.
         end do
         do i=1,IR
            ps(i)=wrk1(i)
         end do
         DO k=1,kmax
            wrk1(1)=(ur(k,1)+ur(k,2))/3.
            wrk1(IR)=0.
            wrk2(1)=(th(k,1)+th(k,2))/3.
            wrk2(IR)=0.
            wrk3(1)=(2.*t(k,1)+t(k,2))/3.
!           wrk3(IR)=(t(k,IR-1)+2.*t(k,IR))/3.
            wrk3(IR)=0.
            wrk4(1)=(2.*r(k,1)+r(k,2))/3.
!           wrk4(IR)=(r(k,IR-1)+2.*r(k,IR))/3.
            wrk4(IR)=0.
            do i=2,IR-1
               wrk1(i)=(ur(k,i-1)+ur(k,i)+ur(k,i+1))/3.
               wrk2(i)=(th(k,i-1)+th(k,i)+th(k,i+1))/3.
               wrk3(i)=(t(k,i-1)+t(k,i)+t(k,i+1))/3.
               wrk4(i)=(r(k,i-1)+r(k,i)+r(k,i+1))/3.
            end do
            do i=1,IR
               ur(k,i)=wrk1(i)
               th(k,i)=wrk2(i)
               t(k,i)=wrk3(i)
               r(k,i)=wrk4(i)
            end do
         END DO
         count_smth=count_smth+1
         IF (count_smth.LE.250.) go to 999  !* ------------------------>
      END IF

      print*,'count_smth=',count_smth !* ===============================

      go to 557

 556  continue  !* UNUSED code for homogenizing ROCI with RMW <- - - - -

      pres_ct=dp_obs/ps(1)
      do i=1,IR
         ps_1mb(i)=ps(i)*pres_ct
      end do

      IRAD_1=1
      do i=1,IR
         if (abs(ps_1mb(i)).GT.100.) then
	    IRAD_1=I
         end if
      end do

      RAD_1=(IRAD_1+0.5)*(rp(2)-rp(1))*1.E-3
      fact_p=PRMAX/RAD_1 !* alfa=ROCI*/ROCI

!     fact_p=0.5*(fact_p+fact_v)
      fact_p=fact_v

      print*,'fact,fact_p=',fact,fact_p,PRMAX,RAD_1

      IF (fact .LT. fact_p) THEN  !* smooth => fact_p = fact_v
         wrk1(1)=(2.*ps(1)+ps(2))/3.
         wrk1(IR)=0.
         do i=2,IR-1
            wrk1(i)=(ps(i-1)+ps(i)+ps(i+1))/3.
         end do
         do i=1,IR
            ps(i)=wrk1(i)
         end do
         do k=1,kmax
            wrk3(1)=(2.*t(k,1)+t(k,2))/3.
            wrk3(IR)=0.
            wrk4(1)=(2.*r(k,1)+r(k,2))/3.
            wrk4(IR)=0.
            do i=2,IR-1
               wrk3(i)=(t(k,i-1)+t(k,i)+t(k,i+1))/3.
               wrk4(i)=(r(k,i-1)+r(k,i)+r(k,i+1))/3.
            end do
            do i=1,IR
               t(k,i)=wrk3(i)
               r(k,i)=wrk4(i)
            end do
         end do
         go to 556 !* - - - - - - - - - - - - - - - - - - - - - - - - ->
      END IF

 557  continue

!*    50% contraint for bogus vortex stretch
!*    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if(fact.lt.0.5)fact=0.5
      if(fact.gt.1.5)fact=1.5

!     fact=1.0

! correct sea level pressure for diff. latitude (UNUSED ps1 by #386)

      ps1=0.

      FC2=2.*7.292E-5*SIN(CLAT_NHC*pi180)
      FC1=2.*7.292E-5*SIN(thac*pi180)
      DFC=FC2-FC1

      density=1.1765     ! P/(RT)=101300/(287*300)

      sum_vt=0.
      do i=IR-1,1,-1
         sum_vt=sum_vt+0.5*(th(1,i+1)+th(1,i))*(rp(i+1)-rp(i))
         ps1(i)=ps1(i)+DFC*sum_vt*density
      end do

! correct sea level pressure for fact (UNUSED ps1 by #386)

      fact1=1./fact-1.
      sum_vt2=0.
      do i=IR-1,1,-1
         th_m=0.5*(th1(i+1)+th1(i))
         sum_vt2=sum_vt2+th_m**2*(ALOG(rp(i)/rp(i+1)))
         ps1(i)=ps1(i)+fact1*sum_vt2*density
!        print*,'i,ps1(i)=',i,ps1(i)
      end do

!           *     *     *     *     *     *     *     *     *     *

!*    RMW of bogus vortex
!*    ~~~~~~~~~~~~~~~~~~~
      xxx = 1.E-6
      DO i = 1,IR
         yyy = th(1,i)**2
         IF ( yyy .GT. xxx ) THEN
            xxx  = yyy
            rmw1 = rp(i)
         ENDIF
      ENDDO

!*    ROCI of bogus vortex
!*    ~~~~~~~~~~~~~~~~~~~~
      xxx=dp_obs/ps(1)
      DO i = 1,IR
         ps2(i)=ps(i)*xxx
      ENDDO

      DO i=1,IR
         IF ( abs(ps2(i)) .GT. 50. ) THEN !* Bogus ROCI ?
            roc1 = rp(i)
         ENDIF
      ENDDO

      roc1 = roc1 + .5*(rp(2)-rp(1))
      roc1 = max(roc1,2.*rmw1+1.)

!*    Observed RMW & ROCI
!*    ~~~~~~~~~~~~~~~~~~~
      rmw2 = vrmax*1000.  !* RMW  (km -> m)
      roc2 = prmax*1000.  !* ROCI (km -> m)

      PRINT*, 'Bogus RMW ,Observed RMW  [km]: rmw1,vrmax =', rmw1*.001, vrmax
      PRINT*, 'Bogus ROCI,Observed ROCI [km]: roc1,prmax =', roc1*.001, prmax

!*    Stretch factors in meters
!*    ~~~~~~~~~~~~~~~~~~~~~~~~~
      xxx  = .5*rmw1 ; yyy = 1.5*rmw1  !* 50% Constraint
      rmw2 =  max(xxx,min(rmw2,yyy))   !* for bogus stretch
      xxx  = .5*roc1 ; yyy = 1.5*roc1  !* 50% Constraint
      roc2 =  max(xxx,min(roc2,yyy))   !* for bogus stretch

      ddd = 1./(roc1*rmw1*(roc1-rmw1))
      aaa = (rmw2*roc1**2-rmw1**2*roc2)*ddd
      bbb = 2.*(roc2*rmw1-roc1*rmw2)*ddd
      bbb = max(-0.05/deg2m,min(bbb,0.05/deg2m))

      aaa = max( 0.5, min(aaa,1.5) )

      iparam = 2  !* Parameters for storm-size correction ?

      IF ( iparam == 1 ) THEN !* ---------------------------------------

      DO i=1,IR1
         rp(i)=rp(i)*fact     !* stretch with RMW only (fact = fact_v)
        rp1(i)=rp(i)
      ENDDO

      PRINT*, 'Using 1 parameter for storm-size correction.'

      ELSEIF ( iparam == 2 ) THEN !* -----------------------------------

      DO i=1,IR1
         rp(i)=aaa*rp(i) + .5*bbb*rp(i)**2 !* stretch with RMW & ROCI
        rp1(i)=rp(i)
      ENDDO

      PRINT*, 'Using 2 parameters for storm-size correction.'
      PRINT*, 'Bogus RMW , Target  RMW   [m]: rmw1, rmw2 =', rmw1, rmw2
      PRINT*, 'Bogus ROCI, Target  ROCI  [m]: roc1, roc2 =', roc1, roc2
      PRINT*, 'Bogus storm-size factors  [m]:  aaa, bbb  =',  aaa, bbb
      PRINT*, 'Bogus stretch difference 0-10 deg: b*R/2  =', bbb*5.*deg2m

      ENDIF !* ---------------------------------------------------------

!           *     *     *     *     *     *     *     *     *     *

!     do i=1,IR
!        ps1(i)=ps1(i)*1.2    ! only correct 75%
!     end do

!     ps1=0.

!*    Correct vortex based on RMN (min radius for filtering)

!     rewind(85)
!     read(85)RMN
!     read(85)RF

      print*,'RMN 1=',RMN

!     PRMAX2=PRMAX*1000./arad
!     RMN=min(RMN,2.*PRMAX2)

      RMN1=RMN*arad

      if (RMN1.LT.2.5*RWMAX) then
         RMN1=2.5*RWMAX   !* RMN1 >= 2.5*RMW
         RMN=2.5*RWMAX/arad
!        rewind(85)
!        write(85)RMN
!        write(85)RF
         print*,'new RMN=',RMN
      end if

!      RMN2=0.5*(RMN1+vrmax*1000.)  !* in meter
!      RMN2=max(PRMAX*1000.,RMN2)   !* RMN2 >= ROCI*
      RMN2=0.9*PRMAX*1000.

      do i=1,IR-1
         if ( RMN2.GE.rp1(i) .AND. RMN2.LT.rp1(i+1) ) then
            icut1=i+1              !* icut1 -> RMN2
         end if
      end do

      go to 777 !* UNUSED cutoff beyond RMN2 - - - - - - - - - -

         do i=1,icut1-1
             ps(i)= ps(i)- ps(icut1)+ ps(IR)
            ps1(i)=ps1(i)-ps1(icut1)+ps1(IR)
            do k=1,kmax
               ur(k,i)=ur(k,i)-ur(k,icut1)+ur(k,IR)
               th(k,i)=th(k,i)-th(k,icut1)+th(k,IR)
               t(k,i)=t(k,i)-t(k,icut1)+t(k,IR)
               r(k,i)=max(0.,r(k,i)-r(k,icut1))+r(k,IR)
            end do
         end do

         do i=icut1,IR
             ps(i)= ps(IR)
            ps1(i)=ps1(IR)
            do k=1,kmax
               ur(k,i)=ur(k,IR)
               th(k,i)=th(k,IR)
               t(k,i)=t(k,IR)
               r(k,i)=r(k,IR)
            end do
         end do

 777  continue  !* - - - - - - - - - - - - - - - - - - - - - - -

      icut2=icut1+1.5*arad/(rp1(2)-rp1(1)) !* icut2 -> RMN2+3'

      if (icut2.gt.IR) icut2=IR

      print*,'icut1,icut2=',icut1,icut2

! special treatment for ur (UNUSED spread of divergence)

      do i=1,icut2
      do k=1,kmax
!        ur(k,i)=ur(k,i)-ur(k,icut2)*rp1(i)/rp1(icut2)
         ur(k,i)=ur(k,i)
      end do
      end do

!*    Cut off beyond RMN2+3'
      do i=icut2,IR
         ps(i)=ps(IR)
         ps1(i)=ps1(IR)
         do k=1,kmax
            ur(k,i)=0.
            th(k,i)=0.
            t(k,i)=t(k,IR)
            r(k,i)=r(k,IR)
         end do
      end do

!*    FADE from RMN2 to RMN2+3'
      do i=icut1,icut2 !* cut_off = 1 -> 0
         cut_off=FLOAT(icut2-i)/FLOAT(icut2-icut1)
         cut_off=cut_off*cut_off*(3.-2.*cut_off)
         print*,'i,cut_off=',i,cut_off
         ps(i)=(ps(i)-ps(IR))*cut_off+ps(IR)
         ps1(i)=(ps1(i)-ps1(IR))*cut_off+ps1(IR)
         do k=1,kmax !* FADING t(k,i) -> t(k,IR)
!           ur(k,i)=(ur(k,i)-ur(k,IR))*cut_off
            th(k,i)=(th(k,i)-th(k,IR))*cut_off
            t(k,i)=(t(k,i)-t(k,IR))*cut_off+t(k,IR)
            r(k,i)=(r(k,i)-r(k,IR))*cut_off+r(k,IR)
         end do
      end do

! END correction

      ps1=0.  !* UNUSED SLP correction for location & stretching

      do i=1,IR
         ps(i)=ps(i)+ps1(i)  ! new pert after correct latitude and fact
      end do
!

      do k=1,kmax
         q(k)=pcst(k)/pcst(1) !* sigma coord
      end do


      do i=1,IR
         ps1(i)=ps(i)+pcst(1) !* ps1 = total sfc pressure
!        print*,'i,ps,ps1=',i,ps(i),ps1(i)
      end do

      do k=1,kmax
         do i=1,IR
           p(k,i)=ps1(i)*q(k) !* pressure at sigma level
         end do
      end do

!     Interpolate const P data onto const sigma level.

!CWH      do i=1,IR1
!CWH         SLP_2SB(i)=ps(i)
!CWH      end do
      do i=1,IR
         SLP_2SB(i)=ps(i)
      end do
      SLP_2SB(IR1)=0

      go to 799 !* UNUSED p-to-sigma correction ------------------------

      DO I=1,IR
         DO N=1,KMAX
            work1(N)=t(N,I)+temp_e(N) !* total temperature
	 END DO
         DO K=1,kmax
            IF (p(k,i).GE.pcst(1)) THEN
	       U_2SB(i,k)=th(1,i)
	       V_2SB(i,k)=ur(1,i)
	       T_2SB(i,k)=work1(1)
	       R_2SB(i,k)=r(1,i)
            ELSEIF (p(k,i).LE.pcst(kmax)) THEN
	       U_2SB(i,k)=th(kmax,i)
	       V_2SB(i,k)=ur(kmax,i)
	       T_2SB(i,k)=work1(kmax)
	       R_2SB(i,k)=r(kmax,i)
	    ELSE !* p-to-sigma interpolation
	       DO N=1,kmax
	       if ( p(k,i).LE.pcst(N) .AND. p(k,i).GT.pcst(N+1) ) then
	          WT1=ALOG(1.*pcst(N+1))-ALOG(1.*pcst(N))
		  WT2=(ALOG(1.*p(k,i))-ALOG(1.*pcst(N)))/WT1
		  WT3=1.-WT2
		  U_2SB(i,k)=WT3*th(N,i)+WT2*th(N+1,i)
		  V_2SB(i,k)=WT3*ur(N,i)+WT2*ur(N+1,i)
		  T_2SB(i,k)=WT3*work1(N)+WT2*work1(N+1)
		  R_2SB(i,k)=WT3*r(N,i)+WT2*r(N+1,i)
                  GOTO 870
	       endif
	       ENDDO
  870          continue
	    ENDIF
	 END DO
      END DO

      TSUM1=0.
      TSUM2=0.

      DO I=1,IR
      DO K=1,KMAX
         TEK1=temp_e(K)+t(k,i) !* total temperature
         TEK2=T_2SB(i,k)
         ESRR=exp(4302.645*(TEK2-TEK1)/((TEK2-29.66)*(TEK1-29.66)))
         R_2SB(i,k)=ESRR*r(k,i)
         T_2SB(i,k)=T_2SB(i,k)-temp_e(K)    !* perturbation temperature
!        T_2SB(i,k)=0.5*(T_2SB(i,k)+t(k,i)) ! avg btw const P and const Sigma
!        T_2SB(i,k)=0.5*t(k,i)      ! average between const P and const Sigma
         TSUM1=TSUM1+t(k,i)
         TSUM2=TSUM2+T_2SB(i,k)
      END DO
      END DO

      print*,'TSUM1,TSUM2=',TSUM1,TSUM2

      TSUM1=TSUM1+TSUM2

      DO I=1,IR
      DO K=1,KMAX
         IF (ABS(TSUM1).GT.0.01) THEN
	    T_2SB(i,k)=(t(k,i)+T_2SB(i,k))*TSUM2/TSUM1
         ELSE
	    T_2SB(i,k)=0.
         END IF
         th(k,i)=U_2SB(i,k)
         ur(k,i)=V_2SB(i,k)
         t(k,i)=T_2SB(i,k)  !* perturbation temperature
         r(k,i)=R_2SB(i,k)
!        print*,'T_2SB(i,k)=',i,k,T_2SB(i,k)
      END DO
      END DO

 799  CONTINUE !* ------------------------------------------------------

        if(SN.eq.'S')then
          do k=1,kmax
          do i=1,IR1
            th(k,i)=-th(k,i)
          end do
          end do
        end if

!*    Using p-level instead of sigma-level
      DO I=1,IR
      DO K=1,KMAX
         U_2SB(i,k)=th(k,i) !* tangen wind !!
         V_2SB(i,k)=ur(k,i) !* radial wind !!
         T_2SB(i,k)=t(k,i)
         R_2SB(i,k)=r(k,i)
      END DO
      END DO

!*    Interpolate from cylin to E-grid
!*    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ALLOCATE ( RIJ1(NX,NY),RIJ2(NX,NY) )
      ALLOCATE ( IDX1(NX,NY),W1(NX,NY),W2(NX,NY) )

      do k=1,kmax
      do j=1,ny
      do i=1,nx
         TENV(i,j,k)=temp_e(k)
         HP(i,j,k)=p(k,IR)
         HV(i,j,k)=p(k,IR)
      end do
      end do
      end do

!*    Presume grid points outside vortex

      SLP_1=0.
      T_1=0.
      Q_1=0.
      U_1=0.
      V_1=0.

      IDX1=-1

!*    Interpolate winds to E-grid inside vortex

      DO J=1,NY
      DO I=1,NX   !* Relocate the vortex to storm center
         RIJ1(I,J)=arad*SQRT(((VLON2(I,J)-CLON_NHC)*cost)**2+      &
                              (VLAT2(I,J)-CLAT_NHC)**2 )
         DO N=1,IR
            DIF=rp(N)-RIJ1(I,J)
            IF (DIF.GT.0.) THEN
               IDX1(I,J)=N  !* Grid point inside vortex
               GO TO 15
            ENDIF
         ENDDO
 15      CONTINUE
         IF (IDX1(I,J).GE.2) THEN
            W1(I,J)=(RIJ1(I,J)-rp(IDX1(I,J)-1))/            &
                (rp(IDX1(I,J))-rp(IDX1(I,J)-1))
            W2(I,J)=1.-W1(I,J)
         ELSEIF (IDX1(I,J).EQ.1) THEN
            W1(I,J)=RIJ1(I,J)/rp(IDX1(I,J))
            W2(I,J)=0.
         ENDIF
      ENDDO
      ENDDO

      DO J=1,NY
      DO I=1,NX
      if (IDX1(I,J).GT.0) then

         IF (IDX1(I,J).GE.2) THEN
            DO K=1,KMAX
               vrad(K)=W1(I,J)*ur(k,IDX1(I,J))+W2(I,J)*ur(k,IDX1(I,J)-1)
               vtan(K)=W1(I,J)*th(k,IDX1(I,J))+W2(I,J)*th(k,IDX1(I,J)-1)
            END DO
         ELSEIF (IDX1(I,J).EQ.1) THEN
            DO K=1,KMAX
               vrad(K)=W1(I,J)*ur(k,IDX1(I,J))  ! WT=0. at center
               vtan(K)=W1(I,J)*th(k,IDX1(I,J))
            END DO
         ENDIF

         DTX=cost*(VLON2(I,J)-CLON_NHC)
         DTY=VLAT2(I,J)-CLAT_NHC
         DTR=RIJ1(I,J)/arad

!CWH         DO K=1,KMAX
!CWH            U_1(I,J,K)=(vrad(K)*DTX-vtan(K)*DTY)/(DTR+1.E-20)
!CWH            V_1(I,J,K)=(vrad(K)*DTY+vtan(K)*DTX)/(DTR+1.E-20)
!CWH            HV(I,J,K)=W1(I,J)*P(k,IDX1(I,J))+W2(I,J)*P(k,IDX1(I,J)-1)
!CWH         END DO

         IF (IDX1(I,J).GE.2) THEN
            DO K=1,KMAX
               U_1(I,J,K)=(vrad(K)*DTX-vtan(K)*DTY)/(DTR+1.E-20)
               V_1(I,J,K)=(vrad(K)*DTY+vtan(K)*DTX)/(DTR+1.E-20)
               HV(I,J,K)=W1(I,J)*P(k,IDX1(I,J))+W2(I,J)*P(k,IDX1(I,J)-1)
            END DO
         ELSEIF (IDX1(I,J).EQ.1) THEN
            DO K=1,KMAX
               U_1(I,J,K)=(vrad(K)*DTX-vtan(K)*DTY)/(DTR+1.E-20)
               V_1(I,J,K)=(vrad(K)*DTY+vtan(K)*DTX)/(DTR+1.E-20)
               HV(I,J,K)=W1(I,J)*P(k,IDX1(I,J))
            END DO
         ENDIF

      endif
      ENDDO
      ENDDO

!*    Interpolate ps, T, r to E-grid

      do i=1,IR
      do k=1,kmax
         if (r(k,i).LT.0.) r(k,i)=0.  !* positive-definite
      end do
      end do

      IDX1=-1     !* Presume grid point outside vortex

      DO J=1,NY
      DO I=1,NX   !* Relocate the vortex to storm center
         RIJ2(I,J)=arad*SQRT(((HLON2(I,J)-CLON_NHC)*cost)**2+     &
                              (HLAT2(I,J)-CLAT_NHC)**2 )
         DO N=1,IR
            DIF=rp(N)-RIJ2(I,J)
            IF (DIF.GT.0.) THEN
               IDX1(I,J)=N  !* Grid point inside vortex
               GO TO 25
            ENDIF
         ENDDO
 25      CONTINUE
         IF (IDX1(I,J).GE.2) THEN
            W1(I,J)=(RIJ2(I,J)-rp(IDX1(I,J)-1))/           &
                (rp(IDX1(I,J))-rp(IDX1(I,J)-1))
            W2(I,J)=1.-W1(I,J)
         ELSE
            W1(I,J)=1.
            W2(I,J)=0.
         ENDIF
      ENDDO
      ENDDO

      DO J=1,NY
      DO I=1,NX
      if (IDX1(I,J).GT.0) then
         IF (IDX1(I,J).GE.2) THEN
            SLP_1(I,J)=W1(I,J)*ps(IDX1(I,J))+W2(I,J)*ps(IDX1(I,J)-1)
            DO K=1,KMAX
               T_1(I,J,K)=W1(I,J)*t(k,IDX1(I,J))+W2(I,J)*t(k,IDX1(I,J)-1)
               Q_1(I,J,K)=W1(I,J)*r(k,IDX1(I,J))+W2(I,J)*r(k,IDX1(I,J)-1)
                HP(I,J,K)=W1(I,J)*P(k,IDX1(I,J))+W2(I,J)*P(k,IDX1(I,J)-1)
            ENDDO
         ELSE
            SLP_1(I,J)=ps(IDX1(I,J))
            DO K=1,KMAX
               T_1(I,J,K)=t(k,IDX1(I,J))
               Q_1(I,J,K)=r(k,IDX1(I,J))
                HP(I,J,K)=P(k,IDX1(I,J))
            ENDDO
         ENDIF
      endif
      ENDDO
      ENDDO

      RIJ_m=1.e20
      DO J=1,NY
      DO I=1,NX
         IF (RIJ2(I,J).LT.RIJ_m) THEN
            RIJ_m=RIJ2(I,J) !* min R
            II1=I
            JJ1=J
         ENDIF
      ENDDO
      ENDDO
      print*,'center inside axisym_xy=',HLON2(II1,JJ1),HLAT2(II1,JJ1)

      PIJ_m=1.e20
      DO J=1,NY
      DO I=1,NX
         IF (SLP_1(I,J).LT.PIJ_m) THEN
            PIJ_m=SLP_1(I,J) !* min Ps
            II1=I
            JJ1=J
         ENDIF
      ENDDO
      ENDDO

      print*,'center inside axisym_xy 2=',HLON2(II1,JJ1),HLAT2(II1,JJ1),PIJ_m

      END

!==============================================================================
