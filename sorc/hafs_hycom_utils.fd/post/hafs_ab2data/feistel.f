      function density(S_psu,t_degC,depth_m,p_dbar)
C  #####################################################################
C  #                                                                   #
C  #    FORTRAN front end to Reiner and Sabine Feistel 18 Nov 2003     #
C  #    thermodynamic function package to compute potential density    #
C  #    given salinity S_psu,temperature t_degC and                    #
C  #    depth depth_m    Potential density is referred to p_dbar       #
C  #    Note the approximation of pressure from depth.                 #
C  #####################################################################
      implicit none
      real*4 density,S_psu,t_degC,depth_m,p_dbar
      real*8 S,p,t,pref,C1,pot_t
      real*8 pottemp,gstp03,d


c no latitudinal dependence
      C1=5.92D-3
      p=((1-C1)-sqrt(((1-C1)**2)-(8.84D-6*depth_m)))/4.42D-6

c change precision
      S=S_psu*1.0D0
      t=t_degC*1.0D0

c set reference pressure
      pref=p_dbar*1.0D0

      
c Feistel estimate
      pot_t=POTTEMP(S, t, p, pref)

c the coefficients are setup in the call to POTTEMP

C     psu  -> psu
      S = S_psu
C     degC -> K
      T = pot_t + 273.15d0
C     dBar -> Pa
      P = pref * 1d4 + 101325d0

      D= 1.d0 / GSTP03(0, 0, 1, S, T, P)

c change precision
      density=d

      return
      end
      subroutine pot_t2t(S_psu,pot_t_degC,depth_m,t_degC)
C  #####################################################################
C  #                                                                   #
C  #    FORTRAN front end to Reiner and Sabine Feistel 18 Nov 2003     #
C  #    thermodynamic function package to compute temperature t_degC   #
C  #    given salinity S_psu,potential temperature pot_t_degC and      #
C  #    depth depth_m    Potential density is referred to 2000 dbar    #
C  #    Note the approximation of pressure from depth.                 #
C  #####################################################################
      implicit none
      real*8 S,pot_t,t
      real*4 S_psu,pot_t_degC,depth_m,t_degC,p_dbar
      real*8 pottemp,p,pref,C1

c no latitudinal dependence
      C1=5.92D-3
      p=((1-C1)-sqrt(((1-C1)**2)-(8.84D-6*depth_m)))/4.42D-6

c change precision
      S=S_psu*1.0D0
      pot_t=pot_t_degC*1.0D0

c set reference pressure
      pref=2000.0D0

c Feistel estimate
      t=POTTEMP(S, pot_t, pref, p)

c change precision
      t_degC=t
      return
      end
      
      subroutine t2pot_t(S_psu,t_degC,depth_m,pot_t_degC)
C  #####################################################################
C  #                                                                   #
C  #    FORTRAN front end to Reiner and Sabine Feistel 18 Nov 2003     #
C  #    thermodynamic function package to compute potentail temperature#
C  #    pot_t_degC given salinity S_psu,temperature t_degC and         #
C  #    depth depth_m    Potential density is referred to 2000 dbar    #
C  #    Note the approximation of pressure from depth.                 #
C  #####################################################################
      implicit none
      real*8 S,pot_t,t
      real*4 S_psu,pot_t_degC,depth_m,t_degC,p_dbar
      real*8 pottemp,p,pref,C1

c no latitudinal dependence
      C1=5.92D-3
      p=((1-C1)-sqrt(((1-C1)**2)-(8.84D-6*depth_m)))/4.42D-6

c change precision
      S=S_psu*1.0D0
      t=t_degC*1.0D0

c set reference pressure
      pref=2000.0D0

c Feistel estimate
      pot_t=POTTEMP(S,t, p, pref)

c change precision
      pot_t_degC=pot_t
      return
      end
      

cjl       other than this comments 
cjl       there are no changes to the PROGRAM DEMO_F03
cjl       in the  original package of Rainer & Feistel
      subroutine DEMO_F03

C  #####################################################################
C  #                                                                   #
C  #    FORTRAN Version by Rainer and Sabine Feistel 18 Nov 2003       #
C  #    Amended by David Webb 18 Oct 2004                              #
C  #                                                                   #
C  #    FORTRAN demo program to compute thermodynamic properties       #
C  #    of seawater from a thermodynamic potential function g(S,t,p).  #
C  #    Details are published in "Progress in Oceanography",           #
C  #    58 (2003) 43-114, entitled                                     #
C  #                                                                   #
C  #    "A new extended Gibbs thermodynamic potential of seawater"     #
C  #                                                                   #
C  #               by R.Feistel                                        #
C  #                                                                   #
C  #                                                                   #
C  #                 Dr. Rainer Feistel                                #
C  #                 Institut fuer Ostseeforschung, IOW                #
C  #                 D-18119 Warnemuende                               #
C  #                 Seestrasse 15                                     #
C  #                 GERMANY                                           #
C  #                                                                   #
C  #                 Tel: +49-381-5197-152                             #
C  #                 Fax: +49-381-5197-4818                            #
C  #                 Email: rainer.feistel@io-warnemuende.de           #
C  #                                                                   #
C  #####################################################################
C
C2345678901234567890123456789012345678901234567890123456789012345678901234567890
C        1         2         3         4         5         6         7         8
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     Array of polynomial coefficients
      DATA maxS/7/ 
      DATA maxT/7/ 
      DATA maxP/6/
      COMMON /Coeffs/gc(0:7,0:7,0:6)
C
C     error flag
      DATA ErrorReturn/-9.999999999999D+99/
C
      S = 35d0
      T = 20d0
      P = 2000d0
      CALL F03DEMO(S, T, P)

      END
C
C
      SUBROUTINE F03DEMO(Spsu, tdegC, pdbar)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     Array of polynomial coefficients
      DATA maxS/7/
      DATA maxT/7/
      DATA maxP/6/
C
      COMMON /Coeffs/gc(0:7,0:7,0:6)
C
C     error flag
      DATA ErrorReturn/-9.999999999999D+99/
C
      CALL COEFFS03
C
C     psu  -> psu
      S = Spsu
C     degC -> K
      T = tdegC + 273.15d0
C     dBar -> Pa
      P = pdbar * 1d4 + 101325d0
C
      WRITE(*,'(/)')
      WRITE(*,'(''S =  '',f5.2,'' psu'')') Spsu
      WRITE(*,'(''T =  '',f10.5,'' °C'')') tdegC
      WRITE(*,'(''P = '',f10.5,'' dbar'')') pdbar
      WRITE(*,'(/)')
      WRITE(*,'(''free enthalpy'', f20.12, '' J/kg'')')
     _ GSTP03(0, 0, 0, S, T, P)
      WRITE(*,'(''chem. pot.   '', f20.12, '' J/kg psu'')')
     _ GSTP03(1, 0, 0, S, T, P)
      WRITE(*,'(''entropy      '', f20.12, '' J/kgK'')')
     _ -GSTP03(0, 1, 0, S, T, P)
      WRITE(*,'(''density      '', f20.12, '' kg/m^3'')')
     _ 1 / GSTP03(0, 0, 1, S, T, P)
      WRITE(*,'(''heat capacity'', f20.12, '' J/kgK'')')
     _ -T * GSTP03(0, 2, 0, S, T, P)
      WRITE(*,'(''Ch. pot. H2O '', f20.12, '' J/kg'')')
     _ GSTP03(0, 0, 0, S, T, P) - S * GSTP03(1, 0, 0, S, T, P)
      WRITE(*,'(''therm. exp.  '', e20.15, '' 1/K'')')
     _ GSTP03(0, 1, 1, S, T, P) / GSTP03(0, 0, 1, S, T, P)
      WRITE(*,'(''compressib.  '', e20.15, '' 1/Pa'')')
     _ -GSTP03(0, 0, 2, S, T, P) / GSTP03(0, 0, 1, S, T, P)
      WRITE(*,'(''lapse rate   '', e20.15, '' K/Pa'')')
     _ -GSTP03(0, 1, 1, S, T, P) / GSTP03(0, 2, 0, S, T, P)
      WRITE(*,'(''pot. temp.   '', f20.12, '' °C'')')
     _ POTTEMP(Spsu, tdegC, pdbar, 0d0)
C
C     Printout example:
C	S =  35.00 psu
C	T =    20.00000 °C
C	P = 2000.00000 dbar
C
C
C	free enthalpy  16583.180671479684 J/kg
C	chem. pot.        60.009936669281 J/kg psu
C	entropy          276.780886190056 J/kgK
C	density         1033.329304335836 kg/m^3
C	heat capacity   3951.778371490321 J/kgK
C	Ch. pot. H2O   14482.832888054865 J/kg
C	therm. exp.  .278522499678412E-03 1/K
C	compressib.  .406129773355324E-09 1/Pa
C	lapse rate   .199948825300137E-07 K/Pa
C	pot. temp.        19.617987328589 °C

C
      RETURN
      END
C
C
C
      DOUBLEPRECISION FUNCTION GSTP03(nS, nT, nP, S, tabs, pabs)
C=======================================================
C computes partial derivatives of specific free enthalpy
C nS, nT, nP  order of partial derivative in S, T, P
C S in psu, tabs in K, pabs in Pa
C=======================================================
C
      IMPLICIT REAL*8 (A-H,O-Z)
C     Array of polynomial coefficients
      DATA maxS/7/
      DATA maxT/7/
      DATA maxP/6/
      COMMON /Coeffs/gc(0:7,0:7,0:6)
C
C     error flag
      DATA ErrorReturn/-9.999999999999D+99/
C
C     40 psu
      Data Sref/40d0/
C     40°C
      Data Tref/40d0/
C     100 MPa
      Data Pref/1d8/
C
C     preset: flag for error return
      GSTP03 = ErrorReturn
C
C     exclude crudely erratic calls:
      IF (nS .LT. 0 .OR. nT .LT. 0 .OR. nP .LT. 0) RETURN
      IF (S .LT. 0 .OR. tabs .LE. 0 .OR. pabs .LT. 0) RETURN
      IF (S .EQ. 0 .AND. nS .GT. 0 .AND. nP .EQ. 0 .AND. nT .LT. 2)
     _ RETURN
C
      If (gc(0, 0, 0) .EQ. 0) CALL COEFFS03
C
C  CHANGE - add d0 to constants
      x = SQRT(S / Sref)
      y = (tabs - 273.15d0) / Tref
      z = (pabs - 101325d0) / Pref
C
C     Derivatives of the polynomial part:
C     consider only lowest salinity derivatives
C
      IF (nS .EQ. 0) THEN
          gstp = Gxyz(0, nT, nP, x, y, z)
C
      ELSE IF (nS .EQ. 1) THEN
          IF (x .GT. 0) THEN
            gstp = Gxyz(1, nT, nP, x, y, z) / (2 * x)
          ELSE
            gstp = Gxyz(2, nT, nP, x, y, z) / 2
          END IF
C
      ELSE IF (nS .EQ. 2) THEN
          IF (x .GT. 0) THEN
            gstp = Gxyz(2, nT, nP, x, y, z) / (2 * x) ** 2
            gstp = gstp - Gxyz(1, nT, nP, x, y, z) / (4 * x ** 3)
          ELSE
            WRITE(*,*)'2. S-Derivative at S = 0 Not Implemented'
            RETURN
          END IF
C
      ELSE
          WRITE(*,*)'higher S-Derivatives Not Implemented'
          RETURN
C
      END IF
C
C     Add derivatives of the logarithm term:
C    (term with x^2*ln(x) is independent of pressure .AND. linear in temperature)
C
      IF (nP .EQ. 0 .AND. nT .LT. 2 .AND. x .GT. 0) THEN
C
        x2lnx = 0
C
C      'S derivative
C      'consider only lowest salinity derivatives
        IF (nS .EQ. 0) THEN
          x2lnx = x ** 2 * LOG(x)
        ELSE IF (nS .EQ. 1) THEN
          x2lnx = LOG(x) + 0.5d0
        ELSE IF (nS .EQ. 2) THEN
          x2lnx = 1 / (2d0 * x ** 2)
        ELSE
          WRITE(*,*)'higher S-Derivatives Not Implemented'
          RETURN
        END IF
C
C      'T derivative
        IF (nT .EQ. 0) THEN
          gstp = gstp + (gc(1, 0, 0) + gc(1, 1, 0) * y) * x2lnx
        ELSE
          gstp = gstp + gc(1, 1, 0) * x2lnx
        END IF
C
      END IF
C
      GSTP03 = gstp / (Sref ** nS * Tref ** nT * Pref ** nP)
      RETURN
      END
C
C
C
      DOUBLEPRECISION FUNCTION Gxyz(nx, ny, nz, x, y, z)
C=============================================================
C Compute the partial derivative (d/dx)^nx (d/dy)^ny (d/dz)^nz
C of the Sum gc(i,j,k) * x^i * y^j * z^k
C at given point x, y, z
C=============================================================
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     Array of polynomial coefficients
      DATA maxS/7/
      DATA maxT/7/
      DATA maxP/6/
      COMMON /Coeffs/gc(0:7,0:7,0:6)
C
C     error flag
      DATA ErrorReturn/-9.999999999999D+99/
C
      gijk = 0
      imax = maxS
      IF (x .EQ. 0) imax = nx
      jmax = maxT
      IF (y .EQ. 0) jmax = ny
      kmax = maxP
      IF (z .EQ. 0) kmax = nz
C
      xi = 1
      DO 100 i = nx , imax
C      'skip coefficients c(1,J,K), they belong to x²lnx , treated separately
        IF (i .EQ. 1) GoTo 91
C
        yj = 1
        DO 90 J = ny , jmax
C
          zk = 1
          DO 80 K = nz , kmax
C
            IF (gc(i, J, K) .NE. 0) THEN
              c = gc(i, J, K) * xi * yj * zk
              DO 71 l = 1 , nx
71              c = c * (i - l + 1)
              DO 72 l = 1 , ny
72              c = c * (J - l + 1)
              DO 73 l = 1 , nz
73              c = c * (K - l + 1)
              gijk = gijk + c
            END IF
C
            IF (K .LT. kmax) zk = zk * z
80        CONTINUE
          IF (J .LT. jmax) yj = yj * y
90      CONTINUE
91    CONTINUE
      IF (i .LT. imax) xi = xi * x
100   CONTINUE
C
      Gxyz = gijk
      RETURN
      END
C
C
C
      DOUBLEPRECISION FUNCTION POTTEMP(Spsu, tdegC, pdbar, pref_dbar)
C=======================================================
C compute potential temperatur (°C) for reference pressure pref (dbar)
C=======================================================
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     Array of polynomial coefficients
      DATA maxS/7/
      DATA maxT/7/
      DATA maxP/6/
      COMMON /Coeffs/gc(0:7,0:7,0:6)
C
C     error flag
      DATA ErrorReturn/-9.999999999999d+99/
C
      If (gc(0, 0, 0) .EQ. 0) CALL COEFFS03
C
      S = Spsu
      T = tdegC + 273.15d0
      P = pdbar * 1D4 + 101325d0
      Pref = pref_dbar * 1D4 + 101325d0
C
      POTTEMP = ErrorReturn
C
      theta = T
      sigma = -GSTP03(0, 1, 0, S, T, P)
C
      DO 100 iter = 1 , 100
C       'Newton iteration
        dt = -(sigma + GSTP03(0, 1, 0, S, theta, Pref)) /
     _       GSTP03(0, 2, 0, S, theta, Pref)
        theta = theta + dt
        IF (ABS(dt) .LT. 0.0001d0) THEN
          POTTEMP = theta - 273.15d0
          RETURN
        END IF
100   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE COEFFS03
C=================================================================================
C g(S,T,P) = (gc(1,0,0) + gc(1,1,0)*y)*x^2*ln(x) + Sum gc(i,j,k) * x^i * y^j * z^k
C                                                  ijk
C
C S = 40 PSU * x^2, T = 40 C * y, P = 100 MPa * z
C=================================================================================
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     Array of polynomial coefficients
      DATA maxS/7/
      DATA maxT/7/
      DATA maxP/6/
      COMMON /Coeffs/gc(0:7,0:7,0:6)
C
C     error flag
      DATA ErrorReturn/-9.999999999999D+99/
C
C     IAPWS95:
C     CP(0,T,0)
      gc(0, 2, 0) = -12357.785933039d0
      gc(0, 3, 0) = 736.741204151612d0
      gc(0, 4, 0) = -148.185936433658d0
      gc(0, 5, 0) = 58.0259125842571d0
      gc(0, 6, 0) = -18.9843846514172d0
      gc(0, 7, 0) = 3.05081646487967d0
C
C     IAPWS95:
C     V(0,T,0)
C     V(0,T,P)
C     dV/dt (0,t,p)
C     dV/dt (0,t,0)
C     dV/dp (0,t,p)
      gc(0, 0, 1) = 100015.695367145d0
      gc(0, 1, 1) = -270.983805184062d0
      gc(0, 2, 1) = 1455.0364540468d0
      gc(0, 3, 1) = -672.50778314507d0
      gc(0, 4, 1) = 397.968445406972d0
      gc(0, 5, 1) = -194.618310617595d0
      gc(0, 6, 1) = 63.5113936641785d0
      gc(0, 7, 1) = -9.63108119393062d0
      gc(0, 0, 2) = -2544.5765420363d0
      gc(0, 1, 2) = 776.153611613101d0
      gc(0, 2, 2) = -756.558385769359d0
      gc(0, 3, 2) = 499.360390819152d0
      gc(0, 4, 2) = -301.815380621876d0
      gc(0, 5, 2) = 120.520654902025d0
      gc(0, 6, 2) = -22.2897317140459d0
      gc(0, 0, 3) = 284.517778446287d0
      gc(0, 1, 3) = -196.51255088122d0
      gc(0, 2, 3) = 273.479662323528d0
      gc(0, 3, 3) = -239.545330654412d0
      gc(0, 4, 3) = 152.196371733841d0
      gc(0, 5, 3) = -55.2723052340152d0
      gc(0, 6, 3) = 8.17060541818112d0
      gc(0, 0, 4) = -33.3146754253611d0
      gc(0, 1, 4) = 28.9796526294175d0
      gc(0, 2, 4) = -55.5604063817218d0
      gc(0, 3, 4) = 48.8012518593872d0
      gc(0, 4, 4) = -26.3748377232802d0
      gc(0, 5, 4) = 6.48190668077221d0
      gc(0, 0, 5) = 4.20263108803084d0
      gc(0, 1, 5) = -2.13290083518327d0
      gc(0, 2, 5) = 4.34420671917197d0
      gc(0, 3, 5) = -1.66307106208905d0
      gc(0, 0, 6) = -0.546428511471039d0
C
C     IAPWS95 Reference State: energy=0 and entropy=0 at triple point of liquid water:
      gc(0, 0, 0) = 101.342743139672d0
      gc(0, 1, 0) = 5.90578348518236d0
C
C     enthalpy=0 and entropy=0 standard seawater
C     g(35,0,0) = 0:
      gc(2, 0, 0) = 1376.28030233939d0
C     dg(35,0,0)/dt = 0:
      gc(2, 1, 0) = 140.576997717291d0
C
C     MPD73 CP(S,T,0)
C     P80 V(S,T,0)
C     M76 V(S,T,0)
C     PG93 V(S,T,0)
C     Table IV
C     Table I-III
C     Table V-VI
C     Caldwell TMD
C     CM76 V(S,T,P)
C     BS70 V(S,t,P) - V(S,0,P)
C     BDSW70 CP(S,t,0)
      gc(2, 2, 0) = 929.460016974089d0
      gc(3, 2, 0) = -158.720177628421d0
      gc(2, 3, 0) = -260.427286048143d0
      gc(3, 3, 0) = 67.5232147262047d0
      gc(2, 4, 0) = 97.1562727658403d0
      gc(3, 4, 0) = -16.8901274896506d0
      gc(2, 5, 0) = -17.43743842213d0
      gc(2, 0, 1) = -3310.49154044839d0
      gc(3, 0, 1) = 199.459603073901d0
      gc(4, 0, 1) = -54.7919133532887d0
      gc(5, 0, 1) = 36.0284195611086d0
      gc(2, 1, 1) = 729.116529735046d0
      gc(3, 1, 1) = -175.292041186547d0
      gc(4, 1, 1) = -22.6683558512829d0
      gc(2, 2, 1) = -860.764303783977d0
      gc(3, 2, 1) = 383.058066002476d0
      gc(2, 3, 1) = 694.244814133268d0
      gc(3, 3, 1) = -460.319931801257d0
      gc(2, 4, 1) = -297.728741987187d0
      gc(3, 4, 1) = 234.565187611355d0
      gc(2, 0, 2) = 384.794152978599d0
      gc(3, 0, 2) = -52.2940909281335d0
      gc(4, 0, 2) = -4.08193978912261d0
      gc(2, 1, 2) = -343.956902961561d0
      gc(3, 1, 2) = 83.1923927801819d0
      gc(2, 2, 2) = 337.409530269367d0
      gc(3, 2, 2) = -54.1917262517112d0
      gc(2, 3, 2) = -204.889641964903d0
      gc(2, 4, 2) = 74.726141138756d0
      gc(2, 0, 3) = -96.5324320107458d0
      gc(3, 0, 3) = 68.0444942726459d0
      gc(4, 0, 3) = -30.1755111971161d0
      gc(2, 1, 3) = 124.687671116248d0
      gc(3, 1, 3) = -29.483064349429d0
      gc(2, 2, 3) = -178.314556207638d0
      gc(3, 2, 3) = 25.6398487389914d0
      gc(2, 3, 3) = 113.561697840594d0
      gc(2, 4, 3) = -36.4872919001588d0
      gc(2, 0, 4) = 15.8408172766824d0
      gc(3, 0, 4) = -3.41251932441282d0
      gc(2, 1, 4) = -31.656964386073d0
      gc(2, 2, 4) = 44.2040358308d0
      gc(2, 3, 4) = -11.1282734326413d0
      gc(2, 0, 5) = -2.62480156590992d0
      gc(2, 1, 5) = 7.04658803315449d0
      gc(2, 2, 5) = -7.92001547211682d0
C
C     limiting laws:
      gc(1, 0, 0) = 5813.28667992895d0
      gc(1, 1, 0) = 851.295871122672d0
      gc(3, 0, 0) = -2432.0947227047d0
      gc(3, 1, 0) = -493.512590658728d0
C
C     Colligative Properties
C     Fit of Freezing Points DK74
C     Fit of Dilution Heat Bromley
C     Fit of Dilution Heat MHH
      gc(4, 0, 0) = 2630.93863474177d0
      gc(5, 0, 0) = -2559.89065469719d0
      gc(6, 0, 0) = 1695.91780114244d0
      gc(7, 0, 0) = -466.680815621115d0
      gc(4, 1, 0) = 845.15825213234d0
      gc(5, 1, 0) = -810.552561548477d0
      gc(6, 1, 0) = 506.103588839417d0
      gc(7, 1, 0) = -129.049444012372d0
C
      RETURN
      END
