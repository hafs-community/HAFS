      SUBROUTINE WNLSM(W,MDW,MME,MA,N,L,PRGOPT,X,RNORM,MODE,IPIVOT, &
        ITYPE,WD,H,SCALE,Z,TEMP,D)
!C***BEGIN PROLOGUE  WNLSM
!C***REFER TO  WNNLS
!C
!C     This is a companion subprogram to WNNLS( ).
!C     The documentation for WNNLS( ) has more complete
!C     usage instructions.
!C
!C     Written by Karen H. Haskell, Sandia Laboratories,
!C     with the help of R.J. Hanson, Sandia Laboratories,
!C     December 1976 - January 1978.
!C     Revised March 4, 1982.
!C
!C     In addition to the parameters discussed in the prologue to
!C     subroutine WNNLS, the following work arrays are used in
!C     subroutine WNLSM  (they are passed through the calling
!C     sequence from WNNLS for purposes of variable dimensioning).
!C     Their contents will in general be of no interest to the user.
!C
!C         IPIVOT(*)
!C            An array of length N.  Upon completion it contains the
!C         pivoting information for the cols of W(*,*).
!C
!C         ITYPE(*)
!C            An array of length M which is used to keep track
!C         of the classification of the equations.  ITYPE(I)=0
!C         denotes equation I as an equality constraint.
!C         ITYPE(I)=1 denotes equation I as a least squares
!C         equation.
!C
!C         WD(*)
!C            An array of length N.  Upon completion it contains the
!C         dual solution vector.
!C
!C         H(*)
!C            An array of length N.  Upon completion it contains the
!C         pivot scalars of the Householder transformations performed
!C         in the case KRANK.LT.L.
!C
!C         SCALE(*)
!C            An array of length M which is used by the subroutine
!C         to store the diagonal matrix of weights.
!C         These are used to apply the modified Givens
!C         transformations.
!C
!C         Z(*),TEMP(*)
!C            Working arrays of length N.
!C
!C         D(*)
!C            An array of length N that contains the
!C         column scaling for the matrix (E).
!C                                       (A)
!C***ROUTINES CALLED  H12,ISAMAX,SASUM,SAXPY,SCOPY,SNRM2,SROTM,SROTMG,
!C                    SSCAL,SSWAP,WNLIT,XERROR
!C***END PROLOGUE  WNLSM
!C
!C     THE EDITING REQUIRED TO CONVERT THIS SUBROUTINE FROM SINGLE TO
!C     DOUBLE PRECISION INVOLVES THE FOLLOWING CHARACTER STRING CHANGES.
!C     USE AN EDITING COMMAND (CHANGE) /STRING-1/(TO)STRING-2/.
!C     (BEGIN CHANGES AT LINE WITH C++ IN COLS. 1-3.)
!C     /REAL (12 BLANKS)/DOUBLE PRECISION/,/SASUM/DASUM/,/SROTMG/DROTMG/,
!C     /SNRM2/DNRM2/,/ SQRT/ DSQRT/,/SROTM/DROTM/,/AMAX1/DMAX1/,
!C     /SCOPY/DCOPY/,/SSCAL/DSCAL/,/SAXPY/DAXPY/,/E0/D0/,/SSWAP/DSWAP/,
!C     /ISAMAX/IDAMAX/,/SRELPR/DRELPR/
!C
!C     SUBROUTINE WNLSM (W,MDW,MME,MA,N,L,PRGOPT,X,RNORM,MODE,
!C    1                  IPIVOT,ITYPE,WD,H,SCALE,Z,TEMP,D)
!C++
      USE setparms
      implicit none
      REAL             W(MDW,1), X(1), WD(1), H(1), SCALE(1), DOPE(4)
      REAL             Z(1), TEMP(1), PRGOPT(1), D(1), SPARAM(5)
      REAL             ALAMDA, ALPHA, ALSQ, AMAX, BNORM, EANORM
      REAL             SRELPR, FAC, ONE, BLOWUP
      REAL             RNORM, SM, T, TAU, TWO, WMAX, ZERO, ZZ, Z2
      REAL             AMAX1, SQRT, SNRM2, SASUM, DNRM2, DASUM

      INTEGER IPIVOT(1), ITYPE(1), IDOPE(8)
      integer(kind = int_single) ISAMAX,IDAMAX
      LOGICAL HITCON, FEASBL, DONE, POS
      DATA ZERO /0.E0/, ONE /1.E0/, TWO /2.E0/, SRELPR /0.E0/

      integer KEY, NEXT
      integer ISOL, NP1, NSOLN, NIV, NIV1, KRP1, KRANK
      integer I,J,JJ,LP1, NSP1, JP, JM1, JCON, IWMAX
      integer ITEMP, ITMAX, IMAX, NM1, MODE, IOPT, NERR , LINK, LAST
      integer NLINK, NTIMES, ITER, NOPT
      integer MA, MME, ME, MEP1, M, MDW, L1, N, L

      integer ind59,ind91

!C
!C     INITIALIZE-VARIABLES
!C***FIRST EXECUTABLE STATEMENT  WNLSM
      !yonghui call sub998(SRELPR, M, MA, MME,MEP1)
      call sub998(SRELPR, M, MA, MME, ME, MEP1)
!C
!C     TO PROCESS-OPTION-VECTOR
      FAC = 1.E-4
!C
!C     THE NOMINAL TOLERANCE USED IN THE CODE,
      TAU = SQRT(SRELPR)
!C
!C     THE NOMINAL BLOW-UP FACTOR USED IN THE CODE.
      BLOWUP = TAU
!C
!C     THE NOMINAL COLUMN SCALING USED IN THE CODE IS
!C     THE IDENTITY SCALING.
      D(1) = ONE
      if (kind(D) == real_single) then
        CALL SCOPY(N, D, 0, D, 1)
      else if (kind(D) == real_double) then
        CALL DCOPY(N, D, 0, D, 1)
      endif
!C
!C     DEFINE BOUND FOR NUMBER OF OPTIONS TO CHANGE.
      NOPT = 1000
!C
!C     DEFINE BOUND FOR POSITIVE VALUE OF LINK.
      NLINK = 100000
      NTIMES = 0
      LAST = 1
      LINK = PRGOPT(1)
      IF ((LINK.LE.0 .OR. LINK.GT.NLINK)) then
        NERR = 3
        IOPT = 1
        CALL XERROR( 'WNNLS( ) THE OPTION VECTOR IS UNDEFINED', 39, &
                     NERR, IOPT)
        MODE = 2
        RETURN

      endif
      do
        IF (.NOT.(LINK.GT.1)) exit
        NTIMES = NTIMES + 1
        IF ((NTIMES.GT.NOPT)) then
          NERR = 3
          IOPT = 1
          CALL XERROR( 'WNNLS( ). THE LINKS IN THE OPTION VECTOR ARE &
               CYCLING.', 53,     NERR, IOPT)
          MODE = 2
          RETURN
        endif
        KEY = PRGOPT(LAST+1)
        IF ((KEY.EQ.6 .AND. PRGOPT(LAST+2).NE.ZERO)) then
          if (kind(W) == real_single) then
            do J=1,N
              T = SNRM2(M,W(1,J),1)
              IF (T.NE.ZERO) T = ONE/T
              D(J) = T
            enddo
          else if (kind(W) == real_double) then
            do J=1,N
              T = DNRM2(M,W(1,J),1)
              IF (T.NE.ZERO) T = ONE/T
              D(J) = T
            enddo
          endif
        endif

        IF (KEY.EQ.7) then
          if (kind(PRGOPT) == real_single) then
            CALL SCOPY(N, PRGOPT(LAST+2), 1, D, 1)
          else if (kind(PRGOPT) == real_double) then
            CALL DCOPY(N, PRGOPT(LAST+2), 1, D, 1)
          endif
        endif
        IF (KEY.EQ.8) TAU = AMAX1(SRELPR,PRGOPT(LAST+2))
        IF (KEY.EQ.9) BLOWUP = AMAX1(SRELPR,PRGOPT(LAST+2))
        NEXT = PRGOPT(LINK)
        IF ((NEXT.LE.0 .OR. NEXT.GT.NLINK)) then
          NERR = 3
          IOPT = 1
          CALL XERROR( 'WNNLS( ) THE OPTION VECTOR IS UNDEFINED', &
              39, NERR, IOPT)
          MODE = 2
          RETURN
        endif
        LAST = LINK
        LINK = NEXT
      enddo !110

      if (kind(W) == real_single) then
        do J=1,N
          CALL SSCAL(M, D(J), W(1,J), 1)
        enddo
      else if (kind(W) == real_double) then
        do J=1,N
          CALL DSCAL(M, D(J), W(1,J), 1)
        enddo
      endif
!C
!C     PROCESS-OPTION-VECTOR
      DONE = .FALSE.
      ITER = 0
      ITMAX = 3*(N-L)
      MODE = 0
      LP1 = L + 1
      NSOLN = L
      NSP1 = NSOLN + 1
      NP1 = N + 1
      NM1 = N - 1
      L1 = MIN0(M,L)
!C
!C     COMPUTE SCALE FACTOR TO APPLY TO EQUAL. CONSTRAINT EQUAS.

      if (kind(W) == real_single) then
        do J=1,N
          WD(J) = SASUM(M,W(1,J),1)
        enddo
        IMAX   = ISAMAX(N,WD,1)
        EANORM = WD(IMAX)
        BNORM  = SASUM(M,W(1,NP1),1)
      else if (kind(W) == real_double) then
        do J=1,N
          WD(J) = DASUM(M,W(1,J),1)
        enddo
        IMAX   = IDAMAX(N,WD,1)
        EANORM = WD(IMAX)
        BNORM  = DASUM(M,W(1,NP1),1)
      endif

      ALAMDA = EANORM/(SRELPR*FAC)
!C
!C     DEFINE SCALING DIAG MATRIX FOR MOD GIVENS USAGE AND
!C     CLASSIFY EQUATION TYPES.
      ALSQ = ALAMDA**2
      DO I=1,M !260
!C
!C     WHEN EQU I IS HEAVILY WEIGHTED ITYPE(I)=0, ELSE ITYPE(I)=1.
        IF ((I.LE.ME)) then
          T = ALSQ
          ITEMP = 0
          SCALE(I) = T
          ITYPE(I) = ITEMP
          cycle
        endif
        T = ONE
        ITEMP = 1
        SCALE(I) = T
        ITYPE(I) = ITEMP
      enddo !260
!C
!C     SET THE SOLN VECTOR X(*) TO ZERO AND THE COL INTERCHANGE
!C     MATRIX TO THE IDENTITY.
      X(1) = ZERO
      if (kind(X) == real_single) then
        CALL SCOPY(N, X, 0, X, 1)
      else if (kind(X) == real_double) then
        CALL DCOPY(N, X, 0, X, 1)
      endif
      DO I=1,N !270
        IPIVOT(I) = I
      enddo !270
!C
!C     PERFORM INITIAL TRIANGULARIZATION IN THE SUBMATRIX
!C     CORRESPONDING TO THE UNCONSTRAINED VARIABLES USING
!C     THE PROCEDURE INITIALLY-TRIANGULARIZE.
       !yonghui call sub995(L, ITYPE, N, MDW, W, WD, ME, MEP1, NSOLN, L1, &
       call sub995(L, ITYPE, N, MDW, W, WD, ME, MEP1, M, NSOLN, L1, &
                 ALSQ, EANORM, FAC, TAU, KRANK, KRP1, NIV, NIV1, SCALE)
!C
!C     PERFORM WNNLS ALGORITHM USING THE FOLLOWING STEPS.
!C
!C     UNTIL(DONE)
!C
!C        COMPUTE-SEARCH-DIRECTION-AND-FEASIBLE-POINT
!C
!C        WHEN (HITCON) ADD-CONSTRAINTS
!C
!C        ELSE PERFORM-MULTIPLIER-TEST-AND-DROP-A-CONSTRAINT
!C
!C        FIN
!C
!C     COMPUTE-FINAL-SOLUTION
!C
      do
        IF (DONE) exit
!!!!!!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C
!C       TO COMPUTE-SEARCH-DIRECTION-AND-FEASIBLE-POINT
!C
!C       SOLVE THE TRIANGULAR SYSTEM OF CURRENTLY NON-ACTIVE
!C       VARIABLES AND STORE THE SOLUTION IN Z(*).
!C
!C       SOLVE-SYSTEM

        call sub958(DONE,LP1,NSOLN,MDW,NP1,W,TEMP,NIV,KRANK,Z,L)
!C
!C       INCREMENT ITERATION COUNTER AND CHECK AGAINST MAX. NUMBER
!C       OF ITERATIONS.
        ITER = ITER + 1
        IF ((ITER.GT.ITMAX)) then
          MODE = 1
          DONE = .TRUE.
        endif
!C
!C       CHECK TO SEE IF ANY CONSTRAINTS HAVE BECOME ACTIVE.
!C       IF SO, CALCULATE AN INTERPOLATION FACTOR SO THAT ALL
!C       ACTIVE CONSTRAINTS ARE REMOVED FROM THE BASIS.
        ALPHA = TWO
        HITCON = .FALSE.
        IF ((L.LT.NSOLN)) then
          DO J=LP1,NSOLN !350
            ZZ = Z(J)
            IF (.NOT.(ZZ.LE.ZERO)) cycle
            T = X(J)/(X(J)-ZZ)
            IF ((T.LT.ALPHA)) then
              ALPHA = T
              JCON = J
            endif
            HITCON = .TRUE.
          enddo !350
        endif
!!!!!!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C
!C       COMPUTE-SEARCH-DIRECTION-AND-FEASIBLE-POINT
!C
        IF ((HITCON)) then
!!!!!!    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          ind59=0
!C
!C         TO ADD-CONSTRAINTS
!C
!C         USE COMPUTED ALPHA TO INTERPOLATE BETWEEN LAST
!C         FEASIBLE SOLUTION X(*) AND CURRENT UNCONSTRAINED
!C         (AND INFEASIBLE) SOLUTION Z(*).
          IF ((LP1.LE.NSOLN)) then
            DO J=LP1,NSOLN !380
              X(J) = X(J) + ALPHA*(Z(J)-X(J))
            enddo !380
          endif
          FEASBL = .FALSE.


          do
!C
!C           REMOVE COL JCON AND SHIFT COLS JCON+1 THROUGH N TO THE
!C           LEFT. SWAP COL JCON INTO THE N-TH POSITION.  THIS ACHIEVES
!C           UPPER HESSENBERG FORM FOR THE NONACTIVE CONSTRAINTS AND
!C           LEAVES AN UPPER HESSENBERG MATRIX TO RETRIANGULARIZE.
            DO I=1,M !420
              T = W(I,JCON)
!              if (kind(W) == real_single) then
!                CALL SCOPY(N-JCON, W(I,JCON+1), MDW, W(I,JCON), MDW)
!              else if (kind(W) == real_double) then
!                CALL DCOPY(N-JCON, W(I,JCON+1), MDW, W(I,JCON), MDW)
!              endif
              do j=jcon,n-1
                w(i,j)=w(i,j+1)
              end do
              W(I,N) = T
            enddo !420
!C
!C           UPDATE PERMUTED INDEX VECTOR TO REFLECT THIS SHIFT AND SWAP.
            ITEMP = IPIVOT(JCON)
            IF ((JCON.LT.N)) then
              DO I=JCON,NM1 !430
                IPIVOT(I) = IPIVOT(I+1)
              enddo !430
            endif
            IPIVOT(N) = ITEMP
!C
!C           SIMILARLY REPERMUTE X(*) VECTOR.
!            if (kind(X) == real_single) then
!              CALL SCOPY(N-JCON, X(JCON+1), 1, X(JCON), 1)
!            else if (kind(X) == real_double) then
!              CALL DCOPY(N-JCON, X(JCON+1), 1, X(JCON), 1)
!            endif
            do j=jcon,n-1
              X(j)=X(J+1)
            end do

            X(N) = ZERO
            NSP1 = NSOLN
            NSOLN = NSOLN - 1
            NIV1 = NIV
            NIV = NIV - 1
!C
!C           RETRIANGULARIZE UPPER HESSENBERG MATRIX AFTER ADDING CONSTRAINTS.
            J = JCON
            I = KRANK + JCON - L
            do
              IF (.NOT.(J.LE.NSOLN)) exit
                IF ((ITYPE(I).EQ.0 .AND. ITYPE(I+1).EQ.0)) then
                  call sub938(SPARAM, SCALE, W, MDW, I, J, NP1)
!C
!C                 (ITYPE(I).EQ.0 .AND. ITYPE(I+1).EQ.0) ZERO-IP1-TO-I-IN-COL-J
                  I = I + 1
                  J = J + 1
                  cycle
              endif
              IF ((ITYPE(I).EQ.1 .AND. ITYPE(I+1).EQ.1)) then
                call sub938(SPARAM, SCALE, W, MDW, I, J, NP1)
!C
!C               (ITYPE(I).EQ.1 .AND. ITYPE(I+1).EQ.1) ZERO-IP1-TO-I-IN-COL-J
                I = I + 1
                J = J + 1
                cycle
              endif
              IF ((ITYPE(I).EQ.1 .AND. ITYPE(I+1).EQ.0)) then
                if (kind(W) == real_single) then
                  CALL SSWAP(NP1, W(I,1), MDW, W(I+1,1), MDW)
                else if (kind(W) == real_double) then
                  CALL DSWAP(NP1, W(I,1), MDW, W(I+1,1), MDW)
                endif
                if (kind(SCALE) == real_single) then
                  CALL SSWAP(1, SCALE(I), 1, SCALE(I+1), 1)
                else if (kind(SCALE) == real_double) then
                  CALL DSWAP(1, SCALE(I), 1, SCALE(I+1), 1)
                endif
                ITEMP = ITYPE(I+1)
                ITYPE(I+1) = ITYPE(I)
                ITYPE(I) = ITEMP
!C
!C               SWAPPED ROW WAS FORMERLY A PIVOT ELT., SO IT WILL
!C               BE LARGE ENOUGH TO PERFORM ELIM.
                call sub938(SPARAM, SCALE, W, MDW, I, J, NP1)
!C
!C               ZERO-IP1-TO-I-IN-COL-J
                I = I + 1
                J = J + 1
                cycle
              endif
              IF (.NOT.(ITYPE(I).EQ.0 .AND. ITYPE(I+1).EQ.1)) then
                I = I + 1
                J = J + 1
                cycle
              endif
              T = SCALE(I)*W(I,J)**2/ALSQ
              IF ((T.GT.TAU**2*EANORM**2)) then
                call sub938(SPARAM, SCALE, W, MDW, I, J, NP1)
                I = I + 1
                J = J + 1
                cycle
              endif
              if (kind(W) == real_single) then
                CALL SSWAP(NP1, W(I,1), MDW, W(I+1,1), MDW)
              else if (kind(W) == real_double) then
                CALL DSWAP(NP1, W(I,1), MDW, W(I+1,1), MDW)
              endif
              if (kind(SCALE) == real_single) then
                CALL SSWAP(1, SCALE(I), 1, SCALE(I+1), 1)
              else if (kind(SCALE) == real_double) then
                CALL DSWAP(1, SCALE(I), 1, SCALE(I+1), 1)
              endif

              ITEMP = ITYPE(I+1)
              ITYPE(I+1) = ITYPE(I)
              ITYPE(I) = ITEMP
              W(I+1,J) = ZERO
              I = I + 1
              J = J + 1
            enddo
!C
!C         SEE IF THE REMAINING COEFFS IN THE SOLN SET ARE FEASIBLE.  THEY
!C         SHOULD BE BECAUSE OF THE WAY ALPHA WAS DETERMINED.  IF ANY ARE
!C         INFEASIBLE IT IS DUE TO ROUNDOFF ERROR.  ANY THAT ARE NON-
!C         POSITIVE WILL BE SET TO ZERO AND REMOVED FROM THE SOLN SET.
            IF ((LP1.LE.NSOLN)) then
              DO JCON=LP1,NSOLN !580
                IF (X(JCON).LE.ZERO) then
                  ind59=1
                  exit
                endif
              enddo !580
            endif
            if (ind59.ne.1) then
              FEASBL = .TRUE.
            endif
            IF (FEASBL) exit
          enddo
!!!!!!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          cycle
        endif
!C
!C       WHEN (HITCON) ADD-CONSTRAINTS
!C
!!!!!!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ind91=0

!C
!C       TO PERFORM-MULTIPLIER-TEST-AND-DROP-A-CONSTRAINT
        if (kind(Z) == real_single) then
          CALL SCOPY(NSOLN, Z, 1, X, 1)
        else if (kind(Z) == real_double) then
          CALL DCOPY(NSOLN, Z, 1, X, 1)
        endif

        IF ((NSOLN.LT.N)) then
          X(NSP1) = ZERO
          if (kind(X) == real_single) then
            CALL SCOPY(N-NSOLN, X(NSP1), 0, X(NSP1), 1)
          else if (kind(X) == real_double) then
            CALL DCOPY(N-NSOLN, X(NSP1), 0, X(NSP1), 1)
          endif
        endif
        I = NIV1
        do
          IF (.NOT.(I.LE.ME)) exit
!C
!C         RECLASSIFY LEAST SQUARES EQATIONS AS EQUALITIES AS
!C         NECESSARY.
          IF ((ITYPE(I).EQ.0)) then
            I = I + 1
            cycle
          endif
          if (kind(W) == real_single) then
            CALL SSWAP(NP1, W(I,1), MDW, W(ME,1), MDW)
          else if (kind(W) == real_double) then
            CALL DSWAP(NP1, W(I,1), MDW, W(ME,1), MDW)
          endif
          if (kind(SCALE) == real_single) then
            CALL SSWAP(1, SCALE(I), 1, SCALE(ME), 1)
          else if (kind(SCALE) == real_double) then
            CALL DSWAP(1, SCALE(I), 1, SCALE(ME), 1)
          endif

          ITEMP = ITYPE(I)
          ITYPE(I) = ITYPE(ME)
          ITYPE(ME) = ITEMP
          MEP1 = ME
          ME = ME - 1
        enddo
!C
!C       FORM INNER PRODUCT VECTOR WD(*) OF DUAL COEFFS.
        IF ((NSP1.LE.N)) then
          DO J=NSP1,N !720
            SM = ZERO
            IF ((NSOLN.LT.M)) then
              DO I=NSP1,M !700
                SM = SM + SCALE(I)*W(I,J)*W(I,NP1)
              enddo !700
            endif
            WD(J) = SM
          enddo !720
        endif

        do
!C
!C         FIND J SUCH THAT WD(J)=WMAX IS MAXIMUM.  THIS DETERMINES
!C         THAT THE INCOMING COL J WILL REDUCE THE RESIDUAL VECTOR
!C         AND BE POSITIVE.
          WMAX = ZERO
          IWMAX = NSP1
          IF ((NSP1.LE.N)) then
            DO J=NSP1,N !770
              IF ((WD(J).GT.WMAX)) then
                WMAX = WD(J)
                IWMAX = J
              endif
            enddo !770
          endif
          IF ((WMAX.LE.ZERO)) then
            DONE = .TRUE.
            exit
          endif
!C
!C         SET DUAL COEFF TO ZERO FOR INCOMING COL.
          WD(IWMAX) = ZERO
!
!C         WMAX .GT. ZERO, SO OKAY TO MOVE COL IWMAX TO SOLN SET.
!C         PERFORM TRANSFORMATION TO RETRIANGULARIZE, AND TEST
!C         FOR NEAR LINEAR DEPENDENCE.
!C         SWAP COL IWMAX INTO NSOLN-TH POSITION TO MAINTAIN UPPER
!C         HESSENBERG FORM OF ADJACENT COLS, AND ADD NEW COL TO
!C         TRIANGULAR DECOMPOSITION.
          NSOLN = NSP1
          NSP1 = NSOLN + 1
          NIV = NIV1
          NIV1 = NIV + 1
          IF ((NSOLN.NE.IWMAX)) then
            if (kind(W) == real_single) then
              CALL SSWAP(M, W(1,NSOLN), 1, W(1,IWMAX), 1)
            else if (kind(W) == real_double) then
              CALL DSWAP(M, W(1,NSOLN), 1, W(1,IWMAX), 1)
            endif
            WD(IWMAX) = WD(NSOLN)
            WD(NSOLN) = ZERO
            ITEMP = IPIVOT(NSOLN)
            IPIVOT(NSOLN) = IPIVOT(IWMAX)
            IPIVOT(IWMAX) = ITEMP
          endif
!C
!C         REDUCE COL NSOLN SO THAT THE MATRIX OF NONACTIVE
!C         CONSTRAINTS VARIABLES IS TRIANGULAR.
          J = M
          do !810
            IF (.NOT.(J.GT.NIV)) exit
            JM1 = J - 1
            JP = JM1
!C
!C           WHEN OPERATING NEAR THE ME LINE, TEST TO SEE IF THE PIVOT ELT.
!C           IS NEAR ZERO.  IF SO, USE THE LARGEST ELT. ABOVE IT AS THE PIVOT.
!C           THIS IS TO MAINTAIN THE SHARP INTERFACE BETWEEN WEIGHTED AND
!C           NON-WEIGHTED ROWS IN ALL CASES.
            IF ((J.EQ.MEP1)) then
              IMAX = ME
              AMAX = SCALE(ME)*W(ME,NSOLN)**2
              do !820
                IF (.NOT.(JP.GE.NIV)) exit
                T = SCALE(JP)*W(JP,NSOLN)**2
                IF ((T.GT.AMAX)) then
                  IMAX = JP
                  AMAX = T
                endif
                JP = JP - 1
              enddo !820
              JP = IMAX
            endif
            IF ((W(J,NSOLN).NE.ZERO)) then
              CALL SROTMG(SCALE(JP), SCALE(J), W(JP,NSOLN), &
                          W(J,NSOLN), SPARAM)
              W(J,NSOLN) = ZERO
              CALL SROTM(NP1-NSOLN, W(JP,NSP1), MDW, W(J,NSP1), &
                         MDW, SPARAM)
            endif
            J = JM1
          enddo !810
!C
!C         SOLVE FOR Z(NSOLN)=PROPOSED NEW VALUE FOR X(NSOLN).
!C         TEST IF THIS IS NONPOSITIVE OR TOO LARGE.
!C         IF THIS WAS TRUE OR IF THE PIVOT TERM WAS ZERO REJECT
!C         THE COL AS DEPENDENT.
          IF ((W(NIV,NSOLN).NE.ZERO)) then
            ISOL = NIV
            call sub897(Z2,Z,BLOWUP,BNORM, EANORM,ISOL, NP1, &
                       NSOLN,POS,MDW, W)
!C
!C           TEST-PROPOSED-NEW-COMPONENT
            IF (.NOT.(POS)) then
              NSP1 = NSOLN
              NSOLN = NSOLN - 1
              NIV1 = NIV
              NIV = NIV - 1
            endif
            IF (POS .OR. DONE) then
              exit
            else
              cycle
            endif
          endif
          IF ((NIV.LE.ME .AND. W(MEP1,NSOLN).NE.ZERO)) then
!C
!C           TRY TO ADD ROW MEP1 AS AN ADDITIONAL EQUALITY CONSTRAINT.
!C           CHECK SIZE OF PROPOSED NEW SOLN COMPONENT.
!C           REJECT IT IF IT IS TOO LARGE.
            ISOL = MEP1
            call sub897(Z2,Z,BLOWUP,BNORM, EANORM,ISOL, NP1, &
                       NSOLN,POS,MDW, W)
!C
!C           TEST-PROPOSED-NEW-COMPONENT
            IF ((POS)) then
!C
!C           SWAP ROWS MEP1 AND NIV, AND SCALE FACTORS FOR THESE ROWS.

              if (kind(W) == real_single) then
                CALL SSWAP(NP1, W(MEP1,1), MDW, W(NIV,1), MDW)
              else if (kind(W) == real_double) then
                CALL DSWAP(NP1, W(MEP1,1), MDW, W(NIV,1), MDW)
              endif
              if (kind(SCALE) == real_single) then
                CALL SSWAP(1, SCALE(MEP1), 1, SCALE(NIV), 1)
              else if (kind(SCALE) == real_double) then
                CALL DSWAP(1, SCALE(MEP1), 1, SCALE(NIV), 1)
              endif

              ITEMP = ITYPE(MEP1)
              ITYPE(MEP1) = ITYPE(NIV)
              ITYPE(NIV) = ITEMP
              ME = MEP1
              MEP1 = ME + 1
            endif
            ind91=1
          endif
          if(ind91.ne.1) then
            POS = .FALSE.
          endif
          IF (.NOT.(POS)) then
            NSP1 = NSOLN
            NSOLN = NSOLN - 1
            NIV1 = NIV
            NIV = NIV - 1
          endif
          IF (POS .OR. DONE) exit
        enddo

!!!!!!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C
!C       ELSE PERFORM-MULTIPLIER-TEST-AND-DROP-A-CONSTRAINT
!C
      enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 1000 CONTINUE
!C     TO COMPUTE-FINAL-SOLUTION
!C
!C     SOLVE SYSTEM, STORE RESULTS IN X(*).
!C
!!!!!!!!!!FNL SOLUTION DONE!!!!!!!!!!!!!!!!!!!!!!

       call sub958(DONE,LP1,NSOLN,MDW,NP1,W,TEMP,NIV,KRANK,Z,L)

!C     SOLVE-SYSTEM
      if (kind(Z) == real_single) then
        CALL SCOPY(NSOLN, Z, 1, X, 1)
      else if (kind(Z) == real_double) then
        CALL DCOPY(NSOLN, Z, 1, X, 1)
      endif
!C
!C     APPLY HOUSEHOLDER TRANSFORMATIONS TO X(*) IF KRANK.LT.L
      IF ((0.LT.KRANK .AND. KRANK.LT.L)) then
        DO I=1,KRANK !1020
          CALL H12(2, I, KRP1, L, W(I,1), MDW, H(I), X, 1, 1, 1)
        enddo !1020
      endif
!C
!C     FILL IN TRAILING ZEROES FOR CONSTRAINED VARIABLES NOT IN SOLN.
      IF ((NSOLN.LT.N)) then
        X(NSP1) = ZERO
        if (kind(X) == real_single) then
          CALL SCOPY(N-NSOLN, X(NSP1), 0, X(NSP1), 1)
        else if (kind(X) == real_double) then
          CALL DCOPY(N-NSOLN, X(NSP1), 0, X(NSP1), 1)
        endif
      endif
!C
!C     REPERMUTE SOLN VECTOR TO NATURAL ORDER.
      DO I=1,N !1070
        J = I
        do
          IF (IPIVOT(J).EQ.I) exit
          J = J + 1
        enddo
        IPIVOT(J) = IPIVOT(I)
        IPIVOT(I) = J
        if (kind(X) == real_single) then
          CALL SSWAP(1, X(J), 1, X(I), 1)
        else if (kind(X) == real_double) then
          CALL DSWAP(1, X(J), 1, X(I), 1)
        endif
      enddo !1070
!C
!C     RESCALE THE SOLN USING THE COL SCALING.
      DO  J=1,N !1080
        X(J) = X(J)*D(J)
      enddo !1080
      IF ((NSOLN.LT.M)) then
        DO I=NSP1,M !1090
          T = W(I,NP1)
          IF (I.LE.ME) T = T/ALAMDA
          T = (SCALE(I)*T)*T
          RNORM = RNORM + T
        enddo !1090
      endif
      RNORM = SQRT(RNORM)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C
!C     COMPUTE-FINAL-SOLUTION
!C
      RETURN

      END subroutine WNLSM

      !yonghui subroutine sub998(SRELPR, M, MA, MME,MEP1)
      subroutine sub998(SRELPR, M, MA, MME, ME, MEP1)
      use setparms
      implicit none
      REAL             SRELPR
      REAL             TWO, ZERO, ONE
      integer MA, MME, ME, MEP1, M
      DATA ZERO /0.E0/, ONE /1.E0/, TWO /2.E0/ !, SRELPR /0.E0/

!C     TO INITIALIZE-VARIABLES
!C
!C     SRELPR IS THE PRECISION FOR THE PARTICULAR MACHINE
!C     BEING USED.  THIS LOGIC AVOIDS RECOMPUTING IT EVERY ENTRY.
      IF ((SRELPR.EQ.ZERO)) then
!c***   changed back by BROSS
!c***   changed by RF Boisvert, 19-Feb-92  (fails on HP 9000 Series 300)
!cross      srelpr = r1mach(4)
         SRELPR = ONE
        do
          IF (ONE+SRELPR.EQ.ONE) exit
          SRELPR = SRELPR/TWO
        enddo
        SRELPR = SRELPR*TWO
!cross
      endif
      M = MA + MME
      ME = MME
      MEP1 = ME + 1

      return
      end subroutine sub998


      subroutine sub958(DONE,LP1,NSOLN,MDW,NP1,W,TEMP,NIV,KRANK,Z,L)
      use setparms
      implicit none
      logical DONE
      integer I,J,JJ,LP1,NSOLN,MDW,NP1,KRANK, ISOL, L,NIV
      REAL W(MDW,1)
      REAL Z(1), TEMP(1),ZERO
      DATA ZERO /0.E0/

      IF ((DONE)) then
        ISOL = 1
      else
        ISOL = LP1
      endif
      IF ((NSOLN.GE.ISOL)) then
!C
!C       COPY RT. HAND SIDE INTO TEMP VECTOR TO USE OVERWRITING METHOD.
        if (kind(W) == real_single) then
          CALL SCOPY(NIV, W(1,NP1), 1, TEMP, 1)
        else if (kind(W) == real_double) then
          CALL DCOPY(NIV, W(1,NP1), 1, TEMP, 1)
        endif

        DO JJ=ISOL,NSOLN
          J = NSOLN - JJ + ISOL
          IF ((J.GT.KRANK)) then
            I = NIV - JJ + ISOL
          else
            I = J
          endif
          IF ((J.GT.KRANK .AND. J.LE.L)) then
            Z(J) = ZERO
            cycle
          endif
          Z(J) = TEMP(I)/W(I,J)

          if (kind(W) == real_single .and. &
            kind(TEMP) == real_single) then
            CALL SAXPY(I-1, -Z(J), W(1,J), 1, TEMP, 1)
          else if (kind(W) == real_double .and. &
            kind(TEMP) == real_double) then
            CALL DAXPY(I-1, -Z(J), W(1,J), 1, TEMP, 1)
          endif

        enddo
      endif

      return
      end subroutine sub958


      !yonghui subroutine sub995(L, ITYPE, N, MDW, W, WD, ME, MEP1, NSOLN, L1, &
      subroutine sub995(L, ITYPE, N, MDW, W, WD, ME, MEP1, M, NSOLN, L1, &
                 ALSQ, EANORM, FAC, TAU, KRANK, KRP1, NIV, NIV1, SCALE)
      USE setparms
      implicit none
      integer MA, MME, ME, MEP1, M, MDW, L1, N, L
      REAL             W(MDW,1), WD(1), H(1), SCALE(1), DOPE(4)
      REAL             ALSQ, EANORM
      REAL             FAC, ONE
      REAL             RNORM,  TAU, TWO, ZERO
      integer ISOL, NP1, NSOLN, NIV, NIV1, KRP1, KRANK

      LOGICAL DONE
      INTEGER IPIVOT(1), ITYPE(1), IDOPE(8)
      DATA ZERO /0.E0/, ONE /1.E0/, TWO /2.E0/

!C
!C     TO INITIALLY-TRIANGULARIZE
!C
!C     SET FIRST L COMPS. OF DUAL VECTOR TO ZERO BECAUSE
!C     THESE CORRESPOND TO THE UNCONSTRAINED VARIABLES.
      IF ((L.GT.0)) then
        WD(1) = ZERO
        if (kind(WD) == real_single) then
          CALL SCOPY(L, WD, 0, WD, 1)
        else if (kind(WD) == real_double) then
          CALL DCOPY(L, WD, 0, WD, 1)
        endif
      endif
!C
!C     THE ARRAYS IDOPE(*) AND DOPE(*) ARE USED TO PASS
!C     INFORMATION TO WNLIT().  THIS WAS DONE TO AVOID
!C     A LONG CALLING SEQUENCE OR THE USE OF COMMON.
      IDOPE(1) = ME
      IDOPE(2) = MEP1
      IDOPE(3) = 0
      IDOPE(4) = 1
      IDOPE(5) = NSOLN
      IDOPE(6) = 0
      IDOPE(7) = 1
      IDOPE(8) = L1
!C
      DOPE(1) = ALSQ
      DOPE(2) = EANORM
      DOPE(3) = FAC
      DOPE(4) = TAU
      CALL WNLIT(W, MDW, M, N, L, IPIVOT, ITYPE, H, SCALE, RNORM, &
                IDOPE, DOPE, DONE)
      ME = IDOPE(1)
      MEP1 = IDOPE(2)
      KRANK = IDOPE(3)
      KRP1 = IDOPE(4)
      NSOLN = IDOPE(5)
      NIV = IDOPE(6)
      NIV1 = IDOPE(7)
      L1 = IDOPE(8)

      return
      end subroutine sub995

      subroutine sub897(Z2,Z,BLOWUP,BNORM, EANORM,ISOL, NP1, &
                  NSOLN,POS,MDW, W)
      use setparms
      implicit none
      integer MDW
      REAL             W(MDW,1)
      real BNORM, EANORM, Z2, Z(1),BLOWUP
      integer ISOL, NP1, NSOLN
      logical pos
      REAL             TWO, WMAX, ZERO, ONE
      DATA ZERO /0.E0/, ONE /1.E0/, TWO /2.E0/
!C
!C     TO TEST-PROPOSED-NEW-COMPONENT
      Z2 = W(ISOL,NP1)/W(ISOL,NSOLN)
      Z(NSOLN) = Z2
      POS = Z2.GT.ZERO
      IF ((Z2*EANORM.GE.BNORM .AND. POS)) then
        POS = .NOT.(BLOWUP*Z2*EANORM.GE.BNORM)
      endif
      return
      end subroutine sub897

      subroutine sub938(SPARAM, SCALE, W, MDW, I, J, NP1)
      use setparms
      implicit none
      integer          MDW, I, J, NP1
      REAL             W(MDW,1), SCALE(1)
      REAL             SPARAM(5)
      REAL             TWO, WMAX, ZERO, ONE
      DATA ZERO /0.E0/, ONE /1.E0/, TWO /2.E0/
!C
!C     TO ZERO-IP1-TO-I-IN-COL-J
      IF ((W(I+1,J).NE.ZERO)) then
        CALL SROTMG(SCALE(I), SCALE(I+1), W(I,J), W(I+1,J), SPARAM)
        W(I+1,J) = ZERO
        CALL SROTM(NP1-J, W(I,J+1), MDW, W(I+1,J+1), MDW, SPARAM)
      endif

      return
      end subroutine sub938

