      SUBROUTINE WNLIT(W,MDW,M,N,L,IPIVOT,ITYPE,H,SCALE,RNORM,IDOPE, &
        DOPE,DONE)
!C***BEGIN PROLOGUE  WNLIT
!C***REFER TO  WNNLS
!C
!C     This is a companion subprogram to WNNLS( ).
!C     The documentation for WNNLS( ) has more complete
!C     usage instructions.
!C
!C     Note  The M by (N+1) matrix W( , ) contains the rt. hand side
!C           B as the (N+1)st col.
!C
!C     Triangularize L1 by L1 subsystem, where L1=MIN(M,L), with
!C     col interchanges.
!C     Revised March 4, 1982.
!C***ROUTINES CALLED  H12,ISAMAX,SCOPY,SROTM,SROTMG,SSCAL,SSWAP
!C***END PROLOGUE  WNLIT
!C
!C     THE EDITING REQUIRED TO CONVERT THIS SUBROUTINE FROM SINGLE TO
!C     DOUBLE PRECISION INVOLVES THE FOLLOWING CHARACTER STRING CHANGES.
!C     USE AN EDITING COMMAND (CHANGE) /STRING-1/(TO)STRING-2/.
!C     (BEGIN CHANGES AT LINE WITH C++ IN COLS. 1-3.)
!C     /REAL (12 BLANKS)/DOUBLE PRECISION/,/SCOPY/DCOPY/,/SROTM/DROTM/,
!C     /SSCAL/DSCAL/,
!C     /SSWAP/DSWAP/,/AMAX1/DMAX1/,/ISAMAX/IDAMAX/,/.E-/.D-/,/E0/D0/
!C
!C++
      USE setparms
      implicit none
      integer LP, I, J, JJ, JP , JM1, ind, IC, I1, J1
      integer ME, MEP1, KRANK, KRP1, NSOLN, NIV, NIV1, L1 , N, M, L
      integer IR, KK, IP1, MEND, IRP1, LP1, LEND, IMAX, MAX, MDW
      real  NP1, LB
!c
      REAL             W(MDW,1), H(1), SCALE(1), DOPE(4), SPARAM(5)
      REAL             ALSQ, AMAX, EANORM, FAC, FACTOR, HBAR, ONE, RN
      REAL             RNORM, SN, T, TAU, TENM3, ZERO
      REAL             AMAX1
      INTEGER ITYPE(1), IPIVOT(1), IDOPE(8)
      integer(kind = int_single) ISAMAX,IDAMAX
      LOGICAL INDEP, DONE, RECALC
      DATA TENM3 /1.E-3/, ZERO /0.E0/, ONE /1.E0/
!C
!C***FIRST EXECUTABLE STATEMENT  WNLIT
      ME = IDOPE(1)
      MEP1 = IDOPE(2)
      KRANK = IDOPE(3)
      KRP1 = IDOPE(4)
      NSOLN = IDOPE(5)
      NIV = IDOPE(6)
      NIV1 = IDOPE(7)
      L1 = IDOPE(8)
!C
      ALSQ = DOPE(1)
      EANORM = DOPE(2)
      FAC = DOPE(3)
      TAU = DOPE(4)
      NP1 = N + 1
      LB = MIN0(M-1,L)
      RECALC = .TRUE.
      RNORM = ZERO
      KRANK = 0
!C     WE SET FACTOR=1.E0 SO THAT THE HEAVY WEIGHT ALAMDA WILL BE
!C     INCLUDED IN THE TEST FOR COL INDEPENDENCE.
      FACTOR = 1.E0
      I = 1
      IP1 = 2
      LEND = L
      do
        IF (.NOT.(I.LE.LB)) exit
!C
!C       SET IR TO POINT TO THE I-TH ROW.
        IR = I
        MEND = M
        call sub996 (I, LEND, MEND, IR, MDW, RECALC, IMAX, HBAR, H, &
                     SCALE, W)
!C
!C       UPDATE-COL-SS-AND-FIND-PIVOT-COL
        call sub993 (I, IMAX, M, MDW, IPIVOT, H, W)
!C
!C       PERFORM-COL-INTERCHANGE
!C
!C       SET IC TO POINT TO I-TH COL.
        do
          IC = I
          call sub990(ME, MEND, IR, FACTOR, TAU, SCALE, W)

!C
!C         TEST-INDEP-OF-INCOMING-COL
          IF ((INDEP)) then
!C
!C           ELIMINATE I-TH COL BELOW DIAG. USING MOD. GIVENS TRANSFORMATIONS
!C           APPLIED TO (A B).
            J = M
            DO JJ=IP1,M
              JM1 = J - 1
              JP = JM1
!C           WHEN OPERATING NEAR THE ME LINE, USE THE LARGEST ELT.
!C           ABOVE IT AS THE PIVOT.
              IF ((J.EQ.MEP1)) then
                IMAX = ME
                AMAX = SCALE(ME)*W(ME,I)**2
                do
                  IF (.NOT.(JP.GE.I)) exit
                  T = SCALE(JP)*W(JP,I)**2
                  IF (.NOT.(T.GT.AMAX)) then
                    IMAX = JP
                    AMAX = T
                  endif
                  JP = JP - 1
                enddo
                JP = IMAX
              endif
              IF ((W(J,I).NE.ZERO)) then
                CALL SROTMG(SCALE(JP), SCALE(J), W(JP,I), W(J,I), SPARAM)
                W(J,I) = ZERO
                CALL SROTM(NP1-I, W(JP,IP1), MDW, W(J,IP1), MDW, SPARAM)
              endif
              J = JM1
            enddo
            ind=0
            exit
          endif

          IF (.NOT.(LEND.GT.I)) exit
!C
!C         COL I IS DEPENDENT. SWAP WITH COL LEND.
          MAX = LEND
!C
!C         PERFORM-COL-INTERCHANGE
          call sub993 (I, IMAX, M, MDW, IPIVOT, H, W)
          LEND = LEND - 1
!C
!C         FIND COL IN REMAINING SET WITH LARGEST SS.
          if (kind(H) == real_single) then
            MAX = ISAMAX(LEND-I+1,H(I),1) + I - 1
          else if (kind(H) == real_double) then
            MAX = IDAMAX(LEND-I+1,H(I),1) + I - 1
          endif
          HBAR = H(MAX)
        enddo

        if (ind.ne.0) then
          KRANK = I - 1
          exit
        else
          I = IP1
          IP1 = IP1 + 1
          cycle
        endif
      enddo
      KRANK = L1

      KRP1 = KRANK + 1
      IF ((KRANK.LT.ME)) then
        FACTOR = ALSQ
        DO I=KRP1,ME
          IF (L.GT.0) W(I,1) = ZERO
          if (kind(W) == real_single) then
            CALL SCOPY(L, W(I,1), 0, W(I,1), MDW)
          else if (kind(W) == real_double) then
            CALL DCOPY(L, W(I,1), 0, W(I,1), MDW)
          endif
        enddo
!C
!C       DETERMINE THE RANK OF THE REMAINING EQUALITY CONSTRAINT
!C       EQUATIONS BY ELIMINATING WITHIN THE BLOCK OF CONSTRAINED
!C       VARIABLES.  REMOVE ANY REDUNDANT CONSTRAINTS.
        LP1 = L + 1
        RECALC = .TRUE.
        LB = MIN0(L+ME-KRANK,N)
        I = LP1
        IP1 = I + 1

        do
          IF (.NOT.(I.LE.LB)) exit
          IR = KRANK + I - L
          LEND = N
          MEND = ME
          call sub996 (I, LEND, MEND, IR, MDW, RECALC, IMAX, HBAR, H, &
                       SCALE, W)
!C
!C         UPDATE-COL-SS-AND-FIND-PIVOT-COL
           call sub993 (I, IMAX, M, MDW, IPIVOT, H, W)
!C
!C         PERFORM-COL-INTERCHANGE
!C
!C         ELIMINATE ELEMENTS IN THE I-TH COL.
          J = ME
          do
            IF (.NOT.(J.GT.IR)) exit
            JM1 = J - 1
            IF ((W(J,I).NE.ZERO)) then
              CALL SROTMG(SCALE(JM1), SCALE(J), W(JM1,I), W(J,I), SPARAM)
              W(J,I) = ZERO
              CALL SROTM(NP1-I, W(JM1,IP1), MDW, W(J,IP1), MDW, SPARAM)
            endif
            J = JM1
          enddo
!C
!C         SET IC=I=COL BEING ELIMINATED
          IC = I
          call sub990(ME, MEND, IR, FACTOR, TAU, SCALE, W)
!C
!C         TEST-INDEP-OF-INCOMING-COL
          IF (.NOT.INDEP) then
!C
!C           REMOVE ANY REDUNDANT OR DEPENDENT EQUALITY CONSTRAINTS.
            JJ = IR
            do
              IF (.NOT.(IR.LE.ME)) exit
              W(IR,1) = ZERO

              if (kind(W) == real_single) then
                CALL SCOPY(N, W(IR,1), 0, W(IR,1), MDW)
              else if (kind(W) == real_double) then
                CALL DCOPY(N, W(IR,1), 0, W(IR,1), MDW)
              endif

              RNORM = RNORM + (SCALE(IR)*W(IR,NP1)/ALSQ)*W(IR,NP1)
              W(IR,NP1) = ZERO
              SCALE(IR) = ONE
!C             RECLASSIFY THE ZEROED ROW AS A LEAST SQUARES EQUATION.
              ITYPE(IR) = 1
              IR = IR + 1
            enddo
!C
!C           REDUCE ME TO REFLECT ANY DISCOVERED DEPENDENT EQUALITY
!C           CONSTRAINTS.
            ME = JJ - 1
            MEP1 = ME + 1
            exit
          else
            I = IP1
            IP1 = IP1 + 1
            cycle
          endif
        enddo
      endif
      IF ((KRANK.LT.L1)) then
!C
!C       TRY TO DETERMINE THE VARIABLES KRANK+1 THROUGH L1 FROM THE
!C       LEAST SQUARES EQUATIONS.  CONTINUE THE TRIANGULARIZATION WITH
!C       PIVOT ELEMENT W(MEP1,I).
!C
        RECALC = .TRUE.
!C
!C       SET FACTOR=ALSQ TO REMOVE EFFECT OF HEAVY WEIGHT FROM
!C       TEST FOR COL INDEPENDENCE.
        FACTOR = ALSQ
        KK = KRP1
        I = KK
        IP1 = I + 1
        do
          IF (.NOT.(I.LE.L1)) exit
!C
!C         SET IR TO POINT TO THE MEP1-ST ROW.
          IR = MEP1
          LEND = L
          MEND = M
          call sub996 (I, LEND, MEND, IR, MDW, RECALC, IMAX, HBAR, H, &
                      SCALE, W)
!C
!C         UPDATE-COL-SS-AND-FIND-PIVOT-COL
           call sub993 (I, IMAX, M, MDW, IPIVOT, H, W)
!C
!C         PERFORM-COL-INTERCHANGE
!C
!C         ELIMINATE I-TH COL BELOW THE IR-TH ELEMENT.
          IRP1 = IR + 1
          J = M
          DO JJ=IRP1,M
            JM1 = J - 1
            IF (.NOT.(W(J,I).NE.ZERO)) then
              J = JM1
              cycle
            endif
            CALL SROTMG(SCALE(JM1), SCALE(J), W(JM1,I), W(J,I), SPARAM)
            W(J,I) = ZERO
            CALL SROTM(NP1-I, W(JM1,IP1), MDW, W(J,IP1), MDW, SPARAM)
            J = JM1
          enddo
!C
!C         TEST IF NEW PIVOT ELEMENT IS NEAR ZERO. IF SO, THE COL IS
!C         DEPENDENT.
          T = SCALE(IR)*W(IR,I)**2
          INDEP = T.GT.TAU**2*EANORM**2
          IF (INDEP) then
!C
!C           COL TEST PASSED. NOW MUST PASS ROW NORM TEST TO BE CLASSIFIED
!C           AS INDEPENDENT.
            RN = ZERO
            DO I1=IR,M
              DO J1=IP1,N
                RN = AMAX1(RN,SCALE(I1)*W(I1,J1)**2)
              enddo
            enddo
            INDEP = T.GT.TAU**2*RN
          endif
!C
!C         IF INDEPENDENT, SWAP THE IR-TH AND KRP1-ST ROWS TO MAINTAIN THE
!C         TRIANGULAR FORM.  UPDATE THE RANK INDICATOR KRANK AND THE
!C         EQUALITY CONSTRAINT POINTER ME.
          IF (.NOT.(INDEP)) exit
          if (kind(W) == real_single) then
            CALL SSWAP(NP1, W(KRP1,1), MDW, W(IR,1), MDW)
          else if (kind(W) == real_double) then
            CALL DSWAP(NP1, W(KRP1,1), MDW, W(IR,1), MDW)
          endif
          if (kind(SCALE) == real_single) then
            CALL SSWAP(1, SCALE(KRP1), 1, SCALE(IR), 1)
          else if (kind(SCALE) == real_double) then
            CALL DSWAP(1, SCALE(KRP1), 1, SCALE(IR), 1)
          endif
!C         RECLASSIFY THE LEAST SQ. EQUATION AS AN EQUALITY CONSTRAINT AND
!C         RESCALE IT.
          ITYPE(IR) = 0
          T = SQRT(SCALE(KRP1))

          if (kind(W) == real_single) then
            CALL SSCAL(NP1, T, W(KRP1,1), MDW)
          else if (kind(W) == real_double) then
            CALL DSCAL(NP1, T, W(KRP1,1), MDW)
          endif

          SCALE(KRP1) = ALSQ
          ME = MEP1
          MEP1 = ME + 1
          KRANK = KRP1
          KRP1 = KRANK + 1
          I = IP1
          IP1 = IP1 + 1
        enddo

      endif
!C
!C     IF PSEUDORANK IS LESS THAN L, APPLY HOUSEHOLDER TRANS.
!C     FROM RIGHT.
      IF ((KRANK.LT.L)) then
        DO I=1,KRANK
          J = KRP1 - I
          CALL H12(1, J, KRP1, L, W(J,1), MDW, H(J), W, MDW, 1, J-1)
        enddo
      endif
      NIV = KRANK + NSOLN - L
      NIV1 = NIV + 1
      IF (L.EQ.N) DONE = .TRUE.
!C
!C  END OF INITIAL TRIANGULARIZATION.
      IDOPE(1) = ME
      IDOPE(2) = MEP1
      IDOPE(3) = KRANK
      IDOPE(4) = KRP1
      IDOPE(5) = NSOLN
      IDOPE(6) = NIV
      IDOPE(7) = NIV1
      IDOPE(8) = L1
      RETURN
      END subroutine WNLIT

      subroutine sub993 (I, IMAX, M, MDW, IPIVOT, H, W)
      use setparms
      implicit none
      integer MAX !, real_single, real_double
      INTEGER I, IMAX, IPIVOT(*), M, MDW
      REAL             H(*), W(MDW,*)
      EXTERNAL SSWAP
      REAL T
      INTEGER ITEMP
!
!C
!C     TO PERFORM-COL-INTERCHANGE
!C
      IF ((MAX.NE.I)) then
!C     EXCHANGE ELEMENTS OF PERMUTED INDEX VECTOR AND PERFORM COL
!C     INTERCHANGES.
        ITEMP = IPIVOT(I)
        IPIVOT(I) = IPIVOT(MAX)
        IPIVOT(MAX) = ITEMP

        if (kind(W) == real_single) then
          CALL SSWAP(M, W(1,MAX), 1, W(1,I), 1)
        else if (kind(W) == real_double) then
          CALL DSWAP(M, W(1,MAX), 1, W(1,I), 1)
        endif

        T = H(MAX)
        H(MAX) = H(I)
        H(I) = T
      endif
      return
      end subroutine sub993

      subroutine sub990(ME, MEND, IR, FACTOR, TAU, SCALE, W)
      implicit none
      logical indep
      REAL             FACTOR, SCALE(*), TAU, W(*)
      INTEGER IR, ME, MEND
      REAL             RN, SN, T
      INTEGER J
!C
!C     TO TEST-INDEP-OF-INCOMING-COL
!C
!C     TEST THE COL IC TO DETERMINE IF IT IS LINEARLY INDEPENDENT
!C     OF THE COLS ALREADY IN THE BASIS.  IN THE INIT TRI
!C     STEP, WE USUALLY WANT THE HEAVY WEIGHT ALAMDA TO
!C     BE INCLUDED IN THE TEST FOR INDEPENDENCE.  IN THIS CASE THE
!C     VALUE OF FACTOR WILL HAVE BEEN SET TO 1.E0 BEFORE THIS
!C     PROCEDURE IS INVOKED.  IN THE POTENTIALLY RANK DEFICIENT
!C     PROBLEM, THE VALUE OF FACTOR WILL HAVE BEEN
!C     SET TO ALSQ=ALAMDA**2 TO REMOVE THE EFFECT OF THE HEAVY WEIGHT
!C     FROM THE TEST FOR INDEPENDENCE.
!C
!C     WRITE NEW COL AS PARTITIONED VECTOR
!C             (A1)  NUMBER OF COMPONENTS IN SOLN SO FAR = NIV
!C             (A2)  M-NIV COMPONENTS
!C     AND COMPUTE  SN = INVERSE WEIGHTED LENGTH OF A1
!C                  RN = INVERSE WEIGHTED LENGTH OF A2
!C     CALL THE COL INDEPENDENT WHEN RN .GT. TAU*SN
      SN = 0.
      RN = 0.
      do j=1,mend
        T = SCALE(J)
        IF (J.LE.ME) T = T/FACTOR
        T = T*W(J)**2
!        T = T*W(J,IC)**2
        IF (.NOT.(J.LT.IR)) then
          RN = RN + T
        else
          SN = SN + T
        endif
      enddo
      INDEP = RN.GT.TAU**2*SN

      return
      end subroutine sub990


      subroutine sub996 (I, LEND, MEND, IR, MDW, RECALC, IMAX, HBAR, H, &
         SCALE, W)
      use setparms
      implicit none
      integer max !, real_single, real_double
      real tenm3
      INTEGER I, IMAX, IR, LEND, MDW, MEND
      REAL             H(*), HBAR, SCALE(*), W(MDW,*)
      LOGICAL RECALC, INDEP
      EXTERNAL ISAMAX, IDAMAX
      INTEGER ISAMAX, IDAMAX
      INTEGER J, K
!C
!C     TO UPDATE-COL-SS-AND-FIND-PIVOT-COL
!C
!C     THE COL SS VECTOR WILL BE UPDATED AT EACH STEP. WHEN
!C     NUMERICALLY NECESSARY, THESE VALUES WILL BE RECOMPUTED.
!C
      IF ((IR.NE.1 .AND. (.NOT.RECALC))) then
!C     UPDATE COL SS =SUM OF SQUARES.
        do J=i,lend
          H(J) = H(J) - SCALE(IR-1)*W(IR-1,J)**2
        enddo
!C
!C     TEST FOR NUMERICAL ACCURACY.
        if (kind(H) == real_single) then
          MAX = ISAMAX(LEND-I+1,H(I),1) + I - 1
        else if (kind(H) == real_double) then
          MAX = IDAMAX(LEND-I+1,H(I),1) + I - 1
        endif
        RECALC = HBAR + TENM3*H(MAX).EQ.HBAR
      endif
!C
!C     IF REQUIRED, RECALCULATE COL SS, USING ROWS IR THROUGH MEND.
      IF ((RECALC)) then
        do j=1,lend
          H(J) = 0.
          do k=ir,mend
            H(J) = H(J) + SCALE(K)*W(K,J)**2
          enddo
        enddo
!C
!C     FIND COL WITH LARGEST SS.
        if (kind(H) == real_single) then
          MAX = ISAMAX(LEND-I+1,H(I),1) + I - 1
        else if (kind(H) == real_double) then
          MAX = IDAMAX(LEND-I+1,H(I),1) + I - 1
        endif

        HBAR = H(MAX)
      endif
      return
      end subroutine sub996
