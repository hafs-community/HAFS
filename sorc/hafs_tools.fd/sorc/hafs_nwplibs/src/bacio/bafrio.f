!-----------------------------------------------------------------------
!
!  revision history:
!
!  Aug, 2012  Jun Wang    bafrio for big and little endian files
!
!  note:
!     This version of bafrio.f is revised to have byteswap in FORTRAN
!  data file control words. It is designed to be run on 
!  on WCOSS(little endian machine) and to generate big endian files. 
!     It does byteswap on fortran record control words(4 byte integer 
!  before and after data field), not on data field itself. Users need
!  to byteswap their data after(for reading)/before(for writing) 
!  calling subroutines this file. This is considered to be the best 
!  way to keep subroutine inerfaces intact for backward compatible.
!
!-----------------------------------------------------------------------
      SUBROUTINE BAFRINDEX(LU,IB,LX,IX)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAFRINDEX      BYTE-ADDRESSABLE FORTRAN RECORD INDEX
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1999-01-21
!
! ABSTRACT: THIS SUBPROGRAM IS CALLING BAFRINDEXL TO EITHER READ AN 
!   UNFORMATTED FORTRAN RECORD
!   AND RETURN ITS LENGTH AND START BYTE OF THE NEXT FORTRAN RECORD;
!   OR GIVEN THE RECORD LENGTH, WITHOUT I/O IT DETERMINES THE START BYTE
!   OF THE NEXT FORTRAN RECORD. THE DIFFERENCE BETWEEN BAFRINDEX AND 
!   BAFRINDEXL IS THE KIND TYPE OF INTERGERS IN THE ARGUMENT LIST
!
! PROGRAM HISTORY LOG:
!   1999-01-21  IREDELL
!   2009-04-20  J. WANG
!
! USAGE:    CALL BAFRINDEX(LU,IB,LX,IX)
!   INPUT ARGUMENTS:
!     LU           INTEGER LOGICAL UNIT TO READ
!                  IF LU<=0, THEN DETERMINE IX FROM LX
!     IB           INTEGER FORTRAN RECORD START BYTE
!                  (FOR THE FIRST FORTRAN RECORD, IB SHOULD BE 0)
!     LX           INTEGER RECORD LENGTH IN BYTES IF LU<=0
!
!   OUTPUT ARGUMENTS:
!     LX           INTEGER RECORD LENGTH IN BYTES IF LU>0,
!                  OR LX=-1 FOR I/O ERROR (PROBABLE END OF FILE),
!                  OR LX=-2 FOR I/O ERROR (INVALID FORTRAN RECORD)
!     IX           INTEGER START BYTE FOR THE NEXT FORTRAN RECORD
!                  (COMPUTED ONLY IF LX>=0)
!
! SUBPROGRAMS CALLED:
!   BAFRINDEXL     BYTE-ADDRESSABLE FORTRAN RECORD INDEX
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
!
      IMPLICIT NONE
      INTEGER,INTENT(IN):: LU,IB
      INTEGER,INTENT(INOUT):: LX
      INTEGER,INTENT(OUT):: IX
      integer(kind=8) :: LONG_IB,LONG_LX ,LONG_IX
!
      LONG_IB=IB
      LONG_LX=LX
      call BAFRINDEXL(LU,LONG_IB,LONG_LX,LONG_IX)
      LX=LONG_LX
      IX=LONG_IX

      return
      end SUBROUTINE BAFRINDEX
!-----------------------------------------------------------------------
      SUBROUTINE BAFRINDEXL(LU,IB,LX,IX)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAFRINDEXL      BYTE-ADDRESSABLE FORTRAN RECORD INDEX
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1999-01-21
!
! ABSTRACT: THIS SUBPROGRAM EITHER READS AN UNFORMATTED FORTRAN RECORD
!   AND RETURN ITS LENGTH AND START BYTE OF THE NEXT FORTRAN RECORD;
!   OR GIVEN THE RECORD LENGTH, WITHOUT I/O IT DETERMINES THE START BYTE
!   OF THE NEXT FORTRAN RECORD.
!
! PROGRAM HISTORY LOG:
!   1999-01-21  IREDELL
!   2009-04-20  J. WANG
!
! USAGE:    CALL BAFRINDEXL(LU,IB,LX,IX)
!   INPUT ARGUMENTS:
!     LU           INTEGER LOGICAL UNIT TO READ
!                  IF LU<=0, THEN DETERMINE IX FROM LX
!     IB           INTEGER(8) FORTRAN RECORD START BYTE
!                  (FOR THE FIRST FORTRAN RECORD, IB SHOULD BE 0)
!     LX           INTEGER(8) RECORD LENGTH IN BYTES IF LU<=0
!
!   OUTPUT ARGUMENTS:
!     LX           INTEGER(8) RECORD LENGTH IN BYTES IF LU>0,
!                  OR LX=-1 FOR I/O ERROR (PROBABLE END OF FILE),
!                  OR LX=-2 FOR I/O ERROR (INVALID FORTRAN RECORD)
!     IX           INTEGER(8) START BYTE FOR THE NEXT FORTRAN RECORD
!                  (COMPUTED ONLY IF LX>=0)
!
! SUBPROGRAMS CALLED:
!   BAREADL        BYTE-ADDRESSABLE READ
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      IMPLICIT NONE
      INTEGER,INTENT(IN):: LU
      INTEGER(KIND=8),INTENT(IN):: IB
      INTEGER(KIND=8),INTENT(INOUT):: LX
      INTEGER(KIND=8),INTENT(OUT):: IX
      INTEGER(KIND=8),PARAMETER:: LBCW=4
      INTEGER(KIND=LBCW):: BCW1,BCW2
      INTEGER(KIND=8):: KR
      CHARACTER(16) :: MACHINE_ENDIAN
      LOGICAL :: DO_BYTESWAP
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPARE FIRST BLOCK CONTROL WORD AND TRAILING BLOCK CONTROL WORD
      IF(LU.GT.0) THEN
!
!-- set do_byteswap from machine endianness and file endianness
        CALL CHK_ENDIANC(MACHINE_ENDIAN)
        IF( LU<=999) THEN
          IF( trim(MACHINE_ENDIAN)=="big_endian") THEN
            DO_BYTESWAP=.false.
          ELSEIF( trim(MACHINE_ENDIAN)=="little_endian") THEN
            DO_BYTESWAP=.true.
          ENDIF
        ELSEIF(LU<=1999) THEN
          IF( trim(MACHINE_ENDIAN)=="big_endian") THEN
            DO_BYTESWAP=.true.
          ELSEIF( trim(MACHINE_ENDIAN)=="little_endian") THEN
            DO_BYTESWAP=.false.
          ENDIF
        ENDIF
! 
!
!-- read out control word      
        CALL BAREADL(LU,IB,LBCW,KR,BCW1)
        IF(DO_BYTESWAP) CALL Byteswap(BCW1,LBCW,1)
!
        IF(KR.NE.LBCW) THEN
          LX=-1
        ELSE
          CALL BAREADL(LU,IB+LBCW+BCW1,LBCW,KR,BCW2)
          IF(DO_BYTESWAP) CALL Byteswap(BCW2,LBCW,1)
!
          IF(KR.NE.LBCW.OR.BCW1.NE.BCW2) THEN
            LX=-2
          ELSE
            LX=BCW1
          ENDIF
        ENDIF
!
!end luif
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE START BYTE FOR THE NEXT FORTRAN RECORD
      IF(LX.GE.0) IX=IB+LBCW+LX+LBCW
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END SUBROUTINE BAFRINDEXL
!-----------------------------------------------------------------------
      SUBROUTINE BAFRREAD(LU,IB,NB,KA,A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAFRREAD       BYTE-ADDRESSABLE FORTRAN RECORD READ
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1999-01-21
!
! ABSTRACT: THIS SUBPROGRAM IS CALLING BAFREAD TO REAS AN UNFORMATTED 
!   FORTRAN RECORD. THE DIFFERENCE BETWEEN BAFRREAD AND BAFRREADL IS
!    THE KIND TYPE OF INTERGERS IN THE ARGUMENT LIST
!
! PROGRAM HISTORY LOG:
!   1999-01-21  IREDELL
!   2009-04-20  J. WANG
!
! USAGE:    CALL BAFRREAD(LU,IB,NB,KA,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER LOGICAL UNIT TO READ
!     IB           INTEGER FORTRAN RECORD START BYTE
!                  (FOR THE FIRST FORTRAN RECORD, IB SHOULD BE 0)
!     NB           INTEGER NUMBER OF BYTES TO READ
!
!   OUTPUT ARGUMENTS:
!     KA           INTEGER NUMBER OF BYTES IN FORTRAN RECORD
!                  (IN WHICH CASE THE NEXT FORTRAN RECORD
!                  SHOULD HAVE A START BYTE OF IB+KA),
!                  OR KA=-1 FOR I/O ERROR (PROBABLE END OF FILE),
!                  OR KA=-2 FOR I/O ERROR (INVALID FORTRAN RECORD),
!                  OR KA=-3 FOR I/O ERROR (REQUEST LONGER THAN RECORD)
!     A            CHARACTER*1 (NB) DATA READ
!
! SUBPROGRAMS CALLED:
!   BAFRREADL      BYTE-ADDRESSABLE READ
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
!
      IMPLICIT NONE
      INTEGER,INTENT(IN):: LU,IB,NB
      INTEGER,INTENT(OUT):: KA
      CHARACTER,INTENT(OUT):: A(NB)
      INTEGER(KIND=8) :: LONG_IB,LONG_NB,LONG_KA
!
        if((IB<0.and.IB/=-1) .or. NB<0 ) THEN
          print *,'WRONG: in BAFRREAD starting postion IB or read '//    &
     & 'data size NB < 0, STOP! Consider use BAFREADL and long integer'
          KA=0
          return
        ENDIF
        LONG_IB=IB
        LONG_NB=NB
        CALL BAFRREADL(LU,LONG_IB,LONG_NB,LONG_KA,A)
        KA=LONG_KA
      END SUBROUTINE BAFRREAD
!-----------------------------------------------------------------------
      SUBROUTINE BAFRREADL(LU,IB,NB,KA,A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAFRREADL      BYTE-ADDRESSABLE FORTRAN RECORD READ
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1999-01-21
!
! ABSTRACT: THIS SUBPROGRAM READS AN UNFORMATTED FORTRAN RECORD
!
! PROGRAM HISTORY LOG:
!   1999-01-21  IREDELL
!   2009-04-20  J. WANG
!
! USAGE:    CALL BAFRREADL(LU,IB,NB,KA,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER LOGICAL UNIT TO READ
!     IB           INTEGER(8) FORTRAN RECORD START BYTE
!                  (FOR THE FIRST FORTRAN RECORD, IB SHOULD BE 0)
!     NB           INTEGER(8) NUMBER OF BYTES TO READ
!
!   OUTPUT ARGUMENTS:
!     KA           INTEGER(8) NUMBER OF BYTES IN FORTRAN RECORD
!                  (IN WHICH CASE THE NEXT FORTRAN RECORD
!                  SHOULD HAVE A START BYTE OF IB+KA),
!                  OR KA=-1 FOR I/O ERROR (PROBABLE END OF FILE),
!                  OR KA=-2 FOR I/O ERROR (INVALID FORTRAN RECORD),
!                  OR KA=-3 FOR I/O ERROR (REQUEST LONGER THAN RECORD)
!     A            CHARACTER*1 (NB) DATA READ
!
! SUBPROGRAMS CALLED:
!   BAFRINDEXL     BYTE-ADDRESSABLE FORTRAN RECORD INDEX
!   BAREADL        BYTE-ADDRESSABLE READ
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      IMPLICIT NONE
      INTEGER,INTENT(IN):: LU
      INTEGER(kind=8),INTENT(IN):: IB,NB
      INTEGER(kind=8),INTENT(OUT):: KA
      CHARACTER,INTENT(OUT):: A(NB)
      INTEGER(kind=8),PARAMETER:: LBCW=4
      INTEGER(kind=8):: LX,IX
      INTEGER(kind=8):: KR
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  VALIDATE FORTRAN RECORD
      CALL BAFRINDEXL(LU,IB,LX,IX)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  READ IF VALID
      IF(LX.LT.0) THEN
        KA=LX
      ELSEIF(LX.LT.NB) THEN
        KA=-3
      ELSE
        CALL BAREADL(LU,IB+LBCW,NB,KR,A)
        IF(KR.NE.NB) THEN
          KA=-1
        ELSE
          KA=LBCW+LX+LBCW
        ENDIF
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END SUBROUTINE BAFRREADL
!-----------------------------------------------------------------------
      SUBROUTINE BAFRWRITE(LU,IB,NB,KA,A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAFRWRITE      BYTE-ADDRESSABLE FORTRAN RECORD WRITE
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1999-01-21
!
! ABSTRACT: THIS SUBPROGRAM IS CALLING BAFRWRITE TO WRITE AN UNFORMATTED 
!    FORTRAN RECORD. THE DIFFERENCE BETWEEN BAFRWRITE AND BAFRWRITEL IS 
!    THE KIND TYPE OF INTERGERS IN THE ARGUMENT LIST
!
! PROGRAM HISTORY LOG:
!   1999-01-21  IREDELL
!   2009-04-20  J. WANG
!
! USAGE:    CALL BAFRWRITE(LU,IB,NB,KA,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER LOGICAL UNIT TO WRITE
!     IB           INTEGER FORTRAN RECORD START BYTE
!                  (FOR THE FIRST FORTRAN RECORD, IB SHOULD BE 0)
!     NB           INTEGER NUMBER OF BYTES TO WRITE
!     A            CHARACTER*1 (NB) DATA TO WRITE
!
!   OUTPUT ARGUMENTS:
!     KA           INTEGER NUMBER OF BYTES IN FORTRAN RECORD
!                  (IN WHICH CASE THE NEXT FORTRAN RECORD
!                  SHOULD HAVE A START BYTE OF IB+KA),
!                  OR KA=-1 FOR I/O ERROR
!
! SUBPROGRAMS CALLED:
!   BAWRITEL       BYTE-ADDRESSABLE WRITE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$

!
      IMPLICIT NONE
      INTEGER,INTENT(IN):: LU,IB,NB
      INTEGER,INTENT(OUT):: KA
      CHARACTER,INTENT(IN):: A(NB)
      INTEGER(KIND=8) :: LONG_IB,LONG_NB,LONG_KA
!
        if((IB<0.and.IB/=-1) .or. NB<0 ) THEN
          print *,'WRONG: in BAFRWRITE starting postion IB or read '//   &
     &   'data size NB <0, STOP! ' //                                    &
     &   'Consider use BAFRRWRITEL and long integer'
          KA=0
          return
        ENDIF
        LONG_IB=IB
        LONG_NB=NB
        CALL BAFRWRITEL(LU,LONG_IB,LONG_NB,LONG_KA,A)
        KA=LONG_KA
!
      END SUBROUTINE BAFRWRITE
!-----------------------------------------------------------------------
      SUBROUTINE BAFRWRITEL(LU,IB,NB,KA,A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAFRWRITEL     BYTE-ADDRESSABLE FORTRAN RECORD WRITE
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1999-01-21
!
! ABSTRACT: THIS SUBPROGRAM WRITES AN UNFORMATTED FORTRAN RECORD
!
! PROGRAM HISTORY LOG:
!   1999-01-21  IREDELL
!   2009-04-20  J. WANG 
!
! USAGE:    CALL BAFRWRITEL(LU,IB,NB,KA,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER LOGICAL UNIT TO WRITE
!     IB           INTEGER(8) FORTRAN RECORD START BYTE
!                  (FOR THE FIRST FORTRAN RECORD, IB SHOULD BE 0)
!     NB           INTEGER(8) NUMBER OF BYTES TO WRITE
!     A            CHARACTER*1 (NB) DATA TO WRITE
!
!   OUTPUT ARGUMENTS:
!     KA           INTEGER(8) NUMBER OF BYTES IN FORTRAN RECORD
!                  (IN WHICH CASE THE NEXT FORTRAN RECORD
!                  SHOULD HAVE A START BYTE OF IB+KA),
!                  OR KA=-1 FOR I/O ERROR
!
! SUBPROGRAMS CALLED:
!   BAWRITEL       BYTE-ADDRESSABLE WRITE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      IMPLICIT NONE
      INTEGER,INTENT(IN):: LU
      INTEGER(KIND=8),INTENT(IN):: IB,NB
      INTEGER(kind=8),INTENT(OUT):: KA
      CHARACTER,INTENT(IN):: A(NB)
!
      INTEGER(kind=8),PARAMETER:: LBCW=4
      INTEGER(kind=LBCW):: BCW
      INTEGER(kind=8):: KR
      CHARACTER(16) :: MACHINE_ENDIAN
      LOGICAL :: DO_BYTESWAP
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  WRITE DATA BRACKETED BY BLOCK CONTROL WORDS
!
!-- set do_byteswap from machine endianness and file endianness
      CALL CHK_ENDIANC(MACHINE_ENDIAN)
      IF( LU<=999) THEN
        IF( trim(MACHINE_ENDIAN)=="big_endian") THEN
          DO_BYTESWAP=.false.
        ELSEIF( trim(MACHINE_ENDIAN)=="little_endian") THEN
          DO_BYTESWAP=.true.
        ENDIF
      ELSEIF(LU<=1999) THEN
        IF( trim(MACHINE_ENDIAN)=="big_endian") THEN
          DO_BYTESWAP=.true.
        ELSEIF( trim(MACHINE_ENDIAN)=="little_endian") THEN
          DO_BYTESWAP=.false.
        ENDIF
      ENDIF
!
!
      BCW=NB
      IF(DO_BYTESWAP) CALL Byteswap(BCW,LBCW,1)
      CALL BAWRITEL(LU,IB,LBCW,KR,BCW)
      IF(KR.NE.LBCW) THEN
        KA=-1
      ELSE
        CALL BAWRITEL(LU,IB+LBCW,NB,KR,A)
        IF(KR.NE.NB) THEN
          KA=-1
        ELSE
          CALL BAWRITEL(LU,IB+LBCW+NB,LBCW,KR,BCW)
          IF(KR.NE.LBCW) THEN
            KA=-1
          ELSE
            KA=LBCW+NB+LBCW
          ENDIF
        ENDIF
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END SUBROUTINE  BAFRWRITEL
