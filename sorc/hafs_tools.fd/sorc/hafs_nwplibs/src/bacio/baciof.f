!-----------------------------------------------------------------------
      MODULE BACIO_MODULE
!$$$  F90-MODULE DOCUMENTATION BLOCK
!
! F90-MODULE: BACIO_MODULE   BYTE-ADDRESSABLE I/O MODULE
!   PRGMMR: IREDELL          ORG: NP23        DATE: 98-06-04
!
! ABSTRACT: MODULE TO SHARE FILE DESCRIPTORS
!   IN THE BYTE-ADDESSABLE I/O PACKAGE.
!
! PROGRAM HISTORY LOG:
!   98-06-04  IREDELL
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      INTEGER,EXTERNAL:: BACIO,BACIOL
      INTEGER,PARAMETER :: FDDIM=9999
      INTEGER,DIMENSION(FDDIM),SAVE:: FD=FDDIM*0
      INTEGER,DIMENSION(20),SAVE:: BAOPTS=0
      INCLUDE 'baciof.h'
      END
!-----------------------------------------------------------------------
      SUBROUTINE BASETO(NOPT,VOPT)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BASETO         BYTE-ADDRESSABLE SET OPTIONS
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: SET OPTIONS FOR BYTE-ADDRESSABLE I/O.
!   ALL OPTIONS DEFAULT TO 0.
!   OPTION 1: BLOCKED READING OPTION
!             IF THE OPTION VALUE IS 1, THEN THE READING IS BLOCKED
!             INTO FOUR 4096-BYTE BUFFERS.  THIS MAY BE EFFICIENT IF
!             THE READS WILL BE REQUESTED IN MUCH SMALLER CHUNKS.
!             OTHERWISE, EACH CALL TO BAREAD INITIATES A PHYSICAL READ.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BASETO(NOPT,VOPT)
!   INPUT ARGUMENTS:
!     NOPT         INTEGER OPTION NUMBER
!     VOPT         INTEGER OPTION VALUE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      USE BACIO_MODULE
      INTEGER NOPT,VOPT
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(NOPT.GE.1.AND.NOPT.LE.20) BAOPTS(NOPT)=VOPT
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------
      SUBROUTINE BAOPEN(LU,CFN,IRET)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAOPEN         BYTE-ADDRESSABLE OPEN
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BAOPEN(LU,CFN,IRET)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO OPEN
!     CFN          CHARACTER FILENAME TO OPEN
!                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
!   OUTPUT ARGUMENTS:
!     IRET         INTEGER RETURN CODE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
      integer(kind=8) IB,JB,NB,KA
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.FDDIM) THEN
        IRET=6
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_OPENRW,IB,JB,1,NB,KA,FD(LU),CFN,A)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------
      SUBROUTINE BAOPENR(LU,CFN,IRET)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAOPENR        BYTE-ADDRESSABLE OPEN
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR READ ONLY.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BAOPENR(LU,CFN,IRET)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO OPEN
!     CFN          CHARACTER FILENAME TO OPEN
!                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
!   OUTPUT ARGUMENTS:
!     IRET         INTEGER RETURN CODE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
      INTEGER LU,iret
      integer(kind=8) IB,JB,NB,KA
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.FDDIM) THEN
        IRET=6
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_OPENR,IB,JB,1,NB,KA,FD(LU),CFN,A)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------
      SUBROUTINE BAOPENW(LU,CFN,IRET)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAOPENW        BYTE-ADDRESSABLE OPEN
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR WRITE ONLY.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BAOPENW(LU,CFN,IRET)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO OPEN
!     CFN          CHARACTER FILENAME TO OPEN
!                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
!   OUTPUT ARGUMENTS:
!     IRET         INTEGER RETURN CODE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
      integer(kind=8) IB,JB,NB,KA
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.FDDIM) THEN
        IRET=6
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_OPENW,IB,JB,1,NB,KA,FD(LU),CFN,A)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------
      SUBROUTINE BAOPENWT(LU,CFN,IRET)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAOPENWT       BYTE-ADDRESSABLE OPEN
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR WRITE ONLY WITH TRUNCATION.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BAOPENWT(LU,CFN,IRET)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO OPEN
!     CFN          CHARACTER FILENAME TO OPEN
!                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
!   OUTPUT ARGUMENTS:
!     IRET         INTEGER RETURN CODE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
      integer(kind=8) IB,JB,NB,KA
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.FDDIM) THEN
        IRET=6
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_OPENWT,IB,JB,1,NB,KA,FD(LU),CFN,A)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------
      SUBROUTINE BAOPENWA(LU,CFN,IRET)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAOPENWA       BYTE-ADDRESSABLE OPEN
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR WRITE ONLY WITH APPEND.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BAOPENWA(LU,CFN,IRET)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO OPEN
!     CFN          CHARACTER FILENAME TO OPEN
!                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
!   OUTPUT ARGUMENTS:
!     IRET         INTEGER RETURN CODE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
      integer(kind=8) IB,JB,NB,KA
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.FDDIM) THEN
        IRET=6
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_OPENWA,IB,JB,1,NB,KA,FD(LU),CFN,A)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------
      SUBROUTINE BACLOSE(LU,IRET)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BACLOSE        BYTE-ADDRESSABLE CLOSE
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: CLOSE A BYTE-ADDRESSABLE FILE.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!
! USAGE:    CALL BACLOSE(LU,IRET)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO CLOSE
!   OUTPUT ARGUMENTS:
!     IRET         INTEGER RETURN CODE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      USE BACIO_MODULE
      integer(kind=8) IB,JB,NB,KA
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(LU.LT.001.OR.LU.GT.FDDIM) THEN
        IRET=6
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIOL(BACIO_CLOSE,IB,JB,1,NB,KA,FD(LU),CHAR(0),A)
      IF(IRET.EQ.0) FD(LU)=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
!-----------------------------------------------------------------------
      SUBROUTINE BAREAD(LU,IB,NB,KA,A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAREAD         BYTE-ADDRESSABLE READ
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: THIS BAREAD IS CALLING BAREADL TO READ A GIVEN NUMBER OF 
!   BYTES FROM AN UNBLOCKED FILE,SKIPPING A GIVEN NUMBER OF BYTES.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!   2009-04-20  J. WANG
!
! USAGE:    CALL BAREAD(LU,IB,NB,KA,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO READ
!     IB           INTEGER NUMBER OF BYTES TO SKIP
!                  (IF IB<0, THEN THE FILE IS ACCESSED WITH NO SEEKING)
!     NB           INTEGER NUMBER OF BYTES TO READ
!   OUTPUT ARGUMENTS:
!     KA           INTEGER NUMBER OF BYTES ACTUALLY READ
!     A            CHARACTER*1 (NB) DATA READ
!
! SUBPROGRAMS CALLED:
!   BAREADL        BYTE-ADDRESSABLE READ SUBROUTINE
!
! REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
!
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: LU,IB,NB
        INTEGER,INTENT(OUT) :: KA
        CHARACTER,INTENT(OUT) :: A(NB)
        INTEGER(KIND=8) :: LONG_IB,LONG_NB,LONG_KA
!
        if(NB<0 ) THEN
          print *,'WRONG: in BAREAD read data size NB < 0, STOP! '//&
         'Consider using BAREADL and long integer'
          KA=0
          return
        ENDIF
        LONG_IB=IB
        LONG_NB=NB
        CALL BAREADL(LU,LONG_IB,LONG_NB,LONG_KA,A)
        KA=LONG_KA

      END SUBROUTINE BAREAD
!-----------------------------------------------------------------------
      SUBROUTINE BAREADL(LU,IB,NB,KA,A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAREAD         BYTE-ADDRESSABLE READ
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: THIS SUBROUYTINE IS USING UPDATED BACIOL I/O PACKAGE TO READ 
!   A GIVEN NUMBER OF BYTES FROM AN UNBLOCKED FILE, SKIPPING A GIVEN 
!   NUMBER OF BYTES. 
!   THE PHYSICAL I/O IS BLOCKED INTO FOUR 4096-BYTE BUFFERS
!   IF THE BYTE-ADDRESSABLE OPTION 1 HAS BEEN SET TO 1 BY BASETO.
!   THIS BUFFERED READING IS INCOMPATIBLE WITH NO-SEEK READING.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!   2009-04-20  J. WANG
!
! USAGE:    CALL BAREAD(LU,IB,NB,KA,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO READ
!     IB           INTEGER(8) NUMBER OF BYTES TO SKIP
!                  (IF IB<0, THEN THE FILE IS ACCESSED WITH NO SEEKING)
!     NB           INTEGER(8) NUMBER OF BYTES TO READ
!   OUTPUT ARGUMENTS:
!     KA           INTEGER(8) NUMBER OF BYTES ACTUALLY READ
!     A            CHARACTER*1 (NB) DATA READ
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIOL         BYTE-ADDRESSABLE I/O C PACKAGE
!
! REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      USE BACIO_MODULE
!    
      IMPLICIT NONE
      INTEGER,intent(in)          :: LU
      INTEGER(kind=8),intent(in)  :: IB,NB
      INTEGER(kind=8),intent(out) :: KA
      CHARACTER,intent(out)       :: A(NB)
      integer(kind=8),PARAMETER :: NY=4096,MY=4
      INTEGER(KIND=8) NS(MY),NN(MY)
      INTEGER(kind=8) JB,LONG_0,KY,I,K,IY,JY,LUX
      INTEGER IRET
!      INTEGER LU,IB,NB,KA
      CHARACTER Y(NY,MY)
      DATA LUX/0/
      SAVE JY,NS,NN,Y,LUX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(FD(LU).LE.0) THEN
        KA=0
        RETURN
      ENDIF
      IF(IB.LT.0.AND.BAOPTS(1).EQ.1) THEN
        KA=0
        RETURN
      ENDIF
      IF(NB.LE.0) THEN
        KA=0
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      LONG_0=0                                                         
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  UNBUFFERED I/O
      IF(BAOPTS(1).NE.1) THEN
        KA=0
        IF(IB.GE.0) THEN
          IRET=BACIOL(BACIO_READ,IB,JB,1,NB,KA,FD(LU),CHAR(0),A)
        ELSE
          IRET=BACIOL(BACIO_READ+BACIO_NOSEEK,LONG_0,JB,1,NB,KA,&
                      FD(LU),CHAR(0),A)
        ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  BUFFERED I/O
!  GET DATA FROM PREVIOUS CALL IF POSSIBLE
      ELSE
        KA=0
        IF(LUX.NE.LU) THEN
          JY=0
          NS=0
          NN=0
        ELSE
          DO I=1,MY
            IY=MOD(JY+I-1,MY)+1
            KY=IB+KA-NS(IY)
            IF(KA.LT.NB.AND.KY.GE.LONG_0.AND.KY.LT.NN(IY)) THEN
              K=MIN(NB-KA,NN(IY)-KY)
              A(KA+1:KA+K)=Y(KY+1:KY+K,IY)
              KA=KA+K
            ENDIF
          ENDDO
        ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  SET POSITION AND READ BUFFER AND GET DATA
        IF(KA.LT.NB) THEN
          LUX=ABS(LU)
          JY=MOD(JY,MY)+1
          NS(JY)=IB+KA
          IRET=BACIOL(BACIO_READ,NS(JY),JB,1,NY,NN(JY),&
                     FD(LUX),CHAR(0),Y(1,JY))
          IF(NN(JY).GT.0) THEN
            K=MIN(NB-KA,NN(JY))
            A(KA+1:KA+K)=Y(1:K,JY)
            KA=KA+K
          ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  CONTINUE TO READ BUFFER AND GET DATA
          DO WHILE(NN(JY).EQ.NY.AND.KA.LT.NB)
            JY=MOD(JY,MY)+1
            NS(JY)=NS(JY)+NN(JY)
            IRET=BACIOL(BACIO_READ+BACIO_NOSEEK,NS(JY),JB,1,NY,NN(JY),&
                        FD(LUX),CHAR(0),Y(1,JY))
            IF(NN(JY).GT.0) THEN
              K=MIN(NB-KA,NN(JY))
              A(KA+1:KA+K)=Y(1:K,JY)
              KA=KA+K
            ENDIF
          ENDDO
        ENDIF
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END SUBROUTINE BAREADL
!-----------------------------------------------------------------------
      SUBROUTINE BAWRITE(LU,IB,NB,KA,A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAWRITE        BYTE-ADDRESSABLE WRITE
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: THIS PROGRAM IS CALLING BAWRITEL TO WRITE A GIVEN NUMBER OF 
!   BYTES TO AN UNBLOCKED FILE,SKIPPING A GIVEN NUMBER OF BYTES.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!   2009-04-20  J. WANG
!
! USAGE:    CALL BAWRITE(LU,IB,NB,KA,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO WRITE
!     IB           INTEGER NUMBER OF BYTES TO SKIP
!                  (IF IB<0, THEN THE FILE IS ACCESSED WITH NO SEEKING)
!     NB           INTEGER NUMBER OF BYTES TO WRITE
!     A            CHARACTER*1 (NB) DATA TO WRITE
!   OUTPUT ARGUMENTS:
!     KA           INTEGER NUMBER OF BYTES ACTUALLY WRITTEN
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
!
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: LU,IB,NB
        INTEGER,INTENT(OUT) :: KA
        CHARACTER,INTENT(IN) :: A(NB)
        INTEGER(KIND=8) :: LONG_IB,LONG_NB,LONG_KA
!
        if(NB<0 ) THEN
          print *,'WRONG: in BAWRITE read data size NB <0, STOP! '//&
          'Consider using BAWRITEL and long integer'
          KA=0
          return
        ENDIF
!
        LONG_IB=IB
        LONG_NB=NB
        CALL BAWRITEL(LU,LONG_IB,LONG_NB,LONG_KA,A)
        KA=LONG_KA         

      END SUBROUTINE BAWRITE
!-----------------------------------------------------------------------
      SUBROUTINE BAWRITEL(LU,IB,NB,KA,A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAWRITEL       BYTE-ADDRESSABLE WRITE
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: THIS SUBROUYTINE IS USING UPDATED BACIOL I/O PACKAGE TO WRITE
!   A GIVEN NUMBER OF BYTES TO AN UNBLOCKED FILE, SKIPPING A GIVEN NUMBER
!   OF BYTES.
!
! PROGRAM HISTORY LOG:
!   1998-06-04  IREDELL
!   2009-04-20  J. WANG
!
! USAGE:    CALL BAWRITEL(LU,IB,NB,KA,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO WRITE
!     IB           INTEGER(8) NUMBER OF BYTES TO SKIP
!                  (IF IB<0, THEN THE FILE IS ACCESSED WITH NO SEEKING)
!     NB           INTEGER(8) NUMBER OF BYTES TO WRITE
!     A            CHARACTER*1 (NB) DATA TO WRITE
!   OUTPUT ARGUMENTS:
!     KA           INTEGER(8) NUMBER OF BYTES ACTUALLY WRITTEN
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIOL         BYTE-ADDRESSABLE I/O C PACKAGE
!
! REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      USE BACIO_MODULE
!
      IMPLICIT NONE
!
      INTEGER,intent(in)         :: LU
      INTEGER(kind=8),intent(in) :: IB,NB
      INTEGER(kind=8),intent(out):: KA
      CHARACTER,intent(in) ::  A(NB)
!
      INTEGER(kind=8) :: JB,LONG_0
      INTEGER :: IRET
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(FD(LU).LE.0) THEN
        KA=0
        RETURN
      ENDIF
      IF(NB.LE.0) THEN
        KA=0
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      LONG_0=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(IB.GE.0) THEN
        KA=0
        IRET=BACIOL(BACIO_WRITE,IB,JB,1,NB,KA,FD(LU),CHAR(0),A)
      ELSE
        KA=0
        IRET=BACIOL(BACIO_WRITE+BACIO_NOSEEK,LONG_0,JB,1,NB,KA,&
                    FD(LU),CHAR(0),A)
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END SUBROUTINE  BAWRITEL
!-----------------------------------------------------------------------
      SUBROUTINE WRYTE(LU,NB,A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: WRYTE          WRITE DATA OUT BY BYTES
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: THSI SUBROUTINE IS CALLING WRYTEL TO WRITE A GIVEN NUMBER OF 
!   BYTES TO AN UNBLOCKED FILE.
!
! PROGRAM HISTORY LOG:
!   92-10-31  IREDELL
!   95-10-31  IREDELL     WORKSTATION VERSION
!   1998-06-04  IREDELL   BACIO VERSION
!   2009-04-20  J. WANG  WRYTEL VERSION
!
! USAGE:    CALL WRYTE(LU,NB,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO WHICH TO WRITE
!     NB           INTEGER(4) NUMBER OF BYTES TO WRITE
!     A            CHARACTER*1 (NB) DATA TO WRITE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
!
! REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
!
      USE BACIO_MODULE
!
      IMPLICIT NONE
!
      INTEGER,intent(in) :: LU
      INTEGER,intent(in) :: NB
      CHARACTER,intent(in) ::  A(NB)
      INTEGER(kind=8) :: LONG_NB
!
      IF(NB<0) THEN
       PRINT *,'WRONG: NB: the number of bytes to write  <0, STOP!'
       RETURN
      ENDIF
      LONG_NB=NB
      CALL WRYTEL(LU,LONG_NB,A)
!
      END SUBROUTINE WRYTE
!-----------------------------------------------------------------------
      SUBROUTINE WRYTEL(LU,NB,A)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: WRYTE          WRITE DATA OUT BY BYTES
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
!
! ABSTRACT: WRITE A GIVEN NUMBER OF BYTES TO AN UNBLOCKED FILE.
!
! PROGRAM HISTORY LOG:
!   92-10-31  IREDELL
!   95-10-31  IREDELL     WORKSTATION VERSION
!   1998-06-04  IREDELL   BACIO VERSION
!   2009-04-20  J. WANG   BACIOL VERSION
!
! USAGE:    CALL WRYTE(LU,NB,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO WHICH TO WRITE
!     NB           INTEGER(8) NUMBER OF BYTES TO WRITE
!     A            CHARACTER*1 (NB) DATA TO WRITE
!
! MODULES USED:
!   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
!
! SUBPROGRAMS CALLED:
!   BACIOL         BYTE-ADDRESSABLE I/O C PACKAGE
!
! REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      USE BACIO_MODULE
!
      IMPLICIT NONE
      INTEGER,intent(in) :: LU
      INTEGER(kind=8),intent(in) :: NB
      CHARACTER,INTENT(in)       :: A(NB)
      INTEGER(kind=8) :: LONG_0,JB,KA
      INTEGER :: IRET
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(FD(LU).LE.0) THEN
        RETURN
      ENDIF
      IF(NB.LE.0) THEN
        RETURN
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      LONG_0=0
      KA=0
      JB=0
      IRET=BACIOL(BACIO_WRITE+BACIO_NOSEEK,LONG_0,JB,1,NB,KA,&
                  FD(LU),CHAR(0),A)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
