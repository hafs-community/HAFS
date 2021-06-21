       PROGRAM CHANGE_PREPBUFR_QM_TYP

C-----------------------------------------------------------------------
C  MAIN PROGRAM CHANGE_PREPBUFR_QM_TYP
C
C  THIS CODE WILL READ THOUGH AN INPUT PREPBUFR FILE (SPECIFIED AS
C   POSITIONAL PARAMETER 1) AND CHANGE ALL QUALITY MARKS OF 14 TO 2 FOR
C   ALL REPORTS OF A PARTICULAR REPORT TYPE (OR TYPES) - NORMALLY, THE
C   INPUT PREPBUFR FILE SHOULD BE A POST-QC/PRE-ANAL VERSION
C
C-----------------------------------------------------------------------
 
      PARAMETER (IQMVAR=7)

      CHARACTER*8  SUBSET,SUBSET_LAST,SID
      CHARACTER*11 CTEXT(IQMVAR)
     
      REAL*8  RID_8(3),OBS_8(5,255),BMISS,GETBMISS
      REAL    RID(3),OBS(5,255)
      REAL*8  QM1(255),QM1x(255)
      REAL*8  QM2(255),QM2x(255)
      REAL*8  QM3(255),QM3x(255)
      REAL*8  QM4(255),QM4x(255)
      REAL*8  QM5(255),QM5x(255)
      REAL*8  QM6(255),QM6x(255)
      REAL*8  QM7(255),QM7x(255)

      EQUIVALENCE (RID_8(1),SID)

      DATA  LUBFI/21/,LUBFJ/51/,IRECI/0/,IRECO/0/,IRECO_LAST/0/,
     $ SUBSET_LAST/'XXXXXXXX'/,IKNTTYP/0/

      DATA  CTEXT/'PRESSURE   ','HEIGHT     ','WIND(U/V)  ',
     $            'WIND(D/S)  ','WIND SPEED ','TEMPERATURE',
     $            'SPEC HUMID '/

C  On WCOSS should always set BUFRLIB missing (BMISS) to 10E8 to avoid
C   overflow when either an INTEGER*4 variable is set to BMISS or a
C   REAL*8 (or REAL*4) variable that is missing is NINT'd
C  -------------------------------------------------------------------
ccccc CALL SETBMISS(10E10_8)
      CALL SETBMISS(10E8_8)
      BMISS=GETBMISS()
      print *
      print *, 'BUFRLIB value for missing is: ',bmiss
      print *

      CALL DATELEN(10)
      CALL OPENBF(LUBFI,'IN',LUBFI)
      PRINT 100, LUBFI
  100 FORMAT(/5X,'===> PREPBUFR DATA SET IN UNIT',I3,
     * ' SUCCESSFULLY OPENED FOR INPUT; FIRST MESSAGES ',
     * 'CONTAIN BUFR TABLES A,B,D'/)

      CALL OPENBF(LUBFJ,'OUT',LUBFI)
      PRINT 101, LUBFJ
  101 FORMAT(/5X,'===> PREPBUFR DATA SET IN UNIT',I3,' SUCCESSFULLY',
     *    ' OPENED FOR OUTPUT'/)

C  READ IN NEXT INPUT BUFR MESSAGE FROM PREPBUFR FILE
C  --------------------------------------------------

      DO WHILE(IREADMG(LUBFI,SUBSET,IDATE).EQ.0)
 
         CALL UFBCNT(LUBFI,IRECI,ISUB)
         PRINT 102, IRECI,SUBSET,IDATE
  102    FORMAT(/5X,'===> READ IN BUFR DATA MESSAGE NUMBER ',I5,' -- ',
     $    'TABLE A ENTRY IS "',A8,'" AND DATE IS',I11/)

CVVVVVVVVVVVVVVVVVVVV
C  Specific to this run:
C  COPY ALL MESSAGES WITH TYPE OTHER THAN ADPUPA TO OUTPUT (REPORT
C   TYPES 132/232 WILL ONLY BE IN ADPUPA MESSAGE TYPES)
C  --------------------------------------------------------------------
CAAAAAAAAAAAAAAAAAAAA

C         IF((SUBSET.NE.'ADPUPA  ') .OR. (SUBSET.NE.'ADPSFC  '))  THEN
C            IF((SUBSET_LAST.EQ.'ADPUPA  ') .OR. (SUBSET_LAST .EQ. 
C     $   'ADPSFC  '))  THEN
C         IF(SUBSET.NE.'ADPSFC  ')  THEN
C            IF(SUBSET_LAST.EQ.'ADPSFC  ')  THEN
         IF(SUBSET(1:3).NE.'ADP')  THEN
            IF(SUBSET_LAST(1:3).EQ.'ADP')  THEN
               CALL CLOSMG(LUBFJ)
               CALL UFBCNT(LUBFJ,IRECO,ISUBO)
               PRINT 104, SUBSET_LAST,IDATE,IRECO
            END IF
            CALL COPYMG(LUBFI,LUBFJ)
            CALL UFBCNT(LUBFJ,IRECO,ISUBO)
      PRINT 103, SUBSET,IDATE,IRECO
  103 FORMAT(/5X,'------> COPIED THIS MESSAGE TO OUTPUT FILE -- (ENTRY',
     $ ' "',A8,'", DATE ',I11,' - NO. OF DATA MESSAGES WRITTEN SO FAR:',
     $ I6/)
            IRECO_LAST  = IRECO
            SUBSET_LAST = SUBSET
            CYCLE
         END IF

         CALL OPENMB(LUBFJ,SUBSET,IDATE)
         CALL UFBCNT(LUBFJ,IRECO,ISUBO)
C         IF(IRECO.NE.IRECO_LAST.AND.IRECO_LAST.NE.0.AND.
C     $    ((SUBSET_LAST.EQ.'ADPUPA  ').OR.(SUBSET_LAST.EQ.'ADPSFC  ')))
C         IF(IRECO.NE.IRECO_LAST.AND.IRECO_LAST.NE.0.AND.SUBSET_LAST.EQ.
C     $    'ADPSFC  ')
         IF(IRECO.NE.IRECO_LAST.AND.IRECO_LAST.NE.0.AND.
     $   SUBSET_LAST(1:3).EQ.'ADP')
     $    PRINT 104, SUBSET_LAST,IDATE,IRECO-1
  104 FORMAT(/5X,'------> WROTE MESSAGE TO OUTPUT FILE -- (ENTRY',
     $ ' "',A8,'", DATE ',I11,' - NO. OF DATA MESSAGES WRITTEN SO FAR:',
     $ I6/)

C  READ A SUBSET (REPORT) IN MESSAGE AND CHECK REPORT TYPE
C  -------------------------------------------------------

         DO WHILE(IREADSB(LUBFI).EQ.0)
            CALL UFBINT(LUBFI,RID_8,3,1,NLV,'SID TYP RPT')
            RID = RID_8

CVVVVVVVVVVVVVVVVVVVV
C  Specific to this run:
C  IF REPORT TYPE IS NOT 132 OR 232, SIMPLY WRITE OUT THE SUBSET (AS
C   IS) TO THE OUTPUT FILE AND MOVE ON TO THE NEXT ONE
C  -----------------------------------------------------------------
CAAAAAAAAAAAAAAAAAAAA

            IF(NINT(RID(2)).EQ.132 .OR. NINT(RID(2)).EQ.232 
     $      .OR. NINT(RID(2)).EQ. 181 .OR. NINT(RID(2)).EQ. 187
     $      .OR. NINT(RID(2)).EQ. 281 .OR. NINT(RID(2)).EQ. 287)
     $      THEN

C  FOR SELECTED REPORT TYPE, FIRST COPY THE SUBSET AS IS TO THE OUTPUT
C   FILE
C  -------------------------------------------------------------------

               CALL UFBCPY(LUBFI,LUBFJ)

C  NEXT, DECODE THE SUBSET (REPORT) LOOKING FOR ONE OR MORE QUALITY
C   MARKS
C  ----------------------------------------------------------------

C  USE UFBREP TO READ HERE BECAUSE UFBINT CANNOT BE USED TO WRITE (SEE
C  BELOW)
C     Note: My tests show that in order to do this properly you have
C           to split up the q.m.'s since they may appear on a different
C           number of levels within the same report. (I tried putting
C           them all together and the output is almost the same but a
C           few levels w/ q.m. changes were lost)
C  --------------------------------------------------------------------


CVVVVVVVVVVVVVVVVVVVV
C  Specific to this run:
C  SELECT ON ALL QUALITY MARKS
C  -----------------------------------
CAAAAAAAAAAAAAAAAAAAA

               CALL UFBREP(LUBFI,QM1,1,255,NLV1,'PQM')
               CALL UFBREP(LUBFI,QM2,1,255,NLV3,'ZQM')
               CALL UFBREP(LUBFI,QM3,1,255,NLV4,'WQM')
               CALL UFBREP(LUBFI,QM4,1,255,NLV5,'DFQ')
               CALL UFBREP(LUBFI,QM5,1,255,NLV6,'SQM')
               CALL UFBREP(LUBFI,QM6,1,255,NLV7,'TQM')
               CALL UFBREP(LUBFI,QM7,1,255,NLV8,'QQM')

C  REPLACE ALL QUALITY MARKS OF 14 WITH THE VALUE 2
C  ------------------------------------------------

               IF(NINT(RID(2)).EQ. 181 .OR. NINT(RID(2)).EQ. 187
     $         .OR. NINT(RID(2)).EQ. 281 .OR. NINT(RID(2)).EQ. 287)
     $         THEN 
                  IF (QM1(1) .GT. 2) GOTO 400 
               END IF

               QM1x=QM1
               I = 1
               IF(NLV1.GT.0)  THEN
                  DO J=1,NLV1
                     IF(QM1(J).GE.9)  THEN
                        PRINT 113,CTEXT(I),NINT(QM1(J)),J,SID,
     $                   NINT(RID(2)),RID(3)
  113 FORMAT('Changed ',A11,' q.m. from ',I2,' to  2 on level ',I3,
     $ ' for ID ',A6,' RTYP ',I3,' at ',F7.3,'UTC')
                        QM1X(J) = 2
                     END IF
                  END DO
               END IF

               QM2x=QM2
               I = 2
               IF(NLV3.GT.0)  THEN
                  DO J=1,NLV3
                     IF(QM2(J).GE.9)  THEN
                        PRINT 113,CTEXT(I),NINT(QM2(J)),J,SID,
     $                   NINT(RID(2)),RID(3)
                        QM2X(J) = 2
                     END IF
                  END DO
               END IF

               QM3x=QM3
               I = 3
               IF(NLV4.GT.0)  THEN
                  DO J=1,NLV4
                     IF(QM3(J).GE.9)  THEN
                        PRINT 113,CTEXT(I),NINT(QM3(J)),J,SID,
     $                   NINT(RID(2)),RID(3)
                        QM3X(J) = 2
                     END IF
                  END DO
               END IF

               QM4x=QM4
               I = 4
               IF(NLV5.GT.0)  THEN
                  DO J=1,NLV5
                     IF(QM4(J).GE.9)  THEN
                        PRINT 113,CTEXT(I),NINT(QM4(J)),J,SID,
     $                   NINT(RID(2)),RID(3)
                        QM4X(J) = 2
                     END IF
                  END DO
               END IF

               QM5x=QM5
               I = 5
               IF(NLV6.GT.0)  THEN
                  DO J=1,NLV6
                     IF(QM5(J).GE.9)  THEN
                        PRINT 113,CTEXT(I),NINT(QM5(J)),J,SID,
     $                   NINT(RID(2)),RID(3)
                        QM5X(J) = 2
                     END IF
                  END DO
               END IF

               QM6x=QM6
               I = 6
               IF(NLV7.GT.0)  THEN
                  DO J=1,NLV7
                     IF(QM6(J).GE.9)  THEN
                        PRINT 113,CTEXT(I),NINT(QM6(J)),J,SID,
     $                   NINT(RID(2)),RID(3)
                        QM6X(J) = 2
                     END IF
                  END DO
               END IF

               QM7x=QM7
               I = 7
               IF(NLV8.GT.0)  THEN
                  DO J=1,NLV8
                     IF(QM7(J).GE.9)  THEN
                        PRINT 113,CTEXT(I),NINT(QM7(J)),J,SID,
     $                   NINT(RID(2)),RID(3)
                        QM7X(J) = 2
                     END IF
                  END DO
               END IF

C  WRITE ALL QUALITY MARKS (INCLUDING THOSE UPDATED) (FOR THE TYPES
C   SELECTED) TO OUTPUT FILE (UFBREP WILL REPLACE ORIGINAL VALUES
C   WITHOUT GENERATING AN EVENT LIKE UFBINT WOULD) (USING UFBINT HERE
C   WOULD ALSO WRITE MISSING OBS VALUES!)
C  ------------------------------------------------------------------

               IF(NLV1.GT.0) CALL UFBREP(LUBFJ,QM1x,1,NLV1,IRET,'PQM')
               IF(NLV3.GT.0) CALL UFBREP(LUBFJ,QM2x,1,NLV3,IRET,'ZQM')
               IF(NLV4.GT.0) CALL UFBREP(LUBFJ,QM3x,1,NLV4,IRET,'WQM')
               IF(NLV5.GT.0) CALL UFBREP(LUBFJ,QM4x,1,NLV5,IRET,'DFQ')
               IF(NLV6.GT.0) CALL UFBREP(LUBFJ,QM5x,1,NLV6,IRET,'SQM')
               IF(NLV7.GT.0) CALL UFBREP(LUBFJ,QM6x,1,NLV7,IRET,'TQM')
               IF(NLV8.GT.0) CALL UFBREP(LUBFJ,QM7x,1,NLV8,IRET,'QQM')

CVVVVVVVVVVVVVVVVVVVV
C  Specific to this run:
C  FINALLY, CHANGE ALL QUALITY MARKS ON THE SURFACE LEVEL (CATEGORY 0)
C   BACK TO 15 SINCE ALL SURFACE DATA IS FLAGGED FOR REPORT TYPE 132
C   (THE SURFACE LEVEL DATA ARE LATER INCORPORATED INTO A UNIQUE
C   SPLASH-LEVEL SURFACE REPORT WITH TYPE 182)
C
C   Note: Here must use UFBINT to read in data to align proper CAT and
C         xQM values - must also read in xOB, xPC and xRC because we
C         will be writing back the updated xQM as an event soon (and
C         the other values would be written back as missing unless we
C         read them in first)
C
C   Note: Won't change the xQM unless it is < 15
C   Note: If this is NOT the surface level (i.e., CAT .ne. 0) or the
C         xQM on the surface level is NOT 15, then set all of the event
C         values xQB, xQM, xPC and xRC to missing - this will prevent
C         an event from being written out in the upcoming call TO UFBINT
C  ---------------------------------------------------------------------
            IF(NINT(RID(2)).EQ.132 .OR. NINT(RID(2)).EQ.232) THEN 

               I = 1
              CALL UFBINT(-LUBFJ,OBS_8,5,255,NLEV,'CAT POB PQM PPC PRC')
               OBS=OBS_8
               IF(NLEV.GT.0)  THEN
                  DO L=1,NLEV
                     IF(NINT(OBS(1,L)).EQ.0 .AND. OBS(3,L).LT.15.) THEN
                        PRINT 213,CTEXT(I),NINT(OBS(3,L)),L,
     $                   NINT(OBS(1,L)),SID,NINT(RID(2)),RID(3)
  213 FORMAT('Changed ',A11,' q.m. from ',I2,' to  15 on level ',I3,
     $ ' cat ',I3,' for ID ',A6,' RTYP ',I3,' at ',F7.3,'UTC')
                        OBS(3,L) = 15
                     ELSE
                        OBS(2:5,L) = BMISS
                     END IF
                  END DO
                  OBS_8=OBS

C  USE UFBINT TO WRITE OUT THE NEW EVENT WITH THE CHANGED SURFACE PQM -
C   AN EVENT WILL NOT BE WRITTEN IF POB, PQM, PPC AND PRC ARE MISSING
C  --------------------------------------------------------------------

              CALL UFBINT(LUBFJ,OBS_8,5,NLEV,IRET,'CAT POB PQM PPC PRC')
               END IF
Cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
               I = 2
              CALL UFBINT(-LUBFJ,OBS_8,5,255,NLEV,'CAT ZOB ZQM ZPC ZRC')
               OBS=OBS_8
               IF(NLEV.GT.0)  THEN
                  DO L=1,NLEV
                     IF(NINT(OBS(1,L)).EQ.0 .AND. OBS(3,L).LT.15.) THEN
                        PRINT 213,CTEXT(I),NINT(OBS(3,L)),L,
     $                   NINT(OBS(1,L)),SID,NINT(RID(2)),RID(3)
                        OBS(3,L) = 15
                     ELSE
                        OBS(2:5,L) = BMISS
                     END IF
                  END DO
                  OBS_8=OBS

C  USE UFBINT TO WRITE OUT THE NEW EVENT WITH THE CHANGED SURFACE ZQM -
C   AN EVENT WILL NOT BE WRITTEN IF ZOB, ZQM, ZPC AND ZRC ARE MISSING
C  --------------------------------------------------------------------

              CALL UFBINT(LUBFJ,OBS_8,5,NLEV,IRET,'CAT ZOB ZQM ZPC ZRC')
               END IF
Cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
               I = 6
              CALL UFBINT(-LUBFJ,OBS_8,5,255,NLEV,'CAT TOB TQM TPC TRC')
               OBS=OBS_8
               IF(NLEV.GT.0)  THEN
                  DO L=1,NLEV
                     IF(NINT(OBS(1,L)).EQ.0 .AND. OBS(3,L).LT.15.) THEN
                        PRINT 213,CTEXT(I),NINT(OBS(3,L)),L,
     $                   NINT(OBS(1,L)),SID,NINT(RID(2)),RID(3)
                        OBS(3,L) = 15
                     ELSE
                        OBS(2:5,L) = BMISS
                     END IF
                  END DO
                  OBS_8=OBS

C  USE UFBINT TO WRITE OUT THE NEW EVENT WITH THE CHANGED SURFACE TQM -
C   AN EVENT WILL NOT BE WRITTEN IF TOB, TQM, TPC AND TRC ARE MISSING
C  --------------------------------------------------------------------

              CALL UFBINT(LUBFJ,OBS_8,5,NLEV,IRET,'CAT TOB TQM TPC TRC')
               END IF
Cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
               I = 7
              CALL UFBINT(-LUBFJ,OBS_8,5,255,NLEV,'CAT QOB QQM QPC QRC')
               OBS=OBS_8
               IF(NLEV.GT.0)  THEN
                  DO L=1,NLEV
                     IF(NINT(OBS(1,L)).EQ.0 .AND. OBS(3,L).LT.15.) THEN
                        PRINT 213,CTEXT(I),NINT(OBS(3,L)),L,
     $                   NINT(OBS(1,L)),SID,NINT(RID(2)),RID(3)
                        OBS(3,L) = 15
                     ELSE
                        OBS(2:5,L) = BMISS
                     END IF
                  END DO
                  OBS_8=OBS

C  FINALLY, USE UFBINT TO WRITE OUT THE NEW EVENT WITH THE CHANGED
C   SURFACE QQM - AN EVENT WILL NOT BE WRITTEN IF QOB, QQM, QPC AND QRC
C   ARE MISSING
C  --------------------------------------------------------------------

              CALL UFBINT(LUBFJ,OBS_8,5,NLEV,IRET,'CAT QOB QQM QPC QRC')
               END IF
Cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

CAAAAAAAAAAAAAAAAAAAA

C  WRITE OUT THE SUBSET AND MOVE ON THE THE NEXT INPUT SUBSET
C  ----------------------------------------------------------
            ENDIF
400            CALL WRITSB(LUBFJ)

               IKNTTYP = IKNTTYP + 1

               CYCLE

            ENDIF
         
            CALL UFBCPY(LUBFI,LUBFJ)
            CALL WRITSB(LUBFJ)
         END DO
         SUBSET_LAST = SUBSET
         IRECO_LAST  = IRECO
      END DO

C  ALL MESSAGES IN INPUT PREPBUFR FILE HAVE BEEN READ AND PROCESSED
C  ----------------------------------------------------------------

      CALL CLOSBF(LUBFJ)
      CALL CLOSBF(LUBFJ)

      print *, 'COMPLETED!!'
      print *
      print *, '    - number of reports of selected type(s) ',
     $ 'encountered = ',ikntTYP
      print *

      STOP
 
      END
