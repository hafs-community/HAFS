
       PROGRAM CHANGE_PREPBUFR_QM_IN_CIRCLE

C-----------------------------------------------------------------------
C  MAIN PROGRAM CHANGE_PREPBUFR_QM_IN_CIRCLE
C
C   PRGMMR: M. TONG  DATE: 2012-03-13
C   Modified subroutine EDTPRP for HWRF, but do not add BOGUS data
C   Flag psob within max(ROCI,RBLDC), when categorary is greater than 0 
C   and input RRBLD > 0.
C   Flag dropsonde u,v obs within max(111.,3*RMW,RRADC), when input
C   RRADC > 0 
C
C SUBPROGRAM:    EDTPRP      COPIES/EDITS PREPBUFR FILE, ADDS BOGUS
C   PRGMMR: D. A. KEYSER     ORG: NP22       DATE: 2005-07-01
C
C   ABSTRACT: FIRST, READS THROUGH THE INPUT PREPBUFR FILE AND COPIES
C   ALL DATA TO THE OUTPUT PREPBUFR FILE.  IN THE PROCESS OF DOING
C   THIS, - IF REQUESTED - CHECKS FOR MASS REPORTS IN THE VICINITY OF
C   ALL STORMS PROCESSED AND FLAGS THE PRESSURE Q.M. ON ALL LEVELS
C   FOR THESE REPORTS, ALSO - IF REQUESTED - CHECKS FOR DROPWINSONDE
C   WIND REPORTS IN THE VICINITY OF ALL STORMS PROCESSED AND FLAGS THE
C   WIND Q.M. ON ALL LEVELS FOR THESE REPORTS (SEE COMMENTS IN THIS
C   SUBROUTINE FOR DEFINITION OF "IN THE VICINITY").  IN BOTH CASES,
C   THIS WILL PREVENT THESE REPORTS FROM BEING ASSIMILATED BY THE
C   ANALYSIS.
C
C-----------------------------------------------------------------------

      CHARACTER*52 HDSTR,PESTR,WESTR
      character*9  stmnam
      CHARACTER*80 CRADC,CBLDC
      CHARACTER*8  STNID,SUBSET,LAST
      INTEGER LSTORM
      PARAMETER(LSTORM=20)

      character*4     tcv_center
      character*3     tcv_storm_id
      character*9     tcv_storm_name  ! Storm name
      integer tcv_century
      integer tcv_yymmdd
      integer tcv_hhmm
      integer tcv_lat
      character*1     tcv_latns
      integer tcv_lon
      character*1     tcv_lonew
      integer tcv_stdir
      integer tcv_stspd
      integer tcv_pcen
      integer tcv_penv
      integer tcv_penvrad
      integer tcv_vmax
      integer tcv_vmaxrad
      integer tcv_r341
      integer tcv_r342
      integer tcv_r343
      integer tcv_r344
      character*1     tcv_depth
      integer tcv_r501
      integer tcv_r502
      integer tcv_r503
      integer tcv_r504
      integer tcv_mfh
      integer tcv_flat
      character*1     tcv_flatns
      integer tcv_flon
      character*1     tcv_flonew
      integer tcv_r641
      integer tcv_r642
      integer tcv_r643
      integer tcv_r644

      INTEGER RDIST64

      DIMENSION    HDR(6),PES(4,255),WES(5,255),TZS(13,255),UVS(10,255)
      DIMENSION    RMW(LSTORM),ROCI(LSTORM),STMLAT(LSTORM),
     $             STMLON(LSTORM),RLON(LSTORM),vmax(lstorm),
     $             icat(lstorm),padj(0:5),stmnam(lstorm)

      REAL(8)      HDR_8(6),PES_8(4,255),WES_8(5,255)
      EQUIVALENCE  (STNID,HDR_8(1))

      LOGICAL  FLMASS,DROP,FLBWND,UFLBWND,CFLBWND

      DATA HDSTR/'SID XOB YOB DHR TYP TSB                             '/
      DATA PESTR/'POB PQM PPC PRC                                     '/
      DATA WESTR/'UOB VOB WQM WPC WRC                                 '/

      DATA  IUNTPN/21/,IUNTPO/51/

      DATA BMISS/10E10/

C Storm center pressure adjustment (from guess pressure) based on
C  Category -- cat.  0    1    2    3    4    5
      data padj /    0.,  0.,  4.,  8., 12., 15.  /

      SAVE

      CALL GETENV('RRADC',CRADC)
      IF(CRADC.EQ.' ') CALL ERREXIT(99)
      CALL GETENV('RBLDC',CBLDC)
      IF(CBLDC.EQ.' ') CALL ERREXIT(99)
      READ(CRADC,'(F6.0)') RRADC
      READ(CBLDC,'(F6.0)') RBLDC

      print *, 'RRADC, RBLDC: ', RRADC, RBLDC
      if(RBLDC > 0.0)then
         FLMASS=.true.
      else
         FLMASS=.false.
      end if
      if(RRADC > 0.0)then
         FLBWND=.true.
      else
         FLBWND=.false.
      end if
      if(RRADC < 0.0)then
         UFLBWND=.true.
      else
         UFLBWND=.false.
      end if
      if(FLBWND .OR. UFLBWND)then
         CFLBWND=.true.
      else
         CFLBWND=.false.
      end if
 
      print *, 'FLMASS, FLBWND=', FLMASS, FLBWND
      print *, 'UFLBWND=', UFLBWND
      print *, 'CFLBWND=', CFLBWND

C Read tcvitals, incluing R34, R50, and R64 quadrants 1-4
      OPEN(11,file='tcvitals',form='formatted')
      ii=0
      do while (.true.)
         read (11,22,END=801,ERR=891) tcv_center,tcv_storm_id,
     $   tcv_storm_name,tcv_century,tcv_yymmdd,tcv_hhmm,tcv_lat,
     $   tcv_latns,tcv_lon,tcv_lonew,tcv_stdir,tcv_stspd,tcv_pcen,
     $   tcv_penv,tcv_penvrad,tcv_vmax,tcv_vmaxrad,tcv_r341,tcv_r342,
     $   tcv_r343,tcv_r344,tcv_depth,tcv_r501,tcv_r502,tcv_r503,
     $   tcv_r504,tcv_mfh,tcv_flat,tcv_flatns,tcv_flon,tcv_flonew,
     $   tcv_r641,tcv_r642,tcv_r643,tcv_r644
         ii = ii + 1
      enddo

  801 continue

      numstorms=ii
      rewind(11)

      LAST = 'XXXXXXXX'

C  OPEN THE INPUT AND OUTPUT BUFR FILES
C  ------------------------------------

      CALL DATELEN(10)

      CALL OPENBF(IUNTPN,'IN',IUNTPN)
      CALL READMG(IUNTPN,SUBSET,IDATBF,IRET)
      IF(IRET.NE.0)  THEN
         PRINT *, 'EDTPRP - ERROR OPENING INPUT PREPBUFR FILE'
         CALL ERREXIT(20)
      END IF
      PRINT 789, IUNTPN,IDATBF
  789 FORMAT(/' ===> EDTPRP: INPUT PREPBUFR FILE IN UNIT',I3,
     $ ' SUCCESSFULLY OPENED - SECTION 1 MESSAGE DATE =',I11/)

      IF(IDATBF.LT.1000000000)  THEN

C If 2-digit year returned in IDATBF, must use "windowing" technique
C  to create a 4-digit year

C IMPORTANT: IF DATELEN(10) IS CALLED, THE DATE HERE SHOULD ALWAYS
C            CONTAIN A 4-DIGIT YEAR, EVEN IF INPUT FILE IS NOT
C            Y2K COMPLIANT (BUFRLIB DOES THE WINDOWING HERE)

         PRINT *, '##SYNDATA - THE FOLLOWING SHOULD NEVER HAPPEN!!!!!'
         PRINT *, '##SYNDATA - 2-DIGIT YEAR IN IDATBF RETURNED FROM ',
     $    'READMG (IDATBF IS: ',IDATBF,') - USE WINDOWING TECHNIQUE ',
     $    'TO OBTAIN 4-DIGIT YEAR'
         IF(IDATBF/1000000.GT.20)  THEN
            IDATBF = 1900000000 + IDATBF
         ELSE
            IDATBF = 2000000000 + IDATBF
         ENDIF
         PRINT *, '##SYNDATA - CORRECTED IDATBF WITH 4-DIGIT YEAR, ',
     $    'IDATBF NOW IS: ',IDATBF
      ENDIF

      PRINT 1234
 1234 FORMAT(//' ===> EDTPRP: THE ENTIRE PREPBUFR FILE WILL BE READ, ',
     $ 'ALL MESSAGES ARE COPIED FROM INPUT PREPBUFR FILE TO OUTPUT ',
     $ 'PREPBUFR FILE.'/15X,'STORM BOUNDARIES AND RADIUS OF MAXIMUM ',
     $ 'SURFACE WIND WILL FOLLOW.'//)
      IF(FLMASS) PRINT 1335
 1335 FORMAT(' ===> EDTPRP: ALL MASS REPORTS WITHIN THE STORM ',
     $ 'BOUNDARIES WILL HAVE THEIR PRESSURE Q.M. FLAGGED ON EVERY ',
     $ 'LEVEL.'//)
      IF(FLBWND) PRINT 1236
 1236 FORMAT(' ===> EDTPRP: ALL DROPWINSONDE REPORTS WITHIN A ',
     $ 'DISTANCE TO STORM CENTER OF THE LARGER OF 111 KM OR 3 X THE ',
     $ 'RADIUS OF MAXIMUM'/15X,'SURFACE WIND WILL HAVE THEIR WIND Q.M.',
     $ ' FLAGGED ON EVERY LEVEL.'//)
      IF(UFLBWND) PRINT 1237
 1237 FORMAT(' ===> EDTPRP: ALL DROPWINSONDE REPORTS WITHIN A ',
     $ 'DISTANCE TO STORM CENTER OF THE LARGER OF 111 KM OR 3 X THE ',
     $ 'RADIUS OF MAXIMUM'/15X,'SURFACE WIND WILL HAVE THEIR WIND Q.M.',
     $ ' UNFLAGGED ON EVERY LEVEL.'//)

      DO J = 1,numstorms
         read (11,22) tcv_center,tcv_storm_id,
     $   tcv_storm_name,tcv_century,tcv_yymmdd,tcv_hhmm,tcv_lat,
     $   tcv_latns,tcv_lon,tcv_lonew,tcv_stdir,tcv_stspd,tcv_pcen,
     $   tcv_penv,tcv_penvrad,tcv_vmax,tcv_vmaxrad,tcv_r341,tcv_r342,
     $   tcv_r343,tcv_r344,tcv_depth,tcv_r501,tcv_r502,tcv_r503,
     $   tcv_r504,tcv_mfh,tcv_flat,tcv_flatns,tcv_flon,tcv_flonew,
     $   tcv_r641,tcv_r642,tcv_r643,tcv_r644

         stmnam(J)=tcv_storm_name
         STMLAT(J)=tcv_lat/10
         if(tcv_lonew .EQ. 'W')then
            STMLON(J)=360.-0.1*tcv_lon
         else
            STMLON(J)=0.1*tcv_lon
         end if
         ROCI(J)=tcv_penvrad*1.0
         RMW(J)=tcv_vmaxrad*1.0
         vmax(J)=tcv_vmax*1.0

C        FOR HWRF ONLY HAVE 1 STORM, NO NEED FOR ARRAYS
C        east/west motion in km/h (1 m/s = 3.6 km/h)
         DXDT=tcv_stspd/10.*SIND(tcv_stdir*1.0)*3.6
C        convert to degrees
         DDXDT=-DXDT/(111.*COSD(STMLAT(J)*1.0))
C        north/south motion in km/h (1 m/s = 3.6 km/h)
         DYDT=tcv_stspd/10.*COSD(tcv_stdir*1.0)*3.6
C        convert to degrees
         DDYDT=DYDT/111.

C Determine the Saffir-Simpson Storm Category based on the maximum
C  reported wind speed (use cateogry 0 for all speeds below hurricane
C  strength)

         vmax(j) = 1.94 * vmax(j)
         if(vmax(j).gt.135.0) then
            icat(j) = 5
         else  if(vmax(j).gt.113.0) then
            icat(j) = 4
         else  if(vmax(j).gt.95.0) then
            icat(j) = 3
         else  if(vmax(j).gt.82.0) then
            icat(j) = 2
         else  if(vmax(j).gt.63.0) then
            icat(j) = 1
         else
            icat(j) = 0
         end if

C  Convert the storm center longitude to west longitude (0-360)
C   (this is assuming storm center longitude comes in as 0-360 E)
C  --------------------------------------------------------------

         RLON(J) = 360. - STMLON(J)
         IF(RLON(J).EQ.360.)  RLON(J) = 0.

         PRINT 1294, stmnam(j),J,STMLAT(J),RLON(J),ROCI(J),
     $    nint(RMW(J)),nint(vmax(j)),icat(j)
 1294 FORMAT(1X,A9,'(storm #',i2,'):: center:',f7.2,'N lat, ',f8.2,
     $ 'W lon; ROCI: ',F7.2,' km',
     $ /24X,'Radius of max. wind speed is ',I4,
     $ ' km; Max. wind speed is ',I3,' knots making this a Cat. ',I1,
     $ ' storm'//)
      ENDDO

      CLOSE(11)

   22 format (a4,1x,a3,1x,a9,1x,i2,i6,1x,i4,1x,i3,a1,1x,i4,a1,
     $        1x,i3,1x,i3,3(1x,i4),1x,i2,1x,i3,4(1x,i4),1x,a1,
     $        4(1x,i4),1x,i2,1x,i3,a1,1x,i4,a1,1x,4(1x,i4))

      CALL CLOSBF(IUNTPN)
      CALL OPENBF(IUNTPN,'IN',IUNTPN)

      CALL OPENBF(IUNTPO,'OUT',IUNTPN)
      CALL UFBQCD(IUNTPO,'SYNDATA',SYNPC)

C  VARIOUS COPYING OPTIONS
C  -----------------------

      ICPYMG = 0
      ICNTOT = 0
      ICNTMM = 0
      ICNTMW = 0
      ICNTTA = 0

   30 CONTINUE

      CALL READMG(IUNTPN,SUBSET,JDATBF,IRET)
      IF(IRET.NE.0) GO TO 20
      IF(.NOT.FLMASS.AND.(.NOT.CFLBWND.OR.SUBSET.NE.'ADPUPA  '))  THEN

C  NO NEED TO DECODE THIS MESSAGE IF FLMASS IS FALSE AND EITHER
C   CFLBWND IS FALSE OR THIS MESSAGE TYPE IS NOT ADPUPA
C  ------------------------------------------------------------

         IF(SUBSET.NE.LAST)  THEN
            IF(LAST.NE.'XXXXXXXX')  THEN
               IF(LAST.NE.'ADPUPA  ')  THEN
                  PRINT 2249, IUNTPN,LAST
 2249 FORMAT(/' ===> EDTPRP: ALL MESSAGES IN INPUT PREPBUFR FILE IN ',
     $ 'UNIT',I3,' WITH TABLE A ENTRY "',A8,'" PROCESSED -- '/15X,
     $ 'SINCE FLMASS IS FALSE, NO INDIVIDUAL REPORTS WERE DECODED OUT ',
     $ 'OF THESE MESSAGES'/)
               ELSE
                  IF(.NOT.CFLBWND)  THEN
                     PRINT 2250, IUNTPN,LAST
 2250 FORMAT(/' ===> EDTPRP: ALL MESSAGES IN INPUT PREPBUFR FILE IN ',
     $ 'UNIT',I3,' WITH TABLE A ENTRY "',A8,'" PROCESSED -- '/15X,
     $ 'SINCE FLMASS AND CFLBWND BOTH FALSE, NO INDIVIDUAL REPORTS',
     $ 'WERE DECODED OUT OF THESE MESSAGES'/)
                  ELSE
                     PRINT 1249, IUNTPN,LAST,ICNTTA
                     CALL CLOSMG(IUNTPO)
                  END IF
               END IF
               PRINT 1240, IUNTPN,SUBSET,JDATBF
            END IF
         END IF
         LAST = SUBSET
         CALL COPYMG(IUNTPN,IUNTPO)
         GO TO 30
      END IF
      IF(SUBSET.NE.LAST)  THEN
         ICPYMG = 0
         IF(LAST.NE.'XXXXXXXX')  PRINT 1249, IUNTPN,LAST,ICNTTA
 1249 FORMAT(/' ===> EDTPRP: ALL MESSAGES IN INPUT PREPBUFR FILE IN ',
     $ 'UNIT',I3,' WITH TABLE A ENTRY "',A8,'" PROCESSED -- '/15X,
     $ 'NUMBER OF INDIVIDUAL REPORTS DECODED OUT OF THESE MESSAGES =',
     $ I5/)
         ICNTTA = 0
         PRINT 1240, IUNTPN,SUBSET,JDATBF
 1240 FORMAT(/' ===> EDTPRP: NEXT MESSAGE IN INPUT PREPBUFR FILE IN ',
     $ 'UNIT',I3,' HAS NEW TABLE A ENTRY OF "',A8,'" -- DATE IS',I11)

C  NO NEED TO DECODE MESSAGES CONTAINING ONLY WIND DATA OR NO PRESSURE
C  -------------------------------------------------------------------

         IF(SUBSET.EQ.'SATWND  '.OR.SUBSET.EQ.'PROFLR  '.OR.
     $      SUBSET.EQ.'VADWND  '.OR.SUBSET.EQ.'GOESND  '.OR.
     $      SUBSET.EQ.'SPSSMI  '.OR.SUBSET.EQ.'ERS1DA  '.OR.
     $      SUBSET.EQ.'GPSIPW  ') THEN
            PRINT 1241, IUNTPO
 1241 FORMAT(/' ===> EDTPRP: MESSAGES WITH THIS TABLE A ENTRY WILL ',
     $ 'NEVER BE MODIFIED - COPY MESSAGES TO OUTPUT PREPBUFR FILE IN ',
     $ 'UNIT',I3/)

cc dak 7/3/05 - icpymg is ALWAYS zero here
cc dak 7/3/05 IF(ICPYMG.EQ.0.AND.LAST.NE.'XXXXXXXX') CALL CLOSMG(IUNTPO)
            if(last.ne.'XXXXXXXX')  call closmg(iuntpo)

            ICPYMG = 1
         ELSE  IF(SUBSET.EQ.'SYNDAT  ') THEN
            PRINT 1291, IUNTPO
 1291 FORMAT(/' ===> EDTPRP: MESSAGES WITH THIS TABLE A ENTRY WILL ',
     $ 'NEVER BE PROCESSED NOR COPIED TO OUTPUT PREPBUFR FILE IN UNIT',
     $ I3/15X,'SINCE THE BOGUS SYNDATA SHOULD NOT BE IN THE INPUT ',
     $ 'PREPBUFR FILE'/)
            ICPYMG = 1
         ELSE
            PRINT 1242, IUNTPO
 1242 FORMAT(/' ===> EDTPRP: MESSAGES WITH THIS TABLE A ENTRY MAY BE ',
     $ 'MODIFIED PRIOR TO THEIR OUTPUT TO PREPBUFR FILE IN UNIT',I3,/
     $ 15X,' - EACH INDIVIDUAL REPORT MUST BE DECODED'/)
         END IF
      END IF
      LAST = SUBSET

      IF(ICPYMG.EQ.1)  THEN

C  "SYNDAT" MESSAGES SHOULD NEVER BE IN INPUT PREPBUFR FILE, BUT IF
C   THEY ARE DON'T COPY THEM TO OUTPUT PREPBUFR FILE
C  ----------------------------------------------------------------

         IF(SUBSET.NE.'SYNDAT  ')  THEN
            CALL COPYMG(IUNTPN,IUNTPO)
         ELSE
            PRINT 2345, IUNTPN,IUNTPO
 2345 FORMAT(/' ===> EDTPRP: MESSAGE WITH TABLE A ENTRY "SYNDAT  " ',
     $ 'ENCOUNTERED IN INPUT PREPBUFR FILE IN UNIT',I3,/15X,'-- DO NOT',
     $ ' COPY TO OUTPUT PREPBUFR FILE IN UNIT',I3/)
         END IF
         GO TO 30
      END IF

      CALL OPENMB(IUNTPO,SUBSET,JDATBF)

   10 CONTINUE

      CALL READSB(IUNTPN,IRET)
      IF(IRET.NE.0) GO TO 30
      ICNTTA = ICNTTA + 1
      ICNTOT = ICNTOT + 1
      CALL UFBCPY(IUNTPN,IUNTPO)
      CALL UFBINT(IUNTPN,HDR_8,6,1,IRET,HDSTR); HDR(2:6)=HDR_8(2:6)
      XOB  = HDR(2)
      YOB  = HDR(3)
      DHR  = HDR(4)
      KX   = HDR(5)
      ITSB = HDR(6)
      KK   = KX/100
      DROP = (KX.EQ.232 .AND. ITSB.EQ.2)
      IF(KK.EQ.2 .AND. .NOT.DROP) GO TO 15 !If this is a wind rpt other
                                           ! than a drop, no chg made
      IF(.NOT.DROP)  THEN
         IF(FLMASS)  THEN
C-----------------------------------------------------------------------
C      Processing to flag pressure q.m. on all lvls for mass reports
C            within any one of the storm(s) lat/lon boundaries
C-----------------------------------------------------------------------
            XOB = 360. - XOB
            IF(XOB.EQ.360.)  XOB = 0.
            DO  J = 1,numstorms
               CALL CHDIST(RLON(J),STMLAT(J),XOB,YOB,RDIST)
CC               IF(RDIST.LE.MAX(ROCI(J),RBLDC).and.icat(J).GT.0)
               IF(RDIST.LE.MAX(ROCI(J),RBLDC))
     $      GO TO 1500
            ENDDO
            GO TO 15
 1500       CONTINUE
            CALL UFBINT(IUNTPN,PES_8,4,255,NLEV,PESTR); PES=PES_8
            NFLAG = 0
            DO J=1,NLEV
               PQM = PES(2,J)
               IF(PQM.LT.10) THEN
                  PES(2,J) = 10
                  PES(3,J) = SYNPC
                  PES(4,J) = 10
                  NFLAG = 1
               ELSE
                  PES(:,J) = BMISS
               ENDIF
            ENDDO
            IF(NFLAG.EQ.1)  THEN
               PRINT 7280
 7280 FORMAT(/' ===> EDTPRP: ALL LEVEL PRESSURE Q. MARKS WERE FLAGGED ',
     $ '(SET TO 10, R. CODE=10) FOR THE FOLLOWING NON-BOGUS REPORT:')
               PRINT 7220, YOB,XOB,STNID,NLEV,HDR(4),KX
 7220 FORMAT(1X,'LATITUDE =',F7.2,'  LONGITUDE=',F7.2,'  STN. ID  =',A8,
     $ '  NO. LVLS =',I3,'  DEL. TIME=',F6.2,'  REP. TYPE=',I3/)
               ICNTMM = ICNTMM + 1
            END IF
            PES_8=PES
            CALL UFBINT(IUNTPO,PES_8,4,NLEV,IRET,PESTR)
         END IF
      ELSE
         IF(CFLBWND) THEN
C-----------------------------------------------------------------------
C  Processing to flag wind q.m. on all lvls for dropwindonde reports
C   which are within R64 
C-----------------------------------------------------------------------

C  Convert reported dropwinsonde longitude to west longitude (0-360)
C   (this is assuming dropwinsonde longitude comes in as 0-360 E)
C  -----------------------------------------------------------------
            XOB = 360. - XOB
            IF(XOB.EQ.360.)  XOB = 0.
            DO  J = 1,numstorms
               JST = J
C              Estimate storm lat/lon at time of observation
               XTOB=NINT((RLON(J)+DHR*DDXDT)*10)/10.
               YTOB=NINT((STMLAT(J)+DHR*DDYDT)*10)/10.
               CALL CHDIST(XTOB,YTOB,XOB,YOB,RDIST)
cppppppppppppppp
cc            print *, '++ distance to storm ',jst,' = ',rdist,', max ',
cc     $       'limit for flagging is ',MAX(MAX(111.,3*RMW(J)),RRADC)
cc            print *, 'RLON(J),STMLAT(J),XOB,YOB: ',
cc     $                RLON(J),STMLAT(J),XOB,YOB
cppppppppppppppp
               IF ( XOB.LE.RLON(J).AND.YOB.GT.STMLAT(J) ) RDIST64 = tcv_r641 
               IF ( XOB.LT.RLON(J).AND.YOB.LE.STMLAT(J) ) RDIST64 = tcv_r642 
               IF ( XOB.GE.RLON(J).AND.YOB.LT.STMLAT(J) ) RDIST64 = tcv_r643 
               IF ( XOB.GT.RLON(J).AND.YOB.GE.STMLAT(J) ) RDIST64 = tcv_r644  

               IF ( RDIST64.GT.0.AND.RDIST64.LT.20 ) RDIST64 = 20

               IF(RDIST.LE.MIN(RDIST64*1.0,111.)) GO TO 1600
    
            ENDDO
            GO TO 15
 1600       CONTINUE
            CALL UFBINT(IUNTPN,WES_8,5,255,NLEV,WESTR); WES=WES_8
            IF(FLBWND)THEN
               NFLAG = 0
               DO J=1,NLEV
                  WQM = WES(3,J)
                  IF(WQM.LE.10) THEN
                     WES(3,J) = 10
                     WES(4,J) = SYNPC
                     WES(5,J) = 99
                     NFLAG = 1
                  ELSE
                     WES(:,J) = BMISS
                  ENDIF
               ENDDO
            END IF
            IF(UFLBWND)THEN
               DO J=1,NLEV
                  WQM = WES(3,J)
                  print *,'mtong WQM,WES(4,J),WES(5,J)=',WQM,WES(4,J),WES(5,J) 
                  IF(WQM.EQ.10) THEN
                    WES(3,J) = 2
                    NFLAG = 2
                  ELSE
                    WES(:,J) = BMISS
                  ENDIF
               END DO
            ENDIF
            IF(NFLAG.NE.0)  THEN
               IF(NFLAG.EQ.1)PRINT 8280
 8280 FORMAT(/' ===> EDTPRP: ALL LEVEL WIND Q. MARKS WERE FLAGGED ',
     $ '(SET TO 10, R. CODE=99) FOR THE FOLLOWING DROPWINSONDE REPORT:')
               IF(NFLAG.EQ.2)PRINT 8281
 8281 FORMAT(/' ===> EDTPRP: ALL LEVEL WIND Q. MARKS WERE UNFLAGGED ',
     $ '(SET TO 2, R. CODE=99) FOR THE FOLLOWING DROPWINSONDE REPORT:')
C               PRINT 8220, YOB,XOB,STNID,NLEV,HDR(4),KX,ITSB,JST,
C     $         NINT(RDIST),MAX(111,3*NINT(RMW(JST)))
               PRINT 8220, YOB,XOB,STNID,NLEV,HDR(4),KX,ITSB,JST,
     $         NINT(RDIST),MAX(20,MIN(RDIST64,111))
 8220 FORMAT(1X,'LATITUDE =',F7.2,'N  LONGITUDE=',F7.2,'W  STN. ID  =',
     $ A8,'  NO. LVLS =',I3,'  DEL. TIME=',F6.2,'  REP. TYPE/SUBTYPE=',
     $ I3,'/',I1/1X,'DIST. FROM STORM',I3,'=',I5,'KM WHICH IS WITHIN ',
     $ 'LIMIT OF ',I5,'KM'/)
               ICNTMW = ICNTMW + 1
            END IF
            WES_8=WES
            CALL UFBINT(IUNTPO,WES_8,5,NLEV,IRET,WESTR)
         END IF
C-----------------------------------------------------------------------
      END IF
   15 CONTINUE
      CALL WRITSB(IUNTPO)
      GOTO 10

   20 CONTINUE
      IF(.NOT.FLMASS.AND.(.NOT.CFLBWND.OR.LAST.NE.'ADPUPA  '))  THEN
         IF(LAST.NE.'ADPUPA  ')  THEN
            PRINT 2249, IUNTPN,LAST
         ELSE
            PRINT 2250, IUNTPN,LAST
         END IF
      ELSE
         PRINT 1249, IUNTPN,LAST,ICNTTA
      END IF
      PRINT 1259, IUNTPN,ICNTOT,ICNTMM,ICNTMW,IUNTPO
 1259 FORMAT(//' ===> EDTPRP: ALL MESSAGES IN INPUT PREPBUFR FILE IN ',
     $ 'UNIT',I3,' PROCESSED -'/16X,'NUMBER OF INDIVIDUAL REPORTS ',
     $ 'DECODED OUT OF ALL MESSAGES =',I6,' -- OF THESE ...'/19X,
     $ '-- TOTAL NUMBER OF MASS REPORTS ACTUALLY MODIFIED =',10X,I7/
     $ 19X,'-- TOTAL NUMBER OF DROPWINSONDE WIND REPORTS ACTUALLY ',
     $ 'MODIFIED = ',I3//15X,'OUTPUT PREPBUFR FILE IN UNIT',I3,
     $ ' CONTAINS ALL ORIGINAL REPORTS PLUS ANY OF THESE MODIFICATIONS'
     $ //)
      CALL CLOSBF(IUNTPN)
      PRINT 799, IUNTPN
  799 FORMAT(/' ===> EDTPRP: INPUT PREPBUFR FILE IN UNIT',I3,
     $ ' SUCCESSFULLY CLOSED'/)
      CALL CLOSMG(IUNTPO)

      CALL CLOSBF(IUNTPO)
      PRINT 899, IUNTPO
      PRINT 809
  809 FORMAT(/' ===> EDTPRP: NO BOGUS DATA APPENDED TO PREPBUFR FILE ',
     $ ' (AS REQUESTED)'/)
  899 FORMAT(/' ===> EDTPRP: OUTPUT PREPBUFR FILE IN UNIT',I3,
     $ ' SUCCESSFULLY CLOSED'/)

      STOP

  891 write(6,*)'***ERROR*** in reading tcvitals'

      STOP
      END

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: CHDIST
C   PRGMMR: D. KEYSER        ORG: NP22       DATE: 2004-02-02
C
C ABSTRACT:  COMPUTES CHORD LENGTH DISTANCE FROM ONE LAT/LON POINT
C   TO ANOTHER LAT/LON POINT USING THE FORMULA:
C     S**2/2 = 1 - COS(Y1-Y2) + COS(Y1)*COS(Y2)*(1-COS(X1-X2)).
C
C PROGRAM HISTORY LOG:
C 1990-11-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 2004-02-02  D. KEYSER  -- CORRECTED SUBROUTINE ARGUMENT MISALIGNMENT
C    (THIS DID NOT APPEAR TO CAUSE PROBLEMS IN OUTPUT BUT COULD CAUSE
C    MEMORY CLOBBERING DOWN THE LINE SOMEWHERE)
C
C USAGE:  CALL CHDIST(X1,Y1,X2,Y2,DIST)
C   INPUT ARGUMENTS:
C     X1         - LONGITUDE (0.-360. W) OF POINT 1
C     Y1         - LATITUDE  (N+,S-)     OF POINT 1
C     X2         - LONGITUDE (0.-360. W) OF POINT 2
C     Y2         - LATITUDE  (N+,S-)     OG POINT 2
C
C   OUTPUT ARGUMENTS:
C     DIST       - CHORD LENGTH DISTANCE BETWEEN POINTS (KM)
C
C REMARKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
      SUBROUTINE CHDIST(X1,Y1,X2,Y2,DIST)

      DATA PI180/.0174532 /,RADE/6371./

      SAVE PI180,RADE

C  COMPUTE THE DISTANCE
C  --------------------

      COSY1 = COS(Y1*PI180)
      COSY2 = COS(Y2*PI180)
      COSDX = COS((X1-X2)*PI180)
      COSDY = COS((Y1-Y2)*PI180)
      S = 1.0-COSDY+COSY1*COSY2*(1.0-COSDX)
      S = SQRT(2.*S)
      IF(S.LE..002) S = 0.
      DIST = S*RADE

      RETURN
      END
