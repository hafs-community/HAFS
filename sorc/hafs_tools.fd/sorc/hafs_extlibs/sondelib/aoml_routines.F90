
!     -----------------------------------------------------
      FUNCTION GSNDFALL2(PR,TE,BAD, sfcp,mbps)

!     Function returns theoretical fall rate of GPS sonde.
!     If TE is missing, mean West Indies hurricane season
!     TE is used.  Pressure must be valid to compute fall.
! 
!     4/18/01:  Changed mass from 390 to 430 g and changed

!               fudge factor from 1.15 to 1.00.
!     12/20/17: Added logical variable 'mbps' to function
!               declaration; the default return units are
!               now meters per second but can be changed 
!               to millibars per second if mbps = .true. 

!     -----------------------------------------------------
!
        LOGICAL mbps
        PARAMETER (NSTD = 20)
        PARAMETER (NM = 13) 

        DIMENSION PSTD(20), TSTD(20), HMHPA(13), HGTM(13)

        DATA PSTD/100.,150.,200.,250.,300.,350.,400.,450.,500.,550., &
             & 600.,650.,700.,750.,800.,850.,900.,950.,1000.,1050./
        DATA TSTD/-73.5,-67.6,-55.2,-43.3,-33.2,-24.8,-17.7,-11.9, &
             & -6.9,-2.5,1.4,5.1,8.6,11.8,14.6,17.3,19.8,23.0, &
             & 26.0,26.3/
        DATA HMHPA/50.0,100.0,150.0,200.0,250.0,300.0,400.0,500.0,600.0, &
             & 700.0,850.0,925.0,1025.0/
        DATA HGTM/20732.,16593.,14197.,12412.,10948.,9693.,7604., &
             & 5895.,4450.,3188.,1551.,821.,0./
   

!     Set constants
!     -------------
        SMASS = 0.430                 !   MASS OF SONDE (KG) 
        CD = .63                      !   DRAG COEFF
        AREA = 0.09                   !   AREA OF PARACHUTE (30 CM x 30 CM)
        GRAV = 9.8                    !   GRAVITY
        ADJFACTOR = 1.00              !   FINAL ADJUSTMENT FACTOR
        IF ((sfcp.LT.1050.).AND.(sfcp.GT.925.)) hmhpa(nm) = sfcp  ! pal

        IF (PR.LE.0.)  THEN
           GSNDFALL2 = BAD
           RETURN
        ENDIF

        IF (TE.GT.BAD) THEN
           T = TE
        ELSE
           CALL POLATE_AOML(NSTD,PSTD,TSTD,PR,T,M,BAD)
           IF (T.EQ.BAD)THEN
              GSNDFALL2 = BAD
              RETURN
           ENDIF
        ENDIF

!     Compute density
!     ---------------
        RHO = (PR * 100.)/(287.*(T+273.16))

!     Compute fall speed - theoretical
!     --------------------------------
        GSNDFALL2 = -((2.0*SMASS*GRAV)/(CD*AREA*RHO))**0.5

!     Add empirical fudge factor
!     --------------------------
        GSNDFALL2 = GSNDFALL2*ADJFACTOR

!     Convert m/s to mb/s, added fudge. pal

!      CALL POLATE(nm, hmhpa, hgtm, pr, hgt, m, bad)

! KSellwood conversion should be pressure change (not pressure) /  height
! Bottom up calculation
! If surface then use top - bottom fallspeed

!      IF(HGT.GT.0.AND.(SFCP - PR).GT.0)THEN
!      GSNDFALL2 = GSNDFALL2 * (PR - SFCP) / HGT  ! m/s * mb/m = mb/s
!      ELSE
!      GSNDFALL2 = BAD
!      ENDIF 
! SDA - convert from using polate to calculate to using the hydrostatic conversion
!

! HRW - The default is to return values with units meters per second;
! provide option to return in millibars per second.

        if(mbps) then

           GSNDFALL2 = -RHO * GRAV * GSNDFALL2

        end if ! if(mbps)

        RETURN
      END FUNCTION GSNDFALL2

!----

!    ------------------------------------------- 
      SUBROUTINE POLATE_AOML(N,X,Y,XIN,YOUT,M,BAD)
!    ------------------------------------------- 

        DIMENSION X(N),Y(N)
        LOGICAL INC


!     Check for exact match 
!    --------------------- 
        IF (N.LE.0) THEN
           YOUT = BAD
           M = -1
           RETURN
        ENDIF

        DO 15 L=1,N
           IF (X(L).EQ.BAD) EXIT
           IF (XIN.EQ.X(L)) THEN
              YOUT=Y(L)
              M = L
              RETURN
           ENDIF
         
15      CONTINUE

        IF (N.EQ.1) THEN
           YOUT = BAD
           M = -1
           RETURN
        ENDIF  ! GOTO 500


!     Determine if X increases or decreases 
!     ------------------------------------- 
        XMAX = -9999999.
        XMIN = 9999999.

        DO 100 L = 1,N
           IF (X(L).NE.BAD .AND. X(L).GT.XMAX) THEN
              XMAX = X(L)
              LMAX = L
           ENDIF
           IF (X(L).NE.BAD .AND. X(L).LT.XMIN) THEN
              XMIN = X(L)
              LMIN = L
           ENDIF
100     CONTINUE

        IF (XMIN.GT.XMAX.OR.XMIN.EQ.XMAX.OR. &
             XIN.LT.XMIN.OR.XIN.GT.XMAX) THEN
           YOUT = BAD
           RETURN
        ENDIF

        INC = .FALSE.
        IF (LMAX.GT.LMIN) INC = .TRUE.
 
 
!     Interpolate
!     -----------
        DO 10 L=1,N-1

           IF (X(L).EQ.BAD .OR. X(L+1).EQ.BAD) THEN
              YOUT = BAD
              M = -1
              RETURN
           ENDIF
           IF (XIN.GT.X(L) .AND. XIN.LT.X(L+1)) EXIT
           IF (XIN.LT.X(L) .AND. XIN.GT.X(L+1)) EXIT
10      CONTINUE          


50      M=L+1 
        DUM=((X(M)-XIN)*(Y(M)-Y(L)))/(X(M)-X(L))
        YOUT=Y(M)-DUM

        IF(Y(M).EQ.BAD .OR. Y(L).EQ.BAD) YOUT = BAD
        IF (.NOT. INC) M = L
        RETURN


      END SUBROUTINE POLATE_AOML

!----

      SUBROUTINE SPGINFO(line,logtime,loglat,loglon,string)

        character*70 line, line2
        character*1  clon, clat
        character*3 string
        real loglat, loglon
        integer logtime
        integer ilon1, ilon2, ilat1, ilat2
        integer chkidx
 
        ispgtime = -999
        chkidx = index(line,string)
        if(chkidx .ne. 0) then
           read(line(chkidx+4:chkidx+14),'(2I2, A1, I3, I2, A1)') &
        &      ilat1, ilat2, clat, ilon1, ilon2, clon
           loglat = FLOAT(ilat1) + FLOAT(ilat2) / 100.
           IF (clat.eq.'S') loglat = -rloglat
           loglon = FLOAT(ilon1) + FLOAT(ilon2) / 100.
           IF (clon.EQ.'E') loglon = -loglon
           if(string .eq. 'SPL') then
               read(line(chkidx+16:chkidx+21),'(I4)') logtime
               logtime = logtime*100
           else
               READ(line(chkidx+16:chkidx+21), '(I6)') logtime
           endif
        endif
      
    END SUBROUTINE SPGINFO

!--------------------------------
    SUBROUTINE NRMIN(TIME,ROUND)
!--------------------------------

!Value of round determines whether to round up to next minute 

        INTEGER:: ISECS, IMIN, ROUND
        REAL::TIME

        ISECS = MOD(INT(TIME), 3600)
        IMIN = ISECS/60
        ISECS= MOD(isecs, 60)

        ROUND = 0

        IF(ISECS.GT.30) THEN
            IF(IMIN.GE.59)THEN
               ROUND = 41
            ELSE
            ROUND = 1
            ENDIF
        ELSE
           ROUND = 0
        ENDIF

        RETURN
      END SUBROUTINE NRMIN
