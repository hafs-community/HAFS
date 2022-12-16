      SUBROUTINE XERPRT(MESSG,NMESSG)
      implicit none
!C***BEGIN PROLOGUE  XERPRT
!C***DATE WRITTEN   790801   (YYMMDD)
!C***REVISION DATE  820801   (YYMMDD)
!C***CATEGORY NO.  Z
!C***KEYWORDS  ERROR,XERROR PACKAGE
!C***AUTHOR  JONES, R. E., (SNLA)
!C***PURPOSE  Prints error messages.
!C***DESCRIPTION
!C     Abstract
!C        Print the Hollerith message in MESSG, of length NMESSG,
!C        on each file indicated by XGETUA.
!C     Latest revision ---  19 MAR 1980
!C***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-"
!C                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,"
!C                 1982.
!C***ROUTINES CALLED  I1MACH,S88FMT,XGETUA
!C***END PROLOGUE  XERPRT
      INTEGER LUN(5)
      CHARACTER*(*) MESSG
      integer lenmes,ichar,iunit,kunit,nunit,last,nmessg
      integer, external :: I1MACH
!C     OBTAIN UNIT NUMBERS AND WRITE LINE TO EACH UNIT
!C***FIRST EXECUTABLE STATEMENT  XERPRT
      CALL XGETUA(LUN,NUNIT)
      LENMES = LEN(MESSG)
      do kunit=1,nunit
        IUNIT = LUN(KUNIT)
        IF (IUNIT.EQ.0) IUNIT = I1MACH(4)
        do ichar=1,lenmes,72
          LAST = MIN0(ICHAR+71 , LENMES)
          WRITE (IUNIT,'(1X,A)') MESSG(ICHAR:LAST)
        enddo
      enddo
      RETURN
      END
