      SUBROUTINE XGETUA(IUNITA,N)
      use setparms
      implicit none

!***BEGIN PROLOGUE  XGETUA
!***DATE WRITTEN   790801   (YYMMDD)
!***REVISION DATE  820801   (YYMMDD)
!***CATEGORY NO.  R3C
!***KEYWORDS  ERROR,XERROR PACKAGE
!***AUTHOR  JONES, R. E., (SNLA)
!***PURPOSE  Returns unit number(s) to which error messages are being
!            sent.
!***DESCRIPTION
!     Abstract
!        XGETUA may be called to determine the unit number or numbers
!        to which error messages are being sent.
!        These unit numbers may have been set by a call to XSETUN,
!        or a call to XSETUA, or may be a default value.
!
!     Description of Parameters
!      --Output--
!        IUNIT - an array of one to five unit numbers, depending
!                on the value of N.  A value of zero refers to the
!                default unit, as defined by the I1MACH machine
!                constant routine.  Only IUNIT(1),...,IUNIT(N) are
!                defined by XGETUA.  The values of IUNIT(N+1),...,
!                IUNIT(5) are not defined (for N .LT. 5) or altered
!                in any way by XGETUA.
!        N     - the number of units to which copies of the
!                error messages are being sent.  N will be in the
!                range from 1 to 5.
!
!     Latest revision ---  19 MAR 1980
!     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
!***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-"
!                 HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,"
!                 1982.
!***ROUTINES CALLED  J4SAVE
!***END PROLOGUE  XGETUA
     ! DIMENSION IUNITA(5)
      integer,dimension(5) :: iunita
      integer :: N,i,index
      integer, external:: J4SAVE
!***FIRST EXECUTABLE STATEMENT  XGETUA
      N = J4SAVE(5,0,.FALSE.)
      do i=1,n
        INDEX = I+4
        IF (I.EQ.1) INDEX = 3
        IUNITA(I) = J4SAVE(INDEX,0,.FALSE.)
      enddo
      RETURN
      END SUBROUTINE XGETUA
