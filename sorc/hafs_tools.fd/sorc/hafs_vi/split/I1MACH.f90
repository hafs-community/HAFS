      INTEGER FUNCTION I1MACH(I)
      implicit none
!subroutine I1MACH(I)
!integer:: I, I1MACH
!***BEGIN PROLOGUE  I1MACH
!***DATE WRITTEN   750101   (YYMMDD)
!***REVISION DATE  910131   (YYMMDD)
!***CATEGORY NO.  R1
!***KEYWORDS  MACHINE CONSTANTS
!***AUTHOR  FOX, P. A., (BELL LABS)
!     HALL, A. D., (BELL LABS)
!     SCHRYER, N. L., (BELL LABS)
!***PURPOSE  Returns integer machine dependent constants
!***DESCRIPTION
!
!     This is the CMLIB version of I1MACH, the integer machine
!     constants subroutine originally developed for the PORT library.
!
!     I1MACH can be used to obtain machine-dependent parameters
!     for the local machine environment.  It is a function
!     subroutine with one (input) argument, and can be called
!     as follows, for example
!
!    K = I1MACH(I)
!
!     where I=1,...,16.  The (output) value of K above is
!     determined by the (input) value of I.  The results for
!     various values of I are discussed below.
!
!  I/O unit numbers.
!    I1MACH( 1) = the standard input unit.
!    I1MACH( 2) = the standard output unit.
!    I1MACH( 3) = the standard punch unit.
!    I1MACH( 4) = the standard error message unit.
!
!  Words.
!    I1MACH( 5) = the number of bits per integer storage unit.
!    I1MACH( 6) = the number of characters per integer storage unit.
!
!  Integers.
!    assume integers are represented in the S-digit, base-A form
!
!   sign ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
!
!   where 0 .LE. X(I) .LT. A for I=0,...,S-1.
!    I1MACH( 7) = A, the base.
!    I1MACH( 8) = S, the number of base-A digits.
!    I1MACH( 9) = A**S - 1, the largest magnitude.
!
!  Floating-Point Numbers.
!    Assume floating-point numbers are represented in the T-digit,
!    base-B form
!   sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
!
!   where 0 .LE. X(I) .LT. B for I=1,...,T,
!   0 .LT. X(1), and EMIN .LE. E .LE. EMAX.
!    I1MACH(10) = B, the base.
!
!  Single-Precision
!    I1MACH(11) = T, the number of base-B digits.
!    I1MACH(12) = EMIN, the smallest exponent E.
!    I1MACH(13) = EMAX, the largest exponent E.
!
!  Double-Precision
!    I1MACH(14) = T, the number of base-B digits.
!    I1MACH(15) = EMIN, the smallest exponent E.
!    I1MACH(16) = EMAX, the largest exponent E.
!
!  To alter this function for a particular environment,
!  the desired set of DATA statements should be activated by
!  removing the C from column 1.  Also, the values of
!  I1MACH(1) - I1MACH(4) should be checked for consistency
!  with the local operating system.
!***REFERENCES  FOX P.A., HALL A.D., SCHRYER N.L.,*FRAMEWORK FOR A
!     PORTABLE LIBRARY*, ACM TRANSACTIONS ON MATHEMATICAL
!     SOFTWARE, VOL. 4, NO. 2, JUNE 1978, PP. 177-188.
!ROUTINES CALLED  (NONE)
!***END PROLOGUE  I1MACH
!
!INTEGER:: I1MACH, I
      integer:: i
      INTEGER:: IMACH(16),OUTPUT
      EQUIVALENCE (IMACH(4),OUTPUT)
!
!     MACHINE CONSTANTS FOR THE IBM RS 6000
!     USING THE 32 BIT INTEGER COMPILER OPTION
!
! === MACHINE = 1 .32-BIT-INTEGER
      DATA IMACH( 1) /    5 /
      DATA IMACH( 2) /    6 /
      DATA IMACH( 3) /    6 /
      DATA IMACH( 4) /    0 /
      DATA IMACH( 5) /   32 /
      DATA IMACH( 6) /    4 /
      DATA IMACH( 7) /    2 /
      DATA IMACH( 8) /   31 /
      DATA IMACH( 9) / 2147483647 /
      DATA IMACH(10) /    2 /
      DATA IMACH(11) /   24 /
      DATA IMACH(12) / -125 /
      DATA IMACH(13) /  128 /
      DATA IMACH(14) /   53 /
      DATA IMACH(15) /-1021 /
      DATA IMACH(16) / 1024 /
!
!***FIRST EXECUTABLE STATEMENT  I1MACH
      IF (I .LT. 1  .OR.  I .GT. 16) THEN
        CALL XERROR ( 'I1MACH -- I OUT OF BOUNDS',25,1,2)
        I1MACH=IMACH(I)
      ENDIF
      RETURN
      END function I1MACH
