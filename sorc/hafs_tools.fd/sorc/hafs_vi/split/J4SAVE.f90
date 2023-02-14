      FUNCTION J4SAVE(IWHICH,IVALUE,ISET)
!***BEGIN PROLOGUE  J4SAVE
!***REFER TO  XERROR
!     Abstract
!  J4SAVE saves and recalls several global variables needed
!  by the library error handling routines.
!
!     Description of Parameters
!--Input--
!  IWHICH - Index of item desired.
!    = 1 Refers to current error number.
!    = 2 Refers to current error control flag.
!     = 3 Refers to current unit number to which error
!  messages are to be sent.  (0 means use standard.)
!     = 4 Refers to the maximum number of times any
!   message is to be printed (as set by XERMAX).
!     = 5 Refers to the total number of units to which
!   each error message is to be written.
!     = 6 Refers to the 2nd unit for error messages
!     = 7 Refers to the 3rd unit for error messages
!     = 8 Refers to the 4th unit for error messages
!     = 9 Refers to the 5th unit for error messages
!  IVALUE - The value to be set for the IWHICH-th parameter,
!     if ISET is .TRUE. .
!  ISET   - If ISET=.TRUE., the IWHICH-th parameter will BE
!     given the value, IVALUE.  If ISET=.FALSE., the
!     IWHICH-th parameter will be unchanged, and IVALUE
!     is a dummy parameter.
!--Output--
!  The (old) value of the IWHICH-th parameter will be returned
!  in the function value, J4SAVE.
!
!     Written by Ron Jones, with SLATEC Common Math Library Subcommittee
!    Adapted from Bell Laboratories PORT Library Error Handler
!     Latest revision ---  23 MAY 1979
!***REFERENCES  JONES R.E., KAHANER D.K., "XERROR, THE SLATEC ERROR-"
!     HANDLING PACKAGE", SAND82-0800, SANDIA LABORATORIES,"
!     1982.
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  J4SAVE
      implicit none

      integer IPARAM(9)
      logical iset
      integer ivalue
      integer iwhich
      integer j4save

      SAVE IPARAM
      DATA IPARAM(1),IPARAM(2),IPARAM(3),IPARAM(4)/0,2,0,10/
      DATA IPARAM(5)/1/
      DATA IPARAM(6),IPARAM(7),IPARAM(8),IPARAM(9)/0,0,0,0/
      !***FIRST EXECUTABLE STATEMENT  J4SAVE
      J4SAVE = IPARAM(IWHICH)
      IF (ISET) THEN
        IPARAM(IWHICH) = IVALUE
      ENDIF
      RETURN
      END
