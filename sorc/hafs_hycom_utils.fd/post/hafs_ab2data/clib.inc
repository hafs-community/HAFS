C     Include file to define variables for Fortran to C interface(s)
C     Robert Grumbine 16 March 1998
C     NOSEEK added 25 March 1998

C     IO-related (bacio.c, banio.c) :
      INTEGER BAOPEN_RONLY, BAOPEN_WONLY, BAOPEN_RW
      INTEGER BACLOSE, BAREAD, BAWRITE, NOSEEK
      INTEGER BAOPEN_WONLY_TRUNC, BAOPEN_WONLY_APPEND
      PARAMETER (BAOPEN_RONLY =  1)
      PARAMETER (BAOPEN_WONLY =  2)
      PARAMETER (BAOPEN_RW    =  4)
      PARAMETER (BACLOSE      =  8)
      PARAMETER (BAREAD       = 16)
      PARAMETER (BAWRITE      = 32)
      PARAMETER (NOSEEK       = 64)
      PARAMETER (BAOPEN_WONLY_TRUNC       = 128)
      PARAMETER (BAOPEN_WONLY_APPEND      = 256)
      
