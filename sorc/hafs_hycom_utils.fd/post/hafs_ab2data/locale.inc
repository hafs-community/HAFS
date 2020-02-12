C     Include file for Fortran to define locale-specific parameters
C     Robert Grumbine 16 March 1998

      INTEGER SIZEOF_INTEGER, SIZEOF_REAL, SIZEOF_CHARACTER
      INTEGER SIZEOF_COMPLEX
      PARAMETER (SIZEOF_CHARACTER = 1)
      PARAMETER (SIZEOF_INTEGER   = 4)
      PARAMETER (SIZEOF_REAL      = 4)
      PARAMETER (SIZEOF_COMPLEX   = 8)

