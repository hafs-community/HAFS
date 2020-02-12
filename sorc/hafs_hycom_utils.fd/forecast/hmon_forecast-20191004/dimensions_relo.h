c-----------------------------------------------------------------------------
c --- START OF USER SETABLE, BUT REGION INDEPEDENT, PARAMETERS
c
c --- iqr   = maximum number of tiles in i direction
c --- jqr   = maximum number of tiles in j direction
      integer    iqr,jqr
      parameter (iqr=299,jqr=299)  ! multiple tiles (TYPE=ompi or mpi or shmem)
c
c --- mxthrd= maximum number of OpenMP threads
      integer    mxthrd
      parameter (mxthrd=1)  ! NOMP=0,1
c
c --- mxtrcr= maximum number of tracers
      integer    mxtrcr
      parameter (mxtrcr=99)  !not used to allocate large arrays
c
c ---   END OF USER SETABLE, BUT REGION INDEPEDENT, PARAMETERS
c-----------------------------------------------------------------------------
