c-----------------------------------------------------------------------------
c --- START OF REGION AND TILING SPECIFIC PARAMETERS
c --- See: README.src.newregion for more details.
c
c --- itdm  = total grid dimension in i direction
c --- jtdm  = total grid dimension in j direction
c --- kdm   =       grid dimension in k direction
      integer    itdm,jtdm,kdm
      parameter (itdm=1135,jtdm=633,kdm=41)  ! GLBa0.08
c
c --- iqr   = maximum number of tiles in i direction
c --- jqr   = maximum number of tiles in j direction
      integer    iqr,jqr
      parameter (iqr=99,jqr=99)  ! multiple tiles (TYPE=ompi or mpi or shmem)
c
c --- idm   = maximum single tile grid dimension in i direction
c --- jdm   = maximum single tile grid dimension in j direction
      integer    idm,jdm
*     parameter (idm=itdm,jdm=jtdm)  ! always works if enough memory
*     parameter (idm= 300,jdm= 184)  ! NMPI=379,713,1388
*     parameter (idm= 150,jdm= 207)  ! NMPI=377,758 small memory footprint
*     parameter (idm= 180,jdm= 118)  ! NMPI=758,1001 memory footprint
      parameter (idm= 257,jdm= 143)  ! NMPI=2034 memory footprint
c
c --- mxthrd= maximum number of OpenMP threads
      integer    mxthrd
      parameter (mxthrd=1)  ! NOMP=0,1
c
c --- kkwall= grid dimension in k direction for wall relax arrays
c --- kknest= grid dimension in k direction for nest relax arrays
      integer    kkwall,kknest
      parameter (kkwall=kdm)  ! must be 1 or kdm
      parameter (kknest= 41)  ! must be 1 or kdm
c
c --- kkmy25= grid dimension in k direction for M-Y 2.5 arrays
      integer    kkmy25
      parameter (kkmy25= -1)  ! must be -1 or kdm
c
c --- nlgiss= size of lookup table for GISS
      integer    nlgiss
      parameter (nlgiss=1)  ! must be 1 (no GISS) or 762
c
c --- mxtrcr= maximum number of tracers
      integer    mxtrcr
      parameter (mxtrcr=1)
c
c --- natm  = number of saved atmospheric fields
      integer    natm
      parameter (natm=2)      ! must be 2 (high freq.) or 4 (monthly)
c
c --- mslprf      use msl presssure forcing
      logical, parameter :: mslprf = .true. ! must be true (on) or false (off)
c
c ---   END OF REGION AND TILING SPECIFIC PARAMETERS
c-----------------------------------------------------------------------------
c
c --- how far out the halo is valid (margin<=nbdy)
      integer      margin
      common/edge/ margin
      save  /edge/
c
c-----------------------------------------------------------------------------
