c     Parameter file for dataio.f.  
c
c     Separated from the main routine to allow one time maintenance 
c     of the values.  This whole file is included in any subroutine 
c     that needs any of these parameters.

c       numtau  is  the number of synoptic periods ahead for which
c                   tropical cyclone position forecasts are done at a
c                   single forecast time, at this writing, ten.  For
c                   all aids except cliper-120 the taus are 12, 24,
c                   36, 48, and 72.  Only the first five taus are used.
c                   For cliper-120 the taus are 12, 24, 36, 48, 60,  
c                   72, 84, 96, 108, and 120.  All ten taus are used.

      integer   numtau
      parameter (numtau=10)

c        newnumtau is used for the newWriteAidRcd routine.  This 
c                     includes the above mentioned forecast periods
c                     and also include tau 0.  So the taus are:
c                     0, 12, 24, 36, 48, 60, 72, 84, 96, 108 and 120.

      integer   newnumtau
      parameter (newnumtau=11)

c       llw    is   the second dimension of the ltlnwnd array.  This
c                   is for the lat, lon and wind.

      integer   llw
      parameter (llw=3)

