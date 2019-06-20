c
c     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c     *                                                               *
c     *                  copyright (c) 1998 by UCAR                   *
c     *                                                               *
c     *       University Corporation for Atmospheric Research         *
c     *                                                               *
c     *                      all rights reserved                      *
c     *                                                               *
c     *                      SPHEREPACK version 3.2                   *
c     *                                                               *
c     *       A Package of Fortran77 Subroutines and Programs         *
c     *                                                               *
c     *              for Modeling Geophysical Processes               *
c     *                                                               *
c     *                             by                                *
c     *                                                               *
c     *                  John Adams and Paul Swarztrauber             *
c     *                                                               *
c     *                             of                                *
c     *                                                               *
c     *         the National Center for Atmospheric Research          *
c     *                                                               *
c     *                Boulder, Colorado  (80307)  U.S.A.             *
c     *                                                               *
c     *                   which is sponsored by                       *
c     *                                                               *
c     *              the National Science Foundation                  *
c     *                                                               *
c     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c
c
c
c
c ... file slapgc.f
c
c     this file includes documentation and code for
c     subroutine slapgc          i
c
c ... files which must be loaded with slapec.f
c
c     sphcom.f, hrfft.f, shagc.f, shsgc.f
c
c
c
c     subroutine slapgc(nlat,nlon,isym,nt,slap,ids,jds,a,b,
c    +mdab,ndab,wshsgc,lshsgc,work,lwork,ierror)
c
c
c     given the scalar spherical harmonic coefficients a and b, precomputed
c     by subroutine shagc for a scalar field sf, subroutine slapgc computes
c     the laplacian of sf in the scalar array slap.  slap(i,j) is the
c     laplacian of sf at the gaussian colatitude theta(i) (see nlat as
c     an input parameter) and east longitude lambda(j) = (j-1)*2*pi/nlon
c     on the sphere.  i.e.
c
c         slap(i,j) =
c
c                  2                2
c         [1/sint*d (sf(i,j)/dlambda + d(sint*d(sf(i,j))/dtheta)/dtheta]/sint
c
c
c     where sint = sin(theta(i)).  the scalar laplacian in slap has the
c     same symmetry or absence of symmetry about the equator as the scalar
c     field sf.  the input parameters isym,nt,mdab,ndab must have the
c     same values used by shagc to compute a and b for sf. the associated
c     legendre functions are stored rather than recomputed as they are
c     in subroutine slapgc.
c
c     input parameters
c
c     nlat   the number of points in the gaussian colatitude grid on the
c            full sphere. these lie in the interval (0,pi) and are computed
c            in radians in theta(1) <...< theta(nlat) by subroutine gaqd.
c            if nlat is odd the equator will be included as the grid point
c            theta((nlat+1)/2).  if nlat is even the equator will be
c            excluded as a grid point and will lie half way between
c            theta(nlat/2) and theta(nlat/2+1). nlat must be at least 3.
c            note: on the half sphere, the number of grid points in the
c            colatitudinal direction is nlat/2 if nlat is even or
c            (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct longitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than zero. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c     isym   this parameter should have the same value input to subroutine
c            shagc to compute the coefficients a and b for the scalar field
c            sf.  isym is set as follows:
c
c            = 0  no symmetries exist in sf about the equator. scalar
c                 synthesis is used to compute slap on the entire sphere.
c                 i.e., in the array slap(i,j) for i=1,...,nlat and
c                 j=1,...,nlon.
c
c           = 1  sf and slap are antisymmetric about the equator. the
c                synthesis used to compute slap is performed on the
c                northern hemisphere only.  if nlat is odd, slap(i,j) is
c                computed for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if
c                nlat is even, slap(i,j) is computed for i=1,...,nlat/2
c                and j=1,...,nlon.
c
c
c           = 2  sf and slap are symmetric about the equator. the
c                synthesis used to compute slap is performed on the
c                northern hemisphere only.  if nlat is odd, slap(i,j) is
c                computed for i=1,...,(nlat+1)/2 and j=1,...,nlon.  if
c                nlat is even, slap(i,j) is computed for i=1,...,nlat/2
c                and j=1,...,nlon.
c
c
c     nt     the number of analyses.  in the program that calls slapgc
c            the arrays slap,a, and b can be three dimensional in which
c            case multiple synthesis will be performed.  the third index
c            is the synthesis index which assumes the values k=1,...,nt.
c            for a single analysis set nt=1. the description of the
c            remaining parameters is simplified by assuming that nt=1
c            or that all the arrays are two dimensional.
c
c   ids      the first dimension of the array slap as it appears in the
c            program that calls slapgc.  if isym = 0 then ids must be at
c            least nlat.  if isym > 0 and nlat is even then ids must be
c            at least nlat/2. if isym > 0 and nlat is odd then ids must
c            be at least (nlat+1)/2.
c
c   jds      the second dimension of the array slap as it appears in the
c            program that calls slapgc. jds must be at least nlon.
c
c
c   a,b      two or three dimensional arrays (see input parameter nt)
c            that contain scalar spherical harmonic coefficients
c            of the scalar field sf as computed by subroutine shagc.
c     ***    a,b must be computed by shagc prior to calling slapgc.
c
c
c    mdab    the first dimension of the arrays a and b as it appears
c            in the program that calls slapgc.  mdab must be at
c            least min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c    ndab    the second dimension of the arrays a and b as it appears
c            in the program that calls slapgc. ndbc must be at least
c            least nlat.
c
c            mdab,ndab should have the same values input to shagc to
c            compute the coefficients a and b.
c
c
c    wshsgc  an array which must be initialized by subroutine shsgci.
c            once initialized, wshsgc
c            can be used repeatedly by slapgc as long as nlat and nlon
c            remain unchanged.  wshsgc must not be altered between calls
c            of slapgc.
c
c    lshsgc  the dimension of the array wshsgc as it appears in the
c            program that calls slapgc.  let
c
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lshsgc must be at least
c
c               nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls slapgc. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,(nlon+2)/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            if isym = 0 let
c
c               lwkmin = nlat*(2*nt*nlon+max0(6*l2,nlon)+2*nt*l1+1.
c
c            if isym > 0 let
c
c               lwkmin = l2*(2*nt*nlon+max0(6*nlat,nlon))+nlat*(2*nt*l1+1)
c
c
c     then lwork must be greater than or equal to lwkmin (see ierror=10)
c
c     **************************************************************
c
c     output parameters
c
c
c    slap    a two or three dimensional arrays (see input parameter nt) that
c            contain the scalar laplacian of the scalar field sf.  slap(i,j)
c            is the scalar laplacian at the gaussian colatitude theta(i)
c            and longitude lambda(j) = (j-1)*2*pi/nlon for i=1,...,nlat
c            and j=1,...,nlon.
c
c
c  ierror    a parameter which flags errors in input parameters as follows:
c
c            = 0  no errors detected
c
c            = 1  error in the specification of nlat
c
c            = 2  error in the specification of nlon
c
c            = 3  error in the specification of ityp
c
c            = 4  error in the specification of nt
c
c            = 5  error in the specification of ids
c
c            = 6  error in the specification of jds
c
c            = 7  error in the specification of mdbc
c
c            = 8  error in the specification of ndbc
c
c            = 9  error in the specification of lshsgc
c
c            = 10 error in the specification of lwork
c
c
c **********************************************************************
c                                                                              
c     end of documentation for slapgc
c
c **********************************************************************
c
c
      subroutine slapgc(nlat,nlon,isym,nt,slap,ids,jds,a,b,mdab,ndab,
     +                  wshsgc,lshsgc,work,lwork,ierror)
      dimension slap(ids,jds,nt),a(mdab,ndab,nt),b(mdab,ndab,nt)
      dimension wshsgc(lshsgc),work(lwork)
c
c     check input parameters
c
      ierror = 1
      if(nlat .lt. 3) return
      ierror = 2
      if(nlon .lt. 4) return
      ierror = 3
      if (isym.lt.0 .or. isym.gt.2) return
      ierror = 4
      if(nt .lt. 0) return
      ierror = 5
      imid = (nlat+1)/2
      if((isym.eq.0 .and. ids.lt.nlat) .or.
     1   (isym.gt.0 .and. ids.lt.imid)) return
      ierror = 6
      if(jds .lt. nlon) return
      ierror = 7
      mmax = min0(nlat,nlon/2+1)
      if(mdab .lt. mmax) return
      ierror = 8
      if(ndab .lt. nlat) return
      ierror = 9
c
c     set and verify saved work space length
c
c
      l1 = min0(nlat,(nlon+2)/2)
      l2 = (nlat+1)/2
      if (lshsgc .lt. nlat*(2*l2+3*l1-2)+3*l1*(1-l1)/2+nlon+15)return
      ierror = 10
c
c     set and verify unsaved work space length
c
      ls = nlat
      if(isym .gt. 0) ls = imid
      nln = nt*ls*nlon
      mn = mmax*nlat*nt
c     lwmin = nln+ls*nlon+2*mn+nlat
c     if (lwork .lt. lwmin) return
      l2 = (nlat+1)/2
      l1 = min0(nlat,nlon/2+1)
      if (isym .eq. 0) then
	lwkmin = nlat*(2*nt*nlon+max0(6*l2,nlon)+2*nt*l1+1)
      else
	lwkmin = l2*(2*nt*nlon+max0(6*nlat,nlon))+nlat*(2*nt*l1+1)
      end if
      if (lwork .lt. lwkmin) return
      ierror = 0
c
c     set work space pointers
c
      ia = 1
      ib = ia+mn
      ifn = ib+mn
      iwk = ifn+nlat
      lwk = lwork-2*mn-nlat
      call slapgc1(nlat,nlon,isym,nt,slap,ids,jds,a,b,mdab,ndab,
     +work(ia),work(ib),mmax,work(ifn),wshsgc,lshsgc,work(iwk),lwk,
     +ierror)
      return
      end

      subroutine slapgc1(nlat,nlon,isym,nt,slap,ids,jds,a,b,mdab,ndab,
     +                  alap,blap,mmax,fnn,wsave,lsave,wk,lwk,ierror)
      dimension slap(ids,jds,nt),a(mdab,ndab,nt),b(mdab,ndab,nt)
      dimension alap(mmax,nlat,nt),blap(mmax,nlat,nt),fnn(nlat)
      dimension wsave(lsave),wk(lwk)
c
c     set coefficient multiplyers
c
      do 1 n=2,nlat
      fn = float(n-1)
      fnn(n) = fn*(fn+1.)
    1 continue
c
c     compute scalar laplacian coefficients for each vector field
c
      do 2 k=1,nt
      do 3 n=1,nlat
      do 4 m=1,mmax
      alap(m,n,k) = 0.0
      blap(m,n,k) = 0.0
    4 continue
    3 continue
c
c     compute m=0 coefficients
c
      do 5 n=2,nlat
      alap(1,n,k) = -fnn(n)*a(1,n,k)
      blap(1,n,k) = -fnn(n)*b(1,n,k)
    5 continue
c
c     compute m>0 coefficients
c
      do 6 m=2,mmax
      do 7 n=m,nlat
      alap(m,n,k) = -fnn(n)*a(m,n,k)
      blap(m,n,k) = -fnn(n)*b(m,n,k)
    7 continue
    6 continue
    2 continue
c
c     synthesize alap,blap into slap
c
      call shsgc(nlat,nlon,isym,nt,slap,ids,jds,alap,blap,
     +           mmax,nlat,wsave,lsave,wk,lwk,ierror)
      return
      end
