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
c ... file idvtgc.f
c
c     this file includes documentation and code for
c     subroutine idvtgc         i
c
c ... files which must be loaded with idvtgc.f
c
c     sphcom.f, hrfft.f, vhsgc.f,shagc.f, gaqd.f
c
c
c     subroutine idvtgc(nlat,nlon,isym,nt,v,w,idvw,jdvw,ad,bd,av,bv,
c    +mdab,ndab,wvhsgc,lvhsgc,work,lwork,pertbd,pertbv,ierror)
c
c     given the scalar spherical harmonic coefficients ad,bd precomputed
c     by subroutine shagc for the scalar field divg and coefficients av,bv
c     precomputed by subroutine shagc for the scalar field vort, subroutine
c     idvtgc computes a vector field (v,w) whose divergence is divg - pertbd
c     and whose vorticity is vort - pertbv.  w the is east longitude component
c     and v is the colatitudinal component of the velocity.  if nt=1 (see nt
c     below) pertrbd and pertbv are constants which must be subtracted from
c     divg and vort for (v,w) to exist (see the description of pertbd and
c     pertrbv below).  usually pertbd and pertbv are zero or small relative
c     to divg and vort.  w(i,j) and v(i,j) are the velocity components at
c     gaussian colatitude theta(i) (see nlat as input argument) and longitude
c     lambda(j) = (j-1)*2*pi/nlon
c
c     the
c
c            divergence(v(i,j),w(i,j))
c
c         =  [d(sint*v)/dtheta + dw/dlambda]/sint
c
c         =  divg(i,j) - pertbd
c
c     and
c
c            vorticity(v(i,j),w(i,j))
c
c         =  [-dv/dlambda + d(sint*w)/dtheta]/sint
c
c         =  vort(i,j) - pertbv
c
c     where
c
c            sint = cos(theta(i)).
c
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
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than 3. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c
c     isym   isym determines whether (v,w) are computed on the full or half
c            sphere as follows:
c
c      = 0
c            divg,vort are neither pairwise symmetric/antisymmetric nor
c            antisymmetric/symmetric about the equator as described for
c            isym = 1 or isym = 2  below.  in this case, the vector field
c            (v,w) is computed on the entire sphere.  i.e., in the arrays
c            w(i,j) and v(i,j) i=1,...,nlat and j=1,...,nlon.
c
c      = 1
c
c            divg is antisymmetric and vort is symmetric about the equator.
c            in this case w is antisymmetric and v is symmetric about the
c            equator.  w and v are computed on the northern hemisphere only.
c            if nlat is odd they are computed for i=1,...,(nlat+1)/2
c            and j=1,...,nlon.  if nlat is even they are computed for
c            i=1,...,nlat/2 and j=1,...,nlon.
c
c      = 2
c
c            divg is symmetric and vort is antisymmetric about the equator.
c            in this case w is symmetric and v is antisymmetric about the
c            equator.  w and v are computed on the northern hemisphere only.
c            if nlat is odd they are computed for i=1,...,(nlat+1)/2
c            and j=1,...,nlon.  if nlat is even they are computed for
c            i=1,...,nlat/2 and j=1,...,nlon.
c
c
c     nt     in the program that calls idvtgc, nt is the number of scalar
c            and vector fields.  some computational efficiency is obtained
c            for multiple fields.  the arrays ad,bd,av,bv,u, and v can be
c            three dimensional and pertbd,pertbv can be one dimensional
c            corresponding to indexed multiple arrays divg, vort.  in this
c            case, multiple synthesis will be performed to compute each
c            vector field.  the third index for ad,bd,av,bv,v,w and first
c            pertrbd,pertbv is the synthesis index which assumes the values
c            k=1,...,nt.  for a single synthesis set nt=1. the description of
c            remaining parameters is simplified by assuming that nt=1 or that
c            ad,bd,av,bv,v,w are two dimensional and pertbd,pertbv are
c            constants.
c
c     idvw   the first dimension of the arrays v,w as it appears in
c            the program that calls idvtgc. if isym = 0 then idvw
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then idvw must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays v,w as it appears in
c            the program that calls idvtgc. jdvw must be at least nlon.
c
c     ad,bd  two or three dimensional arrays (see input parameter nt)
c            that contain scalar spherical harmonic coefficients
c            of the divergence array divg as computed by subroutine shagc.
c
c     av,bv  two or three dimensional arrays (see input parameter nt)
c            that contain scalar spherical harmonic coefficients
c            of the vorticity array vort as computed by subroutine shagc.
c     ***    ad,bd,av,bv must be computed by shagc prior to calling idvtgc.
c
c     mdab   the first dimension of the arrays ad,bd,av,bv as it appears
c            in the program that calls idvtgc (and shagc). mdab must be at
c            least min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays ad,bd,av,bv as it appears in
c            the program that calls idvtgc (and shagc). ndab must be at
c            least nlat.
c
c  wvhsgc    an array which must be initialized by subroutine vhsgci.
c            wvhsgc can be used repeatedly by idvtgc as long as nlon
c            and nlat remain unchanged.  wvhsgc must not be altered
c            between calls of idvtgc.
c
c
c  lvhsgc    the dimension of the array wvhsgc as it appears in the
c            program that calls idvtgc. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhsgc must be at least
c
c               4*nlat*l2+3*max0(l1-2,0)*(2*nlat-l1-1)+nlon+15
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls idvtgc. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,nlon/2)       if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2)   if nlon is odd
c
c
c            if isym = 0 then lwork must be at least
c
c              l2*(2*nt*nlon+max0(6*nlat,nlon))+nlat*(4*nt*l1+1)
c
c            if isym = 1 or 2 then lwork must be at least
c
c              nlat*(2*nt*nlon+max0(6*l2,nlon)+4*nt*l1+1)
c
c
c     **************************************************************
c
c     output parameters
c
c
c     v,w   two or three dimensional arrays (see input parameter nt) that
c           contain a vector field whose divergence is divg - pertbd and
c           whose vorticity is vort - pertbv.  w(i,j) is the east longitude
c           component and v(i,j) is the colatitudinal component of velocity
c           at the colatitude theta(i) = (i-1)*pi/(nlat-1) and longitude
c           lambda(j) = (j-1)*2*pi/nlon for i=1,...,nlat and j=1,...,nlon.
c
c   pertbd  a nt dimensional array (see input parameter nt and assume nt=1
c           for the description that follows).  divg - pertbd is a scalar
c           field which can be the divergence of a vector field (v,w).
c           pertbd is related to the scalar harmonic coefficients ad,bd
c           of divg (computed by shagc) by the formula
c
c                pertbd = ad(1,1)/(2.*sqrt(2.))
c
c           an unperturbed divg can be the divergence of a vector field
c           only if ad(1,1) is zero.  if ad(1,1) is nonzero (flagged by
c           pertbd nonzero) then subtracting pertbd from divg yields a
c           scalar field for which ad(1,1) is zero.  usually pertbd is
c           zero or small relative to divg.
c
c   pertbv a nt dimensional array (see input parameter nt and assume nt=1
c           for the description that follows).  vort - pertbv is a scalar
c           field which can be the vorticity of a vector field (v,w).
c           pertbv is related to the scalar harmonic coefficients av,bv
c           of vort (computed by shagc) by the formula
c
c                pertbv = av(1,1)/(2.*sqrt(2.))
c
c           an unperturbed vort can be the vorticity of a vector field
c           only if av(1,1) is zero.  if av(1,1) is nonzero (flagged by
c           pertbv nonzero) then subtracting pertbv from vort yields a
c           scalar field for which av(1,1) is zero.  usually pertbv is
c           zero or small relative to vort.
c
c    ierror = 0  no errors
c           = 1  error in the specification of nlat
c           = 2  error in the specification of nlon
c           = 3  error in the specification of isym
c           = 4  error in the specification of nt
c           = 5  error in the specification of idvw
c           = 6  error in the specification of jdvw
c           = 7  error in the specification of mdab
c           = 8  error in the specification of ndab
c           = 9  error in the specification of lvhsgc
c           = 10 error in the specification of lwork
c **********************************************************************
c                                                                              
c   
      subroutine idvtgc(nlat,nlon,isym,nt,v,w,idvw,jdvw,ad,bd,av,bv,
     +mdab,ndab,wvhsgc,lvhsgc,work,lwork,pertbd,pertbv,ierror)
      dimension w(idvw,jdvw,nt),v(idvw,jdvw,nt),pertbd(nt),pertbv(nt)
      dimension ad(mdab,ndab,nt),bd(mdab,ndab,nt)
      dimension av(mdab,ndab,nt),bv(mdab,ndab,nt)
      dimension wvhsgc(lvhsgc),work(lwork)
c
c     check input parameters
c
      ierror = 1
      if(nlat .lt. 3) return
      ierror = 2
      if(nlon .lt. 4) return
      ierror = 3
      if(isym.lt.0 .or. isym.gt.2) return
      ierror = 4
      if(nt .lt. 0) return
      ierror = 5
      imid = (nlat+1)/2
      if((isym.eq.0 .and. idvw.lt.nlat) .or.
     +   (isym.ne.0 .and. idvw.lt.imid)) return
      ierror = 6
      if(jdvw .lt. nlon) return
      ierror = 7
      mmax = min0(nlat,(nlon+1)/2)
      if(mdab .lt. min0(nlat,(nlon+2)/2)) return
      ierror = 8
      if(ndab .lt. nlat) return
      ierror = 9
      lzz1 = 2*nlat*imid
      labc = 3*(max0(mmax-2,0)*(nlat+nlat-mmax-1))/2
      if(lvhsgc .lt. 2*(lzz1+labc)+nlon+15) return
      ierror = 10
c
c     verify unsaved work space length
c
      mn = mmax*nlat*nt
      if(isym.ne.0  .and. lwork .lt.
     +nlat*(2*nt*nlon+max0(6*imid,nlon))+4*mn+nlat) return
      if(isym.eq.0  .and. lwork .lt.
     +imid*(2*nt*nlon+max0(6*nlat,nlon))+4*mn+nlat) return
      ierror = 0
c
c     set work space pointers
c
      ibr = 1
      ibi = ibr+mn
      icr = ibi+mn
      ici = icr + mn
      is = ici + mn
      iwk = is + nlat
      liwk = lwork-4*mn-nlat
      call idvtgc1(nlat,nlon,isym,nt,v,w,idvw,jdvw,work(ibr),
     +work(ibi),work(icr),work(ici),mmax,work(is),mdab,ndab,ad,bd,
     +av,bv,wvhsgc,lvhsgc,work(iwk),liwk,pertbd,pertbv,ierror)
      return
      end

      subroutine idvtgc1(nlat,nlon,isym,nt,v,w,idvw,jdvw,br,bi,
     +cr,ci,mmax,sqnn,mdab,ndab,ad,bd,av,bv,wvhsgc,lvhsgc,wk,lwk,
     +pertbd,pertbv,ierror)
      dimension w(idvw,jdvw,nt),v(idvw,jdvw,nt)
      dimension br(mmax,nlat,nt),bi(mmax,nlat,nt),sqnn(nlat)
      dimension cr(mmax,nlat,nt),ci(mmax,nlat,nt)
      dimension ad(mdab,ndab,nt),bd(mdab,ndab,nt)
      dimension av(mdab,ndab,nt),bv(mdab,ndab,nt)
      dimension wvhsgc(lvhsgc),wk(lwk)
      dimension pertbd(nt),pertbv(nt)
c
c     preset coefficient multiplyers in vector
c
      do 1 n=2,nlat
      fn = float(n-1)
      sqnn(n) = sqrt(fn*(fn+1.))
    1 continue
c
c     compute multiple vector fields coefficients
c
      do 2 k=1,nt
c
c     set divergence,vorticity perturbation constants
c
      pertbd(k) = ad(1,1,k)/(2.*sqrt(2.))
      pertbv(k) = av(1,1,k)/(2.*sqrt(2.))
c
c     preset br,bi,cr,ci to 0.0
c
      do 3 n=1,nlat
      do 4 m=1,mmax
      br(m,n,k) = 0.0
      bi(m,n,k) = 0.0
      cr(m,n,k) = 0.0
      ci(m,n,k) = 0.0
    4 continue
    3 continue
c
c     compute m=0 coefficients
c
      do 5 n=2,nlat
      br(1,n,k) = -ad(1,n,k)/sqnn(n)
      bi(1,n,k) = -bd(1,n,k)/sqnn(n)
      cr(1,n,k) = av(1,n,k)/sqnn(n)
      ci(1,n,k) = bv(1,n,k)/sqnn(n)
    5 continue
c
c     compute m>0 coefficients
c
      do 6 m=2,mmax
      do 7 n=m,nlat
      br(m,n,k) = -ad(m,n,k)/sqnn(n)
      bi(m,n,k) = -bd(m,n,k)/sqnn(n)
      cr(m,n,k) = av(m,n,k)/sqnn(n)
      ci(m,n,k) = bv(m,n,k)/sqnn(n)
    7 continue
    6 continue
    2 continue
c
c     set ityp for vector synthesis without assuming div=0 or curl=0
c
      if (isym.eq.0) then
      ityp = 0
      else if (isym.eq.1) then
      ityp = 3
      else if (isym.eq.2) then
      ityp = 6
      end if
c
c     sythesize br,bi,cr,ci into the vector field (v,w)
c
      call vhsgc(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
     +           mmax,nlat,wvhsgc,lvhsgc,wk,lwk,ierror)
      return
      end
