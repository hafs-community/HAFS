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
c ... file ivrtgc.f
c
c     this file includes documentation and code for
c     subroutine ivrtgc         i
c
c ... files which must be loaded with ivrtgc.f
c
c     sphcom.f, hrfft.f, vhsgc.f,shagc.f, gaqd.f
c
c
c     subroutine ivrtgc(nlat,nlon,isym,nt,v,w,idvw,jdvw,a,b,mdab,ndab,
c    +                  wvhsgc,lvhsgc,work,lwork,pertrb,ierror)
c
c     given the scalar spherical harmonic coefficients a and b, precomputed
c     by subroutine shagc for a scalar array vt, subroutine ivrtgc computes
c     a divergence free vector field (v,w) whose vorticity is vt - pertrb.
c     w is the east longitude component and v is the colatitudinal component.
c     pertrb is a constant which must be subtracted from vt for (v,w) to
c     exist (see the description of pertrb below).  usually pertrb is zero
c     or small relative to vort.  the divergence of (v,w), as computed by
c     ivrtgc, is the zero scalar field.  v(i,j) and w(i,j) are the
c     colatitudinal and east longitude velocity components at gaussian
c     colatitude theta(i) (see nlat as input parameter) and longitude
c     lambda(j) = (j-1)*2*pi/nlon.  the
c
c            vorticity(v(i,j),w(i,j))
c
c         =  [-dv/dlambda + d(sint*w)/dtheta]/sint
c
c         =  vort(i,j) - pertrb
c
c     and
c
c            divergence(v(i,j),w(i,j))
c
c         =  [d(sint*v)/dtheta + dw/dlambda]/sint
c
c         =  0.0
c
c     where sint = sin(theta(i)).  required associated legendre polynomials
c     are recomputed rather than stored as they are in subroutine ivrtgs.
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
c     isym   this has the same value as the isym that was input to
c            subroutine shagc to compute the arrays a and b.  isym
c            determines whether (v,w) are computed on the full or half
c            sphere as follows:
c
c      = 0
c            vt is not symmetric about the equator. in this case
c            the vector field (v,w) is computed on the entire sphere.
c            i.e., in the arrays  v(i,j),w(i,j) for i=1,...,nlat and
c            j=1,...,nlon.
c
c      = 1
c
c            vt is symmetric about the equator. in this case w is
c            antiymmetric and v is symmetric about the equator. v
c            and w are computed on the northern hemisphere only.  i.e.,
c            if nlat is odd they are computed for i=1,...,(nlat+1)/2
c            and j=1,...,nlon.  if nlat is even they are computed for
c            i=1,...,nlat/2 and j=1,...,nlon.
c
c       = 2
c
c            vt is antisymmetric about the equator. in this case w is
c            symmetric and v is antisymmetric about the equator. w
c            and v are computed on the northern hemisphere only.  i.e.,
c            if nlat is odd they are computed for i=1,...,(nlat+1)/2
c            and j=1,...,nlon.  if nlat is even they are computed for
c            i=1,...,nlat/2 and j=1,...,nlon.
c
c
c     nt     in the program that calls ivrtgc, nt is the number of vorticity
c            and vector fields.  some computational efficiency is obtained
c            for multiple fields.  the arrays a,b,v, and w can be three
c            dimensional and pertrb can be one dimensional corresponding
c            to an indexed multiple array vort.  in this case, multiple vector
c            synthesis will be performed to compute each vector field.  the
c            third index for a,b,v,w and first for pertrb is the synthesis
c            index which assumes the values k=1,...,nt.  for a single
c            synthesis set nt=1. the description of the remaining parameters
c            is simplified by assuming that nt=1 or that a,b,v,w are two
c            dimensional and pertrb is a constant.
c
c     idvw   the first dimension of the arrays v,w as it appears in
c            the program that calls ivrtgc. if isym = 0 then idvw
c            must be at least nlat.  if isym = 1 or 2 and nlat is
c            even then idvw must be at least nlat/2. if isym = 1 or 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays v,w as it appears in
c            the program that calls ivrtgc. jdvw must be at least nlon.
c
c     a,b    two or three dimensional arrays (see input parameter nt)
c            that contain scalar spherical harmonic coefficients
c            of the vorticity array vt as computed by subroutine shagc.
c     ***    a,b must be computed by shagc prior to calling ivrtgc.
c
c     mdab   the first dimension of the arrays a and b as it appears in
c            the program that calls ivrtgcs (and shagc). mdab must be at
c            least min0(nlat,(nlon+2)/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays a and b as it appears in
c            the program that calls ivrtgc (and shagc). ndab must be at
c            least nlat.
c
c
c  wvhsgc    an array which must be initialized by subroutine vhsgci.
c            once initialized
c            wvhsgc can be used repeatedly by ivrtgc as long as nlon
c            and nlat remain unchanged.  wvhsgs must not be altered
c            between calls of ivrtgc.
c
c
c  lvhsgc    the dimension of the array wvhsgc as it appears in the
c            program that calls ivrtgc. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhsgc be at least
c
c               4*nlat*l2+3*max0(l1-2,0)*(2*nlat-l1-1)+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls ivrtgc. define
c
c               l2 = nlat/2                    if nlat is even or
c               l2 = (nlat+1)/2                if nlat is odd
c               l1 = min0(nlat,nlon/2)         if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2)     if nlon is odd
c
c            if isym = 0 then lwork must be at least
c
c               nlat*(2*nt*nlon+max0(6*l2,nlon) + 2*nt*l1 + 1)
c
c            if isym = 1 or 2 then lwork must be at least
c
c               l2*(2*nt*nlon+max0(6*nlat,nlon)) + nlat*(2*nt*l1+1)
c
c     **************************************************************
c
c     output parameters
c
c
c     v,w   two or three dimensional arrays (see input parameter nt) that
c           contain a divergence free vector field whose vorticity is
c           vt - pertrb at the gaussian colatitude point theta(i)
c           and longitude point lambda(j)=(j-1)*2*pi/nlon.  w is the east
c           longitude component and v is the colatitudinal component.  the
c           indices for v and w are defined at the input parameter isym.
c           the divergence of (v,w) is the zero scalar field.
c
c   pertrb  a nt dimensional array (see input parameter nt and assume nt=1
c           for the description that follows).  vt - pertrb is a scalar
c           field which can be the vorticity of a vector field (v,w).
c           pertrb is related to the scalar harmonic coefficients a,b
c           of vt (computed by shagc) by the formula
c
c                pertrb = a(1,1)/(2.*sqrt(2.))
c
c           an unperturbed vt can be the vorticity of a vector field
c           only if a(1,1) is zero.  if a(1,1) is nonzero (flagged by
c           pertrb nonzero) then subtracting pertrb from vt yields a
c           scalar field for which a(1,1) is zero.
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
      subroutine ivrtgc(nlat,nlon,isym,nt,v,w,idvw,jdvw,a,b,mdab,ndab,
     +                  wvhsgc,lvhsgc,work,lwork,pertrb,ierror)
      dimension v(idvw,jdvw,nt),w(idvw,jdvw,nt),pertrb(nt)
      dimension a(mdab,ndab,nt),b(mdab,ndab,nt)
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
c
c     check save work space length
c
      l1 = min0(nlat,(nlon+1)/2)
      l2 = (nlat+1)/2
      lwmin =   4*nlat*l2+3*max0(l1-2,0)*(2*nlat-l1-1)+nlon+15
      if (lvhsgc .lt. lwmin) return
c     if(lvhsgc .lt. 2*(lzz1+labc)+nlon+15) return
      ierror = 10
c
c     verify unsaved work space length
c
      mn = mmax*nlat*nt
      if(isym.ne.0  .and. lwork .lt.
     +nlat*(2*nt*nlon+max0(6*imid,nlon))+2*mn+nlat) return
      if(isym.eq.0  .and. lwork .lt.
     +imid*(2*nt*nlon+max0(6*nlat,nlon))+2*mn+nlat) return
      ierror = 0
c
c     set work space pointers
c
      icr = 1
      ici = icr + mn
      is = ici + mn
      iwk = is + nlat
      liwk = lwork-2*mn-nlat
      call ivtgc1(nlat,nlon,isym,nt,v,w,idvw,jdvw,work(icr),work(ici),
     +            mmax,work(is),mdab,ndab,a,b,wvhsgc,lvhsgc,work(iwk),
     +            liwk,pertrb,ierror)
      return
      end

      subroutine ivtgc1(nlat,nlon,isym,nt,v,w,idvw,jdvw,cr,ci,mmax,
     +sqnn,mdab,ndab,a,b,wsav,lsav,wk,lwk,pertrb,ierror)
      dimension v(idvw,jdvw,nt),w(idvw,jdvw,nt),pertrb(nt)
      dimension cr(mmax,nlat,nt),ci(mmax,nlat,nt),sqnn(nlat)
      dimension a(mdab,ndab,nt),b(mdab,ndab,nt)
      dimension wsav(lsav),wk(lwk)
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
c     set vorticity field perturbation adjustment
c
      pertrb(k) = a(1,1,k)/(2.*sqrt(2.))
c
c     preset br,bi to 0.0
c
      do 3 n=1,nlat
      do 4 m=1,mmax
      cr(m,n,k) = 0.0
      ci(m,n,k) = 0.0
    4 continue
    3 continue
c
c     compute m=0 coefficients
c
      do 5 n=2,nlat
      cr(1,n,k) = a(1,n,k)/sqnn(n)
      ci(1,n,k) = b(1,n,k)/sqnn(n)
    5 continue
c
c     compute m>0 coefficients
c
      do 6 m=2,mmax
      do 7 n=m,nlat
      cr(m,n,k) = a(m,n,k)/sqnn(n)
      ci(m,n,k) = b(m,n,k)/sqnn(n)
    7 continue
    6 continue
    2 continue
c
c     set ityp for vector synthesis with divergence=0
c
      if (isym.eq.0) then
      ityp = 2
      else if (isym.eq.1) then
      ityp = 5
      else if (isym.eq.2) then
      ityp = 8
      end if
c
c     vector sythesize cr,ci into divergence free vector field (v,w)
c
      call vhsgc(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
     +           mmax,nlat,wsav,lsav,wk,lwk,ierror)
      return
      end
