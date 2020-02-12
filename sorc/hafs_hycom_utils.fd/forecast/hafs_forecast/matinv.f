c ------------------------------------------------------------------
c --- matrix inversion subroutines for implicit solution of vertical
c --- diffusion equation - tri-diagonal matrix
c ------------------------------------------------------------------
c
      subroutine tridcof(diff,tri,nlayer,tcu,tcc,tcl)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
c --- compute coefficients for tridiagonal matrix (dimension=kdm).
c --- Note: tcu(1) = 0. and tcl(kdm+1) = 0. are necessary conditions.
c     
c --- input
      real diff(kdm+1)    ! diffusivity profile on interfaces
      integer nlayer
c
c --- output
      real tcu(kdm),      ! upper coeff. for (k-1) on k line of trid.matrix
     &     tcc(kdm),      ! central ...      (k  ) ..
     &     tcl(kdm)       ! lower .....      (k-1) ..
c --- common tridiagonal factors
      real tri(kdm,0:1)   ! dt/dz/dz factors in trid. matrix
c
c --- local
      integer k
c
c --- in the surface layer
      tcu(1)=0.
      tcc(1)=1.+tri(1,1)*diff(2)           ! 1.+ delt1/h(1)/dzb(1)*diff(2)
      tcl(1)=  -tri(1,1)*diff(2)           !   - delt1/h(1)/dzb(1)*diff(2)
c
c --- inside the domain
      do 10 k=2,nlayer
      tcu(k)=  -tri(k,0)*diff(k  )
      tcc(k)=1.+tri(k,1)*diff(k+1)+tri(k,0)*diff(k)
      tcl(k)=  -tri(k,1)*diff(k+1)
 10   continue
c
c --- in the bottom layer
      tcl(nlayer)= 0.
      return
      end

***********************************************************************

      subroutine tridrhs(h,yo,diff,ghat,ghatflux,tri,nlayer,rhs)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
c --- compute right hand side of tridiagonal matrix for scalar fields:
c --- =  yo (old field) 
c ---  + flux-divergence of ghat
c ---  + flux-divergence of non-turbulant fluxes
c
c --- note: if surface and bottom fluxes are nonzero, the following must apply
c ---    sfc. lyr. needs +delt1/h(1)*surfaceflux
c ---    bot. lyr. needs +delt1/h(nlayer)*diff(nlayer+1)/
c ---                     dzb(nlayer)*yo(nlayer+1)
c
c --- input
      real h(kdm),         ! layer thickness
     &     yo(kdm+1),      ! old profile
     &     diff(kdm+1),    ! diffusivity profile on interfaces
     &     ghat(kdm+1),    ! ghat turbulent flux   
     &     ghatflux        ! surface flux for ghat: includes solar flux
      integer nlayer
c
c --- output
      real rhs(kdm)        ! right hand side
c
      real tri(kdm,0:1)    ! dt/dz/dz factors in trid. matrix
c
c --- local
      integer k
c
c --- in the top layer
      rhs(1)=yo(1)+delt1/h(1)*(ghatflux*diff(2)*ghat(2))
c
c --- inside the domain 
      do 10 k=2,nlayer-1
      rhs(k)=yo(k)+delt1/h(k)*
     &      (ghatflux*(diff(k+1)*ghat(k+1)-diff(k)*ghat(k)))
 10   continue
c
c --- in the bottom layer     
      k=nlayer
      rhs(k)=yo(k)+delt1/h(k)*
     &      (ghatflux*(diff(k+1)*ghat(k+1)-diff(k)*ghat(k)))
c
      return
      end

***********************************************************************

      subroutine tridmat(tcu,tcc,tcl,nlayer,h,rhs,yo,yn,diff, i,j)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
c --- solve tridiagonal matrix for new vector yn, given right hand side
c --- vector rhs.
c
c --- note: if surface and bottom fluxes are nonzero, the following must apply
c ---    surface layer needs +delt1*surfaceflux/(h(1)*bet)
c ---    bottom  layer needs +tri(nlayer,1)*diff(nlayer+1)*yo(nlayer+1))/bet
c
c --- input
      real tcu (kdm),     ! upper coeff. for (k-1) on k line of tridmatrix
     &     tcc (kdm),     ! central ...      (k  ) ..
     &     tcl (kdm),     ! lower .....      (k-1) ..
     &     h  (kdm),      ! layer thickness
     &     rhs(kdm),      ! right hand side
     &     yo(kdm+1),     ! old field
     &     diff(kdm+1),   ! diffusivity profile
     &     gam(kdm)       ! temporary array for tridiagonal solver
      real bet            ! ...
      integer nlayer,     ! number of active layers <=kdm
     &        i,j         ! local grid point
c
c --- output
      real yn(kdm+1)      ! new field
c
c --- local
      integer k
c
c --- solve tridiagonal matrix.
      bet=tcc(1)
      yn(1)=rhs(1)/bet                               ! surface
      do 21 k=2,nlayer
      gam(k)=tcl(k-1)/bet
      bet=tcc(k)-tcu(k)*gam(k)
      if(bet.eq.0.) then
        write(lp,*) 
        write(lp,*) '** algorithm for solving tridiagonal matrix fails'
        write(lp,*) '** i,j=',i0+i,j0+j  !global grid point
        write(lp,*) '** bet=',bet
        write(lp,*) '** k=',k,' tcc=',tcc(k),' tcu=',tcu(k),
     &              ' gam=',gam(k)
        call flush(lp)
        call xchalt('(tridmat)')
               stop '(tridmat)'
*       bet=1.E-12
      endif
      yn(k) =      (rhs(k)  - tcu(k)  *yn(k-1)  )/bet
c     to avoid "Underflow" at single precision on the sun
c     yni   =      (rhs(k)  - tcu(k)  *yn(k-1)  )/bet 
c     if(yni.lt.0.) then
c       yn(k) =min( (rhs(k)  - tcu(k)  *yn(k-1)  )/bet ,-1.E-12 )
c     else
c       yn(k) = max( (rhs(k)  - tcu(k)  *yn(k-1)  )/bet , 1.E-12 )
c     endif
 21   continue
c
      do 22 k=nlayer-1,1,-1
      yn(k)=yn(k)-gam(k+1)*yn(k+1)
 22   continue
c
      yn(nlayer+1)=yo(nlayer+1)
c
      return
      end
