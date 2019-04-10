      module sphere
!
! fortran95 interface for spherepack.
! For regular and gaussian lat/lon grids.
!
! requires NCAR's SPHEREPACK 3.2
! (http://www2.cisl.ucar.edu/resources/legacy/spherepack)
!
! Version 1.2 - February, 2012
! Jeff Whitaker <jeffrey.s.whitaker@noaa.gov>
 
!----------------------------------------------------------------------------
! Fortran95 module that drives NCAR's SPHEREPACK 3.0 Fortran77 routines 
! for spherical harmonic synthesis and analysis. sphere.f90 works for
! both gaussian and regularly spaced lat/lon grids. All of the routines
! have very simple interfaces, and the arguments are almost self-explanatory.
! The last argument for each subroutine (gridtype - character*3) is optional
! and specifies the type of lat/lon grid. Valid values are 'REG' or 'GAU'
! ('REG' is the default). Each one of the routines can also be called
! using SUBROUTINENAME_REG or SUBROUTINENAME_GAU, with the gridtype argument
! omitted. For example, converting spherical harmonic coefficients to a 
! regular grid can be accomplished by CALL GRDTOSPEC(datagrid,dataspec,'REG')
! or CALL SPECTOGRD_REG(datagrid,dataspec). CALL GRDTOSPEC(datagrid,dataspec)
! would accomplish the same thing, since the default value of the optional
! argument gridtype is 'REG'. 

! The available routines are:

! SUBROUTINE GRDTOSPEC(datagrid,dataspec,gridtype):
! converts input gridded data array (datagrid) to complex spectral
! coefficients (dataspec).
 
! SUBROUTINE SPECTOGRD(dataspec,datagrid,gridtype):
! converts input spectral coefficient array (dataspec) to a grid (datagrid).

! SUBROUTINE SPECSMOOTH(datagrid,smooth,gridtype):
! isotropic spectral smoothing of input gridded data array (datagrid).
! Smoothing is a function of total wavenumber only, given by vector smooth
! (smooth(j),j=1,mtrunc+1, where mtrunc is the triangular truncation 
! limit - see Important Details below). For example, for Gaussian spectral
! smoothing, use smooth(j) = exp(-(float(j-1)/deln)**2), where deln is the
! e-folding width of the smoother in total wavenumber space. For straight
! triangular truncation, set smooth(j) = 0 for j-1 > desired truncation limit.

! SUBROUTINE GETUV(vrtspec,divspec,ugrid,vgrid,rsphere,gridtype):
! given input spectral coefficients of vorticity and divergence
! (vrtspec,divspec) calculates gridded winds (ugrid,vgrid).
! rsphere is the radius of the sphere.

! SUBROUTINE GETGRAD(dataspec,gradx,grady,rsphere,gridtype):
! calculates gridded vector gradient (gradx,grady) given input
! spectral coefficients. rsphere is the radius of the sphere.

! SUBROUTINE GETVRTDIVSPEC(ugrid,vgrid,vrtspec,divspec,rsphere,gridtype):
! given input gridded winds (ugrid,vgrid) calculates spectral coefficients
! of vorticity and divergence (vrtspec,divspec). rsphere is the radius of 
! the sphere.

! SUBROUTINE GAUINFO(gaulats,weights):
! calculate gaussian latitudes and weights for grid determined
! by the size of gaulats and weights (which must be the same).

! SUBROUTINE CLEANUP(gridtype):
! Garbage collection. Deallocates all work arrays.
! Call this when you are done with the module to free up memory. 

! All of these routines use triangular truncation. 

! Important Details:

! The grid and spectral arrays must be rank 2 and rank 1, respectively.
! Passing array sections is OK.

! The gridded data is assumed to be oriented such that i=1 is the 
! Greenwich meridian and j=1 is the northernmost point. Grid indices
! increase eastward and southward. If nlat is odd the equator will be
! included as a grid point. If nlat is even the equator will lie half
! way between points nlat/2 and (nlat/2)+1. nlat must be at least 3. 
! The grid increment in longitude is 2*pi/nlon radians. For example,
! nlon = 72 for a five degree grid. nlon must be greater than or 
! equal to 4. The efficiency of the computation is improved when nlon
! is a product of small prime numbers. 

! The spectral data is assumed to be in a complex array of dimension
! (MTRUNC+1)*(MTRUNC+2)/2. MTRUNC is the triangular truncation limit
! (MTRUNC = 42 for T42). MTRUNC must be <= nlat-1. Coefficients are
! ordered so that first (nm=1) is m=0,n=0, second is m=0,n=1, 
! nm=mtrunc is m=0,n=mtrunc, nm=mtrunc+1 is m=1,n=1, etc.
! In Fortran95 syntax, values of m (degree) and n (order) as a function
! of the index nm are: 

! integer, dimension((mtrunc+1)*(mtrunc+2)/2) :: indxm,indxn
! indxm = (/((m,n=m,mtrunc),m=0,mtrunc)/)
! indxn = (/((n,n=m,mtrunc),m=0,mtrunc)/)

! Conversely, the index nm as a function of m and n is: 
! nm = sum((/(i,i=mtrunc+1,mtrunc-m+2,-1)/))+n-m+1

! The associated legendre polynomials are normalized so that the
! integral (pbar(n,m,theta)**2)*sin(theta) on the interval theta=0
! to theta=pi is 1, where: 

! pbar(m,n,theta) = sqrt((2*n+1)*factorial(n-m)/(2*factorial(n+m))) *
! sin(theta)**m/(2**n*factorial(n)) times the (n+m)th derivative of
! (x**2-1)**n with respect to x=cos(theta) 

! note: theta = 0.5*pi - phi, where phi is latitude and theta is colatitude.
! Therefore, cos(theta) = sin(phi) and sin(theta) = cos(phi). 
! Note that pbar(0,0,theta)=sqrt(2)/2, and 
! pbar(1,0,theta)=0.5*sqrt(6.)*sin(lat). 
!----------------------------------------------------------------------------


! logical variables set to .FALSE. after work arrays initialized on
! first call. If nlon or nlat changes (as determined by size of
! input array), work arrays are reallocated and
! recomputed.

      implicit none 
      private 
      public :: &
      grdtospec_reg,spectogrd_reg,getuv_reg,getgrad_reg,&
      getvrtdivspec_reg,specsmooth_reg,cleanup_reg,&
      grdtospec_gau,spectogrd_gau,getuv_gau,getgrad_gau,&
      getvrtdivspec_gau,specsmooth_gau,cleanup_gau,&
      grdtospec,spectogrd,getuv,getgrad,&
      getvrtdivspec,specsmooth,cleanup

      integer, save :: saved_nlon_reg_spectogrd = -1  
      integer, save :: saved_nlat_reg_spectogrd = -1  
      integer, save :: saved_nlon_reg_vspectogrd = -1  
      integer, save :: saved_nlat_reg_vspectogrd = -1  
      integer, save :: saved_nlon_reg_grdtospec = -1  
      integer, save :: saved_nlat_reg_grdtospec = -1  
      integer, save :: saved_nlon_reg_vgrdtospec = -1  
      integer, save :: saved_nlat_reg_vgrdtospec = -1  
      integer, save :: saved_nlon_gau_spectogrd = -1  
      integer, save :: saved_nlat_gau_spectogrd = -1  
      integer, save :: saved_nlon_gau_vspectogrd = -1  
      integer, save :: saved_nlat_gau_vspectogrd = -1  
      integer, save :: saved_nlon_gau_grdtospec = -1  
      integer, save :: saved_nlat_gau_grdtospec = -1  
      integer, save :: saved_nlon_gau_vgrdtospec = -1  
      integer, save :: saved_nlat_gau_vgrdtospec = -1  
      logical, save :: lfrst_grdtospec_reg=.TRUE.
      logical, save :: lfrst_spectogrd_reg=.TRUE.
      logical, save :: lfrst_vgrdtospec_reg=.TRUE.
      logical, save :: lfrst_vspectogrd_reg=.TRUE.
      logical, save :: lfrst_grdtospec_gau=.TRUE.
      logical, save :: lfrst_spectogrd_gau=.TRUE.
      logical, save :: lfrst_vgrdtospec_gau=.TRUE.
      logical, save :: lfrst_vspectogrd_gau=.TRUE.

! work arrays are allocated in module subroutines when above
! logical variables are .TRUE., or when nlon or nlat change.

      real, dimension(:), allocatable, save :: wshaes,wshses,wvhaes,wvhses
      real, dimension(:), allocatable, save :: wshags,wshsgs,wvhags,wvhsgs

      contains

      subroutine gauinfo(gaulats,weights)
!
! calculate gaussian latitudes and weights.
! number of latitudes desired inferred from size of gaulats and weights
! (both must have same size)
!
      real, dimension(:), intent(out) :: gaulats,weights
      double precision, dimension(:), allocatable :: theta,wts
      double precision workdp
      integer nlat, lwork, ierror

      nlat = size(gaulats)
      if (nlat .ne. size(weights)) then
         write(6,*) 'gaulats and weights must be same size!'
         write(6,*) 'stopping in gauinfo'
         stop
      end if
      lwork = nlat*(nlat+2)
      allocate(theta(nlat))
      allocate(wts(nlat))
      call gaqd(nlat,theta,wts,workdp,lwork,ierror)
      gaulats = 2.*atan(1.0) - theta
      weights = wts
      deallocate(theta)
      deallocate(wts)
  
      end subroutine gauinfo

      subroutine grdtospec(datagrid,dataspec,gridtype)
      real, dimension(:,:), intent(in) :: datagrid
      complex, dimension(:), intent(out) :: dataspec
      character(*), intent(in), optional :: gridtype
      if (present(gridtype)) then
      if (gridtype .eq. 'GAU') then
           call  grdtospec_gau(datagrid,dataspec)
      else if (gridtype .eq. 'REG') then
           call  grdtospec_reg(datagrid,dataspec)
      else
           write(6,*) 'SUBROUTINE GRDTOSPEC:'
           write(6,*) 'optional argument gridtype = ',gridtype
           write(6,*) 'must be either REG or GAU (default REG)'
           stop
      end if
      else
           call  grdtospec_reg(datagrid,dataspec)
      endif
      end subroutine grdtospec

      subroutine spectogrd(dataspec,datagrid,gridtype)
      real, dimension(:,:), intent(out) :: datagrid
      complex, dimension(:), intent(in) :: dataspec
      character(*), intent(in), optional :: gridtype
      if (present(gridtype)) then
      if (gridtype .eq. 'GAU') then
           call  spectogrd_gau(dataspec,datagrid)
      else if (gridtype .eq. 'REG') then
           call  spectogrd_reg(dataspec,datagrid)
      else
           write(6,*) 'SUBROUTINE SPECTOGRD:'
           write(6,*) 'optional argument gridtype = ',gridtype
           write(6,*) 'must be either REG or GAU (default REG)'
           stop
      end if
      else
           call  spectogrd_reg(dataspec,datagrid)
      endif
      end subroutine spectogrd

      subroutine specsmooth(datagrid,smooth,gridtype)
      real, dimension(:,:), intent(inout) :: datagrid
      real, dimension(:), intent(in) :: smooth
      character(*), intent(in), optional :: gridtype
      if (present(gridtype)) then
      if (gridtype .eq. 'GAU') then
           call  specsmooth_gau(datagrid,smooth)
      else if (gridtype .eq. 'REG') then
           call  specsmooth_reg(datagrid,smooth)
      else
           write(6,*) 'SUBROUTINE SPECSMOOTH:'
           write(6,*) 'optional argument gridtype = ',gridtype
           write(6,*) 'must be either REG or GAU (default REG)'
           stop
      end if
      else
           call  specsmooth_reg(datagrid,smooth)
      endif
      end subroutine specsmooth

      subroutine getvrtdivspec(ugrid,vgrid,vrtspec,divspec,rsphere,gridtype)
      real, intent(in) :: rsphere
      real, dimension(:,:), intent(in) :: ugrid,vgrid
      complex, dimension(:), intent(out) :: vrtspec,divspec
      character(*), intent(in), optional :: gridtype
      if (present(gridtype)) then
      if (gridtype .eq. 'GAU') then
           call getvrtdivspec_gau(ugrid,vgrid,vrtspec,divspec,rsphere)
      else if (gridtype .eq. 'REG') then
           call getvrtdivspec_reg(ugrid,vgrid,vrtspec,divspec,rsphere)
      else
           write(6,*) 'SUBROUTINE GETVRTDIVSPEC:'
           write(6,*) 'optional argument gridtype = ',gridtype
           write(6,*) 'must be either REG or GAU (default REG)'
           stop
      end if
      else
           call getvrtdivspec_reg(ugrid,vgrid,vrtspec,divspec,rsphere)
      endif
      end subroutine getvrtdivspec

      subroutine getuv(vrtspec,divspec,ugrid,vgrid,rsphere,gridtype)
      real, intent(in) :: rsphere
      real, dimension(:,:), intent(out) :: ugrid,vgrid
      complex, dimension(:), intent(in) :: vrtspec,divspec
      character(*), intent(in), optional :: gridtype
      if (present(gridtype)) then
      if (gridtype .eq. 'GAU') then
           call getuv_gau(vrtspec,divspec,ugrid,vgrid,rsphere)
      else if (gridtype .eq. 'REG') then
           call getuv_reg(vrtspec,divspec,ugrid,vgrid,rsphere)
      else
           write(6,*) 'SUBROUTINE GETUV:'
           write(6,*) 'optional argument gridtype = ',gridtype
           write(6,*) 'must be either REG or GAU (default REG)'
           stop
      end if
      else
           call getuv_reg(vrtspec,divspec,ugrid,vgrid,rsphere)
      endif
      end subroutine getuv

      subroutine getgrad(dataspec,gradx,grady,rsphere,gridtype)
      real, intent(in) :: rsphere
      real, dimension(:,:), intent(out) :: gradx,grady
      complex, dimension(:), intent(in) :: dataspec
      character(*), intent(in), optional :: gridtype
      if (present(gridtype)) then
      if (gridtype .eq. 'GAU') then
           call getgrad_gau(dataspec,gradx,grady,rsphere)
      else if (gridtype .eq. 'REG') then
           call getgrad_reg(dataspec,gradx,grady,rsphere)
      else
           write(6,*) 'SUBROUTINE GETGRAD:'
           write(6,*) 'optional argument gridtype = ',gridtype
           write(6,*) 'must be either REG or GAU (default REG)'
           stop
      end if
      else
           call getgrad_reg(dataspec,gradx,grady,rsphere)
      endif
      end subroutine getgrad
  
      subroutine cleanup(gridtype)
      character(*), intent(in), optional :: gridtype
      if (present(gridtype)) then
      if (gridtype .eq. 'GAU') then
           call cleanup_gau
      else if (gridtype .eq. 'REG') then
           call cleanup_reg
      else
           write(6,*) 'SUBROUTINE CLEANUP:'
           write(6,*) 'optional argument gridtype = ',gridtype
           write(6,*) 'must be either REG or GAU (default REG)'
           stop
      end if
      else
           call cleanup_reg
      endif
      end subroutine cleanup

      subroutine grdtospec_reg(datagrid,dataspec)

! converts gridded input array (datagrid) to complex spectral coefficients
! (dataspec).

      real, dimension(:,:), intent(in) :: datagrid
      complex, dimension(:), intent(out) :: dataspec

      real, dimension((4*size(datagrid,1)+2)*size(datagrid,2)) :: work
      double precision, dimension(2*(size(datagrid,2)+1)) :: dwork
      real, dimension(size(datagrid,2),size(datagrid,1)) :: temp
      real, dimension(size(datagrid,2),size(datagrid,2)) :: a,b
      integer nlon,nlat,lwork,ldwork,ntrunc,l1,l2,lshaes,ierror,m,n

! compute array dimensions and infer truncation limit
! from size of spectral arrays.


      nlon = size(datagrid,1)
      nlat = size(datagrid,2)
      if (nlon .ne. saved_nlon_reg_grdtospec .or. nlat .ne. saved_nlat_reg_grdtospec) then
          lfrst_grdtospec_reg = .TRUE.
          saved_nlon_reg_grdtospec = nlon
          saved_nlat_reg_grdtospec = nlat
      end if
      lwork = size(work)
      ldwork = size(dwork)
      ntrunc = nint((-1.+sqrt(1+8*float(size(dataspec))))/2.)-1


! initialize work array wshaes for spherical harmonic analysis.
! only done when lfrst_grdtospec_reg = .T.

      if (lfrst_grdtospec_reg) then

      if (allocated(wshaes)) deallocate(wshaes)

      if (mod(nlon,2) .eq. 0) then
         l1 = min0(nlat,(nlon+2)/2) 
      else
         l1 = min0(nlat,(nlon+1)/2)
      end if
      if (mod(nlat,2) .eq. 0) then
         l2 = nlat/2      
      else
         l2 = (nlat+1)/2
      end if

      lshaes = (l1*l2*(nlat+nlat-l1+1))/2+nlon+15

      allocate(wshaes(lshaes))

      call shaesi(nlat,nlon,wshaes,lshaes, &
          work,lwork,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,1001) ierror
 1001 format(' error',i4,' in shaesi')
      lfrst_grdtospec_reg = .FALSE.
      else
      lshaes = size(wshaes,1)
      endif

! transpose data.

      temp = transpose(datagrid)

! spherical harmonic analysis.

      call shaes(nlat,nlon,0,1,temp,nlat,nlon,a,b,nlat,nlat, &
                 wshaes,lshaes,work,lwork,ierror)
      if(ierror .ne. 0) write(*,1003) ierror
 1003  format(' error',i4,' in shaes')
 
! fill complex array dataspec with result.

      dataspec = cmplx( 0.5*(/((a(m,n),n=m,ntrunc+1),m=1,ntrunc+1)/), &
                        0.5*(/((b(m,n),n=m,ntrunc+1),m=1,ntrunc+1)/) )
 
      end subroutine grdtospec_reg

      subroutine spectogrd_reg(dataspec,datagrid)
      
! converts complex spectral coefficients (dataspec) to 
! gridded data array (datagrid).


      real, dimension(:,:), intent(inout) :: datagrid
      complex, dimension(:), intent(in) :: dataspec

      real, dimension((4*size(datagrid,1)+2)*size(datagrid,2)) :: work
      double precision, dimension(2*(size(datagrid,2)+1)) :: dwork
      real, dimension(size(datagrid,2),size(datagrid,1)) :: temp
      real, dimension(size(datagrid,2),size(datagrid,2)) :: a,b

      integer nlon,nlat,lwork,ldwork,ntrunc,l1,l2,lshses,ierror,m,n,nn,i
      
! compute array dimensions and infer truncation limit
! from size of dataspec.

      nlon = size(datagrid,1)
      nlat = size(datagrid,2)
      if (nlon .ne. saved_nlon_reg_spectogrd .or. nlat .ne. saved_nlat_reg_spectogrd) then
          lfrst_spectogrd_reg = .TRUE.
          saved_nlon_reg_spectogrd = nlon
          saved_nlat_reg_spectogrd = nlat
      else
      end if
      lwork = size(work)
      lwork = size(work)
      ldwork = size(dwork)
      ntrunc = nint((-1.+sqrt(1+8*float(size(dataspec))))/2.)-1

! compute work array wshses for spherical harmonic synthesis.
! only done when lfrst_spectogrd_reg = .T.

      if (lfrst_spectogrd_reg) then

      if (allocated(wshses)) deallocate(wshses)

      if (mod(nlon,2) .eq. 0) then
         l1 = min0(nlat,(nlon+2)/2) 
      else
         l1 = min0(nlat,(nlon+1)/2) 
      end if
      if (mod(nlat,2) .eq. 0) then
         l2 = nlat/2        
      else
         l2 = (nlat+1)/2    
      end if

      lshses = (l1*l2*(nlat+nlat-l1+1))/2+nlon+15

      allocate(wshses(lshses))

      call shsesi(nlat,nlon,wshses,lshses,work,lwork, &
                  dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,1001) ierror
 1001 format(' error',i4,' in shsesi')
      lfrst_spectogrd_reg = .FALSE.
      else
      lshses = size(wshses,1)
      endif

! fill two real arrays (a,b) with contents of dataspec.

      a = 0.
      b = 0.
      do m=1,ntrunc+1
      do n=m,ntrunc+1
         nn = sum((/(i,i=ntrunc+1,ntrunc-m+3,-1)/))+n-m+1
         a(m,n) = 2.*real(dataspec(nn))
         b(m,n) = 2.*aimag(dataspec(nn))
      enddo
      enddo

! spherical harmonic synthesis.

      call shses(nlat,nlon,0,1,temp,nlat,nlon,a,b,nlat,nlat, &
                 wshses,lshses,work,lwork,ierror)
      if(ierror .ne. 0) write(*,1003) ierror
 1003  format(' error',i4,' in shses')

! transpose data.

      datagrid = transpose(temp)
 
      end subroutine spectogrd_reg

      subroutine getgrad_reg(dataspec,gradx,grady,rsphere)

! given spectral coefficients compute gridded vector gradient.
! rsphere is the radius of the sphere in meters.

      real, intent(in) :: rsphere
      real, dimension(:,:), intent(out) :: grady,gradx
      complex, dimension(:), intent(in) :: dataspec

      real, dimension((4*size(grady,1)+2)*size(grady,2)) :: work
      double precision, dimension(2*(size(grady,2)+1)) :: dwork
      real, dimension(size(grady,2),size(grady,1)) :: v,w
      real, dimension(size(grady,2),size(grady,2)) :: br,bi,cr,ci,a,b
      real, dimension(size(grady,2)) :: sqnn

      real fn
      integer nlon,nlat,lwork,ldwork,ntrunc,l1,l2,lvhses,ierror,m,n,nn,i
      
! compute array dimensions and infer truncation limit
! from size of spectral arrays.

      nlon = size(grady,1)
      nlat = size(grady,2)
      if (nlon .ne. saved_nlon_reg_vspectogrd .or. nlat .ne. saved_nlat_reg_vspectogrd) then
          lfrst_vspectogrd_reg = .TRUE.
          saved_nlon_reg_vspectogrd = nlon
          saved_nlat_reg_vspectogrd = nlat
      end if
      lwork = size(work)
      ldwork = size(dwork)
      ntrunc = nint((-1.+sqrt(1+8*float(size(dataspec))))/2.)-1
      
! if not already done, compute work arrays for vector spherical
! harmonic synthesis.

      if (lfrst_vspectogrd_reg) then

      if (allocated(wvhses)) deallocate(wvhses)

      if (mod(nlon,2) .eq. 0) then
         l1 = min0(nlat,nlon/2) 
      else
         l1 = min0(nlat,(nlon+1)/2)
      end if
      if (mod(nlat,2) .eq. 0) then
         l2 = nlat/2      
      else
         l2 = (nlat+1)/2    
      end if

      lvhses =  l1*l2*(nlat+nlat-l1+1)+nlon+15

      allocate(wvhses(lvhses))

      call vhsesi(nlat,nlon,wvhses,lvhses,work,lwork,dwork, &
                  ldwork,ierror)
      if(ierror .ne. 0) write(*,1001) ierror
 1001 format(' error',i4,' in vhsesi')
      lfrst_vspectogrd_reg = .FALSE.
      else
      lvhses = size(wvhses,1)
      end if

! multiply spectral coefficients of vorticity and divergence
! by appropriate factors to convert them into vector harmonic
! coefficients of winds.

      do n=1,nlat
        fn = float(n-1)
        sqnn(n) = sqrt(fn*(fn+1.))/rsphere
      enddo

      a = 0.
      b = 0.
      br = 0.
      bi = 0.
      do m=1,ntrunc+1
      do n=m,ntrunc+1
         nn = sum((/(i,i=ntrunc+1,ntrunc-m+3,-1)/))+n-m+1
         a(m,n) = -2.*real(dataspec(nn))
         b(m,n) = -2.*aimag(dataspec(nn))
      enddo
      enddo
      do n=1,nlat
      br(:,n) = a(:,n)*sqnn(n)
      bi(:,n) = b(:,n)*sqnn(n)
      enddo

      cr = 0.
      ci = 0.
      
! compute vector harmonic synthesis to get winds on grid.

      call vhses(nlat,nlon,0,1,v,w,nlat,nlon,br,bi,cr,ci, &
                 nlat,nlat,wvhses,lvhses,work,lwork,ierror)
      if(ierror .ne. 0) write(*,1003) ierror
 1003  format(' error',i4,' in vhaes')
 
! transpose data.
! minus sign to account for differences
! between mathematical and geophysical spherical coords.

      grady = transpose(v)
      gradx = -transpose(w)
 
      end subroutine getgrad_reg

      subroutine specsmooth_reg(datagrid,smooth)
      
! isotropic spectral smoothing of gridded data (datagrid).
! smoothing is a function of total wavenumber only, as specified
! by (smooth(n),n=1,nlat).

      real, dimension(:,:), intent(inout) :: datagrid
      real, dimension(:), intent(in) :: smooth

      real, dimension((4*size(datagrid,1)+2)*size(datagrid,2)) :: work
      double precision, dimension(2*(size(datagrid,2)+1)) :: dwork
      real, dimension(size(datagrid,2),size(datagrid,1)) :: temp
      real, dimension(size(datagrid,2),size(datagrid,2)) :: a,b

      integer nlon,nlat,lwork,ldwork,l1,l2,lshaes,lshses,ierror,j
      
! compute array dimensions.

      nlon = size(datagrid,1)
      nlat = size(datagrid,2)
      if (nlon .ne. saved_nlon_reg_spectogrd .or. &
          nlat .ne. saved_nlat_reg_spectogrd .or. &
          nlon .ne. saved_nlon_reg_grdtospec .or. &
          nlat .ne. saved_nlat_reg_grdtospec) then
          lfrst_grdtospec_reg = .TRUE.
          lfrst_spectogrd_reg = .TRUE.
          saved_nlon_reg_spectogrd = nlon
          saved_nlat_reg_spectogrd = nlat
          saved_nlon_reg_grdtospec = nlon
          saved_nlat_reg_grdtospec = nlat
      end if
      lwork = size(work)
      lwork = size(work)
      ldwork = size(dwork)
      
! if not already done, initialized work arrays for spherical
! harmonic synthesis and analysis.

      if (lfrst_grdtospec_reg) then

      if (allocated(wshaes)) deallocate(wshaes)

      if (mod(nlon,2) .eq. 0) then
         l1 = min0(nlat,(nlon+2)/2) 
      else
         l1 = min0(nlat,(nlon+1)/2) 
      end if
      if (mod(nlat,2) .eq. 0) then
         l2 = nlat/2      
      else
         l2 = (nlat+1)/2
      end if

      lshaes = (l1*l2*(nlat+nlat-l1+1))/2+nlon+15

      allocate(wshaes(lshaes))

      call shaesi(nlat,nlon,wshaes,lshaes, &
          work,lwork,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,1001) ierror
 1001 format(' error',i4,' in shaesi')
      lfrst_grdtospec_reg = .FALSE.
      else
      lshaes = size(wshaes,1)
      end if
      if (lfrst_spectogrd_reg) then

      if (allocated(wshses)) deallocate(wshses)

      if (mod(nlon,2) .eq. 0) then
         l1 = min0(nlat,(nlon+2)/2) 
      else
         l1 = min0(nlat,(nlon+1)/2) 
      end if
      if (mod(nlat,2) .eq. 0) then
         l2 = nlat/2        
      else
         l2 = (nlat+1)/2    
      end if

      lshses = (l1*l2*(nlat+nlat-l1+1))/2+nlon+15

      allocate(wshses(lshses))

      call shsesi(nlat,nlon,wshses,lshses,work,lwork, &
        dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,1002) ierror
 1002 format(' error',i4,' in shsesi')
      lfrst_spectogrd_reg = .FALSE.
      else
      lshses = size(wshses,1)
      endif
      
! transpose data.

      temp = transpose(datagrid)
      
! perform spherical harmonic analysis.

      call shaes(nlat,nlon,0,1,temp,nlat,nlon,a,b,nlat,nlat, &
                 wshaes,lshaes,work,lwork,ierror)
      if(ierror .ne. 0) write(*,1003) ierror
 1003  format(' error',i4,' in shaes')
 
! multiply spectral coefficients by smoothing factor
! (a function of degree only).

      do j=1,nlat
         a(:,j) = smooth(j)*a(:,j)
         b(:,j) = smooth(j)*b(:,j)
      enddo
      
! perform spherical harmonic synthesis.

      call shses(nlat,nlon,0,1,temp,nlat,nlon,a,b,nlat,nlat, &
                 wshses,lshses,work,lwork,ierror)
      if(ierror .ne. 0) write(*,1004) ierror
 1004  format(' error',i4,' in shses')
 
! transpose data.

      datagrid = transpose(temp)
 
      end subroutine specsmooth_reg

      subroutine getvrtdivspec_reg(ugrid,vgrid,vrtspec,divspec,rsphere)
      
! calculate spectral coefficients of vorticity and divergence
! (vrtspec,divspec) given input gridded winds (ugrid,vgrid).
! rsphere is the radius of the sphere in meters.

      real, intent(in) :: rsphere
      real, dimension(:,:), intent(in) :: ugrid,vgrid
      complex, dimension(:), intent(out) :: vrtspec,divspec

      real, dimension((4*size(ugrid,1)+2)*size(ugrid,2)) :: work
      double precision, dimension(2*(size(ugrid,2)+1)) :: dwork
      real, dimension(size(ugrid,2),size(ugrid,1)) :: v,w
      real, dimension(size(ugrid,2),size(ugrid,2)) :: br,bi,cr,ci,a,b
      real, dimension(size(ugrid,2)) :: sqnn

      real fn
      integer nlon,nlat,lwork,ldwork,ntrunc,l1,l2,lvhaes,ierror,m,n
      
! compute array dimensions and infer truncation limit
! from size of spectral arrays.

      nlon = size(ugrid,1)
      nlat = size(ugrid,2)
      if (nlon .ne. saved_nlon_reg_vgrdtospec .or. nlat .ne. saved_nlat_reg_vgrdtospec) then
          lfrst_vgrdtospec_reg = .TRUE.
          saved_nlon_reg_vgrdtospec = nlon
          saved_nlat_reg_vgrdtospec = nlat
      end if
      lwork = size(work)
      ldwork = size(dwork)
      ntrunc = nint((-1.+sqrt(1+8*float(size(vrtspec))))/2.)-1
      
! if not already done, compute work arrays for vector spherical
! harmonic analysis and scalar spherical harmonic synthesis.

      if (lfrst_vgrdtospec_reg) then

      if (allocated(wvhaes)) deallocate(wvhaes)

      if (mod(nlon,2) .eq. 0) then
         l1 = min0(nlat,nlon/2) 
      else
         l1 = min0(nlat,(nlon+1)/2)
      end if
      if (mod(nlat,2) .eq. 0) then
         l2 = nlat/2      
      else
         l2 = (nlat+1)/2    
      end if

      lvhaes =  l1*l2*(nlat+nlat-l1+1)+nlon+15

      allocate(wvhaes(lvhaes))

      call vhaesi(nlat,nlon,wvhaes,lvhaes,work,lwork,dwork, &
                  ldwork,ierror)
      if(ierror .ne. 0) write(*,1001) ierror
 1001 format(' error',i4,' in vhaesi')
      lfrst_vgrdtospec_reg = .FALSE.
      else
      lvhaes = size(wvhaes,1)
      endif
      
! transpose data.
! minus sign to account for difference between
! mathematical and geophysical spherical coords.

      v = -transpose(vgrid)
      w = transpose(ugrid)

! calculate vector spherical harmonic analysis.

      call vhaes(nlat,nlon,0,1,v,w,nlat,nlon,br,bi,cr,ci, &
                 nlat,nlat,wvhaes,lvhaes,work,lwork,ierror)
      if(ierror .ne. 0) write(*,1003) ierror
 1003  format(' error',i4,' in vhaes')
 
! multiply vector harmonic coefficients of winds by 
! appropriate factors to convert into vorticity and
! divergence coefficients.

      do n=1,nlat
      fn = float(n-1)
      sqnn(n) = sqrt(fn*(fn+1.))
      enddo

      a = 0.
      b = 0.
      do n=1,nlat
      a(:,n) = -(sqnn(n)/rsphere)*br(:,n)
      b(:,n) = -(sqnn(n)/rsphere)*bi(:,n)
      enddo
      divspec = cmplx( 0.5*(/((a(m,n),n=m,ntrunc+1),m=1,ntrunc+1)/), &
                       0.5*(/((b(m,n),n=m,ntrunc+1),m=1,ntrunc+1)/) )

      do n=1,nlat
      a(:,n) = (sqnn(n)/rsphere)*cr(:,n)
      b(:,n) = (sqnn(n)/rsphere)*ci(:,n)
      enddo
      vrtspec = cmplx( 0.5*(/((a(m,n),n=m,ntrunc+1),m=1,ntrunc+1)/), &
                       0.5*(/((b(m,n),n=m,ntrunc+1),m=1,ntrunc+1)/) )

      end subroutine getvrtdivspec_reg

      subroutine getuv_reg(vrtspec,divspec,ugrid,vgrid,rsphere)
      
! given spectral coefficients of vorticity and divergence
! (vrtspec,divspec) compute gridded winds (ugrid,vgrid).
! rsphere is the radius of the sphere in meters.

      real, intent(in) :: rsphere
      real, dimension(:,:), intent(out) :: ugrid,vgrid
      complex, dimension(:), intent(in) :: vrtspec,divspec

      real, dimension((4*size(ugrid,1)+2)*size(ugrid,2)) :: work
      double precision, dimension(2*(size(ugrid,2)+1)) :: dwork
      real, dimension(size(ugrid,2),size(ugrid,1)) :: v,w
      real, dimension(size(ugrid,2),size(ugrid,2)) :: br,bi,cr,ci,a,b
      real, dimension(size(ugrid,2)) :: sqnn

      real fn
      integer nlon,nlat,lwork,ldwork,ntrunc,l1,l2,lvhses,ierror,m,n,nn,i
      
! compute array dimensions and infer truncation limit
! from size of spectral arrays.

      nlon = size(ugrid,1)
      nlat = size(ugrid,2)
      if (nlon .ne. saved_nlon_reg_vspectogrd .or. nlat .ne. saved_nlat_reg_vspectogrd) then
          lfrst_vspectogrd_reg = .TRUE.
          saved_nlon_reg_vspectogrd = nlon
          saved_nlat_reg_vspectogrd = nlat
      end if
      lwork = size(work)
      ldwork = size(dwork)
      ntrunc = nint((-1.+sqrt(1+8*float(size(vrtspec))))/2.)-1
      
! if not already done, compute work arrays for vector spherical
! harmonic synthesis.

      if (lfrst_vspectogrd_reg) then

      if (allocated(wvhses)) deallocate(wvhses)

      if (mod(nlon,2) .eq. 0) then
         l1 = min0(nlat,nlon/2) 
      else
         l1 = min0(nlat,(nlon+1)/2)
      end if
      if (mod(nlat,2) .eq. 0) then
         l2 = nlat/2      
      else
         l2 = (nlat+1)/2    
      end if

      lvhses =  l1*l2*(nlat+nlat-l1+1)+nlon+15

      allocate(wvhses(lvhses))

      call vhsesi(nlat,nlon,wvhses,lvhses,work,lwork,dwork, &
                  ldwork,ierror)
      if(ierror .ne. 0) write(*,1001) ierror
 1001 format(' error',i4,' in vhsesi')
      lfrst_vspectogrd_reg = .FALSE.
      else
      lvhses = size(wvhses,1)
      end if

! multiply spectral coefficients of vorticity and divergence
! by appropriate factors to convert them into vector harmonic
! coefficients of winds.

      sqnn(1) = 0.
      do n=2,nlat
      fn = float(n-1)
      sqnn(n) = rsphere/sqrt(fn*(fn+1.))
      enddo

      a = 0.
      b = 0.
      br = 0.
      bi = 0.
      do m=1,ntrunc+1
      do n=m,ntrunc+1
         nn = sum((/(i,i=ntrunc+1,ntrunc-m+3,-1)/))+n-m+1
         a(m,n) = -2.*real(divspec(nn))
         b(m,n) = -2.*aimag(divspec(nn))
      enddo
      enddo
      do n=1,nlat
      br(:,n) = sqnn(n)*a(:,n)
      bi(:,n) = sqnn(n)*b(:,n)
      enddo

      cr = 0.
      ci = 0.
      do m=1,ntrunc+1
      do n=m,ntrunc+1
         nn = sum((/(i,i=ntrunc+1,ntrunc-m+3,-1)/))+n-m+1
         a(m,n) = 2.*real(vrtspec(nn))
         b(m,n) = 2.*aimag(vrtspec(nn))
      enddo
      enddo
      do n=1,nlat
      cr(:,n) = sqnn(n)*a(:,n)
      ci(:,n) = sqnn(n)*b(:,n)
      enddo
      
! compute vector harmonic synthesis to get winds on grid.

      call vhses(nlat,nlon,0,1,v,w,nlat,nlon,br,bi,cr,ci, &
                 nlat,nlat,wvhses,lvhses,work,lwork,ierror)
      if(ierror .ne. 0) write(*,1003) ierror
 1003  format(' error',i4,' in vhaes')
 
! transpose data.
! minus sign to account for differences
! between mathematical and geophysical spherical coords.

      vgrid = -transpose(v)
      ugrid = transpose(w)

      end subroutine getuv_reg

      subroutine cleanup_reg

! cleanup memory allocations when done with module.

      if (allocated(wshaes)) deallocate(wshaes)
      if (allocated(wshses)) deallocate(wshses)
      if (allocated(wvhaes)) deallocate(wvhaes)
      if (allocated(wvhses)) deallocate(wvhses)
      saved_nlat_reg_grdtospec = -1
      saved_nlon_reg_grdtospec = -1
      saved_nlat_reg_vgrdtospec = -1
      saved_nlon_reg_vgrdtospec = -1
      saved_nlat_reg_spectogrd = -1
      saved_nlon_reg_spectogrd = -1
      saved_nlat_reg_vspectogrd = -1
      saved_nlon_reg_vspectogrd = -1
      lfrst_grdtospec_reg = .TRUE.
      lfrst_spectogrd_reg = .TRUE.
      lfrst_vgrdtospec_reg = .TRUE.
      lfrst_vspectogrd_reg = .TRUE.

      end subroutine cleanup_reg

      subroutine grdtospec_gau(datagrid,dataspec)

! converts gridded input array (datagrid) to complex spectral coefficients
! (dataspec).

      real, dimension(:,:), intent(in) :: datagrid
      complex, dimension(:), intent(out) :: dataspec

      real, dimension((4*size(datagrid,1)+2)*size(datagrid,2)) :: work
      double precision, dimension((3*size(datagrid,2)*(size(datagrid,2)+3)+2)/2) :: dwork
      real, dimension(size(datagrid,2),size(datagrid,1)) :: temp
      real, dimension(size(datagrid,2),size(datagrid,2)) :: a,b

      integer nlon,nlat,lwork,ldwork,ntrunc,l1,l2,lshags,ierror,m,n

! compute array dimensions and infer truncation limit
! from size of spectral arrays.


      nlon = size(datagrid,1)
      nlat = size(datagrid,2)
      if (nlon .ne. saved_nlon_gau_grdtospec .or. nlat .ne. saved_nlat_gau_grdtospec) then
          lfrst_grdtospec_gau = .TRUE.
          saved_nlon_gau_grdtospec = nlon
          saved_nlat_gau_grdtospec = nlat
      end if
      lwork = size(work)
      ldwork = size(dwork)
      ntrunc = nint((-1.+sqrt(1+8*float(size(dataspec))))/2.)-1


! initialize work array wshags for spherical harmonic analysis.
! only done when lfrst_grdtospec_gau = .T.

      if (lfrst_grdtospec_gau) then

      if (allocated(wshags)) deallocate(wshags)

      if (mod(nlon,2) .eq. 0) then
         l1 = min0(nlat,(nlon+2)/2) 
      else
         l1 = min0(nlat,(nlon+1)/2) 
      end if
      if (mod(nlat,2) .eq. 0) then
         l2 = nlat/2      
      else
         l2 = (nlat+1)/2
      end if

      lshags = nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15

      allocate(wshags(lshags))

      call shagsi(nlat,nlon,wshags,lshags, &
          work,lwork,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,1001) ierror
 1001 format(' error',i4,' in shagsi')
      lfrst_grdtospec_gau = .FALSE.
      else
      lshags = size(wshags,1)
      endif

! transpose data.

      temp = transpose(datagrid)

! spherical harmonic analysis.

      call shags(nlat,nlon,0,1,temp,nlat,nlon,a,b,nlat,nlat, &
                 wshags,lshags,work,lwork,ierror)
      if(ierror .ne. 0) write(*,1003) ierror
 1003  format(' error',i4,' in shags')
 
! fill complex array dataspec with result.

      dataspec = cmplx( 0.5*(/((a(m,n),n=m,ntrunc+1),m=1,ntrunc+1)/), &
                        0.5*(/((b(m,n),n=m,ntrunc+1),m=1,ntrunc+1)/) )
 
      end subroutine grdtospec_gau

      subroutine spectogrd_gau(dataspec,datagrid)
      
! converts complex spectral coefficients (dataspec) to 
! gridded data array (datagrid).


      real, dimension(:,:), intent(out) :: datagrid
      complex, dimension(:), intent(in) :: dataspec

      real, dimension((4*size(datagrid,1)+2)*size(datagrid,2)) :: work
      double precision, dimension((3*size(datagrid,2)*(size(datagrid,2)+3)+2)/2) :: dwork
      real, dimension(size(datagrid,2),size(datagrid,1)) :: temp
      real, dimension(size(datagrid,2),size(datagrid,2)) :: a,b

      integer nlon,nlat,lwork,ldwork,ntrunc,l1,l2,lshsgs,ierror,m,n,nn,i
      
! compute array dimensions and infer truncation limit
! from size of dataspec.

      nlon = size(datagrid,1)
      nlat = size(datagrid,2)
      if (nlon .ne. saved_nlon_gau_spectogrd .or. nlat .ne. saved_nlat_gau_spectogrd) then
          lfrst_spectogrd_gau = .TRUE.
          saved_nlon_gau_spectogrd = nlon
          saved_nlat_gau_spectogrd = nlat
      end if
      lwork = size(work)
      lwork = size(work)
      ldwork = size(dwork)
      ntrunc = nint((-1.+sqrt(1+8*float(size(dataspec))))/2.)-1

! compute work array wshsgs for spherical harmonic synthesis.
! only done when lfrst_spectogrd_gau = .T.

      if (lfrst_spectogrd_gau) then

      if (allocated(wshsgs)) deallocate(wshsgs)

      if (mod(nlon,2) .eq. 0) then
         l1 = min0(nlat,(nlon+2)/2) 
      else
         l1 = min0(nlat,(nlon+1)/2) 
      end if
      if (mod(nlat,2) .eq. 0) then
         l2 = nlat/2        
      else
         l2 = (nlat+1)/2    
      end if

      lshsgs = nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15

      allocate(wshsgs(lshsgs))

      call shsgsi(nlat,nlon,wshsgs,lshsgs,work,lwork, &
                  dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,1001) ierror
 1001 format(' error',i4,' in shsgsi')
      lfrst_spectogrd_gau = .FALSE.
      else
      lshsgs = size(wshsgs,1)
      endif

! fill two real arrays (a,b) with contents of dataspec.

      a = 0.
      b = 0.
      do m=1,ntrunc+1
      do n=m,ntrunc+1
         nn = sum((/(i,i=ntrunc+1,ntrunc-m+3,-1)/))+n-m+1
         a(m,n) = 2.*real(dataspec(nn))
         b(m,n) = 2.*aimag(dataspec(nn))
      enddo
      enddo

! spherical harmonic synthesis.

      call shsgs(nlat,nlon,0,1,temp,nlat,nlon,a,b,nlat,nlat, &
                 wshsgs,lshsgs,work,lwork,ierror)
      if(ierror .ne. 0) write(*,1003) ierror
 1003  format(' error',i4,' in shsgs')

! transpose data.

      datagrid = transpose(temp)
 
      end subroutine spectogrd_gau

      subroutine getgrad_gau(dataspec,gradx,grady,rsphere)
      
! given spectral coefficients compute gridded vector gradient.
! rsphere is the radius of the sphere in meters.

      real, intent(in) :: rsphere
      real, dimension(:,:), intent(out) :: grady,gradx
      complex, dimension(:), intent(in) :: dataspec

      real, dimension((4*size(grady,1)+2)*size(grady,2)) :: work
      double precision, dimension((3*size(grady,2)*(size(grady,2)+3)+2)/2) :: dwork
      real, dimension(size(grady,2),size(grady,1)) :: v,w
      real, dimension(size(grady,2),size(grady,2)) :: br,bi,cr,ci,a,b
      real, dimension(size(grady,2)) :: sqnn

      real fn
      integer nlon,nlat,lwork,ldwork,ntrunc,l1,l2,lvhsgs,ierror,m,n,nn,i
      
! compute array dimensions and infer truncation limit
! from size of spectral arrays.

      nlon = size(grady,1)
      nlat = size(grady,2)
      if (nlon .ne. saved_nlon_gau_vspectogrd .or. nlat .ne. saved_nlat_gau_vspectogrd) then
          lfrst_vspectogrd_gau = .TRUE.
          saved_nlon_gau_vspectogrd = nlon
          saved_nlat_gau_vspectogrd = nlat
      end if
      lwork = size(work)
      ldwork = size(dwork)
      ntrunc = nint((-1.+sqrt(1+8*float(size(dataspec))))/2.)-1
      
! if not already done, compute work arrays for vector spherical
! harmonic synthesis.

      if (lfrst_vspectogrd_gau) then

      if (allocated(wvhsgs)) deallocate(wvhsgs)

      if (mod(nlon,2) .eq. 0) then
         l1 = min0(nlat,nlon/2) 
      else
         l1 = min0(nlat,(nlon+1)/2)
      end if
      if (mod(nlat,2) .eq. 0) then
         l2 = nlat/2      
      else
         l2 = (nlat+1)/2    
      end if

      lvhsgs =  l1*l2*(nlat+nlat-l1+1)+nlon+15+2*nlat

      allocate(wvhsgs(lvhsgs))

      call vhsgsi(nlat,nlon,wvhsgs,lvhsgs,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,1001) ierror
 1001 format(' error',i4,' in vhsgsi')
      lfrst_vspectogrd_gau = .FALSE.
      else
      lvhsgs = size(wvhsgs,1)
      end if

! multiply spectral coefficients of vorticity and divergence
! by appropriate factors to convert them into vector harmonic
! coefficients of winds.

      do n=2,nlat
         fn = float(n-1)
         sqnn(n) = sqrt(fn*(fn+1.))/rsphere
      enddo

      a = 0.
      b = 0.
      br = 0.
      bi = 0.
      do m=1,ntrunc+1
      do n=m,ntrunc+1
         nn = sum((/(i,i=ntrunc+1,ntrunc-m+3,-1)/))+n-m+1
         a(m,n) = -2.*real(dataspec(nn))
         b(m,n) = -2.*aimag(dataspec(nn))
      enddo
      enddo
      do n=1,nlat
      br(:,n) = a(:,n)*sqnn(n)
      bi(:,n) = b(:,n)*sqnn(n)
      enddo

      cr = 0.
      ci = 0.
      
! compute vector harmonic synthesis to get winds on grid.

      call vhsgs(nlat,nlon,0,1,v,w,nlat,nlon,br,bi,cr,ci, &
                 nlat,nlat,wvhsgs,lvhsgs,work,lwork,ierror)
      if(ierror .ne. 0) write(*,1003) ierror
 1003  format(' error',i4,' in vhags')
 
! transpose data.
! minus sign to account for differences
! between mathematical and geophysical spherical coords.

      grady = transpose(v)
      gradx = -transpose(w)

      end subroutine getgrad_gau

      subroutine specsmooth_gau(datagrid,smooth)
      
! isotropic spectral smoothing of gridded data (datagrid).
! smoothing is a function of total wavenumber only, as specified
! by (smooth(n),n=1,nlat).

      real, dimension(:,:), intent(inout) :: datagrid
      real, dimension(:), intent(in) :: smooth

      real, dimension((4*size(datagrid,1)+2)*size(datagrid,2)) :: work
      double precision, dimension((3*size(datagrid,2)*(size(datagrid,2)+3)+2)/2) :: dwork
      real, dimension(size(datagrid,2),size(datagrid,1)) :: temp
      real, dimension(size(datagrid,2),size(datagrid,2)) :: a,b

      integer nlon,nlat,lwork,ldwork,l1,l2,lshags,lshsgs,ierror,j
      
! compute array dimensions.


      nlon = size(datagrid,1)
      nlat = size(datagrid,2)
      if (nlon .ne. saved_nlon_gau_spectogrd .or. &
          nlat .ne. saved_nlat_gau_spectogrd .or. &
          nlon .ne. saved_nlon_gau_grdtospec .or. &
          nlat .ne. saved_nlat_gau_grdtospec) then
          lfrst_grdtospec_gau = .TRUE.
          lfrst_spectogrd_gau = .TRUE.
          saved_nlon_gau_spectogrd = nlon
          saved_nlat_gau_spectogrd = nlat
          saved_nlon_gau_grdtospec = nlon
          saved_nlat_gau_grdtospec = nlat
      end if
      lwork = size(work)
      lwork = size(work)
      ldwork = size(dwork)
      
! if not already done, initialized work arrays for spherical
! harmonic synthesis and analysis.

      if (lfrst_grdtospec_gau) then

      if (allocated(wshags)) deallocate(wshags)

      if (mod(nlon,2) .eq. 0) then
         l1 = min0(nlat,(nlon+2)/2) 
      else
         l1 = min0(nlat,(nlon+1)/2) 
      end if
      if (mod(nlat,2) .eq. 0) then
         l2 = nlat/2      
      else
         l2 = (nlat+1)/2
      end if

      lshags = nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15

      allocate(wshags(lshags))

      call shagsi(nlat,nlon,wshags,lshags, &
          work,lwork,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,1001) ierror
 1001 format(' error',i4,' in shagsi')
      lfrst_grdtospec_gau = .FALSE.
      else
      lshags = size(wshags,1)
      end if
      if (lfrst_spectogrd_gau) then

      if (allocated(wshsgs)) deallocate(wshsgs)

      if (mod(nlon,2) .eq. 0) then
         l1 = min0(nlat,(nlon+2)/2) 
      else
         l1 = min0(nlat,(nlon+1)/2) 
      end if
      if (mod(nlat,2) .eq. 0) then
         l2 = nlat/2        
      else
         l2 = (nlat+1)/2    
      end if

      lshsgs = nlat*(3*(l1+l2)-2)+(l1-1)*(l2*(2*nlat-l1)-3*l1)/2+nlon+15

      allocate(wshsgs(lshsgs))

      call shsgsi(nlat,nlon,wshsgs,lshsgs,work,lwork, &
        dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,1002) ierror
 1002 format(' error',i4,' in shsgsi')
      lfrst_spectogrd_gau = .FALSE.
      else
      lshsgs = size(wshsgs,1)
      endif
      
! transpose data.

      temp = transpose(datagrid)
      
! perform spherical harmonic analysis.

      call shags(nlat,nlon,0,1,temp,nlat,nlon,a,b,nlat,nlat, &
                 wshags,lshags,work,lwork,ierror)
      if(ierror .ne. 0) write(*,1003) ierror
 1003  format(' error',i4,' in shags')
 
! multiply spectral coefficients by smoothing factor
! (a function of degree only).

      do j=1,nlat
         a(:,j) = smooth(j)*a(:,j)
         b(:,j) = smooth(j)*b(:,j)
      enddo
      
! perform spherical harmonic synthesis.

      call shsgs(nlat,nlon,0,1,temp,nlat,nlon,a,b,nlat,nlat, &
                 wshsgs,lshsgs,work,lwork,ierror)
      if(ierror .ne. 0) write(*,1004) ierror
 1004  format(' error',i4,' in shsgs')
 
! transpose data.

      datagrid = transpose(temp)
 
      end subroutine specsmooth_gau

      subroutine getvrtdivspec_gau(ugrid,vgrid,vrtspec,divspec,rsphere)
      
! calculate spectral coefficients of vorticity and divergence
! (vrtspec,divspec) given input gridded winds (ugrid,vgrid).
! rsphere is the radius of the sphere in meters.

      real, intent(in) :: rsphere
      real, dimension(:,:), intent(in) :: ugrid,vgrid
      complex, dimension(:), intent(out) :: vrtspec,divspec

      real, dimension((4*size(ugrid,1)+2)*size(ugrid,2)) :: work
      double precision, dimension((3*size(ugrid,2)*(size(ugrid,2)+3)+2)/2) :: dwork
      real, dimension(size(ugrid,2),size(ugrid,1)) :: v,w
      real, dimension(size(ugrid,2),size(ugrid,2)) :: br,bi,cr,ci,a,b
      real, dimension(size(ugrid,2)) :: sqnn

      real fn
      integer nlon,nlat,lwork,ldwork,ntrunc,l1,l2,lvhags,ierror,m,n
      
! compute array dimensions and infer truncation limit
! from size of spectral arrays.

      nlon = size(ugrid,1)
      nlat = size(ugrid,2)
      if (nlon .ne. saved_nlon_gau_vgrdtospec .or. nlat .ne. saved_nlat_gau_vgrdtospec) then
          lfrst_vgrdtospec_gau = .TRUE.
          saved_nlon_gau_vgrdtospec = nlon
          saved_nlat_gau_vgrdtospec = nlat
      end if
      lwork = size(work)
      ldwork = size(dwork)
      ntrunc = nint((-1.+sqrt(1+8*float(size(vrtspec))))/2.)-1
      
! if not already done, compute work arrays for vector spherical
! harmonic analysis and scalar spherical harmonic synthesis.

      if (lfrst_vgrdtospec_gau) then

      if (allocated(wvhags)) deallocate(wvhags)

      if (mod(nlon,2) .eq. 0) then
         l1 = min0(nlat,nlon/2) 
      else
         l1 = min0(nlat,(nlon+1)/2)
      end if
      if (mod(nlat,2) .eq. 0) then
         l2 = nlat/2      
      else
         l2 = (nlat+1)/2    
      end if

      lvhags = (nlat+1)*(nlat+1)*nlat/2 + nlon + 15

      allocate(wvhags(lvhags))

      call vhagsi(nlat,nlon,wvhags,lvhags,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,1001) ierror
 1001 format(' error',i4,' in vhagsi')
      lfrst_vgrdtospec_gau = .FALSE.
      else
      lvhags = size(wvhags,1)
      endif
      
! transpose data.
! minus sign to account for difference between
! mathematical and geophysical spherical coords.

      v = -transpose(vgrid)
      w = transpose(ugrid)

! calculate vector spherical harmonic analysis.

      call vhags(nlat,nlon,0,1,v,w,nlat,nlon,br,bi,cr,ci, &
                 nlat,nlat,wvhags,lvhags,work,lwork,ierror)
      if(ierror .ne. 0) write(*,1003) ierror
 1003  format(' error',i4,' in vhags')

! multiply vector harmonic coefficients of winds by 
! appropriate factors to convert into vorticity and
! divergence coefficients.

      do n=1,nlat
      fn = float(n-1)
      sqnn(n) = sqrt(fn*(fn+1.))
      enddo

      a = 0.
      b = 0.
      do n=1,nlat
      a(:,n) = -(sqnn(n)/rsphere)*br(:,n)
      b(:,n) = -(sqnn(n)/rsphere)*bi(:,n)
      enddo
      divspec = cmplx( 0.5*(/((a(m,n),n=m,ntrunc+1),m=1,ntrunc+1)/), &
                       0.5*(/((b(m,n),n=m,ntrunc+1),m=1,ntrunc+1)/) )

      do n=1,nlat
      a(:,n) = (sqnn(n)/rsphere)*cr(:,n)
      b(:,n) = (sqnn(n)/rsphere)*ci(:,n)
      enddo
      vrtspec = cmplx( 0.5*(/((a(m,n),n=m,ntrunc+1),m=1,ntrunc+1)/), &
                       0.5*(/((b(m,n),n=m,ntrunc+1),m=1,ntrunc+1)/) )

      end subroutine getvrtdivspec_gau

      subroutine getuv_gau(vrtspec,divspec,ugrid,vgrid,rsphere)
      
! given spectral coefficients of vorticity and divergence
! (vrtspec,divspec) compute gridded winds (ugrid,vgrid).
! rsphere is the radius of the sphere in meters.

      real, intent(in) :: rsphere
      real, dimension(:,:), intent(out) :: ugrid,vgrid
      complex, dimension(:), intent(in) :: vrtspec,divspec

      real, dimension((4*size(ugrid,1)+2)*size(ugrid,2)) :: work
      double precision, dimension((3*size(ugrid,2)*(size(ugrid,2)+3)+2)/2) :: dwork
      real, dimension(size(ugrid,2),size(ugrid,1)) :: v,w
      real, dimension(size(ugrid,2),size(ugrid,2)) :: br,bi,cr,ci,a,b
      real, dimension(size(ugrid,2)) :: sqnn

      real fn
      integer nlon,nlat,lwork,ldwork,ntrunc,l1,l2,lvhsgs,ierror,m,n,nn,i
      
! compute array dimensions and infer truncation limit
! from size of spectral arrays.

      nlon = size(ugrid,1)
      nlat = size(ugrid,2)
      if (nlon .ne. saved_nlon_gau_vspectogrd .or. nlat .ne. saved_nlat_gau_vspectogrd) then
          lfrst_vspectogrd_gau = .TRUE.
          saved_nlon_gau_vspectogrd = nlon
          saved_nlat_gau_vspectogrd = nlat
      end if
      lwork = size(work)
      ldwork = size(dwork)
      ntrunc = nint((-1.+sqrt(1+8*float(size(vrtspec))))/2.)-1
      
! if not already done, compute work arrays for vector spherical
! harmonic synthesis.

      if (lfrst_vspectogrd_gau) then

      if (allocated(wvhsgs)) deallocate(wvhsgs)

      if (mod(nlon,2) .eq. 0) then
         l1 = min0(nlat,nlon/2) 
      else
         l1 = min0(nlat,(nlon+1)/2)
      end if
      if (mod(nlat,2) .eq. 0) then
         l2 = nlat/2      
      else
         l2 = (nlat+1)/2    
      end if

      lvhsgs =  l1*l2*(nlat+nlat-l1+1)+nlon+15+2*nlat

      allocate(wvhsgs(lvhsgs))

      call vhsgsi(nlat,nlon,wvhsgs,lvhsgs,dwork,ldwork,ierror)
      if(ierror .ne. 0) write(*,1001) ierror
 1001 format(' error',i4,' in vhsgsi')
      lfrst_vspectogrd_gau = .FALSE.
      else
      lvhsgs = size(wvhsgs,1)
      end if

! multiply spectral coefficients of vorticity and divergence
! by appropriate factors to convert them into vector harmonic
! coefficients of winds.

      sqnn(1) = 0.
      do n=2,nlat
      fn = float(n-1)
      sqnn(n) = rsphere/sqrt(fn*(fn+1.))
      enddo

      a = 0.
      b = 0.
      br = 0.
      bi = 0.
      do m=1,ntrunc+1
      do n=m,ntrunc+1
         nn = sum((/(i,i=ntrunc+1,ntrunc-m+3,-1)/))+n-m+1
         a(m,n) = -2.*real(divspec(nn))
         b(m,n) = -2.*aimag(divspec(nn))
      enddo
      enddo
      do n=1,nlat
      br(:,n) = sqnn(n)*a(:,n)
      bi(:,n) = sqnn(n)*b(:,n)
      enddo

      cr = 0.
      ci = 0.
      do m=1,ntrunc+1
      do n=m,ntrunc+1
         nn = sum((/(i,i=ntrunc+1,ntrunc-m+3,-1)/))+n-m+1
         a(m,n) = 2.*real(vrtspec(nn))
         b(m,n) = 2.*aimag(vrtspec(nn))
      enddo
      enddo
      do n=1,nlat
      cr(:,n) = sqnn(n)*a(:,n)
      ci(:,n) = sqnn(n)*b(:,n)
      enddo
      
! compute vector harmonic synthesis to get winds on grid.

      call vhsgs(nlat,nlon,0,1,v,w,nlat,nlon,br,bi,cr,ci, &
                 nlat,nlat,wvhsgs,lvhsgs,work,lwork,ierror)
      if(ierror .ne. 0) write(*,1003) ierror
 1003  format(' error',i4,' in vhags')
 
! transpose data.
! minus sign to account for differences
! between mathematical and geophysical spherical coords.

      vgrid = -transpose(v)
      ugrid = transpose(w)

      end subroutine getuv_gau

      subroutine cleanup_gau

! cleanup memory allocations when done with module.

      if (allocated(wshags)) deallocate(wshags)
      if (allocated(wshsgs)) deallocate(wshsgs)
      if (allocated(wvhags)) deallocate(wvhags)
      if (allocated(wvhsgs)) deallocate(wvhsgs)
      saved_nlat_gau_grdtospec = -1
      saved_nlon_gau_grdtospec = -1
      saved_nlat_gau_vgrdtospec = -1
      saved_nlon_gau_vgrdtospec = -1
      saved_nlat_gau_spectogrd = -1
      saved_nlon_gau_spectogrd = -1
      saved_nlat_gau_vspectogrd = -1
      saved_nlon_gau_vspectogrd = -1
      lfrst_grdtospec_gau = .TRUE.
      lfrst_spectogrd_gau = .TRUE.
      lfrst_vgrdtospec_gau = .TRUE.
      lfrst_vspectogrd_gau = .TRUE.

      end subroutine cleanup_gau

! that's it!

      end module sphere
