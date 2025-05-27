!-----------------------------------------------------------------------+
! This program performs the Fourier decomposition of a scalar in the
!   Cartesian coordinates.  The procedure consists of three steps: the
!   first one is to transfer the scalar from the Cartesian coordinates
!   to the cylindrical coordinates; the second step is to calculate the
!   coefficients for a given Fourier component; the final step is to
!   transfer the scalar from the cylindrical to Cartesian coordinates
!   for graphic purpose.
! Authors and history:
!      -- 202410, JungHoon Shin created the f77 subroutines
!      -- 202411, Yonghui Weng changed the format to f90
!-----------------------------------------------------------------------+
  subroutine decom(fi,fir,mm,nn,lm,mc,uvt)
!      parameter(lm=141,ln=360)
!      parameter(lm=141,ln=180)   !orig
!      parameter(lm=901,ln=360)
  parameter(ln=360)
  real fi(mm,nn),fir(mm,nn)
  real fd(2*mm-1,2*nn-1),fdr(2*mm-1,2*nn-1)
  real fi0(2*mm-1,2*nn-1),fi1(2*mm-1,2*nn-1)
  real fi2(2*mm-1,2*nn-1),fi3(2*mm-1,2*nn-1)
  real fi4(2*mm-1,2*nn-1),fi5(2*mm-1,2*nn-1)
  real fi6(2*mm-1,2*nn-1)

  real fs(lm)
  real ft0(lm,ln),ft(lm,ln),fts(lm,ln),fta(lm,ln)
  real rst1(lm,ln),rst2(lm,ln),rst3(lm,ln)
  real rst4(lm,ln),rst5(lm,ln),rst6(lm,ln)
  real x1(2*mm-1),x2(2*nn-1)
  integer uvt,mm2,nn2
!-----------------------------------------------------
  if ( abs(mc) > 99 ) then
     write(*,*)' !!!! warnning: mc=',mc,'  is > 99 or < -99'
     stop "wave-number should be [-99 99] "
  endif

  mm2=2*mm-1
  nn2=2*nn-1
  j0=mm
  i0=nn
  dd=360.0/float(ln)

  fd(1:mm2:2,  1:nn2:2  )=fi(1:mm,1:nn)
!  fd(2:mm2+1:2,2:nn2+1:2)=0.25*(fi(1:mm-1,1:nn-1)+fi(2:mm,1:nn-1)+fi(1:mm-1,2:nn)+fi(2:mm,2:nn))
  fd(2:mm2:2,2:nn2:2)=0.25*(fi(1:mm-1,1:nn-1)+fi(2:mm,1:nn-1)+fi(1:mm-1,2:nn)+fi(2:mm,2:nn))
!  fd(2:mm2+1:2,1:nn2+1:2)=0.5*(fi(1:mm-1,1:nn)+fi(2:mm,1:nn))
  fd(2:mm2:2,1:nn2:2)=0.5*(fi(1:mm-1,1:nn)+fi(2:mm,1:nn))
!  fd(1:mm2:2,  2:nn2+1:2)=0.5*(fi(1:mm,1:nn-1)+fi(1:mm,2:nn))
  fd(1:mm2:2,  2:nn2:2)=0.5*(fi(1:mm,1:nn-1)+fi(1:mm,2:nn))

  fd(i0+1,j0+1) = fd(i0+1,j0+1)*0.4+fd(i0+2,j0+2)*0.6
  fd(i0+1,j0  ) = fd(i0+1,j0  )*0.4+fd(i0+2,j0  )*0.6
  fd(i0+1,j0-1) = fd(i0+1,j0-1)*0.4+fd(i0+2,j0-2)*0.6
  fd(i0-1,j0+1) = fd(i0-1,j0+1)*0.4+fd(i0-2,j0+2)*0.6
  fd(i0-1,j0  ) = fd(i0-1,j0  )*0.4+fd(i0-2,j0  )*0.6
  fd(i0-1,j0-1) = fd(i0-1,j0-1)*0.4+fd(i0-2,j0-2)*0.6
  fd(i0,j0+1  ) = fd(i0,j0+1  )*0.4+fd(i0,j0+2  )*0.6
  fd(i0,j0-1  ) = fd(i0,j0-1  )*0.4+fd(i0,j0-2  )*0.6

  ! angle is measued from due north (j0,i0) the center of frame
  pi1=atan(1.0)/45.0
  do i=1,lm
  do j=1,ln
     phi=real(j-1)*dd
     x=real(j0)-(i-1)*sin(phi*pi1)
     y=real(i0)+(i-1)*cos(phi*pi1)
     call scinex(x,y,fd,scint0,mm2,nn2)
     ft0(i,j)=scint0
     ft(i,j)=scint0
  enddo
  enddo

  ft0(1,1:ln)=fd(j0,i0)
  ft(1,1:ln)=fd(j0,i0)

  do i=1,lm
  do j=1,ln
     ftsmooth=0.0
     ! this few lines is for rotating angle of fields
     do jr=-2,3
        j00=j+jr
        if(j00.le.0)j00=ln+j00
        if(j00.gt.ln)j00=j00-ln
        ftsmooth=ftsmooth+ft0(i,j00)
     end do
     ft(i,j)=ftsmooth/6.0
  end do
  end do

  if ( mc>=0 ) then   ! sum(0:mc)
     !---mc==0: wavenumber 0
     call transf000(fts,fs,ft,lm,ln)              ! get the symmetric component
     call transi(fdr,fts,lm,ln,mm2,nn2)           ! tansform back to Cartesian Coordinate
     if ( mc > 0 ) then
        call transf0(fta,fs,ft,lm,ln)             ! get the asymmetric component
        do n = 1, mc
           call transf(rst1,fta,real(n),lm,ln)   ! take the  wave number n component
           call transi(fi1,rst1,lm,ln,mm2,nn2)    ! tansform back to Cartesian Coordinate
           fdr(1:mm2,1:nn2)=fdr(1:mm2,1:nn2)+fi1(1:mm2,1:nn2)
        enddo
     endif  !if ( mc > 0 ) then
  else if ( mc<0 ) then ! just the mcth wave cut-off
     call transf0(fta,fs,ft,lm,ln)
     call transf(rst1,fta,real(abs(mc)),lm,ln)   ! take the  wave number n component
     call transi(fdr,rst1,lm,ln,mm2,nn2)    ! tansform back to Cartesian Coordinate
  endif

  do i=1,mm
  do j=1,nn
     fir(i,j)=fdr(2*i-1,2*j-1)
  end do
  end do

  return
  end subroutine decom

!-----------------------------------------------------------------------+
  subroutine scinex(gm,gn,scala,scinto,lq,lp)
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! this subroutine produces the value scinto of a scalar field at a point
! gm,gn by interpolation or extrapolation of the field scala  (2-directi
! bessel interpolation formula). mmin,mmax and nmin,nmax are the boundar
! of the grid array.
!---------------------------
      real scala(lq,lp)
!     print *,'in scinex gm,gn,scala(2,1)',gm,gn,scala(2,1)
      mmin=1
      nmin=1
      mmax=lq
      nmax=lp
      igm=int(gm)
      jgn=int(gn)
      fm=gm-igm
      fn=gn-jgn
      if(fm.lt.1.e-06)fm=0.
      if(fn.lt.1.e-06)fn=0.
      ms=mmax-1
      ns=nmax-1
      mr=mmin+1
      nr=nmin+1

      !if(gm.lt.mmax)go+to 60
      if ( gm >= mmax ) then
         !if(gn.lt.nmax)go+to 20
         if ( gn >= nmax ) then
            e=gm-mmax
            t1=e*(scala(mmax,nmax)-scala(ms,nmax))
            e=gn-nmax
            t2=e*(scala(mmax,nmax)-scala(mmax,ns))
            scinto=scala(mmax,nmax)+t1+t2
            return
         elseif ( gn < nmax ) then
            ! 20 if(gn.ge.nmin)go+to 40
            if ( gn < nmin ) then
               e=gm-mmax
               t1=e*(scala(mmax,nmin)-scala(ms,nmin))
               e=nmin-gn
               t2=e*(scala(mmax,nmin)-scala(mmax,nr))
               scinto=scala(mmax,nmin)+t1+t2
               return
            else
               !40 p=scala(mmax,jgn)+fn*(scala(mmax,jgn+1)-scala(mmax,jgn))
               p=scala(mmax,jgn)+fn*(scala(mmax,jgn+1)-scala(mmax,jgn))
               h=scala(ms,jgn)+fn*(scala(ms,jgn+1)-scala(ms,jgn))
               e=gm-mmax
               scinto=p+e*(p-h)
               return
            endif
         endif  !if ( gn >= nmax ) then
      else if ( gm < mmax ) then
         !60 if(gm.ge.mmin)go+to 140
         if ( gm < mmin ) then
            !if(gn.lt.nmax)go+to 80
            if ( gn >= nmax ) then
               e=gn-nmax
               t2=e*(scala(mmin,nmax)-scala(mmin,ns))
               e=mmin-gm
               t1=e*(scala(mmin,nmax)-scala(mr,nmax))
               scinto=scala(mmin,nmax)+t1+t2
               return
            else if ( gn < nmax ) then
               ! 80 if(gn.ge.nmin)go+to 100
               if ( gn < nmin ) then
                  e=nmin-gn
                  t2=e*(scala(mmin,nmin)-scala(mmin,nr))
                  e=mmin-gm
                  t1=e*(scala(mmin,nmin)-scala(mr,nmin))
                  scinto=scala(mmin,nmin)+t1+t2
                  return
               else if ( gn >= nmin ) then
                  ! 100 e=mmin-gm
                  e=mmin-gm
                  p=scala(mmin,jgn)+fn*(scala(mmin,jgn+1)-scala(mmin,jgn))
                  h=scala(mr,jgn)+fn*(scala(mr,jgn+1)-scala(mr,jgn))
                  scinto=p+e*(p-h)
                  return
               endif
            endif
         else if ( gm >= mmin ) then
            !140 if(gn.ge.nmax)go+to 120
            if ( gn < nmax ) then
               !if(gn.ge.nmin)go+to 160
               if ( gn < nmin ) then
                  e=nmin-gn
                  p=scala(igm,nmin)+fm*(scala(igm+1,nmin)-scala(igm,nmin))
                  h=scala(igm,nr)+fm*(scala(igm+1,nr)-scala(igm,nr))
                  scinto=p+e*(p-h)
                  return
               else if ( gn >= nmin ) then
                  !160 if(gm.lt.ms.and.gm.ge.mr.and.gn.lt.ns.and.gn.ge.nr)go+to 180
                  if ( gm.lt.ms.and.gm.ge.mr.and.gn.lt.ns.and.gn.ge.nr ) then
                     !180    fq=0.25*(fm*fm-fm)
                     fq=0.25*(fm*fm-fm)
                     a=scala(igm,jgn-1)+fm*(scala(igm+1,jgn-1)-scala(igm,jgn-1))       &
                       +fq*(scala(igm+2,jgn-1)+scala(igm-1,jgn-1)-scala(igm+1,jgn-1)-  &
                       scala(igm,jgn-1))
                     b=scala(igm,jgn)+fm*(scala(igm+1,jgn)-scala(igm,jgn))             &
                       +fq*(scala(igm+2,jgn)+scala(igm-1,jgn)-scala(igm+1,jgn)-        &
                       scala(igm,jgn))
                     c=scala(igm,jgn+1)+fm*(scala(igm+1,jgn+1)-scala(igm,jgn+1))       &
                       +fq*(scala(igm+2,jgn+1)+scala(igm-1,jgn+1)-scala(igm+1,jgn+1)   &
                       -scala(igm,jgn+1))
                     d=scala(igm,jgn+2)+fm*(scala(igm+1,jgn+2)-scala(igm,jgn+2))       &
                       +fq*(scala(igm+2,jgn+2)+scala(igm-1,jgn+2)-scala(igm+1,jgn+2)   &
                       -scala(igm,jgn+2))
                     scinto=b+fn*(c-b)+0.25*(fn*fn-fn)*(a+d-b-c)
                     return
                  else
                     p=scala(igm+1,jgn)+fn*(scala(igm+1,jgn+1)-scala(igm+1,jgn))
                     h=scala(igm,jgn)+fn*(scala(igm,jgn+1)-scala(igm,jgn))
                     scinto=h+fm*(p-h)
                     return
                  endif
               endif
            else if ( gn >= nmax ) then
               !120 e=gn-nmax
               e=gn-nmax
               p=scala(igm,nmax)+fm*(scala(igm+1,nmax)-scala(igm,nmax))
               h=scala(igm,ns)+fm*(scala(igm+1,ns)-scala(igm,ns))
               scinto=p+e*(p-h)
            endif
         endif   !if ( gm < mmin ) then
      endif   !if ( gm >= mmax ) then
      return
      end subroutine scinex

!-----------------------------------------------------------------------+
	subroutine transf(out,psia,ai,l,m)
  real out(l,m),ps1a(m),psia(l,m)
	pi=4.0*atan(1.0)
	dl=2*pi/float(m)
! to get the component you want
  do j=1,l
     do i=1,m
        ps1a(i)=psia(j,i)
     enddo
     call coscoeff(ps1a,cc1a,m,dl,pi,ai)
     call sincoeff(ps1a,cs1a,m,dl,pi,ai)
     do i=1,m
        ang=float(i-1)*dl
        out(j,i)=cc1a*cos(ang*ai)+cs1a*sin(ang*ai)
     enddo
  enddo
  return
  end subroutine transf

!-----------------------------------------------------------------------+
	subroutine transf0(out,amean,u,lm,ln)

  implicit none
  integer, intent(in) :: lm, ln
  real, intent(in)    :: u(lm,ln)
  real, intent(out)   :: out(lm,ln)
  real                :: amean(lm)
  integer             :: i, j

  do j=1,lm
     amean(j)=sum(u(j,1:ln))/real(ln)
  enddo
	do i=1,ln
     out(1:lm,i)=u(1:lm,i)-amean(1:lm)
  enddo
	return
	end subroutine transf0

!-----------------------------------------------------------------------+
  subroutine transf00(out,amean,u,lm,ln)
  real u(lm,ln),out(lm,ln),amean(lm)
  do j=1,lm
     amean(j)=0.
     do i=1,ln
        amean(j)=amean(j)+u(j,i)
     enddo
     amean(j)=amean(j)/float(ln)
  enddo
  do i=1,ln
  do j=1,lm
        out(j,i)=amean(j)
  enddo
  enddo
  return
  end subroutine transf00

!-----------------------------------------------------------------------+
  subroutine transf000(out,amean,u,lm,ln)
  real u(lm,ln),out(lm,ln),amean(lm)
  do j=1,lm
     amean(j)=0.
     do i=1,ln
        amean(j)=amean(j)+u(j,i)
     enddo
     amean(j)=amean(j)/float(ln)
  enddo

  do i=1,ln
     do j=1,10
        out(j,i)=amean(1)
     enddo
     out(lm,i)=amean(lm)
  enddo

  do ii=1,50
     do i=1,ln
     do j=2,lm-1
        out(j,i)=0.25*amean(j-1)+0.5*amean(j)+0.25*amean(j+1)
     enddo
     enddo
  enddo

  return
  end subroutine transf000

!-----------------------------------------------------------------------+
!  This code is used to transfer vector in Cylindrical to Cartesian
!  cooridinates, originally coded by Liguang Wu (1996.8) and modified
!  by Yuqing Wang (1997.1)
!  lm,ln are grid number in cyl. co., respectively. the thansfering
!  domain is 2000km*2000km for 10km*10km grid.
!*****************************************************************
  subroutine transi(u1,uu,im,in,mm,nn)

  real uu(im,in),u1(mm,nn)
  dd=360.0/float(in)
!*************************************************************
!*************************************************************
! j is x-direction (zonal), i is y-direction (meridional)
! angle is measued from due north,(j0,i0) the center of frame
!*************************************************************
  pi=4.0*atan(1.0)
  pi1=180.0/pi
  m0=(mm+1)/2
  n0=(nn+1)/2
  do j=1,mm
  do i=1,nn
     if (j.eq.m0.and.i.eq.n0) then
        u1(j,i)=uu(1,1)
     else
        x=float(j-m0)
        y=float(i-n0)
        r1=sqrt(x**2+y**2)
        phi=asin(x/r1)
        if(x.le.0..and.y.ge.0.) phi=-phi
        if(x.lt.0..and.y.lt.0.) phi=pi+phi
        if(x.ge.0..and.y.le.0.) phi=pi+phi
        if(x.ge.0..and.y.gt.0.) phi=2*pi-phi
        phi=phi*pi1
        ! (lm,ln) the position in cylindrical coordinates
        ln=int(phi/dd+0.5)+1
        lm=int(r1+0.5)+1
        dr=r1-lm
        da=phi-ln*dd
        mp1=lm+1
        np1=ln+1
        mm1=lm-1
        nm1=ln-1
        if(mp1.gt.im) mp1=im
        if(np1.gt.in) np1=np1-in
        if(mm1.lt.1) mm1=1
        if(nm1.lt.1) nm1=in-nm1
        if(ln.gt.in) ln=ln-in
        if(lm.gt.im) lm=im

        d1=0.5*(uu(mp1,ln)-uu(mm1,ln))
        d2=0.5*(uu(lm,np1)-uu(lm,nm1))/dd
        d3=0.5*(uu(mp1,ln)-2.*uu(lm,ln)+uu(mm1,ln))
        d4=0.5*(uu(lm,np1)-2.*uu(lm,ln)+uu(lm,nm1))/dd/dd
        d5=0.125*(uu(mp1,np1)+uu(mm1,nm1)-uu(mm1,np1)-uu(mp1,nm1))/dd
        u1(j,i)=uu(lm,ln)+d1*dr+d2*da+d3*dr**2+d4*da**2+d5*da*dr
     endif
     phi=phi/pi1
  enddo
  enddo
  return
  end subroutine transi

!-----------------------------------------------------------------------+
  subroutine coscoeff(psi,psim,m,dl,pi,ai)
  dimension psi(m)
  psim=0.0
  do j=1,m
     ang=float(j-1)*dl
     psim=psim+psi(j)*cos(ang*ai)*dl
  enddo
  psim=psim/pi
  return
  end subroutine coscoeff

!-----------------------------------------------------------------------+
  subroutine sincoeff(psi,psim,m,dl,pi,ai)
  dimension psi(m)
  psim=0.0
  do j=1,m
     ang=float(j-1)*dl
     psim=psim+psi(j)*sin(ang*ai)*dl
  enddo
  psim=psim/pi
  return
  end subroutine sincoeff

!-----------------------------------------------------------------------+
