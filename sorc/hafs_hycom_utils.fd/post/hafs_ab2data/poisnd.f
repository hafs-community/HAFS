      subroutine poisnd(m,n,rhs,soln,du,dv,res)
      use mod_plot  ! HYCOM plot array interface
c
      real rhs(m-1,*),soln(m-1,*),du(m-1,*),dv(m-1,*),res(*)
c
c --- vectorizable version of s.o.r. poisson solver.
c
c --- the exact form of the equation being solved is
c
c              div [(1/p) grad soln] = rhs/(mesh size squared)
c
c --- where -p- is the layer thickness, -rhs- is the vorticity,
c --- and -soln- is the mass flux stream function
c
c --- m,n  = dimensions of -rhs,soln- in calling program
c --- du   = inverse layer thickness at (i,j+1/2) (u point)
c --- dv   = inverse layer thickness at (i+1/2,j) (v point)
c --- res  = work space of size m*n
c
c --- errmin should represent a number equivalent to [10**(-3) sv]**2
      data rfc/1.7/,itrmax/1500/,errmin/1.e-6/,errmax/1.e37/
c
      mn2=m+n-2
      m1=m-1
      do 10 iter=1,itrmax
      errmx=0.
      do 1 i=4,mn2
      err=0.
      nd=i-1
      if (nsec(nd).eq.0) go to 1
      nptsd=0
      do 4 ns=1,nsec(nd)
      l=i-m
      k1=(m*jfd(nd,ns)+ifd(nd,ns)-l-1)/m1
      k2=(m*jld(nd,ns)+ild(nd,ns)-l-1)/m1
ccc      write (*,'('' nd,ns,ifd,jfd,ild,jld,k1,k2=''8i4)') nd,ns,
ccc     .  ifd(nd,ns),jfd(nd,ns),ild(nd,ns),jld(nd,ns),k1,k2
      nptsd=nptsd+k2-k1+1
      iy1=ifd(nd,ns)+k1
      do 2 k=k1,k2
c --- neighbors i-1,j and i+1,j are at l-1,k and l+1,k respectively
c --- neighbors i,j-1 and i,j+1 are at l-m,k and l+m,k respectively
c
 2    res(k)=(soln(l+1,k)*dv(l,k)+soln(l-1,k)*dv(l-1,k)
     .       +soln(l+m,k)*du(l,k)+soln(l-m,k)*du(l-m,k) - rhs(l,k))
     .      /(            dv(l,k)+            dv(l-1,k) 
     .       +            du(l,k)+            du(l-m,k)) - soln(l,k)
      do 3 k=k1,k2
      err=err+res(k)**2
 3    soln(l,k)=soln(l,k)+rfc*res(k)
 4    continue
      err=err/nptsd
      errmx=amax1(errmx,err)
      if (err.gt.errmax) go to 20
 1    continue
      if (iter.lt.25.or.errmx.gt.errmin) go to 10
      write (*,900) iter,errmx
 900  format(' s.o.r. converged at',i4,' iterations -- errmx =',1pe11.3)
      return
 10   continue
 20   write (*,901) iter,errmx
 901  format(' warning - errmx exceeded in s.o.r. routine - iter=',i4,
     .'  errmx=',1pe10.3)
      return
      end
