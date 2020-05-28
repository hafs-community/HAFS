      subroutine sbmerg(depth,film)
      use mod_plot  ! HYCOM plot array interface
c
      real depth(ii,jj),film
c
c --- submerge islands, i.e., cover with thin film of water
c
c
c --- step 1: find the islands.
c --- mark all land points connected by land to grid perimeter by "-1"
c
      integer, allocatable :: mark(:,:)
c
      allocate( mark(ii,jj) )
c
      do 1 j=1,jj1
      do 1 i=1,ii1
 1    mark(i,j)=0
c
      do 2 j=1,jj1
      if (depth(  1,j).eq.0.) mark(  1,j)=-1
 2    if (depth(ii1,j).eq.0.) mark(ii1,j)=-1
c
      do 3 i=1,ii1
      if (depth(i,  1).eq.0.) mark(i,  1)=-1
 3    if (depth(i,jj1).eq.0.) mark(i,jj1)=-1
c
      iter=0
 7    num1=0
c
c --- reverse scanning direction after every iteration
      incr=1-2*mod(iter,2)
      if (incr.lt.0) then
        j1=jj1
        i1=ii1
      else
        j1=1
        i1=1
      end if
      do 10 j=j1,jj-j1,incr
      ja=max(  1,j-1)
      jb=min(jj1,j+1)
      do 10 i=i1,ii-i1,incr
      ia=max(  1,i-1)
      ib=min(ii1,i+1)
c
      if (depth(i,j).eq.0. .and. mark(i,j).eq.0 .and.
     .   min(mark(ia,j ),mark(ib,j ),mark(i ,ja),mark(i ,jb),
     .       mark(ia,ja),mark(ib,jb),mark(ib,ja),mark(ia,jb)).lt.0) then
ccc      write (*,'(6i5,5x,4i2)')
ccc     .   ia,i,ib,ja,j,jb,mark(ia,j),mark(ib,j),mark(i,ja),mark(i,jb)
      mark(i,j)=-1
      num1=num1+1
      end if
 10   continue
      iter=iter+1
ccc      write (*,'('' mainland extended by'',i7,'' pts'')') num1
      if (num1.gt.0) go to 7
c
c --- step 2: cover islands with water
c
      num2=0
      do 5 j=1,jj1
      do 5 i=1,ii1
      if (depth(i,j).eq.0. .and. mark(i,j).eq.0) then
      depth(i,j)=film
      num2=num2+1
ccc      write (*,'('' grid point'',2i4,'' submerged'')') i,j
      end if
 5    continue
      write (*,'(i7,'' island points submerged'')') num2
c
      deallocate( mark )
      return
      end
