      subroutine indxi(ipt,if,il,is)
      use mod_plot  ! HYCOM plot array interface
c
      dimension ipt(ii,jj),if(jj,ms),il(jj,ms),is(jj)
c
      common/linepr/lp
c
c --- input array ipt contains 1 at grid point locations, 0 elsewhere
c --- output is arrays if, il, is  where
c --- if(j,k) gives row index of first point in column j for k-th section
c --- il(j,k) gives row index of last point
c --- is(j) gives number of sections in column j (maximum: ms)
c
      write (lp,'(a,6i6)') 
     .   'indxi called with ii,jj,ii1,jj1 =',
     .                      ii,jj,ii1,jj1
      do 1 j=1,jj
      is(j)=0
      do 4 k=1,ms
      if(j,k)=0
 4    il(j,k)=0
      i=1
      k=1
 3    if (ipt(i,j).ne.0) go to 2
      i=i+1
      if (i.le.ii) go to 3
      go to 1
 2    if (k.gt.ms) then
      write (lp,'('' error in indxi - ms too small at i,j ='',2i5)') i,j
      write (lp,'('' j-th line of ipt array:'',/(7(1x,10i1)))')
     .   (ipt(l,j),l=1,ii)
      stop '(indxi)'
      end if
      if(j,k)=i
 6    i=i+1
      if (i.le.ii) go to 5
      il(j,k)=ii
      is(j)=k
      go to 1
 5    if (ipt(i,j).ne.0) go to 6
      il(j,k)=i-1
      is(j)=k
      k=k+1
      go to 3
 1    continue
      return
      end
