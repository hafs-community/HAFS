      subroutine indxj(jpt,jf,jl,js)
      use mod_plot  ! HYCOM plot array interface
c
      dimension jpt(ii,jj),jf(ii,ms),jl(ii,ms),js(ii)
c
      common/linepr/lp
c
c --- input array jpt contains 1 at grid point locations, 0 elsewhere
c --- output is arrays jf, jl, js  where
c --- jf(i,k) gives column index of first point in row i for k-th section
c --- jl(i,k) gives column index of last point
c --- js(i) gives number of sections in row i (maximum: ms)
c
c
      write (lp,'(a,6i6)') 
     .    'indxj called with ii,jj,ii1,jj1 =',
     .                       ii,jj,ii1,jj1
      do 1 i=1,ii
      js(i)=0
      do 4 k=1,ms
      jf(i,k)=0
 4    jl(i,k)=0
      j=1
      k=1
 3    if (jpt(i,j).ne.0) go to 2
      j=j+1
      if (j.le.jj) go to 3
      go to 1
 2    if (k.gt.ms) then
      write (lp,'('' error in indxj - ms too small at i,j ='',2i5)') i,j
      write (lp,'('' i-th line of jpt array:'',/(7(1x,10i1)))')
     .   (jpt(i,l),l=1,jj)
      stop '(indxj)'
      end if
      jf(i,k)=j
 6    j=j+1
      if (j.le.jj) go to 5
      jl(i,k)=jj
      js(i)=k
      go to 1
 5    if (jpt(i,j).ne.0) go to 6
      jl(i,k)=j-1
      js(i)=k
      k=k+1
      go to 3
 1    continue
      return
      end
