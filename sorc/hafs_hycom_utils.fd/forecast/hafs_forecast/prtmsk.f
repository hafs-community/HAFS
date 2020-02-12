      subroutine prtmsk(mask,array,work,idm,ii,jj,offset,scale,title)
c
c --- Delete 'array' elements outside 'mask'. Then
c --- break 'array' into sections, each 'nchar' characters wide, for printing.
c
      implicit none
      integer    nchar
      parameter (nchar=120)
ccc   parameter (nchar= 76)
ccc   parameter (nchar= 80)
ccc   parameter (nchar=132)
c
      integer   idm,ii,jj
      integer   mask(idm,*)
      real      array(idm,*),work(idm,*)
      real      offset,scale
      character title*(*)
c
      integer        lp
      common/linepr/ lp
      save  /linepr/
c
      integer i,i1,i2,j,n,ncols
c
      real    cvmgp,cvmgz,a,b,c
      integer ic
      cvmgp(a,b,c)=a*(.5+sign(.5,c))+b*(.5-sign(.5,c))
      cvmgz(a,b,ic)=cvmgp(a,b,-1.*iabs(ic))
c
      ncols=nchar/4
      do 1 n=1,ii/ncols+1
      i1=ncols*(n-1)+1
      i2=min0(ncols*n,ii)
      if (i1.gt.i2) go to 1
      write (lp,'(/'' Sec.'',i2,'' (cols'',i4,'' -'',i4,'') -- '',a)')
     .   n,i1,i2,title
ccc      if (i2.lt.i1+5) then
ccc      write (lp,'('' (Not printed. Too few columns. Save paper.)'')')
ccc      go to 1
ccc      end if
      do 2 j=jj,1,-1
      do 3 i=i1,i2
 3    work(i,j)=cvmgz(0.,array(i,j),mask(i,j))
      do 4 i=i1,i2
 4    work(i,j)=cvmgz(0.,(work(i,j)-offset)*scale,mask(i,j))
      write (lp,'(32i4)')        j,(int(work(i,j)),i=i1,i2)
ccc   write (lp,'(i4,1x,75i1)')  j,(int(work(i,j)),i=i1,i2)
ccc   write (lp,'(i4,1x,120i1)') j,(int(work(i,j)),i=i1,i2)
 2    continue
 1    continue
      call flush(lp)
      return
      end
