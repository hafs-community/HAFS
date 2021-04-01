      subroutine zebra(array,idim,ii,jj)
c
      common/linepr/lp
c
c --- find nice contour interval resulting in 7 to 10 contour lines and
c --- draw contours on line printer through the following set of grid points:
c
c         array( 1, 1) . . . . . . . . .  array( 1,jj)
c              .                               .
c              .                               .          (plot will appear
c              .                               .          on paper as shown,
c              .                               .          i down, j across)
c              .                               .
c         array(ii,jj) . . . . . . . . .  array(ii,jj)
c
c --- ii  may be smaller than  idim, the first (row) dimension of 'array'
c --- in the calling program. thus, plotting of partial arrays is possible.
c
      real array(idim,1)
      data sqrt2/1.414/
c
      amx=-1.e25
      amn= 1.e25
      do 1 i=1,ii
      do 1 j=1,jj
      amx=amax1(amx,array(i,j))
 1    amn=amin1(amn,array(i,j))
c
      if (amx.gt.amn) go to 2
      write (lp,100) array(1,1)
 100  format (//' field to be contoured is constant ...',1pe15.5/)
      return
c
 2    contur=(amx-amn)/6.
      q=10.**int(alog10(contur))
      if (contur.lt.1.) q=q/10.
      ratio=contur/q
      if (ratio.gt.sqrt2*5.)  contur=q*10.
      if (ratio.le.sqrt2*5.)  contur=q*5.
      if (ratio.le.sqrt2*2.)  contur=q*2.
      if (ratio.le.sqrt2)     contur=q
      write (lp,101) contur,amn,amx
 101  format (' contour interval in plot below is',1pe9.1,
     .        6x,'min/max =',2e11.3/)
      call digplt(array,idim,ii,jj,contur)
c
      return
      end
c
c
      subroutine digplt(array,idim,ii,jj,dec)
c
c --- simulate a contour line plot on the printer
c
      common/linepr/lp
c
      real array(idim,1)
      character*1 digit(130),dig(20)
      data dig/'0',' ','1',' ','2',' ','3',' ','4',' ',
     .         '5',' ','6',' ','7',' ','8',' ','9',' '/
c
c     nchar = number of character increments in 'j' direction
c     ratio = character width / line spacing
c
      data nchar/74/,ratio/.58/
      xinc=float(jj-1)/(float(nchar)*ratio)
      yinc=float(jj-1)/ float(nchar)
      k=float(nchar)*ratio*float(ii-1)/float(jj-1)+1.00001
      do 1 i=1,k
      x=1.+float(i-1)*xinc
      ia=min0(ii-1,int(x))
      dx=x-float(ia)
      do 2 j=1,nchar+1
      y=1.+float(j-1)*yinc
      ja=min0(jj-1,int(y))
      dy=y-float(ja)
      dxdy=dx*dy
      value=array(ia,ja)*(1.-dx-dy+dxdy)
     .     +array(ia+1,ja)*(dx-dxdy)
     .     +array(ia,ja+1)*(dy-dxdy)
     .     +array(ia+1,ja+1)*dxdy
      n=mod(mod(int(2.*value/dec+sign(.5,value)),20)+20,20)+1
 2    digit(j)=dig(n)
 1    write (lp,100) 'i',' ',(digit(j),j=1,nchar+1),' ','i'
 100  format(1x,130a1)
      return
      end
