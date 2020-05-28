      subroutine pakk(array,idim,ii,jj,compac,length)
c
c --- converts the contents of -array- into an ascii character string which
c --- is stored in character*2 array -compac-. compac(1)...compac(7) contain
c --- the base value, i.e., the minimum value encountered in -array-.
c --- compac(8)...compac(14) contain a scale factor by which the individual
c --- 6-bit integers encoded as ascii character pairs in compac(8),...
c --- compac(length) must be multiplied before the base value is added
c --- during an unpakking operation. base value and scale factor are encoded
c --- in e14.7 format.
c
c --- the printable ascii characters used to encode the integers include
c --- the numbers 0...9, upper- and lower-case letters a...z, a...z, plus
c --- two additional characters '.' and '/' (total of 64).
c
c --- a packing operation fills (ii*jj+14) array elements in -compac- which
c --- must be dimensioned accordingly in the calling program. the total
c --- number of occupied array elements is returned in -length-. in calls to
c --- unpack, -length- is treated as input variable.
c
      implicit none
      integer      idim,ii,jj,length
      real         array(idim,*)
      character*2  compac(*)
c
      real         base,scal
      integer      i,i1,i2,j,l,nbits,numb
      character*1  char
      character*2  comp2(14)
      character*14 comp14(2)
      equivalence (comp2,comp14)
      data nbits/12/
      base=1.e22
      do 1 i=1,ii
      do 1 j=1,jj
 1    base=min(base,array(i,j))
      scal=0.
      do 2 i=1,ii
      do 2 j=1,jj
 2    scal=max(scal,array(i,j)-base)
      scal=scal/float(2**nbits-1)
      i1=0
      i2=0
      length=14
      do 3 i=1,ii
      do 3 j=1,jj
      if (scal.eq.0.) go to 7
      numb=(array(i,j)-base)/scal+.5
      i1=numb/64
      i2=numb-64*i1
c
c --- map 6-bit numbers onto character set consisting of numbers
c --- 0...9, letters a...z, a...z, and the two characters '.' and '/'.
c --- (if mapping into the character range 32...95 -- which includes the
c --- characters !"#$%&'()*+,-./:;<=>?@[\]^_  -- is deemed safe, delete
c --- the next 6 lines.)
      if (i1.gt.37) i1=i1+6
      if (i1.gt.11) i1=i1+7
      i1=i1+14
      if (i2.gt.37) i2=i2+6
      if (i2.gt.11) i2=i2+7
      i2=i2+14
c
 7    length=length+1
      compac(length)(1:1)=char(i1+32)
      compac(length)(2:2)=char(i2+32)
 100  format (a2)
 3    continue
      write (comp14(1),101) base
      write (comp14(2),101) scal
 101  format (1pe14.7)
      do 8 l=1,14
 8    compac(l)=comp2(l)
c
      return
      end

      subroutine unpakk(array,idim,ii,jj,compac,length)
      implicit none
      integer      idim,ii,jj,length
      real         array(idim,*)
      character*2  compac(*)
c
c --- unpack compac (created by pakk) into array.
c
      real         base,scal
      integer      i,i1,i2,j,l,lngth
      character*2  comp2(14)
      character*14 comp14(2)
      equivalence (comp2,comp14)
c
      do 9 l=1,14
 9    comp2(l)=compac(l)
      read (comp14(1),101) base
      read (comp14(2),101) scal
 101  format (1pe14.7)
      lngth=14
      do 4 i=1,ii
      do 4 j=1,jj
      lngth=lngth+1
      i1=ichar(compac(lngth)(1:1))
      i2=ichar(compac(lngth)(2:2))
c
c --- 6-bit numbers are mapped onto character set consisting of numbers
c --- 0...9, letters a...z, a...z, and the two characters '.' and '/'.
c --- (if mapped into character range 32...95, delete next 6 lines)
      if (i1.gt.96) i1=i1-6
      if (i1.gt.64) i1=i1-7
      i1=i1-14
      if (i2.gt.96) i2=i2-6
      if (i2.gt.64) i2=i2-7
      i2=i2-14
c
 4    array(i,j)=scal*float(64*(i1-32)+(i2-32))+base
      if (lngth.ne.length) stop 'unpack'
      return
      end
