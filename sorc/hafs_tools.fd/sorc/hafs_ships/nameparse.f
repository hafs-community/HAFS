      program nameparse
c**********************************************************************
c     This program takes the variable-length grib2 filename and
c      breaks it down into its component information.
c     Last Modified: 03/02/2012, version 2.0
c**********************************************************************
c 
      IMPLICIT NONE
c 
      !begin variable declaration
      integer :: luin, luou
      character(len=80) :: cline80, fnin, fnou
c 
      integer :: n, npart, ipart, maxlensum
      integer, dimension(1:4) :: maxlen, partbeg, partend
      character(len=2) :: cbasin
c 
      !end variable declaration
c 
      fnin='tname.txt'
      luin=30
      fnou='tname2.txt'
      luou=31
c 
      open(unit=luin,file=fnin,form='formatted',status='old',err=900)
      open(unit=luou,file=fnou,form='formatted',status='replace',
     +     err=900)
c 
      !read in file name from tname.txt
      read(luin,'(a80)') cline80
c 
      !parse name into component parts
      n=1
      npart=1
      ipart=1
  100 continue
      if ((cline80(n:n) .ne. ' ') .and. (n .lt. 80)) then
         if (cline80(n:n) .ne. '.') then
            npart=npart+1
            n=n+1
         else
            maxlen(ipart)=npart-1
            ipart=ipart+1
            npart=1
            n=n+1
         endif
         goto 100
      else
         if (ipart .ne. 4) then
            write(*,*) 'Incorrect number of parts found: %i.', ipart
            goto 900
         else
            maxlen(ipart)=npart-1
         endif
      endif
c 
      !set beginning and ending indices for each part
      maxlensum = 0
      do n=1,4
         partbeg(n) = n + maxlensum
         maxlensum = maxlensum + maxlen(n)
         partend(n) = maxlensum + (n-1)
      enddo
c 
      !debugging test, write out each part
c      do n=1,4
c         write(*,*) maxlen(n)
c         write(*,*) cline80(partbeg(n):partend(n))
c      enddo
c 
      !go from 1 char to 2 char basin designation (default=al)
      cbasin(1:2)='al'
      if (cline80(partend(1):partend(1)) .ne. 'l') then
         if (cline80(partend(1):partend(1)) .eq. 'e') then
            cbasin(1:2)='ep'
         else if (cline80(partend(1):partend(1)) .eq. 'c') then
            cbasin(1:2)='cp'
         else if (cline80(partend(1):partend(1)) .eq. 'w') then
            cbasin(1:2)='wp'
         else if (cline80(partend(1):partend(1)) .eq. 'p') then
            cbasin(1:2)='sh'
         else if (cline80(partend(1):partend(1)) .eq. 's') then
            cbasin(1:2)='sh'
         else if (cline80(partend(1):partend(1)) .eq. 'a') then
            cbasin(1:2)='io'
         else if (cline80(partend(1):partend(1)) .eq. 'b') then
            cbasin(1:2)='io'
         endif
      endif
c 
      !save parts into tname2.txt
      !sname, snum, sbasin
      write(luou,*) cline80(partbeg(1):partend(1)-3)
      write(luou,*) cline80(partend(1)-2:partend(1)-1)
c      write(luou,*) cline80(partend(1):partend(1))
      write(luou,*) cbasin(1:2)
      !DTG, Y, M, D, T
      write(luou,*) cline80(partbeg(2):partend(2))
      write(luou,*) cline80(partbeg(2):partbeg(2)+3)
      write(luou,*) cline80(partbeg(2)+4:partbeg(2)+5)
      write(luou,*) cline80(partbeg(2)+6:partbeg(2)+7)
      write(luou,*) cline80(partbeg(2)+8:partend(2))
      !model, vert, domain
      write(luou,*) cline80(partbeg(3):partbeg(3)+3)
      write(luou,*) cline80(partbeg(3)+4:partend(3)-2)
      write(luou,*) cline80(partend(3):partend(3))
      !file type, forecast time
      write(luou,*) cline80(partbeg(4):partbeg(4)+3)
      write(luou,*) cline80(partbeg(4)+5:partend(4))
      !filename, minus the forecast time
      write(luou,*) cline80(partbeg(1):partbeg(4)+4)
      !filename in two parts, minus the forecast time and domain
      write(luou,*) cline80(partbeg(1):partend(3)-1)
      write(luou,*) cline80(partend(3)+1:partbeg(4)+4)
c 
C added again by Weiguo Wang 2015-0504
      write(luou,*) cline80(partend(1):partend(1))
      close(luin)
      close(luou)
c 
c      return
      goto 950
c 
  900 continue
      stop 'Error during file open for nameparse'
c 
  950 continue
      end
