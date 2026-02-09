      program gridparse
c**********************************************************************
c     This program takes the output from the grib2 nxny and grid
c      calls and breaks it down into components usable by later
c      Fortran code.
c     Last Modified: 03/02/2012, version 2.0
c**********************************************************************
c 
      IMPLICIT NONE
c 
      !begin variable declaration
      integer :: lunx, lull, luou
      character(len=80) :: cline80, fnnx, fnll, fnou
c 
      integer :: n, npart, ipart, maxlensum
      integer, dimension(1:4) :: maxlen, partbeg, partend
c 
      !end variable declaration
c 
      fnnx='tempnxny.txt'
      lunx=30
      fnll='templatlon.txt'
      lull=31
      fnou='tempgrid.txt'
      luou=32
c 
      open(unit=lunx,file=fnnx,form='formatted',status='old',err=900)
      open(unit=lull,file=fnll,form='formatted',status='old',err=900)
      open(unit=luou,file=fnou,form='formatted',status='replace',
     +     err=900)
c 
      !read in nx, ny first
      read(lunx,'(a80)') cline80
c 
      !parse nx,ny
      n=1
      ipart=1
  100 continue
      if ((cline80(n:n) .ne. ')') .and. (n .lt. 80)) then
         if (cline80(n:n) .eq. '(') then
            n=n+1
            partbeg(ipart)=n
         elseif (cline80(n:n) .eq. ' ') then
            if (ipart .eq. 1) then
               maxlen(ipart)=n-partbeg(ipart)
               partend(ipart)=n-1
               ipart=ipart+1
               n=n+3
               partbeg(ipart)=n
            else
               n=n+1
            endif
         else
            n=n+1
         endif
         goto 100
      else
         if (ipart .ne. 2) then
            write(*,*) 'Incorrect number of parts found: %i.', ipart
            goto 900
         else
            partend(ipart)=n-1
            maxlen(ipart)=n-partbeg(ipart)
         endif
      endif
c 
      !debugging test, write out each part (nx, ny)
c      do n=1,2
c         write(*,*) maxlen(n)
c         write(*,*) cline80(partbeg(n):partend(n))
c      enddo
c 
      !save parts into gridtemp.txt
      !nx, ny
      write(luou,*) cline80(partbeg(1):partend(1))
      write(luou,*) cline80(partbeg(2):partend(2))
c 
      !read in latitude (3rd line in file)
      read(lull,'(a80)') cline80
      read(lull,'(a80)') cline80
      read(lull,'(a80)') cline80
c 
      !parse latmax, latmin, and latint (ipart 1,2,3)
      n=6
      ipart=1
      partbeg(ipart)=n
  200 continue
      if ((n .lt. 80) .and. (ipart .le. 3)) then
         if ((cline80(n:n) .eq. ' ') .and. (ipart .lt. 3)) then
            partend(ipart)=n-1
            maxlen(ipart)=n-partbeg(ipart)
            n=n+4
            ipart=ipart+1
            partbeg(ipart)=n
            goto 200
         elseif ((cline80(n:n) .eq. ' ') .and. (ipart .eq. 3)) then
            partend(ipart)=n-1
            maxlen(ipart)=n-partbeg(ipart)
         else
            n=n+1
            goto 200
         endif
      else
         if (ipart .ne. 3) then
            write(*,*) 'Incorrect number of parts found: %i.', ipart
            goto 900
         endif
      endif
c 
      !debugging test, write out each part (latmax,latmin,latint)
c      do n=1,3
c         write(*,*) maxlen(n)
c         write(*,*) cline80(partbeg(n):partend(n))
c      enddo
c 
      !save parts into gridtemp.txt
      !latmax, latmin, latint
      write(luou,*) cline80(partbeg(1):partend(1))
      write(luou,*) cline80(partbeg(2):partend(2))
      write(luou,*) cline80(partbeg(3):partend(3))
c 
      !read in longitude (1st line after latitude)
      read(lull,'(a80)') cline80
c 
      !parse lonmin, lonmax, lonint and npoints (ipart 1,2,3,4)
      n=6
      ipart=1
      partbeg(ipart)=n
  300 continue
      if ((n .lt. 80) .and. (ipart .le. 4)) then
         if ((cline80(n:n) .eq. ' ') .and. (ipart .lt. 3)) then
            partend(ipart)=n-1
            maxlen(ipart)=n-partbeg(ipart)
            n=n+4
            ipart=ipart+1
            partbeg(ipart)=n
            goto 300
         elseif ((cline80(n:n) .eq. ' ') .and. (ipart .eq. 3)) then
            partend(ipart)=n-1
            maxlen(ipart)=n-partbeg(ipart)
            n=n+9
            ipart=ipart+1
            partbeg(ipart)=n
            goto 300
         elseif ((cline80(n:n) .eq. ' ') .and. (ipart .eq. 4)) then
            partend(ipart)=n-1
            maxlen(ipart)=n-partbeg(ipart)
         else
            n=n+1
            goto 300
         endif
      else
         if (ipart .ne. 4) then
            write(*,*) 'Incorrect number of parts found: %i.', ipart
            goto 900
         endif
      endif
c 
      !debugging test, write out each part (latmax,latmin,latint)
c      do n=1,4
c         write(*,*) maxlen(n)
c         write(*,*) cline80(partbeg(n):partend(n))
c      enddo
c 
      !save parts into gridtemp.txt
      !lonmin, lonmax, lonint, npoints
      write(luou,*) cline80(partbeg(1):partend(1))
      write(luou,*) cline80(partbeg(2):partend(2))
      write(luou,*) cline80(partbeg(3):partend(3))
      write(luou,*) cline80(partbeg(4):partend(4))
c 
      close(lunx)
      close(lull)
      close(luou)
c 
c      return
      goto 950
c 
  900 continue
      stop 'Error during file open for gridparse'
c 
  950 continue
      end
