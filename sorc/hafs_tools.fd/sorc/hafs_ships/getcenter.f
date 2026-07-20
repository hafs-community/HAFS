      program getcenter
c**********************************************************************
c     This program reads in the desired dtg, model, and forecast time
c      and writes out the latitude, longitude, vmax, and pmin.
c
c     Needs: diag_util.f
c
c     Last Modified: 03/02/2012, version 2.0
c**********************************************************************
c 
      USE diag_util
      IMPLICIT NONE
c 
      !begin variable declaration
      integer :: luad, luin, luou
      character(len=80) :: fnad, fnin, fnou
c 
      character(len=10) :: cdtg
      character(len=4) :: cmodel
      integer :: iftime, imiss, ilat, ilon, ivmax, ipmin, ierr
c 
      !end variable declaration
c 
      fnad='tempadeck.dat'
      luad=30
      fnin='tempadinfo.txt'
      luin=31
      fnou='center.txt'
      luou=32
c 
      open(unit=luad,file=fnad,form='formatted',status='old',err=900)
      open(unit=luin,file=fnin,form='formatted',status='old',err=900)
      open(unit=luou,file=fnou,form='formatted',status='replace',
     +     err=900)
c 
      !read in cdtg, cmodel, imiss, iftime
      read(luin,'(a10,1x,a4,1x,i5,1x,i3)') cdtg,cmodel,imiss,iftime
      !convert cmodel to uppercase
      call upcase(cmodel,4)
c 
      !debugging test, write out each input
c      write(*,*) cdtg(1:10),cmodel(1:4),imiss,iftime
c 
      !call readADeck
      call readADeck(luad,cdtg,cmodel,iftime,imiss,
     +               ierr,ilat,ilon,ivmax,ipmin)
c 
      !save output into center.txt
      !script currently doesn't read back -9999 designation,
      ! so replace with 9999 (version 1.1)
      if (ilat .eq. imiss) ilat = ilat*(-1)
      if (ilon .eq. imiss) ilon = ilon*(-1)
      if (ivmax .eq. imiss) ivmax = ivmax*(-1)
      if (ipmin .eq. imiss) ipmin = ipmin*(-1)
      write(luou,'(i6,i6,i6,i6)') ilat,ilon,ivmax,ipmin
c      write(luou,*) ilat
c      write(luou,*) ilon
c      write(luou,*) ivmax
c      write(luou,*) ipmin
c 
      !debugging test, write out each input
c      write(*,*) ilat,ilon,ivmax,ipmin,ierr
c 
      close(luad)
      close(luin)
      close(luou)
c 
c      return
      goto 950
c 
  900 continue
      stop 'Error during file open for getcenter'
c 
  950 continue
      end
