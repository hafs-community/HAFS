      program inddiagnull
c**********************************************************************
c     This program takes the center, grid, and field information for
c      an individual time and calculates diagnostic parameters,
c      then produces a parameter file for that time.
c     Last Modified: 03/02/2012, version 2.0
c**********************************************************************
c 
      USE diag_util
      IMPLICIT NONE
c 
      !begin variable declaration
      integer, parameter :: imiss = -9999, imissd = 9999
      real, parameter :: rmiss = -999.9
      integer :: luin, luou, ierrc, istat
      character(len=80) :: fnin, fnou
c 
      !Variables for storm data section
      integer, parameter :: nvar = 16
      real, dimension(nvar) :: diagvar = rmiss
      integer, dimension(nvar) :: idiagvar = imissd
c                                                                        
      !Variables for sounding data section                                    
      integer :: nsnd
      real, allocatable, dimension(:) :: usnd, vsnd, zsnd
      real, allocatable, dimension(:) :: tsnd, rsnd
      real :: usfc=rmiss, vsfc=rmiss, tsfc=rmiss
      real :: rsfc=rmiss, psfc=rmiss
c 
      !Variables for the optional data section
      !currently empty
c 
      !end variable declaration
c 
      fnin='diaginfo.txt'
      luin=30
c 
      fnou='params.txt'
      luou=32
c 
      open(unit=luin,file=fnin,form='formatted',status='old',err=900)
      open(unit=luou,file=fnou,form='formatted',status='replace',
     +     err=900)
c 
      !get correct number of pressure levels for sounding
      read(luin,*) nsnd
c 
      !set up sounding arrays
      allocate(usnd(nsnd),STAT=istat)
      allocate(vsnd(nsnd),STAT=istat)
      allocate(tsnd(nsnd),STAT=istat)
      allocate(rsnd(nsnd),STAT=istat)
      allocate(zsnd(nsnd),STAT=istat)
      usnd=rmiss
      vsnd=rmiss
      tsnd=rmiss
      rsnd=rmiss
      zsnd=rmiss
c 
      !call writeparams for the null case (all variables=rmiss)
      call writeparams(luou,ierrc,rmiss,imiss,imissd,
     +                 nvar,diagvar,idiagvar,nsnd,
     +                 usnd,vsnd,tsnd,rsnd,zsnd,
     +                 usfc,vsfc,tsfc,rsfc,psfc)
c 
      !clean up arrays, close files
      deallocate(usnd,STAT=istat)
      deallocate(vsnd,STAT=istat)
      deallocate(tsnd,STAT=istat)
      deallocate(rsnd,STAT=istat)
      deallocate(zsnd,STAT=istat)
c 
      close(luin)
      close(luou)
c 
c      return
      goto 950
c 
  900 continue
      stop 'Error during file open for inddiagnull'
c 
  950 continue
      end
