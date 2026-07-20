      program totaldiag
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
      integer, parameter :: nvar = 16
      integer :: nlevs
      integer, allocatable, dimension(:) :: iplevs
c      integer, parameter :: nsnd = 21
c      integer, parameter :: nlevs = nsnd+1
c      integer :: nlvls
c      integer, dimension(nsnd) :: plevs
      integer :: inestuse
      integer :: luin, lupl, lupa, luou, ierrc, istat
      integer :: itmax, itint, m, n, ntimes, itpar
      character(len=20) :: fnin, fnpl, fnpa, fnou
      character(len=40) :: dash
      character(len=2) :: cbasin, csbasin, csnum
      character(len=4) :: cmodel, csmodel
      character(len=10) :: csdtg, cname, csname
      character(len=13) :: cfmtstri, cfmtstra
      character(len=31) :: cfmtstrp
c      character(len=10) :: cendt, cendr, cendp, cendu, cendv, cendz
c      character(len=4) :: csurf
      character(len=4) :: cplev
      character(len=16) :: ctlabt, ctlabr, ctlabp
      character(len=16) :: ctlabu, ctlabv, ctlabz

c 
      !parameter variables
      integer :: numparams
c      integer, parameter :: numparams = nvar+5*(nsnd+1)
c      character(len=16), dimension(numparams) :: paramlab
      character(len=16), dimension(nvar) :: paramlab
      integer, allocatable, dimension(:,:) :: iparams
      integer, allocatable, dimension(:) :: arr1d
      integer, allocatable, dimension(:) :: itimes
      real, allocatable, dimension(:) :: rtimes, rlat, rlon
      real, allocatable, dimension(:) :: rxt, ryt, rmagt, rspd, rhdg
      character(len=6), allocatable, dimension(:) :: clon, clat
c 
      !end variable declaration
c 
      dash='----------------------------------------'
c      plevs = (/1013, 1000, 950, 900, 850, 800, 750,
c     +          700, 650, 600, 550, 500, 450, 400,
c     +          350, 300, 250, 200, 150, 100, 50/)
c 
      fnin='diaginfo.txt'
      luin=30
c 
      fnpl='input.plvls'
      lupl=31
c 
      lupa=32
c 
      fnou='diag.txt'
      luou=34
c 
      open(unit=luin,file=fnin,form='formatted',status='old',err=900)
      open(unit=lupl,file=fnpl,form='formatted',status='old',err=900)
      open(unit=luou,file=fnou,form='formatted',status='replace',
     +     err=900)
      !read in model run information
      read(luin,*) nlevs
      read(luin,*) inestuse
      read(luin,*) itmax
      read(luin,*) itint
      read(luin,'(a10)') csdtg
      read(luin,'(a4)') csmodel
      read(luin,'(a2)') csbasin
      read(luin,'(a2)') csnum
      read(luin,'(a10)') csname
      call upcase(csname,10)
      call upcase(csmodel,4)
      call upcase(csbasin,2)
c 
      !allocate iplevs and read from input.plvls
      allocate(iplevs(nlevs),STAT=istat)
      iplevs=imiss
c 
      !read in iplevs array from input.plvls
      do n=1,nlevs
         read(lupl,*) iplevs(n)
      enddo
c 
c      !calculate the number of stored parameters
      numparams=nvar+5*(nlevs+1)
c      write(*,*)'numparams=', numparams
c 
      ntimes=int(float(itmax)/float(itint)) + 1
      allocate(itimes(ntimes),STAT=istat)
      allocate(rtimes(ntimes),STAT=istat)
      allocate(rlat(ntimes),STAT=istat)
      allocate(rlon(ntimes),STAT=istat)
      allocate(rxt(ntimes),STAT=istat)
      allocate(ryt(ntimes),STAT=istat)
      allocate(rmagt(ntimes),STAT=istat)
      allocate(rspd(ntimes),STAT=istat)
      allocate(rhdg(ntimes),STAT=istat)
      allocate(clon(ntimes),STAT=istat)
      allocate(clat(ntimes),STAT=istat)
      allocate(iparams(numparams,ntimes),STAT=istat)
      allocate(arr1d(ntimes),STAT=istat)
c 
      paramlab( 1) = 'LAT     (DEG)   '
      paramlab( 2) = 'LON     (DEG)   '
      paramlab( 3) = 'MAXWIND (KT)    '
      paramlab( 4) = 'RMW     (KM)    '
      paramlab( 5) = 'MIN_SLP (MB)    '
      paramlab( 6) = 'SHR_MAG (KT)    '
      paramlab( 7) = 'SHR_HDG (DEG)   '
      paramlab( 8) = 'STM_SPD (KT)    '
      paramlab( 9) = 'STM_HDG (DEG)   '
      paramlab(10) = 'SST     (10C)   '
      paramlab(11) = 'OHC     (KJ/CM2)'
      paramlab(12) = 'TPW     (MM)    '
      paramlab(13) = 'LAND    (KM)    '
      paramlab(14) = '850TANG (10M/S) '
      paramlab(15) = '850VORT (/S)    '
      paramlab(16) = '200DVRG (/S)    '
c 
c      cendt(1:10) = '  (10C)   '
c      cendr(1:10) = '  (%)     '
c      cendp(1:10) = '  (MB)    '
c      cendu(1:10) = '  (10KT)  '
c      cendv(1:10) = '  (10KT)  '
c      cendz(1:10) = '  (DM)    '
c      csurf(1:4) = 'SURF'
      ctlabt(1:16) = 'T_SURF  (10C)   '
      ctlabr(1:16) = 'R_SURF  (%)     '
      ctlabp(1:16) = 'P_SURF  (MB)    '
      ctlabu(1:16) = 'U_SURF  (10KT)  '
      ctlabv(1:16) = 'V_SURF  (10KT)  '
      ctlabz(1:16) = 'Z_SURF  (DM)    '
c 
      do n=1,ntimes
         itimes(n) = (n-1)*itint
         rtimes(n) = float(itimes(n))
         read(luin,'(a20)') fnpa
         open(unit=lupa,file=fnpa,form='formatted',status='old',
     +        err=900)
         do m=1,numparams
            read(lupa,'(i6)') iparams(m,n)
         enddo
         close(lupa)
         !set up latitude and longitude
         if (iparams(1,n) .eq. imissd) then
            rlat(n) = rmiss
            write(clat(n),'(i6)') imissd
         else
            rlat(n) = float(iparams(1,n))/10.0
            write(clat(n),'(f6.1)') rlat(n)
         endif
         if (iparams(2,n) .eq. imissd) then
            rlon(n) = rmiss
            write(clon(n),'(i6)') imissd
         else
            rlon(n) = float(iparams(2,n))/10.0
            write(clon(n),'(f6.1)') rlon(n)
         endif
      enddo
c 
      !add in storm speed and heading calculations (diagvar 8 and 9)
      call tspdcal(rlat,rlon,rtimes,ntimes,rmiss,rxt,ryt,rmagt)
      do n=1,ntimes
         if ((rlat(n) .le. rmiss) .or. (rlon(n) .le. rmiss)) then
            iparams(8,n) = imissd
            iparams(9,n) = imissd
         elseif (rmagt(n) .le. rmiss) then
            iparams(8,n) = imissd
            iparams(9,n) = imissd
         else
            call ctorh(rxt(n),ryt(n),rspd(n),rhdg(n))
            iparams(8,n) = nint(rspd(n))
            iparams(9,n) = nint(rhdg(n))
         endif
      enddo
c 
      !specify the format for almost all params (except lat/lon)
      cfmtstri(1:5) = '(a16,'
      write(cfmtstri(6:8),'(i3.3)') ntimes
      cfmtstri(9:13) = '(i6))'
c 
      cfmtstra=cfmtstri
      cfmtstra(10:10)='a'
c 
      cfmtstrp(1:31) = '(a4,1x,i3.3,1x,a4,000(1x,i4.4))'
      write(cfmtstrp(19:21),'(i3.3)') nlevs
c 
      !write out the diagnostic file
      write(*,*)'Processing diagnostic file'
      write(luou,101)'*',csmodel,csdtg,'*'
  101 format(15x,a1,3x,a4,2x,a10,3x,a1)
      write(luou,102)'*',csbasin,csnum,csname,'*'
  102 format(15x,a1,3x,2(a2),2x,a10,3x,a1)
      write(luou,*)''
      write(luou,103)dash,dash(1:14),'STORM DATA',dash,dash(1:18)
  103 format(16x,a40,a14,5x,a10,5x,a40,a18)
      write(luou,*)''
      write(luou,'(a5,1x,i3.3)') 'NTIME', ntimes
      write(luou,cfmtstri) 'TIME    (HR)    ', itimes
      write(luou,cfmtstra) paramlab(1), clat
      write(luou,cfmtstra) paramlab(2), clon
      do n=3,nvar
         arr1d=iparams(n,:)
         write(luou,cfmtstri) paramlab(n), arr1d
      enddo
      write(luou,*)''
      !begin sounding data section
      write(luou,104)dash,dash(1:14),'SOUNDING DATA',dash,dash(1:15)
  104 format(16x,a40,a14,5x,a13,5x,a40,a15)
      write(luou,*)''
c      write(luou,105) 'NLEV', nlevs, 'SURF', iplevs
c  105 format(a4,1x,i3.3,1x,a4,21(1x,i4.4))
      write(luou,cfmtstrp) 'NLEV', nlevs+1, 'SURF', iplevs
      write(luou,cfmtstri) 'TIME    (HR)    ', itimes
      itpar=nvar+1
      arr1d=iparams(itpar,:)
      write(luou,cfmtstri) ctlabt(1:16), arr1d
      itpar=itpar+1
      arr1d=iparams(itpar,:)
      write(luou,cfmtstri) ctlabr(1:16), arr1d
      itpar=itpar+1
      arr1d=iparams(itpar,:)
      write(luou,cfmtstri) ctlabp(1:16), arr1d
      itpar=itpar+1
      arr1d=iparams(itpar,:)
      write(luou,cfmtstri) ctlabu(1:16), arr1d
      itpar=itpar+1
      arr1d=iparams(itpar,:)
      write(luou,cfmtstri) ctlabv(1:16), arr1d
      itpar=itpar+1
      do n=1,nlevs
         itpar=nvar+(5*n)+1
         write(cplev(1:4),'(i4.4)') iplevs(n)
         ctlabt(3:6) = cplev(1:4)
         arr1d=iparams(itpar,:)
         write(luou,cfmtstri) ctlabt(1:16), arr1d
         itpar=itpar+1
         ctlabr(3:6) = cplev(1:4)
         arr1d=iparams(itpar,:)
         write(luou,cfmtstri) ctlabr(1:16), arr1d
         itpar=itpar+1
         ctlabz(3:6) = cplev(1:4)
         arr1d=iparams(itpar,:)
         write(luou,cfmtstri) ctlabz(1:16), arr1d
         itpar=itpar+1
         ctlabu(3:6) = cplev(1:4)
         arr1d=iparams(itpar,:)
         write(luou,cfmtstri) ctlabu(1:16), arr1d
         itpar=itpar+1
         ctlabv(3:6) = cplev(1:4)
         arr1d=iparams(itpar,:)
         write(luou,cfmtstri) ctlabv(1:16), arr1d
         itpar=itpar+1
      enddo
      write(luou,*)''
      !begin custom data section
      write(luou,106)dash,dash(1:14),'CUSTOM DATA',dash,dash(1:17)
  106 format(16x,a40,a14,5x,a11,5x,a40,a17)
      write(luou,*)''
      write(luou,'(a4,1x,i3.3)') 'NVAR', 0
      !no current custom data included
      write(luou,*)''
      write(luou,107)dash,dash(1:14),'COMMENTS',dash,dash(1:20)
  107 format(16x,a40,a14,5x,a8,5x,a40,a20)
      write(luou,*)''
c 
      close(luin)
      close(lupl)
      close(luou)
c 
      deallocate(iplevs,STAT=istat)
      deallocate(itimes,STAT=istat)
      deallocate(rtimes,STAT=istat)
      deallocate(rlat,STAT=istat)
      deallocate(rlon,STAT=istat)
      deallocate(rxt,STAT=istat)
      deallocate(ryt,STAT=istat)
      deallocate(rmagt,STAT=istat)
      deallocate(rspd,STAT=istat)
      deallocate(rhdg,STAT=istat)
      deallocate(clon,STAT=istat)
      deallocate(clat,STAT=istat)
      deallocate(iparams,STAT=istat)
      deallocate(arr1d,STAT=istat)
c 
c      return
      goto 950
c 
  900 continue
      stop 'Error during file open for totaldiag'
c 
  950 continue
      end
