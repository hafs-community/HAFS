      subroutine forday(dtime,yrflag, iyear,iday,ihour)
      use mod_xc  ! HYCOM communication interface
      implicit none
c
      real*8  dtime
      integer yrflag, iyear,iday,ihour
c
c --- converts model day to "calendar" date (year,ordinal-day,hour).
c
      real*8  dtim1,day
      integer iyr,nleap
c
      if     (yrflag.eq.0) then
c ---   360 days per model year, starting Jan 16
        iyear =  int((dtime+15.001d0)/360.d0) + 1
        iday  =  mod( dtime+15.001d0 ,360.d0) + 1
        ihour = (mod( dtime+15.001d0 ,360.d0) + 1.d0 - iday)*24.d0
c
      elseif (yrflag.eq.1) then
c ---   366 days per model year, starting Jan 16
        iyear =  int((dtime+15.001d0)/366.d0) + 1
        iday  =  mod( dtime+15.001d0 ,366.d0) + 1
        ihour = (mod( dtime+15.001d0 ,366.d0) + 1.d0 - iday)*24.d0
c
      elseif (yrflag.eq.2) then
c ---   366 days per model year, starting Jan 01
        iyear =  int((dtime+ 0.001d0)/366.d0) + 1
        iday  =  mod( dtime+ 0.001d0 ,366.d0) + 1
        ihour = (mod( dtime+ 0.001d0 ,366.d0) + 1.d0 - iday)*24.d0
c
      elseif (yrflag.eq.3) then
c ---   model day is calendar days since 01/01/1901
        iyr   = (dtime-1.d0)/365.25d0
        nleap = iyr/4
        dtim1 = 365.d0*iyr + nleap + 1.d0
        day   = dtime - dtim1 + 1.d0
        if     (dtim1.gt.dtime) then
          iyr = iyr - 1
        elseif (day.ge.367.d0) then
          iyr = iyr + 1
        elseif (day.ge.366.d0 .and. mod(iyr,4).ne.3) then
          iyr = iyr + 1
        endif
        nleap = iyr/4
        dtim1 = 365.d0*iyr + nleap + 1.d0
c
        iyear =  1901 + iyr
        iday  =  dtime - dtim1 + 1.001d0
        ihour = (dtime - dtim1 + 1.001d0 - iday)*24.d0
c
      else
        if     (mnproc.eq.1) then
        write(lp,*)
        write(lp,*) 'error in forday - unsupported yrflag value'
        write(lp,*)
        endif !1st tile
        call xcstop('(forday)')
               stop '(forday)'
      endif
      return
      end
c
c
      subroutine forfuna
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
c --- initialize input of atmospheric forcing fields
c
c --- units of tau_x  are N/m^2  (positive eastwards  w.r.t. the grid)
c --- units of tau_y  are N/m^2  (positive northwards w.r.t. the grid)
c --- units of wnd_x  are m/s    (positive eastwards  w.r.t. the grid)
c --- units of wnd_y  are m/s    (positive northwards w.r.t. the grid)
c --- units of wndspd are m/s
c --- units of ustar  are m/s
c --- units of airtmp are degC
c --- units of surtmp are degC
c --- units of seatmp are degC
c --- units of vapmix are kg/k
c --- units of mslprs are Pa     (anomaly, offset from total by prsbas)
c --- units of precip are m/s    (positive into ocean)
c --- units of radflx are w/m^2  (positive into ocean)
c --- units of swflx  are w/m^2  (positive into ocean)
c --- units of offlux are w/m^2  (positive into ocean)
c --- units of oftaux are N/m^2  (positive eastwards  w.r.t. the grid)
c --- units of oftauy are N/m^2  (positive northwards w.r.t. the grid)
c
c --- tau_x and tau_y are either on u&v grids or both on the p grid,
c --- depending on the value of blkdat input parameter "wndflg".
c --- in any case, they are always oriented along the local grid
c --- which need not be east-west and north-south.
c --- all other fields, including wnd* and ustar, are always on the p grid.
c
c --- I/O and array I/O units 899-910 are reserved for the entire run.
c
c --- all input fields must be defined at all grid points
c
      integer         mreca,mrecc,mreck,mrecr
      common/rdforfi/ mreca,mrecc,mreck,mrecr
      save  /rdforfi/
c
      integer   lgth,mo
      character preambl(5)*79
      real      pcmax,one,oneps
      integer   i,j
c
      mreca=1
      if     (mnproc.eq.1) then
      write (lp,*) ' now opening forcing fields ...'
      endif !1st tile
      call xcsync(flush_lp)
c
      lgth = len_trim(flnmfor)
c
      if     (mslprf .or. flxflg.eq.6) then
c-org      call zaiopf(flnmfor(1:lgth)//'forcing.mslprs.a', 'old', 899)
c-hsk 2017: change the forcing file name to presur
      call zaiopf(flnmfor(1:lgth)//'forcing.presur.a', 'old', 899)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      open (unit=uoff+899,file=flnmfor(1:lgth)//'forcing.presur.b',
     &   status='old', action='read')
      read (uoff+899,'(a79)') preambl
      endif !1st tile
      call preambl_print(preambl)
      call rdmonth(util1, 899)
cdiag call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,1.,
cdiag.     'mslprs (Pa)')
      endif !mslprf
c
      if (windf) then
c
      if     (wndflg.lt.4) then
        call zaiopf(flnmfor(1:lgth)//'forcing.tauewd.a', 'old', 901)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+901,file=flnmfor(1:lgth)//'forcing.tauewd.b',
     &        status='old', action='read')
        read (uoff+901,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        call rdmonth(util1, 901)
cdiag   call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,1000.,
cdiag.       'tau_x (x 1000 N/m^2 ) ')
c
        call zaiopf(flnmfor(1:lgth)//'forcing.taunwd.a', 'old', 902)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+902,file=flnmfor(1:lgth)//'forcing.taunwd.b',
     &        status='old', action='read')
        read (uoff+902,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        call rdmonth(util1, 902)
cdiag   call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,1000.,
cdiag.       'tau_y (x 1000 N/m^2 ) ')
      else !read 10m wind components
        call zaiopf(flnmfor(1:lgth)//'forcing.wndewd.a', 'old', 901)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+901,file=flnmfor(1:lgth)//'forcing.wndewd.b',
     &        status='old', action='read')
        read (uoff+901,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        call rdmonth(util1, 901)
cdiag   call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,1.,
cdiag.       'wnd_x (m/s ) ')
c
        call zaiopf(flnmfor(1:lgth)//'forcing.wndnwd.a', 'old', 902)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+902,file=flnmfor(1:lgth)//'forcing.wndnwd.b',
     &        status='old', action='read')
        read (uoff+902,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        call rdmonth(util1, 902)
cdiag   call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,1.,
cdiag.       'wnd_y (m/s) ')
        endif !stress:wind
c
        if     (stroff) then
          call zaiopf(flnmfor(1:lgth)//'forcing.ofstrs.a', 'old', 916)
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
          open (unit=uoff+916,file=flnmfor(1:lgth)//'forcing.ofstrs.b',
     &       status='old', action='read')
          read (uoff+916,'(a79)') preambl
          endif !1st tile
          call preambl_print(preambl)
          call rdmonth(oftaux, 916)
          call rdmonth(oftauy, 916)
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
          close( unit=uoff+916)
          endif
          call zaiocl(916)
cdiag     call prtmsk(ip,oftaux,util2,idm,idm,jdm,  0.,1.0,
cdiag.         'taux offset (N)  ')
cdiag     call prtmsk(ip,oftauy,util2,idm,idm,jdm,  0.,1.0,
cdiag.         'tauy offset (N)  ')
          else
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1-nbdy,jj+nbdy
            do i=1-nbdy,ii+nbdy
              oftaux(i,j) = 0.0
              oftauy(i,j) = 0.0
            enddo !i
          enddo !j
        endif !stroff:else
c
      endif				!  windf = .true.
c
      if (thermo) then
c
      if     (ustflg.eq.3) then
      call zaiopf(flnmfor(1:lgth)//'forcing.ustar.a', 'old', 900)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      open (unit=uoff+900,file=flnmfor(1:lgth)//'forcing.ustar.b',
     &   status='old', action='read')
      read (uoff+900,'(a79)') preambl
      endif !1st tile
      call preambl_print(preambl)
      call rdmonth(util1, 900)
cdiag call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,1000.,
cdiag.     'ustar (x 1000 ???)')
      endif !ustflg.eq.3
c
      if (wndflg.lt.3) then
      call zaiopf(flnmfor(1:lgth)//'forcing.wndspd.a', 'old', 903)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      open (unit=uoff+903,file=flnmfor(1:lgth)//'forcing.wndspd.b',
     &   status='old', action='read')
      read (uoff+903,'(a79)') preambl
      endif !1st tile
      call preambl_print(preambl)
      call rdmonth(util1, 903)
cdiag call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,10.,
cdiag.     'wind speed  (x 10 m/s)')
      endif !wndflg.lt.3
c
      if     (flxflg.ne.3) then
      call zaiopf(flnmfor(1:lgth)//'forcing.airtmp.a', 'old', 904)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      open (unit=uoff+904,file=flnmfor(1:lgth)//'forcing.airtmp.b',
     &   status='old', action='read')
      read (uoff+904,'(a79)') preambl
      endif !1st tile
      call preambl_print(preambl)
      call rdmonth(util1, 904)
cdiag call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,10.,
cdiag.     'air temperature  (0.1 c)')
c
      call zaiopf(flnmfor(1:lgth)//'forcing.vapmix.a', 'old', 905)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      open (unit=uoff+905,file=flnmfor(1:lgth)//'forcing.vapmix.b',
     &   status='old', action='read')
      read (uoff+905,'(a79)') preambl
      endif !1st tile
      call preambl_print(preambl)
      call rdmonth(util1, 905)
cdiag call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,10000.,
cdiag.     'mixing ratio  (0.1 g/kg)')
      endif !flxflg.ne.3
c
      if     (pcipf) then
        call zaiopf(flnmfor(1:lgth)//'forcing.precip.a', 'old', 906)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+906,file=flnmfor(1:lgth)//'forcing.precip.b',
     &     status='old', action='read')
        read (uoff+906,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        pcmax = -huge(pcmax)
        call rdmonth(util1, 906)
        do j=1,jj
          do i=1,ii
            pcmax = max(pcmax,util1(i,j))
          enddo
        enddo
        call xcmaxr(pcmax)
cdiag   call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,86400.*36000.,
cdiag.       'precipitation (cm/year) ')
c
c ---   zero fields implies no surface salinity flux
        pcipf = pcmax.ne.0.0
        if     (.not.pcipf) then
          if     (mnproc.eq.1) then
          write (lp,*)
          write (lp,*) '***** no surface salinity flux *****'
          write (lp,*)
          endif !1st tile
          call xcsync(flush_lp)
        endif  !pcipf actually .false.
      endif  !pcipf initially .true.
c
      call zaiopf(flnmfor(1:lgth)//'forcing.radflx.a', 'old', 907)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      open (unit=uoff+907,file=flnmfor(1:lgth)//'forcing.radflx.b',
     &   status='old', action='read')
      read (uoff+907,'(a79)') preambl
      endif !1st tile
      call preambl_print(preambl)
      call rdmonth(util1, 907)
cdiag call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,1.0,
cdiag.     'net radiation (w/m^2 )  ')
c
      call zaiopf(flnmfor(1:lgth)//'forcing.shwflx.a', 'old', 908)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      open (unit=uoff+908,file=flnmfor(1:lgth)//'forcing.shwflx.b',
     &   status='old', action='read')
      read (uoff+908,'(a79)') preambl
      endif !1st tile
      call preambl_print(preambl)
      call rdmonth(util1, 908)
cdiag call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,1.0,
cdiag.     'sw radiation (w/m^2 )  ')
c
      endif                    !  thermo = .true.
c
      if     (lwflag.eq.2 .or. sstflg.eq.2   .or.
     &        icmflg.eq.2 .or. ticegr.eq.0.0     ) then
      call zaiopf(flnmfor(1:lgth)//'forcing.surtmp.a', 'old', 909)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      open (unit=uoff+909,file=flnmfor(1:lgth)//'forcing.surtmp.b',
     &   status='old', action='read')
      read (uoff+909,'(a79)') preambl
      endif !1st tile
      call preambl_print(preambl)
      call rdmonth(util1, 909)
cdiag call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,1.0,
cdiag.     'SST from lw rad (degC) ')
      endif !surtmp
c
      if     (sstflg.eq.3) then
      call zaiopf(flnmfor(1:lgth)//'forcing.seatmp.a', 'old', 910)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      open (unit=uoff+910,file=flnmfor(1:lgth)//'forcing.seatmp.b',
     &   status='old', action='read')
      read (uoff+910,'(a79)') preambl
      endif !1st tile
      call preambl_print(preambl)
      call rdmonth(util1, 910)
cdiag call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,1.0,
cdiag.     'SST from obs. (degC)   ')
      endif !sstflg.eq.3
c
      if     (flxoff) then
        call zaiopf(flnmfor(1:lgth)//'forcing.offlux.a', 'old', 916)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+916,file=flnmfor(1:lgth)//'forcing.offlux.b',
     &     status='old', action='read')
        read (uoff+916,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        call rdmonth(offlux, 916)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        close( unit=uoff+916)
        endif
        call zaiocl(916)
cdiag   call prtmsk(ip,offlux,util2,idm,idm,jdm,  0.,1.0,
cdiag.       'heat flux offset (w/m^2 )  ')
        else
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1-nbdy,jj+nbdy
          do i=1-nbdy,ii+nbdy
            offlux(i,j) = 0.0
          enddo !i
        enddo !j
      endif !flxoff:else
c
c --- jerlov used in call to swfrac_ij
      if     (jerlv0.gt.0) then
c ---   calculate jerlov water type,
c ---   which governs the penetration depth of shortwave radiation.
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1-nbdy,jj+nbdy
          do i=1-nbdy,ii+nbdy
c ---       map shallow depths to high jerlov numbers
            jerlov(i,j)=6-max(1,min(5,int(depths(i,j)/15.0)))
            jerlov(i,j)=max(jerlv0,jerlov(i,j))
          enddo
        enddo
      else
c ---   jerlv0= 0 uses an input annual/monthly kpar field
c ---   jerlv0=-1 uses an input annual/monthly chl  field
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1-nbdy,jj+nbdy
          do i=1-nbdy,ii+nbdy
            jerlov(i,j)=jerlv0
          enddo
        enddo
      endif
c
      if     (mnproc.eq.1) then
      write (lp,*) ' ...finished opening forcing fields'
      endif !1st tile
      call xcsync(flush_lp)
c
      return
      end
c
c
      subroutine forfund(tiddrg)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      integer tiddrg
c
c --- initialize tidal drag tensor
c
c --- units of dragrh and drgten are m/s on the p-grid.
c ---          dragrh and drgten should be positive.
c
c --- I/O and array I/O unit 925 used here, but not reserved.
c
c --- all input fields must be defined at all grid points
c
      integer   i,j,k,l,lgth
c
      if     (mnproc.eq.1) then
      if     (tiddrg.eq.1) then
        write (lp,*) ' now opening tidal drag rh field  ...'
      else
        write (lp,*) ' now opening tidal drag tensor fields  ...'
      endif !tiddrg
      endif !1st tile
      call xcsync(flush_lp)
c
      lgth = len_trim(flnmfor)
c
      if     (tiddrg.eq.1) then
c ---   original filename, 1 field (rh)
        call zaiopf(flnmfor(1:lgth)//'tidal.rh.a', 'old', 925)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+925,file=flnmfor(1:lgth)//'tidal.rh.b',
     &     status='old', action='read')
        endif !1st tile
        call rdmonth(util1, 925)
        call xctilr( util1,1,1, nbdy,nbdy, halo_ps)
c ---   cast scalar to tensor drag.
        drgten(1,1,:,:) = drgscl*util1(:,:) !uu
        drgten(1,2,:,:) = 0.0  !uv
        drgten(2,1,:,:) = 0.0  !vu
        drgten(2,2,:,:) = drgscl*util1(:,:) !vv
      else
c ---   drag tensor filename, 4 fields uu, uv, vv, vu
        call zaiopf(flnmfor(1:lgth)//'tidal.tensor.a', 'old', 925)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+925,file=flnmfor(1:lgth)//'tidal.tensor.b',
     &     status='old', action='read')
        endif !1st tile
        call rdmonth(util1, 925)
        call xctilr( util1,1,1, nbdy,nbdy, halo_ps)
        drgten(1,1,:,:) = drgscl*util1(:,:) !uu
        call rdmonth(util1, 925)
        call xctilr( util1,1,1, nbdy,nbdy, halo_ps)
        drgten(1,2,:,:) = drgscl*util1(:,:) !uv
        call rdmonth(util1, 925)
        call xctilr( util1,1,1, nbdy,nbdy, halo_ps)
        drgten(2,1,:,:) = drgscl*util1(:,:) !vu
        call rdmonth(util1, 925)
        call xctilr( util1,1,1, nbdy,nbdy, halo_ps)
        drgten(2,2,:,:) = drgscl*util1(:,:) !vv
      endif !tiddrg
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      close (unit=uoff+925)
      endif
      call zaiocl(925)
c
      if     (mnproc.eq.1) then
      if     (tiddrg.eq.1) then
        write (lp,*) ' ...finished opening tidal drag rh field'
      else
        write (lp,*) ' ...finished opening tidal drag tensor fields'
      endif !tiddrg
      endif !1st tile
      call xcsync(flush_lp)
c
      return
      end
c
c
      subroutine forfunh(dtime)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      real*8    dtime
c
c --- high frequency atmospheric forcing field processing.
c
c --- units of tau_x  are N/m^2  (positive eastwards  w.r.t. the grid)
c --- units of tau_y  are N/m^2  (positive northwards w.r.t. the grid)
c --- units of wnd_x  are m/s    (positive eastwards  w.r.t. the grid)
c --- units of wnd_y  are m/s    (positive northwards w.r.t. the grid)
c --- units of wndspd are m/s
c --- units of ustar  are m/s
c --- units of airtmp are degC
c --- units of surtmp are degC
c --- units of seatmp are degC
c --- units of vapmix are kg/kg
c --- units of mslprs are Pa     (anomaly, offset from total by prsbas)
c --- units of precip are m/s    (positive into ocean)
c --- units of radflx are w/m^2  (positive into ocean)
c --- units of swflx  are w/m^2  (positive into ocean)
c --- units of offlux are w/m^2  (positive into ocean)
c
c --- tau_x and tau_y are either on u&v grids or both on the p grid,
c --- depending on the value of blkdat input parameter "wndflg".
c --- in any case, they are always oriented along the local grid
c --- which need not be east-west and north-south.
c --- all other fields, including wnd* and ustar, are always on the p grid.
c
c --- I/O and array I/O units 899-910 are reserved for the entire run.
c
c --- all input fields must be defined at all grid points
c
      real*8    dtime0,dtime1
      save      dtime0,dtime1
c
      character preambl(5)*79,cline*80
      real      pcmax
      integer   i,ios,iunit,j,lgth,nrec
c
c --- w0 negative on first call only.
      if     (w0.lt.-1.0) then
c
c ---   initialize forcing fields
c
        if      (.not.windf) then
          if     (mnproc.eq.1) then
          write(lp,*)
          write(lp,*) 'error in forfunh - windf must be .true.'
          write(lp,*)
          endif !1st tile
          call xcstop('(forfunh)')
                 stop '(forfunh)'
        elseif (.not.thermo) then
          if     (mnproc.eq.1) then
          write(lp,*)
          write(lp,*) 'error in forfunh - thermo must be .true.'
          write(lp,*)
          endif !1st tile
          call xcstop('(forfunh)')
                 stop '(forfunh)'
        endif
c
        if     (natm.eq.4) then
c ---     linear interpolation in time, so slots 3 and 4 are zero.
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1-nbdy,jj+nbdy
            do i=1-nbdy,ii+nbdy
                taux(i,j,natm-1) = 0.0
                taux(i,j,natm)   = 0.0
                tauy(i,j,natm-1) = 0.0
                tauy(i,j,natm)   = 0.0
              wndspd(i,j,natm-1) = 0.0
              wndspd(i,j,natm)   = 0.0
              ustara(i,j,natm-1) = 0.0
              ustara(i,j,natm)   = 0.0
              airtmp(i,j,natm-1) = 0.0
              airtmp(i,j,natm)   = 0.0
              vapmix(i,j,natm-1) = 0.0
              vapmix(i,j,natm)   = 0.0
              mslprs(i,j,natm-1) = 0.0
              mslprs(i,j,natm)   = 0.0
              precip(i,j,natm-1) = 0.0
              precip(i,j,natm)   = 0.0
              radflx(i,j,natm-1) = 0.0
              radflx(i,j,natm)   = 0.0
               swflx(i,j,natm-1) = 0.0
               swflx(i,j,natm)   = 0.0
              surtmp(i,j,natm-1) = 0.0
              surtmp(i,j,natm)   = 0.0
              seatmp(i,j,natm-1) = 0.0
              seatmp(i,j,natm)   = 0.0
            enddo
          enddo
        endif !natm.eq.4
c
c ---   open all forcing files.
        if     (mnproc.eq.1) then
        write (lp,*) ' now initializing forcing fields ...'
        endif !1st tile
        call xcsync(flush_lp)
c
        lgth = len_trim(flnmfor)
c
        if     (mslprf .or. flxflg.eq.6) then
c-org        call zaiopf(flnmfor(1:lgth)//'forcing.mslprs.a', 'old', 899)
c-hsk 2017: change the filename to presur
        call zaiopf(flnmfor(1:lgth)//'forcing.presur.a', 'old', 899)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+899,file=flnmfor(1:lgth)//'forcing.presur.b',
     &     status='old', action='read')
        read (uoff+899,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        endif !mslprf
c
        if     (wndflg.lt.4) then
          call zaiopf(flnmfor(1:lgth)//'forcing.tauewd.a', 'old', 901)
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
          open (unit=uoff+901,file=flnmfor(1:lgth)//'forcing.tauewd.b',
     &          status='old', action='read')
          read (uoff+901,'(a79)') preambl
          endif !1st tile
          call preambl_print(preambl)
c
          call zaiopf(flnmfor(1:lgth)//'forcing.taunwd.a', 'old', 902)
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
          open (unit=uoff+902,file=flnmfor(1:lgth)//'forcing.taunwd.b',
     &       status='old', action='read')
          read (uoff+902,'(a79)') preambl
          endif !1st tile
          call preambl_print(preambl)
        else !read 10m wind components
          call zaiopf(flnmfor(1:lgth)//'forcing.wndewd.a', 'old', 901)
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
          open (unit=uoff+901,file=flnmfor(1:lgth)//'forcing.wndewd.b',
     &          status='old', action='read')
          read (uoff+901,'(a79)') preambl
          endif !1st tile
          call preambl_print(preambl)
c             
          call zaiopf(flnmfor(1:lgth)//'forcing.wndnwd.a', 'old', 902)
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
          open (unit=uoff+902,file=flnmfor(1:lgth)//'forcing.wndnwd.b',
     &          status='old', action='read')
          read (uoff+902,'(a79)') preambl
          endif !1st tile
          call preambl_print(preambl)
        endif !stress:wind
c
        if     (stroff) then
          call zaiopf(flnmfor(1:lgth)//'forcing.ofstrs.a', 'old', 916)
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
          open (unit=uoff+916,file=flnmfor(1:lgth)//'forcing.ofstrs.b',
     &       status='old', action='read')
          read (uoff+916,'(a79)') preambl
          endif !1st tile
          call preambl_print(preambl)
          call rdmonth(oftaux, 916)
          call rdmonth(oftauy, 916)
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
          close( unit=uoff+916)
          endif
          call zaiocl(916)
cdiag     call prtmsk(ip,oftaux,util2,idm,idm,jdm,  0.,1.0,
cdiag.         'taux offset (N)  ')
cdiag     call prtmsk(ip,oftauy,util2,idm,idm,jdm,  0.,1.0,
cdiag.         'tauy offset (N)  ')
          else
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1-nbdy,jj+nbdy
            do i=1-nbdy,ii+nbdy
              oftaux(i,j) = 0.0
              oftauy(i,j) = 0.0
            enddo !i
          enddo !j
        endif !stroff:else
c
        if     (ustflg.eq.3) then
        call zaiopf(flnmfor(1:lgth)//'forcing.ustar.a', 'old', 900)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+900,file=flnmfor(1:lgth)//'forcing.ustar.b',
     &     status='old', action='read')
        read (uoff+900,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        endif !ustflg.eq.3
c
        if (wndflg.lt.3) then
        call zaiopf(flnmfor(1:lgth)//'forcing.wndspd.a', 'old', 903)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+903,file=flnmfor(1:lgth)//'forcing.wndspd.b',
     &     status='old', action='read')
        read (uoff+903,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        endif !wndflg.lt.3
c
        if     (flxflg.ne.3) then
        call zaiopf(flnmfor(1:lgth)//'forcing.airtmp.a', 'old', 904)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+904,file=flnmfor(1:lgth)//'forcing.airtmp.b',
     &     status='old', action='read')
        read (uoff+904,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
c
        call zaiopf(flnmfor(1:lgth)//'forcing.vapmix.a', 'old', 905)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+905,file=flnmfor(1:lgth)//'forcing.vapmix.b',
     &     status='old', action='read')
        read (uoff+905,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        endif !flxflg.ne.3
c
        if     (pcipf) then
        call zaiopf(flnmfor(1:lgth)//'forcing.precip.a', 'old', 906)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+906,file=flnmfor(1:lgth)//'forcing.precip.b',
     &     status='old', action='read')
        read (uoff+906,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        endif !pcipf
c
        call zaiopf(flnmfor(1:lgth)//'forcing.radflx.a', 'old', 907)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+907,file=flnmfor(1:lgth)//'forcing.radflx.b',
     &     status='old', action='read')
        read (uoff+907,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
c
        call zaiopf(flnmfor(1:lgth)//'forcing.shwflx.a', 'old', 908)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+908,file=flnmfor(1:lgth)//'forcing.shwflx.b',
     &     status='old', action='read')
        read (uoff+908,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
c
        if     (lwflag.eq.2 .or. sstflg.eq.2   .or.
     &          icmflg.eq.2 .or. ticegr.eq.0.0     ) then
          call zaiopf(flnmfor(1:lgth)//'forcing.surtmp.a', 'old', 909)
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
          open (unit=uoff+909,file=flnmfor(1:lgth)//'forcing.surtmp.b',
     &       status='old', action='read')
          read (uoff+909,'(a79)') preambl
          endif !1st tile
          call preambl_print(preambl)
        endif !surtmp
c
        if     (sstflg.eq.3) then
          call zaiopf(flnmfor(1:lgth)//'forcing.seatmp.a', 'old', 910)
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
          open (unit=uoff+910,file=flnmfor(1:lgth)//'forcing.seatmp.b',
     &       status='old', action='read')
          read (uoff+910,'(a79)') preambl
          endif !1st tile
          call preambl_print(preambl)
        endif
c
        if     (flxoff) then
          call zaiopf(flnmfor(1:lgth)//'forcing.offlux.a', 'old', 916)
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
          open (unit=uoff+916,file=flnmfor(1:lgth)//'forcing.offlux.b',
     &       status='old', action='read')
          read (uoff+916,'(a79)') preambl
          endif !1st tile
          call preambl_print(preambl)
          call rdmonth(offlux, 916)
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
          close( unit=uoff+916)
          endif
          call zaiocl(916)
        else
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1-nbdy,jj+nbdy
            do i=1-nbdy,ii+nbdy
              offlux(i,j) = 0.0
            enddo !i
          enddo !j
        endif !flxoff:else
c
c ---   skip ahead to the start time.
        nrec   = 0
        dtime1 = huge(dtime1)
        do  ! infinate loop, with exit at end
          dtime0 = dtime1
          nrec   = nrec + 1
          call zagetc(cline,ios, uoff+901)
          if     (ios.ne.0) then
            if     (mnproc.eq.1) then
              write(lp,*)
              write(lp,*) 'error in forfunh - hit end of input'
              write(lp,*) 'dtime0,dtime1 = ',dtime0,dtime1
              write(lp,*) 'dtime = ',dtime
              write(lp,*)
            endif !1st tile
            call xcstop('(forfunh)')
                   stop '(forfunh)'
          endif
          i = index(cline,'=')
          read (cline(i+1:),*) dtime1
          if     (yrflag.eq.2) then
            if     (nrec.eq.1 .and. abs(dtime1-1096.0d0).gt.0.01) then
c
c ---         climatology must start on wind day 1096.0, 01/01/1904.
              if     (mnproc.eq.1) then
              write(lp,'(a)')  cline
              write(lp,'(/ a,a / a,g15.6 /)')
     &          'error in forfunh - forcing climatology',
     &          ' must start on wind day 1096',
     &          'dtime1 = ',dtime1
              endif !1st tile
              call xcstop('(forfunh)')
                     stop '(forfunh)'
            endif
            dtime1 = (dtime1 - 1096.0d0) + 
     &               wndrep*int((dtime+0.00001d0)/wndrep)  !wndrep=366 or 732
            if     (nrec.ne.1 .and. dtime1.lt.dtime0) then
              dtime1 = dtime1 + wndrep
            endif
          elseif (nrec.eq.1 .and. dtime1.lt.1462.0d0) then
c
c ---       otherwise, must start after wind day 1462.0, 01/01/1905.
            if     (mnproc.eq.1) then
            write(lp,'(a)')  cline
            write(lp,'(/ a,a / a,g15.6 /)')
     &        'error in forfunh - actual forcing',
     &        ' must start after wind day 1462',
     &        'dtime1 = ',dtime1
            endif !1st tile
            call xcstop('(forfunh)')
                   stop '(forfunh)'
          endif
          if     (dtime0.le.dtime .and. dtime1.gt.dtime) then
            exit
          endif
        enddo   ! infinate loop, with exit above
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          rewind(unit=uoff+901)
          read (uoff+901,'(a79)') preambl
        endif
c
        do iunit= 901,908
          if     (iunit.eq.903 .and. wndflg.ge.3) then
            cycle  !no wndspd
          endif
          do i= 1,nrec-2
            call skmonth(iunit)
          enddo
        enddo
        if     (lwflag.eq.2 .or. sstflg.eq.2   .or.
     &          icmflg.eq.2 .or. ticegr.eq.0.0     ) then
          do i= 1,nrec-2
            call skmonth(909)
          enddo
        endif !surtmp
        if     (sstflg.eq.3) then
          do i= 1,nrec-2
            call skmonth(910)
          enddo
        endif
        dtime1 = huge(dtime1)
        call rdpall(dtime0,dtime1)
        if     (yrflag.eq.2) then
          dtime1 = (dtime1 - 1096.0d0) + 
     &             wndrep*int((dtime+0.00001d0)/wndrep)
        endif
        call rdpall(dtime0,dtime1)
        if     (yrflag.eq.2) then
          dtime1 = (dtime1 - 1096.0d0) + 
     &             wndrep*int((dtime+0.00001d0)/wndrep)  !wndrep=366 or 732
          if     (dtime1.lt.dtime0) then
            dtime1 = dtime1 + wndrep
          endif
        endif
c
c ---   zero precip field implies no surface salinity flux
        if     (pcipf) then
          pcmax = -huge(pcmax)
          do j=1,jj
            do i=1,ii
              pcmax = max(pcmax,precip(i,j,1),precip(i,j,2))
            enddo
          enddo
          call xcmaxr(pcmax)
          pcipf = pcmax.ne.0.0
          if     (.not.pcipf) then
            if     (mnproc.eq.1) then
            write (lp,*)
            write (lp,*) '***** no surface salinity flux *****'
            write (lp,*)
            endif !1st tile
            call xcsync(flush_lp)
          endif  !pcipf actually .false.
        endif  !pcipf initially .true.
c
c ---   jerlov used in call to swfrac_ij
        if     (jerlv0.gt.0) then
c ---     calculate jerlov water type,
c ---     which governs the penetration depth of shortwave radiation.
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1-nbdy,jj+nbdy
            do i=1-nbdy,ii+nbdy
c ---         map shallow depths to high jerlov numbers
              jerlov(i,j)=6-max(1,min(5,int(depths(i,j)/15.0)))
              jerlov(i,j)=max(jerlv0,jerlov(i,j))
            enddo
          enddo
        else
c ---     jerlv0= 0 uses an input annual/monthly kpar field
c ---     jerlv0=-1 uses an input annual/monthly chl  field
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1-nbdy,jj+nbdy
            do i=1-nbdy,ii+nbdy
              jerlov(i,j)=jerlv0
            enddo
          enddo
        endif
        if     (mnproc.eq.1) then
        write (lp,*) 
        write (lp,*) ' dtime,dtime0,dtime1 = ',dtime,dtime0,dtime1
        write (lp,*) 
        write (lp,*) ' ...finished initializing forcing fields'
        endif !1st tile
        call xcsync(flush_lp)
      endif  ! initialization
c
      if     (dtime.gt.dtime1) then
c
c ---   get the next set of fields.
*           if     (mnproc.eq.1) then
*           write(lp,*) 'enter rdpall - ',dtime,dtime0,dtime1
*           endif !1st tile
*           call xcsync(flush_lp)
        call rdpall(dtime0,dtime1)
        if     (yrflag.eq.2) then
          dtime1 = (dtime1 - 1096.0d0) + 
     &             wndrep*int((dtime+0.00001d0)/wndrep)  !wndrep=366 or 732
          if     (dtime1.lt.dtime0) then
            dtime1 = dtime1 + wndrep
          endif
        endif
*           if     (mnproc.eq.1) then
*           write(lp,*) ' exit rdpall - ',dtime,dtime0,dtime1
*           endif !1st tile
*           call xcsync(flush_lp)
      endif
c
c --- linear interpolation in time.
      w0 = (dtime1-dtime)/(dtime1-dtime0)
      w1 = 1.0 - w0
*           if     (mnproc.eq.1) then
*           write(lp,*) 'rdpall - dtime,w0,w1 = ',dtime,w0,w1
*           endif !1st tile
*           call xcsync(flush_lp)
      return
      end
c
c
      subroutine forfunhp(dtime)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      real*8    dtime
c
c --- high frequency atmospheric pressure forcing field processing.
c --- call either forfunh or forfunhp, not both.
c
c --- units of mslprs are Pa (anomaly, offset from total by prsbas)
c
c --- mslprs is always on the p grid.
c --- mslprs must be defined at all grid points
c
c --- I/O and array I/O unit 899 is reserved for the entire run.
c
c
      real*8    dtime0,dtime1
      save      dtime0,dtime1
c
      character preambl(5)*79,cline*80
      integer   i,ios,iunit,j,lgth,nrec
c
c --- w0 negative on first call only.
      if     (w0.lt.-1.0) then
c
c ---   initialize forcing fields
c
        if      (.not.mslprf) then
          if     (mnproc.eq.1) then
          write(lp,*)
          write(lp,*) 'error in forfunhp - mslprf must be .true.'
          write(lp,*)
          endif !1st tile
          call xcstop('(forfunhp)')
                 stop '(forfunhp)'
        elseif  (windf) then
          if     (mnproc.eq.1) then
          write(lp,*)
          write(lp,*) 'error in forfunhp - windf must be .false.'
          write(lp,*)
          endif !1st tile
          call xcstop('(forfunhp)')
                 stop '(forfunhp)'
        elseif (thermo) then
          if     (mnproc.eq.1) then
          write(lp,*)
          write(lp,*) 'error in forfunhp - thermo must be .false.'
          write(lp,*)
          endif !1st tile
          call xcstop('(forfunhp)')
                 stop '(forfunhp)'
        endif
c
        if     (natm.eq.4) then
c ---     linear interpolation in time, so slots 3 and 4 are zero.
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j=1-nbdy,jj+nbdy
            do i=1-nbdy,ii+nbdy
              mslprs(i,j,natm-1) = 0.0
              mslprs(i,j,natm)   = 0.0
            enddo
          enddo
        endif !natm.eq.4
c
c ---   open pressure forcing file.
        if     (mnproc.eq.1) then
        write (lp,*) ' now initializing forcing fields ...'
        endif !1st tile
        call xcsync(flush_lp)
c
        lgth = len_trim(flnmfor)
c
c-org        call zaiopf(flnmfor(1:lgth)//'forcing.mslprs.a', 'old', 899)
c-hsk 2017: change the filename to presur
        call zaiopf(flnmfor(1:lgth)//'forcing.presur.a', 'old', 899)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+899,file=flnmfor(1:lgth)//'forcing.presur.b',
     &     status='old', action='read')
        read (uoff+899,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
c
c ---   skip ahead to the start time.
        nrec   = 0
        dtime1 = huge(dtime1)
        do  ! infinate loop, with exit at end
          dtime0 = dtime1
          nrec   = nrec + 1
          call zagetc(cline,ios, uoff+899)
          if     (ios.ne.0) then
            if     (mnproc.eq.1) then
              write(lp,*)
              write(lp,*) 'error in forfunhp - hit end of input'
              write(lp,*) 'dtime0,dtime1 = ',dtime0,dtime1
              write(lp,*) 'dtime = ',dtime
              write(lp,*)
            endif !1st tile
            call xcstop('(forfunhp)')
                   stop '(forfunhp)'
          endif
          i = index(cline,'=')
          read (cline(i+1:),*) dtime1
          if     (yrflag.eq.2) then
            if     (nrec.eq.1 .and. abs(dtime1-1096.0d0).gt.0.01) then
c
c ---         climatology must start on wind day 1096.0, 01/01/1904.
              if     (mnproc.eq.1) then
              write(lp,'(a)')  cline
              write(lp,'(/ a,a / a,g15.6 /)')
     &          'error in forfunhp - forcing climatology',
     &          ' must start on wind day 1096',
     &          'dtime1 = ',dtime1
              endif !1st tile
              call xcstop('(forfunhp)')
                     stop '(forfunhp)'
            endif
            dtime1 = (dtime1 - 1096.0d0) + 
     &               wndrep*int((dtime+0.00001d0)/wndrep)  !wndrep=366 or 732
            if     (nrec.ne.1 .and. dtime1.lt.dtime0) then
              dtime1 = dtime1 + wndrep
            endif
          elseif (nrec.eq.1 .and. dtime1.lt.1462.0d0) then
c
c ---       otherwise, must start after wind day 1462.0, 01/01/1905.
            if     (mnproc.eq.1) then
            write(lp,'(a)')  cline
            write(lp,'(/ a,a / a,g15.6 /)')
     &        'error in forfunhp - actual forcing',
     &        ' must start after wind day 1462',
     &        'dtime1 = ',dtime1
            endif !1st tile
            call xcstop('(forfunhp)')
                   stop '(forfunhp)'
          endif
          if     (dtime0.le.dtime .and. dtime1.gt.dtime) then
            exit
          endif
        enddo   ! infinate loop, with exit above
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          rewind(unit=uoff+899)
          read (uoff+899,'(a79)') preambl
        endif
c
        do i= 1,nrec-2
          call skmonth(899)
        enddo
        dtime0 = huge(dtime1)
        call rdpall1(mslprs,dtime1,899,.true.)
        if     (yrflag.eq.2) then
          dtime1 = (dtime1 - 1096.0d0) + 
     &             wndrep*int((dtime+0.00001d0)/wndrep)
        endif
        dtime0 = dtime1
        call rdpall1(mslprs,dtime1,899,.true.)
        if     (yrflag.eq.2) then
          dtime1 = (dtime1 - 1096.0d0) + 
     &             wndrep*int((dtime+0.00001d0)/wndrep)  !wndrep=366 or 732
          if     (dtime1.lt.dtime0) then
            dtime1 = dtime1 + wndrep
          endif
        endif
        if     (mnproc.eq.1) then
        write (lp,*) 
        write (lp,*) ' dtime,dtime0,dtime1 = ',dtime,dtime0,dtime1
        write (lp,*) 
        write (lp,*) ' ...finished initializing forcing fields'
        endif !1st tile
        call xcsync(flush_lp)
      endif  ! initialization
c
      if     (dtime.gt.dtime1) then
c
c ---   get the next set of fields.
*           if     (mnproc.eq.1) then
*           write(lp,*) 'enter rdpall - ',dtime,dtime0,dtime1
*           endif !1st tile
*           call xcsync(flush_lp)
        dtime0 = dtime1
        call rdpall1(mslprs,dtime1,899,.true.)
        if     (yrflag.eq.2) then
          dtime1 = (dtime1 - 1096.0d0) + 
     &             wndrep*int((dtime+0.00001d0)/wndrep)  !wndrep=366 or 732
          if     (dtime1.lt.dtime0) then
            dtime1 = dtime1 + wndrep
          endif
        endif
*           if     (mnproc.eq.1) then
*           write(lp,*) ' exit rdpall - ',dtime,dtime0,dtime1
*           endif !1st tile
*           call xcsync(flush_lp)
      endif
c
c --- linear interpolation in time.
      w0 = (dtime1-dtime)/(dtime1-dtime0)
      w1 = 1.0 - w0
*           if     (mnproc.eq.1) then
*           write(lp,*) 'rdpall - dtime,w0,w1 = ',dtime,w0,w1
*           endif !1st tile
*           call xcsync(flush_lp)
      return
      end
c
c
      subroutine forfunhz
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
c --- high frequency atmospheric forcing field processing.
c --- set all fields to zero.
c
      integer   i,j,l
c
      if     (mnproc.eq.1) then
      write (lp,*) ' now zeroing forcing fields ...'
      endif !1st tile
      call xcsync(flush_lp)
c
!$OMP   PARALLEL DO PRIVATE(l,j,i)
!$OMP&           SCHEDULE(STATIC,jblk)
      do l=1,natm
        do j=1-nbdy,jj+nbdy
          do i=1-nbdy,ii+nbdy
              taux(i,j,l) = 0.0
              tauy(i,j,l) = 0.0
            wndspd(i,j,l) = 0.0
            ustara(i,j,l) = 0.0
            airtmp(i,j,l) = 0.0
            vapmix(i,j,l) = 0.0
            mslprs(i,j,l) = 0.0
            precip(i,j,l) = 0.0
            radflx(i,j,l) = 0.0
             swflx(i,j,l) = 0.0
            surtmp(i,j,l) = 0.0
            seatmp(i,j,l) = 0.0
          enddo !i
        enddo !j
      enddo !l
c
      if     (mnproc.eq.1) then
      write (lp,*) ' ...finished zeroing forcing fields'
      endif !1st tile
      call xcsync(flush_lp)
      return
      end
c
c
      subroutine forfunc
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
c --- initialize input of chl forcing field (into akpar array)
c --- call either forfunc or forfunk, not both
c
c --- units of chl are mg/m^3
c --- chl    is always on the p grid.
c
c --- I/O and array I/O unit 919 is reserved for the entire run.
c
c --- all input fields must be defined at all grid points
c
      integer         mreca,mrecc,mreck,mrecr
      common/rdforfi/ mreca,mrecc,mreck,mrecr
      save  /rdforfi/
c
      integer   l,lgth
      character preambl(5)*79
c
      mreck=1
      if     (mnproc.eq.1) then
      write (lp,*) ' now opening chl field  ...'
      endif !1st tile
      call xcsync(flush_lp)
c
      lgth = len_trim(flnmfor)
c
      if (thermo) then
        call zaiopf(flnmfor(1:lgth)//'forcing.chl.a', 'old', 919)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+919,file=flnmfor(1:lgth)//'forcing.chl.b',
     &     status='old', action='read')
        read (uoff+919,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        call rdmonth(util1, 919)
        if     (kparan) then
          if     (mnproc.eq.1) then
          write (lp,*)
          write (lp,*) '***** annual chl *****'
          write (lp,*)
          endif !1st tile
          call xcsync(flush_lp)
          do l= 1,4
            akpar(:,:,l) = util1(:,:)
          enddo
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
          close (unit=uoff+919)
          endif
          call zaiocl(919)
        endif !kparan
c
      else  ! .not.thermo
        kparan = .true.  
      endif                    !  thermo
c
      if     (mnproc.eq.1) then
      write (lp,*) ' ...finished opening chl field '
      endif !1st tile
      call xcsync(flush_lp)
c
      return
      end
c
c
      subroutine forfunk
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
c --- initialize input of kpar forcing field
c --- call either forfunc or forfunk, not both
c
c --- units of akpar are 1/m 
c --- akpar  is always on the p grid.
c
c --- I/O and array I/O unit 919 is reserved for the entire run.
c
c --- all input fields must be defined at all grid points
c
      integer         mreca,mrecc,mreck,mrecr
      common/rdforfi/ mreca,mrecc,mreck,mrecr
      save  /rdforfi/
c
      integer   l,lgth
      character preambl(5)*79
c
      mreck=1
      if     (mnproc.eq.1) then
      write (lp,*) ' now opening kpar field  ...'
      endif !1st tile
      call xcsync(flush_lp)
c
      lgth = len_trim(flnmfor)
c
      if (thermo) then
        call zaiopf(flnmfor(1:lgth)//'forcing.kpar.a', 'old', 919)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+919,file=flnmfor(1:lgth)//'forcing.kpar.b',
     &     status='old', action='read')
        read (uoff+919,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        call rdmonth(util1, 919)
        if     (kparan) then
          if     (mnproc.eq.1) then
          write (lp,*)
          write (lp,*) '***** annual kpar *****'
          write (lp,*)
          endif !1st tile
          call xcsync(flush_lp)
          do l= 1,4
            akpar(:,:,l) = util1(:,:)
          enddo
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
          close (unit=uoff+919)
          endif
          call zaiocl(919)
cdiag     call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,86400.*36000.,
cdiag.         'river precip. (cm/year) ')
        endif !kparan
c
      else  ! .not.thermo
        kparan = .true.  
      endif                    !  thermo
c
      if     (mnproc.eq.1) then
      write (lp,*) ' ...finished opening kpar field '
      endif !1st tile
      call xcsync(flush_lp)
c
      return
      end
c
c
      subroutine forfunp
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
c --- initialize input of river (precip bogas) forcing field
c
c --- units of rivers are m/s    (positive into ocean)
c --- rivers is always on the p grid.
c
c --- I/O and array I/O unit 918 is reserved for the entire run.
c
c --- all input fields must be defined at all grid points
c
      integer         mreca,mrecc,mreck,mrecr
      common/rdforfi/ mreca,mrecc,mreck,mrecr
      save  /rdforfi/
c
      logical   lexist
      integer   l,lgth
      character preambl(5)*79
c
      mrecr=1
      if     (mnproc.eq.1) then
      write (lp,*) ' now opening rivers  field  ...'
      endif !1st tile
      call xcsync(flush_lp)
c
      lgth = len_trim(flnmfor)
c
      if (thermo) then
c
      if     (.not.priver) then
        if     (mnproc.eq.1) then
        write (lp,*)
        write (lp,*) '***** no river precipitation *****'
        write (lp,*)
        endif !1st tile
        call xcsync(flush_lp)
        rivers(:,:,:) = 0.0
        rivera = .true.  
      else
        call zaiopf(flnmfor(1:lgth)//'forcing.rivers.a', 'old', 918)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+918,file=flnmfor(1:lgth)//'forcing.rivers.b',
     &     status='old', action='read')
        read (uoff+918,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        call rdmonth(util1, 918)
        if     (rivera) then
          if     (mnproc.eq.1) then
          write (lp,*)
          write (lp,*) '***** annual river precipitation *****'
          write (lp,*)
          endif !1st tile
          call xcsync(flush_lp)
          do l= 1,4
            rivers(:,:,l) = util1(:,:)
          enddo
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
          close (unit=uoff+918)
          endif
          call zaiocl(918)
cdiag     call prtmsk(ip,util1,util2,idm,idm,jdm,  0.,86400.*36000.,
cdiag.         'river precip. (cm/year) ')
        endif
      endif
c
      else  ! .not.thermo
        priver = .false.
        rivera = .true.  
      endif                    !  thermo
c
      if     (mnproc.eq.1) then
      write (lp,*) ' ...finished opening river   field '
      endif !1st tile
      call xcsync(flush_lp)
c
      return
      end
c
c
      subroutine forfunr
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
c --- initialize input of thermal/tracer relaxation forcing fields
c
c --- thmean is long term mean vertically averaged potential density
c --- sshgmn is long term mean sea surface height (m)
c
c --- sssrmx is maximum SSS difference for relaxation (psu)
c --- rmu    is a single field specifying 1 / e-folding time (1/s)
c ---         set to zero where there is no thermal boundary relaxation
c --- twall  is temperature climatology for all layers
c --- swall  is salinity    climatology for all layers
c --- pwall  is interface   climatology for all layers (pressure units, Pa)
c
c --- rmutr  is a single field specifying 1 / e-folding time (1/s)
c ---         set to zero where there is no tracer boundary relaxation
c --- trwall is tracer climatology for all layers and tracers
c
c --- I/O and array I/O units 911-914 are reserved for the entire run.
c --- I/O and array I/O unit  915 is used but not reserved.
c
c --- all input fields must be defined at all grid points
c
      integer         mreca,mrecc,mreck,mrecr
      common/rdforfi/ mreca,mrecc,mreck,mrecr
      save  /rdforfi/
c
      integer   lgth,mo
      character preambl(5)*79
      character cline*80
      real      one,oneps
      integer   i,incmon,ios,j,k,ktr,nrec
c
c --- initialize all rmu fields to zero
c
      rmu(   :,:) = 0.0  !needed for thermf
      rmutra(:,:) = 0.0
      rmunp( :,:) = 0.0
      rmunv( :,:) = 0.0  !needed for thermf
c
      lgth = len_trim(flnmforw)
c
      if     (sshflg.ne.0) then
        if     (mnproc.eq.1) then
        write (lp,*) ' now opening mean SSH   fields ...'
        endif !1st tile
        call xcsync(flush_lp)
        call zaiopf(flnmforw(1:lgth)//'relax.ssh.a', 'old', 915)
        call rdmonth(thmean, -915)  !no .b file
        call rdmonth(sshgmn, -915)  !no .b file
        call zaiocl(915)
        do j= 1,jj
          do i= 1,ii
            if     (ip(i,j).eq.1) then
              sshgmn(i,j) = sshgmn(i,j)*g  !input is mean ssh in m
            else
              thmean(i,j) = 0.0
              sshgmn(i,j) = 0.0
            endif
          enddo !i
        enddo !j
        call xctilr(thmean,1,1, nbdy,nbdy, halo_ps)
        call xctilr(sshgmn,1,1, nbdy,nbdy, halo_ps)
      endif !sshflg
c
      if (.not.relaxf) then
        return
      endif
c
c --- read fields needed for boundary and surface relaxation
c
      mrecc=1
      if     (mnproc.eq.1) then
      write (lp,*) ' now opening relaxation fields ...'
      endif !1st tile
      call xcsync(flush_lp)
c
      if     (sssflg.eq.-1) then  ! sss relaxation limiter
        call zaiopf(flnmforw(1:lgth)//'relax.sssrmx.a', 'old', 915)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+915,file=flnmforw(1:lgth)//'relax.sssrmx.b',
     &        status='old', action='read')
        read (uoff+915,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        call rdmonth(sssrmx, 915)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        close (unit=uoff+915)
        endif
        call zaiocl(915)
      else
        if     (mnproc.eq.1) then
        write (lp,*) 'No sss relaxation limiter.'
        endif !1st tile
        sssrmx(:,:) = 99.9  !needed for thermf, set to no limit
      endif
c
      if     (relax) then  ! boundary thermal relaxation
        call zaiopf(flnmforw(1:lgth)//'relax.rmu.a', 'old', 915)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+915,file=flnmforw(1:lgth)//'relax.rmu.b',
     &        status='old', action='read')
        read (uoff+915,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        call rdmonth(rmu, 915)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        close (unit=uoff+915)
        endif
        call zaiocl(915)
      else
        if     (mnproc.eq.1) then
        write (lp,*) 'No thermal relaxation mask.'
        endif !1st tile
      endif
c
      if     (trcrlx) then  ! boundary tracer relaxation
        call zaiopf(flnmforw(1:lgth)//'relax.rmutr.a', 'old', 915)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+915,file=flnmforw(1:lgth)//'relax.rmutr.b',
     &        status='old', action='read')
        endif !1st tile
        call zagetc(cline,ios, uoff+915)  !1st line of the header on all tiles
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        rewind uoff+915
        read  (uoff+915,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        call rdmonth(rmutra, 915)
        do j= 1,jj
          do i= 1,ii
            rmutr(i,j,1) = rmutra(i,j)
          enddo !i
        enddo !j
        if     (cline.eq.'Relaxation Masks') then  !multiple masks
          do ktr= 2,ntracr
            call rdmonth(rmutr(1-nbdy,1-nbdy,ktr), 915)
            do j= 1,jj
              do i= 1,ii
                rmutra(i,j) = max(rmutra(i,j), rmutr(i,j,ktr) )
              enddo !i
            enddo !j
          enddo !ktr
        else !one mask (repeated)
          do ktr= 2,ntracr
            do j= 1,jj
              do i= 1,ii
                rmutr(i,j,ktr) = rmutra(i,j)
              enddo !i
            enddo !j
          enddo !ktr
        endif !multiple or one mask(s)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        close (unit=uoff+915)
        endif
        call zaiocl(915)
      elseif (ntracr.gt.0) then
        if     (mnproc.eq.1) then
        write (lp,*) 'No tracer relaxation mask.'
        endif !1st tile
      endif
c
      call zaiopf(flnmforw(1:lgth)//'relax.temp.a', 'old', 911)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      open (unit=uoff+911,file=flnmforw(1:lgth)//'relax.temp.b',
     &      status='old', action='read')
      read (uoff+911,'(a79)') preambl
      endif !1st tile
      call preambl_print(preambl)
      if     (relaxs) then  ! surface only
        call rdmonth(util1, 911)
        do k=2,kk
          call skmonth(       911)
        enddo
      else
        do k=1,kk
          call rdmonth(util1, 911)
        enddo
      endif
c
      call zaiopf(flnmforw(1:lgth)//'relax.saln.a', 'old', 912)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      open (unit=uoff+912,file=flnmforw(1:lgth)//'relax.saln.b',
     &      status='old', action='read')
      read (uoff+912,'(a79)') preambl
      endif !1st tile
      call preambl_print(preambl)
      if     (relaxs) then  ! surface only
        call rdmonth(util1, 912)
        do k=2,kk
          call skmonth(       912)
        enddo
      else
        do k=1,kk
          call rdmonth(util1, 912)
        enddo
      endif
c
      if     (relaxt .or. .not.relaxs) then
        call zaiopf(flnmforw(1:lgth)//'relax.intf.a', 'old', 913)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+913,file=flnmforw(1:lgth)//'relax.intf.b',
     &        status='old', action='read')
        read (uoff+913,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        do k=1,kk
          call rdmonth(util1, 913)
        enddo
      endif
c
      if     (relaxt) then
        call zaiopf(flnmforw(1:lgth)//'relax.trcr.a', 'old', 914)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+914,file=flnmforw(1:lgth)//'relax.trcr.b',
     &        status='old', action='read')
        read (uoff+914,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        do ktr= 1,ntracr
          do k=1,kk
            call rdmonth(util1, 914)
          enddo
        enddo
      endif
c
      if     (mnproc.eq.1) then
      write (lp,*) ' ...finished opening relaxation fields'
      endif !1st tile
      call xcsync(flush_lp)
c
      return
      end
c
c
      subroutine forfuns
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
c --- initialize SAL factor field
c
c --- salfac is unitless and on the p-grid.
c
c --- I/O and array I/O unit 925 used here, but not reserved.
c
c --- all input fields must be defined at all grid points
c
      integer   i,j,k,l,lgth
c
      if     (mnproc.eq.1) then
      write (lp,*) ' now opening salfac field  ...'
      endif !1st tile
      call xcsync(flush_lp)
c
      lgth = len_trim(flnmfor)
c
      call zaiopf(flnmfor(1:lgth)//'tidal.sal.a', 'old', 925)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      open (unit=uoff+925,file=flnmfor(1:lgth)//'tidal.sal.b',
     &   status='old', action='read')
      endif !1st tile
      call rdmonth(salfac, 925)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      close (unit=uoff+925)
      endif
      call zaiocl(925)
c
      call xctilr(salfac,1,1, nbdy,nbdy, halo_ps)
c
      if     (mnproc.eq.1) then
      write (lp,*) ' ...finished opening salfac field '
      endif !1st tile
      call xcsync(flush_lp)
c
      return
      end
c
c
      subroutine forfunt
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
c --- initialize spacially varying minimum depth for isopycnal layers
c
c --- units of isotop are m on the p-grid.
c
c --- I/O and array I/O unit 924 used here, but not reserved.
c
c --- all input fields must be defined at all grid points
c
      integer   k,l,lgth
c
      if     (mnproc.eq.1) then
      write (lp,*) ' now opening isotop field  ...'
      endif !1st tile
      call xcsync(flush_lp)
c
      lgth = len_trim(flnmfor)
c
      call zaiopf(flnmfor(1:lgth)//'iso.top.a', 'old', 924)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      open (unit=uoff+924,file=flnmfor(1:lgth)//'iso.top.b',
     &   status='old', action='read')
      endif !1st tile
      call rdmonth(topiso, 924)
      vland = -isotop  !should be the typical shallowest depth (m)
      call xctilr( topiso,1,1, nbdy,nbdy, halo_ps)
      vland = 0.0
c --- convert to pressure units (Pa).
      topiso(:,:) = onem*topiso(:,:)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      close (unit=uoff+924)
      endif
      call zaiocl(924)
c
      if     (mnproc.eq.1) then
      write (lp,*) ' ...finished opening isotop field '
      endif !1st tile
      call xcsync(flush_lp)
c
      return
      end
c
c
      subroutine forfunv
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
c --- initialize spacially varying isopycnal target densities
c
c --- units of sigma are sigma-theta or sigma-2.
c --- sigma  is always on the p grid.
c
c --- I/O and array I/O unit 922 used here, but not reserved.
c
c --- all input fields must be defined at all grid points
c
      integer   k,l,lgth
c
      if     (mnproc.eq.1) then
      write (lp,*) ' now opening sigma field  ...'
      endif !1st tile
      call xcsync(flush_lp)
c
      lgth = len_trim(flnmfor)
c
      call zaiopf(flnmfor(1:lgth)//'iso.sigma.a', 'old', 922)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      open (unit=uoff+922,file=flnmfor(1:lgth)//'iso.sigma.b',
     &   status='old', action='read')
      endif !1st tile
      do k= 1,kk
        call rdmonth(util1, 922)
        vland = sigma(k)
        call xctilr( util1, 1,1, nbdy,nbdy, halo_ps)
        vland = 0.0
c ---   subtract constant 'thbase' from theta to reduce roundoff errors
        theta(:,:,k) = util1(:,:) - thbase
      enddo
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      close (unit=uoff+922)
      endif
      call zaiocl(922)
c
      if     (mnproc.eq.1) then
      write (lp,*) ' ...finished opening sigma field '
      endif !1st tile
      call xcsync(flush_lp)
c
      return
      end
c
c
      subroutine forfundf
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
c --- initialize spacially varying veldf2, veldf4, thkdf4, and cbar,
c ---  if necessary,
c ---  and calculate laplacian and biharmonic diffusion coefficients.
c --- also initialize spacially varying diws, diwm, diwbot and diwqh0,
c ---   if necessary.
c
c --- veldf2 is diffusion velocity for laplacian  background diffusion
c --- veldf4 is diffusion velocity for biharmonic background diffusion
c --- thkdf4 is diffusion velocity for biharmonic thickness  diffusion
c --- cbar   is rms flow speed     for linear bottom friction
c --- all units are (m/s), and all are always on the p grid.
c
c --- diws   is background/internal wave diffusivity (m**2/s) on the p grid.
c --- diwm   is background/internal wave viscosity   (m**2/s) on the p grid.
c --- diwbot is background diffusivity at the bottom (m**2/s) on the p grid.
c --- diwqh0 is vertical scale for background diffusivity (m) on the p grid.
c --- diwqh0 is input as m, but pre-processed here to 1/pressure-units
c
c --- I/O and array I/O unit 923 used here, but not reserved.
c
c --- all input fields must be defined at all grid points
c
      integer   i,j,l,lgth
c
      if     (veldf2.ge.0.0) then
        util2(:,:) = veldf2
      else
        if     (mnproc.eq.1) then
        write (lp,*) ' now opening veldf2 field  ...'
        endif !1st tile
        call xcsync(flush_lp)
c
        lgth = len_trim(flnmfor)
c
        call zaiopf(flnmfor(1:lgth)//'veldf2.a', 'old', 923)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+923,file=flnmfor(1:lgth)//'veldf2.b',
     &     status='old', action='read')
        endif !1st tile
        call rdmonth(util2, 923)
        call xctilr( util2, 1,1,    1,   1, halo_ps)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        close (unit=uoff+923)
        endif !1st tile
        call zaiocl(923)
c
        if     (mnproc.eq.1) then
        write (lp,*) ' ...finished opening veldf2 field '
        endif !1st tile
        call xcsync(flush_lp)
      endif !veldf2
c
      if     (veldf4.ge.0.0) then
        util3(:,:) = veldf4
      else
        if     (mnproc.eq.1) then
        write (lp,*) ' now opening veldf4 field  ...'
        endif !1st tile
        call xcsync(flush_lp)
c
        lgth = len_trim(flnmfor)
c
        call zaiopf(flnmfor(1:lgth)//'veldf4.a', 'old', 923)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+923,file=flnmfor(1:lgth)//'veldf4.b',
     &     status='old', action='read')
        endif !1st tile
        call rdmonth(util3, 923)
        call xctilr( util3, 1,1,    1,   1, halo_ps)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        close (unit=uoff+923)
        endif !1st tile
        call zaiocl(923)
c
        if     (mnproc.eq.1) then
        write (lp,*) ' ...finished opening veldf4 field '
        endif !1st tile
        call xcsync(flush_lp)
      endif !veldf4
c
      if     (thkdf4.ge.0.0) then
        util1(:,:) = thkdf4
      else
        if     (mnproc.eq.1) then
        write (lp,*) ' now opening thkdf4 field  ...'
        endif !1st tile
        call xcsync(flush_lp)
c
        lgth = len_trim(flnmfor)
c
        call zaiopf(flnmfor(1:lgth)//'thkdf4.a', 'old', 923)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+923,file=flnmfor(1:lgth)//'thkdf4.b',
     &     status='old', action='read')
        endif !1st tile
        call rdmonth(util1, 923)
        call xctilr( util1, 1,1,    1,   1, halo_ps)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        close (unit=uoff+923)
        endif !1st tile
        call zaiocl(923)
c
        if     (mnproc.eq.1) then
        write (lp,*) ' ...finished opening thkdf4 field '
        endif !1st tile
        call xcsync(flush_lp)
      endif !thkdf4
c
      if     (cbar.ge.0.0) then
        cbarp(:,:) = cbar
      else
        if     (mnproc.eq.1) then
        write (lp,*) ' now opening cbar field  ...'
        endif !1st tile
        call xcsync(flush_lp)
c
        lgth = len_trim(flnmfor)
c
        call zaiopf(flnmfor(1:lgth)//'cbar.a', 'old', 923)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+923,file=flnmfor(1:lgth)//'cbar.b',
     &     status='old', action='read')
        endif !1st tile
        call rdmonth(cbarp, 923)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        close (unit=uoff+923)
        endif !1st tile
        call zaiocl(923)
c
        call xctilr( cbarp, 1,1, nbdy,nbdy, halo_ps)
c
        if     (mnproc.eq.1) then
        write (lp,*) ' ...finished opening cbar field '
        endif !1st tile
        call xcsync(flush_lp)
      endif !cbar
c
      if     (cb.ge.0.0) then
        cbp(:,:) = cb
      else
        if     (mnproc.eq.1) then
        write (lp,*) ' now opening cb field  ...'
        endif !1st tile
        call xcsync(flush_lp)
c
        lgth = len_trim(flnmfor)
c
        call zaiopf(flnmfor(1:lgth)//'cb.a', 'old', 923)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+923,file=flnmfor(1:lgth)//'cb.b',
     &     status='old', action='read')
        endif !1st tile
        call rdmonth(cbp, 923)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        close (unit=uoff+923)
        endif !1st tile
        call zaiocl(923)
c
        call xctilr( cbp, 1,1, nbdy,nbdy, halo_ps)
c
        if     (mnproc.eq.1) then
        write (lp,*) ' ...finished opening cb field '
        endif !1st tile
        call xcsync(flush_lp)
      endif !cbar
c
      if     (difsiw.ge.0.0) then !difmiw also +ve
        diws(:,:) = difsiw
        diwm(:,:) = difmiw
      else
        if     (mnproc.eq.1) then
        write (lp,*) ' now opening diws and diwm fields  ...'
        endif !1st tile
        call xcsync(flush_lp)
c
        lgth = len_trim(flnmfor)
c
        call zaiopf(flnmfor(1:lgth)//'diwsm.a', 'old', 923)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+923,file=flnmfor(1:lgth)//'diwsm.b',
     &     status='old', action='read')
        endif !1st tile
        call rdmonth(diws, 923)
        call xctilr( diws, 1,1,    1,   1, halo_ps)
        call rdmonth(diwm, 923)
        call xctilr( diwm, 1,1,    1,   1, halo_ps)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        close (unit=uoff+923)
        endif !1st tile
        call zaiocl(923)
c
        if     (mnproc.eq.1) then
        write (lp,*) ' ...finished opening diws and diwm fields '
        endif !1st tile
        call xcsync(flush_lp)
      endif !difsiw
c
      if     (botdiw) then
        if     (mnproc.eq.1) then
        write (lp,*) ' now opening diwbot fields  ...'
        endif !1st tile
        call xcsync(flush_lp)
c
        lgth = len_trim(flnmfor)
c
        call zaiopf(flnmfor(1:lgth)//'diwbot.a', 'old', 923)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+923,file=flnmfor(1:lgth)//'diwbot.b',
     &     status='old', action='read')
        endif !1st tile
        call rdmonth(diwbot, 923)
        call rdmonth(diwqh0, 923)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        close (unit=uoff+923)
        endif !1st tile
        call zaiocl(923)
c
        call xctilr( diwbot, 1,1, nbdy,nbdy, halo_ps)
        call xctilr( diwqh0, 1,1, nbdy,nbdy, halo_ps)
        diwqh0(:,:) = 1.0 / (diwqh0(:,:)*onem) !1/h0 in pressure units
c
        if     (mnproc.eq.1) then
        write (lp,*) ' ...finished opening diwbot fields '
        endif !1st tile
        call xcsync(flush_lp)
      endif !botdiw
c
c --- diffusion coeficients
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j= 1,jj
        do i= 1,ii
          if     (iu(i,j).eq.1) then
            veldf2u(i,j) = 0.5*(util2(i,j)+util2(i-1,j))*
     &                          aspux(i,j)
            veldf4u(i,j) = 0.5*(util3(i,j)+util3(i-1,j))*
     &                          aspux(i,j)
            thkdf4u(i,j) = 0.5*(util1(i,j)+util1(i-1,j))*
     &                         (aspux(i,j)**3)*scuy(i,j)
          else
            veldf2u(i,j) = 0.0
            veldf4u(i,j) = 0.0
            thkdf4u(i,j) = 0.0
          endif
          if     (iv(i,j).eq.1) then
            veldf2v(i,j) = 0.5*(util2(i,j)+util2(i,j-1))*
     &                          aspvy(i,j)
            veldf4v(i,j) = 0.5*(util3(i,j)+util3(i,j-1))*
     &                          aspvy(i,j)
            thkdf4v(i,j) = 0.5*(util1(i,j)+util1(i,j-1))*
     &                         (aspvy(i,j)**3)*scvx(i,j)
          else
            veldf2v(i,j) = 0.0
            veldf4v(i,j) = 0.0
            thkdf4v(i,j) = 0.0
          endif
        enddo !i
      enddo !j
      call xctilr(veldf2u, 1,1, nbdy,nbdy, halo_us)
      call xctilr(veldf4u, 1,1, nbdy,nbdy, halo_us)
      call xctilr(thkdf4u, 1,1, nbdy,nbdy, halo_us)
      call xctilr(veldf2v, 1,1, nbdy,nbdy, halo_vs)
      call xctilr(veldf4v, 1,1, nbdy,nbdy, halo_vs)
      call xctilr(thkdf4v, 1,1, nbdy,nbdy, halo_vs)
c
      return
      end
c
c
      subroutine preambl_print(preambl)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      character preambl(5)*79
c
c --- print non-blank lines of preambl
c
      integer i
c
      if     (mnproc.eq.1) then
        write(lp,*)
        do i= 1,5
          if     (len_trim(preambl(i)).ne.0) then
            write(lp,'(a)') trim(preambl(i))
          endif
        enddo
      endif !1st tile
      call xcsync(flush_lp)
      return
      end
c
c
      subroutine rdmonth(field, iunit)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      integer   iunit
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
     &          field
c
c --- read a single array field from unit iunit (see also rdmonthck).
c --- ignore the input month, so long as it is between 1 and 12.
c
c --- iunit=900-910; atmospheric forcing field
c --- iunit=911-914; relaxation  forcing field
c --- iunit=915;     relaxation  time scale field
c --- iunit=918;     river       forcing field
c --- iunit=919;     kpar or chl forcing field
c --- iunit=922;     isopycnal target density field
c --- iunit=923;     laplacian or biharmonic diffusion velocity field
c --- iunit=924;     minimum depth for isopycnal layers
c --- iunit=925;     tidal drag (dragrh or drgten) or SAL (salfac)
c
c --- most of work now done by rdmonthck
c
      call rdmonthck(field, iunit, 0)
      return
      end
c
c
      subroutine rdmonthck(field, iunit, mnthck)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      integer   iunit,mnthck
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
     &          field
c
c --- read a single array field from unit iunit (see also rdmonth).
c --- check the input month against mnthck, if mnthck>0
c
c --- iunit=900-910; atmospheric forcing field
c --- iunit=911-914; relaxation  forcing field
c --- iunit=915;     relaxation  time scale field
c --- iunit=916;     offset      forcing field
c --- iunit=918;     river       forcing field
c --- iunit=919;     kpar or chl forcing field
c --- iunit=922;     isopycnal target density field
c --- iunit=923;     laplacian or biharmonic diffusion velocity field
c --- iunit=924;     minimum depth for isopycnal layers
c --- iunit=925;     tidal drag (dragrh or drgten) or SAL (salfac)
c
      integer   i,ios,layer,mnth
      real      denlay,hmina,hminb,hmaxa,hmaxb
      character cline*80
c
      if     (iunit.lt.0) then
c
c ---   special case, no .b file.
c
        call zaiord(field,ip,.false., hmina,hmaxa,
     &              -iunit)
        return
      endif
c
      call zagetc(cline,ios, uoff+iunit)
      if     (ios.ne.0) then
        if     (mnproc.eq.1) then
          write(lp,*)
          write(lp,*) 'error in rdmonth - hit end of input'
          write(lp,*) 'iunit,ios = ',iunit,ios
          write(lp,*)
        endif !1st tile
        call xcstop('(rdmonth)')
               stop '(rdmonth)'
      endif
      if     (mnproc.eq.1) then
      write (lp,'(a)')  cline  !print input array info
      endif !1st tile
      i = index(cline,'=')
      if     (iunit.ge.900 .and. iunit.le.910) then
c ---   atmospheric forcing
        read (cline(i+1:),*) mnth,hminb,hmaxb
        if     (mnth.lt.1 .or. mnth.gt.12) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a,i4,a /)')
     &      'error on unit',iunit,' - not monthly atmospheric data'
          endif !1st tile
          call xcstop('(rdmonth)')
                 stop '(rdmonth)'
        endif
        if     (mnthck.gt.0 .and. mnth.ne.mnthck) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a,i4,a,a,2i4,a /)')
     &      'error on unit',iunit,' - wrong atmospheric month',
     &      ' (expected,input =',mnthck,mnth,')'
          endif !1st tile
          call xcstop('(rdmonth)')
                 stop '(rdmonth)'
        endif
      elseif (iunit.eq.916) then
c ---   time-invarient heat flux correction
        read (cline(i+1:),*) hminb,hmaxb
      elseif (iunit.ge.911 .and. iunit.le.914) then
c ---   relaxation forcing
        read (cline(i+1:),*) mnth,layer,denlay,hminb,hmaxb
        if     (mnth.lt.1 .or. mnth.gt.12) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a,i4,a /)') 
     &      'error on unit',iunit,' - not monthly relaxation data'
          endif !1st tile
          call xcstop('(rdmonth)')
                 stop '(rdmonth)'
        endif
        if     (mnthck.gt.0 .and. mnth.ne.mnthck) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a,i4,a,a,2i4,a /)')
     &      'error on unit',iunit,' - wrong relaxation month',
     &      ' (expected,input =',mnthck,mnth,')'
          endif !1st tile
          call xcstop('(rdmonth)')
                 stop '(rdmonth)'
        endif
      elseif (iunit.eq.919) then
c ---   kpar or chl forcing
        kparan = cline(i-8:i) .eq. ': range ='
        if     (kparan) then
c ---     annual
          read (cline(i+1:),*) hminb,hmaxb
        else
c ---     monthly
          read (cline(i+1:),*) mnth,hminb,hmaxb
          if     (mnth.lt.1 .or. mnth.gt.12) then
            if     (mnproc.eq.1) then
            write(lp,'(/ a,i4,a /)') 
     &        'error on unit',iunit,' - not monthly kpar or chl data'
            endif !1st tile
            call xcstop('(rdmonth)')
                   stop '(rdmonth)'
          endif
          if     (mnthck.gt.0 .and. mnth.ne.mnthck) then
            if     (mnproc.eq.1) then
            write(lp,'(/ a,i4,a,a,2i4,a /)')
     &       'error on unit',iunit,' - wrong kpar month',
     &       ' (expected,input =',mnthck,mnth,')'
           endif !1st tile
           call xcstop('(rdmonth)')
                  stop '(rdmonth)'
         endif
        endif
      elseif (iunit.eq.918) then
c ---   river forcing
        rivera = cline(i-8:i) .eq. ': range ='
        if     (rivera) then
c ---     annual
          read (cline(i+1:),*) hminb,hmaxb
        else
c ---     monthly
          read (cline(i+1:),*) mnth,hminb,hmaxb
          if     (mnth.lt.1 .or. mnth.gt.12) then
            if     (mnproc.eq.1) then
            write(lp,'(/ a,i4,a /)') 
     &        'error on unit',iunit,' - not monthly river data'
            endif !1st tile
            call xcstop('(rdmonth)')
                   stop '(rdmonth)'
          endif
          if     (mnthck.gt.0 .and. mnth.ne.mnthck) then
            if     (mnproc.eq.1) then
            write(lp,'(/ a,i4,a,a,2i4,a /)')
     &       'error on unit',iunit,' - wrong river month',
     &       ' (expected,input =',mnthck,mnth,')'
           endif !1st tile
           call xcstop('(rdmonth)')
                  stop '(rdmonth)'
         endif
        endif
      elseif (iunit.eq.915) then
c ---   relaxation time scale
        read (cline(i+1:),*) hminb,hmaxb
      elseif (iunit.eq.922) then
c ---   target density field.
        read (cline(i+1:),*) layer,hminb,hmaxb
        if     (hminb.gt.sigma(layer)+0.005 .or.
     &          hmaxb.lt.sigma(layer)-0.005     ) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a,i4,a /)') 
     &      'error on unit',iunit,' - not consistent with sigma(k)'
          endif !1st tile
          call xcstop('(rdmonth)')
                 stop '(rdmonth)'
        endif
      elseif (iunit.eq.923) then
c ---   laplacian or biharmonic diffusion velocity field
        read (cline(i+1:),*) hminb,hmaxb
      elseif (iunit.eq.924) then
c ---   minimum depth for isopycnal layers
        read (cline(i+1:),*) hminb,hmaxb
      elseif (iunit.eq.925) then
c ---   tidal drag roughness or SAL
        read (cline(i+1:),*) hminb,hmaxb
      else
        if     (mnproc.eq.1) then
        write(lp,'(a,a / a,i5)')
     &    'error - iunit must be 900-910 or 911-916',
     &                       'or 918-919 or 922-925',
     &    'iunit =',iunit
        endif !1st tile
        call xcstop('(rdmonth)')
               stop '(rdmonth)'
      endif
c
      if     (hminb.eq.hmaxb) then  !constant field
        field(:,:) = hminb
        call zaiosk(iunit)
      else
        call zaiord(field,ip,.false., hmina,hmaxa,
     &              iunit)
c
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,i3 / a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      'iunit = ',iunit,
     &      cline,
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          endif !1st tile
          call xcstop('(rdmonth)')
                 stop '(rdmonth)'
        endif
      endif
c
      return
      end
c
c
      subroutine skmonth(iunit)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      integer   iunit
c
c --- skip a single array field from unit iunit.
c
c --- iunit=900-910; atmospheric forcing field
c --- iunit=911-914; relaxation  forcing field
c --- iunit=915;     relaxation strength field
c --- iunit=918;     river       forcing field
c --- iunit=919;     kpar or chl forcing field
c
      character cline*80
c
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
        read (uoff+iunit,'(a)')  cline
*       write(lp,   '(a)')  cline
      endif
c
      call zaiosk(iunit)
c
      return
      end
c
c
      subroutine rdpall(dtime0,dtime1)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      real*8  dtime0,dtime1
c
c --- copy slot 2 into slot 1, and 
c --- read a set of high frequency forcing fields into slot 2.
c --- on exit, dtime0 and dtime1 are the associated times (wind days).
c
      integer i,j,k
      real    albw,degtorad
      real*8  dtime(899:910)
c
      integer, save :: icall = -1
c
      real, parameter :: sstmin = -1.8
      real, parameter :: sstmax = 35.0
c
      icall = icall + 1
c
      call rdpall1(  taux,dtime(901),901,mod(icall,3).eq.0)
      call rdpall1(  tauy,dtime(902),902,mod(icall,3).eq.0)
      if     (ustflg.eq.3) then
        call rdpall1(ustara,dtime(900),900,mod(icall,3).eq.0)
      else
        dtime(900) = dtime(901)
      endif
      if     (wndflg.lt.3) then
        call rdpall1(wndspd,dtime(903),903,mod(icall,3).eq.0)
      elseif (wndflg.ge.4) then
c ---   taux,tauy contains wndx,wndy
        dtime(903) = dtime(902)
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j= 1-nbdy,jj+nbdy
          do i= 1-nbdy,ii+nbdy
            wndspd(i,j,1) = wndspd(i,j,2)
            wndspd(i,j,2) = sqrt( taux(i,j,2)**2 + tauy(i,j,2)**2 )
          enddo
        enddo
      else
        dtime(903) = dtime(902)
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j= 1-nbdy,jj+nbdy
          do i= 1-nbdy,ii+nbdy
            wndspd(i,j,1) = wndspd(i,j,2)
          enddo
        enddo
        call str2spd(wndspd(1-nbdy,1-nbdy,2),
     &                 taux(1-nbdy,1-nbdy,2),
     &                 tauy(1-nbdy,1-nbdy,2) )
      endif !wndspd
      if     (flxflg.ne.3) then
        call rdpall1(airtmp,dtime(904),904,mod(icall,3).eq.1)
        call rdpall1(vapmix,dtime(905),905,mod(icall,3).eq.1)
      else
        dtime(904) = dtime(900)
        dtime(905) = dtime(900)
      endif
      if     (mslprf .or. flxflg.eq.6) then
        call rdpall1(mslprs,dtime(899),899,mod(icall,3).eq.2)
      else
        dtime(899) = dtime(905)
      endif
      if     (pcipf) then
        call rdpall1(precip,dtime(906),906,mod(icall,3).eq.1)
      else
        dtime(906) = dtime(905)
      endif
      call rdpall1(radflx,dtime(907),907,mod(icall,3).eq.2)
      call rdpall1( swflx,dtime(908),908,mod(icall,3).eq.2)
      if     (albflg.ne.0) then  !swflx is Qswdn
c ---   convert swflx to net shortwave into the ocean
c ---   shortwave through sea ice is handled separately
        if     (albflg.eq.1) then
          do j= 1-nbdy,jj+nbdy
            do i= 1-nbdy,ii+nbdy
              swflx(i,j,2) = swflx(i,j,2)*(1.0-0.09)  !NAVGEM albedo
            enddo
          enddo
        else   !albflg.eq.2
          degtorad = 4.d0*atan(1.d0)/180.d0
          do j= 1-nbdy,jj+nbdy
            do i= 1-nbdy,ii+nbdy
c ---         latitudinally-varying ocean albedo (Large and Yeager, 2009)
c ---         5.8% at the equator and 8% at the poles
              albw = ( 0.069 - 0.011*cos(2.0*degtorad*plat(i,j) ) )
              swflx(i,j,2) = swflx(i,j,2)*(1.0-albw)
            enddo
          enddo
        endif !albflg
      endif !Qswdn
      if     (lwflag.eq.2 .or. sstflg.eq.2   .or.
     &        icmflg.eq.2 .or. ticegr.eq.0.0     ) then
        call rdpall1(surtmp,dtime(909),909,mod(icall,3).eq.2)
        if     (sstflg.ne.3) then  !use atmos. sst as "truth"
          do j= 1-nbdy,jj+nbdy
            do i= 1-nbdy,ii+nbdy
              seatmp(i,j,1) = max( sstmin, min(surtmp(i,j,1), sstmax ) )
              seatmp(i,j,2) = max( sstmin, min(surtmp(i,j,2), sstmax ) )
            enddo
          enddo
        endif
      else
        dtime(909) = dtime(905)
      endif !surtmp:else
      if     (sstflg.eq.3) then
        call rdpall1(seatmp,dtime(910),910,mod(icall,3).eq.2)
      else
        dtime(910) = dtime(905)
      endif
c
      dtime0 = dtime1
      dtime1 = dtime(901)
c
c --- check the input times.
      do k= 902,910
        if     (dtime(k).ne.dtime1) then
          if     (mnproc.eq.1) then
          write(lp,*)
          write(lp,*) 'error in rdpall - inconsistent forcing times'
          write(lp,*) 'dtime0,dtime1 = ',dtime0,dtime1
          write(lp,*) 'dtime = ',dtime
          write(lp,*)
          endif !1st tile
          call xcstop('(rdpall)')
                 stop '(rdpall)'
        endif
      enddo
      return
      end
c
c
      subroutine rdpall1(field,dtime,iunit,lprint)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2) ::
     &        field
      real*8  dtime
      integer iunit
      logical lprint
c
c --- copy field(:,:,2) into field(:,:,1), and
c --- read a high frequency forcing field into field(:,:,2).
c --- on exit, dtime is the time (wind day) of the forcing field.
c
      integer   i,ios,j
      character cline*80
      real      hmina,hminb,hmaxa,hmaxb,span
c
      call zagetc(cline,ios, uoff+iunit)
      if     (ios.lt.0) then  ! e-o-f
        if     (yrflag.eq.2) then
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
*           write(lp,*) 'rdpall1 - rewind unit ',iunit
*           call flush(lp)
            rewind uoff+iunit
            read (uoff+iunit,*)
            read (uoff+iunit,*)
            read (uoff+iunit,*)
            read (uoff+iunit,*)
            read (uoff+iunit,*)
          endif
          call zaiorw(iunit)
          call zagetc(cline,ios, uoff+iunit)
        else
          if     (mnproc.eq.1) then
          write(lp,'(/ a,i4 /)')
     &      'end of file from zagetc, iunit = ',iunit
          endif !1st tile
          call xcstop('(rdpall1)')
                 stop '(rdpall1)'
        endif
      endif  ! e-o-f
      if     (ios.gt.0) then
        if     (mnproc.eq.1) then
          write(lp,'(/ a,i4,i9 /)')
     &      'I/O error from zagetc, iunit,ios = ',iunit,ios
        endif !1st tile
        call xcstop('(rdpall1)')
               stop '(rdpall1)'
      endif
      if     (lprint) then
        if     (mnproc.eq.1) then
        write (lp,'(a)') cline
        endif !1st tile
      endif !lprint
c
      i = index(cline,'=')
      read (cline(i+1:),*) dtime,span,hminb,hmaxb
      if     (span.gt.0.008d0) then
c ---   correct wind day to nearest 15 minutes
        dtime = nint(dtime*96.d0)/96.d0
      endif
c
      if     (hminb.eq.hmaxb) then  !constant field
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j= 1-nbdy,jj+nbdy
          do i= 1-nbdy,ii+nbdy
            field(i,j,1) = field(i,j,2)
            field(i,j,2) = hminb
          enddo
        enddo
        call zaiosk(iunit)
      else  !input field
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j= 1-nbdy,jj+nbdy
          do i= 1-nbdy,ii+nbdy
            field(i,j,1) = field(i,j,2)
          enddo
        enddo
        call zaiord(field(1-nbdy,1-nbdy,2),ip,.false., hmina,hmaxa,
     &              iunit)
c
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,i3 / a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      'iunit = ',iunit,
     &      cline,
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          endif !1st tile
          call xcstop('(rdpall1)')
                 stop '(rdpall1)'
        endif
      endif
c
c --- msl pressure uses the the halo.
c
      if     (iunit.eq.899) then
        call xctilr(field(1-nbdy,1-nbdy,2),1,1, nbdy,nbdy, halo_ps)
      endif
c
c --- wind stress uses the the halo.
c
      if     (iunit.eq.901 .and. wndflg.eq.1) then  ! taux on u-grid
        call xctilr(field(1-nbdy,1-nbdy,2),1,1, nbdy,nbdy, halo_uv)
      elseif (iunit.eq.901) then                    ! taux on p-grid
        call xctilr(field(1-nbdy,1-nbdy,2),1,1, nbdy,nbdy, halo_pv)
      elseif (iunit.eq.902 .and. wndflg.eq.1) then  ! tauy on v-grid
        call xctilr(field(1-nbdy,1-nbdy,2),1,1, nbdy,nbdy, halo_vv)
      elseif (iunit.eq.902) then                    ! tauy on p-grid
        call xctilr(field(1-nbdy,1-nbdy,2),1,1, nbdy,nbdy, halo_pv)
      endif
      return
      end
c
c
      subroutine rdforf(mnth,lslot)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      integer lslot,mnth
c
c --- read forcing functions for one month.
c
      integer         mreca,mrecc,mreck,mrecr
      common/rdforfi/ mreca,mrecc,mreck,mrecr
      save  /rdforfi/
c
      integer i,irec,iunit,j
      real    albw,degtorad
c
      real, parameter :: sstmin = -1.8
      real, parameter :: sstmax = 35.0
c
      if     (mnth.le.mreca) then
c
c ---   rewind all units
c
        if     (mslprf .or. flxflg.eq.6) then
          iunit = 899
            if     (mnproc.eq.1) then  ! .b file from 1st tile only
              rewind uoff+iunit
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
            endif
            call zaiorw(iunit)
        endif
        if     (ustflg.eq.3) then
          iunit = 900
            if     (mnproc.eq.1) then  ! .b file from 1st tile only
              rewind uoff+iunit
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
            endif
            call zaiorw(iunit)
        endif
        if     (windf) then
          do iunit= 901,902
            if     (mnproc.eq.1) then  ! .b file from 1st tile only
              rewind uoff+iunit
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
            endif
            call zaiorw(iunit)
          enddo
        endif
        if     (thermo) then
          do iunit= max(903,901+min(wndflg,3)),908  !904,908 when wndflg>=3
            if     (mnproc.eq.1) then  ! .b file from 1st tile only
              rewind uoff+iunit
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
            endif
            call zaiorw(iunit)
          enddo
        endif
        if     (lwflag.eq.2 .or. sstflg.eq.2   .or.
     &          icmflg.eq.2 .or. ticegr.eq.0.0     ) then
          iunit = 909
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
            rewind uoff+iunit
            read  (uoff+iunit,*)
            read  (uoff+iunit,*)
            read  (uoff+iunit,*)
            read  (uoff+iunit,*)
            read  (uoff+iunit,*)
          endif
          call zaiorw(iunit)
        endif !surtmp
        if     (sstflg.eq.3) then
          iunit = 910
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
            rewind uoff+iunit
            read  (uoff+iunit,*)
            read  (uoff+iunit,*)
            read  (uoff+iunit,*)
            read  (uoff+iunit,*)
            read  (uoff+iunit,*)
          endif
          call zaiorw(iunit)
        endif
*       if     (mnproc.eq.1) then
*       write(lp,*) 'rdforf: mreca,mnth = ',mreca,mnth,'  (rewind)'
*       endif !1st tile
*       call xcsync(flush_lp)
        mreca = 0
      endif
c
c --- skip forward to desired month
c
      do irec= mreca+1,mnth-1
*       if     (mnproc.eq.1) then
*       write(lp,*) 'rdforf: mreca,mnth = ',mreca,mnth,
*    &              '  (skipping ',irec,')'
*       endif !1st tile
*       call xcsync(flush_lp)
        if     (mslprf .or. flxflg.eq.6) then
          call skmonth(899)
        endif
        if     (ustflg.eq.3) then
          call skmonth(900)
        endif
        if     (windf) then
          do iunit= 901,902
            call skmonth(iunit)
          enddo
        endif
        if     (thermo) then
          do iunit= max(903,901+min(wndflg,3)),908  !904,908 when wndflg>=3
            call skmonth(iunit)
          enddo
        endif
        if     (lwflag.eq.2 .or. sstflg.eq.2   .or.
     &          icmflg.eq.2 .or. ticegr.eq.0.0     ) then
          call skmonth(909)
        endif !surtmp
        if     (sstflg.eq.3) then
          call skmonth(910)
        endif
      enddo
c
c --- read desired month
c
      if     (mslprf .or. flxflg.eq.6) then
        call rdmonthck(mslprs(1-nbdy,1-nbdy,lslot),899,mnth)
        call xctilr(mslprs(1-nbdy,1-nbdy,lslot),1,1, nbdy,nbdy, halo_ps)
      endif
      if     (windf) then
        call rdmonthck(taux(1-nbdy,1-nbdy,lslot),901,mnth)
        call rdmonthck(tauy(1-nbdy,1-nbdy,lslot),902,mnth)
        if     (wndflg.eq.1) then  !on uv-grids
          call xctilr(taux(1-nbdy,1-nbdy,lslot),1,1, nbdy,nbdy, halo_uv)
          call xctilr(tauy(1-nbdy,1-nbdy,lslot),1,1, nbdy,nbdy, halo_vv)
        else                       !on p-grid
          call xctilr(taux(1-nbdy,1-nbdy,lslot),1,1, nbdy,nbdy, halo_pv)
          call xctilr(tauy(1-nbdy,1-nbdy,lslot),1,1, nbdy,nbdy, halo_pv)
        endif
      else
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j= 1-nbdy,jj+nbdy
          do i= 1-nbdy,ii+nbdy
            taux(i,j,lslot) = 0.0
            tauy(i,j,lslot) = 0.0
          enddo
        enddo
      endif
      if     (thermo) then
        if     (ustflg.eq.3) then
          call rdmonthck(ustara(1-nbdy,1-nbdy,lslot),900,mnth)
        endif
        if (wndflg.lt.3) then
          call rdmonthck(wndspd(1-nbdy,1-nbdy,lslot),903,mnth)
        elseif (wndflg.ge.4) then
c ---     taux,tauy contains wndx,wndy
!$OMP     PARALLEL DO PRIVATE(j,i)
!$OMP&             SCHEDULE(STATIC,jblk)
          do j= 1-nbdy,jj+nbdy
            do i= 1-nbdy,ii+nbdy
              wndspd(i,j,lslot) = sqrt( taux(i,j,lslot)**2 +
     &                                  tauy(i,j,lslot)**2  )
            enddo
          enddo
        else
          call str2spd(wndspd(1-nbdy,1-nbdy,lslot),
     &                   taux(1-nbdy,1-nbdy,lslot),
     &                   tauy(1-nbdy,1-nbdy,lslot) )
        endif !wndspd
        if     (flxflg.ne.3) then
          call rdmonthck(airtmp(1-nbdy,1-nbdy,lslot),904,mnth)
          call rdmonthck(vapmix(1-nbdy,1-nbdy,lslot),905,mnth)
        endif
        if     (pcipf) then
          call rdmonthck(precip(1-nbdy,1-nbdy,lslot),906,mnth)
        endif
        call rdmonthck(radflx(1-nbdy,1-nbdy,lslot),907,mnth)
        call rdmonthck( swflx(1-nbdy,1-nbdy,lslot),908,mnth)
        if     (albflg.ne.0) then  !swflx is Qswdn
c ---     convert swflx to net shortwave into the ocean
c ---     shortwave through sea ice is handled separately
          if     (albflg.eq.1) then
            do j= 1-nbdy,jj+nbdy
              do i= 1-nbdy,ii+nbdy
                swflx(i,j,lslot) = swflx(i,j,lslot)*(1.0-0.09)  !NAVGEM albedo
              enddo
            enddo
          else   !albflg.eq.2
            degtorad = 4.d0*atan(1.d0)/180.d0
            do j= 1-nbdy,jj+nbdy
              do i= 1-nbdy,ii+nbdy
c ---           latitudinally-varying ocean albedo (Large and Yeager, 2009)
c ---           5.8% at the equator and 8% at the poles
                albw = ( 0.069 - 0.011*cos(2.0*degtorad*plat(i,j) ) )
                swflx(i,j,lslot) = swflx(i,j,lslot)*(1.0-albw)
              enddo
            enddo
          endif !albflg
        endif !Qswdn
      else
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j= 1-nbdy,jj+nbdy
          do i= 1-nbdy,ii+nbdy
            wndspd(i,j,lslot) = 0.0
            airtmp(i,j,lslot) = 0.0
            vapmix(i,j,lslot) = 0.0
            precip(i,j,lslot) = 0.0
            radflx(i,j,lslot) = 0.0
             swflx(i,j,lslot) = 0.0
          enddo
        enddo
      endif
c
      if     (lwflag.eq.2 .or. sstflg.eq.2   .or.
     &        icmflg.eq.2 .or. ticegr.eq.0.0     ) then
        call rdmonthck(surtmp(1-nbdy,1-nbdy,lslot),909,mnth)
        if     (sstflg.ne.3) then
          do j= 1-nbdy,jj+nbdy
            do i= 1-nbdy,ii+nbdy
              seatmp(i,j,lslot) = max( sstmin,
     &                                 min(sstmax, surtmp(i,j,lslot) ) )
            enddo
          enddo
        endif
      endif !surtmp
c
      if     (sstflg.eq.3) then
        call rdmonthck(seatmp(1-nbdy,1-nbdy,lslot),910,mnth)
      endif
c
      mreca = mnth
c
      if     (mnproc.eq.1) then
      write (lp,'(2(a,i3))') ' forcing functions for month',mnth,
     &   ' written into slot',lslot
      endif !1st tile
      call xcsync(flush_lp)
      return
      end
c
c
      subroutine rdkpar(mnth,lslot)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      integer lslot,mnth
c
c --- read kpar or chl forcing for one month.
c
      integer         mreca,mrecc,mreck,mrecr
      common/rdforfi/ mreca,mrecc,mreck,mrecr
      save  /rdforfi/
c
      integer i,irec,iunit,j
c
      if     (kparan) then
        return   ! annual (constant) kpar forcing
      endif
c
      if     (mnth.le.mreck) then
c
c ---   rewind
c
        iunit=919
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          rewind uoff+iunit
          read  (uoff+iunit,*)
          read  (uoff+iunit,*)
          read  (uoff+iunit,*)
          read  (uoff+iunit,*)
          read  (uoff+iunit,*)
        endif
        call zaiorw(iunit)
*       if     (mnproc.eq.1) then
*       write(lp,*) 'rdkpar: mreck,mnth = ',mreck,mnth,'  (rewind)'
*       endif !1st tile
*       call xcsync(flush_lp)
        mreck = 0
      endif
c
c --- skip forward to desired month
c
      do irec= mreck+1,mnth-1
*       if     (mnproc.eq.1) then
*       write(lp,*) 'rdkpar: mreck,mnth = ',mreck,mnth,
*    &              '  (skipping ',irec,')'
*       endif !1st tile
*       call xcsync(flush_lp)
        call skmonth(919)
      enddo
c
c --- read desired month
c
      call rdmonthck(akpar(1-nbdy,1-nbdy,lslot),919,mnth)
c
      mreck = mnth
c
      if     (mnproc.eq.1) then
      write (lp,'(2(a,i3))') ' kpar    forcing   for month',mnth,
     &   ' written into slot',lslot
      endif !1st tile
      call xcsync(flush_lp)
      return
      end
c
c
      subroutine rdrivr(mnth,lslot)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      integer lslot,mnth
c
c --- read river forcing for one month.
c
      integer         mreca,mrecc,mreck,mrecr
      common/rdforfi/ mreca,mrecc,mreck,mrecr
      save  /rdforfi/
c
      integer i,irec,iunit,j
c
      if     (rivera) then
        return   ! annual (constant) river forcing
      endif
c
      if     (mnth.le.mrecr) then
c
c ---   rewind
c
        iunit=918
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          rewind uoff+iunit
          read  (uoff+iunit,*)
          read  (uoff+iunit,*)
          read  (uoff+iunit,*)
          read  (uoff+iunit,*)
          read  (uoff+iunit,*)
        endif
        call zaiorw(iunit)
*       if     (mnproc.eq.1) then
*       write(lp,*) 'rdrivr: mreca,mnth = ',mreca,mnth,'  (rewind)'
*       endif !1st tile
*       call xcsync(flush_lp)
        mrecr = 0
      endif
c
c --- skip forward to desired month
c
      do irec= mrecr+1,mnth-1
*       if     (mnproc.eq.1) then
*       write(lp,*) 'rdforf: mrecr,mnth = ',mrecr,mnth,
*    &              '  (skipping ',irec,')'
*       endif !1st tile
*       call xcsync(flush_lp)
        call skmonth(918)
      enddo
c
c --- read desired month
c
      call rdmonthck(rivers(1-nbdy,1-nbdy,lslot),918,mnth)
c
      mrecr = mnth
c
      if     (mnproc.eq.1) then
      write (lp,'(2(a,i3))') ' rivers  forcing   for month',mnth,
     &   ' written into slot',lslot
      endif !1st tile
      call xcsync(flush_lp)
      return
      end
c
c
      subroutine rdrlax(month,lslot)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      integer lslot,month
c
c --- read relaxation fields for one month,
c --- monthly (clmflg==12) or bi-monthly (clmflg==6) data.
c
      integer         mreca,mrecc,mreck,mrecr
      common/rdforfi/ mreca,mrecc,mreck,mrecr
      save  /rdforfi/
c
      logical lexist,lfatal
      integer i,irec,iunit,j,k,ktr,mrec,mnth,mxunit
      real    p23min(2),shallow
c
      mnth=mod(month-1,12)+1
      if     (mnproc.eq.1) then
      write(lp,*) 'rdrlax - month = ',month,mnth
      endif !1st tile
      call xcsync(flush_lp)
c
      if     (relaxf) then
        if     (clmflg.eq.12) then
          mrec = mnth
        else
          mrec = (mnth+1)/2
        endif
        if     (relaxt) then
          mxunit = 914  ! tracers
        elseif (relaxs) then
          mxunit = 912  ! T&S only
        else
          mxunit = 913
        endif
c
        if     (mrec.le.mrecc) then
c
c ---     rewind all units
c
          do iunit= 911,mxunit
            if     (mnproc.eq.1) then  ! .b file from 1st tile only
              rewind uoff+iunit
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
              read  (uoff+iunit,*)
            endif
            call zaiorw(iunit)
          enddo
*         if     (mnproc.eq.1) then
*         write(lp,*) 'rdrlax: mrecc,mrec = ',mrecc,mrec,'  (rewind)'
*         endif !1st tile
*         call xcsync(flush_lp)
          mrecc = 0
        endif
c
c ---   skip forward to desired month
c
        do irec= mrecc+1,mrec-1
*         if     (mnproc.eq.1) then
*         write(lp,*) 'rdrlax: mrecc,mrec = ',mrecc,mrec,
*    &                '  (skipping ',irec,')'
*         endif !1st tile
*         call xcsync(flush_lp)
          do iunit= 911,mxunit
            if     (iunit.lt.914) then
              do k= 1,kk
                call skmonth(iunit)
              enddo
            else
              do ktr= 1,ntracr
                do k= 1,kk
                  call skmonth(iunit)
                enddo
              enddo
            endif
          enddo
        enddo
c
c --- read desired month
c
        if     (relaxs) then  ! surface only
          k=1
            call rdmonthck(twall(1-nbdy,1-nbdy,k,lslot),911,mnth)
          do k= 2,kk
            call skmonth(                               911)
          enddo
          k=1
            call rdmonthck(swall(1-nbdy,1-nbdy,k,lslot),912,mnth)
          do k= 2,kk
            call skmonth(                               912)
          enddo
          if (relaxt) then  !need pwall for tracers
            do k= 1,kk
              call rdmonthck(pwall(1-nbdy,1-nbdy,k,lslot),913,mnth)
            enddo
          endif
        else
          do k= 1,kk
            call rdmonthck(twall(1-nbdy,1-nbdy,k,lslot),911,mnth)
          enddo
          do k= 1,kk
            call rdmonthck(swall(1-nbdy,1-nbdy,k,lslot),912,mnth)
          enddo
          do k= 1,kk
            call rdmonthck(pwall(1-nbdy,1-nbdy,k,lslot),913,mnth)
          enddo
        endif
c
        if (relaxt) then
          do ktr= 1,ntracr
            do k= 1,kk
              call rdmonthck(trwall(1-nbdy,1-nbdy,k,lslot,ktr),914,mnth)
            enddo
          enddo
        endif
c
        mrecc = mrec
c
c ---   sanity check.
c
        if     (lslot.eq.1 .and. (relaxt .or. .not.relaxs)) then
          if     (isopyc) then
            shallow = dp0k(1)*5.0*qonem
          else
            shallow = sum(dp0k(1:min(max(5,nsigma+2),kk)))*qonem
          endif
          p23min(1) = huge(p23min(1))
          p23min(2) = huge(p23min(2))
          do j= 1,jj
            do i= 1,ii
              if     (depths(i,j).gt.shallow) then
                p23min(1) = min( p23min(1), 
     &                           pwall(i,j,min(2,kkwall),lslot) )
                p23min(2) = min( p23min(2), 
     &                           pwall(i,j,min(3,kkwall),lslot) )
              endif
            enddo
          enddo
          call xcminr(p23min)
          if     (p23min(1).eq.huge(p23min(1))) then
            if     (mnproc.eq.1) then
            write (lp,'(2a,f7.1)')
     &        'rdrlax: could not check pwall.2, all depths below',
     &        shallow
            endif !1st tile
          else
            shallow = dp0k(1)+dp0k(min(2,kk))
            lfatal = .false.
            if    (abs(p23min(1)-dp0k(1)).le.dp0k(1)*0.01) then
              if     (mnproc.eq.1) then
              write (lp,'(a,2f7.2)')
     &          'rdrlax: pwall.2 ok; expected,input min depth =',
     &          dp0k(1)*qonem,p23min(1)*qonem
              endif !1st tile
            else
              lfatal = .true.
              if     (mnproc.eq.1) then
              write (lp,'(a,2f7.2,a)')
     &          'rdrlax: pwall.2 NOT ok; expected,input min depth =',
     &          dp0k(1)*qonem,p23min(1)*qonem,
     &          ' (bad climatology?)'
              endif !1st tile
            endif
            if     (abs(p23min(2)-shallow).le.shallow*0.01) then
              if     (mnproc.eq.1) then
              write (lp,'(a,2f7.2)')
     &          'rdrlax: pwall.3 ok; expected,input min depth =',
     &          shallow*qonem,p23min(2)*qonem
              endif !1st tile
            elseif (.not.isopyc .and. kk.gt.2) then
              lfatal = .true.
              if     (mnproc.eq.1) then
              write (lp,'(a,2f7.2,a)')
     &          'rdrlax: pwall.3 NOT ok; expected,input min depth =',
     &          shallow*qonem,p23min(2)*qonem,
     &          ' (bad climatology?)'
              endif !1st tile
            endif
            if     (lfatal) then
              inquire(
     &          file=flnmforw(1:len_trim(flnmforw))//'relax.weird',
     &          exist=lexist)
              if     (lexist) then
                if     (mnproc.eq.1) then
                write (lp,'(3a)')
     &            'rdrlax: continuing because file ',
     &            flnmforw(1:len_trim(flnmforw))//'relax.weird',
     &            ' exists (ignore the "bad" climatology)'
                endif !1st tile
                call xcsync(flush_lp)
              else
                call xcsync(flush_lp)
                if     (mnproc.eq.1) then
                write (lp,'(3a)')
     &            'rdrlax: create an empty file ',
     &            flnmforw(1:len_trim(flnmforw))//'relax.weird',
     &            ' to ignore the "bad" climatology'
                endif !1st tile
                call xcstop('(rdrlax)')
                       stop '(rdrlax)'
              endif
            endif
          endif
        endif  ! sanity check
c
        if     (mnproc.eq.1) then
        write (lp,'(2(a,i3))') ' relaxation fields for month',mnth,
     &     ' written into slot',lslot
        endif !1st tile
        call xcsync(flush_lp)
      endif
      return
      end
c
c
      subroutine rdbaro(dtime)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      real*8    dtime
c
c --- baroclinic velocity nesting archive input processing
c
c --- filenames  nest/arch[vm].????_???_??.[ab]
c
c --- I/O and array I/O unit 921 is reserved for the entire run.
c
c --- all input fields must be defined at all grid points
c
      logical   larchm
      save      larchm
      integer   iarch
      save      iarch
      real*8    dbnstf,dbnsti,dtimei,dtime0,dtime1
      save      dbnstf,dbnsti,dtimei,dtime0,dtime1
c
      integer   i,j,k
c
c --- wb0 negative on first call only.
      if     (wb0.lt.-1.0) then
c
c ---   initialize nesting fields
c
        if     (mnproc.eq.1) then
        write (lp,*) ' now initializing baro nesting fields ...'
        endif !1st tile
        call xcsync(flush_lp)
c
        dbnstf = abs(bnstfq)
        if     (dbnstf.lt.1.0) then
          dbnstf = (baclin/86400.0d0)*
     &             max(1,nint((86400.0d0*dbnstf)/baclin))
        endif
        larchm = bnstfq.lt.0.0  !mean archives spaning -bnstfq days
        if     (larchm) then
          dbnsti = 0.5*dbnstf
        else
          dbnsti = 0.0
        endif
        dtimei = int((dtime-dbnsti)/dbnstf)*dbnstf + dbnsti
c
        dtime0 = dtimei
        lb0    = 1
        call rdbaro_in(dtime0,larchm,1)
c
        iarch  = 1
        dtime1 = dtimei + dbnstf
        lb1    = 2
        call rdbaro_in(dtime1,larchm,2)
c
        if     (mnproc.eq.1) then
        write (lp,*)
        write (lp,*) ' dtime,dtime0,dtime1 = ',dtime,dtime0,dtime1
        write (lp,*)
        write (lp,*) ' ...finished initializing baro nesting fields'
        endif !1st tile
        call xcsync(flush_lp)
      endif  ! initialization
c
      if     (dtime.gt.dtime1) then
c
c ---   get the next set of fields.
        do j= 1-nbdy,jj+nbdy
          do i= 1-nbdy,ii+nbdy
            ubnest(i,j,1) = ubnest(i,j,2)
            vbnest(i,j,1) = vbnest(i,j,2)
            ubpnst(i,j,1) = ubpnst(i,j,2)
            vbpnst(i,j,1) = vbpnst(i,j,2)
            pbnest(i,j,1) = pbnest(i,j,2)
          enddo
        enddo
        dtime0 = dtime1
        iarch  = iarch + 1
        dtime1 = dtimei + dbnstf*iarch
        call rdbaro_in(dtime1,larchm,2)
c
*           if     (mnproc.eq.1) then
*           write(lp,*) ' exit rdbaro_in - ',dtime,dtime0,dtime1
*           endif !1st tile
*           call xcsync(flush_lp)
      endif  ! next set of fields.
c
c --- linear interpolation in time.
      wb0 = (dtime1-dtime)/(dtime1-dtime0)
      wb1 = 1.0 - wb0
*           if     (mnproc.eq.1) then
*           write(lp,*) 'rdbaro - dtime,wb0,wb1 = ',dtime,wb0,wb1
*           endif !1st tile
*           call xcsync(flush_lp)
      return
      end
c
c
      subroutine rdbaro_in(dtime,larchm,lslot)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      real*8    dtime
      integer   lslot
      logical   larchm
c
c --- input barotropic fields from archive on model day dtime.
c --- filenames  nest/arch[vm].????_???_??.[ab]
c --- I/O and array I/O unit 921 is reserved for the entire run.
c
      character flnm*22, cline*80, cvarin*6, cfield*8
      integer   i,idmtst,ios,j,jdmtst,k,layer
      integer   iyear,iday,ihour
      logical   nodens
      real      hqpbot
c
      call forday(dtime, yrflag, iyear,iday,ihour)
c
      if     (larchm) then
        write(flnm,'("nest/archm.",i4.4,"_",i3.3,"_",i2.2)')
     &                             iyear,iday,ihour
      else
        write(flnm,'("nest/archv.",i4.4,"_",i3.3,"_",i2.2)')
     &                             iyear,iday,ihour
      endif
c
      if     (mnproc.eq.1) then
      write (lp,*) 'rdbaro_in: ',flnm
      endif !1st tile
      call xcsync(flush_lp)
*
      call zaiopf(flnm//'.a','old', 921)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+921,file=flnm//'.b',form='formatted',
     &        status='old',action='read')
c
        read(uoff+921,'(a)') cline
        read(uoff+921,'(a)') cline
        read(uoff+921,'(a)') cline
        read(uoff+921,'(a)') cline
c
        read(uoff+921,'(a)') cline
        read(uoff+921,'(a)') cline
        read(uoff+921,'(a)') cline
      endif !1st tile
c
      call zagetc(cline,ios, uoff+921)
      read(cline,*) idmtst,cvarin
*     if     (mnproc.eq.1) then
*     write(lp,*) cvarin,' = ',idmtst
*     endif !1st tile
      if (cvarin.ne.'idm   ') then
        if     (mnproc.eq.1) then
        write(lp,*)
        write(lp,*) 'error in rdbaro_in - input ',cvarin,
     &                        ' but should be idm   '
        write(lp,*)
        endif !1st tile
        call xcstop('(rdbaro_in)')
               stop '(rdbaro_in)'
      endif
      call zagetc(cline,ios, uoff+921)
      read(cline,*) jdmtst,cvarin
*     if     (mnproc.eq.1) then
*     write(lp,*) cvarin,' = ',jdmtst
*     endif !1st tile
      if (cvarin.ne.'jdm   ') then
        if     (mnproc.eq.1) then
        write(lp,*)
        write(lp,*) 'error in rdbaro_in - input ',cvarin,
     &                        ' but should be jdm   '
        write(lp,*)
        endif !1st tile
        call xcstop('(rdbaro_in)')
               stop '(rdbaro_in)'
      endif
c
      if (idmtst.ne.itdm .or. jdmtst.ne.jtdm) then
        if     (mnproc.eq.1) then
        write(lp,*)
        write(lp,*) 'error in rdbaro_in - input idm,jdm',
     &                        ' not consistent with parameters'
        write(lp,*) 'idm,jdm = ',itdm,  jtdm,  '  (dimensions.h)'
        write(lp,*) 'idm,jdm = ',idmtst,jdmtst,'  (input)'
        write(lp,*)
        endif !1st tile
        call xcstop('(rdbaro_in)')
               stop '(rdbaro_in)'
      endif
c
      call zagetc(cline,ios, uoff+921)
c
c --- skip some surface fields.
c
      call rd_archive(util1, cfield,layer, 921)  ! montg1
      if     (cfield.ne.'montg1  ') then
        if     (mnproc.eq.1) then
        write(lp,'(/ a / a,a /)') cfield,
     &         'error in rdbaro_in - expected ','montg1  '
        endif !1st tile
        call xcstop('(rdbaro_in)')
               stop '(rdbaro_in)'
      endif
      nodens = layer.ne.0  !new or original archive type
c
      call rd_archive(util2, cfield,layer, 921)  ! srfhgt=montg1+thref*pbnest
      if     (cfield.ne.'srfhgt  ') then
        if     (mnproc.eq.1) then
        write(lp,'(/ a / a,a /)') cfield,
     &         'error in rdbaro_in - expected ','srfhgt  '
        endif !1st tile
        call xcstop('(rdbaro_in)')
               stop '(rdbaro_in)'
      endif
      call zagetc(cline,ios, uoff+921)  !steric or surflx
      call zaiosk(921)
      if     (cline(1:8).eq.'steric  ') then  !surflx
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          read (uoff+921,*)
        endif
        call zaiosk(921)
      endif
      if     (nodens) then
        do i= 1,3 !salflx,dpbl,dpmixl
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
            read (uoff+921,*)
          endif
          call zaiosk(921)
        enddo
      else
        do i= 1,8 !salflx,dpbl,dpmixl,tmix,smix,thmix,umix,vmix
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
            read (uoff+921,*)
          endif
          call zaiosk(921)
        enddo
      endif !nodens:else
      call rd_archive(ubnest(1-nbdy,1-nbdy,lslot), cfield,layer, 921)
      if     (cfield.eq.'kemix   ') then
        call rd_archive(ubnest(1-nbdy,1-nbdy,lslot), cfield,layer, 921)
      endif
      if     (cfield.eq.'covice  ') then
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          read (uoff+921,*)
        endif
        call zaiosk(921)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          read (uoff+921,*)
        endif
        call zaiosk(921)
        call rd_archive(ubnest(1-nbdy,1-nbdy,lslot), cfield,layer, 921)
      endif
      if     (cfield.ne.'u_btrop ') then
        if     (mnproc.eq.1) then
        write(lp,'(/ a / a,a /)') cfield,
     &         'error in rdbaro_in - expected ','u_btrop '
        endif !1st tile
        call xcstop('(rdbaro_in)')
               stop '(rdbaro_in)'
      endif
      call rd_archive(vbnest(1-nbdy,1-nbdy,lslot), cfield,layer, 921)
      if     (cfield.ne.'v_btrop ') then
        if     (mnproc.eq.1) then
        write(lp,'(/ a / a,a /)') cfield,
     &         'error in rdbaro_in - expected ','v_btrop '
        endif !1st tile
        call xcstop('(rdbaro_in)')
               stop '(rdbaro_in)'
      endif
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      close( unit=uoff+921)
      endif
      call zaiocl(921)
c
      call xctilr(ubnest(1-nbdy,1-nbdy,lslot),1,1, 1,1, halo_uv)
      call xctilr(vbnest(1-nbdy,1-nbdy,lslot),1,1, 1,1, halo_vv)
c
!$OMP PARALLEL DO PRIVATE(j,i,hqpbot)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        do i=1,ii
          if     (ip(i,j).eq.1) then
            hqpbot = 0.5/pbot(i,j)
            pbnest(i,j,lslot) = (util2(i,j)-util1(i,j))*qthref
            ubpnst(i,j,lslot) = (ubnest(i,  j,lslot)*depthu(i,  j)+
     &                           ubnest(i+1,j,lslot)*depthu(i+1,j) )
     &                          *hqpbot
            vbpnst(i,j,lslot) = (vbnest(i,j,  lslot)*depthv(i,j  )+
     &                           vbnest(i,j+1,lslot)*depthv(i,j+1) )
     &                          *hqpbot
          else
            pbnest(i,j,lslot) = 0.0
            ubpnst(i,j,lslot) = 0.0
            vbpnst(i,j,lslot) = 0.0
          endif
        enddo
      enddo
c
      if     (.false. .and. ittest.ne.-1 .and. jttest.ne.-1) then
        call xcsync(flush_lp)
        if     (i0.lt.ittest .and. i0+ii.ge.ittest .and.
     &          j0.lt.jttest .and. j0+jj.ge.jttest      ) then
          write(lp,'(i5,i4,a,1p5e13.5)')
     &       itest+i0,jtest+j0,' rdbaro: ub,vb,pb,ubp,vbp = ',
     &       ubnest(itest,jtest,lslot),
     &       vbnest(itest,jtest,lslot),
     &       pbnest(itest,jtest,lslot),
     &       ubpnst(itest,jtest,lslot),
     &       vbpnst(itest,jtest,lslot)
          write(lp,'(i5,i4,a,1p5e13.5)')
     &       itest+i0,jtest+1+j0,' rdbaro: ub,vb,pb,ubp,vbp = ',
     &       ubnest(itest,jtest+1,lslot),
     &       vbnest(itest,jtest+1,lslot),
     &       pbnest(itest,jtest+1,lslot),
     &       ubpnst(itest,jtest+1,lslot),
     &       vbpnst(itest,jtest+1,lslot)
        endif
        call xcsync(flush_lp)
      endif
      return
      end
c
c
      subroutine rdnest(dtime)
      use mod_xc           ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za           ! HYCOM I/O interface
      implicit none
c
      real*8    dtime
c
c --- 3-d nesting archive input processing
c
c --- filenames  ./nest/arch[vm].????_???_??.[ab]
c ---            ./nest/rmu.[ab]
c
c --- I/O and array I/O unit 915 is used for rmun[pv] only (not reserved).
c --- I/O and array I/O unit 920 is reserved for the entire run.
c
c --- all input fields must be defined at all grid points
c
      logical   larchm
      save      larchm
      integer   iarch
      save      iarch
      real*8    dnestf,dnesti,dtimei,dtime0,dtime1
      save      dnestf,dnesti,dtimei,dtime0,dtime1
c
      integer   i,ios,j,k
      character preambl(5)*79,cline*80
c
c --- wn0 negative on first call only.
      if     (wn0.lt.-1.0) then
c
c ---   initialize nesting fields
c
        if     (mnproc.eq.1) then
        write (lp,*) ' now initializing 3-d nesting fields ...'
        endif !1st tile
        call xcsync(flush_lp)
c
        call zaiopf('nest/rmu.a', 'old', 915)
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+915,file='nest/rmu.b',
     &        status='old',action='read')
        endif !1st tile
        call zagetc(cline,ios, uoff+915)  !1st line of the header on all tiles
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        rewind uoff+915
        read  (uoff+915,'(a79)') preambl
        endif !1st tile
        call preambl_print(preambl)
        if     (cline.eq.'Relaxation Masks') then  !two masks
          call rdmonth(rmunp, 915)
          call rdmonth(rmunv, 915)
          call xctilr(rmunp,1,1, nbdy,nbdy, halo_ps)
          call xctilr(rmunv,1,1, nbdy,nbdy, halo_ps)
        else !one mask
          call rdmonth(rmunp, 915)
          call xctilr(rmunp,1,1, nbdy,nbdy, halo_ps)
          rmunv(:,:) = rmunp(:,:)
        endif !1 or 2 masks
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
        close (unit=uoff+915)
        endif !1st tile
        call zaiocl(915)
c
        dnestf = abs(nestfq)
        if     (dnestf.lt.1.0) then
          dnestf = (baclin/86400.0d0)*
     &             max(1,nint((86400.0d0*dnestf)/baclin))
        endif
        larchm = nestfq.lt.0.0  !mean archives spaning -nestfq days
        if     (larchm) then
          dnesti = 0.5*dnestf
        else
          dnesti = 0.0
        endif
        dtimei = int((dtime-dnesti)/dnestf)*dnestf + dnesti
c
        dtime0 = dtimei
        ln0    = 1
        call rdnest_in(dtime0,larchm,1)
c
        iarch  = 1
        dtime1 = dtimei + dnestf
        ln1    = 2
        call rdnest_in(dtime1,larchm,2)
c
        if     (mnproc.eq.1) then
        write (lp,*)
        write (lp,*) ' dtime,dtime0,dtime1 = ',dtime,dtime0,dtime1
        write (lp,*)
        write (lp,*) ' ...finished initializing 3-d nesting fields'
        endif !1st tile
        call xcsync(flush_lp)
      endif  ! initialization
c
      if     (dtime.gt.dtime1) then
c
c ---   get the next set of fields.
        do k= 1,kk
          do j= 1,jj
            do i= 1,ii
              tnest(i,j,k,1) = tnest(i,j,k,2)
              snest(i,j,k,1) = snest(i,j,k,2)
              pnest(i,j,k,1) = pnest(i,j,k,2)
              unest(i,j,k,1) = unest(i,j,k,2)
              vnest(i,j,k,1) = vnest(i,j,k,2)
            enddo
          enddo
        enddo
        dtime0 = dtime1
        iarch  = iarch + 1
        dtime1 = dtimei + dnestf*iarch
        call rdnest_in(dtime1,larchm,2)
c
*           if     (mnproc.eq.1) then
*           write(lp,*) ' exit rdnest_in - ',dtime,dtime0,dtime1
*           endif !1st tile
*           call xcsync(flush_lp)
      endif  ! next set of fields.
c
c --- linear interpolation in time.
      wn0 = (dtime1-dtime)/(dtime1-dtime0)
      wn1 = 1.0 - wn0
*           if     (mnproc.eq.1) then
*           write(lp,*) 'rdnest - dtime,wn0,wn1 = ',dtime,wn0,wn1
*           endif !1st tile
*           call xcsync(flush_lp)
      return
      end
c
c
      subroutine rdnest_in(dtime,larchm,lslot)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      real*8    dtime
      integer   lslot
      logical   larchm
c
c --- input 3-d nesting fields from archive on model day dtime.
c --- filenames  nest/arch[vm].????_???_??.[ab]
c --- I/O and array I/O unit 920 is reserved for the entire run.
c
      logical    ldebug_rdnest
      parameter (ldebug_rdnest=.false.)
c
      character flnm*22, cline*80, cvarin*6, cfield*8
      integer   i,idmtst,ios,j,jdmtst,k,layer
      integer   iyear,iday,ihour
      logical   meanar,nodens
c
      call forday(dtime, yrflag, iyear,iday,ihour)
c
      if     (larchm) then
        write(flnm,'("nest/archm.",i4.4,"_",i3.3,"_",i2.2)')
     &                             iyear,iday,ihour
      else
        write(flnm,'("nest/archv.",i4.4,"_",i3.3,"_",i2.2)')
     &                             iyear,iday,ihour
      endif
c
      if     (mnproc.eq.1) then
      write (lp,*) 'rdnest_in: ',flnm
      endif !1st tile
      call xcsync(flush_lp)
*
      call zaiopf(flnm//'.a','old', 920)
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
        open (unit=uoff+920,file=flnm//'.b',form='formatted',
     &        status='old',action='read')
c
        read(uoff+920,'(a)') cline
        read(uoff+920,'(a)') cline
        read(uoff+920,'(a)') cline
        read(uoff+920,'(a)') cline
c
        read(uoff+920,'(a)') cline
        read(uoff+920,'(a)') cline
        read(uoff+920,'(a)') cline
      endif !1st tile
c
      call zagetc(cline,ios, uoff+920)
      read(cline,*) idmtst,cvarin
*     if     (mnproc.eq.1) then
*     write(lp,*) cvarin,' = ',idmtst
*     endif !1st tile
      if (cvarin.ne.'idm   ') then
        if     (mnproc.eq.1) then
        write(lp,*)
        write(lp,*) 'error in rdnest_in - input ',cvarin,
     &                        ' but should be idm   '
        write(lp,*)
        endif !1st tile
        call xcstop('(rdnest_in)')
               stop '(rdnest_in)'
      endif
      call zagetc(cline,ios, uoff+920)
      read(cline,*) jdmtst,cvarin
*     if     (mnproc.eq.1) then
*     write(lp,*) cvarin,' = ',jdmtst
*     endif !1st tile
      if (cvarin.ne.'jdm   ') then
        if     (mnproc.eq.1) then
        write(lp,*)
        write(lp,*) 'error in rdnest_in - input ',cvarin,
     &                        ' but should be jdm   '
        write(lp,*)
        endif !1st tile
        call xcstop('(rdnest_in)')
               stop '(rdnest_in)'
      endif
c
      if (idmtst.ne.itdm .or. jdmtst.ne.jtdm) then
        if     (mnproc.eq.1) then
        write(lp,*)
        write(lp,*) 'error in rdnest_in - input idm,jdm',
     &                        ' not consistent with parameters'
        write(lp,*) 'idm,jdm = ',itdm,  jtdm,  '  (dimensions.h)'
        write(lp,*) 'idm,jdm = ',idmtst,jdmtst,'  (input)'
        write(lp,*)
        endif !1st tile
        call xcstop('(rdnest_in)')
               stop '(rdnest_in)'
      endif
c
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
        read (uoff+920,*)
      endif
c
c --- skip surface fields.
c
      call rd_archive(util1, cfield,layer, 920)  ! montg1 (discarded)
      if     (cfield.ne.'montg1  ') then
        if     (mnproc.eq.1) then
        write(lp,'(/ a / a,a /)') cfield,
     &         'error in rdnest_in - expected ','montg1  '
        endif !1st tile
        call xcstop('(rdbaro_in)')
               stop '(rdbaro_in)'
      endif
      nodens = layer.ne.0  !new or original archive type
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
        read (uoff+920,*)
      endif
      call zaiosk(920)                  !srfhgt
      call zagetc(cline,ios, uoff+920)  !steric or surflx
      call zaiosk(920)
      if     (cline(1:8).eq.'steric  ') then  !surflx
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          read (uoff+920,*)
        endif
        call zaiosk(920)
      endif
      if     (nodens) then
        do i= 1,3 !salflx,dpbl,dpmixl
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
            read (uoff+920,*)
          endif
          call zaiosk(920)
        enddo
      else
        do i= 1,8 !salflx,dpbl,dpmixl,tmix,smix,thmix,umix,vmix
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
            read (uoff+920,*)
          endif
          call zaiosk(920)
        enddo
      endif !nodens:else
      call zagetc(cline,ios, uoff+920)  !kemix or covice or u_btrop
      meanar = cline(1:8).eq.'kemix   '
      if     (meanar) then
        call zaiosk(920)  !skip kemix
        call rd_archive(util1, cfield,layer, 920) !covice or u_btrop
        if     (cfield.eq.'covice  ') then
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
            read (uoff+920,*)
          endif
          call zaiosk(920) !skip thkice
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
            read (uoff+920,*)
          endif
          call zaiosk(920) !skip temice
          call rd_archive(util1, cfield,layer, 920) !u_btrop
        endif
        call rd_archive(util2, cfield,layer, 920) !v_btrop
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          read (uoff+920,*)
        endif
        call zaiosk(920)  !skip kebtrop
      else !standard archive file
        if     (cline(1:8).eq.'covice  ') then
          call zaiosk(920) !skip covice
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
            read (uoff+920,*)
          endif
          call zaiosk(920) !skip thkice
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
            read (uoff+920,*)
          endif
          call zaiosk(920) !skip temice
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
            read (uoff+920,*)
          endif
        endif
        call zaiosk(920)  !skip u_btrop
        if     (mnproc.eq.1) then  ! .b file from 1st tile only
          read (uoff+920,*)
        endif
        call zaiosk(920)  !skip v_btrop
      endif !meanar:else
c
c --- 3-d fields.
c
      do k=1,kk
        call rd_archive(unest(1-nbdy,1-nbdy,k,lslot), cfield,layer, 920)
        if     (cfield.ne.'u-vel.  ') then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,a /)') cfield,
     &           'error in rdnest_in - expected ','u-vel.  '
          endif !1st tile
          call xcstop('(rdnest_in)')
                 stop '(rdnest_in)'
        endif
        call rd_archive(vnest(1-nbdy,1-nbdy,k,lslot), cfield,layer, 920)
        if     (cfield.ne.'v-vel.  ') then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,a /)') cfield,
     &           'error in rdnest_in - expected ','v-vel.  '
          endif !1st tile
          call xcstop('(rdnest_in)')
                 stop '(rdnest_in)'
        endif
        if     (meanar) then
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
            read (uoff+920,*)
          endif
          call zaiosk(920)  !skip k.e.
        endif
        if     (k.ne.kk) then
          call rd_archive(pnest(1-nbdy,1-nbdy,k+1,lslot),
     &                    cfield,layer, 920)
          if     (cfield.ne.'thknss  ') then
            if     (mnproc.eq.1) then
            write(lp,'(/ a / a,a /)') cfield,
     &             'error in rdnest_in - expected ','thknss  '
            endif !1st tile
            call xcstop('(rdnest_in)')
                   stop '(rdnest_in)'
          endif
        else
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
            read (uoff+920,*)
          endif
          call zaiosk(920)
        endif
        call rd_archive(tnest(1-nbdy,1-nbdy,k,lslot), cfield,layer, 920)
        if     (cfield.ne.'temp    ') then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,a /)') cfield,
     &           'error in rdnest_in - expected ','temp    '
          endif !1st tile
          call xcstop('(rdnest_in)')
                 stop '(rdnest_in)'
        endif
        call rd_archive(snest(1-nbdy,1-nbdy,k,lslot), cfield,layer, 920)
        if     (cfield.ne.'salin   ') then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,a /)') cfield,
     &           'error in rdnest_in - expected ','salin   '
          endif !1st tile
          call xcstop('(rdnest_in)')
                 stop '(rdnest_in)'
        endif
        if     (.not. nodens) then
          if     (mnproc.eq.1) then  ! .b file from 1st tile only
            read (uoff+920,*)
          endif
          call zaiosk(920)  !skip density
        endif !.not.nodens
      enddo
c
      if     (mnproc.eq.1) then  ! .b file from 1st tile only
      close( unit=uoff+920)
      endif
      call zaiocl(920)

      if     (meanar) then
        call xctilr(pnest(1-nbdy,1-nbdy,1,lslot),1,kk, 1,1, halo_ps)
      endif
c
!$OMP PARALLEL DO PRIVATE(j,i,k)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        if     (meanar) then  !mean archive
c         for thin layers, take baroclinic velocity from above
c         otherwise, convert from total to baroclinic velocity
          do k= 1,kk
            do i=1,ii
              if     (iu(i,j).eq.1) then
                if     (min(pnest(i,  j,k,lslot),
     &                      pnest(i-1,j,k,lslot) ).lt.tencm) then
                  unest(i,j,k,lslot) = unest(i,j,max(1,k-1),lslot)
                else
                  unest(i,j,k,lslot) = unest(i,j,k,lslot) - util1(i,j)
                endif !thin layer:else
              endif !iu
              if     (iv(i,j).eq.1) then
                if     (min(pnest(i,j,  k,lslot),
     &                      pnest(i,j-1,k,lslot) ).lt.tencm) then
                  vnest(i,j,k,lslot) = vnest(i,j,max(1,k-1),lslot)
                else
                  vnest(i,j,k,lslot) = vnest(i,j,k,lslot) - util2(i,j)
                endif !thin layer:else
              endif !iv
            enddo !i
          enddo !k
        endif !meanar
c       convert from layer thickness to interface depth (pressure)
        do i=1,ii
          pnest(i,j,1,lslot) = 0.0
          do k= 3,kk
            pnest(i,j,k,lslot) = pnest(i,j,k,  lslot) +
     &                           pnest(i,j,k-1,lslot)
          enddo !k
        enddo !i
      enddo !j
c
      if     (ldebug_rdnest .and. ittest.ne.-1 .and. jttest.ne.-1) then
        call xcsync(flush_lp)
        if     (i0.lt.ittest .and. i0+ii.ge.ittest .and.
     &          j0.lt.jttest .and. j0+jj.ge.jttest      ) then
 103      format(i8,i5,i4,1x,a,a/
     &           (i8,5x,i4,1x,a,a,2f7.3,2f7.3,f8.4,f9.3,f9.2))
          write(lp,103)
     &       nstep,itest+i0,jtest+j0,'rdnest',
     &       ':   utot   vtot   temp   saln    dens    thkns     dpth',
     &      (nstep,k,                'rdnest',':',
     &       unest(itest,jtest,k,lslot)+ubnest(itest,jtest,lslot),
     &       vnest(itest,jtest,k,lslot)+vbnest(itest,jtest,lslot),
     &       tnest(itest,jtest,k,lslot),
     &       snest(itest,jtest,k,lslot),
     &       0.0,
     &       (pnest(itest,jtest,k+1,lslot)-
     &        pnest(itest,jtest,k,  lslot) )*qonem,
     &       pnest(itest,jtest,k+1,lslot)*qonem,
     &       k=1,kk-1),
     &      (nstep,k,                'rdnest',':',
     &       unest(itest,jtest,k,lslot)+ubnest(itest,jtest,lslot),
     &       vnest(itest,jtest,k,lslot)+vbnest(itest,jtest,lslot),
     &       tnest(itest,jtest,k,lslot),
     &       snest(itest,jtest,k,lslot),
     &       0.0,
     &       depths(i,j)-pnest(itest,jtest,k,lslot)*qonem,
     &       depths(i,j),
     &       k=kk,kk)
        endif
        call xcsync(flush_lp)
      endif
c
      return
      end
c
c
      subroutine rd_archive(field, cfield,layer, iunit)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      character cfield*8
      integer   layer,iunit
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
     &          field
c
c --- read a single archive array field from unit iunit.
c
      integer   i,ios,nnstep
      real      hmina,hminb,hmaxa,hmaxb,timein,thet
      character cline*80
c
      call zagetc(cline,ios, uoff+iunit)
      if     (ios.ne.0) then
        if     (mnproc.eq.1) then
          write(lp,*)
          write(lp,*) 'error in rd_archive - hit end of input'
          write(lp,*) 'iunit,ios = ',iunit,ios
          write(lp,*)
        endif !1st tile
        call xcstop('(rd_archive)')
               stop '(rd_archive)'
      endif
*     if     (mnproc.eq.1) then
*     write(lp,'(a)') cline
*     endif !1st tile
c
      cfield = cline(1:8)
c
      i = index(cline,'=')
      read(cline(i+1:),*) nnstep,timein,layer,thet,hminb,hmaxb
c
      if     (hminb.eq.hmaxb) then  !constant field
        field(:,:) = hminb
        call zaiosk(iunit)
      else
        call zaiord(field,ip,.false., hmina,hmaxa,
     &              iunit)
c
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          endif !1st tile
cnostop   call xcstop('(rd_archive)')
cnostop          stop '(rd_archive)'
        endif
      endif
      return
      end
c
c
      subroutine str2spd(wspd, tx,ty)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_za         ! HYCOM I/O interface
      implicit none
c
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
     &        wspd, tx,ty
c
c --- calculate wind speed from wind stress
c
c --- speed-dependent scale factor from stress to speed is based
c --- on the Kara (neutral) wind-speed dependent drag coefficient 
c
      integer   i,j
      real      strspd,wndstr
c
!$OMP   PARALLEL DO PRIVATE(j,i)
!$OMP&           SCHEDULE(STATIC,jblk)
      do j= 1-nbdy,jj+nbdy
        do i= 1-nbdy,ii+nbdy
          wndstr = sqrt( tx(i,j)**2 + ty(i,j)**2 )
          if     (wndstr.LE.0.7711) THEN
            strspd = 1.0/(1.22*(((3.236E-3 *wndstr -
     +                            5.230E-3)*wndstr +
     +                            3.218E-3)*wndstr +
     +                            0.926E-3)       )
          else
            strspd = 1.0/(1.22*(((0.007E-3 *wndstr -
     +                            0.092E-3)*wndstr +
     +                            0.485E-3)*wndstr +
     +                            1.461E-3)       )
          endif
          wspd(I,J) = sqrt( strspd*wndstr )
        enddo
      enddo
      return
      end
c
c
c> Revision history:
c>
c> Mar. 1995 - added logical variable 'windf'
c> Oct. 1997 - made necessary changes to reduce time dimension from 12 to 4
c> Oct. 1999 - added code to read and store shortwave heat flux used for
c>             penetrating shortwave radiation
c> Jan. 2000 - removed all conversion factors (apply before input)
c> Jan. 2000 - removed biasrd and biaspc      (apply before input)
c> May. 2000 - conversion to SI units, positive flux into ocean
c> Aug. 2000 - added option for high frequency atmospheric forcing
c> Jan. 2001 - converted from pakk to array input file type
c> Jul. 2001 - added skmonth and support for relaxs (surface only relax)
c> Jul. 2001 - added rdopen and rdbaro
c> Aug. 2001 - added constant field logic (skip the array input)
c> Mar. 2003 - added surtmp and seatmp
c> Mar. 2005 - added wndflg==3 and str2spd
c> Aug. 2005 - added tracer climatology
c> Nov. 2006 - [uv]pnst now from interpolation of transports
c> Mar. 2010 - support for daily mean nesting archive files
c> Mar. 2010 - added diwbot
c> Apr. 2010 - added sssrmx
c> Apr  2010 - added diwqh0 and removed diwlat
c> Nov  2010 - added wndrep (yrflag==2) for one or two year forcing repeat
c> Apr  2011 - added cbarp
c> Jul  2011 - fixed a cbarp input bug when cbar<0
c> Jul  2011 - added forfuns for salfac
c> Sep  2011 - added cbp
c> Nov. 2012 - added wndflg=4 for reading 10m wind components
c> Nov. 2012 - added iftaux,oftauy, primarily for wndflg=4
c> Jan. 2013 - replaced dragrh with drgten
c> July 2013 - added diws and diwm
c> Oct. 2013 - added jerlv0=-1 and forfunc
c> Nov. 2013 - added wndflg=5 (also) for reading 10m wind components
c> Nov. 2013 - added lwflag.eq.-1 for radflx=Qlwdn, swflx=Qswdn
c> Jan. 2014 - added mslprf and mslprs and forfunhp
c> Jan. 2014 - added natm logic
c> Jan. 2014 - modified natm and pwall logic to avoid gfortran warnings
c> May  2014 - added forfunhz
c> Oct  2014 - flxflg==6 requires mslprs input
c> Aug  2015 - flxflg==3 does not read airtmp and vapmix
