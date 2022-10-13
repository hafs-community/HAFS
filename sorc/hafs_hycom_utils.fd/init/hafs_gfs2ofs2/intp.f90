program intp
!$$$ MAIN PROGRAM DOCUMENTATION BLOCK
!
!   MAIN PROGRAM:  Intp.f90
!   PRGMMR: ILYA RIVIN       ORG: W/NP21      DATE: 2003-01-14    
!                                UPDATED April 03 C.L. August 03 C.L
!                                       September 03 B.B February 04 .c.l.. 
!                                       July 24 Avichal Mehra
!                                       June 1, 2006: Avichal Mehra & Ilya Rivin
! UPDATED : Biju Thomas on May 27, 2022
!          Different unit file numbers(LUGB) for different grib2 file(prevents error, code 97, in getgb2() routine)
!          Use getarg() function to read command line arguments instead redirected piping
! ABSTRACT: THIS PROGRAM INTERPOLATES MRF SURFACE FIELDS INTO HYCOM 
!   GRID AND WRITES THE RESULTS IN HYCOM-STYLE FILES. 
!
! USAGE:
!   INPUT FILES:
!     FTxxF001 - UNITS 11 THRU 49
!     UNIT  5  - (STANDARD READ)
!     UNIT  7  - FILE intp_pars.dat, COTROL RUN PARAMETRS
!     UNIT  8  - FILE regional.grid.b, DESCRIPTOR FOR HYCOM GRID 
!                (READ IN mod_geom.f90)
!     UNIT  9  - FILE regional.grid.a, HYCOM GRID 
!                (READ IN mod_geom.f90)
!     UNIT 33  - FILE listflx.dat, LIST OF DATES AND MRF FLUS FILES TO 
!                BE USED IN INTERPOLATIO.
!     UNIT 59  - FILE regional.depth.a, HYCOM BATHIMETRY 
!                (READ IN mod_geom.f90)
!     read mask either from unit 61 (switch mask_file_exist=.true.) otherwise
!     from unit 59
!     UNIT 61  - FILE regional.mask.a, HYCOM mask
!                (READ IN mod_geom.f90)
!     UNIT 81  - MRF GRIBBED FLUXES FILES WITH THE NAMES FROM THE LIST 
!                SPECIFIED IN listflx.dat.
!     UNIT 82  - THE SAME !     
!
!   OUTPUT FILES:  
!     FTxxF001 - UNITS 51 THRU 79
!     FTxxF001 - UNIT 6 (STANDARD PRINTFILE)
!     UNIT XX  - FILES forcing.*.a, HYCOM FORCING FILES
!     UNIT XX  - FILES forcing.*.d, DESCRIPTIORS TO HYCOM FORCING FILES 

use horiz_interp_mod
use mod_hycomio1
use mod_za,only : xcspmd,zaiost,zaiopf,zaiowr,zaiocl,idm,jdm
use mod_hytime
use mod_dump
use mod_geom
use mod_grib2io
use mod_flags
use grib_mod
use params

!
! mrfflxs: 1 is zonal momentum flux            [N/m^2]
!          2 is meridional momentum flux       [N/m^2]
!          3 is air temperature at 2m          [K]
!          4 is water vapor mixing ratio at 2m [kg/kg]
!          5 is precipitation rate             [kg/m^2/s]
!          6 is u-wind at 10m                  [m/s]
!          7 is v-wind at 10m                  [m/s]
!          8 is sensible heat flux post. up    [W/m^2] 
!          9 is latent heat flux post. up      [W/m^2]
!         10 is downward long wave flux        [W/m^2]
!         11 is upward long wave flux          [W/m^2]
!         12 is downward short wave flux       [W/m^2]
!         13 is upward short wave flux         [W/m^2]
!         14 is surface temperature            [K]
!         15 is sea level pressure             [Pa]
!         16 is land mask                      [land=1;sea=0]
!
! flxflg=flxflg_flxs
! hycom:    1 is zonal momentum flux                      [N/m^2]
!           2 is meridional momentum flux                 [N/m^2]
!           3 is sensible heat flux positive down         [W/m^2] 
!           4 is latent heat flux positive down           [W/m^2]
!           5 is solar penetrative heat flux in the ocean [W/m^2] 
!           6 is net radiative heat flux in the ocean     [W/m^2] 
!           7 is precipitative rate                       [m/s]
!           8 is sea level pressure                       [Pa]
!           9 is surface temperature                      [C] 
!     Comments:  flxflg_smpl option is used for the case flxflg =3 in hycom
! flxflg=flxflg_smpl
! hycom:    1 is zonal momentum flux                                                    [N/m^2]
!           2 is meridional momentum flux                                               [N/m^2]
!           3 is solar penetrative heat flux in the ocean [ net short_wave ]       [W/m^2] 
!           4 is net heat flux in the ocean (sensible+latent+long_wave+short_wave) [W/m^2] 
!           5 is water flux rate (precipitation - evaporation)                          [m/s]
!           6 is wind speed at 10m                                                      [m/s] 
!           7 is sea level pressure                                                     [Pa]
!           8 is surface temperature                                                    [C] 
!           9 is air temperature  (read in hycom but not used)                          [C]
!          10 is water vapor mixing ratio (read in hycom but not used)                  [kg/kg]
! flxflg=flxflg_flds
! hycom:    1 is zonal momentum flux                      [N/m^2]
!           2 is meridional momentum flux                 [N/m^2]
!           3 is air temperature at 10m                   [C] 
!           4 is water vapor mixing ratio at 10m          [kg/kg] 
!           5 is precipitative rate                       [m/s]
!           6 is wind speed at 10m                        [m/s] 
!           7 is solar penetrative heat flux in the ocean [W/m^2] 
!           8 is net radiative heat flux in the ocean     [W/m^2] 
!           9 is sea level pressure                       [Pa]
!          10 is surface temperature                      [C] 
! flxflg=flxflg_one - each item in flxflg_flds in different instance
!
! i is changing from west (1) to east (nx)
! j is changing from north (1) to south (ny)
!
implicit none
type(gribfield) :: gfld
type(horiz_interp_type) intrp
!
namelist /intp_pars/ dbgn,flxflg,avstep,mrffreq,avg3,wslocal
integer:: flxflg,dbgn,num_month,avg3,wslocal,nflux,astart,aend
!
!dbgn      debugging =0 - no dbg; =1,2,3 - add output
!flxflg    two admissible flags. One to be used if all fluxes are prescribed  (flxflg_flxs & flxflg_smpl)
!          the other one if turbulent fluxes are computed in the ocean model. (flxflg_flds)
!          See parameter values in mod_flags
!avstep    time for averaging fields (hrs)
!mrffreq   time interval(hrs) between MRF fields
!avg3	   if avg3 = 1, then averaged fluxes are converted to instantaneous fields
!wslocal   if  wslocal = 1, then wind stress are computed from wind velocities
!
real,dimension(:,:),allocatable :: mtaux, htaux

real:: avstep,mrffreq,cs,ss,speed,cdval,cd
!
! Constants:
!
real,parameter::   &
   t0=273.17       &  
  ,evaplh=2.47e6   &  ! latent heat of evaporation (j/kg)
  ,thref =1.0e-3   &  ! reference value of specific volume (m**3/kg) 
  ,minspd=3.0	   &  ! minimum value of speed for calculating cd
  ,cdmax=0.0025	      ! max value of cd
integer, parameter :: nmrf=16,nextrap_max=100
!
integer nxhycom,mapflg,nyhycom,nhycom,i,j,iii,jjj,k,n,nu,m,mu,ntime &
 & ,nxmrf,nymrf,nxmrf_prev,nymrf_prev,nymrf2,na,namax ,hh1,hh2,ndiff,nextrap,lua,lub,ii,edges,idum &
&  ,lua1,lub1,open_gate,namax_climo,iparm,jdiscno,jpdtno,xpts,ypts,nhr
integer, dimension(:,:), allocatable:: imsk_intp,imsk_hycom
real, dimension(:), allocatable :: exhycom,eyhycom,exmrf,eymrf,htime,off_time
real, dimension(:,:), allocatable :: exhycom2d,eyhycom2d
real, dimension(:,:), allocatable :: qxhycom2d,qyhycom2d,anhycom2d
real, dimension(:,:), allocatable:: mrfflx ,msk_in,msk_out,mskmrf_tmp,sensible,latent,lwrad,swrad
real, dimension(:,:,:), allocatable :: mrfflxs,hycomflxs,workflxs
real, dimension(:,:,:), allocatable :: hycomflxb,hycomflxp
real, dimension(:,:), allocatable :: hycomflxv,hycomflxu
real ::  reflon=0.,reflat=0.,gridsz=0.,pntlat=0.,chtime,ftime,dmin,dmax,mskfrac=0.99 &
 & ,fldmin,fldmax
character (len=6), dimension(:),allocatable :: hyflxs1
character (len=10), dimension (:),allocatable :: ctime
character (len=100), dimension (:),allocatable :: mrfnames
integer, dimension(:), allocatable :: vecflxs1,avgflxs1

character (len=10) big_ben(3)
character:: preambl(5)*79
logical :: clsgrib,grid_file_exist=.false.,mrf_grid_changed &
     & ,mask_file_exist=.false.
integer, dimension(4,nmrf) :: kpds567
INTEGER, DIMENSION(nmrf) :: jdisc_num,jpdt_num
integer, dimension(200) :: gds 
integer, dimension(8) :: date_time
integer, dimension(4) :: kpds
real,dimension(5000000)      :: xgfld
character(len=100) :: arg
data jdisc_num/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2/
!
!
! Read parameters and allocate arrays
!
  open (7,file='intp_pars.dat')
  read(7,intp_pars)
  close(7)
  open (27,file='jpdt_table.dat')
  read(27,*)(jpdt_num(i),i=1,nmrf)
  close(27)
  write(*,intp_pars); call flush(6)
! check if regional.grid.[ab] exists and read mapflg. nxhycom,nyhycom will be 
! redefined few lines later after call xcspmd
  call rd_hycom_grid_params(nxhycom,nyhycom,mapflg,grid_file_exist)
  if(grid_file_exist) then
    write (*,*) ' ---- Files regional.grid.[ab] are found'
    call xcspmd  !input idm,jdm by use association
    nxhycom=idm
    nyhycom=jdm
  else
    write(*,*) 'WARNING: regional.grid.[ab] are not found'
!   ilya: reflat is not used now in grid calculations (assumed to be =0)
!   the following subroutine reads blkdat.input only in HYCOM 2.0 format.
    call rd_blkdata(10,nxhycom,nyhycom,mapflg,reflon,reflat,pntlat,gridsz)
    idm=nxhycom  ! idm,jdm are used instead of nxhycom,nyhycom in 
    jdm=nyhycom  ! zaio* routines.
  endif
!  write(*,*) ' --- FINALLY: nxhycom=',nxhycom,' nyhycom=',nyhycom,' mapflg=',mapflg
! initialize output 
  call zaiost
  if ( flxflg == flxflg_flxs ) then
    nhycom=9                                          ! number of HYCOM flxs
  elseif ( flxflg == flxflg_flds ) then
    nhycom=10
  elseif ( flxflg == flxflg_smpl ) then
    nhycom=10
  elseif ( flxflg == flxflg_one ) then
    nhycom=1
    if (iargc() < 1) then
       write(*,'(a)') "Error: no command line arguments with gfs2ofs2"
       write(*,'(a)') "usage: ./gfs2ofs2 arg"
       write(*,'(a)') "error stop STOP"
       stop
    endif
    call getarg(1,arg)
    read(arg,*)nflux
!    read (5,*) nflux
    print*,'nflux = ',nflux
  else
     write(*,*) '=== ERROR in interpolation: wrong flag flxflg=',flxflg
     stop
  end if
!
!reads list of input data files
  open (33,file='listflx.dat',form='formatted')
  read(33,*) ntime
  allocate (htime(ntime),ctime(ntime),mrfnames(ntime))
  read(33,'(a10,1x,a100)') (ctime(i),mrfnames(i),i=1,ntime)
  do i=1,ntime
     mrfnames(i)=adjustl(mrfnames(i))
     mrfnames(i)=mrfnames(i)(1:scan(mrfnames(i),' ')-1)
  enddo
  close(33)
!  write(*,*) '# of times:',ntime
!  write(*,*)'  Time     File name'
!  write (*,'(a10," ",a)') (ctime(i),trim(mrfnames(i)),i=1,ntime)
  call flush(6)

!"HYCOM" time
    do i =1,ntime 
      call hytime(ctime(i),htime(i)) ; 
      write (*,*) htime(i), ctime(i) ; 
    end do  
    namax_climo=4*30


allocate (hycomflxs(1:nxhycom,1:nyhycom,1:nhycom),msk_out(1:nxhycom,1:nyhycom) &
 &  ,hyflxs1(nhycom),vecflxs1(nhycom),hycomflxu(1:nxhycom,1:nyhycom),hycomflxv(1:nxhycom,1:nyhycom))

allocate (avgflxs1(nhycom),hycomflxp(1:nxhycom,1:nyhycom,1:nhycom),hycomflxb(1:nxhycom,1:nyhycom,1:nhycom))
allocate (off_time(1:nhycom))
!
! 10/5/2016 hsk's doing in order to have values for anhycom2d
allocate (qxhycom2d(1:nxhycom+1,1:nyhycom+1),qyhycom2d(1:nxhycom+1,1:nyhycom+1) &
         ,anhycom2d(1:nxhycom,1:nyhycom))

  if (mapflg==mapflg_mer) then
    allocate ( exhycom(nxhycom+1),eyhycom(nyhycom+1) )
    allocate ( exhycom2d(2,2),eyhycom2d(2,2) )  ! backwards compatibility (remove later on)
    edges=1
  else
    allocate (exhycom2d(1:nxhycom+1,1:nyhycom+1),eyhycom2d(1:nxhycom+1,1:nyhycom+1))
    edges=0
  endif

!  write (*,*) 'HYCOM Files are allocated' ; 
call flush(6)

if ( flxflg == flxflg_flxs ) then
  hyflxs1=(/'tauewd','taunwd','snsibl','latent','shwflx','radflx','precip','presur','surtmp'/)
!  nhycom=9

  vecflxs1=0
  vecflxs1(1)=1
  vecflxs1(2)=2

  avgflxs1=1
  if ( wslocal==1 ) then
    avgflxs1(1:2)=0
  endif
  avgflxs1(8)=0
  avgflxs1(9)=0

  off_time=0.
  do n=1,nhycom
     if (avgflxs1(n)==1) then
        off_time(n)=-avstep/2./24.
     endif
  enddo

elseif ( flxflg == flxflg_flds ) then
  hyflxs1=(/'tauewd','taunwd','airtmp','vapmix','precip','wndspd','shwflx','radflx','presur','surtmp'/)
!  nhycom=10

  vecflxs1=0
  vecflxs1(1)=1
  vecflxs1(2)=2


  avgflxs1=1
  if ( wslocal==1 ) then
     avgflxs1(1:2)=0
  endif
  avgflxs1(3)=0
  avgflxs1(4)=0
  avgflxs1(6)=0
  avgflxs1(7)=0
  avgflxs1(8)=0
  avgflxs1(9)=0
  avgflxs1(10)=0
    
  off_time=0.0
  do n=1,nhycom
     if (avgflxs1(n)==1) then
        off_time(n)=-avstep/2./24. ! accumulated/average fields are off set by a half of avgstep hours [day]
     endif
  enddo

elseif ( flxflg == flxflg_one ) then
  select case(nflux)
     case(1)
        hyflxs1=(/'tauewd'/)
     case(2)
        hyflxs1=(/'taunwd'/)
     case(3)
        hyflxs1=(/'airtmp'/)
     case(4)
        hyflxs1=(/'vapmix'/)
     case(5)
        hyflxs1=(/'precip'/)
     case(6)
        hyflxs1=(/'wndspd'/)
     case(7)
        hyflxs1=(/'shwflx'/)
     case(8)
        hyflxs1=(/'radflx'/)
     case(9)
        hyflxs1=(/'presur'/)
     case(10)
        hyflxs1=(/'surtmp'/)
     case default
        ! error cycle parsefmt
  end select

  ! default values
  vecflxs1=0
  avgflxs1=0
  if (nflux == 1) then
    vecflxs1=1 
    if (wslocal==0) then
      avgflxs1=1
    endif
  endif
  if (nflux == 2) then
    vecflxs1=1 
    if (wslocal==0) then
      avgflxs1=1
    endif
  endif
  if (nflux == 5) then
    avgflxs1=1 
  endif

  off_time=0.0
  if (nflux==1.or.nflux==2.or.nflux==5.or.nflux==7.or.nflux==8) then
     off_time(1)=-avstep/2./24. ! accumulated/average fields are off set by a half of avgstep hours [day]
  endif

elseif ( flxflg == flxflg_smpl ) then
  hyflxs1=(/'tauewd','taunwd','shwflx','radflx','precip','wndspd','presur','surtmp','airtmp','vapmix'/)
!    nhycom=10

    vecflxs1=0
    vecflxs1(1)=1
    vecflxs1(2)=2

    avgflxs1=1
    if (wslocal==1 ) then
      avgflxs1(1:2)=0
    endif
    avgflxs1(6)=0
    avgflxs1(7)=0
    avgflxs1(8)=0
    avgflxs1(9)=0
    avgflxs1(10)=0

    off_time=0.
    do n=1,nhycom
      if (avgflxs1(n)==1) then
         off_time(n)=-avstep/2./24.
      endif
    enddo
endif

  kpds567=RESHAPE (source=  &
       (/002, 017, 001, 000  &  ! UFLX        1
       ,002,  018, 001, 000  &  ! VFLX        2
       ,000,  000, 103, 002  &  ! TMP 2m      3
       ,001,  000, 103, 002  &  ! SPFH 2m     4
       ,001,  007, 001, 000  &  ! PRATE       5
       ,002,  002, 103, 010  &  ! UGRD 10m    6
       ,002,  003, 103, 010  &  ! VGRD 10m    7
       ,000,  011, 001, 000  &  ! SHTFL       8
       ,000,  010, 001, 000  &  ! LHTFL       9
       ,005,  192, 001, 000  &  ! DLWRF      10
       ,005,  193, 001, 000  &  ! ULWRF      11
       ,004,  192, 001, 000  &  ! DSWRF      12
       ,004,  193, 001, 000  &  ! USWRF      13
       ,000,  000, 001, 000  &  ! TMP 0m     14
       ,003,  000, 001, 000  &  ! PRES       15
       ,000,  000, 001, 000 /) &  ! LAND     16
       ,shape = (/ 4,nmrf /) )
!
! Chape = (/ 3,nmrf /) )lculate HYCOM grid 

if (grid_file_exist) then 
   if(mapflg==mapflg_mer) then
       call hycom_na_mercator(exhycom,eyhycom,dbgn)
! required because of anhycom2d is needed. by hsk 10/5/2016
!print*,'size(qxhycom2d)=',size(qxhycom2d,dim=1),size(qxhycom2d,dim=2)
        call hycom_na(anhycom2d,qxhycom2d,qyhycom2d,dbgn)
   else
        call hycom_na_mercator(gridsz,pntlat,reflon,exhycom,eyhycom,dbgn)
   endif
else
   write(*,*) 'ERROR: no regional.grid.[ab] files for mapflg=',mapflg
   call flush(6)
   stop
endif

!
! For the averaging
!
chtime=htime(1)
if (ntime==1) then
    namax=1
    na=1
    workflxs=0.
else
    namax=avstep/mrffreq
    na=1
end if

if(namax == namax_climo) then
       write(*,*)' Climatological style records '
else
       write(*,*)' High frequency style records '
endif

  open_gate=1

! 
! Read HYCOM land/sea mask from regional.mask.a (land=0,sea=1)
!
! --- hsk 2017: don't consider mask. Make it all sea (1)
!  print *,'--- Calculating HYCOM mask'; call flush(6)
  allocate (imsk_hycom(1:nxhycom,1:nyhycom))
!if (mask_file_exist) then
!    call mask_hycom_2(imsk_hycom)
!    print *,'--- from regional mask'
!else
!    call mask_hycom_1(imsk_hycom)
!    print *,'--- from regional depth'
!    write(*,*) 'imsk_hycom min, max = ',minval(imsk_hycom),maxval(imsk_hycom)
!endif
! 
!<-hsk turn off finding mask in hycom
!
  imsk_hycom=1
num_month=1
timeloop: do m=1,ntime
    
    write (*,*) '<--------- '//trim(ctime(m))//' ********'
    print *, "Inside Time Loop ",m
!
!   Get MRF grid parameters (before  20021029.t12Z: 512x256, after: 768x384)

    call rdgrib(99+m*2,TRIM(mrfnames(m)),xgfld,kpds,jpdtno,jdiscno,0,xpts,ypts)
    nxmrf=xpts ; 
    nymrf=ypts
    nymrf2=nymrf/2 
    if (m==1) then 
      nxmrf_prev=nxmrf ; nymrf_prev=nymrf
      mrf_grid_changed=.true.
    else
      if (nxmrf==nxmrf_prev .and. nymrf==nymrf_prev ) then
         mrf_grid_changed=.false.
      else
         nxmrf_prev=nxmrf ; 
         nymrf_prev=nymrf
         mrf_grid_changed=.true.
      endif
    endif 
!   print *, 'm,nxmrf_prev,nxmrf,nymrf_prev,nymrf,mrf_grid_changed=',m,nxmrf_prev,nxmrf,nymrf_prev,nymrf,mrf_grid_changed
    if (mrf_grid_changed) then
!     allocate arrays on MRF grid
      if (m>1) deallocate(mrfflx,latent,sensible,lwrad,swrad,msk_in,workflxs,mskmrf_tmp)
      allocate (mrfflx(1:nxmrf,1:nymrf),latent(1:nxmrf,1:nymrf),swrad(1:nxmrf,1:nymrf)&
 &      ,sensible(1:nxmrf,1:nymrf),lwrad(1:nxmrf,1:nymrf),msk_in(1:nxmrf,1:nymrf) &
 &      ,workflxs(nxmrf,nymrf,nhycom),mskmrf_tmp(nxmrf,nymrf))
      if(edges==1 )then
        write(*,*) 'Set mrf at edges'
         if (m>1) deallocate(exmrf,eymrf)
        allocate ( exmrf(nxmrf+1),eymrf(nymrf+1)  )
      else
        write(*,*) 'Set mrf at center points'
         if (m>1) deallocate(exmrf,eymrf)
         allocate ( exmrf(nxmrf),eymrf(nymrf)  )
      endif
!
!     Calculate MRF grid  ( coordinates are supposed to increase )
      call mrf_gaussian(exmrf,eymrf,edges,dbgn)
!
!     Allocate array of MRF fluxes
      if (m>1) deallocate(mrfflxs)
      allocate(mrfflxs(1:nxmrf,1:nymrf,nmrf))
!
!     Pre-compute interpolation indices and weights for multiple interpolations
!
      if(mapflg==mapflg_mer) then
        call horiz_interp_init(intrp,exmrf,eymrf,exhycom,eyhycom,verbose=99)
      else
         call horiz_intp(exmrf,eymrf,qxhycom2d,qyhycom2d,exhycom2d,eyhycom2d,idum)
      endif
!
!     Establish MRF mask (land=0,sea=1). 
!
      print *,'--- Changing MRF mask'; call flush(6)
      call mask_mrf(82,intrp,trim(mrfnames(1)),kpds567(:,nmrf),mskfrac,nextrap_max,msk_in &
 &      ,imsk_hycom,mskmrf_tmp,nextrap,mapflg,exhycom2d,eyhycom2d)
      if(nextrap>=nextrap_max) then
        print *,'ERROR: nextrap>=nextrap_max, nextrap=',nextrap,' nextrap_max=',nextrap_max
      endif
    endif
! 
!   Read MRF fluxes from GRIB
!
    clsgrib=.false.
    astart=1
    aend=nmrf-1
    if ( flxflg == flxflg_one ) then
      if (nflux.eq.1) then
        astart=1
        aend=1
      endif
      if (nflux.eq.2) then
        astart=2
        aend=2
      endif
      if (nflux.eq.3) then
        astart=3
        aend=3
      endif
      if (nflux.eq.4) then
        astart=4
        aend=4
      endif
      if (nflux.eq.5) then
        astart=5
        aend=5
      endif
      if (nflux.eq.6) then
        astart=6
        aend=7
      endif
      if (nflux.eq.7) then
        astart=12
        aend=13
      endif
      if (nflux.eq.8) then
        astart=10
        aend=13
      endif
      if (nflux.eq.9) then
        astart=15
        aend=15
      endif
      if (nflux.eq.10) then
        astart=14
        aend=14
      endif
   endif

    do i=astart,aend
        jpdtno=jpdt_num(i)
        jdiscno=jdisc_num(i)
      if (i==nmrf-1) clsgrib=.true.
      kpds=kpds567(:,i)
      if( wslocal==1 .and. i==1) then  !use zonal wind
               kpds=kpds567(:,6)
               jpdtno=jpdt_num(6)
               jdiscno=jdisc_num(6)
      elseif( wslocal==1 .and. i==2) then !use meridional wind
               kpds=kpds567(:,7)
               jpdtno=jpdt_num(7)
               jdiscno=jdisc_num(7)
      endif
      CALL rdgrib(499+m*2,TRIM(mrfnames(m)),xgfld,kpds,jpdtno,jdiscno,1,xpts,ypts)
      mrfflx=reshape(source=xgfld,shape=SHAPE(mrfflx))
      mrfflxs(:,:,i)=mrfflx
      print*,'MRF fluxes: i,min,max=',i,minval(mrfflxs(:,:,i)),maxval(mrfflxs(:,:,i))

    end do
!
   if (na==namax+1) then
      workflxs=0.
      na=1
      num_month=num_month+1
    end if
    chtime=((na-1.)*chtime+htime(m))/na
!
!   Intermediate MRF fluxes 
!
    sensible=mrfflxs(:,:,8)
    latent=mrfflxs(:,:,9); ! where(latent<0.)latent=0.   ! no condensation
    swrad=mrfflxs(:,:,12)-mrfflxs(:,:,13)
    lwrad=mrfflxs(:,:,10)-mrfflxs(:,:,11)
    if ( flxflg == flxflg_flxs ) then
      workflxs(:,:,1)=((na-1.)*workflxs(:,:,1)-mrfflxs(:,:,1))/na
      workflxs(:,:,2)=((na-1.)*workflxs(:,:,2)-mrfflxs(:,:,2))/na
      workflxs(:,:,3)=((na-1.)*workflxs(:,:,3)-sensible)/na
      workflxs(:,:,4)=((na-1.)*workflxs(:,:,4)-latent)/na
      workflxs(:,:,5)=((na-1.)*workflxs(:,:,5)+swrad)/na
      workflxs(:,:,6)=((na-1.)*workflxs(:,:,6)+lwrad+swrad)/na
      workflxs(:,:,7)=((na-1.)*workflxs(:,:,7)+mrfflxs(:,:,5)*thref)/na
      workflxs(:,:,8)=((na-1.)*workflxs(:,:,8)+mrfflxs(:,:,15))/na
      workflxs(:,:,9)=((na-1.)*workflxs(:,:,9)+mrfflxs(:,:,14)-t0)/na
    elseif ( flxflg == flxflg_smpl ) then
      workflxs(:,:,1)=((na-1.)*workflxs(:,:,1)-mrfflxs(:,:,1))/na
      workflxs(:,:,2)=((na-1.)*workflxs(:,:,2)-mrfflxs(:,:,2))/na
      workflxs(:,:,3)=((na-1.)*workflxs(:,:,3)+swrad)/na
      workflxs(:,:,4)=((na-1.)*workflxs(:,:,4)+lwrad+swrad-sensible-latent)/na
      workflxs(:,:,5)=((na-1.)*workflxs(:,:,5)+(mrfflxs(:,:,5)-latent(:,:)/evaplh)*thref)/na
      workflxs(:,:,6)=((na-1.)*workflxs(:,:,6)+sqrt(mrfflxs(:,:,6)**2+mrfflxs(:,:,7)**2))/na
      workflxs(:,:,7)=((na-1.)*workflxs(:,:,7)+mrfflxs(:,:,15))/na
      workflxs(:,:,8)=((na-1.)*workflxs(:,:,8)+mrfflxs(:,:,14)-t0)/na
      workflxs(:,:,9)=((na-1.)*workflxs(:,:,9)+mrfflxs(:,:,3)-t0)/na
      workflxs(:,:,10)=((na-1.)*workflxs(:,:,10)+mrfflxs(:,:,4))/na
     elseif ( flxflg == flxflg_flds ) then
      workflxs(:,:,1)=((na-1.)*workflxs(:,:,1)-mrfflxs(:,:,1))/na
      workflxs(:,:,2)=((na-1.)*workflxs(:,:,2)-mrfflxs(:,:,2))/na
! IMPORTANT : temperature and humidity are at 2m, not 10m
      workflxs(:,:,3)=((na-1.)*workflxs(:,:,3)+mrfflxs(:,:,3)-t0)/na
      workflxs(:,:,4)=((na-1.)*workflxs(:,:,4)+mrfflxs(:,:,4))/na
      workflxs(:,:,5)=((na-1.)*workflxs(:,:,5)+mrfflxs(:,:,5)*thref)/na
      workflxs(:,:,6)=((na-1.)*workflxs(:,:,6)+sqrt(mrfflxs(:,:,6)**2+mrfflxs(:,:,7)**2))/na
      workflxs(:,:,7)=((na-1.)*workflxs(:,:,7)+swrad)/na
      workflxs(:,:,8)=((na-1.)*workflxs(:,:,8)+lwrad+swrad)/na
      workflxs(:,:,9)=((na-1.)*workflxs(:,:,9)+mrfflxs(:,:,15))/na
      workflxs(:,:,10)=((na-1.)*workflxs(:,:,10)+mrfflxs(:,:,14)-t0)/na
     elseif ( flxflg == flxflg_one ) then
      if (nflux.eq.1) then
        workflxs(:,:,1)=((na-1.)*workflxs(:,:,1)-mrfflxs(:,:,1))/na
      endif
      if (nflux.eq.2) then
        workflxs(:,:,1)=((na-1.)*workflxs(:,:,1)-mrfflxs(:,:,2))/na
      endif
      if (nflux.eq.3) then
        workflxs(:,:,1)=((na-1.)*workflxs(:,:,1)+mrfflxs(:,:,3)-t0)/na
      endif
      if (nflux.eq.4) then
        workflxs(:,:,1)=((na-1.)*workflxs(:,:,1)+mrfflxs(:,:,4))/na
      endif
      if (nflux.eq.5) then
        workflxs(:,:,1)=((na-1.)*workflxs(:,:,1)+mrfflxs(:,:,5)*thref)/na
      endif
      if (nflux.eq.6) then
        workflxs(:,:,1)=((na-1.)*workflxs(:,:,1)+sqrt(mrfflxs(:,:,6)**2+mrfflxs(:,:,7)**2))/na
      endif
      if (nflux.eq.7) then
        workflxs(:,:,1)=((na-1.)*workflxs(:,:,1)+swrad)/na
      endif
      if (nflux.eq.8) then
        workflxs(:,:,1)=((na-1.)*workflxs(:,:,1)+lwrad+swrad)/na
      endif
      if (nflux.eq.9) then
        workflxs(:,:,1)=((na-1.)*workflxs(:,:,1)+mrfflxs(:,:,15))/na
      endif
      if (nflux.eq.10) then
        workflxs(:,:,1)=((na-1.)*workflxs(:,:,1)+mrfflxs(:,:,14)-t0)/na
      endif
   end if

    avend: if (na==namax) then
!
!     Loop over fluxes
! <
   flxloop: do i=1,nhycom
!
!       Interpolate 
!(NOTE: later make calls to more efficient multiple interpolation procedures).
!
        mskmrf_tmp=1.
        if(mapflg==mapflg_mer) then
           call horiz_interp(intrp,workflxs(:,:,i),hycomflxp(:,:,i),verbose=0,mask_in=mskmrf_tmp,mask_out=msk_out)
        else
           call horiz_intp_(workflxs(:,:,i),hycomflxp(:,:,i),    &
&                             exhycom2d(:,:),eyhycom2d(:,:))
        endif

! 3-hourly GFS data
! hsk's doing @ 10/31/2011
        if ( (avg3 == 1) .and. (avgflxs1(i) == 1) ) then
           if(m.eq.1) then
              hycomflxs(:,:,i) = hycomflxp(:,:,i)
              hycomflxb(:,:,i) = hycomflxp(:,:,i) !xb field used for m==2
           elseif(m.eq.2) then
              nhr=htime(m)*24
              elseif (mod(nhr,6)==0) then
              hycomflxs(:,:,i) = 2.*hycomflxp(:,:,i) - hycomflxb(:,:,i)
           elseif ( mod(nhr,6) == 3) then
              hycomflxs(:,:,i) = hycomflxp(:,:,i)
              hycomflxb(:,:,i) = hycomflxp(:,:,i) !xb field is used for m-even
           endif
        else
           hycomflxs(:,:,i) = hycomflxp(:,:,i)
        endif

! 1-hourly GDAS data:
        if ( (avg3 == 2) .and. (avgflxs1(i) == 1) ) then
           if(m.eq.1) then
! this very first file will be here to provide "hycomflxb only"
              hycomflxs(:,:,i) = hycomflxp(:,:,i)
              hycomflxb(:,:,i) = hycomflxp(:,:,i) !xb field used for m==2
           elseif(m.ge.2) then
              nhr=htime(m)*24
           elseif ( mod(nhr,6) == 2) then
              hycomflxs(:,:,i) = 2.*hycomflxp(:,:,i) - 1.*hycomflxb(:,:,i)
              hycomflxb(:,:,i) = hycomflxp(:,:,i)
           elseif ( mod(nhr,6) == 3) then
              hycomflxs(:,:,i) = 3.*hycomflxp(:,:,i) - 2.*hycomflxb(:,:,i)
              hycomflxb(:,:,i) = hycomflxp(:,:,i)
           elseif ( mod(nhr,6) == 4) then
              hycomflxs(:,:,i) = 4.*hycomflxp(:,:,i) - 3.*hycomflxb(:,:,i)
              hycomflxb(:,:,i) = hycomflxp(:,:,i)
           elseif ( mod(nhr,6) == 5) then
              hycomflxs(:,:,i) = 5.*hycomflxp(:,:,i) - 4.*hycomflxb(:,:,i)
              hycomflxb(:,:,i) = hycomflxp(:,:,i)
           elseif ( mod(nhr,6) == 0) then
              hycomflxs(:,:,i) = 6.*hycomflxp(:,:,i) - 5.*hycomflxb(:,:,i)
              hycomflxb(:,:,i) = hycomflxp(:,:,i)
           elseif ( mod(nhr,6) == 1) then
              hycomflxs(:,:,i) = 1.*hycomflxp(:,:,i) - 0.*hycomflxb(:,:,i)
              hycomflxb(:,:,i) = hycomflxp(:,:,i)
           endif
        else
           hycomflxs(:,:,i) = hycomflxp(:,:,i)
        endif
! hsk's doing:

! project vector given in zonal and meridional directions
! along x and y hycom grid directions.
        if (vecflxs1(i)==1) then
           hycomflxu=hycomflxs(:,:,i)
        elseif (vecflxs1(i)==2) then
           do jjj=1,nyhycom
           do iii=1,nxhycom
              cs=cos(anhycom2d(iii,jjj))
              ss=sin(anhycom2d(iii,jjj))
              hycomflxv(iii,jjj)= hycomflxs(iii,jjj,i)*cs-    &
&                                 hycomflxu(iii,jjj  )*ss    
              hycomflxu(iii,jjj)= hycomflxs(iii,jjj,i)*ss+    &
&                                 hycomflxu(iii,jjj  )*cs    
              if (wslocal==1 .and. i==2 ) then !this is the wind stress
                    speed=max(minspd,sqrt(hycomflxu(iii,jjj)**2+hycomflxv(iii,jjj)**2))
                    cdval=min(cd(speed),cdmax)*speed
!here we change sign to compensate for a change in sign in the averaging loop
                    hycomflxu(iii,jjj)=-  cdval*hycomflxu(iii,jjj)
                    hycomflxv(iii,jjj)=-  cdval*hycomflxv(iii,jjj)
              endif
           enddo
           enddo
        endif
!       Write air-sea fluxes in HYCOM format (.a and .b files).
!
!ilya May 23, 2003: msk_in instead of msk_out
        lua=10+2*(i-1) ; lub=lua+1001
        if (open_gate==1) then
           if(i==nhycom)open_gate=0
          call date_and_time (big_ben(1),big_ben(2),big_ben(3),date_time)
          open (unit=lub,file='forcing.'//trim(hyflxs1(i))//'.b',status='new' ,action='write')
          write(preambl(1),'(a,i4,''-'',i2,''-'',i2,2x,''['',i5,'']'',2x,i2,'':'',i2)') &
           'GDAS derived fields created ',(date_time(ii),ii=1,6)
          do ii=2,4 ; preambl(ii) = ' ' ; end do
!             if(mapflg==mapflg_mer) then
!                write(preambl(5),'(a,2i5,f9.3,f9.2,f6.3)') &
! &          'i/jdm,reflon,pntlat,gridsz =', &
! &          nxhycom,nyhycom,reflon,pntlat,gridsz
!             else
                write(preambl(5),'(a,2i5,a)') &
 &          'i/jdm =', &
 &          nxhycom,nyhycom,' orthogonal'
!             endif
          write(lub,'(A79)') preambl
          call zaiopf('forcing.'//trim(hyflxs1(i))//'.a','new' , lua)
        endif
! Bhavani
!        print *, " M NAMAX before 120oop ",m,namax
       if (namax.eq.namax_climo) then
!        print *, " AVSTEP from eq 180 loop ",namax
        if(mapflg==mapflg_mer) then
           call zaiowr(hycomflxs(:,:,i),imsk_hycom,.false., fldmin,fldmax, lua,.false.)
           write(lub,'(A,'': month,range = '',I10,1P2E16.7)') &
  &       '   '//trim(hyflxs1(i)),num_month,fldmin,fldmax
        else
           if (vecflxs1(i)==0) then
              call zaiowr(hycomflxs(:,:,i),imsk_hycom,.true., fldmin,fldmax, lua,.false.)
              write(lub,'(A,'': month,range = '',I10,1P2E16.7)') &
  &       '   '//trim(hyflxs1(i)),num_month,fldmin,fldmax
           elseif(vecflxs1(i)==2) then
!dbgz
             write(*,*)' ***** 1/2 ****** hyflxs1,lub,lub1,vecflxs1=',hyflxs1(i-1),trim(hyflxs1(i-1)),' ',trim(hyflxs1(i)),lub,lub1,vecflxs1(i)
              call zaiowr(hycomflxu(:,:),imsk_hycom,.false., fldmin,fldmax, lua1,.false.)
              write(lub1,'(A,'': month,range = '',I10,1P2E16.7)') &
                   &       '   '//trim(hyflxs1(i-1)),num_month,fldmin,fldmax
              call zaiowr(hycomflxv(:,:),imsk_hycom,.true., fldmin,fldmax, lua,.false.)
              write(lub,'(A,'': month,range = '',I10,1P2E16.7)') &
                   &       '   '//trim(hyflxs1(i)),num_month,fldmin,fldmax
           elseif(vecflxs1(i)==1) then
              lua1=lua
              lub1=lub
!dbgz
             write(*,*)' ***** 1/2 ****** hyflxs1,lub,lub1,vecflxs1=',hyflxs1(i-1),trim(hyflxs1(i-1)),' ',trim(hyflxs1(i)),lub,lub1,vecflxs1(i)
           endif
        endif
      else
!        print *, " AVSTEP from ne 180 loop ",m,namax

        ftime=htime(m)+off_time(i)
        if(mapflg==mapflg_mer) then
           call zaiowr(hycomflxs(:,:,i),imsk_hycom,.false., fldmin,fldmax, lua,.false.)
           write(lub,'(A,'': date,span,range = '',F10.2,'' 0 '',1P2E16.7)') &
  &       '   '//trim(hyflxs1(i)),ftime,fldmin,fldmax
        else
           if (vecflxs1(i)==0) then
              call zaiowr(hycomflxs(:,:,i),imsk_hycom,.true., fldmin,fldmax, lua,.false.)
              write(lub,'(A,'': date,span,range = '',F10.2,'' 0 '',1P2E16.7)') &
  &       '   '//trim(hyflxs1(i)),ftime,fldmin,fldmax
           elseif(vecflxs1(i)==2) then
!dbgz
             write(*,*)' ***** 2/2 ****** hyflxs1,lub,lub1,vecflxs1=',hyflxs1(i-1),trim(hyflxs1(i-1)),' ',trim(hyflxs1(i)),lub,lub1,vecflxs1(i)
              call zaiowr(hycomflxu(:,:),imsk_hycom,.false., fldmin,fldmax, lua1,.false.)
              write(lub1,'(A,'': date,span,range = '',F10.2,'' 0 '',1P2E16.7)') &
                   &       '   '//trim(hyflxs1(i-1)),ftime,fldmin,fldmax
              write(*,'(A,'': date,span,range = '',F10.2,'' 0 '',1P2E16.7)') &
                   &       '   '//trim(hyflxs1(i-1)),ftime,fldmin,fldmax
              call zaiowr(hycomflxv(:,:),imsk_hycom,.true., fldmin,fldmax, lua,.false.)
              write(lub,'(A,'': date,span,range = '',F10.2,'' 0 '',1P2E16.7)') &
                   &       '   '//trim(hyflxs1(i)),ftime,fldmin,fldmax
              write(*,'(A,'': date,span,range = '',F10.2,'' 0 '',1P2E16.7)') &
                   &       '   '//trim(hyflxs1(i)),ftime,fldmin,fldmax
           elseif(vecflxs1(i)==1) then
              lua1=lua
              lub1=lub
!dbgz
               write(*,*)' ***** 2/1 ****** hyflxs1(i),lub,lub1,vecflxs1=',trim(hyflxs1(i)),lub,lub1,vecflxs1(i)
           endif
        endif
      endif 
        if (m==ntime) then
           if(mapflg==mapflg_mer) then
              close(lub)
              call zaiocl(lua)
           else

              if (vecflxs1(i)==0) then
                 close(lub)
                 call zaiocl(lua)
              elseif(vecflxs1(i)==2) then
                 close(lub)
                 call zaiocl(lua)
                 close(lub1)
                 call zaiocl(lua1)
              endif
           endif
        endif
!
!       Additional output
!

        if (dbgn>=1) then
           if (mapflg==mapflg_mer) then
              write(*,'(a10,'' '',a10,''  min'',e13.6,'' max'',e13.6)')ctime(m),trim(hyflxs1(i)) &
 &        ,minval(hycomflxs(:,:,i)),maxval(hycomflxs(:,:,i))
              call flush(6)
           endif
        endif

!
!       Output for debugging.
!
!         for GrADS:
        if (dbgn>=4) then
          nu=50
          if (mapflg==mapflg_mer) then

          open (unit=nu,file='GrADS.'//trim(hyflxs1(i))//'.gdas1.'//trim(ctime(m)),form="unformatted")
          write(nu) hycomflxs(:,:,i)
          close(nu) 
          endif
       endif

!         for MATLAB
       if (dbgn>=3) then
          mu=100
          if (mapflg==mapflg_mer) then
          open (unit=mu,file='MATLAB.'//trim(hyflxs1(i))//'.gdas1.'//trim(ctime(m)),form="formatted")
          write(mu,"(e18.6)") hycomflxs(:,:,i)
          close(mu)                               
          else

             if (vecflxs1(i)==0) then
                open (unit=mu,file='MATLAB.'//trim(hyflxs1(i))//'.gdas1.'//trim(ctime(m)),form="formatted")
                write(mu,"(e18.6)") hycomflxs(:,:,i)
                close(mu)   
             elseif(vecflxs1(i)==1) then                            
                open (unit=mu,file='MATLAB.'//trim(hyflxs1(i))//'.gdas1.'//trim(ctime(m)),form="formatted")
             elseif(vecflxs1(i)==2) then
                open (unit=mu+1,file='MATLAB.'//trim(hyflxs1(i))//'.gdas1.'//trim(ctime(m)),form="formatted")
                write(mu,"(e18.6)") hycomflxu(:,:)
                close(mu)
                write(mu+1,"(e18.6)") hycomflxv(:,:)
                close(mu+1)
             endif
             open (unit=mu+2,file='MATLAB.'//trim(hyflxs1(i))//'.GDAS1.'//trim(ctime(m)),form="formatted")
             write(mu+2,"(e18.6)")workflxs(:,:,i)
             close(mu+2)                               
          endif
        end if
!
      end do flxloop
!
    end if avend
    na=na+1
!
  end do timeloop

       if (dbgn>=1 ) then

          mu=100
          open (unit=mu,file='MATLAB_GDAS_GRID',form="formatted")
          write(mu,'(f9.4)')size(exmrf),exmrf
          write(mu,'(f9.4)')size(eymrf),eymrf
          close(mu)
       endif
!
!!  deallocate (hycomflxs,xhycom,yhycom,exhycom,eyhycom,hyflxs1,mrfflx,msk_in,xmrf,exmrf,eymrf,workflxs)
!
!     Comparing masks
!
  if (dbgn==3) then
    allocate (imsk_intp(1:nxhycom,1:nyhycom))
    ndiff=0
    do j=1,nyhycom
      do i=1,nxhycom
        if(msk_out(i,j)>=mskfrac) then ; imsk_intp(i,j)=1 ; else ; imsk_intp(i,j)=0 ; endif
        if(imsk_intp(i,j)==0.and.imsk_hycom(i,j)==1) then
          ndiff=ndiff+1
          write (*,'(''msk_out, imsk_intp,imsk_hycom,i,j'',e13.6,f10.3,2i2,2i5)') &
 &          msk_out(i,j),imsk_intp(i,j),imsk_hycom(i,j),i,j
        end if
      end do
    end do 
    write(*,*)'ndiff=',ndiff                       
    write(*,*)'msk_in:max,min=',maxval(msk_in),minval(msk_in)
    write(*,*)'msk_out:max,min=',maxval(msk_out),minval(msk_out)
    write(*,*)'real(imsk_intp):max,min=',maxval(real(imsk_intp)),minval(real(imsk_intp))
    write(*,*)'real(imsk_hycom):max,min=',maxval(real(imsk_hycom)),minval(real(imsk_hycom))
    open (unit=700,file='masks',form="unformatted")
    write(700) real(imsk_intp(:,:)),msk_out(:,:),real(imsk_hycom(:,:))
    close(700)
!
    call dumpi(701,imsk_hycom,nxhycom,nyhycom,'mask_hycom')
    call dumpi(702,int(msk_in),nxmrf,nymrf,'mask_mrf')
    call dumpr(703,msk_out,nxhycom,nyhycom,'mask_out')
    call dumpr(704,exhycom,nxhycom+1,1,'exhycom')
    call dumpr(705,eyhycom,nyhycom+1,1,'eyhycom')
    call dumpr(706,exmrf,nxmrf+1,1,'exmrf')
    call dumpr(707,eymrf,nymrf+1,1,'eymrf')
  endif
stop    
  
end program intp
