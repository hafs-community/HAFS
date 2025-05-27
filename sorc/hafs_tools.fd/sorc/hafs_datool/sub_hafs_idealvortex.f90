!-----------------------------------------------------------------------------
! HAFS DA tool - ideal_vortex
! authors and history:
!      -- 202403, created by Weiguo Wang
!

!========================================================================================
  subroutine hafs_ideal_vortex(in_dir, in_date, nestdoms)

!-----------------------------------------------------------------------------
! This subroutine read hafs restart files, overwrite them with ideal-vortex
! Input variables from restart files (2 domains) include:
!       ak(Time, xaxis_1), bk(Time, xaxis_1): from
!              20230905.120000.fv_core.res.nc, 20230905.120000.fv_core.res.nest02.nc
!       phalf, pfull, grid_xt, grid_yt :  form
!              atmos_static.nc, atmos_static.nest02.tile2.nc
!       grid_x, grid_y, grid_xt, grid_yt, grid_lon, grid_lat, grid_lont, grid_latt: from
!              grid_spec.nc, grid_spec.nest02.tile2.nc(grid_mspec.nest02_2023_09_05_12.tile2.nc)
!       u, v, T, phis, delp, DZ, ua, va: from
!              20230905.120000.fv_core.res.tile1.nc, 20230905.120000.fv_core.res.nest02.tile2.nc
!       sphum: from
!              20230905.120000.fv_tracer.res.tile1.nc, 20230905.120000.fv_srf_wnd.res.nest02.tile2.nc
!
! Variables will be written back to RESTART files
!     q, delp,  deltz,  u, v, Temperature, psurface,  phis,....

  use constants
  use netcdf
  use module_mpi
  use var_type

  implicit none

  character (len=*), intent(in) :: in_dir, in_date
  integer, intent(in)           :: nestdoms
!--- in_dir,  HAFS_restart_folder, which holds grid_spec.nc, fv_core.res.tile1.nc,
!             fv_srf_wnd.res.tile1.nc, fv_tracer.res.tile1.nc, phy_data.nc, sfc_data.nc
!--- in_date, HAFS_restart file date, like 20200825.120000
!--- nestdoms: total nest domain number: 0-no nesting
!---                                     1-nest02.tile2 + 0
!---                                     2-nest03.tile3 + 1

  character (len=2500)   :: indir, infile
  character (len=2500)   :: infile_fvcore, infile_grid, infile_grid2, infile_atmos, infile_core, &
                            infile_tracer, infile_phy, infile_sfc, infile_oro
  type(grid2d_info)      :: ingrid   ! hafs restart grid
  logical  :: file_exist

!----for hafs restart
  integer  :: ix, iy, iz, npz, nd
  character (len=50) :: nestfl, tilefl, tempfl
  real, allocatable, dimension(:,:)     :: grid_lon, grid_lat, grid_lont, grid_latt
  real, allocatable, dimension(:)       :: ak, bk, pfull, phalf
  real                                  :: ptop

!----for work
  real, allocatable, dimension(:,:,:,:) :: dat4
  real, allocatable, dimension(:,:,:)   :: dat3
  real, allocatable, dimension(:,:)     :: dat2
  real, allocatable, dimension(:)       :: dat1

  real, allocatable, dimension(:,:,:,:) :: delp,temp,u,v,q,delz,ua,va
  real, allocatable, dimension(:,:,:,:)   :: psout,phis
  real, allocatable, dimension(:,:)     :: lon,lat,lont,latt


                 integer :: io_proc, is,ie,js,je,isd,ied,jsd,jed, ndims
  integer, dimension(nf90_max_var_dims) :: dims
!------------------------------------------------------------------------------
! 1 --- arg process
  io_proc=0  !nprocs-1

! 1.1 --- input_dir
  if (len_trim(in_dir) < 2 .or. trim(in_dir) == 'w' .or. trim(in_dir) == 'null') then
     indir='.'
  else
     indir=trim(in_dir)
  endif

! 1.2 --- input_files
  if ( nestdoms < 1 ) then  !---outer domain, nestdoms=0
     infile_grid=trim(indir)//'/grid_spec.nc'
     infile_grid2=trim(indir)//'/grid_mspec_'//in_date(1:4)//'_'//in_date(5:6)//'_'//in_date(7:8)//'_'//in_date(10:11)//'.nc'
     infile_atmos=trim(indir)//'/atmos_static.nc'
     infile_oro =trim(indir)//'/oro_data.nc'
     infile_fvcore=trim(indir)//'/'//trim(in_date)//'.fv_core.res.nc'
     infile_core=trim(indir)//'/'//trim(in_date)//'.fv_core.res.tile1.nc'
     infile_tracer=trim(indir)//'/'//trim(in_date)//'.fv_tracer.res.tile1.nc'
     infile_phy =trim(indir)//'/'//trim(in_date)//'.phy_data.nc'
     infile_sfc =trim(indir)//'/'//trim(in_date)//'.sfc_data.nc'
  else
     write(nestfl,'(a4,i2.2)')'nest',nestdoms+1
     write(tilefl,'(a4,i0)')'tile',nestdoms+1
     infile_grid=trim(indir)//'/grid_spec.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
     infile_grid2=trim(indir)//'/grid_mspec.'//trim(nestfl)//'_'//in_date(1:4)//'_'//in_date(5:6)//'_'//in_date(7:8)//'_'//in_date(10:11)//'.'//trim(tilefl)//'.nc'
     infile_oro =trim(indir)//'/oro_data.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
     infile_atmos=trim(indir)//'/atmos_static.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
     infile_fvcore=trim(indir)//'/'//trim(in_date)//'.fv_core.res.'//trim(nestfl)//'.nc'
     infile_core=trim(indir)//'/'//trim(in_date)//'.fv_core.res.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
     infile_tracer=trim(indir)//'/'//trim(in_date)//'.fv_tracer.res.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
     infile_phy =trim(indir)//'/'//trim(in_date)//'.phy_data.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
     infile_sfc =trim(indir)//'/'//trim(in_date)//'.sfc_data.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
  endif

  inquire(file=infile_grid2, exist=file_exist)
  if ( file_exist ) infile_grid = infile_grid2

!------------------------------------------------------------------------------
! 2 --- get data from restart file
! --- get grid information
!         ingrid%(grid_x, grid_y, grid_xt, grid_yt, grid_lon, grid_lat, grid_lont, grid_latt)
  call rd_grid_spec_data(trim(infile_grid), ingrid)
  ix=ingrid%grid_xt
  iy=ingrid%grid_yt
  allocate(lont(ix,iy)); lont=ingrid%grid_lont
  allocate(latt(ix,iy)); latt=ingrid%grid_latt
  allocate(lon(ix+1,iy+1)); lon=ingrid%grid_lon
  allocate(lat(ix+1,iy+1)); lat=ingrid%grid_lat


! --- iz, pfull, phalf
  call get_var_dim(trim(infile_atmos), 'pfull', ndims, dims)
  iz=dims(1)
  if ( .not. allocated(pfull)) allocate(pfull(iz))
  if ( .not. allocated(phalf)) allocate(phalf(iz+1))
  call get_var_data(trim(infile_atmos), 'pfull', iz, 1, 1, 1, pfull)
  call get_var_data(trim(infile_atmos), 'phalf', iz+1, 1, 1, 1, phalf)
  ptop = phalf(1)*100.0      !! mb to pa

! --- get ak, bk
  if ( .not. allocated(ak) ) allocate(ak(iz+1))
  call get_var_data(trim(infile_fvcore), 'ak', iz+1, 1, 1, 1, ak)
  if ( .not. allocated(bk) ) allocate(bk(iz+1))
  call get_var_data(trim(infile_fvcore), 'bk', iz+1, 1, 1, 1, bk)

! --- get T, u, v, ua, va
!  if ( .not. allocated(t) ) allocate(t(ix, iy, iz, 1))
!  call get_var_data(trim(infile_core), 'T', ix, iy, iz,1, t)
!  if ( .not. allocated(u) ) allocate(u(ix, iy+1, iz,1))
!  call get_var_data(trim(infile_core), 'u', ix, iy+1, iz, 1, u)
!  if ( .not. allocated(u) ) allocate(v(ix+1, iy, iz,1))
!  call get_var_data(trim(infile_core), 'v', ix+1, iy, iz, 1, v)


 if ( .not. allocated(delp) ) allocate(delp(ix, iy, iz, 1))
 if ( .not. allocated(temp) ) allocate(temp(ix, iy, iz, 1))
 if ( .not. allocated(delz) ) allocate(delz(ix, iy, iz, 1))
 if ( .not. allocated(u) ) allocate(u(ix, iy+1, iz, 1))
 if ( .not. allocated(v) ) allocate(v(ix+1, iy, iz, 1))
 if ( .not. allocated(q) ) allocate(q(ix, iy, iz, 1))
 if ( .not. allocated(psout) ) allocate(psout(ix, iy, 1,1))
 if ( .not. allocated(phis) ) allocate(phis(ix, iy, 1,1))


!------------------------------------------------------------------------------
! 3 --- ideal calculation
     is=1    ; ie=ix
     js=1    ;  je=iy
     isd=1   ;  ied=ix
     jsd=1   ; jed=iy
     npz=iz
   CALL DCMIP16_ideal_TC(delp,temp,u,v,q,delz,phis,psout,&           ! out
        is,ie,js,je,isd,ied,jsd,jed,npz,ak,bk,ptop, &             ! IN   ptop unit: pa
        lon,lat,lont,latt)                                        ! IN  degree


!! check, compare with dz,delp in old files
    allocate(dat4(ix, iy, iz, 1))
     call get_var_data(trim(infile_core), 'DZ', ix, iy, iz,1, dat4)
    write(0,*)'dz=',delz(300,300,:,1)
    write(0,*)'dzold=',dat4(300,300,:,1)

    write(0,*)'-------------------------------'
     call get_var_data(trim(infile_core), 'delp', ix, iy, iz,1, dat4)
    write(0,*)'delp=',delp(300,300,:,1)
    write(0,*)'delpold=',dat4(300,300,:,1)

  deallocate(dat4)
    allocate(dat4(ix, iy, 1,1))
     call get_var_data(trim(infile_core), 'phis', ix, iy, 1,1, dat4)
    write(0,*)'old min,max phis=',minval(dat4),maxval(dat4)
    write(0,*)'min,max phis=',minval(phis),maxval(phis)
  deallocate(dat4)

    allocate(dat4(ix, iy, 1,1))
     call get_var_data(trim(infile_sfc), 'tsea', ix, iy, 1,1, dat4)
    write(0,*)'old min,max tsea=',minval(dat4),maxval(dat4)

!    stop
!------------------------------------------------------------------------------
! 4 --- output

! 4.1 update 2-D varibales in sfc files if needed
  call write_nc_real(trim(infile_sfc), 'tsea', ix, iy, 1, 1, 'xaxis_1', 'yaxis_1', '-', 'Time', phis*0+302.15, '-', '-')

  !call update_hafs_restart(trim(infile_sfc), 'tsea', ix, iy, 1, 1, phis*0+302.15)

  call write_nc_real(trim(infile_tracer), 'sphum', ix, iy, iz, 1, 'xaxis_1', 'yaxis_2', 'zaxis_1', 'Time', q, '-', '-')
  deallocate(q)

  call update_hafs_restart(trim(infile_core), 'phis', ix, iy, 1, 1, phis)

  ! --- T
  call write_nc_real(trim(infile_core), 'T', ix, iy, iz, 1, 'xaxis_1', 'yaxis_2', 'zaxis_1', 'Time', temp, '-', '-')
  deallocate(temp)

  call write_nc_real(trim(infile_core), 'DZ', ix, iy, iz, 1, 'xaxis_1', 'yaxis_2', 'zaxis_1', 'Time', delz, '-', '-')
  deallocate(delz)

  call write_nc_real(trim(infile_core), 'delp', ix, iy, iz, 1, 'xaxis_1', 'yaxis_2', 'zaxis_1', 'Time', delp, '-', '-')
  deallocate(delp)

 if ( .not. allocated(ua) ) allocate(ua(ix, iy, iz, 1))
   ua(:,:,:,1)=0.5*(u(:,1:iy,:,1)+u(:,2:iy+1,:,1))
  call write_nc_real(trim(infile_core), 'ua', ix, iy, iz, 1, 'xaxis_1', 'yaxis_2', 'zaxis_1', 'Time', ua, '-', '-')
   deallocate(ua)
  call write_nc_real(trim(infile_core), 'u', ix, iy+1, iz, 1, 'xaxis_1', 'yaxis_1', 'zaxis_1', 'Time', u, '-', '-')
  deallocate(u)

 if ( .not. allocated(va) ) allocate(va(ix, iy, iz, 1))
   va(:,:,:,1)=0.5*(v(1:ix,:,:,1)+v(2:ix+1,:,:,1))
  call write_nc_real(trim(infile_core), 'va', ix, iy, iz, 1, 'xaxis_1', 'yaxis_2', 'zaxis_1', 'Time', va, '-', '-')
   deallocate(va)
  call write_nc_real(trim(infile_core), 'v', ix+1, iy, iz, 1, 'xaxis_2', 'yaxis_2', 'zaxis_1', 'Time', v, '-', '-')
  deallocate(v)


  end subroutine hafs_ideal_vortex

!-------------------------------------
!========================================================================================
  subroutine hafs_ideal_sfc_data(in_file)

!-----------------------------------------------------------------------------
! This subroutine rewrites some varibales with given values for idealized HAFS
! Input filenames and locations
!    e.g., 2019082400/88L/intercom/chgres/sfc_data.tile7.nc
!                                         sfc_data.tile8.nc
! Variables will be written back to sfc_files
!      e.g. soil, veg, ..... tsea....


  use constants
  use netcdf
  use module_mpi
  use var_type

  implicit none

  character (len=*), intent(in) :: in_file
!--- in_file, sfc_data.tile7.nc  (domain 1) or sfc_data.tile8.nc (domain 2)

  type(grid2d_info)      :: ingrid   ! hafs restart grid
  logical  :: file_exist

!----for hafs sfc_data???.nc
  integer  :: ix, iy, iz, it, n
  character (len=50) :: nestfl, tilefl, tempfl
  integer :: ncid, varid, ndims, nvars, xtype, dimids(5), vdim(5)
  character(len=50) :: varname, dimname
!----for work
  real, allocatable, dimension(:,:,:,:) :: dat4
  real, allocatable, dimension(:,:,:)   :: dat3
  real, allocatable, dimension(:,:)     :: dat2
  real, allocatable, dimension(:)       :: dat1

  integer :: io_proc,i
  integer, dimension(nf90_max_var_dims) :: dims
!------------------------------------------------------------------------------
! 1 --- arg process
  io_proc=0  !nprocs-1

! 1.1 --- input_files
  inquire(file=in_file, exist=file_exist)
  if ( .not. file_exist )  then
      write(0,*)'file does not exist, stop'
      return
  endif

!------------------------------------------------------------------------------
! 2 --- get dimensions
  call nccheck(nf90_open(trim(in_file), nf90_nowrite, ncid), 'wrong in open: '//trim(in_file), .true.)
  call nccheck(nf90_inquire(ncid, ndims, nvars), 'wrong in inquire ncid', .true.)
   do n = 1, ndims
      call nccheck(nf90_inquire_dimension(ncid,n,name=dimname, len=i), 'wrong in inquire_dimension', .true.)
      select case (trim(dimname))
             case ('xaxis_1'); ix = i
             case ('yaxis_1'); iy = i
             case ('zaxis_1'); iz = i
             case ('Time') ;   it  = i
      end select
   enddo
   call nccheck(nf90_close(ncid), 'wrong in close '//trim(in_file), .true.)
   write(0,*)'ix,iy,iz,it=',ix,iy,iz,it
! 3  --- allocate a varibale, give a value, then write back
!        Most are (ix,iy,it), Only stc, smc, slc are (ix,iy,iz,it)
  allocate(dat4(ix, iy, 1, 1))
! Tsea ! set tg3=tisfc=stc=tsea (check a run without any changes, they are same)
  call update_hafs_restart(trim(in_file), 'tsea', ix, iy, 1, 1,  dat4*0+302.15)
  call update_hafs_restart(trim(in_file), 't2m', ix, iy, 1, 1,   dat4*0+300.6)
  call update_hafs_restart(trim(in_file), 'q2m', ix, iy, 1, 1,   dat4*0+0.0176)
  call update_hafs_restart(trim(in_file), 'tisfc', ix, iy, 1, 1, dat4*0+302.15)
  call update_hafs_restart(trim(in_file), 'tg3', ix, iy, 1, 1,   dat4*0+302.15)
  call update_hafs_restart(trim(in_file), 'zorl', ix, iy, 1, 1,  dat4*0+0.007)
  call update_hafs_restart(trim(in_file), 'uustar', ix, iy, 1, 1,  dat4*0+0.2)
  deallocate(dat4)
  allocate(dat4(ix, iy, iz, 1))
  dat4(:,:,:,:)=302.15
  call update_hafs_restart(trim(in_file), 'stc', ix, iy, iz, 1,  dat4)
  deallocate(dat4)

  end subroutine hafs_ideal_sfc_data

!-------------------------------------
!========================================================================================
 subroutine DCMIP16_ideal_TC(delp,pt,u,v,q,delz,phis,psout,&           ! out
      is,ie,js,je,isd,ied,jsd,jed,npz,ak,bk,ptop, &             ! IN  ptop in pa
      grid_lon,grid_lat,agrid_lon,agrid_lat)                   ! IN, degree


!! LOG

 !subroutine DCMIP16_TC(delp,pt,u,v,q,w,delz,&
 !     is,ie,js,je,isd,ied,jsd,jed,npz,nq,ak,bk,ptop, &
 !     pk,peln,pe,pkz,gz,phis,ps,grid,agrid, &
 !     hydrostatic, nwat, adiabatic)
   implicit none
   integer, parameter :: R_GRID = 8
   real(kind=R_GRID), parameter   :: rdgas = 287.04, grav = 9.80
   real(kind=R_GRID),parameter::     rvgas = 4.6150e+2                   !< gas constant H2O (\f$J/kg/K\f$)
   real(kind=R_GRID),parameter::     omega  =7.2921e-5                 !< ang vel of earth (\f$s^{-1}\f$)
   real(kind=R_GRID),parameter::      pi     =4.0d0*atan(1.0d0)         !< pi
   real(kind=R_GRID) ,parameter:: radius  =6.3712e+6                 !< radius of earth (\f$m\f$)
   real(kind=R_GRID),parameter :: kappa = 2./7.                      !< Rdgas/CP
   real(kind=R_GRID), parameter :: cp     = RDGAS/KAPPA  !  spec heat cap of dry air (J/kg/deg)

   integer, intent(IN) :: is,ie,js,je,isd,ied,jsd,jed,npz
   real, intent(IN) :: ptop
   real, intent(IN), dimension(npz+1) :: ak, bk
   real, intent(INOUT), dimension(isd:ied,jsd:jed,npz) :: q
   real, intent(OUT), dimension(isd:ied,jsd:jed,npz) :: delp, pt
   real, intent(OUT), dimension(is:ie,js:je,1:npz) :: delz
   real, intent(OUT), dimension(isd:ied,jsd:jed+1,npz) :: u
   real, intent(OUT), dimension(isd:ied+1,jsd:jed,npz) :: v
   real, intent(OUT), dimension(isd:ied,jsd:jed) :: phis,psout
   real, intent(IN), dimension(isd:ied,jsd:jed) :: agrid_lon, agrid_lat
   real, intent(IN), dimension(isd:ied+1,jsd:jed+1) :: grid_lon,grid_lat

   real, parameter :: zt = 15000 !< m
   real, parameter :: q0 = 0.021 !< kg/kg
   real, parameter :: qt = 1.e-11 !< kg/kg
   real, parameter :: T0 = 302.15 !< K
   real, parameter :: Tv0 = 302.15*(1.+0.608*q0) !< K
   real, parameter :: Ts = 302.15 !< K
   real, parameter :: zq1 = 3000. !< m
   real, parameter :: zq2 = 8000. !< m
   real, parameter :: lapse = 7.e-3 !< K/m
   real, parameter :: Tvt = Tv0 - lapse*zt !< K
   real, parameter :: pb = 101500. !< Pa
   real, parameter :: ptt = pb*(TvT/Tv0)**(grav/Rdgas/lapse)
   real, parameter :: lamp = pi
   real, parameter :: phip = pi/18.
   real(kind=R_GRID), parameter :: ppcenter(2) = (/ lamp, phip /)
   real, parameter :: dp = 1115. !< Pa
   real, parameter :: rp = 282000. !< m
   real, parameter :: zp = 7000. !< m
   real(kind=R_GRID), parameter :: fc = 2.*OMEGA*sin(phip)

   real, parameter :: zconv = 1.e-6
   real, parameter :: rdgrav = rdgas/grav
   real, parameter :: rrdgrav = grav/rdgas
   real, parameter :: zvir = rvgas/rdgas - 1.

!! local work varibales
   real, dimension(is:ie,js:je,npz+1) :: pk
   real, dimension(is:ie,npz+1,js:je) :: peln
   real(kind=R_GRID), dimension(is-1:ie+1,npz+1,js-1:je+1) :: pe
   real, dimension(is:ie,js:je,npz) :: pkz
   real(kind=R_GRID), dimension(isd:ied,jsd:jed,2) :: agrid
   real(kind=R_GRID), dimension(isd:ied+1,jsd:jed+1,2) :: grid
   real(kind=R_GRID), dimension(isd:ied,jsd:jed,npz+1) :: gz

   integer :: i,j,k,iter, sphum, cl, cl2, n, iternum
!   real :: p,z,z0,ziter,piter,titer,uu,vv,pl, r
   real :: p,uu,vv,pl, r
   real(kind=R_GRID)ziter,piter,titer,z,z0
   real(kind=R_GRID), dimension(2) :: pa
   real(kind=R_GRID), dimension(3) :: e1,e2,ex,ey
   real(kind=R_GRID), dimension(is:ie,js:je)   :: rc,ps
   real(kind=R_GRID), dimension(is:ie,js:je+1) :: gz_u,p_u,peln_u,ps_u,u1,u2, rc_u
   real(kind=R_GRID), dimension(is:ie,js:je+1) :: lat_u,lon_u
   real(kind=R_GRID), dimension(is:ie+1,js:je) :: gz_v,p_v,peln_v,ps_v,v1,v2, rc_v
   real(kind=R_GRID), dimension(is:ie+1,js:je) :: lat_v,lon_v
   logical  :: hydrostatic,adiabatic


       write(0,*)pa
       write(0,*)agrid(1,1,:)
   agrid(:,:,1)=agrid_lon(:,:)*pi/180.
   agrid(:,:,2)=agrid_lat(:,:)*pi/180.
       write(0,*)agrid(1,1,:)
       write(0,*)agrid_lon(1,1),agrid_lat(1,1)
   grid(:,:,1)=grid_lon(:,:)*pi/180.
   grid(:,:,2)=grid_lat(:,:)*pi/180.

     write(0,*)grid(1,1,:)
     write(0,*)grid(1,2,:)

     write(0,*)grid_lon(1,1),grid_lat(1,1)
     write(0,*)grid_lon(1,2),grid_lat(1,2)


   hydrostatic=.False.
   adiabatic=.False.

   !Compute ps, phis, delp, aux pressure variables, Temperature, winds
   ! (with or without perturbation), moisture, w, delz

   !Compute p, z, T on both the staggered and unstaggered grids. Then compute the zonal
   !  and meridional winds on both grids, and rotate as needed

   !Save r for easy use
   do j=js,je
   do i=is,ie
      rc(i,j) = great_circle_dist(agrid(i,j,:), ppcenter, radius)
   enddo
   enddo

   !PS
   do j=js,je
   do i=is,ie
      ps(i,j) = 1.0d0*pb - 1.0d0*dp*exp( -sqrt((1.0d0*rc(i,j)/rp)**3) )
      psout(i,j)=ps(i,j)
   enddo
   enddo

     write(0,*)'ak=',ak
     write(0,*)'bk=',bk
     write(0,*)'ptop=',ptop
   !delp
   do k=1,npz
   do j=js,je
   do i=is,ie
      delp(i,j,k) = ak(k+1)-ak(k) + ps(i,j)*(bk(k+1)-bk(k))
   enddo
   enddo
   enddo

   !Pressure variables
   do j=js,je
   do i=is,ie
      pe(i,1,j)   = ptop
   enddo
   do i=is,ie
      peln(i,1,j) = log(ptop)
      pk(i,j,1) = ptop**kappa
   enddo
   do k=2,npz+1
   do i=is,ie
      pe(i,k,j)   = 1.0d0*ak(k) + ps(i,j)*bk(k)
   enddo
   do i=is,ie
      pk(i,j,k) = exp(kappa*log(pe(i,k,j)))
      peln(i,k,j) = log(pe(i,k,j))
   enddo
   enddo
   enddo

   do k=1,npz
   do j=js,je
   do i=is,ie
      pkz(i,j,k) = (pk(i,j,k+1)-pk(i,j,k))/(kappa*(peln(i,k+1,j)-peln(i,k,j)))
   enddo
   enddo
   enddo

    print*,'Start compute Z'


   !Height: Use Newton's method
   !Cell centered
   do j=js,je
   do i=is,ie
      phis(i,j) = 0.
      gz(i,j,npz+1) = 0.
   enddo
   enddo
   do k=npz,1,-1
   do j=js,je
   do i=is,ie
      p = pe(i,k,j)
      z = gz(i,j,k+1)
      do iter=1,30
         ziter = z
         piter = DCMIP16_TC_pressure(ziter,1.0d0*rc(i,j))
         titer = DCMIP16_TC_temperature(ziter,1.0d0*rc(i,j))
         z = ziter + (piter - p)*rdgrav*titer/piter
           if (i ==300 .and. j == 300 .and. (k == npz .or. k == 1) ) then
             write(0,*)iter, ziter,z
           endif
         if (abs(z - ziter) < zconv) exit
      enddo
      gz(i,j,k) = z
   enddo
   enddo
   enddo

    print*,'Done compute Z'
    print*,'z(300,300,:)=',gz(300,300,:)

   !Temperature: Compute from hydro balance
   do k=1,npz
   do j=js,je
   do i=is,ie
      pt(i,j,k) = rrdgrav * ( gz(i,j,k) - gz(i,j,k+1) ) / ( peln(i,k+1,j) - peln(i,k,j))
   enddo
   enddo
   enddo
    print*,'pt(300,300,:)=',pt(300,300,:)

   !Compute height and temperature for u and v points also, to be able to compute the local winds
   !Use temporary 2d arrays for this purpose
   do j=js,je+1
   do i=is,ie
      call mid_pt_sphere(grid(i,j,:),grid(i+1,j,:),pa)
      lat_u(i,j) = pa(2)
      lon_u(i,j) = pa(1)

      call get_unit_vect2(grid(i,j,:),grid(i+1,j,:),e1)
      call get_latlon_vector(pa,ex,ey)
      u1(i,j) = inner_prod(e1,ex) !u components
      u2(i,j) = inner_prod(e1,ey)
      rc_u(i,j) = great_circle_dist(pa, ppcenter, radius)
      gz_u(i,j) = 0.
      p_u(i,j) = pb - dp*exp( -sqrt((rc_u(i,j)/rp)**3) )
      peln_u(i,j) = log(p_u(i,j))
      ps_u(i,j) = p_u(i,j)
          if (i == 1 .and. j == 1) then
           write(0,*)'Mid_pt u point lon, lat'
           write(0,*)lon_u(i,j),lat_u(i,j)
           write(0,*)grid(i,j,:)
           write(0,*)grid(i+1,j,:)
           write(0,*)'rc_u(i,j)=',rc_u(i,j)
          endif
   enddo
   enddo
   do k=npz,1,-1
   do j=js,je+1
   do i=is,ie
      !Pressure (Top of interface)
      p = ak(k) + ps_u(i,j)*bk(k)
      pl = log(p)
      !Height (top of interface); use newton's method
      z = gz_u(i,j) !first guess, height of lower level
      z0 = z
      do iter=1,30
         ziter = z
         piter = DCMIP16_TC_pressure(ziter,1.0d0*rc_u(i,j))
         titer = DCMIP16_TC_temperature(ziter,1.0d0*rc_u(i,j))
         z = ziter + (piter - p)*rdgrav*titer/piter
         if (abs(z - ziter) < zconv) exit
      enddo
      !Now compute winds
      call DCMIP16_TC_uwind_pert(0.5*(z+z0),1.0d0*rc_u(i,j),lon_u(i,j),lat_u(i,j), uu, vv)
      u(i,j,k) = u1(i,j)*uu + u2(i,j)*vv

      gz_u(i,j) = z
      p_u(i,j) = p
      peln_u(i,j) = pl
   enddo
   enddo
   enddo
    print*,'u(300,300,:)=',u(300,300,:)

   do j=js,je
   do i=is,ie+1
      call mid_pt_sphere(grid(i,j,:),grid(i,j+1,:),pa)
      lat_v(i,j) = pa(2)
      lon_v(i,j) = pa(1)
      call get_unit_vect2(grid(i,j,:),grid(i,j+1,:),e2)
      call get_latlon_vector(pa,ex,ey)
      v1(i,j) = inner_prod(e2,ex) !v components
      v2(i,j) = inner_prod(e2,ey)
      rc_v(i,j) = great_circle_dist(pa, ppcenter, radius)
      gz_v(i,j) = 0.
      p_v(i,j) = pb - dp*exp( - sqrt((rc_v(i,j)/rp)**3) )
      peln_v(i,j) = log(p_v(i,j))
      ps_v(i,j) = p_v(i,j)
   enddo
   enddo
   do k=npz,1,-1
   do j=js,je
   do i=is,ie+1
      !Pressure (Top of interface)
      p = ak(k) + ps_v(i,j)*bk(k)
      pl = log(p)
      !Height (top of interface); use newton's method
      z = gz_v(i,j) !first guess, height of lower level
      z0 = z
      do iter=1,30
         ziter = z
         piter = DCMIP16_TC_pressure(ziter,1.0d0*rc_v(i,j))
         titer = DCMIP16_TC_temperature(ziter,1.0d0*rc_v(i,j))
         z = ziter + (piter - p)*rdgrav*titer/piter
         if (abs(z - ziter) < zconv) exit
      enddo
      !Now compute winds
      call DCMIP16_TC_uwind_pert(0.5*(z+z0),1.0d0*rc_v(i,j),lon_v(i,j),lat_v(i,j), uu, vv)
      v(i,j,k) = v1(i,j)*uu + v2(i,j)*vv
      gz_v(i,j) = z
      p_v(i,j) = p
      peln_v(i,j) = pl
   enddo
   enddo
   enddo
    print*,'v(300,300,:)=',v(300,300,:)

   !Compute moisture and other tracer fields, as desired
   do n=1,1
   do k=1,npz
   do j=jsd,jed
   do i=isd,ied
      !!q(i,j,k,n) = 0.
      q(i,j,k) = 0.
   enddo
   enddo
   enddo
   enddo
   if (.not. adiabatic) then
!!      sphum = get_tracer_index (MODEL_ATMOS, 'sphum')
      sphum = 1
      do k=1,npz
      do j=js,je
      do i=is,ie
         z = 0.5*(gz(i,j,k) + gz(i,j,k+1))
        ! q(i,j,k,sphum) = DCMIP16_TC_sphum(z)
         q(i,j,k) = DCMIP16_TC_sphum(1.0d0*z)
         !Convert pt to non-virtual temperature
       !  pt(i,j,k) = pt(i,j,k) / ( 1. + zvir*q(i,j,k,sphum))
         pt(i,j,k) = DCMIP16_TC_temperature(1.0d0*z,1.0d0*rc(i,j))
         pt(i,j,k) = pt(i,j,k) / ( 1. + zvir*q(i,j,k))
      enddo
      enddo
      enddo
   endif

   !Compute nonhydrostatic variables, if needed
   if (.not. hydrostatic) then
      do k=1,npz
      do j=js,je
      do i=is,ie
!!         w(i,j,k) = 0.
!!         delz(i,j,k) = gz(i,j,k) - gz(i,j,k+1)
         delz(i,j,k) = -gz(i,j,k) + gz(i,j,k+1)
      enddo
      enddo
      enddo
   endif

    write(0,*)'u(:,300,npz)=',u(:,300,npz)
    write(0,*)'v(:,300,npz)=',v(:,300,npz)

 contains

   !Initialize with virtual temperature
   real(kind=8) function DCMIP16_TC_temperature(z, r)

     real(kind=8), intent(IN) :: z, r
     real :: Tv, term1, term2

     if (z > zt) then
        DCMIP16_TC_temperature = Tvt
        return
     endif

     Tv = Tv0 - lapse*z
     term1 = grav*zp*zp* ( 1. - pb/dp * exp( sqrt(r/rp)**3 + (z/zp)**2 ) )
     term2 = 2.0d0*rdgas*Tv*z
     DCMIP16_TC_temperature = Tv + Tv*( 1./(1 + term2/term1) - 1.)

   end function DCMIP16_TC_temperature

   !Initialize with moist air mass
   real(kind=8) function DCMIP16_TC_pressure(z, r)

     real(kind=8), intent(IN) :: z, r

     if (z <= zt) then
        DCMIP16_TC_pressure = 1.0d0*pb*exp(grav/(Rdgas*lapse) * log( (Tv0-lapse*z)/Tv0) ) -dp* exp(-sqrt((r/rp)**3) - (z/zp)**2) * &
             exp( grav/(Rdgas*lapse) * log( (Tv0-lapse*z)/Tv0) )
     else
        DCMIP16_TC_pressure = ptt*exp(grav*(zt-z)/(Rdgas*Tvt))
     endif

   end function DCMIP16_TC_pressure

   subroutine DCMIP16_TC_uwind_pert(z,r,lon,lat,uu,vv)

     real(kind=R_GRID), intent(IN) :: z, r
     real(kind=R_GRID), intent(IN) :: lon, lat
     real, intent(OUT) :: uu, vv
     real :: rfac, Tvrd, vt, fr5, d1, d2, d
     real(kind=R_GRID) :: dst, pphere(2)

     if (z > zt) then
        uu = 0.
        vv = 0.
        return
     endif

     rfac = sqrt(r/rp)**3

     fr5 = 0.5*fc*r
     Tvrd = (Tv0 - lapse*z)*Rdgas

     vt = -fr5 + sqrt( fr5**2 - (1.5 * rfac * Tvrd) / &
          ( 1. + 2*Tvrd*z/(grav*zp**2) - pb/dp*exp( rfac + (z/zp)**2) ) )

     d1 = sin(phip)*cos(lat) - cos(phip)*sin(lat)*cos(lon - lamp)
     d2 = cos(phip)*sin(lon - lamp)
     d = max(1.e-25,sqrt(d1*d1 + d2*d2))

     uu = vt * d1/d
     vv = vt * d2/d

   end subroutine DCMIP16_TC_uwind_pert

   real(kind=8) function DCMIP16_TC_sphum(z)

     real(kind=8), intent(IN) :: z

     DCMIP16_TC_sphum = qt
     if (z < zt) then
        DCMIP16_TC_sphum = q0 * exp(-z/zq1) * exp(-(z/zq2 )**2)
     endif

   end function DCMIP16_TC_sphum

 subroutine cart_to_latlon(np, q, xs, ys)
! vector version of cart_to_latlon1
  integer, intent(in):: np
  real(kind=8), intent(inout):: q(3,np)
  real(kind=8), intent(inout):: xs(np), ys(np)
! local
  real(kind=8), parameter:: esl=1.d-10
  real (kind=8):: p(3)
  real (kind=8):: dist, lat, lon
  integer i,k

  do i=1,np
     do k=1,3
        p(k) = q(k,i)
     enddo
     dist = sqrt(p(1)**2 + p(2)**2 + p(3)**2)
     do k=1,3
        p(k) = p(k) / dist
     enddo

     if ( (abs(p(1))+abs(p(2)))  < esl ) then
          lon = real(0.,kind=8)
     else
          lon = atan2( p(2), p(1) )   ! range [-pi,pi]
     endif

     if ( lon < 0.) lon = real(2.,kind=8)*pi + lon
! RIGHT_HAND system:
     lat = asin(p(3))

     xs(i) = lon
     ys(i) = lat
! q Normalized:
     do k=1,3
        q(k,i) = p(k)
     enddo
  enddo

 end  subroutine cart_to_latlon
 subroutine get_latlon_vector(pp, elon, elat)
 real(kind=8), intent(IN)  :: pp(2)
 real(kind=8), intent(OUT) :: elon(3), elat(3)

         elon(1) = -SIN(pp(1))
         elon(2) =  COS(pp(1))
         elon(3) =  0.0
         elat(1) = -SIN(pp(2))*COS(pp(1))
         elat(2) = -SIN(pp(2))*SIN(pp(1))
!!! RIGHT_HAND
         elat(3) =  COS(pp(2))
! Left-hand system needed to be consistent with rest of the codes
!        elat(3) = -COS(pp(2))

 end subroutine get_latlon_vector
 subroutine get_unit_vect2( e1, e2, uc )
   real(kind=8), intent(in) :: e1(2), e2(2)
   real(kind=8), intent(out):: uc(3) !< unit vector e1--->e2
! Local:
   real(kind=8), dimension(3):: pc, p1, p2, p3

! RIGHT_HAND system:
   call latlon2xyz(e1, p1)
   call latlon2xyz(e2, p2)

   call mid_pt3_cart(p1, p2,  pc)
   call vect_cross(p3, p2, p1)
   call vect_cross(uc, pc, p3)
   call normalize_vect( uc )

 end subroutine get_unit_vect2
 real*8 function great_circle_dist( q1, q2, radius )
      real(kind=8), intent(IN)           :: q1(2), q2(2)
      real(kind=8), intent(IN), optional :: radius

      real (kind=8):: p1(2), p2(2)
      real (kind=8):: beta
      integer n

      do n=1,2
         p1(n) = q1(n)
         p2(n) = q2(n)
      enddo

      beta = asin( sqrt( sin((p1(2)-p2(2))/2.)**2 + cos(p1(2))*cos(p2(2))*   &
                         sin((p1(1)-p2(1))/2.)**2 ) ) * 2.

      if ( present(radius) ) then
           great_circle_dist = radius * beta
      else
           great_circle_dist = beta   ! Returns the angle
      endif

  end function great_circle_dist
  real function inner_prod(v1, v2)
       real(kind=8),intent(in):: v1(3), v2(3)
       real (kind=8) :: vp1(3), vp2(3), prod16
       integer k

         do k=1,3
            vp1(k) = real(v1(k),kind=8)
            vp2(k) = real(v2(k),kind=8)
         enddo
         prod16 = vp1(1)*vp2(1) + vp1(2)*vp2(2) + vp1(3)*vp2(3)
         inner_prod = prod16

  end function inner_prod
 subroutine latlon2xyz(p, e, id)

 real(kind=8), intent(in) :: p(2)
 real(kind=8), intent(out):: e(3)
 integer, optional, intent(in):: id !< id=0 do nothing; id=1, right_hand

 integer n
 real (kind=8):: q(2)
 real (kind=8):: e1, e2, e3

 logical, save       :: first_time = .true.
 integer, save       :: id_latlon

    do n=1,2
       q(n) = p(n)
    enddo

    e1 = cos(q(2)) * cos(q(1))
    e2 = cos(q(2)) * sin(q(1))
    e3 = sin(q(2))
!-----------------------------------
! Truncate to the desired precision:
!-----------------------------------
    e(1) = e1
    e(2) = e2
    e(3) = e3

 end subroutine latlon2xyz
 subroutine mid_pt_sphere(p1, p2, pm)
      real(kind=8) , intent(IN)  :: p1(2), p2(2)
      real(kind=8) , intent(OUT) :: pm(2)
!------------------------------------------
      real(kind=8) e1(3), e2(3), e3(3)

      call latlon2xyz(p1, e1)
      call latlon2xyz(p2, e2)
      call mid_pt3_cart(e1, e2, e3)
      call cart_to_latlon(1, e3, pm(1), pm(2))

 end subroutine mid_pt_sphere



 subroutine mid_pt3_cart(p1, p2, e)
       real(kind=8), intent(IN)  :: p1(3), p2(3)
       real(kind=8), intent(OUT) :: e(3)
!
       real (kind=8):: q1(3), q2(3)
       real (kind=8):: dd, e1, e2, e3
       integer k

       do k=1,3
          q1(k) = p1(k)
          q2(k) = p2(k)
       enddo

       e1 = q1(1) + q2(1)
       e2 = q1(2) + q2(2)
       e3 = q1(3) + q2(3)

       dd = sqrt( e1**2 + e2**2 + e3**2 )
       e1 = e1 / dd
       e2 = e2 / dd
       e3 = e3 / dd

       e(1) = e1
       e(2) = e2
       e(3) = e3

 end subroutine mid_pt3_cart



 subroutine mid_pt_cart(p1, p2, e3)
    real(kind=8), intent(IN)  :: p1(2), p2(2)
    real(kind=8), intent(OUT) :: e3(3)
!-------------------------------------
    real(kind=8) e1(3), e2(3)

    call latlon2xyz(p1, e1)
    call latlon2xyz(p2, e2)
    call mid_pt3_cart(e1, e2, e3)

 end subroutine mid_pt_cart
!>@brief The subroutine 'normalize_vect' makes 'e' a unit vector.
 subroutine normalize_vect(e)

 real(kind=8), intent(inout):: e(3)
 real(kind=8):: pdot
 integer k

    pdot = e(1)**2 + e(2)**2 + e(3)**2
    pdot = sqrt( pdot )

    do k=1,3
       e(k) = e(k) / pdot
    enddo

 end subroutine normalize_vect
!>@brief The subroutine 'vect_cross performs cross products
!! of 3D vectors: e = P1 X P2
 subroutine vect_cross(e, p1, p2)
 real(kind=8), intent(in) :: p1(3), p2(3)
 real(kind=8), intent(out):: e(3)

      e(1) = p1(2)*p2(3) - p1(3)*p2(2)
      e(2) = p1(3)*p2(1) - p1(1)*p2(3)
      e(3) = p1(1)*p2(2) - p1(2)*p2(1)

 end subroutine vect_cross
 end subroutine DCMIP16_ideal_TC
