!========================================================================================
  subroutine hafsvi_preproc(in_dir, in_date, radius, res, out_file)

!-----------------------------------------------------------------------------
! HAFS DA tool - hafsvi_preproc
! Yonghui Weng, 20211210
!
! This subroutine read hafs restart files and output hafsvi needed input.
! Variables needed:
!      WRITE(IUNIT) NX,NY,NZ
!      WRITE(IUNIT) lon1,lat1,lon2,lat2,cen_lon,cen_lat
!      WRITE(IUNIT) (((pf1(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)                   ! 3D, NZ
!      WRITE(IUNIT) (((tmp(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
!      WRITE(IUNIT) (((spfh(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
!      WRITE(IUNIT) (((ugrd(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
!      WRITE(IUNIT) (((vgrd(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
!      WRITE(IUNIT) (((dzdt(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
!!      WRITE(IUNIT) hgtsfc                ! 2D
!      WRITE(IUNIT) (((z1(i,j,k),i=1,nx),j=1,ny),k=nz1,1,-1)
!      WRITE(IUNIT) glon,glat,glon,glat   ! 2D
!      WRITE(IUNIT) (((ph1(i,j,k),i=1,nx),j=1,ny),k=nz1,1,-1)                ! 3D, NZ+1
!      WRITE(IUNIT) pressfc1              ! 2D
!      WRITE(IUNIT) ak
!      WRITE(IUNIT) bk
!      WRITE(IUNIT) land                  ! =A101 = land sea mask, B101 = ZNT
!      WRITE(IUNIT) sfcr                  ! =B101 = Z0
!      WRITE(IUNIT) C101                  ! =C101 = (10m wind speed)/(level 1 wind speed)
!

!-----------------------------------------------------------------------------

  use netcdf
  use module_mpi
  use var_type

  implicit none

  character (len=*), intent(in) :: in_dir, in_date, radius, res, out_file
!--- in_dir,  HAFS_restart_folder, which holds grid_spec.nc, fv_core.res.tile1.nc, 
!             fv_srf_wnd.res.tile1.nc, fv_tracer.res.tile1.nc, phy_data.nc, sfc_data.nc
!--- in_date, HAFS_restart file date, like 20200825.120000
!--- radius,  to cut a square, default value is 40, which means a 40deg x 40deg square.
!--- out_file: output file, default is bin format, if the file name is *.nc, then output nc format.

  character (len=2500)          :: indir, infile
  type(grid2d_info)             :: ingrid   ! hafs restart grid
  type(grid2d_info)             :: dstgrid  ! rot-ll grid for output
  real     :: radiusf
  
!----for hafs restart
  integer  :: ix, iy, iz, kz
 
!----for hafsvi
  integer  :: nx, ny, nz, filetype  ! filetype: 1=bin, 2=nc
  real     :: lon1,lat1,lon2,lat2,cen_lat,cen_lon,dlat,dlon
  real, allocatable, dimension(:,:) :: glon,glat

  integer  :: i, j, k, flid, ncid, ndims, nrecord
  real     :: rot_lon, rot_lat, ptop
  integer, dimension(nf90_max_var_dims) :: dims
  real, allocatable, dimension(:,:,:,:) :: dat4, dat41, dat42, dat43
  real, allocatable, dimension(:,:,:)   :: dat3, dat31
  real, allocatable, dimension(:,:)     :: dat2, sfcp
  real, allocatable, dimension(:)       :: dat1

  !real, allocatable, dimension(:)       :: pfull, phalf
 
  
!------------------------------------------------------------------------------
! 1 --- arg process
!
! 1.1 --- input_dir
  if (len_trim(in_dir) < 2 .or. trim(in_dir) == 'w' .or. trim(in_dir) == 'null') then
     indir='.'
  else
     indir=trim(in_dir)
  endif

  if (trim(radius) == 'w' .or. trim(radius) == 'null') then
     radiusf = 40.  !deg
  else
     read(radius,*)i
     radiusf = real(i)
     if ( radiusf < 3. .or. radiusf > 70. ) then
        write(*,'(a)')'!!! hafsvi cut radius number wrong: '//trim(radius)
        write(*,'(a)')'!!! please call with --vortexradius=40 (75< 3)'
        stop 'hafsvi_preproc' 
     endif
  endif

  if (trim(res) == 'w' .or. trim(res) == 'null') then
     dlat=0.02
  else
     read(res,*)dlat
  endif
  dlon=dlat

!------------------------------------------------------------------------------
! 2 --- input grid info
!       read from grid file grid_spec.nc:
  infile=trim(indir)//'/grid_spec.nc'
  write(*,'(a)')' --- read grid info from '//trim(infile)
  call rd_grid_spec_data(trim(infile), ingrid)
  ix=ingrid%grid_xt
  iy=ingrid%grid_yt

!------------------------------------------------------------------------------
! 3 --- define rot-ll grid
!-- hafs restart grid
!--   ( 1,ix)  ( 1, 1)
!--   (jx,ix)  (jx, 1)

  !dlon = abs(ingrid%grid_lon(int(iy/2),int(ix/2)) - ingrid%grid_lon(int(iy/2),int(ix/2)+1))   !-- 0.0321
  !dlon = int((dlon+0.0035)*100)/100. 
  !dlat = abs(ingrid%grid_lat(int(iy/2),int(ix/2)) - ingrid%grid_lat(int(iy/2)+1,int(ix/2)))
  !dlat = int((dlat+0.0035)*100)/100. 
  !write(*,'(a,2i5,4f10.5)')'---ingrid: ',ix, iy, ingrid%grid_lon(int(iy/2),int(ix/2)), &
  !     ingrid%grid_lon(int(iy/2),int(ix/2)+1), ingrid%grid_lat(int(iy/2),int(ix/2)), ingrid%grid_lat(int(iy/2)+1,int(ix/2))
  cen_lat = tc%lat
  cen_lon = tc%lon
  nx = int(radiusf/2.0/dlon+0.5)*2+1 
  ny = int(radiusf/2.0/dlat+0.5)*2+1
  lon1 = - radiusf/2.0
  lat1 = - radiusf/2.0
  lon2 = radiusf/2.0
  lat2 = radiusf/2.0
  !!--- get rot-ll grid
  allocate(glon(nx,ny), glat(nx,ny))
  do j = 1, ny; do i = 1, nx
     !rot_lon = lon1 + dlon*(i-1)
     !rot_lat = lat1 + dlat*(j-1)
     rot_lon = lon2 - dlon*(i-1)
     rot_lat = lat2 - dlat*(j-1)
     call rtll(rot_lon, rot_lat, glon(i,j), glat(i,j), cen_lon, cen_lat)
     !write(*,*)rot_lon,rot_lat,glon(i,j), glat(i,j)
     !pause
  !   glon(i,j)=rot_lon
  !   glat(i,j)=rot_lat
  !   !call ijll_rotlatlon(real(i), real(j), radiusf/2.0, radiusf/2.0, nx, ny, lat1, lon1, 'T', glat(i,j), glon(i,j))
  !   call ijll_rotlatlon(real(i), real(j), radiusf/2.0, radiusf/2.0, nx, ny, cen_lat, cen_lon, 'T', glat(i,j), glon(i,j))
  !   !write(*,'(a,2i5,2f10.4)')'---ijll_rotlatlon ', i, j, glat(i,j), glon(i,j)
  enddo; enddo
  !lon1 = glon(1, 1)
  !lat1 = glat(1, 1) 
  !lon2 = glon(nx, ny)
  !lat2 = glat(nx, ny)
  write(*,'(a)')'---rot-ll grid: nx, ny, cen_lon, cen_lat, dlon, dlat, lon1, lon2, lat1, lat2'
  write(*,'(15x,2i5,8f10.5)')    nx, ny, cen_lon, cen_lat, dlon, dlat, lon1, lon2, lat1, lat2

  write(*,'(a,4f10.5)')'---rot-ll grid rot_lon:', glon(1,1), glon(1,ny), glon(nx,ny), glon(nx,1) 
  write(*,'(a,4f10.5)')'---rot-ll grid rot_lat:', glat(1,1), glat(1,ny), glat(nx,ny), glat(nx,1) 

!------------------------------------------------------------------------------
! 4 --- set dstgrid
  dstgrid%grid_x = nx
  dstgrid%grid_y = ny
  dstgrid%ntime  = 1
  dstgrid%grid_xt = nx
  dstgrid%grid_yt = ny
  allocate(dstgrid%grid_lon (dstgrid%grid_x,dstgrid%grid_y))
  allocate(dstgrid%grid_lont(dstgrid%grid_x,dstgrid%grid_y))
  dstgrid%grid_lon  = glon
  dstgrid%grid_lont = glon
  allocate(dstgrid%grid_lat (dstgrid%grid_x,dstgrid%grid_y))
  allocate(dstgrid%grid_latt(dstgrid%grid_x,dstgrid%grid_y))
  dstgrid%grid_lat  = glat
  dstgrid%grid_latt = glat
   
!------------------------------------------------------------------------------
! 5 --- calculate output-grid in input-grid's positions (xin, yin), and each grid's weight to dst
  gwt%max_points=3
  call cal_src_dst_grid_weight(ingrid, dstgrid)

!------------------------------------------------------------------------------
! 6 --- process output file type: now is only for bin
  i=len_trim(out_file)
  if ( out_file(i-2:i) == '.nc' ) then
     write(*,'(a)')' --- output to '//trim(out_file)
     filetype=2
     call nccheck(nf90_open(trim(out_file), nf90_write, flid), 'wrong in open '//trim(out_file), .true.) 
  else
     filetype=1
     flid=71
     open(unit=flid,file=trim(out_file),form='unformatted',status='unknown')
  endif

!------------------------------------------------------------------------------
! 7 --- output
!
  do_out_var_loop: do nrecord = 1, 17
     !-----------------------------
     !---7.1 record 1: nx, ny, nz 
     !---nx, ny, nz, & lon1,lat1,lon2,lat2,cen_lon,cen_lat
     if ( nrecord == 1 ) then 
        infile=trim(indir)//'/atmos_static.nc'
        call get_var_dim(trim(infile), 'pfull', ndims, dims)
        nz=dims(1)
        iz=nz   !same vertical levels
  
        if ( filetype == 1) then
           write(*,'(a,3i6)')'=== record1: ',nx, ny, nz
           write(flid) nx, ny, nz 
        endif
     endif

     !-----------------------------
     !---7.2 record 2: lon1,lat1,lon2,lat2,cen_lon,cen_lat
     if ( nrecord == 2 ) then
        write(*,'(a,6f8.3)')'=== record2: ',lon1,lat1,lon2,lat2,cen_lon,cen_lat
        write(flid) lon1,lat1,lon2,lat2,cen_lon,cen_lat
     endif   

     !-----------------------------
     !---7.3 record 3: (((pf1(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
     !---     hafs-VI/read_hafs_out.f90 pf:
     !---          ph(k) = ak(k) + bk(k)*p_s --> half level pressure
     !---          pf(k) = (ph(k+1) - ph(k)) / log(ph(k+1)/ph(k)) --> full level pressure
     !---          
     !---seem pf1 is pressure on full level, use 
     !---    pf1(k) = phalf(1) + sum(delp(1:k))
     if ( nrecord == 3 ) then
        infile=trim(indir)//'/atmos_static.nc'
        allocate(dat4(iz+1,1,1,1))
        call get_var_data(trim(infile), 'phalf', iz+1, 1, 1, 1, dat4)
        ptop=dat4(1,1,1,1)*100.  !phalf:units = "mb" ;
        deallocate(dat4)

        infile=trim(indir)//'/'//trim(in_date)//'.fv_core.res.tile1.nc'
        allocate(dat4(ix, iy, iz,1))
        allocate(dat41(ix, iy, iz,1))
        allocate(dat2(ix, iy))
        !write(*,'(a,3i5)')'delp: ',ix, iy, iz
        call get_var_data(trim(infile), 'delp', ix, iy, iz,1, dat4)
        dat2(:,:)=ptop
        do k = 1, iz
           dat41(:,:,k,1)=dat2(:,:)+dat4(:,:,k,1)/2.0  
           dat2(:,:)=dat2(:,:)+dat4(:,:,k,1)
           !write(*,*)k, dat2(int(ix/2),int(iy/2)), dat4(int(ix/2),int(iy/2),k,1),dat41(int(ix/2),int(iy/2),k,1)
        enddo
        allocate(sfcp(ix, iy))
        sfcp=dat41(iz,:,:,1)
        deallocate(dat2,dat4)
     endif

     !-----------------------------
     !---7.4 record 4: (((tmp(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
     !--- atm T (K)?  
     if ( nrecord == 4 ) then
        infile=trim(indir)//'/'//trim(in_date)//'.fv_core.res.tile1.nc'
        allocate(dat4(ix, iy, iz,1))
        call get_var_data(trim(infile), 'T', ix, iy, iz,1, dat4)
        !--- need any other processing? 
        allocate(dat41(ix, iy, iz,1))
        dat41=dat4*1.0
        deallocate(dat4)
     endif

     !-----------------------------
     !---7.5 record 5: (((spfh(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
     if ( nrecord == 5 ) then
        infile=trim(indir)//'/'//trim(in_date)//'.fv_tracer.res.tile1.nc'
        allocate(dat4(ix, iy, iz,1))
        call get_var_data(trim(infile), 'sphum', ix, iy, iz,1, dat4)
        allocate(dat41(ix, iy, iz,1))
        dat41=dat4*1.0
        deallocate(dat4)
     endif
  
     !-----------------------------
     !---7.6 record 6: (((ugrd(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
     if ( nrecord == 6 ) then
        infile=trim(indir)//'/'//trim(in_date)//'.fv_core.res.tile1.nc'
        allocate(dat4(ix, iy+1, iz,1))
        call get_var_data(trim(infile), 'u', ix, iy+1, iz, 1, dat4)
        !-- processing: destage + north-wind?
        allocate(dat41(ix, iy, iz,1))
        dat41(:,:,:,1)=(dat4(:,1:iy,:,1)+dat4(:,2:iy+1,:,1))/2.0
        deallocate(dat4)
     endif  
 
     !-----------------------------
     !---7.7 record 7: (((vgrd(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
     if ( nrecord == 7 ) then
        infile=trim(indir)//'/'//trim(in_date)//'.fv_core.res.tile1.nc'
        allocate(dat4(ix+1, iy, iz,1))
        call get_var_data(trim(infile), 'v', ix+1, iy, iz, 1, dat4)
        !-- processing: destage + north-wind?
        allocate(dat41(ix, iy, iz,1))
        dat41(:,:,:,1)=(dat4(1:ix,:,:,1)+dat4(2:ix+1,:,:,1))/2.0
        deallocate(dat4)
     endif

     !-----------------------------
     !---7.8 record 8: (((dzdt(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
     if ( nrecord == 8 ) then
        infile=trim(indir)//'/'//trim(in_date)//'.fv_core.res.tile1.nc'
        allocate(dat4(ix, iy+1, iz,1))
        call get_var_data(trim(infile), 'W', ix, iy, iz,1, dat4)

        !-- w (m/s) or omega (pa/s) is needed here?
        allocate(dat41(ix, iy, iz,1))
        dat41=dat4*1.0
        deallocate(dat4)
     endif

     !-----------------------------
     !---7.9 record 9: (((z1(i,j,k),i=1,nx),j=1,ny),k=nz1,1,-1)
     !---     hafs-VI/read_hafs_out.f90 z1:
     !---          z1(I,J,K)=z1(I,J,K+1)+rdgas1*tmp(i,j,k)*(1.+0.608*spfh(i,j,k))*ALOG(ph1(i,j,k+1)/ph1(i,j,k))
     !--- hgt?: phis-sum(DZ)?
     !--- in fv_core.res.tile1.nc, what are DZ and phis?
     if ( nrecord == 9 ) then
        infile=trim(indir)//'/'//trim(in_date)//'.fv_core.res.tile1.nc'
        allocate(dat4(ix, iy, 1,1))
        call get_var_data(trim(infile), 'phis', ix, iy, 1, 1, dat4)
        allocate(dat41(ix, iy, iz+1, 1))
        dat41(:,:,iz+1,1)=dat4(:,:,1,1)
        deallocate(dat4)

        allocate(dat4(ix, iy, iz, 1))
        call get_var_data(trim(infile), 'DZ', ix, iy, iz, 1, dat4)
        do k = iz, 1, -1
           dat41(:,:,k,1)=dat41(:,:,k+1,1)-dat4(:,:,k,1)
        enddo
        deallocate(dat4)
     endif

     !-----------------------------
     !---7.10 record 10: glon,glat,glon,glat   ! 2D
     !--- glat=grid_yt*180./pi, grid_yt=1:2160, what is this?
     if ( nrecord == 10 ) then
        write(*,'(a,4f8.3)')'=== record10: ',glon(1,1), glat(1,1), glon(ix,iy), glat(ix,iy)
        write(flid) glon,glat,glon,glat 
     endif
   
     !-----------------------------
     !---7.11 record 11: (((ph1(i,j,k),i=1,nx),j=1,ny),k=nz1,1,-1)
     !---     hafs-VI/read_hafs_out.f90 ph:
     !---       ph(k) = ak(k) + bk(k)*p_s --> pressure in pa
     !---       64.270-->100570
     !---seem ph1 is pressure on half level, use 
     !---    pf1(k) = phalf(1) + sum(delp(1:k))
     if ( nrecord == 11 ) then
        infile=trim(indir)//'/atmos_static.nc'
        allocate(dat4(iz+1,1,1,1))
        call get_var_data(trim(infile), 'phalf', iz+1, 1, 1, 1, dat4)
        ptop=dat4(1,1,1,1)*100.  !phalf:units = "mb" ;
        deallocate(dat4)

        infile=trim(indir)//'/'//trim(in_date)//'.fv_core.res.tile1.nc'
        allocate(dat4(ix, iy, iz,1))
        allocate(dat41(ix, iy, iz+1,1))
        call get_var_data(trim(infile), 'delp', ix, iy, iz,1, dat4)
        dat41(:,:,1,1)=ptop
        do k = 1, iz
           dat41(:,:,k+1,1)=dat41(:,:,k,1)+dat4(:,:,k,1)
        enddo
        deallocate(dat4)
     endif
   
     !-----------------------------
     !---7.12 record 12: pressfc1              ! 2D
     !--- use lowest-level pressure?
     if ( nrecord == 12 ) then
        allocate(dat41(ix, iy, 1, 1))
        dat41(:,:,1,1)=sfcp(:,:)
        deallocate(sfcp)
     endif
   
     !-----------------------------
     !---7.13 record 13: ak
     if ( nrecord == 13 ) then
        infile=trim(indir)//'/'//trim(in_date)//'.fv_core.res.nc'
        allocate(dat4(iz+1,1,1,1))
        call get_var_data(trim(infile), 'ak', iz+1, 1, 1, 1, dat4)
        write(*,'(a,20f8.3)')'=== record13: ', (dat4(k,1,1,1),k=1,iz+1)
        write(flid) (dat4(k,1,1,1),k=1,iz+1)
        deallocate(dat4) 
     endif
   
     !-----------------------------
     !---7.14 record 14: bk 
     if ( nrecord == 14 ) then
        infile=trim(indir)//'/'//trim(in_date)//'.fv_core.res.nc'
        allocate(dat4(iz+1,1,1,1))
        call get_var_data(trim(infile), 'bk', iz+1, 1, 1, 1, dat4)
        write(*,'(a,20f8.3)')'=== record14: ', (dat4(k,1,1,1),k=1,iz+1)
        write(flid) (dat4(k,1,1,1),k=1,iz+1)
        deallocate(dat4) 
     endif
   
     !-----------------------------
     !---7.15 record 15: land                  ! =A101 = land sea mask, B101 = ZNT
     !---     hafs-VI/read_hafs_out.f90 land:long_name = "sea-land-ice mask (0-sea, 1-land, 2-ice)" ;
     !--- oro_data.nc: land_frac
     if ( nrecord == 15 ) then
        infile=trim(indir)//'/oro_data.nc'
        allocate(dat4(ix, iy, 1,1))
        call get_var_data(trim(infile), 'land_frac', ix, iy, 1, 1, dat4)
       
        !--- convert frac to 0/1
        allocate(dat41(ix, iy, 1,1))
        dat41=1.0
        where ( dat4 < 0.50 ) dat41=0.0
        deallocate(dat4)
     endif
   
     !-----------------------------
     !---7.16 record 16: sfcr                  ! =B101 = Z0
     !---surface roughness
     if ( nrecord == 16 ) then
        infile=trim(indir)//'/'//trim(in_date)//'.sfc_data.nc'
        allocate(dat4(ix, iy, 1,1))
        allocate(dat41(ix, iy, 1,1))
        call get_var_data(trim(infile), 'zorl', ix, iy, 1, 1, dat41)
        call get_var_data(trim(infile), 'zorll', ix, iy, 1, 1, dat4)

        !--- combine zorl and zorll/100.
        where(dat41>9000.)dat41=dat4/100.
        deallocate(dat4)
     endif
   
     !-----------------------------
     !---7.17 record 17: C101                  ! =C101 = (10m wind speed)/(level 1 wind speed)
     !--- could set to 1.0 or 0.95
     if ( nrecord == 17 ) then
        allocate(dat41(ix, iy, 1,1))
        dat41=0.96 
     endif
   
     !-----------------------------
     !---7.18 output 3d
     if ( nrecord == 3 .or. nrecord == 4 .or. nrecord == 5 .or. nrecord == 6 .or. &
          nrecord == 7 .or. nrecord == 8 .or. nrecord == 9 .or. nrecord ==11 .or. &
          nrecord ==12 .or. nrecord ==15 .or. nrecord ==16 .or. nrecord ==17 ) then
        kz=nz
        if ( nrecord == 12 .or. nrecord == 15 .or. nrecord == 16 .or. nrecord ==17 ) then
           kz=1
        endif
        if ( nrecord ==  9 .or. nrecord == 11 ) then
           kz=nz+1
        endif
        !--- map fv3 grid to rot-ll grid: ingrid-->dstgrid
        allocate(dat42(nx,ny,kz,1))
        dat42=-9999999.
        call combine_grids_for_remap(ix,iy,kz,1,dat41,nx,ny,kz,1,dat42,gwt%gwt_t,dat42)
  
        !--- output
        write(*,'(a,i3)')' --- output record ', nrecord
        if ( filetype == 1) then
           write(*,'(a,i2.2,a,f10.3)')'=== record',nrecord,': ', dat42(1,1,1,1)
           write(flid) (((dat42(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
        endif
        deallocate(dat41,dat42)
     endif 

  enddo do_out_var_loop !: for nrecord = 1, 17

  return
  end subroutine hafsvi_preproc

!========================================================================================

