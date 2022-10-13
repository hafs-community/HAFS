!========================================================================================
  subroutine hafsvi_preproc(in_dir, in_date, nestdoms, radius, res, out_file)

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

  use constants
  use netcdf
  use module_mpi
  use var_type

  implicit none

  character (len=*), intent(in) :: in_dir, in_date, radius, res, out_file
  integer, intent(in)           :: nestdoms 
!--- in_dir,  HAFS_restart_folder, which holds grid_spec.nc, fv_core.res.tile1.nc, 
!             fv_srf_wnd.res.tile1.nc, fv_tracer.res.tile1.nc, phy_data.nc, sfc_data.nc
!--- in_date, HAFS_restart file date, like 20200825.120000
!--- radius,  to cut a square, default value is 40, which means a 40deg x 40deg square.
!--- out_file: output file, default is bin format, if the file name is *.nc, then output nc format.
!--- nestdoms: total nest domain number: 0-no nesting
!---                                     1-nest02.tile2 + 0
!---                                     2-nest03.tile3 + 1

  character (len=2500)   :: indir, infile
  character (len=2500)   :: infile_fvcore, infile_core, infile_tracer, infile_phy, infile_sfc, infile_grid, infile_grid2, infile_atmos, infile_oro
  type(grid2d_info)      :: dstgrid  ! rot-ll grid for output
  type(grid2d_info)      :: ingrid   ! hafs restart grid
  real     :: radiusf
  logical  :: file_exist
  
!----for hafs restart
  integer  :: ix, iy, iz, kz, ndom, nd
  character (len=50) :: nestfl, tilefl, tempfl
                        ! grid_spec.nc : grid_spec.nest02.tile2.nc
                        ! fv_core.res.tile1.nc : fv_core.res.nest02.tile2.nc
                        ! phy_data.nc  : phy_data.nest02.tile2.nc
 
!----for hafsvi
  integer  :: nx, ny, nz, filetype  ! filetype: 1=bin, 2=nc
  real     :: lon1,lat1,lon2,lat2,cen_lat,cen_lon,dlat,dlon
  real, allocatable, dimension(:,:) :: glon,glat

  integer  :: i, j, k, flid_in, flid_out, ncid, ndims, nrecord
  real     :: rot_lon, rot_lat, ptop
  integer, dimension(nf90_max_var_dims) :: dims
  real, allocatable, dimension(:,:,:,:) :: dat4, dat41, dat42, dat43, u, v
  real, allocatable, dimension(:,:,:)   :: dat3, dat31
  real, allocatable, dimension(:,:)     :: dat2, dat21, sfcp
  real, allocatable, dimension(:)       :: dat1

  !real, allocatable, dimension(:)       :: pfull, phalf
  real, allocatable, dimension(:,:)     :: cangu, sangu, cangv, sangv
  real    :: cputime1, cputime2, cputime3 
  
!------------------------------------------------------------------------------
! 1 --- arg process
!
! 1.1 --- ndom
  ndom=nestdoms+1  
  

! 1.2 --- input_dir
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
! 2 --- set dstgrid: rot-ll grid
! 2.1 --- define rot-ll grid
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
     rot_lon = lon1 + dlon*(i-1)
     rot_lat = lat1 + dlat*(j-1)
     call rtll(rot_lon, rot_lat, glon(i,j), glat(i,j), cen_lon, cen_lat)
  enddo; enddo
  write(*,'(a)')'---rot-ll grid: nx, ny, cen_lon, cen_lat, dlon, dlat, lon1, lon2, lat1, lat2'
  write(*,'(15x,2i5,8f10.5)')    nx, ny, cen_lon, cen_lat, dlon, dlat, lon1, lon2, lat1, lat2
  !write(*,'(a,4f10.5)')'---rot-ll grid rot_lon:', glon(1,1), glon(1,ny), glon(nx,ny), glon(nx,1) 
  !write(*,'(a,4f10.5)')'---rot-ll grid rot_lat:', glat(1,1), glat(1,ny), glat(nx,ny), glat(nx,1) 

! 2.2 --- set dstgrid
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
! 3 --- process output file type: now is only for bin
!  i=len_trim(out_file)
!  if ( out_file(i-2:i) == '.nc' ) then
!     write(*,'(a)')' --- output to '//trim(out_file)
!     filetype=2
!     call nccheck(nf90_open(trim(out_file), nf90_write, flid), 'wrong in open '//trim(out_file), .true.) 
!  else
!     filetype=1
!     flid=71
!     open(unit=flid,file=trim(out_file),form='unformatted',status='unknown')
!  endif

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! --- domain loop: from inner domain to outer domain, so the number is from max to 1
  do_nestdom_loop: do nd = 1, ndom

     !-------------------------------------------------------------------------
     ! 3 --- initialization: clean ingrid, weight
     ! ingrid%grid_x=-99; ingrid%grid_y=-99; ingrid%grid_xt=-99; ingrid%grid_yt=-99

     !-------------------------------------------------------------------------
     ! 4 --- input grid info
     !       read from grid file grid_spec.nc:
     !       nestfl, tilefl: infile_core, infile_tracer, infile_grid, infile_atmos, infile_oro
     write(nestfl,'(a4,i2.2)')'nest',nd
     write(tilefl,'(a4,i0)')'tile',nd     
     if ( nd == 1 ) then
        infile_grid=trim(indir)//'/grid_spec.nc'
        infile_grid2=trim(indir)//'/grid_mspec_'//in_date(1:4)//'_'//in_date(5:6)//'_'//in_date(7:8)//'_'//in_date(10:11)//'.nc'
        infile_oro =trim(indir)//'/oro_data.nc'
        infile_atmos=trim(indir)//'/atmos_static.nc'
        infile_fvcore=trim(indir)//'/'//trim(in_date)//'.fv_core.res.nc'
        infile_core=trim(indir)//'/'//trim(in_date)//'.fv_core.res.tile1.nc'
        infile_tracer=trim(indir)//'/'//trim(in_date)//'.fv_tracer.res.tile1.nc'
        infile_phy =trim(indir)//'/'//trim(in_date)//'.phy_data.nc'
        infile_sfc =trim(indir)//'/'//trim(in_date)//'.sfc_data.nc'
     else
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

     if ( debug_level > 10 ) write(*,'(a)')' --- read grid info from '//trim(infile_grid)
     call rd_grid_spec_data(trim(infile_grid), ingrid)
     ix=ingrid%grid_xt
     iy=ingrid%grid_yt
     if ( debug_level > 10 ) then
        write(*,'(a,i,1x,i,1x,f,1x,f,1x,f,1x,f)')' --- ingrid info: ', ix, iy, &
              ingrid%grid_lon(int(ix/2), int(iy/2)), ingrid%grid_lat(int(ix/2), int(iy/2)), & 
              ingrid%grid_lont(int(ix/2), int(iy/2)), ingrid%grid_latt(int(ix/2), int(iy/2))
     endif

     !---to add the test if the tc was inside of the domain
     

     ! call FV3-grid cos and sin
     allocate( cangu(ix,iy+1),sangu(ix,iy+1),cangv(ix+1,iy),sangv(ix+1,iy) ) 
     call cal_uv_coeff_fv3(ix, iy, ingrid%grid_lat, ingrid%grid_lon, cangu, sangu, cangv, sangv) 

     !-------------------------------------------------------------------------
     ! 5 --- calculate output-grid in input-grid's positions (xin, yin), and each grid's weight to dst
     if ( debug_level > 10 ) then
        write(*,'(a)')' --- call cal_src_dst_grid_weight'
        write(*,'(a,2(I,1x),2(f,1x))')' --- dstgrid: ', nx, ny, &
             dstgrid%grid_lont(int(nx/2),int(ny/2)), dstgrid%grid_latt(int(nx/2),int(ny/2)) 
     endif
     call cal_src_dst_grid_weight(ingrid, dstgrid)

     !-------------------------------------------------------------------------
     ! 6 --- dst files
     flid_in=71   !inner domain rot-ll file
     flid_out=72  !current domain rot-ll file
     if ( nd == 1 ) then
        open(unit=flid_out,file=trim(out_file),form='unformatted',status='unknown')
     else
        open(unit=flid_out,file=trim(out_file)//'_'//trim(nestfl),form='unformatted',status='unknown')
     endif
     if ( nd == 2 ) then   !if ( nd >= 1 .and. nd < ndom .and. ndom > 1 ) then
        open(unit=flid_in,file=trim(out_file),form='unformatted',status='old')
     elseif ( nd > 2 ) then   !if ( nd >= 1 .and. nd < ndom .and. ndom > 1 ) then
        write(tempfl,'(a4,i2.2)')'nest',nd-1
        open(unit=flid_in,file=trim(out_file)//'_'//trim(tempfl),form='unformatted',status='old')
     endif

     !-------------------------------------------------------------------------
     ! 7 --- output
     do_out_var_loop: do nrecord = 1, 17
        !-----------------------------
        !---7.1 record 1: nx, ny, nz 
        !---nx, ny, nz, & lon1,lat1,lon2,lat2,cen_lon,cen_lat
        call cpu_time(cputime1)
        write(*,'(a,i3,f)')' --- record start cputime: ', nrecord, cputime1
        if ( nrecord == 1 ) then 
           call get_var_dim(trim(infile_atmos), 'pfull', ndims, dims)
           nz=dims(1)
           iz=nz   !same vertical levels
  
           write(*,'(a,3i6)')'=== record1: ',nx, ny, nz
           write(flid_out) nx, ny, nz 
           if ( nd > 1 ) read(flid_in)
        endif

        !-----------------------------
        !---7.2 record 2: lon1,lat1,lon2,lat2,cen_lon,cen_lat
        if ( nrecord == 2 ) then
           write(*,'(a,6f8.3)')'=== record2: ',lon1,lat1,lon2,lat2,cen_lon,cen_lat
           write(flid_out) lon1,lat1,lon2,lat2,cen_lon,cen_lat
           if ( nd > 1 ) read(flid_in)
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
           allocate(dat4(iz+1,1,1,1))
           call get_var_data(trim(infile_atmos), 'phalf', iz+1, 1, 1, 1, dat4)
           ptop=dat4(1,1,1,1)*100.  !phalf:units = "mb" ;
           deallocate(dat4)

           allocate(dat4(ix, iy, iz,1))
           allocate(dat41(ix, iy, iz,1))
           allocate(dat2(ix, iy))
           !write(*,'(a,3i5)')'delp: ',ix, iy, iz
           call get_var_data(trim(infile_core), 'delp', ix, iy, iz,1, dat4)
           dat2(:,:)=ptop
           do k = 1, iz
              dat41(:,:,k,1)=dat2(:,:)+dat4(:,:,k,1)/2.0  
              dat2(:,:)=dat2(:,:)+dat4(:,:,k,1)
           enddo
           allocate(sfcp(ix, iy))
           sfcp=dat41(:,:,iz,1)
           deallocate(dat2,dat4)
        endif

        !-----------------------------
        !---7.4 record 4: (((tmp(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        if ( nrecord == 4 ) then
           allocate(dat4(ix, iy, iz,1))
           call get_var_data(trim(infile_core), 'T', ix, iy, iz,1, dat4)
           !--- need any other processing? 
           allocate(dat41(ix, iy, iz,1))
           dat41=dat4*1.0
           deallocate(dat4)
        endif

        !-----------------------------
        !---7.5 record 5: (((spfh(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        if ( nrecord == 5 ) then
           allocate(dat4(ix, iy, iz,1))
           call get_var_data(trim(infile_tracer), 'sphum', ix, iy, iz,1, dat4)
           allocate(dat41(ix, iy, iz,1))
           dat41=dat4*1.0
           deallocate(dat4)
        endif
  
        !-----------------------------
        !---7.6 record 6: (((ugrd(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        !---7.7 record 7: (((vgrd(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        if ( nrecord == 6 ) then
           !---get u,v from restart
           allocate(dat41(ix, iy+1, iz,1), dat42(ix+1, iy, iz, 1))
           call get_var_data(trim(infile_core), 'u', ix, iy+1, iz, 1, dat41)
           call get_var_data(trim(infile_core), 'v', ix+1, iy, iz, 1, dat42)

           !---convert u,v from fv3grid to earth
           allocate(u(ix, iy, iz,1), v(ix, iy, iz, 1))
           allocate(dat2(ix, iy+1), dat21(ix+1, iy))
           do k = 1, iz
              call fv3uv2earth(ix, iy, dat41(:,:,k,1), dat42(:,:,k,1), cangu, sangu, cangv, sangv, dat2, dat21)
              !---destage: C-/D- grid to A-grid
              u(:,:,k,1) = (dat2 (:,1:iy)+dat2 (:,2:iy+1))/2.0
              v(:,:,k,1) = (dat21(1:ix,:)+dat21(2:ix+1,:))/2.0
           enddo

           deallocate(dat41, dat42, dat2, dat21, cangu, sangu, cangv, sangv)
        endif  
 
        !-----------------------------
        !---7.8 record 8: (((dzdt(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        if ( nrecord == 8 ) then
           allocate(dat4(ix, iy+1, iz,1))
           call get_var_data(trim(infile_core), 'W', ix, iy, iz,1, dat4)

           !-- w (m/s) or omega (pa/s) is needed here?
           allocate(dat41(ix, iy, iz,1))
           dat41=dat4*1.0
           deallocate(dat4)
        endif

        !-----------------------------
        !---7.9 record 9: (((z1(i,j,k),i=1,nx),j=1,ny),k=nz1,1,-1)
        !---     hafs-VI/read_hafs_out.f90 z1:
        !---          z1(I,J,K)=z1(I,J,K+1)+rdgas1*tmp(i,j,k)*(1.+0.608*spfh(i,j,k))*ALOG(ph1(i,j,k+1)/ph1(i,j,k))
        !--- hgt: phis/g-sum(DZ)
        if ( nrecord == 9 ) then
           allocate(dat4(ix, iy, 1,1))
           call get_var_data(trim(infile_core), 'phis', ix, iy, 1, 1, dat4)
           allocate(dat41(ix, iy, iz+1, 1))
           dat41(:,:,iz+1,1)=dat4(:,:,1,1)/g
           deallocate(dat4)

           allocate(dat4(ix, iy, iz, 1))
           call get_var_data(trim(infile_core), 'DZ', ix, iy, iz, 1, dat4)
           do k = iz, 1, -1
              dat41(:,:,k,1)=dat41(:,:,k+1,1)-dat4(:,:,k,1)
           enddo
           !write(*,'(a,200f)')'z1: ',dat41(int(ix/2),int(iy/2),:,1)
           deallocate(dat4)
        endif

        !-----------------------------
        !---7.10 record 10: glon,glat,glon,glat   ! 2D
        !--- glat=grid_yt*180./pi, grid_yt=1:2160, what is this?
        if ( nrecord == 10 ) then
           !write(*,'(a,4i8)')'=== record10: ',ix, iy, nx, ny

           !For WPAC storms located near the west of international date line, add 360 to
           !longitude value in the eastern side of IDL (western hemisphere) so that all
           !longitude values have positive values in the VI domain.
           if ( cen_lon > 0. ) then
              do j = 1, ny; do i = 1, nx
                 if(glon(i,j).lt.0.0) glon(i,j)=glon(i,j)+360.0
              enddo; enddo
           endif

           !For CPAC storms located near the east of international date line, subrtact 360
           !from longitude value in the western side of IDL (eastern hemisphere) so
           !that all longitude values have negative values in the VI domain.
           if ( cen_lon < -140. )then
              do j = 1, ny; do i = 1, nx
                 if(glon(i,j).gt.0.0) glon(i,j)=glon(i,j)-360.0
              enddo; enddo
           endif

           write(*,'(a,4f8.3)')'=== record10: ',glon(1,1), glat(1,1), glon(nx,ny), glat(nx,ny)
           write(flid_out) glon,glat,glon,glat 
           if ( nd > 1 ) read(flid_in)
        endif
   
        !-----------------------------
        !---7.11 record 11: (((ph1(i,j,k),i=1,nx),j=1,ny),k=nz1,1,-1)
        !---     hafs-VI/read_hafs_out.f90 ph:
        !---       ph(k) = ak(k) + bk(k)*p_s --> pressure in pa
        !---       64.270-->100570
        !---seem ph1 is pressure on half level, use 
        !---    pf1(k) = phalf(1) + sum(delp(1:k))
        if ( nrecord == 11 ) then
           allocate(dat4(iz+1,1,1,1))
           call get_var_data(trim(infile_atmos), 'phalf', iz+1, 1, 1, 1, dat4)
           ptop=dat4(1,1,1,1)*100.  !phalf:units = "mb" ;
           deallocate(dat4)

           allocate(dat4(ix, iy, iz,1))
           allocate(dat41(ix, iy, iz+1,1))
           call get_var_data(trim(infile_core), 'delp', ix, iy, iz,1, dat4)
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
           allocate(dat4(iz+1,1,1,1))
           call get_var_data(trim(infile_fvcore), 'ak', iz+1, 1, 1, 1, dat4)
           write(*,'(a,200f12.1)')'=== record13: ', (dat4(k,1,1,1),k=1,iz+1)
           write(flid_out) (dat4(k,1,1,1),k=1,iz+1)
           if ( nd > 1 ) read(flid_in)
           deallocate(dat4) 
        endif
   
        !-----------------------------
        !---7.14 record 14: bk 
        if ( nrecord == 14 ) then
           allocate(dat4(iz+1,1,1,1))
           call get_var_data(trim(infile_fvcore), 'bk', iz+1, 1, 1, 1, dat4)
           write(*,'(a,200f10.3)')'=== record14: ', (dat4(k,1,1,1),k=1,iz+1)
           write(flid_out) (dat4(k,1,1,1),k=1,iz+1)
           if ( nd > 1 ) read(flid_in)
           deallocate(dat4) 
        endif
   
        !-----------------------------
        !---7.15 record 15: land                  ! =A101 = land sea mask, B101 = ZNT
        !---     hafs-VI/read_hafs_out.f90 land:long_name = "sea-land-ice mask (0-sea, 1-land, 2-ice)" ;
        !--- sfc_data.nc: slmsk
        if ( nrecord == 15 ) then
           allocate(dat41(ix, iy, 1,1))
           call get_var_data(trim(infile_sfc), 'slmsk', ix, iy, 1, 1, dat41)
        endif
   
        !-----------------------------
        !---7.16 record 16: sfcr                  ! =B101 = Z0
        !---surface roughness
        if ( nrecord == 16 ) then
           allocate(dat41(ix, iy, 1,1))
           call get_var_data(trim(infile_sfc), 'zorl', ix, iy, 1, 1, dat41)
           ! convert from cm to m
           dat41=dat41/100.
        endif
   
        !-----------------------------
        !---7.17 record 17: C101                  ! =C101 = (10m wind speed)/(level 1 wind speed)
        !---                                      ! =C101 = f10m (in the sfc_data.nc)
        if ( nrecord == 17 ) then
           allocate(dat41(ix, iy, 1, 1))
           call get_var_data(trim(infile_sfc), 'f10m', ix, iy, 1, 1, dat41)
        endif
   
        !-----------------------------
        !---7.18 output 3d
        if ( nrecord == 3 .or. nrecord == 4 .or. nrecord == 5 .or. &
             nrecord == 8 .or. nrecord == 9 .or. nrecord ==11 .or. &
             nrecord ==12 .or. nrecord ==15 .or. nrecord ==16 .or. nrecord ==17 ) then
           kz=nz
           if ( nrecord == 12 .or. nrecord == 15 .or. nrecord == 16 .or. nrecord ==17 ) then
              kz=1
           endif
           if ( nrecord ==  9 .or. nrecord == 11 ) then
              kz=nz+1
           endif
           !--- map fv3 grid to rot-ll grid: ingrid-->dstgrid
           !call cpu_time(cputime2)
           !write(*,'(a,i3,f)')' --- read rot-ll grid for 1 record ', nrecord, cputime2
           allocate(dat42(nx,ny,kz,1), dat43(nx,ny,kz,1))
           if ( nd > 1 ) then
              read(flid_in)dat43
              do k = 1, kz
                  dat42(:,:,k,1)=dat43(:,:,kz-k+1,1)
              enddo
              dat43=-999999.
           else
              dat42=-9999999.; 
           endif
           call combine_grids_for_remap(ix,iy,kz,1,dat41,nx,ny,kz,1,dat42,gwt%gwt_t,dat43)
  
           !--- output
           !write(*,'(a,i2.2,a,200f)')'=== record',nrecord,': ', dat43(int(nx/2),int(ny/2),:,1)
           write(flid_out) (((dat43(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
           deallocate(dat41, dat42, dat43)
        else if ( nrecord == 6 ) then  !---u,v
           kz=nz
           !--- map u to rot-ll grid: ingrid-->dstgrid
           allocate(dat42(nx,ny,kz,1), dat43(nx,ny,kz,1))
           if ( nd > 1 ) then
              !read(flid_in)(((dat42(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
              read(flid_in)dat43
              do k = 1, kz
                  dat42(:,:,k,1)=dat43(:,:,kz-k+1,1)
              enddo
              dat43=-999999.
           else
              dat42=-9999999.; dat43=0.0
           endif
           call combine_grids_for_remap(ix,iy,kz,1,u,nx,ny,kz,1,dat42,gwt%gwt_t,dat43)
           write(flid_out) (((dat43(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
           if ( nd > 1 ) then
              !read(flid_in)(((dat42(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
              read(flid_in)dat43
              do k = 1, kz
                  dat42(:,:,k,1)=dat43(:,:,kz-k+1,1)
              enddo
              dat43=-999999.
           else
              dat42=-9999999.; dat43=0.0
           endif
           call combine_grids_for_remap(ix,iy,kz,1,v,nx,ny,kz,1,dat42,gwt%gwt_t,dat43)
           write(flid_out) (((dat43(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
           deallocate(u, v, dat42, dat43)
        endif 

     enddo do_out_var_loop !: for nrecord = 1, 17

     !-------------------------------------------------------------------------
     ! 8 --- clean ingrid gwt 
     deallocate( ingrid%grid_lon, ingrid%grid_lat, ingrid%grid_lont, ingrid%grid_latt)
     deallocate( gwt%gwt_t, gwt%gwt_u, gwt%gwt_v )

  enddo do_nestdom_loop !: do nd = 1, ndom

  return
  end subroutine hafsvi_preproc

!========================================================================================
  subroutine hafsvi_postproc(in_file, in_date, out_dir, nestdoms)

!-----------------------------------------------------------------------------
! HAFS DA tool - hafsvi_postproc
! Yonghui Weng, 20220121
!
! This subroutine reads hafs_vi binary output file and merge it to hafs restart files.
! hafs_vi binary output:
!      WRITE(IUNIT) NX,NY,NZ,I360
!      WRITE(IUNIT) LON1,LAT1,LON2,LAT2,CENTRAL_LON,CENTRAL_LAT
!      WRITE(IUNIT) PMID1   
!      WRITE(IUNIT) T1
!      WRITE(IUNIT) Q1
!      WRITE(IUNIT) U1
!      WRITE(IUNIT) V1
!      WRITE(IUNIT) DZDT
!      WRITE(IUNIT) Z1
!!     WRITE(IUNIT) GLON,GLAT
!      WRITE(IUNIT) HLON,HLAT,VLON,VLAT
!      WRITE(IUNIT) P1
!      WRITE(IUNIT) PD1
!      WRITE(IUNIT) ETA1
!      WRITE(IUNIT) ETA2
!
!      ALLOCATE ( T1(NX,NY,NZ),Q1(NX,NY,NZ) )
!      ALLOCATE ( U1(NX,NY,NZ),V1(NX,NY,NZ),DZDT(NX,NY,NZ) )
!      ALLOCATE ( Z1(NX,NY,NZ+1),P1(NX,NY,NZ+1) )
!      ALLOCATE ( GLON(NX,NY),GLAT(NX,NY) )
!      ALLOCATE ( PD1(NX,NY),ETA1(NZ+1),ETA2(NZ+1) )
!      ALLOCATE ( USCM(NX,NY),VSCM(NX,NY) )          ! Env. wind at new grids
!      ALLOCATE ( HLON(NX,NY),HLAT(NX,NY) )
!      ALLOCATE ( VLON(NX,NY),VLAT(NX,NY) )
!      ALLOCATE ( PMID1(NX,NY,NZ),ZMID1(NX,NY,NZ) )

!-----------------------------------------------------------------------------

  use constants
  use netcdf
  use module_mpi
  use var_type

  implicit none

  character (len=*), intent(in) :: in_file,  & ! The VI output binary file on 30x30degree
                                   in_date,  & ! HAFS_restart file date, like 20200825.120000
                                   out_dir     ! HAFS_restart_folder, which holds grid_spec.nc, fv_core.res.tile1.nc,
                                               ! fv_srf_wnd.res.tile1.nc, fv_tracer.res.tile1.nc, phy_data.nc, sfc_data.nc
  integer, intent(in)           :: nestdoms

  type(grid2d_info)             :: ingrid   ! hafs restart grid
  type(grid2d_info)             :: dstgrid  ! rot-ll grid for output

!----for hafs restart
  integer  :: ix, iy, iz, kz, ndom, nd
  character(len=2500)     :: ncfile
  character (len=2500)   :: ncfile_fvcore, ncfile_core, ncfile_tracer, ncfile_phy, ncfile_sfc, ncfile_grid, ncfile_grid2, ncfile_atmos, ncfile_oro
  character (len=50) :: nestfl, tilefl, tempfl
                        ! grid_spec.nc : grid_spec.nest02.tile2.nc
                        ! fv_core.res.tile1.nc : fv_core.res.nest02.tile2.nc
                        ! phy_data.nc  : phy_data.nest02.tile2.nc


!----for hafsvi
  integer  :: nx, ny, nz, i360, filetype  ! filetype: 1=bin, 2=nc
  real     :: lon1,lat1,lon2,lat2,cen_lat,cen_lon,dlat,dlon
  real, allocatable, dimension(:,:) :: hlon, hlat, vlon, vlat

  integer  :: i, j, k, n, flid, ncid, ndims, nrecord, iunit
  real, allocatable, dimension(:,:,:,:) :: dat4, dat41, dat42, dat43, phis1, phis2, sfcp1, sfcp2, u1, v1, u, v
  real, allocatable, dimension(:,:,:)   :: dat3, dat31
  real, allocatable, dimension(:,:)     :: dat2, dat21
  real, allocatable, dimension(:)       :: dat1
  real     :: ptop
  logical  :: file_exist

  real, allocatable, dimension(:,:)     :: cangu, sangu, cangv, sangv

!------------------------------------------------------------------------------
! 1 --- arg process
! 1.1 --- ndom
  ndom=nestdoms+1

!------------------------------------------------------------------------------
! 2 --- input grid info

  !-----------------------------
  !---2.1 get input grid info from binary file
  iunit=36
  open(iunit, file=trim(in_file), form='unformatted')
  read(iunit) nx, ny, nz, i360 
  write(*,'(a,4i5)')'nx, ny, nz, i360 = ',nx, ny, nz, i360
  read(iunit) lon1,lat1,lon2,lat2,cen_lon,cen_lat
  write(*,'(a,6f10.3)')'lon1,lat1,lon2,lat2,cen_lon,cen_lat =', lon1,lat1,lon2,lat2,cen_lon,cen_lat

  !!---add to test vortex-replacement
  !tc%vortexrep=1
  !tc%lat=cen_lat
  !tc%lon=cen_lon
  !!---add to test vortex-replacement

  do i = 1, 7
     read(iunit)
  enddo
  allocate(hlon(nx,ny), hlat(nx,ny), vlon(nx,ny), vlat(nx,ny))
  read(iunit)hlon, hlat, vlon, vlat
! -------------------------------------------------------------

  ! For WPAC storms, CONVERT positive values of western hemisphere within the VI domain
  ! into negative value, IF the portion of western hemisphere (e.g., the eastern side of IDL)
  ! is included in VI domain, because VI is completed
  if ( cen_lon > 0. ) then
     do j = 1, ny; do i = 1, nx
        if(hlon(i,j).gt.180.0) hlon(i,j)=hlon(i,j)-360.0
        if(vlon(i,j).gt.180.0) vlon(i,j)=vlon(i,j)-360.0
     enddo; enddo
  endif

  ! For CPAC storms located near the east of international date line
  ! CONVERT negative values of eastern hemisphere within the VI domain into positive value
  ! because VI is done
  if ( cen_lon < -140. )then
     do j = 1, ny; do i = 1, nx
        if(hlon(i,j).le.-180.0) hlon(i,j)=hlon(i,j)+360.0
        if(vlon(i,j).le.-180.0) vlon(i,j)=vlon(i,j)+360.0
     enddo; enddo
  endif

! -------------------------------------------------------------
  write(*, '(a,8f10.3)')' hlon,hlat(1,1; nx,1; nx,ny; 1,ny) =', &
                        hlon(1,1), hlat(1,1), hlon(nx,1), hlat(nx,1), hlon(nx,ny), hlat(nx,ny), hlon(1,ny), hlat(1,ny) 
  write(*, '(a,8f10.3)')' vlon,vlat(1,1; nx,1; nx,ny; 1,ny) =', &
                        vlon(1,1), vlat(1,1), vlon(nx,1), vlat(nx,1), vlon(nx,ny), vlat(nx,ny), vlon(1,ny), vlat(1,ny) 

  !-----------------------------
  !---2.2 define input rot-ll grids
  ingrid%grid_x = nx
  ingrid%grid_y = ny
  ingrid%ntime  = 1
  ingrid%grid_xt = nx
  ingrid%grid_yt = ny
  allocate(ingrid%grid_lon (ingrid%grid_x,ingrid%grid_y))
  allocate(ingrid%grid_lont(ingrid%grid_x,ingrid%grid_y))
  ingrid%grid_lon  = hlon
  ingrid%grid_lont = vlon
  allocate(ingrid%grid_lat (ingrid%grid_x,ingrid%grid_y))
  allocate(ingrid%grid_latt(ingrid%grid_x,ingrid%grid_y))
  ingrid%grid_lat  = hlat
  ingrid%grid_latt = vlat

!------------------------------------------------------------------------------
! --- domain loop: from inner domain to outer domain, so the number is from max to 1
  do_nestdom_loop: do nd = ndom, ndom

     !-------------------------------------------------------------------------
     ! 3 --- input file
     !       nestfl, tilefl: ncfile_core, ncfile_tracer, ncfile_grid, ncfile_atmos, ncfile_oro
     write(nestfl,'(a4,i2.2)')'nest',nd
     write(tilefl,'(a4,i0)')'tile',nd
     if ( nd == 1 ) then
        ncfile_grid=trim(out_dir)//'/grid_spec.nc'
        ncfile_grid2=trim(out_dir)//'/grid_mspec_'//in_date(1:4)//'_'//in_date(5:6)//'_'//in_date(7:8)//'_'//in_date(10:11)//'.nc'
        ncfile_oro =trim(out_dir)//'/oro_data.nc'
        ncfile_atmos=trim(out_dir)//'/atmos_static.nc'
        ncfile_fvcore=trim(out_dir)//'/'//trim(in_date)//'.fv_core.res.nc'
        ncfile_core=trim(out_dir)//'/'//trim(in_date)//'.fv_core.res.tile1.nc'
        ncfile_tracer=trim(out_dir)//'/'//trim(in_date)//'.fv_tracer.res.tile1.nc'
        ncfile_phy =trim(out_dir)//'/'//trim(in_date)//'.phy_data.nc'
        ncfile_sfc =trim(out_dir)//'/'//trim(in_date)//'.sfc_data.nc'
     else
        ncfile_grid=trim(out_dir)//'/grid_spec.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
        ncfile_grid2=trim(out_dir)//'/grid_mspec.'//trim(nestfl)//'_'//in_date(1:4)//'_'//in_date(5:6)//'_'//in_date(7:8)//'_'//in_date(10:11)//'.'//trim(tilefl)//'.nc'
        ncfile_oro =trim(out_dir)//'/oro_data.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
        ncfile_atmos=trim(out_dir)//'/atmos_static.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
        ncfile_fvcore=trim(out_dir)//'/'//trim(in_date)//'.fv_core.res.'//trim(nestfl)//'.nc'
        ncfile_core=trim(out_dir)//'/'//trim(in_date)//'.fv_core.res.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
        ncfile_tracer=trim(out_dir)//'/'//trim(in_date)//'.fv_tracer.res.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
        ncfile_phy =trim(out_dir)//'/'//trim(in_date)//'.phy_data.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
        ncfile_sfc =trim(out_dir)//'/'//trim(in_date)//'.sfc_data.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
     endif

     !-------------------------------------------------------------------------
     ! 4 --- input grid info
     !---4.1 read from grid file grid_spec.nc:
     inquire(file=ncfile_grid2, exist=file_exist)
     if ( file_exist ) ncfile_grid = ncfile_grid2

     if ( debug_level > 10 ) write(*,'(a)')' --- read grid info from '//trim(ncfile_grid)
     call rd_grid_spec_data(trim(ncfile_grid), dstgrid)
     ix=dstgrid%grid_xt
     iy=dstgrid%grid_yt
     if ( debug_level > 10 ) then
        write(*,'(a,i,1x,i,1x,f,1x,f,1x,f,1x,f)')' --- dstgrid info: ', ix, iy, &
              dstgrid%grid_lon(int(ix/2), int(iy/2)), dstgrid%grid_lat(int(ix/2), int(iy/2)), &
              dstgrid%grid_lont(int(ix/2), int(iy/2)), dstgrid%grid_latt(int(ix/2), int(iy/2))
     endif

     !-----------------------------
     !---4.2 call FV3-grid cos and sin
     allocate( cangu(ix,iy+1),sangu(ix,iy+1),cangv(ix+1,iy),sangv(ix+1,iy) )
     call cal_uv_coeff_fv3(ix, iy, dstgrid%grid_lat, dstgrid%grid_lon, cangu, sangu, cangv, sangv)

     !-----------------------------
     !---4.3 calculate output-grid in input-grid's positions (xin, yin), and each grid's weight to dst
     call cal_src_dst_grid_weight(ingrid, dstgrid)
 
     !-------------------------------------------------------------------------
     ! 5 --- process record one-by-one
     rewind(iunit)
     do_record_loop: do nrecord = 1, 14 

        !-----------------------------
        !---5.1 read data and derive out the var for restart
        iz=-99

        if ( nrecord == 1 .or. nrecord == 2 .or. nrecord == 3 .or. nrecord == 10 .or. &
             nrecord == 12 .or. nrecord == 13 .or. nrecord == 14 ) then
           !---ignore these records
           !---record 1 : nx, ny, nz, i360
           !---record 2 : lon1,lat1,lon2,lat2,cen_lon,cen_lat
           !---record 3 : pmid1(nx,ny,nz): pressure on full level
           !---                ignore, we use p1 to derive delp.
           !---record 10: hlon, hlat, vlon, vlat 
           !---record 12: pd1,  PD1(NX,NY): surface pressure
           !---record 13: eta1, ETA1(NZ+1)
           !---record 14: eta2, ETA2(NZ+1)
           if ( nrecord == 12 ) then
              allocate(dat2(nx,ny))
              read(iunit)dat2
              !write(*,'(a,200f10.1)')'pd1: ',dat2(int(nx/2),int(ny/2))
              deallocate(dat2)
           elseif ( nrecord == 13 .or. nrecord == 14 ) then
              allocate(dat1(nz+1))
              read(iunit)dat1
              !if ( nrecord == 13 ) write(*,'(a3,i1,a,200f10.1)')'eta',nrecord-12,': ',dat1
              !if ( nrecord == 14 ) write(*,'(a3,i1,a,200f10.6)')'eta',nrecord-12,': ',dat1
              deallocate(dat1) 
           else
              read(iunit)
           endif
        endif  
  
        !  ALLOCATE ( T1(NX,NY,NZ),Q1(NX,NY,NZ) )
        !  ALLOCATE ( U1(NX,NY,NZ),V1(NX,NY,NZ),DZDT(NX,NY,NZ) )
        !  ALLOCATE ( Z1(NX,NY,NZ+1),P1(NX,NY,NZ+1) )
        if ( nrecord == 6 ) then   !u,v - 6,7
           iz=nz
           allocate(dat3(nx,ny,iz), u1(nx,ny,iz,1), v1(nx,ny,iz,1))
           read(iunit) dat3
           do k = 1, nz
              u1(:,:,nz-k+1,1)=dat3(:,:,k)
           enddo
           read(iunit) dat3
           do k = 1, nz
              v1(:,:,nz-k+1,1)=dat3(:,:,k)
           enddo
           deallocate(dat3)
        endif  !if ( nrecord == 6 ) then   !u,v - 6,7

        if ( nrecord == 4 .or. nrecord == 5 .or. nrecord == 8 .or. &
             nrecord == 9 .or. nrecord == 11 ) then
           !---record 4 : t1-->T
           !---record 5 : Q1
           !---record 6 : U1
           !---record 7 : V1
           !---record 8 : DZDT
           !---record 9 : z1 --> DZ
           !---record 11: p1-->delp, p1(nx,ny,nz+1): (((p1(i,j,k),i=1,nx),j=1,ny),k=nz+1,1,-1)
           !---           p1-->ps
           iz=nz
           if ( nrecord == 9 .or. nrecord == 11 ) then
              allocate(dat3(nx,ny,iz+1))
           else
              allocate(dat3(nx,ny,iz))
           endif
           read(iunit) dat3
           allocate(dat41(nx,ny,iz,1)) 
           
           if ( nrecord == 9 .or. nrecord == 11 ) then  ! z1 to dz; p1 to delp
              !---back pressure to delp on fv_core.res.tile1.nc
              do k = 1, nz
                 dat41(:,:,nz-k+1,1)=dat3(:,:,k)-dat3(:,:,k+1)
              enddo
              !---phis
              if ( nrecord == 9 ) then
                 allocate(phis1(nx,ny,1,1))
                 phis1(:,:,1,1)=dat3(:,:,1)*g
              endif
           else
              do k = 1, iz
                 dat41(:,:,iz-k+1,1)=dat3(:,:,k)
              enddo
           endif
           deallocate(dat3)
        endif 

        !-----------------------------
        !---5.2 merge hafs restart and update restart files
        !---    note: need to change nesting domain's filenames
        if ( nrecord == 4) then  !T
           allocate(dat42(ix, iy, iz, 1), dat43(ix, iy, iz, 1))
           call get_var_data(trim(ncfile_core), 'T', ix, iy, iz,1, dat42) 
           call combine_grids_for_remap(nx,ny,nz,1,dat41,ix,iy,iz,1,dat42,gwt%gwt_t,dat43)
           call update_hafs_restart(trim(ncfile_core), 'T', ix, iy, iz, 1, dat43)
           deallocate(dat41, dat42, dat43)
        elseif ( nrecord == 5 ) then  !sphum
           allocate(dat42(ix, iy, iz, 1), dat43(ix, iy, iz, 1))
           call get_var_data(trim(ncfile_tracer), 'sphum', ix, iy, iz,1, dat42)
           call combine_grids_for_remap(nx,ny,nz,1,dat41,ix,iy,iz,1,dat42,gwt%gwt_t,dat43)
           call update_hafs_restart(trim(ncfile_tracer), 'sphum', ix, iy, iz, 1, dat43)
           deallocate(dat41, dat42, dat43)
        elseif ( nrecord == 6 ) then  !u and v
           !---get u,v
           allocate(u(ix, iy+1, iz, 1))
           call get_var_data(trim(ncfile_core), 'u', ix, iy+1, iz,1, u)
           allocate(v(ix+1, iy, iz, 1))
           call get_var_data(trim(ncfile_core), 'v', ix+1, iy, iz,1, v)

           !---convert fv3grid to earth
           allocate(dat42(ix, iy+1, iz, 1), dat43(ix+1, iy, iz, 1))
           do k = 1, iz
              call fv3uv2earth(ix, iy, u(:,:,k,1), v(:,:,k,1), cangu, sangu, cangv, sangv, dat42(:,:,k,1), dat43(:,:,k,1))
           enddo

           !---merge
           u=0.; v=0.
           call combine_grids_for_remap(nx,ny,nz,1,u1,ix,iy+1,iz,1,dat42,gwt%gwt_u,u)
           call combine_grids_for_remap(nx,ny,nz,1,v1,ix+1,iy,iz,1,dat43,gwt%gwt_v,v)

           !---convert earth wind to fv3grid wind
           dat42=-999999.; dat43=-99999999.;
           do k = 1, iz
              call earthuv2fv3(ix, iy, u(:,:,k,1), v(:,:,k,1), cangu, sangu, cangv, sangv, dat42(:,:,k,1), dat43(:,:,k,1))
           enddo

           !---output
           call update_hafs_restart(trim(ncfile_core), 'u', ix, iy+1, iz, 1, dat42)
           call update_hafs_restart(trim(ncfile_core), 'v', ix+1, iy, iz, 1, dat43)
           deallocate(u1, v1, u, v, dat42, dat43, cangu, sangu, cangv, sangv)
        elseif ( nrecord == 8 ) then  !W
           allocate(dat42(ix, iy, iz, 1), dat43(ix, iy, iz, 1))
           call get_var_data(trim(ncfile_core), 'W', ix, iy, iz,1, dat42)
           call combine_grids_for_remap(nx,ny,nz,1,dat41,ix,iy,iz,1,dat42,gwt%gwt_t,dat43)
           call update_hafs_restart(trim(ncfile_core), 'W', ix, iy, iz, 1, dat43)
           deallocate(dat41, dat42, dat43)
        elseif ( nrecord == 9 ) then  !DZ, phis
           allocate(dat42(ix, iy, iz, 1), dat43(ix, iy, iz, 1))
           call get_var_data(trim(ncfile_core), 'DZ', ix, iy, iz,1, dat42)
           call combine_grids_for_remap(nx,ny,nz,1,dat41,ix,iy,iz,1,dat42,gwt%gwt_t,dat43)
           call update_hafs_restart(trim(ncfile_core), 'DZ', ix, iy, iz, 1, dat43)
           deallocate(dat41, dat42, dat43)
           allocate(phis2(ix, iy, 1, 1), dat43(ix, iy, 1, 1))
           call get_var_data(trim(ncfile_core), 'phis', ix, iy, 1, 1, phis2)
           call combine_grids_for_remap(nx,ny,1,1,phis1,ix,iy,1,1,phis2,gwt%gwt_t,dat43)
           call update_hafs_restart(trim(ncfile_core), 'phis', ix, iy, 1, 1, dat43)
           deallocate(phis1, phis2, dat43)
        elseif ( nrecord == 11 ) then  !delp
           allocate(dat42(ix, iy, iz, 1), dat43(ix, iy, iz, 1))
           call get_var_data(trim(ncfile_core), 'delp', ix, iy, iz,1, dat42)
           call combine_grids_for_remap(nx,ny,nz,1,dat41,ix,iy,iz,1,dat42,gwt%gwt_t,dat43)
           call update_hafs_restart(trim(ncfile_core), 'delp', ix, iy, iz, 1, dat43)
           deallocate(dat41, dat42, dat43)
        endif
        
     enddo do_record_loop

     !-----------------------------
     ! 6 --- clean 
     deallocate( dstgrid%grid_lon, dstgrid%grid_lat, dstgrid%grid_lont, dstgrid%grid_latt)
     deallocate( gwt%gwt_t, gwt%gwt_u, gwt%gwt_v )

  enddo do_nestdom_loop !: do nd = 1, ndom
  close(iunit)
  write(*,*)'--- hafsvi_postproc completed ---'

  return
  end subroutine hafsvi_postproc

!========================================================================================
