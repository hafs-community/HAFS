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
  character (len=2500)   :: infile_fvcore, infile_core, infile_tracer, infile_phy, &
                            infile_sfc, infile_grid, infile_grid2, infile_atmos, infile_oro
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
  integer :: io_proc, nm, ks, ke, nv


!------------------------------------------------------------------------------
! 1 --- arg process
  io_proc=nprocs-1
  !io_proc=0
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
        if ( my_proc_id == 0 ) write(*,'(a)')'!!! hafsvi cut radius number wrong: '//trim(radius)
        if ( my_proc_id == 0 ) write(*,'(a)')'!!! please call with --vortexradius=40 (75< 3)'
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
  !$omp parallel do &
  !$omp& private(i,j,rot_lon,rot_lat)
  do j = 1, ny; do i = 1, nx
     rot_lon = lon1 + dlon*(i-1)
     rot_lat = lat1 + dlat*(j-1)
     call rtll(rot_lon, rot_lat, glon(i,j), glat(i,j), cen_lon, cen_lat)
  enddo; enddo
  if ( my_proc_id == 0 ) write(*,'(a)')'---rot-ll grid: nx, ny, cen_lon, cen_lat, dlon, dlat, lon1, lon2, lat1, lat2'
  if ( my_proc_id == 0 ) write(*,'(15x,2i5,8f10.5)')    nx, ny, cen_lon, cen_lat, dlon, dlat, lon1, lon2, lat1, lat2
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

     if ( debug_level > 10 .and. my_proc_id == 0 ) write(*,'(a)')' --- read grid info from '//trim(infile_grid)
     call rd_grid_spec_data(trim(infile_grid), ingrid)
     ix=ingrid%grid_xt
     iy=ingrid%grid_yt
     if ( debug_level > 10 .and. my_proc_id == 0 ) then
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
        if ( my_proc_id == 0 ) write(*,'(a)')' --- call cal_src_dst_grid_weight'
        write(*,'(i,a,2(i,1x),2(f,1x))')my_proc_id,' --- dstgrid: ', nx, ny, &
             dstgrid%grid_lont(int(nx/2),int(ny/2)), dstgrid%grid_latt(int(nx/2),int(ny/2))
     endif
     call cal_src_dst_grid_weight(ingrid, dstgrid)

     !-------------------------------------------------------------------------
     ! 6 --- dst files
     if ( my_proc_id == io_proc ) then
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
     endif

     !-------------------------------------------------------------------------
     ! 7 --- output
     do_out_var_loop: do nrecord = 1, 17
        !write(*,*)my_proc_id, '=== nrecord =',nrecord
        !-----------------------------
        !---7.1 record 1: nx, ny, nz
        !---nx, ny, nz, & lon1,lat1,lon2,lat2,cen_lon,cen_lat
        call cpu_time(cputime1)
        if ( my_proc_id == io_proc ) write(*,'(a,i3,f)')' --- record start cputime: ', nrecord, cputime1
        if ( nrecord == 1 ) then
           call get_var_dim(trim(infile_atmos), 'pfull', ndims, dims)
           nz=dims(1)

           if ( my_proc_id == io_proc ) write(*,'(a,3i6)')'=== record1: ',nx, ny, nz
           if ( my_proc_id == io_proc ) write(flid_out) nx, ny, nz
           if ( nd > 1 .and. my_proc_id == io_proc ) read(flid_in)
        endif

        iz=nz   !same vertical levels
        if ( nrecord == 12 .or. nrecord == 15 .or. nrecord == 16 .or. nrecord ==17 ) iz=1
        if ( nrecord ==  9 .or. nrecord == 11 ) iz=nz+1

        !-----------------------------
        !---7.2 record 2: lon1,lat1,lon2,lat2,cen_lon,cen_lat
        if ( nrecord == 2 ) then
           write(*,'(i,a,6f8.3)')my_proc_id, '=== record2: ',lon1,lat1,lon2,lat2,cen_lon,cen_lat
           if ( my_proc_id == io_proc ) write(*,'(a,6f8.3)')'=== record2: ',lon1,lat1,lon2,lat2,cen_lon,cen_lat
           if ( my_proc_id == io_proc ) write(flid_out) lon1,lat1,lon2,lat2,cen_lon,cen_lat
           if ( nd > 1 .and. my_proc_id == io_proc ) read(flid_in)
           write(*,*)'==== finished record 2 at ', my_proc_id
        endif

        !-----------------------------
        !---7.3 record 3: (((pf1(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        !---     hafs-VI/read_hafs_out.f90 pf:
        !---          ph(k) = ak(k) + bk(k)*p_s --> half level pressure
        !---          pf(k) = (ph(k+1) - ph(k)) / log(ph(k+1)/ph(k)) --> full level pressure
        !---
        !---seem pf1 is pressure on full level, use
        !---    pf1(k) = phalf(1) + sum(delp(1:k))
        if ( nrecord == 3 .or. nrecord == 9 .or. nrecord == 11 ) then
           if ( nrecord == 3 ) then
              allocate(dat4(iz+1,1,1,1))
              call get_var_data(trim(infile_atmos), 'phalf', iz+1, 1, 1, 1, dat4)
              ptop=dat4(1,1,1,1)*100.  !phalf:units = "mb" ;
              deallocate(dat4)
           endif

           if ( my_proc_id == io_proc ) then
              if ( nrecord == 3 ) then
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
                 deallocate(dat2, dat4)
              else if ( nrecord == 9 ) then
                 allocate(dat4(ix, iy, 1,1))
                 call get_var_data(trim(infile_core), 'phis', ix, iy, 1, 1, dat4)
                 allocate(dat41(ix, iy, iz, 1))
                 dat41(:,:,iz,1)=dat4(:,:,1,1)/g
                 deallocate(dat4)
                 allocate(dat4(ix, iy, iz-1, 1))
                 call get_var_data(trim(infile_core), 'DZ', ix, iy, iz-1, 1, dat4)
                 do k = iz-1, 1, -1
                    dat41(:,:,k,1)=dat41(:,:,k+1,1)-dat4(:,:,k,1)
                 enddo
                 deallocate(dat4)
              else if ( nrecord == 11 ) then
                 allocate(dat4(ix, iy, iz-1, 1), dat41(ix, iy, iz, 1))
                 call get_var_data(trim(infile_core), 'delp', ix, iy, iz-1, 1, dat4)
                 dat41(:,:,1,1)=ptop
                 do k = 2, iz
                    dat41(:,:,k,1)=dat41(:,:,k-1,1)+dat4(:,:,k-1,1)
                 enddo
                 deallocate(dat4)
              endif  !if ( nrecord == 3 ) then

              !---broadcast dat41 to each computing-core
              if ( nprocs > 1 ) then
                 nm=max(1,int((iz+nprocs-1)/nprocs))  !devide iz to each processor
                 do k = 0, nprocs-1
                    ks=k*nm+1              !k-start
                    ke=k*nm+nm            !k-end
                    if ( ke > iz ) ke=iz
                    if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                       allocate(dat42(ix, iy, ke-ks+1,1))
                       dat42(:,:,:,1)=dat41(:,:,ks:ke,1)
                       if ( k /= io_proc ) then
                          call mpi_send(dat42(1,1,1,1), size(dat42), mpi_real, k, 3000+ks, comm, ierr)
                       else
                          allocate(dat43(ix, iy, ke-ks+1,1))
                          dat43=dat42
                       endif
                       deallocate(dat42)
                    endif
                 enddo
              else  !if ( nprocs > 1 ) then
                 allocate(dat43(ix, iy, iz,1))
                 dat43=dat41
              endif
              deallocate(dat41)
           else ! if ( my_proc_id == io_proc ) then
              !---receive dat43
              nm=max(1,int((iz+nprocs-1)/nprocs))
              ks=min(iz,my_proc_id*nm+1)
              ke=min(iz,my_proc_id*nm+nm)
              if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                 allocate(dat43(ix, iy, ke-ks+1,1))
                 !call mpi_recv(dat43(1,1,1,1), size(dat43), mpi_real, io_proc, mpi_any_tag, comm, status, ierr)
                 call mpi_recv(dat43(1,1,1,1), size(dat43), mpi_real, io_proc, 3000+ks, comm, status, ierr)
              endif
           endif
        endif

        !-----------------------------
        !---7.4 record 4: (((tmp(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        !---7.5 record 5: (((spfh(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        !---7.8 record 8: (((dzdt(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        if ( nrecord == 4 .or. nrecord == 5 .or. nrecord == 8 ) then
           if ( my_proc_id == io_proc ) then
              !---read in
              allocate(dat4(ix, iy, iz,1))
              if ( nrecord == 4 ) call get_var_data(trim(infile_core), 'T', ix, iy, iz,1, dat4)
              if ( nrecord == 5 ) call get_var_data(trim(infile_tracer), 'sphum', ix, iy, iz,1, dat4)
              if ( nrecord == 8 ) call get_var_data(trim(infile_core), 'W', ix, iy, iz,1, dat4)
              !---send to other core
              if ( nprocs > 1 ) then
                 nm=max(1,int((iz+nprocs-1)/nprocs))
                 do k = 0, nprocs-1
                    ks=k*nm+1
                    ke=k*nm+nm            !k-end
                    if ( ke > iz ) ke=iz
                    if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                       allocate(dat42(ix, iy, ke-ks+1,1))
                       dat42(:,:,:,1)=dat4(:,:,ks:ke,1)
                       if ( k /= io_proc ) then
                          call mpi_send(dat42(1,1,1,1), size(dat42), mpi_real, k, 4000+ks, comm, ierr)
                       else
                          allocate(dat43(ix, iy, ke-ks+1,1))
                          dat43=dat42
                       endif
                       deallocate(dat42)
                    endif
                 enddo
              else
                 allocate(dat43(ix, iy, iz,1))
                 dat43=dat4
              endif
              deallocate(dat4)
           else  !if ( my_proc_id == io_proc ) then
              !---receive dat43
              nm=max(1,int((iz+nprocs-1)/nprocs))
              ks=min(iz,my_proc_id*nm+1)
              ke=min(iz,my_proc_id*nm+nm)
              if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                 allocate(dat43(ix, iy, ke-ks+1,1))
                 call mpi_recv(dat43(1,1,1,1), size(dat43), mpi_real, io_proc, 4000+ks, comm, status, ierr)
              endif
           endif  !if ( my_proc_id == io_proc ) then
        endif

        !-----------------------------
        !---7.6 record 6: (((ugrd(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        !---7.7 record 7: (((vgrd(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        if ( nrecord == 6 ) then
           !---get u,v from restart
           if ( my_proc_id == io_proc ) then
              do nv = 1, 2
                 if (nv==1) then
                    allocate(dat4(ix, iy+1, iz,1))
                    call get_var_data(trim(infile_core), 'u', ix, iy+1, iz, 1, dat4)
                 else if (nv==2) then
                    allocate(dat4(ix+1, iy, iz,1))
                    call get_var_data(trim(infile_core), 'v', ix+1, iy, iz, 1, dat4)
                 endif
                 !---send to other core
                 if ( nprocs > 1 ) then
                    nm=max(1,int((iz+nprocs-1)/nprocs))
                    do k = 0, nprocs-1
                       ks=k*nm+1
                       ke=k*nm+nm            !k-end
                       if ( ke > iz ) ke=iz
                       if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                          if (nv==1) then
                             allocate(dat41(ix, iy+1, ke-ks+1,1))
                          else if (nv==2) then
                             allocate(dat41(ix+1, iy, ke-ks+1,1))
                          endif
                          dat41(:,:,:,1)=dat4(:,:,ks:ke,1)
                          if ( k /= io_proc ) then
                             call mpi_send(dat41(1,1,1,1), size(dat41), mpi_real, k, 200*nv+ks, comm, ierr)
                          else
                             if (nv==1) then
                                allocate(dat42(ix, iy+1, ke-ks+1,1))
                                dat42=dat41
                             else if (nv==2) then
                                allocate(dat43(ix+1, iy, ke-ks+1,1))
                                dat43=dat41
                             endif
                          endif
                          deallocate(dat41)
                       endif
                    enddo
                 else  !if ( nprocs > 1 ) then
                    if (nv==1) then
                       allocate(dat42(ix, iy+1, iz,1))
                       dat42=dat4
                    else if (nv==2) then
                       allocate(dat43(ix+1, iy, iz,1))
                       dat43=dat4
                    endif
                 endif
                 deallocate(dat4)
              enddo  !do nv = 1, 2
           else  !if ( my_proc_id == io_proc ) then
              !---receive dat42 dat43
              nm=max(1,int((iz+nprocs-1)/nprocs))
              ks=min(iz,my_proc_id*nm+1)
              ke=min(iz,my_proc_id*nm+nm)
              if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                 allocate(dat42(ix, iy+1, ke-ks+1,1), dat43(ix+1, iy, ke-ks+1,1))
                 call mpi_recv(dat42(1,1,1,1), size(dat42), mpi_real, io_proc, 200*1+ks, comm, status, ierr)
                 call mpi_recv(dat43(1,1,1,1), size(dat43), mpi_real, io_proc, 200*2+ks, comm, status, ierr)
              endif
           endif  !if ( my_proc_id == io_proc ) then
           !write(*,*)'===w11 distributed u,v @ dat42,dat43'
        endif

        !!-----------------------------
        !!---7.9 record 9: (((z1(i,j,k),i=1,nx),j=1,ny),k=nz1,1,-1)
        !!---     hafs-VI/read_hafs_out.f90 z1:
        !!---          z1(I,J,K)=z1(I,J,K+1)+rdgas1*tmp(i,j,k)*(1.+0.608*spfh(i,j,k))*ALOG(ph1(i,j,k+1)/ph1(i,j,k))
        !!--- hgt: phis/g-sum(DZ)
        !if ( nrecord == 9 ) then
        !   allocate(dat4(ix, iy, 1,1))
        !   call get_var_data(trim(infile_core), 'phis', ix, iy, 1, 1, dat4)
        !   allocate(dat41(ix, iy, iz+1, 1))
        !   dat41(:,:,iz+1,1)=dat4(:,:,1,1)/g
        !   deallocate(dat4)

        !   allocate(dat4(ix, iy, iz, 1))
        !   call get_var_data(trim(infile_core), 'DZ', ix, iy, iz, 1, dat4)
        !   do k = iz, 1, -1
        !      dat41(:,:,k,1)=dat41(:,:,k+1,1)-dat4(:,:,k,1)
        !   enddo
        !   !write(*,'(a,200f)')'z1: ',dat41(int(ix/2),int(iy/2),:,1)
        !   deallocate(dat4)
        !endif

        !-----------------------------
        !---7.10 record 10: glon,glat,glon,glat   ! 2D
        !--- glat=grid_yt*180./pi, grid_yt=1:2160, what is this?
        if ( nrecord == 10 ) then
           if ( my_proc_id == io_proc ) then
              write(*,'(a,4f8.3)')'=== record10: ',glon(1,1), glat(1,1), glon(nx,ny), glat(nx,ny)

! New change: Jul 2022 JH Shin-------------------------------------------------------------

          !For WPAC storms located near the west of international date line, add 360 to
          !longitude value in the eastern side of IDL (western hemisphere) so that all
          !longitude values have positive values in the VI domain.
          !if ( cen_lon > 0. ) then
          ! do j = 1, ny; do i = 1, nx
          !  if(glon(i,j).lt.0.0) glon(i,j)=glon(i,j)+360.0
          ! enddo; enddo
          !endif
          if ( cen_lon > 0. ) where ( glon < 0.) glon=glon+360.

          !For CPAC storms located near the east of international date line, subrtact 360
          !from longitude value in the western side of IDL (eastern hemisphere) so
          !that all longitude values have negative values in the VI domain.
          !if ( cen_lon < -140. )then
          ! do j = 1, ny; do i = 1, nx
          !  if(glon(i,j).gt.0.0) glon(i,j)=glon(i,j)-360.0
          ! enddo; enddo
          !endif
          if ( cen_lon < -140. ) where ( glon > 0. ) glon=glon-360.

! New change: Jul 2022 JH Shin-------------------------------------------------------------

              write(flid_out) glon,glat,glon,glat
              if ( nd > 1 ) read(flid_in)
           endif
        endif

        !!-----------------------------
        !!---7.11 record 11: (((ph1(i,j,k),i=1,nx),j=1,ny),k=nz1,1,-1)
        !!---     hafs-VI/read_hafs_out.f90 ph:
        !!---       ph(k) = ak(k) + bk(k)*p_s --> pressure in pa
        !!---       64.270-->100570
        !!---seem ph1 is pressure on half level, use
        !!---    pf1(k) = phalf(1) + sum(delp(1:k))
        !if ( nrecord == 11 ) then
        !   allocate(dat4(iz+1,1,1,1))
        !   call get_var_data(trim(infile_atmos), 'phalf', iz+1, 1, 1, 1, dat4)
        !   ptop=dat4(1,1,1,1)*100.  !phalf:units = "mb" ;
        !   deallocate(dat4)

        !   allocate(dat4(ix, iy, iz,1))
        !   allocate(dat41(ix, iy, iz+1,1))
        !   call get_var_data(trim(infile_core), 'delp', ix, iy, iz,1, dat4)
        !   dat41(:,:,1,1)=ptop
        !   do k = 1, iz
        !      dat41(:,:,k+1,1)=dat41(:,:,k,1)+dat4(:,:,k,1)
        !   enddo
        !   deallocate(dat4)
        !endif

        !-----------------------------
        !---7.12 record 12: pressfc1              ! 2D
        !--- use lowest-level pressure?
        if ( nrecord == 12 ) then
           if ( my_proc_id == io_proc ) then
              allocate(dat43(ix, iy, 1, 1))
              dat43(:,:,1,1)=sfcp(:,:)
              deallocate(sfcp)
           endif
        endif

        !-----------------------------
        !---7.13 record 13: ak
        if ( nrecord == 13 ) then
           if ( my_proc_id == io_proc ) then
              allocate(dat4(iz+1,1,1,1))
              call get_var_data(trim(infile_fvcore), 'ak', iz+1, 1, 1, 1, dat4)
              write(*,'(a,200f12.1)')'=== record13: ', (dat4(k,1,1,1),k=1,iz+1)
              write(flid_out) (dat4(k,1,1,1),k=1,iz+1)
              if ( nd > 1 ) read(flid_in)
              deallocate(dat4)
           endif
        endif

        !-----------------------------
        !---7.14 record 14: bk
        if ( nrecord == 14 ) then
           if ( my_proc_id == io_proc ) then
              allocate(dat4(iz+1,1,1,1))
              call get_var_data(trim(infile_fvcore), 'bk', iz+1, 1, 1, 1, dat4)
              write(*,'(a,200f10.3)')'=== record14: ', (dat4(k,1,1,1),k=1,iz+1)
              write(flid_out) (dat4(k,1,1,1),k=1,iz+1)
              if ( nd > 1 ) read(flid_in)
              deallocate(dat4)
           endif
        endif

        !-----------------------------
        !---7.15 record 15: land                  ! =A101 = land sea mask, B101 = ZNT
        !---     hafs-VI/read_hafs_out.f90 land:long_name = "sea-land-ice mask (0-sea, 1-land, 2-ice)" ;
        !--- sfc_data.nc: slmsk
        if ( nrecord == 15 ) then
           if ( my_proc_id == io_proc ) then
              allocate(dat43(ix, iy, 1,1))
              call get_var_data(trim(infile_sfc), 'slmsk', ix, iy, 1, 1, dat43)
           endif
        endif

        !-----------------------------
        !---7.16 record 16: sfcr                  ! =B101 = Z0
        !---surface roughness
        if ( nrecord == 16 ) then
           if ( my_proc_id == io_proc ) then
              allocate(dat43(ix, iy, 1,1))
              call get_var_data(trim(infile_sfc), 'zorl', ix, iy, 1, 1, dat43)
              ! convert from cm to m
              dat43=dat43/100.
           endif
        endif

        !-----------------------------
        !---7.17 record 17: C101                  ! =C101 = (10m wind speed)/(level 1 wind speed)
        !---                                      ! =C101 = f10m (in the sfc_data.nc)
        if ( nrecord == 17 ) then
           if ( my_proc_id == io_proc ) then
              allocate(dat43(ix, iy, 1, 1))
              call get_var_data(trim(infile_sfc), 'f10m', ix, iy, 1, 1, dat43)
           endif
        endif

        !-----------------------------
        !---7.18 output 3d
        if ( nrecord == 3 .or. nrecord == 4 .or. nrecord == 5 .or. &
             nrecord == 8 .or. nrecord == 9 .or. nrecord ==11 )then
           call mpi_barrier(comm,ierr)
           kz=nz
           if ( nrecord ==  9 .or. nrecord == 11 ) then
              kz=nz+1
           endif
           !--- map fv3 grid to rot-ll grid: ingrid-->dstgrid
           !call cpu_time(cputime2)
           !write(*,'(a,i3,f)')' --- read rot-ll grid for 1 record ', nrecord, cputime2

           if ( nprocs == 1 ) then  !--no mpi
              !----only 1-core
              allocate(dat41(nx,ny,kz,1), dat42(nx,ny,kz,1))
              if ( nd > 1 ) then
                 read(flid_in)dat42
                 do k = 1, kz
                     dat41(:,:,k,1)=dat42(:,:,kz-k+1,1)
                 enddo
                 dat42=-999999.
              else
                 dat41=-999999.
              endif
              call combine_grids_for_remap(ix,iy,kz,1,dat43,nx,ny,kz,1,dat41,gwt%gwt_t,dat42)

              !--- output
              !write(*,'(a,i2.2,a,200f)')'=== record',nrecord,': ', dat42(int(nx/2),int(ny/2),:,1)
              write(flid_out) (((dat42(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
              deallocate(dat41, dat42, dat43)
           else
              !----mpi: 0 is for IO, >0 is for computing

              !---when nd>1, get previous data at 0, and then send to other cores
              if ( nd > 1 ) then
                 if ( my_proc_id == io_proc ) then
                    allocate(dat4(nx,ny,kz,1), dat42(nx,ny,kz,1))
                    read(flid_in)dat42
                    do k = 1, kz
                        dat4(:,:,k,1)=dat42(:,:,kz-k+1,1)
                    enddo
                    deallocate(dat42)
                    nm=max(1,int((kz+nprocs-1)/nprocs))
                    do k = 0, nprocs-1
                       ks=k*nm+1
                       ke=k*nm+nm
                       if ( ke > iz ) ke=iz
                       if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                          allocate(dat42(nx, ny, ke-ks+1,1))
                          dat42(:,:,:,1)=dat4(:,:,ks:ke,1)
                          if ( k /= io_proc ) then
                             call mpi_send(dat42(1,1,1,1),size(dat42),mpi_real, k, 4000+ks, comm, ierr)
                          else
                             allocate(dat41(nx, ny, ke-ks+1,1))
                             dat41=dat42
                          endif
                          deallocate(dat42)
                       endif
                    enddo
                    deallocate(dat4)
                 else  !if ( my_proc_id == io_proc ) then
                    !---receive dat42 for each core
                    nm=max(1,int((kz+nprocs-1)/nprocs))
                    ks=my_proc_id*nm+1
                    ke=my_proc_id*nm+nm
                    if ( ke > kz ) ke=kz
                    if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                       allocate(dat41(nx, ny, ke-ks+1,1))
                       call mpi_recv(dat41(1,1,1,1), size(dat41), mpi_real, io_proc, 4000+ks, comm, status, ierr)
                    endif
                 endif  !if ( my_proc_id == io_proc ) then
              else  !if ( nd > 1 ) then
                 nm=max(1,int((kz+nprocs-1)/nprocs))
                 ks=my_proc_id*nm+1
                 ke=my_proc_id*nm+nm
                 if ( ke > kz ) ke=kz
                 if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                    allocate(dat41(nx, ny, ke-ks+1,1))
                    dat41=-999999.
                 endif
              endif  !if ( nd > 1 ) then

              !---combine dat43+dat41 --> dat42
              !call mpi_barrier(comm,ierr)
              nm=max(1,int((kz+nprocs-1)/nprocs))
              ks=my_proc_id*nm+1
              ke=my_proc_id*nm+nm
              if ( ke > kz ) ke=kz
              if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                 allocate(dat42(nx, ny, ke-ks+1,1))
                 dat42=-999999.
                 call combine_grids_for_remap(ix,iy,ke-ks+1,1,dat43,nx,ny,ke-ks+1,1,dat41,gwt%gwt_t,dat42)
                 if ( my_proc_id /= io_proc ) then
                    call mpi_send(dat42(1,1,1,1),size(dat42),mpi_real, io_proc, 5000+ks, comm, ierr)
                    deallocate(dat42)
                 endif
                 deallocate(dat41, dat43)
              endif

              !---collect dat43 to io_proc, and output
              !call mpi_barrier(comm,ierr)
              if ( my_proc_id == io_proc ) then
                 allocate(dat43(nx,ny,kz,1))
                 nm=max(1,int((kz+nprocs-1)/nprocs))
                 do k = 0, nprocs-1
                    ks=k*nm+1
                    ke=k*nm+nm
                    if ( ke > kz ) ke=kz
                    if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                       if ( k /= io_proc ) then
                          allocate(dat41(nx, ny, ke-ks+1,1))
                          call mpi_recv(dat41(1,1,1,1), size(dat41), mpi_real, k, 5000+ks, comm, status, ierr)
                          dat43(:,:,ks:ke,1)=dat41(:,:,1:ke-ks+1,1)
                          deallocate(dat41)
                       else
                          dat43(:,:,ks:ke,1)=dat42(:,:,1:ke-ks+1,1)
                          deallocate(dat42)
                       endif
                    endif
                 enddo
                 !write(*,'(a,3i5,100f12.3)')'===w34 ', nx, ny, kz, (dat43(10,10,k,1),k=kz,1,-1)
                 write(flid_out) (((dat43(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
                 deallocate(dat43)
              endif  !if ( my_proc_id == io_proc ) then
           endif  ! if ( nprocs == 1 ) then  !--no mpi
        else if ( nrecord == 6 ) then  !---u,v
           kz=nz
           !---convert u,v from fv3grid to earth
           nm=max(1,int((kz+nprocs-1)/nprocs))
           ks=my_proc_id*nm+1; ke=my_proc_id*nm+nm
           if ( ke > kz ) ke=kz
           if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
              allocate(u(ix,iy,ke-ks+1,1), v(ix,iy,ke-ks+1,1))
              allocate(dat2(ix, iy+1), dat21(ix+1, iy))
              do k = 1, ke-ks+1
                 call fv3uv2earth(ix, iy, dat42(:,:,k,1), dat43(:,:,k,1), cangu, sangu, cangv, sangv, dat2, dat21)
                 !---destage: C-/D- grid to A-grid
                 u(:,:,k,1) = (dat2 (:,1:iy)+dat2 (:,2:iy+1))/2.0
                 v(:,:,k,1) = (dat21(1:ix,:)+dat21(2:ix+1,:))/2.0
              enddo
              deallocate(dat42, dat43, dat2, dat21, cangu, sangu, cangv, sangv)
           endif
           !write(*,*)'===w12 dat42,dat43 to earth wind u,v'

           !--- loop u,v
           do nv = 1, 2
              !--- get outer domain u,v
              if ( nd > 1 ) then
                 if ( my_proc_id == io_proc ) then
                    allocate(dat4(nx,ny,kz,1), dat42(nx,ny,kz,1))
                    read(flid_in)dat42
                    do k = 1, kz
                       dat4(:,:,k,1)=dat42(:,:,kz-k+1,1)
                    enddo
                    deallocate(dat42)
                    nm=max(1,int((kz+nprocs-1)/nprocs))
                    do k = 0, nprocs-1
                       ks=k*nm+1
                       ke=k*nm+nm
                       if ( ke > iz ) ke=iz
                       if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                          allocate(dat42(nx, ny, ke-ks+1,1))
                          dat42(:,:,:,1)=dat4(:,:,ks:ke,1)
                          if ( k /= io_proc ) then
                             call mpi_send(dat42(1,1,1,1),size(dat42),mpi_real, k, 300*nv+ks, comm, ierr)
                          else
                             allocate(dat41(nx, ny, ke-ks+1,1))
                             dat41=dat42
                          endif
                          deallocate(dat42)
                       endif
                    enddo
                    deallocate(dat4)
                 else  !if ( my_proc_id == io_proc ) then
                    !---receive dat42 for each core
                    nm=max(1,int((kz+nprocs-1)/nprocs))
                    ks=my_proc_id*nm+1
                    ke=my_proc_id*nm+nm
                    if ( ke > kz ) ke=kz
                    if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                       allocate(dat41(nx, ny, ke-ks+1,1))
                       call mpi_recv(dat41(1,1,1,1), size(dat41), mpi_real, io_proc, 300*nv+ks, comm, status, ierr)
                    endif
                 endif  !if ( my_proc_id == io_proc ) then
              else
                 nm=max(1,int((kz+nprocs-1)/nprocs))
                 ks=my_proc_id*nm+1
                 ke=my_proc_id*nm+nm
                 if ( ke > kz ) ke=kz
                 if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                    allocate(dat41(nx, ny, ke-ks+1,1))
                    dat41=-999999.
                 endif
              endif  !if ( nd > 1 ) then
              !write(*,*)'===w13 got outer domain dat41'

              !--- map u/v to rot-ll grid: ingrid-->dstgrid
              nm=max(1,int((kz+nprocs-1)/nprocs))
              ks=my_proc_id*nm+1
              ke=my_proc_id*nm+nm
              if ( ke > kz ) ke=kz
              if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                 allocate(dat42(nx, ny, ke-ks+1,1))
                 dat42=-999999.
                 if (nv==1) then
                    call combine_grids_for_remap(ix,iy,ke-ks+1,1,u,nx,ny,ke-ks+1,1,dat41,gwt%gwt_t,dat42)
                 else if (nv==2) then
                    call combine_grids_for_remap(ix,iy,ke-ks+1,1,v,nx,ny,ke-ks+1,1,dat41,gwt%gwt_t,dat42)
                 endif
                 if ( my_proc_id /= io_proc ) then
                    call mpi_send(dat42(1,1,1,1),size(dat42),mpi_real, io_proc, 400*nv+ks, comm, ierr)
                    deallocate(dat42)
                 endif
                 deallocate(dat41)
              endif
              !write(*,*)'===w14 got dat42'

              !---collect dat43 to io_proc, and output
              !call mpi_barrier(comm,ierr)
              if ( my_proc_id == io_proc ) then
                 allocate(dat43(nx,ny,kz,1))
                 nm=max(1,int((kz+nprocs-1)/nprocs))
                 do k = 0, nprocs-1
                    ks=k*nm+1
                    ke=k*nm+nm
                    if ( ke > kz ) ke=kz
                    if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                       if ( k /= io_proc ) then
                          allocate(dat41(nx, ny, ke-ks+1,1))
                          call mpi_recv(dat41(1,1,1,1), size(dat41), mpi_real, k, 400*nv+ks, comm, status, ierr)
                          dat43(:,:,ks:ke,1)=dat41(:,:,1:ke-ks+1,1)
                          deallocate(dat41)
                       else
                          dat43(:,:,ks:ke,1)=dat42(:,:,1:ke-ks+1,1)
                          deallocate(dat42)
                       endif
                    endif
                 enddo
                 !write(*,'(a,3i5,100f12.3)')'===w51 ', nx, ny, kz, (dat43(10,10,k,1),k=kz,1,-1)
                 write(flid_out) (((dat43(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
                 deallocate(dat43)
              endif  !if ( my_proc_id == io_proc ) then
              if (nv==1) then
                 deallocate(u)
              else if (nv==2) then
                 deallocate(v)
              endif
           enddo  !do nv = 1, 2
        else if ( nrecord ==12 .or. nrecord ==15 .or. nrecord ==16 .or. nrecord ==17 ) then
           kz=1
           if ( my_proc_id == io_proc ) then
              allocate(dat41(nx,ny,kz,1), dat42(nx,ny,kz,1))
              if ( nd > 1 ) then
                 read(flid_in)dat42
                 do k = 1, kz
                     dat41(:,:,k,1)=dat42(:,:,kz-k+1,1)
                 enddo
                 dat42=-999999.
              else
                 dat41=-999999.
                 dat42=-999999.
              endif
              call combine_grids_for_remap(ix,iy,kz,1,dat43,nx,ny,kz,1,dat41,gwt%gwt_t,dat42)
              write(flid_out) (((dat42(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
              deallocate(dat41, dat42, dat43)
           endif  !if ( my_proc_id == io_proc ) then
        endif

     enddo do_out_var_loop !: for nrecord = 1, 17

     !-------------------------------------------------------------------------
     ! 8 --- clean ingrid gwt
     deallocate( ingrid%grid_lon, ingrid%grid_lat, ingrid%grid_lont, ingrid%grid_latt)
     deallocate( gwt%gwt_t, gwt%gwt_u, gwt%gwt_v )

  enddo do_nestdom_loop !: do nd = 1, ndom
  write(*,*)' === finished hafsvi_preproc ==='

  return
  end subroutine hafsvi_preproc

!========================================================================================
  subroutine hafsvi_preproc_nc(in_dir, in_date, nestdoms, radius, res, out_file)

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
  character (len=2500)   :: infile_fvcore, infile_core, infile_tracer, infile_phy, &
                            infile_sfc, infile_grid, infile_grid2, infile_atmos, infile_oro
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
  character (len=2500):: fl_in, fl_out

!----for hafsvi
  integer  :: nx, ny, nz, filetype  ! filetype: 1=bin, 2=nc
  real     :: lon1,lat1,lon2,lat2,cen_lat,cen_lon,dlat,dlon
  real, allocatable, dimension(:,:) :: glon,glat

  integer  :: i, j, k, n, flid_in, flid_out, ncid, ndims, nrecord
  real     :: rot_lon, rot_lat, ptop
  integer, dimension(nf90_max_var_dims) :: dims
  real, allocatable, dimension(:,:,:,:) :: dat4, dat41, dat42, dat43, u, v
  real, allocatable, dimension(:,:,:)   :: dat3, dat31
  real, allocatable, dimension(:,:)     :: dat2, dat21, sfcp
  real, allocatable, dimension(:)       :: dat1

  !real, allocatable, dimension(:)       :: pfull, phalf
  real, allocatable, dimension(:,:)     :: cangu, sangu, cangv, sangv
  real    :: cputime1, cputime2, cputime3
  integer :: io_proc, nm, ks, ke, nv
  character (len=50)  :: varname, varname_long, units, nzc

!------------------------------------------------------------------------------
! 1 --- arg process
  io_proc=nprocs-1
  !io_proc=0
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
        if ( my_proc_id == 0 ) write(*,'(a)')'!!! hafsvi cut radius number wrong: '//trim(radius)
        if ( my_proc_id == 0 ) write(*,'(a)')'!!! please call with --vortexradius=40 (75< 3)'
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
  !$omp parallel do &
  !$omp& private(i,j,rot_lon,rot_lat)
  do j = 1, ny; do i = 1, nx
     rot_lon = lon1 + dlon*(i-1)
     rot_lat = lat1 + dlat*(j-1)
     call rtll(rot_lon, rot_lat, glon(i,j), glat(i,j), cen_lon, cen_lat)
  enddo; enddo
  if ( my_proc_id == 0 ) write(*,'(a)')'---rot-ll grid: nx, ny, cen_lon, cen_lat, dlon, dlat, lon1, lon2, lat1, lat2'
  if ( my_proc_id == 0 ) write(*,'(15x,2i5,8f10.5)')    nx, ny, cen_lon, cen_lat, dlon, dlat, lon1, lon2, lat1, lat2
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

     if ( debug_level > 10 .and. my_proc_id == 0 ) write(*,'(a)')' --- read grid info from '//trim(infile_grid)
     call rd_grid_spec_data(trim(infile_grid), ingrid)
     ix=ingrid%grid_xt
     iy=ingrid%grid_yt
     if ( debug_level > 10 .and. my_proc_id == 0 ) then
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
        if ( my_proc_id == 0 ) write(*,'(a)')' --- call cal_src_dst_grid_weight'
        write(*,'(i,a,2(i,1x),2(f,1x))')my_proc_id,' --- dstgrid: ', nx, ny, &
             dstgrid%grid_lont(int(nx/2),int(ny/2)), dstgrid%grid_latt(int(nx/2),int(ny/2))
     endif
     call cal_src_dst_grid_weight(ingrid, dstgrid)

     !-------------------------------------------------------------------------
     ! 6 --- dst files
     if ( nd == 1 ) then
        fl_in=""                   !inner domain rot-ll file
        fl_out=trim(out_file)      !current domain rot-ll file
     else if ( nd == 2 ) then
        fl_in=trim(out_file)
        fl_out=trim(out_file)//'_'//trim(nestfl)
     else
        write(tempfl,'(a4,i2.2)')'nest',nd-1
        fl_out=trim(out_file)//'_'//trim(nestfl)
        fl_in=trim(out_file)//'_'//trim(tempfl)
     endif

     !-------------------------------------------------------------------------
     ! 7 --- output
     call cpu_time(cputime1)
     if ( my_proc_id == io_proc ) write(*,'(a,i3,f)')' --- record start cputime: ', 0, cputime1

     ! 7.1 --- no-interp-needed variables: dimension and domain info
     !--- record 1: nx, ny, nz
     !--- record 2: lon1,lat1,lon2,lat2,cen_lon,cen_lat
     !--- record 10: glon,glat,glon,glat   ! 2D
     !--- record 13: ak
     !--- record 14: bk
     call get_var_dim(trim(infile_atmos), 'pfull', ndims, dims)
     nz=dims(1)
     if ( my_proc_id == 0 ) then
        !write(flid_out) nx, ny, nz
        call write_nc_dim(trim(fl_out), 'nx', nx)
        call write_nc_dim(trim(fl_out), 'ny', ny)
        call write_nc_dim(trim(fl_out), 'nz', nz)
        call write_nc_dim(trim(fl_out), 'nz1', nz+1)
        call write_nc_real0d(trim(fl_out), 'lon1', lon1, 'degree', 'longtitude 1')
        call write_nc_real0d(trim(fl_out), 'lat1', lat1, 'degree', 'latitude 1')
        call write_nc_real0d(trim(fl_out), 'lon2', lon2, 'degree', 'longtitude 2')
        call write_nc_real0d(trim(fl_out), 'lat2', lat2, 'degree', 'latitude 1')
        call write_nc_real0d(trim(fl_out), 'cen_lon', cen_lon, 'degree', 'center of longtitude')
        call write_nc_real0d(trim(fl_out), 'cen_lat', cen_lat, 'degree', 'center of latitude')

        !----
        !--- change: Jul 2022 JH Shin
        !  For WPAC storms located near the west of international date line, add 360 to
        !  longitude value in the eastern side of IDL (western hemisphere) so that all
        !  longitude values have positive values in the VI domain.
        !  if ( cen_lon > 0. ) then
        !   do j = 1, ny; do i = 1, nx
        !    if(glon(i,j).lt.0.0) glon(i,j)=glon(i,j)+360.0
        !   enddo; enddo
        !  endif
          if ( cen_lon > 0. ) where ( glon < 0.) glon=glon+360.

        !  For CPAC storms located near the east of international date line, subrtact 360
        !  from longitude value in the western side of IDL (eastern hemisphere) so
        !  that all longitude values have negative values in the VI domain.
        !  if ( cen_lon < -140. )then
        !   do j = 1, ny; do i = 1, nx
        !    if(glon(i,j).gt.0.0) glon(i,j)=glon(i,j)-360.0
        !   enddo; enddo
        !  endif
        if ( cen_lon < -140. ) where ( glon > 0. ) glon=glon-360.

        call write_nc_real(trim(fl_out), 'glon', nx, ny, -1, -1, 'nx', 'ny', '-', '-', glon, 'degree', 'rot-ll longtitude')
        call write_nc_real(trim(fl_out), 'glat', nx, ny, -1, -1, 'nx', 'ny', '-', '-', glat, 'degree', 'rot-ll latitude')
        allocate(dat4(nz+1,1,1,1))
        call get_var_data(trim(infile_fvcore), 'ak', nz+1, 1, 1, 1, dat4)
        call write_nc_real(trim(fl_out), 'ak', -1, -1, nz+1, -1, '-', '-', 'nz1', '-', dat4, 'scalar', 'ak')
        deallocate(dat4)
        allocate(dat4(nz+1,1,1,1))
        call get_var_data(trim(infile_fvcore), 'bk', nz+1, 1, 1, 1, dat4)
        call write_nc_real(trim(fl_out), 'bk', -1, -1, nz+1, -1, '-', '-', 'nz1', '-', dat4, 'scalar', 'bk')
        deallocate(dat4)

     endif
     call mpi_barrier(comm,ierr)

     ! 7.2 --- remapp-needed variables:
     do_out_var_loop: do nrecord = 3, 17
        if ( nrecord == 7 .or. nrecord == 10 .or. nrecord == 13 .or. nrecord == 14 ) cycle do_out_var_loop

        iz=nz   !same vertical levels
        if ( nrecord == 12 .or. nrecord == 15 .or. nrecord == 16 .or. nrecord ==17 ) iz=1
        if ( nrecord ==  9 .or. nrecord == 11 ) iz=nz+1
        if ( nrecord == 3 ) then
           varname='pf1'
           units='pa'
           varname_long='pressure'
        elseif ( nrecord == 4 ) then
           varname='T'
           units='K'
           varname_long='temperature'
        elseif ( nrecord == 5 ) then
           varname='sphum'
           units='kg/kg'
           varname_long='sphum'
        elseif ( nrecord == 8 ) then
           varname='dzdt'
           units='m/s'
           varname_long='vertical velocity w'
        elseif ( nrecord == 9 ) then
           varname='z1'
           units='gpm'
           varname_long='Geopotential Height'
        elseif ( nrecord == 11 ) then
           varname='ph1'
           units='pa'
           varname_long='pressure on half level'
        elseif ( nrecord == 12 ) then
           varname='sfcp'
           units='pa'
           varname_long='surface pressure'
        elseif ( nrecord == 15 ) then
           varname='slmsk'
           units='numeric'
           varname_long='sea-land-ice mask (0-sea, 1-land, 2-ice)'
        elseif ( n == 16 ) then
           varname='zorl'
           units='numeric'
           varname_long='sfcr - surface roughness'
        elseif ( n == 17 ) then
           varname='f10m'
           units='numeric'
           varname_long='(10m wind speed)/(level 1 wind speed)'
        endif
        nzc='nz'
        if ( iz == nz+1 ) nzc='nz1'
        if ( iz == 1 ) nzc='-'

        !-----------------------------
        !---7.1 record 1: nx, ny, nz
        !---nx, ny, nz, & lon1,lat1,lon2,lat2,cen_lon,cen_lat

        !-----------------------------
        !---7.2 record 2: lon1,lat1,lon2,lat2,cen_lon,cen_lat

        !-----------------------------
        !---7.3 record 3: (((pf1(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        !---     hafs-VI/read_hafs_out.f90 pf:
        !---          ph(k) = ak(k) + bk(k)*p_s --> half level pressure
        !---          pf(k) = (ph(k+1) - ph(k)) / log(ph(k+1)/ph(k)) --> full level pressure
        !---
        !---seem pf1 is pressure on full level, use
        !---    pf1(k) = phalf(1) + sum(delp(1:k))
        if ( nrecord == 3 .or. nrecord == 9 .or. nrecord == 11 ) then
           if ( nrecord == 3 ) then
              allocate(dat4(iz+1,1,1,1))
              call get_var_data(trim(infile_atmos), 'phalf', iz+1, 1, 1, 1, dat4)
              ptop=dat4(1,1,1,1)*100.  !phalf:units = "mb" ;
              deallocate(dat4)
           endif

           if ( my_proc_id == io_proc ) then
              if ( nrecord == 3 ) then
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
                 deallocate(dat2, dat4)
              else if ( nrecord == 9 ) then
                 allocate(dat4(ix, iy, 1,1))
                 call get_var_data(trim(infile_core), 'phis', ix, iy, 1, 1, dat4)
                 allocate(dat41(ix, iy, iz, 1))
                 dat41(:,:,iz,1)=dat4(:,:,1,1)/g
                 deallocate(dat4)
                 allocate(dat4(ix, iy, iz-1, 1))
                 call get_var_data(trim(infile_core), 'DZ', ix, iy, iz-1, 1, dat4)
                 do k = iz-1, 1, -1
                    dat41(:,:,k,1)=dat41(:,:,k+1,1)-dat4(:,:,k,1)
                 enddo
                 deallocate(dat4)
              else if ( nrecord == 11 ) then
                 allocate(dat4(ix, iy, iz-1, 1), dat41(ix, iy, iz, 1))
                 call get_var_data(trim(infile_core), 'delp', ix, iy, iz-1, 1, dat4)
                 dat41(:,:,1,1)=ptop
                 do k = 2, iz
                    dat41(:,:,k,1)=dat41(:,:,k-1,1)+dat4(:,:,k-1,1)
                 enddo
                 deallocate(dat4)
              endif  !if ( nrecord == 3 ) then

              !---broadcast dat41 to each computing-core
              if ( nprocs > 1 ) then
                 nm=max(1,int((iz+nprocs-1)/nprocs))  !devide iz to each processor
                 do k = 0, nprocs-1
                    ks=k*nm+1              !k-start
                    ke=k*nm+nm            !k-end
                    if ( ke > iz ) ke=iz
                    if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                       allocate(dat42(ix, iy, ke-ks+1,1))
                       dat42(:,:,:,1)=dat41(:,:,ks:ke,1)
                       if ( k /= io_proc ) then
                          call mpi_send(dat42(1,1,1,1), size(dat42), mpi_real, k, 3000+ks, comm, ierr)
                       else
                          allocate(dat43(ix, iy, ke-ks+1,1))
                          dat43=dat42
                       endif
                       deallocate(dat42)
                    endif
                 enddo
              else  !if ( nprocs > 1 ) then
                 allocate(dat43(ix, iy, iz,1))
                 dat43=dat41
              endif
              deallocate(dat41)
           else ! if ( my_proc_id == io_proc ) then
              !---receive dat43
              nm=max(1,int((iz+nprocs-1)/nprocs))
              ks=min(iz,my_proc_id*nm+1)
              ke=min(iz,my_proc_id*nm+nm)
              if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                 allocate(dat43(ix, iy, ke-ks+1,1))
                 !call mpi_recv(dat43(1,1,1,1), size(dat43), mpi_real, io_proc, mpi_any_tag, comm, status, ierr)
                 call mpi_recv(dat43(1,1,1,1), size(dat43), mpi_real, io_proc, 3000+ks, comm, status, ierr)
              endif
           endif
        endif

        !-----------------------------
        !---7.4 record 4: (((tmp(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        !---7.5 record 5: (((spfh(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        !---7.8 record 8: (((dzdt(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        if ( nrecord == 4 .or. nrecord == 5 .or. nrecord == 8 ) then
           if ( my_proc_id == io_proc ) then
              !---read in
              allocate(dat4(ix, iy, iz,1))
              if ( nrecord == 4 ) call get_var_data(trim(infile_core), 'T', ix, iy, iz,1, dat4)
              if ( nrecord == 5 ) call get_var_data(trim(infile_tracer), 'sphum', ix, iy, iz,1, dat4)
              if ( nrecord == 8 ) call get_var_data(trim(infile_core), 'W', ix, iy, iz,1, dat4)
              !---send to other core
              if ( nprocs > 1 ) then
                 nm=max(1,int((iz+nprocs-1)/nprocs))
                 do k = 0, nprocs-1
                    ks=k*nm+1
                    ke=k*nm+nm            !k-end
                    if ( ke > iz ) ke=iz
                    if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                       allocate(dat42(ix, iy, ke-ks+1,1))
                       dat42(:,:,:,1)=dat4(:,:,ks:ke,1)
                       if ( k /= io_proc ) then
                          call mpi_send(dat42(1,1,1,1), size(dat42), mpi_real, k, 4000+ks, comm, ierr)
                       else
                          allocate(dat43(ix, iy, ke-ks+1,1))
                          dat43=dat42
                       endif
                       deallocate(dat42)
                    endif
                 enddo
              else
                 allocate(dat43(ix, iy, iz,1))
                 dat43=dat4
              endif
              deallocate(dat4)
           else  !if ( my_proc_id == io_proc ) then
              !---receive dat43
              nm=max(1,int((iz+nprocs-1)/nprocs))
              ks=min(iz,my_proc_id*nm+1)
              ke=min(iz,my_proc_id*nm+nm)
              if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                 allocate(dat43(ix, iy, ke-ks+1,1))
                 call mpi_recv(dat43(1,1,1,1), size(dat43), mpi_real, io_proc, 4000+ks, comm, status, ierr)
              endif
           endif  !if ( my_proc_id == io_proc ) then
        endif

        !-----------------------------
        !---7.6 record 6: (((ugrd(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        !---7.7 record 7: (((vgrd(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        if ( nrecord == 6 ) then
           !---get u,v from restart
           if ( my_proc_id == io_proc ) then
              do nv = 1, 2
                 if (nv==1) allocate(dat4(ix, iy+1, iz,1))
                 if (nv==1) call get_var_data(trim(infile_core), 'u', ix, iy+1, iz, 1, dat4)
                 if (nv==2) allocate(dat4(ix+1, iy, iz,1))
                 if (nv==2) call get_var_data(trim(infile_core), 'v', ix+1, iy, iz, 1, dat4)
                 !---send to other core
                 if ( nprocs > 1 ) then
                    nm=max(1,int((iz+nprocs-1)/nprocs))
                    do k = 0, nprocs-1
                       ks=k*nm+1
                       ke=k*nm+nm            !k-end
                       if ( ke > iz ) ke=iz
                       if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                          if (nv==1) allocate(dat41(ix, iy+1, ke-ks+1,1))
                          if (nv==2) allocate(dat41(ix+1, iy, ke-ks+1,1))
                          dat41(:,:,:,1)=dat4(:,:,ks:ke,1)
                          if ( k /= io_proc ) then
                             call mpi_send(dat41(1,1,1,1), size(dat41), mpi_real, k, 200*nv+ks, comm, ierr)
                          else
                             if (nv==1) allocate(dat42(ix, iy+1, ke-ks+1,1))
                             if (nv==1) dat42=dat41
                             if (nv==2) allocate(dat43(ix+1, iy, ke-ks+1,1))
                             if (nv==2) dat43=dat41
                          endif
                          deallocate(dat41)
                       endif
                    enddo
                 else  !if ( nprocs > 1 ) then
                    if (nv==1) allocate(dat42(ix, iy+1, iz,1))
                    if (nv==1) dat42=dat4
                    if (nv==2) allocate(dat43(ix+1, iy, iz,1))
                    if (nv==1) dat43=dat4
                 endif
                 deallocate(dat4)
              enddo  !do nv = 1, 2
           else  !if ( my_proc_id == io_proc ) then
              !---receive dat42 dat43
              nm=max(1,int((iz+nprocs-1)/nprocs))
              ks=min(iz,my_proc_id*nm+1)
              ke=min(iz,my_proc_id*nm+nm)
              if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                 allocate(dat42(ix, iy+1, ke-ks+1,1), dat43(ix+1, iy, ke-ks+1,1))
                 call mpi_recv(dat42(1,1,1,1), size(dat42), mpi_real, io_proc, 200*1+ks, comm, status, ierr)
                 call mpi_recv(dat43(1,1,1,1), size(dat43), mpi_real, io_proc, 200*2+ks, comm, status, ierr)
              endif
           endif  !if ( my_proc_id == io_proc ) then
        endif

        !!-----------------------------
        !!---7.9 record 9: (((z1(i,j,k),i=1,nx),j=1,ny),k=nz1,1,-1)
        !!---     hafs-VI/read_hafs_out.f90 z1:
        !!---          z1(I,J,K)=z1(I,J,K+1)+rdgas1*tmp(i,j,k)*(1.+0.608*spfh(i,j,k))*ALOG(ph1(i,j,k+1)/ph1(i,j,k))
        !!--- hgt: phis/g-sum(DZ)

        !-----------------------------
        !---7.10 record 10: glon,glat,glon,glat   ! 2D
        !--- glat=grid_yt*180./pi, grid_yt=1:2160, what is this?

        !!-----------------------------
        !!---7.11 record 11: (((ph1(i,j,k),i=1,nx),j=1,ny),k=nz1,1,-1)
        !!---     hafs-VI/read_hafs_out.f90 ph:
        !!---       ph(k) = ak(k) + bk(k)*p_s --> pressure in pa
        !!---       64.270-->100570
        !!---seem ph1 is pressure on half level, use
        !!---    pf1(k) = phalf(1) + sum(delp(1:k))

        !-----------------------------
        !---7.12 record 12: pressfc1              ! 2D
        !--- use lowest-level pressure?
        if ( nrecord == 12 ) then
           if ( my_proc_id == io_proc ) then
              allocate(dat43(ix, iy, 1, 1))
              dat43(:,:,1,1)=sfcp(:,:)
              deallocate(sfcp)
           endif
        endif

        !-----------------------------
        !---7.13 record 13: ak

        !-----------------------------
        !---7.14 record 14: bk

        !-----------------------------
        !---7.15 record 15: land                  ! =A101 = land sea mask, B101 = ZNT
        !---     hafs-VI/read_hafs_out.f90 land:long_name = "sea-land-ice mask (0-sea, 1-land, 2-ice)" ;
        !--- sfc_data.nc: slmsk
        if ( nrecord == 15 ) then
           if ( my_proc_id == io_proc ) then
              allocate(dat43(ix, iy, 1,1))
              call get_var_data(trim(infile_sfc), 'slmsk', ix, iy, 1, 1, dat43)
           endif
        endif

        !-----------------------------
        !---7.16 record 16: sfcr                  ! =B101 = Z0
        !---surface roughness
        if ( nrecord == 16 ) then
           if ( my_proc_id == io_proc ) then
              allocate(dat43(ix, iy, 1,1))
              call get_var_data(trim(infile_sfc), 'zorl', ix, iy, 1, 1, dat43)
              ! convert from cm to m
              dat43=dat43/100.
           endif
        endif

        !-----------------------------
        !---7.17 record 17: C101                  ! =C101 = (10m wind speed)/(level 1 wind speed)
        !---                                      ! =C101 = f10m (in the sfc_data.nc)
        if ( nrecord == 17 ) then
           if ( my_proc_id == io_proc ) then
              allocate(dat43(ix, iy, 1, 1))
              call get_var_data(trim(infile_sfc), 'f10m', ix, iy, 1, 1, dat43)
           endif
        endif

        !-----------------------------
        !---7.18 output 3d
        if ( nrecord == 3 .or. nrecord == 4 .or. nrecord == 5 .or. &
             nrecord == 8 .or. nrecord == 9 .or. nrecord ==11 )then
           call mpi_barrier(comm,ierr)
           kz=nz
           if ( nrecord ==  9 .or. nrecord == 11 ) then
              kz=nz+1
           endif
           !--- map fv3 grid to rot-ll grid: ingrid-->dstgrid
           !call cpu_time(cputime2)
           !write(*,'(a,i3,f)')' --- read rot-ll grid for 1 record ', nrecord, cputime2

           if ( nprocs == 1 ) then  !--no mpi
              !----only 1-core
              allocate(dat41(nx,ny,kz,1), dat42(nx,ny,kz,1))
              if ( nd > 1 ) then
                 !read(flid_in)dat42
                 call get_var_data(trim(fl_in), trim(varname), nx, ny, kz, 1, dat42)
                 do k = 1, kz
                     dat41(:,:,k,1)=dat42(:,:,kz-k+1,1)
                 enddo
                 dat42=-999999.
              else
                 dat41=-999999.
              endif
              call combine_grids_for_remap(ix,iy,kz,1,dat43,nx,ny,kz,1,dat41,gwt%gwt_t,dat42)

              !--- output
              !write(*,'(a,i2.2,a,200f)')'=== record',nrecord,': ', dat42(int(nx/2),int(ny/2),:,1)
              !write(flid_out) (((dat42(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
              call write_nc_real(trim(fl_out), trim(varname), nx, ny, kz, -1, 'nx', 'ny', trim(nzc), '-', dat42, trim(units), trim(varname_long))
              deallocate(dat41, dat42, dat43)
           else
              !----mpi: 0 is for IO, >0 is for computing

              !---when nd>1, get previous data at 0, and then send to other cores
              if ( nd > 1 ) then
                 if ( my_proc_id == io_proc ) then
                    allocate(dat4(nx,ny,kz,1), dat42(nx,ny,kz,1))
                    !read(flid_in)dat42
                    call get_var_data(trim(fl_in), trim(varname), nx, ny, kz, 1, dat42)
                    do k = 1, kz
                        dat4(:,:,k,1)=dat42(:,:,kz-k+1,1)
                    enddo
                    deallocate(dat42)
                    nm=max(1,int((kz+nprocs-1)/nprocs))
                    do k = 0, nprocs-1
                       ks=k*nm+1
                       ke=k*nm+nm
                       if ( ke > iz ) ke=iz
                       if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                          allocate(dat42(nx, ny, ke-ks+1,1))
                          dat42(:,:,:,1)=dat4(:,:,ks:ke,1)
                          if ( k /= io_proc ) then
                             call mpi_send(dat42(1,1,1,1),size(dat42),mpi_real, k, 4000+ks, comm, ierr)
                          else
                             allocate(dat41(nx, ny, ke-ks+1,1))
                             dat41=dat42
                          endif
                          deallocate(dat42)
                       endif
                    enddo
                    deallocate(dat4)
                 else  !if ( my_proc_id == io_proc ) then
                    !---receive dat42 for each core
                    nm=max(1,int((kz+nprocs-1)/nprocs))
                    ks=my_proc_id*nm+1
                    ke=my_proc_id*nm+nm
                    if ( ke > kz ) ke=kz
                    if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                       allocate(dat41(nx, ny, ke-ks+1,1))
                       call mpi_recv(dat41(1,1,1,1), size(dat41), mpi_real, io_proc, 4000+ks, comm, status, ierr)
                    endif
                 endif  !if ( my_proc_id == io_proc ) then
              else  !if ( nd > 1 ) then
                 nm=max(1,int((kz+nprocs-1)/nprocs))
                 ks=my_proc_id*nm+1
                 ke=my_proc_id*nm+nm
                 if ( ke > kz ) ke=kz
                 if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                    allocate(dat41(nx, ny, ke-ks+1,1))
                    dat41=-999999.
                 endif
              endif  !if ( nd > 1 ) then

              !---combine dat43+dat41 --> dat42
              !call mpi_barrier(comm,ierr)
              nm=max(1,int((kz+nprocs-1)/nprocs))
              ks=my_proc_id*nm+1
              ke=my_proc_id*nm+nm
              if ( ke > kz ) ke=kz
              if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                 allocate(dat42(nx, ny, ke-ks+1,1))
                 dat42=-999999.
                 call combine_grids_for_remap(ix,iy,ke-ks+1,1,dat43,nx,ny,ke-ks+1,1,dat41,gwt%gwt_t,dat42)
                 if ( my_proc_id /= io_proc ) then
                    call mpi_send(dat42(1,1,1,1),size(dat42),mpi_real, io_proc, 5000+ks, comm, ierr)
                    deallocate(dat42)
                 endif
                 deallocate(dat41, dat43)
              endif

              !---collect dat43 to io_proc, and output
              !call mpi_barrier(comm,ierr)
              if ( my_proc_id == io_proc ) then
                 allocate(dat43(nx,ny,kz,1))
                 nm=max(1,int((kz+nprocs-1)/nprocs))
                 do k = 0, nprocs-1
                    ks=k*nm+1
                    ke=k*nm+nm
                    if ( ke > kz ) ke=kz
                    if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                       if ( k /= io_proc ) then
                          allocate(dat41(nx, ny, ke-ks+1,1))
                          call mpi_recv(dat41(1,1,1,1), size(dat41), mpi_real, k, 5000+ks, comm, status, ierr)
                          dat43(:,:,ks:ke,1)=dat41(:,:,1:ke-ks+1,1)
                          deallocate(dat41)
                       else
                          dat43(:,:,ks:ke,1)=dat42(:,:,1:ke-ks+1,1)
                          deallocate(dat42)
                       endif
                    endif
                 enddo
                 !write(*,'(a,3i5,100f12.3)')'===w34 ', nx, ny, kz, (dat43(10,10,k,1),k=kz,1,-1)
                 !write(flid_out) (((dat43(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
                 call write_nc_real(trim(fl_out), trim(varname), nx, ny, kz, -1, 'nx', 'ny', trim(nzc), '-', dat43, trim(units), trim(varname_long))
                 deallocate(dat43)
              endif  !if ( my_proc_id == io_proc ) then
           endif  ! if ( nprocs == 1 ) then  !--no mpi
        else if ( nrecord == 6 ) then  !---u,v
           kz=nz
           !---convert u,v from fv3grid to earth
           nm=max(1,int((kz+nprocs-1)/nprocs))
           ks=my_proc_id*nm+1; ke=my_proc_id*nm+nm
           if ( ke > kz ) ke=kz
           if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
              allocate(u(ix,iy,ke-ks+1,1), v(ix,iy,ke-ks+1,1))
              allocate(dat2(ix, iy+1), dat21(ix+1, iy))
              do k = 1, ke-ks+1
                 call fv3uv2earth(ix, iy, dat42(:,:,k,1), dat43(:,:,k,1), cangu, sangu, cangv, sangv, dat2, dat21)
                 !---destage: C-/D- grid to A-grid
                 u(:,:,k,1) = (dat2 (:,1:iy)+dat2 (:,2:iy+1))/2.0
                 v(:,:,k,1) = (dat21(1:ix,:)+dat21(2:ix+1,:))/2.0
              enddo
              deallocate(dat42, dat43, dat2, dat21, cangu, sangu, cangv, sangv)
           endif

           !--- loop u,v
           do nv = 1, 2
              !--- get outer domain u,v
              if ( nd > 1 ) then
                 if ( my_proc_id == io_proc ) then
                    allocate(dat4(nx,ny,kz,1), dat42(nx,ny,kz,1))
                    !read(flid_in)dat42
                    if (nv==1)call get_var_data(trim(fl_in), 'u', nx, ny, kz, 1, dat42)
                    if (nv==2)call get_var_data(trim(fl_in), 'v', nx, ny, kz, 1, dat42)
                    do k = 1, kz
                       dat4(:,:,k,1)=dat42(:,:,kz-k+1,1)
                    enddo
                    deallocate(dat42)
                    nm=max(1,int((kz+nprocs-1)/nprocs))
                    do k = 0, nprocs-1
                       ks=k*nm+1
                       ke=k*nm+nm
                       if ( ke > iz ) ke=iz
                       if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                          allocate(dat42(nx, ny, ke-ks+1,1))
                          dat42(:,:,:,1)=dat4(:,:,ks:ke,1)
                          if ( k /= io_proc ) then
                             call mpi_send(dat42(1,1,1,1),size(dat42),mpi_real, k, 300*nv+ks, comm, ierr)
                          else
                             allocate(dat41(nx, ny, ke-ks+1,1))
                             dat41=dat42
                          endif
                          deallocate(dat42)
                       endif
                    enddo
                    deallocate(dat4)
                 else  !if ( my_proc_id == io_proc ) then
                    !---receive dat42 for each core
                    nm=max(1,int((kz+nprocs-1)/nprocs))
                    ks=my_proc_id*nm+1
                    ke=my_proc_id*nm+nm
                    if ( ke > kz ) ke=kz
                    if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                       allocate(dat41(nx, ny, ke-ks+1,1))
                       call mpi_recv(dat41(1,1,1,1), size(dat41), mpi_real, io_proc, 300*nv+ks, comm, status, ierr)
                    endif
                 endif  !if ( my_proc_id == io_proc ) then
              else
                 nm=max(1,int((kz+nprocs-1)/nprocs))
                 ks=my_proc_id*nm+1
                 ke=my_proc_id*nm+nm
                 if ( ke > kz ) ke=kz
                 if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                    allocate(dat41(nx, ny, ke-ks+1,1))
                    dat41=-999999.
                 endif
              endif  !if ( nd > 1 ) then

              !--- map u/v to rot-ll grid: ingrid-->dstgrid
              nm=max(1,int((kz+nprocs-1)/nprocs))
              ks=my_proc_id*nm+1
              ke=my_proc_id*nm+nm
              if ( ke > kz ) ke=kz
              if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                 allocate(dat42(nx, ny, ke-ks+1,1))
                 dat42=-999999.
                 if (nv==1) call combine_grids_for_remap(ix,iy,ke-ks+1,1,u,nx,ny,ke-ks+1,1,dat41,gwt%gwt_t,dat42)
                 if (nv==2) call combine_grids_for_remap(ix,iy,ke-ks+1,1,v,nx,ny,ke-ks+1,1,dat41,gwt%gwt_t,dat42)
                 if ( my_proc_id /= io_proc ) then
                    call mpi_send(dat42(1,1,1,1),size(dat42),mpi_real, io_proc, 400*nv+ks, comm, ierr)
                    deallocate(dat42)
                 endif
                 deallocate(dat41)
              endif

              !---collect dat43 to io_proc, and output
              !call mpi_barrier(comm,ierr)
              if ( my_proc_id == io_proc ) then
                 allocate(dat43(nx,ny,kz,1))
                 nm=max(1,int((kz+nprocs-1)/nprocs))
                 do k = 0, nprocs-1
                    ks=k*nm+1
                    ke=k*nm+nm
                    if ( ke > kz ) ke=kz
                    if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                       if ( k /= io_proc ) then
                          allocate(dat41(nx, ny, ke-ks+1,1))
                          call mpi_recv(dat41(1,1,1,1), size(dat41), mpi_real, k, 400*nv+ks, comm, status, ierr)
                          dat43(:,:,ks:ke,1)=dat41(:,:,1:ke-ks+1,1)
                          deallocate(dat41)
                       else
                          dat43(:,:,ks:ke,1)=dat42(:,:,1:ke-ks+1,1)
                          deallocate(dat42)
                       endif
                    endif
                 enddo
                 !write(*,'(a,3i5,100f12.3)')'===w51 ', nx, ny, kz, (dat43(10,10,k,1),k=kz,1,-1)
                 !write(flid_out) (((dat43(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
                 if (nv==1) call write_nc_real(trim(fl_out), 'u', nx, ny, kz, -1, 'nx', 'ny', 'nz', '-', dat43, 'm/s', 'u-component')
                 if (nv==2) call write_nc_real(trim(fl_out), 'v', nx, ny, kz, -1, 'nx', 'ny', 'nz', '-', dat43, 'm/s', 'v-component')
                 deallocate(dat43)
              endif  !if ( my_proc_id == io_proc ) then
              if (nv==1) deallocate(u)
              if (nv==2) deallocate(v)
           enddo  !do nv = 1, 2
        else if ( nrecord ==12 .or. nrecord ==15 .or. nrecord ==16 .or. nrecord ==17 ) then
           kz=1
           if ( my_proc_id == io_proc ) then
              allocate(dat41(nx,ny,kz,1), dat42(nx,ny,kz,1))
              if ( nd > 1 ) then
                 !read(flid_in)dat42
                 call get_var_data(trim(fl_in), trim(varname), nx, ny, kz, 1, dat42)
                 do k = 1, kz
                     dat41(:,:,k,1)=dat42(:,:,kz-k+1,1)
                 enddo
                 dat42=-999999.
              else
                 dat41=-999999.
                 dat42=-999999.
              endif
              call combine_grids_for_remap(ix,iy,kz,1,dat43,nx,ny,kz,1,dat41,gwt%gwt_t,dat42)
              !write(flid_out) (((dat42(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
              call write_nc_real(trim(fl_out), trim(varname), nx, ny, -1, -1, 'nx', 'ny', '-', '-', dat42, trim(units), trim(varname_long))
              deallocate(dat41, dat42, dat43)
           endif  !if ( my_proc_id == io_proc ) then
        endif

     enddo do_out_var_loop !: for nrecord = 1, 17

     !-------------------------------------------------------------------------
     ! 8 --- clean ingrid gwt
     deallocate( ingrid%grid_lon, ingrid%grid_lat, ingrid%grid_lont, ingrid%grid_latt)
     deallocate( gwt%gwt_t, gwt%gwt_u, gwt%gwt_v )

  enddo do_nestdom_loop !: do nd = 1, ndom
  write(*,*)' === finished hafsvi_preproc ==='

  return
  end subroutine hafsvi_preproc_nc

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
  real, allocatable, dimension(:,:,:,:) :: dat4, dat41, dat42, dat43, dat44, phis1, phis2, sfcp1, sfcp2, u1, v1, u, v
  real, allocatable, dimension(:,:,:)   :: dat3, dat31
  real, allocatable, dimension(:,:)     :: dat2, dat21
  real, allocatable, dimension(:)       :: dat1
  real     :: ptop
  logical  :: file_exist

  real, allocatable, dimension(:,:)     :: cangu, sangu, cangv, sangv

  integer :: io_proc, nm, ks, ke, nv

!------------------------------------------------------------------------------
! 1 --- arg process
! 1.1 --- i/o processor
  io_proc=nprocs-1  !last processor as I/O
  !io_proc=0

! 1.2 --- ndom
  ndom=nestdoms+1

!------------------------------------------------------------------------------
! 2 --- input grid info

  !-----------------------------
  !---2.1 get input grid info from binary file
  iunit=36
  open(iunit, file=trim(in_file), form='unformatted')
  read(iunit) nx, ny, nz, i360
  write(*,'(a,4i5)')'===w40 nx, ny, nz, i360 = ',nx, ny, nz, i360
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
  close(iunit)
! New change: Jul 2022 JH Shin
! -------------------------------------------------------------

! For WPAC storms, CONVERT positive values of western hemisphere within the VI domain
! into negative value, IF the portion of western hemisphere (e.g., the eastern side of IDL)
! is included in VI domain, because VI is completed
  if ( cen_lon > 0. ) then
     where ( hlon > 180. ) hlon=hlon-360.
     where ( vlon > 180. ) vlon=vlon-360.
  endif

! For CPAC storms located near the east of international date line
! CONVERT negative values of eastern hemisphere within the VI domain into positive value
! because VI is done
  if ( cen_lon < -140. )then
     where ( hlon <= -180. ) hlon=hlon+360.
     where ( vlon <= -180. ) vlon=vlon+360.
  endif

! New change: Jul 2022 JH Shin
! -------------------------------------------------------------


  if (my_proc_id==0) then
     write(*, '(a,8f10.3)')' hlon,hlat(1,1; nx,1; nx,ny; 1,ny) =', &
                        hlon(1,1), hlat(1,1), hlon(nx,1), hlat(nx,1), hlon(nx,ny), hlat(nx,ny), hlon(1,ny), hlat(1,ny)
     write(*, '(a,8f10.3)')' vlon,vlat(1,1; nx,1; nx,ny; 1,ny) =', &
                        vlon(1,1), vlat(1,1), vlon(nx,1), vlat(nx,1), vlon(nx,ny), vlat(nx,ny), vlon(1,ny), vlat(1,ny)
  endif

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
     do_record_loop: do nrecord = 1, 14

        if ( my_proc_id == io_proc ) open(iunit, file=trim(in_file), form='unformatted')

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
           if ( my_proc_id == io_proc ) then
              if ( nrecord == 12 ) then
                 allocate(dat2(nx,ny))
                 read(iunit)dat2
                 deallocate(dat2)
              elseif ( nrecord == 13 .or. nrecord == 14 ) then
                 allocate(dat1(nz+1))
                 read(iunit)dat1
                 deallocate(dat1)
              else
                 read(iunit)
              endif
           endif
        endif

        !  ALLOCATE ( T1(NX,NY,NZ),Q1(NX,NY,NZ) )
        !  ALLOCATE ( U1(NX,NY,NZ),V1(NX,NY,NZ),DZDT(NX,NY,NZ) )
        !  ALLOCATE ( Z1(NX,NY,NZ+1),P1(NX,NY,NZ+1) )
        if ( nrecord == 6 ) then   !u,v - 6,7
           !---record 6 : U1
           !---record 7 : V1
           iz=nz
           if ( my_proc_id == io_proc ) then
              nm=max(1,int((iz+nprocs-1)/nprocs))
              do nv = 1, 2
                 !---get data
                 allocate(dat3(nx,ny,iz), dat4(nx,ny,iz,1))
                 read(iunit) dat3
                 do k = 1, nz
                    dat4(:,:,nz-k+1,1)=dat3(:,:,k)
                 enddo
                 deallocate(dat3)

                 !---send
                 if ( nprocs > 1 ) then
                    do k = 0, nprocs-1
                       ks=k*nm+1
                       ke=k*nm+nm            !k-end
                       if ( ke > iz ) ke=iz
                       if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                          allocate(dat41(nx, ny, ke-ks+1,1))
                          dat41(:,:,:,1)=dat4(:,:,ks:ke,1)
                          if ( k /= io_proc ) then
                             call mpi_send(dat41(1,1,1,1), size(dat41), mpi_real, k, 200*nv+ks, comm, ierr)
                          else
                             if ( nv == 1 ) then
                                allocate(dat42(nx, ny, ke-ks+1,1))
                                dat42=dat41
                             else if ( nv == 2 ) then
                                allocate(dat43(nx, ny, ke-ks+1,1))
                                dat43=dat41
                             endif
                          endif
                          deallocate(dat41)
                       endif
                    enddo
                 else  !if ( nprocs > 1 ) then
                    if ( nv == 1 ) then
                       allocate(dat42(nx, ny, iz, 1))
                       dat42=dat4
                    else if ( nv == 2 ) then
                       allocate(dat43(nx, ny, iz, 1))
                       dat43=dat4
                    endif
                 endif  !if ( nprocs > 1 ) then
                 deallocate(dat4)
              enddo  !do nv = 1, 2
           else    !if ( my_proc_id == io_proc ) then
              !---receive dat42 dat43
              if ( nprocs > 1 ) then
                 nm=max(1,int((iz+nprocs-1)/nprocs))
                 ks=min(iz,my_proc_id*nm+1)
                 ke=min(iz,my_proc_id*nm+nm)
                 if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                    allocate(dat42(nx, ny, ke-ks+1,1),dat43(nx, ny, ke-ks+1,1))
                    call mpi_recv(dat42(1,1,1,1), size(dat42), mpi_real, io_proc, 200*1+ks, comm, status, ierr)
                    call mpi_recv(dat43(1,1,1,1), size(dat43), mpi_real, io_proc, 200*2+ks, comm, status, ierr)
                 endif
              endif
           endif  !if ( my_proc_id == io_proc ) then
        endif  !if ( nrecord == 6 ) then   !u,v - 6,7

        if ( nrecord == 4 .or. nrecord == 5 .or. nrecord == 8 .or. &
             nrecord == 9 .or. nrecord == 11 ) then
           !---record 4 : t1-->T
           !---record 5 : Q1
           !---record 8 : DZDT
           !---record 9 : z1 --> DZ
           !---record 11: p1-->delp, p1(nx,ny,nz+1): (((p1(i,j,k),i=1,nx),j=1,ny),k=nz+1,1,-1)
           !---           p1-->ps
           iz=nz
           if ( my_proc_id == io_proc ) then
              !---get data
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

              !---send data to other cores
              if ( nprocs > 1 ) then
                 nm=max(1,int((iz+nprocs-1)/nprocs))  !devide iz to each processor
                 do k = 0, nprocs-1
                    ks=k*nm+1              !k-start
                    ke=k*nm+nm            !k-end
                    if ( ke > iz ) ke=iz
                    if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                       allocate(dat42(nx, ny, ke-ks+1,1))
                       dat42(:,:,:,1)=dat41(:,:,ks:ke,1)
                       if ( k /= io_proc ) then
                          call mpi_send(dat42(1,1,1,1), size(dat42), mpi_real, k, 3000+ks, comm, ierr)
                       else
                          allocate(dat43(nx, ny, ke-ks+1,1))
                          dat43=dat42
                       endif
                       deallocate(dat42)
                    endif
                 enddo
              else  !if ( nprocs > 1 ) then
                 allocate(dat43(nx, ny, iz,1))
                 dat43=dat41
              endif
              deallocate(dat41)
           else ! if ( my_proc_id == io_proc ) then
              !---receive dat43
              nm=max(1,int((iz+nprocs-1)/nprocs))
              ks=min(iz,my_proc_id*nm+1)
              ke=min(iz,my_proc_id*nm+nm)
              if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                 allocate(dat43(nx, ny, ke-ks+1,1))
                 call mpi_recv(dat43(1,1,1,1), size(dat43), mpi_real, io_proc, 3000+ks, comm, status, ierr)
              endif
           endif
        endif   !if ( nrecord == 4 .or. nrecord == 5 .or. nrecord == 8 .or. &

        !-----------------------------
        !---5.2 merge hafs restart and update restart files
        !---    note: need to change nesting domain's filenames
        if ( nrecord == 6 ) then  !u and v
           !---get u,v
           iz=nz
           nm=max(1,int((iz+nprocs-1)/nprocs))
           do nv = 1, 2
              if ( my_proc_id == io_proc ) then
                 allocate(dat4(ix+nv-1, iy+2-nv, iz, 1))  !u(ix, iy+1, iz, 1), v(ix+1, iy, iz, 1)
                 if ( nv == 1 ) then
                    call get_var_data(trim(ncfile_core), 'u', ix, iy+1, iz,1, dat4)
                 else if ( nv == 2 ) then
                    call get_var_data(trim(ncfile_core), 'v', ix+1, iy, iz,1, dat4)
                 endif
                 !---send to other core
                 if ( nprocs > 1 ) then
                    do k = 0, nprocs-1
                       ks=k*nm+1
                       ke=k*nm+nm            !k-end
                       if ( ke > iz ) ke=iz
                       if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                          if ( k /= io_proc ) then
                             allocate(dat41(ix+nv-1, iy+2-nv, ke-ks+1, 1))
                             dat41(:,:,1:ke-ks+1,1)=dat4(:,:,ks:ke,1)
                             call mpi_send(dat41(1,1,1,1), size(dat41), mpi_real, k, 200*nv+ks, comm, ierr)
                             deallocate(dat41)
                          else
                             allocate(dat44(ix+nv-1, iy+2-nv, ke-ks+1, 1))
                             dat44(:,:,1:ke-ks+1,1)=dat4(:,:,ks:ke,1)
                          endif  !if ( k /= io_proc ) then
                       endif  !if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                    enddo  !do k = 0, nprocs-1
                 else
                    allocate(dat44(ix+nv-1, iy+2-nv, iz, 1))
                    dat44=dat4
                 endif  !if ( nprocs > 1 ) then
                 deallocate(dat4)
              else  !if ( my_proc_id == io_proc ) then
                 !---receive u,v
                 ks=min(iz,my_proc_id*nm+1)
                 ke=min(iz,my_proc_id*nm+nm)
                 if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                    allocate(dat44(ix+nv-1, iy+2-nv, ke-ks+1, 1))
                    call mpi_recv(dat44, size(dat43), mpi_real, io_proc, 200*nv+ks, comm, status, ierr)
                 endif
              endif  !if ( my_proc_id == io_proc ) then

              ks=min(iz,my_proc_id*nm+1)
              ke=min(iz,my_proc_id*nm+nm)
              if ( nv == 1 ) then
                 allocate(u(ix, iy+1, ke-ks+1,1))
                 u(:,:,1:ke-ks+1,1)=dat44(:,:,1:ke-ks+1,1)
              else if ( nv == 2 ) then
                 allocate(v(ix+1, iy, ke-ks+1,1))
                 v(:,:,1:ke-ks+1,1)=dat44(:,:,1:ke-ks+1,1)
              endif
              deallocate(dat44)
           enddo  !do nv = 1, 2

           !---convert fv3grid to earth
           ks=min(iz,my_proc_id*nm+1)
           ke=min(iz,my_proc_id*nm+nm)
           allocate(dat4 (ix, iy+1, ke-ks+1, 1), dat41(ix+1, iy, ke-ks+1, 1))
           !$omp parallel do &
           !$omp& private(k)
           do k = 1, ke-ks+1
              call fv3uv2earth(ix, iy, u(:,:,k,1), v(:,:,k,1), cangu, sangu, cangv, sangv, dat4(:,:,k,1), dat41(:,:,k,1))
           enddo
           deallocate(u,v)

           !---merge
           allocate(u1(ix, iy+1, ke-ks+1, 1), v1(ix+1, iy, ke-ks+1, 1))
           u1=0.; v1=0.
           call combine_grids_for_remap(nx,ny,ke-ks+1,1,dat42,ix,iy+1,ke-ks+1,1,dat4,gwt%gwt_u,u1)
           call combine_grids_for_remap(nx,ny,ke-ks+1,1,dat43,ix+1,iy,ke-ks+1,1,dat41,gwt%gwt_v,v1)
           deallocate(dat42, dat43, dat4, dat41)

           !---convert earth wind to fv3grid wind
           allocate(u(ix, iy+1, ke-ks+1, 1), v(ix+1, iy, ke-ks+1, 1))
           u=-999999.; v=-99999999.;
           !$omp parallel do &
           !$omp& private(k)
           do k = 1, ke-ks+1
              call earthuv2fv3(ix, iy, u1(:,:,k,1), v1(:,:,k,1), cangu, sangu, cangv, sangv, u(:,:,k,1), v(:,:,k,1))
           enddo
           deallocate(u1,v1,cangu, sangu, cangv, sangv)

           !---send and collect
           if ( nprocs == 1 ) then
              allocate(u1(ix, iy+1, iz, 1), v1(ix+1, iy, iz, 1))
              u1=u
              v1=v
              deallocate(u,v)
           else
              nm=max(1,int((iz+nprocs-1)/nprocs))
              if ( my_proc_id /= io_proc ) then
                 call mpi_send(u(1,1,1,1),size(u),mpi_real, io_proc, 400*1+my_proc_id, comm, ierr)
                 call mpi_send(v(1,1,1,1),size(v),mpi_real, io_proc, 400*2+my_proc_id, comm, ierr)
                 deallocate(u,v)
              else
                 allocate(u1(ix, iy+1, iz, 1), v1(ix+1, iy, iz, 1))
                 do k = 0, nprocs-1
                    ks=k*nm+1              !k-start
                    ke=k*nm+nm            !k-end
                    if ( ke > iz ) ke=iz
                    if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                       if ( k /= io_proc ) then
                          allocate(dat41(ix, iy+1, ke-ks+1, 1), dat42(ix+1, iy, ke-ks+1, 1))
                          call mpi_recv(dat41(1,1,1,1), size(dat41), mpi_real, k, 400*1+k, comm, status, ierr)
                          call mpi_recv(dat42(1,1,1,1), size(dat42), mpi_real, k, 400*2+k, comm, status, ierr)
                          u1(:,:,ks:ke,1)=dat41(:,:,1:ke-ks+1,1)
                          v1(:,:,ks:ke,1)=dat42(:,:,1:ke-ks+1,1)
                          deallocate(dat41,dat42)
                       else
                          u1(:,:,ks:ke,1)=u(:,:,1:ke-ks+1,1)
                          v1(:,:,ks:ke,1)=v(:,:,1:ke-ks+1,1)
                          deallocate(u,v)
                       endif
                    endif
                 enddo   !do k = 0, nprocs-1
              endif
           endif   !if ( nprocs > 1 ) then  !need recv

           !---output
           if ( my_proc_id == io_proc ) then
              call update_hafs_restart(trim(ncfile_core), 'u', ix, iy+1, iz, 1, u1)
              call update_hafs_restart(trim(ncfile_core), 'v', ix+1, iy, iz, 1, v1)
              deallocate(u1, v1)
           endif
        elseif ( nrecord == 4 .or. nrecord == 5 .or. nrecord == 8 .or. &
                 nrecord == 9 .or. nrecord == 11 ) then
           iz=nz
           nm=max(1,int((iz+nprocs-1)/nprocs))
           if ( my_proc_id == io_proc ) then
              !---get restart data
              allocate(dat4(ix, iy, iz, 1))
              if ( nrecord == 4 ) call get_var_data(trim(ncfile_core), 'T', ix, iy, iz,1, dat4)
              if ( nrecord == 5 ) call get_var_data(trim(ncfile_tracer), 'sphum', ix, iy, iz,1, dat4)
              if ( nrecord == 8 ) call get_var_data(trim(ncfile_core), 'W', ix, iy, iz,1, dat4)
              if ( nrecord == 9 ) call get_var_data(trim(ncfile_core), 'DZ', ix, iy, iz,1, dat4)
              if ( nrecord == 11 ) call get_var_data(trim(ncfile_core), 'delp', ix, iy, iz,1, dat4)

              !---send data to other cores
              if ( nprocs > 1 ) then
                 nm=max(1,int((iz+nprocs-1)/nprocs))  !devide iz to each processor
                 do k = 0, nprocs-1
                    ks=k*nm+1              !k-start
                    ke=k*nm+nm            !k-end
                    if ( ke > iz ) ke=iz
                    if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                       allocate(dat41(ix, iy, ke-ks+1,1))
                       dat41(:,:,1:ke-ks+1,1)=dat4(:,:,ks:ke,1)
                       if ( k /= io_proc ) then
                          call mpi_send(dat41(1,1,1,1), size(dat41), mpi_real, k, 3000+ks, comm, ierr)
                       else
                          allocate(dat42(ix, iy, ke-ks+1,1))
                          dat42=dat41
                       endif
                       deallocate(dat41)
                    endif
                 enddo
              else  !if ( nprocs > 1 ) then
                 allocate(dat42(ix, iy, iz,1))
                 dat42=dat4
              endif
              deallocate(dat4)
           else ! if ( my_proc_id == io_proc ) then
              !---receive dat43
              ks=min(iz,my_proc_id*nm+1)
              ke=min(iz,my_proc_id*nm+nm)
              if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                 allocate(dat42(ix, iy, ke-ks+1,1))
                 call mpi_recv(dat42(1,1,1,1), size(dat42), mpi_real, io_proc, 3000+ks, comm, status, ierr)
              endif
           endif

           !---merge
           ks=min(iz,my_proc_id*nm+1)
           ke=min(iz,my_proc_id*nm+nm)
           allocate(dat4(ix, iy, ke-ks+1, 1))
           call combine_grids_for_remap(nx,ny,ke-ks+1,1,dat43,ix,iy,ke-ks+1,1,dat42,gwt%gwt_t,dat4)
           deallocate(dat43, dat42)

           !---collect data to io_proc
           if ( nprocs == 1 ) then
              allocate(dat41(ix, iy, iz, 1))
              dat41=dat4
           else
              nm=max(1,int((iz+nprocs-1)/nprocs))  !devide iz to each processor
              if ( my_proc_id /= io_proc ) then
                 call mpi_send(dat4(1,1,1,1),size(dat4),mpi_real, io_proc, 600+ks, comm, ierr)
              else
                 allocate(dat41(ix, iy, iz, 1))
                 do k = 0, nprocs-1
                    ks=k*nm+1
                    ke=k*nm+nm
                    if ( ke > iz ) ke=iz
                    if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                       if ( k /= io_proc ) then
                          allocate(dat42(ix, iy, ke-ks+1,1))
                          call mpi_recv(dat42(1,1,1,1), size(dat42), mpi_real, k, 600+ks, comm, status, ierr)
                          dat41(:,:,ks:ke,1)=dat42(:,:,1:ke-ks+1,1)
                          deallocate(dat42)
                       else
                          dat41(:,:,ks:ke,1)=dat4(:,:,1:ke-ks+1,1)
                       endif
                    endif
                 enddo
              endif   !if ( my_proc_id /= io_proc ) then
           endif  !if ( nprocs == 1 ) then

           !---output
           if ( my_proc_id == io_proc ) then
              !---update restartr
              if ( nrecord == 4 ) call update_hafs_restart(trim(ncfile_core), 'T', ix, iy, iz, 1, dat41)
              if ( nrecord == 5 ) call update_hafs_restart(trim(ncfile_tracer), 'sphum', ix, iy, iz, 1, dat41)
              if ( nrecord == 8 ) call update_hafs_restart(trim(ncfile_core), 'W', ix, iy, iz, 1, dat41)
              if ( nrecord == 9 ) call update_hafs_restart(trim(ncfile_core), 'DZ', ix, iy, iz, 1, dat41)
              if ( nrecord ==11 ) call update_hafs_restart(trim(ncfile_core), 'delp', ix, iy, iz, 1, dat41)
              deallocate(dat41)

              !---2d phis
              if ( nrecord == 9 .and. my_proc_id == io_proc ) then  !phis
                 allocate(phis2(ix, iy, 1, 1), dat41(ix, iy, 1, 1))
                 call get_var_data(trim(ncfile_core), 'phis', ix, iy, 1, 1, phis2)
                 call combine_grids_for_remap(nx,ny,1,1,phis1,ix,iy,1,1,phis2,gwt%gwt_t,dat41)
                 call update_hafs_restart(trim(ncfile_core), 'phis', ix, iy, 1, 1, dat41)
                 deallocate(phis1, phis2, dat41)
              endif

           endif  !if ( my_proc_id == io_proc ) then
           deallocate(dat4)
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
