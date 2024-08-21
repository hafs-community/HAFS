!========================================================================================
  subroutine hafs_diff(in_dir,in_dir2,in_date,out_file,nestdoms,vi_cloud)

!-----------------------------------------------------------------------------
! HAFS DA tool - hafs_diff
! Xu Lu -- 01/31/2024  Added to do increments for IAU
!                       Modified from sub_hafsvi_proc.f90
!
! Big Change/Update by JungHoon: Feb~July, 2023
! New input argument "vi_cloud" is introduced
! If input argument "vi_cloud" is 0, it will handle 17 variables
! If input argument "vi_cloud" is 1, it will handle 22 variables
! If input argument "vi_cloud" is 2, it will handle 24 variables
!
! 0: NO cloud changes in VI and handles only 17 variables as same as original
! sub_hafsvi_proc.f90
! 1: (GFDL microphysics) handles 5 more variables cloud water, rain water, ice,
! snow, graupel (22 variables)
! 2: Thompson microphysics: Need to handle two more additional variables (below)
! on the top of option 1
! ice water number concentration and rain number concentration (24 variables)
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
! 5 new variables added for GFDL microphysics: Feb, 2023 (below)
!      WRITE(IUNIT) (((qc(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
!      WRITE(IUNIT) (((qr(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
!      WRITE(IUNIT) (((qi(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
!      WRITE(IUNIT) (((qs(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
!      WRITE(IUNIT) (((qg(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
! 2 new addiontal variables added for Thompson microphysics: Jul, 2023 (below)
!      WRITE(IUNIT) (((NCI(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
!      WRITE(IUNIT) (((NCR(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)

!-----------------------------------------------------------------------------

  use constants
  use netcdf
  use module_mpi
  use var_type

  implicit none

  character (len=*),intent(in) :: in_dir,in_date,in_dir2,out_file,vi_cloud
  integer, intent(in)           :: nestdoms
!--- in_dir,  HAFS_restart_folder, which holds grid_spec.nc, fv_core.res.tile1.nc,
!             fv_srf_wnd.res.tile1.nc, fv_tracer.res.tile1.nc, phy_data.nc, sfc_data.nc
!--- in_date, HAFS_restart file date, like 20200825.120000
!--- radius,  to cut a square, default value is 40, which means a 40deg x 40deg square.
!--- out_file: output file, default is bin format, if the file name is *.nc, then output nc format.
  character (len=2500)   :: indir,indir2, infile,filename
  character (len=2500)   :: infile_fvcore, infile_core, infile_tracer, infile_phy, &
                            infile_sfc, infile_grid, infile_grid2, infile_atmos, infile_oro
  character (len=2500)   :: infile_core2, infile_tracer2
  type(grid2d_info)      :: dstgrid  ! rot-ll grid for output
  type(grid2d_info)      :: ingrid   ! hafs restart grid
  real     :: radiusf
  logical  :: file_exist

!----for hafs restart
  integer  :: ix, iy, iz, kz, ndom, nd
  integer  :: itotal_var ! Total number of variables handled in this
                         ! subrotine: option 2:24, option 1:22, option 0:17
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
  real, allocatable, dimension(:,:,:,:) :: dat4, dat41, dat42, dat43, u,v,dat4u,dat4v
  real, allocatable, dimension(:,:,:,:) :: dat4b, dat41b, dat42b, dat43b, ub, vb
  real, allocatable, dimension(:,:,:,:) :: dat41u,dat41v,dat42u,dat42v,dat43u,dat43v
  real, allocatable, dimension(:,:,:)   :: dat3, dat31
  real, allocatable, dimension(:,:)     :: dat2, dat21, sfcp
  real, allocatable, dimension(:)       :: dat1

  !real, allocatable, dimension(:)       :: pfull, phalf
  real, allocatable, dimension(:,:)     :: cangu, sangu, cangv, sangv
  real    :: cputime1, cputime2, cputime3
  integer :: io_proc, nm, ks, ke, nv
  character (len=50)  :: varname, varname_long, units, nzc

! If input argument is 0, only handle 17 variables.
  if (trim(vi_cloud) == '0') itotal_var=17
! If input argument is 1, handle 22 variables for GFDL microphysics.
  if (trim(vi_cloud) == '1') itotal_var=22
! If input argument is 2, handle 24 variables for Thompson microphysics.
  if (trim(vi_cloud) == '2') itotal_var=24

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

  if (len_trim(in_dir2) < 2 .or. trim(in_dir2) == 'w' .or. trim(in_dir2) == 'null')then
     indir2='.'
  else
     indir2=trim(in_dir2)
  endif

! 4 --- input grid info
!       read from grid file grid_spec.nc:
!       nestfl, tilefl: infile_core, infile_tracer, infile_grid, infile_atmos, infile_oro
  do_nestdom_loop: do nd = 2, ndom

     write(nestfl,'(a4,i2.2)')'nest',nd
     write(tilefl,'(a4,i0)')'tile',nd
     if ( nd == 1 ) then
        infile_grid=trim(indir)//'/grid_spec.nc'
        infile_grid2=trim(indir)//'/grid_mspec_'//in_date(1:4)//'_'//in_date(5:6)//'_'//in_date(7:8)//'_'//in_date(10:11)//'.nc'
        infile_core=trim(indir)//'/'//trim(in_date)//'.fv_core.res.tile1.nc'
        infile_tracer=trim(indir)//'/'//trim(in_date)//'.fv_tracer.res.tile1.nc'
        infile_core2=trim(indir2)//'/'//trim(in_date)//'.fv_core.res.tile1.nc'
        infile_tracer2=trim(indir2)//'/'//trim(in_date)//'.fv_tracer.res.tile1.nc'
        infile_atmos=trim(indir)//'/atmos_static.nc'
     else
        infile_grid=trim(indir)//'/grid_spec.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
        infile_grid2=trim(indir)//'/grid_mspec.'//trim(nestfl)//'_'//in_date(1:4)//'_'//in_date(5:6)//'_'//in_date(7:8)//'_'//in_date(10:11)//'.'//trim(tilefl)//'.nc'
        infile_core=trim(indir)//'/'//trim(in_date)//'.fv_core.res.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
        infile_tracer=trim(indir)//'/'//trim(in_date)//'.fv_tracer.res.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
        infile_core2=trim(indir2)//'/'//trim(in_date)//'.fv_core.res.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
        infile_tracer2=trim(indir2)//'/'//trim(in_date)//'.fv_tracer.res.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
        infile_atmos=trim(indir)//'/atmos_static.'//trim(nestfl)//'.'//trim(tilefl)//'.nc'
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
     call get_var_dim(trim(infile_atmos), 'pfull', ndims, dims)
     nz=dims(1)


     !---to add the test if the tc was inside of the domain


     !-----------------------------
     !---4.2 call FV3-grid cos and sin
     ! call FV3-grid cos and sin
     allocate( cangu(ix,iy+1),sangu(ix,iy+1),cangv(ix+1,iy),sangv(ix+1,iy) )
     call cal_uv_coeff_fv3(ix, iy, ingrid%grid_lat, ingrid%grid_lon, cangu, sangu, cangv, sangv)

     !-----------------------------
     !---4.3 calculate output-grid in input-grid's positions (xin, yin), and
     !each grid's weight to dst
     call cal_src_dst_grid_weight(ingrid, ingrid)

     !-------------------------------------------------------------------------
     call mpi_barrier(comm,ierr)

     ! 6 --- dst files
     if ( nd == 1 ) then
        fl_out=trim(out_file)//'.nc'      !current domain rot-ll file
     else if ( nd == 2 ) then
        fl_out=trim(out_file)//'_'//trim(nestfl)//'.nc'
     else
        write(tempfl,'(a4,i2.2)')'nest',nd-1
        fl_out=trim(out_file)//'_'//trim(nestfl)//'.nc'
     endif

     if ( my_proc_id == io_proc ) then
        call write_nc_dim(trim(fl_out), 'nx', ix)
        call write_nc_dim(trim(fl_out), 'ny', iy)
        call write_nc_dim(trim(fl_out), 'nz', nz)
     endif
     nx=ix
     ny=iy
     ! 7.2 --- remapp-needed variables:
     do_out_var_loop: do nrecord = 4, itotal_var
        if ( (nrecord > 11 .and. nrecord < 18) ) cycle do_out_var_loop

        iz=nz   !same vertical levels
        if ( nrecord == 12 .or. nrecord == 15 .or. nrecord == 16 .or. nrecord ==17 ) cycle do_out_var_loop

!        if ( nrecord ==  9 .or. nrecord == 11 ) iz=nz+1
        if ( nrecord == 4 ) then
           varname='T'
           units='K'
           varname_long='T_inc'
        elseif ( nrecord == 5 ) then
           varname='sphum'
           units='kg/kg'
           varname_long='sphum_inc'
        elseif ( nrecord == 8 ) then
           varname='dzdt'
           units='m/s'
           varname_long='dzdt_inc'
        elseif ( nrecord == 9 ) then
           varname='delp'
           units='pa'
           varname_long='delp_inc'
        elseif ( nrecord == 11 ) then
           varname='DZ'
           units='gpm'
           varname_long='delz_inc'
        elseif ( nrecord == 18 ) then
           varname='liq_wat'
           units='kg/kg'
           varname_long='liq_wat_inc'
        elseif ( nrecord == 19 ) then
           varname='rainwat'
           units='kg/kg'
           varname_long='rainwat_inc'
        elseif ( nrecord == 20 ) then
           varname='ice_wat'
           units='kg/kg'
           varname_long='ice_wat_inc'
        elseif ( nrecord == 21 ) then
           varname='snowwat'
           units='kg/kg'
           varname_long='snowwat_inc'
        elseif ( nrecord == 22 ) then
           varname='graupel'
           units='kg/kg'
           varname_long='graupel_inc'
        ! For Thompson microphysics nrecord ==23 & 24
        elseif ( nrecord == 23 ) then
           varname='ice_nc'
           units='/kg'
           varname_long='ice_nc_inc'
        elseif ( nrecord == 24 ) then
           varname='rain_nc'
           units='/kg'
           varname_long='rain_nc_inc'
        !-----------------------------
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
        if ( nrecord == 3 ) then
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
        !---7.18 record 18: (((qc(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        !---7.19 record 19: (((qr(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        !---7.20 record 20: (((qi(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        !---7.21 record 21: (((qs(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        !---7.22 record 22: (((qg(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        !---Below two variables are needed for Thompson microphysics
        !---7.23 record 23: (((nnqi(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        !---7.24 record 24: (((nnqr(i,j,k),i=1,nx),j=1,ny),k=nz,1,-1)
        if ( nrecord == 4 .or. nrecord == 5 .or. nrecord == 8    &
        .or. nrecord == 9 .or. nrecord == 11 )then !XL                    &
!        .or. nrecord ==18 .or. nrecord ==19 .or. nrecord ==20    &
!        .or. nrecord ==21 .or. nrecord ==22 .or. nrecord ==23    &
!        .or. nrecord ==24 ) then
           if ( my_proc_id == io_proc ) then
              !---read in
              allocate(dat4(ix, iy, iz,1),dat4b(ix, iy, iz,1))
              if ( nrecord == 4 ) call get_var_data(trim(infile_core), 'T', ix, iy, iz,1, dat4)
              if ( nrecord == 5 ) call get_var_data(trim(infile_tracer), 'sphum', ix, iy, iz,1, dat4)
              if ( nrecord == 8 ) call get_var_data(trim(infile_core), 'W', ix, iy, iz,1, dat4)
              if ( nrecord == 9 ) call get_var_data(trim(infile_core), 'delp', ix, iy, iz,1, dat4)
              if ( nrecord == 11 ) call get_var_data(trim(infile_core), 'DZ', ix, iy, iz,1, dat4)
              if ( nrecord == 18 ) call get_var_data(trim(infile_tracer), 'liq_wat', ix, iy, iz,1, dat4)
              if ( nrecord == 19 ) call get_var_data(trim(infile_tracer), 'rainwat', ix, iy, iz,1, dat4)
              if ( nrecord == 20 ) call get_var_data(trim(infile_tracer), 'ice_wat', ix, iy, iz,1, dat4)
              if ( nrecord == 21 ) call get_var_data(trim(infile_tracer), 'snowwat', ix, iy, iz,1, dat4)
              if ( nrecord == 22 ) call get_var_data(trim(infile_tracer), 'graupel', ix, iy, iz,1, dat4)
              ! For Thompson microphysics nrecord ==23 & 24
              if ( nrecord == 23 ) call get_var_data(trim(infile_tracer), 'ice_nc', ix, iy, iz,1, dat4)
              if ( nrecord == 24 ) call get_var_data(trim(infile_tracer), 'rain_nc', ix, iy, iz,1, dat4)
              !!!!! Read in second file
              if ( nrecord == 4 ) call get_var_data(trim(infile_core2), 'T', ix, iy, iz,1, dat4b)
              if ( nrecord == 5 ) call get_var_data(trim(infile_tracer2), 'sphum', ix, iy, iz,1, dat4b)
              if ( nrecord == 8 ) call get_var_data(trim(infile_core2), 'W', ix, iy, iz,1, dat4b)
              if ( nrecord == 9 ) call get_var_data(trim(infile_core2), 'delp', ix, iy, iz,1, dat4b)
              if ( nrecord == 11 ) call get_var_data(trim(infile_core2), 'DZ', ix, iy, iz,1, dat4b)
              if ( nrecord == 18 ) call get_var_data(trim(infile_tracer2),'liq_wat', ix, iy, iz,1, dat4b)
              if ( nrecord == 19 ) call get_var_data(trim(infile_tracer2),'rainwat', ix, iy, iz,1, dat4b)
              if ( nrecord == 20 ) call get_var_data(trim(infile_tracer2),'ice_wat', ix, iy, iz,1, dat4b)
              if ( nrecord == 21 ) call get_var_data(trim(infile_tracer2),'snowwat', ix, iy, iz,1, dat4b)
              if ( nrecord == 22 ) call get_var_data(trim(infile_tracer2),'graupel', ix, iy, iz,1, dat4b)
              ! For Thompson microphysics nrecord ==23 & 24
              if ( nrecord == 23 ) call get_var_data(trim(infile_tracer2),'ice_nc', ix, iy, iz,1, dat4b)
              if ( nrecord == 24 ) call get_var_data(trim(infile_tracer2),'rain_nc', ix, iy, iz,1, dat4b)

              !---send to other core
              if ( nprocs > 1 ) then
                 nm=max(1,int((iz+nprocs-1)/nprocs))
                 do k = 0, nprocs-1
                    ks=k*nm+1
                    ke=k*nm+nm            !k-end
                    if ( ke > iz ) ke=iz
                    if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                       allocate(dat42(ix, iy, ke-ks+1,1))
                       dat42(:,:,:,1)=dat4(:,:,ks:ke,1)-dat4b(:,:,ks:ke,1)
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
                 dat43=dat4-dat4b
              endif
              deallocate(dat4,dat4b)
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
                 if (nv==1) allocate(dat4b(ix, iy+1, iz,1))
                 if (nv==1) call get_var_data(trim(infile_core2), 'u', ix, iy+1, iz, 1, dat4b)
                 if (nv==2) allocate(dat4b(ix+1, iy, iz,1))
                 if (nv==2) call get_var_data(trim(infile_core2), 'v', ix+1, iy, iz, 1, dat4b)
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
                          dat41(:,:,:,1)=dat4(:,:,ks:ke,1)-dat4b(:,:,ks:ke,1)
                          if ( k /= io_proc ) then
                             call mpi_send(dat41(1,1,1,1), size(dat41), mpi_real, k, 200*nv+ks, comm, ierr)
                          else
                             if (nv==1) allocate(dat43u(ix, iy+1, ke-ks+1,1))
                             if (nv==1) dat43u=dat41
                             if (nv==2) allocate(dat43v(ix+1, iy, ke-ks+1,1))
                             if (nv==2) dat43v=dat41
                          endif
                          deallocate(dat41)
                       endif
                    enddo
                 else  !if ( nprocs > 1 ) then
                    if (nv==1) allocate(dat43u(ix, iy+1, iz,1))
                    if (nv==1) dat43u=dat4-dat4b !XL
                    if (nv==2) allocate(dat43v(ix+1, iy, iz,1))
                    if (nv==2) dat43v=dat4-dat4b
                 endif
                 deallocate(dat4,dat4b)
              enddo  !do nv = 1, 2
           else  !if ( my_proc_id == io_proc ) then
              !---receive dat43u dat43v
              nm=max(1,int((iz+nprocs-1)/nprocs))
              ks=min(iz,my_proc_id*nm+1)
              ke=min(iz,my_proc_id*nm+nm)
              if ( ks >= 1 .and. ks <= iz .and. ke >= 1 .and. ke <= iz ) then
                 allocate(dat43u(ix, iy+1, ke-ks+1,1), dat43v(ix+1, iy, ke-ks+1,1))
                 call mpi_recv(dat43u(1,1,1,1), size(dat43u), mpi_real, io_proc, 200*1+ks, comm, status, ierr)
                 call mpi_recv(dat43v(1,1,1,1), size(dat43v), mpi_real, io_proc, 200*2+ks, comm, status, ierr)
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
             nrecord == 8 .or. nrecord == 9 .or. nrecord ==11)then !XL .or. &
!             nrecord ==18 .or. nrecord ==19 .or. nrecord ==20 .or. &
!             nrecord ==21 .or. nrecord ==22 .or. nrecord ==23 .or. &
!             nrecord ==24 )then
           call mpi_barrier(comm,ierr)
           kz=nz
           !--- map fv3 grid to rot-ll grid: ingrid-->dstgrid
           !call cpu_time(cputime2)
           !write(*,'(a,i3,f)')' --- read rot-ll grid for 1 record ', nrecord, cputime2

           if ( nprocs == 1 ) then  !--no mpi
              !----only 1-core
              allocate(dat42(nx,ny,kz,1))
              dat42=dat43
!              call combine_grids_for_remap(ix,iy,kz,1,dat43,nx,ny,kz,1,dat43,gwt%gwt_t,dat42)
              !--- output
              !write(*,'(a,i2.2,a,200f)')'=== record',nrecord,': ', dat42(int(nx/2),int(ny/2),:,1)
              !write(flid_out) (((dat42(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
!              write(filename,*) 'dat_',trim(varname_long),'.txt'
!              open (111,file=trim(filename),form='formatted',status='unknown')
!              write(111,*)dat42(:,:,1,1)
              dat42(1:10,:,:,1)=0.; dat42(:,1:10,:,1)=0.; dat42(ix-10:ix,:,:,1)=0.; dat42(:,iy-10:iy,:,1)=0.
              call write_nc_real(trim(fl_out), trim(varname_long), nx, ny, kz, -1, 'nx', 'ny', trim(nzc), '-', dat42, trim(units), trim(varname_long))
!              close(111)
              deallocate(dat42, dat43)
           else
              !----mpi: 0 is for IO, >0 is for computing
              !---put dat41 --> dat42
              !call mpi_barrier(comm,ierr)
              nm=max(1,int((kz+nprocs-1)/nprocs))
              ks=my_proc_id*nm+1
              ke=my_proc_id*nm+nm
              if ( ke > kz ) ke=kz
              if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                 allocate(dat42(nx, ny, ke-ks+1,1))
                 dat42=dat43
                 if ( my_proc_id /= io_proc ) then
                    call mpi_send(dat42(1,1,1,1),size(dat42),mpi_real, io_proc, 5000+ks, comm, ierr)
                    deallocate(dat42)
                 endif
                 deallocate(dat43)
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
                 dat43(1:10,:,:,1)=0.; dat43(:,1:10,:,1)=0.; dat43(ix-10:ix,:,:,1)=0.; dat43(:,iy-10:iy,:,1)=0.
                 call write_nc_real(trim(fl_out), trim(varname_long), nx, ny, kz, -1, 'nx', 'ny', trim(nzc), '-', dat43, trim(units), trim(varname_long))
                 deallocate(dat43)
              endif  !if ( my_proc_id == io_proc ) then
           endif  ! if ( nprocs == 1 ) then  !--no mpi
        else if ( nrecord == 6 ) then  !---u,v
           allocate(u(nx,ny,kz,1), v(nx,ny,kz,1))
           if ( nprocs == 1 ) then  !--no mpi
              allocate(dat4u(nx,ny+1,kz,1),dat4v(nx+1,ny,kz,1))
              dat4u=dat43u
              dat4v=dat43v
              allocate(dat2(ix, iy+1), dat21(ix+1, iy))
              do k=1,kz
                 call fv3uv2earth(ix, iy, dat4u(:,:,k,1), dat4v(:,:,k,1), cangu, sangu, cangv, sangv, dat2, dat21)
                 u(:,:,k,1)=(dat2 (:,1:iy)+dat2 (:,2:iy+1))/2.0
                 v(:,:,k,1)=(dat21(1:ix,:)+dat21(2:ix+1,:))/2.0
              end do
              u(1:10,:,:,1)=0.; u(:,1:10,:,1)=0.; u(ix-10:ix,:,:,1)=0.; u(:,iy-10:iy,:,1)=0.
              v(1:10,:,:,1)=0.; v(:,1:10,:,1)=0.; v(ix-10:ix,:,:,1)=0.; v(:,iy-10:iy,:,1)=0.
              call write_nc_real(trim(fl_out), 'u_inc', nx, ny, kz, -1, 'nx', 'ny', trim(nzc), '-', u, 'm/s', 'u-component')
              call write_nc_real(trim(fl_out), 'v_inc', nx, ny, kz, -1, 'nx', 'ny', trim(nzc), '-', v, 'm/s', 'u-component')
              deallocate(dat4u,dat4v,dat2,dat21,dat43u,dat43v,cangu,sangu,cangv,sangv,u,v)
           else
              !---collect dat43 to io_proc, and output
              call mpi_barrier(comm,ierr)
              nm=max(1,int((kz+nprocs-1)/nprocs))
              ks=my_proc_id*nm+1
              ke=my_proc_id*nm+nm
              if ( ke > kz ) ke=kz
              if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                 allocate(dat42u(nx, ny, ke-ks+1,1),dat42v(nx, ny, ke-ks+1,1))
                 dat42u=dat43u
                 dat42v=dat43v
                 if ( my_proc_id /= io_proc ) then
                    call mpi_send(dat42u(1,1,1,1),size(dat42u),mpi_real, io_proc, 5000+ks, comm, ierr)
                    call mpi_send(dat42v(1,1,1,1),size(dat42v),mpi_real, io_proc, 9000+ks, comm, ierr)
                    deallocate(dat42u,dat42v)
                 endif
                 deallocate(dat43u,dat43v)
              endif

              if ( my_proc_id == io_proc ) then
                 allocate(dat43u(nx,ny+1,kz,1),dat43v(nx+1,ny,kz,1))
                 nm=max(1,int((kz+nprocs-1)/nprocs))
                 do k = 0, nprocs-1
                    ks=k*nm+1
                    ke=k*nm+nm
                    if ( ke > kz ) ke=kz
                    if ( ks >= 1 .and. ks <= kz .and. ke >= 1 .and. ke <= kz ) then
                       if ( k /= io_proc ) then
                          allocate(dat41u(nx,ny+1,ke-ks+1,1),dat41v(nx+1,ny,ke-ks+1,1))
                          call mpi_recv(dat41u(1,1,1,1),size(dat41u),mpi_real,k,5000+ks,comm,status,ierr)
                          call mpi_recv(dat41v(1,1,1,1),size(dat41v),mpi_real,k,9000+ks,comm,status,ierr)
                          dat43u(:,:,ks:ke,1)=dat41u(:,:,1:ke-ks+1,1)
                          dat43v(:,:,ks:ke,1)=dat41v(:,:,1:ke-ks+1,1)
                          deallocate(dat41u,dat41v)
                       else
                          dat43u(:,:,ks:ke,1)=dat42u(:,:,1:ke-ks+1,1)
                          dat43v(:,:,ks:ke,1)=dat42v(:,:,1:ke-ks+1,1)
                          deallocate(dat42u,dat42v)
                       endif
                    endif
                 enddo
                 allocate(dat4u(nx,ny+1,kz,1),dat4v(nx+1,ny,kz,1))
                 dat4u=dat43u
                 dat4v=dat43v
                 allocate(dat2(ix, iy+1), dat21(ix+1, iy))
                 do k=1,kz
                    call fv3uv2earth(ix, iy, dat43u(:,:,k,1), dat43v(:,:,k,1), cangu,sangu, cangv, sangv, dat2, dat21)
                    u(:,:,k,1)=(dat2 (:,1:iy)+dat2 (:,2:iy+1))/2.0
                    v(:,:,k,1)=(dat21(1:ix,:)+dat21(2:ix+1,:))/2.0
                 end do
                 u(1:10,:,:,1)=0.; u(:,1:10,:,1)=0.; u(ix-10:ix,:,:,1)=0.; u(:,iy-10:iy,:,1)=0.
                 v(1:10,:,:,1)=0.; v(:,1:10,:,1)=0.; v(ix-10:ix,:,:,1)=0.; v(:,iy-10:iy,:,1)=0.
                 call write_nc_real(trim(fl_out), 'u_inc', nx, ny, kz, -1, 'nx', 'ny', trim(nzc), '-', u, 'm/s', 'u-component')
                 call write_nc_real(trim(fl_out), 'v_inc', nx, ny, kz, -1, 'nx', 'ny', trim(nzc), '-', v, 'm/s', 'u-component')
                 deallocate(dat43u,dat43v)
                 deallocate(dat2,dat21,cangu,sangu,cangv,sangv,u,v)
              endif  !if ( my_proc_id == io_proc ) then
           endif
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
                 dat41(1:10,:,:,1)=0.; dat41(:,1:10,:,1)=0.; dat41(ix-10:ix,:,:,1)=0.; dat41(:,iy-10:iy,:,1)=0.
              else
                 dat41=-999999.
              endif
              !write(flid_out) (((dat42(i,j,k,1),i=1,nx),j=1,ny),k=kz,1,-1)
              call write_nc_real(trim(fl_out), trim(varname_long), nx, ny, -1, -1, 'nx', 'ny', '-', '-', dat41, trim(units), trim(varname_long))
              deallocate(dat41, dat42, dat43)
           endif  !if ( my_proc_id == io_proc ) then
        endif

     enddo do_out_var_loop !: for nrecord = 1, itotal_var  !We have 17 or 22 or 24 variables now

     !-------------------------------------------------------------------------
     deallocate( ingrid%grid_lon, ingrid%grid_lat, ingrid%grid_lont, ingrid%grid_latt)

  enddo do_nestdom_loop !: do nd = 1, ndom
  write(*,*)' === finished hafs_diff ==='

  return
  end subroutine hafs_diff

