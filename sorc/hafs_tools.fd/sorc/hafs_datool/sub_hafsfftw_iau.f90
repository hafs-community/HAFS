!========================================================================================
  subroutine hafsfftw_iau(an_file, grid_file, bg_file, out_file, wave_num, vars)

!-----------------------------------------------------------------------------
! HAFS DA tool - fftw_iau
! authors and history:
!      -- 202401, Xu Lu added hafs increment calculation for IAU
!      -- 202409, JungHoon Shin added FFTW specture-filter for inc=rements
!      -- 202410, Yonghui Weng re-coded this subroutine
!
!-----------------------------------------------------------------------------
! This subroutine calculates DA increments of an_file - bg_file, and runs FFTW.
! an_file : analysis file in hafs restart format
! bg_file : background file in hafs restart format
! out_file: file.nc, increment file in nc format, all variables are in dot-grid (t-grid)
!           if len_trim(out_file) < 4, will not write out increment to this file, will update
!              an_file with bg+incr. So if wanted to keep the original DA analysis, need to copy
!              an_file before run hafs_datool.
! wave_num: Fourier wave decomposition number, wave_num=6, the code will calculate the summation
!           of wavenumber 0,1,2,3,4,5, and 6 components. If wave_num is 2, it will calculate the
!           summation of wave number 0,1, and 2 components.
!           if wave_num < 0, will not run fftw
!           if wave_num > 30, wave_num = 2
!-----------------------------------------------------------------------------

  use constants
  use netcdf
  use module_mpi
  use var_type

  implicit none
  character (len=*), intent(in) :: an_file, grid_file, bg_file, out_file, vars
  integer, intent(in)           :: wave_num

  type(grid2d_info)  :: grid
  type(llxy_cons)    :: gt
  integer            :: tc_grid_x, tc_grid_y
  !logical            :: outside

  real               :: earth_dist
  integer            :: io_proc, kxdimwrite, i, j, k, n, m(1), fwd_radius, n1, nm, nv
  integer, allocatable, dimension(:) :: iindex, jindex
  real, allocatable, dimension(:)    :: dis

  integer            :: ix, jx, kx, tx, kxo
  real, allocatable, dimension(:,:,:,:) :: dat41, dat42, fdat_an, fdat_bg, fdat_incr, fdat_incru, fdat_incrv
  real, allocatable, dimension(:,:,:,:) :: tmp4d
  real, allocatable, dimension(:,:,:,:) :: fdat_anu, fdat_anv, fdat_bgu, fdat_bgv
  real, allocatable, dimension(:,:)     :: cangu, sangu, cangv, sangv
  real, allocatable, dimension(:,:)     :: dat21, dat22, vr, vt, vtr, vrr
  character(len=nf90_max_name) :: varname  !, dimname
  character(len=50)  :: strrep
  integer            :: anid, bgid, varid, ndims, nvars, xtype, dimids(5), vdim(5), nvarindex
  integer            :: wndsize, nx, ixv, jxv
  real               :: pil, xc, yc, xr, sta, sta1, a, b
  character(len=3)   :: kxc

!------------------------------------------------------------------------------
! 1 --- arg process and parameters
  io_proc=nprocs-1
  fwd_radius=380   !model grid number for FWD radius centerized the TC
  tc_grid_x=-99
  tc_grid_y=-99

!------------------------------------------------------------------------------
! 2 --- input grid info: read from grid file grid_spec.nc
  if ( len_trim(grid_file) > 2 ) then
     call rd_grid_spec_data(trim(grid_file), grid)
     allocate( cangu(grid%grid_xt,grid%grid_y),sangu(grid%grid_xt,grid%grid_y) )
     allocate( cangv(grid%grid_x,grid%grid_yt),sangv(grid%grid_x,grid%grid_yt) )
     call cal_uv_coeff_fv3(grid%grid_xt, grid%grid_yt, grid%grid_lat, grid%grid_lon, cangu, sangu, cangv, sangv)
  endif

!------------------------------------------------------------------------------
! 3 --- calculate tc position in model grid: (tc%lat,tc%lon) to (tc_grid_y,tc_grid_x)
  if ( abs(wave_num) < 99 .and. len_trim(grid_file) > 2 ) then
     !call general_create_llxy_transform(grid%grid_latt, grid%grid_lont, grid%grid_yt, grid%grid_xt, gt)
     !call general_tll2xy(gt, tc%lon, tc%lat, tc_grid_x, tc_grid_y, outside)

     allocate( iindex(grid%grid_yt*grid%grid_xt), jindex(grid%grid_yt*grid%grid_xt) )
     allocate( dis(grid%grid_yt*grid%grid_xt) )
     n=0
     do j = 1, grid%grid_yt
     do i = 1, grid%grid_xt
         n=n+1
         iindex(n)=i
         jindex(n)=j
         dis(n)=earth_dist(grid%grid_lont(j,i), grid%grid_latt(j,i), tc%lon, tc%lat)
     enddo
     enddo
     m = minloc(dis)
     n = m(1)
     tc_grid_x=iindex(n)
     tc_grid_y=jindex(n)
     !write(*,*)' nearest dot: ', iindex(n), jindex(n), grid%grid_lont(jindex(n),iindex(n)), &
     !           grid%grid_latt(jindex(n),iindex(n))

     write(*,*)' tc position: ', tc%lon, tc%lat, tc_grid_x, tc_grid_y
     !if ( outside ) then
     !   write(*,*)' !!!! warning: tc center is located outside of the domain, please check'
     !   tc_grid_x = int(grid%grid_xt/2)
     !   tc_grid_y = int(grid%grid_yt/2)
     !endif

     ! ---- re-size fwd_radius
     fwd_radius=min(fwd_radius, tc_grid_x-1, grid%grid_xt-tc_grid_x, tc_grid_y-1, grid%grid_yt-tc_grid_y)
     write(*,*)' fwd_radius = ',fwd_radius
     write(*,'(a,i4,a1,i4,a1,i4,a1,i4)')' fwd bin : ', tc_grid_x-fwd_radius, ':', tc_grid_x+fwd_radius, &
                                                  ';', tc_grid_y-fwd_radius, ':', tc_grid_y+fwd_radius
  endif  ! if ( abs(wave_num) < 99 ) then

!------------------------------------------------------------------------------
! 4 --- open file & output dimensions to increment-file
  !call nccheck(nf90_open(trim(an_file), nf90_nowrite, anid), 'wrong in open '//trim(an_file), .true.)
  call nccheck(nf90_open(trim(bg_file), nf90_nowrite, bgid), 'wrong in open '//trim(bg_file), .true.)
  call nccheck(nf90_inquire(bgid, ndims, nvars), 'wrong in inquire anid', .true.)
  !dimensions of fv3 restart:
  !            grid_spec.nc: grid_xt = 2880, grid_yt = 2400, grid_x = 2881, grid_y = 2401
  !    fv_core.res.tile1.nc: xaxis_1 = 2880, yaxis_2 = 2400, xaxis_2 = 2881, yaxis_1 = 2401, zaxis_1 = 91
  ! fv_srf_wnd.res.tile1.nc: xaxis_1 = 2880, yaxis_1 = 2400
  !  fv_tracer.res.tile1.nc: xaxis_1 = 2880, yaxis_1 = 2400, zaxis_1 = 91
  !             phy_data.nc: xaxis_1 = 2880, yaxis_1 = 2400, zaxis_1 = 91
  !             sfc_data.nc: xaxis_1 = 2880, yaxis_1 = 2400, zaxis_1 = 4

  if ( len_trim(out_file) > 3 .and. my_proc_id == io_proc ) then
     call write_nc_dim(trim(out_file), 'nx', grid%grid_xt)
     call write_nc_dim(trim(out_file), 'ny', grid%grid_yt)
     !call write_nc_dim(trim(out_file), 'nz', nz)
  endif

!------------------------------------------------------------------------------
! 5 --- variable parallel computing loop
  kxdimwrite=-1
  nm=max(1,int((nvars+nprocs-1)/nprocs))
  do_input_var_loop: do n1=1, nm
     nv=(n1-1)*nprocs+my_proc_id+1
     if ( nv > nvars ) exit do_input_var_loop

     ! 5.1 --- get variable's dimension
     dimids=-1; vdim=-1
     call nccheck(nf90_inquire_variable(bgid,nv,varname,xtype,ndims,dimids), &
                  'wrong in inquire_variable '//trim(varname), .false.)
     write(*,*)' my_proc_id,nprocs,nv,varname: ',my_proc_id,nprocs,nv, trim(varname)


     nvarindex=-99
     if ( trim(vars) == 'all' .or. trim(vars) == 'ALL' .or. len_trim(vars) <1 ) nvarindex=1
     if ( nvarindex<1 ) nvarindex=index(':'//trim(vars)//':', ':'//trim(varname)//':')
     if ( nvarindex<1 ) nvarindex=index(','//trim(vars)//',', ','//trim(varname)//',')
     if ( nvarindex<1 ) cycle do_input_var_loop

     do i = 1, ndims
        call nccheck(nf90_inquire_dimension(bgid,dimids(i), len=vdim(i)), 'wrong in inquire '//trim(varname)//' dim', .false.)
     enddo
     !write(*,'(a,9i6)')'nf90_inquire_variable:  '//trim(varname), ndims, vdim(1:ndims), xtype

     !---skip 1-dimension variable: xaxis_1(xaxis_1), yaxis_1(yaxis_1), zaxis_1(zaxis_1)
     if ( ndims < 2 ) cycle do_input_var_loop

     ! 5.2 --- determine grids
     if ( ndims == 4 ) then
        ix=vdim(1); jx=vdim(2); kx=vdim(3); tx=vdim(4)
     else if ( ndims == 3) then
        ix=vdim(1); jx=vdim(2); kx=vdim(3); tx=1    !here kxi is time dimension
     else if ( ndims == 2) then
        ix=vdim(1); jx=vdim(2); kx=1; tx=1   !grid_lon, grid_lat, grid_lont, grid_latt, area
     else if ( ndims == 1) then
        ix=vdim(1); jx=1; kx=1; tx=1   !
     else
        write(*,*)' --- var '//trim(varname)//' could not be processed'
        cycle do_input_var_loop
     endif
     if ( len_trim(grid_file) > 2 .and. ix /= grid%grid_x .and. ix /= grid%grid_xt .and. ix /= grid%grid_y .and. ix /= grid%grid_yt ) then
        write(*,*)' --- var '//trim(varname)//' dimension does not match the input grids'
        write(*,'(a,2i5)')' --- input grids dimensions: ', grid%grid_xt, grid%grid_yt
        write(*,'(a,2i5)')' --- '//trim(varname)//' dimension: ', ix, jx
        cycle do_input_var_loop
     endif
     if ( len_trim(out_file) > 3 .and. kx > 1 .and. kxdimwrite < 1) then
        call write_nc_dim(trim(out_file), 'nz', kx)
        kxdimwrite=1
     endif
     kxc='nz'; kxo=kx
     if ( kx<=1) then
        kxo=-kx
        kxc='-  '
     endif
     !----fwd_radius=380
     fwd_radius=min(fwd_radius,int(ix/2-1), int(jx/2-1))
     nx=2*fwd_radius+1
     wndsize=int(1.5*nx)  ! domain size for Fourier wave decomposition
     if(mod(wndsize,2).eq.0) wndsize=wndsize+1
     if ( tc_grid_y > jx .or. tc_grid_y < 1 ) tc_grid_y = int(jx/2)
     if ( tc_grid_x > ix .or. tc_grid_x < 1 ) tc_grid_x = int(ix/2)

     ! 5.3 ---
     if ( trim(varname) == 'u' .or. trim(varname) == 'ua' ) then ! u/v
        !write(*,*)'====w001 =='//trim(strrep(varname,'u','v'))//'===='
        !write(*,*)'====w002 =='//'v'//trim(varname(2:2))//'===='
        !---get an data
        ixv=0; jxv=0
        if ( trim(varname) == 'u' ) then
           ixv=1; jxv=-1
        endif
        allocate(dat41(ix, jx, kx, tx), dat42(ix+ixv, jx+jxv, kx, tx))
        call get_var_data(trim(an_file), trim(varname), ix, jx, kx, tx, dat41)
        call get_var_data(trim(an_file), 'v'//trim(varname(2:2)), ix+ixv, jx+jxv, kx, tx, dat42)
        allocate(fdat_anu(ix, jx, kx, tx), fdat_anv(ix+ixv, jx+jxv, kx, tx))
        if ( trim(varname) == 'u' .and. len_trim(grid_file) > 2 ) then
           !---convert u,v from fv3grid to earth
           do n = 1, tx; do k = 1, kx
              call fv3uv2earth(ix, jx-1, dat41(1:ix,1:jx,k,n), dat42(1:ix+1,1:jx-1,k,n), cangu, sangu, cangv, sangv, fdat_anu(1:ix,1:jx,k,n), fdat_anv(1:ix+1,1:jx-1,k,n))
           enddo; enddo
        else
           fdat_anu=dat41
           fdat_anv=dat42
        endif
        deallocate(dat41, dat42)

        !---get bg data
        allocate(dat41(ix, jx, kx, tx), dat42(ix+ixv, jx+jxv, kx, tx))
        call get_var_data(trim(bg_file), trim(varname), ix, jx, kx, tx, dat41)
        call get_var_data(trim(bg_file), 'v'//trim(varname(2:2)), ix+ixv, jx+jxv, kx, tx, dat42)
        allocate(fdat_bgu(ix, jx, kx, tx), fdat_bgv(ix+ixv, jx+jxv, kx, tx))
        if ( trim(varname) == 'u' .and. len_trim(grid_file) > 2 ) then
           !---convert u,v from fv3grid to earth
           do n = 1, tx; do k = 1, kx
              call fv3uv2earth(ix, jx-1, dat41(1:ix,1:jx,k,n), dat42(1:ix+1,1:jx-1,k,n), cangu, sangu, cangv, sangv, fdat_bgu(1:ix,1:jx,k,n), fdat_bgv(1:ix+1,1:jx-1,k,n))
           enddo; enddo
        else
           fdat_bgu=dat41
           fdat_bgv=dat42
        endif
        deallocate(dat41, dat42)

        !---calculate increments
        allocate(fdat_incru(ix, jx, kx, tx), fdat_incrv(ix+ixv, jx+jxv, kx, tx))
        fdat_incru=fdat_anu-fdat_bgu
        fdat_incrv=fdat_anv-fdat_bgv

        !---small domain for Fourier decomposition
        if ( abs(wave_num) < 99 ) then
           allocate(dat21(nx,nx), dat22(nx,nx), vt(nx,nx), vr(nx,nx), vtr(nx,nx), vrr(nx,nx))
           pil=4.0*atan(1.0)
           do n = 1, tx; do k = 1, kx
              dat21=0.0; dat22=0.0
              dat21(11:nx-10,11:nx-10)=fdat_incru(tc_grid_x-fwd_radius+10:tc_grid_x+fwd_radius-10,tc_grid_y-fwd_radius+10:tc_grid_y+fwd_radius-10,k,n)
              dat22(11:nx-10,11:nx-10)=fdat_incrv(tc_grid_x-fwd_radius+10:tc_grid_x+fwd_radius-10,tc_grid_y-fwd_radius+10:tc_grid_y+fwd_radius-10,k,n)

              !---ua/va --> vt/vr
              vt=0.; vr=0.
              do j = 1,nx; do i = 1,nx
                 xc=float(j)-tc_grid_x
                 yc=float(i)-tc_grid_y
                 xr=sqrt(xc*xc+yc*yc)
                 if ( xr < fwd_radius ) then
                    if ( xc.ne.0.0 ) sta1=abs(atan(yc/xc))
                    if(xc.ge.0.0.and.yc.eq.0.0) sta=0.0
                    if(xc.eq.0.0.and.yc.ge.0.0) sta=pil/2.0
                    if(xc.eq.0.0.and.yc.lt.0.0) sta=pil*1.5
                    if(xc.lt.0.0.and.yc.eq.0.0) sta=pil
                    if(xc.gt.0.0.and.yc.ge.0.0) sta=sta1
                    if(xc.lt.0.0.and.yc.gt.0.0) sta=pil-sta1
                    if(xc.lt.0.0.and.yc.lt.0.0) sta=pil+sta1
                    if(xc.gt.0.0.and.yc.lt.0.0) sta=pil*2.0-sta1
                    a=sin(sta)
                    b=cos(sta)
                    vt(j,i)=dat22(j,i)*b-dat21(j,i)*a
                    vr(j,i)=dat22(j,i)*a+dat21(j,i)*b
                 endif
              enddo; enddo

              !---vt/vr decom to vtr/vrr
              vtr=0.; vrr=0.
              call decom(vt, vtr, nx, nx, wndsize, wave_num, 0)
              call decom(vr, vrr, nx, nx, wndsize, wave_num, 0)

              !---vtr/vrr --> ua/va
              dat21=0.; dat22=0.
              do j = 1,nx; do i = 1,nx
                 xc=float(j)-tc_grid_x
                 yc=float(i)-tc_grid_y
                 xr=sqrt(xc*xc+yc*yc)
                 if ( xr < fwd_radius ) then
                    if ( xc.ne.0.0 ) sta1=abs(atan(yc/xc))
                    if(xc.ge.0.0.and.yc.eq.0.0) sta=0.0
                    if(xc.eq.0.0.and.yc.ge.0.0) sta=pil/2.0
                    if(xc.eq.0.0.and.yc.lt.0.0) sta=pil*1.5
                    if(xc.lt.0.0.and.yc.eq.0.0) sta=pil
                    if(xc.gt.0.0.and.yc.ge.0.0) sta=sta1
                    if(xc.lt.0.0.and.yc.gt.0.0) sta=pil-sta1
                    if(xc.lt.0.0.and.yc.lt.0.0) sta=pil+sta1
                    if(xc.gt.0.0.and.yc.lt.0.0) sta=pil*2.0-sta1
                    a=sin(sta)
                    b=cos(sta)
                    dat21(j,i)=vrr(j,i)*b-vtr(j,i)*a
                    dat22(j,i)=vrr(j,i)*a+vtr(j,i)*b
                 endif
              enddo; enddo

              !---push dat21/dat22 to fdat_incru/fdat_incrv
              fdat_incru(:,:,k,n)=0.; fdat_incrv(:,:,k,n)=0.
              fdat_incru(tc_grid_x-fwd_radius:tc_grid_x+fwd_radius,tc_grid_y-fwd_radius:tc_grid_y+fwd_radius,k,n)=dat21(:,:)
              fdat_incrv(tc_grid_x-fwd_radius:tc_grid_x+fwd_radius,tc_grid_y-fwd_radius:tc_grid_y+fwd_radius,k,n)=dat22(:,:)
           enddo; enddo  !do n = 1, tx; do k = 1, kx
           deallocate(dat21, dat22, vt, vr, vtr, vrr)
        else
           !---from Xu Lu: dat42(1:10,:,:,1)=0.; dat42(:,1:10,:,1)=0.; dat42(ix-10:ix,:,:,1)=0.; dat42(:,iy-10:iy,:,1)=0.
           fdat_incru(1:10,:,:,:)=0.; fdat_incru(:,1:10,:,:)=0.; fdat_incru(ix-10:ix,:,:,:)=0.; fdat_incru(:,jx-10:jx,:,:)=0.
           fdat_incrv(1:10,:,:,:)=0.; fdat_incrv(:,1:10,:,:)=0.; fdat_incrv(ix+ixv-10:ix+ixv,:,:,:)=0.; fdat_incrv(:,jx+jxv-10:jx+jxv,:,:)=0.
        endif  !if ( abs(wave_num) < 99 ) then

        !---output
        if ( len_trim(out_file) <= 2 ) then ! update an file -- write full u/v back
           fdat_anu=fdat_bgu+fdat_incru
           fdat_anv=fdat_bgv+fdat_incrv
           allocate(dat41(ix, jx, kx, tx), dat42(ix+ixv, jx+jxv, kx, tx))
           if ( trim(varname) == 'u' .and. len_trim(grid_file) > 2 ) then
              !---convert earth wind to fv3grid wind
              do n = 1, tx; do k = 1, kx
                 call earthuv2fv3(ix, jx-1, fdat_anu(:,:,k,n), fdat_anv(:,:,k,n), cangu, sangu, cangv, sangv, dat41(:,:,k,n), dat42(:,:,k,n))
              enddo; enddo
           else
              dat41=fdat_anu
              dat42=fdat_anv
           endif
           !---update anlysis file
           call update_hafs_restart(trim(an_file), trim(varname), ix, jx, kx, tx, dat41)
           call update_hafs_restart(trim(an_file), 'v'//trim(varname(2:2)), ix+ixv, jx+jxv, kx, tx, dat42)
           deallocate(dat41, dat42)
        else  ! output to an incremental file
           if ( trim(varname) == 'u' ) then
              allocate(tmp4d(1:ix,1:jx-1,1:kx,1))
              do k = 1, kx
              do j = 1, jx-1
              do i = 1, ix
                 tmp4d(i,j,k,1)=0.5*(fdat_incru(i,j,k,1)+fdat_incru(i,j+1,k,1))
              enddo
              enddo
              enddo
            ! call write_nc_real(trim(out_file),'u_inc',ix,jx-1,kx,-1,'nx','ny','nz','-', &
            !                     0.5*(fdat_incru(1:ix,1:jx-1,1:kx,1)+fdat_incru(1:ix,2:jx,1:kx,1)),'m/s','u_inc')
              call write_nc_real(trim(out_file),'u_inc',ix,jx-1,kx,-1,'nx','ny','nz','-', &
                                 tmp4d,'m/s','u_inc')
              do k = 1, kx
              do j = 1, jx-1
              do i = 1, ix
                 tmp4d(i,j,k,1)=0.5*(fdat_incrv(i,j,k,1)+fdat_incrv(i+1,j,k,1))
              enddo
              enddo
              enddo
             ! call write_nc_real(trim(out_file),'v_inc',ix,jx-1,kx,-1,'nx','ny','nz','-', &
             !                    0.5*(fdat_incrv(1:ix,1:jx-1,1:kx,1)+fdat_incrv(2:ix+1,1:jx-1,1:kx,1)),'m/s','v_inc')
              call write_nc_real(trim(out_file),'v_inc',ix,jx-1,kx,-1,'nx','ny','nz','-', &
                                 tmp4d,'m/s','v_inc')
              deallocate(tmp4d)
           else
              call write_nc_real(trim(out_file),'ua_inc',ix,jx,kx,-1,'nx','ny','nz','-', fdat_incru,'m/s','ua_inc')
              call write_nc_real(trim(out_file),'va_inc',ix,jx,kx,-1,'nx','ny','nz','-', fdat_incrv,'m/s','ua_inc')
           endif
        endif
        deallocate(fdat_anu, fdat_anv, fdat_bgu, fdat_bgv, fdat_incru, fdat_incrv)
     else if ( trim(varname) == 'v' .or. trim(varname) == 'va' ) then
        cycle do_input_var_loop
     else  !---dot
        if ( allocated(fdat_an) ) deallocate(fdat_an)
        allocate(fdat_an(ix, jx, kx, tx))
        call get_var_data(trim(an_file), trim(varname), ix, jx, kx, tx, fdat_an)
        if ( allocated(fdat_bg) ) deallocate(fdat_bg)
        allocate(fdat_bg(ix, jx, kx, tx))
        call get_var_data(trim(bg_file), trim(varname), ix, jx, kx, tx, fdat_bg)
        allocate(fdat_incr(ix, jx, kx, tx))
        fdat_incr=fdat_an-fdat_bg

        !---small domain for Fourier decomposition
        if ( abs(wave_num) < 99 ) then
           allocate(dat21(nx,nx), dat22(nx,nx))
           do n = 1, tx; do k = 1, kx
              dat21=0.0; dat22=0.0
              dat21(11:nx-10,11:nx-10)=fdat_incr(tc_grid_x-fwd_radius+10:tc_grid_x+fwd_radius-10,tc_grid_y-fwd_radius+10:tc_grid_y+fwd_radius-10,k,n)
              call decom(dat21,dat22,nx,nx,wndsize,wave_num,0)
              fdat_incr(:,:,k,n)=0.0
              do j = 1,nx; do i = 1,nx
                 xc=float(j)-1.0*tc_grid_x; yc=float(i)-1.0*tc_grid_y
                 xr=sqrt(xc*xc+yc*yc)
                 if ( xr < fwd_radius ) then
                    !fdat_incr(tc_grid_x-fwd_radius:tc_grid_x+fwd_radius,tc_grid_y-fwd_radius:tc_grid_y+fwd_radius,k,n)=dat22
                    fdat_incr(tc_grid_x-fwd_radius+i-1, tc_grid_y-fwd_radius+j-1,k,n)=dat22(i,j)
                 else
                    fdat_incr(tc_grid_x-fwd_radius+i-1, tc_grid_y-fwd_radius+j-1,k,n)=0.0
                 endif
              enddo; enddo
           enddo; enddo
           deallocate(dat21, dat22)
        else
           !---from Xu Lu: dat42(1:10,:,:,1)=0.; dat42(:,1:10,:,1)=0.; dat42(ix-10:ix,:,:,1)=0.; dat42(:,iy-10:iy,:,1)=0.
           fdat_incr(1:10,:,:,:)=0.; fdat_incr(:,1:10,:,:)=0.; fdat_incr(ix-10:ix,:,:,:)=0.; fdat_incr(:,jx-10:jx,:,:)=0.
        endif   !if ( abs(wave_num) < 99 ) then

        ! 5.5 --- update an file or output to the increment file
        if ( len_trim(out_file) <= 2 ) then
           fdat_an = fdat_bg + fdat_incr
           call update_hafs_restart(trim(an_file), trim(varname), ix, jx, kx, tx, fdat_an)
           deallocate(fdat_bg, fdat_an, fdat_incr)
        else
           !---output to increment file
           if ( trim(varname) == 'DZ' ) then
              call write_nc_real(trim(out_file),'delz_inc',ix,jx,kxo,-1,'nx','ny',trim(kxc),'-', &
                              fdat_incr(1:ix,1:jx,1:kx,1),'-',trim(varname)//'_inc')
           else
              call write_nc_real(trim(out_file),trim(varname)//'_inc',ix,jx,kxo,-1,'nx','ny',trim(kxc),'-', &
                              fdat_incr(1:ix,1:jx,1:kx,1),'-',trim(varname)//'_inc')
           endif
           deallocate(fdat_incr)
        endif   !if ( len_trim(out_file) <= 2 ) then

     endif  !if ( trim(varname) == 'u' .and. len_trim(grid_file) > 2 ) then ! u/v

  enddo do_input_var_loop !do_input_var_loop: do n1=1, nm

  return
  end subroutine hafsfftw_iau
