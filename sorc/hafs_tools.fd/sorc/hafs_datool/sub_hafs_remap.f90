!========================================================================================
  subroutine hafs_remap(src_grid, src_file, dst_grid, dst_file)

!-----------------------------------------------------------------------------
! HAFS DA tool - remap
! authors and history:
!      -- 202102, created by Yonghui Weng
!
! This subroutine drives the interpolation from one-grid to another grid.
!     src_file + dst_file -- > out_file in dst_grid: merge src_file and dst_file to out_file
!     src_file --> out_file: only interpolate src_file to out_fuile
!
! note: -- input files should are on the same grids, which means only read grid info once.
!       -- out_file is just one filename, which only out to one output file
!       --
!-----------------------------------------------------------------------------

  use netcdf
  use module_mpi
  use var_type

  implicit none

  character (len=*), intent(in) :: src_grid, src_file, dst_grid, dst_file

  integer   :: i, j, k, n, i0, i1, j1, k1, n1, nf, nv, nm

  type(grid2d_info)  :: grid_src, grid_dst

  integer :: src_ncid, varid, ndims, nvars, xtype, dimids(5), vdim(5), u_stag, v_stag
  character(len=nf90_max_name) :: varname, dimname
  integer :: ivarid, vartype, nvardims, nvarAtts, dimlen
  integer,dimension(nf90_max_var_dims) :: vdims, vardimids

  real, allocatable, dimension(:,:,:,:) :: fdat_src, fdat_dst, fdat_out
  real, allocatable, dimension(:,:)     :: lat_src, lon_src, lat_dst, lon_dst
  real*8, allocatable, dimension(:,:,:,:) :: ddat_src, ddat_dst

  real, allocatable, dimension(:,:,:,:) :: u_src, v_src, u_dst, v_dst, u_out, v_out, u_tmp, v_tmp
  real, allocatable, dimension(:,:)     :: cangu_src, sangu_src, cangv_src, sangv_src
  real, allocatable, dimension(:,:)     :: cangu_dst, sangu_dst, cangv_dst, sangv_dst
  integer                               :: ix, jx

  integer :: dst_ncid, varid1, varid2, ndims1, nvars1, xtype1, rcode, noutfl, ncount

  !---for mpi
  integer :: total_var_level, ns
  integer, allocatable, dimension(:,:) :: var_level_ord, var_level_ord_tmp   ! (total_var_level,1:6), &
                                                                             ! 1=var-order, 2:5=(ix,jx,kx,tx),
                                                                             ! 6:9=(ixs,jxs,kxs,txs), 10:13=(ixc,jxc,kxc,txc),
                                                                             ! 14=xtype
  character(len=nf90_max_name), dimension(9999) :: var_level_name
  integer :: ixs, jxs, kxs, txs, ixc, jxc, kxc, txc, ixo, jxo, kxo, txo

!------------------------------------------------------------------------------
! 1 --- arg process
!

!------------------------------------------------------------------------------
! 2 --- input grid info
!       read from grid file grid_spec.nc:
  if ( my_proc_id == 0 ) write(*,'(a)')' --- read grid info from '//trim(src_grid)
  call rd_grid_spec_data(trim(src_grid), grid_src)
  ix=grid_src%grid_xt
  jx=grid_src%grid_yt
  allocate( cangu_src(ix,jx+1),sangu_src(ix,jx+1),cangv_src(ix+1,jx),sangv_src(ix+1,jx) )
  call cal_uv_coeff_fv3(ix, jx, grid_src%grid_lat, grid_src%grid_lon, cangu_src, sangu_src, cangv_src, sangv_src)

!------------------------------------------------------------------------------
! 3 --- output grid info
  if ( my_proc_id == 0 ) write(*,'(a)')' --- read grid info from '//trim(dst_grid)
  call rd_grid_spec_data(trim(dst_grid), grid_dst)
  ix=grid_dst%grid_xt
  jx=grid_dst%grid_yt
  allocate( cangu_dst(ix,jx+1),sangu_dst(ix,jx+1),cangv_dst(ix+1,jx),sangv_dst(ix+1,jx) )
  call cal_uv_coeff_fv3(ix, jx, grid_dst%grid_lat, grid_dst%grid_lon, cangu_dst, sangu_dst, cangv_dst, sangv_dst)

!------------------------------------------------------------------------------
! 4 --- calculate output-grid in input-grid's positions (xin, yin), and each grid's weight to dst
  call cal_src_dst_grid_weight(grid_src, grid_dst)

!------------------------------------------------------------------------------
! 5 --- calculate the total number of variables and vertical-levels
     !dimensions of fv3 restart:
     !            grid_spec.nc: grid_xt = 2880, grid_yt = 2400, grid_x = 2881, grid_y = 2401
     !    fv_core.res.tile1.nc: xaxis_1 = 2880, yaxis_2 = 2400, xaxis_2 = 2881, yaxis_1 = 2401, zaxis_1 = 91
     ! fv_srf_wnd.res.tile1.nc: xaxis_1 = 2880, yaxis_1 = 2400
     !  fv_tracer.res.tile1.nc: xaxis_1 = 2880, yaxis_1 = 2400, zaxis_1 = 91
     !             phy_data.nc: xaxis_1 = 2880, yaxis_1 = 2400, zaxis_1 = 91
     !             sfc_data.nc: xaxis_1 = 2880, yaxis_1 = 2400, zaxis_1 = 4
     ! for variables' dimensions:
     !     grid_lon(2881,2401), grid_lat(2881,2401), grid_lont(2880,2400), grid_latt(2880,2400)
     !     T(2880,2400,91,1), u(2880,2401,91,1), v(2881,2400,91,1)

  call nccheck(nf90_open(trim(src_file), nf90_nowrite, src_ncid), 'wrong in open '//trim(src_file), .true.)
  call nccheck(nf90_inquire(src_ncid, ndims, nvars), 'wrong in inquire ncid', .true.)
  allocate(var_level_ord_tmp(9999,14))
  total_var_level=0
  search_var_loop: do nv = 1, nvars
     dimids=-1; vdim=-1
     call nccheck(nf90_inquire_variable(src_ncid,nv,varname,xtype,ndims,dimids),'wrong in inquire_variable '//trim(varname), .true.)
     if ( trim(varname) == 'v' ) cycle search_var_loop
     if ( ndims < 2 .or. ndims > 4 ) cycle search_var_loop
     do i = 1, ndims
        call nccheck(nf90_inquire_dimension(src_ncid,dimids(i), len=vdim(i)), 'wrong in inquire '//trim(varname)//' dim', .true.)
     enddo

     if ( ndims == 4 ) then  ! (x,y,z,t)
        do k = 1, vdim(3)
           total_var_level=total_var_level+1
           var_level_ord_tmp(total_var_level,1)=total_var_level
           var_level_ord_tmp(total_var_level,2:5)=vdim(1:4)   !ix,jx,kx,tx
           var_level_ord_tmp(total_var_level,6:7)=1           !ixs,jxs
           var_level_ord_tmp(total_var_level,8)=k             !kxs
           var_level_ord_tmp(total_var_level,9)=vdim(4)       !txs
           var_level_ord_tmp(total_var_level,10:11)=vdim(1:2) !ixc,jxc
           var_level_ord_tmp(total_var_level,12:13)=1         !kxc,txc
           var_level_ord_tmp(total_var_level,14)=xtype
           var_level_name(total_var_level)=trim(varname)
        enddo
     else if ( ndims == 3) then ! (x,y,t)
        total_var_level=total_var_level+1
        var_level_ord_tmp(total_var_level,1)=total_var_level
        var_level_ord_tmp(total_var_level,2:4)=vdim(1:3)      !ix,jx,kx
        var_level_ord_tmp(total_var_level,5)=-1               !tx
        var_level_ord_tmp(total_var_level,6:7)=1              !ixs,jxs
        var_level_ord_tmp(total_var_level,8)=vdim(3)          !kxs
        var_level_ord_tmp(total_var_level,9)=1                !txs
        var_level_ord_tmp(total_var_level,10:11)=vdim(1:2)    !ixc,jxc
        var_level_ord_tmp(total_var_level,12:13)=1            !kxc,txc
        var_level_ord_tmp(total_var_level,14)=xtype
        var_level_name(total_var_level)=trim(varname)
     else if ( ndims == 2) then ! (x,y) grid_lon, grid_lat, grid_lont, grid_latt, area
        total_var_level=total_var_level+1
        var_level_ord_tmp(total_var_level,1)=total_var_level
        var_level_ord_tmp(total_var_level,2:3)=vdim(1:2)      !ix,jx
        var_level_ord_tmp(total_var_level,4:5)=-1             !kx,tx
        var_level_ord_tmp(total_var_level,6:7)=1              !ixs,jxs
        var_level_ord_tmp(total_var_level,8:9)=1              !kxs,txs
        var_level_ord_tmp(total_var_level,10:11)=vdim(1:2)    !ixc,jxc
        var_level_ord_tmp(total_var_level,12:13)=1            !kxc,txc
        var_level_ord_tmp(total_var_level,14)=xtype
        var_level_name(total_var_level)=trim(varname)
     endif
  enddo search_var_loop
  if ( total_var_level > 1 ) then
     allocate(var_level_ord(total_var_level,14))
     var_level_ord(1:total_var_level,1:14)=var_level_ord_tmp(1:total_var_level,1:14)
     deallocate(var_level_ord_tmp)
  else
     write(*,'(a)')' ===== wrong in getting '//trim(src_file)//' vars in subroutine hafs_remap'
     stop
  endif
  call nccheck(nf90_close(src_ncid), 'wrong in close '//trim(src_file), .true.)

!------------------------------------------------------------------------------
! 6 --- merge loop for variable+level
  call nccheck(nf90_open(trim(src_file), nf90_nowrite, src_ncid), 'wrong in open '//trim(src_file), .true.)
  call nccheck(nf90_open(trim(dst_file), nf90_write, dst_ncid), 'wrong in open '//trim(dst_file), .true.)

  nm=max(1,int((total_var_level+nprocs-1)/nprocs))
  do_input_var_loop: do n1=1, nm
     ns=(n1-1)*nprocs+my_proc_id+1
     if ( ns > total_var_level ) exit do_input_var_loop

     ! 6.1 --- process variable's dimension
     ixs=var_level_ord(ns,6)
     jxs=var_level_ord(ns,7)
     kxs=var_level_ord(ns,8)
     txs=var_level_ord(ns,9)
     ixc=var_level_ord(ns,10)
     jxc=var_level_ord(ns,11)
     kxc=var_level_ord(ns,12)
     txc=var_level_ord(ns,13)
     xtype=var_level_ord(ns,14)
     varname=trim(var_level_name(ns))
     if ( trim(varname) == 'v' ) cycle do_input_var_loop

     ! --- u/v dimensions
     u_stag=0; v_stag=0
     if (var_level_ord(ns,3) == grid_src%grid_y ) u_stag=1
     if (var_level_ord(ns,2) == grid_src%grid_x ) v_stag=1

     ! 6.2 --- get src data
     if ( debug_level > 30) write(*,'(a)')'---get '//trim(varname)//' from '//trim(src_file)
     call nccheck(nf90_inq_varid(src_ncid, trim(varname), varid), 'wrong in inquire '//trim(varname)//' varid', .true.)
     allocate(fdat_src(ixc, jxc, kxc, txc))

     if ( xtype == nf90_float .or. xtype == nf90_real .or. xtype == nf90_real4 ) then
        call nccheck(nf90_get_var(src_ncid, varid, fdat_src, start=(/ixs,jxs,kxs,txs/), count=(/ixc,jxc,kxc,txc/)), &
                     'wrong in get '//trim(varname)//' from '//trim(src_file), .true.)
     else if ( xtype == nf90_double .or. xtype == nf90_real8 ) then
        allocate(ddat_src(ixc, jxc, kxc, txc))
        call nccheck(nf90_get_var(src_ncid, varid, ddat_src, start=(/ixs,jxs,kxs,txs/), count=(/ixc,jxc,kxc,txc/)), &
                     'wrong in get '//trim(varname)//' from '//trim(src_file), .true.)
        fdat_src=real(ddat_src)
        deallocate(ddat_src)
     else
        write(*,*)' !!!! please add ',xtype,' xtype data here '
        stop
     endif
     !--- process u/v
     if ( trim(varname) == 'u' ) then
        !--- get u/v data
        allocate(u_src(ixc, jxc, kxc, txc))
        u_src=fdat_src; deallocate(fdat_src)
        ix=ixc+1; jx=jxc-1
        allocate(v_src(ix, jx, kxc, txc))
        if ( my_proc_id == 0 .and. debug_level > 0) write(*,'(a,8i6)')'--- u_src: ', ixs,jxs,kxs,txs,ixc,jxc,kxc,txc
        if ( my_proc_id == 0 .and. debug_level > 0) write(*,'(a,8i6)')'--- v_src: ', ixs,jxs,kxs,txs,ix,jx,kxc,txc
        call nccheck(nf90_inq_varid(src_ncid, 'v', varid), 'wrong in inquire v varid', .true.)
        if ( xtype == nf90_float .or. xtype == nf90_real .or. xtype == nf90_real4 ) then
           call nccheck(nf90_get_var(src_ncid, varid, v_src, start=(/ixs,jxs,kxs,txs/), count=(/ix,jx,kxc,txc/)), 'wrong in get v from '//trim(src_file), .true.)
        else if ( xtype == nf90_double .or. xtype == nf90_real8 ) then
           allocate(ddat_src(ix, jx, kxc, txc))
           call nccheck(nf90_get_var(src_ncid, varid, ddat_src, start=(/ixs,jxs,kxs,txs/), count=(/ix,jx,kxc,txc/)), 'wrong in get v from '//trim(src_file), .true.)
           v_src=real(ddat_src); deallocate(ddat_src)
        endif

        !--- debug
        if ( debug_level > 100 ) then
           call write_nc_dim('fv_core_uv_src.nc', 'ixc', ixc)
           call write_nc_dim('fv_core_uv_src.nc', 'jxc', jxc)
           call write_nc_dim('fv_core_uv_src.nc', 'kx', var_level_ord(ns,4))
           call write_nc_dim('fv_core_uv_src.nc', 'tx', var_level_ord(ns,5))
           call write_nc_dim('fv_core_uv_src.nc', 'ix', ix)
           call write_nc_dim('fv_core_uv_src.nc', 'jx', jx)
           call write_nc_real_par('fv_core_uv_src.nc', 'u_src', ixc, jxc, var_level_ord(ns,4), var_level_ord(ns,5), 'ixc', 'jxc', 'kx', 'tx', &
                                                       ixs, jxs, kxs, txs, ixc, jxc, kxc, txc, u_src, '-', '-')
           call write_nc_real_par('fv_core_uv_src.nc', 'v_src', ix , jx , var_level_ord(ns,4), var_level_ord(ns,5), 'ix ', 'jx ', 'kx', 'tx', &
                                                       ixs, jxs, kxs, txs, ix , jx , kxc, txc, v_src, '-', '-')
        endif

        !--- convert u/v to ua/va
        allocate(u_out(ixc, jxc, kxc, txc), v_out(ix, jx, kxc, txc))
        do k = 1, kxc; do i = 1, txc
           call fv3uv2earth(ixc, jxc-1, u_src(:,:,k,i), v_src(:,:,k,i), cangu_src, sangu_src, cangv_src, sangv_src, u_out(:,:,k,i), v_out(:,:,k,i))
        enddo; enddo
        u_src=u_out; v_src=v_out; deallocate(u_out, v_out)
        if ( debug_level > 100 ) then
           call write_nc_real_par('fv_core_uv_src.nc', 'ua_src', ixc, jxc, var_level_ord(ns,4), var_level_ord(ns,5), 'ixc', 'jxc', 'kx', 'tx', &
                                                       ixs, jxs, kxs, txs, ixc, jxc, kxc, txc, u_src, '-', '-')
           call write_nc_real_par('fv_core_uv_src.nc', 'va_src', ix , jx , var_level_ord(ns,4), var_level_ord(ns,5), 'ix ', 'jx ', 'kx', 'tx', &
                                                       ixs, jxs, kxs, txs, ix , jx , kxc, txc, v_src, '-', '-')
        endif
     endif

     ! 6.3 --- get dst data
     ixo=grid_dst%grid_xt+v_stag; jxo=grid_dst%grid_yt+u_stag; kxo=kxc; txo=txc  !current no vertical/time interpolation
     allocate(fdat_dst(ixo,jxo,kxo,txo))

     !---inqure the variables from dst_file
     if ( debug_level > 30) write(*,'(a)')'---get '//trim(varname)//' from '//trim(dst_file)
     rcode=nf90_inq_varid(dst_ncid, trim(varname), varid1)
     if ( rcode /= nf90_noerr ) then
        write(*,'(a,2i6)')'---inq '//trim(varname), rcode, nf90_noerr
        stop
     endif
     if ( xtype == nf90_float .or. xtype == nf90_real .or. xtype == nf90_real4 ) then
        call nccheck(nf90_get_var(dst_ncid, varid1, fdat_dst, start=(/ixs,jxs,kxs,txs/), count=(/ixo,jxo,kxo,txo/)), &
                     'wrong in get '//trim(varname)//' from '//trim(dst_file), .true.)
     else if ( xtype == nf90_double .or. xtype == nf90_real8 ) then
        allocate(ddat_dst(ixo,jxo,kxo,txo))
        call nccheck(nf90_get_var(dst_ncid, varid1, ddat_dst, start=(/ixs,jxs,kxs,txs/), count=(/ixo,jxo,kxo,txo/)), &
                     'wrong in get '//trim(varname)//' from '//trim(dst_file), .true.)
        fdat_dst = real(ddat_dst)
        deallocate(ddat_dst)
     endif
     !--- process u/v
     if ( trim(varname) == 'u' ) then
        !--- get u/v data
        allocate(u_dst(ixo,jxo,kxo,txo))
        u_dst=fdat_dst; deallocate(fdat_dst)
        ix=ixo+1; jx=jxo-1
        allocate(v_dst(ix, jx, kxo, txo))
        if ( my_proc_id == 0 .and. debug_level > 0) write(*,'(a,8i6)')'--- u_dst: ', ixs,jxs,kxs,txs,ixo,jxo,kxo,txo
        if ( my_proc_id == 0 .and. debug_level > 0) write(*,'(a,8i6)')'--- v_dst: ', ixs,jxs,kxs,txs,ix,jx,kxo,txo
        rcode=nf90_inq_varid(dst_ncid, 'v', varid2)
        if ( xtype == nf90_float .or. xtype == nf90_real .or. xtype == nf90_real4 ) then
           call nccheck(nf90_get_var(dst_ncid, varid2, v_dst, start=(/ixs,jxs,kxs,txs/), count=(/ix,jx,kxo,txo/)), 'wrong in get v from '//trim(dst_file), .true.)
        else if ( xtype == nf90_double .or. xtype == nf90_real8 ) then
           allocate(ddat_dst(ix, jx, kxo, txo))
           call nccheck(nf90_get_var(dst_ncid, varid2, ddat_dst, start=(/ixs,jxs,kxs,txs/), count=(/ix,jx,kxo,txo/)), 'wrong in get v from '//trim(dst_file), .true.)
           v_dst=real(ddat_dst); deallocate(ddat_dst)
        endif
        if ( debug_level > 100 ) then
           call write_nc_dim('fv_core_uv_dst.nc', 'ixo', ixo)
           call write_nc_dim('fv_core_uv_dst.nc', 'jxo', jxo)
           call write_nc_dim('fv_core_uv_dst.nc', 'kx', var_level_ord(ns,4))
           call write_nc_dim('fv_core_uv_dst.nc', 'tx', var_level_ord(ns,5))
           call write_nc_dim('fv_core_uv_dst.nc', 'ix', ix)
           call write_nc_dim('fv_core_uv_dst.nc', 'jx', jx)
           call write_nc_real_par('fv_core_uv_dst.nc', 'u_dst', ixo, jxo, var_level_ord(ns,4), var_level_ord(ns,5), 'ixo', 'jxo', 'kx', 'tx', &
                                                       ixs, jxs, kxs, txs, ixo, jxo, kxo, txo, u_dst, '-', '-')
           call write_nc_real_par('fv_core_uv_dst.nc', 'v_dst', ix , jx , var_level_ord(ns,4), var_level_ord(ns,5), 'ix ', 'jx ', 'kx', 'tx', &
                                                       ixs, jxs, kxs, txs, ix , jx , kxo, txo, v_dst, '-', '-')
        endif

        !--- convert u/v to ua/va
        allocate(u_out(ixo, jxo, kxo, txo), v_out(ix, jx, kxo, txo))
        do k = 1, kxo; do i = 1, txo
           call fv3uv2earth(ixo, jxo-1, u_dst(:,:,k,i), v_dst(:,:,k,i), cangu_dst, sangu_dst, cangv_dst, sangv_dst, u_out(:,:,k,i), v_out(:,:,k,i))
        enddo; enddo
        u_dst=u_out; v_dst=v_out; deallocate(u_out, v_out)
        if ( debug_level > 100 ) then
           call write_nc_real_par('fv_core_uv_dst.nc', 'ua_dst', ixo, jxo, var_level_ord(ns,4), var_level_ord(ns,5), 'ixo', 'jxo', 'kx', 'tx', &
                                                       ixs, jxs, kxs, txs, ixo, jxo, kxo, txo, u_dst, '-', '-')
           call write_nc_real_par('fv_core_uv_dst.nc', 'va_dst', ix , jx , var_level_ord(ns,4), var_level_ord(ns,5), 'ix ', 'jx ', 'kx', 'tx', &
                                                       ixs, jxs, kxs, txs, ix , jx , kxo, txo, v_dst, '-', '-')
        endif
     endif

     ! 6.4 --- merge dat_src + dat_dst --> data_merge: with distance-weightnening average
     !     --- when output is out of input-grid: xin/yin < 0 or xin/yin > max, fill with output data
     !----allocate gw, gw=gwt%gwt_t, gwt_u, gwt_v
     !  fdat_out = sum(fdat_src(gwt%gwt_t%src_x(:),gwt%gwt_t%src_y(:))*gwt%gwt_t%src_weight)) + &
     !             sum(fdat_dst(gwt%gwt_t%dst_x(:),gwt%gwt_t%dst_y(:))*gwt%gwt_t%dst_weight))
     !write(*,'(a)')'---combine '//trim(varname)
     allocate(fdat_out(ixo,jxo,kxo,txo))
     if ( trim(varname) == 'u' ) then
        allocate(u_tmp(ixo,jxo,kxo,txo), v_tmp(ix,jx,kxo,txo))
        if ( my_proc_id == 0 .and. debug_level > 0) write(*,'(a,4i6,a,4i6)')'--- u_src: ', ixc,jxc,kxc,txc,',  u_dst: ',ixo,jxo,kxo,txo
        if ( my_proc_id == 0 .and. debug_level > 0) write(*,'(a,4i6,a,4i6)')'--- v_src: ', ixc+1,jxc-1,kxc,txc,',  v_dst: ',ix,jx,kxo,txo
        call combine_grids_for_remap(ixc,   jxc,   kxc, txc, u_src, ixo, jxo, kxo, txo, u_dst, gwt%gwt_u, u_tmp)
        call combine_grids_for_remap(ixc+1, jxc-1, kxc, txc, v_src, ix,  jx,  kxo, txo, v_dst, gwt%gwt_v, v_tmp)
        deallocate(u_src,v_src,u_dst,v_dst)
        if ( debug_level > 100 ) then
           call write_nc_real_par('fv_core_uv_dst.nc', 'ua_mrg', ixo, jxo, var_level_ord(ns,4), var_level_ord(ns,5), 'ixo', 'jxo', 'kx', 'tx', &
                                                       ixs, jxs, kxs, txs, ixo, jxo, kxo, txo, u_tmp, '-', '-')
           call write_nc_real_par('fv_core_uv_dst.nc', 'va_mrg', ix , jx , var_level_ord(ns,4), var_level_ord(ns,5), 'ix ', 'jx ', 'kx', 'tx', &
                                                       ixs, jxs, kxs, txs, ix , jx , kxo, txo, v_tmp, '-', '-')
        endif

        !---convert earth wind to fv3grid wind
        allocate(u_out(ixo,jxo,kxo,txo), v_out(ix,jx,kxo,txo))
        do k = 1, kxo; do i = 1, txo
           call earthuv2fv3(ixo, jxo-1, u_tmp(:,:,k,i), v_tmp(:,:,k,i), cangu_dst, sangu_dst, cangv_dst, sangv_dst, u_out(:,:,k,i), v_out(:,:,k,i))
        enddo; enddo
        fdat_out=u_out; deallocate(u_tmp,v_tmp,u_out)
        if ( debug_level > 100 ) then
           call write_nc_real_par('fv_core_uv_dst.nc', 'u_mrg', ixo, jxo, var_level_ord(ns,4), var_level_ord(ns,5), 'ixo', 'jxo', 'kx', 'tx', &
                                                       ixs, jxs, kxs, txs, ixo, jxo, kxo, txo, fdat_out, '-', '-')
           call write_nc_real_par('fv_core_uv_dst.nc', 'v_mrg', ix , jx , var_level_ord(ns,4), var_level_ord(ns,5), 'ix ', 'jx ', 'kx', 'tx', &
                                                       ixs, jxs, kxs, txs, ix , jx , kxo, txo, v_out, '-', '-')
        endif
     else
        call combine_grids_for_remap(ixc, jxc, kxc, txc, fdat_src, ixo, jxo, kxo, txo, fdat_dst, gwt%gwt_t, fdat_out)
     endif

     ! 6.5 --- output nc4
     call nccheck(nf90_inq_varid(dst_ncid, trim(varname), varid1), 'wrong in inq_varid '//trim(varname), .true.)
     if ( xtype == nf90_float .or. xtype == nf90_real .or. xtype == nf90_real4 ) then
        call nccheck(nf90_put_var(dst_ncid, varid1, fdat_out, start=(/ixs,jxs,kxs,txs/), count=(/ixo,jxo,kxo,txo/)), 'wrong in write '//trim(varname), .true.)
     else if ( xtype == nf90_double .or. xtype == nf90_real8 ) then
        call nccheck(nf90_put_var(dst_ncid, varid1, dble(fdat_out), start=(/ixs,jxs,kxs,txs/), count=(/ixo,jxo,kxo,txo/)), 'wrong in write '//trim(varname), .true.)
     endif

     if ( trim(varname) == 'u' ) then
        call nccheck(nf90_inq_varid(dst_ncid, 'v', varid2), 'wrong in inq_varid v', .true.)
        if ( my_proc_id == 0 .and. debug_level > 50) write(*,'(a,8i6)')'--- u: ', ixs,jxs,kxs,txs,ixo,jxo,kxo,txo
        if ( my_proc_id == 0 .and. debug_level > 50) write(*,'(a,8i6)')'--- v: ', ixs,jxs,kxs,txs,ix,jx,kxo,txo
        if ( xtype == nf90_float .or. xtype == nf90_real .or. xtype == nf90_real4 ) then
           call nccheck(nf90_put_var(dst_ncid, varid2, v_out, start=(/ixs,jxs,kxs,txs/), count=(/ix,jx,kxo,txo/)), 'wrong in write v', .true.)
        else if ( xtype == nf90_double .or. xtype == nf90_real8 ) then
           call nccheck(nf90_put_var(dst_ncid, varid2, dble(v_out), start=(/ixs,jxs,kxs,txs/), count=(/ix,jx,kxo,txo/)), 'wrong in write v', .true.)
        endif
        deallocate(v_out)
     endif

     ! 6.6 --- deallocate
     !write(*,*)'---clean up'
     if (allocated(fdat_src)) deallocate(fdat_src)
     if (allocated(ddat_src)) deallocate(ddat_src)
     if (allocated(fdat_dst)) deallocate(fdat_dst)
     if (allocated(ddat_dst)) deallocate(ddat_dst)
     if (allocated(fdat_out)) deallocate(fdat_out)

  enddo do_input_var_loop
  call nccheck(nf90_close(src_ncid), 'wrong in close '//trim(src_file), .true.)
  call nccheck(nf90_close(dst_ncid), 'wrong in close '//trim(dst_file), .true.)

!------------------------------------------------------------------------------

  end subroutine hafs_remap

!========================================================================================
