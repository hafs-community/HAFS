!========================================================================================
  subroutine hafs_remap(src_dir, src_grid, src_file, dst_dir, dst_grid, dst_file, out_file)

!-----------------------------------------------------------------------------
! HAFS DA tool - remap
! Yonghui Weng, 20210201
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

  character (len=*), intent(in) :: src_dir, src_grid, src_file, dst_dir, dst_grid, dst_file, out_file

  integer   :: i, j, k, n, i0, n_srcfl, n_dstfl, i1, j1, k1, n1, nf, nv
  character (len=2500)                :: srcdir, srcgridfl, dstdir, dstgridfl 
  character (len=2500),dimension(50)  :: srcfiles, dstfiles
   
  type(grid2d_info)  :: grid_src, grid_dst

  logical  :: outside, if_fv_core_file
  integer, allocatable, dimension(:,:) :: x_oini, y_oini 
  real    :: dis, dis0, out_ave_dx, out_ave_dy
  integer :: ixi, jxi, kxi, txi, ixo, jxo, kxo, txo
  integer :: ncid, varid, ndims, nvars, xtype, dimids(5), vdim(5), u_stag, v_stag
  character(len=nf90_max_name) :: varname, dimname
  integer :: ivarid, vartype, nvardims, nvarAtts, dimlen
  integer,dimension(nf90_max_var_dims) :: start, count, vdims, vardimids

  real, allocatable, dimension(:,:,:,:) :: fdat_src, fdat_dst, fdat_out
  real, allocatable, dimension(:,:)     :: lat_src, lon_src, lat_dst, lon_dst
  real*8, allocatable, dimension(:,:,:,:) :: ddat_src, ddat_dst, ddat_out
  real, allocatable, dimension(:,:,:,:) :: u_src, v_src, u_dst, v_dst, u_out, v_out

  integer :: ncid1, varid1, ndims1, nvars1, xtype1, rcode, noutfl, ncount

!------------------------------------------------------------------------------
! 1 --- arg process
!
! 1.1 --- source dir
  if (len_trim(src_dir) < 2 .or. trim(src_dir) == 'w' .or. trim(src_dir) == 'null' ) then
     srcdir='.'
  else
     srcdir=trim(src_dir)//'                                                       '
  endif
  write(*,'(a)')' --- source dir: '//trim(srcdir)
 
! 1.2 --- source files
  j=0; n_srcfl=0
  do i = 1, len_trim(src_file)
     if ( src_file(i:i) == ":" .or. i == len_trim(src_file) ) then
        n_srcfl=n_srcfl+1
        i0=1; if ( i == len_trim(src_file) ) i0=0
        if (src_file(j+1:j+1) == '/' .or. src_file(j+1:j+2) == './' ) then 
           srcfiles(n_srcfl) = src_file(j+1:i-i0)
        else
           srcfiles(n_srcfl) = trim(srcdir)//'/'//src_file(j+1:i-i0)
        endif
        j=i
     endif   !if ( src_file(i:i) == ":"
  enddo  !do i = 1, len_trim(src_file)
  write(*,'(a,i,a)')' --- there is', n_srcfl, ' source file(s)' 

! 1.3 --- source grid: one file includes grid-info, could be the same as the source files
  if (len_trim(src_grid) < 2 ) then
     srcgridfl=srcfiles(1)
  else
     if (src_grid(1:1) == '/' .or. src_grid(1:2) == './' ) then
        srcgridfl=trim(src_grid)
     else
        srcgridfl=trim(srcdir)//'/'//trim(src_grid)
     endif
  endif

! 1.4 --- destination dir  : the files used for being-mergered. 
  if (len_trim(dst_dir) < 2 .or. trim(dst_dir) == 'w' .or. trim(dst_dir) == 'null' ) then
     dstdir='.'
  else
     dstdir=trim(dst_dir)
  endif
  write(*,'(a)')' --- destination dir: '//trim(dstdir)

! 1.5 --- will-be-merged data: if have, src_file+dst_file-->out_file
!                              if not,  src_file         -->out_file
!         
  j=0; n_dstfl=0
  do i = 1, len_trim(dst_file)
     if ( dst_file(i:i) == ":" .or. i == len_trim(dst_file) ) then
        n_dstfl=n_dstfl+1
        i0=1; if ( i == len_trim(dst_file) ) i0=0
        if (dst_file(j+1:j+1) == '/' .or. dst_file(j+1:j+2) == './' ) then
           dstfiles(n_dstfl) = dst_file(j+1:i-i0)
        else
           dstfiles(n_dstfl) = trim(dstdir)//'/'//dst_file(j+1:i-i0)
        endif
        j=i
     endif   !if ( dst_file(i:i) == ":"
  enddo  !do i = 1, len_trim(dst_file)
  write(*,'(a,i,a)')' --- there is', n_dstfl, ' file(s) will be merged.'
      
! 1.6 --- destination grid
  if (len_trim(dst_grid) < 2 ) then
     dstgridfl=dstfiles(1)
  else
     if ( dst_grid(1:1) == '/' .or. dst_grid(1:2) == './' ) then
        dstgridfl=dst_grid
     else
        dstgridfl=trim(dst_dir)//'/'//trim(dst_grid)
     endif
  endif
  write(*,'(a)')' --- remap to grid: '//trim(dstgridfl)


! 1.7 --- out_file
  if (len_trim(out_file) > 2) then  !output to one file
     if ( out_file(1:1) == '/' .or. out_file(1:2) == './' ) then
        do j = 1, n_srcfl; dstfiles(j)=trim(out_file); enddo
     else
        do j = 1, n_srcfl; write(*,*)'j=',j
           dstfiles(j)=trim(dstdir)//'/'//trim(out_file); enddo
     endif
  else
     do j = 1, n_srcfl; dstfiles(j)=srcfiles(j)//'_remapped'; enddo
  endif

!------------------------------------------------------------------------------
! 2 --- input grid info
!       read from grid file grid_spec.nc: 
  write(*,'(a)')' --- read grid info from '//trim(srcgridfl)
  call rd_grid_spec_data(trim(srcgridfl), grid_src)

!------------------------------------------------------------------------------
! 3 --- output grid info
  write(*,'(a)')' --- read grid info from '//trim(dstgridfl)
  call rd_grid_spec_data(trim(dstgridfl), grid_dst)

!------------------------------------------------------------------------------
! 4 --- calculate output-grid in input-grid's positions (xin, yin), and each grid's weight to dst
  call cal_src_dst_grid_weight(grid_src, grid_dst)

!------------------------------------------------------------------------------
! 5 --- inputfiles' loop
  do_inputfiles_loop: do nf = 1, n_srcfl   !srcfiles(nf)  
     call nccheck(nf90_open(trim(srcfiles(nf)), nf90_nowrite, ncid), 'wrong in open '//trim(srcfiles(nf)), .false.)
     call nccheck(nf90_inquire(ncid, ndims, nvars), 'wrong in inquire ncid', .true.)
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
     !
     !---check dimname to determine if fv_core.res.tile1.nc
     !k=0; if_fv_core_file = .false.
     !do n = 1, ndims
     !   call nccheck(nf90_inquire_dimension(ncid,n,name=dimname, len=i), 'wrong in inquire_dimension', .true.)
     !   if ( trim(dimname) == 'xaxis_1' .or. trim(dimname) == 'yaxis_2' .or. trim(dimname) == 'xaxis_2' .or. trim(dimname) == 'yaxis_1' ) &
     !      k=k+1
     !enddo
     !if ( k == 4 ) if_fv_core_file = .true. 

! 5.1 --- variables' loop
     do_input_var_loop: do nv=1, nvars
      
        ! 5.1.1 --- get variable's dimension
        dimids=-1; vdim=-1
        call nccheck(nf90_inquire_variable(ncid,nv,varname,xtype,ndims,dimids), &
                     'wrong in inquire_variable '//trim(varname), .false.)
        
        do i = 1, ndims
           call nccheck(nf90_inquire_dimension(ncid,dimids(i), len=vdim(i)), 'wrong in inquire '//trim(varname)//' dim', .false.)
        enddo

        write(*,'(a,9i6)')'nf90_inquire_variable:  '//trim(varname), ndims, vdim(1:ndims), xtype
        !write(*,'(a,2i6)')' NF90_FLOAT, NF90_DOUBLE=', NF90_FLOAT,NF90_DOUBLE

        !---skip xaxis_1(xaxis_1), yaxis_1(yaxis_1), zaxis_1(zaxis_1)
        if ( ndims < 2 ) cycle do_input_var_loop  

        ! 5.1.2 --- determine mass/u/v grids
        u_stag=0; v_stag=0
        if ( ndims == 4 ) then 
           ixi=vdim(1); jxi=vdim(2); kxi=vdim(3); txi=vdim(4)
        else if ( ndims == 3) then
           ixi=vdim(1); jxi=vdim(2); kxi=vdim(3); txi=1    !here kxi is time dimension
        else if ( ndims == 2) then
           ixi=vdim(1); jxi=vdim(2); kxi=1; txi=1   !grid_lon, grid_lat, grid_lont, grid_latt, area
        else if ( ndims == 1) then
           ixi=vdim(1); jxi=1; kxi=1; txi=1   !
        else
           write(*,*)' --- var '//trim(varname)//' could not be processed'
           cycle do_input_var_loop
        endif
        if ( ixi /= grid_src%grid_x .and. ixi /= grid_src%grid_xt .and. ixi /= grid_src%grid_y .and. ixi /= grid_src%grid_yt ) then
           write(*,*)' --- var '//trim(varname)//' dimension does not match the input grids'
           write(*,'(a,2i5)')' --- input grids dimensions: ', grid_src%grid_xt, grid_src%grid_yt
           write(*,'(a,2i5)')' --- '//trim(varname)//' dimension: ', ixi, jxi
           cycle do_input_var_loop
        endif
        if (jxi == grid_src%grid_y ) u_stag=1  
        if (ixi == grid_src%grid_x ) v_stag=1  
         
        ! 5.1.3 --- get src data
        write(*,'(a)')'---get '//trim(varname)//' from '//trim(srcfiles(nf))
        call nccheck(nf90_inq_varid(ncid, trim(varname), varid), 'wrong in inquire '//trim(varname)//' varid', .false.)
        allocate(fdat_src(ixi, jxi, kxi, txi))
        if ( xtype == nf90_float .or. xtype == nf90_real .or. xtype == nf90_real4 ) then
           call nccheck(nf90_get_var(ncid, varid, fdat_src), 'wrong in get '//trim(varname)//' from '//trim(srcfiles(nf)), .true.) 
        else if ( xtype == nf90_double .or. xtype == nf90_real8 ) then
           allocate(ddat_src(ixi, jxi, kxi, txi))
           call nccheck(nf90_get_var(ncid, varid, ddat_src), 'wrong in get '//trim(varname)//' from '//trim(srcfiles(nf)), .true.) 
           fdat_src=real(ddat_src)
           deallocate(ddat_src)
        else
           write(*,*)' !!!! please add ',xtype,' xtype data here '
           stop
        endif
             
        ! 5.1.4 --- get dst data
        ixo=grid_dst%grid_xt+v_stag; jxo=grid_dst%grid_yt+u_stag; kxo=kxi; txo=txi  !current no vertical/time interpolation
        allocate(fdat_dst(ixo,jxo,kxo,txo))
 
        !---inqure the variables from dstfiles(n_dstfl)
        if ( n_dstfl > 0 ) then
           noutfl=-1
           do_search_var_from_dstfiles: do n = 1, n_dstfl 
              call nccheck(nf90_open(trim(dstfiles(n)), nf90_nowrite, ncid1), 'wrong in open '//trim(dstfiles(n)), .false.)
              rcode=nf90_inq_varid(ncid1, trim(varname), varid1)
              write(*,'(a,2i6)')'---inq '//trim(varname), rcode, nf90_noerr
              if ( rcode /= nf90_noerr ) then
                 call nccheck(nf90_close(ncid1), 'wrong in close '//trim(dstfiles(n)), .false.)            
                 cycle do_search_var_from_dstfiles
              else
                 !write(*,'(a,4i5)')'dst grid size: ', ixo,jxo,kxo,txo
                 if ( xtype == nf90_float .or. xtype == nf90_real .or. xtype == nf90_real4 ) then
                    call nccheck(nf90_get_var(ncid1, varid1, fdat_dst), 'wrong in get '//trim(varname)//' from '//trim(dstfiles(n)), .true.)
                 else if ( xtype == nf90_double .or. xtype == nf90_real8 ) then
                    allocate(ddat_dst(ixo,jxo,kxo,txo))
                    call nccheck(nf90_get_var(ncid1, varid1, ddat_dst), 'wrong in get '//trim(varname)//' from '//trim(dstfiles(n)), .true.)
                    fdat_dst = real(ddat_dst)
                    deallocate(ddat_dst)
                 endif
                 call nccheck(nf90_close(ncid1), 'wrong in close '//trim(dstfiles(n)), .false.)
                 noutfl=n
                 write(*,'(a)')'---get '//trim(varname)//' from '//trim(dstfiles(n))
                 exit do_search_var_from_dstfiles
              endif
           enddo do_search_var_from_dstfiles
        endif !if ( n_dstfl > 0 ) then
            
        ! 5.1.5 --- merge dat_src + dat_dst --> data_merge: with distance-weightnening average
        !       --- when output is out of input-grid: xin/yin < 0 or xin/yin > max, fill with output data
        !----allocate gw, gw=gwt%gwt_t, gwt_u, gwt_v
        !  fdat_out = sum(fdat_src(gwt%gwt_t%src_x(:),gwt%gwt_t%src_y(:))*gwt%gwt_t%src_weight)) + &
        !             sum(fdat_dst(gwt%gwt_t%dst_x(:),gwt%gwt_t%dst_y(:))*gwt%gwt_t%dst_weight))
        !write(*,'(a)')'---combine '//trim(varname)
        allocate(fdat_out(ixo,jxo,kxo,txo))
        if ( u_stag == 0 .and. v_stag == 0 ) then
           call combine_grids_for_remap(ixi, jxi, kxi, txi, fdat_src, ixo, jxo, kxo, txo, fdat_dst, &
                                        gwt%gwt_t, fdat_out)
          !i=2; j=2
          !write(*,'(a,30i6)')'    gwt%gwt_t @ ', i, j, gwt%gwt_t(i,j)%src_points, gwt%gwt_t(i,j)%src_x(1:gwt%gwt_t(i,j)%src_points), &
          !       gwt%gwt_t(i,j)%src_y(1:gwt%gwt_t(i,j)%src_points)
          !write(*,'(a,3f10.2)')'src,dst,out =', fdat_src(gwt%gwt_t(i,j)%src_x(1),gwt%gwt_t(i,j)%src_y(1),1,1), &
          !       fdat_dst(i,j,1,1), fdat_out(i,j,1,1)

        else if ( u_stag == 1 .and. v_stag == 0 ) then
           call combine_grids_for_remap(ixi, jxi, kxi, txi, fdat_src, ixo, jxo, kxo, txo, fdat_dst, &
                                        gwt%gwt_u, fdat_out)
        else if ( u_stag == 0 .and. v_stag == 1 ) then
           call combine_grids_for_remap(ixi, jxi, kxi, txi, fdat_src, ixo, jxo, kxo, txo, fdat_dst, &
                                           gwt%gwt_v, fdat_out)
        endif  
        if ( xtype == nf90_double .or. xtype == nf90_real8 ) then
           allocate(ddat_out(ixo,jxo,kxo,txo))
           ddat_out=dble(fdat_out)
        endif

! --- 5.2, 5.3 -->:
! alternative method: call FV3 horiz_interp_new
! calls like: sorc/hafs_forecast.fd/FMS/topography/topography.F90
!     call horiz_interp_new ( Interp, xdat, ydat(js:je+1), blon, blat ) --> horiz_interp_conserve_new_2dx2d
!     call horiz_interp     ( Interp, zdat(:,js:je), zout ) --> horiz_interp_conserve_version2

! 5.4 --- output nc4
        if ( noutfl > 0 ) then
           call nccheck(nf90_open(trim(dstfiles(noutfl)), nf90_write, ncid1), 'wrong in open '//trim(dstfiles(noutfl)), .false.)
           call nccheck(nf90_inq_varid(ncid1, trim(varname), varid1), 'wrong in inq_varid '//trim(varname), .false.)
           if ( xtype == nf90_float .or. xtype == nf90_real .or. xtype == nf90_real4 ) then
              if ( ndims == 4 ) then
                 call nccheck(nf90_put_var(ncid1, varid1, fdat_out), 'wrong in write '//trim(varname), .false.)
              else if ( ndims == 3 ) then
                 call nccheck(nf90_put_var(ncid1, varid1, reshape(fdat_out, (/ixo, jxo, kxo/))), 'wrong in write '//trim(varname), .false.)
              else if ( ndims == 2 ) then
                 call nccheck(nf90_put_var(ncid1, varid1, reshape(fdat_out, (/ixo, jxo/))), 'wrong in write '//trim(varname), .false.)
              endif
           else if ( xtype == nf90_double .or. xtype == nf90_real8 ) then
              if ( ndims == 4 ) then
                 call nccheck(nf90_put_var(ncid1, varid1, ddat_out), 'wrong in write '//trim(varname), .false.)
              else if ( ndims == 3 ) then
                 call nccheck(nf90_put_var(ncid1, varid1, reshape(ddat_out, (/ixo, jxo, kxo/))), 'wrong in write '//trim(varname), .false.)
              else if ( ndims == 2 ) then
                 call nccheck(nf90_put_var(ncid1, varid1, reshape(ddat_out, (/ixo, jxo/))), 'wrong in write '//trim(varname), .false.)
              endif
           endif
           call nccheck(nf90_close(ncid1), 'wrong in close '//trim(dstfiles(noutfl)), .false.)

        endif  !if ( noutfl > 0 ) then

! 5.5 --- deallocate
        !write(*,*)'---clean up'
        if (allocated(fdat_src)) deallocate(fdat_src)
        if (allocated(ddat_src)) deallocate(ddat_src)
        if (allocated(fdat_dst)) deallocate(fdat_dst)
        if (allocated(ddat_dst)) deallocate(ddat_dst)
        if (allocated(fdat_out)) deallocate(fdat_out)
        if (allocated(ddat_out)) deallocate(ddat_out)

     enddo do_input_var_loop

  enddo do_inputfiles_loop
!------------------------------------------------------------------------------

  end subroutine hafs_remap

!========================================================================================
