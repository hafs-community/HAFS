!========================================================================================
  subroutine rd_grid_spec_data(ncfile, grid)

  use netcdf
  use module_mpi
  use var_type
  implicit none

  character(len=*), intent(in)    :: ncfile
  type(grid2d_info) :: grid

  integer :: i, j, n
  integer :: ncid, varid, ndims, nvars, xtype, dimids(5), vdim(5)
  character(len=50) :: varname, dimname

  call nccheck(nf90_open(trim(ncfile), nf90_nowrite, ncid), 'wrong in open: '//trim(ncfile), .true.)
  call nccheck(nf90_inquire(ncid, ndims, nvars), 'wrong in inquire ncid', .true.)
  do n = 1, ndims
     call nccheck(nf90_inquire_dimension(ncid,n,name=dimname, len=i), 'wrong in inquire_dimension', .true.)
     select case (trim(dimname))
            case ('grid_x', 'west_east_stag'); grid%grid_x = i
            case ('grid_y') ; grid%grid_y = i
            case ('time') ;   grid%ntime  = i
            case ('grid_xt') ; grid%grid_xt = i
            case ('grid_yt') ; grid%grid_yt = i
     end select
  enddo

  if (allocated(grid%grid_lon)) deallocate(grid%grid_lon)
  allocate(grid%grid_lon(grid%grid_x,grid%grid_y))
  call nccheck(nf90_inq_varid(ncid, 'grid_lon', varid), 'wrong in nf90_inq_varid grid_lon', .false.)
  call nccheck(nf90_get_var(ncid, varid, grid%grid_lon), 'wrong in get data of grid_lon', .false.)
  where ( grid%grid_lon > 180. ) grid%grid_lon=grid%grid_lon-360.

  if (allocated(grid%grid_lat)) deallocate(grid%grid_lat)
  allocate(grid%grid_lat(grid%grid_x,grid%grid_y))
  call nccheck(nf90_inq_varid(ncid, 'grid_lat', varid), 'wrong in nf90_inq_varid grid_lat', .false.)
  call nccheck(nf90_get_var(ncid, varid, grid%grid_lat), 'wrong in get data of grid_lat', .false.)

  if (allocated(grid%times)) deallocate(grid%times)
  allocate(grid%times(grid%ntime))
  call nccheck(nf90_inq_varid(ncid, 'time', varid), 'wrong in nf90_inq_varid time', .false.)
  call nccheck(nf90_get_var(ncid, varid, grid%times), 'wrong in get data of time', .false.)
  call nccheck(nf90_get_att(ncid, varid, 'units', grid%times_unit), 'wrong in get times_unit', .false.)

  if (allocated(grid%grid_lont)) deallocate(grid%grid_lont)
  allocate(grid%grid_lont(grid%grid_xt,grid%grid_yt))
  call nccheck(nf90_inq_varid(ncid, 'grid_lont', varid), 'wrong in nf90_inq_varid grid_lont', .false.)
  call nccheck(nf90_get_var(ncid, varid, grid%grid_lont), 'wrong in get data of grid_lont', .false.)
  where ( grid%grid_lont > 180. ) grid%grid_lont=grid%grid_lont-360.

  if (allocated(grid%grid_latt)) deallocate(grid%grid_latt)
  allocate(grid%grid_latt(grid%grid_xt,grid%grid_yt))
  call nccheck(nf90_inq_varid(ncid, 'grid_latt', varid), 'wrong in nf90_inq_varid grid_latt', .false.)
  call nccheck(nf90_get_var(ncid, varid, grid%grid_latt), 'wrong in get data of grid_latt', .false.)

  !if (allocated(grid%grid_area)) deallocate(grid%grid_area)
  !allocate(grid%grid_area(grid%grid_xt,grid%grid_yt))
  !call nccheck(nf90_inq_varid(ncid, 'grid_area', varid), 'wrong in nf90_inq_varid grid_area', .false.)
  !call nccheck(nf90_get_var(ncid, varid, grid%grid_area), 'wrong in get data of grid_area', .false.)

  if ( my_proc_id == 0 ) then
     write(*,'(a,i0,a,i0,a,i0,a,i0,a)')'  domain dims:     (1:1) --> (',grid%grid_xt,':1) --> (',&
                                       grid%grid_xt,':',grid%grid_yt,') --> (1:',grid%grid_yt,')'
     write(*,'(a, 2f7.2,a,2f7.2,a,2f7.2,a,2f7.2)')'       T-cell:', grid%grid_lont(1,1),grid%grid_latt(1,1), '--->', &
                                grid%grid_lont(grid%grid_xt,1),grid%grid_latt(grid%grid_xt,1), '--->', &
                             grid%grid_lont(grid%grid_xt,grid%grid_yt),grid%grid_latt(grid%grid_xt,grid%grid_yt), '--->', &
                                grid%grid_lont(1,grid%grid_yt),grid%grid_latt(1,grid%grid_yt)
     write(*,'(a,f7.2,a,f7.2,a,f7.2,a,f7.2)')'   grid_lont :',minval(grid%grid_lont),':',maxval(grid%grid_lont),&
                                             ' grid_latt :',minval(grid%grid_latt),':',maxval(grid%grid_latt)
     write(*,'(a,i0,a,i0,a,i0,a,i0,a)')'  domain dims:     (1:1) --> (',grid%grid_x ,':1) --> (',&
                                       grid%grid_x ,':',grid%grid_y ,') --> (1:',grid%grid_y ,')'
     write(*,'(a, 2f7.2,a,2f7.2,a,2f7.2,a,2f7.2)')'             :', grid%grid_lon (1,1),grid%grid_lat (1,1), '--->', &
                                grid%grid_lon (grid%grid_x ,1),grid%grid_lat (grid%grid_x ,1), '--->', &
                             grid%grid_lon (grid%grid_x ,grid%grid_y ),grid%grid_lat (grid%grid_x ,grid%grid_y ), '--->', &
                                grid%grid_lon (1,grid%grid_y ),grid%grid_lat (1,grid%grid_y )
     write(*,'(a,f7.2,a,f7.2,a,f7.2,a,f7.2)')'   grid_lon  :',minval(grid%grid_lon ),':',maxval(grid%grid_lon ),&
                                             ' grid_lat  :',minval(grid%grid_lat ),':',maxval(grid%grid_lat)
  endif

  call nccheck(nf90_close(ncid), 'wrong in close '//trim(ncfile), .true.)

  return
  end subroutine rd_grid_spec_data

!========================================================================================
  subroutine get_var_data (ncfile, var, ix, jx, kx, tx, data)

  use netcdf
  implicit none
  character(len=*), intent(in)    :: ncfile
  character(len=*), intent(in)    :: var
  integer, intent(in)             :: ix, jx, kx, tx
  real, dimension(ix, jx, kx, tx) :: data

  integer                         :: ncid, varid, dimid,xtype
  real*8, allocatable, dimension(:,:,:,:)  :: ddata
  integer, allocatable, dimension(:,:,:,:) :: idata

  write(*,'(a,4i5)')'---getting '//trim(var)//' :', ix, jx, kx, tx
  call nccheck(nf90_open(trim(ncfile), nf90_nowrite, ncid), 'wrong in open '//trim(ncfile), .false.)
  call nccheck(nf90_inq_varid(ncid, trim(var), varid), 'wrong in nf90_inq_varid '//trim(var), .false.)
  call nccheck(nf90_inquire_variable(ncid, varid, xtype=xtype), 'wrong in nf90_inquire_variable'//trim(var), .false.)

  if ( xtype == nf90_float .or. xtype == nf90_real .or. xtype == nf90_real4 ) then
     call nccheck(nf90_get_var(ncid, varid, data), 'wrong in get data of '//trim(var), .false.)
  else if ( xtype == nf90_double .or. xtype == nf90_real8 ) then
     allocate(ddata(ix, jx, kx, tx))
     call nccheck(nf90_get_var(ncid, varid, ddata), 'wrong in get data of '//trim(var), .false.)
     data=real(ddata)
     deallocate(ddata)
  else if ( xtype == nf90_int ) then
     allocate(idata(ix, jx, kx, tx))
     call nccheck(nf90_get_var(ncid, varid, idata), 'wrong in get data of '//trim(var), .false.)
     data=real(idata)
     deallocate(idata)
  else
     !---NF90_BYTE, NF90_CHAR, NF90_SHORT
     write(*,*)' !!!! please add ',xtype,' xtype data here '
     stop
  endif
  call nccheck(nf90_close(ncid), 'wrong in close '//trim(ncfile), .false.)

  return
  end subroutine get_var_data

!========================================================================================
  subroutine get_var_data_par (ncfile, var, ix, jx, kx, tx, data, ixs, jxs, kxs, txs)

  use netcdf
  use module_mpi

  implicit none
  character(len=*), intent(in)    :: ncfile
  character(len=*), intent(in)    :: var
  integer, intent(in)             :: ix, jx, kx, tx, ixs, jxs, kxs, txs
  real, dimension(ix, jx, kx, tx) :: data

  integer                         :: ncid, varid, dimid,xtype
  real*8, allocatable, dimension(:,:,:,:)  :: ddata
  integer, allocatable, dimension(:,:,:,:) :: idata

  write(*,'(a,3(i0,1x),i0,a,3(i0,1x),i0,a)')'---getting '//trim(var)//' : (', ixs, jxs, kxs, txs, ') --> (', ix, jx, kx, tx,')'
  !call nccheck(nf90_open(trim(ncfile), nf90_nowrite, ncid), &
  call nccheck(nf90_open(trim(ncfile), nf90_nowrite, ncid), 'wrong in open '//trim(ncfile), .false.)
  call nccheck(nf90_inq_varid(ncid, trim(var), varid), 'wrong in nf90_inq_varid '//trim(var), .false.)
  !call nccheck(nf90_var_par_access(ncid, varid, nf90_collective), 'wrong in  nf90_var_par_access '//trim(var), .false.)
  call nccheck(nf90_inquire_variable(ncid, varid, xtype=xtype), 'wrong in nf90_inquire_variable'//trim(var), .false.)
  !call nccheck(nf90_var_par_access(ncid, varid, nf90_collective), 'wrong in nf90_var_par_access '//trim(var), .false.)

  if ( xtype == nf90_float .or. xtype == nf90_real .or. xtype == nf90_real4 ) then
     call nccheck(nf90_get_var(ncid, varid, data, start = (/ixs, jxs, kxs, txs/), count = (/ix, jx, kx, tx/)), 'wrong in get data of '//trim(var), .false.)
  else if ( xtype == nf90_double .or. xtype == nf90_real8 ) then
     allocate(ddata(ix, jx, kx, tx))
     call nccheck(nf90_get_var(ncid, varid, ddata, start = (/ixs, jxs, kxs, txs/), count = (/ix, jx, kx, tx/)), 'wrong in get data of '//trim(var), .false.)
     data=real(ddata)
     deallocate(ddata)
  else if ( xtype == nf90_int ) then
     allocate(idata(ix, jx, kx, tx))
     call nccheck(nf90_get_var(ncid, varid, idata, start = (/ixs, jxs, kxs, txs/), count = (/ix, jx, kx, tx/)), 'wrong in get data of '//trim(var), .false.)
     data=real(idata)
     deallocate(idata)
  else
     !---NF90_BYTE, NF90_CHAR, NF90_SHORT
     write(*,*)' !!!! please add ',xtype,' xtype data here '
     stop
  endif
  call nccheck(nf90_close(ncid), 'wrong in close '//trim(ncfile), .false.)

  return
  end subroutine get_var_data_par
!========================================================================================
  subroutine get_character_var(ncid, varname, dim_len, dimsize, values)

  use netcdf
  implicit none

  integer, intent(in) :: ncid, dim_len, dimsize
  character (len = *), intent(in) :: varname
  character (len = 1), dimension(dim_len, dimsize), intent(out) :: values
  character (len = nf90_max_name) :: vname
  integer :: ivarid, vartype, nvardims, nvarAtts, dimlen
  integer,dimension(nf90_max_var_dims) :: start, count, vdims, vardimids
  character(len=nf90_max_name)  :: dimname
  integer :: lenstr, j

  call nccheck( nf90_inq_varid(ncid, varname, ivarid), 'wrong in inq_varid '//trim(varname), .false.)
  call nccheck( nf90_inquire_variable(ncid, ivarid, &
                                    name = vname, &
                                    xtype = vartype, &
                                    ndims = nvardims, &
                                    dimids = vardimids, &
                                    natts = nvarAtts), 'wrong in inquire_variable '//trim(varname), .false.)

  lenstr = 1

  do j = 1, nvardims
    call nccheck(nf90_inquire_dimension(ncid, vardimids(j), name = dimname, len = dimlen), 'wrong in inquire_dimension '//trim(varname), .false.)
    lenstr = lenstr * dimlen
    start(j) = 1
    count(j) = dimlen
  enddo

  call nccheck( nf90_get_var(ncid, ivarid, values, start = start, count = count), 'wrong in get '//trim(varname), .false. )
  !write(*, fmt = '(a, " filled")') varname

  end subroutine get_character_var

!========================================================================================
  subroutine get_var_dim(ncfile, var, ndims, dims)

  use netcdf
  implicit none
  character(len=*), intent(in)    :: ncfile
  character(len=*), intent(in)    :: var
  integer, intent(out)            :: ndims
  integer, dimension(nf90_max_var_dims), intent(out) :: dims

  integer                               :: ncid, varid, i
  integer, dimension(nf90_max_var_dims) :: dimids

  call nccheck(nf90_open(trim(ncfile), nf90_nowrite, ncid), 'wrong in open '//trim(ncfile), .false.)
  call nccheck(nf90_inq_varid(ncid, trim(var), varid), 'wrong in nf90_inq_varid '//trim(var), .false.)
  call nccheck(nf90_inquire_variable(ncid, varid, ndims=ndims, dimids=dimids), 'wrong in inquire_variable '//trim(var), .false.)
  dims=-999
  do i = 1, ndims
     call nccheck(nf90_inquire_dimension(ncid,dimids(i), len=dims(i)), 'wrong in inquire '//trim(var)//' dim', .false.)
  enddo
  call nccheck(nf90_close(ncid), 'wrong in close '//trim(ncfile), .false.)

  return
  end subroutine get_var_dim

!========================================================================================
  subroutine update_hafs_restart(ncfile, varname, ix, jx, kx, tx, dat4)

  use netcdf
  implicit none
  character(len=*), intent(in)    :: ncfile
  character(len=*), intent(in)    :: varname
  integer, intent ( in)           :: ix, jx, kx, tx  ! -1=no-this-dim
  real, dimension(abs(ix), abs(jx), abs(kx), abs(tx)), intent(in) :: dat4

  integer :: ncid, varid, ndims, xtype, rcode

  call nccheck(nf90_open(trim(ncfile), nf90_write, ncid), 'wrong in open '//trim(ncfile), .true.)
  !---check variable's type: nf90_real, nf90_double
  call nccheck(nf90_inq_varid(ncid, trim(varname), varid), 'wrong in inq_varid '//trim(varname), .true.)
  call nccheck(nf90_inquire_variable(ncid, varid, xtype=xtype, ndims=ndims), 'wrong in inquire '//trim(varname)//' xtype', .false.)
  if ( xtype == nf90_float .or. xtype == nf90_real .or. xtype == nf90_real4 ) then
     call nccheck(nf90_put_var(ncid, varid, dat4), 'wrong in write '//trim(varname), .false.)
  else if ( xtype == nf90_double .or. xtype == nf90_real8 ) then
     call nccheck(nf90_put_var(ncid, varid, dble(dat4)), 'wrong in write '//trim(varname), .false.)
  endif
  call nccheck(nf90_close(ncid), 'wrong in close '//trim(ncfile), .true.)

  return
  end subroutine update_hafs_restart
!========================================================================================
  subroutine update_hafs_restart_par(ncfile, varname, ix, jx, kx, tx, dat4, ixs, jxs, kxs, txs)

  use netcdf
  implicit none
  character(len=*), intent(in)    :: ncfile
  character(len=*), intent(in)    :: varname
  integer, intent ( in)           :: ix, jx, kx, tx, ixs, jxs, kxs, txs  ! -1=no-this-dim
  real, dimension(abs(ix), abs(jx), abs(kx), abs(tx)), intent(in) :: dat4

  integer :: ncid, varid, ndims, xtype, rcode

  call nccheck(nf90_open(trim(ncfile), nf90_write, ncid), 'wrong in open '//trim(ncfile), .true.)
  !---check variable's type: nf90_real, nf90_double
  call nccheck(nf90_inq_varid(ncid, trim(varname), varid), 'wrong in inq_varid '//trim(varname), .true.)
  call nccheck(nf90_inquire_variable(ncid, varid, xtype=xtype, ndims=ndims), 'wrong in inquire '//trim(varname)//' xtype', .false.)
  if ( xtype == nf90_float .or. xtype == nf90_real .or. xtype == nf90_real4 ) then
     call nccheck(nf90_put_var(ncid, varid, dat4, start = (/ixs, jxs, kxs, txs/), count = (/ix, jx, kx, tx/)), 'wrong in write '//trim(varname), .false.)
  else if ( xtype == nf90_double .or. xtype == nf90_real8 ) then
     call nccheck(nf90_put_var(ncid, varid, dble(dat4), start = (/ixs, jxs, kxs, txs/), count = (/ix, jx, kx, tx/)), 'wrong in write '//trim(varname), .false.)
  endif
  call nccheck(nf90_close(ncid), 'wrong in close '//trim(ncfile), .true.)

  return
  end subroutine update_hafs_restart_par

!========================================================================================
  subroutine write_nc_dim(ncfile, dimname, nx)

  use netcdf
  use module_mpi

  implicit none
  character(len=*), intent(in)    :: ncfile
  character(len=*), intent(in)    :: dimname
  integer, intent ( in)           :: nx

  logical                         :: file_exists
  integer                         :: ncid, nxid, rcode

  !----1.0 open or creat file
  inquire(file=trim(ncfile), exist=file_exists)
  if ( file_exists ) then
     call nccheck(nf90_open(trim(ncfile), nf90_write, ncid), 'wrong in open '//trim(ncfile), .true.)
  else
     call nccheck(nf90_create(trim(ncfile), nf90_hdf5, ncid), 'wrong in creat '//trim(ncfile), .true.)
  endif

  !----2.0 define dimension
  rcode=nf90_inq_dimid(ncid, trim(dimname), nxid)
  if ( rcode /= nf90_noerr ) then   !need to create the dimension
     call nccheck(nf90_def_dim(ncid, trim(dimname), nx, nxid), 'wrong in def '//trim(dimname), .true.)
  endif

  call nccheck(nf90_close(ncid), 'wrong in close '//trim(ncfile), .true.)

  return
  end subroutine write_nc_dim

!========================================================================================
  subroutine write_nc_real0d(ncfile, varname, data, units, long_name)

  use netcdf
  use module_mpi

  implicit none
  character(len=*), intent(in)    :: ncfile
  character(len=*), intent(in)    :: varname
  character(len=*), intent(in)    :: units
  character(len=*), intent(in)    :: long_name
  real                            :: data

  logical                         :: file_exists
  integer                         :: ncid, varid, rcode

  !----1.0 open or creat file
  inquire(file=trim(ncfile), exist=file_exists)
  if ( file_exists ) then
     call nccheck(nf90_open(trim(ncfile), nf90_write, ncid), 'wrong in open '//trim(ncfile), .true.)
  else
     call nccheck(nf90_create(trim(ncfile), nf90_hdf5, ncid), 'wrong in creat '//trim(ncfile), .true.)
  endif

  !----2.0 define variables
  rcode=nf90_inq_varid(ncid, varname, varid)
  if ( rcode /= nf90_noerr ) then  ! need to create the var
     call nccheck(nf90_def_var(ncid, varname, nf90_real, varid), 'wrong in def '//trim(varname), .true.)
     !call nccheck(nf90_var_par_access(ncid, varid, nf90_collective), 'wrong in  nf90_var_par_access '//trim(varname), .false.)
     if ( len_trim(units) > 0 ) call nccheck(nf90_put_att(ncid, varid, "units", units), 'wrong in put units', .false.)
     if ( len_trim(long_name) > 0 ) call nccheck(nf90_put_att(ncid, varid, "long_name",long_name), 'wrong in put long_name', .false.)
  endif

  !----3.0
  call nccheck(nf90_put_var(ncid, varid, data), 'wrong in write '//trim(varname), .true.)

  call nccheck(nf90_close(ncid), 'wrong in close '//trim(ncfile), .true.)

  return
  end subroutine write_nc_real0d

!========================================================================================
  subroutine write_nc_real(ncfile, varname, ix, jx, kx, tx, cx, cy, ck, ct, data, units, long_name)

  use netcdf
  use module_mpi

  implicit none
  character(len=*), intent(in)    :: ncfile
  character(len=*), intent(in)    :: varname
  integer, intent(in)             :: ix, jx, kx, tx   !dimensions for this varname
  character(len=*), intent(in)    :: cx, cy, ck, ct   !dimension name
  real, dimension(abs(ix), abs(jx), abs(kx), abs(tx)), intent(inout)  :: data
  character(len=*), intent(in)    :: units
  character(len=*), intent(in)    :: long_name

  logical                         :: file_exists
  integer                         :: ncid, varid, rcode, ixid, jxid, kxid, txid
  integer                         :: date_time(8)
  character*10                    :: date(3)

  real                            :: FillValue

  FillValue=9.999e+20
  !----1.0 process data _FillValue
  where( data > 9.0e+8 .or. data < -300000. ) data=FillValue

  !----2.0 check file
  inquire(file=trim(ncfile), exist=file_exists)
  if ( file_exists ) then
     call nccheck(nf90_open(trim(ncfile), nf90_write, ncid), 'wrong in open '//trim(ncfile)//' for '//trim(varname), .true.)
  else
     write(*,*)' !!!! please call write_nc_dim to generate the nc file of '//trim(ncfile)
     stop
  endif

  !----3.0 check dimensions
  if ( len_trim(cx) > 0 .and. cx(1:1) /= '-' .and. cx(1:1) /= '=' .and. ix > 0 ) then  !ix<0 will not generate this dimension
     rcode=nf90_inq_dimid(ncid, trim(cx), ixid)
     if ( rcode /= nf90_noerr ) call nccheck(nf90_def_dim(ncid, cx, ix, ixid), 'wrong in def_dim '//trim(cx), .true.)
  endif
  if ( len_trim(cy) > 0 .and. cy(1:1) /= '-' .and. cy(1:1) /= '=' .and. jx > 0 ) then
     rcode=nf90_inq_dimid(ncid, trim(cy), jxid)
     if ( rcode /= nf90_noerr ) call nccheck(nf90_def_dim(ncid, cy, jx, jxid), 'wrong in def_dim '//trim(cy), .true.)
  endif
  if ( len_trim(ck) > 0 .and. ck(1:1) /= '-' .and. ck(1:1) /= '=' .and. kx > 0 ) then
     rcode=nf90_inq_dimid(ncid, trim(ck), kxid)
     if ( rcode /= nf90_noerr ) call nccheck(nf90_def_dim(ncid, ck, kx, kxid), 'wrong in def_dim '//trim(ck), .true.)
  endif
  if ( len_trim(ct) > 0 .and. ct(1:1) /= '-' .and. ct(1:1) /= '=' .and. tx > 0 ) then
     rcode=nf90_inq_dimid(ncid, trim(ct), txid)
     if ( rcode /= nf90_noerr ) call nccheck(nf90_def_dim(ncid, ct, tx, txid), 'wrong in def_dim '//trim(ct), .true.)
  endif

  !----4.0 check var
  rcode=nf90_inq_varid(ncid, varname, varid)
  if ( rcode /= nf90_noerr ) then  ! need to define the var
     if ( tx>0 ) then
        if ( kx >0 ) then
           if ( ix>0 .and. jx>0  ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid,jxid,kxid,txid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix>0 .and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid,kxid,txid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0 .and. jx>0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/jxid,kxid,txid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0.and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/kxid,txid/), varid), 'wrong in def_var '//trim(varname), .true.)
        else if ( kx <= 0 ) then
           if ( ix>0 .and. jx>0  ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid,jxid,txid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix>0 .and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid,txid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0 .and. jx>0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/jxid,txid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0.and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/txid/), varid), 'wrong in def_var '//trim(varname), .true.)
        endif
     else if ( tx<=0 ) then
        if ( kx >0 ) then
           if ( ix>0 .and. jx>0  ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid,jxid,kxid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix>0 .and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid,kxid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0 .and. jx>0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/jxid,kxid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0.and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/kxid/), varid), 'wrong in def_var '//trim(varname), .true.)
        else if ( kx <= 0 ) then
           if ( ix>0 .and. jx>0  ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid,jxid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix>0 .and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0 .and. jx>0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/jxid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0.and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,varid), 'wrong in def_var '//trim(varname), .true.)
        endif
     endif
     !---
     !if ( ix>0 .and. jx>0 ) then
     !   call nccheck(nf90_def_var_chunking(ncid, varid, 1, [jx, ix]), 'wrong in nf90_def_var_chunking', .false.)
     !   call nccheck(nf90_def_var_deflate(ncid, varid, 1, 1, 4), 'wrong in nf90_def_var_deflate 4', .false.)
     !endif
     call nccheck(nf90_enddef(ncid), 'wrong in nf90_enddef', .false.)
  endif

  !----5.0 write data
  !call nccheck(nf90_var_par_access(ncid, varid, nf90_collective), 'wrong in  nf90_var_par_access '//trim(varname), .false.)
  if ( tx>0 ) then
     if ( kx >0 ) then
        if ( ix>0 .and. jx>0  ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ix,jx,kx,tx/))), 'wrong in write '//trim(varname), .true.)
        if ( ix>0 .and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ix,   kx,tx/))), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0 .and. jx>0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/   jx,kx,tx/))), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0.and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/      kx,tx/))), 'wrong in write '//trim(varname), .true.)
     else if ( kx <= 0 ) then
        if ( ix>0 .and. jx>0  ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ix,jx,   tx/))), 'wrong in write '//trim(varname), .true.)
        if ( ix>0 .and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ix,      tx/))), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0 .and. jx>0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/   jx,   tx/))), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0.and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/         tx/))), 'wrong in write '//trim(varname), .true.)
     endif
  else if ( tx<=0 ) then
     if ( kx >0 ) then
        if ( ix>0 .and. jx>0  ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ix,jx,kx   /))), 'wrong in write '//trim(varname), .true.)
        if ( ix>0 .and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ix,   kx   /))), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0 .and. jx>0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/   jx,kx   /))), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0.and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/      kx   /))), 'wrong in write '//trim(varname), .true.)
     else if ( kx <= 0 ) then
        if ( ix>0 .and. jx>0  ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ix,jx      /))), 'wrong in write '//trim(varname), .true.)
        if ( ix>0 .and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ix         /))), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0 .and. jx>0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/   jx      /))), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0.and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, data), 'wrong in write '//trim(varname), .true.)
     endif
  endif

  !----6.0 put att
  if ( len_trim(units) > 0 .and. units(1:1) /= '=') call nccheck(nf90_put_att(ncid, varid, "units", units), 'wrong in put units', .false.)
  if ( len_trim(long_name) > 0 .and. long_name(1:1) /= '=') call nccheck(nf90_put_att(ncid, varid, "long_name",long_name), 'wrong in put long_name', .false.)
  call nccheck(nf90_put_att(ncid, nf90_global, "File-type", "HAFS VI pre-file on rot-ll grids derived from HAFS restart files"), 'wrong in put_att', .false.)
  call date_and_time(date(1), date(2), date(3), date_time)
  call nccheck(nf90_put_att(ncid, nf90_global, "Created_date",date(1)), 'wrong input Created_date', .false.)

  call nccheck(nf90_close(ncid), 'wrong in close '//trim(ncfile), .true.)

  return
  end subroutine write_nc_real

!========================================================================================
  subroutine write_nc_real_par(ncfile, varname, ix, jx, kx, tx, cx, cy, ck, ct,      &
                               is, js, ks, ts, ni, nj, nk, nt, data, units, long_name)

  use netcdf
  use module_mpi

  implicit none
  character(len=*), intent(in)    :: ncfile
  character(len=*), intent(in)    :: varname
  integer, intent(in)             :: ix, jx, kx, tx   !dimensions for this varname
  character(len=*), intent(in)    :: cx, cy, ck, ct   !dimension name
  integer, intent(in)             :: is, js, ks, ts   !data block starts
  integer, intent(in)             :: ni, nj, nk, nt   !data size
  real, dimension(ni, nj, nk, nt), intent(inout)  :: data
  character(len=*), intent(in)    :: units
  character(len=*), intent(in)    :: long_name

  logical                         :: file_exists
  integer                         :: ncid, varid, rcode, ixid, jxid, kxid, txid
  integer                         :: date_time(8)
  character*10                    :: date(3)

  real                            :: FillValue

  FillValue=9.999e+20
  !----1.0 process data _FillValue
  where( data > 9.0e+8 .or. data < -300000. ) data=FillValue

  !----2.0 check file
  inquire(file=trim(ncfile), exist=file_exists)
  if ( file_exists ) then
     call nccheck(nf90_open(trim(ncfile), nf90_write, ncid), 'wrong in open '//trim(ncfile)//' for '//trim(varname), .true.)
  else
     write(*,*)' !!!! please call write_nc_dim to generate the nc file of '//trim(ncfile)
     stop
  endif

  !----3.0 check dimensions
  if ( len_trim(cx) > 0 .and. cx(1:1) /= '-' .and. cx(1:1) /= '=' .and. ix > 0 ) then  !ix<0 will not generate this dimension
     rcode=nf90_inq_dimid(ncid, trim(cx), ixid)
     if ( rcode /= nf90_noerr ) call nccheck(nf90_def_dim(ncid, cx, ix, ixid), 'wrong in def_dim '//trim(cx), .true.)
  endif
  if ( len_trim(cy) > 0 .and. cy(1:1) /= '-' .and. cy(1:1) /= '=' .and. jx > 0 ) then
     rcode=nf90_inq_dimid(ncid, trim(cy), jxid)
     if ( rcode /= nf90_noerr ) call nccheck(nf90_def_dim(ncid, cy, jx, jxid), 'wrong in def_dim '//trim(cy), .true.)
  endif
  if ( len_trim(ck) > 0 .and. ck(1:1) /= '-' .and. ck(1:1) /= '=' .and. kx > 0 ) then
     rcode=nf90_inq_dimid(ncid, trim(ck), kxid)
     if ( rcode /= nf90_noerr ) call nccheck(nf90_def_dim(ncid, ck, kx, kxid), 'wrong in def_dim '//trim(ck), .true.)
  endif
  if ( len_trim(ct) > 0 .and. ct(1:1) /= '-' .and. ct(1:1) /= '=' .and. tx > 0 ) then
     rcode=nf90_inq_dimid(ncid, trim(ct), txid)
     if ( rcode /= nf90_noerr ) call nccheck(nf90_def_dim(ncid, ct, tx, txid), 'wrong in def_dim '//trim(ct), .true.)
  endif

  !----4.0 check var
  rcode=nf90_inq_varid(ncid, varname, varid)
  if ( rcode /= nf90_noerr ) then  ! need to define the var
     if ( tx>0 ) then
        if ( kx >0 ) then
           if ( ix>0 .and. jx>0  ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid,jxid,kxid,txid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix>0 .and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid,kxid,txid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0 .and. jx>0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/jxid,kxid,txid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0.and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/kxid,txid/), varid), 'wrong in def_var '//trim(varname), .true.)
        else if ( kx <= 0 ) then
           if ( ix>0 .and. jx>0  ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid,jxid,txid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix>0 .and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid,txid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0 .and. jx>0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/jxid,txid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0.and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/txid/), varid), 'wrong in def_var '//trim(varname), .true.)
        endif
     else if ( tx<=0 ) then
        if ( kx >0 ) then
           if ( ix>0 .and. jx>0  ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid,jxid,kxid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix>0 .and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid,kxid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0 .and. jx>0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/jxid,kxid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0.and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/kxid/), varid), 'wrong in def_var '//trim(varname), .true.)
        else if ( kx <= 0 ) then
           if ( ix>0 .and. jx>0  ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid,jxid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix>0 .and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/ixid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0 .and. jx>0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,(/jxid/), varid), 'wrong in def_var '//trim(varname), .true.)
           if ( ix<=0.and. jx<=0 ) call nccheck(nf90_def_var(ncid, trim(varname), nf90_real,varid), 'wrong in def_var '//trim(varname), .true.)
        endif
     endif
     !call nccheck(nf90_def_var_deflate(ncid, varid, 1, 1, 4), 'wrong in nf90_def_var_deflate 4', .false.)
     call nccheck(nf90_enddef(ncid), 'wrong in nf90_enddef', .false.)
  endif

  !----5.0 write data
  !call nccheck(nf90_var_par_access(ncid, varid, nf90_collective), 'wrong in  nf90_var_par_access '//trim(varname), .false.)
  if ( tx>0 ) then
     if ( kx >0 ) then
        if ( ix>0 .and. jx>0  ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ni,nj,nk,nt/)), start=(/is,js,ks,ts/), count=(/ni,nj,nk,nt/)), 'wrong in write '//trim(varname), .true.)
        if ( ix>0 .and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ni,   nk,nt/)), start=(/is,   ks,ts/), count=(/ni,   nk,nt/)), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0 .and. jx>0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/   nj,nk,nt/)), start=(/   js,ks,ts/), count=(/   nj,nk,nt/)), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0.and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/      nk,nt/)), start=(/      ks,ts/), count=(/      nk,nt/)), 'wrong in write '//trim(varname), .true.)
     else if ( kx <= 0 ) then
        if ( ix>0 .and. jx>0  ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ni,nj,   nt/)), start=(/is,js,   ts/), count=(/ni,nj,   nt/)), 'wrong in write '//trim(varname), .true.)
        if ( ix>0 .and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ni,      nt/)), start=(/is,      ts/), count=(/ni,      nt/)), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0 .and. jx>0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/   nj,   nt/)), start=(/   js,   ts/), count=(/   nj,   nt/)), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0.and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/         nt/)), start=(/         ts/), count=(/         nt/)), 'wrong in write '//trim(varname), .true.)
     endif
  else if ( tx<=0 ) then
     if ( kx >0 ) then
        if ( ix>0 .and. jx>0  ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ni,nj,nk   /)), start=(/is,js,ks   /), count=(/ni,nj,nk   /)), 'wrong in write '//trim(varname), .true.)
        if ( ix>0 .and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ni,   nk   /)), start=(/is,   ks   /), count=(/ni,   nk   /)), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0 .and. jx>0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/   nj,nk   /)), start=(/   js,ks   /), count=(/   nj,nk   /)), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0.and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/      nk   /)), start=(/      ks   /), count=(/      nk   /)), 'wrong in write '//trim(varname), .true.)
     else if ( kx <= 0 ) then
        if ( ix>0 .and. jx>0  ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ni,nj      /)), start=(/is,js      /), count=(/ni,nj      /)), 'wrong in write '//trim(varname), .true.)
        if ( ix>0 .and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/ni         /)), start=(/is         /), count=(/ni         /)), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0 .and. jx>0 ) call nccheck(nf90_put_var(ncid, varid, reshape(data, (/   nj      /)), start=(/   js      /), count=(/   nj      /)), 'wrong in write '//trim(varname), .true.)
        if ( ix<=0.and. jx<=0 ) call nccheck(nf90_put_var(ncid, varid, data), 'wrong in write '//trim(varname), .true.)
     endif
  endif

  !----6.0 put att
  if ( len_trim(units) > 0 .and. units(1:1) /= '=') call nccheck(nf90_put_att(ncid, varid, "units", units), 'wrong in put units', .false.)
  if ( len_trim(long_name) > 0 .and. long_name(1:1) /= '=') call nccheck(nf90_put_att(ncid, varid, "long_name",long_name), 'wrong in put long_name', .false.)
  call nccheck(nf90_put_att(ncid, nf90_global, "File-type", "HAFS VI pre-file on rot-ll grids derived from HAFS restart files"), 'wrong in put_att', .false.)
  call date_and_time(date(1), date(2), date(3), date_time)
  call nccheck(nf90_put_att(ncid, nf90_global, "Created_date",date(1)), 'wrong input Created_date', .false.)

  call nccheck(nf90_close(ncid), 'wrong in close '//trim(ncfile), .true.)

  return
  end subroutine write_nc_real_par

!========================================================================================
  subroutine nccheck(status, states, ifstop)

  use netcdf
  implicit none
  integer, intent ( in) :: status
  character(len=*), intent(in)    :: states
  logical, intent ( in) :: ifstop

  if (status /= nf90_noerr) then
     write(*,'(a,a)')trim(nf90_strerror(status)), '   '//trim(states)
     if ( ifstop ) stop
  end if
  end subroutine nccheck
!=======================================================================================
