!========================================================================================
  subroutine rd_grid_spec_data(ncfile, grid)

  use netcdf
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

  write(*,'(a,i0,a,i0,a,i0,a,i0,a)')'  domain dims:     (1:1) --> (',grid%grid_xt,':1) --> (',grid%grid_xt,':',grid%grid_yt,') --> (1:',grid%grid_yt,')'
  write(*,'(a, 2f7.2,a,2f7.2,a,2f7.2,a,2f7.2)')'       T-cell:', grid%grid_lont(1,1),grid%grid_latt(1,1), '--->', &
                                grid%grid_lont(grid%grid_xt,1),grid%grid_latt(grid%grid_xt,1), '--->', &
                             grid%grid_lont(grid%grid_xt,grid%grid_yt),grid%grid_latt(grid%grid_xt,grid%grid_yt), '--->', &
                                grid%grid_lont(1,grid%grid_yt),grid%grid_latt(1,grid%grid_yt)
  write(*,'(a,f7.2,a,f7.2,a,f7.2,a,f7.2)')'   grid_lont :',minval(grid%grid_lont),':',maxval(grid%grid_lont),' grid_latt :',minval(grid%grid_latt),':',maxval(grid%grid_latt)
  write(*,'(a,i0,a,i0,a,i0,a,i0,a)')'  domain dims:     (1:1) --> (',grid%grid_x ,':1) --> (',grid%grid_x ,':',grid%grid_y ,') --> (1:',grid%grid_y ,')'
  write(*,'(a, 2f7.2,a,2f7.2,a,2f7.2,a,2f7.2)')'             :', grid%grid_lon (1,1),grid%grid_lat (1,1), '--->', &
                                grid%grid_lon (grid%grid_x ,1),grid%grid_lat (grid%grid_x ,1), '--->', &
                             grid%grid_lon (grid%grid_x ,grid%grid_y ),grid%grid_lat (grid%grid_x ,grid%grid_y ), '--->', &
                                grid%grid_lon (1,grid%grid_y ),grid%grid_lat (1,grid%grid_y )
  write(*,'(a,f7.2,a,f7.2,a,f7.2,a,f7.2)')'   grid_lon  :',minval(grid%grid_lon ),':',maxval(grid%grid_lon ),' grid_lat  :',minval(grid%grid_lat ),':',maxval(grid%grid_lat)

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
  subroutine nccheck(status, states, ifstop)

  use netcdf
  implicit none
  integer, intent ( in) :: status
  character(len=*), intent(in)    :: states
  logical, intent ( in) :: ifstop

  if (status /= nf90_noerr) then
     write(*,*)trim(nf90_strerror(status)), '   '//trim(states)
     if ( ifstop ) stop
  end if
  end subroutine nccheck
!=======================================================================================
