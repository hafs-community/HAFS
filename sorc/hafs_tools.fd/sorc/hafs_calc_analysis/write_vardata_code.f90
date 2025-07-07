  !> @file
  !! @brief Write variable data.
  !! Optional return errcode
  !! @author Jeff Whitaker, Cory Martin
  type(Dataset), intent(inout) :: dset
  character(len=*), intent(in) :: varname
  integer, intent(in), optional :: nslice
  integer, intent(in), optional :: slicedim
  integer, intent(out), optional :: errcode
  integer ncerr, nvar, ncount, ndim, nd, n
  integer, allocatable, dimension(:) :: start, count, dimlens
  logical is_slice
  logical return_errcode
  ! check if use the errcode
  if(present(errcode)) then
     return_errcode=.true.
     errcode = 0
  else
     return_errcode=.false.
  endif
  ! check if count/dims of data are avail
  if (present(nslice)) then
     ncount = nslice
     is_slice = .true.
  else
     ncount = 1
     is_slice = .false.
  endif
  ! define variable name and allocate variable
  nvar = get_nvar(dset,varname)
  allocate(start(dset%variables(nvar)%ndims),count(dset%variables(nvar)%ndims))
  allocate(dimlens(dset%variables(nvar)%ndims))
  start(:) = 1
  count(:) = 1
  dimlens(:) = 1
  if (present(slicedim)) then
     nd = slicedim
  else
     nd = dset%variables(nvar)%ndims
  end if
  ndim = 1
  do n=1,dset%variables(nvar)%ndims
     if (n == nd) then
        start(n) = ncount
        count(n) = 1
     else
        start(n) = 1
        count(n) = dset%variables(nvar)%dimlens(n)
        dimlens(ndim) = dset%variables(nvar)%dimlens(n)
        ndim = ndim + 1
     end if
  end do
  ! write operations on a parallel file system are performed collectively
  ncerr = nf90_var_par_access(dset%ncid, dset%variables(nvar)%varid, nf90_collective)
  if (is_slice) then
     if (dset%variables(nvar)%ndims > 1) then
        ncerr = nf90_put_var(dset%ncid, dset%variables(nvar)%varid,values, &
                             start=start,count=count)
     else if (dset%variables(nvar)%ndims == 1) then
        if (return_errcode) then
           errcode = -1
           return
        else
           print *,'cannot write a slice to a 1d variable'
           stop 99
        endif
     endif
  else if (present(ncstart) .and. present(nccount)) then
     ncerr = nf90_put_var(dset%ncid, dset%variables(nvar)%varid,values, &
          start=ncstart, count=nccount)
  else
     ncerr = nf90_put_var(dset%ncid, dset%variables(nvar)%varid, values)
  endif
  if (return_errcode) then
     call nccheck(ncerr,halt=.false.)
     errcode=ncerr
     if (ncerr /= 0) return
  else
     call nccheck(ncerr)
  endif
  ! reset unlim dim size for all variables
  if (dset%variables(nvar)%hasunlim) then
     if (return_errcode) then
        call set_varunlimdimlens_(dset,errcode)
     else
        call set_varunlimdimlens_(dset)
     endif
  endif
