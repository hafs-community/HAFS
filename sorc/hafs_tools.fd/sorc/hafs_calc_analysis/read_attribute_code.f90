  !> @file
  !! @brief Read attribute.
  !! optional return errcode
  !! @author Jeff Whitaker, Cory Martin
  type(Dataset), intent(in) :: dset
  character(len=*), intent(in), optional :: varname
  character(len=*), intent(in) :: attname
  integer, intent(out), optional :: errcode
  integer ncerr, varid, nvar, nlen
  logical return_errcode
  ! check if use the errcode
  if(present(errcode)) then
     return_errcode=.true.
     errcode = 0
  else
     return_errcode=.false.
  endif
  ! access attributes based on variable name
  if(present(varname))then
     nvar = get_nvar(dset,varname)
     varid = dset%variables(nvar)%varid
  else
     varid = NF90_GLOBAL
  endif
  ncerr = nf90_inquire_attribute(dset%ncid, varid, attname, len=nlen)
  if (return_errcode) then
     call nccheck(ncerr,halt=.false.)
     errcode=ncerr
     if (ncerr /= 0) return
  else
     call nccheck(ncerr)
  endif
  if (allocated(values)) deallocate(values)
  allocate(values(nlen))
  ncerr = nf90_get_att(dset%ncid, varid, trim(attname), values)
  ! err check
  if (return_errcode) then
     call nccheck(ncerr,halt=.false.)
     errcode=ncerr
  else
     call nccheck(ncerr)
  endif
