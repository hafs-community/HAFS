  !> @file
  !! @brief Write attributes.
  !! optional return errcode
  !! @author Jeff Whitaker, Cory Martin
  type(Dataset), intent(in) :: dset
  character(len=*), intent(in), optional :: varname
  character(len=*), intent(in) :: attname
  integer, intent(out), optional :: errcode
  integer ncerr, varid, nvar
  logical return_errcode
  ! check if use the errcode
  if(present(errcode)) then
     return_errcode=.true.
     errcode = 0
  else
     return_errcode=.false.
  endif
  ! find the variable
  if(present(varname))then
     nvar = get_nvar(dset,varname)
     varid = dset%variables(nvar)%varid
  else
     varid = NF90_GLOBAL
  endif
  ! varibale data into define mode
  ncerr = nf90_redef(dset%ncid)
  if (return_errcode) then
     call nccheck(ncerr,halt=.false.)
     errcode=ncerr
     if (ncerr /= 0) return
  else
     call nccheck(ncerr)
  endif
  ! add attributes
  ncerr = nf90_put_att(dset%ncid, varid, trim(attname), values)
  if (return_errcode) then
     call nccheck(ncerr,halt=.false.)
     errcode=ncerr
     ncerr = nf90_enddef(dset%ncid)
     return
  else
     call nccheck(ncerr)
     ncerr = nf90_enddef(dset%ncid)
     call nccheck(ncerr)
  endif
