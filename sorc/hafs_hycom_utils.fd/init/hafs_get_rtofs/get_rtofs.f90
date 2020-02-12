program prog_get_rtofs
  use module_get_rtofs
  use sia_mpi_worklist
  use mpi
  implicit none
  type(rtofs_getter) :: getter
  integer :: ierr
  call MPI_Init(ierr)
  call getter%read_namelist('get_rtofs.nml',MPI_COMM_WORLD,ierr)
  if(ierr/=0) then
     write(0,*) 'ABORT: Could not read namelist.'
     call MPI_Abort(MPI_COMM_WORLD,2,ierr)
     stop 2
  endif
  
  do while(ierr==0 .and. getter%active)
     call getter%step(ierr)
  enddo

  if(ierr/=0) then
10   format(I0,'/',I0,' failed')
     print 10,getter%n_failed,getter%n_work
     stop 1
  elseif(getter%irank==0) then
     print 20,getter%n_work
20   format('All ',I0,' completed successfully.')
  endif
  call MPI_Finalize(ierr)
end program prog_get_rtofs
