program tree_test
  use sia_treewalk
  use sia_stat
implicit none
  integer :: nargs,iarg
  character*1000 :: path,op
  logical :: finish,error,success
  
  nargs=command_argument_count()
  success=.true.
  call get_command_argument(1,op)
  do iarg=2,nargs
     call get_command_argument(iarg,path)
     print 8,trim(op),trim(path)
     select case(trim(op))
     case('print')
        print *, listtree(trim(path))
     case('deltree')
        print *, deltree(trim(path))
     case default
        print *,'Unknown operation ',trim(op)
     end select

     print *,'... done walking.'
     print *,'finish = ',finish
     print *,'error  = ',error
     success=success .and. .not. error
  end do
  if( .not. success) stop 1

8       format('Treewalk op=',A,' path=',A,' ...')
end program tree_test
