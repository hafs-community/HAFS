program path_test
  use sia_fileop
  use sia_cmdarg
  type(args) :: cmd
  character(len=:), pointer :: out
1 format(A,'("',A,'") = "',A,'"')
2 format('pathjoin("',A,'","',A,'") = "',A,'"')
  call cmd%getargs
  if(size(cmd%a)<1) then
     call usage('Specify which operation to run.')
  endif
  select case(cmd%a(1)%s)
  case('basename')
     if(size(cmd%a)/=2) call usage('Basename takes exactly one argument.')
     out=>basename(cmd%a(2)%s)
     print 1,'basename',cmd%a(2)%s,out
     nullify(out)
  case('dirname')
     if(size(cmd%a)/=2) call usage('Dirname takes exactly one argument.')
     out=>dirname(cmd%a(2)%s)
     print 1,'dirname',cmd%a(2)%s,out
     nullify(out)
  case('pathjoin')
     if(size(cmd%a)/=3) call usage('Pathjoin takes exactly two arguments.')
     out=>pathjoin(cmd%a(2)%s,cmd%a(3)%s)
     print 2,cmd%a(2)%s,cmd%a(3)%s,out
     deallocate(out)
     nullify(out)
  case default
     call usage('Invalid operation; must be basename, dirname or pathjoin')
  end select
contains
  subroutine usage(why)
    character*(*), intent(in) :: why
7   format(A)
9   format("Aborting.  Reason: ",A)
    write(0,7) 'ERROR: Format: path_test operation arg [arg [...]]'
    write(0,7) 'Supported operations:'
    write(0,7) '  path_test basename /a/b/c/d   =>  d'
    write(0,7) '  path_test dirname  /a/b/c/d   =>  /a/b/c'
    write(0,7) '  path_test pathjoin /a/b c/d   =>  /a/b/c/d'
    write(0,9) why
    stop 1
  end subroutine usage
end program path_test
