program subprocess_test
  use sia_subprocess
  use sia_time, only: nanosleep
  type(subprocess) :: p(3)
  logical :: go

  call p(1)%start("/bin/sleep 2 ; /usr/bin/seq 1 20|perl -ne 'chomp; print'")
  call p(2)%start("/bin/false")
  call p(3)%start("/bin/sleep 10")

  go=.true.
  do while(go)
     go=.false.
     do i=1,3
        call p(i)%check()
        go = go .or. p(i)%state==IS_RUNNING .or. p(i)%state==IS_ERROR
18      format('Process #',I0,' state = ',I0,' statearg=',I0,' id=',I0)
        print 18,i,p(i)%state,p(i)%statearg,p(i)%procid
     enddo
     call nanosleep(1.0_8)
  end do
end program subprocess_test
