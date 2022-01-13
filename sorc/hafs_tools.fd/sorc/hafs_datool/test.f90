  program test

  implicit none

  !----parameter define
  integer              :: i, j, k, n, iind, iargc, rcode, ks, ke
  character (len=2500) :: actions, arg, arg1
  character (len=2500) :: in_dir='w', in_file='w', in_grid='w', &
                          out_grid='w', out_data='w', out_file='w'

!----------------------------------------------------------------
! 1 --- argc
! 1.1 --- get argc
  if (iargc() .lt. 2) then
     write(*,*)' usage: hafs_datool.x function --in_file=inputfile'
     stop
  else
     call getarg(1, actions)

     do i = 2, iargc()
        call getarg(i, arg)
        j=index(trim(arg),'=',.true.)
        n=len_trim(arg)
        select case (arg(1:j-1))
               case ('--in_dir');         in_dir=arg(j+1:n)
               case ('-i', '--in_file');  in_file=arg(j+1:n)
               case ('--in_grid');        in_grid=arg(j+1:n)
               case ('--out_grid');       out_grid=arg(j+1:n)
               case ('--out_data');       out_data=arg(j+1:n)
               case ('--out_file');       out_file=arg(j+1:n)
        end select
     enddo
  endif

  write(*,'(a)')'in_file='//trim(in_file)//'==='
  write(*,'(a)')'out_file='//trim(out_file)//'==='

  end program 
