PROGRAM ens_mean_recenter
!
!   PRGMMR: Ming Hu          ORG: GSL        DATE: 2022-02-11
!
! ABSTRACT: 
!     This appllication calculates ensemble mean and recenter
! 
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!
! REMARKS:
!
! ATTRIBUTES:
!
!$$$
!
!_____________________________________________________________________

  use mpi
  use module_ncfile_stat, only : ncfile_stat
  use module_mpi_arrange, only : mpi_io_arrange

  use netcdf, only: nf90_open,nf90_close,nf90_noerr
  use netcdf, only: nf90_get_var,nf90_put_var
  use netcdf, only: nf90_nowrite,nf90_write
  use netcdf, only: nf90_inq_varid
  use netcdf, only: nf90_strerror

  implicit none
! 
  type(ncfile_stat) :: ncfs_all
  type(mpi_io_arrange) :: mpiioarg
!
! MPI variables
  integer :: npe, mype, ierror
! sub communicator
  integer :: new_comm
  integer :: color, key
!
! namelist
  integer, parameter    :: max_num_file = 10
  integer, parameter    :: filename_len=200
  integer :: fv3_io_layout_y
  character(len=filename_len)  :: filebase
  character(len=filename_len)  :: filetail(max_num_file)
  integer                      :: numvar(max_num_file)
  character(len=500)  :: varlist(max_num_file)
  integer    :: ens_size
  logical    :: l_write_mean             ! if write ensmeble mean
  logical    :: l_recenter               ! if recenter

  namelist/setup/ ens_size,fv3_io_layout_y,l_write_mean,l_recenter, &
                  filebase,filetail,&
                  numvar,varlist

  character (len=filename_len)   :: directory                 ! General filename stub.
  character (len=filename_len)   :: filename                  ! General filename stub.
  character (len=filename_len)   :: input_file(max_num_file)
  character (len=filename_len)   :: input_control_file(max_num_file)
  character (len=filename_len)   :: input_ensmean_file(max_num_file)
  integer :: totalnumfiles

  logical :: ifexist
!
! MPI distribution array
  integer :: mype_fileid
  character(len=20) :: mype_varname
  integer :: mype_vartype
  integer :: mype_nx,mype_ny
  integer :: mype_lbegin,mype_lend
!
! array
  real(4),allocatable :: tmpd3r4(:,:,:)
  real(8),allocatable :: tmpd3r8(:,:,:)

  real(8),allocatable :: d3r8_mean(:,:,:)
  real(4),allocatable :: d4r4(:,:,:,:)

  integer :: startloc(3)
  integer :: countloc(3)
  integer :: ncioid,var_id
!
!
  integer :: i,j,k,iret,ilev,iens
  real(8) :: average
  character (len=3)    :: memnun 
  logical :: l_positive
!
!**********************************************************************
!**********************************************************************
!
!            END OF DECLARATIONS....start of program

! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)
!
!  get namelist
!
  numvar=0
  varlist=''
  filetail=''
  ens_size=1
  fv3_io_layout_y=1
  filebase='fv3sar_tile1'
  l_write_mean=.true.
  l_recenter=.false.

  inquire(file='hafs_mean_recenter.nml', EXIST=ifexist )
  if(ifexist) then
    open(10,file='hafs_mean_recenter.nml',status='old')
       read(10,setup)
    close(10)
  else
     write(*,*) 'No namelist file exist, use default values'
  endif

  if(mype==0) then
    write(*,*) 'Namelist setup are:'
    write(*,setup)
  endif

  !filename = trim(adjustl(directory)) // trim(adjustl(filebase))
  filename = trim(adjustl(filebase))
! decide how many files we will process
  totalnumfiles=0
  do i=1,max_num_file
    if(numvar(i) > 0) then
       totalnumfiles=totalnumfiles+1
    endif
  enddo
! decide control file name and ensemble mean file name
  do i=1,totalnumfiles
     input_control_file(i) = 'control_'//trim(filetail(i))
     input_ensmean_file(i) = trim(adjustl(filebase))//'_'//trim(filetail(i))
  enddo
  if(l_recenter) then
     do i=1,totalnumfiles
        input_file(i) = input_control_file(i)
     end do
  else
     do i=1,totalnumfiles
        input_file(i) = input_ensmean_file(i)
     enddo
  endif
!
  if(mype==0) then
!
! find dimension of each field
!
     call ncfs_all%init(totalnumfiles,input_file(1:totalnumfiles), &
                        numvar(1:totalnumfiles),varlist(1:totalnumfiles))
     call ncfs_all%fill_dims()
!
!  distibute variables to each core
!
     call mpiioarg%init(npe)
     call mpiioarg%arrange(ncfs_all)
         
     call ncfs_all%close()
  endif

  call MPI_Scatter(mpiioarg%fileid, 1, mpi_integer, mype_fileid, 1, mpi_integer, 0, MPI_COMM_WORLD,ierror)
  call MPI_Scatter(mpiioarg%varname, 20, mpi_character, mype_varname, 20, mpi_character, 0, MPI_COMM_WORLD,ierror)
  call MPI_Scatter(mpiioarg%vartype, 1, mpi_integer, mype_vartype, 1, mpi_integer, 0, MPI_COMM_WORLD,ierror)
  call MPI_Scatter(mpiioarg%nx, 1, mpi_integer, mype_nx, 1, mpi_integer, 0, MPI_COMM_WORLD,ierror)
  call MPI_Scatter(mpiioarg%ny, 1, mpi_integer, mype_ny, 1, mpi_integer, 0, MPI_COMM_WORLD,ierror)
  call MPI_Scatter(mpiioarg%lvlbegin, 1, mpi_integer, mype_lbegin, 1, mpi_integer, 0, MPI_COMM_WORLD,ierror)
  call MPI_Scatter(mpiioarg%lvlend, 1, mpi_integer, mype_lend, 1, mpi_integer, 0, MPI_COMM_WORLD,ierror)

  if(mype==0) call mpiioarg%close()
  call mpi_barrier(MPI_COMM_WORLD,ierror)

! Create sub-communicator to handle each file
  key=mype+1
  if(mype_fileid > 0 .and. mype_fileid <= totalnumfiles) then
     color = mype_fileid
  else
     color = MPI_UNDEFINED
  endif

  call MPI_Comm_split(mpi_comm_world,color,key,new_comm,ierror)
  if ( ierror /= 0 ) then
     write(6,'(a,i5)')'***ERROR*** after mpi_comm_create with iret = ',ierror
     call mpi_abort(mpi_comm_world,101,ierror)
  endif
!
! read 2D field from each file using sub communicator
!
  if (MPI_COMM_NULL /= new_comm) then
     if(mype_vartype==5) then
        allocate(tmpd3r4(mype_nx,mype_ny,1))
     elseif(mype_vartype==6) then
        allocate(tmpd3r8(mype_nx,mype_ny,1))
     else
        write(6,*) 'Warning, unknown datatype'
     endif
     allocate(d4r4(mype_nx,mype_ny,mype_lbegin:mype_lend,0:ens_size))
!
! now read in each fields from fv3 file
!
     d4r4=0.0
     do iens=0,ens_size

        if(iens==0) then
           ! save control file as ensemble 0
           filename = trim(input_file(mype_fileid))
        else
           write(UNIT=memnun,FMT='(i3.3)') iens
           filename = trim(adjustl(filebase))//'_mem'//memnun//'_'//trim(filetail(mype_fileid))
        endif
        iret=nf90_open(trim(filename),nf90_nowrite,ncioid,comm=new_comm,info=MPI_INFO_NULL)
        if(iret/=nf90_noerr) then
           write(6,*)' problem opening ', trim(filename),' fileid=',mype_fileid,', Status =',iret
           write(6,*)  nf90_strerror(iret)
           call flush(6)
           stop 333 
        endif
        if(mype==0) write(*,*) 'reading ensemble member =',iens

        do ilev=mype_lbegin,mype_lend
           startloc=(/1,1,ilev/)
           countloc=(/mype_nx,mype_ny,1/)

           iret=nf90_inq_varid(ncioid,trim(adjustl(mype_varname)),var_id)
           if(mype_vartype==5) then
              iret=nf90_get_var(ncioid,var_id,tmpd3r4,start=startloc,count=countloc)
              d4r4(:,:,ilev,iens)=tmpd3r4(:,:,1)
           elseif(mype_vartype==6) then
              iret=nf90_get_var(ncioid,var_id,tmpd3r8,start=startloc,count=countloc)
              d4r4(:,:,ilev,iens)=tmpd3r8(:,:,1)
           endif
        enddo  ! ilev

        iret=nf90_close(ncioid)

     enddo ! iens
!
!  calculate ensemble mean
!
     if(mype==0) write(*,*) 'calculate ensemble mean'
     allocate(d3r8_mean(mype_nx,mype_ny,mype_lbegin:mype_lend))
     d3r8_mean=0.0_8
     do iens=1,ens_size
        do ilev=mype_lbegin,mype_lend
           d3r8_mean(:,:,ilev)=d3r8_mean(:,:,ilev)+d4r4(:,:,ilev,iens)
        enddo
     enddo
     average=ens_size
     average=1.0_8/average
     d3r8_mean=d3r8_mean*average
!
!     do ilev=mype_lbegin,mype_lend
!        write(6,'(I5,A10,I5,2f25.9)') mype_fileid,trim(adjustl(mype_varname)),ilev,  &
!                   maxval(d3r8_mean(:,:,ilev)),minval(d3r8_mean(:,:,ilev))
!     enddo  ! ilev
! write mean
     if( l_write_mean ) then
        filename = trim(input_ensmean_file(mype_fileid))
        iret=nf90_open(trim(filename),nf90_write,ncioid,comm=new_comm,info=MPI_INFO_NULL)
        if(iret/=nf90_noerr) then
           write(6,*)' problem opening ', trim(filename),' fileid=',mype_fileid,', Status =',iret
           write(6,*)  nf90_strerror(iret)
           call flush(6)
           stop 555 
        endif
        if(mype==0) write(*,*) 'write ensemble mean = ', &
                                trim(input_ensmean_file(mype_fileid))

        do ilev=mype_lbegin,mype_lend
           startloc=(/1,1,ilev/)
           countloc=(/mype_nx,mype_ny,1/)

           iret=nf90_inq_varid(ncioid,trim(adjustl(mype_varname)),var_id)
           if(mype_vartype==5) then
              tmpd3r4(:,:,1)=d3r8_mean(:,:,ilev)
              iret=nf90_put_var(ncioid,var_id,tmpd3r4,start=startloc,count=countloc)
           elseif(mype_vartype==6) then
              tmpd3r8(:,:,1)=d3r8_mean(:,:,ilev)
              iret=nf90_put_var(ncioid,var_id,tmpd3r8,start=startloc,count=countloc)
           endif
        enddo  ! ilev

        iret=nf90_close(ncioid)
     endif
!
! recenter  
!
     if(l_recenter) then
! get ensemble perturbations and then add it to the base state
        l_positive=.false.
        if( trim(mype_varname)=="ref_f3d" .or. trim(mype_varname)=="sphum" .or.  &
           trim(mype_varname)=="liq_wat" .or. trim(mype_varname)=="snowwat" .or. & 
           trim(mype_varname)=="rainwat" .or. trim(mype_varname)=="ice_wat" .or.&
           trim(mype_varname)=="o3mr" .or. trim(mype_varname)=="q2m" .or.&
           trim(mype_varname)=="smois" .or. trim(mype_varname)=="water_nc" .or.&
           trim(mype_varname)=="rain_nc" .or. trim(mype_varname)=="ice_nc" .or.&
           trim(mype_varname)=="graupel" )then
           l_positive=.true.
        endif

     if(mype==0) write(*,*) 'calculate ensemble perturbations and add it to base state'
     do iens=1,ens_size
        do ilev=mype_lbegin,mype_lend
           d4r4(:,:,ilev,iens)=d4r4(:,:,ilev,iens)-d3r8_mean(:,:,ilev)+d4r4(:,:,ilev,0)
           if(l_positive) d4r4(:,:,ilev,iens)=max(d4r4(:,:,ilev,iens), 0.0)
        enddo
     enddo
!
!  write to each member
!
        do iens=1,ens_size

           write(UNIT=memnun,FMT='(i3.3)') iens
           filename = 'rec_'//trim(adjustl(filebase))//'_mem'//memnun//'_'//trim(filetail(mype_fileid))
           iret=nf90_open(trim(filename),nf90_write,ncioid,comm=new_comm,info=MPI_INFO_NULL)
           if(iret/=nf90_noerr) then
              write(6,*)' problem opening ', trim(filename),' fileid=',mype_fileid,', Status =',iret
              write(6,*)  nf90_strerror(iret)
              call flush(6)
              stop 444 
           endif
           if(mype==0) write(*,*) 'update ensemble member =',iens

           do ilev=mype_lbegin,mype_lend
              startloc=(/1,1,ilev/)
              countloc=(/mype_nx,mype_ny,1/)

              iret=nf90_inq_varid(ncioid,trim(adjustl(mype_varname)),var_id)
              if(mype_vartype==5) then
                 tmpd3r4(:,:,1)=d4r4(:,:,ilev,iens)
                 iret=nf90_put_var(ncioid,var_id,tmpd3r4,start=startloc,count=countloc)
              elseif(mype_vartype==6) then
                 tmpd3r8(:,:,1)=d4r4(:,:,ilev,iens)
                 iret=nf90_put_var(ncioid,var_id,tmpd3r8,start=startloc,count=countloc)
              endif
           enddo  ! ilev

           iret=nf90_close(ncioid)

        enddo ! iens

     endif

! release memory
     deallocate(d3r8_mean)
     deallocate(d4r4)

     if(mype_vartype==5) then
        deallocate(tmpd3r4)
     elseif(mype_vartype==6) then
        deallocate(tmpd3r8)
     else
        write(6,*) 'Warning, unknown datatype'
     endif
  endif

  if (MPI_COMM_NULL /= new_comm) then
     call MPI_Comm_free(new_comm,iret)
  endif
!
  call mpi_barrier(MPI_COMM_WORLD,ierror)
  if(mype==0)  write(6,*) "=== RRFS ENS MEAN and RECENTER SUCCESS ==="
  call MPI_FINALIZE(ierror)
!
END PROGRAM ens_mean_recenter

