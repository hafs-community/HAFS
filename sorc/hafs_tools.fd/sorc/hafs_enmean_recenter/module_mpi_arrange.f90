module module_mpi_arrange
!
!   PRGMMR: Ming Hu          ORG: GSL        DATE: 2022-03-08
!
! ABSTRACT: 
!     This module figure out the piece of fv3lam file each core to read
! 
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES: 
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!
!$$$
!
!_____________________________________________________________________

  implicit none

  integer,parameter :: max_varname_length=20
!
! Rset default to private
!

  private

  public :: mpi_io_arrange

  type :: mpi_io_arrange

      integer,allocatable :: fileid(:)
      character(len=max_varname_length),allocatable :: varname(:)
      integer,allocatable :: vartype(:)
      integer,allocatable :: lvlbegin(:),lvlend(:)
      integer,allocatable :: nx(:),ny(:)
      integer :: ntotalcore

    contains
      procedure :: init
      procedure :: arrange
      procedure :: close
  end type mpi_io_arrange
!
! constants
!
contains

  subroutine init(this,ntotalcore)
!                .      .    .                                       .
! subprogram: 
!   prgmmr:
!
! abstract:
!
! program history log:
!
!   input argument list:
!
!   output argument list:
!
    implicit none

    integer, intent(in)          :: ntotalcore
    class(mpi_io_arrange) :: this
!
    this%ntotalcore=ntotalcore

    allocate(this%fileid(ntotalcore))
    allocate(this%varname(ntotalcore))
    allocate(this%vartype(ntotalcore))
    allocate(this%lvlbegin(ntotalcore))
    allocate(this%lvlend(ntotalcore))
    allocate(this%nx(ntotalcore))
    allocate(this%ny(ntotalcore))

  end subroutine init

  subroutine close(this)
    implicit none
    class(mpi_io_arrange) :: this

    if(allocated(this%fileid))   deallocate(this%fileid)
    if(allocated(this%varname))  deallocate(this%varname)
    if(allocated(this%vartype))  deallocate(this%vartype)
    if(allocated(this%lvlbegin)) deallocate(this%lvlbegin)
    if(allocated(this%lvlend))   deallocate(this%lvlend)
    if(allocated(this%nx))       deallocate(this%nx)
    if(allocated(this%ny))       deallocate(this%ny)
    this%ntotalcore=0

  end subroutine close

  subroutine arrange(this,ncfs_all)
!                .      .    .                                       .
! subprogram: 
!   prgmmr:
!
! abstract:
!
! program history log:
!
!   input argument list:
!
!   output argument list:
!
    use module_ncfile_stat, only : ncfile_stat
    implicit none

    class(mpi_io_arrange) :: this
    type(ncfile_stat),intent(in) :: ncfs_all
!
! MPI distribution array
    integer,allocatable :: fileid(:)
    character(len=max_varname_length),allocatable :: varname(:)
    integer,allocatable :: vartype(:)
    integer,allocatable :: lvlbegin(:),lvlend(:)
    integer,allocatable :: nx(:),ny(:)
    integer :: ntotalcore
    integer :: nlvl2d,nlvl3d,nlvl3d_small
    integer :: nlvlcore
    integer :: nlvlmax
    integer,allocatable :: nlvl3d_list(:)
    integer :: mynlvl3d
!
    integer :: myfieldid
    integer,allocatable :: ib(:),ie(:)
    integer :: i,j,k,n,nn,n3d,nz
!
! start  allocate memory
!
    ntotalcore=this%ntotalcore
    allocate(fileid(ntotalcore))
    allocate(varname(ntotalcore))
    allocate(vartype(ntotalcore))
    allocate(lvlbegin(ntotalcore))
    allocate(lvlend(ntotalcore))
    allocate(nx(ntotalcore))
    allocate(ny(ntotalcore))
    varname=""
    fileid=0
    vartype=0
    lvlbegin=0
    lvlend=0
    nx=0
    ny=0
!
! find number of 3d and 2 d fields
!
    nlvl2d=0
    nlvl3d=0
    nlvl3d_small=0
    nlvlmax=0
    if(ncfs_all%num_totalvl > 0) then
        do k=1,ncfs_all%numvar
           if(ncfs_all%num_dim(k) == 3) then
              if(ncfs_all%dim_3(k) <= 10) then
                ! treat small 3d as 2D field
                nlvl3d_small=nlvl3d_small+ncfs_all%dim_3(k)
              else
                nlvl3d=nlvl3d+1
              endif
           endif
           if(ncfs_all%num_dim(k) == 2) nlvl2d=nlvl2d+1
           if(nlvlmax < ncfs_all%dim_3(k)) nlvlmax=ncfs_all%dim_3(k)
        enddo
    endif
    write(6,*) 'total 2d_level=',nlvl2d,' 3d_level=',nlvl3d, &
               ' 3d_level_small =',nlvl3d_small
    write(6,*) 'max level =',nlvlmax
!
!  check if we have enought cores
!
    if( ntotalcore-nlvl2d-nlvl3d_small < nlvl3d ) then
        write(6,*) 'not enough cores for the paralleli IO',ntotalcore-nlvl2d-nlvl3d_small,nlvl3d
        stop 123
    endif
!
! decide how many cores can be used for each 3d field
!
    if(nlvl3d > 0) then
      allocate(nlvl3d_list(nlvl3d))
      nlvlcore=(ntotalcore-nlvl2d-nlvl3d_small)/nlvl3d
      nlvl3d_list=nlvlcore
      nlvlcore=(ntotalcore-nlvl2d-nlvl3d_small) - nlvl3d*nlvlcore 
      if(nlvlcore > 0 ) then
          do k=1,nlvlcore
            nlvl3d_list(k)=nlvl3d_list(k)+1
          enddo
      endif
      write(6,*) 'cores for each 3D fields=',nlvl3d_list
   endif
!
!  decide boundary for fileid
!
    allocate(ib(ncfs_all%numfiles),ie(ncfs_all%numfiles))    
    ib=0
    ie=0
    ib(1)=1
    ie(1)=ncfs_all%numvarfile(1)
    do i=2,ncfs_all%numfiles
      ib(i)=ie(i-1)+1
      ie(i)=ib(i)+ncfs_all%numvarfile(i)-1
    enddo
!
!  decide which levels of a variable to read for each core
!
    nn=0
    n3d=0
    if(ncfs_all%numvar > 0) then
        do i=1,ncfs_all%numvar
           myfieldid=0
           do n =1, ncfs_all%numfiles
              if(i >= ib(n) .and. i <= ie(n)) myfieldid=n
           enddo
           if(myfieldid <=0 .or. myfieldid > ncfs_all%numfiles) then
              write(6,*) 'something wrong with file id ',myfieldid
              stop 234
           endif
           if(ncfs_all%num_dim(i)==3) then
              nz=ncfs_all%dim_3(i)
              if(nz <= 10) then
                 do k=1,nz
                    nn=nn+1
                    varname(nn)=trim(ncfs_all%list_varname(i))
                    vartype(nn)=ncfs_all%vartype(i)
                    nx(nn)=ncfs_all%dim_1(i)
                    ny(nn)=ncfs_all%dim_2(i)
                    lvlbegin(nn)=k
                    lvlend(nn)=k
                    fileid(nn)=myfieldid
                 enddo
              else
                 n3d=n3d+1
                 mynlvl3d=min(nlvl3d_list(n3d),nz)
                 nlvlcore=nz/mynlvl3d
                 j=nz-nlvlcore*mynlvl3d
                 do k=1,mynlvl3d
                    nn=nn+1
                    fileid(nn)=myfieldid
                    varname(nn)=trim(ncfs_all%list_varname(i))
                    vartype(nn)=ncfs_all%vartype(i)
                    nx(nn)=ncfs_all%dim_1(i)
                    ny(nn)=ncfs_all%dim_2(i)
                    if(k==1) then
                       lvlbegin(nn)=1
                    else
                       lvlbegin(nn)=lvlend(nn-1)+1
                    endif
                    lvlend(nn)=lvlbegin(nn)+nlvlcore-1
                    if(k<=j) lvlend(nn)=lvlend(nn)+1
                    if(k==mynlvl3d) lvlend(nn)=min(lvlend(nn),nz)
                 enddo
              endif
           elseif(ncfs_all%num_dim(i)==2) then
              nn=nn+1
              varname(nn)=trim(ncfs_all%list_varname(i))
              vartype(nn)=ncfs_all%vartype(i)
              nx(nn)=ncfs_all%dim_1(i)
              ny(nn)=ncfs_all%dim_2(i)
              lvlbegin(nn)=1
              lvlend(nn)=1
              fileid(nn)=myfieldid
           endif
        enddo
    endif

    write(6,'(2a5,2x,a10,10a10)') "core","fid","varname","vartype","nx","ny","lvlbegin","lvlend"
    do k=1,ntotalcore
       write(6,'(2I5,2x,a10,10I10)') k,fileid(k),trim(varname(k)),vartype(k),nx(k),ny(k),lvlbegin(k),lvlend(k) 
    enddo
    write(6,*) "======================================================================="
!
!  save results
!
    this%fileid=fileid
    this%varname=varname
    this%vartype=vartype
    this%lvlbegin=lvlbegin
    this%lvlend=lvlend
    this%nx=nx
    this%ny=ny
!
!  release local arrary
!
    deallocate(fileid)
    deallocate(varname)
    deallocate(vartype)
    deallocate(lvlbegin)
    deallocate(lvlend)
    deallocate(nx)
    deallocate(ny)
    if(nlvl3d > 0) deallocate(nlvl3d_list)
    deallocate(ib)
    deallocate(ie)

  end subroutine arrange

end module module_mpi_arrange
