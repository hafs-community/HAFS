!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! module inc2anl
!!        contains subroutines for calculating analysis fields
!!        for a given input background and increment
!! Original: 2019-09-18   martin   - original module
!!           2019-10-24   martin   - removed support for NEMSIO background but
!!                                   allows for either NEMSIO or netCDF analysis write
!!           2020-01-21   martin   - parallel IO support added
!!           2024-08-12   martin   - aerosol support added
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module inc2anl
  implicit none

  private

  public :: gen_anl, close_files

  integer, parameter :: nincv=13
  character(len=7) :: incvars_nemsio(nincv), incvars_netcdf(nincv), incvars_ncio(nincv)
  integer, parameter :: nnciov=23
  integer, parameter :: naero=14
  integer, parameter :: naero_copy=6
  character(len=7) :: iovars_netcdf(nnciov), iovars_aero(naero), copyvars_aero(naero_copy)
  character(len=50) :: incvars_aero(naero)

  data incvars_nemsio / 'ugrd   ', 'vgrd   ', 'dpres  ', 'delz   ', 'o3mr   ',&
                        'tmp    ', 'spfh   ', 'clwmr  ', 'icmr   ', 'rwmr   ',&
                        'snmr   ', 'grle   ', 'pres   '/
  data incvars_netcdf / 'u      ', 'v      ', 'delp   ', 'delz   ', 'o3mr   ',&
                        'T      ', 'sphum  ', 'liq_wat', 'icmr   ', 'rwmr   ',&
                        'snmr   ', 'grle   ', 'pres   '/
  data incvars_ncio /   'ugrd   ', 'vgrd   ', 'dpres  ', 'delz   ', 'o3mr   ',&
                        'tmp    ', 'spfh   ', 'clwmr  ', 'icmr   ', 'rwmr   ',&
                        'snmr   ', 'grle   ', 'pressfc'/
  data iovars_netcdf /  'grid_xt', 'grid_yt', 'pfull  ', 'phalf  ', 'clwmr  ',&
                        'delz   ', 'dpres  ', 'dzdt   ', 'grle   ', 'hgtsfc ',&
                        'icmr   ', 'o3mr   ', 'pressfc', 'rwmr   ', 'snmr   ',&
                        'spfh   ', 'tmp    ', 'ugrd   ', 'vgrd   ', 'cld_amt',&
                        'nccice ', 'nconrd ', 'omga   '/
  data iovars_aero / 'so4    ', 'bc1    ', 'bc2    ', 'oc1    ', 'oc2    ', &
                     'dust1  ', 'dust2  ', 'dust3  ', 'dust4  ', 'dust5  ',&
                     'seas1  ', 'seas2  ', 'seas3  ', 'seas4  '/
  data incvars_aero / 'mass_fraction_of_sulfate_in_air',&
                      'mass_fraction_of_hydrophobic_black_carbon_in_air', &
                      'mass_fraction_of_hydrophilic_black_carbon_in_air', &
                      'mass_fraction_of_hydrophobic_organic_carbon_in_air', &
                      'mass_fraction_of_hydrophilic_organic_carbon_in_air', &
                      'mass_fraction_of_dust001_in_air', 'mass_fraction_of_dust002_in_air', &
                      'mass_fraction_of_dust003_in_air', 'mass_fraction_of_dust004_in_air', &
                      'mass_fraction_of_dust005_in_air', 'mass_fraction_of_sea_salt001_in_air', &
                      'mass_fraction_of_sea_salt002_in_air', 'mass_fraction_of_sea_salt003_in_air', &
                      'mass_fraction_of_sea_salt004_in_air' /
  data copyvars_aero / 'seas5  ', 'so2    ', 'dms    ', 'msa    ', 'pm25   ', 'pm10   ' /

contains
  subroutine gen_anl
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine gen_anl
  !            loop through fields, read in first guess, read in
  !            increment, add the two together, and write out
  !            the analysis to a new file
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: mype, do_aero, jedi
    implicit none
    ! variables local to this subroutine
    integer :: i, j, iincvar
    logical :: use_increment,readinc


    ! loop through each variable in the background file
    do i=1,nnciov
      use_increment = .false.
      iincvar = -999
      ! determine if we are computing the increment for this field
      do j=1,nincv
        if (iovars_netcdf(i) == incvars_ncio(j)) then
          use_increment = .true.
          iincvar = j
        end if
      end do
      if (use_increment) then
        if (iovars_netcdf(i) == 'pressfc') then
          ! special case for surface pressure
          if (mype==0) print *, 'Computing and Adding Surface Pressure Increment'
          call add_psfc_increment
        else
          ! call generic subroutine for all other fields
          call add_increment(iovars_netcdf(i), incvars_netcdf(iincvar), readinc)
          if (mype==0) then
            if ((.not.jedi) .or. (jedi.and.readinc)) then
               print *, 'Adding Increment to ', iovars_netcdf(i), incvars_netcdf(iincvar)
            else
               print *, 'Copying from Background ', iovars_netcdf(i)
            endif
          endif
        endif
      else
        ! otherwise just write out what is in the input to the output
        if (mype==0) print *, 'Copying from Background ', iovars_netcdf(i)
        call copy_ges_to_anl(iovars_netcdf(i))
      end if
    end do

    ! if including aerosols, loop and add them
    if (do_aero) then
      do i=1,naero
        if (mype==0) print *, 'Adding Increment to ', iovars_aero(i)
        call add_aero_inc(iovars_aero(i), incvars_aero(i))
      end do
      ! need to handle fields that just need copied
      do i=1,naero_copy
        if (mype==0) print *, 'Copying from Background ', copyvars_aero(i)
        call copy_ges_to_anl(copyvars_aero(i))
      end do
    end if

  end subroutine gen_anl

  subroutine close_files
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine close_files
  !            close netCDF files before ending program
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcstncfile, anlncfile
    use module_ncio, only: close_dataset
    implicit none

    call close_dataset(fcstncfile)
    call close_dataset(anlncfile)
  end subroutine close_files

  subroutine copy_ges_to_anl(varname)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine copy_ges_to_anl
  !            generic subroutine for copying guess fields directly to
  !            a new analysis file
  !  args:
  !       varname - input string of variable to process
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcstncfile, anlncfile, &
                                  nlat, nlon, nlev, anlfile, use_nemsio_anl, &
                                  mype, levpe
    use module_ncio, only: Dataset, read_vardata, write_vardata, &
                                  open_dataset, close_dataset, has_var
    use nemsio_module
    implicit none
    character(7), intent(in) :: varname
    real, allocatable, dimension(:) :: work1d
    real, allocatable, dimension(:,:) :: work2d
    real(8), allocatable, dimension(:) :: work1d8
    real(8), allocatable, dimension(:,:) :: work2d8
    real, allocatable, dimension(:,:,:) :: work3d
    integer :: iret, k, krev

    if (has_var(fcstncfile, varname)) then
      select case (varname)
        case ('grid_xt', 'grid_yt')
          if (.not. use_nemsio_anl) then
            call read_vardata(fcstncfile, varname, work1d8)
            call write_vardata(anlncfile, varname, work1d8)
          end if
        case ('pfull  ', 'phalf  ')
          if (.not. use_nemsio_anl) then
            call read_vardata(fcstncfile, varname, work1d)
            call write_vardata(anlncfile, varname, work1d)
          end if
        case ('lat    ', 'lon    ')
          if (.not. use_nemsio_anl) then
            call read_vardata(fcstncfile, varname, work2d8)
            call write_vardata(anlncfile, varname, work2d8)
          end if
        case ('hgtsfc ')
          call read_vardata(fcstncfile, varname, work2d)
          if (use_nemsio_anl) then
            if (.not. allocated(work1d)) allocate(work1d(nlat*nlon))
            work1d = reshape(work2d,(/size(work1d)/))
            call nemsio_writerecv(anlfile, 'hgt', 'sfc', 1, work1d, iret=iret)
            if (iret /=0) write(6,*) 'Error with NEMSIO write', 'hgt', 'sfc', 1, 'iret=',iret
          else
            call write_vardata(anlncfile, varname, work2d)
          end if
        case default
          if (use_nemsio_anl) then
            call read_vardata(fcstncfile, varname, work3d)
            if (.not. allocated(work1d)) allocate(work1d(nlat*nlon))
            do k=1,nlev
              krev = (nlev+1)-k
              work1d = reshape(work3d(:,:,krev),(/size(work1d)/))
              call nemsio_writerecv(anlfile, trim(varname), 'mid layer', k, work1d, iret=iret)
              if (iret /=0) write(6,*) 'Error with NEMSIO write', trim(varname), 'mid layer', k, 'iret=',iret
            end do
          else
            do k=1,nlev
              if (mype == levpe(k)) then
                call read_vardata(fcstncfile, varname, work3d, nslice=k, slicedim=3)
                call write_vardata(anlncfile, varname, work3d, nslice=k, slicedim=3)
              end if
            end do
          end if
      end select
    else
      if (mype == 0) write(6,*) varname, 'not in background file, skipping...'
    end if

  end subroutine copy_ges_to_anl

  subroutine add_aero_inc(fcstvar, incvar)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine add_aero_inc
  !            generic subroutine for adding increment to background
  !            and writing out to analysis for aerosol variables
  !  args:
  !       fcstvar - input string of netCDF fcst/anal var name
  !       incvar  - input string of netCDF increment var name
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcstncfile, anlncfile, aero_file,&
    nlat, nlon, nlev, anlfile, levpe, mype
    use module_ncio, only: Dataset, read_vardata, write_vardata, &
        open_dataset, close_dataset, has_var
    use nemsio_module
    implicit none
    ! input vars
    character(7), intent(in) :: fcstvar
    character(50), intent(in) :: incvar
    ! local variables
    real, allocatable, dimension(:,:,:) :: work3d_bg
    real, allocatable, dimension(:,:) :: work3d_inc
    real, allocatable, dimension(:) :: work1d
    integer :: j,jj,k,krev,iret
    type(Dataset) :: incncfile
    do k=1,nlev
      if (mype == levpe(k)) then
        ! get first guess
        call read_vardata(fcstncfile, trim(fcstvar), work3d_bg, nslice=k, slicedim=3)
        ! get increment
        incncfile = open_dataset(aero_file)
        call read_vardata(incncfile, trim(incvar), work3d_inc, nslice=k, slicedim=1)
        ! add increment to background
        work3d_bg(:,:,1) = work3d_bg(:,:,1) + work3d_inc(:,:)
        ! write out analysis to file
        call write_vardata(anlncfile, trim(fcstvar), work3d_bg, nslice=k, slicedim=3)
      end if
    end do
    ! clean up and close
    deallocate(work3d_bg, work3d_inc)
    call close_dataset(incncfile)

  end subroutine add_aero_inc

  subroutine add_increment(fcstvar, incvar, readinc)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine add_increment
  !            generic subroutine for adding increment to background
  !            and writing out to analysis
  !  args:
  !       fcstvar - input string of netCDF fcst/anal var name
  !       incvar  - input string of netCDF increment var name
  !                 (without _inc suffix added)
  !       readinc - .true. if read increment, .false otherwise
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcstncfile, anlncfile, incr_file,&
                                  nlat, nlon, nlev, anlfile, use_nemsio_anl, &
                                  levpe, mype, jedi
    use module_ncio, only: Dataset, read_vardata, write_vardata, &
                                  open_dataset, close_dataset, has_var
    use nemsio_module
    implicit none
    ! input vars
    character(7), intent(in) :: fcstvar, incvar
    logical, intent(out):: readinc
    ! local variables
    real, allocatable, dimension(:,:,:) :: work3d_bg
    real, allocatable, dimension(:,:) :: work3d_inc_gsi
    real, allocatable, dimension(:,:,:) :: work3d_inc_jedi
    real, allocatable, dimension(:) :: work1d
    integer :: j,jj,k,krev,iret
    type(Dataset) :: incncfile

    readinc=.true.
    if (has_var(fcstncfile, fcstvar)) then
      do k=1,nlev
        if (mype == levpe(k)) then
          ! get first guess
          call read_vardata(fcstncfile, fcstvar, work3d_bg, nslice=k, slicedim=3)
          ! get increment
          incncfile = open_dataset(incr_file, paropen=.true.)
          ! JEDI-derived increments have a time dimension, so read to appropriate array
          if ( jedi ) then
             if (has_var(incncfile, trim(incvar)//"_inc")) then
                call read_vardata(incncfile, trim(incvar)//"_inc", work3d_inc_jedi, nslice=k, slicedim=3)
                ! add increment to background, otherwise do nothing
                do j=1,nlat
                   jj=nlat+1-j ! increment is S->N, history files are N->S
                   work3d_bg(:,j,1) = work3d_bg(:,j,1) + work3d_inc_jedi(:,jj,1)
                end do
             else
                readinc = .false.
             endif
          else
             call read_vardata(incncfile, trim(incvar)//"_inc", work3d_inc_gsi, nslice=k, slicedim=3)
             ! add increment to background
             do j=1,nlat
                jj=nlat+1-j ! increment is S->N, history files are N->S
                work3d_bg(:,j,1) = work3d_bg(:,j,1) + work3d_inc_gsi(:,jj)
             end do
          end if
          ! write out analysis to file
          if (use_nemsio_anl) then
            allocate(work1d(nlat*nlon))
              krev = (nlev+1)-k
              work1d = reshape(work3d_bg(:,:,krev),(/size(work1d)/))
              call nemsio_writerecv(anlfile, trim(fcstvar), 'mid layer', k, work1d, iret=iret)
              if (iret /=0) write(6,*) 'Error with NEMSIO write', trim(fcstvar), 'mid layer', k, 'iret=',iret
            deallocate(work1d)
          else
            call write_vardata(anlncfile, fcstvar, work3d_bg, nslice=k, slicedim=3)
          end if
        end if
      end do
      ! clean up and close
      if ( jedi ) then
        if (allocated(work3d_inc_jedi)) deallocate(work3d_inc_jedi)
      else
        deallocate(work3d_inc_gsi)
      end if
      deallocate(work3d_bg)
      call close_dataset(incncfile)
    else
      write(6,*) fcstvar, ' not in background file, skipping...'
    end if

  end subroutine add_increment

  subroutine add_psfc_increment
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! subroutine add_psfc_increment
  !            special case of getting surface pressure analysis from
  !            bk5 and delp increment to get sfc pressure increment
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use vars_calc_analysis, only: fcstncfile, anlncfile, nlat, nlon, incr_file,&
                                  use_nemsio_anl, anlfile, nlev, jedi
    use module_ncio, only: Dataset, open_dataset, close_dataset,&
                                  read_vardata, write_vardata, read_attribute
    use nemsio_module
    implicit none
    ! local variables
    real, allocatable, dimension(:,:,:) :: work3d_inc
    real, allocatable, dimension(:,:) :: ps_inc, work2d, work2d_inc
    real, allocatable, dimension(:) :: bk5, work1d
    integer :: iret, j, jj
    type(Dataset) :: incncfile

    allocate(ps_inc(nlon,nlat))
    ! get bk5 from attributes
    call read_attribute(fcstncfile, 'bk', bk5)
    ! read in delp increment to get ps increment from delp increment and bk
    incncfile = open_dataset(incr_file)
    if (.not.jedi) then
       call read_vardata(incncfile, 'delp_inc', work2d_inc, nslice=nlev, slicedim=3)
       ps_inc(:,:) = work2d_inc(:,:) / (bk5(nlev) - bk5(nlev-1))
    else
       call read_vardata(incncfile, 'delp_inc', work3d_inc)
       ps_inc(:,:) = work3d_inc(:,:,nlev) / (bk5(nlev) - bk5(nlev-1))
    endif
    ! read in psfc background
    call read_vardata(fcstncfile, 'pressfc', work2d)
    ! add increment to background
    do j=1,nlat
      jj=nlat+1-j ! increment is S->N, history file is N->S
      work2d(:,j) = work2d(:,j) + ps_inc(:,jj)
    end do
    ! write out to file
    if (use_nemsio_anl) then
      allocate(work1d(nlon*nlat))
      ! now write out new psfc to NEMSIO analysis file
      work1d = reshape(work2d,(/size(work1d)/))
      call nemsio_writerecv(anlfile, 'pres', 'sfc', 1, work1d, iret=iret)
      if (iret /=0) write(6,*) 'Error with NEMSIO write sfc pressure'
      deallocate(work1d)
    else
      call write_vardata(anlncfile, 'pressfc', work2d)
    end if
    ! deallocate and close
    call close_dataset(incncfile)
    deallocate(work2d,ps_inc,bk5)
    if (jedi) then
       deallocate(work3d_inc)
    else
       deallocate(work2d_inc)
    endif

  end subroutine add_psfc_increment

end module inc2anl
