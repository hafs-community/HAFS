module diagsread_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! diagsread :: diagsread_interface
  ! Copyright (C) 2019 Henry R. Winterbottom

  ! Email: henry.winterbottom@noaa.gov

  ! This program is free software: you can redistribute it and/or
  ! modify it under the terms of the GNU General Public License as
  ! published by the Free Software Foundation, either version 3 of the
  ! License, or (at your option) any later version.

  ! This program is distributed in the hope that it will be useful,
  ! but WITHOUT ANY WARRANTY; without even the implied warranty of
  ! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  ! General Public License for more details.

  ! You should have received a copy of the GNU General Public License
  ! along with this program.  If not, see
  ! <http://www.gnu.org/licenses/>.

  !=======================================================================

  ! Define associated modules and subroutines

  use kinds_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: diags_convinfo_struct
  public :: diags_obsinfo_struct
  public :: diags_spval
  public :: diagsread_conv_count
  public :: diagsread_conv_data

  ! Define local variables

  type diags_convinfo_struct
     character(len=500)                                                 :: filename
     integer                                                            :: nobs_dw(2)
     integer                                                            :: nobs_gps(2)
     integer                                                            :: nobs_ps(2)
     integer                                                            :: nobs_pw(2)
     integer                                                            :: nobs_q(2)
     integer                                                            :: nobs_rw(2)
     integer                                                            :: nobs_spd(2)
     integer                                                            :: nobs_srw(2)
     integer                                                            :: nobs_sst(2)
     integer                                                            :: nobs_t(2)
     integer                                                            :: nobs_tcp(2)
     integer                                                            :: nobs_tcx(2)
     integer                                                            :: nobs_tcy(2)
     integer                                                            :: nobs_tcz(2)
     integer                                                            :: nobs_uv(2)
     integer                                                            :: idate
     integer                                                            :: ntotal_obs
  end type diags_convinfo_struct  ! type diags_convinfo_struct
  type diags_obsinfo_struct
     character(len=20)                                                  :: x_type
     real(r_single)                                                     :: h_x_ensmean
     real(r_single)                                                     :: h_x_nobc
     real(r_single)                                                     :: s_elev
     real(r_single)                                                     :: x_err
     real(r_single)                                                     :: x_errorig
     real(r_single)                                                     :: x_hgt     
     real(r_single)                                                     :: x_lat
     real(r_single)                                                     :: x_lon
     real(r_single)                                                     :: x_obs
     real(r_single)                                                     :: x_press
     real(r_single)                                                     :: x_time
     integer                                                            :: x_code
  end type diags_obsinfo_struct   ! type diags_obsinfo_struct
  character(len=8),             dimension(:),               allocatable :: cdiagbuf
  logical                                                               :: initpass        = .true.
  real(r_single),               dimension(:,:),             allocatable :: rdiagbuf
  real(r_kind),   parameter                                             :: errorlimit      = 1._r_kind/sqrt(1.e9_r_kind)
  real(r_kind),   parameter                                             :: errorlimit2_obs = 1._r_kind/sqrt(1.e-6_r_kind)
  real(r_kind),   parameter                                             :: errorlimit2_bnd = 1.e3_r_kind*errorlimit2_obs
  real(r_single), parameter                                             :: diags_spval     = huge(1.0)
  integer,        parameter                                             :: diagsread_iunit = 7
  integer                                                               :: nchar
  integer                                                               :: nreal

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! diagsread_conv_count.f90

  ! DESCRIPTION:

  ! This subroutine parses a user specified GSI diag-formatted file
  ! containing conventional-type observations and determines the total
  ! number of observations read, the total number of observations kept
  ! (e.g., those passing the error threshold tests), and the total
  ! number of each conventional observation type.

  ! INPUT VARIABLES:

  ! * diags_convinfo; a FORTRAN diags_convinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * diags_convinfo; a FORTRAN diags_convinfo_struct variable
  !   containing the total number of observations read, the total
  !   number of observations kept (e.g., those passing the error
  !   threshold tests), and the total number of each conventional
  !   observation type.

  !-----------------------------------------------------------------------

  subroutine diagsread_conv_count(diags_convinfo)

    ! Define variables passed to routine

    type(diags_convinfo_struct)                                         :: diags_convinfo
    
    ! Define variables computed within routine

    character(len=3)                                                    :: obstype
    real(r_kind)                                                        :: error
    real(r_kind)                                                        :: errorlimit2
    real(r_kind)                                                        :: obsmax
    real(r_kind)                                                        :: pres
    integer                                                             :: nn(2)
    integer                                                             :: ii

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call init_convinfo(diags_convinfo)
    open(diagsread_iunit,file=trim(adjustl(diags_convinfo%filename)),      &
         & form='unformatted')

    ! Check local variable and proceed accordingly

    if(initpass) then

       ! Define local variables

       read(diagsread_iunit) diags_convinfo%idate
       initpass = .false.

    end if ! if(initpass)

    ! Define local variables

10  continue
    read(diagsread_iunit,err=20,end=30) obstype, nchar, nreal, ii
    errorlimit2 = errorlimit2_obs

    ! Allocate memory for local variables

    if(.not. allocated(cdiagbuf)) allocate(cdiagbuf(ii))
    if(.not. allocated(rdiagbuf)) allocate(rdiagbuf(nreal,ii))

    ! Define local variables

    read(diagsread_iunit) cdiagbuf(1:ii), rdiagbuf(:,1:ii)
    nn = 0

    ! Loop through local variable

    do i = 1, ii

       ! Check local variable and proceed accordingly

       if((trim(adjustl(obstype)) .eq. 'tcx') .or.                         &
            & (trim(adjustl(obstype)) .eq. 'tcy') .or.                     &
            & (trim(adjustl(obstype)) .eq. 'tcz')) then

          ! Define local variables

          error  = rdiagbuf(6,i)
          pres   = rdiagbuf(4,i)
          obsmax = abs(rdiagbuf(7,i))

       else   ! if((trim(adjustl(obstype)) .eq. 'tcx')
              ! .or. (trim(adjustl(obstype)) .eq. 'tcy')
              ! .or. (trim(adjustl(obstype)) .eq. 'tcz'))

          ! Check local variable and proceed accordingly

          if(rdiagbuf(12,i) .lt. 0.0) cycle
          if(trim(adjustl(obstype)) .eq. 'q') then

             ! Define local variables

             error  = rdiagbuf(20,i)*rdiagbuf(16,i)
             pres   = rdiagbuf(6,i)
             obsmax = abs(rdiagbuf(17,i)/rdiagbuf(20,i))

          else   ! if(trim(adjustl(obstype)) .eq. 'q')

             ! Check local variable and proceed accordingly

             if((trim(adjustl(obstype)) .eq. 'ps') .or.                    &
                  & (trim(adjustl(obstype)) .eq. 'tcp')) then

                ! Define local variables

                pres = rdiagbuf(17,i)

             else   ! if((trim(adjustl(obstype)) .eq. 'ps')
                    ! .or. (trim(adjustl(obstype)) .eq. 'tcp'))

                ! Define local variables

                pres = rdiagbuf(6,i)

             end if ! if((trim(adjustl(obstype)) .eq. 'ps')
                    ! .or. (trim(adjustl(obstype)) .eq. 'tcp'))

             ! Define local variables

             error  = rdiagbuf(16,i)
             obsmax = abs(rdiagbuf(17,i))

             ! Check local variable and proceed accordingly

             if(trim(adjustl(obstype)) .eq. 'uv') then

                ! Define local variables

                obsmax = max(obsmax,abs(rdiagbuf(20,i)))

             end if ! if(trim(adjustl(obstype)) .eq. 'uv')

          end if ! if(trim(adjustl(obstype)) .eq. 'q')

       end if ! if((trim(adjustl(obstype)) .eq. 'tcx')
              ! .or. (trim(adjustl(obstype)) .eq. 'tcy')
              ! .or. (trim(adjustl(obstype)) .eq. 'tcz'))
       
       ! Define local variables

       nn(1) = nn(1) + 1 
       
       ! Check local variable and proceed accordingly

       if((error .gt. errorlimit) .and. (error .lt. errorlimit2) .and.     &
            & (abs(obsmax) .le. 1.e9_r_kind) .and. (pres .ge.              &
            & 0.001_r_kind) .and. (pres .le. 1200._r_kind)) then

          ! Define local variables

          nn(2) = nn(2) + 1

       end if ! if((error .gt. errorlimit) .and. (error
              ! .lt. errorlimit2) .and. (abs(obsmax) .le. 1.e9_r_kind)
              ! .and. (pres .ge. 0.001_r_kind) .and. (pres
              ! .le. 1200._r_kind))

    end do ! do i = 1, ii

    ! Check local variable and proceed accordingly

    if(trim(adjustl(obstype)) .eq. 't') then

       ! Define local variables

       diags_convinfo%nobs_t     = diags_convinfo%nobs_t + nn
       diags_convinfo%ntotal_obs = diags_convinfo%ntotal_obs + nn(2)

    elseif(trim(adjustl(obstype)) .eq. 'uv') then

       ! Define local variables

       diags_convinfo%nobs_uv    = diags_convinfo%nobs_uv + 2*nn
       diags_convinfo%ntotal_obs = diags_convinfo%ntotal_obs + 2*nn(2)

    elseif(trim(adjustl(obstype)) .eq. 'ps') then

       ! Define local variables

       diags_convinfo%nobs_ps    = diags_convinfo%nobs_ps + nn
       diags_convinfo%ntotal_obs = diags_convinfo%ntotal_obs + nn(2)

    elseif(trim(adjustl(obstype)) .eq. 'q') then

       ! Define local variables

       diags_convinfo%nobs_q     = diags_convinfo%nobs_q + nn
       diags_convinfo%ntotal_obs = diags_convinfo%ntotal_obs + nn(2)

    elseif(trim(adjustl(obstype)) .eq. 'spd') then

       ! Define local variables

       diags_convinfo%nobs_spd   = diags_convinfo%nobs_spd + nn
       diags_convinfo%ntotal_obs = diags_convinfo%ntotal_obs + nn(2)

    elseif(trim(adjustl(obstype)) .eq. 'sst') then

       ! Define local variables

       diags_convinfo%nobs_sst   = diags_convinfo%nobs_sst + nn
       diags_convinfo%ntotal_obs = diags_convinfo%ntotal_obs + nn(2)

    elseif(trim(adjustl(obstype)) .eq. 'srw') then

       ! Define local variables

       diags_convinfo%nobs_srw   = diags_convinfo%nobs_srw + nn
       diags_convinfo%ntotal_obs = diags_convinfo%ntotal_obs + nn(2)

    elseif(trim(adjustl(obstype)) .eq. 'rw') then

       ! Define local variables

       diags_convinfo%nobs_rw    = diags_convinfo%nobs_rw + nn
       diags_convinfo%ntotal_obs = diags_convinfo%ntotal_obs + nn(2)

    elseif(trim(adjustl(obstype)) .eq. 'gps') then

       ! Define local variables

       diags_convinfo%nobs_gps   = diags_convinfo%nobs_gps + nn
       diags_convinfo%ntotal_obs = diags_convinfo%ntotal_obs + nn(2)

    elseif(trim(adjustl(obstype)) .eq. 'dw') then

       ! Define local variables

       diags_convinfo%nobs_dw    = diags_convinfo%nobs_dw + nn
       diags_convinfo%ntotal_obs = diags_convinfo%ntotal_obs + nn(2)

    elseif(trim(adjustl(obstype)) .eq. 'pw') then

       ! Define local variables

       diags_convinfo%nobs_pw    = diags_convinfo%nobs_pw + nn
       diags_convinfo%ntotal_obs = diags_convinfo%ntotal_obs + nn(2)

    elseif(trim(adjustl(obstype)) .eq. 'tcp') then

       ! Define local variables

       diags_convinfo%nobs_tcp   = diags_convinfo%nobs_tcp + nn
       diags_convinfo%ntotal_obs = diags_convinfo%ntotal_obs + nn(2)

    elseif(trim(adjustl(obstype)) .eq. 'tcx') then

       ! Define local variables

       diags_convinfo%nobs_tcx   = diags_convinfo%nobs_tcx + nn
       diags_convinfo%ntotal_obs = diags_convinfo%ntotal_obs + nn(2)

    elseif(trim(adjustl(obstype)) .eq. 'tcy') then

       ! Define local variables

       diags_convinfo%nobs_tcy   = diags_convinfo%nobs_tcy + nn
       diags_convinfo%ntotal_obs = diags_convinfo%ntotal_obs + nn(2)

    elseif(trim(adjustl(obstype)) .eq. 'tcz') then

       ! Define local variables

       diags_convinfo%nobs_tcz   = diags_convinfo%nobs_tcz + nn
       diags_convinfo%ntotal_obs = diags_convinfo%ntotal_obs + nn(2)

    else   ! if(trim(adjustl(obstype)) .eq. t)

       ! Define local variables

       write(6,500) trim(adjustl(obstype))

    end if ! if(trim(adjustl(obstype)) .eq. t)

    ! Deallocate memory for local variables

    if(allocated(cdiagbuf)) deallocate(cdiagbuf)
    if(allocated(rdiagbuf)) deallocate(rdiagbuf)

    ! Define local variables

    goto 10
20  continue
    write(6,501) trim(adjustl(diags_convinfo%filename))
    stop
30  continue

    ! Define local variables

    close(diagsread_iunit)
    initpass = .true.
500 format('WARNING: Unknown observation type ',a,'.')
501 format('ERROR: Unable to read file ',a,'. Aborting!!!')

    !=====================================================================

  end subroutine diagsread_conv_count

  !=======================================================================

  ! SUBROUTINE:

  ! diagsread_conv_data.f90

  ! DESCRIPTION:

  ! This subroutine reads the user specified GSI diags-formatted file
  ! and returns the observation diagnostic attributes (e.g., location,
  ! time, observations, etc.).

  ! INPUT VARIABLES:

  ! * diags_convinfo; a FORTRAN diags_convinfo_struct variable
  !   containing (at minimum) both the GSI diags-formatted file name
  !   path and the total number of observations within the respective
  !   file.

  ! * diags_obsinfo; an array of FORTRAN diags_obsinfo_struct
  !   variables of dimension diags_convinfo%ntotal_obs.

  ! OUTPUT VARIABLES:

  ! * diags_obsinfo; an array of FORTRAN diags_obsinfo_struct
  !   variables of dimension diags_convinfo%ntotal_obs containing the
  !   GSI diags-formatted file information for all relevant
  !   observation types.

  !-----------------------------------------------------------------------

  subroutine diagsread_conv_data(diags_convinfo,diags_obsinfo)

    ! Define variables passed to routine

    type(diags_convinfo_struct)                                         :: diags_convinfo
    type(diags_obsinfo_struct)                                          :: diags_obsinfo(diags_convinfo%ntotal_obs)

    ! Define variables computed within routine

    character(len=3)                                                    :: obstype
    logical                                                             :: qc_pass
    real(r_kind)                                                        :: errorlimit2
    integer                                                             :: nobs
    integer                                                             :: ii

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Loop through local variable

    do i = 1, diags_convinfo%ntotal_obs

       ! Define local variables

       call init_obsinfo(diags_obsinfo(i))

    end do ! do i = 1, diags_convinfo%ntotal_obs

    ! Define local variables

    open(diagsread_iunit,file=trim(adjustl(diags_convinfo%filename)),      &
         & form='unformatted')
    nobs = 0

    ! Check local variable and proceed accordingly

    if(initpass) then

       ! Define local variables

       read(diagsread_iunit) diags_convinfo%idate
       initpass = .false.

    end if ! if(initpass)

    ! Define local variables

10  continue
    read(diagsread_iunit,err=20,end=30) obstype, nchar, nreal, ii

    ! Allocate memory for local variables

    if(.not. allocated(cdiagbuf)) allocate(cdiagbuf(ii))
    if(.not. allocated(rdiagbuf)) allocate(rdiagbuf(nreal,ii))

    ! Define local variables

    read(diagsread_iunit) cdiagbuf(1:ii), rdiagbuf(:,1:ii)
    errorlimit2 = errorlimit2_obs

    ! Loop through local variable

    do i = 1, ii

       ! Define local variables

       call quality_control_check(nreal,rdiagbuf(1:nreal,i),errorlimit2,   &
            & qc_pass)
       if(.not. qc_pass) cycle

       ! Check local variable and proceed accordingly

       if(trim(adjustl(obstype)) .eq. 'dw') then

          ! Define local variables

          nobs = nobs + 1
          call read_diag_dw(nreal,cdiagbuf(i),rdiagbuf(1:nreal,i),         &
               & diags_obsinfo(nobs))

       end if ! if(trim(adjustl(obstype)) .eq. 'dw')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(obstype)) .eq. 'gps') then

          ! Define local variables

          nobs = nobs + 1
          call read_diag_gps(nreal,cdiagbuf(i),rdiagbuf(1:nreal,i),        &
               & diags_obsinfo(nobs))

       end if ! if(trim(adjustl(obstype)) .eq. 'gps')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(obstype)) .eq. 'ps') then

          ! Define local variables

          nobs = nobs + 1
          call read_diag_ps(nreal,cdiagbuf(i),rdiagbuf(1:nreal,i),         &
               & diags_obsinfo(nobs))

       end if ! if(trim(adjustl(obstype)) .eq. 'ps')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(obstype)) .eq. 'pw') then

          ! Define local variables

          nobs = nobs + 1
          call read_diag_pw(nreal,cdiagbuf(i),rdiagbuf(1:nreal,i),         &
               & diags_obsinfo(nobs))

       end if ! if(trim(adjustl(obstype)) .eq. 'pw')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(obstype)) .eq. 'q') then

          ! Define local variables

          nobs = nobs + 1
          call read_diag_q(nreal,cdiagbuf(i),rdiagbuf(1:nreal,i),          &
               & diags_obsinfo(nobs))

       end if ! if(trim(adjustl(obstype)) .eq. 'q')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(obstype)) .eq. 'rw') then

          ! Define local variables

          nobs = nobs + 1
          call read_diag_rw(nreal,cdiagbuf(i),rdiagbuf(1:nreal,i),         &
               & diags_obsinfo(nobs))

       end if ! if(trim(adjustl(obstype)) .eq. 'rw')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(obstype)) .eq. 'spd') then

          ! Define local variables

          nobs = nobs + 1
          call read_diag_spd(nreal,cdiagbuf(i),rdiagbuf(1:nreal,i),        &
               & diags_obsinfo(nobs))

       end if ! if(trim(adjustl(obstype)) .eq. 'spd')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(obstype)) .eq. 't') then

          ! Define local variables

          nobs = nobs + 1
          call read_diag_t(nreal,cdiagbuf(i),rdiagbuf(1:nreal,i),          &
               & diags_obsinfo(nobs))

       end if ! if(trim(adjustl(obstype)) .eq. 't')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(obstype)) .eq. 'tcp') then

          ! Define local variables

          nobs = nobs + 1
          call read_diag_tcp(nreal,cdiagbuf(i),rdiagbuf(1:nreal,i),        &
               & diags_obsinfo(nobs))

       end if ! if(trim(adjustl(obstype)) .eq. 'tcp')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(obstype)) .eq. 'uv') then

          ! Define local variables

          nobs = nobs + 1
          call read_diag_u(nreal,cdiagbuf(i),rdiagbuf(1:nreal,i),          &
               & diags_obsinfo(nobs))
          nobs = nobs + 1
          call read_diag_v(nreal,cdiagbuf(i),rdiagbuf(1:nreal,i),          &
               & diags_obsinfo(nobs))

       end if ! if(trim(adjustl(obstype)) .eq. 'uv')

    end do ! do i = 1, ii

    ! Deallocate memory for local variables

    if(allocated(cdiagbuf)) deallocate(cdiagbuf)
    if(allocated(rdiagbuf)) deallocate(rdiagbuf)

    ! Define local variables

    goto 10
20  continue
    write(6,501) trim(adjustl(diags_convinfo%filename))
    stop
30  continue

    ! Define local variables

    close(diagsread_iunit)
    initpass = .true.
500 format('WARNING: Unknown observation type ',a,'.')
501 format('ERROR: Unable to read file ',a,'. Aborting!!!')

    !=====================================================================

  end subroutine diagsread_conv_data

  !=======================================================================

  ! SUBROUTINE:

  ! init_convgrid.f90

  ! DESCRIPTION:

  ! This subroutine initializes all variables within a user specified
  ! FORTRAN diags_convinfo_struct variable.

  ! NOTE:

  ! The first element of each array is the total number of values
  ! read; the second element is the total number of values kept (e.g.,
  ! passing the observation error quality control).

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN diags_convinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTAN diags_convinfo_struct variable where all
  !   applicable variables within have been initialized.

  !-----------------------------------------------------------------------

  subroutine init_convinfo(grid)

    ! Define variables passed to routine

    type(diags_convinfo_struct)                                         :: grid

    !=====================================================================

    ! Define local variables

    grid%nobs_dw(:)  = 0
    grid%nobs_gps(:) = 0
    grid%nobs_ps(:)  = 0
    grid%nobs_pw(:)  = 0
    grid%nobs_q(:)   = 0
    grid%nobs_rw(:)  = 0
    grid%nobs_spd(:) = 0
    grid%nobs_srw(:) = 0
    grid%nobs_sst(:) = 0
    grid%nobs_t(:)   = 0
    grid%nobs_tcp(:) = 0
    grid%nobs_tcx(:) = 0
    grid%nobs_tcy(:) = 0
    grid%nobs_tcz(:) = 0
    grid%nobs_uv(:)  = 0
    grid%ntotal_obs  = 0

    !=====================================================================

  end subroutine init_convinfo

  !=======================================================================

  ! SUBROUTINE:

  ! init_obsinfo.f90

  ! DESCRIPTION:

  ! This subroutine initializes all variables within a user specified
  ! FORTRAN diags_obsinfo_struct variable.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN diags_obsinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTAN diags_obsinfo_struct variable where all
  !   applicable variables within have been initialized.

  !-----------------------------------------------------------------------

  subroutine init_obsinfo(grid)

    ! Define variables passed to routine

    type(diags_obsinfo_struct)                                         :: grid

    !=====================================================================

    ! Define local variables

    grid%x_type = 'XXX'
    grid%x_code = -999
    grid%x_time = diags_spval
    call set_missing_obsinfo(grid)

    !=====================================================================

  end subroutine init_obsinfo

  !=======================================================================

  ! SUBROUTINE:

  ! quality_control_check.f90

  ! DESCRIPTION:

  ! This subroutine returns a FORTRAN boolean value indicating whether
  ! a given GSI diag-formatted record passes the imposed quality
  ! control checks.

  ! INPUT VARIABLES:

  ! * nreal; a FORTRAN integer specifying the number of observation
  !   records. 

  ! * rdiagbuf; a FORTRAN real-valued array of dimension nreal
  !   containing the GSI diags-formatted file record observation
  !   statistics.

  ! * errorlimit2; a FORTRAN real-valued variable specifying the user
  !   imposed error threshold.

  ! OUTPUT VARIABLES:

  ! * qc_pass; a FORTRAN boolean variable containing specifying
  !   whether the GSI diag-formatted record passes the imposed quality
  !   control checks.

  !-----------------------------------------------------------------------

  subroutine quality_control_check(nreal,rdiagbuf,errorlimit2,qc_pass)

    ! Define variables passed to routine

    logical                                                             :: qc_pass
    integer                                                             :: nreal
    real(r_kind)                                                        :: rdiagbuf(nreal)
    real(r_kind)                                                        :: errorlimit2

    !=====================================================================

    ! Define local variables

    qc_pass = .true.

    ! Check local variable and proceed accordingly

    if((rdiagbuf(12) .lt. 0.0) .or. (rdiagbuf(16) .lt. errorlimit) .or.    &
         & (rdiagbuf(16) .gt. errorlimit2) .or. (abs(rdiagbuf(17)) .gt.    &
         & 1.e9_r_kind) .or. (rdiagbuf(6) .lt. 0.001_r_kind) .or.          &
         & (rdiagbuf(6) .gt. 1200._r_kind)) then

       ! Define local variables

       qc_pass = .false.

    end if ! if((rdiagbuf(12) .lt. 0.0) .or. (rdiagbuf(16)
           ! .lt. errorlimit) .or. (rdiagbuf(16) .gt. errorlimit2)
           ! .or. (abs(rdiagbuf(17)) .gt. 1.e9_r_kind)
           ! .or. (rdiagbuf(6) .lt. 0.001_r_kind) .or. (rdiagbuf(6)
           ! .gt. 1200._r_kind))

   !=====================================================================

  end subroutine quality_control_check

  !=======================================================================

  ! SUBROUTINE:

  ! read_diag_dw.f90

  ! DESCRIPTION:

  ! This subroutine defines the observation information contained in
  ! the GSI diags-formatted file record for Doppler wind observations.

  ! INPUT VARIABLES:

  ! * nreal; a FORTRAN integer specifying the number of observation
  !   records.

  ! * cdiagbuf; a FORTRAN character string containing the GSI
  !   diags-formatted file record containing the character string
  !   observation information.

  ! * rdiagbuf; a FORTRAN real-valued array of dimension nreal
  !   containing the GSI diags-formatted file record observation
  !   statistics.

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable
  !   containing the observation statistics; if the error threshold
  !   values are not met, all observation statistic values are set to
  !   the diags_spval value.

  !-----------------------------------------------------------------------

  subroutine read_diag_dw(nreal,cdiagbuf,rdiagbuf,diags_obsinfo)

    ! Define variables passed to routine

    integer                                                             :: nreal
    type(diags_obsinfo_struct)                                          :: diags_obsinfo
    character(len=8)                                                    :: cdiagbuf
    real(r_kind)                                                        :: rdiagbuf(nreal)

    !=====================================================================

    ! Define local variables

    diags_obsinfo%x_code      = rdiagbuf(1)
    diags_obsinfo%x_type      = 'dw'
    diags_obsinfo%x_time      = rdiagbuf(8)
    call set_missing_obsinfo(diags_obsinfo)
    diags_obsinfo%h_x_ensmean = rdiagbuf(18)
    diags_obsinfo%x_err       = (1.0/rdiagbuf(16))**2.0
    diags_obsinfo%x_hgt       = rdiagbuf(7)
    diags_obsinfo%x_lat       = rdiagbuf(3)
    diags_obsinfo%x_lon       = rdiagbuf(4)
    diags_obsinfo%x_obs       = rdiagbuf(17)
    diags_obsinfo%x_press     = rdiagbuf(6)

    ! Check local variable and proceed accordingly
    
    if(rdiagbuf(14) .gt. 1.e-5_r_kind) then
       
       ! Define local variables
       
       diags_obsinfo%x_errorig = (1.0/rdiagbuf(14))**2.0

    else   ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

       ! Define local variables

       diags_obsinfo%x_errorig = 1.e10_r_kind

    end if ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

    !=====================================================================
    
  end subroutine read_diag_dw

  !=======================================================================

  ! SUBROUTINE:

  ! read_diag_gps.f90

  ! DESCRIPTION:

  ! This subroutine defines the observation information contained in
  ! the GSI diags-formatted file record for global-positioning system
  ! (GPS) observations.

  ! INPUT VARIABLES:

  ! * nreal; a FORTRAN integer specifying the number of observation
  !   records.

  ! * cdiagbuf; a FORTRAN character string containing the GSI
  !   diags-formatted file record containing the character string
  !   observation information.

  ! * rdiagbuf; a FORTRAN real-valued array of dimension nreal
  !   containing the GSI diags-formatted file record observation
  !   statistics.

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable
  !   containing the observation statistics; if the error threshold
  !   values are not met, all observation statistic values are set to
  !   the diags_spval value.

  !-----------------------------------------------------------------------

  subroutine read_diag_gps(nreal,cdiagbuf,rdiagbuf,diags_obsinfo)

    ! Define variables passed to routine

    integer                                                             :: nreal
    type(diags_obsinfo_struct)                                          :: diags_obsinfo
    character(len=8)                                                    :: cdiagbuf
    real(r_kind)                                                        :: rdiagbuf(nreal)

    !=====================================================================

    ! Define local variables

    diags_obsinfo%x_code      = rdiagbuf(1)
    diags_obsinfo%x_type      = 'gps'
    diags_obsinfo%x_time      = rdiagbuf(8)
    call set_missing_obsinfo(diags_obsinfo)
    diags_obsinfo%h_x_ensmean = rdiagbuf(18)
    diags_obsinfo%x_err       = (1.0/rdiagbuf(16))**2.0
    diags_obsinfo%x_hgt       = rdiagbuf(7)
    diags_obsinfo%x_lat       = rdiagbuf(3)
    diags_obsinfo%x_lon       = rdiagbuf(4)
    diags_obsinfo%x_obs       = rdiagbuf(17)
    diags_obsinfo%x_press     = rdiagbuf(6)

    ! Check local variable and proceed accordingly
    
    if(rdiagbuf(14) .gt. 1.e-5_r_kind) then
       
       ! Define local variables
       
       diags_obsinfo%x_errorig = (1.0/rdiagbuf(14))**2.0

    else   ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

       ! Define local variables

       diags_obsinfo%x_errorig = 1.e10_r_kind

    end if ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

    !=====================================================================
    
  end subroutine read_diag_gps

  !=======================================================================

  ! SUBROUTINE:

  ! read_diag_ps.f90

  ! DESCRIPTION:

  ! This subroutine defines the observation information contained in
  ! the GSI diags-formatted file record for surface pressure
  ! observations.

  ! INPUT VARIABLES:

  ! * nreal; a FORTRAN integer specifying the number of observation
  !   records.

  ! * cdiagbuf; a FORTRAN character string containing the GSI
  !   diags-formatted file record containing the character string
  !   observation information.

  ! * rdiagbuf; a FORTRAN real-valued array of dimension nreal
  !   containing the GSI diags-formatted file record observation
  !   statistics.

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable
  !   containing the observation statistics; if the error threshold
  !   values are not met, all observation statistic values are set to
  !   the diags_spval value.

  !-----------------------------------------------------------------------

  subroutine read_diag_ps(nreal,cdiagbuf,rdiagbuf,diags_obsinfo)

    ! Define variables passed to routine

    integer                                                             :: nreal
    type(diags_obsinfo_struct)                                          :: diags_obsinfo
    character(len=8)                                                    :: cdiagbuf
    real(r_kind)                                                        :: rdiagbuf(nreal)

    !=====================================================================

    ! Define local variables
  
    diags_obsinfo%x_code      = rdiagbuf(1)
    diags_obsinfo%x_type      = 'ps'
    diags_obsinfo%x_time      = rdiagbuf(8)
    call set_missing_obsinfo(diags_obsinfo)
    diags_obsinfo%h_x_ensmean = rdiagbuf(18)
    diags_obsinfo%x_err       = (1.0/rdiagbuf(16))**2.0
    diags_obsinfo%x_hgt       = 0.0
    diags_obsinfo%x_lat       = rdiagbuf(3)
    diags_obsinfo%x_lon       = rdiagbuf(4)
    diags_obsinfo%x_obs       = rdiagbuf(17)
    diags_obsinfo%x_press     = rdiagbuf(17)

    ! Check local variable and proceed accordingly
    
    if(rdiagbuf(14) .gt. 1.e-5_r_kind) then
       
       ! Define local variables
       
       diags_obsinfo%x_errorig = (1.0/rdiagbuf(14))**2.0

    else   ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

       ! Define local variables

       diags_obsinfo%x_errorig = 1.e10_r_kind

    end if ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

    !=====================================================================
    
  end subroutine read_diag_ps

  !=======================================================================

  ! SUBROUTINE:

  ! read_diag_pw.f90

  ! DESCRIPTION:

  ! This subroutine defines the observation information contained in
  ! the GSI diags-formatted file record for precipital water
  ! observations.

  ! INPUT VARIABLES:

  ! * nreal; a FORTRAN integer specifying the number of observation
  !   records.

  ! * cdiagbuf; a FORTRAN character string containing the GSI
  !   diags-formatted file record containing the character string
  !   observation information.

  ! * rdiagbuf; a FORTRAN real-valued array of dimension nreal
  !   containing the GSI diags-formatted file record observation
  !   statistics.

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable
  !   containing the observation statistics; if the error threshold
  !   values are not met, all observation statistic values are set to
  !   the diags_spval value.

  !-----------------------------------------------------------------------

  subroutine read_diag_pw(nreal,cdiagbuf,rdiagbuf,diags_obsinfo)

    ! Define variables passed to routine

    integer                                                             :: nreal
    type(diags_obsinfo_struct)                                          :: diags_obsinfo
    character(len=8)                                                    :: cdiagbuf
    real(r_kind)                                                        :: rdiagbuf(nreal)

    !=====================================================================

    ! Define local variables
    
    diags_obsinfo%x_code      = rdiagbuf(1)
    diags_obsinfo%x_type      = 'pw'
    diags_obsinfo%x_time      = rdiagbuf(8)
    call set_missing_obsinfo(diags_obsinfo)
    diags_obsinfo%h_x_ensmean = rdiagbuf(18)
    diags_obsinfo%x_err       = (1.0/rdiagbuf(16))**2.0
    diags_obsinfo%x_hgt       = rdiagbuf(7)
    diags_obsinfo%x_lat       = rdiagbuf(3)
    diags_obsinfo%x_lon       = rdiagbuf(4)
    diags_obsinfo%x_obs       = rdiagbuf(17)
    diags_obsinfo%x_press     = rdiagbuf(6)

    ! Check local variable and proceed accordingly
    
    if(rdiagbuf(14) .gt. 1.e-5_r_kind) then
       
       ! Define local variables
       
       diags_obsinfo%x_errorig = (1.0/rdiagbuf(14))**2.0

    else   ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

       ! Define local variables

       diags_obsinfo%x_errorig = 1.e10_r_kind

    end if ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

    !=====================================================================
    
  end subroutine read_diag_pw

  !=======================================================================

  ! SUBROUTINE:

  ! read_diag_q.f90

  ! DESCRIPTION:

  ! This subroutine defines the observation information contained in
  ! the GSI diags-formatted file record for specific humidity
  ! observations.

  ! INPUT VARIABLES:

  ! * nreal; a FORTRAN integer specifying the number of observation
  !   records.

  ! * cdiagbuf; a FORTRAN character string containing the GSI
  !   diags-formatted file record containing the character string
  !   observation information.

  ! * rdiagbuf; a FORTRAN real-valued array of dimension nreal
  !   containing the GSI diags-formatted file record observation
  !   statistics.

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable
  !   containing the observation statistics; if the error threshold
  !   values are not met, all observation statistic values are set to
  !   the diags_spval value.

  !-----------------------------------------------------------------------

  subroutine read_diag_q(nreal,cdiagbuf,rdiagbuf,diags_obsinfo)

    ! Define variables passed to routine

    integer                                                             :: nreal
    type(diags_obsinfo_struct)                                          :: diags_obsinfo
    character(len=8)                                                    :: cdiagbuf
    real(r_kind)                                                        :: rdiagbuf(nreal)

    !=====================================================================

    ! Define local variables

    diags_obsinfo%x_code      = rdiagbuf(1)
    diags_obsinfo%x_type      = 'q'
    diags_obsinfo%x_time      = rdiagbuf(8)
    call set_missing_obsinfo(diags_obsinfo)
    diags_obsinfo%h_x_ensmean = (rdiagbuf(17) - rdiagbuf(18))/             &
         & rdiagbuf(20)
    diags_obsinfo%x_err       = (1.0/(rdiagbuf(20)*rdiagbuf(16)))**2.0
    diags_obsinfo%x_hgt       = rdiagbuf(7)
    diags_obsinfo%x_lat       = rdiagbuf(3)
    diags_obsinfo%x_lon       = rdiagbuf(4)
    diags_obsinfo%x_obs       = rdiagbuf(17)/rdiagbuf(20)
    diags_obsinfo%x_press     = rdiagbuf(6)

    ! Check local variable and proceed accordingly
    
    if(rdiagbuf(14) .gt. 1.e-5_r_kind) then
       
       ! Define local variables
       
       diags_obsinfo%x_errorig = (1.0/(rdiagbuf(20)*rdiagbuf(14)))**2.0

    else   ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

       ! Define local variables

       diags_obsinfo%x_errorig = 1.e10_r_kind

    end if ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

    !=====================================================================
    
  end subroutine read_diag_q

  !=======================================================================

  ! SUBROUTINE:

  ! read_diag_rw.f90

  ! DESCRIPTION:

  ! This subroutine defines the observation information contained in
  ! the GSI diags-formatted file record for radial wind observations.

  ! INPUT VARIABLES:

  ! * nreal; a FORTRAN integer specifying the number of observation
  !   records.

  ! * cdiagbuf; a FORTRAN character string containing the GSI
  !   diags-formatted file record containing the character string
  !   observation information.

  ! * rdiagbuf; a FORTRAN real-valued array of dimension nreal
  !   containing the GSI diags-formatted file record observation
  !   statistics.

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable
  !   containing the observation statistics; if the error threshold
  !   values are not met, all observation statistic values are set to
  !   the diags_spval value.

  !-----------------------------------------------------------------------

  subroutine read_diag_rw(nreal,cdiagbuf,rdiagbuf,diags_obsinfo)

    ! Define variables passed to routine

    integer                                                             :: nreal
    type(diags_obsinfo_struct)                                          :: diags_obsinfo
    character(len=8)                                                    :: cdiagbuf
    real(r_kind)                                                        :: rdiagbuf(nreal)

    !=====================================================================

    ! Define local variables
    
    diags_obsinfo%x_code      = rdiagbuf(1)
    diags_obsinfo%x_type      = 'rw'
    diags_obsinfo%x_time      = rdiagbuf(8)
    call set_missing_obsinfo(diags_obsinfo)
    diags_obsinfo%h_x_ensmean = rdiagbuf(18)
    diags_obsinfo%x_err       = (1.0/rdiagbuf(16))**2.0
    diags_obsinfo%x_hgt       = rdiagbuf(7)
    diags_obsinfo%x_lat       = rdiagbuf(3)
    diags_obsinfo%x_lon       = rdiagbuf(4)
    diags_obsinfo%x_obs       = rdiagbuf(17)
    diags_obsinfo%x_press     = rdiagbuf(6)

    ! Check local variable and proceed accordingly
    
    if(rdiagbuf(14) .gt. 1.e-5_r_kind) then
       
       ! Define local variables
       
       diags_obsinfo%x_errorig = (1.0/rdiagbuf(14))**2.0

    else   ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

       ! Define local variables

       diags_obsinfo%x_errorig = 1.e10_r_kind

    end if ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

    !=====================================================================
    
  end subroutine read_diag_rw

  !=======================================================================

  ! SUBROUTINE:

  ! read_diag_spd.f90

  ! DESCRIPTION:

  ! This subroutine defines the observation information contained in
  ! the GSI diags-formatted file record for wind speed observations.

  ! INPUT VARIABLES:

  ! * nreal; a FORTRAN integer specifying the number of observation
  !   records.

  ! * cdiagbuf; a FORTRAN character string containing the GSI
  !   diags-formatted file record containing the character string
  !   observation information.

  ! * rdiagbuf; a FORTRAN real-valued array of dimension nreal
  !   containing the GSI diags-formatted file record observation
  !   statistics.

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable
  !   containing the observation statistics; if the error threshold
  !   values are not met, all observation statistic values are set to
  !   the diags_spval value.

  !-----------------------------------------------------------------------

  subroutine read_diag_spd(nreal,cdiagbuf,rdiagbuf,diags_obsinfo)

    ! Define variables passed to routine

    integer                                                             :: nreal
    type(diags_obsinfo_struct)                                          :: diags_obsinfo
    character(len=8)                                                    :: cdiagbuf
    real(r_kind)                                                        :: rdiagbuf(nreal)

    !=====================================================================

    ! Define local variables

    diags_obsinfo%x_code      = rdiagbuf(1)
    diags_obsinfo%x_type      = 'spd'
    diags_obsinfo%x_time      = rdiagbuf(8)
    call set_missing_obsinfo(diags_obsinfo)
    diags_obsinfo%h_x_ensmean = rdiagbuf(18)
    diags_obsinfo%x_err       = (1.0/rdiagbuf(16))**2.0
    diags_obsinfo%x_hgt       = rdiagbuf(7)
    diags_obsinfo%x_lat       = rdiagbuf(3)
    diags_obsinfo%x_lon       = rdiagbuf(4)
    diags_obsinfo%x_obs       = rdiagbuf(17)
    diags_obsinfo%x_press     = rdiagbuf(6)
    
    ! Check local variable and proceed accordingly
    
    if(rdiagbuf(14) .gt. 1.e-5_r_kind) then
       
       ! Define local variables
       
       diags_obsinfo%x_errorig = (1.0/rdiagbuf(14))**2.0

    else   ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

       ! Define local variables

       diags_obsinfo%x_errorig = 1.e10_r_kind

    end if ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

    !=====================================================================
    
  end subroutine read_diag_spd

  !=======================================================================

  ! SUBROUTINE:

  ! read_diag_t.f90

  ! DESCRIPTION:

  ! This subroutine defines the observation information contained in
  ! the GSI diags-formatted file record for temperature observations.

  ! INPUT VARIABLES:

  ! * nreal; a FORTRAN integer specifying the number of observation
  !   records.

  ! * cdiagbuf; a FORTRAN character string containing the GSI
  !   diags-formatted file record containing the character string
  !   observation information.

  ! * rdiagbuf; a FORTRAN real-valued array of dimension nreal
  !   containing the GSI diags-formatted file record observation
  !   statistics.

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable
  !   containing the observation statistics; if the error threshold
  !   values are not met, all observation statistic values are set to
  !   the diags_spval value.

  !-----------------------------------------------------------------------

  subroutine read_diag_t(nreal,cdiagbuf,rdiagbuf,diags_obsinfo)

    ! Define variables passed to routine

    integer                                                             :: nreal
    type(diags_obsinfo_struct)                                          :: diags_obsinfo
    character(len=8)                                                    :: cdiagbuf
    real(r_kind)                                                        :: rdiagbuf(nreal)

    !=====================================================================

    ! Define local variables

    diags_obsinfo%x_code      = rdiagbuf(1)
    diags_obsinfo%x_type      = 't'
    diags_obsinfo%x_time      = rdiagbuf(8)
    call set_missing_obsinfo(diags_obsinfo)
    diags_obsinfo%h_x_ensmean = rdiagbuf(18) 
    diags_obsinfo%x_err       = (1.0/rdiagbuf(16))**2.0
    diags_obsinfo%x_hgt       = rdiagbuf(7)
    diags_obsinfo%x_lat       = rdiagbuf(3)
    diags_obsinfo%x_lon       = rdiagbuf(4)
    diags_obsinfo%x_obs       = rdiagbuf(17)
    diags_obsinfo%x_press     = rdiagbuf(6)

    ! Check local variable and proceed accordingly
    
    if(rdiagbuf(14) .gt. 1.e-5_r_kind) then
       
       ! Define local variables
       
       diags_obsinfo%x_errorig = (1.0/rdiagbuf(14))**2.0

    else   ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

       ! Define local variables

       diags_obsinfo%x_errorig = 1.e10_r_kind

    end if ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

    !=====================================================================
    
  end subroutine read_diag_t

  !=======================================================================

  ! SUBROUTINE:

  ! read_diag_tcp.f90

  ! DESCRIPTION:

  ! This subroutine defines the observation information contained in
  ! the GSI diags-formatted file record for tropical-cyclone (TC)
  ! central pressure (TCP) observations.

  ! INPUT VARIABLES:

  ! * nreal; a FORTRAN integer specifying the number of observation
  !   records.

  ! * cdiagbuf; a FORTRAN character string containing the GSI
  !   diags-formatted file record containing the character string
  !   observation information.

  ! * rdiagbuf; a FORTRAN real-valued array of dimension nreal
  !   containing the GSI diags-formatted file record observation
  !   statistics.

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable
  !   containing the observation statistics; if the error threshold
  !   values are not met, all observation statistic values are set to
  !   the diags_spval value.

  !-----------------------------------------------------------------------

  subroutine read_diag_tcp(nreal,cdiagbuf,rdiagbuf,diags_obsinfo)

    ! Define variables passed to routine

    integer                                                             :: nreal
    type(diags_obsinfo_struct)                                          :: diags_obsinfo
    character(len=8)                                                    :: cdiagbuf
    real(r_kind)                                                        :: rdiagbuf(nreal)

    !=====================================================================

    ! Define local variables

    diags_obsinfo%x_code      = rdiagbuf(1)
    diags_obsinfo%x_type      = 'ps'
    diags_obsinfo%x_time      = rdiagbuf(8)
    call set_missing_obsinfo(diags_obsinfo)
    diags_obsinfo%h_x_ensmean = rdiagbuf(18)
    diags_obsinfo%x_err       = (1.0/rdiagbuf(16))**2.0
    diags_obsinfo%x_hgt       = rdiagbuf(7)
    diags_obsinfo%x_lat       = rdiagbuf(3)
    diags_obsinfo%x_lon       = rdiagbuf(4)
    diags_obsinfo%x_obs       = rdiagbuf(17)
    diags_obsinfo%x_press     = rdiagbuf(17)

    ! Check local variable and proceed accordingly
    
    if(rdiagbuf(14) .gt. 1.e-5_r_kind) then
       
       ! Define local variables
       
       diags_obsinfo%x_errorig = (1.0/rdiagbuf(14))**2.0

    else   ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

       ! Define local variables

       diags_obsinfo%x_errorig = 1.e10_r_kind

    end if ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

    !=====================================================================
    
  end subroutine read_diag_tcp

  !=======================================================================

  ! SUBROUTINE:

  ! read_diag_u.f90

  ! DESCRIPTION:

  ! This subroutine defines the observation information contained in
  ! the GSI diags-formatted file record for u-wind observations.

  ! INPUT VARIABLES:

  ! * nreal; a FORTRAN integer specifying the number of observation
  !   records.

  ! * cdiagbuf; a FORTRAN character string containing the GSI
  !   diags-formatted file record containing the character string
  !   observation information.

  ! * rdiagbuf; a FORTRAN real-valued array of dimension nreal
  !   containing the GSI diags-formatted file record observation
  !   statistics.

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable
  !   containing the observation statistics; if the error threshold
  !   values are not met, all observation statistic values are set to
  !   the diags_spval value.

  !-----------------------------------------------------------------------

  subroutine read_diag_u(nreal,cdiagbuf,rdiagbuf,diags_obsinfo)

    ! Define variables passed to routine

    integer                                                             :: nreal
    type(diags_obsinfo_struct)                                          :: diags_obsinfo
    character(len=8)                                                    :: cdiagbuf
    real(r_kind)                                                        :: rdiagbuf(nreal)

    !=====================================================================

    ! Define local variables

    diags_obsinfo%x_code      = rdiagbuf(1)
    diags_obsinfo%x_time      = rdiagbuf(8)
    diags_obsinfo%x_type      = 'u'
    call set_missing_obsinfo(diags_obsinfo)
    diags_obsinfo%x_hgt       = rdiagbuf(7)
    diags_obsinfo%x_lat       = rdiagbuf(3)
    diags_obsinfo%x_lon       = rdiagbuf(4)
    diags_obsinfo%x_press     = rdiagbuf(6)
    diags_obsinfo%x_obs       = rdiagbuf(17)
    diags_obsinfo%x_err       = (1.0/rdiagbuf(16))**2.0
    diags_obsinfo%h_x_ensmean = rdiagbuf(18)

    ! Check local variable and proceed accordingly
    
    if(rdiagbuf(14) .gt. 1.e-5_r_kind) then
       
       ! Define local variables
       
       diags_obsinfo%x_errorig = (1.0/rdiagbuf(14))**2.0

    else   ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

       ! Define local variables

       diags_obsinfo%x_errorig = 1.e10_r_kind
       
    end if ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

    !=====================================================================

  end subroutine read_diag_u

  !=======================================================================

  ! SUBROUTINE:

  ! read_diag_v.f90

  ! DESCRIPTION:

  ! This subroutine defines the observation information contained in
  ! the GSI diags-formatted file record for v-wind observations.

  ! INPUT VARIABLES:

  ! * nreal; a FORTRAN integer specifying the number of observation
  !   records.

  ! * cdiagbuf; a FORTRAN character string containing the GSI
  !   diags-formatted file record containing the character string
  !   observation information.

  ! * rdiagbuf; a FORTRAN real-valued array of dimension nreal
  !   containing the GSI diags-formatted file record observation
  !   statistics.

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable
  !   containing the observation statistics; if the error threshold
  !   values are not met, all observation statistic values are set to
  !   the diags_spval value.

  !-----------------------------------------------------------------------

  subroutine read_diag_v(nreal,cdiagbuf,rdiagbuf,diags_obsinfo)

    ! Define variables passed to routine

    integer                                                             :: nreal
    type(diags_obsinfo_struct)                                          :: diags_obsinfo
    character(len=8)                                                    :: cdiagbuf
    real(r_kind)                                                        :: rdiagbuf(nreal)

    !=====================================================================

    ! Define local variables

    diags_obsinfo%x_code      = rdiagbuf(1)
    diags_obsinfo%x_time      = rdiagbuf(8)
    diags_obsinfo%x_type      = 'v'
    call set_missing_obsinfo(diags_obsinfo)
    diags_obsinfo%x_hgt       = rdiagbuf(7)
    diags_obsinfo%x_lat       = rdiagbuf(3)
    diags_obsinfo%x_lon       = rdiagbuf(4)
    diags_obsinfo%x_press     = rdiagbuf(6)
    diags_obsinfo%x_obs       = rdiagbuf(20)
    diags_obsinfo%x_err       = (1.0/rdiagbuf(16))**2.0
    diags_obsinfo%h_x_ensmean = rdiagbuf(21)

    ! Check local variable and proceed accordingly
    
    if(rdiagbuf(14) .gt. 1.e-5_r_kind) then
       
       ! Define local variables
       
       diags_obsinfo%x_errorig = (1.0/rdiagbuf(14))**2.0
       
    else   ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)
       
       ! Define local variables
       
       diags_obsinfo%x_errorig = 1.e10_r_kind
       
    end if ! if(rdiagbuf(14) .gt. 1.e-5_r_kind)

    !=====================================================================

  end subroutine read_diag_v

  !=======================================================================

  ! SUBROUTINE:

  ! set_missing_obsinfo.f90

  ! DESCRIPTION:

  ! This subroutine sets all observation statistic values within the
  ! FORTRAN diags_obsinfo_struct variable to the diags_spval value.

  ! INPUT VARIABLES:

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * diags_obsinfo; a FORTRAN diags_obsinfo_struct variable with all
  !   observation statistic values set to the diags_spval value.

  !-----------------------------------------------------------------------

  subroutine set_missing_obsinfo(diags_obsinfo)

    ! Define variables passed to routine

    type(diags_obsinfo_struct)                                          :: diags_obsinfo

    !=====================================================================

    ! Define local variables

    diags_obsinfo%h_x_ensmean = diags_spval
    diags_obsinfo%h_x_nobc    = diags_spval
    diags_obsinfo%x_err       = diags_spval
    diags_obsinfo%x_errorig   = diags_spval
    diags_obsinfo%x_hgt       = diags_spval
    diags_obsinfo%x_lat       = diags_spval
    diags_obsinfo%x_lon       = diags_spval
    diags_obsinfo%x_obs       = diags_spval
    diags_obsinfo%x_press     = diags_spval

    !=====================================================================

  end subroutine set_missing_obsinfo

  !=======================================================================

end module diagsread_interface
