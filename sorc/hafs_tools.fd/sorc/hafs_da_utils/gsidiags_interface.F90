module gsidiags_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! da-utils :: gsidiags_interface
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

  use diagsread_interface
  use kinds_interface
  use math_methods_interface
  use namelist_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: gsidiags_convobs
  interface gsidiags_convobs
     module procedure convisoobs
     module procedure convprfobs
  end interface gsidiags_convobs

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! bin_diags_plevs.f90

  ! DESCRIPTION:

  ! This subroutine bins the GSI diags-formatted values for a user
  ! specified observation type in accordance with the assigned
  ! pressure level to the interval with the user defined pressure
  ! levels.

  ! INPUT VARIABLES:

  ! * obsinfo; a FORTRAN diags_obsinfo_struct array variable with a
  !   dimension corresponding to the total number of observation
  !   diagnostics ingested from the GSI diags-formatted file.

  ! * gsidiagprfvar; a FORTRAN gsidiagprfvar_struct variable.

  ! OUTPUT VARIABLES:

  ! * gsidiagprfvar; a FORTRAN gsidiagprfvar_struct variable
  !   containing the GSI diags-formatted observation values (from
  !   obsinfo) bin in accordance with the user specified pressure
  !   levels; empty variable values are denoted by a missing datum
  !   value (e.g., spval).

  !-----------------------------------------------------------------------

  subroutine bin_diags_plevs(obsinfo,gsidiagprfvar)

    ! Define variables passed to routine

    type(diags_obsinfo_struct)                                          :: obsinfo(:)
    type(gsidiagprfvar_struct)                                          :: gsidiagprfvar
    
    ! Define variables computed within routine

    real(r_kind)                                                        :: pmax
    real(r_kind)                                                        :: pmin
    integer                                                             :: nobs

    ! Define counting variables

    integer                                                             :: i, j 

    !=====================================================================

    ! Loop through local variable

    do i = 1, size(obsinfo)

       ! Check local variable and proceed accordingly

       if(obsinfo(i)%x_lon .lt. 0.0) obsinfo(i)%x_lon = obsinfo(i)%x_lon   &
            & + 360.0

    end do ! do i = 1, size(obsinfo)
       
    ! Loop through local variable

    do j = 1, (gsidiagprfvar%nlevs - 1)

       ! Define local variables

       pmax = gsidiagprfvar%plevs(j)
       pmin = gsidiagprfvar%plevs(j+1)
       nobs = 0

       ! Loop through local variable

       do i = 1, size(obsinfo)
          
          ! Check local variable and proceed accordingly

          if((obsinfo(i)%x_press .lt. pmax) .and. (obsinfo(i)%x_press      &
               & .ge. pmin) .and. (trim(adjustl(obsinfo(i)%x_type))        &
               & .eq. trim(adjustl(gsidiagprfvar%obstype)))) then

             ! Check local variable and proceed accordingly

             if(gsidiagprfvar%obscode .eq. obscode_spval) then
                
                ! Define local variables

                nobs                       = nobs + 1
                gsidiagprfvar%hgt(nobs,j)  = obsinfo(i)%x_hgt
                gsidiagprfvar%lat(nobs,j)  = obsinfo(i)%x_lat
                gsidiagprfvar%lon(nobs,j)  = obsinfo(i)%x_lon
                gsidiagprfvar%obs(nobs,j)  = obsinfo(i)%x_obs
                gsidiagprfvar%omf(nobs,j)  = obsinfo(i)%h_x_ensmean
                gsidiagprfvar%pres(nobs,j) = obsinfo(i)%x_press
                gsidiagprfvar%time(nobs,j) = obsinfo(i)%x_time
                gsidiagprfvar%code(nobs,j) = obsinfo(i)%x_code
                gsidiagprfvar%ncount(j)    = nobs

             end if ! if(gsidiagprfvar%obscode .eq. obscode_spval)
                
             ! Check local variable and proceed accordingly
             
             if(gsidiagprfvar%obscode .ne. obscode_spval) then

                ! Check local variable and proceed accordingly

                if(obsinfo(i)%x_code .eq. gsidiagprfvar%obscode) then
                   
                   ! Define local variables
                   
                   nobs                       = nobs + 1
                   gsidiagprfvar%hgt(nobs,j)  = obsinfo(i)%x_hgt
                   gsidiagprfvar%lat(nobs,j)  = obsinfo(i)%x_lat
                   gsidiagprfvar%lon(nobs,j)  = obsinfo(i)%x_lon
                   gsidiagprfvar%obs(nobs,j)  = obsinfo(i)%x_obs
                   gsidiagprfvar%omf(nobs,j)  = obsinfo(i)%h_x_ensmean
                   gsidiagprfvar%pres(nobs,j) = obsinfo(i)%x_press
                   gsidiagprfvar%time(nobs,j) = obsinfo(i)%x_time
                   gsidiagprfvar%code(nobs,j) = obsinfo(i)%x_code
                   gsidiagprfvar%ncount(j)    = nobs

                end if ! if(obsinfo(i)%x_code
                       ! .eq. gsidiagprfvar%obscode)
                   
             end if ! if(gsidiagprfvar%obscode .ne. obscode_spval)
             
          end if ! if((obsinfo(i)%x_press .lt. pmax)
                 ! .and. (obsinfo(i)%x_press .ge. pmin)
                 ! .and. (trim(adjustl(obsinfo(i)%x_type))
                 ! .eq. trim(adjustl(gsidiagprfvar%obstype))
                 ! .and. (obsinfo(i)%x_code
                 ! .eq. gsidiagprfvar%obscode))
   
       end do ! do i = 1, size(obsinfo)

    end do ! do j = 1, (gsidiagprfvar%nlevs - 1)

    !=====================================================================

  end subroutine bin_diags_plevs

  !=======================================================================

  ! SUBROUTINE:

  ! bin_diags_plevs_regional.f90

  ! DESCRIPTION:

  ! This subroutine bins the GSI diags-formatted values for a user
  ! specified observation type in accordance with the assigned
  ! pressure level to the interval with the user defined pressure
  ! levels and also within a region specified by the user.

  ! INPUT VARIABLES:

  ! * obsinfo; a FORTRAN diags_obsinfo_struct array variable with a
  !   dimension corresponding to the total number of observation
  !   diagnostics ingested from the GSI diags-formatted file.

  ! * gsidiagprfvar; a FORTRAN gsidiagprfvar_struct variable.

  ! OUTPUT VARIABLES:

  ! * gsidiagprfvar; a FORTRAN gsidiagprfvar_struct variable
  !   containing the GSI diags-formatted observation values (from
  !   obsinfo) bin in accordance with the user specified pressure
  !   levels; empty variable values are denoted by a missing datum
  !   value (e.g., spval).

  !-----------------------------------------------------------------------

  subroutine bin_diags_plevs_regional(obsinfo,gsidiagprfvar)

    ! Define variables passed to routine

    type(diags_obsinfo_struct)                                          :: obsinfo(:)
    type(gsidiagprfvar_struct)                                          :: gsidiagprfvar
    
    ! Define variables computed within routine

    real(r_kind)                                                        :: pmax
    real(r_kind)                                                        :: pmin
    integer                                                             :: nobs

    ! Define counting variables

    integer                                                             :: i, j 

    !=====================================================================

    ! Loop through local variable

    do i = 1, size(obsinfo)

       ! Check local variable and proceed accordingly

       if(obsinfo(i)%x_lon .lt. 0.0) obsinfo(i)%x_lon = obsinfo(i)%x_lon   &
            & + 360.0

    end do ! do i = 1, size(obsinfo)
    
    ! Loop through local variable

    do j = 1, (gsidiagprfvar%nlevs - 1)

       ! Define local variables

       pmax = gsidiagprfvar%plevs(j)
       pmin = gsidiagprfvar%plevs(j+1)
       nobs = 0

       ! Loop through local variable

       do i = 1, size(obsinfo)

          ! Check local variable and proceed accordingly

          if((obsinfo(i)%x_press .lt. pmax) .and. (obsinfo(i)%x_press      &
               & .ge. pmin) .and. (trim(adjustl(obsinfo(i)%x_type))        &
               & .eq. trim(adjustl(gsidiagprfvar%obstype))) .and.          &
               & (obsinfo(i)%x_lat .ge. gsidiags_regional_latmin) .and.    &
               & (obsinfo(i)%x_lat .le. gsidiags_regional_latmax) .and.    &
               & (obsinfo(i)%x_lon .ge. gsidiags_regional_lonmin) .and.    &
               & (obsinfo(i)%x_lon .le. gsidiags_regional_lonmax)) then

             ! Check local variable and proceed accordingly

             if(gsidiagprfvar%obscode .eq. obscode_spval) then
                
                ! Define local variables

                nobs                       = nobs + 1
                gsidiagprfvar%hgt(nobs,j)  = obsinfo(i)%x_hgt
                gsidiagprfvar%lat(nobs,j)  = obsinfo(i)%x_lat
                gsidiagprfvar%lon(nobs,j)  = obsinfo(i)%x_lon
                gsidiagprfvar%obs(nobs,j)  = obsinfo(i)%x_obs
                gsidiagprfvar%omf(nobs,j)  = obsinfo(i)%h_x_ensmean
                gsidiagprfvar%pres(nobs,j) = obsinfo(i)%x_press
                gsidiagprfvar%time(nobs,j) = obsinfo(i)%x_time
                gsidiagprfvar%code(nobs,j) = obsinfo(i)%x_code
                gsidiagprfvar%ncount(j)    = nobs

             end if ! if(gsidiagprfvar%obscode .eq. obscode_spval)
                
             ! Check local variable and proceed accordingly
             
             if(gsidiagprfvar%obscode .ne. obscode_spval) then
                
                ! Check local variable and proceed accordingly

                if(obsinfo(i)%x_code .eq. gsidiagprfvar%obscode) then
             
                   ! Define local variables
                   
                   nobs                       = nobs + 1
                   gsidiagprfvar%hgt(nobs,j)  = obsinfo(i)%x_hgt
                   gsidiagprfvar%lat(nobs,j)  = obsinfo(i)%x_lat
                   gsidiagprfvar%lon(nobs,j)  = obsinfo(i)%x_lon
                   gsidiagprfvar%obs(nobs,j)  = obsinfo(i)%x_obs
                   gsidiagprfvar%omf(nobs,j)  = obsinfo(i)%h_x_ensmean
                   gsidiagprfvar%pres(nobs,j) = obsinfo(i)%x_press
                   gsidiagprfvar%time(nobs,j) = obsinfo(i)%x_time
                   gsidiagprfvar%code(nobs,j) = obsinfo(i)%x_code
                   gsidiagprfvar%ncount(j)    = nobs

                end if ! if(obsinfo(i)%x_code
                       ! .eq. gsidiagprfvar%obscode)
                   
             end if ! if(gsidiagprfvar%obscode .ne. obscode_spval)
             
          end if ! if((obsinfo(i)%x_press .lt. pmax)
                 ! .and. (obsinfo(i)%x_press .ge. pmin)
                 ! .and. (trim(adjustl(obsinfo(i)%x_type))
                 ! .eq. trim(adjustl(gsidiagprfvar%obstype)))
                 ! .and. (obsinfo(i)%x_lat
                 ! .ge. gsidiags_regional_latmin)
                 ! .and. (obsinfo(i)%x_lat
                 ! .le. gsidiags_regional_latmax)
                 ! .and. (obsinfo(i)%x_lon
                 ! .ge. gsidiags_regional_lonmin)
                 ! .and. (obsinfo(i)%x_lon
                 ! .le. gsidiags_regional_lonmax))
   
       end do ! do i = 1, size(obsinfo)

    end do ! do j = 1, (gsidiagprfvar%nlevs - 1)

    !=====================================================================

  end subroutine bin_diags_plevs_regional

  !=======================================================================

  ! SUBROUTINE:

  ! convisoobs.f90

  ! DESCRIPTION:

  ! This subroutine ingests a user specified iso-level variable from
  ! the user specified GSI diags-formatted filename path and returns a
  ! FORTRAN gsidiagisovar_struct variable containing the iso-level
  ! variable attributes and statistics.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the GSI diags-formatted file.

  ! * gsidiagisovar; a FORTRAN gsidiagisovar_struct variable.

  ! OUTPUT VARIABLES:

  ! * gsidiagisovar; a FORTRAN gsidiagisovar_struct variable
  !   containing the iso-level variable attributes and statistics.

  !-----------------------------------------------------------------------

  subroutine convisoobs(filename,gsidiagisovar)

    ! Define variables passed to routine

    type(gsidiagisovar_struct)                                          :: gsidiagisovar
    character(len=500)                                                  :: filename

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(is_gsidiags_regional) then

       ! Compute local variables

       call convisoobs_regional(filename,gsidiagisovar)

    else   ! if(is_gsidiags_regional)

       ! Compute local variables

       call convisoobs_global(filename,gsidiagisovar)

    end if ! if(is_gsidiags_regional)
       
    !=====================================================================

  end subroutine convisoobs

  !=======================================================================

  ! SUBROUTINE:

  ! convisoobs_global.f90

  ! DESCRIPTION:

  ! This subroutine ingests a user specified iso-level variable from
  ! the user specified GSI diags-formatted filename path and returns a
  ! FORTRAN gsidiagisovar_struct variable containing the iso-level
  ! variable attributes and statistics.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the GSI diags-formatted file.

  ! * gsidiagisovar; a FORTRAN gsidiagisovar_struct variable.

  ! * obscode; a FORTRAN integer value specifying the observation
  !   code.

  ! OUTPUT VARIABLES:

  ! * gsidiagisovar; a FORTRAN gsidiagisovar_struct variable
  !   containing the iso-level variable attributes and statistics.

  !-----------------------------------------------------------------------

  subroutine convisoobs_global(filename,gsidiagisovar)

    ! Define variables passed to routine

    type(gsidiagisovar_struct)                                          :: gsidiagisovar
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(diags_obsinfo_struct), dimension(:),               allocatable :: obsinfo
    type(diags_convinfo_struct)                                         :: convinfo
    type(statgrid_struct)                                               :: statgrid
    type(vargrid_struct)                                                :: vargrid
    integer                                                             :: nobs

    ! Define counting variables      

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    convinfo%filename = filename
    call diagsread_conv_count(convinfo)
    write(gsidiagisovar%analdate,'(i10)') convinfo%idate

    ! Check local variable and proceed accordingly

    if(trim(adjustl(gsidiagisovar%obstype)) .eq. 'ps')                     &
         & gsidiagisovar%nobs = convinfo%nobs_ps(2) +                      &
         & convinfo%nobs_tcp(2)

    ! Allocate memory for local variables

    call variable_interface_setup_struct(gsidiagisovar)
    if(.not. allocated(obsinfo)) allocate(obsinfo(convinfo%ntotal_obs))

    ! Define local variables

    call diagsread_conv_data(convinfo,obsinfo)
    nobs = 0

    ! Loop through local variable

    do i = 1, size(obsinfo)

       ! Check local variable and proceed accordingly

       if(obsinfo(i)%x_lon .lt. 0.0) obsinfo(i)%x_lon = obsinfo(i)%x_lon   &
            & + 360.0
       
       ! Check local variable and proceed accordingly

       if((trim(adjustl(obsinfo(i)%x_type)) .eq.                           &
            & trim(adjustl(gsidiagisovar%obstype)))) then

          ! Check local variable and proceed accordingly
       
          if(gsidiagisovar%obscode .eq. obscode_spval) then
             
             ! Define local variables
             
             nobs                     = nobs + 1
             gsidiagisovar%hgt(nobs)  = obsinfo(i)%x_hgt
             gsidiagisovar%lat(nobs)  = obsinfo(i)%x_lat
             gsidiagisovar%lon(nobs)  = obsinfo(i)%x_lon
             gsidiagisovar%obs(nobs)  = obsinfo(i)%x_obs
             gsidiagisovar%omf(nobs)  = obsinfo(i)%h_x_ensmean
             gsidiagisovar%pres(nobs) = obsinfo(i)%x_press
             gsidiagisovar%time(nobs) = obsinfo(i)%x_time
             gsidiagisovar%code(nobs) = obsinfo(i)%x_code

          end if ! if(gsidiagisovar%obscode .eq. obscode_spval)
             
          ! Check local variable and proceed accordingly
          
          if(gsidiagisovar%obscode .ne. obscode_spval) then
             
             ! Check local variable and proceed accordingly
             
             if(obsinfo(i)%x_code .eq. gsidiagisovar%obscode) then
                
                ! Define local variables
                
                nobs                     = nobs + 1
                gsidiagisovar%hgt(nobs)  = obsinfo(i)%x_hgt
                gsidiagisovar%lat(nobs)  = obsinfo(i)%x_lat
                gsidiagisovar%lon(nobs)  = obsinfo(i)%x_lon
                gsidiagisovar%obs(nobs)  = obsinfo(i)%x_obs
                gsidiagisovar%omf(nobs)  = obsinfo(i)%h_x_ensmean
                gsidiagisovar%pres(nobs) = obsinfo(i)%x_press
                gsidiagisovar%time(nobs) = obsinfo(i)%x_time
                gsidiagisovar%code(nobs) = obsinfo(i)%x_code
                
             end if ! if(obsinfo(i)%x_code .eq. gsidiagisovar%obscode)
             
          end if ! if(gsidiagisovar%obscode .ne. obscode_spval)
                
       end if ! if((trim(adjustl(obsinfo(i)%x_type))
              ! .eq. trim(adjustl(gsidiagisovar%obstype)))
                    
    end do ! do i = 1, size(obsinfo)

    ! Define local variables

    vargrid%nvals = size(gsidiagisovar%omf)
    call variable_interface_setup_struct(vargrid)
    vargrid%var   = gsidiagisovar%omf

    ! Compute local variables

    call math_methods_stats(vargrid,statgrid)

    ! Define local variables

    gsidiagisovar%bias   = statgrid%bias
    gsidiagisovar%mean   = statgrid%mean
    gsidiagisovar%rmse   = statgrid%rmse
    gsidiagisovar%vari   = statgrid%vari
    gsidiagisovar%ncount = statgrid%nvals

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(vargrid)

    ! Deallocate memory for local variables

    if(allocated(obsinfo)) deallocate(obsinfo)

    !=====================================================================

  end subroutine convisoobs_global

  !=======================================================================

  ! SUBROUTINE:

  ! convisoobs_regional.f90

  ! DESCRIPTION:

  ! This subroutine ingests a user specified iso-level variable from
  ! the user specified GSI diags-formatted filename path and returns a
  ! FORTRAN gsidiagisovar_struct variable containing the iso-level
  ! variable attributes and statistics.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the GSI diags-formatted file.

  ! * gsidiagisovar; a FORTRAN gsidiagisovar_struct variable.

  ! * obscode; a FORTRAN integer value specifying the observation
  !   code.

  ! OUTPUT VARIABLES:

  ! * gsidiagisovar; a FORTRAN gsidiagisovar_struct variable
  !   containing the iso-level variable attributes and statistics.

  !-----------------------------------------------------------------------

  subroutine convisoobs_regional(filename,gsidiagisovar)

    ! Define variables passed to routine

    type(gsidiagisovar_struct)                                          :: gsidiagisovar
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(diags_obsinfo_struct), dimension(:),               allocatable :: obsinfo
    type(diags_convinfo_struct)                                         :: convinfo
    type(statgrid_struct)                                               :: statgrid
    type(vargrid_struct)                                                :: vargrid
    integer                                                             :: nobs

    ! Define counting variables      

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    convinfo%filename = filename
    call diagsread_conv_count(convinfo)
    write(gsidiagisovar%analdate,'(i10)') convinfo%idate

    ! Check local variable and proceed accordingly

    if(trim(adjustl(gsidiagisovar%obstype)) .eq. 'ps')                     &
         & gsidiagisovar%nobs = convinfo%nobs_ps(2) +                      &
         & convinfo%nobs_tcp(2)

    ! Allocate memory for local variables

    call variable_interface_setup_struct(gsidiagisovar)
    if(.not. allocated(obsinfo)) allocate(obsinfo(convinfo%ntotal_obs))

    ! Define local variables

    call diagsread_conv_data(convinfo,obsinfo)
    nobs = 0

    ! Loop through local variable

    do i = 1, size(obsinfo)

       ! Check local variable and proceed accordingly

       if(obsinfo(i)%x_lon .lt. 0.0) obsinfo(i)%x_lon = obsinfo(i)%x_lon   &
            & + 360.0
       
       ! Check local variable and proceed accordingly

       if((trim(adjustl(obsinfo(i)%x_type)) .eq.                           &
            & trim(adjustl(gsidiagisovar%obstype))) .and.                  &
            & (obsinfo(i)%x_lat .ge. gsidiags_regional_latmin) .and.       &
            & (obsinfo(i)%x_lat .le. gsidiags_regional_latmax) .and.       &
            & (obsinfo(i)%x_lon .ge. gsidiags_regional_lonmin) .and.       &
            & (obsinfo(i)%x_lon .le. gsidiags_regional_lonmax)) then

          ! Check local variable and proceed accordingly
       
          if(gsidiagisovar%obscode .eq. obscode_spval) then
             
             ! Define local variables
             
             nobs                     = nobs + 1
             gsidiagisovar%hgt(nobs)  = obsinfo(i)%x_hgt
             gsidiagisovar%lat(nobs)  = obsinfo(i)%x_lat
             gsidiagisovar%lon(nobs)  = obsinfo(i)%x_lon
             gsidiagisovar%obs(nobs)  = obsinfo(i)%x_obs
             gsidiagisovar%omf(nobs)  = obsinfo(i)%h_x_ensmean
             gsidiagisovar%pres(nobs) = obsinfo(i)%x_press
             gsidiagisovar%time(nobs) = obsinfo(i)%x_time
             gsidiagisovar%code(nobs) = obsinfo(i)%x_code

          end if ! if(gsidiagisovar%obscode .eq. obscode_spval)
             
          ! Check local variable and proceed accordingly
          
          if(gsidiagisovar%obscode .ne. obscode_spval) then
             
             ! Check local variable and proceed accordingly
             
             if(obsinfo(i)%x_code .eq. gsidiagisovar%obscode) then
                
                ! Define local variables
                
                nobs                     = nobs + 1
                gsidiagisovar%hgt(nobs)  = obsinfo(i)%x_hgt
                gsidiagisovar%lat(nobs)  = obsinfo(i)%x_lat
                gsidiagisovar%lon(nobs)  = obsinfo(i)%x_lon
                gsidiagisovar%obs(nobs)  = obsinfo(i)%x_obs
                gsidiagisovar%omf(nobs)  = obsinfo(i)%h_x_ensmean
                gsidiagisovar%pres(nobs) = obsinfo(i)%x_press
                gsidiagisovar%time(nobs) = obsinfo(i)%x_time
                gsidiagisovar%code(nobs) = obsinfo(i)%x_code
                
             end if ! if(obsinfo(i)%x_code .eq. gsidiagisovar%obscode)
             
          end if ! if(gsidiagisovar%obscode .ne. obscode_spval)
                
       end if ! if((trim(adjustl(obsinfo(i)%x_type))
              ! .eq. trim(adjustl(gsidiagisovar%obstype)))
              ! .and. (obsinfo(i)%x_lat .ge. gsidiags_regional_latmin)
              ! .and. (obsinfo(i)%x_lat .le. gsidiags_regional_latmax)
              ! .and. (obsinfo(i)%x_lon .ge. gsidiags_regional_lonmin)
              ! .and. (obsinfo(i)%x_lon
              ! .le. gsidiags_regional_lonmax))
                    
    end do ! do i = 1, size(obsinfo)

    ! Define local variables

    vargrid%nvals = size(gsidiagisovar%omf)
    call variable_interface_setup_struct(vargrid)
    vargrid%var   = gsidiagisovar%omf

    ! Compute local variables

    call math_methods_stats(vargrid,statgrid)

    ! Define local variables

    gsidiagisovar%bias   = statgrid%bias
    gsidiagisovar%mean   = statgrid%mean
    gsidiagisovar%rmse   = statgrid%rmse
    gsidiagisovar%vari   = statgrid%vari
    gsidiagisovar%ncount = statgrid%nvals

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(vargrid)

    ! Deallocate memory for local variables

    if(allocated(obsinfo)) deallocate(obsinfo)

    !=====================================================================

  end subroutine convisoobs_regional
  
  !=======================================================================

  ! SUBROUTINE:

  ! convprfobs.f90

  ! DESCRIPTION:

  ! This subroutine ingests a user specified profile-type variable
  ! from the user specified GSI diags-formatted filename path and
  ! returns a FORTRAN gsidiagprfvar_struct variable containing the
  ! profile variable attributes and statistics.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the GSI diags-formatted file.

  ! * gsidiagprfvar; a FORTRAN gsidiagprfvar_struct variable.

  ! OPTIONAL INPUT VARIABLES:

  ! * obscode; a FORTRAN integer value specifying the observation
  !   code.

  ! OUTPUT VARIABLES:

  ! * gsidiagprfvar; a FORTRAN gsidiagisovar_struct variable
  !   containing the profile variable attributes and statistics.

  !-----------------------------------------------------------------------

  subroutine convprfobs(filename,gsidiagprfvar)

    ! Define variables passed to routine

    type(gsidiagprfvar_struct)                                          :: gsidiagprfvar
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    type(diags_obsinfo_struct), dimension(:),               allocatable :: obsinfo
    type(diags_convinfo_struct)                                         :: convinfo
    type(statgrid_struct)                                               :: statgrid
    type(vargrid_struct)                                                :: vargrid

    ! Define counting variables

    integer                                                             :: i 

    !=====================================================================

    ! Define local variables

    convinfo%filename = filename
    call diagsread_conv_count(convinfo)
    write(gsidiagprfvar%analdate,'(i10)') convinfo%idate

    ! Check local variable and proceed accordingly

    if(trim(adjustl(gsidiagprfvar%obstype)) .eq. 'dw')                     &
         & gsidiagprfvar%nobs = convinfo%nobs_dw(2)
    if(trim(adjustl(gsidiagprfvar%obstype)) .eq. 'gps')                    &
         & gsidiagprfvar%nobs = convinfo%nobs_gps(2)
    if(trim(adjustl(gsidiagprfvar%obstype)) .eq. 'q')                      &
         & gsidiagprfvar%nobs = convinfo%nobs_q(2)
    if(trim(adjustl(gsidiagprfvar%obstype)) .eq. 'rw')                     &
         & gsidiagprfvar%nobs = convinfo%nobs_rw(2)
    if(trim(adjustl(gsidiagprfvar%obstype)) .eq. 'spd')                    &
         & gsidiagprfvar%nobs = convinfo%nobs_spd(2)
    if(trim(adjustl(gsidiagprfvar%obstype)) .eq. 't')                      &
         & gsidiagprfvar%nobs = convinfo%nobs_t(2)
    if(trim(adjustl(gsidiagprfvar%obstype)) .eq. 'u')                      &
         & gsidiagprfvar%nobs = convinfo%nobs_uv(2)
    if(trim(adjustl(gsidiagprfvar%obstype)) .eq. 'v')                      &
         & gsidiagprfvar%nobs = convinfo%nobs_uv(2)
    
    ! Allocate memory for local variables

    call variable_interface_setup_struct(gsidiagprfvar)
    if(.not. allocated(obsinfo)) allocate(obsinfo(convinfo%ntotal_obs))

    ! Loop through local variable

    do i = 1, (gsidiagprfvar%nlevs - 1)

       ! Define local variables

       gsidiagprfvar%plevs(i) = 0.5*(gsidiags_convobs_plevs(i) +           &
            & gsidiags_convobs_plevs(i+1))

    end do ! do i = 1, (gsidiagprfvar%nlevs - 1)

    ! Define local variables

    call diagsread_conv_data(convinfo,obsinfo)
       
    ! Check local variable and proceed accordingly

    if(is_gsidiags_regional) then

       ! Compute local variables

       call bin_diags_plevs_regional(obsinfo,gsidiagprfvar)

    else   ! if(is_gsidiags_regional)
       
       ! Compute local variables

       call bin_diags_plevs(obsinfo,gsidiagprfvar)

    end if ! if(is_gsidiags_regional)
       
    ! Loop through local variable

    do i = 1, gsidiagprfvar%nlevs

       ! Define local variables
       
       vargrid%nvals = gsidiagprfvar%nobs
       call variable_interface_setup_struct(vargrid)
       vargrid%var   = gsidiagprfvar%omf(:,i)

       ! Compute local variables

       call math_methods_stats(vargrid,statgrid)

       ! Define local variables

       gsidiagprfvar%bias(i)   = statgrid%bias
       gsidiagprfvar%mean(i)   = statgrid%mean
       gsidiagprfvar%rmse(i)   = statgrid%rmse
       gsidiagprfvar%vari(i)   = statgrid%vari
       gsidiagprfvar%ncount(i) = statgrid%nvals

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(vargrid)

    end do ! do i = 1, gsidiagprfvar%nlevs

    ! Deallocate memory for local variables

    if(allocated(obsinfo)) deallocate(obsinfo)

    !=====================================================================

  end subroutine convprfobs

  !=======================================================================

end module gsidiags_interface
