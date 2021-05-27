module meteo_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: meteo_methods_interface
  ! Copyright (C) 2020 Henry R. Winterbottom

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

  use constants_interface
  use kinds_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: meteo_methods_winds

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_winds.f90

  ! DESCRIPTION:

  ! This subroutine computes the attributes of the wind field in
  ! accordance with the user input variable (meteo_struct); if the
  ! zonal- (u-) and meridional (v-) wind components are equal to
  ! 'spval' upon entry, they are derived from the wind speed (spd) and
  ! direction (dir); if the wind speed and direction are equal to
  ! 'spval' upon entry, they are computed from the wind components (u-
  ! and v-).

  ! INPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the defined
  !   wind field attributes.

  ! OUTPUT VARIABLES:

  ! * meteo; a FORTRAN meteo_struct variable containing the computed
  !   wind field attributes

  !-----------------------------------------------------------------------

  subroutine meteo_methods_winds(meteo)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    type(winds_struct)                                                  :: winds

    ! Define counting variables

    integer                                                             :: i
  
    !=====================================================================

    ! Define local variables

    winds%nx = meteo%nx
    winds%ny = meteo%ny
    call variable_interface_setup_struct(winds)
    
    ! Loop through local variable

    do i = 1, meteo%nz
    
       ! Check local variable and proceed accordingly

       if((minval(meteo%u(:,i)) .eq. spval) .and. (maxval(meteo%u(:,i))    &
            & .eq. spval) .and. (minval(meteo%v(:,i)) .eq. spval) .and.    &
            & (maxval(meteo%v(:,i)) .eq. spval)) then

          ! Define local variables

          winds%dir = meteo%wdir(:,i)
          winds%spd = meteo%wspd(:,i)
          
          ! Compute local variables

          call wnd_comps(winds)

          ! Define local variables

          meteo%u(:,i) = winds%u
          meteo%v(:,i) = -1.0*winds%v

       end if ! if((minval(meteo%u(:,i)) .eq. spval)
              ! .and. (maxval(meteo%u(:,i)) .eq. spval)
              ! .and. (minval(meteo%v(:,i)) .eq. spval)
              ! .and. (maxval(meteo%v(:,i)) .eq. spval))

       ! Check local variable and proceed accordingly

       if((minval(meteo%wspd(:,i)) .eq. spval) .and.                       &
            & (maxval(meteo%wspd(:,i)) .eq. spval) .and.                   &
            & (minval(meteo%wdir(:,i)) .eq. spval) .and.                   &
            & (maxval(meteo%wdir(:,i)) .eq. spval)) then

          ! Define local variables

          winds%u = meteo%u(:,i)
          winds%v = meteo%v(:,i)
          
          ! Compute local variables

          call wnd_spddir(winds)

          ! Define local variables

          meteo%wdir(:,i) = winds%dir
          meteo%wspd(:,i) = winds%spd

       end if ! if((minval(meteo%wspd(:,i)) .eq. spval)
              ! .and. (maxval(meteo%wspd(:,i)) .eq. spval)
              ! .and. (minval(meteo%wdir(:,i)) .eq. spval)
              ! .and. (maxval(meteo%wdir(:,i)) .eq. spval))
          
    end do ! do i = 1, meteo%nz

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(winds)

    !=====================================================================
    
  end subroutine meteo_methods_winds

  !=======================================================================

  ! SUBROUTINE:

  ! wnd_comps.f90

  ! DESCRIPTION:

  ! This subroutine computes the zonal- and meridional wind (u- and v,
  ! respectively) components from the wind speed (spd) and direction
  ! (dir).

  ! INPUT VARIABLES:

  ! * winds; a FORTRAN winds_struct variable containing (at minimum)
  !   the wind speed (spd) and direction (dir).

  ! OUTPUT VARIABLES:

  ! * winds; a FORTRAN winds_struct variable containing the computed
  !   zonal- (u-) and meridional (v-) wind components.

  !-----------------------------------------------------------------------

  subroutine wnd_comps(winds)

    ! Define variables passed to routine

    type(winds_struct)                                                  :: winds

    !=====================================================================

    ! Compute local variables

    winds%u = winds%spd*cos(winds%dir*deg2rad)
    winds%v = winds%spd*sin(winds%dir*deg2rad)

    !=====================================================================

  end subroutine wnd_comps

  !=======================================================================

  ! SUBROUTINE:

  ! wnd_spddir.f90

  ! DESCRIPTION:

  ! This subroutine computes the wind speed and direction (spd and
  ! dir, respectively) from the zonal- and meridional wind (u- and v,
  ! respectively) components.

  ! INPUT VARIABLES:

  ! * winds; a FORTRAN winds_struct variable containing (at minimum)
  !   the zonal (u) and meridional (v) components.

  ! OUTPUT VARIABLES:

  ! * winds; a FORTRAN winds_struct variable containing the computed
  !   wind speed (spd) and direction (dir).

  !-----------------------------------------------------------------------

  subroutine wnd_spddir(winds)

    ! Define variables passed to routine

    type(winds_struct)                                                  :: winds

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Loop through local variable

    do i = 1, winds%ncoords

       ! Compute local variables

       winds%spd(i) = sqrt(winds%u(i)*winds%u(i) + winds%v(i)*winds%v(i))

       ! Check local variable and proceed accordingly

       if(winds%spd(i) .lt. 1.e-10) then

          ! Define local variables

          winds%dir(i) = 0.0

       else   ! if(winds%spd(i) .lt. 1.e-10)

          ! Compute local variables

          winds%dir(i) = atan2(winds%u(i),winds%v(i))*rad2deg + 270.0

       end if ! if(winds%spd(i) .lt. 1.e-10)
                 
    end do ! do i = 1, winds%ncoords
       
    !=====================================================================

  end subroutine wnd_spddir  

  !=======================================================================

end module meteo_methods_interface
