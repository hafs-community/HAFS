module meteo_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! obs_to_bufr :: meteo_methods_interface
  ! Copyright (C) 2018 Henry R. Winterbottom

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
  public :: meteo_methods_spechumd
  public :: meteo_methods_wvmxrt

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_spechumd.f90

  ! DESCRIPTION:
  
  ! This subroutine computes the specific humidity value.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable containing the pressure
  !   (p; Pascals), the temperature (t; Kelvin) and relative humidity
  !   (rh; percentage) values.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable containing the computed
  !   specific humidity value (q; kilograms per kilograms).

  !-----------------------------------------------------------------------
  
  subroutine meteo_methods_spechumd(grid)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: grid

    !=====================================================================

    ! Compute local variables

    call meteo_methods_wvmxrt(grid)
    grid%q = grid%wvmxrt/(dble(1.0) + grid%wvmxrt)

    !=====================================================================

  end subroutine meteo_methods_spechumd

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_wvmxrt.f90

  ! DESCRIPTION:

  ! This subroutine computes the water vapor mixing ratio value.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable containing the pressure
  !   (p; Pascals), the temperature (t; Kelvin) and relative humidity
  !   (rh; percentage) values.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable containing the computed
  !   water vapor mixing ratio value (wvmxrt; kilograms per
  !   kilograms).

  !-----------------------------------------------------------------------

  subroutine meteo_methods_wvmxrt(grid)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: grid

    ! Define variables computed within routine

    real(r_double)                                                      :: eps
    real(r_double)                                                      :: es
    real(r_double)                                                      :: es0
    real(r_double)                                                      :: svp1
    real(r_double)                                                      :: svp2
    real(r_double)                                                      :: svp3
    real(r_double)                                                      :: svpt0

    !=====================================================================

    ! Define local variables

    es0         = dble(6.1121)
    eps         = dble(0.622)
    svp1        = dble(0.6112)
    svp2        = dble(17.67)
    svp3        = dble(29.62)
    svpt0       = dble(273.15)
    grid%wvmxrt = dble(spval)

    ! Compute local variables

    es = dble(0.01)*grid%rh*svp1*dble(10.0)*exp(svp2*(grid%t - svpt0)/     &
         & (grid%t - svp3))

    ! Check local variable and proceed accordingly

    if(es .ge. grid%p/dble(100.0)) then

       ! Define local variables

       grid%wvmxrt = dble(1.e-6)

    else   ! if(es .ge. grid%p/dble(100.0))

       ! Define local variables

       grid%wvmxrt = max((eps*es)/(grid%p/dble(100.0) - es),dble(1.e-6))

    end if ! if(es .ge. grid%p(i)/dble(100.0))

    !=====================================================================

  end subroutine meteo_methods_wvmxrt

  !=======================================================================

end module meteo_methods_interface
