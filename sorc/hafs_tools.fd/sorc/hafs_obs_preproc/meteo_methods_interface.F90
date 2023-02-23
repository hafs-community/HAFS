module meteo_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! obs-preproc :: meteo_methods_interface
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

  use constants_interface
  use kinds_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: meteo_methods_dwpttemp
  public :: meteo_methods_geolatdist
  public :: meteo_methods_pottemp
  public :: meteo_methods_spechumd
  public :: meteo_methods_vpottemp
  public :: meteo_methods_wspdwdir
  public :: meteo_methods_wvmxrt

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_dwpttemp.f90

  ! DESCRIPTION:

  ! This subroutine computes the dewpoint temperature (dwpt; Kelvin)
  ! from the temperture (t; Kelvin) and relative humidity (rh;
  ! percentage) profiles.

  ! REFERENCES:

  ! http://andrew.rsmas.miami.edu/bmcnoldy/humidity_conversions.pdf

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable containing the temperature
  !   (t; Kelvin) and relative humidity (rh; percentage) profiles.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable containing the dewpoint
  !   temperature (dwpt; Kelvin) profile.

  !-----------------------------------------------------------------------

  subroutine meteo_methods_dwpttemp(grid)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: grid

    ! Define variables computed within routine

    real(r_double)                                                      :: a
    real(r_double)                                                      :: b 
    real(r_double)                                                      :: abt
    real(r_double)                                                      :: lrhp
    real(r_double)                                                      :: svpt0

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    a         = dble(17.625)
    b         = dble(243.04)
    svpt0     = dble(273.15)
    grid%dwpt = spval

    ! Loop through local variable

    do i = 1, grid%nz

       ! Check local variable and proceed accordingly

       if(grid%t(i) .ne. spval .and. grid%rh(i) .ne. spval) then

          ! Compute local variables

          abt          = (a*(grid%t(i) - svpt0))/(b + (grid%t(i) - svpt0))
          lrhp         = log(grid%rh(i)/dble(100.0))
          grid%dwpt(i) = (b*(lrhp + abt))/(a - lrhp - abt) + svpt0
          
       end if ! if(grid%t(i) .ne. spval .and. grid%rh(i) .ne. spval)

    end do ! do i = 1, grid%nz

    !=====================================================================

  end subroutine meteo_methods_dwpttemp

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_geolatdist.f90

  ! DESCRIPTION:

  ! This subroutine computes the zonal- and meridional-displacement
  ! (dx and dy, respectively) as a function of latitude.

  ! INPUT VARIABLES:

  ! * lat; a FORTRAN 4-byte real value specifying the latitude of
  !   interest; units are degrees.

  ! * dx; a FORTRAN 4-byte real value to define the
  !   zonal-displacement, dx.

  ! * dy; a FORTRAN 4-byte real value to define the
  !   meridional-displacement, dy.

  ! OUTPUT VARIABLES:

  ! * dx; a FORTRAN 4-byte real value specifying the
  !   zonal-displacement, dx; units are meters.

  ! * dy; a FORTRAN 4-byte real value specifying the
  !   meridional-displacement, dy; units are meters.

  !-----------------------------------------------------------------------

  subroutine meteo_methods_geolatdist(lat,dx,dy)

    ! Define variables passed to routine

    real(r_kind)                                                        :: lat
    real(r_kind)                                                        :: dx
    real(r_kind)                                                        :: dy

    !=====================================================================

    ! Compute local variables

    dx = 111.32*cos(lat*deg2rad)*1000.0
    dy = 110.574*1000.0

    !=====================================================================

  end subroutine meteo_methods_geolatdist

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_pottemp.f90

  ! DESCRIPTION:

  ! This subroutine computes the potential temperature (thta; Kelvin)
  ! profile from the temperature (t; Kelvin) and pressure (p; Pascals)
  ! profiles.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable containing the temperature
  !   (t; Kelvin) and pressure (p; Pascals) profiles.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable containing the computed
  !   potential temperature (thta; Kelvin) profile.

  !-----------------------------------------------------------------------

  subroutine meteo_methods_pottemp(grid)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: grid

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    grid%thta = spval

    ! Loop through local variable

    do i = 1, grid%nz

       ! Check local variable and proceed accordingly

       if(grid%t(i) .ne. spval .and. grid%p(i) .ne. spval) then

          ! Compute local variables

          grid%thta(i) = grid%t(i)*(dble(100000.0)/grid%p(i))**            &
               & dble(rd_over_cp_mass)

       end if ! if(grid%t(i) .ne. spval .and. grid%p(i) .ne. spval)

    end do ! do i = 1, grid%nz

    !=====================================================================

  end subroutine meteo_methods_pottemp

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

  ! meteo_methods_vpottemp.f90

  ! DESCRIPTION:

  ! This subroutine computes the virtual potential temperature (thtv;
  ! Kelvin) profile from the temperature (t; Kelvin), water vapor
  ! mixing ratio (q; kilograms per kilograms), and pressure (p;
  ! Pascals) profiles.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable containing the temperature
  !   (t; Kelvin), water vapor mixing ratio (q; kilograms per
  !   kilograms), and pressure (p; Pascals) profiles.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable containing the computed
  !   virtual potential temperature (thtv; Kelvin) profile.

  !-----------------------------------------------------------------------

  subroutine meteo_methods_vpottemp(grid)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: grid

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    grid%thtv = spval

    ! Loop through local variable

    do i = 1, grid%nz

       ! Check local variable and proceed accordingly

       if(grid%t(i) .ne. spval .and. grid%q(i) .ne. spval .and. grid%p(i)  &
            & .ne. spval) then

          ! Compute local variables

          grid%thtv(i) = (grid%q(i)*grid%t(i))*(dble(100000.0)/            &
               & grid%p(i))**dble(rd_over_cp_mass)

       end if ! if(grid%t(i) .ne. spval .and. grid%q(i) .ne. spval
              ! .and. grid%p(i) .ne. spval)

    end do ! do i = 1, grid%nz

    !=====================================================================

  end subroutine meteo_methods_vpottemp

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_wspdwdir.f90

  ! DESCRIPTION:

  ! This subroutine computes the wind speed magnitude and
  ! meteorological wind speed direction profiles from the zonal- and
  ! meridional-wind speed component profiles.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable containing the zonal- and
  !   meridional-wind speed component profiles.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable containing the computed
  !   wind-speed magnitude (wspd; meters per second) and
  !   wind-direction profiles (wdir; degrees).

  !-----------------------------------------------------------------------

  subroutine meteo_methods_wspdwdir(grid)

    ! Define variables passed to routine

    type(meteo_struct)                                                  :: grid

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    grid%wspd = spval
    grid%wdir = spval

    ! Loop through local variable

    do i = 1, grid%nz

       ! Check local variable and proceed accordingly

       if(grid%u(i) .ne. spval .and. grid%v(i) .ne. spval) then

          ! Compute local variables

          grid%wspd(i) = dble(sqrt(grid%u(i)*grid%u(i) +                   &
               & grid%v(i)*grid%v(i)))
          grid%wdir(i) = dble(atan2(grid%u(i),grid%v(i)))*dble(rad2deg) +  &
               & dble(180.0)

       end if ! if(grid%u(i) .ne. spval .and. grid%v(i) .ne. spval)

    end do ! do i = 1, grid%nz

    !=====================================================================

  end subroutine meteo_methods_wspdwdir

  !=======================================================================

  ! SUBROUTINE:

  ! meteo_methods_wvmxrt.f90

  ! DESCRIPTION:

  ! This subroutine computes the water vapor mixing ratio profile from
  ! the temperature (t) and relative humidity (rh) profiles.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable containing the temperature
  !   (t; Kelvin) and relative humidity (rh; percentage) profiles.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN meteo_struct variable containing the computed
  !   water vapor mixing ratio profile (q; kilograms per kilograms).

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

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    es0    = dble(6.1121)
    eps    = dble(0.622)
    svp1   = dble(0.6112)
    svp2   = dble(17.67)
    svp3   = dble(29.62)
    svpt0  = dble(273.15)
    grid%q = spval

    ! Loop through local variable

    do i = 1, grid%nz

       ! Check local variable and proceed accordingly

       if(grid%t(i) .ne. spval .and. grid%rh(i) .ne. spval) then

          ! Compute local variables

          es = dble(0.01)*grid%rh(i)*svp1*dble(10.0)*exp(svp2*(grid%t(i)   &
               & - svpt0)/(grid%t(i) - svp3))

          ! Check local variable and proceed accordingly

          if(es .ge. grid%p(i)/dble(100.0)) then

             ! Define local variables

             grid%wvmxrt(i) = dble(1.e-6)

          else   ! if(es .ge. grid%p(i)/dble(100.0))

             ! Define local variables

             grid%wvmxrt(i) = max(eps*es/(grid%p(i)/dble(100.0) - es),     &
                  & dble(1.e-6))

          end if ! if(es .ge. grid%p(i)/dble(100.0))

       end if ! if(grid%t(i) .ne. spval .and. grid%rh(i) .ne. spval)

    end do ! do i = 1, grid%nz

    !=====================================================================

  end subroutine meteo_methods_wvmxrt

  !=======================================================================

end module meteo_methods_interface
