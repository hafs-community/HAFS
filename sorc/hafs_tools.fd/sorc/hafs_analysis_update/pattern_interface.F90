module pattern_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: pattern_interface
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
  use math_methods_interface
  use namelist_interface
  use shtns
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: pattern_interface_compute

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! advance_autoregression.f90
  
  ! DESCRIPTION:

  ! This subroutine advances a first-order autoregressive processed
  ! with specfied auto-correlated and variance spectrum.

  ! INPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable.

  ! OUTPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable containing the
  !   updated noise (e.g., white noise) spectrum via the
  !   auto-regressive process.

  !-----------------------------------------------------------------------

  subroutine advance_autoregression(pattern)

    ! Define variables passed to routine

    type(pattern_struct)                                                :: pattern

    ! Define variables computed within routine

    complex(r_kind),            dimension(:),               allocatable :: noise

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(noise)) allocate(noise(pattern%ndimspec))

    ! Define local variables

    noise = pattern%noise

    ! Compute local variables

    call white_noise_spectrum(pattern)
    noise = pattern%phi*noise + pattern%stdev*sqrt(1.0 - pattern%phi       &
         & **2.0)*pattern%varspec*pattern%noise

    ! Define local variables

    pattern%noise = noise

    ! Deallocate memory for local variables

    if(allocated(noise)) deallocate(noise)

    !=====================================================================

  end subroutine advance_autoregression

  !=======================================================================

  ! SUBROUTINE:
  
  ! define_random_seed.f90

  ! DESCRIPTION:

  ! This subroutine provides a wrapper around the FORTRAN 90 random
  ! number generator (RNG) initialized via a 'seed' for the RNG; the
  ! random number (RN) seed is passed to this routine via the FORTRAN
  ! pattern_struct variable 'seed' attribute.

  ! INPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable.

  !-----------------------------------------------------------------------

  subroutine define_random_seed(pattern)

    ! Define variables passed to routine

    type(pattern_struct)                                                :: pattern

    ! Define variables computed within routine

    integer,                    dimension(:),               allocatable :: seed
    integer                                                             :: nseed

    !=====================================================================

    ! Define local variables

    call random_seed()
    call random_seed(size = nseed)

    ! Allocate memory for local variables

    if(.not. allocated(seed)) allocate(seed(nseed))

    ! Define local variables

    seed(1:nseed) = pattern%seed
    call random_seed(put = seed(1:nseed))

    ! Deallocate memory for local variables

    if(allocated(seed)) deallocate(seed)

    !=====================================================================

  end subroutine define_random_seed

  !=======================================================================

  ! SUBROUTINE:

  ! gaussian_spectrum.f90

  ! DESCRIPTION:

  ! This subroutine will define a variance spectrum assuming isotropic
  ! Gaussian covariance which is normalized with respect to a global
  ! variance of 1.0.

  ! INPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable.

  ! OUTPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable containing the
  !   normalized Gaussian variance spectra.

  !-----------------------------------------------------------------------

  subroutine gaussian_spectrum(pattern)

    ! Define variables passed to routine

    type(pattern_struct)                                                :: pattern

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Loop through local variable

    do i = 0, pattern%ntrunc

       ! Define local variables

       pattern%varspec1d(i) = exp(-1.0*pattern%lengthscale**2.0*(float(i)  &
            & *(float(i)+1.0))/(4.0*rearth_equator**2.0))

    end do ! do i = 0, pattern%ntrunc

    ! Define local variables

    pattern%varspec = sqrt(pattern%ntrunc*exp(pattern%lengthscale**2.0*    &
         & pattern%lap/(4.0*rearth_equator**2.0)))
    pattern%noise   = cmplx(0.0,0.0)

    ! Loop through local variable

    do i = 1, pattern%ndimspec

       ! Check local variable and proceed accordingly

       if(pattern%order(i) .ne. 0.0) then

          ! Define local variables

          pattern%noise(i) = cmplx(1.0,1.0)/sqrt(2.0*pattern%degree(i) +   &
               & 1.0)

       else   ! if(pattern%order(i) .ne. 0.0)

          ! Define local variables

          pattern%noise(i) = sqrt(2.0)/(sqrt(2.0*pattern%degree(i) + 1.0))

       end if ! if(pattern%order(i) .ne. 0.0)

    end do ! do i = 1, pattern%ndimspec

    ! Define local variables

    pattern%noise(1) = cmplx(0.0,0.0)
    pattern%noise    = pattern%noise*sqrt(pattern%normfact/                &
         & pattern%ntrunc)
    pattern%noise    = pattern%noise*pattern%varspec

    ! Compute local variables

    call spectrum_variance(pattern)

    ! Define local variables

    pattern%varspec   = pattern%varspec/sqrt(pattern%vari)
    pattern%varspec1d = pattern%varspec1d/pattern%vari

    !=====================================================================

  end subroutine gaussian_spectrum

  !=======================================================================

  ! SUBROUTINE:

  ! pattern_interface_compute.f90

  ! DESCRIPTION:

  ! This subroutine is the driver routine to compute a random number,
  ! yet spatially and temporally correlated, pattern initiated from a
  ! Gaussian 'white' noise spectrum and advanced using auto-regressive
  ! process.

  ! INPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable.

  ! OUTPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable containing the random
  !   number, spatially and temporally correlated, pattern within the
  !   'rnp' attribute.

  !-----------------------------------------------------------------------

  subroutine pattern_interface_compute(pattern)

    ! Define variables passed to routine

    type(pattern_struct)                                                :: pattern

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================
    
    ! Define local variables

    call variable_interface_setup_struct(pattern)
    pattern%dt          = adv_dtime
    pattern%seed        = rng_seed
    pattern%lengthscale = corr_length
    pattern%normfact    = 2.0*pi
    pattern%stdev       = corr_stdev
    pattern%tau         = corr_tau
    pattern%type        = 'GAU'
    pattern%degree      = (/((j,j=i,pattern%ntrunc),i=0,pattern%ntrunc)/)
    pattern%order       = (/((i,j=i,pattern%ntrunc),i=0,pattern%ntrunc)/)    
    pattern%lap         = -pattern%degree*(pattern%degree + 1.0)
    pattern%phi         = exp(-pattern%dt/pattern%tau)

    ! Compute local variables

    call define_random_seed(pattern)
    call gaussian_spectrum(pattern)
    call white_noise_spectrum(pattern)

    ! Loop through local variable

    do i = 1, corr_nmax

       ! Compute local variables

       call advance_autoregression(pattern)
       call spectrum_variance(pattern)

    end do ! do i = 1, corr_nmax

    ! Define local variables

    call shtns_init(pattern%nx,pattern%ny,pattern%ntrunc)

    ! Compute local variables

    call spectogrd(pattern%noise,pattern%rnp)

    ! Deallocate memory for local variables

    call shtns_destroy()

    !=====================================================================

  end subroutine pattern_interface_compute

  !=======================================================================

  ! SUBROUTINE:

  ! spectrum_variance.f90

  ! DESCRIPTION:

  ! This subroutine computes the globally integrated variance from the
  ! spectral coefficients.

  ! INPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable.

  ! OUTPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable containing the
  !   globally integrated variance within the 'vari' attribute.

  !-----------------------------------------------------------------------

  subroutine spectrum_variance(pattern)

    ! Define variables passed to routine

    type(pattern_struct)                                                :: pattern

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    pattern%vari = 0.0

    ! Loop through local variable

    do i = 1, pattern%ndimspec

       ! Check local variable and proceed accordingly

       if(pattern%order(i) .eq. 0) then

          ! Define local variables

          pattern%vari = pattern%vari + pattern%noise(i)*                  &
               & conjg(pattern%noise(i))

       else   ! if(pattern%order(i) .eq. 0)

          ! Define local variables

          pattern%vari = pattern%vari + 0.5*pattern%noise(i)*              &
               & conjg(pattern%noise(i))

       end if ! if(pattern%order(i) .eq. 0)

    end do ! do i = 1, pattern%ndimspec

    ! Define local variables

    pattern%vari = pattern%vari/pattern%normfact

    !=====================================================================

  end subroutine spectrum_variance

  !=======================================================================

  ! SUBROUTINE:

  ! white_noise_spectrum.f90

  ! DESCRIPTION:

  ! This subroutine generates a 'white' noise spectrum, assuming unit
  ! variance, in spectral space.

  ! INPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable.

  ! OUTPUT VARIABLES:

  ! * pattern; a FORTRAN pattern_struct variable containined the
  !   'white' noise spectrum within the 'noise' attribute.

  !-----------------------------------------------------------------------

  subroutine white_noise_spectrum(pattern)

    ! Define variables passed to routine

    type(pattern_struct)                                                :: pattern

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Loop through local variable

    do i = 1, pattern%ndimspec

       ! Check local variable and proceed accordingly

       if(pattern%order(i) .ne. 0.0) then

          ! Define local variables

          pattern%noise(i) = cmplx(math_methods_rnorm(),                   &
               & math_methods_rnorm())/sqrt(2.0*pattern%degree(i) + 1.0)

       else   ! if(pattern%order(i) .ne. 0.0)

          ! Define local variables

          pattern%noise(i) = sqrt(2.0)*math_methods_rnorm()/               &
               & (sqrt(2.0*pattern%degree(i) + 1.0))

       end if ! if(pattern%order(i) .ne. 0.0)
       
    end do ! do i = 1, pattern%ndimspec
    
    ! Define local variables
    
    pattern%noise(1) = 0.0
    pattern%noise    = pattern%noise*sqrt(pattern%normfact/pattern%ntrunc)

    !=====================================================================

  end subroutine white_noise_spectrum

  !=======================================================================

end module pattern_interface
