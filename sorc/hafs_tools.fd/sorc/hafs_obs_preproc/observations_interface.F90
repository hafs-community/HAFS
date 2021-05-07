module observations_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: observations_interface
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

  ! Review the README, within the top-level directory, which provides
  ! relevant instructions and (any) references cited by algorithms
  ! within this software suite.

  !=======================================================================

  ! Define associated modules and subroutines

  use bufr_obs_interface
  use forecast_model_interface
  use kinds_interface
  use namelist_interface
  use recon_tdr_interface
  use recon_vdm_interface
  use sonde_tempdrop_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: observations

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! observations.f90

  ! DESCRIPTION:

  ! This is the driver routine for the preparation of all observation
  ! types.

  !-----------------------------------------------------------------------

  subroutine observations()

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(is_bufr_obs)   call bufr_obs_update()
    if(is_fcst_model) call obs_fcst_model()
    if(is_recon)      call obs_recon()
    if(is_sonde)      call obs_sonde()
    
    !=====================================================================
    
  end subroutine observations

  !=======================================================================

  ! SUBROUTINE:

  ! obs_fcst_model.f90

  ! DESCRIPTION:

  ! This is the driver routine for the preparation of all observations
  ! collected from forecast model files; currently the following
  ! forecast models are supported:

  ! + Finite Volume Cubed Sphere (FV3)

  !-----------------------------------------------------------------------

  subroutine obs_fcst_model()

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(is_fv3) call forecast_model_fv3()
    
    !=====================================================================

  end subroutine obs_fcst_model

  !=======================================================================

  ! SUBROUTINE:

  ! obs_recon.f90

  ! DESCRIPTION:

  ! This is the driver routine for the preparation of all observations
  ! collected from reconnissance messages; currently the following
  ! platforms are supported:

  ! + Tail-Doppler radar (TDR) reconnissance observation information.
  
  ! + National Hurricane Center (NHC) Vortex Data Messages (VDM).

  !-----------------------------------------------------------------------

  subroutine obs_recon()

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(is_recon_tdr) call recon_tdr()
    if(is_recon_vdm) call recon_vdm()
    
    !=====================================================================

  end subroutine obs_recon

  !=======================================================================

  ! SUBROUTINE:

  ! obs_sonde.f90

  ! DESCRIPTION:

  ! This is the driver routine for the preparation of all observations
  ! collected from sondes; currently the following platforms are
  ! supported:

  ! + American Oceanographic and Meteorological Laboratory (AOML)
  !   Hurricane Research Division (HRD) TEMP-DROP sondes.

  !-----------------------------------------------------------------------

  subroutine obs_sonde()

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(is_sonde_tempdrop) call sonde_tempdrop()
    
    !=====================================================================

  end subroutine obs_sonde
  
  !=======================================================================

end module observations_interface
