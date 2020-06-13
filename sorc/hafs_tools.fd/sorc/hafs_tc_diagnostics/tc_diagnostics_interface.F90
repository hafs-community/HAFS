module tc_diagnostics_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: tc_diagnostics_interface
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

  use diagnostics_interface
  use fileio_interface
  use kinds_interface
  use mpi_interface
  use namelist_interface
  use tcdiags_methods_interface
  use variable_interface

  ! Define interfaces and attributes for module routines

  implicit none
  private
  public :: tc_diagnostics

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! compute_tccps.f90

  ! DESCRIPTION:

  ! This subroutine computes the tropical cyclone (TC) cyclone phase
  ! space (CPS) lower- and upper-troposphere thermal structure
  ! parameters described by Hart [2003] and writes the resultant
  ! attributes of the tccps_struct variable to an external netcdf file
  ! (in the user specified namelist 'datapath' variable) named
  ! 'tccps.<tcid>.<analdate>.nc'.

  ! REFERENCES:

  ! Hart, R. E., 2003: A cyclone phase space derived from thermal wind
  ! and thermal asymmetry. Mon. Wea. Rev., 131, 585-616.

  ! INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable array containing all viable
  !   tropical cyclone attributes (obtained from the NCEP tracker
  !   forecast files).

  ! * meteo; a FORTRAN meteo_struct variable.

  !-----------------------------------------------------------------------

  subroutine compute_tccps(tcv,meteo)

    ! Define variables passed to routine

    type(tcv_struct)                                                    :: tcv(:)
    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    type(tccps_struct)                                                  :: tccps

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Loop through local variable

    do i = 1, size(tcv)

       ! Compute local variables
       
       call tcdiags_methods_tccps(tcv(i),meteo,tccps)

       ! Define local variables

       call fileio_interface_write(tcv(i),tccps)

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(tccps)

    end do ! do i = 1, size(tcv)

    !=====================================================================

  end subroutine compute_tccps

  !=======================================================================

  ! SUBROUTINE:

  ! compute_tcenv.f90

  ! DESCRIPTION:

  ! This subroutine computes the atmospheric state in the absense of
  ! the user specified TCs; diagnostic environmental variables
  ! (defined in the listed references) are computed and written to the
  ! external netcdf file tcenv.<analdate>.nc.

  ! REFERENCES:

  ! DeMaria, M., M. Mainelli, L. K. Shay, J. A. Knaff, and J. Kaplan,
  ! 2005: Further improvements to the Statistical Hurricane Intensity
  ! Prediction Scheme (SHIPS). Wea. Forecasting, 20, 531–543.
  
  ! Velden, C. S. and L. M. Leslie, 1991: The basic relationship
  ! between tropical cyclone intensity and the depth of the
  ! environmental steering layer in the Australian
  ! Region. Wea. Forecasting, 6, 244–253.  

  ! Winterbottom, H. R., and E. P. Chassignet, 2011: A vortex
  ! isolation and removal algorithm for numerical weather prediction
  ! model tropical cyclone applications, J. Adv. Model. Earth Syst.,
  ! 3, M11003.

  ! INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable array containing all viable
  !   tropical cyclone attributes (obtained from the NCEP tracker
  !   forecast files).

  ! * meteo; a FORTRAN meteo_struct variable.

  !-----------------------------------------------------------------------

  subroutine compute_tcenv(tcv,meteo)

    ! Define variables passed to routine

    type(tcv_struct)                                                    :: tcv(:)
    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    type(tcenv_struct)                                                  :: tcenv

    !=====================================================================

    ! Compute local variables

    call tcdiags_methods_tcenv(tcv,meteo,tcenv)

    ! Define local variables

    call fileio_interface_write(tcenv)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(tcenv)

    !=====================================================================

  end subroutine compute_tcenv

  !=======================================================================

  ! SUBROUTINE:

  ! compute_tcmpi.f90

  ! DESCRIPTION:

  ! This subroutine computes the tropical cyclone (TC) maximum
  ! potential intensity (MPI) metric described by Emanuel [1988] and
  ! write the resultant attributes of the tcmpi_struct variable to an
  ! external netcdf file (in the user specified namelist 'datapath'
  ! variable) named 'tcmpi.<analdate>.nc'.

  ! REFERENCES:

  ! Emanuel, K. A., 1988: The Maximum Intensity of
  ! Hurricanes. J. Atmos. Sci., 45, 1143–1155.

  ! INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable array containing all viable
  !   tropical cyclone attributes (obtained from the NCEP tracker
  !   forecast files).

  ! * meteo; a FORTRAN meteo_struct variable.

  !-----------------------------------------------------------------------

  subroutine compute_tcmpi(tcv,meteo)

    ! Define variables passed to routine

    type(tcv_struct)                                                    :: tcv(:)
    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    type(tcmpi_struct)                                                  :: tcmpi

    !=====================================================================

    ! Compute local variables

    call tcdiags_methods_tcmpi(tcv,meteo,tcmpi)

    ! Define local variables

    call fileio_interface_write(tcmpi)
    
    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(tcmpi)

    !=====================================================================

  end subroutine compute_tcmpi

  !=======================================================================

  ! SUBROUTINE:

  ! compute_tcmsi.f90

  ! DESCRIPTION:

  ! This subroutine computes the tropical cyclone (TC) multi-scale
  ! intensity (MSI) index described by Vukicevic et al., [2014] and
  ! the TC integrated kinetic energy (IKE) described by Powell and
  ! Reinhold [2007] and finally writes the resultant attributes of the
  ! tcmsi_struct variable to an external netcdf file (in the user
  ! specified namelist 'datapath' variable) named
  ! 'tcmsi.<tcid>.<analdate>.nc'.

  ! REFERENCES:

  ! Powell, M. D. and T. A. Reinhold, 2007: Tropical Cyclone
  ! Destructive Potential by Integrated Kinetic
  ! Energy. Bull. Amer. Meteor. Soc., 88, 513–526.

  ! Vukicevic, T., E. Uhlhorn, P. Reasor, and B. Klotz, 2014: A Novel
  ! Multiscale Intensity Metric for Evaluation of Tropical Cyclone
  ! Intensity Forecasts. J. Atmos. Sci., 71, 1292–1304.

  ! INPUT VARIABLES:

  ! * tcv; a FORTRAN tcv_struct variable array containing all viable
  !   tropical cyclone attributes (obtained from the NCEP tracker
  !   forecast files).

  ! * meteo; a FORTRAN meteo_struct variable.

  !-----------------------------------------------------------------------

  subroutine compute_tcmsi(tcv,meteo)

    ! Define variables passed to routine

    type(tcv_struct)                                                    :: tcv(:)
    type(meteo_struct)                                                  :: meteo

    ! Define variables computed within routine

    type(tcmsi_struct)                                                  :: tcmsi
    logical                                                             :: tcv_pass

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Loop through local variable

    do i = 1, size(tcv)

       ! Compute local variables
       
       call tcdiags_methods_tcmsi(tcv(i),meteo,tcmsi,tcv_pass)

       ! Check local variable and proceed accordingly

       if(tcv_pass) call fileio_interface_write(tcv(i),tcmsi)

       ! Deallocate memory for local variables

       call variable_interface_cleanup_struct(tcmsi)

    end do ! do i = 1, size(tcv)

    !=====================================================================

  end subroutine compute_tcmsi

  !=======================================================================

  ! SUBROUTINE:

  ! tc_diagnostics.f90

  ! DESCRIPTION:
  
  ! This subroutine is the driver routine for all TC diagnostics
  ! calculations.

  !-----------------------------------------------------------------------

  subroutine tc_diagnostics()

    ! Define variables computed within routine

    type(tcv_struct),           dimension(:),               allocatable :: tcv
    type(meteo_struct)                                                  :: meteo
    real(r_kind)                                                        :: time_start

    !=====================================================================

    ! Define local variables

    call mpi_interface_initialize()
    call diagnostics_time_start(time_start)
    call namelist()
    call fileio_interface_read(tcv_filename,tcv)
    call fileio_interface_read(grib_filename,meteo)

    ! Compute local variables

    if(is_tccps) call compute_tccps(tcv,meteo)
    if(is_tcenv) call compute_tcenv(tcv,meteo)
    if(is_tcmpi) call compute_tcmpi(tcv,meteo)
    if(is_tcmsi) call compute_tcmsi(tcv,meteo)

    ! Deallocate memory for local variables

    if(allocated(tcv)) deallocate(tcv)
    call variable_interface_cleanup_struct(meteo)

    ! Define local variables

    call mpi_interface_finalize()
    call diagnostics_time_stop(time_start)

    !=====================================================================

  end subroutine tc_diagnostics

  !=======================================================================

end module tc_diagnostics_interface
