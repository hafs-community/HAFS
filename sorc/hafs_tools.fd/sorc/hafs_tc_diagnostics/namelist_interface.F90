module namelist_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tc-diagnostics :: namelist_interface
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

  use kinds_interface
  use mpi_interface

  ! Define interfaces and attributes for module routines
  
  implicit none

  !-----------------------------------------------------------------------

  ! DESCRIPTION (alphabetized):

  ! * analdate; a FORTRAN character string specifying the analysis
  !   date string, formatted (assuming the UNIX convention) as
  !   'ccyy-mm-dd_HH:MM:SS'.

  ! * datapath; a FORTRAN character string specifying the full-path to
  !   the directory to contain output files written by the respective
  !   routines.

  ! * debug; a FORTRAN logical value specifying whether to include
  !   debug information during execution.

  ! * filter_dangle; a FORTRAN 4-byte real value specifying the
  !   interval at which to bin angle values when computing the vortex
  !   filter region; units are degrees.

  ! * filter_kinfltfac; a FORTRAN 4-byte real value specifying the
  !   inflation factor for the kinematic (momentum) variable filter
  !   region.

  ! * filter_tinfltfac; a FORTRAN 4-byte real value specifying the
  !   inflation factor for the thermodynamic (mass) variable filter
  !   region.

  ! * filter_dwndthrsh; a FORTRAN 4-byte real value specifying the
  !   radial wind gradient threshhold value; units are meters per
  !   second.

  ! * filter_ratiokvar; a FORTRAN 4-byte real value specifying the
  !   momentum variable change in variance ratio threshold at which
  !   the smoothing filter, used to recombine the filtered tropical
  !   cyclone (TC) vortex analysis with the background analysis.

  ! * filter_ratiomvar; a FORTRAN 4-byte real value specifying the
  !   mass variable change in variance ratio threshold at which the
  !   smoothing filter, used to recombine the filtered tropical
  !   cyclone (TC) vortex analysis with the background analysis.

  ! * filter_twndthrsh; a FORTRAN 4-byte real value specifying the
  !   tangential wind threshhold value; units are meters per second.

  ! * filter_wndthrsh; a FORTRAN 4-byte real value specifying the
  !   radial wind threshhold value; units are meters per second.  

  ! * grib_filename; a FORTRAN character string specifying the
  !   full-path to the WMO GRIB-1 formatted file containing the
  !   gridded analysis variable fields.

  ! * grib_json_vtable; a FORTRAN character string specifying the
  !   full-path to the JSON formatted file containing the variable
  !   attributes for the WMO GRIB-1 formatted file variables.

  ! * is_filter_tcs; a FORTRAN logical variable specifying whether to
  !   apply the Winterbottom and Chassignet [2011] TC vortex filter
  !   application to remove TC-vortex regions from the TC
  !   environmental analysis.

  ! * is_glblmdl; a FORTRAN logical variable specifying whether the
  !   ingested analysis variables are from a global forecast model.

  ! * is_rgnlmdl; a FORTRAN logical variable specifying whether the
  !   ingested analysis variables are from a regional forecast model.

  ! * is_tccps; a FORTRAN logical variable specifying whether to
  !   compute the cyclone phase thermal structure values described in
  !   Hart [2003].

  ! * is_tcenv; a FORTRAN logical variable specifying whether to
  !   compute the TC environment (e.g., the atmospheric state in the
  !   absence of TCs) as described in Winterbottom and Chassignet
  !   [2011] and the diagnostic TC forecasting variables described in
  !   Velden and Leslie [1991] and DeMaria et al., [2005].

  ! * is_tcmpi; a FORTRAN logical variable specifying whether to
  !   compute the maximum potential intensity metrics described in
  !   Emanuel [1988].

  ! * is_tcmsi; a FORTRAN logical variable specifying whether to
  !   compute the tropical cyclone multi-scale intensity (MSI) index
  !   as described in Vukicevic et al., [2014].

  ! * ntrunc; a FORTRAN integer valued variable specifying the
  !   spectral trunction of the global model grid.

  ! * tc_filter_vmin_mps; a FORTRAN 4-byte real valued variable
  !   specifying the TC-relative minimum wind-speed value for which to
  !   consider application of the Winterbottom and Chassignet [2011]
  !   TC vortex filtering algorithm; TCs with wind-speeds below this
  !   value will be ignored during the TC filter applications; units
  !   are meters per second.

  ! * tccps_maxradius; a FORTRAN 4-byte real valued variable
  !   specifying the maximum radial distance, relative to a given TC
  !   position, about which to compute the cyclone phase thermal
  !   structure attributes; units are meters.

  ! * tcd_dangle; a FORTRAN 4-byte real value the specifying the
  !   interval at which to bin angle values when computing various
  !   tropical cyclone (TC) diagnostic values; units are degrees.

  ! * tcd_dpmsl; a FORTRAN 4-byte real value specifying the pressure
  !   reduced to sea-level interval used to compute various tropical
  !   cyclone (TC) diagnostic quantities; units are Pascals.

  ! * tcd_dradius; a FORTRAN 4-byte real value specifying the radial
  !   distance increment at which to compute tropical cyclone (TC)
  !   diagnostic quantities; units are meters.

  ! * tcd_maxradius; a FORTRAN 4-byte real value specifying the
  !   maximum radial distance, relative to the tropical cyclone (TC)
  !   position, about to perform various diagnostic calculations;
  !   units are meters.

  ! * tcd_pvu_tcdpth; a FORTRAN 4-byte real value specifying the
  !   potential vorticity unit (PVU) isosurface defining the top of
  !   the troposphere; units are potential vorticity units, e.g.,
  !   (10^-6*K*m^2)/(kilogram*second) assuming MKS units.

  ! * tcd_srcharea; a FORTRAN 4-byte real value specifying the area
  !   relative to the tropical cyclone (TC) about which to compute the
  !   shear vector (i.e., a value of 300000.0 meters implies the area
  !   will be computed within a region of 300000.0 meters by 300000.0
  !   meters relative to the TC); units are meters.

  ! * tcd_srchradius; a FORTRAN 4-byte real value specifying the
  !   search radius, relative to the first-guess tropical cyclone (TC)
  !   position provided by the user, about which to refine the
  !   reference geographical location; units are meters.

  ! * tcd_tcdpth_pbot; a FORTRAN 4-byte real value specifying the
  !   pressure level at which the PVU isosurface search (defined by
  !   the tropo_pvu) begins (i.e., bottom of layer) ; units are
  !   Pascals.

  ! * tcd_tcdpth_ptop; a FORTRAN 4-byte real value specifying the
  !   pressure level at which the PVU isosurface search (defined by
  !   the tropo_pvu) ends (i.e., top of layer) ; units are Pascals.

  ! * tcd_thetasfc; a FORTRAN 4-byte real value specifying the
  !   potential temperature (theta) surface about which to interpolate
  !   the potentical vorticity in order to determine the size of the
  !   tropical cyclone (TC); this follows from Davis et al., [2008];
  !   units are degrees Kelvin.

  ! * tcd_vrtxsz; a FORTRAN 4-byte real value specifying the maximum
  !   radial size for a tropical cyclone (TC); units are meters.

  ! * tcm_dangle; a FORTRAN 4-byte real value specifying the azimuthal
  !   interval for which to bin the wind speed values in order to
  !   compute the Fast-Fourier Transform (FFT) for the wave-number
  !   decomposition.

  ! * tcm_darea; a FORTRAN 4-byte real value specifying the areal
  !   increment for the TC-centered grid required to compute the the
  !   Fast-Fourier Transform (FFT) for the wave-number decomposition.

  ! * tcm_dradius; a FORTRAN 4-byte real value specifying the radial
  !   interval for which to bin the wind speed values in order to
  !   compute the Fast-Fourier Transform (FFT) for the wave-number
  !   decomposition.

  ! * tcm_mxwvn; a FORTRAN integer value specifying the maximum
  !   wave-number to be used to construct the TC vortex tangential
  !   wind speed values to define the momentum filtering region.

  ! * tcmpi_maskout_radius; a FORTRAN 4-byte real valued variable
  !   specifying the radial distance, relative to viable tropical
  !   cyclone positions, to maskout for the TCMPI calculations; units
  !   are meters.

  ! * tcmsi_area; a FORTRAN 4-byte real value specifying the area,
  !   relative to the respective TC, to define the region used to
  !   compute the TCMSI attribute variables; units are degrees.

  ! * tcmsi_dangle; a FORTRAN 4-byte real value specifying the radial
  !   distance interval, relative to the respective TC, to bin the
  !   TCMSI attribute variables; units are meters.

  ! * tcmsi_darea; a FORTRAN 4-byte real value specifying the areal
  !   interval for which to define the region used to compute the
  !   TCMSI attribute variables; units are degrees.

  ! * tcmsi_dradius; a FORTRAN 4-byte real value specifying the radial
  !   distance interval, relative to the respective TC, to bin the
  !   TCMSI attribute variables; units are meters.

  ! * tcv_filename; a FORTRAN character string specifying the
  !   full-path to the file containing attributes parsed from the NCEP
  !   tracker forecast files.

  !-----------------------------------------------------------------------

  ! Define local variables

  character(len=500)                                                    :: &
       & datapath = './'
  character(len=500)                                                    :: &
       & grib_filename = 'NOT USED'
  character(len=500)                                                    :: &
       & grib_json_vtable = 'NOT USED'
  character(len=500)                                                    :: &
       & tcv_filename = 'NOT USED'
  character(len=19)                                                     :: &
       & analdate = '2000-01-01_00:00:00'
  logical                                                               :: &
       & debug = .false.
  logical                                                               :: &
       & is_filter_tcs = .false.
  logical                                                               :: &
       & is_glblmdl = .false. 
  logical                                                               :: &
       & is_rgnlmdl = .false.
  logical                                                               :: &
       & is_tccps = .false.
  logical                                                               :: &
       & is_tcenv = .false.
  logical                                                               :: &
       & is_tcmpi = .false.
  logical                                                               :: &
       & is_tcmsi = .false.
  real(r_kind)                                                          :: &
       & filter_dangle = 15.0
  real(r_kind)                                                          :: &
       & filter_dradius = 25000.0
  real(r_kind)                                                          :: &
       & filter_dwndthrsh = 4.0e-6
  real(r_kind)                                                          :: &
       & filter_kinfltfac = 1.0 
  real(r_kind)                                                          :: &
       & filter_maxradius = 1200000.0
  real(r_kind)                                                          :: &
       & filter_ratiokvar = 1.e-2
  real(r_kind)                                                          :: &
       & filter_ratiomvar = 1.e-2
  real(r_kind)                                                          :: &
       & filter_tinfltfac = 1.0 
  real(r_kind)                                                          :: &
       & filter_twndthrsh = 3.0 
  real(r_kind)                                                          :: &
       & filter_wndthrsh = 6.0
  real(r_kind)                                                          :: &
       & tc_filter_vmin_mps = 18.0
  real(r_kind)                                                          :: &
       & tccps_maxradius = 500000.0
  real(r_kind)                                                          :: &
       & tcd_dangle = 15.0
  real(r_kind)                                                          :: &
       & tcd_dpmsl = 100.0  
  real(r_kind)                                                          :: &
       & tcd_dradius = 25000.0
  real(r_kind)                                                          :: &
       & tcd_maxradius = 1200000.0
  real(r_kind)                                                          :: &
       & tcd_pvu_tcdpth = 2.0
  real(r_kind)                                                          :: &
       & tcd_srcharea = 600000.0
  real(r_kind)                                                          :: &
       & tcd_srchradius = 250000.0
  real(r_kind)                                                          :: &
       & tcd_thetasfc = 310.0
  real(r_kind)                                                          :: &
       & tcd_tcdpth_pbot = 45000.0
  real(r_kind)                                                          :: &
       & tcd_tcdpth_ptop = 5000.0
  real(r_kind)                                                          :: &
       & tcd_vrtxsz = 1200000.0
  real(r_kind)                                                          :: &
       & tcm_dangle = 15.0
  real(r_kind)                                                          :: &
       & tcm_darea = 0.25
  real(r_kind)                                                          :: &
       & tcm_dradius = 25000.0
  real(r_kind)                                                          :: &
       & tcmpi_maskout_radius = 500000.0
  real(r_kind)                                                          :: &
       & tcmsi_area = 9.0
  real(r_kind)                                                          :: &
       & tcmsi_dangle = 15.0
  real(r_kind)                                                          :: &
       & tcmsi_darea = 0.1
  real(r_kind)                                                          :: &
       & tcmsi_dradius = 100000.0
  integer                                                               :: &
       & ntrunc = -999
  integer                                                               :: &
       & tcm_mxwvn = 2
  namelist /share/     analdate, datapath, debug, grib_filename,           &
       & grib_json_vtable, is_glblmdl, is_rgnlmdl, is_tccps, is_tcenv,     &
       & is_tcmpi, is_tcmsi, ntrunc, tcv_filename
  namelist /tccps_nml/ tccps_maxradius
  namelist /tcenv_nml/ filter_dangle, filter_dradius, filter_dwndthrsh,    &
       & filter_kinfltfac, filter_maxradius, filter_ratiokvar,             &
       & filter_ratiomvar, filter_tinfltfac, filter_twndthrsh,             &
       & filter_wndthrsh, is_filter_tcs, tc_filter_vmin_mps, tcd_dangle,   &
       & tcd_dpmsl, tcd_dradius, tcd_maxradius, tcd_pvu_tcdpth,            &
       & tcd_srcharea, tcd_srchradius, tcd_tcdpth_pbot, tcd_tcdpth_ptop,   &
       & tcd_thetasfc, tcd_vrtxsz, tcm_dangle, tcm_darea, tcm_dradius,     &
       & tcm_mxwvn
  namelist /tcmpi_nml/ tcmpi_maskout_radius
  namelist /tcmsi_nml/ tcmsi_area, tcmsi_dangle, tcmsi_darea,              &
       & tcmsi_dradius

  !-----------------------------------------------------------------------

contains
  
  !=======================================================================

  ! SUBROUTINE: 

  ! namelist.f90

  ! DESCRIPTION: 

  ! This subroutine acts as the interface to the namelist file,
  ! provided as 'tc-diagnostics.input' by the user.

  !-----------------------------------------------------------------------

  subroutine namelist()

    ! Define variables computed within routine
    
    character(len=500)                                                  :: nml_filename
    logical                                                             :: is_it_there
    integer                                                             :: unit_nml

    !=====================================================================

    ! Define local variables

    nml_filename = './tc-diagnostics.input'
    unit_nml     = 9
    is_it_there  = .false.
    inquire(file = trim(adjustl(nml_filename)),exist = is_it_there)

    ! Check local variable and proceed accordingly

    if(is_it_there) then

       ! Define local variables

       open(file   = trim(adjustl(nml_filename)),                        &
            unit   = unit_nml        ,                                   &
            status = 'old'         ,                                     &
            form   = 'formatted'     ,                                   &
            action = 'read')
       read(unit_nml,NML = share)
       read(unit_nml,NML = tccps_nml)
       read(unit_nml,NML = tcenv_nml)
       read(unit_nml,NML = tcmpi_nml)
       read(unit_nml,NML = tcmsi_nml)
       close(unit_nml)

    else  ! if(is_it_there)

       ! Define local variables

       if(mpi_procid .eq. mpi_masternode) write(6,500)                   &
           & trim(adjustl(nml_filename))
       call mpi_interface_waitall()
       call mpi_interface_finalize()
       
    end if ! if(is_it_there)

    ! Check local variable and proceed accordingly

    if(mpi_procid .eq. mpi_masternode) then
       
       ! Define local variables
       
       write(6,*) '&SHARE'
       write(6,*) 'ANALDATE                      = ', analdate
       write(6,*) 'DATAPATH                      = ',                    &
            & trim(adjustl(datapath))
       write(6,*) 'DEBUG                         = ', debug
       write(6,*) 'GRIB_FILENAME                 = ',                    &
            & trim(adjustl(grib_filename))
       write(6,*) 'GRIB_JSON_VTABLE              = ',                    &
            & trim(adjustl(grib_json_vtable))
       write(6,*) 'IS_GLBLMDL                    = ', is_glblmdl
       write(6,*) 'IS_RGNLMDL                    = ', is_rgnlmdl
       write(6,*) 'IS_TCCPS                      = ', is_tccps
       write(6,*) 'IS_TCENV                      = ', is_tcenv
       write(6,*) 'IS_TCMPI                      = ', is_tcmpi
       write(6,*) 'IS_TCMSI                      = ', is_tcmsi
       write(6,*) 'NTRUNC                        = ', ntrunc
       write(6,*) 'TCV_FILENAME                  = ',                    &
            & trim(adjustl(tcv_filename))
       write(6,*) '/'
       write(6,*) '&TCCPS_NML'
       write(6,*) 'TCCPS_MAXRADIUS               = ', tccps_maxradius
       write(6,*) '/'
       write(6,*) '&TCMPI_NML'
       write(6,*) 'TCMPI_MASKOUT_RADIUS          = ',                    &
            & tcmpi_maskout_radius
       write(6,*) '/'
       write(6,*) '&TCENV_NML'
       write(6,*) 'FILTER_DANGLE                 = ', filter_dangle
       write(6,*) 'FILTER_DRADIUS                = ', filter_dradius
       write(6,*) 'FILTER_DWNDTHRSH              = ', filter_dwndthrsh
       write(6,*) 'FILTER_KINFLTFAC              = ', filter_kinfltfac
       write(6,*) 'FILTER_MAXRADIUS              = ', filter_maxradius
       write(6,*) 'FILTER_RATIOKVAR              = ', filter_ratiokvar
       write(6,*) 'FILTER_RATIOMVAR              = ', filter_ratiomvar
       write(6,*) 'FILTER_TINFLTFAC              = ', filter_tinfltfac
       write(6,*) 'FILTER_TWNDTHRSH              = ', filter_twndthrsh
       write(6,*) 'FILTER_WNDTHRSH               = ', filter_wndthrsh
       write(6,*) 'IS_FILTER_TCS                 = ', is_filter_tcs
       write(6,*) 'TC_FILTER_VMIN_MPS            = ', tc_filter_vmin_mps
       write(6,*) 'TCD_DANGLE                    = ', tcd_dangle
       write(6,*) 'TCD_DPMSL                     = ', tcd_dpmsl
       write(6,*) 'TCD_DRADIUS                   = ', tcd_dradius
       write(6,*) 'TCD_MAXRADIUS                 = ', tcd_maxradius
       write(6,*) 'TCD_PVU_TCDPTH                = ', tcd_pvu_tcdpth
       write(6,*) 'TCD_SRCHAREA                  = ', tcd_srcharea
       write(6,*) 'TCD_SRCHRADIUS                = ', tcd_srchradius
       write(6,*) 'TCD_TCDPTH_PBOT               = ', tcd_tcdpth_pbot
       write(6,*) 'TCD_TCDPTH_PTOP               = ', tcd_tcdpth_ptop
       write(6,*) 'TCD_THETASFC                  = ', tcd_thetasfc
       write(6,*) 'TCD_VRTXSZ                    = ', tcd_vrtxsz
       write(6,*) 'TCM_DANGLE                    = ', tcm_dangle
       write(6,*) 'TCM_DAREA                     = ', tcm_darea
       write(6,*) 'TCM_DRADIUS                   = ', tcm_dradius
       write(6,*) 'TCM_MXWVN                     = ', tcm_mxwvn
       write(6,*) '/'       
       write(6,*) '&TCMSI_NML'  
       write(6,*) 'TCMSI_AREA                    = ', tcmsi_area
       write(6,*) 'TCMSI_DANGLE                  = ', tcmsi_dangle
       write(6,*) 'TCMSI_DAREA                   = ', tcmsi_darea
       write(6,*) 'TCMSI_DRADIUS                 = ', tcmsi_dradius
       write(6,*) '/'     

    end if ! if(mpi_procid .eq. mpi_masternode)
    
    ! Define local variables
    
    call mpi_interface_waitall()
500 format('NAMELISTPARAMS: ',a,' not found in the current working ',    &
        & 'directory. ABORTING!!!!')

    !===================================================================

  end subroutine namelist
  
  !=======================================================================
  
end module namelist_interface

