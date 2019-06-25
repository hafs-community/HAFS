module namelist_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! obs-to-bufr :: namelist_interface
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
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none

  !-----------------------------------------------------------------------

  ! DESCRIPTION (alphabetized):

  ! * analdate; a FORTRAN character string specifying the analysis
  !   date about which to define the observation times; formatted as,
  !   assuming UNIX convention, ccyy-mm-dd_HH:MM:SS.

  ! * bufr_filepath; a FORTRAN character string specifying the
  !   full-path to the BUFR file to be written (or appended) to.

  ! * bufr_tblpath; a FORTRAN character string specifying the
  !   full-path to the external file containing the BUFR table to be
  !   written (or appended) to the output BUFR file.  error(s) for the
  !   respective observation type.

  ! * datapath; a FORTRAN character string specifying the full-path to
  !   the directory to contain output files written by the respective
  !   routines.

  ! * debug; a FORTRAN logical value specifying whether to include
  !   debug information during execution.

  ! * givtdruv_bufr_info_filepath; a FORTRAN character string
  !   specifying the full-path tot he external file containing the
  !   BUFR information for the National Oceanic and Atmospheric
  !   Administration (NOAA) Atlantic Oceanographic and Meteorological
  !   Laboratory (AOML) Hurricane Research Division (HRD) GIV-TDR u-
  !   and v-winds observation option.

  ! * givtdruv_obs_filepath; a FORTRAN character string specifying the
  !   full-path to the external file containing a list of National
  !   Oceanic and Atmospheric Administration (NOAA) Atlantic
  !   Oceanographic and Meteorological Laboratory (AOML) Hurricane
  !   Research Division (HRD) GIV-TDR u- and v-wind observation files
  !   to process.

  ! * hsa_bufr_info_filepath; a FORTRAN character string specifying
  !   the full-path tot he external file containing the BUFR
  !   information for the HSA observation option.

  ! * hsa_obs_filepath; a FORTRAN character string specifying the
  !   full-path to the external file containing a list of
  !   HSA-formatted observation files to process.

  ! * is_givtdruv; a FORTRAN logical value specifying whether the
  !   input data type is National Oceanic and Atmospheric
  !   Administration (NOAA) Atlantic Oceanographic and Meteorological
  !   Laboratory (AOML) Hurricane Research Division (HRD) GIV-TDR u-
  !   and v-wind observations post-processed to netcdf files.

  ! * is_hsa; a FORTRAN logical value specifying whether the input
  !   data type is National Oceanic and Atmospheric Administration
  !   (NOAA) Atlantic Oceanographic and Meteorological Laboratory
  !   (AOML) Hurricane Research Division (HRD) spline analysis (HSA)
  !   format.

  ! * is_nhcgtcm; a FORTRAN logical value specifying whether the input
  !   data type is National Hurricane Center (NHC) Gridded Tropical
  !   Cyclone Model (GTCM) wind speed values.

  ! * is_tcm; a FORTRAN logical value specifying whether the input
  !   data type is tropical cyclone (TC) model (TCM) (e.g., a
  !   parameterized wind field model) output surface pressure and wind
  !   values.

  ! * is_temis; a FORTRAN logical value specifying whether the
  !   topography data is derived from Tropospheric Emission Monitoring
  !   Internet Service (TEMIS) data.

  ! * intrp_obserr; a FORTRAN logical value specifying whether to
  !   interpolate the user specified errors to the isobaric levels of
  !   the respective observations.

  ! * mask_land; a FORTRAN logical value specifying whether to apply a
  !   land-mask for observation values (i.e., all observations
  !   occuring over non-zero topography are masked out -- not written
  !   as a BUFR record).

  ! * mask_ocean; a FORTRAN logical value specifying whether to apply
  !   a ocean-mask for observation values (i.e., all observations
  !   occuring over ocean are masked out -- not written as a BUFR
  !   record).

  ! * nhcgtcm_bufr_info_filepath; a FORTRAN character string
  !   specifying the full-path tot he external file containing the
  !   BUFR information for the NHC GTCM observation option.

  ! * nhcgtcm_obs_filepath; a FORTRAN character string specifying the
  !   full-path to the external file containing a list of NHC GTCM
  !   formatted observation files to process.

  ! * obserr_filepath; a FORTRAN character string specifying the
  !   full-path to the external file containing the respective
  !   observation-type errors; see the README files accompanying the
  !   software package.

  ! * obs_flag; a FORTRAN logical value specifying whether to flag
  !   specific observations (see obs_flag_json_vtable) within a user
  !   specified BUFR file.

  ! * tcm_bufr_info_filepath; a FORTRAN character string specifying
  !   the full-path tot he external file containing the BUFR
  !   information for the TCM observation option.

  ! * tcm_obs_filepath; a FORTRAN character string specifying the
  !   full-path to the external file containing a list of TCM
  !   formatted observation files to process.

  ! * topo_filepath; a FORTRAN character string specifying the
  !   full-path to the external file containing topography data (used
  !   for the land- and ocean-mask options).

  !-----------------------------------------------------------------------

  ! Define local variables

  character(len=500)                                                    :: &
       & bufr_filepath = 'NOT USED'  
  character(len=500)                                                    :: &
       & bufr_tblpath = 'NOT USED'  
  character(len=500)                                                    :: &
       & datapath = './'
  character(len=500)                                                    :: &
       & fcst_model_bufr_info_filepath = 'NOT USED'
  character(len=500)                                                    :: &
       & fcst_model_filepath = 'NOT USED'
  character(len=500)                                                    :: &
       & givtdruv_bufr_info_filepath = 'NOT USED'
  character(len=500)                                                    :: &
       & givtdruv_obs_filepath = 'NOT USED'
  character(len=500)                                                    :: &
       & hsa_bufr_info_filepath = 'NOT USED'
  character(len=500)                                                    :: &
       & hsa_obs_filepath = 'NOT USED'
  character(len=500)                                                    :: &
       & hsa_obserr_filepath = 'NOT USED'
  character(len=500)                                                    :: &
       & nhcgtcm_bufr_info_filepath = 'NOT USED'
  character(len=500)                                                    :: &
       & nhcgtcm_obs_filepath = 'NOT USED'
  character(len=500)                                                    :: &
       & obs_flag_json_vtable = 'NOT USED'
  character(len=500)                                                    :: &
       & tcm_bufr_info_filepath = 'NOT USED'
  character(len=500)                                                    :: &
       & tcm_obs_filepath = 'NOT USED'
  character(len=500)                                                    :: &
       & topo_filepath = 'NOT USED'
  character(len=19)                                                     :: &
       & analdate = '2000-01-01_00:00:00'
  logical                                                               :: &
       & debug = .false.
  logical                                                               :: &
       & hsa_intrp_obserr = .false.
  logical                                                               :: &
       & is_fcstmdl = .false.
  logical                                                               :: &
       & is_givtdruv = .false.
  logical                                                               :: &
       & is_hsa = .false.
  logical                                                               :: &
       & is_nhcgtcm = .false.
  logical                                                               :: &
       & is_tcm = .false.
  logical                                                               :: &
       & is_temis = .false.
  logical                                                               :: &
       & mask_land = .false.
  logical                                                               :: &
       & mask_ocean = .false.
  logical                                                               :: &
       & obs_flag = .false.  
  namelist /share/      analdate, datapath, debug, is_fcstmdl,             &
       & is_givtdruv, is_hsa, is_nhcgtcm, is_tcm, obs_flag
  namelist /bufr/       bufr_filepath, bufr_tblpath
  namelist /fcstmdl/    fcst_model_bufr_info_filepath,                     &
       & fcst_model_filepath
  namelist /flag/       obs_flag_json_vtable
  namelist /givtdruv/   givtdruv_bufr_info_filepath,                       &
       & givtdruv_obs_filepath
  namelist /hsa/        hsa_bufr_info_filepath, hsa_intrp_obserr,          &
       & hsa_obs_filepath, hsa_obserr_filepath
  namelist /nhcgtcm/    nhcgtcm_bufr_info_filepath, nhcgtcm_obs_filepath
  namelist /tcm/        tcm_bufr_info_filepath, tcm_obs_filepath
  namelist /topo/       is_temis, mask_land, mask_ocean, topo_filepath
 
  !-----------------------------------------------------------------------

contains
  
  !=======================================================================

  ! SUBROUTINE: 

  ! namelist.f90

  ! DESCRIPTION: 

  ! This subroutine acts as the interface to the namelist file,
  ! provided as 'obs-to-bufr.input' by the user.

  !-----------------------------------------------------------------------

  subroutine namelist()

    ! Define variables computed within routine

    character(len=500)                                                  :: nml_filename
    logical                                                             :: is_it_there
    integer                                                             :: unit_nml
   
    !=====================================================================

    ! Define local variables

    nml_filename = './obs-to-bufr.input'
    unit_nml     = 9
    is_it_there  = .false.
    inquire(file = trim(adjustl(nml_filename)),exist = is_it_there)

    ! Check local variable and proceed accordingly

    if(is_it_there) then

       ! Define local variables

       open(file   = trim(adjustl(nml_filename)),                          &
            unit   = unit_nml        ,                                     &
            status = 'old'         ,                                       &
            form   = 'formatted'     ,                                     &
            action = 'read')
       read(unit_nml,NML = share)
       read(unit_nml,NML = bufr)
       read(unit_nml,NML = fcstmdl)
       read(unit_nml,NML = flag)
       read(unit_nml,NML = givtdruv)
       read(unit_nml,NML = hsa)
       read(unit_nml,NML = nhcgtcm)
       read(unit_nml,NML = tcm)
       read(unit_nml,NML = topo)
       close(unit_nml)

    else  ! if(is_it_there)

       ! Define local variables

       write(6,500)
       stop
       
    end if ! if(is_it_there)
       
    ! Define local variables
    
    write(6,*) '&SHARE'             
    write(6,*) 'DATAPATH                      = ',                         &
         & trim(adjustl(datapath))
    write(6,*) 'DEBUG                         = ', debug
    write(6,*) 'ANALDATE                      = ', analdate
    write(6,*) 'IS_GIVTDRUV                   = ', is_givtdruv
    write(6,*) 'IS_HSA                        = ', is_hsa
    write(6,*) 'IS_NHCGTCM                    = ', is_nhcgtcm
    write(6,*) 'IS_TCM                        = ', is_tcm
    write(6,*) '/'
    write(6,*) '&BUFR'    
    write(6,*) 'BUFR_FILEPATH                 = ',                         &
         & trim(adjustl(bufr_filepath))
    write(6,*) 'BUFR_TBLPATH                  = ',                         &
         & trim(adjustl(bufr_tblpath))
    write(6,*) '/'
    write(6,*) '&FCSTMDL'
    write(6,*) 'FCST_MODEL_BUFR_INFO_FILEPATH = ',                         &
         & trim(adjustl(fcst_model_bufr_info_filepath))
    write(6,*) 'FCST_MODEL_FILEPATH           = ',                         &
         & trim(adjustl(fcst_model_filepath))
    write(6,*) '/'
    write(6,*) '&FLAG'
    write(6,*) 'OBS_FLAG_JSON_VTABLE          = ',                         &
         & trim(adjustl(obs_flag_json_vtable))
    write(6,*) '/'
    write(6,*) '&GIVTDRUV'
    write(6,*) 'GIVTDRUV_BUFR_INFO_FILEPATH   = ',                         &
         & trim(adjustl(givtdruv_bufr_info_filepath))
    write(6,*) 'GIVTDRUV_OBS_FILEPATH         = ',                         &
         & trim(adjustl(givtdruv_obs_filepath))
    write(6,*) '&HSA'  
    write(6,*) 'HSA_BUFR_INFO_FILEPATH        = ',                         &
         & trim(adjustl(hsa_bufr_info_filepath))
    write(6,*) 'HSA_INTRP_OBSERR              = ', hsa_intrp_obserr
    write(6,*) 'HSA_OBS_FILEPATH              = ',                         &
         & trim(adjustl(hsa_obs_filepath))
    write(6,*) 'HSA_OBSERR_FILEPATH           = ',                         &
         & trim(adjustl(hsa_obserr_filepath))
    write(6,*) '/'
    write(6,*) '&NHCGTCM'
    write(6,*) 'NHCGTCM_BUFR_INFO_FILEPATH    = ',                         &
         & trim(adjustl(nhcgtcm_bufr_info_filepath))
    write(6,*) 'NHCGTCM_OBS_FILEPATH          = ',                         &
         & trim(adjustl(nhcgtcm_obs_filepath))
    write(6,*) '/'
    write(6,*) '&TCM'
    write(6,*) 'TCM_BUFR_INFO_FILEPATH        = ',                         &
         & trim(adjustl(tcm_bufr_info_filepath))
    write(6,*) 'TCM_OBS_FILEPATH              = ',                         &
         & trim(adjustl(tcm_obs_filepath))
    write(6,*) '/'
    write(6,*) '&TOPO'
    write(6,*) 'IS_TEMIS                      = ', is_temis
    write(6,*) 'MASK_LAND                     = ', mask_land
    write(6,*) 'MASK_OCEAN                    = ', mask_ocean
    write(6,*) 'TOPO_FILEPATH                 = ',                         &
         & trim(adjustl(topo_filepath))
    write(6,*) '/'
500 format('NAMELISTPARAMS: obs-to-bufr.input not found in the ',          &
         & 'current working directory. ABORTING!!!!')

    !=====================================================================

  end subroutine namelist
  
  !=======================================================================

end module namelist_interface
