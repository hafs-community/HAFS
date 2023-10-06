module namelist_interface

  !=======================================================================

  !$$$ PROGRAM DOCUMENTATION BLOCK
  
  ! obs-preproc :: namelist_interface
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

  ! * bufr_info_filepath; a FORTRAN character string specifying
  !   the full-path to the external file containing the BUFR
  !   information for the respective observation type.

  ! * bufr_obs_filename; an array of FORTRAN character strings
  !   specifying the path to the BUFR-formatted files to be used to
  !   create the time-centered observations BUFR-formatted file.

  ! * bufr_obs_maxdate; a FORTRAN character string specifying the
  !   maximum observation time for all time-centered observation
  !   collected from bufr_obs_filename (see above); formatted as,
  !   assuming UNIX convention, ccyy-mm-dd_HH:MM:SS.

  ! * bufr_obs_mindate; a FORTRAN character string specifying the
  !   minimum observation time for all time-centered observation
  !   collected from bufr_obs_filename (see above); formatted as,
  !   assuming UNIX convention, ccyy-mm-dd_HH:MM:SS.
  
  ! * bufr_tblpath; a FORTRAN character string specifying the
  !   full-path to the external file containing the BUFR table to be
  !   written (or appended) to the output BUFR file. 
  
  ! * datapath; a FORTRAN character string specifying the full-path to
  !   the directory to contain output files written by the respective
  !   routines.

  ! * debug; a FORTRAN logical value specifying whether to include
  !   debug information during execution.

  !   Both is_fcst_model and is_fv3 must both be .true. for these
  !   files to be invoked.

  ! * fv3_dyns_filename; an array of FORTRAN character strings
  !   specifying the path to the FV3 netcdf files containing the
  !   dynamical core variables; namely, the following netcdf
  !   variables:

  !   + T; temperature (K).
  
  !   + delp; pressure layer difference (Pa).
  
  !   + ua; zonal wind on at the center of the grid cell (meters per
  !         second).

  !   + va; meridional wind on at the center of the grid cell (meters
  !         per second).

  ! * fv3_gridspec_filename; an array of FORTRAN character strings
  !   specifying th path to the FV3 netcdf files containing the grid
  !   geographical coordinate values; namely the following netcdf
  !   variables:

  !   + lat; the grid cell corner latitude values.

  !   + latt; the grid cell center latitude values.
  
  !   + lon; the grid cell corner longitude values.

  !   + lont; the grid cell center longitude values.
  
  ! * fv3_orog_filename; an array of FORTRAN character strings
  !   specifying the path to the FV3 netcdf files containing the
  !   orography; namely the following netcdf variables:

  !   + slmsk; the land/sea mask.

  !   Both is_fcst_model and is_fv3 must both be .true. for these
  !   files to be invoked.

  ! * fv3_static_filename; a FORTRAN character string specifying the
  !   path to the FV3 netcdf file containing the static variables;
  !   namely the following netcdf variables:

  !   + hyam; the hybrid-pressure coordinate values for the model
  !     layers.

  !   + hybm; the hybrid-pressure coordinate values for the model
  !     layers.

  !   Both is_fcst_model and is_fv3 must both be .true. for this file
  !   to be invoked.

  ! * fv3_tracer_filename; an array of FORTRAN character strings
  !   specifying the path to the FV3 netcdf files containing the
  !   tracer variables; namely the following netcdf variables:

  !   + sphum; specific humidity (kilograms per kilogram).

  !   Both is_fcst_model and is_fv3 must both be .true. for these
  !   files to be invoked.

  ! * grid_ratio; a FORTRAN 4-byte float value specifying the
  !   grid-refinement ratio; used only for nested grid tiles.

  ! * is_bufrobs; a FORTRAN logical value specifying whether to
  !   created BUFR-formatted observation files using the
  !   bufr_obs_filename (see above) values and the values for
  !   bufr_obs_maxdate and bufr_obs_mindate (see above).
  
  ! * is_fcst_model; a FORTRAN logical value specifying whether the
  !   observations to be formatted are computed from forecast model
  !   fields.

  ! * is_fv3; a FORTRAN logical value specifying whether the forecast
  !   model, for the creation of observations from forecast model
  !   fields, is from the FV3; is_fcst_model must be .true. to invoke
  !   this option.

  ! * is_global; a FORTRAN logical value specifying whether the
  !   ingested fields are from a global forecast model; applies only
  !   if is_fcst_model is .true..

  ! * is_gpsrobufr; a FORTRAN logical value specifying that the
  !   time-centered observations are for Global Position System (GPS)
  !   Radio Occultation (RO) observations; see is_bufrobs (above) and
  !   bufr_obs_filename (above).
  
  ! * is_prepbufr; a FORTRAN logical value specifying that the
  !   time-centered observations are for a prepbufr-type observation
  !   file; see is_bufrobs (above) and bufr_obs_filename (above).

  ! * is_recon; a FORTRAN logical variable specifying whether the
  !   observations to be formatted are derived from reconnissance-type
  !   missions.

  ! * is_recon_vdm; a FORTRAN logical value specifying whether the
  !   reconnissance observations are derived from vortex data message
  !   (VDM) files.

  ! * is_recon_tdr; a FORTRAN logical value specifying whether the
  !   reconnissance observations are derived from Tail-Doppler Radar
  !   (TDR) Binary Universal Formatted (BUFR) files.
  
  ! * is_regional; a FORTRAN logical value specifying whether the
  !   ingested fields are from a regional forecast model; applies only
  !   if is_fcst_model is .true..

  ! * is_relocate; a FORTRAN logical value specifying whether to
  !   relocate forecast model derived observations relative to
  !   tropical cyclone (TC) observed geographical locations.

  ! * is_rotate_winds; a FORTRAN logical value specifying whether to
  !   rotate forecast model vector winds to an Earth-relative rotation
  !   relative to a user specified geographical location.
  
  ! * is_satbufr; a FORTRAN logical value specifying that the
  !   time-centered observations are for a satellite observation
  !   bufr-type files; see is_bufrobs (above) and bufr_obs_filename
  !   (above).
  
  ! * is_sonde; a FORTRAN logical value specifying whether the
  !   observations to be formatted are derived from sondes.

  ! * is_sonde_tempdrop; a FORTRAN logical value specifying whether
  !   the sonde observations are derived from TEMP-DROP messages.

  ! * mask_land; a FORTRAN logical value specifying whether to apply a
  !   land-mask for observation values (i.e., all observations
  !   occuring over non-zero topography are masked out -- not written
  !   as a BUFR record).

  ! * recon_filelist; a FORTRAN character string specifying the
  !   full-path to the external file containing a list of files
  !   containing aircraft reconnissance derived observations to be
  !   written into a BUFR record.

  ! * recon_tdr_filepath; a FORTRAN character string specifying the
  !   full-path to the external file containing the TDR BUFR
  !   observation records.

  ! * sample_radius; a FORTRAN 4-byte float value specifying the
  !   thinning radius for forecast model derived observations; units
  !   are meters.
  
  ! * sonde_filelist; a FORTRAN character string specifying the
  !   full-path to the external file containing a list of TEMPDROP
  !   formatted sondes to be decoded.

  ! * tc_radius; a FORTRAN 4-byte float value specifying the maximum
  !   sampling radius for forecast model derived observations relative
  !   to tropical cyclone (TC) observed positions.

  ! * tcinfo_filename; a FORTRAN character string specifying the path
  !   to the ASCII formatted file containing the observed and model
  !   forecast attributes for the respective TC events; is_fcst_model
  !   must be .true. for this file to be invoked.

  ! * tdr_min_offset_seconds; a FORTRAN 4-byte float value specifying
  !   the minimum observation time offset (in seconds) relative to the
  !   time on the TDR BUFR file (see recon_tdr_filepath).

  ! * tdr_max_offset_seconds; a FORTRAN 4-byte float value specifying
  !   the maximum observation time offset (in seconds) relative to the
  !   time on the TDR BUFR file (see recon_tdr_filepath).

  ! * tdr_offset_deconds; a FORTRAN 4-byte float value specifying the
  !   minimum difference between the observation time offset values
  !   (see tdr_min_offset_seconds and tdr_max_offset_seconds).
  
  ! * tempdrop_compute_drift; a FORTRAN logical value specifying
  !   whether to estimate the sonde drift, and the respective
  !   geographical locations, from the collected TEMP-DROP formatted
  !   observations.

  ! * tempdrop_hsa_table_file; a FORTRAN character string specifying
  !   the full-path to a column-delimited table; if this file does not
  !   exist upon call to this routine, it will be created; if the file
  !   does exist upon call to this routine, it will be appended.
  
  ! * tempdrop_normalize; a FORTRAN logical value specifying whether
  !   to normalize the geographical coordinate values computed for the
  !   advection trajectory of the TEMP-DROP formatted observations.

  ! * tempdrop_write_nc_skewt; a FORTRAN logical value specifying
  !   whether to write a network common data format (netcdf) file
  !   containing interpolated National Oceanic and Atmospheric
  !   Administration (NOAA) Atlantic Oceanographic and Meteorological
  !   Laboratory (AOML) Hurricane Research Division (HRD) spline
  !   analysis (HSA) values; tempdrop_compute_drift must be true.

  ! * wmm_coeff_filepath; a FORTRAN character string containing the
  !   World Magnetic Model (WMM) coefficients that are used to
  !   estimate the variations of the magnetic North Pole relative to
  !   the Meteorological (e.g., 'true') North Pole; these are used in
  !   instances of reconnissance (e.g., aircraft) collected
  !   observations.
 
  !-----------------------------------------------------------------------

  ! Define local variables

  character(len=500)                                                    :: &
       & bufr_obs_filename(10) = 'NOT USED'
  character(len=500)                                                    :: &
       & fv3_dyns_filename(6) = 'NOT USED'
  character(len=500)                                                    :: &
       & fv3_gridspec_filename(6) = 'NOT USED'
  character(len=500)                                                    :: &
       & fv3_orog_filename(6) = 'NOT USED'
  character(len=500)                                                    :: &
       & fv3_tracer_filename(6) = 'NOT USED'  
  character(len=500)                                                    :: &
       & bufr_filepath = 'NOT USED'
  character(len=500)                                                    :: &
       & bufr_info_filepath = 'NOT USED'
  character(len=500)                                                    :: &
       & bufr_tblpath = 'NOT USED'
  character(len=500)                                                    :: &
       & datapath = './'
  character(len=500)                                                    :: &
       & fv3_static_filename = 'NOT USED'
  character(len=500)                                                    :: &
       & recon_filelist = 'NOT USED'
  character(len=500)                                                    :: &
       & recon_tdr_filepath = 'NOT USED'
  character(len=500)                                                    :: &
       & sonde_filelist = 'NOT USED'
  character(len=500)                                                    :: &
       & tcinfo_filename = 'NOT USED'
  character(len=500)                                                    :: &
       & tempdrop_hsa_table_file = './tempdrop-hsa.table'
  character(len=500)                                                    :: &
       & wmm_coeff_filepath = 'NOT USED'
  character(len=19)                                                     :: &
       & analdate = '2000-01-01_00:00:00'
  character(len=19)                                                     :: &
       & bufr_obs_maxdate = '2000-01-01_00:00:00'
  character(len=19)                                                     :: &
       & bufr_obs_mindate = '2000-01-01_00:00:00'
  logical                                                               :: &
       & debug = .false.
  logical                                                               :: &
       & is_bufr_obs = .false.
  logical                                                               :: &
       & is_fcst_model = .false.
  logical                                                               :: &
       & is_fv3 = .false.
  logical                                                               :: &
       & is_global = .false.
  logical                                                               :: &
       & is_gpsrobufr = .false.
  logical                                                               :: &
       & is_prepbufr = .false.
  logical                                                               :: &
       & is_recon = .false.  
  logical                                                               :: &
       & is_recon_tdr = .false.
  logical                                                               :: &
       & is_recon_vdm = .false. 
  logical                                                               :: &
       & is_regional = .false.
  logical                                                               :: &
       & is_relocate = .false.
  logical                                                               :: &
       & is_rotate_winds = .false.
  logical                                                               :: &
       & is_satbufr = .false.  
  logical                                                               :: &
       & is_sonde = .false.
  logical                                                               :: &
       & is_sonde_tempdrop = .false.
  logical                                                               :: &
       & mask_land = .false.
  logical                                                               :: &
       & tempdrop_compute_drift = .false.
  logical                                                               :: &
       & tempdrop_normalize = .false.
  logical                                                               :: &
       & tempdrop_write_nc_skewt = .false.
  real(r_kind)                                                          :: &
       & grid_ratio = 1.0
  real(r_kind)                                                          :: &
       & max_orog_hght = spval
  real(r_kind)                                                          :: &
       & sample_radius = spval
  real(r_kind)                                                          :: &
       & tc_radius = 600000.0
  real(r_kind)                                                          :: &
       & tdr_min_offset_seconds = 7200.0
  real(r_kind)                                                          :: &
       & tdr_max_offset_seconds = -7200.0
  real(r_kind)                                                          :: &
       & tdr_offset_dseconds = 1800.0
  integer                                                               :: &
       & nbufr_obs_files = 0
  namelist /share/      analdate, datapath, debug, is_bufr_obs,            &
       & is_fcst_model, is_recon, is_sonde
  namelist /bufrio/     bufr_filepath, bufr_info_filepath,                 &
       & bufr_obs_filename, bufr_obs_maxdate, bufr_obs_mindate,            &
       & bufr_tblpath, is_gpsrobufr, is_prepbufr, is_satbufr, mask_land,   &
       & max_orog_hght
  namelist /fcst_mdl/   fv3_dyns_filename, fv3_gridspec_filename,          &
       & fv3_orog_filename, fv3_static_filename, fv3_tracer_filename,      &
       & grid_ratio, is_fv3, is_global, is_regional, is_rotate_winds,      &
       & sample_radius
  namelist /recon/      is_recon_tdr, is_recon_vdm, recon_filelist,        &
       & recon_tdr_filepath, tdr_min_offset_seconds,                       &
       & tdr_max_offset_seconds, tdr_offset_dseconds
  namelist /sonde/      is_sonde_tempdrop, sonde_filelist,                 &
       & tempdrop_compute_drift, tempdrop_hsa_table_file,                  &
       & tempdrop_normalize, tempdrop_write_nc_skewt
  namelist /tc/         is_relocate, tc_radius, tcinfo_filename
  namelist /wmm/        wmm_coeff_filepath
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE: 

  ! namelist.f90

  ! DESCRIPTION: 

  ! This subroutine acts as the interface to the namelist file,
  ! provided as 'obs-preproc.input' by the user.

  !-----------------------------------------------------------------------
  
  subroutine namelist()

    ! Define variables computed within routine

    character(len=500)                                                  :: nml_filename
    logical                                                             :: is_it_there
    integer                                                             :: unit_nml

    ! Define counting variables

    integer                                                             :: i

    !===================================================================== 

    ! Define local variables

    nml_filename = './obs-preproc.input'
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
       read(unit_nml,NML = bufrio)
       read(unit_nml,NML = fcst_mdl)
       read(unit_nml,NML = recon)
       read(unit_nml,NML = sonde)
       read(unit_nml,NML = tc)
       read(unit_nml,NML = wmm)
       close(unit_nml)

       ! Loop through local variable

       do i = 1, size(bufr_obs_filename)

          ! Check local variable and proceed accordingly

          if(trim(adjustl(bufr_obs_filename(i))) .ne. 'NOT USED') then

             ! Define local variables

             nbufr_obs_files = nbufr_obs_files + 1

          end if ! if(trim(adjustl(bufr_obs_filename(i))) .ne. 'NOT
                 ! USED')

       end do ! do i = 1, size(bufr_obs_filename)

    else  ! if(is_it_there)

       ! Define local variables

       write(6,500) trim(adjustl(nml_filename))
       stop(99)
       
    end if ! if(is_it_there)

    ! Define local variables
    
    write(6,*) '&SHARE'
    write(6,*) 'ANALDATE                      = ', analdate
    write(6,*) 'DATAPATH                      = ',                         &
         & trim(adjustl(datapath))
    write(6,*) 'DEBUG                         = ', debug
    write(6,*) 'IS_BUFR_OBS                   = ', is_bufr_obs
    write(6,*) 'IS_FCST_MODEL                 = ', is_fcst_model
    write(6,*) 'IS_RECON                      = ', is_recon
    write(6,*) 'IS_SONDE                      = ', is_sonde
    write(6,*) '/'
    write(6,*) '&BUFRIO'
    write(6,*) 'BUFR_FILEPATH                 = ',                         &
         & trim(adjustl(bufr_filepath))
    write(6,*) 'BUFR_INFO_FILEPATH            = ',                         & 
         & trim(adjustl(bufr_info_filepath))
    write(6,*) 'BUFR_OBS_FILENAME             = '

    ! Loop through local variable

    do i = 1, nbufr_obs_files

       ! Define local variables
       
       write(6,*) trim(adjustl(bufr_obs_filename(i)))

    end do ! do i = 1, nbufr_obs_files

    ! Define local variables

    write(6,*) 'BUFR_OBS_MAXDATE              = ', bufr_obs_maxdate
    write(6,*) 'BUFR_OBS_MINDATE              = ', bufr_obs_mindate
    write(6,*) 'BUFR_TBLPATH                  = ',                         &
         & trim(adjustl(bufr_tblpath))
    write(6,*) 'IS_GPSROBUFR                  = ', is_gpsrobufr
    write(6,*) 'IS_PREPBUFR                   = ', is_prepbufr
    write(6,*) 'IS_SATBUFR                    = ', is_satbufr    
    write(6,*) 'MASK_LAND                     = ', mask_land
    write(6,*) 'MAX_OROG_HGHT                 = ', max_orog_hght
    write(6,*) '/'
    write(6,*) '&FCST_MDL'

    ! Check local variable and proceed accordingly
    
    if(is_global) then

       ! Define local variables

       write(6,*) 'FV3_DYNS_FILENAME             = '

       ! Loop through local variable

       do i = 1, 6
          
          ! Define local variables

          write(6,*) trim(adjustl(fv3_dyns_filename(i)))

       end do ! do i = 1, 6

       ! Define local variables

       write(6,*) 'FV3_OROG_FILENAME             = '

       ! Loop through local variable

       do i = 1, 6
          
          ! Define local variables

          write(6,*) trim(adjustl(fv3_orog_filename(i)))

       end do ! do i = 1, 6

       ! Define local variables
       
       write(6,*) 'FV3_STATIC_FILENAME           = ',                      &
            & trim(adjustl(fv3_static_filename))

       ! Define local variables

       write(6,*) 'FV3_TRACER_FILENAME           = '

       ! Loop through local variable

       do i = 1, 6
          
          ! Define local variables

          write(6,*) trim(adjustl(fv3_tracer_filename(i)))

       end do ! do i = 1, 6       
       
    end if ! if(is_global)

    ! Check local variable and proceed accordingly
    
    if(is_regional) then

       ! Define local variables
       
       write(6,*) 'FV3_DYNS_FILENAME             = ',                      &
            & trim(adjustl(fv3_dyns_filename(1)))
       write(6,*) 'FV3_GRIDSPEC_FILENAME         = ',                      &
            & trim(adjustl(fv3_gridspec_filename(1)))
       write(6,*) 'FV3_OROG_FILENAME             = ',                      &
            & trim(adjustl(fv3_orog_filename(1)))
       write(6,*) 'FV3_STATIC_FILENAME           = ',                      &
            & trim(adjustl(fv3_static_filename))
       write(6,*) 'FV3_TRACER_FILENAME           = ',                      &
            & trim(adjustl(fv3_tracer_filename(1)))
       
    end if ! if(is_regional)

    ! Define local variables
    
    write(6,*) 'GRID_RATIO                    = ', grid_ratio
    write(6,*) 'IS_FV3                        = ', is_fv3
    write(6,*) 'IS_GLOBAL                     = ', is_global
    write(6,*) 'IS_REGIONAL                   = ', is_regional
    write(6,*) 'IS_ROTATE_WINDS               = ', is_rotate_winds
    write(6,*) 'SAMPLE_RADIUS                 = ', sample_radius
    write(6,*) '/'
    write(6,*) '&RECON'
    write(6,*) 'IS_RECON_TDR                  = ', is_recon_tdr
    write(6,*) 'IS_RECON_VDM                  = ', is_recon_vdm
    write(6,*) 'RECON_FILELIST                = ',                         &
         & trim(adjustl(recon_filelist))
    write(6,*) 'RECON_TDR_FILEPATH            = ',                         &
         & trim(adjustl(recon_tdr_filepath))
    write(6,*) 'TDR_MAX_OFFSET_SECONDS        = ', tdr_max_offset_seconds
    write(6,*) 'TDR_MIN_OFFSET_SECONDS        = ', tdr_min_offset_seconds
    write(6,*) 'TDR_OFFSET_DSECONDS           = ', tdr_offset_dseconds
    write(6,*) '/'    
    write(6,*) '&SONDE'
    write(6,*) 'IS_SONDE_TEMPDROP             = ', is_sonde_tempdrop
    write(6,*) 'SONDE_FILELIST                = ',                         &
         & trim(adjustl(sonde_filelist))
    write(6,*) 'TEMPDROP_COMPUTE_DRIFT        = ',                         &
         & tempdrop_compute_drift
    write(6,*) 'TEMPDROP_HSA_TABLE_FILE       = ',                         &
         & trim(adjustl(tempdrop_hsa_table_file))
    write(6,*) 'TEMPDROP_NORMALIZE            = ', tempdrop_normalize
    write(6,*) 'TEMPDROP_WRITE_NC_SKEWT       = ',                         &
         & tempdrop_write_nc_skewt
    write(6,*) '/'
    write(6,*) '&TC'    
    write(6,*) 'IS_RELOCATE                   = ', is_relocate
    write(6,*) 'TC_RADIUS                     = ', tc_radius
    write(6,*) 'TCINFO_FILENAME               = ',                         &
         & trim(adjustl(tcinfo_filename))
    write(6,*) '/'
    write(6,*) '&WMM'
    write(6,*) 'WMM_COEFF_FILEPATH            = ',                         &
         & trim(adjustl(wmm_coeff_filepath))
    write(6,*) '/'
    write(6,*) ''
500 format('NAMELISTPARAMS: ', a, ' not found in the current working ',    &
         & 'directory. ABORTING!!!!')
    
    !=====================================================================

  end subroutine namelist

  !=======================================================================

end module namelist_interface
