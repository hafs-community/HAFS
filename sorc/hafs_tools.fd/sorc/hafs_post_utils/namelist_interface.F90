module namelist_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! post-utils :: namelist_interface
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

  use kinds_interface

  ! Define interfaces and attributes for module routines
  
  implicit none

  !-----------------------------------------------------------------------

  ! DESCRIPTION (alphabetized):

  ! * analdate; a FORTRAN character string specifying the analysis
  !   date string, formatted (assuming the UNIX convention) as
  !   'ccyy-mm-dd_HH:MM:SS'.

  ! * cen_lat; a FORTRAN 4-byte real value specifying the center
  !   latitude coordinate for the remapping domain; units are degrees.

  ! * cen_lon; a FORTRAN 4-byte real value specifying the center
  !   longitude coordinate for the remapping domain; units are
  !   degrees.

  ! * datapath; a FORTRAN character string specifying the full-path to
  !   the directory to contain output files written by the respective
  !   routines.

  ! * debug; a FORTRAN logical value specifying whether to include
  !   debug information during execution.

  ! * dlat; a FORTRAN 4-byte real value specifying the latitudinal
  !   resolution of the remapping domain; units are degrees.

  ! * dlon; a FORTRAN 4-byte real value specifying the longitudinal
  !   resolution of the remapping domain; units are degrees.

  ! * fv3_dtp; a FORTRAN 4-byte real value specifying the FV3 forecast
  !   model timestep for the physics parameterizations; this
  !   information is used by the NCEP UPP for some applications.

  ! * fv3_dyns_filename; an array, of dimension 6, of FORTRAN
  !   character strings specifying the full path to the netcdf files
  !   containing the FV3 dynamical variables; the ordering of the tile
  !   names must correspond to the ordering of the file names defining
  !   the FV3 grid (see fv3_grid_filename).
  
  ! * fv3_grid_filename; a FORTRAN character string array specifying
  !   the full-path to the netcdf file(s) describing the FV3 grid;
  !   this variable may contain an array of filenames, each
  !   corresponding to a respective FV3 tile if 'is_global' is 'true'.

  ! * fv3_imp_physics; a FORTRAN integer specifying the FV3
  !   micro-physics parameterization namelist identifier; this
  !   information is used by the NCEP UPP for some applications.

  ! * fv3_input_filename; a FORTRAN character string specifying the
  !   full-path to the netcdf file(s) containing the FV3 variables;
  !   this variable may contain an array of filenames, each
  !   corresponding to a respective FV3 tile if 'is_global' is 'true'.

  ! * fv3_json_vtable_filename; a FORTRAN character string specifying
  !   the full-path to the JSON formatted file containing the FV3
  !   variables to be post-processed.

  ! * fv3_ncld; a FORTRAN integer specifying the number of cloud types
  !   parameterized by the FV3 forecast model; this information is
  !   used by the NCEP UPP for some applications.

  ! * fv3_nsoil; a FORTRAN integer specifying the number of soil
  !   layers utilized by the FV3 forecast model; this information is
  !   used by the NCEP UPP for some applications.

  ! * fv3_output_filename; a FORTRAN character string specifying the
  !   full-path to the file containing the remapped variables from the
  !   FV3 input files, projected upon the grid specified by the
  !   user.

  ! * fv3_phys_filename; an array, of dimension 6, of FORTRAN
  !   character strings specifying the full path to the netcdf files
  !   containing the FV3 physics variables; the ordering of the tile
  !   names must correspond to the ordering of the file names defining
  !   the FV3 grid (see fv3_grid_filename).

  ! * is_fv3; a FORTRAN logical value specifying whether the
  !   operations within are to be performed on FV3 formatted (e.g.,
  !   netcdf) files.

  ! * is_global; a FORTRAN logical variable specifying whether the
  !   ingested analysis variables are from a global forecast model.

  ! * is_read_remap; a FORTRAN logical variable specifying whether to
  !   read the interpolation method remapping attributes from an
  !   existing external file rather than re-computing them; see
  !   remap_filename.

  ! * is_regional; a FORTRAN logical variable specifying whether the
  !   ingested analysis variables are from a regional forecast model.

  ! * is_rezero_time; a FORTRAN logical variable specifying whether to
  !   set the forecast time to '0 hours' relative to the analysis date
  !   timestamp; see analdate.

  ! * is_slint; a FORTRAN logical variable specifying whether the
  !   remapping coefficients are defined via the SLINT interpolation
  !   algorithm (Wang, 2006); currently this is the default and is not
  !   able to be modified by the user.

  ! * is_upp; a FORTRAN logicial variable specifying whether the
  !   output files are to be formatted for ingestion by the NCEP UPP.

  ! * is_write_remap; a FORTRAN logical variable specifying whether to
  !   write the respective user-specified interpolation method
  !   remapping attributes to an external file after computation; see
  !   remap_filename; this method is adventageous as it reduces the
  !   computational load related to the calculation of the
  !   interpolation remapping attributes.

  ! * lat1; a FORTRAN 4-byte real value specifying the number of
  !   degrees latitude for the southern-most boundary of the remapping
  !   domain relative to the central latitude (see cen_lat); this
  !   applies to only regional forecast model remappings (see
  !   is_regional); units are degrees.

  ! * lat2; a FORTRAN 4-byte real value specifying the number of
  !   degrees latitude for the northern-most boundary of the remapping
  !   domain relative to the central latitude (see cen_lat); this
  !   applies to only regional forecast model remappings (see
  !   is_regional); units are degrees.

  ! * lon1; a FORTRAN 4-byte real value specifying the number of
  !   degrees longitude for the western-most boundary of the remapping
  !   domain relative to the central longitude (see cen_lon); this
  !   applies to only regional forecast model remappings (see
  !   is_regional); units are degrees.

  ! * lon2; a FORTRAN 4-byte real value specifying the number of
  !   degrees longitude for the eastern-most boundary of the remapping
  !   domain relative to the central longitude (see cen_lon); this
  !   applies to only regional forecast model remappings (see
  !   is_regional); units are degrees.

  ! * jcap; a FORTRAN integer specifying the spectral truncation for
  !   the remapping grid; this applies only for global model forecasts
  !   (see is_global) and is used to define the remapping projection.

  ! * remap_filename; a FORTRAN character string specifying the
  !   full-path to the file containing the user-specified respective
  !   interpolation method remapping attributes; currently, only SLINT
  !   supported.

  !-----------------------------------------------------------------------

  ! Define local variables

  character(len=500)                                                    :: &
       & fv3_grid_filename(6) = 'NOT USED'
  character(len=500)                                                    :: &
       & fv3_dyns_filename(6) = 'NOT USED'   
  character(len=500)                                                    :: &
       & fv3_phys_filename(6) = 'NOT USED'   
  character(len=500)                                                    :: &
       & datapath = './'
  character(len=500)                                                    :: &
       & fv3_input_filename = 'NOT USED'  
  character(len=500)                                                    :: &
       & fv3_json_vtable = 'NOT USED'
  character(len=500)                                                    :: &
       & fv3_output_filename = 'NOT USED'
  character(len=500)                                                    :: &
       & fv3_static_filename = 'NOT USED'
  character(len=500)                                                    :: &
       & remap_filename = 'remapping.nc'
  character(len=19)                                                     :: &
       & analdate = '2000-01-01_00:00:00'
  logical                                                               :: &
       & debug = .false.
  logical                                                               :: &
       & is_fv3 = .false.
  logical                                                               :: &
       & is_global = .false.
  logical                                                               :: &
       & is_read_remap = .false.
  logical                                                               :: &
       & is_regional = .false.
  logical                                                               :: &
       & is_rezero_time = .false.
  logical                                                               :: &
       & is_slint = .true. ! This is the only supported interpolation
                           ! library at the moment
  logical                                                               :: &
       & is_upp = .false.
  logical                                                               :: &
       & is_write_remap = .false.
  real(r_kind)                                                          :: &
       & cen_lat = 22.0
  real(r_kind)                                                          :: &
       & cen_lon = -64.0
  real(r_kind)                                                          :: &
       & dlat = 0.25
  real(r_kind)                                                          :: &
       & dlon = 0.25
  real(r_kind)                                                          :: &
       & fv3_dtp = 900.0
  real(r_kind)                                                          :: &
       & jcap = 62
  real(r_kind)                                                          :: &
       & lat1 = -10.0 
  real(r_kind)                                                          :: &
       & lat2 = 10.0 
  real(r_kind)                                                          :: &
       & lon1 = -10.0 
  real(r_kind)                                                          :: &
       & lon2 = 10.0 
  integer                                                               :: &
       & fv3_imp_physics = 11
  integer                                                               :: &
       & fv3_ncld = 5
  integer                                                               :: &
       & fv3_nsoil = 4
  namelist /share/    analdate, datapath, debug, is_fv3, is_global,        &
       & is_regional, is_rezero_time, is_upp
  namelist /fv3/      fv3_dtp, fv3_dyns_filename, fv3_phys_filename,       &
       & fv3_grid_filename, fv3_input_filename, fv3_imp_physics,           &
       & fv3_json_vtable, fv3_ncld, fv3_nsoil, fv3_static_filename,        &
       & fv3_output_filename
  namelist /remap/    cen_lat, cen_lon, dlat, dlon, is_read_remap,         &
       & is_slint, is_write_remap, jcap, lat1, lat2, lon1, lon2,           &
       & remap_filename

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE: 

  ! namelist.f90

  ! DESCRIPTION: 

  ! This subroutine acts as the interface to the namelist file,
  ! provided as 'post-utils.input' by the user.

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

    nml_filename = './post-utils.input'
    unit_nml     = 9
    is_it_there  = .false.
    inquire(file = trim(adjustl(nml_filename)),exist = is_it_there)

    ! Check local variable and proceed accordingly

    if(is_it_there) then

       ! Define local variables

       open(file   = trim(adjustl(nml_filename)),                          &
            unit   = unit_nml,                                             &
            status = 'old',                                                &
            form   = 'formatted',                                          &
            action = 'read')
       read(unit_nml,NML = share)
       read(unit_nml,NML = fv3)
       read(unit_nml,NML = remap)
       close(unit_nml)

    else  ! if(is_it_there)

       ! Define local variables

       write(6,500) trim(adjustl(nml_filename))
       
    end if ! if(is_it_there)
       
    ! Define local variables
    
    write(6,*) '&SHARE'
    write(6,*) 'ANALDATE                      = ', analdate
    write(6,*) 'DATAPATH                      = ',                         &
         & trim(adjustl(datapath))
    write(6,*) 'DEBUG                         = ', debug
    write(6,*) 'IS_FV3                        = ', is_fv3
    write(6,*) 'IS_GLOBAL                     = ', is_global
    write(6,*) 'IS_REGIONAL                   = ', is_regional
    write(6,*) 'IS_REZERO_TIME                = ', is_rezero_time
    write(6,*) 'IS_UPP                        = ', is_upp
    write(6,*) '/'
    write(6,*) '&FV3'
    write(6,*) 'FV3_DTP                       = ', fv3_dtp
    write(6,*) 'FV3_DYNS_FILENAME             = '
    do i = 1, size(fv3_dyns_filename)
       write(6,*) trim(adjustl(fv3_dyns_filename(i)))
    end do ! do i = 1, size(fv3_dyns_filename)
    write(6,*) 'FV3_GRID_FILENAME             = '
    do i = 1, size(fv3_grid_filename)
       write(6,*) trim(adjustl(fv3_grid_filename(i)))
    end do ! do i = 1, size(fv3_grid_filename)
    write(6,*) 'FV3_PHYS_FILENAME             = '
    do i = 1, size(fv3_phys_filename)
       write(6,*) trim(adjustl(fv3_phys_filename(i)))
    end do ! do i = 1, size(fv3_phys_filename)
    write(6,*) 'FV3_INPUT_FILENAME            = ',                         &
         & trim(adjustl(fv3_input_filename))
    write(6,*) 'FV3_IMP_PHYSICS               = ', fv3_imp_physics
    write(6,*) 'FV3_JSON_VTABLE               = ',                         &
         & trim(adjustl(fv3_json_vtable))
    write(6,*) 'FV3_NCLD                      = ', fv3_ncld
    write(6,*) 'FV3_NSOIL                     = ', fv3_nsoil
    write(6,*) 'FV3_STATIC_FILENAME           = ',                         &
         & trim(adjustl(fv3_static_filename))
    write(6,*) 'FV3_OUTPUT_FILENAME           = ',                         &
         & trim(adjustl(fv3_output_filename))
    write(6,*) '/'
    write(6,*) '&REMAP'
    write(6,*) 'CEN_LAT                       = ', cen_lat
    write(6,*) 'CEN_LON                       = ', cen_lon
    write(6,*) 'DLAT                          = ', dlat
    write(6,*) 'DLON                          = ', dlon
    write(6,*) 'IS_READ_REMAP                 = ', is_read_remap
    write(6,*) 'IS_SLINT                      = ', is_slint
    write(6,*) 'IS_WRITE_REMAP                = ', is_write_remap
    write(6,*) 'JCAP                          = ', jcap
    write(6,*) 'LAT1                          = ', lat1
    write(6,*) 'LAT2                          = ', lat2
    write(6,*) 'LON1                          = ', lon1
    write(6,*) 'LON2                          = ', lon2
    write(6,*) 'REMAP_FILENAME                = ',                         &
         & trim(adjustl(remap_filename))
    write(6,*) '/'
500 format('NAMELISTPARAMS: ', a, ' not found in the current working ',    &
         & 'directory. ABORTING!!!!')

    !=====================================================================

  end subroutine namelist

  !=======================================================================

end module namelist_interface
