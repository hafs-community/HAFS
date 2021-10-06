module namelist_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: namelist_interface
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

  use kinds_interface

  ! Define interfaces and attributes for module routines
  
  implicit none

  !-----------------------------------------------------------------------

  ! DESCRIPTION (alphabetized):

  ! * adv_time; a FORTRAN 4-byte real valued variable specifying the
  !   integration time-step for the generation of the stochastic
  !   random pattern applied for ensemble member generation; units are
  !   seconds.

  ! * corr_length; a FORTRAN 4-byte real valued variable specifying
  !   the spatial correlation scale for the stochastic random pattern
  !   applied for ensemble member generation; units are meters.

  ! * corr_nmax; a FORTRAN integer specifying the total number of
  !   iterations applied to generate the stochastic random pattern
  !   applied for ensemble member generation.

  ! * corr_stdev; a FORTRAN 4-byte real valued variable specifying the
  !   standard deviation used to define the random (e.g., white-)
  !   noise spectrum for the generation of the stochastic random
  !   pattern applied for ensemble member generation.
  
  ! * corr_tau; a FORTRAN 4-byte real valued variable specifying the
  !   temporal correlation scale for the stochastic random pattern
  !   applied for ensemble member generation; units are seconds.  

  ! * datapath; a FORTRAN character string specifying the full-path to
  !   the directory to contain output files written by the respective
  !   routines.

  ! * debug; a FORTRAN logical value specifying whether to include
  !   debug information during execution.

  ! * domain_blend_ratio; a FORTRAN 4-byte real valued variable
  !   specifying the ratio of the blended nest and parent domains
  !   which contains the nest versus the parent domain; for example, a
  !   value of 0.95 implies that 95% of the total blended region is
  !   defined by the interpolated nested domain while the remaining 5%
  !   is a linear combination (as a function of radius) of the nest
  !   and parent grid solutions.

  ! * ensgen_json_vtable; a FORTRAN character string specifying the
  !   full-path to the JSON-formatted file containing the perturbation
  !   attributes to be applied to variables which is required to
  !   generate ensemble member initial perturbations/uncertainty.

  ! * ensgen_ntrunc; a FORTRAN integer value specifying the spectral
  !   truncation value used to construct the spatially-correlated
  !   white-noise pattern required to generate the perturbations to
  !   the respective state variables described in ensgen_json_vtable
  !   (above).

  ! * ensmem_filename; a FORTRAN character string specifying the
  !   full-path to the external file to be modified in order to
  !   generate ensemble member initial perturbations/uncertainty;
  !   currently only NEMS-formatted files are supported.

  ! * ensmem_pattern_filename; a FORTRAN character string specifying
  !   the full-path to an external file netcdf file containing a
  !   random number pattern (e.g., white noise spectum) to generate
  !   ensemble member initial perturbations/uncertainty.  

  ! * fv3_analysis_filename; a FORTRAN character string specifying the
  !   full-path to the FV3 restart-formatted file containing the
  !   data-assimilation method/algorithm defined/derived "analysis"
  !   state variables.

  ! * fv3_atmos_static_nest_filename; a FORTRAN character string
  !   specifying the full-path to the FV3 file containing the static
  !   atmosphere model fields for the nested-grid tile, namely the
  !   vertical coordinate coefficients.

  ! * fv3_atmos_static_parent_filename; a FORTRAN character string
  !   specifying the full-path to the FV3 file containing the static
  !   atmosphere model fields for the parent tile within which the
  !   nested-grid is embedded, namely the vertical coordinate
  !   coefficients.

  ! * fv3_background_filename; a FORTRAN character string specifying
  !   the full-path to the FV3 restart-formatted file containing the
  !   data-assimilation method/algorithm defined/derived "background"
  !   (e.g., first-guess) state variables.

  ! * fv3_gridspec_filename; a FORTRAN character string specifying the
  !   full-path to the FV3 containing the grid specifications and
  !   attributes corresponding to the variables defined within
  !   fv3_analysis_filename and fv3_background_filename (above).

  ! * fv3_gridspec_nest_filename; a FORTRAN character string
  !   specifying the full-path to the FV3 file containing the
  !   nested-tile grid specifications and attributes.

  ! * fv3_gridspec_parent_filename; a FORTRAN character string
  !   specifying the full-path to the FV3 file containing the parent
  !   tile grid specifications and attributes.

  ! * fv3_presvar_nest_filename; a FORTRAN character string specifying
  !   the full-path to the file containing the FV3 nested-tile
  !   variable 'delp' values; this is typically a FV3
  !   restart-formatted file.

  ! * fv3_presvar_parent_filename; a FORTRAN character string
  !   specifying the full-path to the file containing the FV3 parent
  !   tile variable 'delp' values; this is typically a FV3
  !   restart-formatted file.
  
  ! * fv3_var_nest_filename; a FORTRAN character string specifying the
  !   full-path to the file containing the FV3 nested-tile variable
  !   values; this is typically a FV3 restart-formatted file.

  ! * fv3_var_parent_filename; a FORTRAN character string specifying
  !   the full-path to the file containing the FV3 parent tile
  !   variable values; this is typically a FV3 restart-formatted file.

  ! * is_bcupdate; a FORTRAN logical variable specifying whether the
  !   data-assimilation defined/defived increment values are to be
  !   relaxed to a value of zero in accordance with the user specified
  !   relaxation region (see npad_cells and nrelax_cells, below).

  ! * is_ensgen; a FORTRAN logical variable specifying whether to
  !   generate ensemble member initial perturbations/uncertainty (see
  !   ensgen_json_vtable and ensmem_filename); currently only
  !   NEMS-formatted files are supported.

  ! * is_ensgen_regional; a FORTRAN logical variable specifying
  !   whether to localize perturbations relative to a specified
  !   location; currently this is supported only for tropical cyclone
  !   (TC) locations (see tcv_filename).  
  
  ! * is_fv3; a FORTRAN logical variable specifying whether the
  !   forecast model is the Finite-Volume Cubed-Sphere (FV3).

  ! * is_fv3_tracers; a FORTRAN logical variable specifying whether
  !   the FV3 files contain the tracer variables; this check is
  !   required since the netcdf file grid dimension variable names and
  !   availability change as a function of FV3 variable-type files and
  !   for tracer variables, there are no horizontally staggered
  !   fields.

  ! * is_interp_llp; a FORTRAN logical variable specifying whether to
  !   use linear-log methods to remap/interpolate profile variable
  !   values to a pressure level coordinate.

  ! * is_merge; a FORTRAN logical variable specifying whether the
  !   algorithms are to be applied to merge user specified FV3
  !   variables from nested-grid forecast solutions into the
  !   corresponding parent-tile forecast solutions.

  ! * is_nemsio; a FORTRAN logical variable specifying whether the
  !   external file for the ensemble member generation routines is of
  !   NEMS-format; this is (currently) the only supported option (see
  !   is_ensgen, ensgen_json_vtable, and ensmem_filename).  

  ! * is_read_pattern; a FORTRAN logical variable specifying whether
  !   to read in a previously created random-number pattern file
  !   required to generate a given ensemble member.

  ! * is_remap_pres; a FORTRAN logical variable specifying whether to
  !   remap/interpolate the profile variables to the new pressure
  !   level coordinate; this applies to only nest-to-parent merging
  !   applications.

  ! * is_slint; a FORTRAN logical variable specifying whether the
  !   remapping coefficients are defined via the SLINT interpolation
  !   algorithm (Wang, 2006); currently this is the default and is not
  !   able to be modified by the user.

  ! * json_fv3var_filename; a FORTRAN character string specifying the
  !   full-path to the JSON-formatted file containing the FV3 variable
  !   attributes.

  ! * npad_cells; a FORTRAN integer value specifying the number of
  !   cells, relative to the nested domain size, across which all
  !   data-assimilation defined/derived increment values are set to
  !   zero.

  ! * nrelax_cells; a FORTRAN integer value specifying the number of
  !   grid cells across which to relax the data-assimilation
  !   defined/derived increments to zero.

  ! * rng_seed; a FORTRAN integer specifying the random number seed
  !   applied to generate the the stochastic random pattern applied
  !   for ensemble member generation.

  ! * tc_region_area; a FORTRAN 4-byte real valued variable specifying
  !   the size of the region, relative to a TC-vitals record, which to
  !   merge into a different (i.e., parent) domain; units are degrees.

  ! * tcv_filename; a FORTRAN character string specifying the
  !   full-path to the TC-vitals formatted file.
  
  !-----------------------------------------------------------------------

  ! Define local variables

  character(len=500)                                                    :: &
       & datapath = './'
  character(len=500)                                                    :: &
       & ensgen_json_vtable = 'NOT USED' 
  character(len=500)                                                    :: &
       & ensmem_filename = 'NOT USED'
  character(len=500)                                                    :: &
       & ensmem_pattern_filename = 'NOT USED'   
  character(len=500)                                                    :: &
       & fv3_analysis_filename = 'NOT USED'
  character(len=500)                                                    :: &
       & fv3_atmos_static_nest_filename = 'NOT USED' ! NEED
  character(len=500)                                                    :: &
       & fv3_atmos_static_parent_filename = 'NOT USED' ! NEED
  character(len=500)                                                    :: &
       & fv3_background_filename = 'NOT USED'
  character(len=500)                                                    :: &
       & fv3_gridspec_filename = 'NOT USED'
  character(len=500)                                                    :: &
       & fv3_gridspec_nest_filename = 'NOT USED'
  character(len=500)                                                    :: &
       & fv3_gridspec_parent_filename = 'NOT USED' 
  character(len=500)                                                    :: &
       & fv3_presvar_nest_filename = 'NOT USED' ! NEED
  character(len=500)                                                    :: &
       & fv3_presvar_parent_filename = 'NOT USED' ! NEED
  character(len=500)                                                    :: &
       & fv3_var_nest_filename = 'NOT USED'
  character(len=500)                                                    :: &
       & fv3_var_parent_filename = 'NOT USED'
  character(len=500)                                                    :: &
       & json_fv3var_filename = 'NOT USED'
  character(len=500)                                                    :: &
       & tcv_filename = 'NOT USED'
  logical                                                               :: &
       & debug = .false.
  logical                                                               :: &
       & is_bcupdate = .false.
  logical                                                               :: &
       & is_ensgen = .false.
  logical                                                               :: &
       & is_ensgen_regional = .false.
  logical                                                               :: &
       & is_fv3 = .false.
  logical                                                               :: &
       & is_fv3_tracers = .false.
  logical                                                               :: &
       & is_interp_llp = .false.
  logical                                                               :: &
       & is_merge = .false.
  logical                                                               :: &
       & is_nemsio = .false.   
  logical                                                               :: &
       & is_read_pattern = .true.
  logical                                                               :: &
       & is_remap_pres = .false. ! NEED
  logical                                                               :: &
       & is_slint = .true.
  real(r_kind)                                                          :: &
       & adv_dtime = 3600.0 
  real(r_kind)                                                          :: &
       & corr_length = 500.0e3 
  real(r_kind)                                                          :: &
       & corr_stdev = 1.0   
  real(r_kind)                                                          :: &
       & corr_tau = 21600.0 
  real(r_kind)                                                          :: &
       & domain_blend_ratio = 1.0
  real(r_kind)                                                          :: &
       & tc_region_area = 11.0  
  integer                                                               :: &
       & corr_nmax = 100
  integer                                                               :: &
       & ensgen_ntrunc = 1534
  integer                                                               :: &
       & npad_cells = 1
  integer                                                               :: &
       & nrelax_cells = 10
  integer                                                               :: &
       & rng_seed = 1234 
  namelist /share/    debug, datapath, is_bcupdate, is_bcupdate,           &
       & is_ensgen, is_fv3, is_merge, is_nemsio
  namelist /bcupdate/ npad_cells, nrelax_cells
  namelist /ensgen/   adv_dtime, corr_length, corr_nmax, corr_stdev,       &
       & corr_tau, ensgen_json_vtable, ensgen_ntrunc, ensmem_filename,     &
       & ensmem_pattern_filename, is_ensgen_regional, is_read_pattern,     &
       & rng_seed   
  namelist /fv3/      fv3_analysis_filename,                               &
       & fv3_atmos_static_nest_filename, fv3_atmos_static_parent_filename, &
       & fv3_background_filename, fv3_gridspec_filename,                   &
       & fv3_gridspec_nest_filename, fv3_gridspec_parent_filename,         &
       & fv3_presvar_nest_filename, fv3_presvar_parent_filename,           &
       & fv3_var_nest_filename, fv3_var_parent_filename, is_fv3_tracers,   &
       & json_fv3var_filename
  namelist /interp/   domain_blend_ratio, is_interp_llp, is_remap_pres,    &
       & is_slint
  namelist /tc/       tc_region_area, tcv_filename
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE: 

  ! namelist.f90

  ! DESCRIPTION: 

  ! This subroutine acts as the interface to the namelist file,
  ! provided as 'analysis-update.input' by the user.

  !-----------------------------------------------------------------------

  subroutine namelist()

    ! Define variables computed within routine

    character(len=500)                                                  :: nml_filename
    logical                                                             :: is_it_there
    integer                                                             :: unit_nml

    !=====================================================================
    
    ! Define local variables

    nml_filename = './analysis-update.input'
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
       read(unit_nml,NML = bcupdate)
       read(unit_nml,NML = ensgen)
       read(unit_nml,NML = fv3)
       read(unit_nml,NML = interp)
       read(unit_nml,NML = tc)
       close(unit_nml)

    else  ! if(is_it_there)

       ! Define local variables

       write(6,500) trim(adjustl(nml_filename))
       stop(99)
       
    end if ! if(is_it_there)

    ! Define local variables
    
    write(6,*) '&SHARE'                 
    write(6,*) 'DATAPATH                         = ',                      &
         & trim(adjustl(datapath))
    write(6,*) 'DEBUG                            = ', debug
    write(6,*) 'IS_BCUPDATE                      = ', is_bcupdate
    write(6,*) 'IS_FV3                           = ', is_fv3
    write(6,*) 'IS_MERGE                         = ', is_merge
    write(6,*) '/'
    write(6,*) '&BCUPDATE'
    write(6,*) 'NPAD_CELLS                       = ', npad_cells
    write(6,*) 'NRELAX_CELLS                     = ', nrelax_cells
    write(6,*) '/'
    write(6,*) '&ENSGEN'
    write(6,*) 'ADV_DTIME                        = ', adv_dtime
    write(6,*) 'CORR_LENGTH                      = ', corr_length
    write(6,*) 'CORR_NMAX                        = ', corr_nmax
    write(6,*) 'CORR_STDEV                       = ', corr_stdev
    write(6,*) 'CORR_TAU                         = ', corr_tau
    write(6,*) 'ENSGEN_JSON_VTABLE               = ',                      &
         & trim(adjustl(ensgen_json_vtable))
    write(6,*) 'ENSGEN_NTRUNC                    = ', ensgen_ntrunc
    write(6,*) 'ENSMEM_FILENAME                  = ',                      &
         & trim(adjustl(ensmem_filename))
    write(6,*) 'ENSMEM_PATTERN_FILENAME          = ',                      &
         & trim(adjustl(ensmem_pattern_filename))
    write(6,*) 'IS_ENSGEN_REGIONAL               = ', is_ensgen_regional
    write(6,*) 'IS_READ_PATTERN                  = ', is_read_pattern
    write(6,*) 'RNG_SEED                         = ', rng_seed
    write(6,*) '/'    
    write(6,*) '&FV3'
    write(6,*) 'FV3_ANALYSIS_FILENAME            = ',                      &
         & trim(adjustl(fv3_analysis_filename))
    write(6,*) 'FV3_ATMOS_STATIC_NEST_FILENAME   = ',                      &
         & trim(adjustl(fv3_atmos_static_nest_filename))
    write(6,*) 'FV3_ATMOS_STATIC_PARENT_FILENAME = ',                      &
         & trim(adjustl(fv3_atmos_static_parent_filename))
    write(6,*) 'FV3_BACKGROUND_FILENAME          = ',                      &
         & trim(adjustl(fv3_background_filename))
    write(6,*) 'FV3_GRIDSPEC_FILENAME            = ',                      &
         & trim(adjustl(fv3_gridspec_filename))
    write(6,*) 'FV3_GRIDSPEC_NEST_FILENAME       = ',                      &
         & trim(adjustl(fv3_gridspec_nest_filename))
    write(6,*) 'FV3_GRIDSPEC_PARENT_FILENAME     = ',                      &
         & trim(adjustl(fv3_gridspec_parent_filename))
    write(6,*) 'FV3_PRESVAR_NEST_FILENAME        = ',                      &
         & trim(adjustl(fv3_presvar_nest_filename))
    write(6,*) 'FV3_PRESVAR_PARENT_FILENAME      = ',                      &
         & trim(adjustl(fv3_presvar_parent_filename))
    write(6,*) 'FV3_VAR_NEST_FILENAME            = ',                      &
         & trim(adjustl(fv3_var_nest_filename))
    write(6,*) 'FV3_VAR_PARENT_FILENAME          = ',                      &
         & trim(adjustl(fv3_var_parent_filename))
    write(6,*) 'IS_FV3_TRACERS                   = ', is_fv3_tracers
    write(6,*) 'JSON_FV3VAR_FILENAME             = ',                      &
         & trim(adjustl(json_fv3var_filename))
    write(6,*) '/'    
    write(6,*) '&INTERP'
    write(6,*) 'DOMAIN_BLEND_RATIO               = ', domain_blend_ratio
    write(6,*) 'IS_INTERP_LLP                    = ', is_interp_llp
    write(6,*) 'IS_REMAP_PRES                    = ', is_remap_pres
    write(6,*) 'IS_SLINT                         = ', is_slint
    write(6,*) '/'
    write(6,*) '&TC'  
    write(6,*) 'TC_REGION_AREA                   = ', tc_region_area
    write(6,*) 'TCV_FILENAME                     = ',                      &
         & trim(adjustl(tcv_filename))
    write(6,*) '/'
    write(6,501)
500 format('NAMELISTPARAMS: ', a, ' not found in the current working ',    &
         & 'directory. ABORTING!!!!')
501 format(/)
    
    !=====================================================================

  end subroutine namelist

  !=======================================================================    
       
end module namelist_interface
