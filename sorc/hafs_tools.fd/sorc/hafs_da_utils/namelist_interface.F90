module namelist_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! da-utils :: namelist_interface
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

  ! * bufr_filename; a FORTRAN character string specifying the
  !   full-path to the BUFR-formatted file; it is assumed that this
  !   file adheres to big-endian format.

  ! * bufr_obstypes; a FORTRAN array of integer variables specifying
  !   the observation types to extract from the BUFR-formatted file
  !   (bufr_filename).

  ! * datapath; a FORTRAN character string specifying the full-path to
  !   the directory to contain output files written by the respective
  !   routines.

  ! * debug; a FORTRAN logical value specifying whether to include
  !   debug information during execution.

  ! * gsidiags_convobs_listname; a FORTRAN character string specifying
  !   the full-path to the file containing a list of GSI
  !   diags-formatted files containing conventional-type observations
  !   diagnostics.
  
  ! * gsidiags_convobs_plevs; a FORTRAN array of real-valued variables
  !   specifying the pressure levels within which to bin the GSI
  !   observations for various diagnostic calculations; it is assumed
  !   that the pressure levels are ascending (e.g., the pressure level
  !   nearest the surface is the first entry in the array while the
  !   pressure level highest in the atmosphere is the last); units are
  !   millibars.

  ! * gsidiags_obscodes; a FORTRAN array of integer-valued variables
  !   specifying the observation codes to extract from the GSI
  !   diagnostic-formatted files; if not values are specified, it is
  !   assumed that the user wishes to compute statistics for all
  !   available observation codes for a given observation type.

  ! * gsidiags_regional_latmax; a FORTRAN 4-byte real variable
  !   specifying the maximum latitude value defining the GSI
  !   diagnostics region of interest (used only if
  !   is_gsidiags_regional is True).

  ! * gsidiags_regional_latmin; a FORTRAN 4-byte real variable
  !   specifying the minimum latitude value defining the GSI
  !   diagnostics region of interest (used only if
  !   is_gsidiags_regional is True).

  ! * gsidiags_regional_lonmax; a FORTRAN 4-byte real variable
  !   specifying the maximum longitude value defining the GSI
  !   diagnostics region of interest (used only if
  !   is_gsidiags_regional is True).

  ! * gsidiags_regional_lonmin; a FORTRAN 4-byte real variable
  !   specifying the minimum longitude value defining the GSI
  !   diagnostics region of interest (used only if
  !   is_gsidiags_regional is True).
  
  ! * is_bufrinfo; a FORTRAN logical variable specifying whether to
  !   process the user-specified BUFR-formatted file and extract
  !   attributes for the respective user-specified observation types.

  ! * is_convobs; a FORTRAN logical variable specifying whether to
  !   process conventional-type observations; this assumes that
  !   is_bufrinfo is True and that a corresponding/valid bufr_filename
  !   is provided.

  ! * is_gsiconvdiags; a FORTRAN logical variable specifying whether
  !   to process GSI diags-formatted files for conventional-type
  !   observations.

  ! * is_gsidiags_regional; a FORTRAN logical variable specifying
  !   whether to see GSI diagnostics within a user specified region;
  !   if True, see gsi_diags_regional_latmax,
  !   gsi_diags_regional_latmin, gsi_diags_regional_lonmax, and
  !   gsi_diags_regional_lonmin to specified the region of interest.

  ! * is_ncep_bufr; a FORTRAN logical variable specifying whether the
  !   BUFR-formatted files are produced by the National Centers for
  !   Environmental Prediction (NCEP); the default value is True.

  ! * is_nems2ncdf; a FORTRAN logical variable specifying whether to
  !   output user-specified NEMS variables, from a NEMS-formatted
  !   file, to an external netcdf file in the 'datapath' path.

  ! * is_satobs; a FORTRAN logical variable specifying whether to
  !   process satellite observations; this assumes that is_bufrinfo is
  !   True and that a corresponding/valid satbufr_listname is
  !   provided.

  ! * nems_filename; a FORTRAN character string specifying the path to
  !   the NEMS-formatted file to be processed.

  ! * nems2ncdf_json_vtable; a FORTRAN character string specifying the
  !   path to the JSON-formatted file containing NEMS variables to
  !   write to an external netcdf file.

  ! * satbufr_listname; a FORTRAN character string specifying the
  !   full-path to the file containing a list of satellite observation
  !   BUFR-formatted files.

  !-----------------------------------------------------------------------

  ! Define local variables

  character(len=500)                                                    :: &
       & bufr_filename = 'NOT USED'
  character(len=500)                                                    :: &
       & datapath = './'
  character(len=500)                                                    :: &
       & gsidiags_convobs_listname = 'NOT USED'
  character(len=500)                                                    :: &
       & nems_filename = 'NOT USED' 
  character(len=500)                                                    :: &
       & nems2ncdf_json_vtable = 'NOT USED' 
  character(len=500)                                                    :: &
       & satbufr_listname = 'NOT USED'
  logical                                                               :: &
       & debug = .false.
  logical                                                               :: &
       & is_bufrinfo = .false.
  logical                                                               :: &
       & is_convobs = .false.
  logical                                                               :: &
       & is_gsiconvdiags = .false.
  logical                                                               :: &
       & is_gsidiags_regional = .false.
  logical                                                               :: &
       & is_ncep_bufr = .true.
  logical                                                               :: &
       & is_nems2ncdf = .false. 
  logical                                                               :: &
       & is_satobs = .false.
  real(r_kind)                                                          :: &
       & gsidiags_convobs_plevs(99) = -999.0
  real(r_kind)                                                          :: &
       & gsidiags_regional_latmax = 90.0 
  real(r_kind)                                                          :: &
       & gsidiags_regional_latmin = -90.0
  real(r_kind)                                                          :: &
       & gsidiags_regional_lonmax = 359.9999
  real(r_kind)                                                          :: &
       & gsidiags_regional_lonmin = 0.0
  integer                                                               :: &
       & bufr_obstypes(99) = -999
  integer                                                               :: &
       & gsidiags_obscodes(99) = -9999
  namelist /share/     datapath, debug, is_bufrinfo, is_gsiconvdiags,      &
       & is_nems2ncdf
  namelist /bufrinfo/  bufr_filename, bufr_obstypes, is_convobs,           &
       & is_ncep_bufr, is_satobs, satbufr_listname
  namelist /gsidiags/  gsidiags_convobs_listname, gsidiags_convobs_plevs,  &
       & gsidiags_obscodes, gsidiags_regional_latmax,                      &
       & gsidiags_regional_latmin, gsidiags_regional_lonmax,               &
       & gsidiags_regional_lonmin, is_gsidiags_regional
  namelist /nems2ncdf/ nems_filename, nems2ncdf_json_vtable

  !-----------------------------------------------------------------------

contains
  
  !=======================================================================

  ! SUBROUTINE: 

  ! namelist.f90

  ! DESCRIPTION: 

  ! This subroutine acts as the interface to the namelist file,
  ! provided as 'da-utils.input' by the user.

  !-----------------------------------------------------------------------

  subroutine namelist()

    ! Define variables computed within routine

    character(len=500)                                                  :: nml_filename
    logical                                                             :: is_it_there
    integer                                                             :: unit_nml
    integer                                                             :: nbufr_obstypes
    integer                                                             :: ngsi_obscodes
    integer                                                             :: ngsi_plevs
    
    !=====================================================================
    
    ! Define local variables

    nml_filename = './da-utils.input'
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
       read(unit_nml,NML = bufrinfo)
       read(unit_nml,NML = gsidiags)
       read(unit_nml,NML = nems2ncdf)
       close(unit_nml)

    else  ! if(is_it_there)

       ! Define local variables

       write(6,500) trim(adjustl(nml_filename))
       
    end if ! if(is_it_there)
       
    ! Define local variables

    nbufr_obstypes = count(bufr_obstypes .ne. -999)
    ngsi_obscodes  = count(gsidiags_obscodes .ne. -9999)
    ngsi_plevs     = count(gsidiags_convobs_plevs .ne. -999.0)
    write(6,*) '&SHARE'
    write(6,*) 'DATAPATH                      = ',                         &
         & trim(adjustl(datapath))
    write(6,*) 'DEBUG                         = ', debug
    write(6,*) 'IS_BUFRINFO                   = ', is_bufrinfo
    write(6,*) 'IS_GSICONVDIAGS               = ', is_gsiconvdiags
    write(6,*) 'IS_NEMS2NCDF                  = ', is_nems2ncdf
    write(6,*) '/'
    write(6,*) '&BUFRINFO'
    write(6,*) 'BUFR_FILENAME                 = ',                         &
         & trim(adjustl(bufr_filename))
    write(6,*) 'BUFR_OBSTYPES                 = ',                         &
         & bufr_obstypes(1:nbufr_obstypes)
    write(6,*) 'IS_CONVOBS                    = ', is_convobs
    write(6,*) 'IS_NCEP_BUFR                  = ', is_ncep_bufr
    write(6,*) 'IS_SATOBS                     = ', is_satobs
    write(6,*) 'SATBUFR_LISTNAME              = ',                         &
         & trim(adjustl(satbufr_listname))
    write(6,*) '/'
    write(6,*) '&GSIDIAGS'
    write(6,*) 'GSIDIAGS_CONVOBS_LISTNAME     = ',                         &
         & trim(adjustl(gsidiags_convobs_listname))
    write(6,*) 'GSIDIAGS_CONVOBS_PLEVS        = ',                         &
         & gsidiags_convobs_plevs(1:ngsi_plevs)
    write(6,*) 'GSIDIAGS_OBSCODES             = ',                         &
         & gsidiags_obscodes(1:ngsi_obscodes)
    write(6,*) 'GSIDIAGS_REGIONAL_LATMAX      = ',                         &
         & gsidiags_regional_latmax
    write(6,*) 'GSIDIAGS_REGIONAL_LATMIN      = ',                         &
         & gsidiags_regional_latmin
    write(6,*) 'GSIDIAGS_REGIONAL_LONMAX      = ',                         &
         & gsidiags_regional_lonmax
    write(6,*) 'GSIDIAGS_REGIONAL_LONMIN      = ',                         &
         & gsidiags_regional_lonmin
    write(6,*) 'IS_GSIDIAGS_REGIONAL          = ', is_gsidiags_regional
    write(6,*) '/'
    write(6,*) '&NEMS2NCDF'
    write(6,*) 'NEMS_FILENAME                 = ',                         &
         & trim(adjustl(nems_filename))
    write(6,*) 'NEMS2NCDF_JSON_VTABLE         = ',                         &
         & trim(adjustl(nems2ncdf_json_vtable))
    write(6,*) '/'
    write(6,*) ''
500 format('NAMELISTPARAMS: ', a, ' not found in the current working ',    &
         & 'directory. ABORTING!!!!')

    !=====================================================================

  end subroutine namelist

  !=======================================================================

end module namelist_interface
