# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

##############################################################################
#.rst:
#
# ecbuild_check_fortran
# =====================
#
# Check for Fortran features. ::
#
#   ecbuild_check_fortran( [ FEATURES <feature1> [ <feature2> ... ] ]
#                          [ REQUIRED <feature1> [ <feature2> ... ] ]
#                          [ PRINT ] )
#
# Options
# -------
#
# FEATURES : optional
#   list of optional features to check for
#
# REQUIRED : optional
#   list of required features to check for, fails if not detected
#
# PRINT : optional
#   print a summary of features checked for, found and not found
#
# Note
# ----
#
# If neither ``FEATURES`` nor ``REQUIRED`` are given, check for all features.
#
##############################################################################

function( ecbuild_check_fortran )

  # parse parameters

  set( options PRINT )
  set( single_value_args )
  set( multi_value_args   FEATURES REQUIRED )

  cmake_parse_arguments( _p "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if(_PAR_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ecbuild_check_fortran(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
  endif()

  include( ${ECBUILD_MACROS_DIR}/fortran_features/CheckFortranFeatures.cmake )

  fortran_find_all_features( ALL_FEATURES ) # list all available features to check

  if( NOT _p_FEATURES AND NOT _p_REQUIRED ) # no input, then search for all features

    fortran_feature_check()

  else()

    foreach( _f ${_p_FEATURES} )
      fortran_feature_check( ${_f} )
    endforeach()

    foreach( _f ${_p_REQUIRED} )
      fortran_feature_check( REQUIRED ${_f} )
    endforeach()

  endif()

  if( _p_FEATURES OR _p_REQUIRED )
    set( Fortran_CHECKED_FEATURES ${_p_FEATURES} ${_p_REQUIRED} )
  else()
    set( Fortran_CHECKED_FEATURES ${ALL_FEATURES} )
  endif()

  foreach( f ${Fortran_CHECKED_FEATURES} )
    string( TOUPPER ${f} FEAT )
    if( HAS_Fortran_${FEAT} )
       list( APPEND Fortran_SUPPORTED_FEATURES ${f} )
       set( EC_HAVE_Fortran_${FEAT} 1 PARENT_SCOPE )
    else()
       list( APPEND Fortran_NOT_SUPPORTED_FEATURES ${f} )
       set( EC_HAVE_Fortran_${FEAT} 0 PARENT_SCOPE )
    endif()
  endforeach()

  if( Fortran_CHECKED_FEATURES )
    list( SORT Fortran_CHECKED_FEATURES )
  endif()
  if( Fortran_SUPPORTED_FEATURES )
    list( SORT Fortran_SUPPORTED_FEATURES )
  endif()
  if( Fortran_NOT_SUPPORTED_FEATURES )
    list( SORT Fortran_NOT_SUPPORTED_FEATURES )
  endif()

  set( Fortran_CHECKED_FEATURES       ${Fortran_CHECKED_FEATURES}       PARENT_SCOPE )
  set( Fortran_SUPPORTED_FEATURES     ${Fortran_SUPPORTED_FEATURES}     PARENT_SCOPE )
  set( Fortran_NOT_SUPPORTED_FEATURES ${Fortran_NOT_SUPPORTED_FEATURES} PARENT_SCOPE )

  if( _p_PRINT )
    if( Fortran_CHECKED_FEATURES )
      join( Fortran_CHECKED_FEATURES " " Fortran_CHECKED_FEATURES_STR )
      ecbuild_info( "Checked Fortran features: ${Fortran_CHECKED_FEATURES_STR}" )
    else()
      ecbuild_info( "Checked no Fortran features" )
    endif()
    if( Fortran_SUPPORTED_FEATURES )
      join( Fortran_SUPPORTED_FEATURES " " Fortran_SUPPORTED_FEATURES_STR )
      ecbuild_info( "Found Fortran features: ${Fortran_SUPPORTED_FEATURES_STR}" )
    else()
      ecbuild_info( "Found no Fortran features" )
    endif()
    if( Fortran_NOT_SUPPORTED_FEATURES )
      join( Fortran_NOT_SUPPORTED_FEATURES " " Fortran_NOT_SUPPORTED_FEATURES_STR )
      ecbuild_info( "Not found Fortran features: ${Fortran_NOT_SUPPORTED_FEATURES_STR}" )
    else()
      ecbuild_info( "Found all checked Fortran features" )
    endif()
  endif()

endfunction( ecbuild_check_fortran )
