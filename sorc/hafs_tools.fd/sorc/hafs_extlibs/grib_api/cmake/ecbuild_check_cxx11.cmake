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
# ecbuild_check_cxx11
# ===================
#
# Check for C++11 features. ::
#
#   ecbuild_check_cxx11( [ FEATURES <feature1> [ <feature2> ... ] ]
#                        [ REQUIRED <feature1> [ <feature2> ... ] ]
#                        [ PRINT ] )
#
# This function uses macros from http://github.com/UCL/GreatCMakeCookOff
#
# Options
# -------
#
# FEATURES : optional
#   list of optional features to check for
#
# REQUIRED : optional
#   list of required features to check for
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

function( ecbuild_check_cxx11 )

  # parse parameters

  set( options PRINT )
  set( single_value_args )
  set( multi_value_args   FEATURES REQUIRED )

  cmake_parse_arguments( _p "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if(_PAR_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ecbuild_check_cxx11(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
  endif()

  include( ${ECBUILD_MACROS_DIR}/contrib/GreatCMakeCookOff/CheckCXX11Features.cmake )

  cxx11_find_all_features( ALL_FEATURES ) # list all available features to check

  # Save CXX flags
  set( CXX_FLAGS_SNAPSHOT ${CMAKE_CXX_FLAGS} )

  # Add C++11 flags
  include( ${ECBUILD_MACROS_DIR}/ecbuild_get_cxx11_flags.cmake )
  ecbuild_get_cxx11_flags( CXX11_FLAGS )
  ecbuild_debug( "ecbuild_check_cxx11: detected C++11 flag as ${CXX11_FLAGS}" )
  set( CMAKE_CXX_FLAGS "${CXX11_FLAGS} ${CMAKE_CXX_FLAGS}" )

  if( NOT _p_FEATURES AND NOT _p_REQUIRED ) # no input, then search for all features

    cxx11_feature_check()

  else()

    foreach( _f ${_p_FEATURES} )
      cxx11_feature_check( ${_f} )
    endforeach()

    foreach( _f ${_p_REQUIRED} )
      cxx11_feature_check( REQUIRED ${_f} )
    endforeach()

  endif()

  # Restore CXX flags
  set( CMAKE_CXX_FLAGS ${CXX_FLAGS_SNAPSHOT} )

  if( _p_FEATURES OR _p_REQUIRED )
    set( CXX11_CHECKED_FEATURES ${_p_FEATURES} ${_p_REQUIRED} )
  else()
    set( CXX11_CHECKED_FEATURES ${ALL_FEATURES} )
  endif()

  foreach( f ${CXX11_CHECKED_FEATURES} )
    string( TOUPPER ${f} FEAT )
    if( HAS_CXX11_${FEAT} )
       list( APPEND CXX11_SUPPORTED_FEATURES ${f} )
    else()
       list( APPEND CXX11_NOT_SUPPORTED_FEATURES ${f} )
    endif()
  endforeach()

  if( CXX11_CHECKED_FEATURES )
    list( SORT CXX11_CHECKED_FEATURES )
  endif()
  if( CXX11_SUPPORTED_FEATURES )
    list( SORT CXX11_SUPPORTED_FEATURES )
  endif()
  if( CXX11_NOT_SUPPORTED_FEATURES )
    list( SORT CXX11_NOT_SUPPORTED_FEATURES )
  endif()

  set( CXX11_CHECKED_FEATURES       ${CXX11_CHECKED_FEATURES}       PARENT_SCOPE )
  set( CXX11_SUPPORTED_FEATURES     ${CXX11_SUPPORTED_FEATURES}     PARENT_SCOPE )
  set( CXX11_NOT_SUPPORTED_FEATURES ${CXX11_NOT_SUPPORTED_FEATURES} PARENT_SCOPE )

  if( _p_PRINT )
    if( CXX11_CHECKED_FEATURES )
      join( CXX11_CHECKED_FEATURES " " CXX11_CHECKED_FEATURES_STR )
      ecbuild_info( "Checked C++11 features: ${CXX11_CHECKED_FEATURES_STR}" )
    else()
      ecbuild_info( "Checked no C++11 features" )
    endif()
    if( CXX11_SUPPORTED_FEATURES )
      join( CXX11_SUPPORTED_FEATURES " " CXX11_SUPPORTED_FEATURES_STR )
      ecbuild_info( "Found C++11 features: ${CXX11_SUPPORTED_FEATURES_STR}" )
    else()
      ecbuild_info( "Found no C++11 features" )
    endif()
    if( CXX11_NOT_SUPPORTED_FEATURES )
      join( CXX11_NOT_SUPPORTED_FEATURES " " CXX11_NOT_SUPPORTED_FEATURES_STR )
      ecbuild_info( "Not found C++11 features: ${CXX11_NOT_SUPPORTED_FEATURES_STR}" )
    else()
      ecbuild_info( "Found all checked C++11 features" )
    endif()
  endif()

endfunction( ecbuild_check_cxx11 )
