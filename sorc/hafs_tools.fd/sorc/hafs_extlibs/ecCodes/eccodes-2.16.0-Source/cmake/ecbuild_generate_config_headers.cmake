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
# ecbuild_generate_config_headers
# ===============================
#
# Generates the ecBuild configuration header for the project with the system
# introspection done by CMake. ::
#
#   ecbuild_generate_config_headers( [ DESTINATION <directory> ] )
#
# Options
# -------
#
# DESTINATION : optional
#   installation destination directory
#
##############################################################################

function( ecbuild_generate_config_headers )

  # parse parameters

  set( options )
  set( single_value_args DESTINATION )
  set( multi_value_args  )

  cmake_parse_arguments( _p "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if(_PAR_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ecbuild_generate_config_headers(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
  endif()

  # generate list of compiler flags

  string( TOUPPER ${PROJECT_NAME} PNAME )

  get_property( langs GLOBAL PROPERTY ENABLED_LANGUAGES )

  foreach( lang ${langs} )
    set( EC_${lang}_FLAGS "${CMAKE_${lang}_FLAGS} ${CMAKE_${lang}_FLAGS_${CMAKE_BUILD_TYPE_CAPS}}" )
  endforeach()

  configure_file( ${ECBUILD_MACROS_DIR}/ecbuild_config.h.in  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}_ecbuild_config.h   )

  # install ecbuild configuration

  set( _destination ${INSTALL_INCLUDE_DIR} )
  if( _p_DESTINATION )
    set( _destination ${_p_DESTINATION} )
  endif()

  install( FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}_ecbuild_config.h DESTINATION ${_destination} )

endfunction( ecbuild_generate_config_headers )
