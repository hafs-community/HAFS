# (C) Copyright 2019- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

get_property( _ECBUILD_PROJECT_INCLUDED GLOBAL PROPERTY ECBUILD_PROJECT_INCLUDED SET )
if( NOT _ECBUILD_PROJECT_INCLUDED )
set_property( GLOBAL PROPERTY ECBUILD_PROJECT_INCLUDED TRUE )


# XXX: CMake apparently parses the main CMakeLists.txt looking for a direct call
# to project(), which means we cannot just create an ecbuild_project, hence the
# override.
macro( project _project_name )

  if( ECBUILD_PROJECT_${CMAKE_CURRENT_SOURCE_DIR} OR ECBUILD_PROJECT_${_project_name} )

    include( CMakeParseArguments )
    include( ecbuild_parse_version )
    include( ecbuild_log )
 
    ecbuild_debug( "ecbuild project(${_project_name}) ")

    set( options "" )
    set( oneValueArgs VERSION )
    set( multiValueArgs "" )

    cmake_parse_arguments( _ecbuild_${_project_name} "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

    set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} )

    if( _ecbuild_${_project_name}_VERSION )
      ecbuild_parse_version( "${_ecbuild_${_project_name}_VERSION}" PREFIX ${_project_name} )
    elseif( EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/VERSION )
      ecbuild_parse_version_file( "VERSION" PREFIX ${_project_name} )
    else()
      ecbuild_critical("Please specify a version for project ${_project_name}")
    endif()

    cmake_policy(PUSH)
    cmake_minimum_required(VERSION 3.3 FATAL_ERROR) # for using IN_LIST
    cmake_policy(SET CMP0057 NEW) # for using IN_LIST

    unset( _require_LANGUAGES )
    foreach( _lang C CXX Fortran )
      if( ${_lang} IN_LIST _ecbuild_${_project_name}_UNPARSED_ARGUMENTS )
        set( _require_LANGUAGES TRUE )
      endif()
    endforeach()
    if( _require_LANGUAGES AND NOT "LANGUAGES" IN_LIST _ecbuild_${_project_name}_UNPARSED_ARGUMENTS )
      if(ECBUILD_2_COMPAT)
        if(ECBUILD_2_COMPAT_DEPRECATE)
          ecbuild_deprecate( "Please specify LANGUAGES keyword in project()" )
        endif()
      else()
        ecbuild_critical( "Please specify LANGUAGES keyword in project()" )
      endif()
      list( INSERT _ecbuild_${_project_name}_UNPARSED_ARGUMENTS 0 "LANGUAGES" )
    endif()

    cmake_policy(POP)

    if( ${_project_name}_VERSION_STR )
      cmake_policy(SET CMP0048 NEW )
      _project( ${_project_name} VERSION ${${_project_name}_VERSION} ${_ecbuild_${_project_name}_UNPARSED_ARGUMENTS} )
    else()
      cmake_policy(SET CMP0048 OLD )
      _project( ${_project_name} ${_ecbuild_${_project_name}_UNPARSED_ARGUMENTS} )
    endif()

    unset( _ecbuild_${_project_name}_VERSION )

    include( ecbuild_system NO_POLICY_SCOPE )

    ecbuild_declare_project()

  else() # ecbuild 2 or pure CMake

    ecbuild_debug( "CMake project(${_project_name}) ")

    if(ECBUILD_2_COMPAT)
      cmake_policy(PUSH)
      cmake_minimum_required(VERSION 3.3 FATAL_ERROR) # for using IN_LIST
      cmake_policy(SET CMP0057 NEW) # for using IN_LIST

      unset( _args )
      foreach( arg ${ARGN} )
        list(APPEND _args ${arg} )
      endforeach()

      if( VERSION IN_LIST _args )
        set(_cmp0048_val NEW)
      else()
        set(_cmp0048_val OLD)
      endif()

      cmake_policy(POP)

      cmake_policy(SET CMP0048 ${_cmp0048_val} )
      unset(_cmp0048_val)
    endif()

    _project( ${_project_name} ${ARGN} )

  endif()

endmacro()

macro( ecbuild_project _project_name )
  set( ECBUILD_PROJECT_${_project_name} TRUE )
endmacro()


endif()
