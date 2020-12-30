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
# ecbuild_generate_fortran_interfaces
# ===================================
#
# Generates interfaces from Fortran source files. ::
#
#   ecbuild_generate_fortran_interfaces( TARGET <name>
#                                        DESTINATION <path>
#                                        DIRECTORIES <directory1> [<directory2> ...]
#                                        [ PARALLEL <integer> ]
#                                        [ INCLUDE_DIRS <name> ]
#                                        [ GENERATED <name> ]
#                                        [ SOURCE_DIR <path> ]
#                                        [ SUFFIX <suffix> ]
#                                        [ FCM_CONFIG_FILE <file> ]
#                                      )
#
# Options
# -------
#
# TARGET : required
#   target name
#
# DESTINATION : required
#   sub-directory of ``CMAKE_CURRENT_BINARY_DIR`` to install target to
#
# DIRECTORIES : required
#   list of directories in ``SOURCE_DIR`` in which to search for Fortran files to be processed
#
# PARALLEL : optional, defaults to 1
#   number of processes to use (always 1 on Darwin systems)
#
# INCLUDE_DIRS : optional
#   name of CMake variable to store the path to the include directory containing the resulting interfaces
#
# GENERATED : optional
#   name of CMake variable to store the list of generated interface files, including the full path to each
#
# SOURCE_DIR : optional, defaults to ``CMAKE_CURRENT_SOURCE_DIR``
#   directory in which to look for the sub-directories given as arguments to ``DIRECTORIES``
#
# SUFFIX : optional, defaults to ".intfb.h"
#   suffix to apply to name of each interface file
#
# FCM_CONFIG_FILE : optional, defaults to the ``fcm-make-interfaces.cfg`` file in the ecbuild project
#   FCM configuration file to be used to generate interfaces
#
# Usage
# _____
#
# The listed directories will be recursively searched for Fortran files of the
# form ``<fname>.[fF]``, ``<fname>.[fF]90``, ``<fname>.[fF]03`` or
# ``<fname>.[fF]08``. For each matching file, a file ``<fname><suffix>`` will be
# created containing the interface blocks for all external subprograms within
# it, where ``<suffix>`` is the value given to the ``SUFFIX`` option. If a file
# contains no such subprograms, no interface file will be generated for it.
#
##############################################################################

function( ecbuild_generate_fortran_interfaces )

  find_program( FCM_EXECUTABLE fcm REQUIRED DOC "Fortran interface generator" )

  if( NOT FCM_EXECUTABLE )
    ecbuild_error( "ecbuild_generate_fortran_interfaces: fcm executable not found." )
  endif()

  set( options )
  set( single_value_args TARGET DESTINATION PARALLEL INCLUDE_DIRS GENERATED SOURCE_DIR SUFFIX FCM_CONFIG_FILE )
  set( multi_value_args DIRECTORIES )

  cmake_parse_arguments( P "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if( NOT DEFINED P_TARGET )
    ecbuild_error( "ecbuild_generate_fortran_interfaces: TARGET argument missing" )
  endif()

  if( NOT DEFINED P_DESTINATION )
    ecbuild_error( "ecbuild_generate_fortran_interfaces: DESTINATION argument missing" )
  endif()

  if( NOT DEFINED P_DIRECTORIES )
    ecbuild_error( "ecbuild_generate_fortran_interfaces: DIRECTORIES argument missing" )
  endif()

  if( NOT DEFINED P_PARALLEL OR (${CMAKE_SYSTEM_NAME} MATCHES "Darwin") )
    set( P_PARALLEL 1 )
  endif()

  ecbuild_debug_var( P_PARALLEL )

  if( NOT DEFINED P_SOURCE_DIR )
    set( P_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" )
  endif()

  if( NOT DEFINED P_SUFFIX )
    set( P_SUFFIX ".intfb.h" )
  endif()

  if( DEFINED P_FCM_CONFIG_FILE )
    set( FCM_CONFIG_FILE ${P_FCM_CONFIG_FILE} )
  endif()

  if( NOT FCM_CONFIG_FILE )
    set( PROJECT_FCM_CONFIG_FILE "${PROJECT_SOURCE_DIR}/cmake/fcm-make-interfaces.cfg" )
    if( EXISTS ${PROJECT_FCM_CONFIG_FILE} )
      set( FCM_CONFIG_FILE ${PROJECT_FCM_CONFIG_FILE} )
      ecbuild_debug( "ecbuild_generate_fortran_interfaces: fcm configuration found in ${PROJECT_FCM_CONFIG_FILE}" )
    else()
      ecbuild_debug( "ecbuild_generate_fortran_interfaces: fcm configuration not found in ${PROJECT_FCM_CONFIG_FILE}" )
    endif()
  endif()

  if( NOT FCM_CONFIG_FILE )
    set( FCM_CONFIG_FILE "${ECBUILD_MACROS_DIR}/fcm-make-interfaces.cfg" )
    set( FCM_CONFIG_FILE "${CMAKE_CURRENT_BINARY_DIR}/fcm-make-interfaces.${P_TARGET}.cfg" )
    configure_file( "${ECBUILD_MACROS_DIR}/fcm-make-interfaces.cfg.in" "${FCM_CONFIG_FILE}" @ONLY )
  endif()

  ecbuild_debug_var( FCM_CONFIG_FILE )

  if( NOT EXISTS ${FCM_CONFIG_FILE} )
    ecbuild_error( "ecbuild_generate_fortran_interfaces: needs fcm configuration in ${FCM_CONFIG_FILE}" )
  endif()

  foreach( _srcdir ${P_DIRECTORIES} )
    if( _srcdir MATCHES "/$" )
      ecbuild_critical("ecbuild_generate_fortran_interfaces: directory ${_srcdir} must not end with /")
    endif()
    ecbuild_list_add_pattern( LIST fortran_files SOURCE_DIR ${P_SOURCE_DIR}
      GLOB ${_srcdir}/*.[fF] ${_srcdir}/*.[fF]90 ${_srcdir}/*.[fF]03 ${_srcdir}/*.[fF]08 QUIET )
  endforeach()

  string( REPLACE ";" " " _srcdirs "${P_DIRECTORIES}" )

  set( _cnt 0 )
  set( interface_files "" )
  foreach( fortran_file ${fortran_files} )
    #list( APPEND fullpath_fortran_files ${CMAKE_CURRENT_SOURCE_DIR}/${fortran_file} )
    get_filename_component(base ${fortran_file} NAME_WE)
    set( interface_file "${CMAKE_CURRENT_BINARY_DIR}/${P_DESTINATION}/interfaces/include/${base}${P_SUFFIX}" )
    list( APPEND interface_files ${interface_file} )
    set_source_files_properties( ${interface_file} PROPERTIES GENERATED TRUE )
    math(EXPR _cnt "${_cnt}+1")
  endforeach()

  ecbuild_info("Target ${P_TARGET} will generate ${_cnt} interface files using FCM")


  if( DEFINED P_GENERATED )
    set( ${P_GENERATED} ${interface_files} PARENT_SCOPE )
  endif()

  set( include_dir ${CMAKE_CURRENT_BINARY_DIR}/${P_DESTINATION}/interfaces/include )
  set( ${P_INCLUDE_DIRS} ${include_dir} PARENT_SCOPE )

  execute_process( COMMAND ${CMAKE_COMMAND} -E make_directory ${include_dir}
                   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )

  set( _fcm_lock ${CMAKE_CURRENT_BINARY_DIR}/${P_DESTINATION}/fcm-make.lock )
  set( _timestamp ${CMAKE_CURRENT_BINARY_DIR}/${P_DESTINATION}/generated.timestamp )
  add_custom_command(
    OUTPUT  ${_timestamp}
    COMMAND ${CMAKE_COMMAND} -E remove_directory ${_fcm_lock}
    COMMAND ${FCM_EXECUTABLE} make -j ${P_PARALLEL} --config-file=${FCM_CONFIG_FILE} interfaces.ns-incl=${_srcdirs} interfaces.source=${P_SOURCE_DIR}
    COMMAND ${CMAKE_COMMAND} -E touch ${_timestamp}
    DEPENDS ${fortran_files}
    COMMENT "[fcm] Generating ${_cnt} Fortran interface files for target ${P_TARGET} in ${CMAKE_CURRENT_BINARY_DIR}/${P_DESTINATION}/interfaces/include"
    WORKING_DIRECTORY ${P_DESTINATION} VERBATIM )

  add_custom_target(${P_TARGET}_gen DEPENDS ${_timestamp} )
  ecbuild_add_library(TARGET ${P_TARGET} TYPE INTERFACE DEPENDS ${P_TARGET}_gen)
  target_include_directories(${P_TARGET} INTERFACE $<BUILD_INTERFACE:${include_dir}>)

endfunction( ecbuild_generate_fortran_interfaces )
