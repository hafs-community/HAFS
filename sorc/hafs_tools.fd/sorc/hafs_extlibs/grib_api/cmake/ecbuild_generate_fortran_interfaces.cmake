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
# Generates interfaces from the Fortran source files. ::
#
#   ecbuild_generate_fortran_interfaces()
#
# Options
# -------
#
# TARGET : required
#   target name
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
  foreach( file ${_fortran_files} )
    if( ${${SRC}/file} IS_NEWER_THAN ${${SRC}/file} )
      set( run_fcm 1 )
    endif()
  endforeach()

  foreach( fortran_file ${fortran_files} )
    #list( APPEND fullpath_fortran_files ${CMAKE_CURRENT_SOURCE_DIR}/${fortran_file} )
      get_filename_component(base ${fortran_file} NAME_WE)
      set( interface_file "${CMAKE_CURRENT_BINARY_DIR}/interfaces/include/${base}${P_SUFFIX}" )
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

    add_custom_command(
      OUTPUT  "${P_DESTINATION}/${P_TARGET}.timestamp"
      COMMAND ${FCM_EXECUTABLE} make -j ${P_PARALLEL} --config-file=${FCM_CONFIG_FILE} interfaces.ns-incl=${_srcdirs} interfaces.source=${P_SOURCE_DIR}
      COMMAND touch "${P_TARGET}.timestamp"
      DEPENDS ${fortran_files}
      COMMENT "Generating ${_cnt} interface files for target ${P_TARGET}"
      WORKING_DIRECTORY ${P_DESTINATION} VERBATIM )

    add_custom_target( ${P_TARGET} DEPENDS ${P_DESTINATION}/${P_TARGET}.timestamp )


endfunction( ecbuild_generate_fortran_interfaces )
