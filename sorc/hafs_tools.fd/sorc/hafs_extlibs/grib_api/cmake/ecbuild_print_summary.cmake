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
# ecbuild_print_summary
# =====================
#
# Print a summary of the project, build environment and enabled features. ::
#
#   ecbuild_print_summary()
#
# If ``project_summary.cmake`` exist in the source root directory, a project
# summary is printed by including this file.
#
# For a top level project, a summary of the build environment and a feature
# summary are also printed.
#
##############################################################################

macro( ecbuild_print_summary )

  if( EXISTS ${PROJECT_SOURCE_DIR}/project_summary.cmake )

    ecbuild_info( "---------------------------------------------------------" )
    ecbuild_info( "Project ${PROJECT_NAME} summary" )
    ecbuild_info( "---------------------------------------------------------" )

    include( ${PROJECT_SOURCE_DIR}/project_summary.cmake )

  endif()

  if( PROJECT_NAME STREQUAL CMAKE_PROJECT_NAME )

    get_property( langs GLOBAL PROPERTY ENABLED_LANGUAGES )

    ecbuild_info( "---------------------------------------------------------" )
    if( NOT ${DEVELOPER_MODE} )
      ecbuild_info( "Build summary" )
    else()
      ecbuild_info( "Build summary -- ( DEVELOPER_MODE )" )
    endif()
    ecbuild_info( "---------------------------------------------------------" )

    ecbuild_info( "system : [${BUILD_SITE}] [${CMAKE_SYSTEM}] [${EC_OS_NAME}.${EC_OS_BITS}]" )
    ecbuild_info( "processor        : [${CMAKE_SYSTEM_PROCESSOR}]" )
    if( EC_BIG_ENDIAN )
      ecbuild_info( "endiness         : Big Endian -- IEEE [${IEEE_BE}]" )
    endif()
    if( EC_LITTLE_ENDIAN )
      ecbuild_info( "endiness         : Little Endian -- IEEE [${IEEE_LE}]" )
    endif()
    ecbuild_info( "build type       : [${CMAKE_BUILD_TYPE}]" )
    ecbuild_info( "timestamp        : [${EC_BUILD_TIMESTAMP}]" )
    ecbuild_info( "install prefix   : [${CMAKE_INSTALL_PREFIX}]" )
    ecbuild_info( "  bin dir        : [${${PNAME}_FULL_INSTALL_BIN_DIR}]" )
    ecbuild_info( "  lib dir        : [${${PNAME}_FULL_INSTALL_LIB_DIR}]" )
    ecbuild_info( "  include dir    : [${${PNAME}_FULL_INSTALL_INCLUDE_DIR}]" )
    ecbuild_info( "  data dir       : [${${PNAME}_FULL_INSTALL_DATA_DIR}]" )
    ecbuild_info( "  cmake dir      : [${${PNAME}_FULL_INSTALL_CMAKE_DIR}]" )
    if( EC_LINK_DIR )
      ecbuild_info( "links prefix     : [${EC_LINK_DIR}]" )
    endif()
    ecbuild_info( "---------------------------------------------------------" )

    foreach( lang ${langs} )
      ecbuild_info( "${lang} -- ${CMAKE_${lang}_COMPILER_ID} ${CMAKE_${lang}_COMPILER_VERSION}"  )
      ecbuild_info( "    compiler   : ${CMAKE_${lang}_COMPILER}" )
      ecbuild_info( "    flags      : ${CMAKE_${lang}_FLAGS} ${CMAKE_${lang}_FLAGS_${CMAKE_BUILD_TYPE_CAPS}} ${${PNAME}_${lang}_FLAGS} ${${PNAME}_${lang}_FLAGS_${CMAKE_BUILD_TYPE_CAPS}}" )
      ecbuild_info( "    link flags : ${CMAKE_${lang}_LINK_FLAGS}" )
    endforeach()

    ecbuild_info( "linker : ${CMAKE_LINKER}")
    ecbuild_info( "ar     : ${CMAKE_AR}")
    ecbuild_info( "ranlib : ${CMAKE_RANLIB}")
    ecbuild_info( "link flags" )
    ecbuild_info( "    executable [${CMAKE_EXE_LINKER_FLAGS} ${CMAKE_EXE_LINKER_FLAGS_${CMAKE_BUILD_TYPE_CAPS}}]" )
    ecbuild_info( "    shared lib [${CMAKE_SHARED_LINKER_FLAGS} ${CMAKE_SHARED_LINKER_FLAGS_${CMAKE_BUILD_TYPE_CAPS}}]" )
    ecbuild_info( "    static lib [${CMAKE_MODULE_LINKER_FLAGS} ${CMAKE_MODULE_LINKER_FLAGS_${CMAKE_BUILD_TYPE_CAPS}}]" )
    ecbuild_info( "install rpath  : ${CMAKE_INSTALL_RPATH}" )

    get_directory_property( defs COMPILE_DEFINITIONS )

    ecbuild_info( "common definitions: ${defs}" )

    ### FEATURE SUMMARY

    ecbuild_info( "---------------------------------------------------------" )
    ecbuild_info( "Feature summary" )
    ecbuild_info( "---------------------------------------------------------" )

    if( ${CMAKE_VERSION} VERSION_LESS "2.8.6" )
      set( __what ALL )
    else()
      set( __what ALL INCLUDE_QUIET_PACKAGES )
    endif()

    # Print feature summary
    feature_summary( WHAT ${__what} )
    # Write feature summary to ecbuild.log
    feature_summary( WHAT ${__what} FILENAME ${ECBUILD_LOG_FILE} APPEND )

    ### WARNINGS

    # issue warnings / errors in case there are unused project files
    ecbuild_warn_unused_files()

  endif()

endmacro( ecbuild_print_summary )
