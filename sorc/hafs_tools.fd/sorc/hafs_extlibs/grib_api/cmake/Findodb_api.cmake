# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# - Try to find ODB_API
# Once done this will define
#  ODB_API_FOUND - System has ODB_API
#  ODB_API_INCLUDE_DIRS - The ODB_API include directories
#  ODB_API_LIBRARIES - The libraries needed to use ODB_API
#  ODB_API_DEFINITIONS - Compiler switches required for using ODB_API

if( NOT odb_api_FOUND )

    # find external odb_api

    if( NOT DEFINED ODB_API_PATH AND NOT "$ENV{ODB_API_PATH}" STREQUAL "" )
        list( APPEND ODB_API_PATH "$ENV{ODB_API_PATH}" )
    endif()

    if( DEFINED ODB_API_PATH )

        find_path( ODB_API_INCLUDE_DIR
                   NAMES odb_api_config.h
                   PATHS ${ODB_API_PATH} ${ODB_API_PATH}/include
                   PATH_SUFFIXES odb_api
                   NO_DEFAULT_PATH )

        find_library( ODB_API_LIBRARY NAMES Odb
                      PATHS ${ODB_API_PATH} ${ODB_API_PATH}/lib
                      PATH_SUFFIXES odb_api
                      NO_DEFAULT_PATH )

        find_library( ODB_API_ECLIB_LIBRARY NAMES Ec
                      PATHS ${ODB_API_PATH} ${ODB_API_PATH}/lib
                      PATH_SUFFIXES odb_api
                      NO_DEFAULT_PATH )
    endif()

        find_path( ODB_API_INCLUDE_DIR
                   NAMES odb_api_config.h
                   PATHS
                   PATH_SUFFIXES odb_api )

        find_library( ODB_API_LIBRARY NAMES Odb
                      PATHS
                      PATH_SUFFIXES odb_api )

        find_library( ODB_API_ECLIB_LIBRARY NAMES Ec
                      PATHS
                      PATH_SUFFIXES odb_api )


    # get the version

    if( ODB_API_INCLUDE_DIR )

        set(_odb_api_VERSION_REGEX "([0-9]+)")

        foreach( v MAJOR MINOR PATCH )

          file(STRINGS "${ODB_API_INCLUDE_DIR}/odb_api_config.h" _odb_api_${v}_VERSION_CONTENTS REGEX "#define ODB_API_${v}_VERSION ")

          if( "${_odb_api_${v}_VERSION_CONTENTS}" MATCHES ".*#define ODB_API_${v}_VERSION ${_odb_api_VERSION_REGEX}.*")

              set( ODB_API_${v}_VERSION "${CMAKE_MATCH_1}" )

          endif()

        endforeach()

    endif()

    set( ODB_API_VERSION     "${ODB_API_MAJOR_VERSION}.${ODB_API_MINOR_VERSION}.${ODB_API_PATCH_VERSION}" )
    set( ODB_API_VERSION_STR "${_odb_info_out}" )

    set( odb_api_VERSION     "${ODB_API_VERSION}" )
    set( odb_api_VERSION_STR "${ODB_API_VERSION_STR}" )

    # handle the QUIETLY and REQUIRED arguments and set ODB_API_FOUND to TRUE

    include(FindPackageHandleStandardArgs)

    find_package_handle_standard_args( odb_api DEFAULT_MSG
                                       ODB_API_LIBRARY ODB_API_ECLIB_LIBRARY ODB_API_INCLUDE_DIR )

    set( ODB_API_LIBRARIES    ${ODB_API_LIBRARY} ${ODB_API_ECLIB_LIBRARY} )
    set( ODB_API_INCLUDE_DIRS ${ODB_API_INCLUDE_DIR} )

    mark_as_advanced( ODB_API_INCLUDE_DIR ODB_API_LIBRARY ODB_API_ECLIB_LIBRARY )

    set( odb_api_FOUND ${ODB_API_FOUND} )

endif()
