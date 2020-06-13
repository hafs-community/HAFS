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

if( NOT spot_FOUND )

    # find external odb_api

    if( NOT DEFINED SPOT_PATH AND NOT "$ENV{SPOT_PATH}" STREQUAL "" )
        list( APPEND SPOT_PATH "$ENV{SPOT_PATH}" )
    endif()

    if( DEFINED SPOT_PATH )

        find_path( SPOT_INCLUDE_DIR
                   NAMES spot_database.h
                   PATHS ${SPOT_PATH} ${SPOT_PATH}/include
                   NO_DEFAULT_PATH )

        find_library( SPOT_LIBRARY NAMES spot_database
                      PATHS ${SPOT_PATH} ${SPOT_PATH}/lib
                      NO_DEFAULT_PATH )

    endif()

        find_path( SPOT_INCLUDE_DIR
                   NAMES spot_database.h
                   PATHS
                   PATH_SUFFIXES spot )

        find_library( SPOT_LIBRARY NAMES spot_database
                      PATHS
                      PATH_SUFFIXES odb_api )

        find_library( ODB_API_ECLIB_LIBRARY NAMES Ec
                      PATHS
                      PATH_SUFFIXES spot )


    # get the version

    include(FindPackageHandleStandardArgs)

    find_package_handle_standard_args( spot DEFAULT_MSG
                                       SPOT_LIBRARY SPOT_INCLUDE_DIR )

    set( SPOT_LIBRARIES    ${SPOT_LIBRARY} )
    set( SPOT_INCLUDE_DIRS ${SPOT_INCLUDE_DIR} )

    mark_as_advanced( SPOT_INCLUDE_DIR SPOT_LIBRARY )

    set( spot_FOUND ${SPOT_FOUND} )

endif()
