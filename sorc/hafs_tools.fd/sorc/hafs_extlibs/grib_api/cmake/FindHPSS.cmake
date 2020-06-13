# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# - Try to find HPSS
# Once done this will define
#  HPSS_FOUND - System has HPSS
#  HPSS_INCLUDE_DIRS - The HPSS include directories
#  HPSS_LIBRARIES - The libraries needed to use HPSS
#  HPSS_DEFINITIONS - Compiler switches required for using HPSS

if( DEFINED HPSS_PATH )
	find_path(HPSS_INCLUDE_DIR hpss_api.h PATHS ${HPSS_PATH}/include PATH_SUFFIXES hpss NO_DEFAULT_PATH)
	find_library(HPSS_LIBRARY  hpss       PATHS ${HPSS_PATH}/lib     PATH_SUFFIXES hpss NO_DEFAULT_PATH)
endif()

find_path(HPSS_INCLUDE_DIR hpss_api.h PATH_SUFFIXES hpss )
find_library( HPSS_LIBRARY hpss       PATH_SUFFIXES hpss )

include(FindPackageHandleStandardArgs)

# handle the QUIETLY and REQUIRED arguments and set HPSS_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(HPSS  DEFAULT_MSG
                                  HPSS_LIBRARY HPSS_INCLUDE_DIR)

mark_as_advanced(HPSS_INCLUDE_DIR HPSS_LIBRARY )

if( HPSS_FOUND )
    set( HPSS_LIBRARIES    ${HPSS_LIBRARY} )
    set( HPSS_INCLUDE_DIRS ${HPSS_INCLUDE_DIR} )
else()
    set( HPSS_LIBRARIES    "" )
    set( HPSS_INCLUDE_DIRS "" )
endif()
