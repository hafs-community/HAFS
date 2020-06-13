# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# - Try to find NetCDF
# Once done this will define
#  NDBM_FOUND - System has NetCDF
#  NDBM_INCLUDE_DIRS - The NetCDF include directories
#  NDBM_LIBRARIES - The libraries needed to use NetCDF
#  NDBM_DEFINITIONS - Compiler switches required for using NetCDF

if( DEFINED NDBM_PATH )
	find_path(NDBM_INCLUDE_DIR NAMES ndbm.h   PATHS ${NDBM_PATH} ${NDBM_PATH}/include PATH_SUFFIXES ndbm NO_DEFAULT_PATH)
	find_library(NDBM_LIBRARY  NAMES ndbm dbm PATHS ${NDBM_PATH} ${NDBM_PATH}/lib     PATH_SUFFIXES ndbm NO_DEFAULT_PATH)
endif()

find_path(NDBM_INCLUDE_DIR NAMES ndbm.h   PATH_SUFFIXES ndbm )
find_library( NDBM_LIBRARY NAMES ndbm dbm PATH_SUFFIXES ndbm )

set( NDBM_LIBRARIES    ${NDBM_LIBRARY} )
set( NDBM_INCLUDE_DIRS ${NDBM_INCLUDE_DIR} )

include(FindPackageHandleStandardArgs)

# handle the QUIETLY and REQUIRED arguments and set GRIBAPI_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(NDBM  DEFAULT_MSG
								  NDBM_LIBRARY NDBM_INCLUDE_DIR)

mark_as_advanced(NDBM_INCLUDE_DIR NDBM_LIBRARY )
