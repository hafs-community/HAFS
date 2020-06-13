# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# - Try to find ADSM
# Once done this will define
#  ADSM_FOUND - System has ADSM
#  ADSM_INCLUDE_DIRS - The ADSM include directories
#  ADSM_LIBRARIES - The libraries needed to use ADSM

if( EC_OS_BITS EQUAL 32 )
	set( ADSM_LIBNAME ApiDS )
endif()
if( EC_OS_BITS EQUAL 64 )
	set( ADSM_LIBNAME ApiTSM64 )
endif()
if( NOT DEFINED ADSM_LIBNAME )
	message( STATUS "MARS only supports ADSM with 32 or 64 bits" )
endif()

if( DEFINED ADSM_PATH )
	find_path(ADSM_INCLUDE_DIR dsmapitd.h      PATHS ${ADSM_PATH} ${ADSM_PATH}/include ${ADSM_PATH}/sample NO_DEFAULT_PATH )
	find_library(ADSM_LIBRARY  ${ADSM_LIBNAME} PATHS ${ADSM_PATH} ${ADSM_PATH}/lib     ${ADSM_PATH}/lib64  NO_DEFAULT_PATH )
endif()

find_path(ADSM_INCLUDE_DIR dsmapitd.h      PATH_SUFFIXES bin64 )
find_library( ADSM_LIBRARY ${ADSM_LIBNAME} PATH_SUFFIXES bin64 )

set( ADSM_LIBRARIES    ${ADSM_LIBRARY} )
set( ADSM_INCLUDE_DIRS ${ADSM_INCLUDE_DIR} )

include(FindPackageHandleStandardArgs)

# handle the QUIETLY and REQUIRED arguments and set ADSM_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(ADSM  DEFAULT_MSG
								  ADSM_LIBRARY ADSM_INCLUDE_DIR)

mark_as_advanced(ADSM_INCLUDE_DIR ADSM_LIBRARY )
