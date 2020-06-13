# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# Try to find NetCDF
#
# Input:
#  * NETCDF_PATH     - user defined path where to search for the library first
#                      (CMake or environment variable)
#  * NETCDF_DIR      - user defined path where to search for the library first
#                      (CMake or environment variable)
#  * NETCDF_CXX      - search also for netcdf_c++ wrapper library
#  * NETCDF_Fortran  - search also for netcdff wrapper library
#
# Output:
#  NETCDF_FOUND - System has NetCDF
#  NETCDF_INCLUDE_DIRS - The NetCDF include directories
#  NETCDF_LIBRARIES - The libraries needed to use NetCDF

### TODO: generalize this into a macro for all ecbuild

if( DEFINED NETCDF_PATH )
    list( APPEND _netcdf_incs ${NETCDF_PATH} ${NETCDF_PATH}/include )
    list( APPEND _netcdf_libs ${NETCDF_PATH} ${NETCDF_PATH}/lib )
endif()

if( DEFINED NETCDF_DIR )
    list( APPEND _netcdf_incs ${NETCDF_DIR} ${NETCDF_DIR}/include )
    list( APPEND _netcdf_libs ${NETCDF_DIR} ${NETCDF_DIR}/lib )
endif()

# Honour environment variables NETCDF_DIR, NETCDF_PATH
list( APPEND _netcdf_incs ENV NETCDF_DIR ENV NETCDF_PATH )
list( APPEND _netcdf_libs ENV NETCDF_DIR ENV NETCDF_PATH )

###

set( _inc_sfx netcdf include )
set( _lib_sfx netcdf lib64 lib )

find_path( NETCDF_INCLUDE_DIR  netcdf.h  PATHS ${_netcdf_incs} PATH_SUFFIXES ${_inc_sfx} NO_DEFAULT_PATH )
find_path( NETCDF_INCLUDE_DIR  netcdf.h  PATHS ${_netcdf_incs} PATH_SUFFIXES ${_inc_sfx} )

find_library( NETCDF_LIBRARY  netcdf  PATHS ${_netcdf_libs} PATH_SUFFIXES ${_lib_sfx}  NO_DEFAULT_PATH )
find_library( NETCDF_LIBRARY  netcdf  PATHS ${_netcdf_libs} PATH_SUFFIXES ${_lib_sfx}  )

set( NETCDF_LIBRARIES    ${NETCDF_LIBRARY} )
set( NETCDF_INCLUDE_DIRS ${NETCDF_INCLUDE_DIR} )

mark_as_advanced(NETCDF_INCLUDE_DIR NETCDF_LIBRARY )

list( APPEND NETCDF_REQUIRED_VARS NETCDF_LIBRARY NETCDF_INCLUDE_DIR )

if( NETCDF_CXX )

    find_path( NETCDF_CXX_INCLUDE_DIR netcdfcpp.h PATHS ${_netcdf_incs} PATH_SUFFIXES ${_inc_sfx} NO_DEFAULT_PATH)
    find_path( NETCDF_CXX_INCLUDE_DIR netcdfcpp.h PATHS ${_netcdf_incs} PATH_SUFFIXES ${_inc_sfx} )

    set( _ncdf_cxx netcdf_c++ netcdf_c++ netcdf_c++4 )

    find_library( NETCDF_CXX_LIBRARY NAMES ${_ncdf_cxx} PATHS ${_netcdf_libs} PATH_SUFFIXES ${_lib_sfx} NO_DEFAULT_PATH )
    find_library( NETCDF_CXX_LIBRARY NAMES ${_ncdf_cxx} PATHS ${_netcdf_libs} PATH_SUFFIXES ${_lib_sfx} )

    list( APPEND NETCDF_INCLUDE_DIRS ${NETCDF_CXX_INCLUDE_DIR} )
    list( APPEND NETCDF_LIBRARIES    ${NETCDF_CXX_LIBRARY} )

    list( APPEND NETCDF_REQUIRED_VARS NETCDF_CXX_INCLUDE_DIR NETCDF_CXX_LIBRARY )

    mark_as_advanced(NETCDF_CXX_INCLUDE_DIR NETCDF_CXX_LIBRARY )

endif()

if( NETCDF_Fortran )

    find_path( NETCDF_Fortran_INCLUDE_DIR netcdf.mod PATHS ${_netcdf_incs} PATH_SUFFIXES ${_inc_sfx} NO_DEFAULT_PATH)
    find_path( NETCDF_Fortran_INCLUDE_DIR netcdf.mod PATHS ${_netcdf_incs} PATH_SUFFIXES ${_inc_sfx} )

    set( _ncdf_fortran netcdff )

    find_library( NETCDF_Fortran_LIBRARY NAMES ${_ncdf_fortran} PATHS ${_netcdf_libs} PATH_SUFFIXES ${_lib_sfx} NO_DEFAULT_PATH )
    find_library( NETCDF_Fortran_LIBRARY NAMES ${_ncdf_fortran} PATHS ${_netcdf_libs} PATH_SUFFIXES ${_lib_sfx} )

    list( APPEND NETCDF_INCLUDE_DIRS ${NETCDF_Fortran_INCLUDE_DIR} )
    list( APPEND NETCDF_LIBRARIES    ${NETCDF_Fortran_LIBRARY} )

    list( APPEND NETCDF_REQUIRED_VARS NETCDF_Fortran_INCLUDE_DIR NETCDF_Fortran_LIBRARY )

    mark_as_advanced(NETCDF_Fortran_INCLUDE_DIR NETCDF_Fortran_LIBRARY )

endif()

list( REMOVE_DUPLICATES NETCDF_INCLUDE_DIRS )

include(FindPackageHandleStandardArgs)

if( NETCDF_FIND_QUIETLY )
  set( NETCDF3_FIND_QUIETLY ${NETCDF_FIND_QUIETLY} )
endif()
if( NETCDF_FIND_REQUIRED )
  set( NETCDF3_FIND_REQUIRED ${NETCDF_FIND_REQUIRED} )
endif()

# Handle the QUIET and REQUIRED arguments and set NETCDF3_FOUND to TRUE
# if all listed variables are TRUE
# Note: capitalisation of the package name must be the same as in the file name
find_package_handle_standard_args( NetCDF3  DEFAULT_MSG ${NETCDF_REQUIRED_VARS} )

set( NETCDF_FOUND ${NETCDF3_FOUND} )

