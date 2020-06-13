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
# FindJemalloc
# ============
#
# Find the Jemalloc library. ::
#
#   find_package( Jemalloc [REQUIRED] [QUIET] )
#
# Output variables
# ----------------
#
# The following CMake variables are set on completion:
#
# :JEMALLOC_FOUND:      true if JEMALLOC is found on the system
# :JEMALLOC_LIBRARIES:  full paths to requested JEMALLOC libraries
# :JEMALLOC_INCLUDES:   JEMALLOC include directory
#
# Input variables
# ---------------
#
# The following CMake variables are checked by the function:
#
# :JEMALLOC_USE_STATIC_LIBS:  if true, only static libraries are found
# :JEMALLOC_ROOT:             if set, this path is exclusively searched
# :JEMALLOC_DIR:              equivalent to JEMALLOC_ROOT
# :JEMALLOC_PATH:             equivalent to JEMALLOC_ROOT
# :JEMALLOC_LIBRARY:          JEMALLOC library to use
# :JEMALLOC_INCLUDE_DIR:      JEMALLOC include directory
#
##############################################################################

if( (NOT JEMALLOC_ROOT) AND EXISTS $ENV{JEMALLOC_ROOT} )
  set( JEMALLOC_ROOT ${JEMALLOC_ROOT} )
endif()
if( NOT JEMALLOC_ROOT AND $JEMALLOC_DIR )
  set( JEMALLOC_ROOT ${JEMALLOC_DIR} )
endif()
if( (NOT JEMALLOC_ROOT) AND EXISTS $ENV{JEMALLOC_DIR} )
  set( JEMALLOC_ROOT $ENV{JEMALLOC_DIR} )
endif()
if( (NOT JEMALLOC_ROOT) AND JEMALLOCDIR )
  set( JEMALLOC_ROOT ${JEMALLOCDIR} )
endif()
if( (NOT JEMALLOC_ROOT) AND EXISTS $ENV{JEMALLOCDIR} )
  set( JEMALLOC_ROOT $ENV{JEMALLOCDIR} )
endif()
if( (NOT JEMALLOC_ROOT) AND JEMALLOC_PATH )
  set( JEMALLOC_ROOT ${JEMALLOC_PATH} )
endif()
if( (NOT JEMALLOC_ROOT) AND EXISTS $ENV{JEMALLOC_PATH})
  set( JEMALLOC_ROOT $ENV{JEMALLOC_PATH} )
endif()

#if( NOT JEMALLOC_ROOT )
#  # Check if we can use PkgConfig
#  find_package(PkgConfig)
#  #Determine from PKG
#  if(PKG_CONFIG_FOUND)
#    pkg_check_modules( PKG_JEMALLOC QUIET "jemalloc" )
#  endif()
#endif()

find_path(JEMALLOC_ROOT NAMES include/jemalloc/jemalloc.h)

find_library(JEMALLOC_LIBRARIES
    NAMES jemalloc
    HINTS ${JEMALLOC_ROOT}/lib
)

find_path(JEMALLOC_INCLUDE_DIR
    NAMES jemalloc/jemalloc.h
    HINTS ${JEMALLOC_ROOT}/include
)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Jemalloc DEFAULT_MSG
    JEMALLOC_LIBRARIES
    JEMALLOC_INCLUDE_DIR
)

mark_as_advanced(
    JEMALLOC_ROOT
    JEMALLOC_LIBRARIES
    JEMALLOC_INCLUDE_DIR
)


#Sets:
# DL_LIBRARIES      = the library to link against (RT etc)

if( DEFINED DL_PATH )
    find_library(DL_LIBRARIES dl PATHS ${DL_PATH}/lib NO_DEFAULT_PATH )
endif()

find_library(DL_LIBRARIES dl )

include(FindPackageHandleStandardArgs)

# handle the QUIET and REQUIRED arguments and set DL_FOUND to TRUE
# if all listed variables are TRUE
# Note: capitalisation of the package name must be the same as in the file name
find_package_handle_standard_args(Dl DEFAULT_MSG DL_LIBRARIES )
