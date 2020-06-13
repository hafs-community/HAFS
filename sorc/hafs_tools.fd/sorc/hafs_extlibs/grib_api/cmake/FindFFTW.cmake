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
# FindFFTW
# ========
#
# Find the FFTW library. ::
#
#   find_package(FFTW [REQUIRED] [QUIET]
#                [COMPONENTS [single] [double] [long_double] [quad]])
#
# By default, search for the double precision library ``fftw3``
#
# Components
# ----------
#
# If a different version or multiple versions of the library are required,
# these need to be specified as ``COMPONENTS``. Note that double must be given
# explicitly if any ``COMPONENTS`` are specified.
#
# The libraries corresponding to each of the ``COMPONENTS`` are:
#
# :single:      ``fftw3f``
# :double:      ``fftw3``
# :long_double: ``fftw3l``
# :quad:        ``fftw3q``
#
# Output variables
# ----------------
#
# The following CMake variables are set on completion:
#
# :FFTW_FOUND:      true if FFTW is found on the system
# :FFTW_LIBRARIES:  full paths to requested FFTW libraries
# :FFTW_INCLUDES:   FFTW include directory
#
# Input variables
# ---------------
#
# The following CMake variables are checked by the function:
#
# :FFTW_USE_STATIC_LIBS:  if true, only static libraries are found
# :FFTW_ROOT:             if set, this path is exclusively searched
# :FFTW_DIR:              equivalent to FFTW_ROOT
# :FFTW_PATH:             equivalent to FFTW_ROOT
# :FFTW_LIBRARIES:        User overriden FFTW libraries
# :FFTW_INCLUDES:         User overriden FFTW includes directories
#
##############################################################################

if( (NOT FFTW_ROOT) AND EXISTS $ENV{FFTW_ROOT} )
  set( FFTW_ROOT ${FFTW_ROOT} )
endif()
if( NOT FFTW_ROOT AND $FFTW_DIR )
  set( FFTW_ROOT ${FFTW_DIR} )
endif()
if( (NOT FFTW_ROOT) AND EXISTS $ENV{FFTW_DIR} )
  set( FFTW_ROOT $ENV{FFTW_DIR} )
endif()
if( (NOT FFTW_ROOT) AND FFTWDIR )
  set( FFTW_ROOT ${FFTWDIR} )
endif()
if( (NOT FFTW_ROOT) AND EXISTS $ENV{FFTWDIR} )
  set( FFTW_ROOT $ENV{FFTWDIR} )
endif()
if( (NOT FFTW_ROOT) AND FFTW_PATH )
  set( FFTW_ROOT ${FFTW_PATH} )
endif()
if( (NOT FFTW_ROOT) AND EXISTS $ENV{FFTW_PATH})
  set( FFTW_ROOT $ENV{FFTW_PATH} )
endif()

if( FFTW_ROOT ) # On cc[a|b|t] FFTW_DIR is set to the lib directory :(
  get_filename_component(_dirname ${FFTW_ROOT} NAME)
  if( _dirname MATCHES "lib" )
    set( FFTW_ROOT "${FFTW_ROOT}/.." )
  endif()
endif()

if( NOT FFTW_ROOT )
  # Check if we can use PkgConfig
  find_package(PkgConfig)

  #Determine from PKG
  if( PKG_CONFIG_FOUND AND NOT FFTW_ROOT )
    pkg_check_modules( PKG_FFTW QUIET "fftw3" )
  endif()
endif()

#Check whether to search static or dynamic libs
set( CMAKE_FIND_LIBRARY_SUFFIXES_SAV ${CMAKE_FIND_LIBRARY_SUFFIXES} )

if( ${FFTW_USE_STATIC_LIBS} )
  set( CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_STATIC_LIBRARY_SUFFIX} )
else()
  set( CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_SHARED_LIBRARY_SUFFIX} )
endif()


if( FFTW_FIND_COMPONENTS )
  ecbuild_debug( "FindFFTW: looking for components: ${FFTW_FIND_COMPONENTS}" )
  foreach( _component ${FFTW_FIND_COMPONENTS} )
    if( _component MATCHES "single" )
      ecbuild_debug( "FindFFTW: looking for single precision (fftw3f)" )
      set( _require_sp TRUE )
    elseif( _component MATCHES "double" )
      ecbuild_debug( "FindFFTW: looking for double precision (fftw3)" )
      set( _require_dp TRUE )
    elseif( _component MATCHES "long_double" )
      ecbuild_debug( "FindFFTW: looking for long double precision (fftw3l)" )
      set( _require_lp TRUE )
    elseif( _component MATCHES "quad" )
      ecbuild_debug( "FindFFTW: looking for quad precision (fftw3q)" )
      set( _require_qp TRUE )
    else()
    endif()
  endforeach()
else()
  ecbuild_debug( "FindFFTW: no components specified, looking for double precision (fftw3)" )
  set( _require_dp TRUE )
endif()

if( FFTW_ROOT )
  set( _default_paths NO_DEFAULT_PATH )
  set( _lib_paths ${FFTW_ROOT} )
  set( _include_paths ${FFTW_ROOT} )
else()
  set( _lib_paths ${PKG_FFTW_LIBRARY_DIRS} ${LIB_INSTALL_DIR} )
  set( _include_paths ${PKG_FFTW_INCLUDE_DIRS} ${INCLUDE_INSTALL_DIR} )
endif()

# find includes

if( NOT FFTW_INCLUDES ) # allow user to override with FFTW_INCLUDES

    find_path(
      FFTW_INCLUDES
      NAMES "fftw3.h"
      PATHS ${_include_paths}
      PATH_SUFFIXES "include"
      ${_default_paths}
    )

    if( NOT FFTW_INCLUDES )
      ecbuild_warn("FindFFTW: fftw include headers not found")
    endif()

endif()

# find libs

if( NOT FFTW_LIBRARIES ) # allow user to override with FFTW_LIBRARIES (e.g. for MKL implementation)

    if( _require_dp )
      find_library(
        FFTW_LIB
        NAMES "fftw3"
        PATHS ${_lib_paths}
        PATH_SUFFIXES "lib" "lib64"
        ${_default_paths}
      )
      if( NOT FFTW_LIB )
        ecbuild_warn("FindFFTW: double precision required, but fftw3 was not found")
      else()
        ecbuild_info("FFTW double precision: ${FFTW_LIB}")
      endif()
    endif()

    if( _require_sp )
      find_library(
        FFTWF_LIB
        NAMES "fftw3f"
        PATHS ${_lib_paths}
        PATH_SUFFIXES "lib" "lib64"
        ${_default_paths}
      )
      if( NOT FFTWF_LIB )
        ecbuild_warn("FindFFTW: single precision required, but fftw3f was not found")
      else()
        ecbuild_info("FFTW single precision: ${FFTWF_LIB}")
      endif()
    endif()

    if( _require_lp )
      find_library(
        FFTWL_LIB
        NAMES "fftw3l"
        PATHS ${_lib_paths}
        PATH_SUFFIXES "lib" "lib64"
        ${_default_paths}
      )
      if( NOT FFTWL_LIB )
        ecbuild_warn("FindFFTW: long double precision required, but fftw3l was not found")
      else()
        ecbuild_info("FFTW long double precision: ${FFTWL_LIB}")
      endif()
    endif()

    if( _require_qp )
      find_library(
        FFTWQ_LIB
        NAMES "fftw3q"
        PATHS ${_lib_paths}
        PATH_SUFFIXES "lib" "lib64"
        ${_default_paths}
      )
      if( NOT FFTWQ_LIB )
        ecbuild_warn("FindFFTW: quad precision required, but fftw3q was not found")
      else()
        ecbuild_info("FFTW quad precision: ${FFTWQ_LIB}")
      endif()
    endif()

    set(FFTW_LIBRARIES ${FFTW_LIB} ${FFTWF_LIB} ${FFTWL_LIB} ${FFTWQ_LIB})

endif()

ecbuild_info("FFTW includes : ${FFTW_INCLUDES}")
ecbuild_info("FFTW libraries: ${FFTW_LIBRARIES}")

set( CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES_SAV} )

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(FFTW DEFAULT_MSG
                                  FFTW_INCLUDES FFTW_LIBRARIES)

mark_as_advanced(FFTW_INCLUDES FFTW_LIBRARIES FFTW_LIB FFTWF_LIB FFTWL_LIB FFTWQ_LIB)
