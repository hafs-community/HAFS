# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

##############################################################################
# macro for looking for openmp flags

macro( lookup_omp_flags )
  set(_OMP_FLAG_GNU        "-fopenmp")
  set(_OMPSTUBS_FLAG_GNU   "-fno-openmp")

  set(_OMP_FLAG_Cray       "-homp")
  set(_OMPSTUBS_FLAG_Cray  "-hnoomp")

  set(_OMP_FLAG_XL         "-qsmp=omp")
  set(_OMPSTUBS_FLAG_XL    "-qsmp=noomp")

  set(_OMP_FLAG_Intel      "-qopenmp")
  set(_OMPSTUBS_FLAG_Intel "-qopenmp-stubs")

  # sample C openmp source code to test
  set(_OMP_C_TEST_SOURCE
  "
  #include <omp.h>
  int main() {
  #ifdef _OPENMP
    #pragma omp parallel
    {
      (void)omp_get_thread_num();
    }
    return 0;
  #else
    breaks_on_purpose
  #endif
  }
  ")
  set( _OMP_CXX_TEST_SOURCE ${_OMP_C_TEST_SOURCE} )


  # sample C openmp source code to test
  set(_OMPSTUBS_C_TEST_SOURCE
  "
  // Include must be found
  #include <omp.h>
  int main() {
  #ifdef _OPENMP
    breaks_on_purpose
  #else
    #pragma omp parallel
    {
      // This pragma should have passed compilation
      (void)0;
    }
    return 0;
  #endif
  }
  ")
  set( _OMPSTUBS_CXX_TEST_SOURCE ${_OMPSTUBS_C_TEST_SOURCE} )


  # sample Fortran openmp source code to test
  set(_OMP_Fortran_TEST_SOURCE
  "
  program main
    use omp_lib
  end program
  ")
  set( _OMPSTUBS_Fortran_TEST_SOURCE ${_OMP_Fortran_TEST_SOURCE} )

endmacro()

##############################################################################
#.rst:
#
# ecbuild_find_omp
# ================
#
# Find OpenMP. ::
#
#   ecbuild_find_omp( [ COMPONENTS <component1> [ <component2> ... ] ]
#                     [ REQUIRED ]
#                     [ STUBS ] )
#
# Options
# -------
#
# COMPONENTS : optional, defaults to C
#   list of required languages bindings
#
# REQUIRED : optional
#   fail if OpenMP was not found
#
# STUBS : optional
#   search for OpenMP stubs
#
# Output variables
# ----------------
#
# The following CMake variables are set if OpenMP was found:
#
# :OMP_FOUND: OpenMP was found
#
# For each language listed in COMPONENTS, the following variables are set:
#
# :OMP_<LANG>_FOUND: OpenMP bindings for LANG were found
# :OMP_<LANG>_FLAGS: OpenMP compiler flags for LANG
#
# If the STUBS option was given, all variables are also set with the OMPSTUBS
# instead of the OMP prefix.
#
##############################################################################

macro( ecbuild_find_omp )

  set( options REQUIRED STUBS )
  set( single_value_args )
  set( multi_value_args COMPONENTS )

  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if( NOT _PAR_COMPONENTS )
    ecbuild_critical( "No COMPONENTS were specified, looking for OMP.\n Please find with COMPONENTS C CXX Fortran " )
  endif()

  set( _STUBS "" )
  if( _PAR_STUBS )
    set( _STUBS "STUBS" )
  endif()

  lookup_omp_flags()

  set( OMP${_STUBS}_FOUND TRUE )

  foreach( _LANG ${_PAR_COMPONENTS} )

    if( NOT OMP${_STUBS}_${_LANG}_FLAGS )

      if( DEFINED _OMP${_STUBS}_FLAG_${CMAKE_${_LANG}_COMPILER_ID} )
        set( _OMP${_STUBS}_${_LANG}_FLAG "${_OMP${_STUBS}_FLAG_${CMAKE_${_LANG}_COMPILER_ID}}" )
      endif()
      if( CMAKE_${_LANG}_COMPILER_LOADED AND _OMP${_STUBS}_${_LANG}_FLAG )
        set(SAVE_CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS}")
        set(CMAKE_REQUIRED_FLAGS "${_OMP${_STUBS}_${_LANG}_FLAG}")
        include(Check${_LANG}SourceCompiles)
        set( _SOURCE ${_OMP${_STUBS}_${_LANG}_TEST_SOURCE} )
        set( _FLAG ${_LANG}_COMPILER_SUPPORTS_OMP${_STUBS})
        if( _LANG STREQUAL "C" )
          check_c_source_compiles("${_SOURCE}" ${_FLAG} )
        endif()
        if( _LANG STREQUAL "CXX" )
          check_cxx_source_compiles("${_SOURCE}" ${_FLAG} )
        endif()
        if( _LANG STREQUAL "Fortran" )
          check_fortran_source_compiles("${_SOURCE}" ${_FLAG} SRC_EXT f90)
        endif()
        set(CMAKE_REQUIRED_FLAGS "${SAVE_CMAKE_REQUIRED_FLAGS}")
      endif()

      if( ${_LANG}_COMPILER_SUPPORTS_OMP${_STUBS} )
        set( OMP${_STUBS}_${_LANG}_FLAGS ${_OMP${_STUBS}_${_LANG}_FLAG} )
      endif()

    else()
      set( ${_LANG}_COMPILER_SUPPORTS_OMP${_STUBS} TRUE )
    endif()


    set( OMP${_STUBS}_${_LANG}_FIND_QUIETLY TRUE )
    find_package_handle_standard_args( OMP${_STUBS}_${_LANG} REQUIRED_VARS ${_LANG}_COMPILER_SUPPORTS_OMP${_STUBS}  )

    if( OMP${_STUBS}_FORTRAN_FOUND )
      set( OMP${_STUBS}_Fortran_FOUND TRUE )
    endif()

    if( NOT OMP${_STUBS}_${_LANG}_FOUND )
      set( OMP${_STUBS}_FOUND FALSE )
    endif()

    if( _PAR_STUBS )
      set( OMP_${_LANG}_FOUND ${OMPSTUBS_${_LANG}_FOUND} )
      set( OMP_${_LANG}_FLAGS ${OMPSTUBS_${_LANG}_FLAGS} )
    endif()

  endforeach()

  if( _PAR_STUBS )
    set( OMP_FOUND ${OMPSTUBS_FOUND} )
  endif()

endmacro( ecbuild_find_omp )

##############################################################################
#.rst:
#
# ecbuild_enable_omp
# ==================
#
# Find OpenMP for C, C++ and Fortran and set the compiler flags for each
# language for which OpenMP support was detected.
#
##############################################################################

macro( ecbuild_enable_omp )

  ecbuild_debug("ecbuild_enable_omp: Trying to enable OpenMP")
  ecbuild_find_omp( COMPONENTS C CXX Fortran )

  ecbuild_debug_var("OMP_C_FOUND")
  if( OMP_C_FOUND )
    ecbuild_debug("Adding ${OMP_C_FLAGS} to CMAKE_C_FLAGS")
    set( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OMP_C_FLAGS}" )
  endif()

  ecbuild_debug_var("OMP_CXX_FOUND")
  if( OMP_CXX_FOUND )
    ecbuild_debug("Adding ${OMP_CXX_FLAGS} to CMAKE_CXX_FLAGS")
    set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OMP_CXX_FLAGS}" )
  endif()

  ecbuild_debug_var("OMP_Fortran_FOUND")
  if( OMP_Fortran_FOUND )
    ecbuild_debug("Adding ${OMP_Fortran_FLAGS} to CMAKE_Fortran_FLAGS")
    set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OMP_Fortran_FLAGS}" )
  endif()

endmacro( ecbuild_enable_omp )

##############################################################################
#.rst:
#
# ecbuild_enable_ompstubs
# =======================
#
# Find OpenMP stubs for C, C++ and Fortran and set the compiler flags for each
# language for which OpenMP stubs were detected.
#
##############################################################################

macro( ecbuild_enable_ompstubs )

  ecbuild_find_omp( COMPONENTS C CXX Fortran STUBS )

  if( OMPSTUBS_C_FOUND )
    set( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OMPSTUBS_C_FLAGS}" )
  endif()

  if( OMPSTUBS_CXX_FOUND )
    set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OMPSTUBS_CXX_FLAGS}" )
  endif()

  if( OMPSTUBS_Fortran_FOUND )
    set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OMPSTUBS_Fortran_FLAGS}" )
  endif()

endmacro( ecbuild_enable_ompstubs )
