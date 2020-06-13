# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

##############################################################################
#.rst:
#
# ecbuild_add_fortran_flags
# =========================
#
# Add Fortran compiler flags to CMAKE_Fortran_FLAGS only if supported by the
# compiler. ::
#
#   ecbuild_add_fortran_flags( <flag1> [ <flag2> ... ]
#                              [ BUILD <build> ]
#                              [ NAME <name> ]
#                              [ NO_FAIL ] )
#
# Options
# -------
#
# BUILD : optional
#   add flags to ``CMAKE_Fortran_FLAGS_<build>`` instead of
#   ``CMAKE_Fortran_FLAGS``
#
# NAME : optional
#   name of the check (if omitted, checks are enumerated)
#
# NO_FAIL : optional
#   do not fail if the flag cannot be added
#
##############################################################################

include( CheckFortranCompilerFlag )
macro( ecbuild_add_fortran_flags m_fortran_flags )

  set( _flags ${m_fortran_flags} )

  if( _flags AND CMAKE_Fortran_COMPILER_LOADED )

    set( options NO_FAIL )
    set( single_value_args BUILD NAME )
    set( multi_value_args )

    cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

    set( _try_add_flag TRUE )
    if( _PAR_BUILD )
      string( TOUPPER ${CMAKE_BUILD_TYPE} CMAKE_BUILD_TYPE_CAPS )
      string( TOUPPER ${_PAR_BUILD}  _PAR_BUILD_CAPS )
      if( NOT CMAKE_BUILD_TYPE_CAPS MATCHES "${_PAR_BUILD_CAPS}" )
        set( _try_add_flag FALSE )
      endif()
    endif()

    if( _try_add_flag )
      if( NOT DEFINED N_FortranFLAG )
        set( N_FortranFLAG 0 )
      endif()

      math( EXPR N_FortranFLAG '${N_FortranFLAG}+1' )

      if( ECBUILD_TRUST_FLAGS )
        set( _flag_ok 1 )
      # Due to a bug in CMake < 3.0, check_fortran_compiler_flag ALWAYS fails with ifort
      # see https://cmake.org/Bug/view.php?id=14507
      elseif( CMAKE_MAJOR_VERSION LESS 3 AND CMAKE_Fortran_COMPILER_ID MATCHES "Intel" )
        set( _flag_ok 1 )
        ecbuild_warn( "Not testing Fortran flags due to a bug in CMake < 3.0 with ifort" )
      else()
        if( DEFINED _PAR_NAME )
          check_fortran_compiler_flag( ${_flags} ${_PAR_NAME} )
          set( _flag_ok ${${_PAR_NAME}} )
        else()
          check_fortran_compiler_flag( ${_flags} Fortran_FLAG_TEST_${N_FortranFLAG} )
          set( _flag_ok ${Fortran_FLAG_TEST_${N_FortranFLAG}} )
        endif()
      endif()

      if( _flag_ok )
        if( _PAR_BUILD )
          set( CMAKE_Fortran_FLAGS_${_PAR_BUILD} "${CMAKE_Fortran_FLAGS_${_PAR_BUILD}} ${_flags}" )
          ecbuild_debug( "Fortran FLAG [${_flags}] added for build type ${_PAR_BUILD}" )
        else()
          set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${_flags}" )
          ecbuild_debug( "Fortran FLAG [${_flags}] added" )
        endif()
      elseif( _PAR_NO_FAIL )
        ecbuild_info( "Unrecognised Fortran flag [${_flags}] -- skipping" )
      else()
        ecbuild_error( "Unrecognised Fortran flag [${_flags}]" )
      endif()
    endif()

    unset( _flags )
    unset( _flag_ok )
    unset( _try_add_flag )
  endif()

endmacro()

macro( cmake_add_fortran_flags m_fortran_flags )
  ecbuild_deprecate( " cmake_add_fortran_flags is deprecated, use ecbuild_add_fortran_flags instead." )
  ecbuild_add_fortran_flags( ${m_fortran_flags} )
endmacro()
