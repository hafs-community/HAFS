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
# ecbuild_remove_fortran_flags
# ============================
#
# Remove Fortran compiler flags from ``CMAKE_Fortran_FLAGS``. ::
#
#   ecbuild_remove_fortran_flags( <flag1> [ <flag2> ... ] [ BUILD <build> ] )
#
# Options
# -------
#
# BUILD : optional
#   remove flags from ``CMAKE_Fortran_FLAGS_<build>`` instead of
#   ``CMAKE_Fortran_FLAGS``
#
##############################################################################

include( CheckFortranCompilerFlag )
macro( ecbuild_remove_fortran_flags m_flags )

  set( _flags ${m_flags} )
  if( _flags AND CMAKE_Fortran_COMPILER_LOADED )

    set( single_value_args BUILD )
    set( multi_value_args )
    cmake_parse_arguments( _PAR "" "${single_value_args}" "${multi_value_args}" ${_FIRST_ARG} ${ARGN} )

    string( TOUPPER ${CMAKE_BUILD_TYPE} CMAKE_BUILD_TYPE_CAPS )

    if( _PAR_BUILD )
      string( TOUPPER ${_PAR_BUILD} _PAR_BUILD_CAPS )
    endif()

    if( _PAR_BUILD AND (CMAKE_BUILD_TYPE_CAPS MATCHES "${_PAR_BUILD_CAPS}") )

      foreach( _flag ${_flags} )
        string(REGEX REPLACE " *${_flag} *" " " CMAKE_Fortran_FLAGS_${_PAR_BUILD} ${CMAKE_Fortran_FLAGS_${_PAR_BUILD}})
        ecbuild_debug( "Fortran FLAG [${_flag}] removed from build type ${_PAR_BUILD}" )
      endforeach()

    elseif( NOT _PAR_BUILD )

      foreach( _flag ${_flags} )
        string(REGEX REPLACE " *${_flag} *" " " CMAKE_Fortran_FLAGS_${CMAKE_BUILD_TYPE_CAPS} ${CMAKE_Fortran_FLAGS_${CMAKE_BUILD_TYPE_CAPS}} )
        string(REGEX REPLACE " *${_flag} *" " " CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS} )
        ecbuild_debug( "Fortran FLAG [${_flag}] removed" )
      endforeach()

    endif()

  endif()
  unset( _flags )

endmacro()
