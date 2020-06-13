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
# ecbuild_enable_fortran
# ======================
#
# Enable the Fortran language. ::
#
#   ecbuild_enable_fortran( [ MODULE_DIRECTORY <directory> ] [ REQUIRED ] )
#
# Options
# -------
#
# MODULE_DIRECTORY : optional, defaults to ``${CMAKE_BINARY_DIR}/module``
#   set the CMAKE_Fortran_MODULE_DIRECTORY
#
# REQUIRED : optional
#   fail if no working Fortran compiler was detected
#
##############################################################################

macro( ecbuild_enable_fortran )

  set( options REQUIRED  )
  set( single_value_args MODULE_DIRECTORY )
  set( multi_value_args  )

  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if(_PAR_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ecbuild_enable_fortran(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
  endif()

  if( NOT CMAKE_Fortran_COMPILER_LOADED )
    enable_language( Fortran )
    ecbuild_compiler_flags( Fortran )
    if( ENABLE_WARNINGS AND CMAKE_Fortran_COMPILER_ID MATCHES "Intel" )
      set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -warn all" )
      ecbuild_debug( "Fortran FLAG [-warn all] added" )
    endif()
  endif()

  if( DEFINED _PAR_REQUIRED )
    if( CMAKE_Fortran_COMPILER_FORCED )
      set( CMAKE_Fortran_COMPILER_WORKS 1 )
    endif()
    if( NOT CMAKE_Fortran_COMPILER OR NOT CMAKE_Fortran_COMPILER_WORKS )
      ecbuild_critical( "Fortran compiler required by project ${PROJECT_NAME} but does not seem to work" )
    endif()
  endif()

  if( CMAKE_Fortran_COMPILER_LOADED )

    include(CheckFortranFunctionExists)
    if( CMAKE_C_COMPILER_LOADED AND ENABLE_FORTRAN_C_INTERFACE )
      include(FortranCInterface)
    endif()
    set( EC_HAVE_FORTRAN 1 )

    # see issue ECBUILD-298
    if( CMAKE_Fortran_COMPILER_ID MATCHES PGI )
      unset( CMAKE_Fortran_COMPILE_OPTIONS_PIE )
      unset( CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS )
    endif()

  endif()

  if( DEFINED _PAR_MODULE_DIRECTORY )
    set( CMAKE_Fortran_MODULE_DIRECTORY  ${_PAR_MODULE_DIRECTORY} )
  else()
    set( CMAKE_Fortran_MODULE_DIRECTORY  ${CMAKE_BINARY_DIR}/module
         CACHE PATH "directory for all fortran modules." )
  endif()

  file( MAKE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} )

  include_directories( ${CMAKE_Fortran_MODULE_DIRECTORY} )

  install( CODE "EXECUTE_PROCESS (COMMAND \"${CMAKE_COMMAND}\" -E copy_directory \"${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}\" \"${INSTALL_INCLUDE_DIR}\")" )

endmacro( ecbuild_enable_fortran )
