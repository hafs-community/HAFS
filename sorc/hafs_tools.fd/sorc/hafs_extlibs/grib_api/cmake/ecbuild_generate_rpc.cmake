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
# ecbuild_generate_rpc
# ====================
#
# Process RPC (Remote Procedure Call) Language files using rpcgen. ::
#
#   ecbuild_generate_rpc( SOURCE <file>
#                         [ TARGET_H <file> ]
#                         [ TARGET_C <file> ]
#                         [ DEPENDANT <file1> [ <file2> ... ] ] )
#
# Options
# -------
#
# SOURCE : required
#   RPC source file
#
# TARGET_H : optional (required if TARGET_C not given)
#   name of header file to be generated
#
# TARGET_C : optional (required if TARGET_H not given)
#   name of source file to be generated
#
# DEPENDANT : optional
#  list of files which depend on the generated source and header files
#
##############################################################################

macro( ecbuild_generate_rpc )

  set( options )
  set( single_value_args SOURCE TARGET_H TARGET_C )
  set( multi_value_args DEPENDANT )

  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if(_PAR_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ecbuild_generate_rpc(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
  endif()

  if( NOT _PAR_SOURCE  )
    ecbuild_critical("The call to ecbuild_generate_rpc() doesn't specify the SOURCE file.")
  endif()

  # optional
  #    if( NOT _PAR_DEPENDANT )
  #      ecbuild_critical("The call to ecbuild_generate_rpc() doesn't specify the DEPENDANT files.")
  #    endif()

  if( NOT DEFINED _PAR_TARGET_H AND NOT DEFINED _PAR_TARGET_C )
    ecbuild_critical("The call to ecbuild_generate_rpc() doesn't specify the _PAR_TARGET_H or _PAR_TARGET_C files.")
  endif()

  find_package( RPCGEN REQUIRED )

  if( DEFINED _PAR_TARGET_H )

    add_custom_command(
      OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/${_PAR_TARGET_H}
      COMMAND ${RPCGEN_EXECUTABLE} -h -o ${CMAKE_CURRENT_BINARY_DIR}/${_PAR_TARGET_H} ${_PAR_SOURCE}
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${_PAR_SOURCE} )

    if( DEFINED _PAR_DEPENDANT )
      foreach( file ${_PAR_DEPENDANT} )
        set_source_files_properties( ${file} PROPERTIES OBJECT_DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${_PAR_TARGET_H}" )
      endforeach()
    endif()

  endif()

  if( DEFINED _PAR_TARGET_C )

    add_custom_command(
      OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/${_PAR_TARGET_C}
      COMMAND ${RPCGEN_EXECUTABLE} -c -o ${CMAKE_CURRENT_BINARY_DIR}/${_PAR_TARGET_C} ${_PAR_SOURCE}
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${_PAR_SOURCE} )

    if( DEFINED _PAR_DEPENDANT )
      foreach( file ${_PAR_DEPENDANT} )
        set_source_files_properties( ${file} PROPERTIES OBJECT_DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${_PAR_TARGET_C}" )
      endforeach()
    endif()

  endif()

endmacro( ecbuild_generate_rpc )
