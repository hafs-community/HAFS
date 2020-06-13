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
# ecbuild_add_persistent
# ======================
#
# Add persistent layer object classes. ::
#
#   ecbuild_add_persistent( SRC_LIST <variable>
#                           FILES <file1> [<file2> ...] ]
#                           [ NAMESPACE <namespace> ] )
#
# Options
# -------
#
# SRC_LIST : required
#   CMake variable to append the generated persistent layer objects to
#
# FILES : required
#   list of base names of files to build persistent class information for
#
#   The source file is expected to have a .h extension, the generated file
#   gets a .b extension.
#
# NAMESPACE : optional
#   C++ namespace to place the persistent class information in
#
##############################################################################

# define the script to build the persistent class information
set( sg_perl "${CMAKE_CURRENT_LIST_DIR}/sg.pl" CACHE INTERNAL "perl script to generate persistent objects" )

macro( ecbuild_add_persistent )

  ecbuild_find_perl( REQUIRED )

  set( options )
  set( single_value_args SRC_LIST NAMESPACE )
  set( multi_value_args  FILES )

  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if(_PAR_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ecbuild_add_persistent(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
  endif()

  if( NOT _PAR_SRC_LIST  )
    ecbuild_critical("The call to ecbuild_add_persistent() doesn't specify the SRC_LIST.")
  endif()

  if( NOT _PAR_FILES )
    ecbuild_critical("The call to ecbuild_add_persistent() doesn't specify the FILES.")
  endif()

  ecbuild_debug( "ecbuild_add_persistent: adding persistent layer for ${_PAR_FILES}" )

  foreach( file ${_PAR_FILES} )

    get_filename_component( _file_dir    ${file} PATH )
    get_filename_component( _file_we     ${file} NAME_WE )

    set( file ${_file_we} )
    if( _file_dir )
      file( MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${_file_dir} )
      set( file ${_file_dir}/${_file_we} )
    endif()

    ecbuild_debug( "ecbuild_add_persistent: adding persistent layer for ${file}.b with namespace ${_PAR_NAMESPACE} from ${file}.h in ${CMAKE_CURRENT_BINARY_DIR}/${_file_dir}" )

    add_custom_command( OUTPUT  ${file}.b
                        COMMAND ${PERL_EXECUTABLE} ${sg_perl} ${CMAKE_CURRENT_SOURCE_DIR}/${file}.h
                                ${CMAKE_CURRENT_BINARY_DIR}/${_file_dir} ${_PAR_NAMESPACE}
                        DEPENDS ${sg_perl} ${file}.h )
    set_source_files_properties( ${file}.h PROPERTIES OBJECT_DEPENDS "${file}.b" )
    list( APPEND ${_PAR_SRC_LIST} ${CMAKE_CURRENT_BINARY_DIR}/${file}.b )

  endforeach()

endmacro( ecbuild_add_persistent  )
