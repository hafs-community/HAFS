# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set( __gen_source_flags ${CMAKE_CURRENT_LIST_DIR}/gen_source_flags.py )

# Calls gen_source_flags.py to generate a CMake file with the per
# source file flags for a given target.
function( ecbuild_source_flags OUT TARGET DEFAULT_FLAGS SOURCES )

  if( NOT PYTHONINTERP_FOUND OR PYTHON_VERSION VERSION_LESS 2.7 )
    find_package( PythonInterp 2.7 REQUIRED )
  endif()

  set( OUTFILE ${CMAKE_CURRENT_BINARY_DIR}/${TARGET}_source_flags.cmake )

  if( ECBUILD_LOG_LEVEL LESS 11)
    set( __debug "--debug" )
  endif()
  execute_process( COMMAND ${PYTHON_EXECUTABLE} ${__gen_source_flags}
                           ${ECBUILD_SOURCE_FLAGS} ${OUTFILE} "${DEFAULT_FLAGS}"
                           ${SOURCES} "${__debug}"
                   RESULT_VARIABLE __res )

  if( __res GREATER 0 )
    ecbuild_error( "ecbuild_source_flags: failed generating source flags for target ${TARGET} from ${ECBUILD_SOURCE_FLAGS}" )
  endif()
  set( ${OUT} ${OUTFILE} PARENT_SCOPE )

endfunction()
