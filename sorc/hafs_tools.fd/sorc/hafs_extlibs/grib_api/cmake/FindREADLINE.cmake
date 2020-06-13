# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# - Try to find READLINE
# Once done this will define
#  READLINE_FOUND - System has READLINE
#  READLINE_INCLUDE_DIRS - The READLINE include directories
#  READLINE_LIBRARIES - The libraries needed to use READLINE
#  READLINE_DEFINITIONS - Compiler switches required for using READLINE

if( DEFINED READLINE_PATH )
    find_path(READLINE_INCLUDE_DIR readline/readline.h PATHS ${READLINE_PATH}/include NO_DEFAULT_PATH)
    find_library(READLINE_LIBRARY  readline            PATHS ${READLINE_PATH}/lib     PATH_SUFFIXES readline NO_DEFAULT_PATH)
endif()

find_path(READLINE_INCLUDE_DIR readline/readline.h )
find_library( READLINE_LIBRARY readline            PATH_SUFFIXES readline )

# check what version we got
cmake_push_check_state()

  set( CMAKE_REQUIRED_LIBRARIES ${READLINE_LIBRARY} )
  set( CMAKE_REQUIRED_INCLUDES  ${READLINE_INCLUDE_DIR} )

  # sometimes the link might fail missing -ltermcap or -l(n)curses
  # if we searched before for Curses, then lets try to use it
  if(CURSES_FOUND)
      list( APPEND CMAKE_REQUIRED_LIBRARIES ${CURSES_LIBRARIES} )
      list( APPEND CMAKE_REQUIRED_INCLUDES  ${CURSES_INCLUDE_DIR} )
  endif()

  ecbuild_check_cxx_source_return(
     "#include <stdio.h>
      #include <readline/readline.h>
      #include <iostream>
      int main() {
          std::cout << rl_library_version << std::flush;
          return 0;
     }"
     VAR readline_version
     OUTPUT __readline_version_out )

cmake_pop_check_state()

# ecbuild_debug_var( readline_version )
# ecbuild_debug_var( __readline_version_out )

set( __readline_fail 0 )
if( __readline_version_out )

    if( "${__readline_version_out}" MATCHES "^EditLine" )
      message( STATUS "Found EditLine instead of Readline at '${READLINE_INCLUDE_DIR}'" )
      if( READLINE_WRAPPER_OK )
        set( READLINE_WRAPPER      "EditLine" )
        set( __readline_fail 0 )
      else()
        message( STATUS "Readline wrapper not accepted -- rejecting Readline at '${READLINE_INCLUDE_DIR}'" )
        set( __readline_fail 1 )
      endif()
    endif()

else()
    message( STATUS "Readline test run failed -- rejecting Readline at '${READLINE_INCLUDE_DIR}'" )
    set( __readline_fail 1 )
endif()

if( __readline_fail )
    set( READLINE_LIBRARY      READLINE_LIBRARY-NOTFOUND )
    set( READLINE_INCLUDE_DIR  READLINE_INCLUDE_DIR-NOTFOUND )
endif()

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(READLINE  DEFAULT_MSG READLINE_LIBRARY READLINE_INCLUDE_DIR)

if( READLINE_FOUND )
    set( READLINE_VERSION      ${__readline_version_out} )
    set( READLINE_LIBRARIES    ${READLINE_LIBRARY} )
    set( READLINE_INCLUDE_DIRS ${READLINE_INCLUDE_DIR} )
endif()

mark_as_advanced(READLINE_INCLUDE_DIR READLINE_LIBRARY )
