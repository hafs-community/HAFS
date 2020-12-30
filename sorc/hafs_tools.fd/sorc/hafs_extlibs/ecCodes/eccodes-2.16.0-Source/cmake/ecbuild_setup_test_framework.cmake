# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

ecbuild_add_option( FEATURE TESTS
                    DEFAULT ON
                    DESCRIPTION "Enable the unit tests" )

if( HAVE_TESTS )

  # CTest has built-in support for running with memcheck
  # (https://cmake.org/cmake/help/latest/manual/ctest.1.html#ctest-memcheck-step)
  # via `ctest -T memcheck`, however by default memcheck does not exit with a
  # non-zero error code if any issues are found.
  #
  # CTest will run ${MEMORYCHECK_COMMAND} with ${MEMORYCHECK_COMMAND_OPTIONS}.
  # Suppressions are read from ${MEMORYCHECK_SUPPRESSIONS_FILE} if given.

  find_program( MEMORYCHECK_COMMAND valgrind )
  ecbuild_debug_var( MEMORYCHECK_COMMAND )

  if( NOT MEMORYCHECK_COMMAND_OPTIONS )
    set( MEMORYCHECK_COMMAND_OPTIONS "--trace-children=yes --leak-check=full --error-exitcode=1"
         CACHE STRING "Options passed to memcheck command" )
  endif()
  ecbuild_debug_var( MEMORYCHECK_COMMAND_OPTIONS )

  if( NOT MEMORYCHECK_SUPPRESSIONS_FILE AND EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/valgrind_suppress.txt" )
    set( MEMORYCHECK_SUPPRESSIONS_FILE "${CMAKE_CURRENT_SOURCE_DIR}/${PROJECT_NAME}.supp"
         CACHE FILEPATH "Suppressions file to be used with memcheck command" )
  endif()
  ecbuild_debug_var( MEMORYCHECK_SUPPRESSIONS_FILE )

else()

  ecbuild_info("Tests have been disabled")

endif()
