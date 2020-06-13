# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# Try to find the Trilinos library
#
# Needs environmental variables
#   TRILINOS_PATH
# Sets
#   TRILINOS_INCLUDE_DIRS
#   TRILINOS_LIBRARIES
#   TRILINOS_FOUND

# Try to find Trilinos using Trilinos recommendations

if( DEFINED $ENV{TRILINOS_PATH} )
    find_package(Trilinos PATHS $ENV{TRILINOS_PATH}/lib/cmake/Trilinos $ENV{TRILINOS_PATH}/include )
endif()

if( TRILINOS_PATH )
    find_package(Trilinos PATHS ${TRILINOS_PATH}/lib/cmake/Trilinos ${TRILINOS_PATH}/include )
endif()

if( Trilinos_FOUND )

        set( TRILINOS_INCLUDE_DIRS "" )

        list( APPEND TRILINOS_INCLUDE_DIRS ${Trilinos_INCLUDE_DIRS} )
        list( APPEND TRILINOS_INCLUDE_DIRS ${Trilinos_TPL_INCLUDE_DIRS} )

        foreach( test_lib ${Trilinos_LIBRARIES} )
          if(NOT ${test_lib} STREQUAL "pytrilinos")
            find_library( ${test_lib}_lib ${test_lib} PATHS  ${Trilinos_LIBRARY_DIRS}  NO_DEFAULT_PATH)
            find_library( ${test_lib}_lib ${test_lib})
            mark_as_advanced( ${test_lib}_lib )
            list( APPEND TRILINOS_LIBRARIES ${${test_lib}_lib} )
          endif()
        endforeach()

        list( APPEND TRILINOS_LIBRARIES ${Trilinos_TPL_LIBRARIES} )

    set( TRILINOS_FOUND TRUE )
	set( TRILINOS_VERSION ${Trilinos_VERSION} )

endif()
