# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

############################################################################################
# define default build type

set( _BUILD_TYPE_MSG "Build type options are: [ None | Debug | Bit | Production | Release | RelWithDebInfo ]" )

if( NOT ECBUILD_DEFAULT_BUILD_TYPE )
	set( ECBUILD_DEFAULT_BUILD_TYPE "RelWithDebInfo" )
endif()

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE ${ECBUILD_DEFAULT_BUILD_TYPE} CACHE STRING  ${_BUILD_TYPE_MSG}  FORCE )
endif()

# capitalize the build type for easy use with conditionals
string( TOUPPER ${CMAKE_BUILD_TYPE} CMAKE_BUILD_TYPE_CAPS )

# correct capitatlization of the build type

if( CMAKE_BUILD_TYPE_CAPS STREQUAL "NONE" )
  set(CMAKE_BUILD_TYPE None CACHE STRING ${_BUILD_TYPE_MSG} FORCE )
endif()

if( CMAKE_BUILD_TYPE_CAPS STREQUAL "DEBUG" )
  set(CMAKE_BUILD_TYPE Debug CACHE STRING ${_BUILD_TYPE_MSG} FORCE )
endif()

if( CMAKE_BUILD_TYPE_CAPS STREQUAL "BIT" )
  set(CMAKE_BUILD_TYPE Bit CACHE STRING ${_BUILD_TYPE_MSG} FORCE )
endif()

if( CMAKE_BUILD_TYPE_CAPS STREQUAL "PRODUCTION" )
  set(CMAKE_BUILD_TYPE Production CACHE STRING ${_BUILD_TYPE_MSG} FORCE )
endif()

if( CMAKE_BUILD_TYPE_CAPS STREQUAL "RELEASE" )
  set(CMAKE_BUILD_TYPE Release CACHE STRING ${_BUILD_TYPE_MSG} FORCE )
endif()

if( CMAKE_BUILD_TYPE_CAPS STREQUAL "RELWITHDEBINFO" )
  set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING ${_BUILD_TYPE_MSG} FORCE )
endif()

# fail if build type is not one of the defined ones
if( NOT CMAKE_BUILD_TYPE MATCHES "None"  AND
	  NOT CMAKE_BUILD_TYPE MATCHES "Debug" AND
	  NOT CMAKE_BUILD_TYPE MATCHES "Bit" AND
	  NOT CMAKE_BUILD_TYPE MATCHES "Production" AND
    NOT CMAKE_BUILD_TYPE MATCHES "Release"  AND
    NOT CMAKE_BUILD_TYPE MATCHES "RelWithDebInfo" )
    ecbuild_critical( "CMAKE_BUILD_TYPE is not recognized. ${_BUILD_TYPE_MSG}" )
endif()
