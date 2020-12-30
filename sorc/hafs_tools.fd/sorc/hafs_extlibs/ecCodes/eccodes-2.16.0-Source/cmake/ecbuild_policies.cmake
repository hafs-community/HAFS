# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

##############################################################################
#
# ecBuild Policies
# ================
#
# NOTE: This file needs to be included with NO_POLICY_SCOPE or it will have no
#       effect!
# NOTE: Policies 1 through 17 will be set to NEW by requiring CMake 2.8.4 i.e.
#       calling cmake_minimum_required( VERSION 2.8.4 FATAL_ERROR )
#
##############################################################################

if( NOT ${PROJECT_NAME}_ECBUILD_POLICIES_INCLUDED )
set( ${PROJECT_NAME}_ECBUILD_POLICIES_INCLUDED TRUE )

# fail if empty spaces are found around linked library names
if( POLICY CMP0004 )
    cmake_policy( SET CMP0004 NEW )
endif()

if( ECBUILD_2_COMPAT )
    # Allow mixed use of plain and keyword target_link_libraries
    if( POLICY CMP0023 )
        cmake_policy( SET CMP0023 OLD )
    endif()
else()
    # Prevent mixed use of plain and keyword target_link_libraries
    cmake_policy( SET CMP0023 NEW )
endif()

if( POLICY CMP0022 )
    #The OLD behavior for this policy is to ignore the
    #INTERFACE_LINK_LIBRARIES property for in-build targets.  The NEW
    #behavior for this policy is to use the INTERFACE_LINK_LIBRARIES
    #property for in-build targets, and ignore the old properties matching
    #``(IMPORTED_)?LINK_INTERFACE_LIBRARIES(_<CONFIG>)?``.
    cmake_policy( SET CMP0022 NEW )
endif()


if( ECBUILD_2_COMPAT )
  # Allow use of the LOCATION target property.
  if( POLICY CMP0026 )
      cmake_policy( SET CMP0026 OLD )
  endif()
endif()

# for macosx use @rpath in a target’s install name
if( POLICY CMP0042 )
    cmake_policy( SET CMP0042 NEW )
    set( CMAKE_MACOSX_RPATH ON )
endif()

# Error on non-existent target in get_target_property
if( POLICY CMP0045 )
    cmake_policy( SET CMP0045 NEW )
endif()

# Error on non-existent dependency in add_dependencies
if( POLICY CMP0046 )
    cmake_policy( SET CMP0046 NEW )
endif()

if( ECBUILD_2_COMPAT )
  # Do not manage VERSION variables in project command
  if( POLICY CMP0048 )
    cmake_policy( SET CMP0048 OLD )
  endif()
else()
  # Manage VERSION variables in project command
  cmake_policy( SET CMP0048 NEW )
endif()

# Disallow add_custom_command SOURCE signatures
if( POLICY CMP0050 )
    cmake_policy( SET CMP0050 NEW )
endif()

# Reject source and build dirs in installed INTERFACE_INCLUDE_DIRECTORIES
if( POLICY CMP0052 )
    cmake_policy( SET CMP0052 NEW )
endif()

# inside if() don't dereference variables if they are quoted
# e.g. "VAR" is not dereferenced
#      "${VAR}" is dereference only once
if( POLICY CMP0054 )
    cmake_policy( SET CMP0054 NEW )
endif()

# RPATH settings on macOS do not affect "install_name"
# FTM, keep old behavior -- need to test if new behavior impacts binaries in build directory
if( POLICY CMP0068 )
    cmake_policy( SET CMP0068 OLD )
endif()

# find packages use <package>_ROOT by default
if( POLICY CMP0074 )
    cmake_policy( SET CMP0074 NEW )
endif()

endif()
