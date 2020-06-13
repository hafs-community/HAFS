# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# define project paths

file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/lib )

#######################################################################################################

# setup library building rpaths (both in build dir and then when installed)

# add the automatic parts to RPATH which point to dirs outside build tree
set( CMAKE_INSTALL_RPATH_USE_LINK_PATH   TRUE  )

# use RPATHs for the build tree
set( CMAKE_SKIP_BUILD_RPATH              FALSE  )

# If INSTALL_LIB_DIR is set to anything other than lib, the relative install
# RPATH is wrong in the build tree
if( ENABLE_RELATIVE_RPATHS )
  ecbuild_debug( "Relative RPATHS are enabled" )
  if( INSTALL_LIB_DIR STREQUAL "lib" OR (NOT INSTALL_LIB_DIR) )
    # when building, use the install RPATH immediately (we don't want to relink)
    set( CMAKE_BUILD_WITH_INSTALL_RPATH      TRUE  )
    ecbuild_debug( "Building with install RPATH" )
  else()
    # when building, don't use the install RPATH yet, but later on when installing
    set( CMAKE_BUILD_WITH_INSTALL_RPATH      FALSE  )
    ecbuild_debug( "Not building with install RPATH, need to relink when installing" )
  endif()
endif()

# Always include srcdir and builddir in include path
# This saves typing ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR}
# in about every subdir

set( CMAKE_INCLUDE_CURRENT_DIR OFF )

# put the include dirs which are in the source or build tree
# before all other include dirs, so the headers in the sources
# are prefered over the already installed ones (since cmake 2.4.1)

set(CMAKE_INCLUDE_DIRECTORIES_PROJECT_BEFORE ON)
