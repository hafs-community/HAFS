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
set( CMAKE_SKIP_BUILD_RPATH              FALSE )

# build with *relative* rpaths by default
if( ENABLE_RELATIVE_RPATHS )
    set( CMAKE_BUILD_WITH_INSTALL_RPATH  TRUE )
else()
    # in case the RPATH is absolute, the install RPATH cannot be set
    # at build-time since it breaks the build tree dynamic links
    set( CMAKE_BUILD_WITH_INSTALL_RPATH  FALSE )
endif()

# put the include dirs which are in the source or build tree
# before all other include dirs, so the headers in the sources
# are prefered over the already installed ones (since cmake 2.4.1)
set( CMAKE_INCLUDE_DIRECTORIES_PROJECT_BEFORE ON )
