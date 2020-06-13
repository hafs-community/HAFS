# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# - Try to find lib Lustre API

# usually installed on Cray systems under /opt/cray/lustre-cray_ari_s/default / create_test.c -L
# .../include/lustre/lustreapi.h
# .../lib64/liblustreapi.so

# Once done this will define
#  LUSTREAPI_FOUND        - System has LustreAPI
#  LUSTREAPI_INCLUDE_DIRS - The LustreAPI include directories
#  LUSTREAPI_LIBRARIES    - The libraries needed to use LustreAPI
#
# The following paths will be searched with priority if set in CMake or env
#
#  LUSTREAPI_DIR          - prefix path of the LustreAPI installation
#  LUSTREAPI_PATH         - prefix path of the LustreAPI installation

find_path( LUSTREAPI_INCLUDE_DIR lustre/lustreapi.h
           PATHS ${LUSTREAPI_DIR} ${LUSTREAPI_PATH} ENV LUSTREAPI_DIR ENV LUSTREAPI_PATH
           PATH_SUFFIXES include NO_DEFAULT_PATH )

find_path( LUSTREAPI_INCLUDE_DIR lustre/lustreapi.h PATH_SUFFIXES include )

find_library( LUSTREAPI_LIBRARY NAMES lustreapi
              PATHS ${LUSTREAPI_DIR} ${LUSTREAPI_PATH} ENV LUSTREAPI_DIR ENV LUSTREAPI_PATH
              PATH_SUFFIXES lib lib64 NO_DEFAULT_PATH )
find_library( LUSTREAPI_LIBRARY NAMES lustreapi PATH_SUFFIXES lib lib64 )

set( LUSTREAPI_LIBRARIES    ${LUSTREAPI_LIBRARY} )
set( LUSTREAPI_INCLUDE_DIRS ${LUSTREAPI_INCLUDE_DIR} )

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(LUSTREAPI  DEFAULT_MSG LUSTREAPI_LIBRARY LUSTREAPI_INCLUDE_DIR)

mark_as_advanced(LUSTREAPI_INCLUDE_DIR LUSTREAPI_LIBRARY )
