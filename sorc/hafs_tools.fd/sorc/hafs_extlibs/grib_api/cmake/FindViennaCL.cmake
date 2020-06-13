# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# - Try to find ViennaCL
# Once done this will define
#
#  VIENNACL_FOUND         - system has ViennaCL
#  VIENNACL_INCLUDE_DIRS  - the ViennaCL include directories
#
# The following paths will be searched with priority if set in CMake or env
#
#  VIENNACL_PATH          - prefix path of the ViennaCL installation
#
# ViennaCL is header only, so there are no libraries to be found

# Search with priority for VIENNACL_PATH if given as CMake or env var
find_path(VIENNACL_INCLUDE_DIR viennacl/version.hpp
          PATHS ${VIENNACL_PATH} ENV VIENNACL_PATH
          PATH_SUFFIXES include NO_DEFAULT_PATH)

find_path(VIENNACL_INCLUDE_DIR viennacl/version.hpp
          PATH_SUFFIXES include )

set( VIENNACL_INCLUDE_DIRS ${VIENNACL_INCLUDE_DIR} )

include(FindPackageHandleStandardArgs)

# handle the QUIET and REQUIRED arguments and set VIENNACL_FOUND to TRUE
# if all listed variables are valid
# Note: capitalisation of the package name must be the same as in the file name
find_package_handle_standard_args(ViennaCL  DEFAULT_MSG
                                  VIENNACL_INCLUDE_DIR)

mark_as_advanced(VIENNACL_INCLUDE_DIRS)
