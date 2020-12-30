# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

#Sets:
# CMATH_LIBRARIES      = the library to link against (RT etc)

IF(UNIX)
  if( DEFINED CMATH_PATH )
    find_library(CMATH_LIBRARIES m PATHS ${CMATH_PATH}/lib NO_DEFAULT_PATH )
  endif()

  find_library(CMATH_LIBRARIES m )

  include(FindPackageHandleStandardArgs)

  # handle the QUIET and REQUIRED arguments and set CMATH_FOUND to TRUE
  # if all listed variables are TRUE
  # Note: capitalisation of the package name must be the same as in the file name
  find_package_handle_standard_args(CMath DEFAULT_MSG CMATH_LIBRARIES )

ENDIF(UNIX)
