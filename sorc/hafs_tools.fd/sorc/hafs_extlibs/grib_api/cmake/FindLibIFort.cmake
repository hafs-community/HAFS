# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# date:   July 2015
# author: Florian Rathgeber

###############################################################################

# - Try to find Intel Fortran (ifort) runtime libraries libifcore and libifport
# Once done this will define
#
#  LIBIFORT_FOUND   - system has Intel Fortran (ifort) runtime libraries
#  IFORT_LIBRARIES  - the Intel Fortran (ifort) runtime libraries
#
# The following paths will be searched with priority if set in CMake or env
#
#  INTEL_PATH       - prefix path of the Intel installation
#
# Otherwise the libraries are assumed to be on LIBRARY_PATH or LD_LIBRARY_PATH

# FIXME: might need to add further libraries in future, see
# http://nf.nci.org.au/facilities/software/Compilers/Intel8/doc/f_ug1/files_32.htm

# Search with priority for INTEL_PATH if given as CMake or env var
find_library( IFORT_LIB_CORE ifcore PATHS ${INTEL_PATH} ENV INTEL_PATH
              PATH_SUFFIXES lib/intel64 compiler/lib/intel64 NO_DEFAULT_PATH )
find_library( IFORT_LIB_PORT ifport PATHS ${INTEL_PATH} ENV INTEL_PATH
              PATH_SUFFIXES lib/intel64 compiler/lib/intel64 NO_DEFAULT_PATH )

# Otherwise, search LIBRARY_PATH and LD_LIBRARY_PATH
find_library( IFORT_LIB_CORE ifcore PATHS ENV LIBRARY_PATH LD_LIBRARY_PATH )
find_library( IFORT_LIB_PORT ifport PATHS ENV LIBRARY_PATH LD_LIBRARY_PATH )

mark_as_advanced( IFORT_LIB_CORE IFORT_LIB_PORT )

if( IFORT_LIB_CORE AND IFORT_LIB_PORT )
  set( IFORT_LIBRARIES ${IFORT_LIB_CORE} ${IFORT_LIB_PORT} )
endif()

include(FindPackageHandleStandardArgs)

# Handle the QUIET and REQUIRED arguments and set LIBIFORT_FOUND to TRUE
# if all listed variables are TRUE
# Note: capitalisation of the package name must be the same as in the file name
find_package_handle_standard_args( LibIFort DEFAULT_MSG IFORT_LIBRARIES )
