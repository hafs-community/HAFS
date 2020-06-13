# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# - Try to find OpenCL
# Once done this will define
#
#  OPENCL_FOUND           - system has OpenCL
#  OPENCL_INCLUDE_DIRS    - the OpenCL include directory
#  OPENCL_LIBRARIES       - link these to use OpenCL
#
# The following paths will be searched with priority if set in CMake or env
#
#  OPENCL_ROOT            - root folder of the OpenCL installation
#  CUDA_TOOLKIT_ROOT_DIR  - root folder of the CUDA installation (ships OpenCL)
#  CUDA_ROOT              - root folder of the CUDA installation (ships OpenCL)

if(UNIX)

  if(APPLE)

    # Search with priority for OPENCL_ROOT if given as CMake or env var
    find_path(OPENCL_INCLUDE_DIRS OpenCL/cl.h
              PATHS ${OPENCL_ROOT} ENV OPENCL_ROOT
              PATH_SUFFIXES include NO_DEFAULT_PATH)
    find_path(OPENCL_INCLUDE_DIRS OpenCL/cl.h
              PATH_SUFFIXES include )

    # Search with priority for OPENCL_ROOT if given as CMake or env var
    find_library(OPENCL_LIBRARIES OpenCL
                 PATHS ${OPENCL_ROOT} ENV OPENCL_ROOT
                 PATH_SUFFIXES lib NO_DEFAULT_PATH)
    find_library(OPENCL_LIBRARIES OpenCL
                 PATH_SUFFIXES lib )

  else()

    # Search with priority for OPENCL_ROOT if given as CMake or env var
    find_path(OPENCL_INCLUDE_DIRS NAMES CL/cl.h CL/opencl.h
              PATHS ${OPENCL_ROOT} ENV OPENCL_ROOT
              PATH_SUFFIXES include NO_DEFAULT_PATH)
    find_path(OPENCL_INCLUDE_DIRS NAMES CL/cl.h CL/opencl.h
              PATHS ${CUDA_TOOLKIT_ROOT_DIR} ${CUDA_ROOT} /usr/local/cuda
              PATH_SUFFIXES include )

    # Search with priority for OPENCL_ROOT if given as CMake or env var
    find_library(OPENCL_LIBRARIES OpenCL
                 PATHS ${OPENCL_ROOT} ENV OPENCL_ROOT
                 PATH_SUFFIXES lib64 lib NO_DEFAULT_PATH)
    find_library(OPENCL_LIBRARIES OpenCL
                 PATHS ${CUDA_TOOLKIT_ROOT_DIR} ${CUDA_ROOT} /usr/local/cuda
                 PATH_SUFFIXES lib64 lib )

  endif()

endif()

include(FindPackageHandleStandardArgs)

# Handle the QUIET and REQUIRED arguments and set OPENCL_FOUND to TRUE
# if all listed variables are TRUE
# Note: capitalisation of the package name must be the same as in the file name
find_package_handle_standard_args( OpenCL DEFAULT_MSG
                                   OPENCL_LIBRARIES OPENCL_INCLUDE_DIRS )

mark_as_advanced( OPENCL_INCLUDE_DIRS OPENCL_LIBRARIES )
