# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# general options

option( BUILD_SHARED_LIBS       "build shared libraries when possible"            ON  )

option( ENABLE_RPATHS           "when installing insert RPATHS into binaries"     ON  )
option( ENABLE_RELATIVE_RPATHS  "try to use relative RPATHS, including build dir" ON  )
option( ENABLE_WARNINGS         "enable compiler warnings"                        OFF )

option( ENABLE_LARGE_FILE_SUPPORT "build with large file support"   ON  )

option( ENABLE_PROFILING        "build with profiling support" OFF )

mark_as_advanced( ENABLE_LARGE_FILE_SUPPORT )

option( ENABLE_FORTRAN_C_INTERFACE "Enable Fortran/C Interface" OFF )
mark_as_advanced( ENABLE_FORTRAN_C_INTERFACE )

option( DEVELOPER_MODE           "activates developer mode"               OFF )
option( CHECK_UNUSED_FILES       "check for unused project files (slow)"  OFF )

mark_as_advanced( DEVELOPER_MODE  )
mark_as_advanced( CHECK_UNUSED_FILES  )

option( ECBUILD_INSTALL_LIBRARY_HEADERS "Will install library headers" ON )
mark_as_advanced( ECBUILD_INSTALL_LIBRARY_HEADERS )

option( ECBUILD_INSTALL_FORTRAN_MODULES "Will install Fortran modules" ON )
mark_as_advanced( ECBUILD_INSTALL_FORTRAN_MODULES )

include( CMakeDependentOption ) # make options depend on one another

option( ECBUILD_USE_INCLUDE_DIRECTORIES "Forces to use global include_directories() instead of target specific. Adverse effect on PkgConfig generation." OFF )

mark_as_advanced( ECBUILD_USE_INCLUDE_DIRECTORIES )

set( CMAKE_NO_SYSTEM_FROM_IMPORTED ON )

# hide some CMake options from CMake UI

mark_as_advanced( CMAKE_OSX_ARCHITECTURES CMAKE_OSX_DEPLOYMENT_TARGET CMAKE_OSX_SYSROOT )