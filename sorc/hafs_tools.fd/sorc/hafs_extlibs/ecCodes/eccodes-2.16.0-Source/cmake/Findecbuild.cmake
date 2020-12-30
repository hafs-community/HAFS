# (C) Copyright 2019- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

find_path(ecbuild_MACROS_DIR ecbuild.cmake
    HINTS
        ${CMAKE_CURRENT_LIST_DIR}
    PATH_SUFFIXES
        cmake
        share/cmake
        share/ecbuild/cmake
    NO_DEFAULT_PATH)

if(ecbuild_MACROS_DIR)
    include(${ecbuild_MACROS_DIR}/ecbuild_parse_version.cmake)
    ecbuild_parse_version_file(${ecbuild_MACROS_DIR}/VERSION PREFIX ecbuild)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(ecbuild
    REQUIRED_VARS
        ecbuild_MACROS_DIR
    VERSION_VAR
        ecbuild_VERSION)

if(ecbuild_FOUND)
    include(ecbuild)
endif()
