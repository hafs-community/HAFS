# (C) Copyright 2019- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

##############################################################################
#.rst:
#
# ecbuild_compat_setversion
# ======================
#
# Read a VERSION.cmake file and set the project variables.
#
#   ecbuild_compat_setversion()
#
# Output variables
# ----------------
#
# * <pname>_VERSION_MAJOR
# * <pname>_VERSION_MINOR
# * <pname>_VERSION_PATCH
# * <pname>_VERSION_TWEAK
# * <pname>_VERSION
# * <pname>_VERSION_STR
# * <pname>_VERSION_SUFFIX
#
##############################################################################

macro(ecbuild_compat_setversion)
  # read and parse project version file
  if( EXISTS ${PROJECT_SOURCE_DIR}/VERSION.cmake )
    include( ${PROJECT_SOURCE_DIR}/VERSION.cmake )
    set( __version ${${PROJECT_NAME}_VERSION_STR} )
  else()
    set( __version "0.0.0" )
  endif()

  ecbuild_parse_version("${__version}" PREFIX ${PROJECT_NAME})

  set( ${PROJECT_NAME}_VERSION "${${PROJECT_NAME}_VERSION}" CACHE INTERNAL "package ${PROJECT_NAME} version" )
  set( ${PROJECT_NAME}_VERSION_STR "${${PROJECT_NAME}_VERSION}" CACHE INTERNAL "package ${PROJECT_NAME} version" )
endmacro()
