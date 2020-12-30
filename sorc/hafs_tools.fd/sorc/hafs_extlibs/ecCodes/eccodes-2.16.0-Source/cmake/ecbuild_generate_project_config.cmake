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
# ecbuild_generate_project_config
# ===============================
#
# Generate the <project>-config.cmake file ::
#
#   ecbuild_generate_project_config(<template>
#                                   [FILENAME <filename>]
#                                   [PATH_VARS <var1> ...])
#
# Options
# -------
#
# <template> : required
#   path to the template to use
#
# FILENAME : optional
#   name of the output file
#
# PATH_VARS : optional
#   list of paths to be exported to the config template
#
# Usage
# -----
#
# The PATH_VARS parameter has the same meaning as for the
# configure_package_config_file macro in CMakePackageConfigHelpers:
# the value of ${varN} should be relative to the install directory
# (PROJECT_BINARY_DIR for build-dir export and INSTALL_PREFIX for the installed
# package). A reliable path will be computed and can be evaluated from the
# template through PACKAGE_${varN}.

function(ecbuild_generate_project_config template)
  include(CMakePackageConfigHelpers)

  set(options)
  set(single_value_args FILENAME)
  set(multi_value_args PATH_VARS)
  cmake_parse_arguments(_PAR "${options}" "${single_value_args}" "${multi_value_args}" ${ARGN})

  if(_PAR_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ecbuild_generate_project_config(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
  endif()

  if(NOT _PAR_FILENAME)
    set( LNAME ${PROJECT_NAME_LOWCASE} )
    set(_PAR_FILENAME "${LNAME}-config.cmake")
  endif()

  set(PACKAGE_TARGETS_DIR_REL "\${${PROJECT_NAME}_CMAKE_DIR}")

  file(RELATIVE_PATH BASE_DIR ${PROJECT_BINARY_DIR} ${CMAKE_BINARY_DIR})
  string(REGEX REPLACE "/$" "" BASE_DIR "${BASE_DIR}")
  set(CMAKE_DIR .)
  set(PACKAGE_TARGETS_DIRS ${PACKAGE_TARGETS_DIR_REL} ${PROJECT_BINARY_DIR})
  set(_is_build_dir_export ON)

  configure_package_config_file(${template} ${PROJECT_BINARY_DIR}/${_PAR_FILENAME}
    INSTALL_DESTINATION .
    PATH_VARS BASE_DIR CMAKE_DIR ${PATH_VARS}
    INSTALL_PREFIX ${PROJECT_BINARY_DIR}
  )

  set(BASE_DIR .)
  set(CMAKE_DIR ${INSTALL_CMAKE_DIR})
  set(PACKAGE_TARGETS_DIRS ${PACKAGE_TARGETS_DIR_REL})
  set(_is_build_dir_export OFF)

  configure_package_config_file(${template}
    ${PROJECT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/${_PAR_FILENAME}
    INSTALL_DESTINATION ${INSTALL_CMAKE_DIR}
    PATH_VARS BASE_DIR CMAKE_DIR ${PATH_VARS}
  )
install(FILES "${PROJECT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/${_PAR_FILENAME}" DESTINATION "${INSTALL_CMAKE_DIR}")
endfunction()
