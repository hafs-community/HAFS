# (C) Copyright 2018- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

function(ecbuild_configure_file _PAR_TEMPLATE _PAR_FILENAME)
  get_filename_component(config_file ${_PAR_FILENAME} NAME)
  set(tmp_file "${CMAKE_CURRENT_BINARY_DIR}/${config_file}.ecbuild")

  configure_file( ${_PAR_TEMPLATE} ${tmp_file}  ${ARGN})

  file(STRINGS "${tmp_file}" content)
  string(REGEX MATCH "\\$<INSTALL_INTERFACE:" interface_check "${content}")
  if(interface_check)
    ecbuild_critical("INSTALL_INTERFACE can't be used in a configure file")
  endif()

  if(NOT IS_ABSOLUTE "${_PAR_FILENAME}")
    set(_PAR_FILENAME "${CMAKE_CURRENT_BINARY_DIR}/${_PAR_FILENAME}")
  endif()

  file(GENERATE OUTPUT "${_PAR_FILENAME}" INPUT "${tmp_file}" )
endfunction()
