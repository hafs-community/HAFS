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
# ecbuild_compat_tplconfig
# ======================
#
# Generate a config file to import the third-party libraries (TPL) automatically ::
#
#   ecbuild_compat_tplconfig(<filename>
#                            TPLS tpl1 tpl2 ...)
#
# Options
# -------
# <filename> : required
#   name of the output file
#
# TPLS : optional
#   list of third-party dependencies
#
# Usage
# -----
#
# This function is intended for use in ecbuild_install_project only. Do NOT use
# it in new code.

function(ecbuild_compat_tplconfig cfile)
  set(options)
  set(single_value_args)
  set(multi_value_args TPLS)

  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if(_PAR_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ecbuild_compat_tplconfig(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
  endif()

  if(_PAR_TPLS)
    file(REMOVE ${cfile}.in)
    file(APPEND "${cfile}.in" "include(CMakeFindDependencyMacro)\n")

    foreach(_tpl ${_PAR_TPLS})
      string(TOUPPER ${_tpl} TPL)

      if(${TPL}_IMPORT_FILE) # ecBuild packages should trigger this if they export themselves

        ecbuild_debug("Adding find_dependency call for TPL ${_tpl} to ${cfile}.in")
        get_filename_component(__import_dir "${${TPL}_IMPORT_FILE}" DIRECTORY)
        file(RELATIVE_PATH __import_dir_rel "${${PROJECT_NAME}_FULL_INSTALL_CMAKE_DIR}" "${__import_dir}")
        set(__import_dir_rel "\${CMAKE_CURRENT_LIST_DIR}/${__import_dir_rel}")
        file(APPEND "${cfile}.in" "if(NOT ${TPL}_IMPORT_FILE)\n")
        file(APPEND "${cfile}.in" "    find_dependency(${_tpl} REQUIRED HINTS \"${__import_dir_rel}\" \"${__import_dir}\")\n")
        file(APPEND "${cfile}.in" "endif()\n")

      elseif(${_tpl}_CONFIG) # cmake built packages (e.g. CGAL) may have exported their targets

        ecbuild_debug("Adding TPL ${_tpl} import file to ${cfile}.in")
        set(__import_file "${${_tpl}_CONFIG}")
        get_filename_component(__import_dir "${__import_file}" DIRECTORY)
        file(APPEND "${cfile}.in" "if(NOT ${_tpl}_CONFIG)\n")
        file(APPEND "${cfile}.in" "    find_dependency(${_tpl} REQUIRED HINTS \"${__import_dir}\")\n")
        file(APPEND "${cfile}.in" "    set(${_tpl}_CONFIG \"${__import_file}\")\n")
        file(APPEND "${cfile}.in" "endif()\n")

      elseif(${_tpl}_FULL_INSTALL_CMAKE_DIR)
        # This variable is only available for a ecbuild exported TPL in a bundle. It is therefore safe to use
        # relative install paths between this project and the TPL

        ecbuild_debug("Adding find_dependency call for TPL ${_tpl} to ${cfile}.in")
        file(RELATIVE_PATH __import_dir "${${PROJECT_NAME}_FULL_INSTALL_CMAKE_DIR}" "${${_tpl}_FULL_INSTALL_CMAKE_DIR}")
        set(__import_dir "\${CMAKE_CURRENT_LIST_DIR}/${__import_dir}")
        file(APPEND "${cfile}.in" "find_dependency(${_tpl} REQUIRED HINTS \"${__import_dir}\")\n")

      endif()
    endforeach()

    configure_file("${cfile}.in" "${cfile}" @ONLY)
    install(FILES "${cfile}" DESTINATION "${INSTALL_CMAKE_DIR}")
  endif()
endfunction()
