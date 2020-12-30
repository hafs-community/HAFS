# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

##############################################################################
#.rst:
#
# ecbuild_warn_unused_files
# =========================
#
# Print warnings about unused source files in the project. ::
#
#   ecbuild_warn_unused_files()
#
# If the CMake variable ``CHECK_UNUSED_FILES`` is set, ecBuild will keep track
# of any source files (.c, .cc, .cpp, .cxx) which are not part of a CMake
# target. If set, this macro reports unused files if any have been found. This
# is considered a fatal error unless ``UNUSED_FILES_LEVEL`` is set to a value
# different from ``ERROR``.
#
# .. note ::
#
#   Enabling ``CHECK_UNUSED_FILES`` can slow down the CMake configure time
#   considerably!
#
##############################################################################

macro( ecbuild_warn_unused_files )

    if( PROJECT_NAME STREQUAL CMAKE_PROJECT_NAME ) # only for top level project

      # if cache file with unused files exists remove it
      set( UNUSED_FILE "${CMAKE_BINARY_DIR}/UnusedFiles.txt" )
      if( EXISTS ${UNUSED_FILE} )
              file( REMOVE ${UNUSED_FILE} )
      endif()

      if( CHECK_UNUSED_FILES ) # to check or not to check...

          if( NOT DEFINED UNUSED_FILES_LEVEL ) # to err or not...
              set( UNUSED_FILES_LEVEL "ERROR" )
          endif()

          # if unused files where found, put the list on the file
          if( EC_UNUSED_FILES )

            ecbuild_info("")
            ecbuild_info(" !!!--- ${UNUSED_FILES_LEVEL} ---!!! ")
            ecbuild_info(" !!!--- ${UNUSED_FILES_LEVEL} ---!!! ")
            ecbuild_info("")
            ecbuild_info(" Unused source files found:")
            foreach( AFILE ${EC_UNUSED_FILES} )
              ecbuild_info("     ${AFILE}")
              file( APPEND ${UNUSED_FILE} "${AFILE}\n" )
            endforeach()
            ecbuild_info("")
            ecbuild_info(" List dumped to ${UNUSED_FILE}")
            ecbuild_info("")
            ecbuild_info(" !!!--- ${UNUSED_FILES_LEVEL} ---!!! ")
            ecbuild_info(" !!!--- ${UNUSED_FILES_LEVEL} ---!!! ")
            ecbuild_info("")

            if( UNUSED_FILES_LEVEL STREQUAL "ERROR" )
              ecbuild_critical( "\n Aborted build system configuration. \n Add unused files to the build system or remove them." )
            endif()

          endif()

      endif()

    endif()

endmacro( ecbuild_warn_unused_files )
