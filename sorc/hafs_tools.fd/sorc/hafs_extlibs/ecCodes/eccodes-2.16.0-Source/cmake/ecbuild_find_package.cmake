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
# ecbuild_find_package
# ====================
#
# Find a package and import its configuration. ::
#
#   ecbuild_find_package( NAME <name>
#                         [ VERSION <version> [ EXACT ] ]
#                         [ COMPONENTS <component1> [ <component2> ... ] ]
#                         [ URL <url> ]
#                         [ DESCRIPTION <description> ]
#                         [ TYPE <type> ]
#                         [ PURPOSE <purpose> ]
#                         [ FAILURE_MSG <message> ]
#                         [ REQUIRED ]
#                         [ QUIET ] )
#
# Options
# -------
#
# NAME : required
#   package name (used as ``Find<name>.cmake`` and ``<name>-config.cmake``)
#
# VERSION : optional
#   minimum required package version
#
# COMPONENTS : optional
#   list of package components to find (behaviour depends on the package)
#
# EXACT : optional, requires VERSION
#   require the exact version rather than a minimum version
#
# URL : optional
#   homepage of the package (shown in summary and stored in the cache)
#
# DESCRIPTION : optional
#   literal string or name of CMake variable describing the package
#
# TYPE : optional, one of RUNTIME|OPTIONAL|RECOMMENDED|REQUIRED
#   type of dependency of the project on this package (defaults to OPTIONAL)
#
# PURPOSE : optional
#   literal string or name of CMake variable describing which functionality
#   this package enables in the project
#
# FAILURE_MSG : optional
#   literal string or name of CMake variable containing a message to be
#   appended to the failure message if the package is not found
#
# REQUIRED : optional
#   fail if package cannot be found
#
# QUIET : optional
#   do not output package information if found
#
# Input variables
# ---------------
#
# The following CMake variables influence the behaviour if set (``<name>`` is
# the package name as given, ``<NAME>`` is the capitalised version):
#
# :DEVELOPER_MODE: if enabled, discover projects parallel in the build tree
# :<name>_PATH:    install prefix path of the package
# :<NAME>_PATH:    install prefix path of the package
# :<name>_DIR:     directory containing the ``<name>-config.cmake`` file
#                  (usually ``<install-prefix>/lib/cmake/<name>``)
#
# The environment variables ``<name>_PATH``, ``<NAME>_PATH``, ``<name>_DIR``
# are taken into account only if the corresponding CMake variables are unset.
#
# Usage
# -----
#
# The search proceeds as follows:
#
# 1.  If any paths have been specified by the user via CMake or environment
#     variables as given above or a parallel build tree has been discovered in
#     DEVELOPER_MODE:
#
#     * search for ``<name>-config.cmake`` in those paths only
#     * search using ``Find<name>.cmake`` (which should respect those paths)
#     * fail if the package was not found in any of those paths
#
# 2.  Search for ``<name>-config.cmake`` in the ``CMAKE_PREFIX_PATH`` and if
#     DEVELOPER_MODE is enabled also in the user package registry.
#
# 3.  Search system paths for ``<name>-config.cmake``.
#
# 4.  Search system paths using ``Find<name>.cmake``.
#
# 5.  If the package was found, and a minimum version was requested, check if
#     the version is acceptable and if not, unset ``<NAME>_FOUND``.
#
# 6.  Fail if the package was not found and is REQUIRED.
#
##############################################################################

macro( ecbuild_find_package )

  set( options REQUIRED QUIET EXACT )
  set( single_value_args NAME VERSION URL DESCRIPTION TYPE PURPOSE FAILURE_MSG )
  set( multi_value_args COMPONENTS )

  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if(_PAR_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ecbuild_find_package(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
  endif()

  if( NOT _PAR_NAME  )
    ecbuild_critical("The call to ecbuild_find_package() doesn't specify the NAME.")
  endif()

  if( _PAR_EXACT AND NOT _PAR_VERSION )
    ecbuild_critical("Call to ecbuild_find_package() requests EXACT but doesn't specify VERSION.")
  endif()

  if( _PAR_QUIET )
    set( _find_quiet QUIET )
  endif()

  # If the package is required, set TYPE to REQUIRED
  # Due to shortcomings in CMake's argument parser, passing TYPE REQUIRED has no effect
  if( _PAR_REQUIRED )
    set( _PAR_TYPE REQUIRED )
  endif()

  # ecbuild_debug_var( _PAR_NAME )

  string( TOUPPER ${_PAR_NAME} pkgUPPER )
  string( TOLOWER ${_PAR_NAME} pkgLOWER )

  set( _${pkgUPPER}_version "" )
  if( _PAR_VERSION )
    set( _${pkgUPPER}_version ${_PAR_VERSION} )
    if( _PAR_EXACT )
      set( _${pkgUPPER}_version ${_PAR_VERSION} EXACT )
    endif()
  endif()

  # check developer mode (search in cmake cache )

  if( NOT ${DEVELOPER_MODE} )
    ecbuild_debug("ecbuild_find_package(${_PAR_NAME}): Not in DEVELOPER_MODE - do not search package registry or recent GUI build paths")
    set( NO_DEV_BUILD_DIRS NO_CMAKE_PACKAGE_REGISTRY NO_CMAKE_BUILDS_PATH )
  endif()

  # in DEVELOPER_MODE we give priority to projects parallel in the build tree
  # so lets prepend a parallel build tree to the search path if we find it

  if( DEVELOPER_MODE )
    get_filename_component( _proj_bdir "${CMAKE_BINARY_DIR}/../${pkgLOWER}" ABSOLUTE )
    ecbuild_debug("ecbuild_find_package(${_PAR_NAME}): in DEVELOPER_MODE - searching for ${pkgLOWER}-config.cmake in ${_proj_bdir}")
    if( EXISTS ${_proj_bdir}/${pkgLOWER}-config.cmake )
      ecbuild_debug("ecbuild_find_package(${_PAR_NAME}): in DEVELOPER_MODE - found parallel build tree in ${_proj_bdir}")
      if( ${pkgUPPER}_PATH )
        ecbuild_debug("ecbuild_find_package(${_PAR_NAME}): in DEVELOPER_MODE - ${pkgUPPER}_PATH already set to ${${pkgUPPER}_PATH}, not modifying")
      else()
        ecbuild_debug("ecbuild_find_package(${_PAR_NAME}): in DEVELOPER_MODE - setting ${pkgUPPER}_PATH to ${_proj_bdir}")
        set( ${pkgUPPER}_PATH "${_proj_bdir}" )
      endif()
    endif()
  endif()

  # Read environment variables but ONLY if the corresponding CMake variables are unset

  if( NOT DEFINED ${pkgUPPER}_PATH AND NOT "$ENV{${pkgUPPER}_PATH}" STREQUAL "" )
    set( ${pkgUPPER}_PATH "$ENV{${pkgUPPER}_PATH}" )
    ecbuild_debug("ecbuild_find_package(${_PAR_NAME}): setting ${pkgUPPER}_PATH=${${pkgUPPER}_PATH} from environment")
  endif()

  if( NOT DEFINED ${_PAR_NAME}_PATH AND NOT "$ENV{${_PAR_NAME}_PATH}" STREQUAL "" )
    set( ${_PAR_NAME}_PATH "$ENV{${_PAR_NAME}_PATH}" )
    ecbuild_debug("ecbuild_find_package(${_PAR_NAME}): setting ${_PAR_NAME}_PATH=${${_PAR_NAME}_PATH} from environment")
  endif()

  # XXX: the <package>_DIR variable is handled later, see explanation below

  # Find packages quietly unless in DEVELOPER_MODE or LOG_LEVEL is DEBUG

  if( NOT DEVELOPER_MODE AND ( ECBUILD_LOG_LEVEL GREATER ${ECBUILD_DEBUG} ) )
    set( _find_quiet QUIET )
  endif()

  if( ECBUILD_2_COMPAT )
    # Disable deprecation warnings until ecbuild_mark_compat, because "<PROJECT>_FOUND" may already have been
    #   marked with "ecbuild_mark_compat()" in a bundle.
    set( DISABLE_ECBUILD_DEPRECATION_WARNINGS_orig ${DISABLE_ECBUILD_DEPRECATION_WARNINGS} )
    set( DISABLE_ECBUILD_DEPRECATION_WARNINGS ON )
  endif()

  # cancel the effect of ecbuild_install_project setting <package>_FOUND in
  # compat mode (otherwise this means the <package>-config.cmake file may not
  # always be loaded, see ECBUILD-401)
  if( ECBUILD_2_COMPAT )
    unset( ${_PAR_NAME}_FOUND )
  endif()

  # if a project with the same name has been defined, try to use it

  if( ${_PAR_NAME}_BINARY_DIR )

    # 1) search using CONFIG mode -- try to locate a configuration file provided by the package (package-config.cmake)
    #    <package>_BINARY_DIR is defined by CMake when using project()

    if( NOT ${_PAR_NAME}_FOUND )
      ecbuild_debug("ecbuild_find_package(${_PAR_NAME}): 1) search using CONFIG mode -- try to locate ${_PAR_NAME}-config.cmake")
      ecbuild_debug("ecbuild_find_package(${_PAR_NAME}):    using hints ${_PAR_NAME}_BINARY_DIR=${${_PAR_NAME}_BINARY_DIR}")
      find_package( ${_PAR_NAME} ${_${pkgUPPER}_version} NO_MODULE ${_find_quiet}
        COMPONENTS ${_PAR_COMPONENTS}
        HINTS ${${_PAR_NAME}_BINARY_DIR}
        NO_DEFAULT_PATH )
    endif()

    if( NOT ${_PAR_NAME}_FOUND )
      if( ${_PAR_NAME}_CONSIDERED_VERSIONS )
        ecbuild_critical( "${_PAR_NAME} was found in the source tree but no suitable version (or component set) was found at '${${_PAR_NAME}_BINARY_DIR}'" )
      else()
        ecbuild_critical( "${_PAR_NAME} was found in the source tree but could not be loaded from '${${_PAR_NAME}_BINARY_DIR}'" )
      endif()
    endif()

  endif()

  if( NOT ${_PAR_NAME}_FOUND )
    # XXX: if set, <package>_DIR short-circuits most of the options one can give
    # to find_package, do NOT move above the previous find_package
    if( NOT DEFINED ${_PAR_NAME}_DIR AND NOT "$ENV{${_PAR_NAME}_DIR}" STREQUAL "" )
      set( ${_PAR_NAME}_DIR "$ENV{${_PAR_NAME}_DIR}" )
      ecbuild_debug("ecbuild_find_package(${_PAR_NAME}): setting ${_PAR_NAME}_DIR=${${_PAR_NAME}_DIR} from environment")
    endif()
  endif()

  # search user defined paths

  if( ${_PAR_NAME}_PATH OR ${pkgUPPER}_PATH OR ${_PAR_NAME}_DIR )

    # 2) search using CONFIG mode -- try to locate a configuration file provided by the package (package-config.cmake)

    if( NOT ${_PAR_NAME}_FOUND )
      ecbuild_debug("ecbuild_find_package(${_PAR_NAME}): 2) search using CONFIG mode -- try to locate ${_PAR_NAME}-config.cmake")
      ecbuild_debug("ecbuild_find_package(${_PAR_NAME}):    using hints ${pkgUPPER}_PATH=${${pkgUPPER}_PATH}, ${_PAR_NAME}_PATH=${${_PAR_NAME}_PATH}, ${_PAR_NAME}_DIR=${${_PAR_NAME}_DIR}")
      find_package( ${_PAR_NAME} ${_${pkgUPPER}_version} NO_MODULE ${_find_quiet}
        COMPONENTS ${_PAR_COMPONENTS}
        HINTS ${${pkgUPPER}_PATH} ${${_PAR_NAME}_PATH} ${${_PAR_NAME}_DIR}
        NO_DEFAULT_PATH )
      set( _cfg_considered_versions ${${_PAR_NAME}_CONSIDERED_VERSIONS} )
    endif()

    # 3) search using a file Find<package>.cmake if it exists ( macro should itself take *_PATH into account )

    if( NOT ${_PAR_NAME}_FOUND )
      ecbuild_debug("ecbuild_find_package(${_PAR_NAME}): 3) search using a file Find${_PAR_NAME}.cmake if it exists")
      find_package( ${_PAR_NAME} ${_${pkgUPPER}_version} MODULE ${_find_quiet}
                    COMPONENTS ${_PAR_COMPONENTS} )
    endif()

    # is <package>_PATH was given and we don't find anything then we FAIL

    if( NOT ${_PAR_NAME}_FOUND )
      if( _cfg_considered_versions OR ${_PAR_NAME}_CONSIDERED_VERSIONS )
        if( ${_PAR_NAME}_PATH )
          ecbuild_critical( "${_PAR_NAME}_PATH was provided by user but no suitable version (or component set) of ${_PAR_NAME} was found at '${${_PAR_NAME}_PATH}'" )
        endif()
        if( ${pkgUPPER}_PATH )
          ecbuild_critical( "${pkgUPPER}_PATH was provided by user but no suitable version (or component set) of ${_PAR_NAME} was found at '${${pkgUPPER}_PATH}'" )
        endif()
      else()
        if( ${_PAR_NAME}_PATH )
          ecbuild_critical( "${_PAR_NAME}_PATH was provided by user but package ${_PAR_NAME} wasn't found at '${${_PAR_NAME}_PATH}'" )
        endif()
        if( ${pkgUPPER}_PATH )
          ecbuild_critical( "${pkgUPPER}_PATH was provided by user but package ${_PAR_NAME} wasn't found at '${${pkgUPPER}_PATH}'" )
        endif()
      endif()
    endif()

  endif()

  # 4) search developer cache and recently configured packages in the CMake GUI if in DEVELOPER_MODE
  #    otherwise only search CMAKE_PREFIX_PATH and system paths, for <package>-config.cmake

  if( NOT ${_PAR_NAME}_FOUND )
    if (NO_DEV_BUILD_DIRS)
      ecbuild_debug("ecbuild_find_package(${_PAR_NAME}): 4) search CMAKE_PREFIX_PATH and system paths")
    else()
      ecbuild_debug("ecbuild_find_package(${_PAR_NAME}): 4) search CMAKE_PREFIX_PATH, system paths, and package registry")
    endif()

    find_package( ${_PAR_NAME} ${_${pkgUPPER}_version} ${_find_quiet} NO_MODULE
      COMPONENTS ${_PAR_COMPONENTS}
      ${NO_DEV_BUILD_DIRS} )

  endif()

  # 5) search system paths, using Find<package>.cmake if it exists

  if( NOT ${_PAR_NAME}_FOUND )
    ecbuild_debug("ecbuild_find_package(${_PAR_NAME}): 5) search system paths, using Find${_PAR_NAME}.cmake if it exists")

    find_package( ${_PAR_NAME} ${_${pkgUPPER}_version} ${_find_quiet} MODULE
                  COMPONENTS ${_PAR_COMPONENTS} )

  endif()

  if(ECBUILD_2_COMPAT)
    ecbuild_declare_compat(${pkgUPPER}_FOUND ${_PAR_NAME}_FOUND)
  endif()

  if( ECBUILD_2_COMPAT )
    set( DISABLE_ECBUILD_DEPRECATION_WARNINGS ${DISABLE_ECBUILD_DEPRECATION_WARNINGS_orig} )
  endif()

  ### final messages

  if( ${_PAR_NAME}_FOUND )

    if( NOT _PAR_QUIET )
      ecbuild_info( "[${_PAR_NAME}] (${${_PAR_NAME}_VERSION})" )
      foreach( var IN ITEMS INCLUDE_DIRS INCLUDE_DIR )
        if( ${pkgUPPER}_${var} )
          ecbuild_info( "   ${pkgUPPER}_${var} : [${${pkgUPPER}_${var}}]" )
          break()
        endif()
        if( ${_PAR_NAME}_${var} )
          ecbuild_info( "   ${_PAR_NAME}_${var} : [${${_PAR_NAME}_${var}}]" )
          break()
        endif()
      endforeach()
      foreach( var IN ITEMS LIBRARIES LIBRARY )
        if( ${pkgUPPER}_${var} )
          ecbuild_info( "   ${pkgUPPER}_${var} : [${${pkgUPPER}_${var}}]" )
          break()
        endif()
        if( ${_PAR_NAME}_${var} )
          ecbuild_info( "   ${_PAR_NAME}_${var} : [${${_PAR_NAME}_${var}}]" )
          break()
        endif()
      endforeach()
      foreach( var IN ITEMS DEFINITIONS )
        if( ${pkgUPPER}_${var} )
          ecbuild_info( "   ${pkgUPPER}_${var} : [${${pkgUPPER}_${var}}]" )
          break()
        endif()
        if( ${_PAR_NAME}_${var} )
          ecbuild_info( "   ${_PAR_NAME}_${var} : [${${_PAR_NAME}_${var}}]" )
          break()
        endif()
      endforeach()
    endif()

    if( DEFINED ${_PAR_DESCRIPTION} )
      set( _PAR_DESCRIPTION ${${_PAR_DESCRIPTION}} )
    endif()
    if( DEFINED ${_PAR_PURPOSE} )
      set( _PAR_PURPOSE ${${_PAR_PURPOSE}} )
    endif()
    set_package_properties( ${_PAR_NAME} PROPERTIES
                            URL "${_PAR_URL}"
                            DESCRIPTION "${_PAR_DESCRIPTION}"
                            TYPE "${_PAR_TYPE}"
                            PURPOSE "${_PAR_PURPOSE}" )

  else()

    if( DEFINED ${_PAR_FAILURE_MSG} )
      set( _PAR_FAILURE_MSG ${${_PAR_FAILURE_MSG}} )
    endif()
    set( _failed_message
      "  ${PROJECT_NAME} FAILED to find package ${_PAR_NAME}\n"
      "    Provide location by defining \"-DCMAKE_PREFIX_PATH= /...\" or with \"-D${pkgUPPER}_PATH=/...\" or \"-D${_PAR_NAME}_DIR=/...\" \n"
      "    You may also export environment variables ${pkgUPPER}_PATH or ${_PAR_NAME}_DIR\n"
      "  Values (note CAPITALISATION):\n"
      "    ${pkgUPPER}_PATH should contain the path to the install prefix (as in <install>/bin <install>/lib <install>/include)\n"
      "    ${_PAR_NAME}_DIR should be a directory containing a <package>-config.cmake file (usually <install>/lib/cmake/<package>)\n"
      )

    if( _PAR_REQUIRED )
      ecbuild_critical( "${_failed_message}!! ${PROJECT_NAME} requires package ${_PAR_NAME} !!\n${_PAR_FAILURE_MSG}" )
    else()
      if( NOT _PAR_QUIET )
        ecbuild_warn( "${_failed_message}\n${_PAR_FAILURE_MSG}" )
      endif()
    endif()

  endif()

endmacro()
