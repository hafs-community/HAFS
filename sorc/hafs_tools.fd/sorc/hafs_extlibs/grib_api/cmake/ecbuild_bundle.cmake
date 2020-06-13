# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

# Set policies
include( ecbuild_policies NO_POLICY_SCOPE )

include(CMakeParseArguments)

include(ecbuild_git)

##############################################################################
#.rst:
#
# ecbuild_bundle_initialize
# =========================
#
# Initialise the ecBuild environment for a bundle. *Must* be called *before*
# any call to ``ecbuild_bundle``. ::
#
#   ecbuild_bundle_initialize()
#
##############################################################################

macro( ecbuild_bundle_initialize )

  include( local-config.cmake OPTIONAL )

  # ecmwf_stash( PROJECT ecbuild DIR ${PROJECT_SOURCE_DIR}/ecbuild STASH "ecsdk/ecbuild" BRANCH develop )

  # set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/ecbuild/cmake;${CMAKE_MODULE_PATH}" )

  include( ecbuild_system )

  ecbuild_requires_macro_version( 1.6 )

  ecbuild_declare_project()

  file( GLOB local_config_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *local-config.cmake )

  ecbuild_add_resources( TARGET ecbuild_bundle_dont_pack DONT_PACK "${local_config_files}" )

  if( EXISTS "${PROJECT_SOURCE_DIR}/README.md" )
    add_custom_target( ${PROJECT_NAME}_readme SOURCES "${PROJECT_SOURCE_DIR}/README.md" )
  endif()

endmacro()

##############################################################################
#.rst:
#
# ecbuild_bundle
# ==============
#
# Declare a subproject to be built as part of this bundle. ::
#
#   ecbuild_bundle( PROJECT <name>
#                   STASH <repository> | GIT <giturl> | SOURCE <path>
#                   [ BRANCH <gitbranch> | TAG <gittag> ]
#                   [ UPDATE | NOREMOTE ] )
#                   [ MANUAL ] )
#
# Options
# -------
#
# PROJECT : required
#   project name for the Git repository to be managed
#
# STASH : cannot be combined with GIT or SOURCE
#   Stash repository in the form <project>/<repository>
#
# GIT : cannot be combined with STASH or SOURCE
#   Git URL of the remote repository to clone (see ``git help clone``)
#
# SOURCE : cannot be combined with STASH or GIT
#   Path to an existing local repository, which will be symlinked
#
# BRANCH : optional, cannot be combined with TAG
#   Git branch to check out
#
# TAG : optional, cannot be combined with BRANCH
#   Git tag or commit id to check out
#
# UPDATE : optional, requires BRANCH, cannot be combined with NOREMOTE
#   Create a CMake target update to fetch changes from the remote repository
#
# NOREMOTE : optional, cannot be combined with UPDATE
#   Do not fetch changes from the remote repository
#
# MANUAL : optional
#   Do not automatically switch branches or tags
#
# Usage
# -----
#
# A bundle is used to build a number of projects together. Each subproject
# needs to be declared with a call to ecbuild_bundle, where the order of
# projects is important and needs to respect dependencies: if project B
# depends on project A, A should be listed before B in the bundle.
#
# The first time a bundle is built, the sources of all subprojects are cloned
# into directories named according to project in the *source* tree of the
# bundle (which means these directories should be added to ``.gitignore``).
# If the ``SOURCE`` option is used it must point to an existing local
# repository on disk and no new repository is cloned. Be aware that using the
# ``BRANCH`` or ``TAG`` option leads to the corresponding version being checked
# out in that repository!
#
# Subprojects are configured and built in order. Due to being added as a
# subproject, the usual project discovery mechanism (i.e. locating and
# importing a ``<project>-config.cmake`` file) is not used. Also there are no
# ``<project>-config.cmake`` files being generated for individual subprojects.
# However there *are* package-config files being generated for each library.
#
# To switch off a subproject when building a bundle, set the CMake variable
# ``BUNDLE_SKIP_<PNAME>`` where ``PNAME`` is the capitalised project name.
#
##############################################################################

macro( ecbuild_bundle )

  set( options )
  set( single_value_args PROJECT STASH GIT SOURCE )
  set( multi_value_args )
  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}" ${_FIRST_ARG} ${ARGN} )

  string(TOUPPER "${_PAR_PROJECT}" PNAME)

  ecbuild_info( "---------------------------------------------------------" )

  if( BUNDLE_SKIP_${PNAME} )
    ecbuild_info( "Skipping bundle project ${_PAR_PROJECT}" )
  else()
    ecbuild_info( "Adding bundle project ${_PAR_PROJECT}" )

    if( _PAR_STASH )
      ecmwf_stash( PROJECT ${_PAR_PROJECT} DIR ${PROJECT_SOURCE_DIR}/${_PAR_PROJECT} STASH ${_PAR_STASH} ${_PAR_UNPARSED_ARGUMENTS} )
    elseif( _PAR_GIT )
      ecbuild_git( PROJECT ${_PAR_PROJECT} DIR ${PROJECT_SOURCE_DIR}/${_PAR_PROJECT} URL ${_PAR_GIT} ${_PAR_UNPARSED_ARGUMENTS} )
    elseif( _PAR_SOURCE )
      if( DEFINED ${PNAME}_SOURCE )
        ecbuild_critical( "ecbuild_bundle called with SOURCE for project ${_PAR_PROJECT} but ${PNAME}_SOURCE is defined" )
      endif()
      execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink ${_PAR_SOURCE} ${PROJECT_SOURCE_DIR}/${_PAR_PROJECT} )
    endif()

    if( NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${_PAR_PROJECT} OR NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${_PAR_PROJECT}/CMakeLists.txt )
      ecbuild_critical("Source directory '${CMAKE_CURRENT_SOURCE_DIR}/${_PAR_PROJECT}' for subproject '${_PAR_PROJECT}' does not exist or does not contain a CMakeLists.txt file.")
    endif()

    # Do not descend into ecbuild if included in a bundle (ECBUILD-333)
    if( NOT _PAR_PROJECT STREQUAL "ecbuild" )
      ecbuild_use_package( PROJECT ${_PAR_PROJECT} )
    endif()
  endif()

endmacro()

##############################################################################
#.rst:
#
# ecbuild_bundle_finalize
# =======================
#
# Finalise the ecBuild environment for a bundle. *Must* be called *after* the
# last call to ``ecbuild_bundle``. ::
#
#   ecbuild_bundle_finalize()
#
# Options
# -------
#
# See documentation for ecbuild_install_project() since all arguments are
# forwarded to an internal call to that macro.
#
# If no arguments are passed, then the default installation NAME is set to
# the default project name ${CMAKE_PROJECT_NAME}
#
##############################################################################

macro( ecbuild_bundle_finalize )

  add_custom_target( update DEPENDS ${git_update_targets} )

  ecbuild_info("---------------------------------------------------------")
  ecbuild_info("Bundle ${CMAKE_PROJECT_NAME}")
  ecbuild_info("---------------------------------------------------------")

  if("${ARGV1}")
      ecbuild_install_project( ${ARGV} )
  else()
      ecbuild_install_project( NAME ${CMAKE_PROJECT_NAME} )
  endif()

  ecbuild_print_summary()

endmacro()
