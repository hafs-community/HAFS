# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set( ECBUILD_GIT  ON  CACHE BOOL "Turn on/off ecbuild_git() function" )

mark_as_advanced(ECBUILD_GIT)

if( ECBUILD_GIT )
  find_package(Git)
endif()

##############################################################################
#.rst:
#
# ecbuild_git
# ===========
#
# Manages an external Git repository. ::
#
#   ecbuild_git( PROJECT <name>
#                DIR <directory>
#                URL <giturl>
#                [ BRANCH <gitbranch> | TAG <gittag> ]
#                [ UPDATE | NOREMOTE ] )
#                [ MANUAL ] )
#
# Options
# -------
#
# PROJECT : required
#   project name for the Git repository to be managed
#
# DIR : required
#   directory to clone the repository into (can be relative)
#
# URL : required
#   Git URL of the remote repository to clone (see ``git help clone``)
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
##############################################################################

macro( ecbuild_git )

  set( options UPDATE NOREMOTE MANUAL )
  set( single_value_args PROJECT DIR URL TAG BRANCH )
  set( multi_value_args )
  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}" ${_FIRST_ARG} ${ARGN} )

  if( DEFINED _PAR_BRANCH AND DEFINED _PAR_TAG )
    ecbuild_critical( "Cannot defined both BRANCH and TAG in macro ecbuild_git" )
  endif()

  if( _PAR_UPDATE AND _PAR_NOREMOTE )
    ecbuild_critical( "Cannot pass both NOREMOTE and UPDATE in macro ecbuild_git" )
  endif()

  if(_PAR_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ecbuild_git(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
  endif()

  if( ECBUILD_GIT )

    set( _needs_switch 0 )

    get_filename_component( ABS_PAR_DIR "${_PAR_DIR}" ABSOLUTE )
    get_filename_component( PARENT_DIR  "${_PAR_DIR}/.." ABSOLUTE )

    ### clone if no directory

    if( NOT EXISTS "${_PAR_DIR}" )

      ecbuild_info( "Cloning ${_PAR_PROJECT} from ${_PAR_URL} into ${_PAR_DIR}...")
      execute_process(
        COMMAND ${GIT_EXECUTABLE} "clone" ${_PAR_URL} ${clone_args} ${_PAR_DIR} "-q"
        RESULT_VARIABLE nok ERROR_VARIABLE error
        WORKING_DIRECTORY "${PARENT_DIR}")
      if(nok)
        ecbuild_critical("${_PAR_DIR} git clone failed:\n  ${GIT_EXECUTABLE} clone ${_PAR_URL} ${clone_args} ${_PAR_DIR} -q\n  ${error}\n")
      endif()
      ecbuild_info( "${_PAR_DIR} retrieved.")
      set( _needs_switch 1 )

    endif()

    ### check current tag and sha1

    if( IS_DIRECTORY "${_PAR_DIR}/.git" )

      execute_process( COMMAND ${GIT_EXECUTABLE} rev-parse HEAD
                       OUTPUT_VARIABLE _sha1 RESULT_VARIABLE nok ERROR_VARIABLE error
                       OUTPUT_STRIP_TRAILING_WHITESPACE
                       WORKING_DIRECTORY "${ABS_PAR_DIR}" )
      if(nok)
        ecbuild_info("git rev-parse HEAD on ${_PAR_DIR} failed:\n ${error}")
      endif()

      execute_process( COMMAND ${GIT_EXECUTABLE} rev-parse --abbrev-ref HEAD
                       OUTPUT_VARIABLE _current_branch RESULT_VARIABLE nok ERROR_VARIABLE error
                       OUTPUT_STRIP_TRAILING_WHITESPACE
                       WORKING_DIRECTORY "${ABS_PAR_DIR}" )
      if( nok OR _current_branch STREQUAL "" )
        ecbuild_info("git rev-parse --abbrev-ref HEAD on ${_PAR_DIR} failed:\n ${error}")
      endif()

      execute_process( COMMAND ${GIT_EXECUTABLE} describe --exact-match --abbrev=0
                       OUTPUT_VARIABLE _current_tag RESULT_VARIABLE nok ERROR_VARIABLE error
                       OUTPUT_STRIP_TRAILING_WHITESPACE  ERROR_STRIP_TRAILING_WHITESPACE
                       WORKING_DIRECTORY "${ABS_PAR_DIR}" )

      if( error MATCHES "no tag exactly matches" OR error MATCHES "No names found" )
        unset( _current_tag )
      else()
        if( nok )
          ecbuild_info("git describe --exact-match --abbrev=0 on ${_PAR_DIR} failed:\n ${error}")
        endif()
      endif()

      if( NOT _current_tag ) # try nother method
        execute_process( COMMAND ${GIT_EXECUTABLE} name-rev --tags --name-only ${_sha1}
                         OUTPUT_VARIABLE _current_tag RESULT_VARIABLE nok ERROR_VARIABLE error
                         OUTPUT_STRIP_TRAILING_WHITESPACE
                         WORKING_DIRECTORY "${ABS_PAR_DIR}" )
        if( nok OR _current_tag STREQUAL "" )
          ecbuild_info("git name-rev --tags --name-only on ${_PAR_DIR} failed:\n ${error}")
        endif()
      endif()

    endif()

    if( NOT _PAR_MANUAL AND DEFINED _PAR_BRANCH AND NOT "${_current_branch}" STREQUAL "${_PAR_BRANCH}" )
      set( _needs_switch 1 )
    endif()

    if( NOT _PAR_MANUAL AND DEFINED _PAR_TAG AND NOT "${_current_tag}" STREQUAL "${_PAR_TAG}" )
      set( _needs_switch 1 )
    endif()

    if( DEFINED _PAR_BRANCH AND _PAR_UPDATE AND NOT _PAR_NOREMOTE )

      add_custom_target( git_update_${_PAR_PROJECT}
                         COMMAND "${GIT_EXECUTABLE}" pull -q
                         WORKING_DIRECTORY "${ABS_PAR_DIR}"
                         COMMENT "git pull of branch ${_PAR_BRANCH} on ${_PAR_DIR}" )

      set( git_update_targets "git_update_${_PAR_PROJECT};${git_update_targets}" )

    endif()

    ### updates

    if( _needs_switch AND IS_DIRECTORY "${_PAR_DIR}/.git" )

      if( DEFINED _PAR_BRANCH )
        set ( _gitref ${_PAR_BRANCH} )
        ecbuild_info("Updating ${_PAR_PROJECT} to head of BRANCH ${_PAR_BRANCH}...")
      else()
        ecbuild_info("Updating ${_PAR_PROJECT} to TAG ${_PAR_TAG}...")
        set ( _gitref ${_PAR_TAG} )
      endif()

      # fetching latest tags and branches

      if( NOT _PAR_NOREMOTE )

        ecbuild_info("git fetch --all @ ${ABS_PAR_DIR}")
        execute_process( COMMAND "${GIT_EXECUTABLE}" fetch --all -q
                         RESULT_VARIABLE nok ERROR_VARIABLE error
                         WORKING_DIRECTORY "${ABS_PAR_DIR}")
        if(nok)
          ecbuild_warn("git fetch --all in ${_PAR_DIR} failed:\n ${error}")
        endif()

        ecbuild_info("git fetch --all --tags @ ${ABS_PAR_DIR}")
        execute_process( COMMAND "${GIT_EXECUTABLE}" fetch --all --tags -q
                         RESULT_VARIABLE nok ERROR_VARIABLE error
                         WORKING_DIRECTORY "${ABS_PAR_DIR}")
        if(nok)
          ecbuild_warn("git fetch --all --tags in ${_PAR_DIR} failed:\n ${error}")
        endif()

      else()
        ecbuild_info("${_PAR_DIR} marked NOREMOTE : Skipping git fetch")
      endif()

      # checking out gitref

      ecbuild_info("git checkout ${_gitref} @ ${ABS_PAR_DIR}")
      execute_process( COMMAND "${GIT_EXECUTABLE}" checkout -q "${_gitref}"
                       RESULT_VARIABLE nok ERROR_VARIABLE error
                       WORKING_DIRECTORY "${ABS_PAR_DIR}")
      if(nok)
        ecbuild_critical("git checkout ${_gitref} on ${_PAR_DIR} failed:\n  ${GIT_EXECUTABLE} checkout -q ${_gitref}\n  ${error}")
      endif()

      if( DEFINED _PAR_BRANCH AND _PAR_UPDATE ) #############################################################################

        # Use git pull --ff-only, we WANT this to fail on upstream rebase and
        # we DON'T want merge commits here!
        execute_process( COMMAND "${GIT_EXECUTABLE}" pull -q --ff-only
                         RESULT_VARIABLE nok ERROR_VARIABLE error
                         WORKING_DIRECTORY "${ABS_PAR_DIR}")
        if(nok)
          ecbuild_critical("git pull of branch ${_PAR_BRANCH} on ${_PAR_DIR} failed:\n ${error}")
        endif()

      endif() ####################################################################################

    endif( _needs_switch AND IS_DIRECTORY "${_PAR_DIR}/.git" )

  endif( ECBUILD_GIT )

endmacro()

