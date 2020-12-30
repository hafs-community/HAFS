
set( ECMWF_USER $ENV{USER} CACHE STRING "ECMWF git user" )
set( ECMWF_GIT  SSH        CACHE STRING "ECMWF git protocol" )

set( ECMWF_GIT_SSH   "ssh://git@git.ecmwf.int"            CACHE INTERNAL "ECMWF ssh address" )
set( ECMWF_GIT_HTTPS "https://${ECMWF_USER}@git.ecmwf.int/scm" CACHE INTERNAL "ECMWF https address" )

if( ECMWF_GIT MATCHES "[Ss][Ss][Hh]" )
  set( ECMWF_GIT_ADDRESS ${ECMWF_GIT_SSH} CACHE INTERNAL "" )
else()
  set( ECMWF_GIT_ADDRESS ${ECMWF_GIT_HTTPS} CACHE INTERNAL "" )
endif()

##############################################################################
#.rst:
#
# ecmwf_stash
# =============
#
# Manages an external Git repository on ECMWF Stash. ::
#
#   ecmwf_stash( PROJECT <name>
#                DIR <directory>
#                STASH <repository>
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
# STASH : required
#   Stash repository in the form <project>/<repository>
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

macro( ecmwf_stash )

  set( options )
  set( single_value_args STASH )
  set( multi_value_args )
  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}" ${_FIRST_ARG} ${ARGN} )

  ecbuild_git( URL "${ECMWF_GIT_ADDRESS}/${_PAR_STASH}.git" ${_PAR_UNPARSED_ARGUMENTS} )

endmacro()
