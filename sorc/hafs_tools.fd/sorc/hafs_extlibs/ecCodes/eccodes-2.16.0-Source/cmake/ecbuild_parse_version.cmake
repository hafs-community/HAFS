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
# ecbuild_parse_version
# =====================
#
# Parse version string of the form "<major>[.<minor>[.<patch>[.<tweak>]]][<suffix>]" ::
#
#   ecbuild_parse_version( <version_str> [ PREFIX <prefix> ] )
#
# Options
# -------
#
# PREFIX : optional
#   string to be prefixed to all defined variables. If not given,
#   the value "_" will be used.
#
# Notes
# -----
#
# Following variables if possible:
#
#      <prefix>_VERSION_STR     = <major>[.<minor>[.<patch>[.<tweak>]]][<suffix>]
#      <prefix>_VERSION         = <major>[.<minor>[.<patch>[.<tweak>]]]
#      <prefix>_VERSION_MAJOR   = <major>
#      <prefix>_VERSION_MINOR   = <minor>
#      <prefix>_VERSION_PATCH   = <patch>
#      <prefix>_VERSION_TWEAK   = <tweak>
#      <prefix>_VERSION_SUFFIX  = <suffix>
#

function( ecbuild_parse_version version_str )
  set( options )
  set( single_value_args PREFIX )
  set( multi_value_args )

  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}" ${ARGN} )

  if( NOT _PAR_PREFIX )
    set( prefix "_" )
  else()
    set( prefix ${_PAR_PREFIX} )
  endif()

  ## Parse version_str
  set( ${prefix}_VERSION_STR "${version_str}" )
  string( REGEX REPLACE "^((([0-9]+)\\.)+([0-9]+)).*" "\\1" ${prefix}_VERSION "${version_str}" )
  string( LENGTH "${${prefix}_VERSION}" ver_len )
  string( SUBSTRING "${version_str}" ${ver_len} -1 ${prefix}_VERSION_SUFFIX )
  string( REPLACE "." " " _version_list ${${prefix}_VERSION} ) # dots to spaces
  separate_arguments( _version_list )
  list (LENGTH _version_list _len)
  if( ${_len} GREATER 0 )
    list( GET _version_list 0 ${prefix}_VERSION_MAJOR )
  endif()
  if( ${_len} GREATER 1 )
    list( GET _version_list 1 ${prefix}_VERSION_MINOR )
  endif()
  if( ${_len} GREATER 2 )
    list( GET _version_list 2 ${prefix}_VERSION_PATCH )
  endif()
  if( ${_len} GREATER 3 )
    list( GET _version_list 3 ${prefix}_VERSION_TWEAK )
  endif()

  ## Export variables to parent scope
  list( APPEND export_variables_parent_scope
    ${prefix}_VERSION_STR
    ${prefix}_VERSION
    ${prefix}_VERSION_MAJOR
    ${prefix}_VERSION_MINOR
    ${prefix}_VERSION_PATCH
    ${prefix}_VERSION_TWEAK
    ${prefix}_VERSION_SUFFIX
  )
  foreach( _var ${export_variables_parent_scope} )
    if( DEFINED ${_var} )
      set( ${_var} ${${_var}} PARENT_SCOPE )
    endif()
  endforeach()

endfunction()


##############################################################################
#.rst:
#
# ecbuild_parse_version_file
# ==========================
#
# Parse version string of the form "<major>[.<minor>[.<patch>[.<tweak>]]][<suffix>]"
# contained in a file ::
#
#   ecbuild_parse_version_file( <file> [ PREFIX <prefix> ] )
#
# Options
# -------
#
# PREFIX : optional
#   string to be prefixed to all defined variables. If not given,
#   the value "_" will be used.
#
# Notes
# -----
#
# Following variables if possible:
#
#      <prefix>_VERSION_STR     = <major>[.<minor>[.<patch>[.<tweak>]]][<suffix>]
#      <prefix>_VERSION         = <major>[.<minor>[.<patch>[.<tweak>]]]
#      <prefix>_VERSION_MAJOR   = <major>
#      <prefix>_VERSION_MINOR   = <minor>
#      <prefix>_VERSION_PATCH   = <patch>
#      <prefix>_VERSION_TWEAK   = <tweak>
#      <prefix>_VERSION_SUFFIX  = <suffix>
#

function( ecbuild_parse_version_file file )
  set( options )
  set( single_value_args PREFIX )
  set( multi_value_args )

  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}" ${ARGN} )

  if( NOT _PAR_PREFIX )
    set( prefix "_" )
  else()
    set( prefix ${_PAR_PREFIX} )
  endif()

  if( NOT ( EXISTS ${file} OR EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${file} ) )
    message( FATAL_ERROR "ecbuild_parse_version_file: Cannot parse file ${file} as it does not exist")
  endif()

  ## Read _version_str from file
  file( STRINGS ${file} _version_str )

  ## Parse _version_str
  ecbuild_parse_version( ${_version_str} PREFIX ${prefix} )

  ## Export variables to parent scope
  list( APPEND export_variables_parent_scope
    ${prefix}_VERSION_STR
    ${prefix}_VERSION
    ${prefix}_VERSION_MAJOR
    ${prefix}_VERSION_MINOR
    ${prefix}_VERSION_PATCH
    ${prefix}_VERSION_TWEAK
    ${prefix}_VERSION_SUFFIX
  )
  foreach( _var ${export_variables_parent_scope} )
    if( ${_var} )
      set( ${_var} ${${_var}} PARENT_SCOPE )
    endif()
  endforeach()

endfunction()
