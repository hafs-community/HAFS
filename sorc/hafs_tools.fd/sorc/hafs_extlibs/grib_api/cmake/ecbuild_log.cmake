# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

##############################################################################
#.rst:
#
# Logging
# =======
#
# ecBuild provides functions for logging based on a log level set by the user,
# similar to the Python logging module:
#
# :ecbuild_debug:     logs a ``STATUS`` message if log level <= ``DEBUG``
# :ecbuild_info:      logs a ``STATUS`` message if log level <= ``INFO``
# :ecbuild_warn:      logs a ``WARNING`` message if log level <= ``WARN``
# :ecbuild_error:     logs a ``SEND_ERROR`` message if log level <= ``ERROR``
# :ecbuild_critical:  logs a ``FATAL_ERROR`` message if log level <= ``CRITICAL``
# :ecbuild_deprecate: logs a ``DEPRECATION`` message as a warning
#                     enable CMAKE_ERROR_DEPRECATED to raise an error instead
#                     disable CMAKE_WARN_DEPRECATED to hide deprecations
#
# Furthermore there are auxilliary functions for outputting CMake variables,
# CMake lists and environment variables if the log level is ``DEBUG``:
#
# :ecbuild_debug_var:      logs given CMake variables if log level <= ``DEBUG``
# :ecbuild_debug_list:     logs given CMake lists if log level <= ``DEBUG``
# :ecbuild_debug_env_var:  logs given environment variables if log level <= ``DEBUG``
# :ecbuild_debug_property: logs given global CMake property if log level <= ``DEBUG``
#
# To log a message to the ecBuild log file only at a given log level, use ::
#
#   ecbuild_log( <level> <msg> )
#
# Input variables
# ---------------
#
# CMake variables controlling logging behaviour:
#
# ECBUILD_LOG_FILE : path
#   set the log file, defaults to ``${CMAKE_BINARY_DIR}/ecbuild.log``
#
#   All ecBuild log functions write their messages to this log file with a time
#   stamp. Messages emitted by CMake directly cannot be logged to file.
#
# ECBUILD_LOG_LEVEL : string, one of DEBUG, INFO, WARN, ERROR, CRITICAL, OFF
#   desired log level, defaults to ``INFO``, ``OFF`` to disable logging
#
# ECBUILD_NO_COLOUR : bool
#   if set, does not colour log output (by default log output is coloured)
#
# Usage
# -----
#
# The functions ``ecbuild_debug`` and ``ecbuild_info`` can be used to output
# messages which are not printed by default. Many ecBuild macros use this
# facility to log debugging hints. When debugging a CMake run, users can use
# ``-DECBUILD_LOG_LEVEL=DEBUG`` to get detailed diagnostics.
#
##############################################################################

# Define colour escape sequences (not available on Windows)
if(NOT (WIN32 OR ECBUILD_NO_COLOUR))
  string(ASCII 27 Esc)
  set(ColourReset "${Esc}[m")
  set(ColourBold  "${Esc}[1m")
  set(Red         "${Esc}[31m")
  set(Green       "${Esc}[32m")
  set(Yellow      "${Esc}[33m")
  set(Blue        "${Esc}[34m")
  set(Magenta     "${Esc}[35m")
  set(Cyan        "${Esc}[36m")
  set(White       "${Esc}[37m")
  set(BoldRed     "${Esc}[1;31m")
  set(BoldGreen   "${Esc}[1;32m")
  set(BoldYellow  "${Esc}[1;33m")
  set(BoldBlue    "${Esc}[1;34m")
  set(BoldMagenta "${Esc}[1;35m")
  set(BoldCyan    "${Esc}[1;36m")
  set(BoldWhite   "${Esc}[1;37m")
endif()

set(ECBUILD_DEBUG    10)
set(ECBUILD_INFO     20)
set(ECBUILD_WARN     30)
set(ECBUILD_ERROR    40)
set(ECBUILD_CRITICAL 50)

if( NOT DEFINED ECBUILD_LOG_LEVEL )
  set(ECBUILD_LOG_LEVEL ${ECBUILD_INFO})
elseif( NOT ECBUILD_LOG_LEVEL )
  set(ECBUILD_LOG_LEVEL 60)
elseif( ECBUILD_LOG_LEVEL STREQUAL "DEBUG" )
  set(ECBUILD_LOG_LEVEL ${ECBUILD_DEBUG})
elseif( ECBUILD_LOG_LEVEL STREQUAL "INFO" )
  set(ECBUILD_LOG_LEVEL ${ECBUILD_INFO})
elseif( ECBUILD_LOG_LEVEL STREQUAL "WARN" )
  set(ECBUILD_LOG_LEVEL ${ECBUILD_WARN})
elseif( ECBUILD_LOG_LEVEL STREQUAL "ERROR" )
  set(ECBUILD_LOG_LEVEL ${ECBUILD_ERROR})
elseif( ECBUILD_LOG_LEVEL STREQUAL "CRITICAL" )
  set(ECBUILD_LOG_LEVEL ${ECBUILD_CRITICAL})
else()
  message(WARNING "Unknown log level ${ECBUILD_LOG_LEVEL} (valid are DEBUG, INFO, WARN, ERROR, CRITICAL) - using WARN")
  set(ECBUILD_LOG_LEVEL ${ECBUILD_WARN})
endif()

if( NOT DEFINED ECBUILD_LOG_FILE )
  set( ECBUILD_LOG_FILE ${CMAKE_BINARY_DIR}/ecbuild.log )
endif()
if( NOT DEFINED CMAKE_ERROR_DEPRECATED AND NOT DEFINED CMAKE_WARN_DEPRECATED )
  set( CMAKE_WARN_DEPRECATED ON )
endif()

##############################################################################

function( ecbuild_log LEVEL )
  string( REPLACE ";" " " MSG "${ARGN}" )
  string( TIMESTAMP _time )
  file( APPEND ${ECBUILD_LOG_FILE} "${_time} - ${PROJECT_NAME} - ${LEVEL} - ${MSG}\n" )
endfunction( ecbuild_log )

##############################################################################

function( ecbuild_debug )
  string( REPLACE ";" " " MSG "${ARGV}" )
  ecbuild_log(DEBUG "${MSG}")
  if( ECBUILD_LOG_LEVEL LESS 11)
    message(STATUS "${Blue}DEBUG - ${MSG}${ColourReset}")
  endif()
endfunction( ecbuild_debug )

##############################################################################

function( ecbuild_info )
  string( REPLACE ";" " " MSG "${ARGV}" )
  ecbuild_log(INFO "${MSG}")
  if( ECBUILD_LOG_LEVEL LESS 21)
    message(STATUS "${MSG}")
  endif()
endfunction( ecbuild_info )

##############################################################################

function( ecbuild_warn )
  string( REPLACE ";" " " MSG "${ARGV}" )
  ecbuild_log(WARNING "${MSG}")
  if( ECBUILD_LOG_LEVEL LESS 31)
    message(WARNING "${Yellow}WARN - ${MSG}${ColourReset}")
  endif()
endfunction( ecbuild_warn )

##############################################################################

function( ecbuild_error )
  string( REPLACE ";" " " MSG "${ARGV}" )
  ecbuild_log(ERROR "${MSG}")
  if( ECBUILD_LOG_LEVEL LESS 41)
    message(SEND_ERROR "${BoldRed}ERROR - ${MSG}${ColourReset}")
  endif()
endfunction( ecbuild_error )

##############################################################################

function( ecbuild_deprecate )
  string(REPLACE ";" " " MSG ${ARGV})
  ecbuild_log(DEPRECATION "${MSG}")
  # DEPRECATION message type was only introduced in CMake 3.0, provide
  # consistent behaviour for CMake < 3.0
  if( CMAKE_VERSION VERSION_LESS 3.0 )
    if( CMAKE_ERROR_DEPRECATED )
      message(FATAL_ERROR "${BoldRed}DEPRECATION - ${MSG}${ColourReset}")
    elseif( CMAKE_WARN_DEPRECATED )
      message(WARNING "${Yellow}DEPRECATION - ${MSG}${ColourReset}")
    endif()
  else()
    message(DEPRECATION "${BoldRed}${MSG}${ColourReset}")
  endif()
endfunction( ecbuild_deprecate )

##############################################################################

function( ecbuild_critical )
  string(REPLACE ";" " " MSG ${ARGV})
  ecbuild_log(FATAL_ERROR "${MSG}")
  if( ECBUILD_LOG_LEVEL LESS 51)
    message(FATAL_ERROR "${BoldMagenta}CRITICAL - ${MSG}${ColourReset}")
  endif()
endfunction( ecbuild_critical )

##############################################################################
# function for debugging CMake variables

function( ecbuild_debug_var )
  foreach( VAR ${ARGV} )
    ecbuild_log(DEBUG "${VAR} : ${${VAR}}")
    if( ECBUILD_LOG_LEVEL LESS 11)
      message(STATUS "${Blue}DEBUG - ${VAR} : ${${VAR}}${ColourReset}")
    endif()
  endforeach()
endfunction()

##############################################################################
# function for debugging CMake lists

function( ecbuild_debug_list )
  foreach( VAR ${ARGV} )
    ecbuild_log(DEBUG "${VAR} : ${${VAR}}")
    foreach( _elem ${${VAR}} )
      ecbuild_log( DEBUG "  ${_elem}" )
    endforeach()
    if( ECBUILD_LOG_LEVEL LESS 11)
      message( STATUS "${Blue}DEBUG - ${VAR}" )
      foreach( _elem ${${VAR}} )
        message( STATUS "  ${_elem}" )
      endforeach()
      message(STATUS "${ColourReset}")
    endif()
  endforeach()
endfunction()

##############################################################################
# function for debugging environment variables

function( ecbuild_debug_env_var )
  foreach( VAR ${ARGV} )
    ecbuild_log(DEBUG "ENV ${VAR} : $ENV{${VAR}}")
    if( ECBUILD_LOG_LEVEL LESS 11)
      message(STATUS "${Blue}DEBUG - ENV ${VAR} [$ENV{${VAR}}]${ColourReset}")
    endif()
  endforeach()
endfunction()

##############################################################################
# function for debugging a CMake global property

function( ecbuild_debug_property )
  foreach( VAR ${ARGV} )
    get_property( __prop GLOBAL PROPERTY ${VAR} )
    ecbuild_log(DEBUG "PROPERTY ${VAR} : ${__prop}")
    if( ECBUILD_LOG_LEVEL LESS 11)
      message(STATUS "${Blue}DEBUG - PROPERTY ${VAR} [${__prop}]${ColourReset}")
    endif()
  endforeach()
endfunction()
