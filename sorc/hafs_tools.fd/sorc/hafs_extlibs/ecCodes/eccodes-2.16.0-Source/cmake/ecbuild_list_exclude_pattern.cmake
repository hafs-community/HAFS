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
# ecbuild_list_exclude_pattern
# ============================
#
# Exclude items from a list that match a list of patterns. ::
#
#   ecbuild_list_exclude_pattern( LIST <input_list>
#                                 REGEX <regex1> [ <regex2> ... ]
#                                 [ QUIET ] )
#
# Options
# -------
#
# LIST : required
#   list variable to be cleaned
#
# REGEX : required
#   Regex pattern of exclusions
#
# QUIET  : optional
#   Don't warn if patterns don't match
#
##############################################################################

function( ecbuild_list_exclude_pattern )

  set( options QUIET )
  set( single_value_args LIST )
  set( multi_value_args  REGEX )

  cmake_parse_arguments( _p "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if(_p_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ecbuild_list_exclude_pattern(): \"${_p_UNPARSED_ARGUMENTS}\"")
  endif()

  if( NOT _p_LIST  )
    ecbuild_critical("The call to ecbuild_list_exclude_pattern() doesn't specify the LIST.")
  endif()

  if( NOT _p_REGEX )
    ecbuild_critical("The call to ecbuild_list_exclude_pattern() doesn't specify the REGEX.")
  endif()

  #####

  set( result "" )
  set( matches_found 0 )

  # ecbuild_debug_var(_p_REGEX)

  foreach( item ${${_p_LIST}} )

    set( _keep 1 )

    foreach( pattern ${_p_REGEX} )
        if( ${item} MATCHES ${pattern} )
            set( _keep 0 )
            set( matches_found 1 )
        endif()
    endforeach()
    if( _keep )
        list( APPEND result ${item} )
#    else()
#      ecbuild_warn( "removing ${item}" )
    endif()

  endforeach()

  if( matches_found )
      set( ${_p_LIST} ${result} PARENT_SCOPE )
  else()
    if( NOT _p_QUIET )
        ecbuild_warn( "ecbuild_list_exclude_pattern: no matches found for patterns ${_p_REGEX} in ${_p_LIST}" )
    endif()
  endif()

endfunction(ecbuild_list_exclude_pattern)
