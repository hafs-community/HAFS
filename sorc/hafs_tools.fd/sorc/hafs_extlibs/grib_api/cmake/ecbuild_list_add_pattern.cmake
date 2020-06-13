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
# ecbuild_list_add_pattern
# ========================
#
# Exclude items from a list that match a list of patterns. ::
#
#   ecbuild_list_add_pattern( LIST <input_list>
#                             GLOB <pattern1> [ <pattern2> ... ]
#                             [ SOURCE_DIR <source_dir> ]
#                             [ QUIET ] )
#
# Options
# -------
#
# LIST : required
#   list variable to be appended to
#
# GLOB : required
#   Regex pattern of exclusion
#
# SOURCE_DIR : optional
#   Directory from where to start search
#
# QUIET  : optional
#   Don't warn if patterns don't match
#
##############################################################################

function( ecbuild_list_add_pattern )

  set( options QUIET )
  set( single_value_args LIST SOURCE_DIR )
  set( multi_value_args  GLOB )

  cmake_parse_arguments( _p "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if(_p_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ecbuild_list_add_pattern(): \"${_p_UNPARSED_ARGUMENTS}\"")
  endif()

  if( NOT _p_LIST  )
    ecbuild_critical("The call to ecbuild_list_add_pattern() doesn't specify the LIST.")
  endif()

  if( NOT _p_GLOB )
    ecbuild_critical("The call to ecbuild_list_add_pattern() doesn't specify the GLOB.")
  endif()

  #####

  set( input_list ${${_p_LIST}} )
  unset( matched_files )

  foreach( pattern ${_p_GLOB} )

    if( IS_ABSOLUTE ${pattern} )
      ecbuild_debug( "ecbuild_list_add_pattern: Adding ${pattern}" )
      file( GLOB_RECURSE matched_files ${pattern} )
    else()

      if(_p_SOURCE_DIR)
        if( IS_ABSOLUTE ${_p_SOURCE_DIR} )
          ecbuild_debug( "ecbuild_list_add_pattern: Adding ${_p_SOURCE_DIR}/${pattern}" )
          file( GLOB_RECURSE matched_files ${_p_SOURCE_DIR}/${pattern} )
        else()
          ecbuild_debug( "ecbuild_list_add_pattern: Adding ${_p_SOURCE_DIR}/${pattern}" )
          file( GLOB_RECURSE matched_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${_p_SOURCE_DIR}/${pattern} )
        endif()
      else()
        ecbuild_debug( "ecbuild_list_add_pattern: Adding ${pattern} ")
        file( GLOB_RECURSE matched_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${pattern} )
      endif()

    endif()

    if(matched_files)
      ecbuild_debug( "ecbuild_list_add_pattern: Found ${matched_files}" )
      list( APPEND input_list ${matched_files} )
      list( REMOVE_DUPLICATES input_list )
      set( ${_p_LIST} ${input_list} PARENT_SCOPE )
    else()
      if(NOT _p_QUIET)
        ecbuild_warn( "ecbuild_list_add_pattern: no matches found for patterns ${pattern}" )
      else()
        ecbuild_debug( "ecbuild_list_add_pattern:no matches found for patterns ${pattern}" )
      endif()
    endif()

  endforeach()


endfunction(ecbuild_list_add_pattern)
