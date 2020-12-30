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
# ecbuild_append_to_rpath
# =======================
#
# Append paths to the rpath. ::
#
#   ecbuild_append_to_rpath( RPATH_DIRS )
#
# ``RPATH_DIRS`` is a list of directories to append to ``CMAKE_INSTALL_RPATH``.
#
# * If a directory is absolute, simply append it.
# * If a directory is relative, build a platform-dependent relative path
#   (using ``@loader_path`` on Mac OSX, ``$ORIGIN`` on Linux and Solaris)
#   or fall back to making it absolute by prepending the install prefix.
#
##############################################################################

function( ecbuild_path_append var path )
  list( FIND ${var} ${path} _found )
  if( _found EQUAL "-1" )
    list( APPEND ${var} ${path})
  endif()
  set( ${var} "${${var}}" PARENT_SCOPE ) #
endfunction()

function( _make_relative_rpath_entry entry var )
    if( EC_OS_NAME STREQUAL "macosx" )
        set( ${var} "@loader_path/${entry}" PARENT_SCOPE )

    elseif( EC_OS_NAME STREQUAL "freebsd" )
        set( ${var} "$ORIGIN/${entry}" PARENT_SCOPE )

    elseif( EC_OS_NAME STREQUAL "linux" )
        set( ${var} "$ORIGIN/${entry}" PARENT_SCOPE )

    elseif( EC_OS_NAME STREQUAL "solaris" )
        set( ${var} "$ORIGIN/${entry}" PARENT_SCOPE )

    elseif( EC_OS_NAME STREQUAL "aix" ) # always relative to executable path
        set( ${var} "${entry}" PARENT_SCOPE )

    else()
        set( ${var} "${CMAKE_INSTALL_PREFIX}/${entry}" PARENT_SCOPE )

    endif()
endfunction()

macro( ecbuild_append_to_rpath RPATH_DIRS )

   if( NOT ${ARGC} EQUAL 1 )
     ecbuild_error( "ecbuild_append_to_rpath takes 1 argument")
   endif()

   foreach( RPATH_DIR ${RPATH_DIRS} )

        if( NOT ${RPATH_DIR} STREQUAL "" )

            file( TO_CMAKE_PATH ${RPATH_DIR} RPATH_DIR ) # sanitize the path

            if( IS_ABSOLUTE ${RPATH_DIR} )

                ecbuild_path_append( CMAKE_INSTALL_RPATH "${RPATH_DIR}" )

            else()

                _make_relative_rpath_entry( "${RPATH_DIR}" rpath_dir_rel )
                ecbuild_path_append( CMAKE_INSTALL_RPATH ${rpath_dir_rel} )

            endif()

     endif()

   endforeach()

endmacro( ecbuild_append_to_rpath )

macro( ecbuild_target_rpath target mode )

    if( "${mode}" STREQUAL REPLACE )
        set( _target_rpath "" )
    elseif( "${mode}" STREQUAL APPEND )
        get_target_property( _target_rpath ${target} INSTALL_RPATH )
    else()
        ecbuild_critical( "ecbuild_target_rpath arg 2 should be either APPEND \
            or REPLACE" )
    endif()

    foreach( rpath_dir ${ARGN} )
        if( NOT ${rpath_dir} STREQUAL "" )
            file( TO_CMAKE_PATH ${rpath_dir} rpath_dir ) # sanitise the path

            if( IS_ABSOLUTE ${rpath_dir} )
                ecbuild_path_append( _target_rpath "${rpath_dir}" )

            else()
                _make_relative_rpath_entry( "${rpath_dir}" rpath_dir_rel )
                ecbuild_path_append( _target_rpath ${rpath_dir_rel} )

            endif()
        endif()
    endforeach()

    set_target_properties( ${target} PROPERTIES INSTALL_RPATH "${_target_rpath}" )

endmacro()
