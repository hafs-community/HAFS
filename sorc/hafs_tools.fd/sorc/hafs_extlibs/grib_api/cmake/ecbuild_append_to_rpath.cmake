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

function( _path_append var path )
	if( "${${var}}" STREQUAL "" )
		set( ${var} "${path}" PARENT_SCOPE )
	else()
		list( FIND ${var} ${path} _found )
		if( _found EQUAL "-1" )
			set( ${var} "${${var}}:${path}" PARENT_SCOPE )
		endif()
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

				_path_append( CMAKE_INSTALL_RPATH "${RPATH_DIR}" )

			else()

				set( _done 0 )

				if( EC_OS_NAME STREQUAL "macosx" )

					if("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" VERSION_LESS 3.0) # cmake < 3.0
						set( CMAKE_INSTALL_NAME_DIR "@loader_path/${RPATH_DIR}" )
					endif()
					_path_append( CMAKE_INSTALL_RPATH "@loader_path/${RPATH_DIR}" )

					set( _done 1 )

				endif()

                if( EC_OS_NAME STREQUAL "freebsd" )
                    _path_append( CMAKE_INSTALL_RPATH "$ORIGIN/${RPATH_DIR}" )
                    set( _done 1 )
                endif()

                if( EC_OS_NAME STREQUAL "linux" )
					_path_append( CMAKE_INSTALL_RPATH "$ORIGIN/${RPATH_DIR}" )
					set( _done 1 )
				endif()

				if( EC_OS_NAME STREQUAL "solaris" )
					_path_append( CMAKE_INSTALL_RPATH "$ORIGIN/${RPATH_DIR}" )
					set( _done 1 )
				endif()

                if( EC_OS_NAME STREQUAL "aix" ) # always relative to exectuable path
                    _path_append( CMAKE_INSTALL_RPATH "${RPATH_DIR}" )
                    set( _done 1 )
                endif()

				# fallback

				if( NOT _done )
					_path_append( CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/${RPATH_DIR}" )
				endif()

			endif()

     endif()

   endforeach()

endmacro( ecbuild_append_to_rpath )
