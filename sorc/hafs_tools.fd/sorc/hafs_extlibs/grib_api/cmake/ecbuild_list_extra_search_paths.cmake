# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

############################################################################################
#
# macro for adding search paths for a package to a given CMake variable
#
# usage: ecbuild_list_extra_search_paths( netcdf4 VARIABLE )

function( ecbuild_list_extra_search_paths pkg var )

  ecbuild_deprecate( " ecbuild_list_extra_search_paths should no longer be"
                     " used and is going to be removed in a future version of ecBuild." )

	# ecbuild_debug_var( pkg )
	# ecbuild_debug_var( var )

	string( TOUPPER ${pkg} _PKG )

	# PKG_PATH (upper case)

	if( DEFINED ${_PKG}_PATH AND EXISTS ${${_PKG}_PATH} )
    ecbuild_debug("ecbuild_list_extra_search_paths(${pkg}): appending ${_PKG}_PATH = ${${_PKG}_PATH} to ${var}")
		list( APPEND ${var} ${${_PKG}_PATH} )
	endif()

	# ENV PKG_PATH (upper case)

	if( DEFINED ENV{${_PKG}_PATH} AND EXISTS $ENV{${_PKG}_PATH}  )
    ecbuild_debug("ecbuild_list_extra_search_paths(${pkg}): appending \$${_PKG}_PATH = $ENV{${_PKG}_PATH} to ${var}")
		list( APPEND ${var} $ENV{${_PKG}_PATH} )
	endif()

	# pkg_PATH (lower case)

	if( DEFINED ${pkg}_PATH AND EXISTS ${${pkg}_PATH} )
    ecbuild_debug("ecbuild_list_extra_search_paths(${pkg}): appending ${pkg}_PATH = ${${pkg}_PATH} to ${var}")
		list( APPEND ${var} ${${pkg}_PATH} )
	endif()

	# ENV pkg_PATH (lower case)

  if( DEFINED ${pkg}_PATH AND EXISTS $ENV{${pkg}_PATH} )
    ecbuild_debug("ecbuild_list_extra_search_paths(${pkg}): appending \$${pkg}_PATH = $ENV{${pkg}_PATH} to ${var}")
    list( APPEND ${var} $ENV{${pkg}_PATH} )
	endif()

	# ENV PKG_DIR (upper case)

	if( DEFINED ENV{${_PKG}_DIR} AND EXISTS $ENV{${_PKG}_DIR}  )
    ecbuild_debug("ecbuild_list_extra_search_paths(${pkg}): appending \$${_PKG}_DIR = $ENV{${_PKG}_DIR} to ${var}")
		list( APPEND ${var} $ENV{${_PKG}_DIR} )
	endif()

	# ENV pkg_DIR (lower case)

	if( DEFINED ENV{${pkg}_DIR} AND EXISTS $ENV{${pkg}_DIR} )
    ecbuild_debug("ecbuild_list_extra_search_paths(${pkg}): appending \$${pkg}_DIR = $ENV{${pkg}_DIR} to ${var}")
		list( APPEND ${var} $ENV{${pkg}_DIR} )
	endif()

	# sanitize the list

	if( ${var} )
		list( REMOVE_DUPLICATES ${var} )
	endif()

	# define it out of the function

  ecbuild_debug("ecbuild_list_extra_search_paths(${pkg}): setting ${var} to ${${var}}")
	set( ${var} ${${var}} PARENT_SCOPE )

# ecbuild_debug_var( ${var} )

endfunction()

