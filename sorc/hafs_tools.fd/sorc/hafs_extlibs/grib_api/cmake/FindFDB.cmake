# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# - Try to find FDB
# Once done this will define
#  FDB_FOUND - System has FDB
#  FDB_INCLUDE_DIRS - The FDB include directories
#  FDB_LIBRARIES - The libraries needed to use FDB


if( NOT FDB_FOUND )

	if( DEFINED FDB_PATH )
		find_library( FDB_LIBRARY NAMES fdb PATHS ${FDB_PATH} ${FDB_PATH}/lib NO_DEFAULT_PATH)
	endif()

	find_library( FDB_LIBRARY NAMES fdb )

	set( FDB_LIBRARIES  ${FDB_LIBRARY} )

	include(FindPackageHandleStandardArgs)

	# handle the QUIETLY and REQUIRED arguments and set FDB_FOUND to TRUE
	# if all listed variables are TRUE
	find_package_handle_standard_args(FDB  DEFAULT_MSG
									  FDB_LIBRARY )

	mark_as_advanced(FDB_LIBRARY)

endif()
