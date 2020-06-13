# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

##############################################################################
# function for concatenating list into a string
#
# examples:
#
#   set( _paths "foo" "bar" )
#   join( _paths "/" _mypath )
#
#   message( "${_mpath}" ) #  produces "foo/bar"

function( JOIN _listname _glue _output )

	set( _ret "" )

	foreach( _v ${${_listname}} )
		if( _ret )
			set(_ret "${_ret}${_glue}${_v}") # append
		else()
			set(_ret "${_v}") # init
		endif()
	endforeach()

	set(${_output} "${_ret}" PARENT_SCOPE)

endfunction()

##############################################################################
# function for inserting a key / value into a map
#
# examples:
#
#   map_insert( "mymap" "foo" "bar" )
#

function( MAP_INSERT _map _key _value )
	set( "_${_map}_${_key}" "${_value}" PARENT_SCOPE )
endfunction(MAP_INSERT)

##############################################################################
# function for inserting a key / value into a map
#
# examples:
#
#   map_get( "mymap" "foo" VAR )
#

function( MAP_GET _map _key _var )
	set( ${_var} "${_${_map}_${_key}}" PARENT_SCOPE )
endfunction(MAP_GET)

