# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

##############################################################################
# macro for adding a test
##############################################################################

macro( ecbuild_get_resources )

    set( options                )
    set( single_value_args TO_DIR  )
    set( multi_value_args  LIST )

    cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

    if(_PAR_UNPARSED_ARGUMENTS)
        ecbuild_critical("Unknown keywords given to ecbuild_get_resources(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
    endif()

    if( NOT _PAR_LIST )
        ecbuild_critical( "Missing parameter LIST of resources in macro ecbuild_get_resources()" )
    endif()

    if( NOT _PAR_TO_DIR )
		set( _PAR_TO_DIR ${CMAKE_CURRENT_BINARY_DIR} )
	endif()

    list( LENGTH _PAR_LIST _rsize )
    math( EXPR _max "${_rsize}-1" )
    foreach( i RANGE 0 ${_max} 2 )

        math( EXPR in "${i}+1" )

        list( GET _PAR_LIST ${i}  r  )
        list( GET _PAR_LIST ${in} rh )

#        ecbuild_debug_var( r  )
#        ecbuild_debug_var( rh )

        get_filename_component( rf ${r} NAME )

        file( DOWNLOAD ${r} ${_PAR_TO_DIR}/${rf} EXPECTED_HASH SHA1=${rh} )

    endforeach()


endmacro()
