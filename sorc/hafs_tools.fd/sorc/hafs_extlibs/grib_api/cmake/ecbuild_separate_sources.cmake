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
# ecbuild_separate_sources
# ========================
#
# Separate a given list of sources according to language. ::
#
#   ecbuild_separate_sources( TARGET <name>
#                             SOURCES <source1> [ <source2> ... ] )
#
# Options
# -------
#
# TARGET : required
#   base name for the CMake output variables to set
#
# SOURCES : required
#   list of source files to separate
#
# Output variables
# ----------------
#
# If any file of the following group of extensions is present in the list of
# sources, the corresponding CMake variable is set:
#
# :<target>_h_srcs:       source files with extension .h, .hxx, .hh, .hpp, .H
# :<target>_c_srcs:       source files with extension .c
# :<target>_cxx_srcs:     source files with extension .cc, .cxx, .cpp, .C
# :<target>_fortran_srcs: source files with extension .f, .F, .for, f77, .f90,
#                                                     .f95, .F77, .F90, .F95
# :<target>_cuda_srcs:    source files with extension .cu
#
##############################################################################

function( ecbuild_separate_sources )

	set( options )
	set( single_value_args TARGET  )
	set( multi_value_args  SOURCES )

	cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

	if(_PAR_UNPARSED_ARGUMENTS)
	  ecbuild_critical("Unknown keywords given to ecbuild_separate_sources(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
	endif()

	if( NOT _PAR_TARGET  )
	  ecbuild_critical("The call to ecbuild_separate_sources() doesn't specify the TARGET.")
	endif()

	if( NOT _PAR_SOURCES )
	  ecbuild_critical("The call to ecbuild_separate_sources() doesn't specify the SOURCES.")
	endif()

	foreach( src ${_PAR_SOURCES} )
		if(${src} MATCHES "(\\.h$|\\.hxx$|\\.hh$|\\.hpp$|\\.H$)")
			list( APPEND ${_PAR_TARGET}_h_srcs ${src} )
		endif()
	endforeach()

	foreach( src ${_PAR_SOURCES} )
		if(${src} MATCHES "(\\.c$)")
			list( APPEND ${_PAR_TARGET}_c_srcs ${src} )
		endif()
	endforeach()

	foreach( src ${_PAR_SOURCES} )
		if(${src} MATCHES "(\\.cc$|\\.cxx$|\\.cpp$|\\.C$)")
			list( APPEND ${_PAR_TARGET}_cxx_srcs ${src} )
		endif()
	endforeach()

	foreach( src ${_PAR_SOURCES} )
		if(${src} MATCHES "(\\.f$|\\.F$|\\.for$|\\.f77$|\\.f90$|\\.f95$|\\.f03$|\\.f08$|\\.F77$|\\.F90$|\\.F95$|\\.F03$|\\.F08$)")
			list( APPEND ${_PAR_TARGET}_fortran_srcs ${src} )
		endif()
	endforeach()

    foreach( src ${_PAR_SOURCES} )
        if(${src} MATCHES "(\\.cu$)")
            list( APPEND ${_PAR_TARGET}_cuda_srcs ${src} )
        endif()
    endforeach()

    set_source_files_properties( ${${_PAR_TARGET}_fortran_srcs} PROPERTIES LANGUAGE Fortran )

    set( ${_PAR_TARGET}_h_srcs       "${${_PAR_TARGET}_h_srcs}"       PARENT_SCOPE )
    set( ${_PAR_TARGET}_c_srcs       "${${_PAR_TARGET}_c_srcs}"       PARENT_SCOPE )
    set( ${_PAR_TARGET}_cxx_srcs     "${${_PAR_TARGET}_cxx_srcs}"     PARENT_SCOPE )
    set( ${_PAR_TARGET}_fortran_srcs "${${_PAR_TARGET}_fortran_srcs}" PARENT_SCOPE )
    set( ${_PAR_TARGET}_cuda_srcs    "${${_PAR_TARGET}_cuda_srcs}"    PARENT_SCOPE )


endfunction( ecbuild_separate_sources )
