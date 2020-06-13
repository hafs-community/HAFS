# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

###############################################################################
# FORTRAN support

# set( PGIFORTRAN_SEARCH_LIBS pgmp pgbind numa pgf90 pgf90_rpm1 pgf902 pgf90rtl  pgftnrtl nspgc pgc rt pgsse1 pgsse2 ) # init
# set( PGIFORTRAN_SEARCH_LIBS pgf90 pgf90_rpm1 pgf902 pgf90rtl pgftnrtl pghpf pgc pgf90 rt pgsse1 pgsse2 )             # mars client linux.2
# set( PGIFORTRAN_SEARCH_LIBS pgftnrtl nspgc pgc rt pgsse1 pgsse2 )                                                    # mars client linux.3

if( NOT DEFINED PGIFORTRAN_SEARCH_LIBS )
	set( PGIFORTRAN_SEARCH_LIBS pgmp pgbind numa pgf90 pgf90_rpm1 pgf902 pgf90rtl pgftnrtl pghpf nspgc pgc pgf90 pgf902 pghpf_rpm1 pghpf2 pgsse1 pgsse2 ) # better ?                                                    #
endif()

set( pgi_fortran_all_libs_found 1 )

foreach( pglib ${PGIFORTRAN_SEARCH_LIBS} )

	find_library( ${pglib}_lib  ${pglib} PATHS ${PGI_PATH} PATH_SUFFIXES lib libso NO_DEFAULT_PATH )

	find_library( ${pglib}_lib  ${pglib} HINTS /usr/local/apps/pgi/pgi-10.8/linux86-64/10.8 PATH PATH_SUFFIXES lib libso )

    if( ${pglib}_lib )
        list( APPEND PGIFORTRAN_LIBRARIES ${${pglib}_lib} )
#	else()
#		set( pgi_fortran_all_libs_found 0 )
    endif()

endforeach()

include(FindPackageHandleStandardArgs)

# Handle the QUIET and REQUIRED arguments and set PGIFORTRAN_FOUND to TRUE
# if all listed variables are TRUE
# Note: capitalisation of the package name must be the same as in the file name
find_package_handle_standard_args( PGIFortran DEFAULT_MSG pgi_fortran_all_libs_found PGIFORTRAN_LIBRARIES  )

if( PGIFORTRAN_FOUND )
	find_package( Realtime )
endif()

if( REALTIME_FOUND )
	set( PGIFORTRAN_LIBRARIES ${PGIFORTRAN_LIBRARIES} ${RT_LIB} )
endif()
