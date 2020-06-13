# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# - Try to find GRIB_API
# Once done this will define
#  GRIB_API_FOUND - System has GRIB_API
#  GRIB_API_INCLUDE_DIRS - The GRIB_API include directories
#  GRIB_API_LIBRARIES - The libraries needed to use GRIB_API
#  GRIB_API_DEFINITIONS - Compiler switches required for using GRIB_API

option( NO_GRIB_API_BINARIES "skip trying to find grib_api installed binaries" OFF )
option( GRIB_API_PNG "use png with grib_api" ON )
option( GRIB_API_JPG "use jpg with grib_api" ON )

if( NOT grib_api_FOUND AND NOT NO_GRIB_API_BINARIES )

    if( GRIB_API_JPG ) # jpeg support

        find_package( JPEG     QUIET ) # grib_api might be a static .a library in which

        if( NOT "$ENV{JASPER_PATH}" STREQUAL "" )
            list( APPEND CMAKE_PREFIX_PATH "$ENV{JASPER_PATH}" )
        endif()
        find_package( Jasper   QUIET ) # case we don't know if which jpeg library was used

        find_package( OpenJPEG QUIET ) # so we try to find all jpeg libs and link to them

        if(JPEG_FOUND)
            list( APPEND _grib_api_jpg_incs ${JPEG_INCLUDE_DIR} )
            list( APPEND _grib_api_jpg_libs ${JPEG_LIBRARIES} )
        endif()
        if(JASPER_FOUND)
            list( APPEND _grib_api_jpg_incs ${JASPER_INCLUDE_DIR} )
            list( APPEND _grib_api_jpg_libs ${JASPER_LIBRARIES} )
        endif()
        if(OPENJPEG_FOUND)
            list( APPEND _grib_api_jpg_incs ${OPENJPEG_INCLUDE_DIR} )
            list( APPEND _grib_api_jpg_libs ${OPENJPEG_LIBRARIES} )
        endif()

    endif()

    if( GRIB_API_PNG ) # png support

        find_package(PNG)

        if( DEFINED PNG_PNG_INCLUDE_DIR AND NOT DEFINED PNG_INCLUDE_DIRS )
          set( PNG_INCLUDE_DIRS ${PNG_PNG_INCLUDE_DIR}  CACHE INTERNAL "PNG include dirs" )
        endif()
        if( DEFINED PNG_LIBRARY AND NOT DEFINED PNG_LIBRARIES )
          set( PNG_LIBRARIES ${PNG_LIBRARY} CACHE INTERNAL "PNG libraries" )
        endif()

        if(PNG_FOUND)
            list( APPEND _grib_api_png_defs ${PNG_DEFINITIONS} )
            list( APPEND _grib_api_png_incs ${PNG_INCLUDE_DIRS} )
            list( APPEND _grib_api_png_libs ${PNG_LIBRARIES} )
        endif()

    endif()

	# The grib_api on macos that comes with 'port' is linked against ghostscript
	if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
		find_library(GS_LIBRARIES NAMES gs)
		if( GS_LIBRARIES )
			list( APPEND GRIB_API_LIBRARIES ${GS_LIBRARIES} )
		endif()
	endif()

    # find external grib_api

    if( NOT DEFINED GRIB_API_PATH AND NOT "$ENV{GRIB_API_PATH}" STREQUAL "" )
        list( APPEND GRIB_API_PATH "$ENV{GRIB_API_PATH}" )
    endif()

    if( DEFINED GRIB_API_PATH )
        find_path(GRIB_API_INCLUDE_DIR NAMES grib_api.h PATHS ${GRIB_API_PATH} ${GRIB_API_PATH}/include PATH_SUFFIXES grib_api  NO_DEFAULT_PATH)
        find_library(GRIB_API_LIBRARY  NAMES grib_api   PATHS ${GRIB_API_PATH} ${GRIB_API_PATH}/lib     PATH_SUFFIXES grib_api  NO_DEFAULT_PATH)
        find_library(GRIB_API_LIB_F90  NAMES grib_api_f90 PATHS ${GRIB_API_PATH} ${GRIB_API_PATH}/lib     PATH_SUFFIXES grib_api  NO_DEFAULT_PATH)
        find_library(GRIB_API_LIB_F77  NAMES grib_api_f77 PATHS ${GRIB_API_PATH} ${GRIB_API_PATH}/lib     PATH_SUFFIXES grib_api  NO_DEFAULT_PATH)
        find_program(GRIB_API_INFO     NAMES grib_info  PATHS ${GRIB_API_PATH} ${GRIB_API_PATH}/bin     PATH_SUFFIXES grib_api  NO_DEFAULT_PATH)
    endif()

    find_path(GRIB_API_INCLUDE_DIR NAMES grib_api.h PATHS PATH_SUFFIXES grib_api )
    find_library( GRIB_API_LIBRARY NAMES grib_api   PATHS PATH_SUFFIXES grib_api )
    find_library( GRIB_API_LIB_F90 NAMES grib_api_f90 PATHS PATH_SUFFIXES grib_api )
    find_library( GRIB_API_LIB_F77 NAMES grib_api_f77 PATHS PATH_SUFFIXES grib_api )
    find_program(GRIB_API_INFO     NAMES grib_info  PATHS PATH_SUFFIXES grib_api )

    list( APPEND GRIB_API_LIBRARIES    ${GRIB_API_LIBRARY} ${GRIB_API_LIB_F90} ${GRIB_API_LIB_F77} )
    set( GRIB_API_INCLUDE_DIRS ${GRIB_API_INCLUDE_DIR} )

    if( GRIB_API_INFO )

        execute_process( COMMAND ${GRIB_API_INFO} -v  OUTPUT_VARIABLE _grib_info_out ERROR_VARIABLE _grib_info_err OUTPUT_STRIP_TRAILING_WHITESPACE )

        # ecbuild_debug_var( _grib_info_out )

        string( REPLACE "." " " _version_list ${_grib_info_out} ) # dots to spaces
        separate_arguments( _version_list )

        list( GET _version_list 0 GRIB_API_MAJOR_VERSION )
        list( GET _version_list 1 GRIB_API_MINOR_VERSION )
        list( GET _version_list 2 GRIB_API_PATCH_VERSION )

        set( GRIB_API_VERSION     "${GRIB_API_MAJOR_VERSION}.${GRIB_API_MINOR_VERSION}.${GRIB_API_PATCH_VERSION}" )
        set( GRIB_API_VERSION_STR "${_grib_info_out}" )

        set( grib_api_VERSION     "${GRIB_API_VERSION}" )
        set( grib_api_VERSION_STR "${GRIB_API_VERSION_STR}" )

    endif()

    include(FindPackageHandleStandardArgs)

    # handle the QUIETLY and REQUIRED arguments and set GRIB_API_FOUND to TRUE
    find_package_handle_standard_args( grib_api DEFAULT_MSG
                                       GRIB_API_LIBRARY GRIB_API_INCLUDE_DIR GRIB_API_INFO )

    mark_as_advanced( GRIB_API_INCLUDE_DIR GRIB_API_LIBRARY GRIB_API_INFO )

    list( APPEND GRIB_API_DEFINITIONS  ${_grib_api_jpg_defs} ${_grib_api_png_defs} )
    list( APPEND GRIB_API_INCLUDE_DIRS ${_grib_api_jpg_incs} ${_grib_api_png_incs} )
	list( APPEND GRIB_API_LIBRARIES    ${_grib_api_jpg_libs} ${_grib_api_png_libs} )

    set( grib_api_FOUND ${GRIB_API_FOUND} )

endif()
