# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

############################################################################################

# ensure we use 64bit access to files even on 32bit OS's -- aka Large File Support
# by making off_t 64 bit and stat behave as stat64

macro(ecbuild_add_large_file_support)

  check_type_size( off_t EC_SIZEOF_OFF_T )

  if( EC_SIZEOF_OFF_T LESS "8" )

    if( ${CMAKE_SYSTEM_NAME} MATCHES "Linux" OR ${CMAKE_SYSTEM_NAME} MATCHES "Darwin" )
      add_definitions( -D_FILE_OFFSET_BITS=64 )
    elseif( ${CMAKE_SYSTEM_NAME} MATCHES "AIX" )
      add_definitions( -D_LARGE_FILES=64 )
    else()
      ecbuild_warn("ENABLE_LARGE_FILE_SUPPORT active, sizeof off_t is ${EC_SIZEOF_OFF_T} < 8 "
                   "but ecbuild does not know how to enable large files in this operating system")
    endif()

    get_directory_property( __compile_defs COMPILE_DEFINITIONS )

    if( __compile_defs )
      foreach( def ${__compile_defs} )
        list( APPEND CMAKE_REQUIRED_DEFINITIONS -D${def} )
      endforeach()
    endif()

  endif()

endmacro(ecbuild_add_large_file_support)

