# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# resert the variable on each configure
set( EC_UNUSED_FILES "" CACHE INTERNAL "unused files" )

##############################################################################
# finds project files and adds them to the passed variable

macro( ecbuild_find_files_recursive aFileList )

list( APPEND ecbuild_project_extensions c cc cpp cxx ) # for the moment skip ( h hh )

foreach( aExt ${ecbuild_project_extensions} )
  set( globPatterns ${globPatterns} *.${aExt} )
endforeach()

# This globs for only one pattern at a time
# Shell extglob patterns are unfortunately not supported.
file( GLOB_RECURSE ${aFileList} ${globPatterns} )

endmacro()

##############################################################################
# finds the unused files on all the project
function( ecbuild_find_project_files )

  # Only do this if we actually care to warn about unused files
  if( CHECK_UNUSED_FILES )
    ecbuild_find_files_recursive( cwdFiles )

    # this list will be kept
    set( EC_PROJECT_FILES ${EC_PROJECT_FILES} ${cwdFiles} CACHE INTERNAL "" )
    # this list will be progressevely emptied
    set( EC_UNUSED_FILES  ${EC_UNUSED_FILES}  ${cwdFiles} CACHE INTERNAL "" )
  endif()

endfunction()

##############################################################################
# removed used files from unused list
macro( ecbuild_declare_project_files )

  # Only do this if we actually care to warn about unused files
  if( CHECK_UNUSED_FILES )
    foreach( _afile ${ARGV} )

      # ecbuild_debug_var( _afile )

      get_property( _src_gen SOURCE ${_afile} PROPERTY GENERATED )

      if( NOT _src_gen )

        get_filename_component( _abspath ${_afile} ABSOLUTE )

        # check for existance of all declared files
        if( EXISTS ${_abspath} )
            list( REMOVE_ITEM EC_UNUSED_FILES ${_abspath} )
        else()
        ecbuild_critical( "In directory ${CMAKE_CURRENT_SOURCE_DIR} file ${_afile} was declared in CMakeLists.txt but not found" )
        endif()
      endif()

    endforeach()

    # rewrite the unused file list in cache
    set( EC_UNUSED_FILES ${EC_UNUSED_FILES} CACHE INTERNAL "unused files" )
  endif()

endmacro()
