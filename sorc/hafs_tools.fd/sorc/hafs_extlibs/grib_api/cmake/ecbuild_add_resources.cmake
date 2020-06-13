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
# ecbuild_add_resources
# =====================
#
# Add resources as project files but optionally exclude them from packaging. ::
#
#   ecbuild_add_resources( TARGET <name>
#                          [ SOURCES <source1> [<source2> ...] ]
#                          [ SOURCES_PACK <source1> [<source2> ...] ]
#                          [ SOURCES_DONT_PACK <source1> [<source2> ...] ]
#                          [ PACK <file1> [<file2> ...] ]
#                          [ DONT_PACK <file1> [<file2> ...] ]
#                          [ DONT_PACK_DIRS <directory1> [<directory2> ...] ]
#                          [ DONT_PACK_REGEX <regex1> [<regex2> ...] ] )
#
# Options
# -------
#
# TARGET : required
#   target name (target will only be created if there are any sources)
#
# SOURCES : optional, alias for SOURCES_PACK
#   list of source files included when packaging
#
# SOURCES_PACK : optional, alias for SOURCES
#   list of source files included when packaging
#
# SOURCES_DONT_PACK : optional
#   list of source files excluded when packaging
#
# PACK : optional, priority over DONT_PACK, DONT_PACK_DIRS, DONT_PACK_REGEX
#   list of files to include when packaging
#
# DONT_PACK : optional
#   list of files to exclude when packaging
#
# DONT_PACK_DIRS : optional
#   list of directories to exclude when packaging
#
# DONT_PACK_REGEX : optional
#   list of regular expressions to match files and directories to exclude when
#   packaging
#
# Note
# ----
#
# All file and directory names are also *partially matched*. To ensure that
# only the exact file or directory name is matched at the end of the path add
# a ``$`` at the end and quote the name.
#
##############################################################################

macro( ecbuild_add_resources )

    set( options )
    set( single_value_args TARGET )
    set( multi_value_args  SOURCES SOURCES_PACK SOURCES_DONT_PACK PACK DONT_PACK DONT_PACK_DIRS DONT_PACK_REGEX )

    cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

    if(_PAR_UNPARSED_ARGUMENTS)
      ecbuild_critical("Unknown keywords given to ecbuild_add_resources(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
    endif()

    if( NOT _PAR_TARGET  )
      ecbuild_critical("The call to ecbuild_add_resources() doesn't specify the TARGET.")
    endif()

    set( LOCAL_FILES_NOT_TO_PACK "" )

    # all recursive files are not to pack
    if( DEFINED _PAR_DONT_PACK_REGEX )
        foreach( exp ${_PAR_DONT_PACK_REGEX} )
            file( GLOB_RECURSE all_files_in_subdirs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${exp} )
            list( APPEND LOCAL_FILES_NOT_TO_PACK ${all_files_in_subdirs} )
        endforeach()
    endif()

    # selected dirs not to pack
    if( DEFINED _PAR_DONT_PACK_DIRS )
        foreach( dir ${_PAR_DONT_PACK_DIRS} )
            list( APPEND LOCAL_FILES_NOT_TO_PACK ${dir}/ )
        endforeach()
    endif()

    # selected files not to pack
    if( DEFINED _PAR_DONT_PACK )
        list( APPEND LOCAL_FILES_NOT_TO_PACK ${_PAR_DONT_PACK} )
    endif()

    # now lets remove files that we want to pack from the list
    # note that these have priority over the files not to pack
    # so we can GLOB_RECURSE * -> DONT_PACK and then select only the ones we pack

    # files to pack but are not project files
    if( DEFINED _PAR_PACK )
        foreach( file ${_PAR_PACK} )
            list( REMOVE_ITEM LOCAL_FILES_NOT_TO_PACK ${file} )
        endforeach()
    endif()

    # define as project files, but dont pack them
    if( DEFINED _PAR_SOURCES_DONT_PACK )
        list( APPEND LOCAL_FILES_NOT_TO_PACK ${_PAR_SOURCES_DONT_PACK} )
		foreach( sfile ${_PAR_SOURCES_DONT_PACK} )
			set( _full_sfile "${CMAKE_CURRENT_SOURCE_DIR}/${sfile}" )
	        if( EXISTS ${_full_sfile} )
				list( APPEND ${_PAR_TARGET}_files ${_full_sfile} )
			endif()
		endforeach()
    endif()

    # define as project files and pack them
    # SOURCES_PACK is alias to SOURCES
    if( DEFINED _PAR_SOURCES_PACK )
        list( APPEND _PAR_SOURCES ${_PAR_SOURCES_PACK} )
    endif()
    if( DEFINED _PAR_SOURCES )
        list( APPEND ${_PAR_TARGET}_files ${_PAR_SOURCES} )
        foreach( file ${_PAR_SOURCES} )
            list( REMOVE_ITEM LOCAL_FILES_NOT_TO_PACK ${file} )
        endforeach()
    endif()

    # there are project files, so lets create the target
    if( DEFINED ${_PAR_TARGET}_files )
        add_custom_target( ${_PAR_TARGET} SOURCES ${${_PAR_TARGET}_files} )
    endif()

    # remove CMakeLists.txt
    foreach( file ${LOCAL_FILES_NOT_TO_PACK} )
        if( ${file} MATCHES "CMakeLists.txt" )
            list( REMOVE_ITEM LOCAL_FILES_NOT_TO_PACK ${file} )
        endif()
    endforeach()

    # transform the local files  to full absolute paths
    # and place them in the global list of files not to pack
    foreach( file ${LOCAL_FILES_NOT_TO_PACK} )
        list( APPEND ECBUILD_DONT_PACK_FILES ${CMAKE_CURRENT_SOURCE_DIR}/${file} )
    endforeach()

    # save cache if we added any files not to pack
    if( LOCAL_FILES_NOT_TO_PACK )
        set( ECBUILD_DONT_PACK_FILES ${ECBUILD_DONT_PACK_FILES} CACHE INTERNAL "" )
    endif()

endmacro( ecbuild_add_resources  )
