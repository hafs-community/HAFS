# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

##############################################################################
#.rst:
#
# ecbuild_dont_pack
# =================
#
# Specify files and directories to exclude from packaging. ::
#
#   ecbuild_dont_pack( [ FILES <file1> [ <file2> ... ] ]
#                      [ DIRS <dir1> [ <dir2> ... ] ]
#                      [ REGEX <regex> ] )
#
# Options
# -------
#
# FILES : optional, one of FILES, DIRS, REGEX required
#   list of files to exclude from packaging
#
# DIRS : optional, one of FILES, DIRS, REGEX required
#   list of directories to exclude from packaging
#
# REGEX : optional, one of FILES, DIRS, REGEX required
#   regular expression to match files / directories to exclude from packaging
#
##############################################################################

macro( ecbuild_dont_pack )

    set( options )
    set( single_value_args REGEX )
    set( multi_value_args  FILES DIRS )

    cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

    if(_PAR_UNPARSED_ARGUMENTS)
      ecbuild_critical("Unknown keywords given to ecbuild_dont_pack(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
    endif()

    if( NOT DEFINED _PAR_REGEX AND NOT  DEFINED _PAR_FILES AND NOT  DEFINED _PAR_DIRS )
      ecbuild_critical("Call to ecbuild_dont_pack does not speficify any list to avoid packing.")
    endif()

    set( LOCAL_FILES_NOT_TO_PACK "" )

    # all recursive files are not to pack
    if( DEFINED _PAR_REGEX )
        file( GLOB_RECURSE all_files_in_subdirs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${_PAR_REGEX} )
        list( APPEND LOCAL_FILES_NOT_TO_PACK ${all_files_in_subdirs} )
    endif()

    # selected dirs not to pack
    if( DEFINED _PAR_DIRS )
        foreach( dir ${_PAR_DIRS} )
            list( APPEND LOCAL_FILES_NOT_TO_PACK ${dir}/ )
        endforeach()
    endif()

    # selected files not to pack
    if( DEFINED _PAR_FILES )
        list( APPEND LOCAL_FILES_NOT_TO_PACK ${_PAR_FILES} )
    endif()

    # transform the local files  to full absolute paths
    # and place them in the global list of files not to pack
    foreach( file ${LOCAL_FILES_NOT_TO_PACK} )
        list( APPEND ECBUILD_DONT_PACK_FILES ${CMAKE_CURRENT_SOURCE_DIR}/${file} )
    endforeach()

    # save cache if we added any files not to pack
    if( LOCAL_FILES_NOT_TO_PACK )
        list(REMOVE_DUPLICATES ECBUILD_DONT_PACK_FILES)
        set( ECBUILD_DONT_PACK_FILES ${ECBUILD_DONT_PACK_FILES} CACHE INTERNAL "" )
    endif()

endmacro()
