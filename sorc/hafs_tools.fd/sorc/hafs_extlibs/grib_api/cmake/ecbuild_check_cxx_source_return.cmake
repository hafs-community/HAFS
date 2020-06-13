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
# ecbuild_check_cxx_source_return
# ===============================
#
# Compile and run a given C++ code and return its output. ::
#
#   ecbuild_check_cxx_source_return( <source>
#                                    VAR <name>
#                                    OUTPUT <name>
#                                    [ INCLUDES <path1> [ <path2> ... ] ]
#                                    [ LIBS <library1> [ <library2> ... ] ]
#                                    [ DEFINITIONS <definition1> [ <definition2> ... ] ] )
#
# Options
# -------
#
# VAR : required
#   name of the check and name of the CMake variable to write result to
#
# OUTPUT : required
#   name of CMake variable to write the output to
#
# INCLUDES : optional
#   list of paths to add to include directories
#
# LIBS : optional
#   list of libraries to link against (CMake targets or external libraries)
#
# DEFINITIONS : optional
#   list of definitions to add to preprocessor defines
#
# Usage
# -----
#
# This will write the given source to a .cxx file and compile and run it with
# ecbuild_try_run. If successful, ``${VAR}`` is set to 1 and ``${OUTPUT}`` is
# set to the output of the successful run in the CMake cache.
#
# The check will not run if ``${VAR}`` is defined (e.g. from ecBuild cache).
#
##############################################################################

macro( ecbuild_check_cxx_source_return SOURCE )

    set( options )
    set( single_value_args VAR  OUTPUT )
    set( multi_value_args  INCLUDES LIBS DEFINITIONS )

    cmake_parse_arguments( _p "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

    if(_p_UNPARSED_ARGUMENTS)
      ecbuild_critical("Unknown keywords given to ecbuild_check_cxx_source_return(): \"${_p_UNPARSED_ARGUMENTS}\"")
    endif()

    if( NOT _p_VAR OR NOT _p_OUTPUT )
      ecbuild_critical("The call to ecbuild_check_cxx_source_return() doesn't specify either SOURCE, VAR or OUTPUT")
    endif()

    set( _msg "Testing ${_p_VAR}:" )

    if( NOT DEFINED ${_p_VAR} )

        set(MACRO_CHECK_FUNCTION_DEFINITIONS "-D${_p_VAR} ${CMAKE_REQUIRED_FLAGS}")

        set(CHECK_CXX_SOURCE_COMPILES_ADD_LIBRARIES)
        if(CMAKE_REQUIRED_LIBRARIES)
            list( APPEND __add_libs ${CMAKE_REQUIRED_LIBRARIES} )
        endif()
        if( _p_LIBS )
            list( APPEND __add_libs ${_p_LIBS} )
        endif()
        if( __add_libs )
            set(CHECK_CXX_SOURCE_COMPILES_ADD_LIBRARIES "-DLINK_LIBRARIES:STRING=${__add_libs}")
        endif()

        set(CHECK_CXX_SOURCE_COMPILES_ADD_INCLUDES)
        if(CMAKE_REQUIRED_INCLUDES)
            list( APPEND __add_incs ${CMAKE_REQUIRED_INCLUDES} )
        endif()
        if( _p_INCLUDES )
            list( APPEND __add_incs ${_p_INCLUDES} )
        endif()
        if( __add_incs )
            set(CHECK_CXX_SOURCE_COMPILES_ADD_INCLUDES "-DINCLUDE_DIRECTORIES:STRING=${__add_incs}")
        endif()

        # write the source file

        file( WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CheckCXXSource/test_${_p_VAR}.cxx" "${SOURCE}\n" )

        ecbuild_debug( "${_msg}" )
        ecbuild_try_run( ${_p_VAR}_EXITCODE ${_p_VAR}_COMPILED
          ${CMAKE_BINARY_DIR}
          ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CheckCXXSource/test_${_p_VAR}.cxx
          COMPILE_DEFINITIONS ${CMAKE_REQUIRED_DEFINITIONS}
          CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=${MACRO_CHECK_FUNCTION_DEFINITIONS}
          -DCMAKE_SKIP_RPATH:BOOL=${CMAKE_SKIP_RPATH}
          "${CHECK_CXX_SOURCE_COMPILES_ADD_LIBRARIES}"
          "${CHECK_CXX_SOURCE_COMPILES_ADD_INCLUDES}"
          COMPILE_OUTPUT_VARIABLE compile_OUTPUT
          RUN_OUTPUT_VARIABLE     run_OUTPUT )

        # ecbuild_debug_var( ${_p_VAR}_COMPILED )
        # ecbuild_debug_var( ${_p_VAR}_EXITCODE )

        # if it did not compile make the return value fail code of 1

        if( NOT ${_p_VAR}_COMPILED )
          ecbuild_debug( "${_msg} failed to compile" )
        endif()

        if( "${${_p_VAR}_EXITCODE}" MATCHES  "FAILED_TO_RUN" )
          ecbuild_debug( "${_msg} failed to run" )
        endif()

        # if the return value was 0 then it worked
        if( ${_p_VAR}_COMPILED AND "${${_p_VAR}_EXITCODE}" EQUAL 0 )

          ecbuild_debug("${_msg} Success")
          file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
            "Performing C++ SOURCE FILE Test ${_p_VAR} succeded with the following compile output:\n"
            "${compile_OUTPUT}\n"
            "Performing C++ SOURCE FILE Run ${_p_VAR} succeded with the following run output:\n"
            "${run_OUTPUT}\n"
            "Return value: ${${_p_VAR}}\n"
            "Source file was:\n${SOURCE}\n")

          set( ${_p_VAR}     1              CACHE INTERNAL "Test ${_p_VAR}")
          set( ${_p_OUTPUT} "${run_OUTPUT}" CACHE INTERNAL "Test ${_p_VAR} output")

        else()

          if(CMAKE_CROSSCOMPILING AND "${${_p_VAR}_EXITCODE}" MATCHES  "FAILED_TO_RUN")
            set(${_p_VAR} "${${_p_VAR}_EXITCODE}")
            set(${OUTPUT} "")
          else()
            set(${_p_VAR} "" CACHE INTERNAL "Test ${_p_VAR}")
            set(${_p_OUTPUT} "" CACHE INTERNAL "Test ${_p_VAR} output")
          endif()

          ecbuild_debug("Test ${_p_VAR} - Failed")
          file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
            "Performing C++ SOURCE FILE Test ${_p_VAR} failed with the following compile output:\n"
            "${compile_OUTPUT}\n"
            "Performing C++ SOURCE FILE Run ${_p_VAR} failed with the following run output:\n"
            "${run_OUTPUT}\n"
            "Return value: ${${_p_VAR}_EXITCODE}\n"
            "Source file was:\n${SOURCE}\n")
        endif()

    endif()

endmacro()
