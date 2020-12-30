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
# ecbuild_add_library
# ===================
#
# Add a library with a given list of source files. ::
#
#   ecbuild_add_library( TARGET <name>
#                        SOURCES <source1> [<source2> ...]
#                        [ SOURCES_GLOB <glob1> [<glob2> ...] ]
#                        [ SOURCES_EXCLUDE_REGEX <regex1> [<regex2> ...] ]
#                        [ TYPE SHARED|STATIC|MODULE|OBJECT|INTERFACE ]
#                        [ OBJECTS <obj1> [<obj2> ...] ]
#                        [ TEMPLATES <template1> [<template2> ...] ]
#                        [ LIBS <library1> [<library2> ...] ]
#                        [ PRIVATE_LIBS <library1> [<library2> ...] ]
#                        [ PUBLIC_LIBS <library1> [<library2> ...] ]
#                        [ INCLUDES <path1> [<path2> ...] ]
#                        [ PRIVATE_INCLUDES <path1> [<path2> ...] ]
#                        [ PUBLIC_INCLUDES <path1> [<path2> ...] ]
#                        [ DEFINITIONS <definition1> [<definition2> ...] ]
#                        [ PERSISTENT <file1> [<file2> ...] ]
#                        [ GENERATED <file1> [<file2> ...] ]
#                        [ DEPENDS <target1> [<target2> ...] ]
#                        [ CONDITION <condition> ]
#                        [ PROPERTIES <prop1> <val1> [<prop2> <val2> ...] ]
#                        [ NOINSTALL ]
#                        [ HEADER_DESTINATION <path> ]
#                        [ INSTALL_HEADERS LISTED|ALL ]
#                        [ INSTALL_HEADERS_LIST <header1> [<header2> ...] ]
#                        [ INSTALL_HEADERS_REGEX <pattern> ]
#                        [ VERSION <version> | AUTO_VERSION ]
#                        [ SOVERSION <soversion> | AUTO_SOVERSION ]
#                        [ CFLAGS <flag1> [<flag2> ...] ]
#                        [ CXXFLAGS <flag1> [<flag2> ...] ]
#                        [ FFLAGS <flag1> [<flag2> ...] ]
#                        [ LINKER_LANGUAGE <lang> ]
#                        [ OUTPUT_NAME <name> ] )
#
# Options
# -------
#
# TARGET : required
#   target name
#
# SOURCES : required
#   list of source files
#
# TYPE : optional
#   library type, one of:
#
#   :SHARED:    libraries are linked dynamically and loaded at runtime
#   :STATIC:    archives of object files for use when linking other targets.
#   :MODULE:    plugins that are not linked into other targets but may be loaded
#               dynamically at runtime using dlopen-like functionality
#   :OBJECT:    files are just compiled into objects
#   :INTERFACE: no direct build output, but can be used to aggregate headers,
#               compilation flags and libraries
#
# SOURCES_GLOB : optional
#   search pattern to find source files to compile (note: not recommend according to CMake guidelines)
#   it is usually better to explicitly list the source files in the CMakeList.txt
#
# SOURCES_EXCLUDE_REGEX : optional
#   search pattern to exclude source files from compilation, applies o the results of SOURCES_GLOB
#
# OBJECTS : optional
#   list of object libraries to add to this target
#
# TEMPLATES : optional
#   list of files specified as SOURCES which are not to be compiled separately
#   (these are commonly template implementation files included in a header)
#
# LIBS : (DEPRECATED) optional
#   list of libraries to link against (CMake targets or external libraries),
#   behaves as PUBLIC_LIBS
#   Please use target_link_libraries instead
#
# PRIVATE_LIBS : optional
#   list of libraries to link against (CMake targets or external libraries),
#   they will not be exported
#
# PUBLIC_LIBS : optional
#   list of libraries to link against (CMake targets or external libraries),
#   they will be exported
#
# INCLUDES : (DEPRECATED) optional
#   list of paths to add to include directories, behaves as PUBLIC_INCLUDES
#   Please use target_include_directories instead
#
# PUBLIC_INCLUDES : (DEPRECATED) optional
#   list of paths to add to include directories which will be publicly exported to other projects
#   Please use target_include_directories instead
#
# PRIVATE_INCLUDES : (DEPRECATED) optional
#   list of paths to add to include directories which won't be exported to other projects
#   Please use target_include_directories instead
#
# DEFINITIONS : (DEPRECATED) optional
#   list of definitions to add to preprocessor defines
#   Please use target_compile_definitions instead
#
# PERSISTENT : optional
#   list of persistent layer object files
#
# GENERATED : optional
#   list of files to mark as generated (sets GENERATED source file property)
#
# DEPENDS : optional
#   list of targets to be built before this target
#
# CONDITION : optional
#   conditional expression which must evaluate to true for this target to be
#   built (must be valid in a CMake ``if`` statement)
#
# PROPERTIES : optional
#   custom properties to set on the target
#
# NOINSTALL : optional
#   do not install the library
#
# HEADER_DESTINATION
#   directory to install headers (if not specified, INSTALL_INCLUDE_DIR is used)
#   Note: this directory will automatically be added to target_include_directories
#
# INSTALL_HEADERS : optional
#   specify which header files to install:
#
#   :LISTED: install header files listed as SOURCES
#   :ALL:    install all header files ending in .h, .hh, .hpp, .H
#
# INSTALL_HEADERS_LIST : optional
#   list of extra headers to install
#
# INSTALL_HEADERS_REGEX : optional
#   regular expression to match extra headers to install
#
# VERSION : optional, AUTO_VERSION or LIBS_VERSION is used if not specified
#   build version of the library
#
# AUTO_VERSION : optional, ignored if VERSION is specified
#   use MAJOR.MINOR package version as build version of the library
#
# SOVERSION : optional, AUTO_SOVERSION or LIBS_SOVERSION is used if not specified
#   ABI version of the library
#
# AUTO_SOVERSION : optional, ignored if SOVERSION is specified
#   use MAJOR package version as ABI version of the library
#
# CFLAGS : optional
#   list of C compiler flags to use for all C source files
#
#   See usage note below.
#
# CXXFLAGS : optional
#   list of C++ compiler flags to use for all C++ source files
#
#   See usage note below.
#
# FFLAGS : optional
#   list of Fortran compiler flags to use for all Fortran source files
#
#   See usage note below.
#
# LINKER_LANGUAGE : optional
#   sets the LINKER_LANGUAGE property on the target
#
# OUTPUT_NAME : optional
#   sets the OUTPUT_NAME property on the target
#
# Usage
# -----
#
# The ``CFLAGS``, ``CXXFLAGS`` and ``FFLAGS`` options apply the given compiler
# flags to all C, C++ and Fortran sources passed to this command, respectively.
# If any two ``ecbuild_add_executable``, ``ecbuild_add_library`` or
# ``ecbuild_add_test`` commands are passed the *same* source file and each sets
# a different value for the compiler flags to be applied to that file (including
# when one command adds flags and another adds none), then the two commands
# will be in conflict and the result may not be as expected.
#
# For this reason it is recommended not to use the ``*FLAGS`` options when
# multiple targets share the same source files, unless the exact same flags are
# applied to those sources by each relevant command.
#
# Care should also be taken to ensure that these commands are not passed source
# files which are not required to build the target, if those sources are also
# passed to other commands which set different compiler flags.
#
##############################################################################

function( ecbuild_add_library_impl )

  set( options NOINSTALL AUTO_VERSION AUTO_SOVERSION )
  set( single_value_args TARGET TYPE COMPONENT INSTALL_HEADERS
                         INSTALL_HEADERS_REGEX LINKER_LANGUAGE
                         HEADER_DESTINATION VERSION SOVERSION OUTPUT_NAME )
  set( multi_value_args  SOURCES SOURCES_GLOB SOURCES_EXCLUDE_REGEX OBJECTS
                         TEMPLATES LIBS PRIVATE_LIBS PUBLIC_LIBS INCLUDES
                         PRIVATE_INCLUDES PUBLIC_INCLUDES DEPENDS PERSISTENT
                         DEFINITIONS INSTALL_HEADERS_LIST CFLAGS CXXFLAGS
                         FFLAGS GENERATED CONDITION PROPERTIES )

  cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if(_PAR_UNPARSED_ARGUMENTS)
    ecbuild_critical("Unknown keywords given to ecbuild_add_library(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
  endif()

  if( NOT _PAR_TARGET  )
    ecbuild_critical("The call to ecbuild_add_library() doesn't specify the TARGET.")
  endif()

  if( NOT _PAR_TYPE MATCHES "INTERFACE" )
    if( NOT _PAR_SOURCES AND NOT _PAR_OBJECTS AND NOT _PAR_SOURCES_GLOB )
      ecbuild_critical("The call to ecbuild_add_library() specifies neither SOURCES nor OBJECTS nor SOURCES_GLOB")
    endif()
  endif()

  ### conditional build
  ecbuild_evaluate_dynamic_condition( _PAR_CONDITION _${_PAR_TARGET}_condition )

  if( _${_PAR_TARGET}_condition )

    # defines the type of library
    if( DEFINED _PAR_TYPE )
      # checks that is either SHARED or STATIC or MODULE
      if( NOT _PAR_TYPE MATCHES "STATIC" AND
          NOT _PAR_TYPE MATCHES "SHARED" AND
          NOT _PAR_TYPE MATCHES "OBJECT" AND
          NOT _PAR_TYPE MATCHES "MODULE" AND
          NOT _PAR_TYPE MATCHES "INTERFACE" )
        ecbuild_critical( "library type must be one of [ STATIC | SHARED | MODULE | OBJECT | INTERFACE ]" )
      endif()
      ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): library type is ${_PAR_TYPE}")
    endif()

    # insert already compiled objects (from OBJECT libraries)
    unset( _all_objects )
    foreach( _obj ${_PAR_OBJECTS} )
      list( APPEND _all_objects $<TARGET_OBJECTS:${_obj}> )
    endforeach()

    # glob sources
    unset( _glob_srcs )
    foreach( pattern ${_PAR_SOURCES_GLOB} )
      ecbuild_list_add_pattern( LIST _glob_srcs GLOB "${pattern}" )
    endforeach()

    foreach( pattern ${_PAR_SOURCES_EXCLUDE_REGEX} )
      ecbuild_list_exclude_pattern( LIST _glob_srcs REGEX "${pattern}" )
    endforeach()
    list( APPEND _PAR_SOURCES ${_glob_srcs} )

    if( ECBUILD_LIST_SOURCES )
      ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): sources ${_PAR_SOURCES}")
    endif()

    # add persistent layer files
    ecbuild_add_persistent( SRC_LIST _PAR_SOURCES FILES ${_PAR_PERSISTENT} NAMESPACE "${PERSISTENT_NAMESPACE}" )

    # remove templates from compilation sources
    if( DEFINED _PAR_TEMPLATES )
      ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): removing ${_PAR_TEMPLATES} from sources")
      list( REMOVE_ITEM _PAR_SOURCES ${_PAR_TEMPLATES} )
      add_custom_target( ${_PAR_TARGET}_templates SOURCES ${_PAR_TEMPLATES} )
    endif()

    # Separate sources
    if( _PAR_SOURCES )
      ecbuild_separate_sources( TARGET ${_PAR_TARGET} SOURCES ${_PAR_SOURCES} )
    endif()

    # Purge the sources list for interface libraries (now that they have been filtered)
    if( _PAR_TYPE MATCHES "INTERFACE" )
      set( _PAR_SOURCES "" )
    endif()

    if( ${_PAR_TARGET}_cuda_srcs )
      if( NOT CUDA_FOUND )
          ecbuild_error("ecbuild_add_library(${_PAR_TARGET}): CUDA source files detected"
                        "but CUDA was not found.")
      endif()
      if( _PAR_TYPE MATCHES "OBJECT" )
          ecbuild_error("ecbuild_add_library(${_PAR_TARGET}): CUDA source files detected"
                        "but CMake OBJECT libraries with CUDA are not supported.")
      endif()
      ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): CUDA sources detected."
                    "Building library with cuda_add_library() rather than intrinsic"
                    "add_library().")
    endif()

    if( NOT ${_PAR_TARGET}_cuda_srcs )
      add_library( ${_PAR_TARGET} ${_PAR_TYPE} ${_PAR_SOURCES}  ${_all_objects} )
    else()
      if( NOT DEFINED CUDA_LINK_LIBRARIES_KEYWORD )
        set ( CUDA_LINK_LIBRARIES_KEYWORD PRIVATE )
      endif()
      cuda_add_library( ${_PAR_TARGET} ${_PAR_TYPE} ${_PAR_SOURCES}  ${_all_objects} )
    endif()
    # ecbuild_echo_target( ${_PAR_TARGET} )

    # Set custom properties
    if( ${_PAR_PROPERTIES} )
      set_target_properties( ${_PAR_TARGET} PROPERTIES ${_PAR_PROPERTIES} )
    endif()

    # set OUTPUT_NAME

    if( DEFINED _PAR_OUTPUT_NAME )
      ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): set OUTPUT_NAME to ${_PAR_OUTPUT_NAME}")
      set_target_properties( ${_PAR_TARGET} PROPERTIES OUTPUT_NAME ${_PAR_OUTPUT_NAME} )
    endif()

    # add extra dependencies
    if( DEFINED _PAR_DEPENDS)
      ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): add dependency on ${_PAR_DEPENDS}")
      add_dependencies( ${_PAR_TARGET} ${_PAR_DEPENDS} )
    endif()

    # For interface libraries, there is no build requirement, therefore only
    # the INTERFACE properties should be populated
    set( _PUBLIC_INTF "PUBLIC" )
    if( _PAR_TYPE MATCHES "INTERFACE" )
      set( _PUBLIC_INTF "INTERFACE" )
    endif()

    # takes a list of possible includes LIST and a INTF parameter
    function(__addDeps)
      set( options )
      set( single_value_args TYPE INTF )
      set( multi_value_args  LIST )

      cmake_parse_arguments( _p "${options}" "${single_value_args}" "${multi_value_args}" ${ARGN} )

      if( "${_p_TYPE}" STREQUAL LIBS )
        list(REMOVE_ITEM _p_LIST debug)
        list(REMOVE_ITEM _p_LIST optimized)
      endif()
      ecbuild_filter_list(${_p_TYPE} LIST ${_p_LIST} LIST_INCLUDE deps LIST_EXCLUDE skipped_deps)
      if( "${_p_INTF}" STREQUAL "LEGACY" )
        if(ECBUILD_2_COMPAT_DEPRECATE)
          ecbuild_deprecate("ecbuild_add_library(${_PAR_TARGET}): the usage of ${_p_TYPE} is deprecated. Use PUBLIC_${_p_TYPE} or PRIVATE_${_p_TYPE}.")
        endif()
        set(_p_INTF ${_PUBLIC_INTF})
      endif()


      if( "${_p_TYPE}" STREQUAL LIBS )
        target_link_libraries( ${_PAR_TARGET} ${_p_INTF} ${deps} )
        ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): linking with [${deps}] ${_p_INTF}")
        ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): [${skipped_deps}] not found - not linking ${_p_INTF}")
      else()
        target_include_directories( ${_PAR_TARGET} ${_p_INTF} ${deps} )
        ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): add [${deps}] to include_directories ${_p_INTF}")
        ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): [${skipped_deps}] not found - not adding to include_directories ${_p_INTF}")
      endif()
    endfunction()

    # add the link libraries
    if( DEFINED _PAR_LIBS )
      __addDeps(TYPE LIBS LIST ${_PAR_LIBS} INTF LEGACY)
    endif()

    # add the private link libraries
    if( DEFINED _PAR_PRIVATE_LIBS )
      __addDeps(TYPE LIBS LIST ${_PAR_PRIVATE_LIBS} INTF PRIVATE)
    endif()

    # add the public link libraries
    if( DEFINED _PAR_PUBLIC_LIBS )
      __addDeps(TYPE LIBS LIST ${_PAR_PUBLIC_LIBS} INTF ${_PUBLIC_INTF})
    endif()

    # add include dirs if defined
    if( DEFINED _PAR_INCLUDES )
      __addDeps(TYPE INCLUDES LIST ${_PAR_INCLUDES} INTF LEGACY)
    endif()

    # add private include dirs if defined
    if( DEFINED _PAR_PRIVATE_INCLUDES )
      __addDeps(TYPE INCLUDES LIST ${_PAR_PRIVATE_INCLUDES} INTF PRIVATE)
    endif()

    # add public include dirs if defined
    if( DEFINED _PAR_PUBLIC_INCLUDES )
      __addDeps(TYPE INCLUDES LIST ${_PAR_PUBLIC_INCLUDES} INTF ${_PUBLIC_INTF})
    endif()

    # FIX: Cray compiler PIC option is not detected by CMake

    if( NOT _PAR_TYPE MATCHES "INTERFACE" )
      get_property( _target_pic TARGET ${_PAR_TARGET} PROPERTY POSITION_INDEPENDENT_CODE )
      if( _target_pic )
        if( "${CMAKE_C_COMPILER_ID}" STREQUAL "Cray" )
          set( _PAR_CFLAGS "-fPIC -h PIC ${_PAR_CFLAGS}" )
        endif()
        if( "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Cray" )
          set( _PAR_CXXFLAGS "-fPIC -h PIC ${_PAR_CXXFLAGS}" )
        endif()
        if( "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Cray" )
          set( _PAR_FFLAGS "-fPIC -h PIC ${_PAR_FFLAGS}" )
        endif()
      endif()
    endif()

    # define VERSION if requested
    if( DEFINED _PAR_VERSION )
      ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): set build version to ${_PAR_VERSION}")
      set_target_properties( ${_PAR_TARGET} PROPERTIES VERSION "${_PAR_VERSION}" )
    else()
      if( _PAR_AUTO_VERSION OR LIBS_VERSION MATCHES "[Aa][Uu][Tt][Oo]")
        ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): set build version to ${${PROJECT_NAME}_VERSION_MAJOR}.${${PROJECT_NAME}_VERSION_MINOR} (auto)")
        set_target_properties( ${_PAR_TARGET} PROPERTIES VERSION "${${PROJECT_NAME}_VERSION_MAJOR}.${${PROJECT_NAME}_VERSION_MINOR}" )
      elseif( DEFINED LIBS_VERSION )
        ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): set build version to ${LIBS_VERSION}")
        set_target_properties( ${_PAR_TARGET} PROPERTIES VERSION "${LIBS_VERSION}" )
      endif()
    endif()

    # define SOVERSION if requested
    if( DEFINED _PAR_SOVERSION )
      ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): set ABI version to ${_PAR_SOVERSION}")
      set_target_properties( ${_PAR_TARGET} PROPERTIES SOVERSION "${_PAR_SOVERSION}" )
    else()
      if( _PAR_AUTO_SOVERSION OR LIBS_SOVERSION MATCHES "[Aa][Uu][Tt][Oo]")
        ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): set ABI version to ${${PROJECT_NAME}_VERSION_MAJOR} (auto)")
        set_target_properties( ${_PAR_TARGET} PROPERTIES SOVERSION "${${PROJECT_NAME}_MAJOR_VERSION}" )
      elseif( DEFINED LIBS_SOVERSION )
        ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): set ABI version to ${LIBS_SOVERSION}")
        set_target_properties( ${_PAR_TARGET} PROPERTIES SOVERSION "${LIBS_SOVERSION}" )
      endif()
    endif()

    # Override compilation flags on a per source file basis
    ecbuild_target_flags( ${_PAR_TARGET} "${_PAR_CFLAGS}" "${_PAR_CXXFLAGS}" "${_PAR_FFLAGS}" )

    if( DEFINED _PAR_GENERATED )
      ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): mark as generated ${_PAR_GENERATED}")
      set_source_files_properties( ${_PAR_GENERATED} PROPERTIES GENERATED 1 )
    endif()

    # set linker language
    if( DEFINED _PAR_LINKER_LANGUAGE )
      ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): using linker language ${_PAR_LINKER_LANGUAGE}")
      set_target_properties( ${_PAR_TARGET} PROPERTIES LINKER_LANGUAGE ${_PAR_LINKER_LANGUAGE} )
      if( ECBUILD_${_PAR_LINKER_LANGUAGE}_IMPLICIT_LINK_LIBRARIES )
        target_link_libraries( ${_PAR_TARGET} PRIVATE ${ECBUILD_${_PAR_LINKER_LANGUAGE}_IMPLICIT_LINK_LIBRARIES} )
      endif()
    endif()

    if( NOT _PAR_TYPE MATCHES "OBJECT" AND ECBUILD_IMPLICIT_LINK_LIBRARIES )
      target_link_libraries( ${_PAR_TARGET} PRIVATE ${ECBUILD_IMPLICIT_LINK_LIBRARIES} )
    endif()

    # Publish the relevant include directories for the headers
    if(ECBUILD_2_COMPAT)
      target_include_directories( ${_PAR_TARGET} ${_PUBLIC_INTF} $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}> )
      get_property(_incdirs DIRECTORY PROPERTY INCLUDE_DIRECTORIES)
      string( TOUPPER "${PROJECT_NAME}" PNAME )
      if( ${PNAME}_INCLUDE_DIRS )
        foreach( incdir ${${PNAME}_INCLUDE_DIRS} )
            list( APPEND _incdirs ${incdir} )
        endforeach()
      endif()
      list( REMOVE_DUPLICATES _incdirs )
      foreach(incdir ${_incdirs})
        if(NOT incdir MATCHES "^\\$<")
          file(RELATIVE_PATH _rel_inc ${PROJECT_SOURCE_DIR} "${incdir}")
          if(NOT _rel_inc MATCHES "^\\.\\./")
            target_include_directories( ${_PAR_TARGET} ${_PUBLIC_INTF} $<BUILD_INTERFACE:${incdir}> )
          endif()
          file(RELATIVE_PATH _rel_inc ${PROJECT_BINARY_DIR} "${incdir}")
          if(NOT _rel_inc MATCHES "^\\.\\./")
            target_include_directories( ${_PAR_TARGET} ${_PUBLIC_INTF} $<BUILD_INTERFACE:${incdir}> )
          endif()
        endif()
      endforeach()
    endif()

    # installation (except for OBJECT libraries)

    if( NOT _PAR_NOINSTALL AND NOT _PAR_TYPE MATCHES "OBJECT" )
      ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): installing to ${INSTALL_LIB_DIR}")

      # and associate with defined component
      #            if( DEFINED _PAR_COMPONENT )
      #                set( COMPONENT_DIRECTIVE "${_PAR_COMPONENT}" )
      #            else()
      #                set( COMPONENT_DIRECTIVE "${PROJECT_NAME}" )
      #            endif()

      install( TARGETS ${_PAR_TARGET}
        EXPORT  ${PROJECT_NAME}-targets
        RUNTIME DESTINATION ${INSTALL_BIN_DIR}
        LIBRARY DESTINATION ${INSTALL_LIB_DIR}
        ARCHIVE DESTINATION ${INSTALL_LIB_DIR} )
      #              COMPONENT ${COMPONENT_DIRECTIVE} )

      if(ECBUILD_INSTALL_LIBRARY_HEADERS)

        if( _PAR_HEADER_DESTINATION )
          set( _h_destination "${_PAR_HEADER_DESTINATION}" )
        else()
          set( _h_destination "${INSTALL_INCLUDE_DIR}" )
        endif()

        if( _PAR_INSTALL_HEADERS )

          if( _PAR_INSTALL_HEADERS MATCHES "LISTED" )
            foreach( file ${${_PAR_TARGET}_h_srcs} )
              get_filename_component( _file_dir ${file} PATH )
              install( FILES ${file} DESTINATION "${_h_destination}/${_file_dir}" )
            endforeach()
            if( DEFINED _PAR_TEMPLATES )
              foreach( file ${_PAR_TEMPLATES} )
                get_filename_component( _file_dir ${file} PATH )
                install( FILES ${file} DESTINATION "${_h_destination}/${_file_dir}" )
              endforeach()
            endif()
            if( DEFINED _PAR_PERSISTENT )
              foreach( file ${_PAR_PERSISTENT} )
                get_filename_component( _file_dir ${file} PATH )
                get_filename_component( _file_we  ${file} NAME_WE )
                set( pfile "${CMAKE_CURRENT_BINARY_DIR}/${_file_dir}/${_file_we}.b" )
                install( FILES ${pfile} DESTINATION "${_h_destination}/${_file_dir}" )
              endforeach()
            endif()
          endif()
          if( _PAR_INSTALL_HEADERS MATCHES "ALL" )
            install( DIRECTORY ./  DESTINATION ${_h_destination} FILES_MATCHING PATTERN "*.h" )
            install( DIRECTORY ./  DESTINATION ${_h_destination} FILES_MATCHING PATTERN "*.hh" )
            install( DIRECTORY ./  DESTINATION ${_h_destination} FILES_MATCHING PATTERN "*.hpp" )
            install( DIRECTORY ./  DESTINATION ${_h_destination} FILES_MATCHING PATTERN "*.H" )
            install( DIRECTORY ./  DESTINATION ${_h_destination} FILES_MATCHING PATTERN "*.tcc" )
            install( DIRECTORY ./  DESTINATION ${_h_destination} FILES_MATCHING PATTERN "*.txx" )
            install( DIRECTORY ./  DESTINATION ${_h_destination} FILES_MATCHING PATTERN "*.tcc" )
          endif()
        endif()

        if( DEFINED _PAR_INSTALL_HEADERS_LIST )
          install( FILES ${_PAR_INSTALL_HEADERS_LIST} DESTINATION ${_h_destination} )
        endif()

        if( DEFINED _PAR_INSTALL_HEADERS_REGEX )
          install( DIRECTORY ./  DESTINATION ${_h_destination} FILES_MATCHING PATTERN "${_PAR_INSTALL_HEADERS_REGEX}")
        endif()

        target_include_directories(${_PAR_TARGET} ${_PUBLIC_INTF} $<INSTALL_INTERFACE:${INSTALL_INCLUDE_DIR}>)
        if( _PAR_HEADER_DESTINATION )
          target_include_directories(${_PAR_TARGET} ${_PUBLIC_INTF} $<INSTALL_INTERFACE:${_PAR_HEADER_DESTINATION}> )
        endif()
      endif()

      # set build location

      if( NOT _PAR_TYPE MATCHES "INTERFACE" )
        set_target_properties( ${_PAR_TARGET} PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib )
        set_target_properties( ${_PAR_TARGET} PROPERTIES ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib )
        if( EC_OS_NAME MATCHES "windows" )
          set_target_properties( ${_PAR_TARGET} PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
        endif()
      endif()

      # export location of target to other projects -- must be exactly after setting the build location (see previous 3 commands)

      export( TARGETS ${_PAR_TARGET} APPEND FILE "${PROJECT_TARGETS_FILE}" )

    endif()

    # add definitions to compilation
    if( DEFINED _PAR_DEFINITIONS )
      if( _PAR_TYPE MATCHES "INTERFACE" )
        target_compile_definitions(${_PAR_TARGET} PUBLIC ${_PAR_DEFINITIONS})
      else()
        target_compile_definitions(${_PAR_TARGET} PRIVATE ${_PAR_DEFINITIONS})
      endif()
      ecbuild_debug("ecbuild_add_library(${_PAR_TARGET}): adding definitions ${_PAR_DEFINITIONS}")
    endif()

    # make sure target is removed before - some problems with AIX
    if( NOT _PAR_TYPE MATCHES "OBJECT" AND NOT _PAR_TYPE MATCHES "INTERFACE" )
      add_custom_command( TARGET ${_PAR_TARGET} PRE_BUILD COMMAND ${CMAKE_COMMAND} -E remove $<TARGET_FILE:${_PAR_TARGET}> )
    endif()

    # append to the list of this project targets
    if( NOT ECBUILD_2_COMPAT OR NOT _PAR_TYPE MATCHES "INTERFACE" )
      set( ${PROJECT_NAME}_ALL_LIBS ${${PROJECT_NAME}_ALL_LIBS} ${_PAR_TARGET} CACHE INTERNAL "" )
    endif()

  endif()

  # mark source files as used
  ecbuild_declare_project_files( ${_PAR_SOURCES} )
  if( DEFINED _PAR_TEMPLATES )
    ecbuild_declare_project_files( ${_PAR_TEMPLATES} )
  endif()

endfunction( ecbuild_add_library_impl  )

##############################################################################
# auxiliary macro for adding a library
##############################################################################

function( ecbuild_add_library )

  set( options  )
  set( single_value_args TARGET TYPE )
  set( multi_value_args )

  cmake_parse_arguments( _p "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

  if( DEFINED _p_TYPE ) # don't do anything if TYPE was specified

    if( _p_TYPE MATCHES "[Bb][Oo][Tt][Hh]" ) # build both types

      ecbuild_add_library_impl( TARGET ${_p_TARGET}        TYPE SHARED ${_p_UNPARSED_ARGUMENTS} )
      ecbuild_add_library_impl( TARGET ${_p_TARGET}-static TYPE STATIC ${_p_UNPARSED_ARGUMENTS} OUTPUT_NAME ${_p_TARGET} DEPENDS ${_p_TARGET} )

    else()

      ecbuild_add_library_impl( ${ARGV} )

    endif()

  else()

    if( NOT DEFINED _p_TARGET )
      ecbuild_critical("The call to ecbuild_add_library() doesn't specify the TARGET.")
    else()

      if( BUILD_SHARED_LIBS MATCHES "[Bb][Oo][Tt][Hh]" ) # build both types

        ecbuild_add_library_impl( TARGET ${_p_TARGET}        TYPE SHARED ${_p_UNPARSED_ARGUMENTS} )
        ecbuild_add_library_impl( TARGET ${_p_TARGET}-static TYPE STATIC ${_p_UNPARSED_ARGUMENTS} DEPENDS ${_p_TARGET} )

        # If the library is built conditionally the target might not exist
        if ( TARGET ${_p_TARGET}-static )
          set_target_properties( ${_p_TARGET}-static PROPERTIES OUTPUT_NAME ${_p_TARGET} )
        endif()

      else()

        ecbuild_add_library_impl( ${ARGV} )

      endif()

    endif()

  endif()

endfunction()
