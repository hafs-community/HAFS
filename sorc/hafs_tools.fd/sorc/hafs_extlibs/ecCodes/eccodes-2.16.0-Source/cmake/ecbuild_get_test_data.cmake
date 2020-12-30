# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

##############################################################################

# function for downloading test data

function( _download_test_data _p_NAME _p_DIR_URL )

  # TODO: make that 'at ecmwf'
  #if(1)
  #unset(ENV{no_proxy})
  #unset(ENV{NO_PROXY})
  #set(ENV{http_proxy} "http://proxy.ecmwf.int:3333")
  #endif()

  # Do not retry downloads by default (ECBUILD-307)
  if( NOT DEFINED ECBUILD_DOWNLOAD_RETRIES )
    set( ECBUILD_DOWNLOAD_RETRIES 0 )
  endif()
  # Use default timeout of 30s if not specified (ECBUILD-307)
  if( NOT DEFINED ECBUILD_DOWNLOAD_TIMEOUT )
    set( ECBUILD_DOWNLOAD_TIMEOUT 30 )
  endif()

  find_program( CURL_PROGRAM curl )
  mark_as_advanced(CURL_PROGRAM)

  if( CURL_PROGRAM )

    add_custom_command( OUTPUT ${_p_NAME}
      COMMENT "(curl) downloading ${_p_DIR_URL}/${_p_NAME}"
      COMMAND ${CURL_PROGRAM} --silent --show-error --fail --output ${_p_NAME}
              --retry ${ECBUILD_DOWNLOAD_RETRIES}
              --connect-timeout ${ECBUILD_DOWNLOAD_TIMEOUT}
              ${_p_DIR_URL}/${_p_NAME} )

  else()

    find_program( WGET_PROGRAM wget )

    if( WGET_PROGRAM )

      # wget takes the total number of tries, curl the number or retries
      math( EXPR ECBUILD_DOWNLOAD_RETRIES "${ECBUILD_DOWNLOAD_RETRIES} + 1" )

      add_custom_command( OUTPUT ${_p_NAME}
        COMMENT "(wget) downloading ${_p_DIR_URL}/${_p_NAME}"
        COMMAND ${WGET_PROGRAM} -nv -O ${_p_NAME}
                -t ${ECBUILD_DOWNLOAD_RETRIES} -T ${ECBUILD_DOWNLOAD_TIMEOUT}
                ${_p_DIR_URL}/${_p_NAME} )

    else()

      if( WARNING_CANNOT_DOWNLOAD_TEST_DATA )
        ecbuild_warn( "Couldn't find curl neither wget -- cannot download test data from server.\nPlease obtain the test data by other means and pleace it in the build directory." )
        set( WARNING_CANNOT_DOWNLOAD_TEST_DATA 1 CACHE INTERNAL "Couldn't find curl neither wget -- cannot download test data from server" )
        mark_as_advanced( WARNING_CANNOT_DOWNLOAD_TEST_DATA )
      endif()

    endif()

  endif()

endfunction()

##############################################################################
#.rst:
#
# ecbuild_get_test_data
# =====================
#
# Download a test data set at build time. ::
#
#   ecbuild_get_test_data( NAME <name>
#                          [ TARGET <target> ]
#                          [ DIRNAME <dir> ]
#                          [ MD5 <hash> ]
#                          [ EXTRACT ]
#                          [ NOCHECK ] )
#
# curl or wget is required (curl is preferred if available).
#
# Options
# -------
#
# NAME : required
#   name of the test data file
#
# TARGET : optional, defaults to test_data_<name>
#   CMake target name
#
# DIRNAME : optional, defaults to <project>/<relative path to current dir>
#   directory in which the test data resides
#
# MD5 : optional, ignored if NOCHECK is given
#   md5 checksum of the data set to verify. If not given and NOCHECK is *not*
#   set, download the md5 checksum and verify
#
# EXTRACT : optional
#   extract the downloaded file (supported archives: tar, zip, tar.gz, tar.bz2)
#
# NOCHECK : optional
#   do not verify the md5 checksum of the data file
#
# Usage
# -----
#
# Download test data from ``<ECBUILD_DOWNLOAD_BASE_URL>/<DIRNAME>/<NAME>``
#
# If the ``ECBUILD_DOWNLOAD_BASE_URL`` variable is not set, the default URL
# ``http://download.ecmwf.org/test-data`` is used.
#
# If the ``DIRNAME`` argument is not given, the project name followed by the
# relative path from the root directory to the current directory is used.
#
# By default, the downloaded file is verified against an md5 checksum, either
# given as the ``MD5`` argument or downloaded from the server otherwise. Use
# the argument ``NOCHECK`` to disable this check.
#
# The default timeout is 30 seconds, which can be overridden with
# ``ECBUILD_DOWNLOAD_TIMEOUT``. Downloads are by default only tried once, use
# ``ECBUILD_DOWNLOAD_RETRIES`` to set the number of retries.
#
# Examples
# --------
#
# Do not verify the checksum: ::
#
#   ecbuild_get_test_data( NAME msl.grib NOCHECK )
#
# Checksum agains remote md5 file: ::
#
#   ecbuild_get_test_data( NAME msl.grib )
#
# Checksum agains local md5: ::
#
#   ecbuild_get_test_data( NAME msl.grib MD5 f69ca0929d1122c7878d19f32401abe9 )
#
##############################################################################

function( ecbuild_get_test_data )

    set( options NOCHECK EXTRACT )
    set( single_value_args TARGET URL NAME DIRNAME MD5 SHA1)
    set( multi_value_args  )

    cmake_parse_arguments( _p "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

    if(_p_UNPARSED_ARGUMENTS)
      ecbuild_critical("Unknown keywords given to ecbuild_get_test_data(): \"${_p_UNPARSED_ARGUMENTS}\"")
    endif()

    file( RELATIVE_PATH currdir ${PROJECT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR} )

    ### check parameters

    if( NOT _p_NAME )
      ecbuild_critical("ecbuild_get_test_data() expects a NAME")
    endif()

    if( NOT _p_TARGET )
      string( REGEX REPLACE "[^A-Za-z0-9_]" "_" _p_TARGET "test_data_${_p_NAME}")
#      string( REGEX REPLACE "[^A-Za-z0-9_]" "_" _p_TARGET "${_p_NAME}")
#      set( _p_TARGET ${_p_NAME} )
    endif()

    if( NOT _p_DIRNAME )
      set( _p_DIRNAME ${PROJECT_NAME}/${currdir} )
    endif()

#    ecbuild_debug_var( _p_TARGET )
#    ecbuild_debug_var( _p_NAME )
#    ecbuild_debug_var( _p_URL )
#    ecbuild_debug_var( _p_DIRNAME )

    # Allow the user to override the download URL (ECBUILD-447)
    if( NOT DEFINED ECBUILD_DOWNLOAD_BASE_URL )
      set( ECBUILD_DOWNLOAD_BASE_URL http://download.ecmwf.org/test-data )
    endif()

    # download the data

    _download_test_data( ${_p_NAME} ${ECBUILD_DOWNLOAD_BASE_URL}/${_p_DIRNAME} )

    # perform the checksum if requested

    set( _deps ${_p_NAME} )

    if( NOT _p_NOCHECK )

        if( NOT _p_MD5 AND NOT _p_SHA1) # use remote md5

            add_custom_command( OUTPUT ${_p_NAME}.localmd5
                                COMMAND ${CMAKE_COMMAND} -E md5sum ${_p_NAME} > ${_p_NAME}.localmd5
                                DEPENDS ${_p_NAME} )

            _download_test_data( ${_p_NAME}.md5 ${ECBUILD_DOWNLOAD_BASE_URL}/${_p_DIRNAME} )

            add_custom_command( OUTPUT ${_p_NAME}.ok
                                COMMAND ${CMAKE_COMMAND} -E compare_files ${_p_NAME}.md5 ${_p_NAME}.localmd5 &&
                                        ${CMAKE_COMMAND} -E touch ${_p_NAME}.ok
                                DEPENDS ${_p_NAME}.localmd5 ${_p_NAME}.md5 )

            list( APPEND _deps  ${_p_NAME}.localmd5 ${_p_NAME}.ok )

        endif()

        if( _p_MD5 )

            add_custom_command( OUTPUT ${_p_NAME}.localmd5
                                COMMAND ${CMAKE_COMMAND} -E md5sum ${_p_NAME} > ${_p_NAME}.localmd5
                                DEPENDS ${_p_NAME} )

            configure_file( "${ECBUILD_MACROS_DIR}/md5.in" ${_p_NAME}.md5 @ONLY )

            add_custom_command( OUTPUT ${_p_NAME}.ok
                                COMMAND ${CMAKE_COMMAND} -E compare_files ${_p_NAME}.md5 ${_p_NAME}.localmd5 &&
                                        ${CMAKE_COMMAND} -E touch ${_p_NAME}.ok
                                DEPENDS ${_p_NAME}.localmd5 )

            list( APPEND _deps ${_p_NAME}.localmd5 ${_p_NAME}.ok )

        endif()

#        if( _p_SHA1 )

#            find_program( SHASUM NAMES sha1sum shasum )
#            if( SHASUM )
#                add_custom_command( OUTPUT ${_p_NAME}.localsha1
#                                    COMMAND ${SHASUM} ${_p_NAME} > ${_p_NAME}.localsha1 )

#                add_custom_command( OUTPUT ${_p_NAME}.ok
#                                    COMMAND diff ${_p_NAME}.sha1 ${_p_NAME}.localsha1 && touch ${_p_NAME}.ok )

#                configure_file( "${ECBUILD_MACROS_DIR}/sha1.in" ${_p_NAME}.sha1 @ONLY )

#                list( APPEND _deps ${_p_NAME}.localsha1 ${_p_NAME}.ok )
#            endif()

#        endif()

    endif()

    add_custom_target( ${_p_TARGET} DEPENDS ${_deps} )

    if( _p_EXTRACT )
      ecbuild_debug("ecbuild_get_test_data: extracting ${_p_NAME} (post-build for target ${_p_TARGET}")
      add_custom_command( TARGET ${_p_TARGET} POST_BUILD
                          COMMAND ${CMAKE_COMMAND} -E tar xv ${_p_NAME} )
    endif()

endfunction(ecbuild_get_test_data)

##############################################################################
#.rst:
#
# ecbuild_get_test_multidata
# ==========================
#
# Download multiple test data sets at build time. ::
#
#   ecbuild_get_test_multidata( NAMES <name1> [ <name2> ... ]
#                               TARGET <target>
#                               [ DIRNAME <dir> ]
#                               [ LABELS <label1> [<label2> ...] ]
#                               [ EXTRACT ]
#                               [ NOCHECK ] )
#
# curl or wget is required (curl is preferred if available).
#
# Options
# -------
#
# NAMES : required
#   list of names of the test data files
#
# TARGET : optional
#   CMake target name
#
# DIRNAME : optional, defaults to <project>/<relative path to current dir>
#   directory in which the test data resides
#
# LABELS : optional
#   list of labels to assign to the test
#
#   Lower case project name and ``download_data`` are always added as labels.
#
#   This allows selecting tests to run via ``ctest -L <regex>`` or tests
#   to exclude via ``ctest -LE <regex>``.
#
# EXTRACT : optional
#   extract downloaded files (supported archives: tar, zip, tar.gz, tar.bz2)
#
# NOCHECK : optional
#   do not verify the md5 checksum of the data file
#
# Usage
# -----
#
# Download test data from ``<ECBUILD_DOWNLOAD_BASE_URL>/<DIRNAME>``
# for each name given in the list of ``NAMES``. Each name may contain a
# relative path, which is appended to ``DIRNAME`` and may be followed by an
# md5 checksum, separated with a ``:`` (the name must not contain spaces).
#
# If the ``ECBUILD_DOWNLOAD_BASE_URL`` variable is not set, the default URL
# ``http://download.ecmwf.org/test-data`` is used.
#
# If the ``DIRNAME`` argument is not given, the project name followed by the
# relative path from the root directory to the current directory is used.
#
# By default, each downloaded file is verified against an md5 checksum, either
# given as part of the name as described above or a remote checksum downloaded
# from the server. Use the argument ``NOCHECK`` to disable this check.
#
# Examples
# --------
#
# Do not verify checksums: ::
#
#   ecbuild_get_test_multidata( TARGET get_grib_data NAMES foo.grib bar.grib
#                               DIRNAME test/data/dir NOCHECK )
#
# Checksums agains remote md5 file: ::
#
#   ecbuild_get_test_multidata( TARGET get_grib_data NAMES foo.grib bar.grib
#                               DIRNAME test/data/dir )
#
# Checksum agains local md5: ::
#
#   ecbuild_get_test_multidata( TARGET get_grib_data DIRNAME test/data/dir
#                               NAMES msl.grib:f69ca0929d1122c7878d19f32401abe9 )
#
##############################################################################

function( ecbuild_get_test_multidata )

    set( options EXTRACT NOCHECK )
    set( single_value_args TARGET DIRNAME )
    set( multi_value_args  NAMES LABELS )

    cmake_parse_arguments( _p "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

    if(_p_UNPARSED_ARGUMENTS)
      ecbuild_critical("Unknown keywords given to ecbuild_get_test_data(): \"${_p_UNPARSED_ARGUMENTS}\"")
    endif()

    ### check parameters

    if( NOT _p_NAMES )
      ecbuild_critical("ecbuild_get_test_data() expects a NAMES")
    endif()

    if( NOT _p_TARGET )
      ecbuild_critical("ecbuild_get_test_data() expects a TARGET")
    endif()

#    ecbuild_debug_var( _p_TARGET )
#    ecbuild_debug_var( _p_NAME )
#    ecbuild_debug_var( _p_DIRNAME )

    if( _p_EXTRACT )
        set( _extract EXTRACT )
    endif()

    if( _p_NOCHECK )
        set( _nocheck NOCHECK )
    endif()

    ### prepare file

    set( _script ${CMAKE_CURRENT_BINARY_DIR}/get_data_${_p_TARGET}.cmake )

    file( WRITE ${_script} "
function(EXEC_CHECK)
     execute_process(COMMAND \${ARGV} RESULT_VARIABLE CMD_RESULT)
     if(CMD_RESULT)
           message(FATAL_ERROR \"Error running ${CMD}\")
     endif()
endfunction()\n\n" )

    foreach( _d ${_p_NAMES} )

        string( REGEX MATCH "[^:]+" _f "${_d}" )

        get_filename_component( _file ${_f} NAME )
        get_filename_component( _dir  ${_f} PATH )

        set( _path_comps "" )
        list( APPEND _path_comps ${_p_DIRNAME} ${_dir} )
        join( _path_comps "/" _dirname )
        if( _dirname )
            set( _dirname DIRNAME ${_dirname} )
        endif()
        unset( _path_comps )

        string( REPLACE "." "_" _name "${_file}" )
        string( REGEX MATCH ":.*"  _md5  "${_d}" )
        string( REPLACE ":" "" _md5 "${_md5}" )

        if( _md5 )
            set( _md5 MD5 ${_md5} )
        endif()

        #ecbuild_debug_var(_f)
        #ecbuild_debug_var(_file)
        #ecbuild_debug_var(_dirname)
        #ecbuild_debug_var(_name)
        #ecbuild_debug_var(_md5)

        ecbuild_get_test_data(
            TARGET __get_data_${_p_TARGET}_${_name}
            NAME ${_file} ${_dirname} ${_md5} ${_extract} ${_nocheck} )

        if ( ${CMAKE_GENERATOR} MATCHES Ninja )
          set( _fast "" )
        else()
          # The option /fast disables dependency checking on a target, see
          # https://cmake.org/Wiki/CMake_FAQ#Is_there_a_way_to_skip_checking_of_dependent_libraries_when_compiling.3F
          set( _fast "/fast" )
        endif()
        file( APPEND ${_script}
              "exec_check( \"${CMAKE_COMMAND}\" --build \"${CMAKE_BINARY_DIR}\" --target __get_data_${_p_TARGET}_${_name}${_fast} )\n" )

    endforeach()

    if( HAVE_TESTS )
      add_test(  NAME ${_p_TARGET} COMMAND ${CMAKE_COMMAND} -P ${_script} )
      set( _p_LABELS ${PROJECT_NAME_LOWCASE} download_data ${_p_LABELS} )
      list( REMOVE_DUPLICATES _p_LABELS )
      set_property( TEST ${_p_TARGET} APPEND PROPERTY LABELS "${_p_LABELS}" )
    endif()

endfunction(ecbuild_get_test_multidata)
