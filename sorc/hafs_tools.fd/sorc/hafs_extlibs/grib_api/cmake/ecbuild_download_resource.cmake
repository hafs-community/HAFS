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
# ecbuild_download_resource
# =========================
#
# Download a file from a given URL and save to FILE at configure time. ::
#
#   ecbuild_download_resource( FILE URL )
#
# curl or wget is required (curl is preferred if available).
#
# The default timeout is 30 seconds, which can be overridden with
# ``ECBUILD_DOWNLOAD_TIMEOUT``. Downloads are by default only tried once, use
# ``ECBUILD_DOWNLOAD_RETRIES`` to set the number of retries.
#
##############################################################################

function( ecbuild_download_resource _p_OUT _p_URL )

  # Do not retry downloads by default (ECBUILD-307)
  if( NOT DEFINED ECBUILD_DOWNLOAD_RETRIES )
    set( ECBUILD_DOWNLOAD_RETRIES 0 )
  endif()
  # Use default timeout of 30s if not specified (ECBUILD-307)
  if( NOT DEFINED ECBUILD_DOWNLOAD_TIMEOUT )
    set( ECBUILD_DOWNLOAD_TIMEOUT 30 )
  endif()

  if( NOT EXISTS ${_p_OUT} )

    find_program( CURL_PROGRAM curl )
    mark_as_advanced(CURL_PROGRAM)
    if( CURL_PROGRAM )

      execute_process( COMMAND ${CURL_PROGRAM} --silent --show-error --fail
                               --retry ${ECBUILD_DOWNLOAD_RETRIES}
                               --connect-timeout ${ECBUILD_DOWNLOAD_TIMEOUT}
                               --output ${_p_OUT} ${_p_URL}
                       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} RESULT_VARIABLE CMD_RESULT )

    else()

      find_program( WGET_PROGRAM wget )

      if( WGET_PROGRAM )

        # wget takes the total number of tries, curl the number or retries
        math( EXPR ECBUILD_DOWNLOAD_RETRIES ${ECBUILD_DOWNLOAD_RETRIES} + 1 )

        execute_process( COMMAND ${WGET_PROGRAM} -nv -O ${_p_OUT}
                                 -t ${ECBUILD_DOWNLOAD_RETRIES}
                                 -T ${ECBUILD_DOWNLOAD_TIMEOUT} ${_p_URL}
                         WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} RESULT_VARIABLE CMD_RESULT )

      else()
        ecbuild_critical("Could not find curl or wget. Error downloading ${_p_URL}")
      endif()

    endif()

    if(CMD_RESULT)
      ecbuild_critical("Error downloading ${_p_URL}")
    endif()

  endif()

endfunction()
