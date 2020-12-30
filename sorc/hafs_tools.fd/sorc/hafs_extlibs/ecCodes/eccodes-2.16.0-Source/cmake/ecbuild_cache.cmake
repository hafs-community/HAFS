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
# ecBuild Cache
# =============
#
# During initialisation, ecBuild introspects the compiler and operating system
# and performs a number of checks. The result of these is written to a
# dedicated ``ecbuild-cache.cmake`` file in the build tree. This cache may be
# used to speed up subsequent *clean* builds i.e. those where no CMakeCache.txt
# exists yet.
#
# To use the ecBuild cache, configure with ``-DECBUILD_CACHE=<cache-file>``,
# where ``<cache-file>`` is the path to an existing ``ecbuild-cache.cmake``.
#
# .. note ::
#
#   The ecBuild cache is specific to compiler *and* operating system. Do *not*
#   attempt to use a cache file created on a different machine or with a
#   different compiler!
#
##############################################################################

# Prepare the cache and clobber any existing ecbuild-cache.cmake
macro( ecbuild_prepare_cache )
    include( CheckSymbolExists )
    include( CheckIncludeFiles )
    include( CheckCSourceCompiles )
    include( CheckCXXSourceCompiles )
    include( CheckTypeSize )
    set( ecbuild_cache_file ${CMAKE_BINARY_DIR}/ecbuild-cache.cmake )
    file(WRITE ${ecbuild_cache_file} "# ecbuild cache file\n\n")
endmacro()

# Buffer the CMake variable var to be written to the ecBuild cache
function( ecbuild_cache_var var )
  if( NOT ${var} )
    set( ${var} 0 )
  endif()
  set( ECBUILD_CACHE_BUFFER "${ECBUILD_CACHE_BUFFER}set( ${var} ${${var}} )\n" CACHE INTERNAL "Cache buffer" )
endfunction()

# Call check_symbol_exists only if the output is not defined yet
function( ecbuild_cache_check_symbol_exists symbol includes output )
  if( NOT DEFINED ${output} )
    check_symbol_exists( ${symbol} ${includes} ${output} )
  endif()
  ecbuild_cache_var( ${output} )
endfunction()

# Call check_include_files only if the output is not defined yet
function( ecbuild_cache_check_include_files includes output )
  if( NOT DEFINED ${output} )
    check_include_files( ${includes} ${output} )
  endif()
  ecbuild_cache_var( ${output} )
endfunction()

# Call check_c_source_compiles only if the output is not defined yet
function( ecbuild_cache_check_c_source_compiles source output )
  if( NOT DEFINED ${output} )
    check_c_source_compiles( "${source}" ${output} )
  endif()
  ecbuild_cache_var( ${output} )
endfunction()

# Call check_cxx_source_compiles only if the output is not defined yet
function( ecbuild_cache_check_cxx_source_compiles source output )
  if( NOT DEFINED ${output} )
    check_cxx_source_compiles( "${source}" ${output} )
  endif()
  ecbuild_cache_var( ${output} )
endfunction()

# Call check_type_size only if the output is not defined yet
function( ecbuild_cache_check_type_size type output )
  if( NOT DEFINED ${output} )
    check_type_size( "${type}" ${output} )
  endif()
  ecbuild_cache_var( ${output} )
endfunction()

# Flush the ecBuild cache to disk and reset the buffer
function( ecbuild_flush_cache )
  file( APPEND ${ecbuild_cache_file} "${ECBUILD_CACHE_BUFFER}" )
  set( ECBUILD_CACHE_BUFFER "" CACHE INTERNAL "Cache buffer" )
endfunction()
