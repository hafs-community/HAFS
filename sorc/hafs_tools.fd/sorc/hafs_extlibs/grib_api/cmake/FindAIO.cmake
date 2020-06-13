# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# OUTPUT:
# RT_LIB  = the library to link against

if( CMAKE_SYSTEM_NAME MATCHES "Linux" )

	find_package( Realtime )

	if( REALTIME_FOUND ) # check that aio needs realtime
		set( AIO_LIBRARIES ${RT_LIB} )
	endif()

endif()

find_path( AIO_INCLUDE_DIRS NAMES aio.h HINTS ENV AIO_PATH ${AIO_PATH} )

mark_as_advanced( AIO_INCLUDE_DIRS )

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args( AIO  DEFAULT_MSG  AIO_INCLUDE_DIRS  )

# # checks for AIO64 vs AIO
# if( AIO_FOUND )
#
# 	include( CheckCSourceCompiles )
# 	include( CMakePushCheckState )
#
#     cmake_push_check_state()
#
# 		set( CMAKE_REQUIRED_INCLUDES ${AIO_INCLUDE_DIRS} )
#
# 		if( AIO_LIBRARIES )
# 			set( CMAKE_REQUIRED_LIBRARIES ${AIO_LIBRARIES} )
# 		endif()
#
# 		if( NOT DEFINED EC_HAVE_AIOCB )
# 			ecbuild_cache_check_c_source_compiles( "#include <aio.h>
# 									  #include <fcntl.h>
# 									  int main(){
# 										  struct aiocb* aiocbp;
# 										  int n = aio_write(aiocbp);
# 										  n = aio_read(aiocbp);
# 										  n = aio_fsync(O_SYNC,aiocbp);
# 										  return 0; }"
# 									EC_HAVE_AIOCB )
# 		endif()
#
# 		if( NOT DEFINED EC_HAVE_AIOCB64 )
# 			check_c_source_compiles( "#include <aio.h>
# 									  #include <fcntl.h>
# 									  int main(){
# 										  struct aiocb64* aiocbp;
# 										  int n = aio_write64(aiocbp);
# 										  n = aio_read64(aiocbp);
# 										  n = aio_fsync64(O_SYNC,aiocbp);
# 										  return 0; }"
# 									EC_HAVE_AIOCB64 )
# 		endif()
#
#     cmake_pop_check_state()
#
# endif()
