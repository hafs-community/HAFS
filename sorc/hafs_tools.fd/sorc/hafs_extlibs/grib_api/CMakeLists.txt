############################################################################################
# cmake options:
#
#       -DCMAKE_BUILD_TYPE=Debug|RelWithDebInfo|Release|Production
#       -DCMAKE_INSTALL_PREFIX=/path/to/install
#
#       -DCMAKE_MODULE_PATH=/path/to/ecbuild/cmake

cmake_minimum_required( VERSION 2.8.11 FATAL_ERROR )

project( grib_api C )

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild/cmake")

include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_requires_macro_version( 1.9 )

###############################################################################
# local project

ecbuild_declare_project()

###############################################################################
# system checks needed for grib_api_config.h

check_type_size( int            GRIB_API_SIZEOF_INT    )
check_type_size( long           GRIB_API_SIZEOF_LONG   )
check_type_size( size_t         GRIB_API_SIZEOF_SIZE_T )

check_include_files( assert.h         GRIB_API_HAVE_ASSERT_H      )
check_include_files( string.h         GRIB_API_HAVE_STRING_H      )
check_include_files( sys/types.h      GRIB_API_HAVE_SYS_TYPES_H   )
check_include_files( sys/stat.h       GRIB_API_HAVE_SYS_STAT_H    )
check_include_files( fcntl.h          GRIB_API_HAVE_FCNTL_H      )
check_include_files( unistd.h         GRIB_API_HAVE_UNISTD_H      )

check_symbol_exists( fseeko           "stdio.h"    GRIB_API_HAVE_FSEEKO )
check_symbol_exists( posix_memalign   "stdlib.h"   GRIB_API_HAVE_POSIX_MEMALIGN )
check_symbol_exists( fmemopen         "stdio.h"    GRIB_API_HAVE_FMEMOPEN )
check_symbol_exists( funopen          "stdio.h"    GRIB_API_HAVE_FUNOPEN )

check_c_source_compiles(
      " typedef int foo_t;
      static inline foo_t static_foo(){return 0;}
      foo_t foo(){return 0;}
      int main(int argc, char *argv[]){return 0;}
      " GRIB_API_HAVE_C_INLINE )

include(grib_api_test_endiness)

ecbuild_debug("GRIB_API_BIG_ENDIAN=${GRIB_API_BIG_ENDIAN}")
ecbuild_debug("GRIB_API_LITTLE_ENDIAN=${GRIB_API_LITTLE_ENDIAN}")

###############################################################################
# some variables/options of this project

ecbuild_add_option( FEATURE EXAMPLES
    DESCRIPTION "Build the examples"
    DEFAULT ON
)

ecbuild_add_option( FEATURE JPG
    DESCRIPTION "support for JPG decoding/encoding"
    DEFAULT ON
)

ecbuild_add_option( FEATURE PNG
    DESCRIPTION "support for PNG decoding/encoding"
    DEFAULT OFF
    REQUIRED_PACKAGES PNG
)

if( HAVE_PNG )
    set( HAVE_LIBPNG 1 ) # compatibility with autotools
    add_definitions( ${PNG_DEFINITIONS} )
else()
    set( HAVE_LIBPNG 0 )
endif()

ecbuild_add_option( FEATURE NETCDF
    DESCRIPTION "support for GRIB to NetCDF conversion"
    DEFAULT ON
    REQUIRED_PACKAGES NetCDF
)

ecbuild_add_option( FEATURE AEC
    DESCRIPTION "support for Adaptive Entropy Coding"
    DEFAULT OFF
    REQUIRED_PACKAGES AEC
)

ecbuild_add_option( FEATURE PYTHON
    DESCRIPTION "build the GRIB_API Python interface"
    DEFAULT ON
    REQUIRED_PACKAGES "Python VERSION 2.5"
)

ecbuild_add_option( FEATURE FORTRAN
    DESCRIPTION "build the GRIB_API Fortran interface"
    DEFAULT ON
    # REQUIRED_LANGUAGES Fortran # TODO
)

# TODO Remove this after REQUIRED_LANGUAGES
if( ENABLE_FORTRAN )
    # will set EC_HAVE_FORTRAN with the result
    ecbuild_enable_fortran( MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/fortran/modules )
    set( HAVE_FORTRAN ${EC_HAVE_FORTRAN} )
else()
    set( HAVE_FORTRAN 0 )
endif()

# advanced options (not visible in cmake-gui )

ecbuild_add_option( FEATURE MEMORY_MANAGEMENT DESCRIPTION "enable memory management" DEFAULT OFF ADVANCED )
ecbuild_add_option( FEATURE ALIGN_MEMORY      DESCRIPTION "enable memory alignment"  DEFAULT OFF ADVANCED )
ecbuild_add_option( FEATURE GRIB_TIMER       DESCRIPTION "enable timer"         DEFAULT OFF ADVANCED )
ecbuild_add_option( FEATURE GRIB_THREADS     DESCRIPTION "enable POSIX threads" DEFAULT OFF ADVANCED )
ecbuild_add_option( FEATURE GRIB_OMP_THREADS DESCRIPTION "enable OMP threads"   DEFAULT OFF ADVANCED )

###############################################################################
# macro processing

set( GRIB_API_EXTRA_LIBRARIES    "" )
set( GRIB_API_EXTRA_INCLUDE_DIRS "" )
set( GRIB_API_EXTRA_DEFINITIONS  "" )

find_package( CMath )
list( APPEND GRIB_API_TPLS CMath )

### JPG support

set( HAVE_JPEG 0 )
set( HAVE_LIBJASPER 0 )
set( HAVE_LIBOPENJPEG 0 )

if( ENABLE_JPG )

    # Note: This is a deprecated feature but we need it to find Jasper at ECMWF.
    #       ecbuild_add_extra_search_paths modifies CMAKE_PREFIX_PATH
    #       which can affect future package discovery if not undone by the caller.
    #       The current CMAKE_PREFIX_PATH is backed up as _CMAKE_PREFIX_PATH
    #
    set(CMAKE_WARN_DEPRECATED OFF) # Suppress deprecation message
    ecbuild_add_extra_search_paths( jasper )
    find_package( Jasper )
    set(CMAKE_PREFIX_PATH ${_CMAKE_PREFIX_PATH})    # Restore CMAKE_PREFIX_PATH
    set(CMAKE_WARN_DEPRECATED ON)  # Remove suppression

    find_package( OpenJPEG )

    if( JASPER_FOUND )
        list( APPEND GRIB_API_TPLS Jasper )
        set( HAVE_JPEG 1 )
        set( HAVE_LIBJASPER 1 )
    endif()

    if( OPENJPEG_FOUND )
        list( APPEND GRIB_API_TPLS OpenJPEG )
        set( HAVE_JPEG 1 )
        set( HAVE_LIBOPENJPEG 1 )
    endif()

endif()


###############################################################################
# other options

if( GRIB_TIMER )
    set( GRIB_TIMER 1 )
else()
    set( GRIB_TIMER 0 )
endif()

set( IS_BIG_ENDIAN 0 )
if( GRIB_API_BIG_ENDIAN )
    set( IS_BIG_ENDIAN 1 )
endif()

set( MANAGE_MEM 0 )
if( ENABLE_MEMORY_MANAGEMENT )
    set( MANAGE_MEM 1 )
endif()

set( CMAKE_THREAD_PREFER_PTHREAD 1 ) # find thread library, but prefer pthreads
find_package(Threads REQUIRED)

# debug
ecbuild_info(" CMAKE_THREAD_LIBS_INIT=${CMAKE_THREAD_LIBS_INIT}")
ecbuild_info(" CMAKE_USE_PTHREADS_INIT=${CMAKE_USE_PTHREADS_INIT}")
ecbuild_info(" GRIB_THREADS=${GRIB_THREADS}, HAVE_GRIB_THREADS=${HAVE_GRIB_THREADS}")


set( GRIB_PTHREADS 0 )
set( GRIB_OMP_THREADS 0 )
set( GRIB_LINUX_PTHREADS 0 )
#if( HAVE_GRIB_THREADS AND CMAKE_THREAD_LIBS_INIT )
if( HAVE_GRIB_THREADS )
    if( NOT ${CMAKE_USE_PTHREADS_INIT} )
        ecbuild_critical("Only pthreads supported - thread library found is [${CMAKE_THREAD_LIBS_INIT}]" )
    endif()
    set( GRIB_PTHREADS 1 )
    if( ${CMAKE_SYSTEM_NAME} MATCHES "Linux" )
        set( GRIB_LINUX_PTHREADS 1 )
    endif()
elseif(HAVE_GRIB_OMP_THREADS)
    ecbuild_enable_omp()
    set( GRIB_OMP_THREADS 1 )
endif()

# Cannot have both options
if( HAVE_GRIB_THREADS AND HAVE_GRIB_OMP_THREADS )
    ecbuild_critical( "Cannot enable both POSIX threads and OMP! Please specify just one option" )
endif()

ecbuild_info(" GRIB_PTHREADS=${GRIB_PTHREADS}")
ecbuild_info(" GRIB_OMP_THREADS=${GRIB_OMP_THREADS}")

set( GRIB_MEM_ALIGN 0 )
if( ENABLE_ALIGN_MEMORY )
  set( GRIB_MEM_ALIGN 1 )
endif()

# fix for #if IEEE_LE or IEE_BE instead of #ifdef

if( IEEE_BE )
    set( IEEE_LE 0 )
endif()

if( IEEE_LE )
    set( IEEE_BE 0 )
endif()

###############################################################################
# contents

if( NOT ${DEVELOPER_MODE} )
    set( the_default_data_prefix ${CMAKE_INSTALL_PREFIX} )
else()
    set( the_default_data_prefix ${CMAKE_BINARY_DIR} )
endif()

if( NOT DEFINED GRIB_API_DEFINITION_SUFF )
  set( GRIB_API_DEFINITION_SUFF  share/grib_api/definitions )
endif()
if( NOT DEFINED GRIB_API_SAMPLES_SUFF )
  set( GRIB_API_SAMPLES_SUFF     share/grib_api/samples )
endif()
if( NOT DEFINED GRIB_API_IFS_SAMPLES_SUFF )
  set( GRIB_API_IFS_SAMPLES_SUFF share/grib_api/ifs_samples )
endif()

set( GRIB_API_DEFINITION_PATH  ${the_default_data_prefix}/${GRIB_API_DEFINITION_SUFF} )
set( GRIB_API_SAMPLES_PATH     ${the_default_data_prefix}/${GRIB_API_SAMPLES_SUFF} )
set( GRIB_API_IFS_SAMPLES_PATH ${the_default_data_prefix}/${GRIB_API_IFS_SAMPLES_SUFF} )

### config header

ecbuild_generate_config_headers()

configure_file( grib_api_config.h.in grib_api_config.h )

add_definitions( -DHAVE_GRIB_API_CONFIG_H )

install( FILES ${CMAKE_CURRENT_BINARY_DIR}/grib_api_config.h DESTINATION ${INSTALL_INCLUDE_DIR} )

if( CMAKE_COMPILER_IS_GNUCC )
    ecbuild_add_c_flags("-pedantic")
endif()

############################################################################################
# contents

### export package to other ecbuild packages

set( GRIB_API_INCLUDE_DIRS    ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_BINARY_DIR}/src )
set( GRIB_API_LIBRARIES       grib_api )

get_directory_property( COMPILE_DEFINITIONS GRIB_API_DEFINITIONS )

foreach( _tpl ${GRIB_API_TPLS} )
    string( TOUPPER ${_tpl} TPL )
    if( ${TPL}_FOUND )
        list( APPEND GRIB_API_EXTRA_DEFINITIONS   ${${TPL}_DEFINITIONS} )
        list( APPEND GRIB_API_EXTRA_INCLUDE_DIRS  ${${TPL}_INCLUDE_DIRS} ${${TPL}_INCLUDE_DIR} )
        list( APPEND GRIB_API_EXTRA_LIBRARIES     ${${TPL}_LIBRARIES} )
    endif()
endforeach()

### include directories

include_directories( ${GRIB_API_INCLUDE_DIRS}  ${GRIB_API_EXTRA_INCLUDE_DIRS} )

add_subdirectory( src )
add_subdirectory( tools )
add_subdirectory( fortran )
add_subdirectory( python )
add_subdirectory( definitions )
add_subdirectory( tests )
add_subdirectory( tigge )
add_subdirectory( examples )
add_subdirectory( data )
add_subdirectory( samples )
add_subdirectory( ifs_samples ) # must come after samples

# ecbuild_dont_pack( DIRS samples DONT_PACK_REGEX "*.grib" )
ecbuild_dont_pack( DIRS
     concepts tests.ecmwf doxygen examples.dev templates sms parameters java
     gaussian_experimental gribex examples/F77
     examples/extra bamboo definitions/bufr use fortran/fortranCtypes tigge/tools share/grib_api
     src/.deps tests/.deps tools/.deps tigge/.deps examples/C/.deps examples/python/.deps
     python/.deps fortran/.deps
)

add_custom_target(dist COMMAND ${CMAKE_MAKE_PROGRAM} package_source)

############################################################################################
# export to other projects

# temporary -- add support for ecbuild 1.0.x sub-project inclusion
# to remove once mars server & client use eckit & ecbuild >= 1.1

if( EC_HAVE_FORTRAN )
    list( APPEND GRIB_API_INCLUDE_DIRS  ${CMAKE_Fortran_MODULE_DIRECTORY} )
    list( APPEND GRIB_API_LIBRARIES grib_api_f77 grib_api_f90 )
endif()

# Bundle
if( NOT PROJECT_NAME STREQUAL CMAKE_PROJECT_NAME )
  set( GRIB_API_FOUND            TRUE PARENT_SCOPE )
  set( GRIB_API_DEFINITIONS      ${GRIB_API_DEFINITIONS}  PARENT_SCOPE ) # includes already TPL definitions
  set( GRIB_API_INCLUDE_DIRS     ${GRIB_API_INCLUDE_DIRS} ${GRIB_API_EXTRA_INCLUDE_DIRS} PARENT_SCOPE )
  set( GRIB_API_LIBRARIES        ${GRIB_API_LIBRARIES}    ${GRIB_API_EXTRA_LIBRARIES} ${CMATH_LIBRARIES} PARENT_SCOPE )
  set( GRIB_API_INSTALL_EXTRA_TOOLS ${GRIB_API_INSTALL_EXTRA_TOOLS} PARENT_SCOPE )
  set( GRIB_API_DEFINITION_PATH  ${CMAKE_BINARY_DIR}/${GRIB_API_DEFINITION_SUFF}  PARENT_SCOPE )
  set( GRIB_API_SAMPLES_PATH     ${CMAKE_BINARY_DIR}/${GRIB_API_SAMPLES_SUFF}     PARENT_SCOPE )
  set( GRIB_API_IFS_SAMPLES_PATH ${CMAKE_BINARY_DIR}/${GRIB_API_IFS_SAMPLES_SUFF} PARENT_SCOPE )
endif()

# pkg-config
ecbuild_pkgconfig(
  NAME                grib_api
  URL                 "https://software.ecmwf.int/wiki/display/GRIB/"
  DESCRIPTION         "The GRIB API library"
  LIBRARIES           grib_api
  VARIABLES           HAVE_JPG HAVE_LIBJASPER HAVE_LIBOPENJPEG
                      HAVE_GRIB_THREADS HAVE_GRIB_OMP_THREADS
                      HAVE_NETCDF HAVE_PYTHON HAVE_FORTRAN HAVE_PNG HAVE_AEC
)
if( EC_HAVE_FORTRAN )
 ecbuild_pkgconfig(
   NAME                grib_api_f90
   URL                 "https://software.ecmwf.int/wiki/display/GRIB/"
   LIBRARIES           grib_api_f90 grib_api
   DESCRIPTION         "The GRIB API library for Fortran 90"
   IGNORE_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/fortran ${PROJECT_BINARY_DIR}/fortran
   VARIABLES           HAVE_JPG HAVE_LIBJASPER HAVE_LIBOPENJPEG
                       HAVE_GRIB_THREADS HAVE_GRIB_OMP_THREADS
                       HAVE_NETCDF HAVE_PYTHON HAVE_PNG HAVE_AEC
 )
  ecbuild_pkgconfig(
   NAME                grib_api_f77
   URL                 "https://software.ecmwf.int/wiki/display/GRIB/"
   LIBRARIES           grib_api_f77 grib_api
   DESCRIPTION         "The GRIB API library for Fortran 77"
   IGNORE_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/fortran ${PROJECT_BINARY_DIR}/fortran
   VARIABLES           HAVE_JPG HAVE_LIBJASPER HAVE_LIBOPENJPEG
                       HAVE_GRIB_THREADS HAVE_GRIB_OMP_THREADS
                       HAVE_NETCDF HAVE_PYTHON HAVE_PNG HAVE_AEC
 )
endif()

############################################################################################
# finalize

ecbuild_install_project( NAME ${CMAKE_PROJECT_NAME} )

ecbuild_print_summary()

ecbuild_info("")
ecbuild_info("   +---------------------------+")
ecbuild_info("   |  GRIB API version ${GRIB_API_VERSION}  |")
ecbuild_info("   +---------------------------+")
ecbuild_info("")
ecbuild_info("   +------------------+ ")
ecbuild_info("   | IMPORTANT NOTICE | ")
ecbuild_info("   +---------------------------------------------------------------------------------------------+ ")
ecbuild_info("   | Please note that 'ecCodes' is now the primary GRIB encoding/decoding package used at ECMWF. | ")
ecbuild_info("   | Replacing GRIB API with ecCodes is expected to be transparent for current GRIB API users.   | ")
ecbuild_info("   | In particular the functions with the 'grib_' prefix are included in the ecCodes library.    | ")
ecbuild_info("   |                                                                                             | ")
ecbuild_info("   | GRIB API support is being discontinued at the end of 2018.                                  | ")
ecbuild_info("   | You are strongly advised to start the migration process.                                    | ")
ecbuild_info("   |                                                                                             | ")
ecbuild_info("   | For more details, please see:                                                               | ")
ecbuild_info("   |   https://software.ecmwf.int/wiki/display/ECC/ecCodes+Home                                  | ")
ecbuild_info("   +---------------------------------------------------------------------------------------------+ ")
ecbuild_info("")
