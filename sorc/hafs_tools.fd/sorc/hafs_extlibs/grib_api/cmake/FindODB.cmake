# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# - Try to find ODB
# Once done this will define
#  ODB_FOUND - System has ODB
#  ODB_INCLUDE_DIRS - The ODB include directories
#  ODB_LIBRARIES - The libraries needed to use ODB

# /usr/local/apps/odb/CY37R3.001/pgf90/LP64/include/odbdump.h
# /usr/local/apps/odb/CY37R3.001/pgf90/LP64/module/odb.mod

# -lodb -lodbec -lifsaux -lmpi_serial -lodbdummy

find_package( Dl ) # find the dynamic linker

list( APPEND _odb_search_libs odb odbec ifsaux mpi_serial odbdummy  )

if( DEFINED ODB_PATH )
    find_path(ODB_INCLUDE_DIR odbdump.h PATHS  ${ODB_ROOT} ${ODB_ROOT}/include ${ODB_PATH} ${ODB_PATH}/include PATH_SUFFIXES odb NO_DEFAULT_PATH)
    find_path(ODB_MODULE_DIR odb.mod PATHS ${ODB_ROOT} ${ODB_ROOT}/module ${ODB_PATH} ${ODB_PATH}/module PATH_SUFFIXES odb NO_DEFAULT_PATH)
    foreach( _lib ${_odb_search_libs} )
      find_library(ODB_LIBRARY_${_lib}  ${_lib} PATHS ${ODB_ROOT} ${ODB_ROOT}/lib ${ODB_PATH} ${ODB_PATH}/lib     PATH_SUFFIXES odb NO_DEFAULT_PATH)
    endforeach()
endif()

find_path(ODB_INCLUDE_DIR odbdump.h PATH_SUFFIXES odb )
find_path(ODB_MODULE_DIR odb.mod PATH_SUFFIXES odb )
foreach( _lib ${_odb_search_libs} )
  find_library( ODB_LIBRARY_${_lib} ${_lib} PATH_SUFFIXES odb )
endforeach()

foreach( _lib ${_odb_search_libs} )
  list( APPEND ODB_LIB_LIST   ODB_LIBRARY_${_lib} )
  list( APPEND ODB_LIBRARIES  ${ODB_LIBRARY_${_lib}} )
  mark_as_advanced(${ODB_LIBRARY_${_lib}})
endforeach()

set( ODB_INCLUDE_DIRS ${ODB_INCLUDE_DIR} ${ODB_MODULE_DIR})
mark_as_advanced(ODB_INCLUDE_DIR )

include(FindPackageHandleStandardArgs)

# handle the QUIETLY and REQUIRED arguments and set GRIBAPI_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(ODB  DEFAULT_MSG
                                  ODB_INCLUDE_DIR ${ODB_LIB_LIST} )

