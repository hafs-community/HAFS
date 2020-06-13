# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# Try to find NetCDF3 or NetCDF4 -- default is 4
#
# find_package( NetCDF <version> COMPONENTS C CXX Fortran )
#
# Input:
#  * NETCDF_PATH    - user defined path where to search for the library first
#  * NETCDF_DIR     - user defined path where to search for the library first
#  * NETCDF_ROOT    - user defined path where to search for the library first
#
# Output:
#  NETCDF_FOUND - System has NetCDF
#  NETCDF_DEFINITIONS
#  NETCDF_INCLUDE_DIRS - The NetCDF include directories
#  NETCDF_LIBRARIES - The libraries needed to use NetCDF

# default is netcdf4
if( NetCDF_FIND_VERSION STREQUAL "3" )
  set( PREFER_NETCDF3 1 )
endif()

if( NOT PREFER_NETCDF3 )
  set( PREFER_NETCDF4 1 )
else()
  set( PREFER_NETCDF4 0 )
endif()
mark_as_advanced( PREFER_NETCDF4 PREFER_NETCDF3 )

set( NETCDF_FIND_REQUIRED   ${NetCDF_FIND_REQUIRED} )
set( NETCDF_FIND_QUIETLY    ${NetCDF_FIND_QUIETLY} )
set( NETCDF_FIND_COMPONENTS ${NetCDF_FIND_COMPONENTS} )

list( APPEND NETCDF_FIND_COMPONENTS C )

if( NETCDF_CXX )
  ecbuild_debug( "FindNetCDF: also looking for C++ libraries" )
  list( APPEND NETCDF_FIND_COMPONENTS CXX )
endif()

if( NETCDF_Fortran OR NETCDF_FORTRAN OR NETCDF_F90 )
  ecbuild_debug( "FindNetCDF: also looking for Fortran libraries" )
  list( APPEND NETCDF_FIND_COMPONENTS FORTRAN F90 )
endif()

list(FIND NETCDF_FIND_COMPONENTS "FORTRAN" _index)
if(${_index} GREATER -1)
  list( APPEND NETCDF_FIND_COMPONENTS F90 )
endif()

list (FIND NETCDF_FIND_COMPONENTS "F90" _index)
if(${_index} GREATER -1)
  list( APPEND NETCDF_FIND_COMPONENTS FORTRAN )
endif()

list(FIND NETCDF_FIND_COMPONENTS "Fortran" _index)
if(${_index} GREATER -1)
  list( REMOVE_ITEM NETCDF_FIND_COMPONENTS Fortran )
  list( APPEND NETCDF_FIND_COMPONENTS FORTRAN F90 )
endif()

list( REMOVE_DUPLICATES NETCDF_FIND_COMPONENTS )
ecbuild_debug( "FindNetCDF: looking for components ${NETCDF_FIND_COMPONENTS}" )

### NetCDF4

if( PREFER_NETCDF4 )

  ecbuild_debug( "FindNetCDF: looking for NetCDF4" )

  ## hdf5

  # Note: Only the HDF5 C-library is required for NetCDF
  #       ( even for Fortan and CXX bindings)
  find_package( HDF5 COMPONENTS C QUIET )

  ## netcdf4

  # CONFIGURE the NETCDF_FIND_COMPONENTS variable

  # Find NetCDF4

  # message( "NETCDF CMAKE_PREFIX_PATH = [${CMAKE_PREFIX_PATH}]")
  # ecbuild_debug_var( NETCDF_ROOT )
  # ecbuild_debug_var( NETCDF_FIND_COMPONENTS )
  # ecbuild_debug_var( NETCDF_FIND_QUIETLY )
  # ecbuild_debug_var( NETCDF_FIND_REQUIRED )
  find_package( NetCDF4 COMPONENTS ${NETCDF_FIND_COMPONENTS} )
  # ecbuild_debug_var( NETCDF4_FOUND )
  # ecbuild_debug_var( NETCDF_FOUND )
  # ecbuild_debug_var( NETCDF_LIBRARIES )
  # ecbuild_debug_var( NETCDF_INCLUDE_DIRS )

  list( APPEND NETCDF_Fortran_LIBRARIES ${NETCDF_FORTRAN_LIBRARIES} ${NETCDF_F90_LIBRARIES} )
  if( NETCDF_Fortran_LIBRARIES )
    list( REMOVE_DUPLICATES NETCDF_Fortran_LIBRARIES )
  endif()

  # ecbuild_debug_var( NETCDF_Fortran_LIBRARIES )
  # ecbuild_debug_var( NETCDF_C_LIBRARIES )
  # ecbuild_debug_var( NETCDF_CXX_LIBRARIES )


  set_package_properties( NetCDF4 PROPERTIES TYPE RECOMMENDED PURPOSE "support for NetCDF4 file format" )

  if( NETCDF_FOUND AND HDF5_FOUND )
    # list( APPEND NETCDF_DEFINITIONS  ${HDF5_DEFINITIONS} )
    list( APPEND NETCDF_LIBRARIES    ${HDF5_HL_LIBRARIES} ${HDF5_LIBRARIES}  )
    list( APPEND NETCDF_INCLUDE_DIRS ${HDF5_INCLUDE_DIRS} )
  endif()

  #ecbuild_debug_var( NETCDF_FOUND )
  #ecbuild_debug_var( NETCDF_LIBRARIES )
  #ecbuild_debug_var( NETCDF_INCLUDE_DIRS )
  #ecbuild_debug_var( HDF5_FOUND )
  #ecbuild_debug_var( HDF5_INCLUDE_DIRS )
  #ecbuild_debug_var( HDF5_HL_LIBRARIES )
  #ecbuild_debug_var( HDF5_LIBRARIES )

endif()

### NetCDF3

if( PREFER_NETCDF3 )

  ecbuild_debug( "FindNetCDF: looking for NetCDF3" )

  # ecbuild_debug_var( NetCDF_FIND_COMPONENTS )
  # ecbuild_debug_var( NetCDF_FIND_QUIETLY )
  # ecbuild_debug_var( NetCDF_FIND_REQUIRED )

  list(FIND NetCDF_FIND_COMPONENTS "CXX" _index)
  if(${_index} GREATER -1)
    set( NETCDF_CXX 1 )
  endif()

  list(FIND NetCDF_FIND_COMPONENTS "Fortran" _index)
  if(${_index} GREATER -1)
    set( NETCDF_Fortran 1 )
  endif()

  list(FIND NetCDF_FIND_COMPONENTS "FORTRAN" _index)
  if(${_index} GREATER -1)
    set( NETCDF_Fortran 1 )
  endif()

  list(FIND NetCDF_FIND_COMPONENTS "F90" _index)
  if(${_index} GREATER -1)
    set( NETCDF_Fortran 1 )
  endif()

  #message( "NETCDF CMAKE_PREFIX_PATH = ${CMAKE_PREFIX_PATH}" )

  find_package( NetCDF3 COMPONENTS ${NETCDF_FIND_COMPONENTS} )

  set_package_properties( NetCDF3 PROPERTIES TYPE RECOMMENDED PURPOSE "support for NetCDF3 file format" )

endif()
