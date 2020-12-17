#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

cmake_minimum_required(VERSION 3.15)
project( 
  HAFS_TOOLS
  LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/")
set (HAFS_TOOLS_BASE_DIR ${CMAKE_CURRENT_SOURCE_DIR})

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_C_COMPILER_ID}")
endif()

find_package(NetCDF REQUIRED C Fortran)
#find_package(HDF5 REQUIRED)
find_package(ZLIB REQUIRED)
find_package(BUFR REQUIRED)
find_package(MPI REQUIRED) 

add_subdirectory(sorc/obs_to_bufr)
add_subdirectory(sorc/tempdrop_sonde)
add_subdirectory(sorc/mpiserial)
