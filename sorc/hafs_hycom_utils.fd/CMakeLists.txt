#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

cmake_minimum_required(VERSION 3.15)
project( 
  HYCOM_UTILS
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
find_package(HDF5 REQUIRED)
find_package(ZLIB REQUIRED)
find_package(bacio REQUIRED)
find_package(w3nco REQUIRED)
find_package(w3emc REQUIRED)
find_package(MPI REQUIRED) 
find_package(PNG REQUIRED)
find_package(Jasper REQUIRED)
find_package(g2 REQUIRED)

add_subdirectory(post/hafs_ab2data)
add_subdirectory(init/hafs_archv2restart)
add_subdirectory(init/hafs_subregion)
add_subdirectory(init/hafs_gfs2ofs2)
add_subdirectory(init/hafs_timeinterp_forcing)
add_subdirectory(init/hafs_restart2restart)
add_subdirectory(init/hafs_get_rtofs)
