#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

cmake_minimum_required(VERSION 3.15)
project( 
  HAFS_VI
  LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules/")

message(CMAKE_MODULE_PATH " ${CMAKE_MODULE_PATH}")
if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_C_COMPILER_ID}")
endif()


#find_package(NetCDF REQUIRED C Fortran)
#find_package(MPI REQUIRED)
find_package(OpenMP REQUIRED)
find_package(MKL REQUIRED)

add_subdirectory(create_trak_guess)
add_subdirectory(create_trak_init)
add_subdirectory(split)
add_subdirectory(anl_pert)
add_subdirectory(anl_combine)
add_subdirectory(anl_enhance)
add_subdirectory(anl_bogus)
