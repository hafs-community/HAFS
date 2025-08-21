#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

cmake_minimum_required(VERSION 3.15)
project( 
  TDR_SUPEROB
  LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules/")

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_C_COMPILER_ID}")
endif()

#find_package(NetCDF REQUIRED C Fortran)
find_package(bufr REQUIRED)
#find_package(MPI REQUIRED)

set(fortran_srcs
  ./module_tdr_common.f90
  ./module_tdr_bufr.f90
  ./module_tdr_superob.f90
  ./tdr_so_main.f90)

set(exe_name hafs_tdr_superob.x)
message("TYPE CMAKE_SOURCE_DIR = ${CMAKE_SOURCE_DIR}")
set(exec_dir ${CMAKE_SOURCE_DIR}/../../exec)

add_executable(${exe_name} ${fortran_srcs})

target_compile_options(${exe_name} PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O3>
    -traceback)

target_link_options(${exe_name} PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O3>
    -traceback)

target_link_libraries(
  ${exe_name}
  bufr::bufr_4)
#  NetCDF::NetCDF_Fortran
#  NetCDF::NetCDF_C
#  MPI::MPI_Fortran)

 install(TARGETS ${exe_name} DESTINATION ${exec_dir})
