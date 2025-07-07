#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

cmake_minimum_required(VERSION 3.15)
project(
   CALC_ANALYSIS
   LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules/")

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_C_COMPILER_ID}")
endif()

find_package(NetCDF REQUIRED C Fortran)
find_package(MPI REQUIRED)
find_package(bacio REQUIRED)
find_package(nemsio REQUIRED)
find_package(w3emc REQUIRED)

set(fortran_srcs
  module_ncio.f90
  inc2anl.f90
  init_calc_analysis.f90
  init_io.f90
  main.f90
  vars_calc_analysis.f90)

set(exe_name hafs_calc_analysis.x)
set(exec_dir ${CMAKE_CURRENT_SOURCE_DIR}/../../exec) 

add_executable(${exe_name} ${fortran_srcs}) 

target_compile_options(${exe_name} PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>:-O0 -g -check all -ftrapuv> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O3>
    -traceback -fp-model strict -convert big_endian)

target_link_options(${exe_name} PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>:-O0 -g -check all -ftrapuv> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O3>
    -traceback -fp-model strict -convert big_endian)

target_link_libraries(
  ${exe_name} PRIVATE
  NetCDF::NetCDF_Fortran
  NetCDF::NetCDF_C
  MPI::MPI_Fortran
  bacio::bacio_4
  nemsio::nemsio
  w3emc::w3emc_4)

install(TARGETS ${exe_name} DESTINATION ${exec_dir})
