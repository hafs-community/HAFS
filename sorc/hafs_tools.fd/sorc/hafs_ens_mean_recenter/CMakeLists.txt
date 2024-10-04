cmake_minimum_required(VERSION 3.15)
project(
  HAFS_ENS_MEAN_RECENTER
  LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules/")

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_C_COMPILER_ID}")
endif()

find_package(ncio REQUIRED)

set(fortran_srcs
  module_fv3lam_stats.f90
  module_mpi_arrange.f90
  ens_mean_recenter.f90)

set (exe_name hafs_ens_mean_recenter.x)
set (exec_dir ${CMAKE_CURRENT_SOURCE_DIR}/../../exec)

add_executable(${exe_name} ${fortran_srcs})

target_compile_options(${exe_name} PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>:-O0 -g -check all -ftrapuv> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O3>
    -traceback -fp-model precise -assume byterecl -convert big_endian)

target_link_options(${exe_name} PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>:-O0 -g -check all -ftrapuv> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O3>
    -traceback -fp-model precise -assume byterecl -convert big_endian)

target_link_libraries(
  ${exe_name} PRIVATE
  ncio::ncio
  NetCDF::NetCDF_Fortran
  NetCDF::NetCDF_C
  MPI::MPI_Fortran)

install(TARGETS ${exe_name} DESTINATION ${exec_dir})
