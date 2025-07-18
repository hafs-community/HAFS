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

find_package(bufr REQUIRED)

set(fortran_srcs
  ./module_bufrdrops.f90
  ./read_bufrdrops_dump.f90)

set(exe_name hafs_dropsonde_superob.x)
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

 install(TARGETS ${exe_name} DESTINATION ${exec_dir})
