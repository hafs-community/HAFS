#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

cmake_minimum_required(VERSION 3.15)
project( 
  FILE_CHECK
  LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules/")

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_C_COMPILER_ID}")
endif()

find_package(NetCDF REQUIRED C Fortran)

set(fortran_srcs
  kinds_interface.F90 
  diagnostics_interface.F90 
  namelist_interface.F90 
  file_check_interface.F90
  main.F90)

set(exe_name hafs_file_check.x)
message("TYPE CMAKE_SOURCE_DIR = ${CMAKE_SOURCE_DIR}")
set(exec_dir ${CMAKE_SOURCE_DIR}/../../exec)
set(nwp_dir ${CMAKE_SOURCE_DIR}/../hafs_nwplibs/lib)
set(nwp_inc_dir ${CMAKE_SOURCE_DIR}/../hafs_nwplibs/include)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -mkl=sequential -assume byterecl")

add_executable(${exe_name} ${fortran_srcs})

target_include_directories(
  ${exe_name} PUBLIC
  ${nwp_inc_dir})

target_link_libraries(
  ${exe_name}
  ${nwp_dir}/libnemsio.a
  ${nwp_dir}/libbacio.a)

 install(TARGETS ${exe_name} DESTINATION ${exec_dir})
