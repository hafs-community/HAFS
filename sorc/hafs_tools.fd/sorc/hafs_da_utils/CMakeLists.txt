#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

cmake_minimum_required(VERSION 3.15)
project( 
  DA_UTILS
  LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules/")

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_C_COMPILER_ID}")
endif()

find_package(NetCDF REQUIRED C Fortran)
find_package(bacio REQUIRED)
find_package(nemsio REQUIRED)
find_package(bufr REQUIRED)


set(fortran_srcs
  kinds_interface.F90 
  constants_interface.F90 
  diagnostics_interface.F90 
  time_methods_interface.F90 
  namelist_interface.F90 
  variable_interface.F90 
  math_methods_interface.F90 
  netcdf_interface.F90 
  bufrio_interface.F90 
  json_interface.F90 
  nemsio_interface.F90 
  fileio_interface.F90 
  gsidiags_interface.F90 
  da_utils_interface.F90
  main.F90)

set(exe_name hafs_da_utils.x)
message("TYPE CMAKE_SOURCE_DIR = ${CMAKE_SOURCE_DIR}")
set(exec_dir ${CMAKE_SOURCE_DIR}/../../exec)
set(ilib_dir ${CMAKE_SOURCE_DIR}/../hafs_nwplibs/lib)
set(inc_dir ${CMAKE_SOURCE_DIR}/../hafs_nwplibs/include)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -heap-arrays -mcmodel=large -integer-size 32 -real-size 32")

add_executable(${exe_name} ${fortran_srcs})

target_include_directories(
  ${exe_name} PUBLIC
  ${inc_dir})

target_link_libraries(
  ${exe_name}
  bufr::bufr_d
  ${ilib_dir}/libdiagsread.a
  ${ilib_dir}/libfson.a
  ${ilib_dir}/libgridprojs.a
  nemsio::nemsio
  NetCDF::NetCDF_Fortran
  NetCDF::NetCDF_C
  bacio::bacio_4
  ${ilib_dir}/libw3.a)

 install(TARGETS ${exe_name} DESTINATION ${exec_dir})
