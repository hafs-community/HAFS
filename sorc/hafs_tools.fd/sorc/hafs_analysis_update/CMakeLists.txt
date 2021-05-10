#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

cmake_minimum_required(VERSION 3.15)
project( 
  ANALYSIS_UPDATE
  LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules/")

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_C_COMPILER_ID}")
endif()

find_package(bacio REQUIRED)
find_package(nemsio REQUIRED)
find_package(w3nco REQUIRED)
find_package(w3emc REQUIRED)
find_package(NetCDF REQUIRED C Fortran)

set(fortran_srcs
  kinds_interface.F90 
  constants_interface.F90 
  namelist_interface.F90 
  diagnostics_interface.F90 
  variable_interface.F90 
  json_interface.F90 
  time_methods_interface.F90 
  nemsio_interface.F90 
  netcdf_interface.F90 
  fileio_interface.F90 
  math_methods_interface.F90 
  grid_methods_interface.F90 
  meteo_methods_interface.F90 
  interpolation_interface.F90 
  pattern_interface.F90 
  ensmemgen_interface.F90 
  update_state_interface.F90 
  fv3_analysis_interface.F90 
  analysis_update_interface.F90
  main.F90)

set(exe_name hafs_analysis_update.x)
message("TYPE CMAKE_SOURCE_DIR = ${CMAKE_SOURCE_DIR}")
set(exec_dir ${CMAKE_SOURCE_DIR}/../../exec)
set(ext_dir ${CMAKE_SOURCE_DIR}/../hafs_extlibs/lib)
set(ext_inc_dir ${CMAKE_SOURCE_DIR}/../hafs_extlibs/include)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -heap-arrays -mkl=sequential -convert big_endian -assume byterecl -DLINUX")

add_executable(${exe_name} ${fortran_srcs})

target_include_directories(
  ${exe_name} PUBLIC
  ${ext_inc_dir})

target_link_libraries(
  ${exe_name}
  ${ext_dir}/libfftw3.a
  ${ext_dir}/libfson.a
  ${ext_dir}/libgridprojs.a
  ${ext_dir}/libkdtree.a
  nemsio::nemsio
  NetCDF::NetCDF_Fortran
  NetCDF::NetCDF_C
  ${ext_dir}/libshtns.a
  ${ext_dir}/libfftw3.a
  ${ext_dir}/libshtns_interface.a
  ${ext_dir}/libslatec.a
  ${ext_dir}/libslint.a
  ${ext_dir}/libspline.a
  bacio::bacio_4
  w3emc::w3emc_d
  w3nco::w3nco_d)


 install(TARGETS ${exe_name} DESTINATION ${exec_dir})
