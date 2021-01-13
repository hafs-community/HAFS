#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

cmake_minimum_required(VERSION 3.15)
project( 
  POST_UTILS
  LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules/")

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_C_COMPILER_ID}")
endif()

find_package(NetCDF REQUIRED C Fortran)
find_package(nemsio REQUIRED)
find_package(bacio REQUIRED)


set(fortran_srcs
  kinds_interface.F90 
  constants_interface.F90 
  diagnostics_interface.F90 
  namelist_interface.F90 
  variable_interface.F90 
  json_interface.F90 
  nemsio_interface.F90 
  netcdf_interface.F90 
  fileio_interface.F90 
  interpolation_interface.F90 
  grid_methods_interface.F90 
  fv3_post_interface.F90 
  post_utils_interface.F90
  main.F90)

set(exe_name hafs_post_utils.x)
set(exec_dir ${CMAKE_SOURCE_DIR}/../../exec)
set(nwp_dir ${CMAKE_SOURCE_DIR}/../hafs_nwplibs/lib)
set(nwp_inc_dir ${CMAKE_SOURCE_DIR}/../hafs_nwplibs/include)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -mkl=sequential -convert big_endian -assume byterecl -DLINUX")

add_executable(${exe_name} ${fortran_srcs})

target_include_directories(
  ${exe_name} PUBLIC
  ${nwp_inc_dir})

target_link_libraries(
  ${exe_name}
  ${nwp_dir}/libfson.a
  ${nwp_dir}/libgridprojs.a
  nemsio::nemsio
  NetCDF::NetCDF_Fortran
  ${nwp_dir}/libslint.a
  ${nwp_dir}/libw3.a
  bacio::bacio_4)

 install(TARGETS ${exe_name} DESTINATION ${exec_dir})
