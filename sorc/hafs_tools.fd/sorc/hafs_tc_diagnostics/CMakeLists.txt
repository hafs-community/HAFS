#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

cmake_minimum_required(VERSION 3.15)
project( 
  TC_DIAGNOSTICS
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

set(fortran_srcs
  kinds_interface.F90 
  constants_interface.F90 
  mpi_interface.F90 
  namelist_interface.F90 
  diagnostics_interface.F90 
  variable_interface.F90 
  time_methods_interface.F90 
  math_methods_interface.F90 
  meteo_methods_interface.F90 
  grid_methods_interface.F90 
  grib_api_interface.F90 
  json_interface.F90 
  netcdf_interface.F90 
  fileio_interface.F90 
  interpolation_interface.F90 
  tropcyc_methods_interface.F90 
  tc_model_interface.F90 
  filter_interface.F90 
  analysis_interface.F90 
  tcdiags_methods_interface.F90 
  tc_diagnostics_interface.F90
  tc_mpi/pcmin_2013.f
  main.F90)

set(exe_name hafs_tc_diagnostics.x)
message("TYPE CMAKE_SOURCE_DIR = ${CMAKE_SOURCE_DIR}")
set(exec_dir ${CMAKE_SOURCE_DIR}/../../exec)
set(nwp_dir ${CMAKE_SOURCE_DIR}/../hafs_nwplibs/lib)
set(nwp_inc_dir ${CMAKE_SOURCE_DIR}/../hafs_nwplibs/include)
set(ext_dir ${CMAKE_SOURCE_DIR}/../hafs_extlibs/lib)
set(ext_inc_dir ${CMAKE_SOURCE_DIR}/../hafs_extlibs/include)
set(ecc_dir ${CMAKE_SOURCE_DIR}/../hafs_extlibs/ecCodes/lib)
set(ecc_inc_dir ${CMAKE_SOURCE_DIR}/../hafs_extlibs/ecCodes/include)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -heap-arrays -mkl=sequential -assume byterecl -mcmodel=large -DLINUX")

add_executable(${exe_name} ${fortran_srcs})

target_include_directories(
  ${exe_name} PUBLIC
  ${nwp_inc_dir}
  ${ecc_inc_dir})

target_link_libraries(
  ${exe_name}
  ${ext_dir}/libfftw3.a
  ${nwp_dir}/libfson.a
  ${ecc_dir}/libeccodes_f90.so
  ${ecc_dir}/libeccodes.so 
  ${nwp_dir}/libkdtree.a
  NetCDF::NetCDF_Fortran
  NetCDF::NetCDF_C
  ${nwp_dir}/libslint.a
  ${nwp_dir}/libslatec.a
  ${nwp_dir}/libspherepack.a
  ${nwp_dir}/libspline.a
  ${JASPER_LIBRARIES}
  MPI::MPI_Fortran)

install(TARGETS ${exe_name} DESTINATION ${exec_dir})
