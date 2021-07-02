#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

cmake_minimum_required(VERSION 3.15)
project( 
  OBS_PREPROC
  LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules/")

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_C_COMPILER_ID}")
endif()

find_package(NetCDF REQUIRED C Fortran)
find_package(bufr REQUIRED)
find_package(MPI REQUIRED)

set(fortran_srcs
  obs_preproc/src/kinds_interface.F90
  obs_preproc/src/constants_interface.F90
  obs_preproc/src/variable_interface.F90
  obs_preproc/src/namelist_interface.F90
  obs_preproc/src/diagnostics_interface.F90
  obs_preproc/src/math_methods_interface.F90
  obs_preproc/src/grid_methods_interface.F90
  obs_preproc/src/meteo_methods_interface.F90
  obs_preproc/src/bufrio_interface.F90
  obs_preproc/src/json_interface.F90
  obs_preproc/src/netcdf_interface.F90
  obs_preproc/src/fileio_interface.F90
  obs_preproc/src/time_methods_interface.F90
  obs_preproc/src/bufr_obs_interface.F90
  obs_preproc/src/forecast_model_interface.F90
  obs_preproc/src/recon_tdr_interface.F90
  obs_preproc/src/recon_vdm_interface.F90
  obs_preproc/src/sonde_tempdrop_interface.F90
  obs_preproc/src/observations_interface.F90
  obs_preproc/src/obs_preproc_interface.F90
  obs_preproc/src/main.F90)

set(exe_name hafs_obs_preproc.x)
message("TYPE CMAKE_SOURCE_DIR = ${CMAKE_SOURCE_DIR}")
set(exec_dir ${CMAKE_SOURCE_DIR}/../../exec)
set(ext_dir ${CMAKE_SOURCE_DIR}/../hafs_extlibs/lib)
set(ext_inc_dir ${CMAKE_SOURCE_DIR}/../hafs_extlibs/include)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -fp-model precise -assume byterecl -convert big_endian")

add_executable(${exe_name} ${fortran_srcs})

target_include_directories(
  ${exe_name} PUBLIC
  ${ext_inc_dir})

target_link_libraries(
  ${exe_name}
  bufr::bufr_d
  ${ext_dir}/libfson.a
  ${ext_dir}/libgridprojs.a
  ${ext_dir}/libgridtrans.a
  ${ext_dir}/libkdtree.a
  NetCDF::NetCDF_Fortran
  NetCDF::NetCDF_C
  MPI::MPI_Fortran
  ${ext_dir}/libslatec.a
  ${ext_dir}/libsondelib.a
  ${ext_dir}/libspline.a
  ${ext_dir}/libspline.a
  ${ext_dir}/libwmm.a)

 install(TARGETS ${exe_name} DESTINATION ${exec_dir})
