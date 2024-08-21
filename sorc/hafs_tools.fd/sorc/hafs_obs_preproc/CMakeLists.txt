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

option(USE_NOEXECSTACK "Direct the linker that executable stack is not needed" ON) 

find_package(NetCDF REQUIRED C Fortran)
find_package(bufr REQUIRED)
find_package(MPI REQUIRED)

set(fortran_srcs
  ../hafs_exttools/fson/fson_string_m.f90
  ../hafs_exttools/fson/fson_value_m.f90
  ../hafs_exttools/fson/fson_path_m.f90
  ../hafs_exttools/fson/fson.f90
  ../hafs_exttools/gridprojs/noaa_routines.f
  ../hafs_exttools/gridprojs/gridprojs_interface.F90
  ../hafs_exttools/gridtrans/gridtrans_interface.F90
  ../hafs_exttools/gridtrans/gridtrans_fv3_interface.F90
  ../hafs_exttools/kdtree/kdtree2.F90
  ../hafs_exttools/slatec/fdump.f
  ../hafs_exttools/slatec/i1mach.f
  ../hafs_exttools/slatec/j4save.f
  ../hafs_exttools/slatec/ssort.f
  ../hafs_exttools/slatec/xercnt.f
  ../hafs_exttools/slatec/xerhlt.f
  ../hafs_exttools/slatec/xermsg.f
  ../hafs_exttools/slatec/xerprn.f
  ../hafs_exttools/slatec/xersve.f
  ../hafs_exttools/slatec/xgetua.f
  ../hafs_exttools/sondelib/sondelib.f
  ../hafs_exttools/sondelib/thermo_subs.f
  ../hafs_exttools/sondelib/aoml_routines.F90
  ../hafs_exttools/spline/spline.f90
  ../hafs_exttools/wmm/geomag.f
  ../hafs_exttools/wmm/wmm_interface.F90 
  ./kinds_interface.F90
  ./constants_interface.F90
  ./variable_interface.F90
  ./namelist_interface.F90
  ./diagnostics_interface.F90
  ./math_methods_interface.F90
  ./grid_methods_interface.F90
  ./meteo_methods_interface.F90
  ./bufrio_interface.F90
  ./json_interface.F90
  ./netcdf_interface.F90
  ./fileio_interface.F90
  ./time_methods_interface.F90
  ./bufr_obs_interface.F90
  ./forecast_model_interface.F90
  ./recon_tdr_interface.F90
  ./recon_vdm_interface.F90
  ./sonde_tempdrop_interface.F90
  ./observations_interface.F90
  ./obs_preproc_interface.F90
  ./main.F90)

set(exe_name hafs_obs_preproc.x)
message("TYPE CMAKE_SOURCE_DIR = ${CMAKE_SOURCE_DIR}")
set(exec_dir ${CMAKE_SOURCE_DIR}/../../exec)

add_executable(${exe_name} ${fortran_srcs})

target_compile_options(${exe_name} PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O3>
    -traceback -fp-model precise -assume byterecl -convert big_endian)

target_link_options(${exe_name} PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O3>
    -traceback -fp-model precise -assume byterecl -convert big_endian)

if(USE_NOEXECSTACK)
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -z noexecstack")
endif()

target_link_libraries(
  ${exe_name}
  bufr::bufr_4
  NetCDF::NetCDF_Fortran
  NetCDF::NetCDF_C
  MPI::MPI_Fortran)

 install(TARGETS ${exe_name} DESTINATION ${exec_dir})
