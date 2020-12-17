#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

set(fortran_srcs
  kinds_interface.F90
  constants_interface.F90
  variable_interface.F90
  namelist_interface.F90
  diagnostics_interface.F90
  time_methods_interface.F90
  math_methods_interface.F90
  meteo_methods_interface.F90
  bufrio_interface.F90
  json_interface.F90
  netcdf_interface.F90
  fileio_interface.F90
  interpolation_interface.F90
  observation_interface.F90
  obs_to_bufr_interface.F90
  main.F90)

set(exe_name obs_to_bufr.x)
set(exec_dir ${CMAKE_SOURCE_DIR}/exec)
set(ilib_dir ${CMAKE_SOURCE_DIR}/lib)
set(inc_dir ${CMAKE_SOURCE_DIR}/include)

add_executable(${exe_name} ${fortran_srcs})

target_include_directories(
  ${exe_name} PUBLIC
  ${inc_dir})

target_link_libraries(
  ${exe_name}
  ${ilib_dir}/libfson.a
  ${ilib_dir}/libslatec.a
  ${ilib_dir}/libslint.a
  ${ilib_dir}/libspline.a
  NetCDF::NetCDF_Fortran
  ${BUFR_LIBRARY})

install(TARGETS ${exe_name} DESTINATION ${exec_dir})
