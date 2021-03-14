#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

set(fortran_srcs
  flush.f90
  constants.f90
  horiz_interp.f90
  mod_hytime.f90
  mod_flags.f90
  mod_hycomio1.f90
  mod_dump.f90
  mod_grib2io.f90
  mod_geom.f90
  intp.f90
  cd.f90)

set(ilib_dir ${CMAKE_SOURCE_DIR}/libs/build/src)
set(exe_name hafs_gfs2ofs2)

set(exe_dir ${CMAKE_SOURCE_DIR}/exec)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -convert big_endian -DLINUX -fp-model precise -assume byterecl")
endif()

add_executable(${exe_name} ${fortran_srcs})

target_include_directories(
  ${exe_name} PUBLIC
  ${ilib_dir}/ofs_mods)

target_link_libraries(
  ${exe_name}
  ${ilib_dir}/ofs_mods/libofs_mods.a
  w3nco::w3nco_4
  bacio::bacio_4
  NetCDF::NetCDF_Fortran
  NetCDF::NetCDF_C
  g2::g2_4
  w3emc::w3emc_4
  MPI::MPI_Fortran)

install(TARGETS ${exe_name} DESTINATION ${exe_dir})
