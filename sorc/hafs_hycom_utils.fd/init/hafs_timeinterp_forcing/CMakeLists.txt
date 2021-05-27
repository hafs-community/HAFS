#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================
#/mnt/lfs4/HFIP/hwrfv3/Biju.Thomas/hafs_couplehycom/sorc/hafs_hycom_utils.fd/build/libs/src/ofs_mods/libofs_mods.a
#  ${ilib_dir}/ofs_mods/libofs_mods.a

set(fortran_srcs
  get_abfld1.f
  timeinterp_forcing.f90)

set(ilib_dir ${CMAKE_SOURCE_DIR}/libs/build/src)
set(exe_name hafs_timeinterp_forcing)

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
  MPI::MPI_Fortran)

install(TARGETS ${exe_name} DESTINATION ${exe_dir})
