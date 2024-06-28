#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

set(fortran_srcs
  get_abfld1.f
  timeinterp_forcing.f90)

set(ilib_dir ${CMAKE_SOURCE_DIR}/tools/build/src)
set(exe_name hafs_timeinterp_forcing)

set(exe_dir ${CMAKE_SOURCE_DIR}/exec)

if(NOT CMAKE_BUILD_TYPE)                                                                             
    set(CMAKE_BUILD_TYPE "RELEASE")                                                                  
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -traceback -convert big_endian -DLINUX -fp-model precise -assume byterecl")
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -traceback -check all -ftrapuv -convert big_endian -DLINUX -fp-model precise -assume byterecl")
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
