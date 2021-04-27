#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

set(fortran_srcs
  zh.F
  mod_xc.F
  mod_za.F
  mod_zb.F
  wtime.F
  subregion.f)

set(exe_name hafs_rtofs_subregion)

set(exe_dir ${CMAKE_SOURCE_DIR}/exec)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -convert big_endian -DLINUX -fp-model precise -assume byterecl")
endif()

add_executable(${exe_name} ${fortran_srcs})

add_definitions(-DLITTLE_ENDIAN -DWRF -DLINUX -DSERIAL_IO)

install(TARGETS ${exe_name} DESTINATION ${exe_dir})
