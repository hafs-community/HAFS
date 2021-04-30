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
  mod_plot.F
  mod_restart.F
  wtime.F
  archv2restart.f
  bigrid.f
  blkin.f 
  extrct.f
  extrot.f
  fordate.f
  getdat.f
  getdtm.f
  getdepth.f
  putdat.f
  indxi.f
  indxj.f
  pakk.f
  zebra.f
  dum_gks.f)

set(exe_name hafs_archv2restart)

set(exe_dir ${CMAKE_SOURCE_DIR}/exec)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -convert big_endian -DLINUX -fp-model precise -assume byterecl")
endif()

add_executable(${exe_name} ${fortran_srcs})

add_definitions( -DLITTLE_ENDIAN -DWRF -DLINUX -DMPI -DSERIAL_IO -DNAN2003 -DTIMER -DRELO )

install(TARGETS ${exe_name} DESTINATION ${exe_dir})
