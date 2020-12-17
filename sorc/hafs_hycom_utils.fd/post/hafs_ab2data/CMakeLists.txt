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
  bigrid.f
  bigrd1.f 
  blkin.f
  dum_gks.f
  extrct.f
  extrot.f
  fordate.f
  getdat4rtofs.f
  getdat.f
  getdepth.f
  getdtm.f
  horout_nc.f
  indxi.f
  indxj.f
  layer2z.f
  pakk.f
  poisnd.f
  psmoo.f
  ab2grib.f
  sbmerg.f
  wtime.F
  zebra.f
  horout_nc.f
  layer2z.f
  ab2grib.f 
  gribit.f 
  grib_maps.f
  pot2pot0.f)

set(fortran_srcs1 ${fortran_srcs} archv2data2d.f)
set(fortran_srcs2 ${fortran_srcs} archv2data3z.f)
set(fortran_srcs3 ${fortran_srcs} archv3z2nc.f)

set(ilib_dir ${CMAKE_SOURCE_DIR}/libs/build/src)
set(exe_name1 hafs_archv2data2d)
set(exe_name2 hafs_archv2data3z)
set(exe_name3 hafs_archv3z2nc)

set(exe_dir ${CMAKE_SOURCE_DIR}/exec)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -convert big_endian -DLINUX -fp-model precise -assume byterecl")
endif()

add_executable(${exe_name1} ${fortran_srcs1})
add_executable(${exe_name2} ${fortran_srcs2})
add_executable(${exe_name3} ${fortran_srcs3})

add_definitions(-DLITTLE_ENDIAN -DWRF -DLINUX -DMPI -DSERIAL_IO -DNAN2003 -DTIMER -DRELO)

target_include_directories(${exe_name1} PUBLIC 
  ${ilib_dir}/ofs_mods
  ${CMAKE_SOURCE_DIR}/post/hafs_ab2data)
target_include_directories(${exe_name2} PUBLIC 
  ${ilib_dir}/ofs_mods
  ${CMAKE_SOURCE_DIR}/post/hafs_ab2data)
target_include_directories(${exe_name3} PUBLIC  
  ${ilib_dir}/ofs_mods
  ${CMAKE_SOURCE_DIR}/post/hafs_ab2data)

target_link_libraries(
  ${exe_name1}
  ${ilib_dir}/ofs_mods/libofs_mods.a
  NetCDF::NetCDF_Fortran
  w3nco_4
  w3emc_4
  bacio_4)
target_link_libraries(
  ${exe_name2}
  ${ilib_dir}/ofs_mods/libofs_mods.a
  NetCDF::NetCDF_Fortran
  w3nco_4
  w3emc_4
  bacio_4)
target_link_libraries(
  ${exe_name3}
  ${ilib_dir}/ofs_mods/libofs_mods.a
  NetCDF::NetCDF_Fortran
  w3nco_4
  w3emc_4
  bacio_4)


install(TARGETS ${exe_name1} DESTINATION ${exe_dir})
install(TARGETS ${exe_name2} DESTINATION ${exe_dir})
install(TARGETS ${exe_name3} DESTINATION ${exe_dir})
