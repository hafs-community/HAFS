#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

set(fortran_srcs
   mod_plot.F
   mod_xc.F
   mod_za.F
   mod_zb.F
   wtime.F
   mod_ppsw.F
   bigrid.f
   blkin.f
   buoflx.f
   dum_gks.f
   extrct.f
   fordate.f
   getdat.f
   getdepth.f
   indxi.f
   indxj.f
   psmoo.f
   zebra.f
   zh.F
   horout_nc.f
   layer2z.f
   mixlay.f)

set(fortran_srcs1 ${fortran_srcs} archv2data2d.f)
set(fortran_srcs2 ${fortran_srcs} archv2data3z.f)

set(exe_name1 hafs_archv2ncdf2d)
set(exe_name2 hafs_archv2ncdf3z)

set(exe_dir ${CMAKE_SOURCE_DIR}/exec)

if(NOT CMAKE_BUILD_TYPE)                                                                             
    set(CMAKE_BUILD_TYPE "RELEASE")                                                                  
endif() 

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -traceback -convert big_endian -DLINUX -fp-model precise -assume byterecl")
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -traceback -check all -ftrapuv -convert big_endian -DLINUX -fp-model precise -assume byterecl")
endif()

add_executable(${exe_name1} ${fortran_srcs1})
add_executable(${exe_name2} ${fortran_srcs2})

add_definitions(-DLITTLE_ENDIAN -DWRF -DLINUX -DSERIAL_IO -DNAN2003 -DTIMER -DRELO)

target_link_libraries(
  ${exe_name1}
  NetCDF::NetCDF_Fortran
  NetCDF::NetCDF_C
  w3nco::w3nco_4
  w3emc::w3emc_4
  bacio::bacio_4)
target_link_libraries(
  ${exe_name2}
  NetCDF::NetCDF_Fortran
  NetCDF::NetCDF_C
  w3nco::w3nco_4
  w3emc::w3emc_4
  bacio::bacio_4)

install(TARGETS ${exe_name1} DESTINATION ${exe_dir})
install(TARGETS ${exe_name2} DESTINATION ${exe_dir})
