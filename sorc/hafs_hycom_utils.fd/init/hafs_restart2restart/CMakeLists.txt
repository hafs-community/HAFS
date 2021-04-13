#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

set(fc_srcs
    hycom_subset.F
    hycom_subset.F
    parse.c)

set(exe_name hafs_restart2restart)

set(exe_dir ${CMAKE_SOURCE_DIR}/exec)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -convert big_endian -DLINUX -fp-model precise -assume byterecl")
endif()
if(CMAKE_C_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -cc=icc -O0")
endif()

add_executable(${exe_name} ${fc_srcs})

add_definitions( -DUNDERSCORE -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong="long long" -DLITTLE_ENDIAN -DWRF -DLINUX -DMPI -DSERIAL_IO -DNAN2003 -DTIMER -DRELO )

install(TARGETS ${exe_name} DESTINATION ${exe_dir})
