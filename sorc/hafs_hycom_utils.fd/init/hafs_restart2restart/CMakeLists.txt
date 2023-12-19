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

if(NOT CMAKE_BUILD_TYPE)                                                                             
    set(CMAKE_BUILD_TYPE "RELEASE")                                                                  
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -traceback -convert big_endian -DLINUX -fp-model precise -assume byterecl")
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -traceback -check all -ftrapuv -convert big_endian -DLINUX -fp-model precise -assume byterecl")
endif()

if(CMAKE_C_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O0")
  set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0 -g")
endif()

add_executable(${exe_name} ${fc_srcs})

add_definitions( -DUNDERSCORE -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong="long long" -DLITTLE_ENDIAN -DWRF -DLINUX -DMPI -DSERIAL_IO -DNAN2003 -DTIMER -DRELO )

install(TARGETS ${exe_name} DESTINATION ${exe_dir})
