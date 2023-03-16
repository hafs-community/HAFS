#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

set(fc_srcs
    sia_cmdarg.f90
    sia_const.f90
    sia_dir.f90
    sia_fileop.f90
    sia_glob.f90
    sia_stat.f90
    sia_subprocess.f90
    sia_time.f90
    sia_treewalk.f90
    sia_mpi_worklist.f90
    sia_usrgrp.f90
    sia_c_dir.c
    sia_c_fileop.c
    sia_c_glob.c
    sia_c_stat.c
    sia_c_subprocess.c 
    sia_c_time.c 
    sia_c_usrgrp.c)

set(lib_name sia) 
set(lib_dir ${CMAKE_SOURCE_DIR}/libs)

if(NOT CMAKE_BUILD_TYPE)                                                                             
    set(CMAKE_BUILD_TYPE "RELEASE")                                                                  
endif()

if(CMAKE_CXX_COMPILER_ID MATCHES "^(Intel)$")
    set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -free -O3 -convert big_endian -DLINUX -fp-model precise -assume byterecl")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -free -O0 -g -traceback -check all -convert big_endian -DLINUX -fp-model precise -assume byterecl")
endif()

add_library(${lib_name} STATIC ${fc_srcs})
target_link_libraries(
    ${lib_name} 
    MPI::MPI_Fortran)

install(TARGETS ${lib_name} DESTINATION ${lib_dir})
