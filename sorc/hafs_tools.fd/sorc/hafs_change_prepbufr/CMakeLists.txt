#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

cmake_minimum_required(VERSION 3.15)
project( 
  CHANGE_PREPBUFR
  LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules/")

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_C_COMPILER_ID}")
endif()

find_package(NetCDF REQUIRED C Fortran)
find_package(w3nco REQUIRED)
find_package(w3emc REQUIRED)
find_package(bufr REQUIRED)
find_package(MPI REQUIRED)

message("TYPE CMAKE_SOURCE_DIR = ${CMAKE_SOURCE_DIR}")
set(exec_dir ${CMAKE_SOURCE_DIR}/../../exec)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -msse2 -fp-model precise -assume byterecl -convert big_endian")

add_executable(hafs_change_prepbufr_rm_typ_in_circle.x change_prepbufr_rm_typ_in_circle.f)
add_executable(hafs_change_prepbufr_qm_in_circle.x change_prepbufr_qm_in_circle.f)
add_executable(hafs_change_prepbufr_qm_typ.x change_prepbufr_qm_typ.f)

target_link_libraries(
  hafs_change_prepbufr_rm_typ_in_circle.x
  w3emc::w3emc_4
  w3nco::w3nco_4
  bufr::bufr_4
  MPI::MPI_Fortran)
target_link_libraries(
  hafs_change_prepbufr_qm_in_circle.x
  w3emc::w3emc_4
  w3nco::w3nco_4
  bufr::bufr_4
  MPI::MPI_Fortran)
target_link_libraries(
  hafs_change_prepbufr_qm_typ.x
  w3emc::w3emc_4
  w3nco::w3nco_4
  bufr::bufr_4
  MPI::MPI_Fortran)

install(TARGETS hafs_change_prepbufr_rm_typ_in_circle.x DESTINATION ${exec_dir})
install(TARGETS hafs_change_prepbufr_qm_in_circle.x DESTINATION ${exec_dir})
install(TARGETS hafs_change_prepbufr_qm_typ.x DESTINATION ${exec_dir})
