#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

set(cc_srcs
  mpiserial.c)

set(exe_name mpiserial.x)
set(exec_dir ${CMAKE_SOURCE_DIR}/exec)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O0 -DLINUX -DUNDERSCORE -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'")
add_executable(${exe_name} ${cc_srcs})

#target_include_directories(
#  ${exe_name} PUBLIC
#  ${MPI_C_INCLUDE_PATH})

target_link_libraries(
  ${exe_name} PUBLIC
  MPI::MPI_C)

install(TARGETS ${exe_name} DESTINATION ${exec_dir})
