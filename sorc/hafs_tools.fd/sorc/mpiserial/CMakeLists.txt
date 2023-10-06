#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

set(cc_srcs
  mpiserial.c)

set(exe_name mpiserial.x)
set(exec_dir ${CMAKE_SOURCE_DIR}/exec)

add_definitions(-DLINUX -DUNDERSCORE -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong="long long")

add_executable(${exe_name} ${cc_srcs})

target_compile_options(${exe_name} PRIVATE 
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>:-g> -O0)

target_link_options(${exe_name} PRIVATE 
   $<$<STREQUAL:${BUILD_TYPE},DEBUG>:-g> -O0)

#target_include_directories(
#  ${exe_name} PUBLIC
#  ${MPI_C_INCLUDE_PATH})

target_link_libraries(
  ${exe_name} PUBLIC
  MPI::MPI_C)

install(TARGETS ${exe_name} DESTINATION ${exec_dir})
