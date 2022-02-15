#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================


set(fortran_srcs
    anl_combine.f90
    split_sym_asy.f90
    correct_mat.f90
    interp_coef_agrid.f90
    fill_nmm_gridg.f90)

set(exe_name hafs_vi_anl_combine.x)
set(exec_dir ${CMAKE_CURRENT_SOURCE_DIR}/../../../exec)

add_executable(${exe_name} ${fortran_srcs})

target_compile_options(${exe_name} PRIVATE 
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>:-g -check all> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O3>
    -traceback -fp-model precise -assume byterecl -convert big_endian)

target_link_options(${exe_name} PRIVATE 
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>:-g -check all> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O3>
    -traceback -fp-model precise -assume byterecl -convert big_endian)

target_link_libraries(
  ${exe_name} PRIVATE
  OpenMP::OpenMP_Fortran)

install(TARGETS ${exe_name} DESTINATION ${exec_dir})
