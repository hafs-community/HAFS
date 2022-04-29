#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================


set(fortran_srcs
    modules.f split.f BOUND.f fft99.f sig_p_convt.f 
    SEPAR.f WNLIT.f FDUMP.f H12.f I1MACH.f J4SAVE.f XGETUA.f 
    WNLSM.f WNNLS.f XERABT.f XERCTL.f XERPRT.f XERROR.f XERRWV.f 
    XERSAV.f srotm.f srotmg.f amatrix.f rodist.f landcmsk.f 
    create_rel_domain.f)


set(exe_name hafs_vi_split.x)
set(exec_dir ${CMAKE_CURRENT_SOURCE_DIR}/../../../exec)

add_executable(${exe_name} ${fortran_srcs})

target_compile_options(${exe_name} PRIVATE 
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>:-g -check all> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O3>
    -traceback -fp-model precise -assume byterecl -convert big_endian -integer-size 32 -real-size 64 )

target_link_options(${exe_name} PRIVATE 
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>:-g -check all> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O3>
    -traceback -fp-model precise -assume byterecl -convert big_endian -integer-size 32 -real-size 64 )

target_link_libraries(
  ${exe_name} PRIVATE
  OpenMP::OpenMP_Fortran
  ${MKL_LIBRARIES})

install(TARGETS ${exe_name} DESTINATION ${exec_dir})
