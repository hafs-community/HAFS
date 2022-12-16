#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================


set(fortran_srcs
    modules.f90 split.f90 BOUND.f90 fft99.f90 sig_p_convt.f90 
    SEPAR.f90 WNLIT.f90 FDUMP.f90 H12.f90 I1MACH.f90 J4SAVE.f90 XGETUA.f90 
    WNLSM.f90 WNNLS.f90 XERABT.f90 XERCTL.f90 XERPRT.f90 XERROR.f90 XERRWV.f90 
    XERSAV.f90 srotm.f90 srotmg.f90 amatrix.f90 rodist.f90 landcmsk.f90 
    create_rel_domain.f90)


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
