#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

set(fortran_srcs
    rtofs_obs_read.F90)

set(exe_name hafs_rtofs_obs_read.x)
set(exec_dir ${CMAKE_SOURCE_DIR}/exec)

add_executable(${exe_name} ${fortran_srcs})

target_compile_options(${exe_name} PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
    -traceback -fp-model strict -no-fma -ftz -assume byterecl -convert big_endian -fixed -real-size 32 -i4)

target_link_options(${exe_name} PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
    -traceback -fp-model strict -no-fma -ftz -assume byterecl -convert big_endian -fixed -real-size 32 -i4)

install(TARGETS ${exe_name} DESTINATION ${exec_dir})
