#=======================================================================
#$$$ CMAKEFILE DOCUMENTATION BLOCK
# Biju Thomas
# Email: biju.thomas@noaa.gov
#=======================================================================

cmake_minimum_required(VERSION 3.15)

project(
  SHIPS
  LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/Modules/")

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

if(NOT CMAKE_C_COMPILER_ID MATCHES "^(Intel|GNU)$")
  message(WARNING "Compiler not officially supported: ${CMAKE_C_COMPILER_ID}")
endif()

set(exec_dir ${CMAKE_SOURCE_DIR}/exec)

# -------------------------------------------------------------------
# Object libraries (common compiled objects)
# -------------------------------------------------------------------

add_library(dataio_obj OBJECT dataio.f)
add_library(gbland_obj OBJECT gbland.f)
add_library(upcase_obj OBJECT upcase.f)

add_library(diag_util_obj OBJECT diag_util.f)
target_link_libraries(diag_util_obj PRIVATE dataio_obj)

add_library(lsdiags_obj OBJECT lsdiags.f)
target_link_libraries(lsdiags_obj PRIVATE diag_util_obj gbland_obj)

# -------------------------------------------------------------------
# Executables
# -------------------------------------------------------------------

add_executable(hafs_ships_nameparse.x nameparse.f)

add_executable(hafs_ships_gridparse.x gridparse.f)

add_executable(hafs_ships_totaldiag.x
    totaldiag.f
    $<TARGET_OBJECTS:diag_util_obj>
    $<TARGET_OBJECTS:dataio_obj>
    $<TARGET_OBJECTS:upcase_obj>
)

add_executable(hafs_ships_inddiag.x
    inddiag.f
    $<TARGET_OBJECTS:diag_util_obj>
    $<TARGET_OBJECTS:dataio_obj>
    $<TARGET_OBJECTS:upcase_obj>
    $<TARGET_OBJECTS:lsdiags_obj>
    $<TARGET_OBJECTS:gbland_obj>
)

add_executable(hafs_ships_inddiagnull.x
    inddiagnull.f
    $<TARGET_OBJECTS:diag_util_obj>
    $<TARGET_OBJECTS:dataio_obj>
    $<TARGET_OBJECTS:upcase_obj>
)

add_executable(hafs_ships_getcenter.x
    getcenter.f
    $<TARGET_OBJECTS:diag_util_obj>
    $<TARGET_OBJECTS:dataio_obj>
    $<TARGET_OBJECTS:upcase_obj>
)


target_compile_options(lsdiags_obj PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
    -traceback)

target_compile_options(diag_util_obj PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
    -traceback)

target_compile_options(upcase_obj PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
    -traceback)

target_compile_options(gbland_obj PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
    -traceback)

target_compile_options(dataio_obj PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
    -traceback)

target_compile_options(hafs_ships_nameparse.x PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
    -traceback)

target_compile_options(hafs_ships_gridparse.x PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
    -traceback)

target_compile_options(hafs_ships_totaldiag.x PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
    -traceback)

target_compile_options(hafs_ships_inddiag.x PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
    -traceback)

target_compile_options(hafs_ships_inddiagnull.x PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
    -traceback)

target_compile_options(hafs_ships_getcenter.x PRIVATE
    $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
    -traceback)

target_link_options(hafs_ships_nameparse.x PRIVATE
     $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
     -traceback)

target_link_options(hafs_ships_gridparse.x PRIVATE
     $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
     -traceback)

target_link_options(hafs_ships_totaldiag.x PRIVATE
     $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
     -traceback)

target_link_options(hafs_ships_inddiag.x PRIVATE
     $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
     -traceback)

target_link_options(hafs_ships_inddiagnull.x PRIVATE
     $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
     -traceback)

target_link_options(hafs_ships_getcenter.x PRIVATE
     $<$<STREQUAL:${BUILD_TYPE},DEBUG>: -O0 -g -check all -ftrapuv -fpe0> $<$<STREQUAL:${BUILD_TYPE},RELEASE>:-O2>
     -traceback)

install(TARGETS hafs_ships_nameparse.x DESTINATION ${exec_dir})
install(TARGETS hafs_ships_gridparse.x DESTINATION ${exec_dir})
install(TARGETS hafs_ships_totaldiag.x DESTINATION ${exec_dir})
install(TARGETS hafs_ships_inddiag.x DESTINATION ${exec_dir})
install(TARGETS hafs_ships_inddiagnull.x DESTINATION ${exec_dir})
install(TARGETS hafs_ships_getcenter.x DESTINATION ${exec_dir})
