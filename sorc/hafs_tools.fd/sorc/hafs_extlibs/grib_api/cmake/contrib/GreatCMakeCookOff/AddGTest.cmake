# CMake arguments for gtest.
set(GTEST_CMAKE_ARGS 
      -DBUILD_SHARED_LIBS=OFF
      -Dgtest_force_shared_crt=ON
      -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
      -DCMAKE_C_FLAGS=${CMAKE_C_FLAGS}
      -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
      -DCMAKE_CXX_FLAGS=${CMAKE_CXX_FLAGS}
      -DCMAKE_CXX_FLAGS_DEBUG=${CMAKE_CXX_FLAGS_DEBUG}
      -DCMAKE_CXX_FLAGS_RELWITHDEBINFO=${CMAKE_CXX_FLAGS_RELWIDTHDEBINFO}
      -DCMAKE_CXX_FLAGS_RELEASE=${CMAKE_CXX_FLAGS_RELEASE}
      -DCMAKE_CXX_FLAGS_MINSIZEREL=${CMAKE_CXX_FLAGS_MINSIZEREL})
if(MINGW)
  list(APPEND GTEST_CMAKE_ARGS -Dgtest_disable_pthreads=ON)
else(MINGW)
  find_package(Threads)
endif(MINGW)


# Add gtest
if(NOT EXTERNAL_ROOT)
  set(EXTERNAL_ROOT ${PROJECT_BINARY_DIR}/external)
endif(NOT EXTERNAL_ROOT)
include(ExternalProject)
ExternalProject_Add(
    googletest
    PREFIX ${EXTERNAL_ROOT}
    SVN_REPOSITORY http://googletest.googlecode.com/svn/trunk/
    TIMEOUT 10
    # Force separate output paths for debug and release builds to allow easy
    # identification of correct lib in subsequent TARGET_LINK_LIBRARIES commands
    CMAKE_ARGS ${GTEST_CMAKE_ARGS}
    # Disable install step
    INSTALL_COMMAND ""
    # Wrap download, configure and build steps in a script to log output
    LOG_DOWNLOAD ON
    LOG_CONFIGURE ON
    LOG_BUILD ON)

if(PROJECT_USES_CPP11)
  add_definitions(-DGTEST_LANG_CXX11)
endif(PROJECT_USES_CPP11)

macro(add_gtest name source)

  ExternalProject_Get_Property(googletest source_dir)
  include_directories(${source_dir}/include)
  # Better, but only works on CMake 2.8.6?
  # get_target_property(THISTEST_INCLUDE test_${name} INCLUDE_DIRECTORIES)
  # set_target_properties(test_${name} PROPERTIES INCLUDE_DIRECTORIES
  #                       "${source_dir}/include;${THISTEST_INCLUDE}") 

  add_executable(test_${name} ${source})
  ExternalProject_Get_Property(googletest binary_dir)
  if(MSVC)
    target_link_libraries(test_${name} ${binary_dir}/${CMAKE_CFG_INTDIR}/gtest.lib)
  else(MSVC)
    target_link_libraries(test_${name} ${binary_dir}/libgtest.a)
  endif(MSVC)
  if(CMAKE_THREAD_LIBS_INIT)
    target_link_libraries(test_${name} ${CMAKE_THREAD_LIBS_INIT})
  endif(CMAKE_THREAD_LIBS_INIT)

  add_dependencies(test_${name} googletest)
  if(NOT "${ARGN}" STREQUAL "")
    target_link_libraries(test_${name} ${ARGN})
  endif(NOT "${ARGN}" STREQUAL "")

  add_test(cxx_${name} ${EXECUTABLE_OUTPUT_PATH}/test_${name}
              --gtest_output=xml:${CMAKE_BINARY_DIR}/test-results/test_${name}.xml)
  set_tests_properties(cxx_${name} PROPERTIES LABELS "gtest")
endmacro()
