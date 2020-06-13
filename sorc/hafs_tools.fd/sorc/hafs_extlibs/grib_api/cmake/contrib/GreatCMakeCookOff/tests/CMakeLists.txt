cmake_minimum_required(VERSION 2.8.3 FATAL_ERROR)
project(COOKOFF_TEST)

enable_testing()

include(${PROJECT_SOURCE_DIR}/../TestCMake.cmake)

cmake_test(checkisnan SOURCE)
cmake_test(checkcpp11flags)
cmake_test(addgtest NOEXEC SOURCE --test-command ${CMAKE_MAKE_PROGRAM} test)

add_subdirectory(cpp11)
