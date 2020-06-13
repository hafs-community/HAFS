function(_args_have_option outvar optionname arglist)
  list(FIND ${arglist} "${optionname}" HASOPTION)
  if(HASOPTION EQUAL -1)
    set(${outvar} False PARENT_SCOPE)
  else()
    set(${outvar} True PARENT_SCOPE)
    list(REMOVE_ITEM ${arglist} ${optionname})
    set(${arglist} ${${arglist}} PARENT_SCOPE)
  endif()
endfunction(_args_have_option)

function(cmake_test testname)

  # Parse further arguments
  # Let caller create a source file
  set(ALL_OPTIONS ${ARGN})
  _args_have_option(SOURCE "SOURCE" ALL_OPTIONS)
  # Let caller create executable to run
  # Should be used with test-command
  _args_have_option(NOEXEC "NOEXEC" ALL_OPTIONS)

  # set source and build dir.
  set(FAKE_PROJECT_DIR ${CMAKE_CURRENT_BINARY_DIR}/${testname})
  set(BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR}/fake_project_builds/${testname})

  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${testname}.cmake 
                 ${FAKE_PROJECT_DIR}/CMakeData.cmake @ONLY)
  message(STATUS "[${testname}] project in ${FAKE_PROJECT_DIR}")

  if(NOT SOURCE)
    file(WRITE ${FAKE_PROJECT_DIR}/main.c "int main() { return 0; }" )
  endif(NOT SOURCE)
  file(WRITE ${FAKE_PROJECT_DIR}/CMakeLists.txt
       "cmake_minimum_required(VERSION 2.8.3 FATAL_ERROR)\n"
       "project(allfeatures)\n"
       "include(\"${FAKE_PROJECT_DIR}/CMakeData.cmake\")\n"
       "enable_language(C)\n"
       "if(NOT ${NOEXEC})\n"
       "  file(GLOB ALLFILES \${PROJECT_SOURCE_DIR}/*.c \${PROJECT_SOURCE_DIR}/*.cc)\n"
       "  add_executable(${testname} \${ALLFILES})\n"
       "endif(NOT ${NOEXEC})\n")
  
  
  if(EXISTS ${BUILD_DIR})
    file(REMOVE_RECURSE ${BUILD_DIR})
  endif(EXISTS ${BUILD_DIR})
  
  file(MAKE_DIRECTORY ${BUILD_DIR})
  
  add_test(cmake_test_${testname}
             ${CMAKE_CTEST_COMMAND} --build-and-test ${FAKE_PROJECT_DIR} ${BUILD_DIR}
                                    --build-generator ${CMAKE_GENERATOR}
                                    --build-makeprogram ${CMAKE_MAKE_PROGRAM}
                                    --build-project ${testname}
                                    --build-options -Dcookoff_path=${CMAKE_SOURCE_DIR}/..
                                    ${ALL_OPTIONS})

endfunction(cmake_test)
