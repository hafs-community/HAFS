# examples/python/CMakeLists.txt

# Configure the file which all CMake tests will include
configure_file( include.ctest.sh.in  include.ctest.sh  @ONLY )

execute_process( COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/include.sh ${CMAKE_CURRENT_BINARY_DIR} )

# Build the executables used by test scripts
################################################
list( APPEND test_bins
   keys_iterator
   print_data
   iterator
   count_messages
)

foreach( tool ${test_bins} )
   ecbuild_add_executable( TARGET    p_${tool}
                           NOINSTALL
                           SOURCES   ${tool}.c
                           CONDITION HAVE_PYTHON
                           LIBS      grib_api
   )
   list( APPEND ptools  p_${tool} )
endforeach()


# Now add each test
#################################################
list( APPEND tests
   clone
   count_messages
   get
   index
   iterator
   keys_iterator
   multi_write
   nearest
   print_data
   samples
   set
   set_missing
   binary_message
   set_bitmap
)

# The high level python test requires new features in the unittest
# which are only there for python 2.7 onwards
#if( PYTHON_VERSION_STRING VERSION_GREATER "2.7" )
#    ecbuild_info("Python examples: Adding test for PythonicGrib")
#    list( APPEND tests high_level_api )
#endif()

foreach( test ${tests} )
    ecbuild_add_test( TARGET grib_api_p_${test}_test
                      TYPE       SCRIPT
                      DEPENDS    ${ptools}
                      COMMAND    ${CMAKE_CURRENT_SOURCE_DIR}/${test}.sh
                      CONDITION  HAVE_PYTHON
                      ENVIRONMENT PYTHON=${PYTHON_EXECUTABLE}
                      TEST_DEPENDS grib_api_download_gribs grib_api_download_tigge_gribs
    )
endforeach()
