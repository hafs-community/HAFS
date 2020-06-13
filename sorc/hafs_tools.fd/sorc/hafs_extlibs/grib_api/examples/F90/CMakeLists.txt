# examples/F90/CMakeLists.txt

# Configure the file which all CMake tests will include
configure_file( include.ctest.sh.in  include.ctest.sh  @ONLY )

execute_process( COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/include.sh ${CMAKE_CURRENT_BINARY_DIR} )

# Build the executables used by test scripts
################################################
list( APPEND tests
      index
      copy_message
      get
      get_data
      get_pl
      get_pv
      keys_iterator
      multi_write
      multi
      nearest
      precision
      print_data
      set
      set_bitmap
      set_missing
      set_pv
      samples
      count_messages
      read_message
      read_from_file
      get_set_uuid
      clone
)

foreach( tool ${tests} )

    # Add the test which depends on the above executable
    ecbuild_add_test( TARGET grib_api_f_${tool}
                      SOURCES   ${tool}.f90
                      LINKER_LANGUAGE Fortran
                      LIBS   grib_api_f90 grib_api
                      CONDITION HAVE_FORTRAN
                      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/${tool}.sh
                      TEST_DEPENDS grib_api_download_gribs
    )
endforeach()

# Executables without a shell script
# TODO
ecbuild_add_executable( TARGET    f_new_from_file
                        NOINSTALL
                        SOURCES   new_from_file.f90
                        CONDITION HAVE_FORTRAN
                        LINKER_LANGUAGE Fortran
                        LIBS      grib_api_f90 grib_api
)
ecbuild_add_executable( TARGET    f_copy_namespace
                        NOINSTALL
                        SOURCES   copy_namespace.f90
                        CONDITION HAVE_FORTRAN
                        LINKER_LANGUAGE Fortran
                        LIBS      grib_api_f90 grib_api
)
ecbuild_add_executable( TARGET    f_set_gvc
                        NOINSTALL
                        SOURCES   set_gvc.f90
                        CONDITION HAVE_FORTRAN
                        LINKER_LANGUAGE Fortran
                        LIBS      grib_api_f90 grib_api
)


# # Note extra dependency
# ecbuild_add_test( TARGET grib_api_f_set_test
#                   TYPE SCRIPT
#                   DEPENDS set_gvc set
#                   COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/set.sh
# )
