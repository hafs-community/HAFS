file( GLOB definition_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.def" )
file( GLOB table_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.table" )

install( FILES ${definition_files} ${table_files}
         DESTINATION share/${PROJECT_NAME}/definitions
         PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ )

install( FILES installDefinitions.sh
         DESTINATION share/${PROJECT_NAME}/definitions)

install(  DIRECTORY budg cdf common grib1 grib2 gts mars tide hdf5 wrap
          DESTINATION share/${PROJECT_NAME}/definitions
          FILES_MATCHING
          PATTERN "*.def"
          PATTERN "*.table"
          PATTERN "4.2.192.*.table" EXCLUDE
          PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ )

# link to the definitions
file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/share/${PROJECT_NAME} )
if( NOT EXISTS "${CMAKE_BINARY_DIR}/share/${PROJECT_NAME}/definitions" )
  execute_process( COMMAND "${CMAKE_COMMAND}" "-E" "create_symlink"
    "${CMAKE_CURRENT_SOURCE_DIR}"
    "${CMAKE_BINARY_DIR}/share/${PROJECT_NAME}/definitions" )
endif()