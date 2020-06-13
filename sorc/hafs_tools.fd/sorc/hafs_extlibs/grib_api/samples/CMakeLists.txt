file( GLOB samples_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.tmpl" )

install( FILES ${samples_files}
         DESTINATION share/${PROJECT_NAME}/samples
         PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ )

foreach( tmpl ${samples_files} )
    list( APPEND GRIB_API_SAMPLES_FILES ${CMAKE_CURRENT_SOURCE_DIR}/${tmpl} )
endforeach()

set( GRIB_API_SAMPLES_DIR   ${CMAKE_CURRENT_SOURCE_DIR} PARENT_SCOPE )
set( GRIB_API_SAMPLES_FILES ${GRIB_API_SAMPLES_FILES}   PARENT_SCOPE )

# link to the samples in the build directory
file( MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/share/${PROJECT_NAME} )
if( NOT EXISTS "${CMAKE_BINARY_DIR}/share/${PROJECT_NAME}/samples" )
    execute_process( COMMAND "${CMAKE_COMMAND}" "-E" "create_symlink"
      "${CMAKE_CURRENT_SOURCE_DIR}"
      "${CMAKE_BINARY_DIR}/share/${PROJECT_NAME}/samples" )
endif()