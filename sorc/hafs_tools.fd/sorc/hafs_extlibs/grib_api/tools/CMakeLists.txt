# tools library
ecbuild_add_library( TARGET    grib_tools
                     TYPE      STATIC
                     NOINSTALL
                     SOURCES   grib_tools.c grib_options.c grib_tools.h
                     LIBS      grib_api )

# tools binaries
list( APPEND grib_tools_bins
             grib_keys grib_histogram grib_error grib_add big2gribex
             grib_debug grib_info grib_filter grib_ls grib_dump
             grib2ppm grib_set grib_get grib_get_data grib_copy grib_repair
             grib_packing grib_cmp grib_convert grib_distance grib_corruption_check
             grib_compare grib_parser grib_count grib_index_build 
             gg_sub_area_check grib_moments grib_to_json
             )

list( APPEND grib_tools_bins_EXTRA
             grib_check_gaussian_grid
             )

# Install generic tools
foreach( tool ${grib_tools_bins} )
    # here we use the fact that each tool has only one C file that matches its name
    ecbuild_add_executable( TARGET     ${tool}
                            SOURCES    ${tool}.c
                            LIBS       grib_tools )
endforeach()

# Install extra tools
# User must run cmake with -DGRIB_API_INSTALL_EXTRA_TOOLS=ON
foreach( tool ${grib_tools_bins_EXTRA} )
    ecbuild_add_executable( TARGET     ${tool}
                            SOURCES    ${tool}.c
                            CONDITION  GRIB_API_INSTALL_EXTRA_TOOLS
                            LIBS       grib_tools )
endforeach()

# grib to netcdf is optional
ecbuild_add_executable( TARGET     grib_to_netcdf
                        SOURCES    grib_to_netcdf.c
                        INCLUDES   ${NETCDF_INCLUDE_DIRS}
                        CONDITION  HAVE_NETCDF
                        LIBS       grib_tools ${NETCDF_LIBRARIES})


ecbuild_add_executable( TARGET     grib_list_keys
                        SOURCES    list_keys.c
                        LIBS       grib_tools  )


# grib1to2 script needs to be generated before installation
configure_file( grib1to2.in  grib1to2 )
install( FILES ${CMAKE_CURRENT_BINARY_DIR}/grib1to2
         DESTINATION ${INSTALL_BIN_DIR}
         PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
                     GROUP_EXECUTE GROUP_READ
                     WORLD_EXECUTE WORLD_READ )
