# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

##############################################################################
#.rst:
#
# ecbuild_install_project
# =======================
#
# Set up packaging and export configuration. ::
#
#   ecbuild_install_project( NAME <name> [ DESCRIPTION <description> ] )
#
# Options
# -------
#
# NAME : required
#   project name used for packaging
#
# DESCRIPTION : optional
#   project description used for packaging
#
# Usage
# -----
#
# ``ecbuild_install_project`` should be called at the very end of any ecBuild
# project (only followed by ``ecbuild_print_summary``), sets up packaging of
# the project with cpack and exports the configuration and targets for other
# projects to use.
#
# Unless ECBUILD_SKIP_<PROJECT_NAME>_EXPORT is set, the following files are generated:
#
# :<project>-config.cmake:         default project configuration
# :<project>-config-version.cmake: project version number
# :<project>-import.cmake:         extra project configuration (optional)
# :<project>-targets.cmake:        exported targets
#
# For ``<project>-import.cmake`` to be exported to build and install tree,
# ``<project>-import.cmake`` or ``<project>-import.cmake.in`` must exist in
# the source tree. ``<project>-config.cmake.in`` and
# ``<project>-config-version.cmake.in`` can be provided in the source tree to
# override the default templates used to generate ``<project>-config.cmake``
# and ``<project>-config-version.cmake``.
#
# In DEVELOPER_MODE, the build tree location is also added to the CMake user
# package registry for top level projects.
#
# If the project is added as a subdirectory, the following CMake variables
# are set in the parent scope:
#
# :<PROJECT_NAME>_FOUND:            set to ``TRUE``
# :<PROJECT_NAME>_VERSION:          version string
# :<PROJECT_NAME>_FEATURES:         list of enabled features
# :<PROJECT_NAME>_HAVE_<FEATURE>:   set to 1 for each enabled features
#
##############################################################################

function( ecbuild_set_if_not_defined VAR VALUE )

    if(NOT DEFINED ${VAR})
        set( ${VAR} "${VALUE}" PARENT_SCOPE )
        # ecbuild_info("Variable not defined, setting ${VAR} => ${VALUE}")
    # else()
        # ecbuild_info("${VAR} == ${${VAR}}")
    endif()

endfunction()

macro( ecbuild_install_project )

    set( options )
    set( single_value_args NAME DESCRIPTION )
    set( multi_value_args  COMPONENTS )

    cmake_parse_arguments( _PAR "${options}" "${single_value_args}" "${multi_value_args}"  ${_FIRST_ARG} ${ARGN} )

    if(_PAR_UNPARSED_ARGUMENTS)
      ecbuild_critical("Unknown keywords given to ecbuild_install_project(): \"${_PAR_UNPARSED_ARGUMENTS}\"")
    endif()

    if( NOT _PAR_NAME  )
      ecbuild_critical("The call to ecbuild_install_project() doesn't specify the NAME.")
    endif()

    ### PACKAGING ########################################################

    set( PNAME ${PROJECT_NAME_CAPS} )
    set( LNAME ${PROJECT_NAME_LOWCASE} )

    # components

    #    if( DEFINED _PAR_COMPONENTS )
    #        set(CPACK_COMPONENTS_ALL   "${_PAR_COMPONENTS}")
    #    else()
    #        set(CPACK_COMPONENTS_ALL   "${PROJECT_NAME}")
    #    endif()

    # name, version, etc ...

    ecbuild_set_if_not_defined(CPACK_PACKAGE_NAME      "${_PAR_NAME}")
    ecbuild_set_if_not_defined(CPACK_PACKAGE_VERSION   "${${PROJECT_NAME}_VERSION}")
    # Convert "/" to "-" for the case where the version string contains a "/"
    string( REPLACE "/" "-" CPACK_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION}" )

    ecbuild_set_if_not_defined(CPACK_PACKAGE_FILE_NAME   "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}-${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")

    #    set(CPACK_DEBIAN_PACKAGE_MAINTAINER "ECMWF") # required for DEB
    #    set(CPACK_ARCHIVE_COMPONENT_INSTALL "ON")
    #    set(CPACK_RPM_COMPONENT_INSTALL "ON")

    ecbuild_set_if_not_defined(CPACK_SOURCE_GENERATOR "TGZ")
    ecbuild_set_if_not_defined(CPACK_GENERATOR "TGZ")
    ecbuild_set_if_not_defined(CPACK_PACKAGE_VENDOR "ECMWF")

    # short description

    if( _PAR_DESCRIPTION )
        ecbuild_set_if_not_defined(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${_PAR_DESCRIPTION}" )
    else()
        ecbuild_set_if_not_defined(CPACK_PACKAGE_DESCRIPTION_SUMMARY "${_PAR_NAME} misses a description" )
    endif()

    # long description

    if( EXISTS ${PROJECT_SOURCE_DIR}/INSTALL )
        ecbuild_set_if_not_defined(CPACK_PACKAGE_DESCRIPTION_FILE "${PROJECT_SOURCE_DIR}/INSTALL")
    endif()
    if( EXISTS ${PROJECT_SOURCE_DIR}/LICENSE )
        ecbuild_set_if_not_defined(CPACK_RESOURCE_FILE_LICENSE    "${PROJECT_SOURCE_DIR}/LICENSE")
    endif()

    # set(CPACK_PACKAGE_EXECUTABLES ${ECBUILD_ALL_EXES})

    list( APPEND CPACK_SOURCE_INSTALLED_DIRECTORIES
          "${PROJECT_SOURCE_DIR}" "."
          "${ECBUILD_MACROS_DIR}" "cmake/" )

    # what to pack and not

    set(CPACK_SOURCE_IGNORE_FILES
        /build/
        /\\\\.git/
        /\\\\.svn/
        CMakeLists.txt.user
        \\\\.swp$
        p4config
    )

    # skip the files that were declared as DONT_PACK

    list( APPEND CPACK_SOURCE_IGNORE_FILES ${ECBUILD_DONT_PACK_FILES} )

    # Find the ecbuild toolchain files and include in the source package if found
    find_path( ECBUILD_TOOLCHAIN_DIR ecmwf-XC30-GNU.cmake
               PATHS ${ECBUILD_MACROS_DIR}/../toolchains
                     ${ECBUILD_MACROS_DIR}/../share/ecbuild/toolchains )

    mark_as_advanced( ECBUILD_TOOLCHAIN_DIR )

    if( ECBUILD_TOOLCHAIN_DIR )
      list( APPEND CPACK_SOURCE_INSTALLED_DIRECTORIES "${ECBUILD_TOOLCHAIN_DIR}" "share/ecbuild/toolchains/" )
    endif()

    # Find the ecbuild bin directory and include in the source package if found
    find_program( ECBUILD_SCRIPT ecbuild
                  PATHS ${ECBUILD_MACROS_DIR}/../bin
                        ${ECBUILD_MACROS_DIR}/../../../bin )

    mark_as_advanced( ECBUILD_SCRIPT )

    if( ECBUILD_SCRIPT )
      get_filename_component( ECBUILD_BIN_DIR ${ECBUILD_SCRIPT} PATH )
      list( APPEND CPACK_SOURCE_INSTALLED_DIRECTORIES "${ECBUILD_BIN_DIR}" "bin/" )
    endif()

    # Find the ecbuild linker check files and include in the source package if found
    find_path( ECBUILD_LINKCHECK_DIR dso1.c
               PATHS ${ECBUILD_MACROS_DIR}/../check_linker
                     ${ECBUILD_MACROS_DIR}/../share/ecbuild/check_linker )

    mark_as_advanced( ECBUILD_LINKCHECK_DIR )

    if( ECBUILD_LINKCHECK_DIR )
      list( APPEND CPACK_SOURCE_INSTALLED_DIRECTORIES "${ECBUILD_LINKCHECK_DIR}" "share/ecbuild/check_linker/" )
    endif()

    # cpack config file

    # set(CPACK_INSTALL_CMAKE_PROJECTS "${${PROJECT_NAME}_BINARY_DIR}" "${PROJECT_NAME}" "${CPACK_COMPONENTS_ALL}" "*" )

    include( CPack )

    ### EXPORTS ########################################################

    ecbuild_enabled_features( ${PROJECT_NAME}_FEATURES )
    foreach( _f ${${PROJECT_NAME}_FEATURES} )
        set( ${PROJECT_NAME}_HAVE_${_f} True )
        if(ECBUILD_2_COMPAT AND NOT PROJECT_NAME_CAPS STREQUAL PROJECT_NAME)
            ecbuild_declare_compat( ${PROJECT_NAME_CAPS}_HAVE_${_f} ${PROJECT_NAME}_HAVE_${_f} )
        endif()
    endforeach()

    # Generate the project .cmake config files
    # All variables here must be (sub)project specific in order to work within bundles
    if ( ECBUILD_2_COMPAT AND DEFINED ECBUILD_SKIP_${PNAME}_EXPORT )
        if(ECBUILD_2_COMPAT_DEPRECATE)
            ecbuild_deprecate("ECBUILD_SKIP_${PNAME}_EXPORT is deprecated, please use ECBUILD_SKIP_${PROJECT_NAME}_EXPORT instead")
        endif()
        set(ECBUILD_SKIP_${PROJECT_NAME}_EXPORT ${ECBUILD_SKIP_${PNAME}_EXPORT})
    endif()
    if ( NOT ECBUILD_SKIP_${PROJECT_NAME}_EXPORT )

        set( _template_config "${ECBUILD_MACROS_DIR}/project-config.cmake.in" )
        if( EXISTS ${PROJECT_SOURCE_DIR}/${LNAME}-config.cmake.in )
            set( _template_config "${PROJECT_SOURCE_DIR}/${LNAME}-config.cmake.in" )
        endif()

        # project-config-version.cmake -- format ([0-9]+).([0-9]+).([0-9]+)

        set( PACKAGE_VERSION        "${${PROJECT_NAME}_VERSION}" )
        set( PACKAGE_GIT_SHA1       "${${PROJECT_NAME}_GIT_SHA1}" )
        set( PACKAGE_GIT_SHA1_SHORT "${${PROJECT_NAME}_GIT_SHA1_SHORT}" )

        include(CMakePackageConfigHelpers)
        write_basic_package_version_file("${PROJECT_BINARY_DIR}/${LNAME}-config-version.cmake"
            VERSION ${PACKAGE_VERSION} COMPATIBILITY AnyNewerVersion)

        install( FILES "${PROJECT_BINARY_DIR}/${LNAME}-config-version.cmake" DESTINATION "${INSTALL_CMAKE_DIR}" )

        # prepare imutable variables (don't depend on install path)

        if( ${PROJECT_NAME}_FEATURES )
          set( CONF_FEATURES ${${PROJECT_NAME}_FEATURES} )
        endif()

        if(ECBUILD_2_COMPAT)
            set( CONF_LIBRARIES ${${PROJECT_NAME}_ALL_LIBS} )
            if( ${PNAME}_LIBRARIES )
                set( CONF_LIBRARIES ${${PNAME}_LIBRARIES} )
            endif()

            set( CONF_TPLS ${${PNAME}_TPLS} )

            set( CONF_INCLUDE_DIRS "${PROJECT_SOURCE_DIR}" "${PROJECT_BINARY_DIR}" )
        endif()

        # Generate <project>-import.cmake (if it exists)

        set( CONF_IMPORT_FILE "${LNAME}-import.cmake" )

        # If <project>-import.cmake.in exist in source tree, configure it to
        # the build tree and install the configured version
        if( EXISTS "${PROJECT_SOURCE_DIR}/${CONF_IMPORT_FILE}.in" )
          ecbuild_debug( "Found ${PROJECT_SOURCE_DIR}/${CONF_IMPORT_FILE}.in - configuring to ${PROJECT_BINARY_DIR}/${CONF_IMPORT_FILE}" )
          configure_file( "${PROJECT_SOURCE_DIR}/${CONF_IMPORT_FILE}.in"
                          "${PROJECT_BINARY_DIR}/${CONF_IMPORT_FILE}" @ONLY )
          install( FILES "${PROJECT_BINARY_DIR}/${CONF_IMPORT_FILE}"
                   DESTINATION "${INSTALL_CMAKE_DIR}" )
        # Otherwise, if <project>-import.cmake exist in source tree, copy it to
        # the build tree and install it
        elseif( EXISTS "${PROJECT_SOURCE_DIR}/${CONF_IMPORT_FILE}" )
          ecbuild_debug( "Found ${PROJECT_SOURCE_DIR}/${CONF_IMPORT_FILE} - copying to ${PROJECT_BINARY_DIR}/${CONF_IMPORT_FILE}" )
          configure_file( "${PROJECT_SOURCE_DIR}/${CONF_IMPORT_FILE}"
                          "${PROJECT_BINARY_DIR}/${CONF_IMPORT_FILE}" COPYONLY )
          install( FILES "${PROJECT_SOURCE_DIR}/${CONF_IMPORT_FILE}"
                   DESTINATION "${INSTALL_CMAKE_DIR}" )
        else()
          ecbuild_debug( "No ${CONF_IMPORT_FILE} found in ${PROJECT_SOURCE_DIR}" )
        endif()

        # Generate the <project>-config.cmake
        ecbuild_generate_project_config("${_template_config}")

        if(ECBUILD_2_COMPAT)
            # Generate <project>-config.cmake.tpls (if there are any TPLs)
            ecbuild_compat_tplconfig("${PROJECT_BINARY_DIR}/${LNAME}-config.cmake.tpls" TPLS ${${PNAME}_TPLS})
        endif()

        # install the export
        if( ${PROJECT_NAME}_ALL_EXES OR ${PROJECT_NAME}_ALL_LIBS )
            install( EXPORT ${PROJECT_NAME}-targets
                     DESTINATION "${INSTALL_CMAKE_DIR}" )
        endif()

    endif()  # if ( NOT ECBUILD_SKIP_${PROJECT_NAME}_EXPORT )

    # exports the package for use from the build-tree but only in DEVELOPER_MODE
    # inserts <package> into the CMake user package registry

    if( PROJECT_NAME STREQUAL CMAKE_PROJECT_NAME )

        if( DEVELOPER_MODE )
            export( PACKAGE ${PROJECT_NAME} )
        endif()

    else()

        if(ECBUILD_2_COMPAT)
            # export variables for upper projects
            set( ${PROJECT_NAME}_FOUND TRUE )

            set( ${PROJECT_NAME}_FULL_INSTALL_CMAKE_DIR ${${PROJECT_NAME}_FULL_INSTALL_CMAKE_DIR} PARENT_SCOPE )

            set( ${PROJECT_NAME}_FOUND             ${${PROJECT_NAME}_FOUND}             PARENT_SCOPE )
            set( ${PROJECT_NAME}_VERSION           ${${PROJECT_NAME}_VERSION}           PARENT_SCOPE )
            set( ${PROJECT_NAME}_GIT_SHA1          ${${PROJECT_NAME}_GIT_SHA1}          PARENT_SCOPE )
            set( ${PROJECT_NAME}_GIT_SHA1_SHORT    ${${PROJECT_NAME}_GIT_SHA1_SHORT}    PARENT_SCOPE )
            set( ${PROJECT_NAME}_VERSION           ${${PROJECT_NAME}_VERSION}           PARENT_SCOPE )
            set( ${PROJECT_NAME}_FEATURES          ${${PROJECT_NAME}_FEATURES}          PARENT_SCOPE )
            foreach( _f ${${PROJECT_NAME}_FEATURES} )
                set( ${PROJECT_NAME}_HAVE_${_f} ${${PROJECT_NAME}_HAVE_${_f}} PARENT_SCOPE )
            endforeach()

            ecbuild_declare_compat( ${PNAME}_FULL_INSTALL_CMAKE_DIR  ${PROJECT_NAME}_FULL_INSTALL_CMAKE_DIR PARENT_SCOPE )
            ecbuild_declare_compat( ${PNAME}_FOUND ${PROJECT_NAME}_FOUND PARENT_SCOPE )
            ecbuild_declare_compat( ${PNAME}_VERSION ${PROJECT_NAME}_VERSION PARENT_SCOPE )
            ecbuild_declare_compat( ${PNAME}_GIT_SHA1 ${PROJECT_NAME}_GIT_SHA1 PARENT_SCOPE )
            ecbuild_declare_compat( ${PNAME}_GIT_SHA1_SHORT ${PROJECT_NAME}_GIT_SHA1_SHORT PARENT_SCOPE )
            ecbuild_declare_compat( ${PNAME}_FEATURES ${PROJECT_NAME}_FEATURES PARENT_SCOPE )
            foreach( _f ${${PROJECT_NAME}_FEATURES} )
                ecbuild_declare_compat( ${PNAME}_HAVE_${_f} ${PROJECT_NAME}_HAVE_${_f} PARENT_SCOPE )
            endforeach()

            set( ${PNAME}_LIBRARIES         ${${PNAME}_LIBRARIES}         PARENT_SCOPE )
            set( ${PNAME}_PACKAGES          ${${PNAME}_PACKAGES}          PARENT_SCOPE )
            set( ${PNAME}_TPLS              ${${PNAME}_TPLS}              PARENT_SCOPE )
        endif()

    endif()

    # Some libraries install no headers, and a ${CMAKE_INSTALL_PREFIX}/${INSTALL_INCLUDE_DIR}
    # may not have been created, although it is added to each library's public interface.
    # We therefore need to create the include directory regardless to avoid errors in downstream
    # libraries referencing this include directory. ( see ECBUILD-437 )
    if( ECBUILD_INSTALL_LIBRARY_HEADERS )
        install(CODE "file( MAKE_DIRECTORY \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${INSTALL_INCLUDE_DIR}\")" )
    endif()

endmacro( ecbuild_install_project )
