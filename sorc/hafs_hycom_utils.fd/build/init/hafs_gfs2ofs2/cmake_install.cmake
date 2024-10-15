# Install script for directory: /work2/noaa/uswrp-uu/rfeng/software/Orion_HAFS/sorc/hafs_hycom_utils.fd/init/hafs_gfs2ofs2

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "RELEASE")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "0")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

# Set default install directory permissions.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}/work2/noaa/uswrp-uu/rfeng/software/Orion_HAFS/sorc/hafs_hycom_utils.fd/exec/hafs_gfs2ofs2" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}/work2/noaa/uswrp-uu/rfeng/software/Orion_HAFS/sorc/hafs_hycom_utils.fd/exec/hafs_gfs2ofs2")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}/work2/noaa/uswrp-uu/rfeng/software/Orion_HAFS/sorc/hafs_hycom_utils.fd/exec/hafs_gfs2ofs2"
         RPATH "")
  endif()
  list(APPEND CMAKE_ABSOLUTE_DESTINATION_FILES
   "/work2/noaa/uswrp-uu/rfeng/software/Orion_HAFS/sorc/hafs_hycom_utils.fd/exec/hafs_gfs2ofs2")
  if(CMAKE_WARN_ON_ABSOLUTE_INSTALL_DESTINATION)
    message(WARNING "ABSOLUTE path INSTALL DESTINATION : ${CMAKE_ABSOLUTE_DESTINATION_FILES}")
  endif()
  if(CMAKE_ERROR_ON_ABSOLUTE_INSTALL_DESTINATION)
    message(FATAL_ERROR "ABSOLUTE path INSTALL DESTINATION forbidden (by caller): ${CMAKE_ABSOLUTE_DESTINATION_FILES}")
  endif()
  file(INSTALL DESTINATION "/work2/noaa/uswrp-uu/rfeng/software/Orion_HAFS/sorc/hafs_hycom_utils.fd/exec" TYPE EXECUTABLE FILES "/work2/noaa/uswrp-uu/rfeng/software/Orion_HAFS/sorc/hafs_hycom_utils.fd/build/init/hafs_gfs2ofs2/hafs_gfs2ofs2")
  if(EXISTS "$ENV{DESTDIR}/work2/noaa/uswrp-uu/rfeng/software/Orion_HAFS/sorc/hafs_hycom_utils.fd/exec/hafs_gfs2ofs2" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}/work2/noaa/uswrp-uu/rfeng/software/Orion_HAFS/sorc/hafs_hycom_utils.fd/exec/hafs_gfs2ofs2")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}/work2/noaa/uswrp-uu/rfeng/software/Orion_HAFS/sorc/hafs_hycom_utils.fd/exec/hafs_gfs2ofs2"
         OLD_RPATH "/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/netcdf-fortran-4.6.1-6273464/lib:/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/netcdf-c-4.9.2-oup2wyi/lib:/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/libpng-1.6.37-3rl5jsj/lib64:/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/zlib-1.2.13-f3she4o/lib:/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/jasper-2.0.32-iocstey/lib64:/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/unified-env-rocky9/install/intel/2021.9.0/libjpeg-turbo-2.1.0-5e3tjlw/lib64:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}/work2/noaa/uswrp-uu/rfeng/software/Orion_HAFS/sorc/hafs_hycom_utils.fd/exec/hafs_gfs2ofs2")
    endif()
  endif()
endif()

