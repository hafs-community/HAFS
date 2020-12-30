
file(READ /work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/build/eccodes_f90.pc.tmp _content)

string(REPLACE "/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/build/lib" "\${libdir}" _content "${_content}")
if(NOT "lib" STREQUAL "lib")
  string(REPLACE "/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/build/lib" "\${libdir}" _content "${_content}")
endif()
string(REPLACE "/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/lib" "\${libdir}" _content "${_content}")

string(REGEX REPLACE "%SHORTEN:lib" "%SHORTEN:" _content "${_content}")
string(REGEX REPLACE "\\.(a|so|dylib|dll|lib)(\\.[0-9]+)*%" "%" _content "${_content}")
string(REGEX REPLACE "%SHORTEN:([^%]+)%" "\\1" _content "${_content}")

file(WRITE /work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/build/lib/pkgconfig/eccodes_f90.pc "${_content}")
