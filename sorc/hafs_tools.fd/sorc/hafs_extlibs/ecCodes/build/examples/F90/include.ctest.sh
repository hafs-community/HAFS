# examples/F90 include file for CMake

set -eax

proj_dir=/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/eccodes-2.16.0-Source
data_dir=/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/build/data

# use definitions from binary dir to test if installation will be correct
def_dir="/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/build/share/eccodes/definitions"
ECCODES_DEFINITION_PATH="${def_dir}"
export ECCODES_DEFINITION_PATH

tools_dir=/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/build/bin
examples_dir=/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/build/examples/F90

# If this environment variable is set, then run the
# executables with valgrind
if test "x$ECCODES_TEST_WITH_VALGRIND" != "x"; then
   tools_dir="valgrind --error-exitcode=1 -q $tools_dir"
   examples_dir="valgrind --error-exitcode=1 -q $examples_dir"
fi

# use samples from binary dir to test if installation will be correct
samp_dir="/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/build/share/eccodes/samples"
ECCODES_SAMPLES_PATH=${samp_dir}
export ECCODES_SAMPLES_PATH
