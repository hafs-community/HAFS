# examples/python include file for CMake

set -eax

data_dir=/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/build/data

# use definitions from binary dir to test if installation will be correct
def_dir="/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/build/share/eccodes/definitions"
ECCODES_DEFINITION_PATH="${def_dir}"
export ECCODES_DEFINITION_PATH

tools_dir=/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/build/bin
examples_dir=/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/build/examples/python
examples_src=/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/eccodes-2.16.0-Source/examples/python

# use samples from binary dir to test if installation will be correct
samp_dir="/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/build/share/eccodes/samples"
ECCODES_SAMPLES_PATH=${samp_dir}
export ECCODES_SAMPLES_PATH

PYTHONPATH=/work/noaa/hwrf/noscrub/bthomas/hafs_cmake_hpc_stack/sorc/hafs_tools.fd/sorc/hafs_extlibs/ecCodes/build/python3:$PYTHONPATH
export PYTHONPATH

echo "Current directory: `pwd`"
