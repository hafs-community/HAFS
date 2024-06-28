#!/bin/bash
set -xeu

HOMEhafs="../../../../"
source ${HOMEhafs}/sorc/machine-setup.sh.inc > /dev/null 2>&1

module use ${HOMEhafs}/modulefiles
module load modulefile.hafs.${target}
module list

BuildDir=${HOMEhafs}/sorc/hafs_tools.fd/sorc/build
if [ -d ${BuildDir} ]; then
   rm -rf ${BuildDir}
fi
mkdir ${BuildDir}
cd ${BuildDir}

cmake ../hafs_datool  -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc -DBUILD_TYPE=RELEASE
make VERBOSE=3
make install

# ./build_datool_CMake.bash > build.log 2>&1 &
