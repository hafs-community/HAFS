#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
cwd=$(pwd)

module use ../modulefiles
module load hafs.$target
module list

cd hafs_hycom_utils.fd/tools
if [ -d "build" ]; then
  rm -rf build
fi
mkdir build
cd build

CMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER:-ifort}
CMAKE_C_COMPILER=${CMAKE_C_COMPILER:-icc}

#export BUILD_TYPE=DEBUG
export BUILD_TYPE=RELEASE
export BUILD_TYPE=${BUILD_TYPE:-RELEASE}

cmake .. -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER} -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER} -DCMAKE_BUILD_TYPE=${BUILD_TYPE}
make -j 8 VERBOSE=1

cd ${cwd}/hafs_hycom_utils.fd
if [ -d "build" ]; then
  rm -rf build
fi
mkdir build
cd build
CMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER:-ifort}
CMAKE_C_COMPILER=${CMAKE_C_COMPILER:-icc}
cmake .. -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER} -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}  -DCMAKE_BUILD_TYPE=${BUILD_TYPE}
make -j 8 VERBOSE=1
make install

cd ../

exit
