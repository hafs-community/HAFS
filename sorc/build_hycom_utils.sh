#!/bin/sh
set -xeu
source ./machine-setup.sh.inc > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi

#Supports Debug or Release modes for the build 
BUILD_MODE=${BUILD_MODE:-Release} 

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

if [ "${BUILD_MODE}" = Release ]; then
  export BUILD_TYPE=RELEASE
else
  export BUILD_TYPE=DEBUG
fi
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

