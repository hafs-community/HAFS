#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

module use ../modulefiles
module load modulefile.hafs.$target
module list

cd hafs_hycom_utils.fd/libs
if [ -d "build" ]; then
  rm -rf build
fi
mkdir build
cd build

CMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER:-ifort}
CMAKE_C_COMPILER=${CMAKE_C_COMPILER:-icc}
cmake .. -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER} -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
make -j 8 VERBOSE=1

cd ${cwd}/hafs_hycom_utils.fd
if [ -d "build" ]; then
  rm -rf build
fi
mkdir build
cd build
CMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER:-ifort}
CMAKE_C_COMPILER=${CMAKE_C_COMPILER:-icc}
cmake .. -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER} -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
make -j 8 VERBOSE=1
make install

cd ../

exit
