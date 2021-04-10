#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

if [ $target = wcoss_cray ]; then
  export DM_FC="ftn -static"
  export DM_F90="ftn -free -static"
  export DM_CC="cc -static"
fi

module use ../modulefiles
module load modulefile.hafs.$target
module list

cd hafs_hycom_utils.fd/libs
if [ -d "build" ]; then
  rm -rf build
fi
mkdir build
cd build
if [ $target = wcoss_cray ]; then
  cmake .. -DCMAKE_Fortran_COMPILER=ftn -DCMAKE_C_COMPILER=cc
else
  cmake .. -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc
fi
make -j 8 VERBOSE=1

cd ${cwd}/hafs_hycom_utils.fd
if [ -d "build" ]; then
  rm -rf build
fi
mkdir build
cd build
if [ $target = wcoss_cray ]; then
  cmake .. -DCMAKE_Fortran_COMPILER=ftn -DCMAKE_C_COMPILER=cc
else
  cmake .. -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc
fi
make -j 8 VERBOSE=1
make install

cd ../

exit
