#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

module use ../modulefiles
module load modulefile.hafs.$target
module list

if [ $target = hera ]; then
  export FC=ifort
  export F90=ifort
  export CC=icc
elif [ $target = orion ]; then
  export FC=ifort
  export F90=ifort
  export CC=icc
elif [ $target = jet ]; then
  export FC=ifort
  export F90=ifort
  export CC=icc
elif [ $target = wcoss_cray ]; then
  export FC=ftn
  export F90=ftn
  export CC=icc
elif [ $target = wcoss_dell_p3 ]; then
  export FC=ifort
  export F90=ifort
  export CC=icc
else
  echo "Unknown machine = $target"
  exit 1
fi

cd hafs_vortextracker.fd
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
