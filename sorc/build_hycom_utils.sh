#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

if [ $target = wcoss_cray ]; then
  export DM_FC="ftn -static"
  export DM_F90="ftn -free -static"
  export DM_CC="cc -static"
fi

module use hafs_hycom_utils.fd/modulefiles
module load modulefile.hycom_utils.$target
module list

#export NETCDF_INCLUDE=${NETCDF_INCLUDE:-"-I${NETCDF}/include"}
#export NETCDF_LDFLAGS=${NETCDF_LDFLAGS:-"-L${NETCDF}/lib -lnetcdf -lnetcdff"}
## export NETCDF_INCLUDE="-I${NETCDF}/include"
## export NETCDF_LDFLAGS="-L${NETCDF}/lib -lnetcdff -lnetcdf"

##export HDF5_INCLUDE=${HDF5_INCLUDE:-"-I${HDF5}/include"}
#export HDF5_LDFLAGS=${HDF5_LDFLAGS:-"-L${HDF5}/lib -lhdf5_hl -lhdf5hl_fortran -lhdf5 -lhdf5_fortran"}
##export HDF5_LDFLAGS=${HDF5_LDFLAGS:-"-L${HDF5}/lib -lhdf5_hl -lhdf5"}

#export HDF5_LDFLAGS="-L${HDF5}/lib -lhdf5_hl -lhdf5hl_fortran -lhdf5 -lhdf5_fortran -lz";
#export NETCDF_LDFLAGS="-L${NETCDF}/lib -lnetcdff -lnetcdf -lz ${HDF5_LDFLAGS}";

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

# Ensure a clean compile:
#make -i clean
#find . -name '*.o' -o -name '*.a' -o -name '*.mod' | xargs rm -f
#rm -f ./exec/*

# Compile hycom_utils
#make -i -f makefile all

cd ../

exit
