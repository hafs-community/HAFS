#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi
cwd=$(pwd)

export target=${target}
module use ../modulefiles
module load hafs.${target}
module list

if [ $target = hera ] || [ $target = orion ] || [ $target = jet ]; then
  export FC=ifort
  export F90=ifort
  export CC=icc
  export MPIFC=mpif90
elif [ $target = wcoss2 ]; then
  export FC="ftn -static"
  export F90="ftn -free -static"
  export CC=icc
  export DM_FC="ftn -static"
  export DM_F90="ftn -free -static"
  export DM_CC="cc -static"
elif [[ $target =~ gaea ]]; then
  export FC="ftn"
  export F90="ftn"
  export CC=cc
  export DM_FC="ftn"
  export DM_F90="ftn"
  export DM_CC="cc"
else
  echo "Unknown machine = $target"
  exit 1
fi
export NETCDF="${NETCDF:-$netcdf_c_ROOT}"
export HDF5="${NETCDF:-$hdf5_ROOT}"
export NETCDF_INCLUDE="-I${NETCDF}/include"
export NETCDF_LDFLAGS="-L${NETCDF}/lib -lnetcdff -lnetcdf"
export HDF5_INCLUDE=${HDF5_INCLUDE:-"-I${HDF5_INCLUDES:--I${HDF5}/include}"}
export HDF5_LDFLAGS=${HDF5_LDFLAGS:-"-L${HDF5_LIBRARIES:-${HDF5}/lib} -lhdf5_hl -lhdf5"}
export BUFR_LDFLAGS="${BUFR_LIBd}"

TOOLS_PATH=${cwd}/hafs_tools.fd
export TOOLS_INC=${TOOLS_PATH}/include
export TOOLS_INCLUDE="-I${TOOLS_PATH}/include"
export TOOLS_LIBDIR=${TOOLS_PATH}/lib
if [ -d "${TOOLS_PATH}/build" ]; then
  rm -rf ${TOOLS_PATH}/build
fi
mkdir ${TOOLS_PATH}/build
cd ${TOOLS_PATH}/build

#export BUILD_TYPE=DEBUG
#export BUILD_TYPE=RELEASE
export BUILD_TYPE=${BUILD_TYPE:-RELEASE}

cmake .. -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER} -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER} -DBUILD_TYPE=${BUILD_TYPE}
make -j 8 VERBOSE=1
make install

cd ${TOOLS_PATH}/sorc

./build_hafs_utils.sh

exit
