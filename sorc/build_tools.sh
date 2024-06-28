#!/bin/sh
set -xeu
source ./machine-setup.sh.inc > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi

#Supports Debug or Release modes for the build
BUILD_MODE=${BUILD_MODE:-Release}

#Explicitly pass to linker that executable stack is not needed
USE_NOEXECSTACK=${USE_NOEXECSTACK:-ON}
export USE_NOEXECSTACK

cwd=$(pwd)

export target=${target}
module use ../modulefiles
module load hafs.${target}
module list

if [ $target = hera ] || [ $target = orion ] || [ $target = jet ] || [ $target = hercules ]; then
  export USE_NOEXECSTACK=OFF
fi

if [ $target = wcoss2 ]; then
  export NETCDF_INCLUDE="-I${NETCDF}/include"
  export NETCDF_LDFLAGS="-L${NETCDF}/lib -lnetcdff -lnetcdf"
  export HDF5_INCLUDE=${HDF5_INCLUDE:-"-I${HDF5_INCLUDES:--I${HDF5}/include}"}
  export HDF5_LDFLAGS=${HDF5_LDFLAGS:-"-L${HDF5_LIBRARIES:-${HDF5}/lib} -lhdf5_hl -lhdf5"}
  export BUFR_LDFLAGS="${BUFR_LIB4}"
else
  export NETCDF_INCLUDE="-I${netcdf_fortran_ROOT}/include -I${netcdf_c_ROOT}/include"}
  export NETCDF_LDFLAGS="-L${netcdf_fortran_ROOT}/lib -lnetcdff -L${netcdf_c_ROOT}/lib -lnetcdf"}
  export HDF5_INCLUDE=${HDF5_INCLUDE:-"-I${HDF5_INCLUDES:--I${hdf5_ROOT}/include}"}
  export HDF5_LDFLAGS=${HDF5_LDFLAGS:-"-L${HDF5_LIBRARIES:-${hdf5_ROOT}/lib} -lhdf5_hl -lhdf5"}
  export BUFR_LDFLAGS="${BUFR_LIB4}"
fi

TOOLS_PATH=${cwd}/hafs_tools.fd
export TOOLS_INC=${TOOLS_PATH}/include
export TOOLS_INCLUDE="-I${TOOLS_PATH}/include"
export TOOLS_LIBDIR=${TOOLS_PATH}/lib
if [ -d "${TOOLS_PATH}/build" ]; then
  rm -rf ${TOOLS_PATH}/build
fi
mkdir ${TOOLS_PATH}/build
cd ${TOOLS_PATH}/build

if [ "${BUILD_MODE}" = Release ]; then
  export BUILD_TYPE=RELEASE
else
  export BUILD_TYPE=DEBUG
fi
cmake .. -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER} -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER} -DBUILD_TYPE=${BUILD_TYPE}
make -j 8 VERBOSE=1
make install

cd ${TOOLS_PATH}/sorc

./build_hafs_utils.sh

