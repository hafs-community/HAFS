#!/bin/sh
#set -eux
set -x

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

if [ $target = hera ] || [ $target = orion ] || [ $target = jet ]; then
    #source ../modulefiles/modulefile.tools.$target > /dev/null 2>&1
    module use ../modulefiles
    module load modulefile.tools.$target
    module list
    export FC=ifort
    export F90=ifort
    export CC=icc
    export MPIFC=mpif90
elif [ $target = wcoss_cray ]; then
    #source ../modulefiles/modulefile.tools.${target} > /dev/null 2>&1
    module use ../modulefiles
    module load modulefile.tools.${target}
    module load cmake/3.3.2
    module list
    export FC="ftn -static"
    export F90="ftn -free -static"
    export CC=icc
    export DM_FC="ftn -static"
    export DM_F90="ftn -free -static"
    export DM_CC="cc -static"
elif [ $target = wcoss_dell_p3 ]; then
    #source ../modulefiles/modulefile.tools.${target} > /dev/null 2>&1
    module use ../modulefiles
    module load modulefile.tools.${target}
    module list
    export FC=ifort
    export F90=ifort
    export CC=icc
else
    echo "Unknown machine = $target"
    exit 1
fi

#export NETCDF_INCLUDE=${NETCDF_INCLUDE:-"-I${NETCDF}/include"}
#export NETCDF_LDFLAGS=${NETCDF_LDFLAGS:-"-L${NETCDF}/lib -lnetcdf -lnetcdff"}
export NETCDF_INCLUDE="-I${NETCDF}/include"
export NETCDF_LDFLAGS="-L${NETCDF}/lib -lnetcdff -lnetcdf"
export HDF5_INCLUDE=${HDF5_INCLUDE:-"-I${HDF5}/include"}
#export HDF5_LDFLAGS=${HDF5_LDFLAGS:-"-L${HDF5}/lib -lhdf5_hl -lhdf5hl_fortran -lhdf5 -lhdf5_fortran"}
export HDF5_LDFLAGS=${HDF5_LDFLAGS:-"-L${HDF5}/lib -lhdf5_hl -lhdf5"}
export BUFR_LDFLAGS="${BUFR_LIBd}"

TOOLS_PATH=${cwd}/hafs_tools.fd

# Build the libraries in the tools
##cd ${TOOLS_PATH}/libsrc
##./build_libs_cmake.sh

# Build the tools programs
export TOOLS_INC=${TOOLS_PATH}/include
export TOOLS_INCLUDE="-I${TOOLS_PATH}/include"
export TOOLS_LIBDIR=${TOOLS_PATH}/lib
#cd ${TOOLS_PATH}/sorc
#make clean
#make

if [ -d "${TOOLS_PATH}/build" ]; then
  rm -rf ${TOOLS_PATH}/build
fi
mkdir ${TOOLS_PATH}/build
cd ${TOOLS_PATH}/build
if [ $target = wcoss_cray ]; then
  cmake .. -DCMAKE_Fortran_COMPILER=ftn -DCMAKE_C_COMPILER=cc
else
  cmake .. -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc
fi
#make -j 8
make -j 8 VERBOSE=1
make install

cd ${TOOLS_PATH}/sorc

./build_hafs_utils.sh

exit
