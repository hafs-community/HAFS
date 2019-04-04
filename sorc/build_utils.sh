#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
else
  export MOD_PATH=${cwd}/lib/modulefiles
fi

export EXECpath=${cwd}/hafs_utils.fd/exec
mkdir -p ${EXECpath}
cd ${cwd}/hafs_utils.fd/ush
./build_libs_cmake.sh "$cwd/hafs_utils.fd"

export DEPLIBS_PATH=${cwd}/hafs_utils.fd

if [ $target = jet ]; then

    targetx=jet
    source ${cwd}/hafs_utils.fd/modulefiles/modulefile.hafs_utils.$target

    module list

    export FC=ifort
    export F90=ifort
    export CC=icc
    export NETCDF_INCLUDE="-I${NETCDF}/include"
    export NETCDF_LDFLAGS="-L${NETCDF}/lib -lnetcdff -lnetcdf"
    export BUFR_LDFLAGS="${BUFR_LIB8}"
fi

cd ${cwd}/hafs_utils.fd/sorc
make

exit

