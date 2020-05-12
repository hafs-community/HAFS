#! /usr/bin/env bash
#set -eux
set -x

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
else
  export MOD_PATH=${cwd}/lib/modulefiles
fi

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
else
  export MOD_PATH=${cwd}/lib/modulefiles
fi

if [ $target = wcoss ]; then

    echo "Does not support wcoss phase 1/2."
    exit 1

elif [ $target = hera ]; then

    targetx=hera
    #source ../modulefiles/modulefile.tools.$target > /dev/null 2>&1
    module use ../modulefiles
    module load modulefile.tools.$target
    module list

    export FC=ifort
    export F90=ifort
    export CC=icc

elif [ $target = orion ]; then

    targetx=orion
    #source ../modulefiles/modulefile.tools.$target > /dev/null 2>&1
    module use ../modulefiles
    module load modulefile.tools.$target
    module list

    export FC=ifort
    export F90=ifort
    export CC=icc

elif [ $target = jet ]; then

    targetx=jet
    #source ../modulefiles/modulefile.tools.$target > /dev/null 2>&1
    module use ../modulefiles
    module load modulefile.tools.$target
    module list

    export FC=ifort
    export F90=ifort
    export CC=icc

elif [ $target = wcoss_cray ]; then

    targetx=cray
    if [ $USE_PREINST_LIBS = true ]; then
      #source ../modulefiles/modulefile.tools.$target           > /dev/null 2>&1
      module use ../modulefiles
      module load modulefile.tools.$target
    else
      #source ../modulefiles/modulefile.tools.${target}_userlib > /dev/null 2>&1
      module use ../modulefiles
      module load modulefile.tools.${target}_userlib
    fi
    module load cmake/3.3.2
    module list

    export FC="ftn -static"
    export F90="ftn -free -static"
    export CC=icc

elif [ $target = wcoss_dell_p3 ]; then

    targetx=wcoss_dell_p3
    if [ $USE_PREINST_LIBS = true ]; then
      #source ../modulefiles/modulefile.tools.$target           > /dev/null 2>&1
      module use ../modulefiles
      module load modulefile.tools.$target
    else
      #source ../modulefiles/modulefile.tools.${target}_userlib > /dev/null 2>&1
      module use ../modulefiles
      module load modulefile.tools.${target}_userlib
    fi
    module load cmake/3.10.0
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
cd ${TOOLS_PATH}/libsrc
./build_libs_cmake.sh

# Build the tools programs
export TOOLS_INC=${TOOLS_PATH}/include
export TOOLS_INCLUDE="-I${TOOLS_PATH}/include"
export TOOLS_LIBDIR=${TOOLS_PATH}/lib
cd ${TOOLS_PATH}/sorc
make clean
make

exit
