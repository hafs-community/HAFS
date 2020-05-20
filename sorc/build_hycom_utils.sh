#! /bin/sh

set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
else
  export MOD_PATH=${cwd}/lib/modulefiles
fi

if [ $target = wcoss ]; then

    echo "Does not support wcoss phase 1/2."
    exit 1

elif [ $target = hera ]; then

    module use ../modulefiles
    module load modulefile.tools.$target
    module list

elif [ $target = orion ]; then

    module use ../modulefiles
    module load modulefile.tools.$target
    module list

elif [ $target = jet ]; then

    module use ../modulefiles
    module load modulefile.tools.$target
    module list

elif [ $target = wcoss_cray ]; then

    module use ../modulefiles
    module load modulefile.tools.$target
    module list

elif [ $target = wcoss_dell_p3 ]; then

    module use ../modulefiles
    module load modulefile.tools.$target
    module list

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

#export HDF5_LDFLAGS="-L${HDF5}/lib -lhdf5_hl -lhdf5hl_fortran -lhdf5 -lhdf5_fortran -lz";
#export NETCDF_LDFLAGS="-L${NETCDF}/lib -lnetcdff -lnetcdf -lz ${HDF5_LDFLAGS}";

cd hafs_hycom_utils.fd

# Ensure a clean compile:
make -i clean
#find . -name '*.o' -o -name '*.a' -o -name '*.mod' | xargs rm -f
#rm -f ./exec/*

# Compile hycom_utils
make -i -f makefile all

cd ../

exit
