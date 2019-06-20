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
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
else
  export MOD_PATH=${cwd}/lib/modulefiles
fi

if [ $target = wcoss ]; then

    echo "Does not support wcoss phase 1/2."
    exit 1

elif [ $target = theia ]; then

    targetx=theia
    #source ../modulefiles/modulefile.vortextracker.$target > /dev/null 2>&1
    module use ../modulefiles
    module load modulefile.vortextracker.$target
    module list

    export FC=ifort
    export F90=ifort
    export CC=icc

elif [ $target = jet ]; then

    targetx=jet
    #source ../modulefiles/modulefile.vortextracker.$target > /dev/null 2>&1
    module use ../modulefiles
    module load modulefile.vortextracker.$target
    module list

    export FC=ifort
    export F90=ifort
    export CC=icc

elif [ $target = wcoss_cray ]; then

    targetx=cray
    if [ $USE_PREINST_LIBS = true ]; then
      #source ../modulefiles/modulefile.vortextracker.$target           > /dev/null 2>&1
      module use ../modulefiles
      module load modulefile.vortextracker.$target
    else
      #source ../modulefiles/modulefile.vortextracker.${target}_userlib > /dev/null 2>&1
      module use ../modulefiles
      module load modulefile.vortextracker.${target}_userlib
    fi
    module list

    export FC=ftn
    export F90=ftn
    export CC=icc

elif [ $target = wcoss_dell_p3 ]; then

    targetx=wcoss_dell_p3
    if [ $USE_PREINST_LIBS = true ]; then
      #source ../modulefiles/modulefile.vortextracker.$target           > /dev/null 2>&1
      module use ../modulefiles
      module load modulefile.vortextracker.$target
    else
      #source ../modulefiles/modulefile.vortextracker.${target}_userlib > /dev/null 2>&1
      module use ../modulefiles
      module load modulefile.vortextracker.${target}_userlib
    fi
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
export HDF5_LDFLAGS=${HDF5_LDFLAGS:-"-L${HDF5}/lib -lhdf5_hl -lhdf5hl_fortran -lhdf5 -lhdf5_fortran"}

export INC="-I${W3EMC_INCd} -I${G2_INCd} -I${SIGIO_INC4}"
export LIBS="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${G2_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB}"

export INC_GETTRK="-I${W3EMC_INCd} -I${G2_INCd} -I${SIGIO_INC4} ${NETCDF_INCLUDE} ${HDF5_INCLUDE}"
export LIBS_GETTRK="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${G2_LIBd} ${NETCDF_LDFLAGS} ${HDF5_LDFLAGS} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB}"

cd hafs_vortextracker.fd
   make clean
   make FC=${FC} F90=${F90} CC=${CC} -f Makefile
#  make install

cd ../

exit
