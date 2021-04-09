#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

if [ $target = hera ]; then
    targetx=hera
    #source ../modulefiles/modulefile.vortextracker.$target > /dev/null 2>&1
    module use ../modulefiles
    module load modulefile.vortextracker.$target
    module list
    export FC=ifort
    export F90=ifort
    export CC=icc
    export hwrf_g2_inc=/scratch1/NCEPDEV/hwrf/save/Bin.Liu/hwrf-utilities/libs/mods/g2
    export hwrf_g2_lib=/scratch1/NCEPDEV/hwrf/save/Bin.Liu/hwrf-utilities/libs/libg2.a
elif [ $target = orion ]; then
    targetx=orion
    #source ../modulefiles/modulefile.vortextracker.$target > /dev/null 2>&1
    module use ../modulefiles
    module load modulefile.vortextracker.$target
    module list
    export FC=ifort
    export F90=ifort
    export CC=icc
    export hwrf_g2_inc=/work/noaa/hwrf/noscrub/bthomas/H220/sorc/hwrf-utilities/libs/mods/g2
    export hwrf_g2_lib=/work/noaa/hwrf/noscrub/bthomas/H220/sorc/hwrf-utilities/libs/libg2.a
elif [ $target = jet ]; then
    targetx=jet
    #source ../modulefiles/modulefile.vortextracker.$target > /dev/null 2>&1
    module use ../modulefiles
    module load modulefile.vortextracker.$target
    module list
    export FC=ifort
    export F90=ifort
    export CC=icc
    export hwrf_g2_inc=/lfs4/HFIP/hwrf-vd/Zhan.Zhang/H219_kjet/sorc/hwrf-utilities/libs/mods/g2
    export hwrf_g2_lib=/lfs4/HFIP/hwrf-vd/Zhan.Zhang/H219_kjet/sorc/hwrf-utilities/libs/libg2.a
elif [ $target = wcoss_cray ]; then
    targetx=cray
    #source ../modulefiles/modulefile.vortextracker.$target           > /dev/null 2>&1
    module use ../modulefiles
    module load modulefile.vortextracker.$target
    module list
    export FC=ftn
    export F90=ftn
    export CC=icc
    export hwrf_g2_inc=/gpfs/hps3/emc/hwrf/noscrub/Bin.Liu/save/H221final/sorc/hwrf-utilities/libs/mods/g2
    export hwrf_g2_lib=/gpfs/hps3/emc/hwrf/noscrub/Bin.Liu/save/H221final/sorc/hwrf-utilities/libs/libg2.a
elif [ $target = wcoss_dell_p3 ]; then
    targetx=wcoss_dell_p3
    #source ../modulefiles/modulefile.vortextracker.$target           > /dev/null 2>&1
    module use ../modulefiles
    module load modulefile.vortextracker.$target
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

##export HDF5_INCLUDE=${HDF5_INCLUDE:-"-I${HDF5}/include"}
#export HDF5_LDFLAGS=${HDF5_LDFLAGS:-"-L${HDF5}/lib -lhdf5_hl -lhdf5hl_fortran -lhdf5 -lhdf5_fortran"}
##export HDF5_LDFLAGS=${HDF5_LDFLAGS:-"-L${HDF5}/lib -lhdf5_hl -lhdf5"}

##export INC="-I${W3EMC_INCd} -I${G2_INCd} -I${SIGIO_INC4}"
##export LIBS="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${G2_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB}"

##export INC_GETTRK="-I${W3EMC_INCd} -I${G2_INCd} -I${SIGIO_INC4} ${NETCDF_INCLUDE} ${HDF5_INCLUDE}"
##export LIBS_GETTRK="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${G2_LIBd} ${NETCDF_LDFLAGS} ${HDF5_LDFLAGS} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB}"

# The following is a temporary fix to enable running the tracker on Hera and Orion.
##if [ $target = hera ]; then
##  HWRF_UTIL_LIB=/scratch1/NCEPDEV/hwrf/save/Bin.Liu/hwrf-utilities/libs
##  export INC_GETTRK="-I${HWRF_UTIL_LIB}/mods/bacio_8 -I${HWRF_UTIL_LIB}/mods/g2 -I${W3EMC_INCd} ${NETCDF_INCLUDE} ${HDF5_INCLUDE}"
##  export LIBS_GETTRK="-L${HWRF_UTIL_LIB} -lbacio -lg2 ${W3EMC_LIBd} ${W3NCO_LIBd} ${NETCDF_LDFLAGS} ${HDF5_LDFLAGS} -L/usr/lib64 -lz -lpng -ljasper"
##elif [ $target = orion ]; then
##  HWRF_UTIL_LIB=/work/noaa/hwrf/noscrub/bthomas/H220/sorc/hwrf-utilities/libs
##  LIBS_GETTRK="-L${HWRF_UTIL_LIB} -lbacio -lg2 -lz -lpng  ${W3EMC_LIBd} ${W3NCO_LIBd} ${NETCDF_LDFLAGS} ${HDF5_LDFLAGS} ${JASPER_LIB}"
##  INC_GETTRK="-I${HWRF_UTIL_LIB}/mods/bacio_8 -I${HWRF_UTIL_LIB}/mods/g2 -I${W3EMC_INCd} ${NETCDF_INCLUDE} ${HDF5_INCLUDE}"
##fi

cd hafs_vortextracker.fd
if [ -d "build" ]; then
   rm -rf build
fi
mkdir build
cd build
if [ $target = hera ]; then
  cmake .. -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc -Dhwrf_g2_lib=$hwrf_g2_lib -Dhwrf_g2_inc=$hwrf_g2_inc
elif [ $target = orion ]; then
  cmake .. -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc -Dhwrf_g2_lib=$hwrf_g2_lib -Dhwrf_g2_inc=$hwrf_g2_inc
elif [ $target = jet ]; then
  cmake .. -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc -Dhwrf_g2_lib=$hwrf_g2_lib -Dhwrf_g2_inc=$hwrf_g2_inc
elif [ $target = wcoss_cray ]; then
  cmake .. -DCMAKE_Fortran_COMPILER=ftn -DCMAKE_C_COMPILER=cc -Dhwrf_g2_lib=$hwrf_g2_lib -Dhwrf_g2_inc=$hwrf_g2_inc
else
  cmake .. -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc
fi
make -j 8 VERBOSE=1
make install

#   make clean
#   make FC=${FC} F90=${F90} CC=${CC} -f Makefile
#  make install

cd ../

exit
