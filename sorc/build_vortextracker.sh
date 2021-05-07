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
  export hwrf_g2_inc=/scratch1/NCEPDEV/hwrf/save/Bin.Liu/hwrf-utilities/libs/mods/g2
  export hwrf_g2_lib=/scratch1/NCEPDEV/hwrf/save/Bin.Liu/hwrf-utilities/libs/libg2.a
elif [ $target = orion ]; then
  export FC=ifort
  export F90=ifort
  export CC=icc
  export hwrf_g2_inc=/work/noaa/hwrf/noscrub/bthomas/H220/sorc/hwrf-utilities/libs/mods/g2
  export hwrf_g2_lib=/work/noaa/hwrf/noscrub/bthomas/H220/sorc/hwrf-utilities/libs/libg2.a
elif [ $target = jet ]; then
  export FC=ifort
  export F90=ifort
  export CC=icc
  export hwrf_g2_inc=/lfs4/HFIP/hwrf-vd/Zhan.Zhang/H219_kjet/sorc/hwrf-utilities/libs/mods/g2
  export hwrf_g2_lib=/lfs4/HFIP/hwrf-vd/Zhan.Zhang/H219_kjet/sorc/hwrf-utilities/libs/libg2.a
elif [ $target = wcoss_cray ]; then
  export FC=ftn
  export F90=ftn
  export CC=icc
  export hwrf_g2_inc=/gpfs/hps3/emc/hwrf/noscrub/Bin.Liu/save/H221final/sorc/hwrf-utilities/libs/mods/g2
  export hwrf_g2_lib=/gpfs/hps3/emc/hwrf/noscrub/Bin.Liu/save/H221final/sorc/hwrf-utilities/libs/libg2.a
elif [ $target = wcoss_dell_p3 ]; then
  export FC=ifort
  export F90=ifort
  export CC=icc
  export hwrf_g2_inc=/gpfs/dell2/emc/modeling/noscrub/Biju.Thomas/save/trunk_20210420/sorc/hwrf-utilities/libs/mods/g2
  export hwrf_g2_lib=/gpfs/dell2/emc/modeling/noscrub/Biju.Thomas/save/trunk_20210420/sorc/hwrf-utilities/libs/libg2.a
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
if [ $target = hera ]; then
  cmake .. -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc -Dhwrf_g2_lib=$hwrf_g2_lib -Dhwrf_g2_inc=$hwrf_g2_inc
elif [ $target = orion ]; then
  cmake .. -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc -Dhwrf_g2_lib=$hwrf_g2_lib -Dhwrf_g2_inc=$hwrf_g2_inc
elif [ $target = jet ]; then
  cmake .. -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc -Dhwrf_g2_lib=$hwrf_g2_lib -Dhwrf_g2_inc=$hwrf_g2_inc
elif [ $target = wcoss_cray ]; then
  cmake .. -DCMAKE_Fortran_COMPILER=ftn -DCMAKE_C_COMPILER=cc -Dhwrf_g2_lib=$hwrf_g2_lib -Dhwrf_g2_inc=$hwrf_g2_inc
elif [ $target = wcoss_dell_p3 ]; then
  cmake .. -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc -Dhwrf_g2_lib=$hwrf_g2_lib -Dhwrf_g2_inc=$hwrf_g2_inc
else
  cmake .. -DCMAKE_Fortran_COMPILER=ifort -DCMAKE_C_COMPILER=icc
fi
make -j 8 VERBOSE=1
make install

cd ../

exit
