#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

if [ $target = hera ]; then target=hera.intel ; fi
if [ $target = orion ]; then target=orion.intel ; fi
if [ $target = jet ]; then target=jet.intel ; fi
#if [ $target = wcoss_cray ]; then module load python/2.7.14; fi

cd hafs_forecast.fd/tests

./compile.sh "$target" "APP=HAFS CCPP=Y STATIC=Y SUITES=HAFS_v0_gfdlmp_tedmf_nonsst,HAFS_v0_gfdlmp_tedmf,HAFS_v0_gfdlmp_nocpnsst,HAFS_v0_gfdlmp_nonsst,HAFS_v0_gfdlmp_nocp,HAFS_v0_gfdlmp,HAFS_v0_hwrf_thompson,HAFS_v0_hwrf 32BIT=Y" 32bit YES NO

exit
