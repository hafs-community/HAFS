#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

if [ $target = hera ]; then target=hera.intel ; fi
if [ $target = orion ]; then target=orion.intel ; fi
if [ $target = jet ]; then target=jet.intel ; fi
if [ $target = cheyenne ]; then target=cheyenne.intel ; fi

if [ $target = wcoss_cray ]; then 
  app=HAFSW
else
  app=HAFS-ALL
fi
cd hafs_forecast.fd/tests

./compile.sh "$target" "-DAPP=HAFSW -DMOVING_NEST=ON -DCCPP_SUITES=FV3_HAFS_v0_thompson_noahmp_nonsst,FV3_HAFS_v0_thompson_noahmp,FV3_HAFS_v0_thompson_nonsst,FV3_HAFS_v0_thompson,FV3_HAFS_v0_gfdlmp_tedmf_nonsst,FV3_HAFS_v0_gfdlmp_tedmf,FV3_HAFS_v0_thompson_tedmf_gfdlsf -D32BIT=ON" 32bit YES NO
#./compile.sh "$target" "-DAPP=$app -DCCPP_SUITES=FV3_HAFS_v0_gfdlmp_tedmf_nonsst,FV3_HAFS_v0_gfdlmp_tedmf,FV3_HAFS_v0_thompson_tedmf_gfdlsf -D32BIT=ON" 32bit YES NO

exit
