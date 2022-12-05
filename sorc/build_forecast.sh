#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

if [ $target = hera ]; then target=hera.intel ; fi
if [ $target = orion ]; then target=orion.intel ; fi
if [ $target = jet ]; then target=jet.intel ; fi
if [ $target = cheyenne ]; then target=cheyenne.intel ; fi
if [ $target = wcoss2 ]; then target=wcoss2.intel ; fi

app=HAFS-ALL

cd hafs_forecast.fd/tests

./compile.sh "$target" "-DAPP=HAFSW -DMOVING_NEST=ON -DCCPP_SUITES=FV3_HAFS_v1_thompson_noahmp_nonsst,FV3_HAFS_v1_thompson_noahmp,FV3_HAFS_v1_thompson_nonsst,FV3_HAFS_v1_thompson,FV3_HAFS_v1_gfdlmp_tedmf_nonsst,FV3_HAFS_v1_gfdlmp_tedmf,FV3_HAFS_v1_thompson_tedmf_gfdlsf -D32BIT=ON" 32bit YES NO
#./compile.sh "$target" "-DAPP=$app -DCCPP_SUITES=FV3_HAFS_v1_gfdlmp_tedmf_nonsst,FV3_HAFS_v1_gfdlmp_tedmf,FV3_HAFS_v1_thompson_tedmf_gfdlsf -D32BIT=ON" 32bit YES NO

exit
