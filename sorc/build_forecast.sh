#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi
cwd=$(pwd)

cd hafs_forecast.fd/tests

if [ $target = hera ]; then target=hera.intel ; fi
if [ $target = orion ]; then target=orion.intel ; fi
if [ $target = jet ]; then target=jet.intel ; fi
if [ $target = cheyenne ]; then target=cheyenne.intel ; fi
if [ $target = wcoss2 ]; then target=wcoss2.intel ; fi

cd hafs_forecast.fd/tests
app=HAFS-MOM6

# HAFS-MOM6
./compile.sh "$target" "-DAPP=$app -DREGIONAL_MOM6=ON DMOVING_NEST=ON -DFASTER=ON -DCCPP_SUITES=FV3_HAFS_v1_thompson_noahmp_nonsst,FV3_HAFS_v1_thompson_noahmp,FV3_HAFS_v1_thompson_nonsst,FV3_HAFS_v1_thompson,FV3_HAFS_v1_gfdlmp_tedmf_nonsst,FV3_HAFS_v1_gfdlmp_tedmf,FV3_HAFS_v1_thompson_tedmf_gfdlsf -D32BIT=ON" 32bit YES NO

#app=HAFSW
#./compile.sh "$target" "-DAPP=$app -DMOVING_NEST=ON -DFASTER=ON -DCCPP_SUITES=FV3_HAFS_v1_thompson_noahmp_nonsst,FV3_HAFS_v1_thompson_noahmp,FV3_HAFS_v1_thompson_nonsst,FV3_HAFS_v1_thompson,FV3_HAFS_v1_gfdlmp_tedmf_nonsst,FV3_HAFS_v1_gfdlmp_tedmf,FV3_HAFS_v1_thompson_tedmf_gfdlsf -D32BIT=ON" 32bit YES NO

exit
