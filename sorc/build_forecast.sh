#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi
cwd=$(pwd)

cd hafs_forecast.fd/tests

if [ $target = hera ]; then target=hera ; fi
if [ $target = orion ]; then target=orion ; fi
if [ $target = jet ]; then target=jet ; fi
if [ $target = cheyenne ]; then target=cheyenne ; fi
if [ $target = wcoss2 ]; then target=wcoss2 ; fi

app=HAFS-MOM6
./compile.sh "$target"  "-DAPP=$app -DREGIONAL_MOM6=ON -DMOVING_NEST=ON -DFASTER=ON -DCCPP_SUITES=FV3_HAFS_v1_gfdlmp_tedmf_nonsst,FV3_HAFS_v1_gfdlmp_tedmf,FV3_HAFS_v1_thompson_tedmf_gfdlsf -D32BIT=ON" hafsmom6_intel intel

#app=HAFSW
#./compile.sh "$target" "-DAPP=$app -DMOVING_NEST=ON -DFASTER=ON -DCCPP_SUITES=FV3_HAFS_v1_thompson_noahmp_nonsst,FV3_HAFS_v1_thompson_noahmp,FV3_HAFS_v1_thompson_nonsst,FV3_HAFS_v1_thompson,FV3_HAFS_v1_gfdlmp_tedmf_nonsst,FV3_HAFS_v1_gfdlmp_tedmf,FV3_HAFS_v1_thompson_tedmf_gfdlsf -D32BIT=ON" 32bit YES NO

exit
