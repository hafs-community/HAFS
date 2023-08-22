#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi
cwd=$(pwd)

cd hafs_forecast.fd/tests

# if [ $target = hera ]; then target=hera.intel ; fi
# if [ $target = orion ]; then target=orion.intel ; fi
# if [ $target = jet ]; then target=jet.intel ; fi
# if [ $target = cheyenne ]; then target=cheyenne.intel ; fi
# if [ $target = wcoss2 ]; then target=wcoss2.intel ; fi

app=HAFSW
opts="-DAPP=HAFSW -DMOVING_NEST=ON -DCCPP_SUITES=FV3_HAFS_v1_gfdlmp_tedmf,FV3_HAFS_v1_gfdlmp_tedmf_nonsst,FV3_HAFS_v1_thompson_tedmf_gfdlsf,FV3_RRFSv2c3 -D32BIT=ON"
./compile.sh "$target" "$opts" 32bit intel YES NO

exit
