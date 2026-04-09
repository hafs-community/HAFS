#!/bin/sh
set -xeu
source ./machine-setup.sh.inc > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi

#Supports Debug or Release modes for the build
BUILD_MODE=${BUILD_MODE:-Release}

cwd=$(pwd)

cd hafs_forecast.fd/tests

if [ "${BUILD_MODE}" = Release ]; then
   debug=OFF
else
   debug=ON
fi

app=HAFS-MOM6W
./compile.sh "$target" "-DAPP=$app -DREGIONAL_MOM6=ON -DCDEPS_INLINE=ON -DMOVING_NEST=ON -DFASTER=ON -DDEBUG=$debug \
    -DCCPP_SUITES=FV3_HAFS_v2_coupled,FV3_HAFS_v2,FV3_HAFS_v2_gfdlmp_coupled,FV3_HAFS_v2_gfdlmp,FV3_HAFS_v2_gfdlmpv3_coupled,FV3_HAFS_v2_gfdlmpv3,FV3_HAFS_v2_tiedtke_coupled,FV3_HAFS_v2_tiedtke,FV3_HAFS_v1_thompson_nonsst,FV3_HAFS_v1_thompson,FV3_HAFS_v1_gfdlmp_tedmf_nonsst,FV3_HAFS_v1_gfdlmp_tedmf,FV3_HAFS_v1_gfdlmpv3_tedmf_nonsst,FV3_HAFS_v1_gfdlmpv3_tedmf \
    -D32BIT=ON -DRRTMGP_32BIT=ON" hafs_mom6 intel YES NO

