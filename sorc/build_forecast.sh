#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

if [ $target = hera ]; then target=hera.intel ; fi
if [ $target = orion ]; then target=orion.intel ; fi
if [ $target = jet ]; then target=jet.intel ; fi

cd hafs_forecast.fd/tests

./compile.sh "$target" "-DAPP=HAFSW -DCCPP_SUITES=HAFS_v0_gfdlmp_tedmf_nonsst,HAFS_v0_gfdlmp_tedmf,HAFS_v0_hwrf_thompson,HAFS_v0_hwrf -D32BIT=ON" 32bit YES NO

exit
