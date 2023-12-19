#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi

#Supports Debug or Release modes for the build
BUILD_MODE=${BUILD_MODE:-Release}

cwd=$(pwd)

cp hafs_forecast.fd/FV3/ccpp/suites_not_used/suite_FV3_HAFS_v1* hafs_forecast.fd/FV3/ccpp/suites/
cd hafs_forecast.fd/tests

if [ $target = hera ]; then target=hera ; fi
if [ $target = orion ]; then target=orion ; fi
if [ $target = hercules ]; then target=hercules ; fi
if [ $target = jet ]; then target=jet ; fi
if [ $target = cheyenne ]; then target=cheyenne ; fi
if [ $target = wcoss2 ]; then target=wcoss2 ; fi

if [ "${BUILD_MODE}" = Release ]; then
  app=HAFS-MOM6
  ./compile.sh "$target" "-DAPP=$app -DREGIONAL_MOM6=ON -DMOVING_NEST=ON -DFASTER=ON -DCCPP_SUITES=FV3_HAFS_v1_thompson_noahmp_nonsst,FV3_HAFS_v1_thompson_noahmp,FV3_HAFS_v1_thompson_nonsst,FV3_HAFS_v1_thompson,FV3_HAFS_v1_gfdlmp_tedmf_nonsst,FV3_HAFS_v1_gfdlmp_tedmf,FV3_HAFS_v1_thompson_tedmf_gfdlsf -D32BIT=ON" hafs_mom6 intel YES NO

  app=HAFSW
  ./compile.sh "$target" "-DAPP=$app -DMOVING_NEST=ON -DFASTER=ON -DCCPP_SUITES=FV3_HAFS_v1_thompson_noahmp_nonsst,FV3_HAFS_v1_thompson_noahmp,FV3_HAFS_v1_thompson_nonsst,FV3_HAFS_v1_thompson,FV3_HAFS_v1_gfdlmp_tedmf_nonsst,FV3_HAFS_v1_gfdlmp_tedmf,FV3_HAFS_v1_thompson_tedmf_gfdlsf -D32BIT=ON" hafs_hycom intel YES NO
else
  app=HAFS-MOM6
  ./compile.sh "$target" "-DAPP=$app -DREGIONAL_MOM6=ON -DMOVING_NEST=ON -DFASTER=ON -DDEBUG=ON -DCCPP_SUITES=FV3_HAFS_v1_thompson_noahmp_nonsst,FV3_HAFS_v1_thompson_noahmp,FV3_HAFS_v1_thompson_nonsst,FV3_HAFS_v1_thompson,FV3_HAFS_v1_gfdlmp_tedmf_nonsst,FV3_HAFS_v1_gfdlmp_tedmf,FV3_HAFS_v1_thompson_tedmf_gfdlsf -D32BIT=ON" hafs_mom6 intel YES NO

  app=HAFSW
  ./compile.sh "$target" "-DAPP=$app -DMOVING_NEST=ON -DFASTER=ON -DDEBUG=ON -DCCPP_SUITES=FV3_HAFS_v1_thompson_noahmp_nonsst,FV3_HAFS_v1_thompson_noahmp,FV3_HAFS_v1_thompson_nonsst,FV3_HAFS_v1_thompson,FV3_HAFS_v1_gfdlmp_tedmf_nonsst,FV3_HAFS_v1_gfdlmp_tedmf,FV3_HAFS_v1_thompson_tedmf_gfdlsf -D32BIT=ON" hafs_hycom intel YES NO
fi
exit
