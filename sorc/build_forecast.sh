#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi
cwd=$(pwd)

if [[ $target =~ .*c5 ]] ; then target=gaea-c5 ; fi
test -s hafs_forecast.fd/FV3/ccpp/suites/suite_FV3_global_nest_v0.xml ||
    ln -sf ../../../../customizations/hafs_forecast.fd/suite_FV3_global_nest_v0.xml hafs_forecast.fd/FV3/ccpp/suites/suite_FV3_global_nest_v0.xml

cd hafs_forecast.fd/tests

app=HAFSW
opts="-DAPP=HAFSW -DMOVING_NEST=ON -DCCPP_SUITES=FV3_global_nest_v1,FV3_global_nest_v0 -D32BIT=ON"
./compile.sh "$target" "$opts" 32bit intel YES NO

exit
