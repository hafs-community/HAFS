#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi
cwd=$(pwd)

#export BUILD_TYPE=DEBUG 
#export BUILD_TYPE=RELEASE
export BUILD_TYPE=${BUILD_TYPE:-RELEASE}

cd hafs_tracker.fd/src
./build_all_cmake.sh

exit
