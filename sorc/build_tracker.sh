#!/bin/sh
set -eux
source ../versions/build.ver  > /dev/null 2>&1

#export BUILD_TYPE=DEBUG 
export BUILD_TYPE=RELEASE
export BUILD_TYPE=${BUILD_TYPE:-RELEASE}

cd hafs_tracker.fd/src
./build_all_cmake.sh

exit
