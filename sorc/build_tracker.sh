#!/bin/sh
set -xeu
source ./machine-setup.sh.inc > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi

#Supports Debug or Release modes for the build
BUILD_MODE=${BUILD_MODE:-Release}

cwd=$(pwd)

if [ "${BUILD_MODE}" = Release ]; then 
  export BUILD_TYPE=RELEASE
else
  export BUILD_TYPE=DEBUG
fi

cd hafs_tracker.fd/src
./build_all_cmake.sh

