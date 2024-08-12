#!/bin/sh
set -xeu
source ./machine-setup.sh.inc > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi

#Supports Debug or Release modes for the build
BUILD_MODE=${BUILD_MODE:-Release}

cwd=$(pwd)

cd hafs_jedi.fd


export BUILD_TYPE=${BUILD_MODE}
./build.sh -f -t ${target} > ../logs/build_jedi.log 2>&1 & 

