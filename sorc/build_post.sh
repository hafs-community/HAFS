#!/bin/sh
set -xeu
source ./machine-setup.sh.inc > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi

#Supports Debug or Release modes for the build
BUILD_MODE=${BUILD_MODE:-Release}

cwd=$(pwd)

cd hafs_post.fd/tests
if [ "${BUILD_MODE}" = Release ]; then
  ./compile_upp.sh
else
  ./compile_upp.sh -d 
fi
