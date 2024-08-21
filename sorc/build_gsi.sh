#!/bin/sh
set -xeu
source ./machine-setup.sh.inc > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi

#Supports Debug or Release modes for the build
BUILD_MODE=${BUILD_MODE:-Release}

cwd=$(pwd)

cd hafs_gsi.fd/ush/

export GSI_MODE=Regional
export ENKF_MODE=FV3REG

export BUILD_TYPE=${BUILD_MODE}
./build.sh

