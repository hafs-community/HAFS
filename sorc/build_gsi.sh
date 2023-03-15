#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi
cwd=$(pwd)

cd hafs_gsi.fd/ush/

export GSI_MODE=Regional
export ENKF_MODE=FV3REG
./build.sh

exit
