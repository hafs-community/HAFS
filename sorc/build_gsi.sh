#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
cwd=$(pwd)

cd hafs_gsi.fd/ush/

export GSI_MODE=Regional
export ENKF_MODE=FV3REG
./build.sh

exit
