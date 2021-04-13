#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

cd hafs_gsi.fd/ush/

#./build_all_cmake.sh "PRODUCTION" "$cwd/hafs_gsi.fd"
./build_all_cmake.sh "BUILD_FV3reg" "$cwd/hafs_gsi.fd"

exit
