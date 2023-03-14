#!/bin/sh
set -eux
source ../versions/build.ver  > /dev/null 2>&1
cwd=$(pwd)

cd hafs_utils.fd

./build_all.sh

exit
