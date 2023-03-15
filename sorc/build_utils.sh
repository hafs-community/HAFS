#!/bin/sh
set -eux
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi 
cwd=$(pwd)

cd hafs_utils.fd

./build_all.sh

exit
