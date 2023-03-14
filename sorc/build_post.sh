#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
source ../versions/build.ver  > /dev/null 2>&1
cwd=$(pwd)

cd hafs_post.fd/tests
./compile_upp.sh

exit
