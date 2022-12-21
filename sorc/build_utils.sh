#!/bin/sh
set -eux
cwd=$(pwd)

cd hafs_utils.fd

./build_all.sh

exit
