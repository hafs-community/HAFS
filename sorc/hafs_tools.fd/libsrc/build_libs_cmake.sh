#!/bin/sh

set -ex

cd ..
pwd=$(pwd)

dir_root=${1:-$pwd}

rm -rf $dir_root/libsrc/build
mkdir -p ${dir_root}/libsrc/build 
cd ${dir_root}/libsrc/build
cmake ../
make -j 8
make install
