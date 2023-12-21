#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi
cwd=$(pwd)

if [[ ! -s hafs_tracker.fd/modulefiles/gaea-c5.lua ]] ; then
    pushd hafs_tracker.fd
    patch -p1 < ../customizations/hafs_tracker.fd/gaea-c5-patch.diff
    popd
    cp -fp customizations/hafs_tracker.fd/gaea-c5.lua hafs_tracker.fd/modulefiles/gaea-c5.lua
fi

#export BUILD_TYPE=DEBUG 
#export BUILD_TYPE=RELEASE
export BUILD_TYPE=${BUILD_TYPE:-RELEASE}

cd hafs_tracker.fd/src
./build_all_cmake.sh

exit
