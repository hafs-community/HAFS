#!/bin/sh
set -xeu
source ./machine-setup.sh.inc > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi

#Supports Debug or Release modes for the build
BUILD_MODE=${BUILD_MODE:-Release}

cwd=$(pwd)

cd hafs_jedi.fd

if [ $target = orion ]; then source /apps/other/lmod/lmod/init/sh; fi
module purge

export BUILD_TYPE=${BUILD_MODE}
./build.sh -f -t ${target} 

cd ${cwd}/../parm/analysis/jedi
if [ -d jcb-hdas ]; then rm jcb-hdas; fi
ln -sf ${cwd}/hafs_jedi.fd/parm/jcb-hdas .
cd ${cwd}
