#!/bin/sh
set -xeu
source ./machine-setup.sh.inc > /dev/null 2>&1
if [ $target = wcoss2 ]; then source ../versions/build.ver; fi
export build_dir=`pwd`

#Supports Debug or Release modes for the build
BUILD_MODE=${BUILD_MODE:-Release}

cwd=$(pwd)

cd hafs_jedi.fd

if [ $target = orion ]; then source /apps/other/lmod/lmod/init/sh; fi
module purge

export BUILD_TYPE=${BUILD_MODE}
./build.sh -f -t ${target} 

cd ${build_dir}/hafs_jedi.fd/sorc/jcb
module purge
module use $build_dir/hafs_jedi.fd/modulefiles
module load HDAS/$target.intel
python jcb_client_init.py
cd ${build_dir}/hafs_jedi.fd/sorc/jcb/src/jcb/configuration/apps
rm hdas
ln -sf ${build_dir}/../parm/analysis/jedi/yaml_templates/hdas .
cd ${build_dir}
