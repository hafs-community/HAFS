#!/bin/sh

set -ex

cd ..
pwd=$(pwd)

dir_root=${1:-$pwd}

if [[ -d /dcom && -d /hwrf ]] ; then
    . /usrx/local/Modules/3.2.10/init/sh
    target=wcoss
    . $MODULESHOME/init/sh
elif [[ -d /cm ]] ; then
    . $MODULESHOME/init/sh
    target=wcoss_c
elif [[ -d /ioddev_dell ]]; then
    . $MODULESHOME/init/sh
    target=wcoss_d
elif [[ -d /scratch3 ]] ; then
    . /apps/lmod/lmod/init/sh
    target=theia
elif [[ -d /carddata ]] ; then
    . /opt/apps/lmod/3.1.9/init/sh
    target=s4
elif [[ -d /jetmon ]] ; then
    . $MODULESHOME/init/sh
    target=jet
elif [[ -d /glade ]] ; then
    . $MODULESHOME/init/sh
    target=cheyenne
elif [[ -d /sw/gaea ]] ; then
    . /opt/cray/pe/modules/3.2.10.5/init/sh
    target=gaea
elif [[ -d /discover ]] ; then
    target=discover
    build_type=0
else
    echo "unknown target = $target"
    exit 9
fi

dir_modules=$dir_root/modulefiles
if [ ! -d $dir_modules ]; then
    echo "modulefiles does not exist in $dir_modules"
    exit 10
fi

if [ $target = wcoss_d ]; then
    module purge
    module use -a $dir_modules
    module load modulefile.hafs_utils.$target
elif [ $target = wcoss -o $target = gaea ]; then
    module purge
    module load $dir_modules/modulefile.hafs_utils.$target
elif [ $target = theia -o $target = cheyenne ]; then
    module purge
    source $dir_modules/modulefile.hafs_utils.$target
elif [ $target = wcoss_c ]; then
    module purge
    module load $dir_modules/modulefile.hafs_utils.$target
elif [ $target = discover ]; then
    module load $dir_modules/modulefile.hafs_utils.$target
else 
    module purge
    source $dir_modules/modulefile.hafs_utils.$target
fi

rm -rf $dir_root/deplibs/build
mkdir -p ${dir_root}/deplibs/build 
cd ${dir_root}/deplibs/build
export FC='ifort'
export CC='gcc'
cmake ../
make -j 8
make install

export DEPLIBS=$dir_root/deplibs/lib
export DEPINCS=$dir_root/deplibs/include
