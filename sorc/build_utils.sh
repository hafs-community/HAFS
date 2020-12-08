#!/bin/sh
set -eux

export USE_PREINST_LIBS="true"

#------------------------------------
# END USER DEFINED STUFF
#------------------------------------

build_dir=`pwd`
logs_dir=$build_dir/logs
if [ ! -d $logs_dir  ]; then
  echo "Creating logs folder"
  mkdir $logs_dir
fi

cd hafs_utils.fd

./build_all.sh

cd $build_dir

echo 'Building utils done'
