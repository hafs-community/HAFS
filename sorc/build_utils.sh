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

#------------------------------------
# INCLUDE PARTIAL BUILD 
#------------------------------------

. ./partial_build.sh

cd hafs_utils.fd/sorc

#------------------------------------
# build chgres
#------------------------------------
$Build_chgres && {
echo " .... Building chgres .... "
./build_chgres.sh > $logs_dir/build_chgres.log 2>&1
}

#------------------------------------
# build chgres_cube
#------------------------------------
$Build_chgres_cube && {
echo " .... Building chgres_cube .... "
./build_chgres_cube.sh > $logs_dir/build_chgres_cube.log 2>&1
}

#------------------------------------
# build orog
#------------------------------------
$Build_orog && {
echo " .... Building orog .... "
./build_orog.sh > $logs_dir/build_orog.log 2>&1
}

#------------------------------------
# build fre-nctools
#------------------------------------
$Build_nctools && {
echo " .... Building fre-nctools .... "
./build_fre-nctools.sh > $logs_dir/build_fre-nctools.log 2>&1
}

#------------------------------------
# build sfc_climo_gen
#------------------------------------
$Build_sfc_climo_gen && {
echo " .... Building sfc_climo_gen .... "
./build_sfc_climo_gen.sh > $logs_dir/build_sfc_climo_gen.log 2>&1
}

#------------------------------------
# build regional_grid
#------------------------------------
$Build_regional_grid && {
echo " .... Building regional_grid .... "
./build_regional_grid.sh > $logs_dir/build_regional_grid.log 2>&1
}

cd $build_dir

echo 'Building utils done'
