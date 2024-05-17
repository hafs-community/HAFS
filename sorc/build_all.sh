#!/bin/sh
set -xeu
#------------------------------------
# USER DEFINED STUFF:
#
# USE_PREINST_LIBS: set to "true" to use preinstalled libraries.
#                   Anything other than "true" will use libraries locally.
#------------------------------------

export USE_PREINST_LIBS="true"

#Supports Debug or Release modes for the build
export BUILD_MODE=${BUILD_MODE:-Release}   #|Release|Debug|

#------------------------------------
# END USER DEFINED STUFF
#------------------------------------

build_dir=$(pwd)
logs_dir=$build_dir/logs
mkdir -p $logs_dir
mkdir -p ../exec

#------------------------------------
# INCLUDE PARTIAL BUILD
#------------------------------------

. ./partial_build.sh.inc

#------------------------------------
# build libraries first
#------------------------------------
$Build_libs && {
echo " .... Library build not currently supported .... "
#echo " .... Building libraries .... "
#./build_libs.sh > $logs_dir/build_libs.log 2>&1
}

#------------------------------------
# build forecast
#------------------------------------
$Build_forecast && {
echo " .... Building forecast .... "
./build_forecast.sh > $logs_dir/build_forecast.log 2>&1
}

#------------------------------------
# build post
#------------------------------------
$Build_post && {
echo " .... Building post .... "
./build_post.sh > $logs_dir/build_post.log 2>&1
}

#------------------------------------
# build tracker
#------------------------------------
$Build_tracker && {
echo " .... Building tracker .... "
./build_tracker.sh > $logs_dir/build_tracker.log 2>&1
}

#------------------------------------
# build utils
#------------------------------------
$Build_utils && {
echo " .... Building utils .... "
./build_utils.sh > $logs_dir/build_utils.log 2>&1
}

#------------------------------------
# build tools
#------------------------------------
$Build_tools && {
echo " .... Building tools .... "
./build_tools.sh > $logs_dir/build_tools.log 2>&1
}

#------------------------------------
# build gsi
#------------------------------------
$Build_gsi && {
echo " .... Building gsi .... "
./build_gsi.sh > $logs_dir/build_gsi.log 2>&1
}

#------------------------------------
# build hycom_utils
#------------------------------------
$Build_hycom_utils && {
echo " .... Building hycom_utils .... "
./build_hycom_utils.sh > $logs_dir/build_hycom_utils.log 2>&1
}

#------------------------------------
# build ww3_utils
#------------------------------------
$Build_ww3_utils && {
echo " .... Building ww3_utils .... "
./build_ww3_utils.sh > $logs_dir/build_ww3_utils.log 2>&1
}

echo;echo " .... Build system finished .... "

