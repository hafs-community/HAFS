#!/bin/sh
set -eux
#------------------------------------
# USER DEFINED STUFF:
#
# USE_PREINST_LIBS: set to "true" to use preinstalled libraries.
#                   Anything other than "true"  will use libraries locally.
#------------------------------------

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

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  echo "Creating ../exec folder"
  mkdir ../exec
fi

#------------------------------------
# INCLUDE PARTIAL BUILD 
#------------------------------------

. ./partial_build.sh

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
# build vortextracker
#------------------------------------
$Build_vortextracker && {
echo " .... Building vortextracker .... "
./build_vortextracker.sh > $logs_dir/build_vortextracker.log 2>&1
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

echo;echo " .... Build system finished .... "

exit 0
