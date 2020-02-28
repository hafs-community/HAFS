#! /bin/sh

set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi


cd hafs_hycom_utils.fd/ 

./install_hycom.scr


cd ../

exit

