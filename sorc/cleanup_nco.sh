#!/bin/sh
################################################################################
# Script Name: cleanup_nco.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script conducts some clean-ups for production package delivery.
# History:
#   04/26/2025: Initial version constructed for HAFS.
################################################################################
set -xeu

cwd=$(pwd)
HOMEhafs=$(pwd)/..

cd ${cwd}

if [ ! -e install_hafs.sh ]; then
  echo "Warnning: install_hafs.sh not exist. This is not a HAFS package. Do nothing..."
  exit
fi

if [ -e cleanup_nco.list ]; then
  LIST=$(cat cleanup_nco.list)
  echo "Items to clean up:"
  echo $LIST
else
  echo "Warning: cleanup_nco.txt not exist or empty. Do nothing..."
  exit
fi

cd ${HOMEhafs}

if [ ! -e parm/hafs.conf ]; then
  echo "Warning: parm/hafs.conf not exist. This is not a HAFS package. Do nothing..."
  exit
fi

echo "Items to clean up:"
echo $LIST
rm -rf $LIST
echo "If desired, one can restore the deleted items through:"
echo "git reset --hard; git submodule foreach --recursive git reset --hard"

cd ${cwd}

echo 'cleanup done'

