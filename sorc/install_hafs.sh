#!/bin/sh
set -xeu
source ./machine-setup.sh.inc > /dev/null 2>&1

# INSTALL_ENVIR of nco or dev
INSTALL_ENVIR=${INSTALL_ENVIR:-${1:-dev}}
#Supports Debug or Release modes for the build
export BUILD_MODE=${BUILD_MODE:-Release}  #|Release|Debug|

cwd=$(pwd)

cd ${cwd}

# Clean up directories and files not needed by NCO (through either git sparse-checkout or removing the files/dirs directly)
if [ "${INSTALL_ENVIR^^}" == "NCO" ]; then
  echo "Use git sparse-checkout to clean up directories and files not needed by NCO"
  ./sparse_checkout_nco.sh
fi

# Build subcomponets
./build_all.sh

# Install executables
./install_all.sh

# Link fix files
./link_fix.sh ${INSTALL_ENVIR}

# Copy system.conf file under parm
cd ${cwd}/../parm

if [ "${INSTALL_ENVIR^^}" != "NCO" ]; then
  if [ ! -e system.conf ]; then
    echo "Copying system.conf. Please check and update it if needed."
    cp -p system.conf.${target} system.conf
  else
    echo "system.conf already exists. Will not overwrite."
  fi
else
  echo "Copying system.conf. Will overwrite even it already exists."
  cp -p system.conf.${INSTALL_ENVIR,,} system.conf
fi

cd ${cwd}

exit

