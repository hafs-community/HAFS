#!/bin/sh
set -eux
source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

cd ${cwd}

# Build subcomponets
./build_all.sh

# Install executables
./install_all.sh

# Link fix files
./link_fix.sh

# Copy system.conf file under parm
cd ${cwd}/../parm

if [ ! -e system.conf ]; then
  echo "Copying system.conf. Please check and updated it if needed."
  cp -p system.conf.${target} system.conf
else
  echo "system.conf already exists."
fi

cd ${cwd}

exit

