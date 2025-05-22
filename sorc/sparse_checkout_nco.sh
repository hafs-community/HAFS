#!/bin/sh
################################################################################
# Script Name: sparse_checkout_nco.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script conducts some clean-ups for production package delivery through
#   git sparse-checkout.
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

cd ${HOMEhafs}

if [ ! -e parm/hafs.conf ]; then
  echo "Warning: parm/hafs.conf not exist. This is not a HAFS package. Do nothing..."
  exit
fi

# By default set sparse-checkout to include everything
git sparse-checkout set "/*"
git submodule foreach --recursive 'git sparse-checkout set "/*"'

# Apply sparse-checkout at the HAFS application repository level
#echo -e "/*\n!/ush/rsync-no-vanished.sh" > ${HOMEhafs}/.git/info/sparse-checkout
cat <<EOF > ${HOMEhafs}/.git/info/sparse-checkout
/*
!/ush/rsync-no-vanished.sh
EOF
#submodule UFS_UTILS
#echo -e "/*\n!/docs/\n!/driver_scripts/\n!/fix/\n!/parm/\n!/reg_tests/\n!/scripts/\n!/tests/\n!/ush/" > ${HOMEhafs}/.git/modules/UFS_UTILS/info/sparse-checkout
cat <<EOF > ${HOMEhafs}/.git/modules/UFS_UTILS/info/sparse-checkout
/*
!/docs/
!/driver_scripts/
!/fix/
!/parm/
!/reg_tests/
!/scripts/
!/tests/
!/ush/
EOF
#submodule ufs-weather-model
cat <<EOF > ${HOMEhafs}/.git/modules/ufs-weather-model/info/sparse-checkout
/*
!/doc/
!/tests/logs/
EOF
#submodule ufs-weather-model/FV3
cat <<EOF > ${HOMEhafs}/.git/modules/ufs-weather-model/modules/FV3/info/sparse-checkout
/*
!/docs/
EOF
#submodule ufs-weather-model/FV3/atmos_cubed_sphere
cat <<EOF > ${HOMEhafs}/.git/modules/ufs-weather-model/modules/FV3/modules/atmos_cubed_sphere/info/sparse-checkout
/*
!/docs/
EOF
#submodule ufs-weather-model/FV3/ccpp/physics
cat <<EOF > ${HOMEhafs}/.git/modules/ufs-weather-model/modules/FV3/modules/ccpp/physics/info/sparse-checkout
/*
!/physics/docs/
EOF
#submodule ufs-weather-model/FV3/ccpp/framework
cat <<EOF > ${HOMEhafs}/.git/modules/ufs-weather-model/modules/FV3/modules/ccpp/framework/info/sparse-checkout
/*
!/test/
!/test_prebuild/
EOF
#submodule ufs-weather-model/FV3/upp
cat <<EOF > ${HOMEhafs}/.git/modules/ufs-weather-model/modules/FV3/modules/upp/info/sparse-checkout
/*
!/doc/
!/fix/
!/jobs/
!/scripts/
!/ush/
EOF
#submodule ufs-weather-model/stochastic_physics
cat <<EOF > ${HOMEhafs}/.git/modules/ufs-weather-model/modules/stochastic_physics/info/sparse-checkout
/*
!/docs/
!/unit_tests/
EOF
#submodule ufs-weather-model/MOM6
cat <<EOF > ${HOMEhafs}/.git/modules/ufs-weather-model/modules/MOM6/info/sparse-checkout
/*
!/docs/
EOF
#submodule ufs-weather-model/WW3
cat <<EOF > ${HOMEhafs}/.git/modules/ufs-weather-model/modules/WW3/info/sparse-checkout
/*
!/docs/
!/manual/
!/regtests/
!/smc_docs/
EOF
#submodule ufs-weather-model/CICE
cat <<EOF > ${HOMEhafs}/.git/modules/ufs-weather-model/modules/CICE/info/sparse-checkout
/*
!/doc/
EOF
#submodule ufs-weather-model/CMEPS
cat <<EOF > ${HOMEhafs}/.git/modules/ufs-weather-model/modules/CMEPS/info/sparse-checkout
/*
!/doc/
EOF
#submodule ufs-weather-model/CDEPS
cat <<EOF > ${HOMEhafs}/.git/modules/ufs-weather-model/modules/CDEPS/info/sparse-checkout
/*
!/doc/
EOF
#submodule sorc/hafs_tracker.fd
cat <<EOF > ${HOMEhafs}/.git/modules/gfdl-tracker/info/sparse-checkout
/*
EOF
#submodule sorc/hafs_gsi.fd
cat <<EOF > ${HOMEhafs}/.git/modules/sorc/hafs_gsi.fd/info/sparse-checkout
/*
!/doc/
!/unit-tests/
EOF

echo "Applying git sparse-checkout"
git sparse-checkout reapply
git submodule foreach --recursive 'git sparse-checkout reapply'
echo "If desired, one can disable git sparse-checkout through:"
echo "git sparse-checkout disable; git submodule foreach --recursive 'git sparse-checkout disable'"

cd ${cwd}

echo 'cleanup done'

