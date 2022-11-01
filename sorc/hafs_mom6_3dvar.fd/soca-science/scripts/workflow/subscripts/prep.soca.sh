#!/bin/bash

# (C) Copyright 2020-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

set -e

cat << EOF

#================================================================================
#================================================================================
# prep.soca.sh
#   One-time initialization of  static B and other such things
#================================================================================

EOF

# Required environment variables:
envars=()
envars+=("BKGRST_DIR")
envars+=("DA_DUALRES_ENABLE")
envars+=("DA_ENABLED")
envars+=("DA_REGIONAL_ENABLED")
envars+=("DA_SEAICE_ENABLED")
envars+=("DA_OBGC_ENABLED")
envars+=("DA_VARIABLES")
envars+=("DA_VARIABLES_OCN")
envars+=("DA_VARIABLES_ICE")
envars+=("DATE")
envars+=("SOCA_BIN_DIR")
envars+=("SOCA_SCIENCE_BIN_DIR")
envars+=("SOCA_DEFAULT_CFGS_DIR")
envars+=("SOCA_STATIC_DIR")
envars+=("WORK_DIR")

# make sure required env vars exist
set +u
for v in ${envars[@]}; do
    if [[ -z "${!v}" ]]; then
        echo "ERROR: env var $v is not set."; exit 1
    fi
    printf "%-25s %s\n" " $v " "${!v}"
done
set -u
echo ""

# Prepare the Static B configuration
cp $SOCA_DEFAULT_CFGS_DIR/{fields_metadata,soca_staticbinit,soca_staticbinit_sub}.yaml .
domains='ocn'
if [[ "$DA_SEAICE_ENABLED" =~ [yYtT1] ]]; then
  domains='ocn_ice'
fi
sed -i "s;__DOMAINS__;$domains;g" soca_staticbinit.yaml
sed -i "s;__DA_VARIABLES__;$DA_VARIABLES;g" soca_staticbinit.yaml
sed -i "s;__DA_VARIABLES_OCN__;$DA_VARIABLES_OCN;g" soca_staticbinit.yaml
sed -i "s;__DA_VARIABLES_ICE__;$DA_VARIABLES_ICE;g" soca_staticbinit.yaml

# Prepare the localization configuration
cp $SOCA_DEFAULT_CFGS_DIR/soca_locinit.yaml .
domains='ocn'
if [[ "$DA_SEAICE_ENABLED" =~ [yYtT1] ]]; then
  domains='ocn_ice'
fi
sed -i "s;__DOMAINS__;$domains;g" soca_locinit.yaml

# TODO, remove model config init into a common function
ln -s $SOCA_BIN_DIR/soca_{gridgen,parameters,staticbinit}.x .
ln -s $SOCA_DEFAULT_CFGS_DIR/soca_gridgen.yaml .
ln -s $MODEL_CFG_DIR/* .
export FCST_RESTART=1
export FCST_RST_OFST=24
export FCST_START_TIME=$DATE
if [[ "$DA_OBGC_ENABLED" =~ [yYtT1] ]]; then
    . input_bgc.nml.sh > mom_input.nml
else
    . input.nml.sh > mom_input.nml
fi
mkdir -p INPUT; (cd INPUT && ln -sf $MODEL_DATA_DIR/* .)

# Prepare background, needed for the grid generation and B
if [[ "$DA_REGIONAL_ENABLED" =~ [yYtT1] ]]; then
    # Regional case requires converting from dynamic_symmetric to symmetric restarts
    cp -r $BKGRST_DIR RESTART_IN
    $SOCA_SCIENCE_BIN_DIR/soca_dynsym2dyn.sh RESTART_IN
else
    ln -s $BKGRST_DIR RESTART_IN
fi
ln -s $MODEL_DATA_DIR/../soca/* . # TODO use a proper path
mkdir -p OUTPUT

export OMP_NUM_THREADS=1
mkdir -p $SOCA_STATIC_DIR

# create gridspec
if [[ -f $SOCA_STATIC_DIR/soca_gridspec.nc ]]; then
    echo "soca_gridspec.nc has already been initialized, skipping."
else
    if [[ "$DA_REGIONAL_ENABLED" =~ [yYtT1] ]]; then
        cp $(readlink MOM_input) MOM_input_tmp
        unlink MOM_input
        mv  MOM_input_tmp  MOM_input
        sed -i "s;OBC_NUMBER_OF_SEGMENTS = 3;OBC_NUMBER_OF_SEGMENTS = 0;g" MOM_input
    fi
    $MPIRUN ./soca_gridgen.x soca_gridgen.yaml
    if [[ "$DA_REGIONAL_ENABLED" =~ [yYtT1] ]]; then
        # Mask edges of the grid
        $SOCA_SCIENCE_BIN_DIR/soca_domom6_action.py mask-grid-edges
    fi
    mv soca_gridspec.nc $SOCA_STATIC_DIR/
fi

# create sub-sampled geometry
[[ "$DA_DUALRES_ENABLE" == T ]] && {
    if [[ -f $SOCA_STATIC_DIR/subgeom/soca_gridspec_sub.nc ]]; then
        echo "soca_gridspec_sub.nc has already been initialized, skipping."
    else
        mkdir -p $SOCA_STATIC_DIR/subgeom
        [[ "$DA_DUALRES_GENSUBGEOM" == "T" ]] && (
           # Sub-sample the background files
           $SOCA_SCIENCE_BIN_DIR/soca_sub_grid.sh ./INPUT ./RESTART_IN INPUT_small 3
           $MPIRUN ./soca_gridgen.x soca_gridgen_sub.yaml
           mv soca_gridspec_sub.nc $SOCA_STATIC_DIR/subgeom
           ln -sf $PWD/INPUT_small $SOCA_STATIC_DIR/subgeom/INPUT_small
           cp input_sub.nml $SOCA_STATIC_DIR/subgeom/
           cp MOM_input_sub $SOCA_STATIC_DIR/subgeom/
           cp MOM_override_sub $SOCA_STATIC_DIR/subgeom/
        )
        [[ "$DA_DUALRES_GENSUBGEOM" == "F" ]] && (
           # Not sub-sampling, pointing to existing low-res grid
           # the DA_DUALRES_GENSUBGEOM folder should contain the following (more or less, config dependent):
           #   ├── INPUT_small
           #   │   ├── cice.res.nc
           #   │   ├── hycom1_25.nc
           #   │   ├── layer_coord25.nc
           #   │   ├── MOM.res.nc
           #   │   ├── ocean_hgrid.nc
           #   │   └── ocean_topog.nc
           #   ├── input_sub.nml
           #   ├── MOM_input_sub
           #   ├── MOM_override_sub
           #   └── soca_gridspec_sub.nc
           # TODO symlink instead of copy
           mkdir -p $SOCA_STATIC_DIR/subgeom
           cp -rf $DA_DUALRES_GENSUBGEOMLOC/* $SOCA_STATIC_DIR/subgeom/
        )
    fi
}

# create static B / localization
[[ "$DA_ENABLED" == T ]] && {

    if [[ -d $SOCA_STATIC_DIR/bump || -d $SOCA_STATIC_DIR/bump_sub ]]; then
        echo "static B has already been initialized, skipping."
    else
        [[ "$DA_DUALRES_ENABLE" == "T" ]] && {
           mkdir -p bump_sub
           sed -i "s;__DOMAINS__;$domains;g" soca_staticbinit_sub.yaml
           sed -i "s;__DA_VARIABLES__;$DA_VARIABLES;g" soca_staticbinit_sub.yaml
           sed -i "s;__DA_VARIABLES_OCN__;$DA_VARIABLES_OCN;g" soca_staticbinit_sub.yaml
           sed -i "s;__DA_VARIABLES_ICE__;$DA_VARIABLES_ICE;g" soca_staticbinit_sub.yaml
           ln -s $SOCA_STATIC_DIR/subgeom/soca_gridspec_sub.nc .
           ln -sf $SOCA_STATIC_DIR/subgeom/input_sub.nml .
           ln -sf $SOCA_STATIC_DIR/subgeom/MOM_input_sub .
           ln -sf $SOCA_STATIC_DIR/subgeom/MOM_override_sub .
           [[ "$DA_DUALRES_GENSUBGEOM" == "F" ]] && (ln -sf $SOCA_STATIC_DIR/subgeom/INPUT_small .)
           $MPIRUN ./soca_staticbinit.x soca_staticbinit_sub.yaml
           mv bump_sub $SOCA_STATIC_DIR/
        }
        [[ "$DA_DUALRES_ENABLE" == "F" ]] && {
           mkdir -p bump
           ln -s $SOCA_STATIC_DIR/soca_gridspec.nc .
           $MPIRUN ./soca_parameters.x soca_locinit.yaml
           $MPIRUN ./soca_staticbinit.x soca_staticbinit.yaml
           mv bump $SOCA_STATIC_DIR/
        }
    fi
}

echo "DONE"
