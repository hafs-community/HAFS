#!/bin/bash

# (C) Copyright 2020-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
set -e

cat << EOF

#================================================================================
#================================================================================
# post.obs.sh
#   Post processing of obsservation output files from var, letkf, or hofx apps.
#   Combine and compress the files.
#================================================================================

EOF

# Required environment variables:
envars=()
envars+=("DA_ATMHOFX_ENABLED")
envars+=("DA_LETKF_ENABLED")
envars+=("DA_VAR_ENABLED")
envars+=("OBS_OUT_DIR")
envars+=("OBS_OUT_CTRL_DIR")
envars+=("OBS_OUT_ENS_DIR")
envars+=("SOCA_SCIENCE_BIN_DIR")
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

# which variables to save depends on whether var or just hofx application was run
[[ "$DA_VAR_ENABLED" == "T" ]] && \
    obs_groups="MetaData,ombg,oman,ObsValue,ObsError,EffectiveError0,EffectiveQC0"
[[ "$DA_HOFX_ENABLED" == "T" ]] && \
    obs_groups="MetaData,hofx,ObsValue,ObsError,EffectiveError,EffectiveQC"

ymdh=$(date -ud "$DATE" +"%Y%m%d%H")

# Post processing for the output files from the 3dvar
if [[ "$DA_VAR_ENABLED" == "T" || "$DA_HOFX_ENABLED" == "T" ]]; then
    echo "processing VAR/HofX observation output"
    mkdir -p obsout/ctrl

    # for each obs type
    for ob_dir in $OBS_OUT_CTRL_DIR/*; do
        ob=$(basename $ob_dir)
        echo "  Processing $ob"
        mkdir -p $ob

        # for each variable within the obs type
        # (e.g. insitu has both _T and _S files, these are combined back into
        #  one file at the end)
        for f in $ob_dir/*_0000.nc; do
            ob_var=${f##*/}
            ob_var=${ob_var%_*}

            # concatenate files, and keep only the variables of interest
            $MPIRUN ${SOCA_SCIENCE_BIN_DIR}/obs_cat.x -i $ob_dir/${ob_var} -o $ob_var.tmp.nc
            ncks -h -g $obs_groups $ob_var.tmp.nc -A obsout/ctrl/$ob.$ymdh.nc
            rm *.tmp.nc
        done
    done
    mkdir -p $OBS_OUT_DIR/ctrl
    mv obsout/ctrl/* $OBS_OUT_DIR/ctrl/
fi

# post processing for the output files from the atm h(x)
if [[ "$DA_ATMHOFX_ENABLED" == "T" ]]; then
    echo "processing LETKF observation output"
    echo "TODO: need to implement this"
    mkdir -p $OBS_ATM_OUT_DIR/ctrl
    mv $OBS_ATM_OUT_CTRL_DIR/* $OBS_ATM_OUT_DIR/ctrl/
fi

# post processing for the output files from the letkf
# NOTE: obs output from the run.letkf is already concatenated, unlike run.var
# TODO, save ensemble spread as well??
if [[ "$DA_LETKF_ENABLED" == "T" ]]; then
    echo "processing LETKF observation output"
    mkdir -p obsout/ens
    obs_groups="MetaData,ombg,oman,ObsValue,ObsError,EffectiveQC0"
    for ob_file in $OBS_OUT_ENS_DIR/*; do
        ob=${ob_file##*/}
        ob=${ob%.*}

        ncks -O -h -g $obs_groups $ob_file obsout/ens/$ob.$ymd.nc &
    done
    wait
    mkdir -p $OBS_OUT_DIR/ens
    mv obsout/ens/* $OBS_OUT_DIR/ens/
fi
