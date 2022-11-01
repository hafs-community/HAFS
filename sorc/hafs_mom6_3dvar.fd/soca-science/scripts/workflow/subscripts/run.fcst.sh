#!/bin/bash

# (C) Copyright 2020-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

set -e

cat << EOF

#================================================================================
#================================================================================
# run.fcst.sh
#   Run an ocean forecast, using one of several available model types.
#================================================================================

EOF

# Required environment variables:
envars=()
envars+=("MODEL_DEFAULT_SCRIPT_DIR")
envars+=("MODEL_RST_DIR_IN")
envars+=("MODEL_RST_DIR_OUT")
envars+=("MODEL_SCRIPT")
envars+=("MODEL")
envars+=("WORK_DIR")
envars+=("DIAG_TMP_DIR")
envars+=("FCST_LEN")

MODEL_SCRIPT=${MODEL_SCRIPT:-$MODEL_DEFAULT_SCRIPT_DIR/model.$MODEL.sh}
export FCST_RESTART=${FCST_RESTART:-1}
export FCST_RST_OFST=${FCST_RST_OFST:-$FCST_LEN}

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

# check to see if forecast has already been run
if [[ -d "$MODEL_RST_DIR_OUT" && $(ls $MODEL_RST_DIR_OUT -1q | wc -l) -gt 0 ]]; then
    echo "Forecast has already been run. Background files are at: "
    echo "$MODEL_RST_DIR_OUT"
    exit 0
fi

# find the script containing the details of how the model should be run.
if [[ ! -f "$MODEL_SCRIPT" ]]; then
    echo "ERROR: cannot find run script for model \"$MODEL\""
    echo " tried: $MODEL_SCRIPT"
    echo " either use a built in model type, or define \$MODEL_SCRIPT"
    exit 1
fi

# run the forecast
$MODEL_SCRIPT

# move the diag files
mkdir -p $DIAG_TMP_DIR
shopt -s nullglob
for f in *ocean_diag*; do
      mv $f $DIAG_TMP_DIR
done
