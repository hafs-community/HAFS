#!/bin/bash

# (C) Copyright 2020-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.


# TODO this neeeds some serious cleanup


# Required environment variables:
# TODO, move these to run.fcst?
envars=()
envars+=("DATE")
envars+=("FCST_RESTART")
envars+=("FCST_START_TIME")
envars+=("FORC_DIR")
envars+=("MODEL_CFG_DIR")
envars+=("MODEL_DATA_DIR")
envars+=("MODEL_EXE")
envars+=("MODEL_RST_DIR_IN")
envars+=("MODEL_RST_DIR_OUT")
envars+=("MPIRUN")
envars+=("FCST_LEN")

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

ymdh=$(date -ud "$DATE" +%Y%m%d%H )

# prepare the working directory (which we assume we are already in)
mkdir -p OUTPUT

# output restarts
# TODO, make sure above this level that directory exists
mkdir -p $MODEL_RST_DIR_OUT
ln -s $MODEL_RST_DIR_OUT RESTART

# main configuration files
ln -sf $MODEL_CFG_DIR/* .
. input.nml.sh > input.nml

# diag_table
cp diag_table diag_table.tmp
rm diag_table
mv diag_table.tmp diag_table
sed -i "s;YYYY;${ymdh:0:4};g" diag_table
sed -i "s;MM;${ymdh:4:2};g" diag_table
sed -i "s;DD;${ymdh:6:2};g" diag_table
sed -i "s;HH;${ymdh:8:2};g" diag_table

# forcing
mkdir -p FORC
( cd FORC && ln -sf $FORC_DIR/atm* atm.nc )

# INPUT files
mkdir -p INPUT
(cd INPUT && ln -sf $MODEL_DATA_DIR/* .)# coldstart ?
if [[ "$FCST_RESTART" == 0 ]]; then
    #( cd INPUT && ln -sf $MODEL_IC_DIR/ocean.{T,S}.nc .)
    ( cd INPUT && ln -sf $MODEL_IC_DIR/rtofs_*.nc .)
    echo "USE Maria ICs in INPUT"
else
    ln -sf $MODEL_RST_DIR_IN/ RESTART_IN
fi

# TODO, limit number of PEs
OMP_NUM_THREADS=1 $MPIRUN $MODEL_EXE

# TODO move diag files

# Create cice.res.nc (used as input to soca)
$SOCA_BIN_DIR/soca_seaice.py -f $MODEL_RST_DIR_OUT/ice_model.res.nc \
                             -m sis2 \
                             -a model2soca \
                             -o $MODEL_RST_DIR_OUT/cice.res.nc  # Aggregate ice categories
