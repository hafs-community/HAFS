#!/bin/bash

# (C) Copyright 2020-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

set -e

cat << EOF

#================================================================================
#================================================================================
# post.state.sh
#   Post processing of the state files (analysis/background, mean/spread)
#================================================================================

EOF
SAVE_DIAG_ENABLED=${SAVE_DIAG_ENABLED:-F}

# Required environment variables:
envars=()
envars+=("ANA_DIR")
envars+=("ANA_ENS_DIR")
envars+=("ANARST_DIR")
envars+=("BKG_DIR")
envars+=("BKGRST_DIR")
envars+=("BKGRST_ENS_DIR")
envars+=("DA_ENABLED")
envars+=("DA_ENS_SIZE")
envars+=("DA_LETKF_ENABLED")
envars+=("DA_SEAICE_ENABLED")
envars+=("DA_OBGC_ENABLED")
envars+=("DATE")
envars+=("DIAG_DIR")
envars+=("DIAG_TMP_DIR")
envars+=("SAVE_DIAG_ENABLED")

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

# TODO move these to a user configurable
# TODO the ice variables are the ones for SIS2, make configurable
#  so we can use the onces from CICE
STATE_VARS="Temp,Salt,ave_ssh,u,v,h"
LETKF_STATE_VARS="Temp,Salt,ave_ssh,h" # TODO need to include u,v in state vars
SEAICE_STATE_VARS="aicen,hicen,hsnon"
OBGC_STATE_VARS="chl,po4,biomass_p"

# TODO, this might only be valid for MOM6/SIS2 model

# compress the background
mkdir -p $BKG_DIR/ctrl
ncks -O -7 -L 4 --ppc default=.2 -v $STATE_VARS \
    $BKGRST_DIR/MOM.res.nc \
    $BKG_DIR/ctrl/ocn.bkg.$ymdh.nc
[[ "$DA_OBGC_ENABLED" =~ [tTyT1] ]] && ncks -O -7 -L 4 \
    -v $OBGC_STATE_VARS \
    $BKGRST_DIR/MOM.res.nc \
    $BKG_DIR/ctrl/obgc.bkg.$ymdh.nc
[[ "$DA_SEAICE_ENABLED" =~ [tTyT1] ]] && ncks -O -7 -L 4 \
    --ppc default=.2 -v $SEAICE_STATE_VARS \
    $BKGRST_DIR/cice.res.nc \
    $BKG_DIR/ctrl/ice.bkg.$ymdh.nc

# compress the analysis and increment, only if we actually did DA
if [[ "$DA_ENABLED" == "T" ]]; then
    mkdir -p $ANA_DIR/ctrl
    ncks -O -7 -L 4 --ppc default=.2 -v $STATE_VARS \
        $ANARST_DIR/MOM.res.nc \
        $ANA_DIR/ctrl/ocn.ana.$ymdh.nc
    [[ "$DA_OBGC_ENABLED" =~ [tTyT1] ]] && ncks -O -7 -L 4 \
        -v $OBGC_STATE_VARS \
        $ANARST_DIR/MOM.res.nc \
        $ANA_DIR/ctrl/obgc.ana.$ymdh.nc
    [[ "$DA_SEAICE_ENABLED" =~ [tTyT1] ]] && ncks -O -7 -L 4 \
        --ppc default=.2 -v $SEAICE_STATE_VARS \
        $ANARST_DIR/cice.res.nc \
        $ANA_DIR/ctrl/ice.ana.$ymdh.nc
    if [[ "$DA_MODE" != "letkf" && "$DA_SAVE_INC" =~ [tTyT1] ]]; then
        # compress outer loop increments
        mkdir -p $INCR_DIR/ctrl
        lof=`cd $INCR_TMP_DIR && ls *.nc`
        for f in $lof; do
          ncks -O -7 -L 4 --ppc default=.2 $INCR_TMP_DIR/$f $INCR_DIR/ctrl/$f
        done
    fi
    if [[ "$DA_ENSBDIAG_ENABLED" =~ [tTyT1] ]]; then
        mkdir -p $DIAGB_DIR/ens
        mv $DIAGB_TMP_DIR/*ens_variance*.nc $DIAGB_DIR/ens/
    fi
    if [[ "$DA_MODE" != "letkf" && "$DA_DIAGB_ENABLED" =~ [tTyT1] ]]; then
        # compress bump diagnostic output
        mkdir -p $DIAGB_DIR/ctrl
        lof=`cd $DIAGB_TMP_DIR && ls *.nc`
        for f in $lof; do
          ncks -O -7 -L 4 --ppc default=.2 $DIAGB_TMP_DIR/$f $DIAGB_DIR/ctrl/$f
        done
    fi
fi

# compress the ensemble
if [[ "$DA_LETKF_ENABLED" =~ [yYtT1] ]]; then

    # ana mean
    mkdir -p $ANA_DIR/ens
    ncks -O -7 -L 4 --ppc default=.2 -v $LETKF_STATE_VARS \
        $ANA_ENS_DIR/ocn.letkf.ens.0.nc \
        $ANA_DIR/ens/ocn.ana.ens_mean.$ymdh.nc

    [[ "$DA_OBGC_ENABLED" =~ [tTyT1] ]] &&
        ncks -O -7 -L 4 -v $OBGC_STATE_VARS \
             $ANA_ENS_DIR/ocn.letkf.ens.0.nc \
             $ANA_DIR/ens/obgc.ana.ens_mean.$ymdh.nc

    [[ "$DA_SEAICE_ENABLED" =~ [tTyT1] ]] &&
        ncks -O -7 -L 4 --ppc default=.2 -v $SEAICE_STATE_VARS \
             $ANA_ENS_DIR/ice.letkf.ens.0.nc \
             $ANA_DIR/ens/ice.ana.ens_mean.$ymdh.nc

    # ana sprd
    mkdir ana_sprd
    for ens in $(seq $DA_ENS_SIZE); do
        ncbo -O $ANA_ENS_DIR/ocn.letkf.ens.$ens.nc \
            $ANA_ENS_DIR/ocn.letkf.ens.0.nc \
            ana_sprd/$ens.nc
    done
    nces -y rmssdn -O -7 -L 4 --ppc default=.4 \
        ana_sprd/*.nc \
        $ANA_DIR/ens/ocn.ana.ens_sprd.$ymdh.nc

    if [[ "$DA_OBGC_ENABLED" =~ [tTyT1] ]]; then
        for ens in $(seq $DA_ENS_SIZE); do
            ncbo -O -v $OBGC_STATE_VARS $ANA_ENS_DIR/ocn.letkf.ens.$ens.nc \
                $ANA_ENS_DIR/ocn.letkf.ens.0.nc \
                ana_sprd/obgc.$ens.nc
        done
        nces -y rmssdn -O -7 -L 4 ana_sprd/obgc.*.nc \
            $ANA_DIR/ens/obgc.ana.ens_sprd.$ymdh.nc
    fi

    if [[ "$DA_SEAICE_ENABLED" =~ [tTyT1] ]]; then
        for ens in $(seq $DA_ENS_SIZE); do
            ncbo -O $ANA_ENS_DIR/ice.letkf.ens.$ens.nc \
                $ANA_ENS_DIR/ice.letkf.ens.0.nc \
                ana_sprd/ice.$ens.nc
        done
        nces -y rmssdn -O -7 -L 4 --ppc default=.4 \
            ana_sprd/ice.*.nc \
            $ANA_DIR/ens/ice.ana.ens_sprd.$ymdh.nc
    fi

    # background mean
    mkdir -p $BKG_DIR/ens
    nces  -O -7 -L 4 --ppc default=.2 -v $STATE_VARS \
        $BKGRST_ENS_DIR/???/MOM.res.nc \
        $BKG_DIR/ens/ocn.bkg.ens_mean.$ymdh.nc
    [[ "$DA_OBGC_ENABLED" =~ [tTyT1] ]] &&
       nces  -O -7 -L 4 -v $OBGC_STATE_VARS \
             $BKGRST_ENS_DIR/???/MOM.res.nc \
             $BKG_DIR/ens/obgc.bkg.ens_mean.$ymdh.nc
    [[ "$DA_SEAICE_ENABLED" =~ [tTyT1] ]] &&
       nces  -O -7 -L 4 --ppc default=.2 -v $SEAICE_STATE_VARS \
             $BKGRST_ENS_DIR/???/cice.res.nc \
             $BKG_DIR/ens/ice.bkg.ens_mean.$ymdh.nc

    # background spread
    mkdir -p bkg_sprd
    for ens in $(seq -f "%03g" $DA_ENS_SIZE); do
        ncbo -O -v $STATE_VARS $BKGRST_ENS_DIR/$ens/MOM.res.nc \
                               $BKG_DIR/ens/ocn.bkg.ens_mean.$ymdh.nc \
                               bkg_sprd/$ens.nc
    done
    nces -y rmssdn -O -7 -L 4 --ppc default=.4 bkg_sprd/*.nc $BKG_DIR/ens/ocn.bkg.ens_sprd.$ymdh.nc

    if [[ "$DA_OBGC_ENABLED" =~ [tTyT1] ]]; then
        for ens in $(seq -f "%03g" $DA_ENS_SIZE); do
            ncbo -O -v $OBGC_STATE_VARS $BKGRST_ENS_DIR/$ens/MOM.res.nc \
                                   $BKG_DIR/ens/obgc.bkg.ens_mean.$ymdh.nc \
                                   bkg_sprd/obgc.$ens.nc
        done
        nces -y rmssdn -O -7 -L 4 bkg_sprd/obgc.*.nc $BKG_DIR/ens/obgc.bkg.ens_sprd.$ymdh.nc
    fi

    if [[ "$DA_SEAICE_ENABLED" =~ [tTyT1] ]]; then
        for ens in $(seq -f "%03g" $DA_ENS_SIZE); do
            ncbo -O -v $SEAICE_STATE_VARS $BKGRST_ENS_DIR/$ens/cice.res.nc \
                                   $BKG_DIR/ens/ice.bkg.ens_mean.$ymdh.nc \
                                   bkg_sprd/$ens.nc
        done
        nces -y rmssdn -O -7 -L 4 --ppc default=.4 bkg_sprd/*.nc $BKG_DIR/ens/ice.bkg.ens_sprd.$ymdh.nc
    fi
fi

# Compress and move all the diagnostic files to EXP_DIR

if [[ "$SAVE_DIAG_ENABLED" =~ [yYtT1] ]]; then
    mkdir -p $DIAG_DIR
    shopt -s nullglob # enable nullglob
    for DIAG_FILE in "$DIAG_TMP_DIR"/*.nc
    do
        ncks -O -7 -L 4 --ppc default=.2 \
            $DIAG_FILE \
            $DIAG_DIR/$(basename $DIAG_FILE)
    done
    shopt -u nullglob # disable nullglob
fi
