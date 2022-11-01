#!/bin/bash

# (C) Copyright 2020-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

set -e

cat << EOF

#================================================================================
#================================================================================
# run.hofx.sh
#   Run the standalone HofX application
#================================================================================

EOF

# Required environment variables:
envars=()
envars+=("BKG_FILE")
envars+=("DA_SEAICE_ENABLED")
envars+=("MODEL_CFG_DIR")
envars+=("MODEL_DATA_DIR")
envars+=("MPIRUN")
envars+=("OBS_DIR")
envars+=("OBS_LIST_OCN")
envars+=("OBS_OUT_CTRL_DIR")
envars+=("SEAICE_BKG_FILE")
envars+=("SOCA_BIN_DIR")
envars+=("SOCA_DEFAULT_CFGS_DIR")
envars+=("SOCA_STATIC_DIR")
envars+=("WORK_DIR")
envars+=("DA_OBGC_ENABLED")

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

# skip running hofx if it has already been run
if [[ -d "$OBS_OUT_CTRL_DIR" && $(ls $OBS_OUT_CTRL_DIR -1q | wc -l) -gt 0 ]]; then
  echo "HofX has already been created at :"
  echo " $OBS_OUT_CTRL_DIR"
  exit 0
fi

# TODO handle ensembles??

# TODO move this to a common function, it repeated a lot across scripts
ln -s $SOCA_BIN_DIR/soca_hofx3d.x .
cp $SOCA_DEFAULT_CFGS_DIR/{fields_metadata,soca_hofx}.yaml .
ln -s  $MODEL_CFG_DIR/* .
export FCST_RESTART=1
export FCST_START_TIME=$ANA_DATE
export FCST_RST_OFST=24
if [[ "$DA_OBGC_ENABLED" =~ [yYtT1] ]]; then
    . input_bgc.nml.sh > mom_input.nml
else
    . input.nml.sh > mom_input.nml
fi
mkdir -p OUTPUT RESTART
mkdir -p INPUT
(cd INPUT && ln -sf $MODEL_DATA_DIR/* .)
ln -s $MODEL_DATA_DIR/../soca/* . # TODO use proper path
ln -s $SOCA_STATIC_DIR/* .


# prepare the configuration file
# TODO again, move this to a common function, it's duplicated across scripts
DA_WINDOW_HW=$((FCST_LEN/2))
DA_WINDOW_START=$(date -ud "$ANA_DATE - $DA_WINDOW_HW hours" +"%Y-%m-%dT%H:%M:%SZ")
DA_ANA_DATE=$(date -ud "$ANA_DATE" +"%Y-%m-%dT%H:%M:%SZ")
sed -i "s/__DA_WINDOW_START__/${DA_WINDOW_START}/g" soca_hofx.yaml
sed -i "s/__DA_WINDOW_LENGTH__/PT${FCST_LEN}H/g" soca_hofx.yaml
sed -i "s/__DA_ANA_DATE__/$DA_ANA_DATE/g" soca_hofx.yaml

# prepare the __OBSERVATIONS__ section of the config yaml
# TODO sigh, this TOO should be move into a common function
echo "Preparing individual observations:"
touch obs.yaml
for o in $OBS_LIST_OCN; do
    # if obs directory doesn't exist, skip it
    [[ ! -d $OBS_DIR/$o ]] && continue

    # if obs directory is empty, skip
    [ "$(ls -A $OBS_DIR/$o/)" ] || continue

    # TODO only include obs listed in $OBS_${o}_LIST
    for f in $OBS_DIR/$o/*; do
        odir=$(basename $f)
        odir=${odir%.*}
        plat=${odir#*_}
        plat=${plat%_*}
        odir=${o}_${plat}
        printf " %-24s %s\n" ${odir} $f
        mkdir -p obs_out/$odir
        (
            cat $SOCA_DEFAULT_CFGS_DIR/obs/${o}_${plat}.yaml > ob.tmp
            tmpl_fin="\$(experiment_dir)\/{{current_cycle}}\/${o}_${plat}.{{window_begin}}.nc4"
            tmpl_fout="\$(experiment_dir)\/{{current_cycle}}\/\$(experiment).${o}_${plat}.{{window_begin}}.nc4"
            sed -i "s;$tmpl_fin;$f;g" ob.tmp
            sed -i "s;$tmpl_fout;obs_out/${odir}/${odir}.nc;g" ob.tmp
            cat ob.tmp >> obs.yaml
        )
    done
done
sed -i "s/^/  /g" obs.yaml
sed -i $'/__OBSERVATIONS__/{r obs.yaml\nd}' soca_hofx.yaml

mkdir -p bkg
ln -s $BKG_FILE bkg/
sed -i "s;__BKG_FILE__;$(basename $BKG_FILE);g" soca_hofx.yaml

domains='ocn'
if [[ "$DA_SEAICE_ENABLED" =~ [yYtT1] ]]; then
  domains='ocn_ice'
  ln -s $SEAICE_BKG_FILE bkg/
  sed -i "s;__SEAICE_BKG_FILE__;$(basename $SEAICE_BKG_FILE);g" soca_hofx.yaml
fi
if [[ "$DA_OBGC_ENABLED" =~ [yYtT1] ]]; then
  domains='ocn_ice_bgc'
fi
sed -i "s;__DOMAINS__;$domains;g" soca_hofx.yaml

# run the hofx
export OMP_NUM_THREADS=1
$MPIRUN ./soca_hofx3d.x soca_hofx.yaml

# move the results to a directory inside the SCRATCH directory
mkdir -p $OBS_OUT_CTRL_DIR
mv obs_out/* $OBS_OUT_CTRL_DIR

echo "done with HofX"
