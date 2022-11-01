#!/bin/bash

# (C) Copyright 2020-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

set -e

cat << EOF

#================================================================================
#================================================================================
# run.var.sh
#   Run either a 3DVAR or 3DEnVAR
#================================================================================

EOF

# Required environment variables:
envars=()
envars+=("ANA_DATE")
envars+=("ANARST_DIR")
envars+=("BKGRST_DIR")
envars+=("BKGRST_ENS_DIR")
envars+=("DA_CHKPT_WITH_MODEL")
envars+=("DA_DUALRES_ENABLE")
envars+=("DA_MODE")
envars+=("DA_SEAICE_ENABLED")
envars+=("DA_VARIABLES")
envars+=("DA_VARIABLES_OCN")
envars+=("DA_VARIABLES_ICE")
envars+=("DA_DIAGB_ENABLED")
envars+=("DA_DIAGB_DIRAC_STEP")
envars+=("DA_DUALRES_ENABLE")
envars+=("DA_DUALRES_SKIP")
envars+=("MODEL")
envars+=("MODEL_CFG_DIR")
envars+=("MODEL_DATA_DIR")
envars+=("MPIRUN")
envars+=("OBS_DIR")
envars+=("OBS_OUT_CTRL_DIR")
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

# skip running the var if it has already been run
if [[ -d "$ANARST_DIR" && $(ls $ANARST_DIR -1q | wc -l) -gt 0 ]]; then
  echo "VAR analysis has already been created at :"
  echo "  $ANARST_DIR"
  exit 0
fi

# which mode are we running in?
case "$DA_MODE" in
  3dvar)
    echo "Doing 3DVAR"
    VAR_CFG_FILE=$SOCA_DEFAULT_CFGS_DIR/soca_3dvar.yaml
    [[ "$DA_DUALRES_ENABLE" == T ]] && VAR_CFG_FILE=$SOCA_DEFAULT_CFGS_DIR/soca_3dvardual.yaml
    DO_ENS=0
    ;;

  3dhyb)
    echo "Doing hybrid-EnVAR"
    if [[ "$DA_ENSBDIAG_ENABLED" =~ [yYtT1] ]]; then
        # Hybrid because the auto-covariance are derived from the ensemble
        # but the cross covariances are derived from the balance operator
        VAR_CFG_FILE=$SOCA_DEFAULT_CFGS_DIR/soca_3dvar_ensdiagB.yaml
        # link the ensemble variance files:
        ln -s $DIAGB_TMP_DIR/ocn.ens_variance.*.nc ocn.bkgerror.nc
        ln -s $DIAGB_TMP_DIR/ice.ens_variance.*.nc ice.bkgerror.nc
    else
        VAR_CFG_FILE=$SOCA_DEFAULT_CFGS_DIR/soca_3dhyb.yaml
    fi
    DO_ENS=1
    ;;

  *)
    echo "ERROR, \$DA_MODE $DA_MODE cannot be handled by this script right now"
    exit 1
    ;;
esac


# TODO move this to a common function
ln -s $SOCA_BIN_DIR/soca_{var,checkpoint_model,dirac}.x .
ln -s $SOCA_SCIENCE_BIN_DIR/soca_seaice.py .
ln -s $SOCA_SCIENCE_BIN_DIR/soca_var2dirac.x .
cp $SOCA_DEFAULT_CFGS_DIR/{fields_metadata,soca_checkpoint}.yaml .
ln -s $SOCA_BIN_DIR/soca_ocn_bgc.py .
cp $VAR_CFG_FILE var.yaml
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
[[ "$DA_DUALRES_ENABLE" == T ]] && {
    ln -s $SOCA_STATIC_DIR/subgeom/MOM_input_sub .
    ln -s $SOCA_STATIC_DIR/subgeom/MOM_override_sub .
    ln -s $SOCA_STATIC_DIR/subgeom/input_sub.nml .
    ln -s $SOCA_STATIC_DIR/subgeom/soca_gridspec_sub.nc .
}

# prepare 3dvar configuration file
# TODO, move this to a general script? it is duplicated in the letkf
(( DA_WINDOW_HW=FCST_LEN/2 ))
DA_WINDOW_START=$(date -ud "$ANA_DATE - $DA_WINDOW_HW hours" +"%Y-%m-%dT%H:%M:%SZ")
DA_ANA_DATE=$(date -ud "$ANA_DATE" +"%Y-%m-%dT%H:%M:%SZ")
sed -i "s/__DA_WINDOW_START__/${DA_WINDOW_START}/g" var.yaml
sed -i "s/__DA_WINDOW_LENGTH__/PT${FCST_LEN}H/g" var.yaml
sed -i "s/__DA_ANA_DATE__/$DA_ANA_DATE/g" var.yaml

# Set domain and variables
domains='ocn'
dirac_vars='SSH T S'
if [[ "$DA_SEAICE_ENABLED" =~ [yYtT1] ]]; then
  domains='ocn_ice'
  dirac_vars='SSH T S aice hice'
fi
sed -i "s;__DOMAINS__;$domains;g" var.yaml
sed -i "s;__DA_VARIABLES__;$DA_VARIABLES;g" var.yaml
sed -i "s;__DA_VARIABLES_OCN__;$DA_VARIABLES_OCN;g" var.yaml
sed -i "s;__DA_VARIABLES_ICE__;$DA_VARIABLES_ICE;g" var.yaml

sed -i "s;__DOMAINS__;$domains;g" soca_checkpoint.yaml
sed -i "s;__DA_VARIABLES__;$DA_VARIABLES;g" soca_checkpoint.yaml

# prepare the __OBSERVATIONS__ section of the config yaml
echo "Preparing individual observations:"
touch obs.yaml
for o in $OBS_LIST_OCN; do
    # if obs directory doesn't exist, skipp this ob
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
            cat $SOCA_DEFAULT_CFGS_DIR/obs/$o.yaml > ob.tmp
            sed -i "s;__OBS_PLAT__;$plat;g" ob.tmp
            sed -i "s;__OBS_IN_FILE__;$f;g" ob.tmp
            sed -i "s;__OBS_OUT_DIR__;obs_out/${odir};g" ob.tmp
            cat ob.tmp >> obs.yaml

            #cat $SOCA_DEFAULT_CFGS_DIR/obs/${o}_${plat}.yaml > ob.tmp
            #tmpl_fin="\$(experiment_dir)\/{{current_cycle}}\/${o}_${plat}.{{window_begin}}.nc4"
            #tmpl_fout="\$(experiment_dir)\/{{current_cycle}}\/\$(experiment).${o}_${plat}.{{window_begin}}.nc4"
            #sed -i "s;$tmpl_fin;$f;g" ob.tmp
            #sed -i "s;$tmpl_fout;obs_out/${odir}/${odir}.nc;g" ob.tmp
            #cat ob.tmp >> obs.yaml
        )
    done
done
sed -i "s/^/  /g" obs.yaml
sed -i $'/__OBSERVATIONS__/{r obs.yaml\nd}' var.yaml

# prepare the __ENSEMBLE__ section of the config yaml
if [[ "$DO_ENS" == 1 ]]; then
  touch ens.tmp
  for ens in $(seq -f "%03g" $DA_ENS_SIZE); do
    echo "- <<: *ens_member" >> ens.tmp
    echo "  ocn_filename: bkg_ens/$ens/MOM.res.nc" >> ens.tmp
    if [[ "$DA_SEAICE_ENABLED" =~ [yYtT1] ]]; then
       echo "  ice_filename: bkg_ens/$ens/cice.res.nc" >> ens.tmp
    fi
  done
  sed -i "s/^/        /g" ens.tmp
  sed -i $'/__ENSEMBLE__/{r ens.tmp\nd}' var.yaml
fi

# prepare background
mkdir -p Data
ln -sf $BKGRST_DIR bkg
ln -s bkg RESTART_IN
[[ "$DO_ENS" == 1 ]] && ln -s $BKGRST_ENS_DIR bkg_ens

# TODO Check if seaice aggregated background exist and create if it does not.

# run the var
export OOPS_TRACE=0
export OMP_NUM_THREADS=1
$MPIRUN ./soca_var.x var.yaml

echo "YONGBING"

# diagnose the B-matrix
if [[ "$DA_DIAGB_ENABLED" == [yYtT1] ]]; then
   for v in SSH T S ; do
     # create yaml file for dirac
     ./soca_var2dirac.x -v $v -o dirac.yaml \
                      -l 1 \
                      -s $DA_DIAGB_DIRAC_STEP \
                      -i var.yaml \
                      -d $domains \
                      -a "$DA_VARIABLES" \
                      -t "$DA_ANA_DATE"
     # apply B to diracs
     $MPIRUN ./soca_dirac.x dirac.yaml
  done
  # move files related to B
  mkdir -p $DIAGB_TMP_DIR
  shopt -s nullglob # enable nullglob
  for f in Data/*.{loc,dirac}*.nc
  do
    echo $f
    mv $f $DIAGB_TMP_DIR
  done
  shopt -u nullglob # disable nullglob
  mv soca_bkgerr*.nc $DIAGB_TMP_DIR

fi

# move increment files
mkdir -p $INCR_TMP_DIR
mv Data/*.var.iter* $INCR_TMP_DIR

# perform checkpointing
# Ocean
ln -sf Data/ocn.3dvar.an*.nc checkpoint_ana.nc
# Sea ice
# TODO (Guillaume?) needed in the checkpoint application but not used, remove.
if [[ "$DA_SEAICE_ENABLED" == [yYtT1] ]]; then
   ln -sf Data/ice.3dvar.an*.nc ice.checkpoint_ana.nc
fi

# Dump ocean analysis into MOM6 restart
if [[ "$DA_CHKPT_WITH_MODEL" =~ [yYtT1] ]]; then
   # Use MOM6 to checkpoint
   $MPIRUN ./soca_checkpoint_model.x soca_checkpoint.yaml
else
   # Simply dump ocean analysis in restarts (necessary when restarts are "dynamic_symmetric")
   cp $SOCA_DEFAULT_CFGS_DIR/soca_checkpoint_regional.yaml .
   $SOCA_SCIENCE_BIN_DIR/soca_domom6_action.py checkpoint
fi

# move this to a directory inside the SCRATCH directory
mkdir -p $ANARST_DIR

# Dump sea ice analysis into cice4-5-6 or sis2 restart
if [[ "$DA_SEAICE_ENABLED" =~ [yYtT1] ]]; then
  ln -sf $SOCA_DEFAULT_CFGS_DIR/soca_rescale_seaice.yaml .
  ice_soca_ana=`ls ./Data/ice.*.an.*.nc`
  $SOCA_SCIENCE_BIN_DIR/soca_checkpoint_seaice.sh $MODEL $ANARST_DIR $ice_soca_ana
fi

mv RESTART/* $ANARST_DIR/
for f in $BKGRST_DIR/*; do
    f2=$(basename $f)
    [[ ! -f $ANARST_DIR/$f2 ]] && ln -s $f $ANARST_DIR/
done

# Dump ocean bgc analysis into mom6 restart
if [[ "$DA_OBGC_ENABLED" =~ [yYtT1] ]]; then
  ln -sf $SOCA_DEFAULT_CFGS_DIR/soca_checkpoint_ocn_bgc.yaml .
  ocn_bgc_soca_ana=`ls ./Data/ocn.3dvar.an*.nc`
  $SOCA_SCIENCE_BIN_DIR/soca_checkpoint_ocn_bgc.sh $MODEL $ANARST_DIR $ocn_bgc_soca_ana $ANARST_DIR
  mv $ANARST_DIR/MOM.res.tmp.nc $ANARST_DIR/MOM.res.nc
fi

mkdir -p $OBS_OUT_CTRL_DIR
mv obs_out/* $OBS_OUT_CTRL_DIR

echo "done with VAR"
