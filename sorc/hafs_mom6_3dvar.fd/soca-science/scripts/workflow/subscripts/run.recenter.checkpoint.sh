#!/bin/bash

# (C) Copyright 2021-2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

set -e

cat << EOF

#================================================================================
#================================================================================
# run.recenter.checkpoint.sh
#   Recenter and checkpoint an ensemble around a deterministic member.
#================================================================================

EOF

# Required environment variables:
envars=()
envars+=("ANA_DATE")
envars+=("ANARST_DIR")
envars+=("ANARST_ENS_DIR")
envars+=("BKGRST_ENS_DIR")
envars+=("DA_ENS_SIZE")
envars+=("DA_MODE")
envars+=("DA_SEAICE_ENABLED")
envars+=("DA_VARIABLES")
envars+=("MODEL")
envars+=("MODEL_DATA_DIR")
envars+=("MPIRUN")
envars+=("SOCA_BIN_DIR")
envars+=("SOCA_DEFAULT_CFGS_DIR")
envars+=("SOCA_SCIENCE_BIN_DIR")
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

# skip if the ensemble has already been recentered
if [[ -d "$ANARST_ENS_DIR" && $(ls $ANARST_ENS_DIR -1q | wc -l) == "$DA_ENS_SIZE" ]]; then
    # TODO do a more thorough check to make sure each restart file is correctly created
    echo "Recentered ensemble has already been created at :"
    echo "  $ANARST_ENS_DIR"
    exit 0
fi


# TODO move this to a common function
ln -s $SOCA_BIN_DIR/soca_{ensrecenter,checkpoint_model}.x .
cp $SOCA_DEFAULT_CFGS_DIR/{fields_metadata,soca_ensrecenter,soca_checkpoint}.yaml .
ln -s  $MODEL_CFG_DIR/* .
export FCST_RESTART=1
export FCST_START_TIME=$ANA_DATE
export FCST_RST_OFST=24
. input.nml.sh > mom_input.nml
mkdir -p Data INPUT OUTPUT RESTART
(cd INPUT && ln -sf $MODEL_DATA_DIR/* .)
ln -s $MODEL_DATA_DIR/../soca/* . # TODO use proper path
ln -s $SOCA_STATIC_DIR/* .

# Set domain and variables
domains='ocn'
if [[ "$DA_SEAICE_ENABLED" =~ [yYtT1] ]]; then
  domains='ocn_ice'
fi

# prepare checkpoint configuration file
sed -i "s;__DOMAINS__;$domains;g" soca_checkpoint.yaml
sed -i "s;__DA_VARIABLES__;$DA_VARIABLES;g" soca_checkpoint.yaml

# Analysis date
DA_ANA_DATE=$(date -ud "$ANA_DATE" +"%Y-%m-%dT%H:%M:%SZ")

# link the ensemble
ln -s $BKGRST_ENS_DIR bkg_ens

# Set output descriptor according to the DA applications used
case $DA_PERTURBATION_MODEL  in
  letkf)
    output_descriptor="letkf";;

  none)
    output_descriptor="nopert";;

  *)
    printf "ERROR: $DA_PERTURBATION_MODEL is not a valid perturbation"
    exit 1;;
esac

# TODO remove if statement when checkpointing is done outside of
#      recentering (recentering is not done if using letkf alone)
ln -s $ANARST_DIR ens_center
if [[ "$DA_MODE" == letkf ]]; then
    ln -sf $ANA_ENS_DIR $WORK_DIR/ana_ens
else
    echo "doing a recenter on the deterministic member"
    # recenter the members
    echo "recentering around deterministic member"
    mkdir -p ana_ens
    sed -i "s;__DOMAINS__;$domains;g" soca_ensrecenter.yaml
    sed -i "s/__DA_ANA_DATE__/$DA_ANA_DATE/g" soca_ensrecenter.yaml
    sed -i "s;__DA_VARIABLES__;$DA_VARIABLES;g" soca_ensrecenter.yaml
    sed -i "s;__OUTPUT_DESCRIPTOR__;${output_descriptor};g" soca_ensrecenter.yaml
    echo "" > ens.tmp
    ln -s $BKGRST_ENS_DIR ens_in
    for ens in $(seq -f "%03g" $DA_ENS_SIZE ); do
        echo "  - <<: *ens_member" >> ens.tmp
        echo "    ocn_filename: ./$ens/MOM.res.nc" >> ens.tmp
        if [[ "$DA_SEAICE_ENABLED" =~ [yYtT1] ]]; then
           echo "    ice_filename: ./$ens/cice.res.nc" >> ens.tmp
        fi
    done
    sed -i $'/__ENSEMBLE__/{r ens.tmp\nd}' soca_ensrecenter.yaml
    $MPIRUN ./soca_ensrecenter.x soca_ensrecenter.yaml
fi

# perform checkpoint on each member
# TODO generalize this into a separate script? there
#  is duplicate here with var script
if [[ $DA_MODE == letkf ]]; then
    mem0=0
else
    mem0=1
fi

for ens in $(seq $mem0 $DA_ENS_SIZE); do

    member_dir=$(printf "%03g" $ens)
    # mean member (number 0) is handled differently
    if [[ $ens == 0 ]]; then
        # don't save separate mean member if doing a hybrid DA
        [[ "$DA_LETKF_RECENTER" =~ [yYtT1] ]] && continue
        ana_dir=$ANARST_DIR
        bkg_dir=$BKGRST_DIR
        echo $ana_dir
        echo $bkg_dir
    else
        ana_dir=$ANARST_ENS_DIR/$member_dir
        bkg_dir=$BKGRST_ENS_DIR/$member_dir
    fi

    # run checkpoint
    echo $bkg_dir
    ln -sf $bkg_dir RESTART_IN
    ls $WORK_DIR/ana_ens
    member_chkp=`ls $WORK_DIR/ana_ens/ocn.${output_descriptor}.ens.$ens.*nc`
    ln -sf $member_chkp checkpoint_ana.nc

    # Sea ice
    # TODO(Guillaume) implement checkpointing for ice.
    #                 currently does nothing ...
    if [[ "$DA_SEAICE_ENABLED" =~ [yYtT1] ]]; then
       ln -sf $WORK_DIR/ana_ens/ice.${output_descriptor}.ens.$((10#$ens)).*nc ice.checkpoint_ana.nc
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

    mkdir -p $ana_dir

    if [[ "$DA_SEAICE_ENABLED" =~ [yYtT1] ]]; then
       ln -sf $SOCA_DEFAULT_CFGS_DIR/soca_rescale_seaice.yaml .
       $SOCA_SCIENCE_BIN_DIR/soca_checkpoint_seaice.sh $MODEL $ana_dir ice.checkpoint_ana.nc
    fi

    mv RESTART/* $ana_dir/
    rm *checkpoint_ana.nc
    rm RESTART_IN

    # link other restart files that weren't changed from the background
    for f in $bkg_dir/*; do
        f2=$(basename $f)
        [[ ! -f $ana_dir/$f2 ]] && ln -s $f $ana_dir/
    done
done


echo "Done with RECENTERING"
