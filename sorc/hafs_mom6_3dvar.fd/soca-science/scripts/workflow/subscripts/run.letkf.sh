#!/bin/bash

# (C) Copyright 2020-2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

set -e

cat << EOF

#================================================================================
#================================================================================
# run.letkf.sh
#   Run the LETKF. If being done as part of an EnVAR, the members will be
#   recetnered around the VAR analysis.
#================================================================================

EOF

# Required environment variables:
envars=()
envars+=("ANA_DATE")
envars+=("ANA_ENS_DIR")
envars+=("ANARST_DIR")
envars+=("ANARST_ENS_DIR")
envars+=("BKGRST_ENS_DIR")
envars+=("DA_ENS_SIZE")
envars+=("DA_LETKF_OBS_LIST")
envars+=("DA_LETKF_RECENTER")
envars+=("DA_LETKF_WINDOW")
envars+=("DA_MODE")
envars+=("DA_SEAICE_ENABLED")
envars+=("DA_VARIABLES")
envars+=("MODEL_DATA_DIR")
envars+=("MPIRUN")
envars+=("OBS_DIR")
envars+=("OBS_OUT_ENS_DIR")
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

# skip if the ensemble analysis already exists
if [[ -d "$ANARST_ENS_DIR" && $(ls $ANARST_ENS_DIR -1q | wc -l) == "$DA_ENS_SIZE" ]]; then
    # TODO do a more thorough check to make sure each restart file is correctly created
    echo "LETKF analysis has already been created at :"
    echo "  $ANARST_ENS_DIR"
    exit 0
fi


# TODO move this to a common function
ln -s $SOCA_BIN_DIR/soca_{letkf,ensrecenter,checkpoint_model}.x .
cp $SOCA_DEFAULT_CFGS_DIR/{fields_metadata,soca_letkf,soca_ensrecenter,soca_checkpoint}.yaml .
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
domains='ocn'
if [[ "$DA_SEAICE_ENABLED" =~ [yYtT1] ]]; then
  domains='ocn_ice'
fi
sed -i "s;__DOMAINS__;$domains;g" soca_checkpoint.yaml
sed -i "s;__DA_VARIABLES__;$DA_VARIABLES;g" soca_checkpoint.yaml

# prepare letkf configuration file
# TODO, move this to a general script? it will be duplicated in var script
DA_WINDOW_HW=$((DA_LETKF_WINDOW/2 ))
DA_WINDOW_START=$(date -ud "$ANA_DATE - $DA_WINDOW_HW hours" +"%Y-%m-%dT%H:%M:%SZ")
DA_ANA_DATE=$(date -ud "$ANA_DATE" +"%Y-%m-%dT%H:%M:%SZ")
sed -i "s/__DA_WINDOW_START__/${DA_WINDOW_START}/g" soca_letkf.yaml
sed -i "s/__DA_WINDOW_LENGTH__/PT${DA_LETKF_WINDOW}H/g" soca_letkf.yaml
sed -i "s/__DA_ANA_DATE__/$DA_ANA_DATE/g" soca_letkf.yaml
sed -i "s;__DA_VARIABLES__;$DA_VARIABLES;g" soca_letkf.yaml

# prepare the __OBSERVATIONS__ section of the config yaml
echo "Preparing individual observations:"
touch obs.yaml
for o in $DA_LETKF_OBS_LIST; do
    # if obs directory doesn't exist, skip it
    [[ ! -d $OBS_DIR/$o ]] && continue

    # if obs directory is empty, skip
    [ "$(ls -A $OBS_DIR/$o/)" ] || continue

    # TODO only include obs listed in $OBS_${o}_LIST
    for f in $OBS_DIR/$o/*; do
        fname=$(basename $f)
        fname=${fname%.*}
        plat=${fname#*_}
        plat=${plat%_*}
        fname=${o}_${plat}
        mkdir -p obs_out/$fname
        (
            #cat $SOCA_DEFAULT_CFGS_DIR/obs/${o}_${plat}.yaml > ob.tmp
            #tmpl_fin="\$(experiment_dir)\/{{current_cycle}}\/${o}_${plat}.{{window_begin}}.nc4"
            #tmpl_fout="\$(experiment_dir)\/{{current_cycle}}\/\$(experiment).${o}_${plat}.{{window_begin}}.nc4"
            #sed -i "s;$tmpl_fin;obs_in/$fname.nc;g" ob.tmp
            #sed -i "s;$tmpl_fout;obs_out/$fname/$fname.nc;g" ob.tmp
            #cat ob.tmp >> obs.yaml
            cat $SOCA_DEFAULT_CFGS_DIR/obs/$o.yaml > ob.tmp
            sed -i "s;__OBS_PLAT__;$plat;g" ob.tmp
            sed -i "s;__OBS_IN_FILE__;obs_in/$fname.nc;g" ob.tmp
            sed -i "s;__OBS_OUT_DIR__;obs_out/$fname;g" ob.tmp
            cat ob.tmp >> obs.yaml
        )

        # link the input files
        mkdir -p obs_in
        ln -s $f obs_in/$fname.nc
    done
done
sed -i "s/^/  /g" obs.yaml
sed -i $'/__OBSERVATIONS__/{r obs.yaml\nd}' soca_letkf.yaml

# prepare the __ENSEMBLE__ section of the config yaml
touch ens.tmp
for ens in $(seq -f "%03g" $DA_ENS_SIZE); do
  echo "  - <<: *ens_member" >> ens.tmp
  echo "    ocn_filename: bkg_ens/$ens/MOM.res.nc" >> ens.tmp
  if [[ "$DA_SEAICE_ENABLED" =~ [yYtT1] ]]; then
     echo "    ice_filename: bkg_ens/$ens/cice.res.nc" >> ens.tmp
  fi
done
sed -i $'/__ENSEMBLE__/{r ens.tmp\nd}' soca_letkf.yaml

# link the ensemble
ln -s $BKGRST_ENS_DIR bkg_ens

# run the observer for letkf
sed "s;__MODE__;observer;g" soca_letkf.yaml > soca_letkf.observer.yaml
export OMP_NUM_THREADS=1
$MPIRUN ./soca_letkf.x soca_letkf.observer.yaml

# recombine output observation files
# TODO pull out into a common script callable by run.letkf.sh, and post.obs.sh
rm obs_in/*
for ob_dir in obs_out/*; do
    ob=$(basename $ob_dir)
    echo "  Processing $ob"
    for f in $ob_dir/*_0000.nc; do
        ob_var=${f##*/}
        ob_var=${ob_var%_*}

        # concatenate files
        $MPIRUN ${SOCA_SCIENCE_BIN_DIR}/obs_cat.x -i $ob_dir/${ob_var} -o $ob.tmp.nc
        ncks -h $ob.tmp.nc -A obs_in/$ob.nc
        rm $ob.tmp.nc
    done
    rm -r $ob_dir/*
done

cd obs_in
for inputnc in `ls insitu*.nc`; do
if [[ -f $inputnc ]]; then
echo 'yes ' $inputnc
ls -l $inputnc
${SOCA_SCIENCE_BIN_DIR}/qc0_checkinsitu.py -i $inputnc -o ./output.nc
rm $inputnc
mv output.nc $inputnc 
echo 'yes ' $inputnc
ls -l $inputnc
fi
done
cd -
#exit

# run the letkf
sed "s;__MODE__;solver;g" soca_letkf.yaml > soca_letkf.solver.yaml
export OMP_NUM_THREADS=1
$MPIRUN ./soca_letkf.x soca_letkf.solver.yaml

# we don't care about the obs output files here (because O-A is not
# calculated), and given InefficientDistribution the counts are wrong,
# so copy back the output from the observer instead.
# (at some point we'll want to actually save the O-A, but not today)
#rm -r obs_out

rm obs_in/*
for ob_dir in obs_out/*; do
    ob=$(basename $ob_dir)
    echo "  Processing $ob"
    #if [[ "$ob" != 'insitu_godas' ]]; then
    if [[ "${ob:0:6}" != 'insitu' ]]; then
      cp $ob_dir/${ob}_0000.nc  obs_in/$ob.nc
    fi
    #if [[ "$ob" == 'insitu_godas' ]]; then
    if [[ "${ob:0:6}" == 'insitu' ]]; then
      ncks -A -g MetaData $ob_dir/${ob}_S_0000.nc obs_in/${ob}.nc
      ncks -A -v sea_water_temperature $ob_dir/${ob}_T_0000.nc obs_in/${ob}.nc
      ncks -A -v sea_water_salinity $ob_dir/${ob}_S_0000.nc obs_in/${ob}.nc
    fi
done
rm -r obs_out

mv obs_in obs_out

# move copy of pre-recentered files... for later use in post processing
mkdir -p $ANA_ENS_DIR
for ens in $(seq 0 $DA_ENS_SIZE); do
    mv Data/ocn.letkf.ens.$ens.*.nc $ANA_ENS_DIR/ocn.letkf.ens.$ens.nc
    if [[ "$DA_SEAICE_ENABLED" =~ [yYtT1] ]]; then
       mv Data/ice.letkf.ens.$ens.*.nc $ANA_ENS_DIR/ice.letkf.ens.$ens.nc
    fi
done

# copy observtions out
mkdir -p $OBS_OUT_ENS_DIR
mv obs_out/* $OBS_OUT_ENS_DIR

echo "Done with LETKF"
