#!/bin/sh
################################################################################
# Script Name: exhafs_atm_vi.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the HAFS atmopsheric vortex initialization steps to
#   relocate and modify the storm vortex (if desired).
# History:
#   01/25/2022: Enable vortex initialization tasks in HAFS application workflow
#   02/06/2022: Use hafs_datool to prepare VI input and interpolate VI analysis
#               back to restart files
#   03/02/2022: Enable handling the regional moving nesting configuration
#   06/12/2023: Improvements for HAFSv1 operational implementation
#      10/2026: Expand to support vortex initialization for multiple storms
#      11/2026: Merge all storms' VI analyses into the restart files with one
#               call of the Python/MPI hafsvi_postproc after the storm loop
#      11/2026: Prepare the VI input of all storms with hafsvi_preproc
#               (one call per VI box, restart and resolution) before the loop
#      11/2026: Hybrid merge in Stage 4 (VI analysis in each storm's core, VI
#               increment storm_anl - vi_inp outside), so storms whose VI
#               domains overlap no longer erase each other's vortex
#      11/2026: Stage 4 uses the VI input the VI started from for every
#               domain, also the parent of a nested run (not _grid01)
#   11/04/2026: hafsvi_preproc and hafsvi_postproc from the Python hafs_datool
#               package (sorc/hafs_tools.fd/sorc/hafs_datool_py)
# Condition codes:
#   == 0 : success
#   != 0 : fatal error encounted
################################################################################
set -x -o pipefail
vi_force_cold_start=${vi_force_cold_start:-no}
vi_min_wind_for_init=${vi_min_wind_for_init:-9} # m/s
vi_warm_start_vmax_threshold=$(printf "%.0f" ${vi_warm_start_vmax_threshold:-20}) # m/s
vi_bogus_vmax_threshold=$(printf "%.0f" ${vi_bogus_vmax_threshold:-50}) # m/s
vi_storm_env=${vi_storm_env:-init} # init: from gfs/gdas init; pert: from the same source for the storm perturbation
vi_storm_relocation=${vi_storm_relocation:-yes}
vi_storm_modification=${vi_storm_modification:-yes}
vi_adjust_intensity=${vi_adjust_intensity:-yes}
vi_adjust_size=${vi_adjust_size:-yes}
vi_cloud=${vi_cloud:-0}
vi_pert_smth=${vi_pert_smth:-0}
vi_slp_adjust=${vi_slp_adjust:-0}
crfactor=${crfactor:-1.0}
pubbasin2=${pubbasin2:-AL}
vi_domain_size=${vi_domain_size:-45}
vi_multi_storm=${vi_multi_storm:-no}

FGAT_MODEL=${FGAT_MODEL:-gfs}
FGAT_HR=${FGAT_HR:-00}

if [ "${ENSDA}" = YES ]; then
  export nest_grids=${nest_grids_ens:-${nest_grids}}
  export RESTARTinp=${COMOLD}/${old_out_prefix}.RESTART_ens/mem${ENSID}
  export RESTARTmrg=${WORKhafs}/intercom/RESTART_analysis_merge_ens/mem${ENSID}
  export INTCOMinit=${WORKhafs}/intercom/atm_init_ens/mem${ENSID}
  export RESTARTinit=${WORKhafs}/intercom/RESTART_init_ens/mem${ENSID}
  export RESTARTout=${WORKhafs}/intercom/RESTART_vi_ens/mem${ENSID}
  export CDATE=${CDATE:-${YMDH}}
elif [ ${FGAT_MODEL} = gdas ]; then
  export RESTARTinp=${COMOLD}/${old_out_prefix}.RESTART
  export RESTARTmrg=${WORKhafs}/intercom/RESTART_merge_fgat${FGAT_HR}
  export INTCOMinit=${WORKhafs}/intercom/atm_init_fgat${FGAT_HR}
  export RESTARTinit=${WORKhafs}/intercom/RESTART_init_fgat${FGAT_HR}
  export RESTARTout=${WORKhafs}/intercom/RESTART_vi_fgat${FGAT_HR}
  export CDATE=$(${NDATE} $(awk "BEGIN {print ${FGAT_HR}-6}") ${YMDH})
else
  export RESTARTinp=${COMOLD}/${old_out_prefix}.RESTART
  export RESTARTmrg=${WORKhafs}/intercom/RESTART_merge
  export INTCOMinit=${WORKhafs}/intercom/atm_init
  export RESTARTinit=${WORKhafs}/intercom/RESTART_init
  export RESTARTout=${WORKhafs}/intercom/RESTART_vi
  export CDATE=${CDATE:-${YMDH}}
fi

CDATEprior=$(${NDATE} -6 $YMDH)
DATOOL=${DATOOL:-${EXEChafs}/hafs_tools_datool.x}
# Python hafs_datool package: hafsvi_preproc (Stage 0) and hafsvi_postproc (Stage 4)
DATOOL_PY_DIR=${DATOOL_PY_DIR:-${HOMEhafs}/sorc/hafs_tools.fd/sorc}
DATOOL_PY=${DATOOL_PY:-"python -u -m hafs_datool_py"}
export PYTHONPATH=${DATOOL_PY_DIR}${PYTHONPATH:+:${PYTHONPATH}}

DATA=${DATA:-${WORKhafs}/atm_vi}
mkdir -p ${DATA}

cd $DATA

# default nstorms=1 for single storm VI
vi_nstorms=1

if [ ${vi_multi_storm} = yes ]; then

if [ ${vi_storm_env} = pert ] && [ ! -s ${RESTARTinp}/${CDATE:0:8}.${CDATE:8:2}0000.fv_core.res.nc ]; then
  echo "INFO: vi_multi_storm = yes, vi_storm_env = pert, first guess from prior cycle for VI missing"
  echo "INFO: exiting after skipping vortex initialization ..."
  exit
fi
# Check, extract and sort active storms as desired
#${USHhafs}/tcutil_multistorm_sort.py -r -b -180,0,5,50 ${YMDH} L E C > allvit
${USHhafs}/tcutil_multistorm_sort.py -r ${YMDH} ${STORMID:2:1} > allvit
export err=$?; err_chk
all_nstorms=$(wc -l allvit | awk '{print $1}')
touch allvit.vi
tmp_storm_num=1
while [ ${tmp_storm_num} -le ${all_nstorms} ]; do
  head -n ${tmp_storm_num} allvit | tail -n 1 > ./tttvit
  tmp_message=$( head -n 1 ./tttvit )
  tmp_stormid=${tmp_message:5:3} # e.g., 13L
  # Check if storm is sufficiently inside the domain
  ${APRUNC} ${DATOOL} hafsvi_domaincheck --in_dir=${RESTARTinp} \
        --infile_date=${YMDH:0:8}.${YMDH:8:2}0000 \
        --tcvital=./tttvit
  export err=$?; err_chk
  mindis=$(<./distance)
  if [ "${vi_domain_size}" = "45auto" ] || [ "${vi_domain_size}" = "25auto" ]; then
    if [ "$(echo "$mindis < 10.0" | bc)" -eq 1 ]; then
      echo "INFO: too close to domain boundary, skip vortex initialization for storm $tmp_stormid."
    else
      cat ./tttvit >> allvit.vi
    fi
  elif [ "${vi_domain_size}" = "45" ]; then
    if [ "$(echo "$mindis < 25.0" | bc)" -eq 1 ]; then
      echo "INFO: too close to domain boundary, skip vortex initialization for storm $tmp_stormid."
    else
      cat ./tttvit >> allvit.vi
    fi
  elif [ "${vi_domain_size}" = "25" ]; then
    if [ "$(echo "$mindis < 15.0" | bc)" -eq 1 ]; then
      echo "INFO: too close to domain boundary, skip vortex initialization for storm $tmp_stormid."
    else
      cat ./tttvit >> allvit.vi
    fi
  elif [ "${vi_domain_size}" = "15" ]; then
    if [ "$(echo "$mindis < 10.0" | bc)" -eq 1 ]; then
      echo "INFO: too close to domain boundary, skip vortex initialization for storm $tmp_stormid."
    else
      cat ./tttvit >> allvit.vi
    fi
  else
    echo "FATAL ERROR: unsupported vi_domain_size: ${vi_domain_size}. Exiting ..."
    exit 1
  fi
  tmp_storm_num=$((tmp_storm_num + 1))
done
vi_nstorms=$(wc -l allvit.vi | awk '{print $1}')

# If no active storms, simply copy over the restart files
if [ ${vi_nstorms} -lt 1 ]; then

mkdir -p ${RESTARTout}
if [ -d ${RESTARTinit} ]; then
  RESTARTdst=${RESTARTinit}
elif [ -d ${RESTARTmrg} ]; then
  RESTARTdst=${RESTARTmrg}
else
  RESTARTdst=${RESTARTinp}
fi
# Currently, force RESTARTdst=${RESTARTinp} if vi_multi_storm=yes
if [ ${vi_multi_storm} = yes ]; then
  RESTARTdst=${RESTARTinp}
fi
rm -f cmdfile
for file in $(/bin/ls -1 ${RESTARTdst}/${CDATE:0:8}.${CDATE:8:2}0000* \
                         ${RESTARTdst}/atmos_static*.nc \
                         ${RESTARTdst}/grid_*spec*.nc \
                         ${RESTARTdst}/oro_data*.nc) ; do
  fname=$(basename ${file})
  echo ${NCP} -rp ${RESTARTdst}/${fname} ${RESTARTout}/${fname} >> cmdfile
done
chmod +x cmdfile
if [ $USE_CFP = "YES" ] ; then
  ncmd=$(cat ./cmdfile | wc -l)
  ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
  $APRUNCFP -n $ncmd_max cfp ./cmdfile
else
  ${APRUNC} ${MPISERIAL} -m cmdfile
fi
export err=$?; err_chk
# rm -f cmdfile
echo "INFO: no active storm, copy over the restart files from ${RESTARTdst} to ${RESTARTout} directly."
echo "INFO: exiting after skipping vortex initialization ..."
exit

fi #if [ ${vi_nstorms} -lt 1 ]; then

fi #if [ $vi_multi_storm = yes ]; then

#===============================================================================
# Storm setup, all storms first: tcvitals, VI domain size and VI box of each
# storm. The settings are saved in ${DATAstorm}/vi_setup.env for the storm loop,
# and the storms that go through VI are listed by VI box for Stage 0.
vi_storm_num=1
rm -f ${DATA}/prep_vit_*
while [ ${vi_storm_num} -le ${vi_nstorms} ]; do

DATAstorm=${DATA}/storm${vi_storm_num}
mkdir -p ${DATAstorm}
cd $DATAstorm

if [ ${vi_multi_storm} = yes ]; then
  vi_vit_dir="."
  head -n ${vi_storm_num} ${DATA}/allvit.vi | tail -n 1 > ./tm00vit
  vi_message=$( head -n 1 ./tm00vit )
  vi_stormid=${vi_message:5:3} # e.g., 13L
  # Generate tcvitals files: tmpvit, oldvit, tm03vit, tp03vit
  ${USHhafs}/tcutil_generate_vitals.py -c ${YMDH} -o ./ ${vi_stormid} ${YMDH:0:4}
  export err=$?; err_chk
  old_vi_message=$( head -n 1 ./oldvit )
  old_vi_stormid=${old_vi_message:5:3}
  # Calculate minimum distance from storm center to domain boundaries
  ${APRUNC} ${DATOOL} hafsvi_domaincheck --in_dir=${RESTARTinp} \
        --infile_date=${YMDH:0:8}.${YMDH:8:2}0000 \
        --tcvital=./tm00vit
  export err=$?; err_chk
  mindis=$(<./distance)
  # Configure vi_domain_size_storm accordingly
  if [ "${vi_domain_size}" = "45auto" ] || [ "${vi_domain_size}" = "25auto" ]; then
    vi_domain_size_storm=${vi_domain_size:0:2}
    if [ "${vi_domain_size_storm}" = "45" ] && [ "$(echo "$mindis < 25.0" | bc)" -eq 1 ]; then
      echo "WARNING: Reduce vi_domain_size_storm from 45 to 25 degrees."
      vi_domain_size_storm=25
    fi
    if [ "${vi_domain_size_storm}" = "25" ] && [ "$(echo "$mindis < 15.0" | bc)" -eq 1 ]; then
      echo "WARNING: Reduce vi_domain_size_storm from 25 to 15 degrees."
      vi_domain_size_storm=15
    fi
  elif [ "${vi_domain_size}" = "45" ] || [ "${vi_domain_size}" = "25" ] || [ "${vi_domain_size}" = 15 ]; then
    vi_domain_size_storm=${vi_domain_size}
  else
    echo "FATAL ERROR: unsupported vi_domain_size: ${vi_domain_size}. Exiting ..."
    exit 1
  fi
else
  vi_vit_dir=${WORKhafs}/intercom/launch
  ${NCP} ${vi_vit_dir}/tmpvit ./tm00vit
  vi_message=$( head -n 1 ./tm00vit )
  vi_stormid=${vi_message:5:3} # e.g., 13L
  ${NCP} ${vi_vit_dir}/oldvit ./oldvit
  old_vi_message=$( head -n 1 ./oldvit )
  #old_vi_stormid=${old_vi_message:5:3}
  old_vi_stormid=${old_out_prefix_nodate}
  vi_domain_size_storm=${vi_domain_size}
fi

if [[ ${vi_domain_size_storm} == 45 ]]; then
  deg_box1=30
  deg_box2=45
elif [[ ${vi_domain_size_storm} == 25 ]] || [[ ${vi_domain_size_storm} == 15 ]]; then
  deg_box1=${vi_domain_size_storm}
  deg_box2=${vi_domain_size_storm}
else
  echo "FATAL ERROR: unsupported vi_domain_size_storm: ${vi_domain_size_storm}. Exiting ..."
  exit 1
fi

# Prepare tcvitals file
if [ ${FGAT_HR} = 03 ]; then
  ${NCP} ${vi_vit_dir}/tm03vit tcvitals.vi
  gesfhr=3
elif [ ${FGAT_HR} = 06 ]; then
  ${NCP} ${vi_vit_dir}/tmpvit tcvitals.vi
  gesfhr=6
elif [ ${FGAT_HR} = 09 ]; then
  ${NCP} ${vi_vit_dir}/tp03vit tcvitals.vi
  gesfhr=9
else
  ${NCP} ${vi_vit_dir}/tmpvit tcvitals.vi
  gesfhr=6
fi

# Convert 1800W to 1800E for date line TCs
sed -i 's/1800W/1800E/g' ./tcvitals.vi
# Convert 0000W to 0000E for Greenwich line TCs
sed -i 's/0000W/0000E/g' ./tcvitals.vi
tcvital=${DATAstorm}/tcvitals.vi
# Extract vmax from tcvitals (m/s)
vmax_vit=$(cat ${tcvital} | cut -c68-69 | bc -l)

int_mode=0
old_tcvital=${DATAstorm}/oldvit
if [ -e ${old_tcvital} ]; then
  old_vmax_vit=$(cat ${old_tcvital} | cut -c68-69 | bc -l)
  vdif_tcvital=$(( ${vmax_vit}-${old_vmax_vit} ))
  if [[ ${vdif_tcvital} -ge 5 ]]; then
    int_mode=1
  fi
fi


cat > ${DATAstorm}/vi_setup.env <<EOF
vi_stormid=${vi_stormid}
old_vi_stormid=${old_vi_stormid}
vi_domain_size_storm=${vi_domain_size_storm}
deg_box1=${deg_box1}
deg_box2=${deg_box2}
gesfhr=${gesfhr}
tcvital=${tcvital}
vmax_vit=${vmax_vit}
int_mode=${int_mode}
EOF
if [[ ${vmax_vit} -gt ${vi_min_wind_for_init} ]]; then
  cat ${tcvital} >> ${DATA}/prep_vit_${deg_box1}_${deg_box2}
fi

vi_storm_num=$((vi_storm_num + 1))
done

#===============================================================================
# Stage 0: Run hafsvi_preproc once for all storms (per VI box, restart and
# resolution) to prepare the VI input on rotated lat-lon grids. The parts of a
# VI domain outside the restart domain are filled from a GFS/GDAS netCDF
# background valid at CDATE (read only when needed).

VI_PREPROC_APRUN=${VI_PREPROC_APRUN:-${APRUNO}}
VI_PREPROC_NPROC=${VI_PREPROC_NPROC:-${PURE_OMP_THREADS:-${OMP_NUM_THREADS:-1}}}
# Restarts to prepare from: prior cycle (guess) and/or current cycle (init)
vi_prep_sources=""
if [[ ${vi_force_cold_start,,} != "yes" ]] && [ -d ${RESTARTinp} ]; then
  vi_prep_sources="guess:${RESTARTinp}"
fi
if [ $vi_storm_env = init ]; then
  vi_prep_sources="${vi_prep_sources} init:${RESTARTinit}"
fi
# Background files (atm and sfc), chosen as in exhafs_atm_ic.sh
if [ -z "${vi_preproc_background:-}" ]; then
  PDYp=${CDATEprior:0:8}
  cycp=${CDATEprior:8:2}
  if [ "${GFSVER}" = "PROD2021" ]; then
    if [ "${ENSDA}" = YES ]; then
      bgdir=${COMINgdas}/enkfgdas.${PDYp}/${cycp}/atmos/mem${ENSID}
      vi_preproc_background="${bgdir}/gdas.t${cycp}z.atmf006.nc ${bgdir}/gdas.t${cycp}z.sfcf006.nc"
    elif [ ${FGAT_MODEL} = gdas ]; then
      bgdir=${COMINgdas}/gdas.${PDYp}/${cycp}/atmos
      vi_preproc_background="${bgdir}/gdas.t${cycp}z.atmf0${FGAT_HR}.nc ${bgdir}/gdas.t${cycp}z.sfcf0${FGAT_HR}.nc"
    else
      bgdir=${COMINgfs}/gfs.${CDATE:0:8}/${CDATE:8:2}/atmos
      vi_preproc_background="${bgdir}/gfs.t${CDATE:8:2}z.atmanl.nc ${bgdir}/gfs.t${CDATE:8:2}z.sfcanl.nc"
    fi
  else
    if [ "${ENSDA}" = YES ]; then
      bgdir=${COMINgdas}/enkfgdas.${PDYp}/${cycp}/mem${ENSID}/model/atmos/history
      vi_preproc_background="${bgdir}/enkfgdas.t${cycp}z.atm.f006.nc ${bgdir}/enkfgdas.t${cycp}z.sfc.f006.nc"
    elif [ ${FGAT_MODEL} = gdas ]; then
      bgdir=${COMINgdas}/gdas.${PDYp}/${cycp}/model/atmos/history
      vi_preproc_background="${bgdir}/gdas.t${cycp}z.atm.f0${FGAT_HR}.nc ${bgdir}/gdas.t${cycp}z.sfc.f0${FGAT_HR}.nc"
    else
      bgdir=${COMINgfs}/gfs.${CDATE:0:8}/${CDATE:8:2}/analysis/atmos
      vi_preproc_background="${bgdir}/gfs.t${CDATE:8:2}z.analysis.atm.a006.nc ${bgdir}/gfs.t${CDATE:8:2}z.analysis.sfc.a006.nc"
    fi
  fi
fi
vi_bg_args="--bg_files ${vi_preproc_background}"
for file in ${vi_preproc_background}; do
  if [ ! -s ${file} ]; then
    echo "WARNING: VI background ${file} not found: VI domain parts outside the restart get missing values."
    vi_bg_args=""
  fi
done

for vit in $(/bin/ls ${DATA}/prep_vit_* 2>/dev/null); do
  box=${vit##*/prep_vit_}   # deg_box1_deg_box2
  for src in ${vi_prep_sources}; do
    work_dir=${DATA}/prep_${src%%:*}_box${box}
    mkdir -p ${work_dir}
    cd ${work_dir}
    for res in 0.02 0.20; do
      if [[ ${res} == 0.02 ]]; then
        vortexradius=${box%_*}
      else
        vortexradius=${box#*_}
      fi
      ${VI_PREPROC_APRUN} ${DATOOL_PY} hafsvi_preproc \
          --in_dir ${src#*:} \
          --in_date ${CDATE} \
          --tc_vitals ${vit} --vi_threshold ${vi_min_wind_for_init} \
          --vortexradius ${vortexradius} --res ${res} \
          --nestdoms $((${nest_grids:-1}-1)) --vi_cloud ${vi_cloud} \
          --interpolation_points 5 ${vi_bg_args} \
          --nproc ${VI_PREPROC_NPROC} --out_dir ${work_dir} 2>&1 | tee ./vi_inp_${res/\./p}.log
      export err=$?; err_chk
    done
  done
done

vi_storm_num=1
# VI analyses (storm_anl) of all storms, merged into the restart files in this
# order by one hafsvi_postproc call after the storm loop (Stage 4)
vi_anl_files=""
vi_inp_files=""   # the VI input of each storm: Stage 4 adds storm_anl - vi_inp
vi_anl_txt=""
RESTARTdst_vi=""
#===============================================================================
# Loop over VI storms
while [ ${vi_storm_num} -le ${vi_nstorms} ]; do

DATAstorm=${DATA}/storm${vi_storm_num}
cd $DATAstorm
. ${DATAstorm}/vi_setup.env

cd $DATAstorm

#===============================================================================
# Skip atm_vi if vmax is weaker than vi_min_wind_for_init (e.g., 9 m/s)
if [[ ${vmax_vit} -le ${vi_min_wind_for_init} ]]; then

echo "INFO: vmax_vit of ${vmax_vit} <= ${vi_min_wind_for_init} m/s"
echo "INFO: skip vortex initialization for weak storm ${vi_stormid}."
# The restart files are copied to ${RESTARTout} once, after the storm loop (Stage 4).

else

#===============================================================================
# Stage 0: Link this storm's VI input, made for all storms before the loop

for src in ${vi_prep_sources}; do
  work_dir=${DATAstorm}/prep_${src%%:*}
  mkdir -p ${work_dir}
  for res in 0.02 0.20; do
    prep_file=${DATA}/prep_${src%%:*}_box${deg_box1}_${deg_box2}/vi_inp_${res/\./p}_${vi_stormid}.bin
    if [[ ${nest_grids:-1} -gt 1 ]]; then
      # parent-only VI input, for diagnostics (Stage 4 uses the merged file)
      ${NLN} ${prep_file} ${work_dir}/vi_inp_${res/\./p}.bin_grid01
      prep_file=${prep_file}_nest$(printf "%02d" ${nest_grids})
    fi
    if [ ! -s ${prep_file} ]; then
      echo "FATAL ERROR: hafsvi_preproc did not produce ${prep_file}"
      exit 1
    fi
    ${NLN} ${prep_file} ${work_dir}/vi_inp_${res/\./p}.bin
  done
done

#===============================================================================
# Stage 1: Process prior cycle's vortex if exists and if storm intensity is
# stronger than vi_warm_start_vmax_threshold (e.g., 20 m/s)

# Force to cold start storm vortex if desired
if [[ ${vi_force_cold_start,,} != "yes" ]]; then

if [[ ${vmax_vit} -ge ${vi_warm_start_vmax_threshold} ]] && [ -d ${RESTARTinp} ]; then

  # create_trak and split
  work_dir=${DATAstorm}/split_guess
  mkdir -p ${work_dir}
  cd ${work_dir}
  # input
  ${NLN} ${tcvital} fort.11
  if [ -e ${COMOLD}/${old_out_prefix}.${RUN}.trak.atcfunix.all ]; then
    ${NCP} ${COMOLD}/${old_out_prefix}.${RUN}.trak.atcfunix.all ./trak.atcfunix.all
    # rename basin id for Southern Hemisphere or Northern Indian Ocean storms
	sed -i -e 's/^AA/IO/g' -e 's/^BB/IO/g' -e 's/^SP/SH/g' -e 's/^SI/SH/g' -e 's/^SQ/SL/g' ./trak.atcfunix.all
    # Convert 1800W to 1800E for date line TCs
	sed -i 's/1800W/1800E/g' ./trak.atcfunix.all
    # Convert 0000W to 0000E for Greenwich line TCs
    sed -i 's/0000W/0000E/g' ./trak.atcfunix.all
    if grep "^${pubbasin2^^}, ${old_vi_stormid:0:2}," trak.atcfunix.all > trak.atcfunix.tmp ; then
      echo "trak.atcfunix.tmp generated."
    else
      touch trak.atcfunix.tmp
    fi
  else
    touch trak.atcfunix.tmp
  fi
  # get vmax in kt then convert into m/s
  vmax_guess=$(grep "^${pubbasin2^^}, ${vi_stormid:0:2}, ${CDATEprior}, .., ...., 00${gesfhr}," trak.atcfunix.tmp | \
               grep "34, NEQ," | cut -c48-51 | bc -l)
  vmax_guess=${vmax_guess:-0}
  vmax_guess=$(printf "%.0f" $(bc <<< "scale=6; ${vmax_guess}*0.514444"))
  # calculate the abs difference
  vdif_guess=$(( ${vmax_guess}-${vmax_vit} ))
  vdif_guess="${vdif_guess#-}"

  ${RLN} trak.atcfunix.tmp fort.12
  # output
  ${RLN} ./trak.fnl.all fort.30

  ${NCP} -p ${EXEChafs}/hafs_tools_vi_create_trak_guess.x ./
  ${SOURCE_PREP_STEP}
  ${APRUNS} ./hafs_tools_vi_create_trak_guess.x ${vi_stormid}
  export err=$?; err_chk

  # split
  # input
  ${NLN} ${tcvital} fort.11
  ${NLN} ./trak.fnl.all fort.30
  ${NLN} ../prep_guess/vi_inp_0p02.bin fort.26
  ${NLN} ../prep_guess/vi_inp_0p20.bin fort.46
  # output
  ${RLN} storm_env fort.56
  ${RLN} rel_inform fort.52
  ${RLN} vital_syn fort.55
  ${RLN} storm_pert fort.71
  ${RLN} storm_radius fort.85

  ${NCP} -p ${EXEChafs}/hafs_tools_vi_split.x ./
  gesfhr=${gesfhr:-6}
  ibgs=0
  iflag_cold=0
  crfactor=${crfactor:-1.0}
  ${SOURCE_PREP_STEP}
  echo ${gesfhr} $ibgs $vmax_vit $iflag_cold $crfactor ${vi_cloud} ${vi_domain_size_storm} | ${APRUNO} ./hafs_tools_vi_split.x 2>&1 | tee ./vi_split.log
  export err=$?; err_chk

  # anl_pert
  work_dir=${DATAstorm}/anl_pert_guess
  mkdir -p ${work_dir}
  cd ${work_dir}
  # input
  ${NLN} ${tcvital} fort.11
  ${NLN} ../split_guess/storm_env fort.26
  ${NLN} ../prep_guess/vi_inp_0p02.bin fort.46
  ${NLN} ../split_guess/storm_pert fort.71
  ${NLN} ../split_guess/storm_radius fort.65
  # output
  ${RLN} storm_pert_new fort.58
  ${RLN} storm_size_p fort.14
  ${RLN} storm_sym fort.23

  ${NCP} -p ${EXEChafs}/hafs_tools_vi_anl_pert.x ./
  if [ ${vi_storm_modification} = auto ]; then
    # Conduct storm modification only if vdif >= 5 m/s or >= 15% of vmax_vit
    if [[ ${vdif_guess} -ge 5 ]] || [[ ${vdif_guess} -ge $(printf "%.0f" $(bc <<< "scale=6; ${vmax_vit}*0.15")) ]]; then
      initopt=0
    else
      initopt=1
    fi
  elif [ ${vi_storm_modification} = vmax_threshold ]; then
    # Conduct storm modification only if vmax >= 30 m/s or vmax_vit > 30 m/s
    if [[ ${vmax_guess} -ge 30 ]] || [[ ${vmax_vit} -ge 30 ]]; then
      initopt=0
    else
      initopt=1
    fi
  elif [ ${vi_storm_modification} = no ]; then
    initopt=1
  elif [ ${vi_storm_modification} = yes ]; then
    initopt=0
  else
    initopt=0
  fi
  initopt_guess=${initopt}
  ${SOURCE_PREP_STEP}
  echo 6 ${pubbasin2} ${initopt} ${vi_pert_smth} ${int_mode} | ${APRUNO} ./hafs_tools_vi_anl_pert.x 2>&1 | tee ./vi_anl_pert.log
  export err=$?; err_chk
fi

fi # end if [[ ${vi_force_cold_start,,} != "yes" ]]; then

#===============================================================================
# Stage 2: Process current cycle's vortex from the global/parent model

cd $DATAstorm
if [ $vi_storm_env = init ]; then

  # create_trak and split
  work_dir=${DATAstorm}/split_init
  mkdir -p ${work_dir}
  cd ${work_dir}
  # input
  ${NLN} ${tcvital} fort.11
  if [ -e ${INTCOMinit}/${STORMID,,}.${CDATE}.${RUN}.trak.atcfunix.all ]; then
    ${NCP} ${INTCOMinit}/${STORMID,,}.${CDATE}.${RUN}.trak.atcfunix.all ./trak.atcfunix.all
    # rename basin id for Southern Hemisphere or Northern Indian Ocean storms
	sed -i -e 's/^AA/IO/g' -e 's/^BB/IO/g' -e 's/^SP/SH/g' -e 's/^SI/SH/g' -e 's/^SQ/SL/g' ./trak.atcfunix.all
    # Convert 1800W to 1800E for date line TCs
	sed -i 's/1800W/1800E/g' ./trak.atcfunix.all
    # Convert 0000W to 0000E for Greenwich line TCs
    sed -i 's/0000W/0000E/g' ./trak.atcfunix.all
    grep "^${pubbasin2^^}, ${vi_stormid:0:2}," trak.atcfunix.all \
      > trak.atcfunix.tmp
  else
    touch trak.atcfunix.tmp
  fi
  # get vmax in kt then convert into m/s
  vmax_init=$(grep "^${pubbasin2^^}, ${vi_stormid:0:2}, ${CDATE}, .., ...., 000," trak.atcfunix.tmp | \
              grep "34, NEQ," | cut -c48-51 | bc -l)
  vmax_init=${vmax_init:-0}
  vmax_init=$(printf "%.0f" $(bc <<< "scale=6; ${vmax_init}*0.514444"))
  # calculate the abs difference
  vdif_init=$(( ${vmax_init}-${vmax_vit} ))
  vdif_init="${vdif_init#-}"

  ${RLN} trak.atcfunix.tmp fort.12
  # output
  ${RLN} ./trak.fnl.all fort.30

  ${NCP} -p ${EXEChafs}/hafs_tools_vi_create_trak_init.x ./
  ${SOURCE_PREP_STEP}
  ${APRUNS} ./hafs_tools_vi_create_trak_init.x ${vi_stormid}
  export err=$?; err_chk

  # split
  # input
  ${NLN} ${tcvital} fort.11
  ${NLN} ./trak.fnl.all fort.30
  ${NLN} ../prep_init/vi_inp_0p02.bin fort.26
  ${NLN} ../prep_init/vi_inp_0p20.bin fort.46
  if [ -s ../split_guess/storm_radius ]; then
    ${NLN} ../split_guess/storm_radius fort.65
  fi
  # output
  ${RLN} storm_env fort.56
  ${RLN} rel_inform fort.52
  ${RLN} vital_syn fort.55
  ${RLN} storm_pert fort.71
  ${RLN} storm_radius fort.85

  ${NCP} -p ${EXEChafs}/hafs_tools_vi_split.x ./
  gesfhr=${gesfhr:-6}
  # Warm start or cold start
  if [ -s fort.65 ]; then
    ibgs=1
    iflag_cold=0
  else
    ibgs=2
    iflag_cold=1
  fi
  ${SOURCE_PREP_STEP}
  echo ${gesfhr} $ibgs $vmax_vit $iflag_cold 1.0 ${vi_cloud} ${vi_domain_size_storm} | ${APRUNO} ./hafs_tools_vi_split.x 2>&1 | tee ./vi_split.log
  export err=$?; err_chk

  # anl_pert
  work_dir=${DATAstorm}/anl_pert_init
  mkdir -p ${work_dir}
  cd ${work_dir}
  # input
  ${NLN} ${tcvital} fort.11
  ${NLN} ../split_init/storm_env fort.26
  ${NLN} ../prep_init/vi_inp_0p02.bin fort.46
  ${NLN} ../split_init/storm_pert fort.71
  ${NLN} ../split_init/storm_radius fort.65
  # output
  ${RLN} storm_pert_new fort.58
  ${RLN} storm_size_p fort.14
  ${RLN} storm_sym fort.23

  ${NCP} -p ${EXEChafs}/hafs_tools_vi_anl_pert.x ./
  if [ ${vi_storm_modification} = auto ]; then
    # Conduct storm modification only if vdif >= 5 m/s or >= 15% of vmax_vit
    if [[ ${vdif_init} -ge 5 ]] || [[ ${vdif_init} -ge $(printf "%.0f" $(bc <<< "scale=6; ${vmax_vit}*0.15")) ]]; then
      initopt=0
    else
      initopt=1
    fi
  elif [ ${vi_storm_modification} = vmax_threshold ]; then
    # Conduct storm modification only if vmax >= 30 m/s or vmax_vit > 30 m/s
    if [[ ${vmax_init} -ge 30 ]] || [[ ${vmax_vit} -ge 30 ]]; then
      initopt=0
    else
      initopt=1
    fi
  elif [ ${vi_storm_modification} = no ]; then
    initopt=1
  elif [ ${vi_storm_modification} = yes ]; then
    initopt=0
  else
    initopt=0
  fi
  initopt_init=${initopt}
  ${SOURCE_PREP_STEP}
  echo 6 ${pubbasin2} ${initopt} ${vi_pert_smth} ${int_mode} | ${APRUNO} ./hafs_tools_vi_anl_pert.x 2>&1 | tee ./vi_anl_pert.log
  export err=$?; err_chk

fi
#===============================================================================
# Stage 3:

# anl_storm
work_dir=${DATAstorm}/anl_storm
mkdir -p ${work_dir}
cd ${work_dir}

# Bogus a storm if prior cycle does not exist and tcvital intensity >= vi_bogus_vmax_threshold (e.g., 33 m/s)
if [[ ${vmax_vit} -ge ${vi_bogus_vmax_threshold} ]] && [ ! -s ../anl_pert_guess/storm_pert_new ]; then

  echo "WARNING: Bogus storm vortex since prior cycle does not exist and vmax_vit of ${vmax_vit} >= ${vi_bogus_vmax_threshold}"

  pert=init
  senv=$pert
  # anl_bogus
  # input
  ${NLN} ${tcvital} fort.11
  ${NLN} ../split_${senv}/storm_env fort.26
  ${NLN} ../prep_${pert}/vi_inp_0p02.bin fort.36
  ${NLN} ../prep_${pert}/vi_inp_0p02.bin fort.46 #roughness
  ${NLN} ../split_${pert}/storm_pert fort.61
  ${NLN} ../split_${pert}/storm_radius fort.85

  ${NLN} ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.71
  ${NLN} ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.72
  ${NLN} ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.73
  ${NLN} ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.74
  if [[ $pubbasin2 = AL ]] || [[ $pubbasin2 = EP ]] || [[ $pubbasin2 = CP ]]; then
    ${NLN} ${FIXhafs}/fix_vi/hafs_storm_deep  fort.75
  else
    ${NLN} ${FIXhafs}/fix_vi/hafs_storm_deep_jtwc fort.75
  fi
  ${NLN} ${FIXhafs}/fix_vi/hafs_storm_shallow fort.76
  ${NLN} ${FIXhafs}/fix_vi/hafs_storm_shallow fort.77
  ${NLN} ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.78

  # output
  ${RLN} storm_anl_bogus                       fort.56

  ${NCP} -p ${EXEChafs}/hafs_tools_vi_anl_bogus.x ./
  ${SOURCE_PREP_STEP}
  echo 6 ${pubbasin2} ${vi_cloud} | ${APRUNO} ./hafs_tools_vi_anl_bogus.x 2>&1 | tee ./vi_anl_bogus.log
  export err=$?; err_chk
  ${NCP} -p storm_anl_bogus storm_anl

else # warm-start from prior cycle or cold start from global/parent model

  # anl_combine
  if [[ ${vmax_vit} -ge ${vi_warm_start_vmax_threshold} ]] && [ -s ../anl_pert_guess/storm_pert_new ]; then
    pert=guess
    initopt=${initopt_guess}
  else
    pert=init
    initopt=${initopt_init}
  fi
  if [ $vi_storm_env = init ]; then
    senv=init
  else
    senv=$pert
  fi
  if [ $pert = init ]; then
    gfs_flag=0
  else
    gfs_flag=6
  fi
  if [ $ENSDA = YES ]; then
    gfs_flag=1
  fi

  rm -f flag_file
  # input
  ${NLN} ${tcvital} fort.11
  ${RLN} ../split_${pert}/trak.atcfunix.tmp fort.12
  ${NLN} ../split_${pert}/trak.fnl.all fort.30
  ${NLN} ../anl_pert_${pert}/storm_size_p fort.14
  ${NLN} ../anl_pert_${pert}/storm_sym fort.23
  ${NLN} ../anl_pert_${pert}/storm_pert_new fort.71
  ${NLN} ../split_${senv}/storm_env fort.26
  ${NLN} ../prep_${pert}/vi_inp_0p02.bin fort.46 #roughness
  ${NLN} ../anl_pert_${pert}/iteration ./

  # output
  ${RLN} storm_env_new fort.36
  ${RLN} storm_anl_combine fort.56
  ${RLN} storm_txt_combine fort.91

  gesfhr=${gesfhr:-6}
  gfs_flag=${gfs_flag:-6}

  ${NCP} -p ${EXEChafs}/hafs_tools_vi_anl_combine.x ./
  ${SOURCE_PREP_STEP}
  echo ${gesfhr} ${pubbasin2} ${gfs_flag} ${initopt} ${vi_cloud} ${vi_domain_size_storm} | ${APRUNO} ./hafs_tools_vi_anl_combine.x 2>&1 | tee ./vi_anl_combine.log
  export err=$?; err_chk
  if [ -s storm_anl_combine ]; then
    ${NCP} -p storm_anl_combine storm_anl
    ${NCP} -p storm_txt_combine storm_txt
  fi

  # If the combined storm is weaker than the tcvital intensity, add a small
  # fraction of the composite storm to enhance the storm intensity so that it
  # matches the tcvital intensity
  if [ -s flag_file ] && [ -s storm_env_new ]; then
    # anl_enhance
    # input
   #${NLN} ${tcvital} fort.11
   #${NLN} ./flag_file flag_file
    ${NLN} ../anl_pert_${pert}/storm_sym fort.23
    ${NLN} storm_env_new fort.26
    ${NLN} ../prep_${pert}/vi_inp_0p02.bin fort.46 #roughness
    ${NLN} ../split_${pert}/storm_radius fort.85

    ${NLN} ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.71
    ${NLN} ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.72
    ${NLN} ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.73
    ${NLN} ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.74
    if [[ $pubbasin2 = AL ]] || [[ $pubbasin2 = EP ]] || [[ $pubbasin2 = CP ]]; then
      ${NLN} ${FIXhafs}/fix_vi/hafs_storm_deep  fort.75
    else
      ${NLN} ${FIXhafs}/fix_vi/hafs_storm_deep_jtwc fort.75
    fi
    ${NLN} ${FIXhafs}/fix_vi/hafs_storm_shallow fort.76
    ${NLN} ${FIXhafs}/fix_vi/hafs_storm_shallow fort.77
    ${NLN} ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.78

    # output
    ${RLN} storm_anl_enhance                     fort.56
    ${RLN} storm_txt_enhance                     fort.92

    iflag_cold=${iflag_cold:-0}
    ${NCP} -p ${EXEChafs}/hafs_tools_vi_anl_enhance.x ./
    ${SOURCE_PREP_STEP}
    echo 6 ${pubbasin2} ${iflag_cold} ${vi_cloud} ${vi_slp_adjust} | ${APRUNO} ./hafs_tools_vi_anl_enhance.x 2>&1 | tee ./vi_anl_enhance.log
    export err=$?; err_chk
    ${NCP} -p storm_anl_enhance storm_anl
    ${NCP} -p storm_txt_enhance storm_txt
  fi

fi

if [ ! -s storm_anl ]; then
  echo "FATAL ERROR: Stage3 did not produce storm_anl"
  exit 1
fi

#===============================================================================
# Record this storm's VI analysis and the restart files it is merged into. All
# storms are merged with one hafsvi_postproc call after the loop (Stage 4).
# Every storm's VI starts from the same restart, so Stage 4 uses the hybrid
# merge of hafsvi_postproc: the VI analysis in each storm's core, the VI
# increment (storm_anl minus the vi_inp file its environment came from)
# outside. Where VI domains overlap, the storms' changes add up instead of a
# later storm's analysis restoring an earlier storm's old vortex.

if [ $senv = init ]; then
  RESTARTdst_storm=${RESTARTinit}
elif [ -d ${RESTARTmrg} ]; then
  RESTARTdst_storm=${RESTARTmrg}
else
  RESTARTdst_storm=${RESTARTinp}
fi
if [ -n "${RESTARTdst_vi}" ] && [ "${RESTARTdst_storm}" != "${RESTARTdst_vi}" ]; then
  echo "WARNING: storm ${vi_stormid} merges into ${RESTARTdst_storm}, an earlier storm into ${RESTARTdst_vi}."
  echo "WARNING: using ${RESTARTdst_storm} (last VI storm) as the base restart for all storms."
fi
RESTARTdst_vi=${RESTARTdst_storm}
vi_anl_files="${vi_anl_files} ${DATAstorm}/anl_storm/storm_anl"
vi_inp_files="${vi_inp_files} ${DATAstorm}/prep_${senv}/vi_inp_0p02.bin"
if [ -s ${DATAstorm}/anl_storm/storm_txt ]; then
  vi_anl_txt=${DATAstorm}/anl_storm/storm_txt
fi

fi #if [[ ${vmax_vit} -le ${vi_min_wind_for_init} ]]; then

vi_storm_num=$((vi_storm_num + 1))

done

#===============================================================================
# Stage 4: Copy the restart files to RESTARTout and merge all storms' VI
# analyses into them with one call of the Python/MPI hafsvi_postproc

cd $DATA
mkdir -p ${RESTARTout}
if [ -n "${vi_anl_files}" ]; then
  RESTARTdst=${RESTARTdst_vi}
else
  # No storm went through VI (all too weak): copy the restart files over
  if [ -d ${RESTARTinit} ]; then
    RESTARTdst=${RESTARTinit}
  elif [ -d ${RESTARTmrg} ]; then
    RESTARTdst=${RESTARTmrg}
  else
    RESTARTdst=${RESTARTinp}
  fi
  # Currently, force RESTARTdst=${RESTARTinp} if vi_multi_storm=yes
  if [ ${vi_multi_storm} = yes ]; then
    RESTARTdst=${RESTARTinp}
  fi
fi
rm -f cmdfile
for file in $(/bin/ls -1 ${RESTARTdst}/${CDATE:0:8}.${CDATE:8:2}0000* \
                         ${RESTARTdst}/atmos_static*.nc \
                         ${RESTARTdst}/grid_*spec*.nc \
                         ${RESTARTdst}/oro_data*.nc) ; do
  fname=$(basename ${file})
  echo ${NCP} -rp ${RESTARTdst}/${fname} ${RESTARTout}/${fname} >> cmdfile
done
chmod +x cmdfile
if [ $USE_CFP = "YES" ] ; then
  ncmd=$(cat ./cmdfile | wc -l)
  ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
  $APRUNCFP -n $ncmd_max cfp ./cmdfile
else
  ${APRUNC} ${MPISERIAL} -m cmdfile
fi
export err=$?; err_chk
# Flush the copy before the in-place update: unflushed copy data made the
# update's netCDF writes about 10x slower in testing
sync
# rm -f cmdfile

if [ -z "${vi_anl_files}" ]; then
  echo "INFO: no storm went through vortex initialization."
  echo "INFO: copied the restart files from ${RESTARTdst} to ${RESTARTout} directly."
else
  if [ -s "${vi_anl_txt}" ]; then
    ${NCP} -rp ${vi_anl_txt} ${RESTARTout}/
  fi
  echo "INFO: merging VI analyses into ${RESTARTout}:${vi_anl_files}"
  # VI input of every domain: the file the VI started from (with nests, the
  # merge of all domains). The VI increment is then the VI's own change in
  # each domain, and the parent keeps its terrain outside the storm cores; the
  # parent-only file (_grid01) would add the nest - parent difference there.
  vi_inp_args="--vi_inp ${vi_inp_files}"
  for nd in $(seq 1 ${nest_grids}); do
    ${APRUNC} ${DATOOL_PY} hafsvi_postproc \
        --in_file ${vi_anl_files} ${vi_inp_args} \
        --debug_level 1 --interpolation_points 5 \
        --relaxzone 30 \
        --in_date ${CDATE} \
        --nestdoms $((${nd}-1)) \
        --vi_cloud ${vi_cloud} \
        --out_dir ${RESTARTout} 2>&1 | tee ./vi_postproc_grid${nd}.log
    export err=$?; err_chk
  done
fi

#===============================================================================

date
