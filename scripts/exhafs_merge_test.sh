#!/bin/sh
################################################################################
# Script Name: exhafs_merge.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs hafs_datool to merge atmospheric restart files. It
#   supports the merge_type of analysis or init with the merge_method of
#   domainmerge or vortexreplace.
# History:
#   01/12/2022: Enable cycling the storm region only or cycling the whole domain
#               from the prior HAFS forecast cycle
#   02/26/2022: Enable handling the regional moving nesting configuration
#   03/09/2023: Improvements for HAFSv1 operational implementation
#   04/20/2024: Improve error handling and stdout/stderr redirection for HAFSv2
#   07/12/2024: Add 3DIAU related capabilities for HAFS regional configuration
#   12/16/2024: Add Fourier wave-number filtering capabilities for DA increments
#   06/30/2024: Merge with HFSM (Multistorm) workflow changes
# Condition codes:
#   == 0 : success
#   != 0 : fatal error encounted
################################################################################
set -x -o pipefail

FGAT_MODEL=${FGAT_MODEL:-gfs}
FGAT_HR=${FGAT_HR:-00}

MPISERIAL=${MPISERIAL:-${EXEChafs}/hafs_tools_mpiserial.x}
DATOOL=${DATOOL:-${EXEChafs}/hafs_tools_datool.x}
SENDCOM=${SENDCOM:-YES}

if [ "${RUN_ATM_VI}" == NO ] && [ "${RUN_GSI}" == NO ]; then
    USE_EXTERNAL_VORTEX=${USE_EXTERNAL_VORTEX:-YES}
else
    USE_EXTERNAL_VORTEX=${USE_EXTERNAL_VORTEX:-NO}
fi

# Merge analysis or init
if [ ${MERGE_TYPE} = analysis ]; then

merge_method=${analysis_merge_method:-vortexreplace}
# Deterministic or ensemble
if [ "${ENSDA}" = YES ]; then
  export nest_grids=${nest_grids_ens:-${nest_grids}}
  if [ -d ${WORKhafs}/intercom/RESTART_analysis_ens/mem${ENSID} ]; then
    RESTARTsrc=${WORKhafs}/intercom/RESTART_analysis_ens/mem${ENSID}
  elif [ -d ${WORKhafs}/intercom/RESTART_vi_ens/mem${ENSID} ]; then
    RESTARTsrc=${WORKhafs}/intercom/RESTART_vi_ens/mem${ENSID}
  else
    echo "FATAL ERROR: RESTARTsrc does not exist"
    exit 1
  fi
  RESTARTdst=${WORKhafs}/intercom/RESTART_init_ens/mem${ENSID}
  RESTARTmrg=${WORKhafs}/intercom/RESTART_analysis_merge_ens/mem${ENSID}
  RESTARTcom=${COMhafs}/${out_prefix}.RESTART_analysis_merge_ens/mem${ENSID}
else
  if [ -e ${WORKhafs}/intercom/RESTART_analysis ]; then
    RESTARTsrc=${WORKhafs}/intercom/RESTART_analysis
  elif [ -e ${WORKhafs}/intercom/RESTART_vi ]; then
    RESTARTsrc=${WORKhafs}/intercom/RESTART_vi
  elif [ -e ${WORKhafs}/intercom/RESTART_init ]; then
    RESTARTsrc=${WORKhafs}/intercom/RESTART_init
  else
    echo "FATAL ERROR: RESTARTsrc does not exist"
    exit 1
  fi
  RESTARTdst=${WORKhafs}/intercom/RESTART_init
  RESTARTmrg=${WORKhafs}/intercom/RESTART_analysis_merge
  RESTARTcom=${COMhafs}/${out_prefix}.RESTART_analysis_merge
fi

elif [ ${MERGE_TYPE} = init ]; then

merge_method=${atm_merge_method:-vortexreplace}
if [ ${FGAT_MODEL} = gdas ]; then
  RESTARTsrc=${COMOLD}/${old_out_prefix}.RESTART
  RESTARTdst=${WORKhafs}/intercom/RESTART_atm_merge_init_fgat${FGAT_HR}
  RESTARTmrg=${WORKhafs}/intercom/RESTART_merge_fgat${FGAT_HR}
  #CDATE=$(${NDATE} $(awk "BEGIN {print ${FGAT_HR}-6}") $CDATE)
  CDATE=$(${NDATE} $(awk "BEGIN {print ${FGAT_HR}-6}") ${CDATE:-${YMDH}})
else
  RESTARTsrc=${COMOLD}/${old_out_prefix}.RESTART
  RESTARTdst=${WORKhafs}/intercom/RESTART_atm_merge_init
  RESTARTmrg=${WORKhafs}/intercom/RESTART_merge
fi

else

  echo "FATAL ERROR: unsupported MERGE_TYPE: ${MERGE_TYPE}"
  exit 1

fi # if [ ${MERGE_TYPE} = analysis ]; then

CDATE=${CDATE:-$YMDH}
ymd=$(echo $CDATE | cut -c1-8)
yr=$(echo $CDATE | cut -c1-4)
mn=$(echo $CDATE | cut -c5-6)
dy=$(echo $CDATE | cut -c7-8)
hh=$(echo $CDATE | cut -c9-10)

if [ ${RUN_MULTISTORM} == "YES" ]; then
    multistorm_sids=`echo ${multistorm_sids} | tr ',' ' '`
fi

DATA=${DATA:-${WORKhafs}/merge}

cd ${DATA}

if [ -d ${RESTARTmrg} ]; then
  rm -r ${RESTARTmrg}
fi
mkdir -p ${RESTARTmrg}
${NCP} -rp ${RESTARTdst}/* ${RESTARTmrg}/

if [ -d ${RESTARTsrc} ] || [ -L ${RESTARTsrc} ]; then

  if [ ${FGAT_HR} = 03 ]; then
    tcvital=${WORKhafs}/intercom/launch/tm03vit
  elif [ ${FGAT_HR} = 06 ]; then
    tcvital=${WORKhafs}/intercom/launch/tmpvit
  elif [ ${FGAT_HR} = 09 ]; then
    tcvital=${WORKhafs}/intercom/launch/tp03vit
  else
    tcvital=${WORKhafs}/intercom/launch/tmpvit
  fi
  if [ ${merge_method} = vortexreplace ]; then
    MERGE_CMD="${APRUNO} ${DATOOL} vortexreplace --tcvital=${tcvital} --infile_date=${ymd}.${hh}0000 --vortexradius=650:700"
  elif [ ${merge_method} = domainmerge ]; then
    MERGE_CMD="${APRUNO} ${DATOOL} remap"
  else
    echo "FATAL ERROR: unsupported merge_method: ${merge_method}"
    exit 1
  fi

  ## Regional single domain configuration
  ## Regional with one or more nests configuration
  ## The following steps are needed
  ##   Step 1: merge srcd01 into dstd02 (for atm_merge) or merge srcd0[2...N] into srcd01 (for analysis_merge)
  ##   Step 2: merge srcd01 into dstd01
  ##   Step 3: merge srcd02 into dstd02, srcd03 into dstd03, etc.
  #elif [[ $nest_grids -ge 2 ]]; then
  
  RESTARTtmp=${DATA}/RESTARTtmp
  mkdir -p ${RESTARTtmp}
  
  if [ ${MERGE_TYPE} = init ]; then
  
#  # Step 1: merge srcd02 into srcd01 (for atm_merge)
  ${RLN} ${RESTARTsrc}/* ${RESTARTtmp}/
#  #for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
#  for var in fv_core.res fv_tracer.res fv_srf_wnd.res; do
#    in_grid=${RESTARTtmp}/grid_mspec_${yr}_${mn}_${dy}_${hh}.nc
#
#    out_grid=${RESTARTmrg}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
#
#    if [[ $var = sfc_data ]]; then
#      in_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.nc
#    else
#      in_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.tile1.nc
#    fi
#    out_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.nest02.tile2.nc
#    if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
#       [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
#      echo "FATAL ERROR: Missing in/out_grid or in/out_file"
#      exit 1
#    fi
#    ${MERGE_CMD} \
#      --in_grid=${in_grid} \
#      --out_grid=${out_grid} \
#      --in_file=${in_file} \
#      --out_file=${out_file} 2>&1 | tee ./merge_init_step1_${var}.log
#    export err=$?; err_chk
#  done
  
  # Step 2: merge srcd01 into dstd01
  # Only do this step for real storms
  # For multistorm, don't merge srcd01 (from storm-centric pre-processing) into dstd01 (domain-centric forecast domain)
  #for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
  for var in fv_core.res fv_tracer.res fv_srf_wnd.res; do
    in_grid=${RESTARTtmp}/grid_mspec_${yr}_${mn}_${dy}_${hh}.nc
    out_grid=${RESTARTmrg}/grid_mspec_${yr}_${mn}_${dy}_${hh}.nc
    if [[ $var = sfc_data ]]; then
      in_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.nc
      out_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.nc
    else
      in_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.tile1.nc
      out_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.tile1.nc
    fi
    if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
       [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
      echo "FATAL ERROR: Missing in/out_grid or in/out_file"
      exit 1
    fi
    ${MERGE_CMD} \
      --in_grid=${in_grid} \
      --out_grid=${out_grid} \
      --in_file=${in_file} \
      --out_file=${out_file} 2>&1 | tee ./merge_init_step2_${var}.log
    export err=$?; err_chk
  done
  
  else
    echo "FATAL ERROR: unsupported MERGE_TYPE: ${MERGE_TYPE}"
    exit 1
  fi

else

${RLN} ${RESTARTdst}/*  ${RESTARTmrg}/

fi
date
