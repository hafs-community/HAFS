#!/bin/sh
################################################################################
# Script Name: exhafs_merge.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs hafs_datool to merge atmospheric restart files. It
#   supports the merge_type of analysis or init with the merge_method of
#   domainmerge or vortexreplace.
################################################################################
set -x -o pipefail

FGAT_MODEL=${FGAT_MODEL:-gfs}
FGAT_HR=${FGAT_HR:-00}

MPISERIAL=${MPISERIAL:-${EXEChafs}/hafs_tools_mpiserial.x}
DATOOL=${DATOOL:-${EXEChafs}/hafs_tools_datool.x}

# Merge analysis or init
if [ ${MERGE_TYPE} = analysis ]; then

merge_method=${analysis_merge_method:-vortexreplace}
# Deterministic or ensemble
if [ "${ENSDA}" = YES ]; then
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
else
  if [ -e ${WORKhafs}/intercom/RESTART_analysis ]; then
    RESTARTsrc=${WORKhafs}/intercom/RESTART_analysis
  elif [ -e ${WORKhafs}/intercom/RESTART_vi ]; then
    RESTARTsrc=${WORKhafs}/intercom/RESTART_vi
  else
    echo "FATAL ERROR: RESTARTsrc does not exist"
    exit 1
  fi
  RESTARTdst=${WORKhafs}/intercom/RESTART_init
  RESTARTmrg=${WORKhafs}/intercom/RESTART_analysis_merge
fi

elif [ ${MERGE_TYPE} = init ]; then

merge_method=${atm_merge_method:-vortexreplace}
if [ ${FGAT_MODEL} = gdas ]; then
  RESTARTsrc=${COMOLD}/${old_out_prefix}.RESTART
  RESTARTdst=${WORKhafs}/intercom/RESTART_init_fgat${FGAT_HR}
  RESTARTmrg=${WORKhafs}/intercom/RESTART_merge_fgat${FGAT_HR}
  CDATE=$(${NDATE} $(awk "BEGIN {print ${FGAT_HR}-6}") $CDATE)
else
  RESTARTsrc=${COMOLD}/${old_out_prefix}.RESTART
  RESTARTdst=${WORKhafs}/intercom/RESTART_init
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

DATA=${DATA:-${WORKhafs}/merge}

cd ${DATA}

mkdir -p ${RESTARTmrg}
${NCP} -rp ${RESTARTdst}/* ${RESTARTmrg}/

if [ -d ${RESTARTsrc} ] || [ -L ${RESTARTsrc} ]; then

if [ ${FGAT_HR} = 03 ]; then
  tcvital=${WORKhafs}/tm03vit
elif [ ${FGAT_HR} = 06 ]; then
  tcvital=${WORKhafs}/tmpvit
elif [ ${FGAT_HR} = 09 ]; then
  tcvital=${WORKhafs}/tp03vit
else
  tcvital=${WORKhafs}/tmpvit
fi
if [ ${merge_method} = vortexreplace ]; then
  MERGE_CMD="${APRUNC} ${DATOOL} vortexreplace --tcvital=${tcvital} --infile_date=${ymd}.${hh}0000 --vortexradius=650:700"
elif [ ${merge_method} = domainmerge ]; then
  MERGE_CMD="${APRUNC} ${DATOOL} remap"
else
  echo "FATAL ERROR: unsupported merge_method: ${merge_method}"
  exit 1
fi

# Regional single domain configuration
if [[ $nest_grids -eq 1 ]]; then

#for var in fv_core.res.tile1 fv_tracer.res.tile1 fv_srf_wnd.res.tile1 sfc_data phy_data; do
for var in fv_core.res.tile1 fv_tracer.res.tile1 fv_srf_wnd.res.tile1 sfc_data; do
  in_grid=${RESTARTsrc}/grid_spec.nc
  out_grid=${RESTARTmrg}/grid_spec.nc
  in_file=${RESTARTsrc}/${ymd}.${hh}0000.${var}.nc
  out_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.nc
  if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
     [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
    echo "FATAL ERROR: Missing in/out_grid or in/out_file"
    exit 1
  fi
  ${MERGE_CMD} \
    --in_grid=${in_grid} \
    --out_grid=${out_grid} \
    --in_file=${in_file} \
    --out_file=${out_file} 2>&1 | tee ./merge_regional_${var}.log
  export err=$?; err_chk
done

# Regional with one nest configuration
# The following steps are needed
#   Step 1: merge srcd01 into dstd02 (for atm_merge) or merge srcd02 into srcd01 (for analysis_merge)
#   Step 2: merge srcd01 into dstd01
#   Step 3: merge srcd02 into dstd02
elif [[ $nest_grids -eq 2 ]]; then

RESTARTtmp=${DATA}/RESTARTtmp
mkdir -p ${RESTARTtmp}

if [ ${MERGE_TYPE} = analysis ]; then

# Step 1: merge srcd02 into srcd01 (for analysis_merge)
${NCP} -rp ${RESTARTsrc}/* ${RESTARTtmp}/
for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
  in_grid=${RESTARTtmp}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
  out_grid=${RESTARTtmp}/grid_mspec_${yr}_${mn}_${dy}_${hh}.nc
  in_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.nest02.tile2.nc
  if [[ $var = sfc_data ]]; then
    out_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.nc
  else
    out_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.tile1.nc
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
    --out_file=${out_file} 2>&1 | tee ./merge_analysis_step1_${var}.log
  export err=$?; err_chk
done

elif [ ${MERGE_TYPE} = init ]; then

# Step 1: merge srcd02 into srcd01 (for atm_merge)
${RLN} ${RESTARTsrc}/* ${RESTARTtmp}/
for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
  in_grid=${RESTARTtmp}/grid_mspec_${yr}_${mn}_${dy}_${hh}.nc
  out_grid=${RESTARTmrg}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
  if [[ $var = sfc_data ]]; then
    in_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.nc
  else
    in_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.tile1.nc
  fi
  out_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.nest02.tile2.nc
  if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
     [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
    echo "FATAL ERROR: Missing in/out_grid or in/out_file"
    exit 1
  fi
  ${MERGE_CMD} \
    --in_grid=${in_grid} \
    --out_grid=${out_grid} \
    --in_file=${in_file} \
    --out_file=${out_file} 2>&1 | tee ./merge_init_step1_${var}.log
  export err=$?; err_chk
done

else
  echo "FATAL ERROR: unsupported MERGE_TYPE: ${MERGE_TYPE}"
  exit 1
fi

# Step 2: merge srcd01 into dstd01
for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
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

# Step 3: merge srcd02 into dstd02
for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data; do
  in_grid=${RESTARTtmp}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
  out_grid=${RESTARTmrg}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
  in_file=${RESTARTtmp}/${ymd}.${hh}0000.${var}.nest02.tile2.nc
  out_file=${RESTARTmrg}/${ymd}.${hh}0000.${var}.nest02.tile2.nc
  if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
     [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
    echo "FATAL ERROR: Missing in/out_grid or in/out_file"
    exit 1
  fi
  ${MERGE_CMD} \
    --in_grid=${in_grid} \
    --out_grid=${out_grid} \
    --in_file=${in_file} \
    --out_file=${out_file} 2>&1 | tee ./merge_init_step3_${var}.log
  export err=$?; err_chk
done

else
  echo "FATAL ERROR: only support nest_grids = 1 or 2"
  echo "FATAL ERROR: nest_grids = $nest_grids"
  exit 1
fi

else

echo "RESTARTsrc: ${RESTARTsrc} does not exist"
echo "RESTARTmrg is the same as RESTARTdst"

fi

date
