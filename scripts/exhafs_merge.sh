#!/bin/sh

set -xe

FGAT_MODEL=${FGAT_MODEL:-gfs}
FGAT_HR=${FGAT_HR:-00}

export merge_method=${merge_method:-vortexreplace}
export RESTARTsrc=${RESTARTsrc:-"${COMhafs}/RESTART_analysis"}
export RESTARTdst=${RESTARTdst:-"${COMhafs}/RESTART_init"}
export RESTARTmrg=${RESTARTmrg:-"${COMhafs}/RESTART_analysis_merge"}

TOTAL_TASKS=${TOTAL_TASKS:-2016}
NCTSK=${NCTSK:-12}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-2}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}

# Utilities
NDATE=${NDATE:-ndate}
export NCP=${NCP:-"/bin/cp"}
export NMV=${NMV:-"/bin/mv"}
export NLN=${NLN:-"/bin/ln -sf"}
export MPISERIAL=${MPISERIAL:-${EXEChafs}/hafs_mpiserial.x}
export DATOOL=${DATOOL:-${EXEChafs}/hafs_datool.x}

PDY=`echo $CDATE | cut -c1-8`
cyc=`echo $CDATE | cut -c9-10`
yr=`echo $CDATE | cut -c1-4`
mn=`echo $CDATE | cut -c5-6`
dy=`echo $CDATE | cut -c7-8`
hh=`echo $CDATE | cut -c9-10`

cd ${DATA}

mkdir -p ${RESTARTmrg}
${NCP} -rp ${RESTARTdst}/* ${RESTARTmrg}/

if [ -d ${RESTARTsrc} ] || [ -L ${RESTARTsrc} ] ; then

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
  MERGE_CMD="${DATOOL} vortexreplace --tcvital=${tcvital} --infile_date=${PDY}.${cyc}0000 --vortexradius=650:700"
elif [ ${merge_method} = domainmerge ]; then
  MERGE_CMD="${DATOOL} remap"
else
  echo "Error: unsupported merge_method: ${merge_method}"
  exit 1
fi

# Regional single domain configuration
if [[ $nest_grids -eq 1 ]]; then

rm -f cmdfile_datool_merge
#for var in fv_core.res.tile1 fv_tracer.res.tile1 fv_srf_wnd.res.tile1 sfc_data phy_data;
for var in fv_core.res.tile1 fv_tracer.res.tile1 fv_srf_wnd.res.tile1 sfc_data;
do
  in_grid=${RESTARTsrc}/grid_spec.nc
  out_grid=${RESTARTmrg}/grid_spec.nc
  in_file=${RESTARTsrc}/${PDY}.${cyc}0000.${var}.nc
  out_file=${RESTARTmrg}/${PDY}.${cyc}0000.${var}.nc
  if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
     [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
    echo "ERROR: Missing in/out_grid or in/out_file. Exitting..."
    exit 1
  fi
  cat >> cmdfile_datool_merge << EOF
  time ${MERGE_CMD} \
    --in_grid=${in_grid} \
    --out_grid=${out_grid} \
    --in_file=${in_file} \
    --out_file=${out_file} \
    > datool.${var}.log 2>&1
EOF
done
chmod +x cmdfile_datool_merge
[ $machine = wcoss_cray ] && set +e
if  [ ${machine} = "wcoss2" ]; then
   ncmd=$(cat ./cmdfile_datool_merge | wc -l)
   ncmd_max=$((ncmd < NCTSK ? ncmd : NCTSK))
   $APRUNCFP  -n $ncmd_max cfp ./cmdfile_datool_merge
else
   ${APRUNC} ${MPISERIAL} -m cmdfile_datool_merge
fi
[ $machine = wcoss_cray ] && set -e
cat datool.*.log

# Regional with one nest configuration
# The following steps are needed
#   Step 1: merge srcd01 into dstd02 (for atm_merge) or merge srcd02 into srcd01 (for analysis_merge)
#   Step 2: merge srcd01 into dstd01
#   Step 3: merge srcd02 into dstd02
elif [[ $nest_grids -eq 2 ]]; then

RESTARTtmp=${DATA}/RESTARTtmp
mkdir -p ${RESTARTtmp}

if [ ${MERGE_TYPE} = analysis ]; then

# Step 1: merge src02 into src01 (for analysis_merge)
${NCP} -rp ${RESTARTsrc}/* ${RESTARTtmp}/
rm -f cmdfile_datool_merge.step1
for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data;
do
  in_grid=${RESTARTtmp}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
  out_grid=${RESTARTtmp}/grid_mspec_${yr}_${mn}_${dy}_${hh}.nc
  in_file=${RESTARTtmp}/${PDY}.${cyc}0000.${var}.nest02.tile2.nc
  if [[ $var = sfc_data ]]; then
    out_file=${RESTARTtmp}/${PDY}.${cyc}0000.${var}.nc
  else
    out_file=${RESTARTtmp}/${PDY}.${cyc}0000.${var}.tile1.nc
  fi
  if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
     [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
    echo "ERROR: Missing in/out_grid or in/out_file. Exitting..."
    exit 1
  fi
  cat >> cmdfile_datool_merge.step1 << EOF
  time ${MERGE_CMD} \
    --in_grid=${in_grid} \
    --out_grid=${out_grid} \
    --in_file=${in_file} \
    --out_file=${out_file} \
    > datool.${var}.step1.log 2>&1
EOF
done
chmod +x cmdfile_datool_merge.step1
[ $machine = wcoss_cray ] && set +e
if  [ ${machine} = "wcoss2" ]; then
   ncmd=$(cat ./cmdfile_datool_merge.step1 | wc -l)
   ncmd_max=$((ncmd < NCTSK ? ncmd : NCTSK))
   $APRUNCFP  -n $ncmd_max cfp ./cmdfile_datool_merge.step1
else
   ${APRUNC} ${MPISERIAL} -m cmdfile_datool_merge.step1
fi
[ $machine = wcoss_cray ] && set -e
cat datool.*.step1.log

elif [ ${MERGE_TYPE} = init ]; then

# Step 1: merge srcd02 into srcd01 (for atm_merge)
${NLN} ${RESTARTsrc}/* ${RESTARTtmp}/
rm -f cmdfile_datool_merge.step1
for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data;
do
  in_grid=${RESTARTtmp}/grid_mspec_${yr}_${mn}_${dy}_${hh}.nc
  out_grid=${RESTARTmrg}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
  if [[ $var = sfc_data ]]; then
    in_file=${RESTARTtmp}/${PDY}.${cyc}0000.${var}.nc
  else
    in_file=${RESTARTtmp}/${PDY}.${cyc}0000.${var}.tile1.nc
  fi
  out_file=${RESTARTmrg}/${PDY}.${cyc}0000.${var}.nest02.tile2.nc
  if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
     [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
    echo "ERROR: Missing in/out_grid or in/out_file. Exitting..."
    exit 1
  fi
  cat >> cmdfile_datool_merge.step1 << EOF
  time ${MERGE_CMD} \
    --in_grid=${in_grid} \
    --out_grid=${out_grid} \
    --in_file=${in_file} \
    --out_file=${out_file} \
    > datool.${var}.step1.log 2>&1
EOF
done
chmod +x cmdfile_datool_merge.step1
[ $machine = wcoss_cray ] && set +e
if  [ ${machine} = "wcoss2" ]; then
   ncmd=$(cat ./cmdfile_datool_merge.step1 | wc -l)
   ncmd_max=$((ncmd < NCTSK ? ncmd : NCTSK))
   $APRUNCFP  -n $ncmd_max cfp ./cmdfile_datool_merge.step1
else
   ${APRUNC} ${MPISERIAL} -m cmdfile_datool_merge.step1
fi
[ $machine = wcoss_cray ] && set -e
cat datool.*.step1.log

else
  echo "Error unsupported MERGE_TYPE: ${MERGE_TYPE}"
  exit 1
fi

# Step 2: merge srcd01 into dstd01
rm -f cmdfile_datool_merge.step2
for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data;
do
  in_grid=${RESTARTtmp}/grid_mspec_${yr}_${mn}_${dy}_${hh}.nc
  out_grid=${RESTARTmrg}/grid_mspec_${yr}_${mn}_${dy}_${hh}.nc
  if [[ $var = sfc_data ]]; then
    in_file=${RESTARTtmp}/${PDY}.${cyc}0000.${var}.nc
    out_file=${RESTARTmrg}/${PDY}.${cyc}0000.${var}.nc
  else
    in_file=${RESTARTtmp}/${PDY}.${cyc}0000.${var}.tile1.nc
    out_file=${RESTARTmrg}/${PDY}.${cyc}0000.${var}.tile1.nc
  fi
  if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
     [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
    echo "ERROR: Missing in/out_grid or in/out_file. Exitting..."
    exit 1
  fi
  cat >> cmdfile_datool_merge.step2 << EOF
  time ${MERGE_CMD} \
    --in_grid=${in_grid} \
    --out_grid=${out_grid} \
    --in_file=${in_file} \
    --out_file=${out_file} \
    > datool.${var}.step2.log 2>&1
EOF
done
chmod +x cmdfile_datool_merge.step2
[ $machine = wcoss_cray ] && set +e
if [ ${machine} = "wcoss2" ]; then
   ncmd=$(cat ./cmdfile_datool_merge.step2 | wc -l)
   ncmd_max=$((ncmd < NCTSK ? ncmd : NCTSK))
   $APRUNCFP  -n $ncmd_max cfp ./cmdfile_datool_merge.step2
else
   ${APRUNC} ${MPISERIAL} -m cmdfile_datool_merge.step2
fi
[ $machine = wcoss_cray ] && set -e
cat datool.*.step2.log

# Step 3: merge srcd02 into dstd02
rm -f cmdfile_datool_merge.step3
for var in fv_core.res fv_tracer.res fv_srf_wnd.res sfc_data;
do
  in_grid=${RESTARTtmp}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
  out_grid=${RESTARTmrg}/grid_mspec.nest02_${yr}_${mn}_${dy}_${hh}.tile2.nc
  in_file=${RESTARTtmp}/${PDY}.${cyc}0000.${var}.nest02.tile2.nc
  out_file=${RESTARTmrg}/${PDY}.${cyc}0000.${var}.nest02.tile2.nc
  if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
     [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
    echo "ERROR: Missing in/out_grid or in/out_file. Exitting..."
    exit 1
  fi
  cat >> cmdfile_datool_merge.step3 << EOF
  time ${MERGE_CMD} \
    --in_grid=${in_grid} \
    --out_grid=${out_grid} \
    --in_file=${in_file} \
    --out_file=${out_file} \
    > datool.${var}.step3.log 2>&1
EOF
done
chmod +x cmdfile_datool_merge.step3
[ $machine = wcoss_cray ] && set +e
if [ ${machine} = "wcoss2" ]; then
   ncmd=$(cat ./cmdfile_datool_merge.step3 | wc -l)
   ncmd_max=$((ncmd < NCTSK ? ncmd : NCTSK))
   $APRUNCFP  -n $ncmd_max cfp ./cmdfile_datool_merge.step3
else
   ${APRUNC} ${MPISERIAL} -m cmdfile_datool_merge.step3
fi
[ $machine = wcoss_cray ] && set -e
cat datool.*.step3.log

else
  echo "Error: only support nest_grids = 1 or 2"
  echo "Error: nest_grids = $nest_grids"
  echo "Error: exiting"
  exit 1
fi

else

echo "RESTARTsrc: ${RESTARTsrc} does not exist"
echo "RESTARTmrg is the same as RESTARTdst"

fi

exit
