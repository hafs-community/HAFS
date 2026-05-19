#!/usr/bin/env bash
################################################################################
# Script Name: exhafs_analysis.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script conducts the HAFS atmospheric data assimilation through GSI,
#   and generate analysis diagnoses (if desired).
################################################################################
set -x -o pipefail

# CFP-like execution without a site CFP executable and without mpiserial.
# User-facing entry point: run this script normally once.
# Internal member entry point: written only into the generated command file.
# Each member launches the MPI executable with its own clean one-task srun:
#   srun --mem=0 -l -n6 ./hafs_jedi_convert.x gdas_ens.yaml gdas_ens.out
ENS_SIZE=${ENS_SIZE:-80}
PARALLEL_SHELL=${PARALLEL_SHELL:-bash}
PARALLEL_MEMBER_ARG="--parallel-member"

if [ "${1:-}" != "${PARALLEL_MEMBER_ARG}" ]; then
  case "$0" in
    /*) THIS_SCRIPT="$0" ;;
    *)  THIS_SCRIPT="$(pwd)/$0" ;;
  esac

  # Keep the same CFP-style work location you requested, but do not require cfp.
  export DATA_CFP=${DATA_CFP:-${WORKhafs}/ENS_PREP${jobidstr}/cfp_work}
  export PARALLEL_CMD_FILE=${PARALLEL_CMD_FILE:-${DATA_CFP}/cmdfile_ens_prep}
  export PARALLEL_LOG_DIR=${PARALLEL_LOG_DIR:-${DATA_CFP}/logs}
  export PARALLEL_STATUS_DIR=${PARALLEL_STATUS_DIR:-${DATA_CFP}/status}

  mkdir -p "${DATA_CFP}" "${PARALLEL_LOG_DIR}" "${PARALLEL_STATUS_DIR}"
  : > "${PARALLEL_CMD_FILE}"

  echo "INFO: Created parallel command file: ${PARALLEL_CMD_FILE}"
  echo "INFO: Member logs are in: ${PARALLEL_LOG_DIR}"
  echo "INFO: Member status files are in: ${PARALLEL_STATUS_DIR}"
  echo "INFO: Launching ${ENS_SIZE} member shells in the current batch job."
  echo "INFO: Each member uses: srun --mem=0 -l -n6 ./hafs_jedi_convert.x gdas_ens.yaml gdas_ens.out"

  pids=""
  pid_mems=""

  cleanup_parallel_children() {
    rc=$?
    echo "WARNING: Parent received termination signal or exited early with rc=${rc}." >&2
    echo "WARNING: Checking unfinished member wrapper processes." >&2
    for item in ${pid_mems}; do
      pid=${item%%:*}
      mem=${item#*:}
      if kill -0 "${pid}" 2>/dev/null; then
        echo "WARNING: mem${mem} wrapper pid ${pid} is still running; sending TERM." >&2
        kill "${pid}" 2>/dev/null || true
      fi
    done
    exit 143
  }
  trap cleanup_parallel_children TERM INT

  n=1
  while [ ${n} -le ${ENS_SIZE} ]; do
    mem=$(printf "%03d" ${n})
    mem_data=${WORKhafs}/ENS_PREP${jobidstr}/mem${mem}
    mem_restart=${WORKhafs}/intercom/ENS_PREP/mem${mem}
    mem_log=${PARALLEL_LOG_DIR}/mem${mem}.log
    mem_status=${PARALLEL_STATUS_DIR}/mem${mem}.status
    mkdir -p "${mem_data}" "${mem_restart}"

    echo "cd ${mem_data} && env ENSID=${mem} DATA=${mem_data} RESTARTout_dir=${mem_restart} ${PARALLEL_SHELL} ${THIS_SCRIPT} ${PARALLEL_MEMBER_ARG} > ${mem_log} 2>&1" >> "${PARALLEL_CMD_FILE}"
    echo "RUNNING" > "${mem_status}"

    (
      echo "INFO: mem${mem} wrapper start: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
      cd "${mem_data}" || exit 1
      env ENSID="${mem}" DATA="${mem_data}" RESTARTout_dir="${mem_restart}" \
          "${PARALLEL_SHELL}" "${THIS_SCRIPT}" "${PARALLEL_MEMBER_ARG}"
      rc=$?
      echo "INFO: mem${mem} wrapper end: $(date -u +%Y-%m-%dT%H:%M:%SZ), rc=${rc}"
      echo "${rc}" > "${mem_status}"
      exit ${rc}
    ) > "${mem_log}" 2>&1 &

    pid=$!
    pids="${pids} ${pid}"
    pid_mems="${pid_mems} ${pid}:${mem}"
    n=$((n+1))
  done

  err=0
  for item in ${pid_mems}; do
    pid=${item%%:*}
    mem=${item#*:}
    if wait "${pid}"; then
      echo "INFO: mem${mem} wrapper pid ${pid} finished successfully."
    else
      rc=$?
      echo "ERROR: mem${mem} wrapper pid ${pid} failed with rc=${rc}. See ${PARALLEL_LOG_DIR}/mem${mem}.log" >&2
      err=1
    fi
  done

  trap - TERM INT

  if [ ${err} -eq 0 ]; then
    echo "INFO: All ${ENS_SIZE} ensemble prep member wrappers completed successfully."
  else
    echo "ERROR: One or more ensemble prep members failed. Check ${PARALLEL_LOG_DIR} and ${PARALLEL_STATUS_DIR}." >&2
  fi

  export err
  err_chk
  exit ${err}
fi

# Internal member mode: one background shell runs one ensemble member.
# The JEDI converter itself is MPI-aware, so launch it through a clean one-task
# Slurm job step instead of running it directly under mpiserial/CFP-like PMI env.
shift
export APRUNX="srun --mem=0 -l -n6"

CDATE=${CDATE:-${YMDH}}
yr=$(echo $CDATE | cut -c1-4)
mn=$(echo $CDATE | cut -c5-6)
dy=$(echo $CDATE | cut -c7-8)
hh=$(echo $CDATE | cut -c9-10)
CDATEprior=$(${NDATE} -6 $CDATE)
ymdprior=$(echo ${CDATEprior} | cut -c1-8)
hhprior=$(echo ${CDATEprior} | cut -c9-10)
CDATEtm03=$(${NDATE} -3 $CDATE)
ymdtm03=$(echo ${CDATEtm03} | cut -c1-8)
yrtm03=$(echo ${CDATEtm03} | cut -c1-4)
mntm03=$(echo ${CDATEtm03} | cut -c5-6)
dytm03=$(echo ${CDATEtm03} | cut -c7-8)
hhtm03=$(echo ${CDATEtm03} | cut -c9-10)
CDATEtp03=$(${NDATE} +3 $CDATE)
ymdtp03=$(echo ${CDATEtp03} | cut -c1-8)
yrtp03=$(echo ${CDATEtp03} | cut -c1-4)
mntp03=$(echo ${CDATEtp03} | cut -c5-6)
dytp03=$(echo ${CDATEtp03} | cut -c7-8)
hhtp03=$(echo ${CDATEtp03} | cut -c9-10)
nest_grids=${nest_grids:-1}
npx_nest=$( echo ${npx} | cut -d , -f 2 )
npy_nest=$( echo ${npy} | cut -d , -f 2 )
if [ ${nest_grids} -eq 2 ]; then
  cenlat=$(echo "$output_grid_cen_lat" | cut -d',' -f2)
  cenlon=$(echo "$output_grid_cen_lon" | cut -d',' -f2)
else
  cenlat=$(echo "$output_grid_cen_lat" | cut -d',' -f1)
  cenlon=$(echo "$output_grid_cen_lon" | cut -d',' -f1)
fi
MIN_LON=$(echo "$cenlon - 10" | bc) #Domain cut for DA efficiency, need to consider domain flexibility later
MAX_LON=$(echo "$cenlon + 10" | bc)
MIN_LAT=$(echo "$cenlat -  8" | bc)
MAX_LAT=$(echo "$cenlat +  8" | bc)

export PARMjedi=${PARMjedi:-${PARMhafs}/analysis/jedi}
export FIXcrtm=${FIXcrtm:-${CRTM_FIX:?}}
export COMINgfs=${COMINgfs:?}
export COMINgdas=${COMINgdas:?}
export COMINobs=${COMINobs:?}
export COMINhafs=${COMINhafs:-${COMINgfs}}

export ANALYSIS_MODEL=${ANALYSIS_MODEL:-JEDI}
export RUN_ENVAR=${RUN_ENVAR:-NO}
export RUN_ENSDA=${RUN_ENSDA:-NO}
export gridstr=${gridstr:-$(echo ${out_gridnames} | cut -d, -f 1)}
export neststr=${neststr:-""} # ".nest02" for domain 02
export tilestr=${tilestr:-".tile1"} # ".tile2" for domain 02
export nesttilestr=${nesttilestr:-""} # ".nest02.tile2" for domain 02

export ANALYSISEXEC=${ANALYSISEXEC:-${EXEChafs}/hafs_jedi_convert.x}
export CATEXEC=${CATEXEC:-ncdiag_cat_serial.x}

FV3_AKBK_FILE=${PDY}.${cyc}0000.fv_core.res${neststr}.nc

export RESTARTout_dir=${RESTARTout_dir:-${WORKhafs}/intercom/ENS_PREP/mem${ENSID:-001}}
export DATA=${DATA:-${WORKhafs}/ENS_PREP${jobidstr}/mem${ENSID:-001}}

# We should already be in $DATA, but extra cd to be sure.
cd $DATA
# Link DataFix files
${NCP} ${PARMjedi}/fmsmpp.nml .
${NCP} ${PARMjedi}/satinfo .
sed -e "s|_NPX_|${npx_ens}|g" \
    -e "s|_NPY_|${npy_ens}|g" \
    -e "s|_NPZ_|${npz_ens}|g" \
    -e "s|_FV3_GRID_FILE_|${CASE}_mosaic.nc|g" \
    -e "s|_LAYOUTX_|${layoutx_gdasens}|g" \
    -e "s|_LAYOUTY_|${layouty_gdasens}|g" \
    -e "s|_DLON_|${target_lon}|g" \
    -e "s|_DLAT_|${target_lat}|g" \
    ${PARMjedi}/input_hafs.nml > input_hafs.nml

if [ ${nest_grids} -ge 2 ]; then
sed -e "s|_NPX_|${npx_nest}|g" \
    -e "s|_NPY_|${npy_nest}|g" \
    -e "s|_NPZ_|${npz}|g" \
    -e "s|_FV3_GRID_FILE_|${CASE}_mosaic_nest.nc|g" \
    -e "s|_LAYOUTX_|${layoutx_gdasens}|g" \
    -e "s|_LAYOUTY_|${layouty_gdasens}|g" \
    -e "s|_DLON_|${target_lon}|g" \
    -e "s|_DLAT_|${target_lat}|g" \
    ${PARMjedi}/input_hafs.nml > input_hafs_nest.nml
fi

# Copy the first guess or fgat files
if [ -d ${WORKhafs}/intercom/RESTART_init ]; then
  RESTARTinp=${WORKhafs}/intercom/RESTART_init
elif [ -d ${WORKhafs}/intercom/RESTART_vi ]; then
  RESTARTinp=${WORKhafs}/intercom/RESTART_vi
else
  RESTARTinp=${COMOLD}/${old_out_prefix}.RESTART
fi

if [ ! -s ${RESTARTinp}/${FV3_CORE_FILE} ]; then
  echo "WARNING: First guess for DA/Analysis missing"
  echo "WARNING: Do nothing, Exiting"
  exit
fi
mkdir ${DATA}/bkg
cd ${DATA}/bkg
${NLN} ${RESTARTinp}/${FV3_AKBK_FILE} .
if [ $GFSVER = "PROD2021" ]; then
  export INPUT_FILE3=${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/atmos/mem${ENSID}/gdas.t${hhprior}z.atmf003${GSUFFIX:-.nc}
  export INPUT_FILE6=${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/atmos/mem${ENSID}/gdas.t${hhprior}z.atmf006${GSUFFIX:-.nc}
  export INPUT_FILE9=${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/atmos/mem${ENSID}/gdas.t${hhprior}z.atmf009${GSUFFIX:-.nc}
elif [ $GFSVER = "PROD2026" ]; then
  export INPUT_FILE3=${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/mem${ENSID}/model/atmos/history/enkfgdas.t${hhprior}z.atm.f003.nc
  export INPUT_FILE6=${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/mem${ENSID}/model/atmos/history/enkfgdas.t${hhprior}z.atm.f006.nc
  export INPUT_FILE9=${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/mem${ENSID}/model/atmos/history/enkfgdas.t${hhprior}z.atm.f009.nc
fi

# Create mosaic files
mkdir ${DATA}/INPUT
cd ${DATA}/INPUT
${NCP} ${WORKhafs}/intercom/atm_prep/grid/${CASE}/${CASE}_grid.tile7.halo3.nc .
${NCP} ${EXEChafs}/hafs_utils_make_solo_mosaic.x .
${EXEChafs}/hafs_utils_make_solo_mosaic.x --num_tiles 1 --dir ${WORKhafs}/intercom/atm_prep/grid/${CASE} --mosaic ${CASE}_mosaic --tile_file ${CASE}_grid.tile7.halo3.nc 
if [ ${nest_grids} -ge 2 ]; then
 ${NCP} ${WORKhafs}/intercom/atm_prep/grid/${CASE}/${CASE}_grid.tile8.halo3.nc .
 ${NCP} ${EXEChafs}/hafs_utils_make_solo_mosaic.x .
 ${EXEChafs}/hafs_utils_make_solo_mosaic.x --num_tiles 1 --dir ${WORKhafs}/intercom/atm_prep/grid/${CASE} --mosaic ${CASE}_mosaic_nest --tile_file ${CASE}_grid.tile8.halo3.nc
fi

cd ${DATA}
export gdas_ens_yaml=${PARMjedi}/yaml_templates/basic_config
if [ ${nest_grids} -ge 2 ]; then
  INPUT_HAFS_NML=input_hafs_nest.nml
else
  INPUT_HAFS_NML=input_hafs.nml
fi
INPUT_HAFS_ENS_NML=input_hafs_nest.nml
#----------------------------------------------
sed -e "s|_INTERP_DATE_|${yrtm03}-${mntm03}-${dytm03}T${hhtm03}:00:00Z|g" \
    -e "s|_INPUT_HAFS_NML_|${INPUT_HAFS_ENS_NML}|g" \
    -e "s|_MIN_LAT_|${MIN_LAT}|g" \
    -e "s|_MAX_LAT_|${MAX_LAT}|g" \
    -e "s|_MIN_LON_|${MIN_LON}|g" \
    -e "s|_MAX_LON_|${MAX_LON}|g" \
    -e "s|_FV3_AKBK_FILE_|${FV3_AKBK_FILE}|g" \
    -e "s|_INPUT_FILE_|${INPUT_FILE3}|g" \
    -e "s|_OUTPUT_DIR_|${RESTARTout_dir}|g" \
    ${gdas_ens_yaml}/gdas_ens.yaml > gdas_ens.yaml
${SOURCE_PREP_STEP}
${NCP} ${ANALYSISEXEC} ./hafs_jedi_convert.x
${APRUNX} ./hafs_jedi_convert.x gdas_ens.yaml gdas_ens.out
export err=$?; err_chk
rm gdas_ens.out.*

sed -e "s|_INTERP_DATE_|${yr}-${mn}-${dy}T${hh}:00:00Z|g" \
    -e "s|_INPUT_HAFS_NML_|${INPUT_HAFS_ENS_NML}|g" \
    -e "s|_MIN_LAT_|${MIN_LAT}|g" \
    -e "s|_MAX_LAT_|${MAX_LAT}|g" \
    -e "s|_MIN_LON_|${MIN_LON}|g" \
    -e "s|_MAX_LON_|${MAX_LON}|g" \
    -e "s|_FV3_AKBK_FILE_|${FV3_AKBK_FILE}|g" \
    -e "s|_INPUT_FILE_|${INPUT_FILE6}|g" \
    -e "s|_OUTPUT_DIR_|${RESTARTout_dir}|g" \
    ${gdas_ens_yaml}/gdas_ens.yaml > gdas_ens.yaml
${SOURCE_PREP_STEP}
${APRUNX} ./hafs_jedi_convert.x gdas_ens.yaml gdas_ens.out
export err=$?; err_chk
sed -e "s|_INTERP_DATE_|${yrtp03}-${mntp03}-${dytp03}T${hhtp03}:00:00Z|g" \
    -e "s|_INPUT_HAFS_NML_|${INPUT_HAFS_ENS_NML}|g" \
    -e "s|_MIN_LAT_|${MIN_LAT}|g" \
    -e "s|_MAX_LAT_|${MAX_LAT}|g" \
    -e "s|_MIN_LON_|${MIN_LON}|g" \
    -e "s|_MAX_LON_|${MAX_LON}|g" \
    -e "s|_FV3_AKBK_FILE_|${FV3_AKBK_FILE}|g" \
    -e "s|_INPUT_FILE_|${INPUT_FILE9}|g" \
    -e "s|_OUTPUT_DIR_|${RESTARTout_dir}|g" \
    ${gdas_ens_yaml}/gdas_ens.yaml > gdas_ens.yaml
${SOURCE_PREP_STEP}
${APRUNX} ./hafs_jedi_convert.x gdas_ens.yaml gdas_ens.out
export err=$?; err_chk
rm gdas_ens.out.*

for file in "${RESTARTout_dir}"/*; do
    [ -e "$file" ] || continue   # skip if empty dir
    base=$(basename "$file")
    case "$base" in
        *.nc|*.nc4)
            echo "  [NC]  $file"
            tmp="${file}.tmp.$$"   # unique temp name
            if nccopy -k netCDF-4 -d 1 "$file" "$tmp"; then
                mv -f "$tmp" "$file"
            else
                echo "  ${file} conversion failed"
                rm -f "$tmp"
            fi
            ;;
        *)
            :  # do nothing for non-nc files
            ;;
    esac
done
