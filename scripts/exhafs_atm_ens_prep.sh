#!/usr/bin/env bash
################################################################################
set -x -o pipefail
ENS_SIZE=${ENS_SIZE:-80}
CDATE=${CDATE:-${YMDH}}
yr=$(echo "${CDATE}" | cut -c1-4)
mn=$(echo "${CDATE}" | cut -c5-6)
dy=$(echo "${CDATE}" | cut -c7-8)
hh=$(echo "${CDATE}" | cut -c9-10)
CDATEprior=$(${NDATE} -6 "${CDATE}")
ymdprior=$(echo "${CDATEprior}" | cut -c1-8)
hhprior=$(echo "${CDATEprior}" | cut -c9-10)
CDATEtm03=$(${NDATE} -3 "${CDATE}")
ymdtm03=$(echo "${CDATEtm03}" | cut -c1-8)
yrtm03=$(echo "${CDATEtm03}" | cut -c1-4)
mntm03=$(echo "${CDATEtm03}" | cut -c5-6)
dytm03=$(echo "${CDATEtm03}" | cut -c7-8)
hhtm03=$(echo "${CDATEtm03}" | cut -c9-10)
CDATEtp03=$(${NDATE} +3 "${CDATE}")
ymdtp03=$(echo "${CDATEtp03}" | cut -c1-8)
yrtp03=$(echo "${CDATEtp03}" | cut -c1-4)
mntp03=$(echo "${CDATEtp03}" | cut -c5-6)
dytp03=$(echo "${CDATEtp03}" | cut -c7-8)
hhtp03=$(echo "${CDATEtp03}" | cut -c9-10)

nest_grids=${nest_grids:-1}
npx_nest=$(echo "${npx}" | cut -d , -f 2)
npy_nest=$(echo "${npy}" | cut -d , -f 2)
if [ "${nest_grids}" -eq 2 ]; then
  cenlat=$(echo "${output_grid_cen_lat}" | cut -d',' -f2)
  cenlon=$(echo "${output_grid_cen_lon}" | cut -d',' -f2)
else
  cenlat=$(echo "${output_grid_cen_lat}" | cut -d',' -f1)
  cenlon=$(echo "${output_grid_cen_lon}" | cut -d',' -f1)
fi
MIN_LON=$(echo "${cenlon} - 10" | bc) # Domain cut for DA efficiency.
MAX_LON=$(echo "${cenlon} + 10" | bc)
MIN_LAT=$(echo "${cenlat} -  8" | bc)
MAX_LAT=$(echo "${cenlat} +  8" | bc)

export PARMjedi=${PARMjedi:-${PARMhafs}/analysis/jedi}
export FIXcrtm=${FIXcrtm:-${CRTM_FIX:?}}
export COMINgfs=${COMINgfs:?}
export COMINgdas=${COMINgdas:?}
export COMINobs=${COMINobs:?}
export COMINhafs=${COMINhafs:-${COMINgfs}}

export ANALYSIS_MODEL=${ANALYSIS_MODEL:-JEDI}
export RUN_ENVAR=${RUN_ENVAR:-NO}
export RUN_ENSDA=${RUN_ENSDA:-NO}
export gridstr=${gridstr:-$(echo "${out_gridnames}" | cut -d, -f 1)}
export neststr=${neststr:-""}       # ".nest02" for domain 02
export tilestr=${tilestr:-".tile1"} # ".tile2" for domain 02
export nesttilestr=${nesttilestr:-""}

# Ensemble ConvertState executable. This should be the new executable that wraps
# oops::EnsembleApplication<oops::ConvertState<fv3jedi::Traits>>.
export ANALYSISEXEC=${ANALYSISEXEC:-${EXEChafs}/hafs_jedi_convert.x}
export CATEXEC=${CATEXEC:-ncdiag_cat_serial.x}

FV3_AKBK_FILE=${PDY}.${cyc}0000.fv_core.res${neststr}.nc

# Common run directory for the single ensemble launch.
export DATA=${DATA:-${WORKhafs}/ENS_PREP${jobidstr}}
export ENS_YAML_ROOT=${ENS_YAML_ROOT:-${DATA}/ens_yamls}
# Private per-member working directories used by the OOPS EnsembleApplication
# wrapper.  Each member chdirs here before FV3-JEDI Geometry/FMS initialization
# so FMS/MPP runtime files such as warnfile.000000.out do not collide.
#
# Use exactly ens_member_cwd/mem### because the wrapper/runtime is already using
# this location for member-local files.
export ENS_MEMBER_CWD_ROOT=${DATA}/ens_member_cwd
export ENS_ADD_WORKDIR_TO_YAML=${ENS_ADD_WORKDIR_TO_YAML:-YES}
export ENS_LOG_ROOT=${ENS_LOG_ROOT:-${DATA}/logs}
export RLN=${RLN:-/bin/ln -sf}

mkdir -p "${DATA}" "${ENS_YAML_ROOT}" "${ENS_MEMBER_CWD_ROOT}" "${ENS_LOG_ROOT}"
cd "${DATA}" || exit 1

echo "INFO: ENS_SIZE=${ENS_SIZE}"
echo "INFO: RTASKS=${RTASKS}"
echo "INFO: TOTAL_TASKS=${TOTAL_TASKS}"
echo "INFO: APRUNX=${APRUNX}"
echo "INFO: DATA=${DATA}"
echo "INFO: ENS_YAML_ROOT=${ENS_YAML_ROOT}"
echo "INFO: ENS_MEMBER_CWD_ROOT=${ENS_MEMBER_CWD_ROOT}"
echo "INFO: ENS_ADD_WORKDIR_TO_YAML=${ENS_ADD_WORKDIR_TO_YAML}"

# Link/copy common JEDI/FMS files into the common run directory.
${NCP} "${PARMjedi}/fmsmpp.nml" .
${NCP} "${PARMjedi}/satinfo" .

# Copy the ensemble-capable executable once.
${NCP} "${ANALYSISEXEC}" .

# Copy the first guess or fgat files.
if [ -d "${WORKhafs}/intercom/RESTART_init" ]; then
  RESTARTinp=${WORKhafs}/intercom/RESTART_init
elif [ -d "${WORKhafs}/intercom/RESTART_vi" ]; then
  RESTARTinp=${WORKhafs}/intercom/RESTART_vi
else
  RESTARTinp=${COMOLD}/${old_out_prefix}.RESTART
fi

if [ ! -s "${RESTARTinp}/${FV3_CORE_FILE}" ]; then
  echo "WARNING: First guess for DA/Analysis missing"
  echo "WARNING: Do nothing, Exiting"
  exit 0
fi

# Common geometry/input files. These are read-only during the ensemble run and can
# be shared by all member YAMLs.
mkdir -p "${DATA}/bkg" "${DATA}/INPUT"
cd "${DATA}/bkg" || exit 1
${NLN} "${RESTARTinp}/${FV3_AKBK_FILE}" .

cd "${DATA}/INPUT" || exit 1
${NCP} "${WORKhafs}/intercom/atm_prep/grid/${CASE}/${CASE}_grid.tile7.halo3.nc" .
${NCP} "${EXEChafs}/hafs_utils_make_solo_mosaic.x" .
"./hafs_utils_make_solo_mosaic.x" \
  --num_tiles 1 \
  --dir "${WORKhafs}/intercom/atm_prep/grid/${CASE}" \
  --mosaic "${CASE}_mosaic" \
  --tile_file "${CASE}_grid.tile7.halo3.nc"

if [ "${nest_grids}" -ge 2 ]; then
  ${NCP} "${WORKhafs}/intercom/atm_prep/grid/${CASE}/${CASE}_grid.tile8.halo3.nc" .
  ${NCP} "${EXEChafs}/hafs_utils_make_solo_mosaic.x" .
  "./hafs_utils_make_solo_mosaic.x" \
    --num_tiles 1 \
    --dir "${WORKhafs}/intercom/atm_prep/grid/${CASE}" \
    --mosaic "${CASE}_mosaic_nest" \
    --tile_file "${CASE}_grid.tile8.halo3.nc"
fi

cd "${DATA}" || exit 1

# Build the FV3-JEDI namelist in the common DATA directory.  Member working
# directories symlink this namelist so relative YAML paths remain portable.
sed -e "s|_NPX_|${npx_ens}|g" \
    -e "s|_NPY_|${npy_ens}|g" \
    -e "s|_NPZ_|${npz_ens}|g" \
    -e "s|_FV3_GRID_FILE_|${CASE}_mosaic.nc|g" \
    -e "s|_LAYOUTX_|${layoutx_gdasens}|g" \
    -e "s|_LAYOUTY_|${layouty_gdasens}|g" \
    -e "s|_DLON_|${target_lon}|g" \
    -e "s|_DLAT_|${target_lat}|g" \
    "${PARMjedi}/input_hafs.nml" > "${DATA}/input_hafs.nml"

if [ "${nest_grids}" -ge 2 ]; then
  sed -e "s|_NPX_|${npx_nest}|g" \
      -e "s|_NPY_|${npy_nest}|g" \
      -e "s|_NPZ_|${npz}|g" \
      -e "s|_FV3_GRID_FILE_|${CASE}_mosaic_nest.nc|g" \
      -e "s|_LAYOUTX_|${layoutx_gdasens}|g" \
      -e "s|_LAYOUTY_|${layouty_gdasens}|g" \
      -e "s|_DLON_|${target_lon}|g" \
      -e "s|_DLAT_|${target_lat}|g" \
      "${PARMjedi}/input_hafs.nml" > "${DATA}/input_hafs_nest.nml"
fi

export gdas_ens_yaml=${PARMjedi}/yaml_templates/basic_config
if [ "${nest_grids}" -ge 2 ]; then
  INPUT_HAFS_ENS_NML=input_hafs_nest.nml
else
  INPUT_HAFS_ENS_NML=input_hafs.nml
fi

setup_member_cwd() {
  local mem="$1"
  local mem_cwd="${ENS_MEMBER_CWD_ROOT}/mem${mem}"

  mkdir -p "${mem_cwd}"

  # Create symlinks from inside the member cwd.  Do not use NLN/WLN here:
  # those helpers check the source path relative to the current shell cwd.
  # The ../../ paths below are intentionally relative to ${mem_cwd}.
  (
    cd "${mem_cwd}" || exit 1

    rm -f bkg INPUT fmsmpp.nml satinfo input_hafs.nml input_hafs_nest.nml intercom

    ${RLN} ../../bkg bkg
    ${RLN} ../../INPUT INPUT
    ${RLN} ../../fmsmpp.nml fmsmpp.nml
    ${RLN} ../../satinfo satinfo

    if [ -s "../../input_hafs.nml" ]; then
      ${RLN} ../../input_hafs.nml input_hafs.nml
    fi

    if [ -s "../../input_hafs_nest.nml" ]; then
      ${RLN} ../../input_hafs_nest.nml input_hafs_nest.nml
    fi

    # Only needed if a YAML/template uses relative output paths under intercom/.
    # The current script uses absolute output dirs, but this keeps the cwd portable.
    if [ -d "${WORKhafs}/intercom" ]; then
      ${RLN} "${WORKhafs}/intercom" intercom
    fi

    # Fail early if the member cwd is not usable.  FMS/MPP reads this file
    # from the current working directory during Geometry initialization.
    if [ ! -s "fmsmpp.nml" ]; then
      echo "ERROR: ${mem_cwd}/fmsmpp.nml is missing or broken" >&2
      ls -l "${mem_cwd}" >&2
      exit 1
    fi
  )

  echo "${mem_cwd}"
}

get_member_input_file() {
  local mem="$1"
  local lead="$2"

  if [ "${GFSVER}" = "PROD2021" ]; then
    echo "${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/atmos/mem${mem}/gdas.t${hhprior}z.atmf${lead}${GSUFFIX:-.nc}"
  elif [ "${GFSVER}" = "PROD2026" ]; then
    echo "${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/mem${mem}/model/atmos/history/enkfgdas.t${hhprior}z.atm.f${lead}.nc"
  else
    echo "ERROR: Unsupported GFSVER=${GFSVER}" >&2
    return 2
  fi
}

create_ensemble_yaml() {
  local tag="$1"
  local interp_date="$2"
  local lead="$3"
  local member_yaml_dir="${ENS_YAML_ROOT}/${tag}"
  local ens_yaml="${ENS_YAML_ROOT}/ens_${tag}.yaml"
  local n mem mem_restart mem_cwd mem_cwd_rel input_file member_yaml

  mkdir -p "${member_yaml_dir}"
  : > "${ens_yaml}"
  echo "files:" >> "${ens_yaml}"

  n=1
  while [ "${n}" -le "${ENS_SIZE}" ]; do
    mem=$(printf "%03d" "${n}")
    mem_restart=../../../intercom/ENS_PREP/mem${mem}
    input_file=$(get_member_input_file "${mem}" "${lead}") || return 2
    member_yaml=${member_yaml_dir}/gdas_ens_${tag}_mem${mem}.yaml

    mkdir -p "${mem_restart}"

    mem_cwd=$(setup_member_cwd "${mem}")
    mem_cwd_rel="ens_member_cwd/mem${mem}"

    if [ "${ENS_ADD_WORKDIR_TO_YAML}" = "YES" ]; then
      {
        echo "working directory: ${mem_cwd_rel}"
        sed -e "s|_INTERP_DATE_|${interp_date}|g" \
            -e "s|_INPUT_HAFS_NML_|${INPUT_HAFS_ENS_NML}|g" \
            -e "s|_MIN_LAT_|${MIN_LAT}|g" \
            -e "s|_MAX_LAT_|${MAX_LAT}|g" \
            -e "s|_MIN_LON_|${MIN_LON}|g" \
            -e "s|_MAX_LON_|${MAX_LON}|g" \
            -e "s|_FV3_AKBK_FILE_|${FV3_AKBK_FILE}|g" \
            -e "s|_INPUT_FILE_|${input_file}|g" \
            -e "s|_OUTPUT_DIR_|${mem_restart}|g" \
            "${gdas_ens_yaml}/gdas_ens.yaml"
      } > "${member_yaml}"
    else
      sed -e "s|_INTERP_DATE_|${interp_date}|g" \
          -e "s|_INPUT_HAFS_NML_|${INPUT_HAFS_ENS_NML}|g" \
          -e "s|_MIN_LAT_|${MIN_LAT}|g" \
          -e "s|_MAX_LAT_|${MAX_LAT}|g" \
          -e "s|_MIN_LON_|${MIN_LON}|g" \
          -e "s|_MAX_LON_|${MAX_LON}|g" \
          -e "s|_FV3_AKBK_FILE_|${FV3_AKBK_FILE}|g" \
          -e "s|_INPUT_FILE_|${input_file}|g" \
          -e "s|_OUTPUT_DIR_|${mem_restart}|g" \
          "${gdas_ens_yaml}/gdas_ens.yaml" > "${member_yaml}"
    fi

    echo "  - ${member_yaml}" >> "${ens_yaml}"

    n=$((n+1))
  done

  echo "${ens_yaml}"
}

run_ensemble_convert() {
  local tag="$1"
  local interp_date="$2"
  local lead="$3"
  local ens_yaml
  local log_file="${ENS_LOG_ROOT}/ens_${tag}.log"

  ens_yaml=$(create_ensemble_yaml "${tag}" "${interp_date}" "${lead}") || return 2

  echo "INFO: Running ensemble ConvertState for ${tag}"
  echo "INFO: Ensemble YAML: ${ens_yaml}"
  echo "INFO: Log file: ${log_file}"

  # Keep this hook close to the executable launch.  It now runs once per ensemble
  # stage, not once per member.
  ${SOURCE_PREP_STEP}

  ${APRUNX} ./hafs_jedi_convert.x "${ens_yaml}" "ens_${tag}.out" > "${log_file}" 2>&1
  export err=$?
  if [ "${err}" -ne 0 ]; then
    echo "ERROR: Ensemble ConvertState failed for ${tag}; see ${log_file}" >&2
  fi
  err_chk

  rm -f "ens_${tag}.out".*
}

# The old script did three separate FGAT conversions: -3h, analysis time, +3h.
# This keeps that science behavior, but each stage is now one ensemble launch
# instead of 80 separate member launches.
run_ensemble_convert "tm03" "${yrtm03}-${mntm03}-${dytm03}T${hhtm03}:00:00Z" "003"
run_ensemble_convert "anal" "${yr}-${mn}-${dy}T${hh}:00:00Z" "006"
run_ensemble_convert "tp03" "${yrtp03}-${mntp03}-${dytp03}T${hhtp03}:00:00Z" "009"

# ---------------------------------------------------------------------------
# NetCDF-4 compression for ensemble outputs
# ---------------------------------------------------------------------------

ENS_POST_NJOBS=${ENS_SIZE}
ENS_POST_DEFLATE=${ENS_POST_DEFLATE:-1}
ENS_POST_OUT_ROOT=${WORKhafs}/intercom/ENS_PREP
ENS_POST_LIST=${DATA}/postprocess_nc_files.list
ENS_POST_LOG_ROOT=${ENS_LOG_ROOT}/postprocess
ENS_POST_STATUS_ROOT=${ENS_POST_LOG_ROOT}/status
ENS_POST_HELPER=${DATA}/postprocess_one_nc.sh
ENS_POST_CMD_FILE=${DATA}/postprocess_nc_cmds.sh
ENS_POST_RETRY_LIST=${DATA}/postprocess_nc_files.retry.list
ENS_POST_RETRY_CMD_FILE=${DATA}/postprocess_nc_cmds.retry.sh

mkdir -p "${ENS_POST_LOG_ROOT}" "${ENS_POST_STATUS_ROOT}"
: > "${ENS_POST_LIST}"

# Build file list.
n=1
while [ "${n}" -le "${ENS_SIZE}" ]; do
  mem=$(printf "%03d" "${n}")
  memdir="${ENS_POST_OUT_ROOT}/mem${mem}"

  if [ -d "${memdir}" ]; then
    find "${memdir}" -maxdepth 1 -type f \( -name '*.nc' -o -name '*.nc4' \) | sort >> "${ENS_POST_LIST}"
  fi

  n=$((n+1))
done

nfiles=$(wc -l < "${ENS_POST_LIST}" | awk '{print $1}')
echo "INFO: Found ${nfiles} NetCDF files for post-processing"

if [ "${nfiles}" -eq 0 ]; then
  echo "WARNING: No NetCDF files found under ${ENS_POST_OUT_ROOT}/mem*"
else

cat > "${ENS_POST_HELPER}" <<'EOF_POST_NC'
#!/usr/bin/env bash
set -o pipefail

file=$1
deflate=${ENS_POST_DEFLATE:-1}
log_root=${ENS_POST_LOG_ROOT:?}
status_root=${ENS_POST_STATUS_ROOT:?}

base=$(basename "${file}")
mem=$(basename "$(dirname "${file}")")
log="${log_root}/${mem}_${base}.log"
status="${status_root}/${mem}_${base}.status"
tmp="${file}.tmp.$$.${RANDOM}"

{
  echo "INFO: start $(date -u +%Y-%m-%dT%H:%M:%SZ) ${file}"

  ncks -O -4 -L "${deflate}" "${file}" "${tmp}"
  rc=$?

  if [ "${rc}" -eq 0 ] && [ -s "${tmp}" ]; then
    mv -f "${tmp}" "${file}"
    rc=$?
  else
    rm -f "${tmp}"
    [ "${rc}" -eq 0 ] && rc=1
  fi

  echo "${rc}" > "${status}"
  echo "INFO: end $(date -u +%Y-%m-%dT%H:%M:%SZ) ${file} rc=${rc}"
  exit "${rc}"
} > "${log}" 2>&1
EOF_POST_NC

chmod +x "${ENS_POST_HELPER}"

export ENS_POST_DEFLATE ENS_POST_LOG_ROOT ENS_POST_STATUS_ROOT

# Build command file, similar to orog.file1 logic.
: > "${ENS_POST_CMD_FILE}"

ijob=0
while IFS= read -r file; do
  [ -n "${file}" ] || continue

  printf "%q %q &\n" "${ENS_POST_HELPER}" "${file}" >> "${ENS_POST_CMD_FILE}"

  ijob=$((ijob+1))
  if [ "${ijob}" -ge "${ENS_POST_NJOBS}" ]; then
    echo "wait" >> "${ENS_POST_CMD_FILE}"
    ijob=0
  fi
done < "${ENS_POST_LIST}"

if [ "${ijob}" -ne 0 ]; then
  echo "wait" >> "${ENS_POST_CMD_FILE}"
fi

chmod u+x "${ENS_POST_CMD_FILE}"

echo "INFO: Running NetCDF post-processing with ENS_POST_NJOBS=${ENS_POST_NJOBS}"
time "${ENS_POST_CMD_FILE}"
post_rc=$?

# Build retry list from failed status files.
: > "${ENS_POST_RETRY_LIST}"

while IFS= read -r file; do
  [ -n "${file}" ] || continue

  base=$(basename "${file}")
  mem=$(basename "$(dirname "${file}")")
  status="${ENS_POST_STATUS_ROOT}/${mem}_${base}.status"

  if [ ! -s "${status}" ]; then
    echo "${file}" >> "${ENS_POST_RETRY_LIST}"
  else
    rc=$(cat "${status}" 2>/dev/null || echo 1)
    if [ "${rc}" != "0" ]; then
      echo "${file}" >> "${ENS_POST_RETRY_LIST}"
    fi
  fi
done < "${ENS_POST_LIST}"

nretry=$(wc -l < "${ENS_POST_RETRY_LIST}" | awk '{print $1}')

if [ "${nretry}" -gt 0 ]; then
  echo "WARNING: Retrying ${nretry} failed NetCDF files serially"

  : > "${ENS_POST_RETRY_CMD_FILE}"

  while IFS= read -r file; do
    [ -n "${file}" ] || continue
    printf "%q %q\n" "${ENS_POST_HELPER}" "${file}" >> "${ENS_POST_RETRY_CMD_FILE}"
  done < "${ENS_POST_RETRY_LIST}"

  chmod u+x "${ENS_POST_RETRY_CMD_FILE}"
  time "${ENS_POST_RETRY_CMD_FILE}"
fi

# Final failure check.
failed=0

while IFS= read -r file; do
  [ -n "${file}" ] || continue

  base=$(basename "${file}")
  mem=$(basename "$(dirname "${file}")")
  status="${ENS_POST_STATUS_ROOT}/${mem}_${base}.status"

  rc=$(cat "${status}" 2>/dev/null || echo 1)
  if [ "${rc}" != "0" ]; then
    echo "ERROR: post-processing failed: ${file}"
    echo "ERROR: log: ${ENS_POST_LOG_ROOT}/${mem}_${base}.log"
    failed=1
  fi
done < "${ENS_POST_LIST}"

if [ "${failed}" -ne 0 ]; then
  export err=1
  err_chk
fi

echo "INFO: NetCDF post-processing completed"

fi
