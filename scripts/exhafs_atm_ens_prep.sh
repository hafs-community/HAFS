#!/bin/sh
################################################################################
# Script Name: exhafs_analysis.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script conducts the HAFS atmospheric data assimilation through GSI,
#   and generate analysis diagnoses (if desired).
################################################################################
set -x -o pipefail

export USE_CFP=${USE_CFP:NO}

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

export ANALYSISEXEC=${ANALYSISEXEC:-${EXEChafs}/hafs_convert.x}
export CATEXEC=${CATEXEC:-ncdiag_cat_serial.x}

FV3_AKBK_FILE=${PDY}.${cyc}0000.fv_core.res${neststr}.nc

export RESTARTout_dir=${RESTARTout_dir:-${WORKhafs}/intercom/ENS_PREP/mem${ENSID:-001}}
export DATA=${DATA:-${WORKhafs}/ENS_PREP/mem${ENSID:-001}${jobidstr}}

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
${APRUNX} ${ANALYSISEXEC} gdas_ens.yaml gdas_ens.out
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
${APRUNX} ${ANALYSISEXEC} gdas_ens.yaml gdas_ens.out
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
${APRUNX} ${ANALYSISEXEC} gdas_ens.yaml gdas_ens.out
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
