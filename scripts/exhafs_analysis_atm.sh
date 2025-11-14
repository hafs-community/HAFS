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
DATOOL=${DATOOL:-${EXEChafs}/hafs_tools_datool.x}
MERGE_CMD="${APRUNS} ${DATOOL} remap"

export PARMjedi=${PARMjedi:-${PARMhafs}/analysis/jedi}
export FIXcrtm=${FIXcrtm:-${CRTM_FIX:?}}
export COMINgfs=${COMINgfs:?}
export COMINgdas=${COMINgdas:?}
export COMINobs=${COMINobs:?}
export COMINhafs=${COMINhafs:-${COMINgfs}}
export DONST=${DONST:-"NO"}
export LEVS=${LEVS:-65}
export use_bufr_nr=${use_bufr_nr:-no}
export grid_ratio_fv3_regional=${grid_ratio_fv3_regional:-1}
export s_ens_h=${s_ens_h:-150}
export s_ens_v=${s_ens_v:--0.5}
export out_prefix=${out_prefix:-$(echo "${STORMID,,}.${CDATE}")}
export nsclgrp=${nsclgrp:-1}
export naensloc=${naensloc:-1}

export ANALYSIS_MODEL=${ANALYSIS_MODEL:-JEDI}
export RUN_FGAT=${RUN_FGAT:-NO}
export RUN_ALLSKY=${RUN_ALLSKY:-NO}
export FGAT=${FGAT:-NO}
export RUN_ENVAR=${RUN_ENVAR:-NO}
export RUN_ENSDA=${RUN_ENSDA:-NO}
export ENSDA=${ENSDA:-NO}
export GRID_RATIO_ENS=${GRID_RATIO_ENS:-1}
export online_satbias=${online_satbias:-no}
export l_both_fv3sar_gfs_ens=${l_both_fv3sar_gfs_ens:-.false.}
export n_ens_gfs=${n_ens_gfs:-80}
export n_ens_fv3sar=${n_ens_fv3sar:-${ENS_SIZE:-20}}
export l4densvar=${l4densvar:-.false.}
export nhr_obsbin=${nhr_obsbin:--1}

export ANALYSIS_D01=${ANALYSIS_D01:-NO}
export ANALYSIS_D02=${ANALYSIS_D02:-NO}
export gridstr=${gridstr:-$(echo ${out_gridnames} | cut -d, -f 1)}
export neststr=${neststr:-""} # ".nest02" for domain 02
export tilestr=${tilestr:-".tile1"} # ".tile2" for domain 02
export nesttilestr=${nesttilestr:-""} # ".nest02.tile2" for domain 02

export ANALYSISEXEC=${ANALYSISEXEC:-${EXEChafs}/hafs_jedi.x}
export CATEXEC=${CATEXEC:-ncdiag_cat_serial.x}

FV3_CORE_ENS_FILE=${PDY}.${cyc}0000.fv_core.res.tile1.nc
FV3_TRCR_ENS_FILE=${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
FV3_SFCD_ENS_FILE=${PDY}.${cyc}0000.sfc_data.nc
FV3_SFCW_ENS_FILE=${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc
FV3_CPLR_ENS_FILE=${PDY}.${cyc}0000.coupler.res
FV3_AKBK_ENS_FILE=${PDY}.${cyc}0000.fv_core.res.nc
FV3_CORE_ENS_FILE3=${ymdtm03}.${hhtm03}0000.fv_core.res.tile1.nc
FV3_TRCR_ENS_FILE3=${ymdtm03}.${hhtm03}0000.fv_tracer.res.tile1.nc
FV3_SFCD_ENS_FILE3=${ymdtm03}.${hhtm03}0000.sfc_data.nc
FV3_SFCW_ENS_FILE3=${ymdtm03}.${hhtm03}0000.fv_srf_wnd.res.tile1.nc
FV3_CPLR_ENS_FILE3=${ymdtm03}.${hhtm03}0000.coupler.res
FV3_AKBK_ENS_FILE3=${ymdtm03}.${hhtm03}0000.fv_core.res.nc
FV3_CORE_ENS_FILE9=${ymdtp03}.${hhtp03}0000.fv_core.res.tile1.nc
FV3_TRCR_ENS_FILE9=${ymdtp03}.${hhtp03}0000.fv_tracer.res.tile1.nc
FV3_SFCD_ENS_FILE9=${ymdtp03}.${hhtp03}0000.sfc_data.nc
FV3_SFCW_ENS_FILE9=${ymdtp03}.${hhtp03}0000.fv_srf_wnd.res.tile1.nc
FV3_CPLR_ENS_FILE9=${ymdtp03}.${hhtp03}0000.coupler.res
FV3_AKBK_ENS_FILE9=${ymdtp03}.${hhtp03}0000.fv_core.res.nc
FV3_CORE_FILE=${PDY}.${cyc}0000.fv_core.res${neststr}${tilestr}.nc
FV3_TRCR_FILE=${PDY}.${cyc}0000.fv_tracer.res${neststr}${tilestr}.nc
FV3_SFCD_FILE=${PDY}.${cyc}0000.sfc_data${neststr}${tilestr}.nc
FV3_SFCW_FILE=${PDY}.${cyc}0000.fv_srf_wnd.res${neststr}${tilestr}.nc
FV3_CPLR_FILE=${PDY}.${cyc}0000.coupler.res
FV3_AKBK_FILE=${PDY}.${cyc}0000.fv_core.res${neststr}.nc
FV3_CORE_FILE3=${ymdtm03}.${hhtm03}0000.fv_core.res${neststr}${tilestr}.nc
FV3_TRCR_FILE3=${ymdtm03}.${hhtm03}0000.fv_tracer.res${neststr}${tilestr}.nc
FV3_SFCD_FILE3=${ymdtm03}.${hhtm03}0000.sfc_data${neststr}${tilestr}.nc
FV3_SFCW_FILE3=${ymdtm03}.${hhtm03}0000.fv_srf_wnd.res${neststr}${tilestr}.nc
FV3_CPLR_FILE3=${ymdtm03}.${hhtm03}0000.coupler.res
FV3_AKBK_FILE3=${ymdtm03}.${hhtm03}0000.fv_core.res${neststr}.nc
FV3_CORE_FILE9=${ymdtp03}.${hhtp03}0000.fv_core.res${neststr}${tilestr}.nc
FV3_TRCR_FILE9=${ymdtp03}.${hhtp03}0000.fv_tracer.res${neststr}${tilestr}.nc
FV3_SFCD_FILE9=${ymdtp03}.${hhtp03}0000.sfc_data${neststr}${tilestr}.nc
FV3_SFCW_FILE9=${ymdtp03}.${hhtp03}0000.fv_srf_wnd.res${neststr}${tilestr}.nc
FV3_CPLR_FILE9=${ymdtp03}.${hhtp03}0000.coupler.res
FV3_AKBK_FILE9=${ymdtp03}.${hhtp03}0000.fv_core.res${neststr}.nc

if [ $GFSVER = PROD2021 ]; then
  export atmos="atmos/"
  export USE_GFS_NEMSIO=.false.
  export USE_GFS_NCIO=.true.
  GSUFFIX=${GSUFFIX:-.nc}
else
  echo "FATAL ERROR: Unknown or unsupported GFS version ${GFSVER}"
  exit 9
fi

# Diagnostic files options
export netcdf_diag=${netcdf_diag:-".true."}
export binary_diag=${binary_diag:-".false."}

if [ ! ${RUN_ANALYSIS} = "YES" ]; then
  echo "RUN_ANALYSIS: ${RUN_ANALYSIS} is not YES"
  echo "Do nothing. Exiting"
  exit
fi

export RESTARTanl=${RESTARTanl:-${WORKhafs}/intercom/RESTART_analysis}
export OBSIODA_DIR=${OBSIODA_DIR:-${WORKhafs}/intercom/obs_prep}
export DIAGanl=${DIAGanl:-${COMhafs}}
mkdir -p ${RESTARTanl}
mkdir -p ${DIAGanl}

# We should already be in $DATA, but extra cd to be sure.
cd $DATA
# Link DataFix files
${NCP} ${PARMjedi}/Fix/* .
sed -e "s|_NPX_|${npx_ens}|g" \
    -e "s|_NPY_|${npy_ens}|g" \
    -e "s|_NPZ_|${npz_ens}|g" \
    -e "s|_FV3_GRID_FILE_|${CASE}_mosaic.nc|g" \
    -e "s|_LAYOUTX_|${layoutx_jedi}|g" \
    -e "s|_LAYOUTY_|${layouty_jedi}|g" \
    -e "s|_DLON_|${target_lon}|g" \
    -e "s|_DLAT_|${target_lat}|g" \
    ${PARMjedi}/Fix/input_hafs.nml > input_hafs.nml

if [ ${nest_grids} -ge 2 ]; then
sed -e "s|_NPX_|${npx_nest}|g" \
    -e "s|_NPY_|${npy_nest}|g" \
    -e "s|_NPZ_|${npz}|g" \
    -e "s|_FV3_GRID_FILE_|${CASE}_mosaic_nest.nc|g" \
    -e "s|_LAYOUTX_|${layoutx_jedi}|g" \
    -e "s|_LAYOUTY_|${layouty_jedi}|g" \
    -e "s|_DLON_|${target_lon}|g" \
    -e "s|_DLAT_|${target_lat}|g" \
    ${PARMjedi}/Fix/input_hafs.nml > input_hafs_nest.nml
fi

# Copy the first guess or fgat files
if [ ${RUN_ATM_VI_FGAT} = "YES" ]; then
  RESTARTinp_fgat03=${WORKhafs}/intercom/RESTART_vi_fgat03
  RESTARTinp_fgat06=${WORKhafs}/intercom/RESTART_vi_fgat06
  RESTARTinp_fgat09=${WORKhafs}/intercom/RESTART_vi_fgat09
elif [ ${RUN_ATM_MERGE_FGAT} = "YES" ]; then
  RESTARTinp_fgat03=${WORKhafs}/intercom/RESTART_merge_fgat03
  RESTARTinp_fgat06=${WORKhafs}/intercom/RESTART_merge_fgat06
  RESTARTinp_fgat09=${WORKhafs}/intercom/RESTART_merge_fgat09
elif [ ${RUN_ATM_INIT_FGAT} = "YES" ]; then
  RESTARTinp_fgat03=${WORKhafs}/intercom/RESTART_init_fgat03
  RESTARTinp_fgat06=${WORKhafs}/intercom/RESTART_init_fgat06
  RESTARTinp_fgat09=${WORKhafs}/intercom/RESTART_init_fgat09
else
  if [ ${RUN_ATM_VI} = "YES" ]; then
    RESTARTinp_fgat06=${WORKhafs}/intercom/RESTART_vi
  elif [ ${RUN_ATM_MERGE} = "YES" ]; then
    RESTARTinp_fgat06=${WORKhafs}/intercom/RESTART_merge_fgat06
  elif [ ${RUN_ATM_INIT} = "YES" ]; then
    RESTARTinp_fgat06=${WORKhafs}/intercom/RESTART_init_fgat06
  else
    RESTARTinp_fgat06=${COMOLD}/${old_out_prefix}.RESTART
  fi
fi
RESTARTinp=${RESTARTinp_fgat06}

if [ ! -s ${RESTARTinp}/${FV3_CORE_FILE} ]; then
  echo "WARNING: First guess for DA/Analysis missing"
  echo "WARNING: Do nothing, Exiting"
  exit
fi
mkdir ${DATA}/bkg
cd ${DATA}/bkg
${NLN} ${RESTARTinp}/${FV3_CORE_FILE} .
${NLN} ${RESTARTinp}/${FV3_TRCR_FILE} .
${NLN} ${RESTARTinp}/${FV3_SFCD_FILE} .
${NLN} ${RESTARTinp}/${FV3_SFCW_FILE} .
${NLN} ${RESTARTinp}/${FV3_CPLR_FILE} .
${NLN} ${RESTARTinp}/${FV3_AKBK_FILE} .

if [ ${RUN_FGAT} = "YES" ]; then
  ${NLN} ${RESTARTinp_fgat03}/${FV3_CORE_FILE3} .
  ${NLN} ${RESTARTinp_fgat03}/${FV3_TRCR_FILE3} .
  ${NLN} ${RESTARTinp_fgat03}/${FV3_SFCD_FILE3} .
  ${NLN} ${RESTARTinp_fgat03}/${FV3_SFCW_FILE3} .
  ${NLN} ${RESTARTinp_fgat03}/${FV3_CPLR_FILE3} .
  ${NLN} ${RESTARTinp_fgat03}/${FV3_AKBK_FILE3} .
  ${NLN} ${RESTARTinp_fgat09}/${FV3_CORE_FILE9} .
  ${NLN} ${RESTARTinp_fgat09}/${FV3_TRCR_FILE9} .
  ${NLN} ${RESTARTinp_fgat09}/${FV3_SFCD_FILE9} .
  ${NLN} ${RESTARTinp_fgat09}/${FV3_SFCW_FILE9} .
  ${NLN} ${RESTARTinp_fgat09}/${FV3_CPLR_FILE9} .
  ${NLN} ${RESTARTinp_fgat09}/${FV3_AKBK_FILE9} .
  ${NLN} ${FV3_CPLR_FILE} ${PDY}.${cyc}0000.coupler${neststr}${tilestr}.res
  ${NLN} ${FV3_CPLR_FILE3} ${ymdtm03}.${hhtm03}0000.coupler${neststr}${tilestr}.res
  ${NLN} ${FV3_CPLR_FILE9} ${ymdtp03}.${hhtp03}0000.coupler${neststr}${tilestr}.res
fi
if [ ${nest_grids} -ge 2 ]; then
 ${NLN} ${RESTARTinp}/${FV3_CORE_ENS_FILE} .
 ${NLN} ${RESTARTinp}/${FV3_TRCR_ENS_FILE} .
 ${NLN} ${RESTARTinp}/${FV3_SFCD_ENS_FILE} .
 ${NLN} ${RESTARTinp}/${FV3_SFCW_ENS_FILE} .
 ${NLN} ${RESTARTinp}/${FV3_CPLR_ENS_FILE} .
 ${NLN} ${RESTARTinp}/${FV3_AKBK_ENS_FILE} .
fi

${NLN} ${RESTARTinp}/oro_data${nesttilestr}.nc .
${NLN} ${RESTARTinp}/atmos_static${nesttilestr}.nc .
${NLN} ${RESTARTinp}/grid_spec${nesttilestr}.nc .

if [ ${RUN_ENVAR} = "YES" ]; then
  mkdir ${DATA}/ensemble_data
  for mem in $(seq -f '%03g' 1 ${n_ens_fv3sar}); do
    mkdir ${DATA}/ensemble_data/mem${mem}
    if [ ${RUN_ENSDA} = "YES" ]; then
     RESTARTens=${COMOLD}/${old_out_prefix}.RESTART_ens/mem${mem}
    else
     if [ ${l4denvar:-.false.} = ".true." ]; then
      RESTARTens=${WORKhafs}/intercom/RESTART_init_fgat06_ens/mem${mem}
     else
      RESTARTens=${WORKhafs}/intercom/RESTART_init_ens/mem${mem}
     fi
    fi
    if [ ${RUN_ENSDA} != "YES" ]; then #Leave ATM_INIT options for GDAS Ensemble, need to lock with atm_init_fgat_ens
      for file in `ls ${RESTARTens}/*`; do
        ${NLN} ${file} ${DATA}/ensemble_data/mem${mem}/
      done
      if [ ${l4denvar:-.false.} = ".true." ] && [ ${RUN_ENSDA} = "NO" ]; then
        for file in `ls ${WORKhafs}/intercom/RESTART_init_fgat03_ens/mem${mem}/*`; do
          ${WLN} ${file} ${DATA}/ensemble_data/mem${mem}/
        done
        for file in `ls ${WORKhafs}/intercom/RESTART_init_fgat09_ens/mem${mem}/*`; do
          ${WLN} ${file} ${DATA}/ensemble_data/mem${mem}/
        done
      fi
    fi
  done
fi
# Interpolate the HAFS ensemble grid to control grid
# Assume the ensemble grid is always different from control and therefore needs interpolation
if [ ${RUN_ENSDA} = "YES" ]; then
  cd ${DATA}/ensemble_data
  for mem in $(seq -f '%03g' 1 ${n_ens_fv3sar}); do
    mkdir ${DATA}/ensemble_data/mem${mem}
    RESTARTens=${WORKhafs}/intercom/RESTART_init_merge_ens/mem${mem}/
    for file in `ls ${RESTARTens}/*`; do
      ${NLN} ${file} ${DATA}/ensemble_data/mem${mem}/
    done
  done
fi

# Stat files
RADSTAT=${RADSTAT:-${DIAGanl}/${out_prefix}.${RUN}.${gridstr}.analysis.radstat}
CNVSTAT=${CNVSTAT:-${DIAGanl}/${out_prefix}.${RUN}.${gridstr}.analysis.cnvstat}
DASOUT=${DASOUT:-${DIAGanl}/${out_prefix}.${RUN}.${gridstr}.analysis.dasout}
# Obs diag
RUN_SELECT=${RUN_SELECT:-"NO"}
USE_SELECT=${USE_SELECT:-"NO"}
USE_RADSTAT=${USE_RADSTAT:-"NO"}
SELECT_OBS=${SELECT_OBS:-${COMhafs}/${out_prefix}.${RUN}.${gridstr}.obsinput.tar}
GENDIAG=${GENDIAG:-"YES"}
DIAG_SUFFIX=${DIAG_SUFFIX:-""}
if [ $netcdf_diag = ".true." ]; then
   DIAG_SUFFIX="${DIAG_SUFFIX}.nc4"
fi
DIAG_COMPRESS=${DIAG_COMPRESS:-"YES"}
DIAG_TARBALL=${DIAG_TARBALL:-"YES"}
USE_MPISERIAL=${USE_MPISERIAL:-"YES"}
USE_CFP=${USE_CFP:-"NO"}
CFP_MP=${CFP_MP:-"NO"}
nm=""
if [ $CFP_MP = "YES" ]; then
  nm=0
fi

export DIAG_DIR=${DIAG_DIR:-./hofx}
REMOVE_DIAG_DIR=${REMOVE_DIAG_DIR:-"NO"}

# Set script / GSI control parameters
lrun_subdirs=${lrun_subdirs:-".true."}

#----------------------------------------------
# Link all the necessary fix files
#----------------------------------------------
# Link CRTM coefficient files based on entries in satinfo file
mkdir ${DATA}/crtm
cd ${DATA}/crtm

#if [ -e $HOMEhafs/sorc/hafs_jedi.fd/build/lib/python3.11 ]; then
# CRTM_TEMP="${PARMhafs}/../sorc/hafs_jedi.fd/bundle/test-data-release/crtm/3.0.0_skylab_6.0"
#else
CRTM_TEMP="${PARMhafs}/../sorc/hafs_jedi.fd/bundle/test-data-release/crtm/2.4.1_skylab_4.0"
#fi
for file in $(awk '{if($1!~"!"){print $1}}' ${DATA}/satinfo | sort | uniq); do
  ${NLN} ${CRTM_TEMP}/SpcCoeff/Little_Endian/${file}.SpcCoeff.bin ./
  ${NLN} ${CRTM_TEMP}/TauCoeff/ODPS/Little_Endian/${file}.TauCoeff.bin ./
done
${NLN} ${CRTM_TEMP}/EmisCoeff/MW_Water/Little_Endian/FASTEM6.MWwater.EmisCoeff.bin ./FASTEM6.MWwater.EmisCoeff.bin
for file in `ls ${CRTM_TEMP}/EmisCoeff/*/*/*/NPO*.bin`; do
  ${NLN} ${file} ./
done
for file in `ls ${CRTM_TEMP}/EmisCoeff/*/*/Nall*.bin`; do
  ${NLN} ${file} ./
done
for file in `ls ${CRTM_TEMP}/EmisCoeff/*/*/*/Nall*.bin`; do
  ${NLN} ${file} ./
done
${NLN} ${CRTM_TEMP}/AerosolCoeff/Little_Endian/AerosolCoeff.bin ./AerosolCoeff.bin
${NLN} ${CRTM_TEMP}/CloudCoeff/Little_Endian/CloudCoeff.bin ./CloudCoeff.bin


# Link GFS/GDAS input and observation files
radtypes="atms_npp amsua_n19 atms_n20 iasi_metop-b ssmis_f17 abi_g16 abi_g18 amsua_metop-b amsua_n18"
convtypes="adpsfc_specificHumidity_181 adpsfc_stationPressure_181 adpsfc_stationPressure_187 adpsfc_winds_281 adpsfc_winds_287 adpupa_airTemperature_120 adpupa_winds_220 adpupa_specificHumidity_120 aircft_winds_230 aircft_winds_231 aircft_winds_234 aircft_winds_235 aircft_airTemperature_130 aircft_airTemperature_131 satwnd_abi_goes-16 satwnd_abi_goes-18 satwhr_abi_goes-16 satwhr_abi_goes-18 satwhr_abi_goes-19 tldplr_rw_993 aircar_airTemperature_133 aircar_specificHumidity_133 aircar_winds_233 hdob_airTemperature_136 hdob_specificHumidity_136 hdob_winds_236 drpsnd_airTemperature_137 drpsnd_specificHumidity_137 drpsnd_winds_237"
convfiles="adpsfc adpupa aircft satwnd_abi_goes-16 satwnd_abi_goes-18 satwhr_abi_goes-16 satwhr_abi_goes-18 satwhr_abi_goes-19 tldplr aircar hdob drpsnd"
IFS=' ' read -ra convtypes_array <<< "$convtypes"
mkdir ${DATA}/obs
cd ${DATA}/obs
valid_convfiles=()
valid_convtypes=()
for file in ${convfiles}; do
  ncfile="${OBSIODA_DIR}/hafs.t${cyc}z.${file}.nc"
  if [[ -f "$ncfile" ]]; then
    ${NLN} "$ncfile" .
    valid_convfiles+=("$file")

    # Keep all convtypes entries that contain this $file
    for type in "${convtypes_array[@]}"; do
      if [[ "$type" == *"$file"* ]]; then
        valid_convtypes+=("$type")
      fi
    done
  else
    echo "WARNING: Missing file $ncfile, skipping $file"
  fi
done

# Update convtypes to only valid ones
convtypes="${valid_convtypes[*]}"
convfiles="${valid_convfiles[*]}"

# === Now handle radtypes ===
IFS=' ' read -ra radtypes_array <<< "$radtypes"

valid_radtypes=()
for file in ${radtypes_array[@]}; do
  ncfile="${OBSIODA_DIR}/hafs.t${cyc}z.${file}.nc"
  if [[ -f "$ncfile" ]]; then
    ${NLN} "$ncfile" .
    tlapse_file="${OBSIODA_DIR}/${file}.tlapse.txt"
    [[ -f "$tlapse_file" ]] && ${NLN} "$tlapse_file" .
    valid_radtypes+=("$file")
  else
    echo "WARNING: Missing file $ncfile, skipping $file"
  fi
done

radtypes="${valid_radtypes[*]}"

# Final combined types
obstypes="$convtypes $radtypes"
bctypes="${radtypes}"

for file in ${bctypes}; do
  ${NLN} ${OBSIODA_DIR}/satbias_${file}_t${cyc}z.nc .
  ${NLN} ${OBSIODA_DIR}/satbias_${file}_t${cyc}z_cov.nc .
done
mkdir ${DATA}/bc #Create bc for output

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
### XL need to think about tile #, is 7 & 8 always the default value?
#??XL: Do it here or in atm_prep

#----------------------------------------------
# Prepare NICAS LOCALIZATION
# ?? Should we do it here? XL
#----------------------------------------------
cd ${DATA}
export basic_yaml_dir=${PARMjedi}/yaml_templates/basic_config
export obs_yaml_dir=${PARMjedi}/yaml_templates/obtype_config
export jcb_yaml_dir=${PARMjedi}/yaml_templates/jcb-hdas/test/client_integration
mkdir ${DATA}/bump
if [ ${nest_grids} -ge 2 ]; then
  INPUT_HAFS_NML=input_hafs_nest.nml
else
  INPUT_HAFS_NML=input_hafs.nml
fi
INPUT_HAFS_ENS_NML=input_hafs_nest.nml
sed -e "s|_FV3_CORE_ENS_FILE_|${FV3_CORE_FILE}|g" \
    -e "s|_FV3_TRCR_ENS_FILE_|${FV3_TRCR_FILE}|g" \
    -e "s|_FV3_SFCD_ENS_FILE_|${FV3_SFCD_FILE}|g" \
    -e "s|_FV3_SFCW_ENS_FILE_|${FV3_SFCW_FILE}|g" \
    -e "s|_FV3_CPLR_ENS_FILE_|${FV3_CPLR_FILE}|g" \
    -e "s|_FV3_AKBK_ENS_FILE_|${FV3_AKBK_FILE}|g" \
    -e "s|_INPUT_HAFS_ENS_NML_|${INPUT_HAFS_NML}|g" \
    -e "s|_ANALYSISDATE_|'${yr}-${mn}-${dy}T${hh}:00:00Z'|g" \
    -e "s|_LOC_H_|${loc_h}|g" \
    -e "s|_LOC_V_|${loc_v}|g" \
    ${basic_yaml_dir}/bump_nicas.yaml > bump_nicas.yaml
${NCP} ${EXEChafs}/hafs_nicas.x .
if [ ${l4densvar:-.true.} = ".true." ]; then
#  ${APRUNCD3} ${EXEChafs}/hafs_nicas.x bump_nicas.yaml nicas.log
  ${APRUNC} ${EXEChafs}/hafs_nicas.x bump_nicas.yaml nicas.log #XL turn off parallel subwindow until the thinning issue is fixed
else
  ${APRUNC} ${EXEChafs}/hafs_nicas.x bump_nicas.yaml nicas.log
fi
mv hdas-atmosphere-templates.yaml hdas-atmosphere-templates_bump.yaml
rm nicas.log.*
#----------------------------------------------
# Prepare yaml
#----------------------------------------------
cd ${DATA}
mkdir ${DATA}/hofx #Create hofx for diagfile output
if [ ${l4denvar:-.true.} = ".true." ]; then
#TOTAL_TASKS_tmp=${TOTAL_TASKSD3}
TOTAL_TASKS_tmp=${TOTAL_TASKS}
sed -e "s|_TARGET_YAML_|jedi.yaml|g" ${jcb_yaml_dir}/run.py > run_jedi.py
sed -e "s|_ALGORITHM_|fv3jedi_4denvar|g" \
    -e "s|_INITIALDATE_|${yrtm03}-${mntm03}-${dytm03}T${hhtm03}:00:00Z|g" \
    -e "s|_ANALYSISDATE_|${yr}-${mn}-${dy}T${hh}:00:00Z|g" \
    -e "s|_ENDDATE_|${yrtp03}-${mntp03}-${dytp03}T${hhtp03}:00:00Z|g" \
    -e "s|_HH_|${cyc}|g" \
    -e "s|_ENS_SIZE_|${ENS_SIZE}|g" \
    -e "s|_LAYOUTX_|${layoutx_jedi}|g" \
    -e "s|_LAYOUTY_|${layouty_jedi}|g" \
    -e "s|_LOC_H_|${loc_h}|g" \
    -e "s|_LOC_V_|${loc_v}|g" \
    -e "s|_TOTAL_TASKS_|${TOTAL_TASKS_tmp}|g" \
    -e "s|_MIN_LAT_|${MIN_LAT}|g" \
    -e "s|_MAX_LAT_|${MAX_LAT}|g" \
    -e "s|_MIN_LON_|${MIN_LON}|g" \
    -e "s|_MAX_LON_|${MAX_LON}|g" \
    -e "s|_NPX_|${npx_ens}|g" \
    -e "s|_NPY_|${npy_ens}|g" \
    -e "s|_NPZ_|${npz_ens}|g" \
    -e "s|_FV3_AKBK_FILE_|${FV3_AKBK_FILE}|g" \
    -e "s|_YYMODD6_|${yr}${mn}${dy}|g" \
    -e "s|_HH6_|${hh}|g" \
    -e "s|_YYMODD3_|${yrtm03}${mntm03}${dytm03}|g" \
    -e "s|_HH3_|${hhtm03}|g" \
    -e "s|_YYMODD9_|${yrtp03}${mntp03}${dytp03}|g" \
    -e "s|_HH9_|${hhtp03}|g" \
    ${jcb_yaml_dir}/hdas-atmosphere-templates.yaml > hdas-atmosphere-templates.yaml
for obstype in ${obstypes}; do
  echo "- ${obstype}" >> hdas-atmosphere-templates.yaml
done
for obstype in ${obstypes}; do
  echo "iuse_${obstype}: accept" >> hdas-atmosphere-templates.yaml
done
python run_jedi.py
elif [ ${RUN_FGAT} = NO ]; then
sed -e "s|_FV3_CORE_ENS_FILE_|${FV3_CORE_FILE}|g" \
    -e "s|_FV3_TRCR_ENS_FILE_|${FV3_TRCR_FILE}|g" \
    -e "s|_FV3_SFCD_ENS_FILE_|${FV3_SFCD_FILE}|g" \
    -e "s|_FV3_SFCW_ENS_FILE_|${FV3_SFCW_FILE}|g" \
    -e "s|_FV3_CPLR_ENS_FILE_|${FV3_CPLR_FILE}|g" \
    -e "s|_FV3_AKBK_ENS_FILE_|${FV3_AKBK_FILE}|g" \
    -e "s|_INPUT_HAFS_ENS_NML_|${INPUT_HAFS_NML}|g" \
    -e "s|_FV3_CORE_FILE_|${FV3_CORE_FILE}|g" \
    -e "s|_FV3_TRCR_FILE_|${FV3_TRCR_FILE}|g" \
    -e "s|_FV3_SFCD_FILE_|${FV3_SFCD_FILE}|g" \
    -e "s|_FV3_SFCW_FILE_|${FV3_SFCW_FILE}|g" \
    -e "s|_FV3_CPLR_FILE_|${FV3_CPLR_FILE}|g" \
    -e "s|_FV3_AKBK_FILE_|${FV3_AKBK_FILE}|g" \
    -e "s|_INPUT_HAFS_NML_|${INPUT_HAFS_NML}|g" \
    -e "s|_INITIALDATE_|${yrtm03}-${mntm03}-${dytm03}T${hhtm03}:00:00Z|g" \
    -e "s|_ANALYSISDATE_|${yr}-${mn}-${dy}T${hh}:00:00Z|g" \
    -e "s|#HH#|t${cyc}z|g" \
    -e "s|_ENS_SIZE_|${ENS_SIZE}|g" \
    -e "s|_LAYOUTX_|${layoutx_jedi}|g" \
    -e "s|_LAYOUTY_|${layouty_jedi}|g" \
    ${basic_yaml_dir}/fv3jedi_3denvar.yaml > ./jedi.yaml
else
sed -e "s|_FV3_CORE_ENS_FILE_|${FV3_CORE_FILE}|g" \
    -e "s|_FV3_TRCR_ENS_FILE_|${FV3_TRCR_FILE}|g" \
    -e "s|_FV3_SFCD_ENS_FILE_|${FV3_SFCD_FILE}|g" \
    -e "s|_FV3_SFCW_ENS_FILE_|${FV3_SFCW_FILE}|g" \
    -e "s|_FV3_CPLR_ENS_FILE_|${FV3_CPLR_FILE}|g" \
    -e "s|_FV3_AKBK_ENS_FILE_|${FV3_AKBK_FILE}|g" \
    -e "s|_INPUT_HAFS_ENS_NML_|${INPUT_HAFS_NML}|g" \
    -e "s|_INPUT_HAFS_NML_|${INPUT_HAFS_NML}|g" \
    -e "s|_FV3_CORE_FILE3_|${FV3_CORE_FILE3}|g" \
    -e "s|_FV3_TRCR_FILE3_|${FV3_TRCR_FILE3}|g" \
    -e "s|_FV3_SFCD_FILE3_|${FV3_SFCD_FILE3}|g" \
    -e "s|_FV3_SFCW_FILE3_|${FV3_SFCW_FILE3}|g" \
    -e "s|_FV3_CPLR_FILE3_|${FV3_CPLR_FILE3}|g" \
    -e "s|_FV3_AKBK_FILE_|${FV3_AKBK_FILE}|g" \
    -e "s|_INITIALDATE_|${yrtm03}-${mntm03}-${dytm03}T${hhtm03}:00:00Z|g" \
    -e "s|_ANALYSISDATE_|${yr}-${mn}-${dy}T${hh}:00:00Z|g" \
    -e "s|#HH#|t${cyc}z|g" \
    -e "s|_NEST_TILE_STR_|${neststr}${tilestr}|g" \
    -e "s|_ENS_SIZE_|${ENS_SIZE}|g" \
    -e "s|_LAYOUTX_|${layoutx_jedi}|g" \
    -e "s|_LAYOUTY_|${layouty_jedi}|g" \
    ${basic_yaml_dir}/fv3jedi_3dfgat.yaml > ./jedi.yaml
fi

#-------------------------------------------------------------------
# Link the executable and run the analysis
#-------------------------------------------------------------------
ANALYSISEXEC=${ANALYSISEXEC:-${EXEChafs}/hafs_jedi.x}
${NCP} -p ${ANALYSISEXEC} ./hafs_jedi.x
${SOURCE_PREP_STEP}
${APRUNC} ${ANALYSISEXEC} jedi.yaml jedi.out
export err=$?; err_chk
rm jedi.out.*
cat ./jedi.out > ${DASOUT}

created=0
for file in ${radtypes}; do
  for file0 in hofx/diag_${file}_t${cyc}z*.nc; do
    if [ ${created} -eq 0 ]; then
      tar cvf "$RADSTAT" "$file0"   # create once
      created=1
    else
      tar rvf "$RADSTAT" "$file0"   # append thereafter
    fi
  done
done
created=0
for file in ${convtypes}; do
  for file0 in hofx/diag_${file}_t${cyc}z*.nc; do
    if [ ${created} -eq 0 ]; then
      tar cvf "$CNVSTAT" "$file0"   # create once
      created=1
    else
      tar rvf "$CNVSTAT" "$file0"   # append thereafter
    fi
  done
done

#Store the output to intercom
if [ ${l4denvar:-.false.} = ".true." ]; then
 ${NCP} ${DATA}/${PDY}.${cyc}0000.fv_tracer.res.nc ${RESTARTanl}/${FV3_TRCR_FILE}
 ${NCP} ${DATA}/${PDY}.${cyc}0000.sfc_data.nc ${RESTARTanl}/${FV3_SFCD_FILE}
 ${NCP} ${DATA}/${PDY}.${cyc}0000.fv_srf_wnd.res.nc ${RESTARTanl}/${FV3_SFCW_FILE}
 ${NCP} ${DATA}/${PDY}.${cyc}0000.coupler.res ${RESTARTanl}/${FV3_CPLR_FILE}
else
 ${NCP} ${DATA}/analysis.coupler.res        ${RESTARTanl}/${FV3_CPLR_FILE}
 ${NCP} ${DATA}/analysis.sfc_data.nc        ${RESTARTanl}/${FV3_SFCD_FILE}
 ${NCP} ${DATA}/analysis.fv_srf_wnd.res.nc  ${RESTARTanl}/${FV3_SFCW_FILE}
 ${NCP} ${DATA}/analysis.fv_tracer.res.nc   ${RESTARTanl}/${FV3_TRCR_FILE}
fi
${NCP} ${RESTARTinp}/oro_data${nesttilestr}.nc ${RESTARTanl}/
${NCP} ${RESTARTinp}/atmos_static${nesttilestr}.nc ${RESTARTanl}/
${NCP} ${RESTARTinp}/grid_spec${nesttilestr}.nc ${RESTARTanl}/
${NCP} ${RESTARTinp}/${FV3_AKBK_FILE} ${RESTARTanl}/

# pass over phy_data as well
${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.phy_data${nesttilestr}.nc ${RESTARTanl}/${PDY}.${cyc}0000.phy_data${nesttilestr}.nc

if [[ ! -z "$neststr" ]] ; then
 if [ -e ${RESTARTinp}/${PDY}.${cyc}0000.fv_BC_ne.res${neststr}.nc ]; then
   ${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_BC_ne.res${neststr}.nc ${RESTARTanl}/${PDY}.${cyc}0000.fv_BC_ne.res${neststr}.nc
   ${NCP} ${RESTARTinp}/${PDY}.${cyc}0000.fv_BC_sw.res${neststr}.nc ${RESTARTanl}/${PDY}.${cyc}0000.fv_BC_sw.res${neststr}.nc
 fi
fi

## Update u/v based on ua/va since JEDI analysis is on ua/va, but FV3 initializes based on u/v
if [ ${l4denvar:-.false.} = ".true." ]; then
 IN_FILE=${DATA}/${PDY}.${cyc}0000.fv_core.res.nc
else
 IN_FILE=${DATA}/analysis.fv_core.res.nc
fi
DATOOL=${DATOOL:-${EXEChafs}/hafs_tools_datool.x}
${APRUNS} ${DATOOL} ua_update_u \
   --in_grid=${RESTARTanl}/grid_spec${nesttilestr}.nc \
   --in_file=${IN_FILE} \
   --out_file=${RESTARTanl}/${FV3_CORE_FILE}

ncks -v sgs_tke ${DATA}/bkg/${FV3_TRCR_FILE} -A ${RESTARTanl}/${FV3_TRCR_FILE} #add sgs_tke from the background file

# Pass over the grid_mspec files for moving nest
if [[ "${is_moving_nest:-".false."}" = *".true."* ]] || [[ "${is_moving_nest:-".false."}" = *".T."* ]] ; then
  if [[ -z "$neststr" ]] && [[ $tilestr = ".tile1" ]]; then
    # "grid_mspec_${yr}_${mn}_${dy}_${cyc}.nc" for domain 02
    ${NCP} -p ${RESTARTinp}/grid_mspec_${yr}_${mn}_${dy}_${cyc}.nc ${RESTARTanl}/
  else
    # "grid_mspec.nest02_${yr}_${mn}_${dy}_${cyc}.tile2.nc" for domain 02
    ${NCP} -p ${RESTARTinp}/grid_mspec${neststr}_${yr}_${mn}_${dy}_${cyc}${tilestr}.nc ${RESTARTanl}/
  fi
fi
