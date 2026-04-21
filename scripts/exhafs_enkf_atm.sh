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
nest_grids=${nest_grids_ens:-1}
npx_nest=$( echo ${npx} | cut -d , -f 2 )
npy_nest=$( echo ${npy} | cut -d , -f 2 )
if [ ${nest_grids} -eq 2 ]; then
  cenlat=$(echo "$output_grid_cen_lat" | cut -d',' -f2)
  cenlon=$(echo "$output_grid_cen_lon" | cut -d',' -f2)
else
  cenlat=$(echo "$output_grid_cen_lat" | cut -d',' -f1)
  cenlon=$(echo "$output_grid_cen_lon" | cut -d',' -f1)
fi
MIN_LON=$(echo "$cenlon - ${dlon_cutoff:-180}" | bc) #Domain cut for DA efficiency, need to consider domain flexibility later
MAX_LON=$(echo "$cenlon + ${dlon_cutoff:-180}" | bc)
MIN_LAT=$(echo "$cenlat - ${dlat_cutoff:-90}" | bc)
MAX_LAT=$(echo "$cenlat + ${dlat_cutoff:-90}" | bc)
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
export ENS_SIZE=${ENS_SIZE:-40}
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
export n_ens_fv3sar=${n_ens_fv3sar:-${ENS_SIZE:-40}}
export l4densvar=${l4densvar:-.false.}
export nhr_obsbin=${nhr_obsbin:--1}

export ANALYSIS_D01=${ANALYSIS_D01:-NO}
export ANALYSIS_D02=${ANALYSIS_D02:-NO}
export gridstr=${gridstr:-$(echo ${out_gridnames} | cut -d, -f 1)}
export neststr=${neststr:-""} # ".nest02" for domain 02
export tilestr=${tilestr:-".tile1"} # ".tile2" for domain 02
export nesttilestr=${nesttilestr:-""} # ".nest02.tile2" for domain 02

export ANALYSISEXEC=${ANALYSISEXEC:-${EXEChafs}/hafs_jedi_enkf.x}
export CATEXEC=${CATEXEC:-ncdiag_cat_serial.x}

FV3_CORE_ENS_FILE=${PDY}.${cyc}0000.fv_core.res.tile1.nc
FV3_TRCR_ENS_FILE=${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
FV3_SFCD_ENS_FILE=${PDY}.${cyc}0000.sfc_data.nc
FV3_SFCW_ENS_FILE=${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc
FV3_CPLR_ENS_FILE=${PDY}.${cyc}0000.coupler.res
FV3_AKBK_ENS_FILE=${PDY}.${cyc}0000.fv_core.res.nc

if [ ${nest_grids} -ge 2 ]; then
  INPUT_HAFS_NML=input_hafs_bkg.nml
  if [ ${RUN_ENSDA} = "YES" ]; then
    INPUT_HAFS_ENS_NML=input_hafs_ens.nml
  else
    INPUT_HAFS_ENS_NML=input_hafs_bkg.nml #If No HAFS Ens, GDAS Ens is interpolated to bkg.
  fi
else
  INPUT_HAFS_NML=input_hafs_bkg.nml
  INPUT_HAFS_ENS_NML=input_hafs_ens.nml
fi

if [ ! ${RUN_ENKF} = "YES" ]; then
  echo "RUN_ENKF: ${RUN_ENKF} is not YES"
  echo "Do nothing. Exiting"
  exit
fi

export RESTARTinp=${WORKhafs}/intercom/RESTART_analysis_ens/ensmean
export RESTARTens_anl=${WORKhafs}/intercom/RESTART_analysis_ens
export OBSIODA_DIR=${OBSIODA_DIR:-${WORKhafs}/intercom/obs_prep}
export DIAGanl=${DIAGanl:-${COMhafs}}
if [ ! -d ${COMOLD}/${old_out_prefix}.RESTART_ens ]; then
  echo "WARNING: First guess for DA/Analysis missing"
  echo "WARNING: Do nothing, Exiting"
  exit
fi
mkdir ${DATA}/bkg
mkdir -p ${RESTARTanl}
mkdir -p ${DIAGanl}

# We should already be in $DATA, but extra cd to be sure.
cd $DATA
# Link DataFix files
${NCP} ${PARMjedi}/Fix/* .
sed -e "s|_NPX_|${npx_ens}|g" \
    -e "s|_NPY_|${npy_ens}|g" \
    -e "s|_NPZ_|${npz}|g" \
    -e "s|_FV3_GRID_FILE_|${CASE}_mosaic_ens.nc|g" \
    -e "s|_LAYOUTX_|${layoutx_enkf}|g" \
    -e "s|_LAYOUTY_|${layouty_enkf}|g" \
    -e "s|_DLON_|${target_lon}|g" \
    -e "s|_DLAT_|${target_lat}|g" \
    ${PARMjedi}/Fix/input_hafs.nml > input_hafs_ens.nml

cd ${DATA}/bkg
${NLN} ${RESTARTinp}/${FV3_CORE_ENS_FILE} .
${NLN} ${RESTARTinp}/${FV3_TRCR_ENS_FILE} .
${NLN} ${RESTARTinp}/${FV3_SFCD_ENS_FILE} .
${NLN} ${RESTARTinp}/${FV3_SFCW_ENS_FILE} .
${NLN} ${RESTARTinp}/${FV3_CPLR_ENS_FILE} .
${NLN} ${RESTARTinp}/${FV3_AKBK_ENS_FILE} .

${NLN} ${RESTARTinp}/oro_data${nesttilestr}.nc .
${NLN} ${RESTARTinp}/atmos_static${nesttilestr}.nc .
${NLN} ${RESTARTinp}/grid_spec${nesttilestr}.nc .

if [ ${RUN_ENVAR} = "YES" ]; then
  mkdir ${DATA}/ensemble_data
  for mem in $(seq -f '%03g' 1 ${n_ens_fv3sar}); do
    mkdir ${DATA}/ensemble_data/mem${mem}
    if [ ${RUN_ENSDA} = "YES" ]; then
      RESTARTens=${COMOLD}/${old_out_prefix}.RESTART_ens/mem${mem}
      for file in `ls ${RESTARTens}/*`; do
        ${NLN} ${file} ${DATA}/ensemble_data/mem${mem}/
      done
      mkdir -p ${DATA}/output/mem${mem}
    else
     echo 'WARNING: RUN_ENSDA must be YES for EnKF component'
     exit
    fi
  done
fi

# Stat files
RADSTAT=${RADSTAT:-${DIAGanl}/${out_prefix}.${RUN}.${gridstr}.EnKFana.radstat}
CNVSTAT=${CNVSTAT:-${DIAGanl}/${out_prefix}.${RUN}.${gridstr}.EnKFana.cnvstat}
DASOUT=${DASOUT:-${DIAGanl}/${out_prefix}.${RUN}.${gridstr}.EnKFana.dasout}
# Obs diag
export netcdf_diag=${netcdf_diag:-".true."}
export binary_diag=${binary_diag:-".false."}
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

CRTM_TEMP="${PARMhafs}/../sorc/hafs_jedi.fd/bundle/test-data-release/crtm/2.4.1_skylab_4.0"
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
radtypes="radiance_atms_npp radiance_amsua_n19 radiance_atms_n20 radiance_iasi_metop-b radiance_ssmis_f17 radiance_amsua_metop-b radiance_amsua_n18 radiance_abi_g16 radiance_abi_g18 radiance_cris-fsr_n20 radiance_cris-fsr_n21 radiance_cris-fsr_npp"
convtypes="conventional_air_aircar_133q conventional_air_aircar_133t conventional_air_aircar_233 conventional_air_drpsnd_137q conventional_air_drpsnd_137t conventional_air_drpsnd_237 conventional_air_amdar_130t conventional_air_amdar_131t conventional_air_amdar_230 conventional_air_amdar_231 conventional_air_amdar_234 conventional_air_amdar_235 conventional_air_hdob_136q conventional_air_hdob_136t conventional_air_hdob_236 conventional_air_raob_120q conventional_air_raob_220 conventional_air_raob_120t conventional_land_synop_181ps conventional_land_synop_187ps conventional_land_synop_181q conventional_land_synop_181t conventional_land_synop_281 conventional_land_synop_287 conventional_radar_tdr_992 conventional_radar_tdr_993 conventional_radar_vadwnd conventional_sea_ship_180ps conventional_sea_ship_180q conventional_sea_ship_180t conventional_sea_ship_280 retrieval_amv_abi_goes-16 retrieval_amv_abi_goes-18 retrieval_hiamv_abi_goes-16 retrieval_hiamv_abi_goes-18 retrieval_hiamv_abi_goes-19 conventional_osw_ascat conventional_air_raob_120ps"
convfiles="conventional_air_aircar conventional_air_drpsnd conventional_air_amdar conventional_air_hdob conventional_air_raob conventional_land_synop conventional_radar_tdr conventional_radar_vadwnd conventional_sea_ship retrieval_amv_abi_goes-16 retrieval_amv_abi_goes-18 retrieval_hiamv_abi_goes-16 retrieval_hiamv_abi_goes-18 retrieval_hiamv_abi_goes-19"
mkdir -p "${DATA}/obs"
cd "${DATA}/obs" || exit 1
IFS=' ' read -ra convtypes_array <<< "$convtypes"

valid_convtypes=()
for file in ${convtypes_array[@]}; do
  ncfile="${WORKhafs}/intercom/RESTART_analysis_ens/hofx/diag_${file}_t${cyc}z.nc"
  linkfile="hafs.t${cyc}z.${file}.nc"
  if [[ -f "$ncfile" ]]; then
    nloc=$(ncdump -h "$ncfile" | sed -n '/dimensions:/,/variables:/p' | grep -E "Location|nobs" | head -1 | grep -oE '[0-9]+' | tail -1)
    if [[ -z "$nloc" || "$nloc" -eq 0 ]]; then
        echo "Skipping empty or invalid file: $ncfile"
        continue
    fi
    ${NLN} "$ncfile" "$linkfile"
    valid_convtypes+=("$file")
  else
    echo "WARNING: Missing file $ncfile, skipping $file"
  fi
done

convtypes="${valid_convtypes[*]}"

# === Now handle radtypes ===
IFS=' ' read -ra radtypes_array <<< "$radtypes"

valid_radtypes=()
for file in ${radtypes_array[@]}; do
  ncfile="${WORKhafs}/intercom/RESTART_analysis_ens/hofx/diag_${file}_t${cyc}z.nc"
  linkfile="hafs.t${cyc}z.${file}.nc"
  if [[ -f "$ncfile" ]]; then
    nloc=$(ncdump -h "$ncfile" | sed -n '/dimensions:/,/variables:/p' | grep -E "Location|nobs" | head -1 | grep -oE '[0-9]+' | tail -1)
    if [[ -z "$nloc" || "$nloc" -eq 0 ]]; then
        echo "Skipping empty or invalid file: $ncfile"
        continue
    fi
    ${NLN} "$ncfile" "$linkfile"
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
if [ ${nest_grids} -ge 2 ]; then
  ${NCP} ${WORKhafs}/intercom/atm_prep/grid/${CASE}/${CASE}_grid.tile8.halo3.nc .
  ${NCP} ${EXEChafs}/hafs_utils_make_solo_mosaic.x .
  ${EXEChafs}/hafs_utils_make_solo_mosaic.x --num_tiles 1 --dir ${WORKhafs}/intercom/atm_prep/grid/${CASE} --mosaic ${CASE}_mosaic_bkg --tile_file ${CASE}_grid.tile8.halo3.nc
else
  ${NCP} ${WORKhafs}/intercom/atm_prep/grid/${CASE}/${CASE}_grid.tile7.halo3.nc .
  ${NCP} ${EXEChafs}/hafs_utils_make_solo_mosaic.x .
  ${EXEChafs}/hafs_utils_make_solo_mosaic.x --num_tiles 1 --dir ${WORKhafs}/intercom/atm_prep/grid/${CASE} --mosaic ${CASE}_mosaic_bkg --tile_file ${CASE}_grid.tile7.halo3.nc
fi
if [ -d ${WORKhafs}/intercom/atm_prep_ens/grid_ens ]; then
  ${NCP} ${WORKhafs}/intercom/atm_prep_ens/grid_ens/${CASE}/${CASE}_grid.tile7.halo3.nc ${CASE}_grid_ens.tile7.halo3.nc
  ${NCP} ${WORKhafs}/intercom/atm_prep_ens/grid_ens/${CASE}/${CASE}_grid.tile7.halo3.nc ${WORKhafs}/intercom/atm_prep_ens/grid_ens/${CASE}/${CASE}_grid_ens.tile7.halo3.nc
  ${EXEChafs}/hafs_utils_make_solo_mosaic.x --num_tiles 1 --dir ${WORKhafs}/intercom/atm_prep_ens/grid_ens/${CASE} --mosaic ${CASE}_mosaic_ens --tile_file ${CASE}_grid_ens.tile7.halo3.nc
else
  ${NCP} ${CASE}_mosaic_bkg.nc ${CASE}_mosaic_ens.nc
fi
#----------------------------------------------
# Prepare yaml
#----------------------------------------------
export basic_yaml_dir=${PARMjedi}/yaml_templates/basic_config
export obs_yaml_dir=${PARMjedi}/yaml_templates/obtype_config
export jcb_yaml_dir=${PARMjedi}/jcb-hdas/test/client_integration
cd ${DATA}
mkdir ${DATA}/hofx #Create hofx for diagfile output
TOTAL_TASKS_tmp=${TOTAL_TASKS}
sed -e "s|_TARGET_YAML_|jedi.yaml|g" ${jcb_yaml_dir}/run.py > run_jedi.py
sed -e "s|_ALGORITHM_|enkf3d|g" \
    ${jcb_yaml_dir}/hdas-atmosphere-templates.yaml > hdas-atmosphere-templates.yaml.tmp
sed -e "s|_INITIALDATE_|${yrtm03}-${mntm03}-${dytm03}T${hhtm03}:00:00Z|g" \
    -e "s|_ANALYSISDATE_|${yr}-${mn}-${dy}T${hh}:00:00Z|g" \
    -e "s|_ENDDATE_|${yrtp03}-${mntp03}-${dytp03}T${hhtp03}:00:00Z|g" \
    -e "s|_HH_|${cyc}|g" \
    -e "s|_ENS_SIZE_|${ENS_SIZE}|g" \
    -e "s|_LAYOUTX_|${layoutx_enkf}|g" \
    -e "s|_LAYOUTY_|${layouty_enkf}|g" \
    -e "s|_LOC_H_|${loc_h}|g" \
    -e "s|_LOC_V_|${loc_v}|g" \
    -e "s|_POOL_SIZE_|${TOTAL_TASKS_tmp}|g" \
    -e "s|_MIN_LAT_|${MIN_LAT}|g" \
    -e "s|_MAX_LAT_|${MAX_LAT}|g" \
    -e "s|_MIN_LON_|${MIN_LON}|g" \
    -e "s|_MAX_LON_|${MAX_LON}|g" \
    -e "s|_NPX_|${npx_ens}|g" \
    -e "s|_NPY_|${npy_ens}|g" \
    -e "s|_NPZ_|${npz}|g" \
    -e "s|_FV3_AKBK_FILE_|${FV3_AKBK_ENS_FILE}|g" \
    -e "s|_YYMODD6_|${yr}${mn}${dy}|g" \
    -e "s|_HH6_|${hh}|g" \
    -e "s|_YYMODD3_|${yrtm03}${mntm03}${dytm03}|g" \
    -e "s|_HH3_|${hhtm03}|g" \
    -e "s|_YYMODD9_|${yrtp03}${mntp03}${dytp03}|g" \
    -e "s|_HH9_|${hhtp03}|g" \
    -e "s|_NESTTILESTRNC_|${neststr}${tilestr}.nc|g" \
    -e "s|_NESTTILESTRSFC_|${nesttilestr}.nc|g" \
    -e "s|_INPUT_HAFS_ENS_NML_|${INPUT_HAFS_ENS_NML}|g" \
    -e "s|_INPUT_HAFS_NML_|${INPUT_HAFS_NML}|g" \
    -e "s|_DISTRIBUTION_|Halo|g" \
    ./hdas-atmosphere-templates.yaml.tmp > hdas-atmosphere-templates.yaml
for obstype in ${obstypes}; do
  echo "- ${obstype}" >> hdas-atmosphere-templates.yaml
done
for obstype in ${obstypes}; do
  echo "iuse_${obstype}: accept" >> hdas-atmosphere-templates.yaml
done
python run_jedi.py

#-------------------------------------------------------------------
# Link the executable and run the analysis
#-------------------------------------------------------------------
${NCP} -p ${ANALYSISEXEC} ./hafs_jedi_enkf.x
${SOURCE_PREP_STEP}
${APRUNC} ${ANALYSISEXEC} jedi.yaml jedi.out
export err=$?; err_chk
rm jedi.out.*
cat ./jedi.out > ${DASOUT}

for imem in $(seq 2 $nens); do
  memout="mem"$(printf %03i $imem)
  mkdir -p ${RESTARTens_anl}/${memout}
  ${NCP} $DATA/output/${memout}/${PDY}.${cyc}0000.coupler.res ${RESTARTens_anl}/${memout}/${FV3_CPLR_ENS_FILE}
  ${NCP} $DATA/output/${memout}/${PDY}.${cyc}0000.fv_core.res.nc ${RESTARTens_anl}/${memout}/${FV3_CORE_ENS_FILE}
  ${NCP} $DATA/output/${memout}/${PDY}.${cyc}0000.fv_tracer.res.nc ${RESTARTens_anl}/${memout}/${FV3_TRCR_ENS_FILE}
  ${NCP} $DATA/output/${memout}/${PDY}.${cyc}0000.fv_srf_wnd.res.nc ${RESTARTens_anl}/${memout}/${FV3_SFCW_ENS_FILE}
  ${NCP} $DATA/output/${memout}/${PDY}.${cyc}0000.sfc_data.nc ${RESTARTens_anl}/${memout}/${FV3_SFCD_ENS_FILE}
  ${NCP} ${COMOLD}/${old_out_prefix}.RESTART_ens/mem001/atmos_static.nc .
  ${NCP} ${COMOLD}/${old_out_prefix}.RESTART_ens/mem001/grid_spec.nc .
  ${NCP} ${COMOLD}/${old_out_prefix}.RESTART_ens/mem001/oro_data.nc .
done
