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
MIN_LON=$(echo "$cenlon - ${dlon_cutoff:-180}" | bc) #Domain cut for DA efficiency, need to consider domain flexibility later
MAX_LON=$(echo "$cenlon + ${dlon_cutoff:-180}" | bc)
MIN_LAT=$(echo "$cenlat - ${dlat_cutoff:-90}" | bc)
MAX_LAT=$(echo "$cenlat + ${dlat_cutoff:-90}" | bc)
DATOOL=${DATOOL:-${EXEChafs}/hafs_tools_datool.x}
MERGE_CMD="${APRUNS} ${DATOOL} remap"
TOTAL_TASKS_tmp=60 #${TOTAL_TASKS}
DIRECT_GDASENS=${DIRECT_GDASENS:-NO}

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
if [ ${nest_grids} -ge 2 ]; then
  FV3_SFCD_FILE=${PDY}.${cyc}0000.sfc_data${neststr}${tilestr}.nc
else
  FV3_SFCD_FILE=${PDY}.${cyc}0000.sfc_data.nc
fi
FV3_SFCW_FILE=${PDY}.${cyc}0000.fv_srf_wnd.res${neststr}${tilestr}.nc
FV3_CPLR_FILE=${PDY}.${cyc}0000.coupler.res
FV3_AKBK_FILE=${PDY}.${cyc}0000.fv_core.res${neststr}.nc
FV3_CORE_FILE3=${ymdtm03}.${hhtm03}0000.fv_core.res${neststr}${tilestr}.nc
FV3_TRCR_FILE3=${ymdtm03}.${hhtm03}0000.fv_tracer.res${neststr}${tilestr}.nc
if [ ${nest_grids} -ge 2 ]; then
  FV3_SFCD_FILE3=${ymdtm03}.${hhtm03}0000.sfc_data${neststr}${tilestr}.nc
else
  FV3_SFCD_FILE3=${ymdtm03}.${hhtm03}0000.sfc_data.nc
fi
FV3_SFCW_FILE3=${ymdtm03}.${hhtm03}0000.fv_srf_wnd.res${neststr}${tilestr}.nc
FV3_CPLR_FILE3=${ymdtm03}.${hhtm03}0000.coupler.res
FV3_AKBK_FILE3=${ymdtm03}.${hhtm03}0000.fv_core.res${neststr}.nc
FV3_CORE_FILE9=${ymdtp03}.${hhtp03}0000.fv_core.res${neststr}${tilestr}.nc
FV3_TRCR_FILE9=${ymdtp03}.${hhtp03}0000.fv_tracer.res${neststr}${tilestr}.nc
if [ ${nest_grids} -ge 2 ]; then
  FV3_SFCD_FILE9=${ymdtp03}.${hhtp03}0000.sfc_data${neststr}${tilestr}.nc
else
  FV3_SFCD_FILE9=${ymdtp03}.${hhtp03}0000.sfc_data.nc
fi
FV3_SFCW_FILE9=${ymdtp03}.${hhtp03}0000.fv_srf_wnd.res${neststr}${tilestr}.nc
FV3_CPLR_FILE9=${ymdtp03}.${hhtp03}0000.coupler.res
FV3_AKBK_FILE9=${ymdtp03}.${hhtp03}0000.fv_core.res${neststr}.nc

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
${NCP} ${PARMjedi}/fmsmpp.nml .
${NCP} ${PARMjedi}/satinfo .
if [ ${nest_grids} -ge 2 ]; then #Is it reasonable to keep ens the same as bkg when nest run?
sed -e "s|_NPX_|${npx_nest}|g" \
    -e "s|_NPY_|${npy_nest}|g" \
    -e "s|_NPZ_|${npz}|g" \
    -e "s|_FV3_GRID_FILE_|${CASE}_mosaic_bkg.nc|g" \
    -e "s|_LAYOUTX_|${layoutx_jedi}|g" \
    -e "s|_LAYOUTY_|${layouty_jedi}|g" \
    -e "s|_DLON_|${target_lon}|g" \
    -e "s|_DLAT_|${target_lat}|g" \
    ${PARMjedi}/input_hafs.nml > input_hafs_ens.nml
else
sed -e "s|_NPX_|${npx_ens}|g" \
    -e "s|_NPY_|${npy_ens}|g" \
    -e "s|_NPZ_|${npz}|g" \
    -e "s|_FV3_GRID_FILE_|${CASE}_mosaic_ens.nc|g" \
    -e "s|_LAYOUTX_|${layoutx_jedi}|g" \
    -e "s|_LAYOUTY_|${layouty_jedi}|g" \
    -e "s|_DLON_|${target_lon}|g" \
    -e "s|_DLAT_|${target_lat}|g" \
    ${PARMjedi}/input_hafs.nml > input_hafs_ens.nml
fi
if [ ${nest_grids} -ge 2 ]; then
sed -e "s|_NPX_|${npx_nest}|g" \
    -e "s|_NPY_|${npy_nest}|g" \
    -e "s|_NPZ_|${npz}|g" \
    -e "s|_FV3_GRID_FILE_|${CASE}_mosaic_bkg.nc|g" \
    -e "s|_LAYOUTX_|${layoutx_jedi}|g" \
    -e "s|_LAYOUTY_|${layouty_jedi}|g" \
    -e "s|_DLON_|${target_lon}|g" \
    -e "s|_DLAT_|${target_lat}|g" \
    ${PARMjedi}/input_hafs.nml > input_hafs_bkg.nml
else
sed -e "s|_NPX_|${npx}|g" \
    -e "s|_NPY_|${npy}|g" \
    -e "s|_NPZ_|${npz}|g" \
    -e "s|_FV3_GRID_FILE_|${CASE}_mosaic_bkg.nc|g" \
    -e "s|_LAYOUTX_|${layoutx_jedi}|g" \
    -e "s|_LAYOUTY_|${layouty_jedi}|g" \
    -e "s|_DLON_|${target_lon}|g" \
    -e "s|_DLAT_|${target_lat}|g" \
    ${PARMjedi}/input_hafs.nml > input_hafs_bkg.nml
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
if [ ${RUN_ENSDA} = "YES" ]; then
  COMOLD00_Ens=${COMOLD}/../00L/00l.${ymdprior}${hhprior}.RESTART_ens
  if [ -s ${COMOLD}/${old_out_prefix}.RESTART_ens/mem001/${PDY}.${cyc}0000.fv_core.res${neststr}.nc ]; then
    ${NCP} ${COMOLD}/${old_out_prefix}.RESTART_ens/mem001/${PDY}.${cyc}0000.fv_core.res${neststr}.nc ${PDY}.${cyc}0000.fv_core_ens.res.nc
    FV3_AKBK_ENS_FILE_BUMP=${PDY}.${cyc}0000.fv_core_ens.res.nc
  elif [ -s ${COMOLD}/${old_out_prefix}.RESTART_ens/mem001/${PDY}.${cyc}0000.fv_core.res.nc ]; then
    ${NCP} ${COMOLD}/${old_out_prefix}.RESTART_ens/mem001/${PDY}.${cyc}0000.fv_core.res.nc ${PDY}.${cyc}0000.fv_core_ens.res.nc
    FV3_AKBK_ENS_FILE_BUMP=${PDY}.${cyc}0000.fv_core_ens.res.nc
  elif [ -s ${COMOLD00_Ens}/mem001/${PDY}.${cyc}0000.fv_core.res.nc ]; then
    ${NCP} ${COMOLD00_Ens}/mem001/${PDY}.${cyc}0000.fv_core.res.nc ${PDY}.${cyc}0000.fv_core_ens.res.nc
    FV3_AKBK_ENS_FILE_BUMP=${PDY}.${cyc}0000.fv_core_ens.res.nc
  fi
elif [ -s ${WORKhafs}/intercom/ENS_PREP/mem001/${PDY}.${cyc}0000.fv_core.res${neststr}.nc ]; then
  ${NCP} ${WORKhafs}/intercom/ENS_PREP/mem001/${PDY}.${cyc}0000.fv_core.res${neststr}.nc ${PDY}.${cyc}0000.fv_core_ens.res.nc
  FV3_AKBK_ENS_FILE_BUMP=${PDY}.${cyc}0000.fv_core_ens.res.nc
else
  FV3_AKBK_ENS_FILE_BUMP=${FV3_AKBK_FILE}
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
      if [ -d ${COMOLD00_Ens} ]; then
        RESTARTens=${COMOLD00_Ens}/mem${mem}
      fi
      for file in `ls ${RESTARTens}/*`; do
        ${NLN} ${file} ${DATA}/ensemble_data/mem${mem}/
      done
    else
      if [[ ${DIRECT_GDASENS} = "NO" ]]; then
        RESTARTens=${WORKhafs}/intercom/ENS_PREP/mem${mem}
        for file in `ls ${RESTARTens}/*`; do
          ${NLN} ${file} ${DATA}/ensemble_data/mem${mem}/
        done
      else
        if [ ${l4densvar:-.false.} = ".true." ]; then
	  fhrs="03 06 09"
        else
          fhrs="06"
        fi
        cd ${DATA}/ensemble_data
        for mem in $(seq -f '%03g' 1 ${n_ens_fv3sar}); do
          mkdir ${DATA}/ensemble_data/mem${mem}
          for fhh in $fhrs; do
            if [ $GFSVER = "PROD2021" ]; then
              ${NLN} ${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/atmos/mem${mem}/gdas.t${hhprior}z.atmf0${fhh}${GSUFFIX:-.nc} ${DATA}/ensemble_data/mem${mem}/gdas.atmf0${fhh}
            elif [ $GFSVER = "PROD2026" ]; then
              ${NLN} ${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/mem${mem}/model/atmos/history/enkfgdas.t${hhprior}z.atm.f0${fhh}${GSUFFIX:-.nc} ${DATA}/ensemble_data/mem${mem}/gdas.atmf0${fhh}
            fi
          done
        done
      fi
    fi
  done
else ## Should be 3DVar, But since JEDI is having trouble with static B. Here use direct GDAS Ens instead
  if [ ${l4densvar:-.false.} = ".true." ]; then
    fhrs="03 06 09"
  else
    fhrs="06"
  fi
  cd ${DATA}/ensemble_data
  for mem in $(seq -f '%03g' 1 ${n_ens_fv3sar}); do
    mkdir ${DATA}/ensemble_data/mem${mem}
    for fhh in $fhrs; do
      if [ $GFSVER = "PROD2021" ]; then
        ${NLN} ${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/atmos/mem${mem}/gdas.t${hhprior}z.atmf0${fhh}${GSUFFIX:-.nc} ${DATA}/ensemble_data/mem${mem}/enkfgdas.${ymdprior}${hhprior}.atmf0${fhh}
      elif [ $GFSVER = "PROD2026" ]; then
        ${NLN} ${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/mem${mem}/model/atmos/history/enkfgdas.t${hhprior}z.atm.f0${fhh}${GSUFFIX:-.nc} ${DATA}/ensemble_data/mem${mem}/enkfgdas.${ymdprior}${hhprior}.atmf0${fhh}
      fi
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
radtypes="radiance_atms_npp radiance_amsua_n19 radiance_atms_n20 radiance_iasi_metop-b radiance_ssmis_f17 radiance_amsua_metop-b radiance_amsua_n18 radiance_abi_g16 radiance_abi_g18 radiance_cris-fsr_n20 radiance_cris-fsr_n21 radiance_cris-fsr_npp"
convtypes="conventional_air_aircar_133q conventional_air_aircar_133t conventional_air_aircar_233 conventional_air_drpsnd_137q conventional_air_drpsnd_137t conventional_air_drpsnd_237 conventional_air_amdar_130t conventional_air_amdar_131t conventional_air_amdar_230 conventional_air_amdar_231 conventional_air_amdar_234 conventional_air_amdar_235 conventional_air_hdob_136q conventional_air_hdob_136t conventional_air_hdob_236 conventional_air_raob_120q conventional_air_raob_220 conventional_air_raob_120t conventional_land_synop_181ps conventional_land_synop_187ps conventional_land_synop_181q conventional_land_synop_181t conventional_land_synop_281 conventional_land_synop_287 conventional_radar_tdr_992 conventional_radar_tdr_993 conventional_sea_ship_180ps conventional_sea_ship_180q conventional_sea_ship_180t conventional_sea_ship_280 retrieval_amv_seviri_m8 retrieval_amv_seviri_m9 retrieval_amv_seviri_m10 retrieval_amv_seviri_m11 retrieval_amv_abi_goes-16 retrieval_amv_abi_goes-18 retrieval_hiamv_abi_goes-16 retrieval_hiamv_abi_goes-18 retrieval_hiamv_abi_goes-19 conventional_osw_ascat conventional_air_raob_120ps conventional_gnssro_cosmic2 conventional_gnssro_geooptics conventional_gnssro_grace conventional_gnssro_kompsat5 conventional_gnssro_metop conventional_gnssro_paz.yaml conventional_gnssro_planetiq conventional_gnssro_sentinel6 conventional_gnssro_spire conventional_gnssro_tandemx conventional_gnssro_terrasarx" #conventional_radar_vadwnd
convfiles="conventional_air_aircar conventional_air_drpsnd conventional_air_amdar conventional_air_hdob conventional_air_raob conventional_land_synop conventional_radar_tdr conventional_sea_ship retrieval_amv_seviri_m8 retrieval_amv_seviri_m9 retrieval_amv_seviri_m10 retrieval_amv_seviri_m11 retrieval_amv_abi_goes-16 retrieval_amv_abi_goes-18 retrieval_hiamv_abi_goes-16 retrieval_hiamv_abi_goes-18 retrieval_hiamv_abi_goes-19 conventional_gnssro_cosmic2 conventional_gnssro_geooptics conventional_gnssro_grace conventional_gnssro_kompsat5 conventional_gnssro_metop conventional_gnssro_paz.yaml conventional_gnssro_planetiq conventional_gnssro_sentinel6 conventional_gnssro_spire conventional_gnssro_tandemx conventional_gnssro_terrasarx" #conventional_radar_vadwnd
mkdir -p "${DATA}/obs"
cd "${DATA}/obs" || exit 1
IFS=' ' read -ra convtypes_array <<< "$convtypes"
IFS=' ' read -ra convfiles_array <<< "$convfiles"
valid_convfiles=()
valid_convtypes=()
checked_convfiles=()
for type in "${convtypes_array[@]}"; do
  matched_file=""
  # Find the matching parent convfile for this convtype
  # Use longest prefix match
  for file in "${convfiles_array[@]}"; do
    if [[ "$type" == "$file"* ]]; then
      if [[ -z "$matched_file" || ${#file} -gt ${#matched_file} ]]; then
        matched_file="$file"
      fi
    fi
  done
  if [[ -z "$matched_file" ]]; then
    echo "WARNING: No matching convfile found for convtype $type, skipping"
    continue
  fi
  src_ncfile="${OBSIODA_DIR}/hafs.t${cyc}z.${matched_file}.nc"
  local_ncfile="${DATA}/obs/hafs.t${cyc}z.${matched_file}.nc"
  tgt_ncfile="${DATA}/obs/hafs.t${cyc}z.${type}.nc"
  if [[ ! -f "$src_ncfile" ]]; then
    echo "WARNING: Missing source file $src_ncfile for convtype $type, skipping"
    continue
  fi
  # Check each source file only once
  need_check=1
  for f in "${checked_convfiles[@]}"; do
    if [[ "$f" == "$matched_file" ]]; then
      need_check=0
      break
    fi
  done
  if [[ $need_check -eq 1 ]]; then
    nloc=$(ncdump -h "$src_ncfile" \
      | sed -n '/dimensions:/,/variables:/p' \
      | grep -E "Location|nobs" \
      | head -1 \
      | grep -oE '[0-9]+' \
      | tail -1)
    if [[ -z "$nloc" || "$nloc" -eq 0 ]]; then
      echo "Skipping empty or invalid file: $src_ncfile"
      checked_convfiles+=("$matched_file")
      continue
    fi
    # Copy source file into ${DATA}/obs once
    if [[ ! -f "$local_ncfile" ]]; then
      if [[ "$matched_file" == conventional_gnssro_* ]]; then
        python ${USHhafs:-${HOMEhafs}/ush}/offline_domain_check_gpsro.py -g ${DATA}/bkg/grid_spec${nesttilestr}.nc -i $src_ncfile -o $local_ncfile
        if [[ ! -s "$local_ncfile" ]]; then
          echo "Skipping: local file was not generated or is empty: $local_ncfile"
          checked_convfiles+=("$matched_file")
          continue
        fi
      else
        ${NCP} "$src_ncfile" "$local_ncfile"
      fi
#      python ${USHhafs:-${HOMEhafs}/ush}/offline_domain_check.py -g ${DATA}/bkg/grid_spec${nesttilestr}.nc -o $src_ncfile -s 0.1 -clon=${target_lon} -clat=${target_lat} -n ${TOTAL_TASKS} -out $local_ncfile
    fi
    checked_convfiles+=("$matched_file")
    valid_convfiles+=("$matched_file")
  else
    # If already checked before, make sure it was valid
    already_valid=0
    for f in "${valid_convfiles[@]}"; do
      if [[ "$f" == "$matched_file" ]]; then
        already_valid=1
        break
      fi
    done
    if [[ $already_valid -eq 0 ]]; then
      echo "Skipping convtype $type because parent file $matched_file was invalid"
      continue
    fi
    # If valid and local copy somehow missing, restore it
    if [[ ! -f "$local_ncfile" ]]; then
      ${NCP} "$src_ncfile" "$local_ncfile"
#      python ${USHhafs:-${HOMEhafs}/ush}/offline_domain_check.py -g ${DATA}/bkg/grid_spec${nesttilestr}.nc -o $src_ncfile -s 0.1 -clon=${target_lon} -clat=${target_lat} -n ${TOTAL_TASKS} -out $local_ncfile
    fi
  fi
  # Create link named after convtype, pointing to the local copied file
  ${NLN} "$local_ncfile" "$tgt_ncfile"
  valid_convtypes+=("$type")
done

# Update convtypes to only valid ones
convtypes="${valid_convtypes[*]}"
convfiles="${valid_convfiles[*]}"

# === Now handle radtypes ===
IFS=' ' read -ra radtypes_array <<< "$radtypes"

valid_radtypes=()
for file in ${radtypes_array[@]}; do
  ncfile="${OBSIODA_DIR}/hafs.t${cyc}z.${file}.nc"
  local_ncfile="${DATA}/obs/hafs.t${cyc}z.${file}.nc"
  if [[ -f "$ncfile" ]]; then
    nloc=$(ncdump -h "$ncfile" | sed -n '/dimensions:/,/variables:/p' | grep -E "Location|nobs" | head -1 | grep -oE '[0-9]+' | tail -1)
    if [[ -z "$nloc" || "$nloc" -eq 0 ]]; then
        echo "Skipping empty or invalid file: $ncfile"
        continue
    fi
    ${NLN} "$ncfile" .
#    python ${USHhafs:-${HOMEhafs}/ush}/offline_domain_check.py -g ${DATA}/bkg/grid_spec${nesttilestr}.nc -o ${ncfile} -s 0.1 -clon=${target_lon} -clat=${target_lat} -n ${TOTAL_TASKS} -out $local_ncfile
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
### XL need to think about tile #, is 7 & 8 always the default value?
#??XL: Do it here or in atm_prep

#----------------------------------------------
# Prepare NICAS LOCALIZATION
# ?? Should we do it here? XL
#----------------------------------------------
cd ${DATA}
export basic_yaml_dir=${PARMjedi}/yaml_templates/basic_config
export jcb_yaml_dir=${PARMjedi}/jcb-hdas/test/client_integration
mkdir ${DATA}/bump
if [ ${nest_grids} -ge 2 ]; then
  INPUT_HAFS_NML=input_hafs_bkg.nml
  ####### In the nest DA configuration, AKBK_ENS is used in the minimization. Needs to consider dual-resolution in the future
  ###################################################
  if [ ${RUN_ENSDA} = "YES" ]; then
    INPUT_HAFS_ENS_NML=input_hafs_ens.nml
  else
    FV3_AKBK_ENS_FILE=${FV3_AKBK_FILE}
    FV3_CORE_ENS_FILE=${FV3_CORE_FILE}
    FV3_TRCR_ENS_FILE=${FV3_TRCR_FILE}
    FV3_SFCD_ENS_FILE=${FV3_SFCD_FILE}
    FV3_SFCW_ENS_FILE=${FV3_SFCW_FILE}
    FV3_CPLR_ENS_FILE=${FV3_CPLR_FILE}
    INPUT_HAFS_ENS_NML=input_hafs_bkg.nml #If No HAFS Ens, GDAS Ens is interpolated to bkg.
  fi
else
  INPUT_HAFS_NML=input_hafs_bkg.nml
  INPUT_HAFS_ENS_NML=input_hafs_ens.nml
fi
if [ ${RUN_ENSDA} = "YES" ]; then
  ${NCP} ${basic_yaml_dir}/bump_nicas_dual.yaml bump_nicas.yaml.tmp
else
  ${NCP} ${basic_yaml_dir}/bump_nicas.yaml bump_nicas.yaml.tmp
fi
sed -e "s|_FV3_CORE_ENS_FILE_|${FV3_CORE_ENS_FILE}|g" \
    -e "s|_FV3_TRCR_ENS_FILE_|${FV3_TRCR_ENS_FILE}|g" \
    -e "s|_FV3_SFCD_ENS_FILE_|${FV3_SFCD_ENS_FILE}|g" \
    -e "s|_FV3_SFCW_ENS_FILE_|${FV3_SFCW_ENS_FILE}|g" \
    -e "s|_FV3_CPLR_ENS_FILE_|${FV3_CPLR_ENS_FILE}|g" \
    -e "s|_FV3_AKBK_ENS_FILE_|${FV3_AKBK_ENS_FILE_BUMP}|g" \
    -e "s|_INPUT_HAFS_ENS_NML_|${INPUT_HAFS_ENS_NML}|g" \
    -e "s|_ANALYSISDATE_|'${yr}-${mn}-${dy}T${hh}:00:00Z'|g" \
    -e "s|_LOC_H_|${loc_h}|g" \
    -e "s|_LOC_V_|${loc_v}|g" \
    -e "s|_RESOLUTION_|${bump_resolution}|g" \
    bump_nicas.yaml.tmp > bump_nicas.yaml
${NCP} ${EXEChafs}/hafs_jedi_nicas.x ./hafs_jedi_nicas.x
if [ ${l4densvar:-.true.} = ".true." ]; then
#  ${APRUNCD3} ${EXEChafs}/hafs_nicas.x bump_nicas.yaml nicas.log
  ${APRUNC} ./hafs_jedi_nicas.x bump_nicas.yaml nicas.log #XL turn off parallel subwindow until the thinning issue is fixed
else
  ${APRUNC} ./hafs_jedi_nicas.x bump_nicas.yaml nicas.log
fi
mv hdas-atmosphere-templates.yaml hdas-atmosphere-templates_bump.yaml
rm nicas.log.*
#----------------------------------------------
# Prepare yaml
#----------------------------------------------
cd ${DATA}
mkdir ${DATA}/hofx #Create hofx for diagfile output
sed -e "s|_TARGET_YAML_|jedi.yaml|g" ${jcb_yaml_dir}/run.py > run_jedi.py
if [ ${l4denvar:-.true.} = ".true." ]; then
export ALGORITHM="4denvar"
elif [ ${RUN_FGAT} = NO ]; then
export ALGORITHM="3denvar"
else
export ALGORITHM="3dfgat"
fi
if [[ ${DIRECT_GDASENS} = "NO" ]]; then
export ALGORITHM_COV=${ALGORITHM}
else
export ALGORITHM_COV="${ALGORITHM}_gdasens"
fi
if [ ${RUN_ENSDA} = "YES" ] && [ ${nest_grids} -eq 2 ]; then
  export neststr_ens=""
  export tilestr_ens=".tile1"
  export nesttilestr_ens=""
else
  export neststr_ens=${neststr}
  export tilestr_ens=${tilestr}
  export nesttilestr_ens=${nesttilestr}
fi
sed -e "s|_ALGORITHM_|${ALGORITHM}|g" \
    -e "s|_ALGORITHMCOV_|${ALGORITHM_COV}|g" \
    ${jcb_yaml_dir}/hdas-atmosphere-templates.yaml > hdas-atmosphere-templates.yaml.tmp
sed -e "s|_INITIALDATE_|${yrtm03}-${mntm03}-${dytm03}T${hhtm03}:00:00Z|g" \
    -e "s|_ANALYSISDATE_|${yr}-${mn}-${dy}T${hh}:00:00Z|g" \
    -e "s|_ENDDATE_|${yrtp03}-${mntp03}-${dytp03}T${hhtp03}:00:00Z|g" \
    -e "s|_HH_|${cyc}|g" \
    -e "s|_ENS_SIZE_|${ENS_SIZE}|g" \
    -e "s|_LAYOUTX_|${layoutx_jedi}|g" \
    -e "s|_LAYOUTY_|${layouty_jedi}|g" \
    -e "s|_LOC_H_|${loc_h}|g" \
    -e "s|_LOC_V_|${loc_v}|g" \
    -e "s|_POOL_SIZE_|${TOTAL_TASKS_tmp}|g" \
    -e "s|_MIN_LAT_|${MIN_LAT}|g" \
    -e "s|_MAX_LAT_|${MAX_LAT}|g" \
    -e "s|_MIN_LON_|${MIN_LON}|g" \
    -e "s|_MAX_LON_|${MAX_LON}|g" \
    -e "s|_NPX_|${npx_ens}|g" \
    -e "s|_NPY_|${npy_ens}|g" \
    -e "s|_NPZ_|${npz_ens}|g" \
    -e "s|_FV3_AKBK_FILE_|${FV3_AKBK_FILE}|g" \
    -e "s|_FV3_AKBK_ENS_FILE_|${FV3_AKBK_ENS_FILE_BUMP}|g" \
    -e "s|_YYMODD6_|${yr}${mn}${dy}|g" \
    -e "s|_HH6_|${hh}|g" \
    -e "s|_YYMODD3_|${yrtm03}${mntm03}${dytm03}|g" \
    -e "s|_HH3_|${hhtm03}|g" \
    -e "s|_YYMODD9_|${yrtp03}${mntp03}${dytp03}|g" \
    -e "s|_HH9_|${hhtp03}|g" \
    -e "s|_NESTTILESTRNC_|${neststr}${tilestr}.nc|g" \
    -e "s|_NESTTILESTRSFC_|${nesttilestr}.nc|g" \
    -e "s|_NESTTILESTRNCENS_|${neststr_ens}${tilestr_ens}.nc|g" \
    -e "s|_NESTTILESTRSFCENS_|${nesttilestr_ens}.nc|g" \
    -e "s|_INPUT_HAFS_ENS_NML_|${INPUT_HAFS_ENS_NML}|g" \
    -e "s|_INPUT_HAFS_NML_|${INPUT_HAFS_NML}|g" \
    -e "s|_DISTRIBUTION_|RoundRobin|g" \
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

${NLN} ${USHhafs}/offline_update_delp.py .
python offline_update_delp.py --sfc_inc Inc6.sfc_data.nc --core_inc Inc6.fv_core.res.nc --akbk bkg/${FV3_AKBK_FILE} --rst_file ${PDY}.${cyc}0000.fv_core.res.nc

#Store the output to intercom
${NCP} ${DATA}/${PDY}.${cyc}0000.fv_tracer.res.nc ${RESTARTanl}/${FV3_TRCR_FILE}
${NCP} ${DATA}/${PDY}.${cyc}0000.sfc_data.nc ${RESTARTanl}/${FV3_SFCD_FILE}
${NCP} ${DATA}/${PDY}.${cyc}0000.fv_srf_wnd.res.nc ${RESTARTanl}/${FV3_SFCW_FILE}
${NCP} ${DATA}/${PDY}.${cyc}0000.coupler.res ${RESTARTanl}/${FV3_CPLR_FILE}
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
IN_FILE=${DATA}/${PDY}.${cyc}0000.fv_core.res.nc
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
