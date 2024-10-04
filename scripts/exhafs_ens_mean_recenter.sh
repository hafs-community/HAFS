#!/bin/sh
# exhafs_recenter: Depend upon the value of ldo_enscalc_option, this script can
# perform the EnKF mean, and recenter functions by running the RRFS's recenter/mean code
# ldo_enscalc_option=1: enkf_mean, calculate the ensemble mean
# ldo_enscalc_option=2: enkf_recenter, recenter the ensemble memmber analysis around the deterministic EnVar analysis

set -xe

if [ ${ENSDA} = YES ]; then
  export NHRS=${NHRS_ENS:-126}
  export NBDYHRS=${NBDYHRS_ENS:-3}
  export NOUTHRS=${NOUTHRS_ENS:-3}
  export CASE=${CASE_ENS:-C768}
  export CRES=$(echo $CASE | cut -c 2-)
  export gtype=${gtype_ens:-regional}
  export LEVS=${LEVS_ENS:-65}
else
  export NHRS=${NHRS:-126}
  export NBDYHRS=${NBDYHRS:-3}
  export NOUTHRS=${NOUTHRS:-3}
  export CASE=${CASE:-C768}
  export CRES=$(echo $CASE | cut -c 2-)
  export gtype=${gtype:-regional}
  export LEVS=${LEVS:-65}
fi

if [ $GFSVER = PROD2021 ]; then
  export atmos="atmos/"
elif [ $GFSVER = PROD2019 ]; then
  export atmos=""
else
  export atmos="atmos/"
fi

export RUN_ENVAR=${RUN_ENVAR:-NO}
export ldo_enscalc_option=${ldo_enscalc_option:-1}
export nens=${ENS_SIZE:-40}
export PARMgsi=${PARMgsi:-${PARMhafs}/analysis/gsi}

CDATE=${CDATE:-${YMDH}}
yr=$(echo $CDATE | cut -c1-4)
mn=$(echo $CDATE | cut -c5-6)
dy=$(echo $CDATE | cut -c7-8)
CDATEprior=$(${NDATE} -6 $CDATE)
ymdprior=$(echo ${CDATEprior} | cut -c1-8)
hhprior=$(echo ${CDATEprior} | cut -c9-10)

export RESTARTens_inp=${COMOLD}/${old_out_prefix}.RESTART_ens
export RESTARTens_anl=${WORKhafs}/intercom/RESTART_analysis_ens
export DIAGens_anl=${COMhafs}
export GRID_RATIO_ENS=${GRID_RATIO_ENS:-1}
DATOOL=${DATOOL:-${EXEChafs}/hafs_tools_datool.x}
mkdir -p ${RESTARTens_anl}
mkdir -p ${DIAGens_anl}

DATA=${DATA:-${WORKhafs}/enkf_mean}
mkdir -p ${DATA}
cd ${DATA}

if [ $ldo_enscalc_option -eq 1 ]; then # enkf_mean 
  MEAN='.true.'
  RECENTER='.false.'
  # prepare ensemble mean files
  memstr="mem001"
  ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3sar_tile1_dynvar
  ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3sar_tile1_tracer
  ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.sfc_data.nc fv3sar_tile1_sfcvar
  ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.coupler.res coupler.res
  ${NLN} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.nc fv_core.res.nc   
  ${NLN} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc fv_srf_wnd.res.tile1.nc
  for memstr in $(seq -f 'mem%03g' 1 $nens); do
    ${NLN} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3sar_tile1_${memstr}_dynvar
    ${NLN} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3sar_tile1_${memstr}_tracer
    ${NLN} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.sfc_data.nc fv3sar_tile1_${memstr}_sfcvar
  done
else # enkf_recenter
  MEAN='.false.'
  RECENTER='.true.'
  memstr="mem001"
  ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc control_dynvar
  ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc control_tracer
  ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.sfc_data.nc control_sfcvar
  ${NLN} control_sfcvar control_sfc_data.nc
  ${NLN} control_tracer control_fv_tracer.res.tile1.nc
  ${NLN} control_dynvar control_fv_core.res.tile1.nc
  MERGE_CMD="${APRUNC} ${DATOOL} remap"
  for var in fv_core.res.tile1 fv_tracer.res.tile1 sfc_data; do
    in_grid=${WORKhafs}/intercom/RESTART_analysis/grid_spec.nc
    out_grid=${RESTARTens_inp}/${memstr}/grid_spec.nc
    in_file=${WORKhafs}/intercom/RESTART_analysis/${PDY}.${cyc}0000.${var}.nc
    out_file=./control_${var}.nc
    if [ ! -s ${in_grid} ] || [ ! -s ${in_file} ] || \
       [ ! -s ${out_grid} ] || [ ! -s ${out_file} ]; then
      echo "FATAL ERROR: Missing in/out_grid or in/out_file"
      exit 1
    fi
    if [ ${GRID_RATIO_ENS} -eq 1 ]; then
      ${NCP} $in_file $out_file
    else
      ${MERGE_CMD} \
        --in_grid=${in_grid} \
        --out_grid=${out_grid} \
        --in_file=${in_file} \
        --out_file=${out_file}
        status=$?; [[ $status -ne 0 ]] && exit $status
    fi
  done
  ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3sar_tile1_dynvar
  ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3sar_tile1_tracer
  ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.sfc_data.nc fv3sar_tile1_sfcvar
  for memstr in $(seq -f 'mem%03g' 1 $nens); do
    ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3sar_tile1_${memstr}_dynvar
    ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3sar_tile1_${memstr}_tracer
    ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.sfc_data.nc fv3sar_tile1_${memstr}_sfcvar
    ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc rec_fv3sar_tile1_${memstr}_dynvar
    ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc rec_fv3sar_tile1_${memstr}_tracer
    ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.sfc_data.nc rec_fv3sar_tile1_${memstr}_sfcvar
  done
fi

${NCP}  ${PARMgsi}/hafs_ens_mean_recenter.nml.tmp ./
sed -e "s/_ENS_SIZE_/${nens:-40}/g" \
    -e "s/_L_WRITE_MEAN_/${MEAN:-.false.}/g" \
    -e "s/_L_RECENTER_/${RECENTER:-.false.}/g" \
    hafs_ens_mean_recenter.nml.tmp > hafs_ens_mean_recenter.nml

RECENTEREXEC=${RECENTEREXEC:-$HOMEhafs/exec/hafs_tools_ens_mean_recenter.x}
${APRUNC} ${RECENTEREXEC} hafs_ens_mean_recenter.nml 2>&1 | tee stdout
export err=$?; err_chk

if  [ $ldo_enscalc_option -eq 1 ]; then # enkf_mean
  memstr="ensmean"
  mkdir -p ${RESTARTens_anl}/${memstr}
  ${NCP} fv3sar_tile1_dynvar ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc
  ${NCP} fv3sar_tile1_tracer ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
  ${NCP} fv3sar_tile1_sfcvar ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.sfc_data.nc
  ${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.coupler.res ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.fv_core.res.nc ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ${RESTARTens_anl}/${memstr}/
  #${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.phy_data.nc ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/grid_spec.nc ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/oro_data.nc ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/atmos_static.nc ${RESTARTens_anl}/${memstr}/
elif [ $ldo_enscalc_option -eq 2 ]; then # enkf_recenter
  mkdir -p ${RESTARTens_anl}/anlmean
  ${NCP} fv3sar_tile1_dynvar ${RESTARTens_anl}/anlmean/${PDY}.${cyc}0000.fv_core.res.tile1.nc
  ${NCP} fv3sar_tile1_tracer ${RESTARTens_anl}/anlmean/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
  ${NCP} fv3sar_tile1_sfcvar ${RESTARTens_anl}/anlmean/${PDY}.${cyc}0000.sfc_data.nc
  #${NCP} ${COMhafs}/RESTART_analysis/{*grid_spec.nc,*sfc_data.nc,*coupler.res,gfs_ctrl.nc,fv_core.res.nc,*bndy*} ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.coupler.res ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_core.res.nc ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/grid_spec.nc ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/oro_data.nc ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/atmos_static.nc ${RESTARTens_anl}/anlmean/
  for memstr in $(seq -f 'mem%03g' 1 $nens); do
    mkdir -p ${RESTARTens_anl}/${memstr}
    ${NCP} rec_fv3sar_tile1_${memstr}_dynvar ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc
    ${NCP} rec_fv3sar_tile1_${memstr}_tracer ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
    ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.coupler.res ${RESTARTens_anl}/${memstr}/
    ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.nc ${RESTARTens_anl}/${memstr}/
    ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ${RESTARTens_anl}/${memstr}/
    ${NCP} ${RESTARTens_inp}/${memstr}/grid_spec.nc ${RESTARTens_anl}/${memstr}/
    ${NCP} ${RESTARTens_inp}/${memstr}/oro_data.nc ${RESTARTens_anl}/${memstr}/
    ${NCP} ${RESTARTens_inp}/${memstr}/atmos_static.nc ${RESTARTens_anl}/${memstr}/
  done
else
  echo "Wrong ldo_enscalc_option: $ldo_enscalc_option"
fi

exit
