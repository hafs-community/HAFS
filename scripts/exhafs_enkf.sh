#!/bin/sh
################################################################################
# Script Name: exhafs_enkf.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script performs the EnKF update functions by
#   running the GSI's enkf.x in different modes.
#     ldo_enscalc_option=0: enkf_update, condut the EnKF analysis to update the
#                           ensemble members
# History:
#   12/29/2020: Initial version for HAFS workflow with self-cycled ENSDA
#   01/27/2021: Support dual-resolution capability for HAFS DA workflow
# Condition codes:
#   == 0 : success
#   != 0 : fatal error encounted
################################################################################
set -x -o pipefail

export USE_CFP=${USE_CFP:NO}
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

export atmos="atmos/"

export PARMgsi=${PARMgsi:-${PARMhafs}/analysis/gsi}
export RUN_ENVAR=${RUN_ENVAR:-NO}
export ldo_enscalc_option=${ldo_enscalc_option:-1}
export nens=${ENS_SIZE:-40}
export online_satbias=${online_satbias:-no}
export corrlength=${corrlength:-500}
export lnsigcutoff=${lnsigcutoff:-1.3}
export nesttilestr=${nesttilestr:-""}

export gridstr=${gridstr:-$(echo ${out_gridnames} | cut -d, -f 1)}

if [ $ldo_enscalc_option -ne 0 ]; then # enkf_update
  echo "FATAL ERROR: Wrong ldo_enscalc_option: $ldo_enscalc_option. Exiting..."
  echo "FATAL ERROR: Wrong ldo_enscalc_option: 1 or 2 needs to be done through exchange_atm_recenter.sh . Exiting..."
  exit 2
fi

# Diagnostic files options
netcdf_diag=${netcdf_diag:-".true."}
binary_diag=${binary_diag:-".false."}

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
mkdir -p ${RESTARTens_anl}
mkdir -p ${DIAGens_anl}

DATA=${DATA:-${WORKhafs}/enkf_mean}
mkdir -p ${DATA}
cd ${DATA}

# prepare ensemble mean files
memstr="mem001"
ensmean="ensmean"
${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3sar_tile1_${ensmean}_dynvars
${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3sar_tile1_${ensmean}_tracer
${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3sar_tile1_${ensmean}_dynvars
${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3sar_tile1_${ensmean}_tracer
${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.nc fv3sar_tile1_akbk.nc
${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.sfc_data.nc fv3_sfcdata
${NCP} ${RESTARTens_inp}/${memstr}/grid_spec.nc fv3sar_tile1_grid_spec.nc
# prepare ensemble member files
rm -f cmdfile_prep_dynvartracer_ens
for memstr in $(seq -f 'mem%03g' 1 $nens); do
  cat > ./prep_dynvartracer_ens${memstr}.sh << EOFprep
#!/bin/sh
set -x
    ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3sar_tile1_${memstr}_dynvars
    ${NCP} ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3sar_tile1_${memstr}_tracer
EOFprep
  chmod +x ./prep_dynvartracer_ens${memstr}.sh
  echo "./prep_dynvartracer_ens${memstr}.sh" >> cmdfile_prep_dynvartracer_ens
done
chmod +x cmdfile_prep_dynvartracer_ens
if [ $USE_CFP = "YES" ] ; then
  ncmd=$(cat ./cmdfile_prep_dynvartracer_ens | wc -l)
  ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
  $APRUNCFP -n $ncmd_max cfp ./cmdfile_prep_dynvartracer_ens
else
  ${APRUNC} ${MPISERIAL} -m cmdfile_prep_dynvartracer_ens
fi

rm -f cmdfile
memstr="ensmean"
RADSTAT=${DIAGens_anl}/${memstr}/${out_prefix}.${RUN}.${gridstr}.analysis.radstat
CNVSTAT=${DIAGens_anl}/${memstr}/${out_prefix}.${RUN}.${gridstr}.analysis.cnvstat
echo "tar -xvf $RADSTAT" >> cmdfile
echo "tar -xvf $CNVSTAT" >> cmdfile

for memstr in $(seq -f "mem%03g" 1 $nens); do
  RADSTAT=${DIAGens_anl}/${memstr}/${out_prefix}.${RUN}.${gridstr}.analysis.radstat
  CNVSTAT=${DIAGens_anl}/${memstr}/${out_prefix}.${RUN}.${gridstr}.analysis.cnvstat
  echo "tar -xvf $RADSTAT" >> cmdfile
  echo "tar -xvf $CNVSTAT" >> cmdfile
done

chmod +x cmdfile
if [ $USE_CFP = "YES" ] ; then
  ncmd=$(cat ./cmdfile | wc -l)
  ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
  $APRUNCFP -n $ncmd_max cfp ./cmdfile
else
  ${APRUNC} ${MPISERIAL} -m cmdfile
fi

rm -f cmdfile
for gzfile in $(/bin/ls diag*ges*.gz); do
  echo "gzip -d $gzfile && rm -f $gzfile" >> cmdfile
done
chmod +x cmdfile
if [ $USE_CFP = "YES" ] ; then
  ncmd=$(cat ./cmdfile | wc -l)
  ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
  $APRUNCFP -n $ncmd_max cfp ./cmdfile
else
  ${APRUNC} ${MPISERIAL} -m cmdfile
fi

${NLN} ${PARMgsi}/nam_glb_berror.f77.gcv ./berror_stats
#checkgfs ${NLN} $RADCLOUDINFO cloudy_radiance_info.txt
${NLN} ${PARMgsi}/atms_beamwidth.txt ./atms_beamwidth.txt
#checkgfs ${NLN} $vqcdat       vqctp001.dat
#checkgfs ${NLN} $INSITUINFO   insituinfo
${NLN} ${PARMgsi}/nam_global_pcpinfo.txt ./pcpinfo
#checkgfs ${NLN} $AEROINFO     aeroinfo
#checkgfs ${NLN} $HYBENSINFO   hybens_info
${NLN} ${PARMgsi}/hafs_nam_errtable.r3dv ./errtable

pseudo_rh=.true.
anavinfo=${PARMgsi}/hafs_anavinfo.tmp
#${NCP} ${anavinfo} ./anavinfo
sed -e "s/_LEV_/${npz:-64}/g" \
    -e "s/_LP1_/${LEVS:-65}/g" \
    ${anavinfo} > ./anavinfo

${NLN} ${PARMgsi}/hafs_satinfo.txt ./satinfo
${NLN} ${PARMgsi}/global_scaninfo.txt ./scaninfo
#${NLN} ${PARMgsi}/nam_global_satangbias.txt ./satbias_angle
${NLN} ${PARMgsi}/global_ozinfo.txt ./ozinfo
${NLN} ${PARMgsi}/hafs_convinfo.txt ./convinfo

# Workflow will read from previous cycles for satbias predictors if online_satbias is set to yes
if [ $GFSVER = "PROD2021" ]; then
  SATBIAS_IN=${COMINgdas}/gdas.${ymdprior}/${hhprior}/${atmos}gdas.t${hhprior}z.abias
  SATBIAS_PC=${COMINgdas}/gdas.${ymdprior}/${hhprior}/${atmos}gdas.t${hhprior}z.abias_pc
fi
if [ $GFSVER = "PROD2026" ]; then
  SATBIAS_IN=${COMINgdas}/gdas.${ymdprior}/${hhprior}/analysis/atmos/gdas.t${hhprior}z.abias.txt
  SATBIAS_PC=${COMINgdas}/gdas.${ymdprior}/${hhprior}/analysis/atmos/gdas.t${hhprior}z.abias_pc.txt
fi
if [ ${online_satbias} = "yes" ] && [ ${RUN_ENVAR} = "YES" ]; then
  PASSIVE_BC=.true.
  UPD_PRED=1
  if [ ! -s ${COMOLD}/${old_out_prefix}.${RUN}.${gridstr}.analysis.abias ] || [ ! -s ${COMOLD}/${old_out_prefix}.${RUN}.${gridstr}.analysis.abias_pc ]; then
    echo "Prior cycle satbias data does not exist. Grabbing satbias data from GDAS"
    ${NLN} ${SATBIAS_IN}                                                           satbias_in
    ${NLN} ${SATBIAS_PC}                                                           satbias_pc
  elif [ -s ${COMOLD}/${old_out_prefix}.${RUN}.${gridstr}.analysis.abias ] && [ -s ${COMOLD}/${old_out_prefix}.${RUN}.${gridstr}.analysis.abias_pc ]; then
    ${NLN} ${COMOLD}/${old_out_prefix}.${RUN}.${gridstr}.analysis.abias            satbias_in
    ${NLN} ${COMOLD}/${old_out_prefix}.${RUN}.${gridstr}.analysis.abias_pc         satbias_pc
  else
    echo "FATAL ERROR: Either source satbias_in or source satbias_pc does not exist. Exiting script."
    exit 2
  fi
elif [ ${online_satbias} = "yes" ] && [ ${RUN_ENVAR} = "NO" ]; then
  echo "FATAL ERROR: Cannot run online satbias correction without EnVar. Exiting script."
  exit 2
else
  PASSIVE_BC=.false.
  UPD_PRED=0
  ${NLN} ${SATBIAS_IN}                                                             satbias_in
  ${NLN} ${SATBIAS_PC}                                                             satbias_pc
fi

# Make enkf namelist
${NCP} ${PARMgsi}/enkf.nml.tmp ./

sed -e "s/_datestring_/${CDATE}/g" \
    -e "s/_corrlength_/${corrlength:-500}/g" \
    -e "s/_lnsigcutoff_/${lnsigcutoff:-1.3}/g" \
    -e "s/_nlons_/$((${npx_ens:-$npx}-1))/g" \
    -e "s/_nlats_/$((${npy_ens:-$npy}-1))/g" \
    -e "s/_nlevs_/${npz_ens:-$npz}/g" \
    -e "s/_nanals_/${nens}/g" \
    -e "s/_pseudo_rh_/${pseudo_rh:-.false.}/g" \
    -e "s/_netcdf_diag_/${netcdf_diag}/g" \
    -e "s/_ldo_enscalc_option_/${ldo_enscalc_option}/g" \
    -e "s/_nx_res_/$((${npx_ens:-$npx}-1))/g" \
    -e "s/_ny_res_/$((${npy_ens:-$npy}-1))/g" \
    enkf.nml.tmp > ./enkf.nml

ENKFEXEC=${ENKFEXEC:-$HOMEhafs/exec/hafs_gsi_enkf.x}
${NCP} -p $ENKFEXEC ./hafs_gsi_enkf.x
${SOURCE_PREP_STEP}
${APRUNC} ./hafs_gsi_enkf.x < enkf.nml 2>&1 | tee gsi_enkf.log
export err=$?; err_chk

rm -f cmdfile
for memstr in $(seq -f "mem%03g" 1 $nens); do
  mkdir -p ${RESTARTens_anl}/${memstr}
  echo "${NCP} fv3sar_tile1_${memstr}_dynvars ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc" >> cmdfile
  echo "${NCP} fv3sar_tile1_${memstr}_tracer ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc" >> cmdfile
done
chmod +x cmdfile
if [ $USE_CFP = "YES" ] ; then
  ncmd=$(cat ./cmdfile | wc -l)
  ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
  $APRUNCFP -n $ncmd_max cfp ./cmdfile
else
  ${APRUNC} ${MPISERIAL} -m cmdfile
fi
export err=$?; err_chk
