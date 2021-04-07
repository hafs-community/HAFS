#!/bin/sh
# exhafs_enkf.sh: Depend upon the value of ldo_enscalc_option, this script can
# perform the EnKF mean, update, and recenter functions by running the GSI's
# enkf.x in different modes. 
# ldo_enscalc_option=1: enkf_mean, calculate the ensemble mean 
# ldo_enscalc_option=0: enkf_update, condut the EnKF analysis to update the ensemble members 
# ldo_enscalc_option=2: enkf_recenter, recenter the ensemble memmber analysis around the deterministic EnVar analysis 

set -xe

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

if [ $GFSVER = PROD2021 ]; then
  export atmos="atmos/"
elif [ $GFSVER = PROD2019 ]; then
  export atmos=""
else
  export atmos=""
fi

export PARMgsi=${PARMgsi:-${PARMhafs}/analysis/gsi}
export ldo_enscalc_option=${ldo_enscalc_option:-1}
export nens=${ENS_SIZE:-40}

# Diagnostic files options
netcdf_diag=${netcdf_diag:-".true."}
binary_diag=${binary_diag:-".false."}

yr=`echo $CDATE | cut -c1-4`
mn=`echo $CDATE | cut -c5-6`
dy=`echo $CDATE | cut -c7-8`

CDATEprior=`${NDATE} -6 $CDATE`
yrprior=`echo ${CDATEprior} | cut -c1-4`
mnprior=`echo ${CDATEprior} | cut -c5-6`
dyprior=`echo ${CDATEprior} | cut -c7-8`
hhprior=`echo ${CDATEprior} | cut -c9-10`
cycprior=`echo ${CDATEprior} | cut -c9-10`
PDYprior=`echo ${CDATEprior} | cut -c1-8`

export COMhafsprior=${COMhafsprior:-${COMhafs}/../../${CDATEprior}/${STORMID}}
export WORKhafsprior=${WORKhafsprior:-${WORKhafs}/../../${CDATEprior}/${STORMID}}

if [ ${RUN_GSI_VR_ENS} = YES ]; then
  export RESTARTens_inp=${COMhafs}/RESTART_analysis_vr_ens
else
  export RESTARTens_inp=${COMhafsprior}/RESTART_ens
fi

export RESTARTens_anl=${COMhafs}/RESTART_analysis_ens
mkdir -p ${RESTARTens_anl}

DATA=${DATA:-${WORKhafs}/enkf_mean}
mkdir -p ${DATA}
cd ${DATA}

if [ $ldo_enscalc_option -ne 2 ]; then # enkf_mean or enkf_update
  # prepare ensemble mean files
  memstr="mem001"
  ensmean="ensmean"
  cp ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3sar_tile1_${ensmean}_tracer
  cp ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3sar_tile1_${ensmean}_dynvars
  #dynvar_list="delp,DZ,phis,T,u,ua,v,va,W"
  dynvar_list=$(ncks --trd -m fv3sar_tile1_${ensmean}_dynvars | grep -E ': type' | cut -f 1 -d ' ' | sed 's/://' | sort | grep -v 'Time' | grep -v 'axis' | paste -sd "," -)
  #tracer_list="cld_amt,graupel,ice_wat,liq_wat,o3mr,rainwat,snowwat,sphum"
  tracer_list=$(ncks --trd -m fv3sar_tile1_${ensmean}_tracer  | grep -E ': type' | cut -f 1 -d ' ' | sed 's/://' | sort | grep -v 'Time' | grep -v 'axis' | paste -sd "," -)
  ncrename -d yaxis_1,yaxis_2 -v yaxis_1,yaxis_2 fv3sar_tile1_${ensmean}_tracer
  # somehow the yaxis_2 get default values, the following line is a temporary workaround
  ncks --no-abc -A -v yaxis_2 fv3sar_tile1_${ensmean}_dynvars fv3sar_tile1_${ensmean}_tracer
  ncks -A -v ${tracer_list} fv3sar_tile1_${ensmean}_tracer fv3sar_tile1_${ensmean}_dynvars
  mv fv3sar_tile1_${ensmean}_dynvars fv3sar_tile1_${ensmean}_dynvartracer
  rm -f fv3sar_tile1_${ensmean}_tracer
  cp ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.nc fv3sar_tile1_akbk.nc
  cp ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.sfc_data.nc fv3_sfcdata
  cp ${RESTARTens_inp}/${memstr}/grid_spec.nc fv3sar_tile1_grid_spec.nc
  # prepare ensemble member files
  rm -f cmdfile_prep_dynvartracer_ens
  for memstr in $(seq -f 'mem%03g' 1 $nens)
  do
    cat > ./prep_dynvartracer_ens${memstr}.sh << EOFprep
#!/bin/sh
set -x
    cp ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3sar_tile1_${memstr}_dynvars 
    cp ${RESTARTens_inp}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3sar_tile1_${memstr}_tracer
    ncrename -d yaxis_1,yaxis_2 -v yaxis_1,yaxis_2 fv3sar_tile1_${memstr}_tracer
    # somehow the yaxis_2 get default values, the following line is a temporary workaround
    ncks --no-abc -A -v yaxis_2 fv3sar_tile1_${memstr}_dynvars fv3sar_tile1_${memstr}_tracer
    ncks -A -v $tracer_list fv3sar_tile1_${memstr}_tracer fv3sar_tile1_${memstr}_dynvars
    mv fv3sar_tile1_${memstr}_dynvars fv3sar_tile1_${memstr}_dynvartracer
    rm -f fv3sar_tile1_${memstr}_tracer
EOFprep
    chmod +x ./prep_dynvartracer_ens${memstr}.sh
    echo "./prep_dynvartracer_ens${memstr}.sh" >> cmdfile_prep_dynvartracer_ens
  done
  chmod +x cmdfile_prep_dynvartracer_ens
  ${APRUNC} ${MPISERIAL} -m cmdfile_prep_dynvartracer_ens
else # enkf_recenter
  ${NCP} ${RESTARTens_anl}/ensmean/grid_spec.nc fv3sar_tile1_grid_spec.nc
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_core.res.nc fv3sar_tile1_akbk.nc
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.coupler.res coupler.res
  ${NCP} ${COMhafs}/RESTART_analysis/${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3_dynvars
  ${NCP} ${COMhafs}/RESTART_analysis/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3_tracer
  #dynvar_list="delp,DZ,phis,T,u,ua,v,va,W"
  dynvar_list=$(ncks --trd -m fv3_dynvars | grep -E ': type' | cut -f 1 -d ' ' | sed 's/://' | sort | grep -v 'Time' | grep -v 'axis' | paste -sd "," -)
  #tracer_list="cld_amt,graupel,ice_wat,liq_wat,o3mr,rainwat,snowwat,sphum"
  tracer_list=$(ncks --trd -m fv3_tracer  | grep -E ': type' | cut -f 1 -d ' ' | sed 's/://' | sort | grep -v 'Time' | grep -v 'axis' | paste -sd "," -)
  ncrename -d yaxis_1,yaxis_2 -v yaxis_1,yaxis_2 fv3_tracer
  # somehow the yaxis_2 get default values, the following line is a temporary workaround
  ncks --no-abc -A -v yaxis_2 fv3_dynvars fv3_tracer
  ncks -A -v ${tracer_list} fv3_tracer fv3_dynvars 
  # mem001 from deterministic EnVar analysis
  # The assumption is that the deterministic and ensemble domains are identical but with different grid resolutions
  # Also, the grid spacing ratio between the deterministic and ensemble domains is 1:GRID_RATIO_ENS, where GRID_RATIO_ENS can be 1, 2, 3, 4, 5, etc.
  if [ ${GRID_RATIO_ENS:-1} -eq 1 ]; then
    # the deterministic and ensemble domains are identical and with the same grid resolution
    mv fv3_dynvars fv3sar_tile1_mem001_dynvartracer
  else 
    # subset every GRID_RATIO_ENS grid points to obtain the deterministic EnVar analysis in ensemble resolution
    ncks -d xaxis_1,,,${GRID_RATIO_ENS} -d xaxis_2,,,${GRID_RATIO_ENS} \
         -d yaxis_1,,,${GRID_RATIO_ENS} -d yaxis_2,,,${GRID_RATIO_ENS} \
         fv3_dynvars fv3sar_tile1_mem001_dynvartracer
  fi
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_dynvartracer fv3sar_tile1_ensmean_dynvartracer
  # copy ensemble member files
  rm -f cmdfile
  let "nens0 = $nens"
  let "nens = $nens + 1"
  for imem in $(seq 1 $nens0)
  do
    memchar="mem"$(printf %03i $imem)
    let "memin = $imem + 1"
    meminchar="mem"$(printf %03i $memin)
    echo "${NCP} ${RESTARTens_anl}/${memchar}/anl_dynvartracer_${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3sar_tile1_${meminchar}_dynvartracer" >> cmdfile
  done
  chmod +x cmdfile
  ${APRUNC} ${MPISERIAL} -m cmdfile
fi

if [ $ldo_enscalc_option -eq 0 ]; then # enkf_update
  rm -f cmdfile
  memstr="ensmean"
  RADSTAT=${RESTARTens_anl}/${memstr}/analysis.radstat
  CNVSTAT=${RESTARTens_anl}/${memstr}/analysis.cnvstat
  echo "tar -xvf $RADSTAT" >> cmdfile
  echo "tar -xvf $CNVSTAT" >> cmdfile

  for memstr in $(seq -f "mem%03g" 1 $nens)
  do
    RADSTAT=${RESTARTens_anl}/${memstr}/analysis.radstat
    CNVSTAT=${RESTARTens_anl}/${memstr}/analysis.cnvstat
    echo "tar -xvf $RADSTAT" >> cmdfile
    echo "tar -xvf $CNVSTAT" >> cmdfile
  done

  chmod +x cmdfile
  ${APRUNC} ${MPISERIAL} -m cmdfile
  
  rm -f cmdfile
  for gzfile in $(/bin/ls diag*ges*.gz) 
  do
    echo "gzip -d $gzfile && rm -f $gzfile" >> cmdfile
  done
  chmod +x cmdfile
  ${APRUNC} ${MPISERIAL} -m cmdfile
fi

${NLN} ${PARMgsi}/nam_glb_berror.f77.gcv ./berror_stats
#checkgfs $NLN $RADCLOUDINFO cloudy_radiance_info.txt
${NLN} ${PARMgsi}/atms_beamwidth.txt ./atms_beamwidth.txt
#checkgfs $NLN $vqcdat       vqctp001.dat
#checkgfs $NLN $INSITUINFO   insituinfo
${NLN} ${PARMgsi}/nam_global_pcpinfo.txt ./pcpinfo
#checkgfs $NLN $AEROINFO     aeroinfo
#checkgfs $NLN $HYBENSINFO   hybens_info
${NLN} ${PARMgsi}/hafs_nam_errtable.r3dv ./errtable

if [ $ldo_enscalc_option -eq 1 -o $ldo_enscalc_option -eq 2 ]; then # enkf_mean or enkf_recenter
  anavinfo=${PARMgsi}/hafs_anavinfo.tmp_enkf
else # enkf_update
  anavinfo=${PARMgsi}/hafs_anavinfo.tmp_enkf
fi
#${NCP} ${anavinfo} ./anavinfo
sed -e "s/_LEV_/${npz:-64}/g" \
    -e "s/_LP1_/${LEVS:-65}/g" \
    ${anavinfo} > ./anavinfo

${NLN} ${PARMgsi}/hafs_satinfo.txt ./satinfo
${NLN} ${PARMgsi}/global_scaninfo.txt ./scaninfo
#${NLN} ${PARMgsi}/nam_global_satangbias.txt ./satbias_angle
${NLN} ${PARMgsi}/global_ozinfo.txt ./ozinfo
${NLN} ${PARMgsi}/hafs_convinfo.txt ./convinfo

${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias           satbias_in
${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias_pc        satbias_pc
#${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias_air       satbias_air

# Make enkf namelist
${NCP} ${PARMgsi}/enkf.nml.tmp ./

sed -e "s/_datestring_/${CDATE}/g" \
    -e "s/_nlons_/$((${npx_ens:-$npx}-1))/g" \
    -e "s/_nlats_/$((${npy_ens:-$npy}-1))/g" \
    -e "s/_nlevs_/${npz_ens:-$npz}/g" \
    -e "s/_nanals_/${nens}/g" \
    -e "s/_netcdf_diag_/${netcdf_diag}/g" \
    -e "s/_ldo_enscalc_option_/${ldo_enscalc_option}/g" \
    -e "s/_nx_res_/$((${npx_ens:-$npx}-1))/g" \
    -e "s/_ny_res_/$((${npy_ens:-$npy}-1))/g" \
    enkf.nml.tmp > ./enkf.nml

ENKFEXEC=${ENKFEXEC:-$HOMEhafs/exec/hafs_enkf.x}
cp -p $ENKFEXEC ./enkf.x

# In case there are memory issues to run enkf_mean/enkf_recenter for all
# variables, can consider separating the enkf control variables into three
# parts
#if [ $ldo_enscalc_option -ne 0  ]; then # enkf_mean or enkf_recenter
#  let i=1
#  #for infile in $(/bin/ls ${anavinfo}_p*)
#  for infile in $(/bin/ls ${anavinfo})
#  do
#    #${NCP} $infile anavinfo
#    sed -e "s/_LEV_/${npz:-64}/g" \
#        -e "s/_LP1_/${LEVS:-65}/g" \
#        ${infile} > ./anavinfo
#    ${APRUNC}  ./enkf.x < enkf.nml > stdout_p${i} 2>&1
#    let i=i+1
#  done
#else
#  ${APRUNC} ./enkf.x < enkf.nml > stdout 2>&1
#fi

${APRUNC} ./enkf.x < enkf.nml > stdout 2>&1

if [ $ldo_enscalc_option -eq 0 ]; then # enkf_update
  rm -f cmdfile
  for memstr in $(seq -f "mem%03g" 1 $nens)
  do
    mkdir -p ${RESTARTens_anl}/${memstr}
    echo "${NCP} fv3sar_tile1_${memstr}_dynvartracer ${RESTARTens_anl}/${memstr}/anl_dynvartracer_${PDY}.${cyc}0000.fv_core.res.tile1.nc" >> cmdfile  
  done
  chmod +x cmdfile
  ${APRUNC} ${MPISERIAL} -m cmdfile
elif  [ $ldo_enscalc_option -eq 1 ]; then # enkf_mean 
  memstr="ensmean"
  mkdir -p ${RESTARTens_anl}/${memstr}
  ${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.fv_core.res.tile1.nc ${RESTARTens_anl}/${memstr}/
  ncks -A -v $dynvar_list fv3sar_tile1_mem001_dynvartracer ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_core.res.tile1.nc
  ${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ${RESTARTens_anl}/${memstr}/
  ncks -A -v $tracer_list fv3sar_tile1_mem001_dynvartracer ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
  ncks --no-abc -O -x -v yaxis_2 ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
  ${NCP} fv3sar_tile1_mem001_dynvartracer ${RESTARTens_anl}/${memstr}/${PDY}.${cyc}0000.fv_dynvartracer # used by recenter 
  ${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.coupler.res ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.fv_core.res.nc ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ${RESTARTens_anl}/${memstr}/
  #${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.phy_data.nc ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/${PDY}.${cyc}0000.sfc_data.nc ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/grid_spec.nc ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/oro_data.nc ${RESTARTens_anl}/${memstr}/
  ${NCP} ${RESTARTens_inp}/mem001/atmos_static.nc ${RESTARTens_anl}/${memstr}/
elif [ $ldo_enscalc_option -eq 2 ]; then # enkf_recenter
  mkdir -p ${RESTARTens_anl}/anlmean
  cp ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_core.res.tile1.nc ${RESTARTens_anl}/anlmean/${PDY}.${cyc}0000.fv_core.res.tile1.nc
  ncks -A -v $dynvar_list fv3sar_tile1_mem001_dynvartracer ${RESTARTens_anl}/anlmean/${PDY}.${cyc}0000.fv_core.res.tile1.nc
  cp ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ${RESTARTens_anl}/anlmean/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
  ncks -A -v $tracer_list fv3sar_tile1_mem001_dynvartracer ${RESTARTens_anl}/anlmean/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
  ncks --no_abc -O -x -v yaxis_2 ${RESTARTens_anl}/anlmean/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ${RESTARTens_anl}/anlmean/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
  #cp ${COMhafs}/RESTART_analysis/{*grid_spec.nc,*sfc_data.nc,*coupler.res,gfs_ctrl.nc,fv_core.res.nc,*bndy*} ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.coupler.res ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_core.res.nc ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/${PDY}.${cyc}0000.sfc_data.nc ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/grid_spec.nc ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/oro_data.nc ${RESTARTens_anl}/anlmean/
  ${NCP} ${RESTARTens_anl}/ensmean/atmos_static.nc ${RESTARTens_anl}/anlmean/

  rm -f cmdfile_post_dynvartracer_ens
  for imem in $(seq 2 $nens)
  do
    memstr="mem"$(printf %03i $imem)
    memout="mem"$(printf %03i $(($imem-1)))
    mkdir -p ${RESTARTens_anl}/${memout}
    cat > ./post_dynvartracer_ens${memout}.sh << EOFpost
#!/bin/sh
set -x
    cp ${RESTARTens_inp}/${memout}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ${RESTARTens_anl}/${memout}/${PDY}.${cyc}0000.fv_core.res.tile1.nc
    ncks -A -v $dynvar_list fv3sar_tile1_${memstr}_dynvartracer ${RESTARTens_anl}/${memout}/${PDY}.${cyc}0000.fv_core.res.tile1.nc
    cp ${RESTARTens_inp}/${memout}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ${RESTARTens_anl}/${memout}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
    ncks -A -v $tracer_list fv3sar_tile1_${memstr}_dynvartracer ${RESTARTens_anl}/${memout}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
    ncks --no_abc -O -x -v yaxis_2 ${RESTARTens_anl}/${memout}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ${RESTARTens_anl}/${memout}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
    #cp ${COMhafs}/RESTART_analysis/{*grid_spec.nc,*sfc_data.nc,*coupler.res,gfs_ctrl.nc,fv_core.res.nc,*bndy*} ${RESTARTens_anl}/${memout}/
    ${NCP} ${RESTARTens_inp}/${memout}/${PDY}.${cyc}0000.coupler.res ${RESTARTens_anl}/${memout}/
    ${NCP} ${RESTARTens_inp}/${memout}/${PDY}.${cyc}0000.fv_core.res.nc ${RESTARTens_anl}/${memout}/
    ${NCP} ${RESTARTens_inp}/${memout}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ${RESTARTens_anl}/${memout}/
    ${NCP} ${RESTARTens_inp}/${memout}/${PDY}.${cyc}0000.sfc_data.nc ${RESTARTens_anl}/${memout}/
    ${NCP} ${RESTARTens_inp}/${memout}/grid_spec.nc ${RESTARTens_anl}/${memout}/
    ${NCP} ${RESTARTens_inp}/${memout}/oro_data.nc ${RESTARTens_anl}/${memout}/
    ${NCP} ${RESTARTens_inp}/${memout}/atmos_static.nc ${RESTARTens_anl}/${memout}/
EOFpost
    chmod +x ./post_dynvartracer_ens${memout}.sh 
    echo "./post_dynvartracer_ens${memout}.sh" >> cmdfile_post_dynvartracer_ens
  done
  chmod +x cmdfile_post_dynvartracer_ens
  ${APRUNC} ${MPISERIAL} -m cmdfile_post_dynvartracer_ens
else
  echo "Wrong ldo_enscalc_option: $ldo_enscalc_option"
fi

exit
