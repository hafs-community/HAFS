#!/bin/sh
# exhafs_enkf.sh: Depend upon the value of ldo_enscalc_option, this script can
# perform the EnKF mean, update, and recenter functions by running the GSI's
# enkf.x in different modes.
# ldo_enscalc_option=1: enkf_mean, calculate the ensemble mean
# ldo_enscalc_option=0: enkf_update, condut the EnKF analysis to update the ensemble members
# ldo_enscalc_option=2: enkf_recenter, recenter the ensemble memmber analysis around the deterministic EnVar analysis

set -xe


let "tt = $GDAS_MIX_ENS_SIZE + 1"
TOTAL_TASKS=$tt #${TOTAL_TASKS:-2016}
NCTSK=${NCTSK:-12}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-2}
APRUNC="srun --ntasks=$TOTAL_TASKS --ntasks-per-node=1 --cpus-per-task=40" # ${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}


if [ ${ENSDA} = YES ]; then
  export NHRS=${NHRS_ENS:-126}
  export NBDYHRS=${NBDYHRS_ENS:-3}
  export NOUTHRS=${NOUTHRS_ENS:-3}
  export CASE=${CASE_ENS:-C768}
  export CRES=$(echo $CASE | cut -c 2-)
  export gtype=${gtype_ens:-regional}
  export LEVS=${LEVS_ENS:-65}
else
  exit
fi

export NDATE=${NDATE:-ndate}
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
export RUN_ENVAR=${RUN_ENVAR:-NO}
export ldo_enscalc_option=${ldo_enscalc_option:-1}
export nens=${GDAS_MIX_ENS_SIZE:-40}   # <-- !!
export online_satbias=${online_satbias:-no}
export corrlength=${corrlength:-500}
export lnsigcutoff=${lnsigcutoff:-1.3}
export nesttilestr=${nesttilestr:-""}

export gridstr=${gridstr:-$(echo ${out_gridnames} | cut -d, -f 1)}

#### KKUROSAWA
###if [ ${MIX_DA_FLAG} = YES ]; then          
###  export nens=${SUM_MIX_ENS_SIZE:-40}      
###else
###  export nens=${ENS_SIZE:-40}
###fi
###export nens_HAFS=${ENS_SIZE:-40}           
###export nens_GDAS=${GDAS_MIX_ENS_SIZE:-40}  

# Diagnostic files options
netcdf_diag=${netcdf_diag:-".true."}
binary_diag=${binary_diag:-".false."}

yr=$(echo $CDATE | cut -c1-4)
mn=$(echo $CDATE | cut -c5-6)
dy=$(echo $CDATE | cut -c7-8)

CDATEprior=$(${NDATE} -6 $CDATE)
yrprior=$(echo ${CDATEprior} | cut -c1-4)
mnprior=$(echo ${CDATEprior} | cut -c5-6)
dyprior=$(echo ${CDATEprior} | cut -c7-8)
hhprior=$(echo ${CDATEprior} | cut -c9-10)
cycprior=$(echo ${CDATEprior} | cut -c9-10)
PDYprior=$(echo ${CDATEprior} | cut -c1-8)

export COMhafsprior=${COMhafsprior:-${COMhafs}/../../${CDATEprior}/${STORMID}}
export WORKhafsprior=${WORKhafsprior:-${WORKhafs}/../../${CDATEprior}/${STORMID}}

export RESTARTens_inp=${COMhafsprior}/RESTART_ens
export RESTARTens_inp_gdas=${WORKhafs}/intercom/restart_gdas_ens  # converted gdas
export RESTARTens_anl=${WORKhafs}/intercom/RESTART_analysis_ens/ensmean
export OUT_BASE_DIR=${WORKhafs}/intercom/mix_ens_recenter_ens       # out
export DIAGens_anl=${COMhafs}
mkdir -p ${RESTARTens_anl}
mkdir -p ${DIAGens_anl}

# enkf_recenter
  ${NCP} ${RESTARTens_anl}/grid_spec.nc                                         fv3sar_tile1_grid_spec.nc
  ${NCP} ${RESTARTens_anl}/${PDY}.${cyc}0000.fv_core.res.nc                     fv3sar_tile1_akbk.nc
  ${NCP} ${RESTARTens_anl}/${PDY}.${cyc}0000.coupler.res                        coupler.res
  ${NCP} ${RESTARTens_anl}/${PDY}.${cyc}0000.fv_core.res.tile1.nc   fv3_dynvars
  ${NCP} ${RESTARTens_anl}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3_tracer
  # mem001 from deterministic EnVar analysis
  # The assumption is that the deterministic and ensemble domains are identical but with different grid resolutions
  # Also, the grid spacing ratio between the deterministic and ensemble domains is 1:GRID_RATIO_ENS, where GRID_RATIO_ENS can be 1, 2, 3, 4, 5, etc.
#  if [ ${GRID_RATIO_ENS:-1} -eq 1 ]; then
    # the deterministic and ensemble domains are identical and with the same grid resolution
    mv fv3_dynvars fv3sar_tile1_mem001_dynvars
    mv fv3_tracer fv3sar_tile1_mem001_tracer
#  else
#    # subset every GRID_RATIO_ENS grid points to obtain the deterministic EnVar analysis in ensemble resolution
#    ncks -d xaxis_1,,,${GRID_RATIO_ENS} -d xaxis_2,,,${GRID_RATIO_ENS} \
#         -d yaxis_1,,,${GRID_RATIO_ENS} -d yaxis_2,,,${GRID_RATIO_ENS} \
#         fv3_dynvars fv3sar_tile1_mem001_dynvars
#    ncks -d xaxis_1,,,${GRID_RATIO_ENS} \
#         -d yaxis_1,,,${GRID_RATIO_ENS} \
#         fv3_tracer fv3sar_tile1_mem001_tracer
#  fi
  ${NCP} ${RESTARTens_anl}/${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3sar_tile1_ensmean_dynvars
  ${NCP} ${RESTARTens_anl}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3sar_tile1_ensmean_tracer

  # copy ensemble member files
  rm -f cmdfile
  let "nens0 = $nens"
  let "nens = $nens + 1"
  for imem in $(seq 1 $nens0); do
#    memchar="mem"$(printf %03i $imem)
    let "memin = $imem + 1"
  #  let "memin = $imem + 0"
    meminchar="mem"$(printf %03i $memin)
    gdas_imem=`expr $imem - 0`
    gdas_memstr="mem"$(printf %03i $gdas_imem)
    cat > cmdfile_$meminchar << EOFprep
#!/bin/sh
set -x
      ${NCP} ${RESTARTens_inp_gdas}/${gdas_memstr}/fv_core.res.tile1.nc   fv3sar_tile1_${meminchar}_dynvars
      ${NCP} ${RESTARTens_inp_gdas}/${gdas_memstr}/fv_tracer.res.tile1.nc fv3sar_tile1_${meminchar}_tracer
      ncrename -d yaxis_1,yaxis_2 -v yaxis_1,yaxis_2 fv3sar_tile1_${meminchar}_tracer
      # somehow the yaxis_2 get default values, the following line is a temporary workaround
 #     ncks --no-abc -A -v yaxis_2 fv3sar_tile1_${meminchar}_dynvars  fv3sar_tile1_${meminchar}_tracer
 #     ncks -A -v ${tracer_list} fv3sar_tile1_${meminchar}_tracer fv3sar_tile1_${meminchar}_dynvars
 #     mv fv3sar_tile1_${meminchar}_dynvars fv3sar_tile1_${meminchar}_dynvartracer
 #     rm -f v3sar_tile1_${meminchar}_dynvars fv3sar_tile1_${meminchar}_tracer
EOFprep
  #  echo "${NCP} ${RESTARTens_anl}/${memchar}/${PDY}.${cyc}0000.fv_core.res.tile1.nc fv3sar_tile1_${meminchar}_dynvars" >> cmdfile
  #  echo "${NCP} ${RESTARTens_anl}/${memchar}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc fv3sar_tile1_${meminchar}_tracer" >> cmdfile
    chmod +x cmdfile_$meminchar
    echo "./cmdfile_$meminchar" >> cmdfile
  done
  chmod +x cmdfile
  ${APRUNC} ${MPISERIAL} -m cmdfile
##fi

${NLN} ${PARMgsi}/nam_glb_berror.f77.gcv ./berror_stats
#checkgfs $NLN $RADCLOUDINFO cloudy_radiance_info.txt
${NLN} ${PARMgsi}/atms_beamwidth.txt ./atms_beamwidth.txt
#checkgfs $NLN $vqcdat       vqctp001.dat
#checkgfs $NLN $INSITUINFO   insituinfo
${NLN} ${PARMgsi}/nam_global_pcpinfo.txt ./pcpinfo
#checkgfs $NLN $AEROINFO     aeroinfo
#checkgfs $NLN $HYBENSINFO   hybens_info
${NLN} ${PARMgsi}/hafs_nam_errtable.r3dv ./errtable

#if [ $ldo_enscalc_option -eq 1 -o $ldo_enscalc_option -eq 2 ]; then # enkf_mean or enkf_recenter
  anavinfo=${PARMgsi}/hafs_anavinfo.tmp_enkf
#else # enkf_update
#  anavinfo=${PARMgsi}/hafs_anavinfo.tmp
#fi
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
if [ ${online_satbias} = "yes" ] && [ ${RUN_ENVAR} = "YES" ]; then
  PASSIVE_BC=.true.
  UPD_PRED=1
  if [ ! -s ${COMhafsprior}/${old_out_prefix}.${RUN}.${gridstr}.analysis.abias ] || [ ! -s ${COMhafsprior}/${old_out_prefix}.${RUN}.${gridstr}.analysis.abias_pc ]; then
    echo "Prior cycle satbias data does not exist. Grabbing satbias data from GDAS"
    ${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias           satbias_in
    ${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias_pc        satbias_pc
  elif [ -s ${COMhafsprior}/${old_out_prefix}.${RUN}.${gridstr}.analysis.abias ] && [ -s ${COMhafsprior}/${old_out_prefix}.${RUN}.${gridstr}.analysis.abias_pc ]; then
    ${NLN} ${COMhafsprior}/${old_out_prefix}.${RUN}.${gridstr}.analysis.abias            satbias_in
    ${NLN} ${COMhafsprior}/${old_out_prefix}.${RUN}.${gridstr}.analysis.abias_pc         satbias_pc
  else
    echo "ERROR: Either source satbias_in or source satbias_pc does not exist. Exiting script."
    exit 2
  fi
elif [ ${online_satbias} = "yes" ] && [ ${RUN_ENVAR} = "NO" ]; then
  echo "ERROR: Cannot run online satbias correction without EnVar. Exiting script."
  exit 2
else
  PASSIVE_BC=.false.
  UPD_PRED=0
  ${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias           satbias_in
  ${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias_pc        satbias_pc
fi
#
#${NLN} ${COMgfs}/gdas.$PDYprior/${hhprior}/${atmos}gdas.t${hhprior}z.abias_air       satbias_air

ldo_enscalc_option=2  #recenter

# Make enkf namelist
${NCP} ${PARMgsi}/enkf.nml.tmp ./

sed -e "s/_datestring_/${CDATE}/g" \
    -e "s/_corrlength_/${corrlength:-500}/g" \
    -e "s/_lnsigcutoff_/${lnsigcutoff:-1.3}/g" \
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
${NCP} -p $ENKFEXEC ./enkf.x
#${APRUNC} ./enkf.x < enkf.nml > stdout 2>&1
set -o pipefail
${APRUNC} ./enkf.x < enkf.nml 2>&1 | tee stdout
set +o pipefail

OUTDIR_MEAN=${OUT_BASE_DIR}/ensmean
rm -rf $OUTDIR_MEAN
mkdir -p $OUTDIR_MEAN
${NCP} fv3sar_tile1_mem001_dynvars ${OUTDIR_MEAN}/${PDY}.${cyc}0000.fv_core.res.tile1.nc
${NCP} fv3sar_tile1_mem001_tracer ${OUTDIR_MEAN}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
${NCP} ${RESTARTens_anl}/${PDY}.${cyc}0000.coupler.res             ${OUTDIR_MEAN}
${NCP} ${RESTARTens_anl}/${PDY}.${cyc}0000.fv_core.res.nc          ${OUTDIR_MEAN}
${NCP} ${RESTARTens_anl}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ${OUTDIR_MEAN}
${NCP} ${RESTARTens_anl}/${PDY}.${cyc}0000.sfc_data.nc             ${OUTDIR_MEAN}
${NCP} ${RESTARTens_anl}/grid_spec.nc                              ${OUTDIR_MEAN}
${NCP} ${RESTARTens_anl}/oro_data.nc                               ${OUTDIR_MEAN}
${NCP} ${RESTARTens_anl}/atmos_static.nc                           ${OUTDIR_MEAN}

rm -f cmdfile_post_dynvartracer_ens
for imem in $(seq 2 $nens); do
  memstr="mem"$(printf %03i $imem)
  memout="mem"$(printf %03i $(($imem-1)))
  OUTDIR_MEM=$OUT_BASE_DIR/${memout}
#  mkdir -p ${RESTARTens_anl}/${memout}
  rm -rf $OUTDIR_MEM
  mkdir -p $OUTDIR_MEM
  gdas_imem=`expr $imem - 1`
  gdas_memstr="mem"$(printf %03i $gdas_imem)
  tmp_memout="mem001" # <-- !!
   cat > ./post_dynvartracer_ens${memout}.sh << EOFpost
#!/bin/sh
set -x
    ${NCP} fv3sar_tile1_${memstr}_dynvars           $OUTDIR_MEM/${PDY}.${cyc}0000.fv_core.res.tile1.nc
    ${NCP} fv3sar_tile1_${memstr}_tracer         $OUTDIR_MEM/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc
    ${NCP} ${RESTARTens_inp_gdas}/${gdas_memstr}/fv_core.res.nc             $OUTDIR_MEM/${PDY}.${cyc}0000.fv_core.res.nc
    ${NCP} ${RESTARTens_inp_gdas}/${gdas_memstr}/out.sfc.tile7.nc           $OUTDIR_MEM/${PDY}.${cyc}0000.sfc_data.nc
    ${NCP} ${RESTARTens_inp}/${tmp_memout}/grid_spec.nc                         $OUTDIR_MEM     # <-- !!
    ${NCP} ${RESTARTens_inp}/${tmp_memout}/oro_data.nc                          $OUTDIR_MEM     # <-- !!
    ${NCP} ${RESTARTens_inp}/${tmp_memout}/atmos_static.nc                      $OUTDIR_MEM     # <-- !!
    ${NCP} ${RESTARTens_inp}/${tmp_memout}/${PDY}.${cyc}0000.coupler.res        $OUTDIR_MEM     # <-- !!
#    ${NCP} ${RESTARTens_inp}/${memout}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc $OUTDIR_MEM/ # <-- !!
EOFpost
  chmod +x ./post_dynvartracer_ens${memout}.sh
  echo "./post_dynvartracer_ens${memout}.sh" >> cmdfile_post_dynvartracer_ens
done
chmod +x cmdfile_post_dynvartracer_ens
${APRUNC} ${MPISERIAL} -m cmdfile_post_dynvartracer_ens

exit
