#!/bin/sh

set -xe
export vi_warm_start_vmax_threshold=$(printf "%.0f" ${vi_warm_start_vmax_threshold:-20}) # m/s
export vi_bogus_vmax_threshold=$(printf "%.0f" ${vi_bogus_vmax_threshold:-50}) # m/s
export vi_storm_env=${vi_storm_env:-init} # init: from gfs/gdas init; pert: from the same source for the storm perturbation
export vi_storm_relocation=${vi_storm_relocation:-yes}
export vi_storm_modification=${vi_storm_modification:-yes}
export vi_ajust_intensity=${vi_adjust_intensity:-yes}
export vi_ajust_size=${vi_adjust_size:-yes}
export crfactor=${crfactor:-1.0}

CDATE=${CDATE:-$YMDH}
FGAT_MODEL=${FGAT_MODEL:-gfs}
FGAT_HR=${FGAT_HR:-00}

MERGE_TYPE=${MERGE_TYPE:-vortexreplace}

TOTAL_TASKS=${TOTAL_TASKS:-24}
NCTSK=${NCTSK:-24}
NCNODE=${NCNODE:-1}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}

# Utilities
NDATE=${NDATE:-ndate}
export NCP=${NCP:-"/bin/cp"}
export NMV=${NMV:-"/bin/mv"}
export NLN=${NLN:-"/bin/ln -sf"}
export MPISERIAL=${MPISERIAL:-${EXEChafs}/hafs_mpiserial.x}
export DATOOL=${DATOOL:-${EXEChafs}/hafs_datool.x}

PDY=`echo $CDATE | cut -c1-8`
cyc=`echo $CDATE | cut -c9-10`
yr=`echo $CDATE | cut -c1-4`
mn=`echo $CDATE | cut -c5-6`
dy=`echo $CDATE | cut -c7-8`
hh=`echo $CDATE | cut -c9-10`

export RESTARTinp=${RESTARTinp:-${COMhafsprior}/RESTART}
export RESTARTmrg=${RESTARTmrg:-${WORKhafs}/intercom/RESTART_merge}
export INTCOMinit=${INTCOMinit:-${WORKhafs}/intercom/atm_init}
export RESTARTinit=${RESTARTinit:-${WORKhafs}/intercom/RESTART_init}
export RESTARTout=${RESTARTout:-${WORKhafs}/intercom/RESTART_vi}
export DATA=${DATA:-${WORKhafs}/atm_vi}

cd $DATA

if [ ${FGAT_HR} = 03 ]; then
  cp ${WORKhafs}/tm03vit tcvitals.vi
# cp ${WORKhafs}/tmpvit tcvitals.vi
  gesfhr=3
elif [ ${FGAT_HR} = 06 ]; then
  cp ${WORKhafs}/tmpvit tcvitals.vi
  gesfhr=6
elif [ ${FGAT_HR} = 09 ]; then
  cp ${WORKhafs}/tp03vit tcvitals.vi
# cp ${WORKhafs}/tmpvit tcvitals.vi
  gesfhr=9
else
  cp ${WORKhafs}/tmpvit tcvitals.vi
  gesfhr=6
fi

basin=${pubbasin2:-AL}
tcvital=${DATA}/tcvitals.vi
vmax_vit=`cat ${tcvital} | cut -c68-69`

#===============================================================================
# Stage 1: Process prior cycle's vortex if exists and storm intensity is
# stronger than vi_warm_start_vmax_threshold (e.g., 20 m/s)

if [[ ${vmax_vit} -ge ${vi_warm_start_vmax_threshold} ]] && [ -d ${RESTARTinp} ]; then

  # prep
  work_dir=${DATA}/prep_guess
  mkdir -p ${work_dir}
  cd ${work_dir}
  vortexradius=30
  res=0.02
  ${APRUNS} ${DATOOL} hafsvi_preproc --in_dir=${RESTARTinp} \
                                     --debug_level=11 --interpolation_points=5 \
                                     --infile_date=${CDATE:0:8}.${CDATE:8:2}0000 \
                                     --tcvital=${tcvital} \
                                     --vortexradius=${vortexradius} --res=${res} \
                                     --nestdoms=$((${nest_grids:-1}-1)) \
                                     --out_file=vi_inp_${vortexradius}deg${res/\./p}.bin
#                                    [--vortexposition=vortex_position ]
#                                    [--debug_level=10 (default is 1) ]
#                                    [--interpolation_points=5 (default is 4, range 1-500) ]
  if [[ ${nest_grids} -gt 1 ]]; then
    mv vi_inp_${vortexradius}deg${res/\./p}.bin vi_inp_${vortexradius}deg${res/\./p}.bin_grid01
    mv vi_inp_${vortexradius}deg${res/\./p}.bin_nest$(printf "%02d" ${nest_grids}) vi_inp_${vortexradius}deg${res/\./p}.bin
  fi

  vortexradius=45
  res=0.20
  ${APRUNS} ${DATOOL} hafsvi_preproc --in_dir=${RESTARTinp} \
                                     --debug_level=11 --interpolation_points=5 \
                                     --infile_date=${CDATE:0:8}.${CDATE:8:2}0000 \
                                     --tcvital=${tcvital} \
                                     --vortexradius=${vortexradius} --res=${res} \
                                     --nestdoms=$((${nest_grids:-1}-1)) \
                                     --out_file=vi_inp_${vortexradius}deg${res/\./p}.bin
  if [[ ${nest_grids} -gt 1 ]]; then
    mv vi_inp_${vortexradius}deg${res/\./p}.bin vi_inp_${vortexradius}deg${res/\./p}.bin_grid01
    mv vi_inp_${vortexradius}deg${res/\./p}.bin_nest$(printf "%02d" ${nest_grids}) vi_inp_${vortexradius}deg${res/\./p}.bin
  fi

  # create_trak and split
  work_dir=${DATA}/split_guess
  mkdir -p ${work_dir}
  cd ${work_dir}
  # input
  ln -sf ${tcvital} fort.11
  if [ -e ${COMhafsprior}/${STORMID,,}.${CDATEprior}.hafs.trak.atcfunix.all ]; then
    ln -sf ${COMhafsprior}/${STORMID,,}.${CDATEprior}.hafs.trak.atcfunix.all ./trak.atcfunix.all
    grep "^${basin^^}, ${STORMID:0:2}," trak.atcfunix.all \
      > trak.atcfunix.tmp
    # | grep -E "^${STORMBS1^^}.,|^.${STORMBS1^^}," \
    # | grep -E "HAFS, 00.," > trak.atcfunix.tmp
  else
    touch trak.atcfunix.tmp
  fi
  # get vmax in kt then convert into m/s
  vmax_guess=$(grep "^${basin^^}, ${STORMID:0:2}, ${CDATEprior}, .., ...., 00${gesfhr}," trak.atcfunix.tmp | grep "34, NEQ," | cut -c48-51)
  vmax_guess=${vmax_guess:-0}
  vmax_guess=$( printf "%.0f" $(bc <<< "scale=6; ${vmax_guess}*0.514444") )
  # calculate the abs difference
  vdif_guess=$(( ${vmax_guess}-${vmax_vit} ))
  vdif_guess="${vdif_guess#-}"

  ln -sf trak.atcfunix.tmp fort.12
  # output
  ln -sf ./trak.fnl.all fort.30

  ln -sf ${EXEChafs}/hafs_vi_create_trak_guess.x ./
  ${APRUNS} ./hafs_vi_create_trak_guess.x ${STORMID}

  # split
  # input
  ln -sf ${tcvital} fort.11
  ln -sf ./trak.fnl.all fort.30
  ln -sf ../prep_guess/vi_inp_30deg0p02.bin ./fort.26
  ln -sf ../prep_guess/vi_inp_45deg0p20.bin ./fort.46
  # output
  ln -sf storm_env                     fort.56
  ln -sf rel_inform                    fort.52
  ln -sf vital_syn                     fort.55
  ln -sf storm_pert                    fort.71
  ln -sf storm_radius                  fort.85

  ln -sf ${EXEChafs}/hafs_vi_split.x ./
  gesfhr=${gesfhr:-6}
  ibgs=0
  iflag_cold=0
  crfactor=${crfactor:-1.0}
  echo ${gesfhr} $ibgs $vmax_vit $iflag_cold $crfactor | ${APRUNS} ./hafs_vi_split.x

  # anl_pert
  work_dir=${DATA}/anl_pert_guess
  mkdir -p ${work_dir}
  cd ${work_dir}
  # input
  ln -sf ${tcvital} fort.11
  ln -sf ../split_guess/storm_env fort.26
  ln -sf ../prep_guess/vi_inp_30deg0p02.bin fort.46
  ln -sf ../split_guess/storm_pert fort.71
  ln -sf ../split_guess/storm_radius fort.65
  # output
  ln -sf storm_pert_new fort.58
  ln -sf storm_size_p fort.14
  ln -sf storm_sym fort.23

  ln -sf ${EXEChafs}/hafs_vi_anl_pert.x ./
  if [ ${vi_storm_modification} = auto ]; then
    # Conduct storm modification only if vdif >= 5 m/s or >= 15% of vmax_vit
    if [[ ${vdif_guess} -ge 5 ]] || [[ ${vdif_guess} -ge $( printf "%.0f" $(bc <<< "scale=6; ${vmax_vit}*0.15") ) ]]; then
      initopt=0
    else
      initopt=1
    fi
  elif [ ${vi_storm_modification} = vmax_threshold ]; then
    # Conduct storm modification only if vmax >= 30 m/s or vmax_vit > 30 m/s
    if [[ ${vmax_guess} -ge 30 ]] || [[ ${vmax_vit} -ge 30 ]]; then
      initopt=0
    else
      initopt=1
    fi
  elif [ ${vi_storm_modification} = no ]; then
    initopt=1
  elif [ ${vi_storm_modification} = yes ]; then
    initopt=0
  else
    initopt=0
  fi
  initopt_guess=${initopt}
  echo 6 ${basin} ${initopt} | ${APRUNS} ./hafs_vi_anl_pert.x

fi
#===============================================================================
# Stage 2: Process current cycle's vortex from the global/parent model

cd $DATA

  # prep
  work_dir=${DATA}/prep_init
  mkdir -p ${work_dir}
  cd ${work_dir}
  vortexradius=30
  res=0.02
  ${APRUNS} ${DATOOL} hafsvi_preproc --in_dir=${RESTARTinit} \
                                     --debug_level=11 --interpolation_points=5 \
                                     --infile_date=${CDATE:0:8}.${CDATE:8:2}0000 \
                                     --tcvital=${tcvital} \
                                     --vortexradius=${vortexradius} --res=${res} \
                                     --nestdoms=$((${nest_grids:-1}-1)) \
                                     --out_file=vi_inp_${vortexradius}deg${res/\./p}.bin
  if [[ ${nest_grids} -gt 1 ]]; then
    mv vi_inp_${vortexradius}deg${res/\./p}.bin vi_inp_${vortexradius}deg${res/\./p}.bin_grid01
    mv vi_inp_${vortexradius}deg${res/\./p}.bin_nest$(printf "%02d" ${nest_grids}) vi_inp_${vortexradius}deg${res/\./p}.bin
  fi
  vortexradius=45
  res=0.20
  ${APRUNS} ${DATOOL} hafsvi_preproc --in_dir=${RESTARTinit} \
                                     --debug_level=11 --interpolation_points=5 \
                                     --infile_date=${CDATE:0:8}.${CDATE:8:2}0000 \
                                     --tcvital=${tcvital} \
                                     --vortexradius=${vortexradius} --res=${res} \
                                     --nestdoms=$((${nest_grids:-1}-1)) \
                                     --out_file=vi_inp_${vortexradius}deg${res/\./p}.bin
  if [[ ${nest_grids} -gt 1 ]]; then
    mv vi_inp_${vortexradius}deg${res/\./p}.bin vi_inp_${vortexradius}deg${res/\./p}.bin_grid01
    mv vi_inp_${vortexradius}deg${res/\./p}.bin_nest$(printf "%02d" ${nest_grids}) vi_inp_${vortexradius}deg${res/\./p}.bin
  fi

  # create_trak and split
  work_dir=${DATA}/split_init
  mkdir -p ${work_dir}
  cd ${work_dir}
  # input
  ln -sf ${tcvital} fort.11
  if [ -e ${INTCOMinit}/${STORMID,,}.${CDATE}.hafs.trak.atcfunix.all ]; then
    ln -sf ${INTCOMinit}/${STORMID,,}.${CDATE}.hafs.trak.atcfunix.all ./trak.atcfunix.all
    grep "^${basin^^}, ${STORMID:0:2}," trak.atcfunix.all \
      > trak.atcfunix.tmp
    # | grep -E "^${STORMBS1^^}.,|^.${STORMBS1^^}," \
    # | grep -E "HAFS, 00.," > trak.atcfunix.tmp
  else
    touch trak.atcfunix.tmp
  fi
  # get vmax in kt then convert into m/s
  vmax_init=$(grep "^${basin^^}, ${STORMID:0:2}, ${CDATE}, .., ...., 000," trak.atcfunix.tmp | grep "34, NEQ," | cut -c48-51)
  vmax_init=${vmax_init:-0}
  vmax_init=$( printf "%.0f" $(bc <<< "scale=6; ${vmax_init}*0.514444") )
  # calculate the abs difference
  vdif_init=$(( ${vmax_init}-${vmax_vit} ))
  vdif_init="${vdif_init#-}"

  ln -sf trak.atcfunix.tmp fort.12
  # output
  ln -sf ./trak.fnl.all fort.30

  ln -sf ${EXEChafs}/hafs_vi_create_trak_init.x ./
  ${APRUNS} ./hafs_vi_create_trak_init.x ${STORMID}

  # split
  # input
  ln -sf ${tcvital} fort.11
  ln -sf ./trak.fnl.all fort.30
  ln -sf ../prep_init/vi_inp_30deg0p02.bin ./fort.26
  ln -sf ../prep_init/vi_inp_45deg0p20.bin ./fort.46
  if [ -s ../split_guess/storm_radius ]; then
    ln -sf ../split_guess/storm_radius ./fort.65
  fi
  # output
  ln -sf storm_env                     fort.56
  ln -sf rel_inform                    fort.52
  ln -sf vital_syn                     fort.55
  ln -sf storm_pert                    fort.71
  ln -sf storm_radius                  fort.85

  ln -sf ${EXEChafs}/hafs_vi_split.x ./
  gesfhr=${gesfhr:-6}
  # Warm start or cold start
  if [ -s fort.65 ]; then
    ibgs=1
    iflag_cold=0
  else
    ibgs=2
    iflag_cold=1
  fi
  echo ${gesfhr} $ibgs $vmax_vit $iflag_cold 1.0 | ${APRUNS} ./hafs_vi_split.x

  # anl_pert
  work_dir=${DATA}/anl_pert_init
  mkdir -p ${work_dir}
  cd ${work_dir}
  # input
  ln -sf ${tcvital} fort.11
  ln -sf ../split_init/storm_env fort.26
  ln -sf ../prep_init/vi_inp_30deg0p02.bin fort.46
  ln -sf ../split_init/storm_pert fort.71
  ln -sf ../split_init/storm_radius fort.65
  # output
  ln -sf storm_pert_new fort.58
  ln -sf storm_size_p fort.14
  ln -sf storm_sym fort.23

  ln -sf ${EXEChafs}/hafs_vi_anl_pert.x ./
  if [ ${vi_storm_modification} = auto ]; then
    # Conduct storm modification only if vdif >= 5 m/s or >= 15% of vmax_vit
    if [[ ${vdif_init} -ge 5 ]] || [[ ${vdif_init} -ge $( printf "%.0f" $(bc <<< "scale=6; ${vmax_vit}*0.15") ) ]]; then
      initopt=0
    else
      initopt=1
    fi
  elif [ ${vi_storm_modification} = vmax_threshold ]; then
    # Conduct storm modification only if vmax >= 30 m/s or vmax_vit > 30 m/s
    if [[ ${vmax_init} -ge 30 ]] || [[ ${vmax_vit} -ge 30 ]]; then
      initopt=0
    else
      initopt=1
    fi
  elif [ ${vi_storm_modification} = no ]; then
    initopt=1
  elif [ ${vi_storm_modification} = yes ]; then
    initopt=0
  else
    initopt=0
  fi
  initopt_init=${initopt}
  echo 6 ${basin} ${initopt} | ${APRUNS} ./hafs_vi_anl_pert.x

#===============================================================================
# Stage 3:

  # anl_storm
  work_dir=${DATA}/anl_storm
  mkdir -p ${work_dir}
  cd ${work_dir}

if [[ ${vmax_vit} -ge ${vi_bogus_vmax_threshold} ]] && [ ! -s ../anl_pert_guess/storm_pert_new ] ; then
  # Bogus a storm if prior cycle does not exist and tcvital intensity >= vi_bogus_vmax_threshold (e.g., 33 m/s)

  pert=init
  senv=$pert
  # anl_bogus
  # input
  ln -sf ${tcvital} fort.11
  ln -sf ../split_${senv}/storm_env fort.26
  ln -sf ../prep_${pert}/vi_inp_30deg0p02.bin ./fort.36
  ln -sf ../prep_${pert}/vi_inp_30deg0p02.bin ./fort.46 #roughness
  ln -sf ../split_${pert}/storm_pert fort.61
  ln -sf ../split_${pert}/storm_radius fort.85

  ln -sf ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.71
  ln -sf ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.72
  ln -sf ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.73
  ln -sf ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.74
  ln -sf ${FIXhafs}/fix_vi/hafs_storm_30       fort.75
  ln -sf ${FIXhafs}/fix_vi/hafs_storm_30       fort.76
  ln -sf ${FIXhafs}/fix_vi/hafs_storm_30       fort.77
  ln -sf ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.78

  # output
  ln -sf storm_anl_bogus                        fort.56

  ln -sf ${EXEChafs}/hafs_vi_anl_bogus.x ./
  echo 6 ${basin} | ${APRUNS} ./hafs_vi_anl_bogus.x
  cp -p storm_anl_bogus storm_anl

else
  # warm-start from prior cycle or cold start from global/parent model

  # anl_combine
  if [[ ${vmax_vit} -ge ${vi_warm_start_vmax_threshold} ]] && [ -s ../anl_pert_guess/storm_pert_new ] ; then
    pert=guess
    initopt=${initopt_guess}
  else
    pert=init
    initopt=${initopt_init}
  fi
  if [ $vi_storm_env = init ] ; then
    senv=init
  else
    senv=$pert
  fi
  if [ $pert = init ] ; then
    gfs_flag=0
  else
    gfs_flag=6
  fi
  if [ $ENSDA = YES ]; then
    gfs_flag=1
  fi

  rm -f flag_file
  # input
  ln -sf ${tcvital} fort.11
  ln -sf ../split_${pert}/trak.atcfunix.tmp fort.12
  ln -sf ../split_${pert}/trak.fnl.all fort.30
  ln -sf ../anl_pert_${pert}/storm_size_p fort.14
  ln -sf ../anl_pert_${pert}/storm_sym fort.23
  ln -sf ../anl_pert_${pert}/storm_pert_new fort.71
  ln -sf ../split_${senv}/storm_env fort.26
  ln -sf ../prep_${pert}/vi_inp_30deg0p02.bin ./fort.46 #roughness

  # output
  ln -sf storm_env_new                 fort.36
  ln -sf storm_anl_combine             fort.56

  gesfhr=${gesfhr:-6}
  gfs_flag=${gfs_flag:-6}

  ln -sf ${EXEChafs}/hafs_vi_anl_combine.x ./
  echo ${gesfhr} ${basin} ${gfs_flag} ${initopt} | ${APRUNS} ./hafs_vi_anl_combine.x
  if [ -s storm_anl_combine ]; then
    cp -p storm_anl_combine storm_anl
  fi

  # If the combined storm is weaker than the tcvital intensity, add a small
  # fraction of the composite storm to enhance the storm intensity so that it
  # matches the tcvital intensity
  if [ -s flag_file ] && [ -s storm_env_new ]; then
  # anl_enhance
  # input
 #ln -sf ${tcvital} fort.11
 #ln -sf ./flag_file flag_file
  ln -sf ../anl_pert_${pert}/storm_sym fort.23
  ln -sf storm_env_new fort.26
  ln -sf ../prep_${pert}/vi_inp_30deg0p02.bin ./fort.46 #roughness
  ln -sf ../split_${pert}/storm_radius fort.85

  ln -sf ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.71
  ln -sf ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.72
  ln -sf ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.73
  ln -sf ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.74
  ln -sf ${FIXhafs}/fix_vi/hafs_storm_30       fort.75
  ln -sf ${FIXhafs}/fix_vi/hafs_storm_30       fort.76
  ln -sf ${FIXhafs}/fix_vi/hafs_storm_30       fort.77
  ln -sf ${FIXhafs}/fix_vi/hafs_storm_axisy_47 fort.78

  # output
  ln -sf storm_anl_enhance                     fort.56

  iflag_cold=${iflag_cold:-0}
  ln -sf ${EXEChafs}/hafs_vi_anl_enhance.x ./
  echo 6 ${basin} ${iflag_cold} | ${APRUNS} ./hafs_vi_anl_enhance.x
  cp -p storm_anl_enhance storm_anl

  fi

fi

if [ ! -s storm_anl ]; then
  echo "FATAL ERROR: Stage3 did not produce storm_anl"
  exit 1
fi

# Interpolate storm_anl back to HAFS restart files
cd $DATA

# post
mkdir -p ${RESTARTout}
if [ $senv = init ] ; then
  RESTARTdst=${RESTARTinit}
elif [ -d ${RESTARTmrg} ] ; then
  RESTARTdst=${RESTARTmrg}
else
  RESTARTdst=${RESTARTinp}
fi
${NCP} -rp ${RESTARTdst}/${CDATE:0:8}.${CDATE:8:2}0000* ${RESTARTout}/
${NCP} -rp ${RESTARTdst}/atmos_static*.nc ${RESTARTout}/
${NCP} -rp ${RESTARTdst}/grid_*spec*.nc ${RESTARTout}/
${NCP} -rp ${RESTARTdst}/oro_data*.nc ${RESTARTout}/

for nd in $(seq 1 ${nest_grids})
do

${APRUNS} ${DATOOL} hafsvi_postproc --in_file=${DATA}/anl_storm/storm_anl \
                               --debug_level=11 --interpolation_points=5 \
                               --relaxzone=30 \
                               --infile_date=${CDATE:0:8}.${CDATE:8:2}0000 \
                               --nestdoms=$((${nd}-1)) \
                               --out_dir=${RESTARTout}
#                              [--relaxzone=50 (grids, default is 30) ]
#                              [--debug_level=10 (default is 1) ]
#                              [--interpolation_points=5 (default is 4, range 1-500) ]

done
#===============================================================================

exit
