#!/bin/sh

set -xe

NCP=${NCP:-'/bin/cp'}
NLN=${NLN:-'/bin/ln -sf'}
NDATE=${NDATE:-ndate}
NSED=${NSED:-'/usr/bin/sed'}

TOTAL_TASKS=${TOTAL_TASKS:-2016}
NCTSK=${NCTSK:-12}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-2}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}

CDATE=${CDATE:-${YMDH}}
cyc=${cyc:-00}
STORM=${STORM:-FAKE}
STORMID=${STORMID:-00L}

CDATEprior=`${NDATE} -6 $CDATE`
COMhafsprior=${COMhafsprior:-${COMhafs}/../../${CDATEprior}/${STORMID}}
WORKhafsprior=${WORKhafsprior:-${WORKhafs}/../../${CDATEprior}/${STORMID}}

PARMforecast=${PARMforecast:-${PARMhafs}/forecast/regional}
PARMhycom=${PARMhycom:-${PARMhafs}/hycom/regional}
PARMww3=${PARMww3:-${PARMhafs}/ww3/regional}
FIXam=${FIXam:-${FIXhafs}/fix_am}
FIXcrtm=${FIXcrtm:-${FIXhafs}/hafs-crtm-2.3.0}
FIXhycom=${FIXhycom:-${FIXhafs}/fix_hycom}
FORECASTEXEC=${FORECASTEXEC:-${EXEChafs}/hafs_forecast.x}

out_prefix=${out_prefix:-$(echo "${STORM}${STORMID}.${YMDH}" | tr '[A-Z]' '[a-z]')}
satpost=${satpost:-.false.}

ENSDA=${ENSDA:-NO}

# Set options specific to the deterministic/ensemble forecast
if [ "${ENSDA}" != YES ]; then
  NHRS=${NHRS:-126}
  NBDYHRS=${NBDYHRS:-3}
  NOUTHRS=${NOUTHRS_ENS:-3}
  CASE=${CASE:-C768}
  CRES=$(echo $CASE | cut -c 2-)
  gtype=${gtype:-regional}
  LEVS=${LEVS:-65}
  stretch_fac=${stretch_fac:-1.0001}
  target_lon=${target_lon:--62.0}
  target_lat=${target_lat:-22.0}
  refine_ratio=${refine_ratio:-4}
  deflate_level=${deflate_level:--1}
  ccpp_suite_regional=${ccpp_suite_regional:-FV3_HAFS_v1}
  ccpp_suite_glob=${ccpp_suite_glob:-FV3_HAFS_v1}
  ccpp_suite_nest=${ccpp_suite_nest:-FV3_HAFS_v1}
  dt_atmos=${dt_atmos:-90}
  restart_interval=${restart_interval:-6}
  quilting=${quilting:-.true.}
  write_groups=${write_groups:-3}
  write_tasks_per_group=${write_tasks_per_group:-72}
  write_dopost=${write_dopost:-.false.}
  output_history=${output_history:-.true.}
  glob_k_split=${glob_k_split:-1}
  glob_n_split=${glob_n_split:-7}
  glob_layoutx=${glob_layoutx:-12}
  glob_layouty=${glob_layouty:-12}
  glob_npx=${glob_npx:-769}
  glob_npy=${glob_npy:-769}
  k_split=${k_split:-4}
  n_split=${n_split:-5}
  layoutx=${layoutx:-40}
  layouty=${layouty:-30}
  npx=${npx:-2881}
  npy=${npy:-1921}
  npz=${npz:-64}
  output_grid_dlon=${output_grid_dlon:-0.025}
  output_grid_dlat=${output_grid_dlon:-0.025}
else
  NHRS=${NHRS_ENS:-6}
  NBDYHRS=${NBDYHRS_ENS:-3}
  NOUTHRS=${NOUTHRS_ENS:-3}
  CASE=${CASE_ENS:-C768}
  CRES=$(echo $CASE | cut -c 2-)
  gtype=${gtype_ens:-regional}
  LEVS=${LEVS_ENS:-65}
  stretch_fac=${stretch_fac_ens:-1.0001}
  target_lon=${target_lon_ens:--62.0}
  target_lat=${target_lat_ens:-22.0}
  refine_ratio=${refine_ratio_ens:-4}
  deflate_level=${deflate_level:-1}
  ccpp_suite_regional=${ccpp_suite_regional_ens:-FV3_HAFS_v1}
  ccpp_suite_glob=${ccpp_suite_glob_ens:-FV3_HAFS_v1}
  ccpp_suite_nest=${ccpp_suite_nest_ens:-FV3_HAFS_v1}
  dt_atmos=${dt_atmos_ens:-90}
  restart_interval=${restart_interval_ens:-6}
  quilting=${quilting_ens:-.true.}
  write_groups=${write_groups_ens:-3}
  write_tasks_per_group=${write_tasks_per_group_ens:-72}
  write_dopost=${write_dopost_ens:-.false.}
  output_history=${output_history_ens:-.true.}
  glob_k_split=${glob_k_split_ens:-1}
  glob_n_split=${glob_n_split_ens:-7}
  glob_layoutx=${glob_layoutx_ens:-12}
  glob_layouty=${glob_layouty_ens:-12}
  glob_npx=${glob_npx_ens:-769}
  glob_npy=${glob_npy_ens:-769}
  k_split=${k_split_ens:-4}
  n_split=${n_split_ens:-5}
  layoutx=${layoutx_ens:-40}
  layouty=${layouty_ens:-30}
  npx=${npx_ens:-2881}
  npy=${npy_ens:-1921}
  npz=${npz_ens:-64}
  output_grid_dlon_ens=${output_grid_dlon_ens:-$(awk "BEGIN {print ${output_grid_dlon:-0.025}*${GRID_RATIO_ENS:-1}}")}
  output_grid_dlat_ens=${output_grid_dlat_ens:-$(awk "BEGIN {print ${output_grid_dlat:-0.025}*${GRID_RATIO_ENS:-1}}")}
  output_grid_dlon=${output_grid_dlon_ens}
  output_grid_dlat=${output_grid_dlat_ens}
fi

app_domain=${app_domain:-regional}
output_grid=${output_grid:-rotated_latlon}
output_grid_cen_lon=${output_grid_cen_lon:-${domlon}}
output_grid_cen_lat=${output_grid_cen_lat:-${domlat}}
output_grid_lon1=${output_grid_lon1:--35.0}
output_grid_lat1=${output_grid_lat1:--30.0}
output_grid_lon2=${output_grid_lon2:-35.0}
output_grid_lat2=${output_grid_lat2:-30.0}

halo_blend=${halo_blend:-0}
nstf_n1=${nstf_n1:-2}
nstf_n2=${nstf_n2:-0}
nstf_n3=${nstf_n3:-0}
nstf_n4=${nstf_n4:-0}
nstf_n5=${nstf_n5:-0}

# Set options for cold-start or warm-start
# Default is cold start from chgres_ic generated ic/bc
na_init=${na_init:-1}
external_ic=${external_ic:-.true.}
nggps_ic=${nggps_ic:-.true.}
mountain=${mountain:-.false.}
warm_start=${warm_start:-.false.}
warm_start_opt=${warm_start_opt:-0}
warmstart_from_restart=${warmstart_from_restart:-no}
RESTARTinp=${RESTARTinp:-"UNNEEDED"}

if [ ${warm_start_opt} -eq 0 ]; then
  warmstart_from_restart=no
  RESTARTinp="UNNEEDED"
fi

# Different warm_start_opt options for determinist/ensemble forecast
if [ ${ENSDA} != "YES" ]; then # for deterministic forecast

if [ ${warm_start_opt} -eq 1 ] && [ -s ${COMhafs}/RESTART_init/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${COMhafs}/RESTART_init
fi
if [ ${warm_start_opt} -eq 2 ] && [ -s ${COMhafsprior}/RESTART/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${COMhafsprior}/RESTART
fi
if [ ${warm_start_opt} -eq 3 ] && [ -s ${COMhafs}/RESTART_vi/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${COMhafs}/RESTART_vi
fi
if [ ${RUN_GSI_VR} = YES ] && [ -s ${COMhafs}/RESTART_analysis_vr/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${COMhafs}/RESTART_analysis_vr
  warm_start_opt=4
fi
if [ ${RUN_GSI} = YES ] && [ -s ${COMhafs}/RESTART_analysis/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${COMhafs}/RESTART_analysis
  warm_start_opt=5
fi

else # for ENSDA member forecast

if [ ${warm_start_opt} -eq 1 ] && [ -s ${COMhafs}/RESTART_init_ens/mem${ENSID}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${COMhafs}/RESTART_init_ens/mem${ENSID}
fi
if [ ${warm_start_opt} -eq 2 ] && [ -s ${COMhafsprior}/RESTART_ens/mem${ENSID}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${COMhafsprior}/RESTART_ens/mem${ENSID}
fi
if [ ${warm_start_opt} -eq 3 ] && [ -s ${COMhafs}/RESTART_vi_ens/mem${ENSID}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${COMhafs}/RESTART_vi_ens/mem${ENSID}
fi
#if [ ${RUN_GSI_VR_ENS} = YES ] && [ -s ${COMhafs}/RESTART_analysis_vr_ens/mem${ENSID}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
if [ ${RUN_GSI_VR_ENS} = YES ] && [ -s ${WORKhafs}/intercom/RESTART_analysis_vr_ens/mem${ENSID}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  #RESTARTinp=${COMhafs}/RESTART_analysis_vr_ens/mem${ENSID}
  RESTARTinp=${WORKhafs}/intercom/RESTART_analysis_vr_ens/mem${ENSID}
  warm_start_opt=4
fi
#if [ ${RUN_ENKF} = YES ] && [ -s ${COMhafs}/RESTART_analysis_ens/mem${ENSID}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
if [ ${RUN_ENKF} = YES ] && [ -s ${WORKhafs}/intercom/RESTART_analysis_ens/mem${ENSID}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  #RESTARTinp=${COMhafs}/RESTART_analysis_ens/mem${ENSID}
  RESTARTinp=${WORKhafs}/intercom/RESTART_analysis_ens/mem${ENSID}
  warm_start_opt=5
fi

fi # ${ENSDA} != "YES"

# For warm start from restart files
if [ ${warmstart_from_restart} = yes ]; then
  na_init=0
  external_ic=.false.
  nggps_ic=.false.
  mountain=.true.
  warm_start=.true.
fi

# Ocean coupling related settings
run_ocean=${run_ocean:-no}
ocean_model=${ocean_model:-hycom}
run_wave=${run_wave:-no}
wave_model=${wave_model:-ww3}
cpl_atm_ocn=${cpl_atm_ocn:-cmeps_2way}
cpl_atm_wav=${cpl_atm_wav:-cmeps_1way_1to2}
cpl_wav_ocn=${cpl_wav_ocn:-cmeps_sidebyside}
ocn_tasks=${ocn_tasks:-120}
med_tasks=${med_tasks:-${ocn_tasks}}
wav_tasks=${wav_tasks:-120}
cplflx=${cplflx:-.false.}
cplocn2atm=${cplocn2atm:-.true.}
cplwav=${cplwav:-.false.}
cplwav2atm=${cplwav2atm:-.false.}
CPL_WND=${CPL_WND:-"CPL:native"}
cpl_dt=${cpl_dt:-360}
ocean_start_dtg=${ocean_start_dtg:-43340.00000}
base_dtg=${CDATE:-2019082900}
end_hour=${NHRS:-126}
merge_import=${merge_import:-.false.}
EARTH_component_list=${EARTH_component_list:-"EARTH_component_list: ATM"}
ATM_model_component=${ATM_model_component:-"ATM_model: fv3"}
OCN_model_component=${OCN_model_component:-""}
WAV_model_component=${WAV_model_component:-""}
MED_model_component=${MED_model_component:-""}
ATM_model_attribute=${ATM_model_attribute:-"ATM_model = fv3"}
OCN_model_attribute=${OCN_model_attribute:-""}
WAV_model_attribute=${WAV_model_attribute:-""}
MED_model_attribute=${MED_model_attribute:-""}

# CDEPS related settings
run_datm=${run_datm:-no}
run_docn=${run_docn:-no}
mesh_atm=${mesh_atm:-''}
mesh_ocn=${mesh_ocn:-''}

if [ $gtype = regional ]; then
  if [ $quilting = .true. ]; then
    ATM_tasks=$(($layoutx*$layouty+$write_groups*$write_tasks_per_group))
  else
    ATM_tasks=$(($layoutx*$layouty))
  fi
elif [ $gtype = nest ]; then
  if [ $quilting = .true. ]; then
    ATM_tasks=$((6*$glob_layoutx*$glob_layouty+$layoutx*$layouty+$write_groups*$write_tasks_per_group))
  else
    ATM_tasks=$((6*$glob_layoutx*$glob_layouty+$layoutx*$layouty))
  fi
else
  echo "FATAL ERROR: Unsupported gtype of ${gtype}. Currently onnly support gtype of nest or regional."
  exit 9
fi

ATM_petlist_bounds=$(printf "ATM_petlist_bounds: %04d %04d" 0 $(($ATM_tasks-1)))

if [ ${run_ocean} = yes ] && [ ${run_wave} != yes ]; then

ATM_model_component="ATM_model: fv3"
OCN_model_component="OCN_model: hycom"
WAV_model_component=""
ATM_model_attribute="ATM_model = fv3"
OCN_model_attribute="OCN_model = hycom"
WAV_model_attribute=""
OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocn_tasks-1)))

# NUOPC based coupling options
if [[ $cpl_atm_ocn = "nuopc"* ]]; then
  EARTH_component_list="EARTH_component_list: ATM OCN"
  MED_model_component=""
  MED_model_attribute=""
  MED_petlist_bounds=""
  # NUOPC based atm-ocn side by side run (no coupling)
  if [ $cpl_atm_ocn = nuopc_sidebyside ]; then
    cplflx=.true.
    cplocn2atm=.false.
    runSeq_ALL="ATM\n OCN"
  # direct coupling through the nearest point regridding method
  elif [ $cpl_atm_ocn = nuopc_nearest ]; then
    cplflx=.true.
    cplocn2atm=.true.
    runSeq_ALL="OCN -> ATM :remapMethod=nearest_stod:srcmaskvalues=0\n ATM -> OCN :remapMethod=nearest_stod:srcmaskvalues=1:dstmaskvalues=0\n ATM\n OCN"
  # direct coupling through the bilinear regridding method
  elif [ $cpl_atm_ocn = nuopc_bilinear ]; then
    cplflx=.true.
    cplocn2atm=.true.
    runSeq_ALL="OCN -> ATM :remapMethod=bilinear:unmappedaction=ignore:zeroregion=select:srcmaskvalues=0\n ATM -> OCN :remapMethod=bilinear:unmappedaction=ignore:zeroregion=select:srcmaskvalues=1:dstmaskvalues=0\n ATM\n OCN"
  fi
# CMEPS based coupling options
elif [[ $cpl_atm_ocn = "cmeps"* ]]; then
  EARTH_component_list="EARTH_component_list: ATM OCN MED"
  MED_model_component="MED_model: cmeps"
  MED_model_attribute="MED_model=cmeps"
  MED_petlist_bounds=$(printf "MED_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$med_tasks-1)))
  # CMEPS based two-way coupling
  if [ $cpl_atm_ocn = cmeps_2way ]; then
    cplflx=.true.
    cplocn2atm=.true.
    runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n OCN -> MED :remapMethod=redist\n MED med_phases_post_ocn\n MED med_phases_prep_atm\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED -> ATM :remapMethod=redist\n MED -> OCN :remapMethod=redist\n ATM\n OCN"
  # CMEPS based one-way coupling from atm to ocn only
  elif [ $cpl_atm_ocn = cmeps_1way_1to2 ]; then
    cplflx=.true.
    cplocn2atm=.false.
    runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n OCN -> MED :remapMethod=redist\n MED med_phases_post_ocn\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED -> OCN :remapMethod=redist\n ATM\n OCN"
  # CMEPS based one-way coupling from ocn to atm only
  elif [ $cpl_atm_ocn = cmeps_1way_2to1 ]; then
    cplflx=.true.
    cplocn2atm=.true.
    runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n OCN -> MED :remapMethod=redist\n MED med_phases_post_ocn\n MED med_phases_prep_atm\n MED -> ATM :remapMethod=redist\n ATM\n OCN"
  # CMEPS based atm-ocn side by side run (no coupling)
  elif [ $cpl_atm_ocn = cmeps_sidebyside ]; then
    cplflx=.true.
    cplocn2atm=.false.
    runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n OCN -> MED :remapMethod=redist\n MED med_phases_post_ocn\n ATM\n OCN"
  fi
# Currently unsupported coupling option combinations
else
  echo "FATAL ERROR: Unsupported coupling option: cpl_atm_ocn=${cpl_atm_ocn}"
  exit 9
fi

fi #if [ ${run_ocean} = yes ] && [ ${run_wave} != yes ]; then

if [ ${run_ocean} != yes ] && [ ${run_wave} = yes ]; then

ATM_model_component="ATM_model: fv3"
OCN_model_component=""
WAV_model_component="WAV_model: ww3"
ATM_model_attribute="ATM_model = fv3"
OCN_model_attribute=""
WAV_model_attribute="WAV_model = ww3"

# NUOPC based coupling options
if [[ $cpl_atm_wav = "nuopc"* ]]; then
  echo "FATAL ERROR: Unsupported coupling option combination: cpl_atm_wav=${cpl_atm_wav}"
  exit 9
# CMEPS based coupling options
elif [[ $cpl_atm_wav = "cmeps"* ]]; then
  EARTH_component_list="EARTH_component_list: ATM WAV MED"
  MED_model_component="MED_model: cmeps"
  MED_model_attribute="MED_model = cmeps"
  MED_petlist_bounds=$(printf "MED_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$med_tasks-1)))
  WAV_petlist_bounds=$(printf "WAV_petlist_bounds: %04d %04d" $(($ATM_tasks+$med_tasks)) $(($ATM_tasks+$med_tasks+$wav_tasks-1)))
  # CMEPS based two-way atm-wav coupling
  if [ $cpl_atm_wav = cmeps_2way ]; then
    cplflx=.true.
    cplocn2atm=.false.
    cplwav=.true.
    cplwav2atm=.true.
    CPL_WND="CPL:native"
    runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n WAV -> MED :remapMethod=redist\n MED med_phases_post_wav\n MED med_phases_prep_atm\n MED -> ATM :remapMethod=redist\n MED med_phases_prep_wav\n MED -> WAV :remapMethod=redist\n ATM\n WAV"
  # CMEPS based one-way atm-wav coupling from atm to wav only
  elif [ $cpl_atm_wav = cmeps_1way_1to2 ]; then
    cplflx=.true.
    cplocn2atm=.false.
    cplwav=.true.
    cplwav2atm=.false.
    CPL_WND="CPL:native"
    runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n WAV -> MED :remapMethod=redist\n MED med_phases_post_wav\n MED med_phases_prep_wav\n MED -> WAV :remapMethod=redist\n ATM\n WAV"
  # CMEPS based one-way atm-wav coupling from wav to atm only
  elif [ $cpl_atm_wav = cmeps_1way_2to1 ]; then
    cplflx=.true.
    cplocn2atm=.false.
    cplwav=.true.
    cplwav2atm=.true.
    CPL_WND="native"
    runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n WAV -> MED :remapMethod=redist\n MED med_phases_post_wav\n MED med_phases_prep_atm\n MED -> ATM :remapMethod=redist\n MED med_phases_prep_wav\n MED -> WAV :remapMethod=redist\n ATM\n WAV"
  # CMEPS based atm-wav side by side run (no coupling)
  elif [ $cpl_atm_wav = cmeps_sidebyside ]; then
    cplflx=.true.
    cplocn2atm=.false.
    cplwav=.true.
    cplwav2atm=.false.
    CPL_WND="native"
    runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n WAV -> MED :remapMethod=redist\n MED med_phases_post_wav\n MED med_phases_prep_atm\n MED -> ATM :remapMethod=redist\n MED med_phases_prep_wav\n MED -> WAV :remapMethod=redist\n ATM\n WAV"
  fi
# Currently unsupported coupling option combinations
else
  echo "FATAL ERROR: Unsupported coupling option combination: cpl_atm_wav=${cpl_atm_wav}"
  exit 9
fi

fi #if [ ${run_ocean} != yes ] && [ ${run_wave} = yes ]; then

if [ ${run_ocean} = yes ] && [ ${run_wave} = yes ]; then

EARTH_component_list="EARTH_component_list: ATM OCN WAV MED"
ATM_model_component="ATM_model: fv3"
OCN_model_component="OCN_model: hycom"
WAV_model_component="WAV_model: ww3"
MED_model_component="MED_model: cmeps"
ATM_model_attribute="ATM_model = fv3"
OCN_model_attribute="OCN_model = hycom"
WAV_model_attribute="WAV_model = ww3"
MED_model_attribute="MED_model = cmeps"

OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocn_tasks-1)))
MED_petlist_bounds=$(printf "MED_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$med_tasks-1)))
WAV_petlist_bounds=$(printf "WAV_petlist_bounds: %04d %04d" $(($ATM_tasks+$ocn_tasks)) $(($ATM_tasks+$ocn_tasks+$wav_tasks-1)))

# NUOPC based atm-ocn-wav side by side run
# Currently, this is the only supported non-CMEPS based configuration for the three component system.
# And this configuration can bitwisely reproduce the CMEPS based sidebyside configuration result (cpl_atm_ocn = cmeps_sidebyside and cpl_atm_wav = cmeps_sidebyside)
if [ $cpl_atm_ocn = nuopc_sidebyside ] && [ $cpl_atm_wav = nuopc_sidebyside ]; then
  EARTH_component_list="EARTH_component_list: ATM OCN WAV"
  MED_model_component=""
  MED_model_attribute=""
  MED_petlist_bounds=""
  cplflx=.true.
  cplocn2atm=.false.
  cplwav=.true.
  cplwav2atm=.false.
  CPL_WND="native"
  runSeq_ALL="ATM\n OCN\n WAV"
# CMEPS based two-way atm-ocn and atm-wav coupling
elif [ $cpl_atm_ocn = cmeps_2way ] && [ $cpl_atm_wav = cmeps_2way ]; then
  cplflx=.true.
  cplocn2atm=.true.
  cplwav=.true.
  cplwav2atm=.true.
  CPL_WND="CPL:native"
  runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n OCN -> MED :remapMethod=redist\n MED med_phases_post_ocn\n WAV -> MED :remapMethod=redist\n MED med_phases_post_wav\n MED med_phases_prep_atm\n MED -> ATM :remapMethod=redist\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED -> OCN :remapMethod=redist\n MED med_phases_prep_wav\n MED -> WAV :remapMethod=redist\n ATM\n OCN\n WAV"
# CMEPS based two-way atm-ocn coupling and one-way atm-wav coupling from atm to wav only
elif [ $cpl_atm_ocn = cmeps_2way ] && [ $cpl_atm_wav = cmeps_1way_1to2 ]; then
  cplflx=.true.
  cplocn2atm=.true.
  cplwav=.true.
  cplwav2atm=.false.
  CPL_WND="CPL:native"
  #runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n OCN -> MED :remapMethod=redist\n MED med_phases_post_ocn\n MED med_phases_prep_atm\n MED -> ATM :remapMethod=redist\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED -> OCN :remapMethod=redist\n MED med_phases_prep_wav\n MED -> WAV :remapMethod=redist\n ATM\n OCN\n WAV"
  runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n OCN -> MED :remapMethod=redist\n MED med_phases_post_ocn\n WAV -> MED :remapMethod=redist\n MED med_phases_post_wav\n MED med_phases_prep_atm\n MED -> ATM :remapMethod=redist\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED -> OCN :remapMethod=redist\n MED med_phases_prep_wav\n MED -> WAV :remapMethod=redist\n ATM\n OCN\n WAV"
# CMEPS based one-way atm-ocn coupling from atm to ocn only and two-way atm-wav coupling
elif [ $cpl_atm_ocn = cmeps_1way_1to2 ] && [ $cpl_atm_wav = cmeps_2way ]; then
  cplflx=.true.
  cplocn2atm=.false.
  cplwav=.true.
  cplwav2atm=.true.
  CPL_WND="CPL:native"
  runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n OCN -> MED :remapMethod=redist\n MED med_phases_post_ocn\n WAV -> MED :remapMethod=redist\n MED med_phases_post_wav\n MED med_phases_prep_atm\n MED -> ATM :remapMethod=redist\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED -> OCN :remapMethod=redist\n MED med_phases_prep_wav\n MED -> WAV :remapMethod=redist\n ATM\n OCN\n WAV"
# CMEPS based one-way atm-ocn coupling from atm to ocn only and one-way atm-wav coupling from atm to wav only
elif [ $cpl_atm_ocn = cmeps_1way_1to2 ] && [ $cpl_atm_wav = cmeps_1way_1to2 ]; then
  cplflx=.true.
  cplocn2atm=.false.
  cplwav=.true.
  cplwav2atm=.false.
  CPL_WND="CPL:native"
  runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n OCN -> MED :remapMethod=redist\n MED med_phases_post_ocn\n WAV -> MED :remapMethod=redist\n MED med_phases_post_wav\n MED med_phases_prep_atm\n MED -> ATM :remapMethod=redist\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED -> OCN :remapMethod=redist\n MED med_phases_prep_wav\n MED -> WAV :remapMethod=redist\n ATM\n OCN\n WAV"
# CMEPS based atm-ocn-wav side by side run
elif [ $cpl_atm_ocn = cmeps_sidebyside ] && [ $cpl_atm_wav = cmeps_sidebyside ]; then
  cplflx=.true.
  cplocn2atm=.false.
  cplwav=.true.
  cplwav2atm=.false.
  CPL_WND="native"
  runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n OCN -> MED :remapMethod=redist\n MED med_phases_post_ocn\n WAV -> MED :remapMethod=redist\n MED med_phases_post_wav\n MED med_phases_prep_atm\n MED -> ATM :remapMethod=redist\n MED med_phases_prep_wav\n MED -> WAV :remapMethod=redist\n ATM\n OCN\n WAV"
# Currently unsupported coupling option combinations
else
  echo "FATAL ERROR: Unsupported coupling options: cpl_atm_ocn=${cpl_atm_ocn}; cpl_atm_wav=${cpl_atm_wav}"
  exit 9
fi

fi #if [ ${run_ocean} = yes ] && [ ${run_wave} = yes ]; then

# CDEPS data models
if [ ${run_datm} = yes ];  then
  OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocn_tasks-1)))
  MED_petlist_bounds=$(printf "MED_petlist_bounds: %04d %04d" 0 $(($ATM_tasks-1)))
  cplflx=.true.
  cplocn2atm=.false.
  cplwav=.false.
  cplwav2atm=.false.
  CPL_WND="native"
  runSeq_ALL="" # not used yet
elif [ ${run_docn} = yes ]; then
  OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocn_tasks-1)))
  MED_petlist_bounds=$(printf "MED_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocn_tasks-1)))
  cplflx=.true.
  cplocn2atm=.true.
  cplwav=.false.
  cplwav2atm=.false.
  CPL_WND="native"
  runSeq_ALL="" # not used yet
fi

# Prepare the output RESTART dir
if [ ${ENSDA} = YES ]; then
  RESTARTout=${RESTARTout:-${COMhafs}/RESTART_ens/mem${ENSID}}
  mkdir -p ${RESTARTout}
  ${NLN} ${RESTARTout} RESTART
elif [ ${RUN_GSI} = YES ] || [ ${RUN_GSI_VR} = YES ]; then
  RESTARTout=${RESTARTout:-${COMhafs}/RESTART}
  mkdir -p ${RESTARTout}
  ${NLN} ${RESTARTout} RESTART
else
  RESTARTout=${RESTARTout:-./RESTART}
  mkdir -p ${RESTARTout}
fi

mkdir -p INPUT

if [ ${run_datm} = no ];  then

# Link the input IC and/or LBC files into the INPUT dir
if [ ! -d $INPdir ]; then
   echo "FATAL ERROR: Input data dir does not exist: $INPdir"
   exit 9
fi

${NLN} ${INPdir}/*.nc INPUT/

# Copy fix files
${NCP} $FIXam/global_solarconstant_noaa_an.txt  solarconstant_noaa_an.txt
${NCP} $FIXam/ozprdlos_2015_new_sbuvO3_tclm15_nuchem.f77 global_o3prdlos.f77
${NCP} $FIXam/global_h2o_pltc.f77               global_h2oprdlos.f77
${NCP} $FIXam/global_sfc_emissivity_idx.txt     sfc_emissivity_idx.txt
${NCP} $FIXam/global_co2historicaldata_glob.txt co2historicaldata_glob.txt
${NCP} $FIXam/co2monthlycyc.txt                 co2monthlycyc.txt
${NCP} $FIXam/global_climaeropac_global.txt     aerosol.dat
${NCP} $FIXam/global_glacier.2x2.grb .
${NCP} $FIXam/global_maxice.2x2.grb .
${NCP} $FIXam/RTGSST.1982.2012.monthly.clim.grb .
${NCP} $FIXam/global_snoclim.1.875.grb .
${NCP} $FIXam/global_snowfree_albedo.bosu.t1534.3072.1536.rg.grb .
${NCP} $FIXam/global_albedo4.1x1.grb .
${NCP} $FIXam/CFSR.SEAICE.1982.2012.monthly.clim.grb .
${NCP} $FIXam/global_tg3clim.2.6x1.5.grb .
${NCP} $FIXam/global_vegfrac.0.144.decpercent.grb .
${NCP} $FIXam/global_vegtype.igbp.t1534.3072.1536.rg.grb .
${NCP} $FIXam/global_soiltype.statsgo.t1534.3072.1536.rg.grb .
${NCP} $FIXam/global_soilmgldas.t1534.3072.1536.grb .
${NCP} $FIXam/seaice_newland.grb .
${NCP} $FIXam/global_shdmin.0.144x0.144.grb .
${NCP} $FIXam/global_shdmax.0.144x0.144.grb .
${NCP} $FIXam/global_slope.1x1.grb .
${NCP} $FIXam/global_mxsnoalb.uariz.t1534.3072.1536.rg.grb .

for file in $(ls ${FIXam}/fix_co2_proj/global_co2historicaldata*); do
  ${NCP} $file $(echo $(basename $file) |sed -e "s/global_//g")
done

# If needed, copy fix files needed by the hwrf ccpp physics suite
if [[ ${ccpp_suite_regional} == *"hwrf"* ]] ||  [[ ${ccpp_suite_glob} == *"hwrf"* ]] || [[ ${ccpp_suite_nest} == *"hwrf"* ]]; then
  ${NCP} ${PARMhafs}/forecast/hwrf_physics_fix/* .
fi

if [ $gtype = nest ]; then
  ntiles=7
elif [ $gtype = regional ]; then
  ntiles=1
elif [ $gtype = uniform ] || [ $gtype = stretch ]; then
  ntiles=6
else
  echo "FATAL ERROR: Unsupported gtype of ${gtype}."
  exit 9
fi

if [ $gtype = nest ]; then

# Copy grid and orography
tile=1
while [ $tile -le $ntiles ]; do
  ${NCP} $FIXgrid/${CASE}/${CASE}_oro_data.tile${tile}.nc INPUT/oro_data.tile${tile}.nc
  ${NCP} $FIXgrid/${CASE}/${CASE}_grid.tile${tile}.nc INPUT/${CASE}_grid.tile${tile}.nc
  let tile=tile+1
done
${NCP} $FIXgrid/${CASE}/${CASE}_mosaic.nc INPUT/grid_spec.nc

cd ./INPUT
#${NLN} ${CASE}_grid.tile7.nc ${CASE}_grid.nest02.tile7.nc
${NLN} ${CASE}_grid.tile7.nc grid.nest02.tile7.nc
${NLN} oro_data.tile7.nc oro_data.nest02.tile7.nc
${NLN} gfs_data.tile7.nc gfs_data.nest02.tile7.nc
${NLN} sfc_data.tile7.nc sfc_data.nest02.tile7.nc
cd ..

# Prepare data_table, diag_table, field_table, input.nml, input_nest02.nml,
# model_configure, and nems.configure
#${NCP} ${PARMforecast}/data_table .
${NCP} ${PARMforecast}/diag_table.tmp .
${NCP} ${PARMforecast}/field_table .
${NCP} ${PARMforecast}/input.nml.tmp .
${NCP} ${PARMforecast}/input_nest02.nml.tmp .
${NCP} ${PARMforecast}/model_configure.tmp .
${NCP} ${PARMforecast}/nems.configure.atmonly ./nems.configure

glob_pes=$(( ${glob_layoutx} * ${glob_layouty} * 6 ))
nest_pes=$(( ${layoutx} * ${layouty} ))
ioffset=$(( (istart_nest-1)/2 + 1))
joffset=$(( (jstart_nest-1)/2 + 1))
blocksize=$(( ${glob_npy}/${glob_layouty} ))

sed -e "s/_blocksize_/${blocksize:-64}/g" \
    -e "s/_ccpp_suite_/${ccpp_suite_glob}/g" \
    -e "s/_deflate_level_/${deflate_level:--1}/g" \
    -e "s/_layoutx_/${glob_layoutx}/g" \
    -e "s/_layouty_/${glob_layouty}/g" \
    -e "s/_npx_/${glob_npx}/g" \
    -e "s/_npy_/${glob_npy}/g" \
    -e "s/_npz_/${npz}/g" \
    -e "s/_k_split_/${glob_k_split}/g" \
    -e "s/_n_split_/${glob_n_split}/g" \
    -e "s/_na_init_/${na_init}/g" \
    -e "s/_external_ic_/${external_ic}/g" \
    -e "s/_nggps_ic_/${nggps_ic}/g" \
    -e "s/_mountain_/${mountain}/g" \
    -e "s/_warm_start_/${warm_start}/g" \
    -e "s/_target_lat_/${target_lat}/g" \
    -e "s/_target_lon_/${target_lon}/g" \
    -e "s/_stretch_fac_/${stretch_fac}/g" \
    -e "s/_refinement_/${refine_ratio}/g" \
    -e "s/_ioffset_/${ioffset}/g" \
    -e "s/_joffset_/${joffset}/g" \
    -e "s/_glob_pes_/${glob_pes}/g" \
    -e "s/_nest_pes_/${nest_pes}/g" \
    -e "s/_levp_/${LEVS}/g" \
    -e "s/_fhswr_/${fhswr:-1800.}/g" \
    -e "s/_fhlwr_/${fhlwr:-1800.}/g" \
    -e "s/_nstf_n1_/${nstf_n1:-2}/g" \
    -e "s/_nstf_n2_/${nstf_n2:-0}/g" \
    -e "s/_nstf_n3_/${nstf_n3:-0}/g" \
    -e "s/_nstf_n4_/${nstf_n4:-0}/g" \
    -e "s/_nstf_n5_/${nstf_n5:-0}/g" \
    -e "s/_cplflx_/${cplflx:-.false.}/g" \
    -e "s/_cplocn2atm_/${cplocn2atm}/g" \
    -e "s/_cplwav_/${cplwav:-.false.}/g" \
    -e "s/_cplwav2atm_/${cplwav2atm:-.false.}/g" \
    -e "s/_merge_import_/${merge_import:-.false.}/g" \
    input.nml.tmp > input.nml

blocksize=$(( ${npy}/${layouty} ))
sed -e "s/_blocksize_/${blocksize:-64}/g" \
    -e "s/_ccpp_suite_/${ccpp_suite_nest}/g" \
    -e "s/_deflate_level_/${deflate_level:--1}/g" \
    -e "s/_layoutx_/${layoutx}/g" \
    -e "s/_layouty_/${layouty}/g" \
    -e "s/_npx_/${npx}/g" \
    -e "s/_npy_/${npy}/g" \
    -e "s/_npz_/${npz}/g" \
    -e "s/_k_split_/${k_split}/g" \
    -e "s/_n_split_/${n_split}/g" \
    -e "s/_na_init_/${na_init}/g" \
    -e "s/_external_ic_/${external_ic}/g" \
    -e "s/_nggps_ic_/${nggps_ic}/g" \
    -e "s/_mountain_/${mountain}/g" \
    -e "s/_warm_start_/${warm_start}/g" \
    -e "s/_target_lat_/${target_lat}/g" \
    -e "s/_target_lon_/${target_lon}/g" \
    -e "s/_stretch_fac_/${stretch_fac}/g" \
    -e "s/_refinement_/${refine_ratio}/g" \
    -e "s/_ioffset_/${ioffset}/g" \
    -e "s/_joffset_/${joffset}/g" \
    -e "s/_glob_pes_/${glob_pes}/g" \
    -e "s/_nest_pes_/${nest_pes}/g" \
    -e "s/_levp_/${LEVS}/g" \
    -e "s/_fhswr_/${fhswr:-1800.}/g" \
    -e "s/_fhlwr_/${fhlwr:-1800.}/g" \
    -e "s/_nstf_n1_/${nstf_n1:-2}/g" \
    -e "s/_nstf_n2_/${nstf_n2:-0}/g" \
    -e "s/_nstf_n3_/${nstf_n3:-0}/g" \
    -e "s/_nstf_n4_/${nstf_n4:-0}/g" \
    -e "s/_nstf_n5_/${nstf_n5:-0}/g" \
    -e "s/_cplflx_/${cplflx:-.false.}/g" \
    -e "s/_cplocn2atm_/${cplocn2atm}/g" \
    -e "s/_cplwav_/${cplwav:-.false.}/g" \
    -e "s/_cplwav2atm_/${cplwav2atm:-.false.}/g" \
    -e "s/_merge_import_/${merge_import:-.false.}/g" \
    input_nest02.nml.tmp > input_nest02.nml

elif [ $gtype = regional ]; then

# Prepare tile data and orography for regional
tile=7
# Copy grid and orog files (halo[034])
${NCP} $FIXgrid/${CASE}/${CASE}_grid.tile${tile}.halo?.nc INPUT/.
${NCP} $FIXgrid/${CASE}/${CASE}_oro_data.tile${tile}.halo?.nc INPUT/.
${NCP} $FIXgrid/${CASE}/${CASE}_mosaic.nc INPUT/.

cd INPUT
${NLN} ${CASE}_mosaic.nc grid_spec.nc
${NLN} ${CASE}_grid.tile7.halo0.nc grid.tile7.halo0.nc
${NLN} ${CASE}_grid.tile7.halo3.nc ${CASE}_grid.tile7.nc
${NLN} ${CASE}_grid.tile7.halo4.nc grid.tile7.halo4.nc
${NLN} ${CASE}_oro_data.tile7.halo0.nc oro_data.nc
${NLN} ${CASE}_oro_data.tile7.halo4.nc oro_data.tile7.halo4.nc
${NLN} sfc_data.tile7.nc sfc_data.nc
${NLN} gfs_data.tile7.nc gfs_data.nc

# For warm start from restart files (either before or after analysis)
if [ ${warmstart_from_restart} = yes ]; then
  ${NLN} ${RESTARTinp}/${PDY}.${cyc}0000.coupler.res ./coupler.res
  ${NLN} ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.nc ./fv_core.res.nc
  ${NLN} ${RESTARTinp}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ./fv_srf_wnd.res.tile1.nc
# ${NLN} ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ./fv_core.res.tile1.nc
# ${NLN} ${RESTARTinp}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ./fv_tracer.res.tile1.nc
  # Remove the checksum attribute for all restart variables, so that the
  # forecast executable will not compare the checksum attribute against the
  # checksum calculated from the actual data. This is because the DA/GSI
  # currently only update the variable itself but not its checksum attribute.
  ncatted -a checksum,,d,, ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ./fv_core.res.tile1.nc
  ncatted -a checksum,,d,, ${RESTARTinp}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ./fv_tracer.res.tile1.nc
#
  line2=`sed -n '2p' coupler.res`
  line3=`sed -n '3p' coupler.res`
  hh2=$(echo $line2 | awk '{print $4}')
  hh3=$(echo $line3 | awk '{print $4}')
  ${NSED} -i 's/\b'$hh2'\b/'$hh3'/' coupler.res
fi

cd ..

# Prepare data_table, diag_table, field_table, input.nml, input_nest02.nml,
# model_configure, and nems.configure
#${NCP} ${PARMforecast}/data_table .
${NCP} ${PARMforecast}/diag_table.tmp .
${NCP} ${PARMforecast}/field_table .
${NCP} ${PARMforecast}/input.nml.tmp .
${NCP} ${PARMforecast}/model_configure.tmp .

if [ ${run_ocean} = yes ] || [ ${run_wave} = yes ]; then
  ${NCP} ${PARMforecast}/nems.configure.cpl.tmp ./nems.configure.tmp
else
  ${NCP} ${PARMforecast}/nems.configure.atmonly ./nems.configure.tmp
fi

  sed -e "s/_EARTH_component_list_/${EARTH_component_list}/g" \
      -e "s/_ATM_model_component_/${ATM_model_component}/g" \
      -e "s/_OCN_model_component_/${OCN_model_component}/g" \
      -e "s/_WAV_model_component_/${WAV_model_component}/g" \
      -e "s/_MED_model_component_/${MED_model_component}/g" \
      -e "s/_ATM_model_attribute_/${ATM_model_attribute}/g" \
      -e "s/_OCN_model_attribute_/${OCN_model_attribute}/g" \
      -e "s/_WAV_model_attribute_/${WAV_model_attribute}/g" \
      -e "s/_MED_model_attribute_/${MED_model_attribute}/g" \
      -e "s/_ATM_petlist_bounds_/${ATM_petlist_bounds}/g" \
      -e "s/_OCN_petlist_bounds_/${OCN_petlist_bounds}/g" \
      -e "s/_WAV_petlist_bounds_/${WAV_petlist_bounds}/g" \
      -e "s/_MED_petlist_bounds_/${MED_petlist_bounds}/g" \
      -e "s/_cpl_dt_/${cpl_dt}/g" \
      -e "s/_runSeq_ALL_/${runSeq_ALL}/g" \
      -e "s/_base_dtg_/${base_dtg}/g" \
      -e "s/_ocean_start_dtg_/${ocean_start_dtg}/g" \
      -e "s/_end_hour_/${end_hour}/g" \
      -e "s/_merge_import_/${merge_import:-.false.}/g" \
      nems.configure.tmp > nems.configure

blocksize=$(( ${npy}/${layouty} ))
sed -e "s/_blocksize_/${blocksize:-64}/g" \
    -e "s/_ccpp_suite_/${ccpp_suite_regional}/g" \
    -e "s/_deflate_level_/${deflate_level:--1}/g" \
    -e "s/_layoutx_/${layoutx}/g" \
    -e "s/_layouty_/${layouty}/g" \
    -e "s/_npx_/${npx}/g" \
    -e "s/_npy_/${npy}/g" \
    -e "s/_npz_/${npz}/g" \
    -e "s/_k_split_/${k_split}/g" \
    -e "s/_n_split_/${n_split}/g" \
    -e "s/_na_init_/${na_init}/g" \
    -e "s/_external_ic_/${external_ic}/g" \
    -e "s/_nggps_ic_/${nggps_ic}/g" \
    -e "s/_mountain_/${mountain}/g" \
    -e "s/_warm_start_/${warm_start}/g" \
    -e "s/_target_lat_/${target_lat}/g" \
    -e "s/_target_lon_/${target_lon}/g" \
    -e "s/_stretch_fac_/${stretch_fac}/g" \
    -e "s/_bc_update_interval_/${NBDYHRS}/g" \
    -e "s/_nrows_blend_/${halo_blend}/g" \
    -e "s/_levp_/${LEVS}/g" \
    -e "s/_fhswr_/${fhswr:-1800.}/g" \
    -e "s/_fhlwr_/${fhlwr:-1800.}/g" \
    -e "s/_nstf_n1_/${nstf_n1:-2}/g" \
    -e "s/_nstf_n2_/${nstf_n2:-0}/g" \
    -e "s/_nstf_n3_/${nstf_n3:-0}/g" \
    -e "s/_nstf_n4_/${nstf_n4:-0}/g" \
    -e "s/_nstf_n5_/${nstf_n5:-0}/g" \
    -e "s/_cplflx_/${cplflx:-.false.}/g" \
    -e "s/_cplocn2atm_/${cplocn2atm}/g" \
    -e "s/_cplwav_/${cplwav:-.false.}/g" \
    -e "s/_cplwav2atm_/${cplwav2atm:-.false.}/g" \
    -e "s/_merge_import_/${merge_import:-.false.}/g" \
    input.nml.tmp > input.nml

fi # if regional

fi # if not cdeps datm

if [ $gtype = regional ]; then

if [ ${run_ocean} = yes ];  then
  # Copy hycom related files
  ${NCP} ${WORKhafs}/intercom/hycominit/hycom_settings hycom_settings
  hycom_basin=$(grep RUNmodIDout ./hycom_settings | cut -c20-)
  # copy IC/BC
  ${NCP} ${WORKhafs}/intercom/hycominit/restart_out.a restart_in.a
  ${NCP} ${WORKhafs}/intercom/hycominit/restart_out.b restart_in.b
  # copy forcing
  ${NCP} ${WORKhafs}/intercom/hycominit/forcing* .
  ${NLN} forcing.presur.a forcing.mslprs.a
  ${NLN} forcing.presur.b forcing.mslprs.b
  # copy fix
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.regional.depth.a regional.depth.a
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.regional.depth.b regional.depth.b
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.regional.grid.a regional.grid.a
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.regional.grid.b regional.grid.b
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.forcing.chl.a forcing.chl.a
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.forcing.chl.b forcing.chl.b
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.iso.sigma.a iso.sigma.a
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.iso.sigma.b iso.sigma.b
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.relax.ssh.a relax.ssh.a
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.relax.ssh.b relax.ssh.b
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.tbaric.a tbaric.a
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.tbaric.b tbaric.b
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.thkdf4.a thkdf4.a
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.thkdf4.b thkdf4.b
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.veldf2.a veldf2.a
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.veldf2.b veldf2.b
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.veldf4.a veldf4.a
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.veldf4.b veldf4.b
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.relax.rmu.a relax.rmu.a
  ${NCP} ${FIXhycom}/hafs_${hycom_basin}.basin.relax.rmu.b relax.rmu.b
  # copy parms
  ${NCP} ${PARMhycom}/hafs_${hycom_basin}.basin.fcst.blkdat.input blkdat.input
  ${NCP} ${PARMhycom}/hafs_${hycom_basin}.basin.ports.input ports.input
  ${NCP} ${PARMhycom}/hafs_${hycom_basin}.basin.patch.input.${ocn_tasks} patch.input
  # create hycom limits
  ${USHhafs}/hafs_hycom_limits.py ${CDATE}
fi #if [ ${run_ocean} = yes ]; then

if [ ${run_wave} = yes ]; then
  # link ww3 related files
  ${NLN} ${COMhafs}/${out_prefix}.mod_def.ww3 mod_def.ww3
  ${NLN} ${COMhafs}/${out_prefix}.wind.ww3 wind.ww3
  ${NLN} ${COMhafs}/${out_prefix}.current.ww3 current.ww3
  ${NLN} ${COMhafs}/${out_prefix}.restart_init.ww3 restart.ww3
  ${NLN} ${COMhafs}/${out_prefix}.nest.ww3 nest.ww3
  # copy parms
  ${NCP} ${PARMww3}/ww3_multi.inp_tmpl ./ww3_multi.inp_tmpl
  # generate ww3_multi.inp
  EDATE=$($NDATE +${NHRS} ${CDATE})
  RDATE=$($NDATE +6 ${CDATE})
  RUN_BEG="${CDATE:0:8} ${CDATE:8:2}0000"
  FLD_BEG="${RUN_BEG}"
  PNT_BEG="${RUN_BEG}"
  RST_BEG="${RUN_BEG}"
  RUN_END="${EDATE:0:8} ${EDATE:8:2}0000"
  FLD_END="${RUN_END}"
  PNT_END="${RUN_END}"
  RST_END="${RDATE:0:8} ${RDATE:8:2}0000"
  FLD_DT=$((3600*${NOUTHRS}))
  PNT_DT=$((3600*${NOUTHRS}))
  RST_DT=$((3600*6))

  sed -e "s/<u:CPL_WND>/${CPL_WND}/g" \
      -e "s/<u:RUN_BEG>/${RUN_BEG}/g" \
      -e "s/<u:RUN_END>/${RUN_END}/g" \
      -e "s/<u:FLD_BEG>/${FLD_BEG}/g" \
      -e "s/<i:FLD_DT>/${FLD_DT}/g" \
      -e "s/<u:FLD_END>/${FLD_END}/g" \
      -e "s/<u:PNT_BEG>/${PNT_BEG}/g" \
      -e "s/<i:PNT_DT>/${PNT_DT}/g" \
      -e "s/<u:PNT_END>/${PNT_END}/g" \
      -e "s/<u:RST_BEG>/${RST_BEG}/g" \
      -e "s/<i:RST_DT>/${RST_DT}/g" \
      -e "s/<u:RST_END>/${RST_END}/g" \
    ./ww3_multi.inp_tmpl > ./ww3_multi.inp
fi #if [ ${run_wave} = yes ]; then

# Pass along the grid_spec.nc, atmos_static.nc, oro_data.nc from the prior cycle if exist
if [ ${ENSDA} = YES ]; then
  if [ -s ${COMhafsprior}/RESTART_ens/mem${ENSID}/grid_spec.nc ]; then
    ${NCP} -p ${COMhafsprior}/RESTART_ens/mem${ENSID}/grid_spec.nc RESTART/
  fi
  if [ -s ${COMhafsprior}/RESTART_ens/mem${ENSID}/atmos_static.nc ]; then
    ${NCP} -p ${COMhafsprior}/RESTART_ens/mem${ENSID}/atmos_static.nc RESTART/
  fi
  if [ -s ${COMhafsprior}/RESTART_ens/mem${ENSID}/oro_data.nc ]; then
    ${NCP} -p ${COMhafsprior}/RESTART_ens/mem${ENSID}/oro_data.nc RESTART/
  fi
else
  if [ -s ${COMhafsprior}/RESTART/grid_spec.nc ]; then
    ${NCP} -p ${COMhafsprior}/RESTART/grid_spec.nc RESTART/
  fi
  if [ -s ${COMhafsprior}/RESTART/atmos_static.nc ]; then
    ${NCP} -p ${COMhafsprior}/RESTART/atmos_static.nc RESTART/
  fi
  if [ -s ${COMhafsprior}/RESTART/oro_data.nc ]; then
    ${NCP} -p ${COMhafsprior}/RESTART/oro_data.nc RESTART/
  fi
fi

fi #if [ $gtype = nest ]; then

# Generate diag_table, model_configure from their tempelates
yr=$(echo $CDATE | cut -c1-4)
mn=$(echo $CDATE | cut -c5-6)
dy=$(echo $CDATE | cut -c7-8)
cat > temp << EOF
${yr}${mn}${dy}.${cyc}Z.${CASE}.32bit.non-hydro
$yr $mn $dy $cyc 0 0
EOF

enddate=`${NDATE} +${NHRS} $CDATE`
endyr=`echo $enddate | cut -c1-4`

if [ ${run_datm} = no ];  then
cat temp diag_table.tmp > diag_table
fi

#---------------------------------------------------
# Copy CDEPS input, parm, and fix files if required.
#---------------------------------------------------

if [ ${run_datm} = yes ];  then
  datm_source=${DATM_SOURCE:-ERA5}
  ${NCP} ${PARMforecast}/model_configure.tmp .
  ${NLN} ${mesh_atm} INPUT/DATM_ESMF_mesh.nc
  ${NLN} "$datm_input_path"/DATM_input*nc INPUT/

  # Generate docn.streams from template specific to the model:
  ${NCP} ${PARMhafs}/cdeps/datm_$( echo "$datm_source" | tr A-Z a-z ).streams datm.streams
  for file in INPUT/DATM_input*nc ; do
      if [[ -s "$file" ]] ; then
      sed -i "/^stream_data_files01:/ s/$/\ \"INPUT\/$(basename $file)\"/" datm.streams
      fi
  done
  sed -i "s/_yearFirst_/$yr/g" datm.streams
  sed -i "s/_yearLast_/$endyr/g" datm.streams
  sed -i "s/_mesh_atm_/INPUT\/DATM_ESMF_mesh.nc/g" datm.streams

  # Generate datm_in and nems.configure from model-independent templates:
  ${NCP} ${PARMhafs}/cdeps/datm_in .
  sed -i "s/_mesh_atm_/INPUT\/DATM_ESMF_mesh.nc/g" datm_in

  ${NCP} ${PARMforecast}/nems.configure.cdeps.tmp ./
  sed -e "s/_ATM_petlist_bounds_/${ATM_petlist_bounds}/g" \
      -e "s/_MED_petlist_bounds_/${MED_petlist_bounds}/g" \
      -e "s/_OCN_petlist_bounds_/${OCN_petlist_bounds}/g" \
      -e "s/_cpl_dt_/${cpl_dt}/g" \
      -e "s/_base_dtg_/${CDATE}/g" \
      -e "s/_ocean_start_dtg_/${ocean_start_dtg}/g" \
      -e "s/_end_hour_/${NHRS}/g" \
      -e "s/_merge_import_/${merge_import:-.true.}/g" \
      -e "s/_mesh_atm_/INPUT\/DATM_ESMF_mesh.nc/g" \
      -e "/_mesh_ocn_/d" \
      -e "/_system_type_/d" \
      -e "s/_atm_model_/datm/g" \
      -e "s/_ocn_model_/hycom/g" \
      nems.configure.cdeps.tmp > nems.configure

elif [ ${run_docn} = yes ];  then
  MAKE_MESH_OCN=$( echo "${make_mesh_ocn:-no}" | tr a-z A-Z )
  ${NLN} "$docn_input_path"/DOCN_input*nc INPUT/

  #${NCP} ${PARMhafs}/cdeps/docn_in .
  #${NCP} ${PARMhafs}/cdeps/docn.streams .
  docn_source=${DOCN_SOURCE:-OISST}

  # Generate docn_in from template:
  ${NCP} ${PARMhafs}/cdeps/docn_in docn_in_template
  sed -e "s/_mesh_ocn_/INPUT\/DOCN_ESMF_mesh.nc/g" \
      -e "s/_nx_global_/$docn_mesh_nx_global/g" \
      -e "s/_ny_global_/$docn_mesh_ny_global/g" \
      < docn_in_template > docn_in

  # Generate docn.streams from template specific to the model:
  ${NCP} ${PARMhafs}/cdeps/docn_$( echo "$docn_source" | tr A-Z a-z ).streams docn.streams
  sed -i "s/_yearFirst_/$yr/g" docn.streams
  sed -i "s/_yearLast_/$endyr/g" docn.streams
  sed -i "s/_mesh_ocn_/INPUT\/DOCN_ESMF_mesh.nc/g" docn.streams
  for file in INPUT/oisst*.nc INPUT/sst*.nc INPUT/DOCN_input*.nc ; do
    if [[ -s "$file" ]] ; then
      sed -i "/^stream_data_files01:/ s/$/\ \"INPUT\/$(basename $file)\"/" docn.streams
    fi
  done

  ${NLN} "${mesh_ocn}" INPUT/DOCN_ESMF_mesh.nc

  ${NCP} ${PARMforecast}/nems.configure.cdeps.tmp ./
  sed -e "s/_ATM_petlist_bounds_/${ATM_petlist_bounds}/g" \
      -e "s/_MED_petlist_bounds_/${MED_petlist_bounds}/g" \
      -e "s/_OCN_petlist_bounds_/${OCN_petlist_bounds}/g" \
      -e "s/_cpl_dt_/${cpl_dt}/g" \
      -e "s/_base_dtg_/${CDATE}/g" \
      -e "s/_ocean_start_dtg_/${ocean_start_dtg}/g" \
      -e "s/_end_hour_/${NHRS}/g" \
      -e "s/_merge_import_/${merge_import:-.true.}/g" \
      -e "/_mesh_atm_/d" \
      -e "s/_mesh_ocn_/INPUT\/DOCN_ESMF_mesh.nc/g" \
      -e "s/_system_type_/ufs/g" \
      -e "s/_atm_model_/fv3/g" \
      -e "s/_ocn_model_/docn/g" \
      nems.configure.cdeps.tmp > nems.configure

fi

sed -e "s/_print_esmf_/${print_esmf:-.false.}/g" \
    -e "s/YR/$yr/g" -e "s/MN/$mn/g" -e "s/DY/$dy/g" \
    -e "s/H_R/$cyc/g" -e "s/NHRS/$NHRS/g" \
    -e "s/_dt_atmos_/${dt_atmos}/g" \
    -e "s/_restart_interval_/${restart_interval}/g" \
    -e "s/_quilting_/${quilting}/g" \
    -e "s/_write_groups_/${write_groups}/g" \
    -e "s/_write_tasks_per_group_/${write_tasks_per_group}/g" \
    -e "s/_write_dopost_/${write_dopost:-.false.}/g" \
    -e "s/_output_history_/${output_history:-.true.}/g" \
    -e "s/_app_domain_/${app_domain}/g" \
    -e "s/_OUTPUT_GRID_/$output_grid/g" \
    -e "s/_CEN_LON_/$output_grid_cen_lon/g" \
    -e "s/_CEN_LAT_/$output_grid_cen_lat/g" \
    -e "s/_LON1_/$output_grid_lon1/g" \
    -e "s/_LAT1_/$output_grid_lat1/g" \
    -e "s/_LON2_/$output_grid_lon2/g" \
    -e "s/_LAT2_/$output_grid_lat2/g" \
    -e "s/_DLON_/$output_grid_dlon/g" \
    -e "s/_DLAT_/$output_grid_dlat/g" \
    model_configure.tmp > model_configure

# Copy fix files needed by inline_post
if [ ${write_dopost:-.false.} = .true. ]; then

  ${NCP} ${PARMhafs}/post/itag                    ./itag
  ${NCP} ${PARMhafs}/post/params_grib2_tbl_new    ./params_grib2_tbl_new

if [ ${satpost} = .true. ]; then
  ${NCP} ${PARMhafs}/post/postxconfig-NT-hafs.txt ./postxconfig-NT.txt
  ${NCP} ${PARMhafs}/post/postxconfig-NT-hafs.txt ./postxconfig-NT_FH00.txt
  # Link crtm fix files
  for file in "amsre_aqua" "imgr_g11" "imgr_g12" "imgr_g13" \
    "imgr_g15" "imgr_mt1r" "imgr_mt2" "seviri_m10" \
    "ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16" \
    "ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" \
    "tmi_trmm" "v.seviri_m10" "imgr_insat3d" "abi_gr" "ahi_himawari8" ; do
    ${NLN} ${FIXcrtm}/fix-4-hafs/${file}.TauCoeff.bin ./
    ${NLN} ${FIXcrtm}/fix-4-hafs/${file}.SpcCoeff.bin ./
  done
  for file in "Aerosol" "Cloud" ; do
    ${NLN} ${FIXcrtm}/fix-4-hafs/${file}Coeff.bin ./
  done
  for file in ${FIXcrtm}/fix-4-hafs/*Emis* ; do
    ${NLN} ${file} ./
  done
else
  ${NCP} ${PARMhafs}/post/postxconfig-NT-hafs_nosat.txt ./postxconfig-NT.txt
  ${NCP} ${PARMhafs}/post/postxconfig-NT-hafs_nosat.txt ./postxconfig-NT_FH00.txt
fi

fi

# Copy the fd_nems.yaml file
${NCP} ${HOMEhafs}/sorc/hafs_forecast.fd/tests/parm/fd_nems.yaml ./

# Copy the executable and run the forecast
FORECASTEXEC=${FORECASTEXEC:-${EXEChafs}/hafs_forecast.x}
${NCP} -p ${FORECASTEXEC} ./hafs_forecast.x
${APRUNC} ./hafs_forecast.x 1>out.forecast 2>err.forecast

# Cat out and err into job log
cat ./out.forecast
cat ./err.forecast

if [ $gtype = regional ] && [ ${run_datm} = no ]; then

# Rename the restart files with a proper convention if needed
cd RESTART
CDATEnhrs=`${NDATE} +${NHRS} $CDATE`
PDYnhrs=`echo ${CDATEnhrs} | cut -c1-8`
cycnhrs=`echo ${CDATEnhrs} | cut -c9-10`
if [ -s fv_core.res.nc ]; then
  for file in $(/bin/ls -1 fv*.nc phy_data.nc sfc_data.nc coupler.res)
  do
    mv ${file} ${PDYnhrs}.${cycnhrs}0000.${file}
  done
fi
cd ${DATA}

# Pass over the grid_spec.nc, atmos_static.nc, oro_data.nc if not yet exist
if [ ! -s RESTART/grid_spec.nc ]; then
  ${NCP} -p grid_spec.nc RESTART/
fi
if [ ! -s RESTART/atmos_static.nc ]; then
  ${NCP} -p atmos_static.nc RESTART/
fi
if [ ! -s RESTART/oro_data.nc ]; then
  ${NCP} -pL INPUT/oro_data.nc RESTART/
fi

fi # if [ $gtype = regional ] && [ ${run_datm} = no ]; then

exit
