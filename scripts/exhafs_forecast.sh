#!/bin/sh
################################################################################
# Script Name: exhafs_forecast.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs a HAFS forecast with various coldstart/warmstart and
#   uncoupled/coupled configurations.
################################################################################
set -xe

CDATE=${CDATE:-${YMDH}}
YMD=$(echo ${CDATE} | cut -c1-8)
yr=$(echo $CDATE | cut -c1-4)
mn=$(echo $CDATE | cut -c5-6)
dy=$(echo $CDATE | cut -c7-8)
hh=$(echo ${CDATE} | cut -c9-10)

PARMforecast=${PARMforecast:-${PARMhafs}/forecast/regional}
PARMhycom=${PARMhycom:-${PARMhafs}/hycom/regional}
PARMww3=${PARMww3:-${PARMhafs}/ww3/regional}
FIXam=${FIXam:-${FIXhafs}/fix_am}
FIXcrtm=${FIXcrtm:-${CRTM_FIX:?}}
FIXhycom=${FIXhycom:-${FIXhafs}/fix_hycom}
FIXmom6=${FIXmom6:-${FIXhafs}/fix_mom6}
if [ ${ocean_model} = "hycom" ]; then
  FORECASTEXEC=${FORECASTEXEC:-${EXEChafs}/hafs_forecast_hycom.x}
else
  FORECASTEXEC=${FORECASTEXEC:-${EXEChafs}/hafs_forecast_mom6.x}
fi

ATPARSE=${ATPARSE:-${USHhafs}/hafs_atparse.sh}
source ${ATPARSE}

ENSDA=${ENSDA:-NO}
ENSID=${ENSID:-000}
FORECAST_RESTART=${FORECAST_RESTART:-NO}
FORECAST_RESTART_HR=${FORECAST_RESTART_HR:-0}
FORECAST_RESTART_HC=${FORECAST_RESTART_HC:-""}

# Reset options specific to the ensemble forecast if needed
if [ "${ENSDA}" = YES ]; then
# Ensemble member with ENSID <= ${ENS_FCST_SIZE} will run the full-length NHRS forecast
  if [ $((10#${ENSID})) -le ${ENS_FCST_SIZE:-10} ]; then
    NHRS=${NHRS:-126}
  else
    NHRS=${NHRS_ENS:-6}
  fi
  NBDYHRS=${NBDYHRS_ENS:-3}
  NOUTHRS=${NOUTHRS_ENS:-3}
  CASE=${CASE_ENS:-C768}
  gtype=${gtype_ens:-regional}
  LEVS=${LEVS_ENS:-65}
  stretch_fac=${stretch_fac_ens:-1.0001}
  target_lon=${target_lon_ens:--62.0}
  target_lat=${target_lat_ens:-22.0}
  refine_ratio=${refine_ratio_ens:-4}
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
  twowaynest=${twowaynest_ens:-.true.}
  glob_k_split=${glob_k_split_ens:-1}
  glob_n_split=${glob_n_split_ens:-7}
  glob_layoutx=${glob_layoutx_ens:-12}
  glob_layouty=${glob_layouty_ens:-12}
  glob_npx=${glob_npx_ens:-769}
  glob_npy=${glob_npy_ens:-769}
  glob_io_layoutx=${glob_io_layoutx_ens:-1}
  glob_io_layouty=${glob_io_layouty_ens:-10}
  glob_hord_mt=${glob_hord_mt_ens:-5}
  glob_hord_vt=${glob_hord_vt_ens:-5}
  glob_hord_tm=${glob_hord_tm_ens:-5}
  glob_hord_dp=${glob_hord_dp_ens:-5}
  glob_hord_tr=${glob_hord_tr_ens:--5}
  glob_lim_fac=${glob_lim_fac_ens:-1.0}
  glob_full_zs_filter=${glob_full_zs_filter_ens:-.true.}
  glob_n_zs_filter=${glob_n_zs_filter_ens:-1}
  glob_n_del2_weak=${glob_n_del2_weak_ens:-20}
  glob_max_slope=${glob_max_slope_ens:-0.25}
  glob_kord_tm=${glob_kord_tm_ens:--11}
  glob_kord_mt=${glob_kord_mt_ens:-11}
  glob_kord_wz=${glob_kord_wz_ens:-11}
  glob_kord_tr=${glob_kord_tr_ens:-11}
  glob_fv_core_tau=${glob_fv_core_tau:-10.}
  glob_rf_cutoff=${glob_rf_cutoff:-10.}
  glob_fast_tau_w_sec=${glob_fast_tau_w_sec:-0.2}
  glob_rlmx=${glob_rlmx_ens:-300.}
  glob_elmx=${glob_elmx_ens:-300.}
  glob_sfc_rlm=${glob_sfc_rlm_ens:-1}
  glob_tc_pbl=${glob_tc_pbl_ens:-0}
  glob_shal_cnv=${glob_shal_cnv_ens:-.true.}
  glob_do_deep=${glob_do_deep_ens:-.true.}
  k_split=${k_split_ens:-4}
  n_split=${n_split_ens:-5}
  layoutx=${layoutx_ens:-40}
  layouty=${layouty_ens:-30}
  npx=${npx_ens:-2881}
  npy=${npy_ens:-1921}
  io_layoutx=${io_layoutx_ens:-1}
  io_layouty=${io_layouty_ens:-10}
  hord_mt=${hord_mt_ens:-5}
  hord_vt=${hord_vt_ens:-5}
  hord_tm=${hord_tm_ens:-5}
  hord_dp=${hord_dp_ens:-5}
  hord_tr=${hord_tr_ens:--5}
  lim_fac=${lim_fac_ens:-1.0}
  full_zs_filter=${full_zs_filter_ens:-.true.}
  n_zs_filter=${n_zs_filter_ens:-1}
  n_del2_weak=${n_del2_weak_ens:-20}
  max_slope=${max_slope_ens:-0.25}
  kord_tm=${kord_tm_ens:--11,-11}
  kord_mt=${kord_mt_ens:-11,11}
  kord_wz=${kord_wz_ens:-11,11}
  kord_tr=${kord_tr_ens:-11,11}
  fv_core_tau=${fv_core_tau:-10.}
  rf_cutoff=${rf_cutoff:-10.}
  fast_tau_w_sec=${fast_tau_w_sec:-0.2}
  rlmx=${rlmx_ens:-300.}
  elmx=${elmx_ens:-300.}
  sfc_rlm=${sfc_rlm_ens:-1}
  tc_pbl=${tc_pbl_ens:-0}
  shal_cnv=${shal_cnv_ens:-.true.}
  do_deep=${do_deep_ens:-.true.}
  do_sppt=${do_sppt_ens:-.false.}
  do_shum=${do_shum_ens:-.false.}
  do_skeb=${do_skeb_ens:-.false.}
  npz=${npz_ens:-64}
  output_grid_dlon_ens=${output_grid_dlon_ens:-$(awk "BEGIN {print ${output_grid_dlon:-0.025}*${GRID_RATIO_ENS:-1}}")}
  output_grid_dlat_ens=${output_grid_dlat_ens:-$(awk "BEGIN {print ${output_grid_dlat:-0.025}*${GRID_RATIO_ENS:-1}}")}
  output_grid_dlon=${output_grid_dlon_ens}
  output_grid_dlat=${output_grid_dlat_ens}

  # Additional physics settings
  glob_sedi_semi=${glob_sedi_semi_ens:-.true.}
  glob_cnvgwd=${glob_cnvgwd_ens:-.true.}
  glob_imfshalcnv=${glob_imfshalcnv_ens:-.true.}
  glob_imfdeepcnv=${glob_imfdeepcnv_ens:-.true.}
  sedi_semi=${sedi_semi_ens:-.true.}
  cnvgwd=${cnvgwd_ens:-.true.}
  imfshalcnv=${imfshalcnv_ens:-.true.}
  imfdeepcnv=${imfdeepcnv_ens:-.true.}

  # Smoke/Dust settings (GFS_typedefs.F90 namelist defaults)
  glob_seas_opt=${glob_seas_opt_ens:-2}
  glob_dust_opt=${glob_dust_opt_ens:-1}
  glob_drydep_opt=${glob_drydep_opt_ens:-1}
  glob_coarsepm_settling=${glob_drydep_opt_ens:-1}
  glob_plume_wind_eff=${glob_plume_wind_eff_ens:-1}
  glob_extended_sd_diags=${glob_extended_sd_diags_ens:-.false.}
  glob_wetdep_ls_opt=${glob_wetdep_ls_opt_ens:-1}
  glob_do_plumerise=${glob_do_plumerise_ens:-.false.}
  glob_addsmoke_flag=${glob_addsmoke_flag_ens:-1}
  glob_plumerisefire_frq=${glob_plumerisefire_frq_ens:-60}
  glob_n_dbg_lines=${glob_n_dbg_lines_ens:-3}
  glob_smoke_forecast=${glob_smoke_forecast_ens:-0}
  glob_aero_ind_fdb=${glob_aero_ind_fdb_ens:-.false.}
  glob_aero_dir_fdb=${glob_aero_dir_fdb_ens:-.false.}
  glob_rrfs_smoke_debug=${glob_rrfs_smoke_debug_ens:-.false.}
  glob_do_smoke_transport=${glob_do_smoke_transport_ens:-.true.}
  glob_mix_chem=${glob_mix_chem_ens:-.false.}
  glob_enh_mix=${glob_enh_mix_ens:-.false.}

  seas_opt=${seas_opt_ens:-2}
  dust_opt=${dust_opt_ens:-1}
  drydep_opt=${drydep_opt_ens:-1}
  coarsepm_settling=${drydep_opt_ens:-1}
  plume_wind_eff=${plume_wind_eff_ens:-1}
  extended_sd_diags=${extended_sd_diags_ens:-.false.}
  wetdep_ls_opt=${wetdep_ls_opt_ens:-1}
  do_plumerise=${do_plumerise_ens:-.false.}
  addsmoke_flag=${addsmoke_flag_ens:-1}
  plumerisefire_frq=${plumerisefire_frq_ens:-60}
  n_dbg_lines=${n_dbg_lines_ens:-3}
  smoke_forecast=${smoke_forecast_ens:-0}
  aero_ind_fdb=${aero_ind_fdb_ens:-.false.}
  aero_dir_fdb=${aero_dir_fdb_ens:-.false.}
  rrfs_smoke_debug=${rrfs_smoke_debug_ens:-.false.}
  do_smoke_transport=${do_smoke_transport_ens:-.true.}
  mix_chem=${mix_chem_ens:-.false.}
  enh_mix=${enh_mix_ens:-.false.}

  ## Lighting threat index diagnostic (GFS_typedefs.F90 namelist defaults)
  glob_lightning_threat=${glob_lightning_threat_ens:-.false.}
  lightning_threat=${lightning_threat_ens:-.false.}
fi

iseed1=$(echo $CDATE $ENSID | awk '{print $1*1000+$2*10+3}')
iseed2=$(echo $CDATE $ENSID | awk '{print $1*1000+$2*10+4}')
iseed3=$(echo $CDATE $ENSID | awk '{print $1*1000+$2*10+5}')
iseed4=$(echo $CDATE $ENSID | awk '{print $1*1000+$2*10+6}')
iseed5=$(echo $CDATE $ENSID | awk '{print $1*1000+$2*10+7}')

if [ $do_sppt = .true. ]; then
  iseed_sppt1=$iseed1; iseed_sppt2=$iseed2; iseed_sppt3=$iseed3; iseed_sppt4=$iseed4; iseed_sppt5=$iseed5
else
  iseed_sppt1=0; iseed_sppt2=0; iseed_sppt3=0; iseed_sppt4=0; iseed_sppt5=0
fi
if [ $do_shum = .true. ]; then
  iseed_shum1=$iseed1; iseed_shum2=$iseed2; iseed_shum3=$iseed3; iseed_shum4=$iseed4; iseed_shum5=$iseed5
else
  iseed_shum1=0; iseed_shum2=0; iseed_shum3=0; iseed_shum4=0; iseed_shum5=0
fi
if [ $do_skeb = .true. ]; then
  iseed_skeb1=$iseed1; iseed_skeb2=$iseed2; iseed_skeb3=$iseed3; iseed_skeb4=$iseed4; iseed_skeb5=$iseed5
else
  iseed_skeb1=0; iseed_skeb2=0; iseed_skeb3=0; iseed_skeb4=0; iseed_skeb5=0
fi

halo_blend=${halo_blend:-0}
nstf_n1=${nstf_n1:-2}
nstf_n2=${nstf_n2:-0}
nstf_n3=${nstf_n3:-0}
nstf_n4=${nstf_n4:-0}
nstf_n5=${nstf_n5:-0}

levp=${LEVS}

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
twowaynest=${twowaynest:-.true.}

if [ ${warm_start_opt} -eq 0 ]; then
  warmstart_from_restart=no
  RESTARTinp="UNNEEDED"
fi

# Sepcial settings if this is an atm_init forecast run
if [ ${RUN_INIT:-NO} = YES ]; then

if [ "${ENSDA}" = YES ]; then
  FIXgrid=${FIXgrid:-${WORKhafs}/intercom/grid_ens}
  INPdir=${INPdir:-${WORKhafs}/intercom/chgres_ens/mem${ENSID}}
  OUTdir=${OUTdir:-${WORKhafs}/intercom/forecast_init_ens/mem${ENSID}}
  RESTARTout=${WORKhafs}/intercom/RESTART_init_ens/mem${ENSID}
elif [ ${FGAT_MODEL} = gdas ]; then
  FIXgrid=${FIXgrid:-${WORKhafs}/intercom/grid}
  INPdir=${INPdir:-${WORKhafs}/intercom/chgres_fgat${FGAT_HR}}
  OUTdir=${OUTdir:-${WORKhafs}/intercom/forecast_init_fgat${FGAT_HR}}
  RESTARTout=${WORKhafs}/intercom/RESTART_init_fgat${FGAT_HR}
else
  FIXgrid=${FIXgrid:-${WORKhafs}/intercom/grid}
  INPdir=${INPdir:-${WORKhafs}/intercom/chgres}
  OUTdir=${OUTdir:-${WORKhafs}/intercom/forecast_init}
  RESTARTout=${WORKhafs}/intercom/RESTART_init
fi
NHRS=$(awk "BEGIN {print ${dt_atmos}/3600}")
NHRS_ENS=$(awk "BEGIN {print ${dt_atmos}/3600}")
restart_interval="$(awk "BEGIN {print ${dt_atmos}/3600}") 6"
warm_start_opt=0
run_ocean=no
run_wave=no
ccpp_suite_regional=${ccpp_suite_regional_init:-$ccpp_suite_regional}
ccpp_suite_glob=${ccpp_suite_glob_init:-$ccpp_suite_glob}
ccpp_suite_nest=${ccpp_suite_nest_init:-$ccpp_suite_nest}
nstf_n1=${nstf_n1_init:-$nstf_n1}
nstf_n2=${nstf_n2_init:-$nstf_n2}
nstf_n3=${nstf_n3_init:-$nstf_n3}
nstf_n4=${nstf_n4_init:-$nstf_n4}
nstf_n5=${nstf_n5_init:-$nstf_n5}
glob_layoutx=${glob_layoutx_init:-$glob_layoutx}
glob_layouty=${glob_layouty_init:-$glob_layouty}
layoutx=${layoutx_init:-$layoutx}
layouty=${layouty_init:-$layouty}
write_groups=${write_groups_init:-$write_groups}
write_tasks_per_group=${write_tasks_per_group_init:-$write_tasks_per_group}
is_moving_nest=$(echo ${is_moving_nest} | sed -e 's/.true./.false./g' -e 's/.T./.F./g')
output_grid=$(echo ${output_grid} | sed -e 's/_moving//g')

else # Otherwise this a regular forecast run

if [ "${ENSDA}" = YES ]; then
  FIXgrid=${FIXgrid:-${WORKhafs}/intercom/grid_ens}
  INPdir=${INPdir:-${WORKhafs}/intercom/chgres_ens/mem${ENSID}}
  OUTdir=${OUTdir:-${WORKhafs}/intercom/forecast_ens/mem${ENSID}}
  RESTARTout=${RESTARTout:-${WORKhafs}/intercom/RESTART_ens/mem${ENSID}}
else
  FIXgrid=${FIXgrid:-${WORKhafs}/intercom/grid}
  INPdir=${INPdir:-${WORKhafs}/intercom/chgres}
  OUTdir=${OUTdir:-${WORKhafs}/intercom/forecast}
  RESTARTout=${RESTARTout:-${WORKhafs}/intercom/RESTART}
fi

# Different warm_start_opt options for determinist/ensemble forecast
if [ ${ENSDA} != "YES" ]; then # for deterministic forecast

if [ ${warm_start_opt} -eq -1 ] && [ -s ${COMOLD}/${old_out_prefix}.RESTART/${YMD}.${hh}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${COMOLD}/${old_out_prefix}.RESTART
fi
if [ ${RUN_ATM_INIT} = YES ] && [ -s ${WORKhafs}/intercom/RESTART_init/${YMD}.${hh}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${WORKhafs}/intercom/RESTART_init
  #warm_start_opt=1
fi
if [ ${RUN_ATM_MERGE} = YES ] && [ -s ${WORKhafs}/intercom/RESTART_merge/${YMD}.${hh}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${WORKhafs}/intercom/RESTART_merge
  #warm_start_opt=2
fi
if [ ${RUN_ATM_VI} = YES ] && [ -s ${WORKhafs}/intercom/RESTART_vi/${YMD}.${hh}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${WORKhafs}/intercom/RESTART_vi
  #warm_start_opt=3
fi
if [ ${RUN_GSI} = YES ] && [ -s ${WORKhafs}/intercom/RESTART_analysis/${YMD}.${hh}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${WORKhafs}/intercom/RESTART_analysis
  #warm_start_opt=5
fi
if [ ${RUN_ANALYSIS_MERGE} = YES ] && [ -s ${WORKhafs}/intercom/RESTART_analysis_merge/${YMD}.${hh}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${WORKhafs}/intercom/RESTART_analysis_merge
  #warm_start_opt=6
fi

else # for ENSDA member forecast

if [ ${warm_start_opt} -eq -1 ] && [ -s ${COMOLD}/${old_out_prefix}.RESTART_ens/mem${ENSID}/${YMD}.${hh}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${COMOLD}/${old_out_prefix}.RESTART_ens/mem${ENSID}
fi
if [ ${RUN_ATM_INIT_ENS} = YES ] && [ -s ${WORKhafs}/intercom/RESTART_init_ens/mem${ENSID}/${YMD}.${hh}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${WORKhafs}/intercom/RESTART_init_ens/mem${ENSID}
  #warm_start_opt=1
fi
if [ ${RUN_ATM_MERGE_ENS} = YES ] && [ -s ${WORKhafs}/intercom/RESTART_merge_ens/mem${ENSID}/${YMD}.${hh}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${WORKhafs}/intercom/RESTART_merge_ens/mem${ENSID}
  #warm_start_opt=2
fi
if [ ${RUN_ATM_VI_ENS} = YES ] && [ -s ${WORKhafs}/intercom/RESTART_vi_ens/mem${ENSID}/${YMD}.${hh}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${WORKhafs}/intercom/RESTART_vi_ens/mem${ENSID}
  #warm_start_opt=3
fi
if [ ${RUN_ENKF} = YES ] && [ -s ${WORKhafs}/intercom/RESTART_analysis_ens/mem${ENSID}/${YMD}.${hh}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${WORKhafs}/intercom/RESTART_analysis_ens/mem${ENSID}
  #warm_start_opt=5
fi
if [ ${RUN_ANALYSIS_MERGE_ENS} = YES ] && [ -s ${WORKhafs}/intercom/RESTART_analysis_merge_ens/mem${ENSID}/${YMD}.${hh}0000.fv_core.res.tile1.nc ]; then
  warmstart_from_restart=yes
  RESTARTinp=${WORKhafs}/intercom/RESTART_analysis_merge_ens/mem${ENSID}
  #warm_start_opt=6
fi

fi # ${ENSDA} != "YES"

fi # ${RUN_INIT} = "NO"

NHRSint=$(printf "%.0f" ${NHRS})

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
wav_tasks=${wav_tasks:-120}
med_tasks=${med_tasks:-${ocn_tasks}}
dat_tasks=${dat_tasks:-${ocn_tasks}}
cplflx=${cplflx:-.false.}
cplocn2atm=${cplocn2atm:-.true.}
icplocn2atm=${icplocn2atm:-0}
cplwav=${cplwav:-.false.}
cplwav2atm=${cplwav2atm:-.false.}
INPUT_WNDFLD=${INPUT_WNDFLD:-"C F"}
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
ATM_omp_num_threads=${atm_threads:-${OMP_THREADS:-${OMP_NUM_THREADS:-1}}}
OCN_omp_num_threads=${ocn_threads:-${OMP_THREADS:-${OMP_NUM_THREADS:-1}}}
WAV_omp_num_threads=${wav_threads:-${OMP_THREADS:-${OMP_NUM_THREADS:-1}}}
MED_omp_num_threads=${med_threads:-${OMP_THREADS:-${OMP_NUM_THREADS:-1}}}
DAT_omp_num_threads=${dat_threads:-${OMP_THREADS:-${OMP_NUM_THREADS:-1}}}

pubbasin2=${pubbasin2:-AL}
if [ ${ocean_domain:-auto} = "auto" ]; then

if [ ${pubbasin2} = "AL" ] || [ ${pubbasin2} = "EP" ] || [ ${pubbasin2} = "CP" ] || \
   [ ${pubbasin2} = "SL" ] || [ ${pubbasin2} = "LS" ]; then
  ocean_domain=nhc
elif [ ${pubbasin2} = "WP" ] || [ ${pubbasin2} = "IO" ]; then
  ocean_domain=jtnh
elif [ ${pubbasin2} = "SH" ] || [ ${pubbasin2} = "SP" ] || [ ${pubbasin2} = "SI" ]; then
  ocean_domain=jtsh
else
  echo "FATAL ERROR: Unknown/unsupported basin of ${pubbasin2}"
  exit 1
fi

fi

# CDEPS related settings
run_datm=${run_datm:-no}
run_docn=${run_docn:-no}
mesh_atm=${mesh_atm:-''}
mesh_ocn=${mesh_ocn:-''}

if [ $gtype = regional ]; then
  ATM_tasks=0
  for n in $(seq 1 ${nest_grids}); do
    layoutx_tmp=$( echo ${layoutx} | cut -d , -f ${n} )
    layouty_tmp=$( echo ${layouty} | cut -d , -f ${n} )
    ATM_tasks=$(($ATM_tasks+$layoutx_tmp*$layouty_tmp ))
  done
  if [ $quilting = .true. ]; then
    ATM_tasks=$(($ATM_tasks+$write_groups*$write_tasks_per_group))
  fi
elif [ $gtype = nest ] || [ $gtype = stretch ] || [ $gtype = uniform ]; then
  ATM_tasks=$(( ${glob_layoutx} * ${glob_layouty} * 6 ))
  for n in $(seq 1 ${nest_grids}); do
    layoutx_tmp=$( echo ${layoutx} | cut -d , -f ${n} )
    layouty_tmp=$( echo ${layouty} | cut -d , -f ${n} )
    ATM_tasks=$(($ATM_tasks+$layoutx_tmp*$layouty_tmp ))
  done
  if [ $quilting = .true. ]; then
    ATM_tasks=$(($ATM_tasks+$write_groups*$write_tasks_per_group))
  fi
else
  echo "FATAL ERROR: Unsupported gtype of ${gtype}. Currently onnly support gtype of nest or regional."
  exit 9
fi

ATM_petlist_bounds=$(printf "ATM_petlist_bounds: %04d %04d" 0 $(($ATM_tasks-1)))

if [ ${run_ocean} = yes ] && [ ${ocean_model} = hycom ] && [ ${run_wave} != yes ]; then

ATM_model_component="ATM_model: fv3"
WAV_model_component=""
ATM_model_attribute="ATM_model = fv3"
WAV_model_attribute=""
OCN_model_component="OCN_model: ${ocean_model}"
OCN_model_attribute="OCN_model = ${ocean_model}"

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

fi #if [ ${run_ocean} = yes ] && [ ${ocean_model} = hycom ] && [ ${run_wave} != yes ]; then

if [ ${run_ocean} = yes ] && [ ${ocean_model} = mom6 ] && [ ${run_wave} != yes ]; then

ATM_model_component="ATM_model: fv3"
WAV_model_component=""
ATM_model_attribute="ATM_model = fv3"
WAV_model_attribute=""
OCN_model_component="OCN_model: ${ocean_model}"
OCN_model_attribute="OCN_model = ${ocean_model}"

OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocn_tasks-1)))

# CMEPS based coupling options
if [[ $cpl_atm_ocn = "cmeps"* ]]; then
  EARTH_component_list="EARTH_component_list: ATM OCN MED"
  MED_model_component="MED_model: cmeps"
  MED_model_attribute="MED_model=cmeps"
  MED_petlist_bounds=$(printf "MED_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$med_tasks-1)))
  runSeq_ALL="MED med_phases_cdeps_run\n MED med_phases_prep_atm\n MED med_phases_ocnalb_run\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED -> ATM :remapMethod=redist\n MED -> OCN :remapMethod=redist\n ATM\n OCN\n ATM -> MED :remapMethod=redist\n OCN -> MED :remapMethod=redist\n MED med_phases_post_atm\n MED med_phases_post_ocn"
  # CMEPS based two-way coupling
  if [ $cpl_atm_ocn = cmeps_2way ]; then
    cplflx=.true.
    cplocn2atm=.true.
  # CMEPS based one-way coupling from atm to ocn only
  elif [ $cpl_atm_ocn = cmeps_1way_1to2 ]; then
    cplflx=.true.
    cplocn2atm=.false.
  # CMEPS based one-way coupling from ocn to atm only
  elif [ $cpl_atm_ocn = cmeps_1way_2to1 ]; then
    cplflx=.true.
    cplocn2atm=.true.
  else
    echo "FATAL ERROR: Unsupported coupling option: cpl_atm_ocn=${cpl_atm_ocn}"
    exit 9
  fi
# Currently unsupported coupling option combinations
else
  echo "FATAL ERROR: Unsupported coupling option: cpl_atm_ocn=${cpl_atm_ocn}"
  exit 9
fi

fi #if [ ${run_ocean} = yes ] && [ ${ocean_model} = mom6 ] && [ ${run_wave} != yes ]; then

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
    INPUT_WNDFLD="C F"
    runSeq_ALL="MED med_phases_prep_atm\n MED med_phases_prep_wav_accum\n MED med_phases_prep_wav_avg\n MED -> ATM :remapMethod=redist\n MED -> WAV :remapMethod=redist\n ATM\n WAV\n ATM -> MED :remapMethod=redist\n WAV -> MED :remapMethod=redist\n MED med_phases_post_atm\n MED med_phases_post_wav\n "
  # CMEPS based one-way atm-wav coupling from atm to wav only
  elif [ $cpl_atm_wav = cmeps_1way_1to2 ]; then
    cplflx=.true.
    cplocn2atm=.false.
    cplwav=.true.
    cplwav2atm=.false.
    INPUT_WNDFLD="C F"
    runSeq_ALL="MED med_phases_prep_atm\n MED med_phases_prep_wav_accum\n MED med_phases_prep_wav_avg\n MED -> WAV :remapMethod=redist\n ATM\n WAV\n ATM -> MED :remapMethod=redist\n WAV -> MED :remapMethod=redist\n MED med_phases_post_atm\n MED med_phases_post_wav\n "
  # CMEPS based one-way atm-wav coupling from wav to atm only
  elif [ $cpl_atm_wav = cmeps_1way_2to1 ]; then
    cplflx=.true.
    cplocn2atm=.false.
    cplwav=.true.
    cplwav2atm=.true.
    INPUT_WNDFLD="T F"
    runSeq_ALL="MED med_phases_prep_atm\n MED med_phases_prep_wav_accum\n MED med_phases_prep_wav_avg\n MED -> ATM :remapMethod=redist\n ATM\n WAV\n ATM -> MED :remapMethod=redist\n WAV -> MED :remapMethod=redist\n MED med_phases_post_atm\n MED med_phases_post_wav\n "
  # CMEPS based atm-wav side by side run (no coupling)
  elif [ $cpl_atm_wav = cmeps_sidebyside ]; then
    cplflx=.true.
    cplocn2atm=.false.
    cplwav=.true.
    cplwav2atm=.false.
    INPUT_WNDFLD="T F"
    runSeq_ALL="MED med_phases_prep_atm\n MED med_phases_prep_wav_accum\n MED med_phases_prep_wav_avg\n MED -> ATM :remapMethod=redist\n ATM\n WAV\n ATM -> MED :remapMethod=redist\n WAV -> MED :remapMethod=redist\n MED med_phases_post_atm\n MED med_phases_post_wav\n "
  fi
# Currently unsupported coupling option combinations
else
  echo "FATAL ERROR: Unsupported coupling option combination: cpl_atm_wav=${cpl_atm_wav}"
  exit 9
fi

fi #if [ ${run_ocean} != yes ] && [ ${run_wave} = yes ]; then

if [ ${run_ocean} = yes ] && [ ${ocean_model} = hycom ] && [ ${run_wave} = yes ]; then
EARTH_component_list="EARTH_component_list: ATM OCN WAV MED"
ATM_model_component="ATM_model: fv3"
OCN_model_component="OCN_model: ${ocean_model}"
WAV_model_component="WAV_model: ww3"
MED_model_component="MED_model: cmeps"
ATM_model_attribute="ATM_model = fv3"
OCN_model_attribute="OCN_model = ${ocean_model}"
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
  INPUT_WNDFLD="T F"
  runSeq_ALL="ATM\n OCN\n WAV"
# CMEPS based two-way atm-ocn and atm-wav coupling
elif [ $cpl_atm_ocn = cmeps_2way ] && [ $cpl_atm_wav = cmeps_2way ]; then
  cplflx=.true.
  cplocn2atm=.true.
  cplwav=.true.
  cplwav2atm=.true.
  INPUT_WNDFLD="C F"
  runSeq_ALL="MED med_phases_prep_atm\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED med_phases_prep_wav_accum\n MED med_phases_prep_wav_avg\n MED -> ATM :remapMethod=redist\n MED -> OCN :remapMethod=redist\n MED -> WAV :remapMethod=redist\n ATM\n OCN\n WAV\n ATM -> MED :remapMethod=redist\n OCN -> MED :remapMethod=redist\n WAV -> MED :remapMethod=redist\n MED med_phases_post_atm\n MED med_phases_post_ocn\n MED med_phases_post_wav"
# CMEPS based two-way atm-ocn coupling and one-way atm-wav coupling from atm to wav only
elif [ $cpl_atm_ocn = cmeps_2way ] && [ $cpl_atm_wav = cmeps_1way_1to2 ]; then
  cplflx=.true.
  cplocn2atm=.true.
  cplwav=.true.
  cplwav2atm=.false.
  INPUT_WNDFLD="C F"
  runSeq_ALL="MED med_phases_prep_atm\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED med_phases_prep_wav_accum\n MED med_phases_prep_wav_avg\n MED -> ATM :remapMethod=redist\n MED -> OCN :remapMethod=redist\n MED -> WAV :remapMethod=redist\n ATM\n OCN\n WAV\n ATM -> MED :remapMethod=redist\n OCN -> MED :remapMethod=redist\n WAV -> MED :remapMethod=redist\n MED med_phases_post_atm\n MED med_phases_post_ocn\n MED med_phases_post_wav"
# CMEPS based one-way atm-ocn coupling from atm to ocn only and two-way atm-wav coupling
elif [ $cpl_atm_ocn = cmeps_1way_1to2 ] && [ $cpl_atm_wav = cmeps_2way ]; then
  cplflx=.true.
  cplocn2atm=.false.
  cplwav=.true.
  cplwav2atm=.true.
  INPUT_WNDFLD="C F"
  runSeq_ALL="MED med_phases_prep_atm\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED med_phases_prep_wav_accum\n MED med_phases_prep_wav_avg\n MED -> ATM :remapMethod=redist\n MED -> OCN :remapMethod=redist\n MED -> WAV :remapMethod=redist\n ATM\n OCN\n WAV\n ATM -> MED :remapMethod=redist\n WAV -> MED :remapMethod=redist\n MED med_phases_post_atm\n MED med_phases_post_ocn\n MED med_phases_post_wav"
# CMEPS based one-way atm-ocn coupling from atm to ocn only and one-way atm-wav coupling from atm to wav only
elif [ $cpl_atm_ocn = cmeps_1way_1to2 ] && [ $cpl_atm_wav = cmeps_1way_1to2 ]; then
  cplflx=.true.
  cplocn2atm=.false.
  cplwav=.true.
  cplwav2atm=.false.
  INPUT_WNDFLD="C F"
  runSeq_ALL="MED med_phases_prep_atm\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED med_phases_prep_wav_accum\n MED med_phases_prep_wav_avg\n MED -> ATM :remapMethod=redist\n MED -> OCN :remapMethod=redist\n MED -> WAV :remapMethod=redist\n ATM\n OCN\n WAV\n ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n MED med_phases_post_ocn\n MED med_phases_post_wav"
# CMEPS based atm-ocn-wav side by side run
elif [ $cpl_atm_ocn = cmeps_sidebyside ] && [ $cpl_atm_wav = cmeps_sidebyside ]; then
  cplflx=.true.
  cplocn2atm=.false.
  cplwav=.true.
  cplwav2atm=.false.
  INPUT_WNDFLD="T F"
  runSeq_ALL="MED med_phases_prep_atm\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED med_phases_prep_wav_accum\n MED med_phases_prep_wav_avg\n MED -> ATM :remapMethod=redist\n ATM\n OCN\n WAV\n ATM -> MED :remapMethod=redist\n OCN -> MED :remapMethod=redist\n WAV -> MED :remapMethod=redist\n MED med_phases_post_atm\n MED med_phases_post_ocn\n MED med_phases_post_wav"
# Currently unsupported coupling option combinations
else
  echo "FATAL ERROR: Unsupported coupling options: cpl_atm_ocn=${cpl_atm_ocn}; cpl_atm_wav=${cpl_atm_wav}"
  exit 9
fi

fi #if [ ${run_ocean} = yes ] && [${ocean_model}=hycom] && [ ${run_wave} = yes ]; then

if [ ${run_ocean} = yes ] && [ ${ocean_model} = mom6 ] && [ ${run_wave} = yes ]; then

EARTH_component_list="EARTH_component_list: ATM OCN WAV MED"
ATM_model_component="ATM_model: fv3"
OCN_model_component="OCN_model: ${ocean_model}"
WAV_model_component="WAV_model: ww3"
MED_model_component="MED_model: cmeps"
ATM_model_attribute="ATM_model = fv3"
OCN_model_attribute="OCN_model = ${ocean_model}"
WAV_model_attribute="WAV_model = ww3"
MED_model_attribute="MED_model = cmeps"

OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocn_tasks-1)))
MED_petlist_bounds=$(printf "MED_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$med_tasks-1)))
WAV_petlist_bounds=$(printf "WAV_petlist_bounds: %04d %04d" $(($ATM_tasks+$ocn_tasks)) $(($ATM_tasks+$ocn_tasks+$wav_tasks-1)))

runSeq_ALL="MED med_phases_cdeps_run\n MED med_phases_prep_atm\n MED med_phases_ocnalb_run\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED med_phases_prep_wav_accum\n MED med_phases_prep_wav_avg\n MED -> ATM :remapMethod=redist\n MED -> OCN :remapMethod=redist\n MED -> WAV :remapMethod=redist\n ATM\n OCN\n WAV\n ATM -> MED :remapMethod=redist\n OCN -> MED :remapMethod=redist\n WAV -> MED :remapMethod=redist\n MED med_phases_post_atm\n MED med_phases_post_ocn\n MED med_phases_post_wav"
# CMEPS based two-way atm-ocn and atm-wav coupling
if [ $cpl_atm_ocn = cmeps_2way ] && [ $cpl_atm_wav = cmeps_2way ]; then
  cplflx=.true.
  cplocn2atm=.true.
  cplwav=.true.
  cplwav2atm=.true.
  INPUT_WNDFLD="C F"
# CMEPS based two-way atm-ocn coupling and one-way atm-wav coupling from atm to wav only
elif [ $cpl_atm_ocn = cmeps_2way ] && [ $cpl_atm_wav = cmeps_1way_1to2 ]; then
  cplflx=.true.
  cplocn2atm=.true.
  cplwav=.true.
  cplwav2atm=.false.
  INPUT_WNDFLD="C F"
# CMEPS based one-way atm-ocn coupling from atm to ocn only and two-way atm-wav coupling
elif [ $cpl_atm_ocn = cmeps_1way_1to2 ] && [ $cpl_atm_wav = cmeps_2way ]; then
  cplflx=.true.
  cplocn2atm=.false.
  cplwav=.true.
  cplwav2atm=.true.
  INPUT_WNDFLD="C F"
# CMEPS based one-way atm-ocn coupling from atm to ocn only and one-way atm-wav coupling from atm to wav only
elif [ $cpl_atm_ocn = cmeps_1way_1to2 ] && [ $cpl_atm_wav = cmeps_1way_1to2 ]; then
  cplflx=.true.
  cplocn2atm=.false.
  cplwav=.true.
  cplwav2atm=.false.
  INPUT_WNDFLD="C F"
# Currently unsupported coupling option combinations
else
  echo "FATAL ERROR: Unsupported coupling options: cpl_atm_ocn=${cpl_atm_ocn}; cpl_atm_wav=${cpl_atm_wav}"
  exit 9
fi

fi #if [ ${run_ocean} = yes ] && [${ocean_model}=mom6] && [ ${run_wave} = yes ]; then

# CDEPS data models
if [ ${run_datm} = yes ]; then
  OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocn_tasks-1)))
  MED_petlist_bounds=$(printf "MED_petlist_bounds: %04d %04d" 0 $(($ATM_tasks-1)))
  cplflx=.true.
  cplocn2atm=.false.
  cplwav=.false.
  cplwav2atm=.false.
  INPUT_WNDFLD="T F"
  runSeq_ALL="" # not used yet
elif [ ${run_docn} = yes ]; then
  OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocn_tasks-1)))
  MED_petlist_bounds=$(printf "MED_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocn_tasks-1)))
  cplflx=.true.
  cplocn2atm=.true.
  cplwav=.false.
  cplwav2atm=.false.
  INPUT_WNDFLD="T F"
  runSeq_ALL="" # not used yet
fi

cd ${DATA}

# Prepare the input, output and RESTART dirs
mkdir -p INPUT ${RESTARTout}
if [ ! -e ./RESTART ]; then
  ${NLN} ${RESTARTout} ./RESTART
fi
# Clean up RESTART and OUTdir if not a restart run
if [ ! "${FORECAST_RESTART}" = "YES" ]; then
  rm -f RESTART/*
  rm -rf ${OUTdir}
fi
mkdir -p ${OUTdir}
cd ${OUTdir}
# Remove the symbolic links if existing
rm -f ./INPUT ./RESTART
# Create the symbolic links
${NLN} ${DATA}/INPUT ./INPUT
${NLN} ${RESTARTout} ./RESTART

cd ${DATA}

if [ ${run_datm} = no ]; then

# Link the input IC and/or LBC files into the INPUT dir
if [ ! -d $INPdir ]; then
   echo "FATAL ERROR: Input data dir does not exist: $INPdir"
   exit 9
fi

# Link all the gfs_bndy files here for full forecast ensemble members, but the
# hour 000 and hour 006 lbc files will be replaced below.
if [ ${ENSDA} = YES ] && [ $((10#${ENSID})) -le ${ENS_FCST_SIZE:-10} ]; then
  ${NLN} ${WORKhafs}/intercom/chgres/gfs_bndy.tile7.*.nc INPUT/
fi

${NLN} ${INPdir}/*.nc INPUT/

if [ ${RUN_INIT:-NO} = YES ]; then
  cd INPUT/
  ${NLN} gfs_bndy.tile7.000.nc gfs_bndy.tile7.$(printf "%03d" "$NBDYHRS").nc
  cd ${DATA}
fi

# Link fix files
${NLN} $FIXam/global_solarconstant_noaa_an.txt solarconstant_noaa_an.txt
${NLN} $FIXam/ozprdlos_2015_new_sbuvO3_tclm15_nuchem.f77 global_o3prdlos.f77
${NLN} $FIXam/global_h2o_pltc.f77 global_h2oprdlos.f77
${NLN} $FIXam/global_sfc_emissivity_idx.txt sfc_emissivity_idx.txt
${NLN} $FIXam/global_co2historicaldata_glob.txt co2historicaldata_glob.txt
${NLN} $FIXam/co2monthlycyc.txt co2monthlycyc.txt
${NLN} $FIXam/global_climaeropac_global.txt aerosol.dat
${NLN} $FIXam/global_glacier.2x2.grb .
${NLN} $FIXam/global_maxice.2x2.grb .
${NLN} $FIXam/RTGSST.1982.2012.monthly.clim.grb .
${NLN} $FIXam/global_snoclim.1.875.grb .
${NLN} $FIXam/global_snowfree_albedo.bosu.t1534.3072.1536.rg.grb .
${NLN} $FIXam/global_albedo4.1x1.grb .
${NLN} $FIXam/CFSR.SEAICE.1982.2012.monthly.clim.grb .
${NLN} $FIXam/global_tg3clim.2.6x1.5.grb .
${NLN} $FIXam/global_vegfrac.0.144.decpercent.grb .
${NLN} $FIXam/global_vegtype.igbp.t1534.3072.1536.rg.grb .
${NLN} $FIXam/global_soiltype.statsgo.t1534.3072.1536.rg.grb .
${NLN} $FIXam/global_soilmgldas.t1534.3072.1536.grb .
${NLN} $FIXam/seaice_newland.grb .
${NLN} $FIXam/global_shdmin.0.144x0.144.grb .
${NLN} $FIXam/global_shdmax.0.144x0.144.grb .
${NLN} $FIXam/global_slope.1x1.grb .
${NLN} $FIXam/global_mxsnoalb.uariz.t1534.3072.1536.rg.grb .
${NLN} $FIXam/ugwp_limb_tau.nc .
${NLN} $PARMhafs/noahmptable.tbl .

for file in $(ls ${FIXam}/fix_co2_proj/global_co2historicaldata*); do
  ${NLN} $file $(echo $(basename $file) | sed -e "s/global_//g")
done

# MERRA2 fix files
if [ ${iaer:-111} = 1011 ]; then
  for n in 01 02 03 04 05 06 07 08 09 10 11 12; do
    ${NLN} ${FIXhafs}/fix_aer/merra2.aerclim.2003-2014.m${n}.nc aeroclim.m${n}.nc
  done
  ${NLN} ${FIXhafs}/fix_lut/optics_BC.v1_3.dat  optics_BC.dat
  ${NLN} ${FIXhafs}/fix_lut/optics_OC.v1_3.dat  optics_OC.dat
  ${NLN} ${FIXhafs}/fix_lut/optics_DU.v15_3.dat optics_DU.dat
  ${NLN} ${FIXhafs}/fix_lut/optics_SS.v3_3.dat  optics_SS.dat
  ${NLN} ${FIXhafs}/fix_lut/optics_SU.v1_3.dat  optics_SU.dat
fi

# Fix files for Thompson MP
if [ ${imp_physics:-11} = 8 ]; then
  ${NLN} ${FIXam}/qr_acr_qgV2.dat ./
  ${NLN} ${FIXam}/qr_acr_qsV2.dat ./
  ${NLN} ${FIXam}/CCN_ACTIVATE.BIN ./
  ${NLN} ${FIXam}/freezeH2O.dat ./
fi

if [ $gtype = nest ] || [ $gtype = stretch ] || [ $gtype = uniform ]; then

cd ./INPUT

ntiles=$((6 + ${nest_grids}))

# Copy grid and orography
for itile in $(seq 1 ${ntiles}); do
  ${NLN} $FIXgrid/${CASE}/${CASE}_oro_data.tile${itile}.nc ./oro_data.tile${itile}.nc
  if [ ${use_orog_gsl:-no} = yes ]; then
    ${NLN} $FIXgrid/${CASE}/${CASE}_oro_data_ls.tile${itile}.nc ./oro_data_ls.tile${itile}.nc
    ${NLN} $FIXgrid/${CASE}/${CASE}_oro_data_ss.tile${itile}.nc ./oro_data_ss.tile${itile}.nc
  fi
  ${NLN} $FIXgrid/${CASE}/${CASE}_grid.tile${itile}.nc ./${CASE}_grid.tile${itile}.nc
done
${NLN} $FIXgrid/${CASE}/${CASE}_mosaic.nc ./grid_spec.nc

for itile in $(seq 7 ${ntiles}); do
  inest=$(($itile - 5))
  ${NLN} ${CASE}_grid.tile${itile}.nc grid.nest0${inest}.tile${itile}.nc
  ${NLN} oro_data.tile${itile}.nc oro_data.nest0${inest}.tile${itile}.nc
  if [ ${use_orog_gsl:-no} = yes ]; then
    ${NLN} oro_data_ls.tile${itile}.nc oro_data_ls.nest0${inest}.tile${itile}.nc
    ${NLN} oro_data_ss.tile${itile}.nc oro_data_ss.nest0${inest}.tile${itile}.nc
  fi
  ${NLN} gfs_data.tile${itile}.nc gfs_data.nest0${inest}.tile${itile}.nc
  ${NLN} sfc_data.tile${itile}.nc sfc_data.nest0${inest}.tile${itile}.nc
done

# moving nest
if [[ "${is_moving_nest}" = *".true."* ]] || [[ "${is_moving_nest}" = *".T."* ]]; then
  mkdir -p moving_nest
  cd moving_nest
  rrtmp=$(echo ${refine_ratio} | rev | cut -d, -f1 | rev)
  ${NLN} $FIXgrid/${CASE}/${CASE}_grid.tile6.nc grid.tile6.nc
  ${NLN} $FIXgrid/${CASE}/${CASE}_oro_data.tile6.nc oro_data.tile6.nc
  if [ ${use_orog_gsl:-no} = yes ]; then
    ${NLN} $FIXgrid/${CASE}/${CASE}_oro_data_ls.tile6.nc oro_data_ls.tile6.nc
    ${NLN} $FIXgrid/${CASE}/${CASE}_oro_data_ss.tile6.nc oro_data_ss.tile6.nc
  fi
  ${NLN} $FIXgrid/../grid_mvnest1res/${CASE_mvnest1res}/${CASE_mvnest1res}_grid.tile6.nc grid.tile6.${rrtmp}x.nc
  ${NLN} $FIXgrid/../grid_mvnest1res/${CASE_mvnest1res}/${CASE_mvnest1res}_oro_data.tile6.nc oro_data.tile6.${rrtmp}x.nc
  if [ ${use_orog_gsl:-no} = yes ]; then
    ${NLN} $FIXgrid/../grid_mvnest1res/${CASE_mvnest1res}/${CASE_mvnest1res}_oro_data_ls.tile6.nc oro_data_ls.tile6.${rrtmp}x.nc
    ${NLN} $FIXgrid/../grid_mvnest1res/${CASE_mvnest1res}/${CASE_mvnest1res}_oro_data_ss.tile6.nc oro_data_ss.tile6.${rrtmp}x.nc
  fi
  for var in facsf maximum_snow_albedo slope_type snowfree_albedo soil_type substrate_temperature vegetation_greenness vegetation_type; do
    ${NLN} $FIXgrid/../grid_mvnest1res/${CASE_mvnest1res}/fix_sfc/${CASE_mvnest1res}.${var}.tile6.nc ${var}.tile6.${rrtmp}x.nc
  done
  cd ..
fi

cd ..

# Prepare diag_table, field_table, input.nml, input_nest02.nml, model_configure, and ufs.configure
${NCP} ${PARMforecast}/diag_table.tmp .
if [ ${imp_physics:-11} = 8 ]; then
  if [ ${ltaerosol} = .true. ]; then
    ${NCP} ${PARMforecast}/field_table_thompson_aero ./field_table
  else
    ${NCP} ${PARMforecast}/field_table_thompson ./field_table
  fi
else
  ${NCP} ${PARMforecast}/field_table .
fi
if [ ${progsigma:-.false.} = .true. ] || [ ${progsigma_nest:-.false.} = .true. ] \
  || [ ${progsigma:-.false.} = T ] || [ ${progsigma_nest:-.false.} = T ] ; then
  cat ${PARMforecast}/field_table_addition_progsigma >> field_table
fi
if [ $gtype = stretch ] || [ $gtype = uniform ]; then
  ${NCP} ${PARMforecast}/input.nml.nonest.tmp  input.nml.tmp
else
  ${NCP} ${PARMforecast}/input.nml.tmp .
  ${NCP} ${PARMforecast}/input_nest.nml.tmp .
fi
${NCP} ${PARMforecast}/model_configure.tmp .
${NCP} ${PARMforecast}/ufs.configure.atmonly ./ufs.configure

# NoahMP table file
${NCP} ${PARMforecast}/noahmptable.tbl .

ngrids=$(( ${nest_grids} + 1 ))
glob_pes=$(( ${glob_layoutx} * ${glob_layouty} * 6 ))
grid_pes="${glob_pes}"
tile_coarse="0,${parent_tile}"
refine="0,${refine_ratio}"
for n in $(seq 1 ${nest_grids}); do
  layoutx_tmp=$( echo ${layoutx} | cut -d , -f ${n} )
  layouty_tmp=$( echo ${layouty} | cut -d , -f ${n} )
  grid_pes="${grid_pes},$(( $layoutx_tmp * $layouty_tmp ))"
done

ioffset=1
joffset=1
for n in $(seq 1 ${nest_grids}); do
  inest=$(( ${n} + 1 ))
  istart_nest_tmp=$( echo ${istart_nest} | cut -d , -f ${n} )
  jstart_nest_tmp=$( echo ${jstart_nest} | cut -d , -f ${n} )
  ioffset="$ioffset,$(( ($istart_nest_tmp-1)/2 + 1))"
  joffset="$joffset,$(( ($jstart_nest_tmp-1)/2 + 1))"
done

ccpp_suite_nml=${ccpp_suite_glob}
layoutx_nml=${glob_layoutx}
layouty_nml=${glob_layouty}
io_layoutx_nml=${glob_io_layoutx:-$layoutx_nml}
io_layouty_nml=${glob_io_layouty:-$layouty_nml}
npx_nml=${glob_npx}
npy_nml=${glob_npy}
k_split_nml=${glob_k_split}
n_split_nml=${glob_n_split}
hord_mt_nml=${glob_hord_mt}
hord_vt_nml=${glob_hord_vt}
hord_tm_nml=${glob_hord_tm}
hord_dp_nml=${glob_hord_dp}
hord_tr_nml=${glob_hord_tr}
lim_fac_nml=${glob_lim_fac}
full_zs_filter_nml=${glob_full_zs_filter:-.true.}
n_zs_filter_nml=${glob_n_zs_filter:-1}
n_del2_weak_nml=${glob_n_del2_weak:-20}
max_slope_nml=${glob_max_slope:-0.25}
kord_tm_nml=${glob_kord_tm:--11}
kord_mt_nml=${glob_kord_mt:-11}
kord_wz_nml=${glob_kord_wz:-11}
kord_tr_nml=${glob_kord_tr:-11}
fv_core_tau=${glob_fv_core_tau:-10.}
rf_cutoff=${glob_rf_cutoff:-10.}
fast_tau_w_sec=${glob_fast_tau_w_sec:-0.2}
rlmx_nml=${glob_rlmx:-300.}
elmx_nml=${glob_elmx:-300.}
sfc_rlm_nml=${glob_sfc_rlm:-1}
tc_pbl_nml=${glob_tc_pbl:-0}
shal_cnv_nml=${glob_shal_cnv:-.true.}
do_deep_nml=${glob_do_deep:-.true.}
blocksize=$(( ${npy_nml}/${layouty_nml} ))

sedi_semi_nml=${glob_sedi_semi:-.true.}
cnvgwd_nml=${glob_cnvgwd:-.true.}
imfshalcnv_nml=${glob_imfshalcnv:-.true.}
imfdeepcnv_nml=${glob_imfdeepcnv:-.true.}
seas_opt_nml=${glob_seas_opt:-2}
dust_opt_nml=${glob_dust_opt:-1}
drydep_opt_nml=${glob_drydep_opt:-1}
coarsepm_settling_ens_nml=${glob_coarsepm_settling_ens:-1}
plume_wind_eff_nml=${glob_plume_wind_eff:-1}
extended_sd_diags_nml=${glob_extended_sd_diags:-.false.}
wetdep_ls_opt_nml=${glob_wetdep_ls_opt:-1}
do_plumerise_nml=${glob_do_plumerise:-.false.}
addsmoke_flag_nml=${glob_addsmoke_flag:-1}
plumerisefire_frq_nml=${glob_plumerisefire_frq:-60}
n_dbg_lines_nml=${glob_n_dbg_lines:-3}
smoke_forecast_nml=${glob_smoke_forecast:-0}
aero_ind_fdb_nml=${glob_aero_ind_fdb:-.false.}
aero_dir_fdb_nml=${glob_aero_dir_fdb:-.false.}
rrfs_smoke_debug_nml=${glob_rrfs_smoke_debug:-.false.}
do_smoke_transport_nml=${glob_do_smoke_transport:-.true.}
mix_chem_nml=${glob_mix_chem:-.false.}
enh_mix_nml=${glob_enh_mix:-.false.}

lightning_threat_nml=${glob_lightning_threat:-.false.}

ltaerosol=${ltaerosol:-.false.}
satmedmf=${satmedmf:-.true.}
do_mynnedmf=${do_mynnedmf:-.false.}
do_mynnsfclay=${do_mynnsfclay:-.false.}
cdmbgwd=${cdmbgwd:-1.0,1.0,1.0,1.0}
iopt_sfc=${iopt_sfc:-3}
gwd_opt=${gwd_opt:-2}
do_ugwp_v0=${do_ugwp_v0:-.false.}
do_ugwp_v1=${do_ugwp_v1:-.false.}
do_ugwp_v0_orog_only=${do_ugwp_v0_orog_only:-.false.}
do_ugwp_v0_nst_only=${do_ugwp_v0_nst_only:-.true.}
do_ugwp_v1_w_gsldrag=${do_ugwp_v1_w_gsldrag:-.false.}
do_ugwp_v1_orog_only=${do_ugwp_v1_orog_only:-.false.}
do_gsl_drag_ls_bl=${do_gsl_drag_ls_bl:-.true.}
do_gsl_drag_ss=${do_gsl_drag_ss:-.true.}
do_gsl_drag_tofd=${do_gsl_drag_tofd:-.true.}
bl_mynn_tkeadvect=${bl_mynn_tkeadvect:-.false.}

atparse < input.nml.tmp > input.nml

for n in $(seq 1 ${nest_grids}); do
  inest=$(( ${n} + 1 ))
  ccpp_suite_nml=${ccpp_suite_nest}
  layoutx_nml=$( echo ${layoutx} | cut -d , -f ${n} )
  layouty_nml=$( echo ${layouty} | cut -d , -f ${n} )
  io_layoutx_nml=$( echo ${io_layoutx} | cut -d , -f ${n} )
  io_layouty_nml=$( echo ${io_layouty} | cut -d , -f ${n} )
  npx_nml=$( echo ${npx} | cut -d , -f ${n} )
  npy_nml=$( echo ${npy} | cut -d , -f ${n} )
  k_split_nml=$( echo ${k_split} | cut -d , -f ${n} )
  n_split_nml=$( echo ${n_split} | cut -d , -f ${n} )
  hord_mt_nml=$( echo ${hord_mt} | cut -d , -f ${n} )
  hord_vt_nml=$( echo ${hord_vt} | cut -d , -f ${n} )
  hord_tm_nml=$( echo ${hord_tm} | cut -d , -f ${n} )
  hord_dp_nml=$( echo ${hord_dp} | cut -d , -f ${n} )
  hord_tr_nml=$( echo ${hord_tr} | cut -d , -f ${n} )
  lim_fac_nml=$( echo ${lim_fac} | cut -d , -f ${n} )
  full_zs_filter_nml=$( echo ${full_zs_filter} | cut -d , -f ${n} )
  n_zs_filter_nml=$( echo ${n_zs_filter} | cut -d , -f ${n} )
  n_del2_weak_nml=$( echo ${n_del2_weak} | cut -d , -f ${n} )
  max_slope_nml=$( echo ${max_slope} | cut -d , -f ${n} )
  kord_tm_nml=$( echo ${kord_tm} | cut -d , -f ${n} )
  kord_mt_nml=$( echo ${kord_mt} | cut -d , -f ${n} )
  kord_wz_nml=$( echo ${kord_wz} | cut -d , -f ${n} )
  kord_tr_nml=$( echo ${kord_tr} | cut -d , -f ${n} )
  fv_core_tau=$( echo ${fv_core_tau} | cut -d , -f ${n} )
  rf_cutoff=$( echo ${rf_cutoff} | cut -d , -f ${n} )
  fast_tau_w_sec=$( echo ${fast_tau_w_sec} | cut -d , -f ${n} )
  blocksize=$(( ${npy_nml}/${layouty_nml} ))

  sedi_semi_nml=$( echo ${sedi_semi} | cut -c , -f ${n} )
  cnvgwd_nml=$( echo ${cnvgwd} | cut -d , -f ${n} )
  imfshalcnv_nml=$( echo ${imfshalcnv} | cut -d , -f ${n} )
  imfdeepcnv_nml=$( echo ${imfdeepcnv} | cut -d , -f ${n} )

  # Smoke/dust
  seas_opt_nml=$( echo ${seas_opt} | cut -d , -f ${n} )
  dust_opt_nml=$( echo ${dust_opt} | cut -d , -f ${n} )
  drydep_opt_nml=$( echo ${drydep_opt} | cut -d , -f ${n} )
  coarsepm_settling_nml=$( echo ${coarsepm_settling} | cut -d , -f ${n} )
  plume_wind_eff_nml=$( echo ${plume_wind_eff} | cut -d , -f ${n} )
  extended_sd_diags_nml=$( echo ${extended_sd_diags} | cut -d , -f ${n} )
  wetdep_ls_opt_nml=$( echo ${wetdep_ls_opt} | cut -d , -f ${n} )
  do_plumerise_nml=$( echo ${do_plumerise} | cut -d , -f ${n} )
  addsmoke_flag_nml=$( echo ${addsmoke_flag} | cut -d , -f ${n} )
  plumerisefire_frq_nml=$( echo ${plumerisefire_frq} | cut -d , -f ${n} )
  n_dbg_lines_nml=$( echo ${n_dbg_lines} | cut -d , -f ${n} )
  smoke_forecast_nml=$( echo ${smoke_forecast} | cut -d , -f ${n} )
  aero_ind_fdb_nml=$( echo ${aero_ind_fdb} | cut -d , -f ${n} )
  aero_dir_fdb_nml=$( echo ${aero_dir_fdb} | cut -d , -f ${n} )
  rrfs_smoke_debug_nml=$( echo ${rrfs_smoke_debug} | cut -d , -f ${n} )
  do_smoke_transport_nml=$( echo ${do_smoke_transport} | cut -d , -f ${n} )
  mix_chem_nml=$( echo ${mix_chem} | cut -d , -f ${n} )
  enh_mix_nml=$( echo ${enh_mix} | cut -d , -f ${n} )

  # Lightning threat index
  lightning_threat_nml=$( echo ${lightning_threat} | cut -d , -f ${n} )

  atparse < input_nest.nml.tmp > input_nest0${inest}.nml
done

elif [ $gtype = regional ]; then

cd INPUT

# Prepare tile data and orography for regional
tile=7
# prepare grid and orog files (halo[034])
${NLN} $FIXgrid/${CASE}/${CASE}_grid.tile${tile}.halo?.nc ./
${NLN} $FIXgrid/${CASE}/${CASE}_oro_data.tile${tile}.halo?.nc ./
if [ ${use_orog_gsl:-no} = yes ]; then
  ${NLN} $FIXgrid/${CASE}/${CASE}_oro_data_ls.tile${tile}.nc ./
  ${NLN} $FIXgrid/${CASE}/${CASE}_oro_data_ss.tile${tile}.nc ./
fi
${NLN} $FIXgrid/${CASE}/${CASE}_mosaic.nc ./

${NLN} ${CASE}_mosaic.nc grid_spec.nc
${NLN} ${CASE}_grid.tile7.halo0.nc grid.tile7.halo0.nc
${NLN} ${CASE}_grid.tile7.halo3.nc ${CASE}_grid.tile7.nc
${NLN} ${CASE}_grid.tile7.halo4.nc grid.tile7.halo4.nc
${NLN} ${CASE}_oro_data.tile7.halo0.nc oro_data.nc
${NLN} ${CASE}_oro_data.tile7.halo4.nc oro_data.tile7.halo4.nc
if [ ${use_orog_gsl:-no} = yes ]; then
  ${NLN} ${CASE}_oro_data_ls.tile7.nc oro_data_ls.nc
  ${NLN} ${CASE}_oro_data_ss.tile7.nc oro_data_ss.nc
fi
${NLN} sfc_data.tile7.nc sfc_data.nc
${NLN} gfs_data.tile7.nc gfs_data.nc

# regional with nests
if [ $nest_grids -gt 1 ]; then

ntiles=$((6 + ${nest_grids}))
for itile in $(seq 8 ${ntiles}); do
  ${NLN} $FIXgrid/${CASE}/${CASE}_oro_data.tile${itile}.nc ./oro_data.tile${itile}.nc
  if [ ${use_orog_gsl:-no} = yes ]; then
    ${NLN} $FIXgrid/${CASE}/${CASE}_oro_data_ls.tile${itile}.nc ./oro_data_ls.tile${itile}.nc
    ${NLN} $FIXgrid/${CASE}/${CASE}_oro_data_ss.tile${itile}.nc ./oro_data_ss.tile${itile}.nc
  fi
  ${NLN} $FIXgrid/${CASE}/${CASE}_grid.tile${itile}.nc ./${CASE}_grid.tile${itile}.nc
done

for itile in $(seq 8 ${ntiles}); do
  inest=$(($itile - 6))
  ${NLN} ${CASE}_grid.tile${itile}.nc grid.nest0${inest}.tile${inest}.nc
  ${NLN} oro_data.tile${itile}.nc oro_data.nest0${inest}.tile${inest}.nc
  if [ ${use_orog_gsl:-no} = yes ]; then
    ${NLN} oro_data_ls.tile${itile}.nc oro_data_ls.nest0${inest}.tile${inest}.nc
    ${NLN} oro_data_ss.tile${itile}.nc oro_data_ss.nest0${inest}.tile${inest}.nc
  fi
  ${NLN} gfs_data.tile${itile}.nc gfs_data.nest0${inest}.tile${inest}.nc
  ${NLN} sfc_data.tile${itile}.nc sfc_data.nest0${inest}.tile${inest}.nc
done

fi #if [ $nest_grids -gt 1 ]; then

# moving nest
if [[ "${is_moving_nest}" = *".true."* ]] || [[ "${is_moving_nest}" = *".T."* ]]; then
  mkdir -p moving_nest
  cd moving_nest
  rrtmp=$(echo ${refine_ratio} | rev | cut -d, -f1 | rev)
  ${NLN} $FIXgrid/${CASE}/${CASE}_grid.tile7.halo0.nc grid.tile1.nc
  ${NLN} $FIXgrid/${CASE}/${CASE}_oro_data.tile7.halo0.nc oro_data.tile1.nc
  if [ ${use_orog_gsl:-no} = yes ]; then
    ${NLN} $FIXgrid/${CASE}/${CASE}_oro_data_ls.tile7.nc oro_data_ls.tile1.nc
    ${NLN} $FIXgrid/${CASE}/${CASE}_oro_data_ss.tile7.nc oro_data_ss.tile1.nc
  fi
  ${NLN} $FIXgrid/../grid_mvnest1res/${CASE_mvnest1res}/${CASE_mvnest1res}_grid.tile7.halo0.nc grid.tile1.${rrtmp}x.nc
  ${NLN} $FIXgrid/../grid_mvnest1res/${CASE_mvnest1res}/${CASE_mvnest1res}_oro_data.tile7.halo0.nc oro_data.tile1.${rrtmp}x.nc
  if [ ${use_orog_gsl:-no} = yes ]; then
    ${NLN} $FIXgrid/../grid_mvnest1res/${CASE_mvnest1res}/${CASE_mvnest1res}_oro_data_ls.tile7.nc oro_data_ls.tile1.${rrtmp}x.nc
    ${NLN} $FIXgrid/../grid_mvnest1res/${CASE_mvnest1res}/${CASE_mvnest1res}_oro_data_ss.tile7.nc oro_data_ss.tile1.${rrtmp}x.nc
  fi
  for var in facsf maximum_snow_albedo slope_type snowfree_albedo soil_type substrate_temperature vegetation_greenness vegetation_type; do
    ${NLN} $FIXgrid/../grid_mvnest1res/${CASE_mvnest1res}/fix_sfc/${CASE_mvnest1res}.${var}.tile7.halo0.nc ${var}.tile1.${rrtmp}x.nc
  done
  cd ..
fi

# For warm start from restart files (either before or after analysis)
if [ ! ${FORECAST_RESTART} = YES ] && [ ${warmstart_from_restart} = yes ]; then
  ${NLN} ${RESTARTinp}/${YMD}.${hh}0000.coupler.res ./coupler.res
  sed -i -e "2s/.*/  ${yr}    $(echo ${mn}|sed 's/^0/ /')    $(echo ${dy}|sed 's/^0/ /')    $(echo ${hh}|sed 's/^0/ /')     0     0        Model start time:   year, month, day, hour, minute, second/" ./coupler.res
  sed -i -e "3s/.*/  ${yr}    $(echo ${mn}|sed 's/^0/ /')    $(echo ${dy}|sed 's/^0/ /')    $(echo ${hh}|sed 's/^0/ /')     0     0        Current model time: year, month, day, hour, minute, second/" ./coupler.res
  ${NLN} ${RESTARTinp}/${YMD}.${hh}0000.fv_core.res.nc ./fv_core.res.nc
  ${NLN} ${RESTARTinp}/${YMD}.${hh}0000.fv_srf_wnd.res.tile1.nc ./fv_srf_wnd.res.tile1.nc
  ${NLN} ${RESTARTinp}/${YMD}.${hh}0000.fv_core.res.tile1.nc ./fv_core.res.tile1.nc
  ${NLN} ${RESTARTinp}/${YMD}.${hh}0000.fv_tracer.res.tile1.nc ./fv_tracer.res.tile1.nc
  rm -f phy_data.nc sfc_data.nc
  ${NLN} ${RESTARTinp}/${YMD}.${hh}0000.phy_data.nc ./phy_data.nc
  ${NLN} ${RESTARTinp}/${YMD}.${hh}0000.sfc_data.nc ./sfc_data.nc
  for n in $(seq 2 ${nest_grids}); do
    ${NLN} ${RESTARTinp}/${YMD}.${hh}0000.fv_core.res.nest$(printf %02d ${n}).nc ./fv_core.res.nest$(printf %02d ${n}).nc
    ${NLN} ${RESTARTinp}/${YMD}.${hh}0000.fv_srf_wnd.res.nest$(printf %02d ${n}).tile${n}.nc ./fv_srf_wnd.res.nest$(printf %02d ${n}).tile${n}.nc
    ${NLN} ${RESTARTinp}/${YMD}.${hh}0000.fv_core.res.nest$(printf %02d ${n}).tile${n}.nc ./fv_core.res.nest$(printf %02d ${n}).tile${n}.nc
    ${NLN} ${RESTARTinp}/${YMD}.${hh}0000.fv_tracer.res.nest$(printf %02d ${n}).tile${n}.nc ./fv_tracer.res.nest$(printf %02d ${n}).tile${n}.nc
    rm -f ./phy_data.nest$(printf %02d ${n}).tile${n}.nc
    ${NLN} ${RESTARTinp}/${YMD}.${hh}0000.phy_data.nest$(printf %02d ${n}).tile${n}.nc ./phy_data.nest$(printf %02d ${n}).tile${n}.nc
    rm -f ./sfc_data.nest$(printf %02d ${n}).tile${n}.nc
    ${NLN} ${RESTARTinp}/${YMD}.${hh}0000.sfc_data.nest$(printf %02d ${n}).tile${n}.nc ./sfc_data.nest$(printf %02d ${n}).tile${n}.nc
  # if [ -e ${RESTARTinp}/${YMD}.${hh}0000.fv_BC_ne.res.nest$(printf %02d ${n}).nc ]; then
  #   ${NLN} ${RESTARTinp}/${YMD}.${hh}0000.fv_BC_ne.res.nest$(printf %02d ${n}).nc ./fv_BC_ne.res.nest$(printf %02d ${n}).nc
  # fi
  # if [ -e ${RESTARTinp}/${YMD}.${hh}0000.fv_BC_sw.res.nest$(printf %02d ${n}).nc ]; then
  #   ${NLN} ${RESTARTinp}/${YMD}.${hh}0000.fv_BC_sw.res.nest$(printf %02d ${n}).nc ./fv_BC_sw.res.nest$(printf %02d ${n}).nc
  # fi
  done
fi

if [ ${FORECAST_RESTART} = YES ] && [[ ${FORECAST_RESTART_HR} -gt 0 ]]; then
  RESTARTymdh=$(${NDATE} +${FORECAST_RESTART_HR} ${CDATE})
  RESTARTymd=$(echo ${RESTARTymdh} | cut -c1-8)
  RESTARThh=$(echo ${RESTARTymdh} | cut -c9-10)
  ${NLN} ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.coupler.res ./coupler.res
  ${NLN} ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.fv_core.res.nc ./fv_core.res.nc
  ${NLN} ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.fv_srf_wnd.res.tile1.nc ./fv_srf_wnd.res.tile1.nc
  ${NLN} ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.fv_core.res.tile1.nc ./fv_core.res.tile1.nc
  ${NLN} ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.fv_tracer.res.tile1.nc ./fv_tracer.res.tile1.nc
  ${NLN} ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.phy_data.nc ./phy_data.nc
  ${NLN} ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.sfc_data.nc ./sfc_data.nc
  for n in $(seq 2 ${nest_grids}); do
    ${NLN} ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.fv_core.res.nest$(printf %02d ${n}).nc ./fv_core.res.nest$(printf %02d ${n}).nc
    ${NLN} ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.fv_srf_wnd.res.nest$(printf %02d ${n}).tile${n}.nc ./fv_srf_wnd.res.nest$(printf %02d ${n}).tile${n}.nc
    ${NLN} ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.fv_core.res.nest$(printf %02d ${n}).tile${n}.nc ./fv_core.res.nest$(printf %02d ${n}).tile${n}.nc
    ${NLN} ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.fv_tracer.res.nest$(printf %02d ${n}).tile${n}.nc ./fv_tracer.res.nest$(printf %02d ${n}).tile${n}.nc
    ${NLN} ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.phy_data.nest$(printf %02d ${n}).tile${n}.nc ./phy_data.nest$(printf %02d ${n}).tile${n}.nc
    ${NLN} ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.sfc_data.nest$(printf %02d ${n}).tile${n}.nc ./sfc_data.nest$(printf %02d ${n}).tile${n}.nc
  # if [ -e ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.fv_BC_ne.res.nest$(printf %02d ${n}).nc ]; then
  #   ${NLN} ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.fv_BC_ne.res.nest$(printf %02d ${n}).nc ./fv_BC_ne.res.nest$(printf %02d ${n}).nc
  # fi
  # if [ -e ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.fv_BC_sw.res.nest$(printf %02d ${n}).nc ]; then
  #   ${NLN} ${RESTARTout}/${RESTARTymd}.${RESTARThh}0000.fv_BC_sw.res.nest$(printf %02d ${n}).nc ./fv_BC_sw.res.nest$(printf %02d ${n}).nc
  # fi
  done
fi

cd ..

# Prepare diag_table, field_table, input.nml, input_nest02.nml, model_configure, and ufs.configure
${NCP} ${PARMforecast}/diag_table.tmp .
if [ ${imp_physics:-11} = 8 ]; then
  if [ ${ltaerosol} = .true. ]; then
    ${NCP} ${PARMforecast}/field_table_thompson_aero ./field_table
  else
    ${NCP} ${PARMforecast}/field_table_thompson ./field_table
  fi
else
  ${NCP} ${PARMforecast}/field_table .
fi
if [ ${progsigma:-.false.} = .true. ] || [ ${progsigma_nest:-.false.} = .true. ] \
  || [ ${progsigma:-.false.} = T ] || [ ${progsigma_nest:-.false.} = T ] ; then
  cat ${PARMforecast}/field_table_addition_progsigma >> field_table
fi
if [ $gtype = stretch ] || [ $gtype = uniform ]; then
  ${NCP} ${PARMforecast}/input.nml.nonest.tmp  input.nml.tmp
else
  ${NCP} ${PARMforecast}/input.nml.tmp .
  ${NCP} ${PARMforecast}/input_nest.nml.tmp .
fi
${NCP} ${PARMforecast}/model_configure.tmp .

# NoahMP table file
${NCP} ${PARMforecast}/noahmptable.tbl .

if [ ${ocean_model} = hycom ]; then
  if [ ${run_ocean} = yes ] || [ ${run_wave} = yes ]; then
    ${NCP} ${PARMforecast}/ufs.configure.cpl.tmp ./ufs.configure.tmp
  else
    ${NCP} ${PARMforecast}/ufs.configure.atmonly ./ufs.configure.tmp
  fi
elif [ ${ocean_model} = mom6 ]; then
  if [ ${run_ocean} = yes ] || [ ${run_wave} = yes ]; then
    ${NCP} ${PARMforecast}/ufs.configure.mom6.tmp ./ufs.configure.tmp
    ${NCP} ${PARMforecast}/data_table ./
  else
    ${NCP} ${PARMforecast}/ufs.configure.atmonly ./ufs.configure.tmp
  fi
else
  echo "WARNING: unknow ocean model of ${ocean_model}"
fi

sed -e "s/_EARTH_component_list_/${EARTH_component_list}/g" \
    -e "s/_globalResourceControl_/${globalResourceControl:-false}/g" \
    -e "s/_ATM_model_component_/${ATM_model_component}/g" \
    -e "s/_DAT_model_component_/${DAT_model_component}/g" \
    -e "s/_OCN_model_component_/${OCN_model_component}/g" \
    -e "s/_WAV_model_component_/${WAV_model_component}/g" \
    -e "s/_MED_model_component_/${MED_model_component}/g" \
    -e "s/_ATM_model_attribute_/${ATM_model_attribute}/g" \
    -e "s/_DAT_model_attribute_/${DAT_model_attribute}/g" \
    -e "s/_OCN_model_attribute_/${OCN_model_attribute}/g" \
    -e "s/_WAV_model_attribute_/${WAV_model_attribute}/g" \
    -e "s/_MED_model_attribute_/${MED_model_attribute}/g" \
    -e "s/_ATM_petlist_bounds_/${ATM_petlist_bounds}/g" \
    -e "s/_DAT_petlist_bounds_/${DAT_petlist_bounds}/g" \
    -e "s/_OCN_petlist_bounds_/${OCN_petlist_bounds}/g" \
    -e "s/_WAV_petlist_bounds_/${WAV_petlist_bounds}/g" \
    -e "s/_MED_petlist_bounds_/${MED_petlist_bounds}/g" \
    -e "s/_ATM_omp_num_threads_/${ATM_omp_num_threads}/g" \
    -e "s/_DAT_omp_num_threads_/${DAT_omp_num_threads}/g" \
    -e "s/_OCN_omp_num_threads_/${OCN_omp_num_threads}/g" \
    -e "s/_WAV_omp_num_threads_/${WAV_omp_num_threads}/g" \
    -e "s/_MED_omp_num_threads_/${MED_omp_num_threads}/g" \
    -e "s/_cpl_dt_/${cpl_dt}/g" \
    -e "s/_runSeq_ALL_/${runSeq_ALL}/g" \
    -e "s/_base_dtg_/${base_dtg}/g" \
    -e "s/_ocean_start_dtg_/${ocean_start_dtg}/g" \
    -e "s/_end_hour_/${end_hour}/g" \
    -e "s/_NHRS_/${NHRS}/g" \
    -e "s/_NOUTHRS_/${NOUTHRS}/g" \
    -e "s/_merge_import_/${merge_import:-.false.}/g" \
    -e "/_mesh_atm_/d" \
    -e "s/_mesh_wav_/ww3_mesh.nc/g" \
    -e "s/_multigrid_/false/g" \
    ufs.configure.tmp > ufs.configure

ngrids=${nest_grids}
n=1
layoutx_tmp=$( echo ${layoutx} | cut -d , -f ${n} )
layouty_tmp=$( echo ${layouty} | cut -d , -f ${n} )
grid_pes="$(( $layoutx_tmp * $layouty_tmp ))"
tile_coarse=0
refine=0
ioffset=999
joffset=999

for n in $(seq 2 ${nest_grids}); do
  layoutx_tmp=$( echo ${layoutx} | cut -d , -f ${n} )
  layouty_tmp=$( echo ${layouty} | cut -d , -f ${n} )
  grid_pes="${grid_pes},$(( $layoutx_tmp * $layouty_tmp ))"
  ptile_tmp=$( echo ${parent_tile} | cut -d , -f ${n} )
  tile_coarse="${tile_coarse},$(( $ptile_tmp - 6 ))"
  refine="$refine,$( echo ${refine_ratio} | cut -d , -f ${n} )"
  istart_nest_tmp=$( echo ${istart_nest} | cut -d , -f ${n} )
  jstart_nest_tmp=$( echo ${jstart_nest} | cut -d , -f ${n} )
  ioffset_tmp=$(( ($istart_nest_tmp-1)/2 + 1))
  joffset_tmp=$(( ($jstart_nest_tmp-1)/2 + 1))
  if [ ${FORECAST_RESTART} = YES ] && [[ ${FORECAST_RESTART_HR} -gt 0 ]]; then
    is_moving_nest_tmp=$( echo ${is_moving_nest} | cut -d , -f ${n} )
    if [[ "${is_moving_nest_tmp}" = ".true." ]] || [[ "${is_moving_nest_tmp}" = ".T." ]]; then
      RESTARTymdh=$(${NDATE} +${FORECAST_RESTART_HR} ${CDATE})
      RESTARTymd=$(echo ${RESTARTymdh} | cut -c1-8)
      RESTARThh=$(echo ${RESTARTymdh} | cut -c9-10)
      RESTARTtstr=${RESTARTymd}.${RESTARThh}0000
      fort_patcf_tmp="fort.6$(printf '%02d' ${n})"
      if grep -h "${RESTARTtstr}" ${OUTdir}/${fort_patcf_tmp}* ; then
        ioffset_tmp=$(grep "${RESTARTtstr}" ${OUTdir}/${fort_patcf_tmp}* | tail -n 1 | awk -F',' '{print $7}' | awk '{print $3}')
        joffset_tmp=$(grep "${RESTARTtstr}" ${OUTdir}/${fort_patcf_tmp}* | tail -n 1 | awk -F',' '{print $8}' | awk '{print $3}')
        if [ -z "$ioffset_tmp" ] || [ -z "$joffset_tmp" ]; then
          echo "FATAL ERROR: Cannot find proper ioffset/joffset for the moving nest forecast to restart. Exiting."
          exit 9
        fi
      else
        echo "FATAL ERROR: Cannot find ${RESTARTtstr} in ${OUTdir}/${fort_patcf_tmp} or ${OUTdir}/${fort_patcf_tmp}_save. Exiting."
        exit 9
      fi
    fi
  fi
  ioffset="$ioffset,$ioffset_tmp"
  joffset="$joffset,$joffset_tmp"
done

n=1
ccpp_suite_nml=${ccpp_suite_regional}
layoutx_nml=$( echo ${layoutx} | cut -d , -f ${n} )
layouty_nml=$( echo ${layouty} | cut -d , -f ${n} )
io_layoutx_nml=$( echo ${io_layoutx} | cut -d , -f ${n} )
io_layouty_nml=$( echo ${io_layouty} | cut -d , -f ${n} )
npx_nml=$( echo ${npx} | cut -d , -f ${n} )
npy_nml=$( echo ${npy} | cut -d , -f ${n} )
k_split_nml=$( echo ${k_split} | cut -d , -f ${n} )
n_split_nml=$( echo ${n_split} | cut -d , -f ${n} )
hord_mt_nml=$( echo ${hord_mt} | cut -d , -f ${n} )
hord_vt_nml=$( echo ${hord_vt} | cut -d , -f ${n} )
hord_tm_nml=$( echo ${hord_tm} | cut -d , -f ${n} )
hord_dp_nml=$( echo ${hord_dp} | cut -d , -f ${n} )
hord_tr_nml=$( echo ${hord_tr} | cut -d , -f ${n} )
lim_fac_nml=$( echo ${lim_fac} | cut -d , -f ${n} )
full_zs_filter_nml=$( echo ${full_zs_filter} | cut -d , -f ${n} )
n_zs_filter_nml=$( echo ${n_zs_filter} | cut -d , -f ${n} )
n_del2_weak_nml=$( echo ${n_del2_weak} | cut -d , -f ${n} )
max_slope_nml=$( echo ${max_slope} | cut -d , -f ${n} )
kord_tm_nml=$( echo ${kord_tm} | cut -d , -f ${n} )
kord_mt_nml=$( echo ${kord_mt} | cut -d , -f ${n} )
kord_wz_nml=$( echo ${kord_wz} | cut -d , -f ${n} )
kord_tr_nml=$( echo ${kord_tr} | cut -d , -f ${n} )
rlmx_nml=$( echo ${rlmx} | cut -d , -f ${n} )
elmx_nml=$( echo ${elmx} | cut -d , -f ${n} )
fv_core_tau=$( echo ${fv_core_tau} | cut -d , -f ${n} )
rf_cutoff=$( echo ${rf_cutoff} | cut -d , -f ${n} )
fast_tau_w_sec=$( echo ${fast_tau_w_sec} | cut -d , -f ${n} )
sfc_rlm_nml=$( echo ${sfc_rlm} | cut -d , -f ${n} )
tc_pbl_nml=$( echo ${tc_pbl} | cut -d , -f ${n} )
shal_cnv_nml=$( echo ${shal_cnv} | cut -d , -f ${n} )
do_deep_nml=$( echo ${do_deep} | cut -d , -f ${n} )
bc_update_interval=${NBDYHRS}
nrows_blend=${halo_blend}
blocksize=$(( ${npy_nml}/${layouty_nml} ))

sedi_semi_nml=$( echo ${sedi_semi} | cut -d , -f ${n} )
cnvgwd_nml=$( echo ${cnvgwd} | cut -d , -f ${n} )
imfshalcnv_nml=$( echo ${imfshalcnv} | cut -d , -f ${n} )
imfdeepcnv_nml=$( echo ${imfdeepcnv} | cut -d , -f ${n} )

# Smoke/dust
seas_opt_nml=$( echo ${seas_opt} | cut -d , -f ${n} )
dust_opt_nml=$( echo ${dust_opt} | cut -d , -f ${n} )
drydep_opt_nml=$( echo ${drydep_opt} | cut -d , -f ${n} )
coarsepm_settling_nml=$( echo ${coarsepm_settling} | cut -d , -f ${n} )
plume_wind_eff_nml=$( echo ${plume_wind_eff} | cut -d , -f ${n} )
extended_sd_diags_nml=$( echo ${extended_sd_diags} | cut -d , -f ${n} )
wetdep_ls_opt_nml=$( echo ${wetdep_ls_opt} | cut -d , -f ${n} )
do_plumerise_nml=$( echo ${do_plumerise} | cut -d , -f ${n} )
addsmoke_flag_nml=$( echo ${addsmoke_flag} | cut -d , -f ${n} )
plumerisefire_frq_nml=$( echo ${plumerisefire_frq} | cut -d , -f ${n} )
n_dbg_lines_nml=$( echo ${n_dbg_lines} | cut -d , -f ${n} )
smoke_forecast_nml=$( echo ${smoke_forecast} | cut -d , -f ${n} )
aero_ind_fdb_nml=$( echo ${aero_ind_fdb} | cut -d , -f ${n} )
aero_dir_fdb_nml=$( echo ${aero_dir_fdb} | cut -d , -f ${n} )
rrfs_smoke_debug_nml=$( echo ${rrfs_smoke_debug} | cut -d , -f ${n} )
do_smoke_transport_nml=$( echo ${do_smoke_transport} | cut -d , -f ${n} )
mix_chem_nml=$( echo ${mix_chem} | cut -d , -f ${n} )
enh_mix_nml=$( echo ${enh_mix} | cut -d , -f ${n} )

# Lightning threat index
lightning_threat_nml=$( echo ${lightning_threat} | cut -d , -f ${n} )

atparse < input.nml.tmp > input.nml

for n in $(seq 2 ${nest_grids}); do
  inest=$(( ${n} ))
  ccpp_suite_nml=${ccpp_suite_nest}
  layoutx_nml=$( echo ${layoutx} | cut -d , -f ${n} )
  layouty_nml=$( echo ${layouty} | cut -d , -f ${n} )
  io_layoutx_nml=$( echo ${io_layoutx} | cut -d , -f ${n} )
  io_layouty_nml=$( echo ${io_layouty} | cut -d , -f ${n} )
  npx_nml=$( echo ${npx} | cut -d , -f ${n} )
  npy_nml=$( echo ${npy} | cut -d , -f ${n} )
  k_split_nml=$( echo ${k_split} | cut -d , -f ${n} )
  n_split_nml=$( echo ${n_split} | cut -d , -f ${n} )
  hord_mt_nml=$( echo ${hord_mt} | cut -d , -f ${n} )
  hord_vt_nml=$( echo ${hord_vt} | cut -d , -f ${n} )
  hord_tm_nml=$( echo ${hord_tm} | cut -d , -f ${n} )
  hord_dp_nml=$( echo ${hord_dp} | cut -d , -f ${n} )
  hord_tr_nml=$( echo ${hord_tr} | cut -d , -f ${n} )
  lim_fac_nml=$( echo ${lim_fac} | cut -d , -f ${n} )
  full_zs_filter_nml=$( echo ${full_zs_filter} | cut -d , -f ${n} )
  n_zs_filter_nml=$( echo ${n_zs_filter} | cut -d , -f ${n} )
  n_del2_weak_nml=$( echo ${n_del2_weak} | cut -d , -f ${n} )
  max_slope_nml=$( echo ${max_slope} | cut -d , -f ${n} )
  kord_tm_nml=$( echo ${kord_tm} | cut -d , -f ${n} )
  kord_mt_nml=$( echo ${kord_mt} | cut -d , -f ${n} )
  kord_wz_nml=$( echo ${kord_wz} | cut -d , -f ${n} )
  kord_tr_nml=$( echo ${kord_tr} | cut -d , -f ${n} )
  fv_core_tau=$( echo ${fv_core_tau} | cut -d , -f ${n} )
  rf_cutoff=$( echo ${rf_cutoff} | cut -d , -f ${n} )
  fast_tau_w_sec=$( echo ${fast_tau_w_sec} | cut -d , -f ${n} )
  rlmx_nml=$( echo ${rlmx} | cut -d , -f ${n} )
  elmx_nml=$( echo ${elmx} | cut -d , -f ${n} )
  sfc_rlm_nml=$( echo ${sfc_rlm} | cut -d , -f ${n} )
  tc_pbl_nml=$( echo ${tc_pbl} | cut -d , -f ${n} )
  shal_cnv_nml=$( echo ${shal_cnv} | cut -d , -f ${n} )
  do_deep_nml=$( echo ${do_deep} | cut -d , -f ${n} )
  blocksize=$(( ${npy_nml}/${layouty_nml} ))

  sedi_semi_nml=$( echo ${sedi_semi} | cut -d , -f ${n} )
  cnvgwd_nml=$( echo ${cnvgwd} | cut -d , -f ${n} )
  imfshalcnv_nml=$( echo ${imfshalcnv} | cut -d , -f ${n} )
  imfdeepcnv_nml=$( echo ${imfdeepcnv} | cut -d , -f ${n} )

  # Smoke/dust
  seas_opt_nml=$( echo ${seas_opt} | cut -d , -f ${n} )
  dust_opt_nml=$( echo ${dust_opt} | cut -d , -f ${n} )
  drydep_opt_nml=$( echo ${drydep_opt} | cut -d , -f ${n} )
  coarsepm_settling_nml=$( echo ${coarsepm_settling} | cut -d , -f ${n} )
  plume_wind_eff_nml=$( echo ${plume_wind_eff} | cut -d , -f ${n} )
  extended_sd_diags_nml=$( echo ${extended_sd_diags} | cut -d , -f ${n} )
  wetdep_ls_opt_nml=$( echo ${wetdep_ls_opt} | cut -d , -f ${n} )
  do_plumerise_nml=$( echo ${do_plumerise} | cut -d , -f ${n} )
  addsmoke_flag_nml=$( echo ${addsmoke_flag} | cut -d , -f ${n} )
  plumerisefire_frq_nml=$( echo ${plumerisefire_frq} | cut -d , -f ${n} )
  n_dbg_lines_nml=$( echo ${n_dbg_lines} | cut -d , -f ${n} )
  smoke_forecast_nml=$( echo ${smoke_forecast} | cut -d , -f ${n} )
  aero_ind_fdb_nml=$( echo ${aero_ind_fdb} | cut -d , -f ${n} )
  aero_dir_fdb_nml=$( echo ${aero_dir_fdb} | cut -d , -f ${n} )
  rrfs_smoke_debug_nml=$( echo ${rrfs_smoke_debug} | cut -d , -f ${n} )
  do_smoke_transport_nml=$( echo ${do_smoke_transport} | cut -d , -f ${n} )
  mix_chem_nml=$( echo ${mix_chem} | cut -d , -f ${n} )
  enh_mix_nml=$( echo ${enh_mix} | cut -d , -f ${n} )

  # Lightning threat index
  lightning_threat_nml=$( echo ${lightning_threat} | cut -d , -f ${n} )

  atparse < input_nest.nml.tmp > input_nest0${inest}.nml
done

fi # if regional

fi # if not cdeps datm

if [ $gtype = regional ]; then

if [ ${run_ocean} = yes ] && [ ${ocean_model} = mom6 ]; then
  mkdir -p OUTPUT
  # Ocean IC and OBC
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_ssh_ic.nc INPUT/ocean_ssh_ic.nc
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_ts_ic.nc INPUT/ocean_ts_ic.nc
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_uv_ic.nc INPUT/ocean_uv_ic.nc
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_ssh_obc_east.nc INPUT/ocean_ssh_obc_east.nc
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_ssh_obc_north.nc INPUT/ocean_ssh_obc_north.nc
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_ssh_obc_south.nc INPUT/ocean_ssh_obc_south.nc
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_ssh_obc_west.nc INPUT/ocean_ssh_obc_west.nc
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_ts_obc_east.nc INPUT/ocean_ts_obc_east.nc
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_ts_obc_north.nc INPUT/ocean_ts_obc_north.nc
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_ts_obc_south.nc INPUT/ocean_ts_obc_south.nc
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_ts_obc_west.nc INPUT/ocean_ts_obc_west.nc
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_uv_obc_east.nc INPUT/ocean_uv_obc_east.nc
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_uv_obc_north.nc INPUT/ocean_uv_obc_north.nc
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_uv_obc_south.nc INPUT/ocean_uv_obc_south.nc
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_uv_obc_west.nc INPUT/ocean_uv_obc_west.nc

  # Ocean fix files
  ${NLN} ${FIXmom6}/ocean_vgrid_lev55.nc INPUT/ocean_vgrid.nc
  ${NLN} ${FIXmom6}/${ocean_domain}/ocean_hgrid.nc INPUT/ocean_hgrid.nc
  ${NLN} ${FIXmom6}/${ocean_domain}/ocean_topog.nc INPUT/ocean_topog.nc
  ${NLN} ${FIXmom6}/${ocean_domain}/ocean_mosaic.nc INPUT/ocean_mosaic.nc
  ${NLN} ${FIXmom6}/${ocean_domain}/ocean_geothermal.nc INPUT/ocean_geothermal.nc
  ${NLN} ${FIXmom6}/${ocean_domain}/ocean_tidal_amplitude.nc INPUT/ocean_tidal_amplitude.nc
  ${NLN} ${FIXmom6}/${ocean_domain}/ocean_chla.nc INPUT/ocean_chla.nc
  ${NLN} ${FIXmom6}/runoff.daitren.clim.v20180328.nc INPUT/ocean_runoff_monthly.nc
  ${NLN} ${FIXmom6}/woa23_decav91C0_monthly_sss_04_fill0.nc INPUT/ocean_salt_restore.nc
# ${NLN} ${FIXmom6}/woa23_decav91C0_monthly_sst_04_fill0.nc INPUT/ocean_sst_restore.nc

  # Ocean mesh
  ${NLN} ${FIXmom6}/${ocean_domain}/mom6_mesh.nc INPUT/mom6_mesh.nc
  # Atmospheric forcings from GFS
  ${NLN} ${WORKhafs}/intercom/ocn_prep/mom6/gfs_forcings.nc INPUT/gfs_forcings.nc
  # DATM gfs mesh
  ${NLN} ${FIXhafs}/fix_cdeps/meshes/datm_gfs_mesh.nc INPUT/gfs_mesh.nc
  # Generate stream.config
  ${NCP} ${PARMhafs}/cdeps/stream.config.mom6.IN stream.config.IN
  STREAM_OFFSET=0
  SYEAR=${yr}
  EYEAR=$(${NDATE} +${NHRSint} $CDATE | cut -c1-4)
  INLINE_MESH_OCN="INPUT/gfs_mesh.nc"
  INLINE_STREAM_FILES_OCN="INPUT/gfs_forcings.nc"
  INLINE_MESH_ATM="INPUT/gfs_mesh.nc"
  INLINE_STREAM_FILES_ATM="INPUT/gfs_forcings.nc"
  atparse < ./stream.config.IN > ./stream.config

  # MOM_input
  ${NCP} ${PARMhafs}/mom6/regional/hafs_mom6.input.IN ./hafs_mom6.input.IN
  NIGLOBAL=$(ncks --trd -m INPUT/ocean_ts_ic.nc | grep -E -i ": lonh, size =" | cut -f 7 -d ' ' | uniq)
  NJGLOBAL=$(ncks --trd -m INPUT/ocean_ts_ic.nc | grep -E -i ": lath, size =" | cut -f 7 -d ' ' | uniq)
  atparse < ./hafs_mom6.input.IN > ./MOM_input

fi # if [ ${run_ocean} = yes ] && [ ${ocean_model} = mom6 ]; then

if [ ${run_ocean} = yes ] && [ ${ocean_model} = hycom ]; then
  # link hycom related files
  ${NLN} ${WORKhafs}/intercom/hycominit/hycom_settings hycom_settings
  hycom_basin=$(grep RUNmodIDout ./hycom_settings | cut -c20-)
  # link IC/BC
  ${NLN} ${WORKhafs}/intercom/hycominit/restart_out.a restart_in.a
  ${NLN} ${WORKhafs}/intercom/hycominit/restart_out.b restart_in.b
  # link forcing
  ${NLN} ${WORKhafs}/intercom/hycominit/forcing* .
  ${NLN} forcing.presur.a forcing.mslprs.a
  ${NLN} forcing.presur.b forcing.mslprs.b
  # link hycom limits
  ${NLN} ${WORKhafs}/intercom/hycominit/limits .
  # link fix
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.regional.depth.a regional.depth.a
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.regional.depth.b regional.depth.b
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.regional.grid.a regional.grid.a
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.regional.grid.b regional.grid.b
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.forcing.chl.a forcing.chl.a
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.forcing.chl.b forcing.chl.b
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.iso.sigma.a iso.sigma.a
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.iso.sigma.b iso.sigma.b
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.relax.ssh.a relax.ssh.a
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.relax.ssh.b relax.ssh.b
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.tbaric.a tbaric.a
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.tbaric.b tbaric.b
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.thkdf4.a thkdf4.a
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.thkdf4.b thkdf4.b
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.veldf2.a veldf2.a
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.veldf2.b veldf2.b
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.veldf4.a veldf4.a
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.veldf4.b veldf4.b
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.relax.rmu.a relax.rmu.a
  ${NLN} ${FIXhycom}/hafs_${hycom_basin}.basin.relax.rmu.b relax.rmu.b
  # copy parms
  ${NCP} ${PARMhycom}/hafs_${hycom_basin}.basin.fcst.blkdat.input blkdat.input
  ${NCP} ${PARMhycom}/hafs_${hycom_basin}.basin.ports.input ports.input
  ${NCP} ${PARMhycom}/hafs_${hycom_basin}.basin.patch.input.${ocn_tasks} patch.input
fi #if [ ${run_ocean} = yes ] && [ ${ocean_model} = hycom ]; then

if [ ${run_wave} = yes ]; then
  # link ww3 related files
  ${NLN} ${WORKhafs}/intercom/ww3/mod_def.ww3 mod_def.ww3
  ${NLN} ${WORKhafs}/intercom/ww3/ww3_mesh.nc ww3_mesh.nc
  ${NLN} ${WORKhafs}/intercom/ww3/wind.ww3 wind.ww3
  ${NLN} ${WORKhafs}/intercom/ww3/current.ww3 current.ww3
  ${NLN} ${WORKhafs}/intercom/ww3/restart_init.ww3 restart.ww3
  ${NLN} ${WORKhafs}/intercom/ww3/nest.ww3 nest.ww3
  # copy parms
  ${NCP} ${PARMww3}/ww3_shel.inp_tmpl ./ww3_shel.inp_tmpl
  # generate ww3_shel.inp
  INPUT_CURFLD="F F"
  INPUT_WNDFLD=${INPUT_WNDFLD:-"C F"}
  INPUT_ICEFLD="F F"
  EDATE=$($NDATE +${NHRSint} ${CDATE})
  RDATE=$($NDATE +6 ${CDATE})
  RUN_BEG="${CDATE:0:8} ${CDATE:8:2}0000"
  FLD_BEG=${RUN_BEG}
  PNT_BEG=${RUN_BEG}
  RST_BEG=${RUN_BEG}
  RUN_END="${EDATE:0:8} ${EDATE:8:2}0000"
  FLD_END=${RUN_END}
  PNT_END=${RUN_END}
  RST_END="${RDATE:0:8} ${RDATE:8:2}0000"
  FLD_DT=$((3600*${NOUTHRS}))
  PNT_DT=$((3600*${NOUTHRS}))
  RST_DT=$((3600*6))
  GOFILETYPE=0
  POFILETYPE=0
  OUTPARS_WAV="WND HS T01 T02 DIR FP DP PHS PTP PDIR UST CHA USP"
  atparse < ./ww3_shel.inp_tmpl > ./ww3_shel.inp
  # link 6-hr ww3 restart file from COMhafs for output, needed for warm-start waves for the next forecast cycle
  ${NLN} ${COMhafs}/${out_prefix}.${RUN}.ww3.restart.f006 ./${RDATE:0:8}.${RDATE:8:2}0000.restart.ww3
fi #if [ ${run_wave} = yes ]; then

if [ ${RUN_INIT:-NO} = NO ]; then

# Pass along the grid_spec.nc, atmos_static.nc, oro_data.nc
if [ ${ENSDA} = YES ]; then
  if [ -s ${WORKhafs}/intercom/RESTART_init_ens/mem${ENSID}/grid_spec.nc ]; then
    ${NCP} -p ${WORKhafs}/intercom/RESTART_init_ens/mem${ENSID}/grid_spec*.nc RESTART/
  fi
  if [ -s ${WORKhafs}/intercom/RESTART_init_ens/mem${ENSID}/atmos_static.nc ]; then
    ${NCP} -p ${WORKhafs}/intercom/RESTART_init_ens/mem${ENSID}/atmos_static*.nc RESTART/
  fi
  if [ -s ${WORKhafs}/intercom/RESTART_init_ens/mem${ENSID}/oro_data.nc ]; then
    ${NCP} -p ${WORKhafs}/intercom/RESTART_init_ens/mem${ENSID}/oro_data*.n*c RESTART/
  fi
else
  if [ -s ${WORKhafs}/intercom/RESTART_init/grid_spec.nc ]; then
    ${NCP} -p ${WORKhafs}/intercom/RESTART_init/grid_spec*.nc RESTART/
  fi
  if [ -s ${WORKhafs}/intercom/RESTART_init/atmos_static.nc ]; then
    ${NCP} -p ${WORKhafs}/intercom/RESTART_init/atmos_static*.nc RESTART/
  fi
  if [ -s ${WORKhafs}/intercom/RESTART_init/oro_data.nc ]; then
    ${NCP} -p ${WORKhafs}/intercom/RESTART_init/oro_data*.n*c RESTART/
  fi
fi

fi #if [ ${RUN_INIT:-NO} = NO ]; then

fi #if [ $gtype = nest ]; then

# Prepare CDEPS input, parm, and fix files if required.
if [ ${run_datm} = yes ]; then
  datm_source=${DATM_SOURCE:-ERA5}
  ${NCP} ${PARMforecast}/model_configure.tmp .
  ${NLN} ${mesh_atm} INPUT/DATM_ESMF_mesh.nc
  ${NLN} "$datm_input_path"/DATM_input*nc INPUT/
  # Generate docn.streams from template specific to the model:
  ${NCP} ${PARMhafs}/cdeps/datm_$( echo "$datm_source" | tr A-Z a-z ).streams datm.streams
  for file in INPUT/DATM_input*nc; do
    if [[ -s "$file" ]]; then
      sed -i "/^stream_data_files01:/ s/$/\ \"INPUT\/$(basename $file)\"/" datm.streams
    fi
  done
  endyr=$(${NDATE} +${NHRSint} $CDATE | cut -c1-4)
  sed -i "s/_yearFirst_/$yr/g" datm.streams
  sed -i "s/_yearLast_/$endyr/g" datm.streams
  sed -i "s/_mesh_atm_/INPUT\/DATM_ESMF_mesh.nc/g" datm.streams
  # Generate datm_in and ufs.configure from model-independent templates:
  ${NCP} ${PARMhafs}/cdeps/datm_in .
  sed -i "s/_mesh_atm_/INPUT\/DATM_ESMF_mesh.nc/g" datm_in
  ${NCP} ${PARMforecast}/ufs.configure.cdeps.tmp ./
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
      ufs.configure.cdeps.tmp > ufs.configure
elif [ ${run_docn} = yes ]; then
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
  endyr=$(${NDATE} +${NHRSint} $CDATE | cut -c1-4)
  sed -i "s/_yearFirst_/$yr/g" docn.streams
  sed -i "s/_yearLast_/$endyr/g" docn.streams
  sed -i "s/_mesh_ocn_/INPUT\/DOCN_ESMF_mesh.nc/g" docn.streams
  for file in INPUT/oisst*.nc INPUT/sst*.nc INPUT/DOCN_input*.nc; do
    if [[ -s "$file" ]]; then
      sed -i "/^stream_data_files01:/ s/$/\ \"INPUT\/$(basename $file)\"/" docn.streams
    fi
  done
  ${NLN} "${mesh_ocn}" INPUT/DOCN_ESMF_mesh.nc
  ${NCP} ${PARMforecast}/ufs.configure.cdeps.tmp ./
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
      ufs.configure.cdeps.tmp > ufs.configure
fi

# Generate model_configure
SYEAR=${yr}
SMONTH=${mn}
SDAY=${dy}
SHOUR=${hh}
FHMAX=${NHRS}
FHROT=${FHROT:-${FORECAST_RESTART_HR:-0}}
DT_ATMOS=${dt_atmos}
RESTART_INTERVAL=${restart_interval}
QUILTING=${quilting}
QUILTING_RESTART=${quilting_restart:-${quilting}}
#QUILTING_RESTART=.false.
WRITE_GROUP=${write_groups}
WRTTASK_PER_GROUP=${write_tasks_per_group}
WRITE_DOPOST=${write_dopost:-.false.}
OUTPUT_HISTORY=${output_history:-.true.}
NUM_FILES=2
FILENAME_BASE="'atm' 'sfc'"
OUTPUT_FILE="'netcdf_parallel' 'netcdf_parallel'"
#OUTPUT_FILE="'netcdf' 'netcdf'"
IDEFLATE=${ideflate:-1}
ZSTANDARD_LEVEL=${zstandard_level:-1}
QUANTIZE_NSD=0
OUTPUT_FH="${NOUTHRS:-3} -1"
NBITS=0

if [ $gtype = regional ]; then
  ngrids=${nest_grids}
elif [ $gtype = nest ] || [ $gtype = stretch ] || [ $gtype = uniform ]; then
  ngrids=$(( ${nest_grids} + 1 ))
else
  echo "FATAL ERROR: Unsupported gtype of ${gtype}. Currently onnly support gtype of nest or regional."
  exit 9
fi

for n in $(seq 1 ${ngrids}); do
  if [ $n -eq 1 ]; then
    nstr=""
  else
    nstr="_${n}"
  fi
  outputgrid=$(echo ${output_grid} | cut -d , -f ${n})
  clon=$(echo ${output_grid_cen_lon} | cut -d , -f ${n})
  clat=$(echo ${output_grid_cen_lat} | cut -d , -f ${n})
  if [[ "$outputgrid" = "rotated_latlon"* ]]; then
    clontmp=0.0
    clattmp=0.0
  else
    clontmp=${clon}
    clattmp=${clat}
  fi
  lon_span=$(echo ${output_grid_lon_span} | cut -d , -f ${n})
  lat_span=$(echo ${output_grid_lat_span} | cut -d , -f ${n})
  dlon=$(echo ${output_grid_dlon} | cut -d , -f ${n})
  dlat=$(echo ${output_grid_dlat} | cut -d , -f ${n})
  lon1=$( printf "%.6f" $(bc <<< "scale=6; ${clontmp}-${lon_span}/2.0") )
  lat1=$( printf "%.6f" $(bc <<< "scale=6; ${clattmp}-${lat_span}/2.0") )
  lon2=$( printf "%.6f" $(bc <<< "scale=6; ${clontmp}+${lon_span}/2.0") )
  lat2=$( printf "%.6f" $(bc <<< "scale=6; ${clattmp}+${lat_span}/2.0") )
  imo=$( printf "%.0f" $(bc <<< "scale=6; ${lon_span}/${dlon} + 1") )
  jmo=$( printf "%.0f" $(bc <<< "scale=6; ${lat_span}/${dlat} + 1") )
  eval OUTPUT_GRID${nstr}=${outputgrid}
  eval CEN_LON${nstr}=${clon}
  eval CEN_LAT${nstr}=${clat}
  eval LON1${nstr}=${lon1}
  eval LAT1${nstr}=${lat1}
  eval LON2${nstr}=${lon2}
  eval LAT2${nstr}=${lat2}
  eval DLON${nstr}=${dlon}
  eval DLAT${nstr}=${dlat}
  eval IMO${nstr}=${imo}
  eval JMO${nstr}=${jmo}
  eval STDLAT1${nstr}=$(echo ${output_grid_stdlat1:-""} | cut -d , -f ${n})
  eval STDLAT2${nstr}=$(echo ${output_grid_stdlat2:-""} | cut -d , -f ${n})
  eval NX${nstr}=$(echo ${output_grid_nx:-""} | cut -d , -f ${n})
  eval NY${nstr}=$(echo ${output_grid_ny:-""} | cut -d , -f ${n})
  eval DX${nstr}=$(echo ${output_grid_dx:-""} | cut -d , -f ${n})
  eval DY${nstr}=$(echo ${output_grid_dy:-""} | cut -d , -f ${n})
done

for n in $(seq $((${ngrids}+1)) 6); do
  nstr=$(printf "_%0.2d" $n)
  sed -i -e "/<output_grid${nstr}>/,/<\/output_grid${nstr}>/d" model_configure.tmp
done

atparse < model_configure.tmp > model_configure

# Generate diag_table
if [ ${RUN_INIT:-NO} = YES ]; then
  GRID_MSPEC_INT=-1
  ATMOS_DIAG_INT=-1
else
  GRID_MSPEC_INT=${GRID_MSPEC_INT:-3}
  ATMOS_DIAG_INT=${ATMOS_DIAG_INT:-3}
fi
if [ ${run_datm} = no ]; then
  atparse < diag_table.tmp > diag_table
fi
# Remove the grid_mspec lines if it is not a moving nesting configuration
if [[ "${is_moving_nest:-".false."}" = *".true."* ]] || [[ "${is_moving_nest:-".false."}" = *".T."* ]] || [[ ${RUN_INIT:-NO} = YES ]]; then
  echo "This is a moving nesting configuration"
else
  sed -i -e "/grid_mspec/d" diag_table
fi

# Prepare files needed by inline_post
if [ ${write_dopost:-.false.} = .true. ]; then
  ${NCP} ${PARMhafs}/post/itag ./itag
  ${NCP} ${PARMhafs}/post/params_grib2_tbl_new ./params_grib2_tbl_new
  if [ ${satpost:-.false.} = .true. ]; then
    ${NCP} ${postxconfig_satpost} ./postxconfig-NT.txt
    ${NCP} ${postxconfig_satpost} ./postxconfig-NT_FH00.txt
    # Link crtm fix files
    for file in "amsre_aqua" "imgr_g11" "imgr_g12" "imgr_g13" \
      "imgr_g15" "imgr_mt1r" "imgr_mt2" "seviri_m10" \
      "ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16" \
      "ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" \
      "tmi_trmm" "v.seviri_m10" "imgr_insat3d" "abi_gr" "ahi_himawari8" \
       "abi_g16" "abi_g17" ; do
      ${NLN} ${FIXcrtm}/${file}.TauCoeff.bin ./
      ${NLN} ${FIXcrtm}/${file}.SpcCoeff.bin ./
    done
    for file in "Aerosol" "Cloud"; do
      ${NLN} ${FIXcrtm}/${file}Coeff.bin ./
    done
    for file in ${FIXcrtm}/*Emis*; do
      ${NLN} ${file} ./
    done
  else
    ${NCP} ${postxconfig_nosat} ./postxconfig-NT.txt
    ${NCP} ${postxconfig_nosat} ./postxconfig-NT_FH00.txt
  fi
fi

# Copy the fd_ufs.yaml file
${NCP} ${HOMEhafs}/sorc/hafs_forecast.fd/tests/parm/fd_ufs.yaml ./

#-------------------------------------------------------------------------------
# Generate symbolic links for forecast output
FHR=${FHRB:-0}
FHR3=$(printf "%03d" "$FHR")
FHRI=${FHRI:-${NOUTHRS:-3}}
FHRE=${FHRE:-${NHRSint:-$NHRS}}

# Loop for forecast hours
while [ $FHR -le ${FHRE} ]; do

NEWDATE=$(${NDATE} +${FHR} $CDATE)
YYYY=$(echo $NEWDATE | cut -c1-4)
MM=$(echo $NEWDATE | cut -c5-6)
DD=$(echo $NEWDATE | cut -c7-8)
HH=$(echo $NEWDATE | cut -c9-10)

# Clean up previously generated log.atm.fhhh files if FHR > FORECAST_RESTART_HR
if [ ${FHR} -gt ${FORECAST_RESTART_HR} ]; then
  rm -f ${OUTdir}/log.atm.f${FHR3}
fi
${NLN} ${OUTdir}/log.atm.f${FHR3} ./

if [ ${gtype} = nest ]; then
  ngrids=$((${nest_grids} + 1))
else
  ngrids=${nest_grids}
fi

# Loop for grids/domains
for ng in $(seq 1 ${ngrids}); do

if [[ $ng -eq 1 ]]; then
  neststr=""
  tilestr=".tile1"
  nesttilestr=""
  nestdotstr=""
else
  neststr=".nest$(printf '%02d' ${ng})"
  tilestr=".tile$(printf '%d' ${ng})"
  nesttilestr=".nest$(printf '%02d' ${ng}).tile$(printf '%d' ${ng})"
  nestdotstr=".nest$(printf '%02d' ${ng})."
fi

${NLN} ${OUTdir}/atm${nestdotstr}f${FHR3}.nc ./
${NLN} ${OUTdir}/sfc${nestdotstr}f${FHR3}.nc ./

if [ ${gtype} = regional ]; then

grid_spec=grid_spec${nesttilestr}.nc
atmos_static=atmos_static${nesttilestr}.nc
if [[ -z "$neststr" ]] && [[ $tilestr = ".tile1" ]]; then
  grid_mspec=grid_mspec${neststr}_${YYYY}_${MM}_${DD}_${HH}.nc
  atmos_diag=atmos_diag${neststr}_${YYYY}_${MM}_${DD}_${HH}.nc
else
  grid_mspec=grid_mspec${neststr}_${YYYY}_${MM}_${DD}_${HH}${tilestr}.nc
  atmos_diag=atmos_diag${neststr}_${YYYY}_${MM}_${DD}_${HH}${tilestr}.nc
fi

${NLN} ${OUTdir}/${grid_spec} ./
${NLN} ${OUTdir}/${atmos_static} ./

if [ ${RUN_INIT:-NO} = YES ] && [ $FHR -eq 0 ] ; then
  ${NLN} ${OUTdir}/${grid_mspec} ./
# ${NLN} ${OUTdir}/${atmos_diag} ./
fi
if [ $FHR -gt 0 ] ; then
  ${NLN} ${OUTdir}/${grid_mspec} ./
# ${NLN} ${OUTdir}/${atmos_diag} ./
fi

is_moving_nest_tmp=$( echo ${is_moving_nest} | cut -d , -f ${ng} )
if [[ "${is_moving_nest_tmp}" = ".true." ]] || [[ "${is_moving_nest_tmp}" = ".T." ]]; then
  if [ $FHR -eq 0 ]; then
    fort_patcf="fort.6$(printf '%02d' ${ng})"
    if [ -s ${OUTdir}/${fort_patcf} ] && [ ${OUTdir}/${fort_patcf} -nt ${OUTdir}/${fort_patcf}_save ]; then
      cat ${OUTdir}/${fort_patcf} >> ${OUTdir}/${fort_patcf}_save
	fi
    ${NLN} ${OUTdir}/${fort_patcf} ./
  fi
fi

fi #if [ ${gtype} = regional ]; then

done
# End loop for grids/domains

FHR=$(($FHR + $FHRI))
FHR3=$(printf "%03d" "$FHR")

done
# End loop for forecast hours
#-------------------------------------------------------------------------------

# Copy the executable and run the forecast
FORECASTEXEC=${FORECASTEXEC:-${EXEChafs}/hafs_forecast.x}
${NCP} -p ${FORECASTEXEC} ./hafs_forecast.x
#${APRUNC} ./hafs_forecast.x > forecast.log 2>&1
#status=$?; [[ $status -ne 0 ]] && exit $status
#cat forecast.log
set -o pipefail
${APRUNC} ./hafs_forecast.x 2>&1 | tee forecast.log
set +o pipefail

if [ $gtype = regional ] && [ ${run_datm} = no ]; then

# Rename the restart files with a proper convention if needed
cd RESTART
CDATEnhrs=$(${NDATE} ${NHRSint} $CDATE)
YMDnhrs=$(echo ${CDATEnhrs} | cut -c1-8)
yrnhrs=$(echo $CDATEnhrs | cut -c1-4)
mnnhrs=$(echo $CDATEnhrs | cut -c5-6)
dynhrs=$(echo $CDATEnhrs | cut -c7-8)
hhnhrs=$(echo ${CDATEnhrs} | cut -c9-10)
if [ -s ${YMDnhrs}.${hhnhrs}*.fv_core.res.nc ]; then
  for file in $(/bin/ls -1 ${YMDnhrs}.${hhnhrs}*.fv*.nc* ${YMDnhrs}.${hhnhrs}*.phy_data*.nc* ${YMDnhrs}.${hhnhrs}*.sfc_data*.nc* ${YMDnhrs}.${hhnhrs}*.coupler.res); do
    if [ ! -s ${YMDnhrs}.${hhnhrs}0000.${file:16} ]; then
      mv ${file} ${YMDnhrs}.${hhnhrs}0000.${file:16}
    fi
  done
  if [ ${RUN_INIT:-NO} = YES ]; then
    sed -i -e "3s/.*/  ${yrnhrs}    $(echo ${mnnhrs}|sed 's/^0/ /')    $(echo ${dynhrs}|sed 's/^0/ /')    $(echo ${hhnhrs}|sed 's/^0/ /')     0     0        Current model time: year, month, day, hour, minute, second/" ${YMDnhrs}.${hhnhrs}0000.coupler.res
  fi
fi
cd ${DATA}

fi # if [ $gtype = regional ] && [ ${run_datm} = no ]; then

exit
