#!/bin/sh

set -xe

NCP=${NCP:-'/bin/cp'}
NLN=${NLN:-'/bin/ln -sf'}
NDATE=${NDATE:-ndate}

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
FIXam=${FIXam:-${FIXhafs}/fix_am}
FIXhycom=${FIXhycom:-${FIXhafs}/fix_hycom}
FORECASTEXEC=${FORECASTEXEC:-${EXEChafs}/hafs_forecast.x}

ENSDA=${ENSDA:-NO}

# Set options specific to the deterministic/ensemble forecast
if [ "${ENSDA}" != YES ]; then
  NHRS=${NHRS:-126}
  NBDYHRS=${NBDYHRS:-3}
  CASE=${CASE:-C768}
  CRES=$(echo $CASE | cut -c 2-)
  gtype=${gtype:-regional}
  LEVS=${LEVS:-65}
  stretch_fac=${stretch_fac:-1.0001}
  target_lon=${target_lon:--62.0}
  target_lat=${target_lat:-22.0}
  refine_ratio=${refine_ratio:-4}
  deflate_level=${deflate_level:--1}
  ccpp_suite_regional=${ccpp_suite_regional:-HAFS_v0_gfdlmp}
  ccpp_suite_glob=${ccpp_suite_glob:-HAFS_v0_gfdlmp}
  ccpp_suite_nest=${ccpp_suite_nest:-HAFS_v0_gfdlmp}
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
  CASE=${CASE_ENS:-C768}
  CRES=$(echo $CASE | cut -c 2-)
  gtype=${gtype_ens:-regional}
  LEVS=${LEVS_ENS:-65}
  stretch_fac=${stretch_fac_ens:-1.0001}
  target_lon=${target_lon_ens:--62.0}
  target_lat=${target_lat_ens:-22.0}
  refine_ratio=${refine_ratio_ens:-4}
  deflate_level=${deflate_level:-1}
  ccpp_suite_regional=${ccpp_suite_regional_ens:-HAFS_v0_gfdlmp}
  ccpp_suite_glob=${ccpp_suite_glob_ens:-HAFS_v0_gfdlmp}
  ccpp_suite_nest=${ccpp_suite_nest_ens:-HAFS_v0_gfdlmp}
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
cpl_ocean=${cpl_ocean:-0}
ocean_tasks=${ocean_tasks:-120}
cplflx=${cplflx:-.false.}
cpl_dt=${cpl_dt:-360}
ocean_start_dtg=${ocean_start_dtg:-43340.00000}
#base_dtg=${CDATE:-2019082900}
#end_hour=${NHRS:-126}
merge_import=${merge_import:-.false.}

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

# run ocean model side by side (no coupling)
if [ ${run_ocean} = yes ] && [ $cpl_ocean -eq 0 ];  then
  cplflx=.false.
  OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocean_tasks-1)))
  MED_petlist_bounds=""
  runSeq_ALL="ATM\n OCN"
fi
# direct coupling through the nearest point regridding method
if [ ${run_ocean} = yes ] && [ $cpl_ocean -eq 1 ];  then
  cplflx=.true.
  runSeq_ALL="OCN -> ATM :remapMethod=nearest_stod:srcmaskvalues=0\n ATM -> OCN :remapMethod=nearest_stod:srcmaskvalues=1:dstmaskvalues=0\n ATM\n OCN"
  OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocean_tasks-1)))
  MED_petlist_bounds=""
fi
# direct coupling through the bilinear regridding method
if [ ${run_ocean} = yes ] && [ $cpl_ocean -eq 2 ];  then
  cplflx=.true.
  OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocean_tasks-1)))
  MED_petlist_bounds=""
  runSeq_ALL="OCN -> ATM :remapMethod=bilinear:unmappedaction=ignore:zeroregion=select:srcmaskvalues=0\n ATM -> OCN :remapMethod=bilinear:unmappedaction=ignore:zeroregion=select:srcmaskvalues=1:dstmaskvalues=0\n ATM\n OCN"
fi
# CMEPS based coupling through the bilinear regridding method
if [ ${run_ocean} = yes ] && [ $cpl_ocean -eq 3 ];  then
  cplflx=.true.
  OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocean_tasks-1)))
  MED_petlist_bounds=$(printf "MED_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocean_tasks-1)))
  runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n OCN -> MED :remapMethod=redist\n MED med_phases_post_ocn\n MED med_phases_prep_atm\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED -> ATM :remapMethod=redist\n MED -> OCN :remapMethod=redist\n ATM\n OCN"
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

# Link the input IC and/or LBC files into the INPUT dir
if [ ! -d $INPdir ]; then
   echo "FATAL ERROR: Input data dir does not exist: $INPdir"
   exit 9
fi
mkdir -p INPUT
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
elif [ $gtype = uniform ] || [ $gtype = stretch ];  then
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
${NCP} ${PARMforecast}/data_table .
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

sed -e "s/_fhmax_/${NHRS}/g" \
    -e "s/_blocksize_/${blocksize:-64}/g" \
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
    -e "s/_merge_import_/${merge_import:-.false.}/g" \
    input.nml.tmp > input.nml

blocksize=$(( ${npy}/${layouty} ))
sed -e "s/_fhmax_/${NHRS}/g" \
    -e "s/_blocksize_/${blocksize:-64}/g" \
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
  ${NLN} ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ./fv_core.res.tile1.nc
  ${NLN} ${RESTARTinp}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ./fv_tracer.res.tile1.nc
fi

cd ..

# Prepare data_table, diag_table, field_table, input.nml, input_nest02.nml,
# model_configure, and nems.configure
${NCP} ${PARMforecast}/data_table .
${NCP} ${PARMforecast}/diag_table.tmp .
${NCP} ${PARMforecast}/field_table .
${NCP} ${PARMforecast}/input.nml.tmp .
${NCP} ${PARMforecast}/model_configure.tmp .

if [ ${run_ocean} = yes ]; then
  if [[ ${cpl_ocean} -eq 3 ]]; then
    ${NCP} ${PARMforecast}/nems.configure.atm_ocn_cmeps.tmp ./nems.configure.tmp
  else
    ${NCP} ${PARMforecast}/nems.configure.atm_ocn.tmp ./nems.configure.tmp
  fi
  sed -e "s/_ATM_petlist_bounds_/${ATM_petlist_bounds}/g" \
      -e "s/_OCN_petlist_bounds_/${OCN_petlist_bounds}/g" \
      -e "s/_MED_petlist_bounds_/${MED_petlist_bounds}/g" \
      -e "s/_cpl_dt_/${cpl_dt}/g" \
      -e "s/_runSeq_ALL_/${runSeq_ALL}/g" \
      -e "s/_base_dtg_/${CDATE}/g" \
      -e "s/_ocean_start_dtg_/${ocean_start_dtg}/g" \
      -e "s/_end_hour_/${NHRS}/g" \
      -e "s/_merge_import_/${merge_import:-.false.}/g" \
      nems.configure.tmp > nems.configure
elif [ ${run_ocean} = no ];  then
  ${NCP} ${PARMforecast}/nems.configure.atmonly ./nems.configure
else
  echo "FATAL ERROR: Unknown run_ocean option: ${run_ocean}"
  exit 9
fi

blocksize=$(( ${npy}/${layouty} ))
sed -e "s/_fhmax_/${NHRS}/g" \
    -e "s/_blocksize_/${blocksize:-64}/g" \
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
    -e "s/_merge_import_/${merge_import:-.false.}/g" \
    input.nml.tmp > input.nml

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
  ${NCP} ${PARMhycom}/hafs_${hycom_basin}.basin.patch.input.${ocean_tasks} patch.input
  # create hycom limits
  ${USHhafs}/hafs_hycom_limits.py ${CDATE}
fi #if [ ${run_ocean} = yes ];  then

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
cat temp diag_table.tmp > diag_table

sed -e "s/NTASKS/${TOTAL_TASKS}/g" -e "s/YR/$yr/g" \
    -e "s/MN/$mn/g" -e "s/DY/$dy/g" -e "s/H_R/$cyc/g" \
    -e "s/NHRS/$NHRS/g" -e "s/NTHRD/$OMP_NUM_THREADS/g" \
    -e "s/NCNODE/$NCNODE/g" \
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
    -e "s/_cpl_/${cplflx:-.false.}/g" \
    model_configure.tmp > model_configure

# Copy fix files needed by inline_post
if [ ${write_dopost:-.false.} = .true. ]; then
  ${NCP} ${PARMhafs}/post/itag                    ./itag
  ${NCP} ${PARMhafs}/post/postxconfig-NT-hafs.txt ./postxconfig-NT.txt
  ${NCP} ${PARMhafs}/post/postxconfig-NT-hafs.txt ./postxconfig-NT_FH00.txt
  ${NCP} ${PARMhafs}/post/params_grib2_tbl_new    ./params_grib2_tbl_new
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

if [ $gtype = regional ]; then

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

fi # if [ $gtype = regional ]; then

exit
