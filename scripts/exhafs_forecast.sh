#!/bin/sh

set -xe

#ulimit -s 6000000 
#unlimited
ulimit -s unlimited
ulimit -a

yr=`echo $CDATE | cut -c1-4`
mn=`echo $CDATE | cut -c5-6`
dy=`echo $CDATE | cut -c7-8`

NDATE=${NDATE:-ndate}
NCP='/bin/cp'
NLN='ln -sf'

CDATEprior=`${NDATE} -6 $CDATE`
yrprior=`echo ${CDATEprior} | cut -c1-4`
mnprior=`echo ${CDATEprior} | cut -c5-6`
dyprior=`echo ${CDATEprior} | cut -c7-8`
hhprior=`echo ${CDATEprior} | cut -c9-10`
PDYprior=`echo ${CDATEprior} | cut -c1-8`

CDATEnhrs=`${NDATE} +${NHRS} $CDATE`
yrnhrs=`echo ${CDATEnhrs} | cut -c1-4`
mnnhrs=`echo ${CDATEnhrs} | cut -c5-6`
dynhrs=`echo ${CDATEnhrs} | cut -c7-8`
hhnhrs=`echo ${CDATEnhrs} | cut -c9-10`
cycnhrs=`echo ${CDATEnhrs} | cut -c9-10`
PDYnhrs=`echo ${CDATEnhrs} | cut -c1-8`

export COMhafsprior=${COMhafsprior:-${COMhafs}/../../${CDATEprior}/${STORMID}}
export WORKhafsprior=${WORKhafsprior:-${WORKhafs}/../../${CDATEprior}/${STORMID}}

export PARMhycom=${PARMhycom:-${PARMhafs}/hycom/regional}
export FIXhycom=${FIXhycom:-${FIXhafs}/fix_hycom}

export gtype=${gtype:-regional}
export halo_blend=${halo_blend:-0}
export nstf_n1=${nstf_n1:-2}
export nstf_n2=${nstf_n2:-0}
export nstf_n3=${nstf_n3:-0}
export nstf_n4=${nstf_n4:-0}
export nstf_n5=${nstf_n5:-0}

export dt_atmos=${dt_atmos:-90}
export restart_interval=${restart_interval:-6}
export quilting=${quilting:-.true.}
export write_groups=${write_groups:-3}
export write_tasks_per_group=${write_tasks_per_group:-72}

# For cold start from chgres_ic
export na_init=${na_init:-1}
export external_ic=${external_ic:-.true.}
export nggps_ic=${nggps_ic:-.true.}
export mountain=${mountain:-.false.}
export warm_start=${warm_start:-.false.}

#export coldstart_from_chgres=${coldstart_from_chgres:-yes}
#export warmstart_from_init=${warmstart_from_init:-no}
#export warmstart_from_priorcyc=${warmstart_from_priorcyc:-no}
#export warmstart_from_analysis_vr=${warmstart_from_analysis_vr:-no}
#export warmstart_from_analysis=${warmstart_from_analysis:-no}

# In the future, can consider using the following multiple options:
# warm_start_opt: 0, coldstart from chgres; 1, warmstart from init; 2,
# warmstart from prior cycle's restart files; 3, warmstart from vortex
# initialization; 4, warmstart from DA based vortex relocation; 5, warmstart
# from DA analysis

export warm_start_opt=${warm_start_opt:-0}

export warmstart_from_restart=${warmstart_from_restart:-no}
export RESTARTinp=${RESTARTinp:-"UNNEEDED"}

if [ ${warm_start_opt} -eq 0 ]; then
  export warmstart_from_restart=no
  export RESTARTinp="UNNEEDED"
fi

if [ ${ENSDA} != "YES" ]; then # for deterministic forecast

if [ ${warm_start_opt} -eq 1 ] && [ -s ${COMhafs}/RESTART_init/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  export warmstart_from_restart=yes
  export RESTARTinp=${COMhafs}/RESTART_init
fi

if [ ${warm_start_opt} -eq 2 ] && [ -s ${COMhafsprior}/RESTART/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  export warmstart_from_restart=yes
  export RESTARTinp=${COMhafsprior}/RESTART
fi

if [ ${warm_start_opt} -eq 3 ] && [ -s ${COMhafs}/RESTART_vi/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  export warmstart_from_restart=yes
  export RESTARTinp=${COMhafs}/RESTART_vi
fi

#if [ ${warm_start_opt} -eq 4 ] && [ ${RUN_GSI_VR} = yes ] && [ -s ${COMhafs}/RESTART_analysis_vr/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
if [ ${RUN_GSI_VR} = YES ] && [ -s ${COMhafs}/RESTART_analysis_vr/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  export warmstart_from_restart=yes
  export RESTARTinp=${COMhafs}/RESTART_analysis_vr
  export warm_start_opt=4
fi

#if [ ${warm_start_opt} -eq 5 ] && [ ${RUN_GSI} = yes ] && [ -s ${COMhafs}/RESTART_analysis/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
if [ ${RUN_GSI} = YES ] && [ -s ${COMhafs}/RESTART_analysis/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  export warmstart_from_restart=yes
  export RESTARTinp=${COMhafs}/RESTART_analysis
  export warm_start_opt=5
fi

else # for ENSDA member forecast

if [ ${warm_start_opt} -eq 1 ] && [ -s ${COMhafs}/RESTART_init_ens/mem${ENSID}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  export warmstart_from_restart=yes
  export RESTARTinp=${COMhafs}/RESTART_init_ens/mem${ENSID}
fi

if [ ${warm_start_opt} -eq 2 ] && [ -s ${COMhafsprior}/RESTART_ens/mem${ENSID}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  export warmstart_from_restart=yes
  export RESTARTinp=${COMhafsprior}/RESTART_ens/mem${ENSID}
fi

if [ ${warm_start_opt} -eq 3 ] && [ -s ${COMhafs}/RESTART_vi_ens/mem${ENSID}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  export warmstart_from_restart=yes
  export RESTARTinp=${COMhafs}/RESTART_vi_ens/mem${ENSID}
fi

if [ ${RUN_GSI_VR_ENS} = YES ] && [ -s ${COMhafs}/RESTART_analysis_vr_ens/mem${ENSID}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  export warmstart_from_restart=yes
  export RESTARTinp=${COMhafs}/RESTART_analysis_vr_ens/mem${ENSID}
  export warm_start_opt=4
fi

if [ ${RUN_ENKF} = YES ] && [ -s ${COMhafs}/RESTART_analysis_ens/mem${ENSID}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ]; then
  export warmstart_from_restart=yes
  export RESTARTinp=${COMhafs}/RESTART_analysis_ens/mem${ENSID}
  export warm_start_opt=5
fi

fi # ${ENSDA} != "YES" 

# For warm start from restart files
if [ ${warmstart_from_restart} = yes ]; then
export na_init=0
export external_ic=.false.
export nggps_ic=.false.
export mountain=.true.
export warm_start=.true.
fi


export stretch_fac=${stretch_fac:-1.0001}
export target_lon=${target_lon:--62.0}
export target_lat=${target_lat:-22.0}
export refine_ratio=${refine_ratio:-4}

export glob_k_split=${glob_k_split:-1}
export glob_n_split=${glob_n_split:-7}
export glob_layoutx=${glob_layoutx:-12}
export glob_layouty=${glob_layouty:-12}
export glob_npx=${glob_npx:-769}
export glob_npy=${glob_npy:-769}

export k_split=${k_split:-4}
export n_split=${n_split:-5}
export layoutx=${layoutx:-40}
export layouty=${layouty:-30}
export npx=${npx:-2881}
export npy=${npy:-1921}
export npz=${npz:-64}

export app_domain=${app_domain:-regional}
export output_grid=${output_grid:-rotated_latlon}
export output_grid_cen_lon=${output_grid_cen_lon:-${domlon}}
export output_grid_cen_lat=${output_grid_cen_lat:-${domlat}}
export output_grid_lon1=${output_grid_lon1:--35.0}
export output_grid_lat1=${output_grid_lat1:--30.0}
export output_grid_lon2=${output_grid_lon2:-35.0}
export output_grid_lat2=${output_grid_lat2:-30.0}
export output_grid_dlon=${output_grid_dlon:-0.025}
export output_grid_dlat=${output_grid_dlon:-0.025}

export out_prefix=${out_prefix:-$(echo "${STORM}${STORMID}.${YMDH}" | tr '[A-Z]' '[a-z]')}

export ccpp_suite_regional=${ccpp_suite_regional:-HAFS_v0_gfdlmp_nocp}
export ccpp_suite_glob=${ccpp_suite_glob:-HAFS_v0_gfdlmp}
export ccpp_suite_nest=${ccpp_suite_nest:-HAFS_v0_gfdlmp_nocp}

export run_ocean=${run_ocean:-no}
export ocean_model=${ocean_model:-hycom}
export cpl_ocean=${cpl_ocean:-0}
export ocean_tasks=${ocean_tasks:-120}
export cplflx=${cplflx:-.false.}
export cpl_dt=${cpl_dt:-360}

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
  echo "Error: please specify grid type with 'gtype' as uniform, stretch, nest, or regional"
  exit 9
fi

export ATM_petlist_bounds=$(printf "ATM_petlist_bounds: %04d %04d" 0 $(($ATM_tasks-1)))

# run ocean model side by side (no coupling)
if [ ${run_ocean} = yes ] && [ $cpl_ocean -eq 0 ];  then
  export cplflx=.false.
  export OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocean_tasks-1)))
  export MED_petlist_bounds=""
  export runSeq_ALL="ATM\n OCN"
fi
# direct coupling through the nearest point regridding method
if [ ${run_ocean} = yes ] && [ $cpl_ocean -eq 1 ];  then
  export cplflx=.true.
  export runSeq_ALL="OCN -> ATM :remapMethod=nearest_stod:srcmaskvalues=0\n ATM -> OCN :remapMethod=nearest_stod:srcmaskvalues=1:dstmaskvalues=0\n ATM\n OCN"
  export OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocean_tasks-1)))
  export MED_petlist_bounds=""
fi
# direct coupling through the bilinear regridding method
if [ ${run_ocean} = yes ] && [ $cpl_ocean -eq 2 ];  then
  export cplflx=.true.
  export OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocean_tasks-1)))
  export MED_petlist_bounds=""
  export runSeq_ALL="OCN -> ATM :remapMethod=bilinear:unmappedaction=ignore:zeroregion=select:srcmaskvalues=0\n ATM -> OCN :remapMethod=bilinear:unmappedaction=ignore:zeroregion=select:srcmaskvalues=1:dstmaskvalues=0\n ATM\n OCN"
fi
# CMEPS based coupling through the bilinear regridding method
if [ ${run_ocean} = yes ] && [ $cpl_ocean -eq 3 ];  then
  export cplflx=.true.
  export OCN_petlist_bounds=$(printf "OCN_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocean_tasks-1)))
  export MED_petlist_bounds=$(printf "MED_petlist_bounds: %04d %04d" $ATM_tasks $(($ATM_tasks+$ocean_tasks-1)))
  export runSeq_ALL="ATM -> MED :remapMethod=redist\n MED med_phases_post_atm\n OCN -> MED :remapMethod=redist\n MED med_phases_post_ocn\n MED med_phases_prep_atm\n MED med_phases_prep_ocn_accum\n MED med_phases_prep_ocn_avg\n MED -> ATM :remapMethod=redist\n MED -> OCN :remapMethod=redist\n ATM\n OCN"
fi

export ocean_start_dtg=${ocean_start_dtg:-43340.00000}
#export base_dtg=${CDATE:-2019082900}
#export end_hour=${NHRS:-126}
export merge_import=${merge_import:-.false.}

if [ $gtype = uniform ];  then
  export ntiles=6
elif [ $gtype = stretch ]; then
  export ntiles=6
elif [ $gtype = nest ]; then
  export ntiles=7
elif [ $gtype = regional ]; then
  export ntiles=1
else
  echo "Error: please specify grid type with 'gtype' as uniform, stretch, nest, or regional"
  exit 9
fi

TOTAL_TASKS=${TOTAL_TASKS:-2016}
NCTSK=${NCTSK:-12}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-2}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}

yr=`echo $CDATE | cut -c1-4`
mn=`echo $CDATE | cut -c5-6`
dy=`echo $CDATE | cut -c7-8`

if [ ! -d $INPdir ]; then
   echo Cannot find $INPdir ... exit
   exit 1
fi

mkdir -p INPUT
ln -sf ${INPdir}/*.nc INPUT/
#cp ${INPdir}/*.nc INPUT/
#rsync ${INPdir}/*.nc INPUT/

if [ ${ENSDA} = YES ]; then
  export RESTARTout=${RESTARTout:-${COMhafs}/RESTART_ens/mem${ENSID}}
  mkdir -p ${RESTARTout}
  ln -sf ${RESTARTout} RESTART
else
  export RESTARTout=${RESTARTout:-${COMhafs}/RESTART}
  mkdir -p ${RESTARTout}
  ln -sf ${RESTARTout} RESTART
fi

# Pass along the grid_spec.nc, atmos_static.nc, oro_data.nc from the prior cycle if exist
if [ ${ENSDA} = YES ]; then
 if [ -s ${COMhafsprior}/RESTART_ens/mem${ENSID}/grid_spec.nc ]; then
  cp -p ${COMhafsprior}/RESTART_ens/mem${ENSID}/grid_spec.nc RESTART/
 fi
 if [ -s ${COMhafsprior}/RESTART_ens/mem${ENSID}/atmos_static.nc ]; then
  cp -p ${COMhafsprior}/RESTART_ens/mem${ENSID}/atmos_static.nc RESTART/
 fi
 if [ -s ${COMhafsprior}/RESTART_ens/mem${ENSID}/oro_data.nc ]; then
  cp -p ${COMhafsprior}/RESTART_ens/mem${ENSID}/oro_data.nc RESTART/
 fi
else
 if [ -s ${COMhafsprior}/RESTART/grid_spec.nc ]; then
  cp -p ${COMhafsprior}/RESTART/grid_spec.nc RESTART/
 fi
 if [ -s ${COMhafsprior}/RESTART/atmos_static.nc ]; then
  cp -p ${COMhafsprior}/RESTART/atmos_static.nc RESTART/
 fi
 if [ -s ${COMhafsprior}/RESTART/oro_data.nc ]; then
  cp -p ${COMhafsprior}/RESTART/oro_data.nc RESTART/
 fi
fi

#---------------------------------------------- 
# Copy all the necessary fix files
#---------------------------------------------- 
cp $FIXam/global_solarconstant_noaa_an.txt  solarconstant_noaa_an.txt
cp $FIXam/ozprdlos_2015_new_sbuvO3_tclm15_nuchem.f77 global_o3prdlos.f77
cp $FIXam/global_h2o_pltc.f77               global_h2oprdlos.f77
cp $FIXam/global_sfc_emissivity_idx.txt     sfc_emissivity_idx.txt
cp $FIXam/global_co2historicaldata_glob.txt co2historicaldata_glob.txt
cp $FIXam/co2monthlycyc.txt                 co2monthlycyc.txt
cp $FIXam/global_climaeropac_global.txt     aerosol.dat

cp $FIXam/global_glacier.2x2.grb .
cp $FIXam/global_maxice.2x2.grb .
cp $FIXam/RTGSST.1982.2012.monthly.clim.grb .
cp $FIXam/global_snoclim.1.875.grb .
cp $FIXam/global_snowfree_albedo.bosu.t1534.3072.1536.rg.grb .
cp $FIXam/global_albedo4.1x1.grb .
cp $FIXam/CFSR.SEAICE.1982.2012.monthly.clim.grb .
cp $FIXam/global_tg3clim.2.6x1.5.grb .
cp $FIXam/global_vegfrac.0.144.decpercent.grb .
cp $FIXam/global_vegtype.igbp.t1534.3072.1536.rg.grb .
cp $FIXam/global_soiltype.statsgo.t1534.3072.1536.rg.grb .
cp $FIXam/global_soilmgldas.t1534.3072.1536.grb .
cp $FIXam/seaice_newland.grb .
cp $FIXam/global_shdmin.0.144x0.144.grb .
cp $FIXam/global_shdmax.0.144x0.144.grb .
cp $FIXam/global_slope.1x1.grb .
cp $FIXam/global_mxsnoalb.uariz.t1534.3072.1536.rg.grb .

for file in `ls $CO2DIR/global_co2historicaldata* ` ; do
 cp $file $(echo $(basename $file) |sed -e "s/global_//g")
done

# Copy the fix files needed by the hwrf ccpp physics suite
cp ${PARMhafs}/forecast/hwrf_physics_fix/* .

if [ $gtype = nest ]; then

#---------------------------------------------- 
# Copy tile data and orography
#---------------------------------------------- 
tile=1
while [ $tile -le $ntiles ]; do
  cp $FIXgrid/${CASE}/${CASE}_oro_data.tile${tile}.nc INPUT/oro_data.tile${tile}.nc
  cp $FIXgrid/${CASE}/${CASE}_grid.tile${tile}.nc INPUT/${CASE}_grid.tile${tile}.nc
  let tile=tile+1
done
cp $FIXgrid/${CASE}/${CASE}_mosaic.nc INPUT/grid_spec.nc

# The next 4 links are a hack GFDL requires for running a nest
 
cd ./INPUT
#ln -sf ${CASE}_grid.tile7.nc ${CASE}_grid.nest02.tile7.nc
ln -sf ${CASE}_grid.tile7.nc grid.nest02.tile7.nc
ln -sf oro_data.tile7.nc oro_data.nest02.tile7.nc
ln -sf gfs_data.tile7.nc gfs_data.nest02.tile7.nc
ln -sf sfc_data.tile7.nc sfc_data.nest02.tile7.nc
cd ..

#-------------------------------------------------------------------
# Copy or set up files data_table, diag_table, field_table,
#   input.nml, input_nest02.nml, model_configure, and nems.configure
#-------------------------------------------------------------------
cp ${PARMforecast}/data_table .
cp ${PARMforecast}/diag_table.tmp .
cp ${PARMforecast}/field_table .
cp ${PARMforecast}/input.nml.tmp .
cp ${PARMforecast}/input_nest02.nml.tmp .
cp ${PARMforecast}/model_configure.tmp .
cp ${PARMforecast}/nems.configure.atmonly ./nems.configure

ccpp_suite_glob_xml="${HOMEhafs}/sorc/hafs_forecast.fd/FV3/ccpp/suites/suite_${ccpp_suite_glob}.xml"
cp ${ccpp_suite_glob_xml} .

glob_pes=$(( ${glob_layoutx} * ${glob_layouty} * 6 ))
nest_pes=$(( ${layoutx} * ${layouty} ))
ioffset=$(( (istart_nest-1)/2 + 1))
joffset=$(( (jstart_nest-1)/2 + 1))

sed -e "s/_fhmax_/${NHRS}/g" \
    -e "s/_ccpp_suite_/${ccpp_suite_glob}/g" \
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
    -e "s/_nstf_n1_/${nstf_n1:-2}/g" \
    -e "s/_nstf_n2_/${nstf_n2:-0}/g" \
    -e "s/_nstf_n3_/${nstf_n3:-0}/g" \
    -e "s/_nstf_n4_/${nstf_n4:-0}/g" \
    -e "s/_nstf_n5_/${nstf_n5:-0}/g" \
    -e "s/_cplflx_/${cplflx:-.false.}/g" \
    -e "s/_merge_import_/${merge_import:-.false.}/g" \
    input.nml.tmp > input.nml

ccpp_suite_nest_xml="${HOMEhafs}/sorc/hafs_forecast.fd/FV3/ccpp/suites/suite_${ccpp_suite_nest}.xml"
cp ${ccpp_suite_nest_xml} .

sed -e "s/_fhmax_/${NHRS}/g" \
    -e "s/_ccpp_suite_/${ccpp_suite_nest}/g" \
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
    -e "s/_nstf_n1_/${nstf_n1:-2}/g" \
    -e "s/_nstf_n2_/${nstf_n2:-0}/g" \
    -e "s/_nstf_n3_/${nstf_n3:-0}/g" \
    -e "s/_nstf_n4_/${nstf_n4:-0}/g" \
    -e "s/_nstf_n5_/${nstf_n5:-0}/g" \
    -e "s/_cplflx_/${cplflx:-.false.}/g" \
    -e "s/_merge_import_/${merge_import:-.false.}/g" \
    input_nest02.nml.tmp > input_nest02.nml

elif [ $gtype = regional ]; then

#---------------------------------------------- 
# Copy tile data and orography for regional
#---------------------------------------------- 
tile=7
# Copy grid and orog files (halo[034])
cp $FIXgrid/${CASE}/${CASE}_grid.tile${tile}.halo?.nc INPUT/.
cp $FIXgrid/${CASE}/${CASE}_oro_data.tile${tile}.halo?.nc INPUT/.
cp $FIXgrid/${CASE}/${CASE}_mosaic.nc INPUT/.

cd INPUT
ln -sf ${CASE}_mosaic.nc grid_spec.nc
ln -sf ${CASE}_grid.tile7.halo0.nc grid.tile7.halo0.nc
ln -sf ${CASE}_grid.tile7.halo3.nc ${CASE}_grid.tile7.nc
ln -sf ${CASE}_grid.tile7.halo4.nc grid.tile7.halo4.nc
ln -sf ${CASE}_oro_data.tile7.halo0.nc oro_data.nc
ln -sf ${CASE}_oro_data.tile7.halo4.nc oro_data.tile7.halo4.nc
ln -sf sfc_data.tile7.nc sfc_data.nc
ln -sf gfs_data.tile7.nc gfs_data.nc

# For warm start from restart files (either before or after analysis)
if [ ${warmstart_from_restart} = yes ]; then
${NLN} ${RESTARTinp}/${PDY}.${cyc}0000.coupler.res ./coupler.res
${NLN} ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.nc ./fv_core.res.nc
${NLN} ${RESTARTinp}/${PDY}.${cyc}0000.fv_srf_wnd.res.tile1.nc ./fv_srf_wnd.res.tile1.nc
${NLN} ${RESTARTinp}/${PDY}.${cyc}0000.fv_core.res.tile1.nc ./fv_core.res.tile1.nc
${NLN} ${RESTARTinp}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc ./fv_tracer.res.tile1.nc
fi

cd ..

#-------------------------------------------------------------------
# Copy or set up files data_table, diag_table, field_table,
#   input.nml, input_nest02.nml, model_configure, and nems.configure
#-------------------------------------------------------------------
cp ${PARMforecast}/data_table .
cp ${PARMforecast}/diag_table.tmp .
cp ${PARMforecast}/field_table .
cp ${PARMforecast}/input.nml.tmp .
cp ${PARMforecast}/model_configure.tmp .

if [ ${run_ocean} = yes ]; then
  if [[ ${cpl_ocean} -eq 3 ]]; then
    cp ${PARMforecast}/nems.configure.atm_ocn_cmeps.tmp ./nems.configure.tmp
  else
    cp ${PARMforecast}/nems.configure.atm_ocn.tmp ./nems.configure.tmp
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
  cp ${PARMforecast}/nems.configure.atmonly ./nems.configure
else
  echo "Error: unknown run_ocean option: ${run_ocean}"
  exit 9
fi

ccpp_suite_regional_xml="${HOMEhafs}/sorc/hafs_forecast.fd/FV3/ccpp/suites/suite_${ccpp_suite_regional}.xml"
cp ${ccpp_suite_regional_xml} .

sed -e "s/_fhmax_/${NHRS}/g" \
    -e "s/_ccpp_suite_/${ccpp_suite_regional}/g" \
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

cp ${WORKhafs}/intercom/hycominit/hycom_settings hycom_settings 
export hycom_basin=$(grep RUNmodIDout ./hycom_settings | cut -c20-)

# copy IC/BC
cp ${WORKhafs}/intercom/hycominit/restart_out.a restart_in.a 
cp ${WORKhafs}/intercom/hycominit/restart_out.b restart_in.b 

# copy forcing
cp ${WORKhafs}/intercom/hycominit/forcing* .
ln -sf forcing.presur.a forcing.mslprs.a
ln -sf forcing.presur.b forcing.mslprs.b

# copy fix
cp ${FIXhycom}/hafs_${hycom_basin}.basin.regional.depth.a regional.depth.a
cp ${FIXhycom}/hafs_${hycom_basin}.basin.regional.depth.b regional.depth.b
cp ${FIXhycom}/hafs_${hycom_basin}.basin.regional.grid.a regional.grid.a
cp ${FIXhycom}/hafs_${hycom_basin}.basin.regional.grid.b regional.grid.b
cp ${FIXhycom}/hafs_${hycom_basin}.basin.forcing.chl.a forcing.chl.a
cp ${FIXhycom}/hafs_${hycom_basin}.basin.forcing.chl.b forcing.chl.b
cp ${FIXhycom}/hafs_${hycom_basin}.basin.iso.sigma.a iso.sigma.a
cp ${FIXhycom}/hafs_${hycom_basin}.basin.iso.sigma.b iso.sigma.b
cp ${FIXhycom}/hafs_${hycom_basin}.basin.relax.ssh.a relax.ssh.a
cp ${FIXhycom}/hafs_${hycom_basin}.basin.relax.ssh.b relax.ssh.b
cp ${FIXhycom}/hafs_${hycom_basin}.basin.tbaric.a tbaric.a
cp ${FIXhycom}/hafs_${hycom_basin}.basin.tbaric.b tbaric.b
cp ${FIXhycom}/hafs_${hycom_basin}.basin.thkdf4.a thkdf4.a
cp ${FIXhycom}/hafs_${hycom_basin}.basin.thkdf4.b thkdf4.b
cp ${FIXhycom}/hafs_${hycom_basin}.basin.veldf2.a veldf2.a
cp ${FIXhycom}/hafs_${hycom_basin}.basin.veldf2.b veldf2.b
cp ${FIXhycom}/hafs_${hycom_basin}.basin.veldf4.a veldf4.a
cp ${FIXhycom}/hafs_${hycom_basin}.basin.veldf4.b veldf4.b
cp ${FIXhycom}/hafs_${hycom_basin}.basin.relax.rmu.a relax.rmu.a
cp ${FIXhycom}/hafs_${hycom_basin}.basin.relax.rmu.b relax.rmu.b

# copy parms
cp ${PARMhycom}/hafs_${hycom_basin}.basin.fcst.blkdat.input blkdat.input 
cp ${PARMhycom}/hafs_${hycom_basin}.basin.ports.input ports.input
cp ${PARMhycom}/hafs_${hycom_basin}.basin.patch.input.${ocean_tasks} patch.input

# create hycom limits
${USHhafs}/hafs_hycom_limits.py ${yr}${mn}${dy}${cyc}

fi

fi
  
#-------------------------------------------------------------------
# Generate diag_table, model_configure from their tempelates
#-------------------------------------------------------------------
echo ${yr}${mn}${dy}.${cyc}Z.${CASE}.32bit.non-hydro
echo $yr $mn $dy $cyc 0 0
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

#-------------------------------------------------------------------
# Copy the fd_nems.yaml file
#-------------------------------------------------------------------
cp ${HOMEhafs}/sorc/hafs_forecast.fd/CMEPS-interface/CMEPS/mediator/fd_nems.yaml ./

#-------------------------------------------------------------------
# Link the executable and run the forecast
#-------------------------------------------------------------------
#cp ${EXEChafs}/hafs_forecast.x hafs_forecast.x
FORECASTEXEC=${FORECASTEXEC:-${EXEChafs}/hafs_forecast.x}
cp -p ${FORECASTEXEC} ./hafs_forecast.x

${APRUNC} ./hafs_forecast.x 1>out.$CRES 2>err.$CRES
export err=$?

#-------------------------------------------------------------------
# Deliver files to COM
#-------------------------------------------------------------------

# Rename the restart files with a proper convention
cd RESTART
if [ -s fv_core.res.nc ]; then
  #for file in $(/bin/ls -1 fv_core.res.nc sfc_data.nc fv_tracer.res.tile*.nc fv_srf_wnd.res.tile*.nc fv_core.res.tile*.nc phy_data.nc coupler.res)
  for file in $(/bin/ls -1 fv*.nc phy_data.nc sfc_data.nc coupler.res)
  do
    mv ${file} ${PDYnhrs}.${cycnhrs}0000.${file}
  done
fi
cd ${DATA}

if [ $gtype = regional ]; then

# Deliver the grid_spec.nc, atmos_static.nc, oro_data.nc if not exist
if [ ! -s RESTART/grid_spec.nc ]; then
  cp -p grid_spec.nc RESTART/
fi
if [ ! -s RESTART/atmos_static.nc ]; then
  cp -p atmos_static.nc RESTART/
fi
if [ ! -s RESTART/oro_data.nc ]; then
  cp -pL INPUT/oro_data.nc RESTART/
fi

fi

exit $err

