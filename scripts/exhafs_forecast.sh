#!/bin/ksh

set -xe

ulimit -s unlimited
ulimit -a

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

export stretch_fac=${stretch_fac:-1.0001}
export target_lon=${target_lon:--62.0}
export target_lat=${target_lat:-22.0}
export refine_ratio=${refine_ratio:-4}

export glob_layoutx=${glob_layoutx:-12}
export glob_layouty=${glob_layouty:-12}
export glob_npx=${glob_npx:-769}
export glob_npy=${glob_npy:-769}

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

export run_datm=${run_datm:-no}
export domain_atm=${domain_atm:-''}
export mesh_atm=${mesh_atm:-''}

export run_ocean=${run_ocean:-no}
export ocean_model=${ocean_model:-hycom}
export cpl_ocean=${cpl_ocean:-0}
export ocean_tasks=${ocean_tasks:-120}
export cpl_dt=${cpl_dt:-360}
export cplflx=${cplflx:-.false.}
export ATM_petlist_bounds=${ATM_petlist_bounds:-'ATM_petlist_bounds: 0000 1319'}
export OCN_petlist_bounds=${OCN_petlist_bounds:-'OCN_petlist_bounds: 1320 1439'}
export runSeq_OCN2ATM=${runSeq_OCN2ATM:-''}
export runSeq_ATM2OCN=${runSeq_ATM2OCN:-''}
export runSeq_ATM=${runSeq_ATM:-'ATM'}
export runSeq_OCN=${runSeq_OCN:-'OCN'}
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

mkdir -p INPUT RESTART
ln -sf ${INPdir}/*.nc INPUT/
#cp ${INPdir}/*.nc INPUT/
#rsync ${INPdir}/*.nc INPUT/

#---------------------------------------------- 
# Copy all the necessary fix files
#---------------------------------------------- 
if [ ${run_datm} = no ];  then

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

if [ ${run_ocean} = yes ];  then
  cp ${PARMforecast}/nems.configure.atm_ocn.tmp ./
  sed -e "s/_ATM_petlist_bounds_/${ATM_petlist_bounds}/g" \
      -e "s/_OCN_petlist_bounds_/${OCN_petlist_bounds}/g" \
      -e "s/_cpl_dt_/${cpl_dt}/g" \
      -e "s/_runSeq_OCN2ATM_/${runSeq_OCN2ATM}/g" \
      -e "s/_runSeq_ATM2OCN_/${runSeq_ATM2OCN}/g" \
      -e "s/_runSeq_ATM_/${runSeq_ATM}/g" \
      -e "s/_runSeq_OCN_/${runSeq_OCN}/g" \
      -e "s/_base_dtg_/${CDATE}/g" \
      -e "s/_ocean_start_dtg_/${ocean_start_dtg}/g" \
      -e "s/_end_hour_/${NHRS}/g" \
      -e "s/_merge_import_/${merge_import:-.false.}/g" \
      nems.configure.atm_ocn.tmp > nems.configure
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

fi # if regional

fi # if not cdeps datm

if [ $gtype = regional ]; then

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

# copy fd_nems.yaml
if [ ${run_datm} = no ];  then
  cp ${HOMEhafs}/sorc/hafs_forecast.fd/CMEPS/mediator/fd_nems.yaml ./ 
else
  cp ${HOMEhafs}/sorc/hafs_forecast.fd/CMEPS/mediator/fd_cdeps.yaml fd.yaml
fi

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

if [ ${run_datm} = no ];  then

cat temp diag_table.tmp > diag_table

else

#---------------------------------------------- 
# Copy CDEPS parm files if required.
#---------------------------------------------- 
  cp ${PARMforecast}/model_configure.tmp .
  cp ${PARMhafs}/cdeps/datm_in .
  cp ${PARMhafs}/cdeps/datm.streams.xml .
  cp ${PARMhafs}/cdeps/pio_in .
  cp ${PARMhafs}/cdeps/modelio.nml atm_modelio.nml
  cp ${PARMhafs}/cdeps/modelio.nml med_modelio.nml
  cp ${PARMhafs}/cdeps/modelio.nml ocn_modelio.nml

  sed -i "s/_mesh_atm_/INPUT\/$(basename $mesh_atm)/g" datm_in

  sed -i "s/_mesh_atm_/INPUT\/$(basename $mesh_atm)/g" datm.streams.xml
  for file in `ls INPUT/*.nc ` ; do
    sed -i "/<\/stream_data_files>/i \ \ \ \ \ \ <file>$file<\/file>" datm.streams.xml
  done

  ln -sf ${domain_atm} INPUT/
  ln -sf ${mesh_atm} INPUT/

  cp ${PARMforecast}/nems.configure.datm_ocn.tmp ./
  sed -e "s/_ATM_petlist_bounds_/${ATM_petlist_bounds}/g" \
      -e "s/_MED_petlist_bounds_/${MED_petlist_bounds}/g" \
      -e "s/_OCN_petlist_bounds_/${OCN_petlist_bounds}/g" \
      -e "s/_cpl_dt_/${cpl_dt}/g" \
      -e "s/_base_dtg_/${CDATE}/g" \
      -e "s/_ocean_start_dtg_/${ocean_start_dtg}/g" \
      -e "s/_end_hour_/${NHRS}/g" \
      -e "s/_merge_import_/${merge_import:-.true.}/g" \
      -e "s/_domain_atm_/INPUT\/$(basename $domain_atm)/g" \
      -e "s/_mesh_atm_/INPUT\/$(basename $mesh_atm)/g" \
      nems.configure.datm_ocn.tmp > nems.configure

fi

cat model_configure.tmp | sed s/NTASKS/$TOTAL_TASKS/ | sed s/YR/$yr/ | \
    sed s/MN/$mn/ | sed s/DY/$dy/ | sed s/H_R/$cyc/ | \
    sed s/NHRS/$NHRS/ | sed s/NTHRD/$OMP_NUM_THREADS/ | \
    sed s/NCNODE/$NCNODE/ | \
    sed s/_dt_atmos_/${dt_atmos}/ | \
    sed s/_restart_interval_/${restart_interval}/ | \
    sed s/_quilting_/${quilting}/ | \
    sed s/_write_groups_/${write_groups}/ | \
    sed s/_write_tasks_per_group_/${write_tasks_per_group}/ | \
    sed s/_app_domain_/${app_domain}/ | \
    sed s/_OUTPUT_GRID_/$output_grid/ | \
    sed s/_CEN_LON_/$output_grid_cen_lon/ | \
    sed s/_CEN_LAT_/$output_grid_cen_lat/ | \
    sed s/_LON1_/$output_grid_lon1/ | \
    sed s/_LAT1_/$output_grid_lat1/ | \
    sed s/_LON2_/$output_grid_lon2/ | \
    sed s/_LAT2_/$output_grid_lat2/ | \
    sed s/_DLON_/$output_grid_dlon/ | \
    sed s/_DLAT_/$output_grid_dlat/ | \
    sed s/_cpl_/${cplflx:-.false.}/ \
    >  model_configure

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

exit $err

