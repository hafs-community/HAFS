#!/bin/ksh

set -xe

ulimit -s unlimited
ulimit -a

export gtype=${gtype:-regional}

export dt_atmos=${dt_atmos:-90}
export restart_interval=${restart_interval:-6}
export quilting=${quilting:-.true.}
export write_groups=${write_groups:-3}
export write_tasks_per_group=${write_tasks_per_group:-72}

export stretch_fac=${stretch_fac:-1.0001}
export target_lon=${target_lon:--62.0}
export target_lat=${target_lat:-22.0}

export nest_grids=${nest_grids:-1}
export parent_grid_num=${parent_grid_num:-1}
export parent_tile=${parent_tile:-6}
export refine_ratio=${refine_ratio:-4}
export istart_nest=${istart_nest:-46}
export jstart_nest=${jstart_nest:-238}
export iend_nest=${iend_nest:-1485}
export jend_nest=${jend_nest:-1287}

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

export ccpp_suite_regional=${ccpp_suite_regional:-HAFS_v0_gfdlmp_nocp}
export ccpp_suite_glob=${ccpp_suite_glob:-HAFS_v0_gfdlmp}
export ccpp_suite_nest=${ccpp_suite_nest:-HAFS_v0_gfdlmp_nocp}

if [ $gtype = uniform ];  then
  export ntiles=6
elif [ $gtype = stretch ]; then
  export ntiles=6
elif [ $gtype = nest ]; then
  #export ntiles=7
  export ntiles=$((6 + ${nest_grids}))
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

if [ $gtype = nest ]; then

#---------------------------------------------- 
# Copy tile data and orography
#---------------------------------------------- 
for itile in $(seq 1 ${ntiles})
do
  cp $FIXgrid/${CASE}/${CASE}_oro_data.tile${itile}.nc INPUT/oro_data.tile${itile}.nc
  cp $FIXgrid/${CASE}/${CASE}_grid.tile${itile}.nc INPUT/${CASE}_grid.tile${itile}.nc
done
cp $FIXgrid/${CASE}/${CASE}_mosaic.nc INPUT/grid_spec.nc

# The next 4 links are a hack GFDL requires for running a nest

cd ./INPUT

for itile in $(seq 7 ${ntiles})
do
  inest=$(($itile - 5))
  ln -sf ${CASE}_grid.tile${itile}.nc grid.nest0${inest}.tile${itile}.nc
  ln -sf oro_data.tile${itile}.nc oro_data.nest0${inest}.tile${itile}.nc
  ln -sf gfs_data.tile${itile}.nc gfs_data.nest0${inest}.tile${itile}.nc
  ln -sf sfc_data.tile${itile}.nc sfc_data.nest0${inest}.tile${itile}.nc
done

cd ..

#-------------------------------------------------------------------
# Copy or set up files data_table, diag_table, field_table,
#   input.nml, input_nest02.nml, model_configure, and nems.configure
#-------------------------------------------------------------------
cp ${PARMforecast}/data_table .
cp ${PARMforecast}/diag_table.tmp .
cp ${PARMforecast}/field_table .
cp ${PARMforecast}/input.nml.tmp .
cp ${PARMforecast}/input_nest.nml.tmp .
cp ${PARMforecast}/model_configure.tmp .
cp ${PARMforecast}/nems.configure .

ngrids=$(( ${nest_grids} + 1 ))
glob_pes=$(( ${glob_layoutx} * ${glob_layouty} * 6 ))
nest_pes="${glob_pes}"
for n in $(seq 1 ${nest_grids})
do
  layoutx_tmp=$( echo ${layoutx} | cut -d , -f ${n} )
  layouty_tmp=$( echo ${layouty} | cut -d , -f ${n} )
  nest_pes="${nest_pes},$(( $layoutx_tmp * $layouty_tmp ))"
done

ccpp_suite_glob_xml="${HOMEhafs}/sorc/hafs_forecast.fd/FV3/ccpp/suites/suite_${ccpp_suite_glob}.xml"
cp ${ccpp_suite_glob_xml} .

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
    -e "s/_ngrids_/${ngrids}/g" \
    -e "s/_nest_pes_/${nest_pes}/g" \
    -e "s/_levp_/${LEVS}/g" \
	input.nml.tmp > input.nml

ccpp_suite_nest_xml="${HOMEhafs}/sorc/hafs_forecast.fd/FV3/ccpp/suites/suite_${ccpp_suite_nest}.xml"
cp ${ccpp_suite_nest_xml} .

for n in $(seq 1 ${nest_grids})
do
  inest=$(( ${n} + 1 ))
  parent_grid_num_tmp=$( echo ${parent_grid_num} | cut -d , -f ${n} )
  parent_tile_tmp=$( echo ${parent_tile} | cut -d , -f ${n} )
  refine_ratio_tmp=$( echo ${refine_ratio} | cut -d , -f ${n} )
  istart_nest_tmp=$( echo ${istart_nest} | cut -d , -f ${n} )
  jstart_nest_tmp=$( echo ${jstart_nest} | cut -d , -f ${n} )
  iend_nest_tmp=$( echo ${iend_nest} | cut -d , -f ${n} )
  jend_nest_tmp=$( echo ${jend_nest} | cut -d , -f ${n} )
  ioffset_tmp=$(( ($istart_nest_tmp-1)/2 + 1))
  joffset_tmp=$(( ($jstart_nest_tmp-1)/2 + 1))

  layoutx_tmp=$( echo ${layoutx} | cut -d , -f ${n} )
  layouty_tmp=$( echo ${layouty} | cut -d , -f ${n} )

  npx_tmp=$( echo ${npx} | cut -d , -f ${n} )
  npy_tmp=$( echo ${npy} | cut -d , -f ${n} )

  sed -e "s/_fhmax_/${NHRS}/g" \
    -e "s/_ccpp_suite_/${ccpp_suite_nest}/g" \
    -e "s/_layoutx_/${layoutx_tmp}/g" \
    -e "s/_layouty_/${layouty_tmp}/g" \
    -e "s/_npx_/${npx_tmp}/g" \
    -e "s/_npy_/${npy_tmp}/g" \
    -e "s/_npz_/${npz}/g" \
    -e "s/_target_lat_/${target_lat}/g" \
    -e "s/_target_lon_/${target_lon}/g" \
    -e "s/_stretch_fac_/${stretch_fac}/g" \
    -e "s/_parent_grid_num_/${parent_grid_num_tmp}/g" \
    -e "s/_parent_tile_/${parent_tile_tmp}/g" \
    -e "s/_refinement_/${refine_ratio_tmp}/g" \
    -e "s/_ioffset_/${ioffset_tmp}/g" \
    -e "s/_joffset_/${joffset_tmp}/g" \
    -e "s/_ngrids_/${ngrids}/g" \
    -e "s/_nest_pes_/${nest_pes}/g" \
    -e "s/_levp_/${LEVS}/g" \
	input_nest.nml.tmp > input_nest0${inest}.nml

done

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
cp ${PARMforecast}/nems.configure .

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
    -e "s/_levp_/${LEVS}/g" \
	input.nml.tmp > input.nml

fi
 
#-------------------------------------------------------------------
# Generate diag_table, model_configure from their tempelates
#-------------------------------------------------------------------
yr=`echo $CDATE | cut -c1-4`
mn=`echo $CDATE | cut -c5-6`
dy=`echo $CDATE | cut -c7-8`

echo ${yr}${mn}${dy}.${cyc}Z.${CASE}.32bit.non-hydro
echo $yr $mn $dy $cyc 0 0
cat > temp << EOF
${yr}${mn}${dy}.${cyc}Z.${CASE}.32bit.non-hydro
$yr $mn $dy $cyc 0 0
EOF

cat temp diag_table.tmp > diag_table

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
    sed s/_DLAT_/$output_grid_dlat/ \
    >  model_configure

#-------------------------------------------------------------------
# Link the executable and run the forecast
#-------------------------------------------------------------------
#cp ${EXEChafs}/hafs_forecast.x hafs_forecast.x
FORECASTEXEC=${FORECASTEXEC:-${EXEChafs}/hafs_forecast.x}
cp ${FORECASTEXEC} hafs_forecast.x

${APRUNC} ./hafs_forecast.x 1>out.$CRES 2>err.$CRES
export err=$?

#-------------------------------------------------------------------
# Deliver files to COM
#-------------------------------------------------------------------

exit $err

