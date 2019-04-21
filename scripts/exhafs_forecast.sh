#!/bin/ksh
############################################################################
# Script name:		run_regional_gfdlmp.sh
# Script description:	Run the 3-km FV3 regional forecast over the CONUS
#			using the GFDL microphysics scheme.
# Script history log:
#   1) 2018-03-14	Eric Rogers
#			run_nest.tmp retrieved from Eric's run_it directory.	
#   2) 2018-04-03	Ben Blake
#                       Modified from Eric's run_nest.tmp script.
#   3) 2018-04-13	Ben Blake
#			Various settings moved to JFV3_FORECAST J-job
#   4) 2018-06-19       Ben Blake
#                       Adapted for stand-alone regional configuration
############################################################################
set -xe

ulimit -s unlimited
ulimit -a

export gtype=${gtype:-regional}

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

if [ ! -d $INPdir ]; then
   echo Cannot find $INPdir ... exit
   exit
fi

mkdir -p INPUT RESTART
cp ${INPdir}/*.nc INPUT
#rsync ${INPdir}/*.nc INPUT

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
cp ${PARMhafs}/globnest/data_table .
cp ${PARMhafs}/globnest/diag_table.tmp .
cp ${PARMhafs}/globnest/field_table .
cp ${PARMhafs}/globnest/input*nml .
cp ${PARMhafs}/globnest/model_configure.tmp .
cp ${PARMhafs}/globnest/nems.configure .

elif [ $gtype = regional ]; then

#---------------------------------------------- 
# Copy tile data and orography for regional
#---------------------------------------------- 
tile=7
cp $FIXgrid/${CASE}/${CASE}_grid.tile${tile}.halo3.nc INPUT/.
cp $FIXgrid/${CASE}/${CASE}_grid.tile${tile}.halo4.nc INPUT/.
cp $FIXgrid/${CASE}/${CASE}_oro_data.tile${tile}.halo0.nc INPUT/.
cp $FIXgrid/${CASE}/${CASE}_oro_data.tile${tile}.halo4.nc INPUT/.
cp $FIXgrid/${CASE}/${CASE}_mosaic.nc INPUT/.

cd INPUT
ln -sf ${CASE}_mosaic.nc grid_spec.nc
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
cp ${PARMhafs}/regional/data_table .
cp ${PARMhafs}/regional/diag_table.tmp .
cp ${PARMhafs}/regional/field_table .
cp ${PARMhafs}/regional/input.nml .
cp ${PARMhafs}/regional/model_configure.tmp .
cp ${PARMhafs}/regional/nems.configure .

fi
  
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
    sed s/NCNODE/$NCNODE/  >  model_configure

#----------------------------------------- 
# Link the executable and run the forecast
#-----------------------------------------
#cp ${EXEChafs}/hafs_forecast.x hafs_forecast.x
FORECASTEXEC=${FORECASTEXEC:-${EXEChafs}/hafs_forecast.x}
cp ${FORECASTEXEC} hafs_forecast.x

${APRUNC} ./hafs_forecast.x 1>out.$CRES 2>err.$CRES
export err=$?

exit $err
