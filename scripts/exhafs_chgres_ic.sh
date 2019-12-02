#!/bin/ksh

set -xe

CASE=${CASE:-C768}
CRES=`echo $CASE | cut -c 2-`
gtype=${gtype:-regional}           # grid type = uniform, stretch, nest, or stand alone regional

CDUMP=gfs		# gfs or gdas
LEVS=${LEVS:-65}
ictype=${ictype:-pfv3gfs}
gtype=${gtype:-regional}           # grid type = uniform, stretch, nest, or stand alone regional
REGIONAL=${REGIONAL:-0}

CDATE=${CDATE:-${YMDH}}
cyc=${cyc:-00}
ymd=`echo $CDATE | cut -c 1-8`
month=`echo $CDATE | cut -c 5-6`
day=`echo $CDATE | cut -c 7-8`
hour=`echo $CDATE | cut -c 9-10`

HOMEhafs=${HOMEhafs:-/gpfs/hps3/emc/hwrf/noscrub/${USER}/save/HAFS}
WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
COMhafs=${COMhafs:-${COMOUT}}
USHhafs=${USHhafs:-${HOMEhafs}/ush}
PARMhafs=${PARMhafs:-${HOMEhafs}/parm}
EXEChafs=${EXEChafs:-${HOMEhafs}/exec}
FIXhafs=${FIXhafs:-${HOMEhafs}/fix}

CHGRESCUBEEXEC=${CHGRESCUBEEXEC:-${EXEChafs}/hafs_chgres_cube.x}

OUTDIR=${OUTDIR:-${WORKhafs}/intercom/chgres}
DATA=${DATA:-${WORKhafs}/chgres_ic}
mkdir -p ${OUTDIR} ${DATA}

GRID_intercom=${WORKhafs}/intercom/grid
FIXDIR=${DATA}/grid
FIXCASE=${DATA}/grid/${CASE}
mkdir -p $DATA ${FIXDIR} ${FIXCASE} 

cd $FIXDIR/${CASE}
ln -sf ${GRID_intercom}/${CASE}/* ./

cd $DATA

# input data is FV3GFS (ictype is 'pfv3gfs')
if [ $ictype = pfv3gfs ]; then		# input is fv3gfs parallel
  atm_files_input_grid=${CDUMP}.t${cyc}z.atmanl.nemsio
  sfc_files_input_grid=${CDUMP}.t${cyc}z.sfcanl.nemsio
else
  echo "ERROR: unsupportted input data type yet."
  exit 1
fi

if [ $gtype = uniform ] || [ $gtype = stretch ] || [ $gtype = nest ];  then

 ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}*.nc $FIXDIR/$CASE/.
 if [ $gtype = nest ];  then
   ln -sf $FIXDIR/$CASE/${CASE}_coarse_mosaic.nc $FIXDIR/$CASE/${CASE}_mosaic.nc
 fi
 mosaic_file_target_grid="$FIXDIR/$CASE/${CASE}_mosaic.nc"
 orog_files_target_grid='"'${CASE}'_oro_data.tile1.nc","'${CASE}'_oro_data.tile2.nc","'${CASE}'_oro_data.tile3.nc","'${CASE}'_oro_data.tile4.nc","'${CASE}'_oro_data.tile5.nc","'${CASE}'_oro_data.tile6.nc"'
 convert_atm=.true.
 convert_sfc=.true.
 convert_nst=.true.
 input_type="gaussian"
 tracers='"sphum","liq_wat","o3mr","ice_wat","rainwat","snowwat","graupel"'
 tracers_input='"spfh","clwmr","o3mr","icmr","rwmr","snmr","grle"'
 regional=0
 halo_bndy=0


elif [ $gtype = regional ]; then
# set the links to use the 4 halo grid and orog files
# these are necessary for creating the boundary data
#
 ln -sf $FIXDIR/$CASE/${CASE}_grid.tile7.halo4.nc $FIXDIR/$CASE/${CASE}_grid.tile7.nc
 ln -sf $FIXDIR/$CASE/${CASE}_oro_data.tile7.halo4.nc $FIXDIR/$CASE/${CASE}_oro_data.tile7.nc
 ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.vegetation_greenness.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.vegetation_greenness.tile7.nc
 ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.soil_type.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.soil_type.tile7.nc
 ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.slope_type.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.slope_type.tile7.nc
 ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.substrate_temperature.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.substrate_temperature.tile7.nc
 ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.facsf.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.facsf.tile7.nc
 ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.maximum_snow_albedo.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.maximum_snow_albedo.tile7.nc
 ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.snowfree_albedo.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.snowfree_albedo.tile7.nc
 ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.vegetation_type.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.vegetation_type.tile7.nc

 mosaic_file_target_grid="$FIXDIR/$CASE/${CASE}_mosaic.nc"
 orog_files_target_grid='"'${CASE}'_oro_data.tile7.halo4.nc"'
 convert_atm=.true.
 input_type="gaussian"
 tracers='"sphum","liq_wat","o3mr","ice_wat","rainwat","snowwat","graupel"'
 tracers_input='"spfh","clwmr","o3mr","icmr","rwmr","snmr","grle"'
 if [ $REGIONAL -eq 1 ]; then
   regional=1
   convert_sfc=.true.
   convert_nst=.true.
 elif [ $REGIONAL -eq 2 ]; then
   regional=2
   convert_sfc=.false.
   convert_nst=.false.
 else
   echo "WARNING: Wrong gtype: $gtype REGIONAL: $REGIONAL combination"
 fi
 halo_bndy=4
else
  echo "Error: please specify grid type with 'gtype' as uniform, stretch, nest, or regional"
  exit 9
fi

# create namelist and run chgres_cube
#
cat>./fort.41<<EOF
&config
 mosaic_file_target_grid="${mosaic_file_target_grid}"
 fix_dir_target_grid="$FIXDIR/$CASE"
 orog_dir_target_grid="$FIXDIR/$CASE"
 orog_files_target_grid=${orog_files_target_grid}
 vcoord_file_target_grid="${FIXhafs}/fix_am/global_hyblev.l65.txt"
 mosaic_file_input_grid="NULL"
 orog_dir_input_grid="NULL"
 orog_files_input_grid="NULL"
 data_dir_input_grid="${INIDIR}"
 atm_files_input_grid="${atm_files_input_grid}"
 sfc_files_input_grid="${sfc_files_input_grid}"
 cycle_mon=$month
 cycle_day=$day
 cycle_hour=$hour
 convert_atm=${convert_atm}
 convert_sfc=${convert_sfc}
 convert_nst=${convert_nst}
 input_type="${input_type}"
 tracers=${tracers}
 tracers_input=${tracers_input}
 regional=${regional}
 halo_bndy=${halo_bndy}
/
EOF

${APRUNC} ${CHGRESCUBEEXEC}

if [ $gtype = uniform ] || [ $gtype = stretch ] || [ $gtype = nest ];  then

mv gfs_ctrl.nc ${OUTDIR}/gfs_ctrl.nc
tile=1
while [ $tile -le 6 ]; do
  mv out.atm.tile${tile}.nc ${OUTDIR}/gfs_data.tile${tile}.nc
  mv out.sfc.tile${tile}.nc ${OUTDIR}/sfc_data.tile${tile}.nc
  tile=`expr $tile + 1 `
done

elif [ $gtype = regional ]; then
#
# move output files to save directory
#
mv gfs_ctrl.nc ${OUTDIR}/gfs_ctrl.nc
mv gfs.bndy.nc ${OUTDIR}/gfs_bndy.tile7.000.nc
mv out.atm.tile1.nc ${OUTDIR}/gfs_data.tile7.nc
mv out.sfc.tile1.nc ${OUTDIR}/sfc_data.tile7.nc
#
#remove the links that were set above for the halo4 files
#
 rm $FIXDIR/$CASE/${CASE}_grid.tile7.nc
 rm $FIXDIR/$CASE/${CASE}_oro_data.tile7.nc
 rm $FIXDIR/$CASE/${CASE}.vegetation_greenness.tile7.nc
 rm $FIXDIR/$CASE/${CASE}.soil_type.tile7.nc
 rm $FIXDIR/$CASE/${CASE}.slope_type.tile7.nc
 rm $FIXDIR/$CASE/${CASE}.substrate_temperature.tile7.nc
 rm $FIXDIR/$CASE/${CASE}.facsf.tile7.nc
 rm $FIXDIR/$CASE/${CASE}.maximum_snow_albedo.tile7.nc
 rm $FIXDIR/$CASE/${CASE}.snowfree_albedo.tile7.nc
 rm $FIXDIR/$CASE/${CASE}.vegetation_type.tile7.nc
else
  echo "Error: please specify grid type with 'gtype' as uniform, stretch, nest, or regional"
  exit 9
fi

# For the global-nesting configuration, run for the 7th tile
if [ $gtype = nest ];  then

 ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}*.nc $FIXDIR/$CASE/.

 ln -sf $FIXDIR/$CASE/${CASE}_nested_mosaic.nc $FIXDIR/$CASE/${CASE}_mosaic.nc
 export GRIDTYPE=nest
 HALO=${HALO:-0}
 mosaic_file_target_grid="$FIXDIR/$CASE/${CASE}_mosaic.nc"
 orog_files_target_grid='"'${CASE}'_oro_data.tile7.nc"'
 convert_atm=.true.
 convert_sfc=.true.
 convert_nst=.true.
 input_type="gaussian"
 tracers='"sphum","liq_wat","o3mr","ice_wat","rainwat","snowwat","graupel"'
 tracers_input='"spfh","clwmr","o3mr","icmr","rwmr","snmr","grle"'
 regional=0
 halo_bndy=0

# create namelist and run chgres_cube
#
cat>./fort.41<<EOF
&config
 mosaic_file_target_grid="${mosaic_file_target_grid}"
 fix_dir_target_grid="$FIXDIR/$CASE"
 orog_dir_target_grid="$FIXDIR/$CASE"
 orog_files_target_grid=${orog_files_target_grid}
 vcoord_file_target_grid="${FIXhafs}/fix_am/global_hyblev.l65.txt"
 mosaic_file_input_grid="NULL"
 orog_dir_input_grid="NULL"
 orog_files_input_grid="NULL"
 data_dir_input_grid="${INIDIR}"
 atm_files_input_grid="${atm_files_input_grid}"
 sfc_files_input_grid="${sfc_files_input_grid}"
 cycle_mon=$month
 cycle_day=$day
 cycle_hour=$hour
 convert_atm=${convert_atm}
 convert_sfc=${convert_sfc}
 convert_nst=${convert_nst}
 input_type="${input_type}"
 tracers=${tracers}
 tracers_input=${tracers_input}
 regional=${regional}
 halo_bndy=${halo_bndy}
/
EOF

${APRUNC} ${CHGRESCUBEEXEC}

mv out.atm.tile1.nc ${OUTDIR}/gfs_data.tile7.nc
mv out.sfc.tile1.nc ${OUTDIR}/sfc_data.tile7.nc

fi

exit
