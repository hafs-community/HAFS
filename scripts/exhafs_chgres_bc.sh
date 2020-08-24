#!/bin/ksh

set -xe

NHRS=${NHRS:-126}
NBDYHRS=${NBDYHRS:-3}
CASE=${CASE:-C768}
CRES=`echo $CASE | cut -c 2-`

CDUMP=gfs                   # gfs or gdas
LEVS=${LEVS:-65}
gtype=${gtype:-regional}    # grid type = uniform, stretch, nest, or stand alone regional
ictype=${ictype:-gfsnemsio} # gfsnemsio, gfsgrib2_master, gfsgrib2_0p25, gfsgrib2ab_0p25, gfsgrib2_0p50, gfsgrib2_1p00
bctype=${bctype:-gfsnemsio} # gfsnemsio, gfsgrib2_master, gfsgrib2_0p25, gfsgrib2ab_0p25, gfsgrib2_0p50, gfsgrib2_1p00
REGIONAL=${REGIONAL:-0}
halo_blend=${halo_blend:-0}

if [ $gtype = uniform ] || [ $gtype = stretch ] || [ $gtype = nest ];  then
 echo "gtype: $gtype"
 echo "No need to run chgres_bc."
 exit
fi

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

vcoord_file_target_grid=${vcoord_file_target_grid:-${FIXhafs}/fix_am/global_hyblev.l${LEVS}.txt}

WGRIB2=${WGRIB2:-wgrib2}
CHGRESCUBEEXEC=${CHGRESCUBEEXEC:-${EXEChafs}/hafs_chgres_cube.x}

GRID_intercom=${WORKhafs}/intercom/grid
OUTDIR=${OUTDIR:-${WORKhafs}/intercom/chgres}
DATA=${DATA:-${WORKhafs}/chgres_bc}
mkdir -p ${OUTDIR} ${DATA}

FHRB=${FHRB:-${NBDYHRS}}
FHRE=${FHRE:-${NHRS}}
FHRI=${FHRI:-${NBDYHRS}}

FHR=${FHRB}
FHR3=$( printf "%03d" "$FHR" )

# Loop for forecast hours
while [ $FHR -le ${FHRE} ];
do

date
hour_name=${FHR3}
DATA_BC=${DATA}/bc_f${FHR3}
FIXDIR=${DATA_BC}/grid
FIXCASE=${DATA_BC}/grid/${CASE}
mkdir -p $DATA_BC ${FIXDIR} ${FIXCASE}

cd $FIXDIR/${CASE}
ln -sf ${GRID_intercom}/${CASE}/* ./

cd ${DATA_BC}

# Use gfs nemsio files from 2019 GFS (fv3gfs)
if [ $bctype = "gfsnemsio" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.atmf${FHR3}.nemsio
  #sfc_files_input_grid=${CDUMP}.t${cyc}z.sfcf${FHR3}.nemsio
  sfc_files_input_grid=${CDUMP}.t${cyc}z.sfcanl.nemsio
  grib2_file_input_grid=""
  input_type="gaussian_nemsio"
  varmap_file=""
  fixed_files_dir_input_grid=""
  tracers='"sphum","liq_wat","o3mr","ice_wat","rainwat","snowwat","graupel"'
  tracers_input='"spfh","clwmr","o3mr","icmr","rwmr","snmr","grle"'
# Use gfs master grib2 files
elif [ $bctype = "gfsgrib2_master" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.master.pgrb2f${FHR3}
  sfc_files_input_grid=${CDUMP}.t${cyc}z.master.pgrb2f${FHR3}
  grib2_file_input_grid=${CDUMP}.t${cyc}z.master.pgrb2f${FHR3}
  input_type="grib2"
  varmap_file="${HOMEhafs}/sorc/hafs_utils.fd/parm/varmap_tables/GFSphys_var_map.txt"
  fixed_files_dir_input_grid="${HOMEhafs}/sorc/hafs_utils.fd/fix/fix_chgres"
  tracers='"sphum","liq_wat","o3mr"'
  tracers_input='"spfh","clwmr","o3mr"'
# Use gfs 0.25 degree grib2 files
elif [ $bctype = "gfsgrib2_0p25" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3}
  sfc_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3}
  grib2_file_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3}
  input_type="grib2"
  varmap_file="${HOMEhafs}/sorc/hafs_utils.fd/parm/varmap_tables/GFSphys_var_map.txt"
  fixed_files_dir_input_grid="${HOMEhafs}/sorc/hafs_utils.fd/fix/fix_chgres"
  tracers='"sphum","liq_wat","o3mr"'
  tracers_input='"spfh","clwmr","o3mr"'
# Use gfs 0.25 degree grib2 a and b files
elif [ $bctype = "gfsgrib2ab_0p25" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3}
  sfc_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3}
  grib2_file_input_grid=${CDUMP}.t${cyc}z.pgrb2ab.0p25.f${FHR3}
  input_type="grib2"
  varmap_file="${HOMEhafs}/sorc/hafs_utils.fd/parm/varmap_tables/GFSphys_var_map.txt"
  fixed_files_dir_input_grid="${HOMEhafs}/sorc/hafs_utils.fd/fix/fix_chgres"
  tracers='"sphum","liq_wat","o3mr"'
  tracers_input='"spfh","clwmr","o3mr"'
# Use gfs 0.50 degree grib2 files
elif [ $bctype = "gfsgrib2_0p50" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p50.f${FHR3}
  sfc_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p50.f${FHR3}
  grib2_file_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p50.f${FHR3}
  input_type="grib2"
  varmap_file="${HOMEhafs}/sorc/hafs_utils.fd/parm/varmap_tables/GFSphys_var_map.txt"
  fixed_files_dir_input_grid="${HOMEhafs}/sorc/hafs_utils.fd/fix/fix_chgres"
  tracers='"sphum","liq_wat","o3mr"'
  tracers_input='"spfh","clwmr","o3mr"'
# Use gfs 1.00 degree grib2 files
elif [ $bctype = "gfsgrib2_1p00" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.1p00.f${FHR3}
  sfc_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.1p00.f${FHR3}
  grib2_file_input_grid=${CDUMP}.t${cyc}z.pgrb2.1p00.f${FHR3}
  input_type="grib2"
  varmap_file="${HOMEhafs}/sorc/hafs_utils.fd/parm/varmap_tables/GFSphys_var_map.txt"
  fixed_files_dir_input_grid="${HOMEhafs}/sorc/hafs_utils.fd/fix/fix_chgres"
  tracers='"sphum","liq_wat","o3mr"'
  tracers_input='"spfh","clwmr","o3mr"'
else
  echo "ERROR: unsupportted input data type yet."
  exit 1
fi

# Check and wait for the input data
n=1
while [ $n -le 30 ]
do
  if [ -s ${INIDIR}/${atm_files_input_grid} ] && [ -s ${INIDIR}/${sfc_files_input_grid} ]; then
    echo "${INIDIR}/${atm_files_input_grid} and ${INIDIR}/${sfc_files_input_grid} ready, do chgres_bc"
    sleep 1s
    break
  else
    echo "Either ${INIDIR}/${atm_files_input_grid} or ${INIDIR}/${sfc_files_input_grid} not ready, sleep 60"
    sleep 60s
  fi
  n=$(( n+1 ))
done

if [ $input_type = "grib2" ]; then
  if [ $bctype = gfsgrib2ab_0p25 ]; then
    # Use both ${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3} and ${CDUMP}.t${cyc}z.pgrb2b.0p25.f${FHR3} files
    cat ${INIDIR}/${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3} ${INIDIR}/${CDUMP}.t${cyc}z.pgrb2b.0p25.f${FHR3} > ./${grib2_file_input_grid}_tmp
    ${WGRIB2} ${grib2_file_input_grid}_tmp -submsg 1 | ${USHhafs}/hafs_grib2_unique.pl | ${WGRIB2} -i ./${grib2_file_input_grid}_tmp -GRIB ./${grib2_file_input_grid}
    #${WGRIB2} ${grib2_file_input_grid} -inv ./chgres.inv
  else
    ln -sf ${INIDIR}/${grib2_file_input_grid} ./
    #${WGRIB2} ${grib2_file_input_grid} -inv ./chgres.inv
  fi
  INPDIR="./"
else
  INPDIR=${INIDIR}
fi

if [ $gtype = regional ]; then
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
 halo_blend=${halo_blend:-0}
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
 vcoord_file_target_grid="${vcoord_file_target_grid}"
 mosaic_file_input_grid="NULL"
 orog_dir_input_grid="NULL"
 orog_files_input_grid="NULL"
 data_dir_input_grid="${INPDIR}"
 atm_files_input_grid="${atm_files_input_grid}"
 sfc_files_input_grid="${sfc_files_input_grid}"
 grib2_file_input_grid="${grib2_file_input_grid}"
 varmap_file="${varmap_file}"
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
 halo_blend=${halo_blend}
/
EOF

cp -p ${CHGRESCUBEEXEC} ./hafs_chgres_cube.x
${APRUNC} ./hafs_chgres_cube.x
#${APRUNC} ${CHGRESCUBEEXEC}

#
# move output files to save directory
#
if [ $gtype = regional ]; then
  if [ $REGIONAL = 1 ]; then
    mv gfs_ctrl.nc ${OUTDIR}/gfs_ctrl.nc
    mv gfs.bndy.nc ${OUTDIR}/gfs_bndy.tile7.${FHR3}.nc
    mv out.atm.tile1.nc ${OUTDIR}/gfs_data.tile7.nc
    mv out.sfc.tile1.nc ${OUTDIR}/sfc_data.tile7.nc
  elif [ $REGIONAL = 2 ]; then
    mv gfs.bndy.nc ${OUTDIR}/gfs_bndy.tile7.${FHR3}.nc
  else
    echo "WARNING: Wrong gtype: $gtype REGIONAL: $REGIONAL combination"
  fi
fi

FHR=`expr $FHR + ${FHRI}`
FHR3=$( printf "%03d" "$FHR" )

done
# End loop for forecast hours

echo "chgres_bc done"

exit
