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

ymd=`echo $CDATE | cut -c 1-8`
month=`echo $CDATE | cut -c 5-6`
day=`echo $CDATE | cut -c 7-8`
hour=`echo $CDATE | cut -c 9-10`
CDATEprior=`${NDATE} -6 $CDATE`
PDY_prior=`echo ${CDATEprior} | cut -c1-8`
cyc_prior=`echo ${CDATEprior} | cut -c9-10`

WGRIB2=${WGRIB2:-wgrib2}
CHGRESCUBEEXEC=${CHGRESCUBEEXEC:-${EXEChafs}/hafs_chgres_cube.x}

ENSDA=${ENSDA:-NO}

#KKUROSAWA
CREATE_RESTART_GDAS_ENS_FLAG=${CREATE_RESTART_GDAS_ENS_FLAG:-NO}

# Set options specific to the deterministic/ensemble forecast
if [ ${ENSDA} != YES ]; then
  NBDYHRS=${NBDYHRS:-3}
  CASE=${CASE:-C768}
  CRES=`echo $CASE | cut -c 2-`
  gtype=${gtype:-regional}
  ictype=${ictype:-gfsnemsio}
  bctype=${bctype:-gfsnemsio}
  LEVS=${LEVS:-65}
  GRID_intercom=${WORKhafs}/intercom/grid
else
  NBDYHRS=${NBDYHRS_ENS:-3}
  CASE=${CASE_ENS:-C768}
  CRES=`echo $CASE | cut -c 2-`
  gtype=${gtype_ens:-regional}
  ictype=${ictype_ens:-gfsnemsio}
  bctype=${bctype_ens:-gfsnemsio}
  LEVS=${LEVS_ENS:-65}
  GRID_intercom=${WORKhafs}/intercom/grid_ens
fi

# Generate the ICs and BC hour 0
if [ $gtype = regional ] ; then
  export REGIONAL=1
  export HALO=4
else
# for gtype = uniform, stretch, or nest
  export REGIONAL=0
fi

export NTRAC=7
halo_blend=${halo_blend:-0}
vcoord_file_target_grid=${vcoord_file_target_grid:-${FIXhafs}/fix_am/global_hyblev.l${LEVS}.txt}

if [ $GFSVER = "PROD2021" ]; then
 if [ ${ENSDA} = YES ]; then
   export INIDIR=${COMgfs}/enkfgdas.${PDY_prior}/${cyc_prior}/atmos/mem${ENSID}
 else
  export INIDIR=${COMgfs}/gfs.$PDY/$cyc/atmos
 fi
elif [ $GFSVER = "PROD2019" ]; then
 if [ ${ENSDA} = YES ]; then
  export INIDIR=${COMgfs}/enkfgdas.${PDY_prior}/${cyc_prior}/mem${ENSID}
 else
  export INIDIR=${COMgfs}/gfs.$PDY/$cyc
 fi
else
  export INIDIR=${COMgfs}/gfs.$PDY/$cyc
fi


# KKUROSAWA # PROD2021
if [ "${CREATE_RESTART_GDAS_ENS_FLAG}" = YES ]; then

  tmp_ENSID=`expr $ENSID - 0`          # <-- !!
  tmp_memstr=$(printf %03i $tmp_ENSID) # <-- !!

  # GDAS
  if [ $ENSID -gt 40 ]; then 
    tmp_ENSID=`expr $ENSID - 40`
    tmp_memstr=$(printf %03i $tmp_ENSID)
    export tmp_INIDIR=${COMgfs}/enkfgdas.${PDY_prior}/${cyc_prior}/atmos/mem${tmp_memstr}
  fi

  # sgs_tke
  COMhafsprior=${COMhafsprior:-${COMhafs}/../../${CDATEprior}/${STORMID}}
  export sgs_tke_org_file=${COMhafsprior}/RESTART_ens/mem${tmp_memstr}/${PDY}.${cyc}0000.fv_tracer.res.tile1.nc

fi



OUTDIR=${OUTDIR:-${WORKhafs}/intercom/chgres}
DATA=${DATA:-${WORKhafs}/atm_ic}
mkdir -p ${OUTDIR} ${DATA}

FIXDIR=${DATA}/grid
FIXCASE=${DATA}/grid/${CASE}
mkdir -p $DATA ${FIXDIR} ${FIXCASE}

cd $FIXDIR/${CASE}
ln -sf ${GRID_intercom}/${CASE}/* ./

cd $DATA

FHR3="000"
CDUMP=gfs                   # gfs or gdas

# Use gfs netcdf files from GFSv16
if [ $ictype = "gfsnetcdf" ]; then
  if [ ${ENSDA} = YES ]; then
    atm_files_input_grid=gdas.t${cyc_prior}z.atmf006.nc
    sfc_files_input_grid=gdas.t${cyc_prior}z.sfcf006.nc
  else
    atm_files_input_grid=${CDUMP}.t${cyc}z.atmanl.nc
    sfc_files_input_grid=${CDUMP}.t${cyc}z.sfcanl.nc
  fi
  grib2_file_input_grid=""
  input_type="gaussian_netcdf"
  varmap_file=""
  fixed_files_dir_input_grid=""
  tracers='"sphum","liq_wat","o3mr","ice_wat","rainwat","snowwat","graupel"'
  tracers_input='"spfh","clwmr","o3mr","icmr","rwmr","snmr","grle"'
# Use gfs nemsio files from 2019 GFS (fv3gfs)
elif [ $ictype = "gfsnemsio" ]; then
  if [ ${ENSDA} = YES ]; then
    atm_files_input_grid=gdas.t${cyc_prior}z.atmf006.nemsio
    sfc_files_input_grid=gdas.t${cyc_prior}z.sfcf006.nemsio
  else
    atm_files_input_grid=${CDUMP}.t${cyc}z.atmanl.nemsio
    sfc_files_input_grid=${CDUMP}.t${cyc}z.sfcanl.nemsio
  fi
  grib2_file_input_grid=""
  input_type="gaussian_nemsio"
  varmap_file=""
  fixed_files_dir_input_grid=""
  tracers='"sphum","liq_wat","o3mr","ice_wat","rainwat","snowwat","graupel"'
  tracers_input='"spfh","clwmr","o3mr","icmr","rwmr","snmr","grle"'
# Use gfs master grib2 files
elif [ $ictype = "gfsgrib2_master" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.master.pgrb2f${FHR3}
  sfc_files_input_grid=${CDUMP}.t${cyc}z.master.pgrb2f${FHR3}
  grib2_file_input_grid=${CDUMP}.t${cyc}z.master.pgrb2f${FHR3}
  input_type="grib2"
  varmap_file="${HOMEhafs}/sorc/hafs_utils.fd/parm/varmap_tables/GFSphys_var_map.txt"
  fixed_files_dir_input_grid="${HOMEhafs}/sorc/hafs_utils.fd/fix/fix_chgres"
  tracers='"sphum","liq_wat","o3mr"'
  tracers_input='"spfh","clwmr","o3mr"'
# Use gfs 0.25 degree grib2 files
elif [ $ictype = "gfsgrib2_0p25" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3}
  sfc_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3}
  grib2_file_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3}
  input_type="grib2"
  varmap_file="${HOMEhafs}/sorc/hafs_utils.fd/parm/varmap_tables/GFSphys_var_map.txt"
  fixed_files_dir_input_grid="${HOMEhafs}/sorc/hafs_utils.fd/fix/fix_chgres"
  tracers='"sphum","liq_wat","o3mr"'
  tracers_input='"spfh","clwmr","o3mr"'
# Use gfs 0.25 degree grib2 a and b files
elif [ $ictype = "gfsgrib2ab_0p25" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3}
  sfc_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3}
  grib2_file_input_grid=${CDUMP}.t${cyc}z.pgrb2ab.0p25.f${FHR3}
  input_type="grib2"
  varmap_file="${HOMEhafs}/sorc/hafs_utils.fd/parm/varmap_tables/GFSphys_var_map.txt"
  fixed_files_dir_input_grid="${HOMEhafs}/sorc/hafs_utils.fd/fix/fix_chgres"
  tracers='"sphum","liq_wat","o3mr"'
  tracers_input='"spfh","clwmr","o3mr"'
# Use gfs 0.50 degree grib2 files
elif [ $ictype = "gfsgrib2_0p50" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p50.f${FHR3}
  sfc_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p50.f${FHR3}
  grib2_file_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p50.f${FHR3}
  input_type="grib2"
  varmap_file="${HOMEhafs}/sorc/hafs_utils.fd/parm/varmap_tables/GFSphys_var_map.txt"
  fixed_files_dir_input_grid="${HOMEhafs}/sorc/hafs_utils.fd/fix/fix_chgres"
  tracers='"sphum","liq_wat","o3mr"'
  tracers_input='"spfh","clwmr","o3mr"'
# Use gfs 1.00 degree grib2 files
elif [ $ictype = "gfsgrib2_1p00" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.1p00.f${FHR3}
  sfc_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.1p00.f${FHR3}
  grib2_file_input_grid=${CDUMP}.t${cyc}z.pgrb2.1p00.f${FHR3}
  input_type="grib2"
  varmap_file="${HOMEhafs}/sorc/hafs_utils.fd/parm/varmap_tables/GFSphys_var_map.txt"
  fixed_files_dir_input_grid="${HOMEhafs}/sorc/hafs_utils.fd/fix/fix_chgres"
  tracers='"sphum","liq_wat","o3mr"'
  tracers_input='"spfh","clwmr","o3mr"'
else
  echo "FATAL ERROR: unsupportted input data type yet."
  exit 1
fi

if [ $input_type = "grib2" ]; then
  if [ $ictype = gfsgrib2ab_0p25 ]; then
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
  if [ ${ENSDA} = YES ]; then
   ln -sf ${INIDIR}/${atm_files_input_grid} ./
   # KKUROSAWA # PROD2021
   if [ "${CREATE_RESTART_GDAS_ENS_FLAG}" = YES ]; then
     if [ $ENSID -gt 40 ]; then 
       ln -sf ${tmp_INIDIR}/${sfc_files_input_grid} ./
     else
       ln -sf ${INIDIR}/${sfc_files_input_grid} ./
     fi
   else
     ln -sf ${INIDIR}/${sfc_files_input_grid} ./
   fi
   INPDIR="./"
  else
   INPDIR=${INIDIR}
  fi
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
 if [ $input_type = "grib2" ]; then
   convert_nst=.false.
 else
   convert_nst=.true.
 fi
 regional=0
 halo_bndy=0
 halo_blend=0

elif [ $gtype = regional ]; then
# set the links to use the 4 halo grid and orog files
# these are necessary for creating the boundary data
#

 mosaic_file_target_grid="$FIXDIR/$CASE/${CASE}_mosaic.nc"

 # KKUROSAWA
 if [ "${CREATE_RESTART_GDAS_ENS_FLAG}" = YES ]; then
   ln -sf $FIXDIR/$CASE/${CASE}_grid.tile7.halo0.nc $FIXDIR/$CASE/${CASE}_grid.tile7.nc
   ln -sf $FIXDIR/$CASE/${CASE}_oro_data.tile7.halo0.nc $FIXDIR/$CASE/${CASE}_oro_data.tile7.nc
   ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.vegetation_greenness.tile7.halo0.nc $FIXDIR/$CASE/${CASE}.vegetation_greenness.tile7.nc
   ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.soil_type.tile7.halo0.nc $FIXDIR/$CASE/${CASE}.soil_type.tile7.nc
   ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.slope_type.tile7.halo0.nc $FIXDIR/$CASE/${CASE}.slope_type.tile7.nc
   ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.substrate_temperature.tile7.halo0.nc $FIXDIR/$CASE/${CASE}.substrate_temperature.tile7.nc
   ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.facsf.tile7.halo0.nc $FIXDIR/$CASE/${CASE}.facsf.tile7.nc
   ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.maximum_snow_albedo.tile7.halo0.nc $FIXDIR/$CASE/${CASE}.maximum_snow_albedo.tile7.nc
   ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.snowfree_albedo.tile7.halo0.nc $FIXDIR/$CASE/${CASE}.snowfree_albedo.tile7.nc
   ln -sf $FIXDIR/$CASE/fix_sfc/${CASE}.vegetation_type.tile7.halo0.nc $FIXDIR/$CASE/${CASE}.vegetation_type.tile7.nc
   orog_files_target_grid='"'${CASE}'_oro_data.tile7.halo0.nc"'
 else
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
   orog_files_target_grid='"'${CASE}'_oro_data.tile7.halo4.nc"'
 fi

 convert_atm=.true.
 if [ $REGIONAL -eq 1 ]; then
   regional=1
   convert_sfc=.true.
   if [ $input_type = "grib2" ]; then
     convert_nst=.false.
   else
     convert_nst=.true.
   fi
 elif [ $REGIONAL -eq 2 ]; then
   regional=2
   convert_sfc=.false.
   convert_nst=.false.
 else
   echo "WARNING: Wrong gtype: $gtype REGIONAL: $REGIONAL combination"
 fi

 # KKUROSAWA
 if [ "${CREATE_RESTART_GDAS_ENS_FLAG}" = YES ]; then
   halo_bndy=0
 else
   halo_bndy=4
 fi

 halo_blend=${halo_blend}
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
 mosaic_file_input_grid="${mosaic_file_input_grid:-NULL}"
 orog_dir_input_grid="${orog_dir_input_grid:-NULL}"
 orog_files_input_grid="${orog_files_input_grid:-NULL}"
 data_dir_input_grid="${INPDIR}"
 atm_files_input_grid="${atm_files_input_grid}"
 atm_core_files_input_grid="${atm_core_files_input:-NULL}"
 atm_tracer_files_input_grid="${atm_tracer_files_input:-NULL}"
 sfc_files_input_grid="${sfc_files_input_grid}"
 nst_files_input_grid="${nst_files_input_grid:-NULL}"
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

# KKUROSAWA
if [ "${CREATE_RESTART_GDAS_ENS_FLAG}" = YES ]; then
  cp -p /work/noaa/aoml-hafsda/kurosawa/WORK/HAFS_RUN/TEST_20211021/sorc/hafs_utils_new.fd/exec/chgres_cube  ./hafs_chgres_cube.x
else
  cp -p ${CHGRESCUBEEXEC} ./hafs_chgres_cube.x
fi
${APRUNC} ./hafs_chgres_cube.x
#${APRUNC} ${CHGRESCUBEEXEC}

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

# KKUROSAWA
if [ "${CREATE_RESTART_GDAS_ENS_FLAG}" = YES ]; then

  ncks -A -v sgs_tke $sgs_tke_org_file ./fv_tracer.res.tile1.nc # <-- !!

  mv fv_core.res.nc           ${OUTDIR}/
  mv fv_core.res.tile1.nc     ${OUTDIR}/
  mv fv_tracer.res.tile1.nc   ${OUTDIR}/
  mv out.sfc.tile7.nc         ${OUTDIR}/
else
  mv gfs_ctrl.nc ${OUTDIR}/gfs_ctrl.nc
  mv gfs.bndy.nc ${OUTDIR}/gfs_bndy.tile7.000.nc
  mv out.atm.tile7.nc ${OUTDIR}/gfs_data.tile7.nc
  mv out.sfc.tile7.nc ${OUTDIR}/sfc_data.tile7.nc
fi
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
 if [ $input_type = "grib2" ]; then
   convert_nst=.false.
 else
   convert_nst=.true.
 fi
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
 vcoord_file_target_grid="${vcoord_file_target_grid}"
 mosaic_file_input_grid="${mosaic_file_input_grid:-NULL}"
 orog_dir_input_grid="${orog_dir_input_grid:-NULL}"
 orog_files_input_grid="${orog_files_input_grid:-NULL}"
 data_dir_input_grid="${INPDIR}"
 atm_files_input_grid="${atm_files_input_grid}"
 atm_core_files_input_grid="${atm_core_files_input:-NULL}"
 atm_tracer_files_input_grid="${atm_tracer_files_input:-NULL}"
 sfc_files_input_grid="${sfc_files_input_grid}"
 nst_files_input_grid="${nst_files_input_grid:-NULL}"
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

#cp -p ${CHGRESCUBEEXEC} ./hafs_chgres_cube.x
${APRUNC} ./hafs_chgres_cube.x
#${APRUNC} ${CHGRESCUBEEXEC}

mv out.atm.tile1.nc ${OUTDIR}/gfs_data.tile7.nc
mv out.sfc.tile1.nc ${OUTDIR}/sfc_data.tile7.nc

fi

exit
