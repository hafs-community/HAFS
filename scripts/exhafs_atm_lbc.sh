#!/bin/sh
################################################################################
# Script Name: exhafs_atm_lbc.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script generates the atmospheric lateral boundary condition (LBC) at a
#   specific forecast lead time through the UFS_UTIL's chgres_cube tool.
################################################################################
set -x -o pipefail

cyc=${cyc:-00}
CDATE=${CDATE:-${YMDH}}
ymd=$(echo $CDATE | cut -c 1-8)
month=$(echo $CDATE | cut -c 5-6)
day=$(echo $CDATE | cut -c 7-8)
hour=$(echo $CDATE | cut -c 9-10)
CDATEprior=$(${NDATE} -6 $CDATE)
PDY_prior=$(echo ${CDATEprior} | cut -c1-8)
cyc_prior=$(echo ${CDATEprior} | cut -c9-10)

CHGRESCUBEEXEC=${CHGRESCUBEEXEC:-${EXEChafs}/hafs_utils_chgres_cube.x}

ENSDA=${ENSDA:-NO}

# Set options specific to the deterministic/ensemble forecast
if [ ${ENSDA} != YES ]; then
  NBDYHRS=${NBDYHRS:-3}
  CASE=${CASE:-C768}
  CRES=$(echo $CASE | cut -c 2-)
  gtype=${gtype:-regional}
  ictype=${ictype:-gfsnetcdf}
  bctype=${bctype:-gfsnetcdf}
  LEVS=${LEVS:-65}
  GRID_intercom=${WORKhafs}/intercom/grid
  FHRB=$(( ${BC_GROUPI} * ${NBDYHRS} ))
  FHRI=$(( ${BC_GROUPN} * ${NBDYHRS} ))
  FHRE=${NHRS}
else
  NBDYHRS=${NBDYHRS_ENS:-3}
  CASE=${CASE_ENS:-C768}
  CRES=$(echo $CASE | cut -c 2-)
  gtype=${gtype_ens:-regional}
  ictype=${ictype_ens:-gfsnetcdf}
  bctype=${bctype_ens:-gfsnetcdf}
  LEVS=${LEVS_ENS:-65}
  GRID_intercom=${WORKhafs}/intercom/grid_ens
  FHRB=$(( ${BC_GROUPI} * ${NBDYHRS_ENS} ))
  FHRI=$(( ${BC_GROUPN} * ${NBDYHRS_ENS} ))
  FHRE=${NHRS_ENS}
fi

export REGIONAL=2

export NTRAC=7
halo_blend=${halo_blend:-0}
vcoord_file_target_grid=${vcoord_file_target_grid:-${FIXhafs}/fix_am/global_hyblev.l${LEVS}.txt}

if [ "${gtype}" != "regional" ]; then
  echo "INFO: ${gtype} not a regional run."
  echo "INFO: No need to run chgres_bc."
  exit
fi

if [ ${ENSDA} = YES ]; then
  CDUMP=gdas # gfs or gdas
else
  CDUMP=gfs # gfs or gdas
fi

if [ $GFSVER = "PROD2021" ]; then
  if [ ${ENSDA} = YES ]; then
    export OUTDIR=${OUTDIR:-${WORKhafs}/intercom/chgres_ens/mem${ENSID}}
    export INIDIR=${COMINgdas}/enkfgdas.${PDY}/${cyc}/atmos/mem${ENSID}
  else
    export OUTDIR=${OUTDIR:-${WORKhafs}/intercom/chgres}
    export INIDIR=${COMINgfs}/gfs.$PDY/$cyc/atmos
  fi
else
  echo "FATAL ERROR: Unknown or unsupported GFS version ${GFSVER}"
  exit 9
fi

OUTDIR=${OUTDIR:-${WORKhafs}/intercom/chgres}
DATA=${DATA:-${WORKhafs}/atm_lbc}
mkdir -p ${OUTDIR} ${DATA}

FHRB=${FHRB:-${NBDYHRS}}
FHRE=${FHRE:-${NHRS}}
FHRI=${FHRI:-${NBDYHRS}}

# If desired, deletes all the BC output files in intercom
if [ "${BC_CLEANUP^^}" = "YES" ]; then
  FHR=${FHRB}
  FHR3=$(printf "%03d" "$FHR")
  # Loop for forecast hours
  while [ $FHR -le $FHRE ]; do
    rm -f ${OUTDIR}/gfs_bndy.tile7.${FHR3}.nc
    rm -f ${OUTDIR}/bcf${FHR3}
    FHR=$(($FHR + $FHRI))
    FHR3=$(printf "%03d" "$FHR")
  done
  # End loop for forecast hours
fi
# End if for BC_CLEANUP

FHR=${FHRB}
FHR3=$( printf "%03d" "$FHR" )

# Loop for forecast hours
while [ $FHR -le ${FHRE} ]; do

date

# Check if bc has processed this forecast hour previously
if [ -s ${OUTDIR}/bcf${FHR3} ] && \
   [ -s ${OUTDIR}/gfs_bndy.tile7.${FHR3}.nc ]; then

echo "bc done file ${OUTDIR}/bcf${FHR3} exist"
echo "${OUTDIR}/gfs_bndy.tile7.${FHR3}.nc exist"
echo "skip bc for forecast hour ${FHR3}"

# Otherwise run bc for this forecast hour
else

hour_name=${FHR3}
DATA_BC=${DATA}/bc_f${FHR3}
FIXDIR=${DATA_BC}/grid
FIXCASE=${DATA_BC}/grid/${CASE}
mkdir -p $DATA_BC ${FIXDIR} ${FIXCASE}

cd $FIXDIR/${CASE}
${RLN} ${GRID_intercom}/${CASE}/* ./

cd ${DATA_BC}

# Use gfs netcdf files from GFSv16
if [ $bctype = "gfsnetcdf" ]; then
  if [ ${ENSDA} = YES ]; then
    atm_files_input_grid=gdas.t${cyc}z.atmf${FHR3}.nc
    sfc_files_input_grid=gdas.t${cyc}z.sfcf${FHR3}.nc
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
# Use gfs master grib2 files
elif [ $bctype = "gfsgrib2_master" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.master.pgrb2f${FHR3}
  sfc_files_input_grid=${CDUMP}.t${cyc}z.master.pgrb2f${FHR3}
  grib2_file_input_grid=${CDUMP}.t${cyc}z.master.pgrb2f${FHR3}
  input_type="grib2"
  varmap_file="${HOMEhafs}/parm/varmap_tables/GFSphys_var_map.txt"
  fixed_files_dir_input_grid=""
  tracers='"sphum","liq_wat","o3mr"'
  tracers_input='"spfh","clwmr","o3mr"'
# Use gfs 0.25 degree grib2 files
elif [ $bctype = "gfsgrib2_0p25" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3}
  sfc_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3}
  grib2_file_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3}
  input_type="grib2"
  varmap_file="${HOMEhafs}/parm/varmap_tables/GFSphys_var_map.txt"
  fixed_files_dir_input_grid=""
  tracers='"sphum","liq_wat","o3mr"'
  tracers_input='"spfh","clwmr","o3mr"'
# Use gfs 0.25 degree grib2 a and b files
elif [ $bctype = "gfsgrib2ab_0p25" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3}
  sfc_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3}
  grib2_file_input_grid=${CDUMP}.t${cyc}z.pgrb2ab.0p25.f${FHR3}
  input_type="grib2"
  varmap_file="${HOMEhafs}/parm/varmap_tables/GFSphys_var_map.txt"
  fixed_files_dir_input_grid=""
  tracers='"sphum","liq_wat","o3mr"'
  tracers_input='"spfh","clwmr","o3mr"'
# Use gfs 0.50 degree grib2 files
elif [ $bctype = "gfsgrib2_0p50" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p50.f${FHR3}
  sfc_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p50.f${FHR3}
  grib2_file_input_grid=${CDUMP}.t${cyc}z.pgrb2.0p50.f${FHR3}
  input_type="grib2"
  varmap_file="${HOMEhafs}/parm/varmap_tables/GFSphys_var_map.txt"
  fixed_files_dir_input_grid=""
  tracers='"sphum","liq_wat","o3mr"'
  tracers_input='"spfh","clwmr","o3mr"'
# Use gfs 1.00 degree grib2 files
elif [ $bctype = "gfsgrib2_1p00" ]; then
  atm_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.1p00.f${FHR3}
  sfc_files_input_grid=${CDUMP}.t${cyc}z.pgrb2.1p00.f${FHR3}
  grib2_file_input_grid=${CDUMP}.t${cyc}z.pgrb2.1p00.f${FHR3}
  input_type="grib2"
  varmap_file="${HOMEhafs}/parm/varmap_tables/GFSphys_var_map.txt"
  fixed_files_dir_input_grid=""
  tracers='"sphum","liq_wat","o3mr"'
  tracers_input='"spfh","clwmr","o3mr"'
else
  echo "FATAL ERROR: unsupportted input data type yet."
  exit 9
fi

# Check and wait for the input data
MAX_WAIT_TIME=${MAX_WAIT_TIME:-900}
n=0
while [ $n -le ${MAX_WAIT_TIME} ]; do
  if [ -s ${INIDIR}/${atm_files_input_grid}.idx ] && [ -s ${INIDIR}/${sfc_files_input_grid}.idx ] && \
     [ -s ${INIDIR}/${atm_files_input_grid} ] && [ -s ${INIDIR}/${sfc_files_input_grid} ]; then
    echo "${INIDIR}/${atm_files_input_grid} and ${INIDIR}/${sfc_files_input_grid} ready, do chgres_bc"
    break
  elif [ -s ${INIDIR}/${atm_files_input_grid} ] && [ -s ${INIDIR}/${sfc_files_input_grid} ]; then
	while [ $(( $(date +%s) - $(stat -c %Y ${INIDIR}/${atm_files_input_grid}) )) -lt 10  ]; do sleep 10; done
	while [ $(( $(date +%s) - $(stat -c %Y ${INIDIR}/${sfc_files_input_grid}) )) -lt 10  ]; do sleep 10; done
    echo "${INIDIR}/${atm_files_input_grid} and ${INIDIR}/${sfc_files_input_grid} ready, do chgres_bc"
    break
  else
    echo "Either ${INIDIR}/${atm_files_input_grid} or ${INIDIR}/${sfc_files_input_grid} not ready, sleep 10"
    sleep 10s
  fi
  n=$((n+10))
  if [ $n -gt ${MAX_WAIT_TIME} ]; then
    echo "FATAL ERROR: Waited ${INIDIR}/${atm_files_input_grid}, ${INIDIR}/${sfc_files_input_grid} too long $n > ${MAX_WAIT_TIME} seconds. Exiting"
    exit 1
  fi
done

# Check and wait for the pgrb2b input data if needed.
if [ $input_type = "grib2" ] && [ $bctype = gfsgrib2ab_0p25 ]; then

MAX_WAIT_TIME=${MAX_WAIT_TIME:-900}
n=0
while [ $n -le ${MAX_WAIT_TIME} ]; do
  if [ -s ${INIDIR}/${CDUMP}.t${cyc}z.pgrb2b.0p25.f${FHR3}.idx ] && \
     [ -s ${INIDIR}/${CDUMP}.t${cyc}z.pgrb2b.0p25.f${FHR3} ]; then
    echo "${INIDIR}/${CDUMP}.t${cyc}z.pgrb2b.0p25.f${FHR3} ready, do chgres_bc"
    break
  elif [ -s ${INIDIR}/${CDUMP}.t${cyc}z.pgrb2b.0p25.f${FHR3} ]; then
	while [ $(( $(date +%s) - $(stat -c %Y ${INIDIR}/${CDUMP}.t${cyc}z.pgrb2b.0p25.f${FHR3}) )) -lt 10  ]; do sleep 10; done
    echo "${INIDIR}/${CDUMP}.t${cyc}z.pgrb2b.0p25.f${FHR3} ready, do chgres_bc"
    break
  else
    echo "Either ${INIDIR}/${CDUMP}.t${cyc}z.pgrb2b.0p25.f${FHR3} not ready, sleep 10"
    sleep 10s
  fi
  n=$((n+10))
  if [ $n -gt ${MAX_WAIT_TIME} ]; then
    echo "FATAL ERROR: Waited ${INIDIR}/${CDUMP}.t${cyc}z.pgrb2b.0p25.f${FHR3} too long $n > ${MAX_WAIT_TIME} seconds. Exiting"
    exit 1
  fi
done

fi

if [ $input_type = "grib2" ]; then
  if [ $bctype = gfsgrib2ab_0p25 ]; then
    # Use both ${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3} and ${CDUMP}.t${cyc}z.pgrb2b.0p25.f${FHR3} files
    cat ${INIDIR}/${CDUMP}.t${cyc}z.pgrb2.0p25.f${FHR3} ${INIDIR}/${CDUMP}.t${cyc}z.pgrb2b.0p25.f${FHR3} \
        > ./${grib2_file_input_grid}_tmp
    ${WGRIB2} ${grib2_file_input_grid}_tmp -submsg 1 | ${USHhafs}/hafs_grib2_unique.pl \
        | ${WGRIB2} -i ./${grib2_file_input_grid}_tmp -GRIB ./${grib2_file_input_grid}
  else
    ${NLN} ${INIDIR}/${grib2_file_input_grid} ./
  fi
  INPDIR="./"
else
  if [ ${ENSDA} = YES ]; then
    ${NLN} ${INIDIR}/${atm_files_input_grid} ./
    ${NLN} ${INIDIR}/${sfc_files_input_grid} ./
    INPDIR="./"
  else
    INPDIR=${INIDIR}
  fi
fi

if [ $gtype = regional ]; then
# Set the links to use the 4 halo grid and orog files, which are necessary for creating the boundary data
  ${NLN} $FIXDIR/$CASE/${CASE}_grid.tile7.halo4.nc $FIXDIR/$CASE/${CASE}_grid.tile7.nc
  ${NLN} $FIXDIR/$CASE/${CASE}_oro_data.tile7.halo4.nc $FIXDIR/$CASE/${CASE}_oro_data.tile7.nc
  ${NLN} $FIXDIR/$CASE/fix_sfc/${CASE}.vegetation_greenness.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.vegetation_greenness.tile7.nc
  ${NLN} $FIXDIR/$CASE/fix_sfc/${CASE}.soil_type.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.soil_type.tile7.nc
  ${NLN} $FIXDIR/$CASE/fix_sfc/${CASE}.slope_type.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.slope_type.tile7.nc
  ${NLN} $FIXDIR/$CASE/fix_sfc/${CASE}.substrate_temperature.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.substrate_temperature.tile7.nc
  ${NLN} $FIXDIR/$CASE/fix_sfc/${CASE}.facsf.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.facsf.tile7.nc
  ${NLN} $FIXDIR/$CASE/fix_sfc/${CASE}.maximum_snow_albedo.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.maximum_snow_albedo.tile7.nc
  ${NLN} $FIXDIR/$CASE/fix_sfc/${CASE}.snowfree_albedo.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.snowfree_albedo.tile7.nc
  ${NLN} $FIXDIR/$CASE/fix_sfc/${CASE}.vegetation_type.tile7.halo4.nc $FIXDIR/$CASE/${CASE}.vegetation_type.tile7.nc
  if [ $nest_grids -gt 1 ]; then
    ${NLN} $FIXDIR/$CASE/${CASE}_coarse_mosaic.nc $FIXDIR/$CASE/${CASE}_mosaic.nc
  fi
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
  echo "FATAL ERROR: please specify grid type with 'gtype' as uniform, stretch, nest, or regional"
  exit 9
fi

# Create namelist and run chgres_cube
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

${NCP} -p ${CHGRESCUBEEXEC} ./hafs_utils_chgres_cube.x
${SOURCE_PREP_STEP}
${APRUNC} ./hafs_utils_chgres_cube.x 2>&1 | tee ./chgres_cube_lbc_${FHR3}.log
export err=$?; err_chk

# Move output files to save directory
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

# Write out the bcdone message file
echo 'done' > ${OUTDIR}/bcf${FHR3}

fi
# End if for checking if bc has processed this forecast hour previously

FHR=$(($FHR + ${FHRI}))
FHR3=$(printf "%03d" "$FHR")

done
# End loop for forecast hours

echo "chgres_bc done"

exit
