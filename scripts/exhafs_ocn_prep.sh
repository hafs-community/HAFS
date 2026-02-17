#!/bin/sh
################################################################################
# Script Name: exhafs_ocn_prep.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the HAFS oceanic preprocessing steps to generate MOM6
#   coupling needed ocean initial condition (IC), open boundary condition (OBC)
#   and atmospheric forcings.
# History:
#   05/13/2023: Enabled MOM6 coupling in HAFS application/workflow
# Condition codes:
#   == 0 : success
#   != 0 : fatal error encounted
################################################################################
set -x -o pipefail

CDATE=${CDATE:-${YMDH}}
cyc=${cyc:-00}
STORM=${STORM:-FAKE}
STORMID=${STORMID:-00L}

ymd=`echo $CDATE | cut -c 1-8`
hour=`echo $CDATE | cut -c 9-10`
CDATEprior=`${NDATE} -6 $CDATE`
ymd_prior=`echo ${CDATEprior} | cut -c1-8`
cyc_prior=`echo ${CDATEprior} | cut -c9-10`

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

if [ "${hour}" == "00" ]; then
  type=${type:-n}
else
  type=${type:-f}
fi

# Make the intercom dir
mkdir -p ${WORKhafs}/intercom/ocn_prep/mom6

DATA=${DATA:-${WORKhafs}/ocn_prep}
mkdir -p ${DATA}
cd $DATA

#==============================================================================
# Generate MOM6 ICs from RTOFS
mkdir -p ${DATA}/mom6_init
cd ${DATA}/mom6_init

# Link global RTOFS depth and grid files
if [ ${pubbasin2} = "AL" ] || [ ${pubbasin2} = "EP" ] || [ ${pubbasin2} = "CP" ] || \
   [ ${pubbasin2} = "SL" ] || [ ${pubbasin2} = "LS" ]; then
  ${NLN} ${FIXhafs}/fix_hycom/rtofs_glo.navy_0.08.regional.depth.a regional.depth.a
  ${NLN} ${FIXhafs}/fix_hycom/rtofs_glo.navy_0.08.regional.depth.b regional.depth.b
elif [ ${pubbasin2} = "WP" ] || [ ${pubbasin2} = "IO" ] || \
     [ ${pubbasin2} = "SH" ] || [ ${pubbasin2} = "SP" ] || [ ${pubbasin2} = "SI" ]; then
  ${NLN} ${FIXhafs}/fix_mom6/fix_gofs/depth_GLBb0.08_09m11ob.a regional.depth.a
  ${NLN} ${FIXhafs}/fix_mom6/fix_gofs/depth_GLBb0.08_09m11ob.b regional.depth.b
else
  echo "FATAL ERROR: Unknown/supported basin of ${pubbasin2}"
  exit 1
fi

${NLN} ${FIXhafs}/fix_hycom/rtofs_glo.navy_0.08.regional.grid.a regional.grid.a
${NLN} ${FIXhafs}/fix_hycom/rtofs_glo.navy_0.08.regional.grid.b regional.grid.b

# Link global RTOFS analysis or forecast files
if [ -e ${COMINrtofs}/rtofs.$ymd/rtofs_glo.t00z.${type}${hour}.archv.a ]; then
  ${NLN} ${COMINrtofs}/rtofs.$ymd/rtofs_glo.t00z.${type}${hour}.archv.a archv_in.a
elif [ -e ${COMINrtofs}/rtofs.$ymd/rtofs_glo.t00z.${type}${hour}.archv.a.tgz ]; then
  tar -xpvzf ${COMINrtofs}/rtofs.$ymd/rtofs_glo.t00z.${type}${hour}.archv.a.tgz
  ${NLN} rtofs_glo.t00z.${type}${hour}.archv.a archv_in.a
else
  echo "FATAL ERROR: ${COMINrtofs}/rtofs.$ymd/rtofs_glo.t00z.${type}${hour}.archv.a does not exist."
  echo "FATAL ERROR: ${COMINrtofs}/rtofs.$ymd/rtofs_glo.t00z.${type}${hour}.archv.a.tgz does not exist either."
  echo "FATAL ERROR: Cannot generate MOM6 IC. Exiting"
  exit 1
fi
if [ -e ${COMINrtofs}/rtofs.$ymd/rtofs_glo.t00z.${type}${hour}.archv.b ]; then
  ${NLN} ${COMINrtofs}/rtofs.$ymd/rtofs_glo.t00z.${type}${hour}.archv.b archv_in.b
else
  echo "FATAL ERROR: ${COMINrtofs}/rtofs.$ymd/rtofs_glo.t00z.${type}${hour}.archv.b does not exist."
  echo "FATAL ERROR: Cannot generate MOM6 IC. Exiting"
  exit 1
fi

outnc_2d=ocean_ssh_ic.nc
outnc_ts=ocean_ts_ic.nc
outnc_uv=ocean_uv_ic.nc
export CDF038=rtofs_${outnc_2d}
export CDF034=rtofs_${outnc_ts}
export CDF033=rtofs_${outnc_uv}

# run HYCOM-tools executables to produce IC netcdf files
${NCP} ${PARMmom6}/hafs_mom6_${ocean_domain}.rtofs_ocean_ssh_ic.in ./rtofs_ocean_ssh_ic.in
${APRUNS} ${EXEChafs}/hafs_hycom_utils_archv2ncdf2d.x < ./rtofs_ocean_ssh_ic.in 2>&1 | tee ./archv2ncdf2d_ssh_ic.log
export err=$?; err_chk

${NCP} ${PARMmom6}/hafs_mom6_${ocean_domain}.rtofs_ocean_3d_ic.in ./rtofs_ocean_3d_ic.in
${APRUNS} ${EXEChafs}/hafs_hycom_utils_archv2ncdf3z.x < ./rtofs_ocean_3d_ic.in 2>&1 | tee archv2ncdf3z_3d_ic.log
export err=$?; err_chk

# Convert float to double precision
ncap2 -O -s 'ssh=double(ssh); Latitude=double(Latitude); Longitude=double(Longitude)' rtofs_${outnc_2d} rtofs_${outnc_2d}
export err=$?; err_chk
ncap2 -O -s 'pot_temp=double(pot_temp); salinity=double(salinity); Latitude=double(Latitude); Longitude=double(Longitude); Depth=double(Depth)' rtofs_${outnc_ts} rtofs_${outnc_ts}
export err=$?; err_chk
ncap2 -O -s 'u=double(u); v=double(v); Latitude=double(Latitude); Longitude=double(Longitude); Depth=double(Depth)' rtofs_${outnc_uv} rtofs_${outnc_uv}
export err=$?; err_chk

# SSH file
${USHhafs}/hafs_mom6_ssh_ic.py rtofs_${outnc_2d} ${outnc_2d} | tee ./mom6_ssh_ic.log
export err=$?; err_chk

# TS file
${USHhafs}/hafs_mom6_ts_ic.py rtofs_${outnc_ts} ${outnc_ts} | tee ./mom6_ts_ic.log
export err=$?; err_chk

# UV file
${USHhafs}/hafs_mom6_stagger_uv_ic.py rtofs_${outnc_uv} ${outnc_uv} | tee ./mom6_stagger_uv_ic.log
export err=$?; err_chk

# Convert float to double precision
ncap2 -O -s 'ssh=double(ssh); latitude=double(latitude); longitude=double(longitude)' ${outnc_2d} ${outnc_2d}
export err=$?; err_chk
ncap2 -O -s 'temp=double(temp); salt=double(salt); latitude=double(latitude); longitude=double(longitude); depth=double(depth)' ${outnc_ts} ${outnc_ts}
export err=$?; err_chk
ncap2 -O -s 'u=double(u); v=double(v); lath=double(lath); lonh=double(lonh); depth=double(depth)' ${outnc_uv} ${outnc_uv}
export err=$?; err_chk

# Deliver to intercom
${NCP} -p ${outnc_2d} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_ssh_ic.nc
${NCP} -p ${outnc_ts} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_ts_ic.nc
${NCP} -p ${outnc_uv} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_uv_ic.nc

#==============================================================================

# Generate MOM6 OBC from RTOFS
mkdir -p ${DATA}/mom6_init
cd ${DATA}/mom6_init

# Define output file names
outnc_2d=ocean_ssh_obc.nc
outnc_ts=ocean_ts_obc.nc
outnc_uv=ocean_uv_obc.nc
export CDF038=rtofs_${outnc_2d}
export CDF034=rtofs_${outnc_ts}
export CDF033=rtofs_${outnc_uv}

# run HYCOM-tools executables to produce IC netcdf files
${NCP} ${PARMmom6}/hafs_mom6_${ocean_domain}.rtofs_ocean_ssh_obc.in ./rtofs_ocean_ssh_obc.in
${APRUNS} ${EXEChafs}/hafs_hycom_utils_archv2ncdf2d.x < ./rtofs_ocean_ssh_obc.in 2>&1 | tee ./archv2ncdf2d_ssh_obc.log
export err=$?; err_chk

${NCP} ${PARMmom6}/hafs_mom6_${ocean_domain}.rtofs_ocean_3d_obc.in ./rtofs_ocean_3d_obc.in
${APRUNS} ${EXEChafs}/hafs_hycom_utils_archv2ncdf3z.x < ./rtofs_ocean_3d_obc.in 2>&1 | tee ./archv2ncdf3z_3d_obc.log
export err=$?; err_chk

# Convert float to double precision
ncap2 -O -s 'ssh=double(ssh); Latitude=double(Latitude); Longitude=double(Longitude)' rtofs_${outnc_2d} rtofs_${outnc_2d}
export err=$?; err_chk
ncap2 -O -s 'pot_temp=double(pot_temp); salinity=double(salinity); Latitude=double(Latitude); Longitude=double(Longitude); Depth=double(Depth)' rtofs_${outnc_ts} rtofs_${outnc_ts}
export err=$?; err_chk
ncap2 -O -s 'u=double(u); v=double(v); Latitude=double(Latitude); Longitude=double(Longitude); Depth=double(Depth)' rtofs_${outnc_uv} rtofs_${outnc_uv}
export err=$?; err_chk

${NLN} ${FIXhafs}/fix_mom6/${ocean_domain}/ocean_hgrid.nc ./
${APRUNO} ${USHhafs}/hafs_mom6_obc_from_rtofs.py rtofs_${outnc_2d} rtofs_${outnc_ts} rtofs_${outnc_uv} ocean_hgrid.nc 2>&1 | tee ./mom6_obc_from_rtofs.log
export err=$?; err_chk

# Rename the OBC files
for var in ssh ts uv; do
  for segm in north south east west; do
    mv rtofs_${var}_obc_${segm}.nc ocean_${var}_obc_${segm}.nc
    # Deliver to intercom
    ${NCP} -p ocean_${var}_obc_${segm}.nc ${WORKhafs}/intercom/ocn_prep/mom6/
  done
done

#==============================================================================

# Prepare atmospheric forcings from GFS forcing
mkdir -p ${DATA}/mom6_forcings
cd ${DATA}/mom6_forcings

PARMave=":USWRF:surface|:DSWRF:surface|:ULWRF:surface|:DLWRF:surface|:UFLX:surface|:VFLX:surface|:SHTFL:surface|:LHTFL:surface"
PARMins=":UGRD:10 m above ground|:VGRD:10 m above ground|:PRES:surface|:PRATE:surface|:TMP:surface"
PARMlist="${PARMave}|${PARMins}"

# Use gfs forcing from prior cycle's 6-h forecast
grib2_file=${COMINgfs}/gfs.${ymd_prior}/${cyc_prior}/atmos/gfs.t${cyc_prior}z.pgrb2.0p25.f006
if [ ! -s ${grib2_file} ]; then
  echo "FATAL ERROR: ${grib2_file} does not exist. Exiting"
  exit 1
fi
# Extract atmospheric forcing related variables
${WGRIB2} ${grib2_file} -match "${PARMlist}" -netcdf gfs_global_${ymd_prior}${cyc_prior}_f006.nc

FHRB=${FHRB:-0}
FHRE=${FHRE:-$((${NHRS}+3))}
FHRI=${FHRI:-3}
FHR=${FHRB}
FHR3=$( printf "%03d" "$FHR" )

# Loop for forecast hours
while [ $FHR -le ${FHRE} ]; do

# Use gfs 0.25 degree grib2 files
grib2_file=${COMINgfs}/gfs.${ymd}/${cyc}/atmos/gfs.t${cyc}z.pgrb2.0p25.f${FHR3}

# Check and wait for input data
MAX_WAIT_TIME=${MAX_WAIT_TIME:-900}
n=0
while [ $n -le ${MAX_WAIT_TIME} ]; do
  if [ -s ${grib2_file} ]; then
	while [ $(( $(date +%s) - $(stat -c %Y ${grib2_file}) )) -lt 10  ]; do sleep 10; done
    echo "${grib2_file} ready, continue ..."
    break
  else
    echo "${grib2_file} not ready, sleep 10"
    sleep 10s
  fi
  n=$((n+10))
  if [ $n -gt ${MAX_WAIT_TIME} ]; then
    echo "FATAL ERROR: Waited ${grib2_file} too long $n > ${MAX_WAIT_TIME} seconds. Exiting"
    exit 1
  fi
done

${WGRIB2} ${grib2_file} -match "${PARMlist}" -netcdf gfs_global_${ymd}${cyc}_f${FHR3}.nc

FHR=$(($FHR + ${FHRI}))
FHR3=$(printf "%03d" "$FHR")

done
# End loop for forecast hours

${USHhafs}/hafs_mom6_gfs_forcings.py ${CDATE} -l ${NHRS} 2>&1 | tee ./mom6_gfs_forcings.log
export err=$?; err_chk

# Deliver to intercom
${NCP} -p gfs_forcings.nc ${WORKhafs}/intercom/ocn_prep/mom6/

#==============================================================================

# Set ecflow event if needed
if [ -n "${ECF_NAME}" ]; then
  ecflow_client --event Ocean
fi      

