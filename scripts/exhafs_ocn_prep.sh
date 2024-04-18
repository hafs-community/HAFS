#!/bin/sh
################################################################################
# Script Name: exhafs_ocn_prep.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the HAFS oceanic preprocessing steps to generate MOM6
#   coupling needed ocean initial condition (IC), open boundary condition (OBC)
#   and atmospheric forcings.
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
${NCP} ${PARMhafs}/mom6/regional/hafs_mom6_${ocean_domain}.rtofs_ocean_ssh_ic.in ./rtofs_ocean_ssh_ic.in
${APRUNS} ${EXEChafs}/hafs_hycom_utils_archv2ncdf2d.x < ./rtofs_ocean_ssh_ic.in 2>&1 | tee ./archv2ncdf2d_ssh_ic.log
export err=$?; err_chk

${NCP} ${PARMhafs}/mom6/regional/hafs_mom6_${ocean_domain}.rtofs_ocean_3d_ic.in ./rtofs_ocean_3d_ic.in
${APRUNS} ${EXEChafs}/hafs_hycom_utils_archv2ncdf3z.x < ./rtofs_ocean_3d_ic.in 2>&1 | tee archv2ncdf3z_3d_ic.log
export err=$?; err_chk

# SSH file
# Change into netcdf3 format
ncks -O -3 rtofs_${outnc_2d} rtofs_${outnc_2d}
# Rename variables so they match MOM6 variable name
ncrename -d Latitude,lath -d Longitude,lonh -d MT,time \
         -v Latitude,lath -v Longitude,lonh -v MT,time -v ssh,ave_ssh \
         rtofs_${outnc_2d} ${outnc_2d}
# Change into netcdf4 format
ncks -O -4 ${outnc_2d} ${outnc_2d}
# Convert variable to double precission
ncap2 -O -s "ave_ssh=ave_ssh*1.0" ${outnc_2d} ${outnc_2d}
# _Fillvalues set to zero
ncatted -a _FillValue,ave_ssh,o,f,0.0 ${outnc_2d}

# TS file
# Change into netcdf3 format
ncks -O -3 rtofs_${outnc_ts} rtofs_${outnc_ts}
# Rename variables so that they match MOM6 variable name
ncrename -d Depth,depth -d Latitude,lath -d Longitude,lonh -d MT,time \
         -v Depth,depth -v Latitude,lath -v Longitude,lonh -v MT,time \
         -v pot_temp,Temp -v salinity,Salt \
         rtofs_${outnc_ts} ${outnc_ts}
# Change into netcdf4 format
ncks -O -4 ${outnc_ts} ${outnc_ts}

# UV file
ncrename -d Depth,Layer -d Latitude,lath -d Longitude,lonh -d MT,Time \
         -v Depth,Layer -v Latitude,lath -v Longitude,lonh -v MT,Time \
         rtofs_${outnc_uv} mom6_layer_${outnc_uv}

${NCP} -p mom6_layer_${outnc_uv} hycom_3d.nc

# This method requires to have a MOM.res.nc file for the specific domain as a
# template. If we follow this procedure, probably we should have a MOM.res.nc
# template in the fix MOM6 files
${NCP} ${FIXhafs}/fix_mom6/${ocean_domain}/MOM.res.nc ./
${NCP} ${FIXhafs}/fix_mom6/${ocean_domain}/MOM.res.nc ./MOM.res_ic.nc
nlonq=$(ncks --trd -m MOM.res.nc | grep -E -i ": lonq, size =" | cut -f 7 -d ' ' | uniq)
nlatq=$(ncks --trd -m MOM.res.nc | grep -E -i ": latq, size =" | cut -f 7 -d ' ' | uniq)
ncks -O -F -d latq,2,${nlatq} -d lonq,2,${nlonq} MOM.res_ic.nc MOM.res.nc
ncks -O -F -v u -d lonq,1,1 MOM.res_ic.nc tmp1_u.nc
ncks -O -F -v v -d latq,1,1 MOM.res_ic.nc tmp1_v.nc

# Convert into double precision
ncap2 -O -s "lonh=lonh*1.0" -s "lath=lath*1.0" -s "Layer=Layer*1.0" -s "u=u*1.0" -s "v=v*1.0" hycom_3d.nc hycom_3d.nc

# Extract u and v
ncks -O -v u hycom_3d.nc tmp2_u.nc
ncks -O -v v hycom_3d.nc tmp2_v.nc

ncrename -O -d lonh,lonq -v lonh,lonq tmp2_u.nc tmp2_u.nc
ncrename -O -d lath,latq -v lath,latq tmp2_v.nc tmp2_v.nc

ncks -A -C -v lonq MOM.res.nc tmp2_u.nc
ncks -A -C -v latq MOM.res.nc tmp2_v.nc

ncpdq -O -a lonq,lath,Layer,Time tmp1_u.nc tmp1_u.nc
ncpdq -O -a lonq,lath,Layer,Time tmp2_u.nc tmp2_u.nc

ncpdq -O -a latq,lonh,Layer,Time tmp1_v.nc tmp1_v.nc
ncpdq -O -a latq,lonh,Layer,Time tmp2_v.nc tmp2_v.nc

# Consider speeding up these two ncrcat steps in the future
ncrcat -O tmp1_u.nc tmp2_u.nc tmp_u.nc
ncrcat -O tmp1_v.nc tmp2_v.nc tmp_v.nc

ncpdq -O -a Time,Layer,lath,lonq tmp_u.nc tmp_u.nc
ncpdq -O -a Time,Layer,latq,lonh tmp_v.nc tmp_v.nc

ncks -A -C -v Time,Layer,lath,lonq,u tmp_u.nc tmp_uv.nc
ncks -A -C -v Time,Layer,latq,lonh,v tmp_v.nc tmp_uv.nc

# Set values on the land grids to zero, and deal with first column/row
ncap2 -s 'where(u>100.0) u=0.0' -s 'where(v>100.0) v=0.0' \
      -s 'u=u;u(:,:,:,0)=u(:,:,:,1)' -s 'v=v;v(:,:,0,:)=v(:,:,1,:)' \
      tmp_uv.nc ${outnc_uv}

# Deliver to intercom
${NCP} -p ${outnc_2d} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_ssh_ic.nc
${NCP} -p ${outnc_ts} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_ts_ic.nc
${NCP} -p ${outnc_uv} ${WORKhafs}/intercom/ocn_prep/mom6/ocean_uv_ic.nc

#==============================================================================

# Generate MOM6 OBC from RTOFS
mkdir -p ${DATA}/mom6_init
cd ${DATA}/mom6_init

IFHR=0
FHR=0
FHR2=$(printf "%02d" "$FHR")
FHR3=$(printf "%03d" "$FHR")
NOBCHRS=24

# Loop for obc hours
#while [ $FHR -le $NHRS ]; do

#NEWDATE=$(${NDATE} +${FHR} $CDATE)
#YYYY=$(echo $NEWDATE | cut -c1-4)
#MM=$(echo $NEWDATE | cut -c5-6)
#DD=$(echo $NEWDATE | cut -c7-8)
#HH=$(echo $NEWDATE | cut -c9-10)

# Define output file names
outnc_2d=ocean_ssh_obc.nc
outnc_ts=ocean_ts_obc.nc
outnc_uv=ocean_uv_obc.nc
export CDF038=rtofs.${type}${hour}_${outnc_2d}
export CDF034=rtofs.${type}${hour}_${outnc_ts}
export CDF033=rtofs.${type}${hour}_${outnc_uv}

# run HYCOM-tools executables to produce IC netcdf files
${NCP} ${PARMhafs}/mom6/regional/hafs_mom6_${ocean_domain}.rtofs_ocean_ssh_obc.in ./rtofs_ocean_ssh_obc.in

${APRUNS} ${EXEChafs}/hafs_hycom_utils_archv2ncdf2d.x < ./rtofs_ocean_ssh_obc.in 2>&1 | tee ./archv2ncdf2d_ssh_obc.log
export err=$?; err_chk

${NCP} ${PARMhafs}/mom6/regional/hafs_mom6_${ocean_domain}.rtofs_ocean_3d_obc.in ./rtofs_ocean_3d_obc.in
${APRUNS} ${EXEChafs}/hafs_hycom_utils_archv2ncdf3z.x < ./rtofs_ocean_3d_obc.in 2>&1 | tee ./archv2ncdf3z_3d_obc.log
export err=$?; err_chk

# Run Python script to generate OBC
${NLN} ${FIXhafs}/fix_mom6/${ocean_domain}/ocean_hgrid.nc ./
${USHhafs}/hafs_mom6_obc_from_rtofs.py ./ ./ \
    rtofs.${type}${hour}_${outnc_2d} rtofs.${type}${hour}_${outnc_ts} rtofs.${type}${hour}_${outnc_uv} \
    'Longitude' 'Latitude' ./ocean_hgrid.nc 'x' 'y' 2>&1 | tee ./mom6_obc_from_rtofs.log
export err=$?; err_chk

# next obc hour
#IFHR=$(($IFHR + 1))
#FHR=$(($FHR + $NOBCHRS))
#FHR3=$(printf "%03d" "$FHR")

#done

# Concatenate in time OBC files
for var in ssh ts uv; do
  for segm in north south east west; do
    obc_file=rtofs.${type}${hour}_${var}_obc_${segm}.nc
    ncks ${obc_file} ocean_${var}_obc_${segm}.nc
  # ncap2 -O -s "time=time+$((${NHRS}/24 + 1))" ${obc_file} ${obc_file}
  # ncrcat --record_append ${obc_file} ocean_${var}_obc_${segm}.nc
    # Deliver to intercom
    ${NCP} -p ocean_${var}_obc_${segm}.nc ${WORKhafs}/intercom/ocn_prep/mom6/
  done
done

#==============================================================================

# Prepare atmospheric forcings from GFS forcing
mkdir -p ${WORKhafs}/ocn_prep/mom6_forcings
cd ${WORKhafs}/ocn_prep/mom6_forcings

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
n=1
while [ $n -le 360 ]; do
  if [ -s ${grib2_file} ]; then
	while [ $(( $(date +%s) - $(stat -c %Y ${grib2_file}) )) -lt 10  ]; do sleep 10; done
    echo "${grib2_file} ready, continue ..."
    break
  else
    echo "${grib2_file} not ready, sleep 10"
    sleep 10s
  fi
  if [ $n -ge 360 ]; then
    echo "FATAL ERROR: Waited for ${grib2_file} too many times: $n. Exiting"
    exit 1
  fi
  n=$(( n+1 ))
done

${WGRIB2} ${grib2_file} -match "${PARMlist}" -netcdf gfs_global_${ymd}${cyc}_f${FHR3}.nc

FHR=$(($FHR + ${FHRI}))
FHR3=$(printf "%03d" "$FHR")

done
# End loop for forecast hours

${USHhafs}/hafs_mom6_gfs_forcings.py ${CDATE} -l ${NHRS} 2>&1 | tee ./mom6_gfs_forcings.log
export err=$?; err_chk

# Obtain net longwave and shortwave radiation file
echo 'Obtaining NETLW'
ncks -A gfs_global_${CDATE}_ULWRF.nc -o gfs_global_${CDATE}_LWRF.nc
ncks -A gfs_global_${CDATE}_DLWRF.nc -o gfs_global_${CDATE}_LWRF.nc
ncap2 -v -O -s "NETLW_surface=DLWRF_surface-ULWRF_surface" gfs_global_${CDATE}_LWRF.nc gfs_global_${CDATE}_NETLW.nc
ncatted -O -a long_name,NETLW_surface,o,c,"Net Long-Wave Radiation Flux" gfs_global_${CDATE}_NETLW.nc
ncatted -O -a short_name,NETLW_surface,o,c,"NETLW_surface" gfs_global_${CDATE}_NETLW.nc

echo 'Obtaining NETSW'
ncks -A gfs_global_${CDATE}_USWRF.nc -o gfs_global_${CDATE}_SWRF.nc
ncks -A gfs_global_${CDATE}_DSWRF.nc -o gfs_global_${CDATE}_SWRF.nc
ncap2 -v -O -s "NETSW_surface=DSWRF_surface-USWRF_surface" gfs_global_${CDATE}_SWRF.nc gfs_global_${CDATE}_NETSW.nc
ncatted -O -a long_name,NETSW_surface,o,c,"Net Short-Wave Radiation Flux" gfs_global_${CDATE}_NETSW.nc
ncatted -O -a short_name,NETSW_surface,o,c,"NETSW_surface" gfs_global_${CDATE}_NETSW.nc

# Add four components to the NETSW and DSWRF radiation files
# SWVDF=Visible Diffuse Downward Solar Flux. SWVDF=0.285*DSWRF_surface
# SWVDR=Visible Beam Downward Solar Flux. SWVDR=0.285*DSWRF_surface
# SWNDF=Near IR Diffuse Downward Solar Flux. SWNDF=0.215*DSWRF_surface
# SWNDR=Near IR Beam Downward Solar Flux. SWNDR=0.215*DSWRF_surface
echo 'Adding four components to the NETSW radiation file'
echo 'Adding SWVDF'
ncap2 -v -O -s "SWVDF_surface=float(0.285*DSWRF_surface)" gfs_global_${CDATE}_DSWRF.nc gfs_global_${CDATE}_SWVDF.nc
ncatted -O -a long_name,SWVDF_surface,o,c,"Visible Diffuse Downward Solar Flux" gfs_global_${CDATE}_SWVDF.nc
ncatted -O -a short_name,SWVDF_surface,o,c,"SWVDF_surface" gfs_global_${CDATE}_SWVDF.nc

echo 'Adding SWVDR'
ncap2 -v -O -s "SWVDR_surface=float(0.285*DSWRF_surface)" gfs_global_${CDATE}_DSWRF.nc gfs_global_${CDATE}_SWVDR.nc
ncatted -O -a long_name,SWVDR_surface,o,c,"Visible Beam Downward Solar Flux" gfs_global_${CDATE}_SWVDR.nc
ncatted -O -a short_name,SWVDR_surface,o,c,"SWVDR_surface" gfs_global_${CDATE}_SWVDR.nc

echo 'Adding SWNDF'
ncap2 -v -O -s "SWNDF_surface=float(0.215*DSWRF_surface)" gfs_global_${CDATE}_DSWRF.nc gfs_global_${CDATE}_SWNDF.nc
ncatted -O -a long_name,SWNDF_surface,o,c,"Near IR Diffuse Downward Solar Flux" gfs_global_${CDATE}_SWNDF.nc
ncatted -O -a short_name,SWNDF_surface,o,c,"SWNDF_surface" gfs_global_${CDATE}_SWNDF.nc

echo 'Adding SWNDR'
ncap2 -v -O -s "SWNDR_surface=float(0.215*DSWRF_surface)" gfs_global_${CDATE}_DSWRF.nc gfs_global_${CDATE}_SWNDR.nc
ncatted -O -a long_name,SWNDR_surface,o,c,"Near IR Beam Downward Solar Flux" gfs_global_${CDATE}_SWNDR.nc
ncatted -O -a short_name,SWNDR_surface,o,c,"SWVDR_surface" gfs_global_${CDATE}_SWNDR.nc

echo 'Changing sign to SHTFL, LHTFL, UFLX, VFLX'
ncap2 -v -O -s "SHTFL_surface=float(SHTFL_surface*-1.0)" gfs_global_${CDATE}_SHTFL.nc gfs_global_${CDATE}_SHTFL.nc
ncap2 -v -O -s "LHTFL_surface=float(LHTFL_surface*-1.0)" gfs_global_${CDATE}_LHTFL.nc gfs_global_${CDATE}_LHTFL.nc
ncap2 -v -O -s "UFLX_surface=float(UFLX_surface*-1.0)" gfs_global_${CDATE}_UFLX.nc gfs_global_${CDATE}_UFLX.nc
ncap2 -v -O -s "VFLX_surface=float(VFLX_surface*-1.0)" gfs_global_${CDATE}_VFLX.nc gfs_global_${CDATE}_VFLX.nc

echo 'Adding EVAP'
ncap2 -v -O -s "EVAP_surface=float(LHTFL_surface/(2.5*10^6))" gfs_global_${CDATE}_LHTFL.nc gfs_global_${CDATE}_EVAP.nc
ncatted -O -a long_name,EVAP_surface,o,c,"Evaporation Rate" gfs_global_${CDATE}_EVAP.nc
ncatted -O -a short_name,EVAP_surface,o,c,"EVAP_surface" gfs_global_${CDATE}_EVAP.nc
ncatted -O -a units,EVAP_surface,o,c,"Kg m-2 s-1" gfs_global_${CDATE}_EVAP.nc

# Concatenate all files
fileall="gfs_global_${CDATE}_NETLW.nc \
         gfs_global_${CDATE}_DSWRF.nc \
         gfs_global_${CDATE}_NETSW.nc \
         gfs_global_${CDATE}_SWVDF.nc \
         gfs_global_${CDATE}_SWVDR.nc \
         gfs_global_${CDATE}_SWNDF.nc \
         gfs_global_${CDATE}_SWNDR.nc \
         gfs_global_${CDATE}_LHTFL.nc \
         gfs_global_${CDATE}_EVAP.nc  \
         gfs_global_${CDATE}_SHTFL.nc \
         gfs_global_${CDATE}_UFLX.nc  \
         gfs_global_${CDATE}_VFLX.nc  \
         gfs_global_${CDATE}_UGRD.nc  \
         gfs_global_${CDATE}_VGRD.nc  \
         gfs_global_${CDATE}_PRES.nc  \
         gfs_global_${CDATE}_PRATE.nc \
         gfs_global_${CDATE}_TMP.nc"
# Use cdo merge, which is faster
cdo merge ${fileall} gfs_forcings.nc
# Alternatively, can use ncks, but slower
#for file in ${fileall}; do ncks -h -A ${file} gfs_forcings.nc; done

# Deliver to intercom
${NCP} -p gfs_forcings.nc ${WORKhafs}/intercom/ocn_prep/mom6/

#==============================================================================

# Set ecflow event if needed
if [ "${RUN_ENVIR^^}" = "NCO" ]; then
  ecflow_client --event Ocean
fi      

