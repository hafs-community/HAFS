#!/bin/sh
################################################################################
# Script Name: exhafs_ocn_prep.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the HAFS oceanic preprocessing steps to generate MOM6
#   coupling needed ocean initial condition (IC), open boundary condition (OBC)
#   and atmospheric forcings.
################################################################################
set -xe

NCP=${NCP:-'/bin/cp'}
NLN=${NLN:-'/bin/ln -sf'}
NDATE=${NDATE:-ndate}

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

WGRIB2=${WGRIB2:-wgrib2}

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
${EXEChafs}/hafs_archv2ncdf2d.x < ./rtofs_ocean_ssh_ic.in

${NCP} ${PARMhafs}/mom6/regional/hafs_mom6_${ocean_domain}.rtofs_ocean_3d_ic.in ./rtofs_ocean_3d_ic.in
${EXEChafs}/hafs_archv2ncdf3z.x < ./rtofs_ocean_3d_ic.in

# SSH file
# Rename variables so they match MOM6 variable name
ncrename -d Latitude,lath -d Longitude,lonh -d MT,time \
         -v Latitude,lath -v Longitude,lonh -v MT,time -v ssh,ave_ssh \
         rtofs_${outnc_2d} mom6_${outnc_2d}
# Convert variable to double precission?
ncap2 -O -s ave_ssh=ave_ssh*1.0 mom6_${outnc_2d} ${outnc_2d}
# _Fillvalues set to zero
ncatted -a _FillValue,ave_ssh,o,f,0.0 ${outnc_2d}

# TS file
# Change format to netcdf3 format
ncks -3 rtofs_${outnc_ts} rtofs_nc3_${outnc_ts}
# Rename variables so they match MOM6 variable name
ncrename -d Depth,depth -d Latitude,lath -d Longitude,lonh -d MT,time \
         -v Depth,depth -v Latitude,lath -v Longitude,lonh -v MT,time \
         -v pot_temp,Temp -v salinity,Salt \
         rtofs_nc3_${outnc_ts} mom6_${outnc_ts}
# Change format to netcdf4 format
ncks -4 mom6_${outnc_ts} ${outnc_ts}

# UV file
ncrename -d Depth,Layer -d Latitude,lath -d Longitude,lonh -d MT,Time \
         -v Depth,Layer -v Latitude,lath -v Longitude,lonh -v MT,Time \
         rtofs_${outnc_uv} mom6_layer_${outnc_uv}

cp -f mom6_layer_${outnc_uv} hycom_3d.nc

# This method requires to have a MOM.res.nc file for the specif domain as a template. If we follow this procedure, probably we should have a MOM.res.nc template in the fix MOM6 files
cp ${FIXhafs}/fix_mom6/${ocean_domain}/MOM.res.nc ./
cp ${FIXhafs}/fix_mom6/${ocean_domain}/MOM.res.nc ./MOM.res_ic.nc

nlonq=$(ncks --trd -m MOM.res.nc | grep -E -i ": lonq, size =" | cut -f 7 -d ' ' | uniq)
nlatq=$(ncks --trd -m MOM.res.nc | grep -E -i ": latq, size =" | cut -f 7 -d ' ' | uniq)

ncks -O -F -d latq,2,${nlatq} -d lonq,2,${nlonq} MOM.res_ic.nc MOM.res.nc

ncks -O -F -v u -d lonq,1,1 MOM.res_ic.nc tmp1_u.nc
ncks -O -F -v v -d latq,1,1 MOM.res_ic.nc tmp1_v.nc

# to make double precision, multiply *1.0
ncap2 -O -s lonh=lonh*1.0 hycom_3d.nc hycom_3d.nc
ncap2 -O -s lath=lath*1.0 hycom_3d.nc hycom_3d.nc
ncap2 -O -s Layer=Layer*1.0 hycom_3d.nc hycom_3d.nc
ncap2 -O -s u=u*1.0 hycom_3d.nc hycom_3d.nc
ncap2 -O -s v=v*1.0 hycom_3d.nc hycom_3d.nc

# extract u & v
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

ncrcat -O tmp1_u.nc tmp2_u.nc tmp_u.nc
ncrcat -O tmp1_v.nc tmp2_v.nc tmp_v.nc

ncpdq -O -a Time,Layer,lath,lonq tmp_u.nc tmp_u.nc
ncpdq -O -a Time,Layer,latq,lonh tmp_v.nc tmp_v.nc

ncks -A -C -v Time,Layer,lath,lonq,u tmp_u.nc tmp_uv.nc
ncks -A -C -v Time,Layer,latq,lonh,v tmp_v.nc tmp_uv.nc

cp tmp_uv.nc test_uv.nc

# values on the land grids set to be zero
ncap2 -s 'where (u > 100.0 ) u=0.0' test_uv.nc test_uv_u0.nc
ncap2 -s 'where (v > 100.0 ) v=0.0' test_uv_u0.nc test_uv_00.nc

ncap2 -s 'u=u;u(:,:,:,0)=u(:,:,:,1)' test_uv_00.nc test_uv_01.nc
ncap2 -s 'v=v;v(:,:,0,:)=v(:,:,1,:)' test_uv_01.nc test_uv_02.nc

mv test_uv_02.nc ${outnc_uv}

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
${EXEChafs}/hafs_archv2ncdf2d.x < ./rtofs_ocean_ssh_obc.in

${NCP} ${PARMhafs}/mom6/regional/hafs_mom6_${ocean_domain}.rtofs_ocean_3d_obc.in ./rtofs_ocean_3d_obc.in
${EXEChafs}/hafs_archv2ncdf3z.x < ./rtofs_ocean_3d_obc.in

# Run Python script to generate OBC
${NLN} ${FIXhafs}/fix_mom6/${ocean_domain}/ocean_hgrid.nc ./
${USHhafs}/hafs_mom6_obc_from_rtofs.py ./ ./ rtofs.${type}${hour}_${outnc_2d} rtofs.${type}${hour}_${outnc_ts} rtofs.${type}${hour}_${outnc_uv} 'Longitude' 'Latitude' ./ocean_hgrid.nc 'x' 'y'

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

${USHhafs}/hafs_mom6_gfs_forcings.py ${CDATE} -l ${NHRS} -s ${COMINgfs}

# Deliver to intercom
${NCP} -p gfs_forcings.nc ${WORKhafs}/intercom/ocn_prep/mom6/

#==============================================================================

# Set ecflow event if needed
if [ "${RUN_ENVIR^^}" = "NCO" ]; then
  ecflow_client --event Ocean
fi      

exit
