#!/bin/sh
################################################################################
# Script Name: exhafs_atm_post.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the HAFS atmospheric post-processing steps including
#   running UPP for regular and satellite post (if needed), subsetting the
#   vortex tracker needed grib2 records, and NHC requested grib2 records (if
#   chosen).
################################################################################
set -x -o pipefail

export MP_LABELIO=yes

CDATE=${CDATE:-${YMDH}}

# Sepcial settings if this is an atm_init run
if [ ${RUN_INIT:-NO} = YES ]; then

# Turn off satpost and nhcpost for atm_init run to save time
satpost=.false.
nhcpost=.false.

if [ "${ENSDA}" = YES ]; then
  INPdir=${WORKhafs}/intercom/forecast_init_ens/mem${ENSID}
  COMOUTpost=${WORKhafs}/intercom/atm_init_ens/mem${ENSID}
  intercom=${WORKhafs}/intercom/atm_init_ens/mem${ENSID}/post
  NHRS_ENS=0
elif [ ${FGAT_MODEL} = gdas ]; then
  INPdir=${WORKhafs}/intercom/forecast_init_fgat${FGAT_HR}
  COMOUTpost=${WORKhafs}/intercom/atm_init_fgat${FGAT_HR}
  intercom=${WORKhafs}/intercom/atm_init_fgat${FGAT_HR}/post
  NHRS=0
else
  INPdir=${WORKhafs}/intercom/forecast_init
  COMOUTpost=${WORKhafs}/intercom/atm_init
  intercom=${WORKhafs}/intercom/atm_init/post
  NHRS=0
fi

RESTARTcom=""

else

if [ "${ENSDA}" = YES ]; then
  INPdir=${WORKhafs}/intercom/forecast_ens/mem${ENSID}
  COMOUTpost=${COMhafs}/post_ens/mem${ENSID}
  intercom=${WORKhafs}/intercom/post_ens/mem${ENSID}
  RESTARTcom=${COMhafs}/${out_prefix}.RESTART_ens/mem${ENSID}
else
  INPdir=${WORKhafs}/intercom/forecast
  COMOUTpost=${COMhafs}
  intercom=${WORKhafs}/intercom/post
  RESTARTcom=${COMhafs}/${out_prefix}.RESTART
fi

fi

if [ ${ENSDA} = YES ]; then
# Ensemble member with ENSID <= ${ENS_FCST_SIZE} will run the full-length NHRS forecast
  if [ $((10#${ENSID})) -le ${ENS_FCST_SIZE:-10} ]; then
    NHRS=${NHRS:-126}
  else
    NHRS=${NHRS_ENS:-6}
  fi
  NBDYHRS=${NBDYHRS_ENS:-3}
  NOUTHRS=${NOUTHRS_ENS:-3}
  gtype=${gtype_ens:-regional}
  post_gridspecs=${post_gridspecs_ens:-""}
  trak_gridspecs=${trak_gridspecs_ens:-""}
  satpost=${satpost_ens:-".false."}
  nhcpost=${nhcpost_ens:-".false."}
else
  NHRS=${NHRS:-126}
  NBDYHRS=${NBDYHRS:-3}
  NOUTHRS=${NOUTHRS:-3}
  gtype=${gtype:-regional}
  post_gridspecs=${post_gridspecs:-""}
  trak_gridspecs=${trak_gridspecs:-""}
  satpost=${satpost:-".false."}
  nhcpost=${nhcpost:-".false."}
fi

out_prefix=${out_prefix:-$(echo "${STORMID,,}.${CDATE}")}
output_grid=${output_grid:-rotated_latlon}

POSTEXEC=${POSTEXEC:-${EXEChafs}/hafs_post.x}
FIXcrtm=${FIXcrtm:-${CRTM_FIX:?}}

SENDCOM=${SENDCOM:-YES}
intercom=${intercom:-${WORKhafs}/intercom/post}
COMOUTpost=${COMOUTpost:-${COMhafs}}
DATA=${DATA:-${WORKhafs}/atm_post}
mkdir -p ${COMOUTpost} ${intercom}
mkdir -p ${DATA}

if [ ! -z "${RESTARTcom}" ]; then
  mkdir -p ${RESTARTcom}
fi

FHRB=$(( $((${POST_GROUPI:-1}-1)) * ${NOUTHRS} ))
FHRI=$(( ${POST_GROUPN:-1} * ${NOUTHRS} ))

# If desired, deletes all the post output files in COMOUTpost and intercom
if [ "${POST_CLEANUP^^}" = "YES" ]; then
  FHR=${FHRB:-0}
  FHR3=$(printf "%03d" "$FHR")
  # Loop for forecast hours
  while [ $FHR -le $NHRS ]; do
    if [ ${gtype} = nest ]; then
      ngrids=$((${nest_grids} + 1))
    else
      ngrids=${nest_grids}
    fi
    # Loop for grids/domains
    for ng in $(seq 1 ${ngrids}); do
      gridstr=$(echo ${out_gridnames} | cut -d, -f ${ng})
      grb2file=${out_prefix}.${RUN}.${gridstr}.atm.f${FHR3}.grb2
      grb2indx=${out_prefix}.${RUN}.${gridstr}.atm.f${FHR3}.grb2.idx
      sat_grb2file=${out_prefix}.${RUN}.${gridstr}.sat.f${FHR3}.grb2
      sat_grb2indx=${out_prefix}.${RUN}.${gridstr}.sat.f${FHR3}.grb2.idx
      nhc_grb2file=${out_prefix}.${RUN}.${gridstr}.nhc.f${FHR3}.grb2
      nhc_grb2indx=${out_prefix}.${RUN}.${gridstr}.nhc.f${FHR3}.grb2.idx
      trk_grb2file=${out_prefix}.${RUN}.${gridstr}.trk.f${FHR3}.grb2
      trk_grb2indx=${out_prefix}.${RUN}.${gridstr}.trk.f${FHR3}.grb2.ix
      rm -f ${COMOUTpost}/${grb2file} ${COMOUTpost}/${grb2indx}
      rm -f ${COMOUTpost}/${sat_grb2file} ${COMOUTpost}/${sat_grb2indx}
      rm -f ${intercom}/${trk_grb2file} ${intercom}/${trk_grb2indx}
    done
    # End loop for grids/domains
    FHR=$(($FHR + $FHRI))
    FHR3=$(printf "%03d" "$FHR")
  done
  # End loop for forecast hours
fi
# End if for POST_CLEANUP

# Start actual post
IFHR=0
FHR=${FHRB:-0}
FHR2=$(printf "%02d" "$FHR")
FHR3=$(printf "%03d" "$FHR")

# Loop for forecast hours
while [ $FHR -le $NHRS ]; do

cd ${DATA}

NEWDATE=$(${NDATE} +${FHR} $CDATE)
YYYY=$(echo $NEWDATE | cut -c1-4)
MM=$(echo $NEWDATE | cut -c5-6)
DD=$(echo $NEWDATE | cut -c7-8)
HH=$(echo $NEWDATE | cut -c9-10)

if [ ${gtype} = nest ]; then
  ngrids=$((${nest_grids} + 1))
else
  ngrids=${nest_grids}
fi

# Loop for grids/domains
for ng in $(seq 1 ${ngrids}); do

if [[ $ng -eq 1 ]]; then
  neststr=""
  tilestr=".tile1"
  nesttilestr=""
  nestdotstr=""
else
  neststr=".nest$(printf '%02d' ${ng})"
  tilestr=".tile$(printf '%d' ${ng})"
  nesttilestr=".nest$(printf '%02d' ${ng}).tile$(printf '%d' ${ng})"
  nestdotstr=".nest$(printf '%02d' ${ng})."
fi

gridstr=$(echo ${out_gridnames} | cut -d, -f ${ng})

outputgrid=$(echo ${output_grid} | cut -d, -f ${ng})
postgridspecs=$(echo ${post_gridspecs} | cut -d, -f ${ng})
trakgridspecs=$(echo ${trak_gridspecs} | cut -d, -f ${ng})

grb2post=${out_prefix}.${RUN}.${gridstr}.atm.f${FHR3}.postgrb2
grb2file=${out_prefix}.${RUN}.${gridstr}.atm.f${FHR3}.grb2
grb2indx=${out_prefix}.${RUN}.${gridstr}.atm.f${FHR3}.grb2.idx
sat_grb2post=${out_prefix}.${RUN}.${gridstr}.sat.f${FHR3}.postgrb2
sat_grb2file=${out_prefix}.${RUN}.${gridstr}.sat.f${FHR3}.grb2
sat_grb2indx=${out_prefix}.${RUN}.${gridstr}.sat.f${FHR3}.grb2.idx
nhc_grb2file=${out_prefix}.${RUN}.${gridstr}.nhc.f${FHR3}.grb2
nhc_grb2indx=${out_prefix}.${RUN}.${gridstr}.nhc.f${FHR3}.grb2.idx
trk_grb2file=${out_prefix}.${RUN}.${gridstr}.trk.f${FHR3}.grb2
trk_grb2indx=${out_prefix}.${RUN}.${gridstr}.trk.f${FHR3}.grb2.ix

fort_patcf="fort.6$(printf '%02d' ${ng})"
trk_patcf=${out_prefix}.${RUN}.trak.patcf

# Check if post has processed this forecast hour previously
if [ -s ${intercom}/post${nestdotstr}f${FHR3} ] && \
   [ ${intercom}/post${nestdotstr}f${FHR3} -nt ${INPdir}/log.atm.f${FHR3} ] && \
   [ -s ${COMOUTpost}/${grb2file} ] && \
   [ -s ${COMOUTpost}/${grb2indx} ]; then

echo "post done file ${intercom}/post${nestdotstr}f${FHR3} exist and newer than ${INPdir}/log.atm.f${FHR3}"
echo "product ${COMOUTpost}/${grb2file} exist"
echo "product ${COMOUTpost}/${grb2indx} exist"
echo "skip post for forecast hour ${FHR3} valid at ${NEWDATE}"

# Otherwise run post for this forecast hour
else

if [ ${write_dopost:-.false.} = .true. ]; then

# Wait for model output
MAX_WAIT_TIME=${MAX_WAIT_TIME:-900}
n=0
while [ $n -le ${MAX_WAIT_TIME} ]; do
  if [ ! -s ${INPdir}/log.atm.f${FHR3} ] || [ ! -s ${INPdir}/HURPRS${neststr}.GrbF${FHR2} ]; then
    echo "${INPdir}/log.atm.f${FHR3} not ready, sleep 10s"
    sleep 10s
  else
    echo "${INPdir}/log.atm.f${FHR3}, ${INPdir}/HURPRS${neststr}.GrbF${FHR2} ready, continue"
    break
  fi
  if [ $n -gt ${MAX_WAIT_TIME} ]; then
    echo "FATAL ERROR: Waited ${INPdir}/log.atm.f${FHR3} , ${INPdir}/HURPRS${neststr}.GrbF${FHR2} too long $n > ${MAX_WAIT_TIME} seconds. Exiting"
    exit 1
  fi
  n=$((n+10))
done

else

# Wait for model output
MAX_WAIT_TIME=${MAX_WAIT_TIME:-900}
n=0
while [ $n -le ${MAX_WAIT_TIME} ]; do
  if [ ! -s ${INPdir}/log.atm.f${FHR3} ] || \
     [ ! -s ${INPdir}/atm${nestdotstr}f${FHR3}.nc ] || \
     [ ! -s ${INPdir}/sfc${nestdotstr}f${FHR3}.nc ]; then
    echo "${INPdir}/log.atm.f${FHR3} not ready, sleep 10s"
    sleep 10s
  else
    echo "${INPdir}/log.atm.f${FHR3}, ${INPdir}/atm${nestdotstr}f${FHR3}.nc ${INPdir}/sfc${nestdotstr}f${FHR3}.nc ready, do post"
    break
  fi
  if [ $n -gt ${MAX_WAIT_TIME} ]; then
    echo "FATAL ERROR: Waited ${INPdir}/log.atm.f${FHR3}, ${INPdir}/atm${nestdotstr}f${FHR3}.nc, ${INPdir}/sfc${nestdotstr}f${FHR3}.nc too long $n > ${MAX_WAIT_TIME} seconds. Exiting"
    exit 1
  fi
  n=$((n+10))
done

fi #if [ ${write_dopost:-.false.} = .true. ]

# Create the post working dir for the time level
DATA_POST=${DATA}/post${neststr}_${NEWDATE}
rm -rf ${DATA_POST}
mkdir -p ${DATA_POST}
cd ${DATA_POST}

# Note: Currently the inline post (write_dopost) does not support nesting configurations yet.
if [ ${write_dopost:-.false.} = .true. ]; then

${NCP} -p ${INPdir}/HURPRS${neststr}.GrbF${FHR2} ${grb2post}
if [ ${satpost} = .true. ]; then
  ${NCP} -p ${INPdir}/HURSAT${neststr}.GrbF${FHR2} ${sat_grb2post}
fi

else

# Preparte itag namelist input file
cat>itag<<EOF
&model_inputs
fileName='${INPdir}/atm${nestdotstr}f${FHR3}.nc'
IOFORM=netcdf
grib='grib2'
DateStr='${YYYY}-${MM}-${DD}_${HH}:00:00'
MODELNAME='FV3R'
fileNameFlux='${INPdir}/sfc${nestdotstr}f${FHR3}.nc'
/
&NAMPGB
KPO=47,PO=1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1.,numx=2
/
EOF

# Copy fix files
${NCP} ${PARMhafs}/post/nam_micro_lookup.dat ./eta_micro_lookup.dat
${NCP} ${PARMhafs}/post/params_grib2_tbl_new ./params_grib2_tbl_new

if [ ${satpost} = .true. ]; then
# ${NCP} ${PARMhafs}/post/postxconfig-NT-hafs_sat.txt ./postxconfig-NT.txt
  ${NCP} ${PARMhafs}/post/postxconfig-NT-hafs.txt ./postxconfig-NT.txt
  # Link crtm fix files
  # Link crtm fix files
  for file in "amsre_aqua" "imgr_g11" "imgr_g12" "imgr_g13" \
    "imgr_g15" "imgr_mt1r" "imgr_mt2" "seviri_m10" \
    "ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16" \
    "ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" \
    "tmi_trmm" "imgr_insat3d" "abi_gr" "abi_g16" \
    "abi_g17" "ahi_himawari8" ; do
    ${NLN} ${FIXcrtm}/${file}.TauCoeff.bin ./
  done
  for file in "amsre_aqua" "imgr_g11" "imgr_g12" "imgr_g13" \
    "imgr_g15" "imgr_mt1r" "imgr_mt2" "seviri_m10" \
    "ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16" \
    "ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" \
    "tmi_trmm" "v.seviri_m10" "imgr_insat3d" "abi_gr" \
    "abi_g16" "abi_g17" "ahi_himawari8" ; do
    ${NLN} ${FIXcrtm}/${file}.SpcCoeff.bin ./
  done
  for file in "Aerosol" "Cloud"; do
    ${NLN} ${FIXcrtm}/${file}Coeff.bin ./
  done
  for file in ${FIXcrtm}/*Emis*; do
    ${NLN} ${file} ./
  done
else
  ${NCP} ${PARMhafs}/post/postxconfig-NT-hafs_nosat.txt ./postxconfig-NT.txt
fi

# Run post
${NCP} -p ${POSTEXEC} ./hafs_post.x
#${APRUNC} ./hafs_post.x < itag >> $pgmout 2>errfile
#export err=$?; err_chk
#if [ -e "${pgmout}" ]; then cat ${pgmout}; fi
${APRUNC} ./hafs_post.x < itag 2>&1 | tee ./post_${NEWDATE}.log
export err=$?; err_chk

mv HURPRS.GrbF${FHR2} ${grb2post}
if [ ${satpost} = .true. ]; then
  mv HURSAT.GrbF${FHR2} ${sat_grb2post}
fi

fi #if [ ${write_dopost:-.false.} = .true. ]

if [ ${postgridspecs} = auto ]; then
  clon=$(echo ${output_grid_cen_lon} | cut -d , -f 1)
  clat=$(echo ${output_grid_cen_lat} | cut -d , -f 1)
  lon_span=$(echo ${output_grid_lon_span} | cut -d , -f 1)
  lat_span=$(echo ${output_grid_lat_span} | cut -d , -f 1)
  latlon_dlon=$(printf "%.6f" $(echo ${output_grid_dlon} | cut -d , -f ${ng}))
  latlon_dlat=$(printf "%.6f" $(echo ${output_grid_dlat} | cut -d , -f ${ng}))
  if [[ "$outputgrid" = "rotated_latlon"* ]]; then
    latlon_lon0=$(printf "%.6f" $(bc <<< "scale=6; ${clon}-${lon_span}/2.0-9.0"))
    latlon_nlon=$(printf "%.0f" $(bc <<< "scale=6; (${lon_span}+18.0)/${latlon_dlon}"))
  else
    latlon_lon0=$(printf "%.6f" $(bc <<< "scale=6; ${clon}-${lon_span}/2.0"))
    latlon_nlon=$(printf "%.0f" $(bc <<< "scale=6; ${lon_span}/${latlon_dlon}"))
  fi
  latlon_lat0=$(printf "%.6f" $(bc <<< "scale=6; ${clat}-${lat_span}/2.0"))
  latlon_nlat=$(printf "%.0f" $(bc <<< "scale=6; ${lat_span}/${latlon_dlat}"))
  postgridspecs="latlon ${latlon_lon0}:${latlon_nlon}:${latlon_dlon} ${latlon_lat0}:${latlon_nlat}:${latlon_dlat}"
fi

if [ ${trakgridspecs} = auto ]; then
  trakgridspecs=${postgridspecs}
fi

if [[ "$outputgrid" = "rotated_latlon"* ]]; then

# For rotated_latlon output grid
# Convert from rotate lat-lon grib2 to regular lat-lon grib2
# Parallelize this section to speed up wgrib2
#opts='-set_bitmap 1 -set_grib_type c3 -new_grid_winds grid -new_grid_vectors "UGRD:VGRD" -new_grid_interpolation neighbor'
opts='-set_grib_type c2 -new_grid_winds grid -new_grid_vectors "UGRD:VGRD" -new_grid_interpolation neighbor'
rm -f cmdfile
echo ${WGRIB2} ${grb2post} -match '":2 mb:|:5 mb:|:7 mb:|:10 mb:|:20 mb:"' ${opts} -new_grid ${postgridspecs} ${grb2post}.part00 >  cmdfile
echo ${WGRIB2} ${grb2post} -match '":30 mb:|:50 mb:|:70 mb:|:100 mb:"'     ${opts} -new_grid ${postgridspecs} ${grb2post}.part01 >> cmdfile
echo ${WGRIB2} ${grb2post} -match '":125 mb:|:150 mb:|:175 mb:|:200 mb:"'  ${opts} -new_grid ${postgridspecs} ${grb2post}.part02 >> cmdfile
echo ${WGRIB2} ${grb2post} -match '":225 mb:|:250 mb:|:275 mb:|:300 mb:"'  ${opts} -new_grid ${postgridspecs} ${grb2post}.part03 >> cmdfile
echo ${WGRIB2} ${grb2post} -match '":325 mb:|:350 mb:|:375 mb:|:400 mb:"'  ${opts} -new_grid ${postgridspecs} ${grb2post}.part04 >> cmdfile
echo ${WGRIB2} ${grb2post} -match '":425 mb:|:450 mb:|:475 mb:|:500 mb:"'  ${opts} -new_grid ${postgridspecs} ${grb2post}.part05 >> cmdfile
echo ${WGRIB2} ${grb2post} -match '":525 mb:|:550 mb:|:575 mb:|:600 mb:"'  ${opts} -new_grid ${postgridspecs} ${grb2post}.part06 >> cmdfile
echo ${WGRIB2} ${grb2post} -match '":625 mb:|:650 mb:|:675 mb:|:700 mb:"'  ${opts} -new_grid ${postgridspecs} ${grb2post}.part07 >> cmdfile
echo ${WGRIB2} ${grb2post} -match '":725 mb:|:750 mb:|:775 mb:|:800 mb:"'  ${opts} -new_grid ${postgridspecs} ${grb2post}.part08 >> cmdfile
echo ${WGRIB2} ${grb2post} -match '":825 mb:|:850 mb:|:875 mb:|:900 mb:"'  ${opts} -new_grid ${postgridspecs} ${grb2post}.part09 >> cmdfile
echo ${WGRIB2} ${grb2post} -match '":925 mb:|:950 mb:|:975 mb:|:1000 mb:"' ${opts} -new_grid ${postgridspecs} ${grb2post}.part10 >> cmdfile
echo ${WGRIB2} ${grb2post} -not '" mb:"'                                   ${opts} -new_grid ${postgridspecs} ${grb2post}.part11 >> cmdfile
if [ ${satpost} = .true. ]; then
  echo ${WGRIB2} ${sat_grb2post}                                           ${opts} -new_grid ${postgridspecs} ${sat_grb2file} >> cmdfile
fi
chmod +x cmdfile
${APRUNC} ${MPISERIAL} -m cmdfile
export err=$?; err_chk
# Cat the temporary files together
cat ${grb2post}.part?? > ${grb2file}
# clean up the temporary files
rm -f ${grb2post}.part??

elif [[ "$outputgrid" = "regional_latlon"* ]]; then

# For regional_latlon output grid, no need to convert
mv ${grb2post} ${grb2file}
if [ ${satpost} = .true. ]; then
  mv ${sat_grb2post} ${sat_grb2file}
fi

## Alternatively, can use wgrib2 to convert from c3 to c2 packing, which can reduce the filesize by ~30%.
#opts='-set_grib_type c2 -grib_out'

else

echo "FATAL ERROR: output grid: ${outputgrid} not supported. Exiting"
exit 1

fi #if [[ "$outputgrid" = "rotated_latlon"* ]]; then

# Generate the grib2 index file
${WGRIB2} -s ${grb2file} > ${grb2indx}
export err=$?; err_chk
if [ ${satpost} = .true. ]; then
  ${WGRIB2} -s ${sat_grb2file} > ${sat_grb2indx}
  export err=$?; err_chk
fi

# Subsetting hafs grib2 files for NHC
if [ ${nhcpost} = .true. ]; then
  PARMlist=$(sed -z 's/\n/|/g' ${PARMhafs}/post/hafs_subset_nhc.txt | sed -e 's/.$//g')
  ${WGRIB2} ${grb2file} -match "${PARMlist}" -grib ${nhc_grb2file}
  export err=$?; err_chk
  if [ ${satpost} = .true. ]; then
    ${WGRIB2} -append ${sat_grb2file} -match "${PARMlist}" -grib ${nhc_grb2file}
    export err=$?; err_chk
  fi
  ${WGRIB2} -s ${nhc_grb2file} > ${nhc_grb2indx}
  export err=$?; err_chk
fi

# Extract hafstrk grib2 files for the tracker
PARMlistp1="UGRD:850|UGRD:700|UGRD:500|VGRD:850|VGRD:700|VGRD:500|UGRD:10 m a|VGRD:10 m a|ABSV:850|ABSV:700|MSLET"
PARMlistp2="HGT:900|HGT:850|HGT:800|HGT:750|HGT:700|HGT:650|HGT:600|HGT:550|HGT:500|HGT:450|HGT:400"
PARMlistp3="HGT:350|HGT:300|HGT:250|HGT:200|TMP:500|TMP:450|TMP:400|TMP:350|TMP:300|TMP:250|TMP:200"
PARMlist=${PARMlistp1}"|"${PARMlistp2}"|"${PARMlistp3}
echo ${PARMlist}

${WGRIB2} ${grb2file} -match "${PARMlist}" -grib ${trk_grb2file}
export err=$?; err_chk

# If desired, create the combined grid01 and grid02 hafstrk grib2 file and use it to replace the grid02 hafstrk grib2 file
if [ ${trkd12_combined:-no} = "yes" ] && [ $ng -eq 2 ]; then
  gridstr01=$(echo ${out_gridnames} | cut -d, -f 1)
  gridstr02=$(echo ${out_gridnames} | cut -d, -f 2)
  gridstr12="merged"
  trkd01_grb2file=${out_prefix}.${RUN}.${gridstr01}.trk.f${FHR3}.grb2
  trkd02_grb2file=${out_prefix}.${RUN}.${gridstr02}.trk.f${FHR3}.grb2
  trkd12_grb2file=${out_prefix}.${RUN}.${gridstr12}.trk.f${FHR3}.grb2
  opts='-set_grib_type c2 -new_grid_winds grid -new_grid_vectors "UGRD:VGRD" -new_grid_interpolation neighbor'
  rm -f cmdfile_regrid
 #echo ${WGRIB2} ${intercom}/${trkd01_grb2file} ${opts} -new_grid ${trakgridspecs} ${trkd01_grb2file}.hires >  cmdfile_regrid
  echo ${WGRIB2} ${intercom}/${trkd01_grb2file} -match '"'${PARMlistp1}'"' ${opts} -new_grid ${trakgridspecs} ${trkd01_grb2file}.hires_p1 >  cmdfile_regrid
  echo ${WGRIB2} ${intercom}/${trkd01_grb2file} -match '"'${PARMlistp2}'"' ${opts} -new_grid ${trakgridspecs} ${trkd01_grb2file}.hires_p2 >> cmdfile_regrid
  echo ${WGRIB2} ${intercom}/${trkd01_grb2file} -match '"'${PARMlistp3}'"' ${opts} -new_grid ${trakgridspecs} ${trkd01_grb2file}.hires_p3 >> cmdfile_regrid
 #echo ${WGRIB2} ${trkd02_grb2file} ${opts} -new_grid ${trakgridspecs} ${trkd02_grb2file}.hires             >> cmdfile_regrid
  echo ${WGRIB2} ${trkd02_grb2file} -match '"'${PARMlistp1}'"' ${opts} -new_grid ${trakgridspecs} ${trkd02_grb2file}.hires_p1             >> cmdfile_regrid
  echo ${WGRIB2} ${trkd02_grb2file} -match '"'${PARMlistp2}'"' ${opts} -new_grid ${trakgridspecs} ${trkd02_grb2file}.hires_p2             >> cmdfile_regrid
  echo ${WGRIB2} ${trkd02_grb2file} -match '"'${PARMlistp3}'"' ${opts} -new_grid ${trakgridspecs} ${trkd02_grb2file}.hires_p3             >> cmdfile_regrid
  chmod +x cmdfile_regrid
  ${APRUNC} ${MPISERIAL} -m cmdfile_regrid
  export err=$?; err_chk
  rm -f cmdfile_merge
 #${WGRIB2} ${trkd02_grb2file}.hires -rpn sto_1 -import_grib ${trkd01_grb2file}.hires -rpn "rcl_1:merge" -grib_out ${trkd12_grb2file}
  echo ${WGRIB2} ${trkd02_grb2file}.hires_p1 -rpn sto_1 -import_grib ${trkd01_grb2file}.hires_p1 -rpn "rcl_1:merge" -grib_out ${trkd12_grb2file}_p1 >  cmdfile_merge
  echo ${WGRIB2} ${trkd02_grb2file}.hires_p2 -rpn sto_1 -import_grib ${trkd01_grb2file}.hires_p2 -rpn "rcl_1:merge" -grib_out ${trkd12_grb2file}_p2 >> cmdfile_merge
  echo ${WGRIB2} ${trkd02_grb2file}.hires_p3 -rpn sto_1 -import_grib ${trkd01_grb2file}.hires_p3 -rpn "rcl_1:merge" -grib_out ${trkd12_grb2file}_p3 >> cmdfile_merge
  chmod +x cmdfile_merge
  ${APRUNC} ${MPISERIAL} -m cmdfile_merge
  export err=$?; err_chk
  cat ${trkd12_grb2file}_p1 ${trkd12_grb2file}_p2 ${trkd12_grb2file}_p3 > ${trkd12_grb2file}
  mv ${trkd12_grb2file} ${trkd02_grb2file}
fi

# Generate the index file for the tracker
${GRB2INDEX} ${trk_grb2file} ${trk_grb2indx}
export err=$?; err_chk

# Deliver to intercom
mv ${trk_grb2file} ${intercom}/
mv ${trk_grb2indx} ${intercom}/

# Deliver to COMOUTpost
if [ $SENDCOM = YES ]; then
  mv ${grb2file} ${COMOUTpost}/
  if [ "${SENDDBN^^}" = "YES" ] && [ ${COMOUTpost} = ${COMhafs} ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN^^}_GB2 $job ${COMOUTpost}/${grb2file}
  fi
  mv ${grb2indx} ${COMOUTpost}/
  if [ "${SENDDBN^^}" = "YES" ] && [ ${COMOUTpost} = ${COMhafs} ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN^^}_GB2_WIDX $job ${COMOUTpost}/${grb2indx}
  fi
  if [ ${nhcpost} = .true. ]; then
    mv ${nhc_grb2file} ${COMOUTpost}/
    mv ${nhc_grb2indx} ${COMOUTpost}/
  fi
  if [ ${satpost} = .true. ]; then
    mv ${sat_grb2file} ${COMOUTpost}/
    if [ "${SENDDBN^^}" = "YES" ] && [ ${COMOUTpost} = ${COMhafs} ]; then
      $DBNROOT/bin/dbn_alert MODEL ${RUN^^}_GB2 $job ${COMOUTpost}/${sat_grb2file}
    fi
    mv ${sat_grb2indx} ${COMOUTpost}/
    if [ "${SENDDBN^^}" = "YES" ] && [ ${COMOUTpost} = ${COMhafs} ]; then
      $DBNROOT/bin/dbn_alert MODEL ${RUN^^}_GB2_WIDX $job ${COMOUTpost}/${sat_grb2indx}
    fi
  fi
fi

if [ ${gtype} = regional ]; then

grid_spec=grid_spec${nesttilestr}.nc
atmos_static=atmos_static${nesttilestr}.nc
oro_data=oro_data${nesttilestr}.nc
oro_data_ls=oro_data_ls${nesttilestr}.nc
oro_data_ss=oro_data_ss${nesttilestr}.nc

# Pass over the grid_spec.nc, atmos_static.nc, oro_data.nc if not yet exist
if [ -s ${INPdir}/${grid_spec} ] && [ ${INPdir}/${grid_spec} -nt ${INPdir}/RESTART/${grid_spec} ]; then
  ${NCP} -pL ${INPdir}/${grid_spec} ${INPdir}/RESTART/
fi
if [ ! -z "${RESTARTcom}" ] && [ $SENDCOM = YES ] && \
   [ -s ${INPdir}/RESTART/${grid_spec} ] && [ ${INPdir}/RESTART/${grid_spec} -nt ${RESTARTcom}/${grid_spec} ]; then
  ${FCP} ${INPdir}/RESTART/${grid_spec} ${RESTARTcom}/
fi
if [ -s ${INPdir}/${atmos_static} ] && [ ${INPdir}/${atmos_static} -nt ${INPdir}/RESTART/${atmos_static} ]; then
  ${NCP} -pL ${INPdir}/${atmos_static} ${INPdir}/RESTART/
fi
if [ ! -z "${RESTARTcom}" ] && [ $SENDCOM = YES ] && \
   [ -s ${INPdir}/RESTART/${atmos_static} ] && [ ${INPdir}/RESTART/${atmos_static} -nt ${RESTARTcom}/${atmos_static} ]; then
  ${FCP} ${INPdir}/RESTART/${atmos_static} ${RESTARTcom}/
fi
if [ -s ${INPdir}/INPUT/${oro_data} ] && [ ${INPdir}/INPUT/${oro_data} -nt ${INPdir}/RESTART/${oro_data} ]; then
  ${NCP} -pL ${INPdir}/INPUT/${oro_data} ${INPdir}/RESTART/
fi
if [ ! -z "${RESTARTcom}" ] && [ $SENDCOM = YES ] && \
   [ -s ${INPdir}/RESTART/${oro_data} ] && [ ${INPdir}/RESTART/${oro_data} -nt ${RESTARTcom}/${oro_data} ]; then
  ${FCP} ${INPdir}/RESTART/${oro_data} ${RESTARTcom}/
fi
if [ -s ${INPdir}/INPUT/${oro_data_ls} ] && [ ${INPdir}/INPUT/${oro_data_ls} -nt ${INPdir}/RESTART/${oro_data_ls} ]; then
  ${NCP} -pL ${INPdir}/INPUT/${oro_data_ls} ${INPdir}/RESTART/
fi
if [ ! -z "${RESTARTcom}" ] && [ $SENDCOM = YES ] && \
   [ -s ${INPdir}/RESTART/${oro_data_ls} ] && [ ${INPdir}/RESTART/${oro_data_ls} -nt ${RESTARTcom}/${oro_data_ls} ]; then
  ${FCP} ${INPdir}/RESTART/${oro_data_ls} ${RESTARTcom}/
fi
if [ -s ${INPdir}/INPUT/${oro_data_ss} ] && [ ${INPdir}/INPUT/${oro_data_ss} -nt ${INPdir}/RESTART/${oro_data_ss} ]; then
  ${NCP} -pL ${INPdir}/INPUT/${oro_data_ss} ${INPdir}/RESTART/
fi
if [ ! -z "${RESTARTcom}" ] && [ $SENDCOM = YES ] && \
   [ -s ${INPdir}/RESTART/${oro_data_ss} ] && [ ${INPdir}/RESTART/${oro_data_ss} -nt ${RESTARTcom}/${oro_data_ss} ]; then
  ${FCP} ${INPdir}/RESTART/${oro_data_ss} ${RESTARTcom}/
fi

# grid_mspec files at the current and prior forecast hours
OLDDATE=$(${NDATE} -${NOUTHRS} $NEWDATE)
YYYYold=$(echo $OLDDATE | cut -c1-4)
MMold=$(echo $OLDDATE | cut -c5-6)
DDold=$(echo $OLDDATE | cut -c7-8)
HHold=$(echo $OLDDATE | cut -c9-10)
if [[ -z "$neststr" ]] && [[ $tilestr = ".tile1" ]]; then
  grid_mspec=grid_mspec${neststr}_${YYYY}_${MM}_${DD}_${HH}.nc
  grid_mspec_old=grid_mspec${neststr}_${YYYYold}_${MMold}_${DDold}_${HHold}.nc
else
  grid_mspec=grid_mspec${neststr}_${YYYY}_${MM}_${DD}_${HH}${tilestr}.nc
  grid_mspec_old=grid_mspec${neststr}_${YYYYold}_${MMold}_${DDold}_${HHold}${tilestr}.nc
fi
# Deliver grid_mspec_old at forecast hours 3, 6, 9
if [ $FHR -le 12 ] && [ -s ${INPdir}/${grid_mspec_old} ]; then
  while [ $(( $(date +%s) - $(stat -c %Y ${INPdir}/${grid_mspec_old}) )) -lt 30 ]; do sleep 10s; done
  if [ ${INPdir}/${grid_mspec_old} -nt ${INPdir}/RESTART/${grid_mspec_old} ]; then
    ${NCP} -pL ${INPdir}/${grid_mspec_old} ${INPdir}/RESTART/
  fi
  if [ ! -z "${RESTARTcom}" ] && [ $SENDCOM = YES ] && \
     [ -s ${INPdir}/RESTART/${grid_mspec_old} ] && \
     [ ${INPdir}/RESTART/${grid_mspec_old} -nt ${RESTARTcom}/${grid_mspec_old} ]; then
    ${FCP} ${INPdir}/RESTART/${grid_mspec_old} ${RESTARTcom}/
  fi
fi
# Deliver grid_mspec at NHRS if NHRS less than 12
if [ $FHR -lt 12 ] && [ $FHR -eq $NHRS ] && [ -s ${INPdir}/${grid_mspec} ]; then
  while [ $(( $(date +%s) - $(stat -c %Y ${INPdir}/${grid_mspec}) )) -lt 30 ]; do sleep 10s; done
  if [ ${INPdir}/${grid_mspec} -nt ${INPdir}/RESTART/${grid_mspec} ]; then
    ${NCP} -pL ${INPdir}/${grid_mspec} ${INPdir}/RESTART/
  fi
  if [ ! -z "${RESTARTcom}" ] && [ $SENDCOM = YES ] && \
     [ -s ${INPdir}/RESTART/${grid_mspec} ] && \
     [ ${INPdir}/RESTART/${grid_mspec} -nt ${RESTARTcom}/${grid_mspec} ]; then
    ${FCP} ${INPdir}/RESTART/${grid_mspec} ${RESTARTcom}/
  fi
fi

# Deliver restart files for forecast hours 3, 6, 9
fv_core=${YYYY}${MM}${DD}.${HH}0000.fv_core.res${neststr}.nc
fv_core_tile=${YYYY}${MM}${DD}.${HH}0000.fv_core.res${neststr}${tilestr}.nc
fv_tracer_tile=${YYYY}${MM}${DD}.${HH}0000.fv_tracer.res${neststr}${tilestr}.nc
fv_srf_wnd_tile=${YYYY}${MM}${DD}.${HH}0000.fv_srf_wnd.res${neststr}${tilestr}.nc
sfc_data=${YYYY}${MM}${DD}.${HH}0000.sfc_data${nesttilestr}.nc
phy_data=${YYYY}${MM}${DD}.${HH}0000.phy_data${nesttilestr}.nc
coupler_res=${YYYY}${MM}${DD}.${HH}0000.coupler.res
if [ ! -z "${RESTARTcom}" ] && [ $SENDCOM = YES ] && [ $FHR -lt 12 ] && [ -s ${INPdir}/RESTART/${coupler_res} ]; then
  while [ $(( $(date +%s) - $(stat -c %Y ${INPdir}/RESTART/${coupler_res}) )) -lt 30 ]; do sleep 10s; done
  for file_res in $fv_core $fv_core_tile $fv_tracer_tile $fv_srf_wnd_tile $sfc_data $phy_data $coupler_res ; do
    if [ -s ${INPdir}/RESTART/${file_res} ] && [ ${INPdir}/RESTART/${file_res} -nt ${RESTARTcom}/${file_res} ]; then
      ${FCP} ${INPdir}/RESTART/${file_res} ${RESTARTcom}/${file_res}
    fi
  done
fi

# Deliver WW3 restart file if needed and exists
if [ ${run_wave:-no} = yes ]; then
  ww3_restart=${YYYY}${MM}${DD}.${HH}0000.restart.ww3
  ww3_restart_f006=${out_prefix}.${RUN}.ww3.restart.f006
  if [ ! -z "${RESTARTcom}" ] && [ $SENDCOM = YES ] && [ $FHR -eq 6 ] && [ -s ${INPdir}/${ww3_restart} ]; then
    while [ $(( $(date +%s) - $(stat -c %Y ${INPdir}/${ww3_restart}) )) -lt 30 ]; do sleep 10s; done
    if [ ${INPdir}/${ww3_restart} -nt ${COMOUTpost}/${ww3_restart_f006} ]; then
      ${FCP} ${INPdir}/${ww3_restart} ${COMOUTpost}/${ww3_restart_f006}
    fi
  fi
fi

# Deliver hafs.trak.patcf at NHRS if needed and exists
if [ $FHR -eq $NHRS ] && [ -s ${INPdir}/${fort_patcf} ]; then
  if [ -s ${INPdir}/${fort_patcf}_save ]; then
    cat ${INPdir}/${fort_patcf} >> ${INPdir}/${fort_patcf}_save
    ${FCP} ${INPdir}/${fort_patcf}_save ${COMOUTpost}/${trk_patcf}
  else
    ${FCP} ${INPdir}/${fort_patcf} ${COMOUTpost}/${trk_patcf}
  fi
fi

fi #if [ ${gtype} = regional ]; then

# Write out the postdone message file
echo 'done' > ${intercom}/post${nestdotstr}f${FHR3}

cd ${DATA}

fi
# End if for checking if post has processed this forecast hour previously

done
# End loop for grids/domains

IFHR=$(($IFHR + 1))
FHR=$(($FHR + $FHRI))
FHR2=$(printf "%02d" "$FHR")
FHR3=$(printf "%03d" "$FHR")

done
# End loop for forecast hours

cd ${DATA}

