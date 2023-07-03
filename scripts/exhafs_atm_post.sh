#!/bin/sh

set -xe

if [ ${ENSDA} = YES ]; then
# Ensemble member with ENSID <= ${ENS_FCST_SIZE} will run the full-length NHRS forecast
  if [ $((10#${ENSID})) -le ${ENS_FCST_SIZE:-10} ]; then
    NHRS=${NHRS:-126}
  else
    NHRS=${NHRS_ENS:-6}
  fi
  export NBDYHRS=${NBDYHRS_ENS:-3}
  export NOUTHRS=${NOUTHRS_ENS:-3}
  export CASE=${CASE_ENS:-C768}
  export CRES=$(echo $CASE | cut -c 2-)
  export gtype=${gtype_ens:-regional}
  export LEVS=${LEVS_ENS:-65}
  export post_gridspecs=${post_gridspecs_ens:-""}
  export trak_gridspecs=${trak_gridspecs_ens:-""}
  export satpost=${satpost_ens:-".false."}
else
  export NHRS=${NHRS:-126}
  export NBDYHRS=${NBDYHRS:-3}
  export NOUTHRS=${NOUTHRS:-3}
  export CASE=${CASE:-C768}
  export CRES=$(echo $CASE | cut -c 2-)
  export gtype=${gtype:-regional}
  export LEVS=${LEVS:-65}
  export post_gridspecs=${post_gridspecs:-""}
  export trak_gridspecs=${trak_gridspecs:-""}
  export satpost=${satpost:-".false."}
fi

export MP_LABELIO=yes

CDATE=${CDATE:-${YMDH}}
NHRS=${NHRS:-126}
NOUTHRS=${NOUTHRS:-3}

NCP=${NCP:-'/bin/cp'}
NLN=${NLN:-'/bin/ln -sf'}
NDATE=${NDATE:-ndate}
WGRIB2=${WGRIB2:-wgrib2}
GRB2INDEX=${GRB2INDEX:-grb2index}
MPISERIAL=${MPISERIAL:-${EXEChafs}/hafs_mpiserial.x}
POSTEXEC=${POSTEXEC:-${EXEChafs}/hafs_post.x}

FIXcrtm=${FIXcrtm:-${CRTM_FIX:?}}
intercom=${intercom:-${WORKhafs}/intercom/post}
COMOUTpost=${COMOUTpost:-${COMhafs}}
SENDCOM=${SENDCOM:-YES}

out_prefix=${out_prefix:-$(echo "${STORMID,,}.${CDATE}")}
output_grid=${output_grid:-rotated_latlon}
post_gridspecs=${post_gridspecs:-""}
trak_gridspecs=${trak_gridspecs:-""}

DATA=${DATA:-${WORKhafs}/atm_post}

IFHR=0
FHR=0
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
trk_grb2file=${out_prefix}.${RUN}.${gridstr}.trk.f${FHR3}.grb2
trk_grb2indx=${out_prefix}.${RUN}.${gridstr}.trk.f${FHR3}.grb2.ix

fort_patcf="fort.6$(printf '%02d' ${ng})"
trk_patcf=${out_prefix}.${RUN}.trak.patcf

# Check if post has processed this forecast hour previously
if [ -s ${INPdir}/post${nestdotstr}f${FHR3} ] && \
   [ ${INPdir}/post${nestdotstr}f${FHR3} -nt ${INPdir}/logf${FHR3} ] && \
   [ -s ${COMOUTpost}/${grb2file} ] && \
   [ -s ${COMOUTpost}/${grb2indx} ]; then

echo "post done file ${INPdir}/post${nestdotstr}f${FHR3} exist and newer than ${INPdir}/logf${FHR3}"
echo "product ${COMOUTpost}/${grb2file} exist"
echo "product ${COMOUTpost}/${grb2indx} exist"
echo "skip post for forecast hour ${FHR3} valid at ${NEWDATE}"

# Otherwise run post for this forecast hour
else

if [ ${write_dopost:-.false.} = .true. ]; then

# Wait for model output
n=1
while [ $n -le 600 ]; do
  if [ ! -s ${INPdir}/logf${FHR3} ] || [ ! -s ${INPdir}/HURPRS${neststr}.GrbF${FHR2} ]; then
    echo "${INPdir}/logf${FHR3} not ready, sleep 60"
    sleep 60s
  else
    echo "${INPdir}/logf${FHR3}, ${INPdir}/HURPRS${neststr}.GrbF${FHR2} ready, continue"
    sleep 3s
    break
  fi
  if [ $n -ge 600 ]; then
    echo "ERROR: waited too many times: $n. exitting"
    exit 1
  fi
  n=$((n+1))
done

else

# Wait for model output
n=1
while [ $n -le 600 ]; do
  if [ ! -s ${INPdir}/logf${FHR3} ] || \
     [ ! -s ${INPdir}/atm${nestdotstr}f${FHR3}.nc ] || \
     [ ! -s ${INPdir}/sfc${nestdotstr}f${FHR3}.nc ]; then
    echo "${INPdir}/logf${FHR3} not ready, sleep 60"
    sleep 60s
  else
    echo "${INPdir}/logf${FHR3}, ${INPdir}/atm${nestdotstr}f${FHR3}.nc ${INPdir}/sfc${nestdotstr}f${FHR3}.nc ready, do post"
    sleep 3s
    break
  fi
  if [ $n -ge 600 ]; then
    echo "ERROR: waited too many times: $n. exitting"
    exit 1
  fi
  n=$((n+1))
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

rm -f fort.*
# Copy fix files
${NCP} ${PARMhafs}/post/nam_micro_lookup.dat ./eta_micro_lookup.dat
${NCP} ${PARMhafs}/post/params_grib2_tbl_new ./params_grib2_tbl_new

if [ ${satpost} = .true. ]; then
# ${NCP} ${PARMhafs}/post/postxconfig-NT-hafs_sat.txt ./postxconfig-NT.txt
  ${NCP} ${PARMhafs}/post/postxconfig-NT-hafs.txt ./postxconfig-NT.txt
  # Link crtm fix files
  for file in "amsre_aqua" "imgr_g11" "imgr_g12" "imgr_g13" \
    "imgr_g15" "imgr_mt1r" "imgr_mt2" "seviri_m10" \
    "ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16" \
    "ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" \
    "tmi_trmm" "v.seviri_m10" "imgr_insat3d" "abi_gr" "ahi_himawari8" \
    "abi_g16" "abi_g17" ; do
    ${NLN} ${FIXcrtm}/${file}.TauCoeff.bin ./
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
set -o pipefail
${APRUNC} ./hafs_post.x < itag 2>&1 | tee ./outpost_${NEWDATE}
set +o pipefail

mv HURPRS.GrbF${FHR2} ${grb2post}
if [ ${satpost} = .true. ]; then
  mv HURSAT.GrbF${FHR2} ${sat_grb2post}
fi

fi #if [ ${write_dopost:-.false.} = .true. ]

if [[ "$postgridspecs" == auto && "$outputgrid" == lambert_conformal ]] ; then
  clon=$(echo ${output_grid_cen_lon} | cut -d , -f ${ng})
  clat=$(echo ${output_grid_cen_lat} | cut -d , -f ${ng})
  lon_span=$(echo ${output_grid_lon_span} | cut -d , -f ${ng})
  lat_span=$(echo ${output_grid_lat_span} | cut -d , -f ${ng})
  outputgrid_nx=$(echo ${output_grid_nx} | cut -d , -f ${ng})
  outputgrid_ny=$(echo ${output_grid_ny} | cut -d , -f ${ng})
  outputgrid_dx=$(echo ${output_grid_dx} | cut -d , -f ${ng})
  outputgrid_dy=$(echo ${output_grid_dy} | cut -d , -f ${ng})
  outputgrid_stdlat1=$(echo ${output_grid_stdlat1} | cut -d , -f ${ng})
  outputgrid_stdlat2=$(echo ${output_grid_stdlat2} | cut -d , -f ${ng})
  outputgrid_lon0=$(printf "%.6f" $(bc <<< "scale=6; ${clon}-${lon_span}/2.0"))
  outputgrid_lat0=$(printf "%.6f" $(bc <<< "scale=6; ${clat}-${lat_span}/2.0"))

  postgridspecs="lambert:$clon:$outputgrid_stdlat1:$outputgrid_stdlat2 $outputgrid_lon0:$outputgrid_nx:$outputgrid_dx $outputgrid_lat0:$outputgrid_ny:$outputgrid_dy"

  unset clon clat lon_span lat_span outputgrid_nx outputgrid_ny
  unset outputgrid_dx outputgrid_dy outputgrid_stdlat1 outputgrid_stdlat2
  unset outputgrid_lon0 outputgrid_lat0
elif [ ${postgridspecs} = auto ]; then
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
  unset clon clat lon_span lat_span latlon_dlon latlon_dlat latlon_lon0
  unset latlon_nlon latlon_lat0 latlon_nlat
fi # if [[ "$postgridspecs" == auto && "$outputgrid" == lambert_conformal ]]

if [[ "$trakgridspecs" == auto && "$outputgrid" == lambert_conformal ]] ; then
  clon=$(echo ${output_grid_cen_lon} | cut -d , -f ${ng})
  clat=$(echo ${output_grid_cen_lat} | cut -d , -f ${ng})
  lon_span=$(echo ${trak_grid_lon_span} | cut -d , -f ${ng})
  lat_span=$(echo ${trak_grid_lat_span} | cut -d , -f ${ng})
  latlon_dlon=$(printf "%.6f" $(echo ${output_grid_dlon} | cut -d , -f ${ng}))
  latlon_dlat=$(printf "%.6f" $(echo ${output_grid_dlat} | cut -d , -f ${ng}))
  latlon_lon0=$(printf "%.6f" $(bc <<< "scale=6; ${clon}-${lon_span}/2.0"))
  latlon_nlon=$(printf "%.0f" $(bc <<< "scale=6; ${lon_span}/${latlon_dlon}"))
  latlon_lat0=$(printf "%.6f" $(bc <<< "scale=6; ${clat}-${lat_span}/2.0"))
  latlon_nlat=$(printf "%.0f" $(bc <<< "scale=6; ${lat_span}/${latlon_dlat}"))
  trakgridspecs="latlon ${latlon_lon0}:${latlon_nlon}:${latlon_dlon} ${latlon_lat0}:${latlon_nlat}:${latlon_dlat}"
  unset clon clat lon_span lat_span latlon_dlon latlon_dlat latlon_lon0
  unset latlon_nlon latlon_lat0 latlon_nlat
elif [ ${trakgridspecs} = auto ]; then
  trakgridspecs=${postgridspecs}
fi # if [[ "$trakgridspecs" == auto && "$outputgrid" == lambert_conformal ]]

if [[ "$outputgrid" = "rotated_latlon"* || "$outputgrid" == lambert_conformal ]]; then

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
if [ ${machine} = "wcoss2" ]; then
  ncmd=$(cat ./cmdfile | wc -l)
  ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
  $APRUNCFP -n $ncmd_max cfp ./cmdfile
else
  ${APRUNC} ${MPISERIAL} -m cmdfile
fi
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

echo "ERROR: output grid: ${outputgrid} not supported exitting"
exit 1

fi #if [[ "$outputgrid" = "rotated_latlon"* ]]; then

# Generate the grib2 index file
${WGRIB2} -s ${grb2file} > ${grb2indx}
if [ ${satpost} = .true. ]; then
  ${WGRIB2} -s ${sat_grb2file} > ${sat_grb2indx}
fi

# Extract hafstrk grib2 files for the tracker
PARMlistp1="UGRD:850|UGRD:700|UGRD:500|VGRD:850|VGRD:700|VGRD:500|UGRD:10 m a|VGRD:10 m a|ABSV:850|ABSV:700|MSLET"
PARMlistp2="HGT:900|HGT:850|HGT:800|HGT:750|HGT:700|HGT:650|HGT:600|HGT:550|HGT:500|HGT:450|HGT:400"
PARMlistp3="HGT:350|HGT:300|HGT:250|HGT:200|TMP:500|TMP:450|TMP:400|TMP:350|TMP:300|TMP:250|TMP:200"
PARMlist=${PARMlistp1}"|"${PARMlistp2}"|"${PARMlistp3}
echo ${PARMlist}

${WGRIB2} ${grb2file} -match "${PARMlist}" -grib ${trk_grb2file}

# Create the combined grid01 and grid02 hafstrk grib2 file and use it to replace the grid02 hafstrk grib2 file
if [ $ng -eq 2 ]; then
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
  if [ ${machine} = "wcoss2" ]; then
    ncmd=$(cat ./cmdfile_regrid | wc -l)
    ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
    $APRUNCFP -n $ncmd_max cfp ./cmdfile_regrid
  else
    ${APRUNC} ${MPISERIAL} -m cmdfile_regrid
  fi
  rm -f cmdfile_merge
 #${WGRIB2} ${trkd02_grb2file}.hires -rpn sto_1 -import_grib ${trkd01_grb2file}.hires -rpn "rcl_1:merge" -grib_out ${trkd12_grb2file}
  echo ${WGRIB2} ${trkd02_grb2file}.hires_p1 -rpn sto_1 -import_grib ${trkd01_grb2file}.hires_p1 -rpn "rcl_1:merge" -grib_out ${trkd12_grb2file}_p1 >  cmdfile_merge
  echo ${WGRIB2} ${trkd02_grb2file}.hires_p2 -rpn sto_1 -import_grib ${trkd01_grb2file}.hires_p2 -rpn "rcl_1:merge" -grib_out ${trkd12_grb2file}_p2 >> cmdfile_merge
  echo ${WGRIB2} ${trkd02_grb2file}.hires_p3 -rpn sto_1 -import_grib ${trkd01_grb2file}.hires_p3 -rpn "rcl_1:merge" -grib_out ${trkd12_grb2file}_p3 >> cmdfile_merge
  chmod +x cmdfile_merge
  if [ ${machine} = "wcoss2" ]; then
    ncmd=$(cat ./cmdfile_merge | wc -l)
    ncmd_max=$((ncmd < TOTAL_TASKS ? ncmd : TOTAL_TASKS))
    $APRUNCFP -n $ncmd_max cfp ./cmdfile_merge
  else
    ${APRUNC} ${MPISERIAL} -m cmdfile_merge
  fi
  cat ${trkd12_grb2file}_p1 ${trkd12_grb2file}_p2 ${trkd12_grb2file}_p3 > ${trkd12_grb2file}
  mv ${trkd12_grb2file} ${trkd02_grb2file}
fi

# Generate the index file for the tracker
${GRB2INDEX} ${trk_grb2file} ${trk_grb2indx}

# Deliver to intercom
mkdir -p ${intercom}
mv ${trk_grb2file} ${intercom}/
mv ${trk_grb2indx} ${intercom}/

# Deliver to COMOUTpost
if [ $SENDCOM = YES ]; then
  mkdir -p ${COMOUTpost}
  mv ${grb2file} ${COMOUTpost}/
  mv ${grb2indx} ${COMOUTpost}/
  if [ ${satpost} = .true. ]; then
    mv ${sat_grb2file} ${COMOUTpost}/
    mv ${sat_grb2indx} ${COMOUTpost}/
  fi
fi

if [ ${gtype} = regional ]; then

grid_spec=grid_spec${nesttilestr}.nc
atmos_static=atmos_static${nesttilestr}.nc
if [[ -z "$neststr" ]] && [[ $tilestr = ".tile1" ]]; then
  grid_mspec=grid_mspec${neststr}_${YYYY}_${MM}_${DD}_${HH}.nc
  atmos_diag=atmos_diag${neststr}_${YYYY}_${MM}_${DD}_${HH}.nc
else
  grid_mspec=grid_mspec${neststr}_${YYYY}_${MM}_${DD}_${HH}${tilestr}.nc
  atmos_diag=atmos_diag${neststr}_${YYYY}_${MM}_${DD}_${HH}${tilestr}.nc
fi
fv_core=${YYYY}${MM}${DD}.${HH}0000.fv_core.res${neststr}${tilestr}.nc
fv_tracer=${YYYY}${MM}${DD}.${HH}0000.fv_tracer.res${neststr}${tilestr}.nc
fv_srf_wnd=${YYYY}${MM}${DD}.${HH}0000.fv_srf_wnd.res${neststr}${tilestr}.nc
sfc_data=${YYYY}${MM}${DD}.${HH}0000.sfc_data${nesttilestr}.nc
phy_data=${YYYY}${MM}${DD}.${HH}0000.phy_data${nesttilestr}.nc

# Pass over the grid_spec.nc, atmos_static.nc, oro_data.nc if not yet exist
if [ -s ${INPdir}/${grid_spec} ] && [ ! -s ${INPdir}/RESTART/${grid_spec} ]; then
  ${NCP} -p ${INPdir}/${grid_spec} ${INPdir}/RESTART/
fi
if [ -s ${INPdir}/${atmos_static} ] && [ ! -s ${INPdir}/RESTART/${atmos_static} ]; then
  ${NCP} -p ${INPdir}/${atmos_static} ${INPdir}/RESTART/
fi
oro_data=oro_data${nesttilestr}.nc
if [ -s ${INPdir}/INPUT/${oro_data} ] && [ ! -s ${INPdir}/RESTART/${oro_data} ]; then
  ${NCP} -pL ${INPdir}/INPUT/${oro_data} ${INPdir}/RESTART/
fi
oro_data_ls=oro_data_ls${nesttilestr}.nc
if [ -s ${INPdir}/INPUT/${oro_data_ls} ] && [ ! -s ${INPdir}/RESTART/${oro_data_ls} ]; then
  ${NCP} -pL ${INPdir}/INPUT/${oro_data_ls} ${INPdir}/RESTART/
fi
oro_data_ss=oro_data_ss${nesttilestr}.nc
if [ -s ${INPdir}/INPUT/${oro_data_ss} ] && [ ! -s ${INPdir}/RESTART/${oro_data_ss} ]; then
  ${NCP} -pL ${INPdir}/INPUT/${oro_data_ss} ${INPdir}/RESTART/
fi

if [[ "${is_moving_nest:-.false.}" = *".true."* ]] || [[ "${is_moving_nest:-.false.}" = *".T."* ]]; then
  # Pass over the grid_mspec files for moving nest (useful for storm cycling)
  if [ $FHR -lt 12 ] && [ -s ${INPdir}/${grid_mspec} ]; then
    while [ $(( $(date +%s) - $(stat -c %Y ${INPdir}/${grid_mspec}) )) -lt 30  ]; do sleep 10; done
    if [ ! -L ${INPdir}/${grid_mspec} ]; then
      mv ${INPdir}/${grid_mspec} ${INPdir}/RESTART/${grid_mspec}
      ${NLN} ${INPdir}/RESTART/${grid_mspec} ${INPdir}/${grid_mspec}
    fi
  fi
  # Deliver hafs.trak.patcf if exists
  if [ $FHR -eq $NHRS ] && [ -s ${INPdir}/${fort_patcf} ]; then
    ${NCP} -p ${INPdir}/${fort_patcf} ${COMOUTpost}/${trk_patcf}
  fi
fi

fi #if [ ${gtype} = regional ]; then

# Write out the postdone message file
echo 'done' > ${INPdir}/post${nestdotstr}f${FHR3}

cd ${DATA}

fi
# End if for checking if post has processed this forecast hour previously

done
# End loop for grids/domains

IFHR=$(($IFHR + 1))
FHR=$(($FHR + $NOUTHRS))
FHR2=$(printf "%02d" "$FHR")
FHR3=$(printf "%03d" "$FHR")

done
# End loop for forecast hours

cd ${DATA}

#===============================================================================
# Produce swath grib2 products for peak gust, precipitation rate from
# accumulated total precipitation, peak 10 m wind, convective precipitation
# rate from accumulated convective precipitation, peak updraft, peak downdraft,
# updraft helicity in the lower troposphere (2000-5000 m), updraft helicity
# near the surface (0-3000 m).
# From Lew.Gramer@noaa.gov, 2023-01-19

# Currently swath grib2 products are based on the parent domain grib2 output only
if [ ${swathpost:-.true.} = .true. ] && [ ${COMOUTpost} = ${COMhafs} ] ; then

DATA_SWATH=${DATA}/swath
rm -rf ${DATA_SWATH}
mkdir -p ${DATA_SWATH}
cd ${DATA_SWATH}

# currently only deal with parent domain)
gridstr=$(echo ${out_gridnames} | cut -d, -f 1)

swath_grb2file=${out_prefix}.${RUN}.${gridstr}.swath.grb2
swath_grb2indx=${out_prefix}.${RUN}.${gridstr}.swath.grb2.idx
rm -f ${swath_grb2file} ${swath_grb2indx}
# temporary grib2 files
GUSTF="./gust.grb2"
APCPF="./apcp.grb2"
PRATEF="./prate.grb2"
WINDMAXF="./windmax.grb2"
ACPCPF="./acpcp.grb2"
CPRATF="./cprat.grb2"
MAXUVVF="./maxuvv.grb2"
MAXDVVF="./maxdvv.grb2"
UPHLSFCF="./uphlsfc.grb2"
UPHLTROF="./uphltro.grb2"
TMPFILE="./tmpfile.grb2"
TMPFILE2="./tmpfile2.grb2"
rm -f ${GUSTF} ${APCPF} ${PRATEF} ${WINDMAXF} ${ACPCPF} ${CPRATF} ${MAXUVVF} ${MAXDVVF} ${UPHLSFCF} ${UPHLTROF}

# Deal with forecast F000
DHR=$NOUTHRS
IFHR=0
FHR=0
FHR3=$(printf "%03d" "$FHR")
grb2file=${out_prefix}.${RUN}.${gridstr}.atm.f${FHR3}.grb2
swath_grb2fhhh=${out_prefix}.${RUN}.${gridstr}.swath.f${FHR3}.grb2

# Replace UNKNOWN values in GRB2 messages of interest with 0; convert PCP accs to PRATEs
${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:GUST:)'           	-rpn "0:swap:merge" \
    -set_metadata_str "0:0:d=+0hr:GUST:10-10 m above ground:0-$((IFHR*DHR)) hour max fcst::Wind Speed (Gust) [m/s]:" \
    -grib_out ${GUSTF}
${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:APCP:)'           	-rpn "0:swap:merge" -grib_out ${APCPF}
${WGRIB2} ${APCPF} -match '(:APCP:)'            	-rpn "0:*" \
    -set_metadata_str "0:0:d=+0hr:PRATE:atmos col:0-$((IFHR*DHR)) hour ave fcst::Precipitation Rate [kg/m^2/s]:" \
    -grib_out ${PRATEF}
${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:WIND.*max)'       	-rpn "0:swap:merge" \
    -set_metadata_str "0:0:d=+0hr:WIND:10-10 m above ground:0-$((IFHR*DHR)) hour max fcst::Wind Speed [m/s]:" \
    -grib_out ${WINDMAXF}
${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:ACPCP:)'          	-rpn "0:swap:merge" -grib_out ${ACPCPF}
${WGRIB2} ${ACPCPF} -match '(:ACPCP:)'            	-rpn "0:*" \
    -set_metadata_str "0:0:d=+0hr:CPRAT:atmos col:0-$((IFHR*DHR)) hour ave fcst::Convective Precipitation Rate [kg/m^2/s]:" \
    -grib_out ${CPRATF}
${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:MAXUVV:)'       	-rpn "0:swap:merge" \
    -set_metadata_str "0:0:d=+0hr:DZDT:0-400 mb above ground:0-$((IFHR*DHR)) hour max fcst::Vertical Velocity (Geometric) [m/s]:" \
    -grib_out ${MAXUVVF}
${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:MAXDVV:)'       	-rpn "0:swap:merge" \
    -set_metadata_str "0:0:d=+0hr:DZDT:0-400 mb above ground:0-$((IFHR*DHR)) hour min fcst::Vertical Velocity (Geometric) [m/s]:" \
    -grib_out ${MAXDVVF}
${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:MXUPHL:5000-2000 m)' 	-rpn "0:swap:merge" \
    -set_metadata_str "0:0:d=+0hr:UPHL:5000-2000 m above ground:0-0 hour max fcst::Updraft Helicity [m^2/s^2]:" \
    -grib_out ${UPHLTROF}
${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:MXUPHL:3000-0 m)'   	-rpn "0:swap:merge" \
    -set_metadata_str "0:0:d=+0hr:UPHL:3000-0 m above ground:0-0 hour max fcst::Updraft Helicity [m^2/s^2]:" \
    -grib_out ${UPHLSFCF}

#cat ${GUSTF} ${PRATEF} ${WINDMAXF} ${CPRATF} ${MAXUVVF} ${MAXDVVF} ${UPHLTROF} ${UPHLSFCF} >> ${swath_grb2fhhh}

# Loop for other forecast hours
IFHR=$(($IFHR + 1))
FHR=$(($FHR + $NOUTHRS))
FHR3=$(printf "%03d" "$FHR")

while [ $FHR -le $NHRS ]; do
  grb2file=${out_prefix}.${RUN}.${gridstr}.atm.f${FHR3}.grb2
  swath_grb2fhhh=${out_prefix}.${RUN}.${gridstr}.swath.f${FHR3}.grb2
  # MAXIMIZE, MINIMIZE, or ACCUMULATE current 3-hourly values with
  # persistent intermediate swath files (e.g., GUSTF) for each variable.
  # GUST - peak wind gust
  # Replace UNKNOWN values with 0, change variable name, metadata
  ${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:GUST:)'         	-rpn "0:swap:merge" \
      -set_metadata_str "0:0:d=+0hr:GUST:10-10 m above ground:0-$((IFHR*DHR)) hour max fcst::Wind Speed (Gust) [m/s]:" \
      -grib_out ${TMPFILE}
  # Max with previous swath intermediate file
  ${WGRIB2} ${TMPFILE} -rpn "sto_1" -import_grib ${GUSTF} -rpn "rcl_1:max" -grib_out ${TMPFILE2}
  rm ${TMPFILE}
  mv ${TMPFILE2} ${GUSTF}
  # PRATE - accumulated total precipitation
  ${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:APCP:)'         	-rpn "0:swap:merge" -grib_out ${TMPFILE}
  ${WGRIB2} ${TMPFILE} -rpn "sto_1" -import_grib ${APCPF} -rpn "rcl_1:+" -grib_out ${TMPFILE2}
  rm ${TMPFILE}
  mv ${TMPFILE2} ${APCPF}
  ${WGRIB2} ${APCPF} -match '(:APCP:)'            	-rpn "$((IFHR*DHR*3600)):/" \
      -set_metadata_str "0:0:d=+0hr:PRATE:atmos col:0-$((IFHR*DHR)) hour ave fcst::Precipitation Rate [kg/m^2/s]:" \
      -grib_out ${PRATEF}
  # WINDmax - peak 10 m wind
  ${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:WIND.*max)'     	-rpn "0:swap:merge" \
      -set_metadata_str "0:0:d=+0hr:WIND:10-10 m above ground:0-$((IFHR*DHR)) hour max fcst::Wind Speed [m/s]:" \
      -grib_out ${TMPFILE}
  ${WGRIB2} ${TMPFILE} -rpn "sto_1" -import_grib ${WINDMAXF} -rpn "rcl_1:max" -grib_out ${TMPFILE2}
  rm ${TMPFILE}
  mv ${TMPFILE2} ${WINDMAXF}
  # CPRAT - accumulated convective precipitation
  ${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:ACPCP:)'        	-rpn "0:swap:merge" -grib_out ${TMPFILE}
  ${WGRIB2} ${TMPFILE} -rpn "sto_1" -import_grib ${ACPCPF} -rpn "rcl_1:+" -grib_out ${TMPFILE2}
  rm ${TMPFILE}
  mv ${TMPFILE2} ${ACPCPF}
  ${WGRIB2} ${ACPCPF} -match '(:ACPCP:)'            	-rpn "$((IFHR*DHR*3600)):/" \
      -set_metadata_str "0:0:d=+0hr:CPRAT:atmos col:0-$((IFHR*DHR)) hour ave fcst::Convective Precipitation Rate [kg/m^2/s]:" \
      -grib_out ${CPRATF}
  # DZDTmax - peak updrafts
  ${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:MAXUVV:)'       	-rpn "0:swap:merge" \
      -set_metadata_str "0:0:d=+0hr:DZDT:0-400 mb above ground:0-$((IFHR*DHR)) hour max fcst::Vertical Velocity (Geometric) [m/s]:" \
      -grib_out ${TMPFILE}
  ${WGRIB2} ${TMPFILE} -rpn "sto_1" -import_grib ${MAXUVVF} -rpn "rcl_1:max" -grib_out ${TMPFILE2}
  rm ${TMPFILE}
  mv ${TMPFILE2} ${MAXUVVF}
  # DZDTmin - peak downdrafts
  ${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:MAXDVV:)'       	-rpn "0:swap:merge" \
      -set_metadata_str "0:0:d=+0hr:DZDT:0-400 mb above ground:0-$((IFHR*DHR)) hour min fcst::Vertical Velocity (Geometric) [m/s]:" \
      -grib_out ${TMPFILE}
  ${WGRIB2} ${TMPFILE} -rpn "sto_1" -import_grib ${MAXDVVF} -rpn "rcl_1:min" -grib_out ${TMPFILE2}
  rm ${TMPFILE}
  mv ${TMPFILE2} ${MAXDVVF}
  # UPHLtro - updraft helicity in the lower troposphere
  ${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:MXUPHL:5000-2000 m)' 	-rpn "0:swap:merge" \
      -set_metadata_str "0:0:d=+0hr:UPHL:5000-2000 m above ground:$(((IFHR-1)*DHR))-$((IFHR*DHR)) hour max fcst::Updraft Helicity [m^2/s^2]:" \
      -grib_out ${TMPFILE}
  mv ${TMPFILE} ${UPHLTROF}
  # UPHLsfc - updraft helicity in the surface layer
  ${WGRIB2} ${COMOUTpost}/${grb2file} -match '(:MXUPHL:3000-0 m)'   	-rpn "0:swap:merge" \
      -set_metadata_str "0:0:d=+0hr:UPHL:3000-0 m above ground:$(((IFHR-1)*DHR))-$((IFHR*DHR)) hour max fcst::Updraft Helicity [m^2/s^2]:" \
      -grib_out ${TMPFILE}
  mv ${TMPFILE} ${UPHLSFCF}
 #cat ${GUSTF} ${PRATEF} ${WINDMAXF} ${CPRATF} ${MAXUVVF} ${MAXDVVF} ${UPHLTROF} ${UPHLSFCF} > ${swath_grb2fhhh}
  cat ${GUSTF} ${PRATEF} ${WINDMAXF} ${CPRATF} ${MAXUVVF} ${MAXDVVF} ${UPHLTROF} ${UPHLSFCF} >> ${swath_grb2file}

  # next forecast hour
  IFHR=$(($IFHR + 1))
  FHR=$(($FHR + $NOUTHRS))
  FHR3=$(printf "%03d" "$FHR")
done

# Compress swath grib2 file to save disk space
${WGRIB2} ${swath_grb2file} -set_grib_type c2 -grib_out ${swath_grb2file}.c2
mv ${swath_grb2file}.c2 ${swath_grb2file}
# Generate the index file for the swath grib2 file
${GRB2INDEX} ${swath_grb2file} ${swath_grb2indx}

# Deliver to COMOUTpost
if [ $SENDCOM = YES ]; then
  mkdir -p ${COMOUTpost}
  mv ${swath_grb2file} ${COMOUTpost}/
  mv ${swath_grb2indx} ${COMOUTpost}/
fi

fi

#===============================================================================

cd ${DATA}

echo "post job done"

exit
