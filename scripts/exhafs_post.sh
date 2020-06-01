#!/bin/ksh

set -xe

TOTAL_TASKS=${TOTAL_TASKS:-144}
NCTSK=${NCTSK:-24}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
APRUNS=${APRUNS:-"aprun -b -j1 -n1 -N1 -d1 -cc depth"}
APRUNF=${APRUNF:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth cfp"}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}

export MP_LABELIO=yes

CDATE=${CDATE:-${YMDH}}
NHRS=${NHRS:-126}
NOUTHRS=${NOUTHRS:-3}

POSTEXEC=${POSTEXEC:-${EXEChafs}/exec/hafs_post.x}

MPISERIAL=${MPISERIAL:-mpiserial}
NDATE=${NDATE:-ndate}
WGRIB2=${WGRIB2:-wgrib2}
GRB2INDEX=${GRB2INDEX:-grb2index}

WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
COMhafs=${COMhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/com/${CDATE}/${STORMID}}
intercom=${intercom:-${WORKhafs}/intercom/post}
SENDCOM=${SENDCOM:-YES}

output_grid=${output_grid:-rotated_latlon}
synop_gridspecs=${synop_gridspecs:-"latlon 246.6:4112:0.025 -2.4:1976:0.025"}
trker_gridspecs=${trker_gridspecs:-"latlon 246.6:4112:0.025 -2.4:1976:0.025"}
out_prefix=${out_prefix:-$(echo "${STORM}${STORMID}.${YMDH}" | tr '[A-Z]' '[a-z]')}

DATA=${DATA:-${WORKhafs}/post}

IFHR=0
FHR=0
FHR2=$( printf "%02d" "$FHR" )
FHR3=$( printf "%03d" "$FHR" )

# Loop for forecast hours
while [ $FHR -le $NHRS ];
do

cd ${DATA}

NEWDATE=`${NDATE} +${FHR3} $CDATE`
YYYY=`echo $NEWDATE | cut -c1-4`
MM=`echo $NEWDATE | cut -c5-6`
DD=`echo $NEWDATE | cut -c7-8`
HH=`echo $NEWDATE | cut -c9-10`

FMIN=$(( ${FHR}*60 )) 
minstr=$( printf "%5.5d" "$FMIN" )

synop_grb2post=hafsprs.${CDATE}.f${FHR3}.grb2
synop_grb2file=${out_prefix}.hafsprs.synoptic.0p03.f${FHR3}.grb2
synop_grb2indx=${out_prefix}.hafsprs.synoptic.0p03.f${FHR3}.grb2.idx
gmodname=hafs
rundescr=trak
atcfdescr=storm
hafstrk_grb2file=${gmodname}.${rundescr}.${atcfdescr}.${CDATE}.f${minstr}
hafstrk_grb2indx=${gmodname}.${rundescr}.${atcfdescr}.${CDATE}.f${minstr}.ix

# Check if post has processed this forecast hour previously
if [ -s ${INPdir}/postf${FHR3} ] && [ -s ${COMhafs}/${synop_grb2file} ] && [ -s ${COMhafs}/${synop_grb2indx} ] ; then

echo "post message ${INPdir}/postf${FHR3} exist"
echo "product ${COMhafs}/${synop_grb2file} exist"
echo "product ${COMhafs}/${synop_grb2indx} exist"
echo "skip post for forecast hour ${FHR3} valid at ${NEWDATE}"

# Otherwise run post for this forecast hour
else

# Wait for model output
n=1
while [ $n -le 600 ]
do
  if [ ! -s ${INPdir}/logf${FHR3} ] || [ ! -s ${INPdir}/dynf${FHR3}.nc ] || [ ! -s ${INPdir}/phyf${FHR3}.nc ]; then
    echo "${INPdir}/logf${FHR3} not ready, sleep 60"
    sleep 60s
  else
    echo "${INPdir}/logf${FHR3}, ${INPdir}/dynf${FHR3}.nc ${INPdir}/phyf${FHR3}.nc ready, do post"
    sleep 3s
    break
  fi
  n=$(( n+1 ))
done

# Create the post working dir for the time level
DATA_POST=${DATA}/post_${NEWDATE}
rm -rf ${DATA_POST}
mkdir -p ${DATA_POST}
cd ${DATA_POST}

# In /bin/sh this cat part does not work
# Need to figure it out why. Use ksh for now.
cat>itag<<EOF
${INPdir}/dynf${FHR3}.nc
netcdf
grib2
${YYYY}-${MM}-${DD}_${HH}:00:00
FV3R
${INPdir}/phyf${FHR3}.nc
&NAMPGB
KPO=47,PO=1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1.,
/
EOF

rm -f fort.*
# Copy flat files
cp ${PARMhafs}/post/nam_micro_lookup.dat    ./eta_micro_lookup.dat
# So far the postxconfig-NT.txt is a plain text file, it would be better
# provide the actual xml file for easy configuration, while generating this
# plain text file on the fly.
cp ${PARMhafs}/post/postxconfig-NT-hafs.txt ./postxconfig-NT.txt
cp ${PARMhafs}/post/params_grib2_tbl_new    ./params_grib2_tbl_new

# Run the post
#cp ${POSTEXEC} ./post.x
ln -sf ${POSTEXEC} ./post.x
${APRUNC} ./post.x < itag > outpost_${NEWDATE}

mv HURPRS.GrbF${FHR2} ${synop_grb2post} 

if [ "$output_grid" = rotated_latlon ]; then

# For rotated_latlon output grid
# Convert from rotate lat-lon grib2 to regular lat-lon grib2
#${APRUNS} ${WGRIB2} ${synop_grb2post} -set_bitmap 1 -set_grib_type c3 -new_grid_winds grid -new_grid_vectors "UGRD:VGRD" -new_grid_interpolation neighbor -new_grid ${synop_gridspecs} ${synop_grb2file}
# Parallelize this section to speed up wgrib2 
#opts='-set_bitmap 1 -set_grib_type c3 -new_grid_winds grid -new_grid_vectors "UGRD:VGRD" -new_grid_interpolation neighbor'
opts='-set_grib_type c2 -new_grid_winds grid -new_grid_vectors "UGRD:VGRD" -new_grid_interpolation neighbor'
echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":2 mb:|:5 mb:|:7 mb:|:10 mb:|:20 mb:"' ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part00 ${BACKGROUND} >  cmdfile
echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":30 mb:|:50 mb:|:70 mb:|:100 mb:"'     ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part01 ${BACKGROUND} >> cmdfile
echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":125 mb:|:150 mb:|:175 mb:|:200 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part02 ${BACKGROUND} >> cmdfile
echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":225 mb:|:250 mb:|:275 mb:|:300 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part03 ${BACKGROUND} >> cmdfile
echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":325 mb:|:350 mb:|:375 mb:|:400 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part04 ${BACKGROUND} >> cmdfile
echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":425 mb:|:450 mb:|:475 mb:|:500 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part05 ${BACKGROUND} >> cmdfile
echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":525 mb:|:550 mb:|:575 mb:|:600 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part06 ${BACKGROUND} >> cmdfile
echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":625 mb:|:650 mb:|:675 mb:|:700 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part07 ${BACKGROUND} >> cmdfile
echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":725 mb:|:750 mb:|:775 mb:|:800 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part08 ${BACKGROUND} >> cmdfile
echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":825 mb:|:850 mb:|:875 mb:|:900 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part09 ${BACKGROUND} >> cmdfile
echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":925 mb:|:950 mb:|:975 mb:|:1000 mb:"' ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part10 ${BACKGROUND} >> cmdfile
echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -not '" mb:"'                                   ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part11 ${BACKGROUND} >> cmdfile
if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ]; then
  echo 'wait' >> cmdfile
fi
chmod u+x ./cmdfile
${APRUNF} ./cmdfile
wait

# Cat the temporary files together
cat ${synop_grb2post}.part?? > ${synop_grb2file}
# clean up the temporary files
rm -f ${synop_grb2post}.part??

elif [ "$output_grid" = regional_latlon ]; then

# For regional_latlon output grid, no need to convert
mv ${synop_grb2post} ${synop_grb2file}

## Alternatively, can use wgrib2 to convert from c3 to c2 packing, which can reduce the filesize by ~30%.
#opts='-set_grib_type c2 -grib_out'
#echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":2 mb:|:5 mb:|:7 mb:|:10 mb:|:20 mb:"' ${opts} ${synop_grb2post}.part00 ${BACKGROUND} >  cmdfile
#echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":30 mb:|:50 mb:|:70 mb:|:100 mb:"'     ${opts} ${synop_grb2post}.part01 ${BACKGROUND} >> cmdfile
#echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":125 mb:|:150 mb:|:175 mb:|:200 mb:"'  ${opts} ${synop_grb2post}.part02 ${BACKGROUND} >> cmdfile
#echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":225 mb:|:250 mb:|:275 mb:|:300 mb:"'  ${opts} ${synop_grb2post}.part03 ${BACKGROUND} >> cmdfile
#echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":325 mb:|:350 mb:|:375 mb:|:400 mb:"'  ${opts} ${synop_grb2post}.part04 ${BACKGROUND} >> cmdfile
#echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":425 mb:|:450 mb:|:475 mb:|:500 mb:"'  ${opts} ${synop_grb2post}.part05 ${BACKGROUND} >> cmdfile
#echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":525 mb:|:550 mb:|:575 mb:|:600 mb:"'  ${opts} ${synop_grb2post}.part06 ${BACKGROUND} >> cmdfile
#echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":625 mb:|:650 mb:|:675 mb:|:700 mb:"'  ${opts} ${synop_grb2post}.part07 ${BACKGROUND} >> cmdfile
#echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":725 mb:|:750 mb:|:775 mb:|:800 mb:"'  ${opts} ${synop_grb2post}.part08 ${BACKGROUND} >> cmdfile
#echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":825 mb:|:850 mb:|:875 mb:|:900 mb:"'  ${opts} ${synop_grb2post}.part09 ${BACKGROUND} >> cmdfile
#echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -match '":925 mb:|:950 mb:|:975 mb:|:1000 mb:"' ${opts} ${synop_grb2post}.part10 ${BACKGROUND} >> cmdfile
#echo ${APRUNO} ${WGRIB2} ${synop_grb2post} -not '" mb:"'                                   ${opts} ${synop_grb2post}.part11 ${BACKGROUND} >> cmdfile
#if [ "$machine" = hera ] || [ "$machine" = orion ] || [ "$machine" = jet ]; then
#  echo 'wait' >> cmdfile
#fi
#chmod u+x ./cmdfile
#${APRUNF} ./cmdfile
#wait
#
## Cat the temporary files together
#cat ${synop_grb2post}.part?? > ${synop_grb2file}
## clean up the temporary files
#rm -f ${synop_grb2post}.part??

else

  echo "ERROR: output grid: ${output_grid} not supported exitting"
  exit 1

fi

# Generate the grib2 index file
${WGRIB2} -s ${synop_grb2file} > ${synop_grb2indx}

# Extract hafstrk grib2 files for the tracker
# From HMON
#PARMlist=':UGRD:500 mb|:VGRD:500 mb|:HGT:700 mb:|:UGRD:700 mb:|:VGRD:700 mb:|:ABSV:700 mb:|:HGT:850 mb:|:UGRD:850 mb:|:VGRD:850 mb:|:ABSV:850 mb:|:MSLET:|:UGRD:10 m above|:VGRD:10 m above'
# From HWRF
#PARMlist='HGT:925|HGT:850|HGT:700|UGRD:850|UGRD:700|UGRD:500|VGRD:850|VGRD:700|VGRD:500|UGRD:10 m |VGRD:10 m |ABSV:850|ABSV:700|PRMSL|HGT:900|HGT:800|HGT:750|HGT:650|HGT:600|HGT:550|HGT:500|HGT:450|HGT:400|HGT:350|HGT:300|HGT:250|TMP:500|TMP:450|TMP:400|TMP:350|TMP:300|TMP:250'
# From ens_tracker for GFS
#PARMlist='UGRD:850|UGRD:700|UGRD:500|VGRD:850|VGRD:700|VGRD:500|UGRD:10 m a|VGRD:10 m a|ABSV:850|ABSV:700|MSLET|HGT:900|HGT:850|HGT:800|HGT:750|HGT:700|HGT:650|HGT:600|HGT:550|HGT:500|HGT:450|HGT:400|HGT:350|HGT:300|TMP:500|TMP:450|TMP:400|TMP:350|TMP:300'

#PARMlist='UGRD:850|UGRD:700|UGRD:500|VGRD:850|VGRD:700|VGRD:500|UGRD:10 m a|VGRD:10 m a|ABSV:850|ABSV:700|MSLET|HGT:900|HGT:850|HGT:800|HGT:750|HGT:700|HGT:650|HGT:600|HGT:550|HGT:500|HGT:450|HGT:400|HGT:350|HGT:300|HGT:250|HGT:200|TMP:500|TMP:450|TMP:400|TMP:350|TMP:300|TMP:250|TMP:200'

PARMlist='UGRD:850|UGRD:700|UGRD:500|VGRD:850|VGRD:700|VGRD:500|UGRD:10 m a|VGRD:10 m a|ABSV:850|ABSV:700|PRMSL|HGT:900|HGT:850|HGT:800|HGT:750|HGT:700|HGT:650|HGT:600|HGT:550|HGT:500|HGT:450|HGT:400|HGT:350|HGT:300|HGT:250|HGT:200|TMP:500|TMP:450|TMP:400|TMP:350|TMP:300|TMP:250|TMP:200'

${APRUNS} ${WGRIB2} ${synop_grb2file} -match "${PARMlist}" -grib ${hafstrk_grb2file}

# Generate the index file for the tracker
${GRB2INDEX} ${hafstrk_grb2file} ${hafstrk_grb2indx}

# Deliver to COMhafs
if [ $SENDCOM = YES ]; then
  mv ${synop_grb2file} ${COMhafs}/
  mv ${synop_grb2indx} ${COMhafs}/
fi

# Check if the products are missing
if [ ! -s ${COMhafs}/${synop_grb2file} ]; then
  echo "ERROR: product ${COMhafs}/${synop_grb2file} not exist"
  echo "ERROR: post for hour ${FHR3} valid at ${NEWDATE} exitting"
  exit 1
fi
if [ ! -s ${COMhafs}/${synop_grb2indx} ] ; then
  echo "ERROR: product ${COMhafs}/${synop_grb2indx} not exist"
  echo "ERROR: post for hour ${FHR3} valid at ${NEWDATE} exitting"
  exit 1
fi

# Deliver to intercom
mkdir -p ${intercom}
mv ${hafstrk_grb2file} ${intercom}/
mv ${hafstrk_grb2indx} ${intercom}/

# Check if the products are missing
if [ ! -s ${intercom}/${hafstrk_grb2file} ]; then
  echo "ERROR: intercom product ${intercom}/${hafstrk_grb2file} not exist"
  echo "ERROR: post for hour ${FHR3} valid at ${NEWDATE} exitting"
  exit 1
fi
if [ ! -s ${intercom}/${hafstrk_grb2indx} ] ; then
  echo "ERROR: intercom product ${intercom}/${hafstrk_grb2indx} not exist"
  echo "ERROR: post for hour ${FHR3} valid at ${NEWDATE} exitting"
  exit 1
fi

# Write out the postdone message file
echo 'done' > ${INPdir}/postf${FHR3}

cd ${DATA}

fi
# End if for checking if post has processed this forecast hour previously

IFHR=`expr $IFHR + 1`
FHR=`expr $FHR + $NOUTHRS`
FHR2=$( printf "%02d" "$FHR" )
FHR3=$( printf "%03d" "$FHR" )

done
# End loop for forecast hours

echo "post job done"

exit
