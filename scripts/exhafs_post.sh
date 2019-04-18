#!/bin/sh

set -x

TOTAL_TASKS=${TOTAL_TASKS:-144}
NCTSK=${NCTSK:-24}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}
export MP_LABELIO=yes

CDATE=${CDATE:-${YMDH}}
NHRS=${NHRS:-126}
NOUTHRS=${NOUTHRS:-3}

POSTEXEC=${POSTEXEC:-${EXEChafs}/exec/hafs_post.x}
GRB2INDEX=${GRB2INDEX:-grb2index}
WGRIB2=${WGRIB2:-wgrib2}
NDATE=${NDATE:-ndate}

synop_gridspecs=${synop_gridspecs:-"latlon 246.6:4112:0.025 -2.4:1976:0.025"}
trker_gridspecs=${trker_gridspecs:-"latlon 246.6:4112:0.025 -2.4:1976:0.025"}
out_prefix=${out_prefix:-$(echo "${STORM}${STORMID}.${YMDH}" | tr '[A-Z]' '[a-z]')}

WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
COMhafs=${COMhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/com/${CDATE}/${STORMID}}
intercom=${intercom:-${WORKhafs}/intercom}
SENDCOM=${SENDCOM:-YES}

DATA=${DATA:-${WORKhafs}/runpost}

# Force to process from the very first output time level
rm -f ${INPdir}/postdonef???

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

fmin=`echo ${FHR}*60 | bc `
minstr=$( printf "%5.5d" "$fmin" )

synop_grb2post=hafsprs.${CDATE}.f${FHR3}.grb2
synop_grb2file=${out_prefix}.hafsprs.synoptic.0p025.f${FHR3}.grb2
synop_grb2indx=${out_prefix}.hafsprs.synoptic.0p025.f${FHR3}.grb2.idx
hafstrk_grb2file=hafs.trak.storm.${CDATE}.f${minstr}
hafstrk_grb2indx=hafs.trak.storm.${CDATE}.f${minstr}.ix

# Check if post has processed this forecast hour previously
if [ -s ${INPdir}/postdonef${FHR3} ] && [ -s ${COMhafs}/${synop_grb2file} ] && [ -s ${COMhafs}/${synop_grb2indx} ] ; then

echo "postdone message ${INPdir}/postdonef${FHR3} exists"
echo "product ${COMhafs}/${synop_grb2file} exists"
echo "product ${COMhafs}/${synop_grb2indx} exists"
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

cat > itag <<EOF
$INPdir/dynf${FHR3}.nc
netcdf
grib2
${YYYY}-${MM}-${DD}_${HH}:00:00
FV3R
$INPdir/phyf${FHR3}.nc

 &NAMPGB
 KPO=47,PO=1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1.,
 /
EOF

rm -f fort.*
# Copy flat files
cp ${PARMhafs}/post/nam_micro_lookup.dat    ./eta_micro_lookup.dat
cp ${PARMhafs}/post/postxconfig-NT-hafs.txt ./postxconfig-NT.txt
cp ${PARMhafs}/post/params_grib2_tbl_new    ./params_grib2_tbl_new

# Run the post
#cp ${POSTEXEC} ./post.x
ln -sf ${POSTEXEC} ./post.x
${APRUNC} ./post.x < itag > outpost_${NEWDATE}

mv HURPRS.GrbF${FHR2} ${synop_grb2post} 

# Convert from rotate lat-lon grib2 to regular lat-lon grib2
${WGRIB2} ${synop_grb2post} -set_bitmap 1 -set_grib_type c3 -new_grid_winds grid -new_grid_vectors "UGRD:VGRD" -new_grid_interpolation neighbor -new_grid ${synop_gridspecs} ${synop_grb2file}
# Generate the grib2 index file
${WGRIB2} -s ${synop_grb2file} > ${synop_grb2indx}

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

# Extract hafstrk grib2 files for the tracker
${WGRIB2} ${synop_grb2file} -s | egrep '(:UGRD:500 mb|:VGRD:500 mb|:HGT:700 mb:|:UGRD:700 mb:|:VGRD:700 mb:|:ABSV:700 mb:|:HGT:850 mb:|:UGRD:850 mb:|:VGRD:850 mb:|:ABSV:850 mb:|:MSLET:|:UGRD:10 m above|:VGRD:10 m above)' | ${WGRIB2} -i ${synop_grb2file} -grib ${hafstrk_grb2file}
# Generate the grib2 index file
${GRB2INDEX} ${hafstrk_grb2file} ${hafstrk_grb2indx}

# Deliver to intercom
mv ${hafstrk_grb2file} ${intercom}/
mv ${hafstrk_grb2indx} ${intercom}/

# Write out the postdone message file
echo 'done' > ${INPdir}/postdonef${FHR3}

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
