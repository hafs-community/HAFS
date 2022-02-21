#!/bin/sh

set -xe

NCP=${NCP:-'/bin/cp'}
NLN=${NLN:-'/bin/ln -sf'}
NDATE=${NDATE:-ndate}

if [ ${ENSDA} = YES ]; then
  export NHRS=${NHRS_ENS:-126}
  export NBDYHRS=${NBDYHRS_ENS:-3}
  export NOUTHRS=${NOUTHRS_ENS:-3}
  export CASE=${CASE_ENS:-C768}
  export CRES=`echo $CASE | cut -c 2-`
  export gtype=${gtype_ens:-regional}
  export LEVS=${LEVS_ENS:-65}
  export synop_gridspecs=${synop_gridspecs_ens:-"latlon 246.6:4112:0.025 -2.4:1976:0.025"}
  export trker_gridspecs=${trker_gridspecs_ens:-"latlon 246.6:4112:0.025 -2.4:1976:0.025"}
else
  export NHRS=${NHRS:-126}
  export NBDYHRS=${NBDYHRS:-3}
  export NOUTHRS=${NOUTHRS:-3}
  export CASE=${CASE:-C768}
  export CRES=`echo $CASE | cut -c 2-`
  export gtype=${gtype:-regional}
  export LEVS=${LEVS:-65}
  export synop_gridspecs=${synop_gridspecs:-"latlon 246.6:4112:0.025 -2.4:1976:0.025"}
  export trker_gridspecs=${trker_gridspecs:-"latlon 246.6:4112:0.025 -2.4:1976:0.025"}
fi

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

POSTEXEC=${POSTEXEC:-${EXEChafs}/hafs_post.x}
MPISERIAL=${MPISERIAL:-${EXEChafs}/hafs_mpiserial.x}
WGRIB2=${WGRIB2:-wgrib2}
GRB2INDEX=${GRB2INDEX:-grb2index}

WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
COMhafs=${COMhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/com/${CDATE}/${STORMID}}
FIXcrtm=${FIXcrtm:-${FIXhafs}/hafs-crtm-2.3.0}
intercom=${intercom:-${WORKhafs}/intercom/post}
SENDCOM=${SENDCOM:-YES}

COMOUTpost=${COMOUTpost:-${COMhafs}}

satpost=${satpost:-.false.}
output_grid=${output_grid:-rotated_latlon}
synop_gridspecs=${synop_gridspecs:-"latlon 246.6:4112:0.025 -2.4:1976:0.025"}
trker_gridspecs=${trker_gridspecs:-"latlon 246.6:4112:0.025 -2.4:1976:0.025"}
out_prefix=${out_prefix:-$(echo "${STORM}${STORMID}.${CDATE}" | tr '[A-Z]' '[a-z]')}

DATA=${DATA:-${WORKhafs}/atm_post}

IFHR=0
FHR=0
FHR2=$( printf "%02d" "$FHR" )
FHR3=$( printf "%03d" "$FHR" )

# Loop for forecast hours
while [ $FHR -le $NHRS ];
do

cd ${DATA}

NEWDATE=`${NDATE} +${FHR} $CDATE`
YYYY=`echo $NEWDATE | cut -c1-4`
MM=`echo $NEWDATE | cut -c5-6`
DD=`echo $NEWDATE | cut -c7-8`
HH=`echo $NEWDATE | cut -c9-10`

FMIN=$(( ${FHR}*60 )) 
minstr=$( printf "%5.5d" "$FMIN" )

synop_grb2post=hafsprs.${CDATE}.f${FHR3}.grb2
synop_grb2file=${out_prefix}.hafsprs.synoptic.0p03.f${FHR3}.grb2
synop_grb2indx=${out_prefix}.hafsprs.synoptic.0p03.f${FHR3}.grb2.idx
synop_sat_grb2post=hafssat.${CDATE}.f${FHR3}.grb2
synop_sat_grb2file=${out_prefix}.hafssat.synoptic.0p03.f${FHR3}.grb2
synop_sat_grb2indx=${out_prefix}.hafssat.synoptic.0p03.f${FHR3}.grb2.idx
gmodname=hafs
rundescr=trak
atcfdescr=storm
hafstrk_grb2file=${gmodname}.${rundescr}.${atcfdescr}.${CDATE}.f${minstr}
hafstrk_grb2indx=${gmodname}.${rundescr}.${atcfdescr}.${CDATE}.f${minstr}.ix

# Check if post has processed this forecast hour previously
if [ -s ${INPdir}/postf${FHR3} ] && \
   [ -s ${COMOUTpost}/${synop_grb2file} ] && \
   [ -s ${COMOUTpost}/${synop_grb2indx} ] ; then

echo "post message ${INPdir}/postf${FHR3} exist"
echo "product ${COMOUTpost}/${synop_grb2file} exist"
echo "product ${COMOUTpost}/${synop_grb2indx} exist"
echo "skip post for forecast hour ${FHR3} valid at ${NEWDATE}"

# Otherwise run post for this forecast hour
else

if [ ${write_dopost:-.false.} = .true. ]; then

# Wait for model output
n=1
while [ $n -le 600 ]
do
  if [ ! -s ${INPdir}/logf${FHR3} ] || [ ! -s ${INPdir}/HURPRS.GrbF${FHR2} ]; then
    echo "${INPdir}/logf${FHR3} not ready, sleep 60"
    sleep 60s
  else
    echo "${INPdir}/logf${FHR3}, ${INPdir}/HURPRS.GrbF${FHR2} ready, continue"
    sleep 3s
    break
  fi
  if [ $n -ge 600 ]; then
    echo "ERROR: waited too many times: $n. exitting"
    exit 1
  fi
  n=$(( n+1 ))
done

else

# Wait for model output
n=1
while [ $n -le 600 ]
do
  if [ ! -s ${INPdir}/logf${FHR3} ] || [ ! -s ${INPdir}/atmf${FHR3}.nc ] || [ ! -s ${INPdir}/sfcf${FHR3}.nc ]; then
    echo "${INPdir}/logf${FHR3} not ready, sleep 60"
    sleep 60s
  else
    echo "${INPdir}/logf${FHR3}, ${INPdir}/atmf${FHR3}.nc ${INPdir}/sfcf${FHR3}.nc ready, do post"
    sleep 3s
    break
  fi
  if [ $n -ge 600 ]; then
    echo "ERROR: waited too many times: $n. exitting"
    exit 1
  fi
  n=$(( n+1 ))
done

fi #if [ ${write_dopost:-.false.} = .true. ]

# Create the post working dir for the time level
DATA_POST=${DATA}/post_${NEWDATE}
rm -rf ${DATA_POST}
mkdir -p ${DATA_POST}
cd ${DATA_POST}

if [ ${write_dopost:-.false.} = .true. ]; then

${NCP} -p ${INPdir}/HURPRS.GrbF${FHR2} ${synop_grb2post}
if [ ${satpost} = .true. ]; then
${NCP} -p ${INPdir}/HURSAT.GrbF${FHR2} ${synop_sat_grb2post}
fi

else

# Preparte itag namelist input file
cat>itag<<EOF
&model_inputs
fileName='${INPdir}/atmf${FHR3}.nc'
IOFORM=netcdf
grib='grib2'
DateStr='${YYYY}-${MM}-${DD}_${HH}:00:00'
MODELNAME='FV3R'
fileNameFlux='${INPdir}/sfcf${FHR3}.nc'
/
&NAMPGB
KPO=47,PO=1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1.,
/
EOF

rm -f fort.*
# Copy fix files
${NCP} ${PARMhafs}/post/nam_micro_lookup.dat    ./eta_micro_lookup.dat
${NCP} ${PARMhafs}/post/params_grib2_tbl_new    ./params_grib2_tbl_new

if [ ${satpost} = .true. ]; then
  ${NCP} ${PARMhafs}/post/postxconfig-NT-hafs.txt ./postxconfig-NT.txt
  # Link crtm fix files
  for file in "amsre_aqua" "imgr_g11" "imgr_g12" "imgr_g13" \
    "imgr_g15" "imgr_mt1r" "imgr_mt2" "seviri_m10" \
    "ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16" \
    "ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" \
    "tmi_trmm" "v.seviri_m10" "imgr_insat3d" "abi_gr" "ahi_himawari8" ; do
    ${NLN} ${FIXcrtm}/fix-4-hafs/${file}.TauCoeff.bin ./
    ${NLN} ${FIXcrtm}/fix-4-hafs/${file}.SpcCoeff.bin ./
  done
  for file in "Aerosol" "Cloud" ; do
    ${NLN} ${FIXcrtm}/fix-4-hafs/${file}Coeff.bin ./
  done
  for file in ${FIXcrtm}/fix-4-hafs/*Emis* ; do
    ${NLN} ${file} ./
  done
else
  ${NCP} ${PARMhafs}/post/postxconfig-NT-hafs_nosat.txt ./postxconfig-NT.txt
fi

# Run the post
${NCP} -p  ${POSTEXEC} ./hafs_post.x
#ln -sf ${POSTEXEC} ./hafs_post.x
${APRUNC} ./hafs_post.x < itag > outpost_${NEWDATE}

mv HURPRS.GrbF${FHR2} ${synop_grb2post}
if [ ${satpost} = .true. ]; then
  mv HURSAT.GrbF${FHR2} ${synop_sat_grb2post}
fi

fi #if [ ${write_dopost:-.false.} = .true. ]

if [ "$output_grid" = rotated_latlon ]; then

# For rotated_latlon output grid
# Convert from rotate lat-lon grib2 to regular lat-lon grib2
#${WGRIB2} ${synop_grb2post} -set_bitmap 1 -set_grib_type c3 -new_grid_winds grid -new_grid_vectors "UGRD:VGRD" -new_grid_interpolation neighbor -new_grid ${synop_gridspecs} ${synop_grb2file}
# Parallelize this section to speed up wgrib2
#opts='-set_bitmap 1 -set_grib_type c3 -new_grid_winds grid -new_grid_vectors "UGRD:VGRD" -new_grid_interpolation neighbor'
opts='-set_grib_type c2 -new_grid_winds grid -new_grid_vectors "UGRD:VGRD" -new_grid_interpolation neighbor'
rm -f cmdfile
echo ${WGRIB2} ${synop_grb2post} -match '":2 mb:|:5 mb:|:7 mb:|:10 mb:|:20 mb:"' ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part00 >  cmdfile
echo ${WGRIB2} ${synop_grb2post} -match '":30 mb:|:50 mb:|:70 mb:|:100 mb:"'     ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part01 >> cmdfile
echo ${WGRIB2} ${synop_grb2post} -match '":125 mb:|:150 mb:|:175 mb:|:200 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part02 >> cmdfile
echo ${WGRIB2} ${synop_grb2post} -match '":225 mb:|:250 mb:|:275 mb:|:300 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part03 >> cmdfile
echo ${WGRIB2} ${synop_grb2post} -match '":325 mb:|:350 mb:|:375 mb:|:400 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part04 >> cmdfile
echo ${WGRIB2} ${synop_grb2post} -match '":425 mb:|:450 mb:|:475 mb:|:500 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part05 >> cmdfile
echo ${WGRIB2} ${synop_grb2post} -match '":525 mb:|:550 mb:|:575 mb:|:600 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part06 >> cmdfile
echo ${WGRIB2} ${synop_grb2post} -match '":625 mb:|:650 mb:|:675 mb:|:700 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part07 >> cmdfile
echo ${WGRIB2} ${synop_grb2post} -match '":725 mb:|:750 mb:|:775 mb:|:800 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part08 >> cmdfile
echo ${WGRIB2} ${synop_grb2post} -match '":825 mb:|:850 mb:|:875 mb:|:900 mb:"'  ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part09 >> cmdfile
echo ${WGRIB2} ${synop_grb2post} -match '":925 mb:|:950 mb:|:975 mb:|:1000 mb:"' ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part10 >> cmdfile
echo ${WGRIB2} ${synop_grb2post} -not '" mb:"'                                   ${opts} -new_grid ${synop_gridspecs} ${synop_grb2post}.part11 >> cmdfile
if [ ${satpost} = .true. ]; then
echo ${WGRIB2} ${synop_sat_grb2post}                                             ${opts} -new_grid ${synop_gridspecs} ${synop_sat_grb2file} >> cmdfile
fi
chmod +x cmdfile
${APRUNC} ${MPISERIAL} -m cmdfile
# Cat the temporary files together
cat ${synop_grb2post}.part?? > ${synop_grb2file}
# clean up the temporary files
rm -f ${synop_grb2post}.part??

elif [ "$output_grid" = regional_latlon ]; then

# For regional_latlon output grid, no need to convert
mv ${synop_grb2post} ${synop_grb2file}
if [ ${satpost} = .true. ]; then
mv ${synop_sat_grb2post} ${synop_sat_grb2file}
fi

## Alternatively, can use wgrib2 to convert from c3 to c2 packing, which can reduce the filesize by ~30%.
#opts='-set_grib_type c2 -grib_out'

else

echo "ERROR: output grid: ${output_grid} not supported exitting"
exit 1

fi

# Generate the grib2 index file
${WGRIB2} -s ${synop_grb2file} > ${synop_grb2indx}
if [ ${satpost} = .true. ]; then
${WGRIB2} -s ${synop_sat_grb2file} > ${synop_sat_grb2indx}
fi

# Extract hafstrk grib2 files for the tracker
# From HMON
#PARMlist=':UGRD:500 mb|:VGRD:500 mb|:HGT:700 mb:|:UGRD:700 mb:|:VGRD:700 mb:|:ABSV:700 mb:|:HGT:850 mb:|:UGRD:850 mb:|:VGRD:850 mb:|:ABSV:850 mb:|:MSLET:|:UGRD:10 m above|:VGRD:10 m above'
# From HWRF
#PARMlist='HGT:925|HGT:850|HGT:700|UGRD:850|UGRD:700|UGRD:500|VGRD:850|VGRD:700|VGRD:500|UGRD:10 m |VGRD:10 m |ABSV:850|ABSV:700|PRMSL|HGT:900|HGT:800|HGT:750|HGT:650|HGT:600|HGT:550|HGT:500|HGT:450|HGT:400|HGT:350|HGT:300|HGT:250|TMP:500|TMP:450|TMP:400|TMP:350|TMP:300|TMP:250'
# From ens_tracker for GFS
#PARMlist='UGRD:850|UGRD:700|UGRD:500|VGRD:850|VGRD:700|VGRD:500|UGRD:10 m a|VGRD:10 m a|ABSV:850|ABSV:700|MSLET|HGT:900|HGT:850|HGT:800|HGT:750|HGT:700|HGT:650|HGT:600|HGT:550|HGT:500|HGT:450|HGT:400|HGT:350|HGT:300|TMP:500|TMP:450|TMP:400|TMP:350|TMP:300'

PARMlist='UGRD:850|UGRD:700|UGRD:500|VGRD:850|VGRD:700|VGRD:500|UGRD:10 m a|VGRD:10 m a|ABSV:850|ABSV:700|MSLET|HGT:900|HGT:850|HGT:800|HGT:750|HGT:700|HGT:650|HGT:600|HGT:550|HGT:500|HGT:450|HGT:400|HGT:350|HGT:300|HGT:250|HGT:200|TMP:500|TMP:450|TMP:400|TMP:350|TMP:300|TMP:250|TMP:200'

#PARMlist='UGRD:850|UGRD:700|UGRD:500|VGRD:850|VGRD:700|VGRD:500|UGRD:10 m a|VGRD:10 m a|ABSV:850|ABSV:700|PRMSL|HGT:900|HGT:850|HGT:800|HGT:750|HGT:700|HGT:650|HGT:600|HGT:550|HGT:500|HGT:450|HGT:400|HGT:350|HGT:300|HGT:250|HGT:200|TMP:500|TMP:450|TMP:400|TMP:350|TMP:300|TMP:250|TMP:200'

${APRUNS} ${WGRIB2} ${synop_grb2file} -match "${PARMlist}" -grib ${hafstrk_grb2file}

# Generate the index file for the tracker
${GRB2INDEX} ${hafstrk_grb2file} ${hafstrk_grb2indx}

# Deliver to COMOUTpost
if [ $SENDCOM = YES ]; then
  mkdir -p ${COMOUTpost}
  mv ${synop_grb2file} ${COMOUTpost}/
  mv ${synop_grb2indx} ${COMOUTpost}/
if [ ${satpost} = .true. ]; then
  mv ${synop_sat_grb2file} ${COMOUTpost}/
  mv ${synop_sat_grb2indx} ${COMOUTpost}/
fi
fi

# Deliver to intercom
mkdir -p ${intercom}
mv ${hafstrk_grb2file} ${intercom}/
mv ${hafstrk_grb2indx} ${intercom}/

# Pass over the grid_mspec files for moving nest (useful for storm cycling)
if [[ "${is_moving_nest:-".false."}" = *".true."* ]] || [[ "${is_moving_nest:-".false."}" = *".T."* ]] ; then
  if [ $FHR -lt 12 ] && [ -s ${INPdir}/grid_mspec_${YYYY}_${MM}_${DD}_${HH}.nc ]; then
    ${NCP} -p ${INPdir}/grid_mspec_${YYYY}_${MM}_${DD}_${HH}.nc ${INPdir}/RESTART/
  fi
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
