#!/bin/sh

set -xe

TOTAL_TASKS=${TOTAL_TASKS:-1}
NCTSK=${NCTSK:-1}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}

CDATE=${CDATE:-${YMDH}}
NHRS=${NHRS:-126}
NOUTHRS=${NOUTHRS:-3}

GETTRKEXEC=${GETTRKEXEC:-${EXEChafs}/hafs_gettrk.x}
TAVEEXEC=${GETTRKEXEC:-${EXEChafs}/hafs_tave.x}
VINTEXEC=${VINTEXEC:-${EXEChafs}/hafs_vint.x}
SUPVITEXEC=${SUPVITEXEC:-${EXEChafs}/hafs_supvit.x}

MPISERIAL=${MPISERIAL:-mpiserial}
NDATE=${NDATE:-ndate}
WGRIB2=${WGRIB2:-wgrib2}
GRB2INDEX=${GRB2INDEX:-grb2index}

WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
INPdir=${INPdir:-${WORKhafs}/intercom/post}
DATA=${DATA:-${WORKhafs}/product}
COMhafs=${COMhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/com/${CDATE}/${STORMID}}

out_prefix=${out_prefix:-$(echo "${STORM}${STORMID}.${YMDH}" | tr '[A-Z]' '[a-z]')}
trk_atcfunix=${out_prefix}.trak.hafs.atcfunix
all_atcfunix=${out_prefix}.trak.hafs.atcfunix.all

tmp_vital=${WORKhafs}/tmpvit
old_vital=${WORKhafs}/oldvit

#===============================================================================
# Run GFDL vortextracker  
# *** Currently, the tave step is skipped (same as HMON). Need add it and make it the same as HWRF.
# *** Need add the capability of TC genesis tracking.

DATA_tracker=${DATA}/tracker
mkdir -p ${DATA_tracker}

cd ${DATA_tracker}

# Compute domain for tracker
gmodname=hafs
rundescr=trak
atcfdescr=storm

# Link the track files and generate the input.fcst_minutes file
if [ -s input.fcst_minutes ] ; then
  rm -f input.fcst_minutes
fi
IFHR=0
FHR=0
FHR3=$( printf "%03d" "$FHR" )
while [ $FHR -le $NHRS ]
do
  echo "FHR3="${FHR3}   
  FMIN=$(( ${FHR} * 60 )) 
  minstr=$( printf "%5.5d" "$FMIN" )
  hafstrk_grb2file=${gmodname}.${rundescr}.${atcfdescr}.${CDATE}.f${minstr}
  hafstrk_grb2indx=${gmodname}.${rundescr}.${atcfdescr}.${CDATE}.f${minstr}.ix
  ln -sf ${INPdir}/${hafstrk_grb2file} .
  ln -sf ${INPdir}/${hafstrk_grb2indx} .
  IFHR=`expr $IFHR + 1`
  LINE=$( printf "%4d %5d" "$IFHR" "$FMIN" )
  echo "$LINE" >> input.fcst_minutes
  FHR=`expr $FHR + $NOUTHRS`
  FHR3=$( printf "%03d" "$FHR" )
done

rm -f fort.*

# Find and sort active storms for this cycle from known tcvitals file
# This can potentially provide multiple tcvital messages to the tracker, 
# so that it can track multiple storms simultaneously.
# *** Currently, tcutil_multistorm_sort.py searches the tcvitals files
# specified in the script. Need to modify it to be able to deal with storm
# message files/dirs, as well as passing in tcvitals files.
${USHhafs}/tcutil_multistorm_sort.py ${CDATE} | cut -c1-96 > allvit

# Prepare the input/output files
cat ${tmp_vital} allvit > input.vitals
#cat ${tmp_vital} > input.vitals

cp input.vitals tcvit_rsmc_storms.txt
ln -sf input.vitals       fort.12
touch fort.14
ln -sf input.fcst_minutes fort.15
ln -sf output.all         fort.61
ln -sf output.atcf        fort.62
ln -sf output.radii       fort.63
ln -sf output.atcfunix    fort.64
ln -sf output.initvitl    fort.65
ln -sf output.atcf_gen    fort.66
ln -sf output.genvitals   fort.67
ln -sf output.atcf_sink   fort.68
ln -sf output.atcf_hfip   fort.69
ln -sf output.cps_parms   fort.71
ln -sf output.structure   fort.72
ln -sf output.fractwind   fort.73
ln -sf output.ike         fort.74
ln -sf output.pdfwind     fort.76

# The product atcf track file
touch ${COMhafs}/${all_atcfunix}
ln -sf ${COMhafs}/${all_atcfunix} output.atcfunix

# Prepare the input namelist
CC=`echo $CDATE | cut -c 1-2`
YY=`echo $CDATE | cut -c 3-4`
MM=`echo $CDATE | cut -c 5-6`
DD=`echo $CDATE | cut -c 7-8`
HH=`echo $CDATE | cut -c 9-10`

cp ${PARMhafs}/product/namelist.gettrk_tmp ./
cat namelist.gettrk_tmp | sed s/_BCC_/${CC}/ | \
                          sed s/_BYY_/${YY}/ | \
                          sed s/_BMM_/${MM}/ | \
                          sed s/_BDD_/${DD}/ | \
                          sed s/_BHH_/${HH}/ | \
                          sed s/_YMDH_/${CDATE}/ > namelist.gettrk

# Run the vortex tracker gettrk.x
cp -p ${GETTRKEXEC} ./hafs_gettrk.x
#ln -sf ${GETTRKEXEC} ./hafs_gettrk.x
${APRUNC} ./hafs_gettrk.x < namelist.gettrk

# Extract the tracking records for tmpvit
STORMNUM=$(echo ${STORMID} | cut -c1-2)
STORMBS1=$(echo ${STORMID} | cut -c3)
cp ${COMhafs}/${all_atcfunix} ${COMhafs}/${all_atcfunix}.orig
if [ $STORMNUM == "00" ] ; then
norig=`cat ${COMhafs}/${all_atcfunix}.orig |wc -l `
if [ $norig -eq 1 ] ; then
> ${COMhafs}/${all_atcfunix}
else
grep -v "^.., ${STORMNUM}," ${COMhafs}/${all_atcfunix}.orig >  ${COMhafs}/${all_atcfunix}
fi
else
grep "^.., ${STORMNUM}," ${COMhafs}/${all_atcfunix} | grep -E "^${STORMBS1}.,|^.${STORMBS1}," > ${COMhafs}/${trk_atcfunix}
fi

# Deliver track file to NOSCRUB:
mkdir -p ${CDNOSCRUB}/${SUBEXPT}
cp -p ${COMhafs}/${all_atcfunix}.orig ${CDNOSCRUB}/${SUBEXPT}/.
if [ -s ${COMhafs}/${all_atcfunix} ] ; then 
cp -p ${COMhafs}/${all_atcfunix} ${CDNOSCRUB}/${SUBEXPT}/.
fi
if [ -s ${COMhafs}/${trk_atcfunix} ] && [ $STORMNUM != "00" ] ; then 
cp -p ${COMhafs}/${trk_atcfunix} ${CDNOSCRUB}/${SUBEXPT}/.
fi
#===============================================================================

cd ${DATA}

echo "product job done"

exit
