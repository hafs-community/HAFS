#!/bin/sh
################################################################################
# Script Name: exhafs_ocn_post.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the HAFS oceanic post-processing steps for MOM6 coupling.
################################################################################
set -x -o pipefail

CDATE=${CDATE:-${YMDH}}
ENSDA=${ENSDA:-NO}
if [ "${ENSDA}" = YES ]; then
  INPdir=${WORKhafs}/forecast_ens/mem${ENSID}
  COMOUTpost=${COMhafs}/ocn_post_ens/mem${ENSID}
  intercom=${WORKhafs}/intercom/ocn_post_ens/mem${ENSID}
else
  INPdir=${WORKhafs}/forecast
  COMOUTpost=${COMhafs}
  intercom=${WORKhafs}/intercom/ocn_post
fi

if [ ${ENSDA} = YES ]; then
# Ensemble member with ENSID <= ${ENS_FCST_SIZE} will run the full-length NHRS forecast
  if [ $((10#${ENSID})) -le ${ENS_FCST_SIZE:-10} ]; then
    NHRS=${NHRS:-126}
  else
    NHRS=${NHRS_ENS:-6}
  fi
  NOUTHRS=${NOUTHRS_ENS:-3}
else
  NHRS=${NHRS:-126}
  NOUTHRS=${NOUTHRS:-3}
fi

out_prefix=${out_prefix:-$(echo "${STORMID,,}.${CDATE}")}

SENDCOM=${SENDCOM:-YES}
COMOUTpost=${COMOUTpost:-${COMhafs}}
intercom=${intercom:-${WORKhafs}/intercom/ocn_post}
DATA=${DATA:-${WORKhafs}/ocn_post}
mkdir -p ${COMOUTpost} ${intercom}
mkdir -p ${DATA}

cd ${DATA}

#FHR=${FHRB:-0}
FHR=${FHRB:-${NOUTHRS}}
FHR2=$(printf "%02d" "$FHR")
FHR3=$(printf "%03d" "$FHR")

# Loop for forecast hours
while [ $FHR -le $NHRS ]; do

NEWDATE=$(${NDATE} +${FHR} $CDATE)
YYYY=$(echo $NEWDATE | cut -c1-4)
MM=$(echo $NEWDATE | cut -c5-6)
DD=$(echo $NEWDATE | cut -c7-8)
HH=$(echo $NEWDATE | cut -c9-10)

ocnout=ocn_${YYYY}_${MM}_${DD}_${HH}.nc

if [ $FHR -lt $NHRS ]; then
  NEWDATEn=$(${NDATE} +$((${FHR}+${NOUTHRS})) $CDATE)
else
  NEWDATEn=${NEWDATE}
fi
YYYYn=$(echo $NEWDATEn | cut -c1-4)
MMn=$(echo $NEWDATEn | cut -c5-6)
DDn=$(echo $NEWDATEn | cut -c7-8)
HHn=$(echo $NEWDATEn | cut -c9-10)
ocnoutn=ocn_${YYYYn}_${MMn}_${DDn}_${HHn}.nc

ocnpost=${out_prefix}.${RUN}.mom6.f${FHR3}.nc

# Check if post has processed this forecast hour previously
if [ -s ${intercom}/ocnpostf${FHR3} ] && \
   [ -s ${INPdir}/log.atm.f${FHR3} ] && \
   [ ${intercom}/ocnpostf${FHR3} -nt ${INPdir}/log.atm.f${FHR3} ] && \
   [ -s ${COMOUTpost}/${ocnpost} ]; then

echo "ocnpost done file ${intercom}/ocnpostf${FHR3} exist and newer than ${INPdir}/log.atm.f${FHR3}"
echo "product ${COMOUTpost}/${ocnpost} exist"
echo "skip ocnpost for forecast hour ${FHR3} valid at ${NEWDATE}"

else

# Wait for model output
MAX_WAIT_TIME=${MAX_WAIT_TIME:-1800}
n=0
while [ $n -le ${MAX_WAIT_TIME} ]; do
  if [ ! -s ${INPdir}/log.atm.f${FHR3} ] || \
     [ ! -s ${INPdir}/${ocnout} ] || \
     [ ! -s ${INPdir}/${ocnoutn} ]; then
    echo "${INPdir}/log.atm.f${FHR3}, ${INPdir}/${ocnout}, or ${INPdir}/${ocnoutn} not ready, sleep 10s"
    sleep 10s
  else
    echo "${INPdir}/log.atm.f${FHR3}, ${INPdir}/${ocnout}, and ${INPdir}/${ocnoutn} exist"
    echo "Wait ${INPdir}/${ocnout} to be old enough, then do ocn post"
	while [ $(( $(date +%s) - $(stat -c %Y ${INPdir}/${ocnout}) )) -lt 20 ]; do sleep 20; done
    break
  fi
  if [ $n -gt ${MAX_WAIT_TIME} ]; then
    echo "FATAL ERROR: Waited ${INPdir}/log.atm.f${FHR3}, ${INPdir}/${ocnout}, ${INPdir}/${ocnoutn} too long $n > ${MAX_WAIT_TIME} seconds. Exiting"
    exit 1
  fi
  n=$((n+10))
done

# Deliver to COMOUTpost
if [ $SENDCOM = YES ]; then
  ${FCP} ${INPdir}/${ocnout} ${COMOUTpost}/${ocnpost}
fi

# Write out the ocnpostdone message file
echo 'done' > ${intercom}/ocnpostf${FHR3}

fi
# End if for checking if post has processed this forecast hour previously

FHR=$(($FHR + ${NOUTHRS}))
FHR2=$(printf "%02d" "$FHR")
FHR3=$(printf "%03d" "$FHR")

done
# End loop for forecast hours

# Special treatment for ocn f000 history output (actually first time step output)
# Deliver oicout to COMOUTpost
# Note: This is because the oicout file will not be ready/closed until the forecast completes.
NEWDATE=$CDATE
YYYY=$(echo $NEWDATE | cut -c1-4)
MM=$(echo $NEWDATE | cut -c5-6)
DD=$(echo $NEWDATE | cut -c7-8)
HH=$(echo $NEWDATE | cut -c9-10)
oicout=oic_${YYYY}_${MM}_${DD}_${HH}.nc
oicpost=${out_prefix}.${RUN}.mom6.f000.nc

if [ $SENDCOM = YES ]; then
  ${FCP} ${INPdir}/${oicout} ${COMOUTpost}/${oicpost}
fi

cd ${DATA}
