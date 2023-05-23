#!/bin/sh

set -xe

CDATE=${CDATE:-${YMDH}}

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

ocnpost=${out_prefix}.${RUN}.mom6.f${FHR3}.nc

# Check if post has processed this forecast hour previously
if [ -s ${intercom}/ocnpostf${FHR3} ] && \
   [ ${intercom}/ocnpostf${FHR3} -nt ${INPdir}/log.atm.f${FHR3} ] && \
   [ -s ${COMOUTpost}/${ocnpost} ]; then

echo "ocnpost done file ${intercom}/ocnpostf${FHR3} exist and newer than ${INPdir}/log.atm.f${FHR3}"
echo "product ${COMOUTpost}/${ocnpost} exist"
echo "skip ocnpost for forecast hour ${FHR3} valid at ${NEWDATE}"

else

# Wait for model output
n=1
while [ $n -le 360 ]; do
  if [ ! -s ${INPdir}/log.atm.f${FHR3} ] || \
     [ ! -s ${INPdir}/ocn_${YYYY}_${MM}_${DD}_${HH}.nc ]; then
    echo "${INPdir}/log.atm.f${FHR3} not ready, sleep 20s"
    sleep 20s
  else
    echo "${INPdir}/log.atm.f${FHR3}, ${INPdir}/ocn_${YYYY}_${MM}_${DD}_${HH}.nc exist"
    echo "Wait ${INPdir}/ocn_${YYYY}_${MM}_${DD}_${HH}.nc to be old enough, then do ocn post"
	while [ $(( $(date +%s) - $(stat -c %Y ${INPdir}/ocn_${YYYY}_${MM}_${DD}_${HH}.nc) )) -lt 20 ]; do sleep 20; done
    break
  fi
  if [ $n -ge 360 ]; then
    echo "FATAL ERROR: Waited too many times: $n. Exiting"
    exit 1
  fi
  n=$((n+1))
done

# Deliver to COMOUTpost
if [ $SENDCOM = YES ]; then
  ${NCP} -p ${INPdir}/ocn_${YYYY}_${MM}_${DD}_${HH}.nc ${COMOUTpost}/${ocnpost}
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

cd ${DATA}

