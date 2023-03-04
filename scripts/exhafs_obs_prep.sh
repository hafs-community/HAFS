#!/bin/sh

set -xe

cyc=${cyc:?}
CDATE=${CDATE:-${YMDH}}
yr=$(echo $CDATE | cut -c1-4)
mn=$(echo $CDATE | cut -c5-6)
dy=$(echo $CDATE | cut -c7-8)

RUN_GSI=${RUN_GSI:-NO}
use_bufr_nr=${use_bufr_nr:-no}
out_prefix=${out_prefix:-$(echo "${STORMID,,}.${CDATE}")}
atmos="atmos/"

if [ ${RUN_GSI} = "NO" ]; then
  echo "RUN_GSI: $RUN_GSI"
  echo "Do nothing. Exiting"
  exit
fi

PARMgsi=${PARMgsi:-${PARMhafs}/analysis/gsi}
intercom=${intercom:-${WORKhafs}/intercom/obs_prep}
SENDCOM=${SENDCOM:-YES}

DATA=${DATA:-${WORKhafs}/obs_prep}

mkdir -p ${DATA}

cd ${DATA}

# Update the original prepbufr data
# To enable assimilating METAR observations
mkdir -p prepbufr
cd prepbufr

# Copy gfs prepbufr file
COMIN_OBS=${COMIN_OBS:-${COMINobs}/gfs.$PDY/$cyc/${atmos}}
if [[ ${use_bufr_nr:-no} = "yes" ]]; then
  PREPQC=${COMIN_OBS}/gfs.t${cyc}z.prepbufr.nr
else
  PREPQC=${COMIN_OBS}/gfs.t${cyc}z.prepbufr
fi

${NCP} -L ${PREPQC} ./prepbufr.orig
${NCP} -L ${PREPQC} ./prepbufr.qm_typ

${NLN} -sf ./prepbufr.orig   ./fort.21
${NLN} -sf ./prepbufr.qm_typ ./fort.51

# Link and run the executable
${NCP} -p ${EXEChafs}/hafs_change_prepbufr_qm_typ.x ./hafs_change_prepbufr_qm_typ.x
${APRUNS} ./hafs_change_prepbufr_qm_typ.x > ./hafs_change_prepbufr_qm_typ.out 2>&1
status=$?; [[ $status -ne 0 ]] && exit $status
cat ./hafs_change_prepbufr_qm_typ.out

# Deliver to com
if [ $SENDCOM = YES ]; then
  mkdir -p ${COMhafs}
  ${NCP} -p ./prepbufr.qm_typ ${COMhafs}/${out_prefix}.${RUN}.prepbufr
fi

# Deliver to intercom
mkdir -p ${intercom}
${NCP} -p ./prepbufr.qm_typ ${intercom}/hafs.prepbufr

# Deal with tempdrop drifting
cd ${DATA}
mkdir -p tempdrop
cd tempdrop

COMINhafs_OBS=${COMINhafs_OBS:-${COMINhafs}/hafs.$PDY/$cyc/${atmos}}
dropsondetarfile=${COMINhafs_OBS}/hafs.t${cyc}z.dropsonde.tar
# Proceed if dropsondetarfile exists and non-empty
if [ -s ${dropsondetarfile} ]; then
  ${TAR} -xvf ${dropsondetarfile}
  #Genereate the tempdrop.filelist
  /bin/ls -1 ./*.mod | sed -e 's/^/"/g' -e 's/$/"/g' > ./tempdrop.filelist
fi

# Proceed if tempdrop.filelist non-empty
if [ -s ./tempdrop.filelist ]; then

# Copy the needed files
${NCP} ${PARMgsi}/prepobs_prep.bufrtable ./
${NCP} ${PARMgsi}/bufrinfo.json.tempdrop ./bufrinfo.json
${NCP} ${PARMgsi}/obs-preproc.input.tempdrop.tmp ./obs-preproc.input.tmp

# Prepare the namelist
analdate="${yr}-${mn}-${dy}_${cyc}:00:00"
sed -e "s/_analdate_/${analdate}/g" \
    obs-preproc.input.tmp > obs-preproc.input

# Run the executable
OBSPREPROCEXEC=${OBSPREPROCEXEC:-${EXEChafs}/hafs_obs_preproc.x}
${NCP} -p ${OBSPREPROCEXEC} ./hafs_obs_preproc.x
${APRUNS} ./hafs_obs_preproc.x > ./hafs_obs_preproc.out 2>&1
status=$?; [[ $status -ne 0 ]] && exit $status
cat ./hafs_obs_preproc.out

# Deliver to com
if [ $SENDCOM = YES ]; then
  mkdir -p ${COMhafs}
  if [ -s ./tempdrop.prepbufr ]; then
    ${NCP} -p ./tempdrop.prepbufr ${COMhafs}/${out_prefix}.${RUN}.tempdrop.prepbufr
  fi
fi

# Deliver to intercom
mkdir -p ${intercom}
if [ -s ./tempdrop.prepbufr ]; then
  ${NCP} -p ./tempdrop.prepbufr ${intercom}/hafs.tempdrop.prepbufr
fi

fi # end if [ -s ./tempdrop.filelist ]; then

