#!/bin/sh

set -xe

export PARMgsi=${PARMgsi:-${PARMhafs}/analysis/gsi}
export COMgfs=${COMgfs:-/gpfs/dell1/nco/ops/com/gfs/para}
export COMINhafs=${COMgfs:-/gpfs/dell1/nco/ops/com/gfs/para}
export use_bufr_nr=${use_bufr_nr:-no}
export out_prefix=${out_prefix:-$(echo "${STORM}${STORMID}.${YMDH}" | tr '[A-Z]' '[a-z]')}

export RUN_GSI=${RUN_GSI:-NO}

TOTAL_TASKS=${TOTAL_TASKS:-2016}
NCTSK=${NCTSK:-12}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-2}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}

# Utilities
NDATE=${NDATE:-ndate}
export NCP=${NCP:-"/bin/cp"}
export NMV=${NMV:-"/bin/mv"}
export NLN=${NLN:-"/bin/ln -sf"}
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
export MPISERIAL=${MPISERIAL:-${EXEChafs}/hafs_mpiserial.x}
export COMPRESS=${COMPRESS:-gzip}
export UNCOMPRESS=${UNCOMPRESS:-gunzip}
export TAR=${TAR:-tar}

if [ $GFSVER = PROD2021 ]; then
  export atmos="atmos/"
elif [ $GFSVER = PROD2019 ]; then
  export atmos=""
else
  export atmos=""
fi

yr=`echo $CDATE | cut -c1-4`
mn=`echo $CDATE | cut -c5-6`
dy=`echo $CDATE | cut -c7-8`


if [ ${RUN_GSI} = "NO" ]; then
  echo "RUN_GSI: $RUN_GSI"
  echo "Do nothing. Exiting"
  exit
fi

COMhafs=${COMhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/com/${CDATE}/${STORMID}}
WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
intercom=${intercom:-${WORKhafs}/intercom/obs_proc}
SENDCOM=${SENDCOM:-YES}

DATA=${DATA:-${WORKhafs}/obs_proc}

mkdir -p ${DATA}

cd ${DATA}

# Update the original prepbufr data
# To enable assimilating METAR observations
mkdir -p prepbufr
cd prepbufr

# Copy gfs prepbufr file
COMIN_OBS=${COMIN_OBS:-${COMgfs}/gfs.$PDY/$cyc/${atmos}}
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

# Deliver to com
if [ $SENDCOM = YES ]; then
  mkdir -p ${COMhafs}
  ${NCP} -p ./prepbufr.qm_typ ${COMhafs}/${out_prefix}.hafs.prepbufr
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
  #Remove bad data (not containing REL or SPG)
  grep -lLE --null 'REL|SPG' ./*.mod | xargs -0 rm
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

# Link and run the executable
OBSPREPROCEXEC=${OBSPREPROCEXEC:-${EXEChafs}/hafs_obs_preproc.x}
${NCP} -p ${OBSPREPROCEXEC} ./hafs_obs_preproc.x
${APRUNS} ./hafs_obs_preproc.x 1> ./hafs_obs_preproc.out 2>&1

# Deliver to com
if [ $SENDCOM = YES ]; then
  mkdir -p ${COMhafs}
  ${NCP} -p ./tempdrop.prepbufr ${COMhafs}/${out_prefix}.tempdrop.prepbufr
fi

# Deliver to intercom
mkdir -p ${intercom}
${NCP} -p ./tempdrop.prepbufr ${intercom}/tempdrop.prepbufr

fi # end if [ -s ./tempdrop.filelist ]; then

exit
