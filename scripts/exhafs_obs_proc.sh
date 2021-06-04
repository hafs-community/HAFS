#!/bin/sh

set -xe

export PARMgsi=${PARMgsi:-${PARMhafs}/analysis/gsi}
export COMgfs=${COMgfs:-/gpfs/dell1/nco/ops/com/gfs/para}
export COMINhafs=${COMgfs:-/gpfs/dell1/nco/ops/com/gfs/para}
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
# Deal with tempdrop drifting
mkdir -p tempdrop
cd tempdrop

COMIN_OBS=${COMIN_OBS:-${COMINhafs}/hafs.$PDY/$cyc/${atmos}}
dropsondetarfile=${COMIN_OBS}/hafs.t${cyc}z.dropsonde.tar
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
