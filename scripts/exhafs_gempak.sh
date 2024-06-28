#!/bin/sh
################################################################################
# Script Name: exhafs_gempak.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script generates HAFS GEMPAK products.
################################################################################
set -x -o pipefail

export storm_id=${STORMID,,}
export fstart=0
export finc=6
export fend=${NHRS:-126}

intercom=${intercom:-"${WORKhafs}/intercom/gempak"}
mkdir -p ${intercom}

cd ${DATA}
mkdir -p $DATA/${NET} $DATA/${NET}p
mkdir -p ${COMOUT}/gempak/${storm_id}/meta

# Use a custom table to decode reflectivity parameters
${NLN} ${PARMhafs}/gempak/g2varsncep1.tbl $DATA/${NET}/g2varsncep1.tbl
# Use custom tables to filter some parameters
${NLN} ${PARMhafs}/gempak/g2varsncep1_synoptic.tbl $DATA/${NET}p/g2varsncep1.tbl
${NLN} ${PARMhafs}/gempak/g2varswmo2_synoptic.tbl $DATA/${NET}p/g2varswmo2.tbl

fhr=$fstart
# Loop for forecast hours
while [ $fhr -le $fend ]; do

fhr3=$(printf "%03d" "$fhr")

if [ ${gtype} = nest ]; then
  ngrids=$((${nest_grids} + 1))
else
  ngrids=${nest_grids}
fi

# Loop for grids/domains
for ng in $(seq 1 ${ngrids}); do

gridstr=$(echo ${out_gridnames} | cut -d, -f ${ng})
GBFILE=${COMIN}/${out_prefix}.${RUN}.${gridstr}.atm.f${fhr3}.grb2
GBINDX=${COMIN}/${out_prefix}.${RUN}.${gridstr}.atm.f${fhr3}.grb2.idx

if [ "${gridstr}" = "parent" ]; then
  DATAgempak=${DATA}/${NET}p
  GDOUTF=${NET}p_${PDY}${cyc}f${fhr3}_${storm_id}
  GPOUTF=${COMOUT}/gempak/${storm_id}/${RUN}p_${PDY}${cyc}f${fhr3}_${storm_id}
  nestdotstr=""
elif [ "${gridstr}" = "storm" ]; then
  DATAgempak=${DATA}/${NET}
  GDOUTF=${NET}n_${PDY}${cyc}f${fhr3}_${storm_id}
  GPOUTF=${COMOUT}/gempak/${storm_id}/${RUN}n_${PDY}${cyc}f${fhr3}_${storm_id}
  nestdotstr=".nest$(printf '%02d' ${ng})."
else
  echo "FATAL ERROR: unknown gridstr of ${gridstr}"
  exit 1
fi
PDFILE=${WORKhafs}/intercom/post/post${nestdotstr}f${fhr3}

mkdir -p ${DATAgempak}
cd ${DATAgempak}

# Check if gempak has processed this forecast hour previously
if [ -e ${intercom}/${GDOUTF}.done ] && \
   [ ${intercom}/${GDOUTF}.done -nt ${GBINDX} ] && \
   [ -s ${GPOUTF} ]; then

# Symbolically link ${GDOUTF} and ${GDOUTF}.done files needed by gempak meta file generation.
${NLN} ${GPOUTF} ./${GDOUTF}
#${NLN} ${intercom}/${GDOUTF}.done ./

echo "gempak done file ${intercom}/${GDOUTF}.done exist and newer than ${GBINDX}"
echo "gempak product ${GPOUTF} exist"
echo "skip gempak for forecast hour ${fhr3}"

# Otherwise run gempak for this forecast hour
else

# Wait for model output
MAX_WAIT_TIME=${MAX_WAIT_TIME:-900}
n=0
while [ $n -le ${MAX_WAIT_TIME} ]; do
  if [ ! -s ${PDFILE} ] || [ ! -s ${GBFILE} ] || [ ! -s ${GBINDX} ]; then
    echo "${PDFILE}, ${GBFILE} or ${GBINDX} not ready, sleep 10s"
    sleep 10s
  else
    echo "${PDFILE}, ${GBFILE}, ${GBINDX} ready, continue"
    break
  fi
  if [ $n -gt ${MAX_WAIT_TIME} ]; then
    echo "FATAL ERROR: Waited ${PDFILE}, ${GBFILE}, ${GBINDX} too long $n > ${MAX_WAIT_TIME} seconds. Exiting"
    exit 1
  fi
  n=$((n+10))
done

${GEMEXE:?}/nagrib2 << EOF
 GBFILE   = $GBFILE
 GDOUTF   = $GDOUTF
 PROJ     = 
 GRDAREA  = 
 KXKY     = 
 MAXGRD   = 1000
 CPYFIL   = GDS
 GAREA    = DSET
 OUTPUT   = T
 G2TBLS   = 
 G2DIAG   = 
 OVERWR   = NO
 PDSEXT   = NO
l
r
EOF
export err=$?; err_chk

# Gempak does not always have a non-zero return code when it
# cannot produce the desired grid. Check for this case here.
ls -l $GDOUTF
export err=$?; err_chk

${GEMEXE}/gpend
export err=$?; err_chk

if [ "${SENDCOM^^}" = "YES" ]; then
  ${FCP} $GDOUTF $GPOUTF
  if [ "${SENDDBN^^}" = "YES" ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN^^}_GEMPAK $job $GPOUTF
  fi
fi

touch ${intercom}/$GDOUTF.done

fi
# End if for checking if gempak has processed this forecast hour previously

done # End loop for grids/domains

fhr=$(($fhr+$finc))

done # End loop for forecast hours

# Generate gempak meta files
cd ${DATA}
export MODEL=$DATA # Used for datatype.tbl in meta tasks
rm -f cmdfile.gempak
echo "${USHhafs}/hafs_gempak_meta_grid.sh > $DATA/meta_grid.log 2>&1" >> cmdfile.gempak
echo "${USHhafs}/hafs_gempak_meta_nest.sh > $DATA/meta_nest.log 2>&1" >> cmdfile.gempak
chmod +x cmdfile.gempak
${APRUNC} ${MPISERIAL} -m ./cmdfile.gempak
export err=$?; err_chk

cat $DATA/meta_grid.log
cat $DATA/meta_nest.log

date
