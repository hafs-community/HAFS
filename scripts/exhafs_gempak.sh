#!/bin/sh

set -xe

export storm_id=${STORMID,,}
export fstart=0
export finc=6
export fend=${NHRS:-126}

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
elif [ "${gridstr}" = "storm" ]; then
  DATAgempak=${DATA}/${NET}
  GDOUTF=${NET}n_${PDY}${cyc}f${fhr3}_${storm_id}
  GPOUTF=${COMOUT}/gempak/${storm_id}/${RUN}n_${PDY}${cyc}f${fhr3}_${storm_id}
else
  echo "FATAL ERROR: unknown gridstr of ${gridstr}"
  exit 1
fi

mkdir -p ${DATAgempak}
cd ${DATAgempak}
# Wait for model output
n=1
while [ $n -le 360 ]; do
  if [ ! -s ${GBFILE} ] || [ ! -s ${GBINDX} ]; then
    echo "${GBFILE} or ${GBINDX} not ready, sleep 10s"
    sleep 10s
  else
    echo "${GBFILE}, ${GBINDX} ready, continue"
    sleep 3s
    break
  fi
  if [ $n -ge 360 ]; then
    echo "FATAL ERROR: Waited too many times: $n. Exiting"
    exit 1
  fi
  n=$((n+1))
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
status=$?; [[ $status -ne 0 ]] && exit $status

# Gempak does not always have a non-zero return code when it
# cannot produce the desired grid. Check for this case here.
ls -l $GDOUTF
status=$?; [[ $status -ne 0 ]] && exit $status

touch $GDOUTF.done

${GEMEXE}/gpend
status=$?; [[ $status -ne 0 ]] && exit $status

if [ "${SENDCOM^^}" = "YES" ]; then
  ${NCP} -p $GDOUTF $GPOUTF
  if [ "${SENDDBN^^}" = "YES" ]; then
    $DBNROOT/bin/dbn_alert MODEL HAFS_GEMPAK $job $GPOUTF
  fi
fi

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
status=$?; [[ $status -ne 0 ]] && exit $status

cat $DATA/meta_grid.log
cat $DATA/meta_nest.log

date
