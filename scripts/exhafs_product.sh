#!/bin/sh

set -xe

CDATE=${CDATE:-${YMDH}}
YYYY=$(echo $CDATE | cut -c 1-4)
CC=$(echo $CDATE | cut -c 1-2)
YY=$(echo $CDATE | cut -c 3-4)
MM=$(echo $CDATE | cut -c 5-6)
DD=$(echo $CDATE | cut -c 7-8)
HH=$(echo $CDATE | cut -c 9-10)

# Sepcial settings if this is an atm_init run
if [ ${RUN_INIT:-NO} = YES ]; then

if [ "${ENSDA}" = YES ]; then
  INPdir=${WORKhafs}/atm_init_ens/mem${ENSID}/post
  COMOUTproduct=${WORKhafs}/intercom/atm_init_ens/mem${ENSID}
  NHRS_ENS=0
elif [ ${FGAT_MODEL} = gdas ]; then
  INPdir=${WORKhafs}/intercom/atm_init_fgat${FGAT_HR}/post
  COMOUTproduct=${WORKhafs}/intercom/atm_init_fgat${FGAT_HR}
  NHRS=0
else
  INPdir=${WORKhafs}/intercom/atm_init/post
  COMOUTproduct=${WORKhafs}/intercom/atm_init
  NHRS=0
fi

else

if [ "${ENSDA}" = YES ]; then
  INPdir=${WORKhafs}/intercom/post_ens/mem${ENSID}
  COMOUTproduct=${COMhafs}/product_ens/mem${ENSID}
else
  INPdir=${WORKhafs}/intercom/post
  COMOUTproduct=${COMhafs}
fi

fi

if [ ${ENSDA} = YES ]; then
# Ensemble member with ENSID <= ${ENS_FCST_SIZE} will run the full-length NHRS forecast
  if [ $((10#${ENSID})) -le ${ENS_FCST_SIZE:-10} ]; then
    NHRS=${NHRS:-126}
  else
    NHRS=${NHRS_ENS:-6}
  fi
  export NBDYHRS=${NBDYHRS_ENS:-3}
  export NOUTHRS=${NOUTHRS_ENS:-3}
  export CASE=${CASE_ENS:-C768}
  export CRES=$(echo $CASE | cut -c 2-)
  export gtype=${gtype_ens:-regional}
  export LEVS=${LEVS_ENS:-65}
else
  export NHRS=${NHRS:-126}
  export NBDYHRS=${NBDYHRS:-3}
  export NOUTHRS=${NOUTHRS:-3}
  export CASE=${CASE:-C768}
  export CRES=$(echo $CASE | cut -c 2-)
  export gtype=${gtype:-regional}
  export LEVS=${LEVS:-65}
fi

out_prefix=${out_prefix:-$(echo "${STORMID,,}.${CDATE}")}

neststr=${neststr:-""} #".nest02"
tilestr=${tilestr:-".tile1"} #".tile2"
gridstr=${gridstr:?}

trk_atcfunix=${out_prefix}.${RUN}.trak.atcfunix
all_atcfunix=${out_prefix}.${RUN}.trak.atcfunix.all
fhr_atcfunix=${out_prefix}.${RUN}.trak.atcfunix.f

trk_atcfunix_grid=${out_prefix}.${RUN}.${gridstr}.trak.atcfunix
all_atcfunix_grid=${out_prefix}.${RUN}.${gridstr}.trak.atcfunix.all
fhr_atcfunix_grid=${out_prefix}.${RUN}.${gridstr}.trak.atcfunix.f

GETTRKEXEC=${GETTRKEXEC:-${EXEChafs}/hafs_gettrk.x}
TAVEEXEC=${GETTRKEXEC:-${EXEChafs}/hafs_tave.x}
VINTEXEC=${VINTEXEC:-${EXEChafs}/hafs_vint.x}
SUPVITEXEC=${SUPVITEXEC:-${EXEChafs}/hafs_supvit.x}
NHCPRODUCTSEXEC=${NHCPRODUCTSEXEC:-${EXEChafs}/hafs_nhc_products.x}

INPdir=${INPdir:-${WORKhafs}/intercom/post}
DATA=${DATA:-${WORKhafs}/product}
COMOUTproduct=${COMOUTproduct:-${COMhafs}}
mkdir -p ${COMOUTproduct}
mkdir -p ${DATA}

tmp_vital=${WORKhafs}/tmpvit
old_vital=${WORKhafs}/oldvit

#===============================================================================
# Run GFDL vortextracker

DATA_tracker=${DATA}/tracker${neststr}
mkdir -p ${DATA_tracker}

cd ${DATA_tracker}

# Compute domain for tracker
gmodname=hafs
rundescr=trak
atcfdescr=storm

# Link the track files and generate the input.fcst_minutes file
if [ -s input.fcst_minutes ]; then
  rm -f input.fcst_minutes
fi
IFHR=0
FHR=0
FHR3=$(printf "%03d" "$FHR")
while [ $FHR -le $NHRS ]; do
  echo "FHR3="${FHR3}
  FMIN=$((${FHR} * 60))
  minstr=$(printf "%5.5d" "$FMIN" )
  trk_grb2file=${out_prefix}.${RUN}.${gridstr}.trk.f${FHR3}.grb2
  trk_grb2indx=${out_prefix}.${RUN}.${gridstr}.trk.f${FHR3}.grb2.ix
  tracker_grb2file=${gmodname}.${rundescr}.${atcfdescr}.${CDATE}.f${minstr}
  tracker_grb2indx=${gmodname}.${rundescr}.${atcfdescr}.${CDATE}.f${minstr}.ix
  ${NLN} ${INPdir}/${trk_grb2file} ./${tracker_grb2file}
  ${NLN} ${INPdir}/${trk_grb2indx} ./${tracker_grb2indx}
  IFHR=$(($IFHR + 1))
  LINE=$(printf "%4d %5d" "$IFHR" "$FMIN")
  echo "$LINE" >> input.fcst_minutes
  FHR=$(($FHR + $NOUTHRS))
  FHR3=$(printf "%03d" "$FHR")
done

rm -f fort.*

# Find and sort active storms for this cycle from known tcvitals file
# This can potentially provide multiple tcvital messages to the tracker,
# so that it can track multiple storms simultaneously.
# *** Currently, tcutil_multistorm_sort.py searches the tcvitals files
# specified in the script. Need to modify it to be able to deal with storm
# message files/dirs, as well as passing in tcvitals files.
${USHhafs}/tcutil_multistorm_sort.py ${YMDH} | cut -c1-95 > allvit

# Prepare the input/output files
rm -f input.vitals
vitmsg=$(cat ${tmp_vital} | cut -c1-95)
echo "${vitmsg}" > input.vitals
if grep -v "${vitmsg}" allvit; then
  grep -v "${vitmsg}" allvit >> input.vitals
fi

${NCP} input.vitals tcvit_rsmc_storms.txt
${NLN} input.vitals       fort.12
touch fort.14
${NLN} input.fcst_minutes fort.15
${NLN} output.all         fort.61
${NLN} output.atcf        fort.62
${NLN} output.radii       fort.63
${NLN} output.atcfunix    fort.64
${NLN} output.initvitl    fort.65
${NLN} output.atcf_gen    fort.66
${NLN} output.genvitals   fort.67
${NLN} output.atcf_sink   fort.68
${NLN} output.atcf_hfip   fort.69
${NLN} output.cps_parms   fort.71
${NLN} output.structure   fort.72
${NLN} output.fractwind   fort.73
${NLN} output.ike         fort.74
${NLN} output.pdfwind     fort.76

# Prepare ./deliver.sh, which will used in gettrk.x to deliver the atcfunix
# track file to COMhafs as soon as it becomes available. It also delivers
# atcfunix before forecast hour 12 into COMhafs for storm cycling.
if [ "${tilestr}" = ".tile${nest_grids}" ]; then

cat > ./deliver.sh<<EOF
#!/bin/sh
set -x
${NCP} output.atcfunix ${COMOUTproduct}/${all_atcfunix_grid}
${NCP} output.atcfunix ${COMOUTproduct}/${all_atcfunix}
if [[ \${1:-''} -le 12 ]]; then
  ${NCP} output.atcfunix ${COMOUTproduct}/${fhr_atcfunix_grid}\$(printf "%03d" "\${1:-''}")
  ${NCP} output.atcfunix ${COMOUTproduct}/${fhr_atcfunix}\$(printf "%03d" "\${1:-''}")
fi
EOF

else

cat > ./deliver.sh<<EOF
#!/bin/sh
set -x
${NCP} output.atcfunix ${COMOUTproduct}/${all_atcfunix_grid}
if [[ \${1:-''} -le 12 ]]; then
  ${NCP} output.atcfunix ${COMOUTproduct}/${fhr_atcfunix_grid}\$(printf "%03d" "\${1:-''}")
fi
EOF

fi

chmod +x ./deliver.sh

# Prepare the input namelist
${NCP} ${PARMhafs}/product/namelist.gettrk_tmp ./
if [ ${trkd12_combined:-no} = "no" ] && [ "${tilestr}" = ".tile${nest_grids}" ] && \
   [[ "${is_moving_nest}" = *".true."* ]]; then
  NESTTYP="movable"
else
  NESTTYP="fixed"
fi
cat namelist.gettrk_tmp | sed s/_BCC_/${CC}/ | \
                          sed s/_BYY_/${YY}/ | \
                          sed s/_BMM_/${MM}/ | \
                          sed s/_BDD_/${DD}/ | \
                          sed s/_BHH_/${HH}/ | \
                          sed s/_NESTTYP_/${NESTTYP:-fixed}/ | \
                          sed s/_RUN_/${RUN^^}/ | \
                          sed s/_YMDH_/${CDATE}/ > namelist.gettrk
sleep 3s
# Run the vortex tracker gettrk.x
${NCP} -p ${GETTRKEXEC} ./hafs_gettrk.x
set +e
set -o pipefail
time ./hafs_gettrk.x 2>&1 | tee ./hafs_gettrk.out
set +o pipefail
set -e

if grep "top of output_all" ./hafs_gettrk.out ; then
  echo "INFO: exhafs_product has run the vortex tracker successfully"
else
  echo "FATAL ERROR: exhafs_product failed running vortex tracker"
  exit 1
fi

# Extract the tracking records for tmpvit
STORMNUM=$(echo ${STORMID} | cut -c1-2)
STORMBS1=$(echo ${STORMID} | cut -c3)

# Deal with rare Southern Atlantic storms
if [ $STORMBS1 = "Q" ]; then
 sed -i 's/SL/SQ/g' ${COMOUTproduct}/${all_atcfunix_grid}
fi

${NCP} ${COMOUTproduct}/${all_atcfunix_grid} ${COMOUTproduct}/${all_atcfunix_grid}.orig
if [ -s ${COMOUTproduct}/${all_atcfunix_grid}.orig ]; then
  if [ $STORMNUM == "00" ]; then
    if grep -v "^.., ${STORMNUM}," ${COMOUTproduct}/${all_atcfunix_grid}.orig; then
      grep -v "^.., ${STORMNUM}," ${COMOUTproduct}/${all_atcfunix_grid}.orig > ${COMOUTproduct}/${all_atcfunix_grid}
    else
      echo -n > ${COMOUTproduct}/${all_atcfunix_grid}
    fi
  else
    grep "^.., ${STORMNUM}," ${COMOUTproduct}/${all_atcfunix_grid} | grep -E "^${STORMBS1}.,|^.${STORMBS1}," \
      > ${COMOUTproduct}/${trk_atcfunix_grid}
  fi
else
  if [ $STORMNUM == "00" ]; then
    echo -n > ${COMOUTproduct}/${all_atcfunix_grid}
  else
    echo -n > ${COMOUTproduct}/${trk_atcfunix_grid}
  fi
fi

if [ "${tilestr}" = ".tile${nest_grids}" ]; then

# Deal with rare Southern Atlantic storms
if [ $STORMBS1 = "Q" ]; then
 sed -i 's/SL/SQ/g' ${COMOUTproduct}/${all_atcfunix}
fi

${NCP} ${COMOUTproduct}/${all_atcfunix} ${COMOUTproduct}/${all_atcfunix}.orig
if [ -s ${COMOUTproduct}/${all_atcfunix}.orig ]; then
  if [ $STORMNUM == "00" ]; then
    if grep -v "^.., ${STORMNUM}," ${COMOUTproduct}/${all_atcfunix}.orig ; then
      grep -v "^.., ${STORMNUM}," ${COMOUTproduct}/${all_atcfunix}.orig >  ${COMOUTproduct}/${all_atcfunix}
    else
      echo -n > ${COMOUTproduct}/${all_atcfunix}
    fi
  else
    grep "^.., ${STORMNUM}," ${COMOUTproduct}/${all_atcfunix} | grep -E "^${STORMBS1}.,|^.${STORMBS1}," \
      > ${COMOUTproduct}/${trk_atcfunix}
  fi
else
  if [ $STORMNUM == "00" ]; then
    echo -n > ${COMOUTproduct}/${all_atcfunix}
  else
    echo -n > ${COMOUTproduct}/${trk_atcfunix}
  fi
fi

if [ ${COMOUTproduct} = ${COMhafs} ] && [ "${SENDDBN^^}" = "YES" ]; then
  if [ $STORMNUM == "00" ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN^^}_ASCII $job ${COMhafs}/${all_atcfunix}
  else
    $DBNROOT/bin/dbn_alert MODEL ${RUN^^}_ASCII $job ${COMhafs}/${trk_atcfunix}
  fi
fi

# Deliver atcf track file
if [ ${COMOUTproduct} = ${COMhafs} ] && [ -s ${COMhafs}/${trk_atcfunix} ]; then
  # Deliver atcf ncep file
  mkdir -p ${COMhafs}/atcf/${hafsbasin2,,}${STORMNUM}${YYYY}
  atcfncep="${COMhafs}/atcf/${hafsbasin2,,}${STORMNUM}${YYYY}/ncep_a${hafsbasin2,,}${STORMNUM}${YYYY}.dat"
  ${NCP} ${COMhafs}/${trk_atcfunix} ${atcfncep}
  if [ "${SENDDBN^^}" = "YES" ]; then
    $DBNROOT/bin/dbn_alert MODEL NHC_ATCF_${RUN^^} $job ${atcfncep}
  fi
  if [ "${RUN_ENVIR^^}" = "NCO" ]; then
    ecflow_client --event SentTrackToNHC
  fi
  # Cat atcf global file
  mkdir -p ${COMhafs}/global
  atcfglobal="${COMhafs}/global/tracks.atcfunix.${YY}"
  cut -c1-112 ${COMhafs}/${trk_atcfunix} >> ${atcfglobal}
fi

# generate nhc products
if [ ${COMOUTproduct} = ${COMhafs} ] && [ -s ${COMhafs}/${trk_atcfunix} ]; then
  mkdir -p ${DATA}/nhc_products
  cd ${DATA}/nhc_products
  ${NLN} ${COMhafs}/${trk_atcfunix} fort.20
  # prepare storm_info
  rm -f storm_info
  echo ${CDATE} > storm_info
  echo ${STORMID^^} >> storm_info
  echo ${STORM^^} >> storm_info
  echo ${RUN^^} >> storm_info
  ${NCP} -p ${NHCPRODUCTSEXEC} ./hafs_nhc_products.x
  ${APRUN} ./hafs_nhc_products.x > ./hafs_nhc_products.out 2>&1
  status=$?; [[ $status -ne 0 ]] && exit $status
  short=${out_prefix}.${RUN}.grib.stats.short
  afos=${out_prefix}.${RUN}.afos
  tpc=${out_prefix}.${RUN}.stats.tpc
  ${NCP} fort.41 ${COMhafs}/${short}
  if [ "${SENDDBN^^}" = "YES" ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN^^}_ASCII $job ${COMhafs}/${short}
  fi
  ${NCP} fort.51 ${COMhafs}/${afos}
  if [ "${SENDDBN^^}" = "YES" ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN^^}_AFOS $job ${COMhafs}/${afos}
  fi
  ${NCP} fort.61 ${COMhafs}/${tpc}
  if [ "${SENDDBN^^}" = "YES" ]; then
    $DBNROOT/bin/dbn_alert MODEL ${RUN^^}_STATS $job ${COMhafs}/${tpc}
  fi
  echo "INFO: nhc products has been successfully generated"
fi

fi #if [ "${tilestr}" = ".tile${nest_grids}" ]; then

cd ${DATA}
