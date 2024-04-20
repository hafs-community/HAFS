#!/bin/sh
################################################################################
# Script Name: exhafs_obs_prep.sh
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script runs the HAFS specific observation preprocessing steps needed
#   by data assimilation.
################################################################################
set -x -o pipefail

cyc=${cyc:?}
CDATE=${CDATE:-${YMDH}}
yr=$(echo $CDATE | cut -c1-4)
mn=$(echo $CDATE | cut -c5-6)
dy=$(echo $CDATE | cut -c7-8)

atmos="atmos/"
COMINhafs_OBS=${COMINhafs_OBS:-${COMINhafs}/hafs.$PDY/$cyc/${atmos}}
RUN_GSI=${RUN_GSI:-NO}
use_bufr_nr=${use_bufr_nr:-no}
out_prefix=${out_prefix:-$(echo "${STORMID,,}.${CDATE}")}

if [ ${RUN_GSI} = "NO" ]; then
  echo "RUN_GSI: $RUN_GSI"
  echo "Do nothing. Exiting"
  exit
fi

PARMgsi=${PARMgsi:-${PARMhafs}/analysis/gsi}
SENDCOM=${SENDCOM:-YES}
intercom=${intercom:-${WORKhafs}/intercom/obs_prep}
mkdir -p ${COMhafs} ${intercom}

DATA=${DATA:-${WORKhafs}/obs_prep}
mkdir -p ${DATA}

cd ${DATA}

# Update the original prepbufr data
# To enable assimilating METAR observations
mkdir -p prepbufr
cd prepbufr

# Copy gfs prepbufr file
if [[ ${use_bufr_nr:-no} = "yes" ]]; then
  PREPQC=${COMINobs}/gfs.$PDY/$cyc/${atmos}/gfs.t${cyc}z.prepbufr.nr
else
  PREPQC=${COMINobs}/gfs.$PDY/$cyc/${atmos}/gfs.t${cyc}z.prepbufr
fi

# Check if gfs prepbufr file exists and non-empty, otherwise exit with fatal error.
if [ ! -s ${PREPQC} ]; then
  echo "FATAL ERROR: ${PREPQC} does not exist or is empty. Exiting ..."
  exit 1
fi

${NCP} -Lp ${PREPQC} ./prepbufr.orig
${NCP} -Lp ${PREPQC} ./prepbufr.qm_typ

${NLN} ./prepbufr.orig   ./fort.21
${NLN} ./prepbufr.qm_typ ./fort.51

# Run the executable
${NCP} -p ${EXEChafs}/hafs_tools_change_prepbufr_qm_typ.x ./hafs_tools_change_prepbufr_qm_typ.x
${SOURCE_PREP_STEP}
${APRUNS} ./hafs_tools_change_prepbufr_qm_typ.x 2>&1 | tee ./change_prepbufr_qm_typ.out
export err=$?; err_chk

# Deliver to intercom
${NCP} -p ./prepbufr.qm_typ ${intercom}/${NET}.t${cyc}z.prepbufr

cd $DATA

NFTLDPLR=${NET}.t${cyc}z.tldplr.tm00.bufr_d
RFTLDPLR=${out_prefix}.${RUN}.tldplr.tm00.bufr_d
NFHDOB=${NET}.t${cyc}z.hdob.tm00.bufr_d
RFHDOB=${out_prefix}.${RUN}.hdob.tm00.bufr_d
NFNEXRAD=${NET}.t${cyc}z.nexrad.tm00.bufr_d
RFNEXRAD=${out_prefix}.${RUN}.nexrad.tm00.bufr_d
NFdropsonde=${NET}.t${cyc}z.dropsonde.tar
RFdropsonde=${out_prefix}.${RUN}.dropsonde.tar
NFtempdrop=${NET}.t${cyc}z.tempdrop.prepbufr
RFtempdrop=${out_prefix}.${RUN}.tempdrop.prepbufr
rm -f ${intercom}/${NFTLDPLR}
rm -f ${intercom}/${NFHDOB}
rm -f ${intercom}/${NFNEXRAD}
rm -f ${intercom}/${NFdropsonde}
rm -f ${intercom}/${NFtempdrop}

if [ $OBS_DUMP = YES ]; then

export TANK=${TANK:-${DCOMROOT:?}}
export FIX_PATH=${BUFR_DUMPLIST:?}/fix
export LOUD=on

# Dump TDR data
BDATE=$( $NDATE -24 $CDATE )
BPDY=$(echo $BDATE | cut -c1-8)
if [[ -s $TANK/$PDY/b006/xx070 || -s $TANK/$BPDY/b006/xx070 ]]; then
  export DATA_DUMPJB=$DATA/tldplr_dumpjb.log
  ${DUMPJB:?} ${CDATE} 3.00 tldplr
  status=$?
  if [[ $status -ne 0 ]]; then
    echo "WARNING: TDR dump with exit code of $status. Continue ..."
  fi
  cat ./tldplr.out
  cat ${DATA_DUMPJB}
else
  echo "INFO: TDR tank $TANK/$PDY/b006/xx070 or $TANK/$BPDY/b006/xx070 empty or not found. Continue ..."
fi
if [ -s ./tldplr.ibm ]; then
  # Deliver to intercom
  ${NCP} ./tldplr.ibm ${intercom}/${NFTLDPLR}
  # Deliver to com
  if [ $SENDCOM = YES ]; then
    ${FCP} ./tldplr.ibm ${COMhafs}/${RFTLDPLR}
  fi
fi

# Dump HDOB data
export DATA_DUMPJB=$DATA/hdob_dumpjb.log
${DUMPJB} ${CDATE} 3.00 hdob
status=$?
if [[ $status -ne 0 ]]; then
  echo "WARNING: HDOB dump with exit code of $status. Continue ..."
fi
cat ./hdob.out
cat ${DATA_DUMPJB}
if [ -s ./hdob.ibm ]; then
  # Deliver to intercom
  ${NCP} ./hdob.ibm ${intercom}/${NFHDOB}
  # Deliver to com
  if [ $SENDCOM = YES ]; then
    ${FCP} ./hdob.ibm ${COMhafs}/${RFHDOB}
  fi
fi

# Dump NEXRAD data
#===========================================================================
# Dump # 6 : NEXRAD -- TOTAL NUMBER OF SUBTYPES = 8
#              (8)
#            for catch-up cycles tm06-01:
#              time window radius is (-0.50,+0.50) hours for NEXRAD
#            for on-time tm00 cycles:
#              time window radius is (-0.75,+1.50) hours for NEXRAD
#===========================================================================

# Dump globally since all reports over CONUS (and geographical filtering is
#  computationally expensive)

export LALO=0

# NEXRAD tanks are hourly

# Initialize radial wind and reflectivity dumps to SKIP all hourly tanks
for dump_file in 006010 006011 006012 006013 006014 006015 006016 006017 \
                 006018 006019 006020 006021 006022 006023 006024 006025 \
                 006026 006027 006028 006029 006030 006031 006032 006033; do
  eval SKIP_$dump_file=YES
  eval export SKIP_$dump_file
done

for dump_file in 006040 006041 006042 006043 006044 006045 006046 006047 \
                 006048 006049 006050 006051 006052 006053 006054 006055 \
                 006056 006057 006058 006059 006060 006061 006062 006063; do
  eval SKIP_$dump_file=YES
  eval export SKIP_$dump_file
done

#-------------------------------------------------------------------------------
# Now unset the SKIP for those hourly radial wind and relectivity tanks that
#  are w/i requested dump center cycle time window so these will be read
# This is based on center data dump time and run analysis cycle time
#-------------------------------------------------------------------------------
# 00z run analysis cycle time:
#  tm06 (17.50-18.50 Z) - unsets SKIP on tanks 006/ 027, 028, 057, 058
#  tm05 (18.50-19.50 Z) - unsets SKIP on tanks 006/ 028, 029, 058, 059
#  tm04 (19.50-20.50 Z) - unsets SKIP on tanks 006/ 029, 030, 059, 060
#  tm03 (20.50-21.50 Z) - unsets SKIP on tanks 006/ 030, 031, 060, 061
#  tm02 (21.50-22.50 Z) - unsets SKIP on tanks 006/ 031, 032, 061, 062
#  tm01 (22.50-23.50 Z) - unsets SKIP on tanks 006/ 032, 033, 062, 063
#  tm00 (23.25-01.50 Z) - unsets SKIP on tanks 006/ 033, 010, 011, 063, 040, 041
# 06z run analysis cycle time:
#  tm06 (23.50-00.50 Z) - unsets SKIP on tanks 006/ 033, 010, 063, 040
#  tm05 (00.50-01.50 Z) - unsets SKIP on tanks 006/ 010, 011, 040, 041
#  tm04 (01.50-02.50 Z) - unsets SKIP on tanks 006/ 011, 012, 041, 042
#  tm03 (02.50-03.50 Z) - unsets SKIP on tanks 006/ 012, 013, 042, 043
#  tm02 (03.50-04.50 Z) - unsets SKIP on tanks 006/ 013, 014, 043, 044
#  tm01 (04.50-05.50 Z) - unsets SKIP on tanks 006/ 014, 015, 044, 045
#  tm00 (05.25-07.50 Z) - unsets SKIP on tanks 006/ 015, 016, 017, 045, 046, 047
# 12z run analysis cycle time:
#  tm06 (05.50-06.50 Z) - unsets SKIP on tanks 006/ 015, 016, 045, 046
#  tm05 (06.50-07.50 Z) - unsets SKIP on tanks 006/ 016, 017, 046, 047
#  tm04 (07.50-08.50 Z) - unsets SKIP on tanks 006/ 017, 018, 047, 048
#  tm03 (08.50-09.50 Z) - unsets SKIP on tanks 006/ 018, 019, 048, 049
#  tm02 (09.50-10.50 Z) - unsets SKIP on tanks 006/ 019, 020, 049, 050
#  tm01 (10.50-11.50 Z) - unsets SKIP on tanks 006/ 020, 021, 050, 051
#  tm00 (11.25-13.50 Z) - unsets SKIP on tanks 006/ 021, 022, 023, 051, 052, 053
# 18z run analysis cycle time:
#  tm06 (11.50-12.50 Z) - unsets SKIP on tanks 006/ 021, 022, 051, 052
#  tm05 (12.50-13.50 Z) - unsets SKIP on tanks 006/ 022, 023, 052, 053
#  tm04 (13.50-14.50 Z) - unsets SKIP on tanks 006/ 023, 024, 053, 054
#  tm03 (14.50-15.50 Z) - unsets SKIP on tanks 006/ 024, 025, 054, 055
#  tm02 (15.50-16.50 Z) - unsets SKIP on tanks 006/ 025, 026, 055, 056
#  tm01 (16.50-17.50 Z) - unsets SKIP on tanks 006/ 026, 027, 056, 057
#  tm00 (17.25-19.50 Z) - unsets SKIP on tanks 006/ 027, 028, 029, 057, 058, 059
#-------------------------------------------------------------------------------

cycp=${cyc}
subtyp1=$(($cycp  +  9))
[ $subtyp1 -eq 9 ] && subtyp1=33
subtyp2=$(( $cycp  + 10 ))
subtyp3=$(( $cycp  + 11 ))
unset SKIP_0060${subtyp1}
eval export SKIP_0060${subtyp1}
unset SKIP_0060${subtyp2}
eval export SKIP_0060${subtyp2}
unset SKIP_0060${subtyp3}
eval export SKIP_0060${subtyp3}
unset SKIP_0060$(($subtyp1 + 30))
eval export  SKIP_0060$(($subtyp1 + 30))
unset SKIP_0060$(($subtyp2 + 30))
eval export SKIP_0060$(($subtyp2 + 30))
unset SKIP_0060$(($subtyp3 + 30))
eval export SKIP_0060$(($subtyp3 + 30))

export DTIM_earliest_nexrad=${DTIM_earliest_nexrad:-"-0.75"}
export DTIM_latest_nexrad=${DTIM_latest_nexrad:-"+1.50"}

export DATA_DUMPJB=$DATA/nexrad_dumpjb.log
${DUMPJB} ${CDATE} 0.5 nexrad
status=$?
if [[ $status -ne 0 ]]; then
  echo "WARNING: NEXRAD dump with exit code of $status. Continue ..."
fi
cat ./nexrad.out
cat ${DATA_DUMPJB}
if [ -s ./nexrad.ibm ]; then
  # Deliver to intercom
  ${NCP} -p ./nexrad.ibm ${intercom}/${NFNEXRAD}
  # Deliver to com
  if [ $SENDCOM = YES ]; then
    ${FCP} ./nexrad.ibm ${COMhafs}/${RFNEXRAD}
  fi
fi

cd ${DATA}
mkdir -p dropsonde
cd dropsonde
# Deal with tempdrop drifting
analdate="${yr}-${mn}-${dy}_${cyc}:00:00"
${USHhafs}/hafs_format_sonde.py -d ${TANK:?}/ldmdata/obs/upperair/sonde -c ${analdate}
if [[ -s ./dropsonde.${CDATE}.tar ]]; then
  # Deliver to intercom
  ${NCP} -p ./dropsonde.${CDATE}.tar ${intercom}/${NFdropsonde}
  # Deliver to com
  if [ $SENDCOM = YES ]; then
    ${FCP} ./dropsonde.${CDATE}.tar ${COMhafs}/${RFdropsonde}
  fi
fi

else

# Copy over the hafs obs data, which were previous dumped
# TDR data
if [ -s ${COMINhafs_OBS}/${NFTLDPLR} ]; then
  # Deliver to intercom
  ${NCP} -L ${COMINhafs_OBS}/${NFTLDPLR} ${intercom}/${NFTLDPLR}
  # Deliver to com
  if [ $SENDCOM = YES ]; then
    ${FCP} ${intercom}/${NFTLDPLR} ${COMhafs}/${RFTLDPLR}
  fi
fi
# HDOB data
if [ -s ${COMINhafs_OBS}/${NFHDOB} ]; then
  # Deliver to intercom
  ${NCP} -L ${COMINhafs_OBS}/${NFHDOB} ${intercom}/${NFHDOB}
  # Deliver to com
  if [ $SENDCOM = YES ]; then
    ${FCP} ${intercom}/${NFHDOB} ${COMhafs}/${RFHDOB}
  fi
fi
# NEXRAD data
if [ -s ${COMINhafs_OBS}/${NFNEXRAD} ]; then
  # Deliver to intercom
  ${NCP} -L ${COMINhafs_OBS}/${NFNEXRAD} ${intercom}/${NFNEXRAD}
  # Deliver to com
  if [ $SENDCOM = YES ]; then
    ${FCP} ${intercom}/${NFNEXRAD} ${COMhafs}/${RFNEXRAD}
  fi
fi
# TEMPdropsonde data
if [ -s ${COMINhafs_OBS}/${NFdropsonde} ]; then
  # Deliver to intercom
  ${NCP} -L ${COMINhafs_OBS}/${NFdropsonde} ${intercom}/${NFdropsonde}
  # Deliver to com
  if [ $SENDCOM = YES ]; then
    ${FCP} ${intercom}/${NFdropsonde} ${COMhafs}/${RFdropsonde}
  fi
fi
# TEMPdrop prepbufr
if [ -s ${COMINhafs_OBS}/${NFtempdrop} ]; then
  # Deliver to intercom
  ${NCP} -L ${COMINhafs_OBS}/${NFtempdrop} ${intercom}/${NFtempdrop}
  # Deliver to com
  if [ $SENDCOM = YES ]; then
    ${FCP} ${intercom}/${NFtempdrop} ${COMhafs}/${RFtempdrop}
  fi
fi

fi # end if [ $OBS_DUMP = YES ]; then

if [ ! -s ${intercom}/${NFtempdrop} ] && [ -s ${intercom}/${NFdropsonde} ]; then

cd ${DATA}
mkdir -p tempdrop
cd tempdrop
dropsondetarfile=${intercom}/${NFdropsonde}
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
OBSPREPROCEXEC=${OBSPREPROCEXEC:-${EXEChafs}/hafs_tools_obs_preproc.x}
${NCP} -p ${OBSPREPROCEXEC} ./hafs_tools_obs_preproc.x
${APRUNS} ./hafs_tools_obs_preproc.x 2>&1 | tee ./obs_preproc.out
export err=$?; err_chk
if [ -s ./tempdrop.prepbufr ]; then
  # Deliver to intercom
  ${NCP} -p ./tempdrop.prepbufr ${intercom}/${NFtempdrop}
  # Deliver to com
  if [ $SENDCOM = YES ]; then
    ${FCP} ./tempdrop.prepbufr ${COMhafs}/${RFtempdrop}
  fi
fi

fi # end if [ -s ./tempdrop.filelist ]; then

fi # end if [ ! -s ${intercom}/${NFtempdrop} ] && [ -s ${intercom}/${NFdropsonde} ]; then

cd ${DATA}

date
