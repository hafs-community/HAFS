#!/bin/sh

set -xe

CDATE=${CDATE:-${YMDH}}
NHRS=${NHRS:-126}
NHR3=$( printf "%03d" "$NHRS" )

STORMID=${STORMID:-00L}
STORMNUM=$( echo ${STORMID} | cut -c1-2)
BASIN=${pubbasin2:-AL}

# GPLOT-specific variables (might go elsewhere)
GPLOT_PARSE="${GPLOThafs}/ush/parse_atcf.sh"
GPLOT_WRAPPER="${GPLOThafs}/sorc/GPLOT/scripts/GPLOT_wrapper.sh"
GPLOT_ARCHIVE="${GPLOThafs}/archive/GPLOT_tarballer.sh"
## GJA: 2024-07-04: force custom ADECKhafs directory here
##ADECKhafs=${ADECKhafs:?}
#ADECKhafs="/lfs5/HFIP/hur-aoml/Ghassan.Alaka/adeck/GPLOT"
# LJG: 2024-11-01
ADECKhafs="/scratch2/AOML/aoml-hafs1/Ghassan.Alaka/adeck/GPLOT_2024"
# LJG: 2024-07-31: Fix bad ADECK link?
#ADECKhafs="/lfs5/HFIP/hur-aoml/Ghassan.Alaka/adeck/GPLOT_2024"
BDECKhafs=${BDECKhafs:?}
SYNDAThafs=${SYNDAThafs:?}
if [ "${machine}" == "orion" ]; then
  SIDhafs="/home/galaka/GPLOT/tbl/SIDs_Old_New.dat"
else
  SIDhafs="/home/Ghassan.Alaka/GPLOT/tbl/SIDs_Old_New.dat"
fi

# Setup the working directory and change into it
COMgplot=${COMgplot:-${COMhafs}/hrdgraphics}
WORKgplot=${WORKgplot:-${WORKhafs}/hrdgraphics}
mkdir -p ${COMgplot}
mkdir -p ${WORKgplot}
cd ${WORKgplot}

# Copy and edit the GPLOT namelist
NML=${WORKgplot}/namelist.master.${SUBEXPT}
if [ ! -f ${NML} ]; then
  if [ -f ${GPLOThafs}/parm/namelist.master.${SUBEXPT} ]; then
    cp -p ${GPLOThafs}/parm/namelist.master.${SUBEXPT} ${NML}
  else
    cp -p ${GPLOThafs}/parm/namelist.master.HAFS_Default ${NML}
  fi
fi
sed -i 's/^EXPT =.*/EXPT = '"${SUBEXPT}"'/g' ${NML}
# Lew.Gramer@noaa.gov 2024-04-27:
sed -i 's/^DSOURCE =.*/DSOURCE = '"${RUN^^}"'/g' ${NML}
sed -i 's/^IDATE =.*/IDATE = '"${CDATE}"'/g' ${NML}
sed -i 's/^SID =.*/SID = '"${STORMID}"'/g' ${NML}
sed -i 's/^INIT_HR =.*/INIT_HR = 0/g' ${NML}
sed -i 's/^FNL_HR =.*/FNL_HR = '"${NHRS}"'/g' ${NML}
#sed -i 's@^OCEAN_DIR =.*@OCEAN_DIR = '"${COMhafs}"'@g' ${NML}
sed -i 's@^OCEAN_DIR =.*@OCEAN_DIR = '"${CDSCRUB}"'@g' ${NML} # Lew.Gramer@noaa.gov 2024-05-21
sed -i 's@^IDIR =.*@IDIR = '"${COMhafs}"'@g' ${NML}
sed -i 's@^ODIR =.*@ODIR = '"${WORKgplot}"'@g' ${NML}
model=${RUN,,}
MODEL=${RUN^^}
sed -i 's@^ATCF1_DIR =.*@ATCF1_DIR = '"${COMgplot}"'@g' ${NML}
sed -i 's@^ATCF1_TAG =.*@ATCF1_TAG = '"trak.${model:0:4}.atcfunix"'@g' ${NML}
#   Ghassan.Alaka@noaa.gov: Force GPLOT to only find the ATCF in COMgplot
#sed -i 's@^ATCF2_DIR =.*@ATCF2_DIR = '"${CDNOSCRUB}/${SUBEXPT}"'@g' ${NML}
sed -i 's@^ATCF2_DIR =.*@ATCF2_DIR = '"${COMgplot}"'@g' ${NML}
#   GJA
sed -i 's@^ATCF2_TAG =.*@ATCF2_TAG = '"${RUN,,}.trak.atcfunix"'@g' ${NML}
# LJG
sed -i 's@^ADECK_DIR =.*@ADECK_DIR = '"${ADECKhafs}"'@g' ${NML}
sed -i 's@^BDECK_DIR =.*@BDECK_DIR = '"${BDECKhafs}"'@g' ${NML}
sed -i 's/^SYS_ENV =.*/SYS_ENV = '"$( echo ${machine} | tr "[a-z]" "[A-Z]")"'/g' ${NML}
sed -i 's/^BATCH_MODE =.*/BATCH_MODE = Background/g' ${NML}

# Initialize ALL_COMPLETE as false
ALL_COMPLETE=0

# Loop until everything is complete (ALL_COMPLETE=1)
while [[ ${ALL_COMPLETE} -eq 0 ]]; do
  echo "Top of loop"

  # Find and parse the ATCF file into an individual file for each storm
  # Do this even for HAFS regional to remove ".all" from file name.
  # Lew.Gramer@noaa.gov 2024-04-27:
  # Ghassan.Alaka@noaa.gov 2024-07-07: Add logic to parse ATCFs for all storms if this is the Fake Storm (storm1)
  #                                    Not doing this yet because it may produce storm-centric graphics similar to GFS (no nest)
  # LJG 2024-09-01: Turn on "all" parsing to get labels in d01 titles, and (take steps to) fix ocean_domain plotting
  # if [ "${STORMNUM}" == "00" ]; then
  #  ${GPLOT_PARSE} ${MODEL:0:4} "all" "all" ${COMgplot} ${COMhafs} ${BDECKhafs} ${SYNDAThafs} ${SIDhafs} 0 "*${CDATE}.${RUN}.trak.atcfunix.all"
  # else
  # ${GPLOT_PARSE} ${MODEL:0:4} ${STORMNUM} ${BASIN} ${COMgplot} ${COMhafs} ${BDECKhafs} ${SYNDAThafs} ${SIDhafs} 0 "*${CDATE}.${RUN}.trak.atcfunix.all"
  # fi
  #if [ "${STORMNUM}" == "00" ]; then
  #  ${GPLOT_PARSE} ${MODEL:0:4} "all" "all" ${COMgplot} ${COMhafs} ${BDECKhafs} ${SYNDAThafs} ${SIDhafs} 0 "*${CDATE}.${RUN}.trak.atcfunix.all"
  #else
  ${GPLOT_PARSE} ${MODEL:0:4} ${STORMNUM} ${BASIN} ${COMgplot} ${COMhafs} ${BDECKhafs} ${SYNDAThafs} ${SIDhafs} 0 "*${CDATE}.${RUN}.trak.atcfunix.all"
  #fi

  # GJA
  # LJG
  # Ghassan.Alaka@noaa.gov 2024-07-06: Add additional parse for the "final" ATCF in CDNOSCRUB.
  #                                    Now, GPLOT will always find a parsed ATCF with the storm name in it --> "beryl02l" instead of "02l"
  ${GPLOT_PARSE} ${MODEL:0:4} ${STORMNUM} ${BASIN} ${COMgplot} ${CDNOSCRUB}/${SUBEXPT} ${BDECKhafs} ${SYNDAThafs} ${SIDhafs} 0 "*${CDATE}.${RUN}.trak.atcfunix"
  # GJA

  # Check the status logs for all GPLOT components.
  # If every log doesn't say "complete", set ALL_COMPLETE=0
  GPLOT_STATUS=( $(find ${WORKgplot} -name "status.*" -exec cat {} \;) )
  ALL_COMPLETE=1
  if [ ! -z "${GPLOT_STATUS[*]}" ]; then
    for GS in ${GPLOT_STATUS[@]}; do
      if [ ${GS} != "complete" ]; then
        ALL_COMPLETE=0
      fi
    done
  else
    ALL_COMPLETE=0
  fi

  # Check that the final HAFS output has been post-processed by atm_post.
  # If not, set ALL_COMPLETE=0
  if [ ! -f ${WORKhafs}/intercom/post/postf${NHR3} ]; then
    ALL_COMPLETE=0
    echo "This file doesn't exist --> ${WORKhafs}/forecast/postf${NHR3}"
    echo "That means the final HAFS output has not been post-processed by atm_post."
  fi

  # Deliver all new and modified graphics to COMhafs/graphics
  ${USHhafs}/rsync-no-vanished.sh -av --no-links --include="*/" --include="*gif" --include="*dat" --include="*structure*txt" --include="*.nc" --exclude="*" ${WORKgplot}/. ${COMgplot}/.

  # If all status logs are complete and the final output has been processed
  # by atm_post, then exit with success!
  # If not, submit the GPLOT wrapper again.
  if [[ ${ALL_COMPLETE} -eq 1 ]]; then
    echo "All status logs say 'complete' and the final output has been processed by atm_post (fhr=${NHRS}). That means we're done!"
    break
  else
    echo "Waiting for all status logs to say 'complete' and for atm_post to process the final output (fhr=${NHRS}). Trying again..."
  fi

  # Call the GPLOT wrapper
  ${GPLOT_WRAPPER} ${NML} &

  # Now sleep for 5 min
  echo "sleeping 300 seconds..."
  sleep 300s
done

# Now that everything is complete, move all graphics to the $COMhafs directory.
if [ "${SENDCOM}" == "YES" ]; then
  ${USHhafs}/rsync-no-vanished.sh -av --no-links --include="*/" --include="*gif" --include="*dat" --include="*structure*txt" --include="*.nc" --exclude="*" ${WORKgplot}/. ${COMgplot}/.
fi

echo "graphics job done"

