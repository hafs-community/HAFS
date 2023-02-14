#!/bin/sh --login

set -xe

TOTAL_TASKS=${TOTAL_TASKS:-144}
NCTSK=${NCTSK:-24}
NCNODE=${NCNODE:-24}
OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
APRUNS=${APRUNS:-"aprun -b -j1 -n1 -N1 -d1 -cc depth"}
APRUNF=${APRUNF:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth cfp"}
APRUNC=${APRUNC:-"aprun -b -j1 -n${TOTAL_TASKS} -N${NCTSK} -d${OMP_NUM_THREADS} -cc depth"}

export MP_LABELIO=yes

CDATE=${CDATE:-${YMDH}}
NHRS=${NHRS:-126}
NHR3=$( printf "%03d" "$NHRS" )
NOUTHRS=${NOUTHRS:-3}
machine=${machine:-jet}

MPISERIAL=${MPISERIAL:-mpiserial}
NDATE=${NDATE:-ndate}
WGRIB2=${WGRIB2:-wgrib2}
GRB2INDEX=${GRB2INDEX:-grb2index}

WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
COMhafs=${COMhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/com/${CDATE}/${STORMID}}
SENDCOM=${SENDCOM:-YES}
CDNOSCRUB="${CDNOSCRUB}"

STORMID=${STORMID:-00L}
STORMNUM=$( echo ${STORMID} | cut -c1-2)
BASIN=${pubbasin2:-AL}

out_prefix=${out_prefix:-$(echo "${STORMID,,}.${CDATE}")}

# GPLOT-specific variables (might go elsewhere)
GPLOT_PARSE="${GPLOThafs}/ush/parse_atcf.sh"
GPLOT_WRAPPER="${GPLOThafs}/sorc/GPLOT/scripts/GPLOT_wrapper.sh"
GPLOT_ARCHIVE="${GPLOThafs}/archive/GPLOT_tarballer.sh"
ADECKhafs=${ADECKhafs:-/lfs1/HFIP/hur-aoml/Ghassan.Alaka/adeck/NHC}
BDECKhafs=${BDECKhafs:-/lfs1/HFIP/hur-aoml/Ghassan.Alaka/bdeck}
SYNDAThafs=${SYNDAThafs:-/lfs4/HFIP/hwrf-data/hwrf-input/SYNDAT-PLUS}
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
if [ ! -f ${NML} ];
then
    if [ -f ${GPLOThafs}/parm/namelist.master.${SUBEXPT} ];
    then
        cp -p ${GPLOThafs}/parm/namelist.master.${SUBEXPT} ${NML}
    else
        cp -p ${GPLOThafs}/parm/namelist.master.HAFS_Default ${NML}
    fi
fi
sed -i 's/^EXPT =.*/EXPT = '"${SUBEXPT}"'/g' ${NML}
sed -i 's/^IDATE =.*/IDATE = '"${CDATE}"'/g' ${NML}
sed -i 's/^SID =.*/SID = '"${STORMID}"'/g' ${NML}
sed -i 's/^INIT_HR =.*/INIT_HR = 0/g' ${NML}
sed -i 's/^FNL_HR =.*/FNL_HR = '"${NHRS}"'/g' ${NML}
sed -i 's@^OCEAN_DIR =.*@OCEAN_DIR = '"${COMhafs}"'@g' ${NML}
sed -i 's@^IDIR =.*@IDIR = '"${COMhafs}"'@g' ${NML}
sed -i 's@^ODIR =.*@ODIR = '"${WORKgplot}"'@g' ${NML}
sed -i 's@^ATCF1_DIR =.*@ATCF1_DIR = '"${COMgplot}"'@g' ${NML}
sed -i 's@^ATCF2_DIR =.*@ATCF2_DIR = '"${CDNOSCRUB}/${SUBEXPT}"'@g' ${NML}
sed -i 's@^ADECK_DIR =.*@ADECK_DIR = '"${ADECKhafs}"'@g' ${NML}
sed -i 's@^BDECK_DIR =.*@BDECK_DIR = '"${BDECKhafs}"'@g' ${NML}
sed -i 's/^SYS_ENV =.*/SYS_ENV = '"$( echo ${machine} | tr "[a-z]" "[A-Z]")"'/g' ${NML}
sed -i 's/^BATCH_MODE =.*/BATCH_MODE = Background/g' ${NML}

# Initialize ALL_COMPLETE as false
ALL_COMPLETE=0

# Loop until everything is complete (ALL_COMPLETE=1)
while [[ ${ALL_COMPLETE} -eq 0 ]];
do
    echo "Top of loop"

    # Find and parse the ATCF file into an individual file for each storm
    # Do this even for HAFS regional to remove ".all" from file name.
    ${GPLOT_PARSE} HAFS ${STORMNUM} ${BASIN} ${COMgplot} ${COMhafs} ${BDECKhafs} ${SYNDAThafs} ${SIDhafs} 0 "*${DATE}.${RUN}.trak.atcfunix.all"

    # Check the status logs for all GPLOT components.
    # If every log doesn't say "complete", set ALL_COMPLETE=0
    GPLOT_STATUS=( $(find ${WORKgplot} -name "status.*" -exec cat {} \;) )
    ALL_COMPLETE=1
    if [ ! -z "${GPLOT_STATUS[*]}" ];
    then
        for GS in ${GPLOT_STATUS[@]};
        do
            if [ ${GS} != "complete" ]; then
                ALL_COMPLETE=0
            fi
        done
    else
        ALL_COMPLETE=0
    fi

    # Check that the final HAFS output has been post-processed by atm_post.
    # If not, set ALL_COMPLETE=0
    if [ ! -f ${WORKhafs}/forecast/postf${NHR3} ];
    then
        ALL_COMPLETE=0
        echo "This file doesn't exist --> ${WORKhafs}/forecast/postf${NHR3}"
        echo "That means the final HAFS output has not been post-processed by atm_post."
    fi

    # Deliver all new and modified graphics to COMhafs/graphics
    ${USHhafs}/rsync-no-vanished.sh -av --no-links --include="*/" --include="*gif" --include="*dat" --include="*structure*txt" --exclude="*" ${WORKgplot}/. ${COMgplot}/.
    #rsync -av --include="*.atcfunix" --exclude="*" ${COMhafs}/. ${COMgplot}/.

    # If all status logs are complete and the final output has been processed
    # by atm_post, then exit with success!
    # If not, submit the GPLOT wrapper again.
    if [[ ${ALL_COMPLETE} -eq 1 ]];
    then
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
    #cp -rup ${WORKgplot} ${COMgplot}
    ${USHhafs}/rsync-no-vanished.sh -av --no-links --include="*/" --include="*gif" --include="*dat" --include="*structure*txt" --exclude="*" ${WORKgplot}/. ${COMgplot}/.
fi

# Zip up and move contents to the tape archive. Local scrubbing optional.
# This is experimental and turned OFF for now.
GPLOT_DOARCH="NO"
if [ "${GPLOT_DOARCH}" == "YES" ];
then
    ${GPLOT_ARCHIVE} ${SUBEXPT} ${DATA} /5year/HFIP/hur-aoml/Ghassan.Alaka/GPLOT ${CDATE} NO NO
fi

echo "graphics job done"

exit
