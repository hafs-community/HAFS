#!/bin/bash

if [[ "$run_docn" != yes || ! "$docn_download_jobs" -gt 0 ]] ; then
    echo "This job should only be run if \$run_docn=yes and \$docn_download_jobs>0"
    echo "  \$docn_download_jobs=\"$docn_download_jobs\""
    echo "  \$run_docn=\"$run_docn\""
    echo "Beware! You may anger Poseidon by misusing this script. Avoid coastlines."
    echo " -> SCRIPT IS EXITING BECAUSE THIS JOB SHOULD NOT BE RUN <- "
    exit 0
fi

set -uxe

HOMEhafs=${HOMEhafs:-/gpfs/hps3/emc/hwrf/noscrub/${USER}/save/HAFS}
WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
USHhafs=${USHhafs:-${HOMEhafs}/ush}
CDATE=${CDATE:-${YMDH}}
NHRS=${NHRS:?NHRS is not set}
DOCNdir=${DOCNdir:?DOCNdir is not set}

export TZ=UTC # Orion workaround

M1DATE=$( date -d "${CDATE:0:4}-${CDATE:4:2}-${CDATE:6:2}t${CDATE:8:2}:00:00+00 -24 hours" +%Y%m%d%H )
P1DATE=$( date -d "${CDATE:0:4}-${CDATE:4:2}-${CDATE:6:2}t${CDATE:8:2}:00:00+00 +$(( NHRS+24 )) hours" +%Y%m%d%H )

[ -d "$DOCNdir" ] || mkdir -p "$DOCNdir"
test -d "$DOCNdir"
cd "$DOCNdir"

echo "Downloading OISST files."
echo "Start date: ${M1DATE:0:8}"
echo "End date: ${P1DATE:0:8}"
echo "Destination dir: \"$DOCNdir\""

docn_source=${DOCN_SOURCE:-OISST}

if [[ "$docn_source" == OISST ]] ; then
    "$USHhafs/hafs_oisst_download.py" ${M1DATE:0:8}-${P1DATE:0:8}
elif [[ "${docn_source}" == RTOFS ]] ; then
    "$USHhafs/hafs_rtofs_download.py" ${CDATE:0:8}
elif [[ "${docn_source}" == GHRSST ]] ; then
    "$USHhafs/hafs_ghrsst_download.py" ${M1DATE:0:8}-${P1DATE:0:8}
else
    echo "ERROR: Unknown data ocean source $docn_source. Giving up." 2>&1
    echo " -> SCRIPT IS FAILING BECAUSE OF INVALID \$DOCN_SOURCE VALUE <- "
    exit 1
fi

echo "Successfully downloaded all DOCN $docn_source files."
echo "Enjoy your files and have a nice day."
