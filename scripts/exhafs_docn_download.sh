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

EDATE=$( date -d "${CDATE:0:4}-${CDATE:4:2}-${CDATE:6:2}t${CDATE:8:2}:00:00+00 +$NHRS hours" +%Y%m%d%H )

[ -d "$DOCNdir" ] || mkdir "$DOCNdir"
test -d "$DOCNdir"
cd "$DOCNdir"

echo "Downloading OISST files."
echo "Start date: ${CDATE:0:8}"
echo "End date: ${EDATE:0:8}"
echo "Destination dir: \"$DOCNdir\""

"$USHhafs/hafs_docn_download.py" ${CDATE:0:8}-${EDATE:0:8}

echo "Successfully downloaded all OISST files."
echo "Enjoy your files and have a nice day."
