#!/bin/bash

if [[ "$run_datm" != yes || ! "$datm_download_jobs" -gt 0 ]] ; then
    echo "This job should only be run if \$run_datm=yes and \$datm_download_jobs>0"
    echo "  \$datm_download_jobs=\"$datm_download_jobs\""
    echo "  \$run_datm=\"$run_datm\""
    echo "Billions of electrons have whizzed by, the universe's entropy has increased, and yet nothing has been accomplished."
    echo " -> SCRIPT IS EXITING BECAUSE THIS JOB SHOULD NOT BE RUN <- "
    exit 0
fi

if ( ! which cdo ) ; then
    echo "The \"cdo\" program is not in your \$PATH. This script uses cdo to" 1>&2
    echo "reverse latitudes. Go get cdo." 1>&2
    echo " -> SCRIPT IS EXITING BECAUSE cdo IS NOT IN \$PATH <- " 1>&2
    exit 1
fi

set -uxe

HOMEhafs=${HOMEhafs:-/gpfs/hps3/emc/hwrf/noscrub/${USER}/save/HAFS}
WORKhafs=${WORKhafs:-/gpfs/hps3/ptmp/${USER}/${SUBEXPT}/${CDATE}/${STORMID}}
USHhafs=${USHhafs:-${HOMEhafs}/ush}
CDATE=${CDATE:-${YMDH}}
NHRS=${NHRS:?NHRS is not set}
DATMdir=${DATMdir:?DATMdir is not set}

export TZ=UTC # Orion workaround

M1DATE=$( date -d "${CDATE:0:4}-${CDATE:4:2}-${CDATE:6:2}t${CDATE:8:2}:00:00+00 -24 hours" +%Y%m%d%H )
P1DATE=$( date -d "${CDATE:0:4}-${CDATE:4:2}-${CDATE:6:2}t${CDATE:8:2}:00:00+00 +$(( NHRS+24 )) hours" +%Y%m%d%H )

[ -d "$DATMdir" ] || mkdir "$DATMdir"
test -d "$DATMdir"
cd "$DATMdir"

echo "Downloading ERA5 files and reversing latitudes."
echo "Start date: ${M1DATE:0:8}"
echo "End date: ${P1DATE:0:8}"
echo "Destination dir: \"$DATMdir\""

datm_source=${DATM_SOURCE:-ERA5}

if [[ "$datm_source" == ERA5 ]] ; then
    "$USHhafs/hafs_era5_download.py" ${M1DATE:0:8}-${P1DATE:0:8}
else
    echo "ERROR: Unknown data atmosphere source $datm_source. Giving up." 2>&1
    echo " -> SCRIPT IS FAILING BECAUSE OF INVALID \$DATM_SOURCE VALUE <- "
    exit 1
fi


echo "Successfully downloaded all DATM $datm_source files."
echo "Enjoy your files and have a nice day."
