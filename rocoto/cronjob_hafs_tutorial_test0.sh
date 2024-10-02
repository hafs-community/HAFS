#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/work/noaa/hwrf/tutorial/save/${USER}/hafs-ncmrwf}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"

#===============================================================================
 # Regional standalone storm-focused configuration with ESG grid and GFS grib2ab input
 # Mocha 01B2023
 ./run_hafs.py ${opts} 2023051106 01B HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_tutorial_test0 \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no config.archive=none \
     ../parm/tutorial/hafs_tutorial_test0.conf

#===============================================================================

date

echo 'cronjob done'
