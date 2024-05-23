#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/work/noaa/hwrf/tutorial/save/${USER}/hafs_202405}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"

#===============================================================================
 # Regional standalone storm-focused configuration with ESG grid and GFS grib2ab input
 # Hurricane Ida 09L2021
 ./run_hafs.py ${opts} 2021082712 09L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_tutorial_test0 \
     config.NHRS=12 config.scrub_work=no config.scrub_com=no config.archive=none \
     ../parm/rt_conf/hafs_regional_atm.conf

#===============================================================================

date

echo 'cronjob done'
