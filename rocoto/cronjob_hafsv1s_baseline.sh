#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})
SUBEXPT=${EXPT}
#===============================================================================
#opts="-t -s sites/${WHERE_AM_I:-wcoss2}.ent -f"
 opts="-t -f"

 #hafsv1a baseline
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT}_v1s_bl_test \
     config.scrub_com=no config.scrub_work=no \
     ../parm/hafsv1s_baseline.conf"
 ./run_hafs.py ${opts} 2020082512 13L HISTORY ${confopts} # Laura

date

echo 'cronjob done'
