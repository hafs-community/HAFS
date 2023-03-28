#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================
 # ensda_eps: fgat+3denvar+enkf
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_ensda_eps \
     ../parm/tests/hafs_2022_regional_ensda_eps_AL.conf"

 ./run_hafs.py ${opts} 2020082506-2020082518 00L HISTORY ${confopts} \
     config.NHRS=120 config.ENS_SIZE=20 config.scrub_work=no config.scrub_com=no \
     forecast.write_group=1 \
     forecast.write_tasks_per_group=20 \

#===============================================================================

date

echo 'cronjob done'
