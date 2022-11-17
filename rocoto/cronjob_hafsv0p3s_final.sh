#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"

#===============================================================================
 # atm_init+atm_vi+fgat+d02_3denvar+anal_merge and cycling storm perturbation
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_v0p3s_final \
     ../parm/hafsv0p3s_final.conf"

 # Technical testing
#./run_hafs.py ${opts} 2020082506-2020082512 13L HISTORY ${confopts} \
#    config.NHRS=12 config.scrub_work=no config.scrub_com=no
#===============================================================================

date

echo 'cronjob done'
