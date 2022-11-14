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

 #hafsv0.3a with regional esg grid
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT}_v0p3a_esg \
     config.scrub_com=no config.scrub_work=no \
     ../parm/hafsv0p3a_esg.conf"
 ./run_hafs.py ${opts} 2020082512 13L HISTORY ${confopts} # Laura

 #hafsv0.3a with regional esg grid and automatically determined parent and moving nest domain locations
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT}_v0p3a_esg_auto \
     config.scrub_com=no config.scrub_work=no \
     ../parm/hafsv0p3a_esg_auto.conf"
 ./run_hafs.py ${opts} 2020082512 13L HISTORY ${confopts} # Laura

 #hafsv0.3s with regional esg grid
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT}_v0p3s_esg \
     config.scrub_com=no config.scrub_work=no \
     ../parm/hafsv0p3s_esg.conf"
 ./run_hafs.py ${opts} 2020082512 13L HISTORY ${confopts} # Laura

date

echo 'cronjob done'
