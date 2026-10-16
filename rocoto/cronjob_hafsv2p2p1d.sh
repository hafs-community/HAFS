#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
#opts="-t -s sites/${WHERE_AM_I:-wcoss2}.ent -f"
opts="-t -f"
#===============================================================================
# HAFSv2.2.1D prototype 1
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_v2p2p1d ../parm/hafsv2p2p1d.conf"
#./run_hafs.py ${opts} 2025102300-2025102306 00L HISTORY ${confopts} \
#   config.NHRS=12 config.scrub_work=no config.scrub_com=no

#-------------------------------------------------------------------------------
# HAFSv2.2.1D prototype 2
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_v2p2p1d_p2 ../parm/hafsv2p2p1d_p2.conf"
# One storm: 13L
#./run_hafs.py ${opts} 2025102300-2025102306 00L HISTORY ${confopts} \
#   config.NHRS=12 config.scrub_work=no config.scrub_com=no
# Three storms: 07L, 08L, 09L
#./run_hafs.py ${opts} 2025092512-2025092518 00L HISTORY ${confopts} \
#   config.NHRS=12 config.scrub_work=no config.scrub_com=no

#===============================================================================
# 2025 NATL example
#./run_hafs.py ${opts} 2025101600-2025103118 00L HISTORY ${confopts}

#===============================================================================

date

echo 'cronjob done'
