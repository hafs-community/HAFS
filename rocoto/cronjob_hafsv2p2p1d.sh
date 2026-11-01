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
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_v2p2p1d_p1 ../parm/hafsv2p2p1d.conf"
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

#-------------------------------------------------------------------------------
# HAFSv2.2.1D prototype 3
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_v2p2p1d_p3 ../parm/hafsv2p2p1d_p3.conf"
# One storm: 13L
#./run_hafs.py ${opts} 2025102300-2025102306 00L HISTORY ${confopts} \
#   config.NHRS=12 config.scrub_work=no config.scrub_com=no
# Three storms: 07L, 08L, 09L
#./run_hafs.py ${opts} 2025092512-2025092518 00L HISTORY ${confopts} \
#   config.NHRS=12 config.scrub_work=no config.scrub_com=no

#===============================================================================
# 2025 NATL slots
#./run_hafs.py ${opts} 2025062000-2025081000 00L HISTORY ${confopts} # slot 1 (01-04L)
#./run_hafs.py ${opts} 2025080800-2025091518 00L HISTORY ${confopts} # slot 2 (05-06L)
#./run_hafs.py ${opts} 2025091400-2025101800 00L HISTORY ${confopts} # slot 3 (07-12L)
#./run_hafs.py ${opts} 2025101600-2025103118 00L HISTORY ${confopts} # slot 4 (13L)

# 2024 NATL slots
#./run_hafs.py ${opts} 2024062600-2024071100 00L HISTORY ${confopts} # slot 1 (02-03L)
#./run_hafs.py ${opts} 2024071000-2024080118 00L HISTORY ${confopts} # slot 1.1
#./run_hafs.py ${opts} 2024080100-2024092118 00L HISTORY ${confopts} # slot 2 (04-08L)
#./run_hafs.py ${opts} 2024092100-2024101518 00L HISTORY ${confopts} # slot 3 (09-14L)
#./run_hafs.py ${opts} 2024101500-2024111818 00L HISTORY ${confopts} # slot 4 (15-19L)

#===============================================================================

date

echo 'cronjob done'
