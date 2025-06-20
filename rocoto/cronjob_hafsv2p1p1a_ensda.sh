#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
#opts="-t -s sites/${WHERE_AM_I:-wcoss2}.ent -f"
#opts="-t -f"
opts="-t -s sites/ursa_hafsv2p1p1.ent -f"
#===============================================================================
# 
## Technical testing for Helene 09L2024
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hfsa_ensda ../parm/hafsv2p1p1a_ensda.conf "
 ./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY ${confopts} \
    config.NHRS=126 config.scrub_work=no config.scrub_com=no config.run_emcgraphics=yes
#===============================================================================

date

echo 'cronjob done'
