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
# HAFSv2.1.1A 2025rt NHC basins
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT} ../parm/hafsv2p1p1a_final.conf \
          config.run_emcgraphics=yes"

#./run_hafs.py ${opts} 2025 01L HISTORY ${confopts}
#./run_hafs.py ${opts} 2025 02L HISTORY ${confopts}

#===============================================================================
# HAFSv2.1.1A 2025rt JTWC basins
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT} ../parm/hafsv2p1p1a_jtwc.conf \
          config.run_emcgraphics=yes"

#./run_hafs.py ${opts} 2025 11W HISTORY ${confopts}
#./run_hafs.py ${opts} 2025 12W HISTORY ${confopts}

#===============================================================================

date

echo 'cronjob done'
