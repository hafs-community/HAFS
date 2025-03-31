#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"

#===============================================================================
# Here are some simple examples, more examples can be seen in cronjob_hafs_rt.sh

# Run all cycles of a storm
#./run_hafs.py ${opts} 2024 09L HISTORY config.EXPT=${EXPT} # Helene

# Run specified cycles of a storm
#./run_hafs.py ${opts} 2024092406-2024092412 09L HISTORY \
#   config.EXPT=${EXPT} config.SUBEXPT=${EXPT} # Helene

# Run one cycle of a storm
 ./run_hafs.py ${opts} 2024092412 09L HISTORY config.EXPT=${EXPT}

#===============================================================================

date

echo 'cronjob done'
