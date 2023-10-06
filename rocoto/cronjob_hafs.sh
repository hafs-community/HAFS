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
#./run_hafs.py ${opts} 2020 13L HISTORY config.EXPT=${EXPT} # Laura

# Run specified cycles of a storm
#./run_hafs.py ${opts} 2020082506-2020082512 13L HISTORY \
#   config.EXPT=${EXPT} config.SUBEXPT=${EXPT} # Laura

# Run one cycle of a storm
 ./run_hafs.py ${opts} 2020082512 13L HISTORY config.EXPT=${EXPT}

#===============================================================================

date

echo 'cronjob done'
