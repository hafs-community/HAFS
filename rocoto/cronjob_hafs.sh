#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/work2/noaa/hwrf/tutorial/save/${USER}/hafs_202405}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"

#===============================================================================
# Here are some simple examples, more examples can be seen in cronjob_hafs_rt.sh

# Run all cycles of a storm
#./run_hafs.py ${opts} 2021 09L HISTORY config.EXPT=${EXPT} # Ida

# Run specified cycles of a storm
#./run_hafs.py ${opts} 2021082712-2021082718 09L HISTORY \
#   config.EXPT=${EXPT} config.SUBEXPT=${EXPT} # Ida

# Run one cycle of a storm
 ./run_hafs.py ${opts} 2021082712 09L HISTORY config.EXPT=${EXPT} # Ida

#===============================================================================

date

echo 'cronjob done'
