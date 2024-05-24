#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/work2/noaa/hwrf/tutorial/save/${USER}/hafs_202405}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no config.archive=none"

#===============================================================================
 # HAFS tutorial test 2: Regional coarse-resolution, moving-nesting,
 # atmosphere-only configuration with HFSB physics
 # Hurricane Ida 09L2021
 ./run_hafs.py ${opts} 2021082712 09L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_tutorial_test2 \
     config.NHRS=12 ${scrubopt} config.run_emcgraphics=no \
     ../parm/tutorial/hafs_tutorial_test2.conf

## HAFS tutorial test 2a: Regional coarse-resolution, moving-nesting,
## atmosphere-only configuration with HFSB physics, and turn on progsigma in saSAS CP
## Hurricane Ida 09L2021
#./run_hafs.py ${opts} 2021082712 09L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_tutorial_test2a \
#    config.NHRS=12 ${scrubopt} config.run_emcgraphics=no \
#    ../parm/tutorial/hafs_tutorial_test2.conf forecast.progsigma=.false.

#===============================================================================

date

echo 'cronjob done'
