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
 # HAFS tutorial test 3: Regional coarse-resolution, moving-nesting,
 # atmosphere-only configuration with vortex initialization and ocean coupling
 # Hurricane Ida 09L2021
 ./run_hafs.py ${opts} 2021082712 09L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_tutorial_test3 \
     config.NHRS=12 ${scrubopt} config.run_emcgraphics=yes \
     ../parm/tutorial/hafs_tutorial_test3.conf

## HAFS tutorial test 3a: Regional coarse-resolution, moving-nesting,
## atmosphere-only configuration with vortex initialization, data assimilation
## and ocean coupling
## Hurricane Ida 09L2021
#./run_hafs.py ${opts} 2021082712 09L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_tutorial_test3a \
#    config.NHRS=12 ${scrubopt} config.run_emcgraphics=yes \
#    ../parm/tutorial/hafs_tutorial_test3a.conf

#===============================================================================

date

echo 'cronjob done'
