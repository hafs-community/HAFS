#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/work/noaa/hwrf/tutorial/save/${USER}/hafs-ncmrwf}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no config.archive=none"

#===============================================================================
 # HAFS tutorial test 2: Regional coarse-resolution, moving-nesting,
 # atmosphere-only configuration with vortex initialization and ocean coupling
 # Mocha 01B2023
 ./run_hafs.py ${opts} 2023051106 01B HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_tutorial_test2 \
     config.NHRS=12 ${scrubopt} config.run_emcgraphics=yes \
     ../parm/tutorial/hafs_tutorial_test2.conf

## HAFS tutorial test 2a: Regional coarse-resolution, moving-nesting,
## atmosphere-only configuration with vortex initialization, data assimilation
## and ocean coupling
## Remal 01B2024
#./run_hafs.py ${opts} 2024052512 01B HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_tutorial_test2a \
#    config.NHRS=12 ${scrubopt} config.run_emcgraphics=yes \
#    ../parm/tutorial/hafs_tutorial_test2a.conf

#===============================================================================

date

echo 'cronjob done'
