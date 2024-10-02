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
 # HAFS tutorial test 1: Regional coarse-resolution, moving-nesting,
 # atmosphere-only configuration
 # Mocha 01B2023
 ./run_hafs.py ${opts} 2023051106 01B HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_tutorial_test1 \
     config.NHRS=12 ${scrubopt} config.run_emcgraphics=no \
     ../parm/tutorial/hafs_tutorial_test1.conf

## HAFS tutorial test 1a: Regional coarse-resolution, moving-nesting,
## ocean-coupling configuration
## Mocha 01B2023
#./run_hafs.py ${opts} 2023051106 01B HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_tutorial_test1a \
#    config.NHRS=12 ${scrubopt} config.run_emcgraphics=no \
#    ../parm/tutorial/hafs_tutorial_test1a.conf

#===============================================================================

date

echo 'cronjob done'
