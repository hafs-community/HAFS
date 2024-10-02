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
 # HAFS tutorial test 3: HAFSv2A JTWC configuration using dev computation resources with 72-h forecast
 # Mocha 01B2023
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_tutorial_test3 ../parm/hfsa_dev.conf"
 ./run_hafs.py ${opts} 2023051106 01B HISTORY ${confopts} ${scrubopt} config.NHRS=72 config.run_emcgraphics=yes

## HAFS tutorial test 3a: HAFSv2A JTWC configuration with DA and using dev computation resources with 72-h forecast
## Remal 01B2024
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_tutorial_test3a ../parm/tutorial/hafs_tutorial_test3a.conf"
#./run_hafs.py ${opts} 2024052512 01B HISTORY ${confopts} ${scrubopt} config.NHRS=72 config.run_emcgraphics=yes

#===============================================================================

date

echo 'cronjob done'
