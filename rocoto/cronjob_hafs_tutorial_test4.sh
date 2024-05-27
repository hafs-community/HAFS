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
 # HAFS tutorial test 4: HAFSv2A configuration with dev computation resources
 # and without wave coupling for 72-h forecast
 # Hurricane Ida 09L2021
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_tutorial_test4 ../parm/hfsa_dev.conf"
 ./run_hafs.py ${opts} 2021082712 09L HISTORY ${confopts} ${scrubopt} config.NHRS=72 config.run_emcgraphics=yes

## HAFS tutorial test 4a: HAFSv2A configuration with dev computation resources
## and with one-way wave coupling
## Hurricane Ida 09L2021
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_tutorial_test4a ../parm/hfsa_dev_ww3.conf"
#./run_hafs.py ${opts} 2021082712-2021082718 09L HISTORY ${confopts} config.run_emcgraphics=yes

## HAFS tutorial test 4b: HAFSv2B configuration with dev computation resources
## Hurricane Ida 09L2021
#confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_tutorial_test4b ../parm/hfsb_dev.conf"
#./run_hafs.py ${opts} 2021082712-2021082718 09L HISTORY ${confopts} config.run_emcgraphics=yes

#===============================================================================

date

echo 'cronjob done'
