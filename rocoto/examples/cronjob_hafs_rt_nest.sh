#!/bin/sh
set -x
date

HOMEhafs=${HOMEhafs:-/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS}
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================

 ./run_hafs.py ${opts} 2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_C96_regional_storm \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_C96_regional_storm.conf

 ./run_hafs.py ${opts} 2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_C96_regional_1nest_storm \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_C96_regional_1nest_storm.conf

 ./run_hafs.py ${opts} 2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_C96_regional_telescopic_2nests_storm \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_C96_regional_telescopic_2nests_storm.conf

 ./run_hafs.py ${opts} 2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_C96_global_1nest_storm \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_C96_global_1nest_storm.conf

 ./run_hafs.py ${opts} 2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_C96_global_multiple_4nests_storm \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_C96_global_multiple_4nests_storm.conf

#===============================================================================

date

echo 'cronjob done'
