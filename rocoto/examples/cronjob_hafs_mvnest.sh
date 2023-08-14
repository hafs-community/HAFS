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
# Example hafs moving nest experiments

 ./run_hafs.py ${opts} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96_regional_1mvnest_storm \
     config.domlat=23.3 config.domlon=-86.3 \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_C96_regional_1mvnest_storm.conf

 ./run_hafs.py ${opts} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C512_regional_1mvnest_storm \
     config.domlat=23.3 config.domlon=-86.3 \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_C512_regional_1mvnest_storm.conf

 ./run_hafs.py ${opts} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C512_regional_1mvnest_atm_ocn \
     config.domlat=23.3 config.domlon=-86.3 \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_C512_regional_1mvnest_storm.conf \
     ../parm/tests/hafsv0p3_hycom.conf

 ./run_hafs.py ${opts} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C192_global_1mvnest_storm \
     config.domlat=23.3 config.domlon=-86.3 \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_C192_global_1mvnest_storm.conf

 ./run_hafs.py ${opts} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C768_global_1mvnest_storm \
     config.domlat=23.3 config.domlon=-86.3 \
     config.NHRS=12 ${scrubopt} \
     ../parm/tests/hafs_C768_global_1mvnest_storm.conf

#===============================================================================

date

echo 'cronjob done'
