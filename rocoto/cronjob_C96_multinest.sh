#!/bin/sh
set -x
date

#HOMEhafs=${HOMEhafs:-/work/noaa/hwrf/save/bliu/hafsv1_merge}
HOMEhafs=/work2/noaa/aoml-hafs1/wramstro/basin/HAFS

source ${HOMEhafs}/ush/hafs_pre_job.sh.inc

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"
scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================
# Example hafs moving nest experiments

# This is called in cronjob_hafs_mvnest.sh
# ./run_hafs.py ${opts} 2020082512 13L HISTORY \
#     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96_regional_1mvnest_storm \
#     config.domlat=23.3 config.domlon=-86.3 \
#     config.NHRS=12 ${scrubopt} \
#     ../parm/hafs_C96_regional_1mvnest_storm.conf

 ./run_hafs.py ${opts} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96_regional_2mvnest_storm \
     config.domlat=23.3 config.domlon=-86.3 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_C96_regional_2mvnest_storm.conf

 ./run_hafs.py ${opts} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96_regional_2staticmvnest_storm \
     config.domlat=23.3 config.domlon=-86.3 \
     config.NHRS=72 ${scrubopt} \
     ../parm/hafs_C96_regional_2staticmvnest_storm.conf

 ./run_hafs.py ${opts} 2020091400 17L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C96_regional_3mvnest_storm \
     config.domlat=23.3 config.domlon=-86.3 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_C96_regional_3mvnest_storm.conf



#===============================================================================

date

echo 'cronjob done'
