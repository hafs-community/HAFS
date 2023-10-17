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


 ./run_hafs.py ${opts} 2020091400 19L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hafsb_regional_3mvnest_storm \
     config.domlat=20.0 config.domlon=-65.0 \
     config.NHRS=96 ${scrubopt} \
     ../parm/hafsb_regional_3mvnest_storm.conf

 ./run_hafs.py ${opts} 2020091400 19L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hafsb_regional_2mvnest_storm \
     config.domlat=20.0 config.domlon=-65.0 \
     config.NHRS=96 ${scrubopt} \
     ../parm/hafsb_regional_2mvnest_storm.conf

 ./run_hafs.py ${opts} 2020091400 19L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_hafsb_regional_1mvnest_storm \
     config.domlat=20.0 config.domlon=-65.0 \
     config.NHRS=96 ${scrubopt} \
     ../parm/hafsb_regional_1mvnest_storm.conf


#===============================================================================

date

echo 'cronjob done'
