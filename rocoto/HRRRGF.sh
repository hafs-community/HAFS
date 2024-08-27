#! /bin/bash
set -ue

HOMEhafs=$( cd $( dirname ${BASH_SOURCE[0]} ) ; cd .. ; pwd -P )
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc
set -x

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
SUBEXPT=HRRRGF
conf=../parm/hafs_hrrr_gf.conf
opts="-t -f"

scrubopt="config.scrub_work=no config.scrub_com=no"
./run_hafs.py ${opts} 2023082618-2023090218 10L HISTORY \
    config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT} \
    config.NHRS=24 ${scrubopt} \
    ../parm/hafs_hrrr_gf.conf \
    forecast.write_tasks_per_group=240 \
    forecast.atm_tasks=1440 \
    forecast.all_tasks=1560 \
    gsi.use_bufr_nr=yes
