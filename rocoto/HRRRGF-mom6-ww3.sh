#! /bin/bash
set -ue

HOMEhafs=$( cd $( dirname ${BASH_SOURCE[0]} ) ; cd .. ; pwd -P )
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc
set -x

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
SUBEXPT=HRRRGF-mom6-ww3
conf=../parm/hafs_hrrr_gf.conf
opts="-t -f"

first_test_works='2023082618-2023082700 10L'
breaks_noahmp='2020082506-2020082618 13L'

scrubopt="config.scrub_work=no config.scrub_com=no"
./run_hafs.py ${opts} ${breaks_noahmp} HISTORY \
    config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT} \
    config.NHRS=24 ${scrubopt} \
    ../parm/hafs_hrrr_gf.conf \
    forecast.atm_tasks=1200 \
    forecast.all_tasks=1320 \
    gsi.use_bufr_nr=yes \
    config.run_wave=yes \
    config.run_ocean=yes
