#! /bin/bash
set -ue

HOMEhafs=$( cd $( dirname ${BASH_SOURCE[0]} ) ; cd .. ; pwd -P )
source ${HOMEhafs}/ush/hafs_pre_job.sh.inc
set -x

cd ${HOMEhafs}/rocoto
EXPT=$(basename ${HOMEhafs})
opts="-t -f"
#scrubopt="config.scrub_work=yes config.scrub_com=no"

scrubopt="config.scrub_work=yes config.scrub_com=no"

lee_part1='2023090512-2023091606 13L'
lee_part2='2023091618-2023091712 13L'

./run_hafs.py ${opts} ${lee_part1} HISTORY \
    config.EXPT=${EXPT} config.SUBEXPT=hafs-thompson-aero-02 \
    config.NHRS=126 ${scrubopt} \
    ../parm/hfsa_thompson_aero.conf \
    gsi.use_bufr_nr=yes \
    config.run_emcgraphics=yes \
    config.RUN=haro

./run_hafs.py ${opts} ${lee_part1} HISTORY \
    config.EXPT=${EXPT} config.SUBEXPT=hfsa-01 \
    config.NHRS=126 ${scrubopt} \
    ../parm/hfsa.conf \
    gsi.use_bufr_nr=yes \
    config.run_emcgraphics=yes

./run_hafs.py ${opts} ${lee_part1} HISTORY \
    config.EXPT=${EXPT} config.SUBEXPT=hafs-rrfs-coupled-02 \
    config.NHRS=126 ${scrubopt} \
    ../parm/hafs_hrrr_gf.conf \
    forecast.atm_tasks=1200 \
    forecast.all_tasks=1320 \
    gsi.use_bufr_nr=yes \
    config.run_wave=yes \
    config.run_ocean=yes \
    config.run_emcgraphics=yes \
    config.RUN=hrfs
