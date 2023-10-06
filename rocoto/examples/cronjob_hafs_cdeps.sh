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
# Here are some simple examples, more examples can be seen in cronjob_hafs_rt.sh

# Run data atmosphere with ERA5
 ./run_hafs.py ${opts} 2019082900 00L HISTORY config.EXPT=${EXPT} \
    config.SUBEXPT=${EXPT}_era5 \
    forecast.output_history=.true. \
    ../parm/tests/hafs_regional_static.conf ../parm/tests/hafs_hycom.conf \
    ../parm/tests/hafs_datm.conf ../parm/tests/hafs_datm_era5.conf

# Run data ocean with OISST
 ./run_hafs.py ${opts} 2019082900 00L HISTORY config.EXPT=${EXPT} \
    config.SUBEXPT=${EXPT}_oisst \
    forecast.output_history=.true. \
    ../parm/tests/hafs_regional_static.conf \
    ../parm/tests/hafs_docn.conf ../parm/tests/hafs_docn_oisst.conf

# Run data ocean with GHRSST
 ./run_hafs.py ${opts} 2019082900 00L HISTORY config.EXPT=${EXPT} \
    config.SUBEXPT=${EXPT}_ghrsst \
    forecast.output_history=.true. \
    ../parm/tests/hafs_regional_static.conf \
    ../parm/tests/hafs_docn.conf ../parm/tests/hafs_docn_ghrsst.conf

#===============================================================================

date

echo 'cronjob done'
