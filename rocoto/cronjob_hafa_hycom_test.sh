#!/bin/sh
set -x
date

ROCOTOhafs=specify_hafs_dir/HAFS/rocoto
cd ${ROCOTOhafs}
EXPT=$(basename $(dirname ${ROCOTOhafs}))
#dev="-f"
dev="-s sites/hera.ent -f"
scrubopt="config.scrub_work=no config.scrub_com=no"

 ./run_hafs.py -t ${dev} 2019082900 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=hafs_couplehycom \
     config.ictype=gfsnemsio config.bctype=gfsgrib2_0p25 \
     ${scrubopt} \
     ../parm/hafs_regional_static.conf

date
echo 'cronjob done'
