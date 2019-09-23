#!/bin/sh
set -x
date

cd /lfs3/projects/hwrfv3/${USER}/hafs3/rocoto

dev="-s sites/xjet.ent -f"

#./run_hafs.py ${dev} 2018 06L HISTORY # Florence
 ./run_hafs.py ${dev} -t 2019042200 00L HISTORY config.EXPT=hafs3 config.SUBEXPT=HAFS_jet ../parm/hafs_regional_static.conf

date
echo 'cronjob done'

