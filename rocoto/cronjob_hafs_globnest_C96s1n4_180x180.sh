#!/bin/sh
set -x
date

cd /lfs3/projects/hwrfv3/${USER}/HAFS_develop/rocoto

dev="-s sites/xjet.ent -f"

./run_hafs.py ${dev} -t 2019042200 00L HISTORY \
    config.EXPT=HAFS_develop config.SUBEXPT=hafs_globnest_C96s1n4_180x180 \
	../parm/examples/hafs_globnest_C96s1n4_180x180.conf \
	../parm/hafs_fakestorm.conf

date
echo 'cronjob done'

