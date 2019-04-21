#!/bin/sh
set -x
date

cd /gpfs/hps3/emc/hwrf/noscrub/${USER}/save/HAFS/rocoto

#dev="-f"
dev="-s sites/wcoss_cray.ent -f"

#./run_hafs.py ${dev} 2018 06L HISTORY # Florence
#./run_hafs.py ${dev} 2018083018-2018083100 06L HISTORY config.EXPT=HAFS config.SUBEXPT=HAFS # Florence
#./run_hafs.py -t ${dev} 2019041600-2019041606 00L HISTORY ../parm/hafs_fakestorm.conf # nostorm
#./run_hafs.py -t ${dev} 2019042000 00L HISTORY config.EXPT=HAFS config.SUBEXPT=HAFS_NATL00L ../parm/hafs_fakestorm.conf # real-time static NATL domain
 ./run_hafs.py -t ${dev} 2019041800-2019041900 00L HISTORY \
     config.EXPT=HAFS config.SUBEXPT=HAFS_NATL00L \
     config.scrub_work=no config.scrub_com=no \
     dir.CDSCRUB=/gpfs/hps2/ptmp/{ENV[USER]} \
     ../parm/hafs_fakestorm.conf

date
echo 'cronjob done'
