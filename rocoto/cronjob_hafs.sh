#!/bin/sh
set -x
date

#dev="-f"
dev="-s sites/wcoss_cray.ent -f"

#./run_hafs.py ${dev} 2018 06L HISTORY # Florence
#./run_hafs.py ${dev} 2018083018-2018083100 06L HISTORY config.EXPT=HAFS config.SUBEXPT=HAFS # Florence
#./run_hafs.py -t ${dev} 2018100100 00L HISTORY # nostorm
#./run_hafs.py -t ${dev} 2019041600-2019041606 00L HISTORY ../parm/hafs_fakestorm.conf # nostorm
#./run_hafs.py -t ${dev} 2019041600-2019041606 00L HISTORY  # nostorm
 ./run_hafs.py -t ${dev} 2019041600 00L HISTORY  # nostorm

date
echo 'cronjob done'
