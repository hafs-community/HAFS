#!/bin/sh
set -x
date

cd /gpfs/hps3/emc/hwrf/noscrub/${USER}/save/HAFS/rocoto

#dev="-f"
dev="-s sites/wcoss_cray.ent -f"

# Run a storm by using relocatable regional standalone domain configuration
# The 7th tile for the regional domain (2560x2160) sits at the center of the 6th tile.
# Domain center and output grids are automatically determined
# Real storm
#./run_hafs.py ${dev} 2018 06L HISTORY # Florence
#./run_hafs.py ${dev} 2018083018-2018083100 06L HISTORY config.EXPT=HAFS config.SUBEXPT=HAFS # Florence

# Fake storm (e.g., NATL00L)
# Run a real-time experiment (fakestorm, e.g., NATL00L) with the regional standalone domain configuration (using the default domain size configuration)
#./run_hafs.py -t ${dev} 2019042000 00L HISTORY config.EXPT=HAFS config.SUBEXPT=HAFS_NATL00L ../parm/hafs_fakestorm.conf # real-time static NATL domain

# Another example
#./run_hafs.py -t ${dev} 2019042200 00L HISTORY \
#    config.EXPT=HAFS config.SUBEXPT=HAFS_regional \
#    config.scrub_work=no config.scrub_com=no \
#    dir.CDSCRUB=/gpfs/hps2/ptmp/{ENV[USER]} \
#    ../parm/hafs_fakestorm.conf

# Run a storm by using HAFSv0.0A regional standalone domain and output configurations
# The static regional domain (2880x1920) is slightly offcenter of the 6th tile.  
# Real storm
#./run_hafs.py ${dev} 2018083018-2018083100 06L ../parm/hafs_regional_static.conf
# Fake storm (e.g., NATL00L)
#./run_hafs.py -t ${dev} 2019042000 00L HISTORY ../parm/hafs_regional_static.conf

date
echo 'cronjob done'
