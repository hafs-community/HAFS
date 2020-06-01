#!/bin/sh
set -x
date

# NOAA WCOSS Dell Phase3
#HOMEhafs=/gpfs/dell2/emc/modeling/noscrub/${USER}/save/HAFS
#dev="-s sites/wcoss_dell_p3.ent -f"
#PYTHON3=/usrx/local/prod/packages/python/3.6.3/bin/python3

# NOAA WCOSS Cray
#HOMEhafs=/gpfs/hps3/emc/hwrf/noscrub/${USER}/save/HAFS
#dev="-s sites/wcoss_cray.ent -f"
#PYTHON3=/opt/intel/intelpython3/bin/python3

# NOAA RDHPCS Jet
HOMEhafs=/mnt/lfs4/HFIP/hwrfv3/${USER}/hafs_multinests
dev="-s sites/xjet.ent -f"
PYTHON3=/apps/intel/intelpython3/bin/python3

# NOAA RDHPCS Hera
#HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/HAFS
#dev="-s sites/hera.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})
scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================

 ${PYTHON3} ./run_hafs.py -t ${dev} 2019091600 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest2_static \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_globnest2_static.conf

 ${PYTHON3} ./run_hafs.py -t ${dev} 2019091600 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest1_static \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_globnest_static.conf

#===============================================================================

date

echo 'cronjob done'
