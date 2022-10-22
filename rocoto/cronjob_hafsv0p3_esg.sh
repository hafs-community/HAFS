#!/bin/sh
set -x
date

# NOAA WCOSS2
 HOMEhafs=/lfs/h2/emc/hur/noscrub/${USER}/save/HAFS
 dev="-s sites/wcoss2.ent -f"
 PYTHON3=/usr/bin/python3

# NOAA RDHPCS Jet
#HOMEhafs=/mnt/lfs4/HFIP/hwrfv3/${USER}/HAFS
#dev="-s sites/xjet.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

# MSU Orion
#HOMEhafs=/work/noaa/hwrf/save/${USER}/HAFS
#dev="-s sites/orion.ent -f"
#PYTHON3=/apps/intel-2020/intel-2020/intelpython3/bin/python3

# NOAA RDHPCS Hera
#HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/HAFS
#dev="-s sites/hera.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})
SUBEXPT=${EXPT}
#===============================================================================
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT}_v0p3a_esg \
     config.scrub_com=no config.scrub_work=no \
     ../parm/hafsv0p3a_esg.conf"

 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082512 13L HISTORY ${confopts} # Laura

 confopts="config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT}_v0p3s_esg \
     config.scrub_com=no config.scrub_work=no \
     ../parm/hafsv0p3s_esg.conf"

 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082512 13L HISTORY ${confopts} # Laura

date

echo 'cronjob done'
