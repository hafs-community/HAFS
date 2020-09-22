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
#HOMEhafs=/mnt/lfs1/HFIP/hwrfv3/${USER}/HAFS
#dev="-s sites/xjet.ent -f"
#dev="-s sites/xjet_hafsv0p1a.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

# MSU Orion
#HOMEhafs=/work/noaa/hwrf/save/${USER}/HAFS
#dev="-s sites/orion.ent -f"
#PYTHON3=/apps/intel-2020/intel-2020/intelpython3/bin/python3

# NOAA RDHPCS Hera
 HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/hafs_vigsi_202008
 dev="-s sites/hera.ent -f"
 PYTHON3=/apps/intel/intelpython3/bin/python3

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})
scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================

 ${PYTHON3} ./run_hafs.py -t ${dev} 2019082900-2019082906 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_regional_da_C96s1n4_320x312 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C96s1n4_320x312.conf

#===============================================================================

date

echo 'cronjob done'
