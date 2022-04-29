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
#HOMEhafs=/mnt/lfs4/HFIP/hwrfv3/${USER}/HAFS
#dev="-s sites/xjet.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

# MSU Orion
#HOMEhafs=/work/noaa/hwrf/save/${USER}/hafsv0p3_20220305
#dev="-s sites/orion.ent -f"
#PYTHON3=/apps/intel-2020/intel-2020/intelpython3/bin/python3

# NOAA RDHPCS Hera
 HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/hafsv0p3_20220322
 dev="-s sites/hera.ent -f"
 PYTHON3=/apps/intel/intelpython3/bin/python3

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})

#===============================================================================

 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_conus_3km \
     config.NHRS=60 config.scrub_work=no config.scrub_com=no \
     ../parm/hafsv0p3_regional_conus_3km.conf"

# Iowa Derecho case: 2020/08/10/00 or 2020/08/10/06. The 06Z cycle performed slightly better.
#${PYTHON3} ./run_hafs.py -t ${dev} 2020081000 00L HISTORY ${confopts}
#${PYTHON3} ./run_hafs.py -t ${dev} 2020081006 00L HISTORY ${confopts}

# KY/TN tornado outbreak:  2021/12/10/12
 ${PYTHON3} ./run_hafs.py -t ${dev} 2021121012 00L HISTORY ${confopts}

# MCS and diurnal isolated deep convection: 2020/07/07/00
#${PYTHON3} ./run_hafs.py -t ${dev} 2020070700 00L HISTORY ${confopts}

#===============================================================================

date

echo 'cronjob done'
