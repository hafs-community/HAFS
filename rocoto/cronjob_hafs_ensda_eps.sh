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
# HOMEhafs=/mnt/lfs4/HFIP/hwrfv3/${USER}/hafsv0p3e_20220429
 #dev="-s sites/xjet.ent -f"
# dev="-s sites/xjet_ensda_eps.ent -f"
# PYTHON3=/apps/intel/intelpython3/bin/python3

# MSU Orion
HOMEhafs=/work2/noaa/hurricane/save/${USER}/H222_ensemble
 #dev="-s sites/orion.ent -f"
dev="-s sites/orion_ensda_eps.ent -f"
PYTHON3=/apps/intel-2020/intel-2020/intelpython3/bin/python3

# NOAA RDHPCS Hera
#HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/HAFS
#dev="-s sites/hera.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})

#===============================================================================
 # ensda_eps: fgat+3denvar+enkf
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_ensda_eps \
     ../parm/hafs_2022_regional_ensda_eps_AL.conf"

 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082518 00L HISTORY ${confopts} \
     config.NHRS=120 config.ENS_SIZE=20 config.scrub_work=no config.scrub_com=no \
     forecast.write_group=1 \
     forecast.write_tasks_per_group=20 \


#===============================================================================

date

echo 'cronjob done'
