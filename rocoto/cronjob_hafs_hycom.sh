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
#HOMEhafs=/work/noaa/hwrf/save/${USER}/HAFS
#dev="-s sites/orion.ent -f"
#PYTHON3=/apps/intel-2020/intel-2020/intelpython3/bin/python3

# NOAA RDHPCS Hera
 HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/HAFS
 dev="-s sites/hera.ent -f"
 PYTHON3=/apps/intel/intelpython3/bin/python3

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})

scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================

# Run regional hafs-hycom coupled configuration with the bilinear regridding method
 ${PYTHON3} ./run_hafs.py -t ${dev} 2019082900 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT} \
     config.ictype=gfsnemsio config.bctype=gfsgrib2_0p25 \
     ${scrubopt} \
     ../parm/hafs_regional_static.conf \
     ../parm/hafs_hycom.conf
#    forecast.cpl_ocean=2 forecast.merge_import=.true. # default settings in hafs_hycom.conf

# Run regional hafs-hycom coupled configuration with the nearest point regridding method
#${PYTHON3} ./run_hafs.py -t ${dev} 2019082900 00L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT} \
#    config.ictype=gfsnemsio config.bctype=gfsgrib2_0p25 \
#    ${scrubopt} \
#    ../parm/hafs_regional_static.conf \
#    ../parm/hafs_hycom.conf \
#    forecast.cpl_ocean=1 forecast.merge_import=.false.

# Run regional hafs-hycom coupled configuration in side-by-side model (no coupling)
#${PYTHON3} ./run_hafs.py -t ${dev} 2019082900 00L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT} \
#    config.ictype=gfsnemsio config.bctype=gfsgrib2_0p25 \
#    ${scrubopt} \
#    ../parm/hafs_regional_static.conf \
#    ../parm/hafs_hycom.conf \
#    forecast.cpl_ocean=0 forecast.merge_import=.false.


#===============================================================================

date

echo 'cronjob done'
