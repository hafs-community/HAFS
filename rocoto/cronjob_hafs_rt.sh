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
 HOMEhafs=/work/noaa/hwrf/save/${USER}/HAFS
 dev="-s sites/orion.ent -f"
 PYTHON3=/apps/intel-2020/intel-2020/intelpython3/bin/python3

# NOAA RDHPCS Hera
#HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/HAFS
#dev="-s sites/hera.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})
scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================

 # Regional static NATL basin-focused configuration with atm-ocn coupling
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_atm_ocn \
     config.NHRS=6 ${scrubopt} \
     ../parm/hafs_regional_static.conf \
     ../parm/hafs_hycom.conf

 # Regional static NATL basin-focused configuration with atm-wav coupling
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_atm_wav \
     config.NHRS=6 ${scrubopt} \
     ../parm/hafs_regional_static.conf \
     ../parm/hafs_ww3.conf \
     forecast.cpl_atm_wav=cmeps_2way

## Regional static NATL basin-focused configuration with atm-ocn-wav coupling
#${PYTHON3} ./run_hafs.py -t ${dev} 2020082512 00L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_atm_ocn_wav \
#    config.NHRS=6 ${scrubopt} \
#    ../parm/hafs_regional_static.conf \
#    ../parm/hafs_hycom_ww3.conf \
#    forecast.cpl_atm_ocn=cmeps_2way \
#    forecast.cpl_atm_wav=cmeps_2way

 # Regional low-resolution static NATL basin-focused configuration with atm-ocn-wav coupling
 ${PYTHON3} ./run_hafs.py -t ${dev} 2019082900 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_C192s1n4_atm_ocn_wav \
     config.NHRS=24 ${scrubopt} \
     ../parm/hafs_regional_static_C192s1n4.conf \
     ../parm/hafs_hycom_ww3.conf \
     forecast.cpl_atm_ocn=cmeps_2way \
     forecast.cpl_atm_wav=cmeps_2way

 # Regional storm-focused configuration with atm-ocn-wav coupling
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_atm_ocn_wav \
     config.NHRS=6 ${scrubopt} \
     ../parm/hafs_hycom_ww3.conf \
     forecast.cpl_atm_ocn=cmeps_2way \
     forecast.cpl_atm_wav=cmeps_2way

 # Regional storm-focused configuration (atm-only) with GFS grib2ab format IC/BC
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_atm_only \
     config.ictype=gfsgrib2ab_0p25 forecast.nstf_n2=1 \
     config.NHRS=6 ${scrubopt}

#===============================================================================

 # Global-nesting static NATL basin-focused configuration (atm-only)
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest_static \
     config.NHRS=6 ${scrubopt} \
     ../parm/hafs_globnest_static.conf

 # Global-nesting storm-focused configuration (atm-only) with GFS grib2ab format IC/BC
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest_grib2ab \
     config.ictype=gfsgrib2ab_0p25 forecast.nstf_n2=1 \
     config.NHRS=6 ${scrubopt} \
     ../parm/hafs_globnest.conf

#===============================================================================

# 3DEnVar with GDAS ensembles
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082506-2020082512 00L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_C192s1n4_3denvar \
     config.run_gsi=yes config.run_envar=yes \
     gsi.use_bufr_nr=yes \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_da_C192s1n4.conf

#===============================================================================

date

echo 'cronjob done'
