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

# Regional static NATL basin-focused configuration with GFS nemsio format IC/BC
#${PYTHON3} ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static \
#    config.NHRS=12 ${scrubopt} \
#    ../parm/hafs_regional_static.conf

# Regional storm-focused configuration with GFS nemsio format IC/BC
 ${PYTHON3} ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional \
     config.NHRS=12 ${scrubopt}

# Regional static NATL basin-focused configuration with GFS nemsio format IC and grib2ab format BC
 ${PYTHON3} ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_grib2ab_lbc \
     config.ictype=gfsnemsio config.bctype=gfsgrib2ab_0p25 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_regional_static.conf

# Regional static NATL basin-focused configuration with GFS nemsio format IC and grib2 format BC
#${PYTHON3} ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_grib2_lbc \
#    config.ictype=gfsnemsio config.bctype=gfsgrib2_0p25 \
#    config.NHRS=12 ${scrubopt} \
#    ../parm/hafs_regional_static.conf

# Regional storm-focused configuration with GFS nemsio format IC and grib2ab format BC
#${PYTHON3} ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_grib2ab_lbc \
#    config.ictype=gfsnemsio config.bctype=gfsgrib2ab_0p25 \
#    config.NHRS=12 ${scrubopt}

# Regional storm-focused configuration with GFS grib2 format IC and grib2 format BC
 ${PYTHON3} ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_grib2 \
     config.ictype=gfsgrib2_0p25 config.bctype=gfsgrib2_0p25 \
     config.halo_blend=10 forecast.nstf_n2=1 \
     config.NHRS=12 ${scrubopt}

#===============================================================================

# Global-nesting static NATL basin-focused configuration with GFS nemsio format IC/BC
 ${PYTHON3} ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest_static \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_globnest_static.conf

# Global-nesting storm-focused configuration with GFS nemsio format IC/BC
#${PYTHON3} ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest \
#    config.NHRS=12 ${scrubopt} \
#    ../parm/hafs_globnest.conf

# Global-nesting storm-focused configuration with GFS grib2ab format IC/BC
 ${PYTHON3} ./run_hafs.py -t ${dev} 2019091600 09L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest_grib2ab \
     config.ictype=gfsgrib2ab_0p25 forecast.nstf_n2=1 \
     config.NHRS=12 ${scrubopt} \
     ../parm/hafs_globnest.conf

#===============================================================================

# Fakestorm (e.g., NATL00L) with the regional static NATL basin-focused domain configuration
#${PYTHON3} ./run_hafs.py -t ${dev} 2019091600 00L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_static_fakestorm \
#    config.NHRS=12 ${scrubopt} \
#    ../parm/hafs_fakestorm.conf

# Fakestorm globnest_C96s1n4_180x180 configuration
#${PYTHON3} ./run_hafs.py -t ${dev} 2019091600 00L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_globnest_C96s1n4_180x180 \
#    config.NHRS=12 ${scrubopt} \
#    ../parm/examples/hafs_globnest_C96s1n4_180x180.conf \
#    ../parm/hafs_fakestorm.conf

# Fakestorm regional_C96s1n4_180x180 configuration
#${PYTHON3} ./run_hafs.py -t ${dev} 2019091600 00L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_rt_regional_C96s1n4_180x180 \
#    config.NHRS=12 ${scrubopt} \
#    ../parm/examples/hafs_regional_C96s1n4_180x180.conf \
#    ../parm/hafs_fakestorm.conf

#===============================================================================

date

echo 'cronjob done'
