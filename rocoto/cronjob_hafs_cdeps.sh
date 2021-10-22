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

#NOAA RDHPCS Hera
#HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/HAFS
#dev="-s sites/hera.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})

#===============================================================================
# Here are some simple examples, more examples can be seen in cronjob_hafs_rt.sh

# Run data atmosphere with ERA5
${PYTHON3} ./run_hafs.py -t ${dev} 2019082900 00L HISTORY config.EXPT=${EXPT} \
    config.SUBEXPT=${EXPT}_era5 \
    forecast.output_history=.true. \
    ../parm/hafs_regional_static.conf ../parm/hafs_hycom.conf \
    ../parm/hafs_datm.conf ../parm/hafs_datm_era5.conf

# Run data ocean with OISST
${PYTHON3} ./run_hafs.py -t ${dev} 2019082900 00L HISTORY config.EXPT=${EXPT} \
    config.SUBEXPT=${EXPT}_oisst \
    forecast.output_history=.true. \
    ../parm/hafs_regional_static.conf \
    ../parm/hafs_docn.conf ../parm/hafs_docn_oisst.conf

# Run data ocean with GHRSST
${PYTHON3} ./run_hafs.py -t ${dev} 2019082900 00L HISTORY config.EXPT=${EXPT} \
    config.SUBEXPT=${EXPT}_ghrsst \
    forecast.output_history=.true. \
    ../parm/hafs_regional_static.conf \
    ../parm/hafs_docn.conf ../parm/hafs_docn_ghrsst.conf

#===============================================================================

date

echo 'cronjob done'
