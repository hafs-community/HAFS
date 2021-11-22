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
HOMEhafs=/work/noaa/hwrf/save/${USER}/hafs_nesting_20211119
dev="-s sites/orion.ent -f"
PYTHON3=/apps/intel-2020/intel-2020/intelpython3/bin/python3

# NOAA RDHPCS Hera
# HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/HAFS
# dev="-s sites/hera.ent -f"
# PYTHON3=/apps/intel/intelpython3/bin/python3

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})
scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================
# Here are some simple examples, more examples can be seen in cronjob_hafs_rt.sh

# Run all cycles of a storm
#${PYTHON3} ./run_hafs.py ${dev} 2019 05L HISTORY config.EXPT=${EXPT}# Dorian

# Run specified cycles of a storm
#${PYTHON3} ./run_hafs.py ${dev} 2018083018-2018083100 06L HISTORY \
#   config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_try1 # Florence

# Run one cycle of a storm with C96
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082512 13L HISTORY \
    config.EXPT=${EXPT} config.SUBEXPT=hafs_movingnest3x_C96_Laural13L2021082512 \
    config.NHRS=36 ${scrubopt} \
    ../parm/hafs_globmovenest3x_C96_Laura13L_2020082512.conf

# Run one cycle of a storm with C768
 ${PYTHON3} ./run_hafs.py -t ${dev} 2020082512 13L HISTORY \
     config.EXPT=${EXPT} config.SUBEXPT=hafs_movingnest3x_C768_Laural13L2021082512 \
     config.NHRS=36 ${scrubopt} \
     ../parm/hafs_globmovenest3x_C768_Laura13L_2020082512.conf

#===============================================================================

date

echo 'cronjob done'
