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
# HOMEhafs=/lfs4/BMC/wrfruc/${USER}/HAFS/HAFS
# dev="-s sites/xjet.ent -f"
# PYTHON3=/lfs4/BMC/wrfruc/Samuel.Trahan/HAFS/python-3.7.10/bin/python

# MSU Orion
 HOMEhafs=/work/noaa/zrtrr/${USER}/HAFS
 dev="-s sites/orion.ent -f"
 PYTHON3=/work/noaa/zrtrr/strahan/python-3.7.10/bin/python

# NOAA RDHPCS Hera
# HOMEhafs=/scratch1/NCEPDEV/hwrf/save/${USER}/HAFS
# dev="-s sites/hera.ent -f"
# PYTHON3=/scratch2/BMC/wrfruc/Samuel.Trahan/HAFS/python-3.7.10/bin/python

cd ${HOMEhafs}/rocoto

EXPT=$(basename ${HOMEhafs})

#===============================================================================
# Here are some simple examples, more examples can be seen in cronjob_hafs_rt.sh

 # ${PYTHON3} ./run_hafs.py -t ${dev} 2019082900 00L HISTORY config.EXPT=${EXPT} \
 #     config.SUBEXPT=${EXPT}_ghrsst \
 #     forecast.output_history=.true. \
 #     ../parm/hafs_regional_static.conf \
 #     ../parm/hafs_docn.conf ../parm/hafs_docn_ghrsst.conf

 ${PYTHON3} ./run_hafs.py -t ${dev} 2019082900 00L HISTORY config.EXPT=${EXPT} \
     config.SUBEXPT=${EXPT}_era5 \
     forecast.output_history=.true. \
     ../parm/hafs_regional_static.conf ../parm/hafs_hycom.conf \
     ../parm/hafs_datm.conf ../parm/hafs_datm_era5.conf

# Run all cycles of a storm
#${PYTHON3} ./run_hafs.py ${dev} 2020 13L HISTORY config.EXPT=${EXPT} # Laura

# Run specified cycles of a storm
#${PYTHON3} ./run_hafs.py ${dev} 2020082506-2020082512 13L HISTORY \
#   config.EXPT=${EXPT} config.SUBEXPT=${EXPT} # Laura

# Run one cycle of a storm
# ${PYTHON3} ./run_hafs.py -t ${dev} 2020082512 13L HISTORY config.EXPT=${EXPT}

#===============================================================================

date

echo 'cronjob done'
