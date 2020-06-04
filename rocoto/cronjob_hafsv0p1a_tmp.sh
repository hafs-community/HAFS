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
 HOMEhafs=/mnt/lfs4/HFIP/hwrfv3/${USER}/hafsv0p1a_202005
#dev="-s sites/xjet.ent -f"
 dev="-s sites/xjet_hafsv0p1a.ent -f"
 PYTHON3=/apps/intel/intelpython3/bin/python3

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
scrubopt="config.scrub_work=no config.scrub_com=no"

#===============================================================================
# hafs.v0.1a NATL

# NATL00L2019 block# 1-1
#${PYTHON3} ./run_hafs.py -t ${dev} 2019082406-2019082818 00L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_l75 \
#    ../parm/hafsv0p1a_al.conf

# NATL00L2019 block# 1-2
#${PYTHON3} ./run_hafs.py -t ${dev} 2019082900-2019091006 00L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_l75 \
#    ../parm/hafsv0p1a_al.conf

# NATL00L2019 block# 2-1
#${PYTHON3} ./run_hafs.py -t ${dev} 2019091212-2019092106 00L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_l75 \
#    ../parm/hafsv0p1a_al.conf

# NATL00L2019 block# 2-2
#${PYTHON3} ./run_hafs.py -t ${dev} 2019092112-2019100206 00L HISTORY \
#    config.EXPT=${EXPT} config.SUBEXPT=${EXPT}_l75 \
#    ../parm/hafsv0p1a_al.conf

#===============================================================================

date

echo 'cronjob done'
