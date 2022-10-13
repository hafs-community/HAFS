#!/bin/sh
set -x
date

# NOAA WCOSS Dell Phase3
# HOMEhafs=/gpfs/dell2/emc/modeling/noscrub/${USER}/save/hafsv0p3a_2022rt
# dev="-s sites/wcoss_dell_p3.ent -f"
# PYTHON3=/usrx/local/prod/packages/python/3.6.3/bin/python3

# NOAA WCOSS Cray
#HOMEhafs=/gpfs/hps3/emc/hwrf/noscrub/${USER}/save/hafsv0p3a_2022rt
#dev="-s sites/wcoss_cray.ent -f"
#PYTHON3=/opt/intel/intelpython3/bin/python3

# NOAA WCOSS2
HOMEhafs=/lfs/h2/emc/hur/noscrub/${USER}/save/hafs.v0.3.0
dev="-s sites/wcoss2.ent -f"
PYTHON3=/usr/bin/python3

# NOAA RDHPCS Jet
#HOMEhafs=/mnt/lfs4/HFIP/hwrfv3/${USER}/HAFS
#dev="-s sites/xjet.ent -f"
#PYTHON3=/apps/intel/intelpython3/bin/python3

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
SUBEXPT="hafsv0p3a_2022rt"
#===============================================================================
 # atm_init+atm_vi+fgat+d02_3denvar+anal_merge and cycling storm perturbation
 confopts="config.EXPT=${EXPT} config.SUBEXPT=${SUBEXPT} \
     ../parm/hafsv0p3a_final.conf"

 # 2022 NATL storms
#done ${PYTHON3} ./run_hafs.py -t ${dev} 2022060218-2022060618 01L HISTORY ${confopts} # Alex 
#done ${PYTHON3} ./run_hafs.py -t ${dev} 2022060218-2022060406 01L HISTORY ${confopts} # Alex 
#done ${PYTHON3} ./run_hafs.py -t ${dev} 2022060412-2022060618 01L HISTORY ${confopts} # Alex 

 # 2022 EPAC storms
${PYTHON3} ./run_hafs.py -t ${dev} 2022080718-2022081000 09E HISTORY ${confopts}

date

echo 'cronjob done'
